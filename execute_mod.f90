! execute_mod.f90 - Z-machine instruction execution
! Implements all opcodes for versions 3, 5, and 8.

module execute_mod
  use memory_mod
  use header_mod
  use stack_mod
  use text_mod
  use object_mod
  use decode_mod
  use quetzal_mod
  implicit none
  private
  public :: exec_init, exec_run, exec_pc, exec_running

  integer :: exec_pc = 0
  logical :: exec_running = .false.

  ! Output stream state
  logical :: output_stream1 = .true.   ! screen
  logical :: output_stream2 = .false.  ! transcript
  integer :: stream3_depth = 0
  integer :: stream3_table(16) = 0     ! nested stream 3 tables
  integer :: stream3_pos(16) = 0       ! current write position in each

  ! Window state (0 = lower/main, 1 = upper/status)
  integer :: current_window = 0

  ! Undo state
  integer(1), allocatable :: undo_memory(:)
  integer, allocatable :: undo_stack(:)
  type(call_frame), allocatable :: undo_frames(:)
  integer :: undo_pc = 0
  integer :: undo_sp = 0
  integer :: undo_fp = 0
  logical :: undo_stream1 = .true.
  logical :: undo_stream2 = .false.
  integer :: undo_stream3_depth = 0
  integer :: undo_stream3_table(16) = 0
  integer :: undo_stream3_pos(16) = 0
  integer :: undo_window = 0
  logical :: undo_valid = .false.

contains

  subroutine exec_init()
    integer :: flags2

    exec_pc = hdr_initial_pc
    exec_running = .true.
    call stack_init()
    output_stream1 = .true.
    flags2 = mem_read_word(16)
    output_stream2 = (iand(flags2, 1) /= 0)
    stream3_depth = 0
    current_window = 0
    undo_valid = .false.
  end subroutine exec_init

  subroutine set_transcript_state(enabled)
    logical, intent(in) :: enabled
    integer :: flags2

    output_stream2 = enabled
    flags2 = mem_read_word(16)
    if (enabled) then
      flags2 = ior(flags2, 1)
    else
      flags2 = iand(flags2, not(1))
    end if
    call mem_write_word(16, flags2)
  end subroutine set_transcript_state

  subroutine checked_store_byte(addr, val)
    integer, intent(in) :: addr, val

    if (addr >= mem_static_base) then
      write(*,'(A,Z6)') 'Error: storeb to static memory at $', addr
      exec_running = .false.
      return
    end if
    call mem_write_byte(addr, val)
  end subroutine checked_store_byte

  subroutine checked_store_word(addr, val)
    integer, intent(in) :: addr, val

    if (addr >= mem_static_base .or. addr + 1 >= mem_static_base) then
      write(*,'(A,Z6)') 'Error: storew to static memory at $', addr
      exec_running = .false.
      return
    end if
    call mem_write_word(addr, val)
  end subroutine checked_store_word

  subroutine do_encode_text(operands)
    integer, intent(in) :: operands(8)
    integer :: zscii_addr, text_len, from_pos, coded_addr, i, actual_len, enc_words
    integer :: encoded(3)
    character(len=256) :: text_buf

    zscii_addr = operands(1)
    text_len = operands(2)
    from_pos = operands(3)
    coded_addr = operands(4)

    actual_len = max(0, min(text_len, len(text_buf)))
    text_buf = ''

    do i = 1, actual_len
      text_buf(i:i) = text_zscii_to_char(mem_read_byte(zscii_addr + from_pos + i - 1))
    end do

    call text_encode(text_buf, actual_len, encoded, enc_words)
    do i = 1, enc_words
      call mem_write_word(coded_addr + (i - 1) * 2, encoded(i))
    end do
  end subroutine do_encode_text

  function verify_story_checksum() result(ok)
    logical :: ok
    integer :: checksum, limit, i

    if (hdr_version < 3 .or. hdr_file_length == 0) then
      ok = .false.
      return
    end if

    limit = min(hdr_file_length, mem_size)
    checksum = 0
    do i = 64, limit - 1
      checksum = iand(checksum + mem_read_byte(i), 65535)
    end do
    ok = (checksum == hdr_checksum)
  end function verify_story_checksum

  ! Convert packed address to byte address
  function unpack_addr(packed, is_string) result(addr)
    integer, intent(in) :: packed
    logical, intent(in) :: is_string
    integer :: addr

    select case (hdr_version)
    case (1:3)
      addr = packed * 2
    case (4:5)
      addr = packed * 4
    case (6:7)
      if (is_string) then
        addr = packed * 4 + hdr_string_offset * 8
      else
        addr = packed * 4 + hdr_routine_offset * 8
      end if
    case (8)
      addr = packed * 8
    case default
      addr = packed * 2
    end select
  end function unpack_addr

  ! Interpret a value as signed 16-bit
  function to_signed(val) result(s)
    integer, intent(in) :: val
    integer :: s
    s = iand(val, 65535)
    if (s >= 32768) s = s - 65536
  end function to_signed

  ! Output a character respecting output streams
  subroutine output_char(ch)
    character, intent(in) :: ch
    integer :: addr

    if (stream3_depth > 0) then
      ! Write to memory table (bypass normal write protection)
      stream3_pos(stream3_depth) = stream3_pos(stream3_depth) + 1
      addr = stream3_table(stream3_depth) + 2 + stream3_pos(stream3_depth) - 1
      if (addr >= 0 .and. addr < mem_size) then
        memory(addr) = int(iachar(ch), 1)
      end if
      return
    end if

    if (output_stream1 .and. current_window == 0) then
      write(*,'(A)', advance='no') ch
    end if
  end subroutine output_char

  ! Output a string respecting output streams
  subroutine output_string(str, slen)
    character(len=*), intent(in) :: str
    integer, intent(in) :: slen
    integer :: i

    do i = 1, slen
      call output_char(str(i:i))
    end do
  end subroutine output_string

  ! Print a Z-encoded string at address
  subroutine print_zstr(addr, next_addr)
    integer, intent(in) :: addr
    integer, intent(out) :: next_addr
    character(len=1024) :: text
    integer :: tlen

    call text_decode(addr, text, tlen, next_addr)
    call output_string(text, tlen)
  end subroutine print_zstr

  ! Call a routine
  subroutine call_routine(packed_addr, args, num_args, store_var)
    integer, intent(in) :: packed_addr
    integer, intent(in) :: args(:)
    integer, intent(in) :: num_args
    integer, intent(in) :: store_var  ! -1 for no store

    integer :: routine_addr, num_locals, i
    integer :: init_vals(15)

    ! Call to address 0 returns false
    if (packed_addr == 0) then
      if (store_var >= 0) call var_write(store_var, 0)
      return
    end if

    routine_addr = unpack_addr(packed_addr, .false.)

    ! Read routine header
    num_locals = mem_read_byte(routine_addr)
    routine_addr = routine_addr + 1

    init_vals = 0
    if (hdr_version <= 4) then
      ! Read initial values for locals
      do i = 1, num_locals
        init_vals(i) = mem_read_word(routine_addr)
        routine_addr = routine_addr + 2
      end do
    end if

    ! Push call frame
    call frame_push(exec_pc, store_var, num_locals, init_vals, args, num_args)

    ! Set PC to first instruction of routine
    exec_pc = routine_addr
  end subroutine call_routine

  ! Return from routine with given value
  subroutine return_routine(retval)
    integer, intent(in) :: retval
    integer :: return_pc, return_var

    call frame_pop(return_pc, return_var)
    exec_pc = return_pc
    if (return_var >= 0) then
      call var_write(return_var, iand(retval, 65535))
    end if
  end subroutine return_routine

  ! Process a branch based on condition
  subroutine do_branch(condition, instr)
    logical, intent(in) :: condition
    type(decoded_instr), intent(in) :: instr
    logical :: should_branch

    should_branch = (condition .eqv. instr%branch_on_true)

    if (should_branch) then
      if (instr%branch_offset == 0) then
        call return_routine(0)  ! return false
      else if (instr%branch_offset == 1) then
        call return_routine(1)  ! return true
      else
        exec_pc = instr%next_pc + instr%branch_offset - 2
      end if
    end if
  end subroutine do_branch

  ! Read branch data from current address
  subroutine read_branch(addr, branch_on_true, branch_offset, next_addr)
    integer, intent(in) :: addr
    logical, intent(out) :: branch_on_true
    integer, intent(out) :: branch_offset
    integer, intent(out) :: next_addr
    integer :: b1, b2

    b1 = mem_read_byte(addr)
    branch_on_true = (iand(b1, 128) /= 0)

    if (iand(b1, 64) /= 0) then
      ! Single byte offset (6 bits, unsigned)
      branch_offset = iand(b1, 63)
      next_addr = addr + 1
    else
      ! Two byte offset (14 bits, signed)
      b2 = mem_read_byte(addr + 1)
      branch_offset = ior(ishft(iand(b1, 63), 8), b2)
      ! Sign extend from 14 bits
      if (branch_offset >= 8192) branch_offset = branch_offset - 16384
      next_addr = addr + 2
    end if
  end subroutine read_branch

  ! Main execution loop
  subroutine exec_run()
    type(decoded_instr) :: instr
    integer :: i, val, operands(8), addr, dummy
    integer :: a, b, result, sval, args(7), num_args

    do while (exec_running)
      ! Decode instruction
      call decode_instruction(exec_pc, instr)

      ! Resolve variable operands (reading from variables, including stack pops)
      do i = 1, instr%num_operands
        if (instr%op_types(i) == OP_VAR) then
          if (instr%operands(i) == 0) then
            ! About to pop stack - check if this will underflow
            val = 0
            if (fp > 0) val = frames(fp)%stack_base
            if (sp <= val) then
              write(*,'(A,Z6,A,I0,A,I0)') 'TRACE: underflow at PC=$', exec_pc, &
                ' class=', instr%op_count_class, ' op=', instr%opcode
            end if
          end if
          instr%operands(i) = var_read(instr%operands(i))
        end if
        operands(i) = instr%operands(i)
      end do

      ! Read store variable if applicable
      addr = instr%next_pc
      call read_store_and_branch(instr, addr)
      exec_pc = instr%next_pc

      ! Execute based on opcode class and number
      select case (instr%op_count_class)

      ! ==================== 2OP ====================
      case (COUNT_2OP)
        a = operands(1)
        b = operands(2)

        select case (instr%opcode)
        case (1)  ! je - jump if a equals any of b,c,d...
          val = 0
          do i = 2, instr%num_operands
            if (a == operands(i)) val = 1
          end do
          call do_branch(val == 1, instr)

        case (2)  ! jl
          call do_branch(to_signed(a) < to_signed(b), instr)

        case (3)  ! jg
          call do_branch(to_signed(a) > to_signed(b), instr)

        case (4)  ! dec_chk (indirect variable reference)
          if (a == 0) then
            val = to_signed(stack_peek()) - 1
          else
            val = to_signed(var_read(a)) - 1
          end if
          call var_indirect_write(a, iand(val, 65535))
          call do_branch(to_signed(iand(val, 65535)) < to_signed(b), instr)

        case (5)  ! inc_chk (indirect variable reference)
          if (a == 0) then
            val = to_signed(stack_peek()) + 1
          else
            val = to_signed(var_read(a)) + 1
          end if
          call var_indirect_write(a, iand(val, 65535))
          call do_branch(to_signed(iand(val, 65535)) > to_signed(b), instr)

        case (6)  ! jin - test if a is child of b
          call do_branch(obj_get_parent(a) == b, instr)

        case (7)  ! test - bitmap test
          call do_branch(iand(a, b) == b, instr)

        case (8)  ! or
          call var_write(instr%store_var, ior(a, b))

        case (9)  ! and
          call var_write(instr%store_var, iand(a, b))

        case (10) ! test_attr
          call do_branch(obj_get_attr(a, b), instr)

        case (11) ! set_attr
          call obj_set_attr(a, b)

        case (12) ! clear_attr
          call obj_clear_attr(a, b)

        case (13) ! store (indirect variable reference)
          call var_indirect_write(a, b)

        case (14) ! insert_obj
          call obj_insert(a, b)

        case (15) ! loadw
          val = mem_read_word(a + b * 2)
          call var_write(instr%store_var, val)

        case (16) ! loadb
          val = mem_read_byte(a + b)
          call var_write(instr%store_var, val)

        case (17) ! get_prop
          val = obj_get_prop(a, b)
          call var_write(instr%store_var, val)

        case (18) ! get_prop_addr
          val = obj_get_prop_addr(a, b)
          call var_write(instr%store_var, val)

        case (19) ! get_next_prop
          val = obj_get_next_prop(a, b)
          call var_write(instr%store_var, val)

        case (20) ! add
          result = to_signed(a) + to_signed(b)
          call var_write(instr%store_var, iand(result, 65535))

        case (21) ! sub
          result = to_signed(a) - to_signed(b)
          call var_write(instr%store_var, iand(result, 65535))

        case (22) ! mul
          result = to_signed(a) * to_signed(b)
          call var_write(instr%store_var, iand(result, 65535))

        case (23) ! div
          if (b == 0) then
            write(*,*) 'Error: division by zero'
            exec_running = .false.
          else
            result = to_signed(a) / to_signed(b)
            call var_write(instr%store_var, iand(result, 65535))
          end if

        case (24) ! mod
          if (b == 0) then
            write(*,*) 'Error: division by zero'
            exec_running = .false.
          else
            result = mod(to_signed(a), to_signed(b))
            call var_write(instr%store_var, iand(result, 65535))
          end if

        case (25) ! call_2s
          args(1) = b
          call call_routine(a, args, 1, instr%store_var)

        case (26) ! call_2n
          args(1) = b
          call call_routine(a, args, 1, -1)

        case (27) ! set_colour
          ! Stub: ignore color setting
          continue

        case (28) ! throw
          ! Unwind stack to frame b, return value a
          do while (stack_frame_count() > b)
            call frame_pop(val, dummy)
          end do
          call return_routine(a)

        case default
          write(*,'(A,I0,A,Z2)') 'Unknown 2OP:', instr%opcode, ' at $', exec_pc
          exec_running = .false.
        end select

      ! ==================== 1OP ====================
      case (COUNT_1OP)
        a = operands(1)

        select case (instr%opcode)
        case (0)  ! jz
          call do_branch(a == 0, instr)

        case (1)  ! get_sibling
          val = obj_get_sibling(a)
          call var_write(instr%store_var, val)
          call do_branch(val /= 0, instr)

        case (2)  ! get_child
          val = obj_get_child(a)
          call var_write(instr%store_var, val)
          call do_branch(val /= 0, instr)

        case (3)  ! get_parent
          val = obj_get_parent(a)
          call var_write(instr%store_var, val)

        case (4)  ! get_prop_len
          val = obj_get_prop_len(a)
          call var_write(instr%store_var, val)

        case (5)  ! inc (indirect variable reference)
          if (a == 0) then
            val = to_signed(stack_peek()) + 1
          else
            val = to_signed(var_read(a)) + 1
          end if
          call var_indirect_write(a, iand(val, 65535))

        case (6)  ! dec (indirect variable reference)
          if (a == 0) then
            val = to_signed(stack_peek()) - 1
          else
            val = to_signed(var_read(a)) - 1
          end if
          call var_indirect_write(a, iand(val, 65535))

        case (7)  ! print_addr
          call print_zstr(a, dummy)

        case (8)  ! call_1s
          call call_routine(a, args(1:0), 0, instr%store_var)

        case (9)  ! remove_obj
          call obj_remove(a)

        case (10) ! print_obj
          block
            integer :: name_addr
            logical :: has_name
            call obj_short_name_addr(a, name_addr, has_name)
            if (has_name) call print_zstr(name_addr, dummy)
          end block

        case (11) ! ret
          call return_routine(a)

        case (12) ! jump
          exec_pc = instr%next_pc + to_signed(a) - 2

        case (13) ! print_paddr
          addr = unpack_addr(a, .true.)
          call print_zstr(addr, dummy)

        case (14) ! load
          ! Load value of variable without popping stack
          if (a == 0) then
            val = stack_peek()
          else
            val = var_read(a)
          end if
          call var_write(instr%store_var, val)

        case (15) ! not (v1-4) / call_1n (v5+)
          if (hdr_version <= 4) then
            call var_write(instr%store_var, iand(not(a), 65535))
          else
            call call_routine(a, args(1:0), 0, -1)
          end if

        case default
          write(*,'(A,I0,A,Z2)') 'Unknown 1OP:', instr%opcode, ' at $', exec_pc
          exec_running = .false.
        end select

      ! ==================== 0OP ====================
      case (COUNT_0OP)
        select case (instr%opcode)
        case (0)  ! rtrue
          call return_routine(1)

        case (1)  ! rfalse
          call return_routine(0)

        case (2)  ! print (inline text)
          call print_zstr(instr%next_pc, addr)
          exec_pc = addr

        case (3)  ! print_ret
          call print_zstr(instr%next_pc, addr)
          call output_char(char(10))
          exec_pc = addr
          call return_routine(1)

        case (4)  ! nop
          continue

        case (5)  ! save (v1-4)
          call do_save(instr)

        case (6)  ! restore (v1-4)
          call do_restore(instr)

        case (7)  ! restart
          val = iand(mem_read_word(16), 3)
          call mem_load_story(mem_story_filename, dummy)
          call header_init()
          call mem_write_word(16, ior(iand(mem_read_word(16), not(3)), val))
          call exec_init()

        case (8)  ! ret_popped
          val = stack_pop()
          call return_routine(val)

        case (9)  ! pop (v1-4) / catch (v5+)
          if (hdr_version <= 4) then
            val = stack_pop()  ! discard
          else
            ! catch: store current frame count
            call var_write(instr%store_var, stack_get_frame_id())
          end if

        case (10) ! quit
          exec_running = .false.

        case (11) ! new_line
          call output_char(char(10))

        case (12) ! show_status (v3)
          ! TODO: implement status line
          continue

        case (13) ! verify
          call do_branch(verify_story_checksum(), instr)

        case (15) ! piracy
          call do_branch(.true., instr)  ! always pass

        case default
          write(*,'(A,I0,A,Z2)') 'Unknown 0OP:', instr%opcode, ' at $', exec_pc
          exec_running = .false.
        end select

      ! ==================== VAR ====================
      case (COUNT_VAR)
        ! Extended opcodes are decoded as COUNT_VAR but have their own handler
        if (instr%form == FORM_EXT) then
          call exec_extended(instr, operands)
          cycle
        end if

        select case (instr%opcode)
        case (0)  ! call_vs / call
          if (instr%num_operands > 1) then
            num_args = instr%num_operands - 1
            do i = 1, num_args
              args(i) = operands(i + 1)
            end do
          else
            num_args = 0
          end if
          call call_routine(operands(1), args, num_args, instr%store_var)

        case (1)  ! storew
          call checked_store_word(operands(1) + operands(2) * 2, operands(3))

        case (2)  ! storeb
          call checked_store_byte(operands(1) + operands(2), operands(3))

        case (3)  ! put_prop
          call obj_put_prop(operands(1), operands(2), operands(3))

        case (4)  ! sread / aread
          call do_read(operands, instr)

        case (5)  ! print_char
          val = iand(operands(1), 65535)
          if (val == 13) then
            call output_char(char(10))  ! ZSCII 13 = newline
          else
            call output_char(char(iand(val, 255)))
          end if

        case (6)  ! print_num
          call print_number(to_signed(operands(1)))

        case (7)  ! random
          sval = to_signed(operands(1))
          if (sval <= 0) then
            ! Seed random number generator
            if (sval == 0) then
              call random_seed()
            else
              call random_seed(put=[(abs(sval), i=1,8)])
            end if
            call var_write(instr%store_var, 0)
          else
            call random_number_gen(sval, val)
            call var_write(instr%store_var, val)
          end if

        case (8)  ! push
          call stack_push(operands(1))

        case (9)  ! pull (indirect variable reference)
          val = stack_pop()
          call var_indirect_write(operands(1), val)

        case (10) ! split_window
          ! Track split but don't render upper window
          continue

        case (11) ! set_window
          current_window = operands(1)

        case (12) ! call_vs2
          if (instr%num_operands > 1) then
            num_args = instr%num_operands - 1
            do i = 1, num_args
              args(i) = operands(i + 1)
            end do
          else
            num_args = 0
          end if
          call call_routine(operands(1), args, num_args, instr%store_var)

        case (13) ! erase_window
          if (to_signed(operands(1)) == -1) then
            ! Unsplit and clear: reset to lower window
            current_window = 0
          else if (to_signed(operands(1)) == -2) then
            ! Clear without unsplitting
            continue
          end if

        case (14) ! erase_line
          continue

        case (15) ! set_cursor
          ! Stub
          continue

        case (16) ! get_cursor
          ! Stub: store 1,1
          call mem_write_word(operands(1), 1)
          call mem_write_word(operands(1) + 2, 1)

        case (17) ! set_text_style
          ! Stub
          continue

        case (18) ! buffer_mode
          ! Stub
          continue

        case (19) ! output_stream
          sval = to_signed(operands(1))
          select case (sval)
          case (1)
            output_stream1 = .true.
          case (-1)
            output_stream1 = .false.
          case (2)
            call set_transcript_state(.true.)
          case (-2)
            call set_transcript_state(.false.)
          case (3)
            if (instr%num_operands >= 2) then
              if (stream3_depth >= 16) then
                write(*,*) 'Error: stream 3 nesting overflow (max 16)'
                exec_running = .false.
              else
                stream3_depth = stream3_depth + 1
                stream3_table(stream3_depth) = operands(2)
                stream3_pos(stream3_depth) = 0
              end if
            end if
          case (-3)
            if (stream3_depth > 0) then
              ! Write length to first word of table (bypass write protection)
              addr = stream3_table(stream3_depth)
              if (addr >= 0 .and. addr + 1 < mem_size) then
                memory(addr) = int(iand(ishft(stream3_pos(stream3_depth), -8), 255), 1)
                memory(addr+1) = int(iand(stream3_pos(stream3_depth), 255), 1)
              end if
              stream3_depth = stream3_depth - 1
            end if
          end select

        case (20) ! input_stream
          ! Stub
          continue

        case (21) ! sound_effect
          ! Stub
          continue

        case (22) ! read_char
          call read_single_char(val)
          call var_write(instr%store_var, val)

        case (23) ! scan_table
          call do_scan_table(operands, instr)

        case (24) ! not (v5+)
          call var_write(instr%store_var, iand(not(operands(1)), 65535))

        case (25) ! call_vn
          if (instr%num_operands > 1) then
            num_args = instr%num_operands - 1
            do i = 1, num_args
              args(i) = operands(i + 1)
            end do
          else
            num_args = 0
          end if
          call call_routine(operands(1), args, num_args, -1)

        case (26) ! call_vn2
          if (instr%num_operands > 1) then
            num_args = instr%num_operands - 1
            do i = 1, num_args
              args(i) = operands(i + 1)
            end do
          else
            num_args = 0
          end if
          call call_routine(operands(1), args, num_args, -1)

        case (27) ! tokenise
          call do_tokenise(operands, instr%num_operands)

        case (28) ! encode_text
          call do_encode_text(operands)

        case (29) ! copy_table
          call do_copy_table(operands)

        case (30) ! print_table
          call do_print_table(operands, instr%num_operands)

        case (31) ! check_arg_count
          call do_branch(frame_arg_count() >= operands(1), instr)

        case default
          write(*,'(A,I0,A,Z2)') 'Unknown VAR:', instr%opcode, ' at $', exec_pc
          exec_running = .false.
        end select

      end select


    end do
  end subroutine exec_run

  ! Read store variable and branch data following an instruction
  subroutine read_store_and_branch(instr, addr)
    type(decoded_instr), intent(inout) :: instr
    integer, intent(inout) :: addr
    integer :: next_addr

    ! Determine if instruction has store/branch based on opcode tables
    call classify_instruction(instr)

    if (instr%has_store) then
      instr%store_var = mem_read_byte(addr)
      addr = addr + 1
    end if

    if (instr%has_branch) then
      call read_branch(addr, instr%branch_on_true, instr%branch_offset, next_addr)
      addr = next_addr
    end if

    ! For print/print_ret, the text follows the opcode byte directly
    ! (already handled by setting next_pc in execution)

    instr%next_pc = addr
  end subroutine read_store_and_branch

  ! Classify instruction to determine if it has store/branch
  subroutine classify_instruction(instr)
    type(decoded_instr), intent(inout) :: instr

    instr%has_store = .false.
    instr%has_branch = .false.

    if (instr%form == FORM_EXT) then
      select case (instr%opcode)
      case (0, 1, 2, 3, 4, 9, 10, 12, 19, 29)  ! save,restore,log_shift,art_shift,set_font,save_undo,restore_undo,check_unicode,get_wind_prop,buffer_screen
        instr%has_store = .true.
      case (6, 24, 27)  ! picture_data, push_stack, make_menu
        instr%has_branch = .true.
      end select
      return
    end if

    select case (instr%op_count_class)
    case (COUNT_2OP)
      select case (instr%opcode)
      case (1, 2, 3, 4, 5, 6, 7, 10)  ! je,jl,jg,dec_chk,inc_chk,jin,test,test_attr
        instr%has_branch = .true.
      case (8, 9, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)
        ! or,and,loadw,loadb,get_prop,get_prop_addr,get_next_prop,
        ! add,sub,mul,div,mod,call_2s
        instr%has_store = .true.
      end select

    case (COUNT_1OP)
      select case (instr%opcode)
      case (0)  ! jz
        instr%has_branch = .true.
      case (1, 2)  ! get_sibling, get_child
        instr%has_store = .true.
        instr%has_branch = .true.
      case (3, 4, 8, 14)  ! get_parent, get_prop_len, call_1s, load
        instr%has_store = .true.
      case (15) ! not (v1-4) has store, call_1n (v5+) has neither
        if (hdr_version <= 4) instr%has_store = .true.
      end select

    case (COUNT_0OP)
      select case (instr%opcode)
      case (5, 6)  ! save, restore
        if (hdr_version <= 3) then
          instr%has_branch = .true.
        else if (hdr_version == 4) then
          instr%has_store = .true.
        end if
      case (9)  ! pop (v1-4) / catch (v5+)
        if (hdr_version >= 5) instr%has_store = .true.
      case (13) ! verify
        instr%has_branch = .true.
      case (15) ! piracy
        instr%has_branch = .true.
      end select

    case (COUNT_VAR)
      select case (instr%opcode)
      case (0, 12)  ! call_vs, call_vs2
        instr%has_store = .true.
      case (4)  ! sread/aread
        if (hdr_version >= 5) instr%has_store = .true.
      case (7)  ! random
        instr%has_store = .true.
      case (22) ! read_char
        instr%has_store = .true.
      case (23) ! scan_table
        instr%has_store = .true.
        instr%has_branch = .true.
      case (24) ! not
        instr%has_store = .true.
      case (31) ! check_arg_count
        instr%has_branch = .true.
      end select
    end select
  end subroutine classify_instruction

  ! Execute extended opcodes
  subroutine exec_extended(instr, operands)
    type(decoded_instr), intent(in) :: instr
    integer, intent(in) :: operands(8)
    integer :: val, amount

    select case (instr%opcode)
    case (0)  ! save (EXT, v5+)
      call do_save(instr)

    case (1)  ! restore (EXT, v5+)
      call do_restore(instr)

    case (2)  ! log_shift
      amount = to_signed(operands(2))
      if (amount >= 0) then
        val = iand(ishft(operands(1), amount), 65535)
      else
        val = iand(ishft(operands(1), amount), 65535)  ! logical shift right
      end if
      call var_write(instr%store_var, val)

    case (3)  ! art_shift
      amount = to_signed(operands(2))
      val = to_signed(operands(1))
      if (amount >= 0) then
        val = ishft(val, amount)
      else
        ! Arithmetic right shift: preserve sign bit
        ! ishft does logical shift; manually fill sign bits
        val = ishft(iand(operands(1), 65535), amount)
        if (iand(operands(1), 32768) /= 0) then
          ! Original was negative: fill vacated high bits with 1s
          val = ior(val, iand(ishft(-1, 16 + amount), 65535))
        end if
      end if
      call var_write(instr%store_var, iand(val, 65535))

    case (4)  ! set_font
      if (operands(1) == 1 .or. operands(1) == 4) then
        call var_write(instr%store_var, 1)  ! return previous font
      else
        call var_write(instr%store_var, 0)  ! unsupported font
      end if

    case (9)  ! save_undo
      call do_save_undo(instr%store_var)

    case (10) ! restore_undo
      call do_restore_undo(instr%store_var)

    case (11) ! print_unicode
      if (operands(1) < 128) then
        call output_char(char(operands(1)))
      else
        call output_char('?')
      end if

    case (12) ! check_unicode
      if (operands(1) < 128) then
        call var_write(instr%store_var, 3)  ! can input and output
      else
        call var_write(instr%store_var, 0)
      end if

    case default
      ! Many extended opcodes are V6 only; stub them
      continue
    end select
  end subroutine exec_extended

  ! Print a signed number
  subroutine print_number(num)
    integer, intent(in) :: num
    character(len=12) :: buf
    integer :: blen

    write(buf, '(I0)') num
    blen = len_trim(buf)
    call output_string(buf, blen)
  end subroutine print_number

  ! Random number generation (1 to range inclusive)
  subroutine random_number_gen(range, result)
    integer, intent(in) :: range
    integer, intent(out) :: result
    real :: r

    call random_number(r)
    result = int(r * range) + 1
    if (result > range) result = range
  end subroutine random_number_gen

  ! Read a single character from input
  subroutine read_single_char(result)
    integer, intent(out) :: result
    character :: ch

    read(*,'(A1)', advance='no', eor=10) ch
    result = iachar(ch)
    return
10  result = 13  ! newline on end-of-record
  end subroutine read_single_char

  ! Read input (sread/aread)
  subroutine do_read(operands, instr)
    integer, intent(in) :: operands(8)
    type(decoded_instr), intent(in) :: instr
    integer :: text_buf_addr, parse_buf_addr
    integer :: max_chars, input_len, i
    character(len=256) :: input_line
    character :: ch

    text_buf_addr = operands(1)
    parse_buf_addr = operands(2)

    if (text_buf_addr < 0 .or. text_buf_addr + 2 >= mem_size) then
      write(*,*) 'Error: text buffer too small for read'
      exec_running = .false.
      return
    end if
    if (parse_buf_addr /= 0 .and. (parse_buf_addr < 0 .or. parse_buf_addr + 5 >= mem_size)) then
      write(*,*) 'Error: parse buffer too small for read'
      exec_running = .false.
      return
    end if

    if (hdr_version <= 4) then
      ! V1-4: first byte is max chars (including terminator)
      max_chars = mem_read_byte(text_buf_addr) - 1
      ! Show status line for V3
      ! Read line
      write(*,'(A)', advance='no') '> '
      read(*,'(A)', end=100) input_line
      input_len = len_trim(input_line)
      if (input_len > max_chars) input_len = max_chars

      ! Convert to lowercase and store
      do i = 1, input_len
        ch = input_line(i:i)
        if (iachar(ch) >= 65 .and. iachar(ch) <= 90) then
          ch = char(iachar(ch) + 32)
        end if
        call mem_write_byte(text_buf_addr + i, iachar(ch))
      end do
      call mem_write_byte(text_buf_addr + input_len + 1, 0)  ! null terminator
    else
      ! V5+: byte 0 = max chars, byte 1 = num chars already present
      max_chars = mem_read_byte(text_buf_addr)
      write(*,'(A)', advance='no') '> '
      read(*,'(A)', end=100) input_line
      input_len = len_trim(input_line)
      if (input_len > max_chars) input_len = max_chars

      ! Convert to lowercase and store starting at byte 2
      do i = 1, input_len
        ch = input_line(i:i)
        if (iachar(ch) >= 65 .and. iachar(ch) <= 90) then
          ch = char(iachar(ch) + 32)
        end if
        call mem_write_byte(text_buf_addr + 1 + i, iachar(ch))
      end do
      ! Store count
      call mem_write_byte(text_buf_addr + 1, input_len)
    end if

    ! Tokenise if parse buffer given
    if (parse_buf_addr /= 0) then
      call tokenise_input(text_buf_addr, parse_buf_addr, input_len)
    end if

    ! V5+: store terminating character (13 = newline)
    if (hdr_version >= 5 .and. instr%has_store) then
      call var_write(instr%store_var, 13)
    end if
    return

100 continue
    exec_running = .false.
  end subroutine do_read

  ! Tokenise input text into parse buffer
  subroutine tokenise_input(text_addr, parse_addr, input_len, alt_dict, no_overwrite)
    integer, intent(in) :: text_addr, parse_addr, input_len
    integer, intent(in), optional :: alt_dict
    logical, intent(in), optional :: no_overwrite

    integer :: max_tokens, num_tokens, text_start
    integer :: dict_addr, sep_count, separators(32)
    integer :: dict_entry_len, dict_num_entries, dict_entries_start
    integer :: pos, word_start, word_len, i
    character :: ch
    logical :: is_sep, skip_unknown
    character(len=10) :: word_buf
    integer :: encoded(3), enc_words, match_addr

    skip_unknown = .false.
    if (present(no_overwrite)) skip_unknown = no_overwrite

    dict_addr = hdr_dictionary
    if (present(alt_dict) .and. alt_dict /= 0) dict_addr = alt_dict
    ! Read dictionary header
    sep_count = mem_read_byte(dict_addr)
    do i = 1, sep_count
      separators(i) = mem_read_byte(dict_addr + i)
    end do
    dict_entry_len = mem_read_byte(dict_addr + 1 + sep_count)
    ! num_entries is signed: negative means unsorted dictionary
    dict_num_entries = to_signed(mem_read_word(dict_addr + 2 + sep_count))
    dict_entries_start = dict_addr + 4 + sep_count

    max_tokens = mem_read_byte(parse_addr)
    num_tokens = 0

    if (hdr_version <= 4) then
      text_start = text_addr + 1  ! text starts at byte 1
    else
      text_start = text_addr + 2  ! text starts at byte 2
    end if

    pos = 0
    do while (pos < input_len .and. num_tokens < max_tokens)
      ! Skip spaces
      do while (pos < input_len)
        ch = char(mem_read_byte(text_start + pos))
        if (ch /= ' ') exit
        pos = pos + 1
      end do
      if (pos >= input_len) exit

      ! Check if separator
      is_sep = .false.
      do i = 1, sep_count
        if (iachar(ch) == separators(i)) then
          is_sep = .true.
          exit
        end if
      end do

      if (is_sep) then
        ! Single separator character is a word
        word_buf(1:1) = ch
        word_len = 1
        word_start = pos
        pos = pos + 1
      else
        ! Read word until space or separator
        word_start = pos
        word_len = 0
        do while (pos < input_len)
          ch = char(mem_read_byte(text_start + pos))
          if (ch == ' ') exit
          is_sep = .false.
          do i = 1, sep_count
            if (iachar(ch) == separators(i)) then
              is_sep = .true.
              exit
            end if
          end do
          if (is_sep) exit
          word_len = word_len + 1
          if (word_len <= 10) word_buf(word_len:word_len) = ch
          pos = pos + 1
        end do
      end if

      ! Encode word and look up in dictionary
      call text_encode(word_buf, min(word_len, 10), encoded, enc_words)

      ! DEBUG: print encoded word and first few dict entries
      match_addr = dict_lookup(encoded, enc_words, dict_entries_start, &
                               dict_entry_len, dict_num_entries)

      ! Write to parse buffer
      num_tokens = num_tokens + 1
      i = parse_addr + 2 + (num_tokens - 1) * 4
      if (.not. skip_unknown .or. match_addr /= 0) then
        call mem_write_word(i, match_addr)           ! dictionary address (0 if not found)
      end if
      call mem_write_byte(i + 2, word_len)           ! word length
      call mem_write_byte(i + 3, word_start + (text_start - text_addr))  ! position in text buffer
    end do

    call mem_write_byte(parse_addr + 1, num_tokens)
  end subroutine tokenise_input

  ! Look up encoded word in dictionary (binary search)
  function dict_lookup(encoded, enc_words, entries_start, entry_len, num_entries) result(addr)
    integer, intent(in) :: encoded(:), enc_words
    integer, intent(in) :: entries_start, entry_len, num_entries
    integer :: addr
    integer :: lo, hi, mid, j, eaddr, dict_word, cmp, entry_count
    logical :: sorted

    addr = 0
    sorted = (num_entries >= 0)
    entry_count = abs(num_entries)

    if (.not. sorted) then
      do mid = 0, entry_count - 1
        eaddr = entries_start + mid * entry_len
        cmp = 0
        do j = 1, enc_words
          dict_word = mem_read_word(eaddr + (j-1) * 2)
          if (dict_word < encoded(j)) then
            cmp = -1
            exit
          else if (dict_word > encoded(j)) then
            cmp = 1
            exit
          end if
        end do
        if (cmp == 0) then
          addr = eaddr
          return
        end if
      end do
      return
    end if

    lo = 0
    hi = entry_count - 1

    do while (lo <= hi)
      mid = (lo + hi) / 2
      eaddr = entries_start + mid * entry_len

      ! Compare encoded words lexicographically (unsigned)
      cmp = 0
      do j = 1, enc_words
        dict_word = mem_read_word(eaddr + (j-1) * 2)
        if (dict_word < encoded(j)) then
          cmp = -1
          exit
        else if (dict_word > encoded(j)) then
          cmp = 1
          exit
        end if
      end do

      if (cmp == 0) then
        addr = eaddr
        return
      else if (cmp < 0) then
        lo = mid + 1
      else
        hi = mid - 1
      end if
    end do
  end function dict_lookup

  ! scan_table opcode
  subroutine do_scan_table(operands, instr)
    integer, intent(in) :: operands(8)
    type(decoded_instr), intent(in) :: instr
    integer :: x, table, len, form, entry_size, i, val
    logical :: is_word

    x = operands(1)
    table = operands(2)
    len = operands(3)
    if (instr%num_operands >= 4) then
      form = operands(4)
    else
      form = 130  ! $82 = word entries, entry size 2
    end if

    is_word = iand(form, 128) /= 0
    entry_size = iand(form, 127)
    if (entry_size == 0) entry_size = 2

    do i = 0, len - 1
      if (is_word) then
        val = mem_read_word(table + i * entry_size)
      else
        val = mem_read_byte(table + i * entry_size)
      end if
      if (val == x) then
        call var_write(instr%store_var, table + i * entry_size)
        call do_branch(.true., instr)
        return
      end if
    end do

    call var_write(instr%store_var, 0)
    call do_branch(.false., instr)
  end subroutine do_scan_table

  ! copy_table opcode
  subroutine do_copy_table(operands)
    integer, intent(in) :: operands(8)
    integer :: first, second, size, i

    first = operands(1)
    second = operands(2)
    size = to_signed(operands(3))

    if (second == 0) then
      ! Zero out first table
      do i = 0, abs(size) - 1
        call mem_write_byte(first + i, 0)
      end do
    else if (size > 0) then
      ! Copy forward (safe for non-overlapping or backward overlap)
      do i = 0, size - 1
        call mem_write_byte(second + i, mem_read_byte(first + i))
      end do
    else
      ! Negative size: copy backward (safe for forward overlap)
      size = abs(size)
      do i = size - 1, 0, -1
        call mem_write_byte(second + i, mem_read_byte(first + i))
      end do
    end if
  end subroutine do_copy_table

  ! print_table opcode
  subroutine do_print_table(operands, num_ops)
    integer, intent(in) :: operands(8), num_ops
    integer :: zaddr, width, height, skip, row, col

    zaddr = operands(1)
    width = operands(2)
    height = 1
    skip = 0
    if (num_ops >= 3) height = operands(3)
    if (num_ops >= 4) skip = operands(4)

    do row = 1, height
      do col = 1, width
        call output_char(char(mem_read_byte(zaddr)))
        zaddr = zaddr + 1
      end do
      if (row < height) then
        call output_char(char(10))
        zaddr = zaddr + skip
      end if
    end do
  end subroutine do_print_table

  ! tokenise opcode (VAR:251)
  subroutine do_tokenise(operands, num_ops)
    integer, intent(in) :: operands(8), num_ops
    integer :: text_buf, parse_buf, tlen, i, alt_dict
    logical :: no_overwrite

    text_buf = operands(1)
    parse_buf = operands(2)
    alt_dict = 0
    no_overwrite = .false.
    if (num_ops >= 3) alt_dict = operands(3)
    if (num_ops >= 4) no_overwrite = (operands(4) /= 0)

    ! Count text length
    if (hdr_version <= 4) then
      tlen = 0
      do i = 1, 255
        if (mem_read_byte(text_buf + i) == 0) exit
        tlen = tlen + 1
      end do
    else
      tlen = mem_read_byte(text_buf + 1)
    end if

    call tokenise_input(text_buf, parse_buf, tlen, alt_dict, no_overwrite)
  end subroutine do_tokenise

  ! Save undo state (full snapshot: memory, PC, stack, frames, streams)
  subroutine do_save_undo(store_var)
    integer, intent(in) :: store_var

    ! Save dynamic memory
    if (allocated(undo_memory)) deallocate(undo_memory)
    allocate(undo_memory(0:mem_static_base-1))
    undo_memory = memory(0:mem_static_base-1)

    ! Save PC (already pointing past save_undo instruction + store byte)
    undo_pc = exec_pc

    ! Save stack
    call stack_save_state(undo_sp, undo_fp)
    if (allocated(undo_stack)) deallocate(undo_stack)
    if (undo_sp > 0) then
      allocate(undo_stack(undo_sp))
      undo_stack = stack(1:undo_sp)
    end if
    if (allocated(undo_frames)) deallocate(undo_frames)
    if (undo_fp > 0) then
      allocate(undo_frames(undo_fp))
      undo_frames = frames(1:undo_fp)
    end if

    ! Save output stream and window state
    undo_stream1 = output_stream1
    undo_stream2 = output_stream2
    undo_stream3_depth = stream3_depth
    undo_stream3_table = stream3_table
    undo_stream3_pos = stream3_pos
    undo_window = current_window

    undo_valid = .true.
    call var_write(store_var, 1)  ! success
  end subroutine do_save_undo

  ! Restore undo state (full snapshot)
  subroutine do_restore_undo(store_var)
    integer, intent(in) :: store_var
    integer :: saved_store_var

    if (.not. undo_valid) then
      call var_write(store_var, 0)  ! fail
      return
    end if

    ! Restore dynamic memory
    memory(0:mem_static_base-1) = undo_memory

    ! Restore stack
    if (allocated(undo_stack)) stack(1:undo_sp) = undo_stack
    if (allocated(undo_frames)) frames(1:undo_fp) = undo_frames
    call stack_restore_state(undo_sp, undo_fp)

    ! Restore output streams and window state
    output_stream1 = undo_stream1
    output_stream2 = undo_stream2
    stream3_depth = undo_stream3_depth
    stream3_table = undo_stream3_table
    stream3_pos = undo_stream3_pos
    current_window = undo_window

    ! Restore PC and write result into the *original* save_undo's store variable
    exec_pc = undo_pc
    saved_store_var = mem_read_byte(undo_pc - 1)
    call var_write(saved_store_var, 2)  ! 2 = restored
  end subroutine do_restore_undo

  ! Prompt user for save filename and perform save
  ! Quetzal convention: the saved PC points past the entire save instruction
  ! (including store byte / branch data). On restore, the interpreter reads
  ! the store variable byte at (saved_pc - 1) for V4+ or re-evaluates the
  ! branch at the saved PC position for V1-3.
  subroutine do_save(instr)
    type(decoded_instr), intent(in) :: instr
    character(len=256) :: save_filename
    logical :: ok

    write(*,'(A)', advance='no') 'Save filename: '
    read(*,'(A)', end=100) save_filename
    if (len_trim(save_filename) == 0) save_filename = 'save.qzl'

    ! Save with PC pointing past this instruction
    call quetzal_save(exec_pc, trim(save_filename), ok)

    if (ok) then
      write(*,*) 'Saved.'
    else
      write(*,*) 'Save failed.'
    end if

    if (hdr_version <= 3) then
      call do_branch(ok, instr)
    else if (instr%has_store) then
      if (ok) then
        call var_write(instr%store_var, 1)  ! 1 = save succeeded
      else
        call var_write(instr%store_var, 0)
      end if
    end if
    return

100 continue
    ! EOF on filename input
    if (hdr_version <= 3) then
      call do_branch(.false., instr)
    else if (instr%has_store) then
      call var_write(instr%store_var, 0)
    end if
  end subroutine do_save

  ! Prompt user for restore filename and perform restore
  subroutine do_restore(instr)
    type(decoded_instr), intent(in) :: instr
    character(len=256) :: save_filename
    integer :: new_pc, store_var_num
    logical :: ok

    write(*,'(A)', advance='no') 'Restore filename: '
    read(*,'(A)', end=100) save_filename
    if (len_trim(save_filename) == 0) save_filename = 'save.qzl'

    call quetzal_restore(trim(save_filename), new_pc, ok)

    if (ok) then
      write(*,*) 'Restored.'
      exec_pc = new_pc
      if (hdr_version <= 3) then
        ! V1-3: save uses branch. On restore, the saved PC points past
        ! the save instruction's branch data. The game resumes from there.
        ! The save instruction in the saved game already branched, so we
        ! just continue execution from the restored PC.
        continue
      else
        ! V4+: store 2 into the save instruction's result variable.
        ! The saved PC is past the full instruction. For EXT:0 save
        ! (no branch, has store), the store variable byte is the last
        ! byte before any operand-dependent data. We look it up:
        ! the store byte is immediately before the saved PC for
        ! instructions with store and no branch.
        store_var_num = mem_read_byte(new_pc - 1)
        call var_write(store_var_num, 2)  ! 2 = restore succeeded
      end if
    else
      write(*,*) 'Restore failed.'
      if (hdr_version <= 3) then
        call do_branch(.false., instr)
      else if (instr%has_store) then
        call var_write(instr%store_var, 0)
      end if
    end if
    return

100 continue
    if (hdr_version <= 3) then
      call do_branch(.false., instr)
    else if (instr%has_store) then
      call var_write(instr%store_var, 0)
    end if
  end subroutine do_restore

end module execute_mod
