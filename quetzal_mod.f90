! quetzal_mod.f90 - Quetzal save file format (IFF FORM/IFZS)
! Implements the standard Z-machine save file format.
!
! Quetzal file structure:
!   IFF FORM header: "FORM" <size> "IFZS"
!   Chunk "IFhd" (required): header info - release, serial, checksum, PC
!   Chunk "CMem" or "UMem": compressed or uncompressed dynamic memory
!   Chunk "Stks" (required): stack frames and evaluation stack

module quetzal_mod
  use memory_mod
  use header_mod
  use stack_mod
  implicit none
  private
  public :: quetzal_save, quetzal_restore

  ! Buffer for building the save file
  integer, parameter :: MAX_SAVE_SIZE = 1048576  ! 1MB max save
  integer(1) :: savebuf(MAX_SAVE_SIZE)
  integer :: bufpos

contains

  ! ---- Low-level write helpers ----

  subroutine buf_write_byte(b)
    integer, intent(in) :: b
    bufpos = bufpos + 1
    if (bufpos <= MAX_SAVE_SIZE) savebuf(bufpos) = int(iand(b, 255), 1)
  end subroutine buf_write_byte

  subroutine buf_write_word(w)
    integer, intent(in) :: w
    call buf_write_byte(iand(ishft(w, -8), 255))
    call buf_write_byte(iand(w, 255))
  end subroutine buf_write_word

  subroutine buf_write_long(l)
    integer, intent(in) :: l
    call buf_write_byte(iand(ishft(l, -24), 255))
    call buf_write_byte(iand(ishft(l, -16), 255))
    call buf_write_byte(iand(ishft(l, -8), 255))
    call buf_write_byte(iand(l, 255))
  end subroutine buf_write_long

  subroutine buf_write_id(id)
    character(len=4), intent(in) :: id
    integer :: i
    do i = 1, 4
      call buf_write_byte(iachar(id(i:i)))
    end do
  end subroutine buf_write_id

  ! Patch a 4-byte big-endian integer at position pos (1-based)
  subroutine buf_patch_long(pos, l)
    integer, intent(in) :: pos, l
    savebuf(pos)   = int(iand(ishft(l, -24), 255), 1)
    savebuf(pos+1) = int(iand(ishft(l, -16), 255), 1)
    savebuf(pos+2) = int(iand(ishft(l, -8), 255), 1)
    savebuf(pos+3) = int(iand(l, 255), 1)
  end subroutine buf_patch_long

  ! ---- Low-level read helpers ----

  function read_byte_at(buf, pos) result(val)
    integer(1), intent(in) :: buf(:)
    integer, intent(in) :: pos
    integer :: val
    val = iand(int(buf(pos), 4), 255)
  end function read_byte_at

  function read_word_at(buf, pos) result(val)
    integer(1), intent(in) :: buf(:)
    integer, intent(in) :: pos
    integer :: val
    val = ior(ishft(read_byte_at(buf, pos), 8), read_byte_at(buf, pos+1))
  end function read_word_at

  function read_long_at(buf, pos) result(val)
    integer(1), intent(in) :: buf(:)
    integer, intent(in) :: pos
    integer :: val
    val = ior(ior(ishft(read_byte_at(buf, pos), 24), &
                  ishft(read_byte_at(buf, pos+1), 16)), &
              ior(ishft(read_byte_at(buf, pos+2), 8), &
                  read_byte_at(buf, pos+3)))
  end function read_long_at

  function read_id_at(buf, pos) result(id)
    integer(1), intent(in) :: buf(:)
    integer, intent(in) :: pos
    character(len=4) :: id
    integer :: i
    do i = 1, 4
      id(i:i) = char(read_byte_at(buf, pos + i - 1))
    end do
  end function read_id_at

  ! ======== SAVE ========

  subroutine quetzal_save(pc, filename, success)
    integer, intent(in) :: pc
    character(len=*), intent(in) :: filename
    logical, intent(out) :: success

    integer :: form_size_pos
    integer :: iu, ios

    success = .false.
    bufpos = 0

    ! IFF FORM header
    call buf_write_id('FORM')
    form_size_pos = bufpos + 1
    call buf_write_long(0)        ! placeholder for FORM size
    call buf_write_id('IFZS')

    ! ---- IFhd chunk: story file identity + PC ----
    call write_ifhd_chunk(pc)

    ! ---- CMem chunk: compressed dynamic memory ----
    call write_cmem_chunk()

    ! ---- Stks chunk: stack frames ----
    call write_stks_chunk(pc)

    ! Patch FORM size (total size minus 8 bytes for "FORM" + size field)
    call buf_patch_long(form_size_pos, bufpos - 8)

    ! Write to file
    open(newunit=iu, file=filename, status='replace', access='stream', &
         form='unformatted', iostat=ios)
    if (ios /= 0) then
      write(*,*) 'Error: cannot create save file'
      return
    end if

    write(iu, iostat=ios) savebuf(1:bufpos)
    close(iu)

    if (ios /= 0) then
      write(*,*) 'Error: cannot write save file'
      return
    end if

    success = .true.
  end subroutine quetzal_save

  ! Write IFhd chunk: 13 bytes of identification
  subroutine write_ifhd_chunk(pc)
    integer, intent(in) :: pc

    call buf_write_id('IFhd')
    call buf_write_long(13)       ! chunk data size is always 13

    ! Release number (word at $02)
    call buf_write_word(mem_read_word(2))

    ! Serial number (6 bytes at $12)
    call buf_write_byte(mem_read_byte(18))
    call buf_write_byte(mem_read_byte(19))
    call buf_write_byte(mem_read_byte(20))
    call buf_write_byte(mem_read_byte(21))
    call buf_write_byte(mem_read_byte(22))
    call buf_write_byte(mem_read_byte(23))

    ! Checksum (word at $1C)
    call buf_write_word(mem_read_word(28))

    ! PC: 3 bytes, big-endian
    call buf_write_byte(iand(ishft(pc, -16), 255))
    call buf_write_byte(iand(ishft(pc, -8), 255))
    call buf_write_byte(iand(pc, 255))

    ! IFF chunks must be even-aligned; 13 is odd, so add pad byte
    call buf_write_byte(0)
  end subroutine write_ifhd_chunk

  ! Write CMem chunk: XOR-compressed dynamic memory
  subroutine write_cmem_chunk()
    integer :: size_pos, chunk_start, chunk_end
    integer :: i, xor_val, run_length

    call buf_write_id('CMem')
    size_pos = bufpos + 1
    call buf_write_long(0)        ! placeholder
    chunk_start = bufpos

    ! XOR current dynamic memory with original, then run-length encode zeros
    i = 0
    do while (i < mem_static_base)
      xor_val = iand(ieor(int(memory(i), 4), int(mem_original(i), 4)), 255)
      if (xor_val == 0) then
        ! Count run of zeros (max 256 per Quetzal encoding: 0 byte + count-1)
        run_length = 1
        do while (i + run_length < mem_static_base .and. run_length < 256)
          xor_val = iand(ieor(int(memory(i + run_length), 4), &
                              int(mem_original(i + run_length), 4)), 255)
          if (xor_val /= 0) exit
          run_length = run_length + 1
        end do
        ! Write: 0 byte followed by (run_length - 1)
        call buf_write_byte(0)
        call buf_write_byte(run_length - 1)
        i = i + run_length
      else
        call buf_write_byte(xor_val)
        i = i + 1
      end if
    end do

    chunk_end = bufpos
    call buf_patch_long(size_pos, chunk_end - chunk_start)

    ! Pad to even
    if (mod(chunk_end - chunk_start, 2) /= 0) call buf_write_byte(0)
  end subroutine write_cmem_chunk

  ! Write Stks chunk: all stack frames and evaluation stack
  subroutine write_stks_chunk(current_pc)
    integer, intent(in) :: current_pc  ! unused but kept for API symmetry
    integer :: size_pos, chunk_start, chunk_end
    integer :: f, i, eval_count, prev_base, arg_flags

    call buf_write_id('Stks')
    size_pos = bufpos + 1
    call buf_write_long(0)        ! placeholder
    chunk_start = bufpos

    prev_base = 0

    ! Write dummy frame for any stack values before the first call frame
    ! (Quetzal spec requires this for V1-5 where execution starts without
    ! a routine call)
    if (fp > 0) then
      eval_count = frames(1)%stack_base
    else
      eval_count = sp
    end if
    if (eval_count > 0 .or. fp == 0) then
      ! Dummy frame: return PC = 0, no locals, no result
      call buf_write_byte(0)    ! return PC byte 1
      call buf_write_byte(0)    ! return PC byte 2
      call buf_write_byte(0)    ! return PC byte 3
      call buf_write_byte(0)    ! flags (no discard needed)
      call buf_write_byte(0)    ! result variable
      call buf_write_byte(0)    ! arg count
      call buf_write_word(eval_count)  ! eval stack words
      call buf_write_byte(0)    ! 0 locals
      do i = 1, eval_count
        call buf_write_word(stack(i))
      end do
    end if

    do f = 1, fp
      ! Return PC: 3 bytes
      call buf_write_byte(iand(ishft(frames(f)%return_pc, -16), 255))
      call buf_write_byte(iand(ishft(frames(f)%return_pc, -8), 255))
      call buf_write_byte(iand(frames(f)%return_pc, 255))

      ! Flags byte: bit 4 = discard result
      if (frames(f)%return_var == -1) then
        call buf_write_byte(16)   ! bit 4 set = discard
      else
        call buf_write_byte(0)
      end if

      ! Result variable (0 if discarded)
      if (frames(f)%return_var == -1) then
        call buf_write_byte(0)
      else
        call buf_write_byte(frames(f)%return_var)
      end if

      ! Arguments supplied: bitmask in 1 byte
      arg_flags = 0
      do i = 1, frames(f)%arg_count
        arg_flags = ior(arg_flags, ishft(1, i - 1))
      end do
      call buf_write_byte(arg_flags)

      ! Number of evaluation stack words for THIS frame
      if (f < fp) then
        eval_count = frames(f+1)%stack_base - frames(f)%stack_base
      else
        eval_count = sp - frames(f)%stack_base
      end if
      call buf_write_word(eval_count)

      ! Local variables (num_locals words)
      call buf_write_byte(frames(f)%local_count)
      do i = 1, frames(f)%local_count
        call buf_write_word(frames(f)%locals(i))
      end do

      ! Evaluation stack words for this frame
      do i = 1, eval_count
        call buf_write_word(stack(frames(f)%stack_base + i))
      end do
    end do

    chunk_end = bufpos
    call buf_patch_long(size_pos, chunk_end - chunk_start)

    ! Pad to even
    if (mod(chunk_end - chunk_start, 2) /= 0) call buf_write_byte(0)
  end subroutine write_stks_chunk

  ! ======== RESTORE ========

  subroutine quetzal_restore(filename, new_pc, success)
    character(len=*), intent(in) :: filename
    integer, intent(out) :: new_pc
    logical, intent(out) :: success

    integer(1), allocatable :: filebuf(:)
    integer :: iu, ios, file_size, pos
    integer :: form_size, chunk_size
    character(len=4) :: id
    logical :: got_ifhd, got_mem, got_stks
    integer :: saved_flags2

    success = .false.
    new_pc = 0

    ! Read entire file
    inquire(file=filename, size=file_size)
    if (file_size <= 0) then
      write(*,*) 'Error: cannot read save file'
      return
    end if

    allocate(filebuf(file_size))
    open(newunit=iu, file=filename, status='old', access='stream', &
         form='unformatted', iostat=ios)
    if (ios /= 0) then
      write(*,*) 'Error: cannot open save file'
      deallocate(filebuf)
      return
    end if
    read(iu, iostat=ios) filebuf
    close(iu)
    if (ios /= 0) then
      write(*,*) 'Error: cannot read save file'
      deallocate(filebuf)
      return
    end if

    ! Verify IFF FORM / IFZS header
    if (file_size < 12) then
      write(*,*) 'Error: save file too small'
      deallocate(filebuf)
      return
    end if

    id = read_id_at(filebuf, 1)
    if (id /= 'FORM') then
      write(*,*) 'Error: not an IFF file'
      deallocate(filebuf)
      return
    end if

    form_size = read_long_at(filebuf, 5)
    id = read_id_at(filebuf, 9)
    if (id /= 'IFZS') then
      write(*,*) 'Error: not a Quetzal save file'
      deallocate(filebuf)
      return
    end if

    ! Save Flags 2 before restore (spec says to preserve it)
    saved_flags2 = mem_read_word(16)

    ! Parse chunks
    got_ifhd = .false.
    got_mem = .false.
    got_stks = .false.
    pos = 13  ! after "FORM" + size + "IFZS"

    do while (pos + 7 <= file_size)
      id = read_id_at(filebuf, pos)
      chunk_size = read_long_at(filebuf, pos + 4)
      pos = pos + 8  ! skip id + size

      select case (id)
      case ('IFhd')
        call restore_ifhd(filebuf, pos, chunk_size, new_pc, got_ifhd)
        if (.not. got_ifhd) then
          deallocate(filebuf)
          return
        end if

      case ('CMem')
        call restore_cmem(filebuf, pos, chunk_size, got_mem)

      case ('UMem')
        call restore_umem(filebuf, pos, chunk_size, got_mem)

      case ('Stks')
        call restore_stks(filebuf, pos, chunk_size, got_stks)

      case default
        ! Skip unknown chunks
        continue
      end select

      ! Advance past chunk data (+ pad byte if odd size)
      pos = pos + chunk_size
      if (mod(chunk_size, 2) /= 0) pos = pos + 1
    end do

    if (.not. got_ifhd) then
      write(*,*) 'Error: save file missing IFhd chunk'
      deallocate(filebuf)
      return
    end if
    if (.not. got_mem) then
      write(*,*) 'Error: save file missing memory chunk'
      deallocate(filebuf)
      return
    end if
    if (.not. got_stks) then
      write(*,*) 'Error: save file missing Stks chunk'
      deallocate(filebuf)
      return
    end if

    ! Restore preserved Flags 2 (word at $10)
    memory(16) = int(iand(ishft(saved_flags2, -8), 255), 1)
    memory(17) = int(iand(saved_flags2, 255), 1)

    ! Re-set interpreter header fields
    call header_init()

    deallocate(filebuf)
    success = .true.
  end subroutine quetzal_restore

  ! Restore IFhd: verify identity matches current story
  subroutine restore_ifhd(buf, pos, size, new_pc, ok)
    integer(1), intent(in) :: buf(:)
    integer, intent(in) :: pos, size
    integer, intent(out) :: new_pc
    logical, intent(out) :: ok
    integer :: release, checksum, i, serial_byte

    ok = .false.
    if (size < 13) then
      write(*,*) 'Error: IFhd chunk too small'
      return
    end if

    ! Check release number
    release = read_word_at(buf, pos)
    if (release /= mem_read_word(2)) then
      write(*,*) 'Error: save file is from a different game release'
      return
    end if

    ! Check serial number (6 bytes)
    do i = 0, 5
      serial_byte = read_byte_at(buf, pos + 2 + i)
      if (serial_byte /= mem_read_byte(18 + i)) then
        write(*,*) 'Error: save file is from a different game'
        return
      end if
    end do

    ! Check checksum
    checksum = read_word_at(buf, pos + 8)
    if (checksum /= mem_read_word(28)) then
      write(*,*) 'Error: save file checksum mismatch'
      return
    end if

    ! Read PC (3 bytes big-endian)
    new_pc = ior(ior(ishft(read_byte_at(buf, pos + 10), 16), &
                     ishft(read_byte_at(buf, pos + 11), 8)), &
                 read_byte_at(buf, pos + 12))

    ok = .true.
  end subroutine restore_ifhd

  ! Restore CMem: XOR-compressed dynamic memory
  subroutine restore_cmem(buf, pos, size, ok)
    integer(1), intent(in) :: buf(:)
    integer, intent(in) :: pos, size
    logical, intent(out) :: ok
    integer :: i, mem_pos, xor_val, run_len

    ok = .false.

    ! First restore original dynamic memory
    memory(0:mem_static_base-1) = mem_original(0:mem_static_base-1)

    ! Then apply XOR differences
    i = 0
    mem_pos = 0
    do while (i < size .and. mem_pos < mem_static_base)
      xor_val = read_byte_at(buf, pos + i)
      i = i + 1

      if (xor_val /= 0) then
        ! XOR this byte with original
        memory(mem_pos) = int(iand(ieor(int(memory(mem_pos), 4), xor_val), 255), 1)
        mem_pos = mem_pos + 1
      else
        ! Zero byte followed by run length
        if (i >= size) exit
        run_len = read_byte_at(buf, pos + i) + 1
        i = i + 1
        ! These bytes are unchanged from original (already restored above)
        mem_pos = mem_pos + run_len
      end if
    end do

    ok = .true.
  end subroutine restore_cmem

  ! Restore UMem: uncompressed dynamic memory
  subroutine restore_umem(buf, pos, size, ok)
    integer(1), intent(in) :: buf(:)
    integer, intent(in) :: pos, size
    logical, intent(out) :: ok
    integer :: copy_size

    ok = .false.
    copy_size = min(size, mem_static_base)
    memory(0:copy_size-1) = buf(pos:pos+copy_size-1)
    ok = .true.
  end subroutine restore_umem

  ! Restore Stks: stack frames and evaluation stack
  subroutine restore_stks(buf, pos, size, ok)
    integer(1), intent(in) :: buf(:)
    integer, intent(in) :: pos, size
    logical, intent(out) :: ok
    integer :: p, return_pc, flags_byte, result_var
    integer :: arg_supplied, eval_count, num_locals
    integer :: j, arg_count

    ok = .false.

    ! Reset stack state
    sp = 0
    fp = 0

    p = pos
    do while (p < pos + size)
      ! Read return PC (3 bytes)
      return_pc = ior(ior(ishft(read_byte_at(buf, p), 16), &
                          ishft(read_byte_at(buf, p+1), 8)), &
                      read_byte_at(buf, p+2))
      p = p + 3

      ! Flags
      flags_byte = read_byte_at(buf, p)
      p = p + 1

      ! Result variable
      result_var = read_byte_at(buf, p)
      p = p + 1
      if (iand(flags_byte, 16) /= 0) result_var = -1  ! discard

      ! Arguments supplied bitmask
      arg_supplied = read_byte_at(buf, p)
      p = p + 1
      arg_count = 0
      do j = 0, 6
        if (iand(arg_supplied, ishft(1, j)) /= 0) arg_count = arg_count + 1
      end do

      ! Evaluation stack count for this frame
      eval_count = read_word_at(buf, p)
      p = p + 2

      ! Number of local variables
      num_locals = read_byte_at(buf, p)
      p = p + 1

      if (return_pc == 0 .and. num_locals == 0 .and. fp == 0) then
        ! Dummy frame: just put values on the stack, don't create a frame
        ! Skip local variables (there are none)
        do j = 1, eval_count
          sp = sp + 1
          stack(sp) = read_word_at(buf, p)
          p = p + 2
        end do
      else
        ! Create a real call frame
        fp = fp + 1
        frames(fp)%return_pc = return_pc
        frames(fp)%return_var = result_var
        frames(fp)%local_count = num_locals
        frames(fp)%stack_base = sp
        frames(fp)%arg_count = arg_count
        frames(fp)%locals = 0

        ! Read local variables
        do j = 1, num_locals
          frames(fp)%locals(j) = read_word_at(buf, p)
          p = p + 2
        end do

        ! Read evaluation stack entries
        do j = 1, eval_count
          sp = sp + 1
          stack(sp) = read_word_at(buf, p)
          p = p + 2
        end do
      end if
    end do

    ok = .true.
  end subroutine restore_stks

end module quetzal_mod
