! stack_mod.f90 - Z-machine stack and routine call frame management
! Manages the evaluation stack, local variables, and routine call frames.

module stack_mod
  use memory_mod
  use header_mod, only: hdr_version, hdr_globals
  implicit none
  private
  public :: stack_init, stack_push, stack_pop, stack_peek, stack_poke, &
            frame_push, frame_pop, &
            var_read, var_write, var_indirect_write, &
            stack_get_frame_id, stack_frame_count, &
            frame_arg_count, &
            stack_save_state, stack_restore_state, &
            stack, sp, frames, fp, call_frame

  integer, parameter :: MAX_STACK = 65536
  integer, parameter :: MAX_FRAMES = 1024

  ! The evaluation stack
  integer :: stack(MAX_STACK)
  integer :: sp = 0  ! stack pointer (points to top element, 0 = empty)

  ! Call frame info
  type :: call_frame
    integer :: return_pc       ! PC to return to
    integer :: return_var      ! variable to store result (or -1 for discard)
    integer :: local_count     ! number of local variables
    integer :: locals(15)      ! local variable values
    integer :: stack_base      ! stack pointer at frame entry
    integer :: arg_count       ! number of arguments passed
  end type call_frame

  type(call_frame) :: frames(MAX_FRAMES)
  integer :: fp = 0  ! frame pointer (current frame index, 0 = no frames)

contains

  subroutine stack_init()
    sp = 0
    fp = 0
    stack = 0
  end subroutine stack_init

  subroutine stack_push(val)
    integer, intent(in) :: val
    if (sp >= MAX_STACK) then
      write(*,*) 'Error: stack overflow'
      stop 1
    end if
    sp = sp + 1
    stack(sp) = iand(val, 65535)
  end subroutine stack_push

  function stack_pop() result(val)
    integer :: val
    integer :: base
    base = 0
    if (fp > 0) base = frames(fp)%stack_base
    if (sp <= base) then
      ! Silently return 0 on underflow - some games trigger this
      ! through status line / window management paths
      val = 0
      return
    end if
    val = stack(sp)
    sp = sp - 1
  end function stack_pop

  function stack_peek() result(val)
    integer :: val
    if (sp <= 0) then
      write(*,*) 'Error: stack peek on empty stack'
      val = 0
      return
    end if
    val = stack(sp)
  end function stack_peek

  ! Write to top of stack in place (for indirect variable references)
  subroutine stack_poke(val)
    integer, intent(in) :: val
    if (sp <= 0) then
      write(*,*) 'Error: stack poke on empty stack'
      return
    end if
    stack(sp) = iand(val, 65535)
  end subroutine stack_poke

  ! Push a new routine call frame
  subroutine frame_push(return_pc, return_var, num_locals, init_vals, &
                         args, num_args)
    integer, intent(in) :: return_pc, return_var, num_locals
    integer, intent(in) :: init_vals(15)
    integer, intent(in) :: args(:)
    integer, intent(in) :: num_args
    integer :: i

    if (fp >= MAX_FRAMES) then
      write(*,*) 'Error: call stack overflow'
      stop 1
    end if

    fp = fp + 1
    frames(fp)%return_pc = return_pc
    frames(fp)%return_var = return_var
    frames(fp)%local_count = num_locals
    frames(fp)%stack_base = sp
    frames(fp)%arg_count = num_args

    ! Initialize locals from routine header defaults
    frames(fp)%locals = 0
    do i = 1, num_locals
      frames(fp)%locals(i) = init_vals(i)
    end do

    ! Override with arguments
    do i = 1, min(num_args, num_locals)
      frames(fp)%locals(i) = iand(args(i), 65535)
    end do
  end subroutine frame_push

  ! Pop a call frame, returning (return_pc, return_var)
  subroutine frame_pop(return_pc, return_var)
    integer, intent(out) :: return_pc, return_var

    if (fp <= 0) then
      write(*,*) 'Error: call stack underflow'
      stop 1
    end if

    return_pc = frames(fp)%return_pc
    return_var = frames(fp)%return_var
    sp = frames(fp)%stack_base  ! discard routine's stack
    fp = fp - 1
  end subroutine frame_pop

  ! Read a variable: 0=stack, 1-15=local, 16-255=global
  function var_read(varnum) result(val)
    integer, intent(in) :: varnum
    integer :: val, gaddr

    if (varnum == 0) then
      ! Stack top (pop)
      val = stack_pop()
    else if (varnum >= 1 .and. varnum <= 15) then
      ! Local variable
      if (fp <= 0 .or. varnum > frames(fp)%local_count) then
        write(*,'(A,I0)') 'Error: invalid local variable ', varnum
        val = 0
        return
      end if
      val = frames(fp)%locals(varnum)
    else
      ! Global variable (16-255)
      gaddr = hdr_globals + (varnum - 16) * 2
      val = mem_read_word(gaddr)
    end if
  end function var_read

  ! Write a variable
  subroutine var_write(varnum, val)
    integer, intent(in) :: varnum, val
    integer :: gaddr

    if (varnum == 0) then
      ! Stack (push)
      call stack_push(val)
    else if (varnum >= 1 .and. varnum <= 15) then
      if (fp <= 0 .or. varnum > frames(fp)%local_count) then
        write(*,'(A,I0)') 'Error: invalid local variable ', varnum
        return
      end if
      frames(fp)%locals(varnum) = iand(val, 65535)
    else
      ! Global variable
      gaddr = hdr_globals + (varnum - 16) * 2
      call mem_write_word(gaddr, iand(val, 65535))
    end if
  end subroutine var_write

  ! Write a variable with indirect semantics (var 0 = poke stack top, not push)
  ! Used by @store, @inc, @dec, @inc_chk, @dec_chk, @pull
  subroutine var_indirect_write(varnum, val)
    integer, intent(in) :: varnum, val

    if (varnum == 0) then
      call stack_poke(val)
    else
      call var_write(varnum, val)
    end if
  end subroutine var_indirect_write

  function stack_get_frame_id() result(fid)
    integer :: fid
    fid = fp
  end function stack_get_frame_id

  function stack_frame_count() result(n)
    integer :: n
    n = fp
  end function stack_frame_count

  function frame_arg_count() result(n)
    integer :: n
    if (fp <= 0) then
      n = 0
    else
      n = frames(fp)%arg_count
    end if
  end function frame_arg_count

  ! Save complete stack state to arrays for serialization
  subroutine stack_save_state(out_sp, out_fp)
    integer, intent(out) :: out_sp, out_fp
    out_sp = sp
    out_fp = fp
  end subroutine stack_save_state

  ! Restore complete stack state from deserialized data
  subroutine stack_restore_state(in_sp, in_fp)
    integer, intent(in) :: in_sp, in_fp
    sp = in_sp
    fp = in_fp
  end subroutine stack_restore_state

end module stack_mod
