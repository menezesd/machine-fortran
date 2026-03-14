! zmachine.f90 - Z-machine interpreter main program
! A Z-machine interpreter written in Fortran 90.
! Supports versions 3, 5, and 8.

program zmachine
  use memory_mod
  use header_mod
  use stack_mod
  use text_mod
  use object_mod
  use decode_mod
  use execute_mod
  implicit none

  character(len=256) :: filename
  integer :: ierr, nargs

  nargs = command_argument_count()
  if (nargs < 1) then
    write(*,*) 'Usage: zmachine <story-file>'
    stop 1
  end if

  call get_command_argument(1, filename)

  ! Initialize memory and load story file
  call mem_init()
  call mem_load_story(trim(filename), ierr)
  if (ierr /= 0) stop 1

  ! Parse header
  call header_init()

  ! Validate version
  if (hdr_version < 1 .or. hdr_version > 8) then
    write(*,'(A,I0)') 'Error: unsupported Z-machine version ', hdr_version
    stop 1
  end if

  ! Print header info
  call hdr_print()
  write(*,*)

  ! Initialize execution
  call exec_init()

  ! Run
  call exec_run()

end program zmachine
