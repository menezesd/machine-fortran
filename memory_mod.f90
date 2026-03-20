! memory_mod.f90 - Z-machine memory management
! Handles the raw memory array, byte/word access, and story file loading.

module memory_mod
  implicit none
  private
  public :: mem_init, mem_load_story, mem_read_byte, mem_write_byte, &
            mem_read_word, mem_write_word, mem_size, memory, &
            mem_static_base, mem_high_base, &
            mem_original, mem_story_filename

  integer, parameter :: MAX_MEMORY = 524288  ! 512K max for v6-8

  ! Memory stored as unsigned bytes (0-255) in integer array
  integer(1), dimension(0:MAX_MEMORY-1), target :: memory
  integer :: mem_size = 0
  integer :: mem_static_base = 0
  integer :: mem_high_base = 0

  ! Original story file dynamic memory (for save compression)
  integer(1), dimension(0:MAX_MEMORY-1) :: mem_original
  character(len=256) :: mem_story_filename = ''

contains

  subroutine mem_init()
    memory = 0
    mem_size = 0
    mem_static_base = 0
    mem_high_base = 0
  end subroutine mem_init

  subroutine mem_load_story(filename, ierr)
    character(len=*), intent(in) :: filename
    integer, intent(out) :: ierr
    integer :: iu, file_size
    logical :: exists

    ierr = 0
    inquire(file=filename, exist=exists, size=file_size)
    if (.not. exists) then
      write(*,*) 'Error: story file not found: ', trim(filename)
      ierr = 1
      return
    end if

    if (file_size > MAX_MEMORY) then
      write(*,*) 'Error: story file too large: ', file_size, ' bytes'
      ierr = 2
      return
    end if

    open(newunit=iu, file=filename, status='old', access='stream', &
         form='unformatted', iostat=ierr)
    if (ierr /= 0) then
      write(*,*) 'Error: cannot open story file'
      return
    end if

    read(iu, iostat=ierr) memory(0:file_size-1)
    close(iu)

    if (ierr /= 0) then
      write(*,*) 'Error: cannot read story file'
      return
    end if

    mem_size = file_size
    mem_story_filename = filename

    ! Save original dynamic memory for save-file compression
    mem_original(0:file_size-1) = memory(0:file_size-1)

    ! Read static memory base from header word at $0E
    mem_static_base = mem_read_word(14)  ! $0E
    ! Read high memory base from header word at $04
    mem_high_base = mem_read_word(4)     ! $04

    ierr = 0
  end subroutine mem_load_story

  ! Read unsigned byte from address
  function mem_read_byte(addr) result(val)
    integer, intent(in) :: addr
    integer :: val

    if (addr < 0 .or. addr >= mem_size) then
      write(*,'(A,Z8)') 'Error: memory read out of bounds at $', addr
      val = 0
      return
    end if
    val = iand(int(memory(addr), 4), 255)
  end function mem_read_byte

  ! Write byte to address
  subroutine mem_write_byte(addr, val)
    integer, intent(in) :: addr, val

    if (addr < 0 .or. addr >= mem_size) then
      return
    end if
    if (addr >= mem_static_base) then
      write(*,'(A,Z8)') 'Error: illegal write to static memory at $', addr
      stop 1
    end if
    memory(addr) = int(val, 1)
  end subroutine mem_write_byte

  ! Read unsigned 16-bit word (big-endian) from address
  function mem_read_word(addr) result(val)
    integer, intent(in) :: addr
    integer :: val
    val = ior(ishft(mem_read_byte(addr), 8), mem_read_byte(addr+1))
  end function mem_read_word

  ! Write 16-bit word (big-endian) to address
  subroutine mem_write_word(addr, val)
    integer, intent(in) :: addr, val
    call mem_write_byte(addr, iand(ishft(val, -8), 255))
    call mem_write_byte(addr+1, iand(val, 255))
  end subroutine mem_write_word

end module memory_mod
