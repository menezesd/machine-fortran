! header_mod.f90 - Z-machine header parsing
! Reads and provides access to all header fields.

module header_mod
  use memory_mod
  implicit none
  private
  public :: header_init, hdr_version, hdr_himem_base, hdr_initial_pc, &
            hdr_dictionary, hdr_object_table, hdr_globals, &
            hdr_static_base, hdr_abbrev_table, hdr_file_length, &
            hdr_checksum, hdr_routine_offset, hdr_string_offset, &
            hdr_alphabet_table, hdr_flags1, hdr_flags2, &
            hdr_print

  integer :: hdr_version = 0
  integer :: hdr_flags1 = 0
  integer :: hdr_himem_base = 0
  integer :: hdr_initial_pc = 0
  integer :: hdr_dictionary = 0
  integer :: hdr_object_table = 0
  integer :: hdr_globals = 0
  integer :: hdr_static_base = 0
  integer :: hdr_flags2 = 0
  integer :: hdr_abbrev_table = 0
  integer :: hdr_file_length = 0
  integer :: hdr_checksum = 0
  integer :: hdr_routine_offset = 0
  integer :: hdr_string_offset = 0
  integer :: hdr_alphabet_table = 0

contains

  subroutine header_init()
    hdr_version       = mem_read_byte(0)
    hdr_flags1        = mem_read_byte(1)
    hdr_himem_base    = mem_read_word(4)
    hdr_initial_pc    = mem_read_word(6)
    hdr_dictionary    = mem_read_word(8)
    hdr_object_table  = mem_read_word(10)  ! $0A
    hdr_globals       = mem_read_word(12)  ! $0C
    hdr_static_base   = mem_read_word(14)  ! $0E
    hdr_flags2        = mem_read_byte(16)  ! $10
    hdr_abbrev_table  = mem_read_word(24)  ! $18
    hdr_file_length   = mem_read_word(26)  ! $1A
    hdr_checksum      = mem_read_word(28)  ! $1C

    ! Scale file length based on version
    select case (hdr_version)
    case (1:3)
      hdr_file_length = hdr_file_length * 2
    case (4:5)
      hdr_file_length = hdr_file_length * 4
    case (6:8)
      hdr_file_length = hdr_file_length * 8
    end select

    if (hdr_version >= 6) then
      hdr_routine_offset = mem_read_word(40)  ! $28
      hdr_string_offset  = mem_read_word(42)  ! $2A
    else
      hdr_routine_offset = 0
      hdr_string_offset  = 0
    end if

    if (hdr_version >= 5) then
      hdr_alphabet_table = mem_read_word(52)  ! $34
    else
      hdr_alphabet_table = 0
    end if

    ! Set interpreter identity in header
    if (hdr_version >= 4) then
      ! Interpreter number: 6 = IBM PC (generic)
      call mem_write_byte(30, 6)   ! $1E - interpreter number
      call mem_write_byte(31, 70)  ! $1F - interpreter version 'F' (Fortran!)
    end if

    ! Set screen dimensions
    if (hdr_version >= 4) then
      call mem_write_byte(32, 25)  ! $20 - screen height (lines)
      call mem_write_byte(33, 80)  ! $21 - screen width (chars)
    end if
    if (hdr_version >= 5) then
      call mem_write_word(34, 80)  ! $22 - screen width (units)
      call mem_write_word(36, 25)  ! $24 - screen height (units)
      call mem_write_byte(38, 1)   ! $26 - font width
      call mem_write_byte(39, 1)   ! $27 - font height
    end if

    ! Set standard revision to 1.1
    call mem_write_byte(50, 1)     ! $32 - standard revision major
    call mem_write_byte(51, 1)     ! $33 - standard revision minor
  end subroutine header_init

  subroutine hdr_print()
    write(*,'(A,I0)') 'Z-machine version: ', hdr_version
    write(*,'(A,Z6)') 'High memory base:  $', hdr_himem_base
    write(*,'(A,Z6)') 'Initial PC:        $', hdr_initial_pc
    write(*,'(A,Z6)') 'Dictionary:        $', hdr_dictionary
    write(*,'(A,Z6)') 'Object table:      $', hdr_object_table
    write(*,'(A,Z6)') 'Globals:           $', hdr_globals
    write(*,'(A,Z6)') 'Static memory:     $', hdr_static_base
    write(*,'(A,Z6)') 'Abbreviations:     $', hdr_abbrev_table
    write(*,'(A,I0)') 'File length:       ', hdr_file_length
    write(*,'(A,Z4)') 'Checksum:          $', hdr_checksum
  end subroutine hdr_print

end module header_mod
