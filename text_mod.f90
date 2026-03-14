! text_mod.f90 - Z-machine text encoding/decoding
! Handles ZSCII, Z-characters, alphabets, and abbreviations.

module text_mod
  use memory_mod
  use header_mod, only: hdr_version, hdr_abbrev_table, hdr_alphabet_table
  implicit none
  private
  public :: text_decode, text_print_zstr, text_encode, text_zscii_to_char

  ! Default alphabet tables
  character(len=26), parameter :: alpha0 = 'abcdefghijklmnopqrstuvwxyz'
  character(len=26), parameter :: alpha1 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  ! A2 char 7 is the ZSCII escape, represented as '^' placeholder
  character(len=26), parameter :: alpha2_default = '^' // char(13) // '0123456789.,!?_#' // "'" // '"/\-:()'

  integer, parameter :: MAX_ZCHARS = 1024

contains

  ! Get alphabet character for given alphabet (0-2) and z-char index (6-31)
  function get_alpha_char(alphabet, zchar) result(ch)
    integer, intent(in) :: alphabet, zchar
    character :: ch
    integer :: idx, addr

    idx = zchar - 6  ! 0-25 index

    if (hdr_alphabet_table /= 0) then
      ! Custom alphabet table
      addr = hdr_alphabet_table + alphabet * 26 + idx
      ch = char(mem_read_byte(addr))
      return
    end if

    select case (alphabet)
    case (0)
      ch = alpha0(idx+1:idx+1)
    case (1)
      ch = alpha1(idx+1:idx+1)
    case (2)
      if (idx == 0) then
        ch = char(0)  ! ZSCII escape marker
      else
        ! Index into alpha2 (skip the escape at position 0)
        select case (idx)
        case (1)
          ch = char(13)  ! newline
        case default
          ch = alpha2_default(idx+1:idx+1)
        end select
      end if
    case default
      ch = '?'
    end select
  end function get_alpha_char

  ! Decode Z-characters starting at byte address, return decoded string
  ! Also returns the address after the last word of encoded text.
  subroutine text_decode(addr, output, output_len, next_addr)
    integer, intent(in) :: addr
    character(len=*), intent(out) :: output
    integer, intent(out) :: output_len
    integer, intent(out) :: next_addr

    integer :: zchars(MAX_ZCHARS)
    integer :: num_zchars, cur_addr, word
    logical :: done

    ! First, unpack all z-characters from 2-byte words
    num_zchars = 0
    cur_addr = addr
    done = .false.

    do while (.not. done)
      word = mem_read_word(cur_addr)
      cur_addr = cur_addr + 2

      ! Each word has 3 z-characters of 5 bits each
      ! Bit 15 = end flag, bits 14-10 = zchar1, 9-5 = zchar2, 4-0 = zchar3
      num_zchars = num_zchars + 1
      zchars(num_zchars) = iand(ishft(word, -10), 31)
      num_zchars = num_zchars + 1
      zchars(num_zchars) = iand(ishft(word, -5), 31)
      num_zchars = num_zchars + 1
      zchars(num_zchars) = iand(word, 31)

      ! Check end bit (bit 15)
      if (iand(word, 32768) /= 0) done = .true.

      if (num_zchars >= MAX_ZCHARS - 3) done = .true.
    end do

    next_addr = cur_addr

    ! Now decode z-characters into output string
    call decode_zchars(zchars, num_zchars, output, output_len)
  end subroutine text_decode

  ! Internal: decode z-character array to string
  recursive subroutine decode_zchars(zchars, num_zchars, output, output_len)
    integer, intent(in) :: zchars(:), num_zchars
    character(len=*), intent(out) :: output
    integer, intent(out) :: output_len

    integer :: i, zc, alphabet, state
    integer :: abbrev_idx, abbrev_addr, abbrev_ptr
    character(len=256) :: abbrev_text
    integer :: abbrev_len, dummy_addr
    integer :: zscii_hi, zscii_lo, zscii_val

    ! States: 0=normal, 1=expect abbreviation index,
    !         2=expect ZSCII high bits, 3=expect ZSCII low bits
    integer :: abbrev_base

    output_len = 0
    alphabet = 0
    state = 0
    abbrev_base = 0
    zscii_hi = 0
    i = 1

    do while (i <= num_zchars)
      zc = zchars(i)
      i = i + 1

      select case (state)
      case (0)  ! Normal
        select case (zc)
        case (0)
          ! Space
          output_len = output_len + 1
          if (output_len <= len(output)) output(output_len:output_len) = ' '

        case (1)
          if (hdr_version == 1) then
            ! V1: newline
            output_len = output_len + 1
            if (output_len <= len(output)) output(output_len:output_len) = char(10)
          else
            ! V2+: abbreviation with base 0
            abbrev_base = 0
            state = 1
          end if

        case (2)
          if (hdr_version >= 3) then
            abbrev_base = 32
            state = 1
          else
            ! V1-2: shift to A1
            alphabet = 1
          end if

        case (3)
          if (hdr_version >= 3) then
            abbrev_base = 64
            state = 1
          else
            ! V1-2: shift to A2
            alphabet = 2
          end if

        case (4)
          ! Shift to A1 (V3+) or shift-lock (V1-2)
          if (hdr_version <= 2) then
            alphabet = mod(alphabet + 1, 3)  ! shift lock
          else
            alphabet = 1  ! single shift
          end if

        case (5)
          ! Shift to A2
          if (hdr_version <= 2) then
            alphabet = mod(alphabet + 2, 3)  ! shift lock
          else
            alphabet = 2
          end if

        case (6:31)
          if (alphabet == 2 .and. zc == 6) then
            ! ZSCII escape: next two z-chars form a 10-bit ZSCII code
            state = 2
          else
            ! Normal alphabet character
            output_len = output_len + 1
            if (output_len <= len(output)) then
              output(output_len:output_len) = get_alpha_char(alphabet, zc)
            end if
          end if
          ! Reset alphabet after character (V3+ single shift)
          if (hdr_version >= 3) alphabet = 0

        end select

      case (1)  ! Expecting abbreviation index
        abbrev_idx = abbrev_base + zc
        ! Look up abbreviation address
        abbrev_ptr = hdr_abbrev_table + abbrev_idx * 2
        abbrev_addr = mem_read_word(abbrev_ptr) * 2  ! word address -> byte address
        call text_decode(abbrev_addr, abbrev_text, abbrev_len, dummy_addr)
        ! Append abbreviation text
        do abbrev_idx = 1, min(abbrev_len, 256)
          output_len = output_len + 1
          if (output_len <= len(output)) then
            output(output_len:output_len) = abbrev_text(abbrev_idx:abbrev_idx)
          end if
        end do
        state = 0

      case (2)  ! ZSCII high 5 bits
        zscii_hi = zc
        state = 3

      case (3)  ! ZSCII low 5 bits
        zscii_lo = zc
        zscii_val = ior(ishft(zscii_hi, 5), zscii_lo)
        output_len = output_len + 1
        if (output_len <= len(output)) then
          output(output_len:output_len) = text_zscii_to_char(zscii_val)
        end if
        state = 0
        alphabet = 0

      end select
    end do
  end subroutine decode_zchars

  ! Print a Z-encoded string at the given address
  subroutine text_print_zstr(addr, next_addr)
    integer, intent(in) :: addr
    integer, intent(out) :: next_addr
    character(len=1024) :: text
    integer :: tlen

    call text_decode(addr, text, tlen, next_addr)
    if (tlen > 0) write(*,'(A)', advance='no') text(1:tlen)
  end subroutine text_print_zstr

  ! Convert ZSCII code to Fortran character
  function text_zscii_to_char(zscii) result(ch)
    integer, intent(in) :: zscii
    character :: ch

    if (zscii == 0) then
      ch = char(0)
    else if (zscii == 13) then
      ch = char(10)  ! newline
    else if (zscii >= 32 .and. zscii <= 126) then
      ch = char(zscii)  ! standard ASCII
    else
      ch = '?'  ! unknown
    end if
  end function text_zscii_to_char

  ! Encode a string to Z-characters (for dictionary lookup)
  ! V3: 6 z-chars (3 words), V4+: 9 z-chars
  subroutine text_encode(input, input_len, encoded, encoded_words)
    character(len=*), intent(in) :: input
    integer, intent(in) :: input_len
    integer, intent(out) :: encoded(:)
    integer, intent(out) :: encoded_words

    integer :: max_zchars, zchars(12), nzc, i, j, c
    character :: ch
    logical :: found

    if (hdr_version <= 3) then
      max_zchars = 6
      encoded_words = 2  ! V1-3: 4 bytes = 2 words
    else
      max_zchars = 9
      encoded_words = 3  ! V4+: 6 bytes = 3 words
    end if

    nzc = 0

    do i = 1, input_len
      if (nzc >= max_zchars) exit
      ch = input(i:i)

      ! Check A0
      found = .false.
      do j = 1, 26
        if (ch == alpha0(j:j)) then
          nzc = nzc + 1
          zchars(nzc) = j + 5
          found = .true.
          exit
        end if
      end do
      if (found) cycle

      ! Check A1
      do j = 1, 26
        if (ch == alpha1(j:j)) then
          nzc = nzc + 1
          zchars(nzc) = 4  ! shift to A1
          if (nzc < max_zchars) then
            nzc = nzc + 1
            zchars(nzc) = j + 5
          end if
          found = .true.
          exit
        end if
      end do
      if (found) cycle

      ! Space
      if (ch == ' ') then
        nzc = nzc + 1
        zchars(nzc) = 0
        cycle
      end if

      ! Check A2 alphabet (skip index 0 which is the ZSCII escape)
      do j = 2, 26
        if (ch == alpha2_default(j+1:j+1)) then
          nzc = nzc + 1
          zchars(nzc) = 5  ! shift to A2
          if (nzc < max_zchars) then
            nzc = nzc + 1
            zchars(nzc) = j + 5
          end if
          found = .true.
          exit
        end if
      end do
      if (found) cycle

      ! ZSCII escape for remaining characters
      c = iachar(ch)
      if (nzc + 3 <= max_zchars) then
        nzc = nzc + 1
        zchars(nzc) = 5  ! shift to A2
        nzc = nzc + 1
        zchars(nzc) = 6  ! ZSCII escape
        nzc = nzc + 1
        zchars(nzc) = iand(ishft(c, -5), 31)
        if (nzc < max_zchars) then
          nzc = nzc + 1
          zchars(nzc) = iand(c, 31)
        end if
      end if
    end do

    ! Pad with 5s
    do while (nzc < max_zchars)
      nzc = nzc + 1
      zchars(nzc) = 5
    end do

    ! Pack into words
    do i = 1, encoded_words
      j = (i - 1) * 3 + 1
      encoded(i) = ior(ior(ishft(zchars(j), 10), ishft(zchars(j+1), 5)), zchars(j+2))
      if (i == encoded_words) then
        encoded(i) = ior(encoded(i), 32768)  ! set end bit
      end if
    end do
  end subroutine text_encode

end module text_mod
