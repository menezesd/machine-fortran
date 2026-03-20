! decode_mod.f90 - Z-machine instruction decoder
! Decodes instruction bytes into opcode, operands, and metadata.

module decode_mod
  use memory_mod
  use header_mod, only: hdr_version
  implicit none
  private
  public :: decode_instruction, decoded_instr, &
            OP_LARGE, OP_SMALL, OP_VAR, OP_OMIT, &
            FORM_LONG, FORM_SHORT, FORM_EXT, FORM_VAR, &
            COUNT_0OP, COUNT_1OP, COUNT_2OP, COUNT_VAR

  integer, parameter :: OP_LARGE = 0
  integer, parameter :: OP_SMALL = 1
  integer, parameter :: OP_VAR   = 2
  integer, parameter :: OP_OMIT  = 3

  integer, parameter :: FORM_LONG  = 0
  integer, parameter :: FORM_SHORT = 1
  integer, parameter :: FORM_EXT   = 2
  integer, parameter :: FORM_VAR   = 3

  integer, parameter :: COUNT_0OP = 0
  integer, parameter :: COUNT_1OP = 1
  integer, parameter :: COUNT_2OP = 2
  integer, parameter :: COUNT_VAR = 3

  type :: decoded_instr
    integer :: opcode         ! opcode number
    integer :: form           ! FORM_LONG/SHORT/EXT/VAR
    integer :: op_count_class ! COUNT_0OP/1OP/2OP/VAR
    integer :: num_operands   ! actual number of operands
    integer :: operands(8)    ! operand values
    integer :: op_types(8)    ! operand types
    logical :: has_store      ! does this instruction store a result?
    integer :: store_var      ! variable to store result in
    logical :: has_branch     ! does this instruction branch?
    logical :: branch_on_true ! branch condition
    integer :: branch_offset  ! branch offset
    logical :: has_text       ! does this instruction have inline text?
    integer :: text_addr      ! address of inline text
    integer :: next_pc        ! PC after this instruction
  end type decoded_instr

contains

  subroutine decode_instruction(pc, instr)
    integer, intent(in) :: pc
    type(decoded_instr), intent(out) :: instr
    integer :: addr, opbyte, top2, optypes_byte, optypes_byte2
    integer :: i, otype

    addr = pc
    instr%num_operands = 0
    instr%has_store = .false.
    instr%has_branch = .false.
    instr%has_text = .false.
    instr%operands = 0
    instr%op_types = OP_OMIT

    opbyte = mem_read_byte(addr)
    addr = addr + 1
    top2 = iand(ishft(opbyte, -6), 3)

    ! Determine form
    if (top2 == 3) then
      ! Variable form: $C0-$FF
      instr%form = FORM_VAR
      if (iand(opbyte, 32) == 0) then
        instr%op_count_class = COUNT_2OP
      else
        instr%op_count_class = COUNT_VAR
      end if
      instr%opcode = iand(opbyte, 31)
    else if (top2 == 2) then
      ! Short form: $80-$BF
      instr%form = FORM_SHORT
      otype = iand(ishft(opbyte, -4), 3)
      if (otype == 3) then
        instr%op_count_class = COUNT_0OP
      else
        instr%op_count_class = COUNT_1OP
      end if
      instr%opcode = iand(opbyte, 15)

      ! Handle extended form: $BE (legal only in V5+)
      if (opbyte == 190 .and. hdr_version >= 5) then  ! $BE
        instr%form = FORM_EXT
        instr%op_count_class = COUNT_VAR
        instr%opcode = mem_read_byte(addr)
        addr = addr + 1
      end if
    else
      ! Long form: $00-$7F
      instr%form = FORM_LONG
      instr%op_count_class = COUNT_2OP
      instr%opcode = iand(opbyte, 31)
    end if

    ! Read operands based on form
    select case (instr%form)
    case (FORM_LONG)
      ! Bit 6: first operand type (0=small, 1=var)
      ! Bit 5: second operand type (0=small, 1=var)
      instr%num_operands = 2
      if (iand(opbyte, 64) /= 0) then
        instr%op_types(1) = OP_VAR
      else
        instr%op_types(1) = OP_SMALL
      end if
      if (iand(opbyte, 32) /= 0) then
        instr%op_types(2) = OP_VAR
      else
        instr%op_types(2) = OP_SMALL
      end if

      do i = 1, 2
        if (instr%op_types(i) == OP_SMALL .or. instr%op_types(i) == OP_VAR) then
          instr%operands(i) = mem_read_byte(addr)
          addr = addr + 1
        end if
      end do

    case (FORM_SHORT)
      if (instr%op_count_class == COUNT_1OP) then
        otype = iand(ishft(opbyte, -4), 3)
        instr%num_operands = 1
        instr%op_types(1) = otype
        select case (otype)
        case (OP_LARGE)
          instr%operands(1) = mem_read_word(addr)
          addr = addr + 2
        case (OP_SMALL)
          instr%operands(1) = mem_read_byte(addr)
          addr = addr + 1
        case (OP_VAR)
          instr%operands(1) = mem_read_byte(addr)
          addr = addr + 1
        end select
      end if
      ! 0OP has no operands to read

    case (FORM_VAR, FORM_EXT)
      ! Read operand type byte(s)
      optypes_byte = mem_read_byte(addr)
      addr = addr + 1

      ! Double-VAR for call_vs2 (VAR:236/$0C) and call_vn2 (VAR:250/$1A)
      if (instr%form == FORM_VAR .and. &
          (instr%opcode == 12 .or. instr%opcode == 26) .and. &
          instr%op_count_class == COUNT_VAR) then
        optypes_byte2 = mem_read_byte(addr)
        addr = addr + 1
      else
        optypes_byte2 = 255  ! all omitted
      end if

      ! Decode up to 8 operand types
      instr%num_operands = 0
      do i = 1, 4
        otype = iand(ishft(optypes_byte, -(2*(4-i))), 3)
        if (otype == OP_OMIT) exit
        instr%num_operands = instr%num_operands + 1
        instr%op_types(instr%num_operands) = otype
      end do

      ! Second type byte (for 8-operand instructions)
      if (optypes_byte2 /= 255) then
        do i = 1, 4
          otype = iand(ishft(optypes_byte2, -(2*(4-i))), 3)
          if (otype == OP_OMIT) exit
          instr%num_operands = instr%num_operands + 1
          instr%op_types(instr%num_operands) = otype
        end do
      end if

      ! Read operand values
      do i = 1, instr%num_operands
        select case (instr%op_types(i))
        case (OP_LARGE)
          instr%operands(i) = mem_read_word(addr)
          addr = addr + 2
        case (OP_SMALL, OP_VAR)
          instr%operands(i) = mem_read_byte(addr)
          addr = addr + 1
        end select
      end do
    end select

    ! Resolve variable operands
    ! (This is done in the execution phase, not here, to avoid side effects
    !  of stack pops during decode. We just note the types.)

    instr%next_pc = addr
  end subroutine decode_instruction

end module decode_mod
