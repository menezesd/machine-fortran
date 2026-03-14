! object_mod.f90 - Z-machine object table operations
! Handles object tree, attributes, and properties.

module object_mod
  use memory_mod
  use header_mod, only: hdr_version, hdr_object_table
  use text_mod, only: text_decode
  implicit none
  private
  public :: obj_get_parent, obj_get_sibling, obj_get_child, &
            obj_set_parent, obj_set_sibling, obj_set_child, &
            obj_get_attr, obj_set_attr, obj_clear_attr, &
            obj_get_prop, obj_get_prop_addr, obj_get_next_prop, &
            obj_get_prop_len, obj_put_prop, &
            obj_insert, obj_remove, obj_print_short_name, &
            obj_get_prop_default

  ! V1-3: 31 attributes, 4 bytes attrs, 1-byte parent/sibling/child, property addr = 2 bytes
  ! Object entry size: 4 + 3*1 + 2 = 9 bytes
  ! Property defaults: 31 words = 62 bytes

  ! V4+: 48 attributes, 6 bytes attrs, 2-byte parent/sibling/child, property addr = 2 bytes
  ! Object entry size: 6 + 3*2 + 2 = 14 bytes
  ! Property defaults: 63 words = 126 bytes

contains

  ! Get base address of property defaults table
  function prop_defaults_addr() result(addr)
    integer :: addr
    addr = hdr_object_table
  end function prop_defaults_addr

  ! Get address of object entry (1-indexed)
  function obj_addr(obj) result(addr)
    integer, intent(in) :: obj
    integer :: addr

    if (hdr_version <= 3) then
      ! Defaults table: 31 words (62 bytes), then 9-byte entries
      addr = hdr_object_table + 62 + (obj - 1) * 9
    else
      ! Defaults table: 63 words (126 bytes), then 14-byte entries
      addr = hdr_object_table + 126 + (obj - 1) * 14
    end if
  end function obj_addr

  function obj_get_parent(obj) result(parent)
    integer, intent(in) :: obj
    integer :: parent, base
    base = obj_addr(obj)
    if (hdr_version <= 3) then
      parent = mem_read_byte(base + 4)
    else
      parent = mem_read_word(base + 6)
    end if
  end function obj_get_parent

  function obj_get_sibling(obj) result(sib)
    integer, intent(in) :: obj
    integer :: sib, base
    base = obj_addr(obj)
    if (hdr_version <= 3) then
      sib = mem_read_byte(base + 5)
    else
      sib = mem_read_word(base + 8)
    end if
  end function obj_get_sibling

  function obj_get_child(obj) result(child)
    integer, intent(in) :: obj
    integer :: child, base
    base = obj_addr(obj)
    if (hdr_version <= 3) then
      child = mem_read_byte(base + 6)
    else
      child = mem_read_word(base + 10)
    end if
  end function obj_get_child

  subroutine obj_set_parent(obj, val)
    integer, intent(in) :: obj, val
    integer :: base
    base = obj_addr(obj)
    if (hdr_version <= 3) then
      call mem_write_byte(base + 4, val)
    else
      call mem_write_word(base + 6, val)
    end if
  end subroutine obj_set_parent

  subroutine obj_set_sibling(obj, val)
    integer, intent(in) :: obj, val
    integer :: base
    base = obj_addr(obj)
    if (hdr_version <= 3) then
      call mem_write_byte(base + 5, val)
    else
      call mem_write_word(base + 8, val)
    end if
  end subroutine obj_set_sibling

  subroutine obj_set_child(obj, val)
    integer, intent(in) :: obj, val
    integer :: base
    base = obj_addr(obj)
    if (hdr_version <= 3) then
      call mem_write_byte(base + 6, val)
    else
      call mem_write_word(base + 10, val)
    end if
  end subroutine obj_set_child

  function obj_get_attr(obj, attr) result(is_set)
    integer, intent(in) :: obj, attr
    logical :: is_set
    integer :: base, byte_idx, bit_idx, byte_val

    base = obj_addr(obj)
    byte_idx = attr / 8
    bit_idx = 7 - mod(attr, 8)  ! MSB first
    byte_val = mem_read_byte(base + byte_idx)
    is_set = iand(byte_val, ishft(1, bit_idx)) /= 0
  end function obj_get_attr

  subroutine obj_set_attr(obj, attr)
    integer, intent(in) :: obj, attr
    integer :: base, byte_idx, bit_idx, byte_val

    base = obj_addr(obj)
    byte_idx = attr / 8
    bit_idx = 7 - mod(attr, 8)
    byte_val = mem_read_byte(base + byte_idx)
    byte_val = ior(byte_val, ishft(1, bit_idx))
    call mem_write_byte(base + byte_idx, byte_val)
  end subroutine obj_set_attr

  subroutine obj_clear_attr(obj, attr)
    integer, intent(in) :: obj, attr
    integer :: base, byte_idx, bit_idx, byte_val

    base = obj_addr(obj)
    byte_idx = attr / 8
    bit_idx = 7 - mod(attr, 8)
    byte_val = mem_read_byte(base + byte_idx)
    byte_val = iand(byte_val, not(ishft(1, bit_idx)))
    call mem_write_byte(base + byte_idx, iand(byte_val, 255))
  end subroutine obj_clear_attr

  ! Get the property table address for an object
  function obj_prop_table(obj) result(addr)
    integer, intent(in) :: obj
    integer :: addr, base

    base = obj_addr(obj)
    if (hdr_version <= 3) then
      addr = mem_read_word(base + 7)
    else
      addr = mem_read_word(base + 12)
    end if
  end function obj_prop_table

  ! Print object short name
  subroutine obj_print_short_name(obj)
    integer, intent(in) :: obj
    integer :: prop_addr, name_len, dummy
    character(len=256) :: name

    prop_addr = obj_prop_table(obj)
    ! First byte is the text-length (in words) of the short name
    name_len = mem_read_byte(prop_addr)
    if (name_len > 0) then
      call text_decode(prop_addr + 1, name, name_len, dummy)
      write(*,'(A)', advance='no') name(1:name_len)
    end if
  end subroutine obj_print_short_name

  ! Get property default value
  function obj_get_prop_default(prop) result(val)
    integer, intent(in) :: prop
    integer :: val
    val = mem_read_word(hdr_object_table + (prop - 1) * 2)
  end function obj_get_prop_default

  ! Find property entry address, returns 0 if not found
  ! Also returns the property data size
  function find_prop(obj, prop, prop_size) result(data_addr)
    integer, intent(in) :: obj, prop
    integer, intent(out) :: prop_size
    integer :: data_addr
    integer :: addr, size_byte, pnum, name_words

    addr = obj_prop_table(obj)
    ! Skip short name: first byte = word count of name text
    name_words = mem_read_byte(addr)
    addr = addr + 1 + name_words * 2

    ! Walk property list
    do
      if (hdr_version <= 3) then
        size_byte = mem_read_byte(addr)
        if (size_byte == 0) then
          data_addr = 0
          prop_size = 0
          return
        end if
        pnum = iand(size_byte, 31)          ! bits 0-4
        prop_size = ishft(size_byte, -5) + 1 ! bits 5-7 + 1
        addr = addr + 1
      else
        size_byte = mem_read_byte(addr)
        if (size_byte == 0) then
          data_addr = 0
          prop_size = 0
          return
        end if
        pnum = iand(size_byte, 63)  ! bits 0-5
        if (iand(size_byte, 128) /= 0) then
          ! Two-byte size: bit 7 set
          addr = addr + 1
          prop_size = iand(mem_read_byte(addr), 63)
          if (prop_size == 0) prop_size = 64  ! spec says 0 means 64
          addr = addr + 1
        else
          ! One-byte size: bit 6 determines 1 or 2
          if (iand(size_byte, 64) /= 0) then
            prop_size = 2
          else
            prop_size = 1
          end if
          addr = addr + 1
        end if
      end if

      if (pnum == prop) then
        data_addr = addr
        return
      end if

      if (pnum < prop) then
        ! Properties are in descending order
        data_addr = 0
        prop_size = 0
        return
      end if

      addr = addr + prop_size
    end do
  end function find_prop

  ! Get property value
  function obj_get_prop(obj, prop) result(val)
    integer, intent(in) :: obj, prop
    integer :: val, data_addr, psize

    data_addr = find_prop(obj, prop, psize)
    if (data_addr == 0) then
      val = obj_get_prop_default(prop)
      return
    end if

    if (psize == 1) then
      val = mem_read_byte(data_addr)
    else
      val = mem_read_word(data_addr)
    end if
  end function obj_get_prop

  ! Get property data address (0 if not found)
  function obj_get_prop_addr(obj, prop) result(addr)
    integer, intent(in) :: obj, prop
    integer :: addr, psize
    addr = find_prop(obj, prop, psize)
  end function obj_get_prop_addr

  ! Get next property number after given property (0 = get first)
  function obj_get_next_prop(obj, prop) result(next_prop)
    integer, intent(in) :: obj, prop
    integer :: next_prop
    integer :: addr, size_byte, pnum, psize, name_words

    addr = obj_prop_table(obj)
    name_words = mem_read_byte(addr)
    addr = addr + 1 + name_words * 2

    if (prop == 0) then
      ! Return first property
      size_byte = mem_read_byte(addr)
      if (size_byte == 0) then
        next_prop = 0
      else
        if (hdr_version <= 3) then
          next_prop = iand(size_byte, 31)
        else
          next_prop = iand(size_byte, 63)
        end if
      end if
      return
    end if

    ! Find the given property, then return the next one
    do
      if (hdr_version <= 3) then
        size_byte = mem_read_byte(addr)
        if (size_byte == 0) then
          write(*,'(A,I0,A,I0)') 'Error: property not found: obj=', obj, ' prop=', prop
          next_prop = 0
          return
        end if
        pnum = iand(size_byte, 31)
        psize = ishft(size_byte, -5) + 1
        addr = addr + 1
      else
        size_byte = mem_read_byte(addr)
        if (size_byte == 0) then
          write(*,'(A,I0,A,I0)') 'Error: property not found: obj=', obj, ' prop=', prop
          next_prop = 0
          return
        end if
        pnum = iand(size_byte, 63)
        if (iand(size_byte, 128) /= 0) then
          addr = addr + 1
          psize = iand(mem_read_byte(addr), 63)
          if (psize == 0) psize = 64
          addr = addr + 1
        else
          if (iand(size_byte, 64) /= 0) then
            psize = 2
          else
            psize = 1
          end if
          addr = addr + 1
        end if
      end if

      if (pnum == prop) then
        ! Found it, now read the next one
        addr = addr + psize
        size_byte = mem_read_byte(addr)
        if (size_byte == 0) then
          next_prop = 0
        else
          if (hdr_version <= 3) then
            next_prop = iand(size_byte, 31)
          else
            next_prop = iand(size_byte, 63)
          end if
        end if
        return
      end if

      addr = addr + psize
    end do
  end function obj_get_next_prop

  ! Get property data length from property data address
  ! (address must point to property data, not the size byte)
  function obj_get_prop_len(data_addr) result(plen)
    integer, intent(in) :: data_addr
    integer :: plen, size_byte

    if (data_addr == 0) then
      plen = 0
      return
    end if

    ! Look at the byte BEFORE the data address
    if (hdr_version <= 3) then
      size_byte = mem_read_byte(data_addr - 1)
      plen = ishft(size_byte, -5) + 1
    else
      size_byte = mem_read_byte(data_addr - 1)
      if (iand(size_byte, 128) /= 0) then
        ! This is the second size byte
        plen = iand(size_byte, 63)
        if (plen == 0) plen = 64
      else
        if (iand(size_byte, 64) /= 0) then
          plen = 2
        else
          plen = 1
        end if
      end if
    end if
  end function obj_get_prop_len

  ! Put property value
  subroutine obj_put_prop(obj, prop, val)
    integer, intent(in) :: obj, prop, val
    integer :: data_addr, psize

    data_addr = find_prop(obj, prop, psize)
    if (data_addr == 0) then
      write(*,'(A,I0,A,I0)') 'Error: put_prop - property not found: obj=', obj, ' prop=', prop
      return
    end if

    if (psize == 1) then
      call mem_write_byte(data_addr, iand(val, 255))
    else
      call mem_write_word(data_addr, iand(val, 65535))
    end if
  end subroutine obj_put_prop

  ! Remove object from its parent's child list
  subroutine obj_remove(obj)
    integer, intent(in) :: obj
    integer :: parent, child, prev, sib

    parent = obj_get_parent(obj)
    if (parent == 0) return

    child = obj_get_child(parent)
    if (child == obj) then
      ! Object is first child
      call obj_set_child(parent, obj_get_sibling(obj))
    else
      ! Walk sibling chain to find predecessor
      prev = child
      do
        sib = obj_get_sibling(prev)
        if (sib == obj) then
          call obj_set_sibling(prev, obj_get_sibling(obj))
          exit
        end if
        if (sib == 0) exit
        prev = sib
      end do
    end if

    call obj_set_parent(obj, 0)
    call obj_set_sibling(obj, 0)
  end subroutine obj_remove

  ! Insert object as first child of destination
  subroutine obj_insert(obj, dest)
    integer, intent(in) :: obj, dest

    call obj_remove(obj)
    call obj_set_sibling(obj, obj_get_child(dest))
    call obj_set_child(dest, obj)
    call obj_set_parent(obj, dest)
  end subroutine obj_insert

end module object_mod
