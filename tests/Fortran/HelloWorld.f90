subroutine add(a, b, c)
  integer, intent(in) :: a, b
  integer, intent(out) :: c

  c = a + b
end subroutine

subroutine double(a)
  integer, intent(inout) :: a

  a = a*2
end subroutine

program hello
  ! This is a comment line; it is ignored by the compiler
  print *, "Hello, World!"
  
  print *, test (0)
  print *, test (1)

contains
  function test(a) result(b)
    integer :: a
    integer :: b
    if (a == 0) then
      b = -1
      return
    end if
    b = a

  end function
end program hello