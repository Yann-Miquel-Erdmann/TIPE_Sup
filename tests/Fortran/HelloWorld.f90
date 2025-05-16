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
  

end program hello