! test1
subroutine test (a, b)
  integer :: a
  integer :: b

  b = a*2
 
end subroutine


program hello
  integer :: a = 2, b
  
  ! call test(a, b)
  ! call test2(b, b)

  print *, b

end program hello

! test3
subroutine test2 (a, b)
  integer :: a
  integer :: b

  b = a/2

end subroutine