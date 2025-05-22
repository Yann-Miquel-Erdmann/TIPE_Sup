program func
  ! This is a comment line; it is ignored by the compiler
  
  print *, test (0)
  print *, test (1)

contains
  recursive function test(a) result(b)
    integer :: a
    integer :: b
    integer :: c 
    c = a - 6
    if (a == 0) then
      b = -1
      return
    end if
    b = test(a-1)

  end function
  
end program func

