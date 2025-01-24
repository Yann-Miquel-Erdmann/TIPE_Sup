program printNum 
implicit none  

   ! define variables
   integer :: n
   
   if ( n == 2 ) then
      n = 7
   end if

   if (.true.) print *, "Hello"

   do n = 11, 20
      ! printing the value of n 
      print*,  n
   end do 
   
end program printNum  