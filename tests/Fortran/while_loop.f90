program factorial  
   implicit none  

   ! define variables
   integer :: nfact
   integer :: n 
   n = 1 
   nfact = 1 
   
   ! compute factorials   
   do while (n <= 10)       
      nfact = nfact * n 
      n = n + 1
      print*,  n, " ", nfact   
   end do


   test % test

end program factorial