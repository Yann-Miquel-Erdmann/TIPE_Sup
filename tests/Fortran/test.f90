
! slkdvqs

! qsdvmkqsdv

function func(arg1, arg2)
    real func
    real :: arg1
    real :: arg2
    func = arg1+arg2*3

end function




program addNumbers
    ! ceci est un commentaire
    
    real :: a = 1, b = 2, res = 3
    real :: func
    integer :: n 
    n = 7
    a = 12.0
    b = 15.0
    
    if (a==12.0) then
        print * , 'a = 12'
    else
        print *, 'a!=12'
    end if

    res = func(a,b)


    print * , 'The result is ' , res
    print * , 'The result is ' , n

end program addNumbers



