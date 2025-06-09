program fibonacci
    integer :: n, result_rec
    n = 10
    ! Appel de la fonction recursive
    result_rec = fibonacci_rec(n)
    print *, "Fibonacci (recursif) de ", n, " est ", result_rec

contains
    recursive function fibonacci_rec(n) result(retval)
        integer, intent(in) :: n
        integer :: retval

        if (n < 0) then
            print *, "Erreur: n ne doit pas etre negatif"
            retval = -1
            return
        end if
        if (n == 0) then
            retval = 0
            return
        else if (n == 1) then
            retval = 1
            return
        else
            retval = fibonacci_rec(n - 1) + fibonacci_rec(n - 2)
        end if

    end function fibonacci_rec
end program fibonacci