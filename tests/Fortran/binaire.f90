program binaire
  integer :: a = 16330

  a = affiche_binaire(a)

contains

  function affiche_binaire (n) result(res)
    integer :: n
    integer :: res
    integer :: dividende, mod_2
    integer :: i

    do i = 0, 15
      dividende = n/2
      mod_2 = n - 2*dividende
      print *, mod_2
      n = dividende
    end do

    res = 0

  end function affiche_binaire

end program binaire