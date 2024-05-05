# transpileur

https://d1wqtxts1xzle7.cloudfront.net/6710782/pxc387619-libre.pdf?1390847692=&response-content-disposition=inline%3B+filename%3DProgramming_Language_Inter_conversion.pdf&Expires=1709032558&Signature=UHzwzYRteuRPYRWQg~iM8nWL-BBySe925AQJghDXvLOc0emuXT2cFNJTlenE4hpDrAZN2pCIUMJzYW2hz7OACk4er5vhyB3pPjLRF3uXZIUX9dhZ7j6FZH-DQ7BKL-T6HfTb4RAULt5qx~ThCJt-xPPwL5CmNn~WRwtbjQ7wThGu1I8rP6sOveE9XnDNSo50ZDYfakxbQ-vBd7OASy0WHOYven9YnSDPDXo6vdd6nfxr~C7Sr4g81GTh~VNzPA6JdBp3pQB6FWFgZOnUuX3oE0uB4G8CRKR-N~Mr8lhIVrZLvUTwWV2Tp8COyYEIF7wR37V0oWWrg6IqhExLmB2Zow__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA

lien possible (login ut3 requis)
https://ieeexplore-ieee-org.gorgone.univ-toulouse.fr/document/7836839


fortran 90
:warning: https://fr.wikipedia.org/wiki/Fortran#Fortran_moderne_2 code max 132 char / ligne + & pour continuer sur la ligne suivante

https://github.com/search?q=repo%3Afortran-lang%2Ffortls%20path%3A*.json&type=code

https://catalogue-archipel.univ-toulouse.fr/permalink/f/18iboda/TN_cdi_ieee_primary_9797331
Schneider, Larissa, and Dominik Schultes. Evaluating Swift-to-Kotlin and Kotlin-to-Swift Transpilers. 2022 IEEE/ACM 9th International Conference on Mobile Software Engineering and Systems (MobileSoft), 2022, pp. 102106. 


https://ieeexplore-ieee-org.gorgone.univ-toulouse.fr/document/10186853
https://ieeexplore-ieee-org.gorgone.univ-toulouse.fr/document/9034617

var
list
string
fonction

|||
|--|--|
operateurs| + - \* / % && || !
comparateurs| < > = <= >= !=
opérateur bit à bit| << >> & | ^ ~  
operations string| concat loungeur

## basic syntax

### non io

| mot | fin |
| --- | --- |
allocatable
allocate
**assign**
assignment
block data |end block data
call
case
character
common
complex
contains
continue
cycle
data
deallocate
default
do |end do
double precision
else
else if
elsewhere
entry
equivalence
exit
external
function |end function
go to
if |end if
implicit
in
inout
integer
intent
interface |end interface
intrinsic
kind
len
logical
module |end module
namelist
nullify
only
operator
optional
out
parameter
pause
pointer
private
program |end program
public
real
recursive
result
return
save
select case |end select  
stop
subroutine |end subroutine
target
then
type |end type
type()
use
Where |end where
While

### io related

backspace
close
|endfile
format
inquire
open
print
read
rewind
Write

## étates

### tokeniser

https://www.tutorialspoint.com/fortran/fortran_basic_syntax.htm
https://www.nsc.liu.se/~boein/f77to90/a5.html

### parser

      il transforme une liste token en AST suivant la syntaxe correspondant au language du code d'entée
