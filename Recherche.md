# transpileur

https://d1wqtxts1xzle7.cloudfront.net/6710782/pxc387619-libre.pdf?1390847692=&response-content-disposition=inline%3B+filename%3DProgramming_Language_Inter_conversion.pdf&Expires=1709032558&Signature=UHzwzYRteuRPYRWQg~iM8nWL-BBySe925AQJghDXvLOc0emuXT2cFNJTlenE4hpDrAZN2pCIUMJzYW2hz7OACk4er5vhyB3pPjLRF3uXZIUX9dhZ7j6FZH-DQ7BKL-T6HfTb4RAULt5qx~ThCJt-xPPwL5CmNn~WRwtbjQ7wThGu1I8rP6sOveE9XnDNSo50ZDYfakxbQ-vBd7OASy0WHOYven9YnSDPDXo6vdd6nfxr~C7Sr4g81GTh~VNzPA6JdBp3pQB6FWFgZOnUuX3oE0uB4G8CRKR-N~Mr8lhIVrZLvUTwWV2Tp8COyYEIF7wR37V0oWWrg6IqhExLmB2Zow__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA

❌ : won't use

liens possibles (login ut3 requis) : <br>
https://ieeexplore-ieee-org.gorgone.univ-toulouse.fr/document/7836839
=> cite as
```
M. Bysiek, A. Drozd and S. Matsuoka, "Migrating Legacy Fortran to Python While Retaining Fortran-Level Performance through Transpilation and Type Hints," 2016 6th Workshop on Python for High-Performance and Scientific Computing (PyHPC), Salt Lake City, UT, USA, 2016, pp. 9-18, doi: 10.1109/PyHPC.2016.006.
```


fortran 90
:warning: https://fr.wikipedia.org/wiki/Fortran#Fortran_moderne_2 code max 132 char / ligne + & pour continuer sur la ligne suivante

https://github.com/search?q=repo%3Afortran-lang%2Ffortls%20path%3A*.json&type=code

https://catalogue-archipel.univ-toulouse.fr/permalink/f/18iboda/TN_cdi_ieee_primary_9797331
Schneider, Larissa, and Dominik Schultes. Evaluating Swift-to-Kotlin and Kotlin-to-Swift Transpilers. 2022 IEEE/ACM 9th International Conference on Mobile Software Engineering and Systems (MobileSoft), 2022, pp. 102106. 


https://ieeexplore-ieee-org.gorgone.univ-toulouse.fr/document/10186853

❌ https://ieeexplore-ieee-org.gorgone.univ-toulouse.fr/document/9034617
=> cite as
```
W. Jordan, A. Bejo and A. G. Persada, "The Development of Lexer and Parser as Parts of Compiler for GAMA32 Processor’s Instruction-set using Python," 2019 International Seminar on Research of Information Technology and Intelligent Systems (ISRITI), Yogyakarta, Indonesia, 2019, pp. 450-455, doi: 10.1109/ISRITI48646.2019.9034617.
```

definition trasnpilation + usage :
https://ieeexplore-ieee-org.gorgone.univ-toulouse.fr/document/8247456
=> cite as
```
B. F. Andrés and M. Pérez, "Transpiler-based architecture for multi-platform web applications," 2017 IEEE Second Ecuador Technical Chapters Meeting (ETCM), Salinas, Ecuador, 2017, pp. 1-6, doi: 10.1109/ETCM.2017.8247456.
```

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
