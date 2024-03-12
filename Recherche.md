# transpileur

https://d1wqtxts1xzle7.cloudfront.net/6710782/pxc387619-libre.pdf?1390847692=&response-content-disposition=inline%3B+filename%3DProgramming_Language_Inter_conversion.pdf&Expires=1709032558&Signature=UHzwzYRteuRPYRWQg~iM8nWL-BBySe925AQJghDXvLOc0emuXT2cFNJTlenE4hpDrAZN2pCIUMJzYW2hz7OACk4er5vhyB3pPjLRF3uXZIUX9dhZ7j6FZH-DQ7BKL-T6HfTb4RAULt5qx~ThCJt-xPPwL5CmNn~WRwtbjQ7wThGu1I8rP6sOveE9XnDNSo50ZDYfakxbQ-vBd7OASy0WHOYven9YnSDPDXo6vdd6nfxr~C7Sr4g81GTh~VNzPA6JdBp3pQB6FWFgZOnUuX3oE0uB4G8CRKR-N~Mr8lhIVrZLvUTwWV2Tp8COyYEIF7wR37V0oWWrg6IqhExLmB2Zow__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA


fortran 90


var
list
string
fonction


operateurs + - * / %   && || ! 
comparateurs < > = <= >= !=
opérateur bit à bit: << >> & | ^ ~  
operations string: concat loungeur


## basic syntax
### non io
allocatable
allocate
assign
assignment
block data        end block data
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
do                end do
double precision
else
else if
elsewhere
entry
equivalence
exit
external
function          end function
go to
if                end if
implicit
in
inout
integer
intent
interface         end interface
intrinsic
kind
len
logical
module            end module
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
program           end program
public
real
recursive
result
return
save
select case       end select  
stop
subroutine        end subroutine
target
then
type              end type
type()
use
Where             end where
While

### io related
backspace
close
endfile
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
### parser
      il transforme une liste token en AST suivant la syntaxe correspondant au language du code d'entée
