program input

implicit none

! variables whose names start with I,J,K,L,M or N are implicitly INTEGER
! so naming character :: input throws type error. so weird.

! implicit none should disable this implicit type-casting, but somehow its not…

character*20 :: equation ! ONLY 20 FOR BIG NUMBERS THIS WILL MIGHT ERROR
integer :: eqLen
integer :: first_num
character*1 :: operation
integer :: second_num
integer :: ans


character*10 :: eqPartOne, eqPartTwo
integer :: i

write (*,*)
write (*,*)
write (*,*) "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
write (*,*) "By Akash Arora"
write (*,*) "CPSC 354, Kurz"
write (*,*) "Fall 2019"
write (*,*) "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
write (*,*)
write (*,*) "Welcome to Fortulator!"
write (*,*)
write (*,*) "The best dam fortran command-line calculator at Chapman"
write (*,*) 
write (*,*) 
write (*,*) "some helpful commands: " 
write (*,*) "     ’op’ to see available operations [WORKING]"
write (*,*) "     ’prev’ to see your last calculation [nA]"
write (*,*) "     ’ans’ to use the previous answer in your current equation. (‘ans + 5’) [WORKING]"
write (*,*) "     ’exit’ to quit [WORKING]" 
write (*,*)

write (*,*) "go ahead! type in something like '11 + 4' and press [Enter]..."
write (*,*) "note: currently not accepting negatives numbers in input"
write (*,*)

ans = 0

do while (equation /= 'exit')
	read (*,"(A)") equation
	if (equation == 'op') then
		write (*,*)
		write (*,*) "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
		write (*,*) "available operations: " 
		write (*,*) "     ’+’ // Addition          // '4+2'"       
		write (*,*) "     ’-’ // Subtraction       // '6-3'"
		write (*,*) "     ’*’ // Multiplication    // '7*3'"       
		write (*,*) "     ’/’ // Division          // '4/2'"
		write (*,*)
		write (*,*) "note: division currently rounds down"
		write (*,*) "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
		write (*,*)
	else if (equation /= 'exit') then ! clean this up w/ infinite while loop, if 'exit' break else if…
		eqLen = len_trim(equation)
		! NOTE: still need to trim white spaces before and after operation, cleaner UI
		do i=1, eqLen
			if (equation(i:i) == '+'.or.equation(i:i) == '-'.or.equation(i:i) == '*'.or.equation(i:i) == '/') then
				!write (*,*) "Inside IF"
				eqPartOne = equation(1:i-1)
				eqPartOne = eqPartOne(1:len_trim(eqPartOne))
				if (eqPartOne == 'ans') then
					first_num = ans
				else 
					read(equation(1:i-1), *) first_num
				end if

				operation = equation(i:i)
			
				eqPartTwo = equation(i+1:eqLen)
				eqPartTwo = eqPartTwo(1:len_trim(eqPartTwo))
				if (eqPartTwo == 'ans') then
					second_num = ans
				else 
					read(equation(i+1:eqLen), *) second_num
				end if
			end if
		end do
		ans = calc(first_num, second_num, operation)
		write(*,*) "= ", ans
		write(*,*)
	end if
end do

write (*,*)
write (*,*)
write (*,*) "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
write (*,*) "               Hope you enjoyed using Fortulator!"
write (*,*) "      the best dam fortran command-line calculator at Chapman" 
write (*,*) 
write (*,*) "By Akash Arora"
write (*,*) "CPSC 354, Kurz"
write (*,*) "Fall 2019"
write (*,*) "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
write (*,*)
 
contains

function calc(a, b, op) result(j)
	implicit none
	integer, intent(in) :: a, b
	character*1, intent(in) :: op
	integer :: j
	if (op == '+') then
    		j = a + b
	else if (op == '-') then
    		j = a - b
	else if (op == '*') then
    		j = a * b
	else if (op == '/') then
    		j = a / b
	end if
end function calc

end program input
