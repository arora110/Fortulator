program input

implicit none

character*40 :: equation
integer :: eqLen
integer :: first_num
character*1 :: operation
integer :: second_num
integer :: ans

character*10 :: eqPartOne, eqPartTwo
integer :: i ! for loops, maybe get rid of and make local if possible but this for now

integer :: endOfLogIndex
integer :: prevCount

integer :: test
character*30 :: prevEquation

! List of available operation types to validate against
Character (len=65), Allocatable :: opTypes(:)
opTypes = [Character(len=65) :: "+","-","*","/", "^"]

ans = printIntro(1,1) ! cleaner but fix this assignment, should just call printIntro and other functions
ans = 0

do ! infinte loop
	read (*,"(A)") equation
	if (equation == 'exit') then
		exit
	else if (equation == 'op') then
		ans = printOperations(1,1)
		ans = 0
	else if (equation == 'prev') then
		write (*,*) prevEquation
		write (*,*)
	else if (equation /= 'exit') then ! clean this up w/ infinite while loop, if 'exit' break else if…
		eqLen = len_trim(equation)
		! NOTE: still need to trim white spaces before and after operation, cleaner UI
		do i=1, eqLen
			if (ANY(opTypes == equation(i:i))) then
				! write (*,*) "Inside IF"
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
	
		! save equation / ans
		prevEquation = storeEquation(first_num, operation, second_num, ans)

		write(*,*) "= ", ans
		write(*,*)
	end if
end do

ans = printExitMessage(1,1)
ans = 0
 
contains

! FUNCTIONS! FUNCTIONS! FUNCTIONS! FUNCTIONS! FUNCTIONS! FUNCTIONS!

! This function CALCULATES THE EQUATION
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
	else if (op == "^") then
		j = a ** b
	end if
end function calc

! This function CONVERTS EQUATION PARTS INTO A STRING FOR 'prevEquation
function storeEquation(first_num, op, second_num, ans) result(equation)
	implicit none
	integer, intent	(in) :: first_num, second_num, ans
	character*1, intent(in) :: op
	
	character*10 first_num_str
	character*10 second_num_str
	character*10 ans_str
	character*20 equation

	character*1 space
	character*3 equals

	space = " "
	equals = " = "

	write(first_num_str, '(i10)') first_num
	write(first_num_str, *) trim(adjustl(first_num_str))

	write(second_num_str, '(i10)') second_num
	write(second_num_str, *) trim(adjustl(second_num_str))

	write(ans_str, '(i10)') ans
	write(ans_str, *) trim(adjustl(ans_str))
	
	equation = trim(first_num_str) // space // trim(op) // space // trim(second_num_str) // equals // trim(ans_str)
end function storeEquation

! This function PRINTS INTRO MESSAGE
function printIntro(a, b) result(j)
	implicit none
	integer, intent(in) :: a, b
	integer :: j
	j = 1
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
	write (*,*) "     ’prev’ to see your last calculation [WORKING]"
	write (*,*) "     ’ans’ to use the previous answer in your current equation. (‘ans + 5’) [WORKING]"
	write (*,*) "     ’exit’ to quit [WORKING]" 
	write (*,*)

	write (*,*) "go ahead! type in something like '11 + 4' and press [Enter]..."
	write (*,*) "note: currently not accepting negatives numbers in input"
	write (*,*)
end function printIntro


! This function PRINTS AVAILABLE OPERATIONS
function printOperations(a,b) result(j)
	implicit none
	integer, intent(in) :: a, b
	integer :: j
	j = 1
	write (*,*)
	write (*,*) "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
	write (*,*) "available operations: " 
	write (*,*) "     ’+’ // Addition          // '4+2'"       
	write (*,*) "     ’-’ // Subtraction       // '6-3'"
	write (*,*) "     ’*’ // Multiplication    // '7*3'"       
	write (*,*) "     ’/’ // Division          // '4/2'"
	write (*,*) "     ’^’ // Exponentiation    // '3^3'"
	write (*,*)
	write (*,*) "note: division currently rounds down"
	write (*,*) "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
	write (*,*)
end function printOperations


! This function PRINTS EXIT MESSAGE
function printExitMessage(a,b) result(j)
	implicit none
	integer, intent(in) :: a, b
	integer :: j
	j = 1
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
end function printExitMessage

end program input
