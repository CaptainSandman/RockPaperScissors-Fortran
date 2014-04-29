! rps.f90
! Josh's Rock Paper Scissors Game written in Fortran 90/95
! Created by Josh Kennedy on 27 April 2014

! Game module that contains "global" variables.
module rpsGameModule

	implicit none

	integer :: ROCK = 1
	integer :: PAPER = 2
	integer :: SCISSORS = 3
	integer :: EXIT_GAME = 4

end module rpsGameModule

! Program logic subroutines.

! Prompts the user for input.
! Returns: The user's choice as an integer.
subroutine grabInput (choice)

	use rpsGameModule

	implicit none

	integer, intent(out) :: choice

	print *, "Make a selection of 1) Rock, 2) Paper, 3) Scissors, or 4) Exit"

	read *, choice

	if (choice == EXIT_GAME) then
		call exit(0)
	end if

end subroutine grabInput

! Have the computer make its move.
! Returns: The computer's choice as an integer.
subroutine makeMove (choice)

	use rpsGameModule

	integer, intent(out) :: choice

	choice = (SCISSORS - ROCK) * rand(0) + ROCK

end subroutine makeMove

! Figures out who won.
! Inputs: User's choice, and Computer's choice
! Returns: Logical that is true if the user won, false otherwise.
subroutine whoWon (user, computer, winner)

	use rpsGameModule

	implicit none

	integer, intent(in) :: user
	integer, intent(in) :: computer
	integer, intent(out) :: winner

	! TODO: Figure this out. :P

end subroutine whoWon

! Game loop.
subroutine gameLoop

	integer :: player
	integer :: computer
	integer :: theWinner

	do while (1 == 1)

		call grabInput(player)
		call makeMove(computer)
		call whoWon(player, computer, theWinner)

		if (theWinner == 1) then
			print *, "A winner is you!"
		else
			print *, "You lost. Sad face."
		end if

	end do

end subroutine

program rps

	use rpsGameModule

	implicit none

	print *, "Josh's Rock, Paper, Scissors Game in Fortran!"

	call gameLoop()

end program rps