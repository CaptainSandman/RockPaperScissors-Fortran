! rps.f90
! Josh's Rock Paper Scissors Game written in Fortran 90/95
! Created by Josh Kennedy on 27 April 2014

! Game module that contains "global" variables.
module rpsGameModule

	implicit none

	integer, parameter :: ROCK = 1
	integer, parameter :: PAPER = 2
	integer, parameter :: SCISSORS = 3
	integer, parameter :: EXIT_GAME = 4

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

	select case (choice)

		case (ROCK)
			print *, "You chose Rock."

		case (PAPER)
			print *, "You chose Paper."

		case (SCISSORS)
			print *, "You chose Scissors."

		case (EXIT_GAME)
			print *, "Bye bye!"

	end select

	if (choice == EXIT_GAME) then
		call exit(0)
	end if

end subroutine grabInput

! Have the computer make its move.
! Returns: The computer's choice as an integer.
subroutine makeMove (choice)

	use rpsGameModule

	implicit none

	integer, intent(out) :: choice

	choice = (rand(0) * SCISSORS) + ROCK

	select case (choice)

		case (ROCK)
			print *, "Computer chose Rock."

		case (PAPER)
			print *, "Computer chose Paper."

		case (SCISSORS)
			print *, "Computer chose Scissors."

	end select

end subroutine makeMove

! Figures out who won.
! Inputs: User's choice, and Computer's choice
! Returns: Integer that is 1 if the user won, 0 if the CPU won, and -1 if tie.
subroutine whoWon (user, computer, winner)

	use rpsGameModule

	implicit none

	integer, intent(in) :: user
	integer, intent(in) :: computer
	integer, intent(out) :: winner

	select case (user)
	
		case (ROCK)
			select case (computer)

				case (ROCK)
					winner = -1

				case (PAPER)
					winner = 0

				case (SCISSORS)
					winner = 1

			end select

		case (PAPER)
			select case (computer)

				case (ROCK)
					winner = 1

				case (PAPER)
					winner = -1

				case (SCISSORS)
					winner = 0
			end select

		case (SCISSORS)
			select case (computer)

				case (ROCK)
					winner = 0

				case (PAPER)
					winner = 1

				case (SCISSORS)
					winner = -1

			end select
	
	end select

end subroutine whoWon

! Game loop.
subroutine gameLoop

	integer :: player
	integer :: computer
	integer :: theWinner

	do while (.true.)

		call grabInput(player)
		call makeMove(computer)
		call whoWon(player, computer, theWinner)

		if (theWinner .eq. 1) then
			print *, "A winner is you!"
		else if (theWinner .eq. 0) then
			print *, "You lost. Sad face."
		else
			print *, "Tied game."
		end if

	end do

end subroutine

program rps

	print *, "Josh's Rock, Paper, Scissors Game in Fortran!"

	call gameLoop()

end program rps
