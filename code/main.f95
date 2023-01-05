! 
! 
!                             Black Hole G-Force
!                 Code created for Educational Purpose only
!            No warranty are givven or liability can be expected.
!                       Licenced under MIT License.
!                          Always give credits.
! 
program black_hole
    implicit none

    ! Declare variables
    integer :: choice, iostat
    logical :: run, save_report
    real :: mass, distance, g_force, schwarzschild_radius, c
    character(len=50) :: filename
    integer, parameter :: MAX_ITERATIONS = 1000
    integer :: i
    real, dimension(MAX_ITERATIONS) :: masses, distances, g_forces

    ! Set the value of the speed of light in a vacuum
    c = 2.998E8

    ! Set run flag to true
    run = .true.

    ! Loop until user decides to exit
    do while (run)
        ! Display menu
        write(*,'(A)') "Select an option:"
        write(*,'(A)') "1. Calculate G-force using known black hole mass"
        write(*,'(A)') "2. Calculate G-force using user-specified mass"
        write(*,'(A)') "3. Exit program"
        read(*,'(I5)') choice

        if (choice == 1) then
            ! Prompt user to select known black hole mass
            write(*,'(A)') "Select a known black hole mass:"
            write(*,'(A)') "1. Sagittarius A* (4.3 million solar masses)"
            write(*,'(A)') "2. Cygnus X-1 (14.8 solar masses)"
            write(*,'(A)') "3. M87* (6.5 billion solar masses)"
            write(*,'(A)') "4. IC 1101 (100 trillion solar masses)"
            read(*,'(I5)') choice
            ! Set mass based on user selection
            if (choice == 1) then
                mass = 4.3E6 ! Sagittarius A*
            else if (choice == 2) then
                mass = 14.8 ! Cygnus X-1
            else if (choice == 3) then
                mass = 6.5E9 ! M87*
            else if (choice == 4) then
                mass = 1.0E14 ! IC 1101
            else
                ! Print error message
                write(*,'(A)') "Error: Invalid option."
                continue
            endif
        else if (choice == 2) then
            ! Prompt user for mass
            write(*,'(A)') "Enter the mass of the black hole (in kilograms):"
            read(*,'(F5.2)') mass
        else if (choice == 3) then
            ! Set run flag to false
            run = .false.
            continue
        else
            ! Print error message
            write(*,'(A)') "Error: Invalid option."
            continue
        endif

        ! Calculate Schwarzschild radius
        schwarzschild_radius = 2 * 6.674E-11 * mass / c**2

        ! Prompt user for distance
        write(*,'(A,F5.2)') "Enter the distance from the black hole (in meters):"
        write(*,'(A,F5.2)') "(must be greater than the Schwarzschild radius of ", schwarzschild_radius, " meters)"
        read(*,'(F5.2)') distance

        ! Check if distance is greater than Schwarzschild radius
        if (distance > schwarzschild_radius) then
            ! Calculate G-force
            g_force = 6.674E-11 * mass / distance**2

            ! Print result
            write(*,'(A,F5.2)') "The G-force is: ", g_force

            ! Prompt user to save calculation report
            write(*,'(A)') "Do you want to save the calculation report to a file? (Y/N)"
            read(*,'(A)') save_report
            if (save_report .eq. "Y") then
                ! Prompt user for filename
                write(*,'(A)') "Enter the filename (including the file extension):"
                read(*,'(A)') filename

                ! Open file
                open(unit=10, file=filename, status='old', action='write', iostat=iostat)

                ! Check if file already exists
                if (iostat == 0) then
                    ! Prompt user to overwrite or append
                    write(*,'(A)') "The file already exists. Do you want to (O)verwrite or (A)ppend to it?"
                    read(*,'(A)') save_report
                    if (save_report .eq. "O") then
                        open(unit=10, file=filename, status='replace', action='write', iostat=iostat)
                    else if (save_report .eq. "A") then
                        open(unit=10, file=filename, status='old', action='write', iostat=iostat)
                    else
                        ! Print error message
                        write(*,'(A)') "Error: Invalid option."
                        continue
                    endif
                else
                    ! Create new file
                    open(unit=10, file=filename, status='new', action='write', iostat=iostat)
                endif

                ! Write calculation report to file
                write(10,'(A,F5.2,A,F5.2,A,F5.2)') "Mass (kg):,", mass, ",Distance (m):,", distance, ",G-force (N):,", g_force
            else if (save_report .eq. "N") then
                ! Do nothing
            else
                ! Print error message
                write(*,'(A)') "Error: Invalid option."
                continue
            endif
        end if
    end do

    ! Print goodbye message
    write(*,'(A)') "Thank you for using the G-force calculator."
    delay = 1 ! Delay for 1 second
    write(*,'(A)') "Code created for Educational Purpose only, do not use it to navegate in to deep space."
    delay = 1 ! Delay for 1 second
    write(*,'(A)') "Code written by Andre Nemirovsky and Assistant. Licenced under MIT Licence."
    delay = 2 ! Delay for 2 seconds

end program black_hole
