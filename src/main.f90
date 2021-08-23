program main
    use,intrinsic :: iso_fortran_env
    use mod_runtime_parameters, only :  read_runtime_parameters
    implicit none
    character(len=20) :: runtime_parameters_filename
    integer(int32) :: ierror
    !integer(int32) :: it,sit
    !real(real64) :: time

    print *,'input run-time parameters file name'
    read(input_unit,*) runtime_parameters_filename
    ! pre-process
    call read_runtime_parameters(runtime_parameters_filename,ierror)
    if(ierror/=0) error stop
!    call select_procedures
!    call generate_grid
!    call set_flowfield(time)
!    call output_flowfield
!
!    ! main process
!    time_marching : do it = 1, max_it
!        subiteration : do si = 1, max_si
!            call calc_variables
!            call set_boundary_conditions
!            call evaluate_right_hand_side
!            call solve_equations
!        end do subiteration
!        if(mod(it,interval_of_flowfield_output)==0) call output_flowfield
!        if(time>=time_limit) exit
!    end do time_marching
!
!    ! post-process
!    call output_flowfield

end program main