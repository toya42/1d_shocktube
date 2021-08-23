!|  mod_runtime_parameters is a module for run-time parameters,<br>
!   that include one subroutine(read_runtime_parameters).
module mod_runtime_parameters
    use,intrinsic :: iso_fortran_env
    implicit none

    real(real64) :: heat_capacity_ratio
        !! \(\gamma=C_p/C_v\) : heat capacity ratio
    real(real64),dimension(3) :: initial_primitive_values_left,initial_primitive_values_right
        !! left or right initial primitive values<br> 1:density, 2:velocity, 3:pressure
    real(real64) :: delta_t
        !! \(\Delta t\)
    integer(int32) :: time_accuracy
        !! time accuracy. This value is equal to the steps of Runge-Kutta methods.
    integer(int32) :: jmax
        !! number of grid points, that must be an even number
    integer(int32) :: spacial_accuracy
        !* spacial accuracy<br>
        !  1:1st order (w/o MUSCL interpolation)<br>
        !  2:2nd order (w MUSCL interpolation)
    integer(int32) :: type_of_interpolated
        !* values that interpolated by MUSCL scheme<br>
        !  1:characteristics<br>
        !  2:primitives<br>
        !  3:conservatives<br>
    integer(int32) :: type_of_limiter
        !* type of limiter function<br>
        !  0:w/o limiter (unstable)<br>
        !  1:superbee (less disspative)<br>
        !  2:minmod (stable)
    integer(int32) :: max_timestep
        !! number of max time steps
    integer(int32) :: interval_of_flowfield_output
        !! interval of flowfield output
!
    contains
        !| read run-time parametes from file (filename)<br>
        !  return number of errors (nerror) <br>
        subroutine read_runtime_parameters(filename,nerror)
            character(len=20),intent(in) :: filename
            !! input file name
            integer(int32),intent(out) :: nerror
            !! number of errors
            integer(int32) :: unit_read_parameters

            nerror=0

            open(newunit=unit_read_parameters,file=filename)

            read(unit_read_parameters,*)
            read(unit_read_parameters,*) heat_capacity_ratio
            !> heat capacity ratio must be larger than one <br>
            if(heat_capacity_ratio<1.0) then
                print *,'heat capacity ratio must be larger than one'
                print *,'but input value is ',heat_capacity_ratio
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*)
            ! left state
            read(unit_read_parameters,*) initial_primitive_values_left(1)   ! density
            read(unit_read_parameters,*) initial_primitive_values_left(2)   ! velocity
            read(unit_read_parameters,*) initial_primitive_values_left(3)   ! pressure
            read(unit_read_parameters,*)
            ! right state
            read(unit_read_parameters,*) initial_primitive_values_right(1)
            read(unit_read_parameters,*) initial_primitive_values_right(2)
            read(unit_read_parameters,*) initial_primitive_values_right(3)
            !> density must be positive <br>
            if(initial_primitive_values_left(1)<=0.0 .or. initial_primitive_values_right(1)<=0.0) then
                print *,'density must be positive'
                print *,'but input values are',initial_primitive_values_left(1),initial_primitive_values_right(1)
                nerror=nerror+1
            end if
            !> two initial velocities must be equal <br>
            if(initial_primitive_values_left(2).ne.initial_primitive_values_right(2)) then
                print *,'two initial velocities must be equal'
                print *,'but input values are',initial_primitive_values_left(2),initial_primitive_values_right(2)
                nerror=nerror+1
            end if
            !> pressure must be positive <br>
            if(initial_primitive_values_left(3)<=0.0 .or. initial_primitive_values_right(3)<=0.0) then
                print *,'pressure must be positive'
                print *,'but input values are',initial_primitive_values_left(3),initial_primitive_values_right(3)
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*)
            read(unit_read_parameters,*) delta_t
            !> \(\Delta t\) must be positive and small enough <br>
            if(delta_t<=0.0) then
                print *,'delta_t must be positive'
                print *,'but input value is',delta_t
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*) time_accuracy
            !> time accuracy must be 1,2 or 3 <br>
            if(time_accuracy<1 .or. 3<time_accuracy) then
                print *,'time accuracy must be 1,2 or 3'
                print *,'but input value is',time_accuracy
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*)
            read(unit_read_parameters,*) jmax
            !> number of grid points must be even number <br>
            if(jmax<1 .or. mod(jmax,2)/=0) then
                print *,'number of grid points must be even'
                print *,'but input value is',jmax
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*) spacial_accuracy
            !> spacial accuracy (MUSCL interpolation) must be 1,2, or 3. <br>
            if(spacial_accuracy<1 .or. 3<spacial_accuracy) then
                print *,'spacial accuracy must be 1,2, or 3'
                print *,'but input value is',spacial_accuracy
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*) type_of_interpolated
            !> type of interpolated must be 1 (characteristics), 2 (primitives), or 3(conservatives). <br>
            if(spacial_accuracy<1 .or. 3<spacial_accuracy) then
                print *,'type of interpolated must be 1,2, or 3'
                print *,'but input value is',type_of_interpolated
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*) type_of_limiter
            !> type of limiter (MUSCL interpolation) must be 0 (w/o limiter), 1(superbee), or 2(minmod). <br>
            if(type_of_limiter<0 .or. 2<type_of_limiter) then
                print *,'type of limiter must be 0,1, or 2'
                print *,'but input value is',type_of_limiter
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*) max_timestep
            !> max (final) time step must be positive <br>
            if(max_timestep<=0) then
                print *,'max time step must be positive'
                print *,'but input value is',max_timestep
                nerror=nerror+1
            end if

            read(unit_read_parameters,*) interval_of_flowfield_output
            !> interval of flowfield output must be positive <br>
            if(interval_of_flowfield_output<=0) then
                print *,'interval of flowfield output must be positive'
                print *,'but input value is',interval_of_flowfield_output
                nerror=nerror+1
            end if

        end subroutine read_runtime_parameters
end module mod_runtime_parameters