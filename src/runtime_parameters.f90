!|  mod_runtime_parameters is a module for run-time parameters,<br>
!   that include one subroutine(read_runtime_parameters).
module mod_runtime_parameters
    use,intrinsic :: iso_fortran_env
    implicit none
    private
    public class_runtime_parameters
    type class_runtime_parameters
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
    contains
        procedure,pass,public :: read_runtime_parameters
    end type class_runtime_parameters
!---------
    contains
        !| read run-time parametes from file (filename)<br>
        !  return number of errors (nerror) <br>
        subroutine read_runtime_parameters(this,filename,nerror)
            class(class_runtime_parameters),intent(out) :: this
            character(len=20),intent(in) :: filename
            !! input file name
            integer(int32),intent(out) :: nerror
            !! number of errors
            integer(int32) :: unit_read_parameters

            nerror=0

            open(newunit=unit_read_parameters,file=filename)

            read(unit_read_parameters,*)
            read(unit_read_parameters,*) this%heat_capacity_ratio
            !> heat capacity ratio must be larger than one <br>
            if(this%heat_capacity_ratio<1.0) then
                print *,'heat capacity ratio must be larger than one'
                print *,'but input value is ',this%heat_capacity_ratio
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*)
            ! left state
            read(unit_read_parameters,*) this%initial_primitive_values_left(1)   ! density
            read(unit_read_parameters,*) this%initial_primitive_values_left(2)   ! velocity
            read(unit_read_parameters,*) this%initial_primitive_values_left(3)   ! pressure
            read(unit_read_parameters,*)
            ! right state
            read(unit_read_parameters,*) this%initial_primitive_values_right(1)
            read(unit_read_parameters,*) this%initial_primitive_values_right(2)
            read(unit_read_parameters,*) this%initial_primitive_values_right(3)
            !> density must be positive <br>
            if(this%initial_primitive_values_left(1)<=0.0 .or. this%initial_primitive_values_right(1)<=0.0) then
                print *,'density must be positive'
                print *,'but input values are',this%initial_primitive_values_left(1),this%initial_primitive_values_right(1)
                nerror=nerror+1
            end if
            !> two initial velocities must be equal <br>
            if(this%initial_primitive_values_left(2).ne.this%initial_primitive_values_right(2)) then
                print *,'two initial velocities must be equal'
                print *,'but input values are',this%initial_primitive_values_left(2),this%initial_primitive_values_right(2)
                nerror=nerror+1
            end if
            !> pressure must be positive <br>
            if(this%initial_primitive_values_left(3)<=0.0 .or. this%initial_primitive_values_right(3)<=0.0) then
                print *,'pressure must be positive'
                print *,'but input values are',this%initial_primitive_values_left(3),this%initial_primitive_values_right(3)
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*)
            read(unit_read_parameters,*) this%delta_t
            !> \(\Delta t\) must be positive and small enough <br>
            if(this%delta_t<=0.0) then
                print *,'delta_t must be positive'
                print *,'but input value is',this%delta_t
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*) this%time_accuracy
            !> time accuracy must be 1,2 or 3 <br>
            if(this%time_accuracy<1 .or. 3<this%time_accuracy) then
                print *,'time accuracy must be 1,2 or 3'
                print *,'but input value is',this%time_accuracy
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*)
            read(unit_read_parameters,*) this%jmax
            !> number of grid points must be even number <br>
            if(this%jmax<1 .or. mod(this%jmax,2)/=0) then
                print *,'number of grid points must be even'
                print *,'but input value is',this%jmax
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*) this%spacial_accuracy
            !> spacial accuracy (MUSCL interpolation) must be 1,2, or 3. <br>
            if(this%spacial_accuracy<1 .or. 3<this%spacial_accuracy) then
                print *,'spacial accuracy must be 1,2, or 3'
                print *,'but input value is',this%spacial_accuracy
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*) this%type_of_interpolated
            !> type of interpolated must be 1 (characteristics), 2 (primitives), or 3(conservatives). <br>
            if(this%spacial_accuracy<1 .or. 3<this%spacial_accuracy) then
                print *,'type of interpolated must be 1,2, or 3'
                print *,'but input value is',this%type_of_interpolated
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*) this%type_of_limiter
            !> type of limiter (MUSCL interpolation) must be 0 (w/o limiter), 1(superbee), or 2(minmod). <br>
            if(this%type_of_limiter<0 .or. 2<this%type_of_limiter) then
                print *,'type of limiter must be 0,1, or 2'
                print *,'but input value is',this%type_of_limiter
                nerror=nerror+1
            end if

            read(unit_read_parameters,*)
            read(unit_read_parameters,*) this%max_timestep
            !> max (final) time step must be positive <br>
            if(this%max_timestep<=0) then
                print *,'max time step must be positive'
                print *,'but input value is',this%max_timestep
                nerror=nerror+1
            end if

            read(unit_read_parameters,*) this%interval_of_flowfield_output
            !> interval of flowfield output must be positive <br>
            if(this%interval_of_flowfield_output<=0) then
                print *,'interval of flowfield output must be positive'
                print *,'but input value is',this%interval_of_flowfield_output
                nerror=nerror+1
            end if

        end subroutine read_runtime_parameters
end module mod_runtime_parameters