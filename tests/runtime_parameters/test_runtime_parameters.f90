program test_runtime_parameters
    use,intrinsic :: iso_fortran_env
    use mod_runtime_parameters, only : class_runtime_parameters
    implicit none
    type(class_runtime_parameters) :: crp
    integer(int32) :: nerror
    character(len=20) :: filename

!   all parameters are not correct
    filename='error.txt'
    call crp%read_runtime_parameters(filename,nerror)
    if(nerror/=12) error stop
!   all parameters are correct
    filename='correct.txt'
    call crp%read_runtime_parameters(filename,nerror)
    if(nerror/=0) error stop

end program test_runtime_parameters