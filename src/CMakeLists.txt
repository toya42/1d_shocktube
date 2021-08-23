# runtime_paramters.f90
set(LIB_RUNTIME_PARAMETERS
        runtime_parameters.f90
)
add_library(RUNTIME_PARAMETERS ${LIB_RUNTIME_PARAMETERS})
set_target_properties(RUNTIME_PARAMETERS
    PROPERTIES
    Fortran_MODULE_DIRECTORY ${LIB_MOD_DIR}
)

add_executable(${PROJECT_NAME} main.f90)
target_link_libraries(${PROJECT_NAME} RUNTIME_PARAMETERS)
