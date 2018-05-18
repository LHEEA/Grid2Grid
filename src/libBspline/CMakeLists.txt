set(
    BSPLINE_SOURCES
        src/bspline_module.f90
        src/bspline_oo_module.f90
        src/bspline_sub_module.f90
)


add_library(bspline STATIC ${BSPLINE_SOURCES})
target_include_directories(bspline PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})
