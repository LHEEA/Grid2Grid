set(
    GRID2GRID_MOD_SOURCES
        modGrid2GridGlobal.f90
        fftw3/modFourier_r2c_FFTW3.f90
        HDF5/modHDF5Interface.f90
        surf2Vol/modSurf2Vol.f90
        vol2Vol/modVol2Vol.f90
        postGrid2Grid/modPostGrid2Grid.f90
        modGrid2Grid.f90
)

set(GRID2GRID_MAIN_SOURCE
        main.f90
)

add_library(Grid2Grid SHARED ${GRID2GRID_MOD_SOURCES})
add_executable(postG2G ${GRID2GRID_MAIN_SOURCE} ${GRID2GRID_MOD_SOURCES})

set(GRID2GRID_INCLUDE
    ${CMAKE_Fortran_MODULE_DIRECTORY}
    ${HDF5_INC_PATH}
)

set(GRID2GRID_FLAG
    fymc
    bspline
    ${FFTW_LIB_FLAG}
    ${HDF5_LIB_FLAG}
)

target_include_directories(Grid2Grid PRIVATE ${GRID2GRID_INCLUDE} )
target_link_libraries(Grid2Grid ${GRID2GRID_FLAG} )

target_include_directories(postG2G PRIVATE ${GRID2GRID_INCLUDE} )
target_link_libraries(postG2G ${GRID2GRID_FLAG} )

install(TARGETS Grid2Grid DESTINATION ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
install(TARGETS postG2G DESTINATION ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
