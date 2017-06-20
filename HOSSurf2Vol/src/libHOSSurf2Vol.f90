subroutine read_HOS(ConfigFile,HOS_simu_type)
!
use modType 
use modPost_processing_ocean
use modPost_processing_NWT
!
!
    character(len=*), intent(in)   :: HOS_simu_type
    character(len=*), intent(in)   :: ConfigFile

    ! Read input file to define parameters of post-processing
    if (HOS_simu_type == 'ocean') then
        CALL read_input_ocean(ConfigFile)
    elseif (HOS_simu_type == 'NWT') then
        CALL read_input_NWT(ConfigFile)
    else 
        write(*,*) 'error, the type of smulation does not exist'
        pause
    endif

end subroutine read_HOS


subroutine reconstruct_HOSSurf2Vol(HOS_simu_type)
!
use modType 
use modPost_processing_ocean
use modPost_processing_NWT
!
    character(len=*), intent(in)   :: HOS_simu_type
    

    if (HOS_simu_type == 'ocean') then
        call Post_processing_ocean
    elseif (HOS_simu_type == 'NWT') then
        call Post_processing_NWT
    else 
        write(*,*) 'error, the type of smulation does not exist'
        pause
    endif

end subroutine reconstruct_HOSSurf2Vol