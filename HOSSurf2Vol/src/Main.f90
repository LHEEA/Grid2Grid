PROGRAM Main
!
!
!
use modType 
use modPost_processing
!
IMPLICIT NONE
!

character(len=StringLength)  :: HOS_simu_type
character(len=StringLength)  :: ConfigFile


if (iargc()>0) then
    call getarg(1, HOS_simu_type)
    call getarg(2, ConfigFile)
else
   HOS_simu_type = 'NWT'
   ConfigFile='C:\Grid2Grid\HOSSurf2Vol\input\input_post_process_NWT.dat'
endif
write(*,*) "input_file: ", ConfigFile



if (iargc() > 0) then
call getarg(1, HOS_simu_type)
endif

write(*,*) "Type of HOS simulation: 'ocean' or 'NWT'", HOS_simu_type

if (HOS_simu_type == 'ocean') then
    call Post_processing_ocean(ConfigFile)
elseif (HOS_simu_type == 'NWT') then
    call Post_processing_NWT(ConfigFile)
else 
    write(*,*) 'error, the type of smulation does not exist'
    pause
endif

END PROGRAM Main