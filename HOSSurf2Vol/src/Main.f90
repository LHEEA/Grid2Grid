PROGRAM Main
!
!
!
use modType 
use modPost_processing_ocean
use modPost_processing_NWT
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
   ConfigFile='../input/input_post_process_NWT.dat'
endif
write(*,*) "input_file: ", ConfigFile

! Read input file to define parameters of post-processing
if (HOS_simu_type == 'ocean') then
    CALL read_input_ocean(ConfigFile)
elseif (HOS_simu_type == 'NWT') then
    CALL read_input_NWT(ConfigFile)
else 
    write(*,*) 'error, the type of smulation does not exist'
    pause
endif


if (iargc() > 0) then
call getarg(1, HOS_simu_type)
endif

write(*,*) "Type of HOS simulation: 'ocean' or 'NWT'", HOS_simu_type

if (HOS_simu_type == 'ocean') then
    call Post_processing_ocean
elseif (HOS_simu_type == 'NWT') then
    call Post_processing_NWT
else 
    write(*,*) 'error, the type of smulation does not exist'
    pause
endif

END PROGRAM Main