!!- HOS NWT mode array type
type,private :: typHOSNWTmode

    integer :: nXMode, nYMode, nAddMode

    !!- Mode amplitudes
    real(rp),allocatable :: mode(:,:)
    real(rp),allocatable :: modeX(:,:), modeY(:,:), modeZ(:,:)

    real(rp),allocatable :: modet(:,:), modeFS(:,:), modeFSt(:,:), modeFSx(:,:), modeFSy(:,:)

    real(rp),allocatable :: modeAdd(:,:), modeAddt(:,:)

    !!- Wave Numbers
    real(rp),allocatable :: kx(:), ky(:), kxy(:,:), ktheta(:,:)
    real(rp),allocatable :: kxAdd(:)

    contains

        procedure, pass :: allocNWTMode

        !!- Deallocate Dynamic Array of HOS Ocean Mode
        procedure, pass :: destroy => destroyNWTMode

        !!- Destroyer
        final           :: finalHOSNWTMode

end type
