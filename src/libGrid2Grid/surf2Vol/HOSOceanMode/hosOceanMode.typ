!!- HOS Ocean mode array type
type,private :: typHOSOceanMode

    integer :: nXo2p1, nYmode

    !!- Mode amplitudes
    complex(cp),allocatable :: mode(:,:)
    complex(cp),allocatable :: modeX(:,:), modeY(:,:), modeZ(:,:)
    complex(cp),allocatable :: modet(:,:), modeFS(:,:), modeFSt(:,:)
    complex(cp),allocatable :: modeFSx(:,:), modeFSy(:,:)

    !!- Wave Numbers
    real(rp), allocatable :: kx(:), kyN2(:), ktheta(:,:), kxy(:,:)
    complex(cp), allocatable :: ikx(:,:), iky(:,:)

    !!- Wave frequency
    Real(RP), allocatable :: omega(:, :)

    !!- HOS Weight
    Real(RP), allocatable :: weight(:, :)

    contains

        !!- Allocate Dynamic Array
        procedure, pass :: allocOceanMode

        !!- Deallocate Dynamic Array of HOS Ocean Mode
        procedure, pass :: destroy => destroyOceanMode

        !!- Destroyer
        final           :: finalHOSOceanMode

end type
