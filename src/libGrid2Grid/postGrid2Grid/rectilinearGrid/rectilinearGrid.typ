Type :: typRectilinearGrid

    private
        !!- Number of Mesh (X, Y)
        Integer             :: nX_, nY_, nZ_

        !!- Number of Mesh (Z-Direction)
        Integer             :: nZmin_, nZmax_

        !!- Domain min-position
        Real(RP)            :: xMin_, yMin_, zMin_

        !!- Domain max-position
        Real(RP)            :: xMax_, yMax_, zMax_

        !!- Domain Length
        Real(RP)            :: Lx_, Ly_, Lz_

        !!- Mesh Node
        Real(RP),allocatable,public :: X_(:), Y_(:), Z_(:)

        !!- Moving Z
        Real(RP),allocatable :: movingZ_(:,:,:)

        !!- Eta, Flow velocity, pressure
        Real(RP),allocatable,public :: eta_(:,:,:), u_(:,:,:), v_(:,:,:), w_(:,:,:) , pd_(:,:,:)

        !!- Meshing Optional value (Geometric Ratio Mesh for z direction)
        Real(RP)                    :: zMinRatio_ = 1.0
        Real(RP)                    :: zMaxRatio_ = 1.0

contains

    procedure, public :: initialize => buildRectLinearMesh

    procedure         :: destroy => destroyRectLGrid

    final             :: finalRectLgrid

end Type
