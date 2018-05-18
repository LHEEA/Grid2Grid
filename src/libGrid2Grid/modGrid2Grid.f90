!-----------------------------------------------------------------------
Module modGrid2Grid
!-----------------------------------------------------------------------
!
!   Grid2Grid Module to connect to other CFD solvers (fortran and C++ 'OpenFOAM')
!   Reference should be made to:
!   "Grid2Grid : HOS Wrapper Program" YoungMyung Choi, Maite Gouin, Guillaume Ducrozet,
!    Benjamin Bousscasse and Pierre Ferrant
!-----------------------------------------------------------------------
!
!   Copyright (C) 2017 - LHEEA Lab., Ecole Centrale de Nantes, UMR CNRS 6598
!
!   This program is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with this program.  If not, see <https://www.gnu.org/licenses/>.
!-----------------------------------------------------------------------
use iso_c_binding
use modGrid2GridGlobal
use modVol2Vol

use mfpGeneral, only : typDictionaryPtr
!-----------------------------------------------------------------------
Implicit None
!-----------------------------------------------------------------------

Integer, parameter :: nMaxVol2Vol = 100

Type typinpVol2Vol

    Logical                       :: isActive = .FALSE.

    Logical                       :: isDictInput = .FALSE.

    Type(typDictionaryPtr)        :: dict_

    character(len=StringLength)   :: solver_

    character(len=StringLength)   :: fileName_

    double precision              :: zMin, zMax

    integer                       :: nZmin, nZmax

    double precision              :: zMinRatio, zMaxRatio

End Type

Integer                                     :: nVol2Vol

Type(typinpVol2Vol), dimension(nMaxVol2Vol) :: inpVol2Vol
Type(typHOSVol2Vol), dimension(nMaxVol2Vol) :: vol2vol_

contains

    subroutine checkAndIntialize(this, v2vIndex)
        implicit none
        type(typinpVol2Vol),intent(in) :: this
        integer, intent(out)        :: v2vIndex
        logical :: isEqual
        integer :: i, caseIndex

        if (nVol2Vol.eq.0) then
            nVol2Vol = nVol2Vol + 1
            Call initializeV2V(this, nVol2Vol)
            v2vIndex = nVol2Vol
        else
            do i = 1, nVol2Vol
                isEqual = .TRUE.
                if (trim(this%solver_).ne.trim(inpVol2Vol(i)%solver_))     isEqual = .FALSE.
                if (trim(this%fileName_).ne.trim(inpVol2Vol(i)%fileName_)) isEqual = .FALSE.
                if (this%zMin.ne.inpVol2Vol(i)%zMin)           isEqual = .FALSE.
                if (this%zMax.ne.inpVol2Vol(i)%zMax)           isEqual = .FALSE.
                if (this%nZmin.ne.inpVol2Vol(i)%nZmin)         isEqual = .FALSE.
                if (this%nZmax.ne.inpVol2Vol(i)%nZmax)         isEqual = .FALSE.
                if (this%zMinRatio.ne.inpVol2Vol(i)%zMinRatio) isEqual = .FALSE.
                if (this%zMaxRatio.ne.inpVol2Vol(i)%zMaxRatio) isEqual = .FALSE.

                if (isEqual) then
                    caseIndex = i
                    exit
                end if
            enddo

            if (isEqual) then
                v2vIndex = caseIndex
            else
                nVol2Vol = nVol2Vol + 1
                Call initializeV2V(this, nVol2Vol)
                v2vIndex = nVol2Vol
            end if

        end if

    end subroutine

    logical function checkV2VIndex(index, writeOption)
        integer,intent(in) :: index
        logical,optional   :: writeOption
        checkV2VIndex = .TRUE.
        if (index.gt.nMaxVol2Vol) then
            checkV2VIndex = .FALSE.
            if (present(writeOption).and.writeOption) then
                write(*,*) "vol2vol index is over than nMaxVol2Vol."
                write(*,*) "Please increase nMaxVol2Vol."
                write(*,*) " nMaxVol2Vol : ", nMaxVol2Vol
                write(*,*) " idx         : ", index
            end if

        else if (index.le.0) then
            checkV2VIndex = .FALSE.
            if (present(writeOption).and.writeOption) then
                write(*,*) "vol2vol idx is zero or negative !"
                write(*,*) " idx         : ", index
            end if
        end if
    end function

    subroutine initializeV2V(this, idx)
        implicit none
        type(typinpVol2Vol),intent(in) :: this
        integer, intent(in)            :: idx
        logical                        :: chkIndex

        chkIndex = checkV2VIndex(idx, .TRUE.)
        if (chkIndex.EQV..FALSE.) stop

        inpVol2Vol(idx)%isActive = .TRUE.

        inpVol2Vol(idx)%solver_    = this%solver_
        inpVol2Vol(idx)%fileName_  = this%fileName_
        inpVol2Vol(idx)%zMin       = this%zMin
        inpVol2Vol(idx)%zMax       = this%zMax
        inpVol2Vol(idx)%nZmin      = this%nZmin
        inpVol2Vol(idx)%nZmax      = this%nZmax
        inpVol2Vol(idx)%zMinRatio  = this%zMinRatio
        inpVol2Vol(idx)%zMaxRatio  = this%zMaxRatio

        inpVol2Vol(idx)%isDictInput  = this%isDictInput
        inpVol2Vol(idx)%dict_  = this%dict_

        if (inpVol2Vol(idx)%isDictInput) then

            Call vol2vol_(idx)%initialize(inpVol2Vol(idx)%dict_)

        else

            Call vol2vol_(idx)%initialize(inpVol2Vol(idx)%solver_, &
                                          inpVol2Vol(idx)%fileName_, &
                                          inpVol2Vol(idx)%zMin, &
                                          inpVol2Vol(idx)%zMax, &
                                          inpVol2Vol(idx)%nZmin, &
                                          inpVol2Vol(idx)%nZmax, &
                                          inpVol2Vol(idx)%zMinRatio, &
                                          inpVol2Vol(idx)%zMaxRatio)
        end if

    end subroutine

    subroutine destroyV2V(idx)
        implicit none
        integer, intent(in) :: idx
        logical             :: chkIndex

        chkIndex = checkV2VIndex(idx, .FALSE.)
        if (chkIndex.EQV..FALSE.) return

        inpVol2Vol(idx)%isActive = .FALSE.

        inpVol2Vol(idx)%solver_    = ""
        inpVol2Vol(idx)%fileName_  = ""
        inpVol2Vol(idx)%zMin       = 0.0
        inpVol2Vol(idx)%zMax       = 0.0
        inpVol2Vol(idx)%nZmin      = 0
        inpVol2Vol(idx)%nZmax      = 0
        inpVol2Vol(idx)%zMinRatio  = 0.0
        inpVol2Vol(idx)%zMaxRatio  = 0.0

        Call vol2vol_(idx)%destroy

    end subroutine

    subroutine initializeGrid2GridDict(dictFilePath, v2vIndex) &
        bind(c, name='__modgrid2grid_MOD_initializegrid2griddict')

        use mfpGlobal, only : CHAR_LEN
        use mfpGeneral, only : isStringEqual, separateFilePath

        implicit None
        character(kind=c_char, len=1), dimension(StringLength),intent(in) :: dictFilePath
        integer, intent(out)           :: v2vIndex
        Type(typinpVol2Vol)            :: tmpInpVol2Vol
        character(len=StringLength)    :: charF_dictFilePath
        integer :: i
        Type(typDictionaryPtr)         :: fileDict
        Character(len=CHAR_LEN)        :: HOSType, zMeshType
        Character(len=CHAR_LEN)        :: fileDir, fileName, fileExt

        charF_dictFilePath = ''
        do i = 1, StringLength
            if (dictFilePath(i) == c_null_char ) then
                exit
            else
                charF_dictFilePath(i:i) = dictFilePath(i)
            end if
        enddo

        Call separateFilePath(charF_dictFilePath, fileDir, fileName, fileExt)

        Call fileDict%initialize(fileDir, fileName, fileExt)

        HOSType = fileDict%getChar("Grid2Grid")
        tmpInpVol2Vol%dict_ = fileDict%subDict(HOSType)

        tmpInpVol2Vol%solver_    = tmpInpVol2Vol%dict_%getChar("type")
        tmpInpVol2Vol%fileName_  = tmpInpVol2Vol%dict_%getChar("filePath")
        tmpInpVol2Vol%zMin       = tmpInpVol2Vol%dict_%getReal("zMin")
        tmpInpVol2Vol%zMax       = tmpInpVol2Vol%dict_%getReal("zMax")
        tmpInpVol2Vol%nZmin      = tmpInpVol2Vol%dict_%getInt("nZMin")
        tmpInpVol2Vol%nZmax      = tmpInpVol2Vol%dict_%getInt("nZMax")

        zMeshType = tmpInpVol2Vol%dict_%getChar("zMeshType")

        if (isStringEqual(zMeshType,"meshRatio") )then
            tmpInpVol2Vol%zMinRatio  = tmpInpVol2Vol%dict_%getReal("zMinRatio")
            tmpInpVol2Vol%zMaxRatio  = tmpInpVol2Vol%dict_%getReal("zMaxRatio")
        else
            tmpInpVol2Vol%zMinRatio  = 1.0_RP
            tmpInpVol2Vol%zMaxRatio  = 1.0_RP
        end if

        tmpInpVol2Vol%isDictInput = .TRUE.

        !! Call HOS Vol2VOL initialize
        Call checkAndIntialize(tmpInpVol2Vol, v2vIndex)

    End Subroutine

    subroutine initializeGrid2Grid(hosSolver, hosFileName, zMin, zMax, nZmin, nZmax, zMinRatio, zMaxRatio, v2vIndex) &
        bind(c, name='__modgrid2grid_MOD_initializegrid2grid')
        implicit None
        character(kind=c_char, len=1), dimension(StringLength),intent(in) :: hosSolver
        character(kind=c_char, len=1), dimension(StringLength),intent(in) :: hosFileName
        Double precision, intent(in)   :: zMin, zMax
        integer, intent(in)            :: nZmin, nZmax
        Double precision, intent(in)   :: zMinRatio, zMaxRatio
        integer, intent(out)           :: v2vIndex
        !! --------------------------------------------------------------------------
        Type(typinpVol2Vol)            :: tmpInpVol2Vol
        character(len=StringLength)    :: charF_hosSolver
        character(len=StringLength)    :: charF_hosFileName
        integer :: i

        !! C Character and Fortran Character is Different !!!
        charF_hosSolver = ''
        do i = 1, StringLength
            if (hosSolver(i) == c_null_char ) then
                exit
            else
                charF_hosSolver(i:i) = hosSolver(i)
            end if
        enddo

        charF_hosFileName = ''
        do i = 1, StringLength
            if (hosFileName(i) == c_null_char ) then
                exit
            else
                charF_hosFileName(i:i) = hosFileName(i)
            end if
        enddo

        tmpInpVol2Vol%solver_    = charF_hosSolver
        tmpInpVol2Vol%fileName_  = charF_hosFileName
        tmpInpVol2Vol%zMin       = real(zMin, RP)
        tmpInpVol2Vol%zMax       = real(zMax, RP)
        tmpInpVol2Vol%nZmin      = nZmin
        tmpInpVol2Vol%nZmax      = nZmax
        tmpInpVol2Vol%zMinRatio  = real(zMinRatio, RP)
        tmpInpVol2Vol%zMaxRatio  = real(zMaxRatio, RP)

        !! Call HOS Vol2VOL initialize
        Call checkAndIntialize(tmpInpVol2Vol, v2vIndex)

    end subroutine

    subroutine correctGrid2Grid(v2vIndex, simulTime) &
        bind(c, name='__modgrid2grid_MOD_correctgrid2grid')
        implicit None
        integer, intent(in)          :: v2vIndex
        Double precision, intent(in) :: simulTime
        if (inpVol2Vol(v2vIndex)%isActive) then
            Call vol2vol_(v2vIndex)%correct(real(simulTime,RP))
        else
            write(*,*) "    [Error] correctGrid2Grid : wrong index is given., index : ", v2vIndex
            stop
        end if
    end subroutine

    subroutine getHOSeta(v2vIndex, x, y, t, eta) &
        bind(c, name='__modgrid2grid_MOD_gethoseta')
        implicit none
        integer, intent(in)           :: v2vIndex
        Double precision, intent(in)  :: x, y, t
        Double precision, intent(out) ::  eta
        real(rp)         :: tmp_eta
        if (inpVol2Vol(v2vIndex)%isActive) then
            tmp_eta = vol2vol_(v2vIndex)%getEta(real(x,rp), real(y, rp), real(t, rp))
            eta = dble(tmp_eta)
        else
            write(*,*) "    [Error] getHOSeta : wrong index is given., index : ", v2vIndex
            stop
        end if
    end subroutine

    subroutine getHOSU(v2vIndex, x, y, z, t, u, v, w) &
        bind(c, name='__modgrid2grid_MOD_gethosu')
        implicit none
        integer, intent(in)           :: v2vIndex
        Double precision, intent(in)  :: x, y, z, t
        Double precision, intent(out) :: u, v, w
        real(rp)          :: tmp_u, tmp_v, tmp_w
        if (inpVol2Vol(v2vIndex)%isActive) then
            Call vol2vol_(v2vIndex)%getU(real(x,rp), real(y, rp), real(z, rp), real(t, rp), tmp_u, tmp_v, tmp_w)
            u = dble(tmp_u)
            v = dble(tmp_v)
            w = dble(tmp_w)
        else
            write(*,*) "    [Error] getHOSU : wrong index is given., index : ", v2vIndex
            stop
        end if
    end subroutine

    subroutine getHOSPd(v2vIndex, x, y, z, t, pd) &
        bind(c, name='__modgrid2grid_MOD_gethospd')
        implicit none
        integer, intent(in)           :: v2vIndex
        double precision, intent(in)  :: x, y, z, t
        double precision, intent(out) :: pd
        real(rp) :: tmp_pd
        if (inpVol2Vol(v2vIndex)%isActive) then
            tmp_pd = vol2vol_(v2vIndex)%getPd(real(x,rp), real(y, rp), real(z, rp), real(t, rp))
            pd = dble(tmp_pd)
        else
            write(*,*) "    [Error] getHOSPd : wrong index is given., index : ", v2vIndex
            stop
        end if
    end subroutine

    subroutine getHOSFlow(v2vIndex, x, y, z, t, eta, u, v, w, pd) &
        bind(c, name='__modgrid2grid_MOD_gethosflow')
        implicit none
        integer, intent(in)           :: v2vIndex
        double precision, intent(in)  :: x, y, z, t
        double precision, intent(out) :: eta, u, v, w, pd
        real(rp)          :: tmp_eta, tmp_u, tmp_v, tmp_w, tmp_pd
        if (inpVol2Vol(v2vIndex)%isActive) then
            Call vol2vol_(v2vIndex)%getFlow(real(x,rp), real(y, rp), real(z, rp), real(t, rp), &
                          tmp_eta, tmp_u, tmp_v, tmp_w, tmp_pd)
            eta = dble(tmp_eta)
            u = dble(tmp_u)
            v = dble(tmp_v)
            w = dble(tmp_w)
            pd = dble(tmp_pd)
        else
            write(*,*) "    [Error] getHOSFlow : wrong index is given., index : ", v2vIndex
            stop
        end if
    end subroutine

    subroutine getHOSEndTime(v2vIndex, endTime) &
        bind(c, name='__modgrid2grid_MOD_gethosendtime')
        implicit none
        integer, intent(in)           :: v2vIndex
        double precision, intent(out) :: endTime
        if (vol2vol_(v2vIndex)%isInitialized_) then
            endTime = dble(vol2vol_(v2vIndex)%endTime_)
        else
            write(*,*) "    [Warning] Grid2Grid is not initialized, Grid2Grid::getEndTime(endTime)"
        end if
    end subroutine

    subroutine getHOSWaterDepth(v2vIndex, waterDepth) &
        bind(c, name='__modgrid2grid_MOD_gethoswaterdepth')
        implicit none
        integer, intent(in)           :: v2vIndex
        double precision, intent(out) :: waterDepth
        if (vol2vol_(v2vIndex)%isInitialized_) then
            waterDepth = dble(vol2vol_(v2vIndex)%waterDepth_)
        else
            write(*,*) "    [Warning] Grid2Grid is not initialized, Grid2Grid::getWaterDepth(waterDepth)"
        end if
    end subroutine

    subroutine isGrid2GridInitialized(v2vIndex, isG2Initialized) &
        bind(c, name='__modgrid2grid_MOD_isgrid2gridinitialized')
        implicit none
        integer, intent(in)  :: v2vIndex
        logical, intent(out) :: isG2Initialized
        if (inpVol2Vol(v2vIndex)%isActive) then
            isG2Initialized = vol2vol_(v2vIndex)%isInitialized_
        else
            write(*,*) "    [Warning] isGrid2GridInitialized : wrong index is given., index : ", v2vIndex
            isG2Initialized = .FALSE.
        end if
    end subroutine

!-----------------------------------------------------------------------
End Module
!-----------------------------------------------------------------------
