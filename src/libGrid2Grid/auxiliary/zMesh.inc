subroutine buildZmesh(zMin, zMax, nZmin, nZmax, Z, iflag, ratio1, ratio2)
    implicit none
    real(rp),intent(in)                :: zMin, zMax
    integer,intent(in)                 :: nZmin, nZmax
    real(rp),allocatable,intent(inout) :: Z(:)
    integer, optional                  :: iflag
    real(rp),optional                  :: ratio1, ratio2
    !-----------------------------------------------------------------
    logical  :: buildAbove = .FALSE.
    real(rp) :: dz, Lz1, Lz2, commonRatio
    integer  :: nZ, iz, jz
    real(rp) :: dummyR

    nZ = nZmin + nZmax

    buildAbove = .FALSE.
    if (nZmax.gt.0) buildAbove = .TRUE.

    if (nZ.le.2) then
	write(*,*) "    [Error] buildZmesh(zMin, zMax, nZmin, nZmax, Z, iflag, ratio1, ratio2)"
	write(*,*) "        Number of Zmesh should be larger than 2"
	stop
    end if

    if (allocated(Z)) then
	if (sizeof(Z) / sizeof(dummyR).ne.nZ) then
	    write(*,*) "    [Error] buildZmesh(zMin, zMax, nZmin, nZmax, Z, iflag, ratio1, ratio2)"
	    write(*,*) "        Number of Zmesh and given Z(:) array size is different"
	    stop
	endif
    else
	allocate(Z(nZ))
    endif

    if (present(iflag).and.(iflag.eq.INDEX_SINE_MESH)) then
	if (buildAbove) then
	    Lz1 = abs(zMin)
	else
	    Lz1 = zMax - zMin
	endif

	do iz = 1, nZmin
	    Z(iz) = zMin + Lz1 * sin(PIO2 * real(iz - 1, RP) / real(nZmin-1,RP))
	enddo

	if (nZmax.ge.1.0) then
	    Lz2 = zMax - Z(nZmin)
	    do jz = 1, nZmax
		iz = nZmin + jz
		Z(iz) = Z(nZmin) &
			+ Lz2 * (1.0_RP + sin(3.0_RP*PIO2 + PIO2*real(jz , RP)/ real(nZmax,RP)))
	    end do
	endif

    else if (present(iflag).and.(iflag.eq.INDEX_GEOMETRICRATIO_MESH)) then

    	if (present(ratio1).EQV..FALSE.) ratio1 = 1.0_RP
    	if (present(ratio2).EQV..FALSE.) ratio2 = 1.0_RP

        if (buildAbove) then
    	    Lz1 = abs(zMin)
    	    if (nZmin.le.2) then
        		write(*,*) "    [Error] buildZmesh(zMin, zMax, nZmin, nZmax, Z, iflag, ratio1, ratio2)"
        		write(*,*) "        nZmin should be larger than 2"
        		stop
    	    end if
    	    if (nZmax.le.2) then
        		write(*,*) "    [Error] buildZmesh(zMin, zMax, nZmin, nZmax, Z, iflag, ratio1, ratio2)"
        		write(*,*) "        nZmax should be larger than 2"
        		stop
    	    end if

            if ( (dabs(ratio1) - 1.0_RP).ge.0.01_RP ) then
                commonRatio = ratio1**( 1.0_RP / (nZmin - 1.0_RP) )
            	dz = Lz1 * (1.0_RP - commonRatio) / (1.0_RP - commonRatio**(nZmin - 1.0_RP))
            	dz = dz * commonRatio**(nZmin - 2.0_RP)
            	Z(1) = zMin
            	do iz = 2, nZmin
            	    Z(iz) = Z(iz- 1) + dz
            	    dz = dz / commonRatio
            	enddo
            else
                dz = Lz1 / (nZmin - 1.0_RP)
        	    do iz = 1, nZ
        		Z(iz) = Zmin + dz * (iz- 1.0_RP)
        	    enddo
            end if

            if ( (dabs(ratio2) - 1.0_RP).ge.0.01_RP ) then
                if (nZmax.ge.1.0) then
            	    Lz2 = zMax
            	    commonRatio = ratio2**( 1.0_RP/ (nZmax - 1.0_RP) )
            	    dz = Lz2 * (1.0_RP - commonRatio) / (1.0_RP - commonRatio**(nZmax))
            	    do iz = nZmin + 1, nZmin + nZmax
            		Z(iz) = Z(iz- 1) + dz
            		dz = dz * commonRatio
            	    enddo
            	endif
            else
                dz = abs(Zmax) / real(nZmax,rp)

        	    do iz = nZmin + 1, nZmin + nZmax
                Z(iz) = Z(iz- 1) + dz
        	    enddo

            end if

    	else
    	    Lz1 = zMax - zMin
            if ( (dabs(ratio1) - 1.0_RP).ge.0.01_RP ) then
                commonRatio = ratio1**( 1.0_RP / (nZmin - 1.0_RP) )
            	dz = Lz1 * (1.0_RP - commonRatio) / (1.0_RP - commonRatio**(nZmin - 1.0_RP))
            	dz = dz * commonRatio**(nZmin - 2.0_RP)
            	Z(1) = zMin
            	do iz = 2, nZmin
            	    Z(iz) = Z(iz- 1) + dz
            	    dz = dz / commonRatio
            	enddo
            else
                dz = Lz1 / (nZmin - 1.0_RP)
        	    do iz = 1, nZ
        		Z(iz) = Zmin + dz * (iz- 1.0_RP)
        	    enddo
            end if

    	end if

    else
    	if (buildAbove) then
    	    dz = abs(Zmin) / (nZmin - 1.0_RP)
    	    do iz = 1, nZmin
    		Z(iz) = Zmin + dz * (iz- 1.0_RP)
    	    enddo

    	    dz = abs(Zmax) / real(nZmax,rp)
    	    do iz = 1, nZmax
    		jz = nZmin + iz
    		Z(jz) = real(dz * iz,rp)
    	    enddo
    	else
    	    dz = abs(Zmax - Zmin) / (nZ - 1.0_RP)
    	    do iz = 1, nZ
    		Z(iz) = Zmin + dz * (iz- 1.0_RP)
    	    enddo
    	end if
    end if

end subroutine
