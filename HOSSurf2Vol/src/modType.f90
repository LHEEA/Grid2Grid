MODULE modType
!
! definition de symboles pour les types reels (RP) et complexes (CP)
!
! les reels, simple ou double precision
INTEGER, PARAMETER :: SP = KIND(1.0)
INTEGER, PARAMETER :: DP = KIND(1.0D0)
!
! les complexes, simple ou double precision
INTEGER, PARAMETER :: SPC = KIND(1.0)
INTEGER, PARAMETER :: DPC = KIND(1.0D0)
!
! Les types courants
INTEGER, PARAMETER :: RP = DP
INTEGER, PARAMETER :: CP = DPC
!
! les constantes mathematiques usuelles i, pi, 2pi, pi/2, racine de 2.
COMPLEX(CP), PARAMETER :: iMaginary = CMPLX(0.0_rp, 1.0_rp, KIND=CP)
REAL(RP), PARAMETER    :: PI    = 3.141592653589793238462643383279502888419_rp
REAL(RP), PARAMETER    :: g     = 9.81_rp
REAL(RP), PARAMETER    :: PIO2  = 1.570796326794896619231321691639751442098_rp
REAL(RP), PARAMETER    :: TWOPI = 6.283185307179586476925286766559005768394_rp
REAL(RP), PARAMETER    :: SQ2   = 1.414213562373095048801688724209698078569_rp
!
! For comparison of real numbers
REAL(RP), PARAMETER    :: tiny = epsilon(1.0_rp)
!
INTEGER, PARAMETER :: StringLength = 200

CONTAINS
!
CHARACTER(LEN=12) FUNCTION  int2str(int)
! Converts an integer to a string
!CHARACTER(LEN=*) :: int2str
INTEGER          :: int
WRITE(int2str,*) int
int2str = TRIM(ADJUSTL(int2str))
END FUNCTION int2str
!
!
FUNCTION strtoint(lstr)
!
! Converts a string to an integer
CHARACTER*(*) :: lstr
INTEGER       :: result,leng,i,strtoint
!
leng   = len(lstr)
result = 0
DO i=1,leng
  IF (ichar(lstr(i:i))<58 .and.ichar(lstr(i:i))>=48) THEN 
     result = 10*result+(ichar(lstr(i:i))-48)
  ENDIF
ENDDO
strtoint = result
END FUNCTION strtoint
!
!
END MODULE modType
