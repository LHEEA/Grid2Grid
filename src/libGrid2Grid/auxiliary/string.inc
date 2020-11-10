subroutine splitLine(str, nArg, cArg, scArg, lcArg, iArg, rArg, numFlag, iflag)
!!----------------------------------------------------------------------------------
!!
!!  Split character line to character, integer, real argument by space (' ')
!!
!!  YoungMyung Choi, Ecole Centrale de Nantes
!!
!!----------------------------------------------------------------------------------
!!  Input
!!      tLine : Character Line
!!
!!  Output
!!      nArg     : Number of Argument (if nArg = 0, no Argument)
!!      cArg     : Character argument array, cArg(nArg)
!!      scArg    : Lower character argument array, cArg(nArg)
!!      lcArg    : Upper character argument array, cArg(nArg)
!!      iArg     : Integer   argument array, iArg(nArg)
!!      rArg     : Real      argument array, rArg(nArg)
!!      numFlag  : Logical numeric data argument array, numFlag(nArg)
!!                 if given argument if numeric data, numFlag is .TRUE.
!!      iflag    : Check given tLine is blank line (if blank, iflag = .FALSE.)
!!----------------------------------------------------------------------------------
    implicit none
    character(len=*), intent(inout) :: str
    integer, intent(inout) :: nArg
    character(len = StringLength), allocatable, intent(inout) :: cArg(:), scArg(:), lcArg(:)
    integer, allocatable, intent(inout)                       :: iArg(:)
    real(RP), allocatable, intent(inout)                      :: rArg(:)
    logical,allocatable, intent(inout)                        :: numFlag(:)
    logical, intent(out) :: iflag
    !----------------------------------------------------
    integer :: i, io
    character(len=StringLength), allocatable :: args(:)
    character(len=StringLength)              :: dummyStr
    real :: dummyReal
    iflag = .TRUE.
    call parse(str, ' ', args, nArg)
    if (nArg.eq.0) then
        nArg  = 0
        iflag = .FALSE.
        return
    end if
    if (allocated(cArg))    deallocate(cArg)
    if (allocated(scArg))   deallocate(scArg)
    if (allocated(lcArg))   deallocate(lcArg)
    if (allocated(iArg))    deallocate(iArg)
    if (allocated(rArg))    deallocate(rArg)
    if (allocated(numFlag)) deallocate(numFlag)
    allocate(cArg(nArg), scArg(nArg), lcArg(nArg), iArg(nArg), rArg(nArg), numFlag(nArg))
    iArg    = 0
    rArg    = 0.0
    numFlag = .FALSE.
    do i = 1, nArg
        dummyStr = trim(args(i))
        cArg(i)  = dummyStr
        scArg(i) = lower(dummyStr)
        lcArg(i) = upper(dummyStr)
        read(dummyStr, *, iostat = io) dummyReal
        rArg(i) = dummyReal
        iArg(i) = int(rArg(i))
        if (io.eq.0) numFlag(i) = .TRUE.
    end do
end subroutine

!**********************************************************************

subroutine parse(str,delims,args,nargs)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Parses the string 'str' into arguments args(1), ..., args(nargs) based on
    ! the delimiters contained in the string 'delims'. Preceding a delimiter in
    ! 'str' by a backslash (\) makes this particular instance not a delimiter.
    ! The integer output variable nargs contains the number of arguments found.
	implicit none
    character(len=*) :: str,delims
    character(len=len_trim(str)) :: strsav
    character(len=*),allocatable :: args(:)
    character(len=len_trim(str)),allocatable :: tempArgs(:)
    integer :: i, na, nargs, k, lenstr
    character(len=1) :: dummy

    strsav=str
    call compact(str)
    na = STORAGE_SIZE (str) / STORAGE_SIZE (dummy)
    if (na.le.0) then
        nargs = 0
        return
    endif
    allocate(args(na), tempArgs(na))
    !na=size(args)
    do i=1,na
        args(i)=' '
    end do
    nargs=0
    lenstr=len_trim(str)
    if(lenstr==0) return
    k=0

    do
        if(len_trim(str) == 0) exit
        nargs=nargs+1
        call split(str,delims,args(nargs))
        call removebksl(args(nargs))
        call compact(args(nargs))
    end do
    tempArgs = args
    deallocate(args)
    allocate(args(nargs))
    do i = 1, nargs
        args(i) = tempArgs(i)
    enddo
    deallocate(tempArgs)
    str=strsav

end subroutine parse

!**********************************************************************

subroutine compact(str)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Converts multiple spaces and tabs to single spaces; deletes control characters;
    ! removes initial spaces.
    character(len=*):: str
    character(len=1):: ch
    character(len=len_trim(str)):: outstr
    str=adjustl(str)
    lenstr=len_trim(str)
    outstr=' '
    isp=0
    k=0
    do i=1,lenstr
        ch=str(i:i)
        ich=iachar(ch)
        select case(ich)
        case(9,32)     ! space or tab character
            if(isp==0) then
                k=k+1
                outstr(k:k)=' '
            end if
            isp=1
        case(33:)      ! not a space, quote, or control character
            k=k+1
            outstr(k:k)=ch
            isp=0
        end select
    end do
    str=adjustl(outstr)
end subroutine compact

!**********************************************************************

subroutine split(str,delims,before,sep)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Routine finds the first instance of a character from 'delims' in the
    ! the string 'str'. The characters before the found delimiter are
    ! output in 'before'. The characters after the found delimiter are
    ! output in 'str'. The optional output character 'sep' contains the
    ! found delimiter. A delimiter in 'str' is treated like an ordinary
    ! character if it is preceded by a backslash (\). If the backslash
    ! character is desired in 'str', then precede it with another backslash.

    character(len=*) :: str,delims,before
    character,optional :: sep
    logical :: pres
    character :: ch,cha

    pres=present(sep)
    str=adjustl(str)
    call compact(str)
    lenstr=len_trim(str)
    if(lenstr == 0) return        ! string str is empty
    k=0
    ibsl=0                        ! backslash initially inactive
    before=' '
    do i=1,lenstr
        ch=str(i:i)
        if(ibsl == 1) then          ! backslash active
            k=k+1
            before(k:k)=ch
            ibsl=0
            cycle
        end if
        if(ch == '\') then          ! backslash with backslash inactive
            k=k+1
            before(k:k)=ch
            ibsl=1
            cycle
        end if
        ipos=index(delims,ch)
        if(ipos == 0) then          ! character is not a delimiter
            k=k+1
            before(k:k)=ch
            cycle
        end if
        if(ch /= ' ') then          ! character is a delimiter that is not a space
            str=str(i+1:)
            if(pres) sep=ch
            exit
        end if
        cha=str(i+1:i+1)            ! character is a space delimiter
        iposa=index(delims,cha)
        if(iposa > 0) then          ! next character is a delimiter
            str=str(i+2:)
            if(pres) sep=cha
            exit
        else
            str=str(i+1:)
            if(pres) sep=ch
            exit
        end if
    end do
    if(i >= lenstr) str=''
    str=adjustl(str)              ! remove initial spaces
    return

end subroutine split

!**********************************************************************

subroutine removebksl(str)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Removes backslash (\) characters. Double backslashes (\\) are replaced
    ! by a single backslash.

    character(len=*):: str
    character(len=1):: ch
    character(len=len_trim(str))::outstr

    str=adjustl(str)
    lenstr=len_trim(str)
    outstr=' '
    k=0
    ibsl=0                        ! backslash initially inactive

    do i=1,lenstr
        ch=str(i:i)
        if(ibsl == 1) then          ! backslash active
            k=k+1
            outstr(k:k)=ch
            ibsl=0
            cycle
        end if
        if(ch == '\') then          ! backslash with backslash inactive
            ibsl=1
            cycle
        end if
        k=k+1
        outstr(k:k)=ch              ! non-backslash with backslash inactive
    end do

    str=adjustl(outstr)

end subroutine removebksl

!**********************************************************************

FUNCTION Upper(s1)  RESULT (s2)
    CHARACTER(*)       :: s1
    CHARACTER(LEN(s1)) :: s2
    CHARACTER          :: ch
    INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
    INTEGER            :: i

    DO i = 1,LEN(s1)
        ch = s1(i:i)
        IF (ch >= 'a'.AND.ch <= 'z') ch = CHAR(ICHAR(ch)+DUC)
        s2(i:i) = ch
    END DO
END FUNCTION Upper

FUNCTION Lower(s1)  RESULT (s2)
    CHARACTER(*)       :: s1
    CHARACTER(LEN(s1)) :: s2
    CHARACTER          :: ch
    INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
    INTEGER            :: i

    DO i = 1,LEN(s1)
        ch = s1(i:i)
        IF (ch >= 'A'.AND.ch <= 'Z') ch = CHAR(ICHAR(ch)-DUC)
        s2(i:i) = ch
    END DO
END FUNCTION Lower

Logical Function isAllTrue(logicalArray)
    implicit none
    logical, dimension(:), intent(in) :: logicalArray
    integer :: nn, i
    logical :: dummy
    isAllTrue = .TRUE.
    nn = STORAGE_SIZE (logicalArray) / STORAGE_SIZE (dummy)
    if (nn.eq.0) then
        isAllTrue = .FALSE.
        return
    end if
    do i = 1, nn
        if(logicalArray(i).EQV..FALSE.) then
            isAllTrue = .FALSE.
            return
        end if
    enddo
end function

!!!- Converts an integer to a string
CHARACTER(LEN=12) FUNCTION  int2str(int)
    !CHARACTER(LEN=*) :: int2str
    INTEGER          :: int
    WRITE(int2str,*) int
    int2str = TRIM(ADJUSTL(int2str))
END FUNCTION int2str

!!!- Converts a string to an integer
FUNCTION strtoint(lstr)
    CHARACTER*(*) :: lstr
    INTEGER       :: result,leng,i,strtoint
    leng   = len(lstr)
    result = 0
    DO i=1,leng
        IF (ichar(lstr(i:i))<58 .and.ichar(lstr(i:i))>=48) THEN
            result = 10*result+(ichar(lstr(i:i))-48)
        ENDIF
    ENDDO
    strtoint = result
END FUNCTION strtoint
