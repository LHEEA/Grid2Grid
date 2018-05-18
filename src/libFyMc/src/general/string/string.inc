Subroutine setString(this, string, isTrim)
    Implicit None
    class(typString), intent(inout) :: this
    Character(len=*), intent(in) :: string
    logical,optional :: isTrim
    integer :: nChar
    character(len=len_trim(string)) tmpString

    tmpString = string
    this%str_ = ''
    nChar = sizeof(tmpString)/ sizeof("a")

    if (nChar.gt.CHAR_LEN) then
        write(*,*) "    [WARNING] setString(string), string is longer than CHAR_LEN."
        nChar = CHAR_LEN
    end if

    this%nStr_ = nChar
    this%str_(1:this%nStr_) = string

    if (present(isTrim)) then
        if (isTrim) then
            this%str_ = ""
            Call compactChar(tmpString)
            nChar = len_trim(tmpString)
            this%nStr_ = nChar
            this%str_(1:this%nStr_) = trim(string)
        end if
    end if

End Subroutine

function getString(this) result(string)
    Implicit None
    class(typString), intent(in) :: this
    character(len=this%nStr_) :: string
    string = this%str_(1:this%nStr_)
End function

function getStringLower(this) result(string)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Removes backslash (\) characters. Double backslashes (\\) are replaced
    ! by a single backslash.
    !
    !   Modified by Y.M Choi
    !
    class(typString), intent(in) :: this
    character(len=this%nStr_)       :: string
    CHARACTER          :: ch
    INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
    INTEGER            :: i
    DO i = 1, this%nStr_
        ch = this%str_(i:i)
        IF (ch >= 'A'.AND.ch <= 'Z') ch = CHAR(ICHAR(ch)-DUC)
        string(i:i) = ch
    END DO
end function

function getStringUpper(this)  RESULT (string)
    !
    !  Dr. George Benthiem's string subroutine
    !
    !       See. http://gbenthien.net/about.html
    !
    ! Removes backslash (\) characters. Double backslashes (\\) are replaced
    ! by a single backslash.
    !
    !   Modified by Y.M Choi
    !
    class(typString), intent(in) :: this
    character(len=this%nStr_)       :: string
    CHARACTER          :: ch
    INTEGER,PARAMETER  :: DUC = ICHAR('A') - ICHAR('a')
    INTEGER            :: i
    DO i = 1, this%nStr_
        ch = this%str_(i:i)
        IF (ch >= 'a'.AND.ch <= 'z') ch = CHAR(ICHAR(ch)+DUC)
        string(i:i) = ch
    END DO

END FUNCTION

function getStringInt(this, isWeek) result(value)
    Implicit None
    Class(typString), intent(in) :: this
    integer :: value
    logical, optional :: isWeek
    character(len=CHAR_LEN) :: dummyStr
    integer  :: io
    real(rp) :: valueR

    if (present(isWeek)) then
        if (isWeek.EQV..FALSE.) then
            valueR = getStringReal(this)
            value = int(valueR)

        end if
    else
        dummyStr = trim(this%str_)
        read(dummyStr, *, iostat = io) value
        if (io.ne.0) then
            write(*,*) "    [ERROR] string:getInt(), conversion failed."
            write(*,*) "      given string : ", trim(this%str_)
            value = 0.0_RP
            stop
        end if
    end if

end function

function getStringReal(this) result(value)
    Implicit None
    Class(typString), intent(in) :: this
    real(rp) :: value
    real(rp) :: dummyReal
    integer  :: io
    character(len=CHAR_LEN) :: dummyStr
    dummyStr = trim(this%str_)
    read(dummyStr, *, iostat = io) dummyReal
    value = dummyReal
    if (io.ne.0) then
        write(*,*) "    [ERROR] string:getReal(), conversion failed."
        write(*,*) "      given string : ", trim(this%str_)
        value = 0.0_RP
        stop
    end if
end function

Subroutine clearString(this)
    Implicit None
    class(typString), intent(inout) :: this
    this%str_=""
    this%nStr_=0
End Subroutine

subroutine copyString(origin, target)
Implicit None
    Class(typString), intent(inout) :: origin
    Type(typString), intent(in)     :: target

    origin%nStr_ = target%nStr_
    origin%str_  = target%str_

end subroutine
