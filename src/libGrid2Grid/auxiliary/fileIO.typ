! File IO Setting
integer :: fidUnit = 101

type typFileIO
    character(len=StringLength) :: name
    integer                     :: unit
end type
