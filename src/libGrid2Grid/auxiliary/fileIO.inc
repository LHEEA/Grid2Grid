!!!- Auto file unit indexing for Grid2Grid
integer function callFileUnit()
    callFileUnit = fidUnit
    fidUnit = fidUnit + 1
end function

!!!- Special care !!!!!
integer function RemoveFileUnit()
    fidUnit = fidUnit - 1
    RemoveFileUnit = fidUnit
end function
