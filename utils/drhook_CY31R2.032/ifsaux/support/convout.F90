integer function convout(kcount,ktype)
real zconv(0:5)
integer kcount,ktype
zconv=(/0.25,1.0,2.0,2.0,1.0,1.0/)
convout=ceiling(kcount/zconv(ktype))
return
end function convout
