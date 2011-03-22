!      use m_StrTemplate
      IMPLICIT NONE
      CHARACTER(len=300)   :: str, tmpl,  class
      INTEGER  :: nymd, nhms, stat
      INTEGER  :: date

      date=2004040106
      print*,INT ( Date/100 )
      print*,MOD(Date,100)
      print*,date-int(date/100)*100
      stop
!      tmpl="%y4%m2%d2%h2"
 !     nymd=20040401
 !     nhms=060000


!       print*, str,tmpl,class,xid,nymd,nhms,stat
!       call StrTemplate(str,tmpl,nnymd=nymd,nhms=nhms)

  !                  call StrTemplate ( str, tmpl, &
   !                             nymd=nymd, nhms=nhms )
    !   print*, str,tmpl,nymd,nhms

END
