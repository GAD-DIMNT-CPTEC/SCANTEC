SUBROUTINE eta_read(fname)
   IMPLICIT NONE
   character(len=*), intent(IN) :: fname

   character(len=*),parameter :: myname='eta_read'


    !
    !  0. Hello
    !

    PRINT*,'Hello from ', myname

    print*,trim(fname)


END SUBROUTINE
