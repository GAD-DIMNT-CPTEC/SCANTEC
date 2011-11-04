SUBROUTINE template_read(fname)
   IMPLICIT NONE
   character(len=*), intent(IN) :: fname

   character(len=*),parameter :: myname='template_read'


    !
    !  0. Hello
    !

    PRINT*,'Hello from ', myname

    print*,trim(fname)


END SUBROUTINE
