SUBROUTINE agcm_read(fname)
   IMPLICIT NONE
   character(len=*), intent(IN) :: fname

   character(len=*),parameter :: myname='agcm_read'


    !
    !  0. Hello
    !

    WRITE(6,'(    2A)')'Hello from ', myname
    WRITE(6,'(A,1X,A)')'Open File ::', trim(fname)

    !
    !  1. 
    !
END SUBROUTINE
