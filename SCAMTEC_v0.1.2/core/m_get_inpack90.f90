MODULE m_read_nml

  USE m_inpak90
  USE m_die, only : perr

  IMPLICIT NONE
  PRIVATE

  INTERFACE get_value
     MODULE PROCEDURE get_real, get_int, get_char
  END INTERFACE get_value

contains 

  subroutine get_real(label,value,iret)
    implicit none
    character(len=*),  intent(in)  :: label
    real,              intent(out) :: value
    integer,           intent(out) :: iret

    character(len=36) :: msg

    iret = 0

    call i90_label(label, iret)
    if (iret .eq. 0 )then
       value = i90_gint(iret)
    else
       write(msg,'(3A)')'i90_label("',trim(label),'")' 
       call perr(myname_,msg,iret)
       return
    endif


  endsubroutine get_real

  subroutine get_int(label,value,iret)
    implicit none
    character(len=*),  intent(in)  :: label
    integer,           intent(out) :: value
    integer,           intent(out) :: iret

    character(len=36) :: msg

    iret = 0

    call i90_label(label, iret)
    if (iret .eq. 0 )then
       value = i90_gfloat(iret)
    else
       write(msg,'(3A)')'i90_label("',trim(label),'")' 
       call perr(myname_,msg,iret)
       return
    endif

  endsubroutine get_int

  subroutine get_char(label,value,iret)
    implicit none
    character(len=*),  intent(in)  :: label
    character(len=*),  intent(out) :: value
    integer,           intent(out) :: iret

    character(len=36) :: msg

    iret = 0

    call i90_label(label, iret)
    if (iret .eq. 0 )then
       call Gtoken(value,iret)
    else
       write(msg,'(3A)')'i90_label("',trim(label),'")' 
       call perr(myname_,msg,iret)
       return
    endif

  endsubroutine get_char



END MODULE m_read_nml
