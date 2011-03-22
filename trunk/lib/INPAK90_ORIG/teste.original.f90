PROGRAM TESTE
   USE m_inpak90

   real    rr
   integer ii
   character*20 fn1, fn2, fn3
   integer      iret
   real          table(7,3)
   real          levs(44)
   character*20  word

   call i90_LoadF ( 'teste.rc', iret )

   call i90_label ( 'constants:', iret )
   print*, iret
   rr = i90_gfloat(iret)    ! results in r = 3.1415
   ii = i90_gint(iret)      ! results in i = 25

   call i90_label ( 'my_file_names:', iret )
   call i90_Gtoken ( fn1, iret )  ! ==> fn1 = 'jan87.dat'
   call i90_Gtoken ( fn2, iret )  ! ==> fn1 = 'jan88.dat'
   call i90_Gtoken ( fn3, iret )  ! ==> fn1 = 'jan89.dat'

   call i90_label ( 'my_table_name::', iret )
    do i = 1, 7
       call i90_gline ( iret )
       print*, 'iret::',iret
       do j = 1, 3
          table(i,j) = i90_gfloat ( iret )
       end do                   
    end do


           call i90_label ( 'levs::', iret )
            I=1
            DO while(I.le.45)
               levs(i) = i90_gfloat ( iret )
               IF (iret.eq.-1)THEN
                  call i90_gline ( iret )
                  print*,iret
               ELSE
                  I=I+1
               END IF
            ENDDO
            print*,levs
stop
   PRINT*,'my_file_names:', fn1, fn2, fn3
   PRINT*,'constants:',rr,ii
   PRINT*,'my_table_name::'
       do i = 1, 7
!       do j = 1, 3
          PRINT*,(table(i,j),j=1,3)
 !      end do                   
    end do

   call i90_label ( 'Meu nome:', iret )
   call i90_Gtoken ( fn1, iret ) 
   print*,'NOME:', fn1

END PROGRAM

