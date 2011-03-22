MODULE COMMOM
   USE m_inpak90
   USE m_die, only : perr

   IMPLICIT NONE
   PUBLIC

   REAL, ALLOCATABLE, DIMENSION(:)  :: longitude
   REAL, ALLOCATABLE, DIMENSION(:)  :: latitude
   REAL, ALLOCATABLE, DIMENSION(:)  :: niveis
   CHARACTER(len=15), ALLOCATABLE, DIMENSION(:)  :: varnames
   INTEGER                          :: xdef_num
   INTEGER                          :: ydef_num
   INTEGER                          :: zdef_num
   INTEGER                          :: vars_num



   CONTAINS
      SUBROUTINE get_def( nfile,def,num, levs, istat )
      !
      ! Rotina para ler os definicoes 
      ! xdef, ydef, zdef de um dado ctl
      !
         IMPLICIT NONE

         CHARACTER(len=*),                INTENT(IN)   :: nfile
         CHARACTER(len=*),                INTENT(IN)   :: def
         INTEGER,                         INTENT(OUT)  :: num
         REAL, DIMENSION(:), ALLOCATABLE, INTENT(OUT)  :: levs
         INTEGER, OPTIONAL,               INTENT(OUT)  :: istat

         CHARACTER(len=*),PARAMETER    :: myname_="get_def"
         INTEGER                       :: I, iret
         REAL                          :: icor, incr
         CHARACTER(len=10)             :: mapping

         if(present(istat)) istat=0

         call i90_LoadF ( TRIM(nfile), iret )   ! Load arquivo descritor

         if(iret /= 0) then
            call perr(myname_,'i90_LoadF("'//trim(nfile)//'")',iret)
            istat=iret
            return
         endif

         call i90_label ( TRIM(def), iret )     ! Le linha com a referencia
         if(iret /= 0) then
            call perr(myname_,'i90_label("'//trim(def)//'")',iret)
            istat=iret
            return
         endif

         num = i90_gint(iret)
         ALLOCATE(levs(num),stat=iret)
         if(iret /= 0) then
            call perr(myname_,'allocate(levs)',iret)
            return
         endif

         call i90_Gtoken ( mapping, iret )
         SELECT CASE ( TRIM(mapping) )

            CASE ("linear","LINEAR")
               icor = i90_gfloat(iret)
               incr = i90_gfloat(iret)
               DO I=0,num-1
                 levs(i+1)=icor+(incr*i)
               END DO
            CASE ("levels","LEVELS")
               I=1
               DO while(I.le.num)
                  levs(i) = i90_gfloat ( iret )
                  IF (iret.eq.-1)THEN
                     call i90_gline ( iret )
                  ELSE
                     I=I+1
                  END IF
               ENDDO
   
         END SELECT

   END SUBROUTINE

   SUBROUTINE get_vars( nfile, num ,vars, istat )
      !
      ! Carrega o nome das variaveis e o numero de 
      ! niveis de cada uma
      !
         IMPLICIT NONE

         CHARACTER(len=*),                INTENT(IN)   :: nfile
         INTEGER,                         INTENT(OUT)  :: num
         REAL, DIMENSION(:), ALLOCATABLE, INTENT(OUT)  :: vars
         INTEGER, OPTIONAL,               INTENT(OUT)  :: istat

         CHARACTER(len=*),PARAMETER    :: myname_="get_vars"
         INTEGER                       :: I, iret
         REAL                          :: icor, incr
         CHARACTER(len=10)             :: mapping

         if(present(istat)) istat=0

         call i90_LoadF ( TRIM(nfile), iret )   ! Load arquivo descritor

         if(iret /= 0) then
            call perr(myname_,'i90_LoadF("'//trim(nfile)//'")',iret)
            istat=iret
            return
         endif

         call i90_label ( 'vars', iret )
         num = i90_gint(iret)
         ALLOCATE(vars(num),stat=iret)
         if(iret /= 0) then
            call perr(myname_,'allocate(vars)',iret)
            return
         endif

         DO I=1,num
            call i90_Gtoken(vars(i), iret )
            call i90_gline ( iret )
         ENDDO
   
   END SUBROUTINE

END MODULE

