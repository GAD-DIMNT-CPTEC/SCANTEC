MODULE COMMOM
   USE m_inpak90
   USE m_die, only : perr

   IMPLICIT NONE
   PUBLIC

   REAL, ALLOCATABLE, DIMENSION(:)  :: longitude
   REAL, ALLOCATABLE, DIMENSION(:)  :: latitude
   REAL, ALLOCATABLE, DIMENSION(:)  :: niveis
   CHARACTER(len=15), ALLOCATABLE, DIMENSION(:)  :: varnames
   CHARACTER(len=9)                 :: dtype
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

         call i90_Release ( )
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

   SUBROUTINE get_vars( nfile, num ,vars, dtp, istat )
      !
      ! Carrega o nome das variaveis e o numero de 
      ! niveis de cada uma
      !
         IMPLICIT NONE

         CHARACTER(len=*),                INTENT(IN)   :: nfile
         INTEGER,                         INTENT(OUT)  :: num
         CHARACTER(len=15), DIMENSION(:), ALLOCATABLE, INTENT(OUT)  :: vars
         INTEGER, OPTIONAL,               INTENT(OUT)  :: istat

         CHARACTER(len=*),PARAMETER    :: myname_="get_vars"
         INTEGER                       :: I, iret
         REAL                          :: icor, incr
         CHARACTER(len=*)              :: dtp

         if(present(istat)) istat=0

         call i90_Release ( )
         call i90_LoadF ( TRIM(nfile), iret )   ! Load arquivo descritor

         if(iret /= 0) then
            call perr(myname_,'i90_LoadF("'//trim(nfile)//'")',iret)
            istat=iret
            return
         endif

         dtp = ""
         call i90_label ( 'dtype', iret)
         if(iret /= 0) call i90_label ( 'DTYPE', iret)
         if(iret == 0) then
            call i90_Gtoken(dtp, iret)
         endif

         call i90_label ( 'vars', iret )
         if(iret /= 0)  call i90_label ( 'VARS', iret )
         if(iret /= 0) then
            call perr(myname_,'i90_label("vars")',iret)
            istat=iret
            return
         endif

         num = i90_gint(iret)
         ALLOCATE(vars(num),stat=iret)
         if(iret /= 0) then
            call perr(myname_,'allocate(vars)',iret)
            return
         endif

         SELECT CASE (dtp)
            CASE ( "bufr"      )
               print*,'bufr not yet implemented'
               call exit
               istat = -1
            CASE ( "grib"      )
               DO I=1,num
                  call i90_gline ( iret )
                  call i90_Gtoken(vars(i), iret )
               ENDDO
            CASE ( "grib2"     )
               print*,''
               print*,'>> grib2 :: not yet implemented'
               call exit
               istat = -1
            CASE ( "hdfsds"    )
               print*,'hdfsds not yet implemented'
               call exit
               istat = -1
            CASE ( "hdf5_grid" )
               print*,'hdf5_grib not yet implemented'
               call exit
               istat = -1
            CASE ( "netcdf"    )
               print*,'netcdf not yet implemented'
               call exit
               istat = -1
            CASE ( "station"   )
               print*,'station not yet implemented'
               call exit
               istat = -1
            CASE DEFAULT
              DO I=1,num
                  call i90_gline ( iret )
                  call i90_Gtoken(vars(i), iret )
               ENDDO
         END SELECT
   
   END SUBROUTINE

END MODULE


PROGRAM lectl

   USE COMMOM
   USE m_die, only : perr

   CHARACTER(len=200)   :: fname
   INTEGER  :: iret, istat
   character(len=*),parameter  :: myname_='read_ctl'

   call getarg(1, fname)
!   fname='GPOSCPT20090101002009010218P.fct.T213L42.ctl'

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Lendo as definicoes de xdef, ydef, zdef
!
   CALL get_def(fname,'xdef', xdef_num, longitude, iret )
   if(iret /= 0) CALL get_def(fname,'XDEF', xdef_num, longitude, iret )
   if(iret /= 0) then
      call perr(myname_,'get_def("'//trim(fname)//',xdef,xdef_num,longitude")',iret)
      istat=iret
      call exit
   endif
   print*,xdef_num
   print*,longitude

   CALL get_def(fname,'ydef', ydef_num, latitude,iret )
   if(iret /= 0) CALL get_def(fname,'YDEF', ydef_num, latitude, iret )
   if(iret /= 0) then
      call perr(myname_,'get_def("'//trim(fname)//',ydef,ydef_num,latitude")',iret)
      istat=iret
      call exit
   endif
   print*,ydef_num
   print*,latitude

   CALL get_def(fname,'zdef', zdef_num, niveis,iret )
   if(iret /= 0) CALL get_def(fname,'ZDEF', zdef_num, niveis, iret )
   if(iret /= 0) then
      call perr(myname_,'get_def("'//trim(fname)//',zdef,zdef_num,levels")',iret)
      istat=iret
      call exit
   endif
   print*,zdef_num
   print*,niveis


!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Lendo as Variaveis
!

   CALL get_vars( fname, vars_num ,varnames, dtype, iret )
   if(iret /= 0) then
      call perr(myname_,'get_vars("'//trim(fname)//',vars_num, dtype, vars")',iret)
      istat=iret
      call exit
   endif
   print*,vars_num
   print*,varnames
END PROGRAM


