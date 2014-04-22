!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                  SCAMTeC - GDAD/CPTEC/INPE - 2010                   !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
! Copyright 2010 Free Software Foundation, Inc.                       !
!                                                                     !
! This program is free software; you can redistribute it and/or modify!
! it under the terms of the GNU General Public License as published by!
! the Free Software Foundation; either version 2 of the License, or   !
! (at your option) any later version.                                 !
!                                                                     !
! This program is distributed in the hope that it will be useful,     !
! but WITHOUT ANY WARRANTY; without even the implied warranty of      !
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       !
! GNU General Public License for more details.                        !
!                                                                     !
! You should have received a copy of the GNU General Public License   !
! along with GNU Emacs; see the file COPYING.  If not, write to the   !
! Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,    !
! Boston, MA 02110-1301, USA.                                         !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!BOP
!
MODULE SCAM_Utils
   ! Modules from INPAK90 lib
   USE scamtec_module
   USE m_inpak90
   USE m_die, only : perr
   !
   IMPLICIT NONE

   PRIVATE
!
! ROUTINES
!

   PUBLIC :: readcard
   PUBLIC :: banner

!
! KINDS
!

!   INTEGER, PUBLIC, PARAMETER :: I4B = SELECTED_INT_KIND(9)
!   INTEGER, PUBLIC, PARAMETER :: I2B = SELECTED_INT_KIND(4)
!   INTEGER, PUBLIC, PARAMETER :: I1B = SELECTED_INT_KIND(2)
!   INTEGER, PUBLIC, PARAMETER :: SP  = KIND(1.0)
!   INTEGER, PUBLIC, PARAMETER :: DP  = KIND(1.0D0)

!
! PARAMETERS
!

   INTEGER, PUBLIC, PARAMETER :: nvmx = 50   ! max number of variables
   INTEGER, PUBLIC, PARAMETER :: nlmx = 50   ! max number of levels
!
! CONSTANTS
!

	REAL(SP), PUBLIC, PARAMETER :: PI      = 3.141592653589793238462643383279502884197_sp
	REAL(SP), PUBLIC, PARAMETER :: PIO2    = 1.57079632679489661923132169163975144209858_sp
	REAL(SP), PUBLIC, PARAMETER :: TWOPI   = 6.283185307179586476925286766559005768394_sp
	REAL(SP), PUBLIC, PARAMETER :: SQRT2   = 1.41421356237309504880168872420969807856967_sp
	REAL(SP), PUBLIC, PARAMETER :: EULER   = 0.5772156649015328606065120900824024310422_sp
	REAL(DP), PUBLIC, PARAMETER :: PI_D    = 3.141592653589793238462643383279502884197_dp
	REAL(DP), PUBLIC, PARAMETER :: PIO2_D  = 1.57079632679489661923132169163975144209858_dp
	REAL(DP), PUBLIC, PARAMETER :: TWOPI_D = 6.283185307179586476925286766559005768394_dp

!
! DERIVADE TYPES
!

   TYPE, PUBLIC  :: domain
      REAL    :: ll_lat   !Lower Left Latitude
      REAL    :: ll_lon   !Lower Left Longitude
      REAL    :: ur_lat   !Upper Right Latitude
      REAL    :: ur_lon   !Upper Right Longitude
      REAL    :: dx
      REAL    :: dy
      INTEGER :: nx
      INTEGER :: ny
   END TYPE

   TYPE, PUBLIC  :: variable
      CHARACTER(len=15)          :: name
      INTEGER                    :: nlevs
      INTEGER, DIMENSION(nlmx)   :: levs
   END TYPE

   TYPE, PUBLIC  :: RUNS
      INTEGER                          :: Id
      CHARACTER(len=300)               :: name
      CHARACTER(len=300)               :: file
      TYPE(variable), DIMENSION(nvmx)  :: var
   END TYPE
!
! Global Variables
!
   INTEGER, PUBLIC   :: starting_time
   INTEGER, PUBLIC   :: ending_time
   INTEGER, PUBLIC   :: time_step
   INTEGER, PUBLIC   :: Forecast_time
   INTEGER, PUBLIC   :: ntime_forecast
   INTEGER, PUBLIC   :: ntime_steps
   REAL(DP), PUBLIC  :: incr
   INTEGER, PUBLIC   :: hist_time
   REAL(DP), PUBLIC  :: hist_incr

   TYPE(domain), PUBLIC, DIMENSION(:), ALLOCATABLE    :: dom


   TYPE(RUNS), PUBLIC                            :: Refer
   TYPE(RUNS), PUBLIC                            :: Clima
   INTEGER   , PUBLIC                            :: Clima_Flag
   TYPE(RUNS), PUBLIC, DIMENSION(:), ALLOCATABLE :: Exper
!   TYPE(RUNS), 


   CONTAINS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
! !ROUTINE: readcard
!  \label{readcard}
!
! !REVISION HISTORY:
!  Initial Code :: Joao Gerd - 
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
! !INTERFACE:
!
   SUBROUTINE readcard(istat)
      IMPLICIT NONE
      CHARACTER(len=*),PARAMETER    :: myname_="readcard"
      INTEGER                       :: I, J, iret, narg, ndom
      INTEGER, OPTIONAL,INTENT(OUT) :: istat
      CHARACTER(len=300)            :: config, formato
      LOGICAL                       :: exists
      INTEGER, ALLOCATABLE          :: tmp(:)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
! !DESCRIPTION:
!
!  The code in this file initializes the SCAMTec configuration management 
!  utility.
!  The generic model-independent runtime specifications of a SCAMTec
!  are read by this routine. The routine also initializes the SCAMTeC
!  log buffers, and calls some of the registries that set up the 
!  component plugin definitions.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
!EOP
#ifdef DEBUG
         WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
         WRITE(*,'(1x,a10,a8,a8,44x,a1)')'! INSIDE :', TRIM(myname_),' Routine','!'
         WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
#endif

         if(present(istat)) istat=0

!         call i90_Release ( )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Load descritor file

         narg=iargc()
         IF(narg.EQ.1)then
            call getarg(1,config)
         ELSE
            config='scamtec.conf'
!            INQUIRE(FILE=trim(config),EXIST=exists)
!            IF(.NOT. exists)THEN
!               iret = -1
!               call perr(myname_,'config file not found',iret)
!               if(present(istat))istat=iret
!               return
!            END IF
         END IF


         call i90_LoadF ( TRIM(config), iret )

         if(iret /= 0) then
            call perr(myname_,'i90_LoadF("'//trim(config)//'")',iret)
             if(present(istat))istat=iret
            return
         endif
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Reading time parameters
!
         call i90_label ( 'Starting Time:', iret )
         starting_time = i90_gint(iret)
         if(iret /= 0) then
            call perr(myname_,'i90_label("Starting Time:")',iret)
             if(present(istat))istat=iret
            return
         endif

         call i90_label ( 'Ending Time:', iret )
         ending_time = i90_gint(iret)
         if(iret /= 0) then
            call perr(myname_,'i90_label("Ending Time:")',iret)
             if(present(istat))istat=iret
            return
         endif

         call i90_label ( 'Time Step:', iret )
         time_step = i90_gint(iret)
         if(iret /= 0) then
            call perr(myname_,'i90_label("Time Step:")',iret)
             if(present(istat))istat=iret
            return
         endif

         call i90_label ( 'Forecast Time:', iret )
         forecast_time = i90_gint(iret)
         if(iret /= 0) then
            call perr(myname_,'i90_label("Forecast Time:")',iret)
             if(present(istat))istat=iret
            return
         endif
         call i90_label ( 'History Time:', iret )
         hist_time = i90_gint(iret)
         if(iret /= 0) then
            call perr(myname_,'i90_label("History Time:")',iret)
             if(present(istat))istat=iret
            return
         endif

!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Reading Domain Specifications

         call i90_label ( 'run domain number:', iret )
         ndom = i90_gint(iret)
         if(iret /= 0) then
            call perr(myname_,'i90_label("run domain number:")',iret)
            if(present(istat))istat=iret
            return
         endif

         ALLOCATE(dom(ndom),STAT=iret)
         if(iret/=0)then
            call perr(myname_,'allocate(dom('//int2str(ndom)//'))',iret)
            if(present(istat))istat=iret
            return
         endif

         call i90_label ( 'run domain lower left lat:', iret )
         DO I=1,ndom
            dom(I)%ll_lat = i90_gfloat(iret)
         ENDDO
         if(iret /= 0) then
            call perr(myname_,'i90_label("run domain lower left lat:")',iret)
            if(present(istat))istat=iret
            return
         endif
         call i90_label ( 'run domain lower left lon:', iret )
         DO I=1,ndom
         dom(I)%ll_lon = i90_gfloat(iret)
         ENDDO
         if(iret /= 0) then
            call perr(myname_,'i90_label("run domain lower left lon:")',iret)
            if(present(istat))istat=iret
            return
         endif
         call i90_label ( 'run domain upper right lat:', iret )
         DO I=1,ndom
            dom(I)%ur_lat = i90_gfloat(iret)
         ENDDO
         if(iret /= 0) then
            call perr(myname_,'i90_label("run domain upper right lat:")',iret)
            if(present(istat))istat=iret
            return
         endif
         call i90_label ( 'run domain upper right lon:', iret )
         DO I=1,ndom
            dom(I)%ur_lon = i90_gfloat(iret)
         ENDDO
         if(iret /= 0) then
            call perr(myname_,'i90_label("run domain upper right lon:")',iret)
            if(present(istat))istat=iret
            return
         endif
         call i90_label ( 'run domain resolution dx:', iret )
         DO I=1,ndom
            dom(I)%dx = i90_gfloat(iret)
         ENDDO
         if(iret /= 0) then
            call perr(myname_,'i90_label("run domain resolution dx:")',iret)
            if(present(istat))istat=iret
            return
         endif
         call i90_label ( 'run domain resolution dy:', iret )
         DO I=1,ndom
            dom(I)%dy = i90_gfloat(iret)
         ENDDO
         if(iret /= 0) then
            call perr(myname_,'i90_label("run domain resolution dy:")',iret)
            if(present(istat))istat=iret
            return
         endif

         DO I=1,ndom
            dom(I)%nx = ((dom(I)%ur_lon-dom(I)%ll_lon)/dom(I)%dx ) + 1
            dom(I)%ny = ((dom(I)%ur_lat-dom(I)%ll_lat)/dom(I)%dy ) + 1
         ENDDO
         
#ifdef DEBUG         
         DO I=1,ndom
            WRITE(*,'(A,I2.2,A)')'Grid ',I,' Specification'
            WRITE(*,'(A,F9.3)')'lower left latitude  :',dom(I)%ll_lat
            WRITE(*,'(A,F9.3)')'lower left longitude :',dom(I)%ll_lon
            WRITE(*,'(A,F9.3)')'upper right latitude :',dom(I)%ur_lat
            WRITE(*,'(A,F9.3)')'upper right longitude:',dom(I)%ur_lon
            WRITE(*,'(A,F9.3)')'resolution dx        :',dom(I)%dx
            WRITE(*,'(A,F9.3)')'resolution dy        :',dom(I)%dy
            WRITE(*,'(A,I9.3)')'number of points (X) :',dom(I)%nx
            WRITE(*,'(A,I9.3)')'number of points (Y) :',dom(I)%ny            
         ENDDO
#endif
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!Reading Files Names and Variables names to Analise 
!

!
! Reference File
!
         call i90_label ( 'Reference model:', iret )
         Refer%Id = i90_gint(iret)
         if(iret /= 0) then
            call perr(myname_,'i90_gint("Reference model:")',iret)
            if(present(istat))istat=iret
            return
         endif

         call i90_label ( 'Reference file:', iret )
         call i90_Gtoken(Refer%file,iret)
         if(iret /= 0) then
            call perr(myname_,'i90_label("Reference file:")',iret)
            if(present(istat))istat=iret
            return
         endif
         call i90_label ( 'Reference label:', iret )
         call i90_Gtoken(Refer%name,iret)
         if(iret /= 0) then
            call perr(myname_,'i90_label("Reference label:")',iret)
            if(present(istat))istat=iret
            return
         endif

#ifdef DEBUG
         WRITE(*,'(A)')'Reference'
         WRITE(*,'(A,I2.2)')'  |---- Id     :',Refer%Id
         WRITE(*,'(2A)')    '  |---- Name   :',TRIM(Refer%name)
         WRITE(*,'(2A)')    '  |---- File   :',TRIM(Refer%file)
#endif


!
! Experiments
!
         call i90_label ( 'Number of Experiments:', iret )
         scamtec%nexp=i90_gint(iret)
         if(iret /= 0) then
            call perr(myname_,'i90_label("Number of Experiments:")',iret)
            if(present(istat))istat=iret
            return
         endif

         ALLOCATE(Exper(scamtec%nexp),STAT=iret)

         call i90_label ( 'Experiments:', iret )
         if(iret /= 0) then
            call perr(myname_,'i90_label("Experiments:")',iret)
            if(present(istat))istat=iret
            return
         endif

         call i90_gline(iret)

         DO I=1,scamtec%nexp
            Exper(I)%Id = i90_gint(iret)
            if(iret /= 0) then
               call perr(myname_,'i90_gint("Experiment model:")',iret)
               if(present(istat))istat=iret
               return
            endif
            call i90_Gtoken(Exper(I)%name,iret)
            if(iret /= 0) then
               call perr(myname_,'i90_label("Experiment Name")',iret)
               if(present(istat))istat=iret
               return
            endif
            call i90_Gtoken(Exper(I)%file,iret)
            if(iret /= 0) then
               call perr(myname_,'i90_label("Experiment file_name_with_mask")',iret)
               if(present(istat))istat=iret
               return
            endif

#ifdef DEBUG
            WRITE(*,'(A)')'Experiment'
            WRITE(*,'(A,I2.2)')'  |---- Id     :',Exper(I)%Id
            WRITE(*,'(2A)')    '  |---- Name   :',TRIM(Exper(I)%name)
            WRITE(*,'(2A)')    '  |---- File   :',TRIM(Exper(I)%file)
#endif
            call i90_gline(iret)

         ENDDO


!
! Climatology
!
         call i90_label ( 'Use Climatology:', iret )
         if(iret == -2) then
            call perr(myname_,'Climarology Not Found')
            clima_flag = 0
!            if(present(istat))istat=iret
!            return
!         endif
         else
            Clima_Flag = i90_gint(iret)
            if(iret /= 0) then
               call perr(myname_,'i90_label("Use Climatology:")',iret)
               if(present(istat))istat=iret
               return
            endif
         endif

         IF(Clima_Flag.EQ.1)THEN
            Clima%name='Climatology'
            call i90_label ( 'Climatology Model Id:', iret )
            Clima%Id = i90_gint(iret)
            if(iret /= 0) then
               call perr(myname_,'i90_gint("Climatology Model Id:")',iret)
               if(present(istat))istat=iret
               return
            endif

            call i90_label ( 'Climatology file:', iret )
            call i90_Gtoken(Clima%file,iret)
            if(iret /= 0) then
               call perr(myname_,'i90_label("Climatology file:")',iret)
               if(present(istat))istat=iret
               return
            endif
            
#ifdef DEBUG
            WRITE(*,'(A)')'Climatology'
            WRITE(*,'(A,I2.2)')'  |---- Id     :',Clima%Id
            WRITE(*,'(2A)')    '  |---- Dir    :',TRIM(Clima%name)
            WRITE(*,'(2A)')    '  |---- File   :',TRIM(Clima%file)
#endif
         ELSE


            WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
            WRITE(*,'(a72)')'!                         Climatology Not Found                       !'
            WRITE(*,'(a72)')'!         The mean reference field will be used as climatology        !'
            WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
         ENDIF
!
! ïndices dos modelos
!
         IF(Clima_Flag.eq.1)then

            Allocate(tmp(scamtec%nexp+1))
            call unique((/Exper(:)%Id,Clima%Id/),tmp,I)

            Allocate(scamtec%Init_ModelID(I))
            scamtec%Init_ModelID(1:I) = tmp(1:I)

            DeAllocate(tmp)

         else
            Allocate(tmp(scamtec%nexp))
            call unique(Exper(:)%Id,tmp,I)

            Allocate(scamtec%Init_ModelID(I))
            scamtec%Init_ModelID(1:I) = tmp(1:I)

            DeAllocate(tmp)
         endif



   END SUBROUTINE

   SUBROUTINE BANNER()
      IMPLICIT NONE
      WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
      WRITE(*,'(a72)')'!                  SCAMTeC - GDAD/CPTEC/INPE - 2010                   !'
      WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
      WRITE(*,'(a72)')'! Copyright 2010 Free Software Foundation, Inc.                       !'
      WRITE(*,'(a72)')'!                                                                     !'
      WRITE(*,'(a72)')'! This program is free software; you can redistribute it and/or modify!'
      WRITE(*,'(a72)')'! it under the terms of the GNU General Public License as published by!'
      WRITE(*,'(a72)')'! the Free Software Foundation; either version 2 of the License, or   !'
      WRITE(*,'(a72)')'! (at your option) any later version.                                 !'
      WRITE(*,'(a72)')'!                                                                     !'
      WRITE(*,'(a72)')'! This program is distributed in the hope that it will be useful,     !'
      WRITE(*,'(a72)')'! but WITHOUT ANY WARRANTY; without even the implied warranty of      !'
      WRITE(*,'(a72)')'! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       !'
      WRITE(*,'(a72)')'! GNU General Public License for more details.                        !'
      WRITE(*,'(a72)')'!                                                                     !'
      WRITE(*,'(a72)')'! You should have received a copy of the GNU General Public License   !'
      WRITE(*,'(a72)')'! along with GNU Emacs; see the file COPYING.  If not, write to the   !'
      WRITE(*,'(a72)')'! Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,    !'
      WRITE(*,'(a72)')'! Boston, MA 02110-1301, USA.                                         !'
      WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
#ifdef DEBUG
      WRITE(*,'(a72)')'                                                                       '
      WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
      WRITE(*,'(a72)')'!   ATTENTION :: RUNNING IN DEBUG MODE, THE PROGRAM CAN BECOME SLOW   !'
      WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
      WRITE(*,'(a72)')'                                                                       '
#endif

   END SUBROUTINE

   character(20) function int2str(num) 
      integer, intent(in):: num
      character(20) :: str
      ! convert integer to string using formatted write
      write(str, '(i20)') num
      int2str = adjustl(str)
   end function int2str

   subroutine unique(input,output,nelem)
     
     integer, intent(in)  :: input(:)   ! The input
     integer, intent(out) :: output(:)  ! The output
     integer, intent(out) :: nelem      ! The number of unique elements
     integer :: i, j
 
  nelem = 1
  output(1) = input(1)
  outer: do i=2,size(input)
     do j=1,nelem
        if (output(j) == input(i)) then
           ! Found a match so start looking again
           cycle outer
        end if
     end do
     ! No match found so add it to the output
     nelem = nelem + 1
     output(nelem) = input(i)
  end do outer

  end subroutine

END MODULE
