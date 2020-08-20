!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                  scantec - GDAD/CPTEC/INPE - 2010                   !
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
MODULE scan_Utils
   USE scantec_module
   ! Modules from INPAK90 lib
   USE m_inpak90, only: i90_LoadF, &
                        i90_getVal,&
                        i90_gint,  &
                        i90_gfloat,&
                        i90_gtoken,&
                        i90_gline, &
                        i90_label, &
                        i90_perr,  &
                        i90_die,   &
                        i90_lcase, &
                        i90_fullRelease
   use m_constants, only: i4, r4
   
   !
   IMPLICIT NONE

   PRIVATE
!
! ROUTINES
!

   PUBLIC :: readcard
   PUBLIC :: banner

!
! ISTES DERIVED TYPES DEVEM PASSAR PARA O modulo >>> scantec_module.f90 <<<
!


!
! DERIVED TYPES
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

   TYPE, PUBLIC  :: RUNS
      CHARACTER(len=300)               :: Id
      CHARACTER(len=300)               :: name
      CHARACTER(len=300)               :: file
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
   REAL(r8), PUBLIC  :: incr
   INTEGER, PUBLIC   :: hist_time
   REAL(r8), PUBLIC  :: hist_incr

   TYPE(domain), PUBLIC, DIMENSION(:), ALLOCATABLE    :: dom


   TYPE(RUNS), PUBLIC                            :: Refer
   TYPE(RUNS), PUBLIC                            :: Clima
   TYPE(RUNS), PUBLIC                            :: Precip             !paulo dias
   INTEGER   , PUBLIC                            :: Precipitation_Flag !paulo dias
   TYPE(RUNS), PUBLIC, DIMENSION(:), ALLOCATABLE :: Exper
!   TYPE(RUNS), 

   !
   !Variaveis EOFs
   !
   INTEGER   , PUBLIC                            :: EOFs_Flag  !paulo dias
   INTEGER   , PUBLIC                            :: quant_EOFs !paulo dias

!---------------------------------------------------------------------paulo dias
! Variaveis de Preciptation Histograma  paulo dias
!
   TYPE, PUBLIC     :: param_hist                  
     REAL, PUBLIC     :: valor_limit, valor_min   !valor maximo e valor minimo 
     REAL, PUBLIC     :: rang                     !valor do range
     INTEGER, PUBLIC  :: tipo_precip              !tipo de precipitacao
     INTEGER, PUBLIC  :: acumulo_obs              !acumulo de precipitacao da observacao
     INTEGER, PUBLIC  :: acumulo_exp              !acumulo de precipitacao do experimento
   END TYPE
   
   TYPE(param_hist), public   :: hist   
!-------------------------------------------------------------------- paulo dias

!
! INTERFACES
!


   CONTAINS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
! !ROUTINE: readcard
!  \label{readcard}
!
! !REVISION HISTORY:
!  ## ### ####   J. G. de Mattos - Initial Code
!  22 Oct 2012 - R. Mello        - Correcao na aquisição dos indices 
!                                  dos modelos.
!  06 Jan 2016 - J. G. de Mattos - Colocando linked list para obter 
!                                  informacoes dos experimentos.
!                                  Nao e mais necessario informar o
!                                  numeros de experimentos que deverao 
!                                  ser avaliados, isto evita erros de 
!                                  esquecimento.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
! !INTERFACE:
!
   SUBROUTINE readcard(istat)
      IMPLICIT NONE
      INTEGER, OPTIONAL,INTENT(OUT) :: istat
      INTEGER                       :: I, J, narg, ndom
      CHARACTER(len=300)            :: config, formato
      LOGICAL                       :: exists
      INTEGER, ALLOCATABLE          :: tmp(:)
#ifndef gfortran
      INTEGER, EXTERNAL             :: iargc
#endif

      CHARACTER(len=*),PARAMETER    :: myname_="readcard"
      integer :: epcount
      integer :: iret, ierr
      character(len=256) :: ModelName
      character(len=256) :: FileName
      character(len=256) :: ExpName
      character(len=256) :: timeStepType


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
! !DESCRIPTION:
!
!  The code in this file initializes the scantec configuration management 
!  utility.
!  The generic model-independent runtime specifications of a scantec
!  are read by this routine. The routine also initializes the scantec
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
            config='scantec.conf'
!            INQUIRE(FILE=trim(config),EXIST=exists)
!            IF(.NOT. exists)THEN
!               iret = -1
!               call i90_perr(myname_,'config file not found',iret)
!               if(present(istat))istat=iret
!               return
!            END IF
         END IF


         call i90_LoadF ( TRIM(config), iret )

         if(iret /= 0) then
            call i90_perr(myname_,'i90_LoadF("'//trim(config)//'")',iret)
             if(present(istat))istat=iret
            return
         endif
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Reading time parameters
!
         scantec%starting_time = huge(1)
         call i90_getVal ( 'Starting Time:',      scantec%starting_time, iret )

         scantec%ending_time = huge(1)
         call i90_getVal ( 'Ending Time:',        scantec%ending_time,   iret )

         call i90_getVal ( 'Analisys Time Step:', scantec%atime_step,    iret )
         call i90_getVal ( 'Forecast Time Step:', scantec%ftime_step,    iret )
         call i90_getVal ( 'Forecast Total Time:',scantec%forecast_time, iret )
         call i90_getVal ( 'History Time:',       scantec%hist_time,     iret )
         !call i90_getVal ( 'Undefined Value:',    scantec%udef,          iret )

         call i90_getVal ('scantec tables:', scantec%tables, iret, default='../tables')
         call i90_getVal ('Time Step Type:', TimeStepType, iret, default = 'forward')

         timeStepType = i90_lcase(TimeStepType)

         if(trim(timeStepType) .eq. 'forward' .or. &
            trim(timeStepType) .eq. 'backward')then

            scantec%TimeStepType = i90_lcase(TimeStepType)
            
          else
            call i90_perr(myname_,'wrong time step type: '//trim(timeStepType))
            call i90_perr(myname_,'setting default type: forward')

            scantec%TimeStepType = 'forward'
         endif
!
! Apply Sanity Checks on time specifications!
!

! Starting time is validy?

         if (scantec%starting_time .eq. huge(1) )then

            iret = 99
            call i90_perr(myname_,'Error to specify Starting Time :',iret)
            if(present(istat))istat=iret
            return

         endif

! Ending time is validy?

         if (scantec%Ending_time .eq. huge(1) )then

            iret = 99
            call i90_perr(myname_,'Error to specify Ending Time :',iret)
            if(present(istat))istat=iret
            return

         endif


! Ending time >= Starting time?


#ifdef DEBUG
        write(*,'(   1A   )')'Running Specification:'
        write(*,'(A,x,I10.10)')'Starting Time:',     scantec%starting_time
        write(*,'(A,x,I10.10)')'Ending Time:',       scantec%ending_time
        write(*,'(A,x,I3.2)')'Time Step:',           scantec%time_step 
        write(*,'(A,x,   A)')'Time Step Type:',      scantec%TimeStepType
        write(*,'(A,x,I3.2)')'Analisys Time Step:',  scantec%atime_step
        write(*,'(A,x,I3.2)')'Forecast Time Step:',  scantec%ftime_step
        write(*,'(A,x,I3.2)')'Forecast Total Time:', scantec%forecast_time
        write(*,'(A,x,I3.2)')'History Time:',        scantec%hist_time
#endif


!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Reading Domain Specifications

         call i90_getVal ( 'run domain number:', ndom, iret )

         ALLOCATE(dom(ndom),STAT=iret)
         if(iret/=0)then
            call i90_perr(myname_,'allocate(dom('//int2str(ndom)//'))',iret)
            if(present(istat))istat=iret
            return
         endif

         call i90_getVal ( 'run domain lower left lat:', dom(:)%ll_lat, iret )
         call i90_getVal ( 'run domain lower left lon:', dom(:)%ll_lon, iret )
         call i90_getVal ( 'run domain upper right lat:',dom(:)%ur_lat, iret )
         call i90_getVal ( 'run domain upper right lon:',dom(:)%ur_lon, iret )
         call i90_getVal ( 'run domain resolution dx:',  dom(:)%dx,     iret )
         call i90_getVal ( 'run domain resolution dy:',  dom(:)%dy,     iret )

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
         call i90_getVal( 'Reference Model Name:', ModelName, ierr ); iret = ierr
         call i90_getVal( 'Reference file:'      ,  FileName, ierr ); iret = iret + ierr
         if(iret .eq. 0)then
            call scantec%insertModel('Reference',trim(ModelName), 'refer', trim(FileName))
         else
            call i90_perr(trim(myname_),'You must configure a reference file', -1)
         endif


!
! Experiments
!

         call i90_label ( 'Experiments:', iret )
         if(iret /= 0) then
            call i90_perr(trim(myname_),'i90_label("Experiments:")',iret)
            if(.not.present(istat)) call i90_die(trim(myname_))
            istat=iret
            return
         endif

         ! get experiment info, one by line of config file
         call i90_gline(iret)
         if(iret .ne. 0)then
            call i90_perr(trim(myname_), 'You must configure at least one experiment !',-1)
            call i90_die(myname_)
         endif

         do while (iret .eq. 0)

            !--------------------------------------------------------------------
            ! Used Model
            call i90_Gtoken(ModelName,iret)
            if(iret /= 0) then
               call i90_perr(myname_,'i90_gint("Experiment Model Name:")',iret)
               if(.not.present(istat)) call i90_die(trim(myname_))
               istat=iret
               return
            endif

            ! Experiment Name
            call i90_Gtoken(ExpName,iret)
            if(iret /= 0) then
               call i90_perr(trim(myname_),'i90_label("Experiment Name")',iret)
               if(present(istat))call i90_die(trim(myname_))
               istat=iret
               return
            endif

            ! Experiment File
            call i90_Gtoken(FileName,iret)
            if(iret /= 0) then
               call i90_perr(trim(myname_),'i90_label("Experiment file_name_with_mask")',iret)
               if(present(istat))call i90_die(trim(myname_))
               istat=iret
               return
            endif

            !
            !Insert model at scantec structure
            !
            call scantec%insertModel('Experiment',    &
                                     trim(ModelName), &
                                     trim(ExpName),   &
                                     trim(FileName)   &
                                     )

            ! get next line
            !    - iret =  0, next line ok
            !    - iret = -1, end of buffer (some problem with table)
            !    - iret = +1, end of table
            call i90_gline(iret)
            if(iret.lt.0)then
               call i90_perr(trim(myname_),'getting experiment table.', iret)
               call i90_die(trim(myname_))
            endif

         enddo

!
! Climatology
!
         call i90_label ( 'Use Climatology:', iret )
         if(iret == -2) then
            call i90_perr(myname_,'Climarology Not Found')
            scantec%cflag = 0
         else
            scantec%cflag = i90_gint(iret)
            if(iret /= 0) then
               call i90_perr(trim(myname_),'i90_label("Use Climatology:")',iret)
               if(.not.present(istat))call i90_die(trim(myname_))
               istat=iret
               return
            endif
         endif

         IF(scantec%cflag.EQ.1)THEN

            call i90_label ( 'Climatology Model Name:', iret )
            call i90_Gtoken(ModelName, iret)
            if(iret /= 0) then
               call i90_perr(trim(myname_),'i90_gint("Climatology Model Name:")',iret)
               if(present(istat))call i90_die(trim(myname_))
               istat=iret
               return
            endif

            call i90_label ( 'Climatology file:', iret )
            call i90_Gtoken(FileName,iret)
            if(iret /= 0) then
               call i90_perr(trim(myname_),'i90_label("Climatology file:")',iret)
               if(.not.present(istat))call i90_die(trim(myname_))
               istat=iret
               return
            endif
            
            call scantec%insertModel('Climatology',   &
                                     trim(ModelName), &
                                     'clima',         &
                                     trim(FileName)   &
                                    )
         ELSE


            WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
            WRITE(*,'(a72)')'!                         Climatology Not Found                       !'
            WRITE(*,'(a72)')'!         The mean reference field will be used as climatology        !'
            WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
         ENDIF

         !-----------------------------------------------------------------------------------!
         ! Mantem a estrutura antiga, assim não preciso mexer em muitas coisas
         ! neste momento
         ALLOCATE(Exper(scantec%nexp),STAT=iret)
         scantec%currModel => scantec%FirstModel ! return to begin of linked list 
         I = 0
         DO while(associated(scantec%currModel))
            if (trim(scantec%currModel%Type_) .eq. 'Reference')then
               Refer%Id   = trim(scantec%currModel%Name_)
               Refer%file = trim(scantec%currModel%FileName_)
               Refer%name = trim(scantec%currModel%ExpName_)

            else if (trim(scantec%currModel%Type_) .eq. 'Climatology')then
               clima%Id   = trim(ModelName)
               clima%name = 'Climatology'
               clima%file = trim(FileName)

            else if (trim(scantec%currModel%Type_) .eq. 'Experiment')then
               
               i = i + 1
               Exper(I)%Id   = trim(scantec%currModel%Name_)
               Exper(I)%name = trim(scantec%currModel%ExpName_)
               Exper(I)%file = trim(scantec%currModel%FileName_)

            endif 

#ifdef DEBUG
               WRITE(*,'(2A)')'Type : ', trim(scantec%currModel%Type_)
               WRITE(*,'(2A)')'  |---- Model Name :',trim(scantec%currModel%Name_)
               WRITE(*,'(2A)')'  |---- Exp Name   :',trim(scantec%currModel%ExpName_)
               WRITE(*,'(2A)')'  |---- File       :',trim(scantec%currModel%FileName_)
#endif
            scantec%currModel => scantec%currModel%next
         ENDDO

         
!-----------------------------------------------------------------------------------------------------------Paulo Dias

!
! Precipitation
!
!    !print*, '::: Lendo arquivos de Precipitacao :::' 
!         call i90_label ( 'Use Precipitation:', iret )
!         if(iret == -2) then
!            call i90_perr(myname_,'Preciptarion Not Found')
!            Precipitation_Flag = 0
!!            if(present(istat))istat=iret
!!            return
!!         endif
!         else
!            Precipitation_Flag = i90_gint(iret)
!            if(iret /= 0) then
!               call i90_perr(myname_,'i90_label("Use Precipitation:")',iret)
!               if(present(istat))istat=iret
!               return
!            endif
!         endif
!
!         IF(Precipitation_Flag .EQ. 1)THEN
!            Precip%name='Precipitation'
!            call i90_label ( 'Precipitation Model Id:', iret )
!            Precip%Id = i90_gint(iret)
!            if(iret /= 0) then
!               call i90_perr(myname_,'i90_gint("Precipitation Model Id:")',iret)
!               if(present(istat))istat=iret
!               return
!            endif
!            
!            call i90_label ( 'Precipitation file:', iret )
!            call i90_Gtoken(Precip%file,iret)
!            if(iret /= 0) then
!               call i90_perr(myname_,'i90_label("Precipitation file:")',iret)
!               if(present(istat))istat=iret
!               return
!            endif
!            
!            call i90_label ( 'Define o Range do Histograma:', iret )
!            hist%rang = i90_gfloat(iret)
!            if(iret /= 0) then
!               call i90_perr(myname_,'i90_gint("Define o Range do Histograma:")',iret)
!               if(present(istat))istat=iret
!               return
!            endif
!            
!            call i90_label ( 'Define valor do limite da ultima classe do histograma:', iret )
!            hist%valor_limit = i90_gfloat(iret)
!            if(iret /= 0) then
!               call i90_perr(myname_,'i90_gint("Define valor do limite da ultima classe do histograma:")',iret)
!               if(present(istat))istat=iret
!               return
!            endif            
!            
!            call i90_label ( 'Define valor do minimo inferior da primeira classe do histograma:', iret )
!            hist%valor_min = i90_gfloat(iret)
!            if(iret /= 0) then
!               call i90_perr(myname_,'i90_gint("Define valor do minimo inferior da primeira classe do histograma:")',iret)
!               if(present(istat))istat=iret
!               return
!            endif
!            
!            call i90_label ( 'Define qual Precipitacao deseja avaliar:', iret )
!            hist%tipo_precip = i90_gint(iret)
!            if(iret /= 0) then
!               call i90_perr(myname_,'i90_gint("Define qual Precipitacao deseja avaliar:")',iret)
!               if(present(istat))istat=iret
!               return
!            endif
!            
!            call i90_label ( 'Define o periodo de acumulo de precpitacao da observacao:', iret )
!            hist%acumulo_obs = i90_gint(iret)
!            if(iret /= 0) then
!               call i90_perr(myname_,'i90_gint("Define o periodo de acumulo de precpitacao da observacao:")',iret)
!               if(present(istat))istat=iret
!               return
!            endif
!            
!            call i90_label ( 'Define o periodo de acumulo de precpitacao do experimento:', iret )
!            hist%acumulo_exp = i90_gint(iret)
!            if(iret /= 0) then
!               call i90_perr(myname_,'i90_gint("Define o periodo de acumulo de precpitacao do experimento:")',iret)
!               if(present(istat))istat=iret
!               return
!            endif
!            
!            
!#ifdef DEBUG
!            WRITE(*,'(A)')'Precipitation'
!            WRITE(*,'(A,I2.2)')'  |---- Id                 :',Precip%Id
!            WRITE(*,'(2A)')    '  |---- Dir                :',TRIM(Precip%name)
!            WRITE(*,'(2A)')    '  |---- File               :',TRIM(Precip%file)
!            WRITE(*,'(A,F4.2)')'  |---- Range              :',hist%rang
!            WRITE(*,'(A,F6.2)')'  |---- Valor Minimo       :',hist%valor_min
!            WRITE(*,'(A,F8.2)')'  |---- Valor Limite       :',hist%valor_limit
!            IF(hist%tipo_precip==21)THEN
!               WRITE(*,'(A)')'  |---- Precipitacao       : TOTAL'
!            ELSE
!               WRITE(*,'(A)')'  |---- Precipitacao       : CONVECTIVE'
!            ENDIF
!            WRITE(*,'(A,I2.2)')'  |---- Acumulo OBS        :',hist%acumulo_obs
!            WRITE(*,'(A,I2.2)')'  |---- Acumulo EXP        :',hist%acumulo_exp
!#endif
!          ELSE
!
!#ifdef DEBUG
!            WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
!            WRITE(*,'(a72)')'!                       Precipitation Not Found                       !'
!            WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
!#endif
!
!         ENDIF

!-----------------------------------------------------------------------------------------------------------Paulo Dias
   
   
!-----------------------------------------------------------------------------------------------------------Paulo Dias
! EOFs
!
!            call i90_label ( 'Use EOFs:', iret )
!            EOFs_Flag = i90_gint(iret)
!            if(iret /= 0) then
!               call i90_perr(myname_,'i90_gint("Use EOFs:")',iret)
!               if(present(istat))istat=iret
!               return
!            endif
!            
!            call i90_label ( 'Define a quantidade de EOFs:', iret )
!            quant_EOFs = i90_gint(iret)
!            if(iret /= 0) then
!               call i90_perr(myname_,'i90_gint("Define a quantidade de EOFs:")',iret)
!               if(present(istat))istat=iret
!               return
!            endif           
!            
!#ifdef DEBUG
!            WRITE(*,'(A)')'Empirical Orthogonal Functions (EOF)'
!            WRITE(*,'(A,I2.2)')'  |---- Quantidade de EOF:',quant_EOFs
!            
!#endif
!
!
!!-----------------------------------------------------------------------------------------------------------Paulo Dias
!    
!         
!!
!! indices dos modelos
!!
!
!         if(scantec%cflag.eq.1 .and. Precipitation_Flag.eq.0) then
!         
!               Allocate(tmp(scantec%nexp+2))
!               call unique((/Refer%id, Exper(:)%Id, Clima%Id/),tmp,I)
!
!               Allocate(scantec%Init_ModelID(I))
!               scantec%Init_ModelID(1:I) = tmp(1:I)
!
!               DeAllocate(tmp)
!
!         else if (scantec%cflag.eq.1 .and. Precipitation_Flag.eq.1) then
!              
!               Allocate(tmp(scantec%nexp+3))
!               call unique((/Refer%id, Exper(:)%Id, Clima%Id, Precip%Id/),tmp,I)
!
!               Allocate(scantec%Init_ModelID(I))
!               scantec%Init_ModelID(1:I) = tmp(1:I)
!
!               DeAllocate(tmp)
!
!
!         else
!
!            
!               Allocate(tmp(scantec%nexp+1))
!               call unique((/Refer%id, Exper(:)%Id/),tmp,I)
!
!               Allocate(scantec%Init_ModelID(I))
!               scantec%Init_ModelID(1:I) = tmp(1:I)
!
!               DeAllocate(tmp)
!         end if
!
!
!------------------------------------------------------------------------------- !Paulo Dias
! Diretorio de Saida
!   
        call i90_label ( 'Output directory:', iret )
            call i90_Gtoken(scantec%output_dir,iret)
            if(iret /= 0) then
               call i90_perr(myname_,'i90_label("Output directory:")',iret)
               if(present(istat))istat=iret
               return
            endif

!Fim Diretorio de Saida
!--------

   call I90_fullRelease( iret )
   if(iret /= 0) then
      call i90_perr(myname_,'i90_fullRelease',iret)
      if(present(istat))istat=iret
      return
   endif


   END SUBROUTINE

   SUBROUTINE BANNER()
      IMPLICIT NONE
      WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
      WRITE(*,'(a72)')'!                  scantec - GDAD/CPTEC/INPE - 2010                   !'
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
