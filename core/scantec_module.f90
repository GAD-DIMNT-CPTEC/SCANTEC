!-----------------------------------------------------------------------------!
!             Modeling and Development Division - DMD/CPTEC/INPE              !
!-----------------------------------------------------------------------------!
!BOP
! !MODULE: scantec_module - main module to define the scantec structure
! 
!
!
! !DESCRIPTION: This module constains the main structure for scantec.
!               Here are defined all class and methods used by scantec
!               to access basic characteritics of models and evaluation
!               metrics.
!
! !INTERFACE:
!

MODULE scantec_module
  use scan_Modelplugin, only: scanModel
  use m_constants
  use m_ioutil
  use time_module, only: jul2cal, cal2jul ! time operations
  use m_inpak90,   only: i90_LoadF, &
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


  implicit none
  private


  !
  ! scantec data type
  !

  type, extends(scanModel) :: scanType

     !
     ! General Variables
     !

     integer(kind = i4) :: starting_time      ! Initial time
     integer(kind = i4) :: ending_time        ! End time
     integer(kind = i4) :: time_step          ! Atual time step in hours
     integer(kind = i4) :: loop_count         !
     !character(len=normalStr) :: tables       ! directory of config tables
     character(len=tinyStr)   :: TimeStepType ! forward or backward

     !
     ! Analisys Variables
     !

     integer(kind = i4) :: atime              ! Analisys time
     integer(kind = i4) :: atime_step         ! Analisys time step
     integer(kind = i4) :: ntime_steps        ! total # of Analisys time step
     real(kind = r8)    :: aincr              ! Analisys time increment
     logical            :: atime_flag         ! is it a new date reference (not forecast)?

     !
     ! Forecast Variables
     !

     integer(kind = i4) :: ftime              ! forecast time
     integer(kind = i4) :: ftime_step         ! Forecast time step 
     integer(kind = i4) :: ntime_forecast     ! total # of forecast time step
     real(kind = r8)    :: fincr              ! Forecast time increment
     integer(kind = i4) :: Forecast_time      ! Forecast total time
     integer(kind = i4) :: ftime_idx          ! forecast time idx count
     integer(kind = i4), Allocatable :: ftime_count(:)  
     CHARACTER(len=LongStr), PUBLIC :: output_dir !

     !
     ! History Variables
     !

     integer(kind = i4) :: hist_time
     real(kind = r8)    :: hist_incr          ! history time increment

     !
     ! SCANTEC variables
     !

     integer(kind = i4)                   :: nvar
     character(len=tinyStr), allocatable  :: VarName(:)
     character(len=shortStr), allocatable :: VarDesc(:)

     !
     !    flag to use/no use climatology
     !

     integer(kind = i4) :: cflag

     !
     ! SCANTEC Grid Info
     !

     real(kind = r4)    :: gridDesc(50)
     real(kind = r4)    :: udef
     integer(kind = i4) :: nxpt
     integer(kind = i4) :: nypt
     integer(kind = i4) :: npts


     !
     ! routines
     !
   contains
      procedure :: configure
  end type scanType

  type(scanType), public :: scantec

  !------------------------------------------------------------------
  ! para manter a consistencia com as versoes antigas e não 
  ! modificar todo o scantec ainda
  !
  type, public  :: runs
     character(len=300)               :: id
     character(len=300)               :: name
     character(len=300)               :: file
  end type runs

  type(runs), public  :: Refer
  type(runs), public  :: Clima
  type(runs), allocatable, public  :: Exper(:)
  !------------------------------------------------------------------
  character(len=*), parameter :: myname='scantec_module'


contains

  SUBROUTINE Configure(self,istat)
   class(scanType), intent(inout) :: self

    integer, optional,intent(out) :: istat

    integer :: i, iret, ierr
    integer :: narg
    character(len=NormalStr) :: scanVarsConf
    type vars
       character(len=8)        :: vname
       character(len=shortStr) :: vdesc
       type(vars), pointer :: next => null()
    end type vars
    integer :: nvars
    type(vars), pointer :: first => null()
    type(vars), pointer :: curr => null()

    logical :: found
    character(len=smallStr) :: msg
    character(len=NormalStr):: config
    character(len=shortSTr) :: ModelName
    character(len=shortSTr) :: FileName
    character(len=shortSTr) :: ExpName
    character(len=shortStr) :: timeStepType


    !-------------------------------------------------------------------!

    character(len=*), parameter :: myname_=myname//' :: Configure( )'

    !-------------------------------------------------------------------!
    ! Load descritor file

    narg=iargc()
    IF(narg.EQ.1)then
       call getarg(1,config)
    ELSE
       config='scantec.conf'
    END IF

    call i90_LoadF ( TRIM(config), iret )

    if(iret /= 0) then
       call i90_perr(myname_,'i90_LoadF("'//trim(config)//'")',iret)
       if(present(istat))istat=iret
       !return
       stop 99038
    endif

    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Reading time parameters
    !
    self%starting_time = huge(1)
    call i90_getVal ( 'Starting Time:',      self%starting_time, iret )

    self%ending_time = huge(1)
    call i90_getVal ( 'Ending Time:',        self%ending_time,   iret )

    call i90_getVal ( 'Analisys Time Step:', self%atime_step,    iret )
    call i90_getVal ( 'Forecast Time Step:', self%ftime_step,    iret )
    call i90_getVal ( 'Forecast Total Time:',self%forecast_time, iret )
    call i90_getVal ( 'History Time:',       self%hist_time,     iret )

    call i90_getVal ('scantec tables:', self%tables, iret, default='../tables')
    call i90_getVal ('Time Step Type:', TimeStepType, iret, default = 'forward')

    timeStepType = i90_lcase(TimeStepType)

    if(trim(timeStepType) .eq. 'forward' .or. &
         trim(timeStepType) .eq. 'backward')then

       self%TimeStepType = i90_lcase(TimeStepType)

    else
       call i90_perr(myname_,'wrong time step type: '//trim(timeStepType))
       call i90_perr(myname_,'setting default type: forward')

       self%TimeStepType = 'forward'
    endif
    !
    ! Apply Sanity Checks on time specifications!
    !

    ! Starting time is validy?

    if (self%starting_time .eq. huge(1) )then

       iret = 99
       call i90_perr(myname_,'Error to specify Starting Time :',iret)
       if(present(istat))istat=iret
       return

    endif

    ! Ending time is validy?

    if (self%Ending_time .eq. huge(1) )then

       iret = 99
       call i90_perr(myname_,'Error to specify Ending Time :',iret)
       if(present(istat))istat=iret
       return

    endif


    ! Ending time >= Starting time?


#ifdef DEBUG
    write(*,'(   1A   )')'Running Specification:'
    write(*,'(A,x,I10.10)')'Starting Time:',     self%starting_time
    write(*,'(A,x,I10.10)')'Ending Time:',       self%ending_time
    write(*,'(A,x,I3.2)')'Time Step:',           self%time_step 
    write(*,'(A,x,   A)')'Time Step Type:',      self%TimeStepType
    write(*,'(A,x,I3.2)')'Analisys Time Step:',  self%atime_step
    write(*,'(A,x,I3.2)')'Forecast Time Step:',  self%ftime_step
    write(*,'(A,x,I3.2)')'Forecast Total Time:', self%forecast_time
    write(*,'(A,x,I3.2)')'History Time:',        self%hist_time
#endif

    !
    !  Time configuration
    !

    self%time_step      = 1
    self%ftime_idx      = 1
    self%loop_count     = 1
    self%atime_flag     = .true.

    ! Sanity Check
    if (self%atime_step .gt. self%ftime_step)then
       write(stdout,*)'ERROR: Analisys Time Step should be less or equal to Forecast Time Step'
       stop 99000
    endif

    self%hist_incr      = real(self%hist_time/24.0d0)
    self%aincr          = real(self%atime_step/24.0d0)
    self%fincr          = real(self%ftime_step/24.0d0)


    self%ntime_steps    = ( ( cal2jul(self%ending_time) - &
         cal2jul(self%starting_time) +  &
         self%aincr ) / self%aincr )

    self%ntime_forecast = ( self%Forecast_time / self%ftime_step ) + 1


    self%atime          = self%starting_time
    self%ftime          = self%starting_time

    Allocate(self%ftime_count(self%ntime_forecast))
    self%ftime_count    = 0
    self%ftime_count(1) = 1

#ifdef DEBUG    
    write(6,'(A,F9.3)')'history increment    :',self%hist_incr
    write(6,'(A,F9.3)')'Analisys increment   :',self%aincr
    write(6,'(A,F9.3)')'Forecast increment   :',self%aincr           
    write(6,'(A,I9.3)')'N time steps         :',self%ntime_steps
    write(6,'(A,I9.3)')'N forecast time steps :',self%ntime_forecast
#endif

    !
    !-------------------------------------------------------------------!
    ! Reading Domain Specifications

    self%gridDesc = 0

    self%gridDesc( 1) = 0
    self%gridDesc( 6) = 128
    self%gridDesc(20) = 0

    call i90_getVal ( 'run domain lower left lat:', self%gridDesc( 4), iret ) ! First latitude point (South point)
    call i90_getVal ( 'run domain lower left lon:', self%gridDesc( 5), iret ) ! First longitude point (West point)
    call i90_getVal ( 'run domain upper right lat:',self%gridDesc( 7), iret ) ! Last latitude point (North Point)
    call i90_getVal ( 'run domain upper right lon:',self%gridDesc( 8), iret ) ! Last longitude Point (East point)
    call i90_getVal ( 'run domain resolution dx:',  self%gridDesc( 9), iret ) ! Delta x point
    call i90_getVal ( 'run domain resolution dy:',  self%gridDesc(10), iret ) ! Delta y point

    !
    ! reallocate grid between 0-360
    !

    if (self%gridDesc( 5) .lt. 0) self%gridDesc( 5) = self%gridDesc( 5) + 360.0
    if (self%gridDesc( 8) .lt. 0) self%gridDesc( 8) = self%gridDesc( 8) + 360.0

    self%gridDesc( 2) = ( ( self%gridDesc( 8) - self%gridDesc( 5) ) / self%gridDesc( 9) ) + 1
    self%gridDesc( 3) = ( ( self%gridDesc( 7) - self%gridDesc( 4) ) / self%gridDesc(10) ) + 1

#ifdef DEBUG         

    WRITE(*,'(  A   )')'Grid Specification'
    WRITE(*,'(A,F9.3)')'lower left latitude  :',self%gridDesc( 4)
    WRITE(*,'(A,F9.3)')'lower left longitude :',self%gridDesc( 5)
    WRITE(*,'(A,F9.3)')'upper right latitude :',self%gridDesc( 7)
    WRITE(*,'(A,F9.3)')'upper right longitude:',self%gridDesc( 8)
    WRITE(*,'(A,F9.3)')'resolution dx        :',self%gridDesc( 9)
    WRITE(*,'(A,F9.3)')'resolution dy        :',self%gridDesc(10)
    WRITE(*,'(A,I9.3)')'number of points (X) :',self%gridDesc( 2)
    WRITE(*,'(A,I9.3)')'number of points (Y) :',self%gridDesc( 3)            
#endif

    self%nxpt = self%gridDesc( 9)
    self%nypt = self%gridDesc(10)
    self%npts = self%gridDesc( 9) * self%gridDesc(10)

    self%udef = -999.9

    !
    ! Models stuffs
    !



    ! Reading Files Names and Variables names to Analise 
    !

    !
    ! Reference File
    !
    call i90_getVal( 'Reference Model Name:', ModelName, ierr ); iret = ierr
    call i90_getVal( 'Reference file:'      ,  FileName, ierr ); iret = iret + ierr
    if(iret .eq. 0)then
       call self%insertModel('Reference',trim(ModelName), 'refer', trim(FileName))
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
       call self%insertModel('Experiment',    &
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
       self%cflag = 0
    else
       self%cflag = i90_gint(iret)
       if(iret /= 0) then
          call i90_perr(trim(myname_),'i90_label("Use Climatology:")',iret)
          if(.not.present(istat))call i90_die(trim(myname_))
          istat=iret
          return
       endif
    endif

    IF(self%cflag.EQ.1)THEN

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

       call self%insertModel('Climatology',   &
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
    ALLOCATE(Exper(self%nexp),STAT=iret)
    self%currModel => self%FirstModel ! return to begin of linked list 
    I = 0
    DO while(associated(self%currModel))
       if (trim(self%currModel%Type_) .eq. 'Reference')then
          Refer%Id   = trim(self%currModel%Name_)
          Refer%file = trim(self%currModel%FileName_)
          Refer%name = trim(self%currModel%ExpName_)

       else if (trim(self%currModel%Type_) .eq. 'Climatology')then
          clima%Id   = trim(ModelName)
          clima%name = 'Climatology'
          clima%file = trim(FileName)

       else if (trim(self%currModel%Type_) .eq. 'Experiment')then

          i = i + 1
          Exper(I)%Id   = trim(self%currModel%Name_)
          Exper(I)%name = trim(self%currModel%ExpName_)
          Exper(I)%file = trim(self%currModel%FileName_)

       endif

#ifdef DEBUG
       WRITE(*,'(2A)')'Type : ', trim(self%currModel%Type_)
       WRITE(*,'(2A)')'  |---- Model Name :',trim(self%currModel%Name_)
       WRITE(*,'(2A)')'  |---- Exp Name   :',trim(self%currModel%ExpName_)
       WRITE(*,'(2A)')'  |---- File       :',trim(self%currModel%FileName_)
#endif
       self%currModel => self%currModel%next
    ENDDO

    !------------------------------------------------------------------------------- !Paulo Dias
    ! Diretorio de Saida
    !   
    call i90_label ( 'Output directory:', iret )
    call i90_Gtoken(self%output_dir,iret)
    if(iret /= 0) then
       call i90_perr(myname_,'i90_label("Output directory:")',iret)
       if(present(istat))istat=iret
       return
    endif

    !Fim Diretorio de Saida
    !--------


    !
    !-------------------------------------------------------------------!
    ! Reliase scantec.conf

    call I90_fullRelease( iret )
    if(iret /= 0) then
       call i90_perr(myname_,'i90_fullRelease("'//trim(config)//'")',iret)
       if(present(istat))istat=iret
       return
    endif


    !-------------------------------------------------------------------!
    ! configure scantec vars
    scanVarsConf = trim(self%tables)//'/scantec.vars'
    inquire(file=trim(scanVarsConf), exist=found)

    if(.not.found)then
       call i90_perr(trim(myname_),'scantec.vars not found!', -1 )
       call i90_die(trim(myname_))
    endif

    call i90_LoadF(trim(scanVarsConf), iret)
    if(iret.ne.0)then
       write(msg,'(3A)')'i90_LoadF("',trim(scanVarsConf),'")'
       call i90_perr(trim(myname_),trim(msg),iret)
       stop 99001
    endif

    call i90_label('variables:',iret)
    if(iret.ne.0)then
       call i90_perr(trim(myname_),'GetDef( variables ... )', iret)
       call i90_die(trim(myname_))
    endif
    allocate(First)
    curr => first
    nvars = 0
    call i90_gline(iret)
    if(iret.ne.0)then
       call i90_perr(trim(myname_),'scantec.vars: need configure at least one variable', iret)
       call i90_die(trim(myname_))
    endif

    do while(iret.eq.0)
       call i90_gtoken(curr%vname, iret)
       if (iret.eq.0)then
          nvars = nvars + 1
       else
          if(iret.lt.0)then
             call i90_perr(trim(myname_),'reading scantec.vars', iret)
             call i90_die(trim(myname_))
          endif
       endif

       call i90_gtoken(curr%vdesc, iret)
       if(iret.ne.0)then
          curr%vdesc = 'variable without description'
       endif


       ! get next line
       !    - iret =  0, next line ok
       !    - iret = -1, end of buffer (some problem with table)
       !    - iret = +1, end of table
       call i90_gline(iret)
       if(iret.lt.0)then
          call i90_perr(trim(myname_),'reading scantec.vars', iret)
          call i90_die(trim(myname_))
       endif
       allocate(curr%next)
       curr => curr%next
    enddo

    self%nvar = nVars

    Allocate(self%VarName(nVars))
    Allocate(self%VarDesc(nVars))
    curr => First
    do i=1,nvars
       self%VarName(i) = i90_lcase(curr%vname)
       self%VarDesc(i) = i90_lcase(curr%vdesc)
       curr => curr%next
    enddo


    curr => First%next
    do while(associated(curr))
       deallocate(First)
       First => curr
       curr => First%next
    enddo
    call I90_fullRelease(iret)
    if(iret /= 0) then
       call i90_perr(myname_,'i90_fullRelease("'//trim(scanVarsConf)//'")',iret)
       if(present(istat))istat=iret
       return
    endif



#ifdef DEBUG  
    write(6, FMT=123)'xdef',self%nxpt,'linear', self%gridDesc(5), self%gridDesc(10)
    write(6, FMT=123)'ydef',self%nypt,'linear', self%gridDesc(4), self%gridDesc(9)
123 FORMAT(A,1x,I4.3,1x,A,F9.3,F9.3)    
#endif


  END SUBROUTINE configure


END MODULE scantec_module
