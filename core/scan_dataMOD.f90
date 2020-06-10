!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: scan_dataMOD.f90
!
! !DESCRIPTON:
!             
!                 
!\\
!\\
! !INTERFACE:
!

MODULE scan_dataMOD
  !
  ! !USES:
  !

  USE scantec_module    ! scantec types
  USE m_string          ! String Manipulation
  USE scan_Utils, only: dom, Refer, Clima, Exper, Precip
  USE m_inpak90
  USE m_ioutil
  USE m_GrADSfiles
  USE BilinInterp, only: bilinear_interp
  IMPLICIT NONE
  PRIVATE

  !
  ! !PUBLIC TYPES
  !

  type model_dec_type
     real, allocatable :: tmpfield(:,:) ! data from model read
     real, allocatable :: expfield(:,:) ! experiment model field
     real, allocatable :: reffield(:,:) ! reference model field
     real, allocatable :: clmfield(:,:) ! climatology field


     logical, allocatable :: UdfIdx (:,:)
  end type model_dec_type

  TYPE obs_dec_type
     real, allocatable :: tmpfield(:) ! data from model read
     real, allocatable :: expfield(:) ! experiment model field
     real, allocatable :: reffield(:) ! reference model field
     real, allocatable :: clmfield(:) ! climatology field
     real, allocatable :: diffield(:) ! diference field
  END TYPE obs_dec_type

  public :: model_dec_type
  public :: obs_dec_type

  !
  ! !PUBLIC DATA MEMBERS:
  !

  type(model_dec_type), public, Target, allocatable :: scandata(:)

  !
  ! !PUBLIC MEMBER FUNCTIONS:
  !

  public :: data_config
  public :: allocate_data_mem
  public :: data_init
  !  public :: ldata
  public :: scan_ModelData

  !
  ! !REVISION HISTORY:
  !  09 OCT 2011 - J. G. de Mattos - Initial Version
  !     SEP 2012 - J. G. de Mattos - Include routine to read files
  !  11 Oct 2012 - J. G. de Mattos - Remove bug at data_config routine.
  !                                  Lat was being read inverted
  !                                  dx and dy was inverted
  !
  ! !SEE ALSO:
  !   
  !
  !EOP
  !-----------------------------------------------------------------------------!
  !
  character(len=*),parameter :: myname = 'scan_dataMOD'

CONTAINS

  SUBROUTINE data_config()
    IMPLICIT NONE
    integer :: I
    integer :: iret
    character(len=256) :: scanVarsConf
    type vars
       character(len=8)    :: vname
       type(vars), pointer :: next => null()
    end type vars
    integer :: nvars
    type(vars), pointer :: first => null()
    type(vars), pointer :: curr => null()

    logical :: found
    character(len=32) :: msg

    character(len=*),parameter :: myname_=myname//' :: readModelConf( )'

    scantec%gridDesc = 0

    scantec%gridDesc( 1) = 0
    scantec%gridDesc( 2) = dom(1)%nx        ! Number of x points
    scantec%gridDesc( 3) = dom(1)%ny        ! number of y points
    scantec%gridDesc( 4) = dom(1)%ll_lat    ! First latitude point (South point)
    scantec%gridDesc( 5) = dom(1)%ll_lon    ! First longitude point (West point)
    scantec%gridDesc( 6) = 128
    scantec%gridDesc( 7) = dom(1)%ur_lat    ! Last latitude point (North Point)
    scantec%gridDesc( 8) = dom(1)%ur_lon    ! Last longitude Point (East point)
    scantec%gridDesc( 9) = dom(1)%dx        ! Delta x point
    scantec%gridDesc(10) = dom(1)%dy        ! Delta y point
    scantec%gridDesc(20) = 0

    scantec%nxpt = dom(1)%nx
    scantec%nypt = dom(1)%ny
    scantec%npts = dom(1)%nx*dom(1)%ny

    scantec%udef = -999.9

    scanVarsConf = trim(scantec%tables)//'/scantec.vars'
    inquire(file=trim(scanVarsConf), exist=found)

    if(.not.found)then
       call i90_perr(trim(myname_),'scantec.vars not found!', -1 )
       call i90_die(trim(myname_))
    endif

    call i90_LoadF(trim(scanVarsConf), iret)
    if(iret.ne.0)then
       write(msg,'(3A)')'i90_LoadF("',trim(scanVarsConf),'")'
       call i90_perr(trim(myname_),trim(msg),iret)
       stop
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

    scantec%nvar = nVars

    Allocate(scantec%VarName(nVars))
    curr => First
    do i=1,nvars
       scantec%VarName(i) = i90_lcase(curr%vname)
       curr => curr%next
    enddo


    curr => First%next
    do while(associated(curr))
       deallocate(First)
       First => curr
       curr => First%next
    enddo
    call I90_fullRelease(iret)



#ifdef DEBUG  
    write(6, FMT=123)'xdef',scantec%nxpt,'linear', scantec%gridDesc(5), scantec%gridDesc(10)
    write(6, FMT=123)'ydef',scantec%nypt,'linear', scantec%gridDesc(4), scantec%gridDesc(9)
123 FORMAT(A,1x,I4.3,1x,A,F9.3,F9.3)    
#endif


  END SUBROUTINE data_config



  SUBROUTINE allocate_data_mem()
    IMPLICIT NONE
    integer :: I
    type(ModelType), pointer :: currModel => null()

    currModel => scantec%FirstModel
    do while(associated(currModel))
       allocate(currModel%Field(scantec%nxpt*scantec%nypt,scantec%nvar))
       allocate(currModel%bitMap(scantec%nxpt*scantec%nypt,scantec%nvar))
       currModel => currModel%next
    enddo

  END SUBROUTINE allocate_data_mem

  SUBROUTINE data_init()
    IMPLICIT NONE
    integer :: I

!    DO I=1,scantec%nexp
!
!       scandata(I)%UdfIdx    = .true.
!
!    ENDDO

  END SUBROUTINE data_init

  SUBROUTINE release_data_mem()
    IMPLICIT NONE
    integer :: I

!    IF (Allocated(scandata(1)%reffield))DeAllocate(scandata(1)%reffield)
!    IF (Allocated(scandata(1)%clmfield))DeAllocate(scandata(1)%clmfield)
!    IF (Allocated(scandata(1)%tmpfield))DeAllocate(scandata(1)%tmpfield)
!
!    DO I=1,scantec%nexp
!       IF (Allocated(scandata(I)%expfield))DeAllocate(scandata(I)%expfield)
!       IF (Allocated(scandata(I)%UdfIdx))Deallocate(scandata(I)%UdfIdx)
!    ENDDO

  END SUBROUTINE release_data_mem


  SUBROUTINE scan_ModelData( NExp )
    IMPLICIT NONE
    integer, intent(in) :: NExp
    integer             :: aymd, ahms
    integer             :: fymd, fhms
    character(len=1024) :: Reference    ! Reference File Name
    character(len=1024) :: Experiment   ! Experiment File Name
    character(len=1024) :: Climatology  ! Climatology File Name


    aymd = scantec%atime/100
    ahms = MOD(scantec%atime,100) * 10000
    fymd = scantec%ftime/100
    fhms = MOD(scantec%ftime,100) * 10000

    !
    ! 1. Create file name and Open 
    !

    !
    ! 1.1 Reference data file 
    !

    Reference=TRIM(Refer%file)
    CALL str_template(Reference, fymd,fhms)
    CALL loadData('Reference', Refer%Id, Reference)

    !
    ! 1.2 Climatology data file
    !

    if (scantec%atime_flag)then

       IF(scantec%cflag.EQ.1)THEN
          Climatology=TRIM(Clima%file)
          CALL str_template(Climatology, fymd,fhms)
          CALL loadData('Climatology', Clima%Id, Climatology)
       END IF

       !
       ! 1.3 definindo flag = .false. para inibir a reabertura do arquivo de
       ! referencia e de climatologia para o mesmo tempo
       !

       scantec%atime_flag = .false.

    endif

    !
    ! 1.3 Experiment Data File
    !

    Experiment = TRIM(Exper(NExp)%file)

    !Joao adicionou para verificar quando nao tem o link das 0h
!    if (scantec%atime.eq.scantec%ftime)then
!       CALL replace(Experiment, 'fct','icn')
!    end if

    CALL str_template(Experiment, aymd, ahms, fymd, fhms)
    CALL loadData('Experiment', Exper(NExp)%Id, Experiment)

  END SUBROUTINE scan_ModelData

  SUBROUTINE loadData(ExpType, ModelName, FileName)
    character(len=*), intent(in) :: ExpType
    character(len=*), intent(in) :: ModelName
    character(len=*), intent(in) :: FileName

    character(len=*),parameter :: myname_=myname//' :: loadData( )'

    type(GrADSfiles) :: gs
    integer :: xdef
    integer :: ydef
    integer :: zdef
    integer :: klev
    integer :: i, idx, iret
    integer, pointer :: Mxdef => null()
    integer, pointer :: Mydef => null()
    integer, pointer :: Mzdef => null()
    real, pointer :: zlevs(:) => null()
    real    :: undef, level
    real, allocatable :: iField(:), oField(:)

    logical, allocatable :: ibitmap(:), obitmap(:)

    type(ModelType), pointer :: currModel => null()
    type(EvalVar), pointer :: ModelVar => null()
    character(len=10) :: VarName, VarLevel

    ! get model by name
    currModel => scantec%getModel(ExpType, ModelName)

    write(*,'(3(1x,A))')trim(currModel%Name_),trim(currModel%Type_),trim(currModel%ExpName_)

    ! Open current model and get info
    call GrADS_open(gs,trim(FileName))
    xdef  = GrADS_getDim(gs, 'xdef')
    ydef  = GrADS_getDim(gs, 'ydef')
    zdef  = GrADS_getDim(gs, 'zdef')
    undef = GrADS_Undef(gs)

    ! Get model info from scantec table
    Mxdef => getDimInfo(currModel,'xdim:')
    Mydef => getDimInfo(currModel,'ydim:')
    Mzdef => getDimInfo(currModel,'zdim:')
    zlevs => getDimVec(currModel,'zdim:')

    ! Sanity Check
    if (xdef .ne. Mxdef .or. &
         ydef .ne. Mydef .or. &
         zdef .ne. Mzdef) then
       write(stderr,    '(A,1x,A)')trim(myname_),'error:'
       write(stderr,      '(2x,A)')'Dimensions not match!'
       write(stderr,      '(2x,A)')'wrong info by xdef, ydef or zdef.'
       write(stderr,'(2x,A,1x,2A)')'See ',trim(currModel%Name_),'.model'
       call i90_die(trim(myname_))
    endif

    ! Get field and interpolate for scantec grid
    allocate(iField(xdef*ydef))
    allocate(iBitMap(xdef*ydef))

    do i=1,scantec%nvar
       ModelVar => getModelVar(currModel,trim(scantec%varName(i)))

       if (.not.ModelVar%deriv_)then
          !-------------------------------------------------------!
          ! Get model var info

          idx = index(ModelVar%Mod_,':')
          VarName  = ModelVar%Mod_(1:idx-1)
          VarLevel = ModelVar%Mod_(idx+1:len_trim(ModelVar%Mod_))
          read(VarLevel,*) Level

          !
          !-------------------------------------------------------!
          ! Get klevel - index of level
          
          zlevs => getDimVec(currModel,'zdim:')
          idx  = minloc(zlevs-Level,mask=zlevs-Level.ge.0, dim=1)

          !
          !-------------------------------------------------------!
          ! get field

          call GrADS_input(gs, trim(VarName),1,idx,iField, iret)

          if(iret.ne.0)then
             call i90_perr(trim(myname_),'GrADS_input('//trim(VarName)//')',iret)
             call i90_die(trim(myname_))
          endif

       else
          Modelvar%funcArg => Modelvar%FirstfuncArg
          do while(associated(Modelvar%funcArg))
!             print*,trim(ModelVar%Mod_),' ',trim(modelVar%funcArg%str_)
             modelVar%funcArg => modelVar%funcArg%next
          enddo
!          print*,'----'
          iField = undef
       endif

       iBitmap = .true.
       where(iField.eq.undef) iBitMap = .false.
       currModel%bitMap(:,i) = .false.
       call bilinear_interp( ibitmap, iField, scantec%udef, &
            currModel%w11, &
            currModel%w12, &
            currModel%w21, &
            currModel%w22, &
            currModel%n11, &
            currModel%n12, &
            currModel%n21, &
            currModel%n22, &
            currModel%bitmap(:,i),&
            currModel%Field(:,i), &
            iret           &
            )
         where(.not.currModel%bitmap(:,i))currModel%Field(:,i) = scantec%udef

    enddo

    call GrADS_close(gs, iret) 
    deallocate(iField)
    deallocate(iBitMap)
  END SUBROUTINE loadData

END MODULE scan_dataMOD
