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
  USE scan_Modelplugin, only: ModelType, EvalVar
  USE m_constants
  USE m_string          ! String Manipulation
  !USE scan_Utils, only: dom, Refer, Clima, Exper, Precip
  USE m_inpak90
  USE m_ioutil
  USE fileAccess
  USE BilinInterp, only: bilinear_interp_init, bilinear_interp
  USE varType
  USE MathExpress, only: tokenize
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


  SUBROUTINE allocate_data_mem()
    IMPLICIT NONE
    integer :: I
    integer :: nx
    integer :: ny
    integer :: nvar
    type(ModelType), pointer :: Model => null()
    character(len=*),parameter :: myname_=myname//' :: allocate_data_mem( )'

    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif


    nx   = scantec%nxpt
    ny   = scantec%nypt
    nvar = scantec%nvar

    Model => scantec%FirstModel
    do while(associated(Model))
       allocate(Model%Field(nx*ny,nvar))
       allocate(Model%bitMap(nx*ny,nvar))

       allocate(Model%w11(nx*ny))
       allocate(Model%w12(nx*ny))
       allocate(Model%w21(nx*ny))
       allocate(Model%w22(nx*ny))
       allocate(Model%n11(nx*ny))
       allocate(Model%n12(nx*ny))
       allocate(Model%n21(nx*ny))
       allocate(Model%n22(nx*ny))

       call bilinear_interp_init(Model%GDesc, &
                                 scantec%GridDesc, &
                                 Model%w11, &
                                 Model%w12, &
                                 Model%w21, &
                                 Model%w22, &
                                 Model%n11, &
                                 Model%n12, &
                                 Model%n21, &
                                 Model%n22  &
                                 )
       
       Model => Model%next
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
    character(len=LongStr) :: Reference    ! Reference File Name
    character(len=LongStr) :: Experiment   ! Experiment File Name
    character(len=LongStr) :: Climatology  ! Climatology File Name

    character(len=*),parameter :: myname_=myname//' :: scan_ModelData( )'


    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif


    aymd = scantec%atime/100
    ahms = MOD(scantec%atime,100) * 10000
    fymd = scantec%ftime/100
    fhms = MOD(scantec%ftime,100) * 10000

    !
    ! 1. Create file name and Open 
    !
    Reference  = TRIM(Refer%file)
    Experiment = TRIM(Exper(NExp)%file)
    Climatology= TRIM(Clima%file)

    if(scantec%TimeStepType .eq. 'forward')then
       CALL str_template(Climatology, fymd, fhms)
       CALL str_template(  Reference, fymd, fhms)
       CALL str_template( Experiment, aymd, ahms, fymd, fhms)
    else
       CALL str_template(Climatology, aymd, ahms)
       CALL str_template(  Reference, aymd, ahms)
       CALL str_template( Experiment, fymd, fhms, aymd, ahms)
    endif


    !
    ! 1.1 Reference data file 
    !

    CALL loadData('Reference', Refer%Id, Reference)

    !
    ! 1.2 Climatology data file
    !

    if (scantec%atime_flag)then

       IF(scantec%cflag.EQ.1)THEN
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


    !Joao adicionou para verificar quando nao tem o link das 0h
!    if (scantec%atime.eq.scantec%ftime)then
!       CALL replace(Experiment, 'fct','icn')
!    end if

    CALL loadData('Experiment', Exper(NExp)%Id, Experiment)

  END SUBROUTINE scan_ModelData

  SUBROUTINE loadData(ExpType, ModelName, FileName)
    character(len=*), intent(in) :: ExpType
    character(len=*), intent(in) :: ModelName
    character(len=*), intent(in) :: FileName

    character(len=*),parameter :: myname_=myname//' :: loadData( )'

    ! Integer variables
    integer :: i, j, k
    integer :: idx
    integer :: xdef
    integer :: ydef
    integer :: zdef
    integer :: klev
    integer :: iret
    integer, pointer :: Mxdef => null()
    integer, pointer :: Mydef => null()
    integer, pointer :: Mzdef => null()
    
    ! Real variables
    real    :: undef, level
    real, pointer :: zlevs(:) => null()
    real, allocatable :: iField(:), oField(:)

    ! Character variables
    character(len=100) :: VarName, VarLevel

    ! Logical variables
    logical :: found
    logical, allocatable :: ibitmap(:), obitmap(:)

    ! derivated type variables
    !type(GrADSfiles) :: gs
    type(ax) :: gs
    type(ModelType), pointer :: Model => null()
    type(EvalVar),   pointer :: ModelVar => null()

    ! to be used by functions
    class(variable), allocatable :: vars(:)
    integer :: ntokens, nvars
    character(len=50), allocatable :: tokens(:)
    character(len=32), allocatable :: outExp(:)
    character(len=32) :: fmtt

    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif


!
!---------------------------------------------------------------------!
!
    ! get model by name
    Model => scantec%getModel(ExpType, ModelName)

#ifdef DEBUG
    write(*,'(3(1x,A))')trim(Model%Name_),trim(Model%Type_),trim(Model%ExpName_)
    write(*,'(2(1x,A))')'FileName:',trim(FileName)
#endif

    ! Open current model and get info
    inquire(File=trim(FileName),exist=found)
    if(.not.found)then
       call i90_perr(trim(myname_),'File not found!'//trim(FileName), -1)
       call i90_die(trim(myname_))       
    endif


    iret = gs%open(trim(FileName))
    
    xdef = gs%getDim('xdef')
    ydef = gs%getDim('ydef')
    zdef = gs%getDim('zdef')

    ! Get model info from scantec table
    Mxdef => Model%getDimInfo('xdim:')
    Mydef => Model%getDimInfo('ydim:')
    Mzdef => Model%getDimInfo('zdim:')
    zlevs => Model%getDimVec('zdim:')

    ! Sanity Check
    if (xdef .ne. Mxdef .or. &
         ydef .ne. Mydef) then
       write(stderr,    '(A,1x,A)')trim(myname_),'error:'
       write(stderr,      '(2x,A)')'Dimensions not match!'
       write(stderr,      '(2x,A)')'wrong info by xdef, ydef.'
       write(stderr,'(2x,A,1x,2A)')'See ',trim(Model%Name_),'.model'
       call i90_die(trim(myname_))
    endif

    ! Get field and interpolate for scantec grid

    do i=1,scantec%nvar
       ModelVar => Model%getModelVar(trim(scantec%varName(i)))
       ! When model doesn't have a variable
       ! ModelVar will return null() and 
       ! Field will be set to undef
       
       if (.not.associated(ModelVar))then
          Model%Field(:,i)  = scantec%udef
          Model%bitmap(:,i) = .false.
          write(stdout,*)
          write(stdout, '(A)')char(27)//'[31;1mWARNING:'//char(27)//'[m'
          write(stdout,'(1x,3(1x,A))')'Variable',trim(scantec%varName(i)),'not set at model config file'
          write(stdout,*)
          write(stdout,'(2x,A,1x,2A)')'See ',trim(Model%Name_),'.model'
          write(stdout,*)
          cycle
       endif

       call tokenize(ModelVar%Mod_, ntokens, tokens)
       if (ntokens .eq. 1)then
          ! No mathematical expression, just need get
          ! a model field
          !
          !-------------------------------------------------------!
          ! Get model var info

          idx = index(ModelVar%Mod_,':')
          VarName  = ModelVar%Mod_(1:idx-1)
          VarLevel = ModelVar%Mod_(idx+1:len_trim(ModelVar%Mod_))
          read(VarLevel,*) Level

          !
          !-------------------------------------------------------!
          ! Get klevel - index of level
          
          zlevs => Model%getDimVec('zdim:')
          idx  = minloc(zlevs-Level,mask=zlevs-Level.ge.0, dim=1)

          !
          !-------------------------------------------------------!
          ! get field

          allocate(iField(xdef*ydef))
!          call GrADS_input(gs, trim(VarName),1,idx,iField, iret)
          call gs%getField(trim(VarName),zlevs(idx),iField,iret)

          if(iret.ne.0)then
             call i90_perr(trim(myname_),'getField('//trim(VarName)//')',iret)
             call i90_die(trim(myname_))
          endif

       else if (ntokens .gt. 1)then
          ! Need evaluate a mathematical
          ! expression to get a model field
          !

          ! count how many variables 
          ! there are in ModelExpression
          !
          nvars = 0
          do k=1,ntokens
             idx = index(trim(tokens(k)),':')
             if (idx .gt. 0) nvars = nvars + 1
          enddo
          allocate(vars(nvars))
          j=0
          do k=1,ntokens
             idx = index(trim(tokens(k)),':')
             if (idx .gt. 0)then
                VarName  = tokens(k)(1:idx-1)
                VarLevel = tokens(k)(idx+1:len_trim(tokens(k)))
                read(VarLevel,*) Level
    
                !
                !-------------------------------------------------------!
                ! Get klevel - index of level
                
                zlevs => Model%getDimVec('zdim:')
                idx  = minloc(zlevs-Level,mask=zlevs-Level.ge.0, dim=1)
                !
                !-------------------------------------------------------!
                ! get field
                allocate(iField(xdef*ydef))
                call gs%getField(trim(VarName),zlevs(idx),iField,iret)
    
                if(iret.ne.0)then
                   call i90_perr(trim(myname_),'getField('//trim(VarName)//')',iret)
                   call i90_die(trim(myname_))
                endif
    
                j=j+1
                call vars(j)%put(trim(tokens(k)),iField)
                deallocate(iField)
    
             endif
          enddo
!          print*, trim(ModelVar%Mod_)
          call scantec%MathEval%infix2postfix(ModelVar%Mod_,outExp)
!          write(fmtt,'(A1,I2,A7)')'(',size(outExp),'(A,1x))'
!          write(*,fmtt)(trim(outExp(k)),k=1,size(outExp))

          allocate(iField(xdef*ydef))
          call scantec%MathEval%evalPostFix(outExp, iField, vars, undef)

          deallocate(vars)
          
       else
          call i90_perr(trim(myname_),'wrong '//trim(ModelName)//'.model',iret)
          call i90_die(trim(myname_))
       endif

       allocate(iBitMap(xdef*ydef))
       iBitmap = .true.
       
       where(iField.eq.undef) iBitMap = .false.
       Model%bitMap(:,i) = .false.
       
       call bilinear_interp( ibitmap, iField, scantec%udef, &
                             Model%w11, Model%w12, Model%w21, Model%w22, &
                             Model%n11, Model%n12, Model%n21, Model%n22, &
                             Model%bitmap(:,i), Model%Field(:,i), &
                             iret )
      
       deallocate(iField)
       deallocate(iBitMap)
       
       where(.not.Model%bitmap(:,i))Model%Field(:,i) = scantec%udef
#ifdef DEBUG                 
       write(*,'(I3,1x,2F16.5)')i,minval(Model%Field(:,i),mask=Model%Field(:,i).ne.scantec%udef),&
                               maxval(Model%Field(:,i),mask=Model%Field(:,i).ne.scantec%udef)
#endif
       nullify(ModelVar)

    enddo

    iret = gs%close() 

  END SUBROUTINE loadData


END MODULE scan_dataMOD
