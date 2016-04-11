!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOI
!
! !TITLE: Documentação do pacote ``Read Grib'' \\ Version 1.0
!
! !AUTHORS: João G. Z. de Mattos
!
! !AFFILIATION: Grupo de Desenvolvimento em Assimilação de Dados, INPE/CPTEC
!
! !DATE: 20 de Julho de 2014
!
! !INTRODUCTION: Visão Geral do Pacote
!
!      O pacote ReadGrid é um modulo escrito em Fortran 90 que contém rotinas 
!      para a leitura de arquivos no formato Grib1. Faz parte deste conjunto 
!      de rotinas o arquivo ascii que contem a tabela grib utilizada pelo 
!      CPTEC/INPE. Esta tabela também é utilizada como tabela padrao caso não
!      exista uma tabela específica para o grib que está sendo lido.
!
! \subsection{Exemplo de Leitura}
!
!  A seguir é mostrado um programa teste que faz uso deste módulo para realizar
!  a leitura de um arquivo grib1 proveniente do modelo global do INPE/CPTEC.
! 
! \begin{verbatin}
!
!   program RGrib
!      use read_grib
!
!      implicit none
!
!      type(grib) :: grb
!      real, allocatable :: fld(:,:)
!
!      grb%file="GPOSCPT20130101122013010118P.fct.TQ0299L064.grb"
!
!
!      call OpenGrib(grb)
!
!      allocate(fld(grb%gds(2),grb%gds(3)))
!   
!      call ReadGrib(grb,'UVEL',1000,fld)
!      print*,minval(fld),maxval(fld)
!
!      call ReadGrib(grb,'PSLC',0000,fld)
!      print*,minval(fld),maxval(fld)
!
!      call CloseGrib(grb)
!
!   end program
!
! \end{verbatin}
!
! Este pacote faz uso de alguns módulos que são listados a seguir e devem
! ser compilados em conjunto para que o módulo ReadGrib funcione corretamente.
!
! \begin{itemize}
!  \item {\bf m\_ioutil} : módulo contendo funcoes para operacoes de I/O;
!  \item {\bf m\_die}    : módulo contendo funcoes para mensagens de saida;
!  \item {\bf m\_stdio}  : módulo com definicoes de unidades de saida;
!  \item {\bf m\_string} : módulo com rotinas para manipulacao de strings.
! \end{itemize}
!
! Os três primeiros módulos fazem parte da biblioteca {\bf MPEU} desenvolvida por 
! membros do {\it Data Assimilation Office} da {\it National Aeronautics and Space
! Administration}. Esta biblioteca está disponível para download em
! \url{http://www.nco.ncep.noaa.gov/pmb/codes//nwprod/ngac.v1.0.0/sorc/ngac_fcst.fd/chem/gocart/src/GMAO_Shared/GMAO_mpeu/}.
!
!
!
!EOI
!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!
!-----------------------------------------------------------------------------!

module read_grib

  use m_ioutil, only : luavail  ! modulo contendo funcoes para operacoes de I/O
  use m_die, only : perr,die    ! modulo contendo funcoes para mensagens de saida
  use m_stdio                   ! modulo com definicoes de unidades de saida
  use m_string, only: GetTokens ! modulo com rotinas para manipulacao de strings


  implicit none
  private

! !ROUTINES:

  public :: OpenGrib
  public :: ReadGrib
  public :: CloseGrib

! !PUBLIC TYPES:

  public :: grib


! !REVISION HISTORY:
!   20 Jul 2014 - J. G. de Mattos - Initial Version
!   05 Jan 2016 - J. G. de Mattos - Include interface to return
!                                   1d and 2d fields
!   06 Jan 2016 - J. G. de Mattos - Bug fix
!                                   Problem to read multiple files
!                                   in same routine, like read files
!                                   to make statistical evaluation
!
! !BUGS:
!   Not yet
!
!EOP
!---------------------------------------------------------------------!


  type gbtab
     integer           :: parm = -1
     character(len=10) :: name = ''
     character(len=50) :: desc = ''
  end type gbtab

  type grbvardesc
     integer           :: pds5
     integer           :: pds6
     integer           :: pds7
     character(len=10) :: name
     character(len=50) :: desc
  end type grbvardesc

  type grib
     character(len=1024)           :: File     = ''
     integer                       :: lu       = -1
     integer                       :: PDS(200) = -1
     integer                       :: GDS(200) = -1
     integer                       :: nvar     = -1
     real                          :: undef    = -1e+20
     type(grbvardesc), allocatable :: var(:)
  end type grib

  interface ReadGrib
     module procedure input1d_gip, input1d_,&
                      input2d_gip, input2d_
  end interface

  Character(len=*), parameter :: myname = "ReadGrib"

contains
!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!
!BOP
!
! !IROUTINE: OpenGrib - Open a Grib1 file and populate the grib data type 
!
! !DESCRIPTION:
!
! !INTERFACE:

  subroutine OpenGrib( grb, istat )
  
    implicit none

    type(grib), intent(inout)      :: grb
    integer, optional, intent(out) :: istat

    Character(len=*), parameter :: myname_ = trim(myname)//"::OpenGrib"

    !
    ! local
    !

    integer, parameter ::  mbuf=256*1024

    type(gbtab), allocatable :: gtb(:)

    integer   :: jpds(200),jgds(200)
    integer   :: kpds(200),kgds(200)
    real      :: kg,kf,kk
    character :: cbuf(mbuf)
    integer   :: nlen
    integer   :: nnum
    integer   :: mnum

    integer :: iret
    integer :: idx
    integer :: nrec
    integer :: i

    type(grbvardesc) :: this_grb(1000)


    if(present(istat))istat=0


    grb%lu = luavail( )

    call baopenr(grb%lu,grb%file, iret)
    if (iret.ne.0)then
       call perr(myname_,'Error to open Grib File file:'//trim(grb%file),iret)
       if(present(istat))istat=iret
       return
    endif

    jpds = -1
    jgds = -1
    call getgbmh(grb%lu,0,-1,jpds,jgds,mbuf,cbuf,nlen,nnum,mnum,kg,kf,kk,kpds,kgds,iret)
!    call getgbh (grb%lu,0,0,jpds,jgds,kg,kf,kk,kpds,kgds,iret)
    if (iret.ne.0)then
       call perr(myname_,'Error to read Grib file:'//trim(grb%file),iret)
       if(present(istat))istat=iret
       return
    endif

    grb%gds = kgds
    grb%pds = kpds

    call GribTable(kpds(1),gtb)

    this_grb(1)%pds5 = kpds(5)
    this_grb(1)%pds6 = kpds(6)
    this_grb(1)%pds7 = kpds(7)

    idx = minloc(gtb%parm-kpds(5),mask=gtb%parm-kpds(5).GE.0,DIM=1)

    if(kpds(5).eq.gtb(idx)%parm)then
       this_grb(1)%name = gtb(idx)%name
       this_grb(1)%desc = gtb(idx)%desc
    else
       write(this_grb(1)%name,'(A4,I3.3)')'NONE',this_grb(1)%pds5
       this_grb(1)%desc = 'None'
    endif

    nrec = 1
    do while(iret.eq.0)
    
       jpds = -1
       jgds = -1

       call getgbmh(grb%lu,0,nrec,jpds,jgds,mbuf,cbuf,nlen,nnum,mnum,kg,kf,kk,kpds,kgds,iret)
!       call getgbh(grb%lu,0,nrec,jpds,jgds,kg,kf,kk,kpds,kgds,iret)

       this_grb(nrec+1)%pds5 = kpds(5)
       this_grb(nrec+1)%pds6 = kpds(6)
       this_grb(nrec+1)%pds7 = kpds(7)

       idx = minloc(gtb%parm-kpds(5),mask=gtb%parm-kpds(5).GE.0,DIM=1)

       if(kpds(5).eq.gtb(idx)%parm)then
          this_grb(nrec+1)%name = gtb(idx)%name
          this_grb(nrec+1)%desc = gtb(idx)%desc
       else
          write(this_grb(nrec+1)%name,'(A4,I3.3)')'NONE',this_grb(1)%pds5
          this_grb(nrec+1)%desc = 'None'
       endif

       nrec = nrec + 1

    enddo
    nrec = nrec - 1
    
#ifdef VERBOSE
    write(stdout,'(A,1X,I4.3,1X,A)')' Found',nrec,'2D fields'
    write(stdout,'(A)')trim(grb%file)
    do i = 1, nrec
       write(stdout,'(2x,a,3(1x,i6.2))')this_grb(i)%name,this_grb(i)%pds5,this_grb(i)%pds6,this_grb(i)%pds7
    enddo
#endif

    allocate(grb%var(nrec))
    grb%nvar        = nrec
    grb%var(1:nrec) = this_grb(1:nrec)

  end subroutine OpenGrib


  subroutine input2d_gip(grb,vgip,hlev,vfld,stat)
     implicit none
       
     type(grib),       intent(in)    :: grb
     integer,          intent(in)    :: vgip ! grid indicator of parameter
     integer,          intent(in)    :: hlev ! which level?
     real,dimension(:,:),intent(out) :: vfld ! a 2-d gridded field
     integer, optional,intent(out)   :: stat

     character(len=*), parameter :: myname_=myname//':: ReadGrib'
     
     integer :: i, nidx, ivar
     character(len=10) :: vname
   
     ! Check/index the requested variable

     nidx=count((grb%var(:)%pds5-vgip).eq.0)

     if(nidx .eq. 0) then

        write(stderr,'(2a,i4,a)') myname_,': unknown Grid Indicator of Parameter"',vgip,'"'
        write(stderr,'(2a)') myname_,': return undefined value to variable "'
!        if(.not.present(stat)) call die(myname_)
!        stat=3
        if(present(stat))stat = 0
        vfld = grb%undef
        return
        
     endif

     !
     ! Get Var Name
     !
     ivar=MINVAL(PACK((/(i,i=1,grb%nvar)/), mask=(grb%var%pds5-vgip).EQ.0))

     vname=grb%var(ivar)%name
     
     call input2d_(grb,trim(vname),hlev,vfld,stat)

  end subroutine

  subroutine input1d_gip(grb,vgip,hlev,vfld,stat)
     implicit none
     
     type(grib),       intent(in)   :: grb
     integer,          intent(in)   :: vgip ! grid indicator of parameter
     integer,          intent(in)   :: hlev ! which level?
     real,dimension(:),intent(out)  :: vfld ! a 1-d gridded field
     integer, optional,intent(out)  :: stat
     character(len=*), parameter :: myname_=myname//':: ReadGrib'
     integer :: i, nidx, ivar
     character(len=10) :: vname
     ! Check/index the requested variable

     nidx=count((grb%var(:)%pds5-vgip).eq.0)

     if(nidx .eq. 0) then

        write(stderr,'(2a,i4,a)') myname_,': unknown Grid Indicator of Parameter"',vgip,'"'
        write(stderr,'(2a)') myname_,': return undefined value to variable "'
!        if(.not.present(stat)) call die(myname_)
!        stat=3
        if(present(stat))stat = 0
        vfld = grb%undef
        return
        
     endif

     !
     ! Get Var Name
     !
     ivar=MINVAL(PACK((/(i,i=1,grb%nvar)/), mask=(grb%var%pds5-vgip).EQ.0))

     vname=grb%var(ivar)%name
     
     call input1d_(grb,trim(vname),hlev,vfld,stat)

  end subroutine


  subroutine input2d_(grb,vnam,hlev,vfld,stat)
     implicit none
     
     type(grib), intent(in) :: grb
     character(len=*), intent(in)    :: vnam    ! what variable?
     integer,          intent(in)    :: hlev    ! which level?
     real,dimension(:,:),intent(out) :: vfld    ! a 2-d gridded field
     integer, optional,intent(out)   :: stat

! !REVISION HISTORY:
!     20 Jul 2014 - J. G. de Mattos - initial prototyping and coding
!
!
!EOP
     character(len=*), parameter :: myname_=myname//':: ReadGrib'

     real, allocatable      :: ftmp(:)
     integer :: ipt, jpt
     integer :: i, j, k

     ! Check the buffer dimensions

     if(size(vfld,1) /= grb%gds(2) .or. size(vfld,2) /= grb%gds(3)) then

        write(stderr,'(2a,$)') myname_,': invalid arguments'
        write(stderr,'(a,1i6,a,$)') ', shape(vfld) = (',shape(vfld),')'
        write(stderr,'(a,2i6,a,$)') ', grb%pds(2:3) = (',grb%gds(2),grb%gds(3),')'
        write(stderr,*)
        if(.not.present(stat)) call die(myname_)
        stat=2
        return

     endif


     ipt = size(vfld,1)
     jpt = size(vfld,2)

     allocate(ftmp(ipt*jpt))

     
     call input1d_(grb,vnam,hlev,ftmp,stat)

     !
     ! Colocando em uma matriz 2d
     !

     k = 0
     do j=1,grb%gds(3)
        do i=1,grb%gds(2)
           vfld(i,j) = ftmp(i+k)
        enddo
        k = k + grb%gds(2)
     enddo


  end subroutine


  subroutine input1d_(grb,vnam,hlev,vfld,stat)
     implicit none
     
     type(grib), intent(in) :: grb
     character(len=*), intent(in)  :: vnam    ! what variable?
     integer,          intent(in)  :: hlev    ! which level?
     real,dimension(:),intent(out) :: vfld    ! a 1-d gridded field
     integer, optional,intent(out) :: stat

! !REVISION HISTORY:
!     20 Jul 2014 - J. G. de Mattos - initial prototyping and coding
!
!
!EOP

     character(len=*), parameter :: myname_=myname//':: ReadGrib'

     !
     ! Local
     !
     integer :: i, j, k
     integer :: ierr
     integer :: ivar
     integer :: vpds
     integer :: hcount
     character(len=512) :: fmt

     !
     ! use to Grib fields
     !

     integer :: npts
     integer :: jpds(200),jgds(200)
     integer :: kpds(200),kgds(200)
     real    :: kf,kh
     logical*1, allocatable :: lb(:)
     real, allocatable      :: f(:)

 
     if(present(stat))stat=0


     !
     ! Sanity checks
     !

     ! Check the buffer dimensions

     if(size(vfld,1) /= grb%gds(2)*grb%gds(3)) then

        write(stderr,'(2a)') myname_,': invalid arguments'
        write(stderr,'(a,i6,a)') ', shape(vfld) = (',shape(vfld),')'
        write(stderr,'(a,i6,a)') ', grb%pds(2:3) = (',grb%gds(2)*grb%gds(3),')'
        write(stderr,*)
        if(.not.present(stat)) call die(myname_)
        stat=2
        return

     endif

     ! Check/index the requested variable

     ivar=lindex_(grb%var(:)%name,vnam)
     if(ivar <= 0) then

        write(stderr,'(4a)') myname_,': unknown variable "',trim(vnam),'"'
        write(stderr,'(4a)') myname_,': return undefined value to variable "',trim(vnam),'"'
!        if(.not.present(stat)) call die(myname_)
!        stat=3
        if(present(stat))stat = 0
        vfld = grb%undef
        return
        
     endif

     ! Check the requested level
     vpds   = grb%var(ivar)%pds5
     hcount = count(                                                &
                  ((grb%var( PACK ( (/(i,i=1,grb%nvar)/),           &
                  mask=(grb%var%pds5-vpds).EQ.0) )%pds7)-hlev).eq.0 &
                 )
     if( hcount < 1 )then

        hcount = count((grb%var%pds5-vpds).EQ.0)
        write(stderr,'(2a)') myname_,': invalid level request'
        write(stderr,'( a,i4)')', Level =',int(hlev)
        write(stderr,'(3a,i3)')'grb%nlevs("',trim(vnam),'") =', hcount
        write(fmt,'(a,i5,a)')'(a,',hcount,'I4)'
        write(stderr,fmt)', grb%levs[hPa] =', grb%var( PACK ( (/(i,i=1,grb%nvar)/),&
                                       mask=(grb%var%pds5-vpds).EQ.0) )%pds7
        if(.not.present(stat)) call die(myname_)
        stat=5
        return

     endif

     !
     ! Get Variable
     !

     npts    = grb%gds(2)*grb%gds(3)
     jpds    = -1
     jgds    = -1
     kpds    = -1
     kgds    = -1

     allocate(lb(npts), f(npts), stat=ierr)

      if(ierr/=0) then
         call perr(myname_,'allocate()',ierr)
         if(.not.present(stat)) call die(myname_)
         stat=ierr
         return
     endif

     lb = .true.

     jpds(5) = vpds
     jpds(7) = hlev

     call getgb(grb%lu,0,npts,-1,jpds,jgds,kf,kh, &
                kpds,kgds,lb,f,ierr)
!     call getgb(grb%lu,0,npts,0,jpds,jgds,kf,kh, &
!                kpds,kgds,lb,f,ierr)

     if(ierr /= 0) then
        write(stderr,'(2a,i5)') myname_,    &
         ': getgb() error, ierr =',ierr
        if(.not.present(stat)) call die(myname_)
        stat=ierr
        return
     endif

     !
     ! aplicando o undef onde lb .false.
     !
     where(.not.lb) f = grb%undef

     vfld = f

  end subroutine


  subroutine GribTable(center,gtb, istat)

    implicit none

    integer, intent(in)                     :: center
    type(gbtab), allocatable, intent(inout) :: gtb(:)
    integer, optional, intent(out)          :: istat

    Character(len=*), parameter :: myname_ = trim(myname)//"::GribTable"


    real                            :: x
    integer                         :: i
    integer                         :: lu
    logical                         :: iret
    integer                         :: nline
    integer                         :: nbf
    character(len=200)              :: GribTableFile
    character(len=256)              :: bf
    character(len=050), allocatable :: bf2(:)
    integer, parameter              :: len_header = 400

    if(present(istat))istat=0

    !
    ! Reading Grib Table
    !

    write(GribTableFile,'(A,I3.3,A)')'gribtab.',center,'.tab'
    inquire(file=trim(GribTableFile),exist=iret)

    if(.not.iret)then

       call perr(myname_,'GribTable '//trim(GribTableFile)//' not found! Will use gribtab.default.tab')
       GribTableFile="gribtab.default.tab"
       inquire(file=trim(GribTableFile),exist=iret)
       if(.not.iret) then
          call perr(myname_,'Default GribTable not found! Will stop...')
          if(present(istat))istat=-1
          call exit()
       endif

    endif

    lu = luavail( )
    open( unit = lu,                &
         file = trim(GribTableFile),&
         action = 'read'            &
         )

    !
    ! count lines on grib table
    !

    nline = 0
    do
       read(lu,*,end=88,err=88)
       nline = nline + 1
    enddo
88  continue
    nline = nline - 1  ! remove header from count
    rewind(lu)

    allocate(gtb(nline))
    allocate(bf2(len_header))

    !
    ! Read Table
    !

    read(lu,'(A)')bf !skip first line

    do i=1,nline

       read(lu,'(A)')bf
       call GetTokens(bf,bf2,nbf,":")

       read(bf2(1),*)x
       gtb(i)%parm = int(x)
       gtb(i)%name = trim(bf2(2))
       gtb(i)%desc = trim(bf2(3))

    enddo
    deallocate(bf2)
    close (lu)

  end subroutine GribTable

  function lindex_(lsts,entr)
    use m_chars, only : uppercase
    implicit none
    character(len=*),dimension(:),intent(in) :: lsts
    character(len=*),             intent(in) :: entr

    integer :: lindex_    ! the result

    !--------------------------------------------------------
    integer :: i

    !--------------------------------------------------------
    lindex_=0
    do i=1,size(lsts)
      if(uppercase(entr) == uppercase(lsts(i))) then
         lindex_=i
         return
      endif
    end do
  end function lindex_

  subroutine CloseGrib(grb,stat)
     implicit none
     type(grib), intent(inout) :: grb
     integer, optional, intent(out) :: stat
     Character(len=*), parameter :: myname_ = trim(myname)//"::CloseGrib"

     integer :: lu
     integer :: ier

     if(present(stat)) stat=0

     lu=grb%lu

     call baclose(lu,ier)

     if(ier/=0) then
        call perr(myname_,'baclose()',ier)
        if(.not.present(stat)) call die(myname_)
        stat=ier
        return
     endif

     grb%File       = ''
     grb%lu         = -1
     grb%PDS(1:200) = -1
     grb%GDS(1:200) = -1
     grb%nvar       = -1

     deallocate(grb%var,stat=ier)
     if(ier/=0) then
        call perr(myname_,'deallocate(grb%var)',ier)
        if(.not.present(stat)) call die(myname_)
        stat = ier
        return
     endif

  end subroutine CloseGrib


end module read_grib
