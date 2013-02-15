!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: SCAM_ctableMOD.f90
!
! !DESCRIPTON: This module contains routines and functions to compute many
!              skill scores based on binary and multicategorical
!              contingency table.
!                 
!\\
!\\
! !INTERFACE:
!

MODULE SCAM_ctableMOD

  !
  ! !USES:
  !

  USE m_die                         ! Error Messages
  USE m_stdio                       ! Module to defines std. I/O parameters
  USE scamtec_module                ! SCAMTEC types
  USE SCAM_dataMOD, only : scamdata ! SCAMTEC data matrix

  IMPLICIT NONE
  PRIVATE
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  !

  public :: bctable     ! Binary Contingency table
  public :: mctable     ! Multictegorical Contingency table
  public :: skill_score ! skill_scores


  !
  !
  ! !REVISION HISTORY:
  !  10 Nov 2012 - J. G. de Mattos - Initial Version
  !  19 Nov 2012 - J. G. de Mattos - include documentation
  !
  ! !SEE ALSO:
  !
  !     * Nurmi (2003): Recommendations on the verification of local
  !                     weather forecasts. ECMWF Tech. Mem. 430.
  ! 
  !     * Wilks (2005): Statistical Methods in the Atmospheric
  !                     Sciences, Chapter 7 (Forecast Verification). Academic Press.
  !
  !
  ! !BUGS:
  !
  !   Not yet!
  !
  !EOP
  !-----------------------------------------------------------------------------!
  !

  character(len=*),parameter :: myname='SCAM_ctableMOD'

contains

  !
  !------------------------------------------------------------------!
  !BOP
  !
  !
  ! !IROUTINE:  bctable
  !
  ! !DESCRIPTION: Routine to compute the Binary contingency table. 
  !               Use thresholds to cut yes/no precipitation event.
  !
  !                                   O B S
  !   
  !                               | Yes |  No |
  !                           -------------------
  !                       F   Yes |  A  |  B  |
  !                       C   -------------------
  !                       T   No  |  C  |  D  |
  !
  ! You should give multiple thresholds to compute this table. In other words,
  ! you should give a vector like this:
  !
  !   thres(1:8) = (/ 0.254, 2.54, 6.53, 12.7, 19.05, 25.4, 38.1, 50.8 /)
  !
  ! to compute a binary table. For this example we have 8 cases of yes/no rain
  !
  !                           |  No |        |  Yes
  !                        Cat| Rain| Thresh | Rain
  !                        ---+-----+--------+------
  !                         1 | x < | 00.254 | <= x
  !                         2 | x < | 02.540 | <= x
  !                         3 | x < | 06.530 | <= x
  !                         4 | x < | 12.700 | <= x
  !                         5 | x < | 19.050 | <= x
  !                         6 | x < | 25.400 | <= x
  !                         7 | x < | 38.100 | <= x
  !                         8 | x < | 50.800 | <= x
  !                        ---+-----+--------+------
  !
  ! for each one we compute the index A, B, C and D of the binary table.
  !\\
  !\\
  ! !INTERFACE:
  !

  subroutine BCTable(Fct, Obs, thres, A, B, C, D, Undef)
    implicit none

    !
    ! !INPUT PARAMETERS:
    !
    ! Fct   = Forecast Field [Nx*Ny]
    ! OBs   = Observation Field [Nx*Ny]
    ! Thres = Threshold to cut rain event [Nthreshold]
    ! Undef = Undefined value                          

    REAL, DIMENSION(:), INTENT(IN) :: Fct
    REAL, DIMENSION(:), INTENT(IN) :: Obs
    REAL, DIMENSION(:), INTENT(IN) :: thres
    REAL              , INTENT(IN) :: Undef

    !
    ! !OUTPUT PARAMETERS:
    !
    ! A = Number of "yes" forecast, "yes" observation grid points.
    ! B = Number of "yes" forecast, "no" observation grid points.
    ! C = Number of "no" forecast, "yes" observation grid points.
    ! D = Number of "no" forecast, "no" observation grid points.

    INTEGER, DIMENSION(:), ALLOCATABLE :: A 
    INTEGER, DIMENSION(:), ALLOCATABLE :: B 
    INTEGER, DIMENSION(:), ALLOCATABLE :: C 
    INTEGER, DIMENSION(:), ALLOCATABLE :: D

    !
    !
    ! !REVISION HISTORY: 
    !  10 Nov 2012 - J. G. de Mattos - Initial Version
    ! 
    !
    ! !SEE ALSO:
    !
    !     * Nurmi (2003): Recommendations on the verification of local
    !                     weather forecasts. ECMWF Tech. Mem. 430.
    ! 
    !     * Wilks (2005): Statistical Methods in the Atmospheric
    !                     Sciences, Chapter 7 (Forecast Verification). Academic Press.
    !
    !
    !EOP
    !----------------------------------------------------------------!
    !BOC
    !----------------------------------------------------------------!
    ! misc variables

    integer :: i, t
    integer :: nt, np
    integer :: iret
    integer, allocatable, dimension(:) :: Idx

    character(len=*),parameter :: myname_=myname//'::BCtable'

    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif


    if(size(Fct).ne.size(Obs)) Then

       call perr(myname_,'Incompatible Size of the matrices',-1)
       return

    End if

    np = min(size(Fct),size(Obs))
    ni = count (scamdata(run)%UdfIdx)
    nt = size(thres)

    ALLOCATE(Idx(np))

    !
    ! Creating mask to only compute contingency table for valid grid points
    !

    Idx(1:ni) = PACK ( (/(i,i=1,np)/), mask = scamdata(run)%UdfIdx)

    !
    ! loop over all thresholds
    !

    DO t=1,nt

       !
       ! Os thresholds sao pontos de corte para chuva e não chuva. Valores iguais ou
       ! superiores ao threshold são considerados chuva e computados como tal.
       !

       A(t) = count(fct(Idx)>=Thres(t).and.Obs(Idx)>=Thres(t))
       B(t) = count(fct(Idx)>=Thres(t).and.Obs(Idx)< Thres(t))
       C(t) = count(fct(Idx)< Thres(t).and.Obs(Idx)>=Thres(t))
       D(t) = count(fct(Idx)< Thres(t).and.Obs(Idx)< Thres(t))

    ENDDO

    DEALLOCATE(idx,stat=iret)
    if(iret.ne.0) then
       call perr(myname_,'deallocate(mask)',iret)
       return
    endif


  end subroutine BCTable
  !
  !EOC
  !
  !------------------------------------------------------------------!
  !BOP
  !
  !
  ! !IROUTINE:  mctable
  !
  ! !DESCRIPTION: Routine to compute the Multicategorical contingency table. 
  !               
  !
  !                                   O B S
  !   
  !                               | Yes |  No |
  !                           -------------------
  !                       F   Yes |  A  |  B  |
  !                       C   -------------------
  !                       T   No  |  C  |  D  |
  !
  !
  ! You should give multiple categories to compute this table. In other words,
  ! you should give a vector like this:
  !
  !   cat(1:8) = (/ 0.254, 2.54, 6.53, 12.7, 19.05, 25.4, 38.1, 50.8 /)
  !
  ! to compute a multicategorical table. For this example we have 9 categories:
  !
  !                          1 -           x <  0.254
  !                          2 -  0.254 <= x <  2.540
  !                          3 -  2.540 <= x <  6.530
  !                          4 -  6.530 <= x < 12.700
  !                          5 - 12.700 <= x < 19.050
  !                          6 - 19.050 <= x < 25.400
  !                          7 - 25.400 <= x < 38.100
  !                          8 - 38.100 <= x < 50.800
  !                          9 - 50.800 <= x
  !
  ! for each one we compute the index A, B, C and D of the multicategorical table.
  !\\
  !\\
  ! !INTERFACE:
  !


  subroutine MCTable(Fct, Obs, Cat, A, B, C, D, Undef)
    implicit none

    !
    ! !INPUT PARAMETERS:
    !
    ! Fct   = Forecast Field [Nx,Ny]
    ! Obs   = Observation Field [Nx,Ny]
    ! Cat   = Categories to compute a rain event [NCategories]
    ! Undef = Undefined value                          

    REAL, DIMENSION(:,:), INTENT(IN) :: Fct
    REAL, DIMENSION(:,:), INTENT(IN) :: Obs
    REAL, DIMENSION(:)  , INTENT(IN) :: Cat
    REAL                , INTENT(IN) :: Undef

    !
    ! !OUTPUT PARAMETERS:
    !
    ! A = Number of "yes" forecast, "yes" observation grid points.
    ! B = Number of "yes" forecast, "no" observation grid points.
    ! C = Number of "no" forecast, "yes" observation grid points.
    ! D = Number of "no" forecast, "no" observation grid points.

    INTEGER, DIMENSION(:), ALLOCATABLE :: A 
    INTEGER, DIMENSION(:), ALLOCATABLE :: B 
    INTEGER, DIMENSION(:), ALLOCATABLE :: C 
    INTEGER, DIMENSION(:), ALLOCATABLE :: D

    !
    !
    ! !REVISION HISTORY: 
    !  10 Nov 2012 - J. G. de Mattos - Initial Version
    ! 
    !
    ! !SEE ALSO:
    !
    !     * Nurmi (2003): Recommendations on the verification of local
    !                     weather forecasts. ECMWF Tech. Mem. 430.
    ! 
    !     * Wilks (2005): Statistical Methods in the Atmospheric
    !                     Sciences, Chapter 7 (Forecast Verification). Academic Press.
    !
    !
    !EOP
    !----------------------------------------------------------------!
    !BOC
    !----------------------------------------------------------------!
    ! misc variables

    integer :: i, j, t
    integer :: nt, ni, nj
!    integer :: iret
    integer, allocatable, dimension(:,:) :: TmpF
    integer, allocatable, dimension(:,:) :: TmpO
    integer, allocatable, dimension(:,:) :: mask

    character(len=*),parameter :: myname_=myname//'::MCtable'

    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif


    if(size(Fct,1).ne.size(Obs,1).or.&
         size(Fct,2).ne.size(Obs,2)) Then

       call perr(myname_,'Incompatible Size of the matrices',-1)
       return

    End if


    ni = min(size(Fct,1),size(Obs,1))
    nj = min(size(Fct,2),size(Obs,2))
    nt = size(cat)

    ALLOCATE(TmpF(ni,nj))
    ALLOCATE(TmpO(ni,nj))
    ALLOCATE(mask(ni,nj))

    !
    ! Creating mask to only compute contingency table for valid grid points
    !

    mask = 1
    where(Fct.eq.Undef.or.Obs.eq.Undef) mask = 0

    TmpF = 0
    TmpO = 0
    
    DO J=1,nj
       DO I=1,ni

          if (mask(i,j).eq.0)cycle

          TmpF(i,j)=minloc(Fct(i,j)-cat,mask=(Fct(i,j)-cat).GE.0,DIM=1)
          TmpO(i,j)=minloc(Obs(i,j)-cat,mask=(Obs(i,j)-cat).GE.0,DIM=1)

       ENDDO
    ENDDO

    !
    ! loop over all thresholds
    !

    DO t=1,nt

       !
       ! Os thresholds sao pontos de corte para chuva e não chuva. Valores iguais ou
       ! superiores ao threshold são considerados chuva e computados como tal.
       !

       A(t) = count(TmpF.eq.t.and.TmpO.eq.t)
       B(t) = count(TmpF.eq.t.and.TmpO.ne.t)
       C(t) = count(TmpF.ne.t.and.TmpO.eq.t)
       D(t) = count(TmpF.ne.t.and.TmpO.ne.t)

    ENDDO



  end subroutine MCTable
  !
  !EOC
  !
  !------------------------------------------------------------------!
  !BOP
  !
  !
  ! !IROUTINE:  skill_score
  !
  ! !DESCRIPTION: Routine to compute some skill scores based on contingency table. 
  !               
  !               The index computed are:
  !
  !                *  ETS : Equitable Threat Score.
  !                         ETS  = (A - R)/(A + B + C - R)
  !                         R    = (A + B)*(A + C)/(A + B + C + D)
  !
  !                *  BIAS:
  !                        Bias = (A + B)/(A + C)
  !
  !                *  PC  : Proportion Correct
  !                         PC = ( A + D ) / ( A + B + C + D )
  !
  !                *  FAR : False Alarm Ratio
  !                         FAR = B / ( A + B )  
  !
  !                *  POD : Probability Of Detection
  !                         POD = A / ( A + C )
  !
  !
  !\\
  !\\
  ! !INTERFACE:
  !


  subroutine skill_score(A, B, C, D, ETS, BIAS, PC, FAR, POD, Undef)
    implicit none

    !
    ! !INPUT PARAMETERS:
    !
    ! A     = Number of "yes" forecast, "yes" observation grid points.
    ! B     = Number of "yes" forecast, "no" observation grid points.
    ! C     = Number of "no" forecast, "yes" observation grid points.
    ! D     = Number of "no" forecast, "no" observation grid points.
    ! Undef = Undefined Value

    INTEGER, DIMENSION(:), INTENT(IN) :: A 
    INTEGER, DIMENSION(:), INTENT(IN) :: B 
    INTEGER, DIMENSION(:), INTENT(IN) :: C 
    INTEGER, DIMENSION(:), INTENT(IN) :: D
    REAL, INTENT(IN)                  :: Undef

    !
    ! !OUTPUT PARAMETERS:
    !
    ! ETS  = Equitable Threat Score.
    ! BIAS =
    ! PC   = Proportion Correct
    ! FAR  = False Alarm Ratio
    ! POD  = Probability Of Detection

    REAL,    DIMENSION(:), INTENT(OUT) :: ETS 
    REAL,    DIMENSION(:), INTENT(OUT) :: BIAS
    REAL,    DIMENSION(:), INTENT(OUT) :: PC
    REAL,    DIMENSION(:), INTENT(OUT) :: POD
    REAL,    DIMENSION(:), INTENT(OUT) :: FAR
    !
    !
    ! !REVISION HISTORY: 
    !  10 Nov 2012 - J. G. de Mattos - Initial Version
    ! 
    !
    ! !SEE ALSO:
    !
    !     * Nurmi (2003): Recommendations on the verification of local
    !                     weather forecasts. ECMWF Tech. Mem. 430.
    ! 
    !     * Wilks (2005): Statistical Methods in the Atmospheric
    !                     Sciences, Chapter 7 (Forecast Verification). Academic Press.
    !
    !
    !EOP
    !----------------------------------------------------------------!
    !BOC
    !----------------------------------------------------------------!
    ! misc variables

    real, allocatable, dimension(:) :: R
    integer :: t
    integer :: nt
!    integer :: iret

    character(len=*),parameter :: myname_=myname//'::Binary_Contingency_table'


    nt = min(size(A),size(B),size(C),size(D))

    Allocate(R(nt))


    DO t=1,nt

       !
       ! Computing each skill score
       !

       IF ((A(t) + B(t) + C(t) + D(t)) .eq. 0) THEN
          R(t)  = Undef
          PC(t) = Undef
       ELSE
          R(t)  = REAL(A(t) + B(t))*REAL(A(t) + C(t))/REAL(A(t) + B(t) + C(t) + D(t))
          PC(t) = REAL(A(t) + D(t)) / REAL(A(t) + B(t) + C(t) + D(t))
       ENDIF

       IF ((R(t) .eq. Undef) .OR. (A(t) + B(t) + C(t) - R(t)) .eq. 0 ) THEN
          ETS(t) = Undef
       ELSE
          ETS(t) = REAL(A(t) - R(t))/REAL(A(t) + B(t) + C(t) - R(t))
       ENDIF

       IF ((A(t)+C(t)) .eq. 0) THEN
          Bias(t) = Undef
          POD(t)  = Undef
       ELSE
          Bias(t) = REAL(A(t)+B(t))/REAL(A(t)+C(t))
          POD(t)  = REAL(A(t))/REAL(A(t)+C(t))
       ENDIF

       IF ((A(t)+B(t)) .eq. 0)THEN
          FAR(t) = Undef
       ELSE
          FAR(t) = REAL(B(t)) / REAL( A(t) + B(t) )
       ENDIF


    ENDDO

  end subroutine skill_score
  !
  !EOC
  !
  !------------------------------------------------------------------!

END MODULE SCAM_ctableMOD
