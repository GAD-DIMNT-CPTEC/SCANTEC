!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: SCAM_ctableMOD.f90
!
! !DESCRIPTON: This module contains routines and functions to compute many
!              scores based on binary contingency table and multicategorical
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

  USE m_die     ! Error Messages
  USE m_stdio   ! Module to defines std. I/O parameters

  IMPLICIT NONE
  PRIVATE
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  !

  public :: binary_contingency_table

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
  ! !IROUTINE:  binary_contingency_table
  !
  ! !DESCRIPTION: Routine to compute some statistical index based on
  !               the Binary contingency table. Use thresholds to cut yes/no
  !               precipitation event.
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
  !\\
  !\\
  ! !INTERFACE:
  !

  subroutine Binary_Contingency_Table(Fct, Obs, thres, ETS, BIAS, PC, FAR, POD, Undef)
    implicit none

    !
    ! !INPUT PARAMETERS:
    !

    REAL, DIMENSION(:,:), INTENT(IN) :: Fct   ! Forecast Field [Nx,Ny]
    REAL, DIMENSION(:,:), INTENT(IN) :: Obs   ! Observation Field [Nx,Ny]
    REAL, DIMENSION(:)  , INTENT(IN) :: thres ! Threshold to cut rain event [Nthreshold]
    REAL                , INTENT(IN) :: Undef ! Undefined value

    !
    ! !OUTPUT PARAMETERS:
    !

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
    !

    ! A = Number of "yes" forecast, "yes" observation grid points.
    ! B = Number of "yes" forecast, "no" observation grid points.
    ! C = Number of "no" forecast, "yes" observation grid points.
    ! D = Number of "no" forecast, "no" observation grid points.
    ! R = Number of random correct forecasts expected due to chance within
    !     the total number of model and observation pairs.

    INTEGER, DIMENSION(:), ALLOCATABLE :: A 
    INTEGER, DIMENSION(:), ALLOCATABLE :: B 
    INTEGER, DIMENSION(:), ALLOCATABLE :: C 
    INTEGER, DIMENSION(:), ALLOCATABLE :: D
    REAL,    DIMENSION(:), ALLOCATABLE :: R

    !
    !----------------------------------------------------------------!
    ! misc variables

    integer :: t
    integer :: nt, ni, nj
    integer :: iret
    integer, allocatable, dimension(:,:) :: mask

    character(len=*),parameter :: myname_=myname//'::Binary_Contingency_table'

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
    nt = size(thres)

    ALLOCATE(mask(ni,nj))
    ALLOCATE(R(nt))

    !
    ! Creating mask to only compute contingency table for valid grid points
    !

    mask = 1
    where(Fct.eq.Undef.or.Obs.eq.Undef) mask = 0

    !
    ! loop over all thresolds
    !

    DO t=1,nt

       !
       ! Os thresholds sao pontos de corte para chuva e não chuva. Valores iguais ou
       ! superiores ao threshold são considerados chuva e computados como tal.
       !

       A(t) = count(fct>=Thres(t).and.Obs>=Thres(t).and.mask.eq.1)
       B(t) = count(fct>=Thres(t).and.Obs< Thres(t).and.mask.eq.1)
       C(t) = count(fct< Thres(t).and.Obs>=Thres(t).and.mask.eq.1)
       D(t) = count(fct< Thres(t).and.Obs< Thres(t).and.mask.eq.1)

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

       IF ((R(t) .eq. Undef) .OR. (A(t) + B(t) + C(t) - R(t)) == 0 ) THEN
          ETS(t) = Undef
       ELSE
          ETS(t) = REAL(A(t) - R(t))/REAL(A(t) + B(t) + C(t) - R(t))
       ENDIF

       IF ((A(t)+C(t)) == 0) THEN
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

    DEALLOCATE(mask,stat=iret)
    if(iret.ne.0) then
       call perr(myname_,'deallocate(mask)',iret)
       return
    endif

    DEALLOCATE(R,stat=iret)
    if(iret.ne.0) then
       call perr(myname_,'deallocate(R)',iret)
       return
    endif

  end subroutine Binary_Contingency_Table
  !
  !EOC
  !

  subroutine Multicategorical_Contingency_Table
     implicit none

  end subroutine Multicategorical_Contingency_Table


END MODULE SCAM_ctableMOD
