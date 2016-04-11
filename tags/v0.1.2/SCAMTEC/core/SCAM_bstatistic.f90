!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: SCAM_bstatistic.f90
!
! !DESCRIPTON:
!             
!                 
!\\
!\\
! !INTERFACE:
!


Module SCAM_bstatistic

!
! !USES:
!

  USE scamtec_module                ! SCAMTEC types
  USE SCAM_dataMOD, only : scamdata ! SCAMTEC data matrix
  USE m_die                         ! Error Messages
  USE m_stdio                       ! Module to defines std. I/O parameters

  IMPLICIT NONE
  PRIVATE
!
! !PARAMETERS:
!

  character(len=*),parameter :: myname='SCAM_bstatistic' 

!
! !PUBLIC MEMBER FUNCTIONS:
!

  public :: CalcBstat
  public :: Corr

!
! !REVISION HISTORY:
!  09 OCT 2011 - J. G. de Mattos - Initial Version
!
! !SEE ALSO:
!   
!
!EOP
!-----------------------------------------------------------------------------!
 
Contains

!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  CalcBstat
!
! !DESCRIPTION: This rotine ....
!
!\\
!\\
! !INTERFACE:
!

  Subroutine CalcBstat( run )

!
!
! !REVISION HISTORY: 
!  19 October 2012 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!

      Implicit None
      integer, intent(in) :: run
      real                :: tmp
      integer :: e, v, clima_flag, f

      character(len=*),parameter :: myname_=myname//'::CalcBstat'

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif
               e=run
               v=1
               f=1
               clima_flag=1
!
             ! Bias
             !
             scamdata(e)%diffield(:,:,v) = scamdata(e)%expfield(:,:,v) - scamdata(1)%reffield(:,:,v)
             scamdata(e)%time_vies(f,v)  = scamdata(e)%time_vies(f,v) + &
                                        (sum(scamdata(e)%diffield(:,:,v))/scamtec%npts)/scamtec%ntime_steps

!             OutFName = "B"//Exper(e)%name//"%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2.gs4r"
!             CALL str_template(OutFName, fymd, fhms, nymd, nhms)
!             CALL write_2d(scamdata(e)%diffield,OutFName)

             !
             ! RMSE
             !
             scamdata(e)%rmsfield(:,:,v) = scamdata(e)%diffield(:,:,v)*scamdata(e)%diffield(:,:,v)
             scamdata(e)%time_rmse(f,v)  = scamdata(e)%time_rmse(f,v) + &
                                        (sum(scamdata(e)%rmsfield(:,:,v))/scamtec%npts)/scamtec%ntime_steps

!             OutFName = "R"//Exper(e)%name//"%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2.gs4r"
!             CALL str_template(OutFName, fymd, fhms, nymd, nhms)
!             CALL write_2d(scamdata(e)%diffield,OutFName)
 
             !
             ! Anomaly Correlation
             !

             if(Clima_Flag.eq.1)then

!             OutFName = "A"//Exper(e)%name//"%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2.gs4r"
!             CALL str_template(OutFName, fymd, fhms, nymd, nhms)
!             CALL write_2d(scamdata(e)%anofield,OutFName)


                CALL corr(scamdata(e)%expfield(:,:,v)-scamdata(1)%clmfield(:,:,v),&
                          scamdata(1)%reffield(:,:,v)-scamdata(1)%clmfield(:,:,v),&
                          tmp)
                scamdata(e)%time_acor(f,v) = scamdata(e)%time_acor(f,v) + &
                                          tmp/float(scamtec%ntime_steps)
             else
                CALL corr(scamdata(e)%expfield(:,:,v),&
                          scamdata(1)%reffield(:,:,v),&
                          tmp)
                scamdata(e)%time_acor(f,v) = scamdata(e)%time_acor(f,v) + &
                                          tmp/float(scamtec%ntime_steps)

             endif


  End Subroutine

  
!=======================================
  Subroutine corr(X,Y,Rho)
    !
    ! Calcula a correlacao entre os
    ! campos de anomalia X e Y.
    ! Retorna o valor na variavel Rho
    !
    ! X [Input]    = Left anomaly Field       => X(xp,yp)
    ! Y [Input]    = Right anomaly Field      => Y(xp,yp)
    ! Rho [OutPut] = Correlation
    !=======================================

    Implicit None

    Real, Intent(In), Dimension(:,:) :: X
    Real, Intent(In), Dimension(:,:) :: Y
    Real, Intent(Out) :: Rho
    !
    Real :: Sxy, Sxx, Syy
    Real :: Mx, My
    Integer      ::  NxPts, NyPts
    !

    NxPts  = size(X,1)
    NyPts  = size(X,2)

    Mx  = sum(X)/float(NxPts*NyPts)
    My  = sum(Y)/float(NxPts*NyPts)

    Sxy = sum( (X-Mx)*(Y-My) )
    Sxx = sum( (X-Mx)*(X-Mx) )
    Syy = sum( (Y-My)*(Y-MY) )
    Rho =  Sxy/sqrt(Sxx*Syy)
 
    return

  End Subroutine corr

  

  !======================================================
  Subroutine stdize(Z,Zmean,Zsdev,Zanom)
    !
    ! Rotina para calcular, media, desvio padrao e anomalia
    ! da variavel Z.
    !
    ! Z [Input]      = Variável utilizada.  => Z(Pontos, Tempo) 
    ! Zmean [Output] = Media temporal de Z. => Zmean(Pontos)
    ! Zsdev [OutPut] = Desvio Padrao de Z.  => Zsdev(Pontos)
    ! Zanom [OutPut] = Anomalia de Z.       => Zanom(Pontos, Tempo)
    !======================================================!
    !
    Real, Intent(In),  Dimension(:,:) :: Z
    Real, Intent(Out), Dimension(:,:) :: Zanom
    Real, Intent(Out), Dimension(:)   :: Zmean, Zsdev
    !
    Integer :: Npts, NRec
    Integer :: K
    !

    Npts = size(Z,1)
    NRec = size(Z,2)


    Do K = 1,Npts

       Zmean(K) = Sum(Z(K,1:NRec))/float(NRec)

       Zanom(K,1:NRec) = Z(K,1:NRec) - Zmean(K)

       Zsdev(K) = Sqrt( Sum(Zanom(K,1:NRec)**2)/float(NRec-1) )

       If ( Zsdev(K) .lt. 1.e-8 ) Then

          Print*,'WARNING:  standard deviation very small, ', Zsdev(K)

          Zanom(k,1:NRec) = Zanom(k,1:NRec)

       Else

          Zanom(k,1:NRec) = Zanom(k,1:NRec)/Zsdev(k)

       EndIf

    End Do

    Return

  End Subroutine stdize

  
End Module SCAM_bstatistic
