Module SCAM_bstatistic
  ! Modulo contendo funcoes
  ! estatisticas basicas
  !
  !===================================

 
Contains

  
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
