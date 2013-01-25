Module SCAM_bstatistic
  ! Modulo contendo funcoes
  ! estatisticas basicas
  !
  !===================================

 
Contains

  
!=======================================
!  Subroutine corr(X,Y,Rho,masc)
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
    real :: tmpX, tmpY
    Integer      ::  NxPts, NyPts, i, j
    integer	:: noUndefXPts, noUndefYPts
    !

    	NxPts  = size(X,1)
    	NyPts  = size(X,2)

	tmpX = 0
	tmpY = 0
	noUndefXPts = 0
	noUndefYPts = 0

	do j = 1, nyPts
		do i = 1, nxPts
	
			if(X(i,j) .gt. -999.9000)then
				tmpX = tmpX + X(i,j)
				noUndefXPts = noUndefXPts + 1
			end if
			
			if(Y(i,j) .gt. -999.9000)then
				tmpY = tmpY + Y(i,j)
				noUndefYPts = noUndefYPts + 1
			end if
		end do
		
	end do

  !!  Mx  = sum(X)/float(NxPts*NyPts) !<<<
  !!  My  = sum(Y)/float(NxPts*NyPts) !<<

	if(tmpX .lt. 1. .or. tmpY .lt. 1.) return 
	mX = tmpX/noUndefXPts
	mY = tmpY/noUndefYPts

	Sxy = 0
	Sxx = 0
	Syy = 0


	do j = 1, nyPts
		do i = 1, nxPts
	
			if(X(i,j) .gt. -999.9000 .and. Y(i,j) .gt. -999.9000)	Sxy = Sxy + ( (X(i,j) - mX) * (Y(i,j) - mY) )
			if(X(i,j) .gt. -999.9000) 				Sxx = Sxx + ( (X(i,j) - mX) * (X(i,j) - mX) )
			if(Y(i,j) .gt. -999.9000)					Syy = Syy + ( (Y(i,j) - mY) * (Y(i,j) - mY) )

		end do
		
	end do


  !!  Sxy = sum( (X-Mx)*(Y-My) ) !<<
  !!  Sxx = sum( (X-Mx)*(X-Mx) ) !<<
  !!  Syy = sum( (Y-My)*(Y-MY) ) !<<
  
 
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
