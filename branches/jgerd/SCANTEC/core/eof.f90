Subroutine eof(Fanom,Ndim,Fvar,Feof,Ieof,Jeof,Cut,Neof,Trace,Npts,Nflds,e,tempo_char)
USE m_string
USE SCAM_Utils
USE scamtec_module
Implicit None
!=====================================================================!
!  PARAMETERS
!
!  Fanom  [input] = field anomalies
!  Ndim   [input] = dimension of rows of fanom as declared in the
!                   calling program; ndim must be >= npts
!  Fvar  [output] = fraction of variance accounted for by EOFs
!  Feof  [output] = neof corresponding eigenvectors
!  Ieof   [input] = first dimension of feof as declared in the
!                   calling program
!  Jeof   [input] = dimension of fvar and second dimension of feof
!  Cut    [input] = determines how many EOFs should be computed;
!                   cut < 1.0: cut gives the relative variance
!                   explained by the computed EOFs;
!                   cut > 1.0: cut is the number of EOFs to be
!                   computed
!  Neof  [output] = number of eigenvalues computed according to the
!                   value of  cut
!  Trace [output] = trace of covariance matrix
!  Npts   [input] = number of (grid) points in input field
!  Nflds  [input] = number of fields in data set
!
!=====================================================================!
! Character Variables
      Character(len=3), Intent(In)  :: tempo_char !variavel para converter inteiro para char paulo dias
      Character(len=200)             :: nome_arq_saida ! variavel para o nome de arquivo de saida
 !=====================================================================!
! Integer Variables
      Integer, Parameter    :: Mflds = 301
      Integer, Parameter    :: Mdim  = Mflds * ( Mflds + 1 ) / 2
      Integer, Parameter    :: Meof  = 4
      Integer               :: I, J, K, JJ, M
      Integer               :: K1, K2
      Integer               :: Iwork(5*Mflds),Ifail(Mflds)
      Integer               :: Index, Ifld, Info, Meof1
      Integer, Intent(In)   :: Ndim
      Integer, Intent(In)   :: e !numeros de experimento paulo dias
      Integer, Intent(In)   :: Ieof, Jeof
      Integer, Intent(In)   :: Npts
      Integer, Intent(In)   :: Nflds
      Integer, Intent(Out)  :: Neof

!=====================================================================!
!Real Variables
      Real (Kind=4), Intent(In),  Dimension(Ndim, Nflds) :: Fanom
      Real (Kind=4), Intent(Out), Dimension(Ieof,Jeof)   :: Feof
      Real (Kind=4), Intent(Out), Dimension(Jeof)        :: Fvar
      Real (Kind=4)                         :: Cut
      Real (Kind=4), Intent(Out)            :: Trace
      Real (Kind=4)                         :: Expvar
      Real (Kind=4)                         :: Fcut, Fmeof
      Real (Kind=4)                         :: Eofnorm
      Real (Kind=4), Dimension(Meof,Mflds)  :: ExpCff
!=====================================================================!
!  Double precision for LAPACK routines
      Real (Kind=8), Dimension(Mflds,Meof) :: Eigvec
      Real (Kind=8), Dimension(Mdim)       :: Cov
      Real (Kind=8), Dimension(Mflds)      :: Lambda
      Real (Kind=8), Dimension(8*Mflds)    :: Work
      Real (Kind=8)                        :: Vl,Vu,Tol
      Real (Kind=8)                        :: Dlamch

!=====================================================================!

	
!  Check value of number of fields in data set (mflds):

      If ( mflds .lt. nflds ) Then
         write(*,*)'Oops, parameter  MFLDS  not big enough in  EOFT!'
         Stop
      EndIf
!=====================================================================!
!  Check value of  cut (number of EOFs to be computed):

      fmeof = meof + 0.01
      If ( cut .gt. fmeof ) Then
        
        write(*,*)'WARNING: cut > meof'
        write(*,*)'Only the first',meof,' eigenvalues will be computed'
        
        cut=float(meof)
      
      EndIf
!=====================================================================!
!  compute  [fanom (transposed) x fanom]  matrix (upper triangle)
	
      trace = 0.0
      
      Do K1 = 1,Nflds
         
         Do K2 = K1,Nflds
            
            Index      = K2 * ( K2 - 1 ) / 2 + K1
            Cov(Index) = 0d0
            
            Do I = 1,Npts
            
               Cov(Index) = Cov(Index) + Fanom(I,K1) * Fanom(I,K2)
               
            End Do
            
         End Do
         
         Trace = Trace + Cov(K1*(K1+1)/2)
      
      End Do
      
!      write(*,*)'trace =',trace
!=====================================================================!
!  find meof largest eigenvalues and corresponding eigenvectors

      write(*,*)'finding eigenvalues...'
      
      Tol = 2.d0 * Dlamch('S')      
	
      print*,"HERE",nflds,meof
      
      
      call dspevx('V','I','U',Nflds,Cov,Vl,Vu,Nflds-Meof+1,Nflds,Tol, &
                  Meof1,Lambda,Eigvec,Mflds,Work,Iwork,Ifail,Info)
                  
                  

!      print*,lambda
!=====================================================================!
!  find neof

      neof   = 0
      expvar = 0.
      
      If ( Cut .lt. 1. ) Then
      
         Fcut = 100. * Cut
         
         Do While ( ( Expvar .lt. Fcut ) .and. ( Neof .lt. Meof ) )
         
            Neof   = Neof + 1            
            Expvar = Expvar + Lambda(Meof-Neof+1) / Trace * 100.
            
         End Do
         
      Else
      
         Neof = Int(Cut+0.01)
         
         Do I = 1,Neof
         
            Expvar = Expvar + Lambda(Meof-I+1) / Trace * 100.
            
         End Do
      
      EndIf
      
      If ( Neof .gt. Jeof ) Print*,'Increase JEOF in call to EOFT1'
      
      Do I = 1,Neof
      
         Fvar(I) = Lambda(Meof-I+1) / Trace * 100.
!         Print*,"Fvar(",I,")",Fvar(I)
         
      End Do

!=====================================================================!
!  store  [fanom x eigenvectors]  (normalised) in array  feof
!  in descending order:

      Do I = 1,Neof
      
         Eofnorm = 0.
         
         Do K = 1,Npts
         
            Feof(K,I) = 0.
            
            Do J = 1,Nflds
            
               Feof(K,I) = Feof(K,I) + Fanom(K,J) * Eigvec(J,Meof-I+1)
               
            End Do
            
            Eofnorm = Eofnorm + Feof(K,I) * Feof(K,I)
            
         End Do
         
         Do K = 1,Npts
         
            Feof(K,I) = Feof(K,I) / Sqrt(Eofnorm)
            
         End Do
         
      End Do
      
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!    WRITING OUT THE EOF EXPANSION COEFFICIENTS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 !nome do arquivo de saida
      if(e .eq. 0)then
        nome_arq_saida=trim(scamtec%output_dir)//'/eofexpcoeffs'//'_'//tempo_char//'.txt'
      else
        nome_arq_saida=trim(scamtec%output_dir)//'/eofexpcoeffs'//Trim(Exper(e)%name)//'_'//tempo_char//'h'//'.txt'
      endif
      
      nome_arq_saida=trim(scamtec%output_dir)//'/eofexpcoeffs'//Trim(Exper(e)%name)//'.txt'

       !OPEN(120,FILE=nome_arq_saida,FORM='UNFORMATTED',ACCESS='DIRECT',RECL=NFLDS*NEOF*4)
       OPEN(120,FILE=nome_arq_saida,FORM='FORMATTED',ACCESS='SEQUENTIAL',POSITION='APPEND')


      DO JJ=1,NFLDS
      !Converting from REAL(KIND=8) to REAL(KIND=4)
         DO I=1,NEOF
            ExpCff(I,JJ)=EIGVEC(JJ,I)
            WRITE(6,"(2I6,2F20.6)") I,JJ,ExpCff(I,JJ),EIGVEC(JJ,I)
            WRITE(120,"(2I6,2F20.6)") I,JJ,ExpCff(I,JJ),EIGVEC(JJ,I)
         ENDDO
      ENDDO     
         
                  
      print*, 'Neof, Nflds: ', NEOF, NFLDS
      
      !WRITE(120,REC=1) ((ExpCff(I,J),I=1,NEOF),J=1,NFLDS)
      !WRITE(120,*) ((ExpCff(I,J),I=1,NEOF),J=1,NFLDS)
      !CLOSE(120)
      
!
!     WRITING OUT THE EOF PATTERNS
!
     
      ! nome do arquivo de saida
      if(e .eq. 0)then
        nome_arq_saida=trim(scamtec%output_dir)//'/eofpatterns'//'_'//tempo_char//'.bin'
      else
        nome_arq_saida=trim(scamtec%output_dir)//'/eofpatterns'//Trim(Exper(e)%name)//'_'//tempo_char//'h'//'.bin'
      endif
      
      nome_arq_saida=trim(scamtec%output_dir)//'/eofpatterns'//Trim(Exper(e)%name)//'.bin'
        
      OPEN(130,FILE=nome_arq_saida,FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
                   
      DO M=1,NEOF
         PRINT*,'NEOF=',M,'NPTS=',NPTS
         
         WRITE(130) FEOF(:,M)
      ENDDO
      
      !CLOSE(130)
      
!
!     WRITING OUT THE EOF CTL
!     
!    Obs. sera criado o arquivo .ctl para os parametros que estao no scamtec.conf      
!
      open(140, file=trim(scamtec%output_dir)//'/eofpatterns.ctl', status='unknown')
      
      write(140,'(A6,A150)')'dset ^',nome_arq_saida
      write(140,'(A)')
      write(140,'(A)')'options sequential'
      write(140,'(A)')      
      write(140,'(A)')'undef -999.9'      
      write(140,'(A)')      
      write(140,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      write(140,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      
      write(140,'(A)') 
      write(140,'(A)')'zdef  1 levels 1000'                  
      write(140,'(A)') 
      write(140,'(A,I3,A,I2,A)')'tdef  ',(scamtec%forecast_time/scamtec%atime_step)+1,' linear 00Z05AUG2014 ',scamtec%atime_step,'HR'
      write(140,'(A)')
      write(140,'(A)')'vars 4'
      write(140,'(A)')'eof1   1 99  EOF'
      write(140,'(A)')'eof2   1 99  EOF'
      write(140,'(A)')'eof3   1 99  EOF'
      write(140,'(A)')'eof4   1 99  EOF'
      write(140,'(A)')'endvars'   
      
      close(140)
      
      
      
      
RETURN	
End Subroutine
