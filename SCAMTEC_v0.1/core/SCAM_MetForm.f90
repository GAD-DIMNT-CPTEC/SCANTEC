!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: SCAM_MetForm.f90
!
! !DESCRIPTON: Este modulo tem por proposito apresentar rotinas e funcoes para 
!              a conversão de variaveis meteorologicas.
!                 
!\\
!\\
! !INTERFACE:
!

Module SCAM_MetForm

  IMPLICIT NONE
  PRIVATE

  public :: es
  public :: ee
  public :: w
  public :: q
  public :: rh
  public :: td
  public :: tv

  interface q
     module procedure q1,& ! utiliza somente razao de mistura q(w)
                      q2,& ! utiliza td e pressao q(p,td)
                      q3   ! utiliza pressao, temperatura e umidade relativa q(p,t,rh)
  end interface q

  interface w
     module procedure w1,& ! utiliza pressão de vapor e pressao atmosferica w(p,e)
                      w2   ! utiliza umidade especifica w(q)
  end interface w

  interface tv
     module procedure tv1,& ! utiliza temperatura do ar [C] e Umidade Especifica [kg/kg] tv(t,q)
                      tv2   ! utiliza temperatura do ar [C], umidade relativa [%] e pressao atm [Pa] tv(t,rh,p)
  end interface tv


  !
  ! !PARAMETERS:
  !

  real, parameter  :: p0 = 1000.00 ! pressão de referencia
  real, parameter  :: Rd =  287.04 ! Constante dos Gases para o ar seco [ Joutes/K/kg]
  real, parameter  :: Rv =  461.50 ! Constante dos Gases para o Vapor d'água [ Joutes/K/kg]
  real, parameter  :: cp = 1003.50 ! Calor especifico para o ar seco [Joules/K/kg]
  real, parameter  :: L  = 2257e03 ! Calor latente de vaporização [Joules/kg]

  integer, parameter :: stdout = 6
  character(len=1024),parameter :: myname='SCAM_MetForm'



  !
  ! !REVISION HISTORY:
  !  19 FebT 2013 - J. G. de Mattos - Initial Version
  !
  ! !SEE ALSO:
  !   
  ! !BUGS:
  !
  !  Nenhum conhecido
  !
  !EOP
  !-----------------------------------------------------------------------------!

Contains

  !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  es
  !
  ! !DESCRIPTION: Esta funcao calcula a pressão de vapor de saturacao [Pa]
  !
  !\\
  !\\
  ! !INTERFACE:

  function es(temp)
    implicit none
    !
    ! !INPUT PARAMETERS:
    !

    real, intent(in) :: temp ! Temperatura do ar [Graus Celsius]

    !
    ! !OUTPUT PARAMETERS:
    !

    real :: es ! pressão de vapor saturado [Pa]

    !
    !
    ! !REVISION HISTORY: 
    !  19 Feb 2013 - J. G. de Mattos - Initial Version
    !
    ! !SEE ALSO: 
    !     Bolton, D., The computation of equivalent potential temperature, 
    !                  Monthly Weather Review, 108, 1046-1053, 1980.. 
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !

    character(len=1024),parameter :: myname_=trim(myname)//'::es'

    es = 611.2*exp((17.67 * temp)/(temp+243.5))

  end function es
  !EOC
  !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  ee
  !
  ! !DESCRIPTION: Esta função calcula a pressão de vapor [Pa]
  !
  !\\
  !\\
  ! !INTERFACE:
  function ee(es, rh)
    implicit none

    !
    ! !INPUT PARAMETERS:
    !

    real, intent(in) :: es ! Pressão de vapor saturado [Pa]
    real, intent(in) :: rh ! Umidade relativa [%]

    !
    ! !OUTPUT PARAMETERS:
    !

    real :: ee ! pressao de vapor [Pa]

    !
    !
    ! !REVISION HISTORY: 
    !  19 Feb 2013 - J. G. de Mattos - Initial Version
    !
    ! !SEE ALSO: 
    !     Huschke, Glossary of Meteorology, 1959, p. 477
    !      
    !     
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !
    character(len=1024),parameter :: myname_=trim(myname)//'::ee'

    ee = ( rh / 100.0 ) * es

  end function ee
  !EOC
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  w
  !
  ! !DESCRIPTION: Esta funcao calcula a razao de mistura [-]
  !               
  !
  !\\
  !\\
  ! !INTERFACE:

  function w1(pres, ee)
    implicit none

    !
    ! !INPUT PARAMETERS:
    !

    real, intent(in) :: pres  ! pressão atmosferica [Pa]
    real, intent(in) :: ee    ! pressao de vapor [Pa]

    !
    ! !OUTPUT PARAMETERS:
    !

    real :: w1 ! razão de mistura [-]

    !
    !
    ! !REVISION HISTORY: 
    !  19 Feb 2013 - J. G. de Mattos - Initial Version
    !
    ! !SEE ALSO: 
    !     Wallace & Hobbs, Atmospheric Science, 1977, Academic Press 
    !
    !     
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !

    real :: eps

    character(len=1024),parameter :: myname_=trim(myname)//'::w1'

    eps = Rd / Rv

    w1 = eps * ( ee / (pres - ee) ) 

  end function w1
  !EOC
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  w2
  !
  ! !DESCRIPTION: Esta rotina calcula a razao de mistura [-] a partir da umidade
  !               especifica [kg/kg].
  !
  !\\
  !\\
  ! !INTERFACE:

  function w2(q)

    Implicit None
    !
    ! !INPUT PARAMETERS:
    !
    real, intent(in) :: q ! Umidade especifica [kg/kg]
    !
    ! !OUTPUT PARAMETERS:
    !

    real :: w2 ! Razão de Mistura [kg/kg]

    !
    !
    ! !REVISION HISTORY: 
    !  19 Feb 2013 - J. G. de Mattos - Initial Version
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !

    character(len=1024),parameter :: myname_=trim(myname)//'::w2'

    w2 = (q / ( 1 - q ) )

  end function w2
  !EOC
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  q1
  !
  ! !DESCRIPTION: Esta funcao calcula a umidade especifica em kg/kg 
  !               
  !
  !\\
  !\\
  ! !INTERFACE:

  function q1(w)
    implicit none

    !
    ! !INPUT PARAMETERS:
    !

    real, intent(in) :: w ! razao de mistura em kg/kg

    !
    ! !OUTPUT PARAMETERS:
    ! 

    real :: q1 ! Umidade especifica em kg/kg

    !
    !
    ! !REVISION HISTORY: 
    !  19 Feb 2013 - J. G. de Mattos - Initial Version
    !
    ! !SEE ALSO: 
    !     Huschke, Glossary of Meteorology, 1959, p. 530
    !      
    !     
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !
    character(len=1024),parameter :: myname_=trim(myname)//'::q1'

    q1 = w / (1.0 + w)
    !     q1 = q1 * 1000.0

  end function q1
  !EOC
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  q2
  !
  ! !DESCRIPTION: Esta funcao calcula a umidade especifica em kg/kg 
  !               
  !
  !\\
  !\\
  ! !INTERFACE:

  function q2(pres,td)
    implicit none

    !
    ! !INPUT PARAMETERS:
    !

    real, intent(in) :: pres ! Pressao Atmosférica [Pa]
    real, intent(in) :: td   ! Tempeatura do Ponto de Orvalho [C]

    !
    ! !OUTPUT PARAMETERS:
    ! 

    real :: q2 ! Umidade especifica em g/kg

    !
    !
    ! !REVISION HISTORY: 
    !  19 Feb 2013 - J. G. de Mattos - Initial Version
    !
    ! !SEE ALSO: 
    !     Bolton, D., The computation of equivalent potential temperature, 
    !                  Monthly Weather Review, 108, 1046-1053, 1980. 
    !      
    !     
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !

    real :: e

    character(len=1024),parameter :: myname_=trim(myname)//'::q2'

    e  = 611.20*exp((17.67*Td)/(Td + 243.5)); 
    q2 = (0.622 * e)/(pres - (0.378 * e)); 
    !     q2 = q2 * 1000.0

  end function q2
  !EOC
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  q3
  !
  ! !DESCRIPTION: Esta funcao calcula a umidade especifica em kg/kg 
  !               
  !
  !\\
  !\\
  ! !INTERFACE:

  function q3(pres,t,rh)
    implicit none

    !
    ! !INPUT PARAMETERS:
    !

    real, intent(in) :: pres ! Pressao Atmosférica [Pa]
    real, intent(in) :: t    ! Tempeatura do Ar [C]
    real, intent(in) :: rh   ! Umidade Relativa [%]


    !
    ! !OUTPUT PARAMETERS:
    ! 

    real :: q3 ! Umidade especifica em g/kg

    !
    !
    ! !REVISION HISTORY: 
    !  19 Feb 2013 - J. G. de Mattos - Initial Version
    !
    ! !SEE ALSO: 
    !     Bolton, D., The computation of equivalent potential temperature, 
    !                  Monthly Weather Review, 108, 1046-1053, 1980. 
    !      
    !     
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !

    real :: eps
    real :: es
    real :: ee
    real :: w

    character(len=1024),parameter :: myname_=trim(myname)//'::q3'

    eps = Rd / Rv
    es  = 611.20 * exp((17.67 * t)/(t + 243.5));
    ee  = es * ( rh / 100.0 );
    eps = Rd / Rv
    w   = eps * ( ee / (pres - ee) ) 
    q3 = w / (1.0 + w)

  end function q3
  !EOC

  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  td
  !
  ! !DESCRIPTION: Esta funcao calcula a Temperatura do Ponto de Orvalho [C] 
  !               
  !
  !\\
  !\\
  ! !INTERFACE:

  function td(t,rh)
    implicit none

    !
    ! !INPUT PARAMETERS:
    !

    real, intent(in) :: rh ! Umidade Relativa [%]
    real, intent(in) :: t   ! Tempeatura do Ar [C]

    !
    ! !OUTPUT PARAMETERS:
    ! 

    real :: td ! Temperatura do Ponto de Orvalho [C]

    !
    !
    ! !REVISION HISTORY: 
    !  19 Feb 2013 - J. G. de Mattos - Initial Version
    !
    ! !SEE ALSO: 
    !     Bolton, D., The computation of equivalent potential temperature, 
    !                  Monthly Weather Review, 108, 1046-1053, 1980. 
    !      
    !     
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !

    real :: e, es

    character(len=1024),parameter :: myname_=trim(myname)//'::td'

    es = 6.112 * exp((17.67 * t)/(t + 243.5)); 
    e = es * ( rh / 100.0 ); 
    Td = log(e/6.112)*243.5/(17.67-log(e/6.112)); 

  end function td
  !EOC
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  rh
  !
  ! !DESCRIPTION: Esta função calcula a umidade relativa [-] a partir da
  !               razão de mistura [-], da pressao de vapor de saturacao [hPa] e
  !               da pressão atmosferica [hPa].
  !\\
  !\\
  ! !INTERFACE:


  function rh(w,es,pres)
    Implicit None

    !
    ! !INPUT PARAMETERS:
    !

    real, intent(in) :: w    ! Razão de Mistura [-]
    real, intent(in) :: es   ! Presão de Vapor Saturada [Pa]
    real, intent(in) :: pres ! Presão Atmosférica [Pa]

    !
    ! !OUTPUT PARAMETERS:
    !

    real :: rh  ! Umidade Relativa [-]

    !
    !
    ! !REVISION HISTORY: 
    !  19 Feb 2013 - J. G. de Mattos - Initial Version
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !

    REAL :: ws, eps

    character(len=1024),parameter :: myname_=trim(myname)//'::rh'

    eps = Rd / Rv
    ws  = (eps*es) / (pres - es);
    rh  = (w/ws) * 100.0

    !  mask to rh > 100 %
    IF(rh .GT. 100.0) rh = 100.0

  end function rh
  !EOC
  !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  Tv1
  !
  ! !DESCRIPTION: Esta função calcula a temperatura virtual [C ou K] a partir da
  !               temperatura do ar [C ou K] e da Umidade especícia [Kg/Kg]
  !
  !\\
  !\\
  ! !INTERFACE:

  function tv1(t,q)

    Implicit None

    !
    ! !INPUT PARAMETERS:
    !

    real, intent(in) :: t ! temperatura do ar [C ou K]
    real, intent(in) :: q ! Umidade Específica [kg/Kg]

    !
    ! !OUTPUT PARAMETERS:
    !

    real :: tv1 ! temperatura virtural [C ou K]

    !
    !
    ! !REVISION HISTORY: 
    !  19 Feb 2013 - J. G. de Mattos - Initial Version
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !

    character(len=1024),parameter :: myname_=trim(myname)//'::tv1'

    tv1 = t * ( 1 + 0.61 * ( q / ( 1 - q ) ) ) 

  end function tv1
  !EOC
  !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  Tv2
  !
  ! !DESCRIPTION: Esta função calcula a temperatura virtual [C] a partir da
  !               temperatura do ar [C] e da Umidade Relativa [%] e da Pressao
  !               atmosferica [Pa]
  !
  !\\
  !\\
  ! !INTERFACE:

  function tv2(t,rh,pres)

    Implicit None

    !
    ! !INPUT PARAMETERS:
    !

    real, intent(in) :: t    ! temperatura do ar [C]
    real, intent(in) :: rh   ! Umidade Relativa [%]
    real, intent(in) :: pres ! Pressão Atmosferica [Pa]

    !
    ! !OUTPUT PARAMETERS:
    !

    real :: tv2 ! temperatura virtural [C]

    !
    !
    ! !REVISION HISTORY: 
    !  19 Feb 2013 - J. G. de Mattos - Initial Version
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !

    real :: eps
    real :: es, ee
    real :: w

    character(len=1024),parameter :: myname_=trim(myname)//'::tv2'

    eps = Rd / Rv
    es  = 611.2*exp((17.67 * t)/(t+243.5))
    ee  = ( rh / 100.0 ) * es
    w   = eps * ( ee / (pres - ee) )

    Tv2 = t * (1 + w / eps) / (1 + w)


  end function tv2
  !EOC
  !
  !-----------------------------------------------------------------------------!

End Module SCAM_MetForm
