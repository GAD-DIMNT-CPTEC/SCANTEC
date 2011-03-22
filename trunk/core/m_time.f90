!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: m_time.f90
!
! !DESCRIPTON: This module contains routines and functions to manipulate time
!              periods, e.g, functions to calculate total number of hours, days, 
!              months and years between two dates, also contains routines to
!              convert julian days to gregorian day and vice and versa.
!\\
!\\
! !INTERFACE:
!
MODULE time_module
  IMPLICIT NONE
  PRIVATE
! !PUBLIC MEMBER FUNCTIONS:

  PUBLIC  :: cal2jul ! Convert from gregorian to julian day
  PUBLIC  :: jul2cal ! Convert fron julian to gregorian day
  PUBLIC  :: eom     ! Calculate end of month
  PUBLIC  :: noh     ! Calculate number of hours between two dates
  PUBLIC  :: nod     ! Calculate number of days between two dates
  PUBLIC  :: nom     ! Calculate number of months between two dates
  PUBLIC  :: noy     ! Calculate number of years between two dates

! !REVISION HISTORY:
! 15 Jun 2005 - J. G. de Mattos - Initial Version
! 18 Mar 2010 - J. G. de Mattos - Include Time calculation:
!                               - End day of Month [eom]
!                               - Number of hours [noh]
!                               - Number of days [nod]
!                               - Number of months [nom]
!                               - Number of year [moy]
! !REMARKS:
! 
!
!EOP
!-----------------------------------------------------------------------------!
!
CONTAINS
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  eom
!
! !DESCRIPTION: This function calculate the end day of month.
!\\
!\\
! !INTERFACE:
!
  FUNCTION eom(year,month) RESULT(day)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
    INTEGER, INTENT(IN)  :: year
    INTEGER, INTENT(IN)  :: month
!
! !OUTPUT PARAMETERS:
    INTEGER :: day
!
!
! !REVISION HISTORY: 
!  18 Mar 2010 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!
    INTEGER, PARAMETER, DIMENSION(12) :: dpm = (/31,28,31,30,31,30,31,31,30,31,30,31/)

    day = dpm(month)
    IF( month.EQ.2 )THEN
      IF ( (MOD(year,4).EQ.0 .AND. MOD(year,100).NE.0).or.(MOD(year,400).EQ.0) ) day = 29
    ENDIF

  END FUNCTION
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  noh
!
! !DESCRIPTION: This function calculate the total number of hours between two
!               dates.
!\\
!\\
! !INTERFACE:
!
  FUNCTION noh(di,df) RESULT(Nhour)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
    INTEGER, INTENT(IN)  :: di ! Starting Date
    INTEGER, INTENT(IN)  :: df ! Ending Date
!
! !OUTPUT PARAMETERS:
!
    INTEGER :: Nhour
!
!
! !REVISION HISTORY: 
!  18 Mar 2010 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!

    Nhour   =  ABS( CAL2JUL(df) - CAL2JUL(di) ) * 24

  END FUNCTION
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  nod
!
! !DESCRIPTION: This function calculate the total number of days between two
!               dates.
!\\
!\\
! !INTERFACE:
!
  FUNCTION nod(di,df) RESULT(Nday)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
    INTEGER, INTENT(IN)  :: di ! Starting Date
    INTEGER, INTENT(IN)  :: df ! Ending Date
!
! !OUTPUT PARAMETERS:
!
    INTEGER :: Nday
!
!
! !REVISION HISTORY: 
!  18 Mar 2010 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!

    Nday   =  ABS(CAL2JUL(df) - CAL2JUL(di)) + 1

  END FUNCTION
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  nom
!
! !DESCRIPTION: This function calculate the total number of months between two
!               dates.
!\\
!\\
! !INTERFACE:
!
  FUNCTION nom(di,df) RESULT(Nmonth)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
    INTEGER, INTENT(IN)  :: di ! Starting Date
    INTEGER, INTENT(IN)  :: df ! Ending Date
!
! !OUTPUT PARAMETERS:
!
    INTEGER :: Nmonth
!
!
! !REVISION HISTORY: 
!  18 Mar 2010 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!
    INTEGER  :: i, f

    i = INT( MOD(di,1000000)/10000 )
    f = INT( MOD(df,1000000)/10000 )

    Nmonth =  ( INT( (df - di) / 1000000 ) * 12 ) + (f-i+1)

  END FUNCTION
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  noy
!
! !DESCRIPTION: This function calculate the total number of Years between two
!               dates.
!\\
!\\
! !INTERFACE:
!
  FUNCTION noy(di,df) RESULT(Nyear)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
    INTEGER, INTENT(IN)  :: di ! Starting Date
    INTEGER, INTENT(IN)  :: df ! Ending Date
!
! !OUTPUT PARAMETERS:
!
    INTEGER :: Nyear
!
!
! !REVISION HISTORY: 
!  18 Mar 2010 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!
    Nyear  =   INT( ABS(df - di) / 1000000 )

  END FUNCTION
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  Cal2Jul
!
! !DESCRIPTION: This function calculate the julian day from gregorian day
!
!\\
!\\
! !INTERFACE:
!
  FUNCTION cal2jul(CalDate) RESULT(julian)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
    INTEGER, INTENT(IN) :: CalDate
!
! !OUTPUT PARAMETERS:
!
    REAL(kind=8)  :: julian
!
!
! !REVISION HISTORY: 
!  15 Jun 2005 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!

    REAL(kind=8)  :: Ano, Mes, Dia, Hora
    REAL(kind=8)  :: A, B, C, D, E
    

    Ano  =       CalDate / 1000000
    Mes  = MOD ( CalDate,  1000000 ) / 10000
    Dia  = MOD ( CalDate,    10000 ) /100
    Hora = MOD ( CalDate,      100 )

    IF(Mes < 3)THEN
       Ano=Ano-1
       Mes=Mes+12
    ENDIF

    IF(CalDate>=15821015)THEN
       A = INT(Ano/100)
       B = INT(A/4)
       C = 2 - A + B
    ENDIF

    IF(CalDate<=15821004)THEN
       C = 0
    ENDIF

    D = INT(365.25 * (Ano + 4716))
    E = INT(30.6001 * (Mes + 1))

    julian = INT(D + E + Dia + 0.5 + C - 1524.5)+ (Hora / 24.0)

    RETURN

  END FUNCTION Cal2Jul
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  Cal2Jul
!
! !DESCRIPTION: This function calculate the gregorian date from julian day.
!               
!\\
!\\
! !INTERFACE:
!
  FUNCTION jul2cal(jd) RESULT(gregorian)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
	 REAL (kind=8), INTENT(IN) :: jd

!
! !OUTPUT PARAMETERS:
!
   INTEGER(kind=8) :: gregorian
!
!
! !REVISION HISTORY: 
!  15 Jun 2005 - J. G. de Mattos - Initial Version
!
! !REMARKS:
!        This algorithm was adopted from Press et al.
!EOP
!-----------------------------------------------------------------------------!
!BOC
!
    INTEGER (kind=4), PARAMETER :: Gregjd = 2299161
    INTEGER                     ::  j1, j2, j3, j4, j5
    INTEGER (kind=4)            ::  Ano
    INTEGER (kind=4)            ::  Mes
    INTEGER (kind=4)            ::  Dia
    INTEGER (kind=4)            ::  Hora
    INTEGER (kind=4)            ::  Min
    INTEGER (kind=4)            ::  Seg
    INTEGER (kind=4)            ::  Intgr
    INTEGER (kind=4)            ::  f, tmp

    REAL (kind=4)               ::  dayfrac, frac

    !       
    ! get the date from the Julian day number
    !       
    ! jd=2453372.25

    intgr   = INT(jd)
    frac    = jd - intgr

    IF( intgr >= gregjd )THEN              !Gregorian calendar correction
       tmp = INT( ( (intgr - 1867216) - 0.25 ) / 36524.25 )
       j1  = intgr + 1 + tmp - INT(0.25*tmp)
    ELSE
       j1 = intgr
    ENDIF
    !       correction for half day offset
    dayfrac = frac + 0.0
    IF( dayfrac >= 1.0 )THEN
       dayfrac = dayfrac - 1.0
       j1 = j1+1
    ENDIF


    j2 = j1 + 1524
    j3 = INT( 6680.0 + ( (j2 - 2439870) - 122.1 )/365.25 )
    j4 = INT(j3*365.25)
    j5 = INT( (j2 - j4)/30.6001 )

    dia = INT(j2 - j4 - INT(j5*30.6001))
    mes = INT(j5 - 1)
    IF( mes > 12 ) mes = mes- 12
    Ano = INT(j3 - 4715)
    IF( mes > 2 )  Ano = Ano - 1
    IF( Ano <= 0 ) Ano = Ano - 1

    !
    ! get time of day from day fraction
    !
    hora = INT(dayfrac * 24.0)
    min  = INT((dayfrac*24.0 - hora)*60.0)
    f    = INT( ((dayfrac*24.0 - hora)*60.0 - min)*60.0)
    seg  = INT(f)

    f = f - seg
    IF( f > 0.5 ) seg = seg +1

    IF( seg == 60 )THEN
       seg = 0
       min = min + 1
    ENDIF

    IF( min == 60 )THEN
       min = 0
       hora = hora +1
    ENDIF

    IF( hora == 24 )THEN

       hora = 0
       !
       ! this could cause a bug, but probably 
       ! will never happen in practice
       !
       dia = dia + 1

    ENDIF

    IF( Ano < 0 )THEN
       Ano = -Ano
    ENDIF

    gregorian=(Ano*1000000)+(Mes*10000)+(Dia*100)+Hora

    RETURN

  END FUNCTION Jul2Cal


END MODULE time_module
