!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: TimeMod.f90
!
! !DESCRIPTION: This module contains routines and functions to manipulate time
!              periods, e.g, functions to calculate total number of hours, days, 
!              months and years between two dates, also contains routines to
!              convert julian days to gregorian day and vice and versa.
!
!
! !INTERFACE:
!
MODULE SCAM_TimeMod
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
  PUBLIC  :: doy     ! Calculate the day of year

! !REVISION HISTORY:
! 15 Jun 2005 - J. G. de Mattos - Initial Version
! 18 Mar 2010 - J. G. de Mattos - Include Time calculation:
!                               - End day of Month [eom]
!                               - Number of hours [noh]
!                               - Number of days [nod]
!                               - Number of months [nom]
!                               - Number of year [moy]
! 23 Mar 2010 - J. G. de Mattos - Modified the call for Cal2Jul routine
!                                 was created the interface block for
!                                 use of this new Cal2Jul
! 09 May 2013 - J. G. de Mattos - Removed Bug in Number of Hours 
! 05 Feb 2014 - J. G. de Mattos - Include day of year calculation
!
! !SEE ALSO: 
! 
!
!EOP
!-----------------------------------------------------------------------------!
!

!
! KINDS
!

!   INTEGER, PUBLIC, PARAMETER :: I4B = SELECTED_INT_KIND(9)
!   INTEGER, PUBLIC, PARAMETER :: I2B = SELECTED_INT_KIND(4)
!   INTEGER, PUBLIC, PARAMETER :: I1B = SELECTED_INT_KIND(2)
!   INTEGER, PUBLIC, PARAMETER :: R4B  = KIND(1.0)
!   INTEGER, PUBLIC, PARAMETER :: R8B  = KIND(1.0D0)


   INTERFACE Cal2Jul
      MODULE PROCEDURE cal2jul_, cal2jul__
   END INTERFACE Cal2Jul

!   INTERFACE Jul2Cal
!      MODULE PROCEDURE jul2cal_, jul2cal__
!   END INTERFACE Jul2Cal

CONTAINS
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  eom
!
! !DESCRIPTION: This function calculate the end day of month.
!
!
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
!
!
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
    INTEGER(kind=8) :: Nhour
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

    Nhour   =  nint(mod(CAL2JUL(df),CAL2JUL(di)) * 24.0)

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
!
!
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
!
!
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
!
!
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
!***************************************************************
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  doy
!
! !DESCRIPTION: This function calculate the day of the year
!
!
! !INTERFACE:
!
  FUNCTION doy(nymd, nhms) RESULT(Day)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
    INTEGER, INTENT(IN)  :: nymd ! year month day (yyyymmdd)  
    INTEGER, INTENT(IN)  :: nhms ! hour minute second (hhmnsd)
!
! !OUTPUT PARAMETERS:
!
    REAL(kind=8) :: Day
!
!
! !REVISION HISTORY: 
!  05 Feb 2014 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!

    INTEGER  :: year

    INTEGER  :: ymd, hms

    year   = INT ( nymd / 10000 )

    ymd = (year*10000) + 101
    hms = 0

    Day = int(cal2jul__(nymd,hms) - cal2jul__(ymd,hms)) + 1
    Day = Day + (cal2jul__(nymd,nhms) - cal2jul__(nymd,hms) )


  END FUNCTION
!
!EOC
!
!*****************************************************8888
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  Cal2Jul__
!
! !DESCRIPTION: This function calculate the julian day from gregorian day
!
!
! !INTERFACE:
!
  FUNCTION cal2jul_(CalDate) RESULT(julian)
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
  INTEGER :: nymd
  INTEGER :: nhms

  nymd = CalDate/100
  nhms = MOD(CalDate,100) * 10000

  julian = cal2jul__(nymd,nhms)

  RETURN

  END FUNCTION
!EOC
!-----------------------------------------------------------------------------!
!
!BOP
!
! !IROUTINE:  Cal2Jul__
!
! !DESCRIPTION: This function calculate the julian day from gregorian day
!
!
! !INTERFACE:
!
  FUNCTION cal2jul__(ymd,hms) RESULT(julian)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
    INTEGER, INTENT(IN) :: ymd
    INTEGER, INTENT(IN) :: hms
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

    REAL(kind=8)  :: year, month, day
    REAL(kind=8)  :: hour, minute, second
    REAL(kind=8)  :: A, B, C, D, E
    

    year   = INT ( ymd / 10000 )
    month  = MOD ( ymd,  10000 ) / 100
    day    = MOD ( ymd,    100 )
    hour   = INT ( hms / 10000 )
    minute = MOD ( hms,  10000 ) / 100
    second = MOD ( hms,    100 )

    IF(month < 3)THEN
       year=year-1
       month=month+12
    ENDIF

    IF(ymd>=15821015)THEN
       A = INT(year/100)
       B = INT(A/4)
       C = 2 - A + B
    ENDIF

    IF(ymd<=15821004)THEN
       C = 0
    ENDIF

    D = INT(365.25 * (year + 4716))
    E = INT(30.6001 * (month + 1))

    julian = INT(D + E + day + 0.5 + C - 1524.5) +      &
                ( hour / 24.0 ) + ( minute / (60*24) ) + &
                ( second / (60*60*24) ) 

    RETURN

  END FUNCTION
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  Jul2Cal
!
! !DESCRIPTION: This function calculate the gregorian date from julian day.
!
!
! !INTERFACE:
!
! FUNCTION jul2cal__(jd) RESULT(gregorian)
!   IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
!   REAL (kind=8), INTENT(IN) :: jd

!
! !OUTPUT PARAMETERS:
!
!  INTEGER(kind=8) :: gregorian
!
!
! !REVISION HISTORY: 
!  23 Mar 2011 - J. G. de Mattos - created to maintain the 
!                                  original interface
!                                  of jul2cal function
! 
! !REMARKS:
!        This algorithm was adopted from Press et al.
!EOP
!-----------------------------------------------------------------------------!
!BOC

! INTEGER :: nymd
! INTEGER :: nhms

! call jul2cal(jd,nymd,nhms)

! gregorian = (nymd*100)+INT(nhms/10000)

! END FUNCTION
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  Jul2Cal
!
! !DESCRIPTION: This function calculate the gregorian date from julian day.
!
!
! !INTERFACE:
!
  SUBROUTINE jul2cal(jd,ymd,hms)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
	 REAL (kind=8), INTENT(IN) :: jd

!
! !OUTPUT PARAMETERS:
!
   INTEGER(kind=4) :: ymd ! year month day (yyyymmdd)
   INTEGER(kind=4) :: hms ! hour minute second (hhmnsd)

!
!
! !REVISION HISTORY: 
!  15 Jun 2005 - J. G. de Mattos - Initial Version
!  23 Mar 2011 - J. G. de Mattos - Modified Interface
!                                  to a subroutine call
!
! !REMARKS:
!        This algorithm was adopted from Press et al.
!EOP
!-----------------------------------------------------------------------------!
!BOC
!
    INTEGER (kind=4), PARAMETER :: Gregjd = 2299161
    INTEGER                     ::  j1, j2, j3, j4, j5
    INTEGER (kind=4)            ::  year
    INTEGER (kind=4)            ::  month
    INTEGER (kind=4)            ::  day
    INTEGER (kind=4)            ::  Hour
    INTEGER (kind=4)            ::  Minute
    INTEGER (kind=4)            ::  Second
    INTEGER (kind=4)            ::  Intgr
    INTEGER (kind=4)            ::  f, tmp

    REAL (kind=8)               ::  dayfrac, frac

    !       
    ! get the date from the Julian day number
    !       
    ! jd=2453372.25

    intgr   = INT(jd)
    frac    = real(jd - intgr,8)

    IF( intgr >= gregjd )THEN              !Gregorian calendar correction
       tmp = INT( ( (intgr - 1867216) - 0.25 ) / 36524.25 )
       j1  = intgr + 1 + tmp - INT(0.25*tmp)
    ELSE
       j1 = intgr
    ENDIF
    !       correction for half day offset

    dayfrac = frac + 0.0d0
!    print*,dayfrac
    IF( dayfrac >= 1.0 )THEN
       dayfrac = dayfrac - 1.0d0
       j1 = j1+1
    ENDIF

    j2 = j1 + 1524
    j3 = INT( 6680.0 + ( (j2 - 2439870) - 122.1 )/365.25 )
    j4 = INT(j3*365.25)
    j5 = INT( (j2 - j4)/30.6001 )

    day   = INT(j2 - j4 - INT(j5*30.6001))
    month = INT(j5 - 1)
    IF( month > 12 ) month = month- 12
    year = INT(j3 - 4715)
    IF( month > 2 )  year = year - 1
    IF( year <= 0 ) year = year - 1

    !
    ! get time of day from day fraction
    !
    hour   = INT(dayfrac * 24.0d0)
    minute = INT((dayfrac*24.0d0 - hour)*60.0d0)
    f      = NINT( ((dayfrac*24.0d0 - hour)*60.0d0 - minute)*60.0d0)
    second = INT(f)
    f      = f-second
!    print*,dayfrac * 24.0d0,hour
!    print*,(dayfrac*24.0d0 - hour)

!    print*,hour,minute,second
    IF( f > 0.5 ) second = second + 1

    IF( second == 60 )THEN
       second = 0
       minute = minute + 1
    ENDIF

    IF( minute == 60 )THEN
       minute = 0
       hour   = hour + 1
    ENDIF

    IF( hour == 24 )THEN

       hour = 0
       !
       ! this could cause a bug, but probably 
       ! will never happen in practice
       !
       day = day + 1

    ENDIF

    IF( year < 0 )THEN
       year = year * (-1)
    ENDIF

    ymd = (year*10000)+(month*100)+(day)
    hms = (hour*10000)+(minute*100)+second

!    gregorian=(year*1000000)+(month*10000)+(day*100)+hour

    RETURN

  END SUBROUTINE


END MODULE SCAM_TimeMod
