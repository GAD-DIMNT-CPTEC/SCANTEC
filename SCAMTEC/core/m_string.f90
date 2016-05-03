!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                                                                     !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!BOP
!  !MODULE: m_string - a module to process strings
!
!  !DESCRIPTION: 
!     Make some operations in strings
!
!  !INTERFACE:

MODULE m_string
  !
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: replace      ! Replace a string by another
  PUBLIC :: str_template ! Replace variables in a template
  PUBLIC :: num2str      ! convert a number to string
  PUBLIC :: GetTokens    ! Get tokens by line
  PUBLIC :: find         ! find substring

  interface Str_Template
     module procedure str_Template_
  end interface

  interface replace
     module procedure replace_
  end interface

  interface num2str ; module procedure &
     int2str,   & ! convert integer to string
     float2str    ! convert float to string
  end interface

  interface GetTokens
     module procedure GetTokens_
  end interface

  interface find 
  module procedure indexarray, indexstr
  end interface


  ! !REVISION HISTORY:
  ! 15 Dec 2010 - J. G. de Mattos - Initial Version
  ! 02 Mar 2011 - J. G. de Mattos - Initial code to strTemplate
  ! 30 Nov 2012 - J. G. de Mattos - All input parameters optionals in strTemplate
  ! 05 Fev 2014 - J. G. de Mattos - Adding julian day mask
  ! 20 jun 2014 - J. G. de Mattos - Adding GetTokens feature
  ! 01 Sep 2014 - J. G. de Mattos - Adding find substring
  !
  ! !BUGS:
  !   Not yet
  !
  !EOP
  !---------------------------------------------------------------------!
  !BOC
  !
  ! PARAMETERS
  !
  CHARACTER(len=3),PARAMETER,DIMENSION(12) :: mon_lc = (/&
       'jan','feb','mar','apr','may','jun',&
       'jul','aug','sep','oct','nov','dec'/)

  CHARACTER(len=3),PARAMETER,DIMENSION(12) :: mon_wd = (/&
       'Jan','Feb','Mar','Apr','May','Jun',&
       'Jul','Aug','Sep','Oct','Nov','Dec'/)

  CHARACTER(len=3),PARAMETER,DIMENSION(12) :: mon_uc = (/&
       'JAN','FEB','MAR','APR','MAY','JUN',&
       'JUL','AUG','SEP','OCT','NOV','DEC'/)

CONTAINS
  !---------------------------------------------------------------------!
  !BOP
  !
  ! !ROUTINE: str_template -  A template formatting a string with variables
  !
  ! !DESCRIPTION:
  !
  !   A template resolver formatting a string with a string variable
  !   and time variables.  The format descriptors are similar to those
  !   used in the GrADS.
  !
  !  %y4    substitute with a 4 digit year
  !  %y2    a 2 digit year
  !  %m1    a 1 or 2 digit month
  !  %m2    a 2 digit month
  !  %mc    a 3 letter month in lower cases
  !  %Mc    a 3 letter month with a leading letter in upper case
  !  %MC    a 3 letter month in upper cases
  !  %d1    a 1 or 2 digit day
  !  %d2    a 2 digit day
  !  %h1    a 1 or 2 digit hour
  !  %h2    a 2 digit hour
  !  %h3    a 3 digit hour (?)
  !  %n2    a 2 digit minute
  !  %e     a string ensemble identify
  !  %jd    a julian day without hours decimals
  !  %jdh   a julian day with hour decimals
  !  %jy    a day of current year without hours decimals
  !  %jyh   a day of current year with hours decimals

  !
  !  %ix1   initial 1 digit decade 
  !  %ix3   initial 3 digit decade 
  !  %iy2   initial 2 digit year 
  !  %iy4   initial 4 digit year 
  !  %im1   initial 1 or 2 digit month 
  !  %im2   initial 2 digit month (leading zero if needed) 
  !  %imc   initial 3 character month abbreviation 
  !  %id1   initial 1 or 2 digit day (leading zero if needed) 
  !  %id2   initial 2 digit day 
  !  %ih1   initial 1 or 2 digit hour 
  !  %ih2   initial 2 digit hour 
  !  %ih3   initial 3 digit hour 
  !  %in2   initial 2 digit minute (leading zero if needed)

  !
  !  %fx1   forecast 1 digit decade 
  !  %fx3   forecast 3 digit decade 
  !  %fy2   forecast 2 digit year 
  !  %fy4   forecast 4 digit year 
  !  %fm1   forecast 1 or 2 digit month 
  !  %fm2   forecast 2 digit month (leading zero if needed) 
  !  %fmc   forecast 3 character month abbreviation 
  !  %fd1   forecast 1 or 2 digit day (leading zero if needed) 
  !  %fd2   forecast 2 digit day 
  !  %fh1   forecast 1 or 2 digit hour 
  !  %fh2   forecast 2 digit hour 
  !  %fh3   forecast 3 digit hour 
  !  %fn2   forecast 2 digit minute (leading zero if needed)
  !
  ! !INTERFACE:

  SUBROUTINE str_template_(strg,nymd,nhms,fymd,fhms,jd,doy,label)
    IMPLICIT NONE
    !
    ! !INPUT/OUTPUT PARAMETERS:
    !
    CHARACTER(len=*), INTENT(INOUT) :: strg
    !
    ! !INPUT PARAMETERS:
    !
    INTEGER,      OPTIONAL, INTENT(IN)   :: nymd
    INTEGER,      OPTIONAL, INTENT(IN)   :: nhms
    INTEGER,      OPTIONAL, INTENT(IN)   :: fymd
    INTEGER,      OPTIONAL, INTENT(IN)   :: fhms
    REAL(kind=8), OPTIONAL, INTENT(IN)   :: jd
    REAL(kind=8), OPTIONAL, INTENT(IN)   :: doy

    CHARACTER(LEN=*), OPTIONAL, INTENT(IN)   :: label


    !
    ! !REVISION HISTORY:
    !  Joao Gerd - 02Mar2011 - Codigo Inicial
    !  Joao Gerd - 30Nov2012 - All input parameters optionals in strTemplate
    !  Joao Gerd - 05Fev2014 - Adding julian day mask

    !EOP
    !---------------------------------------------------------------------!
    !BOC   
    INTEGER :: year4, year2, month, day
    INTEGER :: hour, minute, second

    INTEGER :: fyear4, fyear2, fmonth, fday
    INTEGER :: fhour, fminute, fsecond

    IF(PRESENT(nymd)) THEN

       year4  = INT( nymd / 10000 )
       year2  = MOD( year4, 100 )
       month  = MOD( nymd, 10000 ) / 100 
       day    = MOD( nymd, 100 )

       !
       ! Default template variables
       !

       call replace_( strg, '%y2', int2str(year2,'(I2.2)') )
       call replace_( strg, '%y4', int2str(year4,'(I4.4)') )
       call replace_( strg, '%m1', int2str(month,'(I1)') )
       call replace_( strg, '%m2', int2str(month,'(I2.2)') )
       call replace_( strg, '%mc', mon_lc(month) )
       call replace_( strg, '%Mc', mon_wd(month) )
       call replace_( strg, '%MC', mon_uc(month) )
       call replace_( strg, '%d1', int2str(day,'(I1)') )
       call replace_( strg, '%d2', int2str(day,'(I2.2)') )

       !
       ! Initial/Analysis time template
       !

       call replace_( strg, '%iy2', int2str(year2,'(I2.2)') )
       call replace_( strg, '%iy4', int2str(year4,'(I4.4)') )
       call replace_( strg, '%im1', int2str(month,'(I1)') )
       call replace_( strg, '%im2', int2str(month,'(I2.2)') )
       call replace_( strg, '%imc', mon_lc(month) )
       call replace_( strg, '%iMc', mon_wd(month) )
       call replace_( strg, '%iMC', mon_uc(month) )
       call replace_( strg, '%id1', int2str(day,'(I1)') )
       call replace_( strg, '%id2', int2str(day,'(I2.2)') )

    ENDIF

    IF(PRESENT(nhms)) THEN

       hour   = INT( nhms / 10000 ) 
       minute = MOD( nhms, 10000 ) / 100
       second = MOD( nhms, 100 )

       !
       ! Default template variables
       !
       call replace_( strg, '%h1', int2str(hour,'(I1)') )
       call replace_( strg, '%h2', int2str(hour,'(I2.2)') )
       call replace_( strg, '%h3', int2str(hour,'(I3.3)') )
       call replace_( strg, '%n2', int2str(minute,'(I2.2)') )
       !
       ! Initial/Analysis time template
       !
       call replace_( strg, '%ih1', int2str(hour,'(I1)') )
       call replace_( strg, '%ih2', int2str(hour,'(I2.2)') )
       call replace_( strg, '%ih3', int2str(hour,'(I3.3)') )
       call replace_( strg, '%in2', int2str(minute,'(I2.2)') )

    ENDIF

    !
    ! Final/Forecast time template
    !

    IF ( PRESENT(fymd))THEN 

       fyear4  = INT( fymd / 10000 )
       fyear2  = MOD( fyear4, 100 )
       fmonth  = MOD( fymd, 10000 ) / 100 
       fday    = MOD( fymd, 100 )

       call replace_( strg, '%fy2', int2str(fyear2,'(I2.2)') )
       call replace_( strg, '%fy4', int2str(fyear4,'(I4.4)') )
       call replace_( strg, '%fm1', int2str(fmonth,'(I1)') )
       call replace_( strg, '%fm2', int2str(fmonth,'(I2.2)') )
       call replace_( strg, '%fmc', mon_lc(fmonth) )
       call replace_( strg, '%fMc', mon_wd(fmonth) )
       call replace_( strg, '%fMC', mon_uc(fmonth) )
       call replace_( strg, '%fd1', int2str(fday,'(I1)') )
       call replace_( strg, '%fd2', int2str(fday,'(I2.2)') )

    END IF

    IF (PRESENT(fhms) ) THEN

       fhour   = INT( fhms / 10000 ) 
       fminute = MOD( fhms, 10000 ) / 100
       fsecond = MOD( fhms, 100 )

       call replace_( strg, '%fh1', int2str(fhour,'(I1)') )
       call replace_( strg, '%fh2', int2str(fhour,'(I2.2)') )
       call replace_( strg, '%fh3', int2str(fhour,'(I3.3)') )
       call replace_( strg, '%fn2', int2str(fminute,'(I2.2)') )

    END IF

    IF (PRESENT(label)) call replace_( strg, '%e', trim(label))

    IF (PRESENT(jd))THEN
       call replace_(strg, '%jdh', float2str(jd, '(F15.3)'))
       call replace_(strg, '%jd', int2str(int(jd), '(I10.3)'))
    ENDIF

    IF(PRESENT(doy))THEN
       call replace_(strg, '%jyh', float2str(doy, '(F15.3)'))
       call replace_(strg, '%jy', int2str(int(doy), '(I10.3)'))
    ENDIF

  END SUBROUTINE str_template_
  !---------------------------------------------------------------------!
  !BOP
  !
  ! !ROUTINE: replace
  !
  ! !DESCRIPTION: Rotina para substituir a mask pela repl na strg
  !
  ! !INTERFACE:

  SUBROUTINE replace_(strg,mask,repl)

    !
    !
    IMPLICIT NONE

    ! !INPUT/OUTPUT PARAMETERS:

    CHARACTER(len=*),INTENT(INOUT)  :: strg ! String

    ! !INPUT PARAMETERS:

    CHARACTER(len=*),INTENT(IN)     :: mask ! maskout
    CHARACTER(len=*),INTENT(IN)     :: repl ! replacing string
    !
    ! !REVISION HISTORY:
    !  Joao Gerd - 20Feb2011 - Codigo Inicial
    !
    !EOP
    !---------------------------------------------------------------------!
    !BOC
    CHARACTER(len=300) ::  sub, tmp
    INTEGER :: len_tmp, len_repl, len_mask, len_strg
    INTEGER :: i, j

    len_strg = LEN_TRIM(strg)
    len_repl = LEN_TRIM(repl)
    len_mask = LEN_TRIM(mask)

    tmp = ''
    i   = 1
    j   = 1

    do while( j .LE. len_strg )

       sub = strg(j:j+len_mask-1)

       if( TRIM(sub) .EQ. TRIM(mask) )then

          len_tmp                         = LEN_TRIM(tmp)
          tmp(len_tmp+1:len_tmp+len_repl) = TRIM(repl)

          i = j + len_mask
          j = i

       endif

       len_tmp                  = LEN_TRIM(tmp)
       tmp(len_tmp+1:len_tmp+1) = strg(j:j)

       j = j + 1 

    enddo

    strg = tmp

    return
  END SUBROUTINE replace_
  !EOC
  !---------------------------------------------------------------------!
  !
  !

subroutine GetTokens_(line,tokens,ntokens,del)
   implicit none
   character(len=*)           :: line
   integer                    :: ntokens
   character(len=*)           :: tokens(:)
   character(len=*), optional :: del
!--------------------------------------------------------------------!
!
    
   integer              :: length
   integer              :: i,j
   character(len=10)    :: delimiter
   character, parameter :: BLK = achar(32)   ! blank (space)

   length    = len_trim(line)    
   j         = 1
   ntokens   = 0
   tokens    = BLK
   delimiter = BLK
   if(present(del)) delimiter = del

   do while (j.le.length)

      !
      ! loop over next blank
      !

      do while (line(j:j).eq.trim(delimiter))
         j = j + 1
         if(j.gt.length)exit
      enddo

      !
      ! loop over next token
      !

      ntokens = ntokens + 1
      i = 1
      do while (line(j:j).ne.trim(delimiter))
         tokens(ntokens)(i:i) = line(j:j)
         j = j + 1
         i = i + 1
         if (j.gt.length) exit
      enddo

   end do

   end subroutine

  character(20) function int2str(num,format) 
    integer, intent(in):: num
    character(len=*), intent(in) :: format
    character(20) :: str
    ! convert integer to string using formatted write
    write(str, format ) num
    int2str = adjustl(str)
  end function int2str

  character(20) function float2str(num,format) 
    real(kind=8), intent(in):: num
    character(len=*), intent(in) :: format
    character(20) :: str
    ! convert integer to string using formatted write
    write(str, format ) num
    float2str = adjustl(str)
  end function float2str

  integer  function indexarray(str,tok) result(count)
    implicit none

    character(len=*), intent(in) :: str(:)
    character(len=*), intent(in) :: tok
    integer :: idx, i
    logical :: found


    found = .false.
    
    count = 1
    do i=1,size(str)
       idx=index(trim(tok),trim(str(i)))
       if(idx.gt.0)then
          found = .true.
          exit
       endif
       count = count + 1
    enddo
    if(.not.found) count = 0

  end function indexarray

  integer  function indexstr(str,tok) result(idx)
    implicit none

    character(len=*), intent(in) :: str
    character(len=*), intent(in) :: tok


    idx=index(trim(tok),trim(str))

  end function indexstr


END MODULE m_string


