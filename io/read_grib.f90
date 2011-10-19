!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center Land Information System (LIS) V5.0 BETA
! Released January 2008
!
! See SOFTWARE DISTRIBUTION POLICY for software distribution policies
!
! The LIS source code and documentation are in the public domain,
! available without fee for educational, research, non-commercial and
! commercial purposes.  Users may distribute the binary or source
! code to third parties provided this statement appears on all copies and
! that no charge is made for such copies.
!
! NASA GSFC MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE
! SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED AS IS WITHOUT EXPRESS OR
! IMPLIED WARRANTY.  NEITHER NASA GSFC NOR THE US GOVERNMENT SHALL BE
! LIABLE FOR ANY DAMAGES SUFFERED BY THE USER OF THIS SOFTWARE.
!
! See COPYRIGHT.TXT for copyright details.
!
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
!BOP
! !ROUTINE: retsaldas
!  \label{retsaldas}
!
! !REVISION HISTORY:
!  1  Oct 1999: Jared Entin; Initial code
!  15 Oct 1999: Paul Houser; Significant F90 Revision
!  11 Apr 2000: Brian Cosgrove; changed code to use Forcing Mask (With inland
!               water filled in).  Deleteted unused variables.
!  27 Apr 2000: Brian Cosgrove; changed code to use the original 
!               mask again since that is the
!               mask which  NCEP has already applied to the forcing data
!               by the time NASA gets it......not possible to use the 
!               expanded NASA forcing mask
!  1  May 2000: Brian Cosgrove; changed code so that if parameter 11 (sw)
!               is not found in hourly ncep data, it will just use
!               edas-based shortwave from the hourly ncep files
!  20 Jun 2000: Brian Cosgrove; changed code so that it uses  LDAS%UDEF and
!                not a hard-wired undefined value of -999.9 and -999.0
!  18 Aug 2000: Brian Cosgrove; changed code so that FMASK and not MASK
!               is used when ungribbing.  NCEP data already has a mask applied
!               to it and so may not be able to supply forcing data to
!               all LDAS land forcing points.  In areas where LDAS
!               forcing mask states that land exists, but where NCEP forcing
!               data is non-existant, assign undefined value to forcing data.
!  22 Aug 2000: Brian Cosgrove; Altered code for US/Mexico/Canada Mask
!  05 Sep 2001: Brian Cosgrove; Removed dirnom and infile variables, changed
!               call to ungribncep to match removal.  Added code to make use
!               of precip weighting mask
!  02 Feb 2004: Sujay Kumar; Initial Specification in LIS
! 
! !INTERFACE:
subroutine retsaldas(order,n, name,ferror)
! !USES:
  use lisdrv_module, only : lis,lisdom
  use saldasdomain_module, only : saldas_struc
  use baseforcing_module, only : lisforc
  use lis_logmod, only :logunit

  implicit none
! !ARGUMENTS: 
  integer, intent(in)      :: order
  integer, intent(in)      :: n
  character*80, intent(in) :: name
  integer, intent(out)     :: ferror
!
! !DESCRIPTION:
!  For the given time, reads parameters from
!  SALDAS data, transforms into 9 LIS forcing 
!  parameters and interpolates to the LIS domain.
!
!  The arguments are: 
!  \begin{description}
!  \item[order]
!    flag indicating which data to be read (order=1, read the previous 
!    hourly instance, order=2, read the next hourly instance)
!  \item[n]
!    index of the nest
!  \item[name]
!    name of the 3 hour GDAS forecast file
!  \item[ferror]
!    flag to indicate success of the call (=0 indicates success)
!  \end{description}
! 
!  The routines invoked are: 
!  \begin{description}
!  \item[interp\_saldas](\ref{interp_saldas})\\
!    spatially interpolates a SALDAS variable
!  \end{description}
!EOP
  integer :: errorflag,incr,incc
  integer :: endloop,iv,iret,lugb,io
  logical :: file_exists
  integer :: gbret,jret,jpds(200),jgds(200),gridDesc(200),kpds(200)
  real :: lubi,kf,k
  real :: l1,l2,r1,r2
  integer :: saldas,c,r,j
  integer, dimension(10) :: pds5, pds7,pds2
  logical*1, allocatable :: lb(:)
  real, allocatable :: f(:)
  real, allocatable :: f2(:)
  real, dimension(lis%lnc(n), lis%lnr(n)) :: varfield
  real, dimension(saldas_struc(n)%ncold,saldas_struc(n)%nrold) :: varfield2

  ferror = 1
  endloop=0
  errorflag = 0
  iv = 0
  saldas = (saldas_struc(n)%ncold*saldas_struc(n)%nrold)
  pds5 = (/ 004,005,007,008,001,001,006,002,002,157/) !parameter
  pds7 = (/ 000,000,000,000,000,000,000,000,000,000/) !htlev2
  pds2 = (/ 84, 84, 84, 84, 84, 84, 84, 155, 84,84/)
  allocate(lb(saldas_struc(n)%ncold*saldas_struc(n)%nrold))
  allocate(f(saldas_struc(n)%ncold*saldas_struc(n)%nrold))
  allocate(f2(saldas_struc(n)%ncold*saldas_struc(n)%nrold))
  do
     if ( endloop == 1 ) exit
     iv = iv+1
     lugb = iv
     inquire (file=trim(name), exist=file_exists)
     if (file_exists) then      
!--------------------------------------------------------------------------
! Set up to open file and retrieve specified field 
!--------------------------------------------------------------------------
        lugb = iv
        lubi = 0 
        j = 0 
        jpds = -1
        jpds(5) = pds5(iv)
        jpds(7) = pds7(iv)
!        jpds(2) = pds2(iv)
        call baopenr(lugb,name,iret)
        if(iret==0) then 
           call getgb(lugb,lubi,saldas,j,jpds,jgds,kf,k,kpds, &
                gridDesc,lb,f,gbret)
           if(iv.eq.30) then 
              jpds = -1
              jpds(5) = 204
              jpds(7) = 0
              jpds(2) = 154
              call getgb(lugb,lubi,saldas,j,jpds,jgds,kf,k,kpds, &
                   gridDesc,lb,f2,gbret)
           endif
           if(iv.eq.3) then 
           endif
        else
           gbret = 99
        endif
        call baclose(lugb,jret)
     else
        ferror = 0
        deallocate(f)
        deallocate(f2)
        deallocate(lb)
     endif
     if(gbret == 0) then 
        call interp_saldas(n, kpds,saldas,f,lb,lis%gridDesc(n,:),& 
             lis%lnc(n),lis%lnr(n),varfield)
     else
        errorflag = 1
     endif
     if(errorflag == 1) then 
        endloop = 1
        ferror = 0
     else
!	if( iv == 3)then
	io=0
	do r=1,saldas_struc(n)%nrold
        do c=1,saldas_struc(n)%ncold
	varfield2(c,r)=f(c+io)
	enddo
	io=io+saldas_struc(n)%ncold
	enddo
		
	        do r=1,lis%lnr(n)
           do c=1,lis%lnc(n)
	incr=INT((-82.8125-(-82.9375))/0.125)
        incc=INT((-48.8125-(-48.9375))/0.125)
!	varfield(c,r)=varfield2(c+incc,r+incr)
	enddo
	enddo
!	endif
        do r=1,lis%lnr(n)
           do c=1,lis%lnc(n)
              if(lisdom(n)%gindex(c,r).ne.-1) then 
                 if ( order == 1 ) then 
                    lisforc(n)%glbdata1(iv,lisdom(n)%gindex(c,r)) = varfield(c,r)
                 else
                    lisforc(n)%glbdata2(iv,lisdom(n)%gindex(c,r)) = varfield(c,r)
                    
                 end if
              endif
           end do
        enddo
     end if
     if(errorflag ==9) then 
        write(unit=logunit,fmt=*) 'Could not find forcing parameter in file, ',name
     endif
     if(iv == 9) endloop = 1
  enddo
!  deallocate(lb)
!  deallocate(f)
!  deallocate(f2)
  return
end subroutine retsaldas


!BOP
! !ROUTINE: interp_saldas
! \label{interp_saldas}
!
! !INTERFACE:
subroutine interp_saldas(n, kpds, saldas,f,lb,lis_gds,nc,nr, &
     varfield)
! !USES:
  use lisdrv_module, only : lis
  use saldasdomain_module, only :saldas_struc
  
  implicit none

! !ARGUMENTS:   
  integer, intent(in)   :: n 
  integer, intent(in)   :: kpds(200)
  integer, intent(in)   :: saldas
  real, intent(out)     :: f(saldas)
  logical*1, intent(in) :: lb(saldas)
  real, intent(in)      :: lis_gds(50)
  integer, intent(in)   :: nc
  integer, intent(in)   :: nr
  real, intent(inout)   :: varfield(nc,nr)
  character :: varfield2(nc,nr)
!
! !DESCRIPTION:
!   This subroutine interpolates a given SALDAS field 
!   to the LIS grid. 
!  The arguments are: 
!  \begin{description}
! \item[n]
!  index of the nest
! \item[kpds]
!  grib decoding array
! \item[ngdas]
!  number of elements in the input grid
! \item[f]
!  input data array to be interpolated
! \item[lb]
!  input bitmap
! \item[lis\_gds]
!  array description of the LIS grid
! \item[nc]
!  number of columns (in the east-west dimension) in the LIS grid
! \item[nr]
!  number of rows (in the north-south dimension) in the LIS grid
! \item[varfield]
!  output interpolated field
!  \end{description} 
! 
!
!  The routines invoked are: 
!  \begin{description}
!  \item[bilinear\_interp](\ref{bilinear_interp})\\
!    spatially interpolate the forcing data using bilinear interpolation
!  \item[conserv\_interp](\ref{conserv_interp})\\
!    spatially interpolate the forcing data using conservative interpolation
! \end{description}
!EOP
  integer :: ip, ipopt(20),ibi,km,iret
  integer :: ibo,mo
  integer :: count,i,j
  real, dimension(nc*nr) :: lis1d
  logical*1 :: lo(nc*nr)

!=== End variable declarations

!-----------------------------------------------------------------------
! Setting interpolation options (ip=0,bilinear)
! (km=1, one parameter, ibi=1,use undefined bitmap
! (needed for soil moisture and temperature only)
! Use budget bilinear (ip=3) for precip forcing fields
!-----------------------------------------------------------------------
  mo = nc*nr
  if (kpds(5)==61 .or. kpds(5)==214) then
     ip=3
     ipopt(1)=-1
     ipopt(2)=-1
     km=1
     ibi=1          
  else
     ip=0
     do i=1,20
        ipopt(i)=0
     enddo
     km=1
     ibi=1
  endif
!-----------------------------------------------------------------------
! Initialize output bitmap. Important for soil moisture and temp.
!-----------------------------------------------------------------------
  lo = .true.

!-----------------------------------------------------------------------  
! Interpolate to LIS grid
!-----------------------------------------------------------------------
  if(lis%interp.eq.1) then 
     call bilinear_interp(lis_gds,ibi,lb,f,ibo,lo,lis1d,saldas_struc(n)%mi,mo,&
          saldas_struc(n)%rlat1, saldas_struc(n)%rlon1,&
          saldas_struc(n)%w111, saldas_struc(n)%w121,&
          saldas_struc(n)%w211,saldas_struc(n)%w221,&
          saldas_struc(n)%n111,saldas_struc(n)%n121,&
          saldas_struc(n)%n211,saldas_struc(n)%n221,lis%udef,iret)
  elseif(lis%interp.eq.2) then 
     if (kpds(5)==61 .or. kpds(5)==214) then     
        call conserv_interp(lis_gds,ibi,lb,f,ibo,lo,lis1d,saldas_struc(n)%mi,mo,& 
             saldas_struc(n)%rlat2,saldas_struc(n)%rlon2,&
             saldas_struc(n)%w112,saldas_struc(n)%w122,&
             saldas_struc(n)%w212,saldas_struc(n)%w222,&
             saldas_struc(n)%n112,saldas_struc(n)%n122,&
             saldas_struc(n)%n212,saldas_struc(n)%n222,lis%udef,iret)
     else 
        call bilinear_interp(lis_gds,ibi,lb,f,ibo,lo,lis1d,saldas_struc(n)%mi,mo,&
             saldas_struc(n)%rlat1, saldas_struc(n)%rlon1,&
             saldas_struc(n)%w111,saldas_struc(n)%w121,&
             saldas_struc(n)%w211,saldas_struc(n)%w221,&
             saldas_struc(n)%n111,saldas_struc(n)%n121,&
             saldas_struc(n)%n211,saldas_struc(n)%n221,lis%udef,iret)
     endif
  endif
!-----------------------------------------------------------------------    
! Create 2D array for main program. Also define a "soil" mask
! due to different geography between LDAS & LDAS. For LDAS land 
! points not included in LDAS geography dataset only.
!-----------------------------------------------------------------------    
  count = 0
  do j = 1, nr
     do i = 1, nc
        varfield(i,j) = lis1d(i+count)
	if(lo(i+count))then
	varfield2(i,j)='0'
	else
	varfield2(i,j)='1'
	endif
     enddo
     count = count + nc
  enddo
  do j = 1, nr,15
      ! print *,( varfield2(i,j),i=1,nc,10)
  enddo

!-----------------------------------------------------------------------    
! Save air tempertaure interpolated field for later use in
! initialization of soil temp where geography differs 
! between LDAS and LDAS
!-----------------------------------------------------------------------    
!  if (kpds(5) .eq. 11 .and. kpds(6) .eq. 105) then
!     do i = 1, nc
!        do j = 1, nr
!           geogtemp(i,j) = varfield(i,j)
!        enddo
!     enddo
!  endif

end subroutine interp_saldas
