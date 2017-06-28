!-----------------------------------------------------------------------------------!
!             Group on Data Assimilation Development - GDAD/CPTEC/INPE              !
!                                                                                   !
!                                                                                   !
!       AUTHORS: Arletis Roque Carrasco                                             !
!              	 Luiz Fernando Sapucci                                              !
!										    !
!       Adapted from the work of: Arletis Roque Carrasco                            !
!				  Maibys Sierra Lorenzo                             !
!				  Israel Borrajero Montejo                          !
!				  Camilo Rodríguez Geno                             !
!                               						    ! 
!-----------------------------------------------------------------------------------!

! MODULE: m_mode.f90
!
! DESCRIPTON:
! Module for calculating the Method for Object-based Diagnostic Evaluation (MODE)
! The MODE is based on the objects identification by a convolution procedure
! whereby the fields are first smoothed over space and then thresholded by applying 
! an intensity threshold to the field. 

! Once objects are identified (contiguous nonzero pixels), they are merged and 
! matched by an algorithm of Fuzzy Logic utilizing information about various  
! attributes (centroid position, total area, area overlap, intensity distribution,  
! orientation angle, and boundary separation). 
!


MODULE mode

   USE scamtec_module                 		! module where the structure scantec is defined
   USE SCAM_dataMOD, only : scamdata  		! SCANTEC data matrix
   USE SCAM_Utils
   USE time_module, only: jul2cal, cal2jul 	! Time operations
   USE m_string                      		! string manipulations
   USE m_die                         		! Error Messages
   USE m_mode_objects		      		! module where objects are identified
   USE m_mode_singleAttrib	      		! module where single objects attributes are calculated
   USE m_mode_pairAttrib	      		! module where pair objects attributes are calculated

   IMPLICIT NONE
   PRIVATE
 
   real, pointer     :: prefield(:,:) ! Pointer to save precipitation observation data
   real, pointer     :: expfield(:,:) ! Pointer to save precipitation experiment data 
   
   real, allocatable :: precOriginalField(:,:)  ! Matrix to save precipitation observation data 
   real, allocatable :: expOriginalField(:,:)   ! Matrix to save precipitation experiment data 
   real, allocatable :: inputfield(:,:), subfield(:,:)  

   character(len=512) :: FNameOut = '%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2'
   integer            :: FUnitOut = 30

   ! Statistical indices
   integer			:: hits, false_alarms, misses
   real				:: CSI, POD, FAR, BIAS   

   type statistic
     integer(I4B) 	:: atime
     integer, allocatable  :: fcst_time(:)
     integer, allocatable  :: misses(:), falseAlarms(:), hits(:)
     real, allocatable	:: csi(:)
     real, allocatable	:: pod(:)
     real, allocatable	:: far(:)
     real, allocatable	:: vies(:)
     real, allocatable  :: ets(:)
   end type statistic

   type(statistic), allocatable :: indices(:,:)  

   type(statistic), allocatable :: contingency1(:,:)

   type nobj
     integer(I4B) 	:: atime
     integer, allocatable  :: fcst_time(:)
     integer, allocatable :: nobjFbm(:), nobjFam(:), nobjObm(:), nobjOam(:), hits(:)
     real, allocatable	  :: csi(:)
   end type nobj

   type(nobj), allocatable :: compare(:,:)

   public :: mode_init
   public :: mode_ObjectIdentf
   public :: mode_run


   Contains

!**************************************************************************************************************************************
   Subroutine mode_init(nexp, f, aux)  
   ! Subroutine where variables and structures memory is allocated.   
     Implicit None
     ! Input Parameter         
     integer, intent(in) :: nexp, f      ! experiment number
     ! Output Parameter 
     integer, intent(out) :: aux

     integer            :: i
     character(len=*),parameter :: myname='::mode_init'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname
#endif	      	 	    

     if(.not.allocated(precOriginalField))Allocate(precOriginalField(scamtec%nxpt,scamtec%nypt))
     if(.not.allocated(expOriginalField))Allocate(expOriginalField(scamtec%nxpt,scamtec%nypt))
	
     if (f .EQ. 1) then
       Allocate(inputfield(scamtec%npts,scamtec%ntime_forecast))        
       Allocate(subfield(scamtec%npts,scamtec%ntime_forecast))
     endif 
 
     ! transferindo dados de precipitacion 
     prefield => scamdata(1)%prefield             
     expfield => scamdata(nexp)%expfield         
	

     inputfield(:,f) = expfield(:,hist%tipo_precip)     
     
     if (Exper(nexp)%id .EQ. 11) then ! if model type is BRAMS
       if (f .EQ. 1) then         
         subfield(:,f) = inputfield(:,f)         
       elseif (f .GE. 2) then
         subfield(:,f) = inputfield(:,f) - inputfield(:,f-1)
       endif
     else
       subfield(:,f) = inputfield(:,f)
     endif

     expOriginalField = RESHAPE(subfield(:,f), (/scamtec%nxpt,scamtec%nypt/))
     precOriginalField = RESHAPE(prefield(:,21), (/scamtec%nxpt,scamtec%nypt/))               

     print*,'Min/Max PREFIELD_MODE: ',minval(precOriginalField(:,:)),maxval(precOriginalField(:,:)) 
     print*,'Min/Max EXPFIELD_MODE: ',minval(expOriginalField(:,:)),maxval(expOriginalField(:,:))  
      	
     aux = (scamtec%loop_count + scamtec%ntime_forecast) - (scamtec%ntime_forecast * CEILING(scamtec%loop_count/float(scamtec%ntime_forecast)))

     if(.NOT.Allocated(indices))Allocate(indices(scamtec%nexp,scamtec%ntime_steps))
         
     if(.NOT.Allocated(indices(nexp,scamtec%ftime_count(aux))%fcst_time))Allocate(indices(nexp,scamtec%ftime_count(aux))%fcst_time(scamtec%ntime_forecast-1))
     if(.NOT.Allocated(indices(nexp,scamtec%ftime_count(aux))%misses))Allocate(indices(nexp,scamtec%ftime_count(aux))%misses(scamtec%ntime_forecast-1))
     if(.NOT.Allocated(indices(nexp,scamtec%ftime_count(aux))%falseAlarms))Allocate(indices(nexp,scamtec%ftime_count(aux))%falseAlarms(scamtec%ntime_forecast-1))
     if(.NOT.Allocated(indices(nexp,scamtec%ftime_count(aux))%hits))Allocate(indices(nexp,scamtec%ftime_count(aux))%hits(scamtec%ntime_forecast-1))	
     if(.NOT.Allocated(indices(nexp,scamtec%ftime_count(aux))%csi))Allocate(indices(nexp,scamtec%ftime_count(aux))%csi(scamtec%ntime_forecast-1))
     if(.NOT.Allocated(indices(nexp,scamtec%ftime_count(aux))%pod))Allocate(indices(nexp,scamtec%ftime_count(aux))%pod(scamtec%ntime_forecast-1))
     if(.NOT.Allocated(indices(nexp,scamtec%ftime_count(aux))%far))Allocate(indices(nexp,scamtec%ftime_count(aux))%far(scamtec%ntime_forecast-1))
     if(.NOT.Allocated(indices(nexp,scamtec%ftime_count(aux))%vies))Allocate(indices(nexp,scamtec%ftime_count(aux))%vies(scamtec%ntime_forecast-1))

   End Subroutine mode_init
!**************************************************************************************************************************************




!**************************************************************************************************************************************
   Subroutine Rest_Basic_Statistics(nexp, aux, rowSize, colSize, fcst, obs)
     Implicit None
     ! Input Parameter         
     integer, intent(in) 		:: nexp, aux, rowSize, colSize 
     real, allocatable, intent(in) 	:: fcst(:,:), obs(:,:)

     integer		:: i, j, a, b, c, d, n
     real		:: CSIc, PODc, FARc, BIASc, ETSc

     logical            :: Opened
     character(len=512) :: filename, fname, fmt
     integer            :: nymd, nhms
     integer            :: fymd, fhms

     integer(I4B) 	:: ai
     integer		:: bi,ci,di,ei     	
     real		:: fi,gi,hi,ji, ki
 
     
     if(.NOT.Allocated(contingency1))Allocate(contingency1(scamtec%nexp,scamtec%ntime_steps))
         
     if(.NOT.Allocated(contingency1(nexp,scamtec%ftime_count(aux))%fcst_time))Allocate(contingency1(nexp,scamtec%ftime_count(aux))%fcst_time(scamtec%ntime_forecast-1))
     if(.NOT.Allocated(contingency1(nexp,scamtec%ftime_count(aux))%misses))Allocate(contingency1(nexp,scamtec%ftime_count(aux))%misses(scamtec%ntime_forecast-1))
     if(.NOT.Allocated(contingency1(nexp,scamtec%ftime_count(aux))%falseAlarms))Allocate(contingency1(nexp,scamtec%ftime_count(aux))%falseAlarms(scamtec%ntime_forecast-1))
     if(.NOT.Allocated(contingency1(nexp,scamtec%ftime_count(aux))%hits))Allocate(contingency1(nexp,scamtec%ftime_count(aux))%hits(scamtec%ntime_forecast-1))	
     if(.NOT.Allocated(contingency1(nexp,scamtec%ftime_count(aux))%csi))Allocate(contingency1(nexp,scamtec%ftime_count(aux))%csi(scamtec%ntime_forecast-1))
     if(.NOT.Allocated(contingency1(nexp,scamtec%ftime_count(aux))%pod))Allocate(contingency1(nexp,scamtec%ftime_count(aux))%pod(scamtec%ntime_forecast-1))
     if(.NOT.Allocated(contingency1(nexp,scamtec%ftime_count(aux))%far))Allocate(contingency1(nexp,scamtec%ftime_count(aux))%far(scamtec%ntime_forecast-1))
     if(.NOT.Allocated(contingency1(nexp,scamtec%ftime_count(aux))%vies))Allocate(contingency1(nexp,scamtec%ftime_count(aux))%vies(scamtec%ntime_forecast-1))
     if(.NOT.Allocated(contingency1(nexp,scamtec%ftime_count(aux))%ets))Allocate(contingency1(nexp,scamtec%ftime_count(aux))%ets(scamtec%ntime_forecast-1))

     a=0
     b=0
     c=0
     d=0
     

     do j=1, colSize
       do i=1, rowSize
         if (fcst(i,j) .GT. 0.0) then
	   if (obs(i,j) .GT. 0.0) then
	     a = a + 1
	   else 
	     b = b + 1
	   endif
	 else
	   if (obs(i,j) .GT. 0.0) then
	     c = c + 1
	   else 
	     d = d + 1
	   endif
         endif
       enddo
     enddo

     n = a+b+c+d
     
     !print*, 'hits', a
     !print*, 'f_alarms', b
     !print*, 'misses', c
     !print*, 'Correct_rejection', d
     

     CSIc = REAL(a) / (REAL(a) + REAL(c) + REAL(b))
     !print*
     !print*,  'CSIc', CSIc

     PODc = REAL(a) / (REAL(a) + REAL(c))
     !print*
     !print*,  'PODc', PODc

     FARc = REAL(b) / (REAL(a) + REAL(b))
     !print*
     !print*,  'FARc', FARc

     BIASc = (REAL(a) + REAL(b)) / (REAL(a) + REAL(c))
     !print*
     !print*, 'BIASc', BIASc
     !print* 

     ETSc = (REAL(a)-(REAL(a)+REAL(c))*(REAL(a)+REAL(b))/REAL(n)) / (REAL(a)+REAL(b)+REAL(c)-((REAL(a)+REAL(c))*(REAL(a)+REAL(b))/REAL(n)))
     !print*
     !print*, 'ETSc', ETSc
     !print*   

     contingency1(nexp,scamtec%ftime_count(aux))%atime = scamtec%atime
     contingency1(nexp,scamtec%ftime_count(aux))%fcst_time(scamtec%ftime_idx-1) = int(abs(cal2jul(scamtec%atime)-cal2jul(scamtec%ftime))*24)
     contingency1(nexp,scamtec%ftime_count(aux))%misses(scamtec%ftime_idx-1) = c
     contingency1(nexp,scamtec%ftime_count(aux))%falseAlarms(scamtec%ftime_idx-1) = b
     contingency1(nexp,scamtec%ftime_count(aux))%hits(scamtec%ftime_idx-1) = a
     contingency1(nexp,scamtec%ftime_count(aux))%csi(scamtec%ftime_idx-1) = CSIc
     contingency1(nexp,scamtec%ftime_count(aux))%pod(scamtec%ftime_idx-1) = PODc
     contingency1(nexp,scamtec%ftime_count(aux))%far(scamtec%ftime_idx-1) = FARc
     contingency1(nexp,scamtec%ftime_count(aux))%vies(scamtec%ftime_idx-1) = BIASc
     contingency1(nexp,scamtec%ftime_count(aux))%ets(scamtec%ftime_idx-1) = ETSc     

     ! writing Statistical Índices to text file
     if (scamtec%ftime_idx .EQ. scamtec%ntime_forecast) then	

        nymd = scamtec%atime/100
        nhms = MOD(scamtec%atime,100) * 10000
        fymd = scamtec%ftime/100
        fhms = MOD(scamtec%ftime,100) * 10000	
        
       
	  fname = 'Restored_BasicStatistic'
	  inquire(unit=67, opened=Opened)
	  if(.not.Opened) then 
	    filename = trim(fname)//'_'//trim(FNameOut)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))

	    open(unit   = 67,	&
	         File   = trim(scamtec%output_dir)//Trim(filename)//'.txt',   &
                 access = 'sequential',  &
                 Form   = 'formatted', &
                 Status = 'replace'      &
                )

	    write(67,'(A)')'%Analysis    Forecast   Misses  FAlarms  Hits      CSI         POD       FAR       BIAS       ETS'

	    Do i=1, scamtec%ftime_idx-1
	      ai = contingency1(nexp,scamtec%ftime_count(aux))%atime
	      bi = contingency1(nexp,scamtec%ftime_count(aux))%fcst_time(i)
	      ci = contingency1(nexp,scamtec%ftime_count(aux))%misses(i)
	      di = contingency1(nexp,scamtec%ftime_count(aux))%falseAlarms(i)
	      ei = contingency1(nexp,scamtec%ftime_count(aux))%hits(i)
	      fi = contingency1(nexp,scamtec%ftime_count(aux))%csi(i)
	      gi = contingency1(nexp,scamtec%ftime_count(aux))%pod(i)
	      hi = contingency1(nexp,scamtec%ftime_count(aux))%far(i)
	      ji = contingency1(nexp,scamtec%ftime_count(aux))%vies(i)
	      ki = contingency1(nexp,scamtec%ftime_count(aux))%ets(i)
	      
	      write(67,95)ai,bi,ci,di,ei,fi,gi,hi,ji,ki
95            FORMAT(I10,6X,I2,3X,3(I8),2X,5(3X,F8.6))
	    Enddo
  	    close(67)	  
          endif		
        
       DeAllocate(contingency1)
     endif
     
   End Subroutine
!************************************************************************************************************************************** 





!************************************************************************************************************************************** 
   Subroutine mode_finalize(nexp, f)
   ! Subroutine where variables and structures memory is released.
     Implicit None
     ! INPUT PARAMETER:
     integer, intent(in) :: nexp, f ! experiment number

     character(len=*),parameter :: myname='::mode_finalize'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname
    WRITE(*,*)
#endif

     ! Desassociando ponteiros
     if (associated(expfield)) nullify(expfield)
     if (associated(prefield)) nullify(prefield)

     ! Desalocando variaveis 
     if (f .EQ. scamtec%ntime_forecast)   then     
       DeAllocate(inputfield)
       DeAllocate(subfield)
     endif
     DeAllocate(precOriginalField)
     DeAllocate(expOriginalField)         

   End Subroutine mode_finalize 
!**************************************************************************************************************************************



!**************************************************************************************************************************************
  Subroutine mode_run(nexp)
    Implicit None
    integer, intent(in) :: nexp ! experiment number

    character(len=*),parameter :: myname='::mode_run'

	          ! dimensions of the fields to compare and loop variables
    integer :: rowSize, colSize, i, j, t, f, idField  

	 ! Weights of the attributes used in the fuzzy logic
    real :: merg_weight(8), match_weight(8), merging_tresh, matching_tresh, grid_res  

    real, allocatable		:: obsConvField(:,:)    ! Field resulting of convolution process
    real, allocatable		:: expConvField(:,:)    ! Field resulting of convolution process         
    real, allocatable		:: obsRestoreField(:,:) ! Matrix to save original field values where the mask is 1
    real, allocatable		:: expRestoreField(:,:) ! Matrix to save original field values where the mask is 1
       

         			  ! Mask to count objects -> resulting of Object Identification Algorithm
    integer, allocatable   	:: mask(:,:), obs_maskObj(:,:), exp_maskObj(:,:) 

         			  ! Identified objects total (Observation and Forecast)
    integer 			:: prec_nobj, exp_nobj, precObjB, expObjA

				  ! List to save objects and attributes (Observation and Forecast)
    type(attrs), pointer		:: prec_objects(:), exp_objects(:)
    type(atrib_pair), pointer	:: atrib_matched(:)

				  ! Masks resulting of Matching Algorithm (Objects pairs have the same id in each field)
    integer, allocatable   	:: obsMatch_mask(:,:), expMatch_mask(:,:)
				  ! Auxiliary variables used in the Matching Algorithm
    integer			:: fcst_id, obs_id, num, x, y, cont, count_aux
    !type(attrs)			:: objaux    

    ! The matrices are defined: matrix (longitude, latitude)
    rowSize = scamtec%nxpt  ! longitude -> Total rows
    colSize = scamtec%nypt  ! latitude  -> Total columns     
    grid_res = scamtec%gridDesc(10)  ! Fields resolution
     

    ! Attributes weight used in Merging and Matching process
    !weight = (/min_boundary_dist_weight, dif_centroid_weight, area_ratio_weight, perimeter_ratio_weight, dif_angle_weight,            			aspect_ratio_weight, complexity_ratio_weight, int_area_ratio_weight /)
	 
    merg_weight = (/ 4.0, 2.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0 /)
    merging_tresh = 0.7

    match_weight = (/ 4.0, 2.0, 1.0, 0.0, 1.0, 0.0, 0.0, 2.0 /)	 
    matching_tresh = 0.7    

    f = scamtec%ftime_idx 
    print*, 'scamtec%ftime_idx', f
    print*
    call mode_init(nexp,f, count_aux) 

    If  (f .GT. 1) then      

      WRITE(*,*)
      WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
      WRITE(*,'(a72)')'!                          Running MODE                               !'   
      WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
      WRITE(*,*)

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname
    WRITE(*,*)
#endif

         
      
      ! Observation Field Objects Identification 
#ifdef DEBUG
    WRITE(*,*)
    WRITE(6,'(     2A)')'Observation Field Objects Identification'  
#endif

      idField = 0
      ! Subroutine defined in m_mode_objects where convolution, tresholding, identification of objects, attributes calculation and 
      ! merging algorithms are made
      call mode_ObjectIdentf(nexp,idField,rowSize,colSize,precOriginalField,obsConvField,obsRestoreField,merg_weight,merging_tresh,grid_res,mask,obs_maskObj,precObjB,prec_nobj,prec_objects)
      
     !print*
      print*, ' Merged Objects Total = ', prec_nobj  
                 
      ! Subroutine where Original, Convolution, Mask and Restored Observation fields are written to binary files	 		    
      call mode_writeFields(nexp,idField,rowSize,colSize,precOriginalField,obsConvField,mask,obs_maskObj,obsRestoreField)                   

           
      ! Forecast Field Objects Identification 
#ifdef DEBUG
    WRITE(*,*)
    WRITE(6,'(     2A)')'Forecast Field Objects Identification'  
#endif	
 
      idField = 1
      call mode_ObjectIdentf(nexp,idField,rowSize,colSize,expOriginalField,expConvField,expRestoreField,merg_weight,merging_tresh,grid_res,mask,exp_maskObj,expObjA,exp_nobj,exp_objects)
            
      !print*
      print*, ' Merged Objects Total = ' , exp_nobj  
      print*
	   	 
      ! Subroutine where Original, Convolution, Mask and Restored Forecast fields are written to binary files     
      call mode_writeFields(nexp, idField, rowSize, colSize, expOriginalField, expConvField, mask, exp_maskObj, expRestoreField) 
     
     !!!basic statistics only for a test  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call Rest_Basic_Statistics(nexp, count_aux, rowSize, colSize, expRestoreField, obsRestoreField)  !!!!!!!
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef DEBUG
    WRITE(6,'(     2A)')' Calculating attributes of object pairs in the two fields to match them'  
#endif
         
     ! Subroutine defined in m_mode_pairAttrib where observation objects attributes and forecast objects attributes are compared to select pair 
     ! objects         
     call object_matching(nexp,prec_nobj, prec_objects, exp_nobj, exp_objects, match_weight, grid_res, matching_tresh, atrib_matched, cont)

     ! Subroutine where single objects attributes and pair objects attributes are written to text files
     call mode_writeAttrib(nexp, prec_nobj, exp_nobj, cont, prec_objects, exp_objects, atrib_matched)     

     if (cont .GT. 0) then  ! cont = Total of Objects Pairs matched
       !print*
       print*,'Objects Pairs Detected Total=', cont
       print* 

       ! Creating fields with objects pairs
       allocate(obsMatch_mask(rowSize,colSize))
       allocate(expMatch_mask(rowSize,colSize))

       num = 99  !Only for convenience at the time of plotting in Grads
       do j=1, colSize
         do i=1, rowSize
           if (obs_maskObj(i,j) .EQ. 0)then
             obsMatch_mask(i,j) = 0
           else
             obsMatch_mask(i,j) = num    ! objects that were not matched are defined com id = 99 na matriz obsMatch_mask
           endif
         enddo
       enddo

       do j=1, colSize
         do i=1, rowSize
           if (exp_maskObj(i,j) .EQ. 0)then
             expMatch_mask(i,j) = 0
           else
             expMatch_mask(i,j) = num   ! objects that were not matched are defined com id = 99 na matriz expMatch_mask
           endif
         enddo
       enddo
       
       ! Objects pairs are defined with the same id at each field  
       do i=1, cont         
         fcst_id = atrib_matched(i)%id1
         obs_id = atrib_matched(i)%id2
	  
         do j=1, exp_objects(fcst_id)%area
           x = exp_objects(fcst_id)%total_pts(j)%x
           y = exp_objects(fcst_id)%total_pts(j)%y
           expMatch_mask(y,x) = i	   
	 enddo         
         
         do j=1, prec_objects(obs_id)%area
           x = prec_objects(obs_id)%total_pts(j)%x
           y = prec_objects(obs_id)%total_pts(j)%y           
           obsMatch_mask(y,x) = i	   
	 enddo           
       enddo

       ! Subroutine where Forecast Objects and Observation Objects (matched) fields are written to binary files   
       call mode_writeObj(nexp, obsMatch_mask, expMatch_mask) 
  
       Deallocate(obsMatch_mask)
       Deallocate(expMatch_mask)     
     endif  
         

     !*********** Object-based Statistical Índices *********************************
#ifdef DEBUG
    WRITE(6,'(     2A)')' Object-based Statistical Índices'  
#endif

     misses = abs(prec_nobj - cont)
     false_alarms = abs(exp_nobj - cont)
     hits = cont

     !print*
     print*, 'misses', misses
     print*, 'f_alarms', false_alarms
     print*, 'hits', hits

     CSI = REAL(hits) / (REAL(hits) + REAL(misses) + REAL(false_alarms))
     !print*
     print*,  'CSI', CSI

     POD = REAL(hits) / (REAL(hits) + REAL(misses))
     !print*
     print*,  'POD', POD

     FAR = REAL(false_alarms) / (REAL(hits) + REAL(false_alarms))
     !print*
     print*,  'FAR', FAR

     BIAS = (REAL(hits) + REAL(false_alarms)) / (REAL(hits) + REAL(misses))
     !print*
     print*, 'BIAS', BIAS
     print*        

     indices(nexp,scamtec%ftime_count(count_aux))%atime = scamtec%atime
     indices(nexp,scamtec%ftime_count(count_aux))%fcst_time(scamtec%ftime_idx-1) = int(abs(cal2jul(scamtec%atime)-cal2jul(scamtec%ftime))*24)
     indices(nexp,scamtec%ftime_count(count_aux))%misses(scamtec%ftime_idx-1) = misses
     indices(nexp,scamtec%ftime_count(count_aux))%falseAlarms(scamtec%ftime_idx-1) = false_alarms
     indices(nexp,scamtec%ftime_count(count_aux))%hits(scamtec%ftime_idx-1) = hits
     indices(nexp,scamtec%ftime_count(count_aux))%csi(scamtec%ftime_idx-1) = CSI
     indices(nexp,scamtec%ftime_count(count_aux))%pod(scamtec%ftime_idx-1) = POD
     indices(nexp,scamtec%ftime_count(count_aux))%far(scamtec%ftime_idx-1) = FAR
     indices(nexp,scamtec%ftime_count(count_aux))%vies(scamtec%ftime_idx-1) = BIAS   



     ! writing Statistical Índices to text file
     if (scamtec%ftime_idx .EQ. scamtec%ntime_forecast) then
       call mode_write(nexp, count_aux)       
       DeAllocate(indices)       
     endif     

     call mode_finalize(nexp, f)  
   Endif        

  End Subroutine mode_run
!**************************************************************************************************************************************



!**************************************************************************************************************************************
   Subroutine mode_writeFields(nexp, id, rowSize, colSize, Original, Convolution, Mask, MaskObj, Restored)
        Implicit None
        integer, intent(in) :: nexp,  id, rowSize, colSize ! experiment number
	real, allocatable, intent(in) 		:: Original(:,:)
	real, allocatable, intent(in)		:: Convolution(:,:)    ! Field resulting of convolution process
	real, allocatable, intent(in)		:: Restored(:,:)
        integer, allocatable, intent(in)   	:: Mask(:,:), MaskObj(:,:) ! Mask to count objects -> resulting of Object Identification Algorithm
        real, allocatable :: tmp(:,:)
	 
	integer            :: i,j, ier
   logical            :: Opened
	character(len=512) :: filename, fname, fmt
	integer            :: nymd, nhms
    	integer            :: fymd, fhms


        nymd = scamtec%atime/100
        nhms = MOD(scamtec%atime,100) * 10000
        fymd = scamtec%ftime/100
        fhms = MOD(scamtec%ftime,100) * 10000

	If (id .EQ. 0) then  ! wrinting Reference Precipitation MODE Fields in .bin 

	  fname = 'PrecipField'
	  inquire(unit=FUnitOut, opened=Opened)
	  if(.not.Opened) then 
	    filename = trim(fname)//'_'//trim(FNameOut)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))

	    open(unit   = FUnitOut+0,	&
	         File   = trim(scamtec%output_dir)//'/Original'//Trim(filename)//'.bin',   &
	         status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		)
	    open(unit   = FUnitOut+1,	&
	         File   = trim(scamtec%output_dir)//'/Convolution'//Trim(filename)//'.bin',   &
                 status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		)
	    open(unit   = FUnitOut+2,	&
	         File   = trim(scamtec%output_dir)//'/Mask'//Trim(filename)//'.bin',   &
                 status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		)
	    open(unit   = FUnitOut+3,	&
	         File   = trim(scamtec%output_dir)//'/Restored'//Trim(filename)//'.bin',   &
                 status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		)
	    open(unit   = FUnitOut+4,	&
	         File   = trim(scamtec%output_dir)//'/ObjectsMask'//Trim(filename)//'.bin',   &
	         status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		)             

	    write(FUnitOut+0)Original
	    write(FUnitOut+1)Convolution
	    write(FUnitOut+2)real(Mask)	    
	    write(FUnitOut+3)Restored
	    write(FUnitOut+4)real(MaskObj)            

	    Close(FUnitOut+0)
            Close(FUnitOut+1)
    	    Close(FUnitOut+2)
    	    Close(FUnitOut+3)
	    Close(FUnitOut+4)

	    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !   Wrinting ctl of Reference Precipitation MODE Fields
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    

	    open(50, file=trim(scamtec%output_dir)//'/Original'//Trim(filename)//'.ctl', status='unknown')
	    write(50,'(A,A)')'dset ^','Original'//trim(filename)//'.bin'      	    
      	    write(50,'(A)')'options sequential'      	        
     	    write(50,'(A)')'undef -999.9'      
      	    write(50,'(A)')      
      	    write(50,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(50,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(50,'(A)')'zdef    1 linear 0 1'             
            write(50,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(50,'(A)')
            write(50,'(A)')'vars 1'
	    write(50,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(50,'(A)')'endvars'         
            close(50)

            open(51, file=trim(scamtec%output_dir)//'/Convolution'//Trim(filename)//'.ctl', status='unknown')
	    write(51,'(A,A)')'dset ^','Convolution'//trim(filename)//'.bin'      	    
      	    write(51,'(A)')'options sequential'      	        
     	    write(51,'(A)')'undef -999.9'      
      	    write(51,'(A)')      
      	    write(51,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(51,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(51,'(A)')'zdef    1 linear 0 1'             
            write(51,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(51,'(A)')
            write(51,'(A)')'vars 1'
	    write(51,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(51,'(A)')'endvars'         
            close(51)

	    open(52, file=trim(scamtec%output_dir)//'/Mask'//Trim(filename)//'.ctl', status='unknown')
	    write(52,'(A,A)')'dset ^','Mask'//trim(filename)//'.bin'      	    
      	    write(52,'(A)')'options sequential'      	        
     	    write(52,'(A)')'undef -999.9'      
      	    write(52,'(A)')      
      	    write(52,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(52,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(52,'(A)')'zdef    1 linear 0 1'             
            write(52,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(52,'(A)')
            write(52,'(A)')'vars 1'
	    write(52,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(52,'(A)')'endvars'         
            close(52)

	    open(53, file=trim(scamtec%output_dir)//'/Restored'//Trim(filename)//'.ctl', status='unknown')
	    write(53,'(A,A)')'dset ^','Restored'//trim(filename)//'.bin'      	    
      	    write(53,'(A)')'options sequential'      	        
     	    write(53,'(A)')'undef -999.9'      
      	    write(53,'(A)')      
      	    write(53,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(53,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(53,'(A)')'zdef    1 linear 0 1'             
            write(53,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(53,'(A)')
            write(53,'(A)')'vars 1'
	    write(53,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(53,'(A)')'endvars'         
            close(53)	    

	    open(54, file=trim(scamtec%output_dir)//'/ObjectsMask'//Trim(filename)//'.ctl', status='unknown')
	    write(54,'(A,A)')'dset ^','ObjectsMask'//trim(filename)//'.bin'      	    
      	    write(54,'(A)')'options sequential'      	        
     	    write(54,'(A)')'undef -999.9'      
      	    write(54,'(A)')      
      	    write(54,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(54,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(54,'(A)')'zdef    1 linear 0 1'             
            write(54,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(54,'(A)')
            write(54,'(A)')'vars 1'
	    write(54,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(54,'(A)')'endvars'         
            close(54)
	    
	  endif

	Elseif (id .EQ. 1) then  ! Wrinting Experiment Precipitation MODE Fields in .bin

	  fname = 'ExpField'
	  inquire(unit=FUnitOut, opened=Opened)
	  if(.not.Opened) then 
	    filename = trim(fname)//'_'//trim(FNameOut)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))

	    open(unit   = FUnitOut+5,	&
	         File   = trim(scamtec%output_dir)//'/Original'//Trim(filename)//'.bin',   &
	         status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		)
	    open(unit   = FUnitOut+6,	&
	         File   = trim(scamtec%output_dir)//'/Convolution'//Trim(filename)//'.bin',   &
                 status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		)
	    open(unit   = FUnitOut+7,	&
	         File   = trim(scamtec%output_dir)//'/Mask'//Trim(filename)//'.bin',   &
                 status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		)
	    open(unit   = FUnitOut+8,	&
	         File   = trim(scamtec%output_dir)//'/Restored'//Trim(filename)//'.bin',   &
                 status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		)
	    open(unit   = FUnitOut+9,	&
	         File   = trim(scamtec%output_dir)//'/ObjectsMask'//Trim(filename)//'.bin',   &
	         status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		)  

	    write(FUnitOut+5)Original	        
	    write(FUnitOut+6)Convolution
	    write(FUnitOut+7)real(Mask)
	    write(FUnitOut+8)Restored
	    write(FUnitOut+9)real(MaskObj)
	    
            Close(FUnitOut+5)
    	    Close(FUnitOut+6)
    	    Close(FUnitOut+7)
	    Close(FUnitOut+8)
	    Close(FUnitOut+9)

	    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !   Wrinting ctl of Experiment Precipitation MODE Fields
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 	    

	    open(55, file=trim(scamtec%output_dir)//'/Original'//Trim(filename)//'.ctl', status='unknown')
	    write(55,'(A,A)')'dset ^','Original'//trim(filename)//'.bin'      	    
      	    write(55,'(A)')'options sequential'      	        
     	    write(55,'(A)')'undef -999.9'      
      	    write(55,'(A)')      
      	    write(55,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(55,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(55,'(A)')'zdef    1 linear 0 1'             
            write(55,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(55,'(A)')
            write(55,'(A)')'vars 1'
	    write(55,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(55,'(A)')'endvars'         
            close(55)

            open(56, file=trim(scamtec%output_dir)//'/Convolution'//Trim(filename)//'.ctl', status='unknown')
	    write(56,'(A,A)')'dset ^','Convolution'//trim(filename)//'.bin'      	    
      	    write(56,'(A)')'options sequential'      	        
     	    write(56,'(A)')'undef -999.9'      
      	    write(56,'(A)')      
      	    write(56,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(56,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(56,'(A)')'zdef    1 linear 0 1'             
            write(56,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(56,'(A)')
            write(56,'(A)')'vars 1'
	    write(56,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(56,'(A)')'endvars'         
            close(56)

	    open(57, file=trim(scamtec%output_dir)//'/Mask'//Trim(filename)//'.ctl', status='unknown')
	    write(57,'(A,A)')'dset ^','Mask'//trim(filename)//'.bin'      	    
      	    write(57,'(A)')'options sequential'      	        
     	    write(57,'(A)')'undef -999.9'      
      	    write(57,'(A)')      
      	    write(57,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(57,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(57,'(A)')'zdef    1 linear 0 1'             
            write(57,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(57,'(A)')
            write(57,'(A)')'vars 1'
	    write(57,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(57,'(A)')'endvars'         
            close(57)

	    open(58, file=trim(scamtec%output_dir)//'/Restored'//Trim(filename)//'.ctl', status='unknown')
	    write(58,'(A,A)')'dset ^','Restored'//trim(filename)//'.bin'      	    
      	    write(58,'(A)')'options sequential'      	        
     	    write(58,'(A)')'undef -999.9'      
      	    write(58,'(A)')      
      	    write(58,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(58,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(58,'(A)')'zdef    1 linear 0 1'             
            write(58,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(58,'(A)')
            write(58,'(A)')'vars 1'
	    write(58,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(58,'(A)')'endvars'         
            close(58)

	    open(59, file=trim(scamtec%output_dir)//'/ObjectsMask'//Trim(filename)//'.ctl', status='unknown')
	    write(59,'(A,A)')'dset ^','ObjectsMask'//trim(filename)//'.bin'      	    
      	    write(59,'(A)')'options sequential'      	        
     	    write(59,'(A)')'undef -999.9'      
      	    write(59,'(A)')      
      	    write(59,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(59,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(59,'(A)')'zdef    1 linear 0 1'             
            write(59,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(59,'(A)')
            write(59,'(A)')'vars 1'
	    write(59,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(59,'(A)')'endvars'         
            close(59)
	  endif

	Endif
      End Subroutine mode_writeFields
 !**************************************************************************************************************************************


    !**************************************************************************************************************************************
      Subroutine mode_writeObj(nexp, obsMatch_mask, expMatch_mask)
        Implicit None
        integer, intent(in) :: nexp ! experiment number	        
	integer, allocatable   	:: obsMatch_mask(:,:), expMatch_mask(:,:)
	!integer, allocatable :: mask(:,:)

	integer            :: i,j, ier
   logical            :: Opened
	character(len=512) :: filename, fname, fmt
	integer            :: nymd, nhms
    	integer            :: fymd, fhms	

        nymd = scamtec%atime/100
        nhms = MOD(scamtec%atime,100) * 10000
        fymd = scamtec%ftime/100
        fhms = MOD(scamtec%ftime,100) * 10000
	
  	! Wrinting Objects Reference Merged Field in .bin
	fname = 'PrecipField'
	  inquire(unit=FUnitOut+10, opened=Opened)
	  if(.not.Opened) then 
	    filename = trim(fname)//'_'//trim(FNameOut)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))

	    open(unit   = FUnitOut+10,	&
	         File   = trim(scamtec%output_dir)//'/MatchedObjects'//Trim(filename)//'.bin',   &
	         status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		)	    

	    write(FUnitOut+10)real(obsMatch_mask)	    
    	    Close(FUnitOut+10)

	    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !      Wrinting ctl
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	    

	    open(60, file=trim(scamtec%output_dir)//'/MatchedObjects'//Trim(filename)//'.ctl', status='unknown')
	    write(60,'(A,A)')'dset ^','MatchedObjects'//trim(filename)//'.bin'      	    
      	    write(60,'(A)')'options sequential'      	        
     	    write(60,'(A)')'undef -999.9'      
      	    write(60,'(A)')      
      	    write(60,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(60,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(60,'(A)')'zdef    1 linear 0 1'             
            write(60,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(60,'(A)')
            write(60,'(A)')'vars 1'
	    write(60,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(60,'(A)')'endvars'         
            close(60)
	  endif 
		
	fname = 'ExpField'
	  inquire(unit=FUnitOut+11, opened=Opened)
	  if(.not.Opened) then 
	    filename = trim(fname)//'_'//trim(FNameOut)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))

	    open(unit   = FUnitOut+11,	&
	         File   = trim(scamtec%output_dir)//'/MatchedObjects'//Trim(filename)//'.bin',   &
	         status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		)	    

	    write(FUnitOut+11)real(expMatch_mask)	    
    	    Close(FUnitOut+11)

	    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !    writing ctl
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	    open(61, file=trim(scamtec%output_dir)//'/MatchedObjects'//Trim(filename)//'.ctl', status='unknown')
	    write(61,'(A,A)')'dset ^','MatchedObjects'//trim(filename)//'.bin'      	    
      	    write(61,'(A)')'options sequential'      	        
     	    write(61,'(A)')'undef -999.9'      
      	    write(61,'(A)')      
      	    write(61,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(61,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(61,'(A)')'zdef    1 linear 0 1'             
            write(61,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(61,'(A)')
            write(61,'(A)')'vars 1'
	    write(61,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(61,'(A)')'endvars'         
            close(61)
	  endif
	!endif


      End Subroutine
    !**************************************************************************************************************************************
  

    !**************************************************************************************************************************************
      Subroutine mode_writeAttrib(nexp,nobj1,nobj2,nobj3,attrib1,attrib2,attrib3)
	Implicit None
        integer, intent(in) 			:: nexp, nobj1, nobj2, nobj3 ! experiment number
	type(attrs), pointer, intent(in)	:: attrib1(:), attrib2(:)
        type(atrib_pair), pointer, intent(in)	:: attrib3(:)

	integer            :: nparameters, i
   logical            :: Opened
	character(len=512) :: filename, fname, fmt
	integer            :: nymd, nhms
    	integer            :: fymd, fhms
	integer		   :: a,b
	real		   :: l,c,d,e,f,g,h,j(8),k(8)		

        nymd = scamtec%atime/100
        nhms = MOD(scamtec%atime,100) * 10000
        fymd = scamtec%ftime/100
        fhms = MOD(scamtec%ftime,100) * 10000		

	  fname = 'PrecipField'
	  inquire(unit=62, opened=Opened)
	  if(.not.Opened) then 
	    filename = trim(fname)//'_'//trim(FNameOut)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))
	    open(unit   = 62,	&
	         File   = trim(scamtec%output_dir)//'/Attributes'//Trim(filename)//'.txt',   &
                 access = 'sequential',  &
                 Form   = 'formatted', &
                 Status = 'replace'      &
                )
	    write(62,'(A)')'%Object_id    xcent    ycent    area    perimeter    angle    aspect_ratio'
	    do i=1, nobj1
	       write(62,95)attrib1(i)%id,attrib1(i)%xcent,attrib1(i)%ycent,attrib1(i)%area,attrib1(i)%perimeter,attrib1(i)%angle,attrib1(i)%aspect_ratio
95	       FORMAT(5(4X,I6),2(7X,F8.6))
	    enddo
	    close(62)
	  endif

	  fname = 'ExpField'
	  inquire(unit=63, opened=Opened)
	  if(.not.OPened) then 
	    filename = trim(fname)//'_'//trim(FNameOut)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))
	    open(unit   = 63,	&
	         File   = trim(scamtec%output_dir)//'/Attributes'//Trim(filename)//'.txt',   &
                 access = 'sequential',  &
                 Form   = 'formatted', &
                 Status = 'replace'      &
                )
	    write(63,'(A)')'%Object_id    xcent    ycent    area    perimeter    angle    aspect_ratio'
	    do i=1, nobj2
	       write(63,95)attrib2(i)%id,attrib2(i)%xcent,attrib2(i)%ycent,attrib2(i)%area,attrib2(i)%perimeter,attrib2(i)%angle,attrib2(i)%aspect_ratio
!95	       FORMAT(5(4X,I5),2(7X,F8.6))
	    enddo
	    close(63)
	  endif

	if (nobj3 .GT. 0) then
	  fname = 'PairAttributes'
	  inquire(unit=64, opened=Opened)
	  if(.not.Opened) then 
	    filename = trim(fname)//'_'//trim(FNameOut)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))
	    open(unit   = 64,	&
	         File   = trim(scamtec%output_dir)//Trim(filename)//'.txt',   &
                 access = 'sequential',  &
                 Form   = 'formatted', &
                 Status = 'replace'      &
                )
	    write(64,'(A)')'%Fcst_id   Obs_id    BoundDist   cent_dif      area_ratio     angle_dif     aspect_ratio     overlapping    total_interest     InteresP(8)    Conf(8)'
	    do i=1, nobj3
	       a=attrib3(i)%id1
	       b=attrib3(i)%id2
	       l=attrib3(i)%BoundDist
	       c=attrib3(i)%difCent
	       d=attrib3(i)%areaR
	       e=attrib3(i)%difangle
	       f=attrib3(i)%aspectR
               g=attrib3(i)%intAreaR
               h=attrib3(i)%t_interest
	       j=attrib3(i)%interest
	       k=attrib3(i)%conf

!	       write(64,96)a,b,c,d,e,f,g,h,j(1),j(2),j(3),j(4),j(5),j(6),j(7),j(8),k(1),k(2),k(3),k(4),k(5),k(6),k(7),k(8)
!96	       FORMAT(2(2X,I5),6(5X,F10.6))
	       write(64,96)a,b,l,c,d,e,f,g,h
96	       FORMAT(2(2X,I5),6(5X,F10.6))
	    enddo
	    close(64)
	  endif
	endif

      End Subroutine mode_writeAttrib
    !**************************************************************************************************************************************
   

    !**************************************************************************************************************************************
      Subroutine mode_write(nexp, aux)
	Implicit None
        integer, intent(in) :: nexp, aux ! experiment number
	integer            :: i
   logical            :: Opened
	character(len=512) :: filename, fname, fmt
	integer            :: nymd, nhms
    	integer            :: fymd, fhms

	integer(I4B) 	:: a
     	integer	:: b,c,d,e     	
     	real	:: f,g,h,j

        nymd = scamtec%atime/100
        nhms = MOD(scamtec%atime,100) * 10000
        fymd = scamtec%ftime/100
        fhms = MOD(scamtec%ftime,100) * 10000	
        
          fname = 'StatisticIndices'	  

	  inquire(unit=65, opened=Opened)
	  if(.not.Opened) then 
	    filename = trim(fname)//'_'//trim(FNameOut)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))

	    open(unit   = 65,	&
	         File   = trim(scamtec%output_dir)//Trim(filename)//'.txt',   &
                 access = 'sequential',  &
                 Form   = 'formatted', &
                 Status = 'replace'      &
                )

	    write(65,'(A)')'%Analysis    Forecast    Misses   FAlarms  Hits      CSI         POD       FAR       BIAS'

	    Do i=1, scamtec%ftime_idx-1
	      a = indices(nexp,scamtec%ftime_count(aux))%atime
	      b = indices(nexp,scamtec%ftime_count(aux))%fcst_time(i)
	      c = indices(nexp,scamtec%ftime_count(aux))%misses(i)
	      d = indices(nexp,scamtec%ftime_count(aux))%falseAlarms(i)
	      e = indices(nexp,scamtec%ftime_count(aux))%hits(i)
	      f = indices(nexp,scamtec%ftime_count(aux))%csi(i)
	      g = indices(nexp,scamtec%ftime_count(aux))%pod(i)
	      h = indices(nexp,scamtec%ftime_count(aux))%far(i)
	      j = indices(nexp,scamtec%ftime_count(aux))%vies(i)
	      
	      write(65,95)a,b,c,d,e,f,g,h,j
95            FORMAT(I10,6X,I3,3X,3(7X,I4),2X,4(3X,F8.6))
	    Enddo
  	    close(65)	  
          endif		

      End Subroutine mode_write
    !**************************************************************************************************************************************




END MODULE mode

     

