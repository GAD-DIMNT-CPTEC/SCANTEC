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

   USE scamtec_module                 ! module where the structure scantec is defined
   USE SCAM_dataMOD, only : scamdata  ! SCANTEC data matrix
   USE SCAM_Utils
   USE m_mode_objects		      ! module where objects are identified
   USE m_mode_singleAttrib	      ! module where single objects attributes are calculated
   USE m_mode_pairAttrib	      ! module where pair objects attributes are calculated

   IMPLICIT NONE
   PRIVATE

   integer, allocatable :: Idx(:)     ! Array to save undefined points index
   integer              :: nidx       ! Total undefined points
 
   real, pointer     :: prefield(:,:) ! Pointer to save precipitation observation data
   real, pointer     :: expfield(:,:) ! Pointer to save precipitation experiment data 
   
   real, allocatable :: precOriginalField(:,:)  ! Matrix to save precipitation observation data 
   real, allocatable :: expOriginalField(:,:)   ! Matrix to save precipitation experiment data 

   ! Statistical indices
   integer			:: hits, false_alarms, misses
   real				:: CSI, POD, FAR, BIAS

   public :: mode_init
   public :: mode_ObjectIdentf
   public :: mode_run



   Contains

    !**************************************************************************************************************************************
      Subroutine mode_init(nexp)  
      ! Subroutine where variables and structures memory is allocated.     

         Implicit None

	 ! Input Parameter         
	 integer, intent(in) :: nexp      ! experiment number

         integer            :: i, npts      

    	 npts = scamtec%nxpt*scamtec%nypt                 ! scamtec -> variable type definida em scamtec_module.f90

	 Allocate(precOriginalField(scamtec%nypt,scamtec%nxpt))
	 Allocate(expOriginalField(scamtec%nypt,scamtec%nxpt))          
        
         ! transferindo dados de precipitacion 
	 prefield => scamdata(1)%prefield             
         expfield => scamdata(nexp)%expfield           

	 ! Convirtiendo el vector de los datos de precipitacion para una matriz 
	 precOriginalField = RESHAPE(prefield(:,21), (/scamtec%nypt,scamtec%nxpt/))
	 expOriginalField = RESHAPE(expfield(:,hist%tipo_precip), (/scamtec%nypt,scamtec%nxpt/))      

         !open(46,file=trim(scamtec%output_dir)//'/'//'EXP_precip'//'.bin',form='unformatted',status='unknown',access = 'sequential')         
         !write(46)expOriginalField 

         !print*,'mode_init' 

	 !print*,'Min/Max PREFIELD_MODE: ',minval(precOriginalField(:,:)),maxval(precOriginalField(:,:))  

         !print*,'Min/Max EXPFIELD_MODE: ',minval(expOriginalField(:,:)),maxval(expOriginalField(:,:))  
         !stop  

         

      End Subroutine mode_init
    !**************************************************************************************************************************************



    !************************************************************************************************************************************** 
      Subroutine mode_finalize(nexp)
      ! Subroutine where variables and structures memory is released.

	 Implicit None

         ! INPUT PARAMETER:
         integer, intent(in) :: nexp ! experiment number

         ! Desassociando ponteiros
         if (associated(expfield)) nullify(expfield)
         if (associated(prefield)) nullify(prefield)

         ! Desalocando variaveis
         !DeAllocate(Idx)
         DeAllocate(precOriginalField)
         DeAllocate(expOriginalField)

      End Subroutine mode_finalize 
    !**************************************************************************************************************************************



    !**************************************************************************************************************************************
      Subroutine mode_run(nexp)
         Implicit None
         integer, intent(in) :: nexp ! experiment number

			   ! dimensions of the fields to compare and loop variables
         integer 	:: rowSize, colSize, i, j, t, f  

	 		   ! Weights of the attributes used in the fuzzy logic
	 real		:: min_boundary_dist_weight, dif_centroid_weight, area_ratio_weight, perimeter_ratio_weight, dif_angle_weight, aspect_ratio_weight, complexity_ratio_weight, int_area_ratio_weight, weight(8), total_interest_tresh, grid_res         

					  ! Mask to count objects -> resulting of Object Identification Algorithm
         integer, allocatable   	:: mask(:,:), obs_maskObj(:,:), exp_maskObj(:,:) 

					  ! Identified objects total (Observation and Forecast)
         integer 			:: prec_nobj, exp_nobj

					  ! List to save objects and attributes (Observation and Forecast)
         type(attrs), pointer		:: prec_objects(:), exp_objects(:)
         type(atrib_pair), pointer	:: atrib_matched(:)

					  ! Masks resulting of Matching Algorithm (Objects pairs have the same id in each field)
	 integer, allocatable   	:: obsMatch_mask(:,:), expMatch_mask(:,:)
					  ! Auxiliary variables used in the Matching Algorithm
	 integer			:: fcst_id, obs_id, num, x, y, cont 
         type(attrs)			:: objaux

!					  ! Statistical indices
!	 integer			:: hits, false_alarms, misses
!	 real				:: CSI, POD, FAR, BIAS

         call mode_init(nexp)

         rowSize = scamtec%nypt
         colSize = scamtec%nxpt          

         grid_res = 0.400  ! Este valor creo q es dom(I)%x  

         ! Attributes weight used in Merging and Matching process
         min_boundary_dist_weight = 0.0
	 dif_centroid_weight = 4.0
	 area_ratio_weight = 2.0
	 perimeter_ratio_weight = 0.0
	 dif_angle_weight = 1.0
	 aspect_ratio_weight = 0.0
	 complexity_ratio_weight = 0.0
	 int_area_ratio_weight = 2.0
	 total_interest_tresh = 0.5

         weight(1) = min_boundary_dist_weight
	 weight(2) = dif_centroid_weight 
	 weight(3) = area_ratio_weight
	 weight(4) = perimeter_ratio_weight
	 weight(5) = dif_angle_weight
	 weight(6) = aspect_ratio_weight
	 weight(7) = complexity_ratio_weight
	 weight(8) = int_area_ratio_weight        
	 
	 ! VER DESPUES SI ES PRECISO USAR ESTAS VARIABLES           
         f = scamtec%ftime_idx 
         
	      ! Subroutine defined in m_mode_objects where convolution, tresholding, identification of objects, attributes calculation and 
	      ! merging algorithms are made
	 ! Observation
         call mode_ObjectIdentf(rowSize, colSize, precOriginalField, weight, total_interest_tresh, grid_res, mask, obs_maskObj, prec_nobj, prec_objects)
            !print*
	    !print*, 'prec_nobj', prec_nobj
            ! imprimiendo campo objeto.
	    !DO i=1, rowSize            
               !write(*,*) (mask(i,j), j=1, colSize)         
            !ENDDO

	    !print*
	    !DO i=1, rowSize            
               !write(*,*) (obs_maskObj(i,j), j=1, colSize)         
            !ENDDO                        
         
	 ! Forecast
         call mode_ObjectIdentf(rowSize, colSize, expOriginalField, weight, total_interest_tresh, grid_res, mask, exp_maskObj, exp_nobj, exp_objects)
            !print*
	    !print*, 'exp_nobj', exp_nobj
	    !DO i=1, rowSize            
               !write(*,*) (mask(i,j), j=1, colSize)         
            !ENDDO

	    !print*
	    !DO i=1, rowSize            
               !write(*,*) (exp_maskObj(i,j), j=1, colSize)         
            !ENDDO	    
         
	 
         If  (f .GT. 1) then
	     ! Subroutine defined in m_mode_pairAttrib where observation objects attributes and forecast objects attributes are compared to select pair objects         
           call object_matching(prec_nobj, prec_objects, exp_nobj, exp_objects, weight, grid_res, total_interest_tresh, atrib_matched, cont)
         !stop
           allocate(obsMatch_mask(rowSize,colSize))
	   allocate(expMatch_mask(rowSize,colSize))
	 
!          obsMatch_mask = obs_maskObj
!	   expMatch_mask = exp_maskObj

           obsMatch_mask = 0
	   expMatch_mask = 0	 
         
           num = 1
	   do i=1, cont
	     fcst_id = atrib_matched(i)%id1
	     obs_id = atrib_matched(i)%id2           

	     objaux = prec_objects(obs_id)
	     !print*, 'prec_objects(obs_id)total_pts', prec_objects(obs_id)%total_pts
             do j=1, objaux%area
	       x = objaux%total_pts(j)%x
	       y = objaux%total_pts(j)%y
	     
	       obsMatch_mask(y,x) = num
	     enddo

	     objaux = exp_objects(fcst_id)

             do j=1, objaux%area
	       x = objaux%total_pts(j)%x
	       y = objaux%total_pts(j)%y
	     
	       expMatch_mask(y,x) = num
	     enddo
             num = num + 1	   
           enddo
	   !print*
	   !print*, 'num', num

	   !print*
           !DO i=1, rowSize            
             !write(*,*) (obsMatch_mask(i,j), j=1, colSize)         
           !ENDDO

	   !print*
	   !DO i=1, rowSize            
             !write(*,*) (expMatch_mask(i,j), j=1, colSize)         
           !ENDDO	 

	   call mode_finalize(nexp)         

	 !*********** Object-based Statistical Índices *********************************

	 misses = abs(prec_nobj - cont)
	 false_alarms = abs(exp_nobj - cont)
	 hits = cont

	 print*
	 print*, 'misses', misses, 'false_alarms', false_alarms, 'hits', hits

	 CSI = REAL(hits) / (REAL(hits) + REAL(misses) + REAL(false_alarms))
	 print*
	 print*,  'CSI', CSI

	 POD = REAL(hits) / (REAL(hits) + REAL(misses))
	 print*
	 print*,  'POD', POD

	 FAR = REAL(false_alarms) / (REAL(hits) + REAL(false_alarms))
	 print*
	 print*,  'FAR', FAR

	 BIAS = (REAL(hits) + REAL(false_alarms)) / (REAL(hits) + REAL(misses))
	 print*
	 print*, 'BIAS', BIAS
        Endif

      End Subroutine mode_run
    !**************************************************************************************************************************************
  

    !**************************************************************************************************************************************
      Subroutine mode_write(nexp)
	Implicit None
        integer, intent(in) :: nexp ! experiment number


      End Subroutine mode_write
    !**************************************************************************************************************************************
  




END MODULE mode

     

