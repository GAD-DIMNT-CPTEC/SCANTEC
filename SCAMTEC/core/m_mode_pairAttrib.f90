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

! MODULE: m_mode_pairAttrib.f90
!
! DESCRIPTON:
! In this module are defined the subroutine to calculate pairs object attributes.
! Interest values for each attribute are calculated, which are between zero (no interest) 
! and one (maximum interest). Then confidence values are calculated and weights are 
! assigned to each attribute. Finally, a value of total interest is calculated, a total 
! interest threshold are defined; pairs of objects above the threshold are merged if they 
! are in the same field, or are matched if they are in different fields. 


MODULE m_mode_pairAttrib
   USE scamtec_module
   USE SCAM_Utils
   USE time_module, only: jul2cal, cal2jul 	! Time operations
   USE m_string                      		! string manipulations
   USE m_die  
   USE m_mode_singleAttrib	! module where single objects attributes are calculated

   type atrib_pair
      real		:: BoundDist, difCent, areaR, perR, difAngle
      real		:: aspectR, confAngle, complexR, t_interest, objSize, intAreaR, interest(8), conf(8)
      integer		:: near_x1, near_y1, near_x2, near_y2, id1, id2      
   end type atrib_pair

   type merge_id
      integer 		:: id1, id2
   end type merge_id
      
   type functions
      integer 		:: id1, id2
      real		:: interest(8),conf(8),t_interest
   end type functions
      type(functions), pointer	:: vector_functions(:,:)  

   character(len=512) :: FNameOut2 = '%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2'

   public :: min_set_distance
   public :: min_set_distance_match
   public :: ratio_function
   public :: total_interest
   public :: quick_sort_original
   public :: merging
   public :: merging_new
   public :: object_matching

   contains

   !**************************************************************************************************************************************
    ! Determine the minimum boundary distance between two objects and the nearest point in the merging steps 
      Subroutine min_set_distance(set1, set2, near_x1, near_y1, near_x2, near_y2, per1, per2, min_distance)
	 Implicit None
	 type(point), pointer, intent(in)	:: set1(:), set2(:)
	 integer, intent(in)			:: per1, per2

	 integer, intent(out)			:: near_x1, near_y1, near_x2, near_y2
	 real, intent(out)			:: min_distance

	 integer	:: i,j
	 real		:: distance

	 min_distance = 9999

	 Do i=1, per1
	    Do j=1, per2	       
	       distance = (set1(i)%x - set2(j)%x)**2 + (set1(i)%y - set2(j)%y)**2
	       distance = SQRT(distance)	       
	       if (min_distance .GT. distance) then
		  min_distance = distance

	     	  near_x1 = set1(i)%x
		  near_y1 = set1(i)%y
		  near_x2 = set2(j)%x
	 	  near_y2 = set2(j)%y
	       endif
	    Enddo
	 Enddo
	 return
      End Subroutine
   !**************************************************************************************************************************************



   !**************************************************************************************************************************************
    ! Determine the minimum boundary distance between two objects and the nearest point in the merging steps 
      Subroutine min_set_distance_match(set1, set2, per1, per2, min_distance)
	 Implicit None
	 type(point), pointer, intent(in)	:: set1(:), set2(:)
	 integer, intent(in)			:: per1, per2	 
	 real, intent(out)			:: min_distance

	 integer	:: i,j
	 real		:: distance

	 min_distance = 9999

	 Do i=1, per1
	    Do j=1, per2	       
	       distance = (set1(i)%x - set2(j)%x)**2 + (set1(i)%y - set2(j)%y)**2
	       distance = SQRT(distance)	       
	       if (min_distance .GT. distance) then
		  min_distance = distance
	       endif
	    Enddo
	 Enddo
	 return
      End Subroutine
   !**************************************************************************************************************************************



   !**************************************************************************************************************************************
    !to determine intersection area / union area//
     Subroutine int_area_ratio(area_exp, pts_exp, area_obs, pts_obs, ratio)
       Implicit None
       integer, intent(in)		:: area_exp, area_obs
       type(point), pointer, intent(in)	:: pts_exp(:), pts_obs(:)
       real, intent(out)		:: ratio

       integer	:: i, j, cont

       cont = 0

       Do i=1, area_exp
	 do j=0, area_obs
           if((pts_exp(i)%x .EQ. pts_obs(j)%x) .AND. (pts_exp(i)%y .EQ. pts_obs(j)%y))then
	     cont = cont + 1
           endif
         enddo
       Enddo

       ratio = REAL(cont)/(REAL(area_exp) + REAL(area_obs) - REAL(cont))

     End Subroutine
   !**************************************************************************************************************************************



   !**************************************************************************************************************************************
    ! determine the ratio of two values
      Subroutine ratio_function(first_atrib, second_atrib, atrib_ratio)
	 Implicit None
	 real, intent(in)	:: first_atrib, second_atrib
	 real, intent(out)	:: atrib_ratio

         if (first_atrib .LE. second_atrib) then
	    atrib_ratio = first_atrib/second_atrib
	 else
	    atrib_ratio = second_atrib/first_atrib
	 endif

         return
      End Subroutine
   !**************************************************************************************************************************************


   !**************************************************************************************************************************************
    ! determine the interest value of attributes defined by differences
      Subroutine interest_function_dif(diference, sup_limit, inf_limit, interest)
	 Implicit None
	 real, intent(in)	:: sup_limit, inf_limit
	 real, intent(in)	:: diference
	 real, intent(out)	:: interest
	 real			:: aux

	 if (diference .LT. 0) then
	    aux = (-1)*diference
	 else
	    aux = diference
	 endif

	 !if ( (diference .GT. sup_limit) .OR. (diference .LT. 0) ) then
         if (aux .GE. sup_limit) then
	    interest = 0.0
	 else if (aux .LE. inf_limit) then
	    interest = 1.0
	 else
	    interest = (sup_limit - aux)/(sup_limit - inf_limit)
	 endif

         return
      End Subroutine
   !**************************************************************************************************************************************


   !**************************************************************************************************************************************
    ! determine the interest value of attributes defined by ratio
      Subroutine interest_function_ratio(ratio, sup_limit, interest)
	 Implicit None
	 real, intent(in)	:: ratio, sup_limit	 
	 real, intent(out)	:: interest
	 
	 if (ratio .GE. sup_limit) then
	    interest = 1.0
	 else if (ratio .LE. 0.0) then
	    interest = 0.0
	 else
	    interest = ratio/sup_limit
	 endif

         return
      End Subroutine
   !**************************************************************************************************************************************


   !**************************************************************************************************************************************
    ! Function that determine the total interest value between two objects
      Subroutine total_interest(min_boundary_dist, dif_centroid, area_ratio, perimeter_ratio, dif_angle, aspect_ratio, complexity_ratio, int_area_ratio, conf_angle, obj_size, weight, grid_res, interest, conf, total)
	 Implicit None
	 real, intent(in)	:: min_boundary_dist, dif_centroid, area_ratio, perimeter_ratio, dif_angle, aspect_ratio, complexity_ratio, int_area_ratio, conf_angle, obj_size, weight(8), grid_res	 
	 real, intent(out)	:: interest(8), conf(8), total
	 
	 real 			:: sup_limit(8), inf_limit(8), total_num, total_denom, interest_aux
	 real, parameter   	:: pi=3.141592654
	 integer		:: i	 
	  
	 sup_limit = (/ (400/(grid_res*111)), (600/(grid_res*111)), 0.8, 0.8, (pi/2.0), 0.8, 0.8, 0.25 /)
	 inf_limit = (/ (1.0), (40/(grid_res*111)), 0.0, 0.0, (pi/6.0), 0.0, 0.0, 0.0 /)

	 
	 interest = 0
	 total = 0.0
	 total_num = 0.0
         total_denom = 0.0

	 conf = (/ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 /)
         conf(2) = area_ratio
	 conf(5) = conf_angle
	 
	 if (weight(1) .GE. 0.1) then
	    call interest_function_dif(min_boundary_dist, sup_limit(1), inf_limit(1), interest_aux)
	    interest(1) = interest_aux
	 endif

	 if (weight(2) .GE. 0.1) then
	    call interest_function_dif(dif_centroid, sup_limit(2), inf_limit(2), interest_aux)
	    interest(2) = interest_aux
	 endif

	 if (weight(3) .GE. 0.1) then
	    call interest_function_ratio(area_ratio, sup_limit(3), interest_aux)
	    interest(3) = interest_aux
	 endif

	 if (weight(4) .GE. 0.1) then
	    call interest_function_ratio(perimeter_ratio, sup_limit(4), interest_aux)
	    interest(4) = interest_aux
	 endif

	 if (weight(5) .GE. 0.1) then
	    call interest_function_dif(dif_angle, sup_limit(5), inf_limit(5), interest_aux)
	    interest(5) = interest_aux
	 endif

	 if (weight(6) .GE. 0.1) then
	    call interest_function_ratio(aspect_ratio, sup_limit(6), interest_aux)
	    interest(6) = interest_aux
	 endif

	 if (weight(7) .GE. 0.1) then
	    call interest_function_ratio(complexity_ratio, sup_limit(7), interest_aux)
	    interest(7) = interest_aux
	 endif

	 if (weight(8) .GE. 0.1) then
	    call interest_function_ratio(int_area_ratio, sup_limit(8), interest_aux)
	    interest(8) = interest_aux
	 endif
         
	 Do i=1, 8
	    if (weight(i) .GT. 0.1) then
	       total_num = total_num + weight(i)*conf(i)*interest(i)
	       total_denom = total_denom + weight(i)*conf(i)
	    endif
	 Enddo

	 total = total_num/total_denom

         return
      End Subroutine
   !**************************************************************************************************************************************


   !**************************************************************************************************************************************
    ! Quicksort algorithm to sort arrays counterclockwise
      Recursive Subroutine quick_sort_original(arr, low, high) 

	Implicit None
	type(atrib_pair), pointer, intent(inout)	:: arr(:)
	integer, intent(in)			:: low, high

	integer					:: pivot, i, j, size_line, tempx, tempy
	
	real		:: BoundDist, difCent, areaR, perR, difAngle
        real		:: aspectR, confAngle, complexR, t_interest, objSize, intAreaR
        integer		:: near_x1, near_y1, near_x2, near_y2, id1, id2
        real		:: interest(8)	

	if (low .lT. high) then
	   pivot = low
	   i = low
	   j = high

           do while (i .LT. j)	      
	      
	      do while ( (arr(i)%t_interest .GE. arr(pivot)%t_interest) .AND. (i .LT. high) )		 
		 i=i+1	         
	      enddo
	          
	      do while (arr(j)%t_interest .LT. arr(pivot)%t_interest)		 
	         j=j-1	         
	      enddo
	      
	      if (i .LT. j) then		 
		 BoundDist = arr(i)%BoundDist	   
		 difCent = arr(i)%difCent
		 areaR = arr(i)%areaR
		 perR  = arr(i)%perR
		 difAngle = arr(i)%difAngle
		 aspectR = arr(i)%aspectR
		 confAngle = arr(i)%confAngle
	 	 complexR = arr(i)%complexR
		 t_interest = arr(i)%t_interest
		 objSize = arr(i)%objSize
		 intAreaR = arr(i)%intAreaR
		 near_x1 = arr(i)%near_x1
		 near_y1 = arr(i)%near_y1
	         near_x2 = arr(i)%near_x2
		 near_y2 = arr(i)%near_y2
		 id1 = arr(i)%id1
		 id2 = arr(i)%id2
		 !interest = arr(i)%interest

	         arr(i)%BoundDist = arr(j)%BoundDist
		 arr(i)%difCent = arr(j)%difCent
		 arr(i)%areaR = arr(j)%areaR
		 arr(i)%perR = arr(j)%perR
		 arr(i)%difAngle = arr(j)%difAngle
		 arr(i)%aspectR = arr(j)%aspectR
		 arr(i)%confAngle = arr(j)%confAngle
	 	 arr(i)%complexR = arr(j)%complexR
		 arr(i)%t_interest = arr(j)%t_interest
		 arr(i)%objSize = arr(j)%objSize
		 arr(i)%intAreaR = arr(j)%intAreaR
		 arr(i)%near_x1 = arr(j)%near_x1
		 arr(i)%near_y1 = arr(j)%near_y1
	         arr(i)%near_x2 = arr(j)%near_x2
		 arr(i)%near_y2 = arr(j)%near_y2
		 arr(i)%id1 = arr(j)%id1
		 arr(i)%id2 = arr(j)%id2
		 !arr(i)%interest = arr(j)%interest

	         arr(j)%BoundDist = BoundDist
		 arr(j)%difCent = difCent
		 arr(j)%areaR = areaR
		 arr(j)%perR = perR
		 arr(j)%difAngle = aspectR
		 arr(j)%confAngle = confAngle
	 	 arr(j)%complexR = complexR
		 arr(j)%t_interest = t_interest
		 arr(j)%objSize = objSize
		 arr(j)%intAreaR = intAreaR
		 arr(j)%near_x1 = near_x1
		 arr(j)%near_y1 = near_y1
	         arr(j)%near_x2 = near_x2
		 arr(j)%near_y2 = near_y2
		 arr(j)%id1 = id1
		 arr(j)%id2 = id2
	 	 !arr(j)%interest = interest
	      endif	      
	   enddo

	   BoundDist = arr(pivot)%BoundDist	   
	   difCent = arr(pivot)%difCent
	   areaR = arr(pivot)%areaR
	   perR  = arr(pivot)%perR
	   difAngle = arr(pivot)%difAngle
	   aspectR = arr(pivot)%aspectR
	   confAngle = arr(pivot)%confAngle
	   complexR = arr(pivot)%complexR
	   t_interest = arr(pivot)%t_interest
	   objSize = arr(pivot)%objSize
	   intAreaR = arr(pivot)%intAreaR
	   near_x1 = arr(pivot)%near_x1
	   near_y1 = arr(pivot)%near_y1
	   near_x2 = arr(pivot)%near_x2
	   near_y2 = arr(pivot)%near_y2
	   id1 = arr(pivot)%id1
	   id2 = arr(pivot)%id2

	   arr(pivot)%BoundDist = arr(j)%BoundDist
	   arr(pivot)%difCent = arr(j)%difCent
	   arr(pivot)%areaR = arr(j)%areaR
	   arr(pivot)%perR = arr(j)%perR
	   arr(pivot)%difAngle = arr(j)%difAngle
	   arr(pivot)%aspectR = arr(j)%aspectR
	   arr(pivot)%confAngle = arr(j)%confAngle
	   arr(pivot)%complexR = arr(j)%complexR
	   arr(pivot)%t_interest = arr(j)%t_interest
	   arr(pivot)%objSize = arr(j)%objSize
	   arr(pivot)%intAreaR = arr(j)%intAreaR
	   arr(pivot)%near_x1 = arr(j)%near_x1
	   arr(pivot)%near_y1 = arr(j)%near_y1
	   arr(pivot)%near_x2 = arr(j)%near_x2
	   arr(pivot)%near_y2 = arr(j)%near_y2
	   arr(pivot)%id1 = arr(j)%id1
	   arr(pivot)%id2 = arr(j)%id2

	   arr(j)%BoundDist = BoundDist
	   arr(j)%difCent = difCent
	   arr(j)%areaR = areaR
	   arr(j)%perR = perR
	   arr(j)%difAngle = aspectR
	   arr(j)%confAngle = confAngle
	   arr(j)%complexR = complexR
	   arr(j)%t_interest = t_interest
	   arr(j)%objSize = objSize
	   arr(j)%intAreaR = intAreaR
	   arr(j)%near_x1 = near_x1
	   arr(j)%near_y1 = near_y1
	   arr(j)%near_x2 = near_x2
	   arr(j)%near_y2 = near_y2
	   arr(j)%id1 = id1
	   arr(j)%id2 = id2   
	   
	   call quick_sort_original(arr, low, j-1)	            
	   call quick_sort_original(arr, j+1, high)

	endif        
	
	return
      End Subroutine
    !**************************************************************************************************************************************


    !**************************************************************************************************************************************
      Subroutine merging(atrib_pair_merge, objects, total_obj, maskObj)

	Implicit None
	type(atrib_pair), pointer, intent(in) 	:: atrib_pair_merge
	type(attrs), pointer, intent(inout)	:: objects(:)
	integer, allocatable, intent(inout)     :: maskObj(:,:)
	integer, intent(inout)			:: total_obj

	character(len=*),parameter 		:: myname='::merging'
	integer					:: i, j, k, l, nObj, id, xcent, ycent, area, perimeter, area_hull, x, y, cont
	real					:: angle, aspect_ratio, complexity, temp
	type(attrs), pointer			:: objects_temp(:), new_object
	type(point), pointer			:: pts_per(:), total_pts(:)

#ifdef DEBUG
    WRITE(6,'(     2A)')'  Hello from ', myname
#endif
	
	nObj = total_obj	
	allocate(objects_temp(nObj))
	Do i=1, nObj	   
	   allocate(objects_temp(i)%pts_per(objects(i)%perimeter))
	   allocate(objects_temp(i)%total_pts(objects(i)%area))
	   objects_temp(i) = objects(i)
	Enddo

	id = 1	
	area = objects(atrib_pair_merge%id1)%area + objects(atrib_pair_merge%id2)%area
	perimeter = objects(atrib_pair_merge%id1)%perimeter + objects(atrib_pair_merge%id2)%perimeter   
	
	allocate(pts_per(perimeter))	
        l=0
	Do i=1, objects(atrib_pair_merge%id1)%perimeter
	   pts_per(i) = objects(atrib_pair_merge%id1)%pts_per(i)	   
	   l=l+1	  
	Enddo 	
	k=0
	Do j=l+1, perimeter
	   k=k+1
	   pts_per(j) = objects(atrib_pair_merge%id2)%pts_per(k)	   
	Enddo

	allocate(total_pts(area))
        l=0
	Do i=1, objects(atrib_pair_merge%id1)%area
	   total_pts(i) = objects(atrib_pair_merge%id1)%total_pts(i)
	   l=l+1
	Enddo
	k=0
	Do j=l+1, area
	   k=k+1
	   total_pts(j) = objects(atrib_pair_merge%id2)%total_pts(k)	
	Enddo

	cont=1
	Do i=1, nObj
	   if ( (objects_temp(i)%id .NE. atrib_pair_merge%id1) .AND. (objects_temp(i)%id .NE. atrib_pair_merge%id2)) then	      
              cont = cont + 1   
	   endif
	Enddo

	deallocate(objects)
	allocate(objects(cont))	
	allocate(objects(1)%pts_per(perimeter))
	allocate(objects(1)%total_pts(area))

        objects(1)%id = id
	objects(1)%area = area
	objects(1)%perimeter = perimeter
        objects(1)%pts_per = pts_per
	objects(1)%total_pts = total_pts
	
	call x_centroide(total_pts, area, xcent)	
	objects(1)%xcent = xcent

	call y_centroide(total_pts, area, ycent)	
	objects(1)%ycent = ycent

	area_hull = 9999
        objects(1)%area_hull = area_hull

	call object_angle(total_pts, xcent, ycent, area, angle)	
	objects(1)%angle = angle

	call object_Aspect_Ratio(total_pts, xcent, ycent, area, angle, aspect_ratio)	
	objects(1)%aspect_ratio = aspect_ratio

	complexity = 9999.0	
	objects(1)%complexity = complexity	

	total_obj=1
	Do i=1, nObj
	   if ( (objects_temp(i)%id .NE. atrib_pair_merge%id1) .AND. (objects_temp(i)%id .NE. atrib_pair_merge%id2)) then	      
              total_obj = total_obj + 1
	      allocate(objects(total_obj)%pts_per(objects_temp(i)%perimeter))
	      allocate(objects(total_obj)%total_pts(objects_temp(i)%area)) 
	      objects(total_obj) = objects_temp(i)	      
	      objects(total_obj)%id = total_obj	      
	   endif
	Enddo	
	
        maskObj= 0
	Do i=1, total_obj	  
	  id = objects(i)%id
	  area = objects(i)%area	  
	  Do j=1, area	    
	    x = objects(i)%total_pts(j)%x
	    y = objects(i)%total_pts(j)%y	    
	    maskObj(y,x) = id		    
	  Enddo
	Enddo 	

        deallocate(objects_temp)	
	
	return
      End Subroutine
    !**************************************************************************************************************************************




    !**************************************************************************************************************************************
      Subroutine object_matching(nexp,prec_nobj, prec_objects, exp_nobj, exp_objects, weight, grid_res, total_interest_tresh, atrib_matched, cont)
	Implicit None
	integer, intent(in)			:: nexp, prec_nobj, exp_nobj
	type(attrs), pointer, intent(in)	:: prec_objects(:), exp_objects(:)
        real, intent(in)    			:: weight(8), grid_res, total_interest_tresh
	type(atrib_pair), pointer, intent(out)	:: atrib_matched(:)
	integer, intent(out)			:: cont

	character(len=*),parameter 		:: myname='::object_matching'
        type(atrib_pair), pointer	:: match(:,:), atrib_tresh(:), atrib_treshAux(:)
	integer				:: flag, inter, i, j, cnt, aux, exists 
	real				:: min_distance, dif_centroid_aux, first_atrib, second_atrib, atrib_ratio, ratio, total, interest(8),conf(8), conf_i, conf_j
	real				:: MMI, MMIF, MMIO, sumMIF, sumMIO
	real, allocatable		:: MIF(:), MIO(:)
        

#ifdef DEBUG
    WRITE(6,'(     2A)')' Hello from ', myname
#endif
	
        allocate(match(exp_nobj,prec_nobj))	
	allocate(atrib_treshAux(exp_nobj*prec_nobj))
        allocate(vector_functions(exp_nobj,prec_nobj))
	!allocate(MMI(exp_nobj,prec_nobj))

        flag = 1
        aux = 0 
 	cnt = 0	

	Do i=1, exp_nobj
	  do j=1, prec_nobj
            match(i,j)%id1 = exp_objects(i)%id
	    match(i,j)%id2 = prec_objects(j)%id	 
	    !print*, 'match(i,j)%id1', match(i,j)%id1
	    !print*, 'match(i,j)%id2', match(i,j)%id2  

	    call min_set_distance_match(exp_objects(i)%pts_per, prec_objects(j)%pts_per, exp_objects(i)%perimeter, prec_objects(j)%perimeter, min_distance)
	    ! min_boundary_dist
	    match(i,j)%BoundDist = min_distance	    
	    !print*, 'match(i,j)%BoundDist', match(i,j)%BoundDist
	    ! dif_centroid
	    dif_centroid_aux = (exp_objects(i)%xcent - prec_objects(j)%xcent)**2 + (exp_objects(i)%ycent - prec_objects(j)%ycent)**2
	    match(i,j)%difCent = SQRT(dif_centroid_aux)	    
	    !print*, 'match(i,j)%difCent', match(i,j)%difCent
	    ! area_ratio
	    first_atrib = exp_objects(i)%area
	    second_atrib = prec_objects(j)%area
	    call ratio_function(first_atrib, second_atrib, atrib_ratio)
	    match(i,j)%areaR = atrib_ratio	    
	    !print*, 'match(i,j)%areaR', match(i,j)%areaR
	    ! Objects size
	    match(i,j)%objSize = SQRT(first_atrib) + SQRT(second_atrib)	    

            ! perimeter_ratio
	    first_atrib = exp_objects(i)%perimeter
	    second_atrib = prec_objects(j)%perimeter
	    call ratio_function(first_atrib, second_atrib, atrib_ratio)
	    match(i,j)%perR = atrib_ratio	    

	    ! angle diference
	    match(i,j)%difAngle = exp_objects(i)%angle - prec_objects(j)%angle
	    if (match(i,j)%difAngle .LT. 0.0) then
	      match(i,j)%difAngle = (-1)*match(i,j)%difAngle
	    endif	    
	    !print*, 'match(i,j)%difAngle', match(i,j)%difAngle
	    ! aspect_ratio 
	    first_atrib = exp_objects(i)%aspect_ratio
	    second_atrib = prec_objects(j)%aspect_ratio
	    call ratio_function(first_atrib, second_atrib, atrib_ratio)
	    match(i,j)%aspectR = atrib_ratio	    
	    !print*, 'match(i,j)%aspectR', match(i,j)%aspectR
   	    ! conf_angle
	    !if (exp_objects(i)%aspect_ratio .LT. prec_objects(j)%aspect_ratio) then
            conf_i = ((exp_objects(i)%aspect_ratio-1)**2 / (((exp_objects(i)%aspect_ratio)**2)+1))**0.3
	    !else 
	    conf_j = ((prec_objects(j)%aspect_ratio-1)**2 / (((prec_objects(j)%aspect_ratio)**2)+1))**0.3
	    !endif
      	    match(i,j)%confAngle = SQRT(conf_i*conf_j)

            ! complexity ratio
	    first_atrib = exp_objects(i)%complexity
	    second_atrib = prec_objects(j)%complexity
	    call ratio_function(first_atrib, second_atrib, atrib_ratio)
	    match(i,j)%complexR = atrib_ratio	    

            call int_area_ratio(exp_objects(i)%area, exp_objects(i)%total_pts, prec_objects(j)%area, prec_objects(j)%total_pts, ratio)
            match(i,j)%intAreaR = ratio	    
	    !print*, 'match(i,j)%intAreaR', match(i,j)%intAreaR
            ! TOTAL INTERES
 call total_interest(match(i,j)%BoundDist,match(i,j)%difCent,match(i,j)%areaR,match(i,j)%perR,match(i,j)%difAngle,match(i,j)%aspectR,match(i,j)%complexR,match(i,j)%intAreaR,match(i,j)%confAngle,match(i,j)%objSize,weight,grid_res,interest,conf,total)
	    match(i,j)%interest = interest
	    match(i,j)%conf = conf
	    match(i,j)%t_interest = total

	    !print*, 'match(i,j)%interest', match(i,j)%interest 
	    !print*, 'match(i,j)%conf', match(i,j)%conf
	    !print*, 'match(i,j)%t_interest', match(i,j)%t_interest 	    
	    
            if (match(i,j)%t_interest .GE. total_interest_tresh) then
	      aux = aux + 1              
	      atrib_treshAux(aux) = match(i,j)		     
	    endif 

            vector_functions(i,j)%id1 =  match(i,j)%id1
            vector_functions(i,j)%id2 =  match(i,j)%id2
	    vector_functions(i,j)%interest =  interest 
	    vector_functions(i,j)%conf = conf
	    vector_functions(i,j)%t_interest = match(i,j)%t_interest      
	  enddo
        Enddo

        

	! LOOP PARA CALCULAR O INTERES MAXIMO MEDIO
	ALLOCATE(MIF(exp_nobj))
	ALLOCATE(MIO(prec_nobj))
	sumMIF =0
	Do i=1, exp_nobj
	  MIF(i) = maxval(match(i,:)%t_interest) 
	  sumMIF = sumMIF +  MIF(i)  
        Enddo	
	sumMIO =0
	do j=1, prec_nobj
	  MIO(j) = maxval(match(:,j)%t_interest) 
	  sumMIO = sumMIO +  MIO(j)    
	enddo
	
	MMIF = sumMIF/exp_nobj
	MMIO = sumMIO/prec_nobj
	MMI = (sumMIF + sumMIO)/(exp_nobj + prec_nobj)	
	call write_MMI(nexp,prec_nobj,exp_nobj,MMI,MMIF,MMIO)
	!---------------------------------------------------------------------------------------


        !call attrib_functions(nexp,prec_nobj,exp_nobj)

	If (aux .EQ. 0) then
	  print*, 'No objects pairs found.'
	  cont = 0
	else
	  allocate(atrib_tresh(aux))
	  atrib_tresh = atrib_treshAux

          cnt = aux	  
	  call quick_sort_original(atrib_tresh, 1, cnt)		  

	  allocate(atrib_matched(cnt))

	  atrib_matched(1) = atrib_tresh(1)
	  cont=1	  	   
	  do i=2, cnt	    
	    j=i-1
	    exists = 0
	    do while ((j .GE. 1) .AND. (exists .EQ. 0)) 	      
	      if ((atrib_tresh(i)%id1 .NE. atrib_matched(j)%id1) .AND. (atrib_tresh(i)%id2 .NE. atrib_matched(j)%id2)) then		
	        exists = 0
	      else
		exists = 1	
	      endif
	      j = j - 1
	    enddo
	    if (exists .EQ. 0) then	      
	      cont = cont + 1
	      atrib_matched(cont) = atrib_tresh(i)	    	       
	    endif
	  enddo
	  deallocate(atrib_tresh)
	Endif	              
        call attrib_functions(nexp,cont,atrib_matched)
        deallocate(atrib_treshAux)
	return
      End Subroutine
    !**************************************************************************************************************************************


    !**************************************************************************************************************************************
      Subroutine attrib_functions(nexp,nobj,atrib_matched)
	Implicit None
        integer, intent(in) :: nexp,nobj ! experiment number
	type(atrib_pair), pointer, intent(in)	:: atrib_matched(:)
	integer            :: i, j
	character(len=512) :: filename, fname, fmt
	integer            :: nymd, nhms
 	integer            :: fymd, fhms
   logical            :: Opened

	!integer(I4B) 	:: a
     	integer	:: a,b 	
     	real	:: c(8),d(8),e

        nymd = scamtec%atime/100
        nhms = MOD(scamtec%atime,100) * 10000
        fymd = scamtec%ftime/100
        fhms = MOD(scamtec%ftime,100) * 10000	
        
          fname = 'Attrib_functions'	  

	  inquire(unit=71, opened=Opened)
	  if(.not.Opened) then 
	    filename = trim(fname)//'_'//trim(FNameOut2)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))

	    open(unit   = 71,	&
	         File   = trim(scamtec%output_dir)//Trim(filename)//'.txt',   &
                 access = 'sequential',  &
                 Form   = 'formatted', &
                 Status = 'replace'      &
                )

	    write(71,'(A)')'%Bound   cent   area   angle    intA'
	    !write(71,'(A)')'%FcstId      ObsId     Interest    Conf   Total_interest'

            Do i=1, nobj
	    !Do j=1, prec_nobj             
              !a = vector_functions(i,j)%id1
	      !b = vector_functions(i,j)%id2
	      c = atrib_matched(i)%interest
	      !d = vector_functions(i,j)%conf
	      !e = vector_functions(i,j)%t_interest
	      !f = compare(nexp,j)%nobjOam(i)
	      !g = compare(nexp,j)%csi(i)
	      !h = compare(nexp,j)%hits(i)     
	    
	      write(71,*)c(1),c(2),c(3),c(5),c(8)
	      !write(71,*)a,b,c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),d(1),d(2),d(3),d(4),d(5),d(6),d(7),d(8),e

	    !Enddo
	    Enddo
  	    close(71)	  
          endif		

      End Subroutine attrib_functions
    !**************************************************************************************************************************************

    
     !**************************************************************************************************************************************
      Subroutine write_MMI(nexp,prec_nobj,exp_nobj,MMI,MMIF,MMIO)
	Implicit None
        integer, intent(in) :: nexp,prec_nobj,exp_nobj ! experiment number
	real, intent(in)    :: MMI,MMIF,MMIO
	integer            :: i, j
	character(len=512) :: filename, fname, fmt
	integer            :: nymd, nhms
 	integer            :: fymd, fhms
   logical            :: Opened

	!integer(I4B) 	:: a
     	integer	:: a,b 	
     	real	:: c(8),d(8),e

        nymd = scamtec%atime/100
        nhms = MOD(scamtec%atime,100) * 10000
        fymd = scamtec%ftime/100
        fhms = MOD(scamtec%ftime,100) * 10000	
        
          fname = 'MMI'	  

	  inquire(unit=72, opened=Opened)
	  if(.not.Opened) then 
	    filename = trim(fname)//'_'//trim(FNameOut2)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))

	    open(unit   = 72,	&
	         File   = trim(scamtec%output_dir)//Trim(filename)//'.txt',   &
                 access = 'sequential',  &
                 Form   = 'formatted', &
                 Status = 'replace'      &
                )

	    write(72,'(A)')'%MMI     MMIF     MMIO' 
            write(72,*)MMI,MMIF,MMIO
  	    close(72)	  
          endif		

      End Subroutine write_MMI
    !**************************************************************************************************************************************

    




END MODULE m_mode_pairAttrib
