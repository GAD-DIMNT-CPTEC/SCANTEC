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

! MODULE: m_mode_objects.f90
!
! DESCRIPTON:
! In this module, the main subroutines of the MODE, to identify rain areas, are defined.
! The circular filter (dimension 3x3) is calculated and it is used in the convolution 
! process. In the tresholding algorithm, a mask field based on a user-defined threshold 
! is created, and the raw data are restored to create the object field. 

! A fill method is used to identify objects, and calculate, at the same time, the attributes
! of each object detected. Confidence values are defined for each attribute and weights
! are assigned to calculate the total interest value, for which objects (nearest) are merged.  

MODULE m_mode_objects
   USE scamtec_module
   USE SCAM_dataMOD, only : scamdata  		! SCANTEC data matrix
   USE SCAM_Utils
   USE time_module, only: jul2cal, cal2jul 	! Time operations
   USE m_string                      		! string manipulations
   USE m_die  
   USE m_mode_singleAttrib	! module where single objects attributes are calculated
   USE m_mode_pairAttrib	! module where pair objects attributes are calculated

   IMPLICIT NONE
   
   character(len=512) :: FNameOut1 = '%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2'

   public :: circular_filter   
   public :: convolution   
   public :: tresholding
   public :: valid
   public :: singleObj_Ident_Attrib

   contains    


    !**************************************************************************************************************************************
      Subroutine circular_filter(filter_dimension, filter) 
	 ! Subroutine where the circular filter used in the convolution process is calculated.
         ! Circular filter has dimensions 3x3.
	 ! Convolution radius is defined by the user

         Implicit None
	 ! INPUT PARAMETERS:
	 integer, intent(in) :: filter_dimension    ! radio used in the convolution process

	 ! OUTPUT PARAMETER:
	 real, allocatable, intent(out) :: filter(:,:)     ! Matrix to save circular filter's values

	 character(len=*),parameter :: myname='::circular_filter'
         integer           :: i, j
	 real		   :: x, y, radio
	 real, parameter   :: pi=3.141592654

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname
#endif	 
         
         allocate(filter(filter_dimension,filter_dimension))
	 radio = filter_dimension/2
	 
	 ! Loop para calcular cada ponto (i,j) do filtro circular
	 do j=1, filter_dimension
	    y=j-(filter_dimension+1)/2	    
	    do i=1, filter_dimension	       
	       x=i-(filter_dimension+1)/2
	       if ((x*x+y*y) .LE. (radio*radio)) then
		  filter(i,j) = 1/(pi*radio)		  
	       else
		  filter(i,j) = 0
	       endif
	    enddo	 
	 enddo
	 return
      End Subroutine
    !**************************************************************************************************************************************



    !**************************************************************************************************************************************
      Subroutine convolution(convField, originalField, filter, ysize, xsize, fsize)
      !Subroutine where the convolution process is calculated        

	 Implicit None
	 ! INPUT PARAMETERS:
	 real, allocatable, intent(in) :: originalField(:,:) , filter(:,:)
	 integer, intent(in)           :: xsize, ysize, fsize    ! Convolution matrix dimensions

	 ! OUTPUT PARAMETER:
	 real, allocatable, intent(inout) :: convField(:,:)  ! Field resulting of convolution process - Convolution matrix

	 character(len=*),parameter :: myname='::convolution'
	 integer	:: nrow, ncol, frow, fcol, ccol, crow
	 real		:: radio, radio1

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname
#endif		 

	 radio = (fsize+1)/2	
	 radio1 = fsize/2	 	

	 convField = originalField	 

	 ! Loop para calcular cada ponto (i,j) do campo transformado pelo algoritmo de convolução: 	 
	 do ccol=int(radio), int(xsize-radio1)
	   do crow=int(radio), int(ysize-radio1) 
	       convField(crow,ccol) = 0      
	       fcol = 1
	       do ncol=int(ccol-radio1), int(ccol+radio1)
	          frow = 1
	          do nrow=int(crow-radio1), int(crow+radio1)
		     convField(crow,ccol)= convField(crow,ccol) + filter(frow,fcol)*originalField(nrow,ncol)		                
	 	     frow = frow + 1
		  enddo
	          fcol = fcol + 1
	       enddo		
	    enddo
	 enddo	
	 return
      End Subroutine
    !**************************************************************************************************************************************


    !**************************************************************************************************************************************
     ! The subroutine creates a mask field based on a user-defined threshold,  and the raw data are restored to create the object field 
      Subroutine tresholding(originalField, convField, binaryField, restoreField, ysize, xsize, treshold)       
	Implicit None
	! INPUT PARAMETERS: 
	real, allocatable, intent(in) :: originalField(:,:), convField(:,:) 
	integer, intent(in)           :: xsize, ysize    ! Convolution matrix dimensions
	real, intent(in)	      :: treshold        ! Precipitation threshold defined by the user

	! OUTPUT PARAMETER:
	integer, allocatable, intent(inout) :: binaryField(:,:)  ! Binary field -mask- resulting of tresholding process 
	real, allocatable, intent(inout)    :: restoreField(:,:) ! Matrix to save original field values where the mask is 1 

        character(len=*),parameter :: myname='::tresholding'
	integer   :: i,j

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname
#endif	 	

	do j=1, xsize
	   do i=1, ysize	   
	      if (convField(i,j) .GE. treshold) then
		 binaryField(i,j)=1
	         restoreField(i,j)=originalField(i,j)
	      else
	         binaryField(i,j)=0
	         restoreField(i,j)=0.0
	      endif
	      !restoreField(i,j)= binaryField(i,j)*originalField(i,j)
	   enddo
	enddo	
	return
      End Subroutine
    !**************************************************************************************************************************************



    !**************************************************************************************************************************************
      ! The subroutine verify valid points in the object field
      Subroutine valid(restoreField, ysize, xsize, i, j, treshold, is_valid)
	
	implicit none
	! input parameters:
	real, allocatable, intent(in)   	:: restoreField(:,:)  ! Object Field resulting from tresholding subroutine
	integer, intent(in)            		:: i, j, xsize, ysize    ! Object matrix dimensions
	real, intent(in)	     		:: treshold

        ! output parameters:
	Logical, intent(out)		:: is_valid
   
	is_valid=.false.
	if ( (i .GT. 0) .And. (i .LE. ysize) .And. (j .GT. 0) .And. (j .LE. xsize) ) then 
	   if (restoreField(i,j) .NE. 0.0) then 
	      is_valid = .true.
	   endif
	endif

	return

      end Subroutine valid
    !**************************************************************************************************************************************


    !**************************************************************************************************************************************
     ! The subroutine identifies objects or rain areas and attributes of each object
      Subroutine singleObj_Ident_Attrib(row, col, restoreField, ysize, xsize, treshold, mask, totalObj, perimeter, area, xcent, ycent, angle, aspect_ratio, area_hull, complexity, pts_per, total_linked)

	   Implicit none
	   ! INPUT  PARAMETERS:
	   integer, intent(in)           	    	:: row, col		! Loop variables
	   real, allocatable, intent(in)    		:: restoreField(:,:)	! Object Field resulting from tresholding subroutine
	   integer, intent(in)           	    	:: xsize, ysize		! Object matrix dimensions 	
	   integer, intent(in)	      	    		:: totalObj		! Rain treshold 
	   real, intent(in)				:: treshold
		
	   ! OUTPUT PARAMETERS:
	   integer, allocatable, intent(inout)          :: mask(:,:) !, maskObj(:,:)	! Mask to count objects
	   integer, intent(out)				:: perimeter, area, xcent, ycent
	   real, intent(out)				:: angle, aspect_ratio, complexity, area_hull	
	   !type(mark), pointer, intent(out)    		:: per_linked, total_linked  
	   type(mark), pointer, intent(out)    		:: total_linked  
	   type(point), pointer, intent(out)		:: pts_per(:) 
           character(len=*),parameter 			:: myname='::singleObj_Ident_Attrib'

	   integer				    	:: yini, xini, n, ys, yi, xi, xd
      logical                 :: is_valid
	   logical				    	:: leftPoint, rightPoint, upperPoint, lowerPoint
	   integer				    	:: nsx, nsy  
	   integer					:: i, nhull	   
	   integer				    	:: xleft, xright, x, temp, hull_area, area_hull_int
           
           type(mark), pointer 		    		:: m, maux, maux1, maux2, points, total_points, points_aux, total_points_aux 
	   type(mark), pointer		    		:: hull
	   type(point), pointer				:: convex(:), convex_aux(:)	!pts_per(:), 

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname
#endif   

	   yini=row
	   xini=col

	   nullify(m)
	   nullify(points)
	   nullify(total_points)

	   area=0
	   nsx=0
	   nsy=0
	   perimeter=0
	   area_hull_int=0		   
      	   
	   Do	 
	     call valid(restoreField, ysize, xsize, yini, xini, treshold, is_valid)		
	     If ( is_valid .and. (mask(yini,xini) .EQ. 0) ) then		   
	       ys=yini-1
	       yi=yini+1   	
	       xi=xini-1
	       xd=xini+1		
	                    
               call valid(restoreField, ysize, xsize, ys, xi, treshold, is_valid)			    
	       if ( is_valid .and. (mask(ys,xi) .EQ. 0) ) then			
	         if (.not. associated(maux)) then
		   allocate(maux)
		   maux%x = xi    
		   maux%y = ys 
                 else
		   allocate(m)				   
		   m%x = xi    
		   m%y = ys
                   allocate(m%next)
		   m%next => maux 
		   maux => m				      
		 endif       
               endif			  

               call valid(restoreField, ysize, xsize, ys, xini, treshold, is_valid)			    
	       if ( is_valid .and. (mask(ys,xini) .EQ. 0) ) then			
	         if (.not. associated(maux)) then
		   allocate(maux)
		   maux%x = xini    
                   maux%y = ys 
	         else
                   allocate(m)				   
		   m%x = xini    
		   m%y = ys
                   allocate(m%next)
		   m%next => maux 
		   maux => m				      
		 endif    
	       endif
		
               call valid(restoreField, ysize, xsize, ys, xd, treshold, is_valid)			    
	       if (is_valid .and. (mask(ys,xd) .EQ. 0) ) then	          
                 if (.not. associated(maux)) then
                   allocate(maux)
		   maux%x = xd
		   maux%y = ys
                 else				       
		   allocate(m)				    
		   m%x = xd
		   m%y = ys
                   allocate(m%next)
		   m%next => maux 
		   maux => m				       				
		 endif		
               endif
		
               call valid(restoreField, ysize, xsize, yini, xd, treshold, is_valid)			    
               if ( is_valid .and. (mask(yini,xd) .EQ. 0) ) then		        
		 if (.not. associated(maux)) then
		   allocate(maux)
		   maux%x = xd    
		   maux%y = yini 		              
		 else
		   allocate(m)				   
		   m%x = xd    
		   m%y = yini 
                   allocate(m%next)
		   m%next => maux 
		   maux => m
		 endif       
	       endif
		
               call valid(restoreField, ysize, xsize, yi, xd, treshold, is_valid)		    
	       if ( is_valid .and. (mask(yi,xd) .EQ. 0) ) then			        
		 if (.not. associated(maux)) then
		   allocate(maux)
		   maux%x = xd    
		   maux%y = yi 
                 else
		   allocate(m)				   
		   m%x = xd    
		   m%y = yi 
                   allocate(m%next)
		   m%next => maux 
		   maux => m
		 endif       
               endif
		
	       call valid(restoreField, ysize, xsize, yi, xini, treshold, is_valid)		    
	       if ( is_valid .and. (mask(yi,xini) .EQ. 0) ) then			        
		 if (.not. associated(maux)) then
		   allocate(maux)
		   maux%x = xini    
		   maux%y = yi	              
		 else
		   allocate(m)				   
		   m%x = xini    
		   m%y = yi 
                   allocate(m%next)
		   m%next => maux 
		   maux => m
		 endif       
               endif

               call valid(restoreField, ysize, xsize, yi, xi, treshold, is_valid)		    
	       if ( is_valid .and. (mask(yi,xi) .EQ. 0) ) then			        
                 if (.not. associated(maux)) then
		   allocate(maux)
		   maux%x = xi    
		   maux%y = yi 
                 else
		   allocate(m)				   
		   m%x = xi    
		   m%y = yi 
                   allocate(m%next)
		   m%next => maux 
		   maux => m
		 endif       
	       endif

	       call valid(restoreField, ysize, xsize, yini, xi, treshold, is_valid)		    
	       if ( is_valid .and. (mask(yini, xi) .EQ. 0) ) then			        
                 if (.not. associated(maux)) then
		   allocate(maux)
		   maux%x = xi    
		   maux%y = yini 
                 else
		   allocate(m)				   
		   m%x = xi    
		   m%y = yini 
                   allocate(m%next)
		   m%next => maux 
		   maux => m
		 endif       
               endif

               mask(yini,xini) = 1	       

               ! Checking if the point belongs to the border to calculate the perimeter of the object
	       call valid(restoreField, ysize, xsize, yini, xi, treshold, is_valid)
	       leftPoint=is_valid
	       
	       call valid(restoreField, ysize, xsize, ys, xini, treshold, is_valid)
	       upperPoint=is_valid	       
			  
	       call valid(restoreField, ysize, xsize, yini, xd, treshold, is_valid)
	       rightPoint=is_valid
			     
	       call valid(restoreField, ysize, xsize, yi, xini, treshold, is_valid)
	       lowerPoint=is_valid
		
          if (.NOT. (leftPoint .and. upperPoint .and. rightPoint .and. lowerPoint) ) then        	
	         perimeter = perimeter + 1
		 ! creating linked list to store the perimeter points	          
		 if (.not. associated(points_aux)) then
		   allocate(points_aux)
	 	   points_aux%x = xini
		   points_aux%y = yini
		 else
		   allocate(points)
		   points%x = xini
		   points%y = yini
		   allocate(points%next)
		   points%next => points_aux
	           points_aux => points
		 endif
	       endif

	       area=area+1
	       nsx=nsx+xini   !adding positions x
	       nsy=nsy+yini  !adding positions y

	       ! creating linked list to store all points of the object	        
	       if (.not. associated(total_points_aux)) then
		 allocate(total_points_aux)
	 	 total_points_aux%x = xini
		 total_points_aux%y = yini
	       else
		 allocate(total_points)
		 total_points%x = xini
		 total_points%y = yini
		 allocate(total_points%next)
		 total_points%next => total_points_aux
	         total_points_aux => total_points
	       endif		    
             Endif		

	     if (associated(maux)) then
	       m => maux
	       xini = maux%x
	       yini = maux%y
	       maux => maux%next		   
	       deallocate(m)		   
             else
	       nullify(m)			      
               exit
	     Endif
	   Enddo

	   !per_linked => points
	   total_linked => total_points

	   xcent = nsx/area	! centroid position x
	   ycent = nsy/area	! centroid position y

	   allocate(pts_per(perimeter))

	   Do while (associated(points))
	      do i=1, perimeter
	         pts_per(i)%x = points%x
	         pts_per(i)%y = points%y	
	         points => points%next
	      enddo
           Enddo   
   
	   ! Object Orientation Angle  (subroutine defined in m_mode_singleAttrib)
	   call object_angle(pts_per, xcent, ycent, perimeter, angle)	   	   
	   
	   ! Object Aspect Ratio  (subroutine defined in m_mode_singleAttrib)
	   call object_Aspect_Ratio(pts_per, xcent, ycent, perimeter, angle, aspect_ratio)
           
	   ! compute the convex hull and complexity
	   allocate(convex_aux(perimeter))
	   nhull = 0
	   if (perimeter .GT. 4) then
	          ! (subroutine defined in m_mode_singleAttrib)
	      call quick_hull(pts_per, perimeter, hull)	      
	      
	      do while (associated(hull))		 
		 convex_aux(nhull+1)%x = hull%x
		 convex_aux(nhull+1)%y = hull%y
		 hull => hull%next
		 nhull= nhull+1			 
	      enddo

	      if (nhull .LT. perimeter) then!		  
	          allocate(convex(nhull))	          
	      else
		  allocate(convex(perimeter))	 	  
	      endif


	      Do i=1, nhull		  
		  convex(i) = convex_aux(i)  	          
	      Enddo

	      !ordenar counterclockwise con quicksort  (subroutine defined in m_mode_singleAttrib)
	      call quick_sort(convex, 1, nhull-1)
		  ! (subroutine defined in m_mode_singleAttrib)
	      call polygon_area(convex, nhull, hull_area)
	      area_hull_int = hull_area
	   else
	      area_hull_int = area
	   endif

	   area_hull = area_hull_int

	   complexity = area / area_hull
           return	
      End Subroutine
    !**************************************************************************************************************************************

    

    !**************************************************************************************************************************************
     ! Subroutine defined in m_mode_objects where convolution, tresholding, identification of objects, attributes calculation and merging algorithms are made
      SUBROUTINE mode_ObjectIdentf(nexp, idField, rowSize, colSize, Field, convField, restoreField, weight, total_interest_tresh, grid_res, maskField, maskObj, nObjbefore, totalObj, objects)
         Implicit None
         integer, intent(in) 			:: nexp, idField, rowSize, colSize ! experiment number
         real, intent(in)    			:: weight(8), total_interest_tresh, grid_res
         real, allocatable, intent(in) 		:: Field(:,:)

	 real, allocatable, intent(out)		:: convField(:,:)    ! Field resulting of convolution process
         integer, allocatable, intent(out)   	:: maskField(:,:), maskObj(:,:) ! Binary field -mask- resulting of tresholding process  & Mask to count objects -> resulting of Object Identification Algorithm
	 real, allocatable, intent(out)		:: restoreField(:,:) ! Matrix to save original field values where the mask is 1
         integer, intent(out)			:: totalObj, nObjbefore
	 type(attrs), pointer, intent(out)	:: objects(:)

	 character(len=*),parameter 		:: myname='::mode_ObjectIdentf'

	 real, allocatable    		:: cfilter(:,:)      ! Matrix to save circular filter's values	 
	 integer, allocatable 		:: mask(:,:)    ! Mask to count objects -> resulting of Object Identification Algorithm	 
	 real    			:: treshold	     ! Precipitation threshold defined by the user
         ! Radio and threshold values should be defined in scamtec.conf
	 integer 			:: i, j, radio, filter_dimension, cont	  	 
    logical         :: is_valid
	 ! objects attributes 
	 integer			:: perimeter, area, xcent, ycent
	 real				:: angle, aspect_ratio, complexity, area_hull 
	 type(mark), pointer		:: per_linked, total_linked 
	 type(point), pointer		:: pts_per(:) 
	 real				:: area_tresh  ! Area threshold defined according to the area of interest to the user (scamtec.conf)
	 integer			:: x, y, d, flag, id, points, ii, jj	 

	 type(attrs), pointer		:: objects_temp(:)
	 type(attrs_linked), pointer	:: objects_linked => NULL()
	 type(attrs_linked), pointer	:: attrs_aux => NULL()
	 type(attrs_linked), pointer	:: dir_aux => NULL()
	 type(mark), pointer		:: dir
	 type(atrib_pair), pointer	:: pairObj(:,:), atrib_tresh(:), atrib_treshAux(:), atrib_tresh1	 

	 real				:: min_distance ! Parametro de saida da subrotina min_set_distance
	 real				:: dif_centroid_aux ! usado para calcular la diferencia de centroife entre dos objetos
	 real				:: first_atrib, second_atrib, atrib_ratio ! usados para calcular area_ratio en ratio_function	
    real				:: interest(8), conf(8), conf_i, conf_j 
	 real				::  total   ! Parametro de saida da subrotina total_interest 

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname    
#endif
	 	 
	 ! os valores do radio e o limiar e os pesos devem ser definidos no scamtec.conf
	 filter_dimension=3	 
         treshold=5.0
	 area_tresh=2	 	 	 
	 
	 allocate( convField(rowSize,colSize), maskField(rowSize,colSize), restoreField(rowSize,colSize) )	 	 
	 allocate( mask(rowSize,colSize), maskObj(rowSize,colSize) )	 	 
 
	 mask = 0         
	 
	 ! Calculate Circular filter	 
	 call circular_filter(filter_dimension, cfilter)
	 
	 ! Convolution process         
	 call convolution(convField, Field, cfilter, rowSize, colSize, filter_dimension)	 
         
	 ! Tresholding process
         call tresholding(Field, convField, maskField, restoreField, rowSize, colSize, treshold)

	 ! Loop to identify objects (rain areas) and attributes of each object         
         totalObj=0
	 DO j=1, colSize
	    DO i=1, rowSize            
               call valid(restoreField, rowSize, colSize, i, j, treshold, is_valid)		
	       if (is_valid .and. (mask(i,j) .EQ. 0) ) then		  
                  call singleObj_Ident_Attrib(i, j, restoreField, rowSize, colSize, treshold, mask, totalObj, perimeter, area, xcent, ycent, angle, aspect_ratio, area_hull, complexity, pts_per, total_linked)		  
	
		  if (area .GT. area_tresh) then ! Creating temporal list to save objects attributes
		     allocate(attrs_aux)
		     allocate(attrs_aux%next)
		     attrs_aux%next => objects_linked
		     objects_linked => attrs_aux

		     objects_linked%xcent = xcent
		     objects_linked%ycent = ycent
		     objects_linked%area = area
		     objects_linked%perimeter = perimeter
		     objects_linked%angle = angle
		     objects_linked%aspect_ratio = aspect_ratio
		     objects_linked%complexity = complexity
		     objects_linked%area_hull = area_hull

		     allocate(objects_linked%pts_per(perimeter))		     
		     !do while (associated(per_linked))
		      !do ii=1, perimeter
			!objects_linked%pts_per(ii)%x = per_linked%x 
			!objects_linked%pts_per(ii)%y = per_linked%y
			!per_linked => per_linked%next
		      !enddo
		     !enddo 

		     do ii=1, perimeter
			objects_linked%pts_per(ii)%x = pts_per(ii)%x 
			objects_linked%pts_per(ii)%y = pts_per(ii)%y
			!per_linked => per_linked%next
		     enddo

		     allocate(objects_linked%total_pts(area))		     	              
		     do while (associated(total_linked))
		      do jj=1, area
			objects_linked%total_pts(jj)%x = total_linked%x 
			objects_linked%total_pts(jj)%y = total_linked%y
			total_linked => total_linked%next
		      enddo
		     enddo

		     totalObj = totalObj + 1		        		     
		  endif          	       
	       endif	       
            ENDDO
         ENDDO

         nObjbefore = totalObj
	 Print*
	 Print*, ' Detected Objects Total before Merging = ', totalObj
	 Print*	

	 ! Save the attributes into an array
	 allocate(objects(totalObj))
	 Do while (associated(objects_linked))
	  do i=1, totalObj
	    objects(i)%xcent = objects_linked%xcent
	    objects(i)%ycent = objects_linked%ycent
            objects(i)%area = objects_linked%area
            objects(i)%perimeter = objects_linked%perimeter
            objects(i)%angle = objects_linked%angle
            objects(i)%aspect_ratio = objects_linked%aspect_ratio
            objects(i)%complexity = objects_linked%complexity
            objects(i)%area_hull = objects_linked%area_hull

	    allocate(objects(i)%pts_per(objects(i)%perimeter))
	    do j=1, objects(i)%perimeter
	       objects(i)%pts_per(j)%x = objects_linked%pts_per(j)%x
	       objects(i)%pts_per(j)%y = objects_linked%pts_per(j)%y
	    enddo

	    allocate(objects(i)%total_pts(objects(i)%area))
	    do j=1, objects(i)%area
	       objects(i)%total_pts(j)%x = objects_linked%total_pts(j)%x
	       objects(i)%total_pts(j)%y = objects_linked%total_pts(j)%y
	    enddo

	    objects_linked => objects_linked%next
	  enddo	    
	 Enddo	 

	 Do i=1, totalObj
	    objects(i)%id = i 
	 Enddo
	 
 	 maskObj= 0
	 Do i=1, totalObj	  
	  id = objects(i)%id
	  points = objects(i)%area	  
	  Do j=1, points	    
	    x = objects(i)%total_pts(j)%x
	    y = objects(i)%total_pts(j)%y	    
	    maskObj(y,x) = id	    
	  Enddo
	 Enddo 

         !call write_beforeMerg(nexp, idField, maskObj)	 

        ! *******************  MERGING PROCESS  ********************************************************
	 d = 0	 ! d -> Objects pairs total compared in the field 
         Do i=1, totalObj-1
	   d = d + totalObj - i
	 Enddo
	 
	 allocate(atrib_treshAux(d))  ! Estructura para guardar os pares de objetos comparados que estão acima do limiar do merging
	 allocate(pairObj(totalObj,totalObj))! Estructura para guardar todos os pares de objetos comparados no merging

#ifdef DEBUG
    WRITE(6,'(     2A)')'  Calculating attributes of object pairs within the field to merge them'  
#endif

	 flag = 1
	Do while (flag .EQ. 1) ! Loop para calcular os atributos de pares de objetos
	 cont = 0
	 do i=1, totalObj
	    do j=i+1, totalObj
	       pairObj(i,j)%id1 = objects(i)%id
	       pairObj(i,j)%id2 = objects(j)%id
                  
	       call min_set_distance(objects(i)%pts_per, objects(j)%pts_per, pairObj(i,j)%near_x1, pairObj(i,j)%near_y1, pairObj(i,j)%near_x2, pairObj(i,j)%near_y2, objects(i)%perimeter, objects(j)%perimeter, min_distance)
	       ! min_boundary_dist
	       pairObj(i,j)%BoundDist = min_distance                

	       ! dif_centroid
	       dif_centroid_aux = (objects(i)%xcent - objects(j)%xcent)**2 + (objects(i)%ycent - objects(j)%ycent)**2
	       pairObj(i,j)%difCent = SQRT(dif_centroid_aux)	        

	       ! area_ratio
	       first_atrib = objects(i)%area
	       second_atrib = objects(j)%area	           
	       call ratio_function(first_atrib, second_atrib, atrib_ratio) ! (subroutine defined in m_mode_pairAttrib)
	       pairObj(i,j)%areaR = atrib_ratio		

	       ! Objects size
	       pairObj(i,j)%objSize = SQRT(first_atrib) + SQRT(second_atrib)	       

	       ! perimeter_ratio
	       first_atrib = objects(i)%perimeter
	       second_atrib = objects(j)%perimeter
	       call ratio_function(first_atrib, second_atrib, atrib_ratio) ! (subroutine defined in m_mode_pairAttrib)
	       pairObj(i,j)%perR = atrib_ratio		

	       ! angle diference
	       pairObj(i,j)%difAngle = objects(i)%angle - objects(j)%angle
	       if (pairObj(i,j)%difAngle .LT. 0.0) then
	          pairObj(i,j)%difAngle = (-1)*pairObj(i,j)%difAngle
	       endif	        

	       ! aspect_ratio 
	       first_atrib = objects(i)%aspect_ratio
	       second_atrib = objects(j)%aspect_ratio
	       call ratio_function(first_atrib, second_atrib, atrib_ratio)  ! (subroutine defined in m_mode_pairAttrib)
	       pairObj(i,j)%aspectR = atrib_ratio
		
	       ! conf_angle
	       !if (objects(i)%aspect_ratio .LT. objects(j)%aspect_ratio) then
               conf_i = ((objects(i)%aspect_ratio-1)**2 / (((objects(i)%aspect_ratio)**2)+1))**0.3
	       !else 
	       conf_j = ((objects(j)%aspect_ratio-1)**2 / (((objects(j)%aspect_ratio)**2)+1))**0.3
	       !endif
	       pairObj(i,j)%confAngle = SQRT(conf_i*conf_j)	

	       ! complexity ratio
	       first_atrib = objects(i)%complexity
	       second_atrib = objects(j)%complexity
	       call ratio_function(first_atrib, second_atrib, atrib_ratio)  ! (subroutine defined in m_mode_pairAttrib)
	       pairObj(i,j)%complexR = atrib_ratio		

	       !intersection area 
	       !pairObj(i,j)%intAreaR = 0.0	       

	       ! TOTAL INTERES (subroutine defined in m_mode_pairAttrib)
 call total_interest(pairObj(i,j)%BoundDist,pairObj(i,j)%difCent,pairObj(i,j)%areaR,pairObj(i,j)%perR,pairObj(i,j)%difAngle,pairObj(i,j)%aspectR,pairObj(i,j)%complexR,0.0,pairObj(i,j)%confAngle,pairObj(i,j)%objSize,weight,grid_res,interest,conf,total)
		  
               pairObj(i,j)%t_interest = total	       

	       if (pairObj(i,j)%t_interest .GE. total_interest_tresh) then
		  cont = cont + 1	          
		  atrib_treshAux(cont) = pairObj(i,j)		     
	       endif		  
	    enddo
	 enddo
	 
         allocate(atrib_tresh(cont))   ! Estrutura onde guardo os pares de objetos agrupados no merging
	 Do i=1, cont
	   atrib_tresh(i) = atrib_treshAux(i)	   
         Enddo	 
	 
	 if (cont .GT. 1) then            
	    call quick_sort_original(atrib_tresh, 1, cont)  ! (subroutine defined in m_mode_pairAttrib)		    
	    atrib_tresh1 => atrib_tresh(1)	    
	    call merging(atrib_tresh1, objects, totalObj, maskObj) ! (subroutine defined in m_mode_pairAttrib)	    	    
	    flag = 1
	 else
	    flag = 0
	 endif 
	 
         deallocate(atrib_tresh)
	Enddo
	!stop
	call write_mask(nexp, idField, maskObj)
	
	deallocate(pairObj)
        deallocate (cfilter)	

        return
      END SUBROUTINE mode_ObjectIdentf
    !**************************************************************************************************************************************  


     !**************************************************************************************************************************************   
      Subroutine write_beforeMerg(nexp, idField, mask)
	Implicit None
        integer, intent(in) :: nexp, idField ! experiment number	        
	integer, allocatable   	:: mask(:,:)

	integer            :: i,j, ier, f
	character(len=512) :: filename, fname, fmt
	integer            :: nymd, nhms
 	integer            :: fymd, fhms	
   logical            :: Opened

	f = scamtec%ftime_idx 
        nymd = scamtec%atime/100
        nhms = MOD(scamtec%atime,100) * 10000
        fymd = scamtec%ftime/100
        fhms = MOD(scamtec%ftime,100) * 10000

	If (idField .EQ. 0) then
	  fname = 'PrecipField'
	  inquire(unit=119, opened=Opened)
	  if(.not.Opened) then 
	    filename = trim(fname)//'_'//trim(FNameOut1)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))

	    open(unit   = 119,	&
	         File   = trim(scamtec%output_dir)//'/BeforeMergingMask'//Trim(filename)//'.bin',   &
	         status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		) 
               !write(*,*)'MINIMO E MAXIMO >> ',minval(mask),maxval(mask)
	       write(119) real(mask)
	    Close(119)  

	    open(120, file=trim(scamtec%output_dir)//'/BeforeMergingMask'//Trim(filename)//'.ctl', status='unknown')
	    write(120,'(A,A)')'dset ^','BeforeMergingMask'//Trim(filename)//'.bin'      	    
      	    write(120,'(A)')'options sequential'      	        
     	    write(120,'(A)')'undef -999.9'      
      	    write(120,'(A)')      
      	    write(120,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(120,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(120,'(A)')'zdef    1 linear 0 1'             
            write(120,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(120,'(A)')
            write(120,'(A)')'vars 1'
	    write(120,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(120,'(A)')'endvars'         
            close(120)	    
	  endif

	elseif (idField .EQ. 1) then
	  fname = 'ExpField'
	  inquire(unit=122, opened=Opened)
	  if(.not.Opened) then 
	    filename = trim(fname)//'_'//trim(FNameOut1)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))

	    open(unit   = 122,	&
	         File   = trim(scamtec%output_dir)//'/BeforeMergingMask'//Trim(filename)//'.bin',   &
	         status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		) 
	        write(122)real(mask)	
	    Close(122)  

	    open(123, file=trim(scamtec%output_dir)//'/BeforeMergingMask'//Trim(filename)//'.ctl', status='unknown')
	    write(123,'(A,A)')'dset ^','BeforeMergingMask'//Trim(filename)//'.bin'      	    
      	    write(123,'(A)')'options sequential'      	        
     	    write(123,'(A)')'undef -999.9'      
      	    write(123,'(A)')      
      	    write(123,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(123,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(123,'(A)')'zdef    1 linear 0 1'             
            write(123,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(123,'(A)')
            write(123,'(A)')'vars 1'
	    write(123,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(123,'(A)')'endvars'         
            close(123)	    
	  endif
	Endif

      End Subroutine
    !**************************************************************************************************************************************    


    !**************************************************************************************************************************************   
      Subroutine write_mask(nexp, idField, mask)
	Implicit None
        integer, intent(in) :: nexp, idField ! experiment number	        
	integer, allocatable   	:: mask(:,:)

	integer            :: i,j, ier, f
	character(len=512) :: filename, fname, fmt
	integer            :: nymd, nhms
 	integer            :: fymd, fhms	
   logical            :: Opened

	f = scamtec%ftime_idx 
        nymd = scamtec%atime/100
        nhms = MOD(scamtec%atime,100) * 10000
        fymd = scamtec%ftime/100
        fhms = MOD(scamtec%ftime,100) * 10000

	If (idField .EQ. 0) then
	  fname = 'PrecipField'
	  inquire(unit=119, opened=Opened)
	  if(.not.Opened) then 
	    filename = trim(fname)//'_'//trim(FNameOut1)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))

	    open(unit   = 119,	&
	         File   = trim(scamtec%output_dir)//'/MergingMask'//Trim(filename)//'.bin',   &
	         status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		) 
               !write(*,*)'MINIMO E MAXIMO >> ',minval(mask),maxval(mask)
	       write(119) real(mask)
	    Close(119)  

	    open(120, file=trim(scamtec%output_dir)//'/MergingMask'//Trim(filename)//'.ctl', status='unknown')
	    write(120,'(A,A)')'dset ^','MergingMask'//Trim(filename)//'.bin'      	    
      	    write(120,'(A)')'options sequential'      	        
     	    write(120,'(A)')'undef -999.9'      
      	    write(120,'(A)')      
      	    write(120,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(120,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(120,'(A)')'zdef    1 linear 0 1'             
            write(120,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(120,'(A)')
            write(120,'(A)')'vars 1'
	    write(120,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(120,'(A)')'endvars'         
            close(120)	    
	  endif

	elseif (idField .EQ. 1) then
	  fname = 'ExpField'
	  inquire(unit=122, opened=Opened)
	  if(.not.Opened) then 
	    filename = trim(fname)//'_'//trim(FNameOut1)
            call str_template(filename, nymd, nhms, fymd, fhms, label=num2str(nexp,'(I2.2)'))

	    open(unit   = 122,	&
	         File   = trim(scamtec%output_dir)//'/MergingMask'//Trim(filename)//'.bin',   &
	         status='unknown', &
                 form   = 'unformatted', &
                 access = 'sequential'  &                 
		) 
	        write(122)real(mask)	
	    Close(122)  

	    open(123, file=trim(scamtec%output_dir)//'/MergingMask'//Trim(filename)//'.ctl', status='unknown')
	    write(123,'(A,A)')'dset ^','MergingMask'//Trim(filename)//'.bin'      	    
      	    write(123,'(A)')'options sequential'      	        
     	    write(123,'(A)')'undef -999.9'      
      	    write(123,'(A)')      
      	    write(123,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      	    write(123,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      	     
      	    write(123,'(A)')'zdef    1 linear 0 1'             
            write(123,'(A,I3,A,1X,I2,A)')'tdef  ',1,' linear 00Z05AUG2014',scamtec%atime_step,'HR'
            write(123,'(A)')
            write(123,'(A)')'vars 1'
	    write(123,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
	    write(123,'(A)')'endvars'         
            close(123)	    
	  endif
	Endif

      End Subroutine
    !**************************************************************************************************************************************   




END MODULE m_mode_objects
