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

   USE m_mode_singleAttrib	! module where single objects attributes are calculated
   USE m_mode_pairAttrib	! module where pair objects attributes are calculated

   public :: circular_filter
   public :: convolution
   public :: tresholding
   public :: valid
   public :: singleObj_Ident_Attrib

   contains

    !**************************************************************************************************************************************
      Subroutine circular_filter(filter, radio) 
	 ! Subroutine where the circular filter used in the convolution process is calculated.
         ! Circular filter has dimensions 3x3.
	 ! Convolution radius is defined by the user

         Implicit None
	 ! INPUT PARAMETERS:
	 integer, intent(in) :: radio    ! radio used in the convolution process

	 ! OUTPUT PARAMETER:
	 real, allocatable, intent(out) :: filter(:,:)     ! Matrix to save circular filter's values

         integer           :: i, j, x, y, filter_dimension
	 real, parameter   :: pi=3.141592654

         filter_dimension=3
         allocate(filter(filter_dimension,filter_dimension))

	! Loop para calcular cada ponto (i,j) do filtro circular
	 do j=1, filter_dimension
	    y=j-filter_dimension/2
	    do i=1, filter_dimension
	       x=i-filter_dimension/2
	       if (x*x+y*y .LE. radio*radio) then
		  filter(i,j) = 1/(radio*radio*pi)
	       else
		  filter(i,j) = 0
	       endif
	    enddo	 
	 enddo

      End Subroutine
    !**************************************************************************************************************************************


    !**************************************************************************************************************************************
      Subroutine convolution(convField, originalField, filter, ysize, xsize)
      !Subroutine where the convolution process is calculated        

	 Implicit None
	 ! INPUT PARAMETERS:
	 real, allocatable, intent(in) :: originalField(:,:) , filter(:,:)
	 integer, intent(in)           :: xsize, ysize    ! Convolution matrix dimensions

	 ! OUTPUT PARAMETER:
	 real, allocatable, intent(inout) :: convField(:,:)  ! Field resulting of convolution process - Convolution matrix

	 integer	:: nrow, ncol, frow, fcol, ccol, crow, filter_dimension=3

	 !allocate(convField(ysize, xsize))

	! Loop para calcular cada ponto (i,j) do campo transformado pelo algoritmo de convolução: 	 
	do ncol=1, xsize
	   do nrow=1, ysize 	    
	       convField(nrow,ncol)=0
	       do fcol=1, filter_dimension
	          do frow=1, filter_dimension			  			
		     crow=abs(nrow-frow)
		     ccol=abs(ncol-fcol)
		     if ( (nrow .EQ. 2) .and. (frow .EQ. 2) ) then 
		        crow= frow
		     endif
		     if ( (ncol .EQ. 2) .and. (fcol .EQ. 2) ) then 
			ccol= fcol
		     endif
		     if ( (ccol .GT. 0) .and. (ccol .LE. xsize) .and. (crow .GT. 0) .and. (crow .LE. ysize) ) then
			convField(nrow,ncol)= convField(nrow,ncol) + filter(frow,fcol)*originalField(crow,ccol)
		     endif
		  enddo
	       enddo		
	    enddo
	 enddo

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

	integer    :: i,j 	

	do j=1, xsize
	   do i=1, ysize	   
	      if (convField(i,j) .GE. treshold) then
		 binaryField(i,j)=1
	      else
	         binaryField(i,j)=0
	      endif
	      restoreField(i,j)= binaryField(i,j)*originalField(i,j)
	   enddo
	enddo

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
	integer, intent(out)		:: is_valid
        
	is_valid=0
	if ( (i .GT. 0) .And. (i .LE. ysize) .And. (j .GT. 0) .And. (j .LE. xsize) ) then 
	   if (restoreField(i,j) .GE. treshold) then 
	      is_valid = 1
	   endif
	endif

	return

      end Subroutine valid
    !**************************************************************************************************************************************


    !**************************************************************************************************************************************
     ! The subroutine identifies objects or rain areas and attributes of each object
      Subroutine singleObj_Ident_Attrib(row, col, restoreField, ysize, xsize, treshold, mask, totalObj, maskObj, perimeter, area, xcent, ycent, angle, aspect_ratio, area_hull, complexity, per_linked, total_linked)

	   Implicit none
	   ! INPUT  PARAMETERS:
	   integer, intent(in)           	    	:: row, col		! Loop variables
	   real, allocatable, intent(in)    		:: restoreField(:,:)	! Object Field resulting from tresholding subroutine
	   integer, intent(in)           	    	:: xsize, ysize		! Object matrix dimensions 	
	   integer, intent(in)	      	    		:: totalObj		! Rain treshold 
	   real, intent(in)				:: treshold
		
	   ! OUTPUT PARAMETERS:
	   integer, allocatable, intent(inout)          :: mask(:,:), maskObj(:,:)	! Mask to count objects
	   integer, intent(out)				:: perimeter, area, xcent, ycent
	   real, intent(out)				:: angle, aspect_ratio, complexity, area_hull	
	   type(mark), pointer, intent(out)    		:: per_linked, total_linked   

	   integer				    	:: is_valid, yini, xini, n, ip, ip2, ys, xs, ps, xds, pds, yi, xi, pi, xdi, pdi
	   integer				    	:: leftPoint, rightPoint, upperPoint, lowerPoint 
	   integer				    	:: nsx, nsy  
	   integer					:: i, nhull	   
	   integer				    	:: xleft, xright, x, temp, hull_area, area_hull_int
           
           type(mark), pointer 		    		:: m, maux, maux1, maux2, points, total_points, points_aux, total_points_aux 
	   type(mark), pointer		    		:: hull
	   type(point), pointer				:: pts_per(:), convex(:), convex_aux(:)	   

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

	   ! print apenas para verificar o algoritmo
	   !print*, '************  Inicio loop exterior  **********'
	   !print*, ' Objetos detectados', totalObj+1
      
	   Do	 
		call valid(restoreField, ysize, xsize, yini, xini, treshold, is_valid)
		If (is_valid) then		   
		   ys=yini-1
	      	   yi=yini+1
	      	   n=-1

		   do while (n < 2)		      
		      xs=0
		      xds=0
	              xi=0
		      xdi=0

	              if (n .LT. 0) then
	                 ip=xini+1
	              else
	                 ip=xini
		      endif
		      ip2 = ip
		      
		      do			
			 ip2=ip2+n
			 
			 call valid(restoreField, ysize, xsize, yini, ip2, treshold, is_valid)
			 ! Creating linked list to store the valid neighboring points still to be verified			 
			 if ( is_valid .and. (mask(yini,ip2) .EQ. 0) ) then
	                    
			    call valid(restoreField, ysize, xsize, ys, ip, treshold, is_valid)			    
			    if ( is_valid .and. (mask(ys,ip) .EQ. 0) ) then			
				pds=1
				if (xds .NE. pds) then			     	   
			     	   if (.not. associated(maux)) then
			              allocate(maux)
				      maux%x = ip    
			     	      maux%y = ys 
!			     	      nullify(maux%next)		              
				   else
				         allocate(m)				   
				         m%x = ip    
			     	         m%y = ys
                                         allocate(m%next)
			     	         m%next => maux 
				         maux => m				      
				   endif       
			  	endif
			    else
				pds=0
			    endif
			    xds=pds

			    call valid(restoreField, ysize, xsize, yi, ip, treshold, is_valid)			    
		       	    if (is_valid .and. (mask(yi,ip) .EQ. 0) ) then	          
			        pdi=1
                                if (xdi .NE. pdi) then			            
			            if (.not. associated(maux)) then
			               allocate(maux)
				       maux%x = ip
			               maux%y = yi
!			               nullify(maux%next)			            			            
				    else				       
				          allocate(m)				    
				          m%x = ip
				          m%y = yi
                                          allocate(m%next)
				          m%next => maux 
				          maux => m				       				
				    endif		
			        endif
			    else
				pdi=0
		            endif
			    xdi=pdi

			    call valid(restoreField, ysize, xsize, ys, ip2, treshold, is_valid)			    
			    if ( is_valid .and. (mask(ys,ip2) .EQ. 0) ) then		        
				ps=1
				if (xs .NE. ps) then			     	   
			     	   if (.not. associated(maux)) then
			              allocate(maux)
				      maux%x = ip2    
			     	      maux%y = ys 
!			     	      nullify(maux%next) 		              
				   else
				         allocate(m)				   
				         m%x = ip2    
			     	         m%y = ys 
                                         allocate(m%next)
			     	         m%next => maux 
				         maux => m
				   endif       
			  	endif
			    else
				ps=0
			    endif
			    xs=ps

			    call valid(restoreField, ysize, xsize, yi, ip2, treshold, is_valid)		    
			    if ( is_valid .and. (mask(yi,ip2) .EQ. 0) ) then			        
				pi=1
				if (xi .NE. pi) then			     	   
			     	   if (.not. associated(maux)) then
			              allocate(maux)
				      maux%x = ip2    
			     	      maux%y = yi 
!			     	      nullify(maux%next) 		              
				   else
				         allocate(m)				   
				         m%x = ip2    
			     	         m%y = yi 
                                         allocate(m%next)
			     	         m%next => maux 
				         maux => m
				   endif       
			  	endif
			    else
				pi=0
			    endif
			    xi=pi

			    mask(yini,ip2) = 1
			    maskObj(yini,ip2) = totalObj + 1

			    ! Checking if the point belongs to the border to calculate the perimeter of the object
			    call valid(restoreField, ysize, xsize, ys, ip2, treshold, is_valid)
		            leftPoint=is_valid
			    !print*, 'leftPoint', leftPoint
		       
		            call valid(restoreField, ysize, xsize, yini, ip2-1, treshold, is_valid)
		            upperPoint=is_valid
			    !print*, 'upperPoint', upperPoint
			  
		            call valid(restoreField, ysize, xsize, yi, ip2, treshold, is_valid)
		            rightPoint=is_valid
			    !print*, 'rightPoint', rightPoint
			     
		            call valid(restoreField, ysize, xsize, yini, ip2+1, treshold, is_valid)
		            lowerPoint=is_valid
			    !print*, 'lowerPoint', lowerPoint
		            if (.NOT. (leftPoint .and. upperPoint .and. rightPoint .and. lowerPoint) ) then
				!print*, '.NOT. (leftPoint .and. upperPoint .and. rightPoint .and. lowerPoint)'
			        perimeter = perimeter + 1
		 		! creating linked list to store the perimeter points
				allocate(points_aux)
                                allocate(points_aux%next)
				points_aux%next => points
				points => points_aux
				points%x = ip2
				points%y = yini
		            endif

		            area=area+1
	                    nsx=nsx+ip2   !adding positions x
	                    nsy=nsy+yini  !adding positions y

			    ! creating linked list to store all points of the object
			    allocate(total_points_aux)
			    allocate(total_points_aux%next)
			    total_points_aux%next => total_points
			    total_points => total_points_aux
			    total_points%x = ip2
			    total_points%y = yini			    
			 else			    
			    exit
			 endif			 
		      enddo
		      n = n + 2
		      
		   enddo
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

	   per_linked => points
	   total_linked => total_points

	   xcent = nsx/area	! centroid position x
	   ycent = nsy/area	! centroid position y	   

	   !print*, 'perimetro', perimeter, 'area', area, 'xcent', xcent, 'ycent', ycent  

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
	   !print*, 'orientation angle', angle	   
	   
	   ! Object Aspect Ratio  (subroutine defined in m_mode_singleAttrib)
	   call object_Aspect_Ratio(pts_per, xcent, ycent, perimeter, angle, aspect_ratio)
	   !print*, 'Aspect Ratio', aspect_ratio
           
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
	      !print*, 'nhull', nhull

	      if (nhull .LT. perimeter) then!		  
	          allocate(convex(nhull))	          
	      else
		  allocate(convex(perimeter))	 	  
	      endif


	      Do i=1, nhull		  
		  convex(i) = convex_aux(i)  	          
	      Enddo
	      
	      !print*, 'convex_aux(nhull)', convex_aux
	      !print*, 'convex(nhull)', convex

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

	   !print*, 'area_hull, complexity', area_hull, complexity

           return	
      End Subroutine
    !**************************************************************************************************************************************

    

    !**************************************************************************************************************************************
     ! Subroutine defined in m_mode_objects where convolution, tresholding, identification of objects, attributes calculation and merging algorithms are made
      SUBROUTINE mode_ObjectIdentf(rowSize, colSize, TESTE, weight, total_interest_tresh, grid_res, mask, maskObj, totalObj, objects)
         Implicit None
         integer, intent(in) 			:: rowSize, colSize ! experiment number
         real, intent(in)    			:: weight(8), total_interest_tresh, grid_res
         real, allocatable, intent(in) 		:: TESTE(:,:)

         integer, allocatable, intent(out)   	:: mask(:,:), maskObj(:,:) ! Mask to count objects -> resulting of Object Identification Algorithm
         integer, intent(out)			:: totalObj
	 type(attrs), pointer, intent(out)	:: objects(:)

	 real, allocatable    		:: cfilter(:,:)      ! Matrix to save circular filter's values
	 real, allocatable    		:: convField(:,:)    ! Field resulting of convolution process
	 integer, allocatable 		:: maskField(:,:)    ! Binary field -mask- resulting of tresholding process 
	 real, allocatable    		:: restoreField(:,:) ! Matrix to save original field values where the mask is 1
	 real    			:: treshold	     ! Precipitation threshold defined by the user

         ! Radio and threshold values should be defined in scamtec.conf
	 integer 			:: i, j, radio, is_valid, cont	 

	  	 
	 ! objects attributes 
	 integer			:: perimeter, area, xcent, ycent
	 real				:: angle, aspect_ratio, complexity, area_hull 
	 type(mark), pointer		:: per_linked, total_linked 

	 real				:: area_tresh  ! Area threshold defined according to the area of interest to the user (scamtec.conf)
	 integer			:: d, flag	 

	 type(attrs), pointer		:: objects_temp(:)
	 type(attrs_linked), pointer	:: objects_linked => NULL()
	 type(attrs_linked), pointer	:: attrs_aux => NULL()
	 type(attrs_linked), pointer	:: dir_aux => NULL()
	 type(mark), pointer		:: dir
	 type(atrib_pair), pointer	:: pairObj(:,:), atrib_tresh(:), atrib_treshAux(:), atrib_tresh1	 

	 real				:: min_distance ! Parametro de saida da subrotina min_set_distance
	 real				:: dif_centroid_aux ! usado para calcular la diferencia de centroife entre dos objetos
	 real				:: first_atrib, second_atrib, atrib_ratio ! usados para calcular area_ratio en ratio_function	
         real				:: interest(8) 
	 real				::  total   ! Parametro de saida da subrotina total_interest 

	 ! EXEMPLOS PARA COMPROVAR OS ALGORITMOS	 
	 ! os valores do radio e o limiar e os pesos devem ser definidos no scamtec.conf
	 radio=1
         treshold=0.5
	 area_tresh=2
	 	 
	 !print*, '     rowSize, colSize', rowSize, colSize
	 allocate( convField(rowSize,colSize), maskField(rowSize,colSize), restoreField(rowSize,colSize) )	 	 
	 allocate( mask(rowSize,colSize), maskObj(rowSize,colSize) )	 	 
 
	 mask = 0
         maskObj = 0
	 
	 ! Calculate Circular filter
	 call circular_filter(cfilter, radio)
	 
	 ! Convolution process
         call convolution(convField, TESTE, cfilter, rowSize, colSize)
         
	 ! Tresholding process
         call tresholding(TESTE, convField, maskField, restoreField, rowSize, colSize, treshold)

	 ! Loop to identify objects (rain areas) and attributes of each object
         !print*
	 !print*,' **** Object Identification ****'
         totalObj=0
	 DO j=1, colSize
	    DO i=1, rowSize            
               call valid(restoreField, rowSize, colSize, i, j, treshold, is_valid)		
	       if (is_valid .and. (mask(i,j) .EQ. 0) ) then		  
                  call singleObj_Ident_Attrib(i, j, restoreField, rowSize, colSize, treshold, mask, totalObj, maskObj, perimeter, area, xcent, ycent, angle, aspect_ratio, area_hull, complexity, per_linked, total_linked)		  
	
		  if (area .GE. area_tresh) then ! Creating temporal list to save objects attributes
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
		     dir => per_linked
	    	     cont = 0 
		     do while (associated(dir))
			objects_linked%pts_per(cont+1)%x = dir%x 
			objects_linked%pts_per(cont+1)%y = dir%y
			dir => dir%next
			cont = cont+1
		     enddo

		     allocate(objects_linked%total_pts(area))
		     dir => total_linked
	    	     cont = 0 
		     do while (associated(dir))
			objects_linked%total_pts(cont+1)%x = dir%x 
			objects_linked%total_pts(cont+1)%y = dir%y
			dir => dir%next
			cont = cont+1
		     enddo

		     totalObj = totalObj + 1		     		     
		  endif          	       
	       endif	       
            ENDDO
         ENDDO

	 ! Save the attributes into an array
	 allocate(objects(totalObj))
	 dir_aux => objects_linked
	 i=0
	 Do while (associated(dir_aux))
	    i = i+1
	    objects(i)%xcent = dir_aux%xcent
	    objects(i)%ycent = dir_aux%ycent
            objects(i)%area = dir_aux%area
            objects(i)%perimeter = dir_aux%perimeter
            objects(i)%angle = dir_aux%angle
            objects(i)%aspect_ratio = dir_aux%aspect_ratio
            objects(i)%complexity = dir_aux%complexity
            objects(i)%area_hull = dir_aux%area_hull

	    allocate(objects(i)%pts_per(objects(i)%perimeter))

	    do j=1, objects(i)%perimeter
	       objects(i)%pts_per(j)%x = dir_aux%pts_per(j)%x
	       objects(i)%pts_per(j)%y = dir_aux%pts_per(j)%y
	    enddo

	    allocate(objects(i)%total_pts(objects(i)%area))

	    do j=1, objects(i)%area
	       objects(i)%total_pts(j)%x = dir_aux%total_pts(j)%x
	       objects(i)%total_pts(j)%y = dir_aux%total_pts(j)%y
	    enddo

	    dir_aux => dir_aux%next	    
	 Enddo
	 
	 Do while (associated(dir_aux))
	    dir_aux => objects_linked
	    objects_linked => objects_linked%next
	    deallocate(dir_aux)
	 Enddo

	 Do i=1, totalObj
	    objects(i)%id = i 
	 Enddo

        ! *******************  MERGING PROCESS  ********************************************************
	 d = 0	 
         Do i=1, totalObj-1
	   d = d + totalObj - i
	 Enddo
	 !print*, 'MERGING PROCESS d', d 
	 allocate(atrib_treshAux(d))
	 allocate(pairObj(totalObj,totalObj))	 

	 flag = 1

	Do while (flag .EQ. 1) ! Loop para calcular los atributos de pares de objetos
	 cont = 0
	 do i=1, totalObj
	    do j=i+1, totalObj
	       pairObj(i,j)%id1 = objects(i)%id
	       pairObj(i,j)%id2 = objects(j)%id
                  !print*
	 	  !print*, 'i,j', i,j
		  !print*, 'pairObj(i,j)%id1', pairObj(i,j)%id1
	  	  !print*, 'pairObj(i,j)%id2', pairObj(i,j)%id2
		   ! (subroutine defined in m_mode_pairAttrib)
	       call min_set_distance(objects(i)%pts_per, objects(j)%pts_per, pairObj(i,j)%near_x1, pairObj(i,j)%near_y1, pairObj(i,j)%near_x2, pairObj(i,j)%near_y2, objects(i)%perimeter, objects(j)%perimeter, min_distance)
	       ! min_boundary_dist
	       pairObj(i,j)%BoundDist = min_distance
                !print*, 'pairObj(i,j)%BoundDist', pairObj(i,j)%BoundDist

	       ! dif_centroid
	       dif_centroid_aux = (objects(i)%xcent - objects(j)%xcent)**2 + (objects(i)%ycent - objects(j)%ycent)**2
	       pairObj(i,j)%difCent = SQRT(dif_centroid_aux)
	        !print*, 'pairObj(i,j)%difCent', pairObj(i,j)%difCent
	       ! area_ratio
	       first_atrib = objects(i)%area
	       second_atrib = objects(j)%area	           
	       call ratio_function(first_atrib, second_atrib, atrib_ratio) ! (subroutine defined in m_mode_pairAttrib)

	       pairObj(i,j)%areaR = atrib_ratio
		!print*, ' pairObj(i,j)%areaR',  pairObj(i,j)%areaR 
	       ! Objects size
	       pairObj(i,j)%objSize = SQRT(first_atrib) + SQRT(second_atrib)
		!print*, 'pairObj(i,j)%objSize', pairObj(i,j)%objSize
	       ! perimeter_ratio
	       first_atrib = objects(i)%perimeter
	       second_atrib = objects(j)%perimeter
	       call ratio_function(first_atrib, second_atrib, atrib_ratio) ! (subroutine defined in m_mode_pairAttrib)

	       pairObj(i,j)%perR = atrib_ratio
		!print*, 'pairObj(i,j)%perR', pairObj(i,j)%perR
	       ! angle diference
	       pairObj(i,j)%difAngle = objects(i)%angle - objects(j)%angle
	       if (pairObj(i,j)%difAngle .LT. 0.0) then
	          pairObj(i,j)%difAngle = (-1)*pairObj(i,j)%difAngle
	       endif
	        !print*,'pairObj(i,j)%difAngle', pairObj(i,j)%difAngle
	       ! aspect_ratio 
	       first_atrib = objects(i)%aspect_ratio
	       second_atrib = objects(j)%aspect_ratio
	       call ratio_function(first_atrib, second_atrib, atrib_ratio)  ! (subroutine defined in m_mode_pairAttrib)

	       pairObj(i,j)%aspectR = atrib_ratio
		!print*, 'pairObj(i,j)%aspectR', pairObj(i,j)%aspectR
	       ! conf_angle
	       if (objects(i)%aspect_ratio .LT. objects(j)%aspect_ratio) then
                  pairObj(i,j)%confAngle = ((objects(i)%aspect_ratio)**2 / ((objects(i)%aspect_ratio)**2+1))**0.3
	       else 
	          pairObj(i,j)%confAngle = ((objects(j)%aspect_ratio)**2 / ((objects(j)%aspect_ratio)**2+1))**0.3
	       endif
		!print*, 'pairObj(i,j)%confAngle', pairObj(i,j)%confAngle
	       ! complexity ratio
	       first_atrib = objects(i)%complexity
	       second_atrib = objects(j)%complexity
	       call ratio_function(first_atrib, second_atrib, atrib_ratio)  ! (subroutine defined in m_mode_pairAttrib)

	       pairObj(i,j)%complexR = atrib_ratio
		!print*, 'pairObj(i,j)%complexR', pairObj(i,j)%complexR
	       ! TOTAL INTERES (subroutine defined in m_mode_pairAttrib)
 call total_interest(pairObj(i,j)%BoundDist,pairObj(i,j)%difCent,pairObj(i,j)%areaR,pairObj(i,j)%perR,pairObj(i,j)%difAngle,pairObj(i,j)%aspectR,pairObj(i,j)%complexR,0.0,pairObj(i,j)%confAngle,pairObj(i,j)%objSize,weight,grid_res,interest,total)
		  
               pairObj(i,j)%t_interest = total

	       !print*, 'total_interest_tresh, pairObj(i,j)%t_interest', total_interest_tresh, pairObj(i,j)%t_interest

	       if (pairObj(i,j)%t_interest .GE. total_interest_tresh) then
		  cont = cont + 1
		  atrib_treshAux(cont) = pairObj(i,j)		     
	       endif		  
	    enddo
	 enddo
	 
         !print*
	 !print*, 'cont', cont

         allocate(atrib_tresh(cont))
	 Do i=1, cont
	   atrib_tresh(i) = atrib_treshAux(i)
         Enddo
	 
	 if (cont .GT. 2) then            
	    call quick_sort_original(atrib_tresh, 1, cont)  ! (subroutine defined in m_mode_pairAttrib)    
	    !print*
	    !print*, 'atrib_tresh', atrib_tresh
	    atrib_tresh1 => atrib_tresh(1)
	    !print*
	    !print*, 'atrib_tresh1', atrib_tresh1
	    !print*
	    !print*, 'totalObj', totalObj
	    !print*
	    !print*, 'Objects(1)%pts_per 2', objects(1)%pts_per
            !print*
	    call merging(atrib_tresh1, objects, totalObj, maskObj) ! (subroutine defined in m_mode_pairAttrib)
	    !print*, 'merging totalObj', totalObj 
	    flag = 1
	 else
	    flag = 0
	 endif
	 !Enddo
         deallocate(atrib_tresh)
	Enddo
	
	deallocate(pairObj)        

        deallocate (cfilter)
	deallocate (convField)
	deallocate (maskField)	

        return
      END SUBROUTINE mode_ObjectIdentf
    !**************************************************************************************************************************************    




END MODULE m_mode_objects
