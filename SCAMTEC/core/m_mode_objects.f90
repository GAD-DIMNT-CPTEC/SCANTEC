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
! 

MODULE m_mode_objects

   USE m_mode_singleAttrib

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
      Subroutine singleObj_Ident_Attrib(row, col, restoreField, ysize, xsize, treshold, mask, totalObj, maskObj, perimeter, area, xcent, ycent, angle, aspect_ratio)

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
	   real, intent(out)				:: angle, aspect_ratio	   

	   integer				    	:: is_valid, yini, xini, n, ip, ip2, ys, xs, ps, xds, pds, yi, xi, pi, xdi, pdi
	   integer				    	:: leftPoint, rightPoint, upperPoint, lowerPoint 
	   integer				    	:: nsx, nsy  
	   integer					:: i, nhull	   
	   integer				    	:: xleft, xright, x, temp, area_hull_int
           
           type(mark), pointer 		    		:: m, maux, maux1, maux2, points, total_points, points_aux, total_points_aux 
	   type(mark), pointer		    		:: per_linked, total_linked, hull
	   type(point), pointer				:: pts_per(:), convex(:)	   

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
	   print*, '************  Inicio loop exterior  **********'
	   print*, ' Objetos detectados', totalObj+1
      
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
			     	      nullify(maux%next) 		              
				   else
				         allocate(m)				   
				         m%x = ip    
			     	         m%y = ys 
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
			               nullify(maux%next)			            			            
				    else				       
				          allocate(m)				    
				          m%x = ip
				          m%y = yi
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
			     	      nullify(maux%next) 		              
				   else
				         allocate(m)				   
				         m%x = ip2    
			     	         m%y = ys 
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
			     	      nullify(maux%next) 		              
				   else
				         allocate(m)				   
				         m%x = ip2    
			     	         m%y = yi 
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

	   xcent = nsx/area	! centroid position x
	   ycent = nsy/area	! centroid position y	   

	   print*, 'perimetro', perimeter, 'area', area, 'xcent', xcent, 'ycent', ycent     
   
	   ! Object Orientation Angle 
	   call object_angle(points, xcent, ycent, perimeter, pts_per, angle)
	   print*, 'orientation angle', angle	   
	   
	   ! Object Aspect Ratio
	   call object_Aspect_Ratio(pts_per, xcent, ycent, perimeter, angle, aspect_ratio)
	   print*, 'Aspect Ratio', aspect_ratio

           
	   ! compute the convex hull and complexity
!	   allocate(convex(perimeter))
!	   nhull = 1
!	   if (perimeter .GT. 4) then
!	      call quick_hull(pts_per, perimeter, hull)	      
	      
!	      do while (associated(hull))		 
!		 convex(nhull)%x = hull%x
!		 convex(nhull)%y = hull%y		 
!		 hull => hull%next
!		 nhull= nhull+1	
		 !print*, 'convex(nhull)%x', convex(nhull)%x
		 !print*, 'convex(nhull)%y', convex(nhull)%y	 	 	 
		 !enddo
!	      enddo
	      !print*, 'nhull', nhull
	      !print*, 'convex(nhull)', convex
	      !ordenar counterclockwise con quicksort
	      !call quick_sort(convex, 1, nhull-1)
!	   else
!	      area_hull_int = area
!	   endif


           return
	
      End Subroutine
    !**************************************************************************************************************************************










END MODULE m_mode_objects
