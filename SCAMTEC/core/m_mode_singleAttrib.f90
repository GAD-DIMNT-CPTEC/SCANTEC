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

! MODULE: m_mode_singleAttrib.f90
!
! DESCRIPTON:
! 


MODULE m_mode_singleAttrib

   ! ESTRUTURAS USADAS NA SUBROUTINE  singleObjIdent_Attrib   PARA IDENTIFICAR OBJETOS E ATRIBUTOS
   type mark			
      integer			:: x, y
      type(mark), pointer	:: next
   end type
      !type(mark), pointer	:: points
      
   type point
      integer	:: x, y
   end type    
      !type(point), allocatable		:: pts_per(:), total_pts(:)  

   type attrs
      integer				:: id, area, xcent, ycent, perimetro
      type(point), pointer		:: pts_per, total_pts
   end type

   public :: object_angle
   public :: object_Aspect_Ratio
   public :: quick_hull
   public :: Hull_Set
   public :: orientation
   public :: distance
   public :: quick_sort

   contains

    !**************************************************************************************************************************************
     !The subroutine calculates the orientation of the objects
      Subroutine object_angle(per_points, xcent, ycent, perimeter, pts_per, angle)        

	Implicit None
	! INPUT PARAMETERS:
	type(mark), pointer			:: per_points
        integer, intent(in)			:: xcent, ycent, perimeter	
	!OUTPUT PARAMETERS:
	type(point), pointer, intent(out)	:: pts_per(:)
	real, intent(out)			:: angle

	!type(point), pointer	:: pts_per(:)
	integer			:: i, j
	real, parameter   	:: pi=3.141592654
	real			:: sumUp, sumDown

	sumUp=0.0
	sumDown=0.0

	allocate(pts_per(perimeter))

	Do while (associated(per_points))
	   do i=1, perimeter
	      pts_per(i)%x = per_points%x
	      pts_per(i)%y = per_points%y

	      sumUp = sumUp + (pts_per(i)%x - xcent) * (pts_per(i)%y - ycent)
	      sumDown = sumDown + (pts_per(i)%x - xcent)**2 - (pts_per(i)%y - ycent)**2
	
	      per_points => per_points%next
	   enddo
        Enddo

	angle = 0.5*ATAN2(2*sumUp,sumDown)

	if (angle .LT. 0.0) then
	   angle = angle + 2*pi
        else if (angle .GT. pi) then
	   angle = angle - pi
	endif
	
	return
      End Subroutine 
    !**************************************************************************************************************************************


    
    !**************************************************************************************************************************************
     !The subroutine calculates the aspect ratio of the objects
      Subroutine object_Aspect_Ratio(pts_pos, xcent, ycent, perimeter, angle, aspect_ratio)
      
	Implicit None
	! INPUT PARAMETERS:
	type(point), pointer, intent(in)	:: pts_pos(:)
        integer, intent(in)	:: xcent, ycent, perimeter
	real, intent(in)	:: angle
	!OUTPUT PARAMETERS:
	real, intent(out)	:: aspect_ratio

	integer			:: i
	real			:: dist, vmajor, major_up, major_down, vminor, minor_down, minor_up, major_axis, minor_axis 
	real, parameter   	:: pi=3.141592654        
	
	major_up=0
	major_down=0
	minor_down=0
	minor_up=0

	! calculate the maximum and minimum distances from semiaxes to the perimeter
	Do i=1, perimeter
	   
	   dist =  (pts_pos(i)%x - xcent)*SIN(angle) - (pts_pos(i)%y - ycent)*COS(angle)
	   vmajor = (pts_pos(i)%y - ycent) - TAN(angle)*(pts_pos(i)%x - xcent)
	   
	   If (vmajor .GT. 0) then
	      if(dist .GT. major_up) then
		 major_up = dist
	      endif
	   Endif

	   If (vmajor .LT. 0) then
	      if(dist .GT. major_down) then
		 major_down = dist
	      endif
	   Endif	   
	   
	   dist = (pts_pos(i)%x - xcent)*SIN(angle + pi/2) - (pts_pos(i)%y - ycent)*COS(angle + pi/2)
	   vminor = (pts_pos(i)%y - ycent) + (pts_pos(i)%x - xcent)/TAN(angle)
	   
	   If (vminor .GT. 0) then
	      if(dist .GT. minor_up) then
		 minor_up = dist
	      endif
	   Endif

	   If (vminor .LT. 0) then
	      if(dist .GT. minor_down) then
		 minor_down = dist
	      endif
	   Endif
	   
	Enddo

	major_axis = major_up + major_down
	minor_axis = minor_up + minor_down	
        
	If (major_axis .LT. minor_axis) then
	   aspect_ratio = major_axis/minor_axis
	else 
	   aspect_ratio = minor_axis/major_axis
	Endif
	
	return
      End Subroutine 
    !**************************************************************************************************************************************



      !**************************************************************************************************************************************
     ! Quickhull algorithm to find the convex hulls of the objects
      Subroutine quick_hull(array_pts, npoints, hull)
	
         Implicit None
	 ! INPUT PARAMETERS:
	 type(point), pointer, intent(in)	:: array_pts(:)
	 integer, intent(in)			:: npoints
	 ! OUTPUT PARAMETERS:
	 type(mark), pointer, intent(out)	:: hull

	 type(point), pointer			:: AB(:), temp, left_set(:), right_set(:), array_aux, aux_AB1, aux_AB2
	 type(mark), pointer			:: aux, hull_temp
	 integer				:: i, j, k, positions, nleft, nright, xmin, ymin, xmax, ymax, minpos, maxpos, side, tempAx, tempAy, tempBx, tempBy, tempx, tempy, size_line

	 nleft=1
	 nright=1
	 xmin=9999
	 xmax=0
	 
	 nullify(hull_temp)
	 
	 !print*, 'array_pts(:)', array_pts
	 !Loop para buscar los xmin y xmax
	 Do i=1, npoints	    
	    If (array_pts(i)%x .LT. xmin) then
	       xmin = array_pts(i)%x
	       ymin = array_pts(i)%y
	       minpos = i
	       !print*
	       !print*, 'xmin', xmin, 'ymin', ymin
		!print*, ' minpos', minpos
	    Endif

	    If (array_pts(i)%x .GT. xmax) then
	       xmax = array_pts(i)%x
	       ymax = array_pts(i)%y
	       maxpos = i
	       !print*
	       !print*, 'xmax', xmax, 'ymax', ymax
		!print*, 'maxpos', maxpos
	    Endif
	 Enddo

	 print*	 
	 print*, ' minpos final', minpos, '    maxpos final', maxpos
	 ! pts extremos de las x
	 allocate(AB(2))

	 tempAx = array_pts(minpos)%x
	 tempAy = array_pts(minpos)%y
	 tempBx = array_pts(maxpos)%x
	 tempBy = array_pts(maxpos)%y

	 AB(1)%x = tempAx
	 AB(1)%y = tempAy
	 AB(2)%x = tempBx
	 AB(2)%y = tempBy
	 !A => array_pts(minpos)
	 !B => array_pts(maxpos)

	 !print*, 'array_pts(minpos)', array_pts(minpos)
	 

	 !allocate(temp)
	 !temp => array_pts(minpos)
         tempx = array_pts(minpos)%x
	 tempy = array_pts(minpos)%y	 
	 array_pts(minpos)%x = array_pts(1)%x
	 array_pts(minpos)%y = array_pts(1)%y	 
	 array_pts(1)%x = tempx	
	 array_pts(1)%y = tempy

	 !print*, 'tempx', tempx, 'tempy', tempy
	 !print*, 'array_pts(minpos)', array_pts(minpos)
	 !print*, 'array_pts(1)', array_pts(1) 
	 

	 !temp => array_pts(maxpos)
	 tempx = array_pts(maxpos)%x
	 tempy = array_pts(maxpos)%y
	 array_pts(maxpos)%x = array_pts(2)%x
	 array_pts(maxpos)%y = array_pts(2)%y
         array_pts(2)%x = tempx
	 array_pts(2)%y = tempy

	 print*
	 print*, 'array_pts', array_pts

	 ! guardo A y B como parte de la convex hull
	 allocate(aux)
	 aux%next => hull_temp
	 hull_temp => aux
	 hull_temp%x = AB(1)%x;
         hull_temp%y = AB(1)%y;
!
	 allocate(aux)
	 aux%next => hull_temp
	 hull_temp => aux
	 hull_temp%x = AB(2)%x;
         hull_temp%y = AB(2)%y;

	 !do while(associated(aux))
	    !print*, 'A aux%x', aux%x
	    !print*, 'A aux%y', aux%y
	    !aux => aux%next
	 !enddo
	 
	 ! hallo los conjuntos a ambos lados del segmento AB
	 print*, 'hallo los conjuntos a ambos lados del segmento AB'
	 aux_AB1 => AB(1)
	 aux_AB2 => AB(2)
	 Do i=3, npoints
            array_aux => array_pts(i)	    
	    print*
	    print*, 'i', i, '    array_pts(i)', array_aux
	    print*
	    !print*, 'A', A, 'B', B
	    call orientation(aux_AB1, aux_AB2, array_aux, positions)
	    
	    print*
	    print*, 'positions', positions

	    if (positions .GT. 0) then  
	       nleft = nleft + 1
	    endif

	    if (positions .LT. 0) then  
	       nright = nright + 1
	    endif
	 Enddo

	 print*
	 print*, 'nleft', nleft, 'nright', nright

	 allocate(left_set(nleft))
	 allocate(right_set(nright))

	 j=1
	 k=1

	 print*, 'hallar left_set y right_set'
	 Do i=3, npoints
	    array_aux => array_pts(i)
	    print*
	    print*, 'i', i, '    array_pts(i)', array_aux
	    print*
	    !print*, 'A', A, 'B', B
	    call orientation(aux_AB1, aux_AB2, array_aux, positions)

	    print*
	    print*, 'positions1', positions
	    if (positions .GT. 0) then  
	       left_set(j) = array_pts(i)
	       j=j+1
	    endif

	    if (positions .LT. 0) then  
	       right_set(k) = array_pts(i)
	       k=k+1
	    endif
	 Enddo

	 print*
	 print*, 'j', j, 'k', k
	 print*, 'left_set', left_set
	 print*
	 print*, 'right_set', right_set

	 call Hull_Set(aux_AB1, aux_AB2, right_set, nright, hull_temp)
	 call Hull_Set(aux_AB1, aux_AB2, left_set, nleft, hull_temp)

	 hull => hull_temp

	 return
      End Subroutine
    !**************************************************************************************************************************************



    !**************************************************************************************************************************************
     ! determina los ptos que pertenecen a la convex hull
      Recursive Subroutine Hull_Set(A, B, set, array_size, hull_temp) 

	 Implicit None
	 ! INPUT PARAMETERS:
	 type(point), pointer, intent(in)	:: A, B, set(:)
	 integer, intent(in)			:: array_size

	 type(mark), pointer, intent(inout)	:: hull_temp

	 integer				:: furthest_point, nAP, nPB, i, j, k, size_line
	 type(mark),pointer			:: aux, hull_temp2
	 type(point), pointer			:: set_aux, set_aux2, leftset_AP(:), leftset_PB(:) 
	 real					:: dist, distmax, pdist

	 distmax = 0
	 nAP = 0
	 nPB = 0

	 hull_temp2 => hull_temp

	 ! compruebo el tamaño del arreglo
	 print*, 'array_size', array_size
	 if (array_size .EQ. 1) then
	    return
	 endif

	 if (array_size .EQ. 2) then
	    allocate(aux)
	    aux%next => hull_temp2
	    hull_temp2 => aux
	    hull_temp2%x = set(1)%x
	    hull_temp2%y = set(1)%y
	    !print*, 'termina'
	    return
	 endif
         
         ! determinar el pto mas lejano
	 Do i=1, array_size
	    set_aux => set(i)
	    call distance(A, B, set_aux, pdist)
	    dist = pdist

	    if (dist .GT. distmax) then
	       distmax = dist
	       furthest_point = i
	    endif
	 Enddo
	 	  
	 ! guardar el pto perteneciente a la convex hull
	 allocate(aux) 
	 aux%next => hull_temp2
	 hull_temp2 => aux
	 hull_temp2%x = set(furthest_point)%x
	 hull_temp2%y = set(furthest_point)%y

	 ! determinar los ptos a la izquierda de los segmentos AP y PB
	 Do i=1, array_size
	    set_aux => set(i)
	    set_aux2 => set(furthest_point)

	    call orientation(A, set_aux2, set_aux, size_line)
	    if (size_line .GT. 0) then
	       nAP = nAP+1
	    endif

	    call orientation(set_aux2, B, set_aux, size_line)
	    if (size_line .GT. 0) then
	       nPB = nPB+1
	    endif

         Enddo

	 allocate(leftset_AP(nAP), leftset_PB(nPB))
	 j=1
	 k=1
         Do i=1, array_size
	    set_aux => set(i+1)
	    set_aux2 => set(furthest_point)

	    call orientation(A, set_aux2, set_aux, size_line)
	    if (size_line .GT. 0) then
	       leftset_AP(j) = set_aux
	       j=j+1
	    endif

	    call orientation(set_aux2, B, set_aux, size_line)
	    if (size_line .GT. 0) then
	       leftset_PB(k) = set_aux
	       k=k+1
	    endif
	 Enddo

	 call Hull_Set(A, set_aux2, leftset_AP, nAP, hull_temp2)
	 call Hull_Set(set_aux2, B, leftset_PB, nPB, hull_temp2)

	 hull_temp => hull_temp2

	 return
      End Subroutine
    !**************************************************************************************************************************************





    !**************************************************************************************************************************************
      ! determine on which side of a line is a point located
      Subroutine orientation(p1, p2, p3, size_line)

	 Implicit None
	 type(point), pointer, intent(in)	:: p1, p2, p3
	 integer, intent(out) 			:: size_line

	 print*, 'p1', p1
 	 print*, 'p2', p2
	 print*, 'p3', p3

	 size_line = (p2%x - p1%x)*(p3%y - p1%y) - (p2%y - p1%y)*(p3%x - p1%x)
	 
	 print*, 'size_line', size_line 
         return

      End Subroutine
    !**************************************************************************************************************************************





    !**************************************************************************************************************************************
      ! find a pseudodistance from line to point
      Subroutine distance(p1, p2, p3, pdist)

	 Implicit None
	 type(point), pointer, intent(in)	:: p1, p2, p3
	 real, intent(out) 			:: pdist
	 real					:: ABx, ABy, m

	 ABx = p2%x - p1%x
	 ABy = p2%y - p1%y
	 m = ABy/ABx

	 pdist = (ABx*(p1%y - p3%y) - ABy*(p1%x - p3%x)) / SQRT(m**2 + 1)

	 if (pdist .LT. 0) then
	    pdist = (-1)*pdist
	 endif
         return

      End Subroutine
    !**************************************************************************************************************************************



    !**************************************************************************************************************************************
      ! Quicksort algorithm to sort arrays counterclockwise
      Recursive Subroutine quick_sort(arr, low, high) 

	Implicit None
	type(point), pointer, intent(inout)	:: arr(:)
	integer, intent(in)			:: low, high

	integer					:: pivot, i, j, size_line, tempx, tempy
	type(point), pointer			:: temp, arr_aux, arr_aux2, arr_aux3, arr_sort(:)
	print*, 'low, high', low, high
	if (low .lT. high) then
	   pivot = low
	   i = low
	   j = high
           do while (i .LT. j)
	      do while (i .LT. high) 
 		 arr_aux => arr(1)
		 arr_aux2 => arr(pivot)
		 arr_aux3 => arr(i)
 		 call orientation(arr_aux, arr_aux2, arr_aux3, size_line)
		 print*, 'size_line', size_line
		 if (size_line .GE. 0) then
		    i=i+1
		 endif
	      enddo	   

	      size_line=-1
	      do while (size_line .LT. 0)
		 print*, 'size_line1', size_line
	         arr_aux => arr(1)
	         arr_aux2 => arr(pivot)
	         arr_aux3 => arr(j)
	         call orientation(arr_aux, arr_aux2, arr_aux3, size_line)
		 print*, 'size_line3', size_line
	         if (size_line .LT. 0) then
	            j=j-1
		 endif
	      enddo
	      print*, 'i,j', i,j
	      if (i .LT. j) then
	         !temp => arr(i)
	         tempx = arr(i)%x
		 tempy = arr(i)%y
	         !arr(i) => arr(j)
	         arr(i)%x = arr(j)%x
		 arr(i)%y = arr(j)%y
	         !arr(j) => temp
	         arr(j)%x = tempx
		 arr(j)%y = tempy
	      endif

	      print*, 'arr', arr
	   enddo
	endif
	
	!temp => arr(pivot)
	tempx = arr(pivot)%x
	tempy = arr(pivot)%y
	!arr(pivot) => arr(j)
	arr(pivot)%x = arr(j)%x
	arr(pivot)%y = arr(j)%y
	!arr(j) => temp
	arr(j)%x = tempx
	arr(j)%y = tempy

        print*, 'arr2', arr
	print*, 'low', low, 'j-1', j-1

	call quick_sort(arr, low, j-1)
        print*, 'j+1', j+1, 'high', high
	call quick_sort(arr, j+1, high)

	!arr_sort => arr

      End Subroutine
    !**************************************************************************************************************************************






END MODULE m_mode_singleAttrib
