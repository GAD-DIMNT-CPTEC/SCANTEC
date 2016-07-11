!-----------------------------------------------------------------------------------!
!             Group on Data Assimilation Development - GDAD/CPTEC/INPE              !
!                                                                                   !
!                                                                                   !
!       AUTHORS: Arletis Roque Carrasco                                             !
!              	 Luiz Fernando Sapucci                                              !
!										    !
!       Adapted from the work of: Arletis Roque Carrasco                            !
!				  Maibys Sierra Lorenzo                             !
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

   USE scamtec_module                 ! module where the structure samtec is defined
   USE SCAM_dataMOD, only : scamdata  ! SCANTEC data matrix

   IMPLICIT NONE
   PRIVATE

   integer, allocatable :: Idx(:)     ! Array to save undefined points index
   integer              :: nidx       ! Total undefined points
 
   real, pointer     :: prefield(:,:) ! Pointer to save precipitation observation data
   real, pointer     :: expfield(:,:) ! Pointer to save precipitation experiment data 
   
   real, allocatable :: precOriginalField(:,:)  ! Matrix to save precipitation observation data 
   real, allocatable :: expOriginalField(:,:)   ! Matrix to save precipitation experiment data 

   public :: mode_init
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
	 precOriginalField = RESHAPE(prefield, (/scamtec%nypt,scamtec%nxpt/))
	 expOriginalField = RESHAPE(expfield, (/scamtec%nypt,scamtec%nxpt/))         

         print*,'mode_init'

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
      SUBROUTINE mode_run(nexp)
         Implicit None
         integer, intent(in) :: nexp ! experiment number

	 real, allocatable    :: cfilter(:,:)     ! Matrix to save circular filter's values
	 real, allocatable    :: convField(:,:)   ! Field resulting of convolution process
	 integer, allocatable :: maskField(:,:)
	 real, allocatable    :: restoreField(:,:)
         INTEGER :: I, J, rowSize, colSize, radio
	 real    :: treshold

	 ! exemplos para comprobar os algoritmos
	 rowSize=3
	 colSize=4
	 ! os valores do radio e o limiar devem ser definidos no scamtec.conf
	 radio=1
         treshold=2.01

	 call mode_init(nexp)

	 allocate(cfilter(rowSize,colSize))
	 allocate(convField(rowSize,colSize))
	 allocate(maskField(rowSize,colSize))
	 allocate(restoreField(rowSize,colSize))

         

         deallocate (cfilter)
	 deallocate (convField)
	 deallocate (maskField)



      END SUBROUTINE mode_run
    !**************************************************************************************************************************************




    !**************************************************************************************************************************************
      Subroutine circular_filter(filter, radio) 
	 ! Subroutine where the circular filter used in the convolution process is calculated.
         ! Circular filter has dimensions 3x3.
	 ! Convolution radius is defined by the user

         Implicit None
	 ! INPUT PARAMETERS:
	 integer, intent(in) :: radio    ! nrows= number of rows, ncols= number of columns, radio= radio used in the convolution process

	 ! OUTPUT PARAMETER:
	 real, allocatable, intent(out) :: filter(:,:)     ! Matrix to save circular filter's values

         integer           :: i, j, x, y, filter_dimension=3
	 real, parameter   :: pi=3.141592654

         allocate(filter(filter_dimension,filter_dimension))

	! Loop para calcular cada ponto (i,j) do filtro circular
	 do i=1, filter_dimension
	    x=i-filter_dimension/2
	    do j=1, filter_dimension
	       y=j-filter_dimension/2
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
	 real, allocatable, intent(out) :: convField(:,:)  ! Field resulting of convolution process - Convolution matrix

	 integer	:: nrow, ncol, frow, fcol, ccol, crow, filter_dimension=3

	 allocate(convField(ysize, xsize))

	! Loop para calcular cada ponto (i,j) do campo transformado pelo algoritmo de convolução: 
	 do nrow=1, ysize            
	    do ncol=1, xsize
	       convField(nrow,ncol)=0
	       do frow=1, filter_dimension		  
		  do fcol=1, filter_dimension			
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
      Subroutine tresholding(originalField, convField, mask, restoreField, ysize, xsize, treshold)
       ! The subroutine creates a mask field based on a user-defined threshold,  and the raw data are restored to create the object field 

	Implicit None
	! INPUT PARAMETERS: 
	real, allocatable, intent(in) :: originalField(:,:), convField(:,:) 
	integer, intent(in)           :: xsize, ysize    ! Convolution matrix dimensions
	real, intent(in)	      :: treshold

	! OUTPUT PARAMETER:
	integer, allocatable, intent(out) :: mask(:,:)         ! Binary field -mask- resulting of tresholding process 
	real, allocatable, intent(out)    :: restoreField(:,:) ! Matrix to save original field values where the mask is 1 

	integer    :: i,j 

	allocate(mask(ysize,xsize))
	allocate(restoreField(ysize,xsize))

	do i=1, ysize
	   do j=1, xsize
	      if (convField(i,j) .GE. treshold) then
		 mask(i,j)=1
	      else
	         mask(i,j)=0
	      endif
	      restoreField(i,j)= mask(i,j)*originalField(i,j)
	   enddo
	enddo

      End Subroutine
    !**************************************************************************************************************************************








END MODULE mode

     

