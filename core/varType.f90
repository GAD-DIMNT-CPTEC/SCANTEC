module varType
   implicit none
   private

   public :: variable
   public :: verify
!   public :: assign_
   public :: assignment(=)

   type :: varBase
      character(len=10) :: Name_
      integer :: len_
      real    :: min_
      real    :: max_
      real    :: avg_
      real    :: std_
      contains
         procedure :: construct => varBase_construct
   end type

   type, extends(varBase) :: scalar
      real :: value_
   end type
   type, extends(varBase) :: array1D
      real, allocatable :: value_(:)
   end type
   type, extends(varBase) :: array2D
      real, allocatable :: value_(:,:)
   end type
   type, extends(varBase) :: array3D
      real, allocatable :: value_(:,:,:)
   end type

   interface scalar
      procedure :: scalar_construct
   end interface

   interface array1D
      procedure :: array1D_construct
   end interface

   interface array2D
      procedure :: array2D_construct
   end interface

   interface array3D
      procedure :: array3D_construct
   end interface

   type variable
      class(varBase), allocatable :: v
      contains
         procedure :: init0d, init1d, init2d, init3d
         generic   :: put => init0d, init1d, init2d, init3d

         procedure :: get0d, get1d, get2d, get3d
         generic   :: get => get0d, get1d, get2d, get3d

   end type

   interface assignment(=)
      module procedure assign_       
   end interface

   interface verify
      module procedure :: verifyS, verifyA
   end interface

   contains
      
      subroutine init0d(this, vname, val)
         class(variable)  :: this
         character(len=*) :: vName
         real             :: val
         if (allocated(this%v))then
            deallocate(this%v)
         endif
         this%v = scalar(vname,val)
      end subroutine

      subroutine init1d(this, vname, val)
         class(variable)  :: this
         character(len=*) :: vName
         real             :: val(:)

         if (allocated(this%v))then
            deallocate(this%v)
         endif

         this%v = array1D(vname,val)
      end subroutine

      subroutine init2d(this, vname, val)
         class(variable)  :: this
         character(len=*) :: vName
         real, pointer    :: val(:,:)

         if (allocated(this%v))then
            deallocate(this%v)
         endif

         this%v = array2D(vname,val)
      end subroutine

      subroutine init3d(this, vname, val)
         class(variable)  :: this
         character(len=*) :: vName
         real, pointer    :: val(:,:,:)

         if (allocated(this%v))then
            deallocate(this%v)
         endif

         this%v = array3D(vname,val)
      end subroutine


      subroutine get0d(this, value_)
         class(variable) :: this
         real            :: value_

         select type(ref=>this%v)
         class is (scalar)
            value_ =  ref%value_
         class default
            print*, 'ERROR: wrong type, should be of `scalar` type '
            stop 99028
         end select

      end subroutine

      subroutine get1d(this, return_value)
         class(variable) :: this
         real            :: return_value(:)

         integer :: i, j, k
         integer :: isize, jsize

         select type(ref=>this%v)
         type is (scalar)
            ! when return is a array1D
            ! but type is a scalar
            ! return all array with a constant 
            ! scalar value
            return_value = ref%value_
         type is (array1D)
            return_value = ref%value_
         type is (array2D)
            ! when return is a array1D
            ! but type is a array2D
            ! reshape array2D and return 
            ! at array1D
            isize = size(ref%value_,1)
            jsize = size(ref%value_,2)
            k = 1
            do j=1,jsize
               do i=1,isize
                  return_value(k) = ref%value_(i,j)
                  k = k + 1
               enddo
            enddo
         class default
            print*, 'ERROR: wrong type, should be of `array1D` type '
            stop 99029
         end select

      end subroutine

      subroutine get2d(this, return_value)
         class(variable) :: this
         real            :: return_value(:,:)

         select type(ref=>this%v)
         type is (scalar)
            ! when return is a array
            ! but type is a scalar
            ! return all array with a constant 
            ! scalar value
            return_value = ref%value_

         type is (array2D)
            return_value = ref%value_
         class default
            print*, 'ERROR: wrong type, should be of `array1D` type '
            stop 99030
         end select

      end subroutine

      subroutine get3d(this, return_value)
         class(variable) :: this
         real            :: return_value(:,:,:)

         select type(ref=>this%v)
         type is (scalar)
            ! when return is a array
            ! but type is a scalar
            ! return all array with a constant 
            ! scalar value
            return_value = ref%value_

         type is (array3D)
            return_value = ref%value_
         class default
            print*, 'ERROR: wrong type, should be of `array1D` type '
            stop 99031
         end select

      end subroutine

      subroutine varBase_construct(this, Name_, value_)
         class(varBase)   :: this
         character(len=*) :: Name_
         real, optional   :: value_(:)

         real    :: avg_, std_
         integer :: isize_

         this%Name_ = trim(Name_)
         if(present(value_))then
            isize_ = size(value_)
            if (iSize_ .gt. 1)then
               avg_   = sum(value_)/iSize_
               std_   = sum(Value_-avg_)/(iSize_-1)
            else
               avg_ = value_(1)
               std_ = 0.0
            endif
            this%len_ = iSize_
            this%avg_ = avg_
            this%std_ = std_
            this%min_ = minval(value_)
            this%max_ = maxval(value_)
         endif
      end subroutine

      function scalar_construct(Name_, value_)
         character(len=*) :: Name_
         real             :: value_
         type(scalar)     :: scalar_construct

         scalar_construct%value_ =  value_
         call scalar_construct%construct(name_, [value_])

      end function

      function array1D_construct(Name_, value_)
         character(len=*) :: Name_
         real             :: value_(:)
         type(array1D)    :: array1D_construct

          array1D_construct%value_ = value_
          call array1D_construct%construct(name_, value_)

      end function

      function array2D_construct(Name_, value_)
         character(len=*) :: Name_
         real             :: value_(:,:)
         type(array2D)    :: array2D_construct
         integer          :: isize

         array2D_construct%value_ =  value_
         isize = size(value_)
         call array2D_construct%construct(name_, reshape(value_,[isize]))

      end function

      function array3D_construct(Name_, value_)
         character(len=*) :: Name_
         real             :: value_(:,:,:)
         type(array3D)    :: array3D_construct
         integer          :: isize

         array3D_construct%value_ =  value_
         isize = size(value_)
         
         call array3D_construct%construct(name_, reshape(value_,[isize]))

      end function

      subroutine identify(this)
         class(varBase) :: this

         select type(this)
         type is (scalar)
            print*, 'type is scalar'
         type is (array1D)
            print*, 'type is array1D'
         type is (array2D)
            print*, 'type is array2D'
         type is (array3D)
            print*, 'type is array3D'
         class default
            print*, 'none of them ... '
         end select
      end subroutine

      subroutine verifyS(item)
         type(variable), intent(in) :: item


         call identify(item%v)
         print*,'Name   :', item%v%Name_
         print*,'length :', item%v%len_
         print*,'minimum:', item%v%min_
         print*,'maximum:', item%v%max_
         print*,'Average:', item%v%avg_
         print*,'Std    :', item%v%std_

      end subroutine


      subroutine verifyA(item)
         type(variable), intent(in) :: item(:)

         integer :: i
         integer :: isize
         
         isize = size(item)
         do i=1,isize
            call verifyS(item(i))
         enddo

      end subroutine

      subroutine assign_(a,b)
         class(variable), intent(inout) :: a
         class(variable), intent(in   ) :: b
         
         select type(ref=> b%v)
         type is (scalar)
!            print*,'scalar'
            a%v = b%v
         type is (array1d)
!            print*,'array1d'
            a%v = b%v
         type is (array2d)
!            print*,'array2d'
            a%v = b%v
         type is (array3d)
!            print*,'array3d'
            a%v = b%v
         end select
      end subroutine

end module
