!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                  SCAMTeC - GDAD/CPTEC/INPE - 2010                   !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
! Copyright 2010 Free Software Foundation, Inc.                       !
!                                                                     !
! This program is free software; you can redistribute it and/or modify!
! it under the terms of the GNU General Public License as published by!
! the Free Software Foundation; either version 2 of the License, or   !
! (at your option) any later version.                                 !
!                                                                     !
! This program is distributed in the hope that it will be useful,     !
! but WITHOUT ANY WARRANTY; without even the implied warranty of      !
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       !
! GNU General Public License for more details.                        !
!                                                                     !
! You should have received a copy of the GNU General Public License   !
! along with GNU Emacs; see the file COPYING.  If not, write to the   !
! Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,    !
! Boston, MA 02110-1301, USA.                                         !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!BOP
!
MODULE SCAM_outputMOD
   USE scamtec_module
   USE m_ioutil
   USE m_die

   IMPLICIT NONE

   character(len=*),parameter :: myname='SCAM_outputMod'


   contains

   subroutine write_2d(field, fname)
      implicit none

      character(len=*),parameter :: myname_=myname//'::write_2d'

      real, dimension(:,:), intent(in) :: field
      character(len=*), intent(in)     :: fname

      integer          :: lu
      integer          :: recl
      integer          :: ier
      character(len=20) :: status


      lu     = luavail()      
      recl   = scamtec%npts*4
      status = "unknown"

      call opnieee(lu, trim(fname), status, ier)!, recl)
	   if(ier.ne.0) then
	      call perr(myname_,'opnieee("'//	&
                   trim(fname)//'")',ier)
	      return
	   endif

      write(lu) field

      call clsieee(lu,ier)
      if(ier/=0) then
         call perr(myname_,'deallocate()',ier)
!         if(.not.present(stat)) call die(myname_)
!         stat=ier
         return
      endif

   end subroutine
END MODULE SCAM_outputMOD
