MODULE m_basic
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
! !ROUTINE: m_basic.f90
!  \label{m_basic}
!
! !REVISION HISTORY:
!  Initial Code :: Joao Gerd - 02Sep2010
!BOP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

   IMPLICIT NONE
   PRIVATE

   CONTAINS


!BOP------------------------------------------------------------------!
! !IROUTINE: corr - calulate pearson correlation
!
! !DESCRIPTION:
!     Function to calculate pearson correlation between 2D fields.
!
! !INTERFACE:
!
   REAL FUNCTION corr (x,y)
      IMPLICIT NONE
      Real, Intent(In), Dimension(:,:) :: X
      Real, Intent(In), Dimension(:,:) :: Y
!
! !REVISION HISTORY:
!     02Sep2010 - Joao Gerd - Initial Code
!
!EOP------------------------------------------------------------------!
!
    Real    :: Sxy, Sxx, Syy
    Real    :: Mx, My
    Integer :: NPts, Ntime   
!

    NPts  = size(X,1)
    Ntime = size(X,2)

    Mx  = sum(X)/float(NPts*Ntime)
    My  = sum(Y)/float(NPts*Ntime)

    Sxy = sum( (X-Mx)*(Y-My) )
    Sxx = sum( (X-Mx)*(X-Mx) )
    Syy = sum( (Y-My)*(Y-MY) )
    corr=  Sxy/sqrt(Sxx*Syy)

   END FUNCTION
!BOP------------------------------------------------------------------!
! !IROUTINE: std - calulate standad deviation
!
! !DESCRIPTION:
! 
!
! !INTERFACE:
!
   REAL FUNCTION std (z)
      IMPLICIT NONE
      Real, Intent(In), Dimension(:,:) :: z
!
! !REVISION HISTORY:
!     02Sep2010 - Joao Gerd - Initial Code
!
!EOP------------------------------------------------------------------!
!
    Integer :: Npts, NRec
    Integer :: K
    !

    Npts = size(Z,1)
    NRec = size(Z,2)


    Do K = 1,Npts

       Zmean(K) = Sum(Z(K,1:NRec))/float(NRec)

       Zanom(K,1:NRec) = Z(K,1:NRec) - Zmean(K)

       Zsdev(K) = Sqrt( Sum(Zanom(K,1:NRec)**2)/float(NRec-1) )

    End Do

END MODULE
