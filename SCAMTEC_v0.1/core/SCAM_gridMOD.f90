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

module SCAM_gridMOD
!BOP
!
! !MODULE: SCAM_gridMod
!
! !DESCRIPTION:
!  The code in this file provides a description of the grid data structure in SCAMTeC
!
!  \subsubsection{Overview}
!
!
!  \begin{description}
!   \item[lat] 
!    latitude of the grid cell
!   \item[lon] 
!    longitude of the grid cell
!   \item[col] 
!     index of the grid point along the East-West grid dimension
!   \item[row] 
!    index of the grid point along the North-South grid dimension
!   \item[elev] 
!    Topological elevation of the grid cell
!   \item[slope] 
!    Topological slope of the grid cell
!   \item[aspect] 
!    Topological aspect of the grid cell
!   \item[curv] 
!    Topological curvature of the grid cell
!   \end{description}
!
! !REVISION HISTORY:
!  30 Sep 2010: Joao Gerd Initial Code
!
!EOP
  implicit none
  public griddec
  type griddec
     real            :: lat    
     real            :: lon
     real            :: lev
     integer         :: col   
     integer         :: row  
     real            :: elev   
     real            :: dat
  end type griddec

end module SCAM_gridMOD


