//-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
// NASA Goddard Space Flight Center Land Information System (LIS) V5.0 BETA
// Released January 2008
//
// See SOFTWARE DISTRIBUTION POLICY for software distribution policies
//
// The LIS source code and documentation are in the public domain,
// available without fee for educational, research, non-commercial and
// commercial purposes.  Users may distribute the binary or source
// code to third parties provided this statement appears on all copies and
// that no charge is made for such copies.
//
// NASA GSFC MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE
// SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED AS IS WITHOUT EXPRESS OR
// IMPLIED WARRANTY.  NEITHER NASA GSFC NOR THE US GOVERNMENT SHALL BE
// LIABLE FOR ANY DAMAGES SUFFERED BY THE USER OF THIS SOFTWARE.
//
// See COPYRIGHT.TXT for copyright details.
//
//-------------------------END NOTICE -- DO NOT EDIT-----------------------
#include <stdio.h>
#include <stdlib.h>
//BOP
//
// !ROUTINE: ft_check_index
// \label{ft_check_index}
//
// !INTERFACE:
void ft_check_index(int index, int max, char * regrtn)
// !DESCRIPTION:
//  When registering a plugin into a function pointer table,
//  one must specify where (the index) into the table to place this plugin.
//  This routine checks the index into the function pointer table against
//  the table's maximum value.
//
//  The purpose of this check is to avoid overwriting a function pointer
//  table and causing a memory error.
//
//  This routines exits with a -1 if the index fails the test.
//
//  The arguments are: 
//  \begin{description}
//   \item[index]
//     Position into the function pointer table
//   \item[max]
//     Maximum value of index
//   \item[regrtn]
//     String containing the name of the register routine.  Used for
//     printing an error message.
//  \end{description}
//EOP
{
   /* index should be in the range [0, max-1] */
   if ( index >= max )
   {
      printf("ERR: %s -- %d > %d\n", regrtn, index, max);
      exit(-1);
   }
}
