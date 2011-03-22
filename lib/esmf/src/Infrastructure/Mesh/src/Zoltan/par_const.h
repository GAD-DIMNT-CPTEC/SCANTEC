/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * For more info, see the README file in the top-level Zoltan directory.     *  
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: par_const.h,v $
 *    $Author: dneckels $
 *    $Date: 2007/11/28 16:13:52 $
 *    Revision: 1.17 $
 ****************************************************************************/


#ifndef __PAR_CONST_H
#define __PAR_CONST_H

#include <mpi.h>

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif

extern void Zoltan_Print_Sync_Start(MPI_Comm, int);
extern void Zoltan_Print_Sync_End(MPI_Comm, int);
extern void Zoltan_Print_Stats (MPI_Comm, int, double, char *);

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif
