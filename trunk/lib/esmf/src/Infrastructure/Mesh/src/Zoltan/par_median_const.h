/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * For more info, see the README file in the top-level Zoltan directory.     *  
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: par_median_const.h,v $
 *    $Author: dneckels $
 *    $Date: 2007/11/28 16:13:52 $
 *    Revision: 1.14 $
 ****************************************************************************/


#ifndef __PAR_MEDIAN_CONST_H
#define __PAR_MEDIAN_CONST_H

#include <mpi.h>

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif

extern int Zoltan_RB_find_median(int, double *, double *, int *,
  int, int, double, MPI_Comm,
  double *, int, int *, int, int,
  int, int, int, double, double, double,
  double *, double *, int *, int, int);

/* Prototype for function used with TFLOPS_SPECIAL */
extern void Zoltan_RB_reduce(int, int, int, void*, void*,
                             int, int*, MPI_Datatype, MPI_Comm, 
                             MPI_User_function);


#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif
