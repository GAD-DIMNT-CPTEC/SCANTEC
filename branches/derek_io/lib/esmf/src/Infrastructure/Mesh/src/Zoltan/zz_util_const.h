/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * For more info, see the README file in the top-level Zoltan directory.     *  
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: zz_util_const.h,v $
 *    $Author: dneckels $
 *    $Date: 2007/11/28 16:13:55 $
 *    Revision: 1.9 $
 ****************************************************************************/


#ifndef __ZOLTAN_UTIL_CONST_H
#define __ZOLTAN_UTIL_CONST_H

#include "zoltan_types.h"

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

extern unsigned int Zoltan_Hash(ZOLTAN_ID_PTR, int, unsigned int);
extern int Zoltan_Clean_String(const char *, char **);
extern char *Zoltan_Strdup(const char *);
void Zoltan_Transform_Point( double *p, double (*m)[3], int *a, int d,
  int ndims, double *v);
void Zoltan_Transform_Box(double *lo, double *hi, double (*m)[3], int *a, 
  int d, int ndims);
void Zoltan_Transform_Box_Points(double *lo, double *hi, double (*m)[3], 
  int *a, int d, int ndims, double (*v)[3]);
/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif
