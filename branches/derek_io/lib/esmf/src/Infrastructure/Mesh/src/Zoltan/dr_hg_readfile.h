/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * For more info, see the README file in the top-level Zoltan directory.     *  
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: dr_hg_readfile.h,v $
 *    $Author: dneckels $
 *    $Date: 2007/11/28 16:13:47 $
 *    Revision: 1.6 $
 ****************************************************************************/


#ifndef _ZOLTAN_HG_READFILE_CONST_H_
#define _ZOLTAN_HG_READFILE_CONST_H_

#include <stdio.h>

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif

/* Hypergraph read from file */
int HG_readfile (int, FILE*, int*, int*, int*, int**, int**, int*,
 float**, int*, float**, int*);

/* MatrixMarket read from file */
int MM_readfile (int, FILE*, int*, int*, int*, int**, int**, int*,
 float**, int*, float**, int*, int);

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif 
