/*****************************************************************************
 * Zoltan Dynamic Load-Balancing Library for Parallel Applications           *
 * Copyright (c) 2000, Sandia National Laboratories.                         *
 * For more info, see the README file in the top-level Zoltan directory.     *
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: zz_sort.h,v $
 *    $Author: dneckels $
 *    $Date: 2007/11/28 16:13:55 $
 *    Revision: 1.3 $
 ****************************************************************************/

#ifndef ZOLTAN_SORT_H
#define ZOLTAN_SORT_H

#include "zz_const.h"

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif

/* Sorting */
void Zoltan_quicksort_pointer_dec_float_int (int*, float*, int*, int, int);
void Zoltan_quicksort_pointer_dec_float     (int*, float*, int,  int);
void Zoltan_quicksort_pointer_inc_float     (int*, float*, int,  int);
void Zoltan_quicksort_pointer_inc_int_int   (int*, int*,   int*, int, int);
void Zoltan_quicksort_list_inc_int          (int*, int,    int);
void Zoltan_quicksort_pointer_inc_int_mult  (int*, int,    int,  int*, int*);

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif   /* ZOLTAN_SORT_H_ */
