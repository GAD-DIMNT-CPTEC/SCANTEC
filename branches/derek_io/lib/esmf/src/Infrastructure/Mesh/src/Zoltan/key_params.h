/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * For more info, see the README file in the top-level Zoltan directory.     *  
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: key_params.h,v $
 *    $Author: dneckels $
 *    $Date: 2007/11/28 16:13:50 $
 *    Revision: 1.6 $
 ****************************************************************************/


#ifndef __KEY_PARAMS_H
#define __KEY_PARAMS_H

#include "zz_const.h"

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif

extern int Zoltan_Set_Key_Param(ZZ *, const char *, const char *, int);
extern void Zoltan_Print_Key_Params(ZZ const *);

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif
