/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * For more info, see the README file in the top-level Zoltan directory.     *  
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: parmetis_jostle_const.h,v $
 *    $Author: dneckels $
 *    $Date: 2007/11/28 16:13:53 $
 *    Revision: 1.28 $
 ****************************************************************************/


#ifndef __PARMETIS_JOSTLE_CONST_H
#define __PARMETIS_JOSTLE_CONST_H

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif


/* Zoltan function prototypes */
extern int Zoltan_ParMetis_Set_Param(char *, char *);
extern int Zoltan_Jostle_Set_Param(char *, char *);

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif
