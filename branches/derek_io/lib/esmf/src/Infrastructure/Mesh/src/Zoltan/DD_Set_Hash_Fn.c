/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * This software is distributed under the GNU Lesser General Public License. *
 * For more info, see the README file in the top-level Zoltan directory.     *
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: DD_Set_Hash_Fn.c,v $
 *    $Author: theurich $
 *    $Date: 2009/07/15 15:34:20 $
 *    Revision: 1.8 $
 ****************************************************************************/


#include <stdio.h>
#include <stdlib.h>

#include "DD.h"


#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif


/*  NOTE: See file, README, for associated documentation. (RTH) */






/*************  Zoltan_DD_Set_Hash_Fn()  ***********************/


int Zoltan_DD_Set_Hash_Fn (
 Zoltan_DD_Directory *dd,              /* directory state information */
 ZOLTAN_HASH_FN *hash)
     {
     char *yo = "Zoltan_DD_Set_Hash_Fn" ;

     /* input sanity checking */
     if (dd == NULL || hash == NULL)
        {
        ZOLTAN_PRINT_ERROR (0, yo, "Invalid input argument") ;
        return ZOLTAN_DD_INPUT_ERROR ;
        }

     dd->hash = hash ;

     if (dd->debug_level > 0)
        ZOLTAN_PRINT_INFO (dd->my_proc, yo, "Successful") ;

     return ZOLTAN_DD_NORMAL_RETURN ;
     }


#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif
