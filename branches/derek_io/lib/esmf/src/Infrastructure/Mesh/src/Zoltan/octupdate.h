/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: octupdate.h,v $
 *    $Author: dneckels $
 *    $Date: 2007/11/28 16:13:52 $
 *    Revision: 1.16 $
 ****************************************************************************/

#ifndef __OCTUPDATE_H
#define __OCTUPDATE_H

#include "octupdate_const.h"

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif

static void    Zoltan_Oct_get_bounds(ZZ *zz, pRegion *ptr1, int *num_objs, 
		      COORD min, COORD max, int wgtflag, float *c4);
static int     Zoltan_Oct_fix(ZZ *zz, pRegion Region_array, int num_objs);
static int     Zoltan_Oct_global_insert_object(ZZ *, pRegion Region_array, int num_objs);
static pOctant Zoltan_Oct_global_find(OCT_Global_Info *OCT_info,COORD point);
static pOctant Zoltan_Oct_findOctant(OCT_Global_Info *OCT_info,pOctant oct, COORD coord);
static void    Zoltan_Oct_global_dref(ZZ *, OCT_Global_Info *OCT_info);
static int     Zoltan_Oct_subtree_dref(ZZ *, OCT_Global_Info *OCT_info,pOctant oct);
static void    Zoltan_Oct_terminal_coarsen(ZZ *, OCT_Global_Info *OCT_info,pOctant oct);
static void    Zoltan_Oct_set_maxregions(int max);
static void    Zoltan_Oct_set_minregions(int min);
static void    Zoltan_Oct_global_clear(OCT_Global_Info * OCT_info);

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif
#endif
