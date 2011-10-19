/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: dfs_const.h,v $
 *    $Author: dneckels $
 *    $Date: 2007/11/28 16:13:47 $
 *    Revision: 1.12 $
 ****************************************************************************/

#ifndef __DFS_CONST_H
#define __DFS_CONST_H

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif


extern void Zoltan_Oct_dfs_partition(ZZ *zz, int *counter, float *c1,
				     float *part_sizes);
extern void Zoltan_Oct_dfs_migrate(ZZ *zz, int *nsentags,
			   pRegion *import_tags, int *nrectags, 
			   float *c2, float *c3, int *counter3, int *counter4);


#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif
