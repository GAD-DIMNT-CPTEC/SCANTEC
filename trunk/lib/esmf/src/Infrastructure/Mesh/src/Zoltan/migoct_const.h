/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: migoct_const.h,v $
 *    $Author: dneckels $
 *    $Date: 2007/11/28 16:13:51 $
 *    Revision: 1.17 $
 ****************************************************************************/

#ifndef __MIGOCT_CONST_H
#define __MIGOCT_CONST_H

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif

extern int Zoltan_Oct_migrate_octants(ZZ *zz, int *newpids, pOctant *newocts, int nocts, int *nrecocts);

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif /* __MIGOCT_CONST_H */
