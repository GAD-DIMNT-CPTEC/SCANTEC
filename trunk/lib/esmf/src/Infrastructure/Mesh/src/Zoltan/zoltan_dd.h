/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * This software is distributed under the GNU Lesser General Public License. *
 * For more info, see the README file in the top-level Zoltan directory.     *
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: zoltan_dd.h,v $
 *    $Author: theurich $
 *    $Date: 2009/07/15 15:34:21 $
 *    Revision: 1.6 $
 ****************************************************************************/

#ifndef ZOLTAN_DD_DDIRECTORY_H
#define ZOLTAN_DD_DDIRECTORY_H

#include "zoltan_types.h"
#include <mpi.h>

/*
** Must define this function prototype before #ifdef __cplusplus
** to avoid warning when compiling with C++ on solaris
*/
/* typedef unsigned int ZOLTAN_HASH_FN(ZOLTAN_ID_PTR, int, unsigned int);
*/
    
#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif

typedef unsigned int ZOLTAN_HASH_FN(ZOLTAN_ID_PTR, int, unsigned int);

struct Zoltan_DD_Struct;
typedef struct Zoltan_DD_Struct Zoltan_DD_Directory;


/* The following are used as return value error codes */
#define ZOLTAN_DD_NORMAL_RETURN         ZOLTAN_OK
#define ZOLTAN_DD_INPUT_ERROR           ZOLTAN_WARN
#define ZOLTAN_DD_MEMORY_ERROR          ZOLTAN_MEMERR
#define ZOLTAN_DD_MPI_ERROR             ZOLTAN_FATAL
#define ZOLTAN_DD_COMM_ERROR            ZOLTAN_FATAL
#define ZOLTAN_DD_GID_ADDED             ZOLTAN_WARN
#define ZOLTAN_DD_GID_NOT_FOUND_ERROR   ZOLTAN_WARN
#define ZOLTAN_DD_GID_REDEFINED_ERROR   ZOLTAN_FATAL


/***********  Distributed Directory Function Prototypes ************/

int Zoltan_DD_Create (Zoltan_DD_Directory **dd, MPI_Comm comm, int num_gid,
 int num_lid, int user_length,  int table_length, int debug_level) ;

int Zoltan_DD_Copy_To(Zoltan_DD_Directory **toptr, Zoltan_DD_Directory *from);
Zoltan_DD_Directory *Zoltan_DD_Copy(Zoltan_DD_Directory *from);

void Zoltan_DD_Destroy (Zoltan_DD_Directory **dd) ;

int Zoltan_DD_Update (Zoltan_DD_Directory *dd, ZOLTAN_ID_PTR gid,
 ZOLTAN_ID_PTR lid, ZOLTAN_ID_PTR user, int *partition, int count) ;

int Zoltan_DD_Find (Zoltan_DD_Directory *dd, ZOLTAN_ID_PTR gid,
 ZOLTAN_ID_PTR lid, ZOLTAN_ID_PTR data, int *partition, int count,
 int *owner) ;

int Zoltan_DD_Remove (Zoltan_DD_Directory *dd, ZOLTAN_ID_PTR gid,
 int count) ;

int Zoltan_DD_Set_Hash_Fn (Zoltan_DD_Directory *dd, ZOLTAN_HASH_FN *hash);

void Zoltan_DD_Stats (Zoltan_DD_Directory *dd) ;

int Zoltan_DD_Set_Neighbor_Hash_Fn1 (Zoltan_DD_Directory *dd, int size) ;

int Zoltan_DD_Set_Neighbor_Hash_Fn2 (Zoltan_DD_Directory *dd, int *proc,
 int *low, int *high, int count) ;

int Zoltan_DD_Set_Neighbor_Hash_Fn3 (Zoltan_DD_Directory *dd, int total) ;

int Zoltan_DD_Print (Zoltan_DD_Directory *dd) ;

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif
