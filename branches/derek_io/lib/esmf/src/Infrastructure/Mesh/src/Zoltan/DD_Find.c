/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * This software is distributed under the GNU Lesser General Public License. *
 * For more info, see the README file in the top-level Zoltan directory.     *
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: DD_Find.c,v $
 *    $Author: dneckels $
 *    $Date: 2007/11/28 16:13:42 $
 *    Revision: 1.14 $
 ****************************************************************************/


#include <stdio.h>
#include <stdlib.h>

#include "DD.h"


#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif


/*  NOTE: See file, README, for associated documentation. (RTH) */



static int DD_Find_Local (Zoltan_DD_Directory *dd, ZOLTAN_ID_PTR gid,
 ZOLTAN_ID_PTR lid, ZOLTAN_ID_PTR user, int *partition, int *owner) ;



/********************  Zoltan_DD_Find()  **************************/

int Zoltan_DD_Find (
 Zoltan_DD_Directory *dd, /* contains directory state information          */
 ZOLTAN_ID_PTR gid,       /* Incoming list of GIDs to get owners proc      */
 ZOLTAN_ID_PTR lid,       /* Outgoing corresponding list of LIDs           */
 ZOLTAN_ID_PTR data,      /* Outgoing optional corresponding user data     */
 int *partition,          /* Outgoing optional partition information       */
 int  count,              /* Count of GIDs in above list (in)              */
 int *owner)              /* Outgoing corresponding list of data locations */
   {
   ZOLTAN_COMM_OBJ *plan  = NULL ;  /* efficient MPI communication     */
   char            *rbuff = NULL ;  /* receive buffer                  */
   char            *sbuff = NULL ;  /* send buffer                     */
   int             *procs = NULL ;  /* list of processors to contact   */
   DD_Find_Msg     *ptr   = NULL ;
   int              i ;
   int              nrec ;          /* number of messages to receive   */
   int              err ;           /* return error condition          */
   int              errcount ;      /* count of GIDs not found         */

   char            *yo = "Zoltan_DD_Find" ;

   if (dd != NULL && dd->debug_level > 1)
      ZOLTAN_TRACE_IN(dd->my_proc, yo, NULL);

   /* input sanity check */
   if (dd == NULL || count < 0 || ((owner == NULL || gid == NULL) && count > 0))
      {
      ZOLTAN_PRINT_ERROR ((dd == NULL ? ZOLTAN_DD_NO_PROC : dd->my_proc),
       yo, "Invalid input argument.") ;
      if (dd != NULL && dd->debug_level > 1)
         ZOLTAN_TRACE_OUT((dd == NULL ? ZOLTAN_DD_NO_PROC : dd->my_proc),
          yo, NULL);
      return ZOLTAN_DD_INPUT_ERROR ;
      }

   /* allocate memory for processors to contact for directory info */
   if (count > 0)
      {
      procs = (int *) ZOLTAN_MALLOC (sizeof (int) * count) ;
      if (procs == NULL)
        {
        ZOLTAN_PRINT_ERROR (dd->my_proc, yo, "Unable to malloc proc list.") ;
        if (dd->debug_level > 1)
           ZOLTAN_TRACE_OUT(dd->my_proc, yo, NULL);
        return ZOLTAN_DD_MEMORY_ERROR ;
        }
      }

   /* allocate memory for DD_Find_Msg send buffer */
   if (count > 0)
      {
      sbuff = (char *) ZOLTAN_MALLOC (dd->find_msg_size * count) ;
      if (sbuff == NULL)
         {
         ZOLTAN_FREE (&procs) ;
         ZOLTAN_PRINT_ERROR (dd->my_proc, yo, "Unable to malloc send buffer.") ;
         if (dd->debug_level > 1)
            ZOLTAN_TRACE_OUT(dd->my_proc, yo, NULL);
         return ZOLTAN_DD_MEMORY_ERROR ;
         }
      }

   if (dd->debug_level > 2)
      ZOLTAN_PRINT_INFO(dd->my_proc, yo, "After mallocs.");

   /* for each GID, fill DD_Find_Msg buffer and contact list */
   for (i = 0 ; i < count ; i++)
      {
      procs[i] = dd->hash (gid + i*dd->gid_length, dd->gid_length, dd->nproc) ;
      ptr      = (DD_Find_Msg *) (sbuff + i * dd->find_msg_size) ;

      ptr->index = i ;
      ptr->proc  = procs[i] ;
      ZOLTAN_SET_ID (dd->gid_length, ptr->id, gid + i*dd->gid_length) ;
      }

   if (dd->debug_level > 2)
      ZOLTAN_PRINT_INFO(dd->my_proc, yo, "After fill.");

   /* create efficient communication plan */
   err = Zoltan_Comm_Create (&plan, count, procs, dd->comm,
    ZOLTAN_DD_FIND_MSG_TAG, &nrec) ;
   if (dd->debug_level > 2)
      ZOLTAN_PRINT_INFO(dd->my_proc, yo, "After Comm_Create.");
   if (err != ZOLTAN_OK)
      goto fini ;

   /* allocate receive buffer */
   if (nrec > 0)
      {
      rbuff = (char *) ZOLTAN_MALLOC (nrec * dd->find_msg_size) ;
      if (rbuff == NULL)
         {
         err = ZOLTAN_DD_MEMORY_ERROR ;
         goto fini ;
         }
      }

   /* send out find messages across entire system */
   err = Zoltan_Comm_Do (plan, ZOLTAN_DD_FIND_MSG_TAG+1, sbuff,
    dd->find_msg_size, rbuff) ;
   if (err != ZOLTAN_OK)
      goto fini ;

   if (dd->debug_level > 2)
      ZOLTAN_PRINT_INFO(dd->my_proc, yo, "After Comm_Do.");

   /* get find messages directed to me, fill in return information */
   errcount = 0 ;
   for (i = 0 ; i < nrec ; i++)
      {
      ptr = (DD_Find_Msg *) (rbuff + i*dd->find_msg_size) ;

      err = DD_Find_Local (dd, ptr->id, ptr->id, ptr->id + dd->max_id_length,
       (partition) ? (&ptr->partition) : NULL, &ptr->proc) ;
      if (err == ZOLTAN_DD_GID_NOT_FOUND_ERROR)
          errcount++ ;
      }

   if (dd->debug_level > 2)
      ZOLTAN_PRINT_INFO(dd->my_proc, yo, "After fill in return info.");

   /* send return information back to requester */
   err = Zoltan_Comm_Do_Reverse(plan, ZOLTAN_DD_FIND_MSG_TAG+2, rbuff,
    dd->find_msg_size, NULL, sbuff) ;
   if (err != ZOLTAN_OK)
      goto fini ;

   if (dd->debug_level > 2)
      ZOLTAN_PRINT_INFO(dd->my_proc, yo, "After Comm_Reverse.");

   /* fill in user supplied lists with returned information */
   for (i = 0 ; i < count ; i++)
      {
      ptr = (DD_Find_Msg *) (sbuff + i*dd->find_msg_size) ;

      owner[ptr->index] = ptr->proc ;

      if (partition != NULL)
         partition[ptr->index] = ptr->partition ;

      if (lid != NULL)
         ZOLTAN_SET_ID (dd->lid_length, lid + ptr->index * dd->lid_length,
           ptr->id) ;

      if (data != NULL)
         ZOLTAN_SET_ID (dd->user_data_length, data + ptr->index
          * dd->user_data_length, ptr->id + dd->max_id_length) ;
      }

   /* if at least one GID was not found, notify caller of error */
   err = (errcount == 0) ? ZOLTAN_DD_NORMAL_RETURN
                         : ZOLTAN_DD_GID_NOT_FOUND_ERROR ;

   if (dd->debug_level > 2)
      ZOLTAN_PRINT_INFO(dd->my_proc, yo, "After fill return lists.");

fini:
   ZOLTAN_FREE (&sbuff) ;
   ZOLTAN_FREE (&rbuff) ;
   ZOLTAN_FREE (&procs) ;
   Zoltan_Comm_Destroy (&plan) ;

   if (err != ZOLTAN_DD_NORMAL_RETURN)
      ZOLTAN_PRINT_WARN (dd->my_proc, yo, "Return is not normal.") ;

   if (dd->debug_level > 0)
      {
      char str[100] ;      /* diagnostic message string */
      sprintf (str, "Processed %d GIDs, GIDs not found: %d", count, errcount) ;
      ZOLTAN_PRINT_INFO (dd->my_proc, yo, str) ;
      }

   if (dd->debug_level > 1)
      ZOLTAN_TRACE_OUT(dd->my_proc, yo, NULL);

   return err ;
   }







/******************  DD_Find_Local()  ***************************/

/* For a given gid, DD_Find_Local() provides its local ID, owner, optional
// user data, and partition from the local distributed directory. An error
// is returned if the gid is not found.
*/

static int DD_Find_Local (Zoltan_DD_Directory *dd,
 ZOLTAN_ID_PTR gid,         /* incoming GID to locate (in)            */
 ZOLTAN_ID_PTR lid,         /* gid's LID (out)                        */
 ZOLTAN_ID_PTR user,        /* gid's user data (out)                  */
 int *partition,        /* gid's partition number (out)           */
 int *owner)            /* gid's owner (processor number) (out)   */
   {
   DD_Node *ptr ;
   int      index ;
   char    *yo = "DD_Find_Local" ;

   /* input sanity check */
   if (dd == NULL || owner == NULL || gid == NULL)
      {
      ZOLTAN_PRINT_ERROR ((dd == NULL) ? 0 : dd->my_proc, yo,
       "Invalid input argument.") ;
      return ZOLTAN_DD_INPUT_ERROR ;
      }
   if (dd->debug_level > 2)
      ZOLTAN_TRACE_IN (dd->my_proc, yo, NULL) ;

   /* compute offset into hash table to find head of linked list */
   index = Zoltan_DD_Hash2 (gid, dd->gid_length, dd->table_length) ;

   /* walk link list until end looking for matching global ID */
   for (ptr = dd->table[index] ; ptr != NULL ; ptr = ptr->next)
      if (ZOLTAN_EQ_ID (dd->gid_length, gid, ptr->gid) == TRUE)
         { 
         /* matching global ID found! Return gid's information */
         if (lid) ZOLTAN_SET_ID(dd->lid_length, lid, 
                                ptr->gid + dd->gid_length);
         if (user) ZOLTAN_SET_ID(dd->user_data_length, user,
                                 ptr->gid + (dd->gid_length + dd->lid_length));

         if (owner     != NULL)   *owner     = ptr->owner ;
         if (partition != NULL)   *partition = ptr->partition ;

         if (dd->debug_level > 2)
            ZOLTAN_TRACE_OUT (dd->my_proc, yo, NULL) ;

         return ZOLTAN_DD_NORMAL_RETURN ;
         }

   if (dd->debug_level > 0) 
      ZOLTAN_PRINT_INFO(dd->my_proc, yo, "GID not found.");
   if (dd->debug_level > 2) 
      ZOLTAN_TRACE_OUT (dd->my_proc, yo, NULL) ;

   return ZOLTAN_DD_GID_NOT_FOUND_ERROR ;
   }

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif
