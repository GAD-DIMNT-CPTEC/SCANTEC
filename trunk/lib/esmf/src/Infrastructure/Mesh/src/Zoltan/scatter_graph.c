/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * For more info, see the README file in the top-level Zoltan directory.     *
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: scatter_graph.c,v $
 *    $Author: dneckels $
 *    $Date: 2007/11/28 16:13:54 $
 *    Revision: 1.18 $
 ****************************************************************************/


#include "zz_const.h"
#include "parmetis_jostle.h"


#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif

/*
 * Scatter a ParMetis-style graph to all processors such that each proc
 * gets an even chunk of vertices and corresponding edges. 
 * The entire graph structure is shuffled around; no attempt is made
 * to keep data local. Processor i receives nodes i*n/p through (i+1)*n/p. 
 * This routine is only useful if the distribution is highly imbalanced!
 *
 * The input graph structure is lost and fresh memory is allocated
 * for the new distributed graph.
 *
 * This routine can also handle geometry data. The graph data may be NULL.
 *
 * The vertex communication plan is returned so it may be used to compute
 * the export lists after partitioning. 
 *
 * Currently the number of weights are inferred from zz->Obj_Weight_Dim
 * and zz->Edge_Weight_Dim. Perhaps these values should be input parameters.
 */

int Zoltan_Scatter_Graph(
  idxtype **vtxdist,
  idxtype **xadj,
  idxtype **adjncy,
  idxtype **vwgt,
  idxtype **vsize,
  idxtype **adjwgt,
  float   **xyz,
  int     ndims,		/* # dimensions of xyz geometry data */
  ZZ      *zz,
  ZOLTAN_COMM_OBJ **plan
)
{
  static char *yo = "Zoltan_Scatter_Graph";
  char     msg[256];
  idxtype *old_vtxdist, *old_xadj, *old_adjncy, *old_vwgt; 
  idxtype *old_vsize, *old_adjwgt;
  float   *old_xyz;
  int *ptr, *proclist = NULL, *proclist2 = NULL;
  int i, j, num_obj, old_num_obj, num_edges, nrecv;
  int use_graph;	/* do we use graph data, or only the geometry? */
  int use_vsize;	/* do we use the vsize array? */
  int vwgt_dim= zz->Obj_Weight_Dim, ewgt_dim= zz->Edge_Weight_Dim;
  ZOLTAN_COMM_OBJ *plan2;

  ZOLTAN_TRACE_ENTER(zz, yo);

  /* Save pointers to "old" data distribution */
  old_vtxdist = old_xadj = old_adjncy = NULL;
  old_vwgt = old_vsize = old_adjwgt = NULL;
  old_xyz = NULL;
  if (vtxdist)
    old_vtxdist = *vtxdist;
  if (xadj)
    old_xadj = *xadj;
  if (adjncy)
    old_adjncy = *adjncy;
  if (vwgt)
    old_vwgt = *vwgt;
  if (vsize)
    old_vsize = *vsize;
  if (adjwgt)
    old_adjwgt = *adjwgt;
  if (xyz)
    old_xyz = *xyz;

  old_num_obj = old_vtxdist[zz->Proc+1] - old_vtxdist[zz->Proc]; 
  if (zz->Debug_Level >= ZOLTAN_DEBUG_ALL) 
    printf("[%1d] Debug: Old number of objects = %d\n", zz->Proc, old_num_obj);

  /* Compute new distribution, *vtxdist */
  (*vtxdist) = (idxtype *)ZOLTAN_MALLOC((zz->Num_Proc+1)* sizeof(idxtype));
  for (i=0; i<=zz->Num_Proc; i++){
    (*vtxdist)[i] = (i*old_vtxdist[zz->Num_Proc])/zz->Num_Proc;
  }

  /* Check if any proc has graph data */
  i = (old_xadj != NULL);
  MPI_Allreduce(&i, &use_graph, 1, MPI_INT, MPI_LOR, zz->Communicator);
  j = (old_vsize != NULL);
  MPI_Allreduce(&j, &use_vsize, 1, MPI_INT, MPI_LOR, zz->Communicator);
  if (zz->Debug_Level >= ZOLTAN_DEBUG_ALL) 
    printf("[%1d] Debug: use_graph = %1d, use_vsize = %1d\n", zz->Proc, 
          use_graph, use_vsize);

  /* Reset all data pointers to NULL for now */
  *xadj = *adjncy = *vwgt = *vsize = *adjwgt = NULL;
  *xyz = NULL;

  /* Convert the xdj array so that it contains the degree of each vertex */
  if (use_graph){
    for (i=0; i<old_num_obj; i++){
      old_xadj[i] = old_xadj[i+1] - old_xadj[i];
    }
  }

  /* Allocate new space for vertex data */
  num_obj = (*vtxdist)[zz->Proc+1] - (*vtxdist)[zz->Proc];
  if (zz->Debug_Level >= ZOLTAN_DEBUG_ALL) 
    printf("[%1d] Debug: New number of objects = %d\n", zz->Proc, num_obj);
  if (use_graph)
    *xadj = (idxtype *) ZOLTAN_MALLOC((num_obj+1)*sizeof(idxtype));
  if (vwgt_dim)
    *vwgt = (idxtype *) ZOLTAN_MALLOC(vwgt_dim*num_obj*sizeof(idxtype));
  if (use_vsize)
    *vsize = (idxtype *) ZOLTAN_MALLOC(num_obj*sizeof(idxtype));
  if (ndims)
    *xyz = (float *) ZOLTAN_MALLOC(ndims*num_obj*sizeof(float));

  if (old_num_obj > 0) {
    /* Set up the communication plan for the vertex data */
    proclist = (int *) ZOLTAN_MALLOC(old_num_obj * sizeof(int));
    /* Let j be the new owner of vertex old_vtxdist[zz->Proc]+i */
    j = 0;
    while (old_vtxdist[zz->Proc] >= (*vtxdist)[j+1]) j++;
    for (i=0; i<old_num_obj; i++){
      if (old_vtxdist[zz->Proc]+i >= (*vtxdist)[j+1]) j++;
      proclist[i] = j;
    }
  }

  Zoltan_Comm_Create(plan, old_num_obj, proclist, zz->Communicator, TAG1, &nrecv);

  if (nrecv != num_obj){
    sprintf(msg,"Proc %d received %d object but expected %d.",
      zz->Proc, nrecv, num_obj);
    ZOLTAN_PRINT_ERROR(zz->Proc, yo, msg);
    /* Free data */
    ZOLTAN_FREE(&proclist);
    ZOLTAN_TRACE_EXIT(zz, yo);
    return ZOLTAN_FATAL;
  }

  /* Do the communication. To save memory, we do not pack all the data into
   * a buffer, but send directly from the old arrays to the new arrays. 
   * We use the vertex communication plan for all the vertex-based arrays
   * and the edge communication plan for all the edge-based arrays.
   */

  if (zz->Debug_Level >= ZOLTAN_DEBUG_ALL) 
    printf("[%1d] Debug: Starting vertex-based communication.\n", zz->Proc);

  if (use_graph){
    Zoltan_Comm_Do( *plan, TAG2, (char *) old_xadj, sizeof(idxtype), (char *) *xadj);
  }
  if (vwgt_dim){
    Zoltan_Comm_Do( *plan, TAG3, (char *) old_vwgt, vwgt_dim*sizeof(idxtype), (char *) *vwgt);
  }
  if (use_vsize){
    Zoltan_Comm_Do( *plan, TAG4, (char *) old_vsize, sizeof(idxtype), (char *) *vsize);
  }
  if (ndims){
    Zoltan_Comm_Do( *plan, TAG5, (char *) old_xyz, ndims*sizeof(idxtype), (char *) *xyz);
  }
  if (zz->Debug_Level >= ZOLTAN_DEBUG_ALL) 
    printf("[%1d] Debug: Finished vertex-based communication.\n", zz->Proc);

  if (use_graph){

    /* Rebuild xadj from degrees */
    for (i=1; i<num_obj; i++)
      (*xadj)[i] += (*xadj)[i-1];
    for (i=num_obj; i>0; i--)
      (*xadj)[i] = (*xadj)[i-1];
    (*xadj)[0] = 0;
  
    /* Allocate space for new edge data structures */
    num_edges = (*xadj)[num_obj];
    *adjncy = (idxtype *) ZOLTAN_MALLOC(num_edges*sizeof(idxtype));
  
    if (ewgt_dim)
      *adjwgt = (idxtype *) ZOLTAN_MALLOC(ewgt_dim*num_edges*sizeof(idxtype));
  
    /* Set up the communication plan for the edge data. */
    ptr = proclist2 = (int *) ZOLTAN_MALLOC(old_xadj[old_num_obj] * sizeof(int));
    for (i=0; i<old_num_obj; i++)
      for (j=0; j<old_xadj[i]; j++)
        *ptr++ = proclist[i];
    if (zz->Debug_Level >= ZOLTAN_DEBUG_ALL) {
      printf("[%1d] Debug: Allocated proclist of length %d for edges.\n", 
             zz->Proc, old_xadj[old_num_obj]);
    }

    Zoltan_Comm_Create(&plan2, old_xadj[old_num_obj], proclist2, zz->Communicator, 
                   TAG1, &nrecv);
  
    if (nrecv != num_edges){
      sprintf(msg,"Proc %d received %d edges but expected %d.",
        zz->Proc, nrecv, num_edges);
      ZOLTAN_PRINT_ERROR(zz->Proc, yo, msg);
      /* Free data */
      ZOLTAN_FREE(&proclist);
      ZOLTAN_FREE(&proclist2);
      ZOLTAN_FREE(&old_vtxdist);
      ZOLTAN_FREE(&old_xadj);
      ZOLTAN_FREE(&old_adjncy);
      ZOLTAN_FREE(&old_vwgt);
      ZOLTAN_FREE(&old_vsize);
      ZOLTAN_FREE(&old_adjwgt);
      ZOLTAN_FREE(&old_xyz);
      ZOLTAN_TRACE_EXIT(zz, yo);
      return ZOLTAN_FATAL;
    }
  
    if (zz->Debug_Level >= ZOLTAN_DEBUG_ALL) 
      printf("[%1d] Debug: Starting edge-based communication.\n", zz->Proc);
  
    /* Do the communication. */
    Zoltan_Comm_Do( plan2, TAG2, (char *) old_adjncy, sizeof(idxtype), (char *) *adjncy);
    if (ewgt_dim){
      Zoltan_Comm_Do( plan2, TAG3, (char *) old_adjwgt, ewgt_dim*sizeof(idxtype), (char *) *adjwgt);
    }
  
    if (zz->Debug_Level >= ZOLTAN_DEBUG_ALL) 
      printf("[%1d] Debug: Finished edge-based communication.\n", zz->Proc);
  
    /* Free the comm. plan for edge data */
    Zoltan_Comm_Destroy(&plan2);

  } /* end of use_graph */

  /* Free data structures */
  ZOLTAN_FREE(&proclist);
  ZOLTAN_FREE(&proclist2);
  ZOLTAN_FREE(&old_vtxdist);
  ZOLTAN_FREE(&old_xadj);
  ZOLTAN_FREE(&old_adjncy);
  ZOLTAN_FREE(&old_vwgt);
  ZOLTAN_FREE(&old_vsize);
  ZOLTAN_FREE(&old_adjwgt);
  ZOLTAN_FREE(&old_xyz);

  ZOLTAN_TRACE_EXIT(zz, yo);
  return ZOLTAN_OK;
}

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif
