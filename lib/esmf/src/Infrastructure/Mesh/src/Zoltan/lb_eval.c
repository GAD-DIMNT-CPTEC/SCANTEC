/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * For more info, see the README file in the top-level Zoltan directory.     *  
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: lb_eval.c,v $
 *    $Author: dneckels $
 *    $Date: 2007/11/28 16:13:51 $
 *    Revision: 1.61.2.8 $
 ****************************************************************************/

#include "zz_const.h"
#include "zz_util_const.h"
#include "phg.h"
#include <limits.h>
#include <float.h>

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif

#include "parmetis_jostle.h"
#ifndef FLT_MAX /* just in case it's not defined */
#define FLT_MAX (1e38) /* some large number */
#endif

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
/*
 *  This file contains routines for evaluating the quality of the
 *  current partitioning/balance.
 *  These functions are all callable by the application. 
 */
/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

#define NUM_STATS 5 /* Number of graph stats other than vertex/edge weights */
#define NUM_STATS_PART 3 /* Number of graph stats for partitions. */

static int eval_edge_list(ZZ *, int, int, ZOLTAN_ID_PTR, int *, ZOLTAN_ID_PTR,
  int *, int *, float *, int, int, int, int *, int *, int *, int *, float *,
  int *, float *, int *, int *);

static int get_nbor_parts(ZZ *, int, ZOLTAN_ID_PTR, ZOLTAN_ID_PTR,
  int *, int, ZOLTAN_ID_PTR, int *);


/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

int Zoltan_LB_Eval (ZZ *zz, int print_stats, 
     int *nobj, float *obj_wgt, 
     int *ncuts, float *cut_wgt, 
     int *nboundary, int *nadj)
/* 
 * Input:
 *   zz          - pointer to Zoltan structure
 *   print_stats - if > 0, compute and print max, min and sum of the metrics
 *                 if == 0, stay silent but compute output arguments
 *
 * Output:
 *   nobj      - number of objects (local for this proc)
 *   obj_wgt   - obj_wgt[0:num_vertex_weights-1] are the object weights
 *               (local for this proc)
 *   ncuts     - number of cuts (local for this proc)
 *   cut_wgt   - cut_wgt[0:num_vertex_weights-1] are the cut weights (local for this proc)
 *   nboundary - number of boundary objects (local for this proc)
 *   nadj      - the number of adjacent procs (local for this proc)
 *
 * Output parameters will only be returned if they are 
 * not NULL on entry.
 *
 * EBEB: We should change the function interface to return min/max/sum stats. 
 * EBEB: These data can be computed both w.r.t. processors and partitions.
 * EBEB: Not all partition stats have been implemented yet.
 *
 */

{
  char *yo = "Zoltan_LB_Eval";
  int i, j, k, max_edges, num_edges;
  int cuts, comm_vol;
  int num_obj = 0, num_adj, num_boundary, ierr, compute_part;
  int nproc = zz->Num_Proc; 
  int nparts, nonempty_nparts, max_nparts, req_nparts, obj_wgt_dim;
  int stats[4*NUM_STATS];
  int imin, imax, isum, iimbal;
  int *proc_count=NULL, *nbors_proc=NULL, *nbors_part = NULL;
  float imbal[NUM_STATS];
  float *tmp_vwgt=NULL, *vwgts=NULL, *ewgts=NULL, *tmp_cutwgt=NULL; 
  float *part_sizes, temp;
  int *edges_per_obj=NULL;
  ZOLTAN_ID_PTR local_ids=NULL; 
  ZOLTAN_ID_PTR global_ids=NULL, nbors_global=NULL;
  int *part=NULL;
  ZOLTAN_ID_PTR lid;   /* Temporary pointer to a local id; used to pass NULL
                      pointers to query functions when NUM_LID_ENTRIES = 0. */
  int num_gid_entries = zz->Num_GID;
  int num_lid_entries = zz->Num_LID;
  int gid_off, lid_off;
  int have_graph_callbacks;
  int have_pin_callbacks;
  int sum;
  char msg[256];
  /* Arrays for partition data. */
  int *max_arr, *nobj_arr, *cut_arr, *bndry_arr; 
  int *commvol_arr, *all_arr, *all_arr_glob;
  float *vwgt_arr, *vwgt_arr_glob, *cutwgt_arr, *cutwgt_arr_glob;

  int vwgt_dim;
  double hgraph_global_sum[2];
  double hgraph_local_stats[2];
  ZHG* zhg;
  
  ZOLTAN_TRACE_ENTER(zz, yo);

  /* Set default error code */
  ierr = ZOLTAN_OK;

  /* Set all pointers to NULL */
  global_ids = NULL;
  local_ids = NULL;
  part = NULL;
  tmp_vwgt = tmp_cutwgt = NULL;
  vwgts = ewgts = NULL;
  nbors_global = NULL;
  nbors_proc = proc_count = nbors_part = NULL;
  vwgt_arr = vwgt_arr_glob = cutwgt_arr = cutwgt_arr_glob = NULL;
  max_arr = nobj_arr = cut_arr = bndry_arr = all_arr = all_arr_glob = NULL;
  edges_per_obj = NULL;
  part_sizes = NULL;

  /* Get partition information. We need LB.Num_Global_Parts */
  /* We don't know if the application uses a Zoltan-style
     mapping of partitions to processsors, so avoid Part2Proc! */
  ierr = Zoltan_LB_Build_PartDist(zz);
  if (ierr == ZOLTAN_FATAL){
    ZOLTAN_PRINT_ERROR(zz->Proc, yo, "Zoltan_LB_Build_PartDist returned error");
    goto End;
  }

  /* Get:
   *   Requested number of partitions
   *
   *   Actual number of partitions 
   *     -may be more or less if partitioning has not yet occured, 
   *      because each process will report that it's objects are 
   *      in a partition equal to it's rank.
   *     -may be less than requested if partitioning yielded empty
   *      partitions
   *
   *   Maximum possible number of partitions
   */

  req_nparts = zz->LB.Num_Global_Parts;     /* requested */
  nparts = 0;                               /* actual    */

  /* Have graph or hypergraph query functions? */
  have_graph_callbacks = 
       ((zz->Get_Num_Edges != NULL || zz->Get_Num_Edges_Multi != NULL) &&
        (zz->Get_Edge_List != NULL || zz->Get_Edge_List_Multi != NULL));
  have_pin_callbacks = 
        zz->Get_HG_Size_CS != NULL && zz->Get_HG_CS != NULL ;

  if (have_pin_callbacks){
    /*
     * We need to call Zoltan_PHG_Fill_Hypergraph, which will call 
     * the query functions for us, and process the results.
     *
     * Calling Fill_Hypergraph with a null parameter structure 
     * and null partition array is a flag to it that it is
     * being called from Zoltan_LB_Eval.  Fill_Hypergraph in this case
     * sets the parameters so that all edges are written to the "removed" 
     * lists.  This permits us to use Zoltan_PHG_Removed_Cuts() to get 
     * cut statistics.
     *
     * Fill_Hypergraph will also write the object (vertex) weights to 
     * zhg->HG.vwgts (which normally holds pin weights instead).  
     * Number of vertex weights may exceed zz->Obj_Weight_Dim if
     * the ADD_OBJ_WEIGHT parameter is set.
    */

    zhg = (ZHG*) ZOLTAN_CALLOC(1, sizeof(ZHG));
    if (zhg == NULL) {
      ZOLTAN_PRINT_ERROR(zz->Proc, yo, "Memory error.");
      ierr = ZOLTAN_MEMERR;
      goto End;
    }
  
    ierr = Zoltan_PHG_Fill_Hypergraph(zz, zhg, 
             NULL,   /* no parameters */
             NULL);  /* initial partition assignment irrelevant */

    if ((ierr != ZOLTAN_OK) && (ierr != ZOLTAN_WARN)){
      Zoltan_PHG_Free_Hypergraph_Data(zhg);
      ZOLTAN_FREE(&zhg);
      ZOLTAN_TRACE_EXIT(zz, yo);
      return ierr;
    }

    if (zhg->nObj){
      zhg->Output_Parts = (int *)ZOLTAN_MALLOC(sizeof(int) * zhg->nObj);
      memcpy(zhg->Output_Parts, zhg->Input_Parts, sizeof(int) * zhg->nObj);
    }

    /* Perform cut calculations and find global max, min and sum */
    ierr = Zoltan_PHG_Removed_Cuts(zz, zhg, hgraph_local_stats);

    if (ierr != ZOLTAN_OK && ierr != ZOLTAN_WARN){
      ZOLTAN_PRINT_ERROR(zz->Proc, yo, "Zoltan_PHG_Removed_Cuts failed!");
      Zoltan_PHG_Free_Hypergraph_Data(zhg);
      ZOLTAN_FREE(&zhg);
      ZOLTAN_FREE(&zhg->Output_Parts);
      return ZOLTAN_FATAL;
    }
            
    MPI_Allreduce(hgraph_local_stats, hgraph_global_sum, 2,
                  MPI_DOUBLE, MPI_SUM, zz->Communicator);

    vwgt_dim = zhg->HG.VtxWeightDim;
    num_obj = zhg->nObj;
    global_ids = zhg->GIDs;
    local_ids = zhg->LIDs;
    vwgts = zhg->HG.vwgt;
    part = zhg->Input_Parts;
  }
  else {
  
    ierr = Zoltan_Get_Obj_List(zz, &num_obj, &global_ids, &local_ids, 
                               zz->Obj_Weight_Dim, &vwgts, &part);
  
    if (ierr == ZOLTAN_FATAL) {
      ZOLTAN_TRACE_EXIT(zz, yo);
      return ierr;
    }

    vwgt_dim = zz->Obj_Weight_Dim;
  }

  /* Compute statistics w.r.t. partitions? */
  compute_part = (zz->Get_Partition != NULL || zz->Get_Partition_Multi != NULL);

  if (compute_part){
    int tmp_max_nparts;

    /* Compute max_nparts -- 
     * max of nproc, input partitions and requested partitions 
     */
    tmp_max_nparts = MAX(nproc, req_nparts); 
    for (i = 0; i < num_obj; i++)
      if (part[i] >= tmp_max_nparts) tmp_max_nparts = part[i]+1;
    MPI_Allreduce(&tmp_max_nparts, &max_nparts, 1, MPI_INT, MPI_MAX,
                  zz->Communicator);

    nobj_arr = (int *)  ZOLTAN_CALLOC(max_nparts, sizeof(int));
    max_arr = (int *)  ZOLTAN_MALLOC(max_nparts * sizeof(int));

    if (max_nparts && (!nobj_arr || !max_arr)){
      ZOLTAN_FREE(&nobj_arr);
      ZOLTAN_FREE(&max_arr);
      ierr = ZOLTAN_MEMERR;
      goto End;
    }

    /* Count the local number of objects in each partition, get the
     * global number of partitions, and get the global number of 
     * non-empty partitions. 
     */

    for (i=0; i<num_obj; i++){
      nobj_arr[part[i]]++;
    }

    MPI_Allreduce(nobj_arr, max_arr, max_nparts, MPI_INT, MPI_MAX,
                  zz->Communicator);

    nonempty_nparts = 0;
    tmp_max_nparts = -1;
    for (i=0; i<max_nparts; i++){
      if (max_arr[i] > 0){
        nonempty_nparts++;
        if (i > tmp_max_nparts) tmp_max_nparts = i;
      }
    }

    nparts = tmp_max_nparts + 1;

    ZOLTAN_FREE(&max_arr);

    /* Allocate space. */
    all_arr = (int *)  ZOLTAN_CALLOC(2*NUM_STATS*nparts, sizeof(int));
    vwgt_arr = (float *) ZOLTAN_CALLOC(2*nparts*(vwgt_dim +
                           zz->Edge_Weight_Dim), sizeof(float));
    if (!all_arr || (vwgt_dim + zz->Edge_Weight_Dim && !vwgt_arr)){
      ierr = ZOLTAN_MEMERR;
      goto End;
    }
    memcpy(all_arr, nobj_arr, nparts * sizeof(int));
    ZOLTAN_FREE(&nobj_arr);

    cut_arr  = all_arr + nparts;
    bndry_arr  = cut_arr + nparts;
    commvol_arr  = bndry_arr + nparts;
    all_arr_glob = all_arr + NUM_STATS*nparts;
    vwgt_arr_glob = vwgt_arr + nparts * vwgt_dim;
    cutwgt_arr = vwgt_arr_glob + nparts * vwgt_dim;
    cutwgt_arr_glob = cutwgt_arr + nparts*(zz->Edge_Weight_Dim);

  }

  /* Compute object weight sums */
  if (vwgt_dim>0){
    tmp_vwgt = (float *) ZOLTAN_CALLOC(4*vwgt_dim,  sizeof(float));
    if (!tmp_vwgt){
      ierr = ZOLTAN_MEMERR;
      goto End;
    }
    for (i=0; i<num_obj; i++){
      for (j=0; j<vwgt_dim; j++){
        tmp_vwgt[j] += vwgts[i*vwgt_dim+j];
        if (compute_part)
          vwgt_arr[part[i]*vwgt_dim+j] += vwgts[i*vwgt_dim+j];
      }
    }
  }


  /* Compute (weighted) edge cuts, #boundary vertices,
     and # adjacent procs if possible */

  num_adj = 0;
  cuts = 0;
  num_boundary = 0;
  comm_vol = 0;

  if (have_graph_callbacks) {
    /* Use the basic graph query functions */

    /* First compute max no. of edges so we can allocate the right
       amount of space */

    Zoltan_Get_Num_Edges_Per_Obj(zz, num_obj, global_ids, local_ids, 
                                 &edges_per_obj, &max_edges, &num_edges);

    /* Allocate edge list space */
    nbors_global = ZOLTAN_MALLOC_GID_ARRAY(zz, num_edges);
    nbors_proc = (int *)ZOLTAN_MALLOC(num_edges * sizeof(int));
    if (compute_part) 
      nbors_part = (int *)ZOLTAN_MALLOC(num_edges * sizeof(int));
    else
      nbors_part = nbors_proc;
    /* Allocate a proc list for computing nadjacent */
    proc_count = (int *)ZOLTAN_MALLOC((zz->Num_Proc)* sizeof(int));
    /* Allocate space for edge weights if needed */
    if (zz->Edge_Weight_Dim){
      ewgts = (float *)ZOLTAN_MALLOC((zz->Edge_Weight_Dim) * num_edges 
                                                           * sizeof(float));
      tmp_cutwgt = (float *) ZOLTAN_MALLOC(4 * (zz->Edge_Weight_Dim) 
                                             * sizeof(float));
    }

    if ((num_edges &&
       (!nbors_global || !nbors_proc || !nbors_part || (zz->Edge_Weight_Dim && !ewgts))) || 
        (zz->Edge_Weight_Dim && !tmp_cutwgt) || !proc_count){
      ierr = ZOLTAN_MEMERR;
      goto End;
    }

    for (i=0; i<zz->Num_Proc; i++)
      proc_count[i] = -1;
    for (i=0; i<zz->Edge_Weight_Dim; i++)
      tmp_cutwgt[i] = 0;

    if (zz->Get_Edge_List_Multi) {
      zz->Get_Edge_List_Multi(zz->Get_Edge_List_Multi_Data, 
                              num_gid_entries, num_lid_entries, num_obj,
                              global_ids, local_ids, edges_per_obj,
                              nbors_global, nbors_proc,
                              zz->Edge_Weight_Dim, ewgts, &ierr);
      if ((ierr != ZOLTAN_OK) && (ierr != ZOLTAN_WARN)){
        goto End;
      }
    }
    else {
      sum = 0;
      for (k=0; k<num_obj; k++){
        gid_off = k * num_gid_entries;
        lid_off = k * num_lid_entries;
        lid = (num_lid_entries > 0 ? &(local_ids[lid_off]) : NULL);
        if ((ierr != ZOLTAN_OK) && (ierr != ZOLTAN_WARN)){
          goto End;
        }
        zz->Get_Edge_List(zz->Get_Edge_List_Data, 
                          num_gid_entries, num_lid_entries,
                          &(global_ids[gid_off]), lid,
                          &(nbors_global[sum*num_gid_entries]),
                          &(nbors_proc[sum]), 
                          zz->Edge_Weight_Dim,
                          &(ewgts[sum*zz->Edge_Weight_Dim]), &ierr);
        if ((ierr != ZOLTAN_OK) && (ierr != ZOLTAN_WARN)){
          goto End;
        }
        sum += edges_per_obj[k];
      }
    }

    if (compute_part) {
      /* Compute the partition number for neighboring objects */
      get_nbor_parts(zz, num_obj, global_ids, local_ids, part,
                     num_edges, nbors_global, nbors_part);
    }

    sum = 0;
    for (k = 0; k < num_obj; k++) {
      ierr = eval_edge_list(zz, k, num_gid_entries, global_ids, part, 
                            &(nbors_global[sum]), &(nbors_proc[sum]), 
                            &(nbors_part[sum]),
                            &(ewgts[sum*zz->Edge_Weight_Dim]), 
                            edges_per_obj[k],
                            num_obj, compute_part, proc_count,
                            &cuts, &num_boundary, &comm_vol,
                            tmp_cutwgt, cut_arr, cutwgt_arr, 
                            bndry_arr, commvol_arr);
      if (ierr) {
        ZOLTAN_PRINT_ERROR(zz->Proc, yo, "Error in eval_edge_list");
        goto End;
      }
      sum += edges_per_obj[k];
    }
    /* Compute the number of adjacent procs */
    for (j=0; j<zz->Num_Proc; j++)
      if (proc_count[j]>=0) num_adj++;
      
  }
  
  if (print_stats){
    imin = 1;
    imax = 2;
    isum = 3;

    /* Global reduction for object weights. */
    if (vwgt_dim>0){
      MPI_Allreduce(tmp_vwgt, &tmp_vwgt[imin*vwgt_dim], 
                    vwgt_dim, MPI_FLOAT, MPI_MIN, zz->Communicator);
      MPI_Allreduce(tmp_vwgt, &tmp_vwgt[imax*vwgt_dim], 
                    vwgt_dim, MPI_FLOAT, MPI_MAX, zz->Communicator);
      MPI_Allreduce(tmp_vwgt, &tmp_vwgt[isum*vwgt_dim], 
                    vwgt_dim, MPI_FLOAT, MPI_SUM, zz->Communicator);
    }
    /* Global reduction for comm weights. */
    if (zz->Edge_Weight_Dim>0 && have_graph_callbacks) {
      MPI_Allreduce(tmp_cutwgt, &tmp_cutwgt[imin*(zz->Edge_Weight_Dim)], 
                    zz->Edge_Weight_Dim, MPI_FLOAT, MPI_MIN, zz->Communicator);
      MPI_Allreduce(tmp_cutwgt, &tmp_cutwgt[imax*zz->Edge_Weight_Dim], 
                    zz->Edge_Weight_Dim, MPI_FLOAT, MPI_MAX, zz->Communicator);
      MPI_Allreduce(tmp_cutwgt, &tmp_cutwgt[isum*(zz->Edge_Weight_Dim)], 
                    zz->Edge_Weight_Dim, MPI_FLOAT, MPI_SUM, zz->Communicator);
    }
     
    stats[0] = num_obj;
    stats[1] = cuts;
    stats[2] = num_boundary;
    stats[3] = comm_vol;
    stats[4] = num_adj;

    /* Compute min, max, sum in the upper portions of the stats array. */
    MPI_Allreduce(stats, &stats[imin*NUM_STATS], NUM_STATS, MPI_INT, MPI_MIN, 
                  zz->Communicator);
    MPI_Allreduce(stats, &stats[imax*NUM_STATS], NUM_STATS, MPI_INT, MPI_MAX, 
                  zz->Communicator);
    MPI_Allreduce(stats, &stats[isum*NUM_STATS], NUM_STATS, MPI_INT, MPI_SUM, 
                  zz->Communicator);

    /* Print min-max-sum of results */
    if (zz->Proc == zz->Debug_Proc){
      printf("\n%s  Statistics with respect to %1d processors:\n", yo, nproc);
      printf("%s                         Min.     Max.      Sum  Imbalance\n", yo);
      printf("%s  No. of objects   :  %8d %8d %8d   %5.3f\n",
        yo, stats[imin*NUM_STATS], 
        stats[imax*NUM_STATS],
        stats[isum*NUM_STATS], 
        (stats[isum*NUM_STATS] > 0
              ? stats[imax*NUM_STATS]*nproc/ (float) stats[isum*NUM_STATS]
              : 1.));
 
      for (i=0; i<vwgt_dim; i++){
        printf("%s  Object weight #%1d :  %8.3g %8.3g %8.3g   %5.3f\n",
          yo, i, tmp_vwgt[imin*vwgt_dim+i], 
          tmp_vwgt[imax*vwgt_dim+i], 
          tmp_vwgt[isum*vwgt_dim+i], 
          (tmp_vwgt[isum*vwgt_dim+i] > 0 
           ? tmp_vwgt[imax*vwgt_dim+i]*nproc/tmp_vwgt[isum*vwgt_dim+i]
           : 1.));
      }

      if (have_graph_callbacks) {
        for (i=0; i<zz->Edge_Weight_Dim; i++){
          printf("%s  Cut weight  #%1d   :  %8.3g %8.3g %8.3g   %5.3f\n",
            yo, i, tmp_cutwgt[imin*(zz->Edge_Weight_Dim)+i], 
            tmp_cutwgt[imax*(zz->Edge_Weight_Dim)+i], 
            tmp_cutwgt[isum*(zz->Edge_Weight_Dim)+i], 
            (tmp_cutwgt[isum*(zz->Edge_Weight_Dim)+i] > 0 
             ? tmp_cutwgt[imax*(zz->Edge_Weight_Dim)+i]*nproc/tmp_cutwgt[isum*(zz->Edge_Weight_Dim)+i]
             : 1.));
        }

        for (i=1; i<NUM_STATS; i++){
          switch(i){
          case 1:  sprintf(msg, "%s", "No. of edge cuts : ");
                   break;
          case 2:  sprintf(msg, "%s", "Boundary objects : ");
                   break;
          case 3:  sprintf(msg, "%s", "Comm. volume     : ");
                   break;
          case 4:  sprintf(msg, "%s", "Adjacent procs   : ");
                   break;
          default: sprintf(msg, "%s", "                   ");
                   break;
          }

          printf("%s  %s %8d %8d %8d   %5.3f\n",
            yo, msg, stats[imin*NUM_STATS+i], 
            stats[imax*NUM_STATS+i],
            stats[isum*NUM_STATS+i], 
            (stats[isum*NUM_STATS+i] > 0 
                  ? stats[imax*NUM_STATS+i]*nproc/ (float) stats[isum*NUM_STATS+i]
                  : 1.));
        }
      }
      printf("\n");
      
    }
  }

  /* Copy results to output parameters if desired */
  if (nobj) *nobj = num_obj;
  if (ncuts) *ncuts = cuts;
  if (nadj) *nadj = num_adj;
  if (nboundary) *nboundary = num_boundary;
  if (obj_wgt){
    for (i=0; i<vwgt_dim; i++) 
      obj_wgt[i] = tmp_vwgt[i];
  }
  if (cut_wgt && have_graph_callbacks){
    for (i=0; i<zz->Edge_Weight_Dim; i++) 
      cut_wgt[i] = tmp_cutwgt[i];
  }


  if (compute_part && print_stats){
    /* Compute statistics w.r.t. partitions. */
    imin = 0;
    imax = 1;
    isum = 2;
    iimbal = 3;

    /* Get partition sizes. */
    obj_wgt_dim = (vwgt_dim > 0 ? vwgt_dim : 1);
    part_sizes = (float *) ZOLTAN_MALLOC(nparts * obj_wgt_dim * sizeof(float));
    if (!part_sizes){
      ierr = ZOLTAN_MEMERR;
      goto End;
    }
    Zoltan_LB_Get_Part_Sizes(zz, nparts, obj_wgt_dim, part_sizes);

    /* Allreduce data w.r.t. partitions onto all procs. */
    MPI_Allreduce(all_arr, all_arr_glob, NUM_STATS_PART*nparts, 
                  MPI_INT, MPI_SUM, zz->Communicator);
    if (vwgt_dim > 0)
      MPI_Allreduce(vwgt_arr, vwgt_arr_glob, nparts*(vwgt_dim), 
                    MPI_FLOAT, MPI_SUM, zz->Communicator);
    if (zz->Edge_Weight_Dim > 0)
      MPI_Allreduce(cutwgt_arr, cutwgt_arr_glob, nparts*(zz->Edge_Weight_Dim), 
                    MPI_FLOAT, MPI_SUM, zz->Communicator);

    /* Find min, max, sum. */
    for (i=0; i<NUM_STATS; i++)
      stats[i] = INT_MAX; /* set min to very large number */
    for (i=NUM_STATS; i<3*NUM_STATS; i++)
      stats[i] = 0;       /* max and sum */
 
    for (i=0; i<nparts; i++){
      for (j=0; j<NUM_STATS_PART; j++){
        if (all_arr_glob[j*nparts+i] < stats[j])           /* min */
          stats[j] = all_arr_glob[j*nparts+i];
        if (all_arr_glob[j*nparts+i] > stats[imax*NUM_STATS+j]) /* max */
          stats[imax*NUM_STATS+j] = all_arr_glob[j*nparts+i];
        stats[isum*NUM_STATS+j] += all_arr_glob[j*nparts+i];  /* sum */
      }
    }

    /* Compute scaled imbalance. */
    for (j=0; j<NUM_STATS_PART; j++)
      imbal[j] = 0.0;
    k = (vwgt_dim>0 ? vwgt_dim : 1); 
    for (i=0; i<nparts; i++){
      for (j=0; j<NUM_STATS_PART; j++){
        if (all_arr_glob[j*nparts+i]*part_sizes[i*k] != 0.0)
          temp = all_arr_glob[j*nparts+i] / (stats[isum*NUM_STATS+j]
                 * part_sizes[i*k]);
        else
          temp = 1.0;
        if (temp > imbal[j])
          imbal[j] = temp;
      }
    }

    /* Min, max, sum for object weights. */
    if (vwgt_dim>0){
      for (i=0; i<vwgt_dim; i++)
        tmp_vwgt[i] = FLT_MAX; /*min */
      for (i=vwgt_dim; i<4*vwgt_dim; i++)
        tmp_vwgt[i] = 0.0; /* max and sum and imbalance */

      for (j=0; j<nparts; j++){
        for (i=0; i<vwgt_dim; i++){
          if (vwgt_arr_glob[j*vwgt_dim+i] < tmp_vwgt[i]) 
            tmp_vwgt[i] = vwgt_arr_glob[j*vwgt_dim+i];
          if (vwgt_arr_glob[j*vwgt_dim+i] > tmp_vwgt[imax*vwgt_dim+i]) 
            tmp_vwgt[imax*vwgt_dim+i] = vwgt_arr_glob[j*vwgt_dim+i];
          tmp_vwgt[isum*vwgt_dim+i] += vwgt_arr_glob[j*vwgt_dim+i]; 
        }
      }

      /* Compute scaled imbalance. */
      for (j=0; j<nparts; j++){
        for (i=0; i<vwgt_dim; i++){
          if (tmp_vwgt[isum*vwgt_dim+i]*part_sizes[j*vwgt_dim+i] != 0.0)
            temp = vwgt_arr_glob[j*vwgt_dim+i]/(tmp_vwgt[isum*vwgt_dim+i]*part_sizes[j*vwgt_dim+i]);
          else
            temp = 1.0;
          if (temp > tmp_vwgt[iimbal*vwgt_dim+i])
            tmp_vwgt[iimbal*vwgt_dim+i] = temp;
        }
      }
    }

    /* Min, max, sum for cut weights. */
    if (zz->Edge_Weight_Dim>0 && have_graph_callbacks){
      for (i=0; i<zz->Edge_Weight_Dim; i++)
        tmp_cutwgt[i] = FLT_MAX; /*min */
      for (i=zz->Edge_Weight_Dim; i<3*zz->Edge_Weight_Dim; i++)
        tmp_cutwgt[i] = 0; /* max and sum */

      for (j=0; j<nparts; j++){
        for (i=0; i<zz->Edge_Weight_Dim; i++){
          if (cutwgt_arr_glob[j*zz->Edge_Weight_Dim+i] < tmp_cutwgt[i]) 
            tmp_cutwgt[i] = cutwgt_arr_glob[j*zz->Edge_Weight_Dim+i];
          if (cutwgt_arr_glob[j*zz->Edge_Weight_Dim+i] > tmp_cutwgt[imax*zz->Edge_Weight_Dim+i]) 
            tmp_cutwgt[imax*zz->Edge_Weight_Dim+i] = cutwgt_arr_glob[j*zz->Edge_Weight_Dim+i];
          tmp_cutwgt[isum*zz->Edge_Weight_Dim+i] += cutwgt_arr_glob[j*zz->Edge_Weight_Dim+i]; 
        }
      }
    }

    /* Print min-max-sum of results */
    if (zz->Proc == zz->Debug_Proc){
      printf("\n%s  Partition count: %1d requested, %1d computed, %1d non-empty\n", yo, req_nparts, nparts, nonempty_nparts);
      printf("%s  Statistics with respect to %1d partitions: \n", yo, nparts);
      printf("%s                         Min.     Max.      Sum  Imbalance\n", yo);
      printf("%s  No. of objects   :  %8d %8d %8d   %5.3f\n",
        yo, stats[imin*NUM_STATS], 
        stats[imax*NUM_STATS],
        stats[isum*NUM_STATS], 
        imbal[0]);
 
      for (i=0; i<vwgt_dim; i++){
        printf("%s  Object weight #%1d :  %8.3g %8.3g %8.3g   %5.3f\n",
          yo, i, tmp_vwgt[imin*vwgt_dim+i], 
        tmp_vwgt[imax*vwgt_dim+i], 
        tmp_vwgt[isum*vwgt_dim+i], 
        tmp_vwgt[iimbal*vwgt_dim+i]);
      }

      if (have_graph_callbacks) {
        for (i=0; i<zz->Edge_Weight_Dim; i++){
          printf("%s  Cut weight  #%1d   :  %8.3g %8.3g %8.3g   %5.3f\n",
            yo, i, tmp_cutwgt[imin*(zz->Edge_Weight_Dim)+i], 
          tmp_cutwgt[imax*(zz->Edge_Weight_Dim)+i], 
          tmp_cutwgt[isum*(zz->Edge_Weight_Dim)+i], 
          (tmp_cutwgt[isum*(zz->Edge_Weight_Dim)+i] > 0 
             ? tmp_cutwgt[imax*(zz->Edge_Weight_Dim)+i]*nparts/tmp_cutwgt[isum*(zz->Edge_Weight_Dim)+i]
             : 1.));
        }
      }

      for (i=1; i<NUM_STATS_PART; i++){
        switch(i){
        case 1:  sprintf(msg, "%s", "No. of edge cuts : ");
                 break;
        case 2:  sprintf(msg, "%s", "Boundary objects : ");
                 break;
        case 3:  sprintf(msg, "%s", "Comm. volume     : ");
                 break;
        case 4:  sprintf(msg, "%s", "Adj. partitions  : ");
                 break;
        default: sprintf(msg, "%s", "                   ");
                 break;
        }

        printf("%s  %s %8d %8d %8d   %5.3f\n",
          yo, msg, stats[imin*NUM_STATS+i], 
          stats[imax*NUM_STATS+i],
          stats[isum*NUM_STATS+i], 
          imbal[i]);
      }

      if (have_pin_callbacks){
        printf("%s  Hyperedge (k-1)-connectivity cut:     %8.0f\n", yo, 
                hgraph_global_sum[0]);
        printf("%s  No. cut hyperedges:                   %8.0f\n\n", yo,
                hgraph_global_sum[1]);
      }
      
      printf("\n");
    }
    

  }

End:

  /* Free data */

  ZOLTAN_FREE(&tmp_vwgt);

  if (have_pin_callbacks){
     Zoltan_PHG_Free_Hypergraph_Data(zhg);
     ZOLTAN_FREE(&zhg);
  }
  else {
    ZOLTAN_FREE(&global_ids);
    ZOLTAN_FREE(&local_ids);
    ZOLTAN_FREE(&part); 
    ZOLTAN_FREE(&vwgts);
  }
  ZOLTAN_FREE(&nbors_global);
  ZOLTAN_FREE(&nbors_proc);
  if (compute_part) ZOLTAN_FREE(&nbors_part);
  ZOLTAN_FREE(&ewgts);
  ZOLTAN_FREE(&tmp_cutwgt);
  ZOLTAN_FREE(&proc_count);
  ZOLTAN_FREE(&edges_per_obj);

  if (compute_part){
    ZOLTAN_FREE(&all_arr);
    ZOLTAN_FREE(&vwgt_arr);
    ZOLTAN_FREE(&part_sizes);
  }
  ZOLTAN_TRACE_EXIT(zz, yo);

  return ierr;
}
/***************************************************************************/
static int eval_edge_list(
  ZZ *zz, 
  int k,                      /* Index of object being processed */
  int num_gid_entries,        /* # of unsigned ints in a GID */
  ZOLTAN_ID_PTR global_ids,   /* Array of GIDs on this proc */
  int *part,                  /* Partition numbers of GIDs on this proc */
  ZOLTAN_ID_PTR nbors_global, /* GIDs of this obj's neighboring objs. */
  int *nbors_proc,            /* Procs owning the neighboring objs. */
  int *nbors_part,            /* Parts owning the neighboring objs. */
  float *ewgts,               /* Edge weights for each of obj's edges */
  int nedges,                 /* # of edges for this object */
  int num_obj,                /* # of objs on this proc */
  int compute_part,           /* Flag:  compute partition stats */
  int *proc_count,            /* # of nbors on each other proc */
  int *cuts,                  /* # of cut edges */
  int *num_boundary,          /* # of boundary objs */
  int *comm_vol,              /* communication volume */
  float *cutwgt,              /* total weight of cut edges (and other stats) */
  int *cut_arr,               /* # of partition cut edges. */
  float *cutwgt_arr,          /* weights of partition cut edges */
  int *bndry_arr,             /* # of partition boundary objs */
  int *commvol_arr           /* communication volume (for partitions) */
  /* int *part_count            TODO:  # edges to each partition  */
)
{
/* Function to evaluate edge cuts, etc., for an object's edge list. */
int i, j;
int ierr = ZOLTAN_OK;
int proc_flag, part_flag;

  /* Set flags to 0; we have not counted object k as a boundary vertex. */
  proc_flag = 0;
  part_flag = 0;

  /* Check for cut edges */
  for (j=0; j<nedges; j++){
    if (nbors_proc[j] != zz->Proc){
      (*cuts)++;
      for (i=0; i<zz->Edge_Weight_Dim; i++)
        cutwgt[i] += ewgts[j*(zz->Edge_Weight_Dim)+i];
      if (proc_flag==0){
        (*num_boundary)++;
        proc_flag = 1;
      }
      proc_count[nbors_proc[j]] = k;  /* overwrite proc_count with 
                                         current object number */
    }
    if (compute_part){
      if (nbors_part[j] != part[k]){
        cut_arr[part[k]]++;
        for (i=0; i<zz->Edge_Weight_Dim; i++)
          cutwgt_arr[part[k]*zz->Edge_Weight_Dim+i] 
            += ewgts[j*zz->Edge_Weight_Dim+i];
        if (part_flag==0){
          bndry_arr[part[k]]++;
          part_flag = 1;
        }
      }
    }
  }
  /* compute comm. vol. as number of procs there are edges to */
  for (i=0; i<zz->Num_Proc; i++){
    if (proc_count[i]==k) (*comm_vol)++;
  }
  /* TODO: also compute comm_vol wrt partitions here */
  return ierr;
}

/****************************************************************************/
#define TEST_DD_ERROR(ierr, yo, proc, fn) \
  if (ierr != ZOLTAN_OK && ierr != ZOLTAN_WARN) { \
    char msg[64];  \
    sprintf(msg, "Error in %s\n", fn); \
    ZOLTAN_PRINT_ERROR(proc, yo, msg); \
    goto End;\
  }

static int get_nbor_parts(
  ZZ *zz, 
  int nobj,                     /* Input:  number of local objs */
  ZOLTAN_ID_PTR global_ids,     /* Input:  GIDs of local objs */
  ZOLTAN_ID_PTR local_ids,      /* Input:  LIDs of local objs */
  int *part,                    /* Input:  part assignments of local objs */
  int nnbors,                   /* Input:  number of neighboring objs */
  ZOLTAN_ID_PTR nbors_global,   /* Input:  GIDs of neighboring objs */
  int *nbors_part               /* Output:  part assignments of neighbor objs */
)
{
/* Function to retrieve the partition number for neighboring nodes. */
char *yo = "get_nbor_parts";
struct Zoltan_DD_Struct *dd = NULL;
int *owner = NULL;
int ierr;

  ierr = Zoltan_DD_Create(&dd, zz->Communicator, zz->Num_GID, zz->Num_LID,
                          0, 0, 0);
  TEST_DD_ERROR(ierr, yo, zz->Proc, "Zoltan_DD_Create");

  ierr = Zoltan_DD_Update(dd, global_ids, local_ids, NULL, part, nobj);
  TEST_DD_ERROR(ierr, yo, zz->Proc, "Zoltan_DD_Update");
  
  owner = (int *) ZOLTAN_MALLOC(nnbors * sizeof(int));
  ierr = Zoltan_DD_Find(dd, nbors_global, NULL, NULL, nbors_part,
                        nnbors, owner);
  ZOLTAN_FREE(&owner);
  TEST_DD_ERROR(ierr, yo, zz->Proc, "Zoltan_DD_Find");

End:
  Zoltan_DD_Destroy(&dd);
  return ierr;
}


#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif
