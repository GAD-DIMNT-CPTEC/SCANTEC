/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * For more info, see the README file in the top-level Zoltan directory.     *
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: phg.c,v $
 *    $Author: w6ws $
 *    $Date: 2008/12/10 20:50:37 $
 *    Revision: 1.101.2.3 $
 ****************************************************************************/

#include <math.h>
#include "phg.h"
#include "params_const.h"
#include "all_allo_const.h"

#if defined (ESMF_OS_MinGW)
#define strcasecmp _strcmpi
#endif
 
#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif



/*
#define CHECK_LEFTALONE_VERTICES
*/
    
/*
 *  Main routine for Zoltan interface to hypergraph partitioning. 
 *  Also routines that build input data structures, set parameters, etc.
 */


/******************************************************************************/
/*  Parameters structure for parallel HG method.  */
static PARAM_VARS PHG_params[] = {
  /* Add parameters here. */
  {"HYPERGRAPH_PACKAGE",              NULL,  "STRING", 0},
  {"PHG_OUTPUT_LEVEL",                NULL,  "INT",    0},
  {"FINAL_OUTPUT",                    NULL,  "INT",    0},
  {"CHECK_GRAPH",                     NULL,  "INT",    0},
  {"CHECK_HYPERGRAPH",                NULL,  "INT",    0},
  {"PHG_NPROC_VERTEX",                NULL,  "INT",    0},
  {"PHG_NPROC_EDGE",                  NULL,  "INT",    0},
  {"PHG_COARSENING_LIMIT",            NULL,  "INT",    0},
  {"PHG_COARSENING_METHOD",           NULL,  "STRING", 0},
  {"PHG_COARSENING_METHOD_FAST",      NULL,  "STRING", 0},
  {"PHG_VERTEX_VISIT_ORDER",          NULL,  "INT",    0},
  {"PHG_EDGE_SCALING",                NULL,  "INT",    0},
  {"PHG_VERTEX_SCALING",              NULL,  "INT",    0},
  {"PHG_COARSEPARTITION_METHOD",      NULL,  "STRING", 0},
  {"PHG_REFINEMENT_METHOD",           NULL,  "STRING", 0},
  {"PHG_DIRECT_KWAY",                 NULL,  "INT",    0},
  {"PHG_REFINEMENT_LOOP_LIMIT",       NULL,  "INT",    0},
  {"PHG_REFINEMENT_MAX_NEG_MOVE",     NULL,  "INT",    0},    
  {"PHG_REFINEMENT_QUALITY",          NULL,  "FLOAT",  0},
  {"PHG_USE_TIMERS",                  NULL,  "INT",    0},    
  {"USE_TIMERS",                      NULL,  "INT",    0},    
  {"PHG_EDGE_SIZE_THRESHOLD",         NULL,  "FLOAT",  0},
  {"PHG_BAL_TOL_ADJUSTMENT",          NULL,  "FLOAT",  0},  
  {"PHG_EDGE_WEIGHT_OPERATION",       NULL,  "STRING",  0},
  {"PARKWAY_SERPART",                 NULL,  "STRING", 0},
  {"ADD_OBJ_WEIGHT",                  NULL,  "STRING", 0},
  {"PHG_EDGE_WEIGHT_OPERATION",       NULL,  "STRING", 0},
  {"PHG_RANDOMIZE_INPUT",             NULL,  "INT",    0},    
  { "PATOH_ALLOC_POOL0",              NULL,  "INT",    0},
  { "PATOH_ALLOC_POOL1",              NULL,  "INT",    0},   
  {NULL,                              NULL,  NULL,     0}     
};

/* prototypes for static functions: */

static int Zoltan_PHG_Output_Parts(ZZ*, ZHG*, Partition);
static int Zoltan_PHG_Return_Lists(ZZ*, ZHG*, int*, ZOLTAN_ID_PTR*, 
  ZOLTAN_ID_PTR*, int**, int**);

#ifdef CHECK_LEFTALONE_VERTICES    
static int findAndSaveLeftAloneVertices(ZZ *zz, HGraph *hg, int p, 
                                 Partition parts,
                                 PHGPartParams *hgp) 
{
    char *yo="findAndSaveLeftAloneVertices";
    int *lneigh[2]={NULL, NULL}, *neigh[2]={NULL, NULL}, i, j, ierr=ZOLTAN_OK;
    int *lpins=NULL, *pins=NULL;
    PHGComm *hgc=hg->comm;

    if (hg->nEdge && (!(lpins = (int*) ZOLTAN_CALLOC(p * hg->nEdge, sizeof(int))) ||
                      !(pins  = (int*) ZOLTAN_MALLOC(p * hg->nEdge * sizeof(int)))))
        MEMORY_ERROR;

    for (i = 0; i < hg->nEdge; ++i)
        for (j = hg->hindex[i]; j < hg->hindex[i+1]; ++j)
            ++lpins[i*p+parts[hg->hvertex[j]]];
    if (hg->nEdge)
        MPI_Allreduce(lpins, pins, p*hg->nEdge, MPI_INT, MPI_SUM, 
                      hgc->row_comm);
    
    if (hg->nVtx && !(lneigh[0]  = (int*) ZOLTAN_MALLOC(2 * hg->nVtx * sizeof(int))))
        MEMORY_ERROR;
    if (!hgc->myProc_y) 
        if (hg->nVtx && !(neigh[0]  = (int*) ZOLTAN_MALLOC(2 * hg->nVtx * sizeof(int))))
            MEMORY_ERROR;

    if (hg->nVtx) {
        lneigh[1] = &(lneigh[0][hg->nVtx]);
        if (!hgc->myProc_y)
            neigh[1] = &(neigh[0][hg->nVtx]);
    }

    for (i = 0; i < hg->nVtx; ++i) {
        int pno = parts[i];
        lneigh[0][i] = lneigh[1][i] = 0;
        for (j = hg->vindex[i]; j < hg->vindex[i+1]; j++) {
            int edge = hg->vedge[j], k;
            lneigh[0][i] += (pins[edge*p+pno]-1); /* exclude the vertex's itself */
            for (k=0; k<p; ++k)
                if (k!=pno)
                    lneigh[1][i] += pins[edge*p+k];            
        }
    }
    
    if (hg->nVtx) 
        MPI_Reduce(lneigh[0], neigh[0], 2*hg->nVtx, MPI_INT, MPI_SUM, 0, hgc->col_comm);

    if (!hgc->myProc_y) {
        int alone=0, galone=0;        
        for (i=0; i<hg->nVtx; ++i)
            if (!neigh[0] && neigh[1]) {
                ++alone;
                if (alone<10)
                    uprintf(hgc, "vertex %d is alone in part %d but it has %d neighbours (!overcounted!) on other %d parts\n", i, parts[i], neigh[1]);
            }
        MPI_Reduce(&alone, &galone, 1, MPI_INT, MPI_SUM, 0, hgc->row_comm);
        if (!hgc->myProc)
            uprintf(hgc, "There are %d left-alone vertices\n", galone);        
    }
End:
    Zoltan_Multifree(__FILE__,__LINE__, 4, &lpins, &pins, &lneigh[0], &neigh[0]);
    return ierr;
}
#endif
 
 
/******************************************************************************/
/* Main routine for Zoltan interface to hypergraph partitioning. Builds input */
/* data structures, set parameters, calls HG partitioner, builds return lists.*/
/* Type = ZOLTAN_LB_FN.                                                       */

int Zoltan_PHG(
ZZ *zz,                    /* The Zoltan structure  */
float *part_sizes,         /* Input:  Array of size zz->Num_Global_Parts 
                                containing the percentage of work assigned 
                                to each partition. */
int *num_imp,              /* not computed */
ZOLTAN_ID_PTR *imp_gids,   /* not computed */
ZOLTAN_ID_PTR *imp_lids,   /* not computed */
int **imp_procs,           /* not computed */
int **imp_to_part,         /* not computed */
int *num_exp,              /* number of objects to be exported */
ZOLTAN_ID_PTR *exp_gids,   /* global ids of objects to be exported */
ZOLTAN_ID_PTR *exp_lids,   /* local  ids of objects to be exported */
int **exp_procs,           /* list of processors to export to */
int **exp_to_part )         /* list of partitions to which exported objs
                                are assigned. */
{
  char *yo = "Zoltan_PHG";
  ZHG *zoltan_hg = NULL;
  PHGPartParams hgp;               /* Hypergraph parameters. */
  HGraph *hg = NULL;               /* Hypergraph itself */
  Partition parts = NULL;          /* Partition assignments in 
                                      2D distribution. */
  int err = ZOLTAN_OK, p=0;
  static int timer_all = -1;       /* Note:  this timer includes other
                                      timers and their synchronization time,
                                      so it will be a little high. */
  static int timer_build=-1;       /* timers to be used in this function;
                                      declared static so that, over multiple
                                      runs, can accumulate times.  */
  static int timer_retlist=-1;
  static int timer_patoh=-1;
  static int timer_parkway=-1;
  static int timer_finaloutput=-1;
  static int timer_setupvmap=-1;
  int do_timing = 0;

  ZOLTAN_TRACE_ENTER(zz, yo);

  /* Initialization of return arguments. */
  *num_imp   = *num_exp   = -1;
  *imp_gids  = *exp_gids  = NULL;
  *imp_lids  = *exp_lids  = NULL;
  *imp_procs = *exp_procs = NULL;
  
  /* Initialize HG parameters. */
  err = Zoltan_PHG_Initialize_Params (zz, part_sizes, &hgp);
  if (err != ZOLTAN_OK)
    goto End;

  if (hgp.use_timers) {
    if (timer_all < 0) 
      timer_all = Zoltan_Timer_Init(zz->ZTime, 1, "Zoltan_PHG");
  }

  if (hgp.use_timers > 1) {
    do_timing = 1;
    if (timer_build < 0) 
      timer_build = Zoltan_Timer_Init(zz->ZTime, 1, "Build");
    if (timer_setupvmap < 0) 
      timer_setupvmap = Zoltan_Timer_Init(zz->ZTime, 0, "Vmaps");
  }

  if (hgp.use_timers) 
    ZOLTAN_TIMER_START(zz->ZTime, timer_all, zz->Communicator);
    
  if (do_timing)
    ZOLTAN_TIMER_START(zz->ZTime, timer_build, zz->Communicator);
    
  /* build initial Zoltan hypergraph from callback functions. */

  err = Zoltan_PHG_Build_Hypergraph (zz, &zoltan_hg, &parts, &hgp);
  if (err != ZOLTAN_OK && err != ZOLTAN_WARN) {
    ZOLTAN_PRINT_ERROR(zz->Proc, yo, "Error building hypergraph.");
    goto End;
  }

  zz->LB.Data_Structure = zoltan_hg;
  hg = &zoltan_hg->HG;
  p = zz->LB.Num_Global_Parts;  
  zoltan_hg->HG.redl = MAX(hgp.redl,p);     /* redl needs to be dynamic */
  /* RTHRTH -- redl may need to be scaled by number of procs */
  /* EBEB -- at least make sure redl > #procs */
 
  if (do_timing)
    ZOLTAN_TIMER_STOP(zz->ZTime, timer_build, zz->Communicator);

/*
  uprintf(hg->comm, "Zoltan_PHG kway=%d #parts=%d\n", hgp.kway, zz->LB.Num_Global_Parts);
*/

  if (zz->LB.Method == PARKWAY) {
    if (do_timing) {
      if (timer_parkway < 0)
        timer_parkway = Zoltan_Timer_Init(zz->ZTime, 0, "PHG_ParKway");
      ZOLTAN_TIMER_START(zz->ZTime, timer_parkway, zz->Communicator);
    }
    err = Zoltan_PHG_ParKway(zz, hg, p,
                             parts, &hgp);
    if (err != ZOLTAN_OK) 
        goto End;
    if (do_timing)
      ZOLTAN_TIMER_STOP(zz->ZTime, timer_parkway, zz->Communicator);
  } else if (zz->LB.Method == PATOH) {
    if (hgp.use_timers > 1) {
      if (timer_patoh < 0)
        timer_patoh = Zoltan_Timer_Init(zz->ZTime, 0, "HG_PaToH");
      ZOLTAN_TIMER_START(zz->ZTime, timer_patoh, zz->Communicator);
    }
    err = Zoltan_PHG_PaToH(zz, hg, p,
                           parts, &hgp);
    if (err != ZOLTAN_OK) 
      goto End;
    if (hgp.use_timers > 1)
      ZOLTAN_TIMER_STOP(zz->ZTime, timer_patoh, zz->Communicator);
  }      
  else { /* it must be PHG */
    /* UVC: if it is bisection anyways; no need to create vmap etc; 
       rdivide is going to call Zoltan_PHG_Partition anyways... */
    if (hgp.globalcomm.Communicator != MPI_COMM_NULL) {
      /* This processor is part of the 2D data distribution; it should
         participate in partitioning. */

        
      if (hgp.kway || zz->LB.Num_Global_Parts == 2) {
        /* call main V cycle routine */
        err = Zoltan_PHG_Partition(zz, hg, p,
                                   hgp.part_sizes, parts, &hgp, 0);
        if (err != ZOLTAN_OK) {
          ZOLTAN_PRINT_ERROR(zz->Proc, yo, "Error partitioning hypergraph.");
          goto End;
        }
      }
      else {
        int i;
          
        if (do_timing) 
          ZOLTAN_TIMER_START(zz->ZTime, timer_setupvmap, zz->Communicator);
        /* vmap associates original vertices to sub hypergraphs */
        if (hg->nVtx && 
            !(hg->vmap = (int*) ZOLTAN_MALLOC(hg->nVtx*sizeof (int))))  {
          err = ZOLTAN_MEMERR;
          ZOLTAN_PRINT_ERROR(zz->Proc, yo, "Memory error.");
          goto End;
        }
        for (i = 0; i < hg->nVtx; ++i)
          hg->vmap[i] = i;
  
        if (do_timing) 
          ZOLTAN_TIMER_STOP(zz->ZTime, timer_setupvmap, zz->Communicator);
  
          
        /* partition hypergraph */
        err = Zoltan_PHG_rdivide (0, p-1, parts, zz, hg, &hgp, 0);
  
        if (hgp.output_level >= PHG_DEBUG_LIST)     
          uprintf(hg->comm, "FINAL %3d |V|=%6d |E|=%6d #pins=%6d %s/%s/%s p=%d "
                  "bal=%.2f cutl=%.2f\n", 
                  hg->info, hg->nVtx, hg->nEdge, hg->nPins,
                  hgp.redm_str, hgp.coarsepartition_str, hgp.refinement_str, p,
                  Zoltan_PHG_Compute_Balance(zz, hg, hgp.part_sizes, p, parts),
                  Zoltan_PHG_Compute_ConCut(hg->comm, hg, parts, p, &err));
            
        if (err != ZOLTAN_OK)  {
          ZOLTAN_PRINT_ERROR(zz->Proc, yo, "Error partitioning hypergraph.");
          goto End;
        }
        ZOLTAN_FREE (&hg->vmap);
      }
#ifdef CHECK_LEFTALONE_VERTICES          
      findAndSaveLeftAloneVertices(zz, hg, p, parts, &hgp);
#endif      
    }
  }
        
  if (do_timing) {
    /* Initialize these timers here so their output is near end of printout */
    if (timer_retlist < 0) 
      timer_retlist = Zoltan_Timer_Init(zz->ZTime, 1, "Return_Lists");
    if (timer_finaloutput < 0) 
      timer_finaloutput = Zoltan_Timer_Init(zz->ZTime, 1, "Final_Output");

    ZOLTAN_TIMER_START(zz->ZTime, timer_retlist, zz->Communicator);
  }

  /* Build Zoltan's Output_Parts, mapped from 2D distribution 
     to input distribution. */

  Zoltan_PHG_Output_Parts(zz, zoltan_hg, parts);

  /* Build Zoltan's return arguments. */
  Zoltan_PHG_Return_Lists(zz, zoltan_hg, num_exp, exp_gids,
   exp_lids, exp_procs, exp_to_part);
    
  if (do_timing)
    ZOLTAN_TIMER_STOP(zz->ZTime, timer_retlist, zz->Communicator);

End:
  if (err == ZOLTAN_MEMERR)
    ZOLTAN_PRINT_ERROR (zz->Proc, yo, "Memory error.")
  else if (err != ZOLTAN_OK)
    ZOLTAN_PRINT_ERROR (zz->Proc, yo, "Error partitioning hypergraph.")
    
  /* KDDKDD The following code prints a final quality result even when
   * KDDKDD phg_output_level is zero.  It is useful for our tests and
   * KDDKDD data collection, but it should NOT be included in the released
   * KDDKDD code.  */
  if ((err == ZOLTAN_OK) && hgp.final_output) {
    static int nRuns=0;
    static double balsum = 0.0, cutlsum = 0.0, cutnsum = 0.0;
    static double balmax = 0.0, cutlmax = 0.0, cutnmax = 0.0;
    static double balmin = 1e100, cutlmin = 1e100, cutnmin = 1e100;
    double bal = 0.; 
    double cutl = 0.; /* Connnectivity cuts:  sum_over_edges((npart-1)*ewgt) */
    double cutn = 0.; /* Net cuts:  sum_over_edges((nparts>1)*ewgt) */

    double rlocal[2];  /* local cut stats for removed edges */
    double rglobal[2]; /* global cut stats for removed edges */
    int gnremove;

    if (do_timing) {
      /* Do not include final output time in partitioning time */
      ZOLTAN_TIMER_STOP(zz->ZTime, timer_all, zz->Communicator);
      ZOLTAN_TIMER_START(zz->ZTime, timer_finaloutput, zz->Communicator);
    }

    if (hgp.globalcomm.Communicator != MPI_COMM_NULL) {
      /* Processor participated in partitioning */
      bal = Zoltan_PHG_Compute_Balance(zz, hg, hgp.part_sizes,
                                       zz->LB.Num_Global_Parts, parts);
      cutl= Zoltan_PHG_Compute_ConCut(hg->comm, hg, parts,
                                      zz->LB.Num_Global_Parts, &err);
      cutn = Zoltan_PHG_Compute_NetCut(hg->comm, hg, parts,
                                       zz->LB.Num_Global_Parts);
    }

    if (!err) {
     
      /* Add in cut contributions from removed edges */
      MPI_Allreduce(&(zoltan_hg->nRemove), &gnremove, 1, MPI_INT, MPI_SUM,
                    zz->Communicator);
      if (gnremove) {
        err = Zoltan_PHG_Removed_Cuts(zz, zoltan_hg, rlocal);
        MPI_Allreduce(rlocal, rglobal, 2, MPI_DOUBLE,MPI_SUM,zz->Communicator);
        
        cutl += rglobal[0];
        cutn += rglobal[1];
      }
  
      cutlsum += cutl;
      if (cutl > cutlmax) cutlmax = cutl;
      if (cutl < cutlmin) cutlmin = cutl;
      cutnsum += cutn;
      if (cutn > cutnmax) cutnmax = cutn;
      if (cutn < cutnmin) cutnmin = cutn;
      balsum += bal;
      if (bal > balmax) balmax = bal;
      if (bal < balmin) balmin = bal;
      nRuns++;
   
      if (zz->Proc == 0) {
        uprintf(hg->comm, 
                "STATS Runs %d  bal  CURRENT %f  MAX %f  MIN %f  AVG %f\n", 
                nRuns, bal, balmax, balmin, balsum/nRuns);
        uprintf(hg->comm, 
                "STATS Runs %d  cutl CURRENT %f  MAX %f  MIN %f  AVG %f\n", 
                nRuns, cutl, cutlmax, cutlmin, cutlsum/nRuns);
        uprintf(hg->comm, 
                "STATS Runs %d  cutn CURRENT %f  MAX %f  MIN %f  AVG %f\n", 
                nRuns, cutn, cutnmax, cutnmin, cutnsum/nRuns);
      }
    }

    if (do_timing) {
      ZOLTAN_TIMER_STOP(zz->ZTime, timer_finaloutput, zz->Communicator);
      ZOLTAN_TIMER_START(zz->ZTime, timer_all, zz->Communicator);
    }
  }
  /* KDDKDD  End of printing section. */
  
  ZOLTAN_FREE(&parts);
  Zoltan_PHG_Free_Structure(zz);

  if (hgp.use_timers) {
    ZOLTAN_TIMER_STOP(zz->ZTime, timer_all, zz->Communicator);
    if (hgp.globalcomm.Communicator != MPI_COMM_NULL)
      Zoltan_Timer_PrintAll(zz->ZTime, 0, hgp.globalcomm.Communicator, stdout);
  }

  if (hgp.globalcomm.Communicator != MPI_COMM_NULL)
    MPI_Comm_free(&(hgp.globalcomm.Communicator));

  ZOLTAN_TRACE_EXIT(zz, yo);
  return err;
}
/*****************************************************************************/

void Zoltan_PHG_Free_Hypergraph_Data(ZHG *zoltan_hg)
{
  if (zoltan_hg != NULL) {
    Zoltan_Multifree(__FILE__, __LINE__, 10, &zoltan_hg->GIDs,
                                            &zoltan_hg->LIDs,
                                            &zoltan_hg->Input_Parts,
                                            &zoltan_hg->Output_Parts,
                                            &zoltan_hg->Remove_EGIDs,
                                            &zoltan_hg->Remove_ELIDs,
                                            &zoltan_hg->Remove_Esize,
                                            &zoltan_hg->Remove_Ewgt,
                                            &zoltan_hg->Remove_Pin_GIDs,
                                            &zoltan_hg->Remove_Pin_Procs);

    Zoltan_HG_HGraph_Free (&zoltan_hg->HG);
  }
}

/*****************************************************************************/

void Zoltan_PHG_Free_Structure(ZZ *zz)
{
  /* frees all data associated with LB.Data_Structure for hypergraphs */
  ZHG *zoltan_hg = (ZHG*) zz->LB.Data_Structure;

  if (zoltan_hg != NULL) {
    Zoltan_PHG_Free_Hypergraph_Data(zoltan_hg);
    ZOLTAN_FREE (&zz->LB.Data_Structure);
  }
}

    
/*****************************************************************************/

int Zoltan_PHG_Initialize_Params(
  ZZ *zz,   /* the Zoltan data structure */
  float *part_sizes,
  PHGPartParams *hgp
)
{
  int err = ZOLTAN_OK;
  char *yo = "Zoltan_PHG_Initialize_Params";
  int nProc;
  int usePrimeComm;
  MPI_Comm communicator;
  char add_obj_weight[MAX_PARAM_STRING_LEN];
  char edge_weight_op[MAX_PARAM_STRING_LEN];

  memset(hgp, 0, sizeof(*hgp)); /* in the future if we forget to initialize
                                   another param at least it will be 0 */
  
  Zoltan_Bind_Param(PHG_params, "HYPERGRAPH_PACKAGE", &hgp->hgraph_pkg);
  Zoltan_Bind_Param(PHG_params, "PHG_OUTPUT_LEVEL", &hgp->output_level);
  Zoltan_Bind_Param(PHG_params, "FINAL_OUTPUT", &hgp->final_output); 
  Zoltan_Bind_Param(PHG_params, "CHECK_GRAPH", &hgp->check_graph);   
  Zoltan_Bind_Param(PHG_params, "CHECK_HYPERGRAPH", &hgp->check_graph);   
  Zoltan_Bind_Param(PHG_params, "PHG_NPROC_VERTEX", &hgp->nProc_x_req);
  Zoltan_Bind_Param(PHG_params, "PHG_NPROC_EDGE", &hgp->nProc_y_req);
  Zoltan_Bind_Param(PHG_params, "PHG_COARSENING_LIMIT", &hgp->redl);
  Zoltan_Bind_Param(PHG_params, "PHG_COARSENING_METHOD", hgp->redm_str);
  Zoltan_Bind_Param(PHG_params, "PHG_COARSENING_METHOD_FAST", hgp->redm_fast);
  Zoltan_Bind_Param(PHG_params, "PHG_VERTEX_VISIT_ORDER", &hgp->visit_order);
  Zoltan_Bind_Param(PHG_params, "PHG_EDGE_SCALING", &hgp->edge_scaling);
  Zoltan_Bind_Param(PHG_params, "PHG_VERTEX_SCALING", &hgp->vtx_scaling);
  Zoltan_Bind_Param(PHG_params, "PHG_REFINEMENT_METHOD", hgp->refinement_str);
  Zoltan_Bind_Param(PHG_params, "PHG_DIRECT_KWAY", &hgp->kway);
  Zoltan_Bind_Param(PHG_params, "PHG_REFINEMENT_LOOP_LIMIT", 
                                &hgp->fm_loop_limit);
  Zoltan_Bind_Param(PHG_params, "PHG_REFINEMENT_MAX_NEG_MOVE", 
                                &hgp->fm_max_neg_move);  
  Zoltan_Bind_Param(PHG_params, "PHG_REFINEMENT_QUALITY", 
                                &hgp->refinement_quality);  
  Zoltan_Bind_Param(PHG_params, "PHG_COARSEPARTITION_METHOD", 
                                 hgp->coarsepartition_str);
  Zoltan_Bind_Param(PHG_params, "PHG_USE_TIMERS",
                                 (void*) &hgp->use_timers);  
  Zoltan_Bind_Param(PHG_params, "USE_TIMERS",
                                 (void*) &hgp->use_timers);  
  Zoltan_Bind_Param(PHG_params, "PHG_EDGE_SIZE_THRESHOLD",
                                 (void*) &hgp->EdgeSizeThreshold);  
  Zoltan_Bind_Param(PHG_params, "PHG_BAL_TOL_ADJUSTMENT",
                                 (void*) &hgp->bal_tol_adjustment);  
  Zoltan_Bind_Param(PHG_params, "PARKWAY_SERPART",
                                 (void *) hgp->parkway_serpart);  
  Zoltan_Bind_Param(PHG_params, "ADD_OBJ_WEIGHT",
                                 (void *) add_obj_weight);
  Zoltan_Bind_Param(PHG_params, "PHG_EDGE_WEIGHT_OPERATION",
                                 (void *) edge_weight_op);
  Zoltan_Bind_Param(PHG_params, "PHG_RANDOMIZE_INPUT",
                                 (void*) &hgp->RandomizeInitDist);  
  Zoltan_Bind_Param(PHG_params, "PATOH_ALLOC_POOL0",
                                 (void*) &hgp->patoh_alloc_pool0);
  Zoltan_Bind_Param(PHG_params, "PATOH_ALLOC_POOL1",
                                 (void*) &hgp->patoh_alloc_pool1);
  
  
  /* Set default values */
  strncpy(hgp->hgraph_pkg,       "default",  MAX_PARAM_STRING_LEN);
  strncpy(hgp->redm_str,            "ipm",   MAX_PARAM_STRING_LEN);
  strncpy(hgp->redm_fast,           "l-ipm", MAX_PARAM_STRING_LEN);
  strncpy(hgp->coarsepartition_str, "auto",  MAX_PARAM_STRING_LEN);
  strncpy(hgp->refinement_str,      "fm2",   MAX_PARAM_STRING_LEN);
  strncpy(hgp->parkway_serpart,     "patoh", MAX_PARAM_STRING_LEN);
  strncpy(add_obj_weight,            "none", MAX_PARAM_STRING_LEN);
  strncpy(edge_weight_op,            "max",  MAX_PARAM_STRING_LEN);

  hgp->use_timers = 0;
  hgp->LocalCoarsePartition = 0;
  hgp->edge_scaling = 0;
  hgp->vtx_scaling = 0;
  hgp->vtx_scal = NULL;  /* Array for storing vertex degree scale vector. 
                            Should perhaps go in hg structure, not the
                            param struct? */
  hgp->visit_order = 0;  /* Random */
  hgp->check_graph = 0;
  hgp->bal_tol = zz->LB.Imbalance_Tol[0]; /* Make vector for multiconstraint */
  hgp->bal_tol_adjustment = 0.7;
  hgp->redl = MAX(2*zz->LB.Num_Global_Parts, 100);
  hgp->output_level = PHG_DEBUG_NONE;
  hgp->final_output = 0;
  hgp->nProc_x_req = -1;
  hgp->nProc_y_req = -1;
  hgp->kway = 0;
  hgp->fm_loop_limit = 10;
  hgp->fm_max_neg_move = 250;  
  hgp->refinement_quality = 1;
  hgp->part_sizes = part_sizes;
  hgp->RandomizeInitDist = 0;
  hgp->EdgeSizeThreshold = 0.25;  
  hgp->hybrid_keep_factor = 0.;
  hgp->patoh_alloc_pool0 = 0;
  hgp->patoh_alloc_pool1 = 0;
  
  /* Get application values of parameters. */
  err = Zoltan_Assign_Param_Vals(zz->Params, PHG_params, zz->Debug_Level, 
          zz->Proc, zz->Debug_Proc);

  nProc = zz->Num_Proc;
  usePrimeComm = 0;

  /* Reset LB.Method if hgraph_pkg was set using a parameter */
  if (!strcasecmp(hgp->hgraph_pkg, "PHG"))
    zz->LB.Method = PHG;
  else if (!strcasecmp(hgp->hgraph_pkg, "PATOH"))
    zz->LB.Method = PATOH;
  else if (!strcasecmp(hgp->hgraph_pkg, "PARKWAY"))
    zz->LB.Method = PARKWAY;
  else if (!strcasecmp(hgp->hgraph_pkg, "default"))
    ; /* do nothing; leave LB.Method as is */
  else {
    ZOLTAN_PRINT_ERROR(zz->Proc, yo, "Invalid hypergraph package.\n");
    err = ZOLTAN_WARN;
  }

  /* Parse add_obj_weight parameter */
  if (!strcasecmp(add_obj_weight, "none")){
    hgp->add_obj_weight = PHG_ADD_NO_WEIGHT;
  } else if (!strcasecmp(add_obj_weight, "vertices")){
    hgp->add_obj_weight = PHG_ADD_UNIT_WEIGHT;
  } else if (!strcasecmp(add_obj_weight, "unit")){
    hgp->add_obj_weight = PHG_ADD_UNIT_WEIGHT;
  } else if (!strcasecmp(add_obj_weight, "vertex degree")){
    hgp->add_obj_weight = PHG_ADD_PINS_WEIGHT;
  } else if (!strcasecmp(add_obj_weight, "nonzeros")){
    hgp->add_obj_weight = PHG_ADD_PINS_WEIGHT;
  } else if (!strcasecmp(add_obj_weight, "pins")){
    hgp->add_obj_weight = PHG_ADD_PINS_WEIGHT;
  } else{
    ZOLTAN_PRINT_ERROR(zz->Proc, yo, "Invalid ADD_OBJ_WEIGHT parameter.\n");
    err = ZOLTAN_WARN;
  }

  if ((zz->Obj_Weight_Dim==0) &&      /* no application supplied weights */
      (hgp->add_obj_weight==PHG_ADD_NO_WEIGHT)){ /* no calculated weight */

    hgp->add_obj_weight = PHG_ADD_UNIT_WEIGHT; /* default object weight */
  }

  if (!strcasecmp(edge_weight_op, "max")){
    hgp->edge_weight_op = PHG_MAX_EDGE_WEIGHTS;
  } else if (!strcasecmp(edge_weight_op, "add")){
    hgp->edge_weight_op = PHG_ADD_EDGE_WEIGHTS;
  } else if (!strcasecmp(edge_weight_op, "error")){
    hgp->edge_weight_op = PHG_FLAG_ERROR_EDGE_WEIGHTS;
  } else{
    ZOLTAN_PRINT_ERROR(zz->Proc, yo,
      "Invalid PHG_EDGE_WEIGHT_OPERATION parameter.\n");
    err = ZOLTAN_WARN;
  }

  /* Adjust refinement parameters using hgp->refinement_quality */
  if (hgp->refinement_quality < 0.5/hgp->fm_loop_limit) 
    /* No refinement */
    strncpy(hgp->refinement_str,      "no",   MAX_PARAM_STRING_LEN);
  else {
    /* Scale FM parameters */
    hgp->fm_loop_limit   *= hgp->refinement_quality;
    hgp->fm_max_neg_move *= hgp->refinement_quality;
  }

  if (zz->LB.Method == PHG) {
    /* Test to determine whether we should change the number of processors
       used for partitioning to make more efficient 2D decomposition */

    if (hgp->nProc_x_req != 1 && hgp->nProc_y_req != 1)  /* Want 2D decomp */
      if (zz->Num_Proc > SMALL_PRIME && Zoltan_PHG_isPrime(zz->Num_Proc)) 
        /* 2D data decomposition is requested but we have a prime 
         * number of processors. */
        usePrimeComm = 1;
  }
  else if (zz->LB.Method == PARKWAY) {
    if (hgp->nProc_x_req>1) {
      err = ZOLTAN_FATAL;
      ZOLTAN_PRINT_ERROR(zz->Proc, yo, "ParKway requires nProc_x=1 or -1.");
      goto End;
    }
    hgp->nProc_x_req = 1;
  } else if (zz->LB.Method == PATOH) {
    if (zz->Num_Proc>1) {
      err = ZOLTAN_FATAL;
      ZOLTAN_PRINT_ERROR(zz->Proc, yo, "PaToH only works with Num_Proc=1.");
      goto End;
    }
  }

  if (!usePrimeComm)
    MPI_Comm_dup(zz->Communicator, &communicator);
  else {
    MPI_Group newgrp, zzgrp;
    nProc--;
    MPI_Comm_group(zz->Communicator, &zzgrp);
    MPI_Group_excl(zzgrp, 1, &nProc, &newgrp);
    MPI_Comm_create(zz->Communicator, newgrp, &communicator);
    MPI_Group_free(&newgrp);
    MPI_Group_free(&zzgrp);
  }

  err = Zoltan_PHG_Set_2D_Proc_Distrib(zz, communicator, zz->Proc, 
                                       nProc, hgp->nProc_x_req, 
                                       hgp->nProc_y_req, 
                                       &hgp->globalcomm);
  if (err != ZOLTAN_OK) 
    goto End;

  /* Convert strings to function pointers. */
  err = Zoltan_PHG_Set_Part_Options (zz, hgp);
  
End:
  return err;
}

/*****************************************************************************/

int Zoltan_PHG_Set_Param(
  char *name,                     /* name of variable */
  char *val)                      /* value of variable */
{
  /* associates value to named variable for PHG partitioning parameters */
  PARAM_UTYPE result;         /* value returned from Check_Param */
  int index;                  /* index returned from Check_Param */

  return Zoltan_Check_Param (name, val, PHG_params, &result, &index);
}

/****************************************************************************/

static int Zoltan_PHG_Output_Parts (
  ZZ *zz,
  ZHG *zhg,
  Partition hg_parts   /* Output partitions relative to the 2D distribution
                          of zhg->HG */
)
{
/* Function to map the computed partition from the distribution in HGraph
 * to the input distribution 
 */

static char *yo = "Zoltan_PHG_Output_Parts";
int i;
int msg_tag = 31000;
int ierr = ZOLTAN_OK;
int nObj = zhg->nObj;
int *outparts = NULL;
int *sendbuf = NULL;  
HGraph *phg = &(zhg->HG);

  zhg->Output_Parts = outparts 
                     = (int*) ZOLTAN_MALLOC (nObj * sizeof(int));
  if (zhg->VtxPlan != NULL) {
    /* Get the partition information from the 2D decomposition back to the
     * original owning processor for each GID.  */
    sendbuf = (int*) ZOLTAN_MALLOC(zhg->nRecv_GNOs * sizeof(int));
    for (i = 0; i < zhg->nRecv_GNOs; i++)
      sendbuf[i] = hg_parts[VTX_GNO_TO_LNO(phg, zhg->Recv_GNOs[i])];
    ierr = Zoltan_Comm_Do_Reverse(zhg->VtxPlan, msg_tag, (char*) sendbuf,
                                  sizeof(int), NULL, (char *) outparts);
    if (ierr) {
      ZOLTAN_PRINT_ERROR(zz->Proc, yo, "Error from Zoltan_Comm_Do_Reverse");
      goto End;
    }

    ZOLTAN_FREE(&sendbuf);
    Zoltan_Comm_Destroy(&(zhg->VtxPlan));
  }
  else {
    for (i = 0; i < zhg->nRecv_GNOs; i++)
      outparts[i] = hg_parts[zhg->Recv_GNOs[i]];
  }

  if (zz->LB.Remap_Flag) {
    int new_map;
    int *newproc = (int *) ZOLTAN_MALLOC(nObj * sizeof(int));
    int num_gid_entries = zz->Num_GID;

    if (nObj && !newproc) {
      ZOLTAN_PRINT_ERROR(zz->Proc, yo, "Memory error.");
      ierr = ZOLTAN_MEMERR;
      goto End;
    }
    for (i = 0; i < nObj; i++){
      newproc[i] = Zoltan_LB_Part_To_Proc(zz, outparts[i],
                                          &(zhg->GIDs[i*num_gid_entries]));
      if (newproc[i]<0){
        ZOLTAN_PRINT_ERROR(zz->Proc, yo,
         "Zoltan_LB_Part_To_Proc returned invalid processor number.");
        ierr = ZOLTAN_FATAL;
        ZOLTAN_FREE(&newproc);
        goto End;
      }
    }
    
    ierr = Zoltan_LB_Remap(zz, &new_map, nObj, newproc, zhg->Input_Parts,
                           outparts, 1);
    if (ierr < 0) 
      ZOLTAN_PRINT_ERROR(zz->Proc, yo, "Error returned from Zoltan_LB_Remap");
    ZOLTAN_FREE(&newproc);
  }

End:
  if (zhg->Recv_GNOs) ZOLTAN_FREE(&(zhg->Recv_GNOs));
  zhg->nRecv_GNOs = 0;
  return ierr;
}

/****************************************************************************/

static int Zoltan_PHG_Return_Lists (
  ZZ *zz,
  ZHG *zhg,
  int *num_exp,
  ZOLTAN_ID_PTR *exp_gids,
  ZOLTAN_ID_PTR *exp_lids,
  int **exp_procs,
  int **exp_to_part)
{
/* Routine to build export lists of ZOLTAN_LB_FN. */
char *yo = "Zoltan_PHG_Return_Lists";
int i, j;
int ierr = ZOLTAN_OK;
int eproc;
int num_gid_entries   = zz->Num_GID;
int num_lid_entries   = zz->Num_LID;
int nObj              = zhg->nObj;
Partition input_parts = zhg->Input_Parts;
ZOLTAN_ID_PTR gids    = zhg->GIDs;
ZOLTAN_ID_PTR lids    = zhg->LIDs;
int *outparts         = zhg->Output_Parts; 

  if (zz->LB.Return_Lists == ZOLTAN_LB_NO_LISTS) 
    goto End;

  /* Count number of objects with new partitions or new processors. */
  *num_exp = 0;
  for (i = 0; i < nObj; i++) {
    eproc = Zoltan_LB_Part_To_Proc(zz, outparts[i], &gids[i*num_gid_entries]);
    if (outparts[i] != input_parts[i] || zz->Proc != eproc)
      (*num_exp)++;
  }

  /* Allocate memory for return lists. */
  if (*num_exp > 0) {
    if (!Zoltan_Special_Malloc(zz, (void**)exp_gids, *num_exp,
                               ZOLTAN_SPECIAL_MALLOC_GID)
     || !Zoltan_Special_Malloc(zz, (void**)exp_lids, *num_exp,
                               ZOLTAN_SPECIAL_MALLOC_LID)
     || !Zoltan_Special_Malloc(zz, (void**)exp_procs, *num_exp,
                               ZOLTAN_SPECIAL_MALLOC_INT)
     || !Zoltan_Special_Malloc(zz, (void**)exp_to_part, *num_exp,
                               ZOLTAN_SPECIAL_MALLOC_INT)) {
        Zoltan_Special_Free(zz,(void**)exp_gids,   ZOLTAN_SPECIAL_MALLOC_GID);
        Zoltan_Special_Free(zz,(void**)exp_lids,   ZOLTAN_SPECIAL_MALLOC_LID);
        Zoltan_Special_Free(zz,(void**)exp_procs,  ZOLTAN_SPECIAL_MALLOC_INT);
        Zoltan_Special_Free(zz,(void**)exp_to_part,ZOLTAN_SPECIAL_MALLOC_INT);
        ZOLTAN_PRINT_ERROR(zz->Proc, yo, "Memory error.");
        ierr = ZOLTAN_MEMERR;
        goto End;
     }

    for (j = 0, i = 0; i < nObj; i++) {
      eproc = Zoltan_LB_Part_To_Proc(zz, outparts[i], &gids[i*num_gid_entries]);
      if (outparts[i] != input_parts[i] || eproc != zz->Proc) {
        ZOLTAN_SET_GID(zz, &((*exp_gids)[j*num_gid_entries]),
                           &(gids[i*num_gid_entries]));
        if (num_lid_entries > 0)
          ZOLTAN_SET_LID(zz, &((*exp_lids)[j*num_lid_entries]),
                             &(lids[i*num_lid_entries]));
        (*exp_procs)  [j] = eproc;
        (*exp_to_part)[j] = outparts[i];
        j++;
      }
    }
  }

End:

  return ierr;
}

/****************************************************************************/

void Zoltan_PHG_HGraph_Print(
  ZZ *zz,          /* the Zoltan data structure */
  ZHG *zoltan_hg,
  HGraph *hg,
  Partition parts, 
  FILE *fp
)
{
/* Printing routine. Can be used to print a Zoltan_HGraph or just an HGraph.
 * Set zoltan_hg to NULL if want to print only an HGraph.
 * Lots of output; synchronized across processors, so is a bottleneck.
 */
  int i;
  int num_gid = zz->Num_GID;
  int num_lid = zz->Num_LID;
  char *yo = "Zoltan_PHG_HGraph_Print";

  if (zoltan_hg != NULL  &&  hg != &zoltan_hg->HG) {
    ZOLTAN_PRINT_WARN(zz->Proc, yo, "Input hg != Zoltan HG");
    return;
  }

  Zoltan_Print_Sync_Start (zz->Communicator, 1);

  /* Print Vertex Info */
  fprintf (fp, "%s Proc %d\n", yo, zz->Proc);
  fprintf (fp, "Vertices (GID, LID, index)\n");
  for (i = 0; i < zoltan_hg->nObj; i++) {
    fprintf(fp, "(");
    ZOLTAN_PRINT_GID(zz, &zoltan_hg->GIDs[i * num_gid]);
    fprintf(fp, ", ");
    ZOLTAN_PRINT_LID(zz, &zoltan_hg->LIDs [i * num_lid]);
    fprintf(fp, ", %d)\n", i);
  }
  Zoltan_HG_Print(zz, hg, parts, fp, "Build");  
  Zoltan_Print_Sync_End(zz->Communicator, 1);
}

/*****************************************************************************/

int Zoltan_PHG_Set_2D_Proc_Distrib(
    ZZ *zz,                /* Input:  ZZ struct; for debuging   */
    MPI_Comm Communicator, /* Input:  The MPI Communicator; this communicator
                                      may be MPI_COMM_NULL, as PHG_Redistribute
                                      uses this function with MPI_COMM_NULL
                                      to compute nProc_x and nProc_y.  */
    int proc,              /* Input:  Rank of current processor */
    int nProc,             /* Input:  Total # of processors     */    
    int nProc_x,           /* Input:  Suggested #procs in x-direction */
    int nProc_y,           /* Input:  Suggested #procs in y-direction */
    PHGComm *comm          /* Ouput: filled */
    )    
{
/* Computes the processor distribution for the 2D data distrib.
 * Sets nProc_x, nProc_y.
 * Constraint:  nProc_x * nProc_y == nProc. 
 * For 2D data distrib, default should approximate sqrt(nProc).
 * If nProc_x and nProc_y both equal -1 on input, compute default.
 * Otherwise, compute valid values and/or return error.
 */
char *yo = "Zoltan_PHG_Set_2D_Proc_Distrib";
int tmp;
int ierr = ZOLTAN_OK;
    
  if (nProc_x == -1 && nProc_y == -1) {
    /* Compute default */
    tmp = (int) sqrt((double)nProc+0.1);
    while (nProc % tmp) tmp--;
    comm->nProc_x = tmp;
    comm->nProc_y = nProc / tmp;
  } else if (nProc_x == -1) {
    comm->nProc_y = MIN(nProc_y, nProc);
    comm->nProc_x = nProc / comm->nProc_y;
  } else if (nProc_y == -1) {
    comm->nProc_x = MIN(nProc_x, nProc);
    comm->nProc_y = nProc / comm->nProc_x;
  } else {
    comm->nProc_x = nProc_x;
    comm->nProc_y = nProc_y;    
  }
    
  /* Error check */
  if (comm->nProc_x * comm->nProc_y != nProc) {
    ZOLTAN_PRINT_ERROR(proc, yo,
                       "Values for PHG_NPROC_X and PHG_NPROC_Y "
                       "do not evenly divide the "
                       "total number of processors.");
    ierr = ZOLTAN_FATAL;
    goto End;
  }

  comm->nProc = nProc;
  comm->Communicator = Communicator;
  comm->zz = zz;

  if (Communicator==MPI_COMM_NULL) {
    comm->myProc_x = -1;
    comm->myProc_y = -1;
    comm->myProc = -1;
    comm->col_comm = comm->row_comm = MPI_COMM_NULL;
  } else {
    comm->myProc_x = proc % comm->nProc_x;
    comm->myProc_y = proc / comm->nProc_x;
    comm->myProc = proc;
    if ((MPI_Comm_split(Communicator, comm->myProc_x, comm->myProc_y, 
                        &comm->col_comm) != MPI_SUCCESS)
     || (MPI_Comm_split(Communicator, comm->myProc_y, comm->myProc_x, 
                        &comm->row_comm) != MPI_SUCCESS)) {
      ZOLTAN_PRINT_ERROR(proc, yo, "MPI_Comm_Split failed");
      return ZOLTAN_FATAL;
    }
    Zoltan_Srand_Sync(Zoltan_Rand(NULL), &(comm->RNGState_row),
                      comm->row_comm);
    Zoltan_Srand_Sync(Zoltan_Rand(NULL), &(comm->RNGState_col),
                      comm->col_comm);
    Zoltan_Srand_Sync(Zoltan_Rand(NULL), &(comm->RNGState),
                      comm->Communicator);
  } 
/*  printf("(%d, %d) of [%d, %d] -> After Comm_split col_comm=%d  row_comm=%d\n", hgp->myProc_x, hgp->myProc_y, hgp->nProc_x, hgp->nProc_y, (int)hgp->col_comm, (int)hgp->row_comm);  */
  

    
End:

  return ierr;
}


#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

