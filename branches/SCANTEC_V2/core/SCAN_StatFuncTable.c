//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//                  SCAMTeC - GDAD/CPTEC/INPE - 2010                   //
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
// Copyright 2010 Free Software Foundation, Inc.                       //
//                                                                     //
// This program is free software; you can redistribute it and/or modify//
// it under the terms of the GNU General Public License as published by//
// the Free Software Foundation; either version 2 of the License, or   //
// (at your option) any later version.                                 //
//                                                                     //
// This program is distributed in the hope that it will be useful,     //
// but WITHOUT ANY WARRANTY; without even the implied warranty of      //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       //
// GNU General Public License for more details.                        //
//                                                                     //
// You should have received a copy of the GNU General Public License   //
// along with GNU Emacs; see the file COPYING.  If not, write to the   //
// Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,    //
// Boston, MA 02110-1301, USA.                                         //
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

//BOP
//
// !MODULE: SCAM_stats_FTable
//  
// !DESCRIPTION:
//  Function table registries for storing the interface
//  implementations for the operation of statistics  
//  algorithms.
//
//EOP
#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>

#include "ftn_drv.h"
struct statsetupnode
{ 
  char *name;
  void (*func)(int*);

  struct statsetupnode* next;
} ;
struct statsetupnode* dasetup_table = NULL; 

struct statinitnode
{ 
  char *name;
  void (*func)();

  struct statinitnode* next;
} ;
struct statinitnode* statinit_table = NULL; 

struct statcompincrnode
{ 
  char *name;
  void (*func)(int*, int*);

  struct statcompincrnode* next;
} ;
struct statcompincrnode* dacompincr_table = NULL; 

struct statapplyincrnode
{ 
  char *name;
  void (*func)(int*, int*);

  struct statapplyincrnode* next;
} ;
struct statapplyincrnode* daapplyincr_table = NULL; 

struct statoutnode
{ 
  char *name;
  void (*func)(int*, int*);

  struct statoutnode* next;
} ;
struct statoutnode* daout_table = NULL; 

struct statfinalnode
{ 
  char *name;
  void (*func)();

  struct statfinalnode* next;
} ;
struct statfinalnode* dafinal_table = NULL; 

struct statobssetnode
{ 
  char *name;
  void (*func)(int*, void*, void*);

  struct statobssetnode* next;
} ;
struct statobssetnode* daobsset_table = NULL; 

struct statobsreadnode
{ 
  char *name;
  void (*func)(int*, void*, void*);

  struct statobsreadnode* next;
} ;
struct statobsreadnode* daobsread_table = NULL; 

struct statobswritenode
{ 
  char *name;
  void (*func)(int*, void*);

  struct statobswritenode* next;
} ;
struct statobswritenode* daobswrite_table = NULL; 

struct statgetnsonode
{ 
  char *name;
  void (*func)(int*, int*, int*, int*);

  struct statgetnsonode* next;
} ;
struct statgetnsonode* dagetnso_table = NULL; 

struct statobsfinalnode
{ 
  char *name;
  void (*func)();

  struct statobsfinalnode* next;
} ;
struct statobsfinalnode* daobsfinal_table = NULL; 

//BOP
// !ROUTINE: registerstatinit
// \label{registerstatinit}
//  
// !DESCRIPTION: 
// Creates an entry in the registry for the routine to 
// intialize algorithm specific structures for 
// the statistic method used. 
//
// The arguments are: 
// \begin{description}
//  \item[j]
//   name of the statistics algorithm
//  \end{description}
// 
// !INTERFACE:
void FTN(registerstatinit)(char *j, void (*func)(),int len)
//EOP
{ 
  struct statinitnode* current;
  struct statinitnode* pnode; 
  // create node
  
  pnode=(struct statinitnode*) malloc(sizeof(struct statinitnode));
  pnode->name=(char*) malloc(len*sizeof(char));
  strcpy(pnode->name,j);
  pnode->func = func;
  pnode->next = NULL; 

  if(statinit_table == NULL){
    statinit_table = pnode;
  }
  else{
    current = statinit_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: statisticsinit
// \label{statisticsinit}
//
// !INTERFACE:
void FTN(statisticsinit)(char *j,int len)
//  
// !DESCRIPTION: 
// Invokes the routine from the registry to initialize
// the specific structures for the 
// statistics algorithm used. 
// 
// The arguments are: 
// \begin{description}
//  \item[j]
//   name of the statistics algorithm
//  \end{description}
// 
//EOP
{
  struct statinitnode* current;
  
  current = statinit_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("init routine for %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(); 
}

//BOP
// !ROUTINE: registerstatsetup
// \label{registerstatsetup}
//  
// !DESCRIPTION: 
// Creates an entry in the registry for the routine to 
// set up algorithm specific structures for 
// the statistics method used. 
//
// The arguments are: 
// \begin{description}
//  \item[j]
//   name of the statistics algorithm
//  \end{description}
// 
// !INTERFACE:
void FTN(registerstatsetup)(char *j, void (*func)(int*),int len)
//EOP
{ 
  struct statsetupnode* current;
  struct statsetupnode* pnode; 
  // create node
  
  pnode=(struct statsetupnode*) malloc(sizeof(struct statsetupnode));
  pnode->name=(char*) malloc(len*sizeof(char));
  strcpy(pnode->name,j);
  pnode->func = func;
  pnode->next = NULL; 

  if(dasetup_table == NULL){
    dasetup_table = pnode;
  }
  else{
    current = statsetup_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: statisticssetup
// \label{statisticssetup}
//
// !INTERFACE:
void FTN(statisticssetup)(char *j, int *k,int len)
//  
// !DESCRIPTION: 
// Invokes the routine from the registry to setup
// the specific structures for the 
// data statistics algorithm used. 
// 
// The arguments are: 
// \begin{description}
//  \item[j]
//   name of the statistics algorithm
//  \item[k]
//   index of the statistic instance
//  \end{description}
// 
//EOP
{
  struct statsetupnode* current;
  
  current = statsetup_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("setup routine for %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(k); 
}


//BOP
// !ROUTINE: registerapplyincrements
// \label{registerapplyincrements}
//  
// !INTERFACE:
void FTN(registerapplyincrements)(char *j, void (*func)(int*, int*),int len)
// !DESCRIPTION: 
// Creates an entry in the registry for the routine that
// applies the analysis increments
//
// The arguments are: 
// \begin{description}
//  \item[j]
//   name of the assimilation algorithm
//  \end{description}
//  
//EOP
{ 
  struct daapplyincrnode* current;
  struct daapplyincrnode* pnode; 
  // create node
  
  pnode=(struct daapplyincrnode*) malloc(sizeof(struct daapplyincrnode));
  pnode->name=(char*) malloc(len*sizeof(char));
  strcpy(pnode->name,j);
  pnode->func = func;
  pnode->next = NULL; 

  if(daapplyincr_table == NULL){
    daapplyincr_table = pnode;
  }
  else{
    current = daapplyincr_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: applyincrements
// \label{applyIncrements}
//  
// !INTERFACE:
void FTN(applyincrements)(char *j, int *n, int *k,int len)
// !DESCRIPTION:
// Invoke the routine from the registry that applies
// analysis increments
//
// The arguments are: 
// \begin{description}
//  \item[j]
//   name of the assimilation algorithm
//  \item[n]
//   index of the nest
//  \item[k]
//   index of the assimilation instance
//  \end{description}
//  
//EOP
{
  struct daapplyincrnode* current;
  
  current = daapplyincr_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("apply increments routine for %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n,k); 
}


//BOP
// !ROUTINE: registercomputeincrements
// \label{registercomputeincrements}
//  
// !INTERFACE:
void FTN(registercomputeincrements)(char *j, void (*func)(int*, int*),int len)
// !DESCRIPTION: 
// Creates an entry in the registry for the routine that
// computes the analysis increments
//
// The arguments are: 
// \begin{description}
//  \item[j]
//   name of the assimilation algorithm
//  \end{description}
//  
//EOP
{ 
  struct dacompincrnode* current;
  struct dacompincrnode* pnode; 
  // create node
  
  pnode=(struct dacompincrnode*) malloc(sizeof(struct dacompincrnode));
  pnode->name=(char*) malloc(len*sizeof(char));
  strcpy(pnode->name,j);
  pnode->func = func;
  pnode->next = NULL; 

  if(dacompincr_table == NULL){
    dacompincr_table = pnode;
  }
  else{
    current = dacompincr_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: computeincrements
// \label{computeIncrements}
//  
// !INTERFACE:
void FTN(computeincrements)(char *j, int *n, int *k,int len)
// !DESCRIPTION:
// Invoke the routine from the registry that computes 
// analysis increments
//
// The arguments are: 
// \begin{description}
//  \item[j]
//   name of the assimilation algorithm
//  \item[n]
//   index of the nest
//  \item[k]
//   index of assimilation instance
//  \end{description}
//  
//EOP
{
  struct dacompincrnode* current;
  
  current = dacompincr_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("compute increments routine for %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n,k); 
}

//BOP
// !ROUTINE: registerdafinalize
// \label{registerdafinalize}
//  
//  
// !INTERFACE:
void FTN(registerdafinalize)(char *j, void (*func)(),int len)
// !DESCRIPTION: 
//  Cretes an entry in the registry for the routine 
//  to cleanup allocated structures
//
// The arguments are: 
// \begin{description}
//  \item[j]
//   name of the assimilation algorithm
//  \end{description}
//EOP
{ 
  struct dafinalnode* current;
  struct dafinalnode* pnode; 
  // create node
  
  pnode=(struct dafinalnode*) malloc(sizeof(struct dafinalnode));
  pnode->name=(char*) malloc(len*sizeof(char));
  strcpy(pnode->name,j);
  pnode->func = func;
  pnode->next = NULL; 

  if(dafinal_table == NULL){
    dafinal_table = pnode;
  }
  else{
    current = dafinal_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: dafinalize
// \label{dafinalize}
// 
// !INTERFACE:
void FTN(dafinalize)(char *j,int len)
// !DESCRIPTION: 
// Invokes the routine from the registry that cleans up
// the data assimilation specific structures
// 
// The arguments are: 
// \begin{description}
//  \item[j]
//   name of the assimilation algorithm 
//  \end{description}
// 
//EOP
{ 
  struct dafinalnode* current;
  
  current = dafinal_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("finalize routine for %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(); 
}


//BOP
// !ROUTINE: registerdaoutput
// \label{registerdaoutput}
//  
//  
// !INTERFACE:
void FTN(registerdaoutput)(char *j, void (*func)(int*, int*),int len)
// !DESCRIPTION: 
//  Cretes an entry in the registry for the 
//  writing diagnostic output from the data
//  assimilation routines
//
// The arguments are: 
// \begin{description}
//  \item[j]
//   name of the assimilation algorithm
//  \end{description}
//EOP
{ 
  struct daoutnode* current;
  struct daoutnode* pnode; 
  // create node
  
  pnode=(struct daoutnode*) malloc(sizeof(struct daoutnode));
  pnode->name=(char*) malloc(len*sizeof(char));
  strcpy(pnode->name,j);
  pnode->func = func;
  pnode->next = NULL; 

  if(daout_table == NULL){
    daout_table = pnode;
  }
  else{
    current = daout_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: daoutput
// \label{daoutput}
// 
// !INTERFACE:
void FTN(daoutput)(char *j,int *n, int *k,int len)
// !DESCRIPTION: 
// Invokes the routine from the registry to write 
// data assimilation specific output
// 
// The arguments are: 
// \begin{description}
//  \item[j]
//   name of the assimilation algorithm 
//  \item[n]
//   index of the nest
//  \item[k]
//   index of the assimilation instance
//  \end{description}
// 
//EOP
{ 
  struct daoutnode* current;
  
  current = daout_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("Output routine for  %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n,k); 
}
//BOP
// !ROUTINE: registergetnso
// \label{registergetnso}
//
// !INTERFACE:
void FTN(registergetnso)(char *j, void (*func)(int*, int*, int*, int*),int len)
//  
// !DESCRIPTION: 
// Creates an entry in the registry for the routine that
// computes the number of selected observations for the 
// specified grid point. 
// 
// The arguments are: 
// \begin{description}
//  \item[j]
//   index of the observations variable
//  \end{description}
//EOP
{ 
  struct dagetnsonode* current;
  struct dagetnsonode* pnode; 
  // create node
  
  pnode=(struct dagetnsonode*) malloc(sizeof(struct dagetnsonode));
  pnode->name=(char*) malloc(len*sizeof(char));
  strcpy(pnode->name,j);
  pnode->func = func;
  pnode->next = NULL; 

  if(dagetnso_table == NULL){
    dagetnso_table = pnode;
  }
  else{
    current = dagetnso_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: getselectedobsnumber 
// \label{getselectedobsnumber}
//
// !INTERFACE: 
void FTN(getselectedobsnumber)(char *j, int *n, int *gid, int *sid, int *eid,int len)
//
// !DESCRIPTION:
// Invokes the routine from the registry that computes
// the number of selected observations for a particular modeling
// point
// 
// The arguments are: 
// \begin{description}
//  \item[j]
//   index of the observations variable
//  \item[n]
//   index of the nest
//  \item[gid]
//   model grid point index
//  \item[sid]
//   starting index of the selected observations
//  \item[eid]
//   ending index of the selected observations
//  \end{description}
//
//EOP
{ 
  struct dagetnsonode* current;
  
  current = dagetnso_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("getNSO (number of selected observations) routine for %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n,gid,sid,eid); 
}

//BOP
// !ROUTINE: registerreaddaobssetup
// \label{registerrdaobssetup}
//  
// 
// !INTERFACE:
void FTN(registerdaobssetup)(char *j, void (*func)(int*, void*, void*),int len)
//
// !DESCRIPTION: 
//  Creates an entry in the registry for the routine that
//  sets up structures for handling observation data for 
//  data assimilation
//
// The arguments are: 
// \begin{description}
//  \item[j]
//   index of the assimilation set
//  \end{description}
//EOP
{ 
  struct daobssetnode* current;
  struct daobssetnode* pnode; 
  // create node
  
  pnode=(struct daobssetnode*) malloc(sizeof(struct daobssetnode));
  pnode->name=(char*) malloc(len*sizeof(char));
  strcpy(pnode->name,j);
  pnode->func = func;
  pnode->next = NULL; 

  if(daobsset_table == NULL){
    daobsset_table = pnode;
  }
  else{
    current = daobsset_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: readobsdataconfig
// \label{readobsdataconfig}
// 
// !INTERFACE:
void FTN(readobsdataconfig)(char *j, int *k, void *obs, void *pert,int len)
//  
// !DESCRIPTION: 
// Invokes the routine from the registry to set up 
// structures for handling observation data for data
// assimilation 
//
// The arguments are: 
// \begin{description}
//  \item[j]
//   index of the variable being assimilated
//  \item[k]
//   index of the DA instance
//  \item[obs]
//   ESMF state that contain the observations
//  \item[pert]
//   ESMF state that contain the observation perturbations
//  \end{description}
//
//EOP
{ 
  struct daobssetnode* current;
  
  current = daobsset_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("setup routine for DA obs %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(k,obs,pert); 
}

//BOP
// !ROUTINE: registerreaddaobs
// \label{registerreaddaobs}
//
// !INTERFACE:
void FTN(registerreaddaobs)(char *j, void (*func)(int*, void*, void*),int len)
//  
// !DESCRIPTION: 
// Creates an entry in the registry for the routine that 
// reads observations for data assimilation. 
// 
// The arguments are: 
// \begin{description}
//  \item[j]
//   index of the observations variable
//  \end{description}
//EOP
{ 
  struct daobsreadnode* current;
  struct daobsreadnode* pnode; 
  // create node
  
  pnode=(struct daobsreadnode*) malloc(sizeof(struct daobsreadnode));
  pnode->name=(char*) malloc(len*sizeof(char));
  strcpy(pnode->name,j);
  pnode->func = func;
  pnode->next = NULL; 

  if(daobsread_table == NULL){
    daobsread_table = pnode;
  }
  else{
    current = daobsread_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: readdaobs
// \label{readdaobs}
//
// !INTERFACE: 
void FTN(readdaobs)(char *j,int *n, void *obsstate,void *obspertstate, int len)
//
// !DESCRIPTION:
// Invokes the routine from the registry to read 
// observations for data assimilation. This routine
// also packages the observations into an ESMF state
// 
// The arguments are: 
// \begin{description}
//  \item[j]
//   index of the observations variable
//  \item[n]
//   index of the nest
//  \item[obsstate]
//   pointer to the ESMF observation state
//  \item[obspertstate]
//   pointer to the ESMF observation perturbation state
//  \end{description}
//
//EOP
{ 
  struct daobsreadnode* current;
  
  current = daobsread_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("read DA obs routine for %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n,obsstate,obspertstate); 
}
//BOP
// !ROUTINE: registerwritedaobs
// \label{registerwritedaobs}
//
// !INTERFACE:
void FTN(registerwritedaobs)(char *j, void (*func)(int*, void*),int len)
//  
// !DESCRIPTION: 
// Creates an entry in the registry for the routine that 
// writes observations used in data assimilation to disk. 
// 
// The arguments are: 
// \begin{description}
//  \item[j]
//   index of the observations variable
//  \end{description}
//EOP
{ 
  struct daobswritenode* current;
  struct daobswritenode* pnode; 
  // create node
  
  pnode=(struct daobswritenode*) malloc(sizeof(struct daobswritenode));
  pnode->name=(char*) malloc(len*sizeof(char));
  strcpy(pnode->name,j);
  pnode->func = func;
  pnode->next = NULL; 

  if(daobswrite_table == NULL){
    daobswrite_table = pnode;
  }
  else{
    current = daobswrite_table; 
    while(current->next!=NULL){
      current = current->next;
    }
    current->next = pnode; 
  }
}
//BOP
// !ROUTINE: writedaobs
// \label{writedaobs}
//
// !INTERFACE: 
void FTN(writedaobs)(char *j,int *n, void *obsstate,int len)
//
// !DESCRIPTION:
// Invokes the routine from the registry to write
// observations used in data assimilation to disk.
// 
// The arguments are: 
// \begin{description}
//  \item[j]
//   index of the observations variable
//  \item[n]
//   index of the nest
//  \item[obsstate]
//   pointer to the ESMF observation state
//  \end{description}
//
//EOP
{ 
  struct daobswritenode* current;
  
  current = daobswrite_table;
  while(strcmp(current->name,j)!=0){
    current = current->next;
    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("write DA obs routine for %s is not defined\n",j); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }
  }
  current->func(n,obsstate); 
}




