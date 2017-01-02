//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//                   SCAMTEC - CPTEC/INPE - 2010-2016                  //
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~// 
//                       Release License                               //
//                                                                     //
//  Released into the public domain.                                   //
//  This work is free: you can redistribute it and/or modify it under  //
//  the terms of Creative Commons BY-NC-SA 4.0 International License.  //
//                                                                     //
//  To view a copy of this license, visit:                             //
//                                                                     //
//  https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode        //
//                                                                     //
//  or send a letter to Creative Commons, 171 Second Street, Suite 300,//
//  San Francisco, California, 94105, USA.                             //
//                                                                     //
// This program is distributed in the hope that it will be useful,     //
// but WITHOUT ANY WARRANTY; without even the implied warranty of      //
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       //
// CC BY-NC-SA 4.0 International License for more details.             //
//                                                                     //
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//BOP
// !MODULE: SCAM_ObsFuncTable                                          
//
// !DESCRIPTION:
//              Function table registeries for storing the interface 
//              implementations for managing routines used to read
//              different Observation Tyapes that will be used to evaluate 
//              by evaluate atmospheric models by SCAMTEC.
//
// !REVISION HISTORY:
//  12 Apr 2016 - J.G. de Mattos - Initial Version.
//
//EOP
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//EOC

#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>
#include<string.h>
#include "ftn_drv.h"

//
// Estrutura de dados da tabela que contem as rotinas utilizadas
// para inicializar os vetores dos tipos de observacao lidos pelo SCAMTEC
//
struct ObsInitNode
{ 
  char *name;
  void (*func)( );

  struct ObsInitNode* next;
} ;
struct ObsInitNode* ObsInitTable = NULL;

//
// Estrutura de dados da tabela que contem as rotinas utilizadas
// para ler os diferentes tipos de observacao utilizados pelo SCAMTEC
// para avaliar as simulacoes dos modelos atmosfericos
//
struct ObsReadNode
{ 
  char *name;
  void (*func)(char*, int slen);

  struct ObsReadNode* next;
} ;
struct ObsReadNode* ObsReadTable = NULL;


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//BOP
// !ROUTINE: RegisterObsInit
//
// !DESCRIPTION:
//     Makes an entry in the registry for the routine used to initialize
//     observation type reading routines.
//
// !INTERFACE:

void FTN(registerobsinit)(char *i, void(*func)( ), int len) {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//BOC

  struct ObsInitNode* current;
  struct ObsInitNode* pnode; 

  // create node
  
  pnode = (struct ObsInitNode*) malloc(sizeof(struct ObsInitNode));


  pnode->name = (char*) malloc(len*sizeof(char));
  strcpy(pnode->name,i);

  pnode->func = func;
  pnode->next = NULL; 

  if(ObsInitTable == NULL){

    ObsInitTable = pnode;

  }else{

    current = ObsInitTable; 

    while(current->next!=NULL){
      current = current->next;
    }

    current->next = pnode;

  }

}
//EOC
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//BOP
// !ROUTINE: ObsInit
//
// !DESCRIPTION:
//    Invokes the routine from registry to perform the observation type
//    vectors initialization
//
// !INTERFACE:
//

void FTN(obsinit)( char *i){

  struct ObsInitNode* current;
  int found ; 

  current = ObsInitTable; // Return to begin of the list

  // find an obs type in the linked list
  while(strcmp(current->name,i)!=0){

    current = current->next;

    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("init routine for Obs Type %s is not defined\n",i); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }

  }

  current->func();
  
}
//EOC
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//BOP
// !ROUTINE: RegisterObsRead
//
// !DESCRIPTION:
//     Makes an entry in the registry for the routine used to read
//     an observation type vector.
//
// !INTERFACE:

void FTN(registerobsread)(char *i, void(*func)( ), int len) {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//BOC

  struct ObsReadNode* current;
  struct ObsReadNode* pnode; 

  // create node
  
  pnode = (struct ObsReadNode*) malloc(sizeof(struct ObsReadNode));


  pnode->name = (char*) malloc(len*sizeof(char));
  strcpy(pnode->name,i);

  pnode->func = func;
  pnode->next = NULL; 

  if(ObsReadTable == NULL){

    ObsReadTable = pnode;

  }else{

    current = ObsReadTable; 

    while(current->next!=NULL){
      current = current->next;
    }

    current->next = pnode;

  }

}
//EOC
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//BOP
// !ROUTINE: ObsRead
//
// !DESCRIPTION:
//    Invokes the routine from registry for reading the observation type
//    vector.
//
// !INTERFACE:
//

void FTN(obsread)( char *i, char *name){

  struct ObsReadNode* current;
  int found ; 

  current = ObsReadTable; // Return to begin of the list

  // find an observation type in the linked list
  while(strcmp(current->name,i)!=0){

    current = current->next;

    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("Read routine for Obs Type %s is not defined\n",i); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }

  }

  current->func(name,strlen(name));
  
}

