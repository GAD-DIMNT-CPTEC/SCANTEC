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
// !MODULE: SCAM_ModFuncTable                                          
//
// !DESCRIPTION:
//              Function table registeries for storing the interface 
//              implementations for managing routines used to read
//              different atmosferic models that will be evaluate by
//              SCAMTEC.
//
// !REVISION HISTORY:
//  15 Jun 2005 - J.G. de Mattos - Initial Version.
//  12 Apr 2016 - J.G. de Mattos - Change table entries to use linked 
//                                 list.
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
// para inicializar as matrizes dos modelos atmosfericos lidos
//
struct ModInitNode
{ 
  char *name;
  void (*func)( );

  struct ModInitNode* next;
} ;
struct ModInitNode* ModInitTable = NULL;

//
// Estrutura de dados da tabela que contem as rotinas utilizadas
// para ler os campos das simulacoes dos modelos atmosfericos lidos
//
struct ModReadNode
{ 
  char *name;
  void (*func)(char*, int slen);

  struct ModReadNode* next;
} ;
struct ModReadNode* ModReadTable = NULL;


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//BOP
// !ROUTINE: RegisterModelInit
//
// !DESCRIPTION:
//     Makes an entry in the registry for the routine used to initialize
//     atmospheric model reading routines.
//
// !INTERFACE:

void FTN(registermodelinit)(char *i, void(*func)( ), int len) {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//BOC

  struct ModInitNode* current;
  struct ModInitNode* pnode; 

  // create node
  
  pnode = (struct ModInitNode*) malloc(sizeof(struct ModInitNode));


  pnode->name = (char*) malloc(len*sizeof(char));
  strcpy(pnode->name,i);

  pnode->func = func;
  pnode->next = NULL; 

  if(ModInitTable == NULL){

    ModInitTable = pnode;

  }else{

    current = ModInitTable; 

    while(current->next!=NULL){
      current = current->next;
    }

    current->next = pnode;

  }

}
//EOC
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//BOP
// !ROUTINE: ModelInit
//
// !DESCRIPTION:
//    Invokes the routine from registry to perform the atmopheric
//    model matrices initialization
//
// !INTERFACE:
//

void FTN(scam_modelinit)( char *i){

  struct ModInitNode* current;
  int found ; 

  current = ModInitTable; // Return to begin of the list

  // find an atmospheric model in the linked list
  while(strcmp(current->name,i)!=0){

    current = current->next;

    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("init routine for Atm Model %s is not defined\n",i); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }

  }

  current->func();
  
}
//EOC
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//BOP
// !ROUTINE: RegisterModelRead
//
// !DESCRIPTION:
//     Makes an entry in the registry for the routine used to read
//     atmospheric model fields.
//
// !INTERFACE:

void FTN(registermodelread)(char *i, void(*func)( ), int len) {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//BOC

  struct ModReadNode* current;
  struct ModReadNode* pnode; 

  // create node
  
  pnode = (struct ModReadNode*) malloc(sizeof(struct ModReadNode));


  pnode->name = (char*) malloc(len*sizeof(char));
  strcpy(pnode->name,i);

  pnode->func = func;
  pnode->next = NULL; 

  if(ModReadTable == NULL){

    ModReadTable = pnode;

  }else{

    current = ModReadTable; 

    while(current->next!=NULL){
      current = current->next;
    }

    current->next = pnode;

  }

}
//EOC
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~//
//BOP
// !ROUTINE: ModelRead
//
// !DESCRIPTION:
//    Invokes the routine from registry for reading the atmopheric
//    model fields.
//
// !INTERFACE:
//

void FTN(scam_modelread)( char *i, char *name){

  struct ModReadNode* current;
  int found ; 

  current = ModReadTable; // Return to begin of the list

  // find an atmospheric model in the linked list
  while(strcmp(current->name,i)!=0){

    current = current->next;

    if(current==NULL) {
      printf("****************Error****************************\n"); 
      printf("Read routine for Atm Model %s is not defined\n",i); 
      printf("program will seg fault.....\n"); 
      printf("****************Error****************************\n"); 
    }

  }

  current->func(name,strlen(name));
  
}

