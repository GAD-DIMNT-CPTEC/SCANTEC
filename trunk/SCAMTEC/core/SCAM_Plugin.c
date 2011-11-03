#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>

#include<ftn_drv.h>


#define SCAMTEC_MAX_MODEL (20)

typedef struct
{ 
  void (*func)(int*, int*, int*, int*);
} SCAMTEC_MODEL_INI_TABLE;

SCAMTEC_MODEL_INI_TABLE load_data[SCAMTEC_MAX_MODEL];


void FTN(registermodelread)(int *i, void (*func)())

{ 
  ft_check_index(*i, SCAMTEC_MAX_MODEL, "registermodelread");
  load_data[*i].func = func; 
}



void FTN(load_data)(int *i, int *nymd, int *nhms, int *fymd, int *fhms)
{ 
  if(load_data[*i].func==NULL) {
    printf("****************Error****************************\n"); 
    printf("subroutine that read fields of the Model %d is not defined\n",*i); 
    printf("program will segfault.....\n"); 
    printf("****************Error****************************\n"); 
    exit;
  }

  load_data[*i].func(nymd, nhms, fymd, fhms); 

}
