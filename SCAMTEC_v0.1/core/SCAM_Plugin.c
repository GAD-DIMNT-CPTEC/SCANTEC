#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>
#include<string.h>

#include<ftn_drv.h>


#define SCAMTEC_MAX_MODEL (20)

typedef struct
{ 
  void (*func)(char*, int slen);
} SCAMTEC_MODEL_INI_TABLE;


SCAMTEC_MODEL_INI_TABLE load_data[SCAMTEC_MAX_MODEL];

typedef struct
{ 
  void (*func)( );
} SCAMTEC_MODEL_CONFIG_TABLE;


SCAMTEC_MODEL_CONFIG_TABLE config_model[SCAMTEC_MAX_MODEL];



void FTN(registermodelread)(int *i, void (*func)( ))

{ 
  ft_check_index(*i, SCAMTEC_MAX_MODEL, "registermodelread");
  load_data[*i].func = func; 
}


void FTN(load_data)( int *i, char *name)
{ 


  if(load_data[*i].func==NULL) {
    printf("****************Error****************************\n"); 
    printf("subroutine that read fields of the Model %d is not defined\n",*i); 
    printf("program will segfault.....\n"); 
    printf("****************Error****************************\n"); 
    exit;
  }

  load_data[*i].func(name,strlen(name));

}

/* --------------------------------------- */

void FTN(registermodelinit)(int *i, void (*func)())

{ 
  ft_check_index(*i, SCAMTEC_MAX_MODEL, "registermodelinit");
  config_model[*i].func = func; 
}


void FTN(config_model)(int *i)
{ 


  if(config_model[*i].func==NULL) {
    printf("****************Error****************************\n"); 
    printf("subroutine that configure model %d is not defined\n",*i); 
    printf("program will segfault.....\n"); 
    printf("****************Error****************************\n"); 
    exit;
  }

  config_model[*i].func( ); 

}
/**/
