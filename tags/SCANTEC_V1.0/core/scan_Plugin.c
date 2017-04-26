#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>
#include<string.h>

#include<ftn_drv.h>

#define scantec_MAX_MODEL (20)

void ft_check_index(int index, int max, char * regrtn);

typedef struct
{ 
  void (*func)(char*, int slen);
} scantec_MODEL_INI_TABLE;


scantec_MODEL_INI_TABLE load_data[scantec_MAX_MODEL];

typedef struct
{ 
  void (*func)( );
} scantec_MODEL_CONFIG_TABLE;


scantec_MODEL_CONFIG_TABLE config_model[scantec_MAX_MODEL];



void FTN(registermodelread)(int *i, void (*func)( ))

{ 
  ft_check_index(*i, scantec_MAX_MODEL, "registermodelread");
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
  ft_check_index(*i, scantec_MAX_MODEL, "registermodelinit");
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
