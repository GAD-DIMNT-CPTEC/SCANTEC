#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>
#include<string.h>

#include<ftn_drv.h>


#define SCAMTEC_MAX_OBS (20)

typedef struct
{ 
  void (*func)(char*, int slen);
} SCAMTEC_OBS_INI_TABLE;


SCAMTEC_OBS_INI_TABLE loadobs_data[SCAMTEC_MAX_OBS];

typedef struct
{ 
  void (*func)( );
} SCAMTEC_OBS_CONFIG_TABLE;


SCAMTEC_OBS_CONFIG_TABLE config_obs[SCAMTEC_MAX_OBS];



void FTN(registerobsread)(int *i, void (*func)( ))

{ 
  ft_check_obs_index(*i, SCAMTEC_MAX_OBS, "registerobsread");
  loadobs_data[*i].func = func; 
}


void FTN(loadobs_data)( int *i, char *name)
{ 

/*
  if(loadobs_data[*i].func==NULL) {
    printf("****************Error****************************\n"); 
    printf("subroutine that read fields of the Obs %d is not defined\n",*i); 
    printf("program will segfault.....\n"); 
    printf("****************Error****************************\n"); 
    exit;
  }

  loadobs_data[*i].func(name,strlen(name));
*/

  if(config_obs[*i].func==NULL) {
    printf("****************Error****************************\n"); 
    printf("subroutine that read fields of the Obs %d is not defined\n",*i); 
    printf("program will segfault.....\n"); 
    printf("****************Error****************************\n"); 
    exit;
  }

  config_obs[*i].func(name,strlen(name));


}

/* --------------------------------------- */

void FTN(registerobsinit)(int *i, void (*func)())

{ 
  ft_check_obs_index(*i, SCAMTEC_MAX_OBS, "registerobsinit");
  config_obs[*i].func = func;
  printf("iniciando registerobsinit\n"); 
}


void FTN(config_obs)(int *i)
{ 


  if(config_obs[*i].func==NULL) {
    printf("****************Error****************************\n"); 
    printf("subroutine that configure obs %d is not defined\n",*i); 
    printf("program will segfault.....\n"); 
    printf("****************Error****************************\n"); 
    exit;
  }

  config_obs[*i].func( ); 

}
/**/
