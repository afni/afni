#ifndef SUMA_SUMA_DATASETS_INCLUDED
#define SUMA_SUMA_DATASETS_INCLUDED

#include "../suma_datasets.h"


#ifdef SUMA_COMPILED
   #undef SUMA_STANDALONE_INIT
   #define SUMA_STANDALONE_INIT {   \
      /* install signal handler, shamelessly copied from AFNI) */ \
      signal(SIGINT ,SUMA_sigfunc) ;      \
      signal(SIGBUS ,SUMA_sigfunc) ;   \
      signal(SIGSEGV,SUMA_sigfunc) ;   \
      signal(SIGTERM,SUMA_sigfunc) ;   \
      SUMA_process_environ(); \
         SUMAg_CF = SUMA_Create_CommonFields ();   \
	      if (SUMAg_CF == NULL) { \
		      fprintf(SUMA_STDERR,\
                  "Error %s: Failed in SUMA_Create_CommonFields\n", FuncName); \
		      exit(1); \
	      }  \
         /* SUMAg_CF->scm = SUMA_Build_Color_maps();  require X connection*/\
      SUMA_ParseInput_basics_s (argv, argc);   \
   }
#endif   

void SUMA_ParseInput_basics_s (char *argv[], int argc); 
int SUMA_WriteDset_NameCheck_s (char *Name, SUMA_DSET *dset, SUMA_DSET_FORMAT form, int verb, char **NameOutp); 
void WorkErrLog_s(void);
SUMA_DSET * SUMA_FindDset_s (char *idcode, DList *DsetList);
DListElmt * SUMA_FindDsetEl_s (char *idcode, DList *DsetList);
int SUMA_GetNodeIndex_FromNodeRow_s(SUMA_DSET *dset, int row, int N_Node);
int SUMA_GetNodeRow_FromNodeIndex_s(SUMA_DSET *dset, int node, int N_Node);
SUMA_DSET *SUMA_LoadDset_s (char *Name, SUMA_DSET_FORMAT *form, int verb);
char * SUMA_WriteDset_s (char *Name, SUMA_DSET *dset, SUMA_DSET_FORMAT form, int overwrite, int verb) ;
char *SUMA_RemoveDsetExtension_s (char*Name, SUMA_DSET_FORMAT form);
SUMA_DSET *SUMA_far2dset_s( char *FullName, char *dset_id, char *dom_id, 
                                 float **farp, int vec_len, int vec_num, 
                                 int ptr_cpy) ;
SUMA_DSET *SUMA_LoadDXDset_s (char *Name, int verb);
SUMA_DSET *SUMA_Load1DDset_s (char *oName, int verb);
float *SUMA_Load1D_s (char *oName, int *ncol, int *nrow, int RowMajor, int verb);
SUMA_Boolean  SUMA_is_ID_4_DSET(char *idcode, SUMA_DSET **dsetp);

#endif
