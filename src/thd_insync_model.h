#ifndef THD_INSYNC_MODEL_H
#define THD_INSYNC_MODEL_H
#include "thd_insync_select.h"
/* Selection model export: validation and files, no fitting or BOLD reads. */
typedef struct {
   char directory[THD_MAX_NAME],source_directory[THD_MAX_NAME];
   const INSYNC_options *options;
   const INSYNC_schedule *schedule;
   THD_datatable *table;
   int nsub,ncond,ncov,*covariates;
   const int *sources;
   char **conditions;
   FILE *index,*master;
} INSYNC_model;
INSYNC_model *INSYNC_model_preflight(const INSYNC_options *,THD_datatable *,const INSYNC_schedule *,int,int,const int *,char **);
void INSYNC_model_begin(INSYNC_model *);
void INSYNC_model_job(INSYNC_model *,int,int,int,THD_3dim_dataset *,float **,float *,float **,int,int,const int *);
void INSYNC_model_finish(INSYNC_model *);
#endif
