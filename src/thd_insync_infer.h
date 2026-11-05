#ifndef THD_INSYNC_INFER_H
#define THD_INSYNC_INFER_H
#include "thd_insync_select.h"
#include "thd_patterns.h"
typedef struct INSYNC_infer INSYNC_infer;
typedef float (*INSYNC_sample_reader)(const void *,int,int,int);
typedef struct {
   int nsub,ncond,ngroup,nwork,nvox,nthread,nvoxelwork,nroi;
   const int *source_at,*gcount,*widx,*roi_ids;
   int **gmember;
   char **clabel,**glabel;
   THD_roilist *rois;
} INSYNC_infer_grid;
INSYNC_infer *INSYNC_infer_plan(const INSYNC_options *,const INSYNC_schedule *,const INSYNC_infer_grid *);
double INSYNC_infer_memory(const INSYNC_infer *);
void INSYNC_infer_begin(INSYNC_infer *);
void INSYNC_infer_selection(INSYNC_infer *,int,int,const void *,INSYNC_sample_reader,const unsigned char *,THD_3dim_dataset *);
void INSYNC_infer_finish(INSYNC_infer *,FILE *);
#endif
