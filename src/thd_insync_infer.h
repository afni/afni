/* How to read this file
   Selection inference lifecycle and borrowed grid/sample-reader contract.
   Plan before allocating; begin once, evaluate selections, then finish.
   See thd_insync_infer.c for family and failure policies.
*/
#ifndef THD_INSYNC_INFER_H
#define THD_INSYNC_INFER_H
#include "thd_insync_select.h"
#include "thd_patterns.h"
typedef struct INSYNC_infer INSYNC_infer;
/* Borrowed source adapter: (source index, work unit, original zero-based TR). */
typedef float (*INSYNC_sample_reader)(const void *,int,int,int);
/* Borrowed analysis geometry. source_at[condition*nsub+subject] maps to
   source indices; gmember[group] lists gcount[group] subject indices. */
typedef struct {
   int nsub,ncond,ngroup,nwork,nvox,nthread,nvoxelwork,nroi;
   const int *source_at,*gcount,*widx,*roi_ids;
   int **gmember;
   char **clabel,**glabel;
   THD_roilist *rois;
} INSYNC_infer_grid;
/* Validate options, create draw descriptors and budget storage; NULL means no inference. */
INSYNC_infer *INSYNC_infer_plan(const INSYNC_options *,const INSYNC_schedule *,const INSYNC_infer_grid *);
/* Planned additional inference bytes; accepts NULL. */
double INSYNC_infer_memory(const INSYNC_infer *);
/* Allocate/open a successful plan before evaluating selections. */
void INSYNC_infer_begin(INSYNC_infer *);
/* Evaluate one selection with borrowed analysis and original-only readers.
   The second reader excludes fills: it supplies observed flags for subject
   inference and samples for temporal identity/null draws. NULL means all
   finite analysis samples are observed. The common mask is original-time. */
void INSYNC_infer_selection(INSYNC_infer *,int,int,const void *,INSYNC_sample_reader,INSYNC_sample_reader,const unsigned char *,THD_3dim_dataset *);
/* Finalize correction families, write products, and release the plan; index stream is borrowed. */
void INSYNC_infer_finish(INSYNC_infer *,FILE *);
#endif
