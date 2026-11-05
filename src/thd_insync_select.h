#ifndef THD_INSYNC_SELECT_H
#define THD_INSYNC_SELECT_H
#include <stdio.h>
#include "thd_insync.h"
#include "thd_xmat.h"

/* M1/M2: source normalization and original-coordinate schedules. No sample reads.
   Objects own strings/arrays/source-table copies; caller's table is unmodified.
   Source and row indices are zero based internally; runs are one based. */
typedef struct {
   int row, ntime;
   double tr;
   char *subject, *condition, *group, *file;
} INSYNC_source;

typedef struct {
   int source, row, run, start, stop, original_start, original_stop;
   int window, selection_id, kept, own_kept;
   int stimulus, block, aggregate, parent_id, component_id;
   char *label, *segment;
} INSYNC_segment;

typedef struct {
   THD_datatable *sources;
   int nsource, nsegment, nselection, nrun, ntime;
   int *runstart;
   INSYNC_source *source;
   INSYNC_segment *segment;
   int short_segment_count, nrequested, requested_capacity;
   INSYNC_segment *requested;
   double tr;
   int xmat_mode, ndesign, nstim;
   THD_xmat **design, **xmat, **full;
   char **stimlabel;
   int *run_selected, *overlap, *stimordinal;
   unsigned char *keep;  /* original-coordinate source masks; common computed later */
} INSYNC_schedule;

/* Fatal input errors follow the command's ERROR_exit convention. */
int INSYNC_selection_requested(const INSYNC_options *, THD_datatable *);
INSYNC_schedule *INSYNC_schedule_build(const INSYNC_options *, THD_datatable *);
void INSYNC_schedule_free(INSYNC_schedule *);
void INSYNC_selection_token(FILE *,const char *);
int INSYNC_schedule_report(const INSYNC_options *, THD_datatable *, INSYNC_schedule *);
int INSYNC_check_table(const INSYNC_options *, THD_datatable *);
/* Strict decimal integer parser shared with CLI, rejects trailing text. */
void INSYNC_apply_censor(unsigned char *,int,const char *);
int INSYNC_selection_int(const char *, int, const char *);
#endif
