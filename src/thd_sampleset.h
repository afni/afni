#ifndef _THD_SAMPLESET_HEADER_
#define _THD_SAMPLESET_HEADER_

#include "mrilib.h"
#include "thd_datatable.h"

/*----------------------------------------------------------------------------
  A canonical one-row-per-sample view of a data table.  Input adapters such as
  thd_xmat expand their compact form before this object is built; direct and
  expanded tables therefore use exactly the same dataset, metadata, and row
  bookkeeping.

  Dataset selectors are resolved once.  dset[] holds distinct base datasets,
  while sample_dset[] and sample_brick[] locate every observation.  The table
  is deep-copied, so the caller retains ownership of the input table.
                                                     -- P Molfese, Oct 2026
------------------------------------------------------------------------------*/

typedef struct {
   const char *subject ;  /* NULL: all samples belong to one subject "All"  */
   const char *run ;      /* NULL: no run role                              */
   const char *item ;     /* NULL: no item/trial role                       */
   const char *fold ;     /* NULL: no explicit fold role                    */
   const char *sample ;   /* NULL: synthesize row000001, ...                 */
} THD_sampleset_columns ;

typedef struct {
   int nsample , ndset , nsubject , nrun , nitem , nfold , nvox ;

   THD_3dim_dataset **dset ;  /* [ndset], headers open, common grid          */
   char             **dset_name ;
   int *sample_dset ;         /* [nsample] index into dset                   */
   int *sample_brick ;        /* [nsample] brick of the base dataset        */

   int *subject , *run , *item , *fold ;  /* [nsample], -1 when absent      */
   char **subject_label , **run_label , **item_label , **fold_label ;

   char **sample_label ;      /* [nsample], always present and unique        */
   int   *table_row ;         /* [nsample], zero-based canonical table row   */

   THD_datatable *metadata ;  /* owned deep copy of the canonical table      */
   char *source ;             /* original table source for diagnostics       */
} THD_sampleset ;

/*! An owned snapshot of dataset/brick identities in one sample set.
    first_sample[i] is the earliest sample selecting the same resolved dataset
    path and brick as sample i (i itself for a first occurrence). Equal entries
    identify equivalent selections, including when comparing arbitrary subsets.
    These indices are meaningful only within this snapshot, not across maps.
    dataset_path follows the source sample set's dset[] order. */
typedef struct {
   int nsample , ndset ;
   int *first_sample ;        /* [nsample], zero-based representative row    */
   char **dataset_path ;      /* [ndset], owned resolved header paths        */
} THD_sampleset_identity ;

/*! Build an identity snapshot without loading voxel data or changing ss.
    Header paths are canonicalized with realpath(), resolving relative paths
    and symlinks; if resolution fails, the original header path is retained.
    Brick indices are the already-resolved base-dataset indices in ss.
    This detects repeated selections, not hard links, copied data, or broader
    scientific dependence. Repeated selections are permitted; callers decide
    whether to warn or reject them. The snapshot survives freeing ss, but must
    be rebuilt if its sample selections change. Returns NULL with an optional
    bounded error message on invalid input or allocation failure. */
extern THD_sampleset_identity * THD_sampleset_build_identity(
                                      const THD_sampleset *ss,
                                      char *err, int nerr ) ;
extern void THD_sampleset_free_identity( THD_sampleset_identity *identity ) ;

/*! Detect conventional role columns that are present: Subj, Run, Trial (or
    Item), Fold, and Sample.  Missing roles are left NULL. */
extern void THD_sampleset_detect_columns( const THD_datatable *dt,
                                          THD_sampleset_columns *columns ) ;

/*! Build and validate a sample set.  Every non-NULL role name must exist.
    InputFile is required and must select exactly one brick per row.  On
    failure, returns NULL and writes a concise reason to err when supplied. */
extern THD_sampleset * THD_sampleset_from_table(
                                      const THD_datatable *dt,
                                      const THD_sampleset_columns *columns,
                                      char *err, int nerr ) ;

/*! Encode any metadata column in first-appearance order.  The returned code
    and labels belong to the caller.  Use THD_sampleset_free_levels(). */
extern int THD_sampleset_encode_column( const THD_sampleset *ss,
                                        const char *column,
                                        int **code, char ***level, int *nlevel,
                                        char *err, int nerr ) ;
extern void THD_sampleset_free_levels( char **level, int nlevel ) ;

/*! Extract numeric metadata in sample order using ss->table_row.
    Delegates column validation and copying to THD_datatable_numeric_columns;
    the owned result has nsample rows and ncolumn columns in requested order.
    Output-row diagnostics refer to one-based sample positions; table rows
    refer to ss->metadata. No datasets are loaded or preprocessing fitted.
    Returns NULL with an optional bounded error on failure; free() on success. */
extern float * THD_sampleset_numeric_columns(
                                      const THD_sampleset *ss,
                                      const char * const *columns, int ncolumn,
                                      char *err, int nerr ) ;

/*! A deterministic audit table suitable for a future -show_samples option. */
extern void THD_sampleset_print( const THD_sampleset *ss, FILE *fp ) ;

extern void THD_sampleset_free( THD_sampleset *ss ) ;

#endif /* _THD_SAMPLESET_HEADER_ */
