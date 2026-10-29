#ifndef _THD_PATTERNS_HEADER_
#define _THD_PATTERNS_HEADER_

#include "thd_sampleset.h"
#include <stddef.h>

/*----------------------------------------------------------------------------
  Turning datasets into the feature vectors that pattern analyses run on.

  Every multi-voxel method -- RSA, classification, encoding models -- starts
  the same way: pick a set of voxels, and reduce each item (a subject, a
  condition, a trial) to one vector over those voxels.  This file holds that
  step, separated from what is done with the vectors afterwards, so that the
  same code serves an atlas ROI and a searchlight sphere.

  "Pattern" rather than "feature" deliberately: FEAT means something else
  entirely in neuroimaging.
                                                     -- P Molfese, Jul 2026
------------------------------------------------------------------------------*/

/*! The voxels of one unit of analysis (an atlas parcel or a searchlight
    sphere), plus a label and an optional center. */
typedef struct {
   int     nroi ;
   int    *val ;    /* [nroi] atlas value, or center voxel/mesh-node ID         */
   intvec *vox ;    /* [nroi] volume voxels or surface dataset rows; .nar is count */
   char  **lab ;    /* [nroi] label from the atlas label table, or NULL */
   int    *center ; /* [nroi] dataset row to paint; -1 means paint the whole ROI */
} THD_roilist ;

/*! Every distinct positive value in the mask becomes an ROI.  'roi_sel' is an
    optional AFNI int-list ("1,3,7..12") restricting which values are kept. */
extern THD_roilist * THD_roilist_from_dset( THD_3dim_dataset *mset , char *roi_sel ) ;

/*! Every non-zero voxel of the mask becomes the center of a searchlight, whose
    voxels are the in-mask voxels reached by the neighborhood 'nbhd' (an
    MCW_cluster of i/j/k offsets, as built by MCW_spheremask / MCW_rectmask /
    MCW_rhddmask / MCW_tohdmask -- the same neighborhoods 3dLocalstat's -nbhd
    uses).  center[] holds the center voxel, so the output paints one value per
    center -- a proper statistical map rather than parcel fills. */
extern THD_roilist * THD_roilist_searchlight( THD_3dim_dataset *mset ,
                                              MCW_cluster *nbhd ) ;

/*! Parse the same SPHERE/RECT/RHDD/TOHD neighborhood grammar accepted by
    3dLocalstat.  A bare positive number is shorthand for SPHERE(number).
    Returns NULL on failure and writes a bounded caller-owned diagnostic. */
extern MCW_cluster * THD_searchlight_parse( const char *spec,
                                            float dx, float dy, float dz,
                                            char *err, size_t errlen ) ;

/*! Paint one value per analysis unit: a searchlight's center voxel/node, or
    every member voxel of an atlas parcel. */
extern void THD_roilist_paint( float *brick, const THD_roilist *rl,
                               const float *values ) ;

/*! Compare the represented node domain of two NIML/GIFTI surface datasets.
    A dense dataset implicitly represents node i at row i; a sparse dataset
    uses dblk->node_list.  Thus dense and sparse inputs can match, but only
    when every row resolves to exactly the same nonnegative, unique node ID.
    Sparse rows need not be sorted. Returns 1 on an exact match and writes a
    bounded diagnostic on failure. */
extern int THD_surface_domain_match( const THD_3dim_dataset *aa,
                                     const THD_3dim_dataset *bb,
                                     char *err, size_t errlen ) ;

/*! Copy only the sparse surface node domain from src to dst.  This deliberately
    leaves brick labels, statistics, and other auxiliary data untouched.  A
    dense surface source clears any stale destination node list; a non-surface
    source succeeds without changing the destination.  Surface source and
    destination row counts must match; malformed/duplicate source IDs fail
    without modifying the destination. */
extern int THD_copy_surface_node_domain( THD_3dim_dataset *dst,
                                         const THD_3dim_dataset *src,
                                         char *err, size_t errlen ) ;

#ifdef USE_SUMA
/*! Geodesic surface searchlights over the nodes represented by mset.  Sparse
    node indexes must be unique, in the mesh range, and exactly one per dataset
    row. val[] reports mesh node IDs while center[] stores dataset rows. */
extern THD_roilist * THD_roilist_searchlight_surf( const char *surface,
                                                   THD_3dim_dataset *mset,
                                                   float radius, int all_nodes,
                                                   char *err, size_t errlen ) ;
#endif

extern void          THD_roilist_free( THD_roilist *rl ) ;

/*! Largest voxel count over all the ROIs, for sizing scratch buffers. */
extern int THD_roilist_maxvox( THD_roilist *rl ) ;

/*----------------------------------------------------------------------------
  Extraction.  'out' is always caller-allocated, so these allocate nothing and
  are safe to call from many threads at once.
------------------------------------------------------------------------------*/

/*! The ROI-mean time course: out[nvals].  With polort >= 0 the mean is
    detrended, which is equivalent to detrending every voxel first. */
extern void THD_roi_mean_ts( THD_3dim_dataset *dset , intvec *vox ,
                             int polort , float *out ) ;

/*! As THD_roi_mean_ts, also records whether each original ROI pattern has at
    least one nonzero voxel.  keep[t]=0 means every selected voxel was exactly
    zero at t; this is determined before optional detrending, so it preserves
    zero-filled-volume censoring semantics. */
extern void THD_roi_mean_ts_zmask( THD_3dim_dataset *dset , intvec *vox ,
                                   int polort , float *out ,
                                   unsigned char *keep ) ;

/*! The ROI's voxel patterns for every sub-brick: out[nvals*nvox], laid out
    as [sub-brick][voxel].  That single layout serves both readings --

      as nvals rows of nvox   -> one pattern per condition (classic RSA)
      as one long vector      -> this subject's whole ROI response (IS-RSA)

    which is why there is only one function here rather than two. */
extern void THD_roi_pattern( THD_3dim_dataset *dset , intvec *vox , float *out ) ;

/*! Extract selected canonical sample rows over one ROI/searchlight into
    out[nsample][vox->nar].  sample_index gives zero-based THD_sampleset rows
    in the desired output order; repeated rows are allowed.  Passing NULL
    selects every canonical row and requires nsample == ss->nsample.

    Dataset scale factors are applied.  The caller owns all storage, and no
    allocation is performed after a dataset is loaded.  Returns 1 on success;
    on failure, out may be partially filled and err receives a bounded
    diagnostic when supplied. */
extern int THD_roi_samples( const THD_sampleset *ss,
                            const int *sample_index, int nsample,
                            const intvec *vox, float *out,
                            char *err, size_t errlen ) ;

/*----------------------------------------------------------------------------
  Run-aware input, for cross-validated (runwise) classic RSA.

  Ordinary classic RSA estimates each condition's pattern once per subject, so a
  distance computed and evaluated from the SAME noisy patterns is positively
  biased -- two conditions look different even when they are not, because the
  noise never cancels.  Cross-validated squared Euclidean and crossnobis remove
  that bias by taking each condition contrast from INDEPENDENT runs, so the noise
  in one run multiplies the noise in another and averages to zero.  That needs
  the data organized by run, which the one-dataset-per-subject '-dataTable'
  cannot express -- hence this separate '-runwiseTable' input and its container.

  The table has one row per subject x run:

      Subj  Run  InputFile          ResidFile
      s01   1    s01_r1_betas+tlrc  s01_r1_errts+tlrc
      s01   2    s01_r2_betas+tlrc  s01_r2_errts+tlrc

  By default InputFile has one sub-brick per condition in a common order across
  all rows. F21 optionally adds a ConditionFile column: each row's text file
  gives one whitespace-free condition label per InputFile sub-brick. S6 instead
  accepts a TrialFile table with Trial and Condition columns, one row per
  already-estimated trial-beta sub-brick. Subject and run are inherited from the
  containing runwiseTable row, completing the subject/run/trial/condition
  nesting. ConditionFile and TrialFile are mutually exclusive. A condition may
  be absent from a run or repeated within it; repeated/trial betas are averaged
  into that run's condition estimate. Trial IDs must be unique within subject.
  The canonical model order is the sorted set of condition labels. Every
  condition pair must still co-occur in at least two runs per subject so its
  cross-validated distance is estimable. ResidFile is optional: needed only for
  crossnobis noise whitening, and holds that run's residual time series for the
  covariance estimate.
------------------------------------------------------------------------------*/

typedef struct {
   int    nsub ;        /*!< number of subjects                               */
   int    nrow ;        /*!< total subject x run rows                         */
   int    ncond ;       /*!< global conditions (model/RDM order)              */
   int    maxbrick ;    /*!< maximum InputFile sub-bricks over rows           */
   int    has_condmap ; /*!< ConditionFile or TrialFile mapping supplied       */
   int    has_trialmap; /*!< TrialFile supplies explicit trial identities      */
   int    nvox ;        /*!< voxels (grid), common to all datasets            */
   int    has_resid ;   /*!< 1 if a ResidFile column was present              */
   int    resid_nt ;    /*!< residual time points (min over rows); 0 if none  */
   int    from_xmat ;   /*!< runs/conditions were expanded from an Xmat table */

   char **subj ;        /*!< [nsub] subject labels, in first-seen order       */
   int   *nrun ;        /*!< [nsub] runs per subject                          */
   int  **row_of ;      /*!< [nsub][nrun[s]] -> row index                     */

   int   *row_sub ;     /*!< [nrow] subject index of each row                 */
   char **run_lab ;     /*!< [nrow] run label of each row                     */
   THD_3dim_dataset **betas ;  /*!< [nrow] InputFile (headers open)           */
   THD_3dim_dataset **resid ;  /*!< [nrow] ResidFile (headers open), or NULL  */

   char **cond_lab ;    /*!< [ncond] sorted labels; NULL on balanced old path */
   int   *nbrick ;      /*!< [nrow] local beta-brick counts                   */
   int  **cond_of ;     /*!< [nrow][nbrick[row]] -> global condition          */
   int  **nrep ;        /*!< [nrow][ncond] local repetitions (0 = absent)     */
   char ***trial_lab ;  /*!< [nrow][nbrick[row]] trial IDs, or NULL           */

   /* Optional exact float32 store over only the voxels used by the analysis.
      Headers remain open for metadata, but their full-grid arrays are unloaded. */
   int    store_nvox ;  /*!< compact voxel count; 0 means ordinary datasets   */
   int   *store_map ;   /*!< [nvox] full-grid voxel -> compact index, or -1    */
   int   *store_vox ;   /*!< [store_nvox] compact index -> full-grid voxel     */
   float **beta_store ; /*!< [nrow][nbrick[row]*store_nvox], brick-major       */
   float **resid_store; /*!< [nrow][residual_nt[row]*store_nvox], or NULL      */
   double store_bytes ; /*!< resident compact arrays                           */
   double store_largest_input ; /*!< largest full dataset while filling store       */

   char  *source ;      /*!< where it came from, for messages                 */
} THD_runset ;

/*! Read and VALIDATE a '-runwiseTable'. Requires Subj, Run and InputFile
    columns; ResidFile and exactly one of ConditionFile or TrialFile are
    optional. Without a mapping, preserves the balanced common-count/order
    contract. A ConditionFile has one label per local brick. A TrialFile is a
    Trial/Condition table with one row per local brick and trial IDs unique
    within subject. Either mapping builds the sorted global condition order and
    requires every pair to have >=2 valid runs per subject. Grids, unique
    within-subject run labels, residual grids, and residual lengths are also
    checked. Returns NULL with an ERROR_message on any problem. Dataset headers
    are opened but not loaded (stream per ROI later). */
extern THD_runset * THD_runset_read ( char *fname ) ;
/*! Read a compact one-row-per-subject table with Subj, Xmat and whole-bucket
    InputFile columns; ResidFile is optional and must likewise be the whole
    residual dataset.  Stimulus beta labels, runs, conditions, censoring and
    residual run samples are derived from Xmat.  stims is a comma-separated
    stimulus-label subset, or NULL for all stimuli. */
extern THD_runset * THD_runset_read_xmat( char *fname , char *stims ) ;
/*! Plan an exact float32 store over the union of primary and optional extra
    ROI voxels. It is enabled only when smaller than the datasets' native
    resident arrays. include_resid says whether residuals will be consumed.
    Returns 1 when compact storage was selected, 0 when dense storage is
    already smaller/equal, and -1 on allocation or invalid-voxel failure. */
extern int          THD_runset_plan_store( THD_runset *rs, THD_roilist *primary,
                                           THD_roilist *extra, int include_resid ) ;
/*! Load one row's beta (residual=0) or residual (residual=1) into the planned
    compact store and unload its full-grid voxel array. */
extern int          THD_runset_load_store_row( THD_runset *rs, int row, int residual ) ;
/*! Extract a row's beta/residual patterns for an ROI from either representation. */
extern void         THD_runset_roi_pattern( THD_runset *rs, int row, int residual,
                                            intvec *vox, float *out ) ;

/*! Extract one run's beta patterns in canonical condition order, averaging
    repeated labels. pat holds ncond*vox->nar floats. For mapped runs, raw
    is separate caller-owned scratch of nbrick[row]*vox->nar floats; otherwise
    it may be NULL. Requires a valid loaded run and ROI, as roi_pattern does.
    Missing mapped conditions remain zero; callers requiring complete repeats
    must check nrep[row][condition] before using this helper. */
extern void THD_runset_roi_condition_pattern( THD_runset *rs, int row,
                                               intvec *vox, float *pat,
                                               float *raw ) ;
extern void         THD_runset_free ( THD_runset *rs ) ;
extern void         THD_runset_print( THD_runset *rs , FILE *fp ) ;

#endif /* _THD_PATTERNS_HEADER_ */
