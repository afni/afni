#ifndef _THD_PATTERNS_HEADER_
#define _THD_PATTERNS_HEADER_

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
   int    *val ;    /* [nroi] atlas value, or the center voxel index (searchlight) */
   intvec *vox ;    /* [nroi] voxel indices; .nar is the voxel count */
   char  **lab ;    /* [nroi] label from the atlas label table, or NULL */
   int    *center ; /* [nroi] center voxel to paint; -1 means paint the whole ROI */
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

#ifdef USE_SUMA
/*! Geodesic surface searchlights over the nodes represented by mset. */
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

/*! The ROI's voxel patterns for every sub-brick: out[nvals*nvox], laid out
    as [sub-brick][voxel].  That single layout serves both readings --

      as nvals rows of nvox   -> one pattern per condition (classic RSA)
      as one long vector      -> this subject's whole ROI response (IS-RSA)

    which is why there is only one function here rather than two. */
extern void THD_roi_pattern( THD_3dim_dataset *dset , intvec *vox , float *out ) ;

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
extern void         THD_runset_free ( THD_runset *rs ) ;
extern void         THD_runset_print( THD_runset *rs , FILE *fp ) ;

#endif /* _THD_PATTERNS_HEADER_ */
