#ifndef _THD_RSA_HEADER_
#define _THD_RSA_HEADER_

#include "thd_simmatrix.h"
#include <stddef.h>

/*----------------------------------------------------------------------------
  Dataset-independent representational geometry workflows.

  thd_simmatrix owns individual matrix arithmetic.  This module owns the RSA
  workflow one level above it: condition-pattern preprocessing, one subject's
  condition RDM, collections of subject RDMs, and the second-order subject
  geometry built from those collections.  Dataset/ROI extraction, command-line
  policy, inference, and output remain in the calling program.
                                                     -- P Molfese, Oct 2026
------------------------------------------------------------------------------*/

typedef struct THD_rsa_rdm_workspace THD_rsa_rdm_workspace ;

/*! Reusable storage for at most max_subject subject RDMs of at most
    max_condition conditions. */
extern THD_rsa_rdm_workspace *
       THD_rsa_rdm_workspace_new( int max_subject, int max_condition ) ;
extern void THD_rsa_rdm_workspace_free( THD_rsa_rdm_workspace *ws ) ;

/*! Reset the active subject/condition dimensions without reallocating. */
extern int THD_rsa_rdm_workspace_reset( THD_rsa_rdm_workspace *ws,
                                        int nsubject, int ncondition,
                                        char *err, size_t errlen ) ;

/*! Remove the feature-wise mean over conditions in place.  patterns is
    [condition][feature]. */
extern int THD_rsa_center_condition_patterns( int ncondition, int nfeature,
                                              float *patterns,
                                              char *err, size_t errlen ) ;

/*! Build one subject's condition RDM from [condition][feature] patterns.
    When center_conditions is nonzero, angle-based metrics first remove each
    feature's mean over conditions.  Euclidean differences already cancel a
    shared mean exactly, so that metric deliberately skips the subtraction to
    preserve bitwise compatibility.  The caller-owned patterns may be centered
    in place. */
extern THD_simmat * THD_rsa_condition_rdm( int ncondition, int nfeature,
                                           float *patterns, int metric,
                                           int center_conditions,
                                           char *err, size_t errlen ) ;

/*! Strict condition RDM for participant outcomes. After optional centering,
    reject nonfinite patterns, constant Pearson/Spearman patterns, and zero
    cosine norms. Returns NULL for undefined geometry. This opt-in variant
    leaves the ordinary condition-RDM API's degenerate-pattern behavior intact.
    patterns is [condition][feature] and may be centered in place. */
extern THD_simmat * THD_rsa_condition_rdm_checked(
                       int ncondition, int nfeature, float *patterns,
                       int metric, int center_conditions ) ;

/*! Pearson or Spearman correlation of two RDM triangles, clipped to
    +/-0.999999f before atanhf. Returns NAN for nonfinite/constant triangles
    or invalid arguments. a, b, sc1 and sc2 each have ntriangle entries;
    sc1/sc2 are caller-owned scratch. Inputs a/b are not modified. */
extern float THD_rsa_fisher_corr( int ntriangle, float *a, float *b,
                                  int metric, float *sc1, float *sc2 ) ;

/*! Equal mean Fisher z over all unordered repeat-RDM pairs. triangles has
    layout [nrepeat][ntriangle]; sc1/sc2 each hold ntriangle scratch entries.
    Uses THD_rsa_fisher_corr's metric/clipping policy. Returns NAN if fewer
    than two repeats or any pair is undefined; never drops individual pairs. */
extern float THD_rsa_repeat_stability( int nrepeat, int ntriangle,
                                       float *triangles, int metric,
                                       float *sc1, float *sc2 ) ;

/*! Copy a matrix's strict upper triangle.  With as_distance nonzero,
    similarities are converted entrywise to 1-s while distances are retained.
    This is the common feature sense required by second-order RSA. */
extern int THD_rsa_simmat_to_triangle( const THD_simmat *sm, float *triangle,
                                       int as_distance,
                                       char *err, size_t errlen ) ;

/*! Store one subject RDM in the active workspace.  as_distance has the same
    meaning as THD_rsa_simmat_to_triangle.  All stored rows must have the same
    resulting similarity/distance sense. */
extern int THD_rsa_rdm_workspace_store( THD_rsa_rdm_workspace *ws,
                                        int subject, const THD_simmat *rdm,
                                        int as_distance,
                                        char *err, size_t errlen ) ;

/*! Build the subject-by-subject second-order geometry from all subject RDMs in
    the active workspace.  Similarity triangles are converted to 1-s in the
    reusable scratch buffer before the outer metric is applied. */
extern THD_simmat *
       THD_rsa_second_order_rdm( THD_rsa_rdm_workspace *ws, int outer_metric,
                                 char *err, size_t errlen ) ;

/*! Descriptive elementwise mean of compact subject RDM triangles.  The input
    layout is [subject][THD_NTRI(ncondition)] and is not modified. */
extern THD_simmat *
       THD_rsa_mean_subject_rdms( int nsubject, int ncondition,
                                  const float *triangles, int is_distance,
                                  char *err, size_t errlen ) ;

#endif /* _THD_RSA_HEADER_ */
