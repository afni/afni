/* afni_diff.h
 *
 * Creates the functionality to perform elementwise diffs on THD_3dim_dsets
 *
 * Author: JB Teves */

/* Set default tolerance */
#define AFNI_DIFF_DEFAULT_TOL 0.0

/* struct for storing the result 
 *
 * Only use this with afni_diff! There is no reason to use this elsewhere.
 * IMPORTANT: It is required that you free counts after use! */
typedef struct {
    int * counts;       /* total number of differing voxels per volume */
    int nt;             /* total number of volumes compared */
    int nv;             /* total number of voxels per volume */
    int msize;          /* how many voxels were in the mask */
    int error;          /* whether an error occurred */
} AfniDiffResult;

/* Calculate the diff between two dset files */
AfniDiffResult afni_diff(
    char * a,     /* dset a filename */
    char * b,     /* dset b filename */
    char * m,     /* mask dset filename */
    float tol           /* max absolute difference to be "equal" */
);

/* Summarize a calculated diff as -1 (error), 0 (no diff), 1 (diff) */
int afni_diff_summary(const AfniDiffResult * adr);

/* Use bits to communicate many possible errors */
enum AfniDiffError {
    AFNI_DIFF_A_NULL =                  0x01,
    AFNI_DIFF_B_NULL =                  0x02,
    AFNI_DIFF_LOAD_A_FAILED =           0x04,
    AFNI_DIFF_LOAD_B_FAILED =           0x08,
    AFNI_DIFF_A_B_MISMATCH =            0x10,
    AFNI_DIFF_A_B_TMISMATCH =           0x20,
    AFNI_DIFF_LOAD_MASK_FAILED =        0x40,
    AFNI_DIFF_MASK_AB_MISMATCH =        0x80
};
