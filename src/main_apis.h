/* main_apis.h
 *
 * This file contains various APIs for working with the AFNI suite.
 * This allows users to compile programs that leverage AFNI functions in C.
 * In order to make python bindings, please see main_appy.h */

/* Most functions return large amounts of information. Consequently, they
 * have to return a struct which contains the information plus any errors
 * encountered. All errors will be in the "error" field as an int. This
 * allows programmers to note many different different errors.
 * Below are the expected naming conventions, with the following shorthands
 * to help ease convention description (case-insensitive):
 *      prog    the modified program name (see NOTE)
 *      desc    an arbitrary description
 *      opt     an option name in a function signature
 *      field   a field name in a struct
 * NOTE: AFNI programs conventionally start with 3d*. However, in C this is
 * not a valid symbol. To avoid this problem, we replace 3d with the 
 * prefix "afni_"; for example, 3dDiff -> afni_diff
 * with the following conventions (case-sensitive):
 *  Symbolic Constants:
 *      PROG_ERR_DESCRIPTION    to represent an error code
 *          "3dDiff error: loading a failed" -> AFNI_DIFF_ERR_LOAD_A_FAILED
 *      PROG_OPT_VARIANT        to represent an option's value
 *          "3dDiff report quiet" -> AFNI_DIFF_REPORT_QUIET
 *  Types:
 *      ProgResult              to represent a return struct
 *          "3dDiff result" -> AfniDiffResult
 *  Functions:
 *      prog                    to represent a main program function
 *          "3dDiff main function" -> afni_diff(...)
 *      prog_failed             to represent a function checking failure
 *          "3dDiff failure check" -> 
 *          afni_diff_failed(AfniDiffResult r, int type)
 *          (This prototype should be followed always)
 *      prog_desc               to represent a program-related function
 *          "3dDiff generate report" -> afni_diff_genreport(...)
 */

#include <limits.h> /* INT_MAX */

/* Facilities for all functions to use */
#define AFNI_FAILURE_ANY INT_MAX

/*---------------------------------------------------------------------- 
 * 3dDiff
 *----------------------------------------------------------------------*/
typedef struct {
    int volumes_different;
    int volumes_total;
    int64_t elements_different;
    int64_t elements_total;
    int error;
} AfniDiffResult;

/* Errors for afni_diff */
#define AFNI_DIFF_ERR_LOAD_A_FAILED     0x01 /* Loading dset a failed */
#define AFNI_DIFF_ERR_LOAD_B_FAILED     0x02 /* Loading dset b failed */
#define AFNI_DIFF_ERR_LOAD_M_FAILED     0x04 /* Loading mask dset failed */
#define AFNI_DIFF_ERR_DSET_MISMATCH     0x08 /* dset a, b mismatch */
#define AFNI_DIFF_ERR_DSET_TPOINTS      0x10 /* dset a, b tpoints not eq */
#define AFNI_DIFF_ERR_DSET_M_MISMATCH   0x20 /* dset, mask mismatch */

/* Errors for afni_diff_report */
#define AFNI_DIFF_ERR_REPORT            0x40 /* reporting level invalid */

/* Functions for afni_diff */
AfniDiffResult afni_diff(
    const char * a, /* fname for dset a */
    const char * b, /* fname for dset b */
    const char * m, /* fname for mask dset */
    float tol       /* tolerance for considering "equal" */
);
char * afni_diff_genreport(const * AfniDiffResult);
int afni_diff_failed(const * AfniDiffResult, int type);
