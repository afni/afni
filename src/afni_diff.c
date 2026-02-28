/* afni_diff.c
 *
 * Implementation details for performing elementwise diffs on
 * THD_3dim_dsets.
 *
 * Author: JB Teves */

#include "assert.h"

#include "mrilib.h"
#include "afni_diff.h"

/* macro to set the values in the ADR for this function, crash if error.
 * Do not use this except in this context.
 * JB Teves only made it because he's lazy. */
#define _AFNI_DIFF_SET_CHECK_ADR(x) \
    do { \
        x.counts = counts; \
        x.nt = nt; \
        x.nv = nv; \
        x.error = error; \
        if ( x.error ) { \
            return x; \
        } \
    } \
    while (0);

AfniDiffResult afni_diff(char * a, char * b, char * m, float tol)
{
    /* TODO: add quiet or fast option that short-circuits on diff */
    /* Return result struct */
    AfniDiffResult adr = {NULL, -1, -1, 1};
    /* Initialize variables to be packaged in return
     * Note: if you change these names, then 
     * _AFNI_DIFF_SET_CHECK_ADR won't keep working. */
    int nt, nv = -1;
    int * counts = NULL;
    int error = 0;
    /* Variables for loading the dsets */
    THD_3dim_dataset *ds1, *ds2 = NULL;

    /* Validate passed args */
    if ( !a ) {
        error |= AFNI_DIFF_A_NULL;
    }
    if ( !b ) {
        error |= AFNI_DIFF_B_NULL;
    }
    /* Quietly make sure tolerance is positive */
    if ( tol < 0 ) tol = 0 - tol;
    _AFNI_DIFF_SET_CHECK_ADR(adr);

    set_obliquity_report(0); /* We'll investigate that ourselves */

    /* Load the DSETS */
    ds1 = THD_open_dataset(a);
    if ( !ds1 ) error |= AFNI_DIFF_LOAD_A_FAILED;
    ds2 = THD_open_dataset(b);
    if ( !ds2 ) error |= AFNI_DIFF_LOAD_B_FAILED;
    _AFNI_DIFF_SET_CHECK_ADR(adr);
    DSET_load(ds1); DSET_load(ds2);
    /* Manually check for loading, based off mrilib.h:CHECK_LOAD_ERROR */
    if ( !(DSET_LOADED(ds1) && ISVALID_DSET(ds1)) ) {
        error |= AFNI_DIFF_LOAD_A_FAILED;
    }
    if ( !(DSET_LOADED(ds2) && ISVALID_DSET(ds2)) ) {
        error |= AFNI_DIFF_LOAD_B_FAILED;
    }
    _AFNI_DIFF_SET_CHECK_ADR(adr);
    if ( THD_dataset_mismatch(ds1, ds2) ) {
        error |= AFNI_DIFF_A_B_MISMATCH;
    }
    _AFNI_DIFF_SET_CHECK_ADR(adr);
    if ( DSET_NVALS(ds1) != DSET_NVALS(ds2) ) {
        error |= AFNI_DIFF_A_B_TMISMATCH;
    }
    _AFNI_DIFF_SET_CHECK_ADR(adr);
    /* At least a and b are valid and comparable from here */

    nt = DSET_NVALS(ds1);
    nv = DSET_NVOX(ds1);
    /* Important: this will get returned to the caller, so we can't free */
    counts = (int*) calloc(nt, sizeof(int));
    
    /* Copy program values to adr */
    adr.counts = counts;
    adr.nt = nt;
    adr.nv = nv;

    if ( m ) {
        THD_3dim_dataset * dsm = THD_open_dataset(m);
        if ( !dsm ) {
            error |= AFNI_DIFF_LOAD_MASK_FAILED;
        }
        _AFNI_DIFF_SET_CHECK_ADR(adr);
        DSET_load(dsm);
        /* Manually check loading as before for a, b */
        if ( !DSET_LOADED(dsm) ) {
            error |= AFNI_DIFF_LOAD_MASK_FAILED;
        }
        _AFNI_DIFF_SET_CHECK_ADR(adr);
        /* Make sure the mask agrees with a, b */
        if ( THD_dataset_mismatch(ds1, dsm) ) {
            error |= AFNI_DIFF_MASK_AB_MISMATCH;
        }
        _AFNI_DIFF_SET_CHECK_ADR(adr);


        /* Allocate arrays to do masked comparisons */
        /* These will all need to be freed by this function */
        float * maskarr = (float*) calloc(nv, sizeof(float));
        int * mi        = (int*)   calloc(nv, sizeof(int));

        int nv_m = 0; /* track number of masked elements */

        /* Binarize the mask array and calculate indices for mask*/
        for (int i = 0; i < nv; ++i) {
            if ( THD_get_voxel(dsm, i, 0) != 0.0 ) {
                mi[nv_m] = i;
                ++nv_m;
            }
        }
        /* Note: do not use indices >= nv_m for mi, they are garbage */
        adr.msize = nv_m;
        
        /* Compare elements within the mask */
        for (int t = 0; t < nt; ++t) {
            for (int v = 0; v < nv_m; ++v) {
                counts[t] += (
                    ABS(
                        THD_get_voxel(ds1, mi[v], t) -
                        THD_get_voxel(ds2, mi[v], t)
                    )
                    > tol
                );
            }
            /* TODO: add -q break here */
        }

        /* Free mask-related memory we allocated */
        free(maskarr);
        free(mi);
    }
    else {
        adr.msize = nv;
        for (int t = 0; t < nt; ++t) {
            for (int i = 0; i < nv; ++i) {
                int val = (
                    ABS(
                        THD_get_voxel(ds1, i, t) -
                        THD_get_voxel(ds2, i, t)
                    )
                    > tol
                );
                counts[t] += val;
            }
            /* TODO: add -q break here */
        }
    }
    adr.counts = counts;

    return adr;
}

int afni_diff_summary(const AfniDiffResult * adr)
{
    if (adr->error) return -1;
    for (int i = 0; i < adr->nt; ++i) {
        if (adr->counts[i]) return 1;
    }
    return 0;
}

/* Tests that could be devised
int main()
{
    char vfa[100] = "/Users/tevesjb/Downloads/d3_test/test1.nii.gz";
    char vfb[100] = "/Users/tevesjb/Downloads/d3_test/test2.nii.gz";
    char bf[100]  = "this_file_does_not_exist.nii.gz";
    char mismatch[100] = "/Users/tevesjb/Downloads/a.nii";
    char vfa1[100] = "/Users/tevesjb/Downloads/d3_test/test1.nii.gz[0]";
    char union_mask[100] = "/Users/tevesjb/Downloads/test_union_mask.nii.gz";
    char zero_mask[100] = "/Users/tevesjb/Downloads/test_zero_mask.nii.gz";
    int64_t counts_nomask, counts_mask = 0;

    AfniDiffResult r;

    r = afni_diff(&vfa[0], &vfb[0], NULL, AFNI_DIFF_DEFAULT_TOL);
    assert( r.error == 0 );
    assert( afni_diff_summary(&r) == 1);
    free(r.counts);

    r = afni_diff(NULL, &vfb[0], NULL, AFNI_DIFF_DEFAULT_TOL);
    assert( r.error == AFNI_DIFF_A_NULL );
    assert( afni_diff_summary(&r) == -1);
    free(r.counts);

    r = afni_diff(&vfa[0], NULL, NULL, AFNI_DIFF_DEFAULT_TOL);
    assert( r.error == AFNI_DIFF_B_NULL );
    free(r.counts);

    r = afni_diff(NULL, NULL, NULL, AFNI_DIFF_DEFAULT_TOL);
    assert( r.error == AFNI_DIFF_A_NULL + AFNI_DIFF_B_NULL );
    free(r.counts);

    r = afni_diff(&bf[0], &vfb[0], NULL, AFNI_DIFF_DEFAULT_TOL);
    assert( r.error == AFNI_DIFF_LOAD_A_FAILED );
    free(r.counts);

    r = afni_diff(&vfa[0], &bf[0], NULL, AFNI_DIFF_DEFAULT_TOL);
    assert( r.error == AFNI_DIFF_LOAD_B_FAILED );
    free(r.counts);

    r = afni_diff(&bf[0], &bf[0], NULL, AFNI_DIFF_DEFAULT_TOL);
    assert( r.error == AFNI_DIFF_LOAD_A_FAILED + AFNI_DIFF_LOAD_B_FAILED );
    free(r.counts);

    r = afni_diff(&vfa[0], &mismatch[0], NULL, AFNI_DIFF_DEFAULT_TOL);
    assert( r.error == AFNI_DIFF_A_B_MISMATCH );
    free(r.counts);

    r = afni_diff(&vfa1[0], &vfb[0], NULL, AFNI_DIFF_DEFAULT_TOL);
    assert( r.error == AFNI_DIFF_A_B_TMISMATCH );
    free(r.counts);

    r = afni_diff(&vfa[0], &vfb[0], &bf[0], AFNI_DIFF_DEFAULT_TOL);
    assert( r.error == AFNI_DIFF_LOAD_MASK_FAILED );
    free(r.counts);

    r = afni_diff(&vfa[0], &vfb[0], &mismatch[0], AFNI_DIFF_DEFAULT_TOL);
    assert( r.error == AFNI_DIFF_MASK_AB_MISMATCH );
    free(r.counts);
}
*/
