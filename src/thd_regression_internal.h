#ifndef THD_REGRESSION_INTERNAL_H
#define THD_REGRESSION_INTERNAL_H

/* Internal shared Cholesky solve for banded ridge and LDA. a[d][d] supplies
   the lower triangle of an SPD matrix; b[d][q] supplies RHS columns. Both
   compact row-major buffers are overwritten (factor and solution). Returns
   0 on a nonpositive/nonfinite pivot; callers retain their own error text.
   Valid dimensions and buffers are the caller's responsibility. */
int THD_regression_spd_solve(double *a, double *b, int d, int q);

/* Private thin SVD shared by fractional regression and nuisance projection.
   Compact row-major thin factors; columns retain the same unsorted order.
   Preparation calls the existing EISPACK routine without changing its source
   or global SVD settings. Fits use only immutable factors. */
typedef struct {
   int n, p, r, rank;
   double *u, *s, *v, cutoff;
} THD_numeric_svd;
THD_numeric_svd *THD_numeric_svd_prepare(const double *x, int n, int p,
                                         double rank_tol, char *err, int nerr);
void THD_numeric_svd_free(THD_numeric_svd *s);

#endif
