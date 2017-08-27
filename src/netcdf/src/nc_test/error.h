/*********************************************************************
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header $
 *********************************************************************/

#ifdef __cplusplus
extern "C" {
#endif

/* Print error message to stderr, don't exit */
extern void	error (const char *fmt, ...);

void print(const char *fmt, ...);

extern int ifFail(const int expr, const int line, const char *file);

extern void
print_n_size_t(size_t nelems, const size_t *array);

#ifdef __cplusplus
}
#endif

#define IF(EXPR) if (ifFail(EXPR, __LINE__, __FILE__))
