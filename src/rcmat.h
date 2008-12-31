#ifndef _AFNI_RCMAT_HEADER_
#define _AFNI_RCMAT_HEADER_

/*****
  Struct to hold a banded square matrix that is either symmetric,
  or upper or lower triangular (depending on the needs of the moment).
*****/

#undef  LENTYP
#define LENTYP unsigned short

typedef struct {
  int     nrc ;    /* # of rows and columns */
  LENTYP *len ;    /* in row/column #i, there are len[i] elements */
  double **rc ;    /* so the first column/row index is i+1-len[i] */
                   /* diagonal element #i is in rc[i][len[i]-1]   */
} rcmat ;

#define ISVALID_RCMAT(rr)                                      \
  ( (rr) != NULL && (rr)->len != NULL && (rr)->len[0] == 1 &&  \
                    (rr)->rc  != NULL && (rr)->rc[0]  != NULL )

extern rcmat * rcmat_init        ( int n ) ;
extern void    rcmat_destroy     ( rcmat *rcm ) ;
extern rcmat * rcmat_copy        ( rcmat *rcm ) ;
extern int     rcmat_choleski    ( rcmat *rcm ) ;
extern void    rcmat_lowert_solve( rcmat *rcm , double *vec ) ;
extern void    rcmat_uppert_solve( rcmat *rcm , double *vec ) ;

extern float * rcmat_lsqfit ( int npt, float *far, int nref, float *ref[] ) ;
extern rcmat * rcmat_normeqn( int npt,             int nref, float *ref[] ) ;

#endif /* _AFNI_RCMAT_HEADER_ */
