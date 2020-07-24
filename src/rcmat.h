#ifndef _AFNI_RCMAT_HEADER_
#define _AFNI_RCMAT_HEADER_

/****************************************************************************
  Struct to hold a banded square matrix that is either symmetric,
  or upper or lower triangular (depending on the needs of the moment).

  That is, only 1/2 the matrix (roughly) is stored, because the
  other half is knowable from what is stored.

  Thinking of the rcmat as a lower triangular structure:
    It has nrc rows and columns.
    Row #i starts with 0s, and then is followed by len[i]
      nonzero values, up to the diagonal;
      thus, the first nonzero column index in row #1 is i+1-len[i],
    Row #i is stored in array rc[i][0 .. len[i]-1 ].

   Picture:
     x            len[0] = 1  (note len[i] is always > 0)
     x x       => len[1] = 2
     0 x x     => len[2] = 2
     0 x x x   => len[3] = 3
     0 0 0 x x => len[4] = 2

   Computational operations:
     rcmat_choleski      = Choleski decomposition of matrix (in-place)
                            -- assuming matrix is positive definite
     rcmat_lowert_vecmul = Treating rcmat as lower triangular (row oriented),
                           multiply it into a vector (in-place)
     rcmat_lowert_solve  = Treating rcmat as lower triangular,
                           solve [rcmat] [x] = [vector], over-writing input
     rcmat_uppert_solve  = Similar to above, but considering
                           rcmat as upper triangular (columns vs rows)

   These functions were written for use in remla.c (and thus 3dREMLfit.c),
   but have been used in other places.
****************************************************************************/

#undef  LENTYP
#define LENTYP unsigned short  /* type to store len[] */

#undef  LENTYP_MAX
#define LENTYP_MAX 65535u      /* sets maximum row length */

typedef struct {
  int     nrc ;    /* # of rows and columns */
  LENTYP *len ;    /* in row/column #i, there are len[i] elements */
  double **rc ;    /* so the first column/row index is i+1-len[i] */
                   /* diagonal element #i is in rc[i][len[i]-1]   */
} rcmat ;

#define ISVALID_RCMAT(rr)                                      \
  ( (rr) != NULL && (rr)->len != NULL && (rr)->len[0] == 1 &&  \
                    (rr)->rc  != NULL && (rr)->rc[0]  != NULL )

extern rcmat * rcmat_init         ( int n ) ;
extern void    rcmat_destroy      ( rcmat *rcm ) ;
extern rcmat * rcmat_copy         ( rcmat *rcm ) ;
extern int     rcmat_choleski     ( rcmat *rcm ) ;
extern void    rcmat_lowert_solve ( rcmat *rcm , double *vec ) ;
extern void    rcmat_uppert_solve ( rcmat *rcm , double *vec ) ;
extern void    rcmat_lowert_vecmul( rcmat *rcm , double *vec ) ; /* 02 Oct 2009 */

extern void    rcmat_lowert_solve_unrolled( rcmat *rcm , double *vec ) ;
extern float   rcmat_avglen( rcmat *rcm ) ;

extern float * rcmat_lsqfit ( int npt, float *far, int nref, float *ref[] ) ;
extern rcmat * rcmat_normeqn( int npt,             int nref, float *ref[] ) ;

extern rcmat * rcmat_from_rows( int nn , float *rr[] ) ;

#endif /* _AFNI_RCMAT_HEADER_ */
