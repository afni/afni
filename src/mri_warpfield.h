#ifndef _MRI_WARPFIELD_HEADER_

/*---------------------------------------------------------------------------*/
/* Functions for warping the cube [-1..1]x[-1..1]x[-1..1].                   */
/*---------------------------------------------------------------------------*/

typedef void (*Warpfield_basis)(int,void *,int,float *,float *,float *,float *);
typedef void * (*Warpfield_setup)(float,int *,void *) ;

#define WARPFIELD_TRIG_TYPE   1  /* sin & cos */
#define WARPFIELD_LEGEN_TYPE  2  /* Legendre polynomials */
#define WARPFIELD_GEGEN_TYPE  3  /* Gegenbauer(-.5) polynomials */

#define WARPFIELD_LAST_TYPE   3

typedef struct {
  int type ;
  mat44 aa ;
  float order ;
  floatvec *pv ;
  int nfun ;
  float *cx , *cy , *cz ;
  void *bpar ;
  Warpfield_basis bfun ;
  Warpfield_setup bset ;
} Warpfield ;

/*---------------------------------------------------------------------------*/

extern void Warpfield_set_verbose( int ) ;

extern Warpfield * Warpfield_init( int type, float order, floatvec *fv ) ;

extern void Warpfield_destroy( Warpfield *wf ) ;

extern void Warpfield_change_order( Warpfield *wf , float neword ) ;

extern Warpfield * Warpfield_inverse( Warpfield *wf , float *rmserr ) ;

extern Warpfield * Warpfield_approx( Warpfield *wf, float ord, float *rmserr );

extern float Warpfield_compose(void) ; /* not implemented yet */

extern void Warpfield_eval_array( Warpfield *wf ,
                                  int npt, float *xi, float *yi, float *zi,
                                           float *xo, float *yo, float *zo ) ;

extern void Warpfield_eval_grid( Warpfield *wf ,
                                 int nx, float xb, float xt,
                                 int ny, float yb, float yt,
                                 int nz, float zb, float zt,
                                 float *xo , float *yo, float *zo ) ;

/*---------------------------------------------------------------------------*/
#endif /* _MRI_WARPFIELD_HEADER_ */
