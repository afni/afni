#define V2PI 6.2831853071795864
#define VPI  3.1415926535897932
#define VHPI 1.5707963267948966
#define VEPS 0.00001

/*! Compute rotation matrix about axis (a,b,c) of angle th radians. */

static THD_dmat33 RCREND_axis_rotmatrix( double th, double a,double b,double c )
{
  THD_dmat33 ips,ims,ss,id , rr ;
  double na,nb,nc, nn , tp ;

  LOAD_DIAG_DMAT(id,1,1,1) ;             /* identity matrix */
  nn = sqrt(a*a + b*b + c*c) ;           /* norm of axis vector */
  tp = 0.5 * (th-floor(th/V2PI)*V2PI) ;  /* half of angle reduced to (0..2*Pi) */

  /* if axis norm is zero, or angle is zero, return identity matrix */

  if( nn < VEPS || fabs(tp) < VEPS || fabs(tp-VPI) < VEPS ) return id ;

  /* if angle is nearly Pi, then use special formula */

  if( fabs(tp-VHPI) < VEPS ){
    na = a/nn ; nb = b/nn ; nc = c/nn ;
    LOAD_DMAT(rr , na*na-nb*nb-nc*nc, 2.0*na*nb        , 2.0*na*nc        ,
                   2.0*na*nb        , nb*nb-na*na-nc*nc, 2.0*nb*nc        ,
                   2.0*na*nc        , 2.0*nb*nc        , nc*nc-na*na-nb*nb ) ;
    return rr ;
  }

  /* otherwise, rr = [I-S] * inv[I+S],                  [  0   nc -nb ]
     where skew-symmetric S is given by  S = tan(th/2)* [ -nc  0   na ]
                                                        [  nb -na  0  ]  */

  ss.mat[0][0] = ss.mat[1][1] = ss.mat[2][2] = 0.0 ;
  nn = tan(tp)/nn ;
  na = a*nn ; ss.mat[1][2] =  na ; ss.mat[2][1] = -na ;
  nb = b*nn ; ss.mat[0][2] = -nb ; ss.mat[2][0] =  nb ;
  nc = c*nn ; ss.mat[0][1] =  nc ; ss.mat[1][0] = -nc ;
  ims = SUB_DMAT(id,ss) ;
  ips = ADD_DMAT(id,ss) ; ss = DMAT_INV(ips) ;
  rr  = DMAT_MUL(ims,ss) ;

  return rr ;
}
