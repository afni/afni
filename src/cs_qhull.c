#include "cs.h"

/*----------------------------------------------------
  Compute the convex hull of a bunch of 3-vectors
  Inputs:
    npt = number of vectors
    xyz = array of coordinates of 3-vectors;
          the i-th vector is stored in
            xyz[3*i] xyz[3*i+1] xyz[3*i+2]
  Output:
    *ijk = pointer to malloc()-ed array of triangles;
           the j-th triangle is stored in
             ijk[3*j] ijk[3*j+1] ijk[3*j+2]
           where the integer index i refers to the
           i-th 3-vector input

  Return value is the number of triangles.  If this
  is zero, something bad happened.

  Example:
    int ntri , *tri , nvec ;
    float vec[something] ;
    ntri = qhull_wrap( nvec , vec , &tri ) ;

  This function just executes the Geometry Center
  program qhull to compute the result.  This program
  should be in the user's path, or this function
  will fail (return 0).
------------------------------------------------------*/

int qhull_wrap( int npt , float * xyz , int ** ijk )
{
   int ii,jj , nfac , *fac ;
   int fd ; FILE *fp ;
   char qbuf[128] ;

#ifndef DONT_USE_MKSTEMP
   char fname[] = "/tmp/afniXXXXXX" ;
#else
   char *fname ;
#endif

   if( npt < 3 || xyz == NULL || ijk == NULL ){
      fprintf(stderr,"qhull_wrap: bad inputs\n") ;
      return 0 ;
   }

#ifndef DONT_USE_MKSTEMP
   fd = mkstemp( fname ) ;
   if( fd == -1 ){ fprintf(stderr,"qhull_wrap: mkstemp fails\n"); return 0; }
   fp = fdopen( fd , "w" ) ;
   if( fp == NULL ){ fprintf(stderr,"qhull_wrap: fdopen fails\n"); close(fd); return 0; }
#else
   fname = tmpnam(NULL) ;
   if( fname == NULL ){ fprintf(stderr,"qhull_wrap: tmpnam fails\n"); return 0; }
   fp = fopen( fname , "w" ) ;
   if( fp == NULL ){ fprintf(stderr,"qhull_wrap: fopen fails\n"); return 0; }
#endif

   fprintf(fp,"3\n%d\n",npt) ;
   for( ii=0 ; ii < npt ; ii++ )
      fprintf(fp,"%g %g %g\n",xyz[3*ii],xyz[3*ii+1],xyz[3*ii+2]) ;

   fclose(fp) ;

   sprintf(qbuf,"qhull -i -Pp < %s",fname) ;
   fp = popen( qbuf , "r" ) ;
   if( fp == NULL ){ fprintf(stderr,"qhull_wrap: popen fails\n"); remove(fname); return 0; }

   jj = fscanf(fp,"%d",&nfac) ;
   if( jj != 1 || nfac < 1 ){ fprintf(stderr,"qhull_wrap: 1st fscanf fails\n"); pclose(fp); remove(fname); return 0; }

   fac = (int *) malloc( sizeof(int)*3*nfac ) ;
   if( fac == NULL ){ fprintf(stderr,"qhull_wrap: malloc fails\n"); pclose(fp); remove(fname); return 0; }

   for( ii=0 ; ii < nfac ; ii++ ){
      jj = fscanf(fp,"%d %d %d",fac+(3*ii),fac+(3*ii+1),fac+(3*ii+2)) ;
      if( jj < 3 ){
         fprintf(stderr,"qhull_wrap: fscanf fails at ii=%d\n",ii) ;
         pclose(fp); remove(fname); free(fac); return 0;
      }
   }

   pclose(fp); remove(fname);

   *ijk = fac ; return nfac ;
}

/*------------------------------------------------------------------
   Compute the Voronoi areas for a collection of points on the
   surface of the unit sphere:
     npt    = number of points
     pol[i] = polar angle (co-latitude) of i-th point (radians)
     azi[i] = azimuthal angle (longitude) of i-th point (radians)
     *wt    = malloc()-ed array holding the output; the area attached
              to the i-th input point is in wt[i]
   Return value is 0 if an error transpired, or 1 if all went OK.
   If return is 0, then *wt is not changed.
--------------------------------------------------------------------*/

int sphere_voronoi_angles( int npt , float *pol , float *azi , float **wt )
{
   float *xyz ;
   double cp,sp,ca,sa ;
   int ii ;

   if( npt < 3 || pol == NULL || azi == NULL || wt == NULL ){
      fprintf(stderr,"sphere_voronoi_angles: bad inputs\n"); return 0;
   }

   /* make 3-vectors of the points on the sphere */

   xyz = (float *) malloc( sizeof(float) * (3*npt) ) ;

   for( ii=0 ; ii < npt ; ii++ ){
      cp = cos(pol[ii]) ; sp = sin(pol[ii]) ;
      ca = cos(azi[ii]) ; sa = sin(azi[ii]) ;
      xyz[3*ii]   = sp * ca ;
      xyz[3*ii+1] = sp * sa ;
      xyz[3*ii+2] = cp ;
   }

   ii = sphere_voronoi_vectors( npt , xyz , wt ) ;
   free(xyz) ; return ii ;
}

/*------------------------------------------------------------------------
   Same as above, but points on surface of sphere are specified by
   unit 3-vectors; i-th vector is in xyz[3*ii], xyz[3*ii+1], xyz[3*ii+2]
   N.B.: if the 3-vectors are NOT unit vectors, the results of this
         routine are erroneous!
--------------------------------------------------------------------------*/

int sphere_voronoi_vectors( int npt , float *xyz , float **wt )
{
   int ntri , *tri , ii,jj , pp,qq,rr ;
   float *ww ;
   double cp,sp,ca,sa ;
   double xpq,ypq,zpq , xpr,ypr,zpr , xqr,yqr,zqr , xcc,ycc,zcc ;
   double xpp,ypp,zpp , xqq,yqq,zqq , xrr,yrr,zrr , xnn,ynn,znn ;
   double pp_pq , pp_pr , pp_cc ,
          qq_pq , qq_qr , qq_cc ,
          rr_qr , rr_cc , rr_pr ,
          pq_cc , qr_cc , pr_cc , ss ;
   double a_pp_pq_cc , a_pp_pr_cc ,
          a_qq_pq_cc , a_qq_qr_cc ,
          a_rr_qr_cc , a_rr_pr_cc  ;

   if( npt < 3 || xyz == NULL || wt == NULL ){
      fprintf(stderr,"sphere_voronoi: bad inputs\n"); return 0;
   }

   /* compute convex hull triangular facets */

   ntri = qhull_wrap( npt , xyz , &tri ) ;
   if( ntri == 0 ){ fprintf(stderr,"sphere_voronoi: qhull_wrap fails\n"); free(xyz); return 0; }

   /* initialize output */

   ww = (float *) malloc( sizeof(float) * npt ) ;
   for( ii=0 ; ii < npt ; ii++ ) ww[ii] = 0.0 ;

   for( jj=0 ; jj < ntri ; jj++ ){  /* loop over triangles */


      /* triangle vertices pp,qq,rr      * pp
                                        / \
                                       /   \
                                   pq *     * pr
                                     /   *   \
                                    /   cc    \
                                qq *-----------* rr   */

      pp  = tri[3*jj  ] ;
      xpp = xyz[3*pp  ] ; ypp = xyz[3*pp+1] ; zpp = xyz[3*pp+2] ;
      qq  = tri[3*jj+1] ;
      xqq = xyz[3*qq  ] ; yqq = xyz[3*qq+1] ; zqq = xyz[3*qq+2] ;
      rr  = tri[3*jj+2] ;
      xrr = xyz[3*rr  ] ; yrr = xyz[3*rr+1] ; zrr = xyz[3*rr+2] ;

      /* midpoints pq,pr,qr, and centroid cc */
      /*** Q: should centroid be replaced by normal?
              what about orientation, largeness?     ***/

      xpq = 0.5*(xpp+xqq) ; ypq = 0.5*(ypp+yqq) ; zpq = 0.5*(zpp+zqq) ;
      xpr = 0.5*(xpp+xrr) ; ypr = 0.5*(ypp+yrr) ; zpr = 0.5*(zpp+zrr) ;
      xqr = 0.5*(xqq+xrr) ; yqr = 0.5*(yqq+yrr) ; zqr = 0.5*(zqq+zrr) ;

      xcc = 0.3333333*(xpp+xqq+xrr) ;
      ycc = 0.3333333*(ypp+yqq+yrr) ;
      zcc = 0.3333333*(zpp+zqq+zrr) ;

#undef SCL
#define SCL(a,b,c) 1.0/sqrt(a*a+b*b+c*c)

#undef USE_NORMAL
#ifdef USE_NORMAL
# define XCROSS(a,b,c,x,y,z) ((b)*(z)-(c)*(y))
# define YCROSS(a,b,c,x,y,z) ((c)*(x)-(a)*(z))
# define ZCROSS(a,b,c,x,y,z) ((a)*(y)-(b)*(x))
      { double apq=xpp-xqq , bpq=ypp-yqq , cpq=zpp-zqq ,
               aqr=xqq-xrr , bqr=yqq-yrr , cqr=zqq-zrr  ;

        xnn = XCROSS(apq,bpq,cpq,aqr,bqr,cqr) ,
        ynn = YCROSS(apq,bpq,cpq,aqr,bqr,cqr) ,
        znn = ZCROSS(apq,bpq,cpq,aqr,bqr,cqr)  ;

        cp = SCL(xnn,ynn,znn) ; xnn *= cp ; ynn *= cp ; znn *= cp ;
        if( xnn*xcc + ynn*ycc + znn*zcc < 0 ){
           xnn = -xnn ; ynn = -ynn ; znn = -znn ;
        }
      }

# define xVV xnn
# define yVV ynn
# define zVV znn

#else

# define xVV xcc
# define yVV ycc
# define zVV zcc

#endif

      /* project pq,pr,qr,cc to sphere (nn is already on sphere) */

      cp = SCL(xpq,ypq,zpq) ; xpq *= cp ; ypq *= cp ; zpq *= cp ;
      cp = SCL(xpr,ypr,zpr) ; xpr *= cp ; ypr *= cp ; zpr *= cp ;
      cp = SCL(xqr,yqr,zqr) ; xqr *= cp ; yqr *= cp ; zqr *= cp ;
      cp = SCL(xcc,ycc,zcc) ; xcc *= cp ; ycc *= cp ; zcc *= cp ;

#undef  ANG
#define ANG(u1,u2,u3,v1,v2,v3) acos(u1*v1+u2*v2+u3*v3)

      /* compute distance on surface between points:
         aa_bb = between points aa and bb, from the picture above */

      pp_pq = ANG( xpp,ypp,zpp , xpq,ypq,zpq ) ;
      pp_cc = ANG( xpp,ypp,zpp , xVV,yVV,zVV ) ;
      pp_pr = ANG( xpp,ypp,zpp , xpr,ypr,zpr ) ;

      qq_pq = ANG( xqq,yqq,zqq , xpq,ypq,zpq ) ;
      qq_qr = ANG( xqq,yqq,zqq , xqr,yqr,zqr ) ;
      qq_cc = ANG( xqq,yqq,zqq , xVV,yVV,zVV ) ;

      rr_qr = ANG( xrr,yrr,zrr , xqr,yqr,zqr ) ;
      rr_pr = ANG( xrr,yrr,zrr , xpr,ypr,zpr ) ;
      rr_cc = ANG( xrr,yrr,zrr , xVV,yVV,zVV ) ;

      pq_cc = ANG( xpq,ypq,zpq , xVV,yVV,zVV ) ;
      qr_cc = ANG( xqr,yqr,zqr , xVV,yVV,zVV ) ;
      pr_cc = ANG( xpr,ypr,zpr , xVV,yVV,zVV ) ;

      /* for each vertex,
         compute areas of 2 subtriangles it touches,
         add these into the area weight for that vertex */

#undef  SS
#define SS(a,b,c) ((a+b+c)/2)
#undef  ATR
#define ATR(s,a,b,c) (4*atan(sqrt(tan(s/2)*tan((s-a)/2)*tan((s-b)/2)*tan((s-c)/2))))

      ss      = SS(pp_pq,pp_cc,pq_cc) ;     /* subtriangle pp,pq,cc */
      ww[pp] += a_pp_pq_cc = ATR(ss,pp_pq,pp_cc,pq_cc) ;

      ss      = SS(pp_pr,pp_cc,pr_cc) ;     /* subtriangle pp,pr,cc */
      ww[pp] += a_pp_pr_cc = ATR(ss,pp_pr,pp_cc,pr_cc) ;

      ss      = SS(qq_pq,qq_cc,pq_cc) ;     /* subtriangle qq,pq,cc */
      ww[qq] += a_qq_pq_cc = ATR(ss,qq_pq,qq_cc,pq_cc) ;

      ss      = SS(qq_qr,qq_cc,qr_cc) ;     /* subtriangle qq,qr,cc */
      ww[qq] += a_qq_qr_cc = ATR(ss,qq_qr,qq_cc,qr_cc) ;

      ss      = SS(rr_qr,rr_cc,qr_cc) ;     /* subtriangle rr,qr,cc */
      ww[rr] += a_rr_qr_cc = ATR(ss,rr_qr,rr_cc,qr_cc) ;

      ss      = SS(rr_pr,rr_cc,pr_cc) ;     /* subtriangle rr,pr,cc */
      ww[rr] += a_rr_pr_cc = ATR(ss,rr_pr,rr_cc,pr_cc) ;


#if 0          /* debugging printouts */
# undef  DDD
# define DDD(x,y,z,a,b,c) sqrt((x-a)*(x-a)+(y-b)*(y-b)+(z-c)*(z-c))

      cp = DDD(xpp,ypp,zpp,xqq,yqq,zqq) ;
      sp = DDD(xpp,ypp,zpp,xrr,yrr,zrr) ;
      ca = DDD(xqq,yqq,zqq,xrr,yrr,zrr) ;
      sa = (cp+sp+ca)/2 ;
      ss = sqrt(sa*(sa-cp)*(sa-sp)*(sa-ca)) ;

      fprintf(stderr,"triangle %d: pp=%d qq=%d rr=%d  AREA=%6.3f PLANAR=%6.3f\n"
                     "  xpp=%6.3f ypp=%6.3f zpp=%6.3f\n"
                     "  xqq=%6.3f yqq=%6.3f zqq=%6.3f\n"
                     "  xrr=%6.3f yrr=%6.3f zrr=%6.3f\n"
                     "  xpq=%6.3f ypq=%6.3f zpq=%6.3f\n"
                     "  xqr=%6.3f yqr=%6.3f zqr=%6.3f\n"
                     "  xpr=%6.3f ypr=%6.3f zpr=%6.3f\n"
                     "  xcc=%6.3f ycc=%6.3f zcc=%6.3f\n"
#ifdef USE_NORMAL
                     "  xnn=%6.3f ynn=%6.3f znn=%6.3f\n"
#endif
                     "  pp_pq=%6.3f pp_pr=%6.3f pp_cc=%6.3f\n"
                     "  qq_pq=%6.3f qq_qr=%6.3f qq_cc=%6.3f\n"
                     "  rr_qr=%6.3f rr_cc=%6.3f rr_pr=%6.3f\n"
                     "  pq_cc=%6.3f qr_cc=%6.3f pr_cc=%6.3f\n"
                     "  a_pp_pq_cc=%6.3f a_pp_pr_cc=%6.3f\n"
                     "  a_qq_pq_cc=%6.3f a_qq_qr_cc=%6.3f\n"
                     "  a_rr_qr_cc=%6.3f a_rr_pr_cc=%6.3f\n" ,
               jj,pp,qq,rr ,
               a_pp_pq_cc+a_pp_pr_cc+a_qq_pq_cc+a_qq_qr_cc+a_rr_qr_cc+a_rr_pr_cc , ss ,
               xpp, ypp, zpp,
               xqq, yqq, zqq,
               xrr, yrr, zrr,
               xpq, ypq, zpq,
               xqr, yqr, zqr,
               xpr, ypr, zpr,
               xcc, ycc, zcc,
#ifdef USE_NORMAL
               xnn, ynn, znn,
#endif
               pp_pq, pp_pr, pp_cc,
               qq_pq, qq_qr, qq_cc,
               rr_qr, rr_cc, rr_pr,
               pq_cc, qr_cc, pr_cc,
               a_pp_pq_cc, a_pp_pr_cc,
               a_qq_pq_cc, a_qq_qr_cc,
               a_rr_qr_cc, a_rr_pr_cc  ) ;
#endif

   } /* end of loop over triangles */

   /* exit stage left */

   *wt = ww ; return 1 ;
}
