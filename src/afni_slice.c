#undef MAIN

/*********************************************************************
   These are the template routines to fill in a slice of data
   in a new image array from an old image brick array, warped on demand.

   To create actual routines for this purpose, you must compile
   the file with the preprocessor symbol DTYPE set to one of
   the following types:

      byte short int float double complex

      cc -c -DDTYPE=short afni_slice.c
      mv -f afni_slice.o afni_slice_short.o

   In the example above, the resulting routines will be named

      AFNI_lmap_to_xslice_short
      AFNI_lmap_to_yslice_short
      AFNI_lmap_to_zslice_short
      AFNI_br2sl_short

   and will take as input pointers bold and bslice, pointers of
   type "short *".
*********************************************************************/

#ifndef DTYPE
#error "Cannot compile, since DTYPE is undefined."
#endif

#include "afni_warp.h"

/** macros for function names defined in this file **/

#define LMAP_XNAME TWO_TWO(AFNI_lmap_to_xslice_,DTYPE)
#define LMAP_YNAME TWO_TWO(AFNI_lmap_to_yslice_,DTYPE)
#define LMAP_ZNAME TWO_TWO(AFNI_lmap_to_zslice_,DTYPE)
#define B2SL_NAME  TWO_TWO(AFNI_br2sl_,DTYPE)

/*******************************************************************
     To add a new DTYPE, you must add definitions in mrilib.h,
     and define the following macros for the new type:
       FMAD2, FMAD4, FSCAL, FINAL, FZERO, and INTYPE
********************************************************************/

/** macros for e = a*d1 + b*d2 (a,b floats; d1,d2 DTYPEs) **/

#define FMAD2_short(a,d1,b,d2,e)   (e)=(a)*(d1)+(b)*(d2)
#define FMAD2_float                FMAD2_short
#define FMAD2_byte                 FMAD2_short
#define FMAD2_int                  FMAD2_short
#define FMAD2_double               FMAD2_short
#define FMAD2_complex(a,d1,b,d2,e) ( (e).r = (a)*(d1).r + (b)*(d2).r, \
                                     (e).i = (a)*(d1).i + (b)*(d2).i   )
#define FMAD2 TWO_TWO(FMAD2_,DTYPE)

/** macros for e = a*d1 + b*d2 + c*d3 + d*d3 (a-d floats; d1-d4 DTYPEs) **/

#define FMAD4_short(a,d1,b,d2,c,d3,d,d4,e)   (e)=(a)*(d1)+(b)*(d2)+(c)*(d3)+(d)*(d4)
#define FMAD4_float                          FMAD4_short
#define FMAD4_byte                           FMAD4_short
#define FMAD4_int                            FMAD4_short
#define FMAD4_double                         FMAD4_short
#define FMAD4_complex(a,d1,b,d2,c,d3,d,d4,e)                              \
             ( (e).r = (a)*(d1).r + (b)*(d2).r + (c)*(d3).r + (d)*(d4).r, \
               (e).i = (a)*(d1).i + (b)*(d2).i + (c)*(d3).i + (d)*(d4).i   )
#define FMAD4 TWO_TWO(FMAD4_,DTYPE)

/** macros to multiply float a times DTYPE b and store the result in b again **/

#define FSCAL_short(a,b)           (b)*=(a)
#define FSCAL_float                FSCAL_short
#define FSCAL_byte                 FSCAL_short
#define FSCAL_int                  FSCAL_short
#define FSCAL_double               FSCAL_short
#define FSCAL_complex(a,b)         ( (b).r *= (a) , (b).i *= (a) )
#define FSCAL TWO_TWO(FSCAL_,DTYPE)

/** macros for assigning final result from INTYPE a to DTYPE b **/

   /* 18 Nov 1998: modify FINAL_short and FINAL_byte to prevent overflow */

#define CLIP_OVERFLOW
#ifdef  CLIP_OVERFLOW

#  define FINAL_short(a,b) (b) = ( ((a)<-32767.0) ? (-32767) : \
                                   ((a)> 32767.0) ? ( 32767) : ((short)((a)+0.5)) )

#  define FINAL_byte(a,b)  (b) = ( ((a)<   0.0) ? (0)   : \
                                   ((a)> 255.0) ? (255) : ((byte)((a)+0.5)) )

#else
# define FINAL_short(a,b)           (b)=((short)((a)+0.5))
# define FINAL_byte(a,b)            (b)=((byte)((a)+0.5))
#endif

#define FINAL_int(a,b)             (b)=((int)((a)+0.5))
#define FINAL_float(a,b)           (b)=(a)
#define FINAL_double               FINAL_float
#define FINAL_complex              FINAL_float
#define FINAL TWO_TWO(FINAL_,DTYPE)

/** macros for putting a zero into DTYPE b **/

#define FZERO_short(b)             (b)=0
#define FZERO_byte                 FZERO_short
#define FZERO_int                  FZERO_short
#define FZERO_float(b)             (b)=0.0
#define FZERO_double               FZERO_float
#define FZERO_complex(b)           ( (b).r = 0.0 , (b).i = 0.0 )
#define FZERO TWO_TWO(FZERO_,DTYPE)

/** macros for a zero value **/

static complex complex_zero = { 0.0,0.0 } ;

#define ZERO_short    0
#define ZERO_byte     0
#define ZERO_int      0
#define ZERO_float    0.0
#define ZERO_double   0.0
#define ZERO_complex  complex_zero
#define ZERO          TWO_TWO(ZERO_,DTYPE)

/** macros for intermediate interpolants data type **/

#define INTYPE_short    float
#define INTYPE_float    float
#define INTYPE_byte     float
#define INTYPE_int      float
#define INTYPE_double   double
#define INTYPE_complex  complex
#define INTYPE TWO_TWO(INTYPE_,DTYPE)

/** debugging macros **/

#ifdef AFNI_DEBUG
#  define USE_TRACING
#endif
#include "dbtrace.h"

/**-------------------------------------------------------------------------**/
/**----- macros for quickly doing NN interpolation along parallel axes -----**/

/** test and flags for which steps are zero **/

#define ZZZ           1.e-5  /* effectively zero */
#define NONE_ZERO     0
#define X_ZERO        1
#define Y_ZERO        2
#define XY_ZERO       3
#define Z_ZERO        4
#define XZ_ZERO       5
#define YZ_ZERO       6
#define XYZ_ZERO      7
#define OUTADD      100
#define THREEZ(x,y,z) ((fabs(x)<ZZZ) + 2*(fabs(y)<ZZZ) + 4*(fabs(z)<ZZZ))

/** ALOOP: inner loop statements for the actual NN assignments,
             and there is no need to check for being outside the input brick
    _GEN:  for the general (non-parallel) case
    _PAR:  for the case when the inner loop is parallel to an input brick axis **/

#define NN_ALOOP_GEN \
 (fxi_old += dfxi_inner , fyj_old += dfyj_inner , fzk_old += dfzk_inner , \
  xi_old = FLOOR(fxi_old) , yj_old = FLOOR(fyj_old) , zk_old = FLOOR(fzk_old) , \
  bslice[out_ind++] = bold[ IBASE(xi_old,yj_old,zk_old) ])

#define NN_ALOOP_PAR(ijk) (bslice[out_ind++] = bold[ ib[ijk]+ob ])

/** BLOOP: assign values to the ib array that will be used to
           index into the input brick for rapid access;
           there is one BLOOP for each possible parallel axis **/

#define NN_BLOOP_XY_ZERO(ijk) \
 (fzk_old += dfzk_inner , zk_old = GFLOOR(fzk_old) , ib[ijk] = IBASE(0,0,zk_old))

#define NN_BLOOP_XZ_ZERO(ijk) \
 (fyj_old += dfyj_inner , yj_old = GFLOOR(fyj_old) , ib[ijk] = IBASE(0,yj_old,0))

#define NN_BLOOP_YZ_ZERO(ijk) \
 (fxi_old += dfxi_inner , xi_old = GFLOOR(fxi_old) , ib[ijk] = IBASE(xi_old,0,0))

/** macros to test if the point we want to resample from is outside the old array **/

#define TEST_OUT_XXX ( fxi_old < fxi_bot || fxi_old > fxi_top )
#define TEST_OUT_YYY ( fyj_old < fyj_bot || fyj_old > fyj_top )
#define TEST_OUT_ZZZ ( fzk_old < fzk_bot || fzk_old > fzk_top )

#define TEST_OUT_ALL (TEST_OUT_XXX || TEST_OUT_YYY || TEST_OUT_ZZZ)

/** CLOOP: like the ALOOP, but for when we must test
           if the desired point may be outside the input brick array **/

#define NN_CLOOP_GEN                                                      \
 (fxi_old += dfxi_inner , fyj_old += dfyj_inner , fzk_old += dfzk_inner , \
  bslice[out_ind++] = (TEST_OUT_ALL) ? ZERO :                             \
                      bold[IBASE(FLOOR(fxi_old),FLOOR(fyj_old),FLOOR(fzk_old))] )

#define NN_CLOOP_PAR(ijk) \
 (bslice[out_ind++] = (ib[ijk]<0 || ib[ijk]>=ub) ? ZERO : bold[ib[ijk]+ob])

/** ZLOOP: just assign zero to each output **/

#define NN_ZLOOP (bslice[out_ind++] = ZERO)

/** space for precomputed indices **/

static int * ib = NULL ;
static int  nib = -1 ;

/** macro to make the ib array as big as we need it **/

#define MAKE_IBIG(top) do{ if(nib < (top)){                                 \
                              if(ib != NULL) free(ib) ;                     \
                              ib  = (int *) malloc(sizeof(int)*((top)+9)) ; \
                              if(ib==NULL){                                 \
                                 fprintf(stderr,"\nmalloc fails in NN reslice!\n");exit(1);} \
                              nib = (top) ; } } while(0)

/*---------------------------------------------------------------------
   routine to apply a linear mapping to a dataset and fill in
   a single fixed-x slice from a 3D dataset:

     map        = linear mapping from old to new
                   (bot & top field contain index limits in new)
     resam_mode = type of interpolation to do in old

     old_daxes  = axis information of old dataset
     bold       = pointer to old dataset's 3D data brick array

     new_daxes  = axis information of new dataset
     xi_fix     = fixed x index in new dataset
     bslice     = pointer to nynew * nznew array to hold slice

  Cognate routines for yslice (output is nznew * nxnew: Y-ZX) and
                       zslize (output is nxnew * nynew: Z-XY)
  follow this one directly.
-----------------------------------------------------------------------*/

#define IBASE(i,j,k) ((i)+(j)*jstep+(k)*kstep)
#define ROUND(qq)    ((int)(qq+0.5))
#define FLOOR(qq)    ((int)(qq))          /* cheap and fast */
#define GFLOOR(qq)   ((int)floor(qq))     /* good and slow */

/* define linear interpolation polynomials */

#define LP_00(x) (1.0-(x))
#define LP_P1(x) (x)

/* define blocky interpolation functions */

#define BP_hh(x) (8.0*((x)*(x))*((x)*(x)))
#define BP_00(x) ( ((x)<0.5) ? (1-BP_hh(x)) : (  BP_hh(1-(x))) )
#define BP_P1(x) ( ((x)<0.5) ? (  BP_hh(x)) : (1-BP_hh(1-(x))) )

/* define cubic interpolation polynomials */

#define CP_M1(x)  (-(x)*((x)-1)*((x)-2))
#define CP_00(x)  (3.0*((x)+1)*((x)-1)*((x)-2))
#define CP_P1(x)  (-3.0*(x)*((x)+1)*((x)-2))
#define CP_P2(x)  ((x)*((x)+1)*((x)-1))
#define CP_FACTOR  4.62962963e-3   /* 1/216 = final scaling factor */

#define FXYZTMP(xx,yy,zz)                            \
       ( fxi_tmp =   mt.mat[0][0] * xx               \
                   + mt.mat[0][1] * yy               \
                   + mt.mat[0][2] * zz - vt.xyz[0] , \
         fyj_tmp =   mt.mat[1][0] * xx               \
                   + mt.mat[1][1] * yy               \
                   + mt.mat[1][2] * zz - vt.xyz[1] , \
         fzk_tmp =   mt.mat[2][0] * xx               \
                   + mt.mat[2][1] * yy               \
                   + mt.mat[2][2] * zz - vt.xyz[2] )


void LMAP_XNAME( THD_linear_mapping * map , int resam_mode ,
                 THD_dataxes * old_daxes , DTYPE * bold ,
                 THD_dataxes * new_daxes , int xi_fix , DTYPE * bslice )
{
   THD_mat33 mt = map->mbac ;  /* map from bslice indices to bold */
   THD_fvec3 vt = map->svec ;

   int   xi_new  , yj_new  , zk_new  ;  /* voxel indices in new */
   int   xi_old  , yj_old  , zk_old  ;  /* voxel indices in old */
   float fxi_old , fyj_old , fzk_old ;  /* voxel indices in old */

   float fxi_top    , fyj_top    , fzk_top    ;  /* floating pt. voxel indices */
   float fxi_bot    , fyj_bot    , fzk_bot    ;
   float fxi_base   , fyj_base   , fzk_base   ;
   float dfxi_outer , dfyj_outer , dfzk_outer ;
   float dfxi_inner , dfyj_inner , dfzk_inner ;

   int xi_bot,xi_top , yj_bot,yj_top , zk_bot,zk_top ;  /* ranges in new */
   int out_ind , jstep , kstep ;
   int nxold,nyold,nzold , nxnew,nynew,nznew ;

  ENTRY("AFNI_lmap_to_xslice") ;

   /*--- set up ranges ---*/

   xi_bot = map->bot.xyz[0] ;  xi_top = map->top.xyz[0] ;
   yj_bot = map->bot.xyz[1] ;  yj_top = map->top.xyz[1] ;
   zk_bot = map->bot.xyz[2] ;  zk_top = map->top.xyz[2] ;

   if( xi_fix < xi_bot || xi_fix > xi_top ) EXRETURN ;  /* map doesn't apply! */

   nxold = old_daxes->nxx ;  nxnew = new_daxes->nxx ;
   nyold = old_daxes->nyy ;  nynew = new_daxes->nyy ;
   nzold = old_daxes->nzz ;  nznew = new_daxes->nzz ;

   jstep = nxold ;
   kstep = nxold * nyold ;

   /* set up base of indices in old */

   xi_new = xi_fix ;
   yj_new = yj_bot-1 ;
   zk_new = zk_bot-1 ;

   fxi_base =   mt.mat[0][0] * xi_new
              + mt.mat[0][1] * yj_new
              + mt.mat[0][2] * zk_new - vt.xyz[0] ;

   fyj_base =   mt.mat[1][0] * xi_new
              + mt.mat[1][1] * yj_new
              + mt.mat[1][2] * zk_new - vt.xyz[1] ;

   fzk_base =   mt.mat[2][0] * xi_new
              + mt.mat[2][1] * yj_new
              + mt.mat[2][2] * zk_new - vt.xyz[2] ;

   dfxi_outer = mt.mat[0][2] ;  /* outer loop is in z = 2 */
   dfyj_outer = mt.mat[1][2] ;
   dfzk_outer = mt.mat[2][2] ;

   dfxi_inner = mt.mat[0][1] ;  /* inner loop is in y = 1 */
   dfyj_inner = mt.mat[1][1] ;
   dfzk_inner = mt.mat[2][1] ;

   fxi_top = nxold - 0.51 ;  fxi_bot = -0.49 ;
   fyj_top = nyold - 0.51 ;  fyj_bot = -0.49 ;
   fzk_top = nzold - 0.51 ;  fzk_bot = -0.49 ;

   switch( resam_mode ){

      default:
      case RESAM_NN_TYPE:{
         float fxi_max , fyj_max , fzk_max ;
         float fxi_min , fyj_min , fzk_min ;
         float fxi_tmp , fyj_tmp , fzk_tmp ;
         int any_outside , all_outside ;

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"NN inner dxyz=%g %g %g  outer dxyz=%g %g %g",
          dfxi_inner,dfyj_inner,dfzk_inner,
          dfxi_outer,dfyj_outer,dfzk_outer ) ; STATUS(str) ; }
#endif
         /** July 15, 1996:
             check if all the points are inside the old grid;
             if so, can use a version of the resampling loop
             that does not need to check each voxel for being
             inside -- hopefully, this will execute more quickly **/

         FXYZTMP(xi_new,yj_bot,zk_bot) ;
         fxi_max = fxi_min = fxi_tmp ;
         fyj_max = fyj_min = fyj_tmp ;
         fzk_max = fzk_min = fzk_tmp ;

         FXYZTMP(xi_new,yj_top,zk_bot) ;
         fxi_max = MAX(fxi_max,fxi_tmp) ; fxi_min = MIN(fxi_min,fxi_tmp) ;
         fyj_max = MAX(fyj_max,fyj_tmp) ; fyj_min = MIN(fyj_min,fyj_tmp) ;
         fzk_max = MAX(fzk_max,fzk_tmp) ; fzk_min = MIN(fzk_min,fzk_tmp) ;

         FXYZTMP(xi_new,yj_bot,zk_top) ;
         fxi_max = MAX(fxi_max,fxi_tmp) ; fxi_min = MIN(fxi_min,fxi_tmp) ;
         fyj_max = MAX(fyj_max,fyj_tmp) ; fyj_min = MIN(fyj_min,fyj_tmp) ;
         fzk_max = MAX(fzk_max,fzk_tmp) ; fzk_min = MIN(fzk_min,fzk_tmp) ;

         FXYZTMP(xi_new,yj_top,zk_top) ;
         fxi_max = MAX(fxi_max,fxi_tmp) ; fxi_min = MIN(fxi_min,fxi_tmp) ;
         fyj_max = MAX(fyj_max,fyj_tmp) ; fyj_min = MIN(fyj_min,fyj_tmp) ;
         fzk_max = MAX(fzk_max,fzk_tmp) ; fzk_min = MIN(fzk_min,fzk_tmp) ;

         any_outside = (fxi_min < fxi_bot) || (fxi_max > fxi_top) ||
                       (fyj_min < fyj_bot) || (fyj_max > fyj_top) ||
                       (fzk_min < fzk_bot) || (fzk_max > fzk_top) ;

         all_outside = (any_outside) ?  (fxi_max < fxi_bot) || (fxi_min > fxi_top) ||
                                        (fyj_max < fyj_bot) || (fyj_min > fyj_top) ||
                                        (fzk_max < fzk_bot) || (fzk_min > fzk_top)
                                     : 0 ;
#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"fxi_bot=%g  fxi_top=%g  fxi_min=%g  fxi_max=%g",fxi_bot,fxi_top,fxi_min,fxi_max);
  STATUS(str) ;
  sprintf(str,"fyj_bot=%g  fyj_top=%g  fyj_min=%g  fyj_max=%g",fyj_bot,fyj_top,fyj_min,fyj_max);
  STATUS(str) ;
  sprintf(str,"fzk_bot=%g  fzk_top=%g  fzk_min=%g  fzk_max=%g",fzk_bot,fzk_top,fzk_min,fzk_max);
  STATUS(str) ; }
#endif

/** redefine the macros specifying loop variables **/

#undef OUD_NAME
#undef IND_NAME
#undef OUD
#undef OUD_bot
#undef OUD_top
#undef IND
#undef IND_bot
#undef IND_top
#undef IND_nnn

#define OUD_NAME zk                        /* name of 2nd dimension of output image */
#define IND_NAME yj                        /* name of 1st dimension of output image */
#define IND_nnn  nynew                     /* inner loop image dimension */

#define OUD     TWO_TWO(OUD_NAME , _new)   /* outer loop index name */
#define OUD_bot TWO_TWO(OUD_NAME , _bot)   /* outer loop index min  */
#define OUD_top TWO_TWO(OUD_NAME , _top)   /* outer loop index max  */
#define IND     TWO_TWO(IND_NAME , _new)   /* inner loop index name */
#define IND_bot TWO_TWO(IND_NAME , _bot)   /* inner loop index min  */
#define IND_top TWO_TWO(IND_NAME , _top)   /* inner loop index max  */

         if( all_outside ){
STATUS("NN resample has all outside") ;
            for( OUD=OUD_bot ; OUD <= OUD_top ; OUD++ ){     /* all points are      */
               out_ind = IND_bot + OUD * IND_nnn ;           /* outside input brick */
               for( IND=IND_bot ; IND <= IND_top ; IND++ ){  /* so just load zeros  */
                  bslice[out_ind++] = ZERO ;
               }
            }
         } else {                                       /* at least some are inside */

            int thz , tho , ob , ub ;

            fxi_base += 0.5 ; fyj_base += 0.5 ; fzk_base += 0.5 ;

            fxi_top = nxold-0.0001 ; fxi_bot = 0.0 ;  /* we can use FLOOR instead */
            fyj_top = nyold-0.0001 ; fyj_bot = 0.0 ;  /* of ROUND to find the NN  */
            fzk_top = nzold-0.0001 ; fzk_bot = 0.0 ;  /* by adding 0.5 to all    */
                                                      /* these variables now.   */

            /** thz = flag that indicates which of the steps df??_inner are zero.
                      If two of them are zero, then the inner loop is parallel to
                      one of the input brick axes, and so data may be pulled
                      out in a very efficient fashion.  In such a case, precompute
                      the indexes for the inner loop:

                      the BLOOP macros load array ib, which holds the inner loop
                        computed indexes for each inner loop position.
                      ub = upper bound value for ib array value to still be
                        inside input brick array.
                      ob = outer loop index into input brick array (computed later) **/

            tho = THREEZ(dfxi_outer,dfyj_outer,dfzk_outer) ;         /* 06 Aug 1996:       */
            if( tho == XY_ZERO || tho == XZ_ZERO || tho == YZ_ZERO ) /* only allow thz to  */
               thz = THREEZ(dfxi_inner,dfyj_inner,dfzk_inner) ;      /* indicate special   */
            else                                                     /* treatment if outer */
               thz = NONE_ZERO ;                                     /* axes are special   */

#ifdef AFNI_DEBUG
{ char str[256] ;
  if( any_outside ) sprintf(str,"NN resample has some outside: thz = %d",thz) ;
  else              sprintf(str,"NN resample has all inside: thz = %d",thz) ;
  STATUS(str) ;
  sprintf(str,"OUD_bot=%d  OUD_top=%d  nxold=%d nyold=%d nzold=%d",
          OUD_bot,OUD_top,nxold,nyold,nzold ) ; STATUS(str) ;
  sprintf(str,"IND_bot=%d  IND_top=%d  nxold=%d nyold=%d nzold=%d",
          IND_bot,IND_top,nxold,nyold,nzold ) ; STATUS(str) ; }
#endif

            switch(thz){
               case XY_ZERO:
                  MAKE_IBIG(IND_top) ;
                  fzk_old = fzk_base + dfzk_outer ; ub = IBASE(0,0,nzold) ;
                  for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_BLOOP_XY_ZERO(IND) ; }
               break ;

               case XZ_ZERO:
                  MAKE_IBIG(IND_top) ;
                  fyj_old = fyj_base + dfyj_outer ; ub = IBASE(0,nyold,0) ;
                  for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_BLOOP_XZ_ZERO(IND) ; }
               break ;

               case YZ_ZERO:
                  MAKE_IBIG(IND_top) ;
                  fxi_old = fxi_base + dfxi_outer ; ub = IBASE(nxold,0,0) ;
                  for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_BLOOP_YZ_ZERO(IND) ; }
               break ;
            }

            thz += OUTADD * any_outside ;

STATUS("beginning NN outer loop") ;

            /*** outer loop ***/

            for( OUD=OUD_bot ; OUD <= OUD_top ; OUD++ ){

               fxi_old = (fxi_base += dfxi_outer) ;  /* floating indexes in  */
               fyj_old = (fyj_base += dfyj_outer) ;  /* input brick at start */
               fzk_old = (fzk_base += dfzk_outer) ;  /* of next inner loop   */

               out_ind = IND_bot + OUD * IND_nnn ;   /* index into output brick */

               /*** There are 8 cases for the inner loop:
                      all inside, inner loop not parallel to any input axis
                      all inside, inner loop parallel to input brick z-axis
                      all inside, inner loop parallel to input brick y-axis
                      all inside, inner loop parallel to input brick x-axis

                    and then the 4 same cases repeated when not all desired
                      points are inside the input brick.  Each of these is
                      coded separately for efficiency.  This is important for
                      rapid re-display of results during interactive imaging. ***/

               switch(thz){
                  case NONE_ZERO:
                  case X_ZERO:
                  case Y_ZERO:
                  case Z_ZERO:
                  case XYZ_ZERO:
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ALOOP_GEN ; }
                  break ;

                  case XY_ZERO:
                     xi_old = FLOOR( fxi_old ) ; yj_old = FLOOR( fyj_old ) ;
                     ob = IBASE(xi_old,yj_old,0) ;
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ALOOP_PAR(IND) ; }
                  break ;

                  case XZ_ZERO:
                     xi_old = FLOOR( fxi_old ) ; zk_old = FLOOR( fzk_old ) ;
                     ob = IBASE(xi_old,0,zk_old) ;
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ALOOP_PAR(IND) ; }
                  break ;

                  case YZ_ZERO:
                     yj_old = FLOOR( fyj_old ) ; zk_old = FLOOR( fzk_old ) ;
                     ob = IBASE(0,yj_old,zk_old) ;
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ALOOP_PAR(IND) ; }
                  break ;

                  default:
                  case NONE_ZERO+OUTADD:
                  case X_ZERO+OUTADD:
                  case Y_ZERO+OUTADD:
                  case Z_ZERO+OUTADD:
                  case XYZ_ZERO+OUTADD:
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_CLOOP_GEN ; }
                  break ;

                  case XY_ZERO+OUTADD:
                     xi_old = FLOOR( fxi_old ) ; yj_old = FLOOR( fyj_old ) ;
                     if( TEST_OUT_XXX || TEST_OUT_YYY ){
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ZLOOP ; }
                     } else {
                        ob = IBASE(xi_old,yj_old,0) ;
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_CLOOP_PAR(IND) ; }
                     }
                  break ;

                  case XZ_ZERO+OUTADD:
                     xi_old = FLOOR( fxi_old ) ; zk_old = FLOOR( fzk_old ) ;
                     if( TEST_OUT_XXX || TEST_OUT_ZZZ ){
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ZLOOP ; }
                     } else {
                        ob = IBASE(xi_old,0,zk_old) ;
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_CLOOP_PAR(IND) ; }
                     }
                  break ;

                  case YZ_ZERO+OUTADD:
                     yj_old = FLOOR( fyj_old ) ; zk_old = FLOOR( fzk_old ) ;
                     if( TEST_OUT_YYY || TEST_OUT_ZZZ ){
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ZLOOP ; }
                     } else {
                        ob = IBASE(0,yj_old,zk_old) ;
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_CLOOP_PAR(IND) ; }
                     }
                  break ;
               }
            }
         }
      }
      break ;  /* end of NN! */

      case RESAM_BLOCK_TYPE:
      case RESAM_LINEAR_TYPE:{
         float  xwt_00,xwt_p1 , ywt_00,ywt_p1 , zwt_00,zwt_p1 ;
         INTYPE f_j00_k00 , f_jp1_k00 , f_j00_kp1 , f_jp1_kp1 ,
                f_k00     , f_kp1 , result ;
         float frac_xi , frac_yj , frac_zk ;
         int   ibase ,
               in_jp1_k00 = jstep ,
               in_j00_kp1 = kstep ,
               in_jp1_kp1 = jstep+kstep ;
         int   nxold1 = nxold-1 , nyold1 = nyold-1 , nzold1 = nzold-1 ;

         for( zk_new=zk_bot ; zk_new <= zk_top ; zk_new++ ){

            fxi_old = (fxi_base += dfxi_outer) ;
            fyj_old = (fyj_base += dfyj_outer) ;
            fzk_old = (fzk_base += dfzk_outer) ;

            out_ind = yj_bot + zk_new * nynew ;

            for( yj_new=yj_bot ; yj_new <= yj_top ; yj_new++ ){

               fxi_old += dfxi_inner ; fyj_old += dfyj_inner ; fzk_old += dfzk_inner ;

               if( fxi_old < fxi_bot || fxi_old > fxi_top ||
                   fyj_old < fyj_bot || fyj_old > fyj_top ||
                   fzk_old < fzk_bot || fzk_old > fzk_top   ){  /* outside */

/***
                  bslice[out_ind++] = 0 ;
***/
                  FZERO(bslice[out_ind]) ; out_ind++ ;
                  continue ;
               }

               xi_old = FLOOR(fxi_old) ; frac_xi = fxi_old - xi_old ;
               yj_old = FLOOR(fyj_old) ; frac_yj = fyj_old - yj_old ;
               zk_old = FLOOR(fzk_old) ; frac_zk = fzk_old - zk_old ;

               /* use NN if at edges of old brick */

               if( xi_old == nxold1 || yj_old == nyold1 || zk_old == nzold1 ||
                   frac_xi < 0      || frac_yj < 0      || frac_zk < 0        ){

                  bslice[out_ind++] = bold[ IBASE( ROUND(fxi_old) ,
                                                   ROUND(fyj_old) ,
                                                   ROUND(fzk_old)  ) ] ;
                  continue ;
               }

               /* compute weights for LINEAR interpolation in each direction */

               if( resam_mode == RESAM_BLOCK_TYPE ){
                  xwt_00 = BP_00(frac_xi) ; xwt_p1 = BP_P1(frac_xi) ;
                  ywt_00 = BP_00(frac_yj) ; ywt_p1 = BP_P1(frac_yj) ;
                  zwt_00 = BP_00(frac_zk) ; zwt_p1 = BP_P1(frac_zk) ;
               } else {
                  xwt_00 = LP_00(frac_xi) ; xwt_p1 = LP_P1(frac_xi) ;
                  ywt_00 = LP_00(frac_yj) ; ywt_p1 = LP_P1(frac_yj) ;
                  zwt_00 = LP_00(frac_zk) ; zwt_p1 = LP_P1(frac_zk) ;
               }

               /* interpolate in the x direction for each y & z */

               ibase = IBASE(xi_old,yj_old,zk_old) ;
/***
               f_j00_k00 =  xwt_00 * bold[ibase]
                          + xwt_p1 * bold[ibase+1] ;

               f_jp1_k00 =  xwt_00 * bold[ibase+in_jp1_k00]
                          + xwt_p1 * bold[ibase+in_jp1_k00+1] ;

               f_j00_kp1 =  xwt_00 * bold[ibase+in_j00_kp1]
                          + xwt_p1 * bold[ibase+in_j00_kp1+1] ;

               f_jp1_kp1 =  xwt_00 * bold[ibase+in_jp1_kp1]
                          + xwt_p1 * bold[ibase+in_jp1_kp1+1] ;
***/
               FMAD2( xwt_00 , bold[ibase]   ,
                      xwt_p1 , bold[ibase+1] ,            f_j00_k00 ) ;

               FMAD2( xwt_00 , bold[ibase+in_jp1_k00]   ,
                      xwt_p1 , bold[ibase+in_jp1_k00+1] , f_jp1_k00 ) ;

               FMAD2( xwt_00 , bold[ibase+in_j00_kp1]   ,
                      xwt_p1 , bold[ibase+in_j00_kp1+1] , f_j00_kp1 ) ;

               FMAD2( xwt_00 , bold[ibase+in_jp1_kp1]   ,
                      xwt_p1 , bold[ibase+in_jp1_kp1+1] , f_jp1_kp1 ) ;

               /* interpolate in the y direction for each z */

/***
               f_k00 =  ywt_00 * f_j00_k00 + ywt_p1 * f_jp1_k00 ;
               f_kp1 =  ywt_00 * f_j00_kp1 + ywt_p1 * f_jp1_kp1 ;
***/
               FMAD2( ywt_00 , f_j00_k00 , ywt_p1 , f_jp1_k00 , f_k00 ) ;
               FMAD2( ywt_00 , f_j00_kp1 , ywt_p1 , f_jp1_kp1 , f_kp1 ) ;

               /* interpolate in the z direction and
                  put the result into the output array!
                  (the +0.5 is to force rounding of the result) */

/***
               bslice[out_ind++] = zwt_00 * f_k00 + zwt_p1 * f_kp1 + 0.5 ;
***/
               FMAD2( zwt_00 , f_k00 , zwt_p1 , f_kp1 , result ) ;
               FINAL( result , bslice[out_ind] ) ;
               out_ind++ ;

            }
         }
      }
      break ;

      case RESAM_CUBIC_TYPE:{
         float xwt_m1,xwt_00,xwt_p1,xwt_p2 ,   /* interpolation weights */
               ywt_m1,ywt_00,ywt_p1,ywt_p2 ,
               zwt_m1,zwt_00,zwt_p1,zwt_p2  ;

         INTYPE f_jm1_km1, f_j00_km1, f_jp1_km1, f_jp2_km1 , /* interpolants */
                f_jm1_k00, f_j00_k00, f_jp1_k00, f_jp2_k00 ,
                f_jm1_kp1, f_j00_kp1, f_jp1_kp1, f_jp2_kp1 ,
                f_jm1_kp2, f_j00_kp2, f_jp1_kp2, f_jp2_kp2 ,
                f_km1    , f_k00    , f_kp1    , f_kp2 , result ;
         float frac_xi , frac_yj , frac_zk ;

         int   ibase ,                        /* base index for interpolant */
               in_jm1_km1 = -jstep-  kstep ,  /* offsets for -1 (m1) */
               in_jm1_k00 = -jstep         ,  /*              0 (00) */
               in_jm1_kp1 = -jstep+  kstep ,  /*             +1 (p1) */
               in_jm1_kp2 = -jstep+2*kstep ,  /*             +2 (p2) */
               in_j00_km1 =       -  kstep ,  /* steps in j and k indices */
               in_j00_k00 = 0              ,
               in_j00_kp1 =          kstep ,
               in_j00_kp2 =        2*kstep ,
               in_jp1_km1 =  jstep-  kstep ,
               in_jp1_k00 =  jstep         ,
               in_jp1_kp1 =  jstep+  kstep ,
               in_jp1_kp2 =2*jstep+2*kstep ,
               in_jp2_km1 =2*jstep-  kstep ,
               in_jp2_k00 =2*jstep         ,
               in_jp2_kp1 =2*jstep+  kstep ,
               in_jp2_kp2 =2*jstep+2*kstep  ;

         int   nxold1 = nxold-1 , nyold1 = nyold-1 , nzold1 = nzold-1 ;
         int   nxold2 = nxold-2 , nyold2 = nyold-2 , nzold2 = nzold-2 ;

         for( zk_new=zk_bot ; zk_new <= zk_top ; zk_new++ ){

            fxi_old = (fxi_base += dfxi_outer) ;
            fyj_old = (fyj_base += dfyj_outer) ;
            fzk_old = (fzk_base += dfzk_outer) ;

            out_ind = yj_bot + zk_new * nynew ;

            for( yj_new=yj_bot ; yj_new <= yj_top ; yj_new++ ){

               fxi_old += dfxi_inner ; fyj_old += dfyj_inner ; fzk_old += dfzk_inner ;

               /* check if outside old brick */

               if( fxi_old < fxi_bot || fxi_old > fxi_top ||
                   fyj_old < fyj_bot || fyj_old > fyj_top ||
                   fzk_old < fzk_bot || fzk_old > fzk_top   ){  /* outside */
/***
                  bslice[out_ind++] = 0 ;
***/
                  FZERO(bslice[out_ind]) ; out_ind++ ;
                  continue ;
               }

               xi_old = FLOOR(fxi_old) ; frac_xi = fxi_old - xi_old ;
               yj_old = FLOOR(fyj_old) ; frac_yj = fyj_old - yj_old ;
               zk_old = FLOOR(fzk_old) ; frac_zk = fzk_old - zk_old ;

               /* use NN if at very edges of old brick */

               if( xi_old == nxold1 || yj_old == nyold1 || zk_old == nzold1 ||
                   frac_xi < 0      || frac_yj < 0      || frac_zk < 0        ){

                  bslice[out_ind++] = bold[ IBASE( ROUND(fxi_old) ,
                                                   ROUND(fyj_old) ,
                                                   ROUND(fzk_old)  ) ] ;
                  continue ;
               }

               ibase = IBASE(xi_old,yj_old,zk_old) ;

               /* use LINEAR if close to edges of old brick */

               if( xi_old == nxold2 || yj_old == nyold2 || zk_old == nzold2 ||
                   xi_old == 0      || yj_old == 0      || zk_old == 0        ){

                  xwt_00 = LP_00(frac_xi) ; xwt_p1 = LP_P1(frac_xi) ;
                  ywt_00 = LP_00(frac_yj) ; ywt_p1 = LP_P1(frac_yj) ;
                  zwt_00 = LP_00(frac_zk) ; zwt_p1 = LP_P1(frac_zk) ;

/***
                  f_j00_k00 =  xwt_00 * bold[ibase]
                             + xwt_p1 * bold[ibase+1] ;

                  f_jp1_k00 =  xwt_00 * bold[ibase+in_jp1_k00]
                             + xwt_p1 * bold[ibase+in_jp1_k00+1] ;

                  f_j00_kp1 =  xwt_00 * bold[ibase+in_j00_kp1]
                             + xwt_p1 * bold[ibase+in_j00_kp1+1] ;

                  f_jp1_kp1 =  xwt_00 * bold[ibase+in_jp1_kp1]
                             + xwt_p1 * bold[ibase+in_jp1_kp1+1] ;
***/
                  FMAD2( xwt_00 , bold[ibase]   ,
                         xwt_p1 , bold[ibase+1] ,            f_j00_k00 ) ;

                  FMAD2( xwt_00 , bold[ibase+in_jp1_k00]   ,
                         xwt_p1 , bold[ibase+in_jp1_k00+1] , f_jp1_k00 ) ;

                  FMAD2( xwt_00 , bold[ibase+in_j00_kp1]   ,
                         xwt_p1 , bold[ibase+in_j00_kp1+1] , f_j00_kp1 ) ;

                  FMAD2( xwt_00 , bold[ibase+in_jp1_kp1]   ,
                         xwt_p1 , bold[ibase+in_jp1_kp1+1] , f_jp1_kp1 ) ;

/***
                  f_k00 =  ywt_00 * f_j00_k00 + ywt_p1 * f_jp1_k00 ;
                  f_kp1 =  ywt_00 * f_j00_kp1 + ywt_p1 * f_jp1_kp1 ;
***/
                  FMAD2( ywt_00 , f_j00_k00 , ywt_p1 , f_jp1_k00 , f_k00 ) ;
                  FMAD2( ywt_00 , f_j00_kp1 , ywt_p1 , f_jp1_kp1 , f_kp1 ) ;
/***
                  bslice[out_ind++] = zwt_00 * f_k00 + zwt_p1 * f_kp1 + 0.5 ;
***/
                  FMAD2( zwt_00 , f_k00 , zwt_p1 , f_kp1 , result ) ;
                  FINAL( result , bslice[out_ind] ) ;
                  out_ind++ ;
                  continue ;
               }

               /* compute weights for CUBIC interpolation in each direction */

               xwt_m1 = CP_M1(frac_xi) ; xwt_00 = CP_00(frac_xi) ;
               xwt_p1 = CP_P1(frac_xi) ; xwt_p2 = CP_P2(frac_xi) ;

               ywt_m1 = CP_M1(frac_yj) ; ywt_00 = CP_00(frac_yj) ;
               ywt_p1 = CP_P1(frac_yj) ; ywt_p2 = CP_P2(frac_yj) ;

               zwt_m1 = CP_M1(frac_zk) ; zwt_00 = CP_00(frac_zk) ;
               zwt_p1 = CP_P1(frac_zk) ; zwt_p2 = CP_P2(frac_zk) ;

/* use the ANSI token-merge operator ## to create an interpolating
   macro for the x-direction, at each offset in y (j) and z (k)    */

/***
#define CXINT(j,k)  xwt_m1 * bold[ibase + in_j ## j ## _k ## k -1 ] \
                  + xwt_00 * bold[ibase + in_j ## j ## _k ## k    ] \
                  + xwt_p1 * bold[ibase + in_j ## j ## _k ## k +1 ] \
                  + xwt_p2 * bold[ibase + in_j ## j ## _k ## k +2 ]
***/

#define CXINT(j,k,ff)                                        \
    FMAD4( xwt_m1 , bold[ibase + in_j ## j ## _k ## k -1 ] , \
           xwt_00 , bold[ibase + in_j ## j ## _k ## k    ] , \
           xwt_p1 , bold[ibase + in_j ## j ## _k ## k +1 ] , \
           xwt_p2 , bold[ibase + in_j ## j ## _k ## k +2 ] , ff )

               /* interpolate in the x direction for each y & z */

               CXINT(m1,m1,f_jm1_km1) ; CXINT(00,m1,f_j00_km1) ;
               CXINT(p1,m1,f_jp1_km1) ; CXINT(p2,m1,f_jp2_km1) ;

               CXINT(m1,00,f_jm1_k00) ; CXINT(00,00,f_j00_k00) ;
               CXINT(p1,00,f_jp1_k00) ; CXINT(p2,00,f_jp2_k00) ;

               CXINT(m1,p1,f_jm1_kp1) ; CXINT(00,p1,f_j00_kp1) ;
               CXINT(p1,p1,f_jp1_kp1) ; CXINT(p2,p1,f_jp2_kp1) ;

               CXINT(m1,p2,f_jm1_kp2) ; CXINT(00,p2,f_j00_kp2) ;
               CXINT(p1,p2,f_jp1_kp2) ; CXINT(p2,p2,f_jp2_kp2) ;

               /* interpolate in the y direction for each z */

/***
               f_km1 =  ywt_m1 * f_jm1_km1 + ywt_00 * f_j00_km1
                      + ywt_p1 * f_jp1_km1 + ywt_p2 * f_jp2_km1 ;

               f_k00 =  ywt_m1 * f_jm1_k00 + ywt_00 * f_j00_k00
                      + ywt_p1 * f_jp1_k00 + ywt_p2 * f_jp2_k00 ;

               f_kp1 =  ywt_m1 * f_jm1_kp1 + ywt_00 * f_j00_kp1
                      + ywt_p1 * f_jp1_kp1 + ywt_p2 * f_jp2_kp1 ;

               f_kp2 =  ywt_m1 * f_jm1_kp2 + ywt_00 * f_j00_kp2
                      + ywt_p1 * f_jp1_kp2 + ywt_p2 * f_jp2_kp2 ;
***/
               FMAD4( ywt_m1 , f_jm1_km1 , ywt_00 , f_j00_km1 ,
                      ywt_p1 , f_jp1_km1 , ywt_p2 , f_jp2_km1 , f_km1 ) ;

               FMAD4( ywt_m1 , f_jm1_k00 , ywt_00 , f_j00_k00 ,
                      ywt_p1 , f_jp1_k00 , ywt_p2 , f_jp2_k00 , f_k00 ) ;

               FMAD4( ywt_m1 , f_jm1_kp1 , ywt_00 , f_j00_kp1 ,
                      ywt_p1 , f_jp1_kp1 , ywt_p2 , f_jp2_kp1 , f_kp1 ) ;

               FMAD4( ywt_m1 , f_jm1_kp2 , ywt_00 , f_j00_kp2 ,
                      ywt_p1 , f_jp1_kp2 , ywt_p2 , f_jp2_kp2 , f_kp2 ) ;

               /* interpolate in the z direction and
                  put the result into the output array!
                  (the +0.5 is to force rounding of the result) */
/***
               bslice[out_ind++] = 0.5 + CP_FACTOR
                                   * ( zwt_m1 * f_km1 + zwt_00 * f_k00
                                      +zwt_p1 * f_kp1 + zwt_p2 * f_kp2 ) ;
***/
               FMAD4( zwt_m1 , f_km1 , zwt_00 , f_k00 ,
                      zwt_p1 , f_kp1 , zwt_p2 , f_kp2 , result ) ;
               FSCAL(CP_FACTOR,result) ;
               FINAL( result , bslice[out_ind] ) ;
               out_ind++ ;

            }  /* end of inner loop */
         }  /* end of outer loop */
      }
      break ;

   }

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/

void LMAP_YNAME( THD_linear_mapping * map , int resam_mode ,
                 THD_dataxes * old_daxes , DTYPE * bold ,
                 THD_dataxes * new_daxes , int yj_fix , DTYPE * bslice )
{
   THD_mat33 mt = map->mbac ;  /* map from bslice indices to bold */
   THD_fvec3 vt = map->svec ;

   int   xi_new  , yj_new  , zk_new  ;  /* voxel indices in new */
   int   xi_old  , yj_old  , zk_old  ;  /* voxel indices in old */
   float fxi_old , fyj_old , fzk_old ;  /* voxel indices in old */

   float fxi_top    , fyj_top    , fzk_top    ;
   float fxi_bot    , fyj_bot    , fzk_bot    ;
   float fxi_base   , fyj_base   , fzk_base   ;
   float dfxi_outer , dfyj_outer , dfzk_outer ;
   float dfxi_inner , dfyj_inner , dfzk_inner ;

   int xi_bot,xi_top , yj_bot,yj_top , zk_bot,zk_top ;  /* ranges in new */
   int out_ind , jstep , kstep ;
   int nxold,nyold,nzold , nxnew,nynew,nznew ;

  ENTRY("AFNI_lmap_to_yslice") ;

   /*--- set up ranges ---*/

   xi_bot = map->bot.xyz[0] ;  xi_top = map->top.xyz[0] ;
   yj_bot = map->bot.xyz[1] ;  yj_top = map->top.xyz[1] ;
   zk_bot = map->bot.xyz[2] ;  zk_top = map->top.xyz[2] ;

   if( yj_fix < yj_bot || yj_fix > yj_top ) EXRETURN ;  /* map doesn't apply! */

   nxold = old_daxes->nxx ;  nxnew = new_daxes->nxx ;
   nyold = old_daxes->nyy ;  nynew = new_daxes->nyy ;
   nzold = old_daxes->nzz ;  nznew = new_daxes->nzz ;

   jstep = nxold ;
   kstep = nxold * nyold ;

   /* set up base of indices in old */

   xi_new = xi_bot-1 ;
   yj_new = yj_fix   ;
   zk_new = zk_bot-1 ;

   fxi_base =   mt.mat[0][0] * xi_new
              + mt.mat[0][1] * yj_new
              + mt.mat[0][2] * zk_new - vt.xyz[0] ;

   fyj_base =   mt.mat[1][0] * xi_new
              + mt.mat[1][1] * yj_new
              + mt.mat[1][2] * zk_new - vt.xyz[1] ;

   fzk_base =   mt.mat[2][0] * xi_new
              + mt.mat[2][1] * yj_new
              + mt.mat[2][2] * zk_new - vt.xyz[2] ;

   dfxi_outer = mt.mat[0][0] ;  /* outer loop is in x = 0 */
   dfyj_outer = mt.mat[1][0] ;
   dfzk_outer = mt.mat[2][0] ;

   dfxi_inner = mt.mat[0][2] ;  /* inner loop is in z = 2 */
   dfyj_inner = mt.mat[1][2] ;
   dfzk_inner = mt.mat[2][2] ;

   fxi_top = nxold - 0.51 ;  fxi_bot = -0.49 ;
   fyj_top = nyold - 0.51 ;  fyj_bot = -0.49 ;
   fzk_top = nzold - 0.51 ;  fzk_bot = -0.49 ;

   switch( resam_mode ){

      default:
      case RESAM_NN_TYPE:{
         float fxi_max , fyj_max , fzk_max ;
         float fxi_min , fyj_min , fzk_min ;
         float fxi_tmp , fyj_tmp , fzk_tmp ;
         int any_outside , all_outside ;

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"NN inner dxyz=%g %g %g  outer dxyz=%g %g %g",
          dfxi_inner,dfyj_inner,dfzk_inner,
          dfxi_outer,dfyj_outer,dfzk_outer ) ; STATUS(str) ; }
#endif
         /** July 15, 1996:
             check if all the points are inside the old grid;
             if so, can use a version of the resampling loop
             that does not need to check each voxel for being
             inside -- hopefully, this will execute more quickly **/

         FXYZTMP(xi_bot,yj_new,zk_bot) ;
         fxi_max = fxi_min = fxi_tmp ;
         fyj_max = fyj_min = fyj_tmp ;
         fzk_max = fzk_min = fzk_tmp ;

         FXYZTMP(xi_top,yj_new,zk_bot) ;
         fxi_max = MAX(fxi_max,fxi_tmp) ; fxi_min = MIN(fxi_min,fxi_tmp) ;
         fyj_max = MAX(fyj_max,fyj_tmp) ; fyj_min = MIN(fyj_min,fyj_tmp) ;
         fzk_max = MAX(fzk_max,fzk_tmp) ; fzk_min = MIN(fzk_min,fzk_tmp) ;

         FXYZTMP(xi_bot,yj_new,zk_top) ;
         fxi_max = MAX(fxi_max,fxi_tmp) ; fxi_min = MIN(fxi_min,fxi_tmp) ;
         fyj_max = MAX(fyj_max,fyj_tmp) ; fyj_min = MIN(fyj_min,fyj_tmp) ;
         fzk_max = MAX(fzk_max,fzk_tmp) ; fzk_min = MIN(fzk_min,fzk_tmp) ;

         FXYZTMP(xi_top,yj_new,zk_top) ;
         fxi_max = MAX(fxi_max,fxi_tmp) ; fxi_min = MIN(fxi_min,fxi_tmp) ;
         fyj_max = MAX(fyj_max,fyj_tmp) ; fyj_min = MIN(fyj_min,fyj_tmp) ;
         fzk_max = MAX(fzk_max,fzk_tmp) ; fzk_min = MIN(fzk_min,fzk_tmp) ;

         any_outside = (fxi_min < fxi_bot) || (fxi_max > fxi_top) ||
                       (fyj_min < fyj_bot) || (fyj_max > fyj_top) ||
                       (fzk_min < fzk_bot) || (fzk_max > fzk_top) ;

         all_outside = (any_outside) ?  (fxi_max < fxi_bot) || (fxi_min > fxi_top) ||
                                        (fyj_max < fyj_bot) || (fyj_min > fyj_top) ||
                                        (fzk_max < fzk_bot) || (fzk_min > fzk_top)
                                     : 0 ;

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"fxi_bot=%g  fxi_top=%g  fxi_min=%g  fxi_max=%g",fxi_bot,fxi_top,fxi_min,fxi_max);
  STATUS(str) ;
  sprintf(str,"fyj_bot=%g  fyj_top=%g  fyj_min=%g  fyj_max=%g",fyj_bot,fyj_top,fyj_min,fyj_max);
  STATUS(str) ;
  sprintf(str,"fzk_bot=%g  fzk_top=%g  fzk_min=%g  fzk_max=%g",fzk_bot,fzk_top,fzk_min,fzk_max);
  STATUS(str) ; }
#endif

/** redefine the macros specifying loop variables **/

#undef OUD_NAME
#undef IND_NAME
#undef OUD
#undef OUD_bot
#undef OUD_top
#undef IND
#undef IND_bot
#undef IND_top
#undef IND_nnn

#define OUD_NAME xi                        /* name of 2nd dimension of output image */
#define IND_NAME zk                        /* name of 1st dimension of output image */
#define IND_nnn  nznew                     /* inner loop image dimension */

#define OUD     TWO_TWO(OUD_NAME , _new)   /* outer loop index name */
#define OUD_bot TWO_TWO(OUD_NAME , _bot)   /* outer loop index min  */
#define OUD_top TWO_TWO(OUD_NAME , _top)   /* outer loop index max  */
#define IND     TWO_TWO(IND_NAME , _new)   /* inner loop index name */
#define IND_bot TWO_TWO(IND_NAME , _bot)   /* inner loop index min  */
#define IND_top TWO_TWO(IND_NAME , _top)   /* inner loop index max  */

         if( all_outside ){
STATUS("NN resample has all outside") ;
            for( OUD=OUD_bot ; OUD <= OUD_top ; OUD++ ){     /* all points are      */
               out_ind = IND_bot + OUD * IND_nnn ;           /* outside input brick */
               for( IND=IND_bot ; IND <= IND_top ; IND++ ){  /* so just load zeros  */
                  bslice[out_ind++] = ZERO ;
               }
            }
         } else {                                       /* at least some are inside */

            int thz, tho , ob , ub ;

            fxi_base += 0.5 ; fyj_base += 0.5 ; fzk_base += 0.5 ;

            fxi_top = nxold - 0.01 ; fxi_bot = 0.0 ;  /* we can use FLOOR instead */
            fyj_top = nyold - 0.01 ; fyj_bot = 0.0 ;  /* of ROUND to find the NN  */
            fzk_top = nzold - 0.01 ; fzk_bot = 0.0 ;  /* by adding 0.5 to all    */
                                                      /* these variables now.   */

            /** thz = flag that indicates which of the steps df??_inner are zero.
                      If two of them are zero, then the inner loop is parallel to
                      one of the input brick axes, and so data may be pulled
                      out in a very efficient fashion.  In such a case, precompute
                      the indexes for the inner loop:

                      the BLOOP macros load array ib, which holds the inner loop
                        computed indexes for each inner loop position.
                      ub = upper bound value for ib array value to still be
                        inside input brick array.
                      ob = outer loop index into input brick array (computed later) **/

            tho = THREEZ(dfxi_outer,dfyj_outer,dfzk_outer) ;         /* 06 Aug 1996:       */
            if( tho == XY_ZERO || tho == XZ_ZERO || tho == YZ_ZERO ) /* only allow thz to  */
               thz = THREEZ(dfxi_inner,dfyj_inner,dfzk_inner) ;      /* indicate special   */
            else                                                     /* treatment if outer */
               thz = NONE_ZERO ;                                     /* axes are special   */

#ifdef AFNI_DEBUG
{ char str[256] ;
  if( any_outside ) sprintf(str,"NN resample has some outside: thz = %d",thz) ;
  else              sprintf(str,"NN resample has all inside: thz = %d",thz) ;
  STATUS(str) ;
  sprintf(str,"OUD_bot=%d  OUD_top=%d  nxold=%d nyold=%d nzold=%d",
          OUD_bot,OUD_top,nxold,nyold,nzold ) ; STATUS(str) ;
  sprintf(str,"IND_bot=%d  IND_top=%d  nxold=%d nyold=%d nzold=%d",
          IND_bot,IND_top,nxold,nyold,nzold ) ; STATUS(str) ; }
#endif

            switch(thz){
               case XY_ZERO:
                  MAKE_IBIG(IND_top) ;
                  fzk_old = fzk_base + dfzk_outer ; ub = IBASE(0,0,nzold) ;
                  for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_BLOOP_XY_ZERO(IND) ; }
               break ;

               case XZ_ZERO:
                  MAKE_IBIG(IND_top) ;
                  fyj_old = fyj_base + dfyj_outer ; ub = IBASE(0,nyold,0) ;
                  for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_BLOOP_XZ_ZERO(IND) ; }
               break ;

               case YZ_ZERO:
                  MAKE_IBIG(IND_top) ;
                  fxi_old = fxi_base + dfxi_outer ; ub = IBASE(nxold,0,0) ;
                  for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_BLOOP_YZ_ZERO(IND) ; }
               break ;
            }

            thz += OUTADD * any_outside ;

STATUS("beginning NN outer loop") ;

            /*** outer loop ***/

            for( OUD=OUD_bot ; OUD <= OUD_top ; OUD++ ){

               fxi_old = (fxi_base += dfxi_outer) ;  /* floating indexes in  */
               fyj_old = (fyj_base += dfyj_outer) ;  /* input brick at start */
               fzk_old = (fzk_base += dfzk_outer) ;  /* of next inner loop   */

               out_ind = IND_bot + OUD * IND_nnn ;   /* index into output brick */

               /*** There are 8 cases for the inner loop:
                      all inside, inner loop not parallel to any input axis
                      all inside, inner loop parallel to input brick z-axis
                      all inside, inner loop parallel to input brick y-axis
                      all inside, inner loop parallel to input brick x-axis

                    and then the 4 same cases repeated when not all desired
                      points are inside the input brick.  Each of these is
                      coded separately for efficiency.  This is important for
                      rapid re-display of results during interactive imaging. ***/

               switch(thz){
                  case NONE_ZERO:
                  case X_ZERO:
                  case Y_ZERO:
                  case Z_ZERO:
                  case XYZ_ZERO:
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ALOOP_GEN ; }
                  break ;

                  case XY_ZERO:
                     xi_old = FLOOR( fxi_old ) ; yj_old = FLOOR( fyj_old ) ;
                     ob = IBASE(xi_old,yj_old,0) ;
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ALOOP_PAR(IND) ; }
                  break ;

                  case XZ_ZERO:
                     xi_old = FLOOR( fxi_old ) ; zk_old = FLOOR( fzk_old ) ;
                     ob = IBASE(xi_old,0,zk_old) ;
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ALOOP_PAR(IND) ; }
                  break ;

                  case YZ_ZERO:
                     yj_old = FLOOR( fyj_old ) ; zk_old = FLOOR( fzk_old ) ;
                     ob = IBASE(0,yj_old,zk_old) ;
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ALOOP_PAR(IND) ; }
                  break ;

                  default:
                  case NONE_ZERO+OUTADD:
                  case X_ZERO+OUTADD:
                  case Y_ZERO+OUTADD:
                  case Z_ZERO+OUTADD:
                  case XYZ_ZERO+OUTADD:
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_CLOOP_GEN ; }
                  break ;

                  case XY_ZERO+OUTADD:
                     xi_old = FLOOR( fxi_old ) ; yj_old = FLOOR( fyj_old ) ;
                     if( TEST_OUT_XXX || TEST_OUT_YYY ){
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ZLOOP ; }
                     } else {
                        ob = IBASE(xi_old,yj_old,0) ;
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_CLOOP_PAR(IND) ; }
                     }
                  break ;

                  case XZ_ZERO+OUTADD:
                     xi_old = FLOOR( fxi_old ) ; zk_old = FLOOR( fzk_old ) ;
                     if( TEST_OUT_XXX || TEST_OUT_ZZZ ){
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ZLOOP ; }
                     } else {
                        ob = IBASE(xi_old,0,zk_old) ;
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_CLOOP_PAR(IND) ; }
                     }
                  break ;

                  case YZ_ZERO+OUTADD:
                     yj_old = FLOOR( fyj_old ) ; zk_old = FLOOR( fzk_old ) ;
                     if( TEST_OUT_YYY || TEST_OUT_ZZZ ){
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ZLOOP ; }
                     } else {
                        ob = IBASE(0,yj_old,zk_old) ;
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_CLOOP_PAR(IND) ; }
                     }
                  break ;
               }
            }
         }
      }
      break ;  /* end of NN! */

      case RESAM_BLOCK_TYPE:
      case RESAM_LINEAR_TYPE:{
         float xwt_00,xwt_p1 , ywt_00,ywt_p1 , zwt_00,zwt_p1 ;
         INTYPE f_j00_k00 , f_jp1_k00 , f_j00_kp1 , f_jp1_kp1 ,
                f_k00     , f_kp1 , result ;
         float frac_xi , frac_yj , frac_zk ;
         int   ibase ,
               in_jp1_k00 = jstep ,
               in_j00_kp1 = kstep ,
               in_jp1_kp1 = jstep+kstep ;
         int   nxold1 = nxold-1 , nyold1 = nyold-1 , nzold1 = nzold-1 ;

         for( xi_new=xi_bot ; xi_new <= xi_top ; xi_new++ ){

            fxi_old = (fxi_base += dfxi_outer) ;
            fyj_old = (fyj_base += dfyj_outer) ;
            fzk_old = (fzk_base += dfzk_outer) ;

            out_ind = zk_bot + xi_new * nznew ;

            for( zk_new=zk_bot ; zk_new <= zk_top ; zk_new++ ){

               fxi_old += dfxi_inner ; fyj_old += dfyj_inner ; fzk_old += dfzk_inner ;

               if( fxi_old < fxi_bot || fxi_old > fxi_top ||
                   fyj_old < fyj_bot || fyj_old > fyj_top ||
                   fzk_old < fzk_bot || fzk_old > fzk_top   ){  /* outside */

/***
                  bslice[out_ind++] = 0 ;
***/
                  FZERO(bslice[out_ind]) ; out_ind++ ;
                  continue ;
               }

               xi_old = FLOOR(fxi_old) ; frac_xi = fxi_old - xi_old ;
               yj_old = FLOOR(fyj_old) ; frac_yj = fyj_old - yj_old ;
               zk_old = FLOOR(fzk_old) ; frac_zk = fzk_old - zk_old ;

               /* use NN if at edges of old brick */

               if( xi_old == nxold1 || yj_old == nyold1 || zk_old == nzold1 ||
                   frac_xi < 0      || frac_yj < 0      || frac_zk < 0        ){

                  bslice[out_ind++] = bold[ IBASE( ROUND(fxi_old) ,
                                                   ROUND(fyj_old) ,
                                                   ROUND(fzk_old)  ) ] ;
                  continue ;
               }

               /* compute weights for LINEAR interpolation in each direction */

               if( resam_mode == RESAM_BLOCK_TYPE ){
                  xwt_00 = BP_00(frac_xi) ; xwt_p1 = BP_P1(frac_xi) ;
                  ywt_00 = BP_00(frac_yj) ; ywt_p1 = BP_P1(frac_yj) ;
                  zwt_00 = BP_00(frac_zk) ; zwt_p1 = BP_P1(frac_zk) ;
               } else {
                  xwt_00 = LP_00(frac_xi) ; xwt_p1 = LP_P1(frac_xi) ;
                  ywt_00 = LP_00(frac_yj) ; ywt_p1 = LP_P1(frac_yj) ;
                  zwt_00 = LP_00(frac_zk) ; zwt_p1 = LP_P1(frac_zk) ;
               }

               /* interpolate in the x direction for each y & z */

               ibase = IBASE(xi_old,yj_old,zk_old) ;
/***
               f_j00_k00 =  xwt_00 * bold[ibase]
                          + xwt_p1 * bold[ibase+1] ;

               f_jp1_k00 =  xwt_00 * bold[ibase+in_jp1_k00]
                          + xwt_p1 * bold[ibase+in_jp1_k00+1] ;

               f_j00_kp1 =  xwt_00 * bold[ibase+in_j00_kp1]
                          + xwt_p1 * bold[ibase+in_j00_kp1+1] ;

               f_jp1_kp1 =  xwt_00 * bold[ibase+in_jp1_kp1]
                          + xwt_p1 * bold[ibase+in_jp1_kp1+1] ;
***/
               FMAD2( xwt_00 , bold[ibase]   ,
                      xwt_p1 , bold[ibase+1] ,            f_j00_k00 ) ;

               FMAD2( xwt_00 , bold[ibase+in_jp1_k00]   ,
                      xwt_p1 , bold[ibase+in_jp1_k00+1] , f_jp1_k00 ) ;

               FMAD2( xwt_00 , bold[ibase+in_j00_kp1]   ,
                      xwt_p1 , bold[ibase+in_j00_kp1+1] , f_j00_kp1 ) ;

               FMAD2( xwt_00 , bold[ibase+in_jp1_kp1]   ,
                      xwt_p1 , bold[ibase+in_jp1_kp1+1] , f_jp1_kp1 ) ;

               /* interpolate in the y direction for each z */
/***
               f_k00 =  ywt_00 * f_j00_k00 + ywt_p1 * f_jp1_k00 ;
               f_kp1 =  ywt_00 * f_j00_kp1 + ywt_p1 * f_jp1_kp1 ;
***/
               FMAD2( ywt_00 , f_j00_k00 , ywt_p1 , f_jp1_k00 , f_k00 ) ;
               FMAD2( ywt_00 , f_j00_kp1 , ywt_p1 , f_jp1_kp1 , f_kp1 ) ;

               /* interpolate in the z direction and
                  put the result into the output array!
                  (the +0.5 is to force rounding of the result) */
/***
               bslice[out_ind++] = zwt_00 * f_k00 + zwt_p1 * f_kp1 + 0.5 ;
***/
               FMAD2( zwt_00 , f_k00 , zwt_p1 , f_kp1 , result ) ;
               FINAL( result , bslice[out_ind] ) ;
               out_ind++ ;
            }
         }
      }
      break ;

      case RESAM_CUBIC_TYPE:{
         float xwt_m1,xwt_00,xwt_p1,xwt_p2 ,   /* interpolation weights */
               ywt_m1,ywt_00,ywt_p1,ywt_p2 ,
               zwt_m1,zwt_00,zwt_p1,zwt_p2  ;

         INTYPE f_jm1_km1, f_j00_km1, f_jp1_km1, f_jp2_km1 , /* interpolants */
                f_jm1_k00, f_j00_k00, f_jp1_k00, f_jp2_k00 ,
                f_jm1_kp1, f_j00_kp1, f_jp1_kp1, f_jp2_kp1 ,
                f_jm1_kp2, f_j00_kp2, f_jp1_kp2, f_jp2_kp2 ,
                f_km1    , f_k00    , f_kp1    , f_kp2 , result ;
         float frac_xi , frac_yj , frac_zk ;

         int   ibase ,                        /* base index for interpolant */
               in_jm1_km1 = -jstep-  kstep ,  /* offsets for -1 (m1) */
               in_jm1_k00 = -jstep         ,  /*              0 (00) */
               in_jm1_kp1 = -jstep+  kstep ,  /*             +1 (p1) */
               in_jm1_kp2 = -jstep+2*kstep ,  /*             +2 (p2) */
               in_j00_km1 =       -  kstep ,  /* steps in j and k indices */
               in_j00_k00 = 0              ,
               in_j00_kp1 =          kstep ,
               in_j00_kp2 =        2*kstep ,
               in_jp1_km1 =  jstep-  kstep ,
               in_jp1_k00 =  jstep         ,
               in_jp1_kp1 =  jstep+  kstep ,
               in_jp1_kp2 =2*jstep+2*kstep ,
               in_jp2_km1 =2*jstep-  kstep ,
               in_jp2_k00 =2*jstep         ,
               in_jp2_kp1 =2*jstep+  kstep ,
               in_jp2_kp2 =2*jstep+2*kstep  ;

         int   nxold1 = nxold-1 , nyold1 = nyold-1 , nzold1 = nzold-1 ;
         int   nxold2 = nxold-2 , nyold2 = nyold-2 , nzold2 = nzold-2 ;

         for( xi_new=xi_bot ; xi_new <= xi_top ; xi_new++ ){

            fxi_old = (fxi_base += dfxi_outer) ;
            fyj_old = (fyj_base += dfyj_outer) ;
            fzk_old = (fzk_base += dfzk_outer) ;

            out_ind = zk_bot + xi_new * nznew ;

            for( zk_new=zk_bot ; zk_new <= zk_top ; zk_new++ ){

               fxi_old += dfxi_inner ; fyj_old += dfyj_inner ; fzk_old += dfzk_inner ;

               /* check if outside old brick */

               if( fxi_old < fxi_bot || fxi_old > fxi_top ||
                   fyj_old < fyj_bot || fyj_old > fyj_top ||
                   fzk_old < fzk_bot || fzk_old > fzk_top   ){  /* outside */
/***
                  bslice[out_ind++] = 0 ;
***/
                  FZERO(bslice[out_ind]) ; out_ind++ ;
                  continue ;
               }

               xi_old = FLOOR(fxi_old) ; frac_xi = fxi_old - xi_old ;
               yj_old = FLOOR(fyj_old) ; frac_yj = fyj_old - yj_old ;
               zk_old = FLOOR(fzk_old) ; frac_zk = fzk_old - zk_old ;

               /* use NN if at very edges of old brick */

               if( xi_old == nxold1 || yj_old == nyold1 || zk_old == nzold1 ||
                   frac_xi < 0      || frac_yj < 0      || frac_zk < 0        ){

                  bslice[out_ind++] = bold[ IBASE( ROUND(fxi_old) ,
                                                   ROUND(fyj_old) ,
                                                   ROUND(fzk_old)  ) ] ;
                  continue ;
               }

               ibase = IBASE(xi_old,yj_old,zk_old) ;

               /* use LINEAR if close to edges of old brick */

               if( xi_old == nxold2 || yj_old == nyold2 || zk_old == nzold2 ||
                   xi_old == 0      || yj_old == 0      || zk_old == 0        ){

                  xwt_00 = LP_00(frac_xi) ; xwt_p1 = LP_P1(frac_xi) ;
                  ywt_00 = LP_00(frac_yj) ; ywt_p1 = LP_P1(frac_yj) ;
                  zwt_00 = LP_00(frac_zk) ; zwt_p1 = LP_P1(frac_zk) ;
/***
                  f_j00_k00 =  xwt_00 * bold[ibase]
                             + xwt_p1 * bold[ibase+1] ;

                  f_jp1_k00 =  xwt_00 * bold[ibase+in_jp1_k00]
                             + xwt_p1 * bold[ibase+in_jp1_k00+1] ;

                  f_j00_kp1 =  xwt_00 * bold[ibase+in_j00_kp1]
                             + xwt_p1 * bold[ibase+in_j00_kp1+1] ;

                  f_jp1_kp1 =  xwt_00 * bold[ibase+in_jp1_kp1]
                             + xwt_p1 * bold[ibase+in_jp1_kp1+1] ;
***/
                  FMAD2( xwt_00 , bold[ibase]   ,
                         xwt_p1 , bold[ibase+1] ,            f_j00_k00 ) ;

                  FMAD2( xwt_00 , bold[ibase+in_jp1_k00]   ,
                         xwt_p1 , bold[ibase+in_jp1_k00+1] , f_jp1_k00 ) ;

                  FMAD2( xwt_00 , bold[ibase+in_j00_kp1]   ,
                         xwt_p1 , bold[ibase+in_j00_kp1+1] , f_j00_kp1 ) ;

                  FMAD2( xwt_00 , bold[ibase+in_jp1_kp1]   ,
                         xwt_p1 , bold[ibase+in_jp1_kp1+1] , f_jp1_kp1 ) ;
/***

                  f_k00 =  ywt_00 * f_j00_k00 + ywt_p1 * f_jp1_k00 ;
                  f_kp1 =  ywt_00 * f_j00_kp1 + ywt_p1 * f_jp1_kp1 ;
***/
                  FMAD2( ywt_00 , f_j00_k00 , ywt_p1 , f_jp1_k00 , f_k00 ) ;
                  FMAD2( ywt_00 , f_j00_kp1 , ywt_p1 , f_jp1_kp1 , f_kp1 ) ;
/***
                  bslice[out_ind++] = zwt_00 * f_k00 + zwt_p1 * f_kp1 + 0.5 ;
***/
                  FMAD2( zwt_00 , f_k00 , zwt_p1 , f_kp1 , result ) ;
                  FINAL( result , bslice[out_ind] ) ;
                  out_ind++ ;
                  continue ;
               }

               /* compute weights for CUBIC interpolation in each direction */

               xwt_m1 = CP_M1(frac_xi) ; xwt_00 = CP_00(frac_xi) ;
               xwt_p1 = CP_P1(frac_xi) ; xwt_p2 = CP_P2(frac_xi) ;

               ywt_m1 = CP_M1(frac_yj) ; ywt_00 = CP_00(frac_yj) ;
               ywt_p1 = CP_P1(frac_yj) ; ywt_p2 = CP_P2(frac_yj) ;

               zwt_m1 = CP_M1(frac_zk) ; zwt_00 = CP_00(frac_zk) ;
               zwt_p1 = CP_P1(frac_zk) ; zwt_p2 = CP_P2(frac_zk) ;

               /* interpolate in the x direction for each y & z */

               CXINT(m1,m1,f_jm1_km1) ; CXINT(00,m1,f_j00_km1) ;
               CXINT(p1,m1,f_jp1_km1) ; CXINT(p2,m1,f_jp2_km1) ;

               CXINT(m1,00,f_jm1_k00) ; CXINT(00,00,f_j00_k00) ;
               CXINT(p1,00,f_jp1_k00) ; CXINT(p2,00,f_jp2_k00) ;

               CXINT(m1,p1,f_jm1_kp1) ; CXINT(00,p1,f_j00_kp1) ;
               CXINT(p1,p1,f_jp1_kp1) ; CXINT(p2,p1,f_jp2_kp1) ;

               CXINT(m1,p2,f_jm1_kp2) ; CXINT(00,p2,f_j00_kp2) ;
               CXINT(p1,p2,f_jp1_kp2) ; CXINT(p2,p2,f_jp2_kp2) ;

               /* interpolate in the y direction for each z */
/***
               f_km1 =  ywt_m1 * f_jm1_km1 + ywt_00 * f_j00_km1
                      + ywt_p1 * f_jp1_km1 + ywt_p2 * f_jp2_km1 ;

               f_k00 =  ywt_m1 * f_jm1_k00 + ywt_00 * f_j00_k00
                      + ywt_p1 * f_jp1_k00 + ywt_p2 * f_jp2_k00 ;

               f_kp1 =  ywt_m1 * f_jm1_kp1 + ywt_00 * f_j00_kp1
                      + ywt_p1 * f_jp1_kp1 + ywt_p2 * f_jp2_kp1 ;

               f_kp2 =  ywt_m1 * f_jm1_kp2 + ywt_00 * f_j00_kp2
                      + ywt_p1 * f_jp1_kp2 + ywt_p2 * f_jp2_kp2 ;
***/
               FMAD4( ywt_m1 , f_jm1_km1 , ywt_00 , f_j00_km1 ,
                      ywt_p1 , f_jp1_km1 , ywt_p2 , f_jp2_km1 , f_km1 ) ;

               FMAD4( ywt_m1 , f_jm1_k00 , ywt_00 , f_j00_k00 ,
                      ywt_p1 , f_jp1_k00 , ywt_p2 , f_jp2_k00 , f_k00 ) ;

               FMAD4( ywt_m1 , f_jm1_kp1 , ywt_00 , f_j00_kp1 ,
                      ywt_p1 , f_jp1_kp1 , ywt_p2 , f_jp2_kp1 , f_kp1 ) ;

               FMAD4( ywt_m1 , f_jm1_kp2 , ywt_00 , f_j00_kp2 ,
                      ywt_p1 , f_jp1_kp2 , ywt_p2 , f_jp2_kp2 , f_kp2 ) ;

               /* interpolate in the z direction and
                  put the result into the output array!
                  (the +0.5 is to force rounding of the result) */
/***
               bslice[out_ind++] = 0.5 + CP_FACTOR
                                   * ( zwt_m1 * f_km1 + zwt_00 * f_k00
                                      +zwt_p1 * f_kp1 + zwt_p2 * f_kp2 ) ;
***/
               FMAD4( zwt_m1 , f_km1 , zwt_00 , f_k00 ,
                      zwt_p1 , f_kp1 , zwt_p2 , f_kp2 , result ) ;
               FSCAL(CP_FACTOR,result) ;
               FINAL( result , bslice[out_ind] ) ;
               out_ind++ ;

            }  /* end of inner loop */
         }  /* end of outer loop */
      }
      break ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void LMAP_ZNAME( THD_linear_mapping * map , int resam_mode ,
                 THD_dataxes * old_daxes , DTYPE * bold ,
                 THD_dataxes * new_daxes , int zk_fix , DTYPE * bslice )
{
   THD_mat33 mt = map->mbac ;  /* map from bslice indices to bold */
   THD_fvec3 vt = map->svec ;

   int   xi_new  , yj_new  , zk_new  ;  /* voxel indices in new */
   int   xi_old  , yj_old  , zk_old  ;  /* voxel indices in old */
   float fxi_old , fyj_old , fzk_old ;  /* voxel indices in old */

   float fxi_top    , fyj_top    , fzk_top    ;
   float fxi_bot    , fyj_bot    , fzk_bot    ;
   float fxi_base   , fyj_base   , fzk_base   ;
   float dfxi_outer , dfyj_outer , dfzk_outer ;
   float dfxi_inner , dfyj_inner , dfzk_inner ;

   int xi_bot,xi_top , yj_bot,yj_top , zk_bot,zk_top ;  /* ranges in new */
   int out_ind , jstep , kstep ;
   int nxold,nyold,nzold , nxnew,nynew,nznew ;

  ENTRY("AFNI_lmap_to_zslice") ;

   /*--- set up ranges ---*/

   xi_bot = map->bot.xyz[0] ;  xi_top = map->top.xyz[0] ;
   yj_bot = map->bot.xyz[1] ;  yj_top = map->top.xyz[1] ;
   zk_bot = map->bot.xyz[2] ;  zk_top = map->top.xyz[2] ;

   if( zk_fix < zk_bot || zk_fix > zk_top ) EXRETURN ;  /* map doesn't apply! */

   nxold = old_daxes->nxx ;  nxnew = new_daxes->nxx ;
   nyold = old_daxes->nyy ;  nynew = new_daxes->nyy ;
   nzold = old_daxes->nzz ;  nznew = new_daxes->nzz ;

   jstep = nxold ;
   kstep = nxold * nyold ;

   /* set up base of indices in old */

   xi_new = xi_bot-1 ;
   yj_new = yj_bot-1 ;
   zk_new = zk_fix ;

   fxi_base =   mt.mat[0][0] * xi_new
              + mt.mat[0][1] * yj_new
              + mt.mat[0][2] * zk_new - vt.xyz[0] ;

   fyj_base =   mt.mat[1][0] * xi_new
              + mt.mat[1][1] * yj_new
              + mt.mat[1][2] * zk_new - vt.xyz[1] ;

   fzk_base =   mt.mat[2][0] * xi_new
              + mt.mat[2][1] * yj_new
              + mt.mat[2][2] * zk_new - vt.xyz[2] ;

   dfxi_outer = mt.mat[0][1] ;  /* outer loop is in y = 1 */
   dfyj_outer = mt.mat[1][1] ;
   dfzk_outer = mt.mat[2][1] ;

   dfxi_inner = mt.mat[0][0] ;  /* inner loop is in x = 0 */
   dfyj_inner = mt.mat[1][0] ;
   dfzk_inner = mt.mat[2][0] ;

   fxi_top = nxold - 0.51 ;  fxi_bot = -0.49 ;
   fyj_top = nyold - 0.51 ;  fyj_bot = -0.49 ;
   fzk_top = nzold - 0.51 ;  fzk_bot = -0.49 ;

   switch( resam_mode ){

      default:
      case RESAM_NN_TYPE:{
         float fxi_max , fyj_max , fzk_max ;
         float fxi_min , fyj_min , fzk_min ;
         float fxi_tmp , fyj_tmp , fzk_tmp ;
         int any_outside , all_outside ;

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"NN inner dxyz=%g %g %g  outer dxyz=%g %g %g",
          dfxi_inner,dfyj_inner,dfzk_inner,
          dfxi_outer,dfyj_outer,dfzk_outer ) ; STATUS(str) ; }
#endif
         /** July 15, 1996:
             check if all the points are inside the old grid;
             if so, can use a version of the resampling loop
             that does not need to check each voxel for being
             inside -- hopefully, this will execute more quickly **/

         FXYZTMP(xi_bot,yj_bot,zk_new) ;
         fxi_max = fxi_min = fxi_tmp ;
         fyj_max = fyj_min = fyj_tmp ;
         fzk_max = fzk_min = fzk_tmp ;

         FXYZTMP(xi_top,yj_bot,zk_new) ;
         fxi_max = MAX(fxi_max,fxi_tmp) ; fxi_min = MIN(fxi_min,fxi_tmp) ;
         fyj_max = MAX(fyj_max,fyj_tmp) ; fyj_min = MIN(fyj_min,fyj_tmp) ;
         fzk_max = MAX(fzk_max,fzk_tmp) ; fzk_min = MIN(fzk_min,fzk_tmp) ;

         FXYZTMP(xi_bot,yj_top,zk_new) ;
         fxi_max = MAX(fxi_max,fxi_tmp) ; fxi_min = MIN(fxi_min,fxi_tmp) ;
         fyj_max = MAX(fyj_max,fyj_tmp) ; fyj_min = MIN(fyj_min,fyj_tmp) ;
         fzk_max = MAX(fzk_max,fzk_tmp) ; fzk_min = MIN(fzk_min,fzk_tmp) ;

         FXYZTMP(xi_top,yj_top,zk_new) ;
         fxi_max = MAX(fxi_max,fxi_tmp) ; fxi_min = MIN(fxi_min,fxi_tmp) ;
         fyj_max = MAX(fyj_max,fyj_tmp) ; fyj_min = MIN(fyj_min,fyj_tmp) ;
         fzk_max = MAX(fzk_max,fzk_tmp) ; fzk_min = MIN(fzk_min,fzk_tmp) ;

         any_outside = (fxi_min < fxi_bot) || (fxi_max > fxi_top) ||
                       (fyj_min < fyj_bot) || (fyj_max > fyj_top) ||
                       (fzk_min < fzk_bot) || (fzk_max > fzk_top) ;

         all_outside = (any_outside) ?  (fxi_max < fxi_bot) || (fxi_min > fxi_top) ||
                                        (fyj_max < fyj_bot) || (fyj_min > fyj_top) ||
                                        (fzk_max < fzk_bot) || (fzk_min > fzk_top)
                                     : 0 ;

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"fxi_bot=%g  fxi_top=%g  fxi_min=%g  fxi_max=%g",fxi_bot,fxi_top,fxi_min,fxi_max);
  STATUS(str) ;
  sprintf(str,"fyj_bot=%g  fyj_top=%g  fyj_min=%g  fyj_max=%g",fyj_bot,fyj_top,fyj_min,fyj_max);
  STATUS(str) ;
  sprintf(str,"fzk_bot=%g  fzk_top=%g  fzk_min=%g  fzk_max=%g",fzk_bot,fzk_top,fzk_min,fzk_max);
  STATUS(str) ; }
#endif

/** redefine the macros specifying loop variables **/

#undef OUD_NAME
#undef IND_NAME
#undef OUD
#undef OUD_bot
#undef OUD_top
#undef IND
#undef IND_bot
#undef IND_top
#undef IND_nnn

#define OUD_NAME yj                        /* name of 2nd dimension of output image */
#define IND_NAME xi                        /* name of 1st dimension of output image */
#define IND_nnn  nxnew                     /* inner loop image dimension */

#define OUD     TWO_TWO(OUD_NAME , _new)   /* outer loop index name */
#define OUD_bot TWO_TWO(OUD_NAME , _bot)   /* outer loop index min  */
#define OUD_top TWO_TWO(OUD_NAME , _top)   /* outer loop index max  */
#define IND     TWO_TWO(IND_NAME , _new)   /* inner loop index name */
#define IND_bot TWO_TWO(IND_NAME , _bot)   /* inner loop index min  */
#define IND_top TWO_TWO(IND_NAME , _top)   /* inner loop index max  */

         if( all_outside ){
STATUS("NN resample has all outside") ;
            for( OUD=OUD_bot ; OUD <= OUD_top ; OUD++ ){     /* all points are      */
               out_ind = IND_bot + OUD * IND_nnn ;           /* outside input brick */
               for( IND=IND_bot ; IND <= IND_top ; IND++ ){  /* so just load zeros  */
                  bslice[out_ind++] = ZERO ;
               }
            }
         } else {                                       /* at least some are inside */

            int thz , tho , ob , ub ;

            fxi_base += 0.5 ; fyj_base += 0.5 ; fzk_base += 0.5 ;

            fxi_top = nxold - 0.01 ; fxi_bot = 0.0 ;  /* we can use FLOOR instead */
            fyj_top = nyold - 0.01 ; fyj_bot = 0.0 ;  /* of ROUND to find the NN  */
            fzk_top = nzold - 0.01 ; fzk_bot = 0.0 ;  /* by adding 0.5 to all    */
                                                      /* these variables now.   */

            /** thz = flag that indicates which of the steps df??_inner are zero.
                      If two of them are zero, then the inner loop is parallel to
                      one of the input brick axes, and so data may be pulled
                      out in a very efficient fashion.  In such a case, precompute
                      the indexes for the inner loop:

                      the BLOOP macros load array ib, which holds the inner loop
                        computed indexes for each inner loop position.
                      ub = upper bound value for ib array value to still be
                        inside input brick array.
                      ob = outer loop index into input brick array (computed later) **/

            tho = THREEZ(dfxi_outer,dfyj_outer,dfzk_outer) ;         /* 06 Aug 1996:       */
            if( tho == XY_ZERO || tho == XZ_ZERO || tho == YZ_ZERO ) /* only allow thz to  */
               thz = THREEZ(dfxi_inner,dfyj_inner,dfzk_inner) ;      /* indicate special   */
            else                                                     /* treatment if outer */
               thz = NONE_ZERO ;                                     /* axes are special   */

#ifdef AFNI_DEBUG
{ char str[256] ;
  if( any_outside ) sprintf(str,"NN resample has some outside: thz = %d",thz) ;
  else              sprintf(str,"NN resample has all inside: thz = %d",thz) ;
  STATUS(str) ;
  sprintf(str,"OUD_bot=%d  OUD_top=%d  nxold=%d nyold=%d nzold=%d",
          OUD_bot,OUD_top,nxold,nyold,nzold ) ; STATUS(str) ;
  sprintf(str,"IND_bot=%d  IND_top=%d  nxold=%d nyold=%d nzold=%d",
          IND_bot,IND_top,nxold,nyold,nzold ) ; STATUS(str) ; }
#endif

            switch(thz){
               case XY_ZERO:
                  MAKE_IBIG(IND_top) ;
                  fzk_old = fzk_base + dfzk_outer ; ub = IBASE(0,0,nzold) ;
                  for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_BLOOP_XY_ZERO(IND) ; }
               break ;

               case XZ_ZERO:
                  MAKE_IBIG(IND_top) ;
                  fyj_old = fyj_base + dfyj_outer ; ub = IBASE(0,nyold,0) ;
                  for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_BLOOP_XZ_ZERO(IND) ; }
               break ;

               case YZ_ZERO:
                  MAKE_IBIG(IND_top) ;
                  fxi_old = fxi_base + dfxi_outer ; ub = IBASE(nxold,0,0) ;
                  for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_BLOOP_YZ_ZERO(IND) ; }
               break ;
            }

            thz += OUTADD * any_outside ;

STATUS("beginning NN outer loop") ;

            /*** outer loop ***/

            for( OUD=OUD_bot ; OUD <= OUD_top ; OUD++ ){

               fxi_old = (fxi_base += dfxi_outer) ;  /* floating indexes in  */
               fyj_old = (fyj_base += dfyj_outer) ;  /* input brick at start */
               fzk_old = (fzk_base += dfzk_outer) ;  /* of next inner loop   */

               out_ind = IND_bot + OUD * IND_nnn ;   /* index into output brick */

               /*** There are 8 cases for the inner loop:
                      all inside, inner loop not parallel to any input axis
                      all inside, inner loop parallel to input brick z-axis
                      all inside, inner loop parallel to input brick y-axis
                      all inside, inner loop parallel to input brick x-axis

                    and then the 4 same cases repeated when not all desired
                      points are inside the input brick.  Each of these is
                      coded separately for efficiency.  This is important for
                      rapid re-display of results during interactive imaging. ***/

               switch(thz){
                  case NONE_ZERO:
                  case X_ZERO:
                  case Y_ZERO:
                  case Z_ZERO:
                  case XYZ_ZERO:
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ALOOP_GEN ; }
                  break ;

                  case XY_ZERO:
                     xi_old = FLOOR( fxi_old ) ; yj_old = FLOOR( fyj_old ) ;
                     ob = IBASE(xi_old,yj_old,0) ;
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ALOOP_PAR(IND) ; }
                  break ;

                  case XZ_ZERO:
                     xi_old = FLOOR( fxi_old ) ; zk_old = FLOOR( fzk_old ) ;
                     ob = IBASE(xi_old,0,zk_old) ;
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ALOOP_PAR(IND) ; }
                  break ;

                  case YZ_ZERO:
                     yj_old = FLOOR( fyj_old ) ; zk_old = FLOOR( fzk_old ) ;
                     ob = IBASE(0,yj_old,zk_old) ;
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ALOOP_PAR(IND) ; }
                  break ;

                  default:
                  case NONE_ZERO+OUTADD:
                  case X_ZERO+OUTADD:
                  case Y_ZERO+OUTADD:
                  case Z_ZERO+OUTADD:
                  case XYZ_ZERO+OUTADD:
                     for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_CLOOP_GEN ; }
                  break ;

                  case XY_ZERO+OUTADD:
                     xi_old = FLOOR( fxi_old ) ; yj_old = FLOOR( fyj_old ) ;
                     if( TEST_OUT_XXX || TEST_OUT_YYY ){
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ZLOOP ; }
                     } else {
                        ob = IBASE(xi_old,yj_old,0) ;
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_CLOOP_PAR(IND) ; }
                     }
                  break ;

                  case XZ_ZERO+OUTADD:
                     xi_old = FLOOR( fxi_old ) ; zk_old = FLOOR( fzk_old ) ;
                     if( TEST_OUT_XXX || TEST_OUT_ZZZ ){
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ZLOOP ; }
                     } else {
                        ob = IBASE(xi_old,0,zk_old) ;
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_CLOOP_PAR(IND) ; }
                     }
                  break ;

                  case YZ_ZERO+OUTADD:
                     yj_old = FLOOR( fyj_old ) ; zk_old = FLOOR( fzk_old ) ;
                     if( TEST_OUT_YYY || TEST_OUT_ZZZ ){
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_ZLOOP ; }
                     } else {
                        ob = IBASE(0,yj_old,zk_old) ;
                        for( IND=IND_bot ; IND <= IND_top ; IND++ ){ NN_CLOOP_PAR(IND) ; }
                     }
                  break ;
               }
            }
         }
      }
      break ;  /* end of NN! */

      case RESAM_BLOCK_TYPE:
      case RESAM_LINEAR_TYPE:{
         float xwt_00,xwt_p1 , ywt_00,ywt_p1 , zwt_00,zwt_p1 ;
         INTYPE f_j00_k00 , f_jp1_k00 , f_j00_kp1 , f_jp1_kp1 ,
                f_k00     , f_kp1 , result ;
         float frac_xi , frac_yj , frac_zk ;
         int   ibase ,
               in_jp1_k00 = jstep ,
               in_j00_kp1 = kstep ,
               in_jp1_kp1 = jstep+kstep ;
         int   nxold1 = nxold-1 , nyold1 = nyold-1 , nzold1 = nzold-1 ;

         for( yj_new=yj_bot ; yj_new <= yj_top ; yj_new++ ){

            fxi_old = (fxi_base += dfxi_outer) ;
            fyj_old = (fyj_base += dfyj_outer) ;
            fzk_old = (fzk_base += dfzk_outer) ;

            out_ind = xi_bot + yj_new * nxnew ;

            for( xi_new=xi_bot ; xi_new <= xi_top ; xi_new++ ){

               fxi_old += dfxi_inner ; fyj_old += dfyj_inner ; fzk_old += dfzk_inner ;

               if( fxi_old < fxi_bot || fxi_old > fxi_top ||
                   fyj_old < fyj_bot || fyj_old > fyj_top ||
                   fzk_old < fzk_bot || fzk_old > fzk_top   ){  /* outside */

/***
                  bslice[out_ind++] = 0 ;
***/
                  FZERO(bslice[out_ind]) ; out_ind++ ;
                  continue ;
               }

               xi_old = FLOOR(fxi_old) ; frac_xi = fxi_old - xi_old ;
               yj_old = FLOOR(fyj_old) ; frac_yj = fyj_old - yj_old ;
               zk_old = FLOOR(fzk_old) ; frac_zk = fzk_old - zk_old ;

               /* use NN if at edges of old brick */

               if( xi_old == nxold1 || yj_old == nyold1 || zk_old == nzold1 ||
                   frac_xi < 0      || frac_yj < 0      || frac_zk < 0        ){

                  bslice[out_ind++] = bold[ IBASE( ROUND(fxi_old) ,
                                                   ROUND(fyj_old) ,
                                                   ROUND(fzk_old)  ) ] ;
                  continue ;
               }

               /* compute weights for LINEAR interpolation in each direction */

               if( resam_mode == RESAM_BLOCK_TYPE ){
                  xwt_00 = BP_00(frac_xi) ; xwt_p1 = BP_P1(frac_xi) ;
                  ywt_00 = BP_00(frac_yj) ; ywt_p1 = BP_P1(frac_yj) ;
                  zwt_00 = BP_00(frac_zk) ; zwt_p1 = BP_P1(frac_zk) ;
               } else {
                  xwt_00 = LP_00(frac_xi) ; xwt_p1 = LP_P1(frac_xi) ;
                  ywt_00 = LP_00(frac_yj) ; ywt_p1 = LP_P1(frac_yj) ;
                  zwt_00 = LP_00(frac_zk) ; zwt_p1 = LP_P1(frac_zk) ;
               }

               /* interpolate in the x direction for each y & z */

               ibase = IBASE(xi_old,yj_old,zk_old) ;
/***
               f_j00_k00 =  xwt_00 * bold[ibase]
                          + xwt_p1 * bold[ibase+1] ;

               f_jp1_k00 =  xwt_00 * bold[ibase+in_jp1_k00]
                          + xwt_p1 * bold[ibase+in_jp1_k00+1] ;

               f_j00_kp1 =  xwt_00 * bold[ibase+in_j00_kp1]
                          + xwt_p1 * bold[ibase+in_j00_kp1+1] ;

               f_jp1_kp1 =  xwt_00 * bold[ibase+in_jp1_kp1]
                          + xwt_p1 * bold[ibase+in_jp1_kp1+1] ;
***/
               FMAD2( xwt_00 , bold[ibase]   ,
                      xwt_p1 , bold[ibase+1] ,            f_j00_k00 ) ;

               FMAD2( xwt_00 , bold[ibase+in_jp1_k00]   ,
                      xwt_p1 , bold[ibase+in_jp1_k00+1] , f_jp1_k00 ) ;

               FMAD2( xwt_00 , bold[ibase+in_j00_kp1]   ,
                      xwt_p1 , bold[ibase+in_j00_kp1+1] , f_j00_kp1 ) ;

               FMAD2( xwt_00 , bold[ibase+in_jp1_kp1]   ,
                      xwt_p1 , bold[ibase+in_jp1_kp1+1] , f_jp1_kp1 ) ;

               /* interpolate in the y direction for each z */
/***
               f_k00 =  ywt_00 * f_j00_k00 + ywt_p1 * f_jp1_k00 ;
               f_kp1 =  ywt_00 * f_j00_kp1 + ywt_p1 * f_jp1_kp1 ;
***/
               FMAD2( ywt_00 , f_j00_k00 , ywt_p1 , f_jp1_k00 , f_k00 ) ;
               FMAD2( ywt_00 , f_j00_kp1 , ywt_p1 , f_jp1_kp1 , f_kp1 ) ;

               /* interpolate in the z direction and
                  put the result into the output array!
                  (the +0.5 is to force rounding of the result) */
/***
               bslice[out_ind++] = zwt_00 * f_k00 + zwt_p1 * f_kp1 + 0.5 ;
***/
               FMAD2( zwt_00 , f_k00 , zwt_p1 , f_kp1 , result ) ;
               FINAL( result , bslice[out_ind] ) ;
               out_ind++ ;

            }
         }
      }
      break ;

      case RESAM_CUBIC_TYPE:{
         float xwt_m1,xwt_00,xwt_p1,xwt_p2 ,   /* interpolation weights */
               ywt_m1,ywt_00,ywt_p1,ywt_p2 ,
               zwt_m1,zwt_00,zwt_p1,zwt_p2  ;

         INTYPE f_jm1_km1, f_j00_km1, f_jp1_km1, f_jp2_km1 , /* interpolants */
                f_jm1_k00, f_j00_k00, f_jp1_k00, f_jp2_k00 ,
                f_jm1_kp1, f_j00_kp1, f_jp1_kp1, f_jp2_kp1 ,
                f_jm1_kp2, f_j00_kp2, f_jp1_kp2, f_jp2_kp2 ,
                f_km1    , f_k00    , f_kp1    , f_kp2 , result ;
         float frac_xi , frac_yj , frac_zk ;

         int   ibase ,                        /* base index for interpolant */
               in_jm1_km1 = -jstep-  kstep ,  /* offsets for -1 (m1) */
               in_jm1_k00 = -jstep         ,  /*              0 (00) */
               in_jm1_kp1 = -jstep+  kstep ,  /*             +1 (p1) */
               in_jm1_kp2 = -jstep+2*kstep ,  /*             +2 (p2) */
               in_j00_km1 =       -  kstep ,  /* steps in j and k indices */
               in_j00_k00 = 0              ,
               in_j00_kp1 =          kstep ,
               in_j00_kp2 =        2*kstep ,
               in_jp1_km1 =  jstep-  kstep ,
               in_jp1_k00 =  jstep         ,
               in_jp1_kp1 =  jstep+  kstep ,
               in_jp1_kp2 =2*jstep+2*kstep ,
               in_jp2_km1 =2*jstep-  kstep ,
               in_jp2_k00 =2*jstep         ,
               in_jp2_kp1 =2*jstep+  kstep ,
               in_jp2_kp2 =2*jstep+2*kstep  ;

         int   nxold1 = nxold-1 , nyold1 = nyold-1 , nzold1 = nzold-1 ;
         int   nxold2 = nxold-2 , nyold2 = nyold-2 , nzold2 = nzold-2 ;

         for( yj_new=yj_bot ; yj_new <= yj_top ; yj_new++ ){

            fxi_old = (fxi_base += dfxi_outer) ;
            fyj_old = (fyj_base += dfyj_outer) ;
            fzk_old = (fzk_base += dfzk_outer) ;

            out_ind = xi_bot + yj_new * nxnew ;

            for( xi_new=xi_bot ; xi_new <= xi_top ; xi_new++ ){

               fxi_old += dfxi_inner ; fyj_old += dfyj_inner ; fzk_old += dfzk_inner ;

               /* check if outside old brick */

               if( fxi_old < fxi_bot || fxi_old > fxi_top ||
                   fyj_old < fyj_bot || fyj_old > fyj_top ||
                   fzk_old < fzk_bot || fzk_old > fzk_top   ){  /* outside */
/***
                  bslice[out_ind++] = 0 ;
***/
                  FZERO(bslice[out_ind]) ; out_ind++ ;
                  continue ;
               }

               xi_old = FLOOR(fxi_old) ; frac_xi = fxi_old - xi_old ;
               yj_old = FLOOR(fyj_old) ; frac_yj = fyj_old - yj_old ;
               zk_old = FLOOR(fzk_old) ; frac_zk = fzk_old - zk_old ;

               /* use NN if at very edges of old brick */

               if( xi_old == nxold1 || yj_old == nyold1 || zk_old == nzold1 ||
                   frac_xi < 0      || frac_yj < 0      || frac_zk < 0        ){

                  bslice[out_ind++] = bold[ IBASE( ROUND(fxi_old) ,
                                                   ROUND(fyj_old) ,
                                                   ROUND(fzk_old)  ) ] ;
                  continue ;
               }

               ibase = IBASE(xi_old,yj_old,zk_old) ;

               /* use LINEAR if close to edges of old brick */

               if( xi_old == nxold2 || yj_old == nyold2 || zk_old == nzold2 ||
                   xi_old == 0      || yj_old == 0      || zk_old == 0        ){

                  xwt_00 = LP_00(frac_xi) ; xwt_p1 = LP_P1(frac_xi) ;
                  ywt_00 = LP_00(frac_yj) ; ywt_p1 = LP_P1(frac_yj) ;
                  zwt_00 = LP_00(frac_zk) ; zwt_p1 = LP_P1(frac_zk) ;
/***
                  f_j00_k00 =  xwt_00 * bold[ibase]
                             + xwt_p1 * bold[ibase+1] ;

                  f_jp1_k00 =  xwt_00 * bold[ibase+in_jp1_k00]
                             + xwt_p1 * bold[ibase+in_jp1_k00+1] ;

                  f_j00_kp1 =  xwt_00 * bold[ibase+in_j00_kp1]
                             + xwt_p1 * bold[ibase+in_j00_kp1+1] ;

                  f_jp1_kp1 =  xwt_00 * bold[ibase+in_jp1_kp1]
                             + xwt_p1 * bold[ibase+in_jp1_kp1+1] ;

                  f_k00 =  ywt_00 * f_j00_k00 + ywt_p1 * f_jp1_k00 ;
                  f_kp1 =  ywt_00 * f_j00_kp1 + ywt_p1 * f_jp1_kp1 ;

                  bslice[out_ind++] = zwt_00 * f_k00 + zwt_p1 * f_kp1 + 0.5 ;
***/
                  FMAD2( xwt_00 , bold[ibase]   ,
                         xwt_p1 , bold[ibase+1] ,            f_j00_k00 ) ;

                  FMAD2( xwt_00 , bold[ibase+in_jp1_k00]   ,
                         xwt_p1 , bold[ibase+in_jp1_k00+1] , f_jp1_k00 ) ;

                  FMAD2( xwt_00 , bold[ibase+in_j00_kp1]   ,
                         xwt_p1 , bold[ibase+in_j00_kp1+1] , f_j00_kp1 ) ;

                  FMAD2( xwt_00 , bold[ibase+in_jp1_kp1]   ,
                         xwt_p1 , bold[ibase+in_jp1_kp1+1] , f_jp1_kp1 ) ;

                  FMAD2( ywt_00 , f_j00_k00 , ywt_p1 , f_jp1_k00 , f_k00 ) ;
                  FMAD2( ywt_00 , f_j00_kp1 , ywt_p1 , f_jp1_kp1 , f_kp1 ) ;

                  FMAD2( zwt_00 , f_k00 , zwt_p1 , f_kp1 , result ) ;
                  FINAL( result , bslice[out_ind] ) ;
                  out_ind++ ;
                  continue ;
               }

               /* compute weights for CUBIC interpolation in each direction */

               xwt_m1 = CP_M1(frac_xi) ; xwt_00 = CP_00(frac_xi) ;
               xwt_p1 = CP_P1(frac_xi) ; xwt_p2 = CP_P2(frac_xi) ;

               ywt_m1 = CP_M1(frac_yj) ; ywt_00 = CP_00(frac_yj) ;
               ywt_p1 = CP_P1(frac_yj) ; ywt_p2 = CP_P2(frac_yj) ;

               zwt_m1 = CP_M1(frac_zk) ; zwt_00 = CP_00(frac_zk) ;
               zwt_p1 = CP_P1(frac_zk) ; zwt_p2 = CP_P2(frac_zk) ;

               /* interpolate in the x direction for each y & z */

               CXINT(m1,m1,f_jm1_km1) ; CXINT(00,m1,f_j00_km1) ;
               CXINT(p1,m1,f_jp1_km1) ; CXINT(p2,m1,f_jp2_km1) ;

               CXINT(m1,00,f_jm1_k00) ; CXINT(00,00,f_j00_k00) ;
               CXINT(p1,00,f_jp1_k00) ; CXINT(p2,00,f_jp2_k00) ;

               CXINT(m1,p1,f_jm1_kp1) ; CXINT(00,p1,f_j00_kp1) ;
               CXINT(p1,p1,f_jp1_kp1) ; CXINT(p2,p1,f_jp2_kp1) ;

               CXINT(m1,p2,f_jm1_kp2) ; CXINT(00,p2,f_j00_kp2) ;
               CXINT(p1,p2,f_jp1_kp2) ; CXINT(p2,p2,f_jp2_kp2) ;

               /* interpolate in the y direction for each z */
/***
               f_km1 =  ywt_m1 * f_jm1_km1 + ywt_00 * f_j00_km1
                      + ywt_p1 * f_jp1_km1 + ywt_p2 * f_jp2_km1 ;

               f_k00 =  ywt_m1 * f_jm1_k00 + ywt_00 * f_j00_k00
                      + ywt_p1 * f_jp1_k00 + ywt_p2 * f_jp2_k00 ;

               f_kp1 =  ywt_m1 * f_jm1_kp1 + ywt_00 * f_j00_kp1
                      + ywt_p1 * f_jp1_kp1 + ywt_p2 * f_jp2_kp1 ;

               f_kp2 =  ywt_m1 * f_jm1_kp2 + ywt_00 * f_j00_kp2
                      + ywt_p1 * f_jp1_kp2 + ywt_p2 * f_jp2_kp2 ;
***/
               FMAD4( ywt_m1 , f_jm1_km1 , ywt_00 , f_j00_km1 ,
                      ywt_p1 , f_jp1_km1 , ywt_p2 , f_jp2_km1 , f_km1 ) ;

               FMAD4( ywt_m1 , f_jm1_k00 , ywt_00 , f_j00_k00 ,
                      ywt_p1 , f_jp1_k00 , ywt_p2 , f_jp2_k00 , f_k00 ) ;

               FMAD4( ywt_m1 , f_jm1_kp1 , ywt_00 , f_j00_kp1 ,
                      ywt_p1 , f_jp1_kp1 , ywt_p2 , f_jp2_kp1 , f_kp1 ) ;

               FMAD4( ywt_m1 , f_jm1_kp2 , ywt_00 , f_j00_kp2 ,
                      ywt_p1 , f_jp1_kp2 , ywt_p2 , f_jp2_kp2 , f_kp2 ) ;

               /* interpolate in the z direction and
                  put the result into the output array!
                  (the +0.5 is to force rounding of the result) */
/***
               bslice[out_ind++] = 0.5 + CP_FACTOR
                                   * ( zwt_m1 * f_km1 + zwt_00 * f_k00
                                      +zwt_p1 * f_kp1 + zwt_p2 * f_kp2 ) ;
***/
               FMAD4( zwt_m1 , f_km1 , zwt_00 , f_k00 ,
                      zwt_p1 , f_kp1 , zwt_p2 , f_kp2 , result ) ;
               FSCAL(CP_FACTOR,result) ;
               FINAL( result , bslice[out_ind] ) ;
               out_ind++ ;

            }  /* end of inner loop */
         }  /* end of outer loop */
      }
      break ;

   }

   EXRETURN ;
}

/*---------------------------------------------------------------------
   Routine to copy data from a brick array into a slice:
     nxx,nyy,nzz = brick dimensions
     fixed_axis  = 1, 2, or 3 (x, y, or z)
     fixed_index = subscript of chosen slice in the fixed_axis direction
     bold        = pointer to brick array
     bslice      = pointer to area to get slice from brick array
                     fixed_axis = 1 --> nyy * nzz
                                  2 --> nzz * nxx
                                  3 --> nxx * nyy
-----------------------------------------------------------------------*/

void B2SL_NAME( int nxx, int nyy, int nzz ,
                int fixed_axis , int fixed_index , DTYPE * bold , DTYPE * bslice )
{
   int ystep = nxx , zstep = nxx*nyy ;

  ENTRY("AFNI_br2sl") ;

   switch( fixed_axis ){

      case 1:{
         register int out , base , yy,zz , inn ;

         out  = 0 ;
         base = fixed_index ;
         for( zz=0 ; zz < nzz ; zz++ ){
            inn   = base ;
            base += zstep ;
            for( yy=0 ; yy < nyy ; yy++ ){
               bslice[out++] = bold[inn] ; inn += ystep ;
            }
         }
      }
      break ;

      case 2:{
         register int out , base , xx,zz , inn ;

         out  = 0 ;
         base = fixed_index * ystep ;
         for( xx=0 ; xx < nxx ; xx++ ){
            inn   = base ;
            base += 1 ;
            for( zz=0 ; zz < nzz ; zz++ ){
               bslice[out++] = bold[inn] ; inn += zstep ;
            }
         }
      }
      break ;

      case 3:{
         register int out , base , xx,yy , inn ;

         base = fixed_index * zstep ;
#ifdef DONT_USE_MEMCPY
         out  = 0 ;
         for( yy=0 ; yy < nyy ; yy++ ){
            inn   = base ;
            base += ystep ;
            for( xx=0 ; xx < nxx ; xx++ )
               bslice[out++] = bold[inn++] ;
         }
#else
         (void) memcpy( bslice , bold+base , sizeof(DTYPE) * zstep ) ;
#endif
      }
      break ;
   }

   EXRETURN ;
}
