#include "mrilib.h"
#include "zgaussian.c"

#undef  LAMBDA
#define LAMBDA(a,b) ((b+a)*(1.0+a*b)/(1.0+2.0*a*b+b*b))

int main( int argc , char *argv[] )
{
   int nxy=256 , nxyq , nz=1 , ntr=200 , ndiv=16 , nbb=3 , iarg ;
   float TR=2.5f , blur=5.0f , base=30.0f , bsig=3.0f ;
   float amx=0.8f , bmx=0.5f , amn=0.0f , bmn=-0.5f ;
   float taumx=3.0f , sigmx=5.0f , sigmn=0.0f ;
   MRI_IMAGE *rim=NULL ;
   char *prefix="Simm" ;

   THD_3dim_dataset *dset ;
   THD_ivec3 ixyz , oxyz ; THD_fvec3 dxyz ;
   int dvx,dvy,xx,yy,zz,tt , kk , kold ;
   float *tsar , val , wal , a0,a1 , b0,b1 , dsig , aa,bb,lam ;
   MRI_IMAGE *armim ; float *armar ;

   /*-- the pitiful help --*/

   if( argc == 1 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "This program generates a simulated FMRI time series dataset\n"
      "for the nefarious and inscrutable purposes of RW Cox and G Chen.\n"
      "At least one option must be given (or you get this help message)!\n"
      "\n"
      "-nxy xx = size of 2D image to produce                [default=256 ]\n"
      "-nz     = number of slices to produce                [default=1   ]\n"
      "-div xx = size of subdivisions to use                [default=16  ]\n"
      "-nTR xx = number of time points to produce           [default=200 ]\n"
      "-TR  xx = TR to use for the output dataset           [default=2.5 ]\n"
      "-bas xx = mean value to use for constant baseline    [default=30  ]\n"
      "-nbb xx = number of baseline polynomials to produce  [default=3   ]\n"
      "-vbb xx = stdev of baseline polynomial coefficients  [default=3   ]\n"
      "-blr xx = FWHM of blurring to be applied to baseline [default=5   ]\n"
      "-mxa xx = max value of ARMA11 parameter 'a' to use   [default=+0.8]\n"
      "-mnb xx = min value of ARMA11 parameter 'a' to use   [default= 0.0]\n"
      "-mxb xx = max value of ARMA11 parameter 'b' to use   [default=+0.5]\n"
      "-mnb xx = max value of ARMA11 parameter 'b' to use   [default=-0.5]\n"
      "-smx xx = max value of time series stdev to use      [default=5   ]\n"
      "-smn xx = min value of time series stdev to use\n"
      "-1D  xx = 1D file to use for response model          [no default  ]\n"
      "-tmx xx = max value of tau to use                    [default=3   ]\n"
      "-pre pp = output dataset prefix                      [default=Simm]\n"
      "\n"
      "There is very little error checking.  If you do something stooopid\n"
      "(e.g., '-mxa 2.0'), then that's just tough for you.\n"
     ) ;
     exit(0) ;
   }

   /*-- process command line options --*/

#undef  OOP
#define OOP(ostr,ovar)                                                         \
 if( strcmp(argv[iarg],(ostr)) == 0 ){                                         \
       iarg++; if( iarg >= argc )ERROR_exit("need arg after %s",argv[iarg-1]); \
       (ovar) = strtod(argv[iarg],NULL) ; iarg++ ; continue ;                  \
 } else

   iarg = 1 ;
   while( iarg < argc ){

     if( strncmp(argv[iarg],"-prefix",4) == 0 ){
       iarg++; if( iarg >= argc ) ERROR_exit("need arg after %s",argv[iarg-1]);
       prefix = strdup(argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-1D") == 0 ){
       iarg++; if( iarg >= argc ) ERROR_exit("need arg after %s",argv[iarg-1]);
       rim = mri_read_1D(argv[iarg]) ;
       if( rim == NULL ) ERROR_exit("Can't read -1D file '%s'",argv[iarg]) ;
       iarg++ ; continue ;
     }

     OOP("-nxy",nxy  ) ;
     OOP("-div",ndiv ) ;
     OOP("-nTR",ntr  ) ;
     OOP("-TR" ,TR   ) ;
     OOP("-bas",base ) ;
     OOP("-nbb",nbb  ) ;
     OOP("-vbb",bsig ) ;
     OOP("-blr",blur ) ;
     OOP("-mxa",amx  ) ;
     OOP("-mxb",bmx  ) ;
     OOP("-mna",amn  ) ;
     OOP("-mnb",bmn  ) ;
     OOP("-tmx",taumx) ;
     OOP("-smx",sigmx) ;
     OOP("-smn",sigmn) ;
     OOP("-nz" ,nz   ) ;

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;
   }

   if( rim != NULL && rim->nx < ntr )
     ERROR_exit("-1D file length %d than nTR=%d",rim->nx,ntr) ;

   /*-- create output dataset ---*/

   dset = EDIT_empty_copy(NULL) ;

   LOAD_IVEC3(ixyz,nxy,nxy,nz) ;
   LOAD_IVEC3(oxyz,ORI_L2R_TYPE,ORI_P2A_TYPE,ORI_I2S_TYPE) ;
   LOAD_FVEC3(dxyz,-1.0,-1.0,1.0) ;
   EDIT_dset_items( dset ,
                      ADN_nxyz      , ixyz ,
                      ADN_xyzorient , oxyz ,
                      ADN_xyzdel    , dxyz ,
                      ADN_prefix    , prefix ,
                      ADN_nvals     , ntr ,
                      ADN_ntt       , ntr ,
                      ADN_ttdel     , TR ,
                      ADN_ttorg     , 0.0 ,
                      ADN_nsl       , 0 ,
                      ADN_tunits    , UNITS_SEC_TYPE ,
                      ADN_datum_all , MRI_float ,
                    ADN_none ) ;
   for( tt=0 ; tt < ntr ; tt++ )   /* attach zero filled volumes */
     EDIT_substitute_brick(dset,tt,MRI_float,NULL) ;

   tsar = (float *)malloc(sizeof(float)*ntr) ;
   srand48( (long)GSEED ) ;

   /* build baseline model into dataset */

   nxyq = nxy * nxy ;

   INFO_message("3dSimARMA11: build baseline model") ;

   for( zz=0 ; zz < nz ; zz++ ){
    for( yy=0 ; yy < nxy ; yy++ ){
     for( xx=0 ; xx < nxy ; xx++ ){
       val = zgaussian() ; val *= val * base ;
       for( tt=0 ; tt < ntr ; tt++ ) tsar[tt] = val ;
       wal = 1.99998f / (ntr-1.0f) ;
       for( kk=1 ; kk <= nbb ; kk++ ){
         val = bsig * zgaussian() ;
         for( tt=0 ; tt < ntr ; tt++ )
           tsar[tt] += val * Plegendre( wal*tt-0.99999 , kk ) ;
       }
       THD_insert_series( xx+yy*nxy+zz*nxyq , dset , ntr , MRI_float , tsar , 1 ) ;
   }}}

   /* smooth baseline model */

   if( blur > 0.0f ){
     INFO_message("blur baseline model") ;
     val = FWHM_TO_SIGMA(blur) ;
     for( tt=0 ; tt < ntr ; tt++ )
       EDIT_blur_volume( nxy,nxy,nz , 1.0f,1.0f,1.0f ,
                         MRI_float , DSET_ARRAY(dset,tt) , val ) ;
   }

   /* add signal model */

   if( rim == NULL )
     INFO_message("no -1D file ==> 'signal' strength will be zero") ;

   if( taumx > 0.0f && rim != NULL ){
     float *rar=MRI_FLOAT_PTR(rim) , dtau , tau , beta ;
     float *bar ; int nbar ;

     INFO_message("adding signal model") ;

     nbar = (nxy-1)/ndiv + 1 ; dtau = taumx / (nbar-1) ;
     bar = (float *)malloc(sizeof(float)*nbar*nbar) ;
     for( yy=0 ; yy < nbar ; yy++ )
       for( xx=0 ; xx < nbar ; xx++ )
         bar[xx+yy*nbar] = zgaussian() * xx*dtau ;

     for( zz=0 ; zz < nz ; zz++ ){
       for( xx=0 ; xx < nxy ; xx++ ){
         dvx = xx/ndiv ;
         for( yy=0 ; yy < nxy ; yy++ ){
           dvy = yy/ndiv ;
           beta = bar[dvx+dvy*nbar] ;
           if( beta != 0.0f ){
             THD_extract_array( xx+yy*nxy+zz*nxyq , dset , 1 , tsar ) ;
             for( tt=0 ; tt < ntr ; tt++ ) tsar[tt] += beta*rar[tt] ;
             THD_insert_series( xx+yy*nxy+zz*nxyq , dset , ntr , MRI_float , tsar , 1 ) ;
           }
         }
       }
     }

     free(bar) ;
   }

   /* add noise model */

   fprintf(stderr,"++ adding ARMA(1,1) model") ;
   kk = (nxy+nxy-2)/(2*ndiv) ; a0 = amn ; a1 = amx/kk ;
   kk = (nxy-1)    /(2*ndiv) ; b0 = 0.5f*(bmn+bmx) ; b1 = (bmx-bmn)/(2.0f*kk) ;
   kk = 1 + (nxy-1)/ndiv ; dsig = (sigmx-sigmn)/kk ; kold = -1 ;
   for( zz=0 ; zz < nz ; zz++ ){
     for( yy=0 ; yy < nxy ; yy++ ){
       kk  = yy/ndiv ; val = (1+kk)*dsig + sigmn ;
       if( kk > kold ){ fprintf(stderr,".") ; kold = kk ; }
       for( xx=0 ; xx < nxy ; xx++ ){
         dvx = (xx+yy)/(2*ndiv) ; dvy = (xx-yy)/(2*ndiv) ;
         aa = a0+a1*dvx ; bb = b0+b1*dvy ; lam = LAMBDA(aa,bb) ;
         armim = mri_genARMA11( ntr , 1 , aa,lam , val ) ;
         if( armim == NULL ){
           WARNING_message("ARMA11 fails: xx=%d yy=%d a=%g b=%g lam=%g sig=%g",
                           xx,yy,aa,bb,lam,val);
           continue ;
         }
         armar = MRI_FLOAT_PTR(armim) ;
         THD_extract_array( xx+yy*nxy+zz*nxyq , dset , 1 , tsar ) ;
         for( tt=0 ; tt < ntr ; tt++ ) tsar[tt] += armar[tt] ;
         THD_insert_series( xx+yy*nxy+zz*nxyq , dset , ntr , MRI_float , tsar , 1 ) ;
         mri_free(armim) ;
       }
     }
   }
   fprintf(stderr,"\n") ;

   /* write output */

   putenv("AFNI_DECONFLICT=OVERWRITE") ;
   DSET_write(dset) ; WROTE_DSET(dset) ; exit(0) ;
}
