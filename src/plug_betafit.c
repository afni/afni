#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

static char helpstring[] =
  "The purpose of this plugin is to fit a Beta distribution to\n"
  "an empirical histogram.  From this, you may be able to infer\n"
  "significance.  The Beta distribution has two parameters (a,b).\n"
  "If the noise in an FMRI time series is Gaussian-white, and\n"
  "linear regression is used (as in FIM), then in theory you\n"
  "should have a=M/2 and b=(N-M-L)/2, where M=number of signal\n"
  "parameter in the regressors (1 or 2), L=number of ort parameters,\n"
  "and N=number of time points used.  This theory does not usually\n"
  "reflect the truth very well.  With this plugin, you can directly\n"
  "estimate a and b, and use this to select a threshold.\n"
  "\n"
  "Source   : Dataset specifies the input dataset\n"
  "           Brick specifies which sub-brick contains the\n"
  "             correlation coefficient (FIM) or R**2 value\n"
  "             (3dDeconvolve or 3dNLfim)\n"
  "           Square = Yes if the plugin should square the\n"
  "                      brick values prior to using them\n"
  "                      (use this for correlations)\n"
  "                    No if the values from the brick should\n"
  "                      be used as is (use this for R**2)\n"
  "a Params : Range of values to search for the a parameter\n"
  "b Params : Range of values to search for the b parameter\n"
  "           H last = top value to plot in the histogram\n"
  "                      (leave 0 to let the plugin choose)\n"
  "Misc     : N ran = number of random (a,b) values to start\n"
  "                     the search with\n"
  "           % cut = how much of the cumulative histogram to\n"
  "                     use in the search for (a,b)\n"
  "           Hsqrt = No to plot a normal histogram\n"
  "                   Yes to plot the y-axis as the square-root\n"
  "                     of the normal histogram counts (this is\n"
  "                     used to emphasize the smaller bins)\n"
  "Mask     : Used to select a sub-region from which the data\n"
  "             source values will be taken\n"
  "           Dataset = which dataset has the mask\n"
  "           Brick = which sub-brick has the mask\n"
  "Range    : Specifies the range of mask values to use\n"
  "Extra    : Used to specify (a,b) for an extra plot on top\n"
  "             of the empirical histogram and the best fit Beta\n"
  "             distribution.  When I use this, I pick (a,b) to\n"
  "             be the theoretical white noise values.\n"
  "\n"
  "The output is a graph of the correlation (or R**2) histogram,\n"
  "with the best fit Beta(a,b) distribution overlaid.  The graph\n"
  "is labeled with the (a,b) parameters.  To pick significance\n"
  "for FMRI, you might choose a cutoff value at the very tail\n"
  "of the fitted Beta(a,b) distribution, assuming that it does\n"
  "look like a good fit to the central part of the data.\n"
  "\n"
  "-- RWCox - June 2000\n"
;

char * BFIT_main( PLUGIN_interface * ) ;

#define NYESNO 2
static char * YESNO_strings[NYESNO] = { "No" , "Yes" } ;

PLUGIN_interface * PLUGIN_init(int ncall)
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "Histogram: BFit" ,
                                "Betafit Histogram" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , BFIT_main  ) ;

   PLUTO_add_hint( plint , "Histogram: Betafit" ) ;

   PLUTO_set_sequence( plint , "A:afniinfo:dsethistog" ) ;

   /*-- first line of input --*/

   PLUTO_add_option( plint , "Source" , "Source" , TRUE ) ;

   PLUTO_add_dataset(  plint ,
                       "Dataset" ,        /* label next to button   */
                       ANAT_ALL_MASK ,    /* take any anat datasets */
                       FUNC_ALL_MASK ,    /* only allow fim funcs   */
                       DIMEN_3D_MASK |    /* need 3D+time datasets  */
                       BRICK_ALLREAL_MASK /* need real-valued datasets */
                    ) ;
   PLUTO_add_number( plint , "Brick"  , 0,9999,0, 0,1 ) ;
   PLUTO_add_string( plint , "Square" , NYESNO , YESNO_strings , 1 ) ;

   /*-- second line of input --*/

   PLUTO_add_option( plint , "a Params" , "Params" , TRUE ) ;
   PLUTO_add_number( plint , "a bot" , 2,50 ,1 ,  5 , 1 ) ;
   PLUTO_add_number( plint , "a top" , 2,500,1 , 20 , 1 ) ;

   PLUTO_add_option( plint , "b Params" , "Params" , TRUE ) ;
   PLUTO_add_number( plint , "b bot" , 10,400 ,0 ,  10 , 1 ) ;
   PLUTO_add_number( plint , "b top" , 10,9999,0 , 200 , 1 ) ;
   PLUTO_add_number( plint , "H last", 0,1000,-1 , 0,1 ) ;

   PLUTO_add_option( plint , "Misc" , "Params" , TRUE ) ;
   PLUTO_add_number( plint , "N ran" , 10,1000,-2 , 100 , 1 ) ;
   PLUTO_add_number( plint , "% cut" , 20,90,0 , 70,1 ) ;
   PLUTO_add_string( plint , "HSqrt"  , NYESNO , YESNO_strings , 0 ) ;

   /*-- (optional) line of input --*/

   PLUTO_add_option( plint , "Mask" , "Mask" , FALSE ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number( plint , "Brick" , 0,9999,0 , 0,1 ) ;

   /*-- (optional) line of input --*/

   PLUTO_add_option( plint , "Range"  , "Range" , FALSE ) ;
   PLUTO_add_number( plint , "Bottom" , -99999,99999, 1, 0,1 ) ;
   PLUTO_add_number( plint , "Top"    , -99999,99999,-1, 0,1 ) ;

   /*-- (optional) line of input --*/

   PLUTO_add_option( plint , "Extra"  , "Extra" , FALSE ) ;
   PLUTO_add_number( plint , "a" , 2,50,1 ,  5 , 1 ) ;
   PLUTO_add_number( plint , "b" , 10,999,0 , 200 , 1 ) ;

   return plint ;
}

#include "betafit.c"

/*--------------------------------------------------------------------*/

char * BFIT_main( PLUGIN_interface * plint )
{
   MCW_idcode * idc ;
   THD_3dim_dataset * input_dset , * mask_dset = NULL ;

   BFIT_data * bfd ;
   BFIT_result * bfr ;

   int nvals,ival , nran,nvox , nbin , miv , sqr,sqt ;
   float abot,atop,bbot,btop,pcut , eps,eps1 , hlast ;
   float *bval , *cval ;
   double aa,bb,xc ;
   double chq,ccc,cdf ;
   int    ihqbot,ihqtop ;

   int mcount,mgood , ii , jj , ibot,itop ;
   float mask_bot=666.0 , mask_top=-666.0 , hbot,htop,dbin ;
   char buf[THD_MAX_NAME+128] , tbuf[THD_MAX_NAME+128] , * tag ;
   int   * hbin , * jbin,*kbin=NULL , *jist[2] ;
   MRI_IMAGE * flim ;

   double aext=-1.0,bext=-1.0 ;

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   if( plint == NULL )
      return "************************\n"
             "BFIT_main:  NULL input\n"
             "************************"  ;

   /*-- read 1st line --*/

   PLUTO_next_option(plint) ;
   idc        = PLUTO_get_idcode(plint) ;
   input_dset = PLUTO_find_dset(idc) ;
   if( input_dset == NULL )
      return "****************************\n"
             "BFIT_main: bad input dataset\n"
             "****************************"  ;

   nvox  = DSET_NVOX(input_dset) ;
   nvals = DSET_NVALS(input_dset) ;
   ival  = (int) PLUTO_get_number(plint) ;
   if( ival < 0 || ival >= nvals )
      return "**************************\n"
             "BFIT_main: bad Brick index\n"
             "**************************" ;

   DSET_load(input_dset) ;
   if( DSET_ARRAY(input_dset,0) == NULL )
      return "*****************************\n"
             "BFIT_main: can't load dataset\n"
             "*****************************"  ;

   tag = PLUTO_get_string(plint) ;
   sqr = PLUTO_string_index(tag,NYESNO,YESNO_strings) ;

   /*-- read 2nd line --*/

   PLUTO_next_option(plint) ;
   abot = PLUTO_get_number(plint) ;
   atop = PLUTO_get_number(plint) ;
   if( atop <= abot )
      return "*** atop <= abot! ***" ;

   PLUTO_next_option(plint) ;
   bbot = PLUTO_get_number(plint) ;
   btop = PLUTO_get_number(plint) ;
   if( atop <= abot )
      return "*** btop <= bbot! ***" ;
   hlast = PLUTO_get_number(plint) ;

   PLUTO_next_option(plint) ;
   nran = (int) PLUTO_get_number(plint) ;
   pcut = PLUTO_get_number(plint) ;

   tag = PLUTO_get_string(plint) ;
   sqt = PLUTO_string_index(tag,NYESNO,YESNO_strings) ;

   /*-- read optional lines --*/

   while( (tag=PLUTO_get_optiontag(plint)) != NULL ){

      /*-- Mask itself --*/

      if( strcmp(tag,"Mask") == 0 ){

         idc       = PLUTO_get_idcode(plint) ;
         mask_dset = PLUTO_find_dset(idc) ;

         if( mask_dset == NULL ){
            return "******************************\n"
                   "BFIT_main:  bad mask dataset\n"
                   "******************************"  ;
         }

         if( DSET_NVOX(mask_dset) != nvox ){
           return "************************************************************\n"
                  "BFIT_main: mask input dataset doesn't match source dataset\n"
                  "************************************************************" ;
         }

         miv = (int) PLUTO_get_number(plint) ;
         if( miv >= DSET_NVALS(mask_dset) || miv < 0 ){
            return "****************************************************\n"
                   "BFIT_main: mask dataset sub-brick index is illegal\n"
                   "****************************************************"  ;
         }

         DSET_load(mask_dset) ;
         if( DSET_ARRAY(mask_dset,miv) == NULL ){
            return "*************************************\n"
                   "BFIT_main:  can't load mask dataset\n"
                   "*************************************"  ;
         }
         continue ;
      }

      /*-- Mask range of values --*/

      if( strcmp(tag,"Range") == 0 ){
         if( mask_dset == NULL ){
            return "******************************************\n"
                   "BFIT_main:  Can't use Range without Mask\n"
                   "******************************************"  ;
         }

         mask_bot = PLUTO_get_number(plint) ;
         mask_top = PLUTO_get_number(plint) ;
         continue ;
      }

      /*-- Extra plot --*/

      if( strcmp(tag,"Extra") == 0 ){
         aext = PLUTO_get_number(plint) ;
         bext = PLUTO_get_number(plint) ;
         continue ;
      }
   }

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   bfd = BFIT_prepare_dataset( input_dset , ival , sqr ,
                               mask_dset , miv , mask_bot , mask_top ) ;

   if( bfd == NULL ) return "*** BFIT_prepare_dataset fails ***" ;

   bfr = BFIT_compute( bfd ,
                       pcut , abot,atop , bbot,btop , nran,200 ) ;

   if( bfr == NULL ){
      BFIT_free_data( bfd ) ;
      return "*** BFIT_compute fails! ***" ;
   }

   itop  = bfr->itop ;
   mgood = bfr->mgood ;

   ibot   = bfd->ibot ;
   bval   = bfd->bval ;
   cval   = bfd->cval ;
   mcount = bfd->mcount ;

   xc   = bfr->xcut ;
   aa   = bfr->a ;
   bb   = bfr->b ;
   eps  = bfr->eps ;
   eps1 = 1.0 - eps ;
   if( eps1 > 1.0 ) eps1 = 1.0 ;
   eps1 = (mcount-ibot) * eps1 ;

   /*-- compute and plot histogram --*/

   /* original data was already squared (e.g., R**2 values) */

   if( !sqr ){
      hbot = 0.0 ; htop = 1.0 ; nbin = 200 ;
      if( bval[mcount-1] < 1.0 ) htop = bval[mcount-1] ;
      dbin = (htop-hbot)/nbin ;

      hbin = (int *) calloc((nbin+1),sizeof(int)) ;  /* actual histogram */
      jbin = (int *) calloc((nbin+1),sizeof(int)) ;  /* theoretical fit */

      for( ii=0 ; ii < nbin ; ii++ ){  /* beta fit */
         jbin[ii] = (int)( eps1 * ( beta_t2p(hbot+ii*dbin,aa,bb)
                                   -beta_t2p(hbot+ii*dbin+dbin,aa,bb) ) ) ;
      }

      jist[0] = jbin ;

      flim = mri_new_vol_empty( mcount-ibot,1,1 , MRI_float ) ;
      mri_fix_data_pointer( bval+ibot , flim ) ;
      mri_histogram( flim , hbot,htop , TRUE , nbin,hbin ) ;

      /* "extra" histogram (nominal values?) */

      if( aext > 0.0 ){
         kbin = (int *) calloc((nbin+1),sizeof(int)) ;
         jist[1] = kbin ;
         for( ii=0 ; ii < nbin ; ii++ ){  /* beta fit */
            kbin[ii] = (int)( eps1 * ( beta_t2p(hbot+ii*dbin,aext,bext)
                                      -beta_t2p(hbot+ii*dbin+dbin,aext,bext) ) ) ;
         }
      }

   } else {   /* original data was not squared (e.g., correlations) */

      double hb,ht ;
      htop = 1.0 ; nbin = 200 ;
      if( bval[mcount-1] < 1.0 ) htop = sqrt(bval[mcount-1]) ;
      hbot = -htop ;
      dbin = (htop-hbot)/nbin ;

      hbin = (int *) calloc((nbin+1),sizeof(int)) ;  /* actual histogram */
      jbin = (int *) calloc((nbin+1),sizeof(int)) ;  /* theoretical fit */

      for( ii=0 ; ii < nbin ; ii++ ){  /* beta fit */
         hb = hbot+ii*dbin ; ht = hb+dbin ;
         hb = hb*hb ; ht = ht*ht ;
         if( hb > ht ){ double qq=hb ; hb=ht ; ht=qq ; }
         jbin[ii] = (int)( 0.5*eps1 * ( beta_t2p(hb,aa,bb)
                                       -beta_t2p(ht,aa,bb) ) ) ;
      }

      jist[0] = jbin ;

      flim = mri_new_vol_empty( mcount-ibot,1,1 , MRI_float ) ;
      mri_fix_data_pointer( cval+ibot , flim ) ;
      mri_histogram( flim , hbot,htop , TRUE , nbin,hbin ) ;

      /* nominal fit */

      if( aext > 0.0 ){
         kbin = (int *) calloc((nbin+1),sizeof(int)) ;
         jist[1] = kbin ;
         for( ii=0 ; ii < nbin ; ii++ ){  /* beta fit */
            hb = hbot+ii*dbin ; ht = hb+dbin ;
            hb = hb*hb ; ht = ht*ht ;
            if( hb > ht ){ double qq=hb ; hb=ht ; ht=qq ; }
            kbin[ii] = (int)( 0.5*eps1 * ( beta_t2p(hb,aext,bext)
                                          -beta_t2p(ht,aext,bext) ) ) ;
         }
      }
   }

   sprintf(buf,"%s[%d] a=%.2f b=%.2f \\epsilon=%.2f %%=%.0f",
           DSET_FILECODE(input_dset),ival,aa,bb,eps,pcut ) ;

   ccc = bfr->q_chisq ;

   /* blow up histogram details by sqrt-ing, if ordered */

   if( sqt ){
      for( ii=0 ; ii < nbin ; ii++ ){
         hbin[ii] = (int) sqrt( (double)(100*hbin[ii]+0.5) ) ;
         jbin[ii] = (int) sqrt( (double)(100*jbin[ii]+0.5) ) ;
         if( kbin!=NULL )
            kbin[ii] = (int) sqrt( (double)(100*kbin[ii]+0.5) ) ;
      }
   }

   /* and plot */

   sprintf(tbuf,"\\beta fit: cutoff=%.2f nvox=%d q(\\chi^2)=%8.2e",
           (sqr)?sqrt(xc):xc , mgood , ccc ) ;
   if( sqt ){
      ii = strlen(tbuf) ;
      sprintf( tbuf+ii , " \\surd ogram" ) ;
   }

   if( hlast > 0.0 ){
      hbin[nbin-1] = jbin[nbin-1] = hlast ;
      if( kbin != NULL ) kbin[nbin-1] = hlast ;
   }

   PLUTO_histoplot( nbin,hbot,htop,hbin ,
                    tbuf,NULL,buf , (kbin==NULL)?1:2 , jist ) ;

   /* cleanup */

   mri_clear_data_pointer(flim) ; mri_free(flim) ;
   free(hbin) ; free(jbin) ; if( kbin != NULL ) free(kbin);

   BFIT_free_data(bfd) ; BFIT_free_result(bfr) ;
   return NULL ;
}
