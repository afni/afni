#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/** This program is really just a quick hack for the Spanish Inquisition!!  **/
/** [They need to torture some data, according to Chief Inquisitor Javier]  **/
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Inputs:
     ntin  = number of time points in input array tsar
     tsar  = input array to be interpolated
     tgar  = time grid of input array -- monotonic increasing, length = ntin
     toff  = additional time offset for input array (allows for slice timing)
     ntout = number of time points in output array
     dtout = time step of output array
     tzout = time offset of output array -- i-th point is at i*dtout+tzout
     outar = output array -- length = ntout (duh) -- linearly interpolated
   Function does not check for bad inputs -- that's supposed to be in the
   domain of the caller function.
*//*-------------------------------------------------------------------------*/

void THD_resample_timeseries_linear(
                         int ntin , float *tsar, float *tgar, float toff,
                         int ntout, float dtout, float tzout, float *outar )
{
   int iin,iout , ntin2=ntin-2 ;
   float tout , f0 ;

   /* loop over output points */

   for( iin=0,iout=0 ; iout < ntout ; iout++ ){
     tout = tzout + iout*dtout ;  /* time of this output point */

     /* find next iin such that tout lies in the input interval iin .. iin+1 */

     for( ; iin < ntin2 && tout > tgar[iin+1]+toff ; iin++ ) ; /*nada */

     /* f0 = linear interpolation factor for tsar[iin] */

     f0 = (tgar[iin+1]+toff-tout) / (tgar[iin+1]-tgar[iin]) ;

     /* if beyond time bounds of input, patch the factor */

     if( f0 < 0.0f ) f0 = 0.0f ; else if( f0 > 1.0f ) f0 = 1.0f ;

     /* and interpolate */

     outar[iout] = f0 * tsar[iin] + (1.0f-f0) * tsar[iin+1] ;
   }

   return ;
}

/*---------------------------------------------------------------------------*/
/* Resample an irregular TR dataset to a fixed TR dataset.
     inset  = input dataset
     tgin   = holds input time grid (monotonic increasing)
     ntout  = number of time points in output
     dtout  = TR of output dataset
     tzout  = time offset of output dataset
     icode  = interpolation method (at present, only MRI_LINEAR works)
   If dtout <= 0, then it will be computed as the average TR
   If ntout <= 1, then it will be computed to fit the input dataset
*//*-------------------------------------------------------------------------*/

THD_3dim_dataset * THD_resample_irregular_dataset(
                            THD_3dim_dataset *inset , MRI_IMAGE *tgin ,
                            char *prefix ,
                            int ntout , float dtout , float tzout , int icode )
{
   float *tgar=NULL , *otar,*itar ;
   float toff ;
   int ii , ntin , nvox , nbad ;
   THD_3dim_dataset *outset=NULL ;

ENTRY("THD_resample_irregular_dataset") ;

   /* check inputs for sanity */

   if( ! ISVALID_DSET(inset) )           RETURN(NULL) ;             /* crazy */
   ntin = DSET_NVALS(inset) ;
   if( ntin < 2 )                        RETURN(NULL) ;            /* whacko */
   DSET_load(inset) ;
   if( ! DSET_LOADED(inset) )            RETURN(NULL) ;   /* just plain nuts */

   if( tgin == NULL || tgin->nx < ntin ) RETURN(NULL) ;             /* gonzo */

   /* check input time grid to see if it is monotonic increasing */

   tgar = MRI_FLOAT_PTR(tgin) ; if( tgar == NULL ) RETURN(NULL) ;  /* lunacy */
   for( nbad=0,ii=1 ; ii < ntin ; ii++ ){
     if( tgar[ii-1] >= tgar[ii] ) nbad++ ;
   }
   if( nbad > 0 ){                                               /* demented */
     ERROR_message(
       "THD_resample_irregular_dataset: input time grid is disordered in %d place%s",
       nbad , (nbad==1) ? "\0" : "s" ) ;
     RETURN(NULL) ;
   }

   /* don't have ntout and/or don't have dtout ==> compute them */

   if( ntout < 2 && dtout <= 0.0f ){
     dtout = ( tgar[ntin-1] - tgar[0] ) / (ntin-1) ;  /* average TR */
     ntout = ntin ;
   } else if( ntout < 2 ){                    /* just missing ntin? */
     ntout = 1 + (int)( ( tgar[ntin-1] - tzout ) / dtout + 0.001f ) ;
   } else if( dtout <= 0.0f ){
     dtout = ( tgar[ntin-1] - tgar[0] ) / (ntout-1) ; /* average TR */
   }
   ININFO_message("Output grid: TR=%.3f  nvals=%d",dtout,ntout) ;

   /* create output dataset */

   if( !THD_filename_ok(prefix) ) prefix = "./TRfixed" ;   /* pathetic user? */

   outset = EDIT_empty_copy(inset) ;
   EDIT_dset_items( outset ,
                      ADN_prefix    , prefix    ,
                      ADN_nvals     , ntout     ,
                      ADN_ntt       , ntout     ,
                      ADN_ttdel     , dtout     ,  /* set TR */
                      ADN_nsl       , 0         ,  /* time shifting is done */
                      ADN_datum_all , MRI_float ,
                      ADN_brick_fac , NULL      ,
                    ADN_none ) ;
   for( ii=0 ; ii < ntout ; ii++ )
     EDIT_substitute_brick( outset , ii , MRI_float , NULL ) ;

   /* loop over input voxels */

   nvox = DSET_NVOX(inset) ;
   otar = (float *)malloc(sizeof(float)*ntout) ;  /* output array */
   itar = (float *)malloc(sizeof(float)*ntin ) ;  /* input array */

   ININFO_message("Computing output dataset") ;

   for( ii=0 ; ii < nvox ; ii++ ){
       /* allow for time shifting */
     toff = THD_timeof_vox( 0 , ii , inset ) ;
       /* get input array */
     (void)THD_extract_array( ii , inset , 0 , itar ) ;
       /* compute output array */
     THD_resample_timeseries_linear( ntin , itar , tgar , toff ,
                                     ntout, dtout, tzout, otar  ) ;
       /* put it into the output dataset */
     THD_insert_series( ii , outset , ntout , MRI_float , otar , 1 ) ;
   }

   free(itar); free(otar);  /* take out the trash */
   RETURN(outset);
}

/*---------------------------------------------------------------------------*/

void TRfix_help(void)
{
   printf(
     "Usage: 3dTRfix [options]\n"
     "\n"
     "This program will read in a dataset that was sampled on an irregular time\n"
     "grid and re-sample it via linear interpolation to a regular time grid.\n"
     "\n"
     "NOTES:\n"
     "------\n"
     "The re-sampling will include the effects of slice time offsets (similarly\n"
     "to program 3dTshift), if these time offsets are encoded in the input dataset's\n"
     "header.\n"
     "\n"
     "No other processing is performed -- in particular, there is no allowance\n"
     "(at present) for T1 artifacts resulting from variable TR.\n"
     "\n"
     "If the first 1 or 2 time points are abnormally bright due to the NMR\n"
     "pre-steady-state effect, then their influence might be spread farther\n"
     "into the output dataset by the interpolation process.  You can avoid this\n"
     "effect by excising these values from the input using the '[2..$]' notation\n"
     "in the input dataset syntax.\n"
     "\n"
     "If the input dataset is catenated from multiple non-contiguous imaging runs,\n"
     "the program will happily interpolate across the time breaks between the runs.\n"
     "For this reason, you should not give such a file (e.g., from 3dTcat) to this\n"
     "program -- you should use 3dTRfix on each run separately, and only later\n"
     "catenate the runs.\n"
     "\n"
     "The output dataset is stored in float format, regardless of the input format.\n"
     "\n"
     "** Basically, this program is a hack for the Mad Spaniard.\n"
     "** When are we going out for tapas y cerveza (sangria es bueno, tambien)?\n"
     "\n"
     "OPTIONS:\n"
     "--------\n"
     "\n"
     " -input iii    = Input dataset 'iii'. [MANDATORY]\n"
     "\n"
     " -TRlist rrr   = 1D columnar file of time gaps between sub-bricks in 'iii';\n"
     "                 If the input dataset has N time points, this file must\n"
     "                 have at least N-1 (positive) values.\n"
     "                * Please note that these time steps (or the time values in\n"
     "                  '-TIMElist') should be in seconds, NOT in milliseconds!\n"
     "                * AFNI time units are seconds!!!\n"
     "\n"
     " -TIMElist ttt = Alternative to '-TRlist', where you give the N values of\n"
     "                 the times at each sub-brick; these values must be monotonic\n"
     "                 increasing and non-negative.\n"
     "                * You must give exactly one of '-TIMElist' or '-TRlist'.\n"
     "                * The TR value given in the input dataset header is ignored.\n"
     "\n"
     " -prefix ppp   = Prefix name for the output dataset.\n"
     "\n"
     " -TRout ddd    = 'ddd' gives the value for the output dataset's TR (in sec).\n"
     "                 If '-TRout' is not given, then the average TR of the input\n"
     "                 dataset will be used.\n"
     "\n"
     "November 2014 -- Zhark the Fixer\n"
     "\n"
   ) ;
   exit(0) ;
}

/*---------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int iarg=1 , ntin=0 , ii , nbad=0 ;
   THD_3dim_dataset *inset=NULL , *outset=NULL ;
   char *prefix="./TRfix" ;
   MRI_IMAGE *TRlist=NULL , *TIMElist=NULL ;
   float dtout=0.0f , *tgar, *dtar ;

   /*-- help the users if we can, they're feeling down down down --*/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ) TRfix_help() ;

   /*-- the legal formalities --*/

   mainENTRY("3dTRfix"); machdep();
   AFNI_logger("3dTRfix",argc,argv);
   PRINT_VERSION("3dTRfix"); AUTHOR("Zhark the Fixer");

   /*-- process args --*/

   while( iarg < argc ){

     /*-- read input dataset --*/

     if( strcasecmp(argv[iarg],"-input") == 0 ){
       if( inset != NULL ) ERROR_exit("Can't use '-input' twice") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       inset = THD_open_dataset(argv[iarg]) ;
       if( inset == NULL ) ERROR_exit("Can't open dataset '%s' :-(",argv[iarg]);
       ntin = DSET_NVALS(inset) ;
       if( ntin < 2 ) ERROR_exit("Dataset '%s' doesn't have any time dimension",argv[iarg]) ;
       DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
       iarg++ ; continue ;
     }

     /*-- read input 1D file --*/

     if( strcasecmp(argv[iarg],"-TRlist") == 0 ){
       if( TRlist   != NULL ) ERROR_exit("Can only use '-TRlist' once") ;
       if( TIMElist != NULL ) ERROR_exit("Can't use '-TRlist' after '-TIMElist'") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       TRlist = mri_read_1D( argv[iarg] ) ;
       if( TRlist == NULL ) ERROR_exit("Can't open '-TRlist' file '%s'",argv[iarg]) ;
       if( TRlist->nx == 1 && TRlist->ny > 1 ){
         MRI_IMAGE *qim = mri_transpose(TRlist); mri_free(TRlist); TRlist = qim ;
       }
       iarg++ ; continue ;
     }

     /*-- read input 1D file --*/

     if( strcasecmp(argv[iarg],"-TIMElist") == 0 ){
       if( TIMElist != NULL ) ERROR_exit("Can only use '-TIMElist' once") ;
       if( TRlist   != NULL ) ERROR_exit("Can't use '-TIMElist' after '-TRlist'") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       TIMElist = mri_read_1D( argv[iarg] ) ;
       if( TIMElist == NULL ) ERROR_exit("Can't open '-TIMElist' file '%s'",argv[iarg]) ;
       if( TIMElist->nx == 1 && TIMElist->ny > 1 ){
         MRI_IMAGE *qim = mri_transpose(TIMElist); mri_free(TIMElist); TIMElist = qim ;
       }
       iarg++ ; continue ;
     }

     /*-- read prefix --*/

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       if( !THD_filename_ok(argv[iarg]) )
         ERROR_exit("badly formed filename: '%s' '%s' :-(",argv[iarg-1],argv[iarg]) ;
       if( strcmp(argv[iarg],"NULL") == 0 )
         WARNING_message("'NULL' prefix will result in an unusual output filename") ;
       prefix = strdup(argv[iarg]) ; iarg++ ; continue ;
     }

     /*-- read dtout --*/

     if( strcasecmp(argv[iarg],"-TRout") == 0 || strcasecmp(argv[iarg],"-dt") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("no argument after '%s' :-(",argv[iarg-1]) ;
       dtout = (float)strtod(argv[iarg],NULL) ;
       if( dtout <= 0.0f )
         WARNING_message("-TRout '%s' means 3dTRfix will choose the output TR",argv[iarg]) ;
       iarg++ ; continue ;
     }

     /*-- really, what is going on here? --*/

     ERROR_message("Don't recognize this as an option: '%s'",argv[iarg]) ;
     suggest_best_prog_option(argv[0], argv[iarg]) ;
     exit(1) ;
  }

  /*-- check for user sanity --*/

  if( inset == NULL ){
    ERROR_message("No input dataset???") ; nbad++ ;
  }
  if( TRlist == NULL && TIMElist == NULL ){
    ERROR_message("Neither -TRlist nor -TIMElist was given???") ; nbad++ ;
  }
  if( TRlist != NULL && TRlist->nx < ntin-1 ){
    ERROR_message("-TRlist file has %d values, but input dataset has %d",TRlist->nx,ntin) ;
    nbad++ ;
  } else if( TIMElist != NULL && TIMElist->nx < ntin ){
    ERROR_message("-TIMElist file has %d values, but input dataset has %d",TIMElist->nx,ntin) ;
    nbad++ ;
  }

  /* process TRlist into TIMElist, if needed, by the black art of arithmetic  */

  if( TRlist != NULL ){
    TIMElist = mri_new(ntin,1,MRI_float) ;
    tgar = MRI_FLOAT_PTR(TIMElist) ;
    dtar = MRI_FLOAT_PTR(TRlist) ;
    tgar[0] = DSET_TIMEORIGIN(inset) ;   /* usually zero */
    for( ii=1 ; ii < ntin ; ii++ ){
      tgar[ii] = tgar[ii-1] + dtar[ii-1] ;  /* addition! */
      if( dtar[ii-1] <= 0.0f ){
        ERROR_message("-TRlist: #%d=%g -- but must be positive!",ii-1,dtar[ii-1]) ;
        nbad++ ;
      }
    }
  } else if( TIMElist != NULL ){  /* check TIMElist for monotonicity */
    tgar = MRI_FLOAT_PTR(TIMElist) ;
    for( ii=1 ; ii < ntin ; ii++ ){
      if( tgar[ii] <= tgar[ii-1] ){
        ERROR_message("-TIMElist: #%d=%g  #%d=%g -- these are out of order!" ,
                      ii-1,tgar[ii-1] , ii,tgar[ii] ) ;
        nbad++ ;
      }
    }
  }

  if( nbad > 0 )
    ERROR_exit(" !!! )-: 3dTRfix can't continue after such bad-ness :-( !!!") ;

  /*-- compute something that might be useful (or might not be) --*/

  outset = THD_resample_irregular_dataset( inset , TIMElist , prefix ,
                                           0 , dtout , 0.0f , MRI_LINEAR ) ;

  if( outset == NULL )
    ERROR_exit("Can't compute output dataset for some reason!?") ;

  /*-- run and hide, pretending that things are OK --*/

  INFO_message("Writing output dataset %s",DSET_BRIKNAME(outset)) ;
  DSET_write(outset) ;
  exit(0) ;
}
