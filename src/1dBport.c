#include "mrilib.h"
#include "mri_bport.c"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *bpim ;
   int nopt=1 , nbad=0 ;
   float fbot=-1.0f , ftop=-666.0f , dt=1.0f ;
   int dtforce=0 , ntime=0 , tt , nblock=0 , *blocklist=NULL , *blocklen=NULL ;
   int bbot, btop ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 1dBport [options]\n"
       "\n"
       "Creates a set of columns of sines and cosines for the purpose of\n"
       "bandpassing via regression (e.g., in 3dDeconvolve).  Various option\n"
       "are given to specify the duration and structure of the time series\n"
       "to be created.  Results are written to stdout, and usually should be\n"
       "redirected appropriately (cf. EXAMPLES, infra).  The file produced\n"
       "could be used with the '-ortvec' option to 3dDeconvolve, for example.\n"
       "\n"
       "OPTIONS\n"
       "-------\n"
       " -band fbot ftop  = Specify lowest and highest frequencies in the passband.\n"
       "                    fbot can be 0 if you want to do a highpass filter only;\n"
       "                    on the other hand, if ftop > Nyquist frequency, then\n"
       "                    it's a lowpass filter only.\n"
       "                  ** Note that if fbot==0 and ftop>=Nyquist frequency, you\n"
       "                     get a 'complete' set of trig functions, meaning that\n"
       "                     using these in regression is effectively a 'no-pass'\n"
       "                     filter -- probably not what you want!\n"
       "\n"
       " -input dataset   } One of these options is used to specify the number of\n"
       "   *OR*           } time points to be created, as in 3dDeconvolve.\n"
       " -input1D 1Dfile  } ** '-input' allow catenated datasets, as in 3dDeconvolve.\n"
       "   *OR*           } ** '-input1D' assumes TR=1 unless you use the '-TR' option.\n"
       " -nodata NT [TR]  }\n"
       "\n"
       " -TR del          = Set the time step to 'del' rather than use the one\n"
       "                    given in the input dataset (if any).\n"
       "\n"
       " -concat rname    = As in 3dDeconvolve, used to specify the list of\n"
       "                    start points for concatenated runs.\n"
       "\n"
       "EXAMPLES\n"
       "--------\n"
       " The first example provides basis functions to filter out all frequency\n"
       " components from 0 to 0.25 Hz:\n"
       "   1dBport -nodata 100 1 -band 0 0.25   > highpass.1D\n"
       " The second example provides basis functions to filter out all frequency\n"
       " components from 0.25 Hz up to the Nyquist freqency:\n"
       "   1dBport -nodata 100 1 -band 0.25 666 > lowpass.1D\n"
       " The third example shows how to examine the results visually, for fun:\n"
       "   1dBport -nodata 100 1 -band 0.10 0.11 | 1dplot -stdin -thick\n"
       "\n"
       "AUTHOR -- RWCox -- Jan 2012\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("1dBport") ; machdep() ; AFNI_logger("1dBport",argc,argv) ;

   /*---- scan args ----*/

   while( nopt < argc ){

     /*-----*/

     if( strcasecmp(argv[nopt],"-band") == 0 ){
       if( nopt+2 >= argc ) ERROR_exit("Need 2 arguments after %s",argv[nopt]) ;
       fbot = (float)strtod(argv[++nopt],NULL) ;
       ftop = (float)strtod(argv[++nopt],NULL) ;
       if( fbot < 0.0f || ftop < fbot )
         ERROR_exit("Illegal values after -band: fbot=%g  ftop=%g",fbot,ftop) ;
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[nopt],"-TR") == 0 || strcasecmp(argv[nopt],"-del") == 0 ){
       if( nopt+1 >= argc ) ERROR_exit("Need 1 argument after %s",argv[nopt]) ;
       dt = (float)strtod(argv[++nopt],NULL) ; dtforce = 1 ;
       if( dt <= 0.0f ){
         WARNING_message("Illegal TR=%g replaced with TR=1",dt); dt = 1.0f; dtforce = 0;
       }
       nopt++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[nopt],"-concat") == 0 ){
       MRI_IMAGE *bim ; float *bar ;
       if( nopt+1 >= argc ) ERROR_exit("Need 2 arguments after %s",argv[nopt]) ;
       if( nblock > 0 )
         WARNING_message("-concat over-riding existing block list") ;
       bim = mri_read_1D(argv[++nopt]) ;
       if( bim == NULL ) ERROR_exit("Can't read -concat file '%s",argv[nopt]) ;
       nblock = bim->nx ; bar = MRI_FLOAT_PTR(bim) ;
       blocklist = (int *)malloc(sizeof(int)*nblock) ;
       for( tt=0 ; tt < nblock ; tt++ ) blocklist[tt] = (int)floorf(bar[tt]+0.5f) ;
       nopt++ ;
     }

     /*-----*/

     if( strcasecmp(argv[nopt],"-nodata") == 0 ){
       if( nopt+1 >= argc ) ERROR_exit("Need at least 1 argument after %s",argv[nopt]) ;
       ntime = (int)strtod(argv[++nopt],NULL) ;
       if( ntime < 9 ) ERROR_exit("Value after -nodata is not allowable: %d",ntime) ;
       nopt++ ;
       if( nopt < argc && isdigit(argv[nopt][0]) ){
         dt = (float)strtod(argv[nopt++],NULL) ;
         if( dt <= 0.0f ){
           WARNING_message("Illegal TR=%g replaced with TR=1",dt) ; dt = 1.0f ;
         }
       }
       continue ;
     }

     /*-----*/

     if( strcasecmp(argv[nopt],"-input1D") == 0 ){
       MRI_IMAGE *tim ;
       if( nopt+1 >= argc ) ERROR_exit("Need 1 argument after %s",argv[nopt]) ;
       tim = mri_read_1D(argv[++nopt]) ;
       if( tim == NULL ) ERROR_exit("Can't read -input1D file '%s'",argv[nopt]) ;
       ntime = tim->nx ; mri_free(tim) ;
       if( ntime < 9 ) ERROR_exit("Length of -input1D file is not allowable: %d",ntime) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-input") == 0 ){
       int iopt , slen=0 ; char *fname ; THD_3dim_dataset *dset ;
       nopt++;
       if( nopt >= argc ) ERROR_exit("Need 1 or more arguments after -input") ;
       for( iopt=nopt ; iopt < argc && argv[iopt][0] != '-' ; iopt++ )
         slen += strlen(argv[iopt])+8 ;
       fname = calloc(sizeof(char),slen) ;
       for( iopt=nopt ; iopt < argc && argv[iopt][0] != '-' ; iopt++ ){
         strcat( fname , argv[iopt] ) ;
         strcat( fname , " "        ) ;
        }
       slen = strlen(fname) ; fname[slen-1] = '\0' ; /* trim last blank */
       nopt = iopt ;
       dset = THD_open_dataset(fname) ;
       CHECK_OPEN_ERROR(dset,fname) ;
       if( !dtforce ){ DSET_UNMSEC(dset) ; dt = DSET_TR(dset) ; }
       ntime = DSET_NVALS(dset) ;
       if( DSET_IS_TCAT(dset) ){
         nblock = dset->tcat_num ;
         blocklist = (int *)malloc(sizeof(int)*nblock) ;
         blocklist[0] = 0 ;
         for( tt=0 ; tt < nblock-1 ; tt++ )
           blocklist[tt+1] = blocklist[tt] + dset->tcat_len[tt] ;
       }
       DSET_delete(dset) ; continue ;
     }

     /*-----*/

     ERROR_message("Unknown option: %s",argv[nopt]) ;
     suggest_best_prog_option(argv[0], argv[nopt]) ;
     exit(1) ;

   } /* end of scanning options */

   /*----- check inputs -----*/

   if( ntime <= 0 ){
     ERROR_message("No way to determine time series length!") ; nbad++ ;
   }
   if( fbot < 0.0f || ftop < 0.0f ){
     ERROR_message("No -band option given! What bandpass do you want?") ; nbad++ ;
   }

   if( nblock == 0 ){
     nblock = 1 ;
     blocklist = (int *)malloc(sizeof(int)) ;
     blocklist[0] = 0 ;
   }

   blocklen = (int *)malloc(sizeof(int)*nblock) ;
   for( tt=0 ; tt < nblock ; tt++ ){
     bbot = blocklist[tt] ;
     btop = (tt+1 < nblock) ? blocklist[tt+1] : ntime ;
     blocklen[tt] = btop - bbot ;
     if( blocklen[tt] < 9 ){
       ERROR_message("Block length #%d = %d ==> too small!",tt+1,blocklen[tt]) ; nbad++ ;
     }
   }

   if( nbad > 0 )
     ERROR_exit("Cannot continue after above error%s :-(", (nbad==1) ? "\0" : "s" ) ;

   /*----- work -----*/

   bpim = mri_bport( dt,fbot,ftop , nblock,blocklen ) ;
   if( bpim == NULL )
     ERROR_exit("Computation fails! :-(((") ;
   mri_write_1D( "-" , bpim ) ;
   exit(0) ;
}
