#include "mrilib.h"
#include "mri_bport.c"

#undef  BMAX
#define BMAX 666

int main( int argc , char *argv[] )
{
   MRI_IMAGE *bpim ;
   int nopt=1 , nbad=0 ;
   float dt=1.0f ;
   int dtforce=0 , ntime=0 , tt , nblock=0 , *blocklist=NULL , *blocklen=NULL ;
   int bbot, btop ;
   int nband=0 , nozero=0 , quad=0 ; float fbot[BMAX] , ftop[BMAX] ;

   /*-----*/

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
       "                  ** This 'option' is actually mandatory! (At least once.)\n"
       "                   * For the un-enlightened, the Nyquist frequency is the\n"
       "                     highest frequency supported on the given grid, and\n"
       "                     is equal to 0.5/TR (units are Hz if TR is in s).\n"
       "                   * The lowest nonzero frequency supported on the grid\n"
       "                     is equaly to 1/(N*TR), where N=number of time points.\n"
       "                  ** Multiple -band options can be used, if needed.\n"
       "                     If the bands overlap, regressors will NOT be duplicated.\n"
       "                   * That is, '-band 0.01 0.05 -band 0.03 0.08' is the same\n"
       "                     as using '-band 0.01 0.08'.\n"
       "                  ** Note that if fbot==0 and ftop>=Nyquist frequency, you\n"
       "                     get a 'complete' set of trig functions, meaning that\n"
       "                     using these in regression is effectively a 'no-pass'\n"
       "                     filter -- probably not what you want!\n"
       "                  ** It is legitimate to set fbot = ftop.\n"
       "                  ** The 0 frequency (fbot = 0) component is all 1, of course.\n"
       "                     But unless you use the '-quad' option, nothing generated\n"
       "                     herein will deal well with linear-ish or quadratic-ish\n"
       "                     trends, which fall below the lowest nonzero frequency\n"
       "                     representable in a full cycle on the grid:\n"
       "                        f_low = 1 / ( NT * TR )\n"
       "                     where NT = number of time points.\n"
       "                  ** See the fourth EXAMPLE to learn how to use 3dDeconvolve\n"
       "                     to generate a file of polynomials for regression fun.\n"
       "\n"
       " -invert          = After computing which frequency indexes correspond to the\n"
       "                    input band(s), invert the selection -- that is, output\n"
       "                    all those frequencies NOT selected by the -band option(s).\n"
       "                    See the fifth EXAMPLE.\n"
       "\n"
       " -nozero          } Do NOT generate the 0 frequency (constant) component\n"
       "   *OR            } when fbot = 0; this has the effect of setting fbot to\n"
       " -noconst         } 1/(N*TR), and is essentially a convenient way to say\n"
       "                    'eliminate all oscillations below the ftop frequency'.\n"
       "\n"
       " -quad            = Add regressors for linear and quadratic trends.\n"
       "                    (These will be the last columns in the output.)\n"
       "\n"
       " -input dataset   } One of these options is used to specify the number of\n"
       "   *OR*           } time points to be created, as in 3dDeconvolve.\n"
       " -input1D 1Dfile  } ** '-input' allow catenated datasets, as in 3dDeconvolve.\n"
       "   *OR*           } ** '-input1D' assumes TR=1 unless you use the '-TR' option.\n"
       " -nodata NT [TR]  } ** One of these options is mandatory, to specify the length\n"
       "                       of the time series file to generate.\n"
       "\n"
       " -TR del          = Set the time step to 'del' rather than use the one\n"
       "                    given in the input dataset (if any).\n"
       "                   ** If TR is not specified by the -input dataset or by\n"
       "                      -nodata or by -TR, the program will assume it is 1.0 s.\n"
       "\n"
       " -concat rname    = As in 3dDeconvolve, used to specify the list of start\n"
       "                    indexes for concatenated runs.\n"
       "                   ** Also as in 3dDeconvolve, if the -input dataset is auto-\n"
       "                      catenated (by providing a list of more than one dataset),\n"
       "                      the run start list is automatically generated.  Otherwise,\n"
       "                      this option is needed if more than one run is involved.\n"
       "\n"
       "EXAMPLES\n"
       "--------\n"
       " The first example provides basis functions to filter out all frequency\n"
       " components from 0 to 0.25 Hz:\n"
       "   1dBport -nodata 100 1 -band 0 0.25   > highpass.1D\n"
       "\n"
       " The second example provides basis functions to filter out all frequency\n"
       " components from 0.25 Hz up to the Nyquist freqency:\n"
       "   1dBport -nodata 100 1 -band 0.25 666 > lowpass.1D\n"
       "\n"
       " The third example shows how to examine the results visually, for fun:\n"
       "   1dBport -nodata 100 1 -band 0.41 0.43 | 1dplot -stdin -thick\n"
       "\n"
       " The fourth example shows how to use 3dDeconvolve to generate a file of\n"
       " polynomial 'orts', in case you find yourself needing this ability someday\n"
       " (e.g., when stranded on a desert isle, with Gilligan, the Skipper, et al.):\n"
       "   3dDeconvolve -nodata 100 1 -polort 2 -x1D_stop -x1D stdout: | 1dcat stdin: > pol3.1D\n"
       "\n"
       " The fifth example shows how to use 1dBport to generate a set of regressors to\n"
       " eliminate all frequencies EXCEPT those in the selected range:\n"
       "   1dBport -nodata 100 1 -band 0.03 0.13 -nozero -invert | 1dplot -stdin\n"
       " In this example, the '-nozero' flag is used because the next step will be to\n"
       " 3dDeconvolve with '-polort 2' and '-ortvec' to get rid of the undesirable stuff.\n"
       "\n"
       "ETYMOLOGICAL NOTES\n"
       "------------------\n"
       " * The word 'ort' was coined by Andrzej Jesmanowicz, as a shorthand name for\n"
       "   a timeseries to which you want to 'orthogonalize' your data.\n"
       " * 'Ort' actually IS an English word, and means 'a scrap of food left from a meal'.\n"
       "   As far as I know, its only usage in modern English is in crossword puzzles,\n"
       "   and in Scrabble.\n"
       " * For other meanings of 'ort', see http://en.wikipedia.org/wiki/Ort\n"
       " * Do not confuse 'ort' with 'Oort': http://en.wikipedia.org/wiki/Oort_cloud\n"
       "\n"
       "AUTHOR -- RWCox -- Jan 2012\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("1dBport") ; machdep() ; AFNI_logger("1dBport",argc,argv) ;

   /*---- scan args ----*/

   while( nopt < argc ){

     /*-----*/

     if( strcasecmp(argv[nopt],"-nozero")  == 0 ||
         strcasecmp(argv[nopt],"-noconst") == 0 ||
         strcasecmp(argv[nopt],"-nonzero") == 0   ){

       mri_bport_set_ffbot(1) ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-quad") == 0 ){
       mri_bport_set_quad(1) ; quad = 1 ; nopt++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[nopt],"-invert") == 0 ){
       mri_bport_set_invert(1) ; nopt++ ; continue ;
     }

     /*-----*/

     if( strcasecmp(argv[nopt],"-band") == 0 ){
       float fb,ft ;
       if( nopt+2 >= argc ) ERROR_exit("Need 2 arguments after %s",argv[nopt]) ;
       if( nband  >= BMAX ) ERROR_exit("Too many -band options given :-(") ;
       fb = (float)strtod(argv[++nopt],NULL) ;
       ft = (float)strtod(argv[++nopt],NULL) ;
       if( fb < 0.0f || ft < fb )
         ERROR_exit("Illegal values after -band:  fbot=%g ftop=%g",fbot,ftop) ;
       fbot[nband] = fb ; ftop[nband] = ft ; nband++ ;
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
       nopt++ ; continue ;
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
   if( nband <= 0 ){
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

   fprintf(stderr,"++ Block length%s:",(nblock==1)?"\0":"s") ;
   for( tt=0 ; tt < nblock ; tt++ ) fprintf(stderr," %d",blocklen[tt]) ;
   fprintf(stderr,"\n") ;

   /*----- work -----*/

   bpim = mri_bport_multi( dt,nband,fbot,ftop , nblock,blocklen ) ;
   if( bpim == NULL )
     ERROR_exit("Computation fails! :-(((") ;
   mri_write_1D( "-" , bpim ) ;
   exit(0) ;
}
