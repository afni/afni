#include "mrilib.h"

#undef  AFNI_DEBUG
#undef  CLUST_DEBUG
#define STATUS(x) /* nada */
#define ENTRY(x)  /* nada */
#define EXRETURN  return
#define RETURN(x) return(x)


/**--------------------------------------------------------------------
   Check option # nopt to see if it is a legal dataset editing command.
   If so, put its value into "edopt" and return the number of options
   consumed.  If not, return 0.  An illegal editing option results in
   a fatal error!
-----------------------------------------------------------------------**/

#define CHECK_DONE return (nopt-nopt_in)

int EDIT_check_argv( int argc , char * argv[] , int nopt , EDIT_options * edopt )
{
   float val ;
   int  ival , nopt_in=nopt ;

   /**** -1clip val ****/

   if( strncmp(argv[nopt],"-1clip",6) == 0 || strncmp(argv[nopt],"-1uclip",6) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"no argument after %s?\n",argv[nopt-1]) ;
         exit(1);
      }
      edopt->clip_top = strtod( argv[nopt++] , NULL ) ;
      if( edopt->clip_top <= 0 ){
         fprintf(stderr,"illegal value after %s!\n",argv[nopt-2]) ;
         exit(1) ;
      }
      edopt->clip_bot = -edopt->clip_top ;
      edopt->clip_unscaled = (strncmp(argv[nopt-2],"-1uclip",6) == 0) ;
      CHECK_DONE ;
   }

   /**** -2clip val1 val2 ****/

   if( strncmp(argv[nopt],"-2clip",6) == 0 || strncmp(argv[nopt],"-2uclip",6) == 0 ){
      nopt++ ;
      if( nopt+1 >= argc ){
         fprintf(stderr,"no arguments after %s?\n",argv[nopt-1]) ;
         exit(1) ;
      }
      edopt->clip_bot = strtod( argv[nopt++] , NULL ) ;  /* bot */
      edopt->clip_top = strtod( argv[nopt++] , NULL ) ;  /* top */

      if( edopt->clip_bot >= edopt->clip_top ){
         fprintf(stderr,
                 "*** %s %f %f is illegal:\n"
                 "*** first value must be less than second value!\n",
                 argv[nopt-3] , edopt->clip_bot , edopt->clip_top ) ;
         exit(1) ;
      }
      edopt->clip_unscaled = (strncmp(argv[nopt-3],"-2uclip",6) == 0) ;
      CHECK_DONE ;
   }

   /**** -1thtoin ****/

   if( strncmp(argv[nopt],"-1thtoin",6) == 0 ){
      edopt->thtoin = 1 ;
      nopt++ ; CHECK_DONE ;
   }

   /**** -2thtoin ****/

   if( strncmp(argv[nopt],"-2thtoin",6) == 0 ){
      edopt->thtoin = 2 ;
      nopt++ ; CHECK_DONE ;
   }

   /**** -1noneg ****/

   if( strncmp(argv[nopt],"-1noneg",6) == 0 ){
      edopt->noneg = 1 ;
      nopt++ ; CHECK_DONE ;
   }

   /**** -1abs ****/

   if( strncmp(argv[nopt],"-1abs",6) == 0 ){
      edopt->abss = 1 ;
      nopt++ ; CHECK_DONE ;
   }

   /**** -1thresh thr ****/

   if( strncmp(argv[nopt],"-1thresh",6) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"no argument after -1thresh!\n") ;
         exit(1);
      }
      val = strtod( argv[nopt++] , NULL ) ;
      if( val < 0.0 ){
         fprintf(stderr,"illegal value after -1thresh!\n") ;
         exit(1) ;
      }
      edopt->thresh = val ;
      CHECK_DONE ;
   }

   /**** -1clust rmm vmul ****/

   if( strncmp(argv[nopt],"-1clust",12) == 0 ){
      nopt++ ;
      if( nopt+1 >= argc ){
         fprintf(stderr,"need 2 arguments after -1clust!\n") ;
         exit(1) ;
      }
      edopt->edit_clust = ECFLAG_SAME;
      edopt->clust_rmm  = strtod( argv[nopt++] , NULL ) ;
      edopt->clust_vmul = strtod( argv[nopt++] , NULL ) ;
      if( edopt->clust_rmm <= 0 || edopt->clust_vmul <= 0 ){
         fprintf(stderr,"illegal value after -1clust\n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }


   /**** -1clust_mean rmm vmul ****/   /* 10 Sept 1996 */

   if( strncmp(argv[nopt],"-1clust_mean",12) == 0 ){
      nopt++ ;
      if( nopt+1 >= argc ){
         fprintf(stderr,"need 2 arguments after -1clust_mean!\n") ;
         exit(1) ;
      }
      edopt->edit_clust = ECFLAG_MEAN;
      edopt->clust_rmm  = strtod( argv[nopt++] , NULL ) ;
      edopt->clust_vmul = strtod( argv[nopt++] , NULL ) ;
      if( edopt->clust_rmm <= 0 || edopt->clust_vmul <= 0 ){
         fprintf(stderr,"illegal value after -1clust_mean\n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }

   /**** -1clust_max rmm vmul ****/   /* 10 Sept 1996 */

   if( strncmp(argv[nopt],"-1clust_max",12) == 0 ){
      nopt++ ;
      if( nopt+1 >= argc ){
         fprintf(stderr,"need 2 arguments after -1clust_max!\n") ;
         exit(1) ;
      }
      edopt->edit_clust = ECFLAG_MAX;
      edopt->clust_rmm  = strtod( argv[nopt++] , NULL ) ;
      edopt->clust_vmul = strtod( argv[nopt++] , NULL ) ;
      if( edopt->clust_rmm <= 0 || edopt->clust_vmul <= 0 ){
         fprintf(stderr,"illegal value after -1clust_max\n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }

   /**** -1clust_amax rmm vmul ****/   /* 10 Sept 1996 */

   if( strncmp(argv[nopt],"-1clust_amax",12) == 0 ){
      nopt++ ;
      if( nopt+1 >= argc ){
         fprintf(stderr,"need 2 arguments after -1clust_amax!\n") ;
         exit(1) ;
      }
      edopt->edit_clust = ECFLAG_AMAX;
      edopt->clust_rmm  = strtod( argv[nopt++] , NULL ) ;
      edopt->clust_vmul = strtod( argv[nopt++] , NULL ) ;
      if( edopt->clust_rmm <= 0 || edopt->clust_vmul <= 0 ){
         fprintf(stderr,"illegal value after -1clust_amax\n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }

   /**** -1clust_smax rmm vmul ****/   /* 10 Sept 1996 */

   if( strncmp(argv[nopt],"-1clust_smax",12) == 0 ){
      nopt++ ;
      if( nopt+1 >= argc ){
         fprintf(stderr,"need 2 arguments after -1clust_smax!\n") ;
         exit(1) ;
      }
      edopt->edit_clust = ECFLAG_SMAX;
      edopt->clust_rmm  = strtod( argv[nopt++] , NULL ) ;
      edopt->clust_vmul = strtod( argv[nopt++] , NULL ) ;
      if( edopt->clust_rmm <= 0 || edopt->clust_vmul <= 0 ){
         fprintf(stderr,"illegal value after -1clust_smax\n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }

   /**** -1clust_size rmm vmul ****/   /* 10 Sept 1996 */

   if( strncmp(argv[nopt],"-1clust_size",12) == 0 ){
      nopt++ ;
      if( nopt+1 >= argc ){
         fprintf(stderr,"need 2 arguments after -1clust_size!\n") ;
         exit(1) ;
      }
      edopt->edit_clust = ECFLAG_SIZE;
      edopt->clust_rmm  = strtod( argv[nopt++] , NULL ) ;
      edopt->clust_vmul = strtod( argv[nopt++] , NULL ) ;
      if( edopt->clust_rmm <= 0 || edopt->clust_vmul <= 0 ){
         fprintf(stderr,"illegal value after -1clust_size\n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }

   /**** -1filter_mean rmm ****/   /* 11 Sept 1996 */

   if( strncmp(argv[nopt],"-1filter_mean",15) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need 1 argument  after -1filter_mean \n") ;
         exit(1) ;
      }
      edopt->filter_opt = FCFLAG_MEAN;
      edopt->filter_rmm  = strtod( argv[nopt++] , NULL ) ;
      if( edopt->filter_rmm <= 0 ){
         fprintf(stderr,"illegal value after -1filter_mean \n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }

   /**** -1filter_nzmean rmm ****/   /* 11 Sept 1996 */

   if( strncmp(argv[nopt],"-1filter_nzmean",15) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need 1 argument  after -1filter_nzmean \n") ;
         exit(1) ;
      }
      edopt->filter_opt = FCFLAG_NZMEAN;
      edopt->filter_rmm  = strtod( argv[nopt++] , NULL ) ;
      if( edopt->filter_rmm <= 0 ){
         fprintf(stderr,"illegal value after -1filter_nzmean \n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }

   /**** -1filter_max rmm ****/   /* 11 Sept 1996 */

   if( strncmp(argv[nopt],"-1filter_max",15) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need 1 argument  after -1filter_max \n") ;
         exit(1) ;
      }
      edopt->filter_opt = FCFLAG_MAX;
      edopt->filter_rmm  = strtod( argv[nopt++] , NULL ) ;
      if( edopt->filter_rmm <= 0 ){
         fprintf(stderr,"illegal value after -1filter_max \n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }

   /**** -1filter_amax rmm ****/   /* 11 Sept 1996 */

   if( strncmp(argv[nopt],"-1filter_amax",15) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need 1 argument  after -1filter_amax \n") ;
         exit(1) ;
      }
      edopt->filter_opt = FCFLAG_AMAX;
      edopt->filter_rmm  = strtod( argv[nopt++] , NULL ) ;
      if( edopt->filter_rmm <= 0 ){
         fprintf(stderr,"illegal value after -1filter_amax \n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }

   /**** -1filter_smax rmm ****/   /* 11 Sept 1996 */

   if( strncmp(argv[nopt],"-1filter_smax",15) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need 1 argument  after -1filter_smax \n") ;
         exit(1) ;
      }
      edopt->filter_opt = FCFLAG_SMAX;
      edopt->filter_rmm  = strtod( argv[nopt++] , NULL ) ;
      if( edopt->filter_rmm <= 0 ){
         fprintf(stderr,"illegal value after -1filter_smax \n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }

   /**** -1filter_aver rmm ****/   /* 07 Jan 1998 */

   if( strncmp(argv[nopt],"-1filter_aver",15) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need 1 argument  after -1filter_aver \n") ;
         exit(1) ;
      }
      edopt->filter_opt = FCFLAG_AVER ;
      edopt->filter_rmm  = strtod( argv[nopt++] , NULL ) ;
      if( edopt->filter_rmm <= 0 ){
         fprintf(stderr,"illegal value after -1filter_aver \n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }


   /**** -t1filter_aver rmm ****/   /* 07 Jan 1998 */

   if( strncmp(argv[nopt],"-t1filter_aver",15) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need 1 argument  after -t1filter_aver \n") ;
         exit(1) ;
      }
      edopt->thrfilter_opt = FCFLAG_AVER ;
      edopt->thrfilter_rmm  = strtod( argv[nopt++] , NULL ) ;
      if( edopt->thrfilter_rmm <= 0 ){
         fprintf(stderr,"illegal value after -t1filter_aver \n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }


   /**** -t1filter_mean rmm ****/   /* 1 Oct 1996 */

   if( strncmp(argv[nopt],"-t1filter_mean",15) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need 1 argument  after -t1filter_mean \n") ;
         exit(1) ;
      }
      edopt->thrfilter_opt = FCFLAG_MEAN;
      edopt->thrfilter_rmm  = strtod( argv[nopt++] , NULL ) ;
      if( edopt->thrfilter_rmm <= 0 ){
         fprintf(stderr,"illegal value after -t1filter_mean \n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }


   /**** -t1filter_nzmean rmm ****/   /* 1 Oct 1996 */

   if( strncmp(argv[nopt],"-t1filter_nzmean",15) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need 1 argument  after -t1filter_nzmean \n") ;
         exit(1) ;
      }
      edopt->thrfilter_opt = FCFLAG_NZMEAN;
      edopt->thrfilter_rmm  = strtod( argv[nopt++] , NULL ) ;
      if( edopt->thrfilter_rmm <= 0 ){
         fprintf(stderr,"illegal value after -t1filter_nzmean \n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }


   /**** -t1filter_max rmm ****/   /* 1 Oct 1996 */

   if( strncmp(argv[nopt],"-t1filter_max",15) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need 1 argument  after -t1filter_max \n") ;
         exit(1) ;
      }
      edopt->thrfilter_opt = FCFLAG_MAX;
      edopt->thrfilter_rmm  = strtod( argv[nopt++] , NULL ) ;
      if( edopt->thrfilter_rmm <= 0 ){
         fprintf(stderr,"illegal value after -t1filter_max \n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }


   /**** -t1filter_amax rmm ****/   /* 1 Oct 1996 */

   if( strncmp(argv[nopt],"-t1filter_amax",15) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need 1 argument  after -t1filter_amax \n") ;
         exit(1) ;
      }
      edopt->thrfilter_opt = FCFLAG_AMAX;
      edopt->thrfilter_rmm  = strtod( argv[nopt++] , NULL ) ;
      if( edopt->thrfilter_rmm <= 0 ){
         fprintf(stderr,"illegal value after -t1filter_amax \n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }


   /**** -t1filter_smax rmm ****/   /* 1 Oct 1996 */

   if( strncmp(argv[nopt],"-t1filter_smax",15) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need 1 argument  after -t1filter_smax \n") ;
         exit(1) ;
      }
      edopt->thrfilter_opt = FCFLAG_SMAX;
      edopt->thrfilter_rmm  = strtod( argv[nopt++] , NULL ) ;
      if( edopt->thrfilter_rmm <= 0 ){
         fprintf(stderr,"illegal value after -t1filter_smax \n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }




   /**** -1blur_sigma size ****/

   if( strncmp(argv[nopt],"-1blur_sigma",12) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need argument after -1blur_sigma!\n") ;
         exit(1) ;
      }
      edopt->blur = strtod( argv[nopt++] , NULL ) ;
      if( edopt->blur <= 0 ){
         fprintf(stderr,"illegal value after -1blur_sigma\n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }

   /**** -1blur_rms size ****/

   if( strncmp(argv[nopt],"-1blur_rms",12) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need argument after -1blur_rms!\n") ;
         exit(1) ;
      }
      edopt->blur = strtod( argv[nopt++] , NULL ) ;
      if( edopt->blur <= 0 ){
         fprintf(stderr,"illegal value after -1blur_rms\n") ;
         exit(1) ;
      }
      edopt->blur = RMS_TO_SIGMA(edopt->blur) ;
      CHECK_DONE ;
   }

   /**** -1blur_fwhm size ****/

   if( strncmp(argv[nopt],"-1blur_fwhm",12) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need argument after -1blur_fwhm!\n") ;
         exit(1) ;
      }
      edopt->blur = strtod( argv[nopt++] , NULL ) ;
      if( edopt->blur <= 0 ){
         fprintf(stderr,"illegal value after -1blur_fwhm\n") ;
         exit(1) ;
      }
      edopt->blur = FWHM_TO_SIGMA(edopt->blur) ;
      CHECK_DONE ;
   }

   /**** -1blur ****/

   if( strncmp(argv[nopt],"-1blur",6) == 0 ){
      fprintf(stderr,
              "*** the old -1blur option is no longer valid! ***\n") ;
      exit(1) ;
   }

   /**** -t1blur_sigma size ****/   /* 4 Oct 1996 */

   if( strncmp(argv[nopt],"-t1blur_sigma",12) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need argument after -t1blur_sigma!\n") ;
         exit(1) ;
      }
      edopt->thrblur = strtod( argv[nopt++] , NULL ) ;
      if( edopt->thrblur <= 0 ){
         fprintf(stderr,"illegal value after -t1blur_sigma\n") ;
         exit(1) ;
      }
      CHECK_DONE ;
   }

   /**** -t1blur_rms size ****/   /* 4 Oct 1996 */

   if( strncmp(argv[nopt],"-t1blur_rms",12) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need argument after -t1blur_rms!\n") ;
         exit(1) ;
      }
      edopt->thrblur = strtod( argv[nopt++] , NULL ) ;
      if( edopt->thrblur <= 0 ){
         fprintf(stderr,"illegal value after -t1blur_rms\n") ;
         exit(1) ;
      }
      edopt->thrblur = RMS_TO_SIGMA(edopt->thrblur) ;
      CHECK_DONE ;
   }

   /**** -t1blur_fwhm size ****/   /* 4 Oct 1996 */

   if( strncmp(argv[nopt],"-t1blur_fwhm",12) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"need argument after -t1blur_fwhm!\n") ;
         exit(1) ;
      }
      edopt->thrblur = strtod( argv[nopt++] , NULL ) ;
      if( edopt->thrblur <= 0 ){
         fprintf(stderr,"illegal value after -t1blur_fwhm\n") ;
         exit(1) ;
      }
      edopt->thrblur = FWHM_TO_SIGMA(edopt->thrblur) ;
      CHECK_DONE ;
   }


   /**** -1scale ****/

   if( strncmp(argv[nopt],"-1scale",6) == 0 ){
#ifdef ALLOW_SCALE_TO_MAX
      edopt->scale = 1 ;
#else
      fprintf(stderr,
              "*** the old -1scale option is no longer valid! ***\n") ;
#endif
      nopt++ ; CHECK_DONE ;
   }

   /**** -1mult mult ****/

   if( strncmp(argv[nopt],"-1mult",6) == 0 ){
      nopt++ ;
      if( nopt >= argc ){
         fprintf(stderr,"no argument after -1mult!\n") ;
         exit(1);
      }
      val = strtod( argv[nopt++] , NULL ) ;
      if( val == 0.0 ){
         fprintf(stderr,"illegal value after -1mult!\n") ;
         exit(1) ;
      }
      edopt->mult = val ;
      CHECK_DONE ;
    }

    /**** -1zvol x1 x2 y1 y2 z1 z2 ***/

    if( strncmp(argv[nopt],"-1zvol",6) == 0 ){
      char * cerr ;

      if( nopt+6 >= argc ){
         fprintf(stderr,"need 6 arguments after -1zvol!\a\n") ;
         exit(1) ;
      }

      edopt->zv_x1 = strtod( argv[nopt+1] , &cerr ) ;
      if( cerr == argv[nopt+1] ){
         fprintf(stderr,"illegal 1st argument after -1zvol!\a\n") ;
         exit(1) ;
      }

      edopt->zv_x2 = strtod( argv[nopt+2] , &cerr ) ;
      if( cerr == argv[nopt+2] ){
         fprintf(stderr,"illegal 2nd argument after -1zvol!\a\n") ;
         exit(1) ;
      }

      edopt->zv_y1 = strtod( argv[nopt+3] , &cerr ) ;
      if( cerr == argv[nopt+3] ){
         fprintf(stderr,"illegal 3rd argument after -1zvol!\a\n") ;
         exit(1) ;
      }

      edopt->zv_y2 = strtod( argv[nopt+4] , &cerr ) ;
      if( cerr == argv[nopt+4] ){
         fprintf(stderr,"illegal 4th argument after -1zvol!\a\n") ;
         exit(1) ;
      }

      edopt->zv_z1 = strtod( argv[nopt+5] , &cerr ) ;
      if( cerr == argv[nopt+5] ){
         fprintf(stderr,"illegal 5th argument after -1zvol!\a\n") ;
         exit(1) ;
      }

      edopt->zv_z2 = strtod( argv[nopt+6] , &cerr ) ;
      if( cerr == argv[nopt+6] ){
         fprintf(stderr,"illegal 6th argument after -1zvol!\a\n") ;
         exit(1) ;
      }
      edopt->do_zvol = 1 ;

      nopt += 7 ; CHECK_DONE ;
   }

   return 0 ;
}
