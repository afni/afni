/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This program takes bucket sub-bricks and creates a fim (fico, fitt,
  fift, ...) dataset.  This program was adapted from 3dbucket.c.


  File:    3dbuc2fim.c
  Author:  B. Douglas Ward
  Date:    18 March 1998

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dbuc2fim"                     /* name of this program */
#define PROGRAM_AUTHOR "B. D. Ward"                        /* program author */
#define PROGRAM_INITIAL "18 March 1998"   /* date of initial program release */
#define PROGRAM_LATEST  "15 August 2001"  /* date of latest program revision */

/*---------------------------------------------------------------------------*/


#include "mrilib.h"

#ifndef myXtFree
#   define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

/*-------------------------- global data --------------------------*/

static THD_3dim_dataset_array * B2F_dsar  = NULL ;
static RwcPointer_array        * B2F_subv  = NULL ;
static int                      B2F_nvox  = -1 ;
static int                      B2F_verb  = 0 ;
static int                      B2F_func_type  = -1;

static char B2F_output_prefix[THD_MAX_PREFIX] = "buc2fim" ;
static char B2F_session[THD_MAX_NAME]         = "./"   ;

#define NSUBV(id)   ( ((int *)B2F_subv->ar[(id)])[0]      )
#define SUBV(id,jj) ( ((int *)B2F_subv->ar[(id)])[(jj)+1] )
#define DSUB(id)    DSET_IN_3DARR(B2F_dsar,(id))

/*--------------------------- prototypes ---------------------------*/

void B2F_read_opts( int , char ** ) ;
void B2F_Syntax(void) ;
int * B2F_get_subv( int , char * ) ;

/*--------------------------------------------------------------------
   read the arguments, load the global variables
----------------------------------------------------------------------*/

void B2F_read_opts( int argc , char * argv[] )
{
   int nopt = 1 , ii ;
   char dname[THD_MAX_NAME] ;
   char subv[THD_MAX_NAME] ;
   char * cpt ;
   THD_3dim_dataset * dset ;
   int * svar ;
   char * str;
   int ok, ilen, nlen;

   INIT_3DARR(B2F_dsar) ;
   INIT_XTARR(B2F_subv) ;

   while( nopt < argc ){

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ||
          strncmp(argv[nopt],"-output",6) == 0   ){
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -prefix!\n") ; exit(1) ;
         }
         MCW_strncpy( B2F_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

      /**** -session directory ****/

      if( strncmp(argv[nopt],"-session",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -session!\n") ; exit(1) ;
         }
         MCW_strncpy( B2F_session , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }

      if( strncmp(argv[nopt],"-verb",5) == 0 ){
         B2F_verb = 1 ;
         nopt++ ; continue ;
      }


      if( argv[nopt][0] == '-' ){
         fprintf(stderr,"Unknown option: %s\n",argv[nopt]) ; exit(1) ;
      }

      /**** read dataset ****/

      cpt = strstr(argv[nopt],"[") ;
      if( cpt == NULL ){
         strcpy(dname,argv[nopt]) ;
         subv[0] = '\0' ;
      } else if( cpt == argv[nopt] ){
         fprintf(stderr,"illegal dataset specifier: %s\n",argv[nopt]) ;
         exit(1) ;
      } else {
         ii = cpt - argv[nopt] ;
         memcpy(dname,argv[nopt],ii) ; dname[ii] = '\0' ;
         strcpy(subv,cpt) ;
      }
      nopt++ ;

      dset = THD_open_one_dataset( dname ) ;
      if( dset == NULL ){
         fprintf(stderr,"can't open dataset %s\n",dname) ; exit(1) ;
      }
      THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_MALLOC ) ;

      ii = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz ;
      if( B2F_nvox < 0 ){
         B2F_nvox = ii ;
      } else if( ii != B2F_nvox ){
         fprintf(stderr,"dataset %s differs in size from others\n",dname);
         exit(1) ;
      }
      ADDTO_3DARR(B2F_dsar,dset) ;

      svar = B2F_get_subv( DSET_NVALS(dset) , subv ) ;
      if( svar == NULL || svar[0] <= 0 ){
         fprintf(stderr,"can't decipher index codes from %s%s\n",dname,subv) ;
         exit(1) ;
      }
      ADDTO_XTARR(B2F_subv,svar) ;

   }  /* end of loop over command line arguments */

   return ;
}

/*------------------------------------------------------------------*/

int * B2F_get_subv( int nvals , char * str )
{
   int * subv = NULL ;
   int ii , ipos , nout , slen ;
   int ibot,itop,istep , nused ;
   char * cpt ;

   /* Meaningless input? */

   if( nvals < 1 ) return NULL ;

   /* No selection list ==> select it all */

   if( str == NULL || str[0] == '\0' ){
      subv = (int *) XtMalloc( sizeof(int) * (nvals+1) ) ;
      subv[0] = nvals ;
      for( ii=0 ; ii < nvals ; ii++ ) subv[ii+1] = ii ;
      return subv ;
   }

   /* skip initial '[' */

   subv    = (int *) XtMalloc( sizeof(int) * 2 ) ;
   subv[0] = nout = 0 ;

   ipos = 0 ;
   if( str[ipos] == '[' ) ipos++ ;

   /*** loop through each sub-selector until end of input ***/

   slen = strlen(str) ;
   while( ipos < slen && str[ipos] != ']' ){

      /** get starting value **/

      if( str[ipos] == '$' ){  /* special case */
         ibot = nvals-1 ; ipos++ ;
      } else {                 /* decode an integer */
         ibot = strtol( str+ipos , &cpt , 10 ) ;
         if( ibot < 0 ){ myXtFree(subv) ; return NULL ; }
         if( ibot >= nvals ) ibot = nvals-1 ;
         nused = (cpt-(str+ipos)) ;
         if( ibot == 0 && nused == 0 ){ myXtFree(subv) ; return NULL ; }
         ipos += nused ;
      }

      /** if that's it for this sub-selector, add one value to list **/

      if( str[ipos] == ',' || str[ipos] == '\0' || str[ipos] == ']' ){
         nout++ ;
         subv = (int *) XtRealloc( (char *)subv , sizeof(int) * (nout+1) ) ;
         subv[0]    = nout ;
         subv[nout] = ibot ;
         ipos++ ; continue ;  /* re-start loop at next sub-selector */
      }

      /** otherwise, must have '..' or '-' as next inputs **/

      if( str[ipos] == '-' ){
         ipos++ ;
      } else if( str[ipos] == '.' && str[ipos+1] == '.' ){
         ipos++ ; ipos++ ;
      } else {
         myXtFree(subv) ; return NULL ;
      }

      /** get ending value for loop now **/

      if( str[ipos] == '$' ){  /* special case */
         itop = nvals-1 ; ipos++ ;
      } else {                 /* decode an integer */
         itop = strtol( str+ipos , &cpt , 10 ) ;
         if( itop < 0 ){ myXtFree(subv) ; return NULL ; }
         if( itop >= nvals ) itop = nvals-1 ;
         nused = (cpt-(str+ipos)) ;
         if( itop == 0 && nused == 0 ){ myXtFree(subv) ; return NULL ; }
         ipos += nused ;
      }

      /** set default loop step **/

      istep = (ibot <= itop) ? 1 : -1 ;

      /** check if we have a non-default loop step **/

      if( str[ipos] == '(' ){  /* decode an integer */
         ipos++ ;
         istep = strtol( str+ipos , &cpt , 10 ) ;
         if( istep == 0 ){ myXtFree(subv) ; return NULL ; }
         nused = (cpt-(str+ipos)) ;
         ipos += nused ;
         if( str[ipos] == ')' ) ipos++ ;
      }

      /** add values to output **/

      for( ii=ibot ; (ii-itop)*istep <= 0 ; ii += istep ){
         nout++ ;
         subv = (int *) XtRealloc( (char *)subv , sizeof(int) * (nout+1) ) ;
         subv[0]    = nout ;
         subv[nout] = ii ;
      }

      /** check if we have a comma to skip over **/

      if( str[ipos] == ',' ) ipos++ ;

   }  /* end of loop through selector string */

   return subv ;
}

/*------------------------------------------------------------------*/
/*
   Routine to display 3dbuc2fim help menu.
*/


void B2F_Syntax(void)
{
  printf
    (
     "This program converts bucket sub-bricks to fim (fico, fitt, fift, ...)\n"
     "type dataset.                                                       \n\n"
     "Usage:                                                              \n\n"
     "3dbuc2fim  -prefix pname  d1+orig[index]                              \n"
     "     This produces a fim dataset.                                   \n\n"
     " -or-                                                               \n\n"
     "3dbuc2fim  -prefix pname  d1+orig[index1]  d2+orig[index2]            \n"
     "     This produces a fico (fitt, fift, ...) dataset,                  \n"
     "     depending on the statistic type of the 2nd subbrick,             \n"
     "     with   d1+orig[index1] -> intensity sub-brick of pname           \n"
     "            d2+orig[index2] -> threshold sub-brick of pname         \n\n"
     " -or-                                                               \n\n"
     "3dbuc2fim  -prefix pname  d1+orig[index1,index2]                      \n"
     "     This produces a fico (fitt, fift, ...) dataset,                  \n"
     "     depending on the statistic type of the 2nd subbrick,             \n"
     "     with   d1+orig[index1] -> intensity sub-brick of pname           \n"
     "            d1+orig[index2] -> threshold sub-brick of pname         \n\n"
     "where the options are:\n"
   ) ;

   printf(
    "     -prefix pname = Use 'pname' for the output dataset prefix name.\n"
    " OR  -output pname     [default='buc2fim']\n"
    "\n"
    "     -session dir  = Use 'dir' for the output dataset session directory.\n"
    "                       [default='./'=current working directory]\n"
    "     -verb         = Print out some verbose output as the program\n"
    "                       proceeds \n"
    "\n"
    "Command line arguments after the above are taken as input datasets.  \n"
    "A dataset is specified using one of these forms:\n"
    "   'prefix+view', 'prefix+view.HEAD', or 'prefix+view.BRIK'.\n"
    "Sub-brick indexes start at 0. \n"
    "\n"
    "N.B.: The sub-bricks are output in the order specified, which may\n"
    " not be the order in the original datasets.  For example, using\n"
    "           fred+orig[5,3]\n"
    " will cause the sub-brick #5 in fred+orig to be output as the intensity\n"
    " sub-brick, and sub-brick #3 to be output as the threshold sub-brick \n"
    " in the new dataset.\n"
    "\n"
    "N.B.: The '$', '(', ')', '[', and ']' characters are special to\n"
    " the shell, so you will have to escape them.  This is most easily\n"
    " done by putting the entire dataset plus selection list inside\n"
    " single quotes, as in 'fred+orig[5,9]'.\n"
    "\n"
    ) ;

   PRINT_COMPILE_DATE ; exit(0) ;
}

/*------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int ninp , ids , nv , iv,jv,kv , ivout , new_nvals ;
   THD_3dim_dataset * new_dset=NULL , * dset ;
   char buf[256] ;

  /*----- Identify software -----*/
#if 0
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR);
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");
#endif

WARNING_message("This program (3dbuc2fim) is old, not maintained, and probably useless!") ;

   /*** read input options ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) B2F_Syntax() ;

   mainENTRY("3dbuc2fim main"); machdep(); AFNI_logger(PROGRAM_NAME,argc,argv);
   PRINT_VERSION("3dbuc2fim") ; AUTHOR(PROGRAM_AUTHOR);

   B2F_read_opts( argc , argv ) ;

   /*** create new dataset (empty) ***/
   ninp = B2F_dsar->num ;
   if( ninp < 1 ){
      fprintf(stderr,"*** No input datasets?\n") ; exit(1) ;
   }

   new_nvals = 0 ;
   for( ids=0 ; ids < ninp ; ids++ ) new_nvals += NSUBV(ids) ;

   /*----- Check for acceptable number of sub-bricks -----*/
   if (new_nvals < 1)
     { fprintf(stderr,"*** Less than 1 sub-brick specified\n") ; exit(1) ; }
   if (new_nvals > 2)
     { fprintf(stderr,"*** More than 2 sub-bricks specified\n") ; exit(1) ; }


   if( B2F_verb ) printf("-verb: output will have %d sub-bricks\n",new_nvals) ;

   new_dset = EDIT_empty_copy( DSUB(0) ) ;

   if( ninp == 1 ) tross_Copy_History( DSUB(0) , new_dset ) ;
   tross_Make_History( "3dbuc2fim" , argc,argv , new_dset ) ;

   /*-----  Set default value for function type. This will be changed later,
            if the second sub-brick has a statistic type.  -----*/
   if (new_nvals == 1)
     B2F_func_type = FUNC_FIM_TYPE;
   else
     B2F_func_type = FUNC_THR_TYPE;


   EDIT_dset_items (new_dset ,
		    ADN_prefix        , B2F_output_prefix ,
		    ADN_directory_name, B2F_session ,
		    ADN_type          , HEAD_FUNC_TYPE,
		    ADN_func_type     , B2F_func_type,
		    ADN_ntt           , 0 ,
		    ADN_nvals         , new_nvals ,
                    ADN_none ) ;


   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(new_dset)) ){
     fprintf(stderr,"*** Fatal error: file %s already exists!\n",
	     DSET_HEADNAME(new_dset) ) ;
     exit(1) ;
   }

   THD_force_malloc_type( new_dset->dblk , DATABLOCK_MEM_MALLOC ) ;

   /*** loop over input datasets ***/

   if( ninp > 1 ) myXtFree( new_dset->keywords ) ;

   ivout = 0 ;
   for( ids=0 ; ids < ninp ; ids++ ){
      dset = DSUB(ids) ;
      nv   = NSUBV(ids) ;

      DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;


      /** loop over sub-bricks to output **/

      for( iv=0 ; iv < nv ; iv++ ){
         jv = SUBV(ids,iv) ;                /* which sub-brick to use */

	 EDIT_substitute_brick( new_dset , ivout ,
				DSET_BRICK_TYPE(dset,jv) , DSET_ARRAY(dset,jv) ) ;
	
	 /*----- If this sub-brick is from a bucket dataset,
	   preserve the label for this sub-brick -----*/
	 if (dset->func_type == FUNC_BUCK_TYPE)
	   sprintf (buf, "%s", DSET_BRICK_LABEL(dset,jv));
	 else
	   sprintf(buf,"%.12s[%d]",DSET_PREFIX(dset),jv) ;
	 EDIT_dset_items( new_dset , ADN_brick_label_one+ivout, buf , ADN_none ) ;

#if 0	
	 sprintf(buf,"%s[%d]",DSET_FILECODE(dset),jv) ;
	 EDIT_dset_items(
			 new_dset, ADN_brick_keywords_replace_one+ivout, buf, ADN_none ) ;
#endif
	
	 EDIT_dset_items(
			 new_dset ,
			 ADN_brick_fac_one            +ivout, DSET_BRICK_FACTOR(dset,jv),
#if 0
			 ADN_brick_keywords_append_one+ivout, DSET_BRICK_KEYWORDS(dset,jv) ,
#endif
			 ADN_none ) ;
	
	 /** possibly write statistical parameters for this sub-brick **/
	
	 kv = DSET_BRICK_STATCODE(dset,jv) ;
	
	 if( FUNC_IS_STAT(kv) ){ /* input sub-brick has stat params */
	
	   int npar = MAX_STAT_AUX , lv ;
	   float * par = (float *) malloc( sizeof(float) * (npar) ) ;
	   float * sax = DSET_BRICK_STATAUX(dset,jv) ;
	   for( lv=0 ; lv < npar ; lv++ )
	     par[lv] = (sax != NULL && lv < FUNC_need_stat_aux[kv]) ? sax[lv] : 0.0;
	
	   if (ivout == 1)
	     {
	       EDIT_dset_items(new_dset ,
			       ADN_func_type     , kv,		
			       ADN_stat_aux, par ,
			       ADN_none ) ;
	     }
	
	   free(par) ;
	
	     /* 2: if the input dataset has statistical parameters */

	 } else if( ISFUNC(dset)                        &&   /* dset has stat */
		    FUNC_IS_STAT(dset->func_type)       &&   /* params        */
		    jv == FUNC_ival_thr[dset->func_type]  ){ /* thr sub-brick */
	
	   int npar , lv ;
	   float * par , * sax ;
	   kv  = dset->func_type ;
	   npar = MAX_STAT_AUX ;
	   par  = (float *) malloc( sizeof(float) * (npar+2) ) ;
	   sax  = dset->stat_aux ;
	   for( lv=0 ; lv < npar ; lv++ )
	     par[lv] = (sax != NULL) ? sax[lv] : 0.0 ;
	

	   if (ivout == 1)
	     {
	       for( lv=0 ; lv < npar+2 ; lv++ )
		 printf ("par[%d] = %f \n", lv, par[lv]);
	       EDIT_dset_items(new_dset ,
			       ADN_func_type     , kv,		
			       ADN_stat_aux, par ,
			       ADN_none ) ;
	     }
	
	   free(par) ;
	 }
	
	 /** print a message? **/
	
	 if( B2F_verb ) printf("-verb: copied %s[%d] into %s[%d]\n" ,
			       DSET_FILECODE(dset) , jv ,
			       DSET_FILECODE(new_dset) , ivout ) ;
	 ivout++ ;
      }

      /** loop over all bricks in input dataset and
	unload them if they aren't going into the output
	(not required, but is done to economize on memory) **/

      if( nv < DSET_NVALS(dset) ){
	
	for( kv=0 ; kv < DSET_NVALS(dset) ; kv++ ){  /* all input sub-bricks */
	  for( iv=0 ; iv < nv ; iv++ ){             /* all output sub-bricks */
	    jv = SUBV(ids,iv) ;
	    if( jv == kv ) break ;                 /* input matches output */
	  }
	  if( iv == nv ){
	    mri_free( DSET_BRICK(dset,kv) ) ;
#if 0
	    if( B2F_verb ) printf("-verb: unloaded unused %s[%d]\n" ,
				  DSET_FILECODE(dset) , kv ) ;
#endif
	  }
	}
      }

   } /* end of loop over input datasets */


   if( B2F_verb ) fprintf(stderr,"-verb: loading statistics\n") ;
   THD_load_statistics( new_dset ) ;
   THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
   if( B2F_verb ) fprintf(stderr,"-verb: wrote output: %s\n",DSET_BRIKNAME(new_dset)) ;


   exit(0) ;
}
