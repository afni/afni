#include "mrilib.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------
  This program catenates multiple 3D+time datasets to create one
  super-dataset.  Adapted from 3dbucket.c -- RWCox 21 Sep 1998.
----------------------------------------------------------------------------*/

#ifndef myXtFree
#   define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

/*-------------------------- global data --------------------------*/

static THD_3dim_dataset_array * TCAT_dsar  = NULL ;  /* input datasets */
static XtPointer_array        * TCAT_subv  = NULL ;  /* sub-brick selectors */
static int                      TCAT_nvox  = -1 ;    /* # voxels */
static int                      TCAT_dry   = 0 ;     /* dry run? */
static int                      TCAT_verb  = 0 ;     /* verbose? */
static int                      TCAT_type  = -1 ;    /* dataset type */
static int                      TCAT_glue  = 0 ;     /* glueing run? */

static char TCAT_output_prefix[THD_MAX_PREFIX] = "tcat" ;
static char TCAT_session[THD_MAX_NAME]         = "./"   ;

/* macros to get
   DSUB  = a particular input dataset
   NSUBV = the number of sub-bricks selected from a dataset
   SUBV  = a particular sub-brick selected from a dataset   */

#define DSUB(id)    DSET_IN_3DARR(TCAT_dsar,(id))
#define NSUBV(id)   ( ((int *)TCAT_subv->ar[(id)])[0]      )
#define SUBV(id,jj) ( ((int *)TCAT_subv->ar[(id)])[(jj)+1] )

/*--------------------------- prototypes ---------------------------*/

void TCAT_read_opts( int , char ** ) ;
void TCAT_Syntax(void) ;
int * TCAT_get_subv( int , char * ) ;

/*--------------------------------------------------------------------
   read the arguments, load the global variables
----------------------------------------------------------------------*/

void TCAT_read_opts( int argc , char * argv[] )
{
   int nopt = 1 , ii ;
   char dname[THD_MAX_NAME] ;
   char subv[THD_MAX_NAME] ;
   char * cpt ;
   THD_3dim_dataset * dset ;
   int * svar ;
   char * str;
   int ok, ilen, nlen;

   INIT_3DARR(TCAT_dsar) ;  /* array of datasets */
   INIT_XTARR(TCAT_subv) ;  /* array of sub-brick selector arrays */

   while( nopt < argc ){

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ||
          strncmp(argv[nopt],"-output",6) == 0   ){
	 if (TCAT_glue){
            fprintf(stderr,"*** -prefix and -glueto options are not compatible\n");
	    exit(1) ;
	 }
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"*** need argument after -prefix!\n") ; exit(1) ;
         }
         MCW_strncpy( TCAT_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

      /**** -session directory ****/

      if( strncmp(argv[nopt],"-session",6) == 0 ){
	 if (TCAT_glue){
            fprintf(stderr,
		    "*** -session and -glueto options are not compatible\n");
	    exit(1) ;
	 }
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"*** need argument after -session!\n") ; exit(1) ;
         }
         MCW_strncpy( TCAT_session , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }

      /**** -dry ****/

      if( strncmp(argv[nopt],"-dry",3) == 0 ){
         TCAT_dry = TCAT_verb = 1 ;
         nopt++ ; continue ;
      }

      /**** -verb ****/

      if( strncmp(argv[nopt],"-verb",2) == 0 ){
         TCAT_verb++ ;
         nopt++ ; continue ;
      }

      /**** -glueto fname ****/

      if( strncmp(argv[nopt],"-glueto",5) == 0 ){
	 if( strncmp(TCAT_output_prefix, "tcat", 5) != 0 ){
            fprintf(stderr,"*** -prefix and -glueto options are not compatible\n");
	    exit(1) ;
	 }
	 if( strncmp(TCAT_session, "./", 5) != 0 ){
            fprintf(stderr,
		    "*** -session and -glueto options are not compatible\n");
	    exit(1) ;
	 }
	 TCAT_glue = 1 ;
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"*** need argument after -glueto!\n") ; exit(1) ;
         }

	 /*----- Verify that file name ends in View Type -----*/
	 ok = 1;
	 nlen = strlen(argv[nopt]);
	 if (nlen <= 5) ok = 0;

	 if (ok)
	   {
	     for (ilen = 0;  ilen < nlen;  ilen++)
	       {
		 str = argv[nopt] + ilen;
		 if (str[0] == '+') break;
	       }
	     if (ilen == nlen)  ok = 0;
	   }

	 if (ok)
	   {
	     str = argv[nopt] + ilen + 1;

	     for (ii=FIRST_VIEW_TYPE ; ii <= LAST_VIEW_TYPE ; ii++)
	       if (! strncmp(str,VIEW_codestr[ii],4)) break ;
	
	     if( ii > LAST_VIEW_TYPE )  ok = 0;
	   }

	 if (! ok)
	   {
	     fprintf(stderr,
	       "*** File name must end in +orig, +acpc, or +tlrc after -glueto\n");
	     exit(1);
	   }

	 /*----- Remove View Type from string to make output prefix -----*/
         MCW_strncpy( TCAT_output_prefix , argv[nopt] , ilen+1) ;

	 /*----- Note: no "continue" statement here.  File name will now
	   be processed as an input dataset -----*/
      }

      if( argv[nopt][0] == '-' ){
         fprintf(stderr,"*** Unknown option: %s\n",argv[nopt]) ; exit(1) ;
      }

      /**** read dataset ****/

      cpt = strstr(argv[nopt],"[") ;  /* look for the sub-brick selector */

      if( cpt == NULL ){              /* no selector */
         strcpy(dname,argv[nopt]) ;
         subv[0] = '\0' ;
      } else if( cpt == argv[nopt] ){ /* can't be at start!*/
         fprintf(stderr,"*** Illegal dataset specifier: %s\n",argv[nopt]) ;
         exit(1) ;
      } else {                        /* found selector */
         ii = cpt - argv[nopt] ;
         memcpy(dname,argv[nopt],ii) ; dname[ii] = '\0' ;
         strcpy(subv,cpt) ;
      }
      nopt++ ;

      dset = THD_open_one_dataset( dname ) ;
      if( dset == NULL ){
         fprintf(stderr,"*** Can't open dataset %s\n",dname) ; exit(1) ;
      }
      THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_MALLOC ) ;

      if( TCAT_type < 0 ) TCAT_type = dset->type ;

      /* check if voxel counts match */

      ii = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz ;
      if( TCAT_nvox < 0 ){
         TCAT_nvox = ii ;
      } else if( ii != TCAT_nvox ){
         fprintf(stderr,"*** Dataset %s differs in size from others\n",dname);
         exit(1) ;
      }
      ADDTO_3DARR(TCAT_dsar,dset) ;  /* list of datasets */

      /* process the sub-brick selector string,
         returning an array of int with
           svar[0]   = # of sub-bricks,
           svar[j+1] = index of sub-brick #j for j=0..svar[0] */

      svar = TCAT_get_subv( DSET_NVALS(dset) , subv ) ;
      if( svar == NULL || svar[0] <= 0 ){
         fprintf(stderr,"*** Can't decipher index codes from %s%s\n",dname,subv) ;
         exit(1) ;
      }
      ADDTO_XTARR(TCAT_subv,svar) ;  /* list of sub-brick selectors */

   }  /* end of loop over command line arguments */

   return ;
}

/*-----------------------------------------------------------------------
  Decode a string like [1..3,5..9(2)] into an array of integers.
-------------------------------------------------------------------------*/

int * TCAT_get_subv( int nvals , char * str )
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

/*-------------------------------------------------------------------------*/

void TCAT_Syntax(void)
{
   printf(
    "Concatenate sub-bricks from input datasets into one big 3D+time dataset.\n"
    "Usage: 3dTcat options\n"
    "where the options are:\n"
   ) ;

   printf(
    "     -prefix pname = Use 'pname' for the output dataset prefix name.\n"
    " OR  -output pname     [default='tcat']\n"
    "\n"
    "     -session dir  = Use 'dir' for the output dataset session directory.\n"
    "                       [default='./'=current working directory]\n"
    "     -glueto fname = Append bricks to the end of the 'fname' dataset.\n"
    "                       This command is an alternative to the -prefix \n"
    "                       and -session commands.                        \n"
    "     -dry          = Execute a 'dry run'; that is, only print out\n"
    "                       what would be done.  This is useful when\n"
    "                       combining sub-bricks from multiple inputs.\n"
    "     -verb         = Print out some verbose output as the program\n"
    "                       proceeds (-dry implies -verb).\n"
    "\n"
    "Command line arguments after the above are taken as input datasets.\n"
    "A dataset is specified using one of these forms:\n"
    "   'prefix+view', 'prefix+view.HEAD', or 'prefix+view.BRIK'.\n"
    "You can also add a sub-brick selection list after the end of the\n"
    "dataset name.  This allows only a subset of the sub-bricks to be\n"
    "included into the output (by default, all of the input dataset\n"
    "is copied into the output).  A sub-brick selection list looks like\n"
    "one of the following forms:\n"
    "  fred+orig[5]                     ==> use only sub-brick #5\n"
    "  fred+orig[5,9,17]                ==> use #5, #9, and #12\n"
    "  fred+orig[5..8]     or [5-8]     ==> use #5, #6, #7, and #8\n"
    "  fred+orig[5..13(2)] or [5-13(2)] ==> use #5, #7, #9, #11, and #13\n"
    "Sub-brick indexes start at 0.  You can use the character '$'\n"
    "to indicate the last sub-brick in a dataset; for example, you\n"
    "can select every third sub-brick by using the selection list\n"
    "  fred+orig[0..$(3)]\n"
    "\n"
    "N.B.: The TR and other time-axis properties are taken from the\n"
    " first input dataset that is itself 3D+time.  If no input\n"
    " datasets contain such information, then TR is set to 1.0.\n"
    " This can be altered using the 3drefit program.\n"
    "\n"
    "N.B.: The sub-bricks are output in the order specified, which may\n"
    " not be the order in the original datasets.  For example, using\n"
    "    fred+orig[0..$(2),1..$(2)]\n"
    " will cause the sub-bricks in fred+orig to be output into the\n"
    " new dataset in an interleaved fashion.  Using\n"
    "    fred+orig[$..0]\n"
    " will reverse the order of the sub-bricks in the output.\n"
    "\n"
    "N.B.: You can use the '3dinfo' program to see how many sub-bricks\n"
    " a 3D+time or a bucket dataset contains.\n"
    "\n"
    "N.B.: The '$', '(', ')', '[', and ']' characters are special to\n"
    " the shell, so you will have to escape them.  This is most easily\n"
    " done by putting the entire dataset plus selection list inside\n"
    " single quotes, as in 'fred+orig[5..7,9]'.\n"
    "\n"
    "N.B.: You may wish to use the 3drefit program on the output dataset\n"
    " to modify some of the .HEAD file parameters.\n"
   ) ;

   exit(0) ;
}

/*-------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int ninp , ids , nv , iv,jv,kv , ivout , new_nvals ;
   THD_3dim_dataset * new_dset=NULL , * dset ;
   char buf[256] ;

   /*** read input options ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) TCAT_Syntax() ;

   TCAT_read_opts( argc , argv ) ;

   /*** create new dataset (empty) ***/
   ninp = TCAT_dsar->num ;
   if( ninp < 1 ){
      fprintf(stderr,"*** No input datasets?\n") ; exit(1) ;
   }

   new_nvals = 0 ;
   for( ids=0 ; ids < ninp ; ids++ ) new_nvals += NSUBV(ids) ;

   if( new_nvals < 2 ){
      fprintf(stderr,
              "*** Can't create 3D+time dataset with only %d sub-bricks!\n",
              new_nvals) ;
      exit(1) ;
   }

   if( TCAT_verb ) printf("-verb: output will have %d sub-bricks\n",new_nvals) ;

   /** find 1st dataset that is time dependent **/

   for( ids=0 ; ids < ninp ; ids++ ){
      dset = DSUB(ids) ;
      if( DSET_TIMESTEP(dset) > 0.0 ) break ;
   }
   if( ids == ninp ) dset = DSUB(0) ;

   new_dset = EDIT_empty_copy( dset ) ; /* make a copy of its header */

   /* modify its header */

   EDIT_dset_items( new_dset ,
                      ADN_prefix        , TCAT_output_prefix ,
                      ADN_directory_name, TCAT_session ,
                      ADN_type          , TCAT_type ,
                      ADN_func_type     , ISANATTYPE(TCAT_type) ? ANAT_EPI_TYPE
                                                                : FUNC_FIM_TYPE ,
                      ADN_ntt           , new_nvals ,  /* both ntt and nvals */
                      ADN_nvals         , new_nvals ,  /* must be altered    */
                    ADN_none ) ;

   /* check if we have a valid time axis; if not, make one up */

   if( DSET_TIMESTEP(new_dset) <= 0.0 ){
      float TR = 1.0 ;
      float torg = 0.0 , tdur = 0.0 ;
      int tunits = UNITS_SEC_TYPE ;

#if 0
      for( ids=0 ; ids < ninp ; ids++ ){
         dset = DSUB(ids) ;
         if( DSET_TIMESTEP(dset) > 0.0 ){
            TR   = DSET_TIMESTEP(dset)   ; tunits = DSET_TIMEUNITS(dset) ;
            torg = DSET_TIMEORIGIN(dset) ; tdur   = DSET_TIMEDURATION(dset) ;
            break ;
         }
      }
#endif

      EDIT_dset_items( new_dset ,
                          ADN_tunits , tunits ,
                          ADN_ttdel  , TR ,
                          ADN_ttorg  , torg ,
                          ADN_ttdur  , tdur ,
                       ADN_none ) ;
   }

   /* can't re-write existing dataset, unless glueing is used */

   if (! TCAT_glue){
     if( THD_is_file(DSET_HEADNAME(new_dset)) ){
       fprintf(stderr,"*** Fatal error: file %s already exists!\n",
	       DSET_HEADNAME(new_dset) ) ;
       exit(1) ;
     }
   } else {   /* if glueing is used, make the 'new'
                 dataset have the same idcode as the old one */

      new_dset->idcode = DSUB(0) -> idcode ;  /* copy the struct */
   }

   THD_force_malloc_type( new_dset->dblk , DATABLOCK_MEM_MALLOC ) ;

   /*** loop over input datasets ***/

   if( ninp > 1 ) myXtFree( new_dset->keywords ) ;

   ivout = 0 ;
   for( ids=0 ; ids < ninp ; ids++ ){
      dset = DSUB(ids) ;
      nv   = NSUBV(ids) ;

      if( ! TCAT_dry ){
         if( TCAT_verb ) printf("-verb: loading %s\n",DSET_FILECODE(dset)) ;
         DSET_load(dset) ;
         if( ! DSET_LOADED(dset) ){
            fprintf(stderr,"*** Fatal error: can't load data from %s\n",
                    DSET_FILECODE(dset)) ;
            exit(1) ;
         }
      }

      /** loop over sub-bricks to output **/

      for( iv=0 ; iv < nv ; iv++ ){
         jv = SUBV(ids,iv) ;                /* which sub-brick to use */

         if( ! TCAT_dry ){
            EDIT_substitute_brick( new_dset , ivout ,
                                   DSET_BRICK_TYPE(dset,jv) , DSET_ARRAY(dset,jv) ) ;

	    /*----- If this sub-brick is from a bucket dataset,
                    preserve the label for this sub-brick -----*/

	    if( ISBUCKET(dset) )
	      sprintf (buf, "%s", DSET_BRICK_LABEL(dset,jv));
	    else
	      sprintf(buf,"%.12s[%d]",DSET_PREFIX(dset),jv) ;
            EDIT_dset_items( new_dset , ADN_brick_label_one+ivout, buf , ADN_none ) ;

            sprintf(buf,"%s[%d]",DSET_FILECODE(dset),jv) ;
            EDIT_dset_items(
              new_dset, ADN_brick_keywords_replace_one+ivout, buf, ADN_none ) ;

            EDIT_dset_items(
              new_dset ,
                ADN_brick_fac_one            +ivout, DSET_BRICK_FACTOR(dset,jv),
                ADN_brick_keywords_append_one+ivout, DSET_BRICK_KEYWORDS(dset,jv) ,
              ADN_none ) ;

            /** possibly write statistical parameters for this sub-brick **/

            kv = DSET_BRICK_STATCODE(dset,jv) ;

            if( FUNC_IS_STAT(kv) ){ /* input sub-brick has stat params */

               int npar = FUNC_need_stat_aux[kv] , lv ;
               float * par = (float *) malloc( sizeof(float) * (npar+2) ) ;
               float * sax = DSET_BRICK_STATAUX(dset,jv) ;
               par[0] = kv ;
               par[1] = npar ;
               for( lv=0 ; lv < npar ; lv++ )
                  par[lv+2] = (sax != NULL) ? sax[lv] : 0.0 ;

               EDIT_dset_items(new_dset ,
                                ADN_brick_stataux_one+ivout , par ,
                               ADN_none ) ;
               free(par) ;
#if 0
            /* 2: if the input dataset has statistical parameters */

            } else if( ISFUNC(dset)                        &&   /* dset has stat */
                       FUNC_IS_STAT(dset->func_type)       &&   /* params        */
                       jv == FUNC_ival_thr[dset->func_type]  ){ /* thr sub-brick */

               int npar , lv ;
               float * par , * sax ;
               kv  = dset->func_type ;
               npar = FUNC_need_stat_aux[kv] ;
               par  = (float *) malloc( sizeof(float) * (npar+2) ) ;
               sax  = dset->stat_aux ;
               par[0] = kv ;
               par[1] = npar ;
               for( lv=0 ; lv < npar ; lv++ )
                  par[lv+2] = (sax != NULL) ? sax[lv] : 0.0 ;

               EDIT_dset_items(new_dset ,
                                ADN_brick_stataux_one+ivout , par ,
                               ADN_none ) ;
               free(par) ;
#endif
            }

            /** print a message? **/

            if( TCAT_verb > 1 ) printf("-verb: copied %s[%d] into %s[%d]\n" ,
                                       DSET_FILECODE(dset) , jv ,
                                       DSET_FILECODE(new_dset) , ivout ) ;
         } else {
            printf("-dry: would copy %s[%d] into %s[%d]\n" ,
                    DSET_FILECODE(dset) , jv ,
                    DSET_FILECODE(new_dset) , ivout ) ;
         }

         ivout++ ;
      }

      /** loop over all bricks in input dataset and
          unload them if they aren't going into the output
          (not required, but is done to economize on memory) **/

      if( ! TCAT_dry && nv < DSET_NVALS(dset) ){

         for( kv=0 ; kv < DSET_NVALS(dset) ; kv++ ){  /* all input sub-bricks */
            for( iv=0 ; iv < nv ; iv++ ){             /* all output sub-bricks */
               jv = SUBV(ids,iv) ;
               if( jv == kv ) break ;                 /* input matches output */
            }
            if( iv == nv ){
               mri_free( DSET_BRICK(dset,kv) ) ;
#if 0
               if( TCAT_verb ) printf("-verb: unloaded unused %s[%d]\n" ,
                                      DSET_FILECODE(dset) , kv ) ;
#endif
            }
         }
      }

   } /* end of loop over input datasets */

   if( ! TCAT_dry ){
      if( TCAT_verb ) printf("-verb: loading statistics\n") ;
      THD_load_statistics( new_dset ) ;
      if( TCAT_verb ) printf("-verb: writing output\n") ;
      THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
   }

   exit(0) ;
}
