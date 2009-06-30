/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*---------------------------------------------------------------------------
  This program catenates multiple 3D+time datasets to create one
  super-dataset.  Adapted from 3dbucket.c -- RWCox 21 Sep 1998.
----------------------------------------------------------------------------*/

#ifndef myXtFree
#   define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

/*-------------------------- global data --------------------------*/

static THD_3dim_dataset_array *TCAT_dsar  = NULL ;  /* input datasets */
static XtPointer_array        *TCAT_subv  = NULL ;  /* sub-brick selectors */
static int                     TCAT_nvox  = -1 ;    /* # voxels */
static int                     TCAT_dry   = 0 ;     /* dry run? */
static int                     TCAT_verb  = 0 ;     /* verbose? */
static int                     TCAT_type  = -1 ;    /* dataset type */
static int                     TCAT_glue  = 0 ;     /* glueing run? */
static int                     TCAT_rlt   = 0 ;     /* remove linear trend? */

static int                     TCAT_rqt   = 0 ;     /* 15 Nov 1999 */
static int                     TCAT_rct   = 0 ;     /* 15 Nov 1999 */

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

void TCAT_read_opts( int argc , char *argv[] )
{
   int nopt = 1 , ii ;
   char dname[THD_MAX_NAME] ;
   char subv[THD_MAX_NAME] ;
   char *cpt ;
   THD_3dim_dataset *dset , *fset ;
   int *svar ;
   char *str;
   int ok, ilen, nlen , max_nsub=0 ;

   INIT_3DARR(TCAT_dsar) ;  /* array of datasets */
   INIT_XTARR(TCAT_subv) ;  /* array of sub-brick selector arrays */

   while( nopt < argc ){

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ||
          strncmp(argv[nopt],"-output",6) == 0   ){
        if(TCAT_glue)
          ERROR_exit("-prefix and -glueto options are not compatible");
         nopt++ ;
         if( nopt >= argc )
           ERROR_exit("need argument after -prefix!") ;
         MCW_strncpy( TCAT_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

      /**** -session directory ****/

      if( strncmp(argv[nopt],"-session",6) == 0 ){
        if(TCAT_glue)
          ERROR_exit("-session and -glueto options are not compatible");
        nopt++ ;
        if( nopt >= argc )
          ERROR_exit("need argument after -session!") ;
        MCW_strncpy( TCAT_session , argv[nopt++] , THD_MAX_NAME ) ;
        continue ;
      }

      /**** -dry ****/

      if( strncmp(argv[nopt],"-dry",3) == 0 ){
        TCAT_dry = 1 ; TCAT_verb++ ;
        nopt++ ; continue ;
      }

      /**** -verb ****/

      if( strncmp(argv[nopt],"-verb",5) == 0 ){
         TCAT_verb++ ;
         nopt++ ; continue ;
      }

      /**** -rlt ****/

      if( strcmp(argv[nopt],"-rlt") == 0 ){
         TCAT_rlt = 1 ;
         nopt++ ; continue ;
      }

      /**** -rlt+ [16 Sep 1999] ****/

      if( strcmp(argv[nopt],"-rlt+") == 0 ){  /* 16 Sep 1999 */
         TCAT_rlt = 2 ;
         nopt++ ; continue ;
      }

      /**** -rlt++ [16 Sep 1999] ****/

      if( strcmp(argv[nopt],"-rlt++") == 0 ){  /* 16 Sep 1999 */
         TCAT_rlt = 3 ;
         nopt++ ; continue ;
      }

      /**** -rqt [15 Nov 1999] ****/

      if( strcmp(argv[nopt],"-rqt") == 0 ){
         TCAT_rqt = 1 ;
         nopt++ ; continue ;
      }

      /**** -rct [15 Nov 1999] ****/

      if( strcmp(argv[nopt],"-rct") == 0 ){
         TCAT_rct = 1 ;
         nopt++ ; continue ;
      }

      /**** -glueto fname ****/

      if( strncmp(argv[nopt],"-glueto",5) == 0 ){
        if( strncmp(TCAT_output_prefix, "tcat", 5) != 0 )
          ERROR_exit("-prefix and -glueto options are not compatible");
        if( strncmp(TCAT_session, "./", 5) != 0 )
          ERROR_exit("-session and -glueto options are not compatible");
        TCAT_glue = 1 ;
        nopt++ ;
        if( nopt >= argc )
          ERROR_exit("need argument after -glueto!") ;

	 /*----- Verify that file name ends in View Type -----*/
	 ok = 1;
	 nlen = strlen(argv[nopt]);
	 if (nlen <= 5) ok = 0;

#define BACKASS   /* 03 Oct 2002 -- RWCox */

	 if (ok)
	   {
#ifndef BACKASS
	     for (ilen = 0;  ilen < nlen;  ilen++)     /* BDW: scan forward */
#else
             for( ilen=nlen-3 ; ilen >= 0 ; ilen-- )   /* RWC: scan backward */
#endif
	       {
		 str = argv[nopt] + ilen;
		 if (str[0] == '+') break;
	       }
#ifndef BACKASS
	     if (ilen == nlen)  ok = 0;
#else
             if (ilen <= 0   )  ok = 0;
#endif
	   }

	 if (ok)
	   {
	     str = argv[nopt] + ilen + 1;

	     for (ii=FIRST_VIEW_TYPE ; ii <= LAST_VIEW_TYPE ; ii++)
	       if (! strncmp(str,VIEW_codestr[ii],4)) break ;
	
	     if( ii > LAST_VIEW_TYPE )  ok = 0;
	   }

	 if (! ok)
	     ERROR_exit(
	       "File name must end in +orig, +acpc, or +tlrc after -glueto");

	 /*----- Remove View Type from string to make output prefix -----*/
         MCW_strncpy( TCAT_output_prefix , argv[nopt] , ilen+1) ;

	 /*----- Note: no "continue" statement here.  File name will now
	   be processed as an input dataset -----*/
      }

      if( argv[nopt][0] == '-' ) ERROR_exit("Unknown option: %s",argv[nopt]) ;

      /**** read dataset ****/

      cpt = strstr(argv[nopt],"[") ;  /* look for the sub-brick selector */

      if( cpt == NULL ){              /* no selector */
         strcpy(dname,argv[nopt]) ;
         subv[0] = '\0' ;
      } else if( cpt == argv[nopt] ){ /* can't be at start!*/
         ERROR_exit("Illegal dataset specifier: %s",argv[nopt]) ;
      } else {                        /* found selector */
         ii = cpt - argv[nopt] ;
         memcpy(dname,argv[nopt],ii) ; dname[ii] = '\0' ;
         strcpy(subv,cpt) ;
      }
      nopt++ ;

      dset = THD_open_one_dataset( dname ) ;
      if( dset == NULL ) ERROR_exit("Can't open dataset %s",dname) ;
      THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_MALLOC ) ;

      if( TCAT_type < 0 ) TCAT_type = dset->type ;

      /* check if voxel counts match */

      ii = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz ;
      if( TCAT_nvox < 0 ){
        TCAT_nvox = ii ; fset = dset ;
      } else if( ii != TCAT_nvox ){
        ERROR_exit("Dataset %s differs in size from first one!",dname);
      } else if( !EQUIV_GRIDS(dset,fset) ){
        WARNING_message("Dataset %s grid differs from first one!",dname);
      }
      ADDTO_3DARR(TCAT_dsar,dset) ;  /* list of datasets */

      /* process the sub-brick selector string,
         returning an array of int with
           svar[0]   = # of sub-bricks,
           svar[j+1] = index of sub-brick #j for j=0..svar[0] */

      svar = TCAT_get_subv( DSET_NVALS(dset) , subv ) ;
      if( svar == NULL || svar[0] <= 0 )
        ERROR_exit("Can't decipher index codes from %s%s",dname,subv) ;
      ADDTO_XTARR(TCAT_subv,svar) ;  /* list of sub-brick selectors */

      max_nsub = MAX( max_nsub , svar[0] ) ;

      if( TCAT_rlt == 3 && svar[0] < 3 )  /* 16 Sep 1999 */
        WARNING_message(
                 "-rlt++ option won't work properly with"
                 " less than 3 sub-bricks per input dataset!") ;

   }  /* end of loop over command line arguments */

   /*--- final sanity checks ---*/

   if( max_nsub < 3 && TCAT_rlt ){
     WARNING_message("can't apply -rlt option -- "
                     "Not enough points per input dataset." ) ;
     TCAT_rlt = 0 ;
   }

   if( TCAT_rlt && TCAT_dry ){
     WARNING_message("-rlt option does nothing with -dry!") ;
     TCAT_rlt = 0 ;
   }

   return ;
}

/*-----------------------------------------------------------------------
  Decode a string like [1..3,5..9(2)] into an array of integers.
-------------------------------------------------------------------------*/

int * TCAT_get_subv( int nvals , char *str )
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

   /* do we have a count string in there ? */
   if (strstr(str,"count ")) {
      return(get_count_intlist ( str, &ii));
   }


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
    "                       Using -verb twice results in quite lengthy output.\n"
    "     -rlt          = Remove linear trends in each voxel time series loaded\n"
    "                       from each input dataset, SEPARATELY.  That is, the\n"
    "                       data from each dataset is detrended separately.\n"
    "                       At least 3 sub-bricks from a dataset must be input\n"
    "                       for this option to apply.\n"
    "             Notes: (1) -rlt removes the least squares fit of 'a+b*t'\n"
    "                          to each voxel time series; this means that\n"
    "                          the mean is removed as well as the trend.\n"
    "                          This effect makes it impractical to compute\n"
    "                          the %% Change using AFNI's internal FIM.\n"
    "                    (2) To have the mean of each dataset time series added\n"
    "                          back in, use this option in the form '-rlt+'.\n"
    "                          In this case, only the slope 'b*t' is removed.\n"
    "                    (3) To have the overall mean of all dataset time\n"
    "                          series added back in, use this option in the\n"
    "                          form '-rlt++'.  In this case, 'a+b*t' is removed\n"
    "                          from each input dataset separately, and the\n"
    "                          mean of all input datasets is added back in at\n"
    "                          the end.  (This option will work properly only\n"
    "                          if all input datasets use at least 3 sub-bricks!)\n"
    "                    (4) -rlt can be used on datasets that contain shorts\n"
    "                          or floats, but not on complex- or byte-valued\n"
    "                          datasets.\n"
    "\n"
    "Command line arguments after the above are taken as input datasets.\n"
    "A dataset is specified using one of these forms:\n"
    "   'prefix+view', 'prefix+view.HEAD', or 'prefix+view.BRIK'.\n"
    "\n"
    "SUB-BRICK SELECTION:\n"
    "You can also add a sub-brick selection list after the end of the\n"
    "dataset name.  This allows only a subset of the sub-bricks to be\n"
    "included into the output (by default, all of the input dataset\n"
    "is copied into the output).  A sub-brick selection list looks like\n"
    "one of the following forms:\n"
    "  fred+orig[5]                     ==> use only sub-brick #5\n"
    "  fred+orig[5,9,17]                ==> use #5, #9, and #17\n"
    "  fred+orig[5..8]     or [5-8]     ==> use #5, #6, #7, and #8\n"
    "  fred+orig[5..13(2)] or [5-13(2)] ==> use #5, #7, #9, #11, and #13\n"
    "Sub-brick indexes start at 0.  You can use the character '$'\n"
    "to indicate the last sub-brick in a dataset; for example, you\n"
    "can select every third sub-brick by using the selection list\n"
    "  fred+orig[0..$(3)]\n"
    "\n"
    "You can also use a syntax based on the usage of the program count.\n"
    "This would be most useful when randomizing (shuffling) the order of\n"
    "the sub-bricks. Example:\n"
    "  fred+orig[count -seed 2 5 11 s] is equivalent to something like:\n"
    "  fred+orig[ 6, 5, 11, 10, 9, 8, 7] \n"
    "You could also do: fred+orig[`count -seed 2 -digits 1 -suffix ',' 5 11 s`]\n"
    "but if you have lots of numbers, the command line would get too\n"
    "long for the shell to process it properly. Omit the seed option if\n"
    "you want the code to generate a seed automatically.\n"
    "You cannot mix and match count syntax with other selection gimmicks.\n"
    "\n"
    "NOTES:\n"
    "* The TR and other time-axis properties are taken from the\n"
    "  first input dataset that is itself 3D+time.  If no input\n"
    "  datasets contain such information, then TR is set to 1.0.\n"
    "  This can be altered later using the 3drefit program.\n"
    "\n"
    "* The sub-bricks are output in the order specified, which may\n"
    "  not be the order in the original datasets.  For example, using\n"
    "     fred+orig[0..$(2),1..$(2)]\n"
    "  will cause the sub-bricks in fred+orig to be output into the\n"
    "  new dataset in an interleaved fashion.  Using\n"
    "     fred+orig[$..0]\n"
    "  will reverse the order of the sub-bricks in the output.\n"
    "  If the -rlt option is used, the sub-bricks selected from each\n"
    "  input dataset will be re-ordered into the output dataset, and\n"
    "  then this sequence will be detrended.\n"
    "\n"
    "* You can use the '3dinfo' program to see how many sub-bricks\n"
    "  a 3D+time or a bucket dataset contains.\n"
    "\n"
    "* The '$', '(', ')', '[', and ']' characters are special to\n"
    "  the shell, so you will have to escape them.  This is most easily\n"
    "  done by putting the entire dataset plus selection list inside\n"
    "  single quotes, as in 'fred+orig[5..7,9]'.\n"
    "\n"
    "* You may wish/need to use the 3drefit program on the output\n"
    "  dataset to modify some of the .HEAD file parameters.\n"
   ) ;

   PRINT_COMPILE_DATE ; exit(0) ;
}

/*-------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int ninp , ids , nv , iv,jv,kv , ivout , new_nvals , ivbot,ivtop ;
   THD_3dim_dataset *new_dset=NULL , * dset ;
   char buf[256] ;
   float *rlt0=NULL , *rlt1=NULL ;
   float *rltsum=NULL ;             /* 16 Sep 1999 */
   int   nrltsum ;
   float dTR , nTR ;

   /*** read input options ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) TCAT_Syntax() ;

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dTcat main"); machdep() ; PRINT_VERSION("3dTcat") ;

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   AFNI_logger("3dTcat",argc,argv) ;

   TCAT_read_opts( argc , argv ) ;

   /*** create new dataset (empty) ***/

   ninp = TCAT_dsar->num ;
   if( ninp < 1 ) ERROR_exit("No input datasets?") ;

   new_nvals = 0 ;
   for( ids=0 ; ids < ninp ; ids++ ) new_nvals += NSUBV(ids) ;

   if( new_nvals < 2 )
     ERROR_exit("Can't create 3D+time dataset with only %d sub-bricks!",
                new_nvals) ;

   if( TCAT_verb ) printf("-verb: output will have %d sub-bricks\n",new_nvals) ;

   /** find 1st dataset that is time dependent **/

   for( ids=0 ; ids < ninp ; ids++ ){
     dset = DSUB(ids) ;
     if( DSET_TIMESTEP(dset) > 0.0 ) break ;
   }
   if( ids == ninp ){ ids = 0 ; dset = DSUB(0) ; }

   new_dset = EDIT_empty_copy( dset ) ; /* make a copy of its header */

   /* 23 May 2005: check for axis consistency */

   for( iv=0 ; iv < ninp ; iv++ ){
     if( iv != ids && !EQUIV_DATAXES(new_dset->daxes,DSUB(iv)->daxes) )
       WARNING_message("%s grid mismatch with %s",
               DSET_BRIKNAME(dset) , DSET_BRIKNAME(DSUB(iv)) ) ;
   }

   tross_Make_History( "3dTcat" , argc,argv , new_dset ) ;

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
      WARNING_message("Set TR of output dataset to 1.0 s") ;
   }

   /* 10 Dec 2007: check if time steps are coherent */

   nTR = DSET_TIMESTEP(new_dset) ;
   for( ids=0 ; ids < ninp ; ids++ ){
     dset = DSUB(ids) ; dTR = DSET_TIMESTEP(dset) ;
     if( dTR > 0.0f && fabsf(dTR-nTR) > 0.001f )
       WARNING_message("TR=%g in dataset %s; differs from output TR=%g",
                       dTR , DSET_HEADNAME(dset) , nTR ) ;
   }

   /* can't re-write existing dataset, unless glueing is used */

   if (! TCAT_glue){
     if( THD_deathcon() && THD_is_file(DSET_HEADNAME(new_dset)) ){
       ERROR_exit("file %s already exists!", DSET_HEADNAME(new_dset) ) ;
     }
   } else {   /* if glueing is used, make the 'new'
                 dataset have the same idcode as the old one */

      new_dset->idcode = DSUB(0) -> idcode ;  /* copy the struct */
   }

   THD_force_malloc_type( new_dset->dblk , DATABLOCK_MEM_MALLOC ) ;

   /*** if needed, create space for detrending ***/

   if( TCAT_rlt ){
      rlt0   = (float *) malloc( sizeof(float) * TCAT_nvox ) ;
      rlt1   = (float *) malloc( sizeof(float) * TCAT_nvox ) ;
      if( rlt0 == NULL || rlt1 == NULL )
        ERROR_exit("can't malloc memory for detrending!") ;

      if( TCAT_rlt == 3 ){
         rltsum = (float *) malloc( sizeof(float) * TCAT_nvox ) ;
         if( rltsum == NULL )
           ERROR_exit("can't malloc memory for detrending!") ;

         for( iv=0 ; iv < TCAT_nvox ; iv++ ) rltsum[iv] = 0.0 ;
         nrltsum = 0 ;
      }
   }

   /*** loop over input datasets ***/

   if( ninp > 1 ) myXtFree( new_dset->keywords ) ;

   ivout = 0 ;
   for( ids=0 ; ids < ninp ; ids++ ){
      dset = DSUB(ids) ;
      nv   = NSUBV(ids) ;

      if( ! TCAT_dry ){
         if( TCAT_verb ) printf("-verb: loading %s\n",DSET_FILECODE(dset)) ;
         DSET_load(dset) ;  CHECK_LOAD_ERROR(dset) ;
      }

      /** loop over sub-bricks to output **/

      ivbot = ivout ;                       /* save this for later */
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
            EDIT_dset_items( new_dset, ADN_brick_label_one+ivout, buf, ADN_none );

#if 0
            sprintf(buf,"%s[%d]",DSET_FILECODE(dset),jv) ;
            EDIT_dset_items(
              new_dset, ADN_brick_keywords_replace_one+ivout, buf, ADN_none ) ;
#endif

            EDIT_dset_items(
              new_dset ,
                ADN_brick_fac_one            +ivout, DSET_BRICK_FACTOR(dset,jv),
#if 0
                ADN_brick_keywords_append_one+ivout, DSET_BRICK_KEYWORDS(dset,jv),
#endif
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
      ivtop = ivout ;  /* new_dset[ivbot..ivtop-1] are from the current dataset */

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

      /*** remove linear trend? ***/

      if( TCAT_rlt ){

         /* have enough data? */

         if( ivtop-ivbot < 3 ){
            if( TCAT_verb )
               printf("-verb: skipping -rlt for %s\n",DSET_FILECODE(dset)) ;

         } else {
            float c0,c1,c2 , det , a0,a1,a2 , qq ;
            int iv , ns , kk , err=0 ;

            if( TCAT_verb )
               printf("-verb: applying -rlt to data from %s\n",DSET_FILECODE(dset)) ;

            /* compute weighting coefficients */

            ns  = ivtop - ivbot ;                        /* number of sub-bricks */
            c0  = ns ;                                   /* sum[ 1 ]   */
            c1  = 0.5 * ns * (ns-1) ;                    /* sum[ qq ]   */
            c2  = 0.16666667 * ns * (ns-1) * (2*ns-1) ;  /* sum[ qq*qq ] */
            det = c0*c2 - c1*c1 ;
            a0  =  c2 / det ;   /*             -1  */
            a1  = -c1 / det ;   /*   [ c0  c1 ]    */
            a2  =  c0 / det ;   /*   [ c1  c2 ]    */

            /* set voxel sums to 0 */

            for( iv=0 ; iv < TCAT_nvox ; iv++ ) rlt0[iv] = rlt1[iv] = 0.0 ;

            /* compute voxel sums */

            for( kk=ivbot ; kk < ivtop ; kk++ ){
               qq = kk - ivbot ;
               switch( DSET_BRICK_TYPE(new_dset,kk) ){
                  default:
                     err = 1 ;
                     WARNING_message(
                             "Warning: -rlt can't use datum type %s from %s",
                             MRI_TYPE_name[DSET_BRICK_TYPE(new_dset,kk)] ,
                             DSET_FILECODE(dset) ) ;
                  break ;

                  case MRI_short:{
                     short * bar = (short *) DSET_ARRAY(new_dset,kk) ;
                     float fac = DSET_BRICK_FACTOR(new_dset,kk) ;

                     if( fac == 0.0 ) fac = 1.0 ;
                     for( iv=0 ; iv < TCAT_nvox ; iv++ ){
                        rlt0[iv] += (fac * bar[iv]) ;        /* sum of voxel    */
                        rlt1[iv] += (fac * bar[iv]) * qq ;   /* sum of voxel*qq */
                     }
                  }
                  break ;

                  case MRI_float:{
                     float * bar = (float *) DSET_ARRAY(new_dset,kk) ;
                     float fac = DSET_BRICK_FACTOR(new_dset,kk) ;

                     if( fac == 0.0 ) fac = 1.0 ;
                     for( iv=0 ; iv < TCAT_nvox ; iv++ ){
                        rlt0[iv] += (fac * bar[iv]) ;
                        rlt1[iv] += (fac * bar[iv]) * qq ;
                     }
                  }
                  break ;
               }
               if( err ) break ;
            } /* end of loop over sub-bricks */

            /* only do the detrending if no errors happened */

            if( !err ){
               float qmid = 0.0 ;                 /* 16 Sep 1999 */

               for( iv=0 ; iv < TCAT_nvox ; iv++ ){     /* transform voxel sums */
                 c0 = a0 * rlt0[iv] + a1 * rlt1[iv] ;
                 c1 = a1 * rlt0[iv] + a2 * rlt1[iv] ;
                 rlt0[iv] = c0 ; rlt1[iv] = c1 ;
               }

               if( TCAT_rlt == 2 ){               /* 16 Sep 1999 */
                  qmid = 0.5 * (ns-1) ;
                  for( iv=0 ; iv < TCAT_nvox ; iv++ ) rlt0[iv] = 0.0 ;
               } else if( TCAT_rlt == 3 ){
                  nrltsum += ns ;
                  for( iv=0 ; iv < TCAT_nvox ; iv++ )
                     rltsum[iv] += (rlt0[iv] + (0.5*ns)*rlt1[iv])*ns ;
               }

               for( kk=ivbot ; kk < ivtop ; kk++ ){     /* detrend */
                  qq = kk - ivbot ;
                  switch( DSET_BRICK_TYPE(new_dset,kk) ){

#undef  ROUND
#define ROUND(qq) ((short)rint((qq)+0.00001))

                     case MRI_short:{
                        short * bar = (short *) DSET_ARRAY(new_dset,kk) ;
                        float fac = DSET_BRICK_FACTOR(new_dset,kk) , val,finv ;

                        if( fac == 0.0 ) fac = 1.0 ;
                        finv = 1.0 / fac ;
                        for( iv=0 ; iv < TCAT_nvox ; iv++ ){
                           val = fac*bar[iv] - rlt0[iv] - rlt1[iv]*(qq-qmid) ;
                           bar[iv] = ROUND(finv*val) ;
                        }
                     }
                     break ;

                     case MRI_float:{
                        float * bar = (float *) DSET_ARRAY(new_dset,kk) ;
                        float fac = DSET_BRICK_FACTOR(new_dset,kk) , val,finv ;

                        if( fac == 0.0 ) fac = 1.0 ;
                        finv = 1.0 / fac ;
                        for( iv=0 ; iv < TCAT_nvox ; iv++ ){
                           val = fac*bar[iv] - rlt0[iv] - rlt1[iv]*(qq-qmid) ;
                           bar[iv] = (finv*val) ;
                        }
                     }
                     break ;
                  }
               }
            }
         }
      } /* end of -rlt */

   } /* end of loop over input datasets */

   /* 16 Sep 1999: add overall average back in */

   if( TCAT_rlt == 3 && rltsum != NULL && nrltsum > 0 ){
      float scl = 1.0/nrltsum ; int kk ;

      for( iv=0 ; iv < TCAT_nvox ; iv++ ) rltsum[iv] *= scl ;

      for( kk=0 ; kk < new_nvals ; kk++ ){
         switch( DSET_BRICK_TYPE(new_dset,kk) ){
            case MRI_short:{
               short * bar = (short *) DSET_ARRAY(new_dset,kk) ;
               float fac = DSET_BRICK_FACTOR(new_dset,kk) , val,finv ;

               if( fac == 0.0 ) fac = 1.0 ;
               finv = 1.0 / fac ;
               for( iv=0 ; iv < TCAT_nvox ; iv++ ){
                  val = fac*bar[iv] + rltsum[iv] ; bar[iv] = ROUND(finv*val) ;
               }
            }
            break ;

            case MRI_float:{
               float * bar = (float *) DSET_ARRAY(new_dset,kk) ;
               float fac = DSET_BRICK_FACTOR(new_dset,kk) , val,finv ;

               if( fac == 0.0 ) fac = 1.0 ;
               finv = 1.0 / fac ;
               for( iv=0 ; iv < TCAT_nvox ; iv++ ){
                  val = fac*bar[iv] + rltsum[iv] ; bar[iv] = (finv*val) ;
               }
            }
            break ;
         }
      }
   }

   if( TCAT_rlt ){ free(rlt0); free(rlt1); if(rltsum!=NULL)free(rltsum); }

   if( ! TCAT_dry ){
      if( TCAT_verb ) INFO_message("-verb: computing sub-brick statistics") ;
      THD_load_statistics( new_dset ) ;
      if( TCAT_glue ) putenv("AFNI_DECONFLICT=OVERWRITE") ;
      THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
      if( TCAT_verb ) INFO_message("-verb: Wrote output to %s",
                              DSET_BRIKNAME(new_dset) ) ;
   }

   exit(0) ;
}
