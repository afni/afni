/*---------------------------------------------------------------------------*/
/*
  This program performs agglomerative hierarchical clustering of voxels 
  for user specified parameter sub-bricks.

  Note: AFNI dataset and user interface code are adapted from program 3dTcat.c 


  File:    3dStatClust.c
  Author:  B. Douglas Ward
  Date:    08 October 1999

  Mod:     Replaced C "pow" function, significantly improving execution speed.
  Date:    11 October 1999


  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.

*/
/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dStatClust"                   /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_DATE "11 October 1999"           /* date of last program mod */

/*---------------------------------------------------------------------------*/
/*
  Include header files.
*/

#include "mrilib.h"
#include "matrix.h"


/*---------------------------------------------------------------------------*/

#ifndef myXtFree
#   define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

/*---------------------------------------------------------------------------*/

/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
if((ptr)==NULL) \
( printf ("Cannot allocate memory \n"),  exit(1) )
     

/*-------------------------- global data ------------------------------------*/

static THD_3dim_dataset_array * SC_dsar     = NULL; /* input datasets */
static XtPointer_array        * SC_subv     = NULL; /* sub-brick selectors */
static int                      SC_nvox     = -1;   /* # voxels */
static int                      SC_verb     = 0;    /* verbose? */
static int                      SC_type     = -1;   /* dataset type */
static float                    SC_thr      = -1.0; /* threshold */
static int                      SC_nclust   = 10;   /* max. output clusters */
static int                      SC_statdist = 0;    /* dist. calc. method */
static int                      SC_dimension= 0;    /* number of parameters */

static char SC_output_prefix[THD_MAX_PREFIX] = "SC" ;
static char SC_session[THD_MAX_NAME]         = "./"   ;

static char * commandline = NULL ;         /* command line for history notes */

/* macros to get
   DSUB  = a particular input dataset
   NSUBV = the number of sub-bricks selected from a dataset
   SUBV  = a particular sub-brick selected from a dataset   */

#define DSUB(id)    DSET_IN_3DARR(SC_dsar,(id))
#define NSUBV(id)   ( ((int *)SC_subv->ar[(id)])[0]      )
#define SUBV(id,jj) ( ((int *)SC_subv->ar[(id)])[(jj)+1] )

/*--------------------------- prototypes ------------------------------------*/

void SC_read_opts( int , char ** ) ;
void SC_Syntax(void) ;
int * SC_get_subv( int , char * ) ;
void SC_error (char * message);

/*---------------------------------------------------------------------------*/
/*
  Get the time series for one voxel from the AFNI 3d+time data set.
*/

void extract_ts_array 
(
  THD_3dim_dataset * dset_time,      /* input 3d+time dataset */
  int iv,                            /* get time series for this voxel */
  float * ts_array                   /* time series data for voxel #iv */
);


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void SC_error (char * message)
{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/
/*
  Include source code files.
*/

#include "matrix.c"
#include "StatClust.c"


/*---------------------------------------------------------------------------*/
/*
   Read the arguments, load the global variables

   This routine was adapted from: TCAT_read_opts
*/

void SC_read_opts( int argc , char * argv[] )
{
   int nopt = 1 , ii ;
   char dname[THD_MAX_NAME] ;
   char subv[THD_MAX_NAME] ;
   char * cpt ;
   THD_3dim_dataset * dset ;
   int * svar ;
   char * str;
   int ok, ilen, nlen , max_nsub=0 ;

   int is_thr = 0;          /* flag for threshold sub-brick */
   char message[80];        /* error message */


   INIT_3DARR(SC_dsar) ;  /* array of datasets */
   INIT_XTARR(SC_subv) ;  /* array of sub-brick selector arrays */

   while( nopt < argc ){

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ||
          strncmp(argv[nopt],"-output",6) == 0   ){
         nopt++ ;
         if( nopt >= argc ){
            SC_error (" need argument after -prefix!");
         }
         MCW_strncpy( SC_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

      /**** -session directory ****/

      if( strncmp(argv[nopt],"-session",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            SC_error (" need argument after -session!"); 
         }
         MCW_strncpy( SC_session , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }

      /**** -verb ****/

      if( strncmp(argv[nopt],"-verb",5) == 0 ){
         SC_verb++ ;
         nopt++ ; continue ;
      }

      /**** -dist_euc ****/

      if( strncmp(argv[nopt],"-dist_euc",9) == 0 ){
         SC_statdist = 0 ;
         nopt++ ; continue ;
      }

      /**** -dist_ind ****/

      if( strncmp(argv[nopt],"-dist_ind",9) == 0 ){
         SC_statdist = 1 ;
         nopt++ ; continue ;
      }

      /**** -dist_cor ****/

      if( strncmp(argv[nopt],"-dist_cor",9) == 0 ){
         SC_statdist = 2 ;
         nopt++ ; continue ;
      }

      /**** -nclust n ****/

      if( strncmp(argv[nopt],"-nclust",7) == 0 ){
	 int ival;
         nopt++ ;
         if( nopt >= argc ){
            SC_error (" need argument after -nclust!");
         }
	 sscanf (argv[nopt], "%d", &ival); 
	 if ((ival < 1) || (ival > 255)){
            SC_error (" Require 1 <= nclust <= 255 ");
         }
	 SC_nclust = ival;
	 nopt++;
	 continue;
      }


      /**** -thresh thr fname ****/

      if( strncmp(argv[nopt],"-thresh",7) == 0 ){
	 float fval;
         nopt++ ;
         if( nopt+1 >= argc ){
            SC_error (" need 2 arguments after -thresh!"); 
         }
	 sscanf (argv[nopt], "%f", &fval); 
	 if (fval < 0.0){
            SC_error (" Require thr >= 0.0 ");
         }
	 SC_thr = fval;
	 is_thr = 1;
	 nopt++;

	 /*----- Note: no "continue" statement here.  File name will now
	   be processed as an input dataset -----*/
      }


      if( argv[nopt][0] == '-' ){
         sprintf (message, " Unknown option: %s ", argv[nopt]);
	 SC_error (message);
      }

      /**** read dataset ****/

      if (SC_thr < 0.0){
         SC_error ("Must specify threshold data before parameter data");
      }


      cpt = strstr(argv[nopt],"[") ;  /* look for the sub-brick selector */

      if( cpt == NULL ){              /* no selector */
         strcpy(dname,argv[nopt]) ;
         subv[0] = '\0' ;
      } else if( cpt == argv[nopt] ){ /* can't be at start!*/
         fprintf(stderr," Illegal dataset specifier: %s\n",argv[nopt]) ;
         exit(1) ;
      } else {                        /* found selector */
         ii = cpt - argv[nopt] ;
         memcpy(dname,argv[nopt],ii) ; dname[ii] = '\0' ;
         strcpy(subv,cpt) ;
      }
      nopt++ ;

      dset = THD_open_one_dataset( dname ) ;
      if( dset == NULL ){
         sprintf (message, " Can't open dataset %s ", dname);
	 SC_error (message);
      }
      THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_MALLOC ) ;

      if( SC_type < 0 ) SC_type = dset->type ;

      /* check if voxel counts match */

      ii = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz ;
      if( SC_nvox < 0 ){
         SC_nvox = ii ;
      } else if( ii != SC_nvox ){
         sprintf (message ," Dataset %s differs in size from others",dname);
         SC_error (message);
      }
      ADDTO_3DARR(SC_dsar,dset) ;  /* list of datasets */

      /* process the sub-brick selector string,
         returning an array of int with
           svar[0]   = # of sub-bricks,
           svar[j+1] = index of sub-brick #j for j=0..svar[0] */

      svar = SC_get_subv( DSET_NVALS(dset) , subv ) ;
      if( svar == NULL || svar[0] <= 0 ){
         sprintf(message, " Can't decipher index codes from %s%s ",
		 dname,subv) ;
         SC_error (message);
      }
      if( is_thr && svar[0] != 1 ){
         SC_error (" Must specify single sub-brick for threshold data ");
      }
      ADDTO_XTARR(SC_subv,svar) ;  /* list of sub-brick selectors */

      max_nsub = MAX( max_nsub , svar[0] ) ;

      is_thr = 0;  /* Reset threshold sub-brick flag */


   }  /* end of loop over command line arguments */


   return ;
}


/*---------------------------------------------------------------------------*/
/*
  Decode a string like [1..3,5..9(2)] into an array of integers.

  This routine was adapted from: TCAT_get_subv
*/

int * SC_get_subv( int nvals , char * str )
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


/*---------------------------------------------------------------------------*/
/*
  Display help file.

  This routine was adapted from: TCAT_Syntax
*/

void SC_Syntax(void)
{
   printf(
    "Perform agglomerative hierarchical clustering for user specified \n"
    "parameter sub-bricks, for all voxels whose threshold statistic   \n"
    "is above a user specified value.\n"
    "\nUsage: 3dStatClust options datasets \n"
    "where the options are:\n"
   ) ;

   printf(
    "-prefix pname    = Use 'pname' for the output dataset prefix name.\n"
    "  OR                 [default='SC']\n"
    "-output pname\n"
    "\n"
    "-session dir     = Use 'dir' for the output dataset session directory.\n"
    "                     [default='./'=current working directory]\n"
    "-verb            = Print out verbose output as the program proceeds.\n"
    "\n"
    "Options for calculating distance between parameter vectors: \n"
    "   -dist_euc        = Calculate Euclidean distance between parameters \n"
    "   -dist_ind        = Statistical distance for independent parameters \n"
    "   -dist_cor        = Statistical distance for correlated parameters \n"
    "The default option is:  Euclidean distance. \n"
    "\n"
    "-thresh t tname  = Use threshold statistic from file tname. \n"
    "                   Only voxels whose threshold statistic is greater \n"
    "                   than t in abolute value will be considered. \n"
    "                     [If file tname contains more than 1 sub-brick, \n"
    "                     the threshold stat. sub-brick must be specified!]\n"
    "-nclust n        = This specifies the maximum number of clusters for \n"
    "                   output (= number of sub-bricks in output dataset).\n"
    "\n"
    "Command line arguments after the above are taken as parameter datasets.\n"
    "A dataset is specified using one of these forms:\n"
    "   'prefix+view', 'prefix+view.HEAD', or 'prefix+view.BRIK'.\n"
    "\n"
    "SUB-BRICK SELECTION:\n"
    "You can also add a sub-brick selection list after the end of the\n"
    "dataset name.  This allows only a subset of the sub-bricks to be\n"
    "used for clustering (by default, all of the input dataset sub-bricks\n"
    "are used for clustering).  A sub-brick selection list looks like\n"
    "one of the following forms:\n"
    "  fred+orig[5]                     ==> use only sub-brick #5\n"
    "  fred+orig[5,9,12]                ==> use #5, #9, and #12\n"
    "  fred+orig[5..8]     or [5-8]     ==> use #5, #6, #7, and #8\n"
    "  fred+orig[5..13(2)] or [5-13(2)] ==> use #5, #7, #9, #11, and #13\n"
    "Sub-brick indexes start at 0.  You can use the character '$'\n"
    "to indicate the last sub-brick in a dataset; for example, you\n"
    "can select every third sub-brick by using the selection list\n"
    "  fred+orig[0..$(3)]\n"
    "\n"
    "NOTES:\n"
    "* The '$', '(', ')', '[', and ']' characters are special to\n"
    "  the shell, so you will have to escape them.  This is most easily\n"
    "  done by putting the entire dataset plus selection list inside\n"
    "  single quotes, as in 'fred+orig[5..7,9]'.\n"
    "\n"
   ) ;

   exit(0) ;
}

/*-------------------------------------------------------------------------*/
/*
  Routine to initialize the program, get all operator inputs, 
  and assemble the parameter sub-bricks into one temporary dataset.

  The first sub-brick will contain the threshold statistics.
  The following sub-bricks will contain the parameters for clustering.

  This routine was adapted from: 3dTcat main routine

  Note:  Some of the code herein is superfluous.  However, since this routine
  is used only for creation of a temporary dataset, the vestigial code
  has been retained.

*/

THD_3dim_dataset * initialize_program ( int argc , char * argv[] )
{
   int ninp , ids , nv , iv,jv,kv , ivout , new_nvals , ivbot,ivtop ;
   THD_3dim_dataset * new_dset=NULL , * dset ;
   char buf[256] ;
   char message[80];        /* error message */


  /*----- Save command line for history notes -----*/
  commandline = tross_commandline( PROGRAM_NAME , argc,argv ) ;


   /*** read input options ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) SC_Syntax() ;

   SC_read_opts( argc , argv ) ;

   /*** create new dataset (empty) ***/

   ninp = SC_dsar->num ;
   if( ninp < 2 ){
      SC_error ("No parameter datasets? ");
   }

   new_nvals = 0 ;
   for( ids=0 ; ids < ninp ; ids++ ) new_nvals += NSUBV(ids) ;

   if( new_nvals < 2 ){
      SC_error ("No parameter sub-bricks? ");
   }


   /** find 1st dataset that is time dependent **/

   for( ids=0 ; ids < ninp ; ids++ ){
      dset = DSUB(ids) ;
      if( DSET_TIMESTEP(dset) > 0.0 ) break ;
   }
   if( ids == ninp ) dset = DSUB(0) ;

   new_dset = EDIT_empty_copy( dset ) ; /* make a copy of its header */

   tross_Make_History( PROGRAM_NAME , argc,argv , new_dset ) ;

   /* modify its header */

   EDIT_dset_items( new_dset ,
                      ADN_prefix        , SC_output_prefix ,
                      ADN_directory_name, SC_session ,
                      ADN_type          , SC_type ,
                      ADN_func_type     , ISANATTYPE(SC_type) ? ANAT_EPI_TYPE
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

   /* can't re-write existing dataset */

   if( THD_is_file(DSET_HEADNAME(new_dset)) ){
     sprintf(message," File %s already exists! ",
	     DSET_HEADNAME(new_dset) ) ;
     SC_error (message);
   }

   THD_force_malloc_type( new_dset->dblk , DATABLOCK_MEM_MALLOC ) ;


   /*** loop over input datasets ***/

   if( ninp > 1 ) myXtFree( new_dset->keywords ) ;

   ivout = 0 ;
   for( ids=0 ; ids < ninp ; ids++ ){
      dset = DSUB(ids) ;
      nv   = NSUBV(ids) ;

      if( SC_verb ) printf("Loading %s\n",DSET_FILECODE(dset)) ;
      DSET_load(dset) ;
      if( ! DSET_LOADED(dset) ){
	fprintf(stderr," Fatal error: can't load data from %s\n",
		DSET_FILECODE(dset)) ;
	exit(1) ;
      }

      /** loop over sub-bricks to output **/

      ivbot = ivout ;                       /* save this for later */
      for( iv=0 ; iv < nv ; iv++ ){
         jv = SUBV(ids,iv) ;                /* which sub-brick to use */

	 EDIT_substitute_brick( new_dset , ivout ,
				DSET_BRICK_TYPE(dset,jv) , DSET_ARRAY(dset,jv) ) ;

	 /*----- If this sub-brick is from a bucket dataset,
	   preserve the label for this sub-brick -----*/
	 
	 if( ISBUCKET(dset) )
	   sprintf (buf, "%s", DSET_BRICK_LABEL(dset,jv));
	 else
	   sprintf(buf,"%.12s[%d]",DSET_PREFIX(dset),jv) ;
	 EDIT_dset_items( new_dset, ADN_brick_label_one+ivout, buf, ADN_none );
	 
	 sprintf(buf,"%s[%d]",DSET_FILECODE(dset),jv) ;
	 EDIT_dset_items(
			 new_dset, ADN_brick_keywords_replace_one+ivout, buf, ADN_none ) ;
	 
	 EDIT_dset_items(
			 new_dset ,
			 ADN_brick_fac_one            +ivout, DSET_BRICK_FACTOR(dset,jv),
			 ADN_brick_keywords_append_one+ivout, DSET_BRICK_KEYWORDS(dset,jv),
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
	 
	 if( SC_verb ) printf("Copied %s[%d] into parametric dataset[%d]\n" ,
			      DSET_FILECODE(dset) , jv , ivout ) ;
	 
         ivout++ ;
      }
      ivtop = ivout ;  /* new_dset[ivbot..ivtop-1] are from the current dataset */

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

	    if( SC_verb > 1 ) printf("Unloaded unused %s[%d]\n" ,
				   DSET_FILECODE(dset) , kv ) ;
	  }
	}
      }


   } /* end of loop over input datasets */


  return (new_dset) ;


}


/*---------------------------------------------------------------------------*/
/*
  Get the time series for one voxel from the AFNI 3d+time data set.
*/

void extract_ts_array 
(
  THD_3dim_dataset * dset_time,      /* input 3d+time dataset */
  int iv,                            /* get time series for this voxel */
  float * ts_array                   /* time series data for voxel #iv */
)

{
  MRI_IMAGE * im = NULL;   /* intermediate float data */
  float * ar = NULL;       /* pointer to float data */
  int ts_length;           /* length of input 3d+time data set */
  int it;                  /* time index */


  /*----- Extract time series from 3d+time data set into MRI_IMAGE -----*/
  im = THD_extract_series (iv, dset_time, 0);


  /*----- Verify extraction -----*/
  if (im == NULL)  SC_error ("Unable to extract parameter data");


  /*----- Now extract time series from MRI_IMAGE -----*/
  ts_length = DSET_NUM_TIMES (dset_time);
  ar = MRI_FLOAT_PTR (im);
  for (it = 0;  it < ts_length;  it++)
    {
      ts_array[it] = ar[it];
    }


  /*----- Release memory -----*/
  mri_free (im);   im = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Perform agglomerative hierarchical clustering.
*/

THD_3dim_dataset * form_clusters (THD_3dim_dataset * param_dset)

{
  THD_3dim_dataset * new_dset = NULL;   /* hierarchical clustering */
  int ts_length;                   /* number of parameter sub-bricks */
  float * ts_array = NULL;         /* parameter array including threshold */
  float * par_array = NULL;        /* parameter array without threshold */
  int ixyz, nxyz;                  /* voxel indices */
  int iclust;                      /* cluster index */
  int num_voxels;                  /* number of voxels above threshold */
  int ip, jp;                      /* parameter indices */
  cluster * head_clust = NULL;     /* last cluster */
  float * parameters = NULL;       /* parameters after normalization */
  byte ** bar = NULL;              /* array of cluster sub-bricks */
  int nbricks;                     /* number of cluster sub-bricks */
  int ibrick;                      /* cluster sub-brick index */
  int ierror;                      /* number of errors in editing data */
  int ok;                          /* Boolean for successful matrix calc. */

  vector v, av;               /* intermediate vector results */
  matrix s;                   /* square root of covariance matrix */
  matrix sinv;                /* inverse of square root of covariance matrix */

  char message[80];           /* error message */


  /*----- Initialize vectors and matrices -----*/
  vector_initialize (&v);
  vector_initialize (&av);
  matrix_initialize (&s);
  matrix_initialize (&sinv);


  /*----- Initialize local variables -----*/
  nxyz = param_dset->daxes->nxx * param_dset->daxes->nyy 
    * param_dset->daxes->nzz;       
  ts_length = DSET_NUM_TIMES (param_dset);
  SC_dimension = ts_length - 1;
  printf ("Number of parameters = %d \n", SC_dimension);
  if (SC_dimension < 1)  SC_error ("No parameter data?");


  /*----- Set up array to hold vector of parameters -----*/
  ts_array = (float *) malloc (sizeof(float) * ts_length);
  MTEST (ts_array);
  par_array = ts_array + 1;


  /*----- Calculate covariance matrix for input parameters -----*/
  calc_covariance (param_dset, &num_voxels, &s, &sinv);


  /*----- Set number of sub-bricks -----*/
  if (num_voxels < SC_nclust)
    nbricks = num_voxels;
  else
    nbricks = SC_nclust;
  if (SC_verb) printf ("Output dataset will have %d sub-bricks\n\n", nbricks);


  /*-- Make an empty copy of prototype dataset, for eventual output --*/
  new_dset = EDIT_empty_copy (param_dset);


  /*----- Record history of dataset -----*/
  if( commandline != NULL ) tross_Append_History( new_dset , commandline ) ;


  /*----- Modify some structural properties.  Note that the nbricks
          just make empty sub-bricks, without any data attached. -----*/
  ierror = EDIT_dset_items (new_dset,
                            ADN_prefix,          SC_output_prefix,
			    ADN_directory_name,  SC_session,
			    ADN_type,            HEAD_FUNC_TYPE,
			    ADN_func_type,       FUNC_BUCK_TYPE,
                            ADN_ntt,             0,               /* no time */
			    ADN_nvals,           nbricks,
			    ADN_malloc_type,     DATABLOCK_MEM_MALLOC ,  
			    ADN_none ) ;
  
  if( ierror > 0 )
    {
      sprintf (message, 
	      " %d errors in attempting to create bucket dataset! ", 
	      ierror);
      SC_error (message);
    }
  
  if (THD_is_file(DSET_HEADNAME(new_dset))) 
    {
      sprintf (message,
	      " Output dataset file %s already exists--cannot continue! ",
	      DSET_HEADNAME(new_dset));
      SC_error (message);
    }


  /*----- Allocate memory -----*/
  bar  = (byte **) malloc (sizeof(byte *) * nbricks);
  MTEST (bar);
  

  /*----- Build lowest level of cluster hierarchy -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      /*----- Get array of parameters for this voxel -----*/
      extract_ts_array (param_dset, ixyz, ts_array);  

      /*----- If voxel is above threshold cutoff -----*/
      if (fabs(ts_array[0]) > SC_thr)
	{
	  /*----- Allocate space for parameter vector -----*/
	  parameters = (float *) malloc (sizeof(float) * SC_dimension);
	  MTEST (parameters);

	  /*----- If using stat. dist., transform the parameter vector -----*/
	  if (SC_statdist)
	    {
	      array_to_vector (SC_dimension, par_array, &v);
	      vector_multiply (sinv, v, &av);
	      vector_to_array (av, parameters);
	    }
	  /*----- Otherwise, just copy the parameter array -----*/
	  else
	    for (ip = 0;  ip < SC_dimension;  ip++)
	      parameters[ip] = par_array[ip];

	  /*----- Create new cluster containing single voxel -----*/
	  head_clust = new_cluster (ixyz, parameters, head_clust);
	}
    }


  /*----- Agglomerate clusters, one-by-one -----*/
  for (iclust = num_voxels;  iclust > 0;  iclust--)
    {
      if (SC_verb && (iclust % 100 == 0))
	printf ("# Clusters = %d \n", iclust);

      if (iclust <= nbricks)
	{
	  /*----- Sort clusters in order of size -----*/
	  head_clust = sort_clusters (head_clust);

	  /*----- Print cluster centroid parameters -----*/
	  if (SC_verb)
	    {
	      printf ("# Clusters = %d \n\n", iclust);
	      print_all_clusters (head_clust, s);
	    }
     
	  /*----- allocate memory for output sub-brick -----*/
	  ibrick = iclust-1;
	  bar[ibrick]  = (byte *) malloc (sizeof(byte) * nxyz);
	  MTEST (bar[ibrick]);

	  /*----- Save clusters into output sub-brick -----*/
	  for (ixyz = 0;  ixyz < nxyz;  ixyz++)	   
	    bar[ibrick][ixyz] = 0;
	  save_all_clusters (head_clust, bar[ibrick]); 

	  /*----- attach bar[ib] to be sub-brick #ibrick -----*/
	  EDIT_substitute_brick (new_dset, ibrick, MRI_byte, bar[ibrick]);

	}

      /*----- Agglomerate clusters -----*/
      if (iclust > 1)
	head_clust = agglomerate_clusters (head_clust);

    }


  /*----- Deallocate memory -----*/
  free (ts_array);   ts_array = NULL;
  vector_destroy (&v);
  vector_destroy (&av);
  matrix_destroy (&s);
  matrix_destroy (&sinv);
  free (head_clust->voxel_ptr);
  delete_cluster (head_clust);


  /*----- Return hierarchical clustering -----*/
  return (new_dset);
  
}


/*---------------------------------------------------------------------------*/

int main( int argc , char * argv[] )

{
  THD_3dim_dataset * param_dset = NULL;    /* statistical parameter data set */
  THD_3dim_dataset * clust_dset = NULL;    /* hierarchical clusters data set */

  
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR); 
  printf ("Date:    %s \n", PROGRAM_DATE);
  printf ("\n");


  /*----- Create dataset which contains all parameters of interest -----*/
  param_dset = initialize_program (argc, argv);


  /*----- Perform agglomerative hierarchical clustering -----*/
  clust_dset = form_clusters (param_dset);

  
  /*----- Deallocate memory for parameter dataset -----*/   
  THD_delete_3dim_dataset( param_dset , False ) ; param_dset = NULL ;


  /*----- Output the hierarchical clustering dataset -----*/
  if( SC_verb ) printf("Computing sub-brick statistics\n") ;
  THD_load_statistics( clust_dset ) ;
    
  if( SC_verb ) printf("Writing output to %s and %s\n",
		       DSET_HEADNAME(clust_dset) , DSET_BRIKNAME(clust_dset) );
  THD_write_3dim_dataset( NULL,NULL , clust_dset , True ) ;
  

  /*----- Deallocate memory for cluster dataset -----*/   
  THD_delete_3dim_dataset( clust_dset , False ) ; clust_dset = NULL ;
  
  exit(0) ;
  

}







