/*---------------------------------------------------------------------------*/
/*
  This program creates AFNI "bucket" type datasets.

  File:    3dbucket.c
  Author:  R. W. Cox
  Date:    17 December 1997


  Mod:     Changes to implement "-glueto" command option. 
           Also, modified output to preserve sub-brick labels.
  Author:  B. D. Ward
  Date:    04 February 1998

*/
/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  This software is Copyright 1997, 1998 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/

#define PROGRAM_NAME "3dbucket"                      /* name of this program */
#define LAST_MOD_DATE "04 February 1998"         /* date of last program mod */

#include "mrilib.h"

#ifndef myXtFree
#   define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

/*-------------------------- global data --------------------------*/

static THD_3dim_dataset_array * BUCK_dsar  = NULL ;
static XtPointer_array        * BUCK_subv  = NULL ;
static int                      BUCK_nvox  = -1 ;
static int                      BUCK_dry   = 0 ;
static int                      BUCK_verb  = 0 ;
static int                      BUCK_type  = -1 ;
static int                      BUCK_glue  = 0 ;

static char BUCK_output_prefix[THD_MAX_PREFIX] = "buck" ;
static char BUCK_session[THD_MAX_NAME]         = "./"   ;

#define NSUBV(id)   ( ((int *)BUCK_subv->ar[(id)])[0]      )
#define SUBV(id,jj) ( ((int *)BUCK_subv->ar[(id)])[(jj)+1] )
#define DSUB(id)    DSET_IN_3DARR(BUCK_dsar,(id))

/*--------------------------- prototypes ---------------------------*/

void BUCK_read_opts( int , char ** ) ;
void BUCK_Syntax(void) ;
int * BUCK_get_subv( int , char * ) ;

/*--------------------------------------------------------------------
   read the arguments, load the global variables
----------------------------------------------------------------------*/

void BUCK_read_opts( int argc , char * argv[] )
{
   int nopt = 1 , ii ;
   char dname[THD_MAX_NAME] ;
   char subv[THD_MAX_NAME] ;
   char * cpt ;
   THD_3dim_dataset * dset ;
   int * svar ;
   char * str;
   int ok, ilen, nlen;

   INIT_3DARR(BUCK_dsar) ;
   INIT_XTARR(BUCK_subv) ;

   while( nopt < argc ){

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ||
          strncmp(argv[nopt],"-output",6) == 0   ){
	 if (BUCK_glue){
            fprintf(stderr,"-prefix and -glueto options are not compatible\n");
	    exit(1) ;
	 }	   
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -prefix!\n") ; exit(1) ;
         }
         MCW_strncpy( BUCK_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

      /**** -session directory ****/

      if( strncmp(argv[nopt],"-session",6) == 0 ){
	 if (BUCK_glue){
            fprintf(stderr,
		    "-session and -glueto options are not compatible\n");
	    exit(1) ;
	 }	   
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -session!\n") ; exit(1) ;
         }
         MCW_strncpy( BUCK_session , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }

      if( strncmp(argv[nopt],"-dry",3) == 0 ){
         BUCK_dry = BUCK_verb = 1 ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-fbuc",4) == 0 ){
         BUCK_type = HEAD_FUNC_TYPE ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-abuc",4) == 0 ){
         BUCK_type = HEAD_ANAT_TYPE ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-verb",2) == 0 ){
         BUCK_verb = 1 ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-glueto",5) == 0 ){
	 if( strncmp(BUCK_output_prefix, "buck", 5) != 0 ){
            fprintf(stderr,"-prefix and -glueto options are not compatible\n");
	    exit(1) ;
	 }	   
	 if( strncmp(BUCK_session, "./", 5) != 0 ){
            fprintf(stderr,
		    "-session and -glueto options are not compatible\n");
	    exit(1) ;
	 }	   
	 BUCK_glue = 1 ;
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -glueto!\n") ; exit(1) ;
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
	       "File name must end in +orig, +acpc, or +tlrc after -glueto\n");
	     exit(1);
	   }
	 
	 /*----- Remove View Type from string to make output prefix -----*/ 
         MCW_strncpy( BUCK_output_prefix , argv[nopt] , ilen+1) ;

	 /*----- Note: no "continue" statement here.  File name will now
	   be processed as an input dataset -----*/
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

      if( BUCK_type < 0 ) BUCK_type = dset->type ;

      ii = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz ;
      if( BUCK_nvox < 0 ){
         BUCK_nvox = ii ;
      } else if( ii != BUCK_nvox ){
         fprintf(stderr,"dataset %s differs in size from others\n",dname);
         exit(1) ;
      }
      ADDTO_3DARR(BUCK_dsar,dset) ;

      svar = BUCK_get_subv( DSET_NVALS(dset) , subv ) ;
      if( svar == NULL || svar[0] <= 0 ){
         fprintf(stderr,"can't decipher index codes from %s%s\n",dname,subv) ;
         exit(1) ;
      }
      ADDTO_XTARR(BUCK_subv,svar) ;

   }  /* end of loop over command line arguments */

   return ;
}

/*------------------------------------------------------------------*/

int * BUCK_get_subv( int nvals , char * str )
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

void BUCK_Syntax(void)
{
   printf(
    "Concatenate sub-bricks from input datasets into one big\n"
    "'bucket' dataset.\n"
    "Usage: 3dbucket options\n"
    "where the options are:\n"
   ) ;

   printf(
    "     -prefix pname = Use 'pname' for the output dataset prefix name.\n"
    " OR  -output pname     [default='buck']\n"
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
    "     -fbuc         = Create a functional bucket.\n"
    "     -abuc         = Create an anatomical bucket.  If neither of\n"
    "                       these options is given, the output type is\n"
    "                       determined from the first input type.\n"
    "\n"
    "Command line arguments after the above are taken as input datasets.\n"
    "A dataset is specified using one of these forms:\n"
    "   'prefix+view', 'prefix+view.HEAD', or 'prefix+view.BRIK'.\n"
    "You can also add a sub-brick selection list after the end of the\n"
    "dataset name.  This allows only a subset of the sub-bricks to be\n"
    "included into the output (by default, all of the input dataset\n"
    "is copied into the output).  A sub-brick selection list looks lik\n"
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
    "N.B.: The sub-bricks are output in the order specified, which may\n"
    " not be the order in the original datasets.  For example, using\n"
    "  fred+orig[0..$(2),1..$(2)]\n"
    " will cause the sub-bricks in fred+orig to be output into the\n"
    " new dataset in an interleaved fashion.  Using\n"
    "  fred+orig[$..0]\n"
    " will reverse the order of the sub-bricks in the output.\n"
    "\n"
    "N.B.: Bucket datasets have multiple sub-bricks, but do NOT have\n"
    " a time dimension.  You can input sub-bricks from a 3D+time dataset\n"
    " into a bucket dataset.  You can use the '3dinfo' program to see\n"
    " how many sub-bricks a 3D+time or a bucket dataset contains.\n"
    "\n"
    "N.B.: The '$', '(', ')', '[', and ']' characters are special to\n"
    " the shell, so you will have to escape them.  This is most easily\n"
    " done by putting the entire dataset plus selection list inside\n"
    " single quotes, as in 'fred+orig[5..7,9]'.\n"
    "\n"
    "N.B.: In non-bucket functional datasets (like the 'fico' datasets\n"
    " output by FIM, or the 'fitt' datasets output by 3dttest), sub-brick\n"
    " [0] is the 'intensity' and sub-brick [1] is the statistical parameter\n"
    " used as a threshold.  Thus, to create a bucket dataset using the\n"
    " intensity from dataset A and the threshold from dataset B, and\n"
    " calling the output dataset C, you would type\n"
    "    3dbucket -prefix C -fbuc 'A+orig[0]' -fbuc 'B+orig[1]'\n"
   ) ;

   exit(0) ;
}

/*------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int ninp , ids , nv , iv,jv,kv , ivout , new_nvals ;
   THD_3dim_dataset * new_dset=NULL , * dset ;
   char buf[256] ;

   /*----- identify program -----*/
   printf ("\n\nProgram %s \n", PROGRAM_NAME);
   printf ("Last revision: %s \n\n", LAST_MOD_DATE);


   /*** read input options ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) BUCK_Syntax() ;

   BUCK_read_opts( argc , argv ) ;

   /*** create new dataset (empty) ***/
   ninp = BUCK_dsar->num ;
   if( ninp < 1 ){
      fprintf(stderr,"*** No input datasets?\n") ; exit(1) ;
   }

   new_nvals = 0 ;
   for( ids=0 ; ids < ninp ; ids++ ) new_nvals += NSUBV(ids) ;

   if( BUCK_verb ) printf("-verb: output will have %d sub-bricks\n",new_nvals) ;

   new_dset = EDIT_empty_copy( DSUB(0) ) ;

   EDIT_dset_items( new_dset ,
                      ADN_prefix        , BUCK_output_prefix ,
                      ADN_directory_name, BUCK_session ,
                      ADN_type          , BUCK_type ,
                      ADN_func_type     , ISANATTYPE(BUCK_type) ? ANAT_BUCK_TYPE
                                                                : FUNC_BUCK_TYPE,
                      ADN_ntt           , 0 ,
                      ADN_nvals         , new_nvals ,
                    ADN_none ) ;

   /* can't re-write existing dataset, unless glueing is used */

   if (! BUCK_glue){
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

      if( ! BUCK_dry ){
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

         if( ! BUCK_dry ){
            EDIT_substitute_brick( new_dset , ivout ,
                                   DSET_BRICK_TYPE(dset,jv) , DSET_ARRAY(dset,jv) ) ;

	    /*----- If this sub-brick is from a bucket dataset, 
	      preserve the label for this sub-brick -----*/
	    if (dset->func_type == FUNC_BUCK_TYPE)
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
            }

            /** print a message? **/

            if( BUCK_verb ) printf("-verb: copied %s[%d] into %s[%d]\n" ,
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

      if( ! BUCK_dry && nv < DSET_NVALS(dset) ){

         for( kv=0 ; kv < DSET_NVALS(dset) ; kv++ ){  /* all input sub-bricks */
            for( iv=0 ; iv < nv ; iv++ ){             /* all output sub-bricks */
               jv = SUBV(ids,iv) ;
               if( jv == kv ) break ;                 /* input matches output */
            }
            if( iv == nv ){
               mri_free( DSET_BRICK(dset,kv) ) ;
#if 0
               if( BUCK_verb ) printf("-verb: unloaded unused %s[%d]\n" ,
                                      DSET_FILECODE(dset) , kv ) ;
#endif
            }
         }
      }

   } /* end of loop over input datasets */

   if( ! BUCK_dry ){
      if( BUCK_verb ) printf("-verb: loading statistics\n") ;
      THD_load_statistics( new_dset ) ;
      if( BUCK_verb ) printf("-verb: writing output\n") ;
      THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
   }

   exit(0) ;
}
