#include "mrilib.h"

#define CENFILL_ZERO  0
#define CENFILL_NBHR  1
#define CENFILL_MODEL 2
#define CENFILL_NONE  3
#define CENFILL_DSET  4

static char *CENFILL_str[] = { "zero" , "nbhr" , "model" , "none" , "dset" } ;

/*----------------------------------------------------------------------*/

static int is_numeric( char *str )
{
   char *spt ;
   if( str == NULL ) return 0 ;
   for( spt=str ; *spt != '\0' ; spt++ )
     if( !isdigit(*spt) && *spt != '-' && *spt != '.' ) return 0 ;
   return 1 ;
}

/*----------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *inset=NULL , *outset ;
   NI_element *nelmat=NULL ;
   int nrow,ncol , nxyz , ii,jj,kk,mm , iarg , nrowfull=0 , nrowout ;
   char *prefix  = "Synthesize" ;
   int   nselect = 0 ;
   char **select = NULL ;
   char *cdt , *cgrp , *clab , *ccc , *cgl, *matname = NULL;
   float  dt=0.0f ;
   NI_str_array *clab_sar=NULL ;
   int          *cgrp_val=NULL ;
   int Ngoodlist, *goodlist=NULL, Nbadlist, *badlist=NULL;
   int *nbblist=NULL , *nbtlist=NULL ;
   int   *ilist, nadd , nilist , ll , 
         dry=0 , nelim=0 , nerr, dry_info=0 ;
   float **clist , *tsar , *cfar , tval ;
   NI_int_array *niar ;

   int cenfill_mode=CENFILL_ZERO ;
   THD_3dim_dataset *cenfill_dset=NULL ;
   int nspk ;

   /*----- Read command line -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
       "Usage: 3dSynthesize options\n"
       "Reads a '-cbucket' dataset and a '.xmat.1D' matrix from 3dDeconvolve,\n"
       "and synthesizes a fit dataset using selected sub-bricks and\n"
       "matrix columns.\n"
       "\n"
       "Options (actually, the first 3 are mandatory)\n"
       "---------------------------------------------\n"
       " -cbucket ccc = Read the dataset 'ccc', which should have been\n"
       "                 output from 3dDeconvolve via the '-cbucket' option.\n"
       " -matrix mmm  = Read the matrix 'mmm', which should have been\n"
       "                 output from 3dDeconvolve via the '-x1D' option.\n"
       " -select sss  = Selects specific columns from the matrix (and the\n"
       "                 corresponding coefficient sub-bricks from the\n"
       "                 cbucket).  The string 'sss' can be of the forms:\n"
       "                   baseline  = All baseline coefficients.\n"
       "                   polort    = All polynomial baseline coefficients\n"
       "                               (skipping -stim_base coefficients).\n"
       "                   allfunc   = All coefficients that are NOT marked\n"
       "                               (in the -matrix file) as being in\n"
       "                               the baseline (i.e., all -stim_xxx\n"
       "                               values except those with -stim_base)\n"
       "                   allstim   = All -stim_xxx coefficients, including\n"
       "                               those with -stim_base.\n"
       "                   all       = All coefficients (should give results\n"
       "                               equivalent to '3dDeconvolve -fitts').\n"
       "                   something = All columns/coefficients that match\n"
       "                               this -stim_label from 3dDeconvolve\n"
       "                               [to be precise, all columns whose   ]\n"
       "                               [-stim_label starts with 'something']\n"
       "                               [will be selected for inclusion.    ]\n"
       "                   digits    = Columns can also be selected by\n"
       "                               numbers (starting at 0), or number\n"
       "                               ranges of the form 3..7 and 3-7.\n"
       "                               [A string is a number range if it]\n"
       "                               [comprises only digits and the   ]\n"
       "                               [characters '.' and/or '-'.      ]\n"
       "                               [Otherwise, it is used to match  ]\n"
       "                               [a -stim_label.                  ]\n"
       "                 More than one '-select sss' option can be used, or\n"
       "                 you can put more than one string after the '-select',\n"
       "                 as in this example:\n"
       "                   3dSynthesize -matrix fred.xmat.1D -cbucket fred+orig \\\n"
       "                                -select baseline FaceStim -prefix FS\n"
       "                 which synthesizes the baseline and 'FaceStim'\n"
       "                 responses together, ignoring any other stimuli\n"
       "                 in the dataset and matrix.\n"
       " -dry         = Don't compute the output, just check the inputs.\n"
       /*" -dry_info    = Don't compute the output, but give info on selection.\n"
       "                No need for cbucket with that option.\n"
       "                Output is to stdout.\n"*/
       " -TR dt       = Set TR in the output to 'dt'.  The default value\n"
       "                 of TR is read from the header of the matrix file.\n"
       " -prefix ppp  = Output result into dataset with name 'ppp'.\n"
       "\n"
       " -cenfill xxx = Determines how censored time points from the\n"
       "                 3dDeconvolve run will be filled.  'xxx' is one of:\n"
       "                   zero    = 0s will be put in at all censored times\n"
       "                   nbhr    = average of non-censored neighboring times\n"
       "                   none    = don't put the censored times in at all\n"
       "                             (in which  case the created  dataset is)\n"
       "                             (shorter than the input to 3dDeconvolve)\n"
#if 0
       "                   model   = compute the model at censored times\n"
       "                   dataset = take the censored values from this dataset\n"
       "                             (usually should be 3dDeconvolve's input)\n"
#endif
       "                 If you don't give some -cenfill option, the default\n"
       "                 operation is 'zero'.  This default is different than\n"
       "                 previous versions of this program, which did 'none'.\n"
       "          **N.B.: You might like the program to compute the model fit\n"
       "                  at the censored times, like it does at all others.\n"
       "                  This CAN be done if you input the matrix file saved\n"
       "                  by the '-x1D_uncensored' option in 3dDeconvolve.\n"
       "\n"
       "NOTES:\n"
       "-- You could do the same thing in 3dcalc, but this way is simpler\n"
       "   and faster.  But less flexible, of course.\n"
       "-- The output dataset is always stored as floats.\n"
       "-- The -cbucket dataset must have the same number of sub-bricks as\n"
       "   the input matrix has columns.\n"
       "-- Each column in the matrix file is a time series, used to model\n"
       "   some component of the data time series at each voxel.\n"
       "-- The sub-bricks of the -cbucket dataset give the weighting\n"
       "   coefficients for these model time series, at each voxel.\n"
       "-- If you want to calculate a time series dataset wherein the original\n"
       "   time series data has the baseline subtracted, then you could\n"
       "   use 3dSynthesize to compute the baseline time series dataset, and\n"
       "   then use 3dcalc to subtract that dataset from the original dataset.\n"
       "-- Other similar applications are left to your imagination.\n"
       "-- To see the column labels stored in matrix file 'fred.xmat.1D', type\n"
       "   the Unix command 'grep ColumnLabels fred.xmat.1D'; sample output:\n"
       " # ColumnLabels = \"Run#1Pol#0 ; Run#1Pol#1 ; Run#2Pol#0 ; Run#2Pol#1 ;\n"
       "                   FaceStim#0 ; FaceStim#1 ; HouseStim#0 ; HouseStim#1\"\n"
       "   which shows the 4 '-polort 1' baseline parameters from 2 separate\n"
       "   imaging runs, and then 2 parameters each for 'FaceStim' and\n"
       "   'HouseStim'.\n"
       "-- The matrix file written by 3dDeconvolve has an XML-ish header\n"
       "   before the columns of numbers, stored in '#' comment lines.\n"
       "   If you want to generate your own 'raw' matrix file, without this\n"
       "   header, you can still use 3dSynthesize, but then you can only use\n"
       "   numeric '-select' options (or 'all').\n"
       "-- When using a 'raw' matrix, you'll probably also want the '-TR' option.\n"
       "-- When putting more than one string after '-select', do NOT combine\n"
       "   these separate strings togther in quotes.  If you do, they will be\n"
       "   seen as a single string, which almost surely won't match anything.\n"
       "-- Author: RWCox -- March 2007\n"
      ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   /** AFNI package setup and logging **/

   mainENTRY("3dSynthesize main"); machdep(); AFNI_logger("3dSynthesize",argc,argv);
   PRINT_VERSION("3dSynthesize") ; AUTHOR("RW Cox") ;
   (void)COX_clock_time() ;  /* anticipating the very end of time */

   /** parse command line options **/

   iarg = 1 ;
   while( iarg < argc ){

      /** -cenfill xxx **/

      if( strcmp(argv[iarg],"-cenfill") == 0 ){   /* 21 Jun 2007 */
        iarg++ ;
        if( cenfill_dset != NULL )
          ERROR_exit("Can't use -cenfill twice!") ;
        if( strcasecmp(argv[iarg],"zero") == 0 ){
          cenfill_mode = CENFILL_ZERO ;
        } else if( strcasecmp(argv[iarg],"none") == 0 ){
          cenfill_mode = CENFILL_NONE ;
#if 0
        } else if( strcasecmp(argv[iarg],"model") == 0 ){
          cenfill_mode = CENFILL_MODEL ;
          ERROR_exit("-cenfill model NOT YET IMPLEMENTED!") ;
#endif
        } else if( strcasecmp(argv[iarg],"nbhr") == 0 ){
          cenfill_mode = CENFILL_NBHR ;
        } else {
#if 0
          cenfill_mode = CENFILL_DSET ;
          cenfill_dset = THD_open_dataset( argv[iarg] ) ;
          if( !ISVALID_DSET(cenfill_dset) )
            ERROR_exit("Can't open -cenfill dataset '%s'",argv[iarg]) ;
          DSET_load(cenfill_dset) ;
          if( !DSET_LOADED(cenfill_dset) )
            ERROR_exit("Can't load -cenfill dataset '%s'",argv[iarg]) ;
#else
          ERROR_exit("-cenfill '%s' is unrecognized",argv[iarg]) ;
#endif
        }
        iarg++ ; continue ;
      }

      /** -TR or -dt **/

      if( strcmp(argv[iarg],"-TR") == 0 || strcmp(argv[iarg],"-dt") == 0 ){
        dt = (float)strtod(argv[++iarg],NULL) ;
        iarg++ ; continue ;
      }

      /** output dataset prefix **/

      if( strcmp(argv[iarg],"-prefix") == 0 ){
        prefix = argv[++iarg] ;
        if( !THD_filename_ok(prefix) ) ERROR_exit("-prefix is not good!");
        iarg++ ; continue ;
      }

      /** -cbucket **/

      if( strcmp(argv[iarg],"-cbucket") == 0 ){
        if( inset != NULL ) ERROR_exit("More than 1 -cbucket option!");
        inset = THD_open_dataset(argv[++iarg]) ;
        if( inset == NULL ) ERROR_exit("Can't open -cbucket dataset!");
        if( !dry ){
          DSET_load(inset) ;
          if( !DSET_LOADED(inset) ) ERROR_exit("Can't load -cbucket dataset!");
        }
        iarg++ ; continue ;
      }

      /** -matrix **/

      if( strcmp(argv[iarg],"-matrix") == 0 ){
        if( nelmat != NULL ) ERROR_exit("More than 1 -matrix option!");
        nelmat = NI_read_element_fromfile( argv[++iarg] ) ;
        matname = argv[iarg];
        if( nelmat == NULL ){
          MRI_IMAGE *nim ; float *nar ;
          nim = mri_read_1D(argv[iarg]) ;
          if( nim != NULL ){    /* construct a minimal NIML element */
            nelmat = NI_new_data_element( "matrix" , nim->nx ) ;
            nar    = MRI_FLOAT_PTR(nim) ;
            for( jj=0 ; jj < nim->ny ; jj++ )
              NI_add_column( nelmat , NI_FLOAT , nar + nim->nx*jj ) ;
            mri_free(nim) ; nelim = 1 ;
          }
        }
        if( nelmat == NULL || nelmat->type != NI_ELEMENT_TYPE )
          ERROR_exit("Can't process -matrix file!");
        iarg++ ; continue ;
      }

      /** -select **/

      if( strcmp(argv[iarg],"-select") == 0 ){
        for( iarg++ ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
          select = (char **)realloc(select,sizeof(char *)*(nselect+1)) ;
          select[nselect++] = strdup(argv[iarg]) ;
        }
        continue ;
      }

      /** -dry **/

      if( strcmp(argv[iarg],"-dry") == 0 ){
        dry++ ; iarg++ ; continue ;
      }
      if( strcmp(argv[iarg],"-dry_info") == 0 ){
        dry_info++ ; iarg++ ; continue ;
      }
      /** bozo-ific user **/

      ERROR_exit("Unknown option: %s",argv[iarg]) ;
   }

   if (dry_info && !inset) {
      if (matname) inset = THD_open_dataset(matname) ;
      if (!inset) {
         ERROR_exit("Can't load -matrix for inset use.");
      }
   }
   ii = 0 ;
   if( nelmat == NULL ){ ii++; ERROR_message("Missing -matrix!") ; }
   if( inset  == NULL ){ ii++; ERROR_message("Missing -cbucket!"); }
   if( select == NULL ){ ii++; ERROR_message("Missing -select!") ; }
   if( ii > 0 )                ERROR_exit("3dSynthesize: can't continue!") ;

   /*-- look at matrix, get it's pieces --*/

   ncol = DSET_NVALS(inset) ;
   nrow = nelmat->vec_len ;
   if( ncol != nelmat->vec_num )
     ERROR_exit("-matrix has %d columns but -cbucket has %d sub-bricks!?",
                nelmat->vec_num , ncol ) ;

   if( nelim ){  /* no NIML header to get */

     if( cenfill_mode != CENFILL_NONE ){
       WARNING_message("raw .1D input ==> using -cenfill none") ;
       cenfill_mode = CENFILL_NONE ;
       if( cenfill_dset != NULL ){ DSET_delete(cenfill_dset); cenfill_dset=NULL; }
     }
     Ngoodlist = nrow ; goodlist = (int *)malloc(sizeof(int)*nrow) ;
     for( ii=0 ; ii < nrow ; ii++ ) goodlist[ii] = ii ;
     Nbadlist = 0 ; badlist = NULL ; nrowfull = nrow ;

   } else {      /* get data from NIML header */

     clab = NI_get_attribute( nelmat , "ColumnLabels" ) ;
     if( clab == NULL )
       ERROR_exit("-matrix is missing 'ColumnLabels' attribute!?") ;
     clab_sar = NI_decode_string_list( clab , ";" ) ;
     if( clab_sar == NULL || clab_sar->num < ncol )
       ERROR_exit("-matrix 'ColumnLabels' badly formatted!?") ;

     cgrp = NI_get_attribute( nelmat , "ColumnGroups" ) ;
     if( cgrp == NULL )
       ERROR_exit("-matrix is missing 'ColumnGroups' attribute!?") ;
     niar = NI_decode_int_list( cgrp , ";,") ;
     if( niar == NULL || niar->num < ncol )
       ERROR_exit("-matrix 'ColumnGroups' badly formatted!?") ;
     cgrp_val = niar->ar ;

     cgl = NI_get_attribute( nelmat , "GoodList" ) ;
     if( cgl == NULL || cenfill_mode == CENFILL_NONE ){
       if( cenfill_mode != CENFILL_NONE )
         WARNING_message("-matrix is missing 'GoodList': can't do -cenfill") ;
       Ngoodlist = nrow ; goodlist = (int *)malloc(sizeof(int)*nrow) ;
       for( ii=0 ; ii < nrow ; ii++ ) goodlist[ii] = ii ;
       Nbadlist = 0; badlist = NULL; nrowfull = nrow; cenfill_mode = CENFILL_NONE;
     } else {
       int *qlist ;
       niar = NI_decode_int_list( cgl , ";,") ;
       if( niar == NULL || niar->num < nrow )
         ERROR_exit("-matrix 'GoodList' badly formatted?") ;
       Ngoodlist = niar->num ; goodlist = niar->ar ;
       cgl = NI_get_attribute( nelmat , "NRowFull" ) ;
       if( cgl != NULL ) nrowfull = (int)strtol(cgl,NULL,10) ;
       else              nrowfull = goodlist[Ngoodlist-1]+1 ;
       qlist = (int *)calloc(sizeof(int),nrowfull) ;
       for( ii=0 ; ii < nrow ; ii++ ) qlist[goodlist[ii]] = 1 ;
       for( ii=jj=0 ; ii < nrowfull ; ii++ ) if( !qlist[ii] ) jj++ ;
       Nbadlist = jj ;
       if( Nbadlist > 0 ){
         badlist = (int *)malloc(sizeof(int)*Nbadlist) ;
         nbblist = (int *)malloc(sizeof(int)*Nbadlist) ;
         nbtlist = (int *)malloc(sizeof(int)*Nbadlist) ;
         for( ii=jj=0 ; ii < nrowfull ; ii++ )
           if( !qlist[ii] ) badlist[jj++] = ii ;
         for( jj=0 ; jj < Nbadlist ; jj++ ){
           ii = badlist[jj] ;
           for( kk=ii-1 ; kk >= 0 && !qlist[kk] ; kk-- ) ; /*nada*/
           nbblist[jj] = kk ;
           for( kk=ii+1 ; kk < nrowfull && !qlist[kk] ; kk++ ) ; /*nada*/
           nbtlist[jj] = (kk < nrowfull) ? kk : -1 ;
         }
       }
       free((void *)qlist) ;
     }

     if( dt <= 0.0f ){
       cdt = NI_get_attribute( nelmat , "RowTR" ) ;
       if( cdt != NULL ) dt = (float)strtod(cdt,NULL) ;
     }
   }

   if( dt <= 0.0f ){
     dt = 1.0f ; WARNING_message("Using default TR=1.0") ;
   }

   /* clist[i]    = pointer to i-th column in matrix (i=0..ncol-1)
      clist[i][j] = j-th time point value in the i-th column (j=0..nrow-1) */

   clist = (float **)malloc(sizeof(float *)*ncol) ;
   if( nelmat->vec_typ[0] == NI_FLOAT ){
     for( ii=0 ; ii < ncol ; ii++ ) clist[ii] = (float *)nelmat->vec[ii] ;
   } else if( nelmat->vec_typ[0] == NI_DOUBLE ){
     double *cd ;
     for( ii=0 ; ii < ncol ; ii++ ){
       clist[ii] = (float *)malloc(sizeof(float)*nrow) ;
       cd        = (double *)nelmat->vec[ii] ;
       for( jj=0 ; jj < nrow ; jj++ ) clist[ii][jj] = (float)cd[jj] ;
     }
   } else {
     ERROR_exit("-matrix file stored will illegal data type!?") ;
   }

   /*-- process the -select options to build a column list --*/

   ilist = (int *)calloc(sizeof(int),ncol) ;  /* list of all columns */

   for( nerr=kk=0 ; kk < nselect ; kk++ ){
     nadd = 0 ;

     if( strcasecmp(select[kk],"baseline") == 0 ){       /* all baselines ---*/

       if( nelim ){
         ERROR_message("'-select %s' illegal with raw matrix input",select[kk]) ;
         nerr++ ; continue ;
       }
       for( ii=0 ; ii < ncol ; ii++ )
         if( cgrp_val[ii] <= 0 ){ ilist[ii]++ ; nadd++ ; }

     } else if( strcasecmp(select[kk],"polort") == 0 ){  /* polort baselines */

       if( nelim ){
         ERROR_message("'-select %s' illegal with raw matrix input",select[kk]) ;
         nerr++ ; continue ;
       }
       for( ii=0 ; ii < ncol ; ii++ )
         if( cgrp_val[ii] < 0 ){ ilist[ii]++ ; nadd++ ; }

     } else if( strcasecmp(select[kk],"allfunc") == 0 ){ /* non-baselines ---*/

       if( nelim ){
         ERROR_message("'-select %s' illegal with raw matrix input",select[kk]) ;
         nerr++ ; continue ;
       }
       for( ii=0 ; ii < ncol ; ii++ )
         if( cgrp_val[ii] > 0 ){ ilist[ii]++ ; nadd++ ; }

     } else if( strcasecmp(select[kk],"allstim") == 0 ){ /* non-polorts -----*/

       if( nelim ){
         ERROR_message("'-select %s' illegal with raw matrix input",select[kk]) ;
         nerr++ ; continue ;
       }
       for( ii=0 ; ii < ncol ; ii++ )
         if( cgrp_val[ii] >= 0 ){ ilist[ii]++ ; nadd++ ; }

     } else if( strcasecmp(select[kk],"all") == 0 ){     /* all -------------*/

       for( ii=0 ; ii < ncol ; ii++ ){ ilist[ii]++ ; nadd++ ; }

     } else if( is_numeric(select[kk]) ){                /* number range ----*/

       int aa , bb=-1 ; char *spt=select[kk] , *qpt ;
       for( ; *spt != '\0' && !isdigit(*spt) ; spt++ ) ; /* skip nondigits */
       if( spt != '\0' ){
         aa = (int)strtol(spt,&qpt,10) ;
         if( *qpt != '\0' ){
           for( ; *qpt != '\0' && !isdigit(*qpt) ; qpt++ ) ; /* skip */
           bb = (int)strtol(qpt,NULL,10) ;
         }
         if( bb <  aa   ) bb = aa ;
         if( bb >= ncol ) bb = ncol-1 ;
         if( aa >= 0 && aa < ncol ){
           for( ; aa <= bb ; aa++ ){ ilist[aa]++ ; nadd++ ; }
         }
       }

     } else {                                            /* a stim label ----*/

       if( nelim ){
         ERROR_message("'-select %s' illegal with raw matrix input",select[kk]) ;
         nerr++ ; continue ;
       }
       for( ii=0 ; ii < ncol ; ii++ )
         if( strstr(clab_sar->str[ii],select[kk]) == clab_sar->str[ii] ){
           ilist[ii]++ ; nadd++ ;
         }
     }

     if( nadd == 0 )
       WARNING_message("-select '%s' didn't match any matrix columns!",
                       select[kk]) ;
   }

   if( nerr > 0 )
     ERROR_exit("Can't continue after '-select' errors") ;

   /* count number of 'activated' columns */

   for( nilist=ii=0 ; ii < ncol ; ii++ ) if( ilist[ii] ) nilist++ ;

   if( nilist == 0 )
     ERROR_exit("No columns selected for dataset synthesis!") ;

   if( dry ){
     INFO_message("Index list: %d nonzero entries",nilist) ;
     fprintf(stderr,"++ ") ;
     for( ii=0 ; ii < ncol ; ii++ ) 
         if( ilist[ii] ) 
            fprintf(stderr," %d "
                           ,ii) ;
     fprintf(stderr,"\n") ;
     INFO_message("3dSynthesize exits: -dry option was given") ;
     exit(0) ;
   }
   if (dry_info) {
     fprintf(stdout,"TR:%.3f\tN_TR:%d\n", dt, nrow);
     for( ii=0 ; ii < ncol ; ii++ ) 
         if( ilist[ii] ) 
            fprintf(stdout,"%d:%s \t"
                           ,ii, clab_sar->str[ii]) ;
     fprintf(stdout,"\n") ;
     exit(0); 
   }

   /*-- create empty output 3D+time dataset --*/

   outset  = EDIT_empty_copy( inset ) ;
   nrowout = (cenfill_mode == CENFILL_NONE) ? nrow : nrowfull ;
   EDIT_dset_items( outset ,
                      ADN_prefix    , prefix ,
                      ADN_brick_fac , NULL ,
                      ADN_datum_all , MRI_float ,
                      ADN_nvals     , nrowout ,
                      ADN_ntt       , nrowout ,
                      ADN_ttdel     , dt ,
                      ADN_tunits    , UNITS_SEC_TYPE ,
                      ADN_type      , HEAD_ANAT_TYPE ,
                      ADN_func_type , ANAT_OMRI_TYPE ,
                    ADN_none ) ;

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(outset)) )
     ERROR_exit("Output dataset already exists: %s\n",DSET_HEADNAME(outset)) ;

   tross_Copy_History( outset , inset ) ;
   tross_Make_History( "3dSynthesize" , argc , argv , outset ) ;

   nxyz = DSET_NVOX(inset) ;

   /* create bricks (will be filled with zeros) */

   for( jj=0 ; jj < nrowout ; jj++ )
     EDIT_substitute_brick( outset , jj , MRI_float , NULL ) ;

   tsar = (float *)calloc(sizeof(float),nrowout+1) ;
   cfar = (float *)calloc(sizeof(float),ncol+1) ;

   INFO_message("Output has %d time points at TR=%g",nrowout,dt) ;
   if( Nbadlist > 0 ){
    if( cenfill_mode == CENFILL_NONE )
      INFO_message("Input had %d time points censored; these are NOT filled",Nbadlist);
    else
      INFO_message("Input had %d time points censored; filling mode = %s",
                   Nbadlist,CENFILL_str[cenfill_mode]);
   }

   nspk = nxyz / 51 ;
   fprintf(stderr,"++ Calculating: ") ;
   for( kk=0 ; kk < nxyz ; kk++ ){   /* kk = voxel index */

      if( nspk > 0 && kk > 0 && kk%nspk == 0 ) fprintf(stderr,".") ;

      /* get kk-th voxel's coefficient array into cfar */

      (void)THD_extract_array( kk , inset , 0 , cfar ) ;
      for( ii=0 ; ii < ncol && cfar[ii] == 0.0f ; ii++ ) ; /* nada */
      if( ii == ncol ) continue ;   /** coefficients are all zero! */

      /* add up matrix columns with the given weights */

      memset( tsar , 0 , sizeof(float)*(nrowout+1) ) ;
      for( jj=0 ; jj < nrow ; jj++ ){
        tval = 0.0f ;
        for( ii=0 ; ii < ncol ; ii++ )
          if( ilist[ii] ) tval += cfar[ii]*clist[ii][jj] ;
        tsar[goodlist[jj]] = tval ;
      }

      /* deal with cenfill now */

      switch( cenfill_mode ){
        default:
        case CENFILL_NONE:
        case CENFILL_ZERO: break ;

        case CENFILL_NBHR:
          for( jj=0 ; jj < Nbadlist ; jj++ ){
            ii = nbblist[jj] ; mm = nbtlist[jj] ;
            if( ii >=0 && mm >= 0 ) tval = 0.5f*(tsar[ii]+tsar[mm]) ;
            else if( ii >= 0 )      tval = tsar[ii] ;
            else if( mm >= 0 )      tval = tsar[mm] ;
            else                    tval = 0.0f ;
            tsar[badlist[jj]] = tval ;
          }
        break ;

        case CENFILL_DSET: break ;

        case CENFILL_MODEL: break ;
      }

      /* put result time series into output dataset */

      THD_insert_series( kk , outset ,
                         nrowout , MRI_float , tsar , 1 ) ;
   }
   fprintf(stderr,"!\n") ;

   /** write output, and let freedom ring!! **/

   DSET_write(outset) ;
   WROTE_DSET(outset) ;
   INFO_message("CPU time=%.2f s ; Elapsed=%.2f s",
                COX_cpu_time(),COX_clock_time()  ) ;
   exit(0) ;
}
