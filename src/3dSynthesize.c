#include "mrilib.h"

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
   int nrow,ncol , nxyz , ii,jj,kk , iarg ;
   char *prefix  = "Synthesize" ;
   int   nselect = 0 ;
   char **select = NULL ;
   char *cdt , *cgrp , *clab , *ccc ;
   float  dt=0.0f ;
   NI_str_array *clab_sar=NULL , *cgrp_sar=NULL ;
   int                           *cgrp_val=NULL ;
   int *ilist, nadd , nilist , ll , dry=0 , nelim=0 , nerr ;
   float **clist , *tsar , *cfar ;

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
       " -TR dt       = Set TR in the output to 'dt'.  The default value\n"
       "                 of TR is read from the header of the matrix file.\n"
       " -prefix ppp  = Output result into dataset with name 'ppp'.\n"
       "\n"
       "NOTES:\n"
       "-- You could do the same thing in 3dcalc, but this way is simpler\n"
       "   and faster.  But less flexible, of course.\n"
       "-- The output dataset is always stored as floats.\n"
       "-- The input dataset must have the same number of sub-bricks as\n"
       "   the input matrix has columns.\n"
       "-- Each column in the matrix file is a time series.\n"
       "-- The sub-bricks of the dataset give the weighting coefficients\n"
       "   for these time series, at each voxel.\n"
       "-- To see the column labels stored in matrix file 'fred.xmat.1D', type\n"
       "   the Unix command 'grep ColumnLabels fred.xmat.1D'; sample output:\n"
       " # ColumnLabels = \"Run#1Pol#0 ; Run#1Pol#1 ; Run#2Pol#0 ; Run#2Pol#1 ;\n"
       "                   FaceStim#0 ; FaceStim#1 ; HouseStim#0 ; HouseStim#1\"\n"
       "   which shows the 4 '-polort 1' baseline parameters from 2 separate\n"
       "   imaging runs, and then 2 parameters each for 'FaceStim' and\n"
       "   'HouseStim'.\n"
       "-- The matrix file written by 3dDeconvolve has an XML-ish header\n"
       "   before the columns of numbers.  If you generate your own 'raw' matrix\n"
       "   file, without the header, you can still use 3dSynthesize, but then\n"
       "   you can only use numeric '-select' options (or 'all').\n"
       "-- When using a 'raw' matrix, you'll probably also want the '-TR' option.\n"
       "-- When putting more than one string after '-select', do NOT put\n"
       "   these separate strings in quotes.  If you do, they will be seen\n"
       "   as a single string, which probably won't match anything.\n"
       "-- Author: RWCox -- March 2007\n"
      ) ;
      exit(0) ;
   }

   /** AFNI package setup and logging **/

   mainENTRY("3dSynthesize main"); machdep(); AFNI_logger("3dSynthesize",argc,argv);
   PRINT_VERSION("3dSynthesize") ; AUTHOR("RW Cox") ;
   (void)COX_clock_time() ;  /* anticipating the very end of time */

   /** parse command line options **/

   iarg = 1 ;
   while( iarg < argc ){

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

      /** bozo-ific user **/

      ERROR_exit("Unknown option: %s",argv[iarg]) ;
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

   if( !nelim ){
     clab = NI_get_attribute( nelmat , "ColumnLabels" ) ;
     if( clab == NULL )
       ERROR_exit("-matrix is missing 'ColumnLabels' attribute!?") ;
     clab_sar = NI_decode_string_list( clab , ";" ) ;
     if( clab_sar == NULL || clab_sar->num < ncol )
       ERROR_exit("-matrix 'ColumnLabels' badly formatted!?") ;

     cgrp = NI_get_attribute( nelmat , "ColumnGroups" ) ;
     if( cgrp == NULL )
       ERROR_exit("-matrix is missing 'ColumnGroups' attribute!?") ;
     cgrp_sar = NI_decode_string_list( cgrp , ";" ) ;
     if( cgrp_sar == NULL || cgrp_sar->num < ncol )
       ERROR_exit("-matrix 'ColumnGroups' badly formatted!?") ;
     cgrp_val = (int *)malloc(sizeof(int)*ncol) ;
     for( ii=0 ; ii < ncol ; ii++ )
       cgrp_val[ii] = (int)strtod(cgrp_sar->str[ii],NULL) ;

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
     ERROR_exit("-matrix file stored will illegal data type!") ;
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

   INFO_message("Index list: %d nonzero entries",nilist) ;
   fprintf(stderr,"++ ") ;
   for( ii=0 ; ii < ncol ; ii++ ) if( ilist[ii] ) fprintf(stderr," %d",ii) ;
   fprintf(stderr,"\n") ;

   if( dry ){
     INFO_message("3dSynthesize exits: -dry option was given") ;
     exit(0) ;
   }

   /*-- create empty output 3D+time dataset --*/

   outset = EDIT_empty_copy( inset ) ;
   EDIT_dset_items( outset ,
                      ADN_prefix    , prefix ,
                      ADN_brick_fac , NULL ,
                      ADN_datum_all , MRI_float ,
                      ADN_nvals     , nrow ,
                      ADN_ntt       , nrow ,
                      ADN_ttdel     , dt ,
                      ADN_tunits    , UNITS_SEC_TYPE ,
                      ADN_type      , HEAD_ANAT_TYPE ,
                      ADN_func_type , ANAT_OMRI_TYPE ,
                    ADN_none ) ;

   if( THD_is_file(DSET_HEADNAME(outset)) )
     ERROR_exit("Output dataset already exists: %s\n",DSET_HEADNAME(outset)) ;

   tross_Copy_History( outset , inset ) ;
   tross_Make_History( "3dSynthesize" , argc , argv , outset ) ;

   nxyz = DSET_NVOX(inset) ;

   /* create bricks (will be filled with zeros) */

   for( jj=0 ; jj < nrow ; jj++ )
     EDIT_substitute_brick( outset , jj , MRI_float , NULL ) ;

   tsar = (float *)calloc(sizeof(float),nrow+1) ;
   cfar = (float *)calloc(sizeof(float),ncol+1) ;

   for( kk=0 ; kk < nxyz ; kk++ ){   /* kk = voxel index */

      /* get kk-th voxel's coefficient array into cfar */

      (void)THD_extract_array( kk , inset , 0 , cfar ) ;
      for( ii=0 ; ii < ncol && cfar[ii] == 0.0f ; ii++ ) ; /* nada */
      if( ii == ncol ) continue ;   /** coefficients are all zero! */

      /* add up matrix columns with the given weights */

      for( jj=0 ; jj < nrow ; jj++ ){
        tsar[jj] = 0.0f ;
        for( ii=0 ; ii < ncol ; ii++ )
          if( ilist[ii] ) tsar[jj] += cfar[ii]*clist[ii][jj] ;
      }

      /* put result time series into output dataset */

      THD_insert_series( kk , outset ,
                         nrow , MRI_float , tsar , 1 ) ;
   }

   /** write output, and let freedom ring!! **/

   DSET_write(outset) ;
   WROTE_DSET(outset) ;
   INFO_message("CPU time=%.2f s ; Elapsed=%.2f s",
                COX_cpu_time(),COX_clock_time()  ) ;
   exit(0) ;
}
