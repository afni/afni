/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  This software is Copyright 1994-6 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application.  The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/

#define NEED_EDIT_HELP
#include "editvol.h" 

#define MAIN
#define MEGA  1048576  /* 2^20 */

#undef USE_GNU_MALLOC
#undef MALLOC_TRACE

#ifdef AFNI_DEBUG
#  define USE_TRACING
#  define PRINT_TRACING
#else
#  undef  USE_TRACING
#endif
#include "dbtrace.h"

#ifndef myXtFree
#define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

/*-------------------------- global data --------------------------*/

/*** combination flags:  mean, mean of nonzeros, max, ... ***/

#define CFLAG_MEAN   1  /* the default */
#define CFLAG_NZMEAN 2
#define CFLAG_MMAX   3
#define CFLAG_COUNT  4
#define CFLAG_AMAX   5
#define CFLAG_SMAX   6
#define CFLAG_ORDER  7
#define CFLAG_FISHER 8  /* 08 Aug 1996 */

#define THFLAG_NONE  0    /* 29 Aug 1996: added the ability to merge */
#define THFLAG_FICO  71   /*  threshold data as well as intensities  */

#define TANH(z)   tanh(z)  /* for the Fisher transformations */
#define ATANH(z) atanh(z)

/*** variables to hold status from command line options ***/

static EDIT_options MRG_edopt ;
static int MRG_have_edopt = 0 ;

static int   MRG_hits_g       = 0 ;
static int   MRG_cflag_g      = CFLAG_MEAN ;
static int   MRG_keepthr      = 0 ;
static float MRG_clust_rmm_g  = 0.0 ;
static float MRG_clust_vmul_g = 0.0 ;
static int   MRG_datum        = ILLEGAL_TYPE ;
static int   MRG_thdatum      = ILLEGAL_TYPE ;
static int   MRG_be_quiet     = 0 ;
static int   MRG_cflag_gthr   = THFLAG_NONE ;  /* 29 Aug 1996 */

static char  MRG_output_session[THD_MAX_NAME]   = "./" ;
static char  MRG_output_prefix [THD_MAX_PREFIX] = "mrg" ;
#if 0
static char  MRG_output_label  [THD_MAX_LABEL]  = "\0" ;
#endif

/*--------------------------- prototypes ---------------------------*/
int MRG_read_opts( int , char ** ) ;
void MRG_Syntax(void) ;

/*--------------------------------------------------------------------
   read the arguments, load the global MRG_ variables, and return
   the index of the argument that we stopped at (that is, the first
   input filename)
----------------------------------------------------------------------*/

#ifdef MERGE_DEBUG
#  define DUMP1 fprintf(stderr,"ARG: %s\n",argv[nopt])
#  define DUMP2 fprintf(stderr,"ARG: %s %s\n",argv[nopt],argv[nopt+1])
#  define DUMP3 fprintf(stderr,"ARG: %s %s %s\n",argv[nopt],argv[nopt+1],argv[nopt+2])
#else
#  define DUMP1
#  define DUMP2
#  define DUMP3
#endif

int MRG_read_opts( int argc , char * argv[] )
{
   int nopt = 1 ;
   float val ;
   int  ival ;

ENTRY("MRG_read_opts") ;

   INIT_EDOPT( &MRG_edopt ) ;

   while( nopt < argc && argv[nopt][0] == '-' ){

      /**** check editing options ****/

      ival = EDIT_check_argv( argc , argv , nopt , &MRG_edopt ) ;
      if( ival > 0 ){
         nopt += ival ; MRG_have_edopt = 1 ;
         continue ;
      }

      /**** -quiet ****/

      if( strncmp(argv[nopt],"-quiet",6) == 0 ){
         MRG_be_quiet = 1 ;
         nopt++ ; continue ;
      }


      /**** -keepthr ****/

      if( strncmp(argv[nopt],"-keepthr",6) == 0 ){
         MRG_keepthr = 1 ;
         nopt++ ; continue ;
      }

      /**** -datum type ****/

      if( strncmp(argv[nopt],"-datum",6) == 0 ){
DUMP2 ;
         if( ++nopt >= argc ){
            fprintf(stderr,"need an argument after -datum!\n") ; exit(1) ;
         }

         if( strcmp(argv[nopt],"short") == 0 ){
            MRG_datum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            MRG_datum = MRI_float ;
         } else if( strcmp(argv[nopt],"byte") == 0 ){
            MRG_datum = MRI_byte ;
         } else if( strcmp(argv[nopt],"complex") == 0 ){  /* not listed help */
            MRG_datum = MRI_complex ;
         } else {
            fprintf(stderr,"-datum of type '%s' is not supported in 3dmerge!\n",
                    argv[nopt] ) ;
            exit(1) ;
         }
         nopt++ ; continue ;  /* go to next arg */
      }

      /**** -thdatum type ****/

      if( strncmp(argv[nopt],"-thdatum",6) == 0 ){
DUMP2 ;
         if( ++nopt >= argc ){
            fprintf(stderr,"need an argument after -thdatum!\n") ; exit(1) ;
         }

         if( strcmp(argv[nopt],"short") == 0 ){
            MRG_thdatum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            MRG_thdatum = MRI_float ;
         } else if( strcmp(argv[nopt],"byte") == 0 ){
            MRG_thdatum = MRI_byte ;
         } else {
            fprintf(stderr,"-thdatum of type '%s' is not supported in 3dmerge!\n",
                    argv[nopt] ) ;
            exit(1) ;
         }
         MRG_keepthr = 1 ;    /* -thdatum is meaningless without this */
         nopt++ ; continue ;  /* go to next arg */
      }

      /**** -ghits count ****/

      if( strncmp(argv[nopt],"-ghits",6) == 0 ){
DUMP2 ;
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -ghits!\n") ; exit(1) ;
         }
         MRG_hits_g = strtod( argv[nopt++] , NULL ) ;
         if( MRG_hits_g <= 0 ){
            fprintf(stderr,"illegal value after -ghits\n") ;
            exit(1) ;
         }
         continue ;
      }

      /**** -gclust rmm vmul ****/

      if( strncmp(argv[nopt],"-gclust",6) == 0 ){
DUMP3 ;
         nopt++ ;
         if( nopt+1 >= argc ){
            fprintf(stderr,"need 2 arguments after -gclust!\n") ;
            exit(1) ;
         }
         MRG_clust_rmm_g  = strtod( argv[nopt++] , NULL ) ;
         MRG_clust_vmul_g = strtod( argv[nopt++] , NULL ) ;
         if( MRG_clust_rmm_g <= 0 || MRG_clust_vmul_g <= 0 ){
            fprintf(stderr,"illegal value after -gclust\n") ;
            exit(1) ;
         }
         continue ;
      }

      /**** -session dirname ****/

      if( strncmp(argv[nopt],"-session",6) == 0 ){
DUMP2 ;
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -session!\n") ;
            exit(1) ;
         }
         MCW_strncpy( MRG_output_session , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ){
DUMP2 ;
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -prefix!\n") ;
            exit(1) ;
         }
         MCW_strncpy( MRG_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

#if 0
      /**** -label string ****/

      if( strncmp(argv[nopt],"-label",6) == 0 ){
DUMP2 ;
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"need argument after -label!\n") ;
            exit(1) ;
         }
         MCW_strncpy( MRG_output_label , argv[nopt++] , THD_MAX_LABEL ) ;
         continue ;
      }
#endif

      /**** -gmean ****/

      if( strncmp(argv[nopt],"-gmean",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_MEAN ;
         nopt++ ; continue ;
      }

      /**** -gfisher ****/

      if( strncmp(argv[nopt],"-gfisher",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_FISHER ;
         nopt++ ; continue ;
      }

      /**** -tgfisher (29 Aug 1996) ****/

      if( strncmp(argv[nopt],"-tgfisher",6) == 0 ){
DUMP1 ;
         MRG_cflag_gthr = THFLAG_FICO ;
         nopt++ ; continue ;
      }

      /**** -gnzmean ****/

      if( strncmp(argv[nopt],"-gnzmean",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_NZMEAN ;
         nopt++ ; continue ;
      }

      /**** -gmax ****/

      if( strncmp(argv[nopt],"-gmax",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_MMAX ;
         nopt++ ; continue ;
      }

      /**** -gamax ****/

      if( strncmp(argv[nopt],"-gamax",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_AMAX ;
         nopt++ ; continue ;
      }

      /**** -gsmax ****/

      if( strncmp(argv[nopt],"-gsmax",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_SMAX ;
         nopt++ ; continue ;
      }

      /*** -gcount ****/

      if( strncmp(argv[nopt],"-gcount",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_COUNT ;
         nopt++ ; continue ;
      }

      /*** -gorder ****/

      if( strncmp(argv[nopt],"-gorder",6) == 0 ){
DUMP1 ;
         MRG_cflag_g = CFLAG_ORDER ;
         nopt++ ; continue ;
      }

      /**** unknown switch ****/

      fprintf(stderr,"unrecognized option %s\n",argv[nopt]) ;
      exit(1) ;

   }  /* end of loop over options */

   /*** cleanup ***/

#if 0
   if( strlen(MRG_output_label) == 0 ){
      MCW_strncpy( MRG_output_label , MRG_output_prefix , THD_MAX_LABEL ) ;
   }
#endif

   RETURN( nopt );
}

/*------------------------------------------------------------------*/

void MRG_Syntax(void)
{
   printf(
    "Edit and/or merge 3D datasets\n"
    "Usage: 3dmerge [options] datasets ...\n"
    "where the options are:\n"
   ) ;

   printf( "%s\n" , EDIT_options_help ) ;

   printf(
    "  -datum type = Coerce the output data to be stored as the given type,\n"
    "                  which may be byte, short, or float.\n"
    "          N.B.: Byte data cannot be negative.  If this datum type is chosen,\n"
    "                  any negative values in the edited and/or merged dataset\n"
    "                  will be set to zero.\n"
    "  -keepthr    = When using 3dmerge to edit exactly one dataset of a\n"
    "                  functional type with a threshold statistic attached,\n"
    "                  normally the resulting dataset is of the 'fim'\n"
    "                  (intensity only) type.  This option tells 3dmerge to\n"
    "                  copy the threshold data (unedited in any way) into\n"
    "                  the output dataset.\n"
    "          N.B.: This option is ignored if 3dmerge is being used to\n"
    "                  combine 2 or more datasets.\n"
    "          N.B.: The -datum option has no effect on the storage of the\n"
    "                  threshold data.  Instead use '-thdatum type'.\n"
    "\n"

    "MERGING OPTIONS APPLIED TO FORM THE OUTPUT DATASET:\n"
    " [That is, different ways to combine results. The]\n"
    " [following '-g' options are mutually exclusive! ]\n"
    "  -gmean     = Combine datasets by averaging intensities\n"
    "                 (including zeros) -- this is the default\n"
    "  -gnzmean   = Combine datasets by averaging intensities\n"
    "                 (not counting zeros)\n"
    "  -gmax      = Combine datasets by taking max intensity\n"
    "                 (e.g., -7 and 2 combine to 2)\n"
    "  -gamax     = Combine datasets by taking max absolute intensity\n"
    "                 (e.g., -7 and 2 combine to 7)\n"
    "  -gsmax     = Combine datasets by taking max signed intensity\n"
    "                 (e.g., -7 and 2 combine to -7)\n"
    "  -gcount    = Combine datasets by counting number of 'hits' in\n"
    "                  each voxel (see below for defintion of 'hit')\n"
    "  -gorder    = Combine datasets in order of input:\n"
    "                * If a voxel is nonzero in dataset #1, then\n"
    "                    that value goes into the voxel.\n"
    "                * If a voxel is zero in dataset #1 but nonzero\n"
    "                    in dataset #2, then the value from #2 is used.\n"
    "                * And so forth: the first dataset with a nonzero\n"
    "                    entry in a given voxel 'wins'\n"
    "  -gfisher   = Takes the arctanh of each input, averages these,\n"
    "                  and outputs the tanh of the average.  If the input\n"
    "                  datum is 'short', then input values are scaled by\n"
    "                  0.0001 and output values by 10000.  This option\n"
    "                  is for merging bricks of correlation coefficients.\n"

    "\n"
    "MERGING OPERATIONS APPLIED TO THE THRESHOLD DATA:\n"
    " [That is, different ways to combine the thresholds.  If none of these ]\n"
    " [are given, the thresholds will not be merged and the output dataset  ]\n"
    " [will not have threshold data attached.  Note that the following '-tg']\n"
    " [command line options are mutually exclusive, but are independent of  ]\n"
    " [the '-g' options given above for merging the intensity data values.  ]\n"
    "  -tgfisher  = This option is only applicable if each input dataset\n"
    "                  is of the 'fico' or 'fith' types -- functional\n"
    "                  intensity plus correlation or plus threshold.\n"
    "                  (In the latter case, the threshold values are\n"
    "                  interpreted as correlation coefficients.)\n"
    "                  The correlation coefficients are averaged as\n"
    "                  described by -gfisher above, and the output\n"
    "                  dataset will be of the fico type if all inputs\n"
    "                  are fico type; otherwise, the output datasets\n"
    "                  will be of the fith type.\n"
    "         N.B.: The difference between the -tgfisher and -gfisher\n"
    "                  methods is that -tgfisher applies to the threshold\n"
    "                  data stored with a dataset, while -gfisher\n"
    "                  applies to the intensity data.  Thus, -gfisher\n"
    "                  would normally be applied to a dataset created\n"
    "                  from correlation coefficients directly, or from\n"
    "                  the application of the -1thtoin option to a fico\n"
    "                  or fith dataset.\n"
    "\n"
    "OPTIONAL WAYS TO POSTPROCESS THE COMBINED RESULTS:\n"
    " [May be combined with the above methods.]\n"
    " [Any combination of these options may be used.]\n"
    "  -ghits count     = Delete voxels that aren't !=0 in at least\n"
    "                       count datasets (!=0 is a 'hit')\n"
    "  -gclust rmm vmul = Form clusters with connection distance rmm\n"
    "                       and clip off data not in clusters of\n"
    "                       volume at least vmul microliters\n"
    "\n"
    "The '-g' and '-tg' options apply to the entire group of input datasets.\n"
    "\n"

    "OPTIONS THAT CONTROL THE NAMES OF THE OUTPUT DATASET:\n"
    "  -session dirname  = write output into given directory (default=./)\n"
    "  -prefix  pname    = use 'pname' for the output directory prefix\n"
    "                       (default=mrg)\n"
#if 0
    "  -label   string   = use 'string' for the label in the output\n"
    "                       dataset (the label is used for switching\n"
    "                       between datasets in AFNI)\n"
#endif
    "\n"

    "NOTES:\n"
    " **  If only one dataset is read into this program, then the '-g'\n"
    "       options do not apply, and the output dataset is simply the\n"
    "       '-1' options applied to the input dataset (i.e., edited).\n"
    " **  A merged output dataset is ALWAYS of the intensity-only variety.\n"
    " **  Complex-valued datasets cannot be merged.\n"
    " **  This program cannot handle time-dependent datasets.\n"
    " **  Note that the input datasets are specified by their .HEAD files,\n"
    "       but that their .BRIK files must exist also!\n"
   ) ;
   exit(0) ;
}

/*------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int file_num , first_file , nx,ny,nz , nxyz , ii , num_dset ,
       file_count , ptmin , iclu,nclu , edit_type , ival , tval ;
   float dx,dy,dz , fac , dxyz , rmm,vmul ;
   THD_3dim_dataset * dset=NULL , * new_dset=NULL ;
   short * gnum=NULL ;
   float * gfim=NULL , * tfim=NULL , * ggfim=NULL , * ttfim=NULL ;
   int     datum ;
   MCW_cluster_array * clar ;
   float fimfac , fimfacinv , first_fimfac , thrfac ;
   int   output_datum , output_thdatum ;
   int   input_datum  , input_thdatum , first_datum ;

   float thr_stataux[MAX_STAT_AUX] ;
   int   num_fico ;

ENTRY("3dmerge MAIN") ;

   /*** read input options ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) MRG_Syntax() ;

   first_file = MRG_read_opts( argc , argv ) ;
   file_count = argc - first_file ;            /* number of datasets input */

   if( ! MRG_be_quiet )
      printf("3dmerge: edit and combine 3D datasets, by R.W. Cox (rwcox@mcw.edu)\n") ;

   if( first_file < 1 || first_file >= argc ){
      fprintf(stderr,"*** ILLEGAL COMMAND LINE ***\n") ; exit(1) ;
   }
   
   /* check for existence of each input data set. */   /* 09 December 1996 */
   for (file_num = first_file;  file_num < argc;  file_num++)
     {
       dset = THD_open_one_dataset( argv[file_num] ) ;
       if( ! ISVALID_3DIM_DATASET(dset) )
	 {
	   fprintf(stderr,"*** cannot open dataset %s\n",argv[file_num]) ; 
	   exit(1) ;
         }
	 THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
     }
       
   /* read first dataset */

   dset = THD_open_one_dataset( argv[first_file] ) ;
   if( ! ISVALID_3DIM_DATASET(dset) ){
      fprintf(stderr,"*** Unable to open first dataset %s\n",argv[first_file]) ;
      exit(1) ;
   }

   if( DSET_NUM_TIMES(dset) > 1 ){
      fprintf(stderr,"*** Unable to merge time-dependent datasets\n") ;
      exit(1) ;
   }

   /* get the dimensions */

   nx = dset->daxes->nxx ;
   ny = dset->daxes->nyy ;
   nz = dset->daxes->nzz ; nxyz = nx*ny*nz ;

   dx = fabs(dset->daxes->xxdel) ;
   dy = fabs(dset->daxes->yydel) ;
   dz = fabs(dset->daxes->zzdel) ;

   nice(5) ;

   /*******************************************************************/
   /****      if only one file, edit it, modify its names ...      ****/
   /****      then write the modified dataset to disk              ****/

   if( file_count == 1 ){

      ival        = DSET_PRINCIPAL_VALUE(dset) ;
      input_datum = DSET_BRICK_TYPE(dset,ival) ;
      if( MRG_datum >= 0 ) output_datum = MRG_datum ;
      else                 output_datum = input_datum ;

      new_dset = EDIT_empty_copy( dset ) ;

      EDIT_dset_items( new_dset ,
                          ADN_prefix , MRG_output_prefix ,
                          ADN_label1 , MRG_output_prefix ,
                          ADN_directory_name , MRG_output_session ,
                       ADN_none ) ;
      strcat( new_dset->self_name , "(ED)" ) ;

      if( ! MRG_keepthr && new_dset->dblk->nvals > 1 )
         EDIT_dset_items( new_dset ,
                             ADN_nvals , 1 ,
                             ADN_func_type , FUNC_FIM_TYPE ,
                          ADN_none ) ;

      if( MRG_keepthr && ISFUNC(new_dset) && FUNC_HAVE_THR(new_dset->func_type) ){
         ii            = FUNC_ival_thr[dset->func_type] ;
         input_thdatum = DSET_BRICK_TYPE(dset,ii) ;
         if( MRG_thdatum >= 0 ) output_thdatum = MRG_thdatum ;
         else                   output_thdatum = input_thdatum ;
      } else {
         output_thdatum = input_thdatum = ILLEGAL_TYPE ;
      }

      if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
         fprintf(stderr,
                 "*** Output file %s already exists -- cannot continue!\n",
                 new_dset->dblk->diskptr->header_name ) ;
         exit(1) ;
      }

      if( ! MRG_be_quiet ){
         printf("-- editing input dataset in memory (%.1f MB)",
                ((double)dset->dblk->total_bytes) / MEGA ) ;
         fflush(stdout) ;
      } else {
STATUS("editing input dataset now") ;
      }

      EDIT_one_dataset( dset , &MRG_edopt ) ;  /* all the real work */

      if( ! MRG_be_quiet ){ printf(".\n") ; fflush(stdout) ; }

      /** Coerce the output data type into a new brick, if needed **/

      ival = DSET_PRINCIPAL_VALUE(dset) ;
      ii   = DSET_PRINCIPAL_VALUE(new_dset) ;

      if( input_datum == output_datum ){

         /** Attach the brick of the input dataset to the brick of the output.  **/
         /** (This isn't exactly kosher, but we are exiting almost immediately) **/

STATUS("connecting edited input to be output") ;

         mri_fix_data_pointer( DSET_ARRAY(dset,ival) , DSET_BRICK(new_dset,ii) ) ;

         DSET_BRICK_FACTOR(new_dset,ii) = DSET_BRICK_FACTOR(dset,ival) ;

      } else {

         /** Must create a new brick and do the conversion **/

         void * dfim , * efim ;
         float etop ;

         if( ! MRG_be_quiet ){
            printf("-- coercing output datum to be %s\n",
                   MRI_TYPE_name[output_datum]);
         } else {
STATUS("coercing output brick from edited input brick") ;
         }

         efim = DSET_ARRAY(dset,ival) ;
         dfim = (void *) XtMalloc( mri_datum_size(output_datum) * nxyz ) ;

         fimfac = EDIT_coerce_autoscale( nxyz , input_datum  , efim ,
                                                output_datum , dfim  ) ;

         DSET_BRICK_FACTOR(new_dset,ii) = (fimfac != 0.0 && fimfac != 1.0)
                                          ? 1.0/fimfac : 0.0 ;

         EDIT_substitute_brick( new_dset , ii , output_datum , dfim ) ;
         mri_free( DSET_BRICK(dset,ival) ) ;
      }

      /** Now do the threshold data **/

      if( output_thdatum >= 0 ){

         ival = FUNC_ival_thr[    dset->func_type] ;
         ii   = FUNC_ival_thr[new_dset->func_type] ;

         if( input_thdatum == output_thdatum ){

STATUS("connecting input and output thresholds") ;

            mri_fix_data_pointer( DSET_ARRAY(dset,ival),DSET_BRICK(new_dset,ii) ) ;

            DSET_BRICK_FACTOR(new_dset,ii) = DSET_BRICK_FACTOR(dset,ival) ;

         } else {
            void * dfim , * efim ;

            if( ! MRG_be_quiet ){
               printf("-- coercing threshold datum to be %s\n",
                      MRI_TYPE_name[output_thdatum]);
            } else {
STATUS("coercing output threshold brick from input threshold brick") ;
            }

            efim = DSET_ARRAY(dset,ival) ;
            dfim = (void *) XtMalloc( mri_datum_size(output_thdatum) * nxyz ) ;

            switch( output_thdatum ){
               default: fprintf(stderr,"** illegal output_thdatum = %d\n",
                                output_thdatum);
               exit(1) ;

               case MRI_float:
                  fimfacinv = 0.0 ;
                  fimfac    = DSET_BRICK_FACTOR(dset,ival) ;
                  if( fimfac == 0.0 ){
                     fimfac = (input_thdatum == MRI_short)
                               ? 1.0/FUNC_scale_short[dset->func_type]
                               : (input_thdatum == MRI_byte)
                               ? 1.0/FUNC_scale_byte[dset->func_type] : 0.0 ;
                  }
               break ;

               case MRI_short:
                  if( input_datum == MRI_float ){
                     fimfac    = FUNC_scale_short[new_dset->func_type] ;
                     fimfacinv = 1.0 / fimfac ;
                  } else if( input_datum == MRI_byte ){
                     fimfac    = ((float)FUNC_scale_short[new_dset->func_type])
                                / FUNC_scale_byte[new_dset->func_type] ;
                     fimfacinv = 1.0 / FUNC_scale_short[new_dset->func_type] ;
                  } else {
                     fprintf(stderr,"** illegal input_thdatum = %d\n",input_thdatum);
                     exit(1) ;
                  }
               break ;

               case MRI_byte:
                  if( input_datum == MRI_float ){
                     fimfac    = FUNC_scale_byte[new_dset->func_type] ;
                     fimfacinv = 1.0 / fimfac ;
                  } else if( input_datum == MRI_short ){
                     fimfac    = ((float)FUNC_scale_byte[new_dset->func_type])
                                / FUNC_scale_short[new_dset->func_type] ;
                     fimfacinv = 1.0 / FUNC_scale_byte[new_dset->func_type] ;
                  } else {
                     fprintf(stderr,"** illegal input_thdatum = %d\n",input_thdatum);
                     exit(1) ;
                  }
               break ;
            }

            EDIT_coerce_scale_type( nxyz , fimfac ,
                                    DSET_BRICK_TYPE(dset,ival),efim ,
                                    output_thdatum,dfim ) ;

            DSET_BRICK_FACTOR(new_dset,ii) = fimfacinv ;
            EDIT_substitute_brick( new_dset , ii , output_thdatum , dfim ) ;
            mri_free( DSET_BRICK(dset,ival) ) ;
         }
      }

      if( ! MRG_be_quiet )
         printf("-- Writing edited dataset in files\n"
                "   %s and %s\n",
                new_dset->dblk->diskptr->header_name ,
                new_dset->dblk->diskptr->brick_name    ) ;

      THD_load_statistics( new_dset ) ;
      THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
      exit(0) ;
   }

   /************************************************************************/
   /********         more than one input dataset --> merger         ********/

   /* make an empty copy of the first dataset, then modify it */

   new_dset = EDIT_empty_copy( dset ) ;
   ival     = DSET_PRINCIPAL_VALUE(dset) ;

   if( MRG_datum >= 0 ) output_datum = MRG_datum ;
   else                 output_datum = DSET_BRICK_TYPE(dset,ival) ;

   EDIT_dset_items( new_dset ,
                       ADN_prefix , MRG_output_prefix ,
                       ADN_label1 , MRG_output_prefix ,
                       ADN_directory_name , MRG_output_session ,
                    ADN_none ) ;
   strcat( new_dset->self_name , "(MG)" ) ;

   /* 29 Aug 1996: change the dataset type, depending on the merger type */

   switch( MRG_cflag_gthr ){
       default:
          EDIT_dset_items( new_dset , ADN_nvals , 1 , ADN_none ) ;
          if( ISFUNC(dset) )
             EDIT_dset_items( new_dset , ADN_func_type,FUNC_FIM_TYPE , ADN_none ) ;
       break ;

       case THFLAG_FICO:  /* do nothing to the dataset now */
          num_fico = 0 ;
       break ;
   }

   if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
      fprintf(stderr,
              "*** Output file %s already exists -- cannot continue!\n",
              new_dset->dblk->diskptr->header_name ) ;
      exit(1) ;
   }

   if( ! MRG_be_quiet && MRG_keepthr )
      printf("-- ignoring -keepthr option\n") ;

   /* make space for the merger computations */

   tfim = (float *) XtMalloc( sizeof(float) * nxyz ) ;  /* dataset copy */
   gfim = (float *) XtMalloc( sizeof(float) * nxyz ) ;  /* results */
   gnum = (short *) XtMalloc( sizeof(short) * nxyz ) ;  /* counts */
   for( ii=0 ; ii < nxyz ; ii++ ) gfim[ii] = 0.0 ;      /* initialize */
   for( ii=0 ; ii < nxyz ; ii++ ) gnum[ii] = 0 ;

   /* 29 Aug 1996: make space for merger of thresholds, if desired */

   if( MRG_cflag_gthr != THFLAG_NONE ){
      ttfim = (float *) XtMalloc( sizeof(float) * nxyz ) ;  /* thresh copy */
      ggfim = (float *) XtMalloc( sizeof(float) * nxyz ) ;  /* thresh results */
      for( ii=0 ; ii < nxyz ; ii++ ) ggfim[ii] = 0.0 ;      /* initialize */

      for( ii=0 ; ii < MAX_STAT_AUX ; ii++ ) thr_stataux[ii] = 0 ;
   }

   if( ! MRG_be_quiet ){
      float nbytes = (2.0*sizeof(float)+sizeof(short))*nxyz ;
      if( MRG_cflag_gthr != THFLAG_NONE ) nbytes += 2.0*sizeof(float)*nxyz ;
      printf("-- allocated %.1f MB scratch memory\n", nbytes/MEGA ) ;
   }

   /***--- read datasets, edit them, add them into gfim and gnum ---***/

   num_dset = 0 ;
   for( file_num=first_file; file_num < argc ; file_num++ ){

      /** don't need to re-read 1st dataset **/

      if( file_num > first_file ){
         dset = THD_open_one_dataset( argv[file_num] ) ;
         if( ! ISVALID_3DIM_DATASET(dset) ){
            fprintf(stderr,"*** cannot open dataset %s\n",argv[file_num]) ; exit(1) ;
         }
      }

      /* check for dimensional mismatch */

      if( dset->daxes->nxx != nx ||
          dset->daxes->nyy != ny || dset->daxes->nzz != nz ){

         fprintf(stderr,"*** dataset size mismatch at file %s\n",
                 argv[file_num] ) ;
         exit(1) ;
      }

      if( DSET_NUM_TIMES(dset) > 1 ){                              /* no time     */
         fprintf(stderr,                                           /* dependence! */
                 "*** cannot use time-dependent dataset %s\n",argv[file_num]) ;
         exit(1) ;
      }

      /* check for dataset type, if needed for the merging operations ordered */

      if( MRG_cflag_gthr == THFLAG_FICO ){

         if( !ISFUNC(dset) ){
            fprintf(stderr,
                  "*** dataset from file %s is anatomical using '-tgfisher'!\n",
                argv[file_num] ) ;
            exit(1) ;
         }

         switch( dset->func_type ){
            default:
               fprintf(stderr,
                "*** dataset from file %s is illegal type using '-tgfisher'!\n",
                   argv[file_num] ) ;
            exit(1) ;

            case FUNC_COR_TYPE:   /* add up degrees-of-freedom */
               num_fico ++ ;
               for( ii=0 ; ii < FUNC_need_stat_aux[FUNC_COR_TYPE] ; ii++ )
                 thr_stataux[ii] += dset->stat_aux[ii] ;
            break ;

            case FUNC_THR_TYPE:  /* do nothing */
            break ;
         }
      }

      /* get the control information about this dataset */

      ival  = DSET_PRINCIPAL_VALUE(dset) ;
      datum = DSET_BRICK_TYPE(dset,ival) ;

      if( ! AFNI_GOOD_FUNC_DTYPE(datum) ){
         fprintf(stderr,"*** Illegal datum for 3dmerge: %s in file %s ***\n" ,
                 MRI_TYPE_name[datum] , argv[file_num] ) ;
         exit(1) ;
      }

      if( ! MRG_be_quiet ){
         printf("-- processing file %s" , argv[file_num] ) ;
         fflush(stdout) ;
      }

      /* mess with the input data */

STATUS("loading (and editing?) one dataset") ;

      if( MRG_have_edopt )
         EDIT_one_dataset( dset , &MRG_edopt ) ;  /* some real work */
      else
         THD_load_datablock( dset->dblk , NULL ) ;

      if( ! MRG_be_quiet ){ printf(".") ; fflush(stdout) ; }

      /* copy it into tfim , scaling if needed */

      fimfac = DSET_BRICK_FACTOR(dset,ival) ;          /* normal case */

      if( MRG_cflag_g == CFLAG_FISHER             &&   /* special case */
          DSET_BRICK_TYPE(dset,ival) == MRI_short &&
          (fimfac==0.0 || fimfac==1.0)              ){

         fimfac = 1.0 / FUNC_COR_SCALE_SHORT ;
      }

      if( num_dset == 0 ){
         first_fimfac = fimfac ;  /* save for later */
         first_datum  = datum  ;
      }

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"scaling to floats with factor %g",fimfac) ;
  STATUS(str) ; }
#endif

      EDIT_coerce_scale_type( nxyz , fimfac ,
                              DSET_BRICK_TYPE(dset,ival) , DSET_ARRAY(dset,ival) ,
                              MRI_float , tfim ) ;

      /* 29 Aug 1996: get the threshold data into ttfim , if needed */

      if( MRG_cflag_gthr != THFLAG_NONE && (tval=DSET_THRESH_VALUE(dset)) >= 0 ){

         int thdatum = DSET_BRICK_TYPE(dset,tval) ;

         if( ! AFNI_GOOD_FUNC_DTYPE(thdatum) ){
            fprintf(stderr,"*** Illegal threshold for 3dmerge: %s in file %s ***\n" ,
                    MRI_TYPE_name[thdatum] , argv[file_num] ) ;
            exit(1) ;
         }

         thrfac = DSET_BRICK_FACTOR(dset,tval) ;          /* normal case */

         if( MRG_cflag_gthr == THFLAG_FICO           &&   /* special case */
             DSET_BRICK_TYPE(dset,tval) == MRI_short &&
             (thrfac==0.0 || thrfac==1.0)              ){

            thrfac = 1.0 / FUNC_COR_SCALE_SHORT ;
         }

         EDIT_coerce_scale_type( nxyz , thrfac ,
                                 thdatum , DSET_ARRAY(dset,tval) ,
                                 MRI_float , ttfim ) ;
      }

      THD_delete_3dim_dataset( dset , False ) ; dset = NULL ; /* no longer needed */

      /*** merge tfim into gfim and gnum ***/

STATUS("merging edited data") ;

      if( MRG_cflag_g == CFLAG_MMAX ){
         for( ii=0 ; ii < nxyz ; ii++ ){
            if( tfim[ii] != 0 ){
               gnum[ii]++ ;
               if( tfim[ii] > gfim[ii] ) gfim[ii] = tfim[ii] ;
            }
         }
      } else if( MRG_cflag_g == CFLAG_AMAX ){
         float dab ;
         for( ii=0 ; ii < nxyz ; ii++ ){
            if( tfim[ii] != 0 ){
               gnum[ii]++ ;
               dab = fabs(tfim[ii]) ;
               if( dab > gfim[ii] ) gfim[ii] = dab ;
            }
         }
      } else if( MRG_cflag_g == CFLAG_SMAX ){
         float dab ;
         for( ii=0 ; ii < nxyz ; ii++ ){
            if( tfim[ii] != 0 ){
               gnum[ii]++ ;
               dab = fabs(tfim[ii]) ;
               if( dab > fabs(gfim[ii]) ) gfim[ii] = tfim[ii] ;
            }
         }
      } else if( MRG_cflag_g == CFLAG_ORDER ){
         for( ii=0 ; ii < nxyz ; ii++ ){
            if( tfim[ii] != 0 ){
               gnum[ii]++ ;
               if( gfim[ii] == 0 ) gfim[ii] = tfim[ii] ;
            }
         }
      } else if( MRG_cflag_g == CFLAG_FISHER ){
         for( ii=0 ; ii < nxyz ; ii++ ){
            if( tfim[ii] != 0 ){
               gnum[ii]++ ; gfim[ii] += ATANH(tfim[ii]) ;
            }
         }
      } else {                                /* default = sum up */
         for( ii=0 ; ii < nxyz ; ii++ ){
            if( tfim[ii] != 0 ){
               gnum[ii]++ ; gfim[ii] += tfim[ii] ;
            }
         }
      }

      /* 29 Aug 1996: merge the threshold data, if any */

      if( MRG_cflag_gthr == THFLAG_FICO ){
         for( ii=0 ; ii < nxyz ; ii++ ) ggfim[ii] += ATANH(ttfim[ii]) ;
      }

      if( ! MRG_be_quiet ){ printf(".\n") ; fflush(stdout) ; }

      num_dset++ ;
   }  /* end of combiner loop over datasets */

   myXtFree(tfim) ;                      /* not needed any more */
   if( ttfim != NULL ) myXtFree(ttfim) ;

   /*** if only one dset encountered, some error! ***/

   if( num_dset <= 1 ){
      fprintf(stderr,"*** Only found 1 dataset -- computations aborted!\n") ;
      exit(1) ;
   }

   if( ! MRG_be_quiet ) printf("-- merging results\n") ;

   /*** now, edit the merged dataset:
        cast out voxels that weren't hit enough ***/

   if( MRG_hits_g > 0 ){
      for( ii=0 ; ii < nxyz ; ii++ )
         if( gnum[ii] < MRG_hits_g ) { gfim[ii] = 0 ; gnum[ii] = 0 ; }
   }

   /*** do the averaging as ordered ***/

   switch( MRG_cflag_g ){
      default: break ;

      case CFLAG_COUNT:
         first_fimfac = 0.0 ;
         for( ii=0 ; ii < nxyz ; ii++ ) gfim[ii] = gnum[ii] ;
      break ;

      case CFLAG_MEAN:
         fac = 1.0 / num_dset ;
         for( ii=0 ; ii < nxyz ; ii++ ) gfim[ii] *= fac ;
      break ;

      case CFLAG_NZMEAN:
         for( ii=0 ; ii < nxyz ; ii++ )
            if( gnum[ii] > 0 ) gfim[ii] /= gnum[ii] ;
      break ;

      case CFLAG_FISHER:
         fac = 1.0 / num_dset ;
         for( ii=0 ; ii < nxyz ; ii++ ) gfim[ii] = TANH( fac * gfim[ii] ) ;
      break ;
   }

   /* 29 Aug 1996: clean up the merged threshold, too */

   switch( MRG_cflag_gthr ){
      default: break ;

      case THFLAG_FICO:
         fac = 1.0 / num_dset ;
         for( ii=0 ; ii < nxyz ; ii++ ) ggfim[ii] = TANH( fac * ggfim[ii] ) ;
      break ;
   }

   /**** at this point, don't need the count brick "gnum" anymore;
         "gfim" contains the results we'll eventually write to disk ****/

   myXtFree( gnum ) ;

   /*** if desired, edit the result for cluster size ***/

   rmm   = MRG_clust_rmm_g ;
   vmul  = MRG_clust_vmul_g ;
   dxyz  = dx*dy*dz ;
   ptmin = vmul / dxyz + 0.99 ;

   if( (rmm >= dx || rmm >= dy || rmm >= dz) && ptmin > 1 ){
      if( ! MRG_be_quiet ) printf("-- editing merger for cluster size\n") ;

      clar  = MCW_find_clusters( nx,ny,nz , dx,dy,dz , MRI_float,gfim , rmm ) ;
      nclu  = 0 ;
      if( clar != NULL ){
         for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
            if( clar->clar[iclu] != NULL && clar->clar[iclu]->num_pt < ptmin ){
               KILL_CLUSTER(clar->clar[iclu]) ;
            } else if( clar->clar[iclu] != NULL ){
               nclu++ ;
            }
         }
      }

      if( nclu > 0 ){
         for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
            if( clar->clar[iclu] != NULL && clar->clar[iclu]->num_pt > 0 )
               MCW_cluster_to_vol( nx,ny,nz , MRI_float,gfim , clar->clar[iclu] ) ;
         }
      }
   }

   /*** scan results for non-zero-ositifulness ***/

   for( ii=0 ; ii < nxyz ; ii++ ) if( gfim[ii] != 0 ) break ;
   if( ii == nxyz ){
      fprintf(stderr,
        "*** Merged dataset has no nonzero entries -- will not write\n" ) ;
      exit(0) ;
   }

   /*** attach new data to output brick ***/

   switch( output_datum ){

      default:
         fprintf(stderr,
                 "*** Fatal Error ***\n"
                 "*** Somehow ended up with output_datum = %d\n",output_datum) ;
      exit(1) ;

      case MRI_complex:{
         void * dfim ;
         dfim = (void *) XtMalloc( sizeof(complex) * nxyz ) ;
         EDIT_coerce_type( nxyz , MRI_float,gfim , MRI_complex,dfim ) ;
         myXtFree( gfim ) ;
         EDIT_substitute_brick( new_dset , 0 , MRI_complex , dfim ) ;
         DSET_BRICK_FACTOR(new_dset,0) = 0.0 ;
      }
      break ;

      case MRI_float:
         EDIT_substitute_brick( new_dset , 0 , MRI_float , gfim ) ;
         DSET_BRICK_FACTOR(new_dset,0) = 0.0 ;
      break ;

      case MRI_byte:
      case MRI_short:{
         void * dfim ;
         float gtop ;

         gtop = MCW_vol_amax( nx,ny,nz , MRI_float,gfim ) ;

         if( MRG_cflag_g == CFLAG_FISHER ){
            fimfac = FUNC_COR_SCALE_SHORT ;
         } else if( output_datum == first_datum && first_fimfac > 0.0 ){
            fimfac = 1.0 / first_fimfac ;
         } else {
            fimfac = (gtop > MRI_TYPE_maxval[output_datum] || gtop <= 1.0 )
                     ? MRI_TYPE_maxval[output_datum]/ gtop : 0.0 ;
         }

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"max value in output = %g",gtop) ; STATUS(str) ;
  sprintf(str,"scaling to %ss with factor %g",MRI_TYPE_name[output_datum],fimfac) ;
  STATUS(str) ; }
#endif

         dfim = (void *) XtMalloc( mri_datum_size(output_datum) * nxyz ) ;
         EDIT_coerce_scale_type( nxyz,fimfac , MRI_float,gfim , output_datum,dfim ) ;
         myXtFree( gfim ) ;
         EDIT_substitute_brick( new_dset , 0 , output_datum , dfim ) ;
         DSET_BRICK_FACTOR(new_dset,0) = (fimfac != 0.0) ? 1.0/fimfac : 0.0 ;
      }
      break ;
   }

   /** 29 Aug 1996: attach output threshold, if any **/

   if( MRG_cflag_gthr != THFLAG_NONE ){
      short * dfim ;

      dfim   = (short *) XtMalloc( sizeof(short) * nxyz ) ;
      thrfac = FUNC_COR_SCALE_SHORT ;
      EDIT_coerce_scale_type( nxyz,thrfac , MRI_float,ggfim , MRI_short,dfim ) ;
      myXtFree( ggfim ) ;
      EDIT_substitute_brick( new_dset , DSET_THRESH_VALUE(new_dset) ,
                             MRI_short , dfim ) ;
      DSET_BRICK_FACTOR(new_dset,1) = 1.0/thrfac ;

      /* if all datasets were fico, then output is fico,
         and needs to get the degrees-of-freedom parameters stataux */

      if( num_fico == num_dset ){
         (void) EDIT_dset_items( new_dset , ADN_stat_aux,thr_stataux , ADN_none ) ;

      /* some datasets were fith, so output is fith */

      } else {
          EDIT_dset_items( new_dset , ADN_func_type,FUNC_THR_TYPE , ADN_none ) ;
      }
   }

   /*** write to disk!!! ***/

   if( ! MRG_be_quiet )
      printf("-- Writing merged dataset in files\n"
             "   %s and %s\n",
             new_dset->dblk->diskptr->header_name ,
             new_dset->dblk->diskptr->brick_name    ) ;

   THD_load_statistics( new_dset ) ;
   THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
   exit(0) ;
}
