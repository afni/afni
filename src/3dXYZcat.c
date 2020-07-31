#include "mrilib.h"
#include "thd.h"

/*---------------------------------------------------------------------------
  This program catenates multiple 3D datasets in various directions.
----------------------------------------------------------------------------*/

/*-------------------------- global data --------------------------*/

static THD_3dim_dataset_array *XCAT_dsar  = NULL ;  /* input datasets */
static int                     XCAT_dir   = 1 ;     /* 1=x, 2=y, 3=z */
static int                     XCAT_nbrik = -1 ;    /* # bricks */
static int                     XCAT_verb  = 0 ;     /* verbose? */
static int                     XCAT_datum = -1 ;    /* dataset datum */

#define DSUB(id) DSET_IN_3DARR(XCAT_dsar,(id))

static char XCAT_output_prefix[THD_MAX_PREFIX] = "xyzcat" ;

/*--------------------------- prototypes ---------------------------*/

void XCAT_read_opts( int , char ** ) ;
void XCAT_Syntax(void) ;

/*--------------------------------------------------------------------
   read the arguments, load the global variables
----------------------------------------------------------------------*/

void XCAT_read_opts( int argc , char *argv[] )
{
   int nopt=1 , ii , nerr=0 ;
   THD_3dim_dataset * dset ;

   INIT_3DARR(XCAT_dsar) ;  /* array of datasets */

   while( nopt < argc ){

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ){
        if( ++nopt >= argc ) ERROR_exit("need argument after -prefix!\n") ;

        MCW_strncpy( XCAT_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;

        if( !THD_filename_ok(XCAT_output_prefix) )
          ERROR_exit("Illegal character in -prefix '%s'",XCAT_output_prefix) ;

        continue ;
      }

      /**** -verb ****/

      if( strncmp(argv[nopt],"-verb",5) == 0 ){
        XCAT_verb++ ; nopt++ ; continue ;
      }

      /*** -dir ***/

      if( strncmp(argv[nopt],"-dir",4) == 0 ){
        nopt++ ;
        if( nopt >= argc ) ERROR_exit("need argument after -dir!") ;
        switch( toupper(argv[nopt][0]) ){
          default: ERROR_exit("don't understand '-dir %s'",argv[nopt]) ;
          case 'X': case 'I': XCAT_dir = 1 ; break ;
          case 'Y': case 'J': XCAT_dir = 2 ; break ;
          case 'Z': case 'K': XCAT_dir = 3 ; break ;
        }
        nopt++ ; continue ;
      }

      /**** Garbage of various flavors ****/

      if( strcmp(argv[nopt],"-input") == 0 ){
        nopt++ ;  /* and fall thru */
      } else if( argv[nopt][0] == '-' ){
        ERROR_exit("Unknown option: %s\n",argv[nopt]) ;
      }

      /**** read dataset ****/

      if( XCAT_verb ) INFO_message("Opening dataset %s",argv[nopt]) ;

      dset = THD_open_dataset( argv[nopt++] ) ;
      if( dset == NULL ){
        ERROR_message("Can't open dataset %s",argv[nopt-1]) ; nerr++ ; continue ;
      }
      THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_MALLOC ) ;

      if( XCAT_datum < 0 ) XCAT_datum = DSET_BRICK_TYPE(dset,0) ;
      if( XCAT_nbrik < 0 ) XCAT_nbrik = DSET_NVALS(dset) ;

      /* check stuff */

      if( !DSET_datum_constant(dset) ){
        ERROR_message("Dataset %s doesn't have a constant data type :-(",argv[nopt-1]) ;
        nerr++ ;
      }
      if( DSET_NVALS(dset) != XCAT_nbrik ){
        ERROR_message("Dataset %s has different number of sub-bricks",argv[nopt-1]) ;
        nerr++ ;
      }
      if( DSET_BRICK_TYPE(dset,0) != XCAT_datum ){
        ERROR_message("Dataset %s has a different data type",argv[nopt-1]) ;
        nerr++ ;
      }
      if( THD_need_brick_factor(dset) ){
        ERROR_message("Can't catenated dataset %s that uses brick factors - sorry!",argv[nopt-1]) ;
        nerr++ ;
      }

      ADDTO_3DARR(XCAT_dsar,dset) ;  /* list of datasets */

   }  /* end of loop over command line arguments */

   if( nerr > 0 ) ERROR_exit("Can't continue after above ERROR message%s",
                             (nerr==1) ? "\0" : "s" ) ;
   return ;
}

/*-------------------------------------------------------------------------*/

void XCAT_Syntax(void)
{
   printf(
    "Usage: 3dXYZcat [options] dataset dataset ...\n"
    "* Catenates datasets spatially (for time cat-ing, cf. 3dTcat).\n"
    "* The input datasets must match, in the sense that the pieces\n"
    "  fit together properly (spatially and in time).\n"
    "* Unlike in 3dZcat, all input datasets must be stored with the\n"
    "  same data type (e.g., shorts, floats, ...); also, sub-brick scale\n"
    "  factors are not allowed.  If you need to spatially catenate scaled\n"
    "  short datasets, for example, convert them to float format using\n"
    "  '3dcalc -float', then catenate THOSE datasets.\n"
    "\n"
    "Options:\n"
    "--------\n"
    "  -prefix pname = Use 'pname' for the output dataset prefix name.\n"
    "                    [default prefix = 'xyzcat']\n"
    "  -verb         = Print out some verbositiness as the program proceeds.\n"
    "  -dir Q        = Catenate along direction 'Q', which is one of\n"
    "                    X or Y or Z (synonyms are I or J or K)\n"
    "                  which are the STORAGE directions (not DICOM) of the\n"
    "                  3D grid of the input datasets.\n"
    "                    [default direction = 'X', for no good reason]\n"
    "\n"
    "Command line arguments after the above are taken as input datasets.\n"
    "\n"
    "Notes:\n"
    "------\n"
    "* If the i-th input dataset has dimensions nx[i] X ny[i] X nz[i], then\n"
    "    case Q = X | I ==> all ny[i] and nz[i] must be the same;\n"
    "                       the output dataset has nx = sum{ nx[i] }\n"
    "    case Q = Y | J ==> all nx[i] and nz[i] must be the same;\n"
    "                       the output dataset has ny = sum{ ny[i] }\n"
    "    case Q = Z | K ==> all nx[i] and ny[i] must be the same;\n"
    "                       the output dataset has nz = sum{ nz[i] }\n"
    "* In all cases, the input datasets must have the same number of\n"
    "  sub-bricks (time points) and the same data storage type.\n"
    "* You can use the '3dinfo' program to see the orientation and\n"
    "    grid size of a dataset, to help you decide how to glue your\n"
    "    inputs together.\n"
    "* There must be at least two datasets input (otherwise, the\n"
    "    program doesn't make much sense, now does it?).\n"
    "* This is mostly useful for making side-by-side pictures from\n"
    "    multiple datasets, for edification and elucidation.\n"
    "* If you have some other use for 3dXYZcat, let me know!\n"
    "\n"
    "** Author: RW Cox [Dec 2010] **\n"
   ) ;

   PRINT_COMPILE_DATE ; exit(0) ;
}

/*-------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int ninp , ids , iv ,kz,new_nz, nx,ny,nz,nxy,nxyz ;
   THD_3dim_dataset *new_dset=NULL , *dset=NULL ;
   MRI_IMAGE *outim;
   MRI_IMARR *im_array;

   /*** read input options ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) XCAT_Syntax() ;

   /*-- addto the arglist, if user wants to --*/

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc, &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   mainENTRY("3dZcat main") ; machdep() ; AFNI_logger("3dZcat",argc,argv) ;
   PRINT_VERSION("3dZcat") ;

   XCAT_read_opts( argc , argv ) ;

   /*** create new dataset (empty) ***/

   ninp = XCAT_dsar->num ;
   if( ninp < 2 )
     ERROR_exit("Must have at least 2 input datasets!") ;

   for( ids=0 ; ids < ninp ; ids++ ){
     dset = DSUB(ids) ; if( DSET_TIMESTEP(dset) > 0.0f ) break ;
   }
   if( ids == ninp ) dset = DSUB(0) ; /* fallback position */
   if( XCAT_verb )
     INFO_message("Using %s as 'master' dataset",DSET_HEADNAME(dset)) ;

   new_dset = EDIT_empty_copy( dset ) ; /* make a copy of its header */

   tross_Copy_History( dset , new_dset ) ;
   tross_Make_History( "3dXYZcat" , argc,argv , new_dset ) ;

   EDIT_dset_items( new_dset ,
                      ADN_prefix    , XCAT_output_prefix ,
                      ADN_datum_all , XCAT_datum ,
                      ADN_nsl       , 0 , /* kill time offsets  */
                    ADN_none ) ;

   /* can't re-write existing dataset */

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(new_dset)) )
     ERROR_exit("Output dataset %s already exists!",DSET_HEADNAME(new_dset) ) ;

   THD_force_malloc_type( new_dset->dblk , DATABLOCK_MEM_MALLOC ) ;

   if( XCAT_verb ) INFO_message("Loading input datasets") ;
   for( ids=0 ; ids < ninp ; ids++ ){
     dset = DSUB(ids) ; DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
   }

   /*** Loop over output sub-bricks ***/

   for( iv=0 ; iv < DSET_NVALS(new_dset) ; iv++ ){

      if( XCAT_verb ) ININFO_message("Computing output sub-brick #%d",iv) ;

      INIT_IMARR(im_array) ;
      for( ids=0 ; ids < ninp ; ids++ )
        ADDTO_IMARR( im_array , DSET_BRICK(DSUB(ids),iv) ) ;

      outim = mri_catvol_1D(im_array,XCAT_dir) ;
      if( outim == NULL )
        ERROR_exit("Can't catenate! Dataset dimensions mismatch? :-(") ;

      FREE_IMARR(im_array) ;
      for( ids=0 ; ids < ninp ; ids++ ) DSET_unload_one(DSUB(ids),iv) ;

      if( iv == 0 ){  /* mangle sub-brick xyz dimensions */
        THD_ivec3 iv_nxyz ;
        LOAD_IVEC3( iv_nxyz , outim->nx , outim->ny , outim->nz ) ;
        EDIT_dset_items( new_dset , ADN_nxyz , iv_nxyz , ADN_none ) ;
      }

      EDIT_substitute_brick( new_dset,iv, XCAT_datum,mri_data_pointer(outim) ) ;
      mri_clear_data_pointer(outim) ; mri_free(outim) ;

   } /* end of loop over output sub-bricks */

   /*-- write dataset --*/

   for( ids=0 ; ids < ninp ; ids++ ) DSET_delete( DSUB(ids) ) ;
   DSET_write(new_dset) ;
   WROTE_DSET(new_dset) ;
   exit(0) ;
}
