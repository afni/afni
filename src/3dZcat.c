#include "mrilib.h"
#include "thd.h"

/*---------------------------------------------------------------------------
  This program catenates multiple 3D datasets in the slice direction.
  -- RWCox - 08 Aug 2001
----------------------------------------------------------------------------*/

#ifndef myXtFree
#   define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

/*-------------------------- global data --------------------------*/

static THD_3dim_dataset_array * ZCAT_dsar  = NULL ;  /* input datasets */
static int                      ZCAT_nxy   = -1 ;    /* # voxels/slice */
static int                      ZCAT_nbrik = -1 ;    /* # bricks */
static int                      ZCAT_verb  = 0 ;     /* verbose? */
static int                      ZCAT_type  = -1 ;    /* dataset type */
static int                      ZCAT_datum = -1 ;    /* dataset datum */
static int                      ZCAT_fscale = 0 ;
static int                      ZCAT_gscale = 0 ;
static int                      ZCAT_nscale = 0 ;
static int                      ZCAT_frugal = 0 ;    /* 05 Apr 2006 */

#define DSUB(id) DSET_IN_3DARR(ZCAT_dsar,(id))

static char ZCAT_output_prefix[THD_MAX_PREFIX] = "zcat" ;
static Boolean write_output = False;  /* 21 Jun 2006 [dg] -force rewrite as in 3drefit by rickr */
static Boolean NIFTI_mode = False;    /* saving NIFTI output */
static int cmode = COMPRESS_NOFILE;   /* check compression mode for NIFTI separately */

/*--------------------------- prototypes ---------------------------*/

void ZCAT_read_opts( int , char ** ) ;
void ZCAT_Syntax(void) ;

/*--------------------------------------------------------------------
   read the arguments, load the global variables
----------------------------------------------------------------------*/

void ZCAT_read_opts( int argc , char * argv[] )
{
   int nopt = 1 , ii ;
   THD_3dim_dataset * dset ;

   INIT_3DARR(ZCAT_dsar) ;  /* array of datasets */

   while( nopt < argc ){

      /**** -frugal [05 Apr 2006] ****/

      if( strncmp(argv[nopt],"-frugal",4) == 0 ){
        ZCAT_frugal = 1 ; nopt++ ; continue ;
      }

      /**** -nscale ****/

      if( strncmp(argv[nopt],"-nscale",6) == 0 ){
        ZCAT_gscale = ZCAT_fscale = 0 ; ZCAT_nscale = 1 ; nopt++ ; continue ;
      }

      /**** -fscale ****/

      if( strncmp(argv[nopt],"-fscale",6) == 0 ){
        ZCAT_fscale = 1 ; ZCAT_nscale = 0 ; nopt++ ; continue ;
      }

#if 0
      /**** -gscale ****/

      if( strncmp(argv[nopt],"-gscale",6) == 0 ){
        ZCAT_gscale = ZCAT_fscale = 1 ; ZCAT_nscale = 0 ; nopt++ ; continue ;
      }
#endif

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ){
        nopt++ ;
        if( nopt >= argc ){
          ERROR_exit("need argument after -prefix!\n") ;
        }
        MCW_strncpy( ZCAT_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
	
        if( strstr(ZCAT_output_prefix,".nii") != NULL ) {
            write_output = True;
	    NIFTI_mode = True;
            if( strstr(ZCAT_output_prefix,".nii.gz") != NULL ) {
	       cmode = 0; /* force gzip compression  (actually zlib from nifti library)*/
	    }   
	}       
        else if( !THD_filename_ok(ZCAT_output_prefix) )
          ERROR_exit("Illegal character in -prefix '%s'",ZCAT_output_prefix) ;
        continue ;
      }

      /**** -verb ****/

      if( strncmp(argv[nopt],"-verb",5) == 0 ){
        ZCAT_verb++ ; nopt++ ; continue ;
      }

      /**** -datum type ****/

      if( strncmp(argv[nopt],"-datum",6) == 0 ){
         if( ++nopt >= argc ){
           ERROR_exit("need an argument after -datum!\n");
         }
              if( strcmp(argv[nopt],"short") == 0 ) ZCAT_datum = MRI_short ;
         else if( strcmp(argv[nopt],"float") == 0 ) ZCAT_datum = MRI_float ;
         else if( strcmp(argv[nopt],"byte" ) == 0 ) ZCAT_datum = MRI_byte  ;
#if 0
         else if( strcmp(argv[nopt],"complex")== 0) ZCAT_datum = MRI_complex ;
#endif
         else {
           ERROR_exit("-datum %s not supported in 3dZcat!\n",
                   argv[nopt] ) ;
         }
         nopt++ ; continue ;  /* go to next arg */
      }

      /**** Garbage ****/

      if( strcmp(argv[nopt],"-input") == 0 ){
        nopt++ ;
      } else if( argv[nopt][0] == '-' ){
        ERROR_exit("Unknown option: %s\n",argv[nopt]) ;
      }

      /**** read dataset ****/

      if( ZCAT_verb )
        fprintf(stderr,"++ Opening dataset %s\n",argv[nopt]) ;

      dset = THD_open_dataset( argv[nopt++] ) ;
      if( dset == NULL ){
         ERROR_exit("Can't open dataset %s\n",argv[nopt-1]) ;
      }
      THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_MALLOC ) ;

      if( ZCAT_type < 0 ) ZCAT_type = dset->type ;

      /* check if voxel counts match, etc. */

      ii = dset->daxes->nxx * dset->daxes->nyy ;
      if( ZCAT_nxy < 0 ){
         ZCAT_nxy   = ii ;
         ZCAT_nbrik = DSET_NVALS(dset) ;
      } else if( ii != ZCAT_nxy ){
         ERROR_exit("** Dataset %s differs in slice size from others\n",argv[nopt-1]);
      } else if ( DSET_NVALS(dset) != ZCAT_nbrik ){
         ERROR_exit("** Dataset %s has different number of sub-bricks\n",argv[nopt-1]) ;
      } else if ( DSET_BRICK_TYPE(dset,0) == MRI_complex ){
         ERROR_exit("** Dataset %s is complex-valued -- ILLEGAL\n",argv[nopt-1]) ;
      }

      ADDTO_3DARR(ZCAT_dsar,dset) ;  /* list of datasets */

   }  /* end of loop over command line arguments */

   if(NIFTI_mode && ZCAT_frugal){
      ERROR_message("Frugality and NIFTI output do not mix.\n");
      ERROR_exit("Try without -frugal or with different output type.\n");
   }
   return ;
}

/*-------------------------------------------------------------------------*/

void ZCAT_Syntax(void)
{
   printf(
    "Usage: 3dZcat [options] dataset dataset ...\n"
    "Concatenates datasets in the slice (z) direction.  Each input\n"
    "dataset must have the same number of voxels in each slice, and\n"
    "must have the same number of sub-bricks.\n"
    "\n"
    "Options:\n"
    "  -prefix pname = Use 'pname' for the output dataset prefix name.\n"
    "                    [default='zcat']\n"
    "  -datum type   = Coerce the output data to be stored as the given\n"
    "                    type, which may be byte, short, or float.\n"
    "  -fscale       = Force scaling of the output to the maximum integer\n"
    "                    range.  This only has effect if the output datum\n"
    "                    is byte or short (either forced or defaulted).\n"
    "                    This option is sometimes necessary to eliminate\n"
    "                    unpleasant truncation artifacts.\n"
    "  -nscale       = Don't do any scaling on output to byte or short datasets.\n"
    "                    This may be especially useful when operating on mask\n"
    "                    datasets whose output values are only 0's and 1's.\n"
    "  -verb         = Print out some verbositiness as the program proceeds.\n"
    "  -frugal       = Be 'frugal' in the use of memory, at the cost of I/O time.\n"
    "                    Only needed if the program runs out of memory.\n"
    "                    Note frugality cannot be combined with NIFTI output\n"
    "\n"
    "Command line arguments after the above are taken as input datasets.\n"
    "\n"
    "Notes:\n"
    "* You can use the '3dinfo' program to see how many slices a\n"
    "    dataset comprises.\n"
    "* There must be at least two datasets input (otherwise, the\n"
    "    program doesn't make much sense, does it?).\n"
    "* Each input dataset must have the same number of voxels in each\n"
    "    slice, and must have the same number of sub-bricks.\n"
    "* This program does not deal with complex-valued datasets.\n"
    "* See the output of '3dZcutup -help' for a C shell script that\n"
    "    can be used to take a dataset apart into single slice datasets,\n"
    "    analyze them separately, and then assemble the results into\n"
    "    new 3D datasets.\n"
   ) ;

   PRINT_COMPILE_DATE ; exit(0) ;
}

/*-------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int ninp , ids , iv ,kz,new_nz, nx,ny,nz,nxy,nxyz ;
   THD_3dim_dataset * new_dset=NULL , * dset ;
   THD_ivec3 iv_nxyz ;
   float * fvol , *ffac ;
   void  * svol ;
   int fscale ; FILE * data_file = NULL ;
   MRI_IMAGE *svol_im;
   MRI_IMARR *im_array;

   /*** read input options ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) ZCAT_Syntax() ;

   /*-- addto the arglist, if user wants to --*/

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc, &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   mainENTRY("3dZcat main") ; machdep() ; AFNI_logger("3dZcat",argc,argv) ;
   PRINT_VERSION("3dZcat") ;

   ZCAT_read_opts( argc , argv ) ;

   /*** create new dataset (empty) ***/

   ninp = ZCAT_dsar->num ;
   if( ninp < 2 ){
      ERROR_exit("Must have at least 2 input datasets!\n");
   }

   /* compute total number of z-slices in output */
   new_nz = 0 ;
   for( ids=0 ; ids < ninp ; ids++ ) new_nz += DSET_NZ(DSUB(ids)) ;

   if( ZCAT_verb )
      fprintf(stderr,"++ Output will have %d slices\n",new_nz) ;

   /** find 1st dataset that is time dependent, if any **/

   for( ids=0 ; ids < ninp ; ids++ ){
      dset = DSUB(ids) ;
      if( DSET_TIMESTEP(dset) > 0.0 ) break ;
   }
   if( ids == ninp ) dset = DSUB(0) ; /* fallback position */
   if( ZCAT_verb )
      fprintf(stderr,"++ Using %s as 'master' dataset\n",DSET_HEADNAME(dset)) ;

   new_dset = EDIT_empty_copy( dset ) ; /* make a copy of its header */

   tross_Copy_History( dset , new_dset ) ;
   tross_Make_History( "3dZcat" , argc,argv , new_dset ) ;

   /* modify its header */

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ; nxy = nx*ny ; nxyz = nxy*new_nz ;

   LOAD_IVEC3( iv_nxyz , nx , ny , new_nz ) ;

   /* if data type has not been selected by user, get the data type from master */
   if( ZCAT_datum < 0 ) ZCAT_datum = DSET_BRICK_TYPE(dset,0) ;

   /*-- open output BRIK file --*/
   /* default cmode is COMPRESS_NOFILE  unless nifti.gzip output (.nii.gz) */
   if ((cmode == COMPRESS_NOFILE)) { /* ignore compression for NIFTI - do in write
   automatically later */
      cmode = THD_get_write_compression() ; /* check env. variable for compression*/
#if 0
      if(NIFTI_mode && (cmode!=0)) /* have to compress this NIFTI data, add .gz to prefix */
         cmode = COMPRESS_NOFILE;
         sprintf(ZCAT_output_prefix, "%s.gz", ZCAT_output_prefix);
#endif
      }

   EDIT_dset_items( new_dset ,
                      ADN_prefix    , ZCAT_output_prefix ,
                      ADN_type      , ZCAT_type ,
                      ADN_nxyz      , iv_nxyz ,
                      ADN_datum_all , ZCAT_datum ,
                      ADN_nsl       , 0 , /* kill time offsets  */
                    ADN_none ) ;

   /* can't re-write existing dataset */
   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(new_dset)) ){
     ERROR_exit("Fatal error: dataset %s already exists!\n",
             DSET_HEADNAME(new_dset) ) ;
   }

   THD_force_malloc_type( new_dset->dblk , DATABLOCK_MEM_MALLOC ) ;

   if( !ZCAT_frugal ){                        /* 05 Apr 2006 */
     for( ids=0 ; ids < ninp ; ids++ ){
       dset = DSUB(ids) ; DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
     }
   }
   else {
      data_file = COMPRESS_fopen_write( DSET_BRIKNAME(new_dset) , cmode ) ;
      if( data_file == NULL ){
         ERROR_exit(
           "\a\n*** cannot open output file %s\n",DSET_BRIKNAME(new_dset)) ;
      }
   }

   /* make space for sub-brick scaling factors */

   ffac = (float *) malloc(sizeof(float)*DSET_NVALS(new_dset)) ;

   if (!ZCAT_frugal)
       INIT_IMARR(im_array);  


   /*** Loop over output sub-bricks ***/

   for( iv=0 ; iv < DSET_NVALS(new_dset) ; iv++ ){

      if( ZCAT_verb )
        fprintf(stderr,"++ Computing output sub-brick #%d\n",iv) ;

      fscale = ZCAT_fscale ;  /* local for this brick */

      /*  make temporary holding space */

      fvol = (float *) malloc(sizeof(float)*nxyz) ;

      /* loop over input datasets */
      /* kz = slice index in output that this input dataset starts at */

      for( kz=ids=0 ; ids < ninp ; ids++ ){

         dset = DSUB(ids) ; nz = DSET_NZ(dset) ;
         if( ZCAT_frugal ){
           DSET_load(dset) ;
           if( ! DSET_LOADED(dset) ){
             ERROR_message("Fatal error: can't load data from %s\n",
                     DSET_BRIKNAME(dset)) ;
             COMPRESS_fclose(data_file) ; remove(DSET_BRIKNAME(new_dset)) ;
             exit(1) ;
          }
         }
         if( ZCAT_verb == 2 )
           fprintf(stderr," + processing input %s\n",DSET_BRIKNAME(dset)) ;

         /* copy data from input to holding space, converting to float */

         switch( DSET_BRICK_TYPE(dset,iv) ){

            default:
               ERROR_exit(
                       "** Illegal input brick type=%s in dataset %s\n",
                       MRI_TYPE_name[DSET_BRICK_TYPE(dset,iv)] ,
                       DSET_HEADNAME(dset) ) ;

            case MRI_short:{
               register int ii , itop = DSET_NVOX(dset) ;
               register short *dar = DSET_ARRAY(dset,iv) ;
               register float fac  = DSET_BRICK_FACTOR(dset,iv) ;
               register float *var = fvol + kz*nxy ;
               if( fac == 0.0 ) fac = 1.0 ;
               for( ii=0 ; ii < itop ; ii++ ) var[ii] = fac*dar[ii] ;
               if( fac != 1.0 ) fscale = 1 ;
            }
            break ;

            case MRI_byte:{
               register int ii , itop = DSET_NVOX(dset) ;
               register byte *dar  = DSET_ARRAY(dset,iv) ;
               register float fac  = DSET_BRICK_FACTOR(dset,iv) ;
               register float *var = fvol + kz*nxy ;
               if( fac == 0.0 ) fac = 1.0 ;
               for( ii=0 ; ii < itop ; ii++ ) var[ii] = fac*dar[ii] ;
               if( fac != 1.0 ) fscale = 1 ;
            }
            break ;

            case MRI_float:{
               register int ii , itop = DSET_NVOX(dset) ;
               register float *dar = DSET_ARRAY(dset,iv) ;
               register float fac  = DSET_BRICK_FACTOR(dset,iv) ;
               register float *var = fvol + kz*nxy ;
               if( fac == 0.0 ) fac = 1.0 ;
               for( ii=0 ; ii < itop ; ii++ ) var[ii] = fac*dar[ii] ;
            }
            break ;

         } /* end of switch over input brick type */

         kz += nz ;
         if( ZCAT_frugal ) DSET_unload(dset) ;
         else              DSET_unload_one(dset,iv) ;

      } /* end of loop over input datasets */

      /* at this point, fvol holds the correctly
         scaled values for the output dataset;
         now, must store it in the correct datum into svol;
         this code is lifted/adapted from 3dcalc.c          */

      switch( ZCAT_datum ){

         default:                   /* should never transpire */
            ERROR_exit(
                    "** Fatal Error **\n"
                    "** Somehow ended up with -datum = %d\n",ZCAT_datum) ;

         case MRI_float:            /* the trivial case */
            svol = (void *) fvol ;
            ffac[iv] = 0.0 ;        /* don't need no stinking factor */
         break ;

         case MRI_byte:             /* harder cases: */
         case MRI_short:{           /* must create svol and scale to it */
            float gtop , fimfac ;

            /* find largest value in fvol */

            gtop = MCW_vol_amax( nxyz , 1 , 1 , MRI_float, fvol ) ;
            if( gtop == 0.0 )
              fprintf(stderr,"++ Warning: output sub-brick %d is all zeros!\n",iv) ;

            /* compute scaling factor */

            if( fscale ){
               fimfac = (gtop > 0.0) ? MRI_TYPE_maxval[ZCAT_datum] / gtop : 0.0 ;
            } else if( !ZCAT_nscale ){
               fimfac = (gtop > MRI_TYPE_maxval[ZCAT_datum] || (gtop > 0.0 && gtop <= 1.0) )
                        ? MRI_TYPE_maxval[ZCAT_datum]/ gtop : 0.0 ;
            } else {
               fimfac = 0.0 ;
            }

            if( ZCAT_verb == 2 ){
               if( fimfac != 0.0 )
                  fprintf(stderr," + Output sub-brick %d scale factor = %f\n",iv,fimfac) ;
               else
                  fprintf(stderr,"++ Output sub-brick %d: no scale factor\n" ,iv) ;
            }

            /* now scale fvol into svol */

            svol = (void *) malloc( mri_datum_size(ZCAT_datum) * nxyz ) ;

            EDIT_coerce_scale_type( nxyz , fimfac ,
                                    MRI_float, fvol , ZCAT_datum,svol ) ;

            free(fvol) ;
            ffac[iv] = (fimfac > 0.0 && fimfac != 1.0) ? 1.0/fimfac : 0.0 ;
         }
         break ;
      } /* end of switch on output datum type */

     /* if being frugal with memory (why?),
         save the data to disk for each output sub-brick */
     if(ZCAT_frugal) {
          /*-- now save svol to disk --*/
          fwrite( svol , mri_datum_size(ZCAT_datum) , nxyz , data_file ) ;
          free(svol) ;
     }
     /* otherwise, save them up and write all sub-bricks all at once */
     else {
          /* copy the sub-brick volume, svol, into an MRI_IMAGE structure  and 
              then append that to the image array */
          svol_im = mri_new_vol_empty( nx, ny, kz, ZCAT_datum) ;
          mri_fix_data_pointer( svol , svol_im ) ;
          ADDTO_IMARR(im_array, svol_im);
     }

   } /* end of loop over output sub-bricks */


   /*-- cleanup, and write dataset header (and brick if not being frugal) --*/

   if( ZCAT_verb )
      fprintf(stderr,"++ Computing output sub-brick statistics\n") ;

   for( ids=0 ; ids < ninp ; ids++ )       /* remove inputs from memory */
      DSET_delete( DSUB(ids) ) ;

   /* update output dataset image array with pointer to all of the data */
   if(!ZCAT_frugal)
      new_dset->dblk->brick = im_array;   /* update pointer to data */
   else
      COMPRESS_fclose(data_file) ;        /* close output BRIK file */


   EDIT_dset_items( new_dset ,             /* set sub-brick factors */
                      ADN_brick_fac,ffac ,
                    ADN_none ) ;
   free(ffac) ;                            /* don't need ffac no more */

   /* read all the data back in if being frugal with memory */
   if(ZCAT_frugal)
      DSET_load(new_dset) ;                   /* read new dataset from disk */
   else
      write_output = 1 ;                   /* have to write everything out */

   THD_load_statistics(new_dset) ;         /* compute sub-brick statistics */

   THD_write_3dim_dataset(NULL,NULL,new_dset,write_output); /* (re)write output file */
   INFO_message("output dataset: %s\n",DSET_BRIKNAME(new_dset)) ;

   exit(0) ;                               /* stage left, pursued by a bear */
}
