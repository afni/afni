#include "mrilib.h"

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

#define DSUB(id) DSET_IN_3DARR(ZCAT_dsar,(id))

static char ZCAT_output_prefix[THD_MAX_PREFIX] = "zcat" ;

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

      /**** -nscale ****/

      if( strncmp(argv[nopt],"-nscale",6) == 0 ){
         ZCAT_gscale = ZCAT_fscale = 0 ; ZCAT_nscale = 1 ;
         nopt++ ; continue ;
      }

      /**** -fscale ****/

      if( strncmp(argv[nopt],"-fscale",6) == 0 ){
         ZCAT_fscale = 1 ; ZCAT_nscale = 0 ;
         nopt++ ; continue ;
      }

#if 0
      /**** -gscale ****/

      if( strncmp(argv[nopt],"-gscale",6) == 0 ){
         ZCAT_gscale = ZCAT_fscale = 1 ; ZCAT_nscale = 0 ;
         nopt++ ; continue ;
      }
#endif

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            fprintf(stderr,"*** need argument after -prefix!\n") ; exit(1) ;
         }
         MCW_strncpy( ZCAT_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

      /**** -verb ****/

      if( strncmp(argv[nopt],"-verb",5) == 0 ){
         ZCAT_verb++ ;
         nopt++ ; continue ;
      }

      /**** -datum type ****/

      if( strncmp(argv[nopt],"-datum",6) == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"*** need an argument after -datum!\n"); exit(1);
         }
              if( strcmp(argv[nopt],"short") == 0 ) ZCAT_datum = MRI_short ;
         else if( strcmp(argv[nopt],"float") == 0 ) ZCAT_datum = MRI_float ;
         else if( strcmp(argv[nopt],"byte" ) == 0 ) ZCAT_datum = MRI_byte  ;
#if 0
         else if( strcmp(argv[nopt],"complex")== 0) ZCAT_datum = MRI_complex ;
#endif
         else {
            fprintf(stderr,"*** -datum %s not supported in 3dZcat!\n",
                    argv[nopt] ) ;
            exit(1) ;
         }
         nopt++ ; continue ;  /* go to next arg */
      }

      /**** Garbage ****/

      if( argv[nopt][0] == '-' ){
         fprintf(stderr,"*** Unknown option: %s\n",argv[nopt]) ; exit(1) ;
      }

      /**** read dataset ****/

      if( ZCAT_verb )
         fprintf(stderr,"+++ Opening dataset %s\n",argv[nopt]) ;

      dset = THD_open_dataset( argv[nopt++] ) ;
      if( dset == NULL ){
         fprintf(stderr,"*** Can't open dataset %s\n",argv[nopt-1]) ; exit(1) ;
      }
      THD_force_malloc_type( dset->dblk , DATABLOCK_MEM_MALLOC ) ;

      if( ZCAT_type < 0 ) ZCAT_type = dset->type ;

      /* check if voxel counts match, etc. */

      ii = dset->daxes->nxx * dset->daxes->nyy ;
      if( ZCAT_nxy < 0 ){
         ZCAT_nxy   = ii ;
         ZCAT_nbrik = DSET_NVALS(dset) ;
      } else if( ii != ZCAT_nxy ){
         fprintf(stderr,"*** Dataset %s differs in slice size from others\n",argv[nopt-1]);
         exit(1) ;
      } else if ( DSET_NVALS(dset) != ZCAT_nbrik ){
         fprintf(stderr,"*** Dataset %s has different number of sub-bricks\n",argv[nopt-1]) ;
         exit(1) ;
      } else if ( DSET_BRICK_TYPE(dset,0) == MRI_complex ){
         fprintf(stderr,"*** Dataset %s is complex-valued -- ILLEGAL\n",argv[nopt-1]) ;
         exit(1) ;
      }

      ADDTO_3DARR(ZCAT_dsar,dset) ;  /* list of datasets */

   }  /* end of loop over command line arguments */

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
    "  -fscale     = Force scaling of the output to the maximum integer\n"
    "                  range.  This only has effect if the output datum\n"
    "                  is byte or short (either forced or defaulted).\n"
    "                  This option is sometimes necessary to eliminate\n"
    "                  unpleasant truncation artifacts.\n"
    "  -nscale     = Don't do any scaling on output to byte or short datasets.\n"
    "                   This may be especially useful when operating on mask\n"
    "                   datasets whose output values are only 0's and 1's.\n"
    "  -verb         = Print out some verbositiness as the program\n"
    "                    proceeds.\n"
    "\n"
    "Command line arguments after the above are taken as input datasets.\n"
    "A dataset is specified using one of these forms:\n"
    "   'prefix+view', 'prefix+view.HEAD', or 'prefix+view.BRIK'.\n"
    "\n"

    MASTER_SHORTHELP_STRING

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

   exit(0) ;
}

/*-------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int ninp , ids , iv , iz,kz,new_nz, nx,ny,nz,nxy,nxyz ;
   THD_3dim_dataset * new_dset=NULL , * dset ;
   THD_ivec3 iv_nxyz ;
   float * fvol , *ffac ;
   void  * svol ;
   int cmode , fscale ; FILE * far ;

   mainENTRY("3dZcat main") ; machdep() ;

   /*** read input options ***/

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) ZCAT_Syntax() ;

   /*-- addto the arglist, if user wants to --*/

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   ZCAT_read_opts( argc , argv ) ;

   /*** create new dataset (empty) ***/

   ninp = ZCAT_dsar->num ;
   if( ninp < 2 ){
      fprintf(stderr,"*** Must have at least 2 input datasets!\n") ; exit(1) ;
   }

   new_nz = 0 ;
   for( ids=0 ; ids < ninp ; ids++ ) new_nz += DSET_NZ(DSUB(ids)) ;

   if( ZCAT_verb )
      fprintf(stderr,"+++ Output will have %d slices\n",new_nz) ;

   /** find 1st dataset that is time dependent, if any **/

   for( ids=0 ; ids < ninp ; ids++ ){
      dset = DSUB(ids) ;
      if( DSET_TIMESTEP(dset) > 0.0 ) break ;
   }
   if( ids == ninp ) dset = DSUB(0) ; /* fallback position */
   if( ZCAT_verb )
      fprintf(stderr,"+++ Using %s as 'master' dataset\n",DSET_HEADNAME(dset)) ;

   new_dset = EDIT_empty_copy( dset ) ; /* make a copy of its header */

   tross_Copy_History( dset , new_dset ) ;
   tross_Make_History( "3dZcat" , argc,argv , new_dset ) ;

   /* modify its header */

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ; nxy = nx*ny ; nxyz = nxy*new_nz ;

   LOAD_IVEC3( iv_nxyz , nx , ny , new_nz ) ;

   if( ZCAT_datum < 0 ) ZCAT_datum = DSET_BRICK_TYPE(dset,0) ;

   EDIT_dset_items( new_dset ,
                      ADN_prefix    , ZCAT_output_prefix ,
                      ADN_type      , ZCAT_type ,
                      ADN_nxyz      , iv_nxyz ,
                      ADN_datum_all , ZCAT_datum ,
                      ADN_nsl       , 0 , /* kill time offsets  */
                    ADN_none ) ;

   /* can't re-write existing dataset */

   if( THD_is_file(DSET_HEADNAME(new_dset)) ){
     fprintf(stderr,"*** Fatal error: dataset %s already exists!\n",
             DSET_HEADNAME(new_dset) ) ;
     exit(1) ;
   }

   THD_force_malloc_type( new_dset->dblk , DATABLOCK_MEM_MALLOC ) ;

   /*-- open output BRIK file --*/

   cmode = THD_get_write_compression() ;
   far = COMPRESS_fopen_write( DSET_BRICKNAME(new_dset) , cmode ) ;
   if( far == NULL ){
      fprintf(stderr,
        "\a\n*** cannot open output file %s\n",DSET_BRICKNAME(new_dset)) ;
      exit(1) ;
   }

   /* make space for sub-brick scaling factors */

   ffac = (float *) malloc(sizeof(float)*DSET_NVALS(new_dset)) ;

   /*** Loop over output sub-bricks ***/

   for( iv=0 ; iv < DSET_NVALS(new_dset) ; iv++ ){

      if( ZCAT_verb )
         fprintf(stderr,"+++ Computing output sub-brick #%d\n",iv) ;

      fscale = ZCAT_fscale ;  /* local for this brick */

      /*  make temporary holding space */

      fvol = (float *) malloc(sizeof(float)*nxyz) ;

      /* loop over input datasets */
      /* kz = slice index in output that this input dataset starts at */

      for( kz=ids=0 ; ids < ninp ; ids++ ){

         dset = DSUB(ids) ; nz = DSET_NZ(dset) ; DSET_load(dset) ;
         if( ! DSET_LOADED(dset) ){
            fprintf(stderr,"*** Fatal error: can't load data from %s\n",
                    DSET_HEADNAME(dset)) ;
            exit(1) ;
         }

         if( ZCAT_verb == 2 )
            fprintf(stderr," ++ processing input %s\n",DSET_HEADNAME(dset)) ;

         /* copy data from input to holding space, converting to float */

         switch( DSET_BRICK_TYPE(dset,iv) ){

            default:
               fprintf(stderr,
                       "*** Illegal input brick type=%s in dataset %s\n",
                       MRI_TYPE_name[DSET_BRICK_TYPE(dset,iv)] ,
                       DSET_HEADNAME(dset) ) ;
            exit(1) ;

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

         kz += nz ; DSET_unload(dset) ;

      } /* end of loop over input datasets */

      /* at this point, fvol holds the correctly
         scaled values for the output dataset;
         now, must store it in the correct datum into svol;
         this code is lifted/adapted from 3dcalc.c          */

      switch( ZCAT_datum ){

         default:                   /* should never transpire */
            fprintf(stderr,
                    "*** Fatal Error ***\n"
                    "*** Somehow ended up with -datum = %d\n",ZCAT_datum) ;
         exit(1) ;

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
              fprintf(stderr,"+++ Warning: output sub-brick %d is all zeros!\n",iv) ;

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
                  fprintf(stderr," ++ Output sub-brick %d scale factor = %f\n",iv,fimfac) ;
               else
                  fprintf(stderr,"+++ Output sub-brick %d: no scale factor\n" ,iv) ;
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

      /*-- now save svol to disk --*/

      fwrite( svol , mri_datum_size(ZCAT_datum) , nxyz , far ) ;
      free(svol) ;

   } /* end of loop over output sub-bricks */

   /*-- cleanup, and write dataset header --*/

   if( ZCAT_verb )
      fprintf(stderr,"+++ Computing output sub-brick statistics\n") ;

   for( ids=0 ; ids < ninp ; ids++ )       /* remove inputs from memory */
      DSET_delete( DSUB(ids) ) ;

   COMPRESS_fclose(far) ;                  /* close output BRIK file */
   EDIT_dset_items( new_dset ,             /* set sub-brick factors */
                      ADN_brick_fac,ffac ,
                    ADN_none ) ;
   free(ffac) ;                            /* don't need ffac no more */
   DSET_load(new_dset) ;                   /* read new dataset from disk */
   THD_load_statistics(new_dset) ;         /* compute sub-brick statistics */
   DSET_write_header(new_dset) ;           /* write output HEAD */

   exit(0) ;                               /* stage left */
}
