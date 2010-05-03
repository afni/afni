/*------------------------------------------------------------------------
  This program takes the mean of a bunch of datasets, voxel-by-voxel.
  Much of this code was taken from 3dcalc.c [I wrote it, I can steal it].
  30 Jan 2001 - RWCox
  20 Dec 2004 - MSB adds -sd option (cleaned up by RWC a little)
--------------------------------------------------------------------------*/

#include "mrilib.h"

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * inset , * outset ;
   int nx,ny,nz,nxyz,nval , ii,kk , nopt=1, nsum=0 ;
   char * prefix = "mean" ;
   int datum=-1 , verb=0 , do_sd=0, do_sum=0 , do_sqr=0, firstds=0 ;
   float ** sum , fsum;
   float ** sd;

   int fscale=0 , gscale=0 , nscale=0 ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dMean [options] dset dset ...\n"
             "Takes the voxel-by-voxel mean of all input datasets;\n"
             "the main reason is to be faster than 3dcalc.\n"
             "\n"
             "Options [see 3dcalc -help for more details on these]:\n"
             "  -verbose    = Print out some information along the way.\n"
             "  -prefix ppp = Sets the prefix of the output dataset.\n"
             "  -datum ddd  = Sets the datum of the output dataset.\n"
             "  -fscale     = Force scaling of the output to the maximum integer range.\n"
             "  -gscale     = Same as '-fscale', but also forces each output sub-brick to\n"
             "                  to get the same scaling factor.\n"
             "  -nscale     = Don't do any scaling on output to byte or short datasets.\n"
             "\n"
             "  -sd *OR*    = Calculate the standard deviation (variance/n-1) instead\n"
             "  -stdev         of the mean (cannot be used with -sqr or -sum).\n"
             "\n"
             "  -sqr        = Average the squares, instead of the values.\n"
             "  -sum        = Just take the sum (don't divide by number of datasets).\n"
             "\n"
             "N.B.: All input datasets must have the same number of voxels along\n"
             "       each axis (x,y,z,t).\n"
             "    * At least 2 input datasets are required.\n"
             "    * Dataset sub-brick selectors [] are allowed.\n"
             "    * The output dataset origin, time steps, etc., are taken from the\n"
             "       first input dataset.\n"
             "*** If you are trying to compute the mean (or some other statistic)\n"
             "    across time for a 3D+time dataset (not across datasets), use\n"
             "    3dTstat instead.\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dMean main"); machdep() ; PRINT_VERSION("3dMean") ;

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   AFNI_logger("3dMean",argc,argv) ;

   /*-- command line options --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcmp(argv[nopt],"-sd") == 0 || strcmp(argv[nopt],"-stdev") == 0 ){
         do_sd = 1 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-sum") == 0 ){
         do_sum = 1 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-sqr") == 0 ){
         do_sqr = 1 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"** ERROR: need an argument after -prefix!\n"); exit(1);
         }
         prefix = argv[nopt] ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-verbose",5) == 0 ){
         verb++ ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-datum") == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"** ERROR: need an argument after -datum!\n"); exit(1);
         }
         if( strcmp(argv[nopt],"short") == 0 ){
            datum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            datum = MRI_float ;
         } else if( strcmp(argv[nopt],"byte") == 0 ){
            datum = MRI_byte ;
         } else {
            fprintf(stderr,"** ERROR -datum of type '%s' not supported in 3dMean!\n",
                    argv[nopt] ) ;
            exit(1) ;
         }
         nopt++ ; continue ;  /* go to next arg */
      }

      if( strncmp(argv[nopt],"-nscale",6) == 0 ){
         gscale = fscale = 0 ; nscale = 1 ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-fscale",6) == 0 ){
         fscale = 1 ; nscale = 0 ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-gscale",6) == 0 ){
         gscale = fscale = 1 ; nscale = 0 ;
         nopt++ ; continue ;
      }

      fprintf(stderr,"** ERROR: unknown option %s\n",argv[nopt]) ;
      exit(1) ;
   }

   /*-- MSB: check for legal combination of options --*/

   if( (do_sd) && ( do_sum || do_sqr )){
     fprintf(stderr,"** ERROR: -sd cannot be used with -sqr or -sum \n") ;
     exit(1) ;
   }

   /*-- rest of command line should be datasets --*/

   if( nopt >= argc-1 ){
      if ( do_sd ) {
         fprintf(stderr,"** ERROR: need at least 2 input datasets with -sd!\n") ;
         exit(1) ;
      } else {
         fprintf(stderr,"++ WARNING: Have only 1 dset.\n");
      }
   }

   /*-- loop over datasets --*/

   firstds = nopt;
   for( ; nopt < argc ; nopt++,nsum++ ){

      /*-- input dataset header --*/

      inset = THD_open_dataset( argv[nopt] ) ;
      CHECK_OPEN_ERROR(inset,argv[nopt]) ;

      /*-- 1st time thru: make workspace and empty output dataset --*/

      if( nsum == 0 ){

         nx   = DSET_NX(inset) ;
         ny   = DSET_NY(inset) ;
         nz   = DSET_NZ(inset) ; nxyz= nx*ny*nz;
         nval = DSET_NVALS(inset) ;

         sum = (float **) malloc( sizeof(float *)*nval ) ;    /* array of sub-bricks */
         for( kk=0 ; kk < nval ; kk++ ){
           sum[kk] = (float *) malloc(sizeof(float)*nxyz) ;  /* kk-th sub-brick */
           for( ii=0 ; ii < nxyz ; ii++ ) sum[kk][ii] = 0.0f ;
         }

         outset = EDIT_empty_copy( inset ) ;

         if( datum < 0 ) datum = DSET_BRICK_TYPE(inset,0) ;

         tross_Copy_History( inset , outset ) ;
         tross_Make_History( "3dMean" , argc,argv , outset ) ;

         EDIT_dset_items( outset ,
                             ADN_prefix    , prefix ,
                             ADN_datum_all , datum ,
                          ADN_none ) ;

         if( THD_deathcon() && THD_is_file(outset->dblk->diskptr->header_name) ){
            fprintf(stderr,
                    "*** Output file %s already exists -- cannot continue!\n",
                    outset->dblk->diskptr->header_name ) ;
            exit(1) ;
         }

      } else { /*-- later: check if dataset matches 1st one --*/

         if( DSET_NX(inset)    != nx ||
             DSET_NY(inset)    != ny ||
             DSET_NZ(inset)    != nz ||
             DSET_NVALS(inset) != nval  ){

             fprintf(stderr,"** ERROR: dataset %s doesn't match 1st one in sizes\n",
                     argv[nopt]) ;
             fprintf(stderr,"** I'm telling your mother about this!\n") ;
             exit(1) ;
         }
      }

      /*-- read data from disk --*/

      DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

      if( verb ) fprintf(stderr,"  ++ read in dataset %s\n",argv[nopt]) ;

      /*-- sum dataset values --*/

      for( kk=0 ; kk < nval ; kk++ ){

         if( verb )
            fprintf(stderr,"   + sub-brick %d [%s]\n",
                    kk,MRI_TYPE_name[DSET_BRICK_TYPE(inset,kk)] ) ;

         switch( DSET_BRICK_TYPE(inset,kk) ){
            default:
              fprintf(stderr,"ERROR: illegal input sub-brick datum\n") ;
              exit(1) ;

            case MRI_float:{
               float *pp = (float *) DSET_ARRAY(inset,kk) ;
               float fac = DSET_BRICK_FACTOR(inset,kk) , val ;
               if( fac == 0.0 ) fac = 1.0 ;
               if( do_sqr )
                  for( ii=0 ; ii < nxyz ; ii++ )
                     { val = fac * pp[ii] ; sum[kk][ii] += val*val ; }
               else
                  for( ii=0 ; ii < nxyz ; ii++ ) sum[kk][ii] += fac * pp[ii] ;
            }
            break ;

            case MRI_short:{
               short *pp = (short *) DSET_ARRAY(inset,kk) ;
               float fac = DSET_BRICK_FACTOR(inset,kk) , val ;
               if( fac == 0.0 ) fac = 1.0 ;
               if( do_sqr )
                  for( ii=0 ; ii < nxyz ; ii++ )
                     { val = fac * pp[ii] ; sum[kk][ii] += val*val ; }
               else
                  for( ii=0 ; ii < nxyz ; ii++ ) sum[kk][ii] += fac * pp[ii] ;
            }
            break ;

            case MRI_byte:{
               byte *pp = (byte *) DSET_ARRAY(inset,kk) ;
               float fac = DSET_BRICK_FACTOR(inset,kk) , val ;
               if( fac == 0.0 ) fac = 1.0 ;
               if( do_sqr )
                  for( ii=0 ; ii < nxyz ; ii++ )
                     { val = fac * pp[ii] ; sum[kk][ii] += val*val ; }
               else
                  for( ii=0 ; ii < nxyz ; ii++ ) sum[kk][ii] += fac * pp[ii] ;
            }
            break ;
         }
      }

      DSET_delete(inset) ;
   }

   /* scale to be mean instead of sum */

   if( !do_sum ){
     fsum = 1.0 / nsum ;
     for( kk=0 ; kk < nval ; kk++ )
       for( ii=0 ; ii < nxyz ; ii++ ) sum[kk][ii] *= fsum ;
   }

   /* MSB: If calculating SD,
           loop through datasets again,
           subtract each value from the mean, square, and sum up */

   if( do_sd ){
     for (nopt=firstds, nsum=0; nopt < argc ; nopt++,nsum++ ){

      /*-- input dataset header --*/

      inset = THD_open_dataset( argv[nopt] ) ;
      CHECK_OPEN_ERROR(inset,argv[nopt]) ;

      /*-- 1st time thru: make workspace to hold sd sums */

      if( nsum == 0 ){
        sd = (float **) malloc( sizeof(float *)*nval ) ;    /* array of sub-bricks */
        for( kk=0 ; kk < nval ; kk++ ){
          sd[kk] = (float *) malloc(sizeof(float)*nxyz) ;  /* kk-th sub-brick */
          for( ii=0 ; ii < nxyz ; ii++ ) sd[kk][ii] = 0.0f ;
        }
      }

      /*-- read data from disk --*/

      DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

      if( verb ) fprintf(stderr,"  ++ read in dataset %s\n",argv[nopt]) ;

      /*-- sum dataset values into sd --*/

      for( kk=0 ; kk < nval ; kk++ ){

         if( verb )
           fprintf(stderr,"   + sub-brick %d [%s]\n",
                   kk,MRI_TYPE_name[DSET_BRICK_TYPE(inset,kk)] ) ;

         switch( DSET_BRICK_TYPE(inset,kk) ){
           default:
             fprintf(stderr,"ERROR: illegal input sub-brick datum\n") ;
             exit(1) ;

           case MRI_float:{
             float *pp = (float *) DSET_ARRAY(inset,kk) ;
             float fac = DSET_BRICK_FACTOR(inset,kk) , val ;
             if( fac == 0.0 ) fac = 1.0 ;
             for( ii=0 ; ii < nxyz ; ii++ )
               { val = (fac * pp[ii]) - sum[kk][ii]; sd[kk][ii] += val*val ; }
           }
           break ;

           case MRI_short:{
             short *pp = (short *) DSET_ARRAY(inset,kk) ;
             float fac = DSET_BRICK_FACTOR(inset,kk) , val ;
             if( fac == 0.0 ) fac = 1.0 ;
             for( ii=0 ; ii < nxyz ; ii++ )
               { val = (fac * pp[ii]) - sum[kk][ii]; sd[kk][ii] += val*val ; }
           }
           break ;

           case MRI_byte:{
             byte *pp = (byte *) DSET_ARRAY(inset,kk) ;
             float fac = DSET_BRICK_FACTOR(inset,kk) , val ;
             if( fac == 0.0 ) fac = 1.0 ;
             for( ii=0 ; ii < nxyz ; ii++ )
               { val = (fac * pp[ii]) - sum[kk][ii]; sd[kk][ii] += val*val ; }
           }
           break ;
         }
       }

       DSET_delete(inset) ;
     } /* end dataset loop */

     /*-- fill up output dataset with sd instead of sum --*/

     fsum = 1.0 / (nsum - 1) ;
     for( kk=0 ; kk < nval ; kk++ ){
       for( ii=0 ; ii < nxyz ; ii++ ) sum[kk][ii] = sqrt(sd[kk][ii]*fsum) ;
       free((void *)sd[kk]) ;
     }

   } /* end sd loop */

   switch( datum ){

      default:
         fprintf(stderr,
                 "*** Fatal Error ***\n"
                 "*** Somehow ended up with datum = %d\n",datum) ;
         exit(1) ;

      case MRI_float:{
         for( kk=0 ; kk < nval ; kk++ ){
             EDIT_substitute_brick(outset, kk, MRI_float, sum[kk]);
             DSET_BRICK_FACTOR(outset, kk) = 0.0;
         }
      }
      break ;

      case MRI_byte:
      case MRI_short:{
         void ** dfim ;
         float gtop , fimfac , gtemp ;

         if( verb )
            fprintf(stderr,"  ++ Scaling output to type %s brick(s)\n",
                    MRI_TYPE_name[datum] ) ;

         dfim = (void **) malloc(sizeof(void *)*nval) ;

         if( gscale ){   /* allow global scaling */
            gtop = 0.0 ;
            for( kk=0 ; kk < nval ; kk++ ){
               gtemp = MCW_vol_amax( nxyz , 1 , 1 , MRI_float, sum[kk] ) ;
               gtop  = MAX( gtop , gtemp ) ;
               if( gtemp == 0.0 )
                  fprintf(stderr,"  -- Warning: output sub-brick %d is all zeros!\n",kk) ;
            }
         }

         for (kk = 0 ; kk < nval ; kk ++ ) {

            if( ! gscale ){
               gtop = MCW_vol_amax( nxyz , 1 , 1 , MRI_float, sum[kk] ) ;
               if( gtop == 0.0 )
                  fprintf(stderr,"  -- Warning: output sub-brick %d is all zeros!\n",kk) ;
            }

            if( fscale ){
               fimfac = (gtop > 0.0) ? MRI_TYPE_maxval[datum] / gtop : 0.0 ;
            } else if( !nscale ){
               fimfac = (gtop > MRI_TYPE_maxval[datum] || (gtop > 0.0 && gtop <= 1.0) )
                        ? MRI_TYPE_maxval[datum]/ gtop : 0.0 ;
            } else {
               fimfac = 0.0 ;
            }

            if( verb ){
               if( fimfac != 0.0 )
                  fprintf(stderr,"  ++ Sub-brick %d scale factor = %f\n",kk,fimfac) ;
               else
                  fprintf(stderr,"  ++ Sub-brick %d: no scale factor\n" ,kk) ;
            }

            dfim[kk] = (void *) malloc( mri_datum_size(datum) * nxyz ) ;
            if( dfim[kk] == NULL ){ fprintf(stderr,"*** malloc fails at output\n");exit(1); }

            EDIT_coerce_scale_type( nxyz , fimfac ,
                                    MRI_float, sum[kk] , datum,dfim[kk] ) ;
            if( datum == MRI_short )
              EDIT_misfit_report( DSET_FILECODE(outset) , kk ,
                                  nxyz , (fimfac != 0.0f) ? 1.0f/fimfac : 0.0f ,
                                  dfim[kk] , sum[kk] ) ;
            free( sum[kk] ) ;
            EDIT_substitute_brick(outset, kk, datum, dfim[kk] );

            DSET_BRICK_FACTOR(outset,kk) = (fimfac != 0.0) ? 1.0/fimfac : 0.0 ;
          }
      }
      break ;
   }

   if( verb ) fprintf(stderr,"  ++ Computing output statistics\n") ;
   THD_load_statistics( outset ) ;

   THD_write_3dim_dataset( NULL,NULL , outset , True ) ;
   if( verb ) fprintf(stderr,"  ++ Wrote output: %s\n",DSET_BRIKNAME(outset)) ;

   exit(0) ;
}
