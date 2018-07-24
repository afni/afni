#include "mrilib.h"
#include "extrema.h"

#define ALLOW_INEDGE

#ifdef ALLOW_INEDGE
#include "mri_intedge.c"  /* Jul 2018 */
#endif

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * inset , * outset ;
   int nx,ny,nz,nxyz,nval , ii,kk , nopt=1, nsum=0 ;
   char * prefix = "edge3" , *insetname=NULL;
   int datum=-1 , verb=0 , do_sd=0, do_sum=0 , do_sqr=0, firstds=0 ;
   float ** sum , fsum, ffac = 0.0;
   float ** sd;
   int border[3]={0,0,0};
   int indims[3]={0,0,0};
   int fscale=0 , gscale=0 , nscale=0 ;
   float filterCoefs[3] = {1.0, 1.0, 1.0};
   recursiveFilterType filterType = ALPHA_DERICHE;
   /* recursiveFilterType filterType = GAUSSIAN_DERICHE; */

   int do_inedge=0 ;  /* Jul 2018 */

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
"Usage: 3dedge3 [options] dset dset ...\n"
 "Does 3D Edge detection using the library 3DEdge by;\n"
 "by Gregoire Malandain (gregoire.malandain@sophia.inria.fr)\n"
 "\n"
 "Options :\n"
 "  -input iii  = Input dataset\n"
 "  -verbose    = Print out some information along the way.\n"
 "  -prefix ppp = Sets the prefix of the output dataset.\n"
 "  -datum ddd  = Sets the datum of the output dataset.\n"
 "  -fscale     = Force scaling of the output to the maximum integer range.\n"
 "  -gscale     = Same as '-fscale', but also forces each output sub-brick to\n"
 "                  to get the same scaling factor.\n"
 "  -nscale     = Don't do any scaling on output to byte or short datasets.\n"
 "  -scale_floats VAL = Multiply input by VAL, but only if the input datum is\n"
 "                      float. This is needed when the input dataset\n"
 "                      has a small range, like 0 to 2.0 for instance.\n"
 "                      With such a range, very few edges are detected due to\n"
 "                      what I suspect to be truncation problems.\n"
 "                      Multiplying such a dataset by 10000 fixes the problem\n"
 "                      and the scaling is undone at the output.\n"
#if 0
 "\n"
 "  -inedge     = Process the #0 sub-brick like the '-inedge' option of\n"
 "                3dQwarp and write the result out. Mostly for checking\n"
 "                things out, not for any other practical purpose. [Jul 2018]\n"
 "\n"
#endif
 "\n"
 "References for the algorithms:\n"
 " -  Optimal edge detection using recursive filtering\n"
 "    R. Deriche, International Journal of Computer Vision,\n"
 "    pp 167-187, 1987.\n"
 " -  Recursive filtering and edge tracking: two primary tools\n"
 "    for 3-D edge detection, O. Monga, R. Deriche, G. Malandain\n"
 "    and J.-P. Cocquerez, Image and Vision Computing 4:9, \n"
 "    pp 203-214, August 1991.\n"
 "\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dedge3 main"); machdep() ; PRINT_VERSION("3dedge3") ;

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   AFNI_logger("3dedge3",argc,argv) ;

   /*-- command line options --*/

   while( nopt < argc ){


      if( strcmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,
               "** ERROR: need an argument after -prefix!\n"); exit(1);
         }
         prefix = argv[nopt] ;
         nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-input") == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"** ERROR: need an argument after -input!\n"); 
            exit(1);
         }
         insetname = argv[nopt] ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-inedge") == 0 ){  /* Jul 2018 */
#ifndef ALLOW_INEDGE
        WARNING_message("-inedge is disabled - ignoring this option :(") ;
#endif
        do_inedge = 1 ; nopt++ ; continue ;
      }
      
      if( strcmp(argv[nopt],"-scale_floats") == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"** ERROR: a factor after -scale_floats!\n"); 
            exit(1);
         }
         ffac = (float)strtod(argv[nopt],NULL) ;
         nopt++ ; continue ;
      }
      
      if( strncmp(argv[nopt],"-verbose",5) == 0 ){
         verb++ ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-datum") == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"** ERROR: need an argument after -datum!\n"); 
            exit(1);
         }
         if( strcmp(argv[nopt],"short") == 0 ){
            datum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            datum = MRI_float ;
         } else if( strcmp(argv[nopt],"byte") == 0 ){
            datum = MRI_byte ;
         } else {
            fprintf(stderr,
                  "** ERROR -datum of type '%s' not supported in 3dedge3!\n",
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

   if (!insetname) {
      fprintf(stderr,"** ERROR: no input dset specified\n") ;
      exit(1) ;
   }

   {

      /*-- input dataset header --*/

      inset = THD_open_dataset( insetname ) ;
      if( !ISVALID_DSET(inset) ){
         fprintf(stderr,"** ERROR: can't open dataset %s\n",argv[nopt]) ;
         exit(1) ;
      }

      /*-- make workspace and empty output dataset --*/

      if( nsum == 0 ){

         nx   = DSET_NX(inset) ;
         ny   = DSET_NY(inset) ;
         nz   = DSET_NZ(inset) ; nxyz= nx*ny*nz;
         nval = DSET_NVALS(inset) ;

         sum = (float **) malloc( sizeof(float *)*nval ) ;    
                                          /* array of sub-bricks */
         for( kk=0 ; kk < nval ; kk++ ){
           sum[kk] = (float *) malloc(sizeof(float)*nxyz) ;  
                                             /* kk-th sub-brick */ 
         }

         outset = EDIT_empty_copy( inset ) ;

         if( datum < 0 ) datum = DSET_BRICK_TYPE(inset,0) ;

         tross_Copy_History( inset , outset ) ;
         tross_Make_History( "3dedge3" , argc,argv , outset ) ;

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

      } 
      

      /*-- read data from disk --*/

      DSET_mallocize(inset);
      DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

      if( verb ) fprintf(stderr,"  ++ read in dataset %s\n",insetname) ;

      /*-- Do the -inedge option now [Jul 2018] --*/


#ifdef ALLOW_INEDGE
      if( do_inedge ){
        MRI_IMAGE *bim = THD_extract_float_brick(0,inset) ;
        if( bim == NULL ) ERROR_exit("-inedge: Cannot get sub-brick #0") ;
        mri_interior_edgeize(bim,4,0.222f) ;

        EDIT_dset_items( outset, ADN_nvals,1 , ADN_datum_all,MRI_float , ADN_none ) ;
        EDIT_substitute_brick(outset, 0, MRI_float, MRI_FLOAT_PTR(bim) ) ;
        DSET_write(outset) ;
        INFO_message("wrote out -inedge dataset: %s",DSET_BRIKNAME(outset)) ;
        exit(0) ;
      }
#endif

      /*-- Edge detect each sub-brik --*/
      
      indims[0] = DSET_NX(inset);
      indims[1] = DSET_NY(inset);
      indims[2] = DSET_NZ(inset);
      border[0] = 50;
      border[1] = 50;
      border[2] = 50;
      for( kk=0 ; kk < nval ; kk++ ){

         if( verb )
            fprintf(stderr,"   + sub-brick %d [%s]\n",
                    kk,MRI_TYPE_name[DSET_BRICK_TYPE(inset,kk)] ) ;
         if (DSET_BRICK_TYPE(inset,kk) != DSET_BRICK_TYPE(inset,0)) {
            fprintf(stderr,"ERROR: Sub-bricks of different types.\n"
                           "This is not splenda\n") ;
              exit(1) ;
         }
         switch( DSET_BRICK_TYPE(inset,kk) ){
            default:
              fprintf(stderr,"ERROR: illegal input sub-brick datum\n") ;
              exit(1) ;

           case MRI_float:{
             float *pp = (float *) DSET_ARRAY(inset,kk) ;
             float fac = DSET_BRICK_FACTOR(inset,kk)  ;
             if(ffac) {
               if (fac) fac = fac*ffac;
               else fac = ffac;
             }
             if( fac ) {
               for( ii=0 ; ii < nxyz ; ii++ ) { pp[ii] *= fac; }
             }
              if ( Extract_Gradient_Maxima_3D( (void *)pp, FLOAT,
				               sum[kk], FLOAT,
				               indims,
				               border,
				               filterCoefs,
				               filterType ) == 0 ) {
                fprintf( stderr, "ERROR: gradient extraction failed.\n" );
                exit( 1 );
              }
             
           }
           break ;

            case MRI_short:{
               short *pp = (short *) DSET_ARRAY(inset,kk) ;
               float fac = DSET_BRICK_FACTOR(inset,kk)  ;
               
               if( fac && verb) {
                  INFO_message(
                     "Ignoring brick factor of %f for Gradient Extraction", 
                                 fac);
               }
               if (verb) INFO_message("Going to Extract_Gradient_Maxima_3D\n");
               if ( Extract_Gradient_Maxima_3D( (void *)pp, SSHORT,
				               sum[kk], FLOAT,
				               indims,
				               border,
				               filterCoefs,
				               filterType ) == 0 ) {
                fprintf( stderr, "ERROR: gradient extraction failed.\n" );
                exit( 1 );
               }
            }
            break ;

            case MRI_byte:{
               byte *pp = (byte *) DSET_ARRAY(inset,kk) ;
               float fac = DSET_BRICK_FACTOR(inset,kk)  ;
               
               if( fac && verb) {
                  INFO_message(
                     "Ignoring brick factor of %f for Gradient Extraction", 
                                 fac);
               }
               if ( Extract_Gradient_Maxima_3D( (void *)pp, UCHAR,
				               sum[kk], FLOAT,
				               indims,
				               border,
				               filterCoefs,
				               filterType ) == 0 ) {
                fprintf( stderr, "ERROR: gradient extraction failed.\n" );
                exit( 1 );
               }
            }
            break ;
         }
      }

      DSET_delete(inset) ;
   }

      

   switch( datum ){

      default:
         fprintf(stderr,
                 "*** Fatal Error ***\n"
                 "*** Somehow ended up with datum = %d\n",datum) ;
         exit(1) ;

      case MRI_float:{
         for( kk=0 ; kk < nval ; kk++ ){
             EDIT_substitute_brick(outset, kk, MRI_float, sum[kk]);
             /* Undo ffac scaling.*/
             if (ffac != 0.0) {
               DSET_BRICK_FACTOR(outset, kk) = 1.0/ffac;
             } else {
               DSET_BRICK_FACTOR(outset, kk) = 0.0;
             }
         }
      }
      break ;

      case MRI_byte:
      case MRI_short:{
         void ** dfim ;
         float gtop=0.0 , fimfac , gtemp ;

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
