/*------------------------------------------------------------------------
  This program does something, but nobody is sure what.
  14 Apr 2003 - RWCox.
--------------------------------------------------------------------------*/

#include "mrilib.h"

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *inset , *outset ;
   int nx,ny,nz,nxyz,nval , kk ;
   char * prefix = "warped" ;
   int nopt=1 , verb=0 ;
   int use_matvec=0 , use_scale=0 ;
   THD_vecmat matvec ;
   float scale_a, scale_b, scale_c ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dWarp [options] dataset\n"
             "Warp (spatially transform) a 3D dataset.\n"
             "\n"
             "Options\n"
             "  -verb        = Print out some information along the way.\n"
             "  -prefix ppp  = Sets the prefix of the output dataset.\n"
             "  -scale a b c = Scale dataset size by factors 'a', 'b', 'c'\n"
             "                  along the x-, y-, and z-axes.\n"
             "  -matvec mmm  = Read a 3x4 affine transform matrix+vector\n"
             "                  from file 'mmm'.\n"
             "\n"
             "  -linear    } \n"
             "  -cubic     }=> Chooses spatial interpolation method.\n"
             "  -NN        } \n"
            ) ;
      exit(0) ;
   }

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dWarp main"); machdep(); AFNI_logger("3dWarp",argc,argv);

   /*-- command line options --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

      /*-----*/

      if( strcmp(argv[nopt],"-prefix") == 0 ){
        if( ++nopt >= argc ){
          fprintf(stderr,"** ERROR: need an argument after -prefix!\n"); exit(1);
        }
        prefix = argv[nopt] ; nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-verbose",5) == 0 ){
         verb++ ; nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-matvec",7) == 0 ){
        MRI_IMAGE *matim ; float *matar ;

        if( use_matvec ){
          fprintf(stderr,"** Can't have two -matvec options!\n"); exit(0);
        }
        if( use_scale ){
          fprintf(stderr,"** Can't have -matvec AND -scale options!\n"); exit(0);
        }
        if( ++nopt >= argc ){
           fprintf(stderr,"** ERROR: need an argument after -matvec!\n"); exit(1);
        }
        matim = mri_read_ascii( argv[nopt] ) ;
        if( matim == NULL ){
          fprintf(stderr,"** Can't read -matvec file %s\n",argv[nopt]); exit(1);
        }
        if( matim->nx != 4 || matim->ny != 3 ){
          fprintf(stderr,"** -matvec file not 3x4!\n"); exit(1);
        }
        matar = MRI_FLOAT_PTR(matim) ;
        LOAD_MAT  ( matvec.mm, matar[0],matar[1],matar[2],
                               matar[4],matar[5],matar[6],
                               matar[8],matar[9],matar[10] ) ;
        LOAD_FVEC3( matvec.vv, matar[3],matar[7],matar[11] ) ;
        use_matvec = 1 ; nopt++ ; continue ;
      }

      /*-----*/

      if( strncmp(argv[nopt],"-scale",6) == 0 ){
        if( use_scale ){
          fprintf(stderr,"** Can't have two -scale options!\n"); exit(0);
        }
        if( use_matvec ){
          fprintf(stderr,"** Can't have -scale AND -matvec options!\n"); exit(0);
        }
        if( nopt+3 >= argc ){
          fprintf(stderr,"** Need 3 arguments after -scale!\n"); exit(0);
        }
        scale_a = strtod( argv[++nopt] , NULL ) ;
        scale_b = strtod( argv[++nopt] , NULL ) ;
        scale_c = strtod( argv[++nopt] , NULL ) ;
        if( scale_a <= 0.0 ){
          fprintf(stderr,"** First argument after -scale is illegal!\n");
        }
        if( scale_b <= 0.0 ){
          fprintf(stderr,"** Second argument after -scale is illegal!\n");
        }
        if( scale_c <= 0.0 ){
          fprintf(stderr,"** Third argument after -scale is illegal!\n");
        }
        if( scale_a <= 0.0 || scale_b <= 0.0 || scale_c <= 0.0 ) exit(1) ;
        if( scale_a == 1.0 && scale_b == 1.0 && scale_c == 1.0 ){
          fprintf(stderr,"** Can't have all scale factors = 1!\n"); exit(1);
        }
        use_scale = 1 ; nopt++ ; continue ;
      }

      fprintf(stderr,"** ERROR: unknown option %s\n",argv[nopt]) ;
      exit(1) ;
   }

   /*-- last argument should be a dataset --*/

   if( nopt != argc-1 ){
     fprintf(stderr,"** Command line should have exactly 1 dataset!\n"
                    "** Whereas there seems to be %d of them!\n",
             argc-nopt ) ;
     exit(1) ;
   }

   /*-- input dataset header --*/

   inset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(inset) ){
      fprintf(stderr,"** ERROR: can't open dataset %s\n",argv[nopt]) ;
      exit(1) ;
   }

   /*-- make empty output dataset --*/

   nx   = DSET_NX(inset) ;
   ny   = DSET_NY(inset) ;
   nz   = DSET_NZ(inset) ; nxyz= nx*ny*nz;
   nval = DSET_NVALS(inset) ;

   outset = EDIT_empty_copy( inset ) ;

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dWarp" , argc,argv , outset ) ;

   EDIT_dset_items( outset , ADN_prefix,prefix , ADN_none ) ;

   if( THD_is_file(outset->dblk->diskptr->header_name) ){
     fprintf(stderr,
             "*** Output file %s already exists -- cannot continue!\n",
             outset->dblk->diskptr->header_name ) ;
     exit(1) ;
   }

   /*-- read data from disk --*/

   DSET_load(inset) ;
   if( !DSET_LOADED(inset) ){
      fprintf(stderr,"** ERROR: can't read data from dataset %s\n",argv[nopt]) ;
      exit(1) ;
   }

   if( verb ) fprintf(stderr,"  ++ read in dataset %s\n",argv[nopt]) ;

   exit(0) ;
}
