#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int iarg=1 , dcode=0 , maxgap=2 , nftot=0 ;
   char * prefix="zfillin" , * dstr=NULL;
   THD_3dim_dataset * inset , * outset ;
   MRI_IMAGE * brim ;
   int verb=0 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dZFillin [options] dataset\n"
             "Extracts 1D rows in the given direction from a 3D dataset,\n"
             "searches for zeros that are 'close' to nonzero values in the row,\n"
             "and replaces the zeros with the closest nonzero neighbor.\n"
             "\n"
             "OPTIONS:\n"
             " -maxstep N  = set the maximum distance to a neighbor\n"
             "                [default=2].\n"
             " -dir D     = set the direction of fill to 'D', which can\n"
             "                be one of the following:\n"
             "                  A-P, P-A, I-S, S-I, L-R, R-L, x, y, z\n"
             "                The first 6 are anatomical directions;\n"
             "                the last 3 are reference to the dataset\n"
             "                internal axes [no default value].\n"
             " -prefix P  = set the prefix to 'P' for the output dataset.\n"
             "\n"
             "N.B.: * If the input dataset has more than one sub-brick,\n"
             "        only the first one will be processed.\n"
             "      * At this time, 3dZFillin only works on byte-valued datasets\n"
             "\n"
             "This program's only purpose is to fill up the Talairach Daemon\n"
             "bricks obtained from the UT San Antonio database.\n"
             "\n"
            ) ;
      exit(0) ;
   }

   mainENTRY("3dZFillin main") ; machdep() ; AFNI_logger("3dZfillin",argc,argv) ;

   /*-- scan args --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strncmp(argv[iarg],"-verb",5) == 0 ){
         verb++ ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"*** Illegal string after -prefix!\n"); exit(1) ;
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-maxstep") == 0 ){
         maxgap = strtol( argv[++iarg] , NULL , 10 ) ;
         if( maxgap < 1 ){
            fprintf(stderr,"*** Illegal value after -maxgap!\n"); exit(1);
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-dir") == 0 ){
         dstr = argv[++iarg] ;
         iarg++ ; continue ;
      }

      fprintf(stderr,"*** Illegal option: %s\n",argv[iarg]) ; exit(1) ;
   }

   if( dstr == NULL ){
      fprintf(stderr,"*** No -dir option on command line!\n"); exit(1);
   }
   if( iarg >= argc ){
      fprintf(stderr,"*** No input dataset on command line!\n"); exit(1);
   }

   inset = THD_open_dataset( argv[iarg] ) ;
   if( inset == NULL ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[iarg]); exit(1);
   }

   outset = EDIT_empty_copy( inset ) ;
   EDIT_dset_items( outset , ADN_prefix , prefix , ADN_none ) ;
   if( THD_is_file( DSET_HEADNAME(outset) ) ){
      fprintf(stderr,"** Output file %s exists -- cannot overwrite!\n",
              DSET_HEADNAME(outset) ) ;
      exit(1) ;
   }

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dZFillin" , argc,argv , outset ) ;

   if( DSET_NVALS(inset) > 1 ){
      fprintf(stderr,"++ WARNING: input dataset has more than one sub-brick!\n");
      EDIT_dset_items( outset ,
                         ADN_ntt   , 0 ,
                         ADN_nvals , 1 ,
                       ADN_none ) ;
   }

   if( DSET_BRICK_TYPE(outset,0) != MRI_byte ){
      fprintf(stderr,"*** This program only works on byte datasets!\n");
      exit(1) ;
   }

   switch( *dstr ){
      case 'x': dcode = 1 ; break ;
      case 'y': dcode = 2 ; break ;
      case 'z': dcode = 3 ; break ;

      default:
        if( *dstr == ORIENT_tinystr[outset->daxes->xxorient][0] ||
            *dstr == ORIENT_tinystr[outset->daxes->xxorient][1]   ) dcode = 1 ;

        if( *dstr == ORIENT_tinystr[outset->daxes->yyorient][0] ||
            *dstr == ORIENT_tinystr[outset->daxes->yyorient][1]   ) dcode = 2 ;

        if( *dstr == ORIENT_tinystr[outset->daxes->zzorient][0] ||
            *dstr == ORIENT_tinystr[outset->daxes->zzorient][1]   ) dcode = 3 ;
      break ;
   }
   if( dcode == 0 ){
      fprintf(stderr,"*** Illegal -dir direction!\n") ; exit(1) ;
   }
   if( verb )
      fprintf(stderr,"++ Direction = axis %d in dataset\n",dcode) ;

   DSET_load(inset) ;
   if( !DSET_LOADED(inset) ){
      fprintf(stderr,"*** Can't load dataset %s\n",argv[iarg]); exit(1);
   }
   brim = mri_copy( DSET_BRICK(inset,0) ) ;
   DSET_unload(inset) ;
   EDIT_substitute_brick( outset , 0 , brim->kind , mri_data_pointer(brim) ) ;
   nftot = THD_dataset_zfillin( outset , 0 , dcode , maxgap ) ;
   fprintf(stderr,"++ Number of voxels filled = %d\n",nftot) ;
   DSET_write(outset) ;
   exit(0) ;
}
