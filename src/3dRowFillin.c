#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int iarg=1 , dcode=0 , maxgap=9 , nftot=0 ;
   char * prefix="rowfillin" , * dstr=NULL;
   THD_3dim_dataset * inset , * outset ;
   MRI_IMAGE * brim ;
   int verb=0 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dRowFillin [options] dataset\n"
             "Extracts 1D rows in the given direction from a 3D dataset,\n"
             "searches for blank (zero) regions, and fills them in if\n"
             "the blank region isn't too large and it is flanked by\n"
             "the same value on either edge.  For example:\n"
             "     input row = 0 1 2 0 0 2 3 0 3 0 0 4 0\n"
             "    output row = 0 1 2 2 2 2 3 3 3 0 0 4 0\n"
             "\n"
             "OPTIONS:\n"
             " -maxgap N  = set the maximum length of a blank region that\n"
             "                will be filled in to 'N' [default=9].\n"
             " -dir D     = set the direction of fill to 'D', which can\n"
             "                be one of the following:\n"
             "                  A-P, P-A, I-S, S-I, L-R, R-L, x, y, z\n"
             "                The first 6 are anatomical directions;\n"
             "                the last 3 are reference to the dataset\n"
             "                internal axes [no default value].\n"
             " -prefix P  = set the prefix to 'P' for the output dataset.\n"
             "\n"
             "N.B.: If the input dataset has more than one sub-brick,\n"
             "      only the first one will be processed.\n"
             "\n"
             "The intention of this program is to let you fill in slice gaps\n"
             "made when drawing ROIs with the 'Draw Dataset' plugin.  If you\n"
             "draw every 5th coronal slice, say, then you could fill in using\n"
             "  3dRowFillin -maxgap 4 -dir A-P -prefix fredfill fred+orig\n"
             "\n"
            ) ;
      exit(0) ;
   }

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

      if( strcmp(argv[iarg],"-maxgap") == 0 ){
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
   tross_Make_History( "3dRowFillin" , argc,argv , outset ) ;

   if( DSET_NVALS(inset) > 1 ){
      fprintf(stderr,"++ WARNING: input dataset has more than one sub-brick!\n");
      EDIT_dset_items( outset ,
                         ADN_ntt   , 0 ,
                         ADN_nvals , 1 ,
                       ADN_none ) ;
   }

   if( DSET_BRICK_TYPE(outset,0) != MRI_short &&
       DSET_BRICK_TYPE(outset,0) != MRI_byte    ){

      fprintf(stderr,"*** This program only works on byte and short dataset!\n");
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
   nftot = THD_dataset_rowfillin( outset , 0 , dcode , maxgap ) ;
   fprintf(stderr,"++ Number of voxels filled = %d\n",nftot) ;
   DSET_write(outset) ;
   exit(0) ;
}
