#include "mrilib.h"

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset ;
   int iarg=1 ;
   char *prefix = "flip" ;
   void *row ;
   int nx,ny,nz , ii,jj,kk , dcode , ival ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dLRflip [-prefix ppp] dataset\n"
             "Flips the Left-to-Right rows of a dataset.\n"
            ) ;
      exit(0) ;
   }

   mainENTRY("3dLRflip main"); machdep(); AFNI_logger("3dLRflip",argc,argv);

   while( argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       prefix = argv[++iarg] ;
       if( !THD_filename_ok(prefix) ){
         fprintf(stderr,"** Illegal prefix: %s\n",prefix); exit(1);
       }
       iarg++ ; continue ;
     }
   }

   /*-- read data --*/

   dset = THD_open_dataset(argv[iarg]) ;
   if( !ISVALID_DSET(dset) ){ fprintf(stderr,"** CAN'T open dataset\n");exit(1); }
   DSET_mallocize(dset) ; DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){ fprintf(stderr,"** CAN'T load dataset\n");exit(1); }

   nx=DSET_NX(dset); ny=DSET_NY(dset); nz=DSET_NZ(dset);

   if( dset->daxes->xxorient == ORI_R2L_TYPE ||
       dset->daxes->xxorient == ORI_L2R_TYPE   ) dcode = 1 ;
   else
   if( dset->daxes->yyorient == ORI_R2L_TYPE ||
       dset->daxes->yyorient == ORI_L2R_TYPE   ) dcode = 2 ;
   else
   if( dset->daxes->zzorient == ORI_R2L_TYPE ||
       dset->daxes->zzorient == ORI_L2R_TYPE   ) dcode = 3 ;
   else
   { fprintf(stderr,"** Dataset has no L-R axis!\n"); exit(1); }

   /* modify dataset name */

   EDIT_dset_items( dset , ADN_prefix , prefix , ADN_none ) ;
   dset->idcode = MCW_new_idcode() ;  /* 24 Aug 2003 - ooops */
   tross_Make_History( "3dLRflip", argc,argv, dset ) ;

   /* loop over bricks */

   for( ival=0 ; ival < DSET_NVALS(dset) ; ival++ ){

     switch( dcode ){

       case 1:   /* flip x-axis rows */
         for( kk=0 ; kk < nz ; kk++ )
          for( jj=0 ; jj < ny ; jj++ ){
            row = THD_get_dset_row( dset , ival , -dcode , 0,jj,kk ) ;
                  THD_put_dset_row( dset , ival ,  dcode , 0,jj,kk , row ) ;
            free(row) ;
          }
       break ;

       case 2:   /* flip y-axis rows */
         for( kk=0 ; kk < nz ; kk++ )
          for( ii=0 ; ii < nx ; ii++ ){
            row = THD_get_dset_row( dset , ival , -dcode , ii,0,kk ) ;
                  THD_put_dset_row( dset , ival ,  dcode , ii,0,kk , row ) ;
            free(row) ;
          }
       break ;

       case 3:   /* flip z-axis rows */
         for( jj=0 ; jj < ny ; jj++ )
          for( ii=0 ; ii < nx ; ii++ ){
            row = THD_get_dset_row( dset , ival , -dcode , ii,jj,0 ) ;
                  THD_put_dset_row( dset , ival ,  dcode , ii,jj,0 , row ) ;
            free(row) ;
          }
       break ;

     }
   }

   /* done */

   DSET_write(dset) ; exit(0) ;
}
