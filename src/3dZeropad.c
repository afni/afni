#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int iarg ;
   THD_3dim_dataset *inset , *outset ;
   int add_I=0 , add_S=0 , add_A=0 , add_P=0 , add_L=0 , add_R=0 ;
   char * prefix="zeropad" ;
   int nxold,nyold,nzold , nxnew,nynew,nznew , nxyold,nxynew ,
       nxbot=0,nxtop=0 , nybot=0,nytop=0 , nzbot=0,nztop=0    ;
   int ii,jj,kk , iv , iibot,iitop , jjbot,jjtop , kkbot,kktop ;

   THD_ivec3 iv_nxyz ;
   THD_fvec3 fv_xyzorg ;

   MRI_IMAGE * oldim ;
   void * vnew ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dZeropad [options] dataset\n"
             "Adds planes of zeros to a dataset (i.e., pads it out).\n"
             "\n"
             "Options:\n"
             "  -I n = adds 'n' planes of zero at the Inferior edge\n"
             "  -S n = adds 'n' planes of zero at the Superior edge\n"
             "  -A n = adds 'n' planes of zero at the Anterior edge\n"
             "  -P n = adds 'n' planes of zero at the Posterior edge\n"
             "  -L n = adds 'n' planes of zero at the Left edge\n"
             "  -R n = adds 'n' planes of zero at the Right edge\n"
             "\n"
             " -prefix ppp = write result into dataset with prefix 'ppp'\n"
             "                 [default = 'zeropad']\n"
             "\n"
             "Nota Bene:\n"
             " * You can use negative values of n to cut planes off the edges\n"
             "     of a dataset.  At least one plane must be added/removed\n"
             "     or the program won't do anything.\n"
             " * Anat parent and Talairach markers are NOT preserved in the\n"
             "     new dataset.\n"
             " * If the old dataset has z-slice-dependent time offsets, and\n"
             "     if new z-planes are added, all the slice-dependent time\n"
             "     offsets will be removed.\n"
             " * You can use program '3dinfo' to find out how many planes\n"
             "     a dataset has in each direction.\n"
             " * Program works for byte-, short-, float-, and complex-valued\n"
             "     datasets.\n"
             " * You can use a sub-brick selector on the input dataset.\n"
             " * 3dZeropad won't overwrite an existing dataset (I hope).\n"
             "\n"
             " Author: RWCox - July 2000\n"
           ) ;
      exit(0) ;
   }

   /*-- read command line options --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      /*- -I, -S, etc. -*/

      if( strlen(argv[iarg]) == 2 ){
         switch( argv[iarg][1] ){
            case 'I': add_I = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'S': add_S = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'A': add_A = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'P': add_P = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'L': add_L = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'R': add_R = (int) strtod(argv[++iarg],NULL) ; break ;

            default:
               fprintf(stderr,"*** Illegal option: %s\n",argv[iarg]) ; exit(1) ;
         }

         iarg++ ; continue ;  /* skip to next argument */
      }

      /*- -prefix -*/

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"*** Illegal string after -prefix!\n"); exit(1) ;
         }
         iarg++ ; continue ;
      }

      /*- what the hell? -*/

      fprintf(stderr,"*** Illegal option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /*- check to see if the user asked for something, anything -*/

   if( add_I==0 && add_S==0 && add_P==0 && add_A==0 && add_L==0 && add_R==0 ){
      fprintf(stderr,"*** Don't you want to DO something!?\n"); exit(1);
   }

   /*-- read the input dataset --*/

   if( iarg >= argc ){
      fprintf(stderr,"*** No input dataset on command line!\n"); exit(1);
   }

#if 0
   if( strncmp(argv[iarg],"3dcalc(",7) == 0 ){
      fprintf(stderr,"*** Can't use '3dcalc()' input datasets here!\n"); exit(1);
   }
#endif

   inset = THD_open_dataset( argv[iarg] ) ;
   if( inset == NULL ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[iarg]); exit(1);
   }

#if 0
   if( DSET_IS_MASTERED(inset) ){
      fprintf(stderr,"*** Can't use partial datasets!\n"); exit(1);
   }
#endif

   /*-- map add_? values into dataset xyz coordinate directions --*/

   nxold = DSET_NX(inset) ;
   nyold = DSET_NY(inset) ;
   nzold = DSET_NZ(inset) ;

   /* comput n?top and n?bot, the number of planes to add at
      the top and bottom of the ? direction, for ? = x, y, or z */

   switch( inset->daxes->xxorient ){
      default: fprintf(stderr,"*** Unknown orientation codes in dataset!\n") ;
               exit(1) ;

      case ORI_R2L_TYPE: nxtop = add_L ; nxbot = add_R ; break ;
      case ORI_L2R_TYPE: nxtop = add_R ; nxbot = add_L ; break ;
      case ORI_P2A_TYPE: nxtop = add_A ; nxbot = add_P ; break ;
      case ORI_A2P_TYPE: nxtop = add_P ; nxbot = add_A ; break ;
      case ORI_I2S_TYPE: nxtop = add_S ; nxbot = add_I ; break ;
      case ORI_S2I_TYPE: nxtop = add_I ; nxbot = add_S ; break ;
   }

   switch( inset->daxes->yyorient ){
      default: fprintf(stderr,"*** Unknown orientation codes in dataset!\n") ;
               exit(1) ;

      case ORI_R2L_TYPE: nytop = add_L ; nybot = add_R ; break ;
      case ORI_L2R_TYPE: nytop = add_R ; nybot = add_L ; break ;
      case ORI_P2A_TYPE: nytop = add_A ; nybot = add_P ; break ;
      case ORI_A2P_TYPE: nytop = add_P ; nybot = add_A ; break ;
      case ORI_I2S_TYPE: nytop = add_S ; nybot = add_I ; break ;
      case ORI_S2I_TYPE: nytop = add_I ; nybot = add_S ; break ;
   }

   switch( inset->daxes->zzorient ){
      default: fprintf(stderr,"*** Unknown orientation codes in dataset!\n") ;
               exit(1) ;

      case ORI_R2L_TYPE: nztop = add_L ; nzbot = add_R ; break ;
      case ORI_L2R_TYPE: nztop = add_R ; nzbot = add_L ; break ;
      case ORI_P2A_TYPE: nztop = add_A ; nzbot = add_P ; break ;
      case ORI_A2P_TYPE: nztop = add_P ; nzbot = add_A ; break ;
      case ORI_I2S_TYPE: nztop = add_S ; nzbot = add_I ; break ;
      case ORI_S2I_TYPE: nztop = add_I ; nzbot = add_S ; break ;
   }

   nxnew = nxold + nxbot + nxtop ;  /* dimensions of new bricks */
   nynew = nyold + nybot + nytop ;
   nznew = nzold + nzbot + nztop ;

   nxyold = nxold * nyold ;         /* for computing subscripts */
   nxynew = nxnew * nynew ;

   iibot = MAX(0,-nxbot) ; iitop = MIN(nxold,nxold+nxtop) ;  /* range of data */
   jjbot = MAX(0,-nybot) ; jjtop = MIN(nyold,nyold+nytop) ;  /* in old dataset */
   kkbot = MAX(0,-nzbot) ; kktop = MIN(nzold,nzold+nztop) ;

   if( nxnew < 2 || iibot >= iitop ||   /* check for reasonable sizes */
       nynew < 2 || jjbot >= jjtop ||   /* and ranges of dataset     */
       nznew < 2 || kkbot >= kktop   ){

      fprintf(stderr,"*** Can't cut dataset down in size that much!\n") ;
      exit(1) ;
   }

   /*-- create the shell of the new dataset --*/

   outset = EDIT_empty_copy( inset ) ;

   LOAD_IVEC3( iv_nxyz , nxnew,nynew,nznew ) ;

   LOAD_FVEC3( fv_xyzorg , inset->daxes->xxorg - nxbot * inset->daxes->xxdel ,
                           inset->daxes->yyorg - nybot * inset->daxes->yydel ,
                           inset->daxes->zzorg - nzbot * inset->daxes->zzdel  ) ;

   EDIT_dset_items( outset ,
                       ADN_prefix , prefix    ,
                       ADN_nxyz   , iv_nxyz   ,
                       ADN_xyzorg , fv_xyzorg ,
                    ADN_none ) ;

   if( THD_is_file(DSET_HEADNAME(outset)) ){
      fprintf(stderr,"*** Output dataset already exists!\n"); exit(1);
   }

   /* Don't need to change the bounding box, since it isn't written to disk. */
   /* However, changing dimensions means old anat parent is no longer valid! */

   EDIT_ZERO_ANATOMY_PARENT_ID( outset ) ;
   outset->anat_parent_name[0] = '\0' ;

   /* if changing number of slices, can't keep slice-dependent time shifts! */

   if( nznew != nzold && outset->taxis != NULL && outset->taxis->nsl > 0 )
      EDIT_dset_items( outset , ADN_nsl , 0 , ADN_none ) ;

   /*-- now read the old dataset in, and make bricks for the new dataset --*/

   DSET_load(inset) ;
   if( !DSET_LOADED(inset) ){
      fprintf(stderr,"*** Can't read input dataset BRIK!\n"); exit(1);
   }

   for( iv=0 ; iv < DSET_NVALS(inset) ; iv++ ){

      /* create a brick of zeros */

      oldim = DSET_BRICK(inset,iv) ;  /* image structure of old brick */

      vnew  = calloc( nxnew*nynew*nznew , oldim->pixel_size ) ;  /* new brick */
      if( vnew == NULL ){
         fprintf(stderr,"*** Can't malloc space for new sub-brick %d\n",iv) ;
         exit(1) ;
      }

      /* macros for computing 1D subscripts from 3D indices */

#define SNEW(i,j,k) ((i+nxbot)+(j+nybot)*nxnew+(k+nzbot)*nxynew)
#define SOLD(i,j,k) (i+j*nxold+k*nxyold)

      switch( oldim->kind ){  /* copy old into new */

         default:
            fprintf(stderr,"*** Illegal sub-brick type %d at index %d\n",
                    oldim->kind,iv) ;
            exit(1) ;

         case MRI_byte:{
            byte * bnew = (byte *) vnew, * bold = mri_data_pointer(oldim) ;
            for( kk=kkbot ; kk < kktop ; kk++ )
               for( jj=jjbot ; jj < jjtop ; jj++ )
                  for( ii=iibot ; ii < iitop ; ii++ )
                     bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
         }
         break ;

         case MRI_short:{
            short * bnew = (short *) vnew, * bold = mri_data_pointer(oldim) ;
            for( kk=kkbot ; kk < kktop ; kk++ )
               for( jj=jjbot ; jj < jjtop ; jj++ )
                  for( ii=iibot ; ii < iitop ; ii++ )
                     bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
         }
         break ;

         case MRI_float:{
            float * bnew = (float *) vnew, * bold = mri_data_pointer(oldim) ;
            for( kk=kkbot ; kk < kktop ; kk++ )
               for( jj=jjbot ; jj < jjtop ; jj++ )
                  for( ii=iibot ; ii < iitop ; ii++ )
                     bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
         }
         break ;

         case MRI_complex:{
            complex * bnew = (complex *) vnew, * bold = mri_data_pointer(oldim) ;
            for( kk=kkbot ; kk < kktop ; kk++ )
               for( jj=jjbot ; jj < jjtop ; jj++ )
                  for( ii=iibot ; ii < iitop ; ii++ )
                     bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
         }
         break ;

      } /* end of switch on sub-brick type */

      DSET_unload_one(inset,iv) ; /* don't need this no more */

      EDIT_substitute_brick( outset , iv , oldim->kind , vnew ) ;

   } /* end of loop on sub-brick index */

   DSET_delete(inset) ;

   /*-- finished --*/

   DSET_write(outset) ;
   exit(0) ;
}
