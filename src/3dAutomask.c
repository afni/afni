#include "mrilib.h"

#undef ALLOW_FILLIN  /* 28 May 2002 */

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset , *mset ;
   char *prefix = "automask" ;
   byte *mask ;
   int iarg=1 , fillin=0 , nmask,nfill , dilate=0 , dd ;
   float SIhh=0.0 ;        /* 06 Mar 2003 */
   int   SIax=0 , SIbot,SItop ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAutomask [options] dataset\n"
             "Input dataset is EPI 3D+time.\n"
             "Output dataset is a brain-only mask dataset.\n"
             "Method:\n"
             " + Uses 3dClipLevel algorithm to find clipping level.\n"
             " + Keeps only the largest connected component of the\n"
             "   supra-threshold voxels, after an erosion/dilation step.\n"
             " + Writes result as a 'fim' type of functional dataset.\n"
             "Options:\n"
             "  -prefix ppp = Write mask into dataset with prefix 'ppp'.\n"
             "                 [default='automask']\n"
             "  -dilate nd  = Dilate the mask outwards 'nd' times.\n"
#ifdef ALLOW_FILLIN
             "  -fillin nnn = Fill in holes inside the mask of width up\n"
             "                 to 'nnn' voxels. [default=0=no fillin]\n"
#endif
             "  -SI hh      = After creating the mask, find the most superior\n"
             "                 voxel, then zero out everything more than 'hh'\n"
             "                 millimeters inferior to that.  hh=130 seems to\n"
             "                 be decent (for human brains).\n"
            ) ;
      exit(0) ;
   }

   mainENTRY("3dAutomask main"); machdep(); AFNI_logger("3dAutomask",argc,argv);

   /*-- options --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-SI") == 0 ){        /* 06 Mar 2003 */
        SIhh = strtod( argv[++iarg] , NULL ) ;
        if( SIhh <= 0.0 ){
          fprintf(stderr,"** -SI value %f is illegal!\n",SIhh) ;
          exit(1) ;
        }
        iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
           fprintf(stderr,"** -prefix %s is illegal!\n",prefix) ;
           exit(1) ;
         }
         iarg++ ; continue ;
      }

#ifdef ALLOW_FILLIN
      if( strcmp(argv[iarg],"-fillin") == 0 ){
         fillin = strtol( argv[++iarg] , NULL , 10 ) ;
         if( fillin <  0 ){
           fprintf(stderr,"** -fillin %s is illegal!\n",argv[iarg]) ;
           exit(1) ;
         } else if( fillin > 0 ){
           fillin = (fillin+2) / 2 ;
         }
         iarg++ ; continue ;
      }
#endif

      if( strcmp(argv[iarg],"-dilate") == 0 ){
         dilate = strtol( argv[++iarg] , NULL , 10 ) ;
         if( dilate < 0 ){
           fprintf(stderr,"** -dilate %s is illegal!\n",argv[iarg]); exit(1);
         }
         iarg++ ; continue ;
      }

      fprintf(stderr,"** ILLEGAL option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /*-- read data --*/

   dset = THD_open_dataset(argv[iarg]) ;
   if( !ISVALID_DSET(dset) ){ fprintf(stderr,"** CAN'T open dataset\n");exit(1); }
   if( DSET_BRICK_TYPE(dset,0) != MRI_short &&
       DSET_BRICK_TYPE(dset,0) != MRI_byte  &&
       DSET_BRICK_TYPE(dset,0) != MRI_float   ){
      fprintf(stderr,"** ILLEGAL dataset type\n"); exit(1);
   }
   fprintf(stderr,"++ Loading dataset\n") ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){ fprintf(stderr,"** CAN'T load dataset\n");exit(1); }

   /*** do all the real work now ***/

   fprintf(stderr,"++ Forming automask\n") ;
   mask = THD_automask( dset ) ;
   if( mask == NULL ){
     fprintf(stderr,"** Mask creation fails for unknown reasons!\n"); exit(1);
   }

   /* 30 Aug 2002 (modified 05 Mar 2003 to do fillin, etc, after dilation) */

   if( dilate > 0 ){
     int ii,nx,ny,nz , nmm ;
     fprintf(stderr,"++ Dilating automask\n") ;
     nx = DSET_NX(dset) ; ny = DSET_NY(dset) ; nz = DSET_NZ(dset) ;
     nmm = 1 ;
     ii  = rint(0.032*nx) ; nmm = MAX(nmm,ii) ;
     ii  = rint(0.032*ny) ; nmm = MAX(nmm,ii) ;
     ii  = rint(0.032*nz) ; nmm = MAX(nmm,ii) ;
     for( dd=0 ; dd < dilate ; dd++ ){
       THD_mask_dilate           ( nx,ny,nz , mask, 3   ) ;
       THD_mask_fillin_completely( nx,ny,nz , mask, nmm ) ;
     }
     nmm = nx*ny*nz ;
     for( ii=0 ; ii < nmm ; ii++ ) mask[ii] = !mask[ii] ;
     THD_mask_clust( nx,ny,nz, mask ) ;
     for( ii=0 ; ii < nmm ; ii++ ) mask[ii] = !mask[ii] ;
   }

   /* 18 Apr 2002: print voxel count */

   nmask = THD_countmask( DSET_NVOX(dset) , mask ) ;
   fprintf(stderr,"++ %d voxels in the mask [out of %d: %.2f%%]\n",
           nmask,DSET_NVOX(dset), (100.0*nmask)/DSET_NVOX(dset) ) ;
   if( nmask == 0 ){
      fprintf(stderr,"** Quitting without saving mask\n"); exit(1);
   }

   /* 18 Apr 2002: maybe fill in voxels */

#ifdef ALLOW_FILLIN
   if( fillin > 0 ){
     nfill = THD_mask_fillin_completely(
                 DSET_NX(dset),DSET_NY(dset),DSET_NZ(dset), mask, fillin ) ;
     fprintf(stderr,"++ %d voxels filled in; %d voxels total\n",
             nfill,nfill+nmask ) ;
   }
#endif

   /** 04 Jun 2002: print cut plane report **/

   { int nx=DSET_NX(dset), ny=DSET_NY(dset), nz=DSET_NZ(dset), nxy=nx*ny ;
     int ii,jj,kk ;

#if 0
     { int xm=-1,xp=-1,ym=-1,yp=-1,zm=-1,zp=-1 ;
       THD_autobbox( dset , &xm,&xp , &ym,&yp , &zm,&zp ) ;
       fprintf(stderr,"++ Auto bbox: x=%d..%d  y=%d..%d  z=%d..%d\n",
               xm,xp,ym,yp,zm,zp ) ;
     }
#endif

     for( ii=0 ; ii < nx ; ii++ )
       for( kk=0 ; kk < nz ; kk++ )
         for( jj=0 ; jj < ny ; jj++ )
           if( mask[ii+jj*nx+kk*nxy] ) goto CP5 ;
     CP5: fprintf(stderr,"++ first %3d x-planes are zero [from %c]\n",
                  ii,ORIENT_tinystr[dset->daxes->xxorient][0]) ;
     if( SIhh > 0.0 && ORIENT_tinystr[dset->daxes->xxorient][0] == 'S' ){
       SIax = 1 ; SIbot = ii + (int)(SIhh/fabs(DSET_DX(dset))+0.5) ; SItop = nx-1 ;
#if 0
fprintf(stderr,"SIax=%d SIbot=%d SItop=%d\n",SIax,SIbot,SItop) ;
#endif
     }

     for( ii=nx-1 ; ii >= 0 ; ii-- )
       for( kk=0 ; kk < nz ; kk++ )
         for( jj=0 ; jj < ny ; jj++ )
           if( mask[ii+jj*nx+kk*nxy] ) goto CP6 ;
     CP6: fprintf(stderr,"++ last  %3d x-planes are zero [from %c]\n",
                  nx-1-ii,ORIENT_tinystr[dset->daxes->xxorient][1]) ;
     if( SIhh > 0.0 && ORIENT_tinystr[dset->daxes->xxorient][1] == 'S' ){
       SIax = 1 ; SIbot = 0 ; SItop = ii - (int)(SIhh/fabs(DSET_DX(dset))+0.5) ;
#if 0
fprintf(stderr,"SIax=%d SIbot=%d SItop=%d\n",SIax,SIbot,SItop) ;
#endif
     }

     for( jj=0 ; jj < ny ; jj++ )
       for( kk=0 ; kk < nz ; kk++ )
         for( ii=0 ; ii < nx ; ii++ )
           if( mask[ii+jj*nx+kk*nxy] ) goto CP3 ;
     CP3: fprintf(stderr,"++ first %3d y-planes are zero [from %c]\n",
                  jj,ORIENT_tinystr[dset->daxes->yyorient][0]) ;
     if( SIhh > 0.0 && ORIENT_tinystr[dset->daxes->yyorient][0] == 'S' ){
       SIax = 2 ; SIbot = jj + (int)(SIhh/fabs(DSET_DY(dset))+0.5) ; SItop = ny-1 ;
#if 0
fprintf(stderr,"SIax=%d SIbot=%d SItop=%d\n",SIax,SIbot,SItop) ;
#endif
     }

     for( jj=ny-1 ; jj >= 0 ; jj-- )
       for( kk=0 ; kk < nz ; kk++ )
         for( ii=0 ; ii < nx ; ii++ )
           if( mask[ii+jj*nx+kk*nxy] ) goto CP4 ;
     CP4: fprintf(stderr,"++ last  %3d y-planes are zero [from %c]\n",
                  ny-1-jj,ORIENT_tinystr[dset->daxes->yyorient][1]) ;
     if( SIhh > 0.0 && ORIENT_tinystr[dset->daxes->yyorient][1] == 'S' ){
       SIax = 2 ; SIbot = 0 ; SItop = jj - (int)(SIhh/fabs(DSET_DY(dset))+0.5) ;
#if 0
fprintf(stderr,"SIax=%d SIbot=%d SItop=%d\n",SIax,SIbot,SItop) ;
#endif
     }

     for( kk=0 ; kk < nz ; kk++ )
       for( jj=0 ; jj < ny ; jj++ )
         for( ii=0 ; ii < nx ; ii++ )
           if( mask[ii+jj*nx+kk*nxy] ) goto CP1 ;
     CP1: fprintf(stderr,"++ first %3d z-planes are zero [from %c]\n",
                  kk,ORIENT_tinystr[dset->daxes->zzorient][0]) ;
     if( SIhh > 0.0 && ORIENT_tinystr[dset->daxes->zzorient][0] == 'S' ){
       SIax = 3 ; SIbot = kk + (int)(SIhh/fabs(DSET_DZ(dset))+0.5) ; SItop = nz-1 ;
#if 0
fprintf(stderr,"SIax=%d SIbot=%d SItop=%d\n",SIax,SIbot,SItop) ;
#endif
     }

     for( kk=nz-1 ; kk >= 0 ; kk-- )
       for( jj=0 ; jj < ny ; jj++ )
         for( ii=0 ; ii < nx ; ii++ )
           if( mask[ii+jj*nx+kk*nxy] ) goto CP2 ;
     CP2: fprintf(stderr,"++ last  %3d z-planes are zero [from %c]\n",
                  nz-1-kk,ORIENT_tinystr[dset->daxes->zzorient][1]) ;
     if( SIhh > 0.0 && ORIENT_tinystr[dset->daxes->zzorient][1] == 'S' ){
       SIax = 3 ; SIbot = 0 ; SItop = kk - (int)(SIhh/fabs(DSET_DZ(dset))+0.5) ;
#if 0
fprintf(stderr,"SIax=%d SIbot=%d SItop=%d\n",SIax,SIbot,SItop) ;
#endif
     }

     /* 06 Mar 2003: cut off stuff below SIhh mm from most Superior point */

     if( SIax > 0 && SIbot <= SItop ){
       char *cax="xyz" ;
       fprintf(stderr,"++ SI clipping mask along axis %c from %d..%d\n" ,
               cax[SIax-1] , SIbot,SItop ) ;
       switch( SIax ){
         case 1:
           for( ii=SIbot ; ii <= SItop ; ii++ )
             for( kk=0 ; kk < nz ; kk++ )
               for( jj=0 ; jj < ny ; jj++ ) mask[ii+jj*nx+kk*nxy] = 0 ;
         break ;
         case 2:
           for( jj=SIbot ; jj <= SItop ; jj++ )
             for( kk=0 ; kk < nz ; kk++ )
               for( ii=0 ; ii < nx ; ii++ ) mask[ii+jj*nx+kk*nxy] = 0 ;
         break ;
         case 3:
           for( kk=SIbot ; kk <= SItop ; kk++ )
             for( jj=0 ; jj < ny ; jj++ )
               for( ii=0 ; ii < nx ; ii++ ) mask[ii+jj*nx+kk*nxy] = 0 ;
         break ;
       }
       nmask = THD_countmask( DSET_NVOX(dset) , mask ) ;
       fprintf(stderr,"++ %d voxels left [out of %d]\n",nmask,DSET_NVOX(dset)) ;
     }
   }

   DSET_unload( dset ) ;  /* don't need data any more */

   /* create output dataset */

   mset = EDIT_empty_copy( dset ) ;
   EDIT_dset_items( mset ,
                      ADN_prefix     , prefix   ,
                      ADN_datum_all  , MRI_byte ,
                      ADN_nvals      , 1        ,
                      ADN_ntt        , 0        ,
                      ADN_type       , HEAD_FUNC_TYPE ,
                      ADN_func_type  , FUNC_FIM_TYPE ,
                    ADN_none ) ;
   EDIT_substitute_brick( mset , 0 , MRI_byte , mask ) ;

   /* 16 Apr 2002: make history */

   tross_Copy_History( dset , mset ) ;
   tross_Make_History( "3dAutomask", argc,argv, mset ) ;

   fprintf(stderr,"++ Writing mask dataset to %s\n",DSET_HEADNAME(mset)) ;
   DSET_write( mset ) ;
   exit(0) ;
}
