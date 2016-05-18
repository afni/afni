#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

static int myTHD_extract_nbhd( THD_3dim_dataset *dset, byte *mask,
                               int xx, int yy, int zz, MCW_cluster *nbhd,
                               int *ivar , float *tsar ) ;

/*------------------------------------------------------------------------*/

#ifndef USE_OMP
static void vstep_print(void)
{
   static char xx[10] = "0123456789" ; static int vn=0 ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}
#endif

/*------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL , *outset=NULL ;
   MCW_cluster *nbhd=NULL ;
   byte *mask=NULL ; int mask_nx=0,mask_ny=0,mask_nz=0 , automask=0 ;
   char *prefix="./LocalACF" ;
   char *evprefix=NULL ; int nev ;
   int iarg=1 , verb=1 , ntype=0 , kk,nx,ny,nz,nxy,nxyz,nvals,nt , vstep=0 ;
   float na,nb,nc , dx,dy,dz ;
   int nmask=0 , domean=0 , use_nonmask=0 ;
   float *evar=NULL,*fvar=NULL ;
   unsigned int gseed ;
   int despike=0 , corder=0 , unif=1 ;

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("\n"
       "Usage: 3dLocalACF [options] inputdataset\n"
       "\n"
       "Options:\n"
       "--------\n"
       " -prefix   ppp\n"
       " -input    inputdataset\n"
       " -mask     maskdataset\n"
       " -automask\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dLocalACF"); mainENTRY("3dLocalACF main"); machdep();
   AFNI_logger("3dLocalACF",argc,argv); AUTHOR("Emperor Zhark the Iterator");

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-input") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-input'") ;
       inset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-prefix'") ;
       prefix = strdup(argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       nmask = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",nmask) ;
       if( nmask < 99 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   /*---- deal with input dataset ----*/

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   }
   if( !IS_REAL_TYPE(DSET_BRICK_TYPE(inset,0)) )
     ERROR_exit("Can only process real-valued datasets in this program!") ;
   nt = nvals = DSET_NVALS(inset) ;
   if( nt < 39 )
     ERROR_exit("Must have at least 39 values per voxel in time series dataset '%s'",
                DSET_BRIKNAME(inset) ) ;

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

   /*-- deal with mask --*/

   nx = DSET_NX(inset) ;
   ny = DSET_NY(inset) ; nxy  = nx*ny  ;
   nz = DSET_NZ(inset) ; nxyz = nxy*nz ;

   if( mask != NULL ){
     if( mask_nx != nx ||
         mask_ny != ny ||
         mask_nz != nz   )
       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( automask ){
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     nmask = THD_countmask( DSET_NVOX(inset) , mask ) ;
     INFO_message("Number of voxels in automask = %d",nmask) ;
     if( nmask < 99 ) ERROR_exit("Automask is too small to process") ;

   } else {
     nmask = nxyz ;  /* all voxels */
   }

   /** create output dataset **/

   outset = EDIT_empty_copy(inset) ;
   EDIT_dset_items( outset,
                      ADN_prefix   , prefix,
                      ADN_brick_fac, NULL  ,
                      ADN_nvals    , 4     ,
                      ADN_ntt      , 0     ,
                    ADN_none );
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dLocalACF" , argc,argv , outset ) ;
   for( kk=0 ; kk < 4 ; kk++ )                         /* create bricks */
     EDIT_substitute_brick( outset , kk , MRI_float , NULL ) ;

   if( despike ){             /* 14 Oct 2010 */
     THD_3dim_dataset *qset ;
     INFO_message("Despiking input dataset") ;
     qset = THD_despike9_dataset( inset , mask ) ;
     if( qset == NULL ) ERROR_exit("Despiking fails!?") ;
     DSET_delete(inset) ; inset = qset ;
   }

   if( corder > 0 ){
     int nref=2*corder+3 , jj,iv,kk ;
     float **ref , tm,fac,fq ;
     THD_3dim_dataset *newset ;

     ref = THD_build_trigref( corder , nvals ) ;
     if( ref == NULL ) ERROR_exit("THD_build_trigref failed!") ;

     newset = THD_detrend_dataset( inset , nref , ref , 2 , 1 , mask , NULL ) ;
     if( newset == NULL ) ERROR_exit("detrending failed!") ;

     for( jj=0 ;jj < nref ; jj++ ) free(ref[jj]) ;
     free(ref) ; DSET_delete(inset) ; inset = newset ;
   }

#ifndef USE_OMP
   vstep = (verb && nxyz > 999) ? nxyz/50 : 0 ;
   if( vstep ) fprintf(stderr,"++ voxel loop: ") ;
#endif

   /*** the real work now begins ***/

 AFNI_OMP_START ;
#pragma omp parallel if( nmask > 666 )
 { int kk , xx,yy,zz , vv,ii , mm ;
   float *zar=NULL , *nbar ; int *ivar ;
   float *tsar , *uvec , *vvec=NULL , *ws=NULL ;
   unsigned short xran[3] ; int ithr=0 ; float sval,tval=0.0 ;

#ifdef USE_OMP
  ithr = omp_get_thread_num() ;
  if( ithr == 0 )
    INFO_message("Starting voxel loop: OpenMP threads=%d",omp_get_num_threads());
#endif

#pragma omp critical (MALLOC)
   { uvec = (float *)malloc(sizeof(float)*nt) ;
     ws   = pv_get_workspace(nt,nbhd->num_pt) ;
     nbar = (float *)malloc(sizeof(float)*nt*nbhd->num_pt) ;
     ivar = (int *)malloc(sizeof(int)*nbhd->num_pt) ;
     if( do_vproj == 2 )
       vvec = (float *)malloc(sizeof(float)*nt) ;
     if( do_vproj )
       zar = (float *)malloc(sizeof(float)*nt) ;
   }

   xran[2] = ( gseed        & 0xffff) + (unsigned short)ithr ;
   xran[1] = ((gseed >> 16) & 0xffff) - (unsigned short)ithr ;
   xran[0] = 0x330e                   + (unsigned short)ithr ;

#pragma omp for
   for( kk=0 ; kk < nxyz ; kk++ ){
#ifndef USE_OMP
     if( vstep && kk%vstep==vstep-1 ) vstep_print() ;
#endif
     if( !INMASK(kk) ) continue ;
     IJK_TO_THREE( kk , xx,yy,zz , nx,nxy ) ;

     mm = myTHD_extract_nbhd( inset , mask , xx,yy,zz , nbhd , ivar,nbar ) ;
     if( mm <= 0 ) continue ;  /* no data? */
     if( do_vproj ){
       for( ii=0 ; ii < nt ; ii++ ) zar[ii] = nbar[ii] ;
     }
     if( do_vnorm ){
       register float sum ;
       for( vv=0 ; vv < mm ; vv++ ){
         tsar = nbar + nt*vv ;
         for( sum=0.0f,ii=0 ; ii < nt ; ii++ ) sum += tsar[ii]*tsar[ii] ;
         if( sum > 0.0f ){
           for( sum=1.0f/sqrtf(sum),ii=0 ; ii < nt ; ii++ ) tsar[ii] *= sum ;
         }
       }
     }
     tsar = nbar ;
     if( do_vproj <= 1 ){
       sval = principal_vector( nt, mm, 0,nbar, uvec, (do_vproj) ? NULL : tsar, ws,xran ) ;
     } else {
       float_pair spair ;
       spair = principal_vector_pair( nt,mm, 0,nbar, uvec,vvec, NULL, ws,xran ) ;
       sval = spair.a ; tval = spair.b ;
     }
     switch( do_vproj ){
       case 1:{
         register float sum ;
         for( sum=0.0f,ii=0 ; ii < nt ; ii++ ) sum += zar[ii]*uvec[ii] ;
         for( ii=0 ; ii < nt ; ii++ ) uvec[ii] *= sum ;
       }
       break ;

       case 2:{
         register float sum ;
         for( sum=0.0f,ii=0 ; ii < nt ; ii++ ) sum += zar[ii]*uvec[ii] ;
         for( ii=0 ; ii < nt ; ii++ ) uvec[ii] *= sum ;
         for( sum=0.0f,ii=0 ; ii < nt ; ii++ ) sum += zar[ii]*vvec[ii] ;
         for( ii=0 ; ii < nt ; ii++ ) uvec[ii] += sum*vvec[ii] ;
       }
     }
     THD_insert_series( kk, outset, nt, MRI_float, uvec, 0 ) ;
     if( evar != NULL ) evar[kk] = sval ;
     if( fvar != NULL ) fvar[kk] = tval ;
   }

#pragma omp critical (MALLOC)
   { free(uvec) ; free(nbar) ; free(ws) ; free(ivar) ;
     if( vvec != NULL ) free(vvec) ;
     if( zar  != NULL ) free(zar) ; }
 } /* end OpenMP */
 AFNI_OMP_END ;

#ifndef USE_OMP
   if( vstep ) fprintf(stderr,"\n") ;
#else
   ININFO_message("Voxel loop finished!") ;
#endif

   /*** cleanup and exit ***/

   DSET_delete(inset) ;
   DSET_write(outset) ; WROTE_DSET(outset) ; DSET_delete(outset) ;

   if( evset != NULL ){
     DSET_write(evset) ; WROTE_DSET(evset) ; DSET_delete(evset) ;
   }

   exit(0) ;
}

/*------------------------------------------------------------------------*/

#undef  TS
#define TS(i,j) tsar[(i)+(j)*nv]

static int myTHD_extract_nbhd( THD_3dim_dataset *dset, byte *mask,
                               int xx, int yy, int zz, MCW_cluster *nbhd,
                               int *ivar , float *tsar )
{
   int nv , ival , kk ;
   int nvox , nx,ny,nz,nxy,nxyz , npt , aa,bb,cc , ii,vv ;

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ; nxy  = nx*ny  ;
   nz = DSET_NZ(dset) ; nxyz = nxy*nz ; npt = nbhd->num_pt ;
   nv = dset->dblk->nvals ;

   for( nvox=ii=0 ; ii < npt ; ii++ ){
     aa = xx + nbhd->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
     bb = yy + nbhd->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
     cc = zz + nbhd->k[ii] ; if( cc < 0 || cc >= nz ) continue ;
     vv = aa + bb*nx + cc*nxy ;
     if( mask == NULL || mask[vv] ) ivar[nvox++] = vv ;
   }
   if( nvox == 0 ) return 0 ;  /* nothing to extract */

   /* fill the output */

   switch( DSET_BRICK_TYPE(dset,0) ){

      default:             /* don't know what to do --> return nada */
        return 0 ;

      case MRI_byte:{
        byte *bar ;
        for( ival=0 ; ival < nv ; ival++ ){
          bar = (byte *)DSET_ARRAY(dset,ival) ;
          for( kk=0 ; kk < nvox ; kk++ ) TS(ival,kk) = (float)bar[ivar[kk]] ;
        }
      }
      break ;

      case MRI_short:{
        short *bar ;
        for( ival=0 ; ival < nv ; ival++ ){
          bar = (short *)DSET_ARRAY(dset,ival) ;
          for( kk=0 ; kk < nvox ; kk++ ) TS(ival,kk) = (float)bar[ivar[kk]] ;
        }
      }
      break ;

      case MRI_float:{
         float *bar ;
         for( ival=0 ; ival < nv ; ival++ ){
            bar = (float *) DSET_ARRAY(dset,ival) ;
            for( kk=0 ; kk < nvox ; kk++ ) TS(ival,kk) = bar[ivar[kk]] ;
         }
      }
      break ;
   }

   /* scale outputs, if needed */

   if( THD_need_brick_factor(dset) ){
      float fac ;
      for( ival=0 ; ival < nv ; ival++ ){
        fac = DSET_BRICK_FACTOR(dset,ival) ;
        for( kk=0 ; kk < nvox ; kk++ ) TS(ival,kk) *= fac ;
      }
   }

   return nvox ;
}
