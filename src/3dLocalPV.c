#include "mrilib.h"

#ifdef USE_OMP    /* don't use this yet */
#include <omp.h>
#endif

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

static int myTHD_extract_nbhd( THD_3dim_dataset *dset, byte *mask,
                               int xx, int yy, int zz, MCW_cluster *nbhd,
                               int *ivar , float *tsar ) ;

#ifdef USE_OMP
#include "cs_pv.c"  /* to ensure it's OpenMP-ized as well */
#endif

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
/* Adapted from 3dLocalSVD, to be more efficient when #evals=1 */

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL , *outset=NULL , *evset=NULL ;
   MCW_cluster *nbhd=NULL ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 ;
   char *prefix="./LocalPV" ;
   char *evprefix=NULL ; int nev ;
   int iarg=1 , verb=1 , ntype=0 , kk,nx,ny,nz,nxy,nxyz,nt , vstep=0 ;
   float na,nb,nc , dx,dy,dz ;
   int do_vnorm=0 , do_vproj=0 , polort=-1 ;
   int nmask=0 , domean=0 , use_nonmask=0 ;
   float *evar=NULL,*fvar=NULL ;
   unsigned int gseed ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3dLocalPV [options] inputdataset\n"
       "* You may want to use 3dDetrend before running this program,\n"
       "   or at least use the '-polort' option.\n"
       "* This program is highly experimental.  And slowish.\n"
       "* Computes the SVD of the time series from a neighborhood of each\n"
       "   voxel.  An inricate way of 'smoothing' 3D+time datasets, sort of.\n"
       "* This is like 3dLocalSVD, except that the '-vproj' option doesn't\n"
       "   allow anything but 1 and 2 dimensional projection.  This is because\n"
       "   3dLocalPV uses a special method to compute JUST the first 1 or 2\n"
       "   principal vectors -- faster than 3dLocalSVD, but less general.\n"
       "\n"
       "Options:\n"
       " -mask mset          = restrict operations to this mask\n"
       " -automask           = create a mask from time series dataset\n"
       " -prefix ppp         = save SVD vector result into this new dataset\n"
       "                        [default = 'LocalPV']\n"
       " -evprefix ppp       = save singular value at each voxel into this dataset\n"
       "                        [default = don't save]\n"
       " -input inputdataset = input time series dataset\n"
       " -nbhd nnn           = e.g., 'SPHERE(5)' 'TOHD(7)' etc.\n"
       " -polort p           = detrending\n"
       " -vnorm              = normalize data vectors [strongly recommended]\n"
       " -vproj [2]          = project central data time series onto local SVD vector;\n"
       "                        if followed by '2', then the central data time series\n"
       "                        will be projected on the the 2-dimensional subspace\n"
       "                        spanned by the first 2 principal SVD vectors.\n"
       "                        [default: just output principal singular vector]\n"
       "                        [for 'smoothing' purposes, '-vnorm -vproj' is an idea]\n"
#if 0
       "\n"
       " -use_nonmask         = Allow the computation of the local SVD time series\n"
       "                         even from voxels that are NOT in the mask, provided\n"
       "                         that there are voxels IN the mask inside the local\n"
       "                         neighborhood.\n"
       "                        * You could use '-use_nonmask' to compute the principal\n"
       "                           SVD vector of local white matter time series, for\n"
       "                           example, even at non-WM voxels.\n"
       "                        * '-vproj' is not allowed with '-use_nonmask'!\n"
#endif
       "\n"
       "Notes:\n"
       "* On my Mac Pro, about 30%% faster than 3dLocalSVD computing the same thing.\n"
       "* If you're curious, the 'special method' used for the eigensolution is\n"
       "  a variant of matrix power iteration, called 'simultaneous iteration'.\n"
       "* By contrast, 3dLocalSVD uses EISPACK functions for eigensolution-izing.\n"
     ) ;
     PRINT_AFNI_OMP_USAGE("3dLocalPV",NULL) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dLocalPV"); mainENTRY("3dLocalPV main"); machdep();
   AFNI_logger("3dLocalPV",argc,argv); AUTHOR("Emperor Zhark the Iterator");

   gseed = ((unsigned int)time(NULL)) + 17*(unsigned int)getpid() ;

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-use_nonmask") == 0 ){  /* 17 Jul 2009 */
       use_nonmask = 1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-vnorm") == 0 ){
       do_vnorm = 1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-vproj") == 0 ){
       if( iarg+1 < argc && isdigit(argv[iarg+1][0]) )
         do_vproj = (int)strtod(argv[++iarg],NULL) ;
       if( do_vproj < 1 ){
         do_vproj = 1 ;
       } else if( do_vproj > 2 ){
         do_vproj = 2 ; WARNING_message("-vproj set to 2") ;
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-polort") == 0 ){
       char *cpt ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-polort'") ;
       polort = (int)strtod(argv[iarg],&cpt) ;
       iarg++ ; continue ;
     }

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

     if( strcmp(argv[iarg],"-evprefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-evprefix'") ;
       evprefix = strdup(argv[iarg]) ;
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
       if( nmask < 2 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-nbhd") == 0 ){
       char *cpt ;
       if( ntype  >  0    ) ERROR_exit("Can't have 2 '-nbhd' options") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-nbhd'") ;

       cpt = argv[iarg] ;
       if( strncasecmp(cpt,"SPHERE",6) == 0 ){
         sscanf( cpt+7 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a SPHERE of radius 0") ;
         ntype = NTYPE_SPHERE ;
       } else if( strncasecmp(cpt,"RECT",4) == 0 ){
         sscanf( cpt+5 , "%f,%f,%f" , &na,&nb,&nc ) ;
         if( na == 0.0f && nb == 0.0f && nc == 0.0f )
           ERROR_exit("'RECT(0,0,0)' is not a legal neighborhood") ;
         ntype = NTYPE_RECT ;
       } else if( strncasecmp(cpt,"RHDD",4) == 0 ){
         sscanf( cpt+5 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a RHDD of radius 0") ;
         ntype = NTYPE_RHDD ;
       } else if( strncasecmp(cpt,"TOHD",4) == 0 ){
         sscanf( cpt+5 , "%f" , &na ) ;
         if( na == 0.0f ) ERROR_exit("Can't have a TOHD of radius 0") ;
         ntype = NTYPE_TOHD ;
       } else {
          ERROR_exit("Unknown -nbhd shape: '%s'",cpt) ;
       }
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   if( use_nonmask ){
     if( do_vproj ){
       do_vproj = 0 ; WARNING_message("-use_nonmask disables -vproj") ;
     }
   }

   /*---- deal with input dataset ----*/

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   }
   if( !IS_REAL_TYPE(DSET_BRICK_TYPE(inset,0)) )
     ERROR_exit("Can only process real-valued datasets in this program!") ;
   nt = DSET_NVALS(inset) ;
   if( nt < 9 )
     ERROR_exit("Must have at least 9 values per voxel in time series dataset '%s'",
                DSET_BRIKNAME(inset) ) ;
   if( polort+1 >= nt )
     ERROR_exit("'-polort %d' too big for time series length = %d",polort,nt) ;

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

   /*-- deal with mask --*/

   nx = DSET_NX(inset) ;
   ny = DSET_NY(inset) ; nxy  = nx*ny  ;
   nz = DSET_NZ(inset) ; nxyz = nxy*nz ;

   if( mask != NULL ){
     if( mask_nx != DSET_NX(inset) ||
         mask_ny != DSET_NY(inset) ||
         mask_nz != DSET_NZ(inset)   )
       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( automask ){
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     nmask = THD_countmask( DSET_NVOX(inset) , mask ) ;
     INFO_message("Number of voxels in automask = %d",nmask) ;
     if( nmask < 2 ) ERROR_exit("Automask is too small to process") ;

   } else {
     nmask = nxyz ;  /* all voxels */
   }

   /*---- create neighborhood (as a cluster) -----*/

   if( ntype <= 0 ){         /* default neighborhood */
     ntype = NTYPE_SPHERE ; na = -1.01f ;
     INFO_message("Using default neighborhood = self + 6 neighbors") ;
   }

   switch( ntype ){
     default:
       ERROR_exit("WTF?  ntype=%d",ntype) ;

     case NTYPE_SPHERE:{
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(inset)) ;
                        dy = fabsf(DSET_DY(inset)) ;
                        dz = fabsf(DSET_DZ(inset)) ; }
       nbhd = MCW_spheremask( dx,dy,dz , na ) ;
     }
     break ;

     case NTYPE_RECT:{
       if( na < 0.0f ){ dx = 1.0f; na = -na; } else dx = fabsf(DSET_DX(inset));
       if( nb < 0.0f ){ dy = 1.0f; nb = -nb; } else dy = fabsf(DSET_DY(inset));
       if( nc < 0.0f ){ dz = 1.0f; nc = -nc; } else dz = fabsf(DSET_DZ(inset));
       nbhd = MCW_rectmask( dx,dy,dz , na,nb,nc ) ;
     }
     break ;

     case NTYPE_RHDD:{
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(inset)) ;
                        dy = fabsf(DSET_DY(inset)) ;
                        dz = fabsf(DSET_DZ(inset)) ; }
       nbhd = MCW_rhddmask( dx,dy,dz , na ) ;
     }
     break ;

     case NTYPE_TOHD:{
       if( na < 0.0f ){ dx = dy = dz = 1.0f ; na = -na ; }
       else           { dx = fabsf(DSET_DX(inset)) ;
                        dy = fabsf(DSET_DY(inset)) ;
                        dz = fabsf(DSET_DZ(inset)) ; }
       nbhd = MCW_tohdmask( dx,dy,dz , na ) ;
     }
     break ;
   }
   MCW_radsort_cluster( nbhd , dx,dy,dz ) ; /* 26 Feb 2008 */
                                           /* ensures first value is centroid */

   INFO_message("Neighborhood comprises %d voxels",nbhd->num_pt) ;
   INFO_message("Each time series has %d points",nt) ;
#if 0
   printf("Neighborhood offsets:\n") ;
   for( kk=0 ; kk < nbhd->num_pt ; kk++ )
     printf(" [%2d] = %2d %2d %2d\n",kk,nbhd->i[kk],nbhd->j[kk],nbhd->k[kk]) ;
#endif

   /** create output dataset **/

   outset = EDIT_empty_copy(inset) ;
   EDIT_dset_items( outset, ADN_prefix,prefix, ADN_brick_fac,NULL, ADN_none );
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dLocalPV" , argc,argv , outset ) ;
   for( kk=0 ; kk < nt ; kk++ )                         /* create bricks */
     EDIT_substitute_brick( outset , kk , MRI_float , NULL ) ;

   if( evprefix != NULL ){
     evset = EDIT_empty_copy(inset) ;
     kk = (do_vproj <= 1) ? 1 : 2 ;
     EDIT_dset_items( evset , ADN_prefix,evprefix, ADN_ntt,0 ,
                              ADN_brick_fac,NULL, ADN_nvals,kk, ADN_none ) ;
     tross_Copy_History( inset , evset ) ;
     tross_Make_History( "3dLocalPV" , argc,argv , evset ) ;
     EDIT_substitute_brick( evset , 0 , MRI_float , NULL ) ;
     evar = DSET_ARRAY(evset,0) ;
     if( kk == 2 ){
       EDIT_substitute_brick( evset , 1 , MRI_float , NULL ) ;
       fvar = DSET_ARRAY(evset,1) ;
     }
   }

   if( polort >= 0 ){
     float **polyref ; THD_3dim_dataset *qset ;
     INFO_message("Detrending input dataset with %d basis functions",polort+1) ;
     polyref = THD_build_polyref( polort+1 , nt ) ;
     qset = THD_detrend_dataset( inset , polort+1,polyref , 2,0,mask,NULL ) ;
     if( qset == NULL ) ERROR_exit("Detrending fails!?") ;
     free(polyref) ; DSET_delete(inset) ; inset = qset ;
   }

#ifndef USE_OMP
   vstep = (verb && nxyz > 999) ? nxyz/50 : 0 ;
   if( vstep ) fprintf(stderr,"++ voxel loop: ") ;
#endif

   /*** the real work now begins ***/

#pragma omp parallel if( nmask > 666 )
 { int kk , xx,yy,zz , vv,ii , mm ;
   float *zar=NULL , *nbar ; int *ivar ;
   float *tsar , *uvec , *vvec=NULL , *ws=NULL ;
   unsigned short xran[3] ; int ithr=0 ; float sval,tval ;

 AFNI_OMP_START ;

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
 AFNI_OMP_END ;
 } /* end OpenMP */

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
