#include "mrilib.h"

#ifdef USE_OMP    /* don't use this yet */
#include "omp.h"
#endif

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

MRI_IMARR * myTHD_get_dset_nbhd_array( THD_3dim_dataset *dset, byte *mask,
                                       int xx, int yy, int zz, MCW_cluster *nbhd ) ;

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
   float **polyref ; int rebase=0 , nmask=0 , domean=0 , use_nonmask=0 ;
   float *evar=NULL , sval ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3dLocalPV [options] inputdataset\n"
       "* You may want to use 3dDetrend before running this program,\n"
       "   or at least use the '-polort' option.\n"
       "* This program is highly experimental.  And slowish.\n"
       "* Computes the SVD of the time series from a neighborhood of each\n"
       "   voxel.  An inricate way of 'smoothing' 3D+time datasets, sort of.\n"
       "* This is like 3dLocalSVD, except that the '-vproj' option doesn't\n"
       "   allow anything but 1 dimensional projection.  This is because\n"
       "   3dLocalPV uses a special method to compute JUST the first\n"
       "   principal vector -- faster than 3dLocalSVD, but less general.\n"
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
       " -polort p [+]       = detrending ['+' means to add trend back]\n"
       " -vnorm              = normalize data vectors [strongly recommended]\n"
       " -vproj              = project central data time series onto local SVD vector\n"
       "                        [default: just output principal singular vector]\n"
       "                        [for 'smoothing' purposes, '-vnorm -vproj' is good]\n"
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
       "\n"
#endif
     ) ;
     /** PRINT_AFNI_OMP_USAGE("3dLocalPV",NULL) ; **/
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dLocalPV"); mainENTRY("3dLocalPV main"); machdep();
   AFNI_logger("3dLocalPV",argc,argv); AUTHOR("Emperor Zhark the Iterator");

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-use_nonmask") == 0 ){  /* 17 Jul 2009 */
       use_nonmask = 1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-vnorm") == 0 ){
       do_vnorm = 1 ; iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-vproj") == 0 ){
       do_vproj = 1 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-polort") == 0 ){
       char *cpt ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-polort'") ;
       polort = (int)strtod(argv[iarg],&cpt) ;
       if( *cpt == '+' ){
         rebase = 1 ;
       } else if( iarg+1 < argc && strcmp(argv[iarg+1],"+") == 0 ){
         rebase = 1; iarg++;
       }
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
   nt = DSET_NVALS(inset) ;
   if( nt < 9 )
     ERROR_exit("Must have at least 9 values per voxel in time series dataset '%s'",
                DSET_BRIKNAME(inset) ) ;
   if( polort+1 >= nt )
     ERROR_exit("'-polort %d' too big for time series length = %d",polort,nt) ;

   if( polort >= 0 ){
     polyref = THD_build_polyref( polort+1 , nt ) ;
   }

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
     EDIT_dset_items( evset , ADN_prefix,evprefix,
                              ADN_brick_fac,NULL, ADN_nvals,1, ADN_none ) ;
     tross_Copy_History( inset , evset ) ;
     tross_Make_History( "3dLocalPV" , argc,argv , evset ) ;
     EDIT_substitute_brick( evset , 0 , MRI_float , NULL ) ;
     evar = DSET_ARRAY(evset,0) ;
   }

#ifndef USE_OMP
   vstep = (verb && nxyz > 999) ? nxyz/50 : 0 ;
   if( vstep ) fprintf(stderr,"++ voxel loop: ") ;
#endif

   /*** the real work now begins ***/

#pragma omp parallel if( nmask > 666 )  /* parallelization 16 Jul 2009 [disabled] */
 { int kk , xx,yy,zz , vv,ii , mm ;
   MRI_IMARR *imar ; MRI_IMAGE *zim ;
   float *tsar , *coef , **xar , *uvec ;

#pragma omp critical (MALLOC)
   { if( rebase ) coef = (float *)malloc(sizeof(float)*(polort+1)) ;
     else         coef = NULL ;
     xar = (float **)malloc(sizeof(float *)*nbhd->num_pt) ;
     uvec = (float *)malloc(sizeof(float)*nt) ;
   }

 AFNI_OMP_START ;
   for( kk=0 ; kk < nxyz ; kk++ ){
#ifndef USE_OMP
     if( vstep && kk%vstep==vstep-1 ) vstep_print() ;
#endif
     if( !use_nonmask && !INMASK(kk) ) continue ;
     IJK_TO_THREE( kk , xx,yy,zz , nx,nxy ) ;
     imar = myTHD_get_dset_nbhd_array( inset , mask , xx,yy,zz , nbhd ) ;
     if( imar == NULL ){
       if( !use_nonmask ) ERROR_message("get failure #%d",kk);
       continue;
     }
     mm = IMARR_COUNT(imar) ;
     for( vv=0 ; vv < mm ; vv++ ){
       xar[vv] = tsar = MRI_FLOAT_PTR(IMARR_SUBIM(imar,vv)) ;
#pragma omp critical (MALLOC)
       { if( polort >= 0 ) /* detrend extracted data in place */
           THD_generic_detrend_LSQ( nt , tsar , -1 , polort+1 , polyref ,
                                    (vv==0) ? coef : NULL ) ;
         if( do_vproj && vv == 0 )
           zim = mri_copy(IMARR_SUBIM(imar,0)) ;  /* before vnorm */
       }
       if( do_vnorm ){
         register float sum ;
         for( sum=0.0f,ii=0 ; ii < nt ; ii++ ) sum += tsar[ii]*tsar[ii] ;
         if( sum > 0.0f ){
           sum = 1.0f / sqrtf(sum) ;
           for( ii=0 ; ii < nt ; ii++ ) tsar[ii] *= sum ;
         }
       }
     }
     tsar = MRI_FLOAT_PTR(IMARR_SUBIM(imar,0)) ;
     sval = principal_vector( nt, mm, 1, xar, uvec, (do_vproj) ? NULL : tsar ) ;
#pragma omp critical (MALLOC)
     { DESTROY_IMARR(imar) ; }
     if( do_vproj ){
       register float sum ; float *zar = MRI_FLOAT_PTR(zim) ;
       for( sum=0.0f,ii=0 ; ii < nt ; ii++ ) sum += zar[ii]*uvec[ii] ;
       for( ii=0 ; ii < nt ; ii++ ) uvec[ii] *= sum ;
     }
     if( polort >= 0 && rebase && coef != NULL ){
       for( vv=0 ; vv <= polort ; vv++ ){
         for( ii=0 ; ii < nt ; ii++ ) uvec[ii] += coef[vv]*polyref[vv][ii] ;
       }
     }
     THD_insert_series( kk, outset, nt, MRI_float, uvec, 0 ) ;
     if( evar != NULL ) evar[kk] = sval ;
   }

#pragma omp critical (MALLOC)
   { free(uvec) ; free(xar) ; if( coef != NULL ) free(coef) ; }
 AFNI_OMP_END ;
 } /* end OpenMP */

#ifndef USE_OMP
   if( vstep ) fprintf(stderr,"\n") ;
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

MRI_IMARR * myTHD_get_dset_nbhd_array( THD_3dim_dataset *dset, byte *mask,
                                       int xx, int yy, int zz, MCW_cluster *nbhd )
{
   MRI_IMARR *imar ;
   int nvox, *ivox , nx,ny,nz , nxy,nxyz , npt, aa,bb,cc,kk,ii ;

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ; nxy  = nx*ny  ;
   nz = DSET_NZ(dset) ; nxyz = nxy*nz ; npt = nbhd->num_pt ;

   kk = xx + yy*nx + zz*nxy ; if( kk < 0 || kk >= nxyz ) return NULL ;

#pragma omp critical (MALLOC)
   ivox = (int *)malloc(sizeof(int)*npt) ;
   for( nvox=ii=0 ; ii < npt ; ii++ ){
     aa = xx + nbhd->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
     bb = yy + nbhd->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
     cc = zz + nbhd->k[ii] ; if( cc < 0 || cc >= nz ) continue ;
     kk = aa + bb*nx + cc*nxy ;
     if( INMASK(kk) ) ivox[nvox++] = kk ;
   }
   if( nvox == 0 ){ free(ivox) ; return NULL ; }  /* no voxels to extract */

#pragma omp critical (MALLOC)
   { imar = THD_extract_many_series( nvox, ivox, dset ) ;
     free(ivox) ; }

   return imar ;
}
