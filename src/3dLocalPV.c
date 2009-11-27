#include "mrilib.h"

#ifdef USE_OMP
#include "omp.h"
#endif

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

static MRI_IMARR * myTHD_get_dset_nbhd_array( THD_3dim_dataset *dset, byte *mask,
                                            int xx, int yy, int zz, MCW_cluster *nbhd ) ;
static float principal_vector( int n , int m , int xtyp , void *xp ,
                               float *uvec , float *tvec            ) ;

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
/* Adapted from 3dLocalSVD */

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

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3dLocalPV [options] inputdataset\n"
       "* You may want to use 3dDetrend before running this program,\n"
       "   or at least use the '-polort' option.\n"
       "* This program is highly experimental.  And slowish.\n"
       "* Computes the SVD of the time series from a neighborhood of each\n"
       "   voxel.  An inricate way of 'smoothing' 3D+time datasets,\n"
       "   in some sense, maybe.\n"
       "\n"
       "Options:\n"
       " -mask mset           = restrict operations to this mask\n"
       " -automask            = create a mask from time series dataset\n"
       " -prefix ppp          = save SVD vector result into this new dataset\n"
       " -input inputdataset  = input time series dataset\n"
       " -nbhd nnn            = e.g., 'SPHERE(5)' 'TOHD(7)' etc.\n"
       " -polort p [+]        = detrending ['+' means to add trend back]\n"
       " -vnorm               = normalize data vectors [strongly recommended]\n"
       " -vproj               = project central data time series onto local SVD vector\n"
       "                         [default: just output principal singular vector]\n"
       "                         [for 'smoothing' purposes, '-vnorm -vproj' might be fun]\n"
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
   AFNI_logger("3dLocalPV",argc,argv); AUTHOR("Emperor Zhark the Singular");

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

#ifndef USE_OMP
   vstep = (verb && nxyz > 999) ? nxyz/50 : 0 ;
   if( vstep ) fprintf(stderr,"++ voxel loop: ") ;
#endif

   /*** the real work now begins ***/

#pragma omp parallel if( nmask > 666 )  /* parallelization 16 Jul 2009 [disabled] */
 { int kk , xx,yy,zz , vv,ii , mm ;
   MRI_IMARR *imar ; MRI_IMAGE *pim ;
   float *tsar , *coef , **xar , *uvec ;

   if( rebase ) coef = (float *)malloc(sizeof(float)*(polort+1)) ;
   else         coef = NULL ;

   xar = (float **)malloc(sizeof(float *)*nbhd->num_pt) ;
   uvec = (float *)malloc(sizeof(float)*nt) ;

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
       if( polort >= 0 ){
         THD_generic_detrend_LSQ( nt , tsar , -1 , polort+1 , polyref ,
                                  (vv==0) ? coef : NULL ) ;
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
     (void)principal_vector( nt, mm, 1, xar, uvec, (do_vproj) ? NULL : tsar ) ;
     DESTROY_IMARR(imar) ;
     if( do_vproj ){
       register float sum ;
       for( sum=0.0f,ii=0 ; ii < nt ; ii++ ) sum += tsar[ii]*uvec[ii] ;
       for( ii=0 ; ii < nt ; ii++ ) uvec[ii] *= sum ;
     }
     if( polort >= 0 && rebase && coef != NULL ){
       for( vv=0 ; vv <= polort ; vv++ ){
         for( ii=0 ; ii < nt ; ii++ ) uvec[ii] += coef[vv]*polyref[vv][ii] ;
       }
     }
     THD_insert_series( kk, outset, nt, MRI_float, uvec, 0 ) ;
   }

   free(uvec) ; free(xar) ; if( coef != NULL ) free(coef) ;
 AFNI_OMP_END ;
 } /* end OpenMP */

#ifndef USE_OMP
   if( vstep ) fprintf(stderr,"\n") ;
#endif

   /*** cleanup and exit ***/

   DSET_delete(inset) ;
   DSET_write(outset) ; WROTE_DSET(outset) ; DSET_delete(outset) ;

   exit(0) ;
}

/*------------------------------------------------------------------------*/

static MRI_IMARR * myTHD_get_dset_nbhd_array( THD_3dim_dataset *dset, byte *mask,
                                            int xx, int yy, int zz, MCW_cluster *nbhd )
{
   MRI_IMARR *imar ;
   int nvox, *ivox , nx,ny,nz , nxy,nxyz , npt, aa,bb,cc,kk,ii ;

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ; nxy  = nx*ny  ;
   nz = DSET_NZ(dset) ; nxyz = nxy*nz ; npt = nbhd->num_pt ;

   kk = xx + yy*nx + zz*nxy ; if( kk < 0 || kk >= nxyz ) return NULL ;

   ivox = (int *)malloc(sizeof(int)*npt) ; nvox = 0 ;
   for( ii=0 ; ii < npt ; ii++ ){
     aa = xx + nbhd->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
     bb = yy + nbhd->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
     cc = zz + nbhd->k[ii] ; if( cc < 0 || cc >= nz ) continue ;
     kk = aa + bb*nx + cc*nxy ;
     if( INMASK(kk) ) ivox[nvox++] = kk ;
   }
   if( nvox == 0 ){ free(ivox) ; return NULL ; }  /* no voxels to extract */

   imar = THD_extract_many_series( nvox, ivox, dset ) ;
   free(ivox) ; return imar ;
}

/*----------------------------------------------------------------------------*/

static float symeig_sim1( int n     , float *asym , float *vec ) ;
static void  symeig_2D  ( double *a , double *e   , int dovec  ) ;
static void  symeig_3D  ( double *a , double *e   , int dovec  ) ;

#undef  A
#define A(i,j) asym[(i)+(j)*nsym]
#undef  XPT
#define XPT(q) ( (xtyp<=0) ? xx+(q)*nn : xar[q] )

/*----------------------------------------------------------------------------*/
/*! Compute the mean vector of a set of m columns, each of length n.
   * If xtyp <=0, the columns are stored in one big array:
      ((float *)xp)[i+j*n] for i=0..n-1, j=0..m-1.
   * If xtyp > 0, the columns are stored in an array of arrays:
      ((float **)xp)[j][i]
   * The return value is the L2-norm of the vector, and the vector is
     stored into uvec.
*//*--------------------------------------------------------------------------*/

static float mean_vector( int n , int m , int xtyp , void *xp , float *uvec )
{
   int nn=n , mm=m , jj ; register int ii ;
   register float *xj , fac,sum ; float *xx=NULL , **xar=NULL ;

   if( nn < 1 || mm < 1 || xp == NULL || uvec == NULL ) return -1.0f ;

   if( xtyp <= 0 ) xx  = (float * )xp ;
   else            xar = (float **)xp ;

   for( ii=0 ; ii < nn ; ii++ ) uvec[ii] = 0.0f ;

   for( jj=0 ; jj < mm ; jj++ ){
     xj = XPT(jj) ;
     for( ii=0 ; ii < nn ; ii++ ) uvec[ii] += xj[ii] ;
   }

   fac = 1.0f / nn ; sum = 0.0f ;
   for( ii=0 ; ii < nn ; ii++ ){ uvec[ii] *= fac; sum += uvec[ii]*uvec[ii]; }
   return sqrtf(sum) ;
}

/*----------------------------------------------------------------------------*/
/*! Compute the principal singular vector of a set of m columns, each
    of length n.
   * If xtyp <=0, the columns are stored in one big array:
      ((float *)xp)[i+j*n] for i=0..n-1, j=0..m-1.
   * If xtyp > 0, the columns are stored in an array of arrays:
      ((float **)xp)[j][i]
   * The singular value is returned, and the vector is stored into uvec[].
   * tvec is a vector so that the sign of uvec dot tvec will be non-negative.
   * If the return value is not positive, something ugly happened.
*//*--------------------------------------------------------------------------*/

static float principal_vector( int n , int m , int xtyp , void *xp ,
                               float *uvec , float *tvec            )
{
   int nn=n , mm=m , nsym , ii,jj,kk,qq ;
   float *asym ;
   register float sum,qsum ; register float *xj , *xk ;
   float sval , *xx=NULL , **xar=NULL ;

   nsym = MIN(nn,mm) ;  /* size of the symmetric matrix to create */

   if( nsym < 1 || xp == NULL || uvec == NULL ) return -666.0f ;

   if( xtyp <= 0 ) xx  = (float * )xp ;
   else            xar = (float **)xp ;

   if( nsym == 1 ){  /*----- trivial case -----*/

     if( mm == 1 ){
       xj = XPT(0) ; qsum = sum = 0.0f ;
       for( ii=0; ii < nn; ii++ ){
         uvec[ii] = xj[ii] ; qsum += uvec[ii]*uvec[ii] ;
         if( tvec != NULL ) sum += tvec[ii]*uvec[ii] ;
       }
       sval = sqrtf(qsum) ;
       if( sval > 0.0f ){
         qsum = 1.0f / sval ;
         if( sum < 0.0f ) qsum = -qsum ;
         for( ii=0 ; ii < nn ; ii++ ) uvec[ii] *= qsum ;
       } else {
         qsum = sqrtf(1.0f/nn) ;
         for( ii=0 ; ii < nn ; ii++ ) uvec[ii] = qsum ;
       }
     } else {  /* nn==1 and have mm > 1 such 'vectors' */
       uvec[0] = (tvec != NULL && tvec[0] < 0.0f) ? -1.0f : 1.0f ;
       sval = 0.0f ;
       for( ii=0 ; ii < mm ; ii++ ){
         xj = XPT(jj) ; sval += xj[0]*xj[0] ;
       }
       sval = sqrtf(sval) ;
     }
     return sval ;

   } /*----- end of trivial case -----*/

#pragma omp critical (MALLOC)
   asym = (float *)malloc(sizeof(float)*nsym*nsym) ;  /* symmetric matrix */

   /** setup matrix to eigensolve: choose smaller of [X]'[X] and [X][X]' **/
   /**     since [X] is n x m, [X]'[X] is m x m and [X][X]' is n x n     **/

   if( nn > mm ){                       /* more rows than columns:  */
                                        /* so [A] = [X]'[X] = m x m */
     int n1 = nn-1 ;
     for( jj=0 ; jj < mm ; jj++ ){
       xj = XPT(jj) ;
       for( kk=0 ; kk <= jj ; kk++ ){
         sum = 0.0 ; xk = XPT(kk) ;
         for( ii=0 ; ii < n1 ; ii+=2 ) sum += xj[ii]*xk[ii] + xj[ii+1]*xk[ii+1];
         if( ii == n1 ) sum += xj[ii]*xk[ii] ;
         A(jj,kk) = sum ; if( kk < jj ) A(kk,jj) = sum ;
       }
     }

   } else {                             /* more columns than rows:  */
                                        /* so [A] = [X][X]' = n x n */
     float *xt ; int m1=mm-1 ;
#pragma omp critical (MALLOC)
     xt = (float *)malloc(sizeof(float)*nn*mm) ;
     for( jj=0 ; jj < mm ; jj++ ){      /* form [X]' into array xt */
       if( xtyp <= 0 )
         for( ii=0 ; ii < nn ; ii++ ) xt[jj+ii*mm] = xx[ii+jj*nn] ;
       else
         for( ii=0 ; ii < nn ; ii++ ) xt[jj+ii*mm] = xar[jj][ii] ;
     }

     for( jj=0 ; jj < nn ; jj++ ){
       xj = xt + jj*mm ;
       for( kk=0 ; kk <= jj ; kk++ ){
         sum = 0.0 ; xk = xt + kk*mm ;
         for( ii=0 ; ii < m1 ; ii+=2 ) sum += xj[ii]*xk[ii] + xj[ii+1]*xk[ii+1];
         if( ii == m1 ) sum += xj[ii]*xk[ii] ;
         A(jj,kk) = sum ; if( kk < jj ) A(kk,jj) = sum ;
       }
     }

#pragma omp critical (MALLOC)
     free(xt) ;  /* don't need this no more */
   }

   /** SVD is [X] = [U] [S] [V]', where [U] = desired output vectors

       case n <= m: [A] = [X][X]' = [U] [S][S]' [U]'
                    so [A][U] = [U] [S][S]'
                    so eigenvectors of [A] are just [U]

       case n > m:  [A] = [X]'[X] = [V] [S]'[S] [V]'
                    so [A][V] = [V] [S'][S]
                    so eigenvectors of [A] are [V], but we want [U]
                    note that [X][V] = [U] [S]
                    so pre-multiplying each column vector in [V] by matrix [X]
                    will give the corresponding column in [U], but scaled;
                    below, just L2-normalize the column to get output vector **/

   if( nn <= mm ){                    /* copy eigenvector into output directly */
                                      /* (e.g., more vectors than time points) */

     (void)mean_vector( nsym , nsym , 0 , asym , uvec ) ;
     sval = symeig_sim1( nsym , asym , uvec ) ;

   } else {  /* n > m: transform eigenvector to get left singular vector */
             /* (e.g., more time points than vectors) */

     float *qvec ;

#pragma omp critical (MALLOC)
     qvec = (float *)calloc(sizeof(float),nsym) ;
     (void)mean_vector( nsym , nsym , 0 , asym , qvec ) ;
     sval = symeig_sim1( nsym , asym , qvec ) ;
     qsum = 0.0f ;
     for( ii=0 ; ii < nn ; ii++ ){
       sum = 0.0f ;
       if( xtyp <= 0 )
         for( kk=0 ; kk < mm ; kk++ ) sum += xx[ii+kk*nn] * qvec[kk] ;
       else
         for( kk=0 ; kk < mm ; kk++ ) sum += xar[kk][ii]  * qvec[kk] ;
       uvec[ii] = sum ; qsum += sum*sum ;
     }
     if( qsum > 0.0f ){       /* L2 normalize */
       register float fac ;
       fac = 1.0f/sqrtf(qsum) ;
       for( ii=0 ; ii < nn ; ii++ ) uvec[ii] *= fac ;
     }
#pragma omp critical (MALLOC)
     free(qvec) ;
   }

   /** make it so that uvec dotted into the mean vector is positive **/

   if( tvec != NULL ){
     sum = 0.0f ;
     for( ii=0 ; ii < nn ; ii++ ) sum += tvec[ii]*uvec[ii] ;
     if( sum < 0.0f ){
       for( ii=0 ; ii < nn ; ii++ ) uvec[ii] = -uvec[ii] ;
     }
   }

   /** free at last!!! **/

#pragma omp critical (MALLOC)
   free(asym) ;

   return sqrtf(sval) ;
}

/*----------------------------------------------------------------------------*/
/* Carry out simultaneous iteration (generalized power iteration) on an
   nn X nn matrix stored in asym, with the initial vector stored in vec,
   and the final vector stored there.  Goal is to compute the dominant
   eigenvalue/vector -- the return value is the eigenvalue.  Three vectors
   are iterated to speed up the convergence.
*//*--------------------------------------------------------------------------*/

#undef  FEPS
#define FEPS 0.00000123456f  /* convergence test */

#undef  DEPS
#undef  DEPSQ
#define DEPS  1.e-8
#define DEPSQ 1.e-4   /* sqrt(DEPS) */

static float symeig_sim1( int nn , float *asym , float *vec )
{
   float *u1,*u2,*u3 , *v1,*v2,*v3 , *aj ;
   float sum1,sum2,sum3 , q1,q2,q3 , r1,r2,r3 , s1,s2,s3 ;
   int ii , jj , nite=0 , n=nn ;
   double bb[9] , ev[3] , evold=0.0 ;
   double g11,g12,g13,g22,g23,g33 ;
   double h11,h12,h13,h22,h23,h33 ;

   if( nn < 1 || asym == NULL || vec == NULL ) return -666.0f ;

   /* special cases: 1x1 and 2x2 and 3x3 matrices */

   if( nn == 1 ){
     vec[0] = 1 ; return asym[0] ;
   } else if( nn == 2 ){
     for( ii=0 ; ii < 4 ; ii++ ) bb[ii] = asym[ii] ;
     symeig_2D( bb , ev , 1 ) ;
     vec[0] = bb[0] ; vec[1] = bb[1] ; return ev[0] ;
   } else if( nn == 3 ){
     for( ii=0 ; ii < 9 ; ii++ ) bb[ii] = asym[ii] ;
     symeig_3D( bb , ev , 1 ) ;
     vec[0] = bb[0] ; vec[1] = bb[1] ; vec[2] = bb[2] ; return ev[0] ;
   }

   /* allocate iteration vectors */

#pragma omp critical (MALLOC)
   { u1 = (float *)malloc(sizeof(float)*n) ;
     u2 = (float *)malloc(sizeof(float)*n) ;
     u3 = (float *)malloc(sizeof(float)*n) ;
     v1 = (float *)malloc(sizeof(float)*n) ;
     v2 = (float *)malloc(sizeof(float)*n) ;
     v3 = (float *)malloc(sizeof(float)*n) ;
   }

   /* initialize u1 vector from input vec */

   sum1 = 0.0f ;
   for( ii=0 ; ii < n ; ii++ ){
     u1[ii] = vec[ii] ; sum1 += u1[ii]*u1[ii] ;
   }
   if( sum1 == 0.0f ){
     for( ii=0 ; ii < n ; ii++ ){
       u1[ii] = drand48()-0.3 ; sum1 += u1[ii]*u1[ii] ;
     }
   }
   sum1 = 1.0f / sqrtf(sum1) ;
   for( ii=0 ; ii < n ; ii++ ) u1[ii] *= sum1 ;

   /* initialize u2, u3 by flipping signs in u1 */

   for( ii=0 ; ii < n ; ii++ ){
     jj = (int)lrand48() ;
     u2[ii] = (((jj >> 3)%2 == 0) ? u1[ii] : -u1[ii]) + 0.001f*drand48() ;
     u3[ii] = (((jj >> 7)%2 == 0) ? u1[ii] : -u1[ii]) + 0.001f*drand48() ;
   }

   /*----- iteration loop -----*/

   while(1){
     /* form V = A U */

     for( ii=0 ; ii < n ; ii++ ){ v1[ii] = v2[ii] = v3[ii] = 0.0f ; }
     for( jj=0 ; jj < n ; jj++ ){
       aj = asym + jj*n ;            /* ptr to jj-th column of asym */
       sum1 = u1[jj] ; sum2 = u2[jj] ; sum3 = u3[jj] ;
       for( ii=0 ; ii < n ; ii++ ){
         v1[ii] += aj[ii] * sum1 ;
         v2[ii] += aj[ii] * sum2 ;
         v3[ii] += aj[ii] * sum3 ;
       }
     }

     /* form G = U' U   and   H = U' V */

     g11 = g12 = g22 = g13 = g23 = g33 = 0.0f ;
     h11 = h12 = h22 = h13 = h23 = h33 = 0.0f ;
     for( ii=0 ; ii < n ; ii++ ){
       g11 += u1[ii] * u1[ii] ; g12 += u1[ii] * u2[ii] ;
       g22 += u2[ii] * u2[ii] ; g13 += u1[ii] * u3[ii] ;
       g23 += u2[ii] * u3[ii] ; g33 += u3[ii] * u3[ii] ;
       h11 += u1[ii] * v1[ii] ; h12 += u1[ii] * v2[ii] ;
       h22 += u2[ii] * v2[ii] ; h13 += u1[ii] * v3[ii] ;
       h23 += u2[ii] * v3[ii] ; h33 += u3[ii] * v3[ii] ;
     }

     /* Choleski-ize G = L L' (in place) */

     g11 = (g11 <= 0.0) ? DEPSQ : sqrt(g11) ;
     g12 = g12 / g11 ;
     g22 = g22 - g12*g12 ; g22 = (g22 <= 0.0) ? DEPSQ : sqrt(g22) ;
     g13 = g13 / g11 ;
     g23 = (g23 - g12*g13) / g22 ;
     g33 = g33 - g13*g13 * g23*g23 ; g33 = (g33 <= 0.0) ? DEPSQ : sqrt(g33) ;

     /* invert lower triangular L (in place) */

     g13 = ( g12*g23 - g22*g13 ) / (g11*g22*g33) ;
     g11 = 1.0 / g11 ; g22 = 1.0 / g22 ; g33 = 1.0 / g33 ;
     g23 = -g23 * g33 * g22 ;
     g12 = -g12 * g11 * g22 ;

     /* form B = inv[L] H inv[L]' (code from Maple 9.5) */

    { double t1, t3, t5, t13, t18, t34, t42, t47 ;
      t1  = g11 * g11 ; t3  = g11 * h11 ; t5  = g11 * h12 ;
      t13 = g12 * g12 ; t18 = g22 * g22 ; t34 = g13 * g13 ;
      t42 = g23 * g23 ; t47 = g33 * g33 ;
      bb[0] = t1 * h11 ;
      bb[4] = t13 * h11 + 2.0 * g12 * g22 * h12 + t18 * h22 ;
      bb[8] = t34 * h11 + 2.0 * g13 * g23 * h12 + 2.0 * g13 * g33 * h13
            + t42 * h22 + 2.0 * g23 * g33 * h23 + t47 * h33 ;
      bb[1] = bb[3] = t3 * g12 + t5 * g22 ;
      bb[2] = bb[6] = t3 * g13 + t5 * g23 + g11 * h13 * g33 ;
      bb[5] = bb[7] = g13 * g12 * h11 + g13 * g22 * h12 + g23 * g12 * h12
                    + g23 * g22 * h22 + g33 * g12 * h13 + g33 * g22 * h23 ;
    }

     /* eigensolve B P  = P D */

     symeig_3D( bb , ev , 1 ) ;  /* P is in bb, D is in ev */

#if 0
fprintf(stderr,"nite=%d  ev=%.9g %.9g %.9g\n",nite,ev[0],ev[1],ev[2]) ;
fprintf(stderr,"         bb=%.5g %.5g %.5g\n"
               "            %.5g %.5g %.5g\n"
               "            %.5g %.5g %.5g\n",
        bb[0],bb[1],bb[2],bb[3],bb[4],bb[5],bb[6],bb[7],bb[8]) ;
#endif

     /* form Q = inv[L]' P */

     q1 = g11*bb[0] + g12*bb[1] + g13*bb[2] ;
     q2 =             g22*bb[1] + g23*bb[2] ;
     q3 =                         g33*bb[2] ;

     r1 = g11*bb[3] + g12*bb[4] + g13*bb[5] ;
     r2 =             g22*bb[4] + g23*bb[5] ;
     r3 =                         g33*bb[5] ;

     s1 = g11*bb[6] + g12*bb[7] + g13*bb[8] ;
     s2 =             g22*bb[7] + g23*bb[8] ;
     s3 =                         g33*bb[8] ;

     /* form U = V Q and normalize it */

     for( ii=0 ; ii < n ; ii++ ){
       u1[ii] = q1 * v1[ii] + q2 * v2[ii] + q3 * v3[ii] ;
       sum1 += u1[ii]*u1[ii] ;
     }
     sum1 = 1.0f / sqrtf(sum1) ;
     for( ii=0 ; ii < n ; ii++ ) u1[ii] *= sum1 ;

     /* check first eigenvalue in D for convergence */

     if( nite > 99 ||
         ( nite > 0 && fabs(ev[0]-evold) <= FEPS*(fabs(evold)+FEPS) ) ) break ;

     /* not done yet ==> iterate */

     nite++ ; evold = ev[0] ;

     /* finish u2 and u3 vectors in U */

     sum2 = sum3 = 0.0f ;
     for( ii=0 ; ii < n ; ii++ ){
       u2[ii] = r1 * v1[ii] + r2 * v2[ii] + r3 * v3[ii] ;
       sum2 += u2[ii]*u2[ii] ;
       u3[ii] = s1 * v1[ii] + s2 * v2[ii] + s3 * v3[ii] ;
       sum3 += u3[ii]*u3[ii] ;
     }
     sum2 = 1.0f / sqrtf(sum2) ;
     sum3 = 1.0f / sqrtf(sum3) ;
     for( ii=0 ; ii < n ; ii++ ){ u2[ii] *= sum2 ; u3[ii] *= sum3 ; }

   } /*----- end of iteration loop -----*/

   /* result vector is u1 */

   for( ii=0 ; ii < n ; ii++ ) vec[ii] = u1[ii] ;

   /* free work vectors */

#pragma omp critical (MALLOC)
   { free(v3); free(v2); free(v1); free(u3); free(u2); free(u1); }

   return (float)ev[0] ;
}

/*---------------------------------------------------------------------------*/

#undef  SQR
#define SQR(a)  ((a)*(a))

#undef  DET3
#define DET3(m) ( m[0]*m[4]*m[8]-m[0]*m[7]*m[5]-m[1]*m[3]*m[8] \
                 +m[1]*m[6]*m[5]+m[2]*m[3]*m[7]-m[2]*m[6]*m[4] )

#undef  PI
#define PI 3.14159265358979323846

#undef  SWAP
#define SWAP(x,y) (th=(x),(x)=(y),(y)=th)

#undef  CSWAP       /* swap columns in a[] starting at indexes i and j */
#define CSWAP(i,j) (SWAP(a[i],a[j]),SWAP(a[i+1],a[j+1]),SWAP(a[i+2],a[j+2]))

/*----------------------------------------------------------------------------*/
/*! Do a 3x3 symmetric eigen-problem as fast as possible, using cubic formula.
     - INPUT: double a[9] = input matrix; a[i+3*j] = matrix (i,j) element
              (actually, only the 0,1,2,4,5,8 elements of a[] are used).
     - OUTPUT: e[i] = i'th eigenvalue, with e[0] >= e[1] >= e[2].
     - OUTPUT: if(dovec) then a[] is replaced with eigenvectors,
               and this orthogonal matrix will have determinant=1.
               Vector #0 is in a[0..2] , #1 in a[3..5], #3 in a[6..8].
*//*--------------------------------------------------------------------------*/

static void symeig_3D( double *a , double *e , int dovec )
{
   double aa,bb,cc,dd,ee,ff ;
   double a1,a2,a3 , qq,rr, qs,th , lam1,lam2,lam3 ;
   double aba,abb,abc,abd,abe,abf , ann ;
   double d12,d13,d23 ;
   double u1,u2,u3 , v1,v2,v3 , w1,w2,w3 , t1,t2,t3 , tn ;
   double anni ;

   if( a == NULL || e == NULL ) return ;

   /*----- unload matrix into local variables -----*/

   aa = a[0] ; bb = a[1] ; cc = a[2] ;  /* matrix is [ aa bb cc ]  */
   dd = a[4] ; ee = a[5] ; ff = a[8] ;  /*           [ bb dd ee ]  */
                                        /*           [ cc ee ff ]  */
   aba = fabs(aa) ; abb = fabs(bb) ; abc = fabs(cc) ;
   abd = fabs(dd) ; abe = fabs(ee) ; abf = fabs(ff) ;
   ann = aba+abb+abc+abd+abe+abf   ;                 /* matrix 'norm' */

   if( ann == 0.0 ){             /* matrix is all zero! */
     e[0] = e[1] = e[2] = 0.0 ;
     if( dovec ){
       a[0] = a[4] = a[8] = 1.0 ;
       a[1] = a[2] = a[3] = a[5] = a[6] = a[7] = 0.0 ;
     }
     return ;
   }

   /*----- check for matrix that is essentially diagonal -----*/

   if( abb+abc+abe == 0.0 ||
       ( DEPS*aba > (abb+abc) && DEPS*abd > (abb+abe) && DEPS*abf > (abc+abe) ) ){

     lam1 = aa ; lam2 = dd ; lam3 = ff ;

     if( dovec ){
       a[0] = a[4] = a[8] = 1.0 ;
       a[1] = a[2] = a[3] = a[5] = a[6] = a[7] = 0.0 ;

       if( lam1 < lam2 ){ SWAP(lam1,lam2) ; CSWAP(0,3) ; }
       if( lam1 < lam3 ){ SWAP(lam1,lam3) ; CSWAP(0,6) ; }
       if( lam2 < lam3 ){ SWAP(lam2,lam3) ; CSWAP(3,6) ; }
       if( DET3(a) < 0.0 ){ a[6] = -a[6]; a[7] = -a[7]; a[8] = -a[8]; }
     } else {
       if( lam1 < lam2 )  SWAP(lam1,lam2) ;
       if( lam1 < lam3 )  SWAP(lam1,lam3) ;
       if( lam2 < lam3 )  SWAP(lam2,lam3) ;
     }
     e[0] = lam1 ; e[1] = lam2 ; e[2] = lam3 ;
     return ;
   }

   /*-- Scale matrix so abs sum is 1; unscale e[i] on output [26 Oct 2005] --*/

   anni = 1.0 / ann ;                      /* ann != 0, from above */
   aa *= anni ; bb *= anni ; cc *= anni ;
   dd *= anni ; ee *= anni ; ff *= anni ;

   /*----- not diagonal ==> must solve cubic polynomial for eigenvalues -----*/
   /*      the cubic polynomial is x**3 + a1*x**2 + a2*x + a3 = 0            */

   a1 = -(aa+dd+ff) ;
   a2 =  (aa*ff+aa*dd+dd*ff - bb*bb-cc*cc-ee*ee) ;
   a3 =  ( aa*(ee*ee-dd*ff) + bb*(bb*ff-cc*ee) + cc*(cc*dd-bb*ee) ) ;

   /*-- Rewrite classical formula for qq as a sum of squares [26 Oct 2005] --*/
#if 0
   qq = (a1*a1 - 3.0*a2) / 9.0 ;
#else
   qq = (  0.5 * ( SQR(dd-aa) + SQR(ff-aa) + SQR(ff-dd) )
         + 3.0 * ( bb*bb      + cc*cc      + ee*ee      ) ) / 9.0 ;
#endif
   rr = (2.0*a1*a1*a1 - 9.0*a1*a2 + 27.0*a3) / 54.0 ;

   if( qq <= 0.0 ){       /*** This should never happen!!! ***/
#pragma omp critical (STDERR)
     {
     static int nerr=0 ;
     if( ++nerr < 4 )
       fprintf(stderr,"** ERROR in symeig_3D: discrim=%g numer=%g\n",qq,rr) ;
     }
     qs = qq = rr = 0.0 ;
   } else {
     qs = sqrt(qq) ; rr = rr / (qs*qq) ;
     if( rr < -1.0 ) rr = -1.0 ; else if( rr > 1.0 ) rr = 1.0 ;
   }
   th = acos(rr) ;

   lam1 = -2.0 * qs * cos(  th        /3.0 ) - a1 / 3.0 ;
   lam2 = -2.0 * qs * cos( (th+2.0*PI)/3.0 ) - a1 / 3.0 ;
   lam3 = -2.0 * qs * cos( (th+4.0*PI)/3.0 ) - a1 / 3.0 ;

   /*-- if not doing eigenvectors, just sort the eigenvalues to be done --*/

   if( !dovec ){
     if( lam1 < lam2 ) SWAP(lam1,lam2) ;
     if( lam1 < lam3 ) SWAP(lam1,lam3) ;
     if( lam2 < lam3 ) SWAP(lam2,lam3) ;
     e[0] = ann*lam1 ; e[1] = ann*lam2 ; e[2] = ann*lam3 ;
     return ;
   }

   /*-- are doing eigenvectors; must do double root as a special case --*/

#undef  CROSS  /* cross product (x1,x2,x3) X (y1,y2,y3) -> (z1,z2,z3) */
#define CROSS(x1,x2,x3,y1,y2,y3,z1,z2,z3) \
 ( (z1)=(x2)*(y3)-(x3)*(y2), (z2)=(x3)*(y1)-(x1)*(y3), (z3)=(x1)*(y2)-(x2)*(y1) )

   d12 = fabs(lam1-lam2) ; d13 = fabs(lam1-lam3) ; d23 = fabs(lam2-lam3) ;
   rr  = MIN(d12,d13)    ; rr  = MIN(rr,d23)     ;

   if( rr > DEPS*ann ){  /*---- not a double root ----*/

     if( lam1 < lam2 )  SWAP(lam1,lam2) ;  /* start by sorting eigenvalues */
     if( lam1 < lam3 )  SWAP(lam1,lam3) ;
     if( lam2 < lam3 )  SWAP(lam2,lam3) ;
     e[0] = ann*lam1 ; e[1] = ann*lam2 ; e[2] = ann*lam3 ;

     /* find eigenvector for lam1 by computing Ay-lam1*y for
        vectors y1=[1,0,0], y2=[0,1,0], and y3=[0,0,1]; the eigenvector
        is orthogonal to all of these, so use the cross product to get it */

     u1 = aa-lam1 ; u2 = bb      ; u3 = cc ;   /* A*y1 - lam1*y1 */
     v1 = bb      ; v2 = dd-lam1 ; v3 = ee ;   /* A*y2 - lam1*y2 */
     CROSS(u1,u2,u3 , v1,v2,v3 , t1,t2,t3 ) ;     tn = sqrt(t1*t1+t2*t2+t3*t3) ;
     if( tn < DEPSQ*ann ){                     /* u and v were parallel? */
       w1 = cc ; w2 = ee ; w3 = ff-lam1 ;      /* A*y3 - lam1*y3 */
       CROSS(u1,u2,u3 , w1,w2,w3 , t1,t2,t3 ) ;   tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < DEPSQ*ann ){                   /* u and w were parallel? */
         CROSS(v1,v2,v3 , w1,w2,w3 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       }
     }
     a[0] = t1/tn ; a[1] = t2/tn ; a[2] = t3/tn ;  /* normalize */

     /* do same for lam2 */

     u1 = aa-lam2 ; u2 = bb      ; u3 = cc ;
     v1 = bb      ; v2 = dd-lam2 ; v3 = ee ;
     CROSS(u1,u2,u3 , v1,v2,v3 , t1,t2,t3 ) ;     tn = sqrt(t1*t1+t2*t2+t3*t3) ;
     if( tn < DEPSQ*ann ){
       w1 = cc ; w2 = ee ; w3 = ff-lam2 ;
       CROSS(u1,u2,u3 , w1,w2,w3 , t1,t2,t3 ) ;   tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < DEPSQ*ann ){
         CROSS(v1,v2,v3 , w1,w2,w3 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       }
     }
     a[3] = t1/tn ; a[4] = t2/tn ; a[5] = t3/tn ;

     /* orthgonality of eigenvectors ==> can get last one by cross product */

#if 1
     CROSS( a[0],a[1],a[2] , a[3],a[4],a[5] , a[6],a[7],a[8] ) ;
#else
     u1 = aa-lam3 ; u2 = bb      ; u3 = cc ;
     v1 = bb      ; v2 = dd-lam3 ; v3 = ee ;
     CROSS(u1,u2,u3 , v1,v2,v3 , t1,t2,t3 ) ;     tn = sqrt(t1*t1+t2*t2+t3*t3) ;
     if( tn < DEPSQ*ann ){
       w1 = cc ; w2 = ee ; w3 = ff-lam3 ;
       CROSS(u1,u2,u3 , w1,w2,w3 , t1,t2,t3 ) ;   tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < DEPSQ*ann ){
         CROSS(v1,v2,v3 , w1,w2,w3 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       }
     }
     a[6] = t1/tn ; a[7] = t2/tn ; a[8] = t3/tn ;
#endif

     return ;

   } else { /*---- if here, we have a double root ----*/

     /* make sure that we have lam1=lam2 and lam3 is the outlier */

          if( d13 < d12 && d13 < d23 ) SWAP(lam2,lam3) ;
     else if( d23 < d12 && d23 < d13 ) SWAP(lam1,lam3) ;
     lam1 = lam2 = 0.5*(lam1+lam2) ;

     /* compute eigenvector for lam3 using method as above */

     u1 = aa-lam3 ; u2 = bb      ; u3 = cc ;
     v1 = bb      ; v2 = dd-lam3 ; v3 = ee ;
     CROSS(u1,u2,u3 , v1,v2,v3 , t1,t2,t3 ) ;     tn = sqrt(t1*t1+t2*t2+t3*t3) ;
     if( tn < DEPSQ*ann ){
       w1 = cc ; w2 = ee ; w3 = ff-lam3 ;
       CROSS(u1,u2,u3 , w1,w2,w3 , t1,t2,t3 ) ;   tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < DEPSQ*ann ){
         CROSS(v1,v2,v3 , w1,w2,w3 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       }
     }
     w1 = a[6] = t1/tn ; w2 = a[7] = t2/tn ; w3 = a[8] = t3/tn ;

     /* find a vector orthogonal to it */

     CROSS(w1,w2,w3 , 1,0,0 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
     if( tn < DEPSQ ){
       CROSS(w1,w2,w3 , 0,1,0 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < DEPSQ ){
         CROSS(w1,w2,w3 , 0,0,1 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       }
     }
     a[0] = t1/tn ; a[1] = t2/tn ; a[2] = t3/tn ;

     /* and the final vector is the cross product of these two */

     CROSS( w1,w2,w3 , a[0],a[1],a[2] , a[3],a[4],a[5] ) ;

     /* sort results (we know lam1==lam2) */

     if( lam1 < lam3 ){
       SWAP(lam1,lam3) ; CSWAP(0,6) ;
       if( DET3(a) < 0.0 ){ a[6] = -a[6]; a[7] = -a[7]; a[8] = -a[8]; }
     }

     e[0] = ann*lam1 ; e[1] = ann*lam2 ; e[2] = ann*lam3 ;
     return ;
   }
}

/*---------------------------------------------------------------------------*/
/*! 2x2 symmetric eigenvalue/vector problem, like symeig_3D() above. */

static void symeig_2D( double *a , double *e , int dovec )
{
   double sxx,sxy,syy , lam1,lam2 , ss,tt , x,y ;

   if( a == NULL || e == NULL ) return ;

   /*----- unload matrix into local variables -----*/

   sxx = a[0] ; sxy = a[1] ; syy = a[3] ;

   ss = fabs(sxx) ; tt = fabs(syy) ; if( ss > tt ) ss = tt ;

   if( fabs(sxy) < DEPS*ss ){   /*--- essentially a diagonal matrix ---*/
     if( sxx >= syy ){
       lam1 = sxx ; lam2 = syy ;
       if( dovec ){ a[0]=a[3]=1.0; a[1]=a[2]=0.0; }
     } else {
       lam1 = syy ; lam2 = sxx ;
       if( dovec ){ a[0]=a[3]=1.0 ; a[1]=a[2]=1.0; }
     }
     e[0] = lam1 ; e[1] = lam2 ;
     return ;
   }

   /*--- non-diagonal matrix ==> solve quadratic equation for eigenvalues ---*/

   ss = sqrt( (sxx-syy)*(sxx-syy) + 4.0*sxy*sxy ) ;   /* positive */
   lam1 = 0.5 * ( sxx + syy + ss ) ;                  /* larger */
   lam2 = 0.5 * ( sxx + syy - ss ) ;                  /* smaller */

   if( dovec ){
     x = 2.0*sxy ; y = syy - sxx + ss ; tt = sqrt(x*x+y*y) ;
     a[0] = x/tt ; a[1] = y/tt ;

     y = syy - sxx - ss ; tt = sqrt(x*x+y*y) ;
     a[2] = x/tt ; a[3] = y/tt ;
   }
   e[0] = lam1 ; e[1] = lam2 ;
   return ;
}
