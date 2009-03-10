#include "mrilib.h"

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

/*-------------------------------------------------------------------*/

#undef  FREEIF
#define FREEIF(p) do{ if((p)!=NULL){free(p);(p)=NULL;} }while(0)

static int maxlag = 0 ;
static float *cor = NULL ;
static int  *ncor = NULL ;
static int   port = 0 ;

/*....................................................................*/

void CORMAT_init( int mlag , int pp )
{
   FREEIF(cor) ; FREEIF(ncor) ; maxlag = 0 ;
   if( mlag > 0 ){
      cor   = (float *)calloc( sizeof(float) , mlag ) ;
     ncor   = (int *)  calloc( sizeof(int)   , mlag ) ;
     maxlag = mlag ;
     port   = (pp < 0 || pp > 3) ? 0 : pp ;
   }
   return ;
}

/*....................................................................*/

MRI_IMAGE * CORMAT_fetch(void)
{
   MRI_IMAGE *cim ; float *car ; int ii ;
   if( maxlag <= 0 || cor == NULL ) return NULL ;
   cim = mri_new(maxlag,1,MRI_float) ; car = MRI_FLOAT_PTR(cim) ;
   for( ii=0 ; ii < maxlag ; ii++ )
     if( ncor[ii] > 0 ) car[ii] = cor[ii] / ncor[ii] ;
   return cim ;
}

/*....................................................................*/

void CORMAT_add_vector( int nv , float *vv )
{  
   int ii,itop , ktop,kk ;
   float ss , sq ;
   
   if( maxlag <= 0 || nv <= 2 || vv == NULL ) return ;
   
   switch(port){
     default: 
     case 0:  THD_const_detrend      ( nv , vv , NULL           ); break;
     case 1:  THD_linear_detrend     ( nv , vv , NULL,NULL      ); break;
     case 2:  THD_quadratic_detrend  ( nv , vv , NULL,NULL,NULL ); break;
     case 3:  THD_cubic_detrend      ( nv , vv                  ); break;
   }
   THD_normRMS( nv , vv ) ;
   
   ktop = MIN(maxlag,nv-1) ;
   for( kk=1 ; kk <= ktop ; kk++ ){
     itop = nv-kk ; 
     for( ii=0 ; ii < itop ; ii++ ){
       cor[kk-1] += vv[ii] * vv[ii+kk] ; ncor[kk-1]++ ;
     }
   }
   
   return ;
}

/*-------------------------------------------------------------------*/

MRI_IMAGE * mri_cormat_vector( MRI_IMARR *imar ) ;
MRI_IMARR * THD_get_dset_nbhd_array( THD_3dim_dataset *dset, byte *mask,
                                     int xx, int yy, int zz, MCW_cluster *nbhd ) ;
static void vstep_print(void) ;

float_pair estimate_arma11( int nx , float * ) ;

static int nbk=0 , *bk=NULL , ntime=0 , mlag=0 , pport=0 ;  /* cheap */

/*------------------------------------------------------------------------*/
/* Adapted from 3dLocalSVD.c and 3dErrtsCormat.c */

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL , *outset=NULL ;
   MCW_cluster *nbhd=NULL ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 ;
   char *prefix="./LocalCormat" ;
   int iarg=1 , verb=1 , ntype=0 , kk,nx,ny,nz,nxy,nxyz,nt , xx,yy,zz, vstep ;
   float na,nb,nc , dx,dy,dz ;
   MRI_IMARR *imar=NULL ; MRI_IMAGE *pim=NULL ;
   int mmlag=10 , ii,jj , do_arma=0 , nvout ;
   MRI_IMAGE *concim=NULL ; float *concar=NULL ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3dLocalCORMAT [options] inputdataset\n"
       "\n"
       "Compute the correlation matrix (in time) of the input dataset,\n"
       "up to lag given by -maxlag.  The matrix is averaged over the\n"
       "neighborhood specified by the -nbhd option, and then the entries\n"
       "are output at each voxel in a new dataset.\n"
       "\n"
       "Normally, the input to this program would be the -errts output\n"
       "from 3dDeconvolve, or the equivalent residuals from some other\n"
       "analysis.  If you input a non-residual time series file, you at\n"
       "least should use an appropriate -polort level for detrending!\n"
       "\n"
       "Options:\n"
       "  -input inputdataset\n"
       "  -prefix ppp\n"
       "  -mask mset    {these 2 options are}\n"
       "  -automask     {mutually exclusive.}\n"
       "  -nbhd nnn     [e.g., 'SPHERE(9)' for 9 mm radius]\n"
       "  -polort ppp   [default = 0, which is reasonable for -errts output]\n"
       "  -concat ccc   [as in 3dDeconvolve]\n"
       "  -maxlag mmm   [default = 10]\n"
       "  -ARMA         [estimate ARMA(1,1) parameters into last 2 sub-bricks]\n"
       "\n"
       "A quick hack for my own benignant purposes -- RWCox -- June 2008\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dLocalCormat"); mainENTRY("3dLocalCormat main"); machdep();
   AFNI_logger("3dLocalCormat",argc,argv); AUTHOR("Zhark the Toeplitzer");

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

#if 0
fprintf(stderr,"argv[%d] = %s\n",iarg,argv[iarg]) ;
#endif

     if( strcmp(argv[iarg],"-ARMA") == 0 ){
       do_arma = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-polort") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after option %s",argv[iarg-1]) ;
       pport = (int)strtod(argv[iarg],NULL) ;
       if( pport > 3 ){
         pport = 3 ; WARNING_message("-polort set to 3 == max implemented") ;
       } else if( pport < 0 ){
         pport = 0 ; WARNING_message("-polort set to 0 == min implemented") ;
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
       THD_3dim_dataset *mset ; int mmm ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       mmm = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 2 ) ERROR_exit("Mask is too small to process") ;
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
       } else {
          ERROR_exit("Unknown -nbhd shape: '%s'",cpt) ;
       }
       iarg++ ; continue ;
     }
       
     if( strcmp(argv[iarg],"-maxlag") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after option %s",argv[iarg-1]) ;
       mmlag = (int)strtod(argv[iarg],NULL) ;
       iarg++ ; continue ;
     }   

     if( strcmp(argv[iarg],"-concat") == 0 ){
       if( concim != NULL )
         ERROR_exit("Can't have two %s options!",argv[iarg]) ;
       if( ++iarg >= argc )
         ERROR_exit("Need argument after option %s",argv[iarg-1]) ;
       concim = mri_read_1D( argv[iarg] ) ;
       if( concim == NULL )
         ERROR_exit("Can't read -concat file '%s'",argv[iarg]) ;
       if( concim->nx < 2 )
         ERROR_exit("-concat file '%s' must have at least 2 entries!",
                    argv[iarg]) ;
       concar = MRI_FLOAT_PTR(concim) ;
       for( ii=1 ; ii < concim->nx ; ii++ )
         if( (int)concar[ii-1] >= (int)concar[ii] )
           ERROR_exit("-concat file '%s' is not ordered increasingly!",
                      argv[iarg]) ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;

   } /*--- end of loop over options ---*/

   if( do_arma && mmlag > 0 && mmlag < 5 )
     ERROR_exit("Can't do -ARMA with -maxlag %d",mmlag) ;

   /*---- deal with input dataset ----*/

   if( inset == NULL ){
     if( iarg >= argc ) ERROR_exit("No input dataset on command line?") ;
     inset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(inset,argv[iarg]) ;
   }
   ntime = DSET_NVALS(inset) ;
   if( ntime < 9 )
     ERROR_exit("Must have at least 9 values per voxel") ;

   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;

   if( mask != NULL ){
     if( mask_nx != DSET_NX(inset) ||
         mask_ny != DSET_NY(inset) ||
         mask_nz != DSET_NZ(inset)   )
       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( automask ){
     int mmm ;
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     mmm = THD_countmask( DSET_NVOX(inset) , mask ) ;
     INFO_message("Number of voxels in automask = %d",mmm) ;
     if( mmm < 2 ) ERROR_exit("Automask is too small to process") ;
   }

   /*-- set up blocks of continuous time data --*/
       
   if( DSET_IS_TCAT(inset) ){
     if( concim != NULL ){
       WARNING_message("Ignoring -concat, since dataset is auto-catenated") ;
       mri_free(concim) ;
     }
     concim = mri_new(inset->tcat_num,1,MRI_float) ;
     concar = MRI_FLOAT_PTR(concim) ;
     concar[0] = 0.0 ;
     for( ii=0 ; ii < inset->tcat_num-1 ; ii++ )
       concar[ii+1] = concar[ii] + inset->tcat_len[ii] ;
   } else if( concim == NULL ){ 
     concim = mri_new(1,1,MRI_float) ;
     concar = MRI_FLOAT_PTR(concim)  ; concar[0] = 0 ;
   }
   nbk = concim->nx ;
   bk  = (int *)malloc(sizeof(int)*(nbk+1)) ;
   for( ii=0 ; ii < nbk ; ii++ ) bk[ii] = (int)concar[ii] ;
   bk[nbk] = ntime ;
   mri_free(concim) ;
   mlag = DSET_NVALS(inset) ;
   for( ii=0 ; ii < nbk ; ii++ ){
     jj = bk[ii+1]-bk[ii] ; if( jj < mlag ) mlag = jj ;
     if( bk[ii] < 0 || jj < 9 )
       ERROR_exit("something is rotten in the dataset run lengths") ;
   }
   mlag-- ;
   if( mmlag > 0 && mlag > mmlag ) mlag = mmlag ;
   else                            INFO_message("Max lag set to %d",mlag) ;

   if( do_arma && mlag < 5 )
     ERROR_exit("Can't do -ARMA with maxlag=%d",mlag) ;

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
   }
   MCW_radsort_cluster( nbhd , dx,dy,dz ) ;  /* 26 Feb 2008 */

   INFO_message("Neighborhood comprises %d voxels",nbhd->num_pt) ;

   /** create output dataset **/

   outset = EDIT_empty_copy(inset) ;
   nvout  = mlag ; if( do_arma ) nvout += 2 ;
   EDIT_dset_items( outset,
                      ADN_prefix   , prefix,
                      ADN_brick_fac, NULL  ,
                      ADN_nvals    , nvout ,
                      ADN_ntt      , nvout ,
                    ADN_none );
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dLocalCormat" , argc,argv , outset ) ;
   for( kk=0 ; kk < nvout ; kk++ )
     EDIT_substitute_brick( outset , kk , MRI_float , NULL ) ;

   nx = DSET_NX(outset) ;
   ny = DSET_NY(outset) ; nxy  = nx*ny  ;
   nz = DSET_NZ(outset) ; nxyz = nxy*nz ;
   vstep = (verb && nxyz > 999) ? nxyz/50 : 0 ;
   if( vstep ) fprintf(stderr,"++ voxel loop: ") ;

   /** actually do the long long slog through all voxels **/

   for( kk=0 ; kk < nxyz ; kk++ ){
     if( vstep && kk%vstep==vstep-1 ) vstep_print() ;
     if( !INMASK(kk) ) continue ;
     IJK_TO_THREE( kk , xx,yy,zz , nx,nxy ) ;
     imar = THD_get_dset_nbhd_array( inset , mask , xx,yy,zz , nbhd ) ;
     if( imar == NULL ) continue ;
     pim = mri_cormat_vector(imar) ; DESTROY_IMARR(imar) ;
     if( pim == NULL ) continue ;
     THD_insert_series( kk, outset, pim->nx, MRI_float, MRI_FLOAT_PTR(pim), 0 ) ;

     if( do_arma ){  /* estimate ARMA(1,1) params and store those, too */
       float_pair ab ;
       float *aa=DSET_ARRAY(outset,mlag), *bb=DSET_ARRAY(outset,mlag+1) ;
       ab = estimate_arma11( pim->nx , MRI_FLOAT_PTR(pim) ) ;
       aa[kk] = ab.a ; bb[kk] = ab.b ;
     }

     mri_free(pim) ;
   }
   if( vstep ) fprintf(stderr,"\n") ;

   DSET_delete(inset) ;
   DSET_write(outset) ;
   WROTE_DSET(outset) ;

   exit(0) ;
}

/*------------------------------------------------------------------------*/

MRI_IMARR * THD_get_dset_nbhd_array( THD_3dim_dataset *dset, byte *mask,
                                     int xx, int yy, int zz, MCW_cluster *nbhd )
{
   MRI_IMARR *imar ;
   int nvox, *ivox , nx,ny,nz , nxy,nxyz , npt, aa,bb,cc,kk,ii ;

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ; nxy  = nx*ny  ;
   nz = DSET_NZ(dset) ; nxyz = nxy*nz ; npt = nbhd->num_pt ;

   kk = xx + yy*nx + zz*nxy ;
   if( kk < 0 || kk >= nxyz || !INMASK(kk) ) return NULL ;

   ivox = (int *)malloc(sizeof(int)*npt) ; nvox = 0 ;
   for( ii=0 ; ii < npt ; ii++ ){
     aa = xx + nbhd->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
     bb = yy + nbhd->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
     cc = zz + nbhd->k[ii] ; if( cc < 0 || cc >= nz ) continue ;
     kk = aa + bb*nx + cc*nxy ;
     if( INMASK(kk) ) ivox[nvox++] = kk ;
   }

   imar = THD_extract_many_series( nvox, ivox, dset ) ;
   free(ivox) ; return imar ;
}

/*------------------------------------------------------------------------*/

MRI_IMAGE * mri_cormat_vector( MRI_IMARR *imar )
{
   int nx , nvec , ii,jj ;
   double *amat , *umat , *vmat , *sval ;
   float *far ; MRI_IMAGE *tim ;

   if( imar == NULL ) return NULL ;
   nvec = IMARR_COUNT(imar) ;       if( nvec < 1 ) return NULL ;
   nx   = IMARR_SUBIM(imar,0)->nx ; if( nx   < 9 ) return NULL ;

   CORMAT_init( mlag , pport ) ;

   for( jj=0 ; jj < nvec ; jj++ ){
     tim = IMARR_SUBIM(imar,jj) ;
     far = MRI_FLOAT_PTR(tim) ;
     for( ii=0 ; ii < nbk ; ii++ )
       CORMAT_add_vector( bk[ii+1]-bk[ii] , far + bk[ii] ) ;
   }

   tim = CORMAT_fetch() ; return tim ;
}

/*------------------------------------------------------------------------*/

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ; static int vn=0 ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}

/*------------------------------------------------------------------------*/
/* Estimation of ARMA(1,1) coefficients a and b from correlations. */

static int    nc ;  /* number of correlations */
static float *rc ;  /* correlation vector */

double arma11func( int np , double *par )  /* cost = weighted least squares */
{
   register double a,b,sum,rr ; register int kk ;

   a = par[0] ; b = par[1] ;
   rr = (b+a)*(1.0+a*b)/(1.0+b*(b+2.0*a)) ;  /* correlation at lag=1 */
   sum = (rr-rc[0])*(rr-rc[0]) * (0.2+rc[0]*rc[0]) ;
   for( kk=1 ; kk < nc ; kk++ ){
     rr *= a ;                               /* correlation at lag=kk+1 */
     sum += (rr-rc[kk])*(rr-rc[kk]) * (0.2+rc[kk]*rc[kk]) ;
   }
   return sum ;
}

float_pair estimate_arma11( int nx , float *cc )
{
   double x[2] , xbot[2],xtop[2] ;
   float_pair ab ;

   nc = nx ; rc = cc ;              /* set static variables for arma11func() */
   x[0] = x[1] = 0.1 ;              /* initial guesses for a and b */
   xbot[0] = 0.0 ; xtop[0] = 0.8 ;  /* range of allowed values for a and b */
   xbot[1] = 0.0 ; xtop[1] = 0.8 ;

   (void)powell_newuoa_constrained( 2 , x , NULL , xbot , xtop ,
                                    13 , 5 , 2 ,
                                    0.10 , 0.005 , 222 , arma11func ) ;
   ab.a = (float)x[0] ;
   ab.b = (float)x[1] ; return ab ;
}
