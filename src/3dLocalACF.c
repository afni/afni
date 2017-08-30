#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#include "powell_int.c"
#endif

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

static float dx,dy,dz ;
static int nx,ny,nz,nxy,nxyz ;

static int myTHD_extract_nbhd( THD_3dim_dataset *dset, byte *mask,
                               int xx, int yy, int zz, MCW_cluster *nbhd,
                               int *ivar , int *ijkar , float *tsar ) ;

static float_quint ACF_nbhd_vec_to_modelE( int nt , int nvec ,
                                           int *ijkar , float *tsar ) ;

/*------------------------------------------------------------------------*/

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ; static int vn=0 ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}

/*------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL , *outset=NULL ; MRI_IMAGE *fim=NULL ;
   MCW_cluster *nbhd=NULL ;
   byte *mask=NULL ; int mask_nx=0,mask_ny=0,mask_nz=0 , automask=0 ;
   char *prefix="./LocalACF" ;
   char *evprefix=NULL ; int nev ;
   int iarg=1 , verb=1 , ntype=0 , kk,qq,nvals,nt , vstep=0 ;
   float na=0.0f,nb=0.0f,nc=0.0f ;
   int nmask=0 , domean=0 , use_nonmask=0 , nthread=1 ;
   int *kmask=NULL ;
   float *evar=NULL,*fvar=NULL ;
   unsigned int gseed ;
   int despike=0 , corder=0 , unif=1 ;
   int ct=0 ; double cput=0.0 ;

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("\n"
       "Usage: 3dLocalACF [options] inputdataset\n"
       "\n"
       "Options:\n"
       "--------\n"
       " -prefix   ppp\n"
       " -input    inputdataset\n"
       " -nbhd     nnn\n"
       " -mask     maskdataset\n"
       " -automask\n"
       "\n"
       "Notes:\n"
       "------\n"
       "* This program estimates the spatial AutoCorrelation Function (ACF)\n"
       "  locally in a neighborhood around each voxel, unlike '3FWHMx -acf',\n"
       "  which produces an average over the whole volume.\n"
       "\n"
       "* The input dataset must be a time series dataset, and must have\n"
       "  been detrended, despiked, etc. already.  The 'errts' output from\n"
       "  afni_proc.py is recommended!\n"
       "\n"
       "* A brain mask is highly recommended as well.\n"
       "\n"
       "* I typically use 'SPHERE(25)' for the neighborhood.  YMMV.\n"
       "\n"
       "* This program is very slow.\n"
#ifndef USE_OMP
       "   And this copy of it does NOT use multiple threads (OpenMP),\n"
       "   which will make it extremely slow!\n"
#else
       "   This copy of it uses multiple threads (OpenMP), so it is\n"
       "   somewhat tolerable to use.\n"
#endif
       "\n"
       "***** This program is experimental *****\n"
       "\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*---- official startup ---*/

   PRINT_VERSION("3dLocalACF"); mainENTRY("3dLocalACF main"); machdep();
   AFNI_logger("3dLocalACF",argc,argv); AUTHOR("Emperor Zhark the Autocorrelator");

   ct = NI_clock_time() ;  /* start the timer */

   /*---- loop over options ----*/

   while( iarg < argc && argv[iarg][0] == '-' ){

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

   kmask = (int *)malloc(sizeof(int)*nmask) ;
   for( kk=qq=0 ; kk < nxyz ; kk++ ){
     if( INMASK(kk) ) kmask[qq++] = kk ;
   }

   /*---- create neighborhood (as a cluster) -----*/

   if( ntype <= 0 ){         /* default neighborhood */
     ntype = NTYPE_SPHERE ; na = -9.666f ;
     INFO_message("Using default neighborhood: SPHERE(-9.666)") ;
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
   dx = fabsf(DSET_DX(inset)) ;
   dy = fabsf(DSET_DY(inset)) ;
   dz = fabsf(DSET_DZ(inset)) ;
   MCW_radsort_cluster( nbhd, dx,dy,dz ) ; /* ensures first value is centroid */

   INFO_message("Neighborhood comprises %d voxels",nbhd->num_pt) ;
   INFO_message("Each time series has %d points",nt) ;

   if( nbhd->num_pt < 9 )
     ERROR_exit("Neighborhood is too small to process :-(") ;

   /** create output dataset **/

   outset = EDIT_empty_copy(inset) ;
   EDIT_dset_items( outset,
                      ADN_prefix   , prefix,
                      ADN_brick_fac, NULL  ,
                      ADN_nvals    , 5     ,
                      ADN_ntt      , 0     ,
                    ADN_none );
   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dLocalACF" , argc,argv , outset ) ;
   for( kk=0 ; kk < 5 ; kk++ )                         /* create bricks */
     EDIT_substitute_brick( outset , kk , MRI_float , NULL ) ;

   EDIT_BRICK_LABEL(outset,0,"ACF:a") ;
   EDIT_BRICK_LABEL(outset,1,"ACF:b") ;
   EDIT_BRICK_LABEL(outset,2,"ACF:c") ;
   EDIT_BRICK_LABEL(outset,3,"ACF:FWHM") ;
   EDIT_BRICK_LABEL(outset,4,"ACF:FWQM") ;

#if 0
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
#endif

#ifdef USE_OMP
#pragma omp parallel
 {
#pragma omp master
   nthread = omp_get_num_threads() ;
 }
#endif

   vstep = (verb && nmask > 999) ? nmask/50 : 0 ;
   if( vstep ){
     if( nthread == 1 ) fprintf(stderr,"++ voxel loop: ") ;
     else               fprintf(stderr,"++ OpenMP [%d] voxel loop: ",nthread) ;
   }

   /*** the real work now begins ***/

 AFNI_OMP_START ;
#pragma omp parallel if( nmask > 333 )
 { int kk,qq , xx,yy,zz , vv,ii , mm ;
   float *zar=NULL , *nbar ; int *ivar , *ijkar ;
   float *tsar ;
   int ithr=0 ; float sval,tval=0.0 ;
   float_quint Epar ; float Evec[5] ;

#pragma omp critical (MALLOC)
   { nbar  = (float *)malloc(sizeof(float)*nt*nbhd->num_pt+4096) ;
     ivar  = (int   *)malloc(sizeof(int)     *nbhd->num_pt+4096) ;
     ijkar = (int   *)malloc(sizeof(int)  * 3*nbhd->num_pt+4096) ;
   }

#ifdef USE_OMP
   ithr = omp_get_thread_num() ;
#endif

#pragma omp for   /* parallel loop over voxels */
   for( qq=0 ; qq < nmask ; qq++ ){
     kk = kmask[qq] ;

     if( vstep && qq%vstep==7 )
#pragma omp critical (VSTEP)
     {  vstep_print() ; }

     IJK_TO_THREE( kk , xx,yy,zz , nx,nxy ) ;

     mm = myTHD_extract_nbhd( inset , mask , xx,yy,zz , nbhd , ivar,ijkar,nbar ) ;
     if( mm <= 9 ) continue ;  /* no data? */

     Epar = ACF_nbhd_vec_to_modelE( nt , mm , ijkar , nbar ) ;
     if( Epar.a >= 0.0f ){
       Evec[0] = Epar.a; Evec[1] = Epar.b; Evec[2] = Epar.c; Evec[3] = Epar.d; Evec[4] = Epar.e;
       THD_insert_series( kk, outset, 5, MRI_float, Evec, 0 ) ;
     }

   } /* end parallel loop */

#pragma omp critical (MALLOC)
   { free(nbar) ; free(ivar) ; free(ijkar) ; }
 } /* end OpenMP */
 AFNI_OMP_END ;

   if( vstep ) fprintf(stderr,"\n++ Voxel loop finished\n") ;

   DSET_delete(inset) ; free(kmask) ;

   /*** median filter ***/

   if( verb ) INFO_message("Median filter output (a little)") ;

   mri_medianfilter_usedxyz(0) ;
   for( kk=0 ; kk < 5 ; kk++ ){
     fim = mri_medianfilter( DSET_BRICK(outset,kk) , 1.444f , mask , 0 ) ;
     EDIT_substitute_brick( outset , kk , MRI_float , MRI_FLOAT_PTR(fim) ) ;
   }

   /*** e finito ***/

   if( verb ) INFO_message("Writing output ...") ;

   DSET_write(outset) ; WROTE_DSET(outset) ; DSET_delete(outset) ;

   cput = COX_cpu_time() ;
   if( cput > 0.05 )
     INFO_message("===== CPU time = %.1f sec  clock time =%s",
                  cput , nice_time_string(NI_clock_time()-ct) ) ;
   else
     INFO_message("===== clock time =%s" , nice_time_string(NI_clock_time()-ct) ) ;

   exit(0) ;
}

/*------------------------------------------------------------------------*/

#undef  TS
#define TS(i,j) tsar[(i)+(j)*nv]

static int myTHD_extract_nbhd( THD_3dim_dataset *dset, byte *mask,
                               int xx, int yy, int zz, MCW_cluster *nbhd,
                               int *ivar , int *ijkar , float *tsar )
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
     if( mask == NULL || mask[vv] ){
       ivar[nvox]      = vv ;
       ijkar[3*nvox+0] = nbhd->i[ii] ;
       ijkar[3*nvox+1] = nbhd->j[ii] ;
       ijkar[3*nvox+2] = nbhd->k[ii] ; nvox++ ;
     }
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

/*------------------------------------------------------------------------*/

AO_DEFINE_SCALAR(int,nnar) ;
AO_DEFINE_ARRAY(float,ccar) ;
AO_DEFINE_ARRAY(float,rrar) ;

double ACF_modelE_cost( int npar , double *par )
{
   float aa=(float)par[0], bb=(float)par[1], cc=(float)par[2] ;
   float a1=1.0f-aa      , bq=0.5f/(bb*bb) , ci=1.0f/cc ;
   float sum=0.0f , fval ;
   int ii , nnar=AO_VALUE(nnar) ;
   float   *ccar=AO_VALUE(ccar) ;
   float   *rrar=AO_VALUE(rrar) ;

   for( ii=0 ; ii < nnar ; ii++ ){
     fval = aa*expf(-bq*rrar[ii]*rrar[ii]) + a1*expf(-ci*rrar[ii]) - ccar[ii] ;
     sum += fval * fval ;
   }

   return (double)sum ;
}

/*------------------------------------------------------------------------*/

AO_DEFINE_SCALAR(float,apar) ;
AO_DEFINE_SCALAR(float,bpar) ;
AO_DEFINE_SCALAR(float,cpar) ;
AO_DEFINE_SCALAR(float,flev) ;

double ACF_fhwm_cost( int npar , double *par )
{
   float fit , rr=(float)(*par) ;
   float apar,bpar,cpar , flev ;

   apar = AO_VALUE(apar) ; bpar = AO_VALUE(bpar) ;
   cpar = AO_VALUE(cpar) ; flev = AO_VALUE(flev) ;

   fit =         apar * expf( -0.5f*rr*rr/(bpar*bpar) )
        + (1.0f-apar) * expf( -rr/cpar) ;

   return (double)fabsf(fit-flev) ;
}

float ACF_fwhm( float apar , float bpar , float cpar )
{
   double rhalf , rtop , rbot ;

   rtop = (double)(2.0f*bpar+cpar) ;
   rbot = 0.0333*rtop ;

   AO_VALUE(apar) = apar; AO_VALUE(bpar) = bpar; AO_VALUE(cpar) = cpar;

   rhalf = minimize_in_1D( rbot        , rtop      , ACF_fhwm_cost ) ;
   rhalf = minimize_in_1D( 0.666*rhalf , 1.5*rhalf , ACF_fhwm_cost ) ;

   return (float)(2.0*rhalf) ;
}

/*------------------------------------------------------------------------*/

static float_quint ACF_nbhd_vec_to_modelE( int nt , int nvec ,
                                           int *ijkar , float *tsar )
{
   float_quint Epar={-1.0f,-1.0f,-1.0f,-1.0f,-1.0f} ;
   float *zar=tsar , zsq , *var , vsq , ssq , xx,yy,zz , vp,rp,vs ;
   int pp,qq,dq,ss , tt ;
   float *ccar , *rrar ;
   float apar , bpar , cpar , dpar , epar ;
   double xpar[3] , xbot[3] , xtop[3] ;

   if( nt==0 || nvec==0 || ijkar==NULL || tsar==NULL ) return Epar ;  /* ERROR */

   /*-- step 1: compute correlation coeff of center vox with each other ---*/

   zsq = 0.0f ;
   for( tt=0 ; tt < nt ; tt++ ) zsq += zar[tt]*zar[tt] ;
   if( zsq == 0.0f ) return Epar ;        /* should not happen */

   AO_RESIZE_ARRAY(float,ccar,nvec) ; ccar = AO_VALUE(ccar) ;
   AO_RESIZE_ARRAY(float,rrar,nvec) ; rrar = AO_VALUE(rrar) ;

#undef  CRAD
#define CRAD(a) ( xx = dx * abs(ijkar[3*(a)+0]) ,   \
                  yy = dy * abs(ijkar[3*(a)+1]) ,   \
                  zz = dz * abs(ijkar[3*(a)+2]) , sqrtf(xx*xx+yy*yy+zz*zz) )

   ccar[0] = 1.0f ; rrar[0] = 0.0f ;
   for( pp=1 ; pp < nvec ; pp++ ){
     ssq = vsq = 0.0f ; var = tsar + nt*pp ;
     for( tt=0 ; tt < nt ; tt++ ){ vsq += var[tt]*var[tt] ; ssq += var[tt]*zar[tt] ; }
     if( vsq == 0.0f ){  /* should not happen */
       ccar[pp] = 0.0f ;
       rrar[pp] = 6666.66f ;
     } else {
       ccar[pp] = ssq / sqrtf(zsq*vsq) ; if( ccar[pp] > 1.0f ) ccar[pp] = 1.0f ;
       rrar[pp] = CRAD(pp) ;
     }
   }

   qsort_floatfloat( nvec , rrar , ccar ) ; /* sort to ensure radius is increasing */

   for( pp=nvec-1 ; pp > 0 && rrar[pp] > 6666.0f ; pp-- ) ; /*nada*/
   if( pp < 9 ) return Epar ;  /* should not happen */
   nvec = pp+1 ;               /* edited out the vsq==0 cases */

   /* collapse data with similar rrar values */

   for( pp=1 ; pp < nvec-1 ; pp++ ){
     vp = ccar[pp] ; rp = rrar[pp]*1.01f ;
     for( qq=pp+1 ; qq < nvec & rrar[qq] <= rp ; qq++ ) ; /*nada*/
     if( qq > pp+1 ){
       for( vs=0.0f,ss=pp ; ss < qq ; ss++ ) vs += ccar[ss] ;
       ccar[pp] = vs/(qq-pp) ;
       dq = qq-(pp+1) ;
       for( ss=qq ; ss < nvec ; ss++ ){
         ccar[ss-dq] = ccar[ss] ; rrar[ss-dq] = rrar[ss] ;
       }
       nvec -= dq ;
     }
   }

   /*-- step 2: estimate ACF parameters from rrar vs. ccar data --*/

   for( pp=0 ; pp < nvec && ccar[pp] > 0.5f ; pp++ ) ; /*nada*/
   if( pp == nvec ) return Epar ;  /* ERROR */
   if( ccar[pp] <= 0.0f || ccar[pp] >= 1.0f ){
     bpar = cpar = sqrtf(dx*dx+dy*dz+dz*dz) ;
   } else {
     apar = 1.0 / logf(ccar[pp]) ;  /* negative since 0 < ccar[pp] < 1 */
     bpar = sqrtf(-0.5*apar) * rrar[pp] ;
     cpar = -rrar[pp] * apar;
   }
   apar = 0.5f ;

   /* nonlinear optimization of params */

   xpar[0] = apar  ; xpar[1] = bpar      ; xpar[2] = cpar      ;
   xbot[0] = 0.006 ; xbot[1] = 0.05*bpar ; xbot[2] = 0.05*cpar ;
   xtop[0] = 0.994 ; xtop[1] = 5.55*bpar ; xtop[2] = 5.55*cpar ;

   AO_VALUE(nnar) = nvec ;

   pp = powell_newuoa_constrained( 3 , xpar , NULL , xbot , xtop ,
                                   444 , 33 , 7 ,
                                   0.09 , 0.0006 , 999 , ACF_modelE_cost ) ;

   if( pp < 0 ) return Epar ;  /* ERROR */

   apar = xpar[0] ; bpar = xpar[1] ; cpar = xpar[2] ;

   /* compute FWHM from these parameters (kind of brute force) */

   AO_VALUE(flev) = 0.5f ;
   dpar = ACF_fwhm(apar,bpar,cpar) ;

   AO_VALUE(flev) = 0.25f ;
   epar = ACF_fwhm(apar,bpar,cpar) ;

   Epar.a = apar; Epar.b = bpar; Epar.c = cpar; Epar.d = dpar; Epar.e = epar;
   return Epar ;
}
