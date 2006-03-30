#include "mrilib.h"

static void linear_filter_reflect( int ntap, float *wt, int npt, float *x ) ;
static void median5_filter_reflect( int npt, float *x ) ;

/*-------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *yset=NULL , *aset=NULL , *mset=NULL ;
   MRI_IMAGE *fim=NULL, *qim,*tim, *pfim=NULL ;
   float     *flar    , *qar,*tar, *par=NULL ;
   MRI_IMARR *fimar=NULL ;
   MRI_IMAGE *aim , *yim ; float *aar , *yar ;
   int nt=0 , nxyz=0 , nvox=0 , nparam=0 , nqbase , polort=0 , ii,jj,kk,bb ;
   byte *mask=NULL ; int nmask=0 , iarg ;
   char *fname_out="-" ;   /** equiv to stdout **/
   float alpha=0.0f ;
   int   nfir =0 ; float firwt[5]={0.09f,0.25f,0.32f,0.25f,0.09f} ;
   int   nmed =0 ;

   /**--- help the pitiful user? ---**/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dInvFMRI [options]\n"
      "Program to compute stimulus time series, given a 3D+time dataset\n"
      "and an activation map (the inverse of the usual FMRI analysis problem).\n"
      "\n"
      "OPTIONS:\n"
      "\n"
      " -data yyy  =\n"
      "   *OR*     = Defines input 3D+time dataset [a non-optional option].\n"
      " -input yyy =\n"
      "\n"
      " -map  aaa  = Defines activation map; 'aaa' should be a bucket dataset,\n"
      "                each sub-brick of which defines the beta weight map for\n"
      "                an unknown stimulus time series [also non-optional].\n"
      "\n"
      " -mask mmm  = Defines a mask dataset, to restrict input voxels from\n"
      "                -data and -map.\n"
      "\n"
      " -base fff  = Each column of the 1D file 'fff' defines a baseline time\n"
      "                series; these columns should be the same length as\n"
      "                number of time points in 'yyy'.  Multiple -base options\n"
      "                can be given.\n"
      " -polort pp = Adds polynomials of order 'pp' to the baseline collection.\n"
      "                The default baseline model is '-polort 0' (constant).\n"
      "                To specify no baseline model at all, use '-polort -1'.\n"
      "\n"
      " -out vvv   = Name of 1D output file will be 'vvv'.\n"
      "                [default = '-', which is stdout]\n"
      "\n"
      " -alpha aa  = Set the 'alpha' factor to 'aa'; alpha is used to penalize\n"
      "                large values of the output vectors.  Default is 0.\n"
      "                A large-ish value for alpha would be 0.1.\n"
      "\n"
      " -fir5     = Smooth the results with a 5 point lowpass FIR filter.\n"
      " -median5  = Smooth the results with a 5 point median filter.\n"
      "\n"
      "METHOD:\n"
      " Formulate the problem as\n"
      "    Y = V A' + F C'\n"
      " where Y = data matrix      (N x M) [from -data]\n"
      "       V = stimulus         (N x p) [to -out]\n"
      "       A = map matrix       (M x p) [from -map]\n"
      "       F = baseline matrix  (N x q) [from -base and -polort]\n"
      "       C = baseline weights (M x q) [not computed]\n"
      "       N = time series length = length of -data file\n"
      "       M = number of voxels in mask\n"
      "       p = number of stimulus time series to estimate\n"
      "         = number of paramters in -map file\n"
      "       q = number of baseline parameters\n"
      " The least squares solution is given by\n"
      "                   -1             -1\n"
      "   V0 = [I - F(F'F)  F'] Y A (A'A)\n"
      "\n"
      " This solution minimizes the sum of squares over the N*M elements\n"
      " of the matrix   Y - V A' + F C'   (N.B.: A' means A-transpose).\n"
      "\n"
      " Technically, the solution is unidentfiable up to an arbitrary\n"
      " multiple of the columns of F (i.e., V = V0 + F G, where G is\n"
      " an arbitrary q x p matrix); the solution above is the solution\n"
      " that is orthogonal to the columns of F.\n"
      "\n"
      "-- RWCox - March 2006 - purely for experimental purposes!\n"
     ) ;
     exit(0) ;
   }

   /**--- bureaucracy ---**/

   mainENTRY("3dInvFMRI main"); machdep();
   PRINT_VERSION("3dInvFMRI"); AUTHOR("Zhark");
   AFNI_logger("3dInvFMRI",argc,argv) ;

   /**--- scan command line ---**/

   iarg = 1 ;
   while( iarg < argc ){

     if( strcmp(argv[iarg],"-fir5") == 0 ){
       if( nmed > 0 ) WARNING_message("Ignoring -fir5 in favor of -median5") ;
       else           nfir = 5 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-median5") == 0 ){
       if( nfir > 0 ) WARNING_message("Ignoring -median5 in favor of -fir5") ;
       else           nmed = 5 ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-alpha") == 0 ){
       alpha = (float)strtod(argv[++iarg],NULL) ;
       if( alpha <= 0.0f ){
         alpha = 0.0f ; WARNING_message("-alpha '%s' ignored!",argv[iarg]) ;
       }
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-data") == 0 || strcmp(argv[iarg],"-input") == 0 ){
       if( yset != NULL ) ERROR_exit("Can't input 2 3D+time datasets") ;
       yset = THD_open_dataset(argv[++iarg]) ;
       if( yset == NULL ) ERROR_exit("Can't open dataset %s",argv[iarg]) ;
       nt = DSET_NVALS(yset) ;
       if( nt < 2 ) ERROR_exit("Only 1 sub-brick in dataset %s",argv[iarg]) ;
       nxyz = DSET_NVOX(yset) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-map") == 0 ){
       if( aset != NULL ) ERROR_exit("Can't input 2 -map datasets") ;
       aset = THD_open_dataset(argv[++iarg]) ;
       if( aset == NULL ) ERROR_exit("Can't open dataset %s",argv[iarg]) ;
       nparam = DSET_NVALS(aset) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       if( mset != NULL ) ERROR_exit("Can't input 2 -mask datasets") ;
       mset = THD_open_dataset(argv[++iarg]) ;
       if( mset == NULL ) ERROR_exit("Can't open dataset %s",argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-polort") == 0 ){
       polort = (int)strtol(argv[++iarg],NULL,10) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-out") == 0 ){
       fname_out = strdup(argv[++iarg]) ;
       if( !THD_filename_ok(fname_out) )
         ERROR_exit("Bad -out filename '%s'",fname_out) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-base") == 0 ){
       if( fimar == NULL ) INIT_IMARR(fimar) ;
       qim = mri_read_1D( argv[++iarg] ) ;
       if( qim == NULL ) ERROR_exit("Can't read 1D file %s",argv[iarg]) ;
       ADDTO_IMARR(fimar,qim) ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unrecognized option '%s'",argv[iarg]) ;
   }

   /**--- finish up processing options ---**/

   if( yset == NULL ) ERROR_exit("No input 3D+time dataset?!") ;
   if( aset == NULL ) ERROR_exit("No input FMRI -map dataset?!") ;

   if( DSET_NVOX(aset) != nxyz )
     ERROR_exit("Grid mismatch between -data and -map") ;

   DSET_load(yset);
   if( !DSET_LOADED(yset) )
     ERROR_exit("Can't load dataset '%s'",DSET_BRIKNAME(yset)) ;
   DSET_load(aset);
   if( !DSET_LOADED(aset) )
     ERROR_exit("Can't load dataset '%s'",DSET_BRIKNAME(aset)) ;

   if( mset != NULL ){
     if( DSET_NVOX(mset) != nxyz )
       ERROR_exit("Grid mismatch between -data and -mask") ;
     DSET_load(mset);
     if( !DSET_LOADED(mset) )
       ERROR_exit("Can't load dataset '%s'",DSET_BRIKNAME(mset)) ;
     mask  = THD_makemask( mset , 0 , 1.0f,-1.0f ); DSET_delete(mset);
     nmask = THD_countmask( nxyz , mask ) ;
     if( nmask < 3 ){
       WARNING_message("Mask has %d voxels -- ignoring!",nmask) ;
       free(mask) ; mask = NULL ; nmask = 0 ;
     }
   }

   /**--- set up baseline funcs in one array ---*/

   nqbase = (polort >= 0 ) ? polort+1 : 0 ;
   if( fimar != NULL ){
     for( kk=0 ; kk < IMARR_COUNT(fimar) ; kk++ ){
       qim = IMARR_SUBIMAGE(fimar,kk) ;
       if( qim->nx != nt )
         WARNING_message("-base '%s' length=%d; data length=%d",qim->nx,nt) ;
       nqbase += qim->ny ;
     }
   }

   nvox = (nmask > 0) ? nmask : nxyz ;

   INFO_message("N = time series length  = %d",nt    ) ;
   INFO_message("M = number of voxels    = %d",nvox  ) ;
   INFO_message("p = number of params    = %d",nparam) ;
   INFO_message("q = number of baselines = %d",nqbase) ;

#undef  F
#define F(i,j) flar[(i)+(j)*nt]   /* nt X nqbase */
   if( nqbase > 0 ){
     fim  = mri_new( nt , nqbase , MRI_float ) ;
     flar = MRI_FLOAT_PTR(fim) ;
     bb = 0 ;
     if( polort >= 0 ){                /** load polynomial baseline **/
       double a = 2.0/(nt-1.0) ;
       for( jj=0 ; jj <= polort ; jj++ ){
         for( ii=0 ; ii < nt ; ii++ )
           F(ii,jj) = (float)Plegendre( a*ii-1.0 , jj ) ;
       }
       bb = polort+1 ;
     }
#undef  Q
#define Q(i,j) qar[(i)+(j)*qim->nx]  /* qim->nx X qim->ny */

     if( fimar != NULL ){             /** load -base baseline columns **/
       for( kk=0 ; kk < IMARR_COUNT(fimar) ; kk++ ){
         qim = IMARR_SUBIMAGE(fimar,kk) ; qar = MRI_FLOAT_PTR(qim) ;
         for( jj=0 ; jj < qim->ny ; jj++ ){
           for( ii=0 ; ii < nt ; ii++ )
             F(ii,bb+jj) = (ii < qim->nx) ? Q(ii,jj) : 0.0f ;
         }
         bb += qim->ny ;
       }
       DESTROY_IMARR(fimar) ; fimar=NULL ;
     }

     /* remove mean from each column after first? */

     if( polort >= 0 && nqbase > 1 ){
       float sum ;
       for( jj=1 ; jj < nqbase ; jj++ ){
         sum = 0.0f ;
         for( ii=0 ; ii < nt ; ii++ ) sum += F(ii,jj) ;
         sum /= nt ;
         for( ii=0 ; ii < nt ; ii++ ) F(ii,jj) -= sum ;
       }
     }

     /* compute pseudo-inverse of baseline matrix,
        so we can project it out from the data time series */

     INFO_message("Computing pseudo-inverse of baseline matrix F") ;

     pfim = mri_matrix_psinv(fim,NULL,0.0f) ; par = MRI_FLOAT_PTR(pfim) ;

#undef  P
#define P(i,j) par[(i)+(j)*nqbase]   /* nqbase X nt */

#if 0
     qim = mri_matrix_transpose(pfim) ;    /** save to disk? **/
     mri_write_1D( "Fpsinv.1D" , qim ) ;
     mri_free(qim) ;
#endif
   }

   /**--- set up map image into aim/aar ---**/

#undef  GOOD
#define GOOD(i) (mask==NULL || mask[i])

#undef  A
#define A(i,j) aar[(i)+(j)*nvox]   /* nvox X nparam */

   INFO_message("Loading map matrix A") ;

   aim = mri_new( nvox , nparam , MRI_float ); aar = MRI_FLOAT_PTR(aim);
   for( jj=0 ; jj < nparam ; jj++ ){
     for( ii=kk=0 ; ii < nxyz ; ii++ ){
       if( GOOD(ii) ){ A(kk,jj) = THD_get_voxel(aset,ii,jj); kk++; }
   }}
   DSET_unload(aset) ;

   /**--- set up data image into yim/yar ---**/

#undef  Y
#define Y(i,j) yar[(i)+(j)*nt]   /* nt X nvox */

   INFO_message("Loading data matrix Y") ;

   yim = mri_new( nt , nvox , MRI_float ); yar = MRI_FLOAT_PTR(yim);
   for( ii=0 ; ii < nt ; ii++ ){
     for( jj=kk=0 ; jj < nxyz ; jj++ ){
       if( GOOD(jj) ){ Y(ii,kk) = THD_get_voxel(yset,jj,ii); kk++; }
   }}
   DSET_unload(yset) ;

   /**--- project baseline out of data image ---**/

   if( pfim != NULL ){
#undef  T
#define T(i,j) tar[(i)+(j)*nt]  /* nt X nvox */
     INFO_message("Projecting baseline out of Y") ;
     qim = mri_matrix_mult( pfim , yim ) ;   /* nqbase X nvox */
     tim = mri_matrix_mult(  fim , qim ) ;   /* nt X nvox */
     tar = MRI_FLOAT_PTR(tim) ;              /* Y projected onto baseline */
     for( jj=0 ; jj < nvox ; jj++ )
       for( ii=0 ; ii < nt ; ii++ ) Y(ii,jj) -= T(ii,jj) ;
     mri_free(tim); mri_free(qim); mri_free(pfim); mri_free(fim);
   }

   /**--- compute pseudo-inverse of map ---**/

   INFO_message("Computing pseudo-inverse of A") ;

   pfim = mri_matrix_psinv(aim,NULL,alpha) ;  /* nparam X nvox */
   if( pfim == NULL ) ERROR_exit("mri_matrix_psinv() fails") ;
   mri_free(aim) ;

   /**--- and apply to data to get results ---*/

   INFO_message("Computing result") ;

   tim = mri_matrix_multranB( yim , pfim ) ; /* nt x nparam */
   mri_free(pfim) ; mri_free(yim) ;

   /**--- smooth? ---**/

   if( nfir > 0 && tim->nx > nfir ){
     INFO_message("FIR-5-ing result") ;
     tar = MRI_FLOAT_PTR(tim) ;
     for( jj=0 ; jj < tim->ny ; jj++ )
       linear_filter_reflect( nfir,firwt , tim->nx , tar + (jj*tim->nx) ) ;
   }

   if( nmed > 0 && tim->nx > nmed ){
     INFO_message("Median-5-ing result") ;
     tar = MRI_FLOAT_PTR(tim) ;
     for( jj=0 ; jj < tim->ny ; jj++ )
       median5_filter_reflect( tim->nx , tar + (jj*tim->nx) ) ;
   }

   /**--- write results ---**/

   INFO_message("Writing result to '%s'",fname_out) ;

   mri_write_1D( fname_out , tim ) ;
   exit(0) ;
}

/*-------------------------------------------------------------------------*/

static void linear_filter_reflect( int ntap, float *wt, int npt, float *x )
{
   int ii , nt2=(ntap-1)/2 , jj , np1=npt-1 , np2=2*(npt-1) ;
   register float sum ;
   static int nfar=0 ;
   static float *far=NULL ;

   if( ntap >= npt || wt == NULL || x == NULL ) return ;

   if( npt > nfar ){
     if( far != NULL ) free(far) ;
     far = (float *)malloc(sizeof(float)*npt) ; nfar = npt ;
   }

#undef  XX
#define XX(i) ( ((i)<0) ? far[-(i)] : ((i)>np1) ? far[np2-(i)] : far[(i)] )

   memcpy( far , x , sizeof(float)*npt ) ;

   for( ii=0 ; ii < npt ; ii++ ){
     for( sum=0.0f,jj=0 ; jj < ntap ; jj++ ) sum += wt[jj] * XX(ii-nt2+jj) ;
     x[ii] = sum ;
   }

   return ;
}

/*-------------------------------------------------------------------------*/

#undef  SWAP
#define SWAP(x,y) (temp=x,x=y,y=temp)
#undef  SORT2
#define SORT2(a,b) if(a>b) SWAP(a,b);

static float median_float5(float *p)
{
    register float temp ;
    SORT2(p[0],p[1]) ; SORT2(p[3],p[4]) ; SORT2(p[0],p[3]) ;
    SORT2(p[1],p[4]) ; SORT2(p[1],p[2]) ; SORT2(p[2],p[3]) ;
    SORT2(p[1],p[2]) ; return(p[2]) ;
}

static void median5_filter_reflect( int npt, float *x )
{
   int ii ;
   float p[5] ;
   static int nfar=0 ;
   static float *far=NULL ;

   if( x == NULL || npt < 5 ) return ;

   if( npt > nfar ){
     if( far != NULL ) free(far) ;
     far = (float *)malloc(sizeof(float)*(npt+4)) ; nfar = npt ;
   }

   memcpy( far+2 , x , sizeof(float)*npt ) ;
   far[0] = x[2]; far[1] = x[1]; far[npt+2] = x[npt-2]; far[npt+3] = x[npt-3];

   for( ii=0 ; ii < npt ; ii++ ){
     p[0] = far[ii]; p[1] = far[ii+1]; p[2] = far[ii+2];
                     p[3] = far[ii+3]; p[4] = far[ii+4];
     x[ii] = median_float5(p) ;
   }

   return ;
}
