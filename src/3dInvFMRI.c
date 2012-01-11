#include "mrilib.h"

static void linear_filter_reflect( int ntap, float *wt, int npt, float *x ) ;
static void median5_filter_reflect( int npt, float *x ) ;

/*-------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *yset=NULL , *aset=NULL , *mset=NULL , *wset=NULL ;
   MRI_IMAGE *fim=NULL, *qim,*tim, *pfim=NULL , *vim     , *wim=NULL  ;
   float     *flar    , *qar,*tar, *par=NULL  , *var     , *war=NULL  ;
   MRI_IMARR *fimar=NULL ;
   MRI_IMAGE *aim , *yim ; float *aar , *yar ;
   int nt=0 , nxyz=0 , nvox=0 , nparam=0 , nqbase , polort=0 , ii,jj,kk,bb ;
   byte *mask=NULL ; int nmask=0 , iarg ;
   char *fname_out="-" ;   /** equiv to stdout **/

   float alpha=0.0f ;
   int   nfir =0 ; float firwt[5]={0.09f,0.25f,0.32f,0.25f,0.09f} ;
   int   nmed =0 ;
   int   nwt  =0 ;

#define METHOD_C  3
#define METHOD_K 11
   int   method = METHOD_C ;

   /**--- help the pitiful user? ---**/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dInvFMRI [options]\n"
      "Program to compute stimulus time series, given a 3D+time dataset\n"
      "and an activation map (the inverse of the usual FMRI analysis problem).\n"
      "-------------------------------------------------------------------\n"
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
      " -mapwt www = Defines a weighting factor to use for each element of\n"
      "                the map.  The dataset 'www' can have either 1 sub-brick,\n"
      "                or the same number as in the -map dataset.  In the\n"
      "                first case, in each voxel, each sub-brick of the map\n"
      "                gets the same weight in the least squares equations.\n"
      "                  [default: all weights are 1]\n"
      "\n"
      " -mask mmm  = Defines a mask dataset, to restrict input voxels from\n"
      "                -data and -map.  [default: all voxels are used]\n"
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
      "                [default = '-', which is stdout; probably not good]\n"
      "\n"
      " -method M  = Determines the method to use.  'M' is a single letter:\n"
      "               -method C = least squares fit to data matrix Y [default]\n"
      "               -method K = least squares fit to activation matrix A\n"
      "\n"
      " -alpha aa  = Set the 'alpha' factor to 'aa'; alpha is used to penalize\n"
      "                large values of the output vectors.  Default is 0.\n"
      "                A large-ish value for alpha would be 0.1.\n"
      "\n"
      " -fir5     = Smooth the results with a 5 point lowpass FIR filter.\n"
      " -median5  = Smooth the results with a 5 point median filter.\n"
      "               [default: no smoothing; only 1 of these can be used]\n"
      "-------------------------------------------------------------------\n"
      "METHODS:\n"
      " Formulate the problem as\n"
      "    Y = V A' + F C' + errors\n"
      " where Y = data matrix      (N x M) [from -data]\n"
      "       V = stimulus         (N x p) [to -out]\n"
      "       A = map matrix       (M x p) [from -map]\n"
      "       F = baseline matrix  (N x q) [from -base and -polort]\n"
      "       C = baseline weights (M x q) [not computed]\n"
      "       N = time series length = length of -data file\n"
      "       M = number of voxels in mask\n"
      "       p = number of stimulus time series to estimate\n"
      "         = number of parameters in -map file\n"
      "       q = number of baseline parameters\n"
      "   and ' = matrix transpose operator\n"
      " Next, define matrix Z (Y detrended relative to columns of F) by\n"
      "                       -1\n"
      "   Z = [I - F(F'F)  F']  Y\n"
      "-------------------------------------------------------------------\n"
      " The method C solution is given by\n"
      "                 -1\n"
      "   V0 = Z A [A'A]\n"
      "\n"
      " This solution minimizes the sum of squares over the N*M elements\n"
      " of the matrix   Y - V A' + F C'   (N.B.: A' means A-transpose).\n"
      "-------------------------------------------------------------------\n"
      " The method K solution is given by\n"
      "             -1                            -1\n"
      "   W = [Z Z']  Z A   and then   V = W [W'W]\n"
      "\n"
      " This solution minimizes the sum of squares of the difference between\n"
      " the A(V) predicted from V and the input A, where A(V) is given by\n"
      "                    -1\n"
      "   A(V) = Z' V [V'V]   = Z'W\n"
      "-------------------------------------------------------------------\n"
      " Technically, the solution is unidentfiable up to an arbitrary\n"
      " multiple of the columns of F (i.e., V = V0 + F G, where G is\n"
      " an arbitrary q x p matrix); the solution above is the solution\n"
      " that is orthogonal to the columns of F.\n"
      "\n"
      "-- RWCox - March 2006 - purely for experimental purposes!\n"
     ) ;

     printf("\n"
     "===================== EXAMPLE USAGE =====================================\n"
     "** Step 1: From a training dataset, generate activation map.\n"
     "  The input dataset has 4 runs, each 108 time points long.  3dDeconvolve\n"
     "  is used on the first 3 runs (time points 0..323) to generate the\n"
     "  activation map.  There are two visual stimuli (Complex and Simple).\n"
     "\n"
     "  3dDeconvolve -x1D xout_short_two.1D -input rall_vr+orig'[0..323]'   \\\n"
     "      -num_stimts 2                                                   \\\n"
     "      -stim_file 1 hrf_complex.1D               -stim_label 1 Complex \\\n"
     "      -stim_file 2 hrf_simple.1D                -stim_label 2 Simple  \\\n"
     "      -concat '1D:0,108,216'                                          \\\n"
     "      -full_first -fout -tout                                         \\\n"
     "      -bucket func_ht2_short_two -cbucket cbuc_ht2_short_two\n"
     "\n"
     "  N.B.: You may want to de-spike, smooth, and register the 3D+time\n"
     "        dataset prior to the analysis (as usual).  These steps are not\n"
     "        shown here -- I'm presuming you know how to use AFNI already.\n"
     "\n"
     "** Step 2: Create a mask of highly activated voxels.\n"
     "  The F statistic threshold is set to 30, corresponding to a voxel-wise\n"
     "  p = 1e-12 = very significant.  The mask is also lightly clustered, and\n"
     "  restricted to brain voxels.\n"
     "\n"
     "  3dAutomask -prefix Amask rall_vr+orig\n"
     "  3dcalc -a 'func_ht2_short+orig[0]' -b Amask+orig -datum byte \\\n"
     "         -nscale -expr 'step(a-30)*b' -prefix STmask300\n"
     "  3dmerge -dxyz=1 -1clust 1.1 5 -prefix STmask300c STmask300+orig\n"
     "\n"
     "** Step 3: Run 3dInvFMRI to estimate the stimulus functions in run #4.\n"
     "  Run #4 is time points 324..431 of the 3D+time dataset (the -data\n"
     "  input below).  The -map input is the beta weights extracted from\n"
     "  the -cbucket output of 3dDeconvolve.\n"
     "\n"
     "  3dInvFMRI -mask STmask300c+orig                       \\\n"
     "            -data rall_vr+orig'[324..431]'              \\\n"
     "            -map cbuc_ht2_short_two+orig'[6..7]'        \\\n"
     "            -polort 1 -alpha 0.01 -median5 -method K    \\\n"
     "            -out ii300K_short_two.1D\n"
     "\n"
     "  3dInvFMRI -mask STmask300c+orig                       \\\n"
     "            -data rall_vr+orig'[324..431]'              \\\n"
     "            -map cbuc_ht2_short_two+orig'[6..7]'        \\\n"
     "            -polort 1 -alpha 0.01 -median5 -method C    \\\n"
     "            -out ii300C_short_two.1D\n"
     "\n"
     "** Step 4: Plot the results, and get confused.\n"
     "\n"
     "  1dplot -ynames VV KK CC -xlabel Run#4 -ylabel ComplexStim \\\n"
     "         hrf_complex.1D'{324..432}'                         \\\n"
     "         ii300K_short_two.1D'[0]'                           \\\n"
     "         ii300C_short_two.1D'[0]'\n"
     "\n"
     "  1dplot -ynames VV KK CC -xlabel Run#4 -ylabel SimpleStim \\\n"
     "         hrf_simple.1D'{324..432}'                         \\\n"
     "         ii300K_short_two.1D'[1]'                          \\\n"
     "         ii300C_short_two.1D'[1]'\n"
     "\n"
     "  N.B.: I've found that method K works better if MORE voxels are\n"
     "        included in the mask (lower threshold) and method C if\n"
     "        FEWER voxels are included.  The above threshold gave 945\n"
     "        voxels being used to determine the 2 output time series.\n"
     "=========================================================================\n"
     ) ;

     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /**--- bureaucracy ---**/

   mainENTRY("3dInvFMRI main"); machdep();
   PRINT_VERSION("3dInvFMRI"); AUTHOR("Zhark");
   AFNI_logger("3dInvFMRI",argc,argv) ;

   /**--- scan command line ---**/

   iarg = 1 ;
   while( iarg < argc ){

     if( strcmp(argv[iarg],"-method") == 0 ){
       switch( argv[++iarg][0] ){
         default:
           WARNING_message("Ignoring illegal -method '%s'",argv[iarg]) ;
         break ;
         case 'C': method = METHOD_C ; break ;
         case 'K': method = METHOD_K ; break ;
       }
       iarg++ ; continue ;
     }

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
       CHECK_OPEN_ERROR(yset,argv[iarg]) ;
       nt = DSET_NVALS(yset) ;
       if( nt < 2 ) ERROR_exit("Only 1 sub-brick in dataset %s",argv[iarg]) ;
       nxyz = DSET_NVOX(yset) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-map") == 0 ){
       if( aset != NULL ) ERROR_exit("Can't input 2 -map datasets") ;
       aset = THD_open_dataset(argv[++iarg]) ;
       CHECK_OPEN_ERROR(aset,argv[iarg]) ;
       nparam = DSET_NVALS(aset) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mapwt") == 0 ){
       if( wset != NULL ) ERROR_exit("Can't input 2 -mapwt datasets") ;
       wset = THD_open_dataset(argv[++iarg]) ;
       CHECK_OPEN_ERROR(wset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-mask") == 0 ){
       if( mset != NULL ) ERROR_exit("Can't input 2 -mask datasets") ;
       mset = THD_open_dataset(argv[++iarg]) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-polort") == 0 ){
       char *cpt ;
       polort = (int)strtod(argv[++iarg],&cpt) ;
       if( *cpt != '\0' ) WARNING_message("Illegal non-numeric value after -polort") ;
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

   INFO_message("Loading dataset for Y") ;
   DSET_load(yset); CHECK_LOAD_ERROR(yset) ;
   INFO_message("Loading dataset for A") ;
   DSET_load(aset); CHECK_LOAD_ERROR(aset) ;

   if( wset != NULL ){
     if( DSET_NVOX(wset) != nxyz )
       ERROR_exit("Grid mismatch between -data and -mapwt") ;
     nwt = DSET_NVALS(wset) ;
     if( nwt > 1 && nwt != nparam )
       ERROR_exit("Wrong number of values=%d in -mapwt; should be 1 or %d",
                  nwt , nparam ) ;
     INFO_message("Loading dataset for mapwt") ;
     DSET_load(wset); CHECK_LOAD_ERROR(wset) ;
   }

   if( mset != NULL ){
     if( DSET_NVOX(mset) != nxyz )
       ERROR_exit("Grid mismatch between -data and -mask") ;
     INFO_message("Loading dataset for mask") ;
     DSET_load(mset); CHECK_LOAD_ERROR(mset) ;
     mask  = THD_makemask( mset , 0 , 1.0f,-1.0f ); DSET_delete(mset);
     nmask = THD_countmask( nxyz , mask ) ;
     if( nmask < 3 ){
       WARNING_message("Mask has %d voxels -- ignoring!",nmask) ;
       free(mask) ; mask = NULL ; nmask = 0 ;
     }
   }

   nvox = (nmask > 0) ? nmask : nxyz ;
   INFO_message("N = time series length  = %d",nt    ) ;
   INFO_message("M = number of voxels    = %d",nvox  ) ;
   INFO_message("p = number of params    = %d",nparam) ;

   /**--- set up baseline funcs in one array ---*/

   nqbase = (polort >= 0 ) ? polort+1 : 0 ;
   if( fimar != NULL ){
     for( kk=0 ; kk < IMARR_COUNT(fimar) ; kk++ ){
       qim = IMARR_SUBIMAGE(fimar,kk) ;
       if( qim != NULL && qim->nx != nt )
         WARNING_message("-base #%d length=%d; data length=%d",kk+1,qim->nx,nt) ;
       nqbase += qim->ny ;
     }
   }

   INFO_message("q = number of baselines = %d",nqbase) ;

#undef  F
#define F(i,j) flar[(i)+(j)*nt]   /* nt X nqbase */
   if( nqbase > 0 ){
     fim  = mri_new( nt , nqbase , MRI_float ) ;   /* F matrix */
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

     /*      -1          */
     /* (F'F)  F' matrix */

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

   /**--- set up map image into aim/aar = A matrix ---**/

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

   /**--- set up map weight into wim/war ---**/

#undef  WT
#define WT(i,j) war[(i)+(j)*nvox]   /* nvox X nparam */

   if( wset != NULL ){
     int numneg=0 , numpos=0 ;
     float fac ;

     INFO_message("Loading map weight matrix") ;
     wim = mri_new( nvox , nwt , MRI_float ) ; war = MRI_FLOAT_PTR(wim) ;
     for( jj=0 ; jj < nwt ; jj++ ){
       for( ii=kk=0 ; ii < nxyz ; ii++ ){
         if( GOOD(ii) ){
           WT(kk,jj) = THD_get_voxel(wset,ii,jj);
                if( WT(kk,jj) > 0.0f ){ numpos++; WT(kk,jj) = sqrt(WT(kk,jj)); }
           else if( WT(kk,jj) < 0.0f ){ numneg++; WT(kk,jj) = 0.0f;            }
           kk++;
         }
     }}
     DSET_unload(wset) ;
     if( numpos <= nparam )
       WARNING_message("Only %d positive weights found in -wtmap!",numpos) ;
     if( numneg > 0 )
       WARNING_message("%d negative weights found in -wtmap!",numneg) ;

     for( jj=0 ; jj < nwt ; jj++ ){
       fac = 0.0f ;
       for( kk=0 ; kk < nvox ; kk++ ) if( WT(kk,jj) > fac ) fac = WT(kk,jj) ;
       if( fac > 0.0f ){
         fac = 1.0f / fac ;
         for( kk=0 ; kk < nvox ; kk++ ) WT(kk,jj) *= fac ;
       }
     }
   }

   /**--- set up data image into yim/yar = Y matrix ---**/

#undef  Y
#define Y(i,j) yar[(i)+(j)*nt]   /* nt X nvox */

   INFO_message("Loading data matrix Y") ;
   yim = mri_new( nt , nvox , MRI_float ); yar = MRI_FLOAT_PTR(yim);
   for( ii=0 ; ii < nt ; ii++ ){
     for( jj=kk=0 ; jj < nxyz ; jj++ ){
       if( GOOD(jj) ){ Y(ii,kk) = THD_get_voxel(yset,jj,ii); kk++; }
   }}
   DSET_unload(yset) ;

   /**--- project baseline out of data image = Z matrix ---**/

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

   /***** At this point:
             matrix A is in aim,
             matrix Z is in yim.
          Solve for V into vim, using the chosen method *****/

   switch( method ){
     default: ERROR_exit("Illegal method code!  WTF?") ; /* Huh? */

     /*.....................................................................*/
     case METHOD_C:
       /**--- compute pseudo-inverse of A map ---**/

       INFO_message("Method C: Computing pseudo-inverse of A") ;
       if( wim != NULL ) WARNING_message("Ignoring -mapwt dataset") ;
       pfim = mri_matrix_psinv(aim,NULL,alpha) ;  /* nparam X nvox */
       if( pfim == NULL ) ERROR_exit("mri_matrix_psinv() fails") ;
       mri_free(aim) ;

       /**--- and apply to data to get results ---*/

       INFO_message("Computing result V") ;
       vim = mri_matrix_multranB( yim , pfim ) ; /* nt x nparam */
       mri_free(pfim) ; mri_free(yim) ;
     break ;

     /*.....................................................................*/
     case METHOD_K:
       /**--- compute pseudo-inverse of transposed Z ---*/

       INFO_message("Method K: Computing pseudo-inverse of Z'") ;
       if( nwt > 1 ){
         WARNING_message("Ignoring -mapwt dataset: more than 1 sub-brick") ;
         nwt = 0 ; mri_free(wim) ; wim = NULL ; war = NULL ;
       }

       if( nwt == 1 ){
         float fac ;
         for( kk=0 ; kk < nvox ; kk++ ){
           fac = war[kk] ;
           for( ii=0 ; ii < nt     ; ii++ ) Y(ii,kk) *= fac ;
           for( ii=0 ; ii < nparam ; ii++ ) A(kk,ii) *= fac ;
         }
       }

       tim  = mri_matrix_transpose(yim)        ; mri_free(yim) ;
       pfim = mri_matrix_psinv(tim,NULL,alpha) ; mri_free(tim) ;
       if( pfim == NULL ) ERROR_exit("mri_matrix_psinv() fails") ;

       INFO_message("Computing W") ;
       tim = mri_matrix_mult( pfim , aim ) ;
       mri_free(aim) ; mri_free(pfim) ;

       INFO_message("Computing result V") ;
       pfim = mri_matrix_psinv(tim,NULL,0.0f) ; mri_free(tim) ;
       vim  = mri_matrix_transpose(pfim)      ; mri_free(pfim);
     break ;

   } /* end of switch on method */

   if( wim != NULL ) mri_free(wim) ;

   /**--- smooth? ---**/

   if( nfir > 0 && vim->nx > nfir ){
     INFO_message("FIR-5-ing result") ;
     var = MRI_FLOAT_PTR(vim) ;
     for( jj=0 ; jj < vim->ny ; jj++ )
       linear_filter_reflect( nfir,firwt , vim->nx , var + (jj*vim->nx) ) ;
   }

   if( nmed > 0 && vim->nx > nmed ){
     INFO_message("Median-5-ing result") ;
     var = MRI_FLOAT_PTR(vim) ;
     for( jj=0 ; jj < vim->ny ; jj++ )
       median5_filter_reflect( vim->nx , var + (jj*vim->nx) ) ;
   }

   /**--- write results ---**/

   INFO_message("Writing result to '%s'",fname_out) ;
   mri_write_1D( fname_out , vim ) ;
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
