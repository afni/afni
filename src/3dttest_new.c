#include "mrilib.h"

/*--- prototypes ---*/

#undef  MAXCOV
#define MAXCOV 31

void regress_toz( int numA , float *zA ,
                  int numB , float *zB , int opcode ,
                  int mcov ,
                  float *xA , float *psinvA , float *xtxinvA ,
                  float *xB , float *psinvB , float *xtxinvB ,
                  float *outvec , float *workspace             ) ;

static int mcov = 0 ;
static int nout = 0 ;
static float *axx , *axx_psinv , *axx_xtxinv ;
static float *bxx , *bxx_psinv , *bxx_xtxinv ;

int main( int argc , char *argv[] )
{

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Gosset (Student) t-test sets of 3D datasets\n"
      "\n"
      "Usage is similar to 3dMEMA.  For example:\n"
      "\n"
      " 3dttest_new -set Happy            \\\n"
      "                sub001 a+tlrc'[3]' \\\n"
      "                sub002 b+tlrc'[3]' \\\n"
      "                ...                \\\n"
      "             -covariates Cfile\n"
      "\n"
      "You can input 1 or 2 sets of data.  With 1 set, the mean across\n"
      "input datasets (usually subjects) is tested against 0 (by default).\n"
      "With 2 sets, the difference in means across each set is tested\n"
      "against 0.\n"
      "\n"
      "Covariates can be per-dataset (input=1 number) or per-voxel/per-dataset\n"
      "(input=1 dataset).  Note that the second option will slow the program\n"
      "down considerably.\n"
      "\n"
      "This program is meant (for most users) to replace the original 3dttest,\n"
      "which was written in 1994 \"when grass was green and grain was yellow\".\n"
      "\n"
      "\n"
      "OPTIONS:\n"
      "--------\n"
      " -set SETNAME             \\\n"
      "         SAMP_1 BETA_DSET \\\n"
      "         SAMP_2 BETA_DSET \\\n"
      "         ...    ...       \\\n"
      "         SAMP_N BETA_DSET\n"
      "   Specify the data for one of the test variables.\n"
      "   SETNAME is the name assigned to the set (used in the output labels).\n"
      "   SAMP_K  is the label for the sample K whose input dataset follows.\n"
      "   DSET    is the name of the dataset of the beta coefficient or GLT.\n"
      "* One or two '-set' options can be given.  The difference between this\n"
      "   option and the version in 3dMEMA is only that you do not give a second\n"
      "   dataset ('T_DSET') with each sample in this program.\n"
      "\n"
      " -covariates COVAR_FILE\n"
      "* Specify the name of a text file containing a table for the covariate(s).\n"
      "   Each column in the file is treated as a separate covariate, and each\n"
      "   row contains the values of these covariates for each sample. Note that\n"
      "   you can use '-covariates' only once -- the COVAR_FILE should contain\n"
      "   the covariates for all input samples from both sets.\n"
      "* The format of COVAR_FILE is like that for 3dMEMA and for 3dGroupInCorr:\n"
      "     FIRST LINE -->   subject IQ   age  GMfrac\n"
      "     LATER LINES -->  Elvis   143   42  Elvis_GM+tlrc\n"
      "                      Fred     85   59  Fred_GM+tlrc\n"
      "                      Ethel   109   49  Ethel_GM+tlrc\n"
      "                      Lucy    133   32  Lucy_GM+tlrc\n"
      "  ++ The first column contains the labels that must match the dataset\n"
      "      SAMP_K labels given in the '-set' option(s).\n"
      "  ++ The later columns contain numbers (as in IQ and age, above), or\n"
      "      dataset names.  In the latter case, you are specifying a voxel-wise\n"
      "      covariate (e.g., GMfrac).\n"
      "  ++ The first line contains column headers.  The header label for the\n"
      "      first column isn't used for anything.  The later header labels are\n"
      "      used in the sub-brick labels sent to AFNI.\n"
      "  ++ At this time, only the -paired and -pooled options can be used with\n"
      "      covariates.  If you use -unpooled, it will be changed to -pooled.\n"
      "      -unpooled still works with a pure t-test (no -covariates option).\n"
      "  ++ If you use -paired, then the covariates for -setB will be the same\n"
      "      as those for -setA, even if the dataset labels are different!\n"
      "  ++ Each covariate column in the regression matrix will have its mean\n"
      "      removed (centered). If there are 2 sets of subjects, each set's\n"
      "      matrix will be centered separately.\n"
      "  ++ For each covariate, 2 sub-bricks are produced:\n"
      "      -- The estimated slope of the beta values vs covariate\n"
      "      -- The t-statistic of this slope\n"
      "  ++ If there are 2 sets of subjects, then each pair of sub-bricks is\n"
      "      produced for the setA-setB, setA, and setB cases, so that you'll\n"
      "      get 6 sub-bricks per covariate (plus 6 more for the mean, which\n"
      "      is treated as a special covariate whose values are all 1).\n"
      "  ++ A maximum of 31 covariates are allowed.  If you have more, then\n"
      "      seriously consider the possibility that you are completely deranged.\n"
      "\n"
      " -paired   = Specifies the use of a paired-sample t-test to\n"
      "              compare set1 and set2.  If this option is used,\n"
      "              set1 and set2 must have the same cardinality (duh).\n"
      "\n"
      " -unpooled = Specifies that the variance estimates for set1 and\n"
      "              set2 be computed separately (not pooled together).\n"
      "              This only makes sense if -paired is NOT given.\n"
      "\n"
      " -prefix p = Gives the name of the output dataset file.\n"
    ) ;
   }


     /*--- setup the setA regression matrix ---*/

     nA    = shd_AAA->ndset ;
     axxim = mri_new( nA , mcov+1 , MRI_float ) ;
     axx   = MRI_FLOAT_PTR(axxim) ;
     for( kk=0 ; kk < shd_AAA->ndset ; kk++ ){  /* loop over datasets */
       ii = string_search( shd_AAA->dslab[kk] , /* find which covariate */
                           covnel->vec_len , (char **)covnel->vec[0] ) ;
       if( ii < 0 ){
         ERROR_message("Can't find dataset label '%s' in covariates file" ,
                       shd_AAA->dslab[kk] ) ;
         nbad++ ;
       } else {             /* ii-th row of covariates == kk-th dataset */
         AXX(kk,0) = 1.0f ;
         for( jj=1 ; jj <= mcov ; jj++ )
           AXX(kk,jj) = ((float *)covnel->vec[jj])[ii] ;
       }
     }
     if( nbad == 0 ){  /* process the matrix */
       MRI_IMARR *impr ; float sum ;
       for( jj=1 ; jj <= mcov ; jj++ ){  /* demean the columns */
         for( sum=0.0f,kk=0 ; kk < shd_AAA->ndset ; kk++ ) sum += AXX(kk,jj) ;
         sum /= shd_AAA->ndset ;
         for( kk=0 ; kk < shd_AAA->ndset ; kk++ ) AXX(kk,jj) -= sum ;
       }
       /* Compute inv[X'X] and the pseudo-inverse inv[X'X]X' for this matrix */
       impr = mri_matrix_psinv_pair( axxim , 0.0f ) ;
       if( impr == NULL ) ERROR_exit("Can't process setA covariate matrix?! :-(") ;
       axxim_psinv  = IMARR_SUBIM(impr,0) ; axx_psinv  = MRI_FLOAT_PTR(axxim_psinv ) ;
       axxim_xtxinv = IMARR_SUBIM(impr,1) ; axx_xtxinv = MRI_FLOAT_PTR(axxim_xtxinv) ;
     }

     /*--- setup the setB regression matrix ---*/

     if( shd_BBB != NULL && ttest_opcode != 2 ){  /* un-paired case */
       nB    = shd_BBB->ndset ;
       bxxim = mri_new( nB , mcov+1 , MRI_float ) ;
       bxx   = MRI_FLOAT_PTR(bxxim) ;
       for( kk=0 ; kk < shd_BBB->ndset ; kk++ ){  /* loop over datasets */
         ii = string_search( shd_BBB->dslab[kk] , /* find which covariate */
                             covnel->vec_len , (char **)covnel->vec[0] ) ;
         if( ii < 0 ){
           ERROR_message("Can't find dataset label '%s' in covariates file" ,
                         shd_BBB->dslab[kk] ) ;
           nbad++ ;
         } else {             /* ii-th row of covariates == kk-th dataset */
           BXX(kk,0) = 1.0f ;
           for( jj=1 ; jj <= mcov ; jj++ )
             BXX(kk,jj) = ((float *)covnel->vec[jj])[ii] ;
         }
       }
       if( nbad == 0 ){  /* process the matrix */
         MRI_IMARR *impr ; float sum ;
         for( jj=1 ; jj <= mcov ; jj++ ){  /* demean the columns */
           for( sum=0.0f,kk=0 ; kk < shd_BBB->ndset ; kk++ ) sum += BXX(kk,jj) ;
           sum /= shd_BBB->ndset ;
           for( kk=0 ; kk < shd_BBB->ndset ; kk++ ) BXX(kk,jj) -= sum ;
         }
         /* Compute inv[X'X] and the pseudo-inverse inv[X'X]X' for this matrix */
         impr = mri_matrix_psinv_pair( bxxim , 0.0f ) ;
         if( impr == NULL ) ERROR_exit("Can't process setB covariate matrix?! :-(") ;
         bxxim_psinv  = IMARR_SUBIM(impr,0) ; bxx_psinv  = MRI_FLOAT_PTR(bxxim_psinv ) ;
         bxxim_xtxinv = IMARR_SUBIM(impr,1) ; bxx_xtxinv = MRI_FLOAT_PTR(bxxim_xtxinv) ;
       }

     } else if( shd_BBB != NULL && ttest_opcode == 2 ){  /* paired case */

       bxx = axx ; bxx_psinv = axx_psinv ; bxx_xtxinv = axx_xtxinv ;

     }

     if( nbad )
       ERROR_exit("Can't continue past the above covariates errors :-((") ;

/*---------------------------------------------------------------------------*/

#undef  PA
#undef  PB
#undef  XA
#undef  XB
#define PA(i,j) psinvA[(i)+(j)*mm]  /* i=0..mm-1 , j=0..numA-1 */
#define PB(i,j) psinvB[(i)+(j)*mm]
#define XA(i,j) xA[(i)+(j)*(nA)]    /* i=0..nA-1 , j=0..mm-1 */
#define XB(i,j) xB[(i)+(j)*(nB)]

#undef  xtxA
#undef  xtxB
#define xtxA(i) xtxinvA[(i)+(i)*mm] /* diagonal elements */
#define xtxB(i) xtxinvB[(i)+(i)*mm]

#undef  VBIG
#define VBIG 1.0e+24f

/*---------------------------------------------------------------------------*/
/*  opcode defines what to do for 2-sample tests:
      0 ==> unpaired, pooled variance
      1 ==> unpaired, unpooled variance (not yet implemented)
      2 ==> paired (numA==numB required)

    xA      = numA X (mcov+1) matrix -- in column-major order
    psinvA  = (mcov+1) X numA matrix -- in column-major order
    xtxinvA = (mcov+1) X (mcov+1) matrix = inv[xA'xA]
*//*-------------------------------------------------------------------------*/

#if defined(COVTEST) && 0
static int first=1 ;
#endif

void regress_toz( int numA , float *zA ,
                  int numB , float *zB , int opcode ,
                  int mcov ,
                  float *xA , float *psinvA , float *xtxinvA ,
                  float *xB , float *psinvB , float *xtxinvB ,
                  float *outvec , float *workspace             )
{
   int kt=0,nws , mm=mcov+1 , nA=numA , nB=numB ;
   float *betA=NULL , *betB=NULL , *zdifA=NULL , *zdifB=NULL ;
   float ssqA=0.0f , ssqB=0.0f , varA=0.0f , varB=0.0f ; double dof=0.0 ;
   register float val ; register int ii,jj,tt ;

   MRI_IMAGE *bxxim_psinv=NULL , *bxxim_xtxinv=NULL ;

   nws = 0 ;
   if( testA || testAB ){
     betA  = workspace + nws ; nws += mm ;
     zdifA = workspace + nws ; nws += nA ;
   }
   if( testB || testAB ){
     betB  = workspace + nws ; nws += mm ;
     zdifB = workspace + nws ; nws += nB ;
   }

   /*-- compute estimates for A parameters --*/

   if( testA || testAB ){
     MRI_IMAGE *axxim_psinv=NULL , *axxim_xtxinv=NULL ;
     if( psinvA == NULL ){
       MRI_IMARR *impr ; MRI_IMAGE *axxim ;
       axxim = mri_new_vol_empty( nA , mcov+1 , 1 , MRI_float ) ;
       mri_fix_data_pointer(xA,axxim) ;
       impr = mri_matrix_psinv_pair(axxim,0.0f) ;
       mri_clear_data_pointer(axxim) ; mri_free(axxim) ;
       if( impr == NULL ){ ERROR_message("psinv setA matrix fails"); return; }
       axxim_psinv  = IMARR_SUBIM(impr,0) ; psinvA  = MRI_FLOAT_PTR(axxim_psinv ) ;
       axxim_xtxinv = IMARR_SUBIM(impr,1) ; xtxinvA = MRI_FLOAT_PTR(axxim_xtxinv) ;
       FREE_IMARR(impr) ;
     }
     for( ii=0 ; ii < mm ; ii++ ){
       for( val=0.0f,jj=0 ; jj < nA ; jj++ ) val += PA(ii,jj)*zA[jj] ;
       betA[ii] = val ;
     }
     for( jj=0 ; jj < nA ; jj++ ){
       val = -zA[jj] ;
       for( ii=0 ; ii < mm ; ii++ ) val += XA(jj,ii)*betA[ii] ;
       zdifA[ii] = val ; ssqA += val*val ;
     }
     if( testA ){ varA = ssqA / (nA-mm) ; if( varA <= 0.0f ) varA = VBIG ; }
     mri_free(axxim_psinv) ; mri_free(axxim_xtxinv) ;
   }

   /*-- compute estimates for B parameters --*/

   if( testB || testAB ){
     MRI_IMAGE *bxxim_psinv=NULL , *bxxim_xtxinv=NULL ;
     if( psinvB == NULL ){
       MRI_IMARR *impr ; MRI_IMAGE *bxxim ;
       bxxim = mri_new_vol_empty( nB , mcov+1 , 1 , MRI_float ) ;
       mri_fix_data_pointer(xB,bxxim) ;
       impr = mri_matrix_psinv_pair(bxxim,0.0f) ;
       mri_clear_data_pointer(bxxim) ; mri_free(bxxim) ;
       if( impr == NULL ){ ERROR_message("psinv setB matrix fails"); return; }
       bxxim_psinv  = IMARR_SUBIM(impr,0) ; psinvB  = MRI_FLOAT_PTR(bxxim_psinv ) ;
       bxxim_xtxinv = IMARR_SUBIM(impr,1) ; xtxinvB = MRI_FLOAT_PTR(bxxim_xtxinv) ;
       FREE_IMARR(impr) ;
     }
     for( ii=0 ; ii < mm ; ii++ ){
       for( val=0.0f,jj=0 ; jj < nB ; jj++ ) val += PB(ii,jj)*zB[jj] ;
       betB[ii] = val ;
     }
     for( jj=0 ; jj < nB ; jj++ ){
       val = -zB[jj] ;
       for( ii=0 ; ii < mm ; ii++ ) val += XB(jj,ii)*betB[ii] ;
       zdifB[ii] = val ; ssqB += val*val ;
     }
     if( testB ){ varB = ssqB / (nB-mm) ; if( varB <= 0.0f ) varB = VBIG ; }
     mri_free(bxxim_psinv) ; mri_free(bxxim_xtxinv) ;
   }

   /*-- carry out 2-sample (A-B) tests, if any --*/

   if( testAB ){
     float varAB ;

     if( opcode == 2 ){  /* paired (nA==nB, xA==xB, etc.) */

       for( varAB=0.0f,ii=0 ; ii < nA ; ii++ ){
         val = zdifA[ii] - zdifB[ii] ; varAB += val*val ;
       }
       varAB /= (nA-mm) ; if( varAB <= 0.0f ) varAB = VBIG ;

       dof = nA - mm ;
       for( tt=0 ; tt < mm ; tt++ ){
         if( (testAB & (1 << tt)) == 0 ) continue ;  /* bitwase AND */
         outvec[kt++] = betA[tt] - betB[tt] ;
         val          = outvec[kt-1] / sqrtf( varAB*xtxA(tt) ) ;
         outvec[kt++] = (float)GIC_student_t2z( (double)val , dof ) ;
       }

     } else {            /* unpaired, pooled variance */

       varAB = (ssqA+ssqB)/(nA+nB-2*mm) ; if( varAB <= 0.0f ) varAB = VBIG ;

       dof = nA + nB - 2*mm ;
       for( tt=0 ; tt < mm ; tt++ ){
         if( (testAB & (1 << tt)) == 0 ) continue ;  /* bitwase AND */
         outvec[kt++] = betA[tt] - betB[tt] ;
         val          = outvec[kt-1] / sqrtf( varAB*(xtxA(tt)+xtxB(tt)) );
         outvec[kt++] = (float)GIC_student_t2z( (double)val , dof ) ;
       }
     } /* end of unpaired pooled variance */
   }

   /*-- carry out 1-sample A tests, if any --*/

   if( testA ){
#if defined(COVTEST) && 0
#pragma omp critical
     { if( first ){
         first = 0 ;
         fprintf(stderr,"testA varA=%g xtxA=",varA) ;
         for( tt=0 ; tt < mm ; tt++ ) fprintf(stderr," %g",xtxA(tt)) ;
         fprintf(stderr,"\n") ;
       }
     }
#endif
     dof = nA - mm ;
     for( tt=0 ; tt < mm ; tt++ ){
       if( (testA & (1 << tt)) == 0 ) continue ;  /* bitwise AND */
       outvec[kt++] = betA[tt] ;
       val          = betA[tt] / sqrtf( varA * xtxA(tt) ) ;
       outvec[kt++] = (float)GIC_student_t2z( (double)val , dof ) ;
     }
   }

   /*-- carry out 1-sample B tests, if any --*/

   if( testB ){
     dof = nB - mm ;
     for( tt=0 ; tt < mm ; tt++ ){
       if( (testB & (1 << tt)) == 0 ) continue ;  /* bitwise AND */
       outvec[kt++] = betB[tt] ;
       val          = betB[tt] / sqrtf( varB * xtxB(tt) ) ;
       outvec[kt++] = (float)GIC_student_t2z( (double)val , dof ) ;
     }
   }


   return ;
}

/*----------------------------------------------------------------------------*/
/*! Various sorts of t-tests; output = Z-score.
   - numx = number of points in the first sample (must be > 1)
   - xar  = array with first sample
   - numy = number of points in the second sample
             - numy = 0 ==> a 1 sample test of first sample against mean=0
  DISABLED   - numy = 1 ==> a 1 sample test of first sample against mean=yar[0]
             - numy > 1 ==> a 2 sample test; opcode determines what kind
   - opcode = 0 for unpaired test with pooled variance
   - opcode = 1 for unpaired test with unpooled variance
   - opcode = 2 for paired test (numx == numy is required)
   - The return value is the Z-score of the t-statistic.
*//*--------------------------------------------------------------------------*/

float_pair ttest_toz( int numx, float *xar, int numy, float *yar, int opcode )
{
   float_pair result = {0.0f,0.0f} ;
   register int ii ; register float val ;
   float avx,sdx , avy,sdy , dof , tstat=0.0f,delta=0.0f ;
   int paired=(opcode==2) , pooled=(opcode==0) ;

#if 0
   /* check inputs for stoopidities or other things that need to be changed */

   if( numx < 2 || xar == NULL                 ) return result ; /* bad */
   if( paired && (numy != numx || yar == NULL) ) return result ; /* bad */
#endif

   if( numy < 2 || yar == NULL ){ numy = paired = pooled = 0 ; yar = NULL ; }

   if( paired ){   /* Case 1: paired t test */

     avx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ) avx += xar[ii]-yar[ii] ;
     avx /= numx ; sdx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ){ val = xar[ii]-yar[ii]-avx; sdx += val*val; }
     if( sdx > 0.0f )      tstat = avx / sqrtf( sdx/((numx-1.0f)*numx) ) ;
     else if( avx > 0.0f ) tstat =  19.0f ;
     else if( avx < 0.0f ) tstat = -19.0f ;
     else                  tstat =   0.0f ;
     dof = numx-1.0f ; delta = avx ;  /* delta = diff in means */

   } else if( numy == 0 ){  /* Case 2: 1 sample test against mean==0 */

     avx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ) avx += xar[ii] ;
     avx /= numx ; sdx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ){ val = xar[ii]-avx ; sdx += val*val ; }
     if( sdx > 0.0f )      tstat = avx / sqrtf( sdx/((numx-1.0f)*numx) ) ;
     else if( avx > 0.0f ) tstat =  19.0f ;
     else if( avx < 0.0f ) tstat = -19.0f ;
     else                  tstat =   0.0f ;
     dof = numx-1.0f ; delta = avx ; /* delta = mean */

   } else {  /* Case 3: 2 sample test (pooled or unpooled) */

     avx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ) avx += xar[ii] ;
     avx /= numx ; sdx = 0.0f ;
     for( ii=0 ; ii < numx ; ii++ ){ val = xar[ii] - avx ; sdx += val*val ; }

     avy = 0.0f ;
     for( ii=0 ; ii < numy ; ii++ ) avy += yar[ii] ;
     avy /= numy ; sdy = 0.0f ;
     for( ii=0 ; ii < numy ; ii++ ){ val = yar[ii] - avy ; sdy += val*val ; }

     delta = avx - avy ; /* difference in means */

     if( sdx+sdy == 0.0f ){

            if( delta > 0.0f ) tstat =  19.0f ;
       else if( delta < 0.0f ) tstat = -19.0f ;
       else                    tstat =   0.0f ;
       dof = numx+numy-2.0f ;

     } else if( pooled ){  /* Case 3a: pooled variance estimate */

       sdx   = (sdx+sdy) / (numx+numy-2.0f) ;
       tstat = delta / sqrtf( sdx*(1.0f/numx+1.0f/numy) ) ;
       dof   = numx+numy-2.0f ;

     } else {       /* Case 3b: unpooled variance estimate */

       sdx  /= (numx-1.0f)*numx ; sdy /= (numy-1.0f)*numy ; val = sdx+sdy ;
       tstat = delta / sqrtf(val) ;
       dof   = (val*val) / (sdx*sdx/(numx-1.0f) + sdy*sdy/(numy-1.0f) ) ;

     }

   } /* end of all possible cases */

   result.a = delta ;
   result.b = (float)GIC_student_t2z( (double)tstat , (double)dof ) ;
   return result ;
}
