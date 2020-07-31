#include "mrilib.h"
#include "matrix.h"
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_permutation.h>
#include <gsl/gsl_types.h>
#include <gsl/gsl_permute.h>
#include "DoTrackit.h"
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_eigen.h>
#include <gsl/gsl_sort.h>
#include <gsl/gsl_linalg.h>
#include "diffusiony.h"


int Finalize_Uncert_Array( float **UU,
                           int *minds, //short *mskd,
                           int Ntodo,
                           int Nj
                           )
{
   int aa;
   int i,j;

   for(aa=0 ; aa<Ntodo ; aa++) {
      i = minds[aa];
      for(j=0 ; j<6 ; j+=2) {
         // -> make mean
         UU[j][i]/= Nj;
         // -> make std
         UU[j+1][i]-= Nj*UU[j][i]*UU[j][i];
         if(UU[j+1][i]>=0)
            UU[j+1][i] = sqrt(UU[j+1][i]/(Nj-1));
         else{
            if( (j+1) < 4)
               UU[j+1][i] = PIo2;
            else
               UU[j+1][i] = 1.;
         }
      }
   }
   
   return 0;
}



float Calc_FA_from_gsl_Evals( gsl_vector *E )
{
   int j;
   float MD=0., num=0.,den=0;

   MD = gsl_vector_get(E,0) + gsl_vector_get(E,1) + gsl_vector_get(E,2);
   MD/= 3.;
   for(j=0 ; j<3 ; j++) {
      num+= pow(gsl_vector_get(E,j) - MD,2);
      den+= pow(gsl_vector_get(E,j),2);
   }
   num/=den;
   num*=1.5;
   num = sqrt(num);
   return num;
}

int Calc_Eigs_Uncert( int v,
                      float **UU,
                      gsl_vector *dd,
                      gsl_matrix *testD,        // have to recreate
                      gsl_vector *Eval,
                      gsl_matrix *Evec,
                      THD_3dim_dataset **PARS,
                      THD_3dim_dataset **VECS
                      )
{
   int i,j;
   gsl_eigen_symmv_workspace *EigenW = gsl_eigen_symmv_alloc(3);
   float dFA;
   float dot11=0., dot12=0., dot13=0.;

   //INFO_message("calc eigs");

   // make sure eigenvalues are all positive
   for(j=0 ; j<3 ; j++) //diagonal elements
      gsl_matrix_set(testD,j,j,gsl_vector_get(dd,j));
   gsl_matrix_set(testD,0,1,gsl_vector_get(dd,3));
   gsl_matrix_set(testD,1,0,gsl_vector_get(dd,3));
   gsl_matrix_set(testD,0,2,gsl_vector_get(dd,4));
   gsl_matrix_set(testD,2,0,gsl_vector_get(dd,4));
   gsl_matrix_set(testD,1,2,gsl_vector_get(dd,5));
   gsl_matrix_set(testD,2,1,gsl_vector_get(dd,5));

   j = gsl_eigen_symmv(testD, Eval, Evec, EigenW);
   j = gsl_eigen_symmv_sort(Eval, Evec, GSL_EIGEN_SORT_VAL_DESC);

   //INFO_message("Eval");
   //Show_1DVector_Floats_gsl(Eval, 3);
   //INFO_message("Evec");
   //Show_2DMatrix_Floats_gsl(Evec, 3, 3);


   // FA stuff
   dFA = Calc_FA_from_gsl_Evals( Eval );
   //INFO_message("FA=%f",dFA);
   dFA-= THD_get_voxel(PARS[0], v, 0); //diff
   //INFO_message("dFA=%f",dFA);
   UU[4][v]+= dFA;
   UU[5][v]+= dFA*dFA;

   // to get angle of (E1_jk * e2_orig) and (E1_jk * e3_orig), also
   // need (E1_jk * e1_orig) for signage
   for(j=0 ; j<3 ; j++) {
      dot11+= THD_get_voxel(VECS[0],v,j)*gsl_matrix_get(Evec,j,0);
      dot12+= THD_get_voxel(VECS[1],v,j)*gsl_matrix_get(Evec,j,0);
      dot13+= THD_get_voxel(VECS[2],v,j)*gsl_matrix_get(Evec,j,0);
      //INFO_message("ele=%f .... %f",
      //             gsl_matrix_get(Evec,j,1),
      //             THD_get_voxel(VECS[1],v,j));
   }
   if(dot11<0) { // due to symmetery could have pos or neg vecs
                 // equivalently, but for ang diffs choose to compare
                 // 'parallel'ish ones
      dot12*= -1;
      dot13*= -1;
   }
   //INFO_message("(early) dot11=%f,  dot12=%f,  dot13=%f",
   //             dot11, dot12, dot13);
   dot12 = (float) (PI/2. - acos(dot12));
   dot13 = (float) (PI/2. - acos(dot13));
   //INFO_message("dot12=%f,  dot13=%f",dot12,dot13);
   UU[0][v]+= dot12;
   UU[1][v]+= dot12*dot12;
   UU[2][v]+= dot13;
   UU[3][v]+= dot13*dot13;

   // Free!
	gsl_eigen_symmv_free(EigenW);

   return 0;
}


int Calc_DTI_lin_tensor( gsl_vector *x,
                         gsl_vector *dd,
                         gsl_matrix *C,
                         gsl_matrix *testD,
                         gsl_vector *Eval,
                         gsl_eigen_symm_workspace *EigenV,
                         int *POSDEF)
{
   int i,ii,j;
   
   gsl_vector_set_zero(dd);
   gsl_blas_dgemv(CblasNoTrans, 1.0, C, x, 1.0, dd);
   
   //INFO_message("C:");
   //Show_2DMatrix_Floats_gsl(C,7,7);
   //INFO_message("x:");
   //Show_1DVector_Floats_gsl(x, 7);
   //INFO_message("solution: dd:");
   //Show_1DVector_Floats_gsl(dd, 7);

   // make sure eigenvalues are all positive
   for(j=0 ; j<3 ; j++) //diagonal elements
      gsl_matrix_set(testD,j,j,gsl_vector_get(dd,j));
   gsl_matrix_set(testD,0,1,gsl_vector_get(dd,3));
   gsl_matrix_set(testD,1,0,gsl_vector_get(dd,3));
   gsl_matrix_set(testD,0,2,gsl_vector_get(dd,4));
   gsl_matrix_set(testD,2,0,gsl_vector_get(dd,4));
   gsl_matrix_set(testD,1,2,gsl_vector_get(dd,5));
   gsl_matrix_set(testD,2,1,gsl_vector_get(dd,5));
   
   j = gsl_eigen_symm(testD, Eval, EigenV);

   if( (gsl_vector_get(Eval,0) <= MINEIG) 
       || (gsl_vector_get(Eval,1) <= MINEIG) 
       || (gsl_vector_get(Eval,2) <= MINEIG) ) {
      //INFO_message("BAD EIGENVALS!:  %f, %f, %f",
      //           gsl_vector_get(Eval,0),
      //           gsl_vector_get(Eval,1),
      //           gsl_vector_get(Eval,2) );
      POSDEF[0] = 0;
   }
   else{
      //INFO_message("OK EIGENVALS!:  %f, %f, %f",
      //           gsl_vector_get(Eval,0),
      //           gsl_vector_get(Eval,1),
      //           gsl_vector_get(Eval,2) );
      POSDEF[0] = 1;
   }


   return 0;
}




int Make_Uncert_Matrs_init( int v,
                            float **bseven,
                            THD_3dim_dataset *DWI,
                            int *SRInd,
                            float *Wei,
                            gsl_vector *x,
                            gsl_matrix *B,
                            gsl_matrix *BTW,
                            int Mj
                            )
{
   int i,ii,j;
   float val;

   for(ii=0 ; ii<Mj ; ii++) {
      // x
      val = THD_get_voxel(DWI, v, SRInd[ii]);
      //INFO_message("[%7d] val : %10f",SRInd[ii], val);
      if(val<=0)
         val = 0.01; // flag more badness here?
      else
         val = log(val);
      gsl_vector_set(x, ii, val);

      // B
      for(j=0 ; j<7 ; j++) {
         gsl_matrix_set(B, 
                        ii, j, 
                        bseven[SRInd[ii]][j]);
      // BTW
         gsl_matrix_set(BTW,
                        j, ii,
                        bseven[SRInd[ii]][j] * Wei[ii]);
      }
   }

   return 0;
}

int Make_Uncert_Matrs_final( gsl_matrix *B,
                             gsl_matrix *BTW,
                             gsl_matrix *BTWB,
                             gsl_matrix *BTWBinv,
                             gsl_matrix *C
                             )
{
   int i,j;
   gsl_permutation *P = gsl_permutation_alloc(7);

   gsl_matrix_set_zero(BTWB);
   gsl_matrix_set_zero(BTWBinv);

   // BTWB
   j = gsl_blas_dgemm(CblasNoTrans,CblasNoTrans,1.0,BTW,B,1.0,BTWB);

   //j = copy_gsl_singular (BTWB);
   //if(j) {
   //   WARNING_message("1 Singular matrix! Is a mask being used?");
   //   gsl_permutation_free(P);
   //      return -1;
   //}

   // BTWBinv: can't use BTWB for itself after this (calcs change it)
   j = gsl_linalg_LU_decomp(BTWB, P, &i);

   // have started finding errors returned for singular values -> GSL
   // change; use a copy of their own function in gsl_lu.c to
   // pre-check for singuarities
   j = copy_gsl_singular (BTWB);
   if(j) {
      // -> just leads to non-tensor fitting scenario.
      gsl_permutation_free(P);
      return -1;
   }

   j = gsl_linalg_LU_invert(BTWB, P, BTWBinv);

   // C = BTWBinv * BTW, what we need to mult x by to get sol
   gsl_matrix_set_zero(C);
   gsl_blas_dgemm(CblasNoTrans,CblasNoTrans,1.0,BTWBinv,BTW,1.0,C);

   // FREE
	gsl_permutation_free(P);

   return 0;
}


int copy_gsl_singular (const gsl_matrix * LU)
{
  size_t i, n = LU->size1;

  for (i = 0; i < n; i++)
    {
      double u = gsl_matrix_get (LU, i, i);
      if (u == 0) return 1;
    }
 
 return 0;
}


/*
  There are M total indices, of which we select Mj randomly.  We do
  this Nj times. A (Nj x Mj) will store the subset of indices.
 */
int Make_Jackknife_Inds_keep0th( int **A, 
                                 int M,
                                 int Mj,
                                 int Nj,
                                 int CHOOSE_SEED)
{
   int i,j;
	long seed1, seed2;
	const gsl_rng_type *T;
	gsl_rng *r;

   // for random number generation
   if( CHOOSE_SEED >= 0 )
      seed1 = seed2 = CHOOSE_SEED;
   else {
      seed1 = time(NULL);
      seed2 = time(NULL);
   }
   
	srand(seed1);
	gsl_rng_env_setup();
	T = gsl_rng_default;
	r = gsl_rng_alloc( T );
	gsl_rng_set( r, seed2 );
   
   // set up indices for M-1, because we will always include 0th DWI
   gsl_permutation *p = gsl_permutation_alloc(M-1); // alloc space
 
   for( i=0 ; i<Nj ; i++ ) {
      gsl_permutation_init (p);
      gsl_ran_shuffle (r, p->data, M-1, sizeof(size_t));
      for( j=0 ; j<(Mj-1) ; j++ ) 
         A[i][j+1] = gsl_permutation_get( p, j ) + 1;
   }

   gsl_permutation_free (p);
	gsl_rng_free(r);

   return 0;
}


int Basic_Grads_to_B7( float **bseven,
                        MRI_IMAGE *flim,
                        int Ng)
{
   int i,j,k;
   float *fptr=NULL;
   float grad[3] = {0.,0.,0.};

   fptr = MRI_FLOAT_PTR( flim );

   for( i=0 ; i<Ng ; i++ ) {
      for( j=0 ; j<3 ; j++ ) 
         grad[j] = *(fptr + 3*i + j);
      k = GradConv_BmatA_from_Bsign( bseven[i+1], grad );
      for( j=0 ; j<3 ; j++ )  // see Kingsley III, Eq.3
         bseven[i+1][j]*= -1;
      for( j=3 ; j<6 ; j++ ) 
         bseven[i+1][j]*= -2;
      bseven[i+1][6] = 1; 
   }

   for( j=0 ; j<6 ; j++ ) // check, prob unnecessary if not recycling.
      bseven[0][j] = 0;
   bseven[0][6] = 1;

   return 0;
}

int Basic_Bmats_to_B7( float **bseven,
                       MRI_IMAGE *flim,
                       int Nb)
{
   int i,j,k;
   float *fptr=NULL;

   fptr = MRI_FLOAT_PTR( flim );

   for( i=0 ; i<Nb ; i++ ) {
      for( j=0 ; j<6 ; j++ ) 
         bseven[i][j] = *(fptr + 6*i + j);

      for( j=0 ; j<3 ; j++ )  // see Kingsley III, Eq.3
         bseven[i][j]*= -1;
      for( j=3 ; j<6 ; j++ ) 
         bseven[i][j]*= -2;
      bseven[i][6] = 1; 
   }

   return 0;
}



// take a len=6 matr and an empt len =3 grad file, calc grads from matr
int GradConv_Gsign_from_GmatA( float *grad, float *matr )
{
   int i;
   int signum[3] = {1,1,1};

   if( (matr[0]<0) || (matr[1]<0) || (matr[2]<0) )
      ERROR_exit("Matrices don't appear to be correct format-- check again") ;

   // get signs for grads
   for( i=0 ; i<3 ; i++)
      if( matr[3+i] < 0 )
         signum[2-i] = -1; // if all neg, then same as having all pos because of symmetry still

   for( i=0 ; i<3 ; i++)
      if ( matr[i] >= 0 ) {
         grad[i] = (float) sqrt(matr[i]);
         grad[i]*= signum[i];
      }
      else {
         WARNING_message("matrices don't appear to be correct format-- check again") ;
         grad[i] = 0;
      }

   return 0;
}

// !! Assumes these are scaled to be unit magnitude!!
// simple conversion of grads to G-matr, diagonal form
int GradConv_GmatA_from_Gsign( float *matr, float *grad )
{
   int i;

   for( i=0 ; i<3 ; i++)
      matr[i] =  grad[i]*grad[i];
   matr[3] = grad[0]*grad[1];
   matr[4] = grad[0]*grad[2];
   matr[5] = grad[1]*grad[2];

   return 0;
}

// doesn't have to be unit mag-- scale appropriately
int GradConv_BmatA_from_Bsign( float *matr, float *grad )
{
   int i;
   float gscale=0.;;

   for( i=0 ; i<3 ; i++) {
      matr[i] =  grad[i]*grad[i];
      gscale+= matr[i];
   }
   matr[3] = grad[0]*grad[1];
   matr[4] = grad[0]*grad[2];
   matr[5] = grad[1]*grad[2];

   if(gscale>0.0000001) {
      gscale = sqrt(gscale);
      for( i=0 ; i<6 ; i++) 
         matr[i]/= gscale;
   } // else, assume it is a b=0 ref

   return 0;
}



/*
  ORDER: 
  [0] Dxx, [1] Dxy, [2] Dyy, [3] Dxz, [4] Dyz, [5] Dzz
*/
int Dyadize(float **DT, 
            int N, 
            THD_3dim_dataset **EVALS, 
            float Lscale,
            THD_3dim_dataset **EVECS, 
            int INV[3], 
            byte *M)
{

   int i,j,k;
   float Lval;

   for( k=0 ; k<N ; k++ ) 
      if(M[k]) {
         for( i=0 ; i<3 ; i++ ) {
            Lval = THD_get_voxel(EVALS[i],k,0)/Lscale;
            DT[0][k]+= Lval*
               THD_get_voxel(EVECS[i],k,0)*
               THD_get_voxel(EVECS[i],k,0);
            DT[1][k]+= Lval*
               THD_get_voxel(EVECS[i],k,0)*
               THD_get_voxel(EVECS[i],k,1)*
               INV[0]*INV[1];
            DT[2][k]+= Lval*
               THD_get_voxel(EVECS[i],k,1)*
               THD_get_voxel(EVECS[i],k,1);
            DT[3][k]+= Lval*
               THD_get_voxel(EVECS[i],k,0)*
               THD_get_voxel(EVECS[i],k,2)*
               INV[0]*INV[2];
            DT[4][k]+= Lval*
               THD_get_voxel(EVECS[i],k,1)*
               THD_get_voxel(EVECS[i],k,2)*
               INV[1]*INV[2];
            DT[5][k]+= Lval*
               THD_get_voxel(EVECS[i],k,2)*
               THD_get_voxel(EVECS[i],k,2);
         }
      }         

   RETURN(0);
   
};


/*
  ORDER: 
  [0] Dxx, [1] Dxy, [2] Dyy, [3] Dxz, [4] Dyz, [5] Dzz

  apr,2016: updating to allow for bvalue weighted grads
*/
int RicianNoiseDWIs( float **dwi,
                     int N,
                     int Ngrad,
                     THD_3dim_dataset *D,
                     float NOISE_DWI,    
                     float NOISE_B0,
                     MRI_IMAGE *g,
                     byte *M,
                     float S0,
                     float bval,
                     gsl_rng *r)
{
   int i,j,k;
   float *grad;
   double sig;
   double sval;
   double riced;
   double gscale;

   grad = MRI_FLOAT_PTR(g);

   for( k=0 ; k<N ; k++) 
      if(M[k]) {
         sval = 1 + gsl_ran_gaussian_ziggurat(r,1.0) * NOISE_B0;
         riced = gsl_ran_gaussian_ziggurat(r,1.0) * NOISE_B0;
         dwi[0][k] = S0 * sqrt(sval*sval + riced*riced);

         for( i=0 ; i<Ngrad ; i++) {
            gscale = sqrt( grad[3*i]*grad[3*i] +
                           grad[3*i+1]*grad[3*i+1] +
                           grad[3*i+2]*grad[3*i+2] ); //bval;  apr,2016
            if( gscale < 0.00001 ) // if b~0
               gscale = 1.;
            sig = 0;
            sig+= THD_get_voxel(D,k,0)*grad[3*i]*grad[3*i];
            sig+= THD_get_voxel(D,k,2)*grad[3*i+1]*grad[3*i+1];
            sig+= THD_get_voxel(D,k,5)*grad[3*i+2]*grad[3*i+2];
            sig+= 2*THD_get_voxel(D,k,1)*grad[3*i]*grad[3*i+1];
            sig+= 2*THD_get_voxel(D,k,3)*grad[3*i]*grad[3*i+2];
            sig+= 2*THD_get_voxel(D,k,4)*grad[3*i+1]*grad[3*i+2];
            sig/= gscale; // apr,2016

            sval = exp(-bval*sig);
            sval+= gsl_ran_gaussian_ziggurat(r,1.0) * NOISE_DWI;
            riced = gsl_ran_gaussian_ziggurat(r,1.0) * NOISE_DWI;
            dwi[i+1][k] = S0 * sqrt(sval*sval + riced*riced);
         }
      }
   RETURN (0);
}

/*
  ORDER: 
  TORT:  [0] Dxx, [1] Dyy, [2] Dzz, [3] Dxy, [4] Dxz, [5] Dyz
  AFNI:  [0] Dxx, [1] Dxy, [2] Dyy, [3] Dxz, [4] Dyz, [5] Dzz
*/
int DT_TORTOISEtoAFNI(float **D, 
                      int N, 
                      THD_3dim_dataset *DTS, 
                      int INV[3], 
                      float Lscale)
{
   int i,j,k;

   for( k=0 ; k<N ; k++) {
      D[0][k] = THD_get_voxel(DTS,k,0)/Lscale;
      D[1][k] = INV[0]*INV[1]*THD_get_voxel(DTS,k,3)/Lscale;
      D[2][k] = THD_get_voxel(DTS,k,1)/Lscale;
      D[3][k] = INV[0]*INV[2]*THD_get_voxel(DTS,k,4)/Lscale;
      D[4][k] = INV[1]*INV[2]*THD_get_voxel(DTS,k,5)/Lscale;
      D[5][k] = THD_get_voxel(DTS,k,2)/Lscale;
   }
    
   RETURN (0);
}
