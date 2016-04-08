#include <afni.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <DoTrackit.h>



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

   RETURN(1);
   
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
   RETURN (1);
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
    
   RETURN (1);
}
