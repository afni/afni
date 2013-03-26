#include <afni.h>
#include <rsfc.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sort.h>
#include <gsl/gsl_permutation.h>



float FisherZ( double Rcorr)
{
  float Z=0.; 
  double denom=0.;
  
  denom = 1.0-Rcorr;
  if( (Rcorr>=-1) && (denom>0) )
    Z = 0.5 * log( (1.+Rcorr)/denom );
  else
    Z = 0; // primarily for diag Corr Matr values...
  
  return Z;
}


int CalcAveRTS(int *LIST, double *RAT, THD_3dim_dataset *T, 
               int *DIM, int *Nv)
{
  int i,n;
  double *ts=NULL;

  ts = (double *)calloc( DIM[3],sizeof(double));

  for( n=0; n<DIM[3] ; n++)  // for each time pt
    for( i=0 ; i<Nv[0] ; i++) // for each vox in TS
      ts[n] += THD_get_voxel(T,LIST[i],n);
  
  for( n=0; n<DIM[3] ; n++)
    RAT[n] = ts[n]/Nv[0];
      
  free(ts);
  
  RETURN(1);
}

// input time series one by one for memory saving

int CalcRanksForReHo(float *IND, int idx, THD_3dim_dataset *T, int *NTIE,
							int TDIM)
{
  int m,mm;
  int ISTIE = -1;
  int LENTIE = 0;
  float TIERANK;
  int *toP=NULL; // to reset permuts
  int *sorted=NULL; // hold sorted time course, assume has been turned into int
  int val;

  // GSL stuff
  gsl_vector *Y = gsl_vector_calloc(TDIM); // will hold time points
  gsl_permutation *P = gsl_permutation_calloc(TDIM); // will hold ranks


  toP = (int *)calloc(TDIM,sizeof(int)); 
  sorted = (int *)calloc(TDIM,sizeof(int)); 

  if( (toP ==NULL) || (sorted ==NULL) ) { 
    fprintf(stderr, "\n\n MemAlloc failure.\n\n");
    exit(122);
    }

  // define time series as gsl vector
  for( m=0 ; m<TDIM ; m++)
    gsl_vector_set(Y,m, THD_get_voxel(T,idx,m));
					
  // perform permutation
  val = gsl_sort_vector_index (P,Y);
  // apply permut to get sorted array values
  for( m=0 ; m<TDIM ; m++) {
    sorted[m] = THD_get_voxel(T,idx,
                              gsl_permutation_get(P,m));
    // information of where it was
    toP[m]= (int) gsl_permutation_get(P,m); 
    // default: just convert perm ind to rank ind:
    // series of rank vals
    IND[gsl_permutation_get(P,m)]=m+1;
  }
					
  // ******** start tie rank adjustment *******
  // find ties in sorted, record how many per time 
  //  series, and fix in IND
  for( m=1 ; m<TDIM ; m++)
    if( (sorted[m]==sorted[m-1]) && LENTIE==0 ) {
      ISTIE = m-1; //record where it starts
      LENTIE = 2;
    }
    else if( (sorted[m]==sorted[m-1]) && LENTIE>0 ) {
      LENTIE+= 1 ;
    }
    else if( (sorted[m]!=sorted[m-1]) && LENTIE>0 ) {
      // end of tie: calc mean index
      TIERANK = 1.0*ISTIE; // where tie started
      TIERANK+= 0.5*(LENTIE-1); // make average rank
      NTIE[idx]+= LENTIE*(LENTIE*LENTIE-1); // record
      // record ave permut ind as rank ind
      for( mm=0 ; mm<LENTIE ; mm++) {
        IND[toP[ISTIE+mm]] = TIERANK+1;
      }
      ISTIE = -1; // reset, prob unnec
      LENTIE = 0; // reset
    } // ******* end of tie rank adjustment ***********
  
  // FREE
  gsl_vector_free(Y);
  gsl_permutation_free(P);
  free(toP);
  free(sorted);
  
  RETURN(1);
}


int FindVoxHood(int *LIST, int **HS,
                int *iam,int *DIM,
                int ***MASK,int VN, int *realHOOD )
{
  int i,j,k,ii,jj,kk;//,m,n;
  int M = 0;// actual size of 'hood, because of possible boundary stuff
  int idx;
	
  for( i=0 ; i<VN ; i++) 
    LIST[i]=0; //wipe clean at start
  
  for( i=0 ; i<VN ; i++) {
    // x,y,z vox indices.
    ii = iam[0]+HS[i][0];
    jj = iam[1]+HS[i][1];
    kk = iam[2]+HS[i][2];
    
    // test possible conditions: inside boundaries and
    // being inside part of mask
    if( (ii>=0) && (ii<DIM[0]) && 
        (jj>=0) && (jj<DIM[1]) &&
        (kk>=0) && (kk<DIM[2])) // inside boundaries
      if (MASK[ii][jj][kk]){ // inside mask
        // idx = THREE_TO_IJK(ii,jj,kk,DIM[0],DIM[0]*DIM[1]);
        LIST[M] = THREE_TO_IJK(ii,jj,kk,DIM[0],DIM[0]*DIM[1]);
        M+=1;
      }
  }
  realHOOD[0]=M; // for friedman chi sq value
	
  RETURN(1);
}


float ReHoIt(int *LIST, float **RANKS, int *TIED, int *DIM, 
             int *realHOOD)
{
  int i,j,k,ii,jj,kk,m,n;
  double W = 0.0;
  int M = realHOOD[0];// actual size of 'hood, which is calc earlier
  int N = DIM[3];
  double miniR;
  double bigR = 0.;
  double fac1,fac2;
  double Tfac = 0.0;
	
  if( (M<1) || (N<2) )
    ERROR_exit("WARNING: either neighborhood size (M=%d) or time series\n"
               "\tlength (N=%d) was too small!",M,N);
	
  for( i=0 ; i<M ; i++)
    Tfac+= TIED[LIST[i]];
	
  fac1 = fac2 = (double) M *  M * N;
  fac1*= (double) 3*(N+1)*(N+1); //fac in numer
  fac2*= (double) (N*N)-1; // fac in denom
   
  // now go back through, and do sums over time series RANKS
  for( n=0; n<N ; n++) {
    miniR = 0.;
    for( i=0 ; i<M ; i++)
      miniR+= RANKS[LIST[i]][n];
    bigR+= miniR*miniR;
  }

  W = 12.*bigR-fac1;
  W/= fac2 - 1.*M*Tfac;
	
  return (float) W;
}

int IntSpherVol(int *RD, float *NR){
  int i,j,k;
  int ct=0;

  for ( i = 0 ; i <3 ; i++ ) 
    RD[i] = (int) ceil(NR[i]);
  for( i=-RD[0] ; i<=RD[0] ; i++)
    for( j=-RD[1] ; j<=RD[1] ; j++) 
      for( k=-RD[2] ; k<=RD[2] ; k++) 
        if( pow(i/NR[0],2)+pow(j/NR[1],2)+pow(k/NR[2],2)<=1 ){
          ct++;
        }

  return ct;
}

// ~silly near duplicate to fill array of values... expediency...
int IntSpherSha(int **HS,int *RD, float *NR){
  int i,j,k;
  int ct=0;

  for ( i = 0 ; i <3 ; i++ ) 
    RD[i] = (int) ceil(NR[i]);
  for( i=-RD[0] ; i<=RD[0] ; i++)
    for( j=-RD[1] ; j<=RD[1] ; j++) 
      for( k=-RD[2] ; k<=RD[2] ; k++) 
        if( pow(i/NR[0],2)+pow(j/NR[1],2)+pow(k/NR[2],2)<=1 ){
          HS[ct][0]=i;
          HS[ct][1]=j;
          HS[ct][2]=k;
          ct++;
        }

  return ct;
}




