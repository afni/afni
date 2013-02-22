#include <afni.h>
#include <rsfc.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sort.h>
#include <gsl/gsl_permutation.h>


int CalcRanksForReHo(float **IND, THD_3dim_dataset *T, int *NTIE,
							int ***MASK,int *DIM)
{
	int i,j,k,m,mm;
	int idx=0;
	int ISTIE = -1;
	int LENTIE = 0;
	float TIERANK;
	int *toP=NULL; // to reset permuts
	int *sorted=NULL; // hold sorted time course, assume has been turned into int
	int val;

	// GSL stuff
	gsl_vector *Y = gsl_vector_calloc(DIM[3]); // will hold time points
	gsl_permutation *P = gsl_permutation_calloc(DIM[3]); // will hold ranks


	toP = (int *)calloc(DIM[3],sizeof(int)); 
	sorted = (int *)calloc(DIM[3],sizeof(int)); 

	if( (toP ==NULL) || (sorted ==NULL) ) { 
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(122);
	}

	for( k=0 ; k<DIM[2] ; k++ ) 
		for( j=0 ; j<DIM[1] ; j++ ) 
			for( i=0 ; i<DIM[0] ; i++ ) {
				if(MASK[i][j][k]){
					// define time series as gsl vector
					for( m=0 ; m<DIM[3] ; m++)
						gsl_vector_set(Y,m, THD_get_voxel(T,idx,m));
					
					// perform permutation
					val = gsl_sort_vector_index (P,Y);
					// apply permut to get sorted array values
					for( m=0 ; m<DIM[3] ; m++) {
						sorted[m] = THD_get_voxel(T,idx,
														  gsl_permutation_get(P,m));
						// information of where it was
						toP[m]= (int) gsl_permutation_get(P,m); 
						// default: just convert perm ind to rank ind:
						// series of rank vals
						IND[idx][gsl_permutation_get(P,m)]=m+1;
					}
					
					// ******** start tie rank adjustment *******
					// find ties in sorted, record how many per time 
					//  series, and fix in IND
					ISTIE = -1; //prob unnec
					LENTIE = 0;
					for( m=1 ; m<DIM[3] ; m++)
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
								IND[idx][toP[ISTIE+mm]] = TIERANK+1;
							}
							ISTIE = -1; // reset, prob unnec
							LENTIE = 0; // reset
						} // ******* end of tie rank adjustment ***********
				}
				idx+= 1; 
			}

	// FREE
	gsl_vector_free(Y);
	gsl_permutation_free(P);
	free(toP);
	free(sorted);

	RETURN(1);
}


int FindVoxHood(int *LIST, 
				 int *iam,int *DIM,
				 int ***MASK,int HOOD, int *realHOOD )
{
	int i,j,k,ii,jj,kk;//,m,n;
	int M = 0;// actual size of 'hood, because of possible boundary stuff
	int idx;
	
	for( i=0 ; i<3 ; i++) //i.e., shifted [-1,0,1]...
		for( j=0 ; j<3 ; j++) //i.e., shifted [-1,0,1]...
			for( k=0 ; k<3 ; k++) { //i.e., shifted [-1,0,1]...
				LIST[M]=0; //wipe clean at start
				
				// x,y,z vox indices.
				ii = iam[0]+i-1; jj = iam[1]+j-1; kk = iam[2]+k-1;
				
				// test possible conditions: inside boundaries and
				// being inside part of mask
				if( (ii>=0) && (ii<DIM[0]) && 
					 (jj>=0) && (jj<DIM[1]) &&
					 (kk>=0) && (kk<DIM[2])) // inside boundaries
					if (MASK[ii][jj][kk]){ // inside mask
						idx = THREE_TO_IJK(ii,jj,kk,DIM[0],DIM[0]*DIM[1]);
						switch(HOOD){ // which types of neighbors 
						case 2: // facewise only
							if( abs(i-1)+abs(j-1)+abs(k-1)<2 ) {
								LIST[M] = idx;
								M+=1;
							}
							break;
						case 1: // face- and edge-wise
							if(abs(i-1)+abs(j-1)+abs(k-1)<3 ){
								LIST[M] = idx;
								M+=1;
							}
							break;
						case 0: // all included
							LIST[M] = idx;
							M+=1;
							break;
						default:
							continue;
						}
					}
			} // ijk loop
	realHOOD[0]=M; // for friedman chi sq value
	
	RETURN(1);
}


float ReHoIt(int *LIST, float **RANKS, int *TIED, int *DIM, 
				 int ***MASK, int *realHOOD)
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
