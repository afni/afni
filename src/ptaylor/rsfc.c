#include <afni.h>
#include <rsfc.h>

float ReHoIt(float ****RANKS, int ***TIED, int *iam, int *DIM, 
				 int ***MASK, int HOOD, int *realHOOD)
{
  	int i,j,k,ii,jj,kk,m,n;
	float W = 0.0;
	int M = 0;// actual size of 'hood, because of possible boundary stuff
	int N = DIM[3];
	int YES[3][3][3];
	float miniR;
	float bigR = 0.;
	float fac1,fac2;
	float Tfac = 0.0;
	
	for( i=0 ; i<3 ; i++) //i.e., shifted [-1,0,1]...
		for( j=0 ; j<3 ; j++) //i.e., shifted [-1,0,1]...
			for( k=0 ; k<3 ; k++) { //i.e., shifted [-1,0,1]...
				// new coor
				YES[i][j][k] = 0;
				ii = iam[0]+i-1; jj = iam[1]+j-1; kk = iam[2]+k-1;

				// test possible conditions: inside boundaries and
				// being inside part of mask
				if( (ii>=0) && (ii<DIM[0]) && (jj>=0) && (jj<DIM[1]) &&
					 (kk>=0) && (kk<DIM[2])) // inside boundaries
					if (MASK[ii][jj][kk]){ // inside mask
						switch(HOOD){ // which types of neighbors 
						case 2: // facewise only
							if( abs(i-1)+abs(j-1)+abs(k-1)<2 ) {
								YES[i][j][k] = 1;
								M+=1;
								Tfac+= TIED[ii][jj][kk];
							}
							break;
						case 1: // face- and edge-wise
							if(abs(i-1)+abs(j-1)+abs(k-1)<3 ){
								YES[i][j][k] = 1;
								M+=1;
								Tfac+= TIED[ii][jj][kk];
							}
							break;
						case 0: // all included
							YES[i][j][k] = 1;
							M+=1;
							Tfac+= TIED[ii][jj][kk];
							break;
						}
					}
			} // ijk loop

	
	fac1 = fac2 = M*M*N;
	fac1*= 3*(N+1)*(N+1); //fac in numer
	fac2*= N*N-1; // fac in denom

	if( (M<1) || (N<2) )
		ERROR_exit("WARNING: either neighborhood size (M=%d) or time series \n"
					  "\tlength (N=%d) was too small at ijk (%d,%d,%d)!",M,N,
					  iam[0],iam[1],iam[2]);

	realHOOD[0]=M; // for friedman chi sq value
	
	// now go back through, and do sums over time series RANKS
	for( n=0; n<N ; n++) {
		miniR = 0.;
		for( i=0 ; i<3 ; i++)
			for( j=0 ; j<3 ; j++)
				for( k=0 ; k<3 ; k++) 
					if( YES[i][j][k] ) {
						ii = iam[0]+i-1; jj = iam[1]+j-1; kk = iam[2]+k-1;
						miniR+= RANKS[ii][jj][kk][n];
					}
		bigR+= miniR*miniR;
	}

	W = 12.*bigR-fac1;
	W/= fac2 - 1.*M*Tfac;

	return W;
}
