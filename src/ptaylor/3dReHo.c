/* 
   REHO/Kendall W code, written by PA Taylor (July, 2012).

   ReHo (regional homogeneity) is just a renaming of the Kendall's W
	(or Kendall's coefficient of concordance, KCC, (Kendall & Babington
	Smith, 1939)) for a time series.  Application to fMRI data was
	described in paper: <<Regional homogeneity approach to fMRI data
	analysis>> by Zang, Jiang, Lu, He, and Tiana (2004, NeuroImage).

	+ updated, Sept. 2012: just memory freeing stuff.
*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>    
#include <rsfc.h>    
#include <3ddata.h>    
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sort.h>
#include <gsl/gsl_permutation.h>


void usage_ReHo(int detail) 
{
	printf(
"\n"
"  REHO/Kendall W code, written by PA Taylor (July, 2012).\n\n"
"  ReHo (regional homogeneity) is just a renaming of the Kendall's W\n"
"  (or Kendall's coefficient of concordance, KCC, (Kendall & Babington\n"
"  Smith, 1939)) for set of time series.  Application to fMRI data was\n"
"  described in paper: <<Regional homogeneity approach to fMRI data\n"
"  analysis>> by Zang, Jiang, Lu, He, and Tiana (2004, NeuroImage),\n"
"  where it was applied to the study of both task and resting state\n"
"  functional connectivity (RSFC).\n"
"  \n"
"  USAGE: Read in data from 4D time series data set (assume brain is masked)\n"
"         and calculate Kendall's W per voxel using neighborhood voxels. \n"
"         Instead of the time series values themselves, Kendall's W uses the\n"
"         relative rank ordering of a 'hood over all time points to evaluate\n"
"         a parameter W in range 0-1, with 0 reflecting no trend of agreement\n"
"         between time series and 1 reflecting perfect agreement. From W, one\n"
"         can simply get Friedman's chi-square value (with degrees of freedom\n"
"         equal to `the length of the time series minus one'), so this can\n"
"         also be calculated here and returned in the second sub-brick:\n"
"         chi-sq = (N_n)*(N_t - 1)*W,   with N_dof = N_t - 1,\n"
"         where N_n is the size of neighborhood; N_t is the number of \n"
"         time points; W is the ReHo or concordance value; and N_dof is the\n"
"         number of degrees of freedom. A switch is included to have the \n"
"         chi-sq value output as a subbrick of the ReHo/W. (In estimating W,\n"
"         tied values are taken into account by averaging appropriate \n"
"         rankings and adjusting other factors in W appropriately, which \n"
"         only makes a small difference in value, but the computational time\n"
"         still isn't that bad).\n"
"  \n"
"  COMMAND:  3dReHo -prefix PREFIX -nneigh {7|19|27} -inset FILE {-chi_sq}\n"
"  \n\n"
"  + RUNNING: need to provide (and your data should be masked already):\n"
"    -prefix PREFIX   :output file name part.\n"
"    -nneigh NUMBER   :number of voxels in neighborhood, inclusive; can be: \n"
"                      7   (for facewise neighbors, only),\n"
"                      19  (for face- and edge-wise neighbors),\n"
"                      27  (for face-, edge-, and node-wise neighbors).\n"
"    -inset  FILE     :time series file. \n\n"
"    -chi_sq          :switch to output Friedman chi-sq value per voxel\n"
"                      as a subbrick.\n"
"  + OUTPUT: single file with name, e.g., PREFIX+orig.BRIK, which may have\n"
"            two subbricks (2nd subbrick if `-chi_sq' switch is used):\n"
"            [0] contains the ReHo (Kendall W) value per voxel;\n"
"            [1] contains Friedman chi-square of ReHo per voxel (optional);\n"
"                note that the number of degrees of freedom of this value is\n"
"                the length of time series minus 1.\n"
"  \n" );
	return;
}

int main(int argc, char *argv[]) {
	int i,j,k,m,n,mm;
	int iarg;
	THD_3dim_dataset *insetTIME = NULL;
	THD_3dim_dataset *inset0 = NULL;
	THD_3dim_dataset *outsetREHO=NULL;
	char *prefix="REHO" ;
	int Nneigh=27,N_TYPE=0;
	char in_name[300];
	char in_name0[300];
	char out_pref[300];

	FILE *fout0;
	float ****INDEX; // hold ranks
	int ***mskd; // define mask of where time series are nonzero
	float *KW,*chisq; // value of Kendall's W and Friedman's chi-sq of it
	int ***Nties; // record number of ties per voxel time course (as t^3-t)
	int *sorted; // hold sorted time course, assume has been turned into int
	int *toP; // to reset permuts
	int ISTIE = -1;
	int LENTIE = 0;
	float TIERANK;
	int CHI_ON = 0; // default to not output chi-sq

	int Nvox=-1;   // tot number vox
	int *Dim=NULL;
	int *here=NULL;
	int *ndof=NULL;

	int val;
	int idx;

	mainENTRY("3dReHo"); machdep(); 
  
	// ****************************************************************
	// ****************************************************************
	//                    load AFNI stuff
	// ****************************************************************
	// ****************************************************************

	// INFO_message("version: ETA");
	
	/** scan args **/
	if (argc == 1) { usage_ReHo(1); exit(0); }
	iarg = 1; 
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 || 
			 strcmp(argv[iarg],"-h") == 0 ) {
			usage_ReHo(strlen(argv[iarg])>3 ? 2:1);
			exit(0);
		}
		
		if( strcmp(argv[iarg],"-chi_sq") == 0) {
			CHI_ON=1;
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-prefix") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-prefix'");
			prefix = strdup(argv[iarg]) ;
			if( !THD_filename_ok(prefix) ) 
				ERROR_exit("Illegal name after '-prefix'");
			iarg++ ; continue ;
		}
	 
		if( strcmp(argv[iarg],"-inset") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-input'");

			sprintf(in_name,"%s", argv[iarg]); 
			insetTIME = THD_open_dataset(in_name) ;
			if( (insetTIME == NULL ))
				ERROR_exit("Can't open time series dataset '%s'.",in_name);
			// just 0th time point for output...
			sprintf(in_name0,"%s[0]", argv[iarg]); 
			inset0 = THD_open_dataset(in_name0) ;
			if( (inset0 == NULL ))
				ERROR_exit("Can't open 0th brick of dataset as '%s[0]'.",in_name0);

			Dim = (int *)calloc(4,sizeof(int));
			DSET_load(insetTIME); CHECK_LOAD_ERROR(insetTIME);
			Nvox = DSET_NVOX(insetTIME) ;
			Dim[0] = DSET_NX(insetTIME); Dim[1] = DSET_NY(insetTIME); 
			Dim[2] = DSET_NZ(insetTIME); Dim[3]= DSET_NVALS(insetTIME); 

			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-nneigh") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-nneigh'");
			
			//INFO_message("Size of neighborhood is: %s",argv[iarg]);
			Nneigh = atoi(argv[iarg]);
			if( Nneigh == 27 ) 
				N_TYPE = 0;
			else if( Nneigh == 19 )
				N_TYPE = 1;
			else if( Nneigh == 7 )
				N_TYPE = 2;
			else 
				ERROR_exit("Illegal after '-nneigh': need '27', '19' or '7'");
			iarg++ ; continue ;
		}
		
		ERROR_message("Bad option '%s'\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
		exit(1);
	}
	
	if (iarg < 3) {
		ERROR_message("Too few options. Try -help for details.\n");
		exit(1);
	}
	
	// ****************************************************************
	// ****************************************************************
	//                    make gsl vectors and storage
	// ****************************************************************
	// ****************************************************************

	KW = (float *)calloc(Nvox,sizeof(float)); 
	chisq = (float *)calloc(Nvox,sizeof(float)); 
	here = (int *)calloc(3,sizeof(int)); 
	sorted = (int *)calloc(Dim[3],sizeof(int)); 
	toP = (int *)calloc(Dim[3],sizeof(int)); 
	ndof = (int *)calloc(1,sizeof(int)); 

	mskd = (int ***) calloc( Dim[0], sizeof(int **) );
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		mskd[i] = (int **) calloc( Dim[1], sizeof(int *) );
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		for ( j = 0 ; j < Dim[1] ; j++ ) 
			mskd[i][j] = (int *) calloc( Dim[2], sizeof(int) );

	Nties = (int ***) calloc( Dim[0], sizeof(int **) );
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		Nties[i] = (int **) calloc( Dim[1], sizeof(int *) );
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		for ( j = 0 ; j < Dim[1] ; j++ ) 
			Nties[i][j] = (int *) calloc( Dim[2], sizeof(int) );

	// hold ranks
	INDEX = (float ****) calloc( Dim[0], sizeof(float ***) );
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		INDEX[i] = (float ***) calloc( Dim[1], sizeof(float **) );
	for ( i = 0 ; i < Dim[0] ; i++ ) 
		for ( j = 0 ; j < Dim[1] ; j++ ) 
			INDEX[i][j] = (float **) calloc( Dim[2], sizeof(float *) );
	for ( i=0 ; i<Dim[0] ; i++ ) 
		for ( j=0 ; j<Dim[1] ; j++ ) 
			for ( k= 0 ; k<Dim[2] ; k++ ) 
				INDEX[i][j][k] = (float *) calloc( Dim[3],  sizeof(float) );
	
	// this statement will never be executed if allocation fails above
	if( (INDEX == NULL) || (mskd == NULL) || (KW == NULL) || (toP ==NULL)
		 || (here == NULL) || (Nties == NULL) || (sorted == NULL) 
		 || (sorted == NULL) || (ndof == NULL)) { 
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(122);
	}
	
	// GSL stuff
	gsl_vector *Y = gsl_vector_calloc(Dim[3]); // will hold time points
	gsl_permutation *P = gsl_permutation_calloc(Dim[3]); // will hold ranks

	// *************************************************************
	// *************************************************************
	//                    Beginning of main loops
	// *************************************************************
	// *************************************************************
	
	// go through once: define data vox, and calc rank for each
	idx = 0;
	for( k=0 ; k<Dim[2] ; k++ ) 
		for( j=0 ; j<Dim[1] ; j++ ) 
			for( i=0 ; i<Dim[0] ; i++ ) {
			   if(fabs(THD_get_voxel(insetTIME,idx,0))+
					fabs(THD_get_voxel(insetTIME,idx,1))+
					fabs(THD_get_voxel(insetTIME,idx,2))+
					fabs(THD_get_voxel(insetTIME,idx,3))+
					fabs(THD_get_voxel(insetTIME,idx,4)) < EPS_V)
					idx+= 1; // skip, and mskd and KW are both still 0 from calloc
				else{
					mskd[i][j][k] = 1;
					// define time series as gsl vector
					for( m=0 ; m<Dim[3] ; m++)
						gsl_vector_set(Y,m, THD_get_voxel(insetTIME,idx,m));

					// perform permutation
					val = gsl_sort_vector_index (P,Y);
					// apply permut to get sorted array values
					for( m=0 ; m<Dim[3] ; m++) {
						sorted[m] = THD_get_voxel(insetTIME,idx,
														  gsl_permutation_get(P,m));
						// information of where it was
						toP[m]= (int) gsl_permutation_get(P,m); 
						// default: just convert perm ind to rank ind:
						// series of rank vals
						INDEX[i][j][k][gsl_permutation_get(P,m)]=m+1;
					}

					
					// ******** start tie rank adjustment *******
					// find ties in sorted, record how many per time 
					//  series, and fix in INDEX
					ISTIE = -1; //prob unnec
					LENTIE = 0;
					for( m=1 ; m<Dim[3] ; m++)
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
							Nties[i][j][k]+= LENTIE*(LENTIE*LENTIE-1); // record
							// record ave permut ind as rank ind
							for( mm=0 ; mm<LENTIE ; mm++) {
								INDEX[i][j][k][toP[ISTIE+mm]] = TIERANK+1;
							}
							ISTIE = -1; // reset, prob unnec
							LENTIE = 0; // reset
						} // ******* end of tie rank adjustment ***********
					idx+= 1; 
				}
			}
	//	printf("\n idx=%d, nvox=%d\n\n",idx,Nvox);
	
	idx = 0;
	// calculate KendallW
	for( k=0 ; k<Dim[2] ; k++ ) 
		for( j=0 ; j<Dim[1] ; j++ ) 
			for( i=0 ; i<Dim[0] ; i++ ) {
				if(mskd[i][j][k]) {
					here[0] = i; here[1] = j; here[2] = k; 
					KW[idx] = ReHoIt(INDEX,Nties,here,Dim,mskd,N_TYPE,ndof);
					chisq[idx] = ndof[0]*(Dim[3]-1)* KW[idx];
				}
				idx+=1;
			}

	// **************************************************************
	// **************************************************************
	//                 Store and output
	// **************************************************************
	// **************************************************************
	
	outsetREHO = EDIT_empty_copy( inset0 ) ; 
	if(CHI_ON)
		EDIT_add_bricklist(outsetREHO,
								 1, NULL , NULL , NULL );
	EDIT_dset_items( outsetREHO,
							  ADN_datum_all , MRI_float , 
							  ADN_prefix    , prefix ,
							  ADN_none ) ;
	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outsetREHO)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(outsetREHO));
	EDIT_substitute_brick(outsetREHO, 0, MRI_float, KW); 
	KW=NULL;
	if(CHI_ON)
		EDIT_substitute_brick(outsetREHO, 1, MRI_float, chisq); 
	chisq=NULL;
	EDIT_BRICK_LABEL(outsetREHO,0,"ReHo");      
	if(CHI_ON) 
		EDIT_BRICK_LABEL(outsetREHO,1,"frchisq");      
	THD_load_statistics(outsetREHO);
	tross_Make_History("3dReHo", argc, argv, outsetREHO);
	THD_write_3dim_dataset(NULL, NULL, outsetREHO, True);

	INFO_message("ReHo (Kendall's W) calculated.");
	if(CHI_ON)
		INFO_message("Friedman chi-sq of W calculated (%d deg of freedom per vox)",Dim[3]-1);

	
	// ************************************************************
	// ************************************************************
	//                    Freeing
	// ************************************************************
	// ************************************************************


	DSET_delete(insetTIME);
	DSET_delete(inset0);
	DSET_delete(outsetREHO);
	free(KW); 
	free(chisq);
	free(here);
	
	free(sorted);
	free(toP);

	for( i=0 ; i<Dim[0] ; i++) 
		for( j=0 ; j<Dim[1] ; j++) 
			for( k=0 ; k<Dim[2] ; k++) 
				free(INDEX[i][j][k]);
	for( i=0 ; i<Dim[0] ; i++) 
		for( j=0 ; j<Dim[1] ; j++) {
			free(INDEX[i][j]);
			free(mskd[i][j]);
			free(Nties[i][j]);
		}
	for( i=0 ; i<Dim[0] ; i++) {
		free(INDEX[i]);
		free(mskd[i]);
		free(Nties[i]);
	}
	free(INDEX);
	free(mskd);
	free(Nties);
	free(ndof);

	free(Dim); // need to free last because it's used for other arrays...
	gsl_vector_free(Y); // need to free these.
	gsl_permutation_free(P);
	free(insetTIME);
	free(inset0);
	free(outsetREHO);
	free(prefix);



	return 0;
}

