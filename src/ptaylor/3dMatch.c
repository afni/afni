/* 

   Program to match sets of components and calculate some quantities of 
	similarity, written by PA Taylor (Nov, 2012).

	This version doesn't tack on extra/unmatched components.

	Dec. 2012: fixed silly bug in indices.

   Apr 2014: fixed something that caused errors on Linux machines, but 
             not on other machines.  Weird. 
*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include "mrilib.h"    
#include "rsfc.h"    
#include "3ddata.h"    
#include <gsl/gsl_rng.h>
#include <gsl/gsl_statistics_double.h>
#include "DoTrackit.h"

#define SILLY_EPS (1.e-5)  // thresh away from zero for to be in mask

void usage_Match(int detail) 
{
	printf(
"\n"
"  3dMatch, written by PA Taylor (Nov., 2012), part of FATCAT (Taylor & Saad,\n"
"    2013) in AFNI.\n\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  Find similar subbricks and rearrange order to ease comparison\n\n"
"  Comparison simply done by comparing (weighted) correlation maps of\n"
"  values, which may include thresholding of either refset or inset\n"
"  values. The weighting is done by squaring each voxel value (whilst\n"
"  maintaining its original sign). The Dice coefficient is also calculated\n"
"  to quantify overlap of regions.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + COMMANDS: \n"
"    3dMatch -inset FILE1 -refset FILE2 {-mask FILE3} {-in_min THR1} \\ \n"
"           {-in_max THR2} {-ref_min THR3} {-ref_max THR4} -prefix FILE4 \\\n"
"           {-only_dice_thr} \n"
"    where:\n"
"     -inset  FILE1  :file with M subbricks of data to match against another\n"
"                     file.\n"
"     -refset FILE2  :file with N subbricks, serving as a reference for\n"
"                     FILE1.  N=M is *not* a requirement; matching is done\n"
"                     based on squares of values (with signs preserved), and\n"
"                     both best fit of in->ref and ref->in are calculated \n"
"                     and output.\n"
"     -mask   FILE3  :a mask of regions to include in the correlation of \n"
"                     data sets; technically not necessary as relative \n"
"                     correlation values shouldn't change, but the magnitudes\n"
"                     would scale up without it. Dice coeff values should not\n"
"                     be affected by absence or presence of wholebrain mask.\n"
"     -in_min  THR1  :during the correlation/matching analysis, values below\n"
"                     THR1 in the `-inset' will be zeroed (and during Dice\n"
"                     coefficient calculation, excluded from comparison).\n"
"                     (See `-only_dice_thr' option, below.)\n"
"     -in_max  THR2  :during the correlation/matching analysis, values above\n"
"                     THR2 in the `-inset' will be zeroed (and during Dice\n"
"                     coefficient calculation, excluded from comparison).\n"
"     -ref_min  THR3 :during the correlation/matching analysis, values below\n"
"                     THR3 in the `-refset' will be zeroed (and during Dice\n"
"                     coefficient calculation, excluded from comparison).\n"
"                     (See `-only_dice_thr' option, below.)\n"
"     -ref_max  THR4 :during the correlation/matching analysis, values above\n"
"                     THR4 in the `-refset' will be zeroed (and during Dice\n"
"                     coefficient calculation, excluded from comparison).\n"
"     -prefix FILE4  :prefix out output name for both *BRIK/HEAD files, as\n"
"                     well as for the *_coeff.vals text files (see below).\n"
"     -only_dice_thr :if option is included in command line, the thresholding\n"
"                     above is only applied during Dice evaluation, not \n"
"                     during spatial correlation.\n\n"
"  + OUTPUTS, named using prefix; \n"
"     *_REF+orig     :AFNI BRIK/HEAD file with the same number of subbricks\n"
"                     as the `-refset' file, each one corresponding to a\n"
"                     subbrick of the `-inset' file with highest weighted\n"
"                     correlation. Any unmatched `-inset' subbricks are NOT\n"
"                     appended at the end. (For example, you could underlay\n"
"                     the -ref_set FILE2 and visually inspect the comparisons\n"
"                     per slice.)\n"
"     *_REF_coeff.vals :simple text file with four columns, recording the\n"
"                     original brick number slices which have been\n"
"                     reordered in the output *_REF+orig file. Cols. 1&2-\n"
"                     orig `-refset' and `-inset' indices, respectively;\n"
"                     Col. 3- weighted correlation coefficient; Col 4.-\n"
"                     simple Dice coefficient.\n"
"     *_IN+orig      :AFNI BRIK/HEAD file with the same number of subbricks\n"
"                     as the `-inset' file, each one corresponding to\n"
"                     a subbrick of the `-refset' file with highest weighted\n"
"                     correlation. Any unmatched `-refset' subbricks are NOT\n"
"                     appended at the end. (For example, you could underlay\n"
"                     the -inset FILE1 and visually inspect the comparisons\n"
"                     per slice.)\n"
"     *_IN_coeff.vals :simple text file with four columns, recording the\n"
"                     original brick number slices which have been\n"
"                     reordered in the output *_IN+orig file. Cols. 1&2-\n"
"                     orig `-inset' and `-refset' indices, respectively;\n"
"                     Col. 3- weighted correlation coefficient; Col 4.-\n"
"                     simple Dice coefficient.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  + EXAMPLE:\n"
"      3dMatch                           \\\n"
"         -inset CORREL_DATA+orig        \\\n"
"         -refset STANDARD_RSNs+orig     \\\n"
"         -mask mask+orig                \\\n"
"         -in_min 0.4                    \\\n"
"         -ref_min 2.3                   \\\n"
"         -prefix MATCHED                \\\n"
"         -only_dice_thr\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  If you use this program, please reference the introductory/description\n"
"  paper for the FATCAT toolbox:\n"
"        Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional\n"
"        And Tractographic Connectivity Analysis Toolbox. Brain \n"
"        Connectivity 3(5):523-535.\n"
"____________________________________________________________________________\n"
);
	return;
}


int main(int argc, char *argv[]) {
	int i,j,k,m,ii,jj,kk,n,bb;
	int X,Y,Z;
	int iarg=0;
	THD_3dim_dataset *inset = NULL;
	char in_name[300];
	int THR=0.;
	char *prefix="NAME_Match";
   int HAVEPREFIX=0;
	THD_3dim_dataset *insetREF = NULL;
	THD_3dim_dataset *insetMASK = NULL;
	THD_3dim_dataset *outset=NULL;
	THD_3dim_dataset *outset2=NULL;
	float **temp_arr;
	char bri_lab[300];
	FILE *fout1;


	char in_REF[300];
	char in_MASK[300];
	char out_corr[300];
	char out_corr2[300];
	char prefix2[300];
	char prefix1[300];

	int HAVEMASK=0;
	int ONLY_DICE_THR=0; // about what to threshold when
	int Nunmatch_IN=0,Nunmatch_IN2=0;
	int idx = 0;
	int Nvox=0, Larray=0; 

	int *Dim=NULL; 
   short *minimask=NULL;
	int REFBRIKS=0;
	double **Data_Ref=NULL, **Data_In=NULL;
	// just really big magn numbers, essentially `no bounds'
	double MIN_THR_IN=-1.e20,MIN_THR_REF=-1.e20; 
	double MAX_THR_IN=1.e20,MAX_THR_REF=1.e20;

	float ***Stats_Matr=NULL;
	int NI=0,NR=0,NIR=0;
	float sign=0.;
	int *MatchList=NULL, *InvMatchList=NULL;
	int *MatchList2=NULL, *InvMatchList2=NULL;
	float **MatchCC;
	
	mainENTRY("3dMatch"); machdep(); 
  
	// ****************************************************************
	// ****************************************************************
	//                    load AFNI stuff
	// ****************************************************************
	// ****************************************************************

   //	INFO_message("version: BETA");
	Dim = (int *)calloc(4,sizeof(int));
   INFO_message("Loading data.");

	// scan args
	if (argc == 1) { usage_Match(1); exit(0); }
	iarg = 1; 
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 || 
			 strcmp(argv[iarg],"-h") == 0 ) {
			usage_Match(strlen(argv[iarg])>3 ? 2:1);
			exit(0);
		}
			 
		if( strcmp(argv[iarg],"-inset") == 0 ){
			iarg++ ; 
			if( iarg >= argc ) 
				ERROR_exit("Need argument after '-inset'");

			sprintf(in_name,"%s", argv[iarg]); 
			inset = THD_open_dataset(in_name) ;
			if( (inset == NULL ))
				ERROR_exit("Can't open time series dataset '%s'.",in_name);
			DSET_load(inset); CHECK_LOAD_ERROR(inset);

			Dim[0] = DSET_NX(inset); Dim[1] = DSET_NY(inset); 
			Dim[2] = DSET_NZ(inset); Dim[3]= DSET_NVALS(inset); 
			Nvox = DSET_NVOX(inset) ;

			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-in_min") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-in_min'");
			//INFO_message("Size of threshold is: %s",argv[iarg]);
			MIN_THR_IN = atof(argv[iarg]);
			
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-in_max") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-in_max'");
			//INFO_message("Size of threshold is: %s",argv[iarg]);
			MAX_THR_IN = atof(argv[iarg]);

			iarg++ ; continue ;
		}


		if( strcmp(argv[iarg],"-ref_min") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-ref_min'");
			//INFO_message("Size of threshold is: %s",argv[iarg]);
			MIN_THR_REF = atof(argv[iarg]);
			
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-ref_max") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-ref_max'");
			//INFO_message("Size of threshold is: %s",argv[iarg]);
			MAX_THR_REF = atof(argv[iarg]);

			iarg++ ; continue ;
		}


		if( strcmp(argv[iarg],"-only_dice_thr") == 0) {
			ONLY_DICE_THR=1;
			iarg++ ; continue ;
		}

		
		if( strcmp(argv[iarg],"-prefix") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-prefix'");
			prefix = strdup(argv[iarg]);

			if( !THD_filename_ok(prefix) ) 
				ERROR_exit("Illegal name after '-prefix'");
         HAVEPREFIX=1;

			iarg++ ; continue ;
		}


		if( strcmp(argv[iarg],"-refset") == 0 ) {
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-refset'");
			
			sprintf(in_REF,"%s", argv[iarg]); 
			insetREF = THD_open_dataset(in_REF) ;
			if( (insetREF == NULL ))
				ERROR_exit("Can't open time series dataset '%s'.",in_REF);
			
			DSET_load(insetREF); CHECK_LOAD_ERROR(insetREF);
			
			REFBRIKS = DSET_NVALS(insetREF);			

			iarg++ ; continue ;
		}


		if( strcmp(argv[iarg],"-mask") == 0 ) {
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-mask'");
			
			sprintf(in_MASK,"%s", argv[iarg]); 
			
			HAVEMASK=1;

			iarg++ ; continue ;
		}


		/*


		// can have vol thr.
		if( strcmp(argv[iarg],"-volthr") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-volthr'");
			VOLTHR = atoi(argv[iarg]);
			
			if(VOLTHR<=0)
				ERROR_exit("Volume threshold for size of ROIs=%d: must be >0!",
							  VOLTHR);
			
			iarg++ ; continue ;
			}*/

		ERROR_message("Bad option '%s'\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
		exit(1);
	}
	
	if (iarg < 3) {
		ERROR_message("Too few options. Try -help for details.\n");
		exit(1);
	}
   
   if((Dim[0] != DSET_NX(insetREF)) || (Dim[1] != DSET_NY(insetREF)) ||
      (Dim[2] != DSET_NZ(insetREF)) )
      ERROR_exit("The xyz-dimensions of refset and inset don't match");
   
   
	// ****************************************************************
	// ****************************************************************
	//                    make inset storage
	// ****************************************************************
	// ****************************************************************
   
   minimask = (short *)calloc(Nvox,sizeof(short));
   INFO_message("Setting up arrays.");

	if(HAVEMASK) {

      insetMASK = THD_open_dataset(in_MASK) ;
      if( (insetMASK == NULL ))
         ERROR_exit("Can't open time series dataset '%s'.",in_MASK);
      
      DSET_load(insetMASK); CHECK_LOAD_ERROR(insetMASK);
      if((Dim[0] != DSET_NX(insetMASK)) || (Dim[1] != DSET_NY(insetMASK)) ||
         (Dim[2] != DSET_NZ(insetMASK)) )
         ERROR_exit("The xyz-dimensions of maskset and inset don't match");


		Larray=0;
		for ( i = 0 ; i < Nvox ; i++ ) 
			if( THD_get_voxel(insetMASK,i,0)>0 ) {
            minimask[i]=1;
				Larray++;
         }

      DSET_delete(insetMASK);
      free(insetMASK);
      
	}
	else {		
      for ( i = 0 ; i < Nvox ; i++ ) 
         minimask[i]=1;
		Larray=Nvox;
   }
      
   



	Data_Ref = calloc( REFBRIKS, sizeof(double *) );
	for ( j = 0 ; j < REFBRIKS ; j++ ) 
		Data_Ref[j] = (double *) calloc( Larray, sizeof(double) );
	Data_In = calloc( Dim[3], sizeof(double *) );
	for ( j = 0 ; j < Dim[3] ; j++ ) 
		Data_In[j] = (double *) calloc( Larray, sizeof(double) );

	// N_inset x N_refset x Nstats --> Weighted map, simple Dice
	Stats_Matr = (float ***) calloc( Dim[3], sizeof(float **) );
	for ( i = 0 ; i < Dim[3] ; i++ ) 
		Stats_Matr[i] = (float **) calloc( REFBRIKS, sizeof(float *) );
	for ( i = 0 ; i < Dim[3] ; i++ ) 
		for ( j = 0 ; j < REFBRIKS ; j++ ) 
			Stats_Matr[i][j] = (float *) calloc( 2, sizeof(float ) );
	
	MatchList = (int *)calloc(REFBRIKS,sizeof(int));
	InvMatchList = (int *)calloc(Dim[3],sizeof(int));
	MatchList2 = (int *)calloc(Dim[3],sizeof(int));
	InvMatchList2 = (int *)calloc(REFBRIKS,sizeof(int));

	if( (Stats_Matr == NULL) || (Data_Ref == NULL) || (Data_In == NULL) 
		 || (MatchList == NULL) || (InvMatchList == NULL) 
		 || (MatchList2 == NULL) || (InvMatchList2 == NULL) ) { 
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(14);
	}
	
	
	// ****************************************************************
	// ****************************************************************
	//                    make inset storage
	// ****************************************************************
	// ****************************************************************

   
	for ( i = 0 ; i < Nvox ; i++ ) 
      if( minimask[i] ) {
         for( m=0 ; m<REFBRIKS ; m++ ) {
            Data_Ref[m][idx] = THD_get_voxel(insetREF,i,m)*
               dabs(THD_get_voxel(insetREF,i,m));
         }
         for( m=0 ; m<Dim[3] ; m++ ) 
            Data_In[m][idx] = THD_get_voxel(inset,i,m)*
               dabs(THD_get_voxel(inset,i,m));
         idx++;
      }
   
   INFO_message("Correlating.");

	if(ONLY_DICE_THR)
		for( m=0 ; m< REFBRIKS ; m++ ) 
			for( n=0 ; n< Dim[3] ; n++ ) 
				if( (gsl_stats_variance(Data_In[n],1,Larray)>0) &&
					 (gsl_stats_variance(Data_Ref[m],1,Larray)>0) ) 
					Stats_Matr[n][m][0] = (float) 
						CORR_FUN(Data_In[n], Data_Ref[m], Larray);
				else
					Stats_Matr[n][m][0] = 0.;


	//				Stats_Matr[n][m][0] = (float) 
	//				CORR_FUN(Data_In[n], Data_Ref[m], Larray);
	
	// because we squared the data!!
	MIN_THR_IN*=dabs(MIN_THR_IN);
	MAX_THR_IN*=dabs(MAX_THR_IN);
	MIN_THR_REF*=dabs(MIN_THR_REF);
	MAX_THR_REF*=dabs(MAX_THR_REF);

	// check values: mins/maxs from output
	for ( i = 0 ; i < Larray ; i++ ) {
		for( m=0 ; m<Dim[3] ; m++ ) 
			if(Data_In[m][i]<MIN_THR_IN)
				Data_In[m][i]=0;//MIN_THR_IN-SILLY_EPS;
			else if(Data_In[m][i]>MAX_THR_IN)
				Data_In[m][i]=0;//MAX_THR_IN+SILLY_EPS;
		for( m=0 ; m<REFBRIKS ; m++ ) 
			if(Data_Ref[m][i]<MIN_THR_REF)
				Data_Ref[m][i]=0;//MIN_THR_REF-SILLY_EPS;
			else if(Data_Ref[m][i]>MAX_THR_REF)
				Data_Ref[m][i]=0;//MAX_THR_REF+SILLY_EPS;
	}
	
	for( m=0 ; m< REFBRIKS ; m++ ) {
		for( n=0 ; n< Dim[3] ; n++ ) {
			
			if(!ONLY_DICE_THR)
				if( gsl_stats_variance(Data_In[n],1,Larray)>0 &&
					 gsl_stats_variance(Data_Ref[m],1,Larray)>0 ) 
					Stats_Matr[n][m][0] = (float) 
						CORR_FUN(Data_In[n], Data_Ref[m], Larray);
				else
					Stats_Matr[n][m][0] = 0.;
			
			//	Stats_Matr[n][m][0] =(float)CORR_FUN(Data_In[n], 
         //                                      Data_Ref[m], Larray);

			// calc dice
			NI = 0;
			NR = 0;
			NIR = 0;
			for ( i = 0 ; i < Larray ; i++ ) {
				if( (Data_In[n][i]>SILLY_EPS) || (Data_In[n][i]<-SILLY_EPS) ) {
					NI++;
					if( (Data_Ref[m][i]>SILLY_EPS) || (Data_Ref[m][i]<-SILLY_EPS) ) 
						NIR++; // both
				}
				if( (Data_Ref[m][i]>SILLY_EPS) || (Data_Ref[m][i]<-SILLY_EPS) ) 
					NR++;
			}
			if(NI+NR>0) 
				Stats_Matr[n][m][1]= NIR*2./(NI+NR);
			else
				Stats_Matr[n][m][1]= 0.;
		}
	}

	/*	for( m=0 ; m< REFBRIKS ; m++ ) {
		printf("\n");
		for( n=0 ; n< Dim[3] ; n++ ) {
			printf("\t%.4f",Stats_Matr[n][m][0]);
		}}
		printf("\n");*/
   INFO_message("Final counting and writing.");

		
	// to find where any unmatches are
	for( n=0 ; n< Dim[3] ; n++ ) 
		InvMatchList[n] = -1;
	// default is '0'th, see if better one is around for each ref
	for( m=0 ; m< REFBRIKS ; m++ ) {
		for( n=0 ; n< Dim[3] ; n++ ) {
			if(Stats_Matr[n][m][0]> Stats_Matr[MatchList[m]][m][0])
				MatchList[m] = n;
		}
		InvMatchList[MatchList[m]] = m;
	}

	Nunmatch_IN=0;
	for( n=0 ; n< Dim[3] ; n++ ) 
		if(InvMatchList[n]==-1)
			Nunmatch_IN++;

// to find where any unmatches are
	for( n=0 ; n< REFBRIKS ; n++ ) 
		InvMatchList2[n] = -1;
	// default is '0'th, see if better one is around for each *inset*
	for( n=0 ; n< Dim[3] ; n++ ) {
		for( m=0 ; m< REFBRIKS ; m++ ) {
			if(Stats_Matr[n][m][0]> Stats_Matr[n][MatchList2[n]][0])
				MatchList2[n] = m;
		}
		InvMatchList2[MatchList2[n]] = n;
	}
	Nunmatch_IN2=0;
	for( n=0 ; n< REFBRIKS ; n++ ) 
		if(InvMatchList2[n]==-1)
			Nunmatch_IN2++;
	
	
	// **************************************************************
	// **************************************************************
	//                 Store and output
	// **************************************************************
	// **************************************************************
	
	
	//REF
	sprintf(prefix1,"%s_REF",prefix);

	outset = EDIT_empty_copy( insetREF ) ; 
	
	EDIT_dset_items( outset,
						  ADN_datum_all , MRI_float , 
						  ADN_prefix    , prefix1 ,
						  ADN_none ) ;
	//@@
	//	if(Nunmatch_IN>0)
	//	EDIT_add_bricklist(outset,
	//							 Nunmatch_IN, NULL , NULL , NULL );

	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outset)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(outset));
	
	temp_arr = calloc( REFBRIKS,sizeof(temp_arr));
   //+Nunmatch_IN,sizeof(temp_arr));//@@
	for(i=0 ; i<REFBRIKS ; i++) //+Nunmatch_IN ; i++) 
		temp_arr[i] = calloc( Nvox,sizeof(float) ); 
	MatchCC = calloc( Dim[3],sizeof(MatchCC));
   //+Nunmatch_IN,sizeof(MatchCC));//@@
	for(i=0 ; i<Dim[3] ; i++) //+Nunmatch_IN ; i++) 
		MatchCC[i] = calloc( Nvox,sizeof(float) ); 

	if(  (temp_arr== NULL) || (MatchCC== NULL) ) { 
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(14);
	}
	
	for( m=0 ; m<REFBRIKS ; m++ ) {
		for( i=0 ; i<Nvox ; i++ ) 
			temp_arr[m][i] = THD_get_voxel(inset,i,MatchList[m]);
		
		EDIT_substitute_brick(outset, m, MRI_float, temp_arr[m]); 
		temp_arr[m]=NULL; // to not get into trouble...
		sprintf(bri_lab,"in_%d",MatchList[m]);
		EDIT_BRICK_LABEL(outset,m,bri_lab); //original index val
	}
	//@@
	//	for( m=REFBRIKS ; m<REFBRIKS+Nunmatch_IN ; m++ ) {
	//	for( n=0 ; n< Dim[3] ; n++ ) 
	//		if(InvMatchList[n]==-1) {
	//			for( i=0 ; i<Nvox ; i++ ) 
	//				temp_arr[m][i] = THD_get_voxel(inset,i,n);
	//			EDIT_substitute_brick(outset, m, MRI_float, temp_arr[m]); 
	//			temp_arr[m]=NULL; // to not get into trouble...
	//			sprintf(bri_lab,"%d",n);
	//			EDIT_BRICK_LABEL(outset,m,bri_lab); //original index val
	//			InvMatchList[n]=m;
	//			break;
	//			}
	//			}
	

	sprintf(out_corr,"%s_coeff.vals",prefix1);
	if( (fout1 = fopen(out_corr, "w")) == NULL) {
		fprintf(stderr, "Error opening file %s.",out_corr);
		exit(19);
	}
	for( i=0 ; i<REFBRIKS ; i++ ) {
		fprintf(fout1,"%d\t\t%d\t\t%.3f\t\t%.3f\n",i,MatchList[i],
				  Stats_Matr[MatchList[i]][i][0],Stats_Matr[MatchList[i]][i][1]);
	}
	fclose(fout1);    
	
	THD_load_statistics(outset);
	tross_Make_History("3dMatch", argc, argv, outset);
	THD_write_3dim_dataset(NULL, NULL, outset, True);
	DSET_delete(outset);

	for( i=0 ; i<REFBRIKS ; i++) //+Nunmatch_IN ; i++) // free all//@@
		free(temp_arr[i]);
	free(temp_arr);
	
	
	
	// INSET	
	sprintf(prefix2,"%s_IN",prefix);
	
	outset2 = EDIT_empty_copy( inset ) ; 
	//@@
	//	if(Nunmatch_IN2>0)
	//	EDIT_add_bricklist(outset2,
	//							 Nunmatch_IN2, NULL , NULL , NULL );
	
	EDIT_dset_items( outset2,
						  ADN_datum_all , MRI_float , 
						  ADN_prefix    , prefix2 ,
						  ADN_none ) ;

	if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(outset2)) )
		ERROR_exit("Can't overwrite existing dataset '%s'",
					  DSET_HEADNAME(outset2));

	for( m=0 ; m<Dim[3] ; m++ ) {
		for( i=0 ; i<Nvox ; i++ ) 
			MatchCC[m][i] = THD_get_voxel(insetREF,i,MatchList2[m]);
		
		EDIT_substitute_brick(outset2, m, MRI_float, MatchCC[m]); 
		MatchCC[m]=NULL; // to not get into trouble...
		sprintf(bri_lab,"ref_%d",MatchList2[m]);
		EDIT_BRICK_LABEL(outset2,m,bri_lab); //original index val
	} 
	//@@
	//	for( m=Dim[3] ; m<Dim[3]+Nunmatch_IN2 ; m++ ) {
	//	for( n=0 ; n< REFBRIKS ; n++ ) 
	//		if(InvMatchList2[n]==-1) {
	//			for( i=0 ; i<Nvox ; i++ ) 
	//				MatchCC[m][i] = THD_get_voxel(insetREF,i,n);
	//			EDIT_substitute_brick(outset2, m, MRI_float, MatchCC[m]); 
	//			MatchCC[m]=NULL; // to not get into trouble...
	//			sprintf(bri_lab,"%d",n);
	//			EDIT_BRICK_LABEL(outset2,m,bri_lab); //original index val
	//			InvMatchList2[n]=m;
	//			break;
	//			}
	//			}

	THD_load_statistics(outset2);
	tross_Make_History("3dMatch", argc, argv, outset2);
	THD_write_3dim_dataset(NULL, NULL, outset2, True);
	DSET_delete(outset2);
	  
	for( i=0 ; i<Dim[3] ; i++) //+Nunmatch_IN2 ; i++) // free all //@@
		free(MatchCC[i]);
	free(MatchCC);
	
	sprintf(out_corr2,"%s_coeff.vals",prefix2);
	if( (fout1 = fopen(out_corr2, "w")) == NULL) {
		fprintf(stderr, "Error opening file %s.",out_corr2);
		exit(19);
	}
	for( i=0 ; i<Dim[3] ; i++ ) {
		fprintf(fout1,"%d\t\t%d\t\t%.3f\t\t%.3f\n",i,MatchList2[i],
				  Stats_Matr[i][MatchList2[i]][0],Stats_Matr[i][MatchList2[i]][1]);
	}
	fclose(fout1);    


   INFO_message("DONE.  You can now check your outputs as follows:");
   INFO_message("%s is a reorganized form of the refset -> best match per "
                "inset brick.\n"
                "\t Overlay on the inset '%s' to check matches visually.\n"
                "\t The associated correlation & Dice quantities are in '%s'.",
                prefix2, in_name, out_corr2);
   INFO_message("%s is a reorganized form of the inset -> best match per "
                "refset brick.\n"
                "\t Overlay on the refset '%s' to check matches visually.\n"
                "\t The associated correlation & Dice quantities are in '%s'.",
                prefix1, in_REF, out_corr);


	// ************************************************************
	// ************************************************************
	//                    Freeing
	// ************************************************************
	// ************************************************************
		
	DSET_delete(inset);
	free(inset);
	DSET_delete(insetREF);
	free(insetREF);
	free(outset);
	free(outset2);

	for( i=0 ; i<Dim[3] ; i++)
		for( j=0 ; j<REFBRIKS ; j++)
			free(Stats_Matr[i][j]);
	for( i=0 ; i<Dim[3] ; i++)
		free(Stats_Matr[i]);
	free(Stats_Matr);
	
	for( i=0 ; i<Dim[3] ; i++)
		free(Data_In[i]);
	for( j=0 ; j<REFBRIKS ; j++)
		free(Data_Ref[j]);
	free(Data_Ref);
	free(Data_In);
	free(MatchList);
	free(InvMatchList);
	free(MatchList2);
	free(InvMatchList2);

	free(Dim); 
   if( HAVEPREFIX )
      free(prefix);
	free(minimask);

	return 0;
}

