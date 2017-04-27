/* 
	Supplementary code for 3dTrackID, written by PA Taylor.

	This code is for using the results of 3dAllineate (using
	-1Dmatrix_save) to map the resulting tracks of 3dTrackID (*.trk
	file and putting in the ORIGIN information {option at this
	juncture}) to another space (for example, MNI standard) for
	visualization purposes.  Note that the WM maps and the FA,
	etc. maps can be transformed directly with -1Dmatrix_apply).
   
   September 2012, part II: fixing some memory stuff.

   Oct,2016: add int to get rid of compilation warnings.
*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_rng.h>
#include <debugtrace.h>
#include <mrilib.h>    
#include <3ddata.h>    
#include <TrackIO.h>
#include <DoTrackit.h>

// shifts to REF coors; need to shift once more to get to TrkVis-type
// units using ORIG of REF
int shift_coors(gsl_matrix *A, float *B, float *xin); 

void usage_map_TrackID(int detail) 
{
	printf(
"\n"
"  Supplementary code for 3dTrackID, written by PA Taylor, part of FATCAT\n"
"  (Taylor & Saad, 2013) in AFNI.\n"
"  \n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  USAGE: This program maps the track file (*.trk) output of 3dTrackID to\n"
"  another space, such as MNI standard, using the 1Dmatrix_save info of\n"
"  3dAllineate.  The scalar values are not changed or interpolated to within,\n"
"  the new space, but instead they just migrate along-- in practice, this\n"
"  should be fine, since they should move along with associated/underlying\n"
"  voxels as one used the 1Dmatrix to shift, e.g., the 3D FA, MD, etc. data\n"
"  sets.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  COMMAND: map_TrackID -prefix FILE -in_trk FILE -in_map FILE -ref FILE \\\n"
"           {-verb  -line_only_num -already_inv}\n"
"  \n\n"
"  OUTPUTS (named using prefix, PREF):  \n"
"    1) TRK file, named PREF.trk, mapped to new space (view in TrackVis).\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
"  RUNNING, need to provide:\n"
"    -prefix  OUT_PREF :this will be the prefix of the output track file,\n"
"                       `OUT_PREF.trk'.\n"
"    -in_trk  TRK_FILE :the name of the *.trk file to be mapped. Must be a\n"
"                       TrackVis readable file, and probably one created\n"
"                       by using 3dTrackID with the (newly added) `-rec_orig'\n"
"                       option (see 3dTrackID help for description, short\n"
"                       reason being: TrackVis currently doesn't use origin\n"
"                       info, and to have image pop up in the middle of the \n"
"                       TrackVis viewer, I left default origin being 0,0,0).\n" 
"    -in_map  1D_MATR  :single line of matrix values for the transformation\n"
"                       of old x-coor to new x'-coor via:\n"
"                             x' = Ux+V.\n"
"                       Have only tested this with the 1D_MATR file coming\n"
"                       from `3dAllineate -1Dmatrix_save 1D_MATR...' command.\n"
"                       NB: map_TrackID has been written to just use the\n"
"                       aformentioned 1D_MATR file spewed out by 3dAllineate,\n"
"                       which has a line of text followed by 12 params in a \n"
"                       single line (see 3dAllineate help for more info):\n"
"                       u11 u12 u13 v1 u21 u22 u23 v2 u31 u32 u33.\n"
"                       However, you can also use outputs of cat_matvec,\n"
"                       which don't have a text line, so you would then want\n"
"                       to then use the `-line_only_num' option (below).\n"
"                       A more subtle point: for whatever reason, when \n"
"                       the U-matrix and V-vector are applied in this code, \n"
"                       they actually have to be applied as if they had been\n"
"                       given for the inverse transform x and x', i.e.:\n"
"                            x' = U^{-1}x - U^{-1}V, \n"
"                       where U^{-1} is the inverse of U.  Therefore, if you\n"
"                       get your transformation from non-3dAllineate usage,\n"
"                       you might have to invert what you mean by U and V\n"
"                       here. If you use your `backward' matrix/vectors, or\n"
"                       if you use cat_matvec to invert your matrix or\n"
"                       something, then use the `-already_inv' switch \n"
"                       (below). \n"
"                       HOWEVER, to avoid confusion, and to not cause worry\n"
"                       if the preceding discussion didn't make sense, the\n"
"                       *default* running of the code is to just use the\n"
"                       standard 1D_MATR of 3dAllineate to give appropriate\n"
"                       transform.  If you use another program, and if your\n"
"                       results look inverted/flipped/rotated/translated,\n"
"                       then consider the above!\n"
"    -ref     TO_FILE  :3D data set in space to which TRK_FILE is being\n"
"                       mapped. Mainly to read the header for necessary info.\n"
"    and the following options (all are just switches):\n"
"    -verb             :Verbose output. \n"
"    -orig_zero        :put (0,0,0) as the origin in the output *.trk file,\n"
"                       as opposed to having the `real' values recorded.\n"
"                       TrackVis does not really use the origin for much,\n"
"                       but having a nonzero-origin will cause the location\n"
"                       of the tracks in the viewer window to be off-center,\n"
"                       and it sets the rotation-in-space axis about the,\n"
"                       origin with the combined effect that a nonzero-origin\n"
"                       can be a bit more difficult to view and manipulate;\n"
"                       however, if you might want to map the tracks again\n"
"                       later, then you would want to have the `real' origin\n"
"                       values recorded. (Default: off.)\n"
"    -line_only_num    :if your 1D_MATR file is just 12 numbers in a row,\n"
"                       like after using cat_matvec or some other program.\n"
"                       Default is to skip the little verbiage in the first\n"
"                       line, as included in `3dAllineate -1Dmatrix_save...'.\n"
"    -already_inv      :if you have inverted a mapping or use some other\n"
"                       program than 3dAllineate, whose transformation matrix\n"
"                       and vector get applied a bit differently than one\n"
"                       (i.e., me) might have thought (and see long `-in_map'\n"
"                       description above for more in depth info); as guide,\n"
"                       one might try this option if transform looks to be\n"
"                       backwards, flipped or shifted oddly, esp. if not just\n"
"                       making use of output of 3dAllineate.\n"
"\n"
"* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n"
"\n"
" EXAMPLE (with view toward PTaylor_TractDemo files, using MNI as ref):\n"
"      map_TrackID                                       \\\n"
"        -prefix TEST_FILES/DTI/o.TRACK_to_MNI           \\\n"
"        -in_trk TEST_FILES/DTI/o.TRACK_ballFG.trk       \\\n"
"        -in_map TEST_FILES/DTI/map_to_refMNI.aff12.1D   \\\n"
"        -ref TEST_FILES/DTI/MNI_3mm+tlrc \n" 
"   which could be run after, for example:\n"
"      3dAllineate                                       \\\n"
"        -1Dmatrix_save TEST_FILES/DTI/map_to_refMNI     \\\n"
"        -input TEST_FILES/DTI/DT_FA+orig.               \\\n"
"        -base TEST_FILES/DTI/MNI_3mm+tlrc               \\\n"
"        -mi                                             \\\n"
"        -prefix TEST_FILES/DTI/MNI_DT_FAn"
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
	int i,j,k,m,mm,nn;
   int pppp;
	int iarg;

	tv_io_header READ_head;
	int READ_in;
	short int READ_sh;
	float READ_fl; 
	char READ_ch='a';
	unsigned char READ_uc;

	THD_3dim_dataset *refset = NULL;
	int nrefset=0;

	char ref_voxel_order[4] = "RAI\0";
	int Dim[3]={0,0,0}; // dim in each dir
	float Ledge[3]={0.,0.,0.}; // voxel edge lengths
	float Orig[3] = {0.0,0.0,0.0};

	float old_orig[3] = {0.0,0.0,0.0}; // need to remember some old vals...
	float old_ledge[3] = {0.0,0.0,0.0};
	int old_dim[3] = {0,0,0};
	char old_voxel_order[4] = "RAI\0";

	int TV_switch[3] = {0,0,0};
	int TV_switch2[3] = {0,0,0};

	// transform info, read in from 1Dmatr_save
	// so that new x' = Ux + V.
	float *Vvect, *Vvect_inv;//[3] = {0.,0.,0.};
	float *loc_old;

	int N_vox_trk = 0;

	FILE *file_map, *file_trk, *file_out; // 1dMAP and input/output *.trk files
	char *prefix_map="default_map";
	char *prefix_trk="default_trk";
	char *prefix_out="default_out";
	char TRK_OUT[300];

	int INP_3DALLIN = 0; // switch about input/format of *.1D matr file
	int MATR_REV = 1; // to do the inversion here (input to not do so)
	int ZERO_ORIG_OUT = 0;

	gsl_matrix *Umatr = gsl_matrix_alloc(3, 3);
	gsl_permutation *P = gsl_permutation_alloc(3);
	gsl_matrix *Umatr_inv = gsl_matrix_alloc(3, 3);
	

	mainENTRY("map_TrackID"); machdep(); 
  
	// ****************************************************************
	// ****************************************************************
	//                    load AFNI stuff
	// ****************************************************************
	// ****************************************************************

	INFO_message("version: THETA");

	// scan args 
	if (argc == 1) { usage_map_TrackID(1); exit(0); }
	iarg = 1;
	while( iarg < argc && argv[iarg][0] == '-' ){
		if( strcmp(argv[iarg],"-help") == 0 || 
			 strcmp(argv[iarg],"-h") == 0 ) {
			usage_map_TrackID(strlen(argv[iarg])>3 ? 2:1);
			exit(0);
		}
    
		if( strcmp(argv[iarg],"-verb") == 0) {
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-verb'") ;
			set_tract_verb(atoi(argv[iarg]));
			iarg++ ; continue ;
		}

		// switch for format of input *.1D matrix-- if exists, then
		// assumes the *.1D file is numbers ONLY probably still in a
		// line, guess it won't matter now
		if( strcmp(argv[iarg],"-line_only_num") == 0) {
			INP_3DALLIN=1;
			iarg++ ; continue ;
		}

		// we will do inversion here if no input from user-- this is
		// because of the format of 3dAllineate matrix
		if( strcmp(argv[iarg],"-already_inv") == 0) {
			MATR_REV=0;
			iarg++ ; continue ;
		}

		if( strcmp(argv[iarg],"-orig_zero") == 0) {
			ZERO_ORIG_OUT=1;
			iarg++ ; continue ;
		}


		// REFERENCE DATA SET: BRIK/HEAD OR NII
		if( strcmp(argv[iarg],"-ref") == 0 ){
			if( ++iarg >= argc ) 
				ERROR_exit("Need argument after '-ref'");
			refset = THD_open_dataset( argv[iarg] );
			if( refset == NULL ) 
				ERROR_exit("Can't open ref dataset '%s'", argv[iarg]);
			DSET_load(refset) ; CHECK_LOAD_ERROR(refset);
			nrefset = DSET_NVOX(refset);
			ref_voxel_order[0]=ORIENT_typestr[refset->daxes->xxorient][0];
			ref_voxel_order[1]=ORIENT_typestr[refset->daxes->yyorient][0];
			ref_voxel_order[2]=ORIENT_typestr[refset->daxes->zzorient][0];

			Dim[0] = DSET_NX(refset); Dim[1] = DSET_NY(refset); 
			Dim[2] = DSET_NZ(refset); 
			Orig[0] = DSET_XORG(refset); Orig[1] = DSET_YORG(refset);
			Orig[2] = DSET_ZORG(refset);
			Ledge[0] = fabs(DSET_DX(refset)); Ledge[1] = fabs(DSET_DY(refset)); 
			Ledge[2] = fabs(DSET_DZ(refset)); 

			iarg++ ; continue ;
		}
	
		// OUTPUT NAME
		if( strcmp(argv[iarg],"-prefix") == 0 ){
			iarg++ ; if( iarg >= argc ) 
							ERROR_exit("Need argument after '-prefix'");
			prefix_out = strdup(argv[iarg]) ;
			
			iarg++ ; continue ;
		}
	 
		// TRACKVIS FILE NAME
		if( strcmp(argv[iarg],"-in_trk") == 0 ){
			iarg++ ; 
			if( iarg >= argc ) 
				ERROR_exit("Need argument after '-in_trk'");
			prefix_trk = strdup(argv[iarg]) ;

			iarg++ ; continue ;
		}

		// 1DMATRIX_SAVE FILE NAME
		if( strcmp(argv[iarg],"-in_map") == 0 ){
			iarg++ ; 
			if( iarg >= argc ) 
				ERROR_exit("Need argument after '-in_map'");
			prefix_map = strdup(argv[iarg]);

			iarg++ ; continue ;
		}

		ERROR_message("Bad option '%s'\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
		exit(1);
	}
	 
	if (iarg < 4) {
		ERROR_message("Too few options. Try -help for details.\n");
		exit(1);
	}
	 
	// ************************************************************
	// ************************************************************
	//                    Start opening and using
	// ************************************************************
	// ************************************************************
	
	// the 1D matrix thing
	if( (file_map = fopen(prefix_map, "r")) == NULL) {
		fprintf(stderr, "Error opening file %s.",prefix_map);
		exit(2);
	}
	
	Vvect = (float *)calloc(3, sizeof(float)); 
	Vvect_inv = (float *)calloc(3, sizeof(float)); 
	loc_old = (float *)calloc(3, sizeof(float)); 
	
	if(  (Vvect == NULL) || (Vvect_inv == NULL) || (loc_old == NULL)) {
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(12);
	}
	
	if(!INP_3DALLIN){
		// got to read a ONELINE file that either has a header or doesn't...
		// because of using either one from 3dAllineate
		
		j=0;
		while(READ_ch!= '\n' && j<300) {
			pppp = fscanf(file_map,"%c",&READ_ch);
			j+=1;
			//printf("\nCHAR:%c\t%d",READ_ch,j);
		}
		if(j ==300)
			ERROR_exit("Something wrong with 1Dmatrix_save file: too long header? or just bad??");
	}
	
	gsl_matrix_set_zero(Umatr);   
	gsl_matrix_set_zero(Umatr_inv);



	for( j=0 ; j<3 ; j++){
		for( i=0 ; i<3 ; i++){
			pppp = fscanf(file_map,"%f",&READ_fl);
			gsl_matrix_set(Umatr,j,i,READ_fl);	
			//printf("\nREAD:%f",Umatr[j][i]);
		}
		pppp = fscanf(file_map,"%f",&Vvect[j]);
		//printf("\nREAD:%f",Vvect[j]);
	}
	
	fclose(file_map);
	
	if( MATR_REV ) {
		j = gsl_linalg_LU_decomp(Umatr, P, &k);
		j = gsl_linalg_LU_invert(Umatr, P, Umatr_inv);
		if( j !=0 )
			ERROR_exit("Something wrong with 1Dmatrix_save file: problem inverting.");
		// the vector in this case is mult by the inverse matr and by `-1'
		for( j=0 ; j<3 ; j++)
			for( i=0 ; i<3 ; i++)
				Vvect_inv[j]-= gsl_matrix_get(Umatr_inv,j,i)*Vvect[i];
	}
	else{
		// if matrix is already inverted
		for( j=0 ; j<3 ; j++) {
			for( i=0 ; i<3 ; i++)
				gsl_matrix_set(Umatr_inv,j,i,gsl_matrix_get(Umatr,j,i));	
			Vvect_inv[j] = Vvect[j];
		}
	}

	// Start reading TRK file
	if( (file_trk = fopen(prefix_trk, "r")) == NULL) {
		fprintf(stderr, "Error opening file %s.",prefix_trk);
		exit(2);
	}
	
	// header
	pppp = fread(&READ_head,sizeof(tv_io_header),1,file_trk);
	
	// as a check
	if( READ_head.hdr_size != 1000 ) 
		ERROR_exit("Header not in correct format-- you sure it's from TrackVis/3dTrackID?") ;
	
	// because old_voxel_order was initialized to RAI, can test with it
	// for condition of making initial switch in converting track coor to
	// DICOM
	for( i=0 ; i<3 ; i++) {
		TV_switch[i] = !(old_voxel_order[i]==READ_head.voxel_order[i]);
		TV_switch2[i] = !(old_voxel_order[i]==ref_voxel_order[i]);
	}
	// adjust info in header: match that of REF data set;
	// need both old and ref info for transform
	for( i=0 ; i<3 ; i++) {
		old_dim[i] = READ_head.dim[i]; // store old value
		old_ledge[i] = READ_head.voxel_size[i]; // store old value
		old_voxel_order[i] = READ_head.voxel_order[i]; // store old value
		old_orig[i] = READ_head.origin[i]; // store old value
		//if(TV_switch[i])
		//	old_orig[i] = old_orig[i]-old_ledge[i]*(old_dim[i]-1);
		READ_head.voxel_order[i] = ref_voxel_order[i];
		READ_head.dim[i] = Dim[i];
		READ_head.voxel_size[i] = Ledge[i];
		if(ZERO_ORIG_OUT != 0) // for viewing
			READ_head.origin[i] = Orig[i]; // so use Orig[] as value in calcs
		else
			READ_head.origin[i] = 0.;
	}
	
	INFO_message("Old origin:\t%f, %f, %f",old_orig[0],old_orig[1],old_orig[2]);
	INFO_message("New origin:\t%f, %f, %f",Orig[0],Orig[1],Orig[2]);



	sprintf(TRK_OUT,"%s.trk",prefix_out); 
	// OPEN the output file
	if( (file_out = fopen(TRK_OUT, "w")) == NULL) {
		fprintf(stderr, "Error opening file %s.",TRK_OUT);
		exit(2);
	}
	
	// start writing out new file...
	fwrite(&READ_head,sizeof(tv_io_header),1,file_out);
		
	// START going through each coor, shifting coor values, but not the assoc.
	// scalar values (for now...).
	//	mm=0;
	while( (fread(&nn,sizeof(int),1,file_trk) == 1) ) {
		//	mm++;
		// printf("\ntracklen:\t%d\t%d",nn,mm);
		fwrite(&nn,sizeof(int),1,file_out);
		// get length of new track
		for( j=0; j<nn ; j++) {
			for( k=0; k<3 ; k++) {
				pppp = fread(&READ_fl,sizeof(float),1,file_trk);
				if(TV_switch[k])
					loc_old[k] = READ_fl+old_orig[k]-old_ledge[k]*(old_dim[k]-1);
				else 
					loc_old[k] = old_ledge[k]*old_dim[k]+old_orig[k]-READ_fl;
			}
			// map `old' to REF
			m = shift_coors(Umatr_inv,Vvect_inv,loc_old); 
			for( k=0; k<3 ; k++) {
				// REF to trackvis-like REF
				if(TV_switch2[k])
					READ_fl = loc_old[k]-Orig[k]
						+ READ_head.voxel_size[k]*(READ_head.dim[k]-1);
				else //if(!TV_switch2[k])
					READ_fl = READ_head.dim[k]*READ_head.voxel_size[k]
						- loc_old[k] + Orig[k];
				fwrite(&READ_fl,sizeof(float),1,file_out);
			}
			// not changing...
			for( k=0; k<READ_head.n_scalars ; k++) {
				pppp = fread(&READ_fl,sizeof(float),1,file_trk);
				fwrite(&READ_fl,sizeof(float),1,file_out);
			}
		}
		// not changing...
		for( k=0; k<READ_head.n_properties ; k++) {
			pppp = fread(&READ_fl,sizeof(float),1,file_trk);
			fwrite(&READ_fl,sizeof(float),1,file_out);
		}
	}
	
	fclose(file_out);
	fclose(file_trk);

	// ************************************************************
	// ************************************************************
	//                    Freeing
	// ************************************************************
	// ************************************************************

	free(prefix_map);
	free(prefix_out);
	free(prefix_trk);
	free(loc_old);
	free(Vvect);
	free(Vvect_inv);
	gsl_matrix_free(Umatr);
	gsl_matrix_free(Umatr_inv);
	gsl_permutation_free(P);
	DSET_delete(refset);
	free(refset);

	INFO_message("Done mapping.");

	return 0;
}

// takes in old coor and changes them to new ones
int shift_coors(gsl_matrix *A, float *B, float *xin)
{
	int i, j;
	float dum[3] = {0.,0.,0.};
	
	for( j=0 ; j<3 ; j++)
		for( i=0 ; i<3 ; i++)
			dum[j]+= gsl_matrix_get(A,j,i)*xin[i];
	
	for( i=0 ; i<3 ; i++)
		xin[i] = dum[i]+B[i];
	
	return 1;
}
