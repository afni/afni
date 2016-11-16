#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>     // AFNIadd
#include <3ddata.h>     // AFNIadd
#include <TrackIO.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_rng.h>
#include <DoTrackit.h>



/*
  Added in Feb,2015.  

  Trim away record of bundles that are 'too small', whose threshold
  can be defined by the user.

 */
int ByeByeBundle( int A,
                  int B,
                  int NET,
                  int **Prob_grid,
                  float ***Prob_grid_L,
                  float ***Param_grid,
                  int L_paramgrid,
                  int ***NETROI,
                  int L_netroi,
                  int *NROI )
{
   int i, lll;
   
   // magical matrix location
   lll = MatrInd_to_FlatUHT(A, B, NROI[NET]);

   Prob_grid[NET][lll] = 0;

   Prob_grid_L[NET][lll][0] = 0; // Nov,2016
   Prob_grid_L[NET][lll][1] = 0;

   for( i=0 ; i<L_paramgrid ; i++ )
      Param_grid[NET][lll][i] = 0;

   for( i=0 ; i<L_netroi ; i++ )
      if( NETROI[i][NET][lll] )
         NETROI[i][NET][lll] = 0;

   RETURN(1);
}


/* 
   Order no longer matters here for 'U'HT.
   We check for the order internally
   Inputs: NxN square matr coor -> (ROW, COL, N)
   Output: flat UHT index value PLUS 1
   *This one goes along the diagonal first!*
   Useful for unique enumeration of tracts, hence the plus one
 */
int MatrInd_to_FlatUHT_DIAG_P1(int i, int j, int N)
{
   int lll;

   if( i<=j ) {
      i = j-i;
      lll = i*N+j; // sq matr coor
      lll-= (i*(i+1))/2; // fix for tridiag.
   }
   else {
      j = i-j;
      lll = j*N+i; // sq matr coor
      lll-= (j*(j+1))/2; // fix for tridiag.
   }
   lll+=1;

   return lll;
}

/*
  For when there are multiple connections-- take the unique UHT decomp
  from P1, and then add N(N-1)/2 to it.  Specifically when the max num
  of overlaps is 2, but we'll also have multi PLUS two of the overlaps
  in the string label.
  Max value to be output (even after the +1): N*N. 
 */
int MatrInd_to_FlatUHT_DIAG_M(int i, int j, int N)
{
   int lll;

   lll = MatrInd_to_FlatUHT_DIAG_P1(i,j,N);
   // add N(N-1)/2 to that value.
   lll+= FlatUHT_Len(N);
   lll-= N;

   return lll;
}

/* 
   Order no longer matters here for 'U'HT.
   We check for the order internally
   Inputs: NxN square matr coor -> (ROW, COL, N)
   Output: flat UHT index value
 */
int MatrInd_to_FlatUHT(int i, int j, int N)
{
   int lll;

   if( i<=j ) {
      lll = i*N+j; // sq matr coor
      lll-= (i*(i+1))/2; // fix for tridiag.
   }
   else {
      lll = j*N+i; // sq matr coor
      lll-= (j*(j+1))/2; // fix for tridiag.
   }

   return lll;
}

int FlatUHT_Len(int N)
{
   int out;
   out = N*(N+1); // out must have been an even number...
   out/= 2;
   return out;
}


// Some funniness to deal with possibility of having biggest label of
// a tractogr ROI be >M, where M is actual number of ROIs.  Before, we
// required labels to be 1..M, but now we'll free that restriction up.
// REF is input data set of ROI labels
// NUMROI   ends with number of ROIs per brik
//     - dimensions:  N_briks x 1
//     - stores value of M per brik
// ROILIST ends with having ordered list of ROILABEL ints
//     - dimensions:  N_briks x (max ROI label)+1
//     - data dims:   N_briks x M+1
//     (where the +1s are because we go 1....value)
// INVLIST has the ith values at the actual input locations... hence gives
//     inverse info to ROILIST
// INVROI starts with max val per brik; ends with keeping
//     track of max input index per brik
// 
// for now, all negative ROI indexing is handled in using MAPROI
// don't think negative integers can be in refset for 3dROIMaker yet, though
int ViveLeRoi(THD_3dim_dataset *REF, int **ROILIST, int **INVLIST, 
				  int *NUMROI, int *INVROI)
{ // NB what index boundaries are for various loops here...
	int Nbrik=0,Nvox=0;
	int i,j,k,m;
	int nrois=0;

	Nbrik = DSET_NVALS(REF);
	Nvox = DSET_NVOX(REF);


	for( m=0 ; m<Nbrik ; m++ ) 
		for( i=0 ; i<Nvox ; i++ ) 
			if( THD_get_voxel(REF,i,m) > 0.5 ) {
				ROILIST[m][(int) THD_get_voxel(REF,i,m)]=1;
			}

	// So, all M rois per brik are now marked.  Go through them,
	// condense them into a list by array indices 0..M-1.  NB, in this
	// case, the actual *value* of the ROI label might be >M.
	// The value of M is then stored for real in NUMROI.
	for( m=0 ; m<Nbrik ; m++ ) {
		j=1;
		for( i=1 ; i<=INVROI[m] ; i++ ){
			if( ROILIST[m][i] == 1 ){
				ROILIST[m][j]=i; 
				INVLIST[m][i]=j;
				j++;
			}
		}

		if(INVROI[m] < j-1) // should never happen...
			ERROR_exit("Problem with ROI labels! Badness in reading/counting.");
		
		// reset value with real total number, not just max label
		NUMROI[m] = j-1; 
	}

	RETURN(1);
};


/*
  check a given voxel about whether the track running through it is in
  an entered NOT-mask
*/
int CheckNotMask(int id, int br, short **amask, int AO){
	int out=0;
	
	if(AO){ // N_nets==NOTMASK
		if(amask[id][br])
			out=1;
	}
	else // single NOTMASK for all
		if(amask[id][0])
			out=1;
	
	return out;
}


/*
  varied length version

 */
int ScoreTrackGrid_M( float ***PG, int idx, int h, int C,
                      THD_3dim_dataset **inset, int bot, int top)
{
   int i,j=1;
	float READS_fl=0;
	
   PG[h][C][0]+= 1.0; // N_vox

   for( i=bot ; i<top ; i++ ) {
      
      //mu and std of FA,MD,RD,L1
      PG[h][C][j]+= 
         THD_get_voxel(inset[i],idx,0);
      PG[h][C][j+1]+= 
         (float) pow(THD_get_voxel(inset[i],idx,0),2);
      j+=2;
   }
	
	RETURN(0);
}



/* OLD
int ScoreTrackGrid_M( float ****PG,int idx, int h, int C, int B, 
                      THD_3dim_dataset **inset, int bot, int top,
                      int DTI)
{
   int i,j=1;
	float READS_fl=0;
	
   PG[h][C][B][0]+= 1.0; // N_vox

   for( i=bot ; i<top ; i++ ) {
      
      //mu and std of FA,MD,RD,L1
      PG[h][C][B][j]+= 
         THD_get_voxel(inset[i],idx,0);
      PG[h][C][B][j+1]+= 
         (float) pow(THD_get_voxel(inset[i],idx,0),2);
      j+=2;
   }
   
   if(DTI) { // RD = 0.5 * ( 3*MD - L1 )
      READS_fl = 0.5*(3.0*THD_get_voxel(inset[i-2],idx,0)-
                      THD_get_voxel(inset[i-1],idx,0));
      PG[h][C][B][j]+= 
         READS_fl;
      PG[h][C][B][j+1]+= 
         (float) pow(READS_fl,2);
   }
	
	RETURN(1);
}*/



// *******************************************************************

// basic format for writing out tracking results of 3dTrackID:
// + a file of individual ROI intercepts
// + a file of the pairwise combinations
int WriteBasicProbFiles(int N_nets, int Ndata, int Nvox, 
								char *prefix,THD_3dim_dataset *insetFA,
								int *TV_switch,char *voxel_order,int *NROI,
								int ***NETROI,int ***mskd,int ***INDEX2,int *Dim,
								THD_3dim_dataset *dsetn,int argc, char *argv[],
                        char ***ROI_STR_LABS, int NameLabelsOut,
                        Dtable *roi_table,
                        int **roi_labs, int PAIR_POWERON,
                        int NIFTI_OUT)
{

	int i,j,k,bb,hh,kk,rr,idx;
	char **prefix_netmap=NULL;
	char **prefix_netmap2=NULL;
	char **prefix_dtable=NULL;
   char *Dtable_str=NULL;
   Dtable *new_dt=NULL;
   char mini[50];
	THD_3dim_dataset *networkMAPS=NULL,*networkMAPS2=NULL;
   char bric_labs[300];
   int idx3;
   FILE *fout1;
   int MAXOVERLAP = 2;
   int i1,i2;
   char EleNameStr[128];
   int maxROInum, MAXOVERLAP_VAL;
   int MULTI_ROI = 0;

	// ****** alloc'ing
	prefix_netmap = calloc( N_nets,sizeof(prefix_netmap));  
	for(i=0 ; i<N_nets ; i++) 
		prefix_netmap[i] = calloc( 300,sizeof(char)); 
	prefix_netmap2 = calloc( N_nets,sizeof(prefix_netmap2));  
	for(i=0 ; i<N_nets ; i++) 
		prefix_netmap2[i] = calloc( 300,sizeof(char)); 
	prefix_dtable = calloc( N_nets,sizeof(prefix_dtable));  
	for(i=0 ; i<N_nets ; i++) 
		prefix_dtable[i] = calloc( 300,sizeof(char)); 

	if( ( prefix_netmap== NULL) || ( prefix_netmap2== NULL)
       || ( prefix_dtable== NULL) ) {
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(122);
	}

	// ****** calc/do
	for( hh=0 ; hh<N_nets ; hh++) {

      // Oct. 2014: if only 1 ROI in set, then just output 0th brick
      // (because others add no information)
      // MULTI_ROI acts as both a switch and a number
      MULTI_ROI = ( NROI[hh] > 1 ) ? 1 : 0;

      if( NIFTI_OUT )
         sprintf(prefix_netmap[hh],"%s_%03d_PAIRMAP.nii.gz",prefix,hh); 
      else
         sprintf(prefix_netmap[hh],"%s_%03d_PAIRMAP",prefix,hh); 

      // Sept 2014
      sprintf(prefix_dtable[hh],"%s_%03d_PAIRMAP.niml.lt",prefix, hh); 
      if( roi_table ) {
         // copy dtable
         Dtable_str = Dtable_to_nimlstring(roi_table, "VALUE_LABEL_DTABLE");
         new_dt = Dtable_from_nimlstring(Dtable_str);
         free(Dtable_str); Dtable_str=NULL;
      }
      else {
         new_dt = new_Dtable( NROI[hh] );
      }
      // from either above cases, we have either a non-empty or empty
      // starter; in either case, check through all our ROIs to add in
      // more as necessary.
      for( bb=1 ; bb<=NROI[hh] ; bb++) {
         snprintf(mini, 50, "%d", roi_labs[hh][bb]);
         if(!(findin_Dtable_a( mini, new_dt ))) {
            addto_Dtable(mini, ROI_STR_LABS[hh][bb], new_dt );
         }
      }

         
		// just get one of right dimensions!
		networkMAPS = EDIT_empty_copy( insetFA ) ; 
      // Oct. 2014: if only 1 ROI in set, then just output 0th brick
      // (because others add no information)
      if( MULTI_ROI )
         EDIT_add_bricklist(networkMAPS ,
                            NROI[hh], NULL , NULL , NULL );
      if( PAIR_POWERON )
         EDIT_dset_items(networkMAPS,
                         ADN_datum_all , MRI_float , 
                         ADN_none ) ;
      else
         EDIT_dset_items(networkMAPS,
                         ADN_datum_all , MRI_short , 
                         ADN_none ) ;

      if( NIFTI_OUT )
         sprintf(prefix_netmap2[hh],"%s_%03d_INDIMAP.nii.gz",prefix,hh); 
      else
         sprintf(prefix_netmap2[hh],"%s_%03d_INDIMAP",prefix,hh); 

		// just get one of right dimensions!
		networkMAPS2 = EDIT_empty_copy( insetFA ) ; 
      // Oct. 2014: if only 1 ROI in set, then just output 0th brick
      // (because others add no information)
      if( MULTI_ROI )
         EDIT_add_bricklist(networkMAPS2 ,
                            NROI[hh], NULL , NULL , NULL );
		EDIT_dset_items(networkMAPS2,
							 ADN_datum_all , MRI_float , 
							 ADN_none ) ;

      float **temp_arrFL=NULL;
      short int **temp_arrSH=NULL;
      float **temp_arr2=NULL;

      int **intersec=NULL;

		// first array for all tracks, 2nd for paired ones.
		// still just need one set of matrices output
      if( MULTI_ROI ) {
         if( PAIR_POWERON ) {
            temp_arrFL = calloc( (NROI[hh]+MULTI_ROI),sizeof(temp_arrFL));
            for(i=0 ; i<(NROI[hh]+MULTI_ROI) ; i++) 
               temp_arrFL[i] = calloc( Nvox,sizeof(float)); 
         }
         else {
            temp_arrSH = calloc( (NROI[hh]+MULTI_ROI),sizeof(temp_arrSH)); 
            for(i=0 ; i<(NROI[hh]+MULTI_ROI) ; i++) 
               temp_arrSH[i] = calloc( Nvox,sizeof(short int)); 
         }
      }

		temp_arr2 = calloc( (NROI[hh]+MULTI_ROI),sizeof(temp_arr2));
		for(i=0 ; i<(NROI[hh]+MULTI_ROI) ; i++) 
			temp_arr2[i] = calloc( Nvox,sizeof(float)); 

		if( ( temp_arr2 == NULL) ) {
			fprintf(stderr, "\n\n MemAlloc failure.\n\n");
			exit(122);
		}

      // use this per vox
      if( MULTI_ROI ){
         intersec = calloc( (NROI[hh]+MULTI_ROI),sizeof(intersec)); 
         for(i=0 ; i<(NROI[hh]+MULTI_ROI) ; i++) 
            intersec[i] = calloc(MAXOVERLAP+1,sizeof( int )); 
         
         if( PAIR_POWERON ) {      
            if( temp_arrFL == NULL) {
               fprintf(stderr, "\n\n MemAlloc failure.\n\n");
               exit(122);
            }
         }
         else {
            if( temp_arrSH == NULL) {
               fprintf(stderr, "\n\n MemAlloc failure.\n\n");
               exit(122);
            }
         }
         if(  ( intersec == NULL)) {
            fprintf(stderr, "\n\n MemAlloc failure.\n\n");
            exit(122);
         }
      }


      // for string label outputs
		maxROInum = roi_labs[hh][NROI[hh]];
      // should never happen normally -- even with _M_<->A<->B
      MAXOVERLAP_VAL = maxROInum*maxROInum + 1;

      idx=0;
      for( k=0 ; k<Dim[2] ; k++ ) 
         for( j=0 ; j<Dim[1] ; j++ ) 
            for( i=0 ; i<Dim[0] ; i++ ) {
               // allow for more than one `connector' tract
               if(mskd[i][j][k]) {
                  for( bb=1 ; bb<=NROI[hh] ; bb++) 
                     for( rr=bb ; rr<=NROI[hh] ; rr++) {
                        idx3 = MatrInd_to_FlatUHT(bb-1,rr-1,NROI[hh]);
                        if(NETROI[INDEX2[i][j][k]][hh][idx3]>0) {
                           // store connectors
                           if( MULTI_ROI ){
                              if(bb != rr){
                                 if(PAIR_POWERON)
                                    temp_arrFL[0][idx] = 1.; 
                                 else
                                    temp_arrSH[0][idx] = (short) 1; 
                              }
                           }
									
									// store tracks through any ROI
									temp_arr2[0][idx] = (float)
										NETROI[INDEX2[i][j][k]][hh][idx3]; 
									
									
                           if( MULTI_ROI ) { 
                              // then add value if overlap
                              if(bb != rr) {
                                 if( PAIR_POWERON ) {// OLD
                                    // unique
                                    temp_arrFL[bb][idx]+=(float) pow(2,rr);
                                    temp_arrFL[rr][idx]+=(float) pow(2,bb);
                                 }
                                 else{ 
                                    temp_arrSH[bb][idx]+= (short) roi_labs[hh][rr];
                                    temp_arrSH[rr][idx]+= (short) roi_labs[hh][bb];
                                    intersec[bb][0]+= 1;
                                    intersec[rr][0]+= 1;
                                    // keep track of which ones get hit,
                                    // less than the value
                                    if( intersec[bb][0]<=MAXOVERLAP )
                                       intersec[bb][intersec[bb][0]] = rr;
                                    if( intersec[rr][0]<=MAXOVERLAP )
                                       intersec[rr][intersec[rr][0]] = bb;
                                 }
                              }
                              
                              
                              if(bb != rr) {
                                 temp_arr2[bb][idx] = (float)
                                    NETROI[INDEX2[i][j][k]][hh][idx3];
                                 temp_arr2[rr][idx] = (float)
                                    NETROI[INDEX2[i][j][k]][hh][idx3];
                              }
                              else
                                 temp_arr2[bb][idx] = (float)
                                    NETROI[INDEX2[i][j][k]][hh][idx3];
                           }
                           
								}
                     }
                  // continuation of labelling functions for this voxel
                  if( MULTI_ROI )
                     for( bb=1 ; bb<=NROI[hh] ; bb++) {
                        
                        if( intersec[bb][0] == 1 ) { // already tabled, ->erase
                           intersec[bb][0] = intersec[bb][1] = 0;
                        }
                        else if ( intersec[bb][0] == 2 ){ // check labeltable
                           
                           i1 = roi_labs[hh][intersec[bb][1]];
                           i2 = roi_labs[hh][intersec[bb][2]];
                           temp_arrSH[bb][idx] = 
                              MatrInd_to_FlatUHT_DIAG_P1( i1-1, 
                                                          i2-1, 
                                                          maxROInum);
                           snprintf(mini, 50, "%d", (int) temp_arrSH[bb][idx]);
                           
                           if(!(findin_Dtable_a( mini, new_dt ))) {
                              snprintf( EleNameStr, 128, "%s<->%s",
                                        ROI_STR_LABS[hh][intersec[bb][1]], 
                                        ROI_STR_LABS[hh][intersec[bb][2]]);
                              addto_Dtable(mini, EleNameStr, new_dt );
                           }
                           intersec[bb][0]=intersec[bb][1]=intersec[bb][2]=0;
                           
                        }
                        else if( intersec[bb][0] ) { // 'multi' + 2 label names
                           
                           i1 = roi_labs[hh][intersec[bb][1]];
                           i2 = roi_labs[hh][intersec[bb][2]];
                           // use the multi function now.
                           temp_arrSH[bb][idx] = 
                              MatrInd_to_FlatUHT_DIAG_M( i1-1, 
                                                         i2-1, 
                                                         maxROInum);
                           snprintf(mini, 50, "%d", (int) temp_arrSH[bb][idx]);
                           
                           if(!(findin_Dtable_a( mini, new_dt ))) {
                              snprintf( EleNameStr, 128, "%s<->%s<->%s",
                                        "_M_",
                                        ROI_STR_LABS[hh][intersec[bb][1]], 
                                        ROI_STR_LABS[hh][intersec[bb][2]]);
                              addto_Dtable(mini, EleNameStr, new_dt );
                           }
                           intersec[bb][0]=intersec[bb][1]=intersec[bb][2]=0;
                           
                        }
                        
                     }
               }
               idx+=1;
            }
      
      
      // FIRST THE PAIR CONNECTORS
      if ( MULTI_ROI ) {
         if( PAIR_POWERON ) {// OLD
            EDIT_substitute_brick(networkMAPS, 0, MRI_float, temp_arrFL[0]);
            temp_arrFL[0]=NULL;
         }
         else {
            EDIT_substitute_brick(networkMAPS, 0, MRI_short, temp_arrSH[0]);
            temp_arrSH[0]=NULL;
         }
      }

      // THEN THE INDIVID TRACKS
		EDIT_substitute_brick(networkMAPS2, 0, MRI_float, temp_arr2[0]);
		temp_arr2[0]=NULL;

      if( MULTI_ROI )
         for( bb=1 ; bb<=NROI[hh] ; bb++) {
            if( PAIR_POWERON ) {// OLD
               EDIT_substitute_brick(networkMAPS,bb,MRI_float,temp_arrFL[bb]);
               temp_arrFL[bb]=NULL; // to not get into trouble...
            }
            else {
               EDIT_substitute_brick(networkMAPS,bb,MRI_short,temp_arrSH[bb]);
               temp_arrSH[bb]=NULL; // to not get into trouble...
            }
            
            // Sept 2014: updating labelling
            sprintf(bric_labs,"AND_%s",ROI_STR_LABS[hh][bb]);
            EDIT_BRICK_LABEL(networkMAPS, bb, bric_labs); // labels, PAIR
            
            EDIT_substitute_brick(networkMAPS2, bb, MRI_float, temp_arr2[bb]);
            sprintf(bric_labs,"OR_%s",ROI_STR_LABS[hh][bb]);
            EDIT_BRICK_LABEL(networkMAPS2, bb, bric_labs); // labels, INDI
            
            temp_arr2[bb]=NULL; // to not get into trouble...
         } 
      
		if(TV_switch[0] || TV_switch[1] || TV_switch[2]) {
         if( MULTI_ROI ){
            dsetn = r_new_resam_dset(networkMAPS, NULL, 0.0, 0.0, 0.0,
                                     voxel_order, RESAM_NN_TYPE, 
                                     NULL, 1, 0);
            DSET_delete(networkMAPS); 
            networkMAPS=dsetn;
            dsetn=NULL;
         }

         dsetn = r_new_resam_dset(networkMAPS2, NULL, 0.0, 0.0, 0.0,
                                  voxel_order, RESAM_NN_TYPE, 
                                  NULL, 1, 0);
         DSET_delete(networkMAPS2); 
         networkMAPS2=dsetn;
         dsetn=NULL;
      }

      if( MULTI_ROI ) {
         EDIT_dset_items(networkMAPS,
                         ADN_prefix    , prefix_netmap[hh] ,
                         ADN_brick_label_one , "AND_all",
                         ADN_none ) ;
         
         // Sept 2014
         if( !PAIR_POWERON ) {
            Dtable_str = Dtable_to_nimlstring(new_dt, "VALUE_LABEL_DTABLE");
            destroy_Dtable(new_dt); new_dt = NULL;
            THD_set_string_atr( networkMAPS->dblk , 
                                "VALUE_LABEL_DTABLE" , Dtable_str);
            
            if( (fout1 = fopen(prefix_dtable[hh], "w")) == NULL) {
               fprintf(stderr, "Error opening file %s.",prefix_dtable[hh]);
               exit(19);
            }
            fprintf(fout1,"%s",Dtable_str);
            fclose(fout1);
            
            free(Dtable_str); Dtable_str = NULL;
         }
         
         THD_load_statistics(networkMAPS);
         if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(networkMAPS)) )
            ERROR_exit("Can't overwrite existing dataset '%s'",
                       DSET_HEADNAME(networkMAPS));
         tross_Make_History("3dTrackID", argc, argv, networkMAPS);
         THD_write_3dim_dataset(NULL, NULL, networkMAPS, True);
         DSET_delete(networkMAPS); 
      
         if(PAIR_POWERON){
            for( i=0 ; i<NROI[hh]+MULTI_ROI ; i++) // free all
               free(temp_arrFL[i]);
            free(temp_arrFL);
         }
         else{
            for( i=0 ; i<NROI[hh]+MULTI_ROI ; i++) // free all
               free(temp_arrSH[i]);
            free(temp_arrSH);
         }
         for( i=0 ; i<NROI[hh]+MULTI_ROI ; i++) // free all
            free(intersec[i]);
         free(intersec);
         intersec = NULL;
      }
      
		EDIT_dset_items(networkMAPS2,
							 ADN_prefix    , prefix_netmap2[hh] ,
							 ADN_brick_label_one , "OR_all",
							 ADN_none ) ;
		THD_load_statistics(networkMAPS2);
		if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(networkMAPS2)) )
			ERROR_exit("Can't overwrite existing dataset '%s'",
						  DSET_HEADNAME(networkMAPS2));
		tross_Make_History("3dTrackID", argc, argv, networkMAPS2);
		THD_write_3dim_dataset(NULL, NULL, networkMAPS2, True);
		DSET_delete(networkMAPS2); 
		for( i=0 ; i<NROI[hh]+MULTI_ROI ; i++) // free all
			free(temp_arr2[i]);
		free(temp_arr2);
      
	}
   
	// ****** freeing  **********
	
   if(MULTI_ROI){
      for( i=0 ; i<N_nets ; i++) {
         free(prefix_netmap[i]); 
      }
      free(prefix_netmap);
      free(networkMAPS);
   }
	for( i=0 ; i<N_nets ; i++) {
		free(prefix_dtable[i]);  // free in all cases because where initialized
		free(prefix_netmap2[i]); 
	}
	free(prefix_netmap2);
   free(prefix_dtable); 
	free(networkMAPS2);
   
	RETURN(1);
}

// second format for writing out tracking results of 3dTrackID:
// each pairwise map in own file
// (will be in new directory with prefix name)
int WriteIndivProbFiles(int N_nets, int Ndata, int Nvox, int **Prob_grid,
								char *prefix,THD_3dim_dataset *insetFA,
								int *TV_switch,char *voxel_order,int *NROI,
								int ***NETROI,int ***mskd,int ***INDEX2,int *Dim,
								THD_3dim_dataset *dsetn,int argc, char *argv[],
								float ***Param_grid, int DUMP_TYPE,
								int DUMP_ORIG_LABS, int **ROI_LABELS, int POST_IT,
                        char ***ROI_STR_LAB, int NameLabelsOut,
                        int NIFTI_OUT)
{

	int i,j,k,bb,hh,rr,ii,jj,kk;
	int idx,idx2,count;
	char ***prefix_netmap=NULL;
	char ***prefix_dump=NULL;
	THD_3dim_dataset *networkMAPS=NULL;
	int *N_totpair=NULL;
	int sum_pairs=0;
	FILE *fout;
   int idx3;
   int OUT_FLOAT_MAP = 0;
   //char str1[128]={""}, str2[128]={""};
   //char *str_lab1=NULL, *str_lab2=NULL;


   if( POST_IT || ( DUMP_TYPE==4) )
      OUT_FLOAT_MAP = 1;
   
   
	N_totpair = (int *)calloc(N_nets, sizeof(int)); 
   //   str_lab1 = (char *)calloc(100, sizeof(char));
   //str_lab2 = (char *)calloc(100, sizeof(char));

	// find out how many networks we'll be outputting.
	// this means going through UHT part of probgrid, looking for nonzeros
	for( k=0 ; k<N_nets ; k++) 
		for( i=0 ; i<NROI[k] ; i++ ) 
			for( j=i ; j<NROI[k] ; j++ ) // include diags
				if(Prob_grid[k][MatrInd_to_FlatUHT(i,j,NROI[k])]>0){
					N_totpair[k]+=1;
					sum_pairs+=1;
				}

	if( sum_pairs==0 )
		INFO_message("No pairs in any network to output.");
	else {

		INFO_message("Writing out individual files to: %s/NET_ ...",prefix);


		// ****** alloc'ing
		prefix_netmap = (char ***) calloc( N_nets, sizeof(char **) );
		for ( i = 0 ; i < N_nets ; i++ ) 
			prefix_netmap[i] = (char **) calloc( N_totpair[i], sizeof(char *) );
		for ( i = 0 ; i < N_nets ; i++ ) 
			for ( j = 0 ; j < N_totpair[i] ; j++ ) 
				prefix_netmap[i][j] = (char *) calloc( 300, sizeof(char) );
		prefix_dump = (char ***) calloc( N_nets, sizeof(char **) );
		for ( i = 0 ; i < N_nets ; i++ ) 
			prefix_dump[i] = (char **) calloc( N_totpair[i], sizeof(char *) );
		for ( i = 0 ; i < N_nets ; i++ ) 
			for ( j = 0 ; j < N_totpair[i] ; j++ ) 
				prefix_dump[i][j] = (char *) calloc( 300, sizeof(char) );

		if( ( prefix_netmap== NULL) || ( prefix_dump== NULL)) {
			fprintf(stderr, "\n\n MemAlloc failure.\n\n");
			exit(122);
		}
		
      mkdir(prefix, 0777);
		// ****** calc/do, loop through networks
		for( hh=0 ; hh<N_nets ; hh++) {
			count=0;
			for( i=0 ; i<NROI[hh] ; i++ ) 
				for( j=i ; j<NROI[hh] ; j++ ) {// include diags
               idx3 = MatrInd_to_FlatUHT(i,j,NROI[hh]);
					if(Prob_grid[hh][idx3]>0) {
                  if( ROI_STR_LAB && NameLabelsOut ) {
                     if( NIFTI_OUT )
                        snprintf(prefix_netmap[hh][count], 300,
                                 "%s/NET_%03d_ROI_%s__%s.nii.gz", prefix, hh,
                                 ROI_STR_LAB[hh][i+1], ROI_STR_LAB[hh][j+1]); 
                     else
                        snprintf(prefix_netmap[hh][count], 300,
                                 "%s/NET_%03d_ROI_%s__%s", prefix, hh,
                                 ROI_STR_LAB[hh][i+1], ROI_STR_LAB[hh][j+1]); 
                  }
						else if(!DUMP_ORIG_LABS) {
                     if( NIFTI_OUT )
                        snprintf(prefix_netmap[hh][count], 300,
                                 "%s/NET_%03d_ROI_%03d__%03d.nii.gz",
                                 prefix,hh,i+1,j+1); 
                     else
                        snprintf(prefix_netmap[hh][count], 300,
                                 "%s/NET_%03d_ROI_%03d__%03d",
                                 prefix,hh,i+1,j+1); 
                  }
                  else{
                     if( NIFTI_OUT )
                        snprintf(prefix_netmap[hh][count], 300,
                                 "%s/NET_%03d_ROI_%03d__%03d.nii.gz",prefix,hh,
                                 ROI_LABELS[hh][i+1],ROI_LABELS[hh][j+1]); 
                     else
                        snprintf(prefix_netmap[hh][count], 300,
                                 "%s/NET_%03d_ROI_%03d__%03d",prefix,hh,
                                 ROI_LABELS[hh][i+1],ROI_LABELS[hh][j+1]); 
                  }




						// single brik, byte map
						networkMAPS = EDIT_empty_copy( insetFA ) ; 
						EDIT_dset_items(networkMAPS,
											 ADN_datum_all , MRI_byte , 
											 ADN_none ) ;
						
						sprintf(prefix_dump[hh][count],
								  "%s/NET_%03d_ROI_%03d__%03d.dump",
								  prefix,hh,i+1,j+1); 

                  float *temp_arr_FL=NULL;
                  byte *temp_arr_BY=NULL;
		
						// first array for all tracks, 2nd for paired ones.
						// still just need one set of matrices output
                  if(OUT_FLOAT_MAP) {
                    // will be single brik output
                    temp_arr_FL = (float *)calloc(Nvox, sizeof(float));
                    if(( temp_arr_FL== NULL)) {
                      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
                      exit(120);
                    }
                  }
                  else {
                    // will be single brik output
                    temp_arr_BY = (byte *)calloc(Nvox, sizeof(byte));
                    if(( temp_arr_BY== NULL)) {
                      fprintf(stderr, "\n\n MemAlloc failure.\n\n");
                      exit(121);
                    }
                  }
                  
                  int **temp_arr2=NULL;
                  // we know how many vox per WM ROI, alloc that much 
                  // for the to-be-dumped mask
                  temp_arr2=calloc(Param_grid[hh][idx3][0],
                                   sizeof(temp_arr2)); 
                  for(bb=0 ; bb<Param_grid[hh][idx3][0] ; bb++) 
                    temp_arr2[bb] = calloc(4,sizeof(int)); //x,y,z,1
                  
						if(( temp_arr2== NULL)) {
                    fprintf(stderr, "\n\n MemAlloc failure.\n\n");
                    exit(122);
						}
                  
						idx=0;
						idx2=0;
						for( kk=0 ; kk<Dim[2] ; kk++ ) 
							for( jj=0 ; jj<Dim[1] ; jj++ ) 
								for( ii=0 ; ii<Dim[0] ; ii++ ) {
									if(mskd[ii][jj][kk]) {
                              idx3 = MatrInd_to_FlatUHT(i,j,NROI[hh]);
										if(NETROI[INDEX2[ii][jj][kk]][hh][idx3]>0) {
                                // store locations
                                if(OUT_FLOAT_MAP)
                                  temp_arr_FL[idx] = (float) 
                                    NETROI[INDEX2[ii][jj][kk]][hh][idx3];
                                else
                                  temp_arr_BY[idx] = 1;
                                temp_arr2[idx2][0] = ii;
                                temp_arr2[idx2][1] = jj;
                                temp_arr2[idx2][2] = kk;
                                temp_arr2[idx2][3] = 1;
                                idx2++;
                              }
                           }
									idx++;
								}
                  
						if(idx2 != Param_grid[hh][idx3][0])
                    printf("ERROR IN COUNTING! Netw,ROI,ROI= (%d, %d, %d); "
                           "idx2 %d != %d paramgrid.\n",
                           hh,i,j,idx2,(int) Param_grid[hh][idx3][0]);
                  
                  if(OUT_FLOAT_MAP){
                    EDIT_substitute_brick(networkMAPS, 0, MRI_float, 
                                          temp_arr_FL);
                    temp_arr_FL=NULL; // to not get into trouble...
                  }
                  else{
                    EDIT_substitute_brick(networkMAPS, 0, MRI_byte, 
                                          temp_arr_BY);
                    temp_arr_BY=NULL; // to not get into trouble...
                  }

						if(TV_switch[0] || TV_switch[1] || TV_switch[2]) {
                    dsetn = r_new_resam_dset(networkMAPS, NULL, 0.0, 0.0, 0.0,
                                             voxel_order, RESAM_NN_TYPE, 
                                             NULL, 1, 0);
                    DSET_delete(networkMAPS); 
                    networkMAPS=dsetn;
                    dsetn=NULL;
                    
                    for( bb=0 ; bb<3 ; bb++ )
                      if(TV_switch[bb])
                        for( rr=0 ; rr<idx2 ; rr++)
                          temp_arr2[rr][bb] = Dim[bb]-1-temp_arr2[rr][bb];
                    
						}      
						EDIT_dset_items(networkMAPS,
											 ADN_prefix , prefix_netmap[hh][count] ,
											 ADN_brick_label_one , "ROI_pair",
											 ADN_none ) ;
						
						// output AFNI if D_T=2 or 3 -> or now 4
						if(DUMP_TYPE>1) {
							THD_load_statistics(networkMAPS);
							if( !THD_ok_overwrite() && 
								 THD_is_ondisk(DSET_HEADNAME(networkMAPS)) )
								ERROR_exit("Can't overwrite existing dataset '%s'",
											  DSET_HEADNAME(networkMAPS));
							tross_Make_History("3dTrackID", argc, argv, networkMAPS);
							THD_write_3dim_dataset(NULL, NULL, networkMAPS, True);
						}

						DSET_delete(networkMAPS); 
                  if(OUT_FLOAT_MAP)
                    free(temp_arr_FL);
                  else
                    free(temp_arr_BY);
                  
						// THEN THE DUMP TEXT FILE
						// if D_T = 1 or 3
						if( (DUMP_TYPE==1) || (DUMP_TYPE==3) ) {
							if( (fout = fopen(prefix_dump[hh][count], "w")) == NULL) {
								fprintf(stderr, "Error opening file %s.",
										  prefix_dump[hh][count]);
								exit(139);
							}
							
							for( bb=0 ; bb<idx2 ; bb++ ){
								for( rr=0 ; rr<4 ; rr++ )
								fprintf(fout,"%d\t",temp_arr2[bb][rr]);
								fprintf(fout,"\n");
							}
							
							fclose(fout);
						}
						for( bb=0 ; bb<(int) Param_grid[hh][idx3][0] ; bb++) // free all
							free(temp_arr2[bb]);
						free(temp_arr2);
					}
            }
			count++;
		} // end of network loop
		
		// ****** freeing  **********
		for ( bb = 0 ; bb < N_nets ; bb++ ) 
			for ( rr = 0 ; rr < N_totpair[bb] ; rr++ ) {
				free(prefix_dump[bb][rr]);
				free(prefix_netmap[bb][rr]);
			}
		for ( bb = 0 ; bb < N_nets ; bb++ ) {
			free(prefix_dump[bb]);
			free(prefix_netmap[bb]);
		}
		free(prefix_dump);
		free(prefix_netmap);
		
		free(networkMAPS);
	}
	
	free(N_totpair);
   /*if(str_lab1) {
      str_lab1 = NULL;
      free(str_lab1);
   }
	   if(str_lab2) {
      str_lab2 = NULL;
      free(str_lab2);
      }*/

	RETURN(1);
}

int Setup_Labels_Indices_Unc_M_both(int *Dim, int ***mskd, int ***INDEX, 
                                    int ***INDEX2, float **UNC,
                                    float **coorded, float **copy_coorded, 
                                    THD_3dim_dataset *insetFA, 
                                    short *DirPerVox,
                                    int N_HAR,
                                    THD_3dim_dataset **insetV, 
                                    THD_3dim_dataset *insetUC,
                                    float unc_minei_std, float unc_minfa_std,
                                    int N_nets, int *NROI,
                                    THD_3dim_dataset *mset1, int **MAPROI, 
                                    int **INV_LABELS, int ***NETROI)
{
   
   int i,j,k,idx,m,mm,rr;
   int idw,n,nn;
   float tempvmagn;
   float temp1;
   
	// set up eigvecs in 3D coord sys,
	// mark off where ROIs are and keep index handy
	// also make uncert matrix, with min values of del ang and del FA
	for( k=0 ; k<Dim[2] ; k++ ) 
		for( j=0 ; j<Dim[1] ; j++ ) 
			for( i=0 ; i<Dim[0] ; i++ ) 
				if(mskd[i][j][k]) {
					idx = INDEX2[i][j][k];
               idw = INDEX[i][j][k];
               
					coorded[idx][0]=copy_coorded[idx][0]=
						THD_get_voxel(insetFA,idw,0); 

               // Uncertainty parts if 
               if( UNC != NULL ) {
                  if( N_HAR==0 ) { // DTI
                     // all from data set
                     //for( m=0 ; m<6 ; m++ ) 
                     //   UNC[idx][m] = THD_get_voxel(insetUC,idw,m);
                     temp1 = (THD_get_voxel(insetUC,idw,1) > unc_minei_std ) ?
                           THD_get_voxel(insetUC,idw,1) : unc_minei_std;
                     temp1 = pow(temp1,2);
                     temp1+= pow(THD_get_voxel(insetUC,idw,0),2);
                     UNC[idx][0] = sqrt(temp1); // delta e_{12}

                     temp1 = (THD_get_voxel(insetUC,idw,3) > unc_minei_std ) ?
                        THD_get_voxel(insetUC,idw,3) : unc_minei_std;
                     temp1 = pow(temp1,2);
                     temp1+= pow(THD_get_voxel(insetUC,idw,2),2);
                     UNC[idx][1] = sqrt(temp1); // delta e_{13}

                     // mean and std of delta FA
                     UNC[idx][2] = THD_get_voxel(insetUC,idw,4);
                     UNC[idx][3] = (THD_get_voxel(insetUC,idw,5) > unc_minfa_std ) ?
                        THD_get_voxel(insetUC,idw,5) : unc_minfa_std;

                  }
                  else { // HARDI
                     // all from data set
                     for( m=0 ; m<DirPerVox[idx] ; m++ ) {
                        UNC[idx][m] = 
                           (THD_get_voxel(insetUC,idw,m+1) > unc_minei_std ) ?
                           THD_get_voxel(insetUC,idw,m+1) : unc_minei_std;
                     }
                     UNC[idx][m] = 
                        (THD_get_voxel(insetUC,idw,0) > unc_minfa_std) ? 
                        THD_get_voxel(insetUC,idw,0) : unc_minfa_std;
                  }
                  
               }
               
               // setup all vectors
               for( n=0 ; n<DirPerVox[idx] ; n++ ) {
                  nn = 3*n;

                  for( m=0 ; m<3 ; m++ ) 
                     coorded[idx][nn+m+1]=copy_coorded[idx][nn+m+1]=
                        THD_get_voxel(insetV[n],idw,m);
                  
                  // apparently, some |V1| != 1... gotta fix
                  tempvmagn = sqrt(copy_coorded[idx][nn+1]*copy_coorded[idx][nn+1]+
                                   copy_coorded[idx][nn+2]*copy_coorded[idx][nn+2]+
                                   copy_coorded[idx][nn+3]*copy_coorded[idx][nn+3]);
                  //					if(tempvmagn<0.99) 
                  if(tempvmagn>0)
                     for( m=1 ; m<4 ; m++ ) {
                        copy_coorded[idx][nn+m]/=tempvmagn;
                        coorded[idx][nn+m]/=tempvmagn;
                     }
               }
               
					for( m=0 ; m<N_nets ; m++ ) {
						// allow indentification by index --
						// now, since we allow non-consecutive ROI labels,
						// just use the compacted list numbers via INV_LABELS
						if( THD_get_voxel(mset1, idw, m)>0.5 )
							MAPROI[idx][m] = INV_LABELS[m][(int) THD_get_voxel(mset1,idw,m)];
						else if( THD_get_voxel(mset1, idw, m)<-0.5 )   
                     // silly, is zero anyways... can use this to set to neg mask
							MAPROI[idx][m] = -1;
                  
                  // ppppretty sure there's no need for this here->
                  // it's all zeros already
						// counter for number of kept tracks passing through
                  //rr = NROI[m]*(NROI[m]+1);
                  //rr/= 2;
						//for( mm=0 ; mm<rr ; mm++ )
                  //  NETROI[idx][m][mm]) = 0;
					}
				}
   
	RETURN(1);

}


int Setup_Ndir_per_vox(int N_HAR, int *Dim, int ***mskd,
                       int ***INDEX, 
                       int ***INDEX2,
                       THD_3dim_dataset **insetHARDIR,
                       short *DirPerVox)
{
   int i,j,k,idx,m,n;
   float tempvmagn;
   
	// set up eigvecs in 3D coord sys,
	// mark off where ROIs are and keep index handy
	// also make uncert matrix, with min values of del ang and del FA
	for( k=0 ; k<Dim[2] ; k++ ) 
		for( j=0 ; j<Dim[1] ; j++ ) 
			for( i=0 ; i<Dim[0] ; i++ ) 
				if(mskd[i][j][k]) {
					idx = INDEX2[i][j][k];
               for( n=0 ; n<N_HAR ; n++ ){
                  tempvmagn = 0;
                  for( m=0 ; m<3 ; m++ ) 
                     tempvmagn+=
                        THD_get_voxel(insetHARDIR[n],INDEX[i][j][k],m)*
                        THD_get_voxel(insetHARDIR[n],INDEX[i][j][k],m);
                  
                  if( tempvmagn > 0.01)
                     DirPerVox[idx]+=1;
                  else if( tempvmagn <0.00001 ) // i.e., zero
                     continue;
                  else { // awkwardly small.
                     INFO_message("Minor note: there is a tiny"
                                  " (magn < 0.1)"
                                  " vector in the %d-th direction set."
                                  "\n\t--> Will exclude that voxel"
                                  " from walkable mask-- recommend checking"
                                  " model fit.",n);
                     mskd[i][j][k] = 0;
                  }
               }
            }
   return 1;
}

/*
  For Monte Carlo iterations, perturb values in mask
 */ 
int DTI_Perturb_M( int *Dim, int ***mskd, int ***INDEX, int ***INDEX2,
                   float **UNC, float **coorded, float **copy_coorded, 
                   gsl_rng *r, 
                   THD_3dim_dataset **insetV)
{
   
   int i,j,k,m,idx,idw;
   float tempvmagn,testang,w2,w3;
   float tempv[3];

   // have already got `del theta' for angles in UNC!

   for( k=0 ; k<Dim[2] ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            idx = INDEX2[i][j][k];
            idw = INDEX[i][j][k];
            // only do region in brain mask
            if(mskd[i][j][k]) {
               
               // these are weights determined by rotation angle,
               // (prob. determined by jackknifing with 3dDWUncert)
               // each tips in the +/- direc toward/away from each evec 
               // by averaging and that's why tan of angle is taken
               //@@@
               testang = GSL_RAN(r,1.0)*UNC[idx][0];
               w2 = tan(testang); 

               testang = GSL_RAN(r,1.0)*UNC[idx][1];
               w3 = tan(testang);


               for( m=0 ; m<3 ; m++)
                  tempv[m] = coorded[idx][m+1] + 
                     w2*THD_get_voxel(insetV[1],idw,m) + 
                     w3*THD_get_voxel(insetV[2],idw,m);
               tempvmagn = sqrt(tempv[0]*tempv[0]+
                                tempv[1]*tempv[1]+tempv[2]*tempv[2]);
               
               for( m=0 ; m<3 ; m++)
                  copy_coorded[idx][m+1] = tempv[m]/tempvmagn;
               
               // apply bias and std of FA
               copy_coorded[idx][0] = coorded[idx][0] + UNC[idx][2] +
                  ( UNC[idx][3] * GSL_RAN(r,1.0) );
               
            }
         }

	RETURN(1);   
}


int HARDI_Perturb( int *Dim, int ***mskd, int ***INDEX, int ***INDEX2,
                   float **UNC, float **coorded, float **copy_coorded, 
                   gsl_rng *r, short *DirPerVox) 
{
   
   int i,j,k,m,idx,n,B,nn;
   float tempvmagn,testang,w2,w3;
   float for_pol, for_azim;
   float tempv[3];
   float rot[3][3];

   for( k=0 ; k<Dim[2] ; k++ ) 
      for( j=0 ; j<Dim[1] ; j++ ) 
         for( i=0 ; i<Dim[0] ; i++ ) {
            idx = INDEX2[i][j][k];
            // only do region in brain mask
            if(mskd[i][j][k]) {
               
               for( n=0 ; n<DirPerVox[idx] ; n++ ){
                  nn=3*n+1; // plus one b/c zeroth brick is FA
                  
                  // unit vect, randomly perturbed around (0,0,1)
                  for_pol =  GSL_RAN(r,1.0)*UNC[idx][n];
                  for_azim = TWOPI*gsl_rng_uniform (r); // rand in 2*pi*[0,1)
                  tempv[0] = tempv[1] = sin(for_pol);
                  tempv[0]*= cos(for_azim);
                  tempv[1]*= sin(for_azim);
                  tempv[2] = cos(for_pol); 
                  
                  for_pol = acos( coorded[idx][nn+2] ); // acos(z)
                  for_azim = atan2( coorded[idx][nn+1],coorded[idx][nn+0]); // atan(y,x)

                  // do rotation: select within func for which vec
                  // (nn) because 0th brick is now FA value
                  B = Two_DOF_Rot(tempv, copy_coorded[idx]+nn, 
                                  for_pol, for_azim, rot);

                  /* printf("\n%d DP = %f; for (%f, %f, %f) and (%f, %f, %f)",n,
                         copy_coorded[idx][nn+0]*coorded[idx][nn+0]+
                         copy_coorded[idx][nn+1]*coorded[idx][nn+1]+
                         copy_coorded[idx][nn+2]*coorded[idx][nn+2],
                         coorded[idx][nn+0],
                         coorded[idx][nn+1],
                         coorded[idx][nn+2],
                         copy_coorded[idx][nn+0],
                         copy_coorded[idx][nn+1],
                         copy_coorded[idx][nn+2]
                         );*/
               }

               // for FA; n should have correct value here...
               //for_pol = GSL_RAN(r,1.0)*UNC[idx][n];
               copy_coorded[idx][0] = coorded[idx][0] + 
                  GSL_RAN(r,1.0)*UNC[idx][n];
            }
         }
   
	RETURN(1);
}


int Two_DOF_Rot(float *X, float *Y, 
                double POL, double AZIM, float rot[3][3] )
{
   int i,j,k;
   float C0,C1,S0,S1;

   C0 = cos(POL);
   S0 = sin(POL);
   C1 = cos(AZIM);
   S1 = sin(AZIM);

   rot[0][0] = C0*C1;   rot[0][1] = -S1;   rot[0][2] = C1*S0;
   rot[1][0] = C0*S1;   rot[1][1] = C1;    rot[1][2] = S0*S1;
   rot[2][0] = -S0;     rot[2][1] = 0.;    rot[2][2] = C0;

   for( i=0 ; i<3 ; i++) 
      Y[i] = 0;

   for( i=0 ; i<3 ; i++) 
      for( j=0 ; j<3 ; j++) 
         Y[i]+= rot[i][j]*X[j];

	RETURN(1);
}


/*
  NEW NEW ONE.  Multi directionality.
 */

int TrackItP_NEW_M(int NHAR, short *DirPerVox, int SEL, float **CC, 
                   int *IND, float *PHYSIND, 
                   float *Edge, int *dim, float minFA, 
                   float maxAng, int arrMax, int **T, 
                   float **flT, int FB, float *physL,
                   int ***ID2) 
{
   int tracL = 0;  // trac length 
   float Iam0[3]; // 'physical' location
   float AA, BB; // mult by L: (1-f)/2 and (1+f)/2, resp., define diagonal
   int vsign[3]; // keep trac of vel component sign
   float test[3]; // use to see if we move diag or not
   int ord[3];
   float dotprod;
   int go[3]; // for walking through
   int win;
   float targedge[3]; // possible walls to aim for 
   float stest[3]; // compare s values-- get shortest 'time' to wall
   int m,n,tt;
   int walkback;
   float physdist = 0.0; // init dist walked is 0;
   float FF = 0.4143; //from: (sin(22.5*CONV)/sin((90-22.5)*CONV));
   float divid;
   int FULLSEL;
   float which_dp;

   ENTRY("TrackItP_NEW_M"); 
  
   AA = 0.5*(1.0-FF);
   BB = 0.5*(1.0+FF);

   // initial place in center of first volume
   for( n=0 ; n<3 ; n++) 
      Iam0[n] = PHYSIND[n];
   tt = ID2[IND[0]][IND[1]][IND[2]]; // tt changes when INDs do
   // init dotprod is ~unity
   dotprod = 0.999;
   // square we are 'in' which survived tests: keep trac of, and add
   // to temp list
   for( n=0 ; n<3 ; n++) {
      T[tracL][n] = IND[n];
      flT[tracL][n] = Iam0[n];
   }
   tracL+= 1; 

   // tracking!
   // conditions to stop are:  
   // + too long of a tract (most likely something wrong with alg,
   //   because max is large)
   // + FA drops below threshold
   // + angulation goes above threshold
   while( (tracL < arrMax) && (CC[tt][0] >= minFA) 
          && ( dotprod >= maxAng) ) {    

      // offset in arrays to select which vect because of having: 
      //    1) FA map as 0th brick in CC,
      //    2) possibly hardi/multi
      FULLSEL = 3*SEL+1;

      // go to nearest edge 
      for( n=0 ; n<3 ; n++) {
         go[n] = 0; // just resetting our direction to 
         // Designate up/down, L/R, forw/back, with FB value given before.
         // Use SEL->FULLSEL to select which vect
         if(CC[tt][FULLSEL+n]*FB >=0) {
            targedge[n] = (IND[n]+1)*Edge[n]; // physical units
            vsign[n] = 1;
         }
         else {
            targedge[n] = IND[n]*Edge[n];
            vsign[n] = -1;
         }
      }
  
    
      // calc 'param' to get to edge... 
      for( n=0 ; n<3 ; n++) {
         if( fabs(CC[tt][FULLSEL+n]) < EPS_V)
            divid = EPS_V*vsign[n];
         else
            divid = FB*CC[tt][FULLSEL+n];
         stest[n] = (targedge[n]-Iam0[n])/divid;
      }
      walkback=0; 
    
      // say, due to very small CC in opp direction, or trying to push
      // us back into previous 
      for( n=0 ; n<3 ; n++) 
         if( (stest[n]<0) )
            walkback =1;
    
    
      if(walkback==0) {
         for( n=0 ; n<3 ; n++) // try this config as initial guess
            ord[n] = n;
       
         if(stest[ ord[1] ]<stest[ ord[0] ]) { // switch
            ord[0] = 1; // these are known values of each...
            ord[1] = 0;
         }
         if(stest[ ord[2] ]<stest[ ord[0] ]) { // switch
            n = ord[0]; // save temp
            ord[0] = ord[2]; // overwrite
            ord[2] = n; // finish switch
         }
         if(stest[ ord[2] ]<stest[ ord[1] ]) { // switch
            n = ord[1];
            ord[1] = ord[2];
            ord[2] = n;
         }
       
         win = ord[0]; 
         go[ord[0]] = vsign[ord[0]];
       
         // winner is here; other 2 indices haven't changed, test them.
         test[ord[1]] = Iam0[ord[1]] + 
            stest[ord[0]]*FB*CC[tt][FULLSEL+ord[1]] - 
            (IND[ord[1]]*Edge[ord[1]]);
       
         if( ( (vsign[ord[1]]>0 ) && (test[ord[1]] > Edge[ord[1]]*BB) ) ||
             ( (vsign[ord[1]]<0 ) && (test[ord[1]] < Edge[ord[1]]*AA) ) ){ 
            // then test and see where it would end up
            test[ord[0]] = Iam0[ord[0]] + 
               stest[ord[1]]*FB*CC[tt][FULLSEL+ord[0]] -
               (IND[ ord[0]] + go[ord[0]])*Edge[ord[0]];
          
            if( ( (vsign[ord[0]]>0) && (test[ord[0]] < Edge[ord[0]]*AA) ) ||
                ( (vsign[ord[0]]<0) && (test[ord[0]] > Edge[ord[0]]*BB) ) ){
               go[ord[1]] = vsign[ord[1]]; // partially-'diagonal' route
               win = ord[1];
             
               // and only now, do we test for the other diagonal
               test[ord[2]] = Iam0[ord[2]] + 
                  stest[ord[0]]*FB*CC[tt][FULLSEL+ord[2]] - 
                  (IND[ord[2]]*Edge[ord[2]]);
             
               if(((vsign[ord[2]]>0 ) && (test[ord[2]] > Edge[ord[2]]*BB)) ||
                  ((vsign[ord[2]]<0 ) && (test[ord[2]] < Edge[ord[2]]*AA)) ){ 
                  test[ord[0]] = Iam0[ord[0]] + 
                     stest[ord[2]]*FB*CC[tt][FULLSEL+ord[0]] - 
                     (IND[ord[0]]+go[ord[0]])*Edge[ord[0]];
                  test[ord[1]] = Iam0[ord[1]] + 
                     stest[ord[2]]*FB*CC[tt][FULLSEL+ord[1]] - 
                     (IND[ord[1]] + go[ord[1]])*Edge[ord[1]];
                
                  // check both for diag-diag
                  if(((vsign[ord[0]]>0) && (test[ord[0]] < Edge[ord[0]]*AA)) ||
                     ((vsign[ord[0]]<0) && (test[ord[0]] > Edge[ord[0]]*BB)))
                     if(((vsign[ord[1]]>0) && 
                         (test[ord[1]] < Edge[ord[1]]*AA)) ||
                        ((vsign[ord[1]]<0) && 
                         (test[ord[1]] > Edge[ord[1]]*BB))){
                        go[ord[2]] = vsign[ord[2]]; // fully-'diagonal' route
                        win = ord[2];
                     }
               }
            }
         }
       
         // move to boundary of next square, updating square we are 'in'
         // with current eigenvec
         for( n=0 ; n<3 ; n++) // phys loc
            Iam0[n]+= stest[ win ]*FB*CC[tt][FULLSEL+n];
         for( n=0 ; n<3 ; n++) // update indices of square we're in
            IND[n] = IND[n]+go[n]; // will change after testing vals
       
         physdist+= stest[win];
       
         // one way we can stop is by trying to 'walk out' of the volume;
         // can check that here
         if((IND[0] < dim[0]) && (IND[1] < dim[1]) && (IND[2] < dim[2]) && 
            (IND[0] >= 0) && (IND[1] >= 0) && (IND[2] >= 0) ) { 
            tt = ID2[IND[0]][IND[1]][IND[2]];

            // dot prod for stopping cond (abs value)
            // check with current vec dotproducted with previous
            // ++ NOW also checking which vector to select if there are 
            // multi directions: largest |dotprod| means smallest angle
            which_dp = 0.;
            SEL = 0;
            for( m=0 ; m<DirPerVox[tt] ; m++) {
               dotprod = 0.;
               for( n=0 ; n<3 ; n++) 
                  dotprod+= CC[tt][3*m+1+n]*
                     FB*CC[ID2[IND[0]-go[0]][IND[1]-go[1]][IND[2]-go[2]]][FULLSEL+n]; 
               if (fabs(dotprod) > which_dp)
                  which_dp = fabs(dotprod) ;
                  SEL = m;
            }
          
            // because of ambiguity of direc/orient of evecs
            // and will be checked for criterion at start of next while loop
            // because we will keep moving in 'negative' orientation of evec
            if(dotprod<0) {
               dotprod*=-1.; 
               FB = -1; 
            }
            else
               FB = 1; // move along current orientation of next one
          
            // make sure we haven't been here before
            for( n=0 ; n<tracL ; n++)
               if( (IND[0]==T[n][0]) && (IND[1]==T[n][1]) && (IND[2]==T[n][2]) )
                  dotprod =-1.; 

            if(dotprod<0) // bad
               for( n=0 ; n<3 ; n++) {
                  T[tracL][n] = -1;
                  flT[tracL][n] = -1;
               }
            else // fine 
               for( n=0 ; n<3 ; n++) {
                  T[tracL][n] = IND[n];
                  flT[tracL][n] = Iam0[n];
               } 
            tracL+= 1; 
            
         }
         else {
            // to not try to access inaccessible value 
            // so we will exit tracking in this direction 
            // at start of next loop
            for( n=0 ; n<3 ; n++) {
               IND[n] = 0; 
               T[tracL][n] = -1;
               flT[tracL][n] = -1;
            }
            dotprod = -2.; 
            tt=0;
            tracL+= 1;
         }
      }
      else{
         dotprod = -3.;
         tt=0;
         for( n=0 ; n<3 ; n++) {
            T[tracL][n] = -1;
            flT[tracL][n] = -1;
         }
         tracL+= 1;
      }
      
   }
   
   if(dotprod>=0 &&  (CC[tt][0] >= minFA)){
      for( n=0 ; n<3 ; n++) {
         T[tracL][n] = -1;
         flT[tracL][n] = -1;
      }
      tracL+= 1;
   }
   if(tracL >= arrMax) //{
      WARNING_message("Err in data set or run conditions:\n"
                      "\t half-tract reached max arr len! Truncating it.\n");
      //exit(101);
   //   }
  
   physL[0] = physdist;
  
   RETURN(tracL); 
}


