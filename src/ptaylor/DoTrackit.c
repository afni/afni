#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>     // AFNIadd
#include <3ddata.h>     // AFNIadd
#include <TrackIO.h>
#include <DoTrackit.h>


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
	in ProbTrackID: store the values in param_grid which eventually
	become ROI statistics.
*/
int ScoreTrackGrid(float ****PG,int idx, int h, int C, int B, 
						 THD_3dim_dataset *FA, THD_3dim_dataset *MD, 
						 THD_3dim_dataset *L1)
{
	float READS_fl=0;
	
	//mu and std of FA,MD,RD,L1
	PG[h][C][B][0]+= 
		THD_get_voxel(FA,idx,0);
	PG[h][C][B][1]+= 
		(float) pow(THD_get_voxel(FA,idx,0),2);
	PG[h][C][B][2]+= 
		THD_get_voxel(MD,idx,0);
	PG[h][C][B][3]+= 
		(float) pow(THD_get_voxel(MD,idx,0),2);
	READS_fl = 0.5*(3.0*THD_get_voxel(MD,idx,0)-
						 THD_get_voxel(L1,idx,0));
	PG[h][C][B][4]+= 
		READS_fl;
	PG[h][C][B][5]+= 
		(float) pow(READS_fl,2);
	PG[h][C][B][6]+= 
		THD_get_voxel(L1,idx,0);
	PG[h][C][B][7]+= 
		(float) pow(THD_get_voxel(L1,idx,0),2);
	PG[h][C][B][8]+= 1.0;
	
	RETURN(1);
}






/*
  we have to check 'plus and minus' eigenvector directions, which is
  controlled with 'FB' variable/switch value; we won't keep rewriting
  eigenvector things
*/
int TrackIt(float ****CC, int *IND, float *PHYSIND, 
            float *Edge, int *dim, float minFA, 
            float maxAng, int arrMax, int **T, 
            float **flT, int FB, float *physL) 
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
  int n;
  int walkback;
  float physdist = 0.0; // init dist walked is 0;
  float FF = 0.4143; //from: (sin(22.5*CONV)/sin((90-22.5)*CONV));
  float divid;

  ENTRY("TrackIt"); 
  
  AA = 0.5*(1.0-FF);
  BB = 0.5*(1.0+FF);

  // initial place in center of first volume
  for( n=0 ; n<3 ; n++) 
    Iam0[n] = PHYSIND[n];

  // init dotprod is ~unity
  dotprod = 0.999;
  
  // tracking!
  // conditions to stop are:  
  // + too long of a tract (most likely something wrong with alg,
  //   because max is large)
  // + FA drops below threshold
  // + angulation goes above threshold
  while( (tracL < arrMax) && (CC[IND[0]][IND[1]][IND[2]][3] >= minFA) 
	 && ( dotprod >= maxAng) ) {    
    
    // square we are 'in' which survived tests: keep trac of, and add
    // to temp list
    for( n=0 ; n<3 ; n++) {
      T[tracL][n] = IND[n];
      flT[tracL][n] = (float) Iam0[n];
    }
    tracL+= 1; 
    
    // go to nearest edge 
    for( n=0 ; n<3 ; n++) {
      go[n] = 0; // just resetting our direction to 
      // designate up/down, L/R, forw/back, with FB value given before
      if(CC[ IND[0] ][ IND[1] ][ IND[2] ][n]*FB >=0) {
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
      if( fabs(CC[ IND[0] ][ IND[1] ][ IND[2] ][n]) < EPS_V)
        divid = EPS_V*vsign[n];
      else
        divid = FB*CC[ IND[0] ][ IND[1] ][ IND[2] ][n];
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
        stest[ord[0]]*FB*CC[IND[0]][IND[1]][IND[2]][ord[1]] - 
        (IND[ord[1]]*Edge[ord[1]]);
      
      if( ( (vsign[ord[1]]>0 ) && (test[ord[1]] > Edge[ord[1]]*BB) ) ||
          ( (vsign[ord[1]]<0 ) && (test[ord[1]] < Edge[ord[1]]*AA) ) ){ 
        // then test and see where it would end up
        test[ord[0]] = Iam0[ord[0]] + 
          stest[ord[1]]*FB*CC[IND[0]][IND[1]][IND[2]][ord[0]] -
          (IND[ ord[0]] + go[ord[0]])*Edge[ord[0]];
        
        if( ( (vsign[ord[0]]>0) && (test[ord[0]] < Edge[ord[0]]*AA) ) ||
            ( (vsign[ord[0]]<0) && (test[ord[0]] > Edge[ord[0]]*BB) ) ){
          go[ord[1]] = vsign[ord[1]]; // partially-'diagonal' route
          win = ord[1];
          
          // and only now, do we test for the other diagonal
          test[ord[2]] = Iam0[ord[2]] + 
            stest[ord[0]]*FB*CC[IND[0]][IND[1]][IND[2]][ord[2]] - 
            (IND[ord[2]]*Edge[ord[2]]);
          
          if(((vsign[ord[2]]>0 ) && (test[ord[2]] > Edge[ord[2]]*BB)) ||
             ((vsign[ord[2]]<0 ) && (test[ord[2]] < Edge[ord[2]]*AA)) ){ 
            test[ord[0]] = Iam0[ord[0]] + 
              stest[ord[2]]*FB*CC[IND[0]][IND[1]][IND[2]][ord[0]] - 
              (IND[ord[0]]+go[ord[0]])*Edge[ord[0]];
            test[ord[1]] = Iam0[ord[1]] + 
              stest[ord[2]]*FB*CC[IND[0]][IND[1]][IND[2]][ord[1]]- 
              (IND[ord[1]] + go[ord[1]])*Edge[ord[1]];
            
            // check both for diag-diag
            if(((vsign[ord[0]]>0) && (test[ord[0]] < Edge[ord[0]]*AA)) ||
               ((vsign[ord[0]]<0) && (test[ord[0]] > Edge[ord[0]]*BB)))
              if(((vsign[ord[1]]>0) && (test[ord[1]] < Edge[ord[1]]*AA)) ||
                 ((vsign[ord[1]]<0) && (test[ord[1]] > Edge[ord[1]]*BB))){
                go[ord[2]] = vsign[ord[2]]; // fully-'diagonal' route
                win = ord[2];
              }
          }
        }
      }
      
      // move to boundary of next square, updating square we are 'in'
      // with current eigenvec
      for( n=0 ; n<3 ; n++) // phys loc
        Iam0[n]+= stest[ win ]*FB*CC[ IND[0] ][ IND[1] ][ IND[2] ][n];
      for( n=0 ; n<3 ; n++) // update indices of square we're in
        IND[n] = IND[n]+go[n];
      
      physdist+= stest[win];
      
      
      // one way we can stop is by trying to 'walk out' of the volume;
      // can check that here
      if((IND[0] < dim[0]) && (IND[1] < dim[1]) && (IND[2] < dim[2]) && 
         (IND[0] >= 0) && (IND[1] >= 0) && (IND[2] >= 0) ) { 
	
        // dot prod for stopping cond (abs value)
        // check with current dotprod with previous
        dotprod = 0.;
        for( n=0 ; n<3 ; n++) 
          dotprod+= CC[IND[0]][IND[1]][IND[2]][n]*
            FB*CC[IND[0]-go[0]][IND[1]-go[1]][IND[2]-go[2]][n]; 
        
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
            dotprod =0.; 
      }
      else {
        // to not try to access inaccessible value 
        // so we will exit tracking in this direction 
        // at start of next loop
        for( n=0 ; n<3 ; n++) 
          IND[n] = 0; 
        dotprod = 0.; 
      }
    }
    else
      dotprod = 0.;
    
  }
  
  if(tracL >= arrMax) {
    fprintf (stderr,"\n\tErr in data set; or need longer max arr len!\n");
    exit(101);
  }
  
  physL[0] = physdist;
  
  RETURN(tracL); 
}



// closely related probabilistic version of Tracking
// just dimensions of CC have changed, and method of indexing...
int TrackItP(float **CC, int *IND, float *PHYSIND, 
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
  int n, tt;
  int walkback;
  float physdist = 0.0; // init dist walked is 0;
  float FF = 0.4143; //from: (sin(22.5*CONV)/sin((90-22.5)*CONV));
  float divid;


  ENTRY("TrackItP"); 
  
  AA = 0.5*(1.0-FF);
  BB = 0.5*(1.0+FF);

  // initial place in center of first volume
  for( n=0 ; n<3 ; n++) 
    Iam0[n] = PHYSIND[n];
  tt = ID2[IND[0]][IND[1]][IND[2]]; // tt changes when INDs do
  // init dotprod is ~unity
  dotprod = 0.999;
 
  // tracking!
  // conditions to stop are:  
  // + too long of a tract (most likely something wrong with alg,
  //   because max is large)
  // + FA drops below threshold
  // + angulation goes above threshold
  while( (tracL < arrMax) && (CC[tt][3] >= minFA) 
	 && ( dotprod >= maxAng) ) {    
    
    // square we are 'in' which survived tests: keep trac of, and add
    // to temp list
    for( n=0 ; n<3 ; n++) {
      T[tracL][n] = IND[n];
      flT[tracL][n] = (float) Iam0[n];
    }
    tracL+= 1; 
    
    // go to nearest edge 
    for( n=0 ; n<3 ; n++) {
      go[n] = 0; // just resetting our direction to 
      // designate up/down, L/R, forw/back, with FB value given before
      if(CC[tt][n]*FB >=0) {
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
      if( fabs(CC[tt][n]) < EPS_V)
	divid = EPS_V*vsign[n];
      else
	divid = FB*CC[tt][n];
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
	stest[ord[0]]*FB*CC[tt][ord[1]] - 
	(IND[ord[1]]*Edge[ord[1]]);

      if( ( (vsign[ord[1]]>0 ) && (test[ord[1]] > Edge[ord[1]]*BB) ) ||
	  ( (vsign[ord[1]]<0 ) && (test[ord[1]] < Edge[ord[1]]*AA) ) ){ 
	// then test and see where it would end up
	test[ord[0]] = Iam0[ord[0]] + 
	  stest[ord[1]]*FB*CC[tt][ord[0]] -
	  (IND[ ord[0]] + go[ord[0]])*Edge[ord[0]];
	
	if( ( (vsign[ord[0]]>0) && (test[ord[0]] < Edge[ord[0]]*AA) ) ||
	    ( (vsign[ord[0]]<0) && (test[ord[0]] > Edge[ord[0]]*BB) ) ){
	  go[ord[1]] = vsign[ord[1]]; // partially-'diagonal' route
	  win = ord[1];
	  
	  // and only now, do we test for the other diagonal
	  test[ord[2]] = Iam0[ord[2]] + 
	    stest[ord[0]]*FB*CC[tt][ord[2]] - 
	    (IND[ord[2]]*Edge[ord[2]]);

	  if(((vsign[ord[2]]>0 ) && (test[ord[2]] > Edge[ord[2]]*BB)) ||
	     ((vsign[ord[2]]<0 ) && (test[ord[2]] < Edge[ord[2]]*AA)) ){ 
	    test[ord[0]] = Iam0[ord[0]] + 
	      stest[ord[2]]*FB*CC[tt][ord[0]] - 
	      (IND[ord[0]]+go[ord[0]])*Edge[ord[0]];
	    test[ord[1]] = Iam0[ord[1]] + 
	      stest[ord[2]]*FB*CC[tt][ord[1]]- 
	      (IND[ord[1]] + go[ord[1]])*Edge[ord[1]];

	    // check both for diag-diag
	    if(((vsign[ord[0]]>0) && (test[ord[0]] < Edge[ord[0]]*AA)) ||
	       ((vsign[ord[0]]<0) && (test[ord[0]] > Edge[ord[0]]*BB)))
	      if(((vsign[ord[1]]>0) && (test[ord[1]] < Edge[ord[1]]*AA)) ||
		 ((vsign[ord[1]]<0) && (test[ord[1]] > Edge[ord[1]]*BB))){
		go[ord[2]] = vsign[ord[2]]; // fully-'diagonal' route
		win = ord[2];
	      }
	  }
	}
      }

      // move to boundary of next square, updating square we are 'in'
      // with current eigenvec
      for( n=0 ; n<3 ; n++) // phys loc
	Iam0[n]+= stest[ win ]*FB*CC[tt][n];
      for( n=0 ; n<3 ; n++) // update indices of square we're in
	IND[n] = IND[n]+go[n]; // will change after testing vals
      
      physdist+= stest[win];

      
      // one way we can stop is by trying to 'walk out' of the volume;
      // can check that here
      if((IND[0] < dim[0]) && (IND[1] < dim[1]) && (IND[2] < dim[2]) && 
	 (IND[0] >= 0) && (IND[1] >= 0) && (IND[2] >= 0) ) { 
	tt = ID2[IND[0]][IND[1]][IND[2]];
	// dot prod for stopping cond (abs value)
	// check with current dotprod with previous
	dotprod = 0.;
	for( n=0 ; n<3 ; n++) 
	  dotprod+= CC[tt][n]*
	    FB*CC[ID2[IND[0]-go[0]][IND[1]-go[1]][IND[2]-go[2]]][n]; 
	
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
	    dotprod =0.; 
      }
      else {
	// to not try to access inaccessible value 
	// so we will exit tracking in this direction 
	// at start of next loop
	for( n=0 ; n<3 ; n++) 
	  IND[n] = 0; 
	dotprod = 0.; 
      }
    }
    else
      dotprod = 0.;
    
  }
  
  if(tracL >= arrMax) {
    fprintf (stderr,"\n\tErr in data set; or need longer max arr len!\n");
    exit(101);
  }
  
  physL[0] = physdist;
  
  RETURN(tracL); 
}


// basic format for writing out tracking results of 3dProbTrackID:
// + a file of individual ROI intercepts
// + a file of the pairwise combinations
int WriteBasicProbFiles(int N_nets, int Ndata, int Nvox, 
								char *prefix,THD_3dim_dataset *insetFA,
								int *TV_switch,char *voxel_order,int *NROI,
								int ****NETROI,int ***mskd,int ***INDEX2,int *Dim,
								THD_3dim_dataset *dsetn,int argc, char *argv[])
{

	int i,j,k,bb,hh,kk,rr,idx;
	char **prefix_netmap=NULL;
	char **prefix_netmap2=NULL;
	THD_3dim_dataset *networkMAPS=NULL,*networkMAPS2=NULL;

	// ****** alloc'ing
	prefix_netmap = calloc( N_nets,sizeof(prefix_netmap));  
	for(i=0 ; i<N_nets ; i++) 
		prefix_netmap[i] = calloc( 300,sizeof(char)); 
	prefix_netmap2 = calloc( N_nets,sizeof(prefix_netmap2));  
	for(i=0 ; i<N_nets ; i++) 
		prefix_netmap2[i] = calloc( 300,sizeof(char)); 

	if( ( prefix_netmap== NULL) || ( prefix_netmap2== NULL)) {
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(122);
	}
	
	// ****** calc/do
	for( hh=0 ; hh<N_nets ; hh++) {
		
		sprintf(prefix_netmap[hh],"%s_%03d_PAIRMAP",prefix,hh+1); 
		// just get one of right dimensions!
		networkMAPS = EDIT_empty_copy( insetFA ) ; 
		EDIT_dset_items(networkMAPS,
							 ADN_datum_all , MRI_short , 
							 ADN_none ) ;
		EDIT_add_bricklist(networkMAPS ,
								 NROI[hh], NULL , NULL , NULL );
		
		sprintf(prefix_netmap2[hh],"%s_%03d_INDIMAP",prefix,hh+1); 
		// just get one of right dimensions!
		networkMAPS2 = EDIT_empty_copy( insetFA ) ; 
		EDIT_dset_items(networkMAPS2,
							 ADN_datum_all , MRI_float , 
							 ADN_none ) ;
		EDIT_add_bricklist(networkMAPS2 ,
								 NROI[hh], NULL , NULL , NULL );

		
		// first array for all tracks, 2nd for paired ones.
		// still just need one set of matrices output
		short int **temp_arr=NULL;
		float **temp_arr2=NULL;
		temp_arr = calloc( (NROI[hh]+1),sizeof(temp_arr)); // XYZ comps
		for(i=0 ; i<(NROI[hh]+1) ; i++) 
			temp_arr[i] = calloc( Nvox,sizeof(short int)); 
		temp_arr2 = calloc( (NROI[hh]+1),sizeof(temp_arr2));
		for(i=0 ; i<(NROI[hh]+1) ; i++) 
			temp_arr2[i] = calloc( Nvox,sizeof(float)); 
		
		if( ( temp_arr== NULL) || ( temp_arr2== NULL)) {
			fprintf(stderr, "\n\n MemAlloc failure.\n\n");
			exit(122);
		}

		for( bb=1 ; bb<=NROI[hh] ; bb++) {
			idx=0;
			for( k=0 ; k<Dim[2] ; k++ ) 
				for( j=0 ; j<Dim[1] ; j++ ) 
					for( i=0 ; i<Dim[0] ; i++ ) {
						temp_arr[bb][idx] = 0;
						// allow for more than one `connector' tract
						if(mskd[i][j][k]) 
							for( rr=0 ; rr<NROI[hh] ; rr++) 
								if(NETROI[INDEX2[i][j][k]][hh][bb-1][rr]>0) {
									// store connectors
									if(bb-1 != rr){
										temp_arr[0][idx] = 1; 
									}
									
									// store tracks through any ROI
									temp_arr2[0][idx] = (float)
										NETROI[INDEX2[i][j][k]][hh][bb-1][rr]; 
									
									// then add value if overlap
									if(bb-1 != rr)
										temp_arr[bb][idx]+=pow(2,rr+1);// unique decomp.
									
									temp_arr2[bb][idx] = (float)
										NETROI[INDEX2[i][j][k]][hh][bb-1][rr];
								}
						idx+=1;
					}
			
			EDIT_substitute_brick(networkMAPS, bb, MRI_short, temp_arr[bb]);
			temp_arr[bb]=NULL; // to not get into trouble...
			
			EDIT_substitute_brick(networkMAPS2, bb, MRI_float, temp_arr2[bb]);
			temp_arr2[bb]=NULL; // to not get into trouble...
		} 
		
		// FIRST THE PAIR CONNECTORS
		EDIT_substitute_brick(networkMAPS, 0, MRI_short, temp_arr[0]);
		temp_arr[0]=NULL;
		if(TV_switch[0] || TV_switch[1] || TV_switch[2]) {
			dsetn = r_new_resam_dset(networkMAPS, NULL, 0.0, 0.0, 0.0,
											 voxel_order, RESAM_NN_TYPE, 
											 NULL, 1, 0);
			DSET_delete(networkMAPS); 
			networkMAPS=dsetn;
			dsetn=NULL;
		}      
		EDIT_dset_items(networkMAPS,
							 ADN_prefix    , prefix_netmap[hh] ,
							 ADN_brick_label_one , "ALLROI",
							 ADN_none ) ;
		THD_load_statistics(networkMAPS);
		if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(networkMAPS)) )
			ERROR_exit("Can't overwrite existing dataset '%s'",
						  DSET_HEADNAME(networkMAPS));
		tross_Make_History("3dProbTrackID", argc, argv, networkMAPS);
		THD_write_3dim_dataset(NULL, NULL, networkMAPS, True);
		DSET_delete(networkMAPS); 
		for( i=0 ; i<NROI[hh]+1 ; i++) // free all
			free(temp_arr[i]);
		free(temp_arr);
		
		// THEN THE INDIVID TRACKS
		EDIT_substitute_brick(networkMAPS2, 0, MRI_float, temp_arr2[0]);
		temp_arr2[0]=NULL;
		if(TV_switch[0] || TV_switch[1] || TV_switch[2]) {
			dsetn = r_new_resam_dset(networkMAPS2, NULL, 0.0, 0.0, 0.0,
											 voxel_order, RESAM_NN_TYPE, 
											 NULL, 1, 0);
			DSET_delete(networkMAPS2); 
			networkMAPS2=dsetn;
			dsetn=NULL;
		}      
		EDIT_dset_items(networkMAPS2,
							 ADN_prefix    , prefix_netmap2[hh] ,
							 ADN_brick_label_one , "ALLROI",
							 ADN_none ) ;
		THD_load_statistics(networkMAPS2);
		if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(networkMAPS2)) )
			ERROR_exit("Can't overwrite existing dataset '%s'",
						  DSET_HEADNAME(networkMAPS2));
		tross_Make_History("3dProbTrackID", argc, argv, networkMAPS2);
		THD_write_3dim_dataset(NULL, NULL, networkMAPS2, True);
		DSET_delete(networkMAPS2); 
		for( i=0 ; i<NROI[hh]+1 ; i++) // free all
			free(temp_arr2[i]);
		free(temp_arr2);
      
	}
	
  
	// ****** freeing  **********
	
	for( i=0 ; i<N_nets ; i++) {
		free(prefix_netmap[i]); 
		free(prefix_netmap2[i]); 
	}
	free(prefix_netmap);
	free(prefix_netmap2);
	free(networkMAPS);
	free(networkMAPS2);

	RETURN(1);
}

// second format for writing out tracking results of 3dProbTrackID:
// each pairwise map in own file
// (will be in new directory with prefix name)
int WriteIndivProbFiles(int N_nets, int Ndata, int Nvox, int ***Prob_grid,
								char *prefix,THD_3dim_dataset *insetFA,
								int *TV_switch,char *voxel_order,int *NROI,
								int ****NETROI,int ***mskd,int ***INDEX2,int *Dim,
								THD_3dim_dataset *dsetn,int argc, char *argv[],
								float ****Param_grid, int DUMP_TYPE,
								int DUMP_ORIG_LABS, int **ROI_LABELS, int POST_IT)
{

	int i,j,k,bb,hh,rr,ii,jj,kk;
	int idx,idx2,count;
	char ***prefix_netmap=NULL;
	char ***prefix_dump=NULL;
	THD_3dim_dataset *networkMAPS=NULL;
	int *N_totpair=NULL;
	int sum_pairs=0;
	FILE *fout;


	N_totpair = (int *)calloc(N_nets, sizeof(int)); 

	// find out how many networks we'll be outputting.
	// this means going through UHT part of probgrid, looking for nonzeros
	for( k=0 ; k<N_nets ; k++) 
		for( i=0 ; i<NROI[k] ; i++ ) 
			for( j=i ; j<NROI[k] ; j++ ) // include diags
				if(Prob_grid[k][i][j]>0){
					N_totpair[k]+=1;
					sum_pairs+=1;
				}

	if( sum_pairs==0 )
		INFO_message("No pairs in any network to output.");
	else {

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
		
		// ****** calc/do, loop through networks
		for( hh=0 ; hh<N_nets ; hh++) {
			count=0;
			for( i=0 ; i<NROI[hh] ; i++ ) 
				for( j=i ; j<NROI[hh] ; j++ ) // include diags
					if(Prob_grid[hh][i][j]>0) {
						
						if(DUMP_ORIG_LABS)
							sprintf(prefix_netmap[hh][count],
									  "%s/NET_%03d_ROI_%03d_%03d",prefix,hh+1,
									  ROI_LABELS[hh][i+1],ROI_LABELS[hh][j+1]); 
						else
							sprintf(prefix_netmap[hh][count],
									  "%s/NET_%03d_ROI_%03d_%03d",prefix,hh+1,i+1,j+1); 

						// single brik, byte map
						networkMAPS = EDIT_empty_copy( insetFA ) ; 
						EDIT_dset_items(networkMAPS,
											 ADN_datum_all , MRI_byte , 
											 ADN_none ) ;
						
						sprintf(prefix_dump[hh][count],
								  "%s/NET_%03d_ROI_%03d_%03d.dump",
								  prefix,hh+1,i+1,j+1); 

                  float *temp_arr_FL=NULL;
                  byte *temp_arr_BY=NULL;
		
						// first array for all tracks, 2nd for paired ones.
						// still just need one set of matrices output
                  if(POST_IT) {
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
                  temp_arr2=calloc(Param_grid[hh][i][j][8],
                                   sizeof(temp_arr2)); 
                  for(bb=0 ; bb<Param_grid[hh][i][j][8] ; bb++) 
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
									if(mskd[ii][jj][kk]) 
										if(NETROI[INDEX2[ii][jj][kk]][hh][i][j]>0) {
                                // store locations
                                if(POST_IT)
                                  temp_arr_FL[idx] = (float) 
                                    NETROI[INDEX2[ii][jj][kk]][hh][i][j];
                                else
                                  temp_arr_BY[idx] = 1;
                                temp_arr2[idx2][0] = ii;
                                temp_arr2[idx2][1] = jj;
                                temp_arr2[idx2][2] = kk;
                                temp_arr2[idx2][3] = 1;
                                idx2++;
                              }
									idx++;
								}
                  
						if(idx2 != Param_grid[hh][i][j][8])
                    printf("ERROR IN COUNTING! Netw,ROI,ROI= (%d, %d, %d); idx2 %d != %d paramgrid.\n",hh,i,j,idx2,(int) Param_grid[hh][i][j][8]);
                  
                  if(POST_IT){
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
						
						// output AFNI if D_T=2 or 3
						if(DUMP_TYPE>1) {
							THD_load_statistics(networkMAPS);
							if( !THD_ok_overwrite() && 
								 THD_is_ondisk(DSET_HEADNAME(networkMAPS)) )
								ERROR_exit("Can't overwrite existing dataset '%s'",
											  DSET_HEADNAME(networkMAPS));
							tross_Make_History("3dProbTrackID", argc, argv, networkMAPS);
							THD_write_3dim_dataset(NULL, NULL, networkMAPS, True);
						}

						DSET_delete(networkMAPS); 
                  if(POST_IT)
                    free(temp_arr_FL);
                  else
                    free(temp_arr_BY);
                  
						// THEN THE DUMP FILE
						// if D_T = 1 or 3
						if( DUMP_TYPE % 2) {
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
						for( bb=0 ; bb<(int) Param_grid[hh][i][j][8] ; bb++) // free all
							free(temp_arr2[bb]);
						free(temp_arr2);
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
	
	RETURN(1);
}
