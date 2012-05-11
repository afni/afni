#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <debugtrace.h>
#include <mrilib.h>     // AFNIadd
#include <3ddata.h>     // AFNIadd
#include <TrackIO.h>
#include <DoTrackit.h>


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

