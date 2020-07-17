#include "mrilib.h"
#include "roiing.h"
//#include <gsl/gsl_randist.h>
//#include <gsl/gsl_rng.h>
//#include "DoTrackit.h"




/*
  transfer values from DATA to 'map', so that we can start the GM
  inflation part after preinflation

  also subtract off the remnants of the WM, so it's just GM in the
  'map': do this, use the inverse skeleton (i.e., no WM)
 */
int MoveData_to_InpSet( int *Dim, 
                        float ****map,
                        int ****DATA,
                        short int ***iskel)
{
   int i,j,k,m;

   for( m=0 ; m<Dim[3] ; m++ )
      for( k=0 ; k<Dim[2] ; k++ ) 
         for( j=0 ; j<Dim[1] ; j++ ) 
            for( i=0 ; i<Dim[0] ; i++ ) {
               if( iskel[i][j][k] && (DATA[i][j][k][m] != 0) ) 
                  // transfer values over
                  map[i][j][k][m] = 1;// DATA[i][j][k][m];
               else
                  map[i][j][k][m] = 0;// DATA[i][j][k][m];
               // reset DATA
               DATA[i][j][k][m] = 0;
            }
   
   RETURN(1);
}



/* 
   go through and start inflating
   do 1 layer at a time, in case of squeezed neighborhoods and 
   book counting of WM intersections, etc.
*/

int ROI_make_inflate( int *Dim, 
                      int INFL_NUM,
                      int SKEL_STOP,
                      int NEIGHBOR_LIMIT,
                      int HAVE_MASK,
                      THD_3dim_dataset *MASK,
                      int ****DATA,
                      short int ***SKEL,
                      int ***COUNT_GM,
                      int **INV_LABELS_GM)
{

   int n,m,k,j,i,ii,jj,kk,idx,aaa;

   INFO_message("SKEL_STOP = %d", SKEL_STOP);
	for( n=0 ; n<INFL_NUM ; n++) {
		for( m=0 ; m<Dim[3] ; m++ ) {
			for( k=0 ; k<Dim[2] ; k++ ) 
				for( j=0 ; j<Dim[1] ; j++ ) 
					for( i=0 ; i<Dim[0] ; i++ ) 
						if(DATA[i][j][k][m]>0) {
							// now check surroundings: only expand from
							// non-skel, if desired
							if( !(SKEL_STOP && SKEL[i][j][k]))
								for( ii=-DEP ; ii<=DEP ; ii++)
									for( jj=-DEP ; jj<=DEP ; jj++)
										for( kk=-DEP ; kk<=DEP ; kk++)
											// need to share face or edge, not only vertex
											if(abs(ii)+abs(jj)+abs(kk)<NEIGHBOR_LIMIT) //3)
												
												// keep in bounds
												if((0 <= i+ii) && (i+ii < Dim[0]) && 
													(0 <= j+jj) && (j+jj < Dim[1]) && 
													(0 <= k+kk) && (k+kk < Dim[2])) {
													idx = THREE_TO_IJK(i+ii,
                                                          j+jj,
                                                          k+kk,
                                                          Dim[0],
                                                          Dim[0]*Dim[1]);
                                       if( HAVE_MASK>1 )
                                          aaa = m;
                                       else
                                          aaa = 0;
													if( (HAVE_MASK==0) || 
														 (HAVE_MASK && 
														  ( THD_get_voxel(MASK,idx,aaa)>0 ))){
														
														// grow if ngb=0; give temp value
														// of -[value it will have]
														if( DATA[i+ii][j+jj][k+kk][m]==0) {
                                             // [PT: June 6, 2017]: new, stricter
                                             // non-inflation condition, if so
                                             // desired: don't expand into SKEL,
                                             // even by one voxel
                                             if( !((SKEL_STOP==2) && SKEL[i+ii][j+jj][k+kk]))
                                                DATA[i+ii][j+jj][k+kk][m] = 
                                                   -DATA[i][j][k][m];
														}
													}
												}
						}
			
			// and now convert the layer to being part of the ROI
			for( k=0 ; k<Dim[2] ; k++ ) 
				for( j=0 ; j<Dim[1] ; j++ ) 
					for( i=0 ; i<Dim[0] ; i++ ) 
						if(DATA[i][j][k][m]<0) {
							DATA[i][j][k][m] = -DATA[i][j][k][m];
							COUNT_GM[m][ INV_LABELS_GM[m][DATA[i][j][k][m]] ][1]++;
							if(SKEL[i][j][k])
								COUNT_GM[m][ INV_LABELS_GM[m][DATA[i][j][k][m]] ][2]++;
						}
			
		}
      
	}
}

















int compfunc_desc(const void * a, const void * b) 
{
   return (*(float*)b > *(float*)a) - (*(float*)a > *(float*)b);
}



int Make_SepLabels( int *Dim,
                    int ****DATA,
                    int max_nroi,
                    int *N_thr,
                    int *NROI_IN, 
                    int **ROI_LABELS_pre,
                    int VOLTHR,
                    int NEIGHBOR_LIMIT,
                    int HOT_POINTS,
                    int HOT_CONN,
                    float ****inset )
{

   int i,j,k,m,ii,jj,kk,mm,nn,pp,idx,index1,count;
   int investigated=0, found=0;
	int **list1=NULL; // growing list of vox while investigating ROI
	int **list1_temp=NULL; // copy for sorting and using
	//int *VOX=NULL;
	int X,Y,Z;
   float *fl_sort=NULL;
   int *ind_sort=NULL, *temp_USED=NULL;
   int temp_found = 0;
   float hot_val=0;
   int KEEPIT = 0;
   int dtemp[3] = {0,0,0};

	list1 = calloc( max_nroi, sizeof(int *) );
	for ( j = 0 ; j < max_nroi ; j++ ) 
		list1[j] = (int *) calloc( 3, sizeof(int) );
   //VOX = (int *)calloc(Dim[3],sizeof(int)); // num of vox >thr per brik,var
 
   if( (list1 == NULL) //|| (VOX == NULL)   
       ) { 
		fprintf(stderr, "\n\n MemAlloc failure.\n\n");
		exit(15);
	}		
	
   for( m=0 ; m<Dim[3] ; m++ ) {
		// make it bigger than any final index possibly could be
		index1 = N_thr[m]; 
      
		for( k=0 ; k<Dim[2] ; k++ ) 
			for( j=0 ; j<Dim[1] ; j++ ) 
				for( i=0 ; i<Dim[0] ; i++ ) {
					if( DATA[i][j][k][m] == BASE_DVAL ) {
						// now we go into separate action to fill out other ones
						investigated = 0;
						NROI_IN[m]+= 1; // keep track of # of rois per brik
						index1+= 1;
						DATA[i][j][k][m] = index1;
						found = 1;
						list1[0][0]=i; list1[0][1]=j; list1[0][2]=k;
						
						while(investigated<found) {
							// shorter notation for use in loops...
							X=list1[investigated][0]; 
							Y=list1[investigated][1];
							Z=list1[investigated][2];
							// investigate its neighbors
							for( ii=-DEP ; ii<=DEP ; ii++)
								for( jj=-DEP ; jj<=DEP ; jj++)
									for( kk=-DEP ; kk<=DEP ; kk++)
										// need to share face or edge, not only vertex
										if(abs(ii)+abs(jj)+abs(kk)<NEIGHBOR_LIMIT)
											// keep in bounds
											if((0 <= X+ii) && (X+ii < Dim[0]) && 
												(0 <= Y+jj) && (Y+jj < Dim[1]) && 
												(0 <= Z+kk) && (Z+kk < Dim[2])) {
												// if a neighbor has value of -one...
												if(DATA[X+ii][Y+jj][Z+kk][m]==BASE_DVAL){
                                       // ... then change
													DATA[X+ii][Y+jj][Z+kk][m] = index1; 
													list1[found][0] = X+ii; // keep the coors
													list1[found][1] = Y+jj;
													list1[found][2] = Z+kk;
													found+= 1; // add to list to investigate,
												}
											}
							investigated+=1;
						}
						
						// if one is thresholding based on volume size, 
						// and if this one volume fails:
						if( (VOLTHR>0) && (found<VOLTHR) ) {
							// erase these, reset val to zero; careful of mm and m...
							for( mm=0 ; mm<found ; mm++ )
								DATA[list1[mm][0]][list1[mm][1]][list1[mm][2]][m] = 0;
							index1-=1; // reset index value
							NROI_IN[m]-=1; // reset index value
						}
						else {
                     // at this point, put in an optional search through all 
                     // 'current' ROIs to threshold based on value
                     if( HOT_POINTS && (found > HOT_POINTS) ) {

                        count = found;

                        fl_sort = (float *)calloc(found,sizeof(float)); 
                        if( fl_sort == NULL) {
                           fprintf(stderr, "\n MemAlloc failure (fl_sort).\n");
                           exit(14);	
                        }
              
                        for( mm=0 ; mm<found ; mm++ ) 
                           fl_sort[mm]=inset[list1[mm][0]][list1[mm][1]][list1[mm][2]][m];
                        
                        qsort(fl_sort, count, sizeof(float), compfunc_desc);
                        hot_val = fl_sort[HOT_POINTS];

                        for( mm=0 ; mm<count ; mm++ ) {
                           if( !(inset[list1[mm][0]][list1[mm][1]][list1[mm][2]][m] 
                                 > hot_val) ) {
                              DATA[list1[mm][0]][list1[mm][1]][list1[mm][2]][m]=
                                 0;
                              found--;
                           }
                        }
                        free(fl_sort);

                        //found = count;// just reset for resetting list1 next
                     }
                     else if( HOT_CONN && (found > HOT_CONN) ) {

                        count = found;

                        fl_sort = (float *)calloc(found,sizeof(float)); 
                        ind_sort = (int *)calloc(found,sizeof(int)); 
                        temp_USED = (int *)calloc(found,sizeof(int)); 
                        // temp storage values
                        list1_temp = calloc( max_nroi, sizeof(int *) ); 
                        for ( mm = 0 ; mm<max_nroi ; mm++ ) 
                           list1_temp[mm] = (int *) calloc( 3, sizeof(int) );

                        if(( fl_sort == NULL) || ( ind_sort == NULL) || 
                           ( list1_temp == NULL) || ( temp_USED == NULL)) {
                           fprintf(stderr, "\n MemAlloc failure (fl_sort).\n");
                           exit(14);	
                        }
              
                        for( mm=0 ; mm<found ; mm++ ) {
                           fl_sort[mm]=inset[list1[mm][0]][list1[mm][1]][list1[mm][2]][m];
                           ind_sort[mm] = mm;
                        }

                        // fl_sort has the new values, and ind_sort has where they came from
                        piksr2_FLOAT(found, fl_sort, ind_sort);

                        // initialize with max value
                        for ( nn=0 ; nn<3 ; nn++ ) {
                           list1_temp[0][nn] = list1[ind_sort[0]][nn];
                           temp_USED[0] = 1;
                        }
                        temp_found = 1;

                        while( temp_found < HOT_CONN ) {

                           for( mm=0 ; mm<found ; mm++ ) {
                              // check for first unused max
                              // in the end, temp_USED contains which were NOT used, to subtract
                              if( temp_USED[mm] == 0 ) {
                                 KEEPIT = 0;

                                 // go through entire list of those already found, and check
                                 // to see: 1) if it is within distance of any
                                 for( pp=0 ; pp<temp_found ; pp++) {
                                    // check distances to previous
                                    for ( nn=0 ; nn<3 ; nn++ ) 
                                       dtemp[nn] = list1[ind_sort[mm]][nn] - list1_temp[pp][nn];
                                    if( Do_Check_Neigh_Diff(dtemp, NEIGHBOR_LIMIT)) {
                                       // if it's close, keep and exit
                                       KEEPIT = 1;
                                       break;
                                    }
                                 }
                              
                                 if( KEEPIT ) {
                                    for ( nn=0 ; nn<3 ; nn++ ) 
                                       list1_temp[temp_found][nn] = list1[ind_sort[mm]][nn];
                                    temp_USED[mm] = 1;
                                    temp_found++;
                                    break;
                                 }
                              }
                           }
                        }

                        for( mm=0 ; mm<count ; mm++ ) {
                           if( !(temp_USED[mm]) ) {
                              DATA[list1[ind_sort[mm]][0]][list1[ind_sort[mm]][1]][list1[ind_sort[mm]][2]][m]=
                                 0;
                              found--;
                           }
                        }
                        free(fl_sort);
                        free(ind_sort);
                        free(temp_USED);
                        for ( mm=0 ; mm<max_nroi ; mm++ ) 
                           free(list1_temp[mm]);
                        free(list1_temp);
                        
                        //found = count;// just reset for resetting list1 next
                     }
                     
                     // !! hold value of Nvox in the ROI; ROI_LABEL
                     // for the ith NROI is
                     // i+N_thr[m].... //index1; // Xth ROI gets value Y
							ROI_LABELS_pre[m][NROI_IN[m]] = found; 
							//VOX[m]+=found;
						}
      
						// so, we found a 1, grew it out and changed all assoc.
						// indices-- keep track of how many voxels there were...
						// numperroi1(index1) = found;
						for( ii=0 ; ii<found ; ii++) // clean up list for use again
							for( jj=0 ; jj<3 ; jj++ )
								list1[ii][jj]=0;
					}
				}
   }

   // ********** start freeing ********************************
	for ( j = 0 ; j < max_nroi ; j++ ) 
		free(list1[j]);
	free(list1);
	//free(VOX);

   // ********** done freeing ********************************

   RETURN(1);
}








/*
  Now, we go through and grow each region 
  (labelled with a 4) into ones with index value larger than N_thr[m]. 
*/
int Relabel_IfNecessary( int *Dim,
                         int ****DATA,
                         int *N_thr,
                         int *relab_vox,
                         int *NROI_IN, 
                         int *NROI_REF,
                         int **ROI_LABELS_REF,
                         int NEIGHBOR_LIMIT)
{

   int i,j,k,m,ii,jj,kk,idx;
	int found_this_iter=0, KEEP_GOING=1;
  

   for( m=0 ; m<Dim[3] ; m++ ) {
      KEEP_GOING = 1;
      while( KEEP_GOING==1 ) {
         
         found_this_iter=0;
         idx=0;
         for( k=0 ; k<Dim[2] ; k++ ) 
            for( j=0 ; j<Dim[1] ; j++ ) 
               for( i=0 ; i<Dim[0] ; i++ ) {
                  // find ones whose neighbor's might need to be relabelled
                  // take data values which are only between (0, N_thr), 
                  // which at this point should just be ref values-- temp 
                  // inlabel values are >N_thr.
                  if( (DATA[i][j][k][m]>0) &&
                      (DATA[i][j][k][m]<N_thr[m]) ) { 
                     // check neighbors
                     for( ii=-DEP ; ii<=DEP ; ii++)
                        for( jj=-DEP ; jj<=DEP ; jj++)
                           for( kk=-DEP ; kk<=DEP ; kk++)
                              // need to share face or edge, not only vertex
                              if(abs(ii)+abs(jj)+abs(kk)<NEIGHBOR_LIMIT) //3)
												
                                 // keep in bounds
                                 if((0 <= i+ii) && (i+ii < Dim[0]) && 
                                    (0 <= j+jj) && (j+jj < Dim[1]) && 
                                    (0 <= k+kk) && (k+kk < Dim[2])) {
                                    // grow if ngb>=N_thr; give temp value
                                    // of -[value it will have]
                                    if( DATA[i+ii][j+jj][k+kk][m]>=N_thr[m]) {
                                       DATA[i+ii][j+jj][k+kk][m] = 
                                          -DATA[i][j][k][m];
                                       
                                       found_this_iter++;
                                    }
                                 }
                  }
                  idx++;
               }
         
         if( found_this_iter==0 )
            KEEP_GOING=0;
         else {
            relab_vox[m]+= found_this_iter;
					
            for( k=0 ; k<Dim[2] ; k++ ) 
               for( j=0 ; j<Dim[1] ; j++ ) 
                  for( i=0 ; i<Dim[0] ; i++ ) 
                     if( DATA[i][j][k][m]<0 ){
                        DATA[i][j][k][m]*= -1;
                     }
         }
         if( relab_vox[m]==N_thr[m] )
            KEEP_GOING=0;				
      }
			
      // final part, relabel the inROIs which are unmatched with the 
      // ref ones
      found_this_iter=0;
      for( k=0 ; k<Dim[2] ; k++ ) 
         for( j=0 ; j<Dim[1] ; j++ ) 
            for( i=0 ; i<Dim[0] ; i++ ) 
               if( DATA[i][j][k][m]>N_thr[m] ) {
                  DATA[i][j][k][m]-= NROI_IN[m] + N_thr[m];
                  DATA[i][j][k][m]+= ROI_LABELS_REF[m][NROI_REF[m]];
                  found_this_iter+=1 ;
               }
      relab_vox[m]+= found_this_iter;
   }



   RETURN(1);

}






int Make_BinaryMask( int *Dim,
                     int HAVE_MASK,
                     THD_3dim_dataset *MASK,
                     float ****inset,
                     float THR,
                     int TRIM_OFF_WM,
                     short int ***SKEL,
                     short int ***CSF_SKEL,
                     int HAVE_CSFSKEL,
                     int ****DATA,
                     int *N_thr ) 
{
   
   int i,j,k,m,idx,aaa;
      
	for( m=0 ; m< Dim[3] ; m++ ) {
      // start this here, because of preinfl-- may call twice
      N_thr[m] = 0;  
      if( HAVE_MASK>1 ) // allow multiple mask briks
         aaa = m;
      else
         aaa = 0;
		// should preserve relative ordering of data
		for( k=0 ; k<Dim[2] ; k++ ) 
			for( j=0 ; j<Dim[1] ; j++ ) 
				for( i=0 ; i<Dim[0] ; i++ ) {
               idx = THREE_TO_IJK(i,j,k,Dim[0],Dim[0]*Dim[1]);
					if( (HAVE_MASK==0) || 
						 (HAVE_MASK && ( THD_get_voxel(MASK,idx,aaa)>0 ) ) )
						if( inset[i][j][k][m] > THR ) 
							if( !TRIM_OFF_WM || (TRIM_OFF_WM && !SKEL[i][j][k]) )
								if( !HAVE_CSFSKEL 
									 || (HAVE_CSFSKEL && !CSF_SKEL[i][j][k]) )
									{
										// temporary until ROIs are 1st labelled
										DATA[i][j][k][m] = BASE_DVAL;
										N_thr[m]+= 1;
									}
				}

      if( N_thr[m] < MIN_NTHR_MAX_NROILAB ) // Jan 2015: safety catch for small input sets
         N_thr[m] = MIN_NTHR_MAX_NROILAB;
	}

   RETURN(1);
}




int MakeSkels( int *Dim, 
               int HAVE_CSFSKEL,
               short int ***CSF_SKEL, 
               THD_3dim_dataset *insetCSF_SKEL,
               int HAVESKEL,
               short int ***SKEL, 
               THD_3dim_dataset *insetSKEL,
               float SKEL_THR ) {

   int i,j,k,idx;

	// make skeleton: either with file input, or just put 1s everywhere.
	// should preserve relative ordering of data
	for( k=0 ; k<Dim[2] ; k++ ) 
		for( j=0 ; j<Dim[1] ; j++ ) 
			for( i=0 ; i<Dim[0] ; i++ ) {
            idx = THREE_TO_IJK(i,j,k,Dim[0],Dim[0]*Dim[1]);
				if( ((HAVESKEL==1) && (THD_get_voxel(insetSKEL,idx,0)>SKEL_THR)) 
					 || (HAVESKEL==0) ) 
					SKEL[i][j][k] = 1;
				if( ((HAVE_CSFSKEL==1) && (THD_get_voxel(insetCSF_SKEL,idx,0)>0.5)) 
					 || (HAVE_CSFSKEL==0) ) 
					CSF_SKEL[i][j][k] = 1;
			}

   RETURN(1);
}




// from Numerical Recipes by Press, Teukolsky, Vetterling and Flannery
// (indices adjusted to modern C)
void piksr2_FLOAT(int n, float arr[], int brr[])
//Sorts an array arr[1..n] into DEscending numerical order, by straight
//  insertion, while making the corresponding rearrangement of the array
//  brr[1..n].
{
	int i,j;
	float a;
	int b;
	for (j=1;j<n;j++) { // Pick out each element in turn.
		a=arr[j];
		b=brr[j];
		i=j-1;
		while (i >= 0 && arr[i] < a) { // Look for the place to insert it.
			arr[i+1]=arr[i];
			brr[i+1]=brr[i];
			i--;
		}
		arr[i+1]=a; // Insert it.
		brr[i+1]=b;
	}
}



int Do_Check_Neigh_Diff(int *D, int NL)
{
   int OUT = 0;

   if ( (abs(D[0]) <= DEP ) && 
        (abs(D[1]) <= DEP ) && 
        (abs(D[2]) <= DEP ) && 
        (abs(D[0])+abs(D[1])+abs(D[2]) < NL) )
      OUT = 1;
       
   return OUT;
}
