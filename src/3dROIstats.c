/*******************************************************
 * 3dROIstats                                          *
 * T. Ross 5/99                                        *
 *                                                     *
 *******************************************************/





#include "mrilib.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>

short non_zero[65536];  /* Ugly; depends upon sizeof(short)=2 */

void Error_Exit(char *message) {
	fprintf (stderr, "\n\nError: %s\n", message);
	exit(1);
}


int main (int argc, char * argv[]) {
	
   THD_3dim_dataset * mask_dset=NULL , * input_dset=NULL ;
   int mask_subbrik = 0;
   int sigma = 0, nzmean = 0, nzcount = 0, debug=0;
   short * mask_data;
   int nvox, i, brik;
   int num_ROI, ROI;
   int narg = 1 ;
   double *sum, *sumsq, *nzsum, sig;
   long  *voxels, *nzvoxels;
   float *input_data;
   byte * temp_datab;
   short * temp_datas;

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dROIstats -mask[n] mset [options] datasets\n"
             "Options:\n"
             "  -mask[n] mset Means to use the dataset 'mset' as a mask:\n"
             "                 If n is present, it specifies which sub-brick\n"
             "                 in mset to use a la 3dcalc.  Note: do not include\n"
             "                 the brackets if specifing a sub-brick, they are\n"
             "                 there to indicate that they are optional.  If not\n"
             "                 present, 0 is assumed\n"
             "                 Voxels with the same nonzero values in 'mset'\n"
             "                 will be statisticized from 'dataset'.  This will\n"
             "                 be repeated for all the different values in mset.\n"
             "                 I.e. all of the 1s in mset are one ROI, as are all\n"
             "                 of the 2s, etc.\n"
             "                 Note that the mask dataset and the input dataset\n"
             "                 must have the same number of voxels and that mset\n"
             "                 must be BYTE or SHORT.\n"
             "\n"
             "   -debug        Print out debugging information\n"
             "\n"
             "The following options specify what stats are computed.  By default\n"
             "the mean is always computed.\n"
             "\n"
             "  -nzmean      Compute the mean using only non_zero voxels.  Implies\n"
             "                the oppisite for the normal mean computed\n"
             "  -nzvoxels    Compute the number of non_zero voxels\n"
             "  -sigma       Means to compute the standard deviation as well\n"
             "                 as the mean.\n"
             "\n"
             "The output is printed to stdout (the terminal), and can be\n"
             "saved to a file using the usual redirection operation '>'.\n"
            ) ;
      exit(0) ;
   }

   /* scan argument list */

   while( narg < argc && argv[narg][0] == '-' ){

      if( strncmp(argv[narg],"-mask",5) == 0 ){
         if( mask_dset != NULL )
            Error_Exit("Cannot have two -mask options!");
         
         if( narg+1 >= argc )
            Error_Exit("-mask option requires a following argument!") ;
         
#if 0
         mask_dset = THD_open_one_dataset( argv[++narg] ) ;
#else
         mask_dset = THD_open_dataset( argv[++narg] ) ;  /* 16 Sep 1999 */
#endif
         if( mask_dset == NULL )
            Error_Exit("Cannot open mask dataset!") ; 
         
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex )
            Error_Exit("Cannot deal with complex-valued mask dataset!") ;
         
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_float )
            Error_Exit("Cannot deal with float-valued mask dataset!") ;
         
         if(isdigit(argv[narg][5])) { /* mask is a subbrik */
            mask_subbrik = (int)atol(argv[narg]+5);
            if ((mask_subbrik < 0) || (mask_subbrik>=(DSET_NVALS(mask_dset))))
               Error_Exit("Illegal sub-brisk following the -mask option");
         }
         narg++ ; continue ;
      }
/*
      if( strncmp(argv[narg],"-dsub",5) == 0 ){
         if( narg+2 >= argc )
            Error_Exit("-dsub option needs 2 following arguments!\n")  ;
         
         dmin = (int) strtod( argv[++narg] , NULL ) ;
         dmax = (int) strtod( argv[++narg] , NULL ) ;
         narg++ ; continue ;
      }
*/
      if( strncmp(argv[narg],"-sigma",5) == 0 ){
         sigma = 1 ;
         narg++ ; continue ;
      }


      if( strncmp(argv[narg],"-nzmean",5) == 0 ){
         nzmean = 1 ;
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-debug",5) == 0 ){
         debug = 1 ;
         narg++ ; continue ;
      }

      if( strncmp(argv[narg],"-nzvoxels",5) == 0 ){
         nzcount = 1 ;
         narg++ ; continue ;
      }

      Error_Exit("Unknown option") ;
   }

   /* Remaining arguements are files */

   if( narg >= argc )
      Error_Exit("No input datasets!?\n") ;
   

   /* See how many ROIS there are to deal with in the mask */
   for (i=0; i<65536; non_zero[i++]=0);

   DSET_load(mask_dset);
   if( DSET_ARRAY(mask_dset,mask_subbrik) == NULL )
       Error_Exit("Cannot read in mask dataset BRIK!") ;
    
   nvox = DSET_NVOX(mask_dset);
   if (debug) fprintf(stderr, "The number of voxels in the mask dataset is %d\n", nvox);

   switch( DSET_BRICK_TYPE(mask_dset,mask_subbrik) ){
      default:
         Error_Exit("Cannot deal with mask dataset datum!") ; 

      case MRI_short:{
	 mask_data = (short *)DSET_ARRAY(mask_dset, mask_subbrik);
         for (i=0; i<nvox; i++) 
            if (mask_data[i]) {
		if (debug) fprintf(stderr, "Nonzero mask voxel %d value is %d\n", i, mask_data[i]);
            	non_zero[mask_data[i]+32768]=1;
	    }
         break ;
       }
       
      case MRI_byte: {
         byte * temp_byte = (byte *)DSET_ARRAY(mask_dset, mask_subbrik); 
         if ((mask_data = (short *)malloc(nvox*sizeof(short))) == NULL)
            Error_Exit("Memory allocation error"); 
         
         for (i=0; i<nvox; i++) {
            mask_data[i] = (short)temp_byte[i];
            if (mask_data[i]){
		if (debug) fprintf(stderr, "Nonzero mask voxel %d value is %d\n", i, mask_data[i]);
            	non_zero[mask_data[i]+32768]=1;
            }
         }
         break ;
       }
    } /* switch */

   /* Print the header line, while we set up the index array */  
   fprintf(stdout, "File\tSub-brick");
 
   for (i=0, num_ROI=0; i<65536; i++)
      if (non_zero[i]) {
         non_zero[i] = num_ROI;
         num_ROI++;

         fprintf(stdout, "\tMean_%d", i-32768);
         if (nzmean)             
            fprintf(stdout, "\tNZMean_%d", i-32768);
         if (nzcount)             
            fprintf(stdout, "\tNZcount_%d", i-32768 );
         if (sigma)
            fprintf(stdout, "\tSigma_%d", i-32768);

      }

   fprintf(stdout,"\n");

   if (debug) fprintf(stderr, "Total Number of ROIs are %d\n", num_ROI);

   /* Now, num_ROI is the number of ROIs from the mask to deal with, 
      and non_zero[mask_data[i]+32768] can be used as in index to 
      a 0..num_ROI array */
      
   if ((sum = (double *)malloc(num_ROI*sizeof(double))) == NULL)
      Error_Exit("Memory allocation error");  
   if ((voxels = (long *)malloc(num_ROI*sizeof(long))) == NULL)
      Error_Exit("Memory allocation error"); 
   if (nzmean || nzcount) { 
      if ((nzsum = (double *)malloc(num_ROI*sizeof(double))) == NULL)
         Error_Exit("Memory allocation error");  
      if ((nzvoxels = (long *)malloc(num_ROI*sizeof(long))) == NULL)
         Error_Exit("Memory allocation error");
   } 
   if (sigma) 
      if (( sumsq = (double *)malloc(num_ROI*sizeof(double))) == NULL)
         Error_Exit("Memory allocation error");  
   

   /* Now, loop over datasets and sub-bricks and compute away */

   for (;narg<argc;narg++) {

#if 0
      input_dset = THD_open_one_dataset( argv[narg] ) ;
#else
      input_dset = THD_open_dataset( argv[narg] ) ; /* 16 Sep 1999 */
#endif
      if( input_dset == NULL ){
         fprintf(stderr,"Warning: Cannot open input dataset %s\n", argv[narg]) ; 
         continue ;
      }
      if (DSET_NVOX(input_dset) != nvox) {
         fprintf(stderr,"Warning: Input dataset %s is a different size than the mask\n", argv[narg]) ; 
         continue ;
      }
      
      DSET_load(input_dset);
      
      for (brik=0; brik<DSET_NVALS(input_dset); brik++) {
      
          for(i=0; i<num_ROI; i++) {
             sum[i] = 0; voxels[i] = 0 ;
             if (nzmean || nzcount) { nzsum[i] = 0; nzvoxels[i] = 0; }
             if (sigma) sumsq[i] = 0;
          }
          
          switch ( DSET_BRICK_TYPE(input_dset, brik) ) {
             
             case MRI_byte: {
                float fac = DSET_BRICK_FACTOR(input_dset, brik);
                if (fac == 0) fac = 1.0;
                temp_datab = (byte *) DSET_ARRAY(input_dset, brik);
                if ((input_data = (float *)malloc(nvox*sizeof(float))) == NULL)
                   Error_Exit("Memory allocation error"); 
                for (i=0; i<nvox; i++)
                   input_data[i] = fac*(float)temp_datab[i];
                break;
             }
 
             case MRI_short: {
                float fac = DSET_BRICK_FACTOR(input_dset, brik);
                if (fac == 0) fac = 1.0;
                temp_datas = (short *) DSET_ARRAY(input_dset, brik);
                if ((input_data = (float *)malloc(nvox*sizeof(float))) == NULL)
                   Error_Exit("Memory allocation error"); 
                for (i=0; i<nvox; i++)
                   input_data[i] = fac*(float)temp_datas[i];
                break;
             }
             
             case MRI_float: {
                float fac = DSET_BRICK_FACTOR(input_dset, brik);
                input_data = (float *) DSET_ARRAY(input_dset, brik);
                if (fac == 0) 
                   fac = 1.0;
                else
                   for (i=0; i<nvox; input_data[i++] *= fac);
                break;
             }
              
             default : {
                fprintf (stderr, "Cannot use sub-brick %d for file %s.  Is it complex?\n", brik, argv[narg]);
                continue;   /* next iteration of loop -> next brick */
             }
          
          } /* switch */  
          
          /* do the stats */
          
          for (i=0; i<nvox; i++) {
             if(mask_data[i]) {
             	ROI = non_zero[mask_data[i]+32768];
                if((ROI<0) || (ROI >= num_ROI))
                   Error_Exit("Somehow I boned computing how many ROIs existed");
                
                sum[ROI]+=(double)input_data[i];
                voxels[ROI]++;
                if (nzmean || nzcount) {
                   if (input_data[i] != 0.0) {
                      nzsum[ROI]+=(double)input_data[i];
                      nzvoxels[ROI]++;
                   }
                }
                if (sigma) sumsq[ROI] += input_data[i]*input_data[i];
             }
          }
          
          /* print the next line of results */
          fprintf(stdout, "%s\t%d", argv[narg], brik);
          for (i=0; i<num_ROI; i++) {
             fprintf(stdout, "\t%f", (float)( sum[i]/(double)voxels[i] )  );
             if (nzmean)             
                fprintf(stdout, "\t%f", nzvoxels[i] ? (float)(nzsum[i]/(double)nzvoxels[i]) : 0.0 );
             if (nzcount)             
                fprintf(stdout, "\t%ld", nzvoxels[i] );
             if (sigma) {
                double mean = sum[i]/(double) voxels[i]; 
                sumsq[i] /= (double) voxels[i];
                if (voxels[i] == 1)
                   sig = 1e30;  /* a really big number */
                else
                   sig = sqrt((voxels[i]/(voxels[i]-1)) * (sumsq[i] - mean*mean));          
                fprintf(stdout, "\t%f", (float) sig);
             }
          } /* loop over ROI for print */
          
          fprintf(stdout,"\n");
        
          if ( DSET_BRICK_TYPE(input_dset, brik) != MRI_float)
             free(input_data);    
       
       } /* loop over bricks */
       
       DSET_unload(input_dset);
       
    } /* loop over input files */
    
    if (DSET_BRICK_TYPE(mask_dset,mask_subbrik) == MRI_byte)
       free(mask_data);
    DSET_unload(mask_dset);
    
    free(sum); free(voxels);
    if (nzmean || nzcount) { free(nzsum); free(nzvoxels); }
    if (sigma) free(sumsq);
    
}
    
     
      
      
      

