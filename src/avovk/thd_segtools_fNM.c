#include <stdio.h>
#include <gsl/gsl_multifit.h>

#include "../mrilib.h"
#include "cluster_floatNOMASK.h"
#include "thd_segtools_fNM.h"

/*!
   \brief out_set = thd_polyfit( in_set,
                                 mask, polorder,
                                 prefix, verb);
          fits a polynomial model of order polorder to
          the time series of voxels in in_set
   \param in_set (THD_3dim_dataset* ) An AFNI dset pointer to input data
   \param mask (byte *)  if mask is not NULL then voxel i will be processed
                         if mask[i] != 0.
                         if mask is NULL then all voxels are processed.
   \param polorder (int) polynomial order
   \param prefix (char *) prefix of output dset
   \param verb (int) verbosity flag
   \return out_set (THD_3dim_dataset* ) Dset containing polynomial fits.
*/         
THD_3dim_dataset *thd_polyfit(THD_3dim_dataset *in_set,
                              byte *mask, int polorder,
                              char *prefix, int verb)
{
   int   i=0, j=0, nl=0, k=0,
         posi=0, posj=0, posk=0, nrow=0,
         ncol = 0;
   double xi=0.0, yi=0.0, yy=0.0, ei=0.0, sumsq=0.0,  med=0.0;
   gsl_matrix *X=NULL, *cov=NULL;
   gsl_vector *y=NULL, *w=NULL, *c=NULL;
   MRI_IMAGE *im = NULL;
   THD_3dim_dataset *out_set=NULL;
   double *dar = NULL;
   float *cbuf=NULL;
   float *dvec = NULL;
   gsl_multifit_linear_workspace *work=NULL;

   ENTRY("thd_polyfit");

   /* prepare output */
   out_set = EDIT_empty_copy(in_set) ;
   EDIT_dset_items(  out_set ,
                     ADN_nvals     , polorder           ,
                     ADN_ntt       , polorder          ,
                     ADN_datum_all , MRI_float      ,
                     ADN_brick_fac , NULL           ,
                     ADN_prefix    , prefix ? prefix : "OMG!"   ,
                     ADN_none ) ;

   for( j=0 ; j < polorder ; j++ ) /* create empty bricks to be filled below */
      EDIT_substitute_brick( out_set , j , MRI_float , NULL ) ;


   /* do the fitting */
   if (verb) fprintf (stderr,"Now fitting...\n");

   ncol = DSET_NVALS(in_set);
   nrow = DSET_NVOX(in_set);
   
   X = gsl_matrix_alloc (ncol, polorder);
   y = gsl_vector_alloc (ncol);
     
   c = gsl_vector_alloc (polorder);
   cov = gsl_matrix_alloc (polorder, polorder);
     
   for (i = 0; i < ncol; i++)  {
      xi = i+1;
      gsl_matrix_set (X, i, 0, 1.0);
      gsl_matrix_set (X, i, 1, xi);
      gsl_matrix_set (X, i, 2, xi*xi);

      gsl_matrix_set (X, i, 3, xi*xi*xi);
      gsl_matrix_set (X, i, 4, xi*xi*xi*xi);
      //    printf ("%lg ",xi);
   }


    /*make header
      printf ("matrvola\n"); */
    if (verb > 1) 
      fprintf(stdout, "#%s_0\t%s_1\t%s_2\t%s_3\t%s_4\n",
                    DSET_PREFIX(in_set),DSET_PREFIX(in_set),
                    DSET_PREFIX(in_set),DSET_PREFIX(in_set),
                    DSET_PREFIX(in_set));

    /* go by lines - signatures
       pre-allocate, I think this should be just fine, 
       there should be no need to reinitialize work 
       all the time */   
    work = gsl_multifit_linear_alloc (ncol, polorder);

    dvec = (float * )malloc(sizeof(float)*ncol) ;  /* array to hold signature */
    cbuf = (float *)malloc(sizeof(float)*polorder) ;  
                              /* array to hold fit */
    for (nl=0; nl<nrow; ++nl) {
      if (!mask || mask[nl]) {
         posi = -1;
         posj = -1;
         posk = -1;

         THD_extract_array( nl , in_set , 0 , dvec ) ; 
                                    /*get signature from voxel */

         for (k = 0; k < ncol; k++) {
            gsl_vector_set (y, k, dvec[k]);
         }

         gsl_multifit_linear (X, y, c, cov,
                              &sumsq, work);

         /* printf ( "\n # best fit: Y = %g + %g X + %g X^2 +%g X^3 + %g X^4\n",
                     C(0), C(1), C(2), C(3), C(4));
            printf ("# sumsq = %g\n", sumsq); */

         for (i=0;i<polorder;++i) cbuf[i] = (float)C(i);
         THD_insert_series( nl , out_set , polorder , MRI_float , cbuf , 1 ) ; 
                                       /* stick result in output */
         if (verb > 1) 
            fprintf (stdout,  "%11g\t%11g\t%11g\t%11g\t%11g\n", 
                           C(0), C(1), C(2), C(3), C(4)); 

         /*
         printf ("# covariance matrix:\n");
         printf ("[ %+.5e, %+.5e, %+.5e  \n",
                 COV(0,0), COV(0,1), COV(0,2));
         printf ("  %+.5e, %+.5e, %+.5e  \n", 
                 COV(1,0), COV(1,1), COV(1,2));
         printf ("  %+.5e, %+.5e, %+.5e ]\n", 
                 COV(2,0), COV(2,1), COV(2,2));
          printf ("# chisq = %g\n", chisq);
         */
      
      }
   }
   gsl_multifit_linear_free (work); work = NULL;
   free(dvec); dvec = NULL;
   free(cbuf); cbuf = NULL;
   gsl_vector_free (y);
   gsl_vector_free (c);
   gsl_matrix_free (cov);
   gsl_matrix_free (X);
   //gsl_vector_free (w);
   free(dvec); dvec = NULL;

   RETURN(out_set);
}


/**********************************************************************
   BEGIN: functions based on command.c code from The C clustering library.
   ANDREJ: If we end up using this, we must include copyright notice
   in AFNI prominently 
**********************************************************************/

void clusterlib_display_version(void)
{ printf ("\n"
"Clustering for segmentation, command line version ,\n"
"using the C Clustering Library version 1.38.\n"
"\n"
"Cluster was originally written by Michael Eisen (eisen 'AT' rana.lbl.gov)\n"
"Copyright 1998-99 Stanford University.\n");
  printf ("\n"
"The command line version of Clustering version was created by Andrej Vovk\n"
"and will be surely corrected by Ziad Saad ;),\n"
"\n");
  return;
}

char* clusterlib_setjobname(const char* basename, int strip)
{ char* jobname=NULL; char* extension=NULL;
  int n = strlen(basename);
  if (strip)
  { extension = strrchr(basename, '.');
    if (extension) n -= strlen(extension);
  }
  jobname = malloc((n+1)*sizeof(char));
  strncpy (jobname, basename, n);
  jobname[n] = '\0';
  
  if (strip) { /* ZSS, also remove +orig? */
   n = strlen(jobname);
   extension = strstr(jobname,"+orig");
   if (!extension) extension = strstr(jobname,"+acpc");
   if (!extension) extension = strstr(jobname,"+tlrc");
   if (extension) n -= strlen(extension);
   jobname[n] = '\0';
  }
  return jobname;
}

int clusterlib_readnumber(const char word[])
{ char* error = 0;
  long value = strtol(word,&error,0);
  if (*error=='\0') return (int)value;
  else return -1;
}

char clusterlib_getmetric(int i)
{ switch (i)
  { case 1: return 'u';
    case 2: return 'c';
    case 3: return 'x';
    case 4: return 'a';
    case 5: return 's';
    case 6: return 'k';
    case 7: return 'e';
    case 8: return 'b';
    default: return '\0';
  }
  /* Never get here */
  return '\0';
}

/* ========================================================================= */



float** example_distance_gene(int nrows, int ncols, float** data)
/* Calculate the distance matrix between genes using the Euclidean distance. */
{ int i, j, ii, nl, nc;
  float** distMatrix;
  float* weight = malloc(ncols*sizeof(float));
  


  printf("============ Euclidean distance matrix between genes ============\n");
  for (i = 0; i < ncols; i++) weight[i] = 1.0;
  distMatrix = distancematrix(nrows, ncols, data, weight, 'e', 0); 
         /* ANDREJ: The compiler gave me a warning that: 
         thd_segtools.c: In function 'example_distance_gene':
         thd_segtools.c:234: warning: assignment makes pointer from integer
                                    without a cast
         This led me to realize that the compiler did not have the prototype
         of this function before reaching this point and assumed the returned
         value was an int to be typecast to float **.
         But int may not be enough to hold a pointer anymore  
         
         In any case, there just is not enough memory for this operation here 
         so we'll have to consider whether this is useful enough to pursue
         in different ways. */

  if (!distMatrix)
  { printf ("Insufficient memory to store the distance matrix\n");
    free(weight);
    return NULL;
  }
  /*printf("   Gene:");
  for(i=0; i<nrows-1; i++) printf("%6d", i);
  printf("\n");
  for(i=0; i<nrows; i++)
     { printf("Gene %2d:",i);
    for(j=0; j<i; j++) printf(" %5.2f",distMatrix[i][j]);
    printf("\n");
  }
  printf("\n");
    */
  
 
 
  free(weight);
  return distMatrix;
}


/* ========================================================================= */

void example_hierarchical( int nrows, int ncols, 
                           float** data, 
                           char* jobname, 
                           int k, float** distmatrix,
                           int *clusterid)
/* Perform hierarchical clustering ... , float** distmatrix */
{ int i, ii, nl, nc;
  const int nnodes = nrows-1;
  float* weight = malloc(ncols*sizeof(float));
  Node* tree;
  
  char* filename;
  //char* filename2;


  for (i = 0; i < ncols; i++) weight[i] = 1.0;
  printf("\n");


  FILE *out1;

  int n = 1 + strlen(jobname) + strlen("_C") + strlen(".ext");

  if (k)
    { int dummy = k;
      do n++; while (dummy/=10);
    }
    

  filename = malloc(n*sizeof(char));
  
  sprintf (filename, "%s_C%d.hie", jobname, k);
  out1 = fopen( filename, "w" );

  /*FILE *out2;
  filename2 = malloc(n*sizeof(char));
  
  sprintf (filename2, "%s_C%d.hi1", jobname, k);
  out2 = fopen( filename2, "w" );*/

  //HERE SHOULD USE method instead of 'xxx' (s,m,a,c)


  printf("================ Pairwise single linkage clustering ============\n");
  /* Since we have the distance matrix here, we may as well use it. */
  tree = treecluster(nrows, ncols, 0, 0, 0, 'e', 's', distmatrix);
  /* The distance matrix was modified by treecluster, so we cannot use it any
   * more. But we still need to deallocate it here.
   * The first row of distmatrix is a single null pointer; no need to free it.
   */
  for (i = 1; i < nrows; i++) free(distmatrix[i]);
  free(distmatrix);
  if (!tree)
  { /* Indication that the treecluster routine failed */
    printf ("treecluster routine failed due to insufficient memory\n");
    free(weight);
    return;
  }

  #if 0
   /* Andrej: This block looked like it was commented out
    I took out some of the * / because they 
    were generating warning and blocked out the 
    entire section with #if 0 . 
    The compiler will not compile this section */
    
  fprintf(out2,"Node     Item 1   Item 2    Distance\n");
  for(i=0; i<nnodes; i++)
    fprintf(out2,"%3d:%9d%9d      %g\n",
           -i-1, tree[i].left, tree[i].right, tree[i].distance);
	   printf("\n");
	   fclose(out2);
  //free(tree);

  
  printf("================ Pairwise maximum linkage clustering ============\n");
  tree = treecluster(nrows, ncols, data, weight, 0, 'e', 'm', 0);
  /* Here, we let treecluster calculate the distance matrix for us. In that
   * case, the treecluster routine may fail due to insufficient memory to store
   * the distance matrix. For the small data sets in this example, that is
   * unlikely to occur though. Let's check for it anyway:
   */
  if (!tree)
  { /* Indication that the treecluster routine failed */
    printf ("treecluster routine failed due to insufficient memory\n");
    free(weight);
    return;
  }
  printf("Node     Item 1   Item 2    Distance\n");
  for(i=0; i<nnodes; i++)
    printf("%3d:%9d%9d      %g\n",
           -i-1, tree[i].left, tree[i].right, tree[i].distance);
  printf("\n");
  free(tree);



  printf("================ Pairwise average linkage clustering ============\n");
  tree = treecluster(nrows, ncols, data, weight, 0, 'e', 'a', 0); 
  if (!tree)
  { /* Indication that the treecluster routine failed */
    printf ("treecluster routine failed due to insufficient memory\n");
    free(weight);
    return;
  }
  printf("Node     Item 1   Item 2    Distance\n");
  for(i=0; i<nnodes; i++)
    printf("%3d:%9d%9d      %g\n",
           -i-1, tree[i].left, tree[i].right, tree[i].distance);
  printf("\n");
  free(tree);



  printf("================ Pairwise centroid linkage clustering ===========\n");
  tree = treecluster(nrows, ncols, data, weight, 0, 'e', 'c', 0); 
  if (!tree)
  { /* Indication that the treecluster routine failed */
    printf ("treecluster routine failed due to insufficient memory\n");
    free(weight);
    return;
  }
  printf("Node     Item 1   Item 2    Distance\n");
  for(i=0; i<nnodes; i++)
    printf("%3d:%9d%9d      %g\n",
           -i-1, tree[i].left, tree[i].right, tree[i].distance);
  printf("\n");

  #endif



  printf("=============== Cutting a hierarchical clustering tree ==========\n");
  clusterid = malloc(nrows*sizeof(int));
  printf(" number of clusters %d \n",k);
  cuttree (nrows, tree, k, clusterid);
  for(i=0; i<nrows; i++)
  fprintf(out1, "%09d\t%2d\n", i, clusterid[i]);
  fprintf(out1, "\n");
  fclose(out1);


   free(tree); 
  free(weight);
  return;
}

/* ========================================================================= */

void getvoxlclusterdist(int* count, float** cdata, 
			int* clusterid, float** data, char* jobname, 
			int nclusters, int nrows, int ncols, float** vcdata,
         char dist)
{
   int i, j, n;
   char* filename4;
   FILE *out4=NULL;
   float difference, difference1;
   float* max_vcdata = NULL;
   int dummy = 0;
   char* filename5;
   float distance = 0.0;
   float (*metric)
      (int, float**, float**, const float[], int, int, int) =
         setmetric(dist);
   float *weight=NULL;
   
   FILE *out5=NULL;

   ENTRY("getvoxlclusterdist");
   /* allocate for answer arrays */
   if (!(max_vcdata = (float *)calloc(sizeof(float), nclusters))) {
      fprintf(stderr,"ERROR: Failed to allocate for max_vcdata\n");
      EXRETURN;
   }


  n = 1 + strlen(jobname) + strlen("_K_G") + strlen(".ext");
  
  dummy = nclusters;
  do n++; while (dummy/=10);
    
  /*avovk, june 09, changed output names*/
  
  filename4 = malloc(n*sizeof(char));
  sprintf (filename4, "%s_K%d_Gx.vcd.1D", jobname, nclusters);

  out4 = fopen( filename4, "w" );

  filename5 = malloc((n+2)*sizeof(char));

  sprintf (filename5, "%s_K%d_Gx.info2.1D", jobname, nclusters);
  out5 = fopen( filename5, "w" );   /* ZSS: This also was "a" 
                                    but I don't think it was necessary */
  weight = (float *)calloc(ncols, sizeof(float));
  for (i = 0; i < ncols; ++i) weight[i] = 1.0;
  
  for (i = 0; i < nrows; i++){
    #if 0      /* ZSS, distances should be same for clustering */
       difference = 0;
       difference1 = 0;
       for (j = 0; j < ncols; j++) {
         difference1 = cdata[clusterid[i]][j]-data[i][j];
         difference = difference + difference1*difference1;
       }
       vcdata[i][0] = 100/(1+sqrt(difference)); /* if values are close to 0 ?! */
    #else   
      distance = metric(ncols, cdata, data, weight, clusterid[i], i, 0);
      vcdata[i][0] = distance;   /* ZSS: Don't know what you want to do here */
    #endif
  }

  /* avovk JULY29_2008, sept 05 */ 
  for (i = 0; i < nclusters; i++){
    max_vcdata[i] = 0;
  }

  for (i = 0; i < nrows; i++){
    if (vcdata[i][0] > max_vcdata[clusterid[i]]) {
       max_vcdata[clusterid[i]] = vcdata[i][0];
    }
  }

  printf ("------- writing  max distances within clusters to file:\t\t"
          " %s_K_G%d.info2.1D", jobname, nclusters);
  for (i = 0; i < nclusters; i++){
    printf("%7.3f\n",max_vcdata[i]);
    fprintf(out5, "#cluster %d:\n"
                  "%d   %7.3f\n",i, i, max_vcdata[i]);
  }

  for (i = 0; i < nrows; i++){
    difference = vcdata[i][0];
    vcdata[i][0] = 100*clusterid[i]+100*difference/(max_vcdata[clusterid[i]]);
  }
  /* avovk JULY29_2008, sept 05 */

  printf ("------- writing voxels-centroids distances to file:\t\t"
          " %s_K_G%d.vcd.1D\n",jobname, nclusters);
  for (i = 0; i < nrows; i++)
    fprintf (out4, "%09d\t%7.3f\n", i, vcdata[i][0]);
  fclose(out4); out4=NULL;
  fclose(out5); out5=NULL;

  EXRETURN;

}
/* ========================================================================= */


void getvoxlclustersdist(int* count, float** cdata, 
			int* clusterid, float** data, char* jobname, 
			int nclusters, int nrows, int ncols, float **vcdata,
         char dist)
{
   int i, j, k;
   float distance = 0.0, *weight=NULL;
   float (*metric)
      (int, float**, float**, const float[], int, int, int) =
         setmetric(dist);


   weight = (float *)calloc(ncols, sizeof(float));
   for (i = 0; i < ncols; ++i) weight[i] = 1.0;
   
   for (k = 0; k < nclusters; k++){
      for (i = 0; i < nrows; i++){
      #if 0 /* ZSS: distances should be same for clustering */
         difference = 0;
         difference1 = 0;
         for (j = 0; j < ncols; j++) {   
            /*ZSS: Did you CHECK THIS LOOP ? ; j is going through fitcoef*/
      difference1 = cdata[k][j]-data[i][j];
      difference = difference + difference1*difference1;
         }
         vcdata[i][k+1] = 100/(1+sqrt(difference)); /* actually 1/distance */
      #else
         distance = metric(ncols, cdata, data, weight, k, i, 0);
         vcdata[i][k+1] = distance;/* ZSS: Don't know what you want to do here */
      #endif
      }   /* some kind of scaled probability */
   }
  
  return;

}


/* avovkJULY30: it would be nice to have color pallete for cluster probability */

void color_palette(int nclusters, char* jobname)
{
  int n = 0;
  int i, j;
  char* filename=NULL;
  FILE *out=NULL;
  float a, c;
  int nsteps, step, colorv, hexp1, hexp2;
  char* hexnumbers=NULL;
  
  hexnumbers = (char *)malloc(32*sizeof(char)); /* ANDREJ: you had 16 here, but that is too short
                                           because sprintf will add a terminating NULL character
                                           = '\0' at the end, pushing you to 17 chars .
                                           I used 32 which is overkill but it is a nice number */
  sprintf (hexnumbers, "0123456789abcdef");

  n = 1 + strlen(jobname) + strlen(".pal") + 2;
  filename = (char *)malloc(n*sizeof(char));
  sprintf (filename, "%s_K%d.pal", jobname,nclusters); 
  /* output file name not good ! */
  if (!(out = fopen( filename, "w" ))) {
   fprintf(stderr,"Failed to open %s for writing\n", filename);
  } 
  
  c = nclusters;
  
  nsteps = 256/nclusters;

  /*printf("num of color steps per cluster: %d \n",nsteps); */

  /* now we use those steps to create color fading to/from black*/
  
  step = 256/nsteps - 2; /* number of clusters -1, that we don't go to black*/

  if (nclusters < 3) 
    step = 256/nsteps - 1;


  fprintf (out, "color_%d_clusters\n",nclusters);

  /* for ( i = 0; i < nclusters; i++) {*/
  
  colorv = 255;
  for ( j = 0; j < nsteps; j++) {
    colorv = colorv - step;
    hexp1 = colorv/16;
    hexp2 = colorv - hexp1*16;

    fprintf (out, "#%c%c0000\n", hexnumbers[hexp1], hexnumbers[hexp2]);

  }


  if (nclusters > 1) {
    colorv = 255;
    for ( j = 0; j < nsteps; j++) {
      colorv = colorv - step;
      hexp1 = colorv/16;
      hexp2 = colorv - hexp1*16;
      
      fprintf (out, "#00%c%c00\n", hexnumbers[hexp1], hexnumbers[hexp2]);
      
    }
  }

  if (nclusters > 2) {
    colorv = 255;
    for ( j = 0; j < nsteps; j++) {
      colorv = colorv - step;
      hexp1 = colorv/16;
      hexp2 = colorv - hexp1*16;
      
      fprintf (out, "#0000%c%c\n", hexnumbers[hexp1], hexnumbers[hexp2]);
      
    }
  }


  if (nclusters > 3) {
    colorv = 255;
    for ( j = 0; j < nsteps; j++) {
      colorv = colorv - step;
      hexp1 = colorv/16;
      hexp2 = colorv - hexp1*16;
      
      fprintf (out, "#%c%c%c%c00\n", hexnumbers[hexp1], hexnumbers[hexp2], 
	       hexnumbers[hexp1], hexnumbers[hexp2]);
      
    }
  }

  if (nclusters > 4) {
    colorv = 255;
    for ( j = 0; j < nsteps; j++) {
      colorv = colorv - step;
      hexp1 = colorv/16;
      hexp2 = colorv - hexp1*16;
      
      fprintf (out, "#00%c%c%c%c\n", hexnumbers[hexp1], hexnumbers[hexp2], 
	       hexnumbers[hexp1], hexnumbers[hexp2]);
      
    }
  }

  if (nclusters > 5) {
    colorv = 255;
    for ( j = 0; j < nsteps; j++) {
      colorv = colorv - step;
      hexp1 = colorv/16;
      hexp2 = colorv - hexp1*16;
      
      fprintf (out, "#%c%c00%c%c\n", hexnumbers[hexp1], hexnumbers[hexp2], 
	       hexnumbers[hexp1], hexnumbers[hexp2]);
      
    }
  }

  if (nclusters > 6) {
    printf("COLOR PALETTE CAN HANDLE MAX & CLUSTERS FOR NOW!!!");
  }
  
  printf ("------- Color palette written to file:\t\t"
	  "%s_K%d.pal\n",jobname,nclusters);

  fclose(out); out=NULL;
   free(hexnumbers); hexnumbers  = NULL; /* clean up */
}

/* ========================================================================= */

void example_kmeans( int nrows, int ncols, 
                     float** data, 
                     int nclusters, int npass, 
                     char dist, char* jobname,
                     int *clusterid, float **vcdata)
/* Perform k-means clustering on genes */
{ 
   int i, j, ii, nl, nc;
   //const int nclusters = 3;
   const int transpose = 0;
   //const char dist = 'e';
   const char method = 'a';
   /* For method=='a', the centroid is defined as the mean over all elements
     belonging to a cluster for each dimension.
     For method=='m', the centroid is defined as the median over all elements
     belonging to a cluster for each dimension.
   */

   //int npass = 1;
   int ifound = 0;
   int test=0;
   float error;
   float distance;
   int** index;
   int* count;
   float* weight = malloc(ncols*sizeof(float));
   float** cdata = malloc(nclusters*sizeof(float*));
   
   int n=0;
   char* filename;
   char* filename2;
   char* filename3;
   char* filename4;
   FILE *out1=NULL;
   FILE *out2=NULL;
   FILE *out3=NULL;
   FILE *out4=NULL;
   float a, b;
   
   
   for (i = 0; i < nclusters; i++)
   { cdata[i] = malloc(ncols*sizeof(float));
    
   }
   for (i = 0; i < ncols; i++) weight[i] = 1.0;

   
   n = 1 + strlen(jobname) + strlen("_K_G") + strlen(".ext");

   if (dist)
   { int dummy = nclusters;
    do n++; while (dummy/=10);
   }
    
   //avovk 
   printf("a je u omari :) \n");
   filename = (char *)malloc(n*sizeof(char));
   filename2 = (char *)malloc(n*sizeof(char));
   filename3 = (char *)malloc(n*sizeof(char));
   filename4 = (char *)malloc((n+2)*sizeof(char));

   /* ZSS:  Put a .1D in the name wherever it is appropriate */
   sprintf (filename, "%s_K%d_G%c.kgg.1D", jobname, nclusters,dist);

   out1 = fopen( filename, "w" );

   sprintf (filename2, "%s_K%d_G%c.dis.1D", jobname, nclusters,dist);

   out2 = fopen( filename2, "w" );

   sprintf (filename3, "%s_K%d_G%c.cen.1D", jobname, nclusters,dist);
   out3 = fopen( filename3, "w" );
   sprintf (filename4, "%s_K%d_G%c.info1.1D", jobname, nclusters,dist);
   out4 = fopen( filename4, "w" );

   printf("======================== k-means clustering"
         " ========================\n");

   printf ("\n");
   printf ("----- doing %d pass-es... go stretch your legs...\n",npass);
   //npass = 3;

   kcluster(   nclusters,
               nrows,ncols,data,
               weight,
               transpose,npass,
               method,dist,
               clusterid, &error, &ifound);
   printf ("Solution found %d times; ", ifound);
   printf ("within-cluster sum of distances is %f\n", error);

   fprintf (out4,"#within-cluster sum of distances: %f\n",error);
   fclose(out4); out4=NULL;

   printf ("------- writing Cluster assignments to file:\t\t"
          " %s_K_G%d.kgg.1D\n",jobname, nclusters);
   for (i = 0; i < nrows; i++)
    fprintf (out1, "%09d\t %d\n", i, clusterid[i]);
   fclose(out1); out1=NULL;

   printf ("------- writing Distance between clusters to file:\t "
          "%s_K_G%d.dis.1D \n", jobname, nclusters);
   fprintf (out2,"#------- Distance between clusters:\n");
   index = (int **)malloc(nclusters*sizeof(int*));
   count = (int *)malloc(nclusters*sizeof(int));
   for (i = 0; i < nclusters; i++) count[i] = 0;
   for (i = 0; i < nrows; i++) count[clusterid[i]]++;
   for (i = 0; i < nclusters; i++) index[i] = malloc(count[i]*sizeof(int));
   for (i = 0; i < nclusters; i++) count[i] = 0;
   for (i = 0; i < nrows; i++){ 
      int id = clusterid[i];
      index[id][count[id]] = i;
      count[id]++;
   }  
   
   for (i = 0; i < nclusters-1; i++)
     {
       for (j = 1+i; j < nclusters; j++)
	 {
	   distance = clusterdistance(nrows, ncols, data, 
				      weight, count[i], count[j], index[i], 
				      index[j], dist , 'a', 0);  /*ZSS: You had 'e' where I put dist
                                                    I don't know how much, if at
                                                    all, you used this output. */
                                                    
	   fprintf(out2,"#Distance between %d and %d: %7.3f\n", 
		   i, j, distance);
	   
	 }
     }

   fclose(out2); out2=NULL;

   printf ("------- writing Cluster centroids to file:\t\t"
          "%s_K_G%d.cen.1D\n",jobname, nclusters);

   /*fprintf (out3,"------- Cluster centroids:\n");*/
   getclustercentroids(nclusters, nrows, ncols, data, clusterid,
                      cdata, 0, 'a');
   /*   fprintf(out3,"   coefficients:");
	for(i=0; i<ncols; i++) fprintf(out3,"\t%7d", i);
	fprintf(out3,"\n");*/
   for (i = 0; i < nclusters; i++){ 
     /*      fprintf(out3,"Cluster %2d:", i);*/
      for (j = 0; j < ncols; j++) fprintf(out3,"\t%7.3f", cdata[i][j]);
      fprintf(out3,"\n");
   }
   fclose(out3); out3=NULL;
   printf("Done...\n");

   /* call function to calculate distance between each voxel and centroid */
   /* we will need: 
      count - number of elements in cluster as we allready have it
      cdata - cluster centroids
      clusterid
      data */

      /* ZSS:  
               Double check on your computations with vcdata
               below the call to color_palette and let us discuss
               what you are doing with them.
               
       */
      
             
      getvoxlclusterdist(count, cdata, clusterid, data, jobname, 
		      nclusters, nrows, ncols, vcdata, dist);
      getvoxlclustersdist(count, cdata, clusterid, data, jobname, 
		      nclusters, nrows, ncols, vcdata, dist);

      color_palette(nclusters, jobname);
      
      /*might want to make some calculations with vcdata*/

      /*lets calculate distance to centroid/sum(distance 2 all centroids)*/

   #if 0
      for (i = 0; i < nrows; i++)
	{
	  a = 0;
	  for (j = 0; j < nclusters; j++)
	    a += vcdata[i][j+1];
	  vcdata[i][nclusters+1] = 1000*vcdata[i][0]/a;
	}
   #else    /* ZSS: What is the purpose of all this ? */
      for (i = 0; i < nrows; i++)
	{
	  a = vcdata[i][0];
     b = a+1e10;
	  for (j = 0; j < nclusters; j++)
	    if (vcdata[i][j+1]>a && vcdata[i][j+1]< b) b = vcdata[i][j+1];
	  vcdata[i][nclusters+1] = (1.0-a/b)*100.0;
	}
   #endif
      /* just a notice: first column is dist to centroid in THE cluster,
	 then there are columns that describe distance to centroids from
         each cluster centroid 
         - and we are adding another column here*/

   for (i = 0; i < nclusters; i++) free(index[i]);
   free(index);
   free(count);

   for (i = 0; i < nclusters; i++){ 
      free(cdata[i]);
   }
   
   free(cdata);
   free(weight);

   return;
}

/**********************************************************************
   END: functions based on command.c code from The C clustering library.
**********************************************************************/
# define EPSILON             1E-10
int thd_Adist (  THD_3dim_dataset *in_set,
                 byte *mask, 
                 float *sigs, int nsigs,
                 THD_3dim_dataset **dist_set,
                 OPT_KMEANS oc)
{ 
   int ii, jj, kk;
   int ncol = -1, nvox = -1;
   float *distvec=NULL, **asigvec=NULL, **afsig=NULL;
   char lll[25]={"buffer"};
   float *weight = NULL, factor =0.0;
   float (*metric)
    (int, float**, float**, const float[], int, int, int) =
       setmetric(oc.distmetric);


   ENTRY("thd_Adist");
   
   nvox = DSET_NVOX(in_set);
   ncol = DSET_NVALS(in_set); 
   if (ncol < DSET_NUM_TIMES(in_set)) ncol = DSET_NUM_TIMES(in_set);
   
   /* make sure in_set is loaded */
   DSET_load(in_set);
   
   
   /* equal weights for all elements of signature */
   weight = (float *)calloc(ncol, sizeof(float));
   for (ii = 0; ii < ncol; ii++) weight[ii] = 1.0;
   
   /*array to hold one voxel's signature */
   asigvec = (float ** )calloc(1, sizeof(float*));
   asigvec[0] = (float *)calloc(ncol, sizeof(float)) ;
   
   /* array to hold signatures in a way pleasing to metric */
   afsig = (float **)calloc(nsigs, sizeof(float*));
   ii=0;
   for (jj=0; jj<nsigs; ++jj) {
      afsig[jj] =(float *)calloc(ncol, sizeof(float));
      for (kk=0; kk<ncol; ++kk) {
         afsig[jj][kk] = sigs[ii]; ++ii;
      }
   }   
   /* Create temporary distance vector */
   distvec = (float *)calloc(nvox, sizeof(float));
   
   /* prepare output */
   *dist_set = EDIT_empty_copy(in_set) ;
   EDIT_dset_items(  *dist_set ,
                     ADN_nvals     , nsigs           ,
                     ADN_ntt       , nsigs          ,
                     ADN_datum_all , MRI_short      ,
                     ADN_brick_fac , NULL           ,
                     ADN_prefix    , "adist"   ,
                     ADN_none ) ;
   
   for (jj=0; jj<nsigs; ++jj) {  /* for each signature type */
      for (ii=0; ii< nvox; ++ii) {
         distvec[ii]=0;
         if (!mask || mask[ii]) {
            THD_extract_array( ii , in_set , 0 , asigvec[0] ) ; 
            /* check if it is all non zero */
            for (kk=0; kk<ncol; ++kk) {
               if (asigvec[0][kk]) { /* non zero, calculate the distance */
                  distvec[ii] = metric(ncol, afsig, asigvec, weight,0, 0, 0);
                  break; /* get out of the loop */
               }
            }
            if (oc.voxdebug[3] == ii) {
               char fname[128]={""};
               FILE *fout=NULL;
               sprintf(fname, "distdbg.vox%d-%d-%d.sig%d.1D", 
                  oc.voxdebug[0], oc.voxdebug[1], oc.voxdebug[2], jj);
               if ((fout = fopen(fname, "w"))) {
                  fprintf(fout,"#Col. 0 signature for voxel %d %d %d\n"
                               "#Col. 1 reference signature %d\n"
                               "#Distance per metric %c = %f\n",
                               oc.voxdebug[0], oc.voxdebug[1], oc.voxdebug[2], 
                               jj, oc.distmetric, distvec[ii]);
                  for (kk=0; kk<ncol; ++kk){
                     fprintf(fout, "%f   %f\n", asigvec[0][kk], afsig[jj][kk]);
                  }
                  ININFO_message("Voxel %d %d %d is in mask \n"
                           "See file %s for debug info with signature %d/%d\n",
                                 oc.voxdebug[0], oc.voxdebug[1], oc.voxdebug[2],
                                 fname, jj,nsigs);
                  fclose(fout); fout = NULL;
               } else {
                  ERROR_message("No permission for writing? No debug output.");
               }
            }
         } else if (oc.voxdebug[3] == ii) {
            ININFO_message("Voxel %d %d %d is not in mask \n ",
                           oc.voxdebug[0], oc.voxdebug[1], oc.voxdebug[2]);
         }
      }
      /* put distvec back into output, and watch the scaling */
      EDIT_substitute_brick( *dist_set , jj , MRI_short , NULL ) ;
      factor = EDIT_coerce_autoscale_new(nvox,  
                                MRI_float,distvec, 
                                MRI_short,
                                 (short*)DSET_BRICK_ARRAY(*dist_set,jj));
      if (factor < EPSILON)  factor = 0.0;
      else factor = 1.0 / factor;
      if( DSET_BRICK_TYPE(*dist_set,jj) == MRI_short )
      EDIT_misfit_report( DSET_FILECODE(*dist_set) , jj ,
                          nvox , factor , 
                          (short*)DSET_BRICK_ARRAY(*dist_set,jj) ,  distvec) ;
      if (oc.verb) {
         ININFO_message("Subbrick factor for %d is %f\n ",
                        jj, factor);
      }
      sprintf(lll,"Dist.%c.Sig.%02d", oc.distmetric, jj);
      EDIT_dset_items (*dist_set, ADN_brick_label_one + jj, lll, ADN_none);             EDIT_BRICK_FACTOR (*dist_set, jj, factor);
    
   }
         
   if (oc.verb) {
      ININFO_message("Have %d/%d voxels to process "
                     "with %d dimensions per voxel.\n",
                     nvox, DSET_NVOX(in_set), ncol);
   }
   
   
   if (oc.verb) {
      ININFO_message("Freedom");
   }
      
   free(weight); weight = NULL;
   free(distvec); distvec=NULL;
   free(asigvec[0]); asigvec[0]=NULL;
   free(asigvec); asigvec=NULL;
   for (jj=0; jj<nsigs; ++jj) free(afsig[jj]); free(afsig);

   RETURN(1);
}

/*!
   Andrej: Put some help like for function thd_polyfit
   You can form the data array Dp outside of this function,
   so if Dp is not NULL, then only header structure of in_set
   is used. If Dp is NULL then it is formed inside the function
   from the contents of in_set 
   D_ncol is used to pass the number of columns in Dp. Note that
   D_ncol may be different from the number of sub-bricks in in_set
   because when Dp is filled externally, in_set can be one of the volumes
   used to form Dp.
*/
int thd_Acluster (  THD_3dim_dataset *in_set,
                  byte *mask, int nmask,
                  THD_3dim_dataset **clust_set,
                  THD_3dim_dataset **dist_set,
                  OPT_KMEANS oc,
                  float **Dp, int D_ncol)
{
   int ii, nl, nc, j;
   float **D=NULL, **distmatrix=NULL;  /* this double business is a waste of
                                           memory, at least for D..*/
   int ncol = -1;
   float *dvec=NULL;
   float factor = 0.0;
   int* clusterid = NULL;
   short *sc = NULL; 
   float *fc = NULL;
   float** vcdata = NULL; 
   int nvc; /*this will be for number of columns in vcdata matrix*/
   char lll[25]={"buffer"};


   ENTRY("thd_Acluster");
   
   if (!clust_set || *clust_set) {
      fprintf(stderr,
               "ERROR: output volume pointer pointers must point to NULL\n");
      RETURN(0);
   }
   if (!mask) nmask = DSET_NVOX(in_set);
   
   if (!Dp) {
      ncol = DSET_NVALS(in_set); 
      if (ncol < DSET_NUM_TIMES(in_set)) ncol = DSET_NUM_TIMES(in_set);

      /* make sure in_set is loaded */
      DSET_load(in_set);
      
      /* Create data matrix */
      D = (float **)calloc(sizeof(float*), nmask);
      for (ii=0;ii<(nmask);++ii) {
         if (!(D[ii] = (float *)calloc(sizeof(float), ncol))) {
            fprintf(stderr,"ERROR: Failed while allocating %dx%d float matrix\n", 
                           nmask, ncol);
            RETURN(0);
         }
      }


      dvec = (float * )malloc(sizeof(float)*ncol) ;  /* array to hold series */
      if (oc.verb) {
         ININFO_message("Filling D(%dx%d) (mask=%p).\n", nmask, ncol, mask);
      }
      ii = 0;
      for (nl=0; nl<DSET_NVOX(in_set); ++nl) {
         if (!mask || mask[nl]) {
            THD_extract_array( nl , in_set , 0 , dvec ) ; 
            for (nc=0; nc<ncol; ++nc) D[ii][nc] = dvec[nc]; 
            ++ii;                              
         }
      }
      /* dump voxel values in in_set to free up memory, 
         but keep header of in_set */
      DSET_unload(in_set); 
   } else {
      ncol = D_ncol; 
      D = Dp;
   }   
   
   clust_seed(oc.rand_seed);
   if (oc.verb) {
      ININFO_message("Have %d/%d voxels to process "
                     "with %d dimensions per voxel.\n"
                     "Seed now: %d\n",
                     nmask, DSET_NVOX(in_set), ncol,
                     clust_seed(0));
   }
   
   /* allocate for answer arrays */
   if (!(clusterid = (int *)calloc(sizeof(int), nmask))) {
      fprintf(stderr,"ERROR: Failed to allocate for clusterid\n");
      RETURN(0);
   }

   nvc = oc.k+2;
   /* allocate for answer array distance voxel centroid */
   vcdata = (float **)calloc(sizeof(float*), nmask);
   for (ii=0;ii<(nmask);++ii) {
     if (!(vcdata[ii] = (float *)calloc(sizeof(float), nvc))) {
     fprintf(stderr,"ERROR: Failed to allocate for voxel cluster distance\n");
     RETURN(0);
     }
   }

   /* now do the clustering 
     (ANDREJ: I do not know why the counting skipped 1st row and 1st col....)
   ZIAD: because input file was made this way that we had numbered lines in 1st
   column and column labels in first row. We should change this I agree.*/
   if (oc.k > 0) {
      if (oc.verb) {
         ININFO_message("Going to cluster: k=%d, r=%d\n"
                        "distmetric %c, jobname %s\n",
                        oc.k, oc.r, oc.distmetric, oc.jobname);
      }
      example_kmeans(   nmask, ncol, D, 
                        oc.k, oc.r, oc.distmetric, 
                        oc.jobname, clusterid, vcdata);
   } else if (oc.kh > 0) {
      if (oc.verb) {
         ININFO_message("Going to h cluster: kh=%d\n"
                        "jobname %s\n",
                        oc.kh, oc.jobname);
      }
      if ((distmatrix = example_distance_gene(nmask, ncol, D))) {
         example_hierarchical(   nmask, ncol, D, 
                                 oc.jobname, oc.kh, 
                                 distmatrix, 
                                 clusterid);
         /* ZSS: Andrej, this looks like a memory leak, 
                  distmatrix should be freed here.
                  This free code is not tested yet! */
         ININFO_message("The freeing below has not been tested");
         for (ii=0; ii<ncol; ++ii) free(distmatrix[ii]); 
         free(distmatrix); distmatrix = NULL;
      } else {
         ERROR_message("Failed to create distmatrix");
         RETURN(0);
      }
   } else {
      ERROR_message("Bad option selection");
      RETURN(0);
   }
   
   /* create output datasets, if required*/
   *clust_set = EDIT_empty_copy(in_set ) ;
   EDIT_dset_items(  *clust_set ,
                     ADN_nvals     , 1           ,
                     ADN_ntt       , 1          ,
                     ADN_datum_all , MRI_short      ,
                     ADN_brick_fac , NULL           ,
                     ADN_prefix    , "OML!"   ,
                     ADN_none ) ;
   if (oc.verb) {
      ININFO_message("loading results into %s\n",
                     DSET_PREFIX(*clust_set));
   }
   
   /* transfer ints in clusterid to shorts array */
   sc = (short *)calloc(sizeof(short),DSET_NVOX(in_set));
   ii = 0;
   for (nl=0; nl<DSET_NVOX(in_set); ++nl) {
      if (!mask || mask[nl]) {
         sc[nl] = (short)clusterid[ii]+1;
         ++ii;
      }
   }
   free(clusterid); clusterid = NULL;
   EDIT_substitute_brick( *clust_set , 0 , MRI_short , sc ) ;
   sc = NULL; /* array now in brick */
     
   
   /* prepare output */
   *dist_set = EDIT_empty_copy(in_set) ;
   EDIT_dset_items(  *dist_set ,
                     ADN_nvals     , nvc           ,
                     ADN_ntt       , nvc          ,
                     ADN_datum_all , MRI_short      ,
                     ADN_brick_fac , NULL           ,
                     ADN_prefix    , "vcd"   ,
                     ADN_none ) ;

   for( j=0 ; j < nvc ; j++ ) /* create empty bricks to be filled below */
        EDIT_substitute_brick( *dist_set , j , MRI_short , NULL ) ;


   ININFO_message("loading results into %s\n",
                  DSET_PREFIX(*dist_set));

   for (j = 0; j < nvc; j++) {
      ININFO_message("...%d,", j);
   #if 0 /* ZSS: Andrej, this may have caused some trouble for certain metrics
                 You don't want to just type cast a float value to short because
                 you may loose a whole lot of precision. THis will be especially
                 bad for the correlation metrics! 
                 I left this code here for you to see what happened and how it
                 may affect your impression of how well correlation might work 
                 
                 */  
      /* transfer data in vcdata to shorts array */
      /* LOOP to pick vcdata[][fromclust] */
         sc = (short *)calloc(sizeof(short),DSET_NVOX(in_set));
         ii = 0;
         for (nl=0; nl<DSET_NVOX(in_set); ++nl) {
	         if (!mask || mask[nl]) {
	            sc[nl] = (short)vcdata[ii][j]+1;
	            ++ii;
	         }
         }
         EDIT_substitute_brick( *dist_set , j , MRI_short , sc ) ; 
                                          /* stick result in output */
	      sc = NULL;  /*array now in brick */

   #else /* proper scaling */
      
      /* transfer data in vcdata to full float array */
      fc = (float *)calloc(sizeof(float),DSET_NVOX(in_set));
      ii = 0;
      for (nl=0; nl<DSET_NVOX(in_set); ++nl) {
	      if (!mask || mask[nl]) {
	         fc[nl] = (float)vcdata[ii][j]; /* ZSS: I took the +1 out here, 
                   I think you had it here because you cut and pasted the line
                   from the clust_dset ?  */
	         ++ii;
	      }
      }
      factor = EDIT_coerce_autoscale_new(DSET_NVOX(in_set),  
                             MRI_float, fc, 
                             MRI_short,
                              (short*)DSET_BRICK_ARRAY(*dist_set,j));
      if (factor < EPSILON)  factor = 0.0;
      else factor = 1.0 / factor;
      if( DSET_BRICK_TYPE(*dist_set,j) == MRI_short )
         EDIT_misfit_report( DSET_FILECODE(*dist_set) , j ,
                             DSET_NVOX(in_set) , factor , 
                             (short*)DSET_BRICK_ARRAY(*dist_set,j), fc) ;
      free(fc); fc = NULL;
      if (oc.verb) {
         ININFO_message("Subbrick factor for %d is %f\n ",
                        j, factor);
      }
   #endif
      /* label bricks */
      if (j==0) 
         EDIT_dset_items (*dist_set, ADN_brick_label_one + j, "Dc", ADN_none);
      else if (j < nvc -1) {
         sprintf(lll,"Dc%02d", j-1);
         EDIT_dset_items (*dist_set, ADN_brick_label_one + j, lll, ADN_none);
      } else {
         EDIT_dset_items (*dist_set, ADN_brick_label_one + j, 
                           "Dc_norm", ADN_none);
      }
      EDIT_BRICK_FACTOR (*dist_set, j, factor);
   }
   
   for (ii=0;ii<(nmask);++ii) {
      free(vcdata[ii]);
   }
   free(vcdata); vcdata = NULL;

   if (oc.verb) {
      ININFO_message("Freedom");
   }
   
   if (dvec) free(dvec); dvec=NULL;


   if (D != Dp) {
      // To free D 
      for (ii=0;ii<nmask;++ii) {
       if (D[ii]) free(D[ii]);
      }
      free(D); D = NULL;
   }else D = NULL;
   
   
   RETURN(1);
}

