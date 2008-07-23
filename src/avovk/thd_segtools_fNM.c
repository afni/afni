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
      printf ("matrvola\n");
      ZSS: By adding # to the text line, 
           I made the output file be a .1D format */
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
{ char* jobname;
  int n = strlen(basename);
  if (strip)
  { char* extension = strrchr(basename, '.');
    if (extension) n -= strlen(extension);
  }
  jobname = malloc((n+1)*sizeof(char));
  strncpy (jobname, basename, n);
  jobname[n] = '\0';
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
			int nclusters, int nrows, int ncols, float** vcdata)
{
  int i, j, n;
  char* filename4;
  FILE *out4=NULL;
  /*float* vcdata = malloc(nrows*sizeof(float*));*/
    float difference, difference1;

  n = 1 + strlen(jobname) + strlen("_K_G") + strlen(".ext");
  
  int dummy = nclusters;
  do n++; while (dummy/=10);
    
    
  filename4 = malloc(n*sizeof(char));
  sprintf (filename4, "%s_K_G%d.vcd", jobname, nclusters);
  out4 = fopen( filename4, "w" );

  for (i = 0; i < nrows; i++){
    difference = 0;
    difference1 = 0;
    for (j = 0; j < ncols; j++) {
      difference1 = cdata[clusterid[i]][j]-data[i][j];
      difference = difference + difference1*difference1;
    }
    vcdata[i][0] = sqrt(difference);
  }
  
  printf ("------- writing voxels-centroids distances to file:\t\t"
          " %s_K_G%d.vcd\n",jobname, nclusters);
  for (i = 0; i < nrows; i++)
    fprintf (out4, "%09d\t%7.3f\n", i, vcdata[i][0]);
  fclose(out4); out4=NULL;
  
  /*for (i = 0; i < nrows; i++){ 
    free(vcdata[i]);
    }
    free(vcdata);*/
  return;

  }
/* ========================================================================= */


void getvoxlclustersdist(int* count, float** cdata, 
			int* clusterid, float** data, char* jobname, 
			int nclusters, int nrows, int ncols, float **vcdata)
{
  int i, j, n, k;
  char* filename4;
  FILE *out4=NULL;
  float difference, difference1;

  /* n = 1 + strlen(jobname) + strlen("_K_G") + strlen(".ext");
  
  int dummy = nclusters;
  do n++; while (dummy/=10);
    
    
  filename4 = malloc(n*sizeof(char));
  sprintf (filename4, "%s_K_G%d.vcd", jobname, nclusters);
  out4 = fopen( filename4, "w" );
  */

  for (k = 0; k < nclusters; k++){
    for (i = 0; i < nrows; i++){
      difference = 0;
      difference1 = 0;
      for (j = 0; j < ncols; j++) {   /*CHECK THIS LOOP ; j is going through fitcoef*/
	difference1 = cdata[k][j]-data[i][j];
	difference = difference + difference1*difference1;
      }
      vcdata[i][k+1] = sqrt(difference);
    }
  }
  
  printf ("------- writing voxels-centroids distances to ...:\t\t"
          " %s_K_G%d.vcd\n",jobname, nclusters);
  /*  for (i = 0; i < nrows; i++)
    fprintf (out4, "%09d\t%7.3f\n", i, vcdata[i]);
    fclose(out4); out4=NULL;*/

  return;

}

/* DO I DARE */

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
   FILE *out1=NULL;
   FILE *out2=NULL;
   FILE *out3=NULL;

   int a;
   
   
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
   sprintf (filename, "%s_K_G%d.kgg", jobname, nclusters);
   out1 = fopen( filename, "w" );
   sprintf (filename2, "%s_K_G%d.dis", jobname, nclusters);
   out2 = fopen( filename2, "w" );
   sprintf (filename3, "%s_K_G%d.cen", jobname, nclusters);
   out3 = fopen( filename3, "w" );

   printf("======================== k-means clustering"
         " ========================\n");

   printf ("\n");
   printf ("----- doing %d passes... go stretch your legs...\n",npass);
   //npass = 3;
   /* ANDREJ: This function returns different answers each time it is 
      executed. Does the library provide for ways to initialize the
      random number generators used for the searching and initializations? */
   kcluster(   nclusters,
               nrows,ncols,data,
               weight,
               transpose,npass,
               method,dist,
               clusterid, &error, &ifound);
   printf ("Solution found %d times; ", ifound);
   printf ("within-cluster sum of distances is %f\n", error);
   printf ("------- writing Cluster assignments to file:\t\t"
          " %s_K_G%d.kgg\n",jobname, nclusters);
   for (i = 0; i < nrows; i++)
    fprintf (out1, "%09d\t %d\n", i, clusterid[i]);
   fclose(out1); out1=NULL;

   printf ("------- writing Distance between clusters to file:\t "
          "%s_K_G%d.dis \n", jobname, nclusters);
   fprintf (out2,"------- Distance between clusters:\n");
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
				      index[j], 'e', 'a', 0); 
	   fprintf(out2,"Distance between %d and %d: %7.3f\n", 
		   i, j, distance);
	 }
     }

   fclose(out2); out2=NULL;

   printf ("------- writing Cluster centroids to file:\t\t"
          "%s_K_G%d.cen\n",jobname, nclusters);
   fprintf (out3,"------- Cluster centroids:\n");
   getclustercentroids(nclusters, nrows, ncols, data, clusterid,
                      cdata, 0, 'a');
   fprintf(out3,"   coefficients:");
   for(i=0; i<ncols; i++) fprintf(out3,"\t%7d", i);
   fprintf(out3,"\n");
   for (i = 0; i < nclusters; i++){ 
      fprintf(out3,"Cluster %2d:", i);
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

      getvoxlclusterdist(count, cdata, clusterid, data, jobname, 
		      nclusters, nrows, ncols, vcdata);

      getvoxlclustersdist(count, cdata, clusterid, data, jobname, 
		      nclusters, nrows, ncols, vcdata);


      /*might want to make some calculations with vcdata*/

      /*lets calculate distance to centroid/sum(distance 2 all centroids)*/

      for (i = 0; i < nrows; i++)
	{
	  a = 0;
	  for (j = 0; j < nclusters; j++)
	    a += vcdata[i][j+1];
	  vcdata[i][nclusters+1] = 1000*vcdata[i][0]/a;
	}
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
/*!
   Put some help like for function thd_polyfit
*/
int thd_Acluster (  THD_3dim_dataset *in_set,
                  byte *mask, int nmask,
                  THD_3dim_dataset **clust_set,
                  THD_3dim_dataset **dist_set,
                  OPT_KMEANS oc )
{
  int ii, nl, nc, j;
   float **D=NULL, **distmatrix=NULL;  /* this double business is a waste of
                                           memory, at least for D..*/
   int ncol = -1;
   float *dvec=NULL;
   int* clusterid = NULL;
   short *sc = NULL;
   
   float** vcdata = NULL; 
   int nvc; /*this will be for number of columns in vcdata matrix*/
   


   ENTRY("thd_Acluster");
   
   if (!clust_set || *clust_set) {
      fprintf(stderr,
               "ERROR: output volume pointer pointers must point to NULL\n");
      RETURN(0);
   }
   if (!mask) nmask = DSET_NVOX(in_set);
   ncol = DSET_NVALS(in_set); 
   if (ncol < DSET_NUM_TIMES(in_set)) ncol = DSET_NUM_TIMES(in_set);
   
   if (oc.verb) {
      ININFO_message("Have %d/%d voxels to process "
                     "with %d dimensions per voxel.\n",
                     nmask, DSET_NVOX(in_set), ncol);
   }
   
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
         /* YOU SHOULD FREE distmatrix here ...*/
      } else {
         ERROR_message("Failed to create distmatrix");
         RETURN(0);
      }
   } else {
      ERROR_message("Bad option selection");
      RETURN(0);
   }
   
   /* create output datasets, if required*/
   *clust_set = EDIT_empty_copy(in_set) ;
   EDIT_dset_items(  *clust_set ,
                     ADN_nvals     , 1           ,
                     ADN_ntt       , 1          ,
                     ADN_datum_all , MRI_short      ,
                     ADN_brick_fac , NULL           ,
                     ADN_prefix    , "OML!"   ,
                     ADN_none ) ;
   /* MRI_float */
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
   
 

#   if 0

/* create another output datasets, voxel centroid distance*/
   *dist_set = EDIT_empty_copy(in_set) ;
   EDIT_dset_items(  *dist_set ,
                     ADN_nvals     , 1           ,
                     ADN_ntt       , 1          ,
                     ADN_datum_all , MRI_short      ,
                     ADN_brick_fac , NULL           ,
                     ADN_prefix    , "vcd"   ,
                     ADN_none ) ;
   /* MRI_float */
   if (oc.verb) {
      ININFO_message("loading results into %s\n",
                     DSET_PREFIX(*dist_set));
   }
   
   /* transfer data in vcdata to shorts array */
   sc = (short *)calloc(sizeof(short),DSET_NVOX(in_set));
      ii = 0;
   for (nl=0; nl<DSET_NVOX(in_set); ++nl) {
      if (!mask || mask[nl]) {
         sc[nl] = (short)vcdata[ii][0]+1;
         ++ii;
      }
   }

   for (ii=0;ii<(nmask);++ii) {
     free(vcdata[ii]);
   }

   free(vcdata); vcdata = NULL;
   EDIT_substitute_brick( *dist_set , 0 , MRI_short , sc ) ;
   sc = NULL; /* array now in brick */

#endif
  
   
   
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
        EDIT_substitute_brick( *dist_set , j , MRI_float , NULL ) ;

/* transfer data in vcdata to shorts array */
 ININFO_message("loading results into %s\n",
   DSET_PREFIX(*dist_set));
/* LOOP to pick vcdata[][fromclust] */
 /*sc = (short *)calloc(sizeof(short),DSET_NVOX(in_set));*/
     for (j = 0; j < nvc; j++) {
       ININFO_message("...%d,", j);
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

     }
     for (ii=0;ii<(nmask);++ii) {
       free(vcdata[ii]);
     }
     free(vcdata); vcdata = NULL;





   if (oc.verb) {
      ININFO_message("Freedom");
   }
   
   if (dvec) free(dvec); dvec=NULL;


   // To free D 
   for (ii=0;ii<nmask;++ii) {
    if (D[ii]) free(D[ii]);
   }
   free(D); D = NULL;
   
   RETURN(1);
}
