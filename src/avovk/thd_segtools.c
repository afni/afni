#include "../mrilib.h"

#include <stdio.h>
#include <gsl/gsl_multifit.h>

#include "thd_segtools.h"

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

    // go by lines - signatures
    /* pre-allocate, I think this should be just fine, 
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



void example_kmeans( int nrows, int ncols, 
                     double** data, 
                     int nclusters, int npass, 
                     char dist, char* jobname,
                     int *clusterid)
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
   double error;
   double distance;
   int** index;
   int* count;
   double* weight = malloc(ncols*sizeof(double));
   double** cdata = malloc(nclusters*sizeof(double*));
   int** cmask = malloc(nclusters*sizeof(int*));
   int** mask = NULL;
   int n=0;
   char* filename;
   char* filename2;
   char* filename3;
   FILE *out1=NULL;
   FILE *out2=NULL;
   FILE *out3=NULL;
   
   for (i = 0; i < nclusters; i++)
   { cdata[i] = malloc(ncols*sizeof(double));
    cmask[i] = malloc(ncols*sizeof(int));
   }
   for (i = 0; i < ncols; i++) weight[i] = 1.0;

   mask = (int **)calloc(sizeof(int*), nrows);
   for (ii=0;ii<nrows;++ii) {
    mask[ii] = (int *)calloc(sizeof(int),ncols);
   }

   for (nl=0; nl<nrows; ++nl) {
    for (nc=0; nc<ncols; ++nc) {
      mask[nl][nc] = 1;
    }
   }



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
               mask,weight,
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
   distance =
    clusterdistance(nrows, ncols, data, mask, weight, count[0], count[1],
		    index[0], index[1], 'e', 'a', 0); 
   fprintf(out2,"Distance between 0 and 1: %7.3f\n", distance);
   distance =
    clusterdistance(nrows, ncols, data, mask, weight, count[0], count[2],
		    index[0], index[2], 'e', 'a', 0); 
   fprintf(out2,"Distance between 0 and 2: %7.3f\n", distance);
   distance =
    clusterdistance(nrows, ncols, data, mask, weight, count[1], count[2],
		    index[1], index[2], 'e', 'a', 0); 
   fprintf(out2,"Distance between 1 and 2: %7.3f\n", distance);
   fclose(out2); out2=NULL;

   printf ("------- writing Cluster centroids to file:\t\t"
          "%s_K_G%d.cen\n",jobname, nclusters);
   fprintf (out3,"------- Cluster centroids:\n");
   getclustercentroids(nclusters, nrows, ncols, data, mask, clusterid,
                      cdata, cmask, 0, 'a');
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
   for (i = 0; i < nclusters; i++) free(index[i]);
   free(index);
   free(count);

   for (i = 0; i < nclusters; i++){ 
      free(cdata[i]);
      free(cmask[i]);
   }
   for (ii=0;ii<nrows;++ii) {
      if (mask[ii]) free(mask[ii]);
   }

   
   free(cdata);
   free(cmask);
   free(weight);
   free(mask);
   return;
}

/**********************************************************************
   END: functions based on command.c code from The C clustering library.
**********************************************************************/
/*!
   Put some help like for function thd_polyfit
*/
int thd_kmeans (  THD_3dim_dataset *in_set,
                  byte *mask, int nmask,
                  THD_3dim_dataset **clust_set,
                  THD_3dim_dataset **dist_set,
                  OPT_KMEANS oc )
{
   int ii, nl, nc;
   double **D=NULL;  /* this double business is a waste of memory ..*/
   int ncol = -1;
   float *dvec=NULL;
   int* clusterid = NULL;
   short *sc = NULL;
   
   ENTRY("thd_kmeans");
   
   if (!clust_set || *clust_set) {
      fprintf(stderr,
               "ERROR: output volume pointer pointers must point to NULL\n");
      RETURN(0);
   }
   if (!mask) nmask = DSET_NVOX(in_set);
   ncol = DSET_NVALS(in_set); 
   if (ncol < DSET_NUM_TIMES(in_set)) ncol = DSET_NUM_TIMES(in_set);
   
   if (oc.verb) {
      ININFO_message("Have %d voxels to process with %d dimensions per voxel.\n",
                     nmask, ncol);
   }
   
   /* Create data matrix */
   D = (double **)calloc(sizeof(double*), nmask);
   for (ii=0;ii<(nmask);++ii) {
      if (!(D[ii] = (double *)calloc(sizeof(double), ncol))) {
         fprintf(stderr,"ERROR: Failed while allocating %dx%d double matrix\n", 
                        nmask, ncol);
         RETURN(0);
      }
   }

   dvec = (float * )malloc(sizeof(float)*ncol) ;  /* array to hold series */
   if (oc.verb) {
      ININFO_message("Filling D (mask=%p).\n", mask);
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

   /* now do the clustering 
      (ANDREJ: I do not know why the counting skipped 1st row and 1st col....) */
   if (oc.verb) {
      ININFO_message("Going to cluster: k=%d, r=%d\n"
                     "distmetric %c, jobname %s\n",
                     oc.k, oc.r, oc.distmetric, oc.jobname);
   }
   example_kmeans(   nmask, ncol, D, 
                     oc.k, oc.r, oc.distmetric, 
                     oc.jobname, clusterid);
   
   /* create output datasets, if required*/
   *clust_set = EDIT_empty_copy(in_set) ;
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
