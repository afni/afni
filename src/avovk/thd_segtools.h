#ifndef THD_SEGTOOLS_INCLUDED
#define THD_SEGTOOLS_INCLUDED

#define C(i) (gsl_vector_get(c,(i)))

THD_3dim_dataset *thd_polyfit(THD_3dim_dataset *in_set,
                              byte *mask, int polorder,
                              char *prefix, int verb);
   
/**********************************************************************
   BEGIN: functions based on command.c code from The C clustering library.
**********************************************************************/
void example_kmeans( int nrows, int ncols, 
                     double** data, 
                     int nclusters, int npass, 
                     char dist, char* jobname,
                     int *clusterid);
void clusterlib_display_version(void);
char* clusterlib_setjobname(const char* basename, int strip);
int clusterlib_readnumber(const char word[]);
char clusterlib_getmetric(int i);
/**********************************************************************
   END: functions based on command.c code from The C clustering library.
**********************************************************************/
                    
typedef struct {
   int k;
   int r;
   char *jobname;
   char distmetric;
   int verb;
} OPT_KMEANS;
int thd_kmeans (  THD_3dim_dataset *in_set,
                  byte *mask, int nmask,
                  THD_3dim_dataset **clust_set,
                  THD_3dim_dataset **dist_set,
                  OPT_KMEANS oc );

#endif
