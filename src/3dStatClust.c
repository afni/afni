/*---------------------------------------------------------------------------*/
/*
  This program performs agglomerative hierarchical clustering of voxels 
  for user specified parameter sub-bricks.


  File:    3dStatClust.c
  Author:  B. Douglas Ward
  Date:    08 October 1999

  Mod:     Replaced C "pow" function, significantly improving execution speed.
  Date:    11 October 1999

  Mod:     Replaced dataset interface code with call to THD_open_dataset.
           Restructured code for initializing hierarchical clustering.
  Date:    19 October 1999


  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.

*/
/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dStatClust"                   /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_DATE "19 October 1999"           /* date of last program mod */

#define MAX_PARAMETERS 100

/*---------------------------------------------------------------------------*/
/*
  Include header files.
*/

#include "mrilib.h"
#include "matrix.h"


/*---------------------------------------------------------------------------*/

#ifndef myXtFree
#   define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

/*---------------------------------------------------------------------------*/

/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
if((ptr)==NULL) \
( printf ("Cannot allocate memory \n"),  exit(1) )
     

/*---------------------------------------------------------------------------*/

/** macro to open a dataset and make it ready for processing **/

#define DOPEN(ds,name)                                                        \
do{ int pv ; (ds) = THD_open_dataset((name)) ;                                \
       if( !ISVALID_3DIM_DATASET((ds)) ){                                     \
          fprintf(stderr,"*** Can't open dataset: %s\n",(name)) ; exit(1) ; } \
       THD_load_datablock( (ds)->dblk , NULL ) ;                              \
       pv = DSET_PRINCIPAL_VALUE((ds)) ;                                      \
       if( DSET_ARRAY((ds),pv) == NULL ){                                     \
          fprintf(stderr,"*** Can't access data: %s\n",(name)) ; exit(1); }   \
       if( DSET_BRICK_TYPE((ds),pv) == MRI_complex ){                         \
          fprintf(stderr,"*** Can't use complex data: %s\n",(name)) ; exit(1);        }     \
       break ; } while (0)


/*---------------------------------------------------------------------------*/

/** macro to return pointer to correct location in brick for current processing **/

#define SUB_POINTER(ds,vv,ind,ptr)                                            \
   do{ switch( DSET_BRICK_TYPE((ds),(vv)) ){                                  \
         default: fprintf(stderr,"\n*** Illegal datum! ***\n");exit(1);       \
            case MRI_short:{ short * fim = (short *) DSET_ARRAY((ds),(vv)) ;  \
                            (ptr) = (void *)( fim + (ind) ) ;                 \
            } break ;                                                         \
            case MRI_byte:{ byte * fim = (byte *) DSET_ARRAY((ds),(vv)) ;     \
                            (ptr) = (void *)( fim + (ind) ) ;                 \
            } break ;                                                         \
            case MRI_float:{ float * fim = (float *) DSET_ARRAY((ds),(vv)) ;  \
                             (ptr) = (void *)( fim + (ind) ) ;                \
            } break ; } break ; } while(0)


/*-------------------------- global data ------------------------------------*/

static int                      SC_nvox     = -1;   /* # voxels */
static int                      SC_verb     = 0;    /* verbose? */
static float                    SC_thr      = -1.0; /* threshold */
static int                      SC_nclust   = 10;   /* max. output clusters */
static int                      SC_statdist = 0;    /* dist. calc. method */
static int                      SC_dimension= 0;    /* number of parameters */

static char SC_thr_filename[THD_MAX_NAME]    = "";
static char SC_output_prefix[THD_MAX_PREFIX] = "SC" ;
static char SC_session[THD_MAX_NAME]         = "./"   ;

static int * SC_voxels = NULL;           /* indices for voxels above thr. */
static float ** SC_parameters = NULL;    /* parameters for voxels above thr. */

static char * commandline = NULL ;       /* command line for history notes */


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void SC_error (char * message)
{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/
/*
  Include source code files.
*/

#include "matrix.c"
#include "StatClust.c"


/*---------------------------------------------------------------------------*/
/*
  Display help file.
*/

void SC_Syntax(void)
{
   printf(
    "Perform agglomerative hierarchical clustering for user specified \n"
    "parameter sub-bricks, for all voxels whose threshold statistic   \n"
    "is above a user specified value.\n"
    "\nUsage: 3dStatClust options datasets \n"
    "where the options are:\n"
   ) ;

   printf(
    "-prefix pname    = Use 'pname' for the output dataset prefix name.\n"
    "  OR                 [default='SC']\n"
    "-output pname\n"
    "\n"
    "-session dir     = Use 'dir' for the output dataset session directory.\n"
    "                     [default='./'=current working directory]\n"
    "-verb            = Print out verbose output as the program proceeds.\n"
    "\n"
    "Options for calculating distance between parameter vectors: \n"
    "   -dist_euc        = Calculate Euclidean distance between parameters \n"
    "   -dist_ind        = Statistical distance for independent parameters \n"
    "   -dist_cor        = Statistical distance for correlated parameters \n"
    "The default option is:  Euclidean distance. \n"
    "\n"
    "-thresh t tname  = Use threshold statistic from file tname. \n"
    "                   Only voxels whose threshold statistic is greater \n"
    "                   than t in abolute value will be considered. \n"
    "                     [If file tname contains more than 1 sub-brick, \n"
    "                     the threshold stat. sub-brick must be specified!]\n"
    "-nclust n        = This specifies the maximum number of clusters for \n"
    "                   output (= number of sub-bricks in output dataset).\n"
    "\n"
    "Command line arguments after the above are taken as parameter datasets.\n"
    "\n"
   ) ;

   printf("\n" MASTER_SHORTHELP_STRING ) ;

   exit(0) ;

}


/*---------------------------------------------------------------------------*/
/*
   Read the arguments, load the global variables

*/

int SC_read_opts( int argc , char * argv[] )
{
   int nopt = 1 , ii ;
   char dname[THD_MAX_NAME] ;
   char subv[THD_MAX_NAME] ;
   char * cpt ;
   THD_3dim_dataset * dset ;
   int * svar ;
   char * str;
   int ok, ilen, nlen , max_nsub=0 ;

   char message[80];        /* error message */


   while( nopt < argc ){

      /**** -prefix prefix ****/

      if( strncmp(argv[nopt],"-prefix",6) == 0 ||
          strncmp(argv[nopt],"-output",6) == 0   ){
         nopt++ ;
         if( nopt >= argc ){
            SC_error (" need argument after -prefix!");
         }
         MCW_strncpy( SC_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }

      /**** -session directory ****/

      if( strncmp(argv[nopt],"-session",6) == 0 ){
         nopt++ ;
         if( nopt >= argc ){
            SC_error (" need argument after -session!"); 
         }
         MCW_strncpy( SC_session , argv[nopt++] , THD_MAX_NAME ) ;
         continue ;
      }

      /**** -verb ****/

      if( strncmp(argv[nopt],"-verb",5) == 0 ){
         SC_verb++ ;
         nopt++ ; continue ;
      }

      /**** -dist_euc ****/

      if( strncmp(argv[nopt],"-dist_euc",9) == 0 ){
         SC_statdist = 0 ;
         nopt++ ; continue ;
      }

      /**** -dist_ind ****/

      if( strncmp(argv[nopt],"-dist_ind",9) == 0 ){
         SC_statdist = 1 ;
         nopt++ ; continue ;
      }

      /**** -dist_cor ****/

      if( strncmp(argv[nopt],"-dist_cor",9) == 0 ){
         SC_statdist = 2 ;
         nopt++ ; continue ;
      }

      /**** -nclust n ****/

      if( strncmp(argv[nopt],"-nclust",7) == 0 ){
	 int ival;
         nopt++ ;
         if( nopt >= argc ){
            SC_error (" need argument after -nclust!");
         }
	 sscanf (argv[nopt], "%d", &ival); 
	 if ((ival < 1) || (ival > 255)){
            SC_error (" Require 1 <= nclust <= 255 ");
         }
	 SC_nclust = ival;
	 nopt++;
	 continue;
      }


      /**** -thresh thr fname ****/

      if( strncmp(argv[nopt],"-thresh",7) == 0 ){
	 float fval;
         nopt++ ;
         if( nopt+1 >= argc ){
            SC_error (" need 2 arguments after -thresh!"); 
         }
	 sscanf (argv[nopt], "%f", &fval); 
	 if (fval < 0.0){
            SC_error (" Require thr >= 0.0 ");
         }
	 SC_thr = fval;
	 nopt++;

	 strcpy (SC_thr_filename, argv[nopt]);
	 nopt++;
	 continue;
      }


      /*----- Invalid option -----*/
      if( argv[nopt][0] == '-' ){
	sprintf (message, " Unknown option: %s ", argv[nopt]);
	SC_error (message);
      }


      /*----- Remaining inputs should be parameter datasets -----*/
      break;


   }  /* end of loop over command line arguments */


   return (nopt) ;
}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the program: get all operator inputs; get indices
  for voxels above threshold; get parameter vectors for all voxels above
  threshold.
*/

THD_3dim_dataset * initialize_program ( int argc , char * argv[] )
{
  const int MIN_NVOX = 10;    /* minimum number of voxels above threshold */

  THD_3dim_dataset * thr_dset=NULL;     /* threshold dataset */
  THD_3dim_dataset * param_dset=NULL;   /* parameter dataset(s) */

  int nx, ny, nz;          /* dataset dimensions in voxels */
  int iv;                  /* index number of sub-brick */
  void * vfim = NULL;      /* sub-brick data pointer */
  float * ffim = NULL;     /* sub-brick data in floating point format */
  int ivox, nvox, icount;  /* voxel indices */
  int nopt;                /* points to current input option */
  int ibrick, nbricks;     /* sub-brick indices */
  char message[80];        /* error message */


  /*----- Save command line for history notes -----*/
  commandline = tross_commandline( PROGRAM_NAME , argc,argv ) ;


  /*----- Read input options -----*/
  if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) SC_Syntax() ;
  nopt = SC_read_opts( argc , argv ) ;


  /*----- Open the threshold dataset -----*/
  if (SC_verb)  printf ("Reading threshold dataset: %s \n", SC_thr_filename);
  DOPEN (thr_dset, SC_thr_filename);

  if (thr_dset == NULL)
    {
      sprintf (message, "Cannot open threshold dataset %s", SC_thr_filename); 
      SC_error (message);
    }

  if (DSET_NVALS(thr_dset) != 1)
    SC_error ("Must specify single sub-brick for threshold data");


  /*----- Save dimensions of threshold dataset for compatibility test -----*/
  nx = DSET_NX(thr_dset);   ny = DSET_NY(thr_dset);   nz = DSET_NZ(thr_dset);


  /*----- Allocate memory for float data -----*/
  nvox = DSET_NVOX (thr_dset);
  ffim = (float *) malloc (sizeof(float) * nvox);   MTEST (ffim);


  /*----- Convert threshold dataset sub-brick to floats (in ffim) -----*/
  iv = DSET_PRINCIPAL_VALUE (thr_dset);
  SUB_POINTER (thr_dset, iv, 0, vfim);
  EDIT_coerce_scale_type (nvox, DSET_BRICK_FACTOR(thr_dset,iv),
			  DSET_BRICK_TYPE(thr_dset,iv), vfim,     /* input  */
			  MRI_float                   , ffim);    /* output */
  
  /*----- Delete threshold dataset -----*/
  THD_delete_3dim_dataset (thr_dset, False);  thr_dset = NULL ;


  /*----- Count number of voxels above threshold -----*/
  SC_nvox = 0;
  for (ivox = 0;  ivox < nvox;  ivox++)
    if (fabs(ffim[ivox]) > SC_thr)  SC_nvox++;
  if (SC_verb)  printf ("Number of voxels above threshold = %d \n", SC_nvox);
  if (SC_nvox < MIN_NVOX)  
    {
      sprintf (message, "Only %d voxels above threshold.  Cannot continue.",
	       SC_nvox);
      SC_error (message);
    }



  /*----- Allocate memory for voxel index array -----*/
  SC_voxels = (int *) malloc (sizeof(int) * SC_nvox);
  MTEST (SC_voxels);


  /*----- Save indices of voxels above threshold -----*/
  icount = 0;
  for (ivox = 0;  ivox < nvox;  ivox++)
    if (fabs(ffim[ivox]) > SC_thr)
      {
	SC_voxels[icount] = ivox;
	icount++;
      }

  
  /*----- Allocate memory for parameter array -----*/
  SC_parameters = (float **) malloc (sizeof(float *) * MAX_PARAMETERS);
  MTEST (SC_parameters);


  /*----- Begin loop over parameter datasets -----*/
  SC_dimension = 0;
  while (nopt < argc)
    {
      /*----- Check if this is an input option -----*/
      if (argv[nopt][0] == '-')
	SC_error ("ALL input options must precede ALL parameter datasets");

      /*----- Open the parameter dataset -----*/
      if (SC_verb)  printf ("Reading parameter dataset: %s \n", argv[nopt]);
      DOPEN (param_dset, argv[nopt]);

      if (param_dset == NULL)
	{
	  sprintf (message, "Cannot open parameter dataset %s", argv[nopt]); 
	  SC_error (message);
	}

      /*----- Test for dataset compatibility -----*/
      if ((nx != DSET_NX(param_dset)) || (ny != DSET_NY(param_dset))
	  || (nz != DSET_NZ(param_dset)))
	{
	  sprintf (message, "Parameter dataset %s has incompatible dimensions",
		   argv[nopt]); 
	  SC_error (message);
	}
	
     
      /*----- Get number of parameters specified by this dataset -----*/
      nbricks = DSET_NVALS(param_dset);


      /*----- Loop over sub-bricks selected from parameter dataset -----*/
      for (ibrick = 0;  ibrick < nbricks;  ibrick++)
	{
	  if (SC_verb)  printf ("Reading parameter #%2d \n", SC_dimension+1);

	  SUB_POINTER (param_dset, ibrick, 0, vfim);
	  EDIT_coerce_scale_type (nvox, DSET_BRICK_FACTOR(param_dset,ibrick),
		     DSET_BRICK_TYPE(param_dset,ibrick), vfim,   /* input  */
		     MRI_float                         , ffim);  /* output */
  
	  /*----- Allocate memory for parameter data -----*/
	  SC_parameters[SC_dimension] 
	    = (float *) malloc (sizeof(float) * SC_nvox);
	  MTEST (SC_parameters[SC_dimension]);
	  
	  /*----- Save parameter data for all voxels above threshold -----*/
	  for (ivox = 0;  ivox < SC_nvox;  ivox++)
	    SC_parameters[SC_dimension][ivox] = ffim[SC_voxels[ivox]];
	  
	  /*----- Increment count of parameters -----*/
	  SC_dimension++;

	}

      /*----- Delete parameter dataset -----*/
      THD_delete_3dim_dataset (param_dset, False);  param_dset = NULL ;
     
      nopt++;
    }


  /*----- Delete floating point sub-brick -----*/
  if (ffim != NULL) { free (ffim);   ffim = NULL; }


  if (SC_verb)  printf ("Number of parameters = %d \n", SC_dimension);
  if (SC_dimension < 1)  SC_error ("No parameter data?");


}


/*---------------------------------------------------------------------------*/
/*
  Perform agglomerative hierarchical clustering.
*/

THD_3dim_dataset * form_clusters ()

{
  THD_3dim_dataset * new_dset = NULL;   /* hierarchical clustering */
  THD_3dim_dataset * thr_dset = NULL;   /* threshold dataset */
  int ivox, ixyz, nxyz;            /* voxel indices */
  int iclust;                      /* cluster index */
  int ip, jp;                      /* parameter indices */
  cluster * head_clust = NULL;     /* last cluster */
  float * parameters = NULL;       /* parameters after normalization */
  byte ** bar = NULL;              /* array of cluster sub-bricks */
  int nbricks;                     /* number of cluster sub-bricks */
  int ibrick;                      /* cluster sub-brick index */
  int ierror;                      /* number of errors in editing data */
  int ok;                          /* Boolean for successful matrix calc. */

  vector v, av;               /* intermediate vector results */
  matrix s;                   /* square root of covariance matrix */
  matrix sinv;                /* inverse of square root of covariance matrix */

  char message[80];           /* error message */


  /*----- Initialize vectors and matrices -----*/
  vector_initialize (&v);
  vector_initialize (&av);
  matrix_initialize (&s);
  matrix_initialize (&sinv);


  /*----- Calculate covariance matrix for input parameters -----*/
  if (SC_statdist)  calc_covariance (&s, &sinv);
  else
    {
      matrix_identity (SC_dimension, &s);
      matrix_identity (SC_dimension, &sinv);
    }
  

  /*----- Set number of sub-bricks -----*/
  if (SC_nvox < SC_nclust)
    nbricks = SC_nvox;
  else
    nbricks = SC_nclust;
  if (SC_verb) printf ("Output dataset will have %d sub-bricks\n\n", nbricks);


  /*----- Open threshold dataset -----*/
  thr_dset = THD_open_dataset (SC_thr_filename);
  nxyz = DSET_NVOX (thr_dset);


  /*-- Make an empty copy of threshold dataset, for eventual output --*/
  new_dset = EDIT_empty_copy (thr_dset);


  /*----- Record history of dataset -----*/
  tross_Copy_History (thr_dset, new_dset);
  if( commandline != NULL ) tross_Append_History( new_dset , commandline ) ;

  
  /*----- Delete threshold dataset -----*/
  THD_delete_3dim_dataset (thr_dset, False);  thr_dset = NULL ;


  /*----- Modify some structural properties.  Note that the nbricks
          just make empty sub-bricks, without any data attached. -----*/
  ierror = EDIT_dset_items (new_dset,
                            ADN_prefix,          SC_output_prefix,
			    ADN_directory_name,  SC_session,
			    ADN_type,            HEAD_FUNC_TYPE,
			    ADN_func_type,       FUNC_BUCK_TYPE,
                            ADN_ntt,             0,               /* no time */
			    ADN_nvals,           nbricks,
			    ADN_malloc_type,     DATABLOCK_MEM_MALLOC ,  
			    ADN_none ) ;
  
  if( ierror > 0 )
    {
      sprintf (message, 
	      " %d errors in attempting to create bucket dataset! ", 
	      ierror);
      SC_error (message);
    }
  
  if (THD_is_file(DSET_HEADNAME(new_dset))) 
    {
      sprintf (message,
	      " Output dataset file %s already exists--cannot continue! ",
	      DSET_HEADNAME(new_dset));
      SC_error (message);
    }


  /*----- Allocate memory -----*/
  bar  = (byte **) malloc (sizeof(byte *) * nbricks);
  MTEST (bar);
  

  /*----- Build lowest level of cluster hierarchy -----*/
  for (ivox = 0;  ivox < SC_nvox;  ivox++)
    {
      /*----- Allocate space for parameter vector -----*/
      parameters = (float *) malloc (sizeof(float) * SC_dimension);
      MTEST (parameters);

      /*----- Copy the parameter array -----*/
      for (ip = 0;  ip < SC_dimension;  ip++)
	parameters[ip] = SC_parameters[ip][ivox];
      
      /*----- If using stat. dist., transform the parameter vector -----*/
      if (SC_statdist)
	{
	  array_to_vector (SC_dimension, parameters, &v);
	  vector_multiply (sinv, v, &av);
	  vector_to_array (av, parameters);
	}

      /*----- Create new cluster containing single voxel -----*/
      ixyz = SC_voxels[ivox];
      head_clust = new_cluster (ixyz, parameters, head_clust);
    }


  /*----- Deallocate memory for parameter data -----*/
  free (SC_voxels);   SC_voxels = NULL;
  for (ip = 0;  ip < SC_dimension;  ip++)
    {
      free (SC_parameters[ip]);   SC_parameters[ip] = NULL;
    }
  free (SC_parameters);   SC_parameters = NULL;


  /*----- Agglomerate clusters, one-by-one -----*/
  for (iclust = SC_nvox;  iclust > 0;  iclust--)
    {
      if (SC_verb && (iclust % 100 == 0))
	printf ("# Clusters = %d \n", iclust);

      if (iclust <= nbricks)
	{
	  /*----- Sort clusters in order of size -----*/
	  head_clust = sort_clusters (head_clust);

	  /*----- Print cluster centroid parameters -----*/
	  if (SC_verb)
	    {
	      printf ("# Clusters = %d \n\n", iclust);
	      print_all_clusters (head_clust, s);
	    }
     
	  /*----- allocate memory for output sub-brick -----*/
	  ibrick = iclust-1;
	  bar[ibrick]  = (byte *) malloc (sizeof(byte) * nxyz);
	  MTEST (bar[ibrick]);

	  /*----- Save clusters into output sub-brick -----*/
	  for (ixyz = 0;  ixyz < nxyz;  ixyz++)	   
	    bar[ibrick][ixyz] = 0;
	  save_all_clusters (head_clust, bar[ibrick]); 

	  /*----- attach bar[ib] to be sub-brick #ibrick -----*/
	  EDIT_substitute_brick (new_dset, ibrick, MRI_byte, bar[ibrick]);

	}

      /*----- Agglomerate clusters -----*/
      if (iclust > 1)
	head_clust = agglomerate_clusters (head_clust);

    }


  /*----- Deallocate memory -----*/
  vector_destroy (&v);
  vector_destroy (&av);
  matrix_destroy (&s);
  matrix_destroy (&sinv);
  destroy_cluster (head_clust);


  /*----- Return hierarchical clustering -----*/
  return (new_dset);
  
}


/*---------------------------------------------------------------------------*/

int main( int argc , char * argv[] )

{
  THD_3dim_dataset * clust_dset = NULL;    /* hierarchical clusters data set */
  int ip;                                  /* parameter index */

  
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR); 
  printf ("Date:    %s \n", PROGRAM_DATE);
  printf ("\n");


  /*----- Initialize program:  get all operator inputs; get indices
    for voxels above threshold; get parameter vectors for all voxels 
    above threshold  -----*/
  initialize_program (argc, argv);


  /*----- Perform agglomerative hierarchical clustering -----*/
  clust_dset = form_clusters ();

  
  /*----- Output the hierarchical clustering dataset -----*/
  if( SC_verb ) printf("Computing sub-brick statistics\n") ;
  THD_load_statistics( clust_dset ) ;

  if( SC_verb ) printf("Writing output to %s and %s\n",
		       DSET_HEADNAME(clust_dset) , DSET_BRIKNAME(clust_dset) );
  THD_write_3dim_dataset( NULL,NULL , clust_dset , True ) ;
  

  /*----- Deallocate memory for cluster dataset -----*/   
  THD_delete_3dim_dataset( clust_dset , False ) ; clust_dset = NULL ;
  
  exit(0) ;
  

}







