/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 2002, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This program implements the False Discovery Rate (FDR) algorithm for
  thresholding of voxelwise statistics.  

  File:    3dFDR.c
  Author:  B. Douglas Ward
  Date:    31 January 2002

*/
/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dFDR"                         /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_INITIAL "31 January 2002" /* date of initial program release */
#define PROGRAM_LATEST "31 January 2002"    /* date of last program revision */


/*---------------------------------------------------------------------------*/
/*
  Include header files and source code files.
*/

#include "mrilib.h"


/*---------------------------------------------------------------------------*/
/*
  Structure declarations 
*/

struct voxel;

typedef struct voxel
{
  int ixyz;                            /* voxel index */
  float pvalue;                        /* input p-value or output q-value */
  struct voxel * next_voxel;           /* pointer to next voxel in list */
} voxel;



/*-------------------------- global data ------------------------------------*/

#define  FDR_MAX_LL 10000        /* maximum number of linked lists of voxels */


static int    FDR_quiet      = 0;      /* flag for suppress screen output */
static int    FDR_list       = 0;      /* flag for list voxel q-values */
static float  FDR_mask_thr   = 1.0;    /* mask threshold */
static float  FDR_cn         = 1.0;    /* statistical constant c(N) */
static int    FDR_nxyz       = 0;      /* dataset dimensions in voxels */
static int    FDR_nthr       = 0;      /* number of voxels in mask */

static char * FDR_input_filename   = NULL;   /* input 3d functional dataset */
static char * FDR_input1D_filename = NULL;   /* input list of p-values */
static char * FDR_mask_filename    = NULL;   /* input 3d mask dataset */
static char * FDR_output_prefix    = NULL;   /* name for output 3d dataset */

static byte  * FDR_mask = NULL;            /* mask for voxels above thr. */
static float * FDR_input1D_data = NULL;    /* input array of p-values */
static voxel * FDR_head_voxel[FDR_MAX_LL]; /* linked lists of voxels */

static char * commandline = NULL;          /* command line for history notes */
static THD_3dim_dataset * FDR_dset = NULL; /* input dataset */  


/*---------------------------------------------------------------------------*/

/** macro to open a dataset and make it ready for processing **/

#define DOPEN(ds,name)                                                        \
do{ int pv ; (ds) = THD_open_dataset((name)) ;                                \
       if( !ISVALID_3DIM_DATASET((ds)) ){                                     \
          fprintf(stderr,"*** Can't open dataset: %s\n",(name)) ; exit(1) ; } \
       THD_load_datablock( (ds)->dblk ) ;                                     \
       pv = DSET_PRINCIPAL_VALUE((ds)) ;                                      \
       if( DSET_ARRAY((ds),pv) == NULL ){                                     \
          fprintf(stderr,"*** Can't access data: %s\n",(name)) ; exit(1); }   \
       if( DSET_BRICK_TYPE((ds),pv) == MRI_complex ){                         \
          fprintf(stderr,"*** Can't use complex data: %s\n",(name)) ; exit(1);\
       }     \
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


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void FDR_error (char * message)
{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/

/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
if((ptr)==NULL) \
( FDR_error ("Cannot allocate memory") )
     

/*---------------------------------------------------------------------------*/
/*
  Display help file.
*/

void FDR_Syntax(void)
{
printf(
"This program implements the False Discovery Rate (FDR) algorithm for       \n"
"thresholding of voxelwise statistics.                                      \n"
"                                                                           \n"
"Program input consists of a functional dataset containing one (or more)    \n"
"statistical sub-bricks.  Output consists of a bucket dataset with one      \n"
"sub-brick for each input sub-brick.  For non-statistical input sub-bricks, \n"
"the output is a copy of the input.  However, statistical input sub-bricks  \n"
"are replaced by their corresponding FDR values, as follows:                \n"
"                                                                           \n"
"For each voxel, the minimum value of q is determined such that             \n"
"                               E(FDR) <= q                                 \n"
"leads to rejection of the null hypothesis in that voxel. Only voxels inside\n"
"the user specified mask will be considered.  These q-values are then mapped\n"
"to z-scores for compatibility with the AFNI statistical threshold display: \n"
"                                                                           \n"
"               stat ==> p-value ==> FDR q-value ==> FDR z-score            \n"
"                                                                           \n"
);

printf(
"Usage:                                                                     \n"
"  3dFDR                                                                    \n"
"    -input fname       fname = filename of input 3d functional dataset     \n"
"      OR                                                                   \n"
"    -input1D dname     dname = .1D file containing column of p-values      \n"
"                                                                           \n"
"    -mask_file mname   Use mask values from file mname.                    \n"
"                       Note: If file mname contains more than 1 sub-brick, \n"
"                       the mask sub-brick must be specified!               \n"
"                       Default: No mask                                    \n"
"                                                                           \n"
"    -mask_thr m        Only voxels whose corresponding mask value is       \n"
"                       greater than or equal to m in absolute value will   \n"
"                       be considered.  Default: m=1                        \n"
"                                                                           \n"
"                       Constant c(N) depends on assumption about p-values: \n"
"    -cind              c(N) = 1   p-values are independent across N voxels \n"
"    -cdep              c(N) = sum(1/i), i=1,...,N   any joint distribution \n"
"                       Default:  c(N) = 1                                  \n"
"                                                                           \n"
"    -quiet             Flag to suppress screen output                      \n"
"                                                                           \n"
"    -list              Write sorted list of voxel q-values to screen       \n"
"                                                                           \n"
"    -prefix pname      Use 'pname' for the output dataset prefix name.     \n"
"      OR                                                                   \n"
"    -output pname                                                          \n"
"                                                                           \n"
"\n") ;

   printf("\n" MASTER_SHORTHELP_STRING ) ;

   exit(0) ;

}


/*---------------------------------------------------------------------------*/
/*
   Read the arguments, load the global variables

*/

void read_options ( int argc , char * argv[] )
{
  int nopt = 1 ;           /* count of input arguments */
  char message[80];        /* error message */


  
  /*----- main loop over input options -----*/
  while( nopt < argc )
    {

      /*-----   -input fname   -----*/
      if (strcmp(argv[nopt], "-input") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FDR_error ("need argument after -input ");
	  FDR_input_filename = (char *) malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (FDR_input_filename);
	  strcpy (FDR_input_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      
      
      /*-----   -input1D dname   -----*/
      if (strcmp(argv[nopt], "-input1D") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FDR_error ("need argument after -input1D ");
	  FDR_input1D_filename = (char *) malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (FDR_input1D_filename);
	  strcpy (FDR_input1D_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      
      
      /*-----   -mask_file mname   -----*/
      if (strcmp(argv[nopt], "-mask_file") == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FDR_error ("need argument after -mask_file ");
	  FDR_mask_filename = (char *) malloc (sizeof(char)*THD_MAX_NAME);
	  MTEST (FDR_mask_filename);
	  strcpy (FDR_mask_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*----- -mask_thr m -----*/
      if( strcmp(argv[nopt],"-mask_thr") == 0 ){
	 float fval;
         nopt++ ;
         if( nopt >= argc ){
            FDR_error (" need 1 argument after -mask_thr"); 
         }
	 sscanf (argv[nopt], "%f", &fval); 
	 if (fval < 0.0){
            FDR_error (" Require mask_thr >= 0.0 ");
         }
	 FDR_mask_thr = fval;
	 nopt++;  continue;
      }


      /*----- -cind -----*/
      if( strcmp(argv[nopt],"-cind") == 0 ){
         FDR_cn = 1.0;
         nopt++ ; continue ;
      }

      
      /*----- -cdep -----*/
      if( strcmp(argv[nopt],"-cdep") == 0 ){
         FDR_cn = -1.0;
         nopt++ ; continue ;
      }

      
      /*----- -quiet -----*/
      if( strcmp(argv[nopt],"-quiet") == 0 ){
         FDR_quiet = 1;
         nopt++ ; continue ;
      }

      
      /*----- -list -----*/
      if( strcmp(argv[nopt],"-list") == 0 ){
         FDR_list = 1;
         nopt++ ; continue ;
      }

      
      /*----- -prefix prefix -----*/
      if( strcmp(argv[nopt],"-prefix") == 0 ||
          strcmp(argv[nopt],"-output") == 0   ){
         nopt++ ;
         if( nopt >= argc ){
            FDR_error (" need argument after -prefix!");
         }
	 FDR_output_prefix = (char *) malloc (sizeof(char) * THD_MAX_PREFIX); 
         MCW_strncpy( FDR_output_prefix , argv[nopt++] , THD_MAX_PREFIX ) ;
         continue ;
      }


      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      FDR_error (message);
      

   }  /*----- end of loop over command line arguments -----*/


}


/*---------------------------------------------------------------------------*/
/*
  Read time series from specified file name.  This file name may have
  a column selector attached.
*/

float * read_time_series 
(
  char * ts_filename,          /* time series file name (plus column index) */
  int * ts_length              /* output value for time series length */
)

{
  char message[THD_MAX_NAME];    /* error message */
  char * cpt;                    /* pointer to column suffix */
  char filename[THD_MAX_NAME];   /* time series file name w/o column index */
  char subv[THD_MAX_NAME];       /* string containing column index */
  MRI_IMAGE * im, * flim;  /* pointers to image structures 
			      -- used to read 1D ASCII */
  float * far;             /* pointer to MRI_IMAGE floating point data */
  int nx;                  /* number of time points in time series */
  int ny;                  /* number of columns in time series file */
  int iy;                  /* time series file column index */
  int ipt;                 /* time point index */
  float * ts_data = NULL;  /* input time series data */


  /*----- First, check for empty filename -----*/
  if (ts_filename == NULL)
    FDR_error ("Missing input time series file name");


  /*----- Check file name for column index -----*/
  cpt = strstr (ts_filename, "[");
  if (cpt == NULL)
    {
      strcpy (filename, ts_filename);
      subv[0] = '\0';
    }
  else
    if (cpt == ts_filename)
      FDR_error ("Illegal time series filename on command line");
    else
      {
	int ii;
	ii = cpt - ts_filename;
	memcpy (filename, ts_filename, ii);
	filename[ii] = '\0';
	strcpy (subv, cpt);
      }

  
  /*----- Read the time series file -----*/
  flim = mri_read_1D(filename) ;
  if (flim == NULL)
    {
      sprintf (message,  "Unable to read time series file: %s",  filename);
      FDR_error (message);
    }

  far = MRI_FLOAT_PTR(flim);
  nx = flim->nx;
  ny = flim->ny; iy = 0 ;
  if( ny > 1 ){
    fprintf(stderr,"WARNING: time series %s has more than 1 column\n",filename);
  }
  

  /*----- Save the time series data -----*/
  *ts_length = nx;
  ts_data = (float *) malloc (sizeof(float) * nx);
  MTEST (ts_data);
  for (ipt = 0;  ipt < nx;  ipt++)
    ts_data[ipt] = far[ipt + iy*nx];   
  
  
  mri_free (flim);  flim = NULL;

  return (ts_data);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check whether one output file already exists.
*/

void check_one_output_file 
(
  THD_3dim_dataset * dset_time,     /* input 3d+time data set */
  char * filename                   /* name of output file */
)

{
  char message[THD_MAX_NAME];      /* error message */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int ierror;                         /* number of errors in editing data */

  
  /*----- make an empty copy of input dataset -----*/
  new_dset = EDIT_empty_copy( dset_time ) ;
  
  
  ierror = EDIT_dset_items( new_dset ,
			    ADN_prefix , filename ,
			    ADN_label1 , filename ,
			    ADN_self_name , filename ,
			    ADN_type , ISHEAD(dset_time) ? HEAD_FUNC_TYPE : 
                               			           GEN_FUNC_TYPE ,
			    ADN_none ) ;
  
  if( ierror > 0 )
    {
      sprintf (message,
	       "*** %d errors in attempting to create output dataset!\n", 
	       ierror);
      FDR_error (message);
    }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) )
    {
      sprintf (message,
	       "Output dataset file %s already exists "
	       " -- cannot continue! ",
	       new_dset->dblk->diskptr->header_name);
      FDR_error (message);
    }
  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the program: get all operator inputs; create mask
  for voxels above mask threshold.
*/

void * initialize_program (int argc, char * argv[])
{
  int iv;                  /* index number of sub-brick */
  void * vfim = NULL;      /* sub-brick data pointer */
  float * ffim = NULL;     /* sub-brick data in floating point format */
  int ixyz;                /* voxel index */
  int nx, ny, nz, nxyz;    /* numbers of voxels in input dataset */
  int mx, my, mz, mxyz;    /* numbers of voxels in mask dataset */
  int nthr;                /* number of voxels above mask threshold */
  char message[80];        /* error message */
  int ibin;                /* p-value bin index */


  /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/
  machdep() ; 
  { int new_argc ; char ** new_argv ;
  addto_args( argc , argv , &new_argc , &new_argv ) ;
  if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
  }
  

  /*----- Save command line for history notes -----*/
  commandline = tross_commandline( PROGRAM_NAME , argc,argv ) ;


  /*----- Does user request help menu? -----*/
  if( argc < 2 || strcmp(argv[1],"-help") == 0 ) FDR_Syntax() ;

  
  /*----- Add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv); 


  /*----- Read input options -----*/
  read_options( argc , argv ) ;


  /*----- Open the mask dataset -----*/
  if (FDR_mask_filename != NULL)
    {
      if (!FDR_quiet) 
	printf ("Reading mask dataset: %s \n", FDR_mask_filename);
      DOPEN (FDR_dset, FDR_mask_filename);

      if (FDR_dset == NULL)
	{
	  sprintf (message, "Cannot open mask dataset %s", FDR_mask_filename); 
	  FDR_error (message);
	}

      if (DSET_NVALS(FDR_dset) != 1)
	FDR_error ("Must specify single sub-brick for mask data");


      /*----- Get dimensions of mask dataset -----*/
      mx   = DSET_NX(FDR_dset);   
      my   = DSET_NY(FDR_dset);   
      mz   = DSET_NZ(FDR_dset);
      mxyz = mx*my*mz;


      /*----- Allocate memory for float data -----*/
      ffim = (float *) malloc (sizeof(float) * mxyz);   MTEST (ffim);


      /*----- Convert mask dataset sub-brick to floats (in ffim) -----*/
      iv = DSET_PRINCIPAL_VALUE (FDR_dset);
      SUB_POINTER (FDR_dset, iv, 0, vfim);
      EDIT_coerce_scale_type (mxyz, DSET_BRICK_FACTOR(FDR_dset,iv),
			      DSET_BRICK_TYPE(FDR_dset,iv), vfim,  /* input  */
			      MRI_float                   , ffim); /* output */
  
      
      /*----- Allocate memory for mask volume -----*/
      FDR_mask = (byte *) malloc (sizeof(byte) * mxyz);
      MTEST (FDR_mask);
      
      
      /*----- Create mask of voxels above mask threshold -----*/
      nthr = 0;
      for (ixyz = 0;  ixyz < mxyz;  ixyz++)
	if (fabs(ffim[ixyz]) >= FDR_mask_thr)  
	  { 
	    FDR_mask[ixyz] = 1;
	    nthr++;
	  }
	else
	  FDR_mask[ixyz] = 0;

      if (!FDR_quiet)  
	printf ("Number of voxels above mask threshold = %d \n", nthr);
      if (nthr < 1)  
	FDR_error ("No voxels above mask threshold.  Cannot continue.");


      /*----- Delete floating point sub-brick -----*/
      if (ffim != NULL) { free (ffim);   ffim = NULL; }

      /*----- Delete mask dataset -----*/
      THD_delete_3dim_dataset (FDR_dset, False);  FDR_dset = NULL ;

    }


  /*----- Get the input data -----*/

  if (FDR_input1D_filename != NULL)
    {
      /*----- Read the input .1D file -----*/
      if (!FDR_quiet)  printf ("Reading input data: %s \n", 
			       FDR_input1D_filename);
      FDR_input1D_data = read_time_series (FDR_input1D_filename, &nxyz);

      if (FDR_input1D_data == NULL)  
	{ 
	  sprintf (message,  "Unable to read input .1D data file: %s", 
		   FDR_input1D_filename);
	  FDR_error (message);
	}
      
      if (nxyz < 1)  
	{ 
	  sprintf (message,  "No p-values in input .1D data file: %s", 
		   FDR_input1D_filename);
	  FDR_error (message);
	}

      FDR_nxyz = nxyz;
      FDR_nthr = nxyz;
    }
  
  else
    {
      /*----- Open the input 3D dataset -----*/
      if (!FDR_quiet)  printf ("Reading input dataset: %s \n", 
			       FDR_input_filename);
      FDR_dset = THD_open_dataset(FDR_input_filename);
      
      if (FDR_dset == NULL)
	{
	  sprintf (message, "Cannot open input dataset %s", 
		   FDR_input_filename); 
	  FDR_error (message);
	}
      
      /*----- Get dimensions of input dataset -----*/
      nx   = DSET_NX(FDR_dset);   
      ny   = DSET_NY(FDR_dset);   
      nz   = DSET_NZ(FDR_dset);
      nxyz = nx*ny*nz;
      
      
      /*----- Check for compatible dimensions -----*/
      if (FDR_mask != NULL)
	{
	  if ((nx != mx) || (ny != my) || (nz != mz))
	    FDR_error ("Mask and input dataset have incompatible dimensions");
	  FDR_nxyz = nxyz;
	  FDR_nthr = nthr;
	}
      else
	{
	  FDR_nxyz = nxyz;
	  FDR_nthr = nxyz;
	}


      /*----- Check whether output dataset already exists -----*/
      check_one_output_file (FDR_dset, FDR_output_prefix);
    }


  /*----- Initialize constant c(N) -----*/
  if (FDR_cn < 0.0)
    {
      double cn;
      cn = 0.0;
      for (ixyz = 1;  ixyz <= FDR_nthr;  ixyz++)
	cn += 1.0 / ixyz;
      FDR_cn = cn;
      if (!FDR_quiet)
	printf ("c(N) = %f \n", FDR_cn);
    }
  
  /*----- Initialize voxel pointers -----*/
  for (ibin = 0;  ibin < FDR_MAX_LL;  ibin++)
    FDR_head_voxel[ibin] = NULL;


}


/*---------------------------------------------------------------------------*/
/*
  Create an empty voxel.
*/
  
voxel * create_voxel ()
{
  voxel * voxel_ptr = NULL;

  voxel_ptr = (voxel *) malloc (sizeof(voxel));
  MTEST (voxel_ptr);
  
  voxel_ptr->ixyz = 0;
  voxel_ptr->pvalue = 0.0;
  voxel_ptr->next_voxel = NULL;

  return (voxel_ptr);
  
}


/*---------------------------------------------------------------------------*/
/*
  Add a new voxel to the linked list of voxels.
*/

voxel * add_voxel (voxel * new_voxel, voxel * head_voxel)
{
  voxel * voxel_ptr = NULL;

  if ((head_voxel == NULL) || (new_voxel->pvalue >= head_voxel->pvalue))
    {
      new_voxel->next_voxel = head_voxel;
      head_voxel = new_voxel;
    }

  else
    {
      voxel_ptr = head_voxel;

      while ((voxel_ptr->next_voxel != NULL) && 
	     (new_voxel->pvalue < voxel_ptr->next_voxel->pvalue))
	voxel_ptr = voxel_ptr->next_voxel;
      
      new_voxel->next_voxel = voxel_ptr->next_voxel;
      voxel_ptr->next_voxel = new_voxel;
    }

  return (head_voxel);
}


/*---------------------------------------------------------------------------*/
/*
  Create and initialize a new voxel, and add to list of voxels.
*/

voxel * new_voxel (int ixyz, float pvalue, voxel * head_voxel)

{
  voxel * voxel_ptr = NULL;

  voxel_ptr = create_voxel ();

  voxel_ptr->ixyz      = ixyz;
  voxel_ptr->pvalue    = pvalue;

  head_voxel = add_voxel (voxel_ptr, head_voxel);

  return (head_voxel);
  
}


/*---------------------------------------------------------------------------*/
/*
  Deallocate memory for all voxel lists.
*/

void delete_all_voxels ()
{
  int ibin;
  voxel * voxel_ptr  = NULL;     /* pointer to current voxel */
  voxel * next_voxel = NULL;     /* pointer to next voxel */


  for (ibin = 0;  ibin < FDR_MAX_LL;  ibin++) 
    {
      voxel_ptr = FDR_head_voxel[ibin];
      while (voxel_ptr != NULL)
	{
	  next_voxel = voxel_ptr->next_voxel;
	  free (voxel_ptr);
	  voxel_ptr = next_voxel;
	}
      FDR_head_voxel[ibin] = NULL;
    }
  
}


/*---------------------------------------------------------------------------*/
/*
  Save voxel contents of all voxels into float array (sub-brick).
*/

void save_all_voxels (float * far)
{
  int ixyz, ibin;
  voxel * voxel_ptr  = NULL;     /* pointer to voxel */
  

  /*----- Initialize all voxels to zero -----*/
  for (ixyz = 0;  ixyz < FDR_nxyz;  ixyz++)
    far[ixyz] = 0.0;


  for (ibin = 0;  ibin < FDR_MAX_LL;  ibin++) 
    {
      voxel_ptr = FDR_head_voxel[ibin];
  
      while (voxel_ptr != NULL)
	{
	  far[voxel_ptr->ixyz] = voxel_ptr->pvalue;
	  voxel_ptr = voxel_ptr->next_voxel;
	}

    }

}


/*---------------------------------------------------------------------------*/
/*
  Calculate FDR z-scores for all voxels within one volume.
*/

void process_volume (float * ffim, int statcode, float * stataux)

{
  int ixyz;                      /* voxel index */
  int icount;                    /* count of sorted p-values */
  float fval;                    /* voxel input statistical value */
  float pval;                    /* voxel input stat. p-value */
  float qval;                    /* voxel FDR q-value */
  float zval;                    /* voxel FDR z-score */
  float qval_min;                /* smallest previous q-value */
  voxel * head_voxel = NULL;     /* linked list of voxels */
  voxel * voxel_ptr  = NULL;     /* pointer to current voxel */
  int ibin;                      /* p-value bin */
  int   * iarray = NULL;         /* output array of voxel indices */
  float * parray = NULL;         /* output array of voxel p-values */
  float * qarray = NULL;         /* output array of voxel FDR q-values */
  float * zarray = NULL;         /* output array of voxel FDR z-scores */

  
  /*----- Allocate memory for screen output arrays -----*/
  if (FDR_list)
    {
      iarray = (int   *) malloc (sizeof(int)   * FDR_nthr);   MTEST(iarray);
      parray = (float *) malloc (sizeof(float) * FDR_nthr);   MTEST(parray);
      qarray = (float *) malloc (sizeof(float) * FDR_nthr);   MTEST(qarray);
      zarray = (float *) malloc (sizeof(float) * FDR_nthr);   MTEST(zarray);
    }
  
  
  /*----- Loop over all voxels; sort p-values -----*/
  icount = FDR_nthr;
  for (ixyz = 0;  ixyz < FDR_nxyz;  ixyz++)
    {

      /*----- First, check if voxel is inside the mask -----*/
      if (FDR_mask != NULL)
	if (!FDR_mask[ixyz]) continue;


      /*----- Convert stats to p-values -----*/
      fval = fabs(ffim[ixyz]);
      if (statcode <= 0)
	pval = fval;
      else
	pval = THD_stat_to_pval (fval, statcode, stataux);

      if (pval >= 1.0)  
	{
	  /*----- Count but don't sort voxels with p-value = 1 -----*/
	  icount--;
	  if (FDR_list)
	    {
	      iarray[icount] = ixyz;
	      parray[icount] = 1.0;
	      qarray[icount] = 1.0;
	      zarray[icount] = 0.0;
	    }
	}
      else
	{ 
	  /*----- Place voxel in p-value bin -----*/
	  ibin = (int)  (pval * (FDR_MAX_LL));
	  if (ibin < 0)  ibin = 0;
	  if (ibin > FDR_MAX_LL-1)  ibin = FDR_MAX_LL-1;
	  head_voxel = new_voxel (ixyz, pval, FDR_head_voxel[ibin]);
	  FDR_head_voxel[ibin] = head_voxel;
	}
    }

  
  /*----- Calculate FDR q-values -----*/
  qval_min = 1.0;
  ibin = FDR_MAX_LL-1;
  while (ibin >= 0) 
    {
      voxel_ptr = FDR_head_voxel[ibin];
  
      while (voxel_ptr != NULL)
	{
          /*----- Convert sorted p-values to FDR q-values -----*/
	  pval = voxel_ptr->pvalue;
	  qval = FDR_cn * (pval*FDR_nthr) / icount;
	  if (qval > qval_min)
	    qval = qval_min;
	  else
	    qval_min = qval;

	  /*----- Convert FDR q-value to FDR z-score -----*/
	  if (qval < 1.0e-20)
	    zval = 10.0;
	  else
	    zval = normal_p2t(qval);

	  icount--;

	  /*----- Save calculated values -----*/
	  if (FDR_list)
	    {
	      iarray[icount] = voxel_ptr->ixyz;
	      parray[icount] = pval;
	      qarray[icount] = qval;
	      zarray[icount] = zval;
	    }

	  voxel_ptr->pvalue = zval;
	  voxel_ptr = voxel_ptr->next_voxel;
	}

      ibin--;
    }


  /*----- Write out the calculated values -----*/
  if (FDR_list)
    {
      printf ("%12s %12s %12s %12s \n", 
	      "Index", "p-value", "q-value", "z-score");
      for (icount = 0;  icount < FDR_nthr;  icount++)
	{
	  if (FDR_input1D_filename != NULL)
	    ixyz = iarray[icount] + 1;
	  else
	    ixyz = iarray[icount];
	  printf ("%12d %12.6f %12.6f %12.6f \n",  
		  ixyz, parray[icount], qarray[icount], zarray[icount]);
	}

      /*----- Deallocate memory for output arrays -----*/
      free (iarray);   free (parray);   free (qarray);   free (zarray);
    }


  /*----- Place FDR z-scores into float array -----*/
  save_all_voxels (ffim);


  /*----- Deallocate linked-list memory -----*/
  delete_all_voxels();

}


/*---------------------------------------------------------------------------*/
/*
  Perform all processing for this array of p-values.
*/

void process_1ddata ()

{
  float * ffim = NULL;     /* input list of p-values */

  
  /*----- Set pointer to input array of p-values -----*/
  ffim = FDR_input1D_data;


  /*----- Calculate FDR z-scores for all input p-values  -----*/
  process_volume (ffim, -1, NULL);
 

  /*----- Deallocate memory -----*/
  if (ffim != NULL) { free (ffim);   ffim = NULL; }

}


/*---------------------------------------------------------------------------*/
/*
  Convert one volume to another type, autoscaling:
     nxy   = # voxels
     itype = input datum type
     ivol  = pointer to input volume
     otype = output datum type
     ovol  = pointer to output volume (again, must be pre-malloc-ed)
  Return value is the scaling factor used (0.0 --> no scaling).
*/

float EDIT_coerce_autoscale_new( int nxyz ,
				 int itype,void *ivol , int otype,void *ovol )
{
  float fac=0.0 , top ;
  
  if( MRI_IS_INT_TYPE(otype) ){
    top = MCW_vol_amax( nxyz,1,1 , itype,ivol ) ;
    if (top == 0.0)  fac = 0.0;
    else  fac = MRI_TYPE_maxval[otype]/top;
  }
  
  EDIT_coerce_scale_type( nxyz , fac , itype,ivol , otype,ovol ) ;
  return ( fac );
}


/*---------------------------------------------------------------------------*/
/*
  Perform all processing for this sub-brick.
*/

void process_subbrick (THD_3dim_dataset * dset, int ibrick)

{
  const float EPSILON = 1.0e-10;
  float factor;            /* factor is new scale factor for this sub-brick */
  void * vfim = NULL;      /* sub-brick data pointer */
  float * ffim = NULL;     /* sub-brick data in floating point format */
  char brick_label[THD_MAX_NAME];       /* sub-brick label */


  if (!FDR_quiet)  printf ("Processing sub-brick #%d \n", ibrick);

  
  /*----- Allocate memory for float data -----*/
  ffim = (float *) malloc (sizeof(float) * FDR_nxyz);   MTEST (ffim);


  /*----- Convert sub-brick to float stats -----*/
  SUB_POINTER (dset, ibrick, 0, vfim);
  EDIT_coerce_scale_type (FDR_nxyz, DSET_BRICK_FACTOR(dset,ibrick),
			  DSET_BRICK_TYPE(dset,ibrick), vfim,   /* input  */
			  MRI_float                   , ffim);  /* output */


  /*----- Calculate FDR z-scores for all voxels within this volume -----*/
  process_volume (ffim, DSET_BRICK_STATCODE(dset,ibrick),
		        DSET_BRICK_STATAUX (dset,ibrick));


  /*----- Replace old sub-brick with new z-scores -----*/
  SUB_POINTER (dset, ibrick, 0, vfim);
  factor = EDIT_coerce_autoscale_new (FDR_nxyz, MRI_float, ffim,
				      DSET_BRICK_TYPE(dset,ibrick), vfim);  
  if (factor < EPSILON)  factor = 0.0;
  else factor = 1.0 / factor;
  

  /*----- edit the sub-brick -----*/
  strcpy (brick_label, "FDRz ");
  strcat (brick_label, DSET_BRICK_LABEL(dset, ibrick));
  EDIT_BRICK_LABEL (dset, ibrick, brick_label);
  EDIT_BRICK_FACTOR (dset, ibrick, factor);
  EDIT_BRICK_TO_FIZT (dset, ibrick);
 

  /*----- Deallocate memory -----*/
  if (ffim != NULL) { free (ffim);   ffim = NULL; }

}


/*---------------------------------------------------------------------------*/
/*
  Process the input dataset.
*/

THD_3dim_dataset * process_dataset ()

{
  THD_3dim_dataset * new_dset = NULL;     /* output bucket dataset */
  int ibrick, nbricks;                    /* sub-brick indices */
  int statcode;                           /* type of stat. sub-brick */


  /*----- Make full copy of input dataset -----*/
  new_dset = EDIT_full_copy(FDR_dset, FDR_output_prefix);


  /*----- Record history of dataset -----*/
  tross_Copy_History( FDR_dset , new_dset ) ;

  if( commandline != NULL )
    {
      tross_Append_History ( new_dset, commandline);
      free(commandline) ;
    }


  /*----- Deallocate memory for input dataset -----*/   
  THD_delete_3dim_dataset (FDR_dset , False );  FDR_dset = NULL ;


  /*----- Loop over sub-bricks in the dataset -----*/
  nbricks = DSET_NVALS(new_dset);
  for (ibrick = 0;  ibrick < nbricks;  ibrick++)
    {
      statcode = DSET_BRICK_STATCODE(new_dset, ibrick);
      if (FUNC_IS_STAT(statcode))
	{
	  /*----- Process the statistical sub-bricks -----*/
	  if (!FDR_quiet)  
	    printf ("ibrick = %3d   statcode = %5s \n", 
		    ibrick, FUNC_prefixstr[statcode]);
	  process_subbrick (new_dset, ibrick);
	}
    }


  return (new_dset);
}


/*---------------------------------------------------------------------------*/
/*
  Output the results as a bucket dataset.
*/

void output_results (THD_3dim_dataset * new_dset)
{
  int ierror;     /* flag for errors in editing dataset */


  /*----- Make sure that output is a bucket dataset -----*/
  ierror = EDIT_dset_items( new_dset ,
			    ADN_func_type , FUNC_BUCK_TYPE,
			    ADN_none ) ;
  if (ierror > 0)  
    FDR_error ("Errors in attempting to create output dataset.");


  /*----- Output the FDR dataset -----*/
  if( !FDR_quiet ) printf("Computing sub-brick statistics\n") ;
  THD_load_statistics( new_dset ) ;

  if( !FDR_quiet ) printf("Writing output to %s and %s\n",
     DSET_HEADNAME(new_dset) , DSET_BRIKNAME(new_dset) );
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  

  /*----- Deallocate memory for output dataset -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}


/*---------------------------------------------------------------------------*/

int main( int argc , char * argv[] )

{
  THD_3dim_dataset * new_dset = NULL;      /* output bucket dataset */
  

  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR); 
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");


  /*----- Initialize program:  get all operator inputs; 
    create mask for voxels above mask threshold -----*/
  initialize_program (argc, argv);


  if (FDR_input1D_filename != NULL)
    {
      /*----- Process list of p-values -----*/
      process_1ddata ();
    }
  else
    {
      /*----- Process 3d dataset -----*/
      new_dset = process_dataset ();

      /*----- Output the results as a bucket dataset -----*/
      output_results (new_dset);
    }
  
  exit(0) ;
}


/*---------------------------------------------------------------------------*/


