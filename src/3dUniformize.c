/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  Program to correct for image intensity non-uniformity.

  File:    3dUniformize.c
  Author:  B. Douglas Ward
  Date:    28 January 2000

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dUniformize"                  /* name of this program */
#define PROGRAM_AUTHOR "B. D. Ward"                        /* program author */
#define PROGRAM_INITIAL "28 January 2000" /* date of initial program release */
#define PROGRAM_LATEST  "16 April 2003"   /* date of latest program revision */

/*---------------------------------------------------------------------------*/
/*
  Include header files.
*/

#include "mrilib.h"
#include "matrix.h"


/*---------------------------------------------------------------------------*/
/*
  Global variables, constants, and data structures.
*/

#define MAX_STRING_LENGTH 80

static THD_3dim_dataset * anat_dset = NULL;     /* input anatomical dataset  */
char * commandline = NULL ;                /* command line for history notes */

int input_datum = MRI_short ;              /* 16 Apr 2003 - RWCox */
int quiet       = 0 ;                      /* ditto */
 
typedef struct UN_options
{ 
  char * anat_filename;       /* file name for input anat dataset */
  char * prefix_filename;     /* prefix name for output dataset */
  Boolean quiet;              /* flag for suppress screen output */
  int lower_limit;    /* lower limit for voxel intensity */
  int rpts;           /* #voxels in sub-sampled image (for pdf) */
  int spts;           /* #voxels in subsub-sampled image (for field poly.) */
  int nbin;           /* #bins for pdf estimation */
  int npar;           /* #parameters for field polynomial */
} UN_options;


/*---------------------------------------------------------------------------*/
/*
  Include source code files.
*/

#include "matrix.c"
#include "estpdf3.c"


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void UN_error (char * message)
{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/

/** macro to test a malloc-ed pointer for validity **/

#define MTEST(ptr) \
if((ptr)==NULL) \
( UN_error ("Cannot allocate memory") )
     

/*---------------------------------------------------------------------------*/
/*
   Routine to display 3dUniformize help menu.
*/

void display_help_menu()
{
  printf 
    (
     "This program corrects for image intensity non-uniformity.\n\n"
     "Usage: \n"
     "3dUniformize  \n"
     "-anat filename    Filename of anat dataset to be corrected            \n"
     "                                                                      \n"
     "[-quiet]          Suppress output to screen                           \n"
     "                                                                      \n"
     "-prefix pname     Prefix name for file to contain corrected image     \n"
      );

  printf("\n" MASTER_SHORTHELP_STRING ) ;
  exit(0);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the input options.
*/
 
void initialize_options 
(
  UN_options * option_data    /* uniformization program options */
)
 
{

  /*----- initialize default values -----*/
  option_data->anat_filename = NULL;    /* file name for input anat dataset */
  option_data->prefix_filename = NULL;  /* prefix name for output dataset */
  option_data->quiet = FALSE;           /* flag for suppress screen output */
  option_data->lower_limit = 25;        /* voxel intensity lower limit */
  option_data->rpts = 200000;   /* #voxels in sub-sampled image (for pdf) */
  option_data->spts = 10000;    /* #voxels in subsub-sampled image 
				   (for field polynomial estimation) */
  option_data->nbin = 250;      /* #bins for pdf estimation */
  option_data->npar = 35;       /* #parameters for field polynomial */

}


/*---------------------------------------------------------------------------*/
/*
  Routine to get user specified input options.
*/

void get_options
(
  int argc,                        /* number of input arguments */
  char ** argv,                    /* array of input arguments */ 
  UN_options * option_data         /* uniformization program options */
)

{
  int nopt = 1;                     /* input option argument counter */
  int ival, index;                  /* integer input */
  float fval;                       /* float input */
  char message[MAX_STRING_LENGTH];  /* error message */


  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();  
   
  
  /*----- add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv); 


  /*----- main loop over input options -----*/
  while (nopt < argc )
    {

      /*-----   -anat filename   -----*/
      if (strncmp(argv[nopt], "-anat", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  UN_error ("need argument after -anat ");
	  option_data->anat_filename = 
	    malloc (sizeof(char) * MAX_STRING_LENGTH);
	  MTEST (option_data->anat_filename);
	  strcpy (option_data->anat_filename, argv[nopt]);

	  anat_dset = THD_open_dataset (option_data->anat_filename);
	  if (!ISVALID_3DIM_DATASET (anat_dset))
	    {
	      sprintf (message, "Can't open dataset: %s\n", 
		       option_data->anat_filename); 
	      UN_error (message); 
	    } 
	  THD_load_datablock (anat_dset->dblk); 
	  if (DSET_ARRAY(anat_dset,0) == NULL)
	    {
	      sprintf (message, "Can't access data: %s\n", 
		       option_data->anat_filename); 
	      UN_error (message); 
	    }

          /** RWCox [16 Apr 2003]
              If input is a byte dataset, make a short copy of it. **/

          if( DSET_BRICK_TYPE(anat_dset,0) == MRI_byte ){

            THD_3dim_dataset *qset ;
            register byte *bar ; register short *sar ;
            register int ii,nvox ;

            fprintf(stderr,"++ WARNING: converting input dataset from byte to short\n") ;
            qset = EDIT_empty_copy(anat_dset) ;
            nvox = DSET_NVOX(anat_dset) ;
            bar  = (byte *) DSET_ARRAY(anat_dset,0) ;
            sar  = (short *)malloc(sizeof(short)*nvox) ;
            for( ii=0 ; ii < nvox ; ii++ ) sar[ii] = (short) bar[ii] ;
            EDIT_substitute_brick( qset , 0 , MRI_short , sar ) ;
            DSET_delete(anat_dset) ; anat_dset = qset ; input_datum = MRI_byte ;

          } else if ( DSET_BRICK_TYPE(anat_dset,0) != MRI_short ){

            fprintf(stderr,"** ERROR: input dataset not short or byte type!\n") ;
            exit(1) ;

          }

	  nopt++;
	  continue;
	}
      

      /*-----   -quiet  -----*/
      if (strncmp(argv[nopt], "-quiet", 6) == 0)
	{
	  option_data->quiet = TRUE;
          quiet = 1 ;                    /* 16 Apr 2003 */
	  nopt++;
	  continue;
	}


      /*-----   -prefix prefixname   -----*/
      if (strncmp(argv[nopt], "-prefix", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  UN_error ("need argument after -prefix ");
	  option_data->prefix_filename = 
	    malloc (sizeof(char) * MAX_STRING_LENGTH);
	  MTEST (option_data->prefix_filename);
	  strcpy (option_data->prefix_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      UN_error (message);
      
    }

  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check whether one output file already exists.
*/

void check_one_output_file 
(
  char * filename                   /* name of output file */
)

{
  char message[MAX_STRING_LENGTH];    /* error message */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int ierror;                         /* number of errors in editing data */

  
  /*----- make an empty copy of input dataset -----*/
  new_dset = EDIT_empty_copy ( anat_dset );
  
  
  ierror = EDIT_dset_items( new_dset ,
			    ADN_prefix , filename ,
			    ADN_label1 , filename ,
			    ADN_self_name , filename ,
			    ADN_none ) ;
  
  if( ierror > 0 )
    {
      sprintf (message,
	       "*** %d errors in attempting to create output dataset!\n", 
	       ierror);
      UN_error (message);
    }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) )
    {
      sprintf (message,
	       "Output dataset file %s already exists"
	       "--cannot continue!\a\n",
	       new_dset->dblk->diskptr->header_name);
      UN_error (message);
    }
  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}


/*---------------------------------------------------------------------------*/

void verify_inputs 
(  
  UN_options * option_data         /* uniformization program options */
)

{
}


/*---------------------------------------------------------------------------*/
/*
  Program initialization.
*/

void initialize_program 
(
  int argc,                        /* number of input arguments */
  char ** argv,                    /* array of input arguments */ 
  UN_options ** option_data,       /* uniformization program options */
  short ** sfim                    /* output image volume */
)

{
  int nxyz;                        /* #voxels in input dataset */


  /*----- Save command line for history notes -----*/
  commandline = tross_commandline( PROGRAM_NAME , argc,argv ) ;


  /*----- Allocate memory for input options -----*/
  *option_data = (UN_options *) malloc (sizeof(UN_options));
  MTEST (*option_data);

  
  /*----- Initialize the input options -----*/
  initialize_options (*option_data); 


  /*----- Get operator inputs -----*/
  get_options (argc, argv, *option_data);


  /*----- Verify that inputs are acceptable -----*/
  verify_inputs (*option_data);


  /*----- Initialize random number generator -----*/
  rand_initialize (1234567);


  /*----- Allocate memory for output volume -----*/
  nxyz = DSET_NX(anat_dset) * DSET_NY(anat_dset) * DSET_NZ(anat_dset);
  *sfim = (short *) malloc (sizeof(short) * nxyz);
  MTEST (*sfim);
}


/*---------------------------------------------------------------------------*/
/*
  Write time series data to specified file.
*/

void ts_write (char * filename, int ts_length, float * data)
{
  int it;
  FILE * outfile = NULL;

  outfile = fopen (filename, "w");


  for (it = 0;  it < ts_length;  it++)
    {
      fprintf (outfile, "%f ", data[it]);
      fprintf (outfile, " \n");
    }

  fclose (outfile);
}


/*---------------------------------------------------------------------------*/
/*
  Resample the original image at randomly selected voxels (whose intensity
  value is greater than the specified lower limit, to exclude voxels outside
  the brain).  Take the logarithm of the intensity values for the selected 
  voxels.
*/

void resample 
(
  UN_options * option_data,
  int * ir,                         /* voxel indices for resampled image */
  float * vr                        /* resampled image data (logarithms) */
)

{
  short * anat_data = NULL;
  int nxyz;
  int rpts;
  int lower_limit;
  int it, k;


  /*----- Initialize local variables -----*/
  nxyz = DSET_NX(anat_dset) * DSET_NY(anat_dset) * DSET_NZ(anat_dset);
  anat_data = (short *) DSET_BRICK_ARRAY(anat_dset,0);
  lower_limit = option_data->lower_limit;
  rpts = option_data->rpts;

  it = 0;
  while (it < rpts)
    {
      k = rand_uniform (0, nxyz);
      if ( (k >= 0) && (k < nxyz) && (anat_data[k] > lower_limit) )
	{
	  ir[it] = k;
	  vr[it] = log (anat_data[k] + rand_uniform (0.0,1.0));
	  it++;
	}
    }

  return;
}


/*---------------------------------------------------------------------------*/
/*
  Create intensity map that will tend to concentrate values around the
  means of the gray and white matter distributions.
*/

void create_map (pdf vpdf, float * pars, float * vtou)

{
  int ibin;
  float v;

  for (ibin = 0;  ibin < vpdf.nbin;  ibin++)
    {
      v = PDF_ibin_to_xvalue (vpdf, ibin);
          
      if ((v > pars[4]-2.0*pars[5]) && (v < 0.5*(pars[4]+pars[7])))
	vtou[ibin] = pars[4];
      else if ((v > 0.5*(pars[4]+pars[7])) && (v < pars[7]+2.0*pars[8]))
	vtou[ibin] = pars[7];
      else
	vtou[ibin] = v;

    }
  
}


/*---------------------------------------------------------------------------*/
/*
  Use the intensity map to transform values of voxel intensities.
*/

void map_vtou (pdf vpdf, int rpts, float * vr, float * vtou, float * ur)

{
  int i, ibin;
  float v;


  for (i = 0;  i < rpts;  i++)
    {
      v = vr[i];
      ibin = PDF_xvalue_to_ibin (vpdf, v);
      
      if ((ibin >= 0) && (ibin < vpdf.nbin))
	ur[i] = vtou[ibin];
      else
	ur[i] = v;
    }

}


/*---------------------------------------------------------------------------*/

void subtract (int rpts, float * a, float * b, float * c)

{
  int i;


  for (i = 0;  i < rpts;  i++)
    {
      c[i] = a[i] - b[i];
    }

}


/*---------------------------------------------------------------------------*/
/*
  Create one row of the X matrix.
*/

void create_row (int ixyz, int nx, int ny, int nz, float * xrow)

{
  int ix, jy, kz;
  float x, y, z, x2, y2, z2, x3, y3, z3, x4, y4, z4;


  IJK_TO_THREE (ixyz, ix, jy, kz, nx, nx*ny); 


  x = (float) ix / (float) nx - 0.5;
  y = (float) jy / (float) ny - 0.5;
  z = (float) kz / (float) nz - 0.5;

  x2 = x*x;   x3 = x*x2;   x4 = x2*x2;
  y2 = y*y;   y3 = y*y2;   y4 = y2*y2;
  z2 = z*z;   z3 = z*z2;   z4 = z2*z2;


  xrow[0] = 1.0;
  xrow[1] = x;
  xrow[2] = y;
  xrow[3] = z;
  xrow[4] = x*y;
  xrow[5] = x*z;
  xrow[6] = y*z;
  xrow[7] = x2;
  xrow[8] = y2;
  xrow[9] = z2;
  xrow[10] = x*y*z;
  xrow[11] = x2*y;
  xrow[12] = x2*z;
  xrow[13] = y2*x;
  xrow[14] = y2*z;
  xrow[15] = z2*x;
  xrow[16] = z2*y;
  xrow[17] = x3;
  xrow[18] = y3;
  xrow[19] = z3;
  xrow[20] = x2*y*z;
  xrow[21] = x*y2*z;
  xrow[22] = x*y*z2;
  xrow[23] = x2*y2;
  xrow[24] = x2*z2;
  xrow[25] = y2*z2;
  xrow[26] = x3*y;
  xrow[27] = x3*z;
  xrow[28] = x*y3;
  xrow[29] = y3*z;
  xrow[30] = x*z3;
  xrow[31] = y*z3;
  xrow[32] = x4;
  xrow[33] = y4;
  xrow[34] = z4;


  return;
}


/*---------------------------------------------------------------------------*/
/*
  Approximate the distortion field with a polynomial function in 3 dimensions.
*/

void poly_field (int nx, int ny, int nz, int rpts, int * ir, float * fr, 
		 int spts, int npar, float * fpar)

{
  int p;                   /* number of parameters in the full model */ 
  int i, j, k;
  matrix x;                      /* independent variable matrix */
  matrix xtxinv;                 /* matrix:  1/(X'X)       */
  matrix xtxinvxt;               /* matrix:  (1/(X'X))X'   */
  vector y;
  vector coef;
  float * xrow = NULL;
  int ip;
  int iter;
  float f;


  p = npar;


  /*----- Initialize matrices and vectors -----*/
  matrix_initialize (&x);
  matrix_initialize (&xtxinv);
  matrix_initialize (&xtxinvxt);
  vector_initialize (&y);
  vector_initialize (&coef);


  /*----- Allocate memory -----*/
  matrix_create (spts, p, &x);
  vector_create (spts, &y);
  xrow = (float *) malloc (sizeof(float) * p); 
 

  /*----- Set up the X matrix and Y vector -----*/
  for (i = 0;  i < spts;  i++)
    {
      k = rand_uniform (0, rpts);
      create_row (ir[k], nx, ny, nz, xrow);

      for (j = 0;  j < p;  j++)
	x.elts[i][j] = xrow[j];
      y.elts[i] = fr[k];
    }


  /*  
      matrix_sprint ("X matrix = ", x);
      vector_sprint ("Y vector = ", y);
  */


  {
    /*----- calculate various matrices which will be needed later -----*/
    matrix xt, xtx;            /* temporary matrix calculation results */
    int ok;                    /* flag for successful matrix inversion */
    
    
    /*----- initialize matrices -----*/
    matrix_initialize (&xt);
    matrix_initialize (&xtx);
    
	
    matrix_transpose (x, &xt);
    matrix_multiply (xt, x, &xtx);
    ok = matrix_inverse (xtx, &xtxinv);
    
    if (ok)
      matrix_multiply (xtxinv, xt, &xtxinvxt);
    else
      {
	matrix_sprint ("X matrix = ", x);
	matrix_sprint ("X'X matrix = ", xtx);
	UN_error ("Improper X matrix  (cannot invert X'X) ");
      }
    
    /*----- dispose of matrices -----*/
    matrix_destroy (&xtx);
    matrix_destroy (&xt);
    
  }


  /*
    matrix_sprint ("1/(X'X)     = ", xtxinv);
    matrix_sprint ("(1/(X'X))X' = ", xtxinvxt);
    vector_sprint ("Y data  = ", y);
  */
  
  vector_multiply (xtxinvxt, y, &coef);
  /*
    vector_sprint ("Coef    = ", coef);
  */
  

  for (ip = 0;  ip < p;  ip++)
    {
     fpar[ip] = coef.elts[ip];
    }
  

  /*----- Dispose of matrices and vectors -----*/
  matrix_destroy (&x);
  matrix_destroy (&xtxinv);
  matrix_destroy (&xtxinvxt);
  vector_destroy (&y);
  vector_destroy (&coef);

  
}


/*---------------------------------------------------------------------------*/
/*
  Use the 3-dimensional polynomial function to estimate the distortion field
  at each point.
*/

float warp_image (int npar, float * fpar, int nx, int ny, int nz,
		  int rpts, int * ir, float * fs)
{
  int i, j;
  float x;
  float * xrow;
  float max_warp;


  xrow = (float *) malloc (sizeof(float) * npar); 


  max_warp = 0.0;

  for (i = 0;  i < rpts;  i++)
    {
      create_row (ir[i], nx, ny, nz, xrow);

      fs[i] = 0.0;
            
      for (j = 1;  j < npar;  j++)
	fs[i] += fpar[j] * xrow[j];

      if (fabs(fs[i]) > max_warp)
	max_warp = fabs(fs[i]);
    }


  free (xrow);   xrow = NULL;


  return (max_warp);
}


/*---------------------------------------------------------------------------*/
/*
  Find polynomial approximation to the distortion field.
*/

void estimate_field (UN_options * option_data, 
		     int * ir, float * vr, float * fpar)
{
  float * ur = NULL, * us = NULL, * fr = NULL, * fs = NULL, * wr = NULL;
  float * vtou = NULL;
  float * gpar;
  int iter = 0;
  int ip;
  int it;
  int nx, ny, nz, nxy, nxyz;
  int rpts, spts, nbin, npar;
  float parameters [DIMENSION];    /* parameters for PDF estimation */
  Boolean ok = TRUE;               /* flag for successful PDF estimation */
  char filename[MAX_STRING_LENGTH];


  /*----- Initialize local variables -----*/
  nx = DSET_NX(anat_dset);  ny = DSET_NY(anat_dset);  nz = DSET_NZ(anat_dset);
  nxy = nx*ny;   nxyz = nxy*nz;
  rpts = option_data->rpts;
  spts = option_data->spts;
  nbin = option_data->nbin;
  npar = option_data->npar;
  


  /*----- Allocate memory -----*/
  ur   = (float *) malloc (sizeof(float) * rpts);   MTEST (ur);
  us   = (float *) malloc (sizeof(float) * rpts);   MTEST (us);
  fr   = (float *) malloc (sizeof(float) * rpts);   MTEST (fr);
  fs   = (float *) malloc (sizeof(float) * rpts);   MTEST (fs);
  wr   = (float *) malloc (sizeof(float) * rpts);   MTEST (wr);
  gpar = (float *) malloc (sizeof(float) * npar);   MTEST (gpar);
  vtou = (float *) malloc (sizeof(float) * nbin);   MTEST (vtou);


  /*----- Initialize polynomial coefficients -----*/
  for (ip = 0;  ip < npar;  ip++)
    {
      fpar[ip] = 0.0;
      gpar[ip] = 0.0;
    }


  /*----- Estimate pdf for resampled data -----*/
  PDF_initialize (&p);
  PDF_float_to_pdf (rpts, vr, nbin, &p);
  sprintf (filename, "p%d.1D", iter);
  PDF_write_file (filename, p);


  /*----- Estimate gross field distortion -----*/
  poly_field (nx, ny, nz, rpts, ir, vr, spts, npar, fpar);
  warp_image (npar, fpar, nx, ny, nz, rpts, ir, fs);
  subtract (rpts, vr, fs, ur);
 
  
  for (ip = 0;  ip < rpts;  ip++)
    vr[ip] = ur[ip];


  /*----- Iterate over field distortion for concentrating the PDF -----*/
  for (iter = 1;  iter <= 5;  iter++)
    {
      /*----- Estimate pdf for perturbed image ur -----*/
      estpdf_float (rpts, ur, nbin, parameters);
      PDF_sprint ("p", p);
      sprintf (filename, "p%d.1D", iter);
      PDF_write_file (filename, p);

      /*----- Sharpen the pdf and produce modified image wr -----*/
      create_map (p, parameters, vtou);
      sprintf (filename, "vtou%d.1D", iter);
      ts_write (filename, p.nbin, vtou);
      map_vtou (p, rpts, ur, vtou, wr);

      /*----- Estimate smooth distortion field fs -----*/
      subtract (rpts, vr, wr, fr);
      poly_field (nx, ny, nz, rpts, ir, fr, spts, npar, gpar);
      warp_image (npar, gpar, nx, ny, nz, rpts, ir, fs);

      /*----- Create perturbed image ur -----*/
      subtract (rpts, vr, fs, ur);
    }
  

  /*----- Accumulate distortion field polynomial coefficients -----*/
  for (ip = 0;  ip < npar;  ip++)
    fpar[ip] += gpar[ip];


  /*----- Deallocate memory -----*/
  free (ur);     ur = NULL;  
  free (us);     us = NULL;
  free (fr);     fr = NULL;
  free (fs);     fs = NULL;
  free (wr);     wr = NULL;
  free (gpar);   gpar = NULL;
  free (vtou);   vtou = NULL;


  return;
}


/*---------------------------------------------------------------------------*/
/*
  Remove the nonuniformity field.
*/

void remove_field (UN_options * option_data, float * fpar, short * sfim)
{
  short * anat_data = NULL;
  int rpts;
  int npar;
  int lower_limit;
  int nx, ny, nz, nxyz;
  int ixyz, jpar;
  float x;
  float * xrow;
  float f;


  /*----- Initialize local variables -----*/
  nx = DSET_NX(anat_dset);  ny = DSET_NY(anat_dset);  nz = DSET_NZ(anat_dset);
  nxyz = nx*ny*nz;
  anat_data = (short *) DSET_BRICK_ARRAY(anat_dset,0);
  rpts = option_data->rpts;
  npar = option_data->npar;
  lower_limit = option_data->lower_limit;

  xrow = (float *) malloc (sizeof(float) * npar); 


  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      if (anat_data[ixyz] > lower_limit) 
	{
	  create_row (ixyz, nx, ny, nz, xrow);
	  
	  f = 0.0;
	  for (jpar = 1;  jpar < npar;  jpar++)
	    f += fpar[jpar] * xrow[jpar];
	  
	  sfim[ixyz] = exp( log(anat_data[ixyz]) - f);
	}
      else
	sfim[ixyz] = anat_data[ixyz];
    }

  
  return;
}


/*---------------------------------------------------------------------------*/
/*
  Correct for image intensity nonuniformity.
*/

void uniformize (UN_options * option_data, short * sfim)

{
  int * ir = NULL;
  float * vr = NULL;
  float * fpar = NULL;
  int rpts, npar;


  /*----- Initialize local variables -----*/
  rpts = option_data->rpts;
  npar = option_data->npar;


  /*----- Allocate memory -----*/
  ir = (int *) malloc (sizeof(int) * rpts);         MTEST(ir);
  vr = (float *) malloc (sizeof(float) * rpts);     MTEST(vr);
  fpar = (float *) malloc (sizeof(float) * npar);   MTEST(fpar);


  /*----- Resample the data -----*/
  resample (option_data, ir, vr);


  /*----- Estimate the nonuniformity field -----*/
  estimate_field (option_data, ir, vr, fpar);


  /*----- Remove the nonuniformity field -----*/
  remove_field (option_data, fpar, sfim);

 
  /*----- Deallocate memory -----*/
  free (ir);     ir = NULL;
  free (vr);     vr = NULL;
  free (fpar);   fpar = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to write one AFNI dataset.
  
  
*/

void write_afni_data 
(
  UN_options * option_data,
  short * sfim
)

{
  int nxyz;                           /* number of voxels */
  int ii;                             /* voxel index */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int ierror;                         /* number of errors in editing data */
  int ibuf[32];                       /* integer buffer */
  float fbuf[MAX_STAT_AUX];           /* float buffer */
  float fimfac;                       /* scale factor for short data */
  int output_datum;                   /* data type for output data */
  char * filename;                    /* prefix filename for output */
  byte *bfim = NULL ;                 /* 16 Apr 2003 */


  /*----- initialize local variables -----*/
  filename = option_data->prefix_filename;
  nxyz = DSET_NX(anat_dset) * DSET_NY(anat_dset) * DSET_NZ(anat_dset);

  
  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy( anat_dset ) ;


  /*----- Record history of dataset -----*/
  tross_Copy_History( anat_dset , new_dset ) ;
  if( commandline != NULL )
     tross_Append_History( new_dset , commandline ) ;

  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset (anat_dset, False);   anat_dset = NULL ;

  /*-- 16 Apr 2003 - RWCox:
       see if we can convert output back to bytes, if input was bytes --*/

  output_datum = MRI_short ;             /* default, in sfim */

  if( input_datum == MRI_byte ){         /* if input was byte */
    short stop = sfim[0] ;
    for( ii=1 ; ii < nxyz ; ii++ )
      if( sfim[ii] > stop ) stop = sfim[ii] ;
    output_datum = MRI_byte ;
    bfim = malloc(sizeof(byte)*nxyz) ;
    if( stop <= 255 ){                   /* output fits into byte range */
      for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (byte) sfim[ii] ;
    } else {                             /* must scale output down */
      float sfac = 255.9 / stop ;
      fprintf(stderr,"++ WARNING: scaling by %g back down to byte data\n",sfac);
      for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (byte)(sfim[ii]*sfac) ;
    }
    free(sfim) ;
  }
 
  /*-- we now return control to your regular programming --*/ 
  ibuf[0] = output_datum ;
  
  ierror = EDIT_dset_items( new_dset ,
			    ADN_prefix , filename ,
			    ADN_label1 , filename ,
			    ADN_self_name , filename ,
			    ADN_datum_array , ibuf ,
			    ADN_malloc_type, DATABLOCK_MEM_MALLOC ,  
			    ADN_none ) ;

     
  if( ierror > 0 ){
    fprintf(stderr,
	    "*** %d errors in attempting to create output dataset!\n", 
	    ierror ) ;
    exit(1) ;
  }


  if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
    fprintf(stderr,
	    "*** Output dataset file %s already exists--cannot continue!\a\n",
	    new_dset->dblk->diskptr->header_name ) ;
    exit(1) ;
  }
  
  
  /*----- attach bricks to new data set -----*/

  if( output_datum == MRI_short )
    mri_fix_data_pointer (sfim, DSET_BRICK(new_dset,0)); 
  else if( output_datum == MRI_byte )
    mri_fix_data_pointer (bfim, DSET_BRICK(new_dset,0));    /* 16 Apr 2003 */

  fimfac = 1.0;

  /*----- write afni data set -----*/
  if (!quiet)
    {
      printf ("\nWriting anatomical dataset: ");
      printf("%s\n", new_dset->dblk->diskptr->header_name) ;
      printf("data type = %s\n",MRI_TYPE_name[output_datum]) ;
    }


  for( ii=0 ; ii < MAX_STAT_AUX ; ii++ ) fbuf[ii] = 0.0 ;
  (void) EDIT_dset_items( new_dset , ADN_stat_aux , fbuf , ADN_none ) ;
  fbuf[0] = (output_datum == MRI_short && fimfac != 1.0 ) ? fimfac : 0.0 ;
  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;
  THD_load_statistics( new_dset ) ;
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;

  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}


/*---------------------------------------------------------------------------*/
/*
  This is the main routine for program 3dUniformize.
*/

int main
(
  int argc,                /* number of input arguments */
  char ** argv             /* array of input arguments */ 
)

{
  UN_options * option_data = NULL;     /* uniformization program options */
  short * sfim = NULL;                 /* output uniformized image */


  /*----- Identify software -----*/
  if( !quiet ){
    printf ("\n\n");
    printf ("Program: %s \n", PROGRAM_NAME);
    printf ("Author:  %s \n", PROGRAM_AUTHOR);
    printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
    printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
    printf ("\n");
  }

  
  /*----- Program initialization -----*/
  initialize_program (argc, argv, &option_data, &sfim);


  /*----- Perform uniformization -----*/
  uniformize (option_data, sfim);


  /*----- Write out the results -----*/
  write_afni_data (option_data, sfim);
  

  exit(0);

}

/*---------------------------------------------------------------------------*/






