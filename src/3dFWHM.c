 /*
  This program estimates the Filter Width Half Maximum (FWHM) of the Gaussian
  filter required to produce clustering in noise data equivalent to that 
  of the input AFNI 3d dataset.
  
  File:    3dFWHM.c
  Author:  B. D. Ward
  Date:    20 February 1997
*/



/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This software is Copyright 1997 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application. The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
*/


/*---------------------------------------------------------------------------*/


#define PROGRAM_NAME "3dFWHM"                        /* name of this program */
#define LAST_MOD_DATE "20 February 1997"         /* date of last program mod */

#define MAX_NAME_LENGTH 80            /* max. strength length for file names */


#include "mrilib.h"


/*---------------------------------------------------------------------------*/

/** macro to open a dataset and make it ready for processing **/

#define DOPEN(ds,name)                                                               \
   do{ int pv ; (ds) = THD_open_one_dataset((name)) ;                                \
       if( !ISVALID_3DIM_DATASET((ds)) ){                                            \
          fprintf(stderr,"*** Can't open dataset: %s\n",(name)) ; exit(1) ; }        \
       if( (ds)->daxes->nxx!=nx || (ds)->daxes->nyy!=ny || (ds)->daxes->nzz!=nz ){   \
          fprintf(stderr,"*** Axes mismatch: %s\n",(name)) ; exit(1) ; }             \
       if( DSET_NUM_TIMES((ds)) > 1 ){                                               \
         fprintf(stderr,"*** Can't use time-dependent data: %s\n",(name));exit(1); } \
       THD_load_datablock( (ds)->dblk , NULL ) ;                                     \
       pv = DSET_PRINCIPAL_VALUE((ds)) ;                                             \
       if( DSET_ARRAY((ds),pv) == NULL ){                                            \
          fprintf(stderr,"*** Can't access data: %s\n",(name)) ; exit(1); }          \
       if( DSET_BRICK_TYPE((ds),pv) == MRI_complex ){                                \
          fprintf(stderr,"*** Can't use complex data: %s\n",(name)) ; exit(1); }     \
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
  Data structure.
*/

typedef struct input_options
{
  char * infilename;            /* name of input file */
  int nx;                       /* number of voxels along x-axis */
  int ny;                       /* number of voxels along y-axis */
  int nz;                       /* number of voxels along z-axis */
  int nxyz;                     /* total number of voxels */
  float dx;                     /* voxel size along x-axis */
  float dy;                     /* voxel size along y-axis */
  float dz;                     /* voxel size along z-axis */
  int quiet;                    /* set to 1 to suppress screen output */
  char * outfilename;           /* name of output file */
} input_options;


/*---------------------------------------------------------------------------*/
/*
  Routine to display 3dFWHM help menu.
*/

void display_help_menu()
{
  printf 
    (
     "This program estimates the Filter Width Half Maximum (FWHM).  \n\n"
     "Usage: \n"
     "3dFWHM \n"
     "-dset file         file = name of input AFNI 3d dataset  \n"
     "[-quiet]           suppress screen output                \n" 
     "[-out file]        file = name of output file            \n"
    );
  
  exit(0);
}

/*---------------------------------------------------------------------------*/
/*
   Routine to print error message and stop.
*/

void FWHM_error (char * message)
{
   fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
   exit(1);
}


/*---------------------------------------------------------------------------*/
/*
   Routine to get the dimensions of the 3d AFNI data sets.
*/

void get_dimensions (input_options * option_data)
{
  
   THD_3dim_dataset * dset=NULL;

   /*----- read first dataset to get dimensions, etc. -----*/

   dset = THD_open_one_dataset( option_data->infilename) ;
   if( ! ISVALID_3DIM_DATASET(dset) ){
      fprintf(stderr,"*** Unable to open dataset file %s\n", 
              option_data->infilename);
      exit(1) ;
   }

   /*----- voxel dimensions and data set dimensions -----*/
   option_data->dx = fabs(dset->daxes->xxdel) ;
   option_data->dy = fabs(dset->daxes->yydel) ;
   option_data->dz = fabs(dset->daxes->zzdel) ;
   option_data->nx = dset->daxes->nxx ;
   option_data->ny = dset->daxes->nyy ;
   option_data->nz = dset->daxes->nzz ;       
   option_data->nxyz = option_data->nx * option_data->ny * option_data->nz ;

   THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;

}


/*---------------------------------------------------------------------------*/
/*
  Routine to read one AFNI data set from the input file. 
  The data is converted to floating point (in ffim).
*/

void read_afni_data (input_options * option_data,  float * ffim)
{
  int iv;                          /* index number of intensity sub-brick */
  THD_3dim_dataset * dset=NULL;    /* data set pointer */
  void * vfim = NULL;              /* image data pointer */
  int nx, ny, nz, nxyz;            /* data set dimensions in voxels */
  
  nx = option_data->nx;
  ny = option_data->ny;
  nz = option_data->nz;
  nxyz = option_data->nxyz;
  
  
  /*----- read in the data -----*/
  DOPEN(dset,option_data->infilename) ;
  iv = DSET_PRINCIPAL_VALUE(dset) ;
  
  /*----- convert it to floats (in ffim) -----*/
  SUB_POINTER(dset,iv,0,vfim) ;
  EDIT_coerce_scale_type( nxyz , DSET_BRICK_FACTOR(dset,iv) ,
			  DSET_BRICK_TYPE(dset,iv),vfim ,      /* input  */
			  MRI_float               ,ffim  ) ;   /* output */
  
  THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the input options.
*/
  
void initialize_options (input_options * option_data)
{
  option_data->infilename = NULL;    /* name of input file */
  option_data->quiet = 0;            /* generate screen output (default)  */
  option_data->outfilename = NULL;   /* name of output file */
}


/*---------------------------------------------------------------------------*/
/*
  Routine to get user specified input options.
*/

void get_options (int argc, char ** argv, input_options * option_data)
{
  int nopt = 1;                  /* input option argument counter */
  int ival;                      /* integer input */
  float fval;                    /* float input */
  char message[MAX_NAME_LENGTH];            /* error message */

  
  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();  
  

  /*----- initialize the input options -----*/
  initialize_options (option_data);
    

  /*----- main loop over input options -----*/
  while (nopt < argc )
    {
      
      /*-----   -dset filename   -----*/
      if (strncmp(argv[nopt], "-dset", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FWHM_error ("need argument after -dset ");
	  option_data->infilename = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->infilename, argv[nopt]);
	  nopt++;
	  continue;
	}

      
      /*-----   -quiet q  -----*/
      if (strncmp(argv[nopt], "-quiet", 6) == 0)
	{
	  option_data->quiet = 1;
	  nopt++;
	  continue;
	}


      /*-----   -out filename   -----*/
      if (strncmp(argv[nopt], "-out", 4) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FWHM_error ("need argument after -out ");
	  option_data->outfilename = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->outfilename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*----- unknown command -----*/
      FWHM_error ("unrecognized command line option ");
    }
  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check for valid inputs.
*/
  
void check_for_valid_inputs (option_data)
{
}


/*---------------------------------------------------------------------------*/
/*
  Routine to perform program initialization.
*/


void initialize (int argc, char ** argv,  
		 input_options ** option_data, float ** fim)
{


  /*----- allocate memory space for input options -----*/   
  *option_data = (input_options *) malloc(sizeof(input_options));
  if (*option_data == NULL)
    FWHM_error ("memory allocation error");
  
  /*----- get command line inputs -----*/
  get_options(argc, argv, *option_data);

  /*----- check for valid inputs -----*/
  check_for_valid_inputs (*option_data);

  /*-----  get data set dimensions -----*/
  get_dimensions (*option_data);

  /*----- allocate memory space for image data -----*/   
  *fim = (float *) malloc( (*option_data)->nxyz * sizeof(float) );
  if (*fim == NULL)
    FWHM_error ("memory allocation error");
  
  /*----- read input data set -----*/
  read_afni_data (*option_data, *fim);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to estimate the Gaussian filter width required to generate the data.
*/
   
void estimate_gfw (input_options * option_data, float * fim,
		   float * sx, float * sy, float * sz)
{
  int nx;                       /* number of voxels along x-axis */
  int ny;                       /* number of voxels along y-axis */
  int nz;                       /* number of voxels along z-axis */
  int nxy, nxyz;                /* total number of voxels */
  int ixyz;                     /* voxel index */
  float dx;                     /* voxel size along x-axis */
  float dy;                     /* voxel size along y-axis */
  float dz;                     /* voxel size along z-axis */
  int ix, jy, kz, ixyz2;
  float fsum, fsq, var;
  float dfdx, dfdxsum, dfdxsq, varxx;
  float dfdy, dfdysum, dfdysq, varyy;
  float dfdz, dfdzsum, dfdzsq, varzz;
  int countx, county, countz;
  float arg;


  /*----- initialize local variables -----*/
  nx = option_data->nx;
  ny = option_data->ny;
  nz = option_data->nz;
  dx = option_data->dx;
  dy = option_data->dy;
  dz = option_data->dz;
  nxyz = option_data->nxyz;
  nxy = nx * ny;


  /*----- estimate the variance of the data -----*/
  fsum = 0.0;
  fsq = 0.0;
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      fsum += fim[ixyz];
      fsq  += fim[ixyz] * fim[ixyz];
    }
  var = (fsq - (fsum * fsum)/nxyz) / (nxyz-1);


  /*----- estimate the partial derivatives -----*/
  dfdxsum = 0.0;   dfdysum = 0.0;   dfdzsum = 0.0;
  dfdxsq = 0.0;    dfdysq  = 0.0;   dfdzsq = 0.0;
  countx = 0;      county = 0;      countz = 0;
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      IJK_TO_THREE (ixyz, ix, jy, kz, nx, nxy);

      if (ix+1 < nx)
	{
	  ixyz2 = THREE_TO_IJK (ix+1, jy, kz, nx, nxy);
	  dfdx = (fim[ixyz2] - fim[ixyz]) / 1.0;
	  dfdxsum += dfdx;
	  dfdxsq  += dfdx * dfdx;
	  countx += 1;
	}

      if (jy+1 < ny)
	{
	  ixyz2 = THREE_TO_IJK (ix, jy+1, kz, nx, nxy);
	  dfdy = (fim[ixyz2] - fim[ixyz]) / 1.0;
	  dfdysum += dfdy;
	  dfdysq  += dfdy * dfdy;
	  county += 1;
	}
      
      if (kz+1 < nz)
	{
	  ixyz2 = THREE_TO_IJK (ix, jy, kz+1, nx, nxy);
	  dfdz = (fim[ixyz2] - fim[ixyz]) / 1.0;
	  dfdzsum += dfdz;
	  dfdzsq  += dfdz * dfdz;
	  countz += 1;
	}
      
     }
 
  /*----- estimate the variance of the partial derivatives -----*/
  if (countx < 2)  
    varxx = 0.0;
  else  
    varxx = (dfdxsq - (dfdxsum * dfdxsum)/countx) / (countx-1);

  if (county < 2)
    varyy = 0.0;
  else
    varyy = (dfdysq - (dfdysum * dfdysum)/county) / (county-1);

  if (countz < 2)
    varzz = 0.0;
  else
    varzz = (dfdzsq - (dfdzsum * dfdzsum)/countz) / (countz-1);


  /*----- now estimate the equivalent Gaussian filter width -----*/
  arg = 1.0 - 0.5*(varxx/var);
  if ( (arg <= 0.0) || (varxx == 0.0) )
    *sx = 0.0;
  else
    *sx = sqrt( -1.0 / (4.0*log(arg)) ) * dx;

  arg = 1.0 - 0.5*(varyy/var);
  if ( (arg <= 0.0) || (varyy == 0.0) )
    *sy = 0.0;
  else
    *sy = sqrt( -1.0 / (4.0*log(arg)) ) * dy;

  arg = 1.0 - 0.5*(varzz/var);
  if ( (arg <= 0.0) || (varzz == 0.0) )
    *sz = 0.0;
  else
    *sz = sqrt( -1.0 / (4.0*log(arg)) ) * dz;


  if (!(option_data->quiet))  
    {
      printf ("var  =%f \n", var);
      printf ("varxx=%f varyy=%f varzz=%f \n", varxx, varyy, varzz);
      printf ("   sx=%f    sy=%f    sz=%f \n", *sx, *sy, *sz);
    }
}


/*---------------------------------------------------------------------------*/
/*
  Routine to generate requested output.
*/
  
void output_results (input_options * option_data, float sx, float sy, float sz)
{
  char message[MAX_NAME_LENGTH];     /* error message */
  char filename[MAX_NAME_LENGTH];    /* output file name */ 
  FILE * fout;


  /*----- if output file has not been specified, use stdout -----*/
  if (option_data->outfilename == NULL)
    fout = stdout;
  else
    {
      /*----- see if output file already exists -----*/
      strcpy (filename, option_data->outfilename);
      fout = fopen (filename, "r");
      if (fout != NULL)
	{
	  sprintf (message, "file %s already exists. ", filename); 
	  FWHM_error (message);
	}
      
      /*----- open file for output -----*/
      fout = fopen (filename, "w");
      if (fout == NULL)
	{ 
	  FWHM_error ("unable to write file ");
	}
    }
  
  /*----- print out the results -----*/
  fprintf (fout, "\n\nProgram %s \n\n", PROGRAM_NAME);
  fprintf (fout, "Last revision: %s \n\n", LAST_MOD_DATE);
  fprintf (fout, "Gaussian filter widths: \n");
  fprintf (fout, "sigmax = %5.2f   FWHMx = %5.2f \n", 
	   sx, sx * 2.0*sqrt(2.0*log(2.0)));
  fprintf (fout, "sigmay = %5.2f   FWHMy = %5.2f \n", 
	   sy, sy * 2.0*sqrt(2.0*log(2.0)));
  fprintf (fout, "sigmaz = %5.2f   FWHMz = %5.2f \n\n", 
	   sz, sz * 2.0*sqrt(2.0*log(2.0)));

  fclose(fout);

}
 
 
/*---------------------------------------------------------------------------*/
/*
  Routine to terminate program.
*/
  
void terminate (input_options ** option_data, float ** fim)
{
  free (*option_data);   *option_data = NULL;
  free (*fim);           *fim = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Calculation of FWHM.
*/
 
void main (int argc, char ** argv)
{
  input_options * option_data = NULL;
  float * fim = NULL;
  float sx, sy, sz;

  
  /*----- program initialization -----*/
  initialize (argc, argv, &option_data, &fim);

  if (!(option_data->quiet))  printf ("\n\nProgram %s \n\n", PROGRAM_NAME);
  if (!(option_data->quiet))  printf ("Last revision: %s\n", LAST_MOD_DATE);


  /*----- estimate equivalent gaussian filter width -----*/
  estimate_gfw (option_data, fim, &sx, &sy, &sz );
  
  
  /*----- generate requested output -----*/
  output_results (option_data, sx, sy, sz);
  
  /*----- terminate program -----*/
  terminate (&option_data, &fim);
  
}














