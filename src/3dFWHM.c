/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
  This program estimates the Filter Width Half Maximum (FWHM) of the Gaussian
  filter required to produce clustering in noise data equivalent to that 
  of the input AFNI 3d dataset.
  
  File:    3dFWHM.c
  Author:  B. D. Ward
  Date:    20 February 1997

  Mod:     Added -mask option to restrict calculations to masked voxels only.
           Also, allow the '[]' sub-brick selector for input datasets.
  Date:    02 March 2000  

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

*/



/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dFWHM"                        /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_INITIAL "20 February 1997"/* date of initial program release */
#define PROGRAM_LATEST  "15 August 2001"  /* date of latest program revision */

/*---------------------------------------------------------------------------*/

#define MAX_NAME_LENGTH 80            /* max. strength length for file names */


#include "mrilib.h"


/*---------------------------------------------------------------------------*/

/** macro to open a dataset and make it ready for processing **/

#define DOPEN(ds,name)                                                               \
   do{ int pv ; (ds) = THD_open_dataset((name)) ;                                    \
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
  char * maskfilename;          /* name of mask file */
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
     "[-mask mname]      mname = filename of 3d mask dataset   \n"
     "[-quiet]           suppress screen output                \n" 
     "[-out file]        file = name of output file            \n"
    );

   printf("\n" MASTER_SHORTHELP_STRING ) ;
  
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

   dset = THD_open_dataset( option_data->infilename) ;
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

void read_afni_data (input_options * option_data,  char * filename, 
		     float * ffim)
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
  DOPEN(dset,filename) ;
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
  option_data->maskfilename = NULL;  /* name of mask file */
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
  
  
  /*----- add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv); 


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

      
      /*-----   -mask filename   -----*/
      if (strncmp(argv[nopt], "-mask", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  FWHM_error ("need argument after -mask ");
	  option_data->maskfilename = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (option_data->maskfilename, argv[nopt]);
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
		 input_options ** option_data, float ** fim, float ** fmask)
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
  read_afni_data (*option_data, (*option_data)->infilename, *fim);


  /*----- check for mask file -----*/
  if ((*option_data)->maskfilename != NULL)
    {
      /*----- allocate memory space for mask data -----*/   
      *fmask = (float *) malloc( (*option_data)->nxyz * sizeof(float) );
      if (*fmask == NULL)  FWHM_error ("memory allocation error");
      
      /*----- read mask data set -----*/
      read_afni_data (*option_data, (*option_data)->maskfilename, *fmask);
      
    }

}


/*---------------------------------------------------------------------------*/
/*
  Routine to estimate the Gaussian filter width required to generate the data.
*/
   
void estimate_gfw (input_options * option_data, float * fim, float * fmask,
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
  int count, countx, county, countz;
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
  count = 0;
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      if (fmask != NULL)
	if (fmask[ixyz] == 0.0)  continue;

      count++;
      fsum += fim[ixyz];
      fsq  += fim[ixyz] * fim[ixyz];
    }
  var = (fsq - (fsum * fsum)/count) / (count-1);


  /*----- estimate the partial derivatives -----*/
  dfdxsum = 0.0;   dfdysum = 0.0;   dfdzsum = 0.0;
  dfdxsq = 0.0;    dfdysq  = 0.0;   dfdzsq = 0.0;
  countx = 0;      county = 0;      countz = 0;
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      if (fmask != NULL)
	if (fmask[ixyz] == 0.0)  continue;

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
      printf ("count=%d \n", count);
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
  fprintf (fout, "\n\n");
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
  
void terminate (input_options ** option_data, float ** fim, float ** fmask)
{
  free (*option_data);   *option_data = NULL;
  free (*fim);           *fim = NULL;
  if (*fmask != NULL)
    {  free (*fmask);       *fmask = NULL; }
}


/*---------------------------------------------------------------------------*/
/*
  Calculation of FWHM.
*/
 
int main (int argc, char ** argv)
{
  input_options * option_data = NULL;
  float * fim = NULL;
  float * fmask = NULL;
  float sx, sy, sz;

  
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR); 
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");


  /*----- program initialization -----*/
  initialize (argc, argv, &option_data, &fim, &fmask);


  /*----- estimate equivalent gaussian filter width -----*/
  estimate_gfw (option_data, fim, fmask, &sx, &sy, &sz );
  
  
  /*----- generate requested output -----*/
  output_results (option_data, sx, sy, sz);
  
  /*----- terminate program -----*/
  terminate (&option_data, &fim, &fmask);
  
  exit(0);
}














