/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This program performs 2d image registration of slices contained in an AFNI
  3d+time dataset.  This program was adapted from plug_imreg.c and imreg.c.


  File:    2dImReg.c
  Author:  B. Douglas Ward
  Date:    04 February 1998


  Mod:     Added routines to write the registration parameters, and the RMS 
           error, to user specified ASCII files.
  Date:    20 March 1998

  Mod:     Added option to change dx and dy output format from pixels to mm.
  Date:    24 March 1998

  Mod:     No longer assume that input and base datasets have same length.
           This problem was reported by Paul Reber.
  Date:    3 June 1998

  Mod:     Routine eval_registration extended to include byte and float datum
           types for base and input datasets.
  Date:    3 June 1998

  Mod:     Corrected problem with base image memory deallocation.
  Date:    20 July 1998

  Mod:     Added changes for incorporating History notes.
  Date:    10 September 1999

  Mod:     Set MAX_NAME_LENGTH equal to THD_MAX_NAME.
  Date:    02 December 2002

  Mod:     If nsl == 0, set num_slices equal to nz.   [4 occurrences]
  Date:    06 October 2003  [rickr]
*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME    "2dImReg"                   /* name of this program */
#define PROGRAM_INITIAL "04 Feb 1998"     /* date of initial program release */
#define PROGRAM_LATEST  "02 Dec 2002"     /* date of latest program revision */

/*---------------------------------------------------------------------------*/

#include "mrilib.h"
#include "matrix.h"

#define MAX_NAME_LENGTH THD_MAX_NAME  /* max. string length for file names */ 
#define STATE_DIM 4                   /* number of registration parameters */ 


/*----- Global variables -----*/ 
int * t_to_z = NULL;           /* convert time-order to z-order of slices */  
int * z_to_t = NULL;           /* convert z-order to time-order of slices */


static char * commandline = NULL ;       /* command line for history notes */
 

typedef struct IR_options      /* user input options */
{
  char * input_filename;       /* file name for input 3d+time dataset */
  char * base_filename;        /* file name for reference (base) volume */
  int base_vol_index;          /* image number for base volume */
  int nofine;                  /* boolean for no fine fit */
  float blur;                  /* FWHM of blurring prior to registration */
  float dxy;                   /* convergence tolerance for translations */
  float dphi;                  /* convergence tolerance for rotations */
  char * new_prefix;           /* prefix name for registered dataset */
  char * dprefix;              /* prefix name for registration parameters */
  int dmm;                     /* change dx and dy output from pixels to mm */
  char * rprefix;              /* prefix name for volume RMS error */
  int debug;                   /* write additional output to screen */
} IR_options;



/*---------------------------------------------------------------------------*/
/*
  Routine to display 2dImReg help menu.
*/

void display_help_menu()
{
  printf 
    (
     "This program performs 2d image registration.  Image alignment is      \n"
     "performed on a slice-by-slice basis for the input 3d+time dataset,    \n"
     "relative to a user specified base image.                              \n"
     "                                                                      \n"
     "Usage:                                                                \n"
     "2dImReg                                                               \n"
     "-input fname           Filename of input 3d+time dataset to process   \n"
     "-basefile fname        Filename of 3d+time dataset for base image     \n"
     "                         (default = current input dataset)            \n"
     "-base num              Time index for base image  (0 <= num)          \n"
     "                         (default:  num = 3)                          \n"
     "-nofine                Deactivate fine fit phase of image registration\n"
     "                         (default:  fine fit is active)               \n"
     "-fine blur dxy dphi    Set fine fit parameters                        \n"
     "   where:                                                             \n"
     "     blur = FWHM of blurring prior to registration (in pixels)        \n"
     "               (default:  blur = 1.0)                                 \n"
     "     dxy  = Convergence tolerance for translations (in pixels)        \n"
     "               (default:  dxy  = 0.07)                                \n"
     "     dphi = Convergence tolerance for rotations (in degrees)          \n"
     "               (default:  dphi = 0.21)                                \n"
     "                                                                      \n"
     "-prefix pname     Prefix name for output 3d+time dataset              \n"
     "                                                                      \n"
     "-dprefix dname    Write files 'dname'.dx, 'dname'.dy, 'dname'.psi     \n"
     "                    containing the registration parameters for each   \n"
     "                    slice in chronological order.                     \n"
     "                    File formats:                                     \n"
     "                      'dname'.dx:    time(sec)   dx(pixels)           \n"
     "                      'dname'.dy:    time(sec)   dy(pixels)           \n"
     "                      'dname'.psi:   time(sec)   psi(degrees)         \n"
     "-dmm              Change dx and dy output format from pixels to mm    \n"
     "                                                                      \n"
     "-rprefix rname    Write files 'rname'.oldrms and 'rname'.newrms       \n"
     "                    containing the volume RMS error for the original  \n"
     "                    and the registered datasets, respectively.        \n"
     "                    File formats:                                     \n"
     "                      'rname'.oldrms:   volume(number)   rms_error    \n"
     "                      'rname'.newrms:   volume(number)   rms_error    \n"
     "                                                                      \n"
     "-debug            Lots of additional output to screen                 \n"
    );
  
  exit(0);
}


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void IR_error 
(
  char * message               /* error message to be displayed */
)

{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the input options.
*/

void initialize_options
(
  IR_options ** opt            /* user input options */
)

{
  (*opt) = (IR_options *) malloc (sizeof(IR_options));

  (*opt)->input_filename = NULL;
  (*opt)->base_filename = NULL;
  (*opt)->base_vol_index = -3;       
  (*opt)->nofine = 0;
  (*opt)->blur = 1.0;     
  (*opt)->dxy  = 0.07;    
  (*opt)->dphi = 0.21;   
  (*opt)->new_prefix = NULL;
  (*opt)->dprefix = NULL;
  (*opt)->rprefix = NULL;
  (*opt)->dmm = 0;
  (*opt)->debug = 0;
}


/*---------------------------------------------------------------------------*/
/*
   Routine to get user specified input options.
*/

void get_user_inputs 
(
  int argc,                       /* number of input arguments */
  char ** argv,                   /* array of input arguments */ 
  IR_options ** option_data       /* user input options */
)

{
  int nopt = 1;                   /* input option argument counter */
  int ival;
  float fval;
  char message[80];               /* error message */

  
  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();  
  

   /*----- main loop over input options -----*/
  while (nopt < argc )
    {

      /*-----   -input filename   -----*/
      if (strncmp(argv[nopt], "-input", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  IR_error ("Need argument after -input ");
	  (*option_data)->input_filename 
	    = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy ((*option_data)->input_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
     

      /*-----   -basefile filename   -----*/
      if (strncmp(argv[nopt], "-basefile", 9) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  IR_error ("Need argument after -basefile ");
	  (*option_data)->base_filename 
	    = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy ((*option_data)->base_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
     

      /*-----   -base num  -----*/
      if (strncmp(argv[nopt], "-base", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  IR_error ("Need argument after -base ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival < 0) 
	    IR_error ("Illegal argument after -base  ( must be >= 0 ) ");
	  (*option_data)->base_vol_index = ival;
	  nopt++;
	  continue;
	}


      /*-----   -prefix pname   -----*/
      if (strncmp(argv[nopt], "-prefix", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  IR_error ("Need argument after -prefix ");
	  (*option_data)->new_prefix 
	    = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy ((*option_data)->new_prefix, argv[nopt]);

	  nopt++;
	  continue;
	}
     

      /*-----   -dprefix dname   -----*/
      if (strncmp(argv[nopt], "-dprefix", 8) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  IR_error ("Need argument after -dprefix ");
	  (*option_data)->dprefix 
	    = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy ((*option_data)->dprefix, argv[nopt]);

	  nopt++;
	  continue;
	}
     

      /*-----   -rprefix rname   -----*/
      if (strncmp(argv[nopt], "-rprefix", 8) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  IR_error ("Need argument after -rprefix ");
	  (*option_data)->rprefix 
	    = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy ((*option_data)->rprefix, argv[nopt]);

	  nopt++;
	  continue;
	}
     

      /*-----   -nofine -----*/
      if (strncmp(argv[nopt], "-nofine", 7) == 0)
	{
	  (*option_data)->nofine = 1;
	  nopt++;
	  continue;
	}

           
      /*-----   -fine blur dxy dphi  -----*/
      if (strncmp(argv[nopt], "-fine", 5) == 0)
	{
	  nopt++;
	  if (nopt+2 >= argc)  IR_error ("Need 3 arguments after -fine ");
	  (*option_data)->nofine = 0;

	  sscanf (argv[nopt], "%f", &fval);
	  if (fval <= 0.0) 
	    IR_error ("Illegal argument for blur  ( must be > 0 ) ");
	  (*option_data)->blur = fval;
	  nopt++;

	  sscanf (argv[nopt], "%f", &fval);
	  if (fval <= 0.0)
	    IR_error ("Illegal argument for dxy  ( must be > 0 ) ");
	  (*option_data)->dxy = fval;
	  nopt++;

	  sscanf (argv[nopt], "%f", &fval);
	  if (fval <= 0.0)
	    IR_error ("Illegal argument for dphi  ( must be > 0 ) ");
	  (*option_data)->dphi = fval;
	  nopt++;

	  continue;
	}
      

      /*-----   -dmm -----*/
      if (strncmp(argv[nopt], "-dmm", 4) == 0)
	{
	  (*option_data)->dmm = 1;
	  nopt++;
	  continue;
	}

           
      /*-----   -debug -----*/
      if (strncmp(argv[nopt], "-debug", 6) == 0)
	{
	  (*option_data)->debug = 1;
	  nopt++;
	  continue;
	}

           
      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      IR_error (message);


    }
      
 
}


/*---------------------------------------------------------------------------*/
/*
   Routine to read a 3d+time dataset.
*/

void read_dataset 
(
  char * filename,                /* file name of 3d+time dataset */
  THD_3dim_dataset ** dset        /* pointer to 3d+time dataset */
)

{
  char message[80];               /* error message */


  /*----- Open the 3d+time dataset -----*/
  *dset = THD_open_one_dataset (filename);
  if (*dset == NULL)  
    { 
      sprintf (message, 
	       "Unable to open data file: %s", filename);
      IR_error (message);
    }

}


/*---------------------------------------------------------------------------*/
/*
   Initialize the slice sequence arrays.
*/

void initialize_slice_sequence 
(
  IR_options * option_data,        /* user input options */
  THD_3dim_dataset * dset          /* pointer to 3d+time dataset */
)

{
  int num_slices;                  /* number of slices per volume */
  int ivolume;                     /* volume index number */
  int itemp;                       /* temporary variable */
  float ttemp;                     /* temporary variable */
  float * time_array = NULL;       /* array of slice acquisition times */
  int iz, i, j;                    /* index numbers */
  float z;                         /* slice z location */
   

  if (!dset->taxis) {               /* Jan 07 [ZSS] */
      IR_error ("NULL taxis, should not be here.");   
  }
  
  /*----- Initialize local variables -----*/
  num_slices = dset->taxis->nsl;  
  
  ivolume = 0;

  if ( num_slices <= 0 )            /* 06 Oct 2003 [rickr] */
      num_slices = dset->daxes->nzz;

  /*----- Allocate memory for arrays -----*/
  t_to_z = (int *) malloc (sizeof(int) * num_slices);
  z_to_t = (int *) malloc (sizeof(int) * num_slices);
  time_array = (float *) malloc (sizeof(float) * num_slices);


  /*----- Initialize array of slice acquisition times -----*/
  for (iz = 0;  iz < num_slices;  iz++)
    {
      z = iz * dset->taxis->dz_sl + dset->taxis->zorg_sl;
      time_array[iz] = THD_timeof (ivolume, z, dset->taxis);
      t_to_z[iz] = iz;
    }


  /*----- Sort slice z-indices by increasing time -----*/
  for (i = 0;  i < num_slices-1;  i++)
    for (j = i+1;  j < num_slices;  j++)
      if (time_array[j] < time_array[i])
	{
	  itemp = t_to_z[i];
	  t_to_z[i] = t_to_z[j];
	  t_to_z[j] = itemp;

	  ttemp = time_array[i];
	  time_array[i] = time_array[j];
	  time_array[j] = ttemp;
	} 


  /*----- Sort slice time-indices by increasing z index -----*/
  for (i = 0;  i < num_slices;  i++)
    {
      j = t_to_z[i];
      z_to_t[j] = i;
    }


  /*----- Write out the slice ordering arrays -----*/
  if (option_data->debug)
    for (i = 0;  i < num_slices;  i++)
      printf ("time[%2d] = %12.3f   t_to_z[%2d] = %2d   z_to_t[%2d] = %2d\n",
	      i, time_array[i], i, t_to_z[i], i, z_to_t[i]);

  
  /*----- Release memory -----*/
  free (time_array);   time_array = NULL;
} 


/*---------------------------------------------------------------------------*/
/*
   Routine to initialize the array of state vectors.
*/

void initialize_state_history 
(
  THD_3dim_dataset * dset,            /* pointer to input 3d+time dataset */
  vector ** state_history             /* time series of state vectors */
)

{
  int num_slices;                     /* number of slices per volume */
  int ts_length;                      /* number of volumes */
  int num_vectors;                    /* total number of state vectors */
  int i;                              /* state vector index */


  /*----- Initialize local variables -----*/
  num_slices = dset->taxis->nsl;

  if ( num_slices <= 0 )              /* 06 Oct 2003 [rickr] */
      num_slices = dset->daxes->nzz;

  ts_length = DSET_NUM_TIMES(dset);
  num_vectors = ts_length * num_slices;


  /*----- Allocate memory for array of state vectors -----*/
  *state_history = (vector *) malloc (sizeof(vector) * num_vectors);


  /*----- Initialize array of state vectors -----*/
  for (i = 0;  i < num_vectors;  i++)
    {
      vector_initialize (&((*state_history)[i]));
      vector_create (STATE_DIM, &((*state_history)[i]));
    }
}

 
/*---------------------------------------------------------------------------*/
/*
   Routine to initialize the RMS error arrays.
*/

void initialize_rms_arrays 
(
  THD_3dim_dataset * dset,       /* pointer to input 3d+time dataset */
  float ** old_rms_array,        /* volume RMS error for input dataset */
  float ** new_rms_array         /* volume RMS error for registered dataset */
)

{
  int ts_length;                 /* number of volumes */


  ts_length = DSET_NUM_TIMES(dset);

  /*----- Allocate space for RMS error arrays -----*/
  *old_rms_array = (float *) malloc (sizeof(float) * ts_length);
  *new_rms_array = (float *) malloc (sizeof(float) * ts_length);
  

}

 
/*---------------------------------------------------------------------------*/
/*
  Routine to perform all program initialization.
*/

void initialize_program
(
  int argc,                       /* number of input arguments */
  char ** argv,                   /* array of input arguments */ 
  IR_options ** option_data,      /* user input options */
  vector ** state_history,        /* time series of state vectors */
  float ** old_rms_array,         /* volume RMS error for input dataset */
  float ** new_rms_array          /* volume RMS error for registered dataset */
)

{
  THD_3dim_dataset * dset = NULL;


  /*----- save command line for history notes -----*/
  commandline = tross_commandline( PROGRAM_NAME , argc,argv ) ;


  /*----- Initialize input options -----*/
  initialize_options (option_data);


  /*----- Get user inputs -----*/
  get_user_inputs (argc, argv, option_data);

  /*----- Read the input 3d+time dataset to be registered -----*/
  read_dataset ((*option_data)->input_filename, &dset);

  
  /*----- Initialize the z-slice time order arrays -----*/
  if (dset->taxis) initialize_slice_sequence (*option_data, dset);


  /*----- Initialize the array of state vectors -----*/
  if ((*option_data)->dprefix != NULL)
    initialize_state_history (dset, state_history);


  /*----- Allocate space for RMS error arrays -----*/
  if ((*option_data)->rprefix != NULL)
    initialize_rms_arrays (dset, old_rms_array, new_rms_array);


  /*----- Release memory -----*/
  THD_delete_3dim_dataset (dset, False);   dset = NULL;

}


/*---------------------------------------------------------------------------*/
/*
   Routine to make a copy of a dataset, with data attached.
   This routine is copied directly from afni_plugin.c
*/

THD_3dim_dataset * copy_dset( THD_3dim_dataset * dset , char * new_prefix )
{
   THD_3dim_dataset * new_dset ;
   int ival , ityp , nbytes , nvals ;
   void * new_brick , * old_brick ;

   /*-- sanity check --*/

   if( ! ISVALID_3DIM_DATASET(dset) ) return NULL ;

   /*-- make the empty copy --*/

   new_dset = EDIT_empty_copy( dset ) ;

   /*-- change its name? --*/

   if( new_prefix != NULL )
      EDIT_dset_items( new_dset ,
                          ADN_prefix , new_prefix ,
                          ADN_label1 , new_prefix ,
                       ADN_none ) ;

   /*-- make brick(s) for this dataset --*/

   THD_load_datablock( dset->dblk ) ;  /* make sure old one is in memory */

   nvals = DSET_NVALS(dset) ;

   for( ival=0 ; ival < nvals ; ival++ ){
      ityp      = DSET_BRICK_TYPE(new_dset,ival) ;   /* type of data */
      nbytes    = DSET_BRICK_BYTES(new_dset,ival) ;  /* how much data */
      new_brick = malloc( nbytes ) ;                 /* make room */

      if( new_brick == NULL ){
        THD_delete_3dim_dataset( new_dset , False ) ;
        return NULL ;
      }

      EDIT_substitute_brick( new_dset , ival , ityp , new_brick ) ;

      /*-- copy data from old brick to new brick --*/

      old_brick = DSET_BRICK_ARRAY(dset,ival) ;

      if( old_brick == NULL ){
         THD_delete_3dim_dataset( new_dset , False ) ;
         return NULL ;
      }

      memcpy( new_brick , old_brick , nbytes ) ;
   }

   return new_dset ;
}


/*---------------------------------------------------------------------------*/
/*
  Check whether output file already exists.
*/

void check_output_file 
(
  THD_3dim_dataset * dset,      /* input afni data set pointer */
  char * filename               /* output file name */
)

{
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int ierror;                         /* number of errors in editing data */
  
  
  /*-- make an empty copy of the input dataset --*/
  new_dset = EDIT_empty_copy( dset ) ;
  
  
  ierror = EDIT_dset_items( new_dset ,
			    ADN_prefix , filename ,
			    ADN_label1 , filename ,
			    ADN_self_name , filename ,
			    ADN_type , ISHEAD(dset) ? HEAD_FUNC_TYPE : 
                               			      GEN_FUNC_TYPE ,
			    ADN_none ) ;
  
  if( ierror > 0 ){
    fprintf(stderr,
	    "*** %d errors in attempting to create output dataset!\n", ierror ) ;
    exit(1) ;
  }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) ){
    fprintf(stderr,
	    "*** Output dataset file %s already exists--cannot continue!\a\n",
	    new_dset->dblk->diskptr->header_name ) ;
    exit(1) ;
  }
  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}


/*---------------------------------------------------------------------------*/
/*
  Evaluate accuracy of registration.
*/

void eval_registration
(
  IR_options * option_data,       /* user input options */
  THD_3dim_dataset * old_dset,    /* input dataset */
  THD_3dim_dataset * new_dset,    /* output datasets */
  THD_3dim_dataset * base_dset,   /* base image dataset */
  int base,                       /* base volume index */
  float * old_rms_array,          /* volume RMS error for input dataset */
  float * new_rms_array           /* volume RMS error for registered dataset */
  )
     
{
  int nx, ny, nz, nxyz;
  int ix, jy, kz;
  int ixyz;
  int datum, base_datum;
  float old_sse, new_sse;
  float diff;
  float old_rmse, new_rmse;
  int ivolume, num_volumes;
  byte  * bold = NULL, * bnew = NULL, * bbase = NULL;
  short * sold = NULL, * snew = NULL, * sbase = NULL;
  float * fold = NULL, * fnew = NULL, * fbase = NULL;
  float float_base, float_old, float_new;


  /*----- Initialize local variables -----*/
  nx = old_dset->daxes->nxx;
  ny = old_dset->daxes->nyy;
  nz = old_dset->daxes->nzz;
  nxyz = nx * ny * nz;
  num_volumes = DSET_NUM_TIMES (old_dset);
  datum       = DSET_BRICK_TYPE (new_dset,0) ;
  base_datum  = DSET_BRICK_TYPE (base_dset,0);


  /*----- Set base dataset pointer depending on base datum type -----*/
  switch ( base_datum )
    { 
    case MRI_byte:
      bbase = (byte *) DSET_ARRAY(base_dset,base);  break;
      
    case MRI_short:
      sbase = (short *) DSET_ARRAY(base_dset,base);  break;

    case MRI_float:
      fbase = (float *) DSET_ARRAY(base_dset,base);  break;
    }


  for (ivolume = 0;  ivolume < num_volumes; ivolume++)
    {
      old_sse = 0.0;
      new_sse = 0.0;

      /*----- Set old and new dataset pointers depending on datum type -----*/
      switch ( datum )
	{  
	case MRI_byte:
	  bold = (byte *) DSET_ARRAY(old_dset,ivolume);  
	  bnew = (byte *) DSET_ARRAY(new_dset,ivolume);  break;
	  
	case MRI_short:
	  sold = (short *) DSET_ARRAY(old_dset,ivolume);  
	  snew = (short *) DSET_ARRAY(new_dset,ivolume);  break;
	  
	case MRI_float:
	  fold = (float *) DSET_ARRAY(old_dset,ivolume);  
	  fnew = (float *) DSET_ARRAY(new_dset,ivolume);  break;
	}

      
      for (kz = 0;  kz < nz;  kz++)
	for (jy = 0;  jy < ny;  jy++)
	  for (ix = 0;  ix < nx;  ix++)
	    {
	      ixyz = ix + jy*nx + kz*nx*ny;

	      /*----- Get base voxel floating point value -----*/
	      switch ( base_datum )
		{ 
		case MRI_byte:   float_base = (float) bbase[ixyz];  break;
		  
		case MRI_short:  float_base = (float) sbase[ixyz];  break;
		  
		case MRI_float:  float_base = fbase[ixyz];  break;
		}
	      
	      /*----- Get old and new voxel floating point value -----*/
	      switch ( datum )
		{  
		case MRI_byte:
		  float_old = (float) bold[ixyz];  
		  float_new = (float) bnew[ixyz];  break;
		  
		case MRI_short:
		  float_old = (float) sold[ixyz];  
		  float_new = (float) snew[ixyz];  break;
		  
		case MRI_float:
		  float_old = fold[ixyz];  
		  float_new = fnew[ixyz];  break;
		}

	      diff = float_old - float_base;
	      old_sse += diff*diff;
	      diff = float_new - float_base;
	      new_sse += diff*diff;
	    }
      
      old_rmse = sqrt (old_sse / nxyz);
      new_rmse = sqrt (new_sse / nxyz);

      if (option_data->debug)
	printf ("Volume = %d   OLD RMSE = %f   NEW RMSE = %f \n", 
		ivolume, old_rmse, new_rmse);
      
      old_rms_array[ivolume] = old_rmse;
      new_rms_array[ivolume] = new_rmse;

    }

}


/*---------------------------------------------------------------------------*/
/*
  Main routine for this program.
  If the return string is not NULL, some error transpired, and
  the program will display the error message.
*/

#define FREEUP(x) do{if((x) != NULL){free((x)); (x)=NULL;}}while(0)

#define FREE_WORKSPACE                             \
  do{ FREEUP(bptr) ; FREEUP(sptr) ; FREEUP(fptr) ; \
      FREEUP(bout) ; FREEUP(sout) ; FREEUP(fout) ; \
      FREEUP(dxar) ; FREEUP(dyar) ; FREEUP(phiar); \
    } while(0) ;


char * IMREG_main 
(
  IR_options * opt,
  vector * state_history,
  float * old_rms_array,
  float * new_rms_array
)
{
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   THD_3dim_dataset * base_dset;               /* base image dataset */
   char * new_prefix ;                         /* string from user */
   int base , ntime , datum , nx,ny,nz , ii,kk , npix ;
   float                      dx,dy,dz ;
   int base_datum, failed;
   MRI_IMARR * ims_in , * ims_out ;
   MRI_IMAGE * im , * imbase ;

   byte   ** bptr = NULL , * bbase = NULL, ** bout = NULL ;
   short  ** sptr = NULL , * sbase = NULL, ** sout = NULL ; 
   float  ** fptr = NULL , * fbase = NULL, ** fout = NULL ;

   float * dxar = NULL , * dyar = NULL , * phiar = NULL ;

   int it;

   /*--------------------------------------------------------------------*/
   /*----- Check batch command inputs to see if they are reasonable -----*/

   old_dset = THD_open_one_dataset(opt->input_filename) ;   
                                                      /* get ptr to dataset */
   if( old_dset == NULL )
      return "*************************\n"
             "Cannot find Input Dataset\n"
             "*************************"  ;

   ntime = DSET_NUM_TIMES(old_dset) ;
   if( ntime < 2 && !opt->base_filename)
      return "*****************************\n"
             "Dataset has only 1 time point\n"
             " and no -basefile specified.\n"
             "*****************************"  ;

   ii = DSET_NVALS_PER_TIME(old_dset) ;
   if( ii > 1 )
      return "************************************\n"
             "Dataset has > 1 value per time point\n"
             "************************************"  ;

   nx = old_dset->daxes->nxx ; dx = old_dset->daxes->xxdel ;
   ny = old_dset->daxes->nyy ; dy = old_dset->daxes->yydel ; npix = nx*ny ;
   nz = old_dset->daxes->nzz ; dz = old_dset->daxes->zzdel ;

   if( nx != ny || fabs(dx) != fabs(dy) ){

     /*     No need to quit, works fine.  ZSS 07
      *     Only if nx >= ny (so fix might be easy).  12 Jan 2010 [rickr] */
     if (opt->debug)
     fprintf(stderr,"\nNotice 2dImreg: nx=%d ny=%d nz=%d  dx=%f dy=%f dz=%f\n",
	       nx,ny,nz,dx,dy,dz ) ;

      return "***********************************\n"
             "Dataset does not have square slices\n"
             "***********************************"  ;
   }

   new_prefix = opt->new_prefix;     /* get string item (the output prefix) */
   if (new_prefix == NULL)           /* check if it is OK */
      return "************************\n"
             "Output Prefix is illegal\n"
             "************************"  ;

   /*----- Check whether output file already exists -----*/
   check_output_file (old_dset, new_prefix);


   /*--------- go to "base" input option ---------*/

   if (opt->base_filename == NULL)
     base_dset = old_dset;
   else
     {
       base_dset = THD_open_one_dataset(opt->base_filename) ;   
                                                  /* get ptr to base dataset */
       if( base_dset == NULL )
	 return "************************\n"
	        "Cannot find Base Dataset\n"
	        "************************"  ;

       if ( (nx != base_dset->daxes->nxx) || (dx != base_dset->daxes->xxdel)
	 || (ny != base_dset->daxes->nyy) || (dy != base_dset->daxes->yydel)
	 || (nz != base_dset->daxes->nzz) || (dz != base_dset->daxes->zzdel) )
	 {
	   if (opt->debug)
	       {
		 fprintf(stderr,"\nIMREG: Input Dataset:\n");
		 fprintf(stderr,"nx=%d ny=%d nz=%d  dx=%f dy=%f dz=%f\n",
		     nx,ny,nz,dx,dy,dz ) ;

		 fprintf(stderr,"\nIMREG: Base Dataset:\n");
		 fprintf(stderr,"nx=%d ny=%d nz=%d  dx=%f dy=%f dz=%f\n",
			 base_dset->daxes->nxx,   base_dset->daxes->nyy,
			 base_dset->daxes->nzz,   base_dset->daxes->xxdel,
			 base_dset->daxes->yydel, base_dset->daxes->zzdel) ;
	       }
	   return "*************************************************\n"
	          "Base Dataset is not compatible with Input Dataset\n"
	          "*************************************************"  ;
	 }
     }

   base_datum = DSET_BRICK_TYPE(base_dset,0);

   base = opt->base_vol_index;
   if (base < 0) { /* default */
      if (-base >= DSET_NUM_TIMES(base_dset)) {
         base = DSET_NUM_TIMES(base_dset)-1;
      } else {
         base = -base;
      }
   }
   
   if( base >= DSET_NUM_TIMES(base_dset) )
      return "******************************\n"
             "Base image number is too large\n"
             "******************************"  ;


   /*--------- see if the "fine" input option is present --------*/
   if (opt->nofine)
     mri_align_params( 0 , 0.0,0.0,0.0 , 0.0,0.0,0.0 ) ;
   else{
      float fsig , fdxy , fdph ;
      fsig = opt->blur * 0.42466090;
      fdxy = opt->dxy;
      fdph = opt->dphi;
      mri_align_params( 0 , 0.0,0.0,0.0 , fsig,fdxy,fdph ) ;

      if (opt->debug)
	fprintf(stderr,"Set fine params = %f %f %f\n",fsig,fdxy,fdph) ; 
   }

   /*------------- ready to compute new dataset -----------*/

   if (opt->debug)
     fprintf(stderr,"IMREG: loading dataset\n") ;


   DSET_load( old_dset ) ;
   
   if (opt->base_filename != NULL)
     DSET_load (base_dset);

   /*** 1) Copy the dataset in toto ***/

   if (opt->debug)
     fprintf(stderr,"IMREG: Copying dataset\n") ;


   new_dset = copy_dset( old_dset , new_prefix ) ;
   if( new_dset == NULL )
      return "****************************\n"
             "Failed to copy input dataset\n"
             "****************************"  ;

   /*** 2) Make an array of empty images ***/

   if (opt->debug)
     fprintf(stderr,"IMREG: making empty images\n") ;


   datum = DSET_BRICK_TYPE(new_dset,0) ;

   INIT_IMARR(ims_in) ;
   for( ii=0 ; ii < ntime ; ii++ ){
      im = mri_new_vol_empty( nx , ny , 1 , datum ) ;
      ADDTO_IMARR(ims_in,im) ;
   }

   imbase = mri_new_vol_empty( nx , ny , 1 , base_datum ) ;

   dxar  = (float *) malloc( sizeof(float) * ntime ) ;
   dyar  = (float *) malloc( sizeof(float) * ntime ) ;
   phiar = (float *) malloc( sizeof(float) * ntime ) ;

   /*** 3) Get pointers to sub-bricks in old, base, and new datasets ***/

   if (opt->debug)
     fprintf(stderr,"IMREG: getting input brick pointers\n") ;


   switch( datum ){  /* pointer type depends on input datum type */
      case MRI_byte:
         bptr  = (byte **) malloc( sizeof(byte *) * ntime ) ;
         bout  = (byte **) malloc( sizeof(byte *) * ntime ) ;
         for( ii=0 ; ii < ntime ; ii++ ){
            bptr[ii]  = (byte *) DSET_ARRAY(old_dset,ii) ;
            bout[ii]  = (byte *) DSET_ARRAY(new_dset,ii) ;
         }
      break ;

      case MRI_short:
         sptr  = (short **) malloc( sizeof(short *) * ntime ) ;
         sout  = (short **) malloc( sizeof(short *) * ntime ) ;
         for( ii=0 ; ii < ntime ; ii++ ){
            sptr[ii]  = (short *) DSET_ARRAY(old_dset,ii) ;
            sout[ii]  = (short *) DSET_ARRAY(new_dset,ii) ;
         }

	 if (opt->debug)
	   fprintf(stderr,"IMREG: sptr[0] = %p  sout[0] = %p\n",
		   sptr[0],sout[0]) ;

      break ;

      case MRI_float:
         fptr  = (float **) malloc( sizeof(float *) * ntime ) ;
         fout  = (float **) malloc( sizeof(float *) * ntime ) ;
         for( ii=0 ; ii < ntime ; ii++ ){
            fptr[ii]  = (float *) DSET_ARRAY(old_dset,ii) ;
            fout[ii]  = (float *) DSET_ARRAY(new_dset,ii) ;
         }
      break ;
   }

   switch( base_datum ){  /* pointer type depends on base datum type */
      case MRI_byte:
	bbase = (byte *) DSET_ARRAY(base_dset,base) ; 
      break ;

      case MRI_short:
	sbase = (short *) DSET_ARRAY(base_dset,base) ;
	if (opt->debug)
	  fprintf(stderr,"IMREG: sbase[%d] = %p \n", base, sbase) ;
      break ;

      case MRI_float:
	fbase = (float *) DSET_ARRAY(base_dset,base) ;
      break ;
   }

   /*** 4) Loop over slices ***/

   for( kk=0 ; kk < nz ; kk++ ){

      /*** 4a) Setup ims_in images to point to input slices ***/

     if (opt->debug)
       fprintf(stderr,"IMREG: slice %d -- setup input images\n",kk) ;


      for( ii=0 ; ii < ntime ; ii++ ){
         im = IMARR_SUBIMAGE(ims_in,ii) ;
         switch( datum ){
            case MRI_byte:  
	      mri_fix_data_pointer( bptr[ii] + kk*npix, im ) ; break ;
            case MRI_short: 
	      mri_fix_data_pointer( sptr[ii] + kk*npix, im ) ; break ;
            case MRI_float: 
	      mri_fix_data_pointer( fptr[ii] + kk*npix, im ) ; break ;
         }
      }

      /*** 4b) Setup imbase to point to base image ***/

      if (opt->debug)
	fprintf(stderr,"IMREG: slice %d -- setup base image\n",kk) ;


      switch( base_datum ){
         case MRI_byte:  
	   mri_fix_data_pointer( bbase + kk*npix, imbase ) ; break ;
         case MRI_short: 
	   mri_fix_data_pointer( sbase + kk*npix, imbase ) ; break ;
         case MRI_float: 
	   mri_fix_data_pointer( fbase + kk*npix, imbase ) ; break ;
      }

      /*** 4c) Register this slice at all times ***/

      if (opt->debug)
	fprintf(stderr,"IMREG: slice %d -- register\n",kk) ;


      ims_out = mri_align_dfspace( imbase , NULL , ims_in ,
                                   ALIGN_REGISTER_CODE , dxar,dyar,phiar ) ;

      /* hack of a fix for failure case       08 Apr 2008 [rickr/dglen] */
      failed = 0;
      if( ims_out == NULL ) {
	fprintf(stderr,"IMREG failure (zero slice?), copying input\n");
        failed = 1;
        ims_out = ims_in;
      }

      
      /*----- Store the registration parameters -----*/
      if (opt->dprefix != NULL)
	for (ii = 0;  ii < ntime;  ii++)
	  {
	    it = ii*nz + z_to_t[kk];
	    if (opt->dmm)
	      {
		state_history[it].elts[1] = dxar[ii] * dx;
		state_history[it].elts[2] = dyar[ii] * dy;
	      }
	    else
	      {
		state_history[it].elts[1] = dxar[ii];
		state_history[it].elts[2] = dyar[ii];
	      }

	    state_history[it].elts[3] = (180.0/PI)*phiar[ii];
	  }
      

      /*** 4d) Put the output back in on top of the input;
	       note that the output is always in MRI_float format ***/

      if (opt->debug)
	fprintf(stderr,"IMREG: slice %d -- put output back into dataset\n",kk);


      for( ii=0 ; ii < ntime ; ii++ ){
         switch( datum ){
           case MRI_byte:
              im = mri_to_mri( MRI_byte , IMARR_SUBIMAGE(ims_out,ii) ) ;
              memcpy( bout[ii] + kk*npix , MRI_BYTE_PTR(im) , 
		      sizeof(byte)*npix ) ;
              mri_free(im) ;
           break ;

           case MRI_short:

	     if (opt->debug)
	       if( ii==0 )
		 fprintf(stderr,"IMREG: conversion to short at ii=%d\n",ii) ;

              im = mri_to_mri( MRI_short , IMARR_SUBIMAGE(ims_out,ii) ) ;

	      if (opt->debug)
		if( ii==0 )
		  fprintf(stderr,"IMREG: copying to %p from %p\n",
			  sout[ii] + kk*npix,MRI_SHORT_PTR(im)) ;


              memcpy( sout[ii] + kk*npix , MRI_SHORT_PTR(im) , 
		      sizeof(short)*npix ) ;

	      if (opt->debug)
		if( ii==0 )
		  fprintf(stderr,"IMREG: freeing\n") ;


              mri_free(im) ;
           break ;

           case MRI_float:
              im = IMARR_SUBIMAGE(ims_out,ii) ;
              memcpy( fout[ii] + kk*npix , MRI_FLOAT_PTR(im) , 
		      sizeof(float)*npix ) ;
           break ;
         }
      }

      /*** 4e) Destroy the output images ***/

      if (opt->debug)
	fprintf(stderr,"IMREG: destroying aligned output\n") ;


      if( !failed ) DESTROY_IMARR( ims_out ) ;  /* 08 Apr 2008 */
   }

   /*** 5) Destroy the empty images and other workspaces ***/

   if (opt->debug)
     fprintf(stderr,"IMREG: destroy workspaces\n") ;


   mri_clear_data_pointer(imbase) ; mri_free(imbase) ;
   for( ii=0 ; ii < ntime ; ii++ ){
      im = IMARR_SUBIMAGE(ims_in,ii) ;
      mri_clear_data_pointer(im) ;
   }
   DESTROY_IMARR(ims_in) ;
   FREE_WORKSPACE ;

   /*------------- write out the new dataset ------------*/

   if (opt->debug)
     fprintf(stderr,"IMREG: write new dataset to disk\n") ;


  /*----- Record history of dataset -----*/
   tross_Copy_History( old_dset , new_dset ) ;
   if( commandline != NULL )
     tross_Append_History( new_dset , commandline ) ;


  THD_load_statistics( new_dset ) ;
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;
  

  /*----- evaluate results -----*/
  if (opt->rprefix != NULL)
    eval_registration (opt, old_dset, new_dset, base_dset, base, 
		       old_rms_array, new_rms_array);


  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( old_dset , False ) ; old_dset = NULL ;
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  if (opt->base_filename != NULL)
    THD_delete_3dim_dataset( base_dset , False ) ; base_dset = NULL ;
    


   return NULL ;  /* null string returned means all was OK */
}


/*---------------------------------------------------------------------------*/
/*
  Routine to write RMS error arrays. 
*/


void output_rms_arrays
(
  IR_options * option_data,     /* user input options */
  THD_3dim_dataset * dset,      /* pointer to input 3d+time dataset */
  float * old_rms_array,        /* volume RMS error for input dataset */
  float * new_rms_array         /* volume RMS error for registered dataset */
)

{
  int it;                                  /* time index */
  int ts_length;                           /* length of time series data */  
  char filename[MAX_NAME_LENGTH];          /* name of output file */
  FILE * old_rms_file , * new_rms_file;    /* file for volume RMS error */


  /*----- Initialize local variables -----*/
  ts_length = DSET_NUM_TIMES(dset);


  /*----- Open output files -----*/
  strcpy (filename, option_data->rprefix);
  strcat (filename, ".oldrms");
  old_rms_file = fopen (filename, "w");
  strcpy (filename, option_data->rprefix);
  strcat (filename, ".newrms");
  new_rms_file = fopen (filename, "w");


  /*----- Write volume RMS error data -----*/
  for (it = 0;  it < ts_length;  it++)
    {
      fprintf (old_rms_file, "%d  %f\n" , it, old_rms_array[it]);
      fprintf (new_rms_file, "%d  %f\n" , it, new_rms_array[it]);
    }


  /*----- Close output files -----*/
  fclose (old_rms_file);
  fclose (new_rms_file);

}


/*---------------------------------------------------------------------------*/
/*
  Get the time corresponding to this particular slice.
*/
 
float get_time  (int ivolume, int iz,  THD_3dim_dataset * dset)

{
  float time;
  float z;


  z = iz * dset->taxis->dz_sl + dset->taxis->zorg_sl;
  time = THD_timeof (ivolume, z, dset->taxis);
  
  if (dset->taxis->units_type == UNITS_MSEC_TYPE)
    time /= 1000.0;

  return (time);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to write the registration parameter time series. 
*/


void output_state_history
(
  IR_options * option_data,     /* user input options */
  THD_3dim_dataset * dset,
  vector * state_history
)

{
  int iv;                           /* vector index */
  int ts_length;                    /* length of time series */
  int num_slices;                   /* number of slices in each volume */
  int num_vectors;                  /* total number of state vectors */
  int ivolume;                      /* volume index */
  int iz;                           /* z slice index */
  float t;                          /* time for current vector */
  char filename[MAX_NAME_LENGTH];   /* string for output file name */

  FILE * dx_file, * dy_file, * psi_file;   /* output files */


  /*----- Initialize local variables -----*/
  num_slices = dset->taxis->nsl;
  ts_length = DSET_NUM_TIMES(dset);

  if ( num_slices <= 0 )            /* 06 Oct 2003 [rickr] */
      num_slices = dset->daxes->nzz;


  /*----- Calculate total number of state vectors -----*/
  num_vectors = ts_length * num_slices;


  /*----- Open output files -----*/
  strcpy (filename, option_data->dprefix);
  strcat (filename, ".dx");
  dx_file = fopen (filename, "w");

  strcpy (filename, option_data->dprefix);
  strcat (filename, ".dy");
  dy_file = fopen (filename, "w");

  strcpy (filename, option_data->dprefix);
  strcat (filename, ".psi");
  psi_file = fopen (filename, "w");

  
  /*----- Write the registration parameters -----*/
  if (dset->taxis) {
     for (iv = 0;  iv < num_vectors;  iv++)
       {
         ivolume = iv / num_slices;
         iz = t_to_z[iv % num_slices];
         t = get_time (ivolume, iz, dset);
         fprintf (dx_file,  "%f   %f\n", t, state_history[iv].elts[1]);
         fprintf (dy_file,  "%f   %f\n", t, state_history[iv].elts[2]);
         fprintf (psi_file, "%f   %f\n", t, state_history[iv].elts[3]);
       }
  } else {
      for (iv = 0;  iv < num_vectors;  iv++)
       {
         fprintf (dx_file,  "%d   %f\n", iv, state_history[iv].elts[1]);
         fprintf (dy_file,  "%d   %f\n", iv, state_history[iv].elts[2]);
         fprintf (psi_file, "%d   %f\n", iv, state_history[iv].elts[3]);
       } 
  }


  /*----- Close output files -----*/
  fclose (dx_file);
  fclose (dy_file);
  fclose (psi_file);
}


/*---------------------------------------------------------------------------*/
/*
  Output results.
*/

void output_results  
(
  IR_options * option_data,     /* user input options */
  vector * state_history,       /* time series of state vectors */
  float * old_rms_array,        /* volume RMS error for input dataset */
  float * new_rms_array         /* volume RMS error for registered dataset */
)

{
  THD_3dim_dataset * dset;

  read_dataset (option_data->input_filename, &dset);

  /*----- Write the time series of state parameters -----*/
  if (option_data->dprefix != NULL)
    output_state_history (option_data, dset, state_history);


  /*----- Write user specified auxiliary time series data -----*/
  if (option_data->rprefix != NULL)
    output_rms_arrays (option_data, dset, old_rms_array, new_rms_array);


  THD_delete_3dim_dataset (dset, False);   dset = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Terminate the program.
*/

void terminate_program  
(
  IR_options ** option_data,
  vector ** state_history,       /* time series of state vectors */
  float ** old_rms_array,        /* volume RMS error for input dataset */
  float ** new_rms_array         /* volume RMS error for registered dataset */
)

{
  THD_3dim_dataset * dset = NULL;   /* pointer to input 3d+time dataset */
  int num_slices;                   /* number of slices in each volume */
  int ts_length;                    /* length of time series */
  int num_vectors;                  /* total number of state vectors */
  int i;                            /* index */


  /*----- Initialize local variables -----*/
  read_dataset ((*option_data)->input_filename, &dset);
  if (dset->taxis) num_slices = dset->taxis->nsl;
   else num_slices = -1;
   
  if ( num_slices <= 0 )            /* 06 Oct 2003 [rickr] */
      num_slices = dset->daxes->nzz;

  ts_length = DSET_NUM_TIMES(dset);
  num_vectors = ts_length * num_slices;
  THD_delete_3dim_dataset (dset, False);   dset = NULL;


  /*----- Release memory -----*/
  free (*option_data);     *option_data = NULL;
  if (t_to_z) free (t_to_z);           t_to_z = NULL;
  if (z_to_t) free (z_to_t);           z_to_t = NULL;

  
  if (*old_rms_array != NULL)
    { free (*old_rms_array);   *old_rms_array = NULL; }
  if (*new_rms_array != NULL)
    { free (*new_rms_array);   *new_rms_array = NULL; }


  /*----- Deallocate memory for array of state vectors -----*/
  if (*state_history != NULL)
    {
      for (i = 0;  i < num_vectors;  i++)
	vector_destroy (&((*state_history)[i]));
      free (*state_history);   *state_history = NULL;
    }

}


/*---------------------------------------------------------------------------*/

int main
(
  int argc,                    /* number of input arguments */
  char ** argv                 /* array of input arguments */ 
)

{
  IR_options * option_data = NULL;     /* user input options */
  char * chptr;                        /* error message from processing */
  vector * state_history = NULL;       /* time series of state vectors */
  float * old_rms_array = NULL;        /* original data volume RMS error */
  float * new_rms_array = NULL;        /* registered data volume RMS error */


  
#if 0 
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");
#endif

  machdep() ; mainENTRY("2dImReg main") ; PRINT_VERSION("2dImReg") ;

  /*----- Program initialization -----*/
  initialize_program (argc, argv, &option_data, &state_history, 
		      &old_rms_array, &new_rms_array);


  /*----- Register all slices in the dataset -----*/
  chptr = IMREG_main (option_data, state_history,
		      old_rms_array, new_rms_array);


  /*----- Check for processing errors -----*/
  if (chptr != NULL)   
    {
      printf ("%s \n\n", chptr);
      exit(1);
    }


  /*----- Output the results -----*/
  output_results (option_data, state_history, 
		  old_rms_array, new_rms_array);


  /*----- Terminate the program -----*/
  terminate_program (&option_data, &state_history,
		     &old_rms_array, &new_rms_array);
 
  return 0;
}

