/*---------------------------------------------------------------------------*/
/*
  Program to automatically segment the intracranial region.

  File:    3dIntracranial.c
  Author:  B. Douglas Ward
  Date:    04 June 1999

  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.

*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dIntracranial"                /* name of this program */
#define PROGRAM_AUTHOR "B. D. Ward"                        /* program author */
#define PROGRAM_DATE "04 June 1999"              /* date of last program mod */

/*---------------------------------------------------------------------------*/
/*
  Include header files.
*/


#include "mrilib.h"
#include "Intracranial.h"


/*---------------------------------------------------------------------------*/
/*
  Global variables and constants.
*/

static char * anat_filename = NULL;      /* file name for input anat dataset */
static char * prefix_filename = NULL;    /* prefix name for output dataset */
static Boolean write_mask = FALSE;       /* flag for generate 'fim' mask */
static Boolean quiet = FALSE;            /* flag for suppress screen output */


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void SI_error (char * message)
{
  fprintf (stderr, "\n");
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/
/*
  Include source code.
*/

#include "estpdf.c"                    /* code for PDF estimation */
#include "Intracranial.c"              /* code for intracranial segmentation */


/*---------------------------------------------------------------------------*/
/*
   Routine to display 3dIntracranial help menu.
*/

void display_help_menu()
{
  printf 
    (
     "This program performs automatic segmentation of intracranial region.\n\n"
     "Usage: \n"
     "3dIntracranial \n"
     "-anat filename    Filename of anat dataset to be segmented            \n"
     "                                                                      \n"
     "[-min_val   a]    Minimum voxel intensity limit                       \n"
     "                     Default: Internal PDF estimate for lower bound   \n"
     "[-max_val   b]    Maximum voxel intensity limit                       \n"
     "                     Default: Internal PDF estimate for upper bound   \n"
     "[-min_conn  m]    Minimum voxel connectivity to enter                 \n"
     "                     Default: m=4                                     \n"
     "[-max_conn  n]    Maximum voxel connectivity to leave                 \n"
     "                     Default: n=2                                     \n"
     "[-mask]           Generate functional image mask (complement)         \n"
     "                     Default: Generate anatomical image               \n"
     "[-quiet]          Suppress output to screen                           \n"
     "                                                                      \n"
     "-prefix pname     Prefix name for file to contain segmented image     \n"
      );
  
  exit(0);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to get user specified input options.
*/

void get_options
(
  int argc,                        /* number of input arguments */
  char ** argv                     /* array of input arguments */ 
)

{
  int nopt = 1;                     /* input option argument counter */
  int ival, index;                  /* integer input */
  float fval;                       /* float input */
  char message[MAX_STRING_LENGTH];  /* error message */


  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();  
   

  /*----- main loop over input options -----*/
  while (nopt < argc )
    {

      /*-----   -anat filename   -----*/
      if (strncmp(argv[nopt], "-anat", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  SI_error ("need argument after -anat ");
	  anat_filename = malloc (sizeof(char) * MAX_STRING_LENGTH);
	  MTEST (anat_filename);
	  strcpy (anat_filename, argv[nopt]);

	  anat = THD_open_one_dataset (anat_filename);
	  if (!ISVALID_3DIM_DATASET (anat))
	    {
	      sprintf (message, "Can't open dataset: %s\n", anat_filename); 
	      SI_error (message); 
	    } 
	  THD_load_datablock (anat->dblk, NULL); 
	  if (DSET_ARRAY(anat,0) == NULL)
	    {
	      sprintf (message, "Can't access data: %s\n", anat_filename); 
	      SI_error (message); 
	    }

	  nopt++;
	  continue;
	}
      

      /*-----   -min_val a  -----*/
      if (strncmp(argv[nopt], "-min_val", 8) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  SI_error ("need argument after -min_val ");
	  sscanf (argv[nopt], "%f", &fval); 
	  if (fval < 0.0)
	    SI_error ("illegal argument after -min_val ");
	  min_val_float = fval;
	  min_val_int = 0;
	  nopt++;
	  continue;
	}
      

      /*-----   -max_val b  -----*/
      if (strncmp(argv[nopt], "-max_val", 8) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  SI_error ("need argument after -max_val ");
	  sscanf (argv[nopt], "%f", &fval); 
	  if (fval < 0.0)
	    SI_error ("illegal argument after -max_val ");
	  max_val_float = fval;
	  max_val_int = 0;
	  nopt++;
	  continue;
	}


      /*-----   -min_conn m  -----*/
      if (strncmp(argv[nopt], "-min_conn", 9) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  SI_error ("need argument after -min_conn ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < 1) || (ival > 7))
	    SI_error ("illegal argument after -min_conn ");
	  min_conn_int = ival;
	  nopt++;
	  continue;
	}


      /*-----   -max_conn n  -----*/
      if (strncmp(argv[nopt], "-max_conn", 9) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  SI_error ("need argument after -max_conn ");
	  sscanf (argv[nopt], "%d", &ival);
	  if ((ival < -1) || (ival > 5))
	    SI_error ("illegal argument after -max_conn ");
	  max_conn_int = ival;
	  nopt++;
	  continue;
	}


      /*-----   -mask  -----*/
      if (strncmp(argv[nopt], "-mask", 5) == 0)
	{
	  write_mask = TRUE;
	  nopt++;
	  continue;
	}


      /*-----   -quiet  -----*/
      if (strncmp(argv[nopt], "-quiet", 6) == 0)
	{
	  quiet = TRUE;
	  nopt++;
	  continue;
	}


      /*-----   -prefix prefixname   -----*/
      if (strncmp(argv[nopt], "-prefix", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  SI_error ("need argument after -prefix ");
	  prefix_filename = malloc (sizeof(char) * MAX_STRING_LENGTH);
	  MTEST (prefix_filename);
	  strcpy (prefix_filename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*----- unknown command -----*/
      sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
      SI_error (message);
      
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
  new_dset = EDIT_empty_copy ( anat );
  
  
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
      SI_error (message);
    }
  
  if( THD_is_file(new_dset->dblk->diskptr->header_name) )
    {
      sprintf (message,
	       "Output dataset file %s already exists"
	       "--cannot continue!\a\n",
	       new_dset->dblk->diskptr->header_name);
      SI_error (message);
    }
  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}


/*---------------------------------------------------------------------------*/
/*
  Program initialization.
*/

void initialize_program 
(
  int argc,                        /* number of input arguments */
  char ** argv                     /* array of input arguments */ 
)

{
  float parameters [DIMENSION];    /* parameters for PDF estimation */
  Boolean ok;                      /* flag for successful PDF estimation */


  /*----- Get operator inputs -----*/
  get_options (argc, argv);


  /*----- Verify that inputs are acceptable -----*/
  verify_inputs();


  /*----- Get PDF estimate and set voxel intensity limits -----*/
  if (min_val_int || max_val_int)  ok = estpdf (parameters);
  if (!ok)  SI_error ("Unable to perform PDF estimation ");
  if (min_val_int)  min_val_float = parameters[4] - 2.0*parameters[5];
  if (max_val_int)  max_val_float = parameters[7] + 2.0*parameters[8];
  
   
  if (! quiet)
    {
      printf ("\n");
      printf ("Control inputs: \n");
      printf ("anat filename = %s \n", anat_filename);
      printf ("min value = %f \n", min_val_float);
      printf ("max value = %f \n", max_val_float);
      printf ("min conn  = %d \n", min_conn_int);
      printf ("max conn  = %d \n", max_conn_int);
      printf ("prefix filename = %s \n", prefix_filename);
    }


}


/*---------------------------------------------------------------------------*/
/*
  Perform initialization for automatic segmentation algorithm.
*/

int auto_initialize (short ** cv)

{
  int nx, ny, nz, nxy, nxyz, ixyz;       /* voxel counters */
  int ok;                                /* Boolean for satisfactory */


  /*----- Initialize local variables -----*/
  nx = DSET_NX(anat);   ny = DSET_NY(anat);   nz = DSET_NZ(anat);
  nxy = nx*ny;   nxyz = nxy*nz;


  /*----- Initialize voxel classification indicators -----*/
  *cv = (short *) malloc (nxyz * sizeof(short));
  MTEST (*cv);
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    (*cv)[ixyz] = 0;


  /*----- Initialize random number generator -----*/
  srand48 (1234567);


  return (1);
}


/*---------------------------------------------------------------------------*/
/*
  Put estimated target structure into output dataset.
*/

void target_into_dataset 
(
  short * cv            /* volume with 1's at target voxel locations */
)

{
  short * anat_data  = NULL;               /* data from anatomical image */
  int nx, ny, nz, nxy, nxyz, ixyz;         /* voxel counters */
 

  /*----- Initialize local variables -----*/
  anat_data  = (short *) DSET_BRICK_ARRAY(anat,0) ;
  nx = DSET_NX(anat);   ny = DSET_NY(anat);   nz = DSET_NZ(anat);
  nxy = nx*ny;   nxyz = nxy*nz;

  if (! write_mask)
    {
      /*----- Reset to zero those voxels which lie outside the brain -----*/
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  if (cv[ixyz])  cv[ixyz] = 0;
	  else           cv[ixyz] = anat_data[ixyz];
	}
    }

  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset (anat, False);   anat = NULL;


  return;
}


/*---------------------------------------------------------------------------*/
/*
  Routine to write one AFNI dataset.
  
  The output is either a segmented anatomical dataset, 
  or a mask 'fim' type dataset.
  
*/

void write_afni_data 
(
  short * cv            /* volume with 1's at target voxel locations */
)

{
  int nxyz;                           /* number of voxels */
  int ii;                             /* voxel index */
  THD_3dim_dataset * dset=NULL;       /* input afni data set pointer */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int ierror;                         /* number of errors in editing data */
  int ibuf[32];                       /* integer buffer */
  float fbuf[MAX_STAT_AUX];           /* float buffer */
  float fimfac;                       /* scale factor for short data */
  int output_datum;                   /* data type for output data */
  char * filename;                    /* prefix filename for output */


  /*----- initialize local variables -----*/
  filename = prefix_filename;
  dset = THD_open_one_dataset (anat_filename);
  nxyz = DSET_NX(dset) * DSET_NY(dset) * DSET_NZ(dset);

  
  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy( dset ) ;


  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset (dset, False);   dset = NULL ;
  

  output_datum = MRI_short ;
  
  ibuf[0] = output_datum ;
  
  if (write_mask)
    {
      int func_type = FUNC_FIM_TYPE;
      ierror = EDIT_dset_items( new_dset ,
				ADN_prefix , filename ,
				ADN_label1 , filename ,
				ADN_self_name , filename ,
				ADN_type , ISHEAD(dset) ? HEAD_FUNC_TYPE : 
				GEN_FUNC_TYPE ,
				ADN_func_type , func_type ,
				ADN_nvals , FUNC_nvals[func_type] ,
				ADN_datum_array , ibuf ,
				ADN_malloc_type, DATABLOCK_MEM_MALLOC ,  
				ADN_none ) ;
    }
  else
    {
      ierror = EDIT_dset_items( new_dset ,
				ADN_prefix , filename ,
				ADN_label1 , filename ,
				ADN_self_name , filename ,
				ADN_datum_array , ibuf ,
				ADN_malloc_type, DATABLOCK_MEM_MALLOC ,  
				ADN_none ) ;
    }

     
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
  mri_fix_data_pointer (cv, DSET_BRICK(new_dset,0)); 
  fimfac = 1.0;
  

  /*----- write afni data set -----*/
  if (! quiet)
    {
      if (write_mask)
	printf("\nWriting functional (mask) dataset: ");
      else
	printf ("\nWriting anatomical dataset: ");
      printf("%s\n", new_dset->dblk->diskptr->header_name) ;
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
  Perform automatic image segmentation.
*/

void SEGMENT_auto ()
{
  short * cv = NULL;           /* volume with 1's at target voxel locations */
  int ok;                      /* Boolean for successful operation */


  /*----- Perform initialization for automatic segmentation algorithm -----*/
  ok = auto_initialize (&cv);
  if (! ok)  return;


  /*----- Segment intracranial voxels -----*/
  segment_volume (cv);


  /*----- Perform voxel connectivity tests -----*/
  connectivity_tests (cv);


  /*----- Put estimated target structure into output dataset -----*/
  target_into_dataset (cv);


  /*----- Write out the segmented dataset -----*/
  write_afni_data (cv);


 return ;

}


/*---------------------------------------------------------------------------*/
/*
  This is the main routine for program 3dIntracranial.
*/

void main
(
  int argc,                /* number of input arguments */
  char ** argv             /* array of input arguments */ 
)

{

  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program: %s \n", PROGRAM_NAME);
  printf ("Author:  %s \n", PROGRAM_AUTHOR);
  printf ("Date:    %s \n", PROGRAM_DATE);
  printf ("\n");

  
  /*----- Program initialization -----*/
  initialize_program (argc, argv);

  
  /*----- Perform automatic segmentation -----*/
  SEGMENT_auto();

}

/*---------------------------------------------------------------------------*/






