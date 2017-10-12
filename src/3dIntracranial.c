/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1999-2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  Program to automatically segment the intracranial region.

  File:    3dIntracranial.c
  Author:  B. Douglas Ward
  Date:    04 June 1999


  Mod:     Corrected initialization of PDF estimation flag.
  Date:    06 August 1999

  Mod:     Added changes for incorporating History notes.
  Date:    09 September 1999

  Mod:     Interface changes for new estpdf.c
  Date:    28 January 2000

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

  Mod:     Added option to suppress spatial smoothing of segmentation mask.
  Date:    03 December 2001

  Mod:     Set MAX_STRING_LENGTH equal to THD_MAX_NAME.
  Date:    02 December 2002

  Mod:     Convert input from MRI_byte to MRI_short if needed.
  Date:    05 December 2002

  Mod:     Modified the -help menu -- P Christidis
  Date:    21 July 2005
*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "3dIntracranial"                /* name of this program */
#define PROGRAM_AUTHOR "B. D. Ward"                        /* program author */
#define PROGRAM_INITIAL "04 June 1999"    /* date of initial program release */
#define PROGRAM_LATEST "21 July 2005"     /* date of latest program revision */

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

#define USE_QUIET

static char * anat_filename = NULL;      /* file name for input anat dataset */
static char * prefix_filename = NULL;    /* prefix name for output dataset */
static Boolean write_mask = FALSE;       /* flag for generate 'fim' mask */
static Boolean quiet = FALSE;            /* flag for suppress screen output */
static char * commandline = NULL ;       /* command line for history notes */
static Boolean nosmooth = FALSE;         /* flag to delete spatial smoothing */


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

#include "estpdf3.c"                    /* code for PDF estimation */
#include "Intracranial.c"              /* code for intracranial segmentation */


/*---------------------------------------------------------------------------*/
/*
   Routine to display 3dIntracranial help menu.
*/

void display_help_menu()
{
  printf
    (
   "3dIntracranial - performs automatic segmentation of intracranial region.\n"
   "                                                                        \n"
   "   This program will strip the scalp and other non-brain tissue from a  \n"
   "   high-resolution T1 weighted anatomical dataset.                      \n"
   "                                                                        \n"
   "** Nota Bene: the newer program 3dSkullStrip should also be considered  \n"
   "**            for this functionality -- it usually works better.        \n"
   "                                                                        \n"
   "----------------------------------------------------------------------- \n"
   "                                                                        \n"
   "Usage:                                                                  \n"
   "-----                                                                   \n"
   "                                                                        \n"
   "3dIntracranial                                                          \n"
   "   -anat filename   => Filename of anat dataset to be segmented         \n"
   "                                                                        \n"
   "   [-min_val   a]   => Minimum voxel intensity limit                    \n"
   "                         Default: Internal PDF estimate for lower bound \n"
   "                                                                        \n"
   "   [-max_val   b]   => Maximum voxel intensity limit                    \n"
   "                         Default: Internal PDF estimate for upper bound \n"
   "                                                                        \n"
   "   [-min_conn  m]   => Minimum voxel connectivity to enter              \n"
   "                         Default: m=4                                   \n"
   "                                                                        \n"
   "   [-max_conn  n]   => Maximum voxel connectivity to leave              \n"
   "                         Default: n=2                                   \n"
   "                                                                        \n"
   "   [-nosmooth]      => Suppress spatial smoothing of segmentation mask  \n"
   "                                                                        \n"
   "   [-mask]          => Generate functional image mask (complement)      \n"
   "                         Default: Generate anatomical image            \n"
   "                                                                        \n"
   "   [-quiet]         => Suppress output to screen                        \n"
   "                                                                        \n"
   "   -prefix pname    => Prefix name for file to contain segmented image  \n"
   "                                                                        \n"
   "   ** NOTE **: The newer program 3dSkullStrip will probably give        \n"
   "               better segmentation results than 3dIntracranial!         \n"

   "----------------------------------------------------------------------- \n"
   "                                                                        \n"
   "Examples:                                                               \n"
   "--------                                                                \n"
   "                                                                        \n"
   "   3dIntracranial -anat elvis+orig -prefix elvis_strip                 \n"
   "                                                                        \n"
   "   3dIntracranial -min_val 30 -max_val 350 -anat elvis+orig -prefix strip\n"
   "                                                                        \n"
   "   3dIntracranial -nosmooth -quiet -anat elvis+orig -prefix elvis_strip \n"
   "                                                                        \n"
   "----------------------------------------------------------------------- \n"
      );

  PRINT_COMPILE_DATE ; exit(0);
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
  int ival;                         /* integer input */
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
	  if (nopt >= argc)  SI_error ("need argument after -anat ");
	  anat_filename = malloc (sizeof(char) * MAX_STRING_LENGTH);
	  MTEST (anat_filename);
	  strcpy (anat_filename, argv[nopt]);

	  anat = THD_open_dataset (anat_filename);
     CHECK_OPEN_ERROR(anat,anat_filename) ;
	  DSET_load(anat) ; CHECK_LOAD_ERROR(anat) ;

          /** RWCox [05 Dec 2002]
              If input is a byte dataset, make a short copy of it. **/

          if( DSET_BRICK_TYPE(anat,0) == MRI_byte ){
            THD_3dim_dataset *qset ;
            register byte *bar ; register short *sar ;
            register int ii,nvox ;
            fprintf(stderr,"++ WARNING: converting input dataset from byte to short\n") ;
            qset = EDIT_empty_copy(anat) ;
            nvox = DSET_NVOX(anat) ;
            bar  = (byte *) DSET_ARRAY(anat,0) ;
            sar  = (short *)malloc(sizeof(short)*nvox) ;
            for( ii=0 ; ii < nvox ; ii++ ) sar[ii] = (short) bar[ii] ;
            EDIT_substitute_brick( qset , 0 , MRI_short , sar ) ;
            DSET_delete(anat) ; anat = qset ;
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


      /*-----   -nosmooth -----*/
      if (strcmp(argv[nopt], "-nosmooth") == 0)
	{
	  nosmooth = TRUE;
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
  int nxyz;
  short * sfim;
  int ixyz, icount;
  short * rfim;
  int lower_cutoff = 25;


  /*----- save command line for history notes -----*/
  commandline = tross_commandline( PROGRAM_NAME , argc,argv ) ;


  /*----- Get operator inputs -----*/
  get_options (argc, argv);


  /*----- Verify that inputs are acceptable -----*/
  verify_inputs();


  /*----- Initialize local variables -----*/
  if (anat == NULL)  SI_error ("Unable to read anat dataset");
  nxyz = DSET_NX(anat) * DSET_NY(anat) * DSET_NZ(anat);
  sfim  = (short *) DSET_BRICK_ARRAY(anat,0) ;
  if (sfim == NULL)  SI_error ("Unable to read anat dataset");
  rfim = (short *) malloc (sizeof(short) * nxyz);   MTEST (rfim);


  /*----- Just use voxels whose intensity is above the lower cutoff -----*/
  icount = 0;
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (sfim[ixyz] > lower_cutoff)
      {
	rfim[icount] = sfim[ixyz];
	icount++;
      }
  if (! quiet)
    printf ("%d voxels above lower cutoff = %d \n", icount, lower_cutoff);


  /*----- Get PDF estimate and set voxel intensity limits -----*/
  if (min_val_int || max_val_int)  estpdf_short (icount, rfim, parameters);
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
  dset = THD_open_dataset (anat_filename); CHECK_OPEN_ERROR(dset,anat_filename);
  nxyz = DSET_NX(dset) * DSET_NY(dset) * DSET_NZ(dset);


  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy( dset ) ;


  /*----- Record history of dataset -----*/
  tross_Copy_History( dset , new_dset ) ;
  if( commandline != NULL )
     tross_Append_History( new_dset , commandline ) ;


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
                    ADN_type , ISHEAD(dset) ? HEAD_FUNC_TYPE : GEN_FUNC_TYPE ,
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


  if( THD_deathcon() && THD_is_file(new_dset->dblk->diskptr->header_name) ){
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
      if (write_mask) printf("\nWriting functional (mask) dataset: ");
      else            printf ("\nWriting anatomical dataset: ");
      printf("%s\n", DSET_BRIKNAME(new_dset) ) ;
    }

  printf("NOTE: You will probably get better results by using\n"
         "      3dSkullStrip -input %s -prefix %s\n"   ,
         anat_filename , prefix_filename ) ;

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

int main
(
  int argc,                /* number of input arguments */
  char ** argv             /* array of input arguments */
)

{
  int ii ;

  /*----- Identify software -----*/

#if 0
  for( ii=1 ; ii < argc ; ii++ )
    if( strncmp(argv[ii],"-quiet",6) == 0 ) break ;
  if( ii == argc ){
    printf ("\n\n");
    printf ("Program: %s \n", PROGRAM_NAME);
    printf ("Author:  %s \n", PROGRAM_AUTHOR);
    printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
    printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
    printf ("\n");
  }
#endif

  mainENTRY("3dIntracranial:main") ; machdep() ; PRINT_VERSION("3dIntracranial") ;
  AUTHOR(PROGRAM_AUTHOR) ;
  WARNING_message("This program (3dIntracranial) is old, obsolete, and not maintained!") ;
  INFO_message("3dSkullStrip is almost always superior to 3dIntracranial :)") ;


  /*----- Program initialization -----*/
  initialize_program (argc, argv);


  /*----- Perform automatic segmentation -----*/
  SEGMENT_auto();

  exit(0);
}

/*---------------------------------------------------------------------------*/

