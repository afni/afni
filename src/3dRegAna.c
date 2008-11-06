/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2001, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------------------------------------------------------------------*/
/*
  This program performs multiple linear regression analysis across AFNI
  3d datasets.

  File:    3dRegAna.c
  Author:  B. Douglas Ward
  Date:    10 October 1997


  Mod:     Changes to allow output of AFNI "bucket" type dataset.
  Date:    17 December 1997

  Mod:     Converted some matrix data structures to vector data structures.
  Date:    18 December 1997

  Mod:     Modified calculation of required disk space to account for the
           'bucket' dataset.
  Date:    08 January 1998

  Mod:     Some of the regression analysis software has been moved to 
           the include file RegAna.c.
  Date:	   20 August 1998

  Mod:     Restructured matrix calculations to improve execution speed.
  Date:    16 December 1998

  Mod:     Add use of the argument list extenstion routine addto_args
           to allow the last switch '-@' to get further command line
           arguments from stdin
  Date:    22 February 1999 -- RWCox

  Mod:     Added changes for incorporating History notes.
  Date:    09 September 1999

  Mod:     Replaced dataset input code with calls to THD_open_dataset,
           to allow operator selection of individual sub-bricks for input.
  Date:    03 December 1999

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

  Mod:     Modified routine write_afni_data so that all output
           subbricks will now have the scaled short integer format.
  Date:    14 March 2002

  Mod:     Set MAX_NAME_LENGTH equal to THD_MAX_NAME.
  Date:    02 December 2002

  Mod:     Check for proper ':' usage in -model parameters.
  Date:    24 August 2005 [rickr]

  Mod:     Changed default workmem to 750 MB (from 12).
  Date:    08 December 2005 [rickr]
*/

/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME    "3dRegAna"                   /* name of this program */
#define PROGRAM_AUTHOR  "B. Douglas Ward"                  /* program author */
#define PROGRAM_INITIAL "10 Oct 1997"     /* date of initial program release */
#define PROGRAM_LATEST  "24 Aug 2005"     /* date of latest program revision */

/*---------------------------------------------------------------------------*/

#define SUFFIX ".3dregana"                     /* suffix for temporary files */

#include <stdio.h>
#include <math.h>
#include "mrilib.h"
#include "matrix.h"


#include "RegAna.c"

/*---------------------------------------------------------------------------*/

/*** HP-UX ***/
#ifdef HP
# define DF "bdf ."
#endif

/*** DEC OSF1 ***/
#ifdef OSF1
# define DF "df -k ."
#endif

/*** SGI IRIX ***/
#ifdef SGI
# define DF "df -k ."
#endif

/*** SunOS or Solaris ***/
#if defined(SOLARIS) || defined(SUN)
# define DF "df -k"
#endif

/*** IBM RS6000  ***/
#ifdef RS6000
#endif

/*** Linux 1.2.x ***/
#ifdef LINUX
# define DF "df -k ."
#endif

/*** other? ***/
#ifndef DF
# define DF "df"
#endif


/*---------------------------------------------------------------------------*/

#define MAX_XVARS 101            /* max. number of independent variables */
#define MAX_OBSERVATIONS 1000    /* max. number of input datasets */
#define MAX_NAME_LENGTH THD_MAX_NAME   /* max. string length for file names */ 
#define MEGA  1048576            /* one megabyte */

static char * commandline = NULL ;       /* command line for history notes */


typedef struct model
{
  int p;                       /* number of parameters in full model */
  int flist[MAX_XVARS];        /* list of parameters in full model */
  int q;                       /* number of parameters in reduced model */
  int rlist[MAX_XVARS];        /* list of parameters in reduced model */
}  model;


typedef struct RA_options
{ 
  int   datum;                 /* data type for "intensity" data subbrick */
  char  session[MAX_NAME_LENGTH];     /* name of output directory */

  char  ** yname;              /* names of the input data files */
  char  * first_dataset;       /* name of the first data set */
   
  int   nx, ny, nz;            /* data set dimensions */
  int   nxyz;                  /* number of voxels per image */

  int diskspace;               /* if true, print out how much disk space
                                  is required for program execution */
  int workmem;                 /* working memory */
  int piece_size;              /* number of voxels in dataset piece */
  int num_pieces;              /* dataset is divided into this many pieces */

  float rms_min;               /* minimum rms error to reject constant model */
  float fdisp;                 /* minimum F-value for screen output */

  int * levels;                /* indices for repeat observations */
  int * counts;                /* counts of repeat observations */
  int c;                       /* number of unique rows in ind. var. matrix */
  float flofmax;               /* max. allowed F due to lack of fit */  

  int numf;                    /* number of F-stats volumes      */
  int numr;                    /* number of R^2 volumes          */
  int numt;                    /* number of t-stats volumes      */
  int numc;                    /* number of reg. coef. volumes   */

  char ** fcoef_filename;      /* file names for reg. coefs. and F-stats */
  char ** rcoef_filename;      /* file names for reg. coefs. and R^2     */
  char ** tcoef_filename;      /* file names for reg. coefs. and t-stats */

  int numfiles;                /* number of 2 sub-brick output files */

  char * bucket_filename;      /* file name for bucket dataset */
  int numbricks;               /* number of sub-bricks in bucket dataset */
  int * brick_type;            /* indicates type of sub-brick */
  int * brick_coef;            /* regression coefficient number for sub-brick*/
  char ** brick_label;         /* character string label for sub-brick */

} RA_options;


/*---------------------------------------------------------------------------*/
/*
   Print error message and stop.
*/

void RA_error (char * message)
{
  fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
  exit(1);
}


/*---------------------------------------------------------------------------*/
/*
   Routine to display 3dRegAna help menu.
*/

void display_help_menu()
{
  printf 
    (
     "This program performs multiple linear regression analysis.          \n\n"
     "Usage: \n"
     "3dRegAna \n"
     "-rows n                             number of input datasets          \n"
     "-cols m                             number of X variables             \n"
     "-xydata X11 X12 ... X1m filename    X variables and Y observations    \n"
     "  .                                   .                               \n"
     "  .                                   .                               \n"
     "  .                                   .                               \n"
     "-xydata Xn1 Xn2 ... Xnm filename    X variables and Y observations    \n"
     "                                                                      \n"
     "-model i1 ... iq : j1 ... jr   definition of linear regression model; \n"
     "                                 reduced model:                       \n"
     "                                   Y = f(Xj1,...,Xjr)                 \n"
     "                                 full model:                          \n"
     "                                   Y = f(Xj1,...,Xjr,Xi1,...,Xiq)     \n"
     "                                                                      \n"
     "[-diskspace]       print out disk space required for program execution\n"
     "[-workmem mega]    number of megabytes of RAM to use for statistical  \n"
     "                   workspace  (default = 750 (was 12))                \n"
     "[-rmsmin r]        r = minimum rms error to reject constant model     \n"
     "[-fdisp fval]      display (to screen) results for those voxels       \n"
     "                   whose F-statistic is > fval                        \n"
     "                                                                      \n"
     "[-flof alpha]      alpha = minimum p value for F due to lack of fit   \n"
     "                                                                      \n"
     "                                                                      \n"
     "The following commands generate individual AFNI 2 sub-brick datasets: \n"
     "                                                                      \n"
     "[-fcoef k prefixname]        estimate of kth regression coefficient   \n"
     "                               along with F-test for the regression   \n"
     "                               is written to AFNI `fift' dataset      \n"
     "[-rcoef k prefixname]        estimate of kth regression coefficient   \n"
     "                               along with coef. of mult. deter. R^2   \n"
     "                               is written to AFNI `fith' dataset      \n"
     "[-tcoef k prefixname]        estimate of kth regression coefficient   \n"
     "                               along with t-test for the coefficient  \n"
     "                               is written to AFNI `fitt' dataset      \n"
     "                                                                      \n"
     "                                                                      \n"
     "The following commands generate one AFNI 'bucket' type dataset:       \n"
     "                                                                      \n"
     "[-bucket n prefixname]     create one AFNI 'bucket' dataset having    \n"
     "                             n sub-bricks; n=0 creates default output;\n"
     "                             output 'bucket' is written to prefixname \n"
     "The mth sub-brick will contain:                                       \n"
     "[-brick m coef k label]    kth parameter regression coefficient       \n"
     "[-brick m fstat label]     F-stat for significance of regression      \n"
     "[-brick m rstat label]     coefficient of multiple determination R^2  \n"
     "[-brick m tstat k label]   t-stat for kth regression coefficient      \n"
     "\n"
     "[-datum DATUM]     write the output in DATUM format. \n"
     "                   Choose from short (default) or float.\n" 
     "\n" );

  printf
    (
     "\n"
     "N.B.: For this program, the user must specify 1 and only 1 sub-brick  \n"
     "      with each -xydata command. That is, if an input dataset contains\n"
     "      more than 1 sub-brick, a sub-brick selector must be used, e.g.: \n"
     "      -xydata 2.17 4.59 7.18  'fred+orig[3]'                          \n"
     );
	  
  printf("\n" MASTER_SHORTHELP_STRING ) ;

  PRINT_COMPILE_DATE ; exit(0);
}


/*---------------------------------------------------------------------------*/

/** macro to open a dataset and make it ready for processing **/

#define DOPEN(ds,name)                                                        \
do{ int pv ; (ds) = THD_open_dataset((name)) ;                                \
       if( !ISVALID_3DIM_DATASET((ds)) ){                                     \
          fprintf(stderr,"*** Can't open dataset: %s\n",(name)) ; exit(1) ; } \
       if( (ds)->daxes->nxx!=nx || (ds)->daxes->nyy!=ny ||                    \
	   (ds)->daxes->nzz!=nz ){                                            \
          fprintf(stderr,"*** Axes mismatch: %s\n",(name)) ; exit(1) ; }      \
       if( DSET_NVALS((ds)) != 1 ){                                           \
         fprintf(stderr,"*** Must specify 1 sub-brick: %s\n",(name));exit(1);}\
       DSET_load((ds)) ;                                                      \
       pv = DSET_PRINCIPAL_VALUE((ds)) ;                                      \
       if( DSET_ARRAY((ds),pv) == NULL ){                                     \
          fprintf(stderr,"*** Can't access data: %s\n",(name)) ; exit(1); }   \
       if( DSET_BRICK_TYPE((ds),pv) == MRI_complex ){                         \
          fprintf(stderr,"*** Can't use complex data: %s\n",(name)); exit(1); \
       }                                                                      \
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
   Get the dimensions of the 3d AFNI data sets.
*/

void get_dimensions 
(
  RA_options * option_data            /* user input options */
)

{
  
   THD_3dim_dataset * dset=NULL;

   /*----- read first dataset to get dimensions, etc. -----*/

   dset = THD_open_dataset( option_data->first_dataset ) ;
   CHECK_OPEN_ERROR(dset,option_data->first_dataset) ;

   /*----- data set dimensions in voxels -----*/
   option_data->nx = dset->daxes->nxx ;
   option_data->ny = dset->daxes->nyy ;
   option_data->nz = dset->daxes->nzz ;       
   option_data->nxyz = option_data->nx * option_data->ny * option_data->nz ;

   THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;

}


/*---------------------------------------------------------------------------*/
/*
  Read one AFNI data set from file 'filename'. 
  The data is converted to floating point (in ffim).
*/

void read_afni_data 
(
  RA_options * option_data,        /* user input options */
  char * filename,                 /* input AFNI dataset file name */    
  int piece_len,                   /* number of voxels in current piece */
  int fim_offset,                  /* array offset to current piece */
  float * ffim                     /* input data in floating point format */
)

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
  DOPEN (dset, filename) ;
  iv = DSET_PRINCIPAL_VALUE(dset) ;
  
  /*----- convert it to floats (in ffim) -----*/
  SUB_POINTER (dset, iv, fim_offset, vfim) ;
  EDIT_coerce_scale_type (piece_len, DSET_BRICK_FACTOR(dset,iv),
			  DSET_BRICK_TYPE(dset,iv), vfim,      /* input  */
			  MRI_float               ,ffim  ) ;   /* output */
  
  THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check whether one piece file already exists.
*/

void check_piece 
(
  char * filename                    /* name of piece file */
)

{
  FILE * far = NULL;                 /* pointer to temporary file */
  char sfilename[MAX_NAME_LENGTH];   /* name of temporary file */
  char message[MAX_NAME_LENGTH];     /* error message */
  
  
  /*-----  see if piece file already exists -----*/
  strcpy (sfilename, filename);
  strcat (sfilename, SUFFIX);
  far = fopen (sfilename, "r");
  if (far != NULL)
    {
      sprintf (message, "temporary file %s already exists. ", sfilename); 
      RA_error (message);
    }
  
}


/*---------------------------------------------------------------------------*/
/*
   Routine to read a piece of floating point volume.
*/

void read_piece 
(
  char * filename,                   /* root name of piece file */
  float * fin,                       /* input floating point piece */
  int size                           /* number of voxels in piece */
)

{
  char sfilename[MAX_NAME_LENGTH];   /* piece file name */
  char message[MAX_NAME_LENGTH];     /* error message */
  FILE * far = NULL;                 /* floating point input file */
  int count;                         /* number of data items read from disk */
  
  
  /*----- input file name -----*/
  strcpy (sfilename, filename);
  strcat (sfilename, SUFFIX);
  
  /*----- open temporary data file for input -----*/
  far = fopen (sfilename, "r");
  if (far == NULL) 
    {
      sprintf (message, "Unable to open temporary file %s", sfilename);
      RA_error (message);
    }
  
  /*----- read 3d data file -----*/
  count = fread (fin, sizeof(float), size, far);   
  fclose (far);
  
  /*----- error in reading file? -----*/
  if (count != size)  
    {
      sprintf (message, "Error in reading temporary file %s", sfilename);
      RA_error (message);
    }
}


/*---------------------------------------------------------------------------*/
/*
   Routine to write a piece of floating point volume to a (temporary)
   disk file.
*/

void write_piece 
(
  char * filename,                   /* root name of output piece file */
  float * fout,                      /* floating point output piece */
  int size                           /* number of voxels in piece */
)

{
  char sfilename[MAX_NAME_LENGTH];   /* piece file name */ 
  char message[MAX_NAME_LENGTH];     /* error message */
  FILE * far = NULL;                 /* floating point output file */
  int count;                         /* number of data items written to disk */


   /*----- output file name -----*/
   strcpy (sfilename, filename);
   strcat (sfilename, SUFFIX);

   /*----- first, see if file already exists -----*/
   far = fopen (sfilename, "r");
   if (far != NULL)
   {
      sprintf (message, "Temporary file %s already exists. ", sfilename); 
      RA_error (message);
   }

   /*----- open temporary data file for output -----*/
   far = fopen (sfilename, "w");
   if (far == NULL) 
     {
       sprintf (message, "Unable to write temporary file %s ", sfilename); 
       RA_error (message);
     }
     
   /*----- write 3d data set -----*/
   count = fwrite (fout, sizeof(float), size, far);
   fclose (far);

  /*----- error in writing file? -----*/
  if (count != size)  
    {
      sprintf (message, "Error in writing temporary file %s ", sfilename); 
      RA_error (message);
    }
 
}


/*---------------------------------------------------------------------------*/
/*
  Routine to delete a file containing a piece of floating point volume.
*/

void delete_piece 
(
  char * filename      /* root name of piece file to be deleted */
)

{
  char sfilename[MAX_NAME_LENGTH];            /* file name */
  
  /*----- build file name -----*/
  strcpy (sfilename, filename);
  strcat (sfilename, SUFFIX);

  /*----- delete 3d data file -----*/
  remove (sfilename);
  
}


/*---------------------------------------------------------------------------*/
/* 
  Allocate memory space for multiple pieces. 
*/

void allocate_pieces 
(
  matrix xdata,                /* independent variable matrix */
  model * regmodel,            /* linear regression model */
  RA_options * option_data,    /* user input options */

  float *** yfimar,      /* array of pieces of Y-datasets */
  float ** freg_piece,   /* piece F-statistic for the full regression model */
  float ** rsqr_piece,   /* piece coeff. of multiple determination R^2  */
  float *** coef_piece,  /* piece regression parameters */
  float *** tcoef_piece  /* piece t-statistics for regression parameters */
)

{  
  int n;                 /* number of datasets */
  int p;                 /* number of parameters in the full model */
  int * flist = NULL;    /* list of parameters in the full model */
  int i;                 /* dataset index */
  int ip;                /* parameter index */
  int ix;                /* x-variable index */
  int piece_size;        /* number of voxels in datasset piece */
  int ib;                /* sub-brick index */
  int nbricks;           /* number of sub-bricks in bucket dataset */


  /*----- initialize local variables -----*/
  n = xdata.rows;
  p = regmodel->p;
  flist = regmodel->flist;
  piece_size = option_data->piece_size;
  nbricks = option_data->numbricks;


  /*----- allocate memory space for input data -----*/
  *yfimar = (float **) malloc (sizeof(float *) * n);  
  MTEST (*yfimar);
  for (i = 0;  i < n;  i++)
    {
      (*yfimar)[i] = (float *) malloc(sizeof(float) * piece_size);  
      MTEST ((*yfimar)[i]);
    }  


  /*----- allocate memory space for F-statistics data -----*/
  for (ip = 0;  ip < p;  ip++)
    {
      ix = flist[ip];

      if (option_data->fcoef_filename[ix] != NULL)
	{
	  if (*freg_piece == NULL)
	    {
	      *freg_piece = (float *) malloc (sizeof(float) * piece_size);
	      MTEST (*freg_piece);
	    }
	}
    }

  for (ib = 0;  ib < nbricks;  ib++)
    {
     if (option_data->brick_type[ib] == FUNC_FT_TYPE)
	{
	  if (*freg_piece == NULL)
	    {
	      *freg_piece = (float *) malloc (sizeof(float) * piece_size);
	      MTEST (*freg_piece);
	    }
	}
    }


  /*----- allocate memory space for coef. of mult. determination R^2 -----*/
  for (ip = 0;  ip < p;  ip++)
    {
      ix = flist[ip];

      if (option_data->rcoef_filename[ix] != NULL)
	{
	  if (*rsqr_piece == NULL)
	    {
	      *rsqr_piece = (float *) malloc (sizeof(float) * piece_size);
	      MTEST (*rsqr_piece);
	    }
	}
    }

  for (ib = 0;  ib < nbricks;  ib++)
    {
     if (option_data->brick_type[ib] == FUNC_THR_TYPE)
	{
	  if (*rsqr_piece == NULL)
	    {
	      *rsqr_piece = (float *) malloc (sizeof(float) * piece_size);
	      MTEST (*rsqr_piece);
	    }
	}
    }


  /*----- allocate memory space for parameters and t-stats -----*/
  *coef_piece = (float **) malloc (sizeof(float *) * MAX_XVARS);
  MTEST (*coef_piece);

  *tcoef_piece = (float **) malloc (sizeof(float *) * MAX_XVARS);
  MTEST (*tcoef_piece);

  for (ix = 0;  ix < MAX_XVARS;  ix++)
    {
	(*coef_piece)[ix] = NULL;
	(*tcoef_piece)[ix] = NULL;
    }

  for (ip = 0;  ip < p;  ip++)
    {
      ix = flist[ip];

      if (   (option_data->fcoef_filename[ix] != NULL) 
	  || (option_data->rcoef_filename[ix] != NULL)
	  || (option_data->tcoef_filename[ix] != NULL))
	{
	  (*coef_piece)[ix] = 
	    (float *) malloc (sizeof(float) * piece_size);
	  MTEST ((*coef_piece)[ix]);
	}

      if (option_data->tcoef_filename[ix] != NULL)
	{
	  (*tcoef_piece)[ix] = 
	    (float *)  malloc (sizeof(float) * piece_size);      
	  MTEST ((*tcoef_piece)[ix]);
	}
    }

  for (ib = 0;  ib < nbricks;  ib++)
    {
     if (option_data->brick_type[ib] == FUNC_FIM_TYPE)
	{
	  ix = option_data->brick_coef[ib];
	  if ((*coef_piece)[ix] == NULL)
	    {
	      (*coef_piece)[ix] = (float *) malloc (sizeof(float)*piece_size);
	      MTEST ((*coef_piece)[ix]);
	    }
	}
    }

  for (ib = 0;  ib < nbricks;  ib++)
    {
     if (option_data->brick_type[ib] == FUNC_TT_TYPE)
	{
	  ix = option_data->brick_coef[ib];
	  if ((*tcoef_piece)[ix] == NULL)
	    {
	      (*tcoef_piece)[ix] = (float *) malloc (sizeof(float)*piece_size);
	      MTEST ((*tcoef_piece)[ix]);
	    }
	}
    }

}


/*---------------------------------------------------------------------------*/
/*
  Save multiple piece data into temporary files.
*/

void save_pieces 
(
  int piece,                  /* current piece index */
  int piece_len,              /* number of voxels in current piece */

  float * freg_piece,    /* piece F-statistic for the full regression model */
  float * rsqr_piece,    /* piece coeff. of multiple determination R^2  */
  float ** coef_piece,   /* piece regression parameters */
  float ** tcoef_piece   /* piece t-statistics for regression parameters */
)

{
  int ip;                                /* parameter index */
  char filename[MAX_NAME_LENGTH];        /* name for temporary data file */ 


  /*----- save piece containing F-statistics -----*/
  if (freg_piece != NULL)
    { 
      sprintf (filename, "freg.p%d", piece);
      write_piece (filename, freg_piece, piece_len);
    }


  /*----- save piece containing R^2 -----*/
  if (rsqr_piece != NULL)
    { 
      sprintf (filename, "rsqr.p%d", piece);
      write_piece (filename, rsqr_piece, piece_len);
    }


  /*----- save pieces containing regression coefficients -----*/
  if (coef_piece != NULL)
    {
      for (ip = 0;  ip < MAX_XVARS;  ip++)
	if (coef_piece[ip] != NULL)
	  {
	    sprintf (filename, "coef.%d.p%d", ip, piece);
	    write_piece (filename, coef_piece[ip], piece_len);
	  }
    }


  /*----- save pieces containing t-statistics -----*/
  if (tcoef_piece != NULL)
    {
      for (ip = 0;  ip < MAX_XVARS;  ip++)
	if (tcoef_piece[ip] != NULL)
	  {
	    sprintf (filename, "tcoef.%d.p%d", ip, piece);
	    write_piece (filename, tcoef_piece[ip], piece_len);
	  }
    }

}


/*---------------------------------------------------------------------------*/
/*
  Deallocate memory space for multiple pieces.
*/

void deallocate_pieces 
(
  int n,                  /* number of datasets */
  float *** yfimar,       /* array of pieces of Y-datasets */
  float ** freg_piece,    /* piece F-statistic for the full regression model */
  float ** rsqr_piece,    /* piece coeff. of multiple determination R^2  */
  float *** coef_piece,   /* piece regression parameters */
  float *** tcoef_piece   /* piece t-statistics for regression parameters */
)

{
  int i;                  /* dataset index */
  int ip;                 /* parameter index */


  /*----- deallocate memory space for input data -----*/
  if (*yfimar != NULL)
    {
      for (i = 0;  i < n;  i++)
	{
	  free ((*yfimar)[i]);   (*yfimar)[i] = NULL;
	}
      free (*yfimar);   
      *yfimar = NULL;
    }

  /*----- deallocate memory space for F-statistics data -----*/
  if (*freg_piece != NULL)
    {  
      free (*freg_piece);
      *freg_piece = NULL;
    }

  /*----- deallocate memory space for coef. of mult. determination R^2 -----*/
  if (*rsqr_piece != NULL)
    {
      free (*rsqr_piece);
      *rsqr_piece = NULL;
    }

  /*----- deallocate memory space for regression coefficients -----*/
  if (*coef_piece != NULL)
    {
      for (ip = 0;  ip < MAX_XVARS;  ip++)
	if ((*coef_piece)[ip] != NULL)
	  {
	    free ((*coef_piece)[ip]);
	    (*coef_piece)[ip] = NULL;
	  }
      free (*coef_piece);
      *coef_piece = NULL;
    }

  /*----- deallocate memory space for t-statistics -----*/
  if (*tcoef_piece != NULL)
    {
      for (ip = 0;  ip < MAX_XVARS;  ip++)
	if ((*tcoef_piece)[ip] != NULL)
	  {
	    free ((*tcoef_piece)[ip]);
	    (*tcoef_piece)[ip] = NULL;
	  }
      free (*tcoef_piece);
      *tcoef_piece = NULL;
    }

}


/*---------------------------------------------------------------------------*/
/*
  Check whether any of the piece files corresponding to a volume already exist.
*/

void check_volume
(
  char * filename,            /* root file name */
  int num_pieces              /* dataset is divided into this many pieces */
)

{
  int piece;                              /* piece index */
  char sfilename[MAX_NAME_LENGTH];        /* name for temporary data file */ 

  
  /*----- loop over the temporary data file pieces -----*/
  for (piece = 0;  piece < num_pieces;  piece++)
    {
      /*----- check for piece data file -----*/
      sprintf (sfilename, "%s.p%d", filename, piece);
      check_piece (sfilename); 
	  
    }  /* loop over pieces */

}


/*---------------------------------------------------------------------------*/
/*
  Read 3d volume consisting of multiple piece files.
*/

void read_volume 
(
  char * filename,            /* root file name */
  float * volume,             /* floating point volume data */
  int nxyz,                   /* number of voxels per volume */
  int piece_size,             /* number of voxels in dataset piece */
  int num_pieces              /* dataset is divided into this many pieces */
)

{
  int piece;                  /* piece index */
  int piece_len;              /* number of voxels in current piece */
  int fim_offset;             /* array offset to current piece */
  char sfilename[MAX_NAME_LENGTH];        /* name for temporary data file */ 

  
  /*----- loop over the temporary data file pieces -----*/
  for (piece = 0;  piece < num_pieces;  piece++)
    {

      /*----- current offset into volume -----*/
      fim_offset = piece * piece_size;

      /*----- size of current piece -----*/
      if (piece < num_pieces-1)
	piece_len = piece_size;
      else
	piece_len = nxyz - fim_offset;

      /*----- read in the piece data -----*/
      sprintf (sfilename, "%s.p%d", filename, piece);
      read_piece (sfilename, volume + fim_offset, piece_len); 

    }  /* loop over pieces */

}


/*---------------------------------------------------------------------------*/
/*
  Delete 3d volume consisting of multiple piece files.
*/

void delete_volume
(
  char * filename,            /* root file name */
  int nxyz,                   /* number of voxels per volume */
  int piece_size,             /* number of voxels in dataset piece */
  int num_pieces              /* dataset is divided into this many pieces */
)

{
  int piece;                              /* piece index */
  char sfilename[MAX_NAME_LENGTH];        /* name for temporary data file */ 

  
  /*----- loop over the temporary data file pieces -----*/
  for (piece = 0;  piece < num_pieces;  piece++)
    {

      /*----- delete the piece data file -----*/
      sprintf (sfilename, "%s.p%d", filename, piece);
      delete_piece (sfilename); 
	  
    }  /* loop over pieces */

}


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize input options.
*/

void initialize_options 
( 
  model * regmodel,            /* regression model */
  RA_options * option_data     /* user input options */
)

{
  int ip;          /* index */


  regmodel->p = 0;
  regmodel->q = 0;
  
  option_data->datum = ILLEGAL_TYPE;
  strcpy (option_data->session, "./");

  option_data->yname = NULL;
  option_data->first_dataset = NULL;
  
  option_data->nx = 0;
  option_data->ny = 0;
  option_data->nz = 0;
  option_data->nxyz = 0;

  option_data->diskspace = 0;
  option_data->workmem = 750;           /* from 12 - 8 Dec 2005 [rickr] */
  option_data->piece_size = 0;
  option_data->num_pieces = 0;

  option_data->rms_min = 0.0;
  option_data->fdisp = -1.0;

  option_data->levels = NULL;
  option_data->counts = NULL;
  option_data->c = 0;
  option_data->flofmax = -1;

  option_data->numf = 0;
  option_data->numr = 0;
  option_data->numc = 0;
  option_data->numt = 0;

  option_data->fcoef_filename = NULL;
  option_data->rcoef_filename = NULL;
  option_data->tcoef_filename = NULL;

  option_data->numfiles = 0;
  
  /*----- allocate memory for storing data file names -----*/
  option_data->yname
      = (char **) malloc (sizeof(char *) * MAX_OBSERVATIONS);
  for (ip = 0;  ip < MAX_OBSERVATIONS;  ip++)
    option_data->yname[ip] = NULL;


  /*----- allocate memory space and initialize pointers for filenames -----*/
  option_data->fcoef_filename = 
    (char **) malloc (sizeof(char *) * MAX_XVARS);
  if (option_data->fcoef_filename == NULL)
    RA_error ("Unable to allocate memory for fcoef_filename");

  option_data->rcoef_filename = 
    (char **) malloc (sizeof(char *) * MAX_XVARS);
  if (option_data->rcoef_filename == NULL)
    RA_error ("Unable to allocate memory for rcoef_filename");

  option_data->tcoef_filename = 
    (char **) malloc (sizeof(char *) * MAX_XVARS);
  if (option_data->tcoef_filename == NULL)
    RA_error ("Unable to allocate memory for tcoef_filename");

  for (ip = 0;  ip < MAX_XVARS;  ip++)
    {
      option_data->fcoef_filename[ip] = NULL;
      option_data->rcoef_filename[ip] = NULL;
      option_data->tcoef_filename[ip] = NULL;
    }


  /*----- initialize bucket dataset options -----*/
  option_data->bucket_filename = NULL;
  option_data->numbricks = -1;
  option_data->brick_type = NULL;
  option_data->brick_coef = NULL;
  option_data->brick_label = NULL;

}

   
/*---------------------------------------------------------------------------*/
/*
  Routine to sort model variable index numbers. (This is for convenience only.)
*/

void sort_model_indices 
(
  model * regmodel             /* regression model */
)

{
  int i, j, temp;              /* model variable index numbers */


  /*----- sort full model indices into ascending order -----*/
  for (i = 0;  i < regmodel->p - 1;  i++)
    for (j = i+1;  j < regmodel->p;  j++)
      if (regmodel->flist[i] > regmodel->flist[j])
	{
	  temp = regmodel->flist[i];
	  regmodel->flist[i] = regmodel->flist[j];
	  regmodel->flist[j] = temp;
	}
      else if (regmodel->flist[i] == regmodel->flist[j])
	RA_error ("Duplicate variable indices in model definition");	


  /*----- sort reduced model indices into ascending order -----*/
  for (i = 0;  i < regmodel->q - 1;  i++)
    for (j = i+1;  j < regmodel->q;  j++)
      if (regmodel->rlist[i] > regmodel->rlist[j])
	{
	  temp = regmodel->rlist[i];
	  regmodel->rlist[i] = regmodel->rlist[j];
	  regmodel->rlist[j] = temp;
	}

}


/*---------------------------------------------------------------------------*/
/*
   Routine to get user specified regression analysis inputs.
*/

void get_inputs 
(
  int argc,                    /* number of input arguments */
  char ** argv,                /* array of input arguments */ 
  matrix * xdata,              /* independent variable matrix */
  model * regmodel,            /* linear regression model */
  RA_options * option_data     /* user input options */
)

{
  const MAX_BRICKS = 100;          /* max. number of bricks in the bucket */
  int nopt = 1;                    /* input option argument counter */
  int ival, index;                 /* integer input */
  float fval;                      /* float input */
  int rows, cols;                  /* number rows and columns for x matrix */
  int irows, jcols;                /* data point counters */ 
  THD_3dim_dataset * dset=NULL;    /* test whether data set exists */
  char message[MAX_NAME_LENGTH];   /* error message */
  char label[MAX_NAME_LENGTH];     /* sub-brick label */
  int ibrick;                      /* sub-brick index */
  int nbricks;                     /* number of sub-bricks in the bucket */
  int p;                           /* number of parameters */
  int ip, ix;                      /* parameter indices */


  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();
 
  
  /*----- add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv); 


  /*----- initialize the input options -----*/
  initialize_options (regmodel, option_data);


  /*----- main loop over input options -----*/
  while (nopt < argc && argv[nopt][0] == '-')
  {

    /*-----   -datum type   -----*/
    if( strncmp(argv[nopt],"-datum",6) == 0 ){
      if( ++nopt >= argc ) RA_error("need an argument after -datum!") ;
      
      if( strcmp(argv[nopt],"short") == 0 ){
	option_data->datum = MRI_short ;
      } else if( strcmp(argv[nopt],"float") == 0 ){
	option_data->datum = MRI_float ;
      } else {
	char buf[256] ;
	sprintf(buf,"-datum of type '%s' is not supported in 3dRegAna.",
		argv[nopt] ) ;
	RA_error(buf) ;
      }
      nopt++ ; continue ;  /* go to next arg */
    }
      
    
    /*-----   -session dirname    -----*/
    if( strncmp(argv[nopt],"-session",8) == 0 ){
      nopt++ ;
      if( nopt >= argc ) RA_error("need argument after -session!") ;
      strcpy(option_data->session , argv[nopt++]) ;
      continue ;
    }
    
    
    /*-----   -diskspace   -----*/
    if( strncmp(argv[nopt],"-diskspace",10) == 0 )
      {
	option_data->diskspace = 1;
	nopt++ ; continue ;  /* go to next arg */
      }

      
    /*-----   -workmem megabytes  -----*/

    if( strncmp(argv[nopt],"-workmem",6) == 0 ){
      nopt++ ;
      if( nopt >= argc ) RA_error ("need argument after -workmem!") ;
      sscanf (argv[nopt], "%d", &ival);
      if( ival <= 0 ) RA_error ("illegal argument after -workmem!") ;
      option_data->workmem = ival ;
      nopt++ ; continue ;
    }
    
    
    /*-----   -rmsmin r  -----*/
    if (strncmp(argv[nopt], "-rmsmin", 7) == 0)
      {
	nopt++;
	if (nopt >= argc)  RA_error ("need argument after -rmsmin ");
	sscanf (argv[nopt], "%f", &fval); 
	if (fval < 0.0)
	  RA_error ("illegal argument after -rmsmin ");
	option_data->rms_min = fval;
	nopt++;
	continue;
      }


    /*-----   -flof alpha   -----*/
    if (strncmp(argv[nopt], "-flof", 6) == 0)
      {
	nopt++;
	if (nopt >= argc)  RA_error ("need argument after -flof ");
	sscanf (argv[nopt], "%f", &fval); 
	if ((fval < 0.0) || (fval > 1.0))
	  RA_error ("illegal argument after -flof ");
	option_data->flofmax = fval;
	nopt++;
	continue;
      }
        
    
    /*-----   -fdisp fval   -----*/
    if (strncmp(argv[nopt], "-fdisp", 6) == 0)
      {
	nopt++;
	if (nopt >= argc)  RA_error ("need argument after -fdisp ");
	sscanf (argv[nopt], "%f", &fval); 
	option_data->fdisp = fval;
	nopt++;
	continue;
      }
    
    
    /*-----   -rows n  -----*/
    if (strncmp(argv[nopt], "-rows", 5) == 0)
      {
	nopt++;
	if (nopt >= argc)  RA_error ("need argument after -rows ");
	sscanf (argv[nopt], "%d", &ival);
	if ((ival <= 0) || (ival > MAX_OBSERVATIONS))
	  RA_error ("illegal argument after -rows ");
	rows = ival;
	nopt++;
	continue;
      }
 

    /*-----   -cols m  -----*/
    if (strncmp(argv[nopt], "-cols", 5) == 0)
      {
	nopt++;
	if (nopt >= argc)  RA_error ("need argument after -cols ");
	sscanf (argv[nopt], "%d", &ival);
	if ((ival <= 0) || (ival > MAX_XVARS))
	  RA_error ("illegal argument after -cols ");
	cols = ival;
	nopt++;

	matrix_create (rows, cols+1, xdata);
	irows = -1;
	continue;
      }
    
    
    /*-----   -xydata x1 ... xm  y  -----*/
    if (strncmp(argv[nopt], "-xydata", 7) == 0)
      {
	nopt++;
	if (nopt + cols >= argc)  
	  RA_error ("need cols+1 arguments after -xydata ");
        
	irows++;
	if (irows > rows-1)  RA_error ("too many data files");

	xdata->elts[irows][0] = 1.0;
	for (jcols = 1;  jcols <= cols;  jcols++)
	  {
	    sscanf (argv[nopt], "%f", &fval); 
	    xdata->elts[irows][jcols] = fval;
            nopt++;
	  }  
	
	/*--- check whether input files exist ---*/
	dset = THD_open_dataset( argv[nopt] ) ;
   CHECK_OPEN_ERROR(dset,argv[nopt]) ;
	THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
	
	option_data->yname[irows] 
	  =  malloc (sizeof(char) * MAX_NAME_LENGTH);
	strcpy (option_data->yname[irows], argv[nopt]);
	nopt++;
	continue;
      }

    
    /*-----   -model   -----*/
    if (strncmp(argv[nopt], "-model", 6) == 0)
      {
	nopt++;
	if (nopt >= argc)  RA_error ("need arguments after -model ");

	while ((nopt < argc)
	       && (strncmp(argv[nopt], "-", 1) != 0)
	       && (strncmp(argv[nopt], ":", 1) != 0))
	  {
	    sscanf (argv[nopt], "%d", &ival);
	    if ((ival <= 0) || (ival > cols))
            {
              fprintf(stderr,"bad Xi integer, '%s'\n\n", argv[nopt]);
	      RA_error ("illegal argument after -model ");
            }
	    regmodel->flist[regmodel->p] = ival;
	
	    regmodel->p++;
	    nopt++;
	  }
	
	if (strncmp(argv[nopt], ":", 1) == 0)
	  {
            if (isdigit(argv[nopt][1]))  /* e.g. the user gives :3 ... */
              {
                fprintf(stderr,
                        "please separate ':' from -model argument in '%s'\n\n",
                        argv[nopt]);
		RA_error ("illegal argument after -model ");
              }
	    nopt++;

	    while ((nopt < argc)
		   && (strncmp(argv[nopt], "-", 1) != 0))
	      {
		sscanf (argv[nopt], "%d", &ival);
		if ((ival < 0) || (ival > cols))
                {
                  fprintf(stderr,"bad Xj integer, '%s'\n\n", argv[nopt]);
		  RA_error ("illegal argument after -model ");
                }
		regmodel->flist[regmodel->p] = ival;
		regmodel->rlist[regmodel->q] = ival;
		regmodel->p++;
		regmodel->q++;
		nopt++;
	      }
	  }    
        /* incorrect usage, complain and fail        24 Aug 2005 [rickr] */
        else
	  {
            fprintf(stderr,"missing ':' between sets of -model parameters\n"
                           "    (is ':' attached to a number?)\n\n");
            RA_error ("bad -model usage ");
	  }    
	
	/*----- sort model variable indices into ascending order -----*/
	sort_model_indices (regmodel);
	
	continue;
      }
    
        
    /*-----   -fcoef k filename   -----*/
    if (strncmp(argv[nopt], "-fcoef", 6) == 0)
      {
	nopt++;
	if (nopt+1 >= argc)  RA_error ("need 2 arguments after -fcoef ");
	sscanf (argv[nopt], "%d", &ival);
	if ((ival < 0) || (ival > cols))
	  RA_error ("illegal argument after -fcoef ");
	index = ival;
	nopt++;
	
	option_data->fcoef_filename[index] = 
	  malloc (sizeof(char) * MAX_NAME_LENGTH);
	if (option_data->fcoef_filename[index] == NULL)
	  RA_error ("Unable to allocate memory for fcoef_filename");
	strcpy (option_data->fcoef_filename[index], argv[nopt]);
	
	nopt++;
	continue;
      }
      
    
    /*-----   -rcoef k filename   -----*/
    if (strncmp(argv[nopt], "-rcoef", 6) == 0)
      {
	nopt++;
	if (nopt+1 >= argc)  RA_error ("need 2 arguments after -rcoef ");
	sscanf (argv[nopt], "%d", &ival);
	if ((ival < 0) || (ival > cols))
	  RA_error ("illegal argument after -rcoef ");
	index = ival;
	nopt++;
	
	option_data->rcoef_filename[index] = 
	  malloc (sizeof(char) * MAX_NAME_LENGTH);
	if (option_data->rcoef_filename[index] == NULL)
	  RA_error ("Unable to allocate memory for rcoef_filename");
	strcpy (option_data->rcoef_filename[index], argv[nopt]);
	
	nopt++;
	continue;
      }
      
    
    /*-----   -tcoef k filename   -----*/
    if (strncmp(argv[nopt], "-tcoef", 6) == 0)
      {
	nopt++;
	if (nopt+1 >= argc)  RA_error ("need 2 arguments after -tcoef ");
	sscanf (argv[nopt], "%d", &ival);
	if ((ival < 0) || (ival > cols))
	  RA_error ("illegal argument after -tcoef ");
	index = ival;
	nopt++;
	
	option_data->tcoef_filename[index] = 
	  malloc (sizeof(char) * MAX_NAME_LENGTH);
	if (option_data->tcoef_filename[index] == NULL)
	  RA_error ("Unable to allocate memory for tcoef_filename");
	strcpy (option_data->tcoef_filename[index], argv[nopt]);
	
	nopt++;
	continue;
      }


    /*----- -bucket n prefixname -----*/
    if (strncmp(argv[nopt], "-bucket", 7) == 0)
      {
	nopt++;
	if (nopt+1 >= argc)  RA_error ("need 2 arguments after -bucket ");
	sscanf (argv[nopt], "%d", &ival);
	if ((ival < 0) || (ival > MAX_BRICKS))
	  RA_error ("illegal argument after -bucket ");
	option_data->numbricks = ival;
	nopt++;
	
	option_data->bucket_filename = 
	  malloc (sizeof(char) * MAX_NAME_LENGTH);
	if (option_data->bucket_filename == NULL)
	  RA_error ("Unable to allocate memory for bucket_filename");
	strcpy (option_data->bucket_filename, argv[nopt]);
	  
	/*----- set number of sub-bricks in the bucket -----*/
	p = regmodel->p;
	if (ival == 0)
	  nbricks = 2*p + 2; 
	else
	  nbricks = ival;
	option_data->numbricks = nbricks;

	/*----- allocate memory and initialize bucket dataset options -----*/
	option_data->brick_type = malloc (sizeof(int) * nbricks);
	option_data->brick_coef = malloc (sizeof(int) * nbricks);
	option_data->brick_label = malloc (sizeof(char *) * nbricks);
	for (ibrick = 0;  ibrick < nbricks;  ibrick++)
	  {
	    option_data->brick_type[ibrick] = -1;
	    option_data->brick_coef[ibrick] = -1;
	    option_data->brick_label[ibrick] = 
	      malloc (sizeof(char) * MAX_NAME_LENGTH);
	  }

	if (ival == 0)     /*----- throw everything into the bucket -----*/
	  {
	    for (ip = 0;  ip < p;  ip++)
	      {
		ix = regmodel->flist[ip];

		ibrick = 2*ip;
		option_data->brick_type[ibrick] = FUNC_FIM_TYPE;
		option_data->brick_coef[ibrick] = ix;
		sprintf (label, "Coef #%d est.", ix);
		strcpy (option_data->brick_label[ibrick], label);

		ibrick = 2*ip + 1;
		option_data->brick_type[ibrick] = FUNC_TT_TYPE;
		option_data->brick_coef[ibrick] = ix;
		sprintf (label, "Coef #%d t-stat", ix);
		strcpy (option_data->brick_label[ibrick], label);
	      }

	    ibrick = 2*p;
	    option_data->brick_type[ibrick] = FUNC_FT_TYPE;
	    strcpy (option_data->brick_label[ibrick], "F-stat Regression");

	    ibrick = 2*p + 1;
	    option_data->brick_type[ibrick] = FUNC_THR_TYPE;
	    strcpy (option_data->brick_label[ibrick], "R^2 Regression");

	  }

	nopt++;
	continue;
      }

    
    /*----- -brick m type k label -----*/
    if (strncmp(argv[nopt], "-brick", 6) == 0)
      {
	nopt++;
	if (nopt+2 >= argc)  RA_error ("need more arguments after -brick ");
	sscanf (argv[nopt], "%d", &ibrick);
	if ((ibrick < 0) || (ibrick >= option_data->numbricks))
	  RA_error ("illegal argument after -brick ");
     	nopt++;

	if (strncmp(argv[nopt], "coef", 4) == 0)
	  {
	    option_data->brick_type[ibrick] = FUNC_FIM_TYPE;

	    nopt++;
	    sscanf (argv[nopt], "%d", &ival);
	    if ((ival < 0) || (ival > cols))
	      RA_error ("illegal argument after coef ");
	    option_data->brick_coef[ibrick] = ival;
    
	    nopt++;
	    if (nopt >= argc)  RA_error ("need more arguments after -brick ");
	    strcpy (option_data->brick_label[ibrick], argv[nopt]);
	    
	  }
	else if (strncmp(argv[nopt], "tstat", 4) == 0)
	  {
	    option_data->brick_type[ibrick] = FUNC_TT_TYPE;

	    nopt++;
	    sscanf (argv[nopt], "%d", &ival);
	    if ((ival < 0) || (ival > cols))
	      RA_error ("illegal argument after tstat ");
	    option_data->brick_coef[ibrick] = ival;
    
	    nopt++;
	    if (nopt >= argc)  RA_error ("need more arguments after -brick ");
	    strcpy (option_data->brick_label[ibrick], argv[nopt]);
	    
	  }
	else if (strncmp(argv[nopt], "fstat", 4) == 0)
	  {
	    option_data->brick_type[ibrick] = FUNC_FT_TYPE;

	    nopt++;
	    if (nopt >= argc)  RA_error ("need more arguments after -brick ");
	    strcpy (option_data->brick_label[ibrick], argv[nopt]);
	    
	  }
	else if (strncmp(argv[nopt], "rstat", 4) == 0)
	  {
	    option_data->brick_type[ibrick] = FUNC_THR_TYPE;

	    nopt++;
	    if (nopt >= argc)  RA_error ("need more arguments after -brick ");
	    strcpy (option_data->brick_label[ibrick], argv[nopt]);
	    
	  }
	else 
	  {
	    sprintf(message,"Unrecognized option after -brick: %s\n", 
		    argv[nopt]);
	    RA_error (message);
	  }
	  
	nopt++;
	continue;
      }


    /*----- unknown command -----*/
    sprintf(message,"Unrecognized command line option: %s\n", argv[nopt]);
    RA_error (message);
    
  }

  /*----- check for fewer than expected datasets -----*/
  if (irows < rows-1)
    RA_error ("Fewer than expected datasets were entered");
}


/*---------------------------------------------------------------------------*/
/*
  Count the number of output data volumes and output files required.
*/

void count_volumes_and_files 
(  
  model * regmodel,            /* regression model */
  RA_options * option_data     /* user input options */
)

{
  int ip;                  /* parameter index */
  int ix;                  /* x-variable index */
  int ib;                  /* sub-brick index */
  int p;                   /* number of parameters in the model */
  int nbricks;             /* number of bricks in bucket dataset */


  /*----- initialize local variables -----*/
  nbricks = option_data->numbricks;
  p = regmodel->p;


  /*----- count number of volumes and files for F-statistics -----*/
  for (ip = 0;  ip < p;  ip++)
    {
      ix = regmodel->flist[ip];

      if (option_data->fcoef_filename[ix] != NULL)
	{  
	  option_data->numf = 1;
	  option_data->numfiles += 1;
	}
    }

  for (ib = 0;  ib < nbricks;  ib++)
    if (option_data->brick_type[ib] == FUNC_FT_TYPE)  
      option_data->numf = 1; 


  /*----- count number of volumes and files for R^2 -----*/
  for (ip = 0;  ip < p;  ip++)
    {
      ix = regmodel->flist[ip];

      if (option_data->rcoef_filename[ix] != NULL)
	{
	  option_data->numr = 1;
	  option_data->numfiles += 1;
	}
    }

  for (ib = 0;  ib < nbricks;  ib++)
    if (option_data->brick_type[ib] == FUNC_THR_TYPE)  
      option_data->numr = 1; 
   

  /*----- count number of volumes and files for t-statistics -----*/
  for (ip = 0;  ip < p;  ip++)
    {
      ix = regmodel->flist[ip];

      if (option_data->tcoef_filename[ix] != NULL)
	{
	  option_data->numt += 1;
	  option_data->numfiles += 1;
	}

      else
	for (ib = 0;  ib < nbricks;  ib++)
	  if ((option_data->brick_type[ib] == FUNC_TT_TYPE)
	      && (option_data->brick_coef[ib] == ix))
	    option_data->numt += 1;   
    }

 
  /*----- count number of volumes for regression coefficients -----*/
  for (ip = 0;  ip < p;  ip++)
    {
      ix = regmodel->flist[ip];

      if (   (option_data->fcoef_filename[ix] != NULL) 
	  || (option_data->rcoef_filename[ix] != NULL)
	  || (option_data->tcoef_filename[ix] != NULL))
	option_data->numc += 1;   

      else
	for (ib = 0;  ib < nbricks;  ib++)
	  if ((option_data->brick_type[ib] == FUNC_FIM_TYPE)
	      && (option_data->brick_coef[ib] == ix))
	    option_data->numc += 1;   
    }

}


/*---------------------------------------------------------------------------*/
/*
  Break problem into smaller pieces.
*/

void break_into_pieces
(
  int num_datasets,            /* number of input datasets */
  RA_options * option_data     /* user input options */
)
 
{
  int num_vols;            /* number of output volumes */


  /*----- count number of distinct floating point volumes required  -----*/ 
  num_vols = option_data->numf + option_data->numr 
    + option_data->numt + option_data->numc;

  /*----- calculate number of voxels per piece -----*/
  option_data->piece_size = option_data->workmem * MEGA 
    / ((num_datasets + num_vols) * sizeof(float));
  if (option_data->piece_size > option_data->nxyz)  
    option_data->piece_size = option_data->nxyz;

  /*----- calculate number of pieces per dataset -----*/
  option_data->num_pieces = (option_data->nxyz + option_data->piece_size - 1) 
    / option_data->piece_size;

  printf ("num_pieces = %d    piece_size = %d \n", 
	  option_data->num_pieces, option_data->piece_size);    

}


/*---------------------------------------------------------------------------*/
/*
  Identify repeat observations.  (Repeat observations are necessary for
  performing the F-test for lack of fit.)  Convert -flof input from alpha to
  corresponding F value.   
*/

void identify_repeats 
(
  matrix * xdata,              /* independent variable matrix */
  model * regmodel,            /* linear regression model */
  RA_options * option_data     /* user input options */
)

{
  int i, k;                    /* matrix row indices */
  int j;                       /* matrix column index */
  int match;                   /* boolean for repeat observation found */

  int which;                   /* indicator for F-distribution calculation */
  double p, q;                 /* cumulative probabilities under F-dist. */
  double f;                    /* F value corresponding to probability p */
  double dfn, dfd;             /* numerator and denominator dof */
  int status;                  /* calculation status */
  double bound;                /* search bound */


  /*----- allocate memory -----*/
  option_data->levels = (int *) malloc (sizeof(int) * (xdata->rows));
  MTEST (option_data->levels);
  option_data->counts = (int *) malloc (sizeof(int) * (xdata->rows));
  MTEST (option_data->counts);


  /*----- initialization -----*/
  for (i = 0;  i < xdata->rows;  i++)
    option_data->counts[i] = 0;
  option_data->levels[0] = 0;
  option_data->counts[0] = 1;
  option_data->c = 1;
  

  /*----- loop over observations -----*/
  for (i = 1;  i < xdata->rows;  i++)
    {
      option_data->levels[i] = option_data->c;

      /*----- determine if this is a repeat observation -----*/
      for (k = 0;  k < i;  k++)
	{
	  match = 1;
	  for (j = 1;  j < xdata->cols;  j++)
	    if (xdata->elts[i][j] != xdata->elts[k][j])  match = 0;
	  
	  if (match)
	    {
	      option_data->levels[i] = option_data->levels[k];
	      break;
	    }
	}

      /*----- increment count of repeat observations -----*/
      k = option_data->levels[i];
      option_data->counts[k] ++;

      /*----- increment count of distinct observation levels -----*/
      if (k == option_data->c)	option_data->c++;
    }


  /*----- determine F value corresponding to alpha for lack of fit -----*/
  which = 2;
  q = option_data->flofmax;
  p = 1.0 - q;
  dfn = option_data->c - regmodel->p;
  dfd = xdata->rows - option_data->c;
  cdff (&which, &p, &q, &f, &dfn, &dfd, &status, &bound);
  if (status != 0)  RA_error ("Error in calculating F - lack of fit ");
  option_data->flofmax = f;

  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check for valid inputs.
*/

void check_for_valid_inputs 
(
  matrix * xdata,               /* independent variable matrix */
  model * regmodel,             /* regression model */
  RA_options * option_data      /* user input options */
)

{
  int ib;                       /* sub-brick index */


  /*----- check data set dimensions -----*/
  if (xdata->cols < 2)
    RA_error ("Too few X variables ");
  if (xdata->rows < 3) 
    RA_error ("Too few data sets for Y-observations ");

  /*----- check model dimensions -----*/
  if (regmodel->q < 1)
    RA_error ("Reduced regression model is too small");
  if (regmodel->p <= regmodel->q)
    RA_error ("Full regression model is too small");
  if (xdata->rows <= regmodel->p)
    RA_error ("Too few data sets for fitting full regression model ");
  if (regmodel->rlist[0] != 0)
    RA_error ("Reduced model must include variable index 0");


  /*----- check for repeat observations -----*/
  if (option_data->flofmax >= 0.0)
    {
      if (xdata->rows <= option_data->c)
	RA_error ("Cannot conduct F-test for lack of fit  (no repeat obs.) ");
      if (option_data->c <= regmodel->p)
	RA_error ("Cannot conduct F-test for lack of fit  (c <= p) ");
    }

  /*----- check bucket dataset parameters -----*/
  if (option_data->numbricks > 0)
    for (ib = 0;  ib < option_data->numbricks;  ib++)
      {
	if (option_data->brick_type[ib] < 0)
	  RA_error ("Must specify contents of each brick in the bucket");
      }
}


/*---------------------------------------------------------------------------*/
/*
  Check whether one output file already exists.
*/

void check_one_output_file 
(
  RA_options * option_data,     /* user input options */
  char * filename               /* output file name */
)

{
  THD_3dim_dataset * dset=NULL;       /* input afni data set pointer */
  THD_3dim_dataset * new_dset=NULL;   /* output afni data set pointer */
  int ierror;                         /* number of errors in editing data */
  
  
  /*----- read first dataset -----*/
  dset = THD_open_dataset (option_data->first_dataset ) ;
  CHECK_OPEN_ERROR(dset,option_data->first_dataset) ;
  
  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy( dset ) ;
  
  
  ierror = EDIT_dset_items( new_dset ,
			    ADN_prefix , filename ,
			    ADN_label1 , filename ,
			    ADN_directory_name , option_data->session ,
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
  THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check whether any output files already exist.
*/

void check_output_files 
(
  RA_options * option_data      /* user input options */
)

{
  int ix;       /* x-variable index */
  
  
  for (ix = 0;  ix < MAX_XVARS;  ix++)
    {
      if (option_data->fcoef_filename[ix] != NULL)
	check_one_output_file (option_data, option_data->fcoef_filename[ix]);
      if (option_data->rcoef_filename[ix] != NULL)
	check_one_output_file (option_data, option_data->rcoef_filename[ix]);
      if (option_data->tcoef_filename[ix] != NULL)
	check_one_output_file (option_data, option_data->tcoef_filename[ix]);
    }

  if (option_data->bucket_filename != NULL)
    check_one_output_file (option_data, option_data->bucket_filename);
  
}


/*---------------------------------------------------------------------------*/
/*
  Check whether any temporary data files already exist.
*/

void check_temporary_files
(
  matrix * xdata,              /* independent variable matrix */
  model * regmodel,            /* linear regression model */
  RA_options * option_data     /* user input options */
)

{
  int p;                       /* number of parameters in full model */
  int ip, ix;                  /* parameter index */ 
  int num_pieces;              /* dataset is divided into this many pieces */
  char filename[MAX_NAME_LENGTH];        /* name for temporary data file */ 


  /*----- initialize local variables -----*/
  p = regmodel->p;
  num_pieces = option_data->num_pieces;


  /*----- check for F-statistics data file -----*/
  if (option_data->numf > 0)
    {
      strcpy (filename, "freg");
      check_volume (filename, num_pieces);
    }


  /*----- check for R^2 data file -----*/
  if (option_data->numr > 0)
    {
      strcpy (filename, "rsqr");
      check_volume (filename, num_pieces);
    }

	  
  /*----- check for t-statistics data files -----*/
  if (option_data->numt > 0)
    {
      for (ip = 0;  ip < p;  ip++)
	{
	  ix = regmodel->flist[ip];
	  
	  if (option_data->tcoef_filename[ix] != NULL)
	    {
	      sprintf (filename, "tcoef.%d", ix);
	      check_volume (filename, num_pieces);
	    }
	}
    }


  /*----- check for regression coefficients data files -----*/
  if (option_data->numc > 0)
    {
      for (ip = 0;  ip < p;  ip++)
	{
	  ix = regmodel->flist[ip];
	  
	  if (    (option_data->fcoef_filename[ix] != NULL)
	       || (option_data->rcoef_filename[ix] != NULL)
	       || (option_data->tcoef_filename[ix] != NULL) )
	    {
	      sprintf (filename, "coef.%d", ix);
	      check_volume (filename, num_pieces);
	    }
	}
    }


}


/*---------------------------------------------------------------------------*/
/*
  Routine to determine if sufficient disk space exists for storing
  the temporary data files.
*/

void check_disk_space 
(
  RA_options * option_data      /* user input options */
)

{
  char ch;                         /* user response */
  int nxyz;                        /* number of voxels per image */
  int nmax;                        /* maximum number of disk files */
  char filename[MAX_NAME_LENGTH];  /* output file name */ 


  /*----- initialize local variables -----*/
  nxyz = option_data->nxyz;

  /*----- first, determine space required for temporary volume data -----*/
  nmax = 4 * nxyz * (option_data->numf + option_data->numr + option_data->numt 
    + option_data->numc);

  /*----- determine additional space required for output files -----*/
  nmax += 4 * nxyz * option_data->numfiles;

  printf("\nThis problem requires approximately %d MB of free disk space.\n", 
	  (nmax/1000000) + 1);


  /*----- Determine how much disk space is available. -----*/
  printf ("Summary of available disk space: \n\n");
  system (DF);
  printf ("\nDo you wish to proceed? ");
  ch = getchar();
  if ((ch != 'Y') && (ch != 'y')) exit(0);
  printf ("\n");
}


/*---------------------------------------------------------------------------*/
/*
  Program initialization.
*/

void initialize_program 
(
  int argc,                    /* number of input arguments */
  char ** argv,                /* array of input arguments */ 
  matrix * xdata,              /* independent variable matrix */
  model * regmodel,            /* regression model */
  RA_options * option_data     /* user input options */
)

{

  /*----- save command line for history notes -----*/
  commandline = tross_commandline( PROGRAM_NAME , argc,argv ) ;


  /*----- create independent variable data matrix -----*/
  matrix_initialize (xdata);


  /*----- get all operator inputs -----*/
  get_inputs (argc, argv, xdata, regmodel, option_data);


  /*----- use first data set to get data set dimensions -----*/
  option_data->first_dataset = option_data->yname[0];
  get_dimensions (option_data);
  printf ("Data set dimensions:  nx = %d  ny = %d  nz = %d  nxyz = %d \n",
	 option_data->nx, option_data->ny, option_data->nz, option_data->nxyz);
 

  /*----- count the number of output volumes and files required -----*/
  count_volumes_and_files (regmodel, option_data);


  /*----- break problem into smaller pieces -----*/
  break_into_pieces (xdata->rows, option_data);


  /*----- identify repeat observations -----*/
  if (option_data->flofmax >= 0.0)  
    identify_repeats (xdata, regmodel, option_data);
  

  /*----- check for valid inputs -----*/
  check_for_valid_inputs (xdata, regmodel, option_data);

   
  /*----- check whether any of the output files already exist -----*/
  if( THD_deathcon() ) check_output_files (option_data); 
 

  /*----- check whether temporary files already exist  -----*/
  check_temporary_files (xdata, regmodel, option_data);


  /*----- check whether there is sufficient disk space -----*/
  if (option_data->diskspace)  check_disk_space (option_data);

}


/*---------------------------------------------------------------------------*/
/*
  Perform initialization required for the regression analysis.
*/

void init_regression_analysis 
(
  int p,                        /* number of parameters in the full model */
  int q,                        /* number of parameters in the rdcd model */
  int * flist,                  /* list of parameters in the full model */
  int * rlist,                  /* list of parameters in the rdcd model */
  matrix xdata,                 /* independent variable matrix */
  matrix * x_full,              /* extracted X matrix    for full model */
  matrix * xtxinv_full,         /* matrix:  1/(X'X)      for full model */
  matrix * xtxinvxt_full,       /* matrix:  (1/(X'X))X'  for full model */
  matrix * x_base,              /* extracted X matrix    for baseline model */
  matrix * xtxinvxt_base,       /* matrix:  (1/(X'X))X'  for baseline model */
  matrix * x_rdcd,              /* extracted X matrices  for reduced models */
  matrix * xtxinvxt_rdcd        /* matrix:  (1/(X'X))X'  for reduced models */
)

{
  int zlist[MAX_XVARS];         /* list of parameters in constant model */
  int ip;                       /* parameter index */
  matrix xtxinv_temp;           /* intermediate results */


  /*----- Initialize matrix -----*/
  matrix_initialize (&xtxinv_temp);

  
  /*----- Initialize list of parameters in the constant model -----*/
  for (ip = 0;  ip < MAX_XVARS;  ip++)
    zlist[ip] = 0;


  /*----- Calculate constant matrices which will be needed later -----*/
  calc_matrices (xdata, 1, zlist, x_base, &xtxinv_temp, xtxinvxt_base);
  calc_matrices (xdata, q, rlist, x_rdcd, &xtxinv_temp, xtxinvxt_rdcd);
  calc_matrices (xdata, p, flist, x_full, xtxinv_full, xtxinvxt_full);


  /*----- Destroy matrix -----*/
  matrix_destroy (&xtxinv_temp);

}


/*---------------------------------------------------------------------------*/
/*
  Perform the entire regression analysis for one voxel.
*/

void regression_analysis 
(
  int N,                      /* number of data points */
  int p,                      /* number of parameters in the full model */
  int q,                      /* number of parameters in the rdcd model */
  matrix x_full,              /* extracted X matrix    for full model */
  matrix xtxinv_full,         /* matrix:  1/(X'X)      for full model */
  matrix xtxinvxt_full,       /* matrix:  (1/(X'X))X'  for full model */
  matrix x_base,              /* extracted X matrix    for baseline model */
  matrix xtxinvxt_base,       /* matrix:  (1/(X'X))X'  for baseline model */
  matrix x_rdcd,              /* extracted X matrix    for reduced model */
  matrix xtxinvxt_rdcd,       /* matrix:  (1/(X'X))X'  for reduced model */
  vector y,                   /* vector of measured data */ 
  float rms_min,              /* minimum rms error to reject zero model */
  int * levels,               /* indices for repeat observations */
  int * counts,               /* number of observations at each level */
  int c,                      /* number of unique rows in ind. var. matrix */
  float flofmax,              /* max. allowed F-stat due to lack of fit */  
  float * flof,               /* F-statistic for lack of fit */
  vector * coef_full,         /* regression parameters */
  vector * scoef_full,        /* std. devs. for regression parameters */
  vector * tcoef_full,        /* t-statistics for regression parameters */
  float * freg,               /* F-statistic for the full regression model */
  float * rsqr                /* coeff. of multiple determination R^2  */
)

{
  float sse_base;             /* error sum of squares, baseline model */
  float sse_rdcd;             /* error sum of squares, reduced model */
  float sse_full;             /* error sum of squares, full model */
  float sspe;                 /* pure error sum of squares */
  vector coef_temp;           /* intermediate results */


  /*----- Initialization -----*/
  vector_initialize (&coef_temp);


  /*----- Calculate regression coefficients for baseline model -----*/
  calc_coef (xtxinvxt_base, y, &coef_temp);


  /*----- Calculate the error sum of squares for the baseline model -----*/ 
  sse_base = calc_sse (x_base, coef_temp, y);

  
  /*----- Stop here if variation about baseline is sufficiently low -----*/
  if (sqrt(sse_base/N) < rms_min)
    {   
      vector_create (p, coef_full);
      vector_create (p, scoef_full);
      vector_create (p, tcoef_full);
      *freg = 0.0;
      *rsqr = 0.0;
      vector_destroy (&coef_temp);
      return;
    }


  /*----- Calculate regression coefficients for the full model  -----*/
  calc_coef (xtxinvxt_full, y, coef_full);
  
  
  /*----- Calculate the error sum of squares for the full model -----*/ 
  sse_full = calc_sse (x_full, *coef_full, y);
  
  
  /*----- Calculate t-statistics for the regression coefficients -----*/
  calc_tcoef (N, p, sse_full, xtxinv_full, 
	      *coef_full, scoef_full, tcoef_full);


  /*----- Test for lack of fit -----*/
  if (flofmax > 0.0)
    {
      /*----- Calculate the pure error sum of squares -----*/
      sspe = calc_sspe (y, levels, counts, c);
    
      /*----- Calculate F-statistic for lack of fit -----*/
      *flof = calc_flof (N, p, c, sse_full, sspe);
    
      if (*flof > flofmax) 
	{   
	  vector_create (p, coef_full);
	  vector_create (p, scoef_full);
	  vector_create (p, tcoef_full);
	  *freg = 0.0;
	  *rsqr = 0.0;
	  vector_destroy (&coef_temp);
	  return;
	} 
    }
  else
    *flof = -1.0;


  /*----- Calculate regression coefficients for reduced model -----*/
  calc_coef (xtxinvxt_rdcd, y, &coef_temp);
  
  
  /*----- Calculate the error sum of squares for the reduced model -----*/ 
  sse_rdcd = calc_sse (x_rdcd, coef_temp, y);

  
  /*----- Calculate F-statistic for significance of the regression -----*/
  *freg = calc_freg (N, p, q, sse_full, sse_rdcd);


  /*----- Calculate coefficient of multiple determination R^2 -----*/
  *rsqr = calc_rsqr (sse_full, sse_base);


  /*----- Dispose of vector -----*/
  vector_destroy (&coef_temp);
   
}


/*---------------------------------------------------------------------------*/
/*
  Save results for current voxel into piece data for output later.
*/

void save_voxel 
(
  int iv,               /* current voxel number within piece */
  vector y,             /* vector of measured data */       
  float fdisp,          /* minimum F-statistic for display */
  model * regmodel,     /* linear regression model */
  float flof,           /* F-statistic for lack of fit */
  vector coef,          /* regression parameters */
  vector tcoef,         /* t-statistics for regression parameters */
  float freg,           /* F-statistic for the full regression model */
  float rsqr,           /* coeff. of multiple determination R^2  */

  float * freg_piece,     /* piece F-statistic for the full regression model */
  float * rsqr_piece,     /* piece coeff. of multiple determination R^2  */
  float ** coef_piece,    /* piece regression parameters */
  float ** tcoef_piece    /* piece t-statistics for regression parameters */
)

{
  int ip;                 /* parameter index */
  int ix;                 /* x-variable index */
  

  /*----- save regression results into piece data -----*/ 
  if (freg_piece != NULL)  freg_piece[iv] = freg;
  if (rsqr_piece != NULL)  rsqr_piece[iv] = rsqr;
  

  /*----- save regression parameter estimates -----*/  
  for (ip = 0;  ip < regmodel->p;  ip++)
    {
      ix = regmodel->flist[ip];
                
      if (coef_piece[ix] != NULL)  coef_piece[ix][iv] = coef.elts[ip];
                       
      if (tcoef_piece[ix] != NULL)  tcoef_piece[ix][iv] = tcoef.elts[ip];
      
    }
    

  /*----- if so requested, display results for this voxel -----*/
  if ((fdisp >= 0.0) && (freg >= fdisp))
    {
      printf ("\n\nVoxel #%d:  \n", iv);
      printf ("\nY data: \n");
      for (ip = 0;  ip < y.dim;  ip++)
	printf ("Y[%d] = %f \n", ip, y.elts[ip]);

      if (flof >= 0.0)	printf ("\nF lack of fit = %f \n", flof);
      printf ("\nF regression  = %f \n", freg);
      printf ("R-squared     = %f \n", rsqr);

      printf ("\nRegression Coefficients: \n");
      for (ip = 0;  ip < coef.dim;  ip++)
	{
	  ix = regmodel->flist[ip];
	  printf ("b[%d] = %f   ", ix, coef.elts[ip]);
	  printf ("t[%d] = %f \n", ix, tcoef.elts[ip]);
	}

    }

}

/*---------------------------------------------------------------------------*/
/*
  Calculate the multiple linear regression analysis for all voxels  
  (by breaking the datasets into smaller pieces, if necessary).
*/

void calculate_results 
(
  matrix xdata,               /* independent variable matrix */
  model * regmodel,           /* linear regression model */
  RA_options * option_data    /* user input options */
)

{
  int * flist = NULL;         /* list of parameters in the full model */
  int * rlist = NULL;         /* list of parameters in the rdcd model */
  int n;                      /* number of data points */
  int p;                      /* number of parameters in the full model */
  int q;                      /* number of parameters in the rdcd model */
  float flof;                 /* F-statistic for lack of fit */
  float freg;                 /* F-statistic for the full regression model */
  float rsqr;                 /* coeff. of multiple determination R^2  */
  vector coef;                /* regression parameters */
  vector scoef;               /* std. devs. for regression parameters */
  vector tcoef;               /* t-statistics for regression parameters */

  matrix x_full;              /* extracted X matrix    for full model */
  matrix xtxinv_full;         /* matrix:  1/(X'X)      for full model */
  matrix xtxinvxt_full;       /* matrix:  (1/(X'X))X'  for full model */
  matrix x_base;              /* extracted X matrix    for baseline model */
  matrix xtxinvxt_base;       /* matrix:  (1/(X'X))X'  for baseline model */
  matrix x_rdcd;              /* extracted X matrix    for reduced model */
  matrix xtxinvxt_rdcd;       /* matrix:  (1/(X'X))X'  for reduced model */
  vector y;                   /* vector of measured data */       

  int i;                      /* dataset index */
  int nxyz;                   /* number of voxels per dataset */
  int num_datasets;           /* total number of datasets */
  int piece_size;             /* number of voxels in dataset piece */
  int num_pieces;             /* dataset is divided into this many pieces */
  int piece;                  /* piece index */
  int piece_len;              /* number of voxels in current piece */
  int fim_offset;             /* array offset to current piece */
  int ivox;                   /* index of voxel in current piece */
  int nvox;                   /* index of voxel within entire volume */

  float ** yfimar = NULL;            /* array of pieces of Y-datasets */
  float * freg_piece = NULL;         /* piece for F-statistics */
  float * rsqr_piece = NULL;         /* piece for R^2  */
  float ** coef_piece = NULL;        /* pieces for regression coefficients */
  float ** tcoef_piece = NULL;       /* pieces for t-statistics */


  /*----- Initialize matrices and vectors -----*/
  matrix_initialize (&x_full);
  matrix_initialize (&xtxinv_full);
  matrix_initialize (&xtxinvxt_full);
  matrix_initialize (&x_base);
  matrix_initialize (&xtxinvxt_base);
  matrix_initialize (&x_rdcd);
  matrix_initialize (&xtxinvxt_rdcd);
  vector_initialize (&coef);
  vector_initialize (&scoef);
  vector_initialize (&tcoef);
  vector_initialize (&y);


  /*----- initialize local variables -----*/
  n = xdata.rows;
  p = regmodel->p;
  flist = regmodel->flist;
  q = regmodel->q;
  rlist = regmodel->rlist;
  nxyz = option_data->nxyz;
  piece_size = option_data->piece_size;
  num_pieces = option_data->num_pieces;
  

  /*----- allocate memory space for pieces -----*/
  allocate_pieces (xdata, regmodel, option_data, &yfimar, 
		  &freg_piece, &rsqr_piece, &coef_piece, &tcoef_piece);


  /*----- initialization for the regression analysis -----*/
  init_regression_analysis (p, q, flist, rlist, xdata,
			    &x_full, &xtxinv_full, &xtxinvxt_full, 
			    &x_base, &xtxinvxt_base, &x_rdcd, &xtxinvxt_rdcd);


  vector_create (n, &y);


  if (option_data->fdisp >= 0)
    {
      printf ("\n");
      printf ("X matrix: \n");
      matrix_print (xdata);
    }

  
  /*----- loop over the pieces of the input datasets -----*/
  nvox = -1;
  for (piece = 0;  piece < num_pieces;  piece++)
    {
      printf ("piece = %d \n", piece);
      fim_offset = piece * piece_size;
      if (piece < num_pieces-1)
	piece_len = piece_size;
      else
	piece_len = nxyz - fim_offset;

      /*----- read in the Y-data -----*/
      for (i = 0;  i < n;  i++)
	read_afni_data (option_data, option_data->yname[i],
			piece_len, fim_offset, yfimar[i]);


      /*----- loop over voxels in this piece -----*/
      for (ivox = 0;  ivox < piece_len;  ivox++)
	{
	  nvox += 1;


	  /*----- extract Y-data for this voxel -----*/
	  for (i = 0;  i < n;  i++)
	    y.elts[i] = yfimar[i][ivox];
     

	  /*----- calculate results for this voxel -----*/
	  regression_analysis (n, p, q,  
			       x_full, xtxinv_full, xtxinvxt_full, x_base,
			       xtxinvxt_base, x_rdcd, xtxinvxt_rdcd, 
			       y, option_data->rms_min, option_data->levels, 
			       option_data->counts, option_data->c, 
			       option_data->flofmax, &flof,
			       &coef, &scoef, &tcoef, &freg, &rsqr);
    

	  /*----- save results for this voxel -----*/
	  save_voxel (ivox, y, option_data->fdisp,
		      regmodel, flof, coef, tcoef, freg, rsqr,
		      freg_piece, rsqr_piece, coef_piece, tcoef_piece);
		       

	}  /* loop over voxels within this piece */


      /*----- save piece data into external files -----*/
      save_pieces (piece, piece_len,
		  freg_piece, rsqr_piece, coef_piece, tcoef_piece);

	  
    }  /* loop over pieces */


  /*----- deallocate memory for pieces -----*/
  deallocate_pieces (n, &yfimar, &freg_piece, &rsqr_piece, 
		    &coef_piece, &tcoef_piece);


  /*----- Dispose of matrices -----*/
  vector_destroy (&y);
  vector_destroy (&tcoef);
  vector_destroy (&scoef);
  vector_destroy (&coef);
  matrix_destroy (&xtxinvxt_rdcd);
  matrix_destroy (&x_rdcd);
  matrix_destroy (&xtxinvxt_base);
  matrix_destroy (&x_base);
  matrix_destroy (&xtxinvxt_full);
  matrix_destroy (&xtxinv_full); 
  matrix_destroy (&x_full); 

}

/*---------------------------------------------------------------------------*/
/*
  Routine to write one AFNI data set.
  
  This data set may be either  `fith' type (intensity + threshold)
                           or  `fitt' type (intensity + t-statistic)
                           or  `fift' type (intensity + F-statistic).
  
  The intensity data is in ffim, and the corresponding statistic is in ftr.
  
*/

void write_afni_data 
(
  RA_options * option_data,           /* user input options */
  char * filename,                    /* output file name */
  float * ffim,                       /* volume of intensity data */
  float * ftr,                        /* volume of test statistics */
  int func_type,                      /* afni data set type */  
  int numdof,                         /* numerator degrees of freedom */
  int dendof                          /* denominator degrees of freedom */
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
  short * tsp = NULL;                 /* 2nd sub-brick data pointer */
  void  * vdif = NULL;                /* 1st sub-brick data pointer */
  float top, bot, func_scale_short;   /* parameters for scaling data */
  int top_ss, bot_ss;                 /* 2nd sub-brick value limits */
  char label[80];                     /* label for output file history */ 
  
  
  /*----- initialize local variables -----*/
  nxyz = option_data->nxyz;
  
  /*----- read first dataset -----*/
  dset = THD_open_dataset (option_data->first_dataset) ;
  CHECK_OPEN_ERROR(dset,option_data->first_dataset) ;
  

  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy( dset ) ;
  
  
  /*----- Record history of dataset -----*/

  sprintf (label, "Output prefix: %s", filename);
  if( commandline != NULL )
     tross_multi_Append_History( new_dset , commandline,label,NULL ) ;
  else
     tross_Append_History ( new_dset, label);
  

  output_datum = MRI_short ;
  ibuf[0] = output_datum ; ibuf[1] = MRI_short ;
  
  
  ierror = EDIT_dset_items( new_dset ,
			    ADN_prefix , filename ,
			    ADN_label1 , filename ,
			    ADN_directory_name , option_data->session ,
			    ADN_self_name , filename ,
			    ADN_type , ISHEAD(dset) ? HEAD_FUNC_TYPE : 
			                              GEN_FUNC_TYPE ,
			    ADN_func_type , func_type ,
			    ADN_nvals , FUNC_nvals[func_type] ,
			    ADN_datum_array , ibuf ,
			    ADN_malloc_type, DATABLOCK_MEM_MALLOC ,  
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
  
  /*----- deleting exemplar dataset -----*/ 
  THD_delete_3dim_dataset( dset , False ) ; dset = NULL ;
  
  
  /*----- allocate memory for output data -----*/
  vdif = (void *)  malloc( mri_datum_size(output_datum) * nxyz ) ;
  tsp  = (short *) malloc( sizeof(short) * nxyz )                ;
  
  /*----- attach bricks to new data set -----*/
  mri_fix_data_pointer (vdif, DSET_BRICK(new_dset,0)); 
  mri_fix_data_pointer (tsp, DSET_BRICK(new_dset,1));  
  
  
  /*----- convert data type to output specification -----*/
  fimfac = EDIT_coerce_autoscale_new (nxyz, 
				      MRI_float, ffim, 
				      output_datum, vdif);
  if (fimfac != 0.0)  fimfac = 1.0 / fimfac;
  

  top_ss = 32700;

  if (func_type == FUNC_THR_TYPE)               /* threshold */
    {
      func_scale_short = FUNC_THR_SCALE_SHORT;
      bot_ss =  0;
    }
  else if (func_type == FUNC_TT_TYPE)           /* t-statistic */
    { 
      func_scale_short = FUNC_TT_SCALE_SHORT;
      bot_ss =  -top_ss;
    }
  else if (func_type == FUNC_FT_TYPE)           /* F-statistic */
    {
      func_scale_short = FUNC_FT_SCALE_SHORT;
      bot_ss =  0;
    }
  else
    RA_error ("Illegal ouput dataset function type");
  
  top = top_ss / func_scale_short;
  bot = bot_ss / func_scale_short;


  for (ii = 0;  ii < nxyz;  ii++)
    {
      if (ftr[ii] > top)
	tsp[ii] = top_ss;
      else  if (ftr[ii] < bot)
	tsp[ii] = bot_ss;
      else 
	tsp[ii] = (short) (func_scale_short * ftr[ii] + 0.5);
    }
  

  /*----- write afni data set -----*/
  if (func_type == FUNC_THR_TYPE)               /* threshold */
    printf("++ Writing `fith' dataset ");
  else if (func_type == FUNC_TT_TYPE)           /* t-statistic */
    printf("++ Writing `fitt' dataset ");
  else if (func_type == FUNC_FT_TYPE)           /* F-statistic */
    printf("++ Writing `fift' dataset ");

  printf("into %s\n", DSET_BRIKNAME(new_dset) ) ;
  
  fbuf[0] = numdof;   
  fbuf[1] = dendof;
  for( ii=2 ; ii < MAX_STAT_AUX ; ii++ ) fbuf[ii] = 0.0 ;
  (void) EDIT_dset_items( new_dset , ADN_stat_aux , fbuf , ADN_none ) ;
  
  fbuf[0] = (output_datum == MRI_short && fimfac != 1.0 ) ? fimfac : 0.0 ;
  fbuf[1] = 1.0 / func_scale_short ;
  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;

  if( !AFNI_noenv("AFNI_AUTOMATIC_FDR") )
  { int ii = THD_create_all_fdrcurves( new_dset ) ;
    if( ii > 0 ) ININFO_message("created %d FDR curves in header",ii) ;
  }
  
  THD_load_statistics( new_dset ) ;
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;

  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to write one bucket data set.
*/

void write_bucket_data 
(
  matrix xdata,                /* independent variable matrix */
  model * regmodel,            /* linear regression model */
  RA_options * option_data     /* user input options */
)

{
  const float EPSILON = 1.0e-10;
  int p;                    /* number of parameters in full model */
  int q;                    /* number of parameters in reduced model */
  int n;                    /* number of data points */
  int nxyz;                 /* number of voxels */
  THD_3dim_dataset * old_dset = NULL;    /* prototype dataset */
  THD_3dim_dataset * new_dset = NULL;    /* output bucket dataset */
  char * output_prefix;     /* prefix name for bucket dataset */
  char * output_session;    /* directory for bucket dataset */
  int nbricks, ib;          /* number of sub-bricks in bucket dataset */
  short ** bar = NULL;      /* bar[ib] points to data for sub-brick #ib */
  float ** far = NULL;
  float factor;             /* factor is new scale factor for sub-brick #ib */
  int brick_type;           /* indicates statistical type of sub-brick */
  int brick_coef;           /* regression coefficient index for sub-brick */
  char * brick_label;       /* character string label for sub-brick */
  int ierror;               /* number of errors in editing data */
  char filename[MAX_NAME_LENGTH];        /* name for temporary data file */ 
  int piece_size;           /* number of voxels in dataset piece */
  int num_pieces;           /* dataset is divided into this many pieces */
  float * volume = NULL;    /* volume of floating point data */
  char label[80];           /* label for output file history */ 
    
  /*----- initialize local variables -----*/
  p = regmodel->p;
  q = regmodel->q;
  n = xdata.rows;
  nxyz = option_data->nxyz; 
  piece_size = option_data->piece_size;
  num_pieces = option_data->num_pieces;
  nbricks = option_data->numbricks;
  output_prefix = option_data->bucket_filename;
  output_session = option_data->session;
  

  /*----- allocate memory -----*/
  volume = (float *) malloc (sizeof(float) * nxyz);
  MTEST (volume);
  if (option_data->datum == MRI_float) {
     far  = (float **) malloc (sizeof(float *) * nbricks);
     MTEST (far);
   } else  {
    bar  = (short **) malloc (sizeof(short *) * nbricks);
    MTEST (bar);
   }
 
  /*----- read first dataset -----*/
  old_dset = THD_open_dataset (option_data->first_dataset) ;
  

  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy (old_dset);
  
  
  /*----- Record history of dataset -----*/
  if( commandline != NULL )
     tross_Append_History( new_dset , commandline ) ;
  sprintf (label, "Output prefix: %s", output_prefix);
  tross_Append_History ( new_dset, label);


  /*----- Modify some structural properties.  Note that the nbricks
          just make empty sub-bricks, without any data attached. -----*/
  ierror = EDIT_dset_items (new_dset,
                            ADN_prefix,          output_prefix,
			    ADN_directory_name,  output_session,
			    ADN_type,            HEAD_FUNC_TYPE,
			    ADN_func_type,       FUNC_BUCK_TYPE,
                            ADN_ntt,             0,               /* no time */
			    ADN_nvals,           nbricks,
			    ADN_malloc_type,     DATABLOCK_MEM_MALLOC ,  
			    ADN_none ) ;
  
  if( ierror > 0 )
    {
      fprintf(stderr, 
	      "*** %d errors in attempting to create output dataset!\n", 
	      ierror);
      exit(1);
    }
  
  if (THD_is_file(DSET_HEADNAME(new_dset))) 
    {
      fprintf(stderr,
	      "*** Output dataset file %s already exists--cannot continue!\n",
	      DSET_HEADNAME(new_dset));
      exit(1);
    }
  

  /*----- deleting exemplar dataset -----*/ 
  THD_delete_3dim_dataset( old_dset , False );  old_dset = NULL ;
  

  /*----- loop over new sub-brick index, attach data array with 
          EDIT_substitute_brick then put some strings into the labels and 
          keywords, and modify the sub-brick scaling factor -----*/
  for (ib = 0;  ib < nbricks;  ib++)
    {
      /*----- get information about this sub-brick -----*/
      brick_type  = option_data->brick_type[ib];
      brick_coef  = option_data->brick_coef[ib];
      brick_label = option_data->brick_label[ib];

      if (brick_type == FUNC_FIM_TYPE)
	{	
	  sprintf (filename, "coef.%d", brick_coef);
	}
      else  if (brick_type == FUNC_THR_TYPE)
	{
	  sprintf (filename, "rsqr");
	}
      else  if (brick_type == FUNC_TT_TYPE)
	{
	  sprintf (filename, "tcoef.%d", brick_coef);
	  EDIT_BRICK_TO_FITT (new_dset, ib, n-p);
	}
      else  if (brick_type == FUNC_FT_TYPE)
	{
	  sprintf (filename, "freg");
	  EDIT_BRICK_TO_FIFT (new_dset, ib, p-q, n-p);
	}

      /*----- allocate memory for output sub-brick -----*/
      read_volume (filename, volume, nxyz, piece_size, num_pieces);
      delete_volume (filename, nxyz, piece_size, num_pieces);

      if (option_data->datum == MRI_float) {
         far[ib]  = (float *) malloc (sizeof(float) * nxyz);
         MTEST (far[ib]);
         memcpy((void *)far[ib], (void *)volume, sizeof(float) * nxyz);
         /*----- attach far[ib] to be sub-brick #ib -----*/
         EDIT_substitute_brick (new_dset, ib, MRI_float, far[ib]);
      } else {
         bar[ib]  = (short *) malloc (sizeof(short) * nxyz);
         MTEST (bar[ib]);
         factor = EDIT_coerce_autoscale_new (nxyz, MRI_float, volume,
					     MRI_short, bar[ib]);

         if (factor < EPSILON)  factor = 0.0;
         else factor = 1.0 / factor;
         EDIT_BRICK_FACTOR (new_dset, ib, factor);
         /*----- attach bar[ib] to be sub-brick #ib -----*/
         EDIT_substitute_brick (new_dset, ib, MRI_short, bar[ib]);
      }

      /*----- edit the sub-brick -----*/
      EDIT_BRICK_LABEL (new_dset, ib, brick_label);

      

    }


  /*----- write bucket data set -----*/

  INFO_message("Writing bucket dataset: %s", DSET_BRIKNAME(new_dset)) ;

  if( !AFNI_noenv("AFNI_AUTOMATIC_FDR") )
  { int ii = THD_create_all_fdrcurves( new_dset ) ;
    if( ii > 0 ) ININFO_message("created %d FDR curves in header",ii) ;
  }

  THD_load_statistics (new_dset);
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;

  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  free (volume);   volume = NULL;

}


/*---------------------------------------------------------------------------*/
/*
  Write out the user requested output files.
*/

void output_results 
(
  matrix xdata,                /* independent variable matrix */
  model * regmodel,            /* linear regression model */
  RA_options * option_data     /* user input options */
)

{
  int p;                    /* number of parameters in full model */
  int q;                    /* number of parameters in reduced model */
  int n;                    /* number of data points */
  int nxyz;                 /* number of voxels */
  int ip, ix;               /* parameter index */ 
  int numdof, dendof;       /* numerator and denominator degrees of freedom */
  int piece_size;           /* number of voxels in dataset piece */
  int num_pieces;           /* dataset is divided into this many pieces */
  float * volume1 = NULL;   /* volume data for 1st sub-brick */
  float * volume2 = NULL;   /* volume data for 2nd sub-brick */
  char filename[MAX_NAME_LENGTH];        /* name for temporary data file */ 


  /*----- initialize local variables -----*/
  p = regmodel->p;
  q = regmodel->q;
  n = xdata.rows;
  nxyz = option_data->nxyz; 
  piece_size = option_data->piece_size;
  num_pieces = option_data->num_pieces;


  /*----- allocate memory space for volume data -----*/
  volume1 = (float *) malloc (sizeof(float) * nxyz);
  MTEST (volume1);
  volume2 = (float *) malloc (sizeof(float) * nxyz);
  MTEST (volume2);


  /*----- write t-statistics data files -----*/
  if (option_data->numt > 0)
    {
      numdof = n - p;
      dendof = 0;
      
      for (ip = 0;  ip < p;  ip++)
	{
	  ix = regmodel->flist[ip];
	  
	  if (option_data->tcoef_filename[ix] != NULL)
	    {
	      sprintf (filename, "coef.%d", ix);
	      read_volume (filename, volume1, nxyz, piece_size, num_pieces);

	      sprintf (filename, "tcoef.%d", ix);
	      read_volume (filename, volume2, nxyz, piece_size, num_pieces);

	      write_afni_data (option_data, 
			       option_data->tcoef_filename[ix], 
			       volume1, volume2, 
			       FUNC_TT_TYPE, numdof, dendof); 
	    }
	}
    }


  /*----- write R^2 data files -----*/
  if (option_data->numr > 0)
    {
      strcpy (filename, "rsqr");
      read_volume (filename, volume2, nxyz, piece_size, num_pieces);
      
      for (ip = 0;  ip < p;  ip++)
	{
	  ix = regmodel->flist[ip];
	  
	  if (option_data->rcoef_filename[ix] != NULL)
	    {
	      sprintf (filename, "coef.%d", ix);
	      read_volume (filename, volume1, nxyz, piece_size, num_pieces);
	      
	      write_afni_data (option_data, 
			       option_data->rcoef_filename[ix], 
			       volume1, volume2,
			       FUNC_THR_TYPE, 0, 0); 
	    }
	}
    }

	  
  /*----- write F-statistics data files -----*/
  if (option_data->numf > 0)
    {
      strcpy (filename, "freg");
      read_volume (filename, volume2, nxyz, piece_size, num_pieces);
      
      numdof = p - q;
      dendof = n - p;
      
      for (ip = 0;  ip < p;  ip++)
	{
	  ix = regmodel->flist[ip];
	  
	  if (option_data->fcoef_filename[ix] != NULL)
	    {
	      sprintf (filename, "coef.%d", ix);
	      read_volume (filename, volume1, nxyz, piece_size, num_pieces);
	      
	      write_afni_data (option_data, 
			       option_data->fcoef_filename[ix], 
			       volume1, volume2, 
			       FUNC_FT_TYPE, numdof, dendof); 
	    }
	}
    }


  /*----- deallocate memory space for volume data -----*/
  free (volume1);   volume1 = NULL;
  free (volume2);   volume2 = NULL;  


  /*----- write the bucket dataset -----*/
  if (option_data->numbricks > 0)
    write_bucket_data (xdata, regmodel, option_data);


}


/*---------------------------------------------------------------------------*/
/*
  Deallocate memory and delete any remaining temporary data files.
*/

void terminate_program
(
  matrix * xdata,              /* independent variable matrix */
  model * regmodel,            /* linear regression model */
  RA_options * option_data     /* user input options */
)

{
  int p;                       /* number of parameters in full model */
  int ip, ix;                  /* parameter index */ 
  int ib;                      /* sub-brick index */
  int nxyz;                    /* number of voxels */
  int piece_size;              /* number of voxels in dataset piece */
  int num_pieces;              /* dataset is divided into this many pieces */
  char filename[MAX_NAME_LENGTH];        /* name for temporary data file */ 


  /*----- initialize local variables -----*/
  p = regmodel->p;
  nxyz = option_data->nxyz; 
  piece_size = option_data->piece_size;
  num_pieces = option_data->num_pieces;


  /*----- delete F-statistics data files -----*/
  if (option_data->numf > 0)
    {
      strcpy (filename, "freg");
      delete_volume (filename, nxyz, piece_size, num_pieces);
    }


  /*----- delete R^2 data files -----*/
  if (option_data->numr > 0)
    {
      strcpy (filename, "rsqr");
      delete_volume (filename, nxyz, piece_size, num_pieces);
    }

	  
  /*----- delete t-statistics data files -----*/
  if (option_data->numt > 0)
    {
      for (ip = 0;  ip < p;  ip++)
	{
	  ix = regmodel->flist[ip];
	  sprintf (filename, "tcoef.%d", ix);
	  delete_volume (filename, nxyz, piece_size, num_pieces);
	}
    }


  /*----- delete regression coefficients data files -----*/
  if (option_data->numc > 0)
    {
      for (ip = 0;  ip < p;  ip++)
	{
	  ix = regmodel->flist[ip];
	  sprintf (filename, "coef.%d", ix);
	  delete_volume (filename, nxyz, piece_size, num_pieces);
	}
    }


  /*----- dispose of data matrix -----*/
  matrix_destroy (xdata);


  /*----- deallocate memory -----*/
  if (option_data->yname != NULL)
    {
      for (ip = 0;  ip < MAX_OBSERVATIONS;  ip++)
	{
	  if (option_data->yname[ip] != NULL)
	    {
	      free (option_data->yname[ip]);
	      option_data->yname[ip] = NULL;
	    }
	}
      free (option_data->yname);
      option_data->yname = NULL;
    }

  if (option_data->fcoef_filename != NULL)
    {
      for (ip = 0;  ip < MAX_XVARS;  ip++)
	{
	  if (option_data->fcoef_filename[ip] != NULL)
	    {
	      free (option_data->fcoef_filename[ip]);
	      option_data->fcoef_filename[ip] = NULL;
	    }
	}
      free (option_data->fcoef_filename);
      option_data->fcoef_filename = NULL;
    }
 
  if (option_data->rcoef_filename != NULL)
    {
      for (ip = 0;  ip < MAX_XVARS;  ip++)
	{
	  if (option_data->rcoef_filename[ip] != NULL)
	    {
	      free (option_data->rcoef_filename[ip]);
	      option_data->rcoef_filename[ip] = NULL;
	    }
	}
      free (option_data->rcoef_filename);
      option_data->rcoef_filename = NULL;
    }
 
  if (option_data->tcoef_filename != NULL)
    {
      for (ip = 0;  ip < MAX_XVARS;  ip++)
	{
	  if (option_data->tcoef_filename[ip] != NULL)
	    {
	      free (option_data->tcoef_filename[ip]);
	      option_data->tcoef_filename[ip] = NULL;
	    }
	}
      free (option_data->tcoef_filename);
      option_data->tcoef_filename = NULL;
    }

  if (option_data->levels != NULL)
    {
      free (option_data->levels);
      option_data->levels = NULL;
    }

  if (option_data->counts != NULL)
    {
      free (option_data->counts);
      option_data->counts = NULL;
    }


  if (option_data->bucket_filename != NULL)
    {
      free (option_data->bucket_filename);
      option_data->bucket_filename = NULL;
    }

  if (option_data->brick_type != NULL)
    {
      free (option_data->brick_type);
      option_data->brick_type = NULL;
    }

  if (option_data->brick_coef != NULL)
    {
      free (option_data->brick_coef);
      option_data->brick_coef = NULL;
    }

  if (option_data->brick_label != NULL)
    {
      for (ib = 0;  ib < option_data->numbricks;  ib++)
	{
	  if (option_data->brick_label[ib] != NULL)
	    {
	      free (option_data->brick_label[ib]);
	      option_data->brick_label[ib] = NULL;
	    }
	}
      free (option_data->brick_label);
      option_data->brick_label = NULL;
    }
      
}


/*---------------------------------------------------------------------------*/
/*
  Multiple linear regression analysis (3dRegAna)
*/

int main 
(
  int argc,                    /* number of input arguments */
  char ** argv                 /* array of input arguments */ 
)

{
  RA_options option_data;      /* user input options */
  matrix xdata;                /* independent variable matrix */ 
  model regmodel;              /* linear regression model */
  int piece_size;              /* number of voxels in dataset piece */

   
  /*----- Identify software -----*/
#if 0
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR); 
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");
#endif

  /*-- 22 Feb 1999: addto the arglist, if user wants to --*/

   PRINT_VERSION("3dRegAna") ; AUTHOR(PROGRAM_AUTHOR);
   mainENTRY("3dRegAna main") ; machdep() ;

  { int new_argc ; char ** new_argv ;
    addto_args( argc , argv , &new_argc , &new_argv ) ;
    if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
  }
  
  /*----- program initialization -----*/
  initialize_program (argc, argv, &xdata, &regmodel, &option_data);


  /*----- perform regression analysis -----*/
  calculate_results (xdata, &regmodel, &option_data);


  /*----- write requested output files -----*/
  output_results (xdata, &regmodel, &option_data);
		  

  /*----- end of program -----*/
  terminate_program (&xdata, &regmodel, &option_data);  

  exit(0);
}


/*---------------------------------------------------------------------------*/
