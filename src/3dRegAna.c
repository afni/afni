/*
  This program performs multiple linear regression analysis across AFNI
  3d datasets.

  File:    3dRegAna.c
  Author:  B. Douglas Ward
  Date:    10 October 1997

*/

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

#define PROGRAM_NAME "3dRegAna"         /* name of this program */
#define LAST_MOD_DATE "10 October 1997" /* date of last program modification */
#define SUFFIX ".3dregana"              /* suffix for temporary files */

#include <stdio.h>
#include <math.h>
#include "editvol.h"
#include "matrix.h"
#include "matrix.c"


/*** HP-UX ***/
#ifdef HP
# define DF "bdf ."
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


#define MAX_XVARS 101            /* max. number of independent variables */
#define MAX_OBSERVATIONS 1000    /* max. number of input datasets */
#define MAX_NAME_LENGTH 80       /* max. streng length for file names */ 
#define MEGA  1048576            /* one megabyte */


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
     "                   workspace  (default = 12)                          \n"
     "[-rmsmin r]        r = minimum rms error to reject constant model     \n"
     "[-fdisp fval]      display (to screen) results for those voxels       \n"
     "                   whose F-statistic is > fval                        \n"
     "                                                                      \n"
     "[-flof alpha]      alpha = minimum p value for F due to lack of fit   \n"
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
     );
  
  exit(0);
}


/*---------------------------------------------------------------------------*/
     
/** macro to test a malloc-ed pointer for validity **/
     
#define MTEST(ptr) \
     if((ptr)==NULL) \
     ( fprintf(stderr,"*** Cannot allocate memory for statistics!\n"         \
	       "*** Try using the -workmem option to reduce memory needs,\n" \
	       "*** or create more swap space in the operating system.\n"    \
	       ), exit(0) )
     

/*---------------------------------------------------------------------------*/

/** macro to open a dataset and make it ready for processing **/

#define DOPEN(ds,name)                                                        \
do{ int pv ; (ds) = THD_open_one_dataset((name)) ;                         \
       if( !ISVALID_3DIM_DATASET((ds)) ){                                     \
          fprintf(stderr,"*** Can't open dataset: %s\n",(name)) ; exit(1) ; } \
       if( (ds)->daxes->nxx!=nx || (ds)->daxes->nyy!=ny || (ds)->daxes->nzz!=nz ){   \
          fprintf(stderr,"*** Axes mismatch: %s\n",(name)) ; exit(1) ; }      \
       if( DSET_NUM_TIMES((ds)) > 1 ){                                        \
         fprintf(stderr,"*** Can't use time-dependent data: %s\n",(name));exit(1); } \
       THD_load_datablock( (ds)->dblk , NULL ) ;                              \
       pv = DSET_PRINCIPAL_VALUE((ds)) ;                                      \
       if( DSET_ARRAY((ds),pv) == NULL ){                                     \
          fprintf(stderr,"*** Can't access data: %s\n",(name)) ; exit(1); }   \
       if( DSET_BRICK_TYPE((ds),pv) == MRI_complex ){                         \
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
   Get the dimensions of the 3d AFNI data sets.
*/

void get_dimensions 
(
  RA_options * option_data            /* user input options */
)

{
  
   THD_3dim_dataset * dset=NULL;

   /*----- read first dataset to get dimensions, etc. -----*/

   dset = THD_open_one_dataset( option_data->first_dataset ) ;
   if( ! ISVALID_3DIM_DATASET(dset) ){
      fprintf(stderr,"*** Unable to open dataset file %s\n", 
              option_data->first_dataset);
      exit(1) ;
   }

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
  Convert one volume to another type, autoscaling:
  Return value is the scaling factor used (0.0 --> no scaling).
*/

float EDIT_coerce_autoscale_new
( 
  int nxyz,             /* number of voxels */
  int itype,            /* input datum type */
  void *ivol,           /* pointer to input volume */
  int otype,            /* output datum type */
  void *ovol            /* pointer to output volume (must be pre-malloc-ed) */
)

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
  int iv;                             /* sub-brick index */ 
  int ierror;                         /* number of errors in editing data */
  int ibuf[32];                       /* integer buffer */
  float fbuf[MAX_STAT_AUX];           /* float buffer */
  float fimfac;                       /* scale factor for short data */
  int output_datum;                   /* data type for output data */
  short * tsp = NULL;                 /* 2nd sub-brick data pointer */
  void  * vdif = NULL;                /* 1st sub-brick data pointer */
  float top, bot, func_scale_short;   /* parameters for scaling data */
  int top_ss, bot_ss;                 /* 2nd sub-brick value limits */
  
  
  /*----- initialize local variables -----*/
  nxyz = option_data->nxyz;
  
  /*----- read first dataset -----*/
  dset = THD_open_one_dataset (option_data->first_dataset) ;
  if( ! ISVALID_3DIM_DATASET(dset) ){
    fprintf(stderr,"*** Unable to open dataset file %s\n",
	    option_data->first_dataset);
    exit(1) ;
  }
  
  /*-- make an empty copy of this dataset, for eventual output --*/
  new_dset = EDIT_empty_copy( dset ) ;
  
  
  iv = DSET_PRINCIPAL_VALUE(dset) ;
  if( option_data->datum >= 0 ){
    output_datum = option_data->datum ;
  } else {
    output_datum = DSET_BRICK_TYPE(dset,iv) ;
    if( output_datum == MRI_byte ) output_datum = MRI_short ;
  }
  
  
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
    printf("----- Writing `fith' dataset ");
  else if (func_type == FUNC_TT_TYPE)           /* t-statistic */
    printf("----- Writing `fitt' dataset ");
  else if (func_type == FUNC_FT_TYPE)           /* F-statistic */
    printf("----- Writing `fift' dataset ");

  printf("into %s\n", new_dset->dblk->diskptr->header_name) ;
  
  fbuf[0] = numdof;   
  fbuf[1] = dendof;
  for( ii=2 ; ii < MAX_STAT_AUX ; ii++ ) fbuf[ii] = 0.0 ;
  (void) EDIT_dset_items( new_dset , ADN_stat_aux , fbuf , ADN_none ) ;
  
  fbuf[0] = (output_datum == MRI_short && fimfac != 1.0 ) ? fimfac : 0.0 ;
  fbuf[1] = 1.0 / func_scale_short ;
  (void) EDIT_dset_items( new_dset , ADN_brick_fac , fbuf , ADN_none ) ;
  
  THD_load_statistics( new_dset ) ;
  THD_write_3dim_dataset( NULL,NULL , new_dset , True ) ;

  
  /*----- deallocate memory -----*/   
  THD_delete_3dim_dataset( new_dset , False ) ; new_dset = NULL ;
  
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
  matrix * xdata,              /* independent variable matrix */
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


  /*----- initialize local variables -----*/
  n = xdata->rows;
  p = regmodel->p;
  flist = regmodel->flist;
  piece_size = option_data->piece_size;


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
  option_data->workmem = 12;
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
  int nopt = 1;                    /* input option argument counter */
  int ival, index;                 /* integer input */
  float fval;                      /* float input */
  int rows, cols;                  /* number rows and columns for x matrix */
  int irows, jcols;                /* data point counters */ 
  THD_3dim_dataset * dset=NULL;    /* test whether data set exists */
  char message[MAX_NAME_LENGTH];   /* error message */


  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();
 

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
	dset = THD_open_one_dataset( argv[nopt] ) ;
	if( ! ISVALID_3DIM_DATASET(dset) )
	  {
	    sprintf(message,"Unable to open dataset file %s\n", argv[nopt]);
	    RA_error (message);
	  }
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
	      RA_error ("illegal argument after -model ");
	    regmodel->flist[regmodel->p] = ival;
	
	    regmodel->p++;
	    nopt++;
	  }
	
	if (strncmp(argv[nopt], ":", 1) == 0)
	  {
	    nopt++;

	    while ((nopt < argc)
		   && (strncmp(argv[nopt], "-", 1) != 0))
	      {
		sscanf (argv[nopt], "%d", &ival);
		if ((ival < 0) || (ival > cols))
		  RA_error ("illegal argument after -model ");
		regmodel->flist[regmodel->p] = ival;
		regmodel->rlist[regmodel->q] = ival;
		regmodel->p++;
		regmodel->q++;
		nopt++;
	      }
	  }    
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
  Count the number of output data volumes required.
*/

void count_volumes 
(  
  model * regmodel,            /* regression model */
  RA_options * option_data     /* user input options */
)

{
  int ip;                  /* parameter index */
  int ix;                  /* x-variable index */


  /*----- count number of volumes for F-statistics -----*/
  for (ip = 0;  ip < regmodel->p;  ip++)
    {
      ix = regmodel->flist[ip];
      if (option_data->fcoef_filename[ix] != NULL)
	{
	  option_data->numf += 1;
	}
    }

  /*----- count number of volumes for R^2 -----*/
  for (ip = 0;  ip < regmodel->p;  ip++)
    {
      ix = regmodel->flist[ip];
      if (option_data->rcoef_filename[ix] != NULL)
	{
	  option_data->numr += 1;
	}
    }

  /*----- count number of volumes for t-statistics -----*/
  for (ip = 0;  ip < regmodel->p;  ip++)
    {
      ix = regmodel->flist[ip];
      if (option_data->tcoef_filename[ix] != NULL)
	{
	  option_data->numt += 1;
	}
    }
 
  /*----- count number of volumes for regression coefficients -----*/
  for (ip = 0;  ip < regmodel->p;  ip++)
    {
      ix = regmodel->flist[ip];
      if (   (option_data->fcoef_filename[ix] != NULL) 
	  || (option_data->rcoef_filename[ix] != NULL)
	  || (option_data->tcoef_filename[ix] != NULL))
	{
	  option_data->numc += 1;
	}
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


  /*----- count number of distinct floating point volumes required -----*/
  num_vols = option_data->numt + option_data->numc;
  if (option_data->numf > 0)  num_vols += 1;
  if (option_data->numr > 0)  num_vols += 1;

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
  dset = THD_open_one_dataset (option_data->first_dataset ) ;
  if( ! ISVALID_3DIM_DATASET(dset) ){
    fprintf(stderr,"*** Unable to open dataset file %s\n",
	    option_data->first_dataset);
    exit(1) ;
  }
  
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
  Routine to check whether output files already exist.
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

  /*----- first, determine the maximum number of files required -----*/
  nmax = option_data->numf + option_data->numr + option_data->numt 
    + option_data->numc;

  printf("\nThis problem requires approximately %d MB of free disk space.\n", 
	  (4*nmax*nxyz/1000000) + 1);


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
  /*----- create independent variable data matrix -----*/
  matrix_initialize (xdata);


  /*----- get all operator inputs -----*/
  get_inputs (argc, argv, xdata, regmodel, option_data);


  /*----- use first data set to get data set dimensions -----*/
  option_data->first_dataset = option_data->yname[0];
  get_dimensions (option_data);
  printf ("Data set dimensions:  nx = %d  ny = %d  nz = %d  nxyz = %d \n",
	 option_data->nx, option_data->ny, option_data->nz, option_data->nxyz);
 

  /*----- sort model variable indices into ascending order -----*/
  sort_model_indices (regmodel);


  /*----- count the number of output volumes required -----*/
  count_volumes (regmodel, option_data);


  /*----- break problem into smaller pieces -----*/
  break_into_pieces (xdata->rows, option_data);


  /*----- identify repeat observations -----*/
  if (option_data->flofmax >= 0.0)  
    identify_repeats (xdata, regmodel, option_data);
  

  /*----- check for valid inputs -----*/
  check_for_valid_inputs (xdata, regmodel, option_data);

    
  /*----- check whether any of the output files already exist -----*/
  check_output_files (option_data); 
 

  /*----- check whether temporary files already exist  -----*/
  check_temporary_files (xdata, regmodel, option_data);


  /*----- check whether there is sufficient disk space -----*/
  if (option_data->diskspace)  check_disk_space (option_data);

}


/*---------------------------------------------------------------------------*/
/*
  Calculate constant matrices to be used for all voxels.
*/

void calc_matrices 
(
  matrix x,                     /* independent variable matrix X   */
  matrix * xtxinv,              /* matrix:  1/(X'X)                */
  matrix * xtxinvxt,            /* matrix:  (1/(X'X))X'            */
  matrix * a                    /* matrix:  I - X(1/(X'X))X'       */
)

{
  matrix ident, xt, xtx, tmp;   /* temporary matrix calculation results */
  int ok;                       /* flag for successful matrix inversion */


  /*----- initialize matrices -----*/
  matrix_initialize (&ident);
  matrix_initialize (&xt);
  matrix_initialize (&xtx);
  matrix_initialize (&tmp);


  /*----- calculate various matrices which will be needed later -----*/
  matrix_transpose (x, &xt);
  matrix_multiply (xt, x, &xtx);
  ok = matrix_inverse (xtx, xtxinv);
  if (! ok)  RA_error ("Improper X matrix  (cannot invert X'X) ");
  matrix_multiply (*xtxinv, xt, xtxinvxt);
  matrix_multiply (x, *xtxinvxt, &tmp);
  matrix_identity (tmp.rows, &ident);
  matrix_subtract (ident, tmp, a);


  /*----- dispose of matrices -----*/
  matrix_destroy (&tmp);
  matrix_destroy (&xtx);
  matrix_destroy (&xt);
  matrix_destroy (&ident);

}


/*---------------------------------------------------------------------------*/
/*
  Perform initialization required for the regression analysis.
*/

void init_regression_analysis 
(
  matrix * xdata,               /* independent variable matrix X  */ 
  int p,                        /* number of parameters in the full model */
  int q,                        /* number of parameters in the rdcd model */
  int * flist,                  /* list of parameters in the full model */
  int * rlist,                  /* list of parameters in the rdcd model */
  matrix * xtxinv,              /* matrix:  1/(Xf'Xf)             */
  matrix * xtxinvxt,            /* matrix:  (1/(Xf'Xf))Xf'        */
  matrix * afull,               /* matrix:  I - Xf(1/(Xf'Xf))Xf'  */  
  matrix * ardcd,               /* matrix:  I - Xr(1/(Xr'Xr))Xr'  */
  matrix * azero                /* matrix:  I - (1/n)11'          */
)

{
  matrix xzero, xrdcd, xfull;   /* matrices extracted from X matrix */
  int zlist[MAX_XVARS];         /* list of parameters in constant model */
  int ip;                       /* parameter index */

  
  /*----- initialize list of parameters in the constant model -----*/
  for (ip = 0;  ip < MAX_XVARS;  ip++)
    zlist[ip] = 0;


  /*----- initialize matrices -----*/
  matrix_initialize (&xzero);
  matrix_initialize (&xrdcd);
  matrix_initialize (&xfull);


  /*----- extract the constant, reduced, and full data matrices from the 
    input data matrix -----*/
  matrix_extract (*xdata, 1, zlist, &xzero);
  matrix_extract (*xdata, q, rlist, &xrdcd);
  matrix_extract (*xdata, p, flist, &xfull);  


  /*----- calculate constant matrices which will be needed later -----*/
  calc_matrices (xzero, xtxinv, xtxinvxt, azero);
  calc_matrices (xrdcd, xtxinv, xtxinvxt, ardcd);
  calc_matrices (xfull, xtxinv, xtxinvxt, afull);


  /*----- dispose of matrices -----*/
  matrix_destroy (&xfull);
  matrix_destroy (&xrdcd);
  matrix_destroy (&xzero);
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the error sum of squares.
*/

float  calc_sse 
(
  matrix a,                  /* matrix:  A = I - X(1/(X'X))X'  */
  matrix y                   /* vector of measured data */
)

{
  matrix yt, ay, ytay;       /* results of intermediate calculations */  
  float sse;                 /* error sum of squares */


  /*----- initialize matrices -----*/
  matrix_initialize (&yt);
  matrix_initialize (&ay);
  matrix_initialize (&ytay);


  /*----- calculate the error sum of squares -----*/
  matrix_multiply (a, y, &ay);
  matrix_transpose (y, &yt);
  matrix_multiply (yt, ay, &ytay);
  sse = ytay.elts[0][0];


  /*----- dispose of matrices -----*/
  matrix_destroy (&ytay);
  matrix_destroy (&ay);
  matrix_destroy (&yt);


  /*----- return SSE -----*/
  return (sse);
 
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the pure error sum of squares.
*/

float  calc_sspe 
(
  matrix y,                  /* vector of measured data */
  int * levels,              /* indices for repeat observations */
  int * counts,              /* number of observations at each level */
  int c                      /* number of unique rows in ind. var. matrix */
)

{
  int i, j;                  /* indices */
  float * sum = NULL;        /* sum of observations at each level */
  float diff;                /* difference between observation and average */
  float sspe;                /* pure error sum of squares */


  /*----- initialize sum -----*/
  sum = (float *) malloc (sizeof(float) * (y.rows));
  MTEST (sum);
  
  for (j = 0;  j < c;  j++)
    sum[j] = 0.0;


  /*----- accumulate sum for each level -----*/
  for (i = 0;  i < y.rows;  i++)
    {
      j = levels[i];
      sum[j] += y.elts[i][0];
    }


  /*----- calculate SSPE -----*/
  sspe = 0.0;
  for (i = 0;  i < y.rows;  i++)
    {
      j = levels[i];
      diff = y.elts[i][0] - (sum[j]/counts[j]);
      sspe += diff * diff;
    }

  
  free (sum);   sum = NULL;


  /*----- return SSPE -----*/
  return (sspe);
 
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the F-statistic for lack of fit.
*/

float calc_flof
(
  int n,                      /* number of data points */
  int p,                      /* number of parameters in the full model */
  int c,                      /* number of unique rows in ind. var. matrix */
  float sse,                  /* error sum of squares from full model */
  float sspe                 /* error sum of squares due to pure error */
)

{
  const float EPSILON = 1.0e-10;      /* protection against divide by zero */
  float mspe;                 /* mean square error due to pure error */
  float sslf;                 /* error sum of squares due to lack of fit */
  float mslf;                 /* mean square error due to lack of fit */
  float flof;                 /* F-statistic for lack of fit */


  /*----- calculate mean sum of squares due to pure error -----*/
  mspe = sspe / (n - c);


  /*----- calculate mean sum of squares due to lack of fit -----*/
  sslf = sse - sspe;
  mslf = sslf / (c - p);


  /*----- calculate F-statistic for lack of fit -----*/
  if (fabs(mspe) < EPSILON)
    flof = 0.0;
  else
    flof = mslf / mspe;


  /*----- return F-statistic for lack of fit -----*/
  return (flof);

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the regression coefficients.
*/

void calc_coef 
(
  matrix xtxinvxt,            /* matrix:  (1/(Xf'Xf))Xf'   */
  matrix y,                   /* vector of measured data   */
  matrix * coef               /* vector of regression parameters */
)

{

  /*----- calculate regression coefficients -----*/
  matrix_multiply (xtxinvxt, y, coef);

}


/*---------------------------------------------------------------------------*/
/*
  Calculate t-statistics for the regression coefficients.
*/

void calc_tcoef 
(
  int n,                      /* number of data points */
  int p,                      /* number of parameters in the full model */
  float sse,                  /* error sum of squares */
  matrix xtxinv,              /* matrix:  1/(Xf'Xf)        */
  matrix coef,                /* vector of regression parameters */
  matrix * tcoef              /* t-statistics for regression parameters */
)

{
  const float EPSILON = 1.0e-10;      /* protection against divide by zero */
  int df;                     /* error degrees of freedom */
  float mse;                  /* mean square error */
  int i;                      /* parameter index */
  float stddev;               /* standard deviation for parameter estimate */


  /*----- calculate t-statistics for regression coefficients -----*/
  df = n - p;
  mse = sse / df;
  matrix_equate (coef, tcoef);
  for (i = 0;  i < p;  i++)
    {
      stddev = sqrt (mse * xtxinv.elts[i][i]);
      if (fabs(stddev) < EPSILON)
	tcoef->elts[i][0] = 0.0;
      else
	tcoef->elts[i][0] /= stddev;
    }
}


/*---------------------------------------------------------------------------*/
/*
  Calculate the F-statistic for significance of the regression.
*/

void calc_freg
(
  int n,                      /* number of data points */
  int p,                      /* number of parameters in the full model */
  int q,                      /* number of parameters in the rdcd model */
  float ssef,                 /* error sum of squares from full model */
  float sser,                 /* error sum of squares from reduced model */
  float * freg                /* F-statistic for the full regression model */
)

{
  const float EPSILON = 1.0e-10;      /* protection against divide by zero */
  float msef;                 /* mean square error for the full model */
  float msreg;                /* mean square due to the regression */


  /*----- F-statistic for significance of the regression -----*/
  msreg = (sser - ssef) / (p - q);
  msef   = ssef / (n - p);
  if (fabs(msef) < EPSILON)
    *freg = 0.0;
  else
    *freg = msreg / msef;

}


/*---------------------------------------------------------------------------*/
/*
  Calculate the coefficient of multiple determination R^2.
*/

void calc_rsqr 
(
  float ssef,                 /* error sum of squares from full model */
  float ssto,                 /* total (corrected for mean) sum of squares */
  float * rsqr                /* coeff. of multiple determination R^2  */
)

{
  const float EPSILON = 1.0e-10;      /* protection against divide by zero */


  /*----- coefficient of multiple determination R^2 -----*/
  if (fabs(ssto) < EPSILON)
    *rsqr = 0.0;
  else
    *rsqr = 1.0 - ssef/ssto;
}


/*---------------------------------------------------------------------------*/
/*
  Perform the entire regression analysis for one voxel.
*/

void regression_analysis 
(
  int n,                      /* number of data points */
  int p,                      /* number of parameters in the full model */
  int q,                      /* number of parameters in the rdcd model */
  matrix xtxinv,              /* matrix:  1/(Xf'Xf)  */
  matrix xtxinvxt,            /* matrix:  (1/(Xf'Xf))Xf'  */
  matrix afull,               /* matrix:  I - Xf(1/(Xf'Xf))Xf'  */  
  matrix ardcd,               /* matrix:  I - Xr(1/(Xr'Xr))Xr'  */
  matrix azero,               /* matrix:  I - (1/n)11'          */
  matrix y,                   /* vector of measured data */ 
  float rms_min,              /* minimum rms error to reject zero model */
  int * levels,               /* indices for repeat observations */
  int * counts,               /* number of observations at each level */
  int c,                      /* number of unique rows in ind. var. matrix */
  float flofmax,              /* max. allowed F-stat due to lack of fit */  
  float * flof,               /* F-statistic for lack of fit */
  matrix * coef,              /* regression parameters */
  matrix * tcoef,             /* t-statistics for regression parameters */
  float * freg,               /* F-statistic for the full regression model */
  float * rsqr                /* coeff. of multiple determination R^2  */
)

{
  float sse_rdcd;             /* error sum of squares from reduced model */
  float sse_full;             /* error sum of squares from full model */
  float ssto;                 /* total (corrected for mean) sum of squares */
  float sspe;                 /* pure error sum of squares */


  /*----- initialization -----*/
  matrix_create (p, 1, coef);
  matrix_create (p, 1, tcoef);
  *freg = 0.0;
  *rsqr = 0.0;


  /*----- Calculate the total (corrected for the mean) sum of squares -----*/
  ssto = calc_sse (azero, y);

  
  /*----- Stop here if variation in data is sufficiently low -----*/
  if (sqrt(ssto/(n-1)) < rms_min)   return;
    

  /*----- Calculate the error sum of squares for the reduced model -----*/ 
  sse_rdcd = calc_sse (ardcd, y);


  /*----- Calculate the error sum of squares for the full model -----*/ 
  sse_full = calc_sse (afull, y);

    
  /*----- Test for lack of fit -----*/
  if (flofmax > 0.0)
    {
      /*----- Calculate the pure error sum of squares -----*/
      sspe = calc_sspe (y, levels, counts, c);
    
      /*----- Calculate F-statistic for lack of fit -----*/
      *flof = calc_flof (n, p, c, sse_full, sspe);
    
      if (*flof > flofmax)  return;
    }
  else
    *flof = -1.0;


  /*----- Calculate the coefficients of the linear regression -----*/
  calc_coef (xtxinvxt, y, coef);


  /*----- Calculate t-statistics for the regression coefficients -----*/
  calc_tcoef (n, p, sse_full, xtxinv, *coef, tcoef);

      
  /*----- Calculate F-statistic for significance of the regression -----*/
  calc_freg (n, p, q, sse_full, sse_rdcd, freg);


  /*----- Calculate coefficient of multiple determination R^2 -----*/
  calc_rsqr (sse_full, ssto, rsqr);
   
}


/*---------------------------------------------------------------------------*/
/*
  Save results for current voxel into piece data for output later.
*/

void save_voxel 
(
  int iv,               /* current voxel number within piece */
  matrix y,             /* vector of measured data */       
  float fdisp,          /* minimum F-statistic for display */
  model * regmodel,     /* linear regression model */
  float flof,           /* F-statistic for lack of fit */
  matrix coef,          /* regression parameters */
  matrix tcoef,         /* t-statistics for regression parameters */
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
                
      if (coef_piece[ix] != NULL)  coef_piece[ix][iv] = coef.elts[ip][0];
                       
      if (tcoef_piece[ix] != NULL)  tcoef_piece[ix][iv] = tcoef.elts[ip][0];
      
    }
    

  /*----- if so requested, display results for this voxel -----*/
  if ((fdisp >= 0.0) && (freg >= fdisp))
    {
      printf ("\n\nVoxel #%d:  \n", iv);
      printf ("\nY data: \n");
      for (ip = 0;  ip < y.rows;  ip++)
	printf ("Y[%d] = %f \n", ip, y.elts[ip][0]);

      if (flof >= 0.0)	printf ("\nF lack of fit = %f \n", flof);
      printf ("\nF regression  = %f \n", freg);
      printf ("R-squared     = %f \n", rsqr);

      printf ("\nRegression Coefficients: \n");
      for (ip = 0;  ip < coef.rows;  ip++)
	{
	  ix = regmodel->flist[ip];
	  printf ("b[%d] = %f   ", ix, coef.elts[ip][0]);
	  printf ("t[%d] = %f \n", ix, tcoef.elts[ip][0]);
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
  matrix * xdata,             /* independent variable matrix */
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
  matrix coef;                /* regression parameters */
  matrix tcoef;               /* t-statistics for regression parameters */
  matrix xtxinv, xtxinvxt;    /* intermediate matrix calculation results */
  matrix afull, ardcd, azero; /* intermediate matrix calculation results */
  matrix y;                   /* vector of measured data */       

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


  /*----- initialize matrices -----*/
  matrix_initialize (&xtxinv);
  matrix_initialize (&xtxinvxt);
  matrix_initialize (&afull);
  matrix_initialize (&ardcd);
  matrix_initialize (&azero);
  matrix_initialize (&coef);
  matrix_initialize (&tcoef);
  matrix_initialize (&y);


  /*----- initialize local variables -----*/
  n = xdata->rows;
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
  init_regression_analysis (xdata, p, q, flist, rlist,
			    &xtxinv, &xtxinvxt, &afull, &ardcd, &azero);
  matrix_create (n, 1, &y);

  if (option_data->fdisp >= 0)
    {
      printf ("\n");
      printf ("X matrix: \n");
      matrix_print (*xdata);
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
	    y.elts[i][0] = yfimar[i][ivox];
     

	  /*----- calculate results for this voxel -----*/
	  regression_analysis (n, p, q, xtxinv, xtxinvxt, afull, ardcd, azero, 
			       y, option_data->rms_min, option_data->levels, 
			       option_data->counts, option_data->c, 
			       option_data->flofmax, &flof,
			       &coef, &tcoef, &freg, &rsqr);
    

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


  /*----- dispose of matrices -----*/
  matrix_destroy (&y);
  matrix_destroy (&tcoef);
  matrix_destroy (&coef);
  matrix_destroy (&azero);
  matrix_destroy (&ardcd); 
  matrix_destroy (&afull);
  matrix_destroy (&xtxinvxt);
  matrix_destroy (&xtxinv); 

}


/*---------------------------------------------------------------------------*/
/*
  Write out the user requested output files.
*/

void output_results 
(
  matrix * xdata,              /* independent variable matrix */
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
  n = xdata->rows;
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
	      delete_volume (filename, nxyz, piece_size, num_pieces);

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
      delete_volume (filename, nxyz, piece_size, num_pieces);
      
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
      delete_volume (filename, nxyz, piece_size, num_pieces);
      
      numdof = p - q;
      dendof = n - p;
      
      for (ip = 0;  ip < p;  ip++)
	{
	  ix = regmodel->flist[ip];
	  
	  if (option_data->fcoef_filename[ix] != NULL)
	    {
	      sprintf (filename, "coef.%d", ix);
	      read_volume (filename, volume1, nxyz, piece_size, num_pieces);
	      delete_volume (filename, nxyz, piece_size, num_pieces);
	      
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
	  
	  if (option_data->tcoef_filename[ix] != NULL)
	    {
	      sprintf (filename, "tcoef.%d", ix);
	      delete_volume (filename, nxyz, piece_size, num_pieces);
	    }
	}
    }


  /*----- delete regression coefficients data files -----*/
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
	      delete_volume (filename, nxyz, piece_size, num_pieces);
	    }
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
 
}


/*---------------------------------------------------------------------------*/
/*
  Multiple linear regression analysis (3dRegAna)
*/

void main 
(
  int argc,                    /* number of input arguments */
  char ** argv                 /* array of input arguments */ 
)

{
  RA_options option_data;      /* user input options */
  matrix xdata;                /* independent variable matrix */ 
  model regmodel;              /* linear regression model */
  int piece_size;              /* number of voxels in dataset piece */


  printf ("\n\nProgram %s \n", PROGRAM_NAME);
  printf ("Last revision: %s \n\n", LAST_MOD_DATE);

  
  /*----- program initialization -----*/
  initialize_program (argc, argv, &xdata, &regmodel, &option_data);


  /*----- perform regression analysis -----*/
  calculate_results (&xdata, &regmodel, &option_data);


  /*----- write requested output files -----*/
  output_results (&xdata, &regmodel, &option_data);
		  

  /*----- end of program -----*/
  terminate_program (&xdata, &regmodel, &option_data);  

}
