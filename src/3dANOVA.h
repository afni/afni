/*
   This file contains routines, data structures, and constants which are
   common for the three ANOVA programs:  3dANOVA, 3dANOVA2, 3dANOVA3.

   File:    3dANOVA.h
   Author:  B. D. Ward
   Date:    20 January 1997
   
   Mod:     23 January 1997
            Added command -diskspace to print out how much disk space is
            required to execute the problem.

   Mod:     27 January 1997
            Change to routine write_afni_data to reduce truncation error.    
	    
   Mod:     29 January 1997
            Added machine specific code for checking disk space.
            
*/

/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
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
-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/

#include <stdio.h>
#include <math.h>
#include "editvol.h"

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


#define MAX_LEVELS 100           /* max. number of factor levels */  
#define MAX_OBSERVATIONS 100     /* max. number of observations per cell */
#define MAX_DIFFS 100            /* max. number of user requested diffs. */
#define MAX_CONTR 100            /* max. number of user requested contrasts */
#define MAX_NAME_LENGTH 80       /* max. strength length for file names */ 


typedef struct anova_options
{ 
  int   datum;                  /* data type for "intensity" data subbrick */
  char  session[MAX_NAME_LENGTH];     /* name of output directory */

  int   diskspace;              /* if positive, print out how much disk space
                                   is required for program execution */
  
  int   nvoxel;                 /* number of voxel for special output */

  int   model;                  /* indicates type of ANOVA model to be used:
				  model=1   A,B,C fixed;          AxBxC
				  model=2   A,B,C random;         AxBxC
				  model=3   A fixed; B,C random;  AxBxC 
				  model=4   A,B fixed; C random;  AxBxC
				  model=5   A,B fixed; C random;  AxB,BxC,C(A)
                                */

  int   a;                      /* number of levels for factor A */
  int   b;                      /* number of levels for factor B */
  int   c;                      /* number of levels for factor C */
  int   n;                      /* number of observations in each cell */
  int   nt;                     /* total number of observations */
  
  int   na[MAX_LEVELS];         /* number of observations at each level
                                   of factor A (for 3dANOVA only) */

  char  ***** xname;            /* names of the input data files */
  char  * first_dataset;        /* name of the first data set */
   
  int   nx, ny, nz;             /* data set dimensions */
  int   nxyz;                   /* number of voxels per image */

  int   nftr;                   /* if positive, calculate F for treatment */
  char  * ftrname;              /* name of output file of F for treatment */

  int   nfa;                    /* if positive, calculate F for A effect */
  char  * faname;               /* name of output file of F for A effect */
  int   nfb;                    /* if positive, calculate F for B effect */
  char  * fbname;               /* name of output file of F for B effect */
  int   nfc;                    /* if positive, calculate F for C effect */
  char  * fcname;               /* name of output file of F for C effect */
  int   nfab;                   /* if pos., calculate F for A*B interaction */
  char  * fabname;              /* name of output file for A*B interaction */
  int   nfac;                   /* if pos., calculate F for A*C interaction */
  char  * facname;              /* name of output file for A*C interaction */
  int   nfbc;                   /* if pos., calculate F for B*C interaction */
  char  * fbcname;              /* name of output file for B*C interaction */
  int   nfabc;                  /* if pos, calculate F for A*B*C interaction */
  char  * fabcname;             /* name of output file for A*B*C interaction */


  int   num_ameans;             /* number of factor A level means */ 
  int   ameans[MAX_LEVELS];     /* calc means for these factor A levels */
  char  * amname[MAX_LEVELS];   /* names of output files for factor A means */
  
  int   num_bmeans;             /* number of factor B level means */ 
  int   bmeans[MAX_LEVELS];     /* calc means for these factor B levels */
  char  * bmname[MAX_LEVELS];   /* names of output files for factor B means */
  
  int   num_cmeans;             /* number of factor C level means */ 
  int   cmeans[MAX_LEVELS];     /* calc means for these factor C levels */
  char  * cmname[MAX_LEVELS];   /* names of output files for factor C means */
  
  int   num_adiffs;             /* num of diffs in factor A level means */
  int   adiffs[MAX_DIFFS][2];   /* calc diffs in these factor A level means */
  char  * adname[MAX_DIFFS];    /* names of output files for A differences */
  
  int   num_bdiffs;             /* num of diffs in factor B level means */
  int   bdiffs[MAX_DIFFS][2];   /* calc diffs in these factor B level means */
  char  * bdname[MAX_DIFFS];    /* names of output files for B differences */
  
  int   num_cdiffs;             /* num of diffs in factor C level means */
  int   cdiffs[MAX_DIFFS][2];   /* calc diffs in these factor C level means */
  char  * cdname[MAX_DIFFS];    /* names of output files for C differences */
  
  int   num_acontr;             /* number of factor A contrasts */
  float acontr[MAX_CONTR][MAX_LEVELS];     /* factor A contrast vectors */
  char  * acname[MAX_CONTR];    /* names of output files for A contrasts */
  
  int   num_bcontr;             /* number of factor B contrasts */
  float bcontr[MAX_CONTR][MAX_LEVELS];     /* factor B contrast vectors */
  char  * bcname[MAX_CONTR];    /* names of output files for B contrasts */
  
  int   num_ccontr;             /* number of factor C contrasts */
  float ccontr[MAX_CONTR][MAX_LEVELS];     /* factor C contrast vectors */
  char  * ccname[MAX_CONTR];    /* names of output files for C contrasts */

} anova_options;


/*---------------------------------------------------------------------------*/
/*
   Routine to initialize input options.
*/

void initialize_options (anova_options * option_data)
{
  int i, j, k, m;
  
  option_data->datum = ILLEGAL_TYPE;
  strcpy (option_data->session, "./");
 
  option_data->diskspace = 0;

  option_data->nvoxel = -1;

  option_data->model = 1;
  
  option_data->a = 0;
  option_data->b = 0;
  option_data->c = 0;
  option_data->n = 0;
  option_data->nt = 0;
 
  for (i = 0;  i < MAX_LEVELS;  i++)
    option_data->na[i] = 0;

  option_data->xname = NULL;
  option_data->first_dataset = NULL;
  
  option_data->nx = 0;
  option_data->ny = 0;
  option_data->nz = 0;
  option_data->nxyz = 0;

  option_data->nftr = 0;
  option_data->ftrname = NULL;

  option_data->nfa = 0;
  option_data->faname = NULL;
  option_data->nfb = 0;
  option_data->fbname = NULL;
  option_data->nfc = 0;
  option_data->fcname = NULL;
  option_data->nfab = 0;
  option_data->fabname = NULL;
  option_data->nfac = 0;
  option_data->facname = NULL;
  option_data->nfbc = 0;
  option_data->fbcname = NULL;
  option_data->nfabc = 0;
  option_data->fabcname = NULL;

  option_data->num_ameans = 0;
  option_data->num_bmeans = 0;
  option_data->num_cmeans = 0;
  for (i = 0;  i < MAX_LEVELS;  i++)
    {
      option_data->ameans[i] = 0;
      option_data->amname[i] = NULL;
      option_data->bmeans[i] = 0;
      option_data->bmname[i] = NULL;
      option_data->cmeans[i] = 0;
      option_data->cmname[i] = NULL;
    }
  
  option_data->num_adiffs = 0;
  option_data->num_bdiffs = 0;
  option_data->num_cdiffs = 0;
  for (i = 0;  i < MAX_DIFFS;  i++)
    {
      option_data->adiffs[i][0] = 0;
      option_data->adiffs[i][1] = 0;
      option_data->adname[i] = NULL;
      option_data->bdiffs[i][0] = 0;
      option_data->bdiffs[i][1] = 0;
      option_data->bdname[i] = NULL;
      option_data->cdiffs[i][0] = 0;
      option_data->cdiffs[i][1] = 0;
      option_data->cdname[i] = NULL;
    }
  
  option_data->num_acontr = 0;
  option_data->num_bcontr = 0;
  option_data->num_ccontr = 0;
  for (i = 0;  i < MAX_CONTR;  i++)
    {
      option_data->acname[i] = NULL;
      option_data->bcname[i] = NULL;
      option_data->ccname[i] = NULL;
      for (j = 0;  j < MAX_LEVELS;  j++)
	{
	  option_data->acontr[i][j] = 0.0;
	  option_data->bcontr[i][j] = 0.0;
	  option_data->ccontr[i][j] = 0.0;
	}
    }
}

   
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
   Routine to print error message and stop.
*/

void ANOVA_error (char * message)
{
   fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
   exit(1);
}


/*---------------------------------------------------------------------------*/
/*
   Routine to write a 3d volume of floating point data to a (temporary)
   disk file.
*/

int volume_write_count (char * filename,  float * fout,  int size)
{
  char sfilename[MAX_NAME_LENGTH];   /* output file name */ 
  char message[MAX_NAME_LENGTH];     /* error message */
  FILE * far;                        /* floating point output file */
  int count;                         /* number of data items written to disk */


   /*----- output file name -----*/
   strcpy (sfilename, filename);
   strcat (sfilename, SUFFIX);

   /*----- first, see if file already exists -----*/
   far = fopen (sfilename, "r");
   if (far != NULL)
   {
      sprintf (message, "file %s already exists. ", sfilename); 
      ANOVA_error (message);
   }

   /*----- open temporary data file for output -----*/
   far = fopen (sfilename, "w");
   if (far == NULL) 
      ANOVA_error ("unable to write file ");
     
   /*----- write 3d data set -----*/
   count = fwrite (fout, sizeof(float), size, far);
   fclose (far);

   return (count);
}


/*---------------------------------------------------------------------------*/
/*
   Routine to write a 3d volume of floating point data to a (temporary)
   disk file. Error exit if cannot write entire file.
*/

void volume_write (char * filename,  float * fout,  int size)
{
  int count;                       /* number of data items written to disk */
  
  
  /*----- attempt to write 3d volume of data -----*/
  count = volume_write_count (filename, fout, size);
   
  /*----- error in writing file? -----*/
  if (count != size)  ANOVA_error ("error in writing data file ");
}


/*---------------------------------------------------------------------------*/
/*
   Routine to read a 3d volume of floating point data.
*/

void volume_read (char * filename,  float * fin,  int size)
{
  char sfilename[MAX_NAME_LENGTH];   /* input file name */
  char message[MAX_NAME_LENGTH];     /* error message */
  FILE * far;                        /* floating point input file */
  int count;                         /* number of data items read from disk */
  
  
  /*----- input file name -----*/
  strcpy (sfilename, filename);
  strcat (sfilename, SUFFIX);
  
  /*----- open temporary data file for input -----*/
  far = fopen (sfilename, "r");
  if (far == NULL) 
    {
      sprintf (message, "Unable to open temporary file %s", sfilename);
      ANOVA_error (message);
    }
  
  /*----- read 3d data file -----*/
  count = fread (fin, sizeof(float), size, far);   
  fclose (far);
  
  /*----- error in reading file? -----*/
  if (count != size)  ANOVA_error ("error in reading data file ");
  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to delete a file containing a 3d volume of floating point data.
*/

void volume_delete (char * filename)
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
  Routine to set a 3d volume of floating point data to zero.
*/

void volume_zero (float * f,  int size)
{
  int i;
  
  for (i = 0;  i < size;  i++)
    f[i] = 0.0;
}


/*---------------------------------------------------------------------------*/
/*
   Routine to get the dimensions of the 3d AFNI data sets.
*/

void get_dimensions (anova_options * option_data)
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
  Routine to check whether one temporary file already exists.
*/

void check_one_temporary_file (char * filename)
{
  FILE * far;                               /* temporary file pointer */
  char sfilename[MAX_NAME_LENGTH];          /* temporary file name */
  char message[MAX_NAME_LENGTH];            /* error message */
  
  
  /*-----  see if file already exists -----*/
  strcpy (sfilename, filename);
  strcat (sfilename, SUFFIX);
  far = fopen (sfilename, "r");
  if (far != NULL)
    {
      sprintf (message, "temporary file %s already exists. ", sfilename); 
      ANOVA_error (message);
    }
  
}


/*---------------------------------------------------------------------------*/
/*
  Routine to check whether one output file already exists.
*/

void check_one_output_file (anova_options * option_data, char * filename)
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
int max (int n1, int n2)
{
  if (n1 > n2)
    return n1;
  else
    return n2;
}

   
/*---------------------------------------------------------------------------*/
/*
  Routine to determine if sufficient disk space exists for storing
  the temporary data files.
*/

void check_disk_space (anova_options * option_data)
{
  char ch;                         /* user response */
  int nxyz;                        /* number of voxels per image */
  int nmax;                        /* maximum number of disk files */
  char filename[MAX_NAME_LENGTH];  /* output file name */ 


  /*----- initialize local variables -----*/
  nxyz = option_data->nxyz;

  /*----- first, determine the maximum number of files required -----*/
  nmax = required_data_files (option_data);

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
  Routine to read one AFNI data set from file 'filename'. 
  The data is converted to floating point (in ffim).
*/

void read_afni_data (anova_options * option_data, 
		     char * filename, float * ffim)
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


/*------------------------------------------------------------------------
  Convert one volume to another type, autoscaling:
     nxy   = # voxels
     itype = input datum type
     ivol  = pointer to input volume
     otype = output datum type
     ovol  = pointer to output volume (again, must be pre-malloc-ed)
  Return value is the scaling factor used (0.0 --> no scaling).
--------------------------------------------------------------------------*/

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
  Routine to write one AFNI data set.
  
  This data set may be either 'fitt' type (intensity + t-statistic)
                           or 'fift' type (intensity + F-statistic).
  
  The intensity data is in ffim, and the corresponding statistic is in ftr.
  
  numdof = numerator degrees of freedom
  dendof = denominator degrees of freedom
  
  Note:  if dendof = 0, then write 'fitt' type data set;
         if dendof > 0, then write 'fift' type data set.
*/

void write_afni_data (anova_options * option_data,  char * filename,  
                      float * ffim,  float * ftr,  int numdof,  int dendof)
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
  short * tsp;                        /* 2nd sub-brick data pointer */
  void  * vdif;                       /* 1st sub-brick data pointer */
  int func_type;                      /* afni data set type */
  float top, func_scale_short;        /* parameters for scaling data */
  
  
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
  
  if (dendof == 0) func_type = FUNC_TT_TYPE;
  else func_type = FUNC_FT_TYPE;
  
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
  
#define TOP_SS  32700
  
  if (dendof == 0)   /* t-statistic */
    { 
      top = TOP_SS/FUNC_TT_SCALE_SHORT;
      func_scale_short = FUNC_TT_SCALE_SHORT;
    }
  else               /* F-statistic */
    {
      top = TOP_SS/FUNC_FT_SCALE_SHORT;
      func_scale_short = FUNC_FT_SCALE_SHORT;
    }
  
  for (ii = 0;  ii < nxyz;  ii++)
    {
      if (ftr[ii] > top)
	tsp[ii] = TOP_SS;
      else  if (ftr[ii] < -top)
	tsp[ii] = -TOP_SS;
      else  if (ftr[ii] >= 0.0)
	tsp[ii] = (short) (func_scale_short * ftr[ii] + 0.5);
      else
	tsp[ii] = (short) (func_scale_short * ftr[ii] - 0.5);

    }
  

  /*----- write afni data set -----*/
  printf("--- Writing combined dataset into %s\n",
	 new_dset->dblk->diskptr->header_name) ;
  
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
  Routine to release memory allocated for anova_options.
*/

void destroy_anova_options (anova_options * option_data)
{
  int i, j, k, m;

  
  /*----- deallocate memory -----*/
  for (i = 0;  i < option_data->a;  i++)
    for (j = 0;  j < option_data->b;  j++)
      for (k = 0;  k < option_data->c;  k++)
	for (m = 0;  m < option_data->n;  m++)   
	  {
	    free (option_data->xname[i][j][k][m]);
	    option_data->xname[i][j][k][m] = NULL;
	  }

  free (option_data->first_dataset);
  option_data->first_dataset = NULL;

  if (option_data->nftr)
    {
      free (option_data->ftrname);
      option_data->ftrname = NULL;
    }
  if (option_data->nfa)    
    {
      free (option_data->faname);
      option_data->faname = NULL;
    }
  if (option_data->nfb)
    {
      free (option_data->fbname);
      option_data->fbname = NULL;
    }
  if (option_data->nfc)
    {
      free (option_data->fcname);
      option_data->fcname = NULL;
    }
  if (option_data->nfab)
    {
      free (option_data->fabname);
      option_data->fabname = NULL;
    }
  if (option_data->nfac)
    {
      free (option_data->facname);
      option_data->facname = NULL;
    }
  if (option_data->nfbc)
    {
      free (option_data->fbcname);
      option_data->fbcname = NULL;
    }
  if (option_data->nfabc)
    {
      free (option_data->fabcname);
      option_data->fabcname = NULL;
    }

  if (option_data->num_ameans)
    for (i=0; i < option_data->num_ameans; i++)  
      {
	free (option_data->amname[i]);
	option_data->amname[i] = NULL;
      }
  
  if (option_data->num_bmeans)
    for (i=0; i < option_data->num_bmeans; i++)  
      {
	free (option_data->bmname[i]);
	option_data->bmname[i] = NULL;
      }
  
  if (option_data->num_cmeans)
    for (i=0; i < option_data->num_cmeans; i++)  
      {
	free (option_data->cmname[i]);
	option_data->cmname[i] = NULL;
      }
  
  if (option_data->num_adiffs)
    for (i=0; i < option_data->num_adiffs; i++)  
      {
	free (option_data->adname[i]);
	option_data->adname[i] = NULL;
      }
  
  if (option_data->num_bdiffs)
    for (i=0; i < option_data->num_bdiffs; i++)  
      {
	free (option_data->bdname[i]);
	option_data->bdname[i] = NULL;
      }
  
  if (option_data->num_cdiffs)
    for (i=0; i < option_data->num_cdiffs; i++)  
      {
	free (option_data->cdname[i]);
	option_data->cdname[i] = NULL;
      }
  
  if (option_data->num_acontr)
    for (i=0; i < option_data->num_acontr; i++)  
      {
	free (option_data->acname[i]);
	option_data->acname[i] = NULL;
      }
  
  if (option_data->num_bcontr)
    for (i=0; i < option_data->num_bcontr; i++)  
      {
	free (option_data->bcname[i]);
	option_data->bcname[i] = NULL;
      }

  if (option_data->num_ccontr)
    for (i=0; i < option_data->num_ccontr; i++)  
      {
	free (option_data->ccname[i]);
	option_data->ccname[i] = NULL;
      }
}
  






















