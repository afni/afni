
/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
   This is the header file for the 3dANOVA.lib library routines.

   File:    3dANOVA.h
   Author:  B. D. Ward
   Date:    20 January 1997

   Mod:     Added command -diskspace to print out how much disk space is
            required to execute the problem.
   Date:    23 January 1997

   Mod:     Change to routine write_afni_data to reduce truncation error.
   Date:    27 January 1997
	
   Mod:     Added machine specific code for checking disk space.
   Date:    29 January 1997

   Mod:     Extensive changes required to implement the 'bucket' dataset.
   Date:    30 December 1997

   Mod:     Library routines moved to 3dANOVA.lib.
   Date:    5 January 1998

   Mod:     Added software for statistical tests of individual cell means,
            cell differences, and cell contrasts.
   Date:    27 October 1998

   Mod:     Added changes for incorporating History notes.
   Date:    09 September 1999

   Mod:     Replaced dataset input code with calls to THD_open_dataset,
            to allow operator selection of individual sub-bricks for input.
   Date:    02 December 1999

   Mod:     Increased maximum number of user requested means, differences,
            and contrasts.
   Date:    31 January 2000

   Mod:     Set MAX_NAME_LENGTH equal to THD_MAX_NAME.
   Date:    02 December 2002

   Mod:     Added aBcontr/Abcontr fields to the anova_options struct.
   Date:    14 October 2005 [rickr]

   Mod:     Added old_method flag to anova_options struct.
            Added prototype for contrasts_are_valid(), and ANOVA_MODS_LINK.
   Date:    01 December 2005 [rickr]

   Mod:     Added debug field to anova_options struct.
   Date:    08 December 2005 [rickr]

   Mod:     Added aBdiff, Abdiff and abmean fields to anova_options struct.
            Added ANOVA_BOUND() macro.
   Date:    16 December 2005 [rickr]

   Mod:     Added mask and nmask fields to anova_options struct.
   Date:    11 Mar 2009 [RWCox]
*/

/*---------------------------------------------------------------------------*/

#include "mrilib.h"

static char * commandline = NULL ;         /* command line for history notes */

/* documentation about modifications to ANOVA computations */
#define ANOVA_MODS_LINK "http://afni.nimh.nih.gov/sscc/gangc/ANOVA_Mod.html"

/*** HP-UX ***/
#ifdef HP
# define DF "bdf ."
#endif

/*** SGI IRIX ***/
#ifdef SGI
# define DF "df -k ."
#endif

/*** DEC OSF1 ***/
#ifdef OSF1
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


#define MAX_LEVELS 300           /* max. number of factor levels */
#define MAX_OBSERVATIONS 300     /* max. number of observations per cell */
#define MAX_MEANS 50             /* max. number of user requested means */
#define MAX_DIFFS 50             /* max. number of user requested diffs. */
#define MAX_CONTR 75             /* max. number of user requested contrasts */
#define MAX_NAME_LENGTH THD_MAX_NAME  /* max. string length for file names */


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

  int   old_method;             /* flag indicating use of old functionality
                                     bits: 001 = old_method
                                           010 = OK
                                           100 = assume sphericity
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
  int   ameans[MAX_MEANS];      /* calc means for these factor A levels */
  char  * amname[MAX_MEANS];    /* names of output files for factor A means */

  int   num_bmeans;             /* number of factor B level means */
  int   bmeans[MAX_MEANS];      /* calc means for these factor B levels */
  char  * bmname[MAX_MEANS];    /* names of output files for factor B means */

  int   num_cmeans;             /* number of factor C level means */
  int   cmeans[MAX_MEANS];      /* calc means for these factor C levels */
  char  * cmname[MAX_MEANS];    /* names of output files for factor C means */

  int   num_xmeans;             /* number of cell means */
  int   xmeans[MAX_MEANS][3];   /* calc means for these cells */
  char  * xmname[MAX_MEANS];    /* name of output files for cell means */

  int   num_adiffs;             /* num of diffs in factor A level means */
  int   adiffs[MAX_DIFFS][2];   /* calc diffs in these factor A level means */
  char  * adname[MAX_DIFFS];    /* names of output files for A differences */

  int   num_bdiffs;             /* num of diffs in factor B level means */
  int   bdiffs[MAX_DIFFS][2];   /* calc diffs in these factor B level means */
  char  * bdname[MAX_DIFFS];    /* names of output files for B differences */

  int   num_cdiffs;             /* num of diffs in factor C level means */
  int   cdiffs[MAX_DIFFS][2];   /* calc diffs in these factor C level means */
  char  * cdname[MAX_DIFFS];    /* names of output files for C differences */

  int   num_xdiffs;               /* num of diffs in cell means */
  int   xdiffs[MAX_DIFFS][2][3];  /* calc diffs in these cell means */
  char  * xdname[MAX_DIFFS];      /* names of output files for cell diffs */

  int   num_acontr;             /* number of factor A contrasts */
  float acontr[MAX_CONTR][MAX_LEVELS];     /* factor A contrast vectors */
  char  * acname[MAX_CONTR];    /* names of output files for A contrasts */

  int   num_bcontr;             /* number of factor B contrasts */
  float bcontr[MAX_CONTR][MAX_LEVELS];     /* factor B contrast vectors */
  char  * bcname[MAX_CONTR];    /* names of output files for B contrasts */

  int   num_ccontr;             /* number of factor C contrasts */
  float ccontr[MAX_CONTR][MAX_LEVELS];     /* factor C contrast vectors */
  char  * ccname[MAX_CONTR];    /* names of output files for C contrasts */

  int   num_xcontr;             /* number of contrasts of cell means */
  float xcontr[MAX_CONTR][MAX_LEVELS][MAX_LEVELS];
                                /* cell means contrast vectors */
  char  * xcname[MAX_CONTR];    /* names of output files for cell contrasts */

  /* second order contrasts for 3dANOVA3 (e.g. -aBcontr) 14 Oct 2005 [rickr] */
  int   num_aBcontr;            /* number of A contrasts at fixed B level */
  float aBcontr[MAX_CONTR][MAX_LEVELS];        /* factor contrast vectors */
  int   aBclevel[MAX_CONTR];    /* factor B level for aB contrasts        */
  char  * aBcname[MAX_CONTR];   /* names of output files for aB contrasts */

  int   num_Abcontr;            /* number of B contrasts at fixed A level */
  float Abcontr[MAX_CONTR][MAX_LEVELS];        /* factor contrast vectors */
  int   Abclevel[MAX_CONTR];    /* factor A level for Ab contrasts        */
  char  * Abcname[MAX_CONTR];   /* names of output files for Ab contrasts */

  /* second order diffs for 3dANOVA3 (e.g. -aBdiff)     16 Dec 2005 [rickr] */
  int   num_aBdiffs;            /* number of A diffs at fixed B level       */
  float aBdiffs[MAX_DIFFS][2];  /* calc diffs in these factor A level means */
  int   aBdlevel[MAX_DIFFS];    /* fixed factor B level for each diff       */
  char  * aBdname[MAX_DIFFS];   /* names of output files for aB diffs       */

  int   num_Abdiffs;            /* number of B diffs at fixed A level       */
  float Abdiffs[MAX_DIFFS][2];  /* calc diffs in these factor B level means */
  int   Abdlevel[MAX_DIFFS];    /* fixed factor A level for each diff       */
  char  * Abdname[MAX_DIFFS];   /* names of output files for Ab diffs       */

  /* -abmeans: at fixed A level and fixed B level       16 Dec 2005 [rickr] */
  int   num_abmeans;            /* number of AB level means */
  int   abmeans[MAX_MEANS][2];  /* calc means at each A-level B-level pair  */
  char  * abmname[MAX_MEANS];   /* names of output files for AB means */

  char * bucket_filename;       /* file name for bucket dataset */

  int    debug;                 /* for more verbose output */

  int    nmask ;
  byte   *mask ;
} anova_options;


/*---------------------------------------------------------------------------*/
/*
   Routine to initialize input options.
*/

void initialize_options (anova_options * option_data);


/*---------------------------------------------------------------------------*/

/** macro to open a dataset and make it ready for processing **/

#define DOPEN(ds,name)                                                        \
   do{ int pv ; (ds) = THD_open_dataset((name)) ;                             \
       if( !ISVALID_3DIM_DATASET((ds)) ){                                     \
          fprintf(stderr,"*** Can't open dataset: %s\n",(name)) ; exit(1) ; } \
       if( (ds)->daxes->nxx!=nx || (ds)->daxes->nyy!=ny ||                    \
          (ds)->daxes->nzz!=nz ){                                             \
          fprintf(stderr,"*** Axes mismatch: %s\n",(name)) ; exit(1) ; }      \
       if( DSET_NVALS((ds)) != 1 ){                                           \
         fprintf(stderr,"*** Must specify 1 sub-brick: %s\n",(name));exit(1);}\
       THD_load_datablock( (ds)->dblk ) ;                                     \
       pv = DSET_PRINCIPAL_VALUE((ds)) ;                                      \
       if( DSET_ARRAY((ds),pv) == NULL ){                                     \
          fprintf(stderr,"*** Can't access data: %s\n",(name)) ; exit(1); }   \
       if( DSET_BRICK_TYPE((ds),pv) == MRI_complex ){                         \
          fprintf(stderr,"*** Can't use complex data: %s\n",(name)) ; exit(1);\
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


/** macro to bound a number **/
#define ANOVA_BOUND(var,bot,top) do{         \
        if((var)<(bot)) (var)=(bot);         \
        else if((var)>(top)) (var)=(top);    \
        } while (0)

/*---------------------------------------------------------------------------*/
/*
   Routine to print error message and stop.
*/

void ANOVA_error (char * message);


/*---------------------------------------------------------------------------*/
/*
   Routine to write a 3d volume of floating point data to a (temporary)
   disk file.
*/

int volume_write_count (char * filename,  float * fout,  int size);


/*---------------------------------------------------------------------------*/
/*
   Routine to write a 3d volume of floating point data to a (temporary)
   disk file. Error exit if cannot write entire file.
*/

void volume_write (char * filename,  float * fout,  int size);


/*---------------------------------------------------------------------------*/
/*
   Routine to read a 3d volume of floating point data.
*/

void volume_read (char * filename,  float * fin,  int size);


/*---------------------------------------------------------------------------*/
/*
  Routine to delete a file containing a 3d volume of floating point data.
*/

void volume_delete (char * filename);


/*---------------------------------------------------------------------------*/
/*
  Routine to set a 3d volume of floating point data to zero.
*/

void volume_zero (float * f,  int size);


/*---------------------------------------------------------------------------*/
/*
   Routine to get the dimensions of the 3d AFNI data sets.
*/

void get_dimensions (anova_options * option_data);


/*---------------------------------------------------------------------------*/
/*
  Routine to check whether one temporary file already exists.
*/

void check_one_temporary_file (char * filename);


/*---------------------------------------------------------------------------*/
/*
  Routine to check whether one output file already exists.
*/

void check_one_output_file (anova_options * option_data, char * filename);


/*---------------------------------------------------------------------------*/
/*
  Return maximum of two integers.
*/

int max (int n1, int n2);


/*---------------------------------------------------------------------------*/
/*
  Routine to determine if sufficient disk space exists for storing
  the temporary data files.
*/

void check_disk_space (anova_options * option_data);


/*---------------------------------------------------------------------------*/
/*
  Routine to read one AFNI data set from file 'filename'.
  The data is converted to floating point (in ffim).
*/

void read_afni_data (anova_options * option_data,
		     char * filename, float * ffim);


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
                      float * ffim,  float * ftr,  int numdof,  int dendof);


/*---------------------------------------------------------------------------*/
/*
   Routine to add file name to command string.
*/

void add_file_name (THD_3dim_dataset * new_dset, char * filename,
		    char * command_str);


/*---------------------------------------------------------------------------*/
/*
   Routine to remove AFNI .HEAD and .BRIK dataset files.
*/

void remove_dataset (THD_3dim_dataset * new_dset, char * filename);


/*---------------------------------------------------------------------------*/
/*
  Routine to release memory allocated for anova_options.
*/

void destroy_anova_options (anova_options * option_data);


/*---------------------------------------------------------------------------*/
/*
  Routine to check for contrasts not summing to zero.
*/

int contrasts_are_valid (anova_options * option_data, int show_errs, int level);


/*---------------------------------------------------------------------------*/

#undef  ANOVA_FLOAT_HELP
#define ANOVA_FLOAT_HELP                                                            \
   "-------------------------------------------------------------------------\n"    \
   "STORAGE FORMAT:\n"                                                              \
   "---------------\n"                                                              \
   "The default output format is to store the results as scaled short\n"            \
   "(16-bit) integers.  This truncantion might cause significant errors.\n"         \
   "If you receive warnings that look like this:\n"                                 \
   "  *+ WARNING: TvsF[0] scale to shorts misfit = 8.09%% -- *** Beware\n"          \
   "then you can force the results to be saved in float format by\n"                \
   "defining the environment variable AFNI_FLOATIZE to be YES\n"                    \
   "before running the program.  For convenience, you can do this\n"                \
   "on the command line, as in\n"                                                   \
   "  3dANOVA -DAFNI_FLOATIZE=YES ... other options ... \n"                         \
   "Also see the following links:\n"                                                \
   " http://afni.nimh.nih.gov/pub/dist/doc/program_help/common_options.html\n"      \
   " http://afni.nimh.nih.gov/pub/dist/doc/program_help/README.environment.html\n"

/*---------------------------------------------------------------------------*/
