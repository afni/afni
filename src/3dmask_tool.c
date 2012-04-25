/* ----------------------------------------------------------------------
 * Tool for manipulation of mask datasets.
 *
 * usage: 3dmask_tool [options] dset1 dset2 ...
 *
 * options:
 *
 *      -count
 *      -datum
 *
 *      -dilate N       (can have many ordered options, where N<0 means erode)
 *                      (dilation is applied to the result, after -frac)
 *      -frac
 *      -intersect
 *      -union
 *
 *
 * - union/intersect/frac overlap
 * - dilate/erode as ordered list of operations
 *
 * Author: R Reynolds  23 Apr 2012
 */

static char * g_history[] =
{
  "----------------------------------------------------------------------\n"
  "history (of 3dmask_tool):\n"
  "\n",
  "0.0  23 Apr 2012\n"
  "     (Rick Reynolds of the National Institutes of Health, SSCC/DIRP/NIMH)\n"
  "     -initial version\n"
};

static char g_version[] = "3dmask_tool version 0.0, 23 April 2012";

#include <stdio.h>
#include "mrilib.h"

/*--------------- global parameters struct ---------------*/
typedef struct
{ /* options */
   int_list            dilations; /* list of dilations/erodes to apply      */
   THD_3dim_dataset ** dsets;     /* input and possibly modified datasets   */
   char             ** inputs;    /* list of input dataset names (in argv)  */
   char              * prefix;    /* prefix for output dataset              */
   float               frac;      /* min frac of overlap/Ndset (0=union)    */
   int                 count;     /* flag to output counts                  */
   int                 datum;     /* output data type                       */
   int                 verb;      /* verbose level                          */

   /* other parameters */
   int                ndsets;     /* number of mask datasets                */
   int                nvols;      /* number of mask volumes (>= ndsets)     */
   int                zeropad;    /* amount of zeropadding applied          */
} param_t;

param_t g_params;

/*--------------- prototypes ---------------*/
int apply_dilations     (THD_3dim_dataset *, int_list *, int);
int convert_to_bytemask (THD_3dim_dataset * dset, int verb);
int count_masks         (THD_3dim_dataset *[], int, char *, int,
                         THD_3dim_dataset **, int *, int *);
int limit_to_frac       (THD_3dim_dataset *, int, int, int);
int process_input_dsets (param_t * params);
int process_opts        (param_t *, int, char *[]);
int show_help           (void);
int write_result        (param_t *, THD_3dim_dataset *, int, char *[]);

/*--------------- overview ---------------*/
/*
 * - for each input name
 *    - open dataset
 *    - convert to bytemask
 *    - zeropad as needed
 *    - apply list of dilations
 *
 * - create mask counts
 *    - foreach dset, foreach voxel, if set: increment voxel
 *
 * - frac *= ndset; if frac == 0: frac = 1
 * - foreach voxel
 *    - if count < overlap_frac * nset: clear
 *    - else if not -count: set to 1 (else leave as count)
 *
 * - undo any zeropad
 *
 * - create dset of first type or via -datum and write
 */


/*--------------- main routine ---------------*/
int main( int argc, char *argv[] )
{
   THD_3dim_dataset * countset=NULL;
   param_t          * params = &g_params;
   int                rv, limit;

   if( argc < 1 ) { show_help();  return 0; }

   /* general stuff */
   mainENTRY("3dmask_tool"); machdep(); AFNI_logger("3dmask_tool",argc,argv);
   enable_mcw_malloc();

   /* process options: a negative return is considered an error */
   rv = process_opts(params, argc, argv);
   if( rv ) RETURN(rv < 0);

   /* open, convert to byte, zeropad, dilate, unzeropad */
   if( process_input_dsets(params) ) RETURN(1);

   /* create mask count dataset, return num volumes and maybe set datum */
   if( count_masks(params->dsets, params->ndsets,
                   params->prefix, params->verb,
                   &countset, &params->nvols, &params->datum) ) RETURN(1);

   /* limit to frac of nvols (if not counting, convert to 0/1 mask) */
   limit = ceil(params->nvols * params->frac);
   if( params->verb )
      INFO_message("frac %g over %d volumes gives min count %d\n",
                   params->frac, params->nvols, limit);
   if( limit <= 0 ) limit = 1;

   /* if not counting, result is binary 0/1 */
   if( limit_to_frac(countset, limit, params->count, params->verb) )
      RETURN(1);

   /* create output */
   if( write_result(params, countset, argc, argv) ) RETURN(1);

   /* clean up a little memory */
   DSET_delete(countset);
   free(params->dsets);

   RETURN(0);
}


/*--------------- apply dilations to mask ---------------*/
/*
 * dilations are passed as a list of +/- integers (- means erode)
 *
 * note: the dilations list should not be long (does more than 2 even
 *       make any sense?), but here they will be treated generically
 *    - foreach dilation: dilate or erode, as specified by sign
 */
int apply_dilations(THD_3dim_dataset * dset, int_list * D, int verb)
{
   byte  * bdata;
   int     index, ivol, id, dsize;
   int     nx, ny, nz;

   ENTRY("apply_dilations");

   if( !dset || !D ) ERROR_exit("missing inputs to apply_dilations");

   /* note geometry */
   nx = DSET_NX(dset);  ny = DSET_NY(dset);  nz = DSET_NZ(dset);

   /* apply the actual dilations */
   if(verb>1) INFO_message("applying dilation list to dataset");

   for( ivol=0; ivol < DSET_NVALS(dset); ivol++ ) {
      bdata = DBLK_ARRAY(dset->dblk, ivol);

      for( index=0; index < D->num; index++ ) {
         dsize = D->list[index];
         if(verb>2) INFO_message("... dilating vol %d by %d\n", ivol, dsize);
         if( dsize > 0 ) {
           for( id=0; id < dsize; id++ ) THD_mask_dilate(nx, ny, nz, bdata, 1);
         } else if( dsize < 0 ) {
           for( id=0; id > dsize; id-- ) THD_mask_erode (nx, ny, nz, bdata, 0);
         }
      }
   }

   RETURN(0);
}


/*--------------- limit to fraction of volumes ---------------*/
/*
 * check count against limit
 *    - clear small values
 *    - if not count, set large values to 1
 */
int limit_to_frac(THD_3dim_dataset * cset, int limit, int count, int verb)
{
   short * dptr;
   int     index, nsub, nsuper;

   ENTRY("limit_to_frac");

   if( ! ISVALID_3DIM_DATASET(cset) ) {
      ERROR_message("invalid count dataset");
      RETURN(1);
   } else if( DSET_BRICK_TYPE(cset, 0) != MRI_short ) {
      ERROR_message("count dataset not of type short");
      RETURN(1);
   }

   if(verb > 1) INFO_message("limiting to %d (count = %d)\n",limit,count);

   /* note how many voxels are affected, just for kicks */
   dptr = DBLK_ARRAY(cset->dblk, 0);
   nsub = nsuper = 0;
   for(index = 0; index < DSET_NVOX(cset); index++, dptr++) {
      if( ! *dptr ) continue;           /* 0, so skip */
      else if( *dptr < limit ) {        /* small, so clear */
         *dptr = 0;
         nsub++;
      }
      else {                            /* big enough */
         if ( ! count ) *dptr = 1;
         nsuper++;
      }
   }

   /* entertain the user */
   if( verb )
      INFO_message("voxel limits: %d clipped, %d survived, %d were zero\n",
                   nsub, nsuper, DSET_NVOX(cset)-nsub-nsuper);

   RETURN(0);
}


/*--------------- process input datasets ---------------*/
/*
 * for each input dataset name
 *    open
 *    convert to byte mask (still multiple volumes)
 *    zeropad as needed
 *    apply list of dilations
 *
 * also, count total volumes
 */
int process_input_dsets(param_t * params)
{
   THD_3dim_dataset * dset, * dfirst=NULL;
   int                iset, nxyz, nvol, index, sum;

   ENTRY("process_input_dsets");

   if( !params ) ERROR_exit("NULL inputs to PID");

   if( params->ndsets <= 0 ) {
      ERROR_message("process_input_dsets: no input datasets");
      RETURN(1);
   }

   /* allocate space for dsets array */
   params->dsets = (THD_3dim_dataset **)malloc(params->ndsets*
                                               sizeof(THD_3dim_dataset*));
   if( !params->dsets ) ERROR_exit("failed to allocate dset pointers");

   if( params->verb ) INFO_message("processing %d input datasets...",
                                   params->ndsets);
   
   /* compute max cumulative dilation to apply to dsets */
   params->zeropad = 0;
   for( index=0, sum=0; index < params->dilations.num; index++ ) {
      sum += params->dilations.list[index];
      if( sum > params->zeropad ) params->zeropad = sum;
   }
   if( params->verb ) INFO_message("zero padding by %d (for dilations)\n",
                                   params->zeropad);

   /* process the datasets */
   nvol = nxyz = 0;
   for( iset=0; iset < params->ndsets; iset++ ) {
      /* open and verify dataset */
      dset = THD_open_dataset(params->inputs[iset]);
      if( !dset ) ERROR_exit("failed to open mask dataset '%s'",
                             params->inputs[iset]);
      DSET_load(dset);  CHECK_LOAD_ERROR(dset);

      if( params->verb>1 ) INFO_message("loaded dset %s, with %d volumes",
                                        DSET_PREFIX(dset), DSET_NVALS(dset));

      if( nxyz == 0 ) { nxyz = DSET_NVOX(dset);  dfirst = dset; }

      /* check for consistency in voxels and grid */
      if( DSET_NVOX(dset) != nxyz ) ERROR_exit("nvoxel mis-match");
      if( ! EQUIV_GRIDS(dset, dfirst) )
         WARNING_message("grid from dset %s does not match that of dset %s",
                         DSET_PREFIX(dset), DSET_PREFIX(dfirst));

      /* convert to byte mask */
      if( convert_to_bytemask(dset, params->verb) ) RETURN(1);

      /* add to our list of dsets, either padded or not */
      if( params->zeropad > 0 ) {
         int pad = params->zeropad;
         params->dsets[iset] = THD_zeropad(dset, pad, pad, pad, pad, pad, pad,
                                           "pad", 0);
         if( dset != dfirst ) DSET_delete(dset);
      } else params->dsets[iset] = dset;

      /* now apply dilations to all volumes */
      if( apply_dilations(params->dsets[iset],&params->dilations,params->verb) )
         RETURN(1);
   } 

   /* undo any zeropadding */
   if( params->zeropad > 0 ) {
      int pad = params->zeropad;
      DSET_delete(dfirst);       /* not in dsets list and no longer needed */
      if( params->verb>1 ) INFO_message("undoing zeropad");
      for( iset=0; iset < params->ndsets; iset++ ) {
         dset = THD_zeropad(params->dsets[iset], -pad,-pad,-pad,-pad,-pad,-pad,
                            "pad", 0);
         DSET_delete(params->dsets[iset]);
         params->dsets[iset] = dset;
      }
   }

   RETURN(0);
}


/*--------------- count masks per voxel ---------------*/
/*
 * create empty count dataset
 * for each input dataset and each sub-volume
 *    for each voxel, if set: increment
 * close datasets as they are processed
 */
int count_masks(THD_3dim_dataset * dsets[], int ndsets,           /* inputs */
                char * prefix, int verb,
                THD_3dim_dataset ** cset, int * nvol, int * datum)/* outputs */
{
   THD_3dim_dataset * dset;
   short * counts = NULL;             /* will become data for returned cset */
   byte  * bptr;                      /* always points to mask volumes      */
   int     nxyz, iset, ivol, ixyz;

   ENTRY("count_masks");

   if( !dsets || !cset || !nvol || !prefix )
      ERROR_exit("NULL inputs to count_masks");

   if( ndsets <= 0 ) {
      ERROR_message("count_masks: no input datasets");
      RETURN(1);
   }

   *nvol = 0;
   nxyz = DSET_NVOX(dsets[0]);
   
   /* allocate memory for the counts */
   counts = (short *)calloc(nxyz, sizeof(short));
   if( !counts ) ERROR_exit("failed to malloc %d shorts", nxyz);

   /* for each volume of each dataset, count set voxels */
   for( iset=0; iset < ndsets; iset++ ) {
      dset = dsets[iset];
      *nvol += DSET_NVALS(dset);        /* accumulate num volumes */

      /* for each volume in this dataset, count set voxels */
      for( ivol=0; ivol < DSET_NVALS(dset); ivol++ ) {
         if( DSET_BRICK_TYPE(dset, ivol) != MRI_byte )
            ERROR_exit("in count_masks with non-byte data");

         bptr = DBLK_ARRAY(dset->dblk, ivol);
         for( ixyz = 0; ixyz < nxyz; ixyz++ ) 
            if( bptr[ixyz] ) counts[ixyz]++;
      }

      if( iset > 0 ) DSET_delete(dset); /* close the first one at end */
   }  /* dataset */

   if( verb > 1 ) {
      int maxval;
      for( maxval=counts[0], ixyz=1; ixyz < nxyz; ixyz++ )
         if( counts[ixyz] > maxval ) maxval = counts[ixyz];

      INFO_message("counted %d mask volumes in %d datasets (%d voxels)\n",
                   *nvol, ndsets, nxyz);
      INFO_message("   (maximum overlap = %d)\n", maxval);
   }

   if( *nvol >= (1<<15) )
      WARNING_message("too many volumes to count as shorts: %d", *nvol);

   /* create output dataset, and copy datum if not initialized */
   *cset = EDIT_empty_copy(dsets[0]);
   EDIT_dset_items(*cset,
          ADN_prefix, prefix,  ADN_nvals, 1,  ADN_ntt, 0,
          ADN_none);
   EDIT_substitute_brick(*cset, 0, MRI_short, counts);
   if( *datum < 0 ) *datum = DSET_BRICK_TYPE(dsets[0], 0);

   DSET_delete(dsets[0]);  /* now finished with first dataset */

   RETURN(0);
}


/*--------------- write the result dataset ---------------*/
/* convert by hand, since no scaling will be done
 * (byte seems inappropriate and float does not need it)  */
int write_result(param_t * params, THD_3dim_dataset * oset,
                 int argc, char * argv[])
{
   short * sptr;
   int     nvox = DSET_NVOX(oset), ind;

   ENTRY("write_results");

   if( params->verb )
      INFO_message("writing result %s...\n", DSET_PREFIX(oset));

   switch( params->datum ) {
      default: ERROR_exit("invalid datum for result: %d", params->datum);
      case MRI_short: break;     /* nothing to do */
      case MRI_float: {
         float * data = (float *)malloc(nvox*sizeof(float));
         sptr = DBLK_ARRAY(oset->dblk, 0);
         if( ! data ) ERROR_exit("failed to alloc %d output floats\n", nvox);
         for( ind = 0; ind < nvox; ind++ ) data[ind] = (float)sptr[ind];
         EDIT_substitute_brick(oset, 0, params->datum, data);
      }
      break;
      case MRI_byte: {
         byte * data = (byte *)malloc(nvox*sizeof(byte));
         int errs = 0;
         sptr = DBLK_ARRAY(oset->dblk, 0);
         if( ! data ) ERROR_exit("failed to alloc %d output bytes\n", nvox);
         for( ind = 0; ind < nvox; ind++ ) {
            if( sptr[ind] > 255 ) {     /* watch for overflow */
               data[ind] = (byte)255;
               errs++;
            } else data[ind] = (byte)sptr[ind];
         }
         EDIT_substitute_brick(oset, 0, params->datum, data);
         if(errs) WARNING_message("convert to byte: %d truncated voxels",errs);
      }
      break;
   }

   tross_Make_History( "3dmask_tool", argc, argv, oset );

   DSET_write(oset);
   WROTE_DSET(oset);

   RETURN(0);
}

int show_help(void)
{
   ENTRY("show_help");

   printf(
   "-------------------------------------------------------------------------\n"
   "Help me...\n"
   "\n"
   "\n"
   "----------------------------------------\n"
   "examples:\n"
   "\n"
   "    3dmatmult -inputA matrix+orig -inputB image+orig  \\\n"
   "              -prefix transformed -datum float -verb 2\n"
   "\n"
   "----------------------------------------\n"
   "informational command arguments (execute option and quit):\n"
   "\n"
   "    -help                   : show this help\n"
   "    -hist                   : show program history\n"
   "    -ver                    : show program version\n"
   "\n"
   "----------------------------------------\n"
   "optional command arguments:\n"
   "\n"
   "    -datum TYPE             : specify verbosity level\n"
   "\n"
   "        Valid TYPEs are 'byte', 'short' and 'float'.  The default is\n"
   "        that of the inputB dataset.\n"
   "\n"
   "    -verb LEVEL             : specify verbosity level\n"
   "\n"
   "        The default level is 1, while 0 is considered 'quiet'.\n"
   "\n"
   "----------------------------------------\n"
   "* comments\n"
   "----------------------------------------------------------------------\n"
   "R. Reynolds         April, 2012\n"
   "\n"
   );

   RETURN(0);
}

/* ----------------------------------------------------------------------
 * fill the param_t struct
 * return 1 on (acceptable) termination
 *        0 on continue
 *       -1 on error
 */
int process_opts(param_t * params, int argc, char * argv[] )
{
   int ac, ival;

   ENTRY("process_opts");

   memset(params, 0, sizeof(param_t));  /* init everything to 0 */
   params->inputs = NULL;
   params->prefix = NULL;
   init_int_list(&params->dilations, 0);

   params->frac = -1.0;
   params->datum = -1;                    /* valid datum start at 0 */
   params->verb = 1;
   params->ndsets = 0;

   if( argc < 2 ) { show_help();  RETURN(1); }

   ac = 1;
   while( ac < argc ) {

      /* check for terminal options */
      if( strcmp(argv[ac],"-help") == 0 ) {
         show_help();
         RETURN(1);
      } else if( strcmp(argv[ac],"-hist") == 0 ) {
         int c, len = sizeof(g_history)/sizeof(char *);
         for( c = 0; c < len; c++) fputs(g_history[c], stdout);
         putchar('\n');
         RETURN(1);
      } else if( strcmp(argv[ac],"-ver") == 0 ) {
         puts(g_version);
         RETURN(1);
      }

      /* the remaining options are alphabetical */

      else if( strcmp(argv[ac],"-count") == 0 ) {
         params->count = 1;
         ac++; continue;
      }

      else if( strcmp(argv[ac],"-datum") == 0 ) {
         if( ++ac >= argc ) ERROR_exit("need argument after '-datum'");

         /* output datum can be byte, short or float */
         if     ( ! strcmp(argv[ac],"byte")  ) params->datum = MRI_byte;
         else if( ! strcmp(argv[ac],"short") ) params->datum = MRI_short;
         else if( ! strcmp(argv[ac],"float") ) params->datum = MRI_float;
         else ERROR_exit("-datum '%s' is not supported", argv[ac]);

         ac++; continue;
      }

      /* read in a list of dilations (negatives are erosions) */
      else if( strcmp(argv[ac],"-dilate") == 0 ) {
         char * rptr; /* return pointer for strtol */
         int    ndilates = 0;

         if( ++ac >= argc ) ERROR_exit("need argument after '-dilate'");

         ival = strtol(argv[ac], &rptr, 10);
         while( ac < argc && rptr > argv[ac] ) {
            if( ! add_to_int_list(&params->dilations, ival, 1) ) RETURN(-1);
            ndilates++;
            ac++;
            ival = strtol(argv[ac], &rptr, 10);
         }

         if( ndilates == 0 )
            ERROR_exit("no integral dilations found after -dilate");

         /* ac is already past last number */ continue;
      }

      /* overlap: -frac, -inter, -union */
      else if( strncmp(argv[ac],"-frac", 5) == 0 ) {
         if( ++ac >= argc ) ERROR_exit("need argument after '-frac'");

         params->frac = atof(argv[ac]);
         if( params->frac < 0.0 )
            ERROR_exit("have -frac < 0 (from %s)", argv[ac]);

         ac++; continue;
      }
      else if( strncmp(argv[ac],"-inter", 6) == 0 ) {
         params->frac = 0.0;
         ac++; continue;
      }
      else if( strcmp(argv[ac],"-union") == 0 ) {
         params->frac = 1.0;
         ac++; continue;
      }

      else if( strcmp(argv[ac],"-inputs") == 0 ) {
         /* store list of names from argv */
         ac++;

         params->inputs = argv+ac;      /* pointer to first name */
         params->ndsets = 0;            /* number of datasets    */
         while( ac < argc && argv[ac][0] != '-' ){ params->ndsets++; ac++; }

         if( params->ndsets == 0 ) ERROR_exit("need datasets after '-inputs'");

         /* already incremented: ac++; */  continue;
      }

      else if( strcmp(argv[ac],"-prefix") == 0 ) {
         if( ++ac >= argc ) ERROR_exit("need argument after '-prefix'");

         params->prefix = argv[ac];

         if( !THD_filename_ok(params->prefix) )
            ERROR_exit("Illegal name after -prefix: %s", argv[ac]);
         ac++; continue;
      }

      else if( strcmp(argv[ac],"-quiet") == 0 ) { /* -quiet means -verb 0 */
         params->verb = 0;
         ac++; continue;
      }

      else if( strcmp(argv[ac],"-verb") == 0 ) {
         if( ++ac >= argc ) ERROR_exit("need argument after '-verb'");

         params->verb = atoi(argv[ac]);
         ac++; continue;
      }

      ERROR_message("** unknown option '%s'\n",argv[ac]);
      RETURN(-1);
       
   }

   if( params->dilations.num > 0 ) {
      int * ilist = params->dilations.list;
      int   err = 0;
      for( ival = 0; ival < params->dilations.num; ival++ )
         if( ilist[ival] == 0 ) {
            ERROR_message("dilation[%d] is zero", ival);
            err++;
         }
      for( ival = 1; ival < params->dilations.num; ival++ )
         if( ilist[ival-1]*ilist[ival] > 0){
            ERROR_message(
               "have sequential dilations of same sign (assuming mistake)\n"
               "   (d[%d] = %d, d[%d] = %d)\n",
               ival-1,ilist[ival-1],ival,ilist[ival]);
            err++;
         }
      if( err ) RETURN(-1);
   }

   if( params->ndsets <= 0 ) ERROR_exit("missing -input dataset list");
   if( !params->prefix ) ERROR_exit("missing -prefix option");
   if( params->frac < 0 ) ERROR_exit("missing -frac option (or -inter/-union)");

   if( params->verb > 1 )
      INFO_message("%d datasets specified, frac = %g, %d dilation(s)\n",
                   params->ndsets, params->frac, params->dilations.num);

   RETURN(0);
}

/*--------------- convert each dset volume to byte mask ---------------*/
int convert_to_bytemask(THD_3dim_dataset * dset, int verb)
{
   byte * bdata;
   int    ivol, nxyz, ixyz;

   ENTRY("convert_to_bytemask");

   if( !dset ) { ERROR_message("CDTB: no dset"); RETURN(1); }

   nxyz = DSET_NVOX(dset);

   /* convert to byte mask */
   for( ivol=0; ivol < DSET_NVALS(dset); ivol++ ) {

      if( verb>2 ) INFO_message("converting vol %d to byte mask", ivol);

      bdata = (byte *)malloc(nxyz * sizeof(byte));
      if( !bdata ) ERROR_exit("failed to alloc %d bytes: dset %s, vol %d\n",
                              nxyz, DSET_PREFIX(dset), ivol);

      /* fill bdata based on input datum */
      switch( DSET_BRICK_TYPE(dset, ivol) ) {
         default: ERROR_exit("illegal datum %d", DSET_BRICK_TYPE(dset,ivol));
         case MRI_byte: { /* count set voxels in this volume */
            byte * dptr = DBLK_ARRAY(dset->dblk, ivol);
            for( ixyz = 0; ixyz < nxyz; ixyz++ )
               if( dptr[ixyz] ) bdata[ixyz] = 1;
               else             bdata[ixyz] = 0;
            break;
         }
         case MRI_short: { /* count set voxels in this volume */
            short * dptr = DBLK_ARRAY(dset->dblk, ivol);
            for( ixyz = 0; ixyz < nxyz; ixyz++ )
               if( dptr[ixyz] ) bdata[ixyz] = 1;
               else             bdata[ixyz] = 0;
            break;
         }
         case MRI_float: { /* count set voxels in this volume */
            float * dptr = DBLK_ARRAY(dset->dblk, ivol);
            for( ixyz = 0; ixyz < nxyz; ixyz++ )
               if( dptr[ixyz] ) bdata[ixyz] = 1;
               else             bdata[ixyz] = 0;
            break;
         }
      }  /* switch */

      /* replace old data with new */
      EDIT_substitute_brick(dset, ivol, MRI_byte, bdata);
   }  /* volume */

   RETURN(0);
}
