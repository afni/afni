/* ----------------------------------------------------------------------
 * Tool for manipulation of mask datasets.
 *
 * usage: 3dmask_tool [options] dset1 dset2 ...
 *
 * Allow users to read multiple datasets as masks, optionally dilate
 * and/or erode them, compute a mask from a fractional overlap, optionally
 * dilate and/or erode that, and write the result.
 *
 * - union/intersect/frac overlap
 * - dilate/erode as ordered list of operations
 *
 * Author: R Reynolds  27 Apr 2012
 */

static char * g_history[] =
{
  "----------------------------------------------------------------------\n"
  "history (of 3dmask_tool):\n"
  "\n",
  "0.0  27 Apr 2012\n"
  "     (Rick Reynolds of the National Institutes of Health, SSCC/DIRP/NIMH)\n"
  "     -initial version\n",
  "0.1   7 May 2012\n"
  "     - replaced THD_mask_erode with new THD_mask_erode_sym\n"
  "       (now dilations and erosions should be symmetric)\n"
  "0.2   5 Jun 2012\n"
  "     - explicit set of DSET_BRICK_TYPE() needed on F8 system?\n"
  "     - make empty copy for grid test\n",
  "0.3   30 Jul 2013\n"
  "     - in apply_dilations: split out count/apply_affected_voxels\n"
  "     - fixes a failure to apply a negative dilation in non-convert case\n"
  "       (thanks to W Gaggl for noting the problematic scenario)\n",
  "0.4   1 Aug 2013\n"
  "     - Fixed actual problem W Gaggl was seeing, apparent pointer step\n"
  "       issue that might work on one system but fail on another (with the\n"
  "       identical binary, ick).\n"
  "       So re-wrote the troubling part.\n"
  "0.5  22 Oct 2014: if zeropadding for dilations, reset ijk_to_dicom_real\n",
  "       to preserve any oblique matrix\n"
  "0.6  11 Dec 2017: fix result-dilation bug, noted by mwlee on MB\n",
  "     - if pad but not convert, inset == dnew, so do not delete\n"
};

static char g_version[] = "3dmask_tool version 0.6, 11 December 2017";

#include "mrilib.h"

/*--------------- global parameters struct ---------------*/
typedef struct
{ /* options */
   int_list            IND;       /* dilations/erodes to apply to inputs    */
   int_list            RESD;      /* dilations/erodes to apply to result    */
   THD_3dim_dataset ** dsets;     /* input and possibly modified datasets   */
   char             ** inputs;    /* list of input dataset names (in argv)  */
   char              * prefix;    /* prefix for output dataset              */
   float               frac;      /* min frac of overlap/Ndset (0=union)    */
   int                 count;     /* flag to output counts                  */
   int                 fill;      /* flag to fill holes in mask             */
   char              * fill_dirs; /* directions to apply hole filling over  */
   int                 datum;     /* output data type                       */
   int                 verb;      /* verbose level                          */

   /* other parameters */
   int                ndsets;     /* number of mask datasets                */
   int                nvols;      /* number of mask volumes (>= ndsets)     */
   int                zeropad;    /* amount of zeropadding applied          */
} param_t;

param_t g_params;

/*--------------- prototypes ---------------*/
THD_3dim_dataset * apply_dilations (THD_3dim_dataset *, int_list *, int, int);
int apply_affected_voxels(void *, void *, int, byte *, int);
int count_affected_voxels(void *, int, byte *, int, int);
int convert_to_bytemask (THD_3dim_dataset * dset, int verb);
int count_masks         (THD_3dim_dataset *[], int, int,
                         THD_3dim_dataset **, int *);
int dilations_are_valid (int_list *);
int fill_holes          (THD_3dim_dataset *, char * axes, int);
int limit_to_frac       (THD_3dim_dataset *, int, int, int);
int needed_padding      (int_list *);
int process_input_dsets (param_t * params);
int process_opts        (param_t *, int, char *[]);
int set_axis_directions(THD_3dim_dataset * dset, char * axes, THD_ivec3 * dirs);
int show_help           (void);
int write_result        (param_t *, THD_3dim_dataset *, int, char *[]);

/*--------------- overview ---------------*/
/*
 * - for each input name
 *    - open dataset
 *    - convert to bytemask
 *    - zeropad as needed
 *    - apply list of dilations
 *    - undo any zeropad as needed
 *
 * - create mask counts
 *    - foreach dset, foreach voxel, if set: increment voxel
 *
 * - frac *= ndset; if frac == 0: frac = 1
 * - foreach voxel
 *    - if count < overlap_frac * nset: clear
 *    - else if not -count: set to 1 (else leave as count)
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

   /* create mask count dataset and return num volumes (delete old dsets) */
   if( count_masks(params->dsets, params->ndsets, params->verb,
                   &countset, &params->nvols) ) RETURN(1);

   /* limit to frac of nvols (if not counting, convert to 0/1 mask) */
   limit = ceil((params->frac>1) ? params->frac : params->nvols*params->frac );
   if( params->verb )
      INFO_message("frac %g over %d volumes gives min count %d\n",
                   params->frac, params->nvols, limit);
   if( limit <= 0 ) limit = 1;

   /* if not counting, result is binary 0/1 */
   if( limit_to_frac(countset, limit, params->count, params->verb) )
      RETURN(1);

   /* maybe apply dilations to output */
   if( params->RESD.num > 0 ) {
      THD_3dim_dataset * crm = countset;
      countset = apply_dilations(countset, &params->RESD, 0, params->verb);
      DSET_delete(crm);
 
      if( !countset ) RETURN(1);
   }

   /* maybe fill any remaining holes */
   if( params->fill )
      if ( fill_holes(countset, params->fill_dirs, params->verb) ) RETURN(1);

   /* create output */
   if( write_result(params, countset, argc, argv) ) RETURN(1);

   /* clean up a little memory */
   DSET_delete(countset);
   free(params->dsets);

   RETURN(0);
}


/*--------------- apply dilations to dataset ---------------*/
/*
 * 1. zeropad (if needed for dilations)
 * 2. make byte copy of dataset (if not already)
 * 3. dilate (using binary mask)
 * 4. if not converting, modify original data
 * 5. undo any zeropad
 * 6. return new dataset
 * 
 * dilations are passed as a list of +/- integers (- means erode)
 *
 * convert: flag specifying whether dset should be converted to MRI_byte
 *
 * note: dilations list should be short, but treat generically
 *       - foreach dilation: dilate or erode, as specified by sign
 */
THD_3dim_dataset * apply_dilations(THD_3dim_dataset * dset, int_list * D,
                                   int convert, int verb)
{
   THD_3dim_dataset * dnew = NULL, * inset = NULL;
   byte             * bdata = NULL;
   int                index, ivol, id, dsize, datum, pad;
   int                nx, ny, nz, nvox;

   ENTRY("apply_dilations");

   if( !dset || !D ) ERROR_exit("missing inputs to apply_dilations");

   /* note and apply any needed zeropadding */
   pad = needed_padding(D);
   if(verb > 3 && pad) INFO_message("padding by %d (for dilations)", pad);
   if( pad ) inset = THD_zeropad(dset, pad, pad, pad, pad, pad, pad, "pad", 0);
   else      inset = dset;

   /* ---- make sure to start with a new dataset, possibly to return ---- */
   /*      inset: original data, padded or not, input for calculations    */
   /*      dnew:  dataset to edit or assign to, output from calculations  */

   /* make a byte output dataset, use padded dset, or make new copy */
   if( convert ) {
      dnew = EDIT_empty_copy(inset);
      EDIT_dset_items(dnew, ADN_datum_all, MRI_byte, ADN_none);
   } else if ( pad ) dnew = inset;
   else              dnew = EDIT_full_copy(inset, "salmon");

   /* note geometry */
   nx = DSET_NX(dnew);  ny = DSET_NY(dnew);  nz = DSET_NZ(dnew);
   nvox = nx*ny*nz;

   /* now apply the actual dilations */
   if(verb>1) INFO_message("applying dilation list to dataset");

   for( ivol=0; ivol < DSET_NVALS(inset); ivol++ ) {
      datum = DSET_BRICK_TYPE(inset, ivol);
      /* if non-byte data (short/float), make byte mask of volume */
      if( datum != MRI_byte && datum != MRI_float && datum != MRI_short ) {
         ERROR_message("invalid datum for mask input: %d", datum);
         RETURN(NULL);
      }

      /* start with a clean mask */
      bdata = THD_makemask(inset, ivol, 1, 0);
      if( !bdata ) { ERROR_message("failed to make as mask"); RETURN(NULL); }

      /* apply dilations to mask */
      for( index=0; index < D->num; index++ ) {
         dsize = D->list[index];
         if(verb>2) INFO_message("... dilating vol %d by %d\n", ivol, dsize);
         if( dsize > 0 ) {
            for( id=0; id < dsize; id++ )
               THD_mask_dilate(nx, ny, nz, bdata, 1);
         } else if( dsize < 0 ) {
            for( id=0; id > dsize; id-- )
               THD_mask_erode_sym(nx, ny, nz, bdata, 1);
         }
      }

      /* split out count/apply_affected_voxels    30 Jul 2013 [rickr] */
      /* -- this also fixes a failure to apply a negative dilation in */
      /*    the non-convert case */

      /* count affected voxels */
      if( count_affected_voxels(DBLK_ARRAY(inset->dblk, ivol), datum, bdata,
                                nvox, verb) ) RETURN(NULL);

      /* if we are converting, just replace the old data */
      if( convert ) {
         if( verb > 2 ) INFO_message("applying byte result from dilate");
         EDIT_substitute_brick(dnew, ivol, MRI_byte, bdata);
      } else { /* apply mask changes, and nuke mask */
         if( apply_affected_voxels(DBLK_ARRAY(dnew->dblk,ivol),
                                   DBLK_ARRAY(inset->dblk,ivol),
                                   datum, bdata, nvox) ) RETURN(NULL);
         free(bdata);
      }
   }

   /* undo any zeropadding (delete original and temporary datasets) */
   if( pad ) {
      /* if pad and not convert, dnew == inset    11 Dec 2017 [rickr] */
      if( dnew != inset ) DSET_delete(inset);
      inset = THD_zeropad(dnew, -pad, -pad, -pad, -pad, -pad, -pad, "pad", 0);
      DSET_delete(dnew);
      dnew = inset;

      /* copy original dicom_real, in case padding nuked oblique matrix */
      dnew->daxes->ijk_to_dicom_real = dset->daxes->ijk_to_dicom_real;
   }

   RETURN(dnew);
}


/*--------------- apply voxels affected by mask operation ---------------*/
/* note: indata and outdata point to same values, but may or may not be  */
/* the same pointers, so assume outdata is new, and edit as if indata    */
int apply_affected_voxels(void * outdata, void * indata,
                          int datum, byte * mask, int nvox)
{
   int index;

   ENTRY("apply_affected_voxels");

   if( datum == MRI_byte ) {
      byte * iptr = (byte *)indata;
      byte * optr = (byte *)outdata;
      for( index = 0; index < nvox; index++ )
         if     ( mask[index] && ! iptr[index] ) optr[index] = 1;
         else if( ! mask[index] && iptr[index] ) optr[index] = 0;
   } else if( datum == MRI_short ) {
      short * iptr = (short *)indata;
      short * optr = (short *)outdata;
      for( index = 0; index < nvox; index++ )
         if     ( mask[index] && ! iptr[index] ) optr[index] = 1;
         else if( ! mask[index] && iptr[index] ) optr[index] = 0;
   } else if( datum == MRI_float ) {
      float * iptr = (float *)indata;
      float * optr = (float *)outdata;
      for( index = 0; index < nvox; index++ )
         if     ( mask[index] && ! iptr[index] ) optr[index] = 1.0;
         else if( ! mask[index] && iptr[index] ) optr[index] = 0.0;
   } else {
      fprintf(stderr,"** apply_affected_voxels: bad datum %d\n", datum);
      RETURN(1);
   }

   RETURN(0);
}


/*--------------- count voxels affected by mask operation ---------------*/
int count_affected_voxels(void * data, int datum, byte * mask, int nvox,
                          int verb)
{
   int index, fill=0, rm=0;

   ENTRY("count_affected_voxels");

   if( datum == MRI_byte ) {
      byte * dptr = (byte *)data;
      for( index = 0; index < nvox; index++ )
         if     ( mask[index] && ! dptr[index] ) fill++;
         else if( ! mask[index] && dptr[index] ) rm++;
   } else if( datum == MRI_short ) {
      short * dptr = (short *)data;
      for( index = 0; index < nvox; index++ )
         if     ( mask[index] && ! dptr[index] ) fill++;
         else if( ! mask[index] && dptr[index] ) rm++;
   } else if( datum == MRI_float ) {
      float * dptr = (float *)data;
      for( index = 0; index < nvox; index++ )
         if     ( mask[index] && ! dptr[index] ) fill++;
         else if( ! mask[index] && dptr[index] ) rm++;
   } else {
      fprintf(stderr,"** count_affected_voxels: bad datum %d\n", datum);
      RETURN(1);
   }

   if(verb>1) INFO_message("AD: filled %d voxels, cleared %d", fill, rm);

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
 *    open (check dims, etc.)
 *    dilate (zeropad, make binary, dilate, unpad, apply)
 *    fill list of bytemask datasets
 *
 * also, count total volumes
 */
int process_input_dsets(param_t * params)
{
   THD_3dim_dataset * dset, * dfirst=NULL;
   int                iset, nxyz;

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
   
   /* warn user of dilations */
   if(params->verb && params->ndsets) {
      int pad = needed_padding(&params->IND);
      INFO_message("padding all datasets by %d (for dilations)", pad);
   }

   /* process the datasets */
   nxyz = 0;
   for( iset=0; iset < params->ndsets; iset++ ) {
      /* open and verify dataset */
      dset = THD_open_dataset(params->inputs[iset]);
      if( !dset ) ERROR_exit("failed to open mask dataset '%s'",
                             params->inputs[iset]);
      DSET_load(dset);  CHECK_LOAD_ERROR(dset);

      if( params->verb>1 ) INFO_message("loaded dset %s, with %d volumes",
                                        DSET_PREFIX(dset), DSET_NVALS(dset));

      if( nxyz == 0 ) { /* make an empty copy of the first dataset */
         nxyz = DSET_NVOX(dset);
         dfirst = EDIT_empty_copy(dset);
      }

      /* check for consistency in voxels and grid */
      if( DSET_NVOX(dset) != nxyz ) ERROR_exit("nvoxel mis-match");
      if( ! EQUIV_GRIDS(dset, dfirst) )
         WARNING_message("grid from dset %s does not match that of dset %s",
                         DSET_PREFIX(dset), DSET_PREFIX(dfirst));

      /* apply dilations to all volumes, returning bytemask datasets */
      params->dsets[iset] = apply_dilations(dset, &params->IND,1,params->verb);
      if( ! params->dsets[iset] ) RETURN(1);
   } 

   DSET_delete(dfirst); /* and nuke */

   RETURN(0);
}

/* compute max cumulative dilation to apply to dsets
 * (as the largest cumulative sum of dilations)
 */
int needed_padding(int_list * L)
{
   int index, sum, pad;
   ENTRY("needed_padding");
   for( index=0, sum=0, pad=0; index < L->num; index++ ) {
      sum += L->list[index];
      if( sum > pad ) pad = sum;
   }
   RETURN(pad);
}

/*--------------- count masks per voxel ---------------*/
/*
 * create empty count dataset
 * for each input dataset and each sub-volume
 *    for each voxel, if set: increment
 * close datasets as they are processed
 */
int count_masks(THD_3dim_dataset * dsets[], int ndsets, int verb, /* inputs */
                THD_3dim_dataset ** cset, int * nvol)             /* outputs */
{
   THD_3dim_dataset * dset;
   short * counts = NULL;             /* will become data for returned cset */
   byte  * bptr;                      /* always points to mask volumes      */
   int     nxyz, iset, ivol, ixyz;

   ENTRY("count_masks");

   if( !dsets || !cset || !nvol )
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
            ERROR_exit("in count_masks with non-byte data (set %d, vol %d)",
                       iset, ivol);

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

   /* create output dataset */
   *cset = EDIT_empty_copy(dsets[0]);
   EDIT_dset_items(*cset, ADN_nvals, 1,  ADN_ntt, 0, ADN_none);
   EDIT_substitute_brick(*cset, 0, MRI_short, counts);

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

   EDIT_dset_items(oset, ADN_prefix, params->prefix, ADN_none);

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
   "3dmask_tool         - for combining/dilating/eroding/filling masks\n"
   "\n"
   "This program can be used to:\n"
   "   1. combine masks, with a specified overlap fraction\n"
   "   2. dilate and/or erode a mask or combination of masks\n"
   "   3. fill holes in masks\n"
   "\n"
   "The outline of operations is as follows.\n"
   "\n"
   "   - read all input volumes\n"
   "      - optionally dilate/erode inputs (with any needed zero-padding)\n"
   "   - restrict voxels to the fraction of overlap\n"
   "   - optionally dilate/erode combination (with zero-padding)\n"
   "   - optionally fill any holes\n"
   "   - write result\n"
   "\n"
   "Note : a hole is defined as a fully connected set of zero voxels that\n"
   "       does not contain an edge voxel.  For any voxel in such a set, it\n"
   "       is not possible to find a path of voxels to reach an edge.\n"
   "\n"
   "       Such paths are evaluated using 6 face neighbors, no diagonals.\n"
   "\n"
   "----------------------------------------\n"
   "examples:\n"
   "\n"
   "   a. dilate a mask by 5 levels\n"
   "\n"
   "      3dmask_tool -input mask_anat.FT+tlrc -prefix ma.dilate \\\n"
   "                  -dilate_input 5\n"
   "\n"
   "   b. dilate and then erode, which connects areas that are close\n"
   "\n"
   "      3dmask_tool -input mask_anat.FT+tlrc -prefix ma.close.edges \\\n"
   "                  -dilate_input 5 -5\n"
   "\n"
   "   b2. dilate and erode after combining many masks\n"
   "\n"
   "      3dmask_tool -input mask_anat.*+tlrc.HEAD -prefix ma.close.result \\\n"
   "                  -dilate_result 5 -5\n"
   "\n"
   "   c1. compute an intersection mask, this time with EPI masks\n"
   "\n"
   "      3dmask_tool -input full_mask.*+tlrc.HEAD -prefix mask_inter \\\n"
   "                  -frac 1.0\n"
   "\n"
   "   c2. compute a mask of 70%% overlap\n"
   "\n"
   "      3dmask_tool -input full_mask.*+tlrc.HEAD -prefix mask_overlap.7 \\\n"
   "                  -frac 0.7\n"
   "   c3. simply count the voxels that overlap\n"
   "\n"
   "      3dmask_tool -input full_mask.*+tlrc.HEAD -prefix mask.counts \\\n"
   "                  -count\n"
   "\n"
   "   d. fill holes\n"
   "\n"
   "      3dmask_tool -input mask_anat.FT+tlrc -prefix ma.filled \\\n"
   "                  -fill_holes\n"
   "\n"
   "   e. fill holes per slice\n"
   "\n"
   "      3dmask_tool -input mask_anat.FT+tlrc -prefix ma.filled.xy \\\n"
   "                  -fill_holes -fill_dirs xy\n"
   "\n"
   "   f. read many masks, dilate and erode, restrict to 70%%, and fill holes\n"
   "\n"
   "      3dmask_tool -input mask_anat.*+tlrc.HEAD -prefix ma.fill.7 \\\n"
   "                  -dilate_input 5 -5 -frac 0.7 -fill_holes\n"
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
   "    -count                  : count the voxels that overlap\n"
   "\n"
   "        Instead of created a binary 0/1 mask dataset, create one with.\n"
   "        counts of voxel overlap, i.e each voxel will contain the number\n"
   "        of masks that it is set in.\n"
   "\n"
   "    -datum TYPE             : specify data type for output\n"
   "\n"
   "            e.g: -datum short\n"
   "            default: -datum byte\n"
   "\n"
   "        Valid TYPEs are 'byte', 'short' and 'float'.\n"
   "\n"
   "    -dilate_inputs D1 D2 ... : dilate inputs at the given levels\n"
   "\n"
   "            e.g. -dilate_inputs 3\n"
   "            e.g. -dilate_inputs -4\n"
   "            e.g. -dilate_inputs 8 -8\n"
   "            default: no dilation\n"
   "\n"
   "        Use this option to dilate and/or erode datasets as they are read.\n"
   "\n"
   "        Dilations are across the 18 voxel neighbors that share either a\n"
   "        face or an edge (i.e. of the 26 neighbors in a 3x3x3 box, it is\n"
   "        all but the outer 8 corners).\n"
   "        \n"
   "        An erosion is specified by a negative dilation.\n"
   "        \n"
   "        One can apply a list of dilations and erosions, though there\n"
   "        should be no reason to apply more than one of each.\n"
   "        \n"
   "        Note: use -dilate_result for dilations on the combined masks.\n"
   "\n"
   "    -dilate_result D1 D2 ... : dilate combined mask at the given levels\n"
   "\n"
   "            e.g. -dilate_result 3\n"
   "            e.g. -dilate_result -4\n"
   "            e.g. -dilate_result 8 -8\n"
   "            default: no dilation\n"
   "\n"
   "        Use this option to dilate and/or erode the result of combining\n"
   "        masks that exceed the -frac cutoff.\n"
   "\n"
   "        See -dilate_inputs for details of the operation.\n"
   "\n"
   "    -frac LIMIT             : specify required overlap threshold\n"
   "\n"
   "            e.g. -frac 0    (same as -union)\n"
   "            e.g. -frac 1.0  (same as -inter)\n"
   "            e.g. -frac 0.6\n"
   "            e.g. -frac 17\n"
   "            default: union (-frac 0)\n"
   "\n"
   "        When combining masks (across datasets and sub-bricks), use this\n"
   "        option to restrict the result to a certain fraction of the set of\n"
   "        volumes (or to a certain number of volumes if LIMIT > 1).\n"
   "\n"
   "        For example, assume there are 7 volumes across 3 datasets.  Then\n"
   "        at each voxel, count the number of masks it is in over the 7\n"
   "        volumes of input.\n"
   "\n"
   "            LIMIT = 0       : union, counts > 0 survive\n"
   "            LIMIT = 1.0     : intersection, counts = 7 survive\n"
   "            LIMIT = 0.6     : 60%% fraction, counts >= 5 survive\n"
   "            LIMIT = 5       : count limit, counts >= 5 survive  \n"
   "\n"
   "        See also -inter and -union.\n"
   "\n"
   "    -inter                  : intersection, this means -frac 1.0\n"
   "    -union                  : union, this means -frac 0\n"
   "\n"
   "    -fill_holes             : fill holes within the combined mask\n"
   "\n"
   "        This option can be used to fill holes in the resulting mask, i.e.\n"
   "        after all other processing has been done.\n"
   "\n"
   "        A hole is defined as a connected set of voxels that is surrounded\n"
   "        by non-zero voxels, and which contains no volume edge voxel, i.e.\n"
   "        there is no connected voxels at a volume edge (edge of a volume\n"
   "        meaning any part of any of the 6 volume faces).\n"
   "\n"
   "        To put it one more way, a zero voxel is part of a hole if there\n"
   "        is no path of zero voxels (in 3D space) to a volume face/edge.\n"
   "        Such a path can be curved.\n"
   "\n"
   "        Here, connections are via the 6 faces only, meaning a voxel could\n"
   "        be consider to be part of a hole even if there were a diagonal\n"
   "        path to an edge.  Please pester me if that is not desirable.\n"
   "\n"
   "    -fill_dirs DIRS         : fill holes only in the given directions\n"
   "\n"
   "            e.g. -fill_dirs xy\n"
   "            e.g. -fill_dirs RA\n"
   "            e.g. -fill_dirs XZ\n"
   "\n"
   "        This option is for use with -fill holes.\n"
   "\n"
   "        By default, a hole is a connected set of zero voxels that does\n"
   "        not have a path to a volume edge.  By specifying fill DIRS, the\n"
   "        filling is done restricted to only those axis directions.\n"
   "\n"
   "        For example, to fill holes once slice at a time (in a sagittal\n"
   "        dataset say, with orientation ASL), one could use any one of the\n"
   "        options:\n"
   "\n"
   "            -fill_dirs xy\n"
   "            -fill_dirs YX\n"
   "            -fill_dirs AS\n"
   "            -fill_dirs ip\n"
   "            -fill_dirs APSI\n"
   "\n"
   "        DIRS should be a single string that specifies 1-3 of the axes\n"
   "        using {x,y,z} labels (i.e. dataset axis order), or using the\n"
   "        labels in {R,L,A,P,I,S}.  Such labels are case-insensitive.\n"
   "\n"
   "    -inputs DSET1 ...       : specify the set of inputs (taken as masks)\n"
   "\n"
   "            e.g. -inputs group_mask.nii\n"
   "            e.g. -inputs full_mask.subj*+tlrc.HEAD\n"
   "            e.g. -inputs amygdala_subj*+tlrc.HEAD\n"
   "\n"
   "        Use this option to specify the input datasets to process.  Any\n"
   "        non-zero voxel will be consider part of that volume's mask.\n"
   "\n"
   "        An input dataset is allowed to have multiple sub-bricks.\n"
   "\n"
   "    -prefix PREFIX          : specify a prefix for the output dataset\n"
   "\n"
   "            e.g. -prefix intersect_mask\n"
   "            default: -prefix combined_mask\n"
   "\n"
   "        The resulting mask dataset will be named using the given prefix.\n"
   "\n"
   "    -quiet                  : limit text output to errors\n"
   "\n"
   "        Restrict text output.  This option is equivalent to '-verb 0'.\n"
   "\n"
   "        See also -verb.\n"
   "\n"
   "    -verb LEVEL             : specify verbosity level\n"
   "\n"
   "        The default level is 1, while 0 is considered 'quiet'.\n"
   "        The maximum level is currently 3, but most people don't care.\n"
   "\n"
   "-------------------------------\n"
   "R. Reynolds         April, 2012\n"
   "----------------------------------------------------------------------\n"
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
   params->prefix = "combined_mask";
   params->fill_dirs = NULL;
   init_int_list(&params->IND, 0);
   init_int_list(&params->RESD, 0);

   params->frac = -1.0;
   params->datum = MRI_byte;
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
      else if( strncmp(argv[ac],"-dilate_in", 10) == 0 ) {
         char * rptr; /* return pointer for strtol */
         int    ndilates = 0;

         if( ++ac >= argc ) ERROR_exit("need argument after '-dilate_inputs'");

         ival = strtol(argv[ac], &rptr, 10);
         while( ac < argc && rptr > argv[ac] ) {
            if( ! add_to_int_list(&params->IND, ival, 1) ) RETURN(-1);
            ndilates++;
            if( ++ac >= argc ) break;
            ival = strtol(argv[ac], &rptr, 10);
         }

         if( ndilates == 0 )
            ERROR_exit("no integral dilations found after -dilate_inputs");

         /* ac is already past last number */ continue;
      }

      /* read in a list of dilations (negatives are erosions) */
      else if( strncmp(argv[ac],"-dilate_result", 11) == 0 ) {
         char * rptr; /* return pointer for strtol */
         int    ndilates = 0;

         if( ++ac >= argc ) ERROR_exit("need argument after '-dilate_result'");

         ival = strtol(argv[ac], &rptr, 10);
         while( ac < argc && rptr > argv[ac] ) {
            if( ! add_to_int_list(&params->RESD, ival, 1) ) RETURN(-1);
            ndilates++;
            if( ++ac >= argc ) break;
            ival = strtol(argv[ac], &rptr, 10);
         }

         if( ndilates == 0 )
            ERROR_exit("no integral dilations found after -dilate_result");

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
         params->frac = 1.0;
         ac++; continue;
      }
      else if( strcmp(argv[ac],"-union") == 0 ) {
         params->frac = 0.0;
         ac++; continue;
      }

      else if( strcmp(argv[ac],"-fill_holes") == 0 ) {
         params->fill = 1;
         ac++; continue;
      }

      else if( strcmp(argv[ac],"-fill_dirs") == 0 ) {
         if( ++ac >= argc ) ERROR_exit("need argument after '-fill_dirs'");
         params->fill_dirs = argv[ac];
         ac++; continue;
      }

      else if( strncmp(argv[ac],"-inputs", 4) == 0 ) {
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

   if( !dilations_are_valid(& params->IND) ||
       !dilations_are_valid(& params->RESD) ) RETURN(-1);

   if( params->ndsets <= 0 ) ERROR_exit("missing -input dataset list");
   if( !params->prefix ) ERROR_exit("missing -prefix option");
   if( params->frac < 0.0 ) {
      if( params->verb ) INFO_message("no -frac option: defaulting to -union");
      params->frac = 0.0;
   }

   if( params->verb > 1 )
      INFO_message("%d datasets, frac = %g, %d IN dilation(s), %d OUT D(s)\n",
             params->ndsets, params->frac, params->IND.num, params->RESD.num);

   RETURN(0);
}

/*--------------- check for invalid dilations ---------------*/
int dilations_are_valid(int_list * D)
{
   int * ilist;
   int   ival, err = 0;

   ENTRY("dilations_are_valid");

   ilist = D->list;
   for( ival = 0; ival < D->num; ival++ )
      if( ilist[ival] == 0 ) {
         ERROR_message("dilation[%d] is zero", ival);
         err++;
      }
   for( ival = 1; ival < D->num; ival++ )
      if( ilist[ival-1]*ilist[ival] > 0){
         ERROR_message(
            "have sequential dilations of same sign (assuming mistake)\n"
            "   (d[%d] = %d, d[%d] = %d)\n",
            ival-1,ilist[ival-1],ival,ilist[ival]);
         err++;
      }
   if( err ) RETURN(0);

   RETURN(1);
}


/* convert something like "RA" to {1, 1, 0}, for first 2 axes
 * (depending on axis order of dset)
 *
 * If axes is not set, default to all.
 *
 * Allow for axes to be equivalent cases like "RA", "ra", "RLAP", "aprl".
 * Only allow 6 directions, else assume error.
 *
 * Also allow for cases like "xy", "YX".
 *
 * dirs 
 */
int set_axis_directions(THD_3dim_dataset * dset, char * axes, THD_ivec3 * dirs)
{
   int nax, odir, ind;

   ENTRY("set_axis_directions");

   if( ! dset || !dirs ) {
      fprintf(stderr,"** set_axis_directions: bad params %p, %p\n", dset,dirs);
      RETURN(1);
   }

   /* default to everything */
   if( !axes ) { dirs->ijk[0] = dirs->ijk[1] = dirs->ijk[2] = 1;  return 0; }

   /* init to nothing */
   dirs->ijk[0] = dirs->ijk[1] = dirs->ijk[2] = 0;

   /* note how many axes to set */
   nax = strlen(axes);
   if( nax > 6 ) {
      fprintf(stderr,"** set_axis_dirs: axis list len > 6 in '%s'\n",axes);
      RETURN(1);
   }

   for( ind = 0; ind < nax; ind++ ) {
      odir = THD_get_axis_direction(dset->daxes, ORCODE(toupper(axes[ind])));
      odir = abs(odir);

      /* allow for use of xyz or XYZ */
      if( odir == 0 ) {
         switch(toupper(axes[ind])) {
            case 'X': { odir = 1; break; }
            case 'Y': { odir = 2; break; }
            case 'Z': { odir = 3; break; }
         }
      }

      if( odir == 0 ) {
         fprintf(stderr,"** set_axis_dirs: illegal axis code in '%s'\n", axes);
         RETURN(1);
      }

      dirs->ijk[odir-1] = 1;
   }

   RETURN(0);
}


/*--------------- fill any holes in volumes ---------------*/
/*
 * A hole is defined as a connected set of zero voxels that does
 * not reach an edge.
 *
 * If axes is set, fill holes along the given axis directions.
 *
 * The core functionality was added to libmri.a in THD_mask_fill_holes.
 */
int fill_holes(THD_3dim_dataset * dset, char * axes, int verb)
{
   THD_ivec3   dirs;
   short     * sptr;     /* to for filling holes */
   byte      * bmask;    /* computed result */
   int         nfilled;
   int         nx, ny, nz, nvox, index, fill=0;

   ENTRY("fill_holes");

   bmask = THD_makemask(dset, 0, 1, 0); /* copy input as byte mask */
   nx = DSET_NX(dset);  ny = DSET_NY(dset);  nz = DSET_NZ(dset);
   nvox = DSET_NVOX(dset);

   if( set_axis_directions(dset, axes, &dirs) ) RETURN(1);

   /* created filled mask */
   nfilled = THD_mask_fill_holes(nx,ny,nz, bmask, &dirs, verb);
   if( nfilled < 0 ) { ERROR_message("failed to fill holes");  RETURN(1); }

   /* apply to short volume */
   sptr = DBLK_ARRAY(dset->dblk, 0);
   for( index = 0; index < nvox; index++ )
      if( !sptr[index] && bmask[index] ) { fill++;  sptr[index] = 1; }

   if(verb>2) INFO_message("final check: fill=%d, nfilled=%d", fill, nfilled);

   RETURN(0);
}
