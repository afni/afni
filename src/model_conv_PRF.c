/*****************************************************************************
   Based on model_conv_cosine3.c.

   The model is from:

        Population receptive field estimates in human visual cortex
        NeuroImage 39 (2008) 647-660
        Serge O. Dumoulin, Brian A. Wandell

   Given stimulus images over time s(x,y,t), find x0, y0, sigma values that
   produce a minimum fit of the model to the data, where the model function
   of x0, y0 and sigma is construction as follows:

        1. generate a 2-D Gaussian density function, centered at x0, y0,
           and with given sigma
           -> pRF model g(x,y) = e^-([(x-x0)^2+(y-y0)^2] / 2*sigma^2)
        2. integrate (dot product) over binary stimulus image per time point
           -> pRF response r(t)
        3. convolve with HRF
           -> Prediction p(t)

   Then perform a simple regression fit, returning A (amplitude), the input
   x0, y0, sigma values, and the error (sum squares of the residuals)

   inputs

        s(x,y,t)        : 2d+time visual field bitmap timeseries
                          (e.g. rotating stimulus wedge)
        hrf             : HRF convolution curve - assume GAM for now
        data            : time series, per voxel

   outputs

        A               : amplitude of fit
        x, y, sigma     : best fit coordinates of center of Gaussian kernel

   R. Reynolds      June, 2014
******************************************************************************/
   
#include "NLfit_model.h"

static int     refnum    = 0;      /* # pts in refts */
static int     refnz     = 0;      /* # of nonzero pts */
static float * refts     = NULL;   /* reference time series */
static int   * refin     = NULL;   /* indexes of nonzero pts */
static int     g_iter    = -1;     /* iteration number */


/* exp variables, parameters */
static float g_exp_maxval  = 8.0;  /* max x in exp(-x) */
static int   g_exp_ipieces = 1000; /* vals per unit length */

/* exp variables, implied */
static int     g_exp_nvals = 0;   /* maxval*ipieces + 1 */
static float * g_exp_ts  = NULL;  /* exp(-x) for x at index VAL*pieces */


static THD_3dim_dataset * g_saset=NULL; /* stimulus aperature dataset */

/* prototypes */
static int signal_model( float * , int , float ** , float *, int );
int reset_stim_aperature_dset(int);
int reset_exp_time_series(void);

THD_3dim_dataset * convert_to_blurred_masks(THD_3dim_dataset *);
THD_3dim_dataset * THD_reorg_dset(THD_3dim_dataset * din);
int convolve_dset(THD_3dim_dataset * tset);
float * get_float_volume_copy(THD_3dim_dataset * dset, int index, int nz);
int compute_e_x_grid(float *e, int nx, int ny, float x0, float y0, float sigma);
int fill_computed_farray(float * ts, int tslen, THD_3dim_dataset * dset,
                         float x0, float y0, float sigma, float A, int debug);
int fill_scaled_farray(float * fdest, int nt, THD_3dim_dataset * dsrc,
                       float x, float y, float sigma, float scale, int debug);
int inputs_to_coords(THD_3dim_dataset * dset, float x, float y, float sigma);

static int   disp_floats(char * mesg, float * p, int len);
static int   model_help(void);
int          convolve_by_ref(float *, int, float *, int, int, int);

void conv_model( float *  gs      , int     ts_length ,
                 float ** x_array , float * ts_array   );

#define ERREX(str) ( fprintf(stderr,"\n*** %s\a\n",str) , exit(1) )

/* MATH uses exp(x) directly, while ARRAY uses pre-computed values */
#define ACCUM_EXP_MATH(val) do { if(val>=0) result += exp(-val); } while (0)
#define ACCUM_EXP_ARRAY(val) do {                                       \
            eind = (int)(val*g_exp_ipieces);  /* truncate towards 0? */ \
            if ( eind >= 0 && eind <= g_exp_nvals )                     \
                result += g_exp_ts[eind]; } while (0)

/* ---------------------------------------------------------------------- */
/* interface to the environment */
char * genv_conv_ref = NULL;    /* AFNI_CONVMODEL_REF */
char * genv_prf_stim = NULL;    /* AFNI_MODEL_PRF_STIM_DSET */
int    genv_diter    = -1;      /* debug iteration */
int    genv_debug    = 0;       /* AFNI_MODEL_DEBUG */

int    genv_on_grid      = 0;   /* restrict computations and results to grid */
float  genv_sigma_max    = 1.0; /* on_grid: maximum blur sigma */
int    genv_sigma_nsteps = 100; /* on_grid: number of blur steps */

int    genv_get_help = 0;       /* AFNI_MODEL_HELP_ALL or HELP_CONV_PRF */

int set_env_vars(void)
{
   genv_conv_ref = my_getenv("AFNI_CONVMODEL_REF");       /* reference file */
   if( genv_conv_ref ) fprintf(stderr,"-- PRF: have REF %s\n", genv_conv_ref);
   else fprintf(stderr,"** model PRF: AFNI_CONVMODEL_REF is not set\n"); 

   genv_prf_stim = my_getenv("AFNI_MODEL_PRF_STIM_DSET"); /* visual stim */
   if( genv_prf_stim ) fprintf(stderr,"-- PRF: have stim %s\n", genv_prf_stim);
   else fprintf(stderr,"** model PRF: AFNI_MODEL_PRF_STIM_DSET is not set\n"); 

   genv_diter = (int)AFNI_numenv_def("AFNI_MODEL_DITER", -1);
   genv_debug = (int)AFNI_numenv_def("AFNI_MODEL_DEBUG", 0);
   fprintf(stderr,"-- PRF: debug %d, iter %d\n", genv_debug, genv_diter);

   /* on grid - default to yes */
   genv_on_grid  = 1-AFNI_noenv("AFNI_MODEL_PRF_ON_GRID"); /* flag */
   fprintf(stderr,"-- PRF: results on grid: %s\n", genv_on_grid?"yes":"no");

   genv_sigma_max = AFNI_numenv_def("AFNI_MODEL_PRF_SIGMA_MAX", genv_sigma_max);
   genv_sigma_nsteps = (int)AFNI_numenv_def("AFNI_MODEL_PRF_SIGMA_NSTEPS",
                                            genv_sigma_nsteps);
   if( genv_on_grid ) fprintf(stderr,"-- PRF: sigma_max = %f, nsteps = %d\n",
                              genv_sigma_max, genv_sigma_nsteps);

   /* help */
   genv_get_help = AFNI_yesenv("AFNI_MODEL_HELP_CONV_PRF")
                || AFNI_yesenv("AFNI_MODEL_HELP_ALL");

   return 0;
}
/* ---------------------------------------------------------------------- */
   

/*----------------------------------------------------------------------
   Function to set the reference time series, with which the
   model function is convolved to produce the simulated data.
------------------------------------------------------------------------*/

void conv_set_ref( int num , float * ref )
{
   if( num > 0 && ref != NULL ){ /*** if have inputs, make space & copy in ***/
      int ii ;

      /* get rid of old data */

      if(refts != NULL){ free(refts); refts = NULL; free(refin); refin = NULL; }

      refnum = num ;
      refts  = (float *) malloc( sizeof(float) * num ) ;
      refin  = (int *)   malloc( sizeof(int)   * num ) ;
      memcpy( refts , ref , sizeof(float) * num ) ;
      for( ii=0,refnz=0 ; ii < num ; ii++ )        /* build list of nonzero */
         if( refts[ii] != 0.0 ) refin[refnz++] = ii ;    /* points in refts */
      if( refnz == 0 )
         ERREX("model_conv_PRF: All zero reference timeseries!") ;

      if( genv_debug ) {
         fprintf(stderr,"+d conv_set_ref: num=%d nonzero=%d\n",num,refnz) ;
         if( genv_debug > 1 ) {
            fprintf(stderr,"  TR locked stimuli :");
            for( ii = 0; ii < refnz; ii++ )
               fprintf(stderr," %f", refts[refin[ii]]);
            fputc('\n',stderr);
         }
      }

      return;

   } else { /*** if no inputs, read it from AFNI_CONVMODEL_REF 1D file ***/

     MRI_IMAGE * flim ;

     if( genv_conv_ref == NULL )
        ERREX("model_conv_PRF: need ref file as AFNI_CONVMODEL_REF") ;

     flim = mri_read_1D(genv_conv_ref);
     if( flim == NULL ){
        fprintf(stderr,"** model_conv_PRF: Can't read timeseries file %s",
                genv_conv_ref);
        ERREX("failing...");
     }

     if( genv_debug ) fprintf(stderr,"+d conv_set_ref: refts=%s  nx=%d\n",
                              genv_conv_ref, flim->ny);

     conv_set_ref( flim->nx , MRI_FLOAT_PTR(flim) ) ;  /* recursion! */
     mri_free(flim) ;
   }
   return;
}

/* any failure should leave g_saset == NULL
 *
 * return 0 on success */
int reset_stim_aperature_dset(int needed_length)
{
   THD_3dim_dataset * sanew;
   int                errs=0;

   /* free and reload saset */
   if( g_saset ) DSET_delete(g_saset);
   g_saset = THD_open_dataset(genv_prf_stim);
   if( ! g_saset ) return 1;

   /* check for square dataset and sufficient nt */
   if( fabs(DSET_DX(g_saset) - DSET_DY(g_saset)) > 0.0001 ) {
      fprintf(stderr,"** PRF: stimset voxels not square (%f, %f)\n",
              DSET_DX(g_saset), DSET_DY(g_saset));
      errs++;
   }

   if( DSET_NX(g_saset) != DSET_NY(g_saset) ) {
      fprintf(stderr,"** PRF: stimset not square (%d, %d)\n",
              DSET_NX(g_saset), DSET_NY(g_saset));
      errs++;
   }

   if( DSET_NVALS(g_saset) < needed_length ) {
      fprintf(stderr,"** PRF: dset nt = %d, tslen = %d\n",
              DSET_NVALS(g_saset), needed_length);
      errs++;
   }

   if( errs ) { DSET_delete(g_saset); g_saset = NULL; return 1; }


   if( THD_dset_to_mask(g_saset, 1.0, 0.0) ) return 1;

   if( genv_on_grid ) {
      sanew = convert_to_blurred_masks(g_saset);
      DSET_delete(g_saset);
      g_saset = sanew;
//EDIT_dset_items(sanew, ADN_prefix,    "sa.blur", ADN_none);
//fprintf(stderr, "++ writing blur dset...\n");
//DSET_write(sanew);

      sanew = THD_reorg_dset(g_saset);
      DSET_delete(g_saset);
      g_saset = sanew;
//EDIT_dset_items(sanew, ADN_prefix,    "sa.reorg", ADN_none);
//fprintf(stderr, "++ writing reorg dset...\n");
//DSET_write(sanew);

      convolve_dset(g_saset);
//EDIT_dset_items(sanew, ADN_prefix,    "sa.conv", ADN_none);
//fprintf(stderr, "++ writing convolve dset...\n");
//DSET_write(sanew);

      if( ! g_saset ) return 1;
   }

   return 0;
}


/* x-axis is convolution time axis, so for each row, convolve */
int convolve_dset(THD_3dim_dataset * tset)
{
   float * result, * volbase, * dbase;
   int     nx, ny, nz, nt, nxy;
   int     xind, yind, zind, tind;

   if( ! tset ) { fprintf(stderr, "** no tset to convolve\n"); return 1; }

   if(genv_debug>1)fprintf(stderr, "++ starting convolution at time %6.1f\n",
                           0.001*NI_clock_time());

   /* allocate room for computation and result */
   nx = DSET_NX(tset); ny = DSET_NY(tset); nz = DSET_NZ(tset);
   nt = DSET_NVALS(tset); nxy = nx * ny;
   result = (float *)malloc(nx * sizeof(float));
   if( !result ) {
      fprintf(stderr, "** PRF: conv_dset: failed malloc of %d floats\n", nx);
      return 1;
   }

   for( tind = 0; tind < nt; tind++ ) {
      volbase = (float *)DSET_ARRAY(tset, tind);

      /* for each row (the signal), convolve */
      for( zind = 0; zind < nz; zind++ ) {
         for( yind = 0; yind < ny; yind++ ) {
            /* result = convolved signal (initialize and demean) */
            dbase = volbase + yind*nx + zind*nxy;
            convolve_by_ref(result, nx, dbase, nx, 1, 1);
            /* and put result back in */
            for( xind = 0; xind < nx; xind++ ) dbase[xind] = result[xind];
         }
      }
   }

   if(genv_debug>1)fprintf(stderr, "-- finished convolution at time %6.1f\n",
                           0.001*NI_clock_time());

   return 0;
}


/* Convert 2D+time byte binary mask dataset into 3D+time float
 * dataset of slices (blur_nsteps) at different blur levels
 * (multiples of blur_res).
 *
 * input:  1 binary mask slice per TR
 * output: duplicate dset, but where each slice is converted to an array of
 *         slices at different blur levels
 *
 * Resample (reorganize) output so that time is the fastest dimension,
 * then x, y and sigma (sub-brick direction).
 *
 * Convert to float array for now, too, just to waste RAM.
 * At least that will make blurring more straightforward
 * and avoid truncation issues.
 *
 * given blur_max (e.g. 0.1)
 *       blur_nsteps (e.g. 50)
 * compute blur_res (e.g. 0.002)
 *
 *------------------------------------------------------------
 * for each TR
 *    convert mask slice to float
 *    allocate for volume of floats
 *    duplicate slice across volume slices
 *    for each blur level
 *       EDIT_blur_volume(NX, NY, NZ=1, dx, dy, dz, MRI_float, data, SIGMA);
 *    attach to dset
 * EDIT_dset_item: set datum, NZ
 *------------------------------------------------------------
 */
THD_3dim_dataset * convert_to_blurred_masks(THD_3dim_dataset * dset)
{
   THD_3dim_dataset * dnew;
   THD_ivec3          iv_nxyz;
   float            * fdata, * foffset, sigma;
   int                nx, ny, nt;
   int                vind, sind;
 
   /* test required inputs */
   if( !dset ) return NULL;
   if( genv_sigma_max <= 0.0 || genv_sigma_max > 1.0 || genv_sigma_nsteps<=1 ){
      fprintf(stderr,"** PRF on grid: bad sigma max %f or nsteps %d\n",
              genv_sigma_max, genv_sigma_nsteps);
      return NULL;
   }

   if( DSET_NZ(dset) != 1 ) {
      fprintf(stderr,"** invalid stim NZ = %d\n", DSET_NZ(dset));
      return NULL;
   }
                              
   /* create initial copy */
   dnew = EDIT_empty_copy(dset);
   if( !dnew ) return NULL;

   nx = DSET_NX(dnew);  ny = DSET_NY(dnew);  nt = DSET_NVALS(dnew);
   LOAD_IVEC3(iv_nxyz , nx, ny, genv_sigma_nsteps);

   EDIT_dset_items(dnew,
           ADN_prefix,    "I_like_jello",
           ADN_nxyz,      iv_nxyz,
           ADN_datum_all, MRI_float,
        ADN_none);

   if(genv_debug)
      fprintf(stderr, "++ making blurred time series: %d x %d x %d  x nt=%d\n",
              DSET_NX(dnew), DSET_NY(dnew), DSET_NZ(dnew), DSET_NVALS(dnew));
   if(genv_debug>1)fprintf(stderr, "++ starting blur at time %6.1f\n",
                           0.001*NI_clock_time());

   for( vind = 0; vind < nt; vind++ ) {
      if( genv_debug > 1 ) fputc('.', stderr);

      fdata = get_float_volume_copy(dset, vind, genv_sigma_nsteps);
      if( !fdata ) { DSET_delete(dnew); return NULL; }

      for( sind = 0; sind < genv_sigma_nsteps; sind++ ) {
         sigma = genv_sigma_max * ((sind + 1.0)/genv_sigma_nsteps);
         foffset = fdata + sind*nx*ny;
         FIR_blur_volume_3d(nx, ny, 1,  2.0/(nx-1), 2.0/(ny-1), 1.0,
                            foffset, sigma, sigma, 0.0);
      }

      mri_fix_data_pointer(fdata, DSET_BRICK(dnew, vind));
   }

   if(genv_debug>1)fprintf(stderr, "\n-- finished blur volume at time %6.1f\n",
                           0.001*NI_clock_time());

   return dnew;
}

/* rotate list of axes and correspondingly shuffle data:
 *      x,y,z,t -> t,x,y,z
 *
 * The main point is to make the time axes the fast direction.
 *
 * require MRI_float for now */
THD_3dim_dataset * THD_reorg_dset(THD_3dim_dataset * din)
{
   THD_3dim_dataset * dout;
   THD_ivec3          iv_nxyz;
   float            * newvol, * inslice, * outbase, * inbase;
   int                in_nx, in_ny, in_nz, in_nt;
   int                out_nx, out_ny, out_nz, out_nt;
   int                in_nxyz, in_nxy, out_nxyz, out_nxy;
   int                xind, yind, zind, tind;

   if( !din ) { fprintf(stderr,"** reorg_dset: missing input\n"); return NULL; }

   dout = EDIT_empty_copy(din);

   in_nx = DSET_NX(din); in_ny = DSET_NY(din); in_nz = DSET_NZ(din);
   in_nt = DSET_NVALS(din);
   in_nxy  = in_nx * in_ny;
   in_nxyz = in_nx * in_ny * in_nz;

   out_nx = in_nt; out_ny = in_nx; out_nz = in_ny; out_nt = in_nz;
   out_nxy  = out_nx * out_ny;
   out_nxyz = out_nx * out_ny * out_nz;

   LOAD_IVEC3(iv_nxyz , out_nx, out_ny, out_nz);

   EDIT_dset_items(dout,
                   ADN_nxyz, iv_nxyz, ADN_nvals, out_nt, ADN_ntt, out_nt,
                   ADN_none);

   if(genv_debug)
      fprintf(stderr,"-- reorg_dset: nxyzt (%d,%d,%d,%d) -> (%d,%d,%d,%d)\n",
              in_nx, in_ny, in_nz, in_nt, out_nx, out_ny, out_nz, out_nt);
   if(genv_debug>1)fprintf(stderr, "\n== reorg starting at %6.1f\n",
                           0.001*NI_clock_time());

   /* create and attach each volume */
   for( tind = 0; tind < out_nt; tind++ ) {
      newvol = (float *)malloc(out_nxyz * sizeof(float));
      if( !newvol ) {
         fprintf(stderr,"** PRF reorg: failed to alloc volume %d\n", tind);
         DSET_delete(dout);
         return NULL;
      }

      for( xind = 0; xind < out_nx; xind++ ) {

         /* copy one slice of input across y and z directions */
         inslice = ((float *)DSET_ARRAY(din, xind)) + tind*in_nxy;

         for( yind = 0; yind < out_ny; yind++ ) {
            outbase = newvol + xind + yind*out_nx;
            inbase  = inslice + yind;
            for( zind = 0; zind < out_nz; zind++ )
               outbase[zind*out_nxy] = inbase[zind*in_nx];
         }

      }

      mri_fix_data_pointer(newvol, DSET_BRICK(dout, tind));
   }

   if(genv_debug>1)fprintf(stderr, "\n== reorg finished at %6.1f\n",
                           0.001*NI_clock_time());

if( genv_debug > 2 ) {
  MRI_IMAGE * im;
  float     * fp;
  int i, j, k;

  i = in_nx/3; j = in_ny/3; k = in_nz/3;

  im = THD_extract_series(i+j*in_nx+k*in_nxy, din, 0);
  disp_floats("== ARY: sig [nxyz/3]: ", MRI_FLOAT_PTR(im), in_nt);
  fp =  ((float *)DSET_ARRAY(dout,k)) + i*out_nx + j*out_nxy;
  disp_floats("== ARY: reorg       : ", fp, out_nx);

  mri_free(im);
}

   return dout;
}


/* duplicate dset slice at given index nz times (into volume of nz slices) */
float * get_float_volume_copy(THD_3dim_dataset * dset, int index, int nz)
{
   MRI_IMAGE * fim;
   float     * fdata;
   int         nxy, sind;

   /* make space for new data */
   nxy = DSET_NXY(dset);
   fdata = (float *)malloc(nxy * nz * sizeof(float));
   if( ! fdata ) {
      fprintf(stderr,"** PRF: failed to alloc %d floats at index %d\n",
                     nxy * nz, index);
      return NULL;
   }

   /* get slice to duplicate */
   fim = mri_to_float( DSET_BRICK(dset, index) );
   if( ! fim ) {
      fprintf(stderr,"** PRF: failed mri_to_float\n");
      free(fdata);
      return NULL;
   }

   /* dupe over slices */
   for( sind = 0; sind < nz; sind++ )
      memcpy(fdata+sind*nxy, MRI_FLOAT_PTR(fim), nxy * sizeof(float));

   mri_free(fim);

   return fdata;
}


/* want e^-x computed out to x=7 (c^-7 goes below 0.001)
 *
 * g_exp_maxval = 7
 * g_exp_ipieces = 1000
 * so step = 1/1000
 * g_exp_nvals = g_exp_maxval * g_exp_ipieces + 1 (for 0), plus a few
 */
int reset_exp_time_series(void)
{
   int   ind;
   float resol = 1.0/g_exp_ipieces;

   g_exp_nvals = (int)(g_exp_maxval * g_exp_ipieces) + 1;

   if(genv_debug) fprintf(stderr, "-- exp nvals = %d, max = %f, pieces = %d\n",
                         g_exp_nvals, g_exp_maxval, g_exp_ipieces);

   if( g_exp_ts ) free(g_exp_ts);
   g_exp_ts = (float *)malloc(g_exp_nvals * sizeof(float));
   if( ! g_exp_ts ) {
      fprintf(stderr,"** failed to alloc %d floats, buring...\n", g_exp_nvals);

      if( g_saset ) DSET_delete(g_saset);
      g_saset = NULL; /* this blocks analysis */
      return 1;
   }

   for( ind = 0; ind < g_exp_nvals; ind++ )
      g_exp_ts[ind] = exp(-resol*ind);

   return 0;
}

/*-----------------------------------------------------------------------
  Function to compute the simulated time series.
-------------------------------------------------------------------------*/

void conv_model( float *  gs      , int     ts_length ,
                 float ** x_array , float * ts_array   )
{
   int ii, cur_debug = 0, irfdur=0;
   int iter_step;

   static int     nid = 0 ;      /* number of pts in impulse */
   static float * fid = NULL;    /* impulse response function */

   /*----- check for env vars -----*/

   g_iter++ ;

   if( g_iter == 0 ) {
      set_env_vars();   /* process environment variables */
      if(genv_debug && x_array) fprintf(stderr,"\n+d TR = %f\n",
                                        x_array[1][1]-x_array[0][1]);
   }

   /*** make sure there is a reference function to convolve with ***/
   /*   it may be used in reset_stim_aperature_dset */

   if( refnum <= 0 ) conv_set_ref( 0 , NULL ) ;

   /* create stim aperature dset */
   if( g_iter == 0 ) {
      (void)reset_stim_aperature_dset(ts_length); /* free and reload saset */
      (void)reset_exp_time_series();     /* pre-compute exp(x) */
      if( genv_debug ) fprintf(stderr, "== start time %d\n", NI_clock_time());
   }

   /*** initialize the output before possible early return ***/

   for( ii=0 ; ii < ts_length ; ii++ ) ts_array[ii] = 0.0 ;

   /* if we had some failure, bail */
   if( !g_saset ) return;

   if( genv_debug > 1 ) {                       /* babble */
      if( genv_on_grid ) iter_step = 100000;
      else               iter_step = 100;
      if( (g_iter % iter_step) == 0 ) {
         if( g_iter % (10*iter_step) ) fputc('\r', stderr);
         fprintf(stderr, "-- time for %d iter set %5d : %6.1f\n",
                 iter_step, g_iter/iter_step, 0.001*NI_clock_time());
      }
   }

   /* to clean up, particularly as a parameter */
   cur_debug = (g_iter == genv_diter || (g_iter == 0 && genv_debug > 1));
   if( cur_debug ) disp_floats("+d input params: ", gs, 4);

   /*** initialize the impulse response ***/

   if( nid < ts_length ){              /* make some space for it */
      if( fid ) free(fid) ;
      nid = ts_length ;
      fid = (float *) malloc( sizeof(float) * nid ) ;
   }

   /* If on grid, get signal directly.  Else, convolve signal impulse
      function with ref time series, and demean. */
   if( genv_on_grid ) {
      irfdur = signal_model(gs, ts_length, x_array, ts_array, cur_debug);
   } else {
      irfdur = signal_model(gs, ts_length, x_array, fid, cur_debug);
      convolve_by_ref(ts_array, ts_length, fid, irfdur, 0, 1);
      if( cur_debug ) disp_floats("+d no_grid conv    : ", ts_array, ts_length);
   }

   return ;
}

/*
   Convolve reference function (GAM here?) with stim time series.

   globals:
      int     refnum  : # pts in refts
      int     refnz   : # of nonzero pts
      float * refts   : reference time series
      int   * refin   : indexes of nonzero pts

   inputs:
      result          : convolved result (already initialized to 0)
      rlen            : length of input data time series (maybe equal)
      signal          : signal model output
      siglen          : length of signal time series
      init            : flag: init result?
      demean          : flag: demean result?
*/
int convolve_by_ref(float * result, int rlen, float * signal, int siglen,
                    int init, int demean)
{
   static int first = 1;
   int    ii, jj, kk, jtop;
   float  val;  /* current reference value to apply */
   double mean; /* for demeaning result */

   if( init ) {
      for( ii=0 ; ii < rlen ; ii++ ) result[ii] = 0.0;
   }

   /* TR-locked convolution */
   for( ii=0 ; ii < refnz ; ii++ ){
      kk  = refin[ii] ; if( kk >= rlen ) break ;
      val = refts[kk] ;

      /* for each point in the impulse, add its val times irf */
      /* (top index offset is min(irfdur, length-kk-1))    */
      jtop = rlen - kk ; if( jtop > siglen ) jtop = siglen ;
      for( jj=0 ; jj < jtop ; jj++ )
         result[kk+jj] += val * signal[jj];
   }

   if( ! demean ) return 0;

   /* demean the result */
   mean = 0.0;
   for( ii=0 ; ii < rlen ; ii++ ) mean += result[ii];
   mean /= rlen;
   for( ii=0 ; ii < rlen ; ii++ ) result[ii] -= mean;

   return 0;
}

static int disp_floats(char * mesg, float * p, int len)
{
   int c;
   if( mesg ) fputs(mesg, stderr);
   for( c = 0; c < len; c++ ) fprintf(stderr," %f ", p[c]);
   fprintf(stderr,"\n\n");
   return 0;
}


/*-----------------------------------------------------------------------*/

DEFINE_MODEL_PROTOTYPE

MODEL_interface * initialize_model ()
{
  MODEL_interface * mi = NULL;

  /*----- first, see if the user wants help -----*/
  if ( AFNI_yesenv("AFNI_MODEL_HELP_CONV_PRF") ||
       AFNI_yesenv("AFNI_MODEL_HELP_ALL") ) model_help();

  /*----- allocate memory space for model interface -----*/

  mi = (MODEL_interface *) XtMalloc (sizeof(MODEL_interface));

  /*----- name of this model -----*/

  strcpy (mi->label, "Conv_PRF");

  /*----- this is a signal model -----*/

  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/

  mi->params = 4;

  /*----- parameter labels -----*/

  strcpy (mi->plabel[0], "Amp");
  strcpy (mi->plabel[1], "X");
  strcpy (mi->plabel[2], "Y");
  strcpy (mi->plabel[3], "Sigma");

  /*----- minimum and maximum parameter constraints -----*/

  /* amplitude, x/y ranges, and sigma range */
  mi->min_constr[0] =    -10.0;   mi->max_constr[0] =    10.0;

  mi->min_constr[1] =    -1.0;    mi->max_constr[1] =     1.0;
  mi->min_constr[2] =    -1.0;    mi->max_constr[2] =     1.0;

  mi->min_constr[3] =     0.0;    mi->max_constr[3] =     1.0;

  /*----- function which implements the model -----*/
  mi->call_func = conv_model;

  return (mi);
}


/*----------------------------------------------------------------------*
 * for use in signal model
 *----------------------------------------------------------------------*/
#ifdef PI
#undef PI
#endif
#define PI 3.141592653589793238462643


/*----------------------------------------------------------------------*/
/*
  Routine to calculate the time series to (hopefully) fit the data.

  Definition of model parameters (gs[2] > 0)

         gs[0] = Amp    = amplitude
         gs[1] = x0     = x-coordinate of gaussian center
         gs[2] = y0     = y-coordinate of gaussian center
         gs[3] = sigma  = "width" of gaussian curve

  For each TR, integrate g(x,y) over stim aperature dset.

         g(x,y) = e^-[((x-x0)^2+(y-y0)^2)/(2*sigma^2)]

  The resulting returned time series will be convolved in the 
  parent function.
*/
static int signal_model
(
  float  * gs,          /* parameters for signal model */
  int      ts_length,   /* length of time series data */
  float ** x_array,     /* independent variable matrix */
  float  * ts_array,    /* estimated signal model time series */
  int      debug        /* make some noise */
)
{
  int    it;            /* time index */
  int    maxind;        /* largest dimension */
  double A, x, y, sigma;/* model params */

  /* assign parameters */
  A = gs[0];
  x = gs[1]; y = gs[2]; sigma = gs[3];

  if( debug ) fprintf(stderr, "-d model_conv_PRF parameters: "
                              "A = %f, x = %f, y = %f, sigma = %f\n"
                              "   nz = %d, nvals = %d, ts_len = %d\n",
                      A, x, y, sigma,
                      DSET_NZ(g_saset), DSET_NVALS(g_saset), ts_length);

  if( ! ISVALID_3DIM_DATASET(g_saset) ) return 0;

  maxind = ts_length;
  if( maxind > DSET_NX(g_saset) ) maxind = DSET_NX(g_saset);
  if( maxind == 0 ) return 0;

  /* time array must be ordered according to stim dset */
  if( genv_on_grid ) /* scale data directly from grid */
     fill_scaled_farray(ts_array, maxind, g_saset, x, y, sigma, A, debug);
  else
     fill_computed_farray(ts_array, maxind, g_saset, x, y, sigma, A, debug);

  if( debug )
     disp_floats("+d signal model result : ", ts_array, ts_length);

  return maxind;
}

/* get j, k, t from x, y, sigma, then copy data */
int fill_scaled_farray(float * fdest, int length, THD_3dim_dataset * dsrc,
                       float x, float y, float sigma, float scale, int debug)
{
   float * inptr, * outptr;
   int     nx, ny, nz, nt;
   int     i, j, k, t;

   nx = DSET_NX(dsrc);  ny = DSET_NY(dsrc);  nz = DSET_NZ(dsrc);
   nt = DSET_NVALS(dsrc);

   if( nx != length ) {
      fprintf(stderr, "** FSF: nx, len mis-match, %d != %d\n", nx, length);
      return 1;
   }

   /* for j,k, map [-1,1] to [0, ny-1] */
   j = (int)(0.5+ny*(x+1.0)/2.0); /* x-coord -> j index */
   if     (j <  0 )   j = 0;
   else if(j >= ny )  j = ny-1;

   k = (int)(0.5+nz*(y+1.0)/2.0); /* y-coord -> k index */
   if     (k <  0 )   k = 0;
   else if(k >= nz )  k = nz-1;

   /* init to round(nsteps * fraction of max) */
   t = (int)(0.5 + genv_sigma_nsteps * sigma / genv_sigma_max);
   if     ( t <  0 )  t = 0;
   else if( t >= nt ) t = nt-1;

   if( debug )
      fprintf(stderr,"-- fill_array from x=%f, y=%f, s=%f\n"
                     "   at j=%d, k=%d, t=%d\n",
              x, y, sigma, j, k, t);

   inptr = ((float *)DSET_ARRAY(dsrc, t)) + j*nx + k*nx*ny;
   outptr = fdest;
   for( i = 0; i < length; i++ ){
      *outptr = scale**inptr;
      inptr++;
      outptr++;
   }

   return 0;
}


/* as in fill_scaled_farray, but map x,y,s to i,j,k */
int inputs_to_coords(THD_3dim_dataset * dset, float x, float y, float sigma)
{
   int     nx, ny, nz;
   int     i, j, k;

   nx = DSET_NX(dset);  ny = DSET_NY(dset);  nz = DSET_NZ(dset);

   /* for i,j, map [-1,1] to [0, nx-1] */
   i = (int)(0.5+nx*(x+1.0)/2.0);
   if     (i <  0 )   i = 0;
   else if(i >= nx )  i = nx-1;

   j = (int)(0.5+ny*(y+1.0)/2.0);
   if     (j <  0 )   j = 0;
   else if(j >= ny )  j = ny-1;

   /* init to round(nsteps * fraction of max) */
   k = (int)(0.5 + genv_sigma_nsteps * sigma / genv_sigma_max);
   if     ( k <  0 )  k = 0;
   else if( k >= nz ) k = nz-1;

   fprintf(stderr,"-- fill_array from x=%f, y=%f, s=%f\n"
                  "   at i=%d, j=%d, k=%d\n",
           x, y, sigma, i, j, k);

   return 0;
}



/* compute the response value directly, given x,y,sigm:
 * - compute at a given index, under the assumption that
 *   such computations will be sequential (at least
 *   starting from 0
 * - at index 0, a new e^x slice will be computed and
 *   applied across time */
int fill_computed_farray(float * ts, int tslen, THD_3dim_dataset * dset,
                          float x0, float y0, float sigma, float A, int debug)
{
   static float * sexpgrid = NULL;
   static int     snxy=0;
   float        * eptr;
   byte         * mptr;
   double         sum;
   int            nx, ny, tind, sind, nmask;

   nx = DSET_NX(dset);  ny = DSET_NY(dset);

   /* maybe allocate new memory for egrid (hopefully once, "ever") */
   if( ! sexpgrid || snxy != nx*ny ) {
      if( genv_debug )
         fprintf(stderr,"++ alloc egrid, snxy = %d, nxy = %d\n", snxy, nx*ny);
      snxy = nx*ny;
      if( sexpgrid ) free(sexpgrid);    /* nuke any old copy - maybe never */
      sexpgrid = (float *)calloc(snxy, sizeof(float *));
      if( !sexpgrid ) {
         fprintf(stderr,"** PRF egrid alloc failure, nxy = %d\n", snxy);
         return 1;
      }
   }

   /* get current e^x grid (scaled to unit area) */
   if( compute_e_x_grid(sexpgrid, nx, ny, x0, y0, sigma) ) {
      fprintf(stderr,"PRF:FCA: e_x_g failure\n");
      return 1;
   }

   for( tind = 0; tind < tslen; tind++ ) {
      mptr = DBLK_ARRAY(dset->dblk, tind);
      eptr = sexpgrid;
      sum = 0.0;
      nmask = 0;
      for( sind = 0; sind < nx*ny; sind++ ) {
         if( *mptr++ ) {
            nmask++;
            sum += *eptr;
         }
         eptr++;
      }

      if( debug && genv_debug > 2 )
         fprintf(stderr,"-- nmask %03d = %d\n",tind,nmask);

      ts[tind] = A * sum;
   }

   return 0;
}

/* - compute a Gaussian curve over the current grid, centered at x0, y0,
 *   and with the given sigma (unless x0,y0,sigma are the same as previous)
 *
 * fill with e^-[((x-x0)^2 + (y-y0)^2) / (2*sigma^2)]
 */
int compute_e_x_grid(float * e, int nx, int ny, float x0, float y0, float sigma)
{
   float  * eptr;
   double   wscale, sig2, sum;
   float    xoff, yoff, maxdiff, eval, expval;
   int      ix, iy, eind;

   wscale = 2.0/(nx-1.0);       /* scale [0,nx-1] to [0,2] */
   sig2 = 0.5/sigma/sigma;      /* get 1/(2*sigma^2) */

   /* restrict |x-x0|^2 to 2*sigma^2*g_exp_maxval
    * so, diff <= sigma*sqrt(2*g_exp_maxval)
    */
   maxdiff = sigma * sqrt(2*g_exp_maxval);

   eptr = e;                    /* base pointer */
   sum = 0.0;                   /* for scaling to unit integral */
   for( iy = 0; iy < ny; iy++ ) {
      for( ix = 0; ix < nx; ix++ ) {
         xoff = ix*wscale - 1.0f - x0; /* map to [-1,1] and dist from x0 */
         yoff = iy*wscale - 1.0f - y0;

         /* if we are out of range, skip */
         if( fabs(xoff) > maxdiff || fabs(yoff) > maxdiff ) {
           *eptr++ = 0.0f;
           continue;
         }

         eval = (xoff*xoff+yoff*yoff)*sig2;
         if( eval < 0.0f ) eval = 0.0f; /* just to be sure */

         /* could do expval = exp(-eval); */
         eind = (int)(eval*g_exp_ipieces);  /* truncate towards 0? */
         if ( eind < g_exp_nvals ) {
            expval = g_exp_ts[eind];
            sum += expval;
         } else expval = 0.0f;
         *eptr++ = expval;
      }
   }

   /* and rescale by 1/sum */
   if( sum != 0.0 ) {
      sum = 1.0/sum;
      for( ix = 0, eptr = e; ix < nx*ny; ix++, eptr++ )
         if( *eptr ) *eptr *= sum;
   }

   return 0;
}

/*----------------------------------------------------------------------*/
static int model_help(void)
{
   printf(
"----------------------------------------------------------------------\n"
"PRF    - population receptive field (in visual cortex)\n"
"\n"
"   This model is from the paper:\n"
"\n"
"      Population receptive field estimates in human visual cortex\n"
"      NeuroImage 39 (2008) 647-660\n"
"      Serge O. Dumoulin, Brian A. Wandell\n"
"\n"
"   The model is made from parameters A, x0, y0, sigma, and from stimulus\n"
"   time series input (visual field masks over time) by:\n"
"\n"
"      1. compute a Gaussian curve centered at x0, y0 of with sigma\n"
"      2. multiply this 2-D image by each 2-D stimulus mask image\n"
"      3. convolve the result with an ideal HRF\n"
"      4. scale by the amplitude A\n"
"\n"
"   Currently, x0, y0, and sigma are limited to [-1,1], which the stimulus\n"
"   images are evaluated on.  This use may be altered in the future.\n"
"\n"
"--------------------------------------------------\n"
"To use this model function:\n"
"\n"
"   1. Generate the stimulus time series (currently, images must be square).\n"
"\n"
"   2. Scale and demean the input EPI time series data.  Scaling is done to\n"
"      put the amplitude values into a reasonable (expected) range.  Setting\n"
"      the mean to zero is done since no baseline modeling is needed (though\n"
"      it might be good to model drifts in the future).\n"
"\n"
"   3. Generate a convolution reference time series, such as one for GAM.\n"
"\n"
"      3dDeconvolve -nodata 10 2 -polort -1                \\\n"
"                   -num_stimts 1 -stim_times 1 '1D:0' GAM \\\n"
"                   -x1D conv.ref.GAM.1D\n"
"\n"
"   4. Set up environment variables to control execution:\n"
"\n"
"      setenv AFNI_CONVMODEL_REF conv.ref.GAM.1D\n"
"      setenv AFNI_MODEL_PRF_STIM_DSET stim.144.bmask.resam+tlrc\n"
"\n"
"   5. And execute:\n"
"\n"
"      3dNLfim -input epi.scale.demean+tlrc \\\n"
"              -noise Zero                  \\\n"
"              -signal Conv_PRF             \\\n"
"              -sconstr 0 -10.0 10.0        \\\n"
"              -sconstr 1 -1.0 1.0          \\\n"
"              -sconstr 2 -1.0 1.0          \\\n"
"              -sconstr 3 0.0 1.0           \\\n"
"              -BOTH                        \\\n"
"              -nrand 10000                 \\\n"
"              -nbest 5                     \\\n"
"              -bucket 0 buck.PRF           \\\n"
"              -snfit snfit.PRF\n"
"\n"
"--------------------------------------------------\n"
"environment variables:\n"
"\n"
"   -----------------------------------\n"
"   needed:\n"
"\n"
"      AFNI_CONVMODEL_REF          : specify convolution reference file\n"
"\n"
"         e.g. setenv AFNI_CONVMODEL_REF conv.ref.GAM.1D\n"
"\n"
"         The file this specifies should contain a (short?) impulse\n"
"         response function, such as made in step #3, above.\n"
"\n"
"      AFNI_MODEL_PRF_STIM_DSET    : specify visual stimulus dataset\n"
"\n"
"         e.g. setenv AFNI_MODEL_PRF_STIM_DSETstim.144.bmask.resam+tlrc\n"
"\n"
"         This should be a 2D+time dataset of stimulus images over TRs.\n"
"         It will be converted to a byte mask over the visual field.\n"
"\n"
"   -----------------------------------\n"
"   optional (for use with pre-computed grid):\n"
"\n"
"      AFNI_MODEL_PRF_ON_GRID      : Y/N - use pre-computed solutions\n"
"\n"
"         e.g. setenv AFNI_MODEL_PRF_ON_GRID NO\n"
"         e.g. default YES\n"
"\n"
"         Recommended.\n"
"\n"
"         When set, the model function will actually pre-compute all possible\n"
"         (unscaled) fit solutions on the first pass.  Since all of these\n"
"         parameters have a smooth effect on the result, this method should\n"
"         be sufficient.\n"
"\n"
"         Note that the resolution of x0, y0 parameters comes directly from\n"
"         the stimulus dataset (AFNI_MODEL_PRF_STIM_DSET), while the sigma\n"
"         resolution comes from the maximum (AFNI_MODEL_PRF_SIGMA_MAX) and\n"
"         the number of computed values (AFNI_MODEL_PRF_SIGMA_NSTEPS).\n"
"\n"
"         The more voxels to solve for in the input EPI, the more useful this\n"
"         is.  For a single voxel, it is slow.  For a large dataset, it can\n"
"         speed up the solution by a factor of 1000.\n"
"\n"
"      AFNI_MODEL_PRF_SIGMA_MAX    : specify maximum allowable sigma\n"
"\n"
"         e.g. setenv AFNI_MODEL_PRF_SIGMA_MAX 2.0\n"
"         e.g. default 1.0\n"
"\n"
"         Applies directly to AFNI_MODEL_PRF_ON_GRID.\n"
"\n"
"         Use this variable to set the maximum pre-computed sigma.\n"
"         This should probably match the sconstr value for sigma.\n"
"\n"
"      AFNI_MODEL_PRF_SIGMA_NSTEPS : specify number of pre-computed sigmas\n"
"\n"
"         e.g. setenv AFNI_MODEL_PRF_SIGMA_NSTEPS 50\n"
"         e.g. default 100\n"
"\n"
"         Applies directly to AFNI_MODEL_PRF_ON_GRID.\n"
"\n"
"         Use this variable to set the number of pre-computed sigma values.\n"
"         Note that the resolution of pre-computed sigma values will be the\n"
"         ratio: AFNI_MODEL_PRF_SIGMA_MAX/AFNI_MODEL_PRF_SIGMA_NSTEPS.\n"
"\n"
"   -----------------------------------\n"
"   helpful:\n"
"\n"
"      AFNI_MODEL_HELP_CONV_PRF    : Y/N - output this help\n"
"\n"
"         e.g. setenv AFNI_MODEL_HELP_CONV_PRF YES\n"
"\n"
"         When set, the model initialization function will output this help.\n"
"\n"
"         Consider:\n"
"\n"
"            3dNLfim -signal Conv_PRF\n"
"\n"
"         or more directly (without setenv):\n"
"\n"
"            3dNLfim -DAFNI_MODEL_HELP_CONV_PRF=Y -signal Conv_PRF\n"
"\n"
"      AFNI_MODEL_DEBUG            : specify debug/verbosity level\n"
"\n"
"         e.g. setenv AFNI_MODEL_DEBUG 2\n"
"\n"
"         Be more verbose.  Valid levels are from 0 to 3, currently.\n"
"\n"
"      AFNI_MODEL_DITER            : specify debug iteration\n"
"\n"
"         e.g. setenv AFNI_MODEL_DITER 999\n"
"\n"
"         Get extra debug info at some iteration.\n"
"\n"
"----------------------------------------------------------------------\n"
"   Written for E Silson and C Baker.\n"
"\n"
"   R. Reynolds                                        27 June, 2014\n"
"----------------------------------------------------------------------\n"
   );

    return 0 ;
}

