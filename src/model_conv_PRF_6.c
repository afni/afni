/*****************************************************************************
   Based on model_conv_PRF.c, but this is with 6 parameters.

   The original (4 parameter) model is from:

        Population receptive field estimates in human visual cortex
        NeuroImage 39 (2008) 647-660
        Serge O. Dumoulin, Brian A. Wandell

   Given stimulus images over time s(x,y,t), find x0, y0, sigma, R and theta
   values that produce a best fit of the model to the data.  Here x0, y0 are
   taken to be the center of the population receptive field, sigma is the
   basic width of it, R is the ratio of axis lengths (sigma_x / sigma_y), and
   theta is the rotation from the x-direction major axis (so zero is in the
   positive x-direction).

   domains:
      x,y       : [-1,1], scaled by the mask, itself
      sigma     : (0,1], where 1 means the mask radius
      R         : [1,inf), since sigma defines the smaller size
      theta     : [-PI/2, PI/2), since rotation by PI has no effect

   The model function of x0, y0, sigma, R and theta is constructed as follows:

        1. generate a 2-D Gaussian density function, centered at x0, y0,
           with given sigma, R (=sigma_x/sigma_y), and theta:

           -> pRF model g(x,y) = e^-(A(x-x0)^2 + 2*B(x-x0)(y-y0) + C(y-y0)^2)

           where,
                       cos^2(theta) + R^2*sin^2(theta)
                  A =  -------------------------------
                             2 * R^2 * sigma^2
                        
                           (1-R^2) * sin(2theta)
                  B =      ---------------------
                             4 * R^2 * sigma^2
                        
                       sin^2(theta) + R^2*cos^2(theta)
                  C =  -------------------------------
                             2 * R^2 * sigma^2
                        

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

/* todo
 *   - all coordinates to be computed in mm mask-space
 *     (rather than as a fraction of the mask radius)
 *     - this might push for different g_exp_ts construction
 *     - this includes X, Y, Sigma being in mm
 *     - for Sigma, see genv_sigma_max, genv_sigma_nsteps,
 *                      genv_sigma_ratio_nsteps
 *   x env var to turn on/off g_exp_ts (see do_precompute_e2x)
 *   x should g_exp_maxval/ipieces be user-controllable?
 */
   
#include "NLfit_model.h"

static int     refnum    = 0;      /* # pts in refts */
static int     refnz     = 0;      /* # of nonzero pts */
static float * refts     = NULL;   /* reference time series */
static int   * refin     = NULL;   /* indexes of nonzero pts */
static int     g_iter    = -1;     /* iteration number */

static char  * g_model_ver = "model_conv_PRF_6, version 1.2, 20 Jun, 2018";

/* exp variables, parameters - from 8,1000 */
static float g_exp_maxval  = 1000.0;  /* max x in exp(-x) */
static int   g_exp_ipieces = 100; /* vals per unit length */

/* exp variables, implied */
static int     g_exp_nvals = 0;   /* maxval*ipieces + 1 */
static float * g_exp_ts  = NULL;  /* exp(-x) for x at index VAL*pieces */

static THD_3dim_dataset * g_saset=NULL; /* stimulus aperture dataset */

/* prototypes */
static void conv_set_ref( int num , float * ref );
static int signal_model( float * , int , float ** , float *, int );
static int reset_stim_aperture_dset(int);
static int reset_exp_time_series(void);

static THD_3dim_dataset * convert_to_blurred_masks(THD_3dim_dataset *);
static THD_3dim_dataset * THD_reorg_dset(THD_3dim_dataset * din);
static int convolve_dset(THD_3dim_dataset * tset);
static float * get_float_volume_copy(THD_3dim_dataset * dset, int index,int nz);
static int compute_e_x_grid(float * e, int nx, int ny, float x0, float y0,
                     float sigma, float sigrat, float theta);
static int fill_computed_farray(float * ts, int tslen, THD_3dim_dataset * dset,
                         float x0, float y0, float sigma, float sigrat,
                         float theta, float A, int debug);
static int fill_scaled_farray(float * fdest, int nt, THD_3dim_dataset * dsrc,
                       float x, float y, float sigma, float scale, int debug);
static int get_ABC(float sigma, float sigrat, float theta,
            double * A, double * B, double * C);
static int inputs_to_coords(THD_3dim_dataset * dset, float x, float y,
			    float sigma, float sigrat, float theta);

static int   disp_floats(char * mesg, float * p, int len);
static int   write_gauss_file(char * fname, float * curve,
                              int nx, int ny, char *hist);
static int   model_help(void);
static int   convolve_by_ref(float *, int, float *, int, int, int);

static void conv_model( float *  gs      , int     ts_length ,
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
static char * genv_conv_ref = NULL;    /* AFNI_CONVMODEL_REF */
static char * genv_prf_stim = NULL;    /* AFNI_MODEL_PRF_STIM_DSET */
static char * genv_gauss_file = NULL;  /* AFNI_MODEL_PRF_GAUSS_FILE */
static int    genv_diter    = -1;      /* debug iteration */
static int    genv_debug    = 0;       /* AFNI_MODEL_DEBUG */

static int    genv_on_grid      = 0;   /* restrict computations to grid */
static float  genv_sigma_max    = 1.0; /* on_grid: maximum blur sigma */
static int    genv_sigma_nsteps = 100; /* on_grid: number of blur steps */
static int    genv_sigma_ratio_nsteps = 4; /* integers >= 1, or any >= 1 if 0 */
static int    genv_theta_nsteps = 6;   /* truncate [-PI/2,PI/2) to S steps
					                           (if > 0) */
static int    genv_precompute_e2x = 0; /* do we use g_exp_ts? */

static int    genv_get_help = 0;       /* AFNI_MODEL_HELP_ALL/HELP_CONV_PRF_6 */

static int set_env_vars(void)
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

   /* on grid - default to no (yes->no 6 Jun 2013) */
   genv_on_grid  = AFNI_yesenv("AFNI_MODEL_PRF_ON_GRID"); /* flag */
   fprintf(stderr,"-- PRF: results on grid: %s\n", genv_on_grid?"yes":"no");

   genv_sigma_max = AFNI_numenv_def("AFNI_MODEL_PRF_SIGMA_MAX", genv_sigma_max);
   genv_sigma_nsteps = (int)AFNI_numenv_def("AFNI_MODEL_PRF_SIGMA_NSTEPS",
                                            genv_sigma_nsteps);
   genv_sigma_ratio_nsteps =
            (int)AFNI_numenv_def("AFNI_MODEL_PRF_SIGMA_RATIO_NSTEPS",
                                 genv_sigma_ratio_nsteps);
   genv_theta_nsteps = (int)AFNI_numenv_def("AFNI_MODEL_PRF_THETA_NSTEPS",
                                 genv_theta_nsteps);
   if( genv_on_grid )
      fprintf(stderr, "-- PRF: sigma_max = %f, nsteps = %d,"
                      " ratio_steps = %d, theta_steps = %d\n", 
              genv_sigma_max, genv_sigma_nsteps,
              genv_sigma_ratio_nsteps, genv_theta_nsteps);

   /* control contents of g_exp_ts (pre-computed e^x values) */
   g_exp_maxval = AFNI_numenv_def("AFNI_MODEL_PRF_MAX_EXP", g_exp_maxval);
   g_exp_ipieces = AFNI_numenv_def("AFNI_MODEL_PRF_MAX_EXP_PIECES",
                                   g_exp_ipieces);

   /* help */
   genv_get_help = AFNI_yesenv("AFNI_MODEL_HELP_CONV_PRF_6")
                || AFNI_yesenv("AFNI_MODEL_HELP_ALL");

   /* write a Gaussian mask? */
   genv_gauss_file = my_getenv("AFNI_MODEL_PRF_GAUSS_FILE");

   /* should we precompute e^x array? */
   genv_precompute_e2x = AFNI_yesenv("AFNI_MODEL_PRF_PRECOMPUTE_EX");


   if( genv_debug > 1 ) {
      fprintf(stderr,"++ params set by AFNI_MODEL_PRF_* env vars:\n");
      fprintf(stderr,"      %-35s : %s\n",
                     "AFNI_CONVMODEL_REF", genv_conv_ref);
      fprintf(stderr,"      %-35s : %s\n",
                     "AFNI_MODEL_PRF_STIM_DSET", genv_prf_stim);
      fprintf(stderr,"      %-35s : %d\n",
                     "AFNI_MODEL_DITER", genv_diter);
      fprintf(stderr,"      %-35s : %d\n",
                     "AFNI_MODEL_DEBUG", genv_debug);
      fprintf(stderr,"      %-35s : %d\n",
                     "AFNI_MODEL_PRF_ON_GRID", genv_on_grid);
      fprintf(stderr,"      %-35s : %g\n",
                     "AFNI_MODEL_PRF_SIGMA_MAX", genv_sigma_max);
      fprintf(stderr,"      %-35s : %d\n",
                     "AFNI_MODEL_PRF_SIGMA_NSTEPS", genv_sigma_nsteps);
      fprintf(stderr,"      %-35s : %d\n", "AFNI_MODEL_PRF_SIGMA_RATIO_NSTEPS",
                                         genv_sigma_ratio_nsteps);
      fprintf(stderr,"      %-35s : %d\n",
                     "AFNI_MODEL_PRF_THETA_NSTEPS", genv_theta_nsteps);
      fprintf(stderr,"      %-35s : %g\n",
                     "AFNI_MODEL_PRF_MAX_EXP", g_exp_maxval);
      fprintf(stderr,"      %-35s : %d\n",
                     "AFNI_MODEL_PRF_MAX_EXP_PIECES", g_exp_ipieces);
      fprintf(stderr,"      %-35s : %s\n",
                     "AFNI_MODEL_PRF_GAUSS_FILE", genv_gauss_file);
      fprintf(stderr,"      %-35s : %d\n",
                     "AFNI_MODEL_PRF_PRECOMPUTE_EX", genv_precompute_e2x);

   }

   return 0;
}
/* ---------------------------------------------------------------------- */
   

/*----------------------------------------------------------------------
   Function to set the reference time series, with which the
   model function is convolved to produce the simulated data.
------------------------------------------------------------------------*/

static void conv_set_ref( int num , float * ref )
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
static int reset_stim_aperture_dset(int needed_length)
{
   THD_3dim_dataset * sanew;
   int                errs=0;

   /* free and reload saset */
   if( g_saset ) DSET_delete(g_saset);
   g_saset = THD_open_dataset(genv_prf_stim);
   if( ! g_saset ) return 1;

   if( genv_debug > 1 ) fprintf(stderr,"=== reset_stim_aperature_dset ...\n");

   /* check for square dataset and sufficient nt */
   if( fabs(fabs(DSET_DX(g_saset)) - fabs(DSET_DY(g_saset))) > 0.0001 ) {
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

   if( genv_debug > 2 ) fprintf(stderr,"=== reset_SAD ... done\n");

   return 0;
}


/* x-axis is convolution time axis, so for each row, convolve */
static int convolve_dset(THD_3dim_dataset * tset)
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
static THD_3dim_dataset * convert_to_blurred_masks(THD_3dim_dataset * dset)
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
   if(genv_debug>1) {
     fprintf(stderr, "-- sigma_max = %f, nsteps = %d\n",
             genv_sigma_max, genv_sigma_nsteps);
     fprintf(stderr, "++ starting blur at time %6.1f\n",
             0.001*NI_clock_time());
   }

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
static THD_3dim_dataset * THD_reorg_dset(THD_3dim_dataset * din)
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
static float * get_float_volume_copy(THD_3dim_dataset * dset, int index, int nz)
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


/* want e^-x computed out to x=1000, as exponents are large
 * (since sigma is a fraction of the window "radius")
 * (or we should change coordinates to be in image space)
 *
 * g_exp_maxval = 1000 (from 8)
 * g_exp_ipieces = 100 (from 1000)
 * so compute values 0..1000, step 0.01
 * g_exp_nvals = g_exp_maxval * g_exp_ipieces + 1 (for 0)
 */
static int reset_exp_time_series(void)
{
   int   ind;
   float resol = 1.0/g_exp_ipieces;

   g_exp_nvals = (int)(g_exp_maxval * g_exp_ipieces) + 1;

   if(genv_debug)
      fprintf(stderr, 
              "-- exp (precomp = %d) nvals = %d, max = %f, pieces = %d\n",
              genv_precompute_e2x, g_exp_nvals, g_exp_maxval, g_exp_ipieces);

   if( g_exp_ts ) free(g_exp_ts);
   g_exp_ts = (float *)malloc(g_exp_nvals * sizeof(float));
   if( ! g_exp_ts ) {
      fprintf(stderr,"** failed to alloc %d floats, burning...\n", g_exp_nvals);

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

static void conv_model( float *  gs      , int     ts_length ,
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
      fprintf(stderr,"++ %s\n", g_model_ver);
   }

   /*** make sure there is a reference function to convolve with ***/
   /*   it may be used in reset_stim_aperture_dset */

   if( refnum <= 0 ) conv_set_ref( 0 , NULL ) ;

   if ( genv_on_grid ) {
      int pad = (g_iter % 76) - 38;
      if ( pad < 0 ) pad = -pad;
      fprintf(stderr,"%*s** not ready for AFNI_MODEL_PRF_ON_GRID **\n",
              pad,"");
      for ( ii=0; ii < ts_length; ii++ )
         ts_array[ii] = 0;
      return;
   }

   /* create stim aperture dset */
   if( g_iter == 0 ) {
      (void)reset_stim_aperture_dset(ts_length); /* free and reload saset */
      (void)reset_exp_time_series();     /* pre-compute exp(x) */
      if( genv_debug ) fprintf(stderr, "== start time %d\n", NI_clock_time());
   }

   /*** initialize the output before possible early return ***/

   for( ii=0 ; ii < ts_length ; ii++ ) ts_array[ii] = 0.0 ;

   /* if any inputs are invalid, return the zero array   14 Aug 2018 */
   /* sigma <= 0 is invalid, and check sigrat, just to be sure       */
   /* A == 0 implies a zero vector in any case ...                   */
   if( gs[0] == 0 || gs[3] <= 0 || gs[4] <= 0 ) {
      if( genv_debug > 1 ) disp_floats("** invalid input params: ", gs, 4);
      return;
   }

   /* if we had some failure, bail */
   if( !g_saset ) return;

   if( genv_debug > 1 ) {                       /* babble */
      if( genv_on_grid ) iter_step = 100000;
      else               iter_step = 1000;
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
static int convolve_by_ref(float * result, int rlen, float * signal, int siglen,
                    int init, int demean)
{
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

/*----------------------------------------------------------------------*
 * for use in signal model
 *----------------------------------------------------------------------*/
#ifdef PI
#undef PI
#endif
#define PI 3.141592653589793238462643


DEFINE_MODEL_PROTOTYPE

MODEL_interface * initialize_model ()
{
  MODEL_interface * mi = NULL;

  /*----- first, see if the user wants help -----*/
  if ( AFNI_yesenv("AFNI_MODEL_HELP_CONV_PRF_6") ||
       AFNI_yesenv("AFNI_MODEL_HELP_ALL") ) model_help();

  /*----- allocate memory space for model interface -----*/

  mi = (MODEL_interface *) RwcMalloc (sizeof(MODEL_interface));

  /*----- name of this model -----*/

  strcpy (mi->label, "Conv_PRF_6");

  /*----- this is a signal model -----*/

  mi->model_type = MODEL_SIGNAL_TYPE;

  /*----- number of parameters in the model -----*/

  mi->params = 6;

  /*----- parameter labels -----*/

  strcpy (mi->plabel[0], "Amp");
  strcpy (mi->plabel[1], "X");
  strcpy (mi->plabel[2], "Y");
  strcpy (mi->plabel[3], "Sigma");
  strcpy (mi->plabel[4], "SigRat");
  strcpy (mi->plabel[5], "Theta");

  /*----- minimum and maximum parameter constraints -----*/

  /* amplitude, x/y ranges, sigma range, sigrat and theta ranges */
  mi->min_constr[0] =    -10.0;   mi->max_constr[0] =    10.0;

  mi->min_constr[1] =    -1.0;    mi->max_constr[1] =     1.0;
  mi->min_constr[2] =    -1.0;    mi->max_constr[2] =     1.0;

  mi->min_constr[3] =     0.0;    mi->max_constr[3] =     1.0;
  mi->min_constr[4] =     1.0;    mi->max_constr[4] =    10.0;
  mi->min_constr[5] =   -PI/2.0;  mi->max_constr[5] =    PI/2.0;

  /*----- function which implements the model -----*/
  mi->call_func = (void_func *)conv_model;

  return (mi);
}


/*----------------------------------------------------------------------*/
/*
  Routine to calculate the time series to (hopefully) fit the data.

  Definition of model parameters (gs[2] > 0)

         gs[0] = Amp    = amplitude
         gs[1] = x0     = x-coordinate of gaussian center
         gs[2] = y0     = y-coordinate of gaussian center
         gs[3] = sigma  = "width" of gaussian curve
         gs[4] = sigrat = sigma ratio = sigma_x / sigma_y
         gs[5] = theta  = angle from "due east"

  For each TR, integrate g(x,y) over stim aperture dset.

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
  int    maxind, tmpmax;/* largest dimension */
  float  A, x, y, sigma;/* model params */
  float  sigrat, theta;

  /* assign parameters */
  A = gs[0];
  x = gs[1]; y = gs[2];
  sigma = gs[3]; sigrat = gs[4]; theta = gs[5];

  if( debug ) fprintf(stderr, "-d model_conv_PRF parameters: "
                              "A = %f, x = %f, y = %f, sigma = %f\n"
                              "   sigrat = %f, theta = %f\n"
                              "   nz = %d, nvals = %d, ts_len = %d\n",
                      A, x, y, sigma, sigrat, theta,
                      DSET_NZ(g_saset), DSET_NVALS(g_saset), ts_length);

  if( ! ISVALID_3DIM_DATASET(g_saset) ) return 0;

  maxind = ts_length;
  if( genv_on_grid ) tmpmax = DSET_NX(g_saset);
  else               tmpmax = DSET_NVALS(g_saset);

  if( maxind > tmpmax ) maxind = tmpmax;
  if( maxind == 0 ) return 0;

  if( debug ) 
      fprintf( stderr,"-d NT orig=%d, applied=%d\n", ts_length, maxind);

  /* time array must be ordered according to stim dset */
  if( genv_on_grid ) /* scale data directly from grid */
{
fprintf(stderr,"== rcr - need to apply sigrat, theta on grid\n");
     fill_scaled_farray(ts_array, maxind, g_saset, x, y, sigma, A, debug);
}
  else
     fill_computed_farray(ts_array, maxind, g_saset, x, y,
                          sigma, sigrat, theta, A, debug);

  if( debug )
     disp_floats("+d signal model result : ", ts_array, ts_length);

  return maxind;
}

/* get j, k, t from x, y, sigma, then copy data */
static int fill_scaled_farray(float *fdest, int length, THD_3dim_dataset *dsrc,
                       float x, float y, float sigma, float scale, int debug)
{
   float * inptr, * outptr;
   int     nx, ny, nz, nt;
   int     i, j, k, t;

   nx = DSET_NX(dsrc);  ny = DSET_NY(dsrc);  nz = DSET_NZ(dsrc);
   nt = DSET_NVALS(dsrc);

   if( nx != length ) {
      fprintf(stderr, "** FSF: nx, len mismatch, %d != %d\n", nx, length);
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
static int inputs_to_coords(THD_3dim_dataset * dset, float x, float y,
			    float sigma, float sigrat, float theta)
{
   int     nx, ny, nz;
   int     i, j, k;
   float   eval;
   double  A, B, C;

   nx = DSET_NX(dset);  ny = DSET_NY(dset);  nz = DSET_NZ(dset);

   /* for i,j, map [-1,1] to [0, nx-1] */
   /* note, (nx*(x+1.0)/2.0) is in [0,nx], with p(val=nx) very small,  */
   /*       so it should be enough to just limit floor(result) to nx-1 */
   i = (int)(nx*(x+1.0)/2.0);
   if     (i <  0 )   i = 0;
   else if(i >= nx )  i = nx-1;

   j = (int)(ny*(y+1.0)/2.0);
   if     (j <  0 )   j = 0;
   else if(j >= ny )  j = ny-1;

   /* init to round(nsteps * fraction of max) */
   k = (int)(genv_sigma_nsteps * sigma / genv_sigma_max);
   if     ( k <  0 )  k = 0;
   else if( k >= nz ) k = nz-1;

   get_ABC(sigma, sigrat, theta, &A, &B, &C);  /* get exp coefficients */
   eval = A*x*x + 2*B*x*y + C*y*y;

   fprintf(stderr,"-- fill_array from x=%f, y=%f, s=%f\n"
                  "   at i=%d, j=%d, k=%d\n",
           x, y, sigma, i, j, k);
   fprintf(stderr,"   sigrat=%f, theta=%f, exp=%.3f\n",
           sigrat, theta, eval);

   return 0;
}



/* compute the response value directly, given parameters:
 * - compute at a given index, under the assumption that
 *   such computations will be sequential (at least starting from 0)
 * - at index 0, a new e^x slice will be computed and
 *   applied across time */
static int fill_computed_farray(float * ts, int tslen, THD_3dim_dataset * dset,
                         float x0, float y0, float sigma, float sigrat,
                         float theta, float A, int debug)
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
      sexpgrid = (float *)calloc(snxy, sizeof(float));
      if( !sexpgrid ) {
         fprintf(stderr,"** PRF egrid alloc failure, nxy = %d\n", snxy);
         return 1;
      }
   }

   /* get current e^x grid (scaled to unit area) */
   if( compute_e_x_grid(sexpgrid, nx, ny, x0, y0, sigma, sigrat, theta) ) {
      fprintf(stderr,"PRF:FCA: e_x_g failure\n");
      return 1;
   }


   /* at each time point, take dot product of mask and gaussian grid */
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

   /* if requested, write this 2D gaussian image (one time only) */
   if( genv_gauss_file ) {
      char hist[256];
      sprintf(hist, "\n   == %s\n   x = %g, y = %g, "
                    "sigma = %g, sigrat = %g, theta = %g\n",
                    g_model_ver, x0, y0, sigma, sigrat, theta);
      fprintf(stderr, "++ writing PRF model curve to %s\n%s\n",
              genv_gauss_file, hist);
      write_gauss_file(genv_gauss_file, sexpgrid, nx, ny, hist);

      genv_gauss_file = NULL;  /* clear - no further writes */
   }

   return 0;
}

/* ------------------------------------------------------------ */
/* write_gauss_curve */
static int write_gauss_file(char * fname, float * curve, int nx, int ny,
                            char * hist)
{
   THD_3dim_dataset * dout;
   THD_ivec3          inxyz;
   THD_fvec3          origin, delta;
   float            * mptr, * dptr;
   byte             * bptr;
   int                ind;

   fprintf(stderr,"++ creating gauss dset (%dx%d)", nx, ny);
   dout = EDIT_empty_copy(NULL);
   LOAD_IVEC3(inxyz, nx, ny, 1);                               /* nxyz   */
   origin.xyz[0] = origin.xyz[1] = -1.0;                       /* origin */
   origin.xyz[2] = 0.0;
   delta.xyz[0] = delta.xyz[1] = 2.0/(nx-1);                   /* delta */
   delta.xyz[2] = 1.0;

   EDIT_dset_items(dout, ADN_nxyz,      inxyz,
                         ADN_xyzorg,    origin,
                         ADN_xyzdel,    delta,
                         ADN_prefix,    fname,
                         ADN_nvals,     2,
                         ADN_none);

   /* first is gaussian, second is first mask (to compare orientations) */
   /* use NULL to create space, then copy results */
   EDIT_substitute_brick(dout, 0, MRI_float, NULL);
   EDIT_substitute_brick(dout, 1, MRI_float, NULL);

   /* first copy 'curve' to slice 0 */
   mptr = DBLK_ARRAY(dout->dblk, 0);
   dptr = curve;
   for(ind = 0; ind < nx*ny; ind++)
      *mptr++ = *dptr++;

   /* now fill mask slice */
   mptr = DBLK_ARRAY(dout->dblk, 1);
   bptr = DBLK_ARRAY(g_saset->dblk, 0);
   for(ind = 0; ind < nx*ny; ind++, bptr++)
      *mptr++ = (float)*bptr;

   dout->daxes->xxorient = ORI_L2R_TYPE;
   dout->daxes->yyorient = ORI_P2A_TYPE;
   dout->daxes->zzorient = ORI_I2S_TYPE;

   if( hist ) tross_Append_History(dout, hist);

   fprintf(stderr,", writing");
   DSET_write(dout);

   DSET_delete(dout);
   fprintf(stderr,", done\n");

   return 0;
}


/* ------------------------------------------------------------ */
/* A = [cos^2(theta) + R^2*sin^2(theta)] / [2R^2sigma^2]
 * B = (1-R^2) * sin(2theta) / [4R^2sigma^2]
 * C = [sin^2(theta) + R^2*cos^2(theta)] / [2R^2sigma^2]
 */
static int get_ABC(float sigma, float sigrat, float theta,
            double * A, double * B, double * C)
{
   double   R2, R2S2, C2, S2, So2;

   R2   = sigrat*sigrat;
   R2S2 = 2*R2*sigma*sigma;
   C2   = cos(theta)*cos(theta);
   S2   = sin(theta)*sin(theta);
   So2  = sin(2*theta);

   *A = (C2 + R2 * S2)   / R2S2;
   *B = (1.0 - R2) * So2 / (2.0 * R2S2);
   *C = (S2 + R2 * C2)   / R2S2;

   return 0;
}


/* - compute an elliptic Gaussian curve over the current grid,
 *   centered at x0, y0, with the given sigma, sigrat and theta
 *
 * old: fill with e^-[((x-x0)^2 + (y-y0)^2) / (2*sigma^2)]
 * new: e^-[A(x-x0)^2 + 2*B(x-x0)(y-y0) + C(y-y0)^2], where
 *      A = [cos^2(theta) + R^2*sin^2(theta)] / [2R^2sigma^2]
 *      B = (1-R^2) * sin(2theta)             / [4R^2sigma^2]
 *      C = [sin^2(theta) + R^2*cos^2(theta)] / [2R^2sigma^2]
 *
 * We do not have to be too efficient in computing A,B,C, since those
 * are constant across the image.  Only x-x0 and y-y0 vary.
 *
 * Compute:
 *   R^2, 2R^2sigma^2,
 *   cos^2(theta), sin^2(theta), sin(2theta)
 */
static int compute_e_x_grid(float * e, int nx, int ny, float x0, float y0,
                     float sigma, float sigrat, float theta)
{
   float  * eptr, eval;
   double   wscale, sum;
   double   xoff, yoff, expval;
   double   A, B, C;
   int      ix, iy, eind;

   /* maybe we want to track exponent and evaluated exp limits */
   float    min_epow=1000, max_epow = 0; /* possibly do limit tracking */
   float    min_e2x=1e40, max_e2x = 0;
   int      track_limits = (g_iter == genv_diter || 
                           (g_iter == 0 && genv_debug > 1));


   wscale = 2.0/(nx-1.0);       /* scale [0,nx-1] to [0,2] */

   get_ABC(sigma, sigrat, theta, &A, &B, &C);  /* get exp coefficients */

   eptr = e;                    /* base pointer */
   sum = 0.0;                   /* for scaling to unit integral */
   for( iy = 0; iy < ny; iy++ ) {
      for( ix = 0; ix < nx; ix++ ) {
         xoff = ix*wscale - 1.0f - x0; /* map to [-1,1] and dist from x0 */
         yoff = iy*wscale - 1.0f - y0;

         /* compute (positive) power of e, will scale by g_exp_ipieces */
         eval = A*xoff*xoff + 2*B*xoff*yoff + C*yoff*yoff;

         /* do limit tracking (on exponents) */
         if( track_limits ) {
            if( eval < min_epow ) min_epow = eval;
            if( eval > max_epow ) max_epow = eval;
         }

         /* if large, do not bother computing e^-eval */
         if( eval > g_exp_maxval ) {
           *eptr++ = 0.0f;
           continue;
         }

         if( eval < 0.0f ) eval = 0.0f;

         /* either use precomputed results, or compute e^-x directly */
         if( genv_precompute_e2x ) {
            eind = (int)(eval*g_exp_ipieces);  /* truncate towards 0? */
            if ( eind < g_exp_nvals )
               expval = g_exp_ts[eind];
            else
               expval = 0.0f;
         } else
            expval = exp(-eval);

         sum += expval;
         *eptr++ = expval;

         /* more limit tracking (on e^-x) */
         if( track_limits ) {
            if( expval < min_e2x ) min_e2x = expval;
            if( expval > max_e2x ) max_e2x = expval;
         }
      }
   }

   /* and rescale by 1/sum */
   if( sum != 0.0 ) {
      sum = 1.0/sum;
      for( ix = 0, eptr = e; ix < nx*ny; ix++, eptr++ )
         if( *eptr ) *eptr *= sum;
   }

   /* report results of limit tracking */
   if ( track_limits ) {
      fprintf(stderr, "=== e2x limits: min_epow = %g, max_epow = %g\n",
              min_epow, max_epow);
      fprintf(stderr, "=== e2x limits: min_e2x = %g,  max_e2x = %g\n",
              min_e2x, max_e2x);
   }

   return 0;
}

/*----------------------------------------------------------------------*/
static int model_help(void)
{
   printf(
"----------------------------------------------------------------------\n"
"PRF_6  - 6 parameter population receptive field (in visual cortex)\n"
"\n"
"      Revisions from 19 Jun, 2018\n"
"\n"
"         - A factor of 2 was missing on the 'B' term.  The help text\n"
"           was accurate in terms of what the model did, but the equation\n"
"           should have read and now does read '... + 2*B()() + ...\n"
"\n"
"         - Sigma has been changed to be sigma_x/sigma_y, making theta=0\n"
"           wide along the x-axis.  Previously y/x had it along the y-axis.\n"
"\n"
"         - Rotations are now CCW.\n"
"\n"
"         - AFNI_MODEL_PRF_GAUSS_FILE can be used to specify a dataset\n"
"           to write a Gassian curve image to.\n"
"\n"
"      Given stimulus images over time s(x,y,t), find x0, y0, sigma, R and\n"
"      theta values that produce a best fit of the model to the data.  Here\n"
"      x0, y0 are taken to be the center of the population receptive field,\n"
"      sigma is the minor width of it (sigma_y, below), sigrat R is the ratio\n"
"      (sigma_x / sigma_y), and theta is the counter clockwise rotation from\n"
"      the x-direction major axis (so zero is in the positive x-direction).\n"
"\n"
"      We assume sigma_x >= sigma_y and refer to sigrat >= 1, since that\n"
"      sufficiently represents all possibilities.  The reciprocol would\n"
"      come from the negative complimentary angle, and would therefore be a\n"
"      redundant solution (if we allowed sigrat < 1).\n"
"\n"
"      So theta represents the CCW rotation of the major axis from x+.\n"
"\n"
"      parameter domains:\n"
"         x,y        : [-1,1] (x=-1 means left edge of mask, +1 means right)\n"
"                             (similarly for y)\n"
"         sigma      : (0,1], where 1 means the mask radius\n"
"         R (sigrat) : [1,inf), since sigma defines the smaller size\n"
"         theta      : [-PI/2, PI/2), since rotation by PI has no effect\n"
"\n"
"      The model function of x0, y0, sigma, R and theta is constructed as\n"
"      follows:\n"
"\n"
"         1. generate a 2-D elliptical Gaussian density function,\n"
"            centered at x0, y0, with given sigma, R (=sigma_x/sigma_y),\n"
"            and theta (CCW rotation of major direction from positive x):\n"
"\n"
"            -> pRF model g(x,y) = generalized 2-D Gaussian\n"
"\n"
"                e^-(A(x-x0)^2 + 2*B(x-x0)(y-y0) + C(y-y0)^2), where\n"
"\n"
"                     cos^2(theta)     sin^2(theta)\n"
"                 A = ------------  +  ------------\n"
"                      2sigma_x^2       2sigma_y^2\n"
"\n"
"                       sin(2theta)     sin(2theta)\n"
"                 B =   -----------  -  -----------     (signs mean CCW rot)\n"
"                       4sigma_x^2      4sigma_y^2\n"
"\n"
"                     sin^2(theta)     cox^2(theta)\n"
"                 C = ------------  +  ------------\n"
"                      2sigma_x^2       2sigma_y^2\n"
"\n"
"            Substituting sigma_x = R*sigma_y, sigma_y = sigma yields,\n"
"\n"
"                     cos^2(theta) + R^2*sin^2(theta)\n"
"                 A = -------------------------------\n"
"                               2*R^2sigma^2\n"
"\n"
"                               sin(2theta)\n"
"                 B = (1-R^2) * -----------\n"
"                               4*R^2sigma^2\n"
"\n"
"                     sin^2(theta) + R^2*cos^2(theta)\n"
"                 C = -------------------------------\n"
"                               2*R^2sigma^2\n"
"\n"
"--------------------------------------------------\n"
"To use this model function:\n"
"\n"
"   1. Generate the stimulus time series (currently, images must be square).\n"
"\n"
"      This should be a 2D+time dataset of visual stimuli over time.  They\n"
"      are viewed as binary masks by the model function.\n"
"\n"
"    * If results are computed on a restricted grid (which is much faster\n"
"      and is the default (see AFNI_MODEL_PRF_ON_GRID)), the resolution of\n"
"      those X,Y results will come directly from this stimulus dataset.\n"
"      It might be reasonable to have this be 100 or 200 (or 101 or 201)\n"
"      voxels on a side.\n"
"\n"
"\n"
"    **** Years have gone by, and I still have not implemented the speed-up\n"
"         for AFNI_MODEL_PRF_ON_GRID.  Please leave it set to NO for now.\n"
"\n"
"\n"
"    * The amount of memory used for the precomputation should be the size\n"
"      of this dataset (in float format) times AFNI_MODEL_PRF_SIGMA_NSTEPS.\n"
"      It is converted to floats because it is blurred internally.\n"
"      The default AFNI_MODEL_PRF_SIGMA_NSTEPS is 100.\n"
"\n"
"   2. Scale and demean the input EPI time series data.\n"
"\n"
"      Scaling is done to put the amplitude values into a reasonable (i.e.\n"
"      expected) range, such as by scaling it to a fraction of the mean\n"
"      (or maybe twice that).\n"
"\n"
"      Setting the mean to zero is done so that no baseline modeling is\n"
"      needed (though it might be good to model drifts in the future).\n"
"\n"
"   3. Generate a convolution reference time series, such as one for GAM.\n"
"      This should be on the same TR grid, which is 2 in this example.\n"
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
"              -signal Conv_PRF_6           \\\n"
"              -sconstr 0 -10.0 10.0        \\\n"
"              -sconstr 1 -1.0 1.0          \\\n"
"              -sconstr 2 -1.0 1.0          \\\n"
"              -sconstr 3 0.0 1.0           \\\n"
"              -sconstr 4 1.0 4.0           \\\n"
"              -sconstr 5 -1.571 1.570      \\\n"
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
"   required:\n"
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
"\n"
"         *** This has not been coded yet.  If there is strong interest,\n"
"             please let me know, though no promises are being made.\n"
"\n"
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
"      AFNI_MODEL_HELP_CONV_PRF_6  : Y/N - output this help\n"
"\n"
"         e.g. setenv AFNI_MODEL_HELP_CONV_PRF_6 YES\n"
"\n"
"         When set, the model initialization function will output this help.\n"
"\n"
"         Consider:\n"
"\n"
"            3dNLfim -signal Conv_PRF_6\n"
"\n"
"         or more directly (without setenv):\n"
"\n"
"            3dNLfim -DAFNI_MODEL_HELP_CONV_PRF_6=Y -signal Conv_PRF_6 \n"
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
"      AFNI_MODEL_PRF_GAUSS_FILE   : specify dataset prefix for Gauss curve\n"
"\n"
"         e.g. setenv AFNI_MODEL_PRF_GAUSS_FILE gauss_curve\n"
"\n"
"         Write a 2-D image with the Gaussian curve, which is helpful\n"
"         for checking the parameters.  This works best when used via\n"
"         the get_afni_model_PRF_6 program.\n"
"\n"
"----------------------------------------------------------------------\n"
"   Written for E Silson and C Baker.\n"
"\n"
"   R. Reynolds                                        27 June, 2014\n"
"----------------------------------------------------------------------\n"
   );

    return 0 ;
}

