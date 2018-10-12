/*****************************************************************************

   Common functions for the 4-parameter PRF models.

   These functions can be applied in the basic 4-param version or for the
   difference of Gaussians version, which has 6 parameters, but is just
   computed as the difference between 2 4-parameter expressions.

   So at this level, there is little difference between the 2 cases.

   ----------------------------------------------------------------------

   The model is from:

        Population receptive field estimates in human visual cortex
        NeuroImage 39 (2008) 647-660
        Serge O. Dumoulin, Brian A. Wandell

   Given stimulus images over time s(x,y,t), find x0, y0, sigma values that
   produce a best fit of the model to the data, where the model function of
   x0, y0 and sigma is constructed as follows:

        1. generate a 2-D Gaussian density function, centered at x0, y0,
           and with given sigma
           -> pRF model g(x,y) = e^-([(x-x0)^2+(y-y0)^2] / (2*sigma^2))
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


static THD_3dim_dataset * g_saset=NULL; /* stimulus aperture dataset */

/* prototypes */
static int reset_stim_aperture_dset(int);
static int reset_exp_time_series(void);
static int show_malloc_stats(char * mesg);

static int convert_to_blurred_masks(THD_3dim_dataset *, THD_3dim_dataset *);
static THD_3dim_dataset * THD_reorg_dset(THD_3dim_dataset*, THD_3dim_dataset*);
static THD_3dim_dataset * alloc_reorg_dset(THD_3dim_dataset * din, int dsetnz);
static int convolve_dset(THD_3dim_dataset * tset);
static float * get_float_volume_copy(THD_3dim_dataset *dset, int index, int nz);
static int compute_e_x_grid(float *e, int nx, int ny, float x0, float y0,
			    float sigma);
static int get_signal_computed(float * ts, int tslen, THD_3dim_dataset * dset,
                         float x0, float y0, float sigma, float A, int debug);
static int get_signal_from_grid(float * fdest, int nt, THD_3dim_dataset * dsrc,
                       float x, float y, float sigma, float scale, int debug);
static int inputs_to_coords(THD_3dim_dataset * dset, float x, float y,
			    float sigma);
static int insert_reorg_slice(THD_3dim_dataset * rset, float * slice,
                              int din_nxy, int din_zind, int din_tind);

static int   disp_floats(char * mesg, float * p, int len);
static int   write_gauss_file(char * fname, float * curve,
                              int nx, int ny, char * hist);
static int   write_dset(THD_3dim_dataset * dset, char * name);
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
static int    genv_debug    = 1;       /* AFNI_MODEL_DEBUG */
static int    genv_ram_stats= 0;       /* AFNI_MODEL_PRF_RAM_STATS */

static int    genv_on_grid      = 0;   /* restrict computations to grid */
static float  genv_sigma_max    = 1.0; /* on_grid: maximum blur sigma */
static int    genv_sigma_nsteps = 100; /* on_grid: number of blur steps */

static int    genv_get_help = 0;      /* AFNI_MODEL_HELP_ALL or HELP_CONV_PRF */

/* possble values for genv_ram_stats, to show malloc_stats() or ps */
#define AC_PRF_STAT_NONE        0
#define AC_PRF_STAT_MALLOC      1
#define AC_PRF_STAT_PS          2
#define AC_PRF_STAT_ALL         (AC_PRF_STAT_MALLOC|AC_PRF_STAT_PS) 
#define AC_PRF_STAT_WAIT        4

/* return one of the AC_PRF_STAT values */
static int set_env_ram_stats(void){
   char * estr = my_getenv("AFNI_MODEL_PRF_RAM_STATS");
   int    rval = AC_PRF_STAT_NONE;

   if( ! estr )                                       rval = AC_PRF_STAT_NONE;
   else if( AFNI_noenv("AFNI_MODEL_PRF_RAM_STATS") )  rval = AC_PRF_STAT_NONE;
   else if( AFNI_yesenv("AFNI_MODEL_PRF_RAM_STATS") ) rval = AC_PRF_STAT_ALL;
   else if( ! strcmp(estr, "MALLOC") )                rval = AC_PRF_STAT_MALLOC;
   else if( ! strcmp(estr, "PS") )                    rval = AC_PRF_STAT_PS;
   else if( ! strcmp(estr, "ALL") )                   rval = AC_PRF_STAT_ALL;
   else if( ! strcmp(estr, "WAIT") ) 
        rval = AC_PRF_STAT_WAIT | AC_PRF_STAT_ALL;
   else rval = (int)AFNI_numenv_def("AFNI_MODEL_PRF_RAM_STATS", 0);

   if( ! rval ) { genv_ram_stats = rval;  return 0; }

   if( genv_debug ) fprintf(stderr,"-- setting PRF_RAM_STATS to %d\n", rval);

   genv_ram_stats = rval;
   return 0;
}

static int set_env_vars(void)
{
   genv_conv_ref = my_getenv("AFNI_CONVMODEL_REF");       /* reference file */
   if( genv_conv_ref ) fprintf(stderr,"-- PRF: have REF %s\n", genv_conv_ref);
   else fprintf(stderr,"** model PRF: AFNI_CONVMODEL_REF is not set\n"); 

   genv_prf_stim = my_getenv("AFNI_MODEL_PRF_STIM_DSET"); /* visual stim */
   if( genv_prf_stim ) fprintf(stderr,"-- PRF: have stim %s\n", genv_prf_stim);
   else fprintf(stderr,"** model PRF: AFNI_MODEL_PRF_STIM_DSET is not set\n"); 

   genv_diter = (int)AFNI_numenv_def("AFNI_MODEL_DITER", -1);
   genv_debug = (int)AFNI_numenv_def("AFNI_MODEL_DEBUG", 1);
   if( genv_debug )
      fprintf(stderr,"-- PRF: debug %d, iter %d\n", genv_debug, genv_diter);

   /* on grid - default to yes */
   genv_on_grid  = 1-AFNI_noenv("AFNI_MODEL_PRF_ON_GRID"); /* flag */
   if( genv_debug )
      fprintf(stderr,"-- PRF: results on grid: %s\n", genv_on_grid?"yes":"no");

   genv_sigma_max = AFNI_numenv_def("AFNI_MODEL_PRF_SIGMA_MAX", genv_sigma_max);
   genv_sigma_nsteps = (int)AFNI_numenv_def("AFNI_MODEL_PRF_SIGMA_NSTEPS",
                                            genv_sigma_nsteps);
   if( genv_debug && genv_on_grid )
      fprintf(stderr,"-- PRF: sigma_max = %f, nsteps = %d\n",
                     genv_sigma_max, genv_sigma_nsteps);

   /* show RAM stats - default to no */
   set_env_ram_stats();

   /* help */
   genv_get_help = AFNI_yesenv("AFNI_MODEL_HELP_CONV_PRF")
                || AFNI_yesenv("AFNI_MODEL_HELP_ALL");

   /* write a Gaussian mask? */
   genv_gauss_file = my_getenv("AFNI_MODEL_PRF_GAUSS_FILE");
   if( genv_gauss_file && genv_debug )
      fprintf(stderr, "-- plan to write gauss file %s\n", genv_gauss_file);

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

/* allocate reorg dset early, to allow free() of all temp dsets
 * back to OS
 *
 * dsetnz : if set, get nz from dset, else get nz from genv_sigma_nsteps
 */
static THD_3dim_dataset * alloc_reorg_dset(THD_3dim_dataset * din, int dsetnz)
{
   THD_3dim_dataset * dout;
   float            * newvol;
   int                tind;

   THD_ivec3          iv_nxyz;
   int                in_nx, in_ny, in_nz, in_nt;
   int                out_nx, out_ny, out_nz, out_nt;

   dout = EDIT_empty_copy(din);

   in_nx = DSET_NX(din); in_ny = DSET_NY(din); in_nz = DSET_NZ(din);
   if( !dsetnz ) in_nz = genv_sigma_nsteps;
   in_nt = DSET_NVALS(din);

   out_nx = in_nt; out_ny = in_nx; out_nz = in_ny; out_nt = in_nz;

   if(genv_debug > 1)
      fprintf(stderr,"-- alloc_reorg: nxyzt (%d,%d,%d,%d) -> (%d,%d,%d,%d)\n",
              in_nx, in_ny, in_nz, in_nt, out_nx, out_ny, out_nz, out_nt);

   LOAD_IVEC3(iv_nxyz , out_nx, out_ny, out_nz);

   EDIT_dset_items(dout,
                   ADN_prefix, "alloc.reorg",
                   ADN_nxyz, iv_nxyz, ADN_nvals, out_nt, ADN_ntt, out_nt,
                   ADN_malloc_type, DATABLOCK_MEM_MALLOC,
                   ADN_none);

   /* create and attach each volume */
   for( tind = 0; tind < DSET_NVALS(dout); tind++ ) {
      newvol = (float *)malloc(DSET_NVOX(dout) * sizeof(float));
      mri_fix_data_pointer(newvol, DSET_BRICK(dout, tind));
   }

   return dout;
}


static int write_dset(THD_3dim_dataset * dset, char * name)
{
   EDIT_dset_items(dset, ADN_prefix, name, ADN_none);
   fprintf(stderr, "++ writing dset %s ...\n", name);
   DSET_write(dset);

   return 0;
}

/* for monitoring the RAM usage */
static int show_malloc_stats(char * mesg)
{
   int show_stats, show_ps, get_char;

   show_stats = genv_ram_stats & AC_PRF_STAT_MALLOC;
   show_ps    = genv_ram_stats & AC_PRF_STAT_PS;
   get_char   = genv_ram_stats & AC_PRF_STAT_WAIT;

   if( ! show_stats && ! show_ps ) return 0;

   if( show_stats ) {
      fprintf(stderr,"\n----- malloc stats: %s\n", mesg);
#ifndef DARWIN
      malloc_stats();
#endif
   }

   if( show_ps ) {
      fprintf(stderr,"\n----- ps info: %s\n", mesg);
      system("ps aux | grep NLfim | grep -v 'bin.time' | grep PRF");
   }

   if( get_char ) {
      fprintf(stderr,"    ... hit <enter> to proceed");
      getchar();
   }

   return 0;
}

/* any failure should leave g_saset == NULL
 *
 * return 0 on success */
static int reset_stim_aperture_dset(int needed_length)
{
   THD_3dim_dataset * sareorg;
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

   /* allocate reorg dset early */
   sareorg = alloc_reorg_dset(g_saset, 0);

   show_malloc_stats("pre blur");

   if( genv_on_grid ) {
      if( convert_to_blurred_masks(g_saset, sareorg) ) {
         fprintf(stderr,"** failed blur/reorg, nuking dset\n");
         DSET_delete(sareorg); sareorg = NULL;
      }

      show_malloc_stats("post blur");
      DSET_delete(g_saset);
      show_malloc_stats("post blur delete");

      g_saset = sareorg;
      // write_dset(g_saset, "sa.blur");

      convolve_dset(g_saset);
      // write_dset(g_saset, "sa.conv");

      if( ! g_saset ) return 1;
   }

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
 * given blur_max (e.g. 0.1)
 *       blur_nsteps (e.g. 50)
 * compute blur_res (e.g. 0.002)
 *
 *------------------------------------------------------------
 * allocate float slize (NX*NY), SLICE
 * for each input TR ("volume" is just a slice)
 *    get float copy of slice (mri_to_float()), COPY
 *    for each blur level (genv_sigma_nsteps)
 *       memcpy(SLICE, COPY, NX*NY*sizeof(float));
 *       EDIT_blur_volume(NX, NY, NZ=1, dx, dy, dz, MRI_float, data, SIGMA);
 *       copy slice into reorg dset
 *    mri_free(COPY)
 * free(SLICE)
 *------------------------------------------------------------
 */
static int convert_to_blurred_masks(THD_3dim_dataset * dset,
				    THD_3dim_dataset * rset)
{
   MRI_IMAGE * fim;
   float     * fslice, sigma;
   int         nx, ny, nt;
   int         vind, sind;
 
   /* test required inputs */
   if( !dset ) return 1;
   if( genv_sigma_max <= 0.0 || genv_sigma_max > 1.0 || genv_sigma_nsteps<=1 ){
      fprintf(stderr,"** PRF on grid: bad sigma max %f or nsteps %d\n",
              genv_sigma_max, genv_sigma_nsteps);
      return 1;
   }

   if( DSET_NZ(dset) != 1 ) {
      fprintf(stderr,"** invalid stim NZ = %d\n", DSET_NZ(dset));
      return 1;
   }
                              
   nx = DSET_NX(dset);  ny = DSET_NY(dset);  nt = DSET_NVALS(dset);

   /* report RAM and time info */
   if(genv_debug) { double mem;
      fprintf(stderr, "++ making blurred time series: %d x %d x %d  x nt=%d\n",
              DSET_NX(dset),DSET_NY(dset), genv_sigma_nsteps, DSET_NVALS(dset));
      fprintf(stderr, "++ stored as reorg dset: %d x %d x %d  x nt=%d\n",
              DSET_NX(rset),DSET_NY(rset), DSET_NZ(rset), DSET_NVALS(rset));
      mem = (double)DSET_NVOX(rset)*DSET_NVALS(rset)*sizeof(float) / (1<<30);
      fprintf(stderr,
              "   --> expected RAM for pre-computed results: %.2f GB\n\n", mem);
      if(genv_debug)fprintf(stderr, "++ starting blur at time %6.1f\n",
                              0.001*NI_clock_time());
   }

   fslice = (float *)malloc(nx * ny * sizeof(float));
   if( !fslice ){fputs("** CTBM N1\n", stderr); return 1;}
   if( genv_debug ) fprintf(stderr, "-- blur/reorg %d images:   0", nt);
   for( vind = 0; vind < nt; vind++ ) {
      if( genv_debug ) fprintf(stderr, "\b\b\b%3d", vind);

      /* get float copy of slice */
      fim = mri_to_float(DSET_BRICK(dset, vind));
      if(!fim){fputs("** CTBM N2\n", stderr); return 1;}

      for( sind = 0; sind < genv_sigma_nsteps; sind++ ) {
         /* start with original slice each time */
         memcpy(fslice, MRI_FLOAT_PTR(fim), nx*ny*sizeof(float));

         sigma = genv_sigma_max * ((sind + 1.0)/genv_sigma_nsteps);
         FIR_blur_volume_3d(nx, ny, 1,  2.0/(nx-1), 2.0/(ny-1), 1.0,
                            fslice, sigma, sigma, 0.0);
         insert_reorg_slice(rset, fslice, nx*ny, sind, vind);
      }

      mri_free(fim);  /* nuke copy of slice */
   }

   free(fslice);

   if(genv_debug)fprintf(stderr, "\n-- finished blur volume at time %6.1f\n",
                         0.001*NI_clock_time());

   return 0;
}

static int insert_reorg_slice(THD_3dim_dataset * rset, float * slice,
                              int din_nxy, int din_zind, int din_tind)
{
   float * outvol, * inptr;
   int     in_nx, in_ny, in_nz, in_nt;
   int     ix, iy;
   int     out_nx, out_ny, out_nxy, out_tind, out_xind;

   /* Could pass in accum volume, and add tind+1 each time.  The result
    * should be the sum of genv_sigma_nsteps at each voxel (i.e. 5050 when
    * genv_sigma_nsteps == 100).  Or just count by 1 to 100, too lazy?  */

   /* get original dimensions, convert output indices */
   in_nx = DSET_NY(rset); in_ny = DSET_NZ(rset); in_nz = DSET_NVALS(rset);
   in_nt = DSET_NX(rset);
   out_nx   = DSET_NX(rset);
   out_ny   = DSET_NY(rset);
   out_nxy  = out_nx * out_ny;
   out_tind = din_zind;
   out_xind = din_tind;

   if( in_nx * in_ny != din_nxy ) {
      fprintf(stderr,"** IRS: no nyz match, nx = %d, ny = %d, DINnxy = %d\n",
              in_nx, in_ny, din_nxy);
      return 1;
   }

   if( din_zind >= in_nz || din_tind >= in_nt ) {
      fprintf(stderr,"** IRS: no nzt match, nz=%d, zind=%d, nt=%d, tind=%d\n",
              in_nz, din_zind, in_nt, din_tind);
      return 1;
   }

   /* okay, copy the slice into new positions */
   outvol = (float *)DSET_ARRAY(rset, out_tind);
   inptr = slice;
   for( iy = 0; iy < in_ny; iy++ )
      for( ix = 0; ix < in_nx; ix++ )
         /* index conversion: in_x -> out_y, in_y -> out_z */
         outvol[out_xind + out_nx*ix + out_nxy * iy] = *inptr++;

   return 0;
}


/* rotate list of axes and correspondingly shuffle data:
 *      x,y,z,t -> t,x,y,z
 *
 * The main point is to make the time axes the fast direction.
 *
 * require MRI_float for now */
static THD_3dim_dataset * THD_reorg_dset(THD_3dim_dataset * din,
                                         THD_3dim_dataset * dorg)
{
   THD_3dim_dataset * dout;
   float            * newvol, * inslice, * outbase, * inbase;
   int                in_nx, in_ny, in_nz, in_nt;
   int                out_nx, out_ny, out_nz, out_nt;
   int                in_nxyz, in_nxy, out_nxyz, out_nxy;
   int                xind, yind, zind, tind;

   if( !din || !dorg ) { fprintf(stderr,"** reorg_dset:NULL\n"); return NULL; }

   dout = dorg;

   in_nx = DSET_NX(din); in_ny = DSET_NY(din); in_nz = DSET_NZ(din);
   in_nt = DSET_NVALS(din);
   in_nxy  = in_nx * in_ny;
   in_nxyz = in_nx * in_ny * in_nz;

   out_nx = in_nt; out_ny = in_nx; out_nz = in_ny; out_nt = in_nz;
   out_nxy  = out_nx * out_ny;
   out_nxyz = out_nx * out_ny * out_nz;

   if(genv_debug > 1)
      fprintf(stderr,"-- reorg_dset: nxyzt (%d,%d,%d,%d) -> (%d,%d,%d,%d)\n",
              in_nx, in_ny, in_nz, in_nt, out_nx, out_ny, out_nz, out_nt);
   if(genv_debug > 1)fprintf(stderr, "\n== reorg starting at %6.1f\n",
                             0.001*NI_clock_time());

   if( out_nx != DSET_NX(dout) || out_ny != DSET_NY(dout) || 
       out_nz != DSET_NZ(dout) || out_nt != DSET_NVALS(dout) ) {
      fprintf(stderr,"** reorg mis-match, crash and burn\n");
   }

   EDIT_dset_items(dout, ADN_prefix, "reorg.boots", ADN_none);

   /* create and attach each volume */
   for( tind = 0; tind < out_nt; tind++ ) {
      /* now newvol just points to existing memory */
      newvol = (float *)DSET_ARRAY(dout, tind);

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
   }

   if(genv_debug > 1)fprintf(stderr, "\n== reorg finished at %6.1f\n",
                             0.001*NI_clock_time());

   /* maybe dump a couple of time series */
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


/* OLD version: saved for comparison
 * (new version will input pre-allocated reorg dset)
 *
 * rotate list of axes and correspondingly shuffle data:
 *      x,y,z,t -> t,x,y,z
 *
 * The main point is to make the time axes the fast direction.
 *
 * require MRI_float for now */
static THD_3dim_dataset * THD_reorg_dset_old(THD_3dim_dataset * din)
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

   EDIT_dset_items(dout, ADN_prefix, "reorg.boots",
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

   /* maybe dump a couple of time series */
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


/* want e^-x computed out to x=7 (c^-7 goes below 0.001)
 *
 * g_exp_maxval = 7
 * g_exp_ipieces = 1000
 * so step = 1/1000
 * g_exp_nvals = g_exp_maxval * g_exp_ipieces + 1 (for 0), plus a few
 */
static int reset_exp_time_series(void)
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

   /* create stim aperture dset */
   if( g_iter == 0 ) {
      (void)reset_stim_aperture_dset(ts_length); /* free and reload saset */
      (void)reset_exp_time_series();     /* pre-compute exp(x) */
      if( genv_debug ) fprintf(stderr, "== start time %d\n", NI_clock_time());
   }

   /*** initialize the output before possible early return ***/

   for( ii=0 ; ii < ts_length ; ii++ ) ts_array[ii] = 0.0 ;

   /* if we had some failure, bail */
   if( !g_saset ) return;

   if( genv_debug ) {                       /* babble */
      if( genv_on_grid ) iter_step = 100000;
      else               iter_step = 100;
      if( (g_iter % iter_step) == 0 ) {
         if( genv_debug > 1 )
            fprintf(stderr, "-- time for %d iter set %5d : %6.1f\n",
                    iter_step, g_iter/iter_step, 0.001*NI_clock_time());
         else {
            static int first = 1;
            if( first ) {
               fprintf(stderr, "-- iteration size %d .", iter_step);
               first = 0;
            } else fputc('.', stderr);
         }
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


/* get j, k, t from x, y, sigma, then copy data */
static int get_signal_from_grid(float * fdest, int length,
                                THD_3dim_dataset *dsrc, float x, float y,
                                float sigma, float scale, int debug)
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


/* as in get_signal_from_grid, but map x,y,s to i,j,k */
static int inputs_to_coords(THD_3dim_dataset * dset, float x, float y,
			    float sigma)
{
   int     nx, ny, nz;
   int     i, j, k;

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
static int get_signal_computed(float * ts, int tslen, THD_3dim_dataset * dset,
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
      if( genv_debug > 1 )
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

   /* if requested, write this 2D image (one time only) */
   if( genv_gauss_file ) {
      char hist[256];
      sprintf(hist, "\n   == %s\n   x = %g, y = %g, sigma = %g\n",
                    g_model_ver, x0, y0, sigma);
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


/* - compute a Gaussian curve over the current grid, centered at x0, y0,
 *   and with the given sigma (unless x0,y0,sigma are the same as previous)
 *
 * fill with e^-[((x-x0)^2 + (y-y0)^2) / (2*sigma^2)]
 */
static int compute_e_x_grid(float * e, int nx, int ny, float x0, float y0,
			    float sigma)
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

