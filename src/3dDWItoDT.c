/*********************** 3dDWItoDT.c **********************************************/
/* Author: Daniel Glen, 17 Nov 2004 */
/* compute 6 principle direction tensors from multiple gradient vectors*/
/* and corresponding image data */
/* This version includes a more complex algorithm that takes into account*/
/* noise.*/

#include "thd_shear3d.h"
/*#ifndef FLOATIZE*/
# include "matrix.h"
# include "matrix.c"
/*#endif*/


#define TINYNUMBER 1E-10
#define SMALLNUMBER 1E-4
#define MAX_CONVERGE_STEPS 10           /* default maximum steps */
#define MAX_RWCONVERGE_STEPS 5

static char prefix[THD_MAX_PREFIX] = "DT";
static int datum = MRI_float;
static matrix Rtmat;
static double *Rvector;		/* residuals at each gradient */
static double *tempRvector;     /* copy of residuals at each gradient */
static matrix Fmatrix;
static matrix Dmatrix;
static matrix OldD;
static matrix Hplusmatrix, Hminusmatrix;
static vector Dvector;
static matrix tempFmatrix[2];
static matrix tempDmatrix[2];
static matrix tempHplusmatrix[2], tempHminusmatrix[2];
/* static vector tempDvector; */

static double eigs[12];
static double deltatau;
static double *wtfactor;	/* weight factors for time points at each voxel */
static double *bmatrix;		/* b matrix = GiGj for gradient intensities */
static double *cumulativewt;    /* overall wt. factor for each gradient */
static long rewtvoxels;         /* how many voxels were reweighted */ 
static double sigma;		/* std.deviation */
static double ED;		/* error for each iteration - cost function result */
static int automask = 0;        /* automasking flag - user option */
static int reweight_flag = 0;   /* reweight computation flag - user option */
static int method = -1;         /* linear or non-linear method - user option */
static int max_iter = -2;       /* maximum #convergence iteration steps - user option */
static int max_iter_rw = -2;    /* maximum #convergence iteration steps - user option */
static int eigs_flag = 0;       /* eigenvalue calculation in output - user option */
static int cumulative_flag = 0; /* calculate, display cumulative wts for gradients - user option */ 
static int debug_briks = 0;     /* put Ed, Ed0 and Converge step sub-briks in output - user option */
static int verbose = 0;         /* print out info every verbose number of voxels - user option */
static int afnitalk_flag = 0;   /* show convergence in AFNI graph - user option */

static void Form_R_Matrix (MRI_IMAGE * grad1Dptr);
static void DWItoDT_tsfunc (double tzero, double tdelta, int npts, float ts[], double ts_mean, double ts_slope, void *ud, int nbriks, float *val);
static void EIG_func (void);
static float Calc_FA(float *val);
static void ComputeD0 (void);
static double ComputeJ (float ts[], int npts);
static void ComputeDeltaTau (void);
static void Computebmatrix (MRI_IMAGE * grad1Dptr);
static void InitGlobals (int npts);
static void FreeGlobals (void);
static void Store_Computations (int i, int npts, int converge_step);
static void Restore_Computations (int i, int npts, int converge_step);
static void InitWtfactors (int npts);
static void ComputeWtfactors (int npts);
static void ComputeHpHm (double deltatau);
static void ComputeNewD (void);
static int TestConvergence (matrix NewD, matrix OldD);
static void udmatrix_to_vector (matrix m, vector * v);
static void udmatrix_copy (double *udptr, matrix * m);
static double matrix_sumabs (matrix m);
static double *InvertSym3 (double a, double b, double c, double e, double f,
			   double i);
static void matrix_copy (matrix a, matrix * b);

int
main (int argc, char *argv[])
{
  THD_3dim_dataset *old_dset, *new_dset;	/* input and output datasets */
  int nopt, nbriks, nvox;
  int i, eigs_brik;
  MRI_IMAGE *grad1Dptr = NULL;
  MRI_IMAGE *anat_im = NULL;
  short *sar = NULL, *tempsptr = NULL, tempval;
  byte *maskptr = NULL, *tempbptr = NULL;
  double *cumulativewtptr;

   /*----- Read command line -----*/
  if (argc < 2 || strcmp (argv[1], "-help") == 0)
    {
      printf ("Usage: 3dDWItoDT [options] gradient-file dataset\n"
	      "Computes 6 principle direction tensors from multiple gradient vectors\n"
	      " and corresponding DTI image volumes.\n"
	      " The program takes two parameters as input :  \n"
	      "    a 1D file of the gradient vectors with lines of ASCII floats Gxi,Gyi,Gzi.\n"
              "    Only the non-zero gradient vectors are included in this file (no G0 line).\n"
	      "    a 3D bucket dataset with Np+1 sub-briks where the first sub-brik is the\n"
              "    volume acquired with no diffusion weighting.\n"
	      " Options:\n"
	      "   -automask =  mask dataset so that the tensors are computed only for\n"
	      "    high-intensity (presumably brain) voxels.  The intensity level is\n"
              "    determined the same way that 3dClipLevel works.\n\n"
              "   -nonlinear = compute iterative solution to avoid negative eigenvalues.\n"
              "    This is the default method.\n\n"
              "   -linear = compute simple linear solution\n\n"
              "   -reweight = recompute weight factors at end of iterations and restart\n\n"
              "   -max_iter n = maximum number of iterations for convergence (Default=10)\n"
              "    values can range from -1 to any positive integer less than 101.\n"
	      "    A value of -1 is equivalent to the linear solution.\n"
	      "    A value of 0 results in only the initial estimate of the diffusion tensor\n"
	      "    solution adjusted to avoid negative eigenvalues.\n\n"
              "   -max_iter_rw n = max number of iterations after reweighting (Default=5)\n"
              "    values can range from 1 to any positive integer less than 101.\n\n"
              "   -eigs = compute eigenvalues, eigenvectors and fractional anisotropy in \n"
              "    sub-briks 6-18. Computed as in 3dDTeig\n\n"
              "   -debug_briks = add sub-briks with Ed (error functional), Ed0 (original error)\n"
              "    and number of steps to convergence\n\n"
              "   -cumulative_wts = show overall weight factors for each gradient level\n"
              "    May be useful as a quality control\n\n"
              "   -verbose nnnnn = print convergence steps every nnnnn voxels that survive to\n"
              "    convergence loops (can be quite lengthy)\n\n"
              " Example:\n"
              "  3dDWItoDTnoise -prefix rw01 -automask -reweight -max_iter 10 \\\n"
              "            -max_iter_rw 10 tensor25.1D grad02+orig.\n\n"
	      " The output is a 6 sub-brick bucket dataset containing Dxx,Dxy,Dxz,Dyy,Dyz,Dzz.\n"
	      " These results are appropriate as the input to the 3dDTeig program.\n"
	      "\n");
      printf ("\n" MASTER_SHORTHELP_STRING);
      exit (0);
    }

  mainENTRY ("3dDWItoDT main");
  machdep ();
  AFNI_logger ("3dDWItoDT", argc, argv);

  nopt = 1;
  nbriks = 6;		/* output contains 6 sub-briks by default */
  method = -1;
  reweight_flag = 0;

  datum = MRI_float;
  while (nopt < argc && argv[nopt][0] == '-')
    {

      /*-- prefix --*/

      if (strcmp (argv[nopt], "-prefix") == 0)
	{
	  if (++nopt >= argc)
	    {
	      fprintf (stderr, "*** -prefix needs an argument!\n");
	      exit (1);
	    }
	  MCW_strncpy (prefix, argv[nopt], THD_MAX_PREFIX);	/* change name from default prefix */
          /* check file name to be sure not to overwrite - mod drg 12/9/2004 */
	  if (!THD_filename_ok (prefix))
	    {
	      fprintf (stderr, "*** %s is not a valid prefix!\n", prefix);
	      exit (1);
	    }
	  nopt++;
	  continue;
	}

      /*-- datum --*/

      if (strcmp (argv[nopt], "-datum") == 0)
	{
	  if (++nopt >= argc)
	    {
	      fprintf (stderr, "*** -datum needs an argument!\n");
	      exit (1);
	    }
	  if (strcmp (argv[nopt], "short") == 0)
	    {
	      datum = MRI_short;
	    }
	  else if (strcmp (argv[nopt], "float") == 0)
	    {
	      datum = MRI_float;
	    }
	  else if (strcmp (argv[nopt], "byte") == 0)
	    {
	      datum = MRI_byte;
	    }
	  else
	    {
	      fprintf (stderr, "-datum of type '%s' is not supported!\n",
		       argv[nopt]);
	      exit (1);
	    }
	  nopt++;
	  continue;
	}
      if (strcmp (argv[nopt], "-automask") == 0)
	{
	  automask = 1;
	  nopt++;
	  continue;
	}

      if (strcmp (argv[nopt], "-linear") == 0)
        {
          if(method==1)
            {
              fprintf(stderr, "*** can not select both linear and non-linear methods at the same time\n");
              exit(1);
            }
          method = 0;
          nopt++;
	  continue;
        }

      if ((strcmp (argv[nopt], "-nonlinear") == 0) || (strcmp (argv[nopt], "-non-linear") == 0))
        {
          if(method==0)
            {
              fprintf(stderr, "*** can not select both linear and non-linear methods at the same time\n");
              exit(1);
            }
          method = 1;
          nopt++;
	  continue;
        }
      if (strcmp (argv[nopt], "-reweight") == 0)
        {
	  reweight_flag = 1;
          nopt++;
	  continue;
        }

      if (strcmp (argv[nopt], "-max_iter") == 0)
        {
	   if(++nopt >=argc ){
	      fprintf(stderr,"*** need an argument after -max_iter!\n");
	      exit(1);
	   }
           max_iter = strtol(argv[nopt], NULL, 10);
	   if ((max_iter <-1)||(max_iter>100)) {
	      fprintf(stderr, "*** max_iter must be between -1 and 100\n");
	      exit(1);
           }
          nopt++;
	  continue;
        }
	
     if (strcmp (argv[nopt], "-max_iter_rw") == 0)
        {
	   if(++nopt >=argc ){
	      fprintf(stderr,"*** need an argument after -max_iter_rw!\n");
	      exit(1);
	   }
           max_iter_rw = strtol(argv[nopt], NULL, 10);
	   if ((max_iter_rw <=0)||(max_iter_rw>100)) {
	      fprintf(stderr, "*** max_iter_rw must be between 1 and 100\n");
	      exit(1);
           }
          nopt++;
	  continue;
        }

     if (strcmp (argv[nopt], "-eigs") == 0)
        {
          eigs_flag = 1;
          nopt++;
	  continue;
        }

     if (strcmp (argv[nopt], "-cumulative_wts") == 0)
        {
          cumulative_flag = 1;
          nopt++;
	  continue;
        }

     if (strcmp (argv[nopt], "-debug_briks") == 0)
        {
          debug_briks = 1;
          nopt++;
	  continue;
        }

     if (strcmp (argv[nopt], "-verbose") == 0)
        {
	   if(++nopt >=argc ){
	      fprintf(stderr,"*** need an argument after -verbose!\n");
	      exit(1);
	   }
           verbose = strtol(argv[nopt], NULL, 10);
	   if (verbose<=0) {
	      fprintf(stderr, "*** verbose steps must be a positive number !\n");
	      exit(1);
           }
          nopt++;
	  continue;
        }


     /*static int afnitalk_flag = 0;*/   /* show convergence in AFNI graph - user option */


	fprintf(stderr, "*** unknown option %s\n", argv[nopt]);
	exit(1);
    }
  
  if(method==-1)
      method = 1;        /* if not selected, choose non-linear method for now */

  if(max_iter>=-1){
     if(method==0)
              fprintf(stderr, "*** max_iter will be ignored for linear methods.\n");
  }
  else
     max_iter = MAX_CONVERGE_STEPS;

  if(max_iter_rw>=0) {
     if(method==0)
              fprintf(stderr, "*** max_iter_rw will be ignored for linear methods.\n");
     if(reweight_flag==0)
              fprintf(stderr, "*** max_iter_rw will be ignored when not reweighting.\n");
  }
  else
     max_iter_rw = MAX_RWCONVERGE_STEPS;
     
  if((method==0)&&(reweight_flag==1)) {
      fprintf(stderr, "+++ Warning - can not reweight voxels for linear method\n");
      reweight_flag = 0;
  }

  if(cumulative_flag==1) {
     if(method==0) {
        fprintf(stderr, "+++ Warning - can not compute cumulative weights for linear method\n");
        cumulative_flag = 0;
     }
     if(reweight_flag == 0) {
        fprintf(stderr, "+++ Warning - can not compute cumulative weights if not reweighting\n");
        cumulative_flag = 0;
     }
  }

  if((method==0)&&(debug_briks==1)) {
      fprintf(stderr, "+++ Warning - can not compute debug sub-briks for linear method\n");
      debug_briks = 0;
  }

  if(eigs_flag)
     nbriks += 13;

  if(debug_briks)
     nbriks += 3;
     

   /*----- read input datasets -----*/

  if (nopt >= argc)
    {
      fprintf (stderr, "*** No input dataset!?\n");
      exit (1);
    }

  /* first input dataset - should be gradient vector file of ascii floats Gx,Gy,Gz */

  /* read gradient vector 1D file */
  grad1Dptr = mri_read_1D (argv[nopt]);
  if (grad1Dptr == NULL)
    {
      fprintf (stderr, "*** Error reading gradient vector file\n");
      exit (1);
    }

  if (grad1Dptr->ny != 3)
    {
      fprintf (stderr, "*** Only 3 columns of gradient vectors allowed\n");
      fprintf (stderr, "%d columns found\n", grad1Dptr->nx);
      mri_free (grad1Dptr);
      exit (1);
    }

  if (grad1Dptr->nx < 6)
    {
      fprintf (stderr, "*** Must have at least 6 gradient vectors\n");
      fprintf (stderr, "%d columns found\n", grad1Dptr->nx);
      mri_free (grad1Dptr);
      exit (1);
    }


  Form_R_Matrix (grad1Dptr);	/* use grad1Dptr to compute R matrix */

  nopt++;

  /* Now read in all the MRI volumes for each gradient vector */
  /* assumes first one is no gradient */
  old_dset = THD_open_dataset (argv[nopt]);

  if (!ISVALID_DSET (old_dset))
    {
      fprintf (stderr, "*** Can't open dataset %s\n", argv[nopt]);
      mri_free (grad1Dptr);
      exit (1);
    }

  /* expect at least 7 values per voxel - 7 sub-briks as input dataset */
  if (DSET_NVALS (old_dset) != (grad1Dptr->nx + 1))
    {
      fprintf (stderr,
	       "*** Dataset must have number of sub-briks equal to one more than number\n");
      fprintf (stderr, "  of gradient vectors (B0+Bi)!\n");
      mri_free (grad1Dptr);
      exit (1);
    }


  InitGlobals (grad1Dptr->nx + 1);	/* initialize all the matrices and vectors */
  Computebmatrix (grad1Dptr);	/* compute bij=GiGj */

  if (automask)
    {
      DSET_mallocize (old_dset);
      DSET_load (old_dset);	/* get B0 (anatomical image) from dataset */
      /*anat_im = THD_extract_float_brick( 0, old_dset ); */
      anat_im = DSET_BRICK (old_dset, 0);	/* set pointer to the 0th sub-brik of the dataset */
      maskptr = mri_automask_image (anat_im);	/* maskptr is a byte pointer for volume */

      /* convert byte mask to same format type as dataset */
      nvox = DSET_NVOX (old_dset);
      sar = (short *) calloc (nvox, sizeof (short));
      /* copy maskptr values to far ptr */
      tempsptr = sar;
      tempbptr = maskptr;
      for (i = 0; i < nvox; i++)
	{
	  *tempsptr++ = (short) *tempbptr++;
	  tempval = *(tempsptr - 1);
	}

      free (maskptr);

      /*old_dset->dblk->malloc_type = DATABLOCK_MEM_MALLOC; *//* had to set this? */
      EDIT_add_brick (old_dset, MRI_short, 0.0, sar);	/* add sub-brik to end */


    }

  /* temporarily set artificial timing to 1 second interval */
  EDIT_dset_items (old_dset,
		   ADN_ntt, DSET_NVALS (old_dset),
		   ADN_ttorg, 0.0,
		   ADN_ttdel, 1.0, ADN_tunits, UNITS_SEC_TYPE, NULL);

#ifdef TESTING4
  system("plugout_drive 'OPEN_GRAPH_1D DWIEDgraph 'DWI Ed' 25 1 'Convergence' 1 0 200000 'Ed' ''");
  system("plugout_drive 'OPEN_GRAPH_1D DWIDTgraph 'DWI \\delta \\tau' 25 1 'Convergence' 1 0 200000 '\\delta \\tau' ''");
#endif


   /*------------- ready to compute new dataset -----------*/

  new_dset = MAKER_4D_to_typed_fbuc (old_dset,	/* input dataset */
				     prefix,	/* output prefix */
				     datum,	/* output datum  */
				     0,	/* ignore count  */
				     0,	/* can't detrend in maker function  KRH 12/02 */
				     nbriks,	/* number of briks */
				     DWItoDT_tsfunc,	/* timeseries processor */
				     NULL	/* data for tsfunc */
    );


  if(cumulative_flag && reweight_flag) {
    cumulativewtptr = cumulativewt;
    printf("Cumulative Wt. factors: ");
    for(i=0;i<(grad1Dptr->nx + 1);i++){
       *cumulativewtptr = *cumulativewtptr / rewtvoxels;
       printf("%5.3f ", *cumulativewtptr++);
    }
     printf("\n");
  }

  FreeGlobals ();
  mri_free (grad1Dptr);
  matrix_destroy (&Rtmat);	/* clean up */

  if (automask)
    {
      mri_free (anat_im);
      DSET_unload_one (old_dset, 0);
      sar = NULL;
    }

  if (new_dset != NULL)
    {
      tross_Copy_History (old_dset, new_dset);
      EDIT_dset_items (new_dset, ADN_brick_label_one + 0, "Dxx", ADN_none);
      EDIT_dset_items (new_dset, ADN_brick_label_one + 1, "Dxy", ADN_none);
      EDIT_dset_items (new_dset, ADN_brick_label_one + 2, "Dxz", ADN_none);
      EDIT_dset_items (new_dset, ADN_brick_label_one + 3, "Dyy", ADN_none);
      EDIT_dset_items (new_dset, ADN_brick_label_one + 4, "Dyz", ADN_none);
      EDIT_dset_items (new_dset, ADN_brick_label_one + 5, "Dzz", ADN_none);
      if(eigs_flag) {
        eigs_brik = ADN_brick_label_one + 6;   /* 1st eigenvalue brik */
        EDIT_dset_items(new_dset, eigs_brik+0, "lambda_1", ADN_none);
        EDIT_dset_items(new_dset, eigs_brik+1, "lambda_2",ADN_none);
        EDIT_dset_items(new_dset, eigs_brik+2, "lambda_3",ADN_none);
        EDIT_dset_items(new_dset, eigs_brik+3, "eigvec_1[1]",ADN_none);
        EDIT_dset_items(new_dset, eigs_brik+4, "eigvec_1[2]",ADN_none);
        EDIT_dset_items(new_dset, eigs_brik+5, "eigvec_1[3]",ADN_none);
        EDIT_dset_items(new_dset, eigs_brik+6, "eigvec_2[1]",ADN_none);
        EDIT_dset_items(new_dset, eigs_brik+7, "eigvec_2[2]",ADN_none);
        EDIT_dset_items(new_dset, eigs_brik+8, "eigvec_2[3]",ADN_none);
        EDIT_dset_items(new_dset, eigs_brik+9, "eigvec_3[1]",ADN_none);
        EDIT_dset_items(new_dset, eigs_brik+10,"eigvec_3[2]",ADN_none);
        EDIT_dset_items(new_dset, eigs_brik+11,"eigvec_3[3]",ADN_none);
        EDIT_dset_items(new_dset, eigs_brik+12,"FA",ADN_none);
      }

      if(debug_briks) {
        EDIT_dset_items (new_dset, ADN_brick_label_one + nbriks - 3, "Converge Step", ADN_none);
        EDIT_dset_items (new_dset, ADN_brick_label_one + nbriks - 2, "ED", ADN_none);
        EDIT_dset_items (new_dset, ADN_brick_label_one + nbriks - 1, "EDorig", ADN_none);
      }

      tross_Make_History ("3dDWItoDT", argc, argv, new_dset);
      DSET_write (new_dset);
      printf ("--- Output dataset %s\n", DSET_FILECODE (new_dset));
    }
  else
    {
      fprintf (stderr, "*** Unable to compute output dataset!\n");
      exit (1);
    }

  exit (0);
}

/* Form R matrix as matrix of [bxx 2bxy 2bxz byy 2byz bzz] for Np rows */
static void
Form_R_Matrix (MRI_IMAGE * grad1Dptr)
{
  matrix Rmat;
  register double sf = 1.0;	/* scale factor = 1.0 for now until we know DELTA, delta (gamma = 267.5 rad/ms-mT) */
  register double sf2;		/* just scale factor * 2 */
  int i, nrows;
  register float *imptr, *Gxptr, *Gyptr, *Gzptr;
  matrix *nullptr = NULL;
  register double Gx, Gy, Gz;
  ENTRY ("Form_R_Matrix");
  nrows = grad1Dptr->nx;
  matrix_initialize (&Rmat);
  matrix_create (nrows, 6, &Rmat);	/* Rmat = Np x 6 matrix */
  if (Rmat.elts == NULL)
    {				/* memory allocation error */
      fprintf (stderr, "could not allocate memory for Rmat \n");
      EXRETURN;
    }
  sf2 = sf + sf;		/* 2 * scale factor for minor speed improvement */
  Gxptr = imptr = MRI_FLOAT_PTR (grad1Dptr);	/* use simple floating point pointers to get values */
  Gyptr = imptr + nrows;
  Gzptr = Gyptr + nrows;

  for (i = 0; i < nrows; i++)
    {
      Gx = *Gxptr++;
      Gy = *Gyptr++;
      Gz = *Gzptr++;
      Rmat.elts[i][0] = sf * Gx * Gx;	/* bxx = Gx*Gx*scalefactor */
      Rmat.elts[i][1] = sf2 * Gx * Gy;	/* 2bxy = 2GxGy*scalefactor */
      Rmat.elts[i][2] = sf2 * Gx * Gz;	/* 2bxz = 2GxGz*scalefactor */
      Rmat.elts[i][3] = sf * Gy * Gy;	/* byy = Gy*Gy*scalefactor */
      Rmat.elts[i][4] = sf2 * Gy * Gz;	/* 2byz = 2GyGz*scalefactor */
      Rmat.elts[i][5] = sf * Gz * Gz;	/* bzz = Gz*Gz*scalefactor */
    }

  matrix_initialize (&Rtmat);
  matrix_psinv (Rmat, nullptr, &Rtmat);	/* compute pseudo-inverse of Rmat=Rtmat */
  matrix_destroy (&Rmat);	/*  from the other two matrices */
  EXRETURN;
}


/**********************************************************************
   Function that does the real work
***********************************************************************/

static void
DWItoDT_tsfunc (double tzero, double tdelta,
		int npts, float ts[],
		double ts_mean, double ts_slope,
		void *ud, int nbriks, float *val)
{
  int i, converge_step, converge, trialstep, ntrial, adjuststep, recordflag;
  double orig_deltatau, EDold, J, dt;
  static int nvox, ncall, noisecall;
  register double i0;
  register double dv, dv0;
  vector lnvector;
  int wtflag;          /* wtflag for recomputing wtfactor*/
  int max_converge_step;

  ENTRY ("DWItoDT_tsfunc");
  /* ts is input vector data of Np+1 floating point numbers.
     For each point in volume brik convert vector data to
     symmetric matrix */
  /* ts should come from data sub-briks in form of I0,I1,...Ip */
  /* if automask is turned on, ts has Np+2 floating point numbers */
  /* val is output vector of form Dxx Dxy Dxz Dyy Dyz Dzz for each voxel in 6 sub-briks */
  /* the Dij vector is computed as the product of  Rt times ln(I0/Ip)
     where Rt is the pseudo-inverse of the [bxx 2bxy 2bxz byy 2byz bzz] for
     each gradient vector b */
   /** is this a "notification"? **/
   if (val == NULL)
    {

      if (npts > 0)
	{			/* the "start notification" */
	  nvox = npts;		/* keep track of   */
	  ncall = 0;		/* number of calls */
          noisecall = 0;
	}
      else
	{			/* the "end notification" */

	  /* nothing to do here */
	}
      EXRETURN;
    }

  ncall++;
  /* if there is an automask turned on, use corresponding voxel in the last sub-brik as a flag */
  if (automask)
    {
      npts = npts - 1;
      if (ts[npts] == 0)
	{			/* don't include this voxel for mask */
	  for (i = 0; i < nbriks; i++)	/* faster to copy preset vector */
	    val[i] = 0.0;	/* return 0 for all Dxx,Dxy,... */
          val[6] = -3.0;  /* use -3 as flag for number of converge steps to mean exited for automasked voxels */
	  EXRETURN;
	}
    }
  /* load the symmetric matrix vector from the "timeseries" subbrik vector values */
  vector_initialize (&lnvector);
  vector_create_noinit (npts - 1, &lnvector);
  dv0 = ts[0];
  if (dv0 > 0.0)
    i0 = log (dv0);
  else
    i0 = 0.0;
  for (i = 0; i < (npts - 1); i++)
    {
      dv = ts[i + 1];
      if ((dv > 0.0) && (dv0 > 0.0))
	lnvector.elts[i] = i0 - log (dv);	/* ln I0/Ip = ln I0 - ln Ip */
      else
	lnvector.elts[i] = 0.0;
    }

  vector_multiply (Rtmat, lnvector, &Dvector);	/* D = Rt * ln(I0/Ip), allocated Dvector here */

  vector_destroy (&lnvector);	/* free vector elements allocated */

  if((method==0)||(max_iter==-1)) {     /* for linear method,stop here and return D values */
     vector_to_array(Dvector, val);

     if(debug_briks) {
       InitWtfactors (npts);		/* initialize all weight factors to 1 for all gradient intensities */
       J = ComputeJ (ts, npts);	/* Ed (error) computed here */
       val[nbriks-3] = -1.0;  /* use -1 as flag for number of converge steps to mean exited for */
                              /* or initial insignificant deltatau value */
       val[nbriks-2] = ED;
       val[nbriks-1] = ED;                  /* store original error */
     }

     if(eigs_flag) {                            /* if user wants eigenvalues in output dataset */
        EIG_func();                              /* calculate eigenvalues, eigenvectors here */
        for(i=0;i<12;i++)
          val[i+6] = eigs[i];
       /* calc FA */
       val[19] = Calc_FA(val+6);                /* calculate fractional anisotropy */
     }
     EXRETURN;
  }
 
  /* now more complex part that takes into account noise */

  /* calculate initial estimate of D using standard linear model */
  EIG_func ();			/* compute eigenvalues, eigenvectors standard way */
  

  InitWtfactors (npts);		/* initialize all weight factors to 1 for all gradient intensities */

  ComputeD0 ();			/* recalculate Dmatrix based on limits on eigenvalues */


  if(matrix_sumabs(Dmatrix)<=TINYNUMBER) {
    for(i=0;i<nbriks;i++)
      val[i] = 0.0;
    if(debug_briks)
       val[6] = -2.0;  /* use -2 as flag for number of converge steps to mean exited for insignificant D values*/
    EXRETURN;
  }



  converge_step = 0;    /* allow up to max_iter=MAX_CONVERGE_STEPS (10) deltatau steps */
  max_converge_step = max_iter;   /* 1st time through set limit of converge steps to user option */
  converge = 0;
  wtflag = reweight_flag;

  /* trial step */
  J = ComputeJ (ts, npts);	/* Ed (error) computed here */
  Store_Computations (0, npts, wtflag);	/* Store 1st adjusted computations */
  matrix_copy (Dmatrix, &OldD);   /* store first Dmatrix in OldD too */

  if(debug_briks)
     val[nbriks-1] = ED;                  /* store original error */

  EDold = ED;
  ComputeDeltaTau ();
  if(deltatau<=TINYNUMBER) {         /* deltatau too small, exit */
    for(i=0;i<nbriks;i++)
      val[i] = 0.0;
    if(debug_briks)
      val[nbriks-3] = -1.0; /* use -1 as flag for number of converge steps to mean exited for insignificant deltatau value */
    EXRETURN;
  }

  if(verbose&&(!(noisecall%verbose)))
     recordflag = 1;
     else
     recordflag = 0;
  noisecall++;

#ifdef TESTING4
      if(recordflag==1){
	system("plugout_drive CLEAR_GRAPH_1D DWIEDgraph\n");
        sprintf(graphstring, "ADDTO_GRAPH_1D DWIEDgraph %f\n", ED);
	system("plugout_drive CLEAR_GRAPH_1D DWIDTgraph\n");
        sprintf(graphstring, "ADDTO_GRAPH_1D DWIDTgraph %f\n",deltatau);
      }      
#endif
      ntrial = 0;

      while ((converge_step < max_converge_step) && (converge!=1) && (ntrial < 10) )
        {
      /* find trial step */
      /* first do trial step to find acceptable delta tau */
      /* Take half of previous tau step to see if less error */
      /* if there is less error than delta tau is acceptable */
      /* try halving up to 10 times, if it does not work, */
      /* use first D from initial estimate */
        trialstep = 1;
        ntrial = 0;
        while ((trialstep) && (ntrial < 10))
	  {			/* allow up to 10 iterations to find trial step */
	    ComputeHpHm (deltatau);
	    ComputeNewD ();
	    J = ComputeJ (ts, npts);	/* Ed (error) computed here */
            if (ED < EDold)          /* is error less than error of trial step or previous step? */
	        {
	        /* found acceptable step size of DeltaTau */
                if(recordflag==1)
                 printf("ncall= %d, converge_step=%d, deltatau=%f, ED=%f, ntrial %d in find dtau\n", ncall, converge_step, deltatau, ED, ntrial);

#ifdef TESTING4
	    if(recordflag==1){
            sprintf(graphstring, "ADDTO_GRAPH_1D DWIEDgraph %f\n", ED); 
            system(graphstring);
            sprintf(graphstring, "ADDTO_GRAPH_1D DWIDTgraph %f\n", deltatau);
            system(graphstring);
            }
#endif
	        EDold = ED;
	        trialstep = 0;
   	        Store_Computations (1, npts, wtflag);	/* Store current computations */
	        }
            else {
                Restore_Computations (0, npts, wtflag);	/* Restore trial 0 computations */
                dt = deltatau / 2;
	        deltatau = dt;	/* find first DeltaTau step with less error than 1st guess */
   	        /* by trying smaller step sizes */
	        ntrial++;
	      }
	  }


	/* end of finding trial step size */
        /* in trial step stage, already have result of deltatau step and may have
           already tried deltatau*2 if halved (ntrial>=1) */
	if(ntrial <10) {
	  orig_deltatau = deltatau;
          adjuststep = 1;

 	  for(i=0;i<2;i++) {
            if(i==0)
	       deltatau = orig_deltatau*2;
	    else
	       deltatau = orig_deltatau/2;
	       
            if((adjuststep==1) && ((i!=0) || (ntrial<2))) {   /* if didn't shrink in initial deltatau step above */
              Restore_Computations (0, npts, wtflag);	/* Restore previous Tau step computations */
     	      ComputeHpHm (deltatau);
	      ComputeNewD ();
              J = ComputeJ (ts, npts);	/* computes Intensity without noise,*/
                                        /*   Ed, Residuals */
              if(ED<EDold){
                adjuststep = 0;
                Store_Computations(0, npts, wtflag);	/* Store Tau+dtau step computations */
                EDold = ED;

            if(recordflag==1)
              printf("ncall= %d, converge_step=%d, deltatau=%f, ED=%f dt*2 best\n", ncall, converge_step, deltatau, ED);

#ifdef TESTING4
	    if(recordflag==1){
             sprintf(graphstring, "ADDTO_GRAPH_1D DWIEDgraph %f\n", ED); 
             system(graphstring);
             sprintf(graphstring, "ADDTO_GRAPH_1D DWIDTgraph %f\n", deltatau);
             system(graphstring);
            }
#endif
              }
            }
          }

          if(adjuststep!=0){            /* best choice was first Delta Tau */
	     ED = EDold;
	     deltatau = orig_deltatau;
  	     Restore_Computations (1,  npts, wtflag);	/* restore old computed matrices*/
					   /*   D,Hp,Hm,F,R */
             Store_Computations(0, npts, wtflag);	/* Store Tau+dtau step computations */
	  }
         if(recordflag==1)
              printf("ncall= %d, converge_step=%d, deltatau=%f, ED=%f dt best\n", ncall, converge_step, deltatau, ED);

	 if (converge_step != 0) {	/* first time through recalculate*/
	     /* now see if converged yet */
             converge = TestConvergence(Dmatrix, OldD);
          }

          matrix_copy (Dmatrix, &OldD);
          if(recordflag==1)
              printf("ncall= %d, converge_step=%d, deltatau=%f, ED=%f\n", ncall, converge_step, deltatau, ED);

          converge_step++;
	}
	else
          {
	    if(recordflag==1)
             printf("ncall= %d, converge_step=%d, deltatau=%f, ED=%f Exiting time evolution\n", ncall, converge_step, deltatau, ED);
            Restore_Computations(0, npts, wtflag);       /* Exit with original step calculation */
            ED = EDold;
	  }


	if(((converge) || (converge_step==max_iter)) && wtflag && reweight_flag) {  /* if at end of iterations the first time*/
             converge = 0;                  /* through whole group of iterations */
             ComputeWtfactors (npts);       /* compute new weight factors */
             converge_step = 1;             /* start over now with computed weight factors */
             max_converge_step = max_iter_rw+1;   /* reset limit of converge steps to user option */
             wtflag = 0;                    /* only do it once - turn off next reweighting */
             ComputeJ(ts, npts);            /* compute new Ed value */
             EDold = ED;                    /* this avoids having to go through converge loop for two loops */
	  }
	}  /* end while converge loop */

          ED = EDold;     

  val[0] = Dmatrix.elts[0][0];
  val[1] = Dmatrix.elts[0][1];
  val[2] = Dmatrix.elts[0][2];
  val[3] = Dmatrix.elts[1][1];
  val[4] = Dmatrix.elts[1][2];
  val[5] = Dmatrix.elts[2][2];

  if(eigs_flag) {                            /* if user wants eigenvalues in output dataset */
    udmatrix_to_vector(Dmatrix, &Dvector);
    EIG_func();                              /* calculate eigenvalues, eigenvectors here */
    for(i=0;i<12;i++)
      val[i+6] = eigs[i];
    /* calc FA */
    val[18] = Calc_FA(val+6);                /* calculate fractional anisotropy */
  }

  /* testing information only */
  if(recordflag)
     printf("ncall= %d, converge_step=%d, deltatau=%f, ED=%f\n", ncall, converge_step, deltatau, ED);
  if(debug_briks) {
    val[nbriks-3] = converge_step;
    val[nbriks-2] = ED;
  }

  EXRETURN;
}

/* taken from 3dDTeig.c */
/* given Dij tensor data for principal directions */
/* calculate 3 principal sets of eigenvalues and eigenvectors */
static void
EIG_func ()
{
  /*  THD_dmat33 inmat;
     THD_dvecmat eigvmat; */
  int i, j;
  int maxindex, minindex, midindex;
  float temp, minvalue, maxvalue;
  int sortvector[3];
  double a[9], e[3];
  int astart, vstart;


  ENTRY ("EIG_func");
  /* Dvector is vector data of 6 floating point numbers.
     For each point in volume brik convert vector data to
     symmetric matrix */
  /* Dvector should come in form of Dxx,Dxy,Dxz,Dyy,Dyz,Dzz */
  /* convert to matrix of form 
     [ Dxx Dxy Dxz]
     [ Dxy Dyy Dyz]
     [ Dxz Dyz Dzz]  */


  /* load the symmetric matrix vector from the "timeseries" subbrik vector values */

  a[0] = Dvector.elts[0];
  a[1] = Dvector.elts[1];
  a[2] = Dvector.elts[2];
  a[3] = Dvector.elts[1];
  a[4] = Dvector.elts[3];
  a[5] = Dvector.elts[4];
  a[6] = Dvector.elts[2];
  a[7] = Dvector.elts[4];
  a[8] = Dvector.elts[5];

  symeig_double (3, a, e);	/* compute eigenvalues in e, eigenvectors in a */

  maxindex = 2;			/* find the lowest, middle and highest eigenvalue */
  maxvalue = e[2];
  minindex = 0;
  minvalue = e[0];
  midindex = 1;

  for (i = 0; i < 3; i++)
    {
      temp = e[i];
      if (temp > maxvalue)
	{			/* find the maximum */
	  maxindex = i;
	  maxvalue = temp;
	}
      if (temp < minvalue)
	{			/* find the minimum */
	  minindex = i;
	  minvalue = temp;
	}
    }

  for (i = 0; i < 3; i++)
    {				/* find the middle */
      if ((i != maxindex) && (i != minindex))
	{
	  midindex = i;
	  break;
	}
    }

  sortvector[0] = maxindex;
  sortvector[1] = midindex;
  sortvector[2] = minindex;

  /* put the eigenvalues at the beginning of the matrix */
  for (i = 0; i < 3; i++)
    {
      eigs[i] = e[sortvector[i]];	/* copy sorted eigenvalues */

      /* start filling in eigenvector values */
      astart = sortvector[i] * 3;	/* start index of double eigenvector */
      vstart = (i + 1) * 3;	/* start index of float val vector to copy eigenvector */

      for (j = 0; j < 3; j++)
	{
	  eigs[vstart + j] = a[astart + j];
	}
    }

  EXRETURN;
}

static float
Calc_FA(float *val)
/* calculate Fractional Anisotropy */
/* passed float pointer to start of eigenvalues */
{
  ENTRY("Calc_FA");

  float FA;
  double ssq, dv0, dv1, dv2, dsq;

  /* calculate the Fractional Anisotropy, FA */
  /*   reference, Pierpaoli C, Basser PJ. Microstructural and physiological features 
       of tissues elucidated by quantitative-diffusion tensor MRI,J Magn Reson B 1996; 111:209-19 */
  if((val[0]<=0.0)||(val[1]<=0.0)||(val[2]<=0.0)) {   /* any negative eigenvalues*/
    RETURN(0.0);                                      /* should not see any for non-linear method. Set FA to 0 */  
  }

  ssq = (val[0]*val[0])+(val[1]*val[1])+(val[2]*val[2]);        /* sum of squares of eigenvalues */
  /* dsq = pow((val[0]-val[1]),2.0) + pow((val[1]-val[2]),2.0) + pow((val[2]-val[0]),2.0);*/ /* sum of differences squared */

  dv0 = val[0]-val[1];
  dv0 *= dv0;
  dv1 = val[1]-val[2];
  dv1 *= dv1;
  dv2 = val[2]-val[0];
  dv2 *= dv2;
  dsq = dv0+dv1+dv2;                 /* sum of differences squared */

  if(ssq!=0.0)
    FA = sqrt(dsq/(2.0*ssq));   /* FA calculated here */
  else
    FA = 0.0;

  RETURN(FA);
}


static void
ComputeD0 ()
/* compute initial estimate of D0 */
/* D = estimated diffusion tensor matrix 
      [Dxx Dxy Dxz, Dxy Dyy Dyz, Dxz Dyz Dzz] */
/* updates Dvector and Dmatrix */
{
  int i, j;
  /*   matrix ULmatrix, Ematrix; */
  double mu, alpha, sum;
  double e10, e11, e12, e20, e21, e22, e30, e31, e32;
  double l1, l2, l3;
  double t1, t3, t5, t8, t10, t12, t14, t18, t19, t21, t23, t32, t33, t35,
    t37;

  ENTRY ("ComputeD0");
  /* create and initialize D0 */

  if (eigs[0] < 0)
    {				/* if all eigenvalues are negative - may never happen */
      /* D0 = diag(a,a,a) where a=1/3 Sum(Abs(Lambda_i)) */
      sum = 0.0;
      for (i = 0; i < 3; i++)
	sum += fabs (eigs[i]);
      alpha = sum / 3;
      for (i = 0; i < 3; i++)
	{
	  for (j = 0; j < 3; j++)
	    {
	      if (i == j)
		Dmatrix.elts[i][j] = alpha;
	      else
		Dmatrix.elts[i][j] = 0.0;
	    }
	}

      udmatrix_to_vector (Dmatrix, &Dvector);	/* convert to vector format for D also */
      EXRETURN;
    }

  mu = 0.2 * eigs[0];
  if (eigs[1] < mu)		/* set limit of eigenvalues to prevent */
     eigs[1] = mu;		/*     too much anisotropy */
  if (eigs[2] < mu)
     eigs[2] = mu;

  /* D0 = U L UT */
/*
                            [e10 l1    e20 l2    e30 l3]
                            [                          ]
                      UL := [e11 l1    e21 l2    e31 l3]
                            [                          ]
                            [e12 l1    e22 l2    e32 l3]
*/


  /* assign variables to match Maple code */
  l1 = eigs[0];
  l2 = eigs[1];
  l3 = eigs[2];

  e10 = eigs[3];
  e11 = eigs[4];
  e12 = eigs[5];

  e20 = eigs[6];
  e21 = eigs[7];
  e22 = eigs[8];

  e30 = eigs[9];
  e31 = eigs[10];
  e32 = eigs[11];


#ifdef lkjsaklfj
  matrix_initialize (&Ematrix);
  matrix_create (3, 3, &Ematrix);
  /* fill Ematrix with Eigenvectors */

  matrix_initialize (&ULmatrix);
  matrix_create (3, 3, &ULmatrix);
  if (ULmatrix.elts == NULL)
    {				/* memory allocation error */
      fprintf (stderr, "could not allocate memory for Rmat \n");
      EXRETURN;
    }

  for (i = 0; i < 3; i++)
    {
      for (j = 0; j < 3; j++)
	{
	  ULmatrix.elts[i][j] = Ematrix.elts[i][j] * eigs[i];
	}
    }

  matrix_multiply (ULmatrix, Ematrix, &Dmatrix);	/* new D is based on modified lambdas */
#endif



  t1 = e10 * e10;
  t3 = e20 * e20;
  t5 = e30 * e30;
  t8 = e10 * l1;
  t10 = e20 * l2;
  t12 = e30 * l3;
  t14 = t8 * e11 + t10 * e21 + t12 * e31;
  t18 = t8 * e12 + t10 * e22 + t12 * e32;
  t19 = e11 * e11;
  t21 = e21 * e21;
  t23 = e31 * e31;
  t32 = e11 * l1 * e12 + e21 * l2 * e22 + e31 * l3 * e32;
  t33 = e12 * e12;
  t35 = e22 * e22;
  t37 = e32 * e32;
  Dmatrix.elts[0][0] = t1 * l1 + t3 * l2 + t5 * l3;
  Dmatrix.elts[0][1] = t14;
  Dmatrix.elts[0][2] = t18;
  Dmatrix.elts[1][0] = t14;
  Dmatrix.elts[1][1] = t19 * l1 + t21 * l2 + t23 * l3;
  Dmatrix.elts[1][2] = t32;
  Dmatrix.elts[2][0] = t18;
  Dmatrix.elts[2][1] = t32;
  Dmatrix.elts[2][2] = t33 * l1 + t35 * l2 + t37 * l3;

  udmatrix_to_vector (Dmatrix, &Dvector);	/* convert to vector format for D */

  EXRETURN;
}

static void
Computebmatrix (MRI_IMAGE * grad1Dptr)
/* compute the diffusion weighting matrix bmatrix for q number of gradients */
/* only need to calculate once */
/* bq = diffusion weighting matrix of qth gradient */
/*      GxGx GxGy GxGz
        GxGy GyGy GyGz
        GxGz GyGz GzGz */
/* b0 is 0 for all 9 elements */
/* bmatrix is really stored as 6 x npts array */
{
  int i, n;
  register double *bptr;
  register float *Gxptr, *Gyptr, *Gzptr;
  double Gx, Gy, Gz;

  ENTRY ("Computebmatrix");
  n = grad1Dptr->nx;		/* number of gradients other than I0 */
  Gxptr = MRI_FLOAT_PTR (grad1Dptr);	/* use simple floating point pointers to get values */
  Gyptr = Gxptr + n;
  Gzptr = Gyptr + n;

  bptr = bmatrix;
  for (i = 0; i < 6; i++)
    *bptr++ = 0.0;		/* initialize first 6 elements to 0.0 for the I0 gradient */

  for (i = 0; i < n; i++)
    {
      Gx = *Gxptr++;
      Gy = *Gyptr++;
      Gz = *Gzptr++;
      *bptr++ = Gx * Gx;
      *bptr++ = Gx * Gy;
      *bptr++ = Gx * Gz;
      *bptr++ = Gy * Gy;
      *bptr++ = Gy * Gz;
      *bptr++ = Gz * Gz;
    }
  EXRETURN;
}

static double
ComputeJ (float ts[], int npts)
/* compute non-gradient intensity, J, based on current calculated values of 
   diffusion tensor matrix, D */
{
  /* J = Sum(wq Iq exp(-bq D)) / Sum (wq exp(-2bq D)) */
  /*     estimate of b0 intensity without noise and applied gradient */
  /* Iq = voxel value for qth gradient */
  /* bq = diffusion weighting matrix of qth gradient */
  /* wq = weighting factor for qth gradient at Iq voxel */
  /* D = estimated diffusion tensor matrix 
     [Dxx Dxy Dxz, Dxy Dyy Dyz, Dxz Dyz Dzz] */
  /* ts = Iq is time series voxel data from original data of intensities */

  register int i, j;
  double sum0, sum1, b1D1, b2D2, b4D4, wtexpbD, J, tempcalc, sumbD, Fscalar;
  double *expbD, *expbDptr, *wtfactorptr, *Ftempmatrix;
  register double *Fptr, *Rptr, *bptr;
  double D0,D1,D2,D3,D4,D5;

  ENTRY ("ComputeJ");
  sum0 = sum1 = 0.0;
  expbD = malloc (npts * sizeof (double));	/* allocate calculations for speed */
  expbDptr = expbD;		/* temporary pointers for indexing */
  wtfactorptr = wtfactor;
  bptr = bmatrix;		/* npts of b vectors (nx6) */

  D0 = Dmatrix.elts[0][0];
  D1 = Dmatrix.elts[0][1];
  D2 = Dmatrix.elts[0][2];
  D3 = Dmatrix.elts[1][1];
  D4 = Dmatrix.elts[1][2];
  D5 = Dmatrix.elts[2][2];


  for (i = 0; i < npts; i++)
    {
      /* compute bq.D */
      /* bq.D is large dot product of b and D at qth gradient */
      /* large dot product for Hilbert algebra */
      /* regular dot product is for Hilbert space (vectors only)- who knew? */
      /* calculate explicitly rather than loop to save time */
      b1D1 = *(bptr + 1) * D1;
      b1D1 += b1D1;
      b2D2 = *(bptr + 2) * D2;
      b2D2 += b2D2;
      b4D4 = *(bptr + 4) * D4;
      b4D4 += b4D4;

      sumbD = *bptr * D0 + b1D1 + b2D2 +	/* bxxDxx + 2bxyDxy +  2bxzDxz + */
	(*(bptr + 3) * D3) +	/* byyDyy + */
	b4D4 +			/* 2byzDyz + */
	(*(bptr + 5) * D5);	/* bzzDzz */

      /*  exp (-bq.D) */
      *expbDptr = exp (-sumbD);
      wtexpbD = *(wtfactor + i) * *expbDptr;
      sum0 += wtexpbD * ts[i];
      sum1 += wtexpbD * *expbDptr;
      expbDptr++;
      wtfactorptr++;
      bptr += 6;		/* increment to next vector of bmatrix */
    }

  J = sum0 / sum1;
  /* Now compute error functional,E(D,J) and gradient of E with respect to D ,Ed or F in notes */
  /* E(D,J)= 1/2 Sum[wq (J exp(-bq.D) - Iq)^2] */
  /* F = Ed =  - Sum[wq (J exp(-bq.D) - Iq) bq] *//* Ed is a symmetric matrix */
  sum0 = 0.0;
  sigma = 0.0;			/* standard deviation of noise for weight factors */
  expbDptr = expbD;
  wtfactorptr = wtfactor;
  /* initialize F matrix */
  Ftempmatrix = malloc(6*sizeof(double));
  Fptr = Ftempmatrix;
  for(i=0;i<6;i++)
    *Fptr++ = 0.0;
  Fptr = Ftempmatrix;
  Rptr = Rvector;		/* residuals calculated here - used in Wt.factor calculations */
  bptr = bmatrix;		/* npts of b vectors (nx6) */
  for (i = 0; i < npts; i++)
    {
      *Rptr = tempcalc = (J * *expbDptr) - ts[i];
      Fscalar = -*wtfactorptr * tempcalc;
      tempcalc = tempcalc * tempcalc;

      for (j = 0; j < 6; j++)
	{			/* for each entry of Fij (Fxx, Fxy,...) */
	  /* F = - Sum[wq (J exp(-bq.D) - Iq) bq] = Sum[-wq (J exp(-bq.D) - Iq) bq] */
	  *(Fptr+j) += Fscalar * (*bptr++);	/*  Fij = Fij + (Fscalar * bij)  */
	}

      sum0 += *wtfactorptr * tempcalc;	/* E(D,J) = Sum (wq temp^2) */
      sigma += tempcalc;	/* standard deviation of noise for weight factors */
      expbDptr++;
      wtfactorptr++;
      Rptr++;
    }

  udmatrix_copy (Ftempmatrix, &Fmatrix);	/* copy upper diagonal vector data into full matrix */

  ED = sum0 / 2;		/* this is the error for this iteration */

  free (Ftempmatrix);
  free (expbD);
  RETURN (J);
}

static void
ComputeDeltaTau ()
/*compute initial step size for gradient descent */
{
  double sum0, sum1;
  matrix Dsqmatrix, FDsqmatrix, DsqFmatrix, Gmatrix;
  /* compute estimate of gradient, dD/dtau */
  /*G = [F] [D]^2 + [D]^2 [F] - ask Bob about ^2 and negative for this part to be sure */

  ENTRY ("ComputeDeltaTau");
  matrix_initialize (&Dsqmatrix);
  matrix_initialize (&FDsqmatrix);
  matrix_initialize (&DsqFmatrix);
  matrix_initialize (&Gmatrix);

  matrix_multiply (Dmatrix, Dmatrix, &Dsqmatrix);	/* compute D^2 */
  matrix_multiply (Fmatrix, Dsqmatrix, &FDsqmatrix);	/* FD^2 */
  matrix_multiply (Dsqmatrix, Fmatrix, &DsqFmatrix);	/* D^2F */
  matrix_add (FDsqmatrix, DsqFmatrix, &Gmatrix);	/* G= FD^2 +D^2F */


  /* deltatau = 0.01 * Sum(|Dij|) / Sum (|Gij|) */
  sum0 = matrix_sumabs (Dmatrix);
  sum1 = matrix_sumabs (Gmatrix);
  if (sum1 != 0.0)
    deltatau = 0.01 * sum0 / sum1;
  else
    deltatau = 0.0;
  matrix_destroy (&Dsqmatrix);
  matrix_destroy (&FDsqmatrix);
  matrix_destroy (&DsqFmatrix);
  matrix_destroy (&Gmatrix);
  EXRETURN;
}


static void
InitGlobals (int npts)
/* allocate all the global matrices and arrays once */
{
  ENTRY ("InitGlobals");
  int i;
  double *cumulativewtptr;

  matrix_initialize (&Fmatrix);
  matrix_create (3, 3, &Fmatrix);
  matrix_initialize (&Dmatrix);
  matrix_create (3, 3, &Dmatrix);
  matrix_initialize (&Hplusmatrix);
  matrix_create (3, 3, &Hplusmatrix);
  matrix_initialize (&Hminusmatrix);
  matrix_create (3, 3, &Hminusmatrix);
  matrix_initialize (&OldD);
  matrix_create (3, 3, &OldD);
  for(i=0;i<2;i++){
    matrix_initialize (&tempFmatrix[i]);
    matrix_create (3, 3, &tempFmatrix[i]);
    matrix_initialize (&tempDmatrix[i]);
    matrix_create (3, 3, &tempDmatrix[i]);
    matrix_initialize (&tempHplusmatrix[i]);
    matrix_create (3, 3, &tempHplusmatrix[i]);
    matrix_initialize (&tempHminusmatrix[i]);
    matrix_create (3, 3, &tempHminusmatrix[i]);
  }
  Rvector = malloc (npts * sizeof (double));
  tempRvector = malloc (npts * sizeof(double));
  wtfactor = malloc (npts * sizeof (double));

  if(cumulative_flag && reweight_flag) {
     cumulativewt = malloc (npts * sizeof (double));
     cumulativewtptr = cumulativewt;
     for(i=0;i<npts;i++)
        *cumulativewtptr++ = 0.0;
     rewtvoxels = 0;
  }

  bmatrix = malloc (npts * 6 * sizeof (double));

  vector_initialize (&Dvector);	/* need to initialize vectors before 1st use-mod drg 12/20/2004 */
  /*  vector_initialize (&tempDvector);  vector_create(npts, &tempDvector);*/
  EXRETURN;
}

static void
FreeGlobals ()
/* free up all the matrices and arrays */
{
  ENTRY ("FreeGlobals");
  int i;

  matrix_destroy (&Fmatrix);
  matrix_destroy (&Dmatrix);
  matrix_destroy (&Hplusmatrix);
  matrix_destroy (&Hminusmatrix);
  matrix_destroy (&OldD);
  for(i=0;i<2;i++){
    matrix_destroy (&tempFmatrix[i]);
    matrix_destroy (&tempDmatrix[i]);
    matrix_destroy (&tempHplusmatrix[i]);
    matrix_destroy (&tempHminusmatrix[i]);
  }


  free (wtfactor);
  wtfactor = NULL;
  free (bmatrix);
  bmatrix = NULL;
  free (Rvector);
  Rvector = NULL;
  free (tempRvector);
  tempRvector = NULL;
  vector_destroy (&Dvector);	/* need to free elements of Dvector - mod-drg 12/20/2004 */
  /*  vector_destroy (&tempDvector);*/
  if(cumulative_flag && reweight_flag) {
    free (cumulativewt);
    cumulativewt = NULL;
  }
  EXRETURN;
}

static void
Store_Computations (int i, int npts, int wtflag)
/* store current computed matrices D,Hp,Hm, R */
{
  ENTRY ("Store_Computations");

  matrix_copy (Fmatrix, &tempFmatrix[i]);
  matrix_copy (Dmatrix, &tempDmatrix[i]);
  matrix_copy (Hplusmatrix, &tempHplusmatrix[i]);
  matrix_copy (Hminusmatrix, &tempHminusmatrix[i]);
  if(wtflag==1) 
    memcpy(tempRvector, Rvector, npts*sizeof(double));
  EXRETURN;
}


static void
Restore_Computations (int i, int npts, int wtflag)
/* restore old computed matrices D,Hp,Hm, R */
{
  ENTRY ("Restore_Computations");

  matrix_copy (tempFmatrix[i], &Fmatrix);
  matrix_copy (tempDmatrix[i], &Dmatrix);
  matrix_copy (tempHplusmatrix[i], &Hplusmatrix);
  matrix_copy (tempHminusmatrix[i], &Hminusmatrix);
  if(wtflag==1)
    memcpy(Rvector, tempRvector, npts*sizeof(double));
  EXRETURN;
}


static void
InitWtfactors (int npts)
/* set all wt factors for all gradient levels to be 1.0 the first time through */
{
  double *wtfactorptr;
  int i;

  ENTRY ("InitWtfactors");
  wtfactorptr = wtfactor;
  for (i = 0; i < npts; i++)
    *wtfactorptr++ = 1.0;
  EXRETURN;
}

static void
ComputeWtfactors (int npts)
/* compute wt factors for each gradient level */
{
  /* Residuals, rq, computed above in ComputeJ, stored in Rmatrix */
  /* unnormalized standard deviation, sigma, computed there too */
/*  wq = 1 / sqrt(1 + (rq/sigma)^2)
    where sigma = sqrt[1/Nq Sum(rq^2)] */
/*  and rq = J exp(-bq.D) - Iq */

  int i;
  double *wtfactorptr, *Rptr;
  double *cumulativewtptr;
  double tempcalc, sum;

  ENTRY ("ComputeWtfactors");
  sigma = sigma / npts;
  sigma = sqrt (sigma);		/* sigma = std.dev. */

  wtfactorptr = wtfactor;
  Rptr = Rvector;

  sum = 0.0;
  for (i = 0; i < npts; i++)
    {
      tempcalc = *Rptr++ / sigma;
      tempcalc = tempcalc * tempcalc;
      tempcalc = 1.0 / (sqrt (1 + tempcalc));
      *wtfactorptr++ = tempcalc;
      sum += tempcalc;
    }
  /* now renormalize to avoid changing the relative value of E(D) */
  tempcalc = npts / sum;     /* normalization factor */
  
  wtfactorptr = wtfactor;
  for (i=0; i<npts; i++)
      *wtfactorptr++ = *wtfactorptr * tempcalc;

  if(cumulative_flag) {
     wtfactorptr = wtfactor;
     cumulativewtptr = cumulativewt;
     /*  printf("Wt.factors: ");*/
     ++rewtvoxels;
     for (i=0; i<npts; i++){
        *cumulativewtptr++ += *wtfactorptr++;   /* calculate cumulative wt.factor across all voxels*/
     }
  }

 EXRETURN;
}

static void
ComputeHpHm (double deltatau)
/* compute Hplus and Hminus as a function of delta tau */
/* H+- = I +/- 1/2 deltatau F D */
{
  matrix FDmatrix;
  double dtau;
  int i, j;

  ENTRY ("ComputeHpHm");
  dtau = 0.5 * deltatau;

  matrix_initialize (&FDmatrix);
  matrix_multiply (Fmatrix, Dmatrix, &FDmatrix);
  for (i = 0; i < 3; i++)
    for (j = 0; j < 3; j++)
      FDmatrix.elts[i][j] = dtau * FDmatrix.elts[i][j];

  for (i = 0; i < 3; i++)
    {
      for (j = 0; j < 3; j++)
	{
	  if (i == j)
	    {
	      Hplusmatrix.elts[i][j] = 1 + FDmatrix.elts[i][j];	/* I + dt/2 * FD */
	      Hminusmatrix.elts[i][j] = 1 - FDmatrix.elts[i][j];	/* I - dt/2 * FD */
	    }
	  else
	    {
	      Hplusmatrix.elts[i][j] = Hminusmatrix.elts[i][j] =
		FDmatrix.elts[i][j];
	    }
	}
    }

  matrix_destroy (&FDmatrix);
  EXRETURN;
}


static void
ComputeNewD ()
/* compute new D matrix */
/* D(tau+deltatau) = H-  H+^-1  D(tau)  H+^-1 H- */
/*                 = A          D(tau)  A^T */
/* where A = H- H+^-1 */
{
  double *Hpinv;
  matrix Hpinvmatrix, Amatrix, ATmatrix, ADmatrix;

  ENTRY ("ComputeNewD");

  Hpinv =
    InvertSym3 (Hplusmatrix.elts[0][0], Hplusmatrix.elts[0][1],
		Hplusmatrix.elts[0][2], Hplusmatrix.elts[1][1],
		Hplusmatrix.elts[1][2], Hplusmatrix.elts[2][2]);
  matrix_initialize (&Hpinvmatrix);
  matrix_initialize (&Amatrix);
  matrix_initialize (&ATmatrix);
  matrix_initialize (&ADmatrix);

  matrix_create (3, 3, &Hpinvmatrix);
  udmatrix_copy (Hpinv, &Hpinvmatrix);	/* copy values from Hpinv vector to Hpinvmatrix */

  matrix_multiply (Hminusmatrix, Hpinvmatrix, &Amatrix);
  matrix_multiply (Amatrix, Dmatrix, &ADmatrix);
  matrix_transpose (Amatrix, &ATmatrix);
  matrix_multiply (ADmatrix, ATmatrix, &Dmatrix);

  matrix_destroy (&ADmatrix);
  matrix_destroy (&ATmatrix);
  matrix_destroy (&Amatrix);
  matrix_destroy (&Hpinvmatrix);

  free (Hpinv);
  EXRETURN;
}

static int
TestConvergence(matrix NewD, matrix OldD)
/* test convergence of calculation of D */
/* if sum of differences hasn't changed by more than 1E-4 */
/*  then the calculations have converged */
{ 
  int converge;
  double convergence;

  ENTRY ("TestConvergence");
  /* convergence test */
  convergence = fabs (NewD.elts[0][0] - OldD.elts[0][0]) +	/* Dxx */
    fabs (NewD.elts[0][1] - OldD.elts[0][1]) +	/* Dxy */
    fabs (NewD.elts[0][2] - OldD.elts[0][2]) +	/* Dxz */
    fabs (NewD.elts[1][1] - OldD.elts[1][1]) +	/* Dyy */
    fabs (NewD.elts[1][2] - OldD.elts[1][2]) +	/* Dyz */
    fabs (NewD.elts[2][2] - OldD.elts[2][2]);	/* Dzz */

  if (convergence < SMALLNUMBER)
    converge = 1;
  else
    converge = 0;

  RETURN (converge);
}

static void
udmatrix_copy (double *udptr, matrix * m)
/* copy an upper diagonal matrix (6 point vector really) into a standard double
   array type matrix for n timepoints */
/* ud0 ud1 ud2         m0 m1 m2
       ud3 ud4   -->   m3 m4 m5
           ud5         m6 m7 m8 */
{
  ENTRY ("udmatrix_copy");

  m->elts[0][0] = *udptr;
  m->elts[0][1] = *(udptr + 1);
  m->elts[0][2] = *(udptr + 2);
  m->elts[1][0] = *(udptr + 1);
  m->elts[1][1] = *(udptr + 3);
  m->elts[1][2] = *(udptr + 4);
  m->elts[2][0] = *(udptr + 2);
  m->elts[2][1] = *(udptr + 4);
  m->elts[2][2] = *(udptr + 5);
  EXRETURN;
}

static void
udmatrix_to_vector (matrix m, vector * v)
/* copy upper part of 3x3 matrix elements to 6-element vector elements */
/* m1 m2 m3
      m4 m5   ->  v = [m1 m2 m3 m4 m5 m6]
         m6
*/
{
  ENTRY ("udmatrix_to_vector");
  v->elts[0] = m.elts[0][0];
  v->elts[1] = m.elts[0][1];
  v->elts[2] = m.elts[0][2];
  v->elts[3] = m.elts[1][1];
  v->elts[4] = m.elts[1][2];
  v->elts[5] = m.elts[2][2];
  EXRETURN;
}


static double
matrix_sumabs (matrix m)
/* sum the absolute value of all  elements of a matrix */
{
  register int i, j;
  register double sum;

  ENTRY ("matrix_sumabs");
  sum = 0.0;
  for (i = 0; i < 3; i++)
    {
      for (j = 0; j < 3; j++)
	sum += fabs (m.elts[i][j]);
    }
  RETURN (sum);
}


static double *
InvertSym3 (double a, double b, double c, double e, double f, double i)
/* calculate inverse of a symmetric 3x3 matrix */
/* returns pointer to 9 element vector corresponding to 3x3 matrix */
/*  a b c */
/*  b e f */
/*  c f i */
/* Maple generated code */
{
  double *symmat, *symmatptr;	/* invert matrix - actually 6 values in a vector form */
  double t2, t4, t7, t9, t12, t15, t20, t24, t30;

  ENTRY ("InvertSym3");
  symmat = malloc (6 * sizeof (double));
  symmatptr = symmat;
  t2 = f * f;
  t4 = a * e;
  t7 = b * b;
  t9 = c * b;
  t12 = c * c;
  t15 = 1 / (t4 * i - a * t2 - t7 * i + 2.0 * t9 * f - t12 * e);
  t20 = (b * i - c * f) * t15;
  t24 = (b * f - c * e) * t15;
  t30 = (a * f - t9) * t15;

  *symmatptr++ = (e * i - t2) * t15;	/*B[0][0] */
  *symmatptr++ = -t20;		/*B[0][1] */
  *symmatptr++ = t24;		/*B[0][2] */
  /* B[1][0] = -t20; */
  *symmatptr++ = (a * i - t12) * t15;	/* B[1][1] */
  *symmatptr++ = -t30;		/* B [1][2] */
  /* B[2][0] = t24; */
  /* B[2][1] = -t30; */
  *symmatptr = (t4 - t7) * t15;	/* B[2][2] */

  RETURN (symmat);
}

static void
matrix_copy (matrix a, matrix * b)
/* copy elements from matrix a to matrix b */
/*  matrix_equate already exists but creates and initializes new matrix */
/*  steps we don't need to do here */
/* assumes both a and b already exist with equal dimensions */
{
  ENTRY ("matrix_copy");
  register int i;
  register int rows, cols;

  rows = a.rows;
  cols = a.cols;

  for (i = 0; i < rows; i++)
    {
#if 0
      register int j;
      for (j = 0; j < cols; j++)
	b->elts[i][j] = a.elts[i][j];
#else
      if (cols > 0)
	memcpy (b->elts[i], a.elts[i], sizeof (double) * cols);	/* RWCox */
#endif
    }
  EXRETURN;
}


#ifdef DWIAFNITALK
/* from SUMA_NIML.c and SUMA_main.c */
/* Open NIML stream */
/*   open stream with tcp, reopen with shared memory */
/*   test writing for connection and timeout */
/* Write NIML stream */
/*   create NIML structure - type and parameters in columns */
/*   send structure on stream with niml write command */
/* Kill NIML stream */
#endif
