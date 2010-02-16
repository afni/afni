/*********************** 3dDTtoDWI.c **********************************************/
/* Author: Daniel Glen, 17 Nov 2004 */
/* compute multiple gradient images from 6 principle direction tensors*/
/* and corresponding gradient vector coordinates */
/* used for modeling diffusion tensor data */

#include "thd_shear3d.h"
/*#ifndef FLOATIZE*/
# include "matrix.h"
/*#endif*/
#include "afni.h"

#define TINYNUMBER 1E-10
#define SMALLNUMBER 1E-4

static char prefix[THD_MAX_PREFIX] = "DT";
static int datum = MRI_float;
static matrix Dmatrix;
static vector Dvector;

static double *bmatrix;		/* b matrix = GiGj for gradient intensities */

static float *I0_ptr;           /* I0 matrix */
static int automask = 0;        /* automasking flag - user option */

static void DTtoDWI_tsfunc (double tzero, double tdelta, int npts, float ts[], double ts_mean, double ts_slope, void *ud, int nbriks, float *val);
static void Computebmatrix (MRI_IMAGE * grad1Dptr);
static void InitGlobals (int npts);
static void FreeGlobals (void);

int
main (int argc, char *argv[])
{
  THD_3dim_dataset *old_dset, *new_dset, *I0_dset;	/* input and output datasets */
  int nopt, nbriks, nvox;
  int i, eigs_brik;
  MRI_IMAGE *grad1Dptr = NULL;
  MRI_IMAGE *anat_im = NULL;
  MRI_IMAGE *data_im = NULL;
  double fac;
  short *sar = NULL, *tempsptr = NULL, tempval;
  byte *maskptr = NULL, *tempbptr = NULL;
  char tempstr[25];

   /*----- Read command line -----*/
  if (argc < 2 || strcmp (argv[1], "-help") == 0)
    {
      printf ("Usage: 3dDTtoDWI [options] gradient-file I0-dataset DT-dataset\n"
	      "Computes  multiple gradient images from 6 principle direction tensors and\n"
              "    corresponding gradient vector coordinates applied to the I0-dataset.\n"
	      " The program takes three parameters as input :  \n"
	      "    a 1D file of the gradient vectors with lines of ASCII floats Gxi,Gyi,Gzi.\n"
              "    Only the non-zero gradient vectors are included in this file (no G0 line).\n"
              " The I0 dataset is a volume without any gradient applied.\n"
              " The DT dataset is the 6-sub-brick dataset containing the diffusion tensor data,\n"
              "    Dxx, Dxy, Dxz, Dyy, Dyz, Dzz\n"
	      " Options:\n"
              "   -prefix pname = Use 'pname' for the output dataset prefix name.\n"
              "    [default='DWI']\n"
	      "   -automask =  mask dataset so that the gradient images are computed only for\n"
	      "    high-intensity (presumably brain) voxels.  The intensity level is\n"
              "    determined the same way that 3dClipLevel works.\n\n"
              "   -datum type = output dataset type [float/short/byte] (default is float).\n"
              "   -help = show this help screen.\n"
              " Example:\n"
              "  3dDTtoDWI -prefix DWI -automask tensor25.1D 'DT+orig[26]' DT+orig.\n\n"
	      " The output is a n sub-brick bucket dataset containing computed DWI images.\n"
              "    where n is the number of vectors in the gradient file + 1\n"
	      "\n");
      printf ("\n" MASTER_SHORTHELP_STRING);
      exit (0);
    }

  mainENTRY ("3dDTtoDWI main");
  machdep ();
  AFNI_logger ("3dDTtoDWI", argc, argv);
  PRINT_VERSION("3dDTtoDWI") ;

  nopt = 1;
  datum = MRI_float;


  while (nopt < argc && argv[nopt][0] == '-')
    {

      /*-- prefix --*/

      if (strcmp (argv[nopt], "-prefix") == 0)
	{
	  if (++nopt >= argc)
	    {
	      fprintf (stderr, "*** Error - prefix needs an argument!\n");
	      exit (1);
	    }
	  MCW_strncpy (prefix, argv[nopt], THD_MAX_PREFIX);	/* change name from default prefix */
          /* check file name to be sure not to overwrite - mod drg 12/9/2004 */
	  if (!THD_filename_ok (prefix))
	    {
	      fprintf (stderr, "*** Error - %s is not a valid prefix!\n", prefix);
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
	      fprintf (stderr, "*** Error - datum needs an argument!\n");
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

	fprintf(stderr, "*** Error - unknown option %s\n", argv[nopt]);
	exit(1);
    }
  
   /*----- read input datasets -----*/

  if (nopt >= argc)
    {
      fprintf (stderr, "*** Error - No input dataset!?\n");
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
      fprintf (stderr, "*** Error - Only 3 columns of gradient vectors allowed\n");
      fprintf (stderr, "%d columns found\n", grad1Dptr->nx);
      mri_free (grad1Dptr);
      exit (1);
    }

  if (grad1Dptr->nx < 6)
    {
      fprintf (stderr, "*** Error - Must have at least 6 gradient vectors\n");
      fprintf (stderr, "%d columns found\n", grad1Dptr->nx);
      mri_free (grad1Dptr);
      exit (1);
    }

  nbriks = grad1Dptr->nx + 1;    /* number of gradients specified here from file */     
  nopt++;

  /* open I0 dataset - idealized no gradient image */
  I0_dset = THD_open_dataset (argv[nopt]);
  CHECK_OPEN_ERROR(IO_dset,argv[nopt]) ;

   DSET_mallocize (I0_dset);
   DSET_load (I0_dset);	                /* load dataset */
   data_im = DSET_BRICK (I0_dset, 0);	/* set pointer to the 0th sub-brik of the dataset */
   fac = DSET_BRICK_FACTOR(I0_dset, 0); /* get scale factor for each sub-brik*/
   if(fac==0.0) fac=1.0;
   if((data_im->kind != MRI_float) || (fac!=1.0)) {
       fprintf (stderr, "*** Error - Can only open float datasets with scale factors of 1\n");
       mri_free (grad1Dptr);
       mri_free (data_im);
       exit (1);
   }

   I0_ptr = mri_data_pointer(data_im) ; /* pointer to I0 data */

   nopt++;

  /* Now read in all the MRI volumes for each gradient vector */
  /* assumes first one is no gradient */
  old_dset = THD_open_dataset (argv[nopt]);
  CHECK_OPEN_ERROR(old_dset,argv[nopt]) ;

  /* expect at least 6 values per voxel - 6 sub-briks as input dataset */
  if (DSET_NVALS (old_dset) <6)
    {
      fprintf (stderr,
      "*** Error - Dataset must have at least 6 sub-briks to describe the diffusion tensor\n");
      mri_free (grad1Dptr);
      mri_free (data_im);
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

   /*------------- ready to compute new dataset -----------*/

  new_dset = MAKER_4D_to_typed_fbuc (old_dset,	/* input dataset */
				     prefix,	/* output prefix */
				     datum,	/* output datum  */
				     0,	/* ignore count  */
				     0,	/* can't detrend in maker function  KRH 12/02 */
				     nbriks,	/* number of briks */
				     DTtoDWI_tsfunc,	/* timeseries processor */
				     NULL,	/* data for tsfunc */
                 NULL   /* mask */
    );



  FreeGlobals ();
  mri_free (grad1Dptr);


  if (automask)
    {
      mri_free (anat_im);
      DSET_unload_one (old_dset, 0);
      sar = NULL;
    }

  if (new_dset != NULL)
    {
      tross_Copy_History (old_dset, new_dset);
      for(i=0;i<nbriks;i++) {
        sprintf(tempstr,"grad%3.3d", i);
        EDIT_dset_items (new_dset, ADN_brick_label_one + i, tempstr, ADN_none);
      }
      tross_Make_History ("3dDTtoDWI", argc, argv, new_dset);
      DSET_write (new_dset);
      fprintf(stderr,"--- Output dataset %s\n", DSET_BRIKNAME(new_dset));
    }
  else
    {
      fprintf (stderr, "*** Error - Unable to compute output dataset!\n");
      exit (1);
    }

  exit (0);
}

/**********************************************************************
   Function that does the real work
***********************************************************************/

static void
DTtoDWI_tsfunc (double tzero, double tdelta,
		int npts, float ts[],
		double ts_mean, double ts_slope,
		void *ud, int nbriks, float *val)
{
  int i, j;
  static int nvox, ncall;
  int allzeros;
  double I0, bq_d;
  double *bptr;
  float *tempptr;
 
  ENTRY ("DTtoDWI_tsfunc");
  /* ts is input vector data of 6 floating point numbers.*/
  /* ts should come from data sub-briks in form of Dxx, Dxy, Dxz, Dyy, Dyz, Dzz */
  /* if automask is turned on, ts has 7 floating point numbers */
  /* val is output vector of form DWI0, DWI1, DWIn sub-briks */
  /* where n = number of gradients = nbriks-1 */

   /** is this a "notification"? **/
   if (val == NULL)
    {

      if (npts > 0)
	{			/* the "start notification" */
	  nvox = npts;		/* keep track of   */
	  ncall = 0;		/* number of calls */
	}
      else
	{			/* the "end notification" */

	  /* nothing to do here */
	}
      EXRETURN;
    }

  ncall++;

  if (automask)
     npts = npts - 1;

  tempptr = I0_ptr+ncall-1;
  I0 = *tempptr;

#if 0
  /* check for reasons not to calculate anything and just return zeros */
if(ncall==1)
   printf("checking for all zeros\n");
  allzeros = 0;
  i=0;

  while ((i<6)&&(I0!=0.0))        /* check if all the DT values are 0 */
  { 
     if(ts[i++]!=0)
        allzeros = 0;
  }


  for(i=0;i<nbriks;i++)
    val[i] = 0.0;
  EXRETURN;


/* return zeros if all the DT values are 0, if the I0 value is 0 or 
   the mask is off at this voxel*/
  if(allzeros||(I0==0.0) || (automask&&(ts[npts] == 0)))
    {
	  for (i = 0; i < nbriks; i++)
	    val[i] = 0.0;	/* return 0 for all DWIn points */
	  EXRETURN;
    }

#endif

  val[0] = I0; /* the first sub-brik is the I0 sub-brik */
  bptr = bmatrix+6;   /* start at the first gradient */

  for(i=1;i<nbriks;i++) {
     bptr = bmatrix+(6*i);   /* start at the first gradient */
     bq_d = *bptr++ * ts[0];          /* GxGxDxx  */
     bq_d += *bptr++ * ts[1] * 2;      /* 2GxGyDxy */
     bq_d += *bptr++ * ts[2] * 2;      /* 2GxGzDxz */
     bq_d += *bptr++ * ts[3];          /* GyGyDyy  */
     bq_d += *bptr++ * ts[4] * 2;      /* 2GyGzDyz */
     bq_d += *bptr++ * ts[5];          /* GzGzDzz  */

     val[i] = I0 * exp(-bq_d);   /* for each gradient,q, Iq = J e -(bq.D) */
                                 /* where bq.D is the large dot product of bq and D */
  }

  EXRETURN;
}


/*! compute the diffusion weighting matrix bmatrix for q number of gradients */
/* only need to calculate once */
/* bq = diffusion weighting matrix of qth gradient */
/*      GxGx GxGy GxGz
        GxGy GyGy GyGz
        GxGz GyGz GzGz */
/* b0 is 0 for all 9 elements */
/* bmatrix is really stored as 6 x npts array */
static void
Computebmatrix (MRI_IMAGE * grad1Dptr)
{
  int i, j, n;
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
      for(j=0;j<6;j++)
         printf("%-13.6g ", *(bmatrix + 6 + (i*6)+ j) );
      printf("\n");
    }


  EXRETURN;
}

/*! allocate all the global matrices and arrays once */
static void
InitGlobals (int npts)
{
  int i;
  double *cumulativewtptr;

  ENTRY ("InitGlobals");

  bmatrix = malloc (npts * 6 * sizeof (double));

  EXRETURN;
}

/*! free up all the matrices and arrays */
static void
FreeGlobals ()
{

  ENTRY ("FreeGlobals");
  free (bmatrix);
  bmatrix = NULL;
  EXRETURN;
}
