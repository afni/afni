/*********************** 3dDTtoDWI.c **********************************************/
/* Author: Daniel Glen, 17 Nov 2004 */
/* compute multiple gradient images from 6 principle direction tensors*/
/* and corresponding gradient vector coordinates */
/* used for modeling diffusion tensor data */

// [May, 2017] input option to complement 3dDWItoDT's -scale_out_1000
// option.  It has the same name, funnily enough!

// [May, 2017] 
// + bug fix: had been a mismatch between bmatrix and DT.  Now fixed.

#include "thd_shear3d.h"
# include "matrix.h"
#include "afni.h"

#define TINYNUMBER 1E-10
#define SMALLNUMBER 1E-4

static char prefix[THD_MAX_PREFIX] = "DWI";
static int datum = MRI_float;

static double *bmatrix;		/* b matrix = GiGj for gradient intensities */
static int BMAT_NZ = 0;    /* non-zero bmatrix supplied: apr,2016: not
                              doing anything here at the moment, just
                              in case we want to start using bmatrices
                              as input instead of grads, such as in
                              3dDWItoDT. */
static float MAX_BVAL = 1.;
static double *B0list = NULL;

static float *I0_ptr;           /* I0 matrix */
static int automask = 0;        /* automasking flag - user option */

static void DTtoDWI_tsfunc (double tzero, double tdelta, int npts, float ts[],
                            double ts_mean, double ts_slope, void *ud, 
                            int nbriks, float *val);
// apr,2016: copying/pasting this function now from 3dDWItoDT
static void Computebmatrix (MRI_IMAGE * grad1Dptr, int NO_ZERO_ROW1); 
static void InitGlobals (int npts);
static void FreeGlobals (void);

int
main (int argc, char *argv[])
{
   THD_3dim_dataset *old_dset, *new_dset, *I0_dset;	/* input and output datasets */
   int nopt, nbriks, nvox;
   int i;
   MRI_IMAGE *grad1Dptr = NULL;
   MRI_IMAGE *anat_im = NULL;
   MRI_IMAGE *data_im = NULL;
   double fac;
   short *sar = NULL, *tempsptr = NULL, tempval;
   byte *maskptr = NULL, *tempbptr = NULL;
   char tempstr[25];

   float SCALE_VAL_OUT = -1.0 ;    // allow users to scaled physical
   // values by 1000 easily; will be
   // set to 1 if negative after
   // reading in inputs

   /*----- Read command line -----*/
   if (argc < 2 || strcmp (argv[1], "-help") == 0) {
      printf (
"Usage: 3dDTtoDWI [options] gradient-file I0-dataset DT-dataset\n"
"\n"
"Computes multiple gradient images from 6 principle direction tensors and\n"
"    corresponding gradient vector coordinates applied to the I0-dataset.\n"
" The program takes three parameters as input :  \n"
"    a 1D file of the gradient vectors with lines of ASCII floats Gxi,Gyi,Gzi.\n"
"    Only the non-zero gradient vectors are included in this file (no G0 line).\n"
" The I0 dataset is a volume without any gradient applied.\n"
" The DT dataset is the 6-sub-brick dataset containing the diffusion tensor data,\n"
"    Dxx, Dxy, Dyy, Dxz, Dyz, Dzz (lower triangular row-wise order)\n"
"\n"
" Options:\n"
"   -prefix pname    = Use 'pname' for the output dataset prefix name.\n"
"                      [default='DWI']\n"
"   -automask        = mask dataset so that the gradient images\n"
"                      are computed only for high-intensity (presumably\n"
"                      brain) voxels.  The intensity level is determined\n"
"                      the same way that 3dClipLevel works.\n\n"
"   -datum type      = output dataset type [float/short/byte] \n"
"                      (default is float).\n"
"   -help            = show this help screen.\n"
"   -scale_out_1000  = matches with 3dDWItoDT's '-scale_out_1000'\n"
"                      functionality.  If the option was used\n"
"                      there, then use it here, too.\n"
"\n"
" Example:\n"
"\n"
"    3dDTtoDWI -prefix DWI -automask tensor25.1D 'DT+orig[26]' DT+orig.\n\n"
"\n"
" The output is a n sub-brick bucket dataset containing computed DWI images.\n"
"    where n is the number of vectors in the gradient file + 1\n"
"\n"
              );
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
               if (++nopt >= argc) {
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
               if (++nopt >= argc) {
                  fprintf (stderr, "*** Error - datum needs an argument!\n");
                  exit (1);
               }
               if (strcmp (argv[nopt], "short") == 0) {
                  datum = MRI_short;
               }
               else if (strcmp (argv[nopt], "float") == 0) {
                  datum = MRI_float;
               }
               else if (strcmp (argv[nopt], "byte") == 0) {
                  datum = MRI_byte;
               }
               else {
                  fprintf (stderr, "-datum of type '%s' is not supported!\n",
                           argv[nopt]);
                  exit (1);
               }
               nopt++;
               continue;
            }
         if (strcmp (argv[nopt], "-automask") == 0) {
            automask = 1;
            nopt++;
            continue;
         }
      
         //  can mult output diff values by 1000 
         if (strcmp(argv[nopt], "-scale_out_1000") == 0) {   
            SCALE_VAL_OUT = 1000.;
            nopt++;
            continue;
         }

         fprintf(stderr, "*** Error - unknown option %s\n", argv[nopt]);
         exit(1);
      }
  
   // ==============================================================
   // ====================== begin processing ======================
  
   /*----- read input datasets -----*/

   if (nopt >= argc) {
      fprintf (stderr, "*** Error - No input dataset!?\n");
      exit (1);
   }
   
   if( SCALE_VAL_OUT < 0 )
      SCALE_VAL_OUT = 1.;
   else
      INFO_message("Implementing user-selected scaling of diffusion "
                   "values by %.0f",
                   SCALE_VAL_OUT);
  
   /* first input dataset - should be gradient vector file of ascii
      floats Gx,Gy,Gz */

   /* read gradient vector 1D file */
   grad1Dptr = mri_read_1D (argv[nopt]);
   if (grad1Dptr == NULL)
      {
         fprintf (stderr, "*** Error reading gradient vector file\n");
         exit (1);
      }

   if (grad1Dptr->ny != 3) {
      fprintf (stderr, "*** Error - Only 3 columns of gradient vectors allowed\n");
      fprintf (stderr, "%d columns found\n", grad1Dptr->nx);
      mri_free (grad1Dptr);
      exit (1);
   }

   if (grad1Dptr->nx < 6) {
      fprintf (stderr, "*** Error - Must have at least 6 gradient vectors\n");
      fprintf (stderr, "%d columns found\n", grad1Dptr->nx);
      mri_free (grad1Dptr);
      exit (1);
   }

   nbriks = grad1Dptr->nx + 1;    /* number of gradients specified here from file */     
   nopt++;

   // [PT: May, 2017] copied from 3dDWItoDT to apply the scaling
   int npix;
   register float *flar=mri_data_pointer(grad1Dptr);
   npix = grad1Dptr->nvox; 
   for( i=0 ; i<npix ; i++ ){
      flar[i]/= SCALE_VAL_OUT;
   }

   /* open I0 dataset - idealized no gradient image */
   I0_dset = THD_open_dataset (argv[nopt]);
   CHECK_OPEN_ERROR(I0_dset,argv[nopt]) ;
 
   DSET_mallocize (I0_dset);
   DSET_load (I0_dset);	                /* load dataset */
   data_im = DSET_BRICK (I0_dset, 0);	/* set pointer to the 0th sub-brik of the dataset */
   fac = DSET_BRICK_FACTOR(I0_dset, 0); /* get scale factor for each sub-brik*/
   if(fac==0.0) fac=1.0;
   if((data_im->kind != MRI_float)) {
      fprintf (stderr, "*** Error - Can only open float datasets. Use 3dcalc to convert.\n");
      mri_free (grad1Dptr);
      mri_free (data_im);
      exit (1);
   }

   I0_ptr = mri_data_pointer(data_im) ; /* pointer to I0 data */
   if( I0_ptr == NULL ) {
      ERROR_exit("Null pointer when trying to make I0 set.");
   }
   

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
   Computebmatrix (grad1Dptr, BMAT_NZ);	/* compute bij=GiGj */
   INFO_message("The maximum magnitude of the bmatrix appears to be: %.2f", MAX_BVAL);

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
                                      NULL,   /* mask */
                                      0       /* Allow auto scaling of output */
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

/*
  [PT: May, 2017] 
  !! NB !! This has been copy/pasted *to* 3dDWItoDT.c, so if changes
           happen there/here, likely make them here/there as well.
           This inglorious route was chosen rather than deal with
           compile-time issues with static variables.  I'm sure I'll
           adjust this more properly later.  Definitely.
*/
static void
DTtoDWI_tsfunc (double tzero, double tdelta,
                int npts, float ts[],
                double ts_mean, double ts_slope,
                void *ud, int nbriks, float *val)
{
   int i;
   static int nvox2, ncall2;
   double I0, bq_d;
   double *bptr;
   float *tempptr;
 
   ENTRY ("DTtoDWI_tsfunc");
   /* ts is input vector data of 6 floating point numbers.*/
   /* ts should come from data sub-briks in form of Dxx, Dxy, Dyy, Dxz, Dyz, Dzz */
   /* if automask is turned on, ts has 7 floating point numbers */
   /* val is output vector of form DWI0, DWI1, DWIn sub-briks */
   /* where n = number of gradients = nbriks-1 */

   /** is this a "notification"? **/
   if (val == NULL)
      {

         if (npts > 0) {			/* the "start notification" */
            nvox2 = npts;		/* keep track of   */
            ncall2 = 0;		/* number of calls */
         }
         else {			/* the "end notification" */
            
            /* nothing to do here */
         }
         EXRETURN;
      }

   ncall2++;

   if (automask)
      npts = npts - 1;
   
   tempptr = I0_ptr+ncall2-1;
   I0 = *tempptr;


   val[0] = I0; /* the first sub-brik is the I0 sub-brik */
   bptr = bmatrix+6;   /* start at the first gradient */

   /* NB: what the ordering of bmatrix and DT are:
   // bmatr
   [0] *bptr++ = Gx * Gx / gscale; 
   [1] *bptr++ = Gx * Gy / gscale;
   [2] *bptr++ = Gx * Gz / gscale;
   [3] *bptr++ = Gy * Gy / gscale;
   [4] *bptr++ = Gy * Gz / gscale;
   [5] *bptr++ = Gz * Gz / gscale;

   [0] Dxx, 
   [1] Dxy, 
   [2] Dyy, 
   [3] Dxz, 
   [4] Dyz, 
   [5] Dzz 
   */

   // Need to match bmatrix and DT orderings, which are different.
   // Yay.
   for(i=1;i<nbriks;i++) {
      bptr = bmatrix+(6*i);        // start at the first gradient 
      bq_d = *bptr++ * ts[0];      // GxGxDxx  
      bq_d += *bptr++ * ts[1] * 2; // 2GxGyDxy 
      bq_d += *bptr++ * ts[3] * 2; // 2GxGzDxz 
      bq_d += *bptr++ * ts[2];     // GyGyDyy  
      bq_d += *bptr++ * ts[4] * 2; // 2GyGzDyz 
      bq_d += *bptr++ * ts[5];     // GzGzDzz  

      // for each gradient,q, Iq = J e -(bq.D) 
      val[i] = I0 * exp(-bq_d);   
      // where bq.D is the large dot product of bq and D 
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
// ----> apr,2016: now copying the Computebmatrix(...) from 3dDWItoDT,
// ----> to have the scale stuff
static void
Computebmatrix (MRI_IMAGE * grad1Dptr, int NO_ZERO_ROW1) 
{
   int i, n;
   register double *bptr;
   register float *Gxptr, *Gyptr, *Gzptr;
   register float *Bxxptr, *Byyptr, *Bzzptr, *Bxyptr, *Bxzptr, *Byzptr;
   double Gx, Gy, Gz, Bxx, Byy, Bzz, Bxy, Bxz, Byz;
   double gscale;
   int j;
   ENTRY ("Computebmatrix");
   n = grad1Dptr->nx;		/* number of gradients other than I0 */

   if ( (grad1Dptr->ny == 6)  && !NO_ZERO_ROW1 ) { // extra switch to keep OLD, zero-row version
      /* just read in b matrix */
      Bxxptr = MRI_FLOAT_PTR (grad1Dptr);	/* use simple floating point pointers to get values */
      Byyptr = Bxxptr + n;
      Bzzptr = Byyptr + n;
      Bxyptr = Bzzptr + n;
      Bxzptr = Bxyptr + n;
      Byzptr = Bxzptr + n;

      bptr = bmatrix;

      /*    B0list[0]= 1;*/  /* keep a record of which volumes have no gradient: first one always assumed */

      for (i = 0; i < n; i++){
         Bxx = *Bxxptr++;
         Byy = *Byyptr++;
         Bzz = *Bzzptr++;
         Bxy = *Bxyptr++;
         Bxz = *Bxzptr++;
         Byz = *Byzptr++;
         *bptr++ = Bxx;
         *bptr++ = Bxy;
         *bptr++ = Bxz;
         *bptr++ = Byy;
         *bptr++ = Byz;
         *bptr++ = Bzz;
         if(Bxx==0.0 && Byy==0.0 && Bzz==0.0)  /* is this a zero gradient volume also? */
            B0list[i] = 1;
         else{
            B0list[i] = 0;
            // apr,2016: need the MAX_BVAL for scaling
            gscale = Bxx + Byy + Bzz;
            if(gscale > MAX_BVAL)
               MAX_BVAL = gscale;
         }
      
      }
   } 
   else if( (grad1Dptr->ny == 6)  && NO_ZERO_ROW1 ) { //  very similar to old bmatrix option
      /* just read in b matrix */
      Bxxptr = MRI_FLOAT_PTR (grad1Dptr);	/* use simple floating point pointers to get values */
      Byyptr = Bxxptr + n;
      Bzzptr = Byyptr + n;
      Bxyptr = Bzzptr + n;
      Bxzptr = Bxyptr + n;
      Byzptr = Bxzptr + n;

      bptr = bmatrix;
    
      // do as grads below
      for (i = 0; i < 6; i++)
         *bptr++ = 0.0;		/* initialize first 6 elements to 0.0 for the I0 gradient */
      B0list[0]= 1;      /* keep a record of which volumes have no gradient */


      for (i = 0; i < n; i++){ 
         Bxx = *Bxxptr++;
         Byy = *Byyptr++;
         Bzz = *Bzzptr++;
         Bxy = *Bxyptr++;
         Bxz = *Bxzptr++;
         Byz = *Byzptr++;
         *bptr++ = Bxx;
         *bptr++ = Bxy;
         *bptr++ = Bxz;
         *bptr++ = Byy;
         *bptr++ = Byz;
         *bptr++ = Bzz;
         if(Bxx==0.0 && Byy==0.0 && Bzz==0.0)  /* is this a zero gradient volume also? */
            B0list[i+1] = 1; 
         else{
            B0list[i+1] = 0; 
            // apr,2016: need the MAX_BVAL for scaling
            gscale = Bxx + Byy + Bzz;
            if(gscale > MAX_BVAL)
               MAX_BVAL = gscale;
         }

      }
   }
   else {
      Gxptr = MRI_FLOAT_PTR (grad1Dptr);	/* use simple floating point pointers to get values */
      Gyptr = Gxptr + n;
      Gzptr = Gyptr + n;

      bptr = bmatrix;
      for (i = 0; i < 6; i++)
         *bptr++ = 0.0;		/* initialize first 6 elements to 0.0 for the I0 gradient */

      B0list[0]= 1;      /* keep a record of which volumes have no gradient */

      for (i = 0; i < n; i++)
         {
            gscale = 1.;  // apr,2016: allow for non-unit gradient magnitudes
            Gx = *Gxptr++;
            Gy = *Gyptr++;
            Gz = *Gzptr++;
            if((Gx==0.0) && (Gy==0.0) && (Gz==0.0))
               B0list[i+1] = 1;   /* no gradient applied*/
            else{
               B0list[i+1] = 0;
               gscale = sqrt(Gx*Gx + Gy*Gy + Gz*Gz);
               if(gscale > MAX_BVAL)
                  MAX_BVAL = gscale; // apr,2016
            }

            *bptr++ = Gx * Gx / gscale; // apr,2016: allow for non-unit gradient magnitudes
            *bptr++ = Gx * Gy / gscale;
            *bptr++ = Gx * Gz / gscale;
            *bptr++ = Gy * Gy / gscale;
            *bptr++ = Gy * Gz / gscale;
            *bptr++ = Gz * Gz / gscale;

            for(j=0;j<6;j++)
               printf(" %15.5f ", *(bmatrix + 6 + (i*6)+ j) );
            printf("\n");
         }
   }
   EXRETURN;
}
/* --->apr,2016: This is the **original** function for this program-- now using 
   the one from 3dDWItoDT, for each of duplicity.  Or something. 
   {
   int i, j, n;
   register double *bptr;
   register float *Gxptr, *Gyptr, *Gzptr;
   double Gx, Gy, Gz;

   ENTRY ("Computebmatrix");
   n = grad1Dptr->nx;		// number of gradients other than I0 
   Gxptr = MRI_FLOAT_PTR (grad1Dptr);	// use simple floating point pointers to get values 
   Gyptr = Gxptr + n;
   Gzptr = Gyptr + n;

   bptr = bmatrix;
   for (i = 0; i < 6; i++)
   *bptr++ = 0.0;		// initialize first 6 elements to 0.0 for the I0 gradient 

   for (i = 0; i < n; i++)
   {
   Gx = *Gxptr++;
   Gy = *Gyptr++;
   Gz = *Gzptr++;
   *bptr++ = Gx * Gx;
   *bptr++ = Gx * Gy;
   *bptr++ = Gy * Gy;
   *bptr++ = Gx * Gz;
   *bptr++ = Gy * Gz;
   *bptr++ = Gz * Gz;
   for(j=0;j<6;j++)
   printf("%-13.6g ", *(bmatrix + 6 + (i*6)+ j) );
   printf("\n");
   }


   EXRETURN;
   }*/

/*! allocate all the global matrices and arrays once */
static void
InitGlobals (int npts)
{

   ENTRY ("InitGlobals");

   bmatrix = malloc (npts * 6 * sizeof (double));
   B0list = malloc (npts * sizeof (double));

   EXRETURN;
}

/*! free up all the matrices and arrays */
static void
FreeGlobals ()
{

   ENTRY ("FreeGlobals");
   free (bmatrix);
   bmatrix = NULL;

   free(B0list);
   B0list = NULL;


   EXRETURN;
}
