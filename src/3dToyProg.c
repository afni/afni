#include "mrilib.h"
#include "matrix.h"


THD_3dim_dataset * New_Dataset_From_Scratch(char *prefix)
{
   THD_3dim_dataset *oset=NULL;
   int nvals, ii, jj, kk, nijk, tt;
   float *ts=NULL;
   char *stmp;

   ENTRY("New_Dataset_From_Scratch");  /* The use of ENTRY and RETURN
                                          will allow for the display of
                                          the function call stack on exit */

   /*
      Create a dataset that has, say, the following properties

      Data stored in RAI directions:
         1st dimension (i) is from Right To Left
         2nd dimension (j) is from Anterior to Posterior
         3rd dimension (k, slice) is from Inferior to Superior
      Number of voxels along each dimension is:
         Ni = 32
         Nj = 32
         Nk = 10;
      Voxel size in mm along each dimension:
         Di = 6.0;
         Dj = 6.0;
         Dk = 5.2;
      Voxel origin in mm DICOM RAI
         Xorg = -12.7;         (Right == -ve X, Left == +ve X)
         Yorg = 2.3;           (Anterior == -ve Y, Posterior == +ve Y)
         Zorg = -4.5;          (Inferior == -ve Z, Superior == +ve Z)

      We can create this dataset grid with the following command:
      EDIT_geometry_constructor("RAI:D:Ni,Xorg,Di,Nj,Yorg,Dj,Nk,Zorg,Dk", PREFIX)
   */

   stmp = modify_afni_prefix(prefix,NULL,".scratch");
   if (!(oset = EDIT_geometry_constructor(
            "RAI:D:32,-12.7,6.0,   32,2.3,6.0,    10,-4.5,5.2",
            stmp ))) {
      ERROR_message("Failed to create dset %s\n",stmp);
      RETURN(oset);
   }

   /* You can create a dataset with non-cardinal orientation (oblique)
      by directly specifying the 3x4 matrix (A) that converts voxel index (i,j,k)
      to  millimeter RAI coordinates (x,y,z) with:
      oset = EDIT_geometry_constructor(
             "MATRIX(a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34):nx,ny,nz",
             "toy_from_grid" );

      See Dataset_Navigation() for coordinate manipulation
   */

   /* Now let us fill up this dataset with time series, and say we want the
      results to be stored in MRI_short.
      I use short because in thie example, one is forced to consider what
      the scaling factor, if any should be before inserting the time series
      into the volume.
      Here I know that my values are between -1.0 and 1.0 so a factor of
         MRI_TYPE_maxval[MRI_short] is just fine for all nvals sub-bricks
   */

   /* Get the dataset ready to receive 100 values per voxel, and say we have
      a time axis sampling frequency of 0.5 Hz, TR = 2sec */
   nvals = 100;
   EDIT_dset_items( oset ,
                    ADN_datum_all , MRI_short , /* type */
                    ADN_nvals     , nvals ,     /* Number of values */
                    ADN_ntt       , nvals ,
                    ADN_ttdel     , 2.0 ,         /* TR */
                    ADN_tunits    , UNITS_SEC_TYPE,
                    ADN_none ) ;                /* Always last param */

   /* Allocate space for the storage of each sub-brick and setup
      brick factors */
   for (tt=0; tt<nvals; ++tt) {
            /* By passing NULL below, the function will allocate
               space for one sub-brick and return */
      EDIT_substitute_brick(oset , tt , MRI_short , NULL ) ;
            /* I don't expect much above 10 */
      EDIT_BRICK_FACTOR(oset, tt, 10.0/MRI_TYPE_maxval[MRI_short]);
   }
   /* Now let us put in the time series */
   ts = (float *)calloc(nvals, sizeof(float));
   nijk=0;
   for (kk=0; kk<DSET_NZ(oset); ++ kk) {
   for (jj=0; jj<DSET_NY(oset); ++ jj) {
   for (ii=0; ii<DSET_NX(oset); ++ ii) {
      for (tt=0; tt<nvals; ++tt) {
         ts[tt] = (sin(tt*(kk+1.0)/100.0) +
                   (1+(float)kk/DSET_NZ(oset))*drand48());
      }
      /* insert the time series */
      THD_insert_series(nijk, oset, nvals,
                        MRI_float,        /* indicates the type of ts */
                        ts,               /* The array to be inserted */
                        0);               /* Values in ts should be scaled
                                             before conversion to volume storage
                                             type. This would have been 1 if
                                             oset was a volume of floats. */
      ++nijk;
   } } }

   free(ts); ts = NULL;

   RETURN(oset);
}


THD_3dim_dataset * Volumewise_Operations(THD_3dim_dataset *dset, char *prefix,
                                         int datum)
{
   int iv6[] = { 17, 0, 13, 4, 6, 8 };
   float fv = 3.14159;
   THD_3dim_dataset *oset = NULL;
   int nvox, isb, ivx;
   float *sb=NULL, *vout=NULL, *yout=NULL;
   char *stmp;

   ENTRY("Volumewise_Operations");

   DSET_load(dset);
   nvox = DSET_NVOX(dset);

   vout = (float *)calloc(nvox, sizeof(float)); /* Allocate for one whole volume,
                                                   one sub-brick that is */
   yout = (float *)calloc(nvox, sizeof(float)); /* and another */
   for (isb=0; isb<DSET_NVALS(dset); ++isb) {
      if (!(sb = THD_extract_to_float(isb, dset))) {
         ERROR_message("Failed to extract sub-brick %d from %s\n",
                       isb, DSET_PREFIX(dset));
         RETURN(NULL);
      }
      for (ivx=0; ivx<nvox; ++ivx) {
         vout[ivx] += sb[ivx];
         yout[ivx] += 1./(sqrt(fabs(sb[ivx])+0.1));
      }
      free(sb); sb = NULL;
   }

   /* Put the results in a new dataset */
   oset = EDIT_empty_copy( dset ) ;
   if (datum == MRI_float) { /* store results as float */
      stmp = modify_afni_prefix(prefix,NULL,".volops.float");
      EDIT_dset_items( oset ,
                      ADN_prefix , stmp,
                      ADN_datum_all, MRI_float ,
                      ADN_nvals  , 2 ,
                      ADN_ntt, 0,
                    ADN_none ) ;

      EDIT_substitute_brick( oset , 0 , MRI_float  , vout  ) ;
         vout = NULL; /* do not free vout, oset uses the same pointer
                        because column types is oset are float */
      EDIT_substitute_brick( oset , 1 , MRI_float  , yout ) ; yout = NULL;
   } else { /* store results as short */
      stmp = modify_afni_prefix(prefix,NULL,".volops.short");
      EDIT_dset_items( oset ,
                      ADN_prefix , stmp,
                      ADN_datum_all, MRI_short ,
                      ADN_nvals  , 2 ,
                    ADN_none ) ;
      /* Store the results using short precision and auto scale */
      EDIT_substscale_brick(oset, 0, MRI_float, vout, MRI_short, -1.0);
         free(vout); vout = NULL; /* vout got copied by value, so free it */

      /* Store the results using short precision and set your own scale
         (here out of laziness I used the same scale as for sub-brick 0) */
      EDIT_substscale_brick(oset, 1, MRI_float, yout,
                            MRI_short, DSET_BRICK_FACTOR(oset,0));
         free(yout); yout = NULL; /* vout got copied by value, so free it */
   }

   /* Label the sub-bricks */
   EDIT_BRICK_LABEL(oset , 0, "sum");
   EDIT_BRICK_LABEL(oset , 1, "sisqrt");

   /* Add your own special attribute -- Note that private attributes
      are not preserved when making copies of datasets               */
   THD_set_string_atr( oset->dblk , "Pinot_Noir" , "sideways" ) ;
   THD_set_int_atr  ( oset->dblk , "Lottery_Numbers", 6, iv6 );
   THD_set_float_atr( oset->dblk , "PI", 1, &fv );

   RETURN(oset);
}

void Dataset_Navigation(THD_3dim_dataset *dset)
{
   int tt, ni, nj, nij, nijk, i, j, k;
   float X[3], I[3], *far=NULL;
   float A[4][4], Ai[4][4];

   ENTRY("Dataset_Navigation");

   /* The most general, though not necessarily most efficient, way to go
      from voxel (i,j,k) to (x,y,z) is by using matrix A:               */

                     /* Load matrix into augmented 4x4 matrix A */
   MAT44_TO_AFF44(A, dset->daxes->ijk_to_dicom_real);

                     /* Load i,j,k into an array. For this toy case,
                        pick middle voxel in volume to be sure we
                        have something for any input dataset. Otherwise
                        it is possible to fall outside the grid. */
   I[0] = DSET_NX(dset)/2; I[1] = DSET_NY(dset)/2; I[2] = DSET_NZ(dset)/2;

                     /* X = A I */
   AFF44_MULT_I(X, A, I );
   INFO_message("\nVoxel %d %d %d is at %.3f %.3f %.3f mm RAI\n",
                (int)I[0], (int)I[1], (int)I[2], X[0], X[1], X[2]);

   /* To go from X to I */
                     /* Compute the inverse of A */
   AFF44_INV( Ai, A );
                     /* I = Ai X */
   AFF44_MULT_I(I, Ai, X );
   INFO_message("Location %.3f %.3f %.3f mm RAI is at voxel %.3f %.3f %.3f\n",
                X[0], X[1], X[2], I[0], I[1], I[2]);

   /* To go from voxel 1D index (nijk) (entire volume in one array),
      to 3D index (i,j,k)                                         */
   ni = DSET_NX(dset);
   nij = DSET_NX(dset)*DSET_NY(dset);

   i = DSET_NX(dset)/2; j = DSET_NY(dset)/2; k = DSET_NZ(dset)/2;
   nijk = AFNI_3D_to_1D_index(i, j, k, ni, nij);
   INFO_message("3D indices %d %d %d correspond to 1D index %d\n",
                i, j, k, nijk);
   /* To go from voxel 1D index (nijk) (entire volume in one array),
      to 3D index (i,j,k) */
   AFNI_1D_to_3D_index(nijk, i, j, k, ni, nij);

   /* Now let's grab the data at voxel nijk */
   far = (float *)calloc(DSET_NVALS(dset), sizeof(float));
   if (THD_extract_float_array( nijk, dset, far ) == -1) {
      ERROR_message("Failed to extract data at voxel %d\n", nijk);
   } else {
      INFO_message(
         "\nHave %d values from voxel %d (%d %d %d) or (%.2f %.2f %.2f mm)\n",
                     DSET_NVALS(dset), nijk, i, j, k, X[0], X[1], X[2]);
      for (tt=0; tt<DSET_NVALS(dset); ++tt) {
         if (tt<5 || tt>(DSET_NVALS(dset)-10))
            fprintf(stdout,"%.3f    ", far[tt]);
         else if (tt == 5) fprintf(stdout," ...    ");
      }
      fprintf(stdout,"\n\n");
      free(far); far = NULL;
   }

   EXRETURN;
}



typedef struct {
   int what;
   float ever;
   char *you;
   THD_3dim_dataset *like;
   int nfft;
} TOY_UD;

static void toy_tsfunc( double tzero, double tdelta ,
                        int npts, float ts[],
                        double ts_mean , double ts_slope ,
                        void *ud, int nbriks, float *val          )
{
   static complex *comp_array=NULL;
   float mag=0, pow=0, pow6=0;
   int  jj;
   TOY_UD *rpud = (TOY_UD *)ud;

   if( val == NULL ){
      INFO_message("toy_tsfunc: %s notification call, npts=%d\n",
                   npts?"Start":"End", npts);
      if( npts > 0 ){  /* the "start notification" */
         /* This is when you perform any setup you don't want to repeat
            each time the function is called for a new voxel.
            Such a setup typically involves allocating for temporary
            arrays */

         /* allocate for fft array */
         comp_array = (complex *) calloc( sizeof(complex) , rpud->nfft);
      } else {  /* the "end notification" */
         /* Last call, meant for cleanup */
         if (comp_array) free(comp_array); comp_array = NULL;
      }
      return ;
   }

   /* if we get here then we have data to process */

   /* Load time series */
   for( jj=0 ; jj < npts ; jj++ ) {
      comp_array[jj].r = ts[jj]; comp_array[jj].i = 0.0f ;
   }
   /* zero pad */
   for( jj=npts ; jj < rpud->nfft ; jj++ )
       comp_array[jj].r = comp_array[jj].i = 0.0f ;

   csfft_cox( -1 , rpud->nfft, comp_array ) ;   /* DFT */

   for( jj=0 ; jj < rpud->nfft ; jj++ ) {
      mag = CABS(comp_array[jj]) ;
      pow += mag*mag;
      if (jj<rpud->nfft/6) pow6 += mag*mag ;
   }

   val[0] =  pow;
   val[1] =  pow6;

   return ;
}

THD_3dim_dataset * Voxelwise_Operations(THD_3dim_dataset *dset,
                                        byte *voxmask, char *prefix)
{
   TOY_UD ud;
   char *stmp;
   THD_3dim_dataset *oset=NULL;

   ENTRY("Voxelwise_Operations");

   /* Here we will perform an fft on each voxel's time series      */
   /* and compute the power in the  first sixth of the spectrum,   */
   /* and the total power; two values per voxel in total to output */

   /* Setup user data structure */
   ud.nfft = 128;

   stmp = modify_afni_prefix(prefix,NULL,".pow"); /* modify prefix */
   /* go for it */
   oset = MAKER_4D_to_typed_fbuc(
            dset ,             /* input dataset */
            stmp ,             /* output prefix */
            MRI_short ,        /* output datum  */
            0 ,                /* samples to skip from beginning of time series*/
            1 ,                /* pass linearly detrended time series */
            2 ,                /* number of values expected in output */
            toy_tsfunc,        /* timeseries processor */
            (void *)(&ud),     /* user data for tsfunc */
            voxmask,           /* byte mask of voxels to process */
            0                  /* Allow auto scaling of output */
                                 ) ;
   EDIT_BRICK_LABEL(oset , 0, "FFTpow6");
   EDIT_BRICK_LABEL(oset , 1, "FFTpow");

   RETURN(oset);
}

/* If you follow a comparable syntax in your help,
   Then you can use AFNI's automatic option completion
   for your program. */
int help_3dToyProg(TFORM targ, int detail)
{
   if (detail >= 0) {
     sphinx_printf(targ,
"Usage: 3dToyProg [-prefix PREF] [-mask MSET] [-datum DATUM] \n"
"                 [-h|-help] <-input ISET>\n"
"   A program to illustrate dataset creation, and manipulation in C using\n"
"   AFNI's API. Comments in the code (should) explain it all.\n"
"\n"
      );
   }
   if (detail >= 1) {
      sphinx_printf(targ,
" -input ISET: reference dataset \n"
" -prefix PREF: Prefix of output datasets. \n"
" -mask MSET: Restrict analysis to non-zero voxels in MSET\n"
" -datum DATUM: Output datum type for one of the datasets.\n"
"               Choose from 'float' or 'short'. Default is\n"
"               'float'\n"
"%s\n"
"\n"
":SPX:\n"
"  .. figure:: :=AFACE:/face_chen_gang.jpg\n\n"
":SPX:\n",
   SUMA_Offset_SLines(get_help_help(),1)) ;
      PRINT_COMPILE_DATE ;
   }

   return(0);
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset  *mask_dset=NULL, *iset=NULL,
                     *sset=NULL, *xset=NULL, *vset=NULL;
   char *prefix="toy";
   int iarg=1 , mcount, udatum = MRI_float;
   byte *maskvox=NULL;

   mainENTRY("3dToyProg main"); machdep(); AFNI_logger("3dToyProg",argc,argv);

#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif

   /*-- options --*/
   set_obliquity_report(0);   /* silence obliquity */

   while( iarg < argc && argv[iarg][0] == '-' ){
      CHECK_HELP(argv[iarg], help_3dToyProg);

      if( strncmp(argv[iarg],"-mask",5) == 0 ){
         if (iarg >= argc) ERROR_exit("Need dset after -mask");
         mask_dset = THD_open_dataset( argv[++iarg] ) ;
         if( mask_dset == NULL )
           ERROR_exit("Cannot open mask dataset!\n") ;
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex )
           ERROR_exit("Cannot deal with complex-valued mask dataset!\n");
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-input") == 0) {
         if (iarg >= argc) ERROR_exit("Need dset after -mask");
         if (!(iset = THD_open_dataset( argv[++iarg]))) {
            ERROR_exit("Cannot open input dataset %s!\n", argv[iarg]) ;
         }
         DSET_mallocize(iset); DSET_load(iset);  /* load data part of dataset */
         iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-prefix",6) == 0) {
         if (iarg >= argc) ERROR_exit("Need name after -prefix");
         prefix = argv[++iarg];
         iarg++ ; continue ;
         continue ;
      }

      if( strcmp(argv[iarg],"-datum") == 0) {
         if (iarg >= argc) ERROR_exit("Need datum type after -datum");
         ++iarg;
         if (!strcmp(argv[iarg],"float")) udatum = MRI_float;
         else if (!strcmp(argv[iarg],"short")) udatum = MRI_short;
         else {
            ERROR_exit(
               "For the purpose of this demo, only float and short are allowed");
         }
         iarg++ ; continue ;
         continue ;
      }

      ERROR_message("ILLEGAL option: %s\n",argv[iarg]) ;
		suggest_best_prog_option(argv[0], argv[iarg]);
      exit(1);
   }

   if( argc < 2 ){
     help_3dToyProg(TXT, 0);
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   if( !iset )
     ERROR_exit("No dataset on command line!?") ;

   if (mask_dset) {
      if (THD_dataset_mismatch(mask_dset, iset))
         ERROR_exit("grid mismatch between input dset and mask dset");
      maskvox = THD_makemask( mask_dset , 0 , 1.0, -1.0 ) ;
      mcount = THD_countmask( DSET_NVOX(mask_dset) , maskvox ) ;
      if( mcount <= 0 )  ERROR_exit("No voxels in the mask!\n") ;

      INFO_message("%d voxels in the mask dset %s\n",
                   mcount, DSET_PREFIX(mask_dset)) ;
      DSET_delete(mask_dset) ; mask_dset=NULL; /* Done with the mask dset */
   }

   /* An illustration of how volume navigation works */
   Dataset_Navigation(iset);

   /* Let us create a dataset from scratch */
   sset = New_Dataset_From_Scratch(prefix);
        /* Now for the output, add history, check for overwrite and write away */
   tross_Copy_History( iset , sset );/* Copy the old history (not mandatory). */
   tross_Make_History("3dToyProg", argc, argv ,sset) ; /* add the new */
   if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(sset)) ) {
      ERROR_message(
         "Output %s already exists, use -overwrite to do you know what",
         DSET_HEADNAME(sset));
   } else DSET_write(sset);

   /* Now we'll do some voxelwise computations */
   xset = Voxelwise_Operations(sset, maskvox, prefix);
   tross_Copy_History( iset , xset ) ; /* Copy the old */
   tross_Make_History("3dToyProg", argc, argv ,xset) ; /* add the new */
   if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(xset)) ) {
      ERROR_message(
         "Output %s already exists, use -overwrite to do you know what",
         DSET_HEADNAME(xset));
   } else DSET_write(xset);

   /* Or some volumewise operations */
   vset = Volumewise_Operations(sset, prefix, udatum);
   tross_Copy_History( iset , vset ) ; /* Copy the old */
   tross_Make_History("3dToyProg", argc, argv ,vset) ; /* add the new */
   if( !THD_ok_overwrite() && THD_is_ondisk(DSET_HEADNAME(vset)) ) {
      ERROR_message(
         "Output %s already exists, use -overwrite to do you know what",
         DSET_HEADNAME(vset));
   } else DSET_write(vset);




   /* cleanup */
   DSET_delete(xset); xset = NULL;
   DSET_delete(vset); vset = NULL;
   DSET_delete(sset); sset = NULL;
   exit(0) ;
}
