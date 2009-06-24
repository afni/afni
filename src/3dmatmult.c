/* ----------------------------------------------------------------------
 * Multiply AFNI datasets where each slice is considered to be a matrix.
 *
 * Multiply an Ra x Ca matrix by an Rb x Cb matrix (where Ca must equal Rb)
 * to obtain an Ra x Cb matrix.
 *
 * Requested by Wolfgang Gaggl.
 *
 * Author: R Reynolds  29 Sep 2008
 */

static char * g_history[] =
{
  "----------------------------------------------------------------------\n"
  "history (of 3dmatmult):\n"
  "\n",
  "0.0  29 Sep 2008\n"
  "     (Rick Reynolds of the National Institutes of Health, SSCC/DIRP/NIMH)\n"
  "     -initial version\n"
};

static char g_version[] = "3dmatmult version 0.0, 29 September 2008";

#include "mrilib.h"

/*--------------- global options struct ---------------*/
typedef struct
{
   THD_3dim_dataset * dsetA, * dsetB;
   char             * prefix;
   int                verb;
   int                datum;
   /* add transpose option(s)? */
} options_t;

options_t g_opts;

/*--------------- prototypes ---------------*/
int copy_slice_as_float (float *, THD_3dim_dataset *, int, int);
int multiply_dsets      (options_t *, THD_3dim_dataset **);
int process_opts        (options_t *, int, char *[]);
int show_help           (void);
int write_result        (options_t *, THD_3dim_dataset *, int, char *[]);

/*--------------- main routine ---------------*/
int main( int argc, char *argv[] )
{
   THD_3dim_dataset * outset;
   options_t        * opts = &g_opts;
   int                rv;

   if( argc < 2 ) { show_help();  return 0; }

   mainENTRY("3dmatmult main"); machdep(); AFNI_logger("3dmatmult",argc,argv);

   /* only a negative return is considered failure */
   if( (rv = process_opts(opts, argc, argv)) != 0 ) RETURN(rv < 0);

   if( multiply_dsets(opts, &outset) ) RETURN(1);

   if( write_result(opts, outset, argc, argv) ) RETURN(1);

   RETURN(0);
}


/*--------------- multiply the datasets ---------------*/
int multiply_dsets(options_t * opts, THD_3dim_dataset ** oset)
{
   THD_3dim_dataset * outset = NULL;  /* assign to oset at end */
   THD_ivec3          nxyz;
   double             sum;
   float            * volout, * sliceA, * sliceB, * sliceC;
   int                nvals, onesub;
   int                nxa, nya, nza, nxya;
   int                nxb, nyb, nzb, nxyb;
   int                nxc, nyc, nzc, nxyc;
   int                sub, asub, sind, row, col, ind;

   ENTRY("multiply_dsets");

   nvals = DSET_NVALS(opts->dsetB);  /* second dataset defines #sub-bricks   */
   onesub = (DSET_NVALS(opts->dsetA) == 1); /* does A have only 1 sub-brick? */

   /*--------------- check for consistency ---------------*/

   nxa = DSET_NX(opts->dsetA);  nxb = DSET_NX(opts->dsetB);
   nya = DSET_NY(opts->dsetA);  nyb = DSET_NY(opts->dsetB);
   nza = DSET_NZ(opts->dsetA);  nzb = DSET_NZ(opts->dsetB);

   if( nxa != nyb ) {
      fprintf(stderr,"** matrices do not match, %d cols != %d rows\n",nxa,nyb);
      RETURN(1);
   }

   if( nza != nzb ) {
      fprintf(stderr,"** input dataset have different number of slices\n");
      RETURN(1);
   }

   if( nvals != DSET_NVALS(opts->dsetA) && ! onesub ) {
      fprintf(stderr,"** dataset have different numbers of sub-bricks\n");
      RETURN(1);
   }

   if( opts->verb > 0 ) 
      fprintf(stderr,"++ creating %d brick(s) of %d (slices) %dx%d matrices\n",
              nvals,nza,nya,nxb);

   /*--------------- create initial output dataset ---------------*/

   outset = EDIT_empty_copy( opts->dsetB );
   nxc = nxb;  nyc = nya;  nzc = nza;
   nxyz.ijk[0] = nxc;  nxyz.ijk[1] = nyc;  nxyz.ijk[2] = nzc;
   EDIT_dset_items( outset,
                      ADN_prefix, opts->prefix,
                      ADN_nxyz,   nxyz,
                      ADN_nvals,  nvals,
                      ADN_ntt,    HAS_TIMEAXIS(opts->dsetB) ? nvals : 0,
                    ADN_none );

   if( THD_is_ondisk(DSET_BRIKNAME(outset)) ) {
      fprintf(stderr,"** won't overwrite existing dataset '%s'\n",
              DSET_BRIKNAME(outset));
      RETURN(1);
   }

   /*--------------- allocate processing memory ---------------*/

   /* create sub-brick arrays (filled with zero) */
   for( sub = 0; sub < nvals; sub++ )
      EDIT_substitute_brick(outset, sub, opts->datum, NULL);

   nxya = nxa * nya;    /* note slice slizes */
   nxyb = nxb * nyb;
   nxyc = nxc * nyc;

   /* create output volume and input slices */
   volout = (float *)malloc(sizeof(float)*(nxyc*nzc));
   sliceA = (float *)malloc(sizeof(float)*(nxya));
   sliceB = (float *)malloc(sizeof(float)*(nxyb));

   if( !volout || !sliceA || !sliceB ) {
      fprintf(stderr,"** failed to alloc %d + %d + %d floats for computation\n",
              nxyc*nzc, nxya, nxyb);
      RETURN(1);
   }

   /*--------------- start work ---------------*/

   if( opts->verb > 1 ) fprintf(stderr,"-- have memory, starting work...\n");

   for( sub = 0; sub < nvals; sub++ ) {         /* for each sub-brick */

      asub = onesub ? 0 : sub;  /* first dset might have just one volume */
   
      if( opts->verb > 0 && nvals > 1 ) fprintf(stderr,".");

      for( sind=0; sind < nzc; sind++ ) {       /* for each slice */
         if( copy_slice_as_float(sliceA, opts->dsetA, asub, sind) ) RETURN(1);
         if( copy_slice_as_float(sliceB, opts->dsetB,  sub, sind) ) RETURN(1);

         sliceC = volout + sind * nxyc;

         for( row=0; row < nyc; row++ ) {        /* row = output row */
            for( col=0; col < nxc; col++ ) {     /* col = output col */
               /* compute the inner product of rowsA[row] and colsB[col] */
               sum = 0.0;
               for(ind = 0; ind < nxa; ind++ )
                   sum += sliceA[nxa*row+ind] * sliceB[nxb*ind+col];
   
               sliceC[nxc*row+col] = sum;
            }
         }
      }  /* slice */

      /* scale floats to output datum */
      EDIT_convert_dtype(nxyc*nzc,          /* nvox */
                         MRI_float, volout, /* type/data */
                         opts->datum, DBLK_ARRAY(outset->dblk, sub),
                         0.0);              /* limit */
   }
   if( opts->verb > 0 && nvals > 1 ) fprintf(stderr," done\n");

   *oset = outset;  /* finished, assign dataset pointer */

   /* free processing memory */
   free(sliceA);  free(sliceB);  free(volout);
   
   RETURN(0);
}

/*--------------- copy one slice as a float matrix ---------------*/
int copy_slice_as_float(float *fmat, THD_3dim_dataset *dset, int sub, int sind)
{
   float       fac;
   int         nxy, nz, ind, datum;

   ENTRY("copy_slice_as_float");

   if( sub >= DSET_NVALS(dset) ) {
      fprintf(stderr,"** sub-brick %d is too big for dataset %s\n",
              sub, DSET_PREFIX(dset));
      RETURN(1);
   }

   nxy = DSET_NX(dset)*DSET_NY(dset);
   nz  = DSET_NZ(dset);

   datum = DSET_BRICK_TYPE(dset, sub);
   fac   = DSET_BRICK_FACTOR(dset, sub); if( fac == 0.0 ) fac = 1.0;

   if( g_opts.verb > 2 )
      fprintf(stderr,"++ copy slice to float: %d voxels, type %s, factor %f\n",
              nxy, MRI_TYPE_name[datum], fac);

   switch( datum ) {
       case MRI_byte: {
          byte * data = DBLK_ARRAY(dset->dblk, sub);    /* volume pointer */
          data += sind * nxy;                           /* slice pointer  */
          if( fac != 1.0 ) {
             for(ind = 0; ind < nxy; ind++)
                fmat[ind] = fac * data[ind];
          } else {
             for(ind = 0; ind < nxy; ind++)
                fmat[ind] = (float)data[ind];
          }
          break;
       }

       case MRI_short: {
          short * data = DBLK_ARRAY(dset->dblk, sub);   /* volume pointer */
          data += sind * nxy;                           /* slice pointer  */
          if( fac != 1.0 ) {
             for(ind = 0; ind < nxy; ind++)
                fmat[ind] = fac * data[ind];
          } else {
             for(ind = 0; ind < nxy; ind++)
                fmat[ind] = (float)data[ind];
          }
          break;
       }

       case MRI_float: { /* copy anyway */
          float * data = DBLK_ARRAY(dset->dblk, sub);   /* volume pointer */
          data += sind * nxy;                           /* slice pointer  */
          if( fac != 1.0 ) {
             for(ind = 0; ind < nxy; ind++)
                fmat[ind] = fac * data[ind];
          } else {
             for(ind = 0; ind < nxy; ind++)
                fmat[ind] = (float)data[ind];
          }
          break;
       }

       default:
           fprintf(stderr,"** invalid input data type %s, exiting...\n",
                   MRI_TYPE_name[datum]);
           RETURN(1);
   }

   RETURN(0);
}

/*--------------- write the result dataset ---------------*/
int write_result(options_t * opts, THD_3dim_dataset * oset,
                 int argc, char * argv[])
{
   ENTRY("write_results");

   if( opts->verb > 1 )
      fprintf(stderr,"++ writing result '%s' as file %s...\n",
              opts->prefix, DSET_BRIKNAME(oset));

   tross_Copy_History( opts->dsetB, oset );
   tross_Make_History( "3dmatmult", argc, argv, oset );

   DSET_write(oset);
   WROTE_DSET(oset);

   RETURN(1);
}

int show_help(void)
{
   ENTRY("show_help");

   printf(
   "-------------------------------------------------------------------------\n"
   "Multiply AFNI datasets slice-by-slice as matrices.\n"
   "\n"
   "If dataset A has Ra rows and Ca columns (per slice), and dataset B has\n"
   "Rb rows and Cb columns (per slice), multiply each slice pair as matrices\n"
   "to obtain a dataset with Ra rows and Cb columns.  Here Ca must equal Rb\n"
   "and the number of slices must be equal.\n"
   "\n"
   "In practice the first dataset will probably be a transformation matrix\n"
   "(or a sequence of them) while the second dataset might just be an image.\n"
   "For this reason, the output dataset will be based on inputB.\n"
   "\n"
   "----------------------------------------\n"
   "examples:\n"
   "\n"
   "    3dmatmult -inputA matrix+orig -inputB image+orig -prefix transformed\n"
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
   "required command arguments:\n"
   "\n"
   "    -inputA DSET_A          : specify first (matrix) dataset\n"
   "\n"
   "        The slices of this dataset might be transformation matrices.\n"
   "\n"
   "    -inputB DSET_B          : specify second (matrix) dataset\n"
   "\n"
   "        This dataset might be any image.\n"
   "\n"
   "    -prefix PREFIX          : specify output dataset prefix\n"
   "\n"
   "        This will be the name of the product (output) dataset.\n"
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
   "* If you need to re-orient a 3D dataset so that the rows, columns\n"
   "  and slices are correct for 3dmatmult, you can use the one of the\n"
   "  programs 3daxialize or 3dresample for this purpose.\n"
   "\n"
   "* To multiply a constant matrix into a vector at each voxel, the\n"
   "  program 3dmatcalc is the proper tool.\n"
   "\n"
   "----------------------------------------------------------------------\n"
   "R. Reynolds    (requested by W. Gaggl)\n"
   "\n"
   );

   printf("%s\ncompiled: %s\n\n", g_version, __DATE__);

   RETURN(0);
}

/* ----------------------------------------------------------------------
 * fill the options_t struct
 * return 1 on (acceptable) termination
 *        0 on continue
 *       -1 on error
 */
int process_opts(options_t * opts, int argc, char * argv[] )
{
   int ac;

   ENTRY("process_opts");

   memset(opts, 0, sizeof(options_t));  /* init everything to 0 */
   opts->datum = -1;
   opts->verb = 1;

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
      else if( strcmp(argv[ac],"-datum") == 0 ) {
         if( ++ac >= argc ) ERROR_exit("need argument after '-datum'");

         /* output datum can be short or float */
         if     ( ! strcmp(argv[ac],"short") ) opts->datum = MRI_short;
         else if( ! strcmp(argv[ac],"float") ) opts->datum = MRI_float;
         else ERROR_exit("-datum '%s' is not supported", argv[ac]);

         ac++; continue;
      }

      else if( strcmp(argv[ac],"-inputA") == 0 ) {
         if( opts->dsetA ) ERROR_exit("can't use 2 '-inputA' options");

         if( ++ac >= argc ) ERROR_exit("need argument after '-inputA'");

         opts->dsetA = THD_open_dataset( argv[ac] );
         if( ! opts->dsetA ) ERROR_exit("can't open dataset '%s'",argv[ac]);

         DSET_load(opts->dsetA); CHECK_LOAD_ERROR(opts->dsetA);
         ac++; continue;
      }

      else if( strcmp(argv[ac],"-inputB") == 0 ) {
         if( opts->dsetB ) ERROR_exit("Can't use 2 '-inputB' options");

         if( ++ac >= argc ) ERROR_exit("need argument after '-inputB'");

         opts->dsetB = THD_open_dataset( argv[ac] );
         if( ! opts->dsetB ) ERROR_exit("Can't open dataset '%s'",argv[ac]);

         DSET_load(opts->dsetB); CHECK_LOAD_ERROR(opts->dsetB);
         ac++; continue;
      }

      else if( strcmp(argv[ac],"-prefix") == 0 ) {
         if( ++ac >= argc ) ERROR_exit("need argument after '-prefix'");

         opts->prefix = argv[ac];

         if( !THD_filename_ok(opts->prefix) )
            ERROR_exit("Illegal name after '-prefix'");
         ac++; continue;
      }

      else if( strcmp(argv[ac],"-verb") == 0 ) {
         if( ++ac >= argc ) ERROR_exit("need argument after '-verb'");

         opts->verb = atoi(argv[ac]);
         ac++; continue;
      }

      fprintf(stderr, "** unknown option '%s'\n",argv[ac]);
      RETURN(-1);
       
   }

   if( !opts->dsetA  ) ERROR_exit("missing -inputA dataset");
   if( !opts->dsetB  ) ERROR_exit("missing -inputB dataset");
   if( !opts->prefix ) ERROR_exit("missing -prefix option");

   /* if no datum was specified, use that of the first dataset */
   if( opts->datum == -1 ) opts->datum = DSET_BRICK_TYPE(opts->dsetB, 0);

   /* and check that it is valid */
   if( opts->datum < 0 || opts->datum > MRI_double ) {
      fprintf(stderr, "** have invalid output datum code %d\n", opts->datum);
      RETURN(-1);
   }

   if( opts->verb > 1 )
      fprintf(stderr,"++ datasets loaded, prefix = %s, datum = %s\n",
              opts->prefix, MRI_TYPE_name[opts->datum]);

   RETURN(0);
}

