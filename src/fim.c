/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/* Functional image is made by correlating refernce function from an ASCII
   file with the series of images.  All orthogonal ~ to a + b*x.
   All is scaled accodingly to number of time points an independent
   of referece function amplitude. Abs % change added.
   Andre Jesmanowicz, MCW Milwaukee, 12.15.1992 */

#ifdef SPARKY
#undef _POSIX_SOURCE
#endif
#include <sys/types.h>

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <sys/stat.h>
#include <stdlib.h>
#ifdef   SYSV
#include <sys/fcntl.h>
#endif

#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

#define EPX1      64                   /* List of supported EPI image sizes */
#define EPY1      64                   /* Each triple is: */
#define EPS1      (2*EPX1*EPY1)        /* xsize, ysize, filesize */
#define EPX2      128
#define EPY2      128
#define EPS2      (2*EPX2*EPY2)

#define NOISE_THR 10.                  /* Minimum signal for delta comput */
#define SCALE     10000
#define D_COEF    1.
#define CORR      ".CORR"
#define IM_HEIGHT 256
#define IM_ARR    (IM_HEIGHT*IM_HEIGHT)
#define OFFSET    (28*IM_HEIGHT)       /* offset to data in 145408 bytes im */
#define H_SIZE    (OFFSET+IM_ARR)      /* 256x256 image with header */
#define IM_SIZE   (2*H_SIZE)

#define SEQ_SIZE  1024                /* The length of reference sequence */
#define NF_MAX    1050                /* Max # of files */
#define EXT_FILES 1                   /* Number of external func files */

extern double   strtod();

char            *ProgName, *SQ_name, *f_name[NF_MAX], **ptr, *file_out;
char            *ort_name, file_CORR[256];
int             ORT_ar[EXT_FILES][SEQ_SIZE], ORT_Nr[EXT_FILES], idot, isq;
float           fORT[EXT_FILES][SEQ_SIZE], alpha, fdot, fsq, noise_thr;
int             SQ_Nr, SQ_arr[SEQ_SIZE], n_ort;
int             Im_frst = 0, npoints = 0, m_points;
int             N_im, fsize, isize, ar_size, itmp;
short int       *ar, tmp_ar[H_SIZE], corr_ar[H_SIZE];
float           fSQ[SEQ_SIZE], ft[SEQ_SIZE], fSQ1[SEQ_SIZE], dt[SEQ_SIZE];
float           fa, fb, fim[IM_ARR], amin, amax, ftmp, abmax, del_1, max_dev;
float           pct = 30., pcc, fSQ_vl, fll, coef = D_COEF, fzz;
float           fl_1ref, sq_1n, fcorr, max_cor, delr, next_cor = 0;;
FILE            *fp;
int             offs, normalize = 1, correl_im = 0, diff_im = 0;
int             n_ess = 0;

/* --------------- */
   main(argc,argv)
   int argc;
   char *argv[];
/* --------------- */
{
   int i, j, m;
	
   ProgName = argv[0];
   if (argc <  2)  Syntax();

   noise_thr = NOISE_THR;

   get_line_args(argc, argv);

   if ( is_file(file_out) ) {
      fprintf(stderr, "\n !!! Output file: %s exist. !!!\n", file_out);
      Syntax();
   }

                                /* Init to zero functional image */
   for (i=0; i < IM_ARR; i++) fim[i] = 0.;

   pcc = 1. - .01 * pct;     /* Reference projection value (for rejecton) */

   m_points = 0;                         /* considered time cours points */
   idot = 0;                             /* set integral of ref to zero */
   for (j=0; j < N_im; j++) {       
      if (SQ_arr[j] < 33333 ) {
         idot += SQ_arr[j];
         m_points++;
      }
   }
   if ( m_points == 0) {
      fprintf (stderr, "\n\n !!! Check reference file: %s !!!\n\n",SQ_name);
      exit(-1);
   }
   sq_1n = 1./sqrt((double) m_points);  /* points # normalizing coeff */

   /* make reference function orthogonal to constant one */
   fa = (float) idot / (float) m_points;
   for (j=0; j < N_im; j++) {
      if ( SQ_arr[j] < 33333 )
         fSQ[j] = (float) SQ_arr[j] - fa;
      else
         fSQ[j] = 0.;
   }
   /* The same orthogoanlization to const for ref ort files */
   for (m=0; m < n_ort; m++) {
      idot = 0;
      for (j=0; j < N_im; j++)
         if (SQ_arr[j] < 33333 ) idot += ORT_ar[m][j];
      fa = (float) idot / (float) m_points;
      for (j=0; j < N_im; j++) {
         if (SQ_arr[j] < 33333 )
            fORT[m][j] = (float) ORT_ar[m][j] - fa;
         else
            fORT[m][j] = 0;
      }
   }

/* once othogonalized to const make orthogonalization to -ort files */
/* Temporarly single ort file case. AJ */
   for (m=0; m < n_ort; m++) {
      isq = 0;                                   /* set integral to zero */
      idot = 0;
      for (j=0; j < N_im; j++) {
         if (SQ_arr[j] < 33333 ) {
            fdot += fSQ[j]*fORT[m][j];
            fsq  += fORT[m][j]*fORT[m][j];
         }
      }
      if ( fsq == 0. ) {
         fprintf (stderr, "\n\n !!! Orthogonal file #%d has zeros only !!!\n",
                  m+1);
         exit(-3);
      }
      alpha = fdot / fsq;
      for (j=0; j < N_im; j++) 
         if (SQ_arr[j] < 33333 ) fSQ[j] -= alpha * fORT[m][j];
   }

        
                                    /* normalize ref vector */
   fSQ_vl = 0.;
   for (j=0; j < N_im; j++)  fSQ_vl += fSQ[j] * fSQ[j];
   if (fSQ_vl < 1.e-10) {
    fprintf (stderr, "\n\n !!! Too small variartion in reference file !!!\n\n");
      exit(-1);
   }
   fl_1ref = 1./sqrt((double) fSQ_vl);
   for (j=0; j < N_im; j++) fSQ1[j] = fSQ[j] * fl_1ref; 

   amin =  1.e37;
   amax = -1.e37;
   for (j=0; j < N_im; j++) {
      if ( fSQ1[j] > amax ) amax = fSQ1[j];
      if ( fSQ1[j] < amin ) amin = fSQ1[j];
   }
   del_1 = amax - amin;  /* for reference of deviation */


   max_dev = -1.e37;                 /* for max deviation of the function */
   max_cor = 0.;

   /* go over all pixels */
   for (i=0; i < ar_size; i++) {
      
   /* othogonalize (to const) data vector */
      ftmp = 0;                                 /* for average brightness */
      for (j=0; j < N_im; j++) {
         if ( SQ_arr[j] < 33333 ) {
            dt[j] = ar[j*ar_size+i];
            ftmp += dt[j];
         }
      }
      fa = ftmp /(float) m_points;      /* average value in data pixel */

      if ( ftmp < 0 ) diff_im = 1;  /* not regular images => no % of dev */

      for (j=0; j < N_im; j++) {
         if ( SQ_arr[j] < 33333 )
            dt[j] -= fa;
         else
            dt[j] = 0.;
      }
   /* once data othogonalized to const make orthogonalization to ref files */
   /* Temporarly single ort file case. AJ */
      for (m=0; m < n_ort; m++) {
         fdot = 0;
         for (j=0; j < N_im; j++)
            if ( SQ_arr[j] < 33333 ) fdot += dt[j] * fORT[m][j];
         alpha = fdot / fsq;
         for (j=0; j < N_im; j++)
            if (SQ_arr[j] < 33333 ) dt[j] -= alpha * fORT[m][j];
      }

/* normalize data vector */
      fSQ_vl = 0.;
      for (j=0; j < N_im; j++)
         fSQ_vl += dt[j] * dt[j];

      if (fSQ_vl < 1.e-10)
         for (j=0; j < N_im; j++) ft[j] = 0.;
      else {
         fll = 1./sqrt(fSQ_vl);
         for (j=0; j < N_im; j++)
              ft[j] = dt[j] * fll;
      }

   /* make projection of normalized vectors */
      fcorr = 0.;
      for (j=0; j < N_im; j++) fcorr += ft[j] * fSQ1[j];

   /* make correlation image */
      corr_ar[i+offs] = fcorr * (float) SCALE;

   /* make functional image for not too far data (correralte with normalized
      vector) */
      if ( (fb = fabs((double) fcorr)) > pcc ) {

         for (j=0; j < N_im; j++) fim[i] += dt[j] * fSQ1[j];

         if ( fa > noise_thr ) delr = del_1 * fim[i] / fa;    /* rel change */
         else                  delr = 0.;

         if ( delr > max_dev ) max_dev = delr;       /* max rel dev */
         if ( fb > max_cor ) {
            next_cor = max_cor;
            max_cor = fb;         /* max correl */
         }
         fim[i] *= fl_1ref;             /* make a ratio rel to ref function */
         n_ess++;
      }
   }

   /* Make functional image independent of number of time course points.
      Find min and max. */
   amin =  1.e37;
   amax = -1.e37;
   for (i=0; i < ar_size; i++) {
      if ( fim[i] > amax ) amax = fim[i];
      if ( fim[i] < amin ) amin = fim[i];
   }
   fa = fabs((double) amax);
   fb = fabs((double) amin);
   abmax = max(fa, fb);
   if (abmax < 1.e-10) {
      fprintf (stderr, "\n !!! Check you reference and data files !!!");
      fprintf (stderr, "\n !!! or increase value of -pcnt option. !!!");
      fprintf (stderr, "\n !!! No variation in functional image.  !!!");
      fprintf (stderr, "\n !!! No output file created.            !!!\n\n");
      exit(-2);
   }

   if ( normalize ) fa = (float) SCALE / abmax;
   else             fa = coef;

   printf ("\n Output file: %s\n", file_out);
   printf (" Image data (relative to reference function):\n");
   printf (" max value : %g , scaled : %g\n", amax, amax*fa);
   printf (" min value : %g , scaled : %g\n", amin, amin*fa);
   printf (" max variation : %g %%\n", max_dev*100.);
   printf (" the best correlation: %g %%\n", max_cor*100.);
   printf (" next to the best    : %g %%\n", next_cor*100.);
   printf (" number of essential pixels: %d\n", n_ess);

   for (i=0; i < ar_size; i++) {
      fzz = fim[i]*fa;
      if ( fzz >= 0. ) tmp_ar[i+offs] = min((int) fzz,  SCALE);
      if ( fzz <  0. ) tmp_ar[i+offs] = max((int) fzz, -SCALE);
   }

                       /* reload header (if present) to corr_ar */
   for (i=0; i < offs; i++) corr_ar[i] = tmp_ar[i];

   corr_ar[0+offs] = 0;
   corr_ar[1+offs] = -SCALE;
   corr_ar[2+offs] =  SCALE;   /* so color image will show zero as green */

   tmp_ar[0+offs] = 0;
   tmp_ar[1+offs] = -SCALE;
   tmp_ar[2+offs] =  SCALE;   /* so color image will show zero as green */
	
                                        /* Write functional image to file */
   if (i = write_iqm(file_out, &fsize, tmp_ar) )
     fprintf (stderr, "\n\n Error writing output_file: %s (will not overwrite) %d\n\n", file_out, i);
   
   if (correl_im) {
     strcpy(file_CORR, file_out);
     strcat(file_CORR, CORR);
     if (i = write_iqm(file_CORR, &fsize, corr_ar) )
       fprintf (stderr, "\n\n Error writing output_file: %s (will not overwrite) %d\n\n", file_CORR, i);
   }
   exit(0) ;
}	

/* ------------------------- */
   get_line_args(argc, argv)
   int  argc;
   char *argv[];
/* ------------------------- */
{
   register int i, j, k, nopt, nnn;
   int          sp;
   float	ff;

   n_ort = 0;
   nopt = 0;
   for (i = 1; i < argc; i++) { /* ------- Options ------- */
      if (!strncmp(argv[i], "-h", 2)) {          /* help */
         Syntax();
      }
      if (strncmp(argv[i], "-non", 4) == 0) { /* don't normalize fim image */
         normalize = 0;
         nopt++;
         continue;
      }
      if (strncmp(argv[i], "-corr", 4) == 0) { /* make correlation image */
         correl_im = 1;
         nopt++;
         continue;
      }
      if (strncmp(argv [i], "-coe", 4) == 0) {   /* extra coeff for im_out */
         if (++i >= argc) { Syntax(); exit(2); }
         ptr = argv;
         coef = strtod(argv[i], ptr);
         if ( **ptr ) {
            fprintf (stderr, "\n !!! Wrong value in option -coef: %g !!!\n\n",
                                     coef);
            exit(1);
         }
         nopt++; nopt++;
         continue;
      }
      if (strncmp(argv [i], "-im1", 4) == 0) {  /* first image to count */
         if (++i >= argc) { Syntax(); exit(2); }
         ptr = argv;
         Im_frst = strtod(argv[i], ptr) + .5;
         if ( **ptr || (Im_frst < 1)) {
            fprintf (stderr, "\n !!! First_image_# < 1 in -im1 !!!\n\n");
            exit(1);
         }
         nopt++; nopt++;
         continue;
      }
      if (strncmp(argv [i], "-ort", 4) == 0) {   /* extra orthogonalize */
         if (++i >= argc) { Syntax(); exit(2); }
         ort_name = argv[i];
         if ((fp = fopen(ort_name, "r")) == 0) {
            fprintf(stderr,"\n !!! Problem opening ort file: %s !!!\n",
                ort_name);
            Syntax();
         }
         for(k=0; k < SEQ_SIZE; k++)                      /* Read sequence */
            if (fscanf(fp, "%d", &ORT_ar[n_ort][k]) == EOF) break;
         ORT_Nr[n_ort] = k;
         if ( ORT_Nr[n_ort] == 0 ) {
            fprintf(stderr,"\n !!! Problem reading %s file !!!\n", ort_name);
            Syntax();
         }
         fclose(fp);
         n_ort++;
         nopt++; nopt++;
         continue;
      }
      if (strncmp(argv [i], "-num", 4) == 0) {
         if (++i >= argc) { Syntax(); exit(2); }
         ptr = argv;
         npoints = strtod(argv[i], ptr) + .5;
         if ( **ptr || (npoints < 1)) {
            fprintf (stderr, "\n !!! Too few images specified !!!\n\n");
            exit(1); /* now symbolic for min npoints = 1 . AJ */
         }
         nopt++; nopt++;
         continue;
      }
      if (strncmp(argv [i], "-pcnt", 5) == 0) {
         if (++i >= argc) { Syntax(); exit(2); }
         ptr = argv;
         ff = strtod(argv[i], ptr);
         if ( **ptr || (ff < .0) || (ff > 100.)) {
            fprintf (stderr, "\n !!! %% accuracy in -pcnt out of range: %g !!!\n\n", ff);
            exit(1);
         }
         pct = ff;
         nopt++; nopt++;
         continue;
      }
      if (strncmp(argv [i], "-list", 5) == 0) {
         if (++i >= argc) { Syntax(); exit(2); }
         ptr = argv;
         ff = strtod(argv[i], ptr);
         if ( **ptr || (ff < .0) ) {
            fprintf (stderr, "\n !!! min_value in -list < 0 : %g !!!\n\n", ff);
            exit(1);
         }
         noise_thr = ff;
         nopt++; nopt++;
         continue;
      }
   }
   nopt++;

   SQ_name = argv[nopt];                   /* reference seqence file name */

   if ((fp = fopen(SQ_name, "r")) == 0) {
      fprintf(stderr,"\n !!! Problem opening sequence file: %s !!!\n", SQ_name);
      Syntax();
   }
   for(k=0; k < SEQ_SIZE; k++)                      /* Read sequence */
      if (fscanf(fp, "%d", &SQ_arr[k]) == EOF) break;
   SQ_Nr = k;
   if (SQ_Nr == 0) {
      fprintf(stderr,"\n !!! Problem reading %s file !!!\n", SQ_name);
      Syntax();
   }
   fclose(fp);

   nopt++;                                   /* Files to read (minimum one) */

   if ( nopt > (argc-1) || nopt < (argc - NF_MAX) ) {  /* Nr of files check */
      fprintf (stderr, "\n Wrong # of files. %d files entered :\n", argc-nopt);
      for(i=nopt, j=1; i < argc; i++, j++)
         fprintf (stderr, "  %3d -  %s\n", j, argv[i]);
         Syntax(); exit(2);
   }

   file_out = argv[argc-1];

   N_im = argc-nopt-1;                                        /* # of images */
   if ( Im_frst > N_im ) {
      fprintf (stderr, "\n !!! First_im_# %d in -im1 is bigger then number of images (%d) !!!\n\n", Im_frst, N_im);
      Syntax(); exit(2);
   }
   for (i=0; i < N_im; i++) f_name[i] = argv[i+nopt];
   for (i=0; i < Im_frst-1; i++) f_name[i] = f_name[Im_frst-1];

               /* read and check the length of the first file for validity */
   isize = 0;
   fprintf(stderr, "\n\n Reading file %s\n", f_name[0]);
   if (k = read_iqm(f_name[0], &isize, tmp_ar)) {
      fprintf (stderr, "\n Problem with file: %s\n", f_name[0]);
      Syntax(); exit(2);
   }
   if ( (isize != EPS1) && (isize != EPS2) && (isize != IM_SIZE) ) {
      fprintf (stderr, "\n\n !!! File %s has wrong format !!!\n", f_name[0]);
      Syntax(); exit(2);
   }

   fsize = isize;
   if (fsize == IM_SIZE) {
      ar_size = IM_ARR;    /* 256x256 + header image */
      offs    = OFFSET;
   }
   else {
      ar_size = fsize/2;   /* AxA and no header */
      offs    = 0;
   }

   ar = (short int *) malloc((unsigned) ((ar_size*N_im)*sizeof(short int)));

   for (i=0; i < ar_size; i++) ar[i] = tmp_ar[offs+i]; /* reload first image */

   for (i=1; i < N_im; i++) {                            /* Read image files */
      fprintf(stderr, " Reading file %s\n", f_name[i]);
      isize = 0;
      if ( k = read_iqm(f_name[i], &isize, tmp_ar) ) {
         fprintf (stderr, "\n Problem with file: %s\n", f_name[i]);
         Syntax(); exit(2);
      }
      if ( isize != fsize) {  /* check lengthes of other files */
         fprintf (stderr, "\n\n !!! File %s has different format !!!\n",
                  f_name[i]);
         Syntax(); exit(2);
      }
      k = i*ar_size;
      for (j=0; j < ar_size; j++) ar[k+j] = tmp_ar[offs+j]; /* reload data */
   }
   if (!npoints || npoints > N_im) npoints = N_im;
   for (i=0; i < n_ort; i++) {
      if ( ORT_Nr[i] < N_im ) {
         fprintf(stderr,"\n !!! Orthogonal file # %d too short !!!\n", i);
         exit(-4);
      }
   }
   if ( SQ_Nr < N_im ) {
     fprintf(stderr, "\n\n !!! Reference file to short. Add %d lines !!!\n\n",
              N_im - SQ_Nr); 
     exit(-1);
   }
}

/* ----------------------------------------------------------------
   Read file subroutine fo use in C.        A.Jesmanowicz, MCW 1991
        return error :  0 - OK,
                        1 - opening problem,
                        2 - file longer then array.
        fname : file name.
        size  : on input - max size of the arr or 0 for any length,
                on output- real size of the file (and arr in bytes).
        arr   : returned file as array.
   ---------------------------------------------------------------- */
/* ----------------------------- */
   int  read_iqm(fname,size,arr)
   int  *size;
   char fname[],arr[];
/* ----------------------------- */
{
        int     isize = *size;
        int     fp;                             /* file descriptor  */
        struct  stat file_stat;                 /* status structure */

        if ((fp = open(fname, O_RDONLY)) <= 0)  /* file must exist */
           return(1);                           /* or error = 1.   */

        fstat(fp, &file_stat);                  /* get file size in bytes   */

        if(file_stat.st_size > isize && isize)  /* file can not be too long */
           return(2);                           /* or error = 2.            */

        *size =  file_stat.st_size;             /* return file size */

        read(fp, arr, file_stat.st_size);       /* read whole file  */
        close(fp);
        return(0);                              /* no error : 0 */
}

/* ----------------------------------------------------------------
   Write file subroutine fo use in C.       A.Jesmanowicz, MCW 1991
        return error :  0 - OK,
                        1 - opening problem,
                        2 - negative length.
        fname : file name.
        size  : size of the file.
        arr   : file array.
   ---------------------------------------------------------------- */
/* ----------------------------- */
   int write_iqm(fname,size,arr)
   int  *size;
   char fname[],arr[];
/* ----------------------------- */
{
        int     isize = *size;
        int     fp;                             /* file descriptor  */

        if(isize < 0)                           /* size has to be real */
           return(2);                           /* or error = 2.       */

        if ((fp = open(fname, O_WRONLY|O_CREAT|O_EXCL,0444)) <= 0)
           return(1);                           /* file must not exist */
                                                /* or error = 1.       */

        write(fp, arr, isize);                  /* write whole file */
        close(fp);
        return(0);
}

/* ------------------ */ /* Check for unpleasant existence of output file */
   int is_file(fname)
   char  *fname;
/* ------------------ */
{
   FILE          *fp;

   if ( (fp = fopen(fname, "r")) != NULL ) {    /* return = 1 if file exist */
      fclose(fp);
      return(1);
   }
   else
      return(0);
}

/* ------------------- */
   Syntax()
/* ------------------- */
{
   fprintf (stderr, "\n\n %s makes functional image from time series of images and", ProgName);
   fprintf (stderr, "\n reference (formated) functional-sequence file.");
   fprintf (stderr, "\n Reference function is normalized before any correlation is done, so");
   fprintf (stderr, "\n the results are proportional to the amplitude of functional changes.");
   fprintf (stderr, "\n Additionally, output values are scaled to be independent of the number of");
   fprintf (stderr, "\n used points in the reference functional-sequence file. Output data can");
   fprintf (stderr, "\n be orthogonalized to additional external function using option -ort.");
   fprintf (stderr, "\n Maximum number of external functios: %d", EXT_FILES);
   fprintf (stderr, "\n Image resolusion: up to 256x256\n");
   fprintf (stderr, "\n Usage: %s  [options] func_seq_file  file_1  [file_2 ... file_n]  out_file\n", ProgName);
  fprintf (stderr, "\n Where options are:\n");
  fprintf (stderr, "\n    -non             - don't make normalization (def. normalize to %d)", SCALE);
  fprintf (stderr, "\n    -coef value      - coefficient for output data (def. = %g)", D_COEF);
  fprintf (stderr, "\n    -im1 image_#     - first image in time course. Previous images will be ");
  fprintf (stderr, "\n                       filled with this one for proper timing of others.");
  fprintf (stderr, "\n    -num #_of_images - # of images in time course [2-%d].", NF_MAX);
  fprintf (stderr, "\n    -pcnt #          - accuracy in %% of functional fit (def. = %g%%).", pct);
  fprintf (stderr, "\n                       This value applies to the shape (not amplitude)");
  fprintf (stderr, "\n                       and 30%% means that all data having correlation");
  fprintf (stderr, "\n                       coefficient greater than .7 will be included.");
  fprintf (stderr, "\n    -ort file        - additional function files to which data will be");
  fprintf (stderr, "\n                       orthogonalized. Use one file with each option -ort .");
  fprintf (stderr, "\n                       Maximum number of options -ort: %d .", EXT_FILES);
  fprintf (stderr, "\n    -corr            - make image of correlation coefficients. Range -1 to 1");
  fprintf (stderr, "\n                       is scaled to -/+ %d. The program will create second", SCALE);
  fprintf (stderr, "\n                       output file with extention %s .", CORR);
  fprintf (stderr, "\n    -list min_value  - make report for pixels brighter than min_value.");
  fprintf (stderr, "\n                       Default min_value: %g", NOISE_THR);
   fprintf (stderr, "\n\n Reference functional-sequence file can contain zero in each line for image");
   fprintf (stderr, "\n with no action and value one for action. More complicated functions are OK.");
   fprintf (stderr, "\n All values should be in the range -32768 to 32767. Any image can");
   fprintf (stderr, "\n be discarded from computation by placing value 33,000 or more");
   fprintf (stderr, "\n in equivalent line. Plot file of center pixel, made by program FD (using <p>)");
   fprintf (stderr, "\n can be used as reference file too. AJ.");
   fprintf (stderr, "\n\n");
   exit(1);
}
