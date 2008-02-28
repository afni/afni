/***** This code is part of the AFNI software package, which is   *****
 ***** partly in the public domain and partly covered by the GPL. *****
 ***** See http://afni.nimh.nih.gov/afni for more information.    *****/

#include "mrilib.h"

static void vstep_print(void) ; /* prototype */

#define FALTUNG

int main( int argc , char *argv[] )
{
   int iarg , ii,jj,kk , nx,ny,nz,nvox , vstep=0 ;
   THD_3dim_dataset *rhset=NULL ; char *rhsnam="?" ;
   THD_3dim_dataset *lset ; MRI_IMAGE *lim ; int nlset=0 , nlhs=0 ;
   THD_3dim_dataset *fset=NULL ;
   XtPointer_array *dsar ;
   int ntime , nvar=0 ;
   char *prefix="Tfitter" ;
   int meth=2 , nbad=0,ngood=0 ;
   intvec *convec=NULL , *kvec=NULL ;
   byte *mask=NULL ; int mnx,mny,mnz ;
   floatvec *bfit ;
   float *dvec , **rvec=NULL , *cvec=NULL ;
   char **lab=NULL ; int nlab=0 ;
   int verb=1 ;
#ifdef FALTUNG
   THD_3dim_dataset *fal_set=NULL ; MRI_IMAGE *fal_im=NULL ;
   char *fal_pre ; int fal_pencod, fal_klen=0 , fal_dcon=0 ;
   float *fal_kern=NULL , fal_penfac ;
   THD_3dim_dataset *defal_set=NULL ;
#endif
   int nvoff=0 ;

   /*------- help the pitifully ignorant user? -------*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dTfitter [options]\n"
      "At each voxels, assembles and solves a set of linear equations.\n"
      "Output is a bucket dataset with the parameters at each voxel.\n"
      "\n"
      "Options:\n"
      "--------\n"
      "  -RHS rset = Specifies the right-hand-side 3D+time dataset.\n"
      "\n"
      "  -LHS lset = Specifies a column (or columns) of the left-hand-side matrix.\n"
      "             * More than one 'lset' can follow the '-LHS' option, but each\n"
      "               input filename must NOT start with the '-' character!\n"
      "             * Or you can use multiple '-LHS' options, if you prefer.\n"
      "             * Each 'lset' can be a 3D+time dataset, or a 1D file\n"
      "               with 1 or more columns.\n"
      "             * A 3D+time dataset defines one column in the LHS matrix.\n"
      "             * A 1D file defines as many columns in the LHS matrix as\n"
      "               are in the file.\n"
      "           *** Columns are assembled in the order given on the command line,\n"
      "               which means that parameters will be output in that order!\n"
      "           *** If all LHS inputs are 1D vectors and you are using least\n"
      "               squares fitting without constraints, then 3dDeconvolve would\n"
      "               be more efficient, since each voxel would have the same set\n"
      "               of equations -- a fact that 3dDeconvolve exploits for speed.\n"
      "\n"
      "  -label lb = Specifies a sub-brick label for the output dataset.\n"
      "             * More than one 'lb' can follow the '-label' option;\n"
      "               however, each label must NOT start with the '-' character!\n"
      "             * Labels are applied in the order given.\n"
      "             * Normally, you would provide exactly as many labels as\n"
      "               LHS columns.  If not, the program invents some labels.\n"
#ifdef FALTUNG
      "\n"
      "  -FALTUNG fset fpre pen fac\n"
      "            = Specifies a convolution (German: Faltung) model to be\n"
      "              added to the LHS matrix:\n"
      "             * 'fset' is a 3D+time dataset or a 1D file that specifies\n"
      "               the known kernel of the convolution.\n"
      "             * fset's time point [0] is the 0-lag point in the kernel,\n"
      "               [1] is the 1-lag into the past point, etc.\n"
      "             * 'fpre' is the prefix for the output time series to\n"
      "               be created -- it will have the same length as the\n"
      "               input 'rset' time series.\n"
      "             * 'pen' denotes the type of penalty function to be\n"
      "               applied to constrain the deconvolved time series:\n"
      "              ++ The following penalty functions are available:\n"
      "                   P0[s] = f^q * sum{ |s(t)|^q }\n"
      "                   P1[s] = f^q * sum{ |s(t)-s(t-1)|^q }\n"
      "                   P2[s] = f^q * sum{ |s(t)-0.5*s(t-1)-0.5*s(t+1)|^q }\n"
      "                 where s(t) is the deconvolved time series;\n"
      "                 where q=1 for L1 fitting, q=2 for L2 fitting;\n"
      "                 where f is the value of 'fac' (defined below).\n"
      "                   P0 tries to keep s(t) itself small\n"
      "                   P1 tries to keep point-to-point fluctuations\n"
      "                      in s(t) small (1st derivative)\n"
      "                   P2 tries to keep 3 point fluctuations\n"
      "                      in s(t) small (2nd derivative)\n"
      "              ++ In L2 regression, these penalties are like Wiener\n"
      "                 deconvolution with noise spectra proportional to\n"
      "                   P0 ==> f^2 (constant in frequency)\n"
      "                   P1 ==> f^2 * freq^2\n"
      "                   P2 ==> f^2 * freq^4\n"
      "                 However, 3dTfitter does deconvolution in\n"
      "                 the time domain, not the frequency domain!\n"
      "              ++ The value of 'pen' is one of the following 7 cases:\n"
      "                     0 = use P0 only\n"
      "                     1 = use P1 only\n"
      "                     2 = use P2 only\n"
      "                    01 = use P0+P1 (the sum of these two functions)\n"
      "                    02 = use P0+P2\n"
      "                    12 = use P1+P2\n"
      "                   012 = use P0+P1+P2\n"
      "                 If 'pen' does not contain any of the digits 0..2,\n"
      "                 then '01' will be used.\n"
      "             * 'fac' is the positive weight for the penalty function:\n"
      "              ++ if fac < 0, then the program chooses a penalty\n"
      "                 factor for each voxel and then scales that by -fac.\n"
      "              ++ fac = 0 is like selecting fac = -1, and is recommended\n"
      "                 if you have no other idea and don't want to experiment.\n"
      "              ++ SOME penalty has to be applied, since otherwise the\n"
      "                 set of linear equations for s(t) is under-determined\n"
      "                 and/or ill-conditioned!\n"
      "             * If '-LHS' is also used, those basis vectors can be\n"
      "               thought of as a baseline to be regressed out at the\n"
      "               same time the convolution model is fitted.\n"
      "           *** At most one '-FALTUNG' option can be used!\n"
      "   *********** THIS OPTION IS EXPERIMENTAL AND NOT FULLY TESTED YET!\n"
      "           *** And it can be pretty slow, applied to a 3D+time dataset.\n"
#endif
      "\n"
      "  -lsqfit   = Solve equations via least squares [the default].\n"
      "             * '-l2fit' is a synonym for this option\n"
      "\n"
      "  -l1fit    = Solve equations via least sum of absolute residuals.\n"
      "             * L1 fitting is a slower than L2 fitting, but is\n"
      "               perhaps less sensitive to outliers in the data.\n"
      "             * L2 fitting is statistically more efficient when\n"
      "               the noise is normally (Gaussian) distributed.\n"
      "\n"
      "  -consign  = Follow this option with a list of parameter indexes\n"
      "              to indicate that the sign of some output parameters\n"
      "              should be constrained in the solution; for example:\n"
      "                 -consign +1 -3\n"
      "              which indicates that parameter #1 (from the first -LHS)\n"
      "              must be non-negative, and that parameter #3 must be\n"
      "              non-positive.  Parameter #2 is unconstrained (e.g., the\n"
      "              output can be positive or negative).\n"
      "             * Parameter counting starts with 1, and corresponds to\n"
      "               the order in which the LHS columns are specified.\n"
      "             * Unlike '-LHS or '-label', only one '-consign' option\n"
      "               can be used.\n"
      "             * Do NOT give the same index more than once after\n"
      "               '-consign' -- you can't specify that an coefficient\n"
      "               is both non-negative and non-positive, for example!\n"
      "           *** Constraints can be used with '-l1fit' AND with '-l2fit'.\n"
#ifdef FALTUNG
      "           *** '-consign' constraints only apply to the '-LHS'\n"
      "               fit parameters.  To constrain the '-FALTUNG' output,\n"
      "               use the option below.\n"
      "\n"
      "  -consFAL s= Constrain the deconvolution time series from '-FALTUNG'\n"
      "              to be positive if 's' is '+' or to be negative if\n"
      "              's' is '-'.\n"
#endif
      "\n"
      "  -prefix p = Prefix for the output dataset filename.\n"
      "             * Which is always in float format.\n"
      "\n"
      "  -mask ms  = Read in dataset 'ms' as a mask; only voxels with nonzero\n"
      "              values in the mask will be processed.  Voxels falling\n"
      "              outside the mask will be set to all zeros in the output.\n"
      "\n"
      "  -quiet    = Don't print progress report messages.\n"
      "\n"
      "NON-Options:\n"
      "------------\n"
      "* There is no '-fitts' option to produces the fitted time series\n"
      "  dataset at each voxel.  You could use 3dcalc for this purpose.\n"
      "  If you are clever.\n"
      "* There is no option to produce statistical estimates of the\n"
      "  significance of the parameter estimates.\n"
      "* There are no options for censoring or baseline generation.\n"
      "* There is no option to constrain the range of the output parameters,\n"
      "  except as provided by '-consign'.\n"
      "\n"
      "Contrived Example:\n"
      "------------------\n"
      "The dataset 'atm' and 'btm' are assumed to have 99 time points each.\n"
      "We use 3dcalc to create a synthetic combination of these plus a constant\n"
      "plus Gaussian noise, then use 3dTfitter to fit the weights of these\n"
      "3 functions to each voxel, using 4 different methods.  Note the use of\n"
      "the input 1D time series '1D: 99@1' to provide the constant term.\n"
      "\n"
      " 3dcalc -a atm+orig -b btm+orig -expr '-2*a+b+gran(100,20)' -prefix 21 -float\n"
      " 3dTfitter -RHS 21+orig -LHS atm+orig btm+orig '1D: 99@1' -prefix F2u -l2fit\n"
      " 3dTfitter -RHS 21+orig -LHS atm+orig btm+orig '1D: 99@1' -prefix F1u -l1fit\n"
      " 3dTfitter -RHS 21+orig -LHS atm+orig btm+orig '1D: 99@1' -prefix F1c -l1fit \\\n"
      "           -consign -1 +3\n"
      " 3dTfitter -RHS 21+orig -LHS atm+orig btm+orig '1D: 99@1' -prefix F2c -l2fit \\\n"
      "           -consign -1 +3\n"
      "\n"
      "In the absence of noise and error, the output datasets should be\n"
      "  #0 sub-brick = -2.0 in all voxels\n"
      "  #1 sub-brick = +1.0 in all voxels\n"
      "  #2 sub-brick = +100.0 in all voxels\n"
      "\n"
      "Yet More Contrivance:\n"
      "---------------------\n"
      "You can input a 1D file for the RHS dataset, as in the example below,\n"
      "to fit a single time series to a weighted sum of other time series:\n"
      "\n"
      " 1deval -num 30 -expr 'cos(t)' > Fcos.1D\n"
      " 1deval -num 30 -expr 'sin(t)' > Fsin.1D\n"
      " 1deval -num 30 -expr 'cos(t)*exp(-t/20)' > Fexp.1D\n"
      " 3dTfitter -quiet -RHS Fexp.1D -LHS Fcos.1D Fsin.1D -prefix -\n"
      "\n"
      "* Note the use of the '-' as a prefix to write the results\n"
      "  (just 2 numbers) to stdout, and the use of '-quiet' to hide\n"
      "  the chatty and informative progress messages.\n"
      "* For the Jedi AFNI Masters out there, the above example can be carried\n"
      "  out on using single complicated command line:\n"
      "\n"
      " 3dTfitter -quiet -RHS `1deval -1D: -num 30 -expr 'cos(t)*exp(-t/20)'` \\\n"
      "                  -LHS `1deval -1D: -num 30 -expr 'cos(t)'`            \\\n"
      "                       `1deval -1D: -num 30 -expr 'sin(t)'`            \\\n"
      "                  -prefix - \n"
      "\n"
      "  resulting in the single output line below:\n"
      "\n"
      " 0.535479 0.000236338\n"
      "\n"
      "  which are respectively the fit coefficients of 'cos(t)' and 'sin(t)'.\n"
#ifdef FALTUNG
      "\n"
      "Contrived Deconvolution Example:\n"
      "--------------------------------\n"
      "(1) Create a 101 point 1D file that is a block of 'activation'\n"
      "    between points 40..50\n"
      "       3dConvolve -input1D -polort -1 -num_stimts 1     \\\n"
      "                  -stim_file 1 '1D: 40@0 10@1 950@0'    \\\n"
      "                  -stim_minlag 1 0 -stim_maxlag 1 5     \\\n"
      "                  -iresp 1 '1D: 0 1 2 3 2 1' -nlast 100 \\\n"
      "            | grep -v Result | grep -v '^$' > F101.1D\n"
      "\n"
      "(2) Create a 3D+time dataset with this time series in each\n"
      "    voxel, plus noise that increases with voxel 'i' index\n"
      "       3dUndump -prefix Fjunk -dimen 100 100 1\n"
      "       3dcalc -a Fjunk+orig -b F101.1D     \\\n"
      "              -expr 'b+gran(0,0.04*(i+1))' \\\n"
      "              -float -prefix F101d\n"
      "       /bin/rm -f Fjunk+orig.*\n"
      "\n"
      "(3) Deconvolve and see what you get\n"
      "       3dTfitter -RHS F101d+orig -l1fit \\\n"
      "                 -FALTUNG '1D: 0 1 2 3 2 1' F101d_fal1 012 0.0\n"
      "       3dTfitter -RHS F101d+orig -l2fit \\\n"
      "                 -FALTUNG '1D: 0 1 2 3 2 1' F101d_fal2 012 0.0\n"
      "\n"
      "    View F101d_fal1+orig, F101d_fal2+orig, and F101d+orig in AFNI,\n"
      "    and see how the fit quality varies with the noise level and the\n"
      "    regression type (L1 or L2 fitting).\n"
#endif
      "\n"
      "---------------------------------------------------------------------\n"
      "-- RWCox - Feb 2008.\n"
      "-- Created for the imperial purposes of John A Butman, MD PhD.\n"
      "-- But may be useful for some other well-meaning souls out there.\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dTfitter"); machdep();
   PRINT_VERSION("3dTfitter"); AUTHOR("RWCox") ;
   AFNI_logger("3dTfitter",argc,argv);

   /*------- read command line args -------*/

   iarg = 1 ; INIT_XTARR(dsar) ;
   while( iarg < argc ){

#ifdef FALTUNG
     if( strcasecmp(argv[iarg],"-faltung") == 0 ){
       int p0,p1,p2 ;
       if( fal_set != NULL || fal_im != NULL )
         ERROR_exit("Can't have two -FALTUNG arguments") ;
       if( iarg+4 >= argc )
         ERROR_exit("Need 4 arguments after '%s'",argv[iarg]);
       iarg++ ;
       if( strstr(argv[iarg],"1D") != NULL ){
         fal_im = mri_read_1D(argv[iarg]) ;
         if( fal_im == NULL )
           ERROR_exit("Can't read -FALTUNG time series from '%s'",argv[iarg]) ;
         fal_klen = fal_im->nx ;
         if( fal_klen < 2 )
           ERROR_exit("-FALTUNG 1D file '%s' has only 1 time point!",argv[iarg]);
         if( fal_im->ny > 1 )
           WARNING_message("Only using first column of -FALTUNG 1D file") ;
         fal_kern = MRI_FLOAT_PTR(fal_im) ;
         for( jj=0 ; jj < fal_klen && fal_kern[jj] == 0.0f ; jj++ ) ; /*nada*/
         if( jj == fal_klen )
           ERROR_exit("-FALTUNG 1D file '%s' is all zeros!",argv[iarg]) ;
       } else {
         fal_set = THD_open_dataset(argv[iarg]) ;
         CHECK_OPEN_ERROR(fal_set,argv[iarg]) ;
         fal_klen = DSET_NVALS(fal_set) ;
         if( fal_klen < 2 )
           ERROR_exit("-FALTUNG dataset '%s' has only 1 sub-brick!",argv[iarg]);
       }
       fal_pre = strdup(argv[++iarg]) ;
       if( !THD_filename_ok(fal_pre) )
         ERROR_exit("Illegal filename prefix '%s'",argv[iarg]) ;
       iarg++ ;
       p0 = (strchr(argv[iarg],'0') != NULL) ;
       p1 = (strchr(argv[iarg],'1') != NULL) ;
       p2 = (strchr(argv[iarg],'2') != NULL) ;
       if( p0==0 && p1==0 && p2==0 ){
         WARNING_message(
           "-FALTUNG 'pen' value '%s' illegal: defaulting to '01'",argv[iarg]);
         p0 = p1 = 1 ;
       }
       fal_pencod = p0 + 2*p1 + 4*p2 ; /* encode in bits */
       fal_penfac = (float)strtod(argv[++iarg],NULL) ;
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-consFAL",7) == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       fal_dcon = (argv[iarg][0] == '+') ?  1
                 :(argv[iarg][0] == '-') ? -1 : 0 ;
       if( fal_dcon == 0 )
         WARNING_message("value after '-consFAL' is not '+' or '-' -- ignoring");
       iarg++ ; continue ;
     }
#endif

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ;
       if( mask != NULL )
         ERROR_exit("Can't have two -mask arguments!") ;
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       mset = THD_open_dataset(argv[iarg]) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mnx = DSET_NX(mset); mny = DSET_NY(mset); mnz = DSET_NZ(mset);
       mask = THD_makemask( mset, 0, 1.0f,0.0f ); DSET_delete(mset);
       if( mask == NULL ) ERROR_exit("Can't make mask") ;
       ii = THD_countmask( mnx*mny*mnz , mask ) ;
       INFO_message("%d voxels in the mask",ii) ;
       if( ii < 1 ) ERROR_exit("mask is empty?!") ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-rhs") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       if( rhset != NULL )
         ERROR_exit("Can't have two '%s' options",argv[iarg-1]);
       rhsnam = malloc(sizeof(char)*(strlen(argv[iarg])+4)) ;
       strcpy(rhsnam,argv[iarg]) ;
       if( STRING_HAS_SUFFIX_CASE(rhsnam,"1D") ||
           strncmp(rhsnam,"1D:",3) == 0          ) strcat(rhsnam,"'");
       rhset = THD_open_dataset( rhsnam ) ;
       if( rhset == NULL )
         ERROR_exit("Can't open dataset '%s'",argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-lhs") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       if( argv[iarg][0] == '-' )
         ERROR_exit("Illegal argument after '%s'",argv[iarg-1]) ;
       for( ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
         if( strstr(argv[iarg],"1D") != NULL ){
           lim = mri_read_1D(argv[iarg]) ;
           if( lim == NULL )
             ERROR_exit("Can't read 1D file '%s'",argv[iarg]) ;
           if( lim->ny > 1 )
             INFO_message("1D file '%s' has %d columns",argv[iarg],lim->ny);
           ADDTO_XTARR(dsar,lim) ;
           ii= XTARR_NUM(dsar)-1 ; XTARR_IC(dsar,ii) = IC_FLIM ;
           nvar += lim->ny ;
         } else {
           lset = THD_open_dataset(argv[iarg]) ;
           if( lset == NULL )
             ERROR_exit("Can't read dataset '%s'",argv[iarg]) ;
           ADDTO_XTARR(dsar,lset) ;
           ii= XTARR_NUM(dsar)-1 ; XTARR_IC(dsar,ii) = IC_DSET ;
           nvar++ ; nlset++ ;
         }
       }
       continue ;
     }

     if( strncasecmp(argv[iarg],"-label",4) == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       if( argv[iarg][0] == '-' )
         ERROR_exit("Illegal argument after '%s'",argv[iarg-1]) ;
       for( ; iarg < argc && argv[iarg][0] != '-' ; iarg++ ){
         lab = (char **)realloc((void *)lab,sizeof(char *)*(nlab+1)) ;
         lab[nlab++] = strdup(argv[iarg]) ;
       }
       continue ;
     }

     if( strncasecmp(argv[iarg],"-lsqfit",4) == 0 ||
         strncasecmp(argv[iarg],"-l2fit",4)  == 0   ){
       meth = 2 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-l1fit") == 0 ){
       meth = 1 ; iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-prefix",5) == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       prefix = argv[iarg] ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("Illegal string after -prefix: '%s'",prefix) ;
       iarg++ ; continue ;
     }

     if( strncasecmp(argv[iarg],"-consign",7) == 0 ){
       char *cpt , nvec ;
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]);
       if( convec != NULL )
         ERROR_exit("Can't have 2 -consign options!") ;
       MAKE_intvec(convec,1) ;
       for( nvec=0 ; iarg < argc ; iarg++ ){
         ii = (int)strtod(argv[iarg],&cpt) ;
         if( ii == 0 || *cpt != '\0' ) break ;  /* bad */
         for( jj=0 ; jj < nvec ; jj++ ){
           if( abs(convec->ar[jj]) == abs(ii) )
             ERROR_exit("Duplicate indexes in -consign!") ;
         }
         RESIZE_intvec(convec,nvec+1) ;
         convec->ar[nvec++] = ii ;
       }
       if( nvec < 1 )
         ERROR_exit("No legal values after -consign?!") ;
       continue ;
     }

     if( strncasecmp(argv[iarg],"-quiet",2) == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }

     ERROR_exit("Unknown argument on command line: '%s'",argv[iarg]) ;
   }

   /*------- check options for completeness and consistency -----*/

   if( rhset == NULL )
     ERROR_exit("No RHS dataset input!?") ;
   ntime = DSET_NVALS(rhset) ;
   if( ntime < 2 )
     ERROR_exit("RHS dataset '%s' has only 1 value per voxel?!",rhsnam) ;

   nx = DSET_NX(rhset); ny = DSET_NY(rhset); nz = DSET_NZ(rhset);
   nvox = nx*ny*nz;

   if( mask != NULL && (mnx != nx || mny != ny || mnz != nz) )
     ERROR_exit("mask and RHS datasets don't match in grid size") ;

#ifndef FALTUNG
   if( nvar < 1 )
     ERROR_exit("no LHS time series input?!") ;
#endif
   if( nvar >= ntime )
     ERROR_exit("too many (%d) LHS time series for %d time points",nvar,ntime) ;

   nlhs = dsar->num ;  /* number of -LHS inputs */

#ifdef FALTUNG
   if( fal_klen > 0 ){
     if( fal_set != NULL ){
       if( DSET_NX(fal_set) != nx ||
           DSET_NY(fal_set) != ny || DSET_NZ(fal_set) != nz )
        ERROR_exit("-FALTUNG dataset and RHS dataset don't match in grid size");
     }
     if( fal_klen >= ntime/2 )
       ERROR_exit("-FALTUNG kernel size %d longer than 1/2 dataset %d",
                  fal_klen , ntime/2 ) ;
     if( fal_set != NULL )
       fal_kern = (float *)malloc(sizeof(float)*fal_klen) ;
     nvoff = ntime ;
   }
#else
   if( nlhs == 0 )
     ERROR_exit("no -LHS option given?!") ;
#endif

   dvec = (float * )malloc(sizeof(float)*ntime) ;  /* RHS vector */
   if( nvar > 0 )
     rvec = (float **)malloc(sizeof(float *)*nvar ) ;  /* LHS vectors */

   /*--- check LHS inputs and assign ref vectors ---*/

   if( nlhs > 0 ){
     MAKE_intvec(kvec,nlhs) ;
     for( kk=ii=0 ; ii < nlhs ; ii++ ){
       if( XTARR_IC(dsar,ii) == IC_FLIM ){  /* 1D image: ref points to data */
         float *lar ; int mm ;
         lim = (MRI_IMAGE *)XTARR_XT(dsar,ii) ;
         jj = lim->nx ; lar = MRI_FLOAT_PTR(lim) ;
         if( jj < ntime )
           ERROR_exit("LHS 1D file is too short: %d < %d",jj,ntime) ;
         if( jj > ntime )
           WARNING_message("LHS 1D file too long: %d > %d: ignoring extra",jj,ntime);
         for( mm=0 ; mm < lim->ny ; mm++ ) rvec[kk++] = lar + mm*lim->nx ;
         kvec->ar[ii] = -1 ;
       } else if( XTARR_IC(dsar,ii) == IC_DSET ){ /* dset: create ref vector */
         lset = (THD_3dim_dataset *)XTARR_XT(dsar,ii) ;
         if( DSET_NX(lset) != nx || DSET_NY(lset) != ny || DSET_NZ(lset) != nz )
           ERROR_exit("LHS dataset '%s' doesn't match RHS dataset grid size",
                      DSET_HEADNAME(lset) ) ;
         jj = DSET_NVALS(lset) ;
         if( jj < ntime )
           ERROR_exit("LHS dataset is too short: %d < %d",jj,ntime) ;
         if( jj > ntime )
           WARNING_message("LHS dataset too long: %d > %d: ignoring extra",jj,ntime);
         kvec->ar[ii] = kk ;  /* index of vector from this dataset */
         rvec[kk++] = (float *)malloc(sizeof(float)*(jj+1)) ;
       } else {
         ERROR_exit("This message should never be seen by mortal eyes!") ;
       }
     }
   }

#if 0
   if( verb && nlset == 0 && meth == 2 && convec == NULL )
     INFO_message("LHS datasets all 1D files ==> you could use 3dDeconvolve");
#endif

   if( nlab < nvar ){
     char lll[32] ;
     if( verb )
       INFO_message("Making up %d LHS labels (out of %d parameters)",nvar-nlab,nvar);
     lab = (char **)realloc((void *)lab,sizeof(char *)*nvar) ;
     for( ii=nlab ; ii < nvar ; ii++ ){
       sprintf(lll,"Param#%d",ii+1) ; lab[ii] = strdup(lll) ;
     }
   } else if( nlab > nvar ){
     WARNING_message("Too many (%d) -label strings for %d parameters",nlab,nvar) ;
   }

   /*--- create constraint vector ---*/

   if( convec != NULL ){
     if( nvar > 0 ){
       cvec = (float *)calloc(sizeof(float),nvar) ;
       for( ii=0 ; ii < convec->nar ; ii++ ){
         kk = convec->ar[ii] ; jj = abs(kk) ;
         if( jj > nvar ) ERROR_exit("Index %d in -consign is too large",jj) ;
         cvec[jj-1] = (kk < 0) ? -1.0f : 1.0f ;
       }
     } else {
       WARNING_message("-consign option ignored: no -LHS given!") ;
     }
   }

   /*----- load input datasets -----*/

   if( verb ) INFO_message("loading input datasets into memory") ;

   DSET_load(rhset) ; CHECK_LOAD_ERROR(rhset) ;
   for( ii=0 ; ii < nlhs ; ii++ ){
     if( XTARR_IC(dsar,ii) == IC_DSET ){
       lset = (THD_3dim_dataset *)XTARR_XT(dsar,ii) ;
       DSET_load(lset) ; CHECK_LOAD_ERROR(lset) ;
     }
   }

   /*------ create output dataset ------*/

   if( nvar > 0 ){
     fset = EDIT_empty_copy(rhset) ;
     EDIT_dset_items( fset ,
                        ADN_nvals     , nvar           ,
                        ADN_ntt       , 0              ,
                        ADN_func_type , FUNC_BUCK_TYPE ,
                        ADN_type      , HEAD_FUNC_TYPE ,
                        ADN_datum_all , MRI_float      ,
                        ADN_brick_fac , NULL           ,
                        ADN_prefix    , prefix         ,
                      ADN_none ) ;
     tross_Copy_History( rhset , fset ) ;
     tross_Make_History( "3dTfitter" , argc,argv , fset ) ;

     for( jj=0 ; jj < nvar ; jj++ ){ /* create empty bricks to be filled below */
       EDIT_substitute_brick( fset , jj , MRI_float , NULL ) ;
       EDIT_BRICK_LABEL( fset , jj , lab[jj] ) ;
     }
   }

#ifdef FALTUNG
   if( fal_klen > 0 ){
     defal_set = EDIT_empty_copy(rhset) ;
     EDIT_dset_items( defal_set ,
                        ADN_datum_all , MRI_float ,
                        ADN_brick_fac , NULL      ,
                        ADN_prefix    , fal_pre   ,
                      ADN_none ) ;
     tross_Copy_History( rhset , defal_set ) ;
     tross_Make_History( "3dTfitter" , argc,argv , defal_set ) ;

     for( jj=0 ; jj < ntime ; jj++ ) /* create empty bricks to be filled below */
       EDIT_substitute_brick( defal_set , jj , MRI_float , NULL ) ;
   }
#endif

   /*------- loop over voxels and process them ---------*/

   if( verb && nvox > 499 ){
     INFO_message("begin voxel loop:") ; vstep = nvox / 50 ;
   }
   if( vstep > 0 ) fprintf(stderr,"++ loop: ") ;

   for( ii=0 ; ii < nvox ; ii++ ){

     if( vstep > 0 && ii%vstep==vstep-1 ) vstep_print() ;

     if( mask != NULL && mask[ii] == 0 ) continue ; /* skip */

     THD_extract_array( ii , rhset , 0 , dvec ) ;   /* get RHS data vector */

     for( jj=0 ; jj < nlhs ; jj++ ){               /* get LHS data vectors */
       if( XTARR_IC(dsar,jj) == IC_DSET ){
         lset = (THD_3dim_dataset *)XTARR_XT(dsar,jj) ;
         kk = kvec->ar[jj] ;
         THD_extract_array( ii , lset , 0 , rvec[kk] ) ;
       }
     }

#ifdef FALTUNG
     if( fal_klen > 0 ){
       if( fal_set != NULL )
         THD_extract_array( ii , fal_set , 0 , fal_kern ) ;

       bfit = THD_deconvolve( ntime , dvec ,
                              0 , fal_klen-1 , fal_kern ,
                              nvar , rvec , meth , cvec , fal_dcon ,
                              fal_pencod , fal_penfac               ) ;
     } else {
       bfit = THD_fitter( ntime , dvec , nvar , rvec , meth , cvec ) ;
     }

     if( bfit == NULL ){ nbad++; continue; } /* bad */

     if( nvar > 0 )
       THD_insert_series( ii , fset , nvar , MRI_float , bfit->ar+nvoff , 1 ) ;

     if( fal_klen > 0 )
       THD_insert_series( ii , defal_set , ntime , MRI_float , bfit->ar , 1 ) ;
#else
     /* solve equations */

     bfit = THD_fitter( ntime , dvec , nvar , rvec , meth , cvec ) ;

     if( bfit == NULL ){ nbad++; continue; } /* bad */

     /* put results into dataset */

     THD_insert_series( ii , fset , nvar , MRI_float , bfit->ar , 1 ) ;
#endif

     KILL_floatvec(bfit) ; ngood++ ;
   }

   if( vstep > 0 ) fprintf(stderr," Done\n") ;

   /*----- clean up and go away -----*/

   if( nbad > 0 )
     WARNING_message("Fit worked in %d voxels; failed in %d",ngood,nbad) ;
   else if( verb )
     INFO_message("Fit worked on all %d voxels",ngood) ;

   if( fset != NULL ){
     DSET_write(fset); DSET_unload(fset);
     if( verb ) WROTE_DSET(fset);
   }
#ifdef FALTUNG
   if( defal_set != NULL ){
     DSET_write(defal_set); DSET_unload(defal_set);
     if( verb ) WROTE_DSET(defal_set);
   }
#endif
   if( verb ) INFO_message("Total CPU time = %.1f s",COX_cpu_time()) ;
   exit(0);
}

/*---------------------------------------------------------------------------*/

static void vstep_print(void)
{
   static int nn=0 ;
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[nn%10] ) ;
   if( nn%10 == 9) fprintf(stderr,".") ;
   nn++ ;
}
