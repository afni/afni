/*********************** 1dSEM.c *****************************************/
/* Author: Daniel Glen, 16 Nov 2006                                      */
/* computes STRUCTURAL EQUATION MODEL (SEM) parameters from the mean     */
/* time courses of 2-n ROIS                                              */
/* This version uses the  Powell NEWUOA optimization method to fit the   */
/* parameters. Various methods are used to verify a model or search for  */
/* the best model                                                        */
/*************************************************************************/
#include "matrix.h"
#include "afni.h"
#include "sqrmat.h"

#define TINYNUMBER 1E-10
#define SMALLNUMBER 1E-4
#define HUGENUMBER 1E38

static int max_iter = 10000;    /* maximum number of iterations (per path) */
static int nrand = 100;         /* number of random trials for optimization */
static int verbose = 0;         /* message display level */
static int ntheta = 2;          /* number of theta connection coefficients to calculate */
static double DF=0.0;           /* degrees of freedom */
static double F_fixed;          /* fixed components for cost function */
static int model_search = 0;    /* search for best model */
static int max_paths = 1000;    /* maximum number of paths to try in model search */
static double stop_cost = 0.1;  /* stop searching if cost function drops below this value */
static int grow_all = 0;        /* search for models over all possible combinations */
static int leafpicker = 0;      /* optimize forest growth over multiple parameter combinations */
static double theta_ll = -1.0;  /* lower limit for theta */
static double theta_ul = 1.0;   /* upper limit for theta */
static sqrmat *kmat;            /* matrix of path coefficients (thetas) */
static sqrmat *theta_init_mat;  /* coded initial matrix of path coefficients */
static sqrmat *psi_mat;             /* variance vector in square matrix form-user provided as 1D vector */
static sqrmat *corr_sq_mat;     /* correlation matrix - user provided*/
static sqrmat *inv_psi_mat;     /* inverse of the psi variance vector */
static sqrmat *i_mat;           /* identity matrix */

static void InitGlobals (int npts);
static void FreeGlobals (void);
static void ModelSearch(int p, char **roilabels);
static void GrowAllModels(char **roilabels);
static void GrowModel(int, int, int, int *, int *, int, double *, sqrmat *, sqrmat *);

static double ComputeThetawithPowell(void);
static sqrmat *ComputeInvSigma(void);
static sqrmat *ComputeInvSigmaExp(sqrmat *invSigma);
static double SEM_cost_fun(sqrmat *thetamat);

static void fill_kmat(int n,double *x);
static void fill_theta(int nx,double *x);
static void UpdatethetawithArray(int *thetavec, sqrmat *theta0_mat);
static void UpdateArraywiththeta(int *thetavec, sqrmat *theta0_mat);

static double Compute_chisq0(int Px);

static void im_to_sqrmat(MRI_IMAGE *mri_im, sqrmat *smat);
static void LTtoSymSM(sqrmat *smat);
static double comp_lndet_diagsm(sqrmat *smat);
static int count_nonzero_sm(sqrmat *smat);
static int count_value_sm(sqrmat *smat, double dval);

static double ln_det_sym(sqrmat *smat);
static sqrmat *inv_diag_sm(sqrmat *smat);
static sqrmat *sm_inverse(sqrmat *smat);

static char ** read_labels(char * file_str, int nrows);
static char * my_fgets( char *buf , int size , FILE *fts );

static void printarray(int *iar, int n);

int
main (int argc, char *argv[])
{
  char *theta_file_str=NULL, *corr_file_str=NULL, *psi_file_str=NULL;
  MRI_IMAGE *theta_init_matrix = NULL, *corr_matrix=NULL, *psi_vector=NULL;
  int Px,Py, Cx, Cy, nlabels;
  int nopt;
  int i, n;
  float *imptr;
  double *mat;
  char **roilabels=NULL;
  double chisq, cost;
  int calccost = 0;
  
   /*----- Read command line -----*/
  if (argc < 2 || strcmp (argv[1], "-help") == 0)
    {
      printf ("Usage: 1dSEM [options] -theta 1dfile -C 1dfile -psi 1dfile -DF nn.n\n"
              "Computes path coefficients for connection matrix in Structural Equation\n"
              "    Modeling (SEM)\n"
              " The program takes as input :\n"
              "    1. A 1D file with an initial representation of the connection matrix\n"
              "       with a 1 for each interaction component to be modeled and a 0 if\n"
              "       if it is not to be modeled. This matrix should be PxP rows and column\n"
              "    2. A 1D file of the C, correlation matrix, also with dimensions PxP\n"
              "    3. A 1D file of the residual variance vector, psi\n"
              "    4. The degrees of freedom, DF\n\n"
              "    Output is printed to the terminal and may be redirected to a 1D file\n"
              "    The path coefficient matrix is printed for each matrix computed\n"
              " Options:\n"
              "   -theta file.1D = connection matrix 1D file with initial representation\n"
              "   -C file.1D = correlation matrix 1D file\n"
              "   -psi file.1D = residual variance vector 1D file\n"
              "   -DF nn.n = degrees of freedom\n"
              "   -max_iter n = maximum number of iterations for convergence (Default=10000).\n"
              "    Values can range from 1 to any positive integer less than 10000.\n"
              "   -nrand n = number of random trials before optimization (Default = 100)\n"
              "   -limits m.mmm n.nnn = lower and upper limits for connection coefficients\n"
              "    (Default = -1.0 to 1.0)\n"
              "   -calccost = no modeling at all, just calculate the cost function for the\n"
              "    coefficients as given in the theta file. This may be useful for verifying\n"
              "    published results\n"
              "   -verbose nnnnn = print info every nnnnn steps\n\n"
              " Model search options:\n"
              " Look for best model. The initial connection matrix file must follow these\n"
              "   specifications. Each entry must be 0 for entries excluded from the model,\n"
              "   1 for each required entry in the minimum model, 2 for each possible path\n"
              "   to try.\n"
              "   -tree_growth or \n"
              "   -model_search = search for best model by growing a model for one additional\n"
              "    coefficient from the previous model for n-1 coefficients. If the initial\n"
              "    theta matrix has no required coefficients, the initial model will grow from\n"
              "    the best model for a single coefficient\n"
              "   -max_paths n = maximum number of paths to include (Default = 1000)\n"
              "   -stop_cost n.nnn = stop searching for paths when cost function is below\n"
              "    this value (Default = 0.1)\n"
              "   -forest_growth or \n"
              "   -grow_all = search over all possible models by comparing models at\n"
              "    incrementally increasing number of path coefficients. This\n"
              "    algorithm searches all possible combinations; for the number of coeffs\n"
              "    this method can be exceptionally slow, especially as the number of\n"
              "    coefficients gets larger, for example at n>=9.\n"
              "   -leafpicker = relevant only for forest growth searches. Expands the search\n"
              "    optimization to look at multiple paths to avoid local minimum. This method\n"
              "    is the default technique for tree growth and standard coefficient searches\n"
              " This program uses a Powell optimization algorithm to find the connection\n"
              "   coefficients for any particular model.\n\n"
              " References:\n"
              "   Powell, MJD, \"The NEWUOA software for unconstrained optimization without\n"
              "    derivatives\", Technical report DAMTP 2004/NA08, Cambridge University\n"
              "    Numerical Analysis Group -- http://www.damtp.cam.ac.uk/user/na/reports.html\n\n"
              "   Bullmore, ET, Horwitz, B, Honey, GD, Brammer, MJ, Williams, SCR, Sharma, T,\n"
              "    How Good is Good Enough in Path Analysis of fMRI Data?\n"
              "    NeuroImage 11, 289-301 (2000)\n\n"
              "   Stein, JL, et al., A validated network of effective amygdala connectivity,\n"
              "    NeuroImage (2007), doi:10.1016/j.neuroimage.2007.03.022\n\n"
              " The initial representation in the theta file is non-zero for each element\n"
              "   to be modeled. The 1D file can have leading columns for labels that will\n"
              "   be used in the output. Label rows must be commented with the # symbol\n"
              " If using any of the model search options, the theta file should have a '1' for\n"
              "   each required coefficient, '0' for each excluded coefficient, '2' for an\n"
              "   optional coefficient. Excluded coefficients are not modeled. Required\n"
              "   coefficients are included in every computed model.\n\n"
              " N.B. - Connection directionality in the path connection matrices is from \n"
              "   column to row of the output connection coefficient matrices.\n\n"
              "   Be very careful when interpreting those path coefficients.\n"
              "   First of all, they are not correlation coefficients. Suppose we have a\n"
              "   network with a path connecting from region A to region B. The meaning\n"
              "   of the coefficient theta (e.g., 0.81) is this: if region A increases by \n"
              "   one standard deviation from its mean, region B would be expected to increase\n"
              "   by 0.81 its own standard deviations from its own mean while holding all other\n"
              "   relevant regional connections constant. With a path coefficient of -0.16, \n"
              "   when region A increases by one standard deviation from its mean, region B \n"
              "   would be expected to decrease by 0.16 its own standard deviations from its\n"
              "   own mean while holding all other relevant regional connections constant.\n\n"
              "   So theoretically speaking the range of the path coefficients can be anything,\n"
              "   but most of the time they range from -1 to 1. To save running time, the\n"
              "   default values for -limits are set with -1 and 1, but if the result hits\n"
              "   the boundary, increase them and re-run the analysis.\n\n"
              " Examples:\n"
              "   To confirm a specific model:\n"
              "    1dSEM -theta inittheta.1D -C SEMCorr.1D -psi SEMvar.1D -DF 30\n"
              "   To search models by growing from the best single coefficient model\n"
              "     up to 12 coefficients\n"
              "    1dSEM -theta testthetas_ms.1D -C testcorr.1D -psi testpsi.1D \\ \n"
              "    -limits -2 2 -nrand 100 -DF 30 -model_search -max_paths 12\n"
              "   To search all possible models up to 8 coefficients:\n"
              "    1dSEM -theta testthetas_ms.1D -C testcorr.1D -psi testpsi.1D \\ \n"
              "    -nrand 10 -DF 30 -stop_cost 0.1 -grow_all -max_paths 8 | & tee testgrow.txt\n\n"
              "   For more information, see http://afni.nimh.nih.gov/sscc/gangc/PathAna.html\n"
              "    and our HBM 2007 poster at\n"
              "   http://afni.nimh.nih.gov/sscc/posters/file.2007-06-07.0771819246\n"
              " If you find this program useful, please cite:\n"
              "   G Chen, DR Glen, JL Stein, AS Meyer-Lindenberg, ZS Saad, RW Cox,\n"
              "   Model Validation and Automated Search in FMRI Path Analysis:\n"
              "   A Fast Open-Source Tool for Structural Equation Modeling,\n"
              "   Human Brain Mapping Conference, 2007\n"
              "\n");

      exit (0);
    }

  mainENTRY ("1dSEM main");
  machdep ();
  AFNI_logger ("1dSEM", argc, argv);
  PRINT_VERSION("1dSEM") ; AUTHOR("Daniel Glen, Gang Chen") ;
  putenv("AFNI_1D_ZERO_TEXT=YES") ;
  nopt = 1;
  while (nopt < argc && argv[nopt][0] == '-')
    {

      if( strcmp(argv[nopt],"-theta") == 0 ){
         theta_file_str = argv[++nopt] ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-C") == 0 ){
         corr_file_str = argv[++nopt] ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-psi") == 0 ){
         psi_file_str = argv[++nopt] ;
         nopt++ ; continue ;
      }

      if(strcmp(argv[nopt], "-DF") == 0 ) {
           if(++nopt >=argc ){
              ERROR_exit("Error - need an argument after -DF!");
           }
         DF =  strtod(argv[nopt], NULL);
         nopt++; continue;
      }

      if (strcmp (argv[nopt], "-max_iter") == 0)
        {
           if(++nopt >=argc ){
              ERROR_exit("Error - need an argument after -max_iter!");
           }
           max_iter = strtol(argv[nopt], NULL, 10);
           if ((max_iter <-1)||(max_iter>100000)) {
              ERROR_exit("Error - max_iter must be between -1 and 100,000");
           }
          nopt++;
          continue;
        }
      if (strcmp (argv[nopt], "-nrand") == 0)
        {
           if(++nopt >=argc ){
              ERROR_exit("Error - need an argument after -nrand!");
           }
           nrand = strtol(argv[nopt], NULL, 10);
           if ((nrand <0)||(nrand>100000)) {
              ERROR_exit("Error - nrand must be between 0 and 100,000");
           }
          nopt++;
          continue;
        }
     if (strcmp (argv[nopt], "-limits") == 0) {
           if(argc < (nopt+3)){
              ERROR_exit("*** Error - need two arguments after -limits!");
           }
           theta_ll = strtod(argv[++nopt], NULL);
           theta_ul = strtod(argv[++nopt], NULL);
           if(theta_ul <= theta_ll) {
              ERROR_exit("*** Error - limits can not be equal, and lower must be less than upper limit!");
           }
           nopt++;
           continue;
       }
        
     if (strcmp (argv[nopt], "-verbose") == 0)
        {
           if(++nopt >=argc ){
              ERROR_exit("*** Error - need an argument after -verbose!");
           }
           verbose = strtol(argv[nopt], NULL, 10);
           if (verbose<=0) {
              ERROR_exit("Error - verbose steps must be a positive number !");
           }
          nopt++;
          continue;
        }

      if((strcmp(argv[nopt], "-model_search") == 0 )||   \
         (strcmp(argv[nopt], "-tree_growth") == 0 ) ) {
         model_search = 1;
         nopt++; continue;
      }

      if((strcmp(argv[nopt], "-grow_all") == 0 ) ||      \
         (strcmp(argv[nopt], "-forest_growth") == 0 ) ) {
         grow_all = 1;
         model_search = 1;
         nopt++; continue;
      }

      if (strcmp (argv[nopt], "-max_paths") == 0)
        {
           if(++nopt >=argc ){
              ERROR_exit("Error - need an argument after -max_paths!");
           }
           max_paths = strtol(argv[nopt], NULL, 10);
           if (max_paths <=0 ) {
              ERROR_exit("Error - max_paths must be greater than 0");
           }
          nopt++;
          continue;
        }

      if (strcmp (argv[nopt], "-stop_cost") == 0)
        {
           if(++nopt >=argc ){
              ERROR_exit("Error - need an argument after -stop_cost!");
           }
           stop_cost = strtod(argv[nopt], NULL);
           if (stop_cost <= 0.0 ) {
              ERROR_exit("Error - stop_cost must be greater than 0");
           }
          nopt++;
          continue;
        }

      if(strcmp(argv[nopt], "-leafpicker") == 0) {
         leafpicker = 1;
         nopt++; continue;
      }

      if(strcmp(argv[nopt], "-calccost") == 0) {
         calccost = 1;
         nopt++; continue;
      }

      ERROR_exit("Error - unknown option %s", argv[nopt]);
    }

   /*----- read input datasets -----*/
   if((theta_file_str==NULL)||(corr_file_str==NULL)||(psi_file_str==NULL)||(DF==0)) {
       ERROR_exit("Need to specify all three 1D files for theta,C,psi and specify DF");
   }

   /* read initial theta matrix 1D file */
   theta_init_matrix = mri_read_1D (theta_file_str);
   if (theta_init_matrix == NULL)   {
      ERROR_exit("Error reading initial theta matrix file");
    }

   Px = theta_init_matrix->nx; Py = theta_init_matrix->ny;
   
   if ((Px < 2) || (Px > Py))
    {
      mri_free (theta_init_matrix);
      ERROR_message("Error - Not enough columns and rows or rows not equal to columns");
      ERROR_exit(" %d rows and %d columns found in theta matrix file", Px, Py);
    }
  
   /* labels may be at beginning of each line, so extract thetas from 1D */
   /* extract theta init matrix separately */
   INIT_SQRMAT(theta_init_mat, Px);   /* assume at least x rows are correct */
   im_to_sqrmat(theta_init_matrix, theta_init_mat);   /* copy from mri_image structure to square matrix */
   if(Px!=Py) {
      nlabels = Py-Px;
      roilabels = read_labels(theta_file_str, Px);
   }

/*   if(verbose){*/
      if(roilabels)
         DUMP_SQRMAT_LABELED("Initial Theta Setup Matrix", theta_init_mat,roilabels);
      else
         DUMP_SQRMAT("Initial Theta Setup Matrix", theta_init_mat);
      INFO_message("\n");
/*   }*/
         
   ntheta = count_nonzero_sm(theta_init_mat);
   if((ntheta<2)&&(!model_search))
       ERROR_exit("Must have at least two connection path coefficients to estimate");
   if(verbose)
       INFO_message("ntheta, number of non-zero elements in connection path coefficients to calculate = %d\n",ntheta);
   
   /* read Correlation matrix 1D file */
   corr_matrix = mri_read_1D (corr_file_str);
   if (corr_matrix == NULL)   {
      ERROR_exit("Error reading correlation matrix file");
    }

   Cx = corr_matrix->nx; Cy = corr_matrix->ny;
   
   if ((Cx != Px) || (Cx != Cy))
    {
      mri_free (corr_matrix);
      mri_free (theta_init_matrix);
      ERROR_message("Error - columns and rows in theta and correlation matrix files must match");
      ERROR_exit(" %d, %d rows and columns found in correlation matrix file", Cx, Cy);
    }
   INIT_SQRMAT(corr_sq_mat, Px);   /* assume at least y columns are correct */
   im_to_sqrmat(corr_matrix, corr_sq_mat);   /* copy from mri_image structure to square matrix */
   if(verbose)
      DUMP_SQRMAT("Correlation Matrix", corr_sq_mat);
   LTtoSymSM(corr_sq_mat); /* symmetrize correlation matrix in case it isn't */
   if(verbose)
      DUMP_SQRMAT("Correlation Matrix - symmetrized", corr_sq_mat);

   /* read psi vector 1D file */
   psi_vector = mri_read_1D (psi_file_str);
   if (psi_vector == NULL)   {
      ERROR_exit("Error reading psi vector file");
    }
   Cx = psi_vector->nx; Cy = psi_vector->ny;
  
   if ((Cx != Px) || (Cy != 1))
    {
      mri_free (psi_vector);
      mri_free (corr_matrix);
      mri_free (theta_init_matrix);
      ERROR_message("Error - only 1 column in Psi vector file"
                    "allowed and rows must match");
      ERROR_exit(" %d, %d rows and columns found in psi vector file", Cx, Cy);
    }

   INIT_SQRMAT(psi_mat, Px);   /* make square matrix from Psi vector */
   imptr = MRI_FLOAT_PTR (psi_vector);
   mat =  psi_mat->mat;
   n = Px;
   for(i=0;i<Px;i++)
     MAT(i,i) = (double) *(imptr + i);
   if(verbose)
      DUMP_SQRMAT("Psi Matrix", psi_mat);
   InitGlobals (Px);        /* initialize all the matrices and vectors */

   INFO_message("Connection directionality is from column to row");
   if(calccost) {          /* optionally compute cost of input coefficients */
      cost = SEM_cost_fun(theta_init_mat);
      INFO_message("Cost for given coefficient matrix is %g\n", cost);
      chisq = cost * (DF-1.0);   /* compute chi square statistic */
      INFO_message("Chi Square = %g\n", chisq);

      if(roilabels)
         DUMP_SQRMAT_LABELED("Given Connection coefficients", kmat,roilabels);
      else
         DUMP_SQRMAT("Given Connection coefficients", kmat);
      INFO_message("-----------------------------------------"
                   "--------------------------------------\n\n");

   }

   if(model_search) {
      if(grow_all)
         GrowAllModels(roilabels);   
      else
         ModelSearch(Px,roilabels);
   }         
   else {
         cost  = ComputeThetawithPowell();  /* calculate connection coefficients */
         INFO_message("Cost is %g\n", cost);
         chisq = cost * (DF-1.0);   /* compute chi square statistic for minimum fit */
         INFO_message("Chi Square = %g\n", chisq);

         if(roilabels)
            DUMP_SQRMAT_LABELED("Connection coefficients", kmat,roilabels);
         else
            DUMP_SQRMAT("Connection coefficients", kmat);
   }


   if(roilabels){
     for(i=0;i<Px;i++)
        if(roilabels[i])
           free(roilabels[i]);
     free(roilabels);
   } 

   FreeGlobals ();

   mri_free (psi_vector);
   mri_free (corr_matrix);
   mri_free (theta_init_matrix);

   exit (0);
}

/* search for the top model */
static void ModelSearch(int p, char **roilabels)
{
   int j,k,n, nelements, max_i,max_j;
   int i, nmodels, maxmodel, kk, nreq;
   double eta, temp, dfdt, d2fdt2, temp2;
   sqrmat *invsigma, *sigma, *Si_mat, *newsigma, *invsigmaexp;
   sqrmat *tempmat, *tempmat2, *tempmat3, *newinvsigma;
   double maxMI, MI, cost, mincost, chisq0, chisq, pfi, aic;
   double *mat, *nat;
   
   ENTRY("ModelSearch");
   maxmodel = max_paths;
   cost = HUGENUMBER;
   nelements = p*(p-1);
/*   nmodels = (long long)pow(2.0,nelements); */  /* maximum number of models to try */
   nmodels = count_value_sm(theta_init_mat, 2.0);
   INFO_message("Number of non-required coefficients %d\n", nmodels);
   n = kmat->n;
   mat = kmat->mat;
   nat = theta_init_mat->mat;
   eta = 0.0001;   /* perturbation amount */
   
   if(nmodels>maxmodel)      /* limit number of models to some maximum */
      nmodels = maxmodel;
      
   kk = n*(n+1)/2;
   ntheta = count_nonzero_sm(theta_init_mat);

   nreq = count_value_sm(theta_init_mat, 1.0);
   INFO_message("Number of required coefficients is %d\n", nreq);

   if(nreq!=0) {
      cost = ComputeThetawithPowell();  /* calculate connection coefficients - minimum model */
      INFO_message("cost = %g\n", cost);
      chisq0 = cost * (DF - 1.0);
      if(roilabels)
         DUMP_SQRMAT_LABELED("Connection coefficients - minimum model", kmat,roilabels);
      else
         DUMP_SQRMAT("Connection coefficients - minimum model", kmat);
      INFO_message("Chi Square 0 = %f  for minimum model\n", chisq0);
      INFO_message("-------------------------------------------------------------------------------\n\n");

   }

   /* will use chisq0 for null model instead of minimum model */
   chisq0 = Compute_chisq0(n);   /* compute chi square for null model too*/
   INFO_message("Chi Square 0 = %f  for null model\n", chisq0);
   INFO_message("-------------------------------------------------------------------------------\n\n");
   for(i=0;(i<nmodels)&&(cost>stop_cost);i++) {     /* for all possible combinations or maximum */
      invsigma = ComputeInvSigma();  /* more efficient and safer to calculate inverse first */
      /* use inverse of the inverse sigma matrix for nice symmetric matrix */  
      sigma = sm_inverse(invsigma);  
      /* sigma^-1*(sigma - C)*Sigma^-1 */
      invsigmaexp = ComputeInvSigmaExp(invsigma);
      maxMI = -HUGENUMBER; max_i = -1; max_j = -1; mincost = HUGENUMBER;
      for(j=0;j<p;j++) {  /* add an element to the current model */
         for(k=0;k<p;k++) { 
            if(NAT(j,k)==2.0) { /* allow user to exclude elements and don't do already modeled elements */

               temp2 = MAT(j,k);
               NAT(j,k) = 1.0;            
               cost = ComputeThetawithPowell();  /* recompute the optimization */ 
               if(cost<mincost) {
                   mincost = cost;
                   max_i = j;
                   max_j = k;
                  }
 #if 0
                temp = MAT(j,k);
               MAT(j,k)+= eta;
               newinvsigma = ComputeInvSigma();
               newsigma = sm_inverse(newinvsigma);
               MAT(j,k) = temp;
               Si_mat = sm_subtract(newsigma,sigma);  /* perturbation matrix */
               sm_scale(Si_mat, 1.0/eta, 0);       /* Si = (sigma(thetai+deltai) - sigma)/eta */
               tempmat = sm_mult(invsigmaexp, Si_mat);
               dfdt = sm_trace(tempmat) / 2.0;
               KILL_SQRMAT(tempmat);
               
               tempmat = sm_mult(invsigma, Si_mat);
               tempmat2 = sm_mult(tempmat, invsigma);
               tempmat3 = sm_mult(tempmat2, Si_mat);
               d2fdt2 = sm_trace(tempmat3) / 2.0;
               MI = (dfdt * dfdt / d2fdt2) / 2.0;
               if(MI>maxMI) {
                  maxMI = MI;
                  max_i = j;
                  max_j = k;
               }          
               KILL_SQRMAT(tempmat);
               KILL_SQRMAT(tempmat2);
               KILL_SQRMAT(tempmat3);
#endif               
               
               NAT(j,k) = 2.0;
               MAT(j,k) = temp2;
               ntheta = count_nonzero_sm(theta_init_mat);

            }
         }
      }
      if((max_i<0)||(max_j<0)) {
         INFO_message("Error finding maximum direction\n");
         EXRETURN;
      } 
      NAT(max_i, max_j) = 1.0;  /* this was the best estimated model to add in this iteration */
      cost = ComputeThetawithPowell();  /* recompute the optimization */
      
      chisq = cost * (DF-1.0);
      INFO_message("\n");
      INFO_message("Growing model at row,col = %d, %d\n", max_i, max_j); 
      if(roilabels)
         INFO_message(" %s -> %s\n", roilabels[max_j], roilabels[max_i]);
      INFO_message("   with new cost = %g, chisq = %g, ntheta = %d\n",
                       cost, chisq, ntheta);

      pfi = 1.0 - (chisq/(kk-ntheta))*kk/chisq0;    /* parsimonious fit index */
      aic = chisq + (ntheta+ntheta);                /* Akaike's information criterion */
      INFO_message("parsimonious fit index = %g   Akaike's Information Criterion = %g\n", pfi, aic);
      if(roilabels)
         DUMP_SQRMAT_LABELED("Connection coefficients", kmat,roilabels);
      else
         DUMP_SQRMAT("Connection coefficients", kmat);
      INFO_message("-------------------------------------------------------------------------------\n\n");
    }
}

static void
GrowAllModels(char **roilabels)
{
   int *thetavec, *minvec;
   double mincost, cost;
   int stopdepth, npts, notheta, maxdepth, depth, ntheta0;
   sqrmat *theta0_mat, *bestkmat;   
   double chisq0, chisq, pfi, aic;
   
   ENTRY("GrowAllModels");

   ntheta0 = count_nonzero_sm(theta_init_mat); /* how many points required in original model */
   notheta = count_value_sm(theta_init_mat, 0.0);  /* how many points excluded with diagonal */
   npts = theta_init_mat->n; 
   npts = npts*npts - notheta;  /* size of vector is number of non-diagonal elements */

   if(ntheta0!=0) {
      cost = ComputeThetawithPowell();  /* calculate connection coefficients - minimum model */
      INFO_message("cost = %g\n", cost);
      chisq0 = cost * (DF - 1.0);
      if(roilabels)
         DUMP_SQRMAT_LABELED("Connection coefficients", kmat,roilabels);
      else
         DUMP_SQRMAT("Connection coefficients", kmat);
      INFO_message("Chi Square 0 = %f  for minimum model\n", chisq0);
   }

   chisq0 = Compute_chisq0(theta_init_mat->n);   /* compute chi square for null model */
   INFO_message("Chi Square 0 = %f  for null model\n", chisq0);

   theta0_mat = sm_copy(theta_init_mat);   /* create temporary copy of matrix */
   INIT_SQRMAT(bestkmat,kmat->n);       /* path coefficients - where final results go*/

   thetavec = malloc(npts * sizeof(int));
   minvec = malloc(npts * sizeof(int));
   if((thetavec==NULL)||(minvec==NULL)||(theta0_mat==NULL)||(bestkmat==NULL)) {
     ERROR_exit("Can not allocate memory for growing all models");
     EXRETURN;
   }
   UpdateArraywiththeta(thetavec,theta0_mat);
   mincost = HUGENUMBER;
   maxdepth = npts;
   if(max_paths<maxdepth)
      maxdepth = max_paths;
   INFO_message("Min. coeffs. %d maxdepth %d npts %d\n", ntheta0, maxdepth, npts);
   INFO_message("-------------------------------------------------------------------------------\n\n");

   for(stopdepth = ntheta0; stopdepth < maxdepth; stopdepth++) {
      mincost = HUGENUMBER;   /* find the best at each depth even if it's the same as lesser depth */
      GrowModel(-1,ntheta0,stopdepth, thetavec, minvec, npts, &mincost, theta0_mat, bestkmat);
      /* best k kept in bestkmat */
      cost = mincost;
      chisq = cost * (DF-1.0);
      depth = stopdepth + 1;
      INFO_message("New model computed with %d coefficients\n", depth);
      INFO_message(" cost = %g chisq = %g\n",cost, chisq);
      pfi = 1.0 - (chisq/(depth-ntheta0))*depth/chisq0;    /* parsimonious fit index */
      aic = chisq + (depth+depth);                /* Akaike's information criterion */
      INFO_message("parsimonious fit index = %g   Akaike's Information Criterion = %g\n", pfi, aic);
      if(roilabels)
         DUMP_SQRMAT_LABELED("Connection coefficients", bestkmat,roilabels);
      else
         DUMP_SQRMAT("Connection coefficients", bestkmat);
      INFO_message("-------------------------------------------------------------------------------\n\n");
   }

   EQUIV_SQRMAT(bestkmat,kmat);
   free(minvec); free(thetavec);
   minvec = NULL; thetavec = NULL;
   KILL_SQRMAT(bestkmat);
   KILL_SQRMAT(theta0_mat);
   EXRETURN;
}

static void
GrowModel(int lastindex, int depth, int stopdepth, int *thetavec, int *minvec, int npts,\
          double *mincost, sqrmat *theta0_mat, sqrmat *bestkmat)
{
   int start, ii, temp;
   double cost;
   
   ENTRY("GrowModel");
   start = lastindex + 1;  /* start at node right after previous index */
   if(start>=npts)
      EXRETURN;

   for(ii=start; ii<npts;ii++){
      temp = thetavec[ii];
      thetavec[ii] = 1;
      if(depth==stopdepth) {
         UpdatethetawithArray(thetavec,theta0_mat);
         cost = ComputeThetawithPowell();
         if(cost<*mincost) {
            *mincost = cost;
            memcpy(minvec, thetavec, npts*sizeof(int));
            EQUIV_SQRMAT(kmat,bestkmat);
         }
        }
      else {
         GrowModel(ii,depth+1,stopdepth, thetavec, minvec, npts, mincost, theta0_mat, bestkmat);
      }
      thetavec[ii] = temp;
   }
   EXRETURN;
}

static void
printarray(int *iar, int n)
{
   int i;

   for(i=0;i<n;i++)
      printf("%d ",iar[i]);
   printf("\n");
}

/*! allocate all the global matrices and arrays once */
static void
InitGlobals (int npts)
{
  double lndetpsi, lndetC;
  
  ENTRY ("InitGlobals");

  INIT_SQRMAT(kmat, npts);       /* path coefficients - where final results go*/
  i_mat = sm_identity(npts);      /* identity matrix */
  lndetpsi = comp_lndet_diagsm(psi_mat);  
  lndetC = ln_det_sym(corr_sq_mat);
  F_fixed = lndetpsi - lndetC - npts;   /* fixed components of cost function*/
  inv_psi_mat  = inv_diag_sm(psi_mat);   /* compute inverse of diagonal psi matrix */
  if(inv_psi_mat==NULL) {
      ERROR_exit("Can not compute inverse of variance vector");
  }
  EXRETURN;
}

/*! free up all the matrices and arrays */
static void
FreeGlobals ()
{
  ENTRY ("FreeGlobals");
  KILL_SQRMAT(kmat);
  KILL_SQRMAT(i_mat);
  KILL_SQRMAT(inv_psi_mat);
  KILL_SQRMAT(psi_mat);
  KILL_SQRMAT(corr_sq_mat);
  KILL_SQRMAT(theta_init_mat);

  EXRETURN;
}

/* function called at each optimization step */
double SEM_Powell_cost_fun(int n, double *x)
{

/* n is the number of parameters to test */
/* The cost function for the SEM,
     F = ln (det[Sigma]) + trace(C Sigma^-1) - ln (det[C] ) - P 
   Sigma varies with the parameters of interest here
   Sigma = (I - K)^-1 Psi [(I-K)^-1]'
     where K is the connection matrix we're trying to find
   C is the estimated correlation matrix and is calculated
     before finding the connection matrix. 
   Efficiency is not very important for that computation.
*/
   static int ncall=0;
   
   static double F = 0.0; /* the cost (error) using these parameters,x */
   double lndet_kkt, C_inv_sigma;
   sqrmat *inv_sigma;
   
   if(n==0) {
      ncall = 0;   /* reset iteration counter */
      return(F);   /* return last computed cost too (as a bonus) */
    }
   /* Sigma  = (I-K)^-1 Psi [(I-K)^-1]'  - not actually needed though */

   /* convert doubles from optimizer pointer to sqrmat (matrix) format */
   fill_kmat(n,x);   /* put thetas in right places in k matrix */

/* DUMP_SQRMAT("kmat",kmat);*/

   /* Sigma^-1 = (I-K)^T Psi^-1 (I-K)   - this we need*/
   inv_sigma = ComputeInvSigma();
 
   /* ln det[Sigma] = ln det[Psi] - ln det[I-K-K'+KK'] */
   /* ln det[Psi] = sum_i(log(Psi_ii)) */ /* calculated once */
   lndet_kkt = sm_lndet_iktk(kmat);
    
     /* trace [C Sigma^-1]
       Sigma^-1 = (I-K)^T Psi^-1 (I-K)
       Psi^-1 = [(1/Psi1, 1/Psi2, 1/Psi3, ..., 1/Psim]   diagonal or vector  calculated once
       trace [C Sigma^-1] = C . Sigma^-1    large dot product - element wise multiplication and sum
     */
       
/*   F = lndetpsi - lndet_kkt + trace(C.Sigma^-1) - ln(det[C]) - P */
/*   F = trace(C.Sigma^-1) - lndet_kkt + lndet_psi - ln(det[C]) - P */
/* last three terms do not vary with theta, so calculated only once */
 
   C_inv_sigma = sm_dot(corr_sq_mat, inv_sigma);
   F =  F_fixed - lndet_kkt + C_inv_sigma; 
   /* free up all the temporary matrices */
   KILL_SQRMAT(inv_sigma);
   if(verbose&&(!(ncall%verbose)))   /* show verbose messages every verbose=n voxels */
      {
         INFO_message("Iteration %d: cost %f\n",ncall,F);
      }
   ncall++;
   
   return(F);
}
 

/* compute cost for a given coefficient matrix */
static double
SEM_cost_fun(sqrmat *thetamat)
{
   static double F = 0.0; /* the cost (error) using these parameters,x */
   double lndet_kkt, C_inv_sigma;
   sqrmat *inv_sigma;
 
   /* *kmat = *thetamat; */   /* copy structure of initial theta matrix to kmat */
   EQUIV_SQRMAT(thetamat, kmat);
   /* Sigma^-1 = (I-K)^T Psi^-1 (I-K)   - this we need*/
   inv_sigma = ComputeInvSigma();
 
   /* ln det[Sigma] = ln det[Psi] - ln det[I-K-K'+KK'] */
   /* ln det[Psi] = sum_i(log(Psi_ii)) */ /* calculated once */
   lndet_kkt = sm_lndet_iktk(kmat);
    
   C_inv_sigma = sm_dot(corr_sq_mat, inv_sigma);
   F =  F_fixed - lndet_kkt + C_inv_sigma; 
   /* free up any temporary matrices */
   KILL_SQRMAT(inv_sigma);
   
   return(F);
}
 


/*! compute using optimization method by Powell, 2004*/
static double ComputeThetawithPowell() /*compute connection matrix */
{
   double *x, *thetamin, *thetamax, cost;
   int i, icalls;

   ENTRY("ComputeThetawithPowell");
   ntheta = count_nonzero_sm(theta_init_mat);
   x = (double *)malloc(ntheta*sizeof(double)) ;
   thetamin = (double *)malloc(ntheta * sizeof(double));
   thetamax = (double *)malloc(ntheta * sizeof(double));
   if((x==NULL)||(thetamin==NULL)||(thetamax==NULL)) {
      ERROR_exit("Error - Can not allocate memory for constraints!");
   }
   
   /* set up lower and upper limits for search */
   for(i=0;i<ntheta;i++) {
      *(x+i) = 0.0;
      *(thetamin+i) = theta_ll;
      *(thetamax+i) = theta_ul;
   }
   fill_theta(ntheta, x);   /* put initial values from theta matrix */
   if(!model_search)
      INFO_message("Finding optimal theta values\n");
   SEM_Powell_cost_fun(0,x);  /* reset iteration counter */
   if(grow_all && !leafpicker) {
      /* use constrained Powell optimization - not multiple path version for speed */
      icalls = powell_newuoa_con(ntheta, x, thetamin, thetamax, nrand, 0.1, 0.0001, max_iter, SEM_Powell_cost_fun);
      cost = SEM_Powell_cost_fun(0,x); /* get final cost */
   }
   else {
      /* this version of Powell optimization is less susceptible to local minima by following multiple paths */
      icalls = powell_newuoa_constrained(ntheta, x, &cost, thetamin, thetamax, nrand, nrand*13, 5, 0.1, 0.0001, \
               max_iter, SEM_Powell_cost_fun);
   }
   fill_kmat(ntheta, x);  /* final fill of k matrix with thetas */
   
   free(thetamax);
   free(thetamin);
   free(x);
   if(!model_search)
      INFO_message("Total number of iterations %d\n", icalls+nrand);
   RETURN(cost);
}

/* compute inverse sigma */
static sqrmat * ComputeInvSigma()
{
   sqrmat *inv_sigma, *imk, *imkt, *tempmat;
   
   ENTRY("ComputeInvSigma");
    /* sigma = (I-K)^-1 Psi (I-K)^-T */
   /* Sigma^-1 = (I-K)^T Psi^-1 (I-K)   - this we need*/
   imk = sm_subtract(i_mat, kmat);

   imkt = sm_transpose(imk);
   tempmat = sm_mult(imkt, inv_psi_mat); /* psi is diagonal-vec multiply instead */

   inv_sigma = sm_mult(tempmat,imk);

   KILL_SQRMAT(tempmat);
   KILL_SQRMAT(imkt);
   KILL_SQRMAT(imk);
   
   RETURN(inv_sigma);
}

/*! compute expression containing inverse sigma */
/* sigma^-1*(sigma - C)*Sigma^-1 */
static sqrmat * ComputeInvSigmaExp(sqrmat *invsigma)
{
   sqrmat *smat, *tmat, *umat;
   
   ENTRY("ComputeInvSigmaExp");
   /* sigma^-1*(sigma - C)*Sigma^-1 */
   /* sigma^-1 - sigma^-1 C sigma^-1 */
   tmat = sm_mult(invsigma, corr_sq_mat);
   umat = sm_mult(tmat, invsigma);
   smat = sm_subtract(invsigma, umat);

   KILL_SQRMAT(umat);
   KILL_SQRMAT(tmat);
   
   RETURN(smat);
}

/*! put new thetas into K matrix according to user scheme */
static void fill_kmat(int nx,double *x)
/* n=number of x values to assign */
{
  double *thetaptr, *mat, *nat;
  int i,j,n, nfill, usespot;
  
  ENTRY("fill_kmat");
  thetaptr = x;
  nat = theta_init_mat->mat;
  mat = kmat->mat;
  n = kmat->n;
  nfill = 0;
  for(i=0;i<n;i++) {
     for(j=0;j<n;j++) {
        usespot = 0;
        if(!model_search) {
           if(NAT(i,j)!=0.0)
             usespot = 1;
         }
        else {
           if(NAT(i,j)==1.0)
              usespot = 1;
        }
        if(usespot)
            {
            MAT(i,j) = *thetaptr++;
            nfill++;
            if(nfill>nx)
               EXRETURN;
        }
        else
            MAT(i,j) = 0.0;
     }
  }
  
  EXRETURN;
}


/*! put thetas from initial theta matrix into vector */
static void fill_theta(int nx,double *x)
/* n=number of x values to assign */
{
  double *thetaptr, *mat, *nat;
  int i,j,n, nfill, usespot;
  
  ENTRY("fill_theta");
  thetaptr = x;
  nat = theta_init_mat->mat;
  mat = kmat->mat;
  n = kmat->n;
  nfill = 0;
  for(i=0;i<n;i++) {
     for(j=0;j<n;j++) {
        usespot = 0; 
        if(model_search) {   /* for model search, use values of 1 to model */
           if(NAT(i,j)==1.0)
              usespot = 1;
        }
        else {
           if(NAT(i,j)!= 0.0)
              usespot = 1;
        }
        if(usespot) {
            *thetaptr++ = MAT(i,j);
            nfill++;
            if(nfill>nx)
               EXRETURN;
        }
     }
  }
  
  EXRETURN;
}

/* refill theta_init_mat setup matrix with values in an array */
static void
UpdatethetawithArray(int *thetavec, sqrmat *theta0_mat)
{
   int i,j,n;
   double *mat, *nat;
   int *thetaptr;

   ENTRY("UpdatethetawithArray");
    
   thetaptr = thetavec;
   mat = theta_init_mat->mat;
   nat = theta0_mat->mat;   /* exclusion matrix */
   n = theta_init_mat->n;
   for(i=0;i<n;i++) {
      for(j=0;j<n;j++) {
         if(NAT(i,j)!=0.0) {   /* skip diagonals and other exclusions*/
            MAT(i,j) = (double) *thetaptr++;  /* thetavec has all included points 0 or 1 */
         }
      }
   }
   EXRETURN;
}

/* fill array with required values from matrix */
static void
UpdateArraywiththeta(int *thetavec, sqrmat *theta0_mat)
{
   int i,j,n;
   double *mat, *nat;
   int *thetaptr;

   ENTRY("UpdateArraywiththeta");
   thetaptr = thetavec;
   mat = theta0_mat->mat;   /* exclusion matrix */
   n = theta0_mat->n;
   for(i=0;i<n;i++) {
      for(j=0;j<n;j++) {
         if(MAT(i,j)!=0.0) {   /* skip diagonals and other exclusions*/
            if(MAT(i,j)==1.0)  /* set vector only for required coeffs*/
                 *thetaptr = 1;
            else
               *thetaptr = 0;
            thetaptr++;   
         }
      }
   }
   EXRETURN;
}

/* compute Chisq0 for no model (no coefficients) */
static double
Compute_chisq0(int npts)
{
   double chisq;
   double lndetpsi, lndetC, trCinvpsi;
  
   ENTRY ("Compute_chisq0");

   lndetpsi = comp_lndet_diagsm(psi_mat);  
   lndetC = ln_det_sym(corr_sq_mat);
   trCinvpsi = sm_dot(corr_sq_mat, inv_psi_mat);
   chisq = (DF-1.0) * (lndetpsi + trCinvpsi - lndetC - npts);
   RETURN(chisq); 
}


/*! copy 1D MRI_IMAGE to a square matrix structure */
/* square matrix must already exist */
static void im_to_sqrmat(MRI_IMAGE *mri_im, sqrmat *smat)
{
   int i,j,n,sqrwidth, imwidth, offset, imoffset;
   float *imptr;
   double *mat;
   
   n = sqrwidth = smat->n;
   imwidth = mri_im->ny;
   offset = imwidth - sqrwidth;
   imptr = MRI_FLOAT_PTR (mri_im);
   imptr += (offset*sqrwidth);  /* ignore leading column if any */
   mat =  smat->mat;
   
   for(i=0; i<sqrwidth;i++){
      for(j=0; j<sqrwidth;j++){
         imoffset = i*sqrwidth+j;
         MAT(j,i) = (double) *(imptr+imoffset);
      }
   }
}

#if 0
static void
dump_mri_im(MRI_IMAGE *mri_im)
{
   int i, Px, Py;
   float *imptr;
   
   Px = mri_im->nx;
   Py = mri_im->ny;
   imptr = MRI_FLOAT_PTR (mri_im);
   for(i=0;i<(Px*Py);i++) {
/*      for(j=0;j<Px;j++) {*/
         printf("%f ", *(imptr+i));
/*       }*/
       printf("\n");
    }
}
#endif

/*! copy lower triangular elements to upper triangle to make symmetric*/
static void 
LTtoSymSM(sqrmat *smat)
{
   int i, j, sqrwidth, n;
   double *mat;
   
   n = sqrwidth = smat->n;
   mat = smat->mat;

   for(i=0; i<sqrwidth;i++){
      for(j=i; j<sqrwidth;j++){
         MAT(i,j) = MAT(j,i);
      }
   }
}

/*! compute the ln det of a diagonal matrix 
  = sum of the logs of the diagonal */
static double
comp_lndet_diagsm(sqrmat *smat)
{
   int i, n;
   double *mat, diag_el;
   double sum = 0;
   
   ENTRY("comp_lndet_diagsm");
   n = smat->n;
   mat = smat->mat;
   for(i=0; i<n;i++){
         diag_el = MAT(i,i);
         if(diag_el<=0)
            RETURN(0);
         sum += log(diag_el);
   }
   RETURN(sum);
}

/*! count the number of non-zero terms in square matrix */
static int
count_nonzero_sm(sqrmat *smat)
{
   int i, j, n;
   int sum = 0;
   double *mat;
   
   ENTRY("count_nonzero_sm");
   n = smat->n;
   mat = smat->mat;

   for(i=0; i<n;i++){
      for(j=0; j<n;j++){
        if(MAT(i,j)) {
           if(!model_search)
                sum++;
           else 
              if(MAT(i,j) == 1.0)
                sum++;
        }
      }
   }

   RETURN(sum);
}

static int
count_value_sm(sqrmat *smat, double value)
{
   int i, j, n;
   int sum = 0;
   double *mat;
   
   ENTRY("count_value_sm");
   n = smat->n;
   mat = smat->mat;

   for(i=0; i<n;i++){
      for(j=0; j<n;j++){
           if(MAT(i,j) == value)
                    sum++;
      }
   }

   RETURN(sum);
}

/*! compute ln det of a symmetric matrix */
static double
ln_det_sym(sqrmat *smat)
{
   double sum = 0;
   sqrmat *tmat;   

   ENTRY("ln_det_sym");
   
   tmat = sm_copy(smat);   /* create temporary copy of matrix */
   sm_choleski(tmat);      /* get lower triangular choleski factor*/
   sum = comp_lndet_diagsm(tmat);  /* 2*sum(ln(Diag)) */
   sum += sum;
   KILL_SQRMAT(tmat);
   RETURN(sum);
}

/*! compute inverse of diagonal square matrix */
static sqrmat * 
inv_diag_sm(sqrmat *smat)
{
   sqrmat *tmat;   
   int i, n;
   double temp;
   double *mat, *nat;
   
   ENTRY("inv_diag_sym");
   
   n = smat->n;
   mat = smat->mat;
   tmat = sm_copy(smat);   /* create temporary copy of matrix */
   nat = tmat->mat;
   for(i=0;i<n;i++) {
      temp = MAT(i,i);
      if(temp==0.0) {   /* check for division by 0 */
         KILL_SQRMAT(tmat);
         RETURN(NULL);
      } 
      NAT(i,i) = 1.0/temp;
   }
      
   RETURN(tmat);
}

/*! find inverse of square matrix */
/* uses function in matrix.c by converting */
/* square matrix structure to matrix structure */
static sqrmat *
sm_inverse(sqrmat *smat)
{
   int i, j, n;
   sqrmat *tmat;
   matrix tempmat, invmat;
   double *mat;
   ENTRY("sm_inverse");
   n = smat->n;
   mat = smat->mat;

   matrix_initialize (&tempmat);
   matrix_create(n,n,&tempmat);
   for(i=0;i<n;i++)
      for(j=0;j<n;j++)
         tempmat.elts[i][j] = MAT(i,j);
   matrix_initialize (&invmat);
   matrix_inverse(tempmat, &invmat);
   INIT_SQRMAT(tmat, n);
   mat = tmat->mat;
   for(i=0;i<n;i++)
      for(j=0;j<n;j++)
        MAT(i,j) = invmat.elts[i][j];

   matrix_destroy(&invmat);
   matrix_destroy(&tempmat);
   RETURN(tmat);         
}

/*! Length of line buffer for mri_read_ascii() */
/* rcr - improve this */
#define LBUF 2524288  /* 08 Jul 2004: increased to 512K from 64K */
/* use first label on each line for ROI label for now */
static char ** read_labels(char * file_str, int nrows)
{
   char **roi_label;
   char *tempstr, *ptr;
   FILE *filehdl;
   int i;
   
   ENTRY("read_labels");
   roi_label = (char **) malloc(nrows*sizeof(char *));
   tempstr = malloc(1024);   /* allow up to 1024 bytes for a leading string */
   filehdl = fopen(file_str,"r");
   (void) my_fgets( NULL , 0 , NULL ) ;  /* reset [20 Jul 2004] */
  
   for(i=0;i<nrows;i++) {
         ptr = my_fgets( tempstr , LBUF , filehdl ) ;  /* read in non-comment line*/
         if( ptr==NULL || *ptr=='\0' ) break ; /* failure --> end of data */
         roi_label[i] = (char *) malloc(sizeof(char) * strlen(tempstr));
         sscanf(ptr,"%s",roi_label[i]);
    }
   fclose(filehdl);   
   RETURN(roi_label);
}



/* below taken directly from mri_read.c */
/*  for static function my_fgets */


/*! Free a buffer and set it to NULL */
#define FRB(b) do{ if( (b)!=NULL ){free((b)); (b)=NULL;} }while(0)

#undef USE_LASTBUF

/*---------------------------------------------------------------*/
/*! [20 Jun 2002] Like fgets, but also
     - skips blank or comment lines
     - skips leading and trailing whitespace
     - catenates lines that end in '\' (replacing '\' with ' ')
     - returns duplicate of last line if first 2
        nonblank input characters are "" [20 Jul 2004]
-----------------------------------------------------------------*/

static char * my_fgets( char *buf , int size , FILE *fts )
{
   char *ptr ;
   int nbuf , ll,ii , cflag ;
   static char *qbuf=NULL ;

#ifdef USE_LASTBUF
   static char *lastbuf = NULL ;   /* 20 Jul 2004 */
   static int  nlastbuf = 0 ;

   if( buf == NULL && lastbuf != NULL ){    /* 20 Jul 2004 */
     free((void *)lastbuf); lastbuf = NULL; nlastbuf = 0 ;
   }
#endif

   if( buf == NULL && qbuf != NULL ){ free((void *)qbuf); qbuf = NULL; }

   if( buf == NULL || size < 1 || fts == NULL ) return NULL ;

   if( qbuf == NULL ) qbuf = AFMALL(char, LBUF) ;  /* 1st time in */

   nbuf  = 0 ;  /* num bytes stored in buf so far */
   cflag = 0 ;  /* flag if we're catenating lines */

   while(1){   /* loop and read lines, creating a logical line */

     ptr = fgets( qbuf , LBUF , fts ) ; /* read next whole line */

     if( ptr == NULL ) break ;          /* must be end-of-file */

     /* skip leading whitespace */

     for( ; *ptr != '\0' && isspace(*ptr) ; ptr++ ) ; /* nada */

     /* skip entirely blank lines, unless we are catenating */

     if( *ptr == '\0' ){ if(cflag) break; else continue; }

#ifdef USE_LASTBUF
     /* if a duplicate is requested, return it now [20 Jul 2004] */

     if( *ptr == '"' && *(ptr+1) == '"' && nlastbuf > 0 && nbuf == 0 ){
       ll = strlen(lastbuf) ; if( ll >= size ) ll = size-1 ;
       memcpy(buf,lastbuf,ll-1) ; buf[ll] = '\0' ;
       return buf ;
     }
#endif

     /* skip comment lines (even if we are catenating) */

     if( *ptr == '#' || (*ptr == '/' && *(ptr+1) == '/') ) continue ;

     /* strip trailing whitespace */

     ll = strlen(ptr) ;                                  /* will be > 0 */
     for( ii=ll-1 ; isspace(ptr[ii]) && ii > 0 ; ii-- )  /* blank => NUL */
       ptr[ii] = '\0' ;

     ll = strlen(ptr) ;                 /* number of chars left */
     if( ll == 0 ) continue ;           /* should not happen */

     cflag = (ptr[ll-1] == '\\') ;      /* catenate next line? */
     if( cflag ) ptr[ll-1] = ' ' ;      /* replace '\' with ' ' */

     /* now copy what's left (ll+1 bytes) at tail of output buffer */

     if( nbuf+ll+1 > size ){   /* too much for output buffer? */
       ll = size - (nbuf+1) ;
       if( ll <= 0 ) break ;   /* should not happen */
     }

     memcpy(buf+nbuf,ptr,ll+1) ; nbuf += ll ;
     if( !cflag ) break ;

   } /* loop to get next line if catenation is turned on */

#ifdef LASTBUF
   /* make a copy of result in lastbuf [20 Jul 2004] */

   ll = strlen(buf) ;
   if( ll+1 > nlastbuf ){
     nlastbuf = ll+2 ; lastbuf = (char *)realloc((void *)lastbuf,nlastbuf) ;
   }
   memcpy(lastbuf,buf,ll+1) ;
#endif

   /* and we is done */

   if( nbuf > 0 ) return buf ;      /* return what we read already */
   return NULL ;                    /* signal of failure get data  */
}

