/*********************** 3dTeig.c **********************************************/
/* Author: Daniel Glen, 15 Nov 2004 */
#include "mrilib.h"
#include "thd_shear3d.h"

static char prefix[THD_MAX_PREFIX] = "eig" ;
static int datum                   = MRI_float ;
static void EIG_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void * ud , int nbriks, float * val ) ;

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   int nopt, nbriks, ii ;
   int addBriks = 0;
   int numMultBriks,methIndex,brikIndex;

   /*----- Read command line -----*/
   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTeig [options] dataset\n"
             "Computes eigenvalues and eigenvectors for an input dataset of\n"
             " 6 sub-bricks Dxx,Dxy,Dxz,Dyy,Dyz,Dzz.\n"
	     " The results are stored in a 13-subbrick bucket dataset.\n"
	     " The resulting 12-subbricks are\n"
	     "  lambda1,lambda2,lambda3,E11,E12,E13,E21,E22,E23,E31,E32,E33,FA.\n"
             "\n"
             "The output is a bucket dataset.  The input dataset\n"
             "may use a sub-brick selection list, as in program 3dcalc.\n"
	     " Fractional Anisotropy (FA) calculated according to Pierpaoli C, Basser PJ.\n"
             " Microstructural and physiological features of tissues elucidated by\n"
             " quantitative-diffusion tensor MRI, J Magn Reson B 1996; 111:209-19\n"
           ) ;
      printf("\n" MASTER_SHORTHELP_STRING ) ;
      exit(0) ;
   }

   mainENTRY("3dTeig main"); machdep(); AFNI_logger("3dTeig",argc,argv);

   nopt = 1 ;
   nbriks = 13 ;
   datum = MRI_float;
   while( nopt < argc && argv[nopt][0] == '-' ){

      /*-- prefix --*/

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"*** -prefix needs an argument!\n"); exit(1);
         }
         MCW_strncpy(prefix,argv[nopt],THD_MAX_PREFIX) ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"*** %s is not a valid prefix!\n",prefix); exit(1);
         }
         nopt++ ; continue ;
      }

      /*-- datum --*/

      if( strcmp(argv[nopt],"-datum") == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"*** -datum needs an argument!\n"); exit(1);
         }
         if( strcmp(argv[nopt],"short") == 0 ){
            datum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            datum = MRI_float ;
         } else if( strcmp(argv[nopt],"byte") == 0 ){
            datum = MRI_byte ;
         } else {
            fprintf(stderr,"-datum of type '%s' is not supported!\n",
                    argv[nopt] ) ;
            exit(1) ;
         }
         nopt++ ; continue ;
      }
   }

   /*----- read input dataset -----*/

   if( nopt >= argc ){
      fprintf(stderr,"*** No input dataset!?\n"); exit(1);
   }

   old_dset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(old_dset) ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[nopt]); exit(1);
   }

   /* expect 6 values per voxel - 6 sub-briks as input dataset */ 
   if( DSET_NVALS(old_dset) != 6 ){
      fprintf(stderr,"*** Can't use dataset that is not 6 values per voxel!\n") ;
      exit(1) ;
   }

   EDIT_dset_items( old_dset ,
                    ADN_ntt    , DSET_NVALS(old_dset) ,
                    ADN_ttorg  , 0.0 ,
                    ADN_ttdel  , 1.0 ,
                    ADN_tunits , UNITS_SEC_TYPE ,
                    NULL ) ;


   /*------------- ready to compute new dataset -----------*/

   new_dset = MAKER_4D_to_typed_fbuc(
                 old_dset ,             /* input dataset */
                 prefix ,               /* output prefix */
                 datum ,                /* output datum  */
                 0 ,                    /* ignore count  */
                 0 ,              /* can't detrend in maker function  KRH 12/02*/
                 nbriks ,               /* number of briks */
		 EIG_tsfunc ,         /* timeseries processor */
                 NULL                   /* data for tsfunc */
              ) ;

   if( new_dset != NULL ){
      tross_Copy_History( old_dset , new_dset ) ;
      EDIT_dset_items(new_dset, ADN_brick_label_one+0, "lambda_1", ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+1, "lambda_2",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+2, "lambda_3",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+3, "eigvec_1[1]",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+4, "eigvec_1[2]",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+5, "eigvec_1[3]",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+6, "eigvec_2[1]",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+7, "eigvec_2[2]",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+8, "eigvec_2[3]",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+9, "eigvec_3[1]",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+10,"eigvec_3[2]",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+11,"eigvec_3[3]",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+12,"FA",ADN_none);

      tross_Make_History( "3dTeig" , argc,argv , new_dset ) ;
      DSET_write( new_dset ) ;
      printf("--- Output dataset %s\n",DSET_FILECODE(new_dset)) ;
   } else {
      fprintf(stderr,"*** Unable to compute output dataset!\n") ;
      exit(1) ;
   }

   exit(0) ;
}

/**********************************************************************
   Function that does the real work
***********************************************************************/

static void EIG_tsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean, double ts_slope,
                          void * ud, int nbriks, float * val          )
{
  /*  THD_dmat33 inmat;
      THD_dvecmat eigvmat;*/
  int i,j,k;
  static int nvox , ncall ;
   int maxindex, minindex, midindex;
   float temp, minvalue, maxvalue;
   int sortvector[3];
   double a[9], e[3];
   int astart, vstart;
   double ssq, dsq;
   double dv0, dv1, dv2;

  ENTRY("EIG_tsfunc"); 
  /* ts is input vector data of 6 floating point numbers.
     For each point in volume brik convert vector data to
     symmetric matrix */
  /* ts should come from data sub-briks in form of Dxx,Dxy,Dxz,Dyy,Dyz,Dzz */
  /* convert to matrix of form 
     [ Dxx Dxy Dxz]
     [ Dxy Dyy Dyz]
     [ Dxz Dyz Dzz]  */

   /** is this a "notification"? **/
   if( val == NULL ){

      if( npts > 0 ){  /* the "start notification" */

         nvox  = npts ;                       /* keep track of   */
         ncall = 0 ;                          /* number of calls */

      } else {  /* the "end notification" */

         /* nothing to do here */
      }
      return ;
   }

   /* load the symmetric matrix vector from the "timeseries" subbrik vector values */
   a[0]=ts[0]; a[1]=ts[1]; a[2]=ts[2];  
   a[3]=ts[1]; a[4]=ts[3]; a[5]=ts[4];
   a[6]=ts[2]; a[5]=ts[4]; a[8]=ts[5];

  symeig_double(3, a, e);    /* compute eigenvalues in e, eigenvectors in a */
  
  maxindex=2;                      /* find the lowest, middle and highest eigenvalue */
  maxvalue=e[2];
  minindex=0;
  minvalue=e[0];
  midindex = 1;

  for(i=0;i<3;i++) {        
    temp = e[i];
    if(temp>maxvalue) {            /* find the maximum */
      maxindex = i;
      maxvalue = temp;
    }
    if(temp<minvalue) {            /* find the minimum */
      minindex = i;
      minvalue = temp;
    }
  }

  for(i=0;i<3;i++){                /* find the middle */
    if((i!=maxindex) && (i!=minindex))
      {
	midindex = i;
        break;
      }        
  }

  sortvector[0] = maxindex;
  sortvector[1] = midindex;
  sortvector[2] = minindex;

  /* put the eigenvalues at the beginning of the matrix */
  for(i=0;i<3;i++) {
     val[i] = e[sortvector[i]];    /* copy sorted eigenvalues */
  
                                 /* start filling in eigenvector values */
     astart=sortvector[i]*3;    /* start index of double eigenvector */    
     vstart=(i+1)*3;                 /* start index of float val vector to copy eigenvector */

     for(j=0;j<3;j++){
        val[vstart+j] = a[astart+j];
      }
  }

  /* calculate the Fractional Anisotropy, FA */
  /*   reference, Pierpaoli C, Basser PJ. Microstructural and physiological features 
       of tissues elucidated by quantitative-diffusion tensor MRI,J Magn Reson B 1996; 111:209-19 */
  if((val[0]<=0.0)||(val[1]<=0.0)||(val[2]<=0.0)) {   /* any negative eigenvalues?*/
    val[12]=0.0;                                      /* set FA to 0 */  
    EXRETURN;
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
    val[12] = sqrt(dsq/(2.0*ssq));   /* FA calculated here */
  else
    val[12] = 0.0;

  EXRETURN;
}

