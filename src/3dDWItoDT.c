/*********************** 3dDWItoDT.c **********************************************/
/* Author: Daniel Glen, 17 Nov 2004 */
/* compute 6 principle direction tensors from multiple gradient vectors*/
/* and corresponding image data */
#include "thd_shear3d.h"
/*#ifndef FLOATIZE*/
# include "matrix.h"
# include "matrix.c"
/*#endif*/

static char prefix[THD_MAX_PREFIX] = "DT" ;
static int datum                   = MRI_float ;
static matrix Rtmat;
static void Form_R_Matrix(MRI_IMAGE *grad1Dptr);
static void DWItoDT_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void * ud , int nbriks, float * val ) ;

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   int nopt, nbriks, ii ;
   int addBriks = 0;
   int numMultBriks,methIndex,brikIndex;
   MRI_IMAGE *grad1Dptr = NULL;

   /*----- Read command line -----*/
   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dDWItoDT [options] gradient-file dataset\n"
             "Computes 6 principle direction tensors from multiple gradient vectors\n"
             " and corresponding DTI image volumes.\n"
             " The program takes two parameters as input: \n"
             "    a 1D file of the gradient vectors with lines of ASCII floats Gxi,Gyi,Gzi\n."
             "    a 3D bucket dataset with Np+1 sub-briks where the first sub-brik is the volume\n"
             "    acquired with no diffusion weighting.\n"
             " The output is a 6 sub-brick bucket dataset containing Dxx,Dxy,Dxz,Dyy,Dyz,Dzz.\n"
	     " These results are appropriate as the input to the 3dDTeig program.\n"
             "\n"
           ) ;
      printf("\n" MASTER_SHORTHELP_STRING ) ;
      exit(0) ;
   }

   mainENTRY("3dDWItoDT main"); machdep(); AFNI_logger("3dDWItoDT",argc,argv);

   nopt = 1 ;
   nbriks = 6;                                      /* output contains 6 sub-briks */
   datum = MRI_float;
   while( nopt < argc && argv[nopt][0] == '-' ){

      /*-- prefix --*/

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"*** -prefix needs an argument!\n"); exit(1);
         }
         MCW_strncpy(prefix,argv[nopt],THD_MAX_PREFIX) ;   /* change name from default prefix */
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

   /*----- read input datasets -----*/

   if( nopt >= argc ){
      fprintf(stderr,"*** No input dataset!?\n"); exit(1);
   }

   /* first input dataset - should be gradient vector file of ascii floats Gx,Gy,Gz */

   /* read gradient vector 1D file */
   grad1Dptr = mri_read_1D(argv[nopt]);
   if(grad1Dptr==NULL){
      fprintf(stderr,"*** Error reading gradient vector file\n") ;
      exit(1) ;
   }

   if(grad1Dptr->ny != 3){
      fprintf(stderr,"*** Only 3 columns of gradient vectors allowed\n") ;
      fprintf(stderr,"%d columns found\n", grad1Dptr->nx);
      mri_free(grad1Dptr);
      exit(1) ;
   }

   if(grad1Dptr->nx < 6){
      fprintf(stderr,"*** Must have at least 6 gradient vectors\n") ;
      fprintf(stderr,"%d columns found\n", grad1Dptr->nx);
      mri_free(grad1Dptr);
      exit(1) ;
   }
   

   Form_R_Matrix(grad1Dptr);              /* use grad1Dptr to computer R matrix */

   nopt++;

   /* Now read in all the MRI volumes for each gradient vector */
   /* assumes first one is no gradient */
   old_dset = THD_open_dataset( argv[nopt] ) ;

   if( !ISVALID_DSET(old_dset) ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[nopt]); 
      mri_free(grad1Dptr);
      exit(1);
   }

   /* expect at least 7 values per voxel - 7 sub-briks as input dataset */ 
   if(DSET_NVALS(old_dset) != (grad1Dptr->nx + 1) ){
      fprintf(stderr,
	      "*** Dataset must have number of sub-briks equal to one more than number\n");
      fprintf(stderr,"  of gradient vectors (B0+Bi)!\n") ;
      mri_free(grad1Dptr);
      exit(1) ;
   }

   mri_free(grad1Dptr);

   /* temporarily set artificial timing to 1 second interval */
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
		 DWItoDT_tsfunc ,         /* timeseries processor */
                 NULL                   /* data for tsfunc */
              ) ;

   matrix_destroy(&Rtmat);              /* clean up */

   if( new_dset != NULL ){
      tross_Copy_History( old_dset , new_dset ) ;
      EDIT_dset_items(new_dset, ADN_brick_label_one+0, "Dxx",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+1, "Dxy",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+2, "Dxz",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+3, "Dyy",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+4, "Dyz",ADN_none);
      EDIT_dset_items(new_dset, ADN_brick_label_one+5, "Dzz",ADN_none);

      tross_Make_History( "3dDWItoDT" , argc,argv , new_dset ) ;
      DSET_write( new_dset ) ;
      printf("--- Output dataset %s\n",DSET_FILECODE(new_dset)) ;
   } else {
      fprintf(stderr,"*** Unable to compute output dataset!\n") ;
      exit(1) ;
   }

   exit(0) ;
}

/* Form R matrix as matrix of [bxx 2bxy 2bxz byy 2byz bzz] for Np rows */
static void Form_R_Matrix(MRI_IMAGE *grad1Dptr)
{
  matrix Rmat;
   register double sf = 1.0; /* scale factor = 1.0 for now until we know DELTA, delta (gamma = 267.5 rad/ms-mT) */
   register double sf2;      /* just scale factor * 2 */
   int i, nrows;
   register float *imptr, *Gxptr, *Gyptr, * Gzptr;
   matrix *nullptr = NULL;
   register double Gx, Gy, Gz;
   ENTRY("Form_R_Matrix"); 
   nrows = grad1Dptr->nx;
   matrix_initialize (&Rmat);
   matrix_create(nrows, 6, &Rmat);  /* Rmat = Np x 6 matrix */
   if(Rmat.elts == NULL){    /* memory allocation error */
     fprintf(stderr,"could not allocate memory for Rmat \n");
     EXRETURN;
   }
   sf2 = sf + sf;                              /* 2 * scale factor for minor speed improvement */
   Gxptr = imptr = MRI_FLOAT_PTR(grad1Dptr);  /* use simple floating point pointers to get values */
   Gyptr = imptr + nrows;
   Gzptr = Gyptr + nrows;

   for(i=0;i<nrows;i++){
     Gx = *Gxptr++;
     Gy = *Gyptr++;
     Gz = *Gzptr++;
     Rmat.elts[i][0] = sf * Gx * Gx;  /* bxx = Gx*Gx*scalefactor */
     Rmat.elts[i][1] = sf2 * Gx * Gy; /* 2bxy = 2GxGy*scalefactor */
     Rmat.elts[i][2] = sf2 * Gx * Gz; /* 2bxz = 2GxGz*scalefactor */
     Rmat.elts[i][3] = sf * Gy * Gy;  /* byy = Gy*Gy*scalefactor */
     Rmat.elts[i][4] = sf2 * Gy * Gz; /* 2byz = 2GyGz*scalefactor */
     Rmat.elts[i][5] = sf * Gz * Gz;  /* bzz = Gz*Gz*scalefactor */
   }

   matrix_initialize (&Rtmat);
   matrix_psinv(Rmat,nullptr, &Rtmat);   /* compute pseudo-inverse of Rmat=Rtmat */
   matrix_destroy(&Rmat);               /*  from the other two matrices*/
   EXRETURN;
}


/**********************************************************************
   Function that does the real work
***********************************************************************/

static void DWItoDT_tsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean, double ts_slope,
                          void * ud, int nbriks, float * val          )
{
   int i;
  static int nvox , ncall ;
    register double i0;
   register double dv,dv0;
   vector lnvector,Dvector;

  ENTRY("DWItoDT_tsfunc"); 
  /* ts is input vector data of Np+1 floating point numbers.
     For each point in volume brik convert vector data to
     symmetric matrix */
  /* ts should come from data sub-briks in form of I0,I1,...Ip */
  /* val is output vector of form Dxx Dxy Dxz Dyy Dyz Dzz for each voxel in 6 sub-briks */
  /* the Dij vector is computed as the product of  Rt times ln(I0/Ip)
     where Rt is the pseudo-inverse of the [bxx 2bxy 2bxz byy 2byz bzz] for
     each gradient vector b */
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
   ncall++;

   /* load the symmetric matrix vector from the "timeseries" subbrik vector values */
   vector_initialize(&lnvector);
   vector_create_noinit(npts-1, &lnvector);
   dv0 = ts[0];
   if(dv0>0.0)
      i0 = log(dv0);
   for(i=0;i<(npts-1);i++){
     dv = ts[i+1];
     if((dv>0.0)&&(dv0>0.0))
       lnvector.elts[i] = i0- log(dv);   /* ln I0/Ip = ln I0 - ln Ip */
     else
       lnvector.elts[i] = 0.0;
   }

   vector_multiply(Rtmat,lnvector, &Dvector);    /* D = Rt * ln(I0/Ip) */
   vector_to_array(Dvector, val);
   vector_destroy(&lnvector);
  EXRETURN;
}

