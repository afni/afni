/*********************** 3dDTeig.c **********************************************/
/* Author: Daniel Glen, 15 Nov 2004 */
#include "mrilib.h"
#include "thd_shear3d.h"

#define SMALLNUMBER 1E-4

static char prefix[THD_MAX_PREFIX] = "eig" ;
static int datum                   = MRI_float ;
static void EIG_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void * ud , int nbriks, float * val ) ;
static void Save_Sep_eigdata(THD_3dim_dataset *, char *, int);
static void Copy_dset_array(THD_3dim_dataset *, int,int,char *, int);

static int udflag = 0;

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   int nopt, nbriks ;
   int sep_dsets = 0;

   /*----- Read command line -----*/
   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dDTeig [options] dataset\n"
             "Computes eigenvalues and eigenvectors for an input dataset of\n"
             " 6 sub-bricks Dxx,Dxy,Dyy,Dxz,Dyz,Dzz (lower diagonal order).\n"
	     " The results are stored in a 14-subbrick bucket dataset.\n"
	     " The resulting 14-subbricks are\n"
	     "  lambda_1,lambda_2,lambda_3,\n"
             "  eigvec_1[1-3],eigvec_2[1-3],eigvec_3[1-3],\n"
             "  FA,MD.\n\n"
             "The output is a bucket dataset.  The input dataset\n"
             "may use a sub-brick selection list, as in program 3dcalc.\n"
             " Options:\n"
             "  -prefix pname = Use 'pname' for the output dataset prefix name.\n"
             "    [default='eig']\n\n"
      	     "  -datum type = Coerce the output data to be stored as the given type\n"
	     "    which may be byte, short or float. [default=float]\n\n"
             "  -sep_dsets = save eigenvalues,vectors,FA,MD in separate datasets\n\n"
             "  -uddata = tensor data is stored as upper diagonal instead of lower diagonal\n\n"
             " Mean diffusivity (MD) calculated as simple average of eigenvalues.\n"
	     " Fractional Anisotropy (FA) calculated according to Pierpaoli C, Basser PJ.\n"
             " Microstructural and physiological features of tissues elucidated by\n"
             " quantitative-diffusion tensor MRI, J Magn Reson B 1996; 111:209-19\n"
           ) ;
      printf("\n" MASTER_SHORTHELP_STRING ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dDTeig main"); machdep(); AFNI_logger("3dDTeig",argc,argv);
   PRINT_VERSION("3dDTeig") ; AUTHOR("Daniel Glen");

   nopt = 1 ;
   nbriks = 14 ;
   datum = MRI_float;
   while( nopt < argc && argv[nopt][0] == '-' ){
      /*-- prefix --*/
      if( strcmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ){
            ERROR_exit("  -prefix needs an argument!");
         }
         MCW_strncpy(prefix,argv[nopt],THD_MAX_PREFIX) ;
         if( !THD_filename_ok(prefix) ){
            ERROR_exit("%s is not a valid prefix!",prefix);
         }
         nopt++ ; continue ;
      }

      /*-- datum --*/
      if( strcmp(argv[nopt],"-datum") == 0 ){
         if( ++nopt >= argc ){
            ERROR_exit(" -datum needs an argument!");
         }
         if( strcmp(argv[nopt],"short") == 0 ){
            datum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            datum = MRI_float ;
         } else if( strcmp(argv[nopt],"byte") == 0 ){
            datum = MRI_byte ;
         } else {
            ERROR_exit("-datum of type '%s' is not supported!",
                    argv[nopt] ) ;
         }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-uddata") == 0 ){   /* upper diagonal tensor data */
        udflag = 1;
        nopt++; continue;
      }

      if (strcmp (argv[nopt], "-sep_dsets") == 0)
         {
           sep_dsets = 1;  /* save data in separate datasets */
           nopt++;
	   continue;
         }

      ERROR_exit("Error - unknown option %s", argv[nopt]);
   }

   /*----- read input dataset -----*/
   if( nopt >= argc ){
      ERROR_exit("No input dataset!?");
   }

   old_dset = THD_open_dataset( argv[nopt] ) ;
   CHECK_OPEN_ERROR(old_dset,argv[nopt]) ;

   /* expect 6 values per voxel - 6 sub-briks as input dataset */ 
   if( DSET_NVALS(old_dset) < 6 ){  /* allows 6 or greater sub-briks */
      ERROR_exit("Can't use dataset that is not at least 6 values per voxel!") ;
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
      EDIT_dset_items(new_dset, ADN_brick_label_one+13,"MD",ADN_none);

      tross_Make_History( "3dDTeig" , argc,argv , new_dset ) ;
      if(sep_dsets)
         Save_Sep_eigdata(new_dset, prefix, datum);
      else {
         DSET_write (new_dset);
         INFO_message("--- Output dataset %s", DSET_BRIKNAME(new_dset));
      } 
   } else {
      ERROR_exit("Unable to compute output dataset!") ;
   }

   exit(0) ;
}


/*! save separate datasets for each kind of output */
/* copied from 3dDWItoDT.c */
static void
Save_Sep_eigdata(whole_dset, prefix, output_datum)
THD_3dim_dataset *whole_dset; /* whole dataset */
char *prefix;
int output_datum;
{
/* takes base prefix and appends to it for eigvalues, eigvectors, FA, MD */
   char nprefix[THD_MAX_PREFIX], tprefix[THD_MAX_PREFIX];
   char *ext, nullch; 
   
   ENTRY("Save_Sep_eigdata");

   sprintf(tprefix,"%s",prefix);
   if(has_known_non_afni_extension(prefix)){   /* for NIFTI, 3D, Niml, Analyze,...*/
      ext = find_filename_extension(prefix);
      tprefix[strlen(prefix) - strlen(ext)] = '\0';  /* remove non-afni-extension for now*/
   }
   else {
      nullch = '\0';
      ext = &nullch;
   }
  
   sprintf(nprefix,"%s_L1%s", tprefix,ext);
   Copy_dset_array(whole_dset,0,1, nprefix, output_datum);
   sprintf(nprefix,"%s_L2%s", tprefix,ext);
   Copy_dset_array(whole_dset,1,1, nprefix, output_datum);
   sprintf(nprefix,"%s_L3%s", tprefix,ext);
   Copy_dset_array(whole_dset,2,1, nprefix, output_datum);
   sprintf(nprefix,"%s_V1%s", tprefix,ext);
   Copy_dset_array(whole_dset,3,3, nprefix, output_datum);
   sprintf(nprefix,"%s_V2%s", tprefix,ext);
   Copy_dset_array(whole_dset,6,3, nprefix, output_datum);
   sprintf(nprefix,"%s_V3%s", tprefix,ext);
   Copy_dset_array(whole_dset,9,3, nprefix, output_datum);
   sprintf(nprefix,"%s_FA%s", tprefix,ext);
   Copy_dset_array(whole_dset,12,1, nprefix, output_datum);
   sprintf(nprefix,"%s_MD%s", tprefix,ext);
   Copy_dset_array(whole_dset,13,1, nprefix, output_datum);
   
   EXRETURN;
}

/*! create new dataset from part of existing dataset in memory */
/* copied from 3dDWItoDT.c */
static void
Copy_dset_array(whole_dset,startbrick,nbriks,prefix,output_datum)
THD_3dim_dataset *whole_dset;
int startbrick, nbriks;
char *prefix;
int output_datum;
{
   THD_3dim_dataset *out_dset;

   int i, ierror;
   MRI_IMAGE *fim;
   void *dataptr;
   float *fbuf;

   ENTRY("Copy_dset_array");

   out_dset = EDIT_empty_copy(whole_dset) ;
   fbuf = (float *)  malloc (sizeof(float)   * nbriks);

   tross_Copy_History (whole_dset, out_dset);
   ierror = EDIT_dset_items( out_dset ,
            ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
                        ADN_prefix , prefix ,
			ADN_datum_all, output_datum,
			ADN_nvals, nbriks,
			ADN_ntt, 0,
                        ADN_type        , ISHEAD(whole_dset)       /* dataset type */
                                 ? HEAD_FUNC_TYPE
                                 : GEN_FUNC_TYPE ,
                        ADN_func_type   , FUNC_BUCK_TYPE ,        /* function type */
                        ADN_none ) ;
			
   if(ierror>0) 
       ERROR_exit("*** Error - Unable to edit dataset!");

   THD_init_datablock_keywords( out_dset->dblk ) ;
   THD_init_datablock_stataux( out_dset->dblk ) ; /* for some reason, need to do this for 
                                                     single brick NIFTI files */
 
   /* attach brick, factors and labels to new dataset using existing brick pointers */
   for(i=0;i<nbriks;i++) {
      fim = DSET_BRICK(whole_dset,startbrick+i);
      dataptr = mri_data_pointer(fim);
      fbuf[i] = whole_dset->dblk->brick_fac[startbrick+i];
      /* copy labels here too.....*/  
      EDIT_dset_items (out_dset, ADN_brick_label_one + i, whole_dset->dblk->brick_lab[startbrick+i], ADN_none);
      /*----- attach mri_image pointer to to be sub-brick #i -----*/
      EDIT_substitute_brick(out_dset, i, output_datum, dataptr);
   }

   (void) EDIT_dset_items( out_dset , ADN_brick_fac , fbuf , ADN_none ) ;
   DSET_write (out_dset);
   INFO_message("--- Output dataset %s", DSET_BRIKNAME(out_dset));
   /*----- deallocate memory -----*/
   THD_delete_3dim_dataset (out_dset, False);   out_dset = NULL ;
   free (fbuf);   fbuf = NULL;
   EXRETURN;
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
  int i,j;
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
   if(udflag) {               /* read in as upper diagonal elements */
      a[0]=ts[0]; a[1]=ts[1]; a[2]=ts[2];  
      a[3]=ts[1]; a[4]=ts[3]; a[5]=ts[4];
      a[6]=ts[2]; a[7]=ts[4]; a[8]=ts[5];
   }
   else {         /* read D tensor in as lower diagonal elements - NIFTI standard */ 
      a[0]=ts[0]; a[1]=ts[1]; a[2]=ts[3];
      a[3]=ts[1]; a[4]=ts[2]; a[5]=ts[4];
      a[6]=ts[3]; a[7]=ts[4]; a[8]=ts[5];
   }
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

  for(i=0;i<3;i++) {
    if(fabs(val[i])<SMALLNUMBER)
       val[i] = 0.0;
  }

  /* calculate the Fractional Anisotropy, FA */
  /*   reference, Pierpaoli C, Basser PJ. Microstructural and physiological features 
       of tissues elucidated by quantitative-diffusion tensor MRI,J Magn Reson B 1996; 111:209-19 */
  if((val[0]<=0.0)||(val[1]<0.0)||(val[2]<0.0)) {   /* any negative eigenvalues?*/
    val[12]=0.0;                                      /* set FA to 0 */  
    val[13]=0.0;                                      /* set MD to 0 */
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
  val[13] = (val[0]+val[1]+val[2]) / 3;  /* MD - mean diffusivity=average of eigenvalues */

  EXRETURN;
}

