/******************* 3danisosmooth.c  *************************************/
/* Author: Daniel Glen, 13 Jun 2005                                   */
/* Smooths volumes using an anisotropic smoothing technique           */
/* Intended for DWI images, it first computes a tensor of structure   */
/* of DWI volumes for the purpose of anisotropic smoothing of the     */
/* image.                                                             */
/* This D tensor is then used as basis for smoothing along directionality of */
/* the original DWI image.                                            */
/* Although originally intended for DWI data, it can be applied to    */
/* other types of data also */
/* The method can be applied in either 2D or 3D                       */
/**********************************************************************/

#include "thd_shear3d.h"
#include "matrix.h"
/*#include "matrix.c"*/
#include "afni.h"


#define TINYNUMBER 1E-10
#define SMALLNUMBER 1E-4

static double Dmean, Dmax, DeltaT;

static char prefix[THD_MAX_PREFIX] = "SmoothAni";

static NI_stream ns = NULL;

int compute_method = 0;   /* use Ding's method to compute phi values */
float deltatflag = -1.0;  /* compute pseudotime step or use specific value */

#define Smooth_WriteCheckWaitMax 2000
#define Smooth_WriteCheckWait 400
#define START_PORT 4444       /* port range for aiv communications */
#define MAX_PORT 4544


THD_3dim_dataset * Copy_dset_to_float(THD_3dim_dataset * dset , char * new_prefix );
static void Smooth_dset_tensor(THD_3dim_dataset *tempdset, THD_3dim_dataset *structtensor, int flag2D3D, byte *maskptr);

static int Smooth_Open_Stream(int port);
static int Smooth_Show_Image(float *far, int nx, int ny);
static int Show_dset_slice(THD_3dim_dataset *dset);
static void Compute_Dstats(THD_3dim_dataset *structtensor,int flag2D3D, byte *maskptr);
static void Compute_Ematrix(THD_3dim_dataset *structtensor, int flag2D3D, byte *maskptr);
static void Compute_Gmatrix(MRI_IMARR *Flux_Im, int flag2D3D, byte *maskptr);
static void Compute_Smooth(THD_3dim_dataset *tempdset,MRI_IMARR *G_Im,int flag2D3D, byte *maskptr);
static void Compute_Flux(MRI_IMARR *Gradient_im, THD_3dim_dataset *structtensor, int flag2D3D, byte *maskptr);
static void Update_Brik(THD_3dim_dataset *indset, THD_3dim_dataset *outdset, int brickn);
static void Test_data(THD_3dim_dataset *structtensor);


extern THD_3dim_dataset *
DWIstructtensor(THD_3dim_dataset * DWI_dset, int flag2D3D, byte *maskptr, int smooth_flag, int save_tempdsets_flag);
extern MRI_IMARR *Compute_Gradient_Matrix(THD_3dim_dataset *DWI_dset, int flag2D3D, byte *maskptr, int prodflag);
extern MRI_IMARR *Compute_Gradient_Matrix_Im(MRI_IMAGE *SourceIm, int flag2D3D, byte *maskptr, int xflag, int yflag);
extern float vox_val(int x,int y,int z,float *imptr, int nx, int ny, int nz, byte *maskptr, int i, int j, int k);
extern void Compute_IMARR_Max(MRI_IMARR *Imptr);

/*! compute the overall minimum and maximum voxel values for a dataset */
int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset , * udset ;  /* input and output datasets */
   THD_3dim_dataset * structtensor; /* structure tensor dataset */
   THD_3dim_dataset * mask_dset ;
   int nxyz, i, datum;
   int afnitalk_flag = 0;
   int nopt = 1;
   byte *maskptr = NULL;
   int automask = 0;
   int flag2D3D = 0;
   int port, ret;
   char tempstring[256];
   int iters = 10;
   int mmvox = 0;
   int save_tempdsets_flag = 0;
   int smooth_flag = 1;

   /*----- Read command line -----*/
   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3danisosmooth [options] dataset\n"
             "Smooths a dataset using anisotropic smoothing.\n"
             "\n"
             "The output dataset is preferentially smoothed in similar areas.\n\n"
             "Options :\n"
             "  -prefix pname = Use 'pname' for output dataset prefix name.\n"
             "  -iters nnn = compute nnn iterations (default=10)\n"
             "  -2D = smooth a slice at a time (default)\n"
             "  -3D = smooth through slices. Can not be combined with 2D option\n"
             "  -mask dset = use dset as mask to include/exclude voxels\n"
             "  -automask = automatically compute mask for dataset\n"
             "    Can not be combined with -mask\n"
             "  -viewer = show central axial slice image every iteration.\n"
             "    Starts aiv program internally.\n"
             "  -nosmooth = do not do intermediate smoothing of gradients\n"
             "  -deltat n.nnn = assign pseudotime step. Default = 0.25\n"
             "  -savetempdata = save temporary datasets each iteration.\n"
             "   Dataset prefixes are Gradient, Eigens, phi and Dtensor.\n"
             "   Each is overwritten each iteration\n"
             "  -phiding = use Ding method for computing phi (default)\n"
             "  -phiexp = use exponential method for computing phi\n\n" 
             "  -help = print this help screen\n"
             "References:\n"
             "  Z Ding, JC Gore, AW Anderson, Reduction of Noise in Diffusion\n"
             "   Tensor Images Using Anisotropic Smoothing, Mag. Res. Med.,\n"
             "   53:485-490, 2005\n"
             "  J Weickert, H Scharr, A Scheme for Coherence-Enhancing\n"
             "   Diffusion Filtering with Optimized Rotation Invariance,\n"
             "   CVGPR Group Technical Report at the Department of Mathematics\n"
             "   and Computer Science,University of Mannheim,Germany,TR 4/2000.\n"
             "  J.Weickert,H.Scharr. A scheme for coherence-enhancing diffusion\n"
             "   filtering with optimized rotation invariance. J Visual\n"
             "   Communication and Image Representation, Special Issue On\n"
             "   Partial Differential Equations In Image Processing,Comp Vision\n"
             "   Computer Graphics, pages 103-118, 2002.\n"
             "  Gerig, G., Kübler, O., Kikinis, R., Jolesz, F., Nonlinear\n"
             "   anisotropic filtering of MRI data, IEEE Trans. Med. Imaging 11\n"
             "   (2), 221-232, 1992.\n\n"
           ) ;
      printf("\n" MASTER_SHORTHELP_STRING ) ;
      exit(0) ;
   }

   mainENTRY("3danisosmooth main"); machdep(); AFNI_logger("3danisosmooth",argc,argv);

  datum = MRI_float;
   compute_method = -1;  /* detect multiple or default selection of compute_method */
   deltatflag = -1.0;    /* pseudo-time step */   
   while( nopt < argc && argv[nopt][0] == '-' ){
      if( strcmp(argv[nopt],"-prefix") == 0 ){
	  if (++nopt >= argc)
	    {
	      fprintf (stderr, "*** Error - prefix needs an argument!\n");
	      exit (1);
	    }
	  MCW_strncpy (prefix, argv[nopt], THD_MAX_PREFIX);	/* change name from default prefix */
	  if (!THD_filename_ok (prefix))
	    {
	      fprintf (stderr, "*** Error - %s is not a valid prefix!\n", prefix);
	      exit (1);
	    }
	  nopt++;
	  continue;
      }
     if (strcmp (argv[nopt], "-automask") == 0)
	{
         if(maskptr != NULL){
           fprintf(stderr,"** ERROR: can't use -mask with -automask!\n");
           exit(1) ;
         }
 	  automask = 1;
	  nopt++;
	  continue;
	}

      if( strcmp(argv[nopt],"-mask") == 0 ){
         if( automask ){
           fprintf(stderr,"** ERROR: can't use -mask with -automask!\n");
           exit(1) ;
         }
         mask_dset = THD_open_dataset(argv[++nopt]) ;
         if( mask_dset == NULL ){
            fprintf(stderr,"** ERROR: can't open -mask dataset!\n"); exit(1);
         }
         if( maskptr != NULL ){
            fprintf(stderr,"** ERROR: can't have 2 -mask options!\n"); exit(1);
         }
         maskptr = THD_makemask( mask_dset , 0 , 1.0,-1.0 ) ;
         mmvox = DSET_NVOX( mask_dset ) ;
         DSET_delete(mask_dset) ; nopt++ ; continue ;
      }

      if (strcmp (argv[nopt], "-viewer") == 0)
        {
         afnitalk_flag = 1;
	 nopt++;
	 continue;
        }

      if (strcmp (argv[nopt], "-2D") == 0)
        {
          if(flag2D3D==0)
             flag2D3D = 2;
          else {
            fprintf(stderr, "*** ERROR: can't select both 2D and 3D flags\n");
            exit(1);
          }
          nopt++;
          continue;
        }

      if (strcmp (argv[nopt], "-3D") == 0)
        {
          if(flag2D3D==0)
             flag2D3D = 3;
          else {
            fprintf(stderr, "*** ERROR: can't select both 2D and 3D flags\n");
            exit(1);
          }
          nopt++;
          continue;
        }
     if (strcmp (argv[nopt], "-nosmooth") == 0)
        {
	  smooth_flag = 0;
          nopt++;
          continue;
        }
     if (strcmp (argv[nopt], "-savetempdata") == 0)
        {
	  save_tempdsets_flag = 1;
          nopt++;
          continue;
        }
  
     if( strcmp(argv[nopt],"-iters") == 0 ){
	   if(++nopt >=argc ){
	      fprintf(stderr,"*** Error - need an argument after -iters!\n");
	      exit(1);
	   }
           iters = strtol(argv[nopt], NULL, 10);
	   if ((iters <1)||(iters>200)) {
	      fprintf(stderr, "Error - iters must be between 1 and 200\n");
	      exit(1);
           }
          nopt++;
	  continue;
      }

     if( strcmp(argv[nopt],"-deltat") == 0 ){
	   if(++nopt >=argc ){
	      fprintf(stderr,"*** Error - need an argument after -iters!\n");
	      exit(1);
	   }
           deltatflag = atof(argv[nopt]);
	   if (deltatflag <0) {
	      fprintf(stderr, "Error - deltatflag must be positive!\n");
	      exit(1);
           }
          nopt++;
	  continue;
      }

     if( strcmp(argv[nopt],"-phiding") == 0 ){
           if(compute_method!=-1) {
	      fprintf(stderr,"*** Error - can not specify two compute methods!\n");
	      exit(1);
	   }
           compute_method  = 0;
	   nopt++;
	   continue;
     }

     if( strcmp(argv[nopt],"-phiexp") == 0 ){
           if(compute_method!=-1) {
	      fprintf(stderr,"*** Error - can not specify two compute methods!\n");
	      exit(1);
	   }
           compute_method  = 1;
	   nopt++;
	   continue;
     }
     
     fprintf(stderr, "*** Error - unknown option %s\n", argv[nopt]);
      exit(1);

   }


   /*----- read input dataset -----*/

   if( nopt >= argc ){
      fprintf(stderr,"*** No input dataset!?\n"); exit(1);
   }

   dset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(dset) ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[nopt]); exit(1);
   }

   nxyz = DSET_NVOX(dset) ;
   if( maskptr != NULL && mmvox != nxyz ){
      fprintf(stderr,"** Mask and input datasets not the same size!\n") ;
      exit(1) ;
   }

   if(automask && (maskptr == NULL )){
      maskptr = THD_automask( dset ) ;
   }

   if(flag2D3D == 0)
     flag2D3D = 2;    /* make default 2D processing for speed */ 

   if(afnitalk_flag) {            /* set up viewer */
      port = START_PORT;
      ret = 0; 
      while ((ret==0) && (port<MAX_PORT)) {   /* find first unused port */
         ret = Smooth_Open_Stream(port); /* Open test stream */
         if(ret==0){     /* should fail because we haven't opened aiv yet */
            port++;          /* stream already opened from other aiv? */
         }
         if(ns) {
            NI_stream_closenow(ns) ;
            ns = NULL;
         }
      }
      if(port==MAX_PORT) {
        afnitalk_flag = 0;
        fprintf(stderr,"+++aiv has too many ports open\n");
      }
      else { 
         sprintf(tempstring,"aiv -p %d &", port);
         ret = system(tempstring); /* use the aiv program to display a slice */

         if(ret==0)
            ret = Smooth_Open_Stream(port); /* Open display stream */
         if (ret!=0) {
            afnitalk_flag = 0;
            fprintf(stderr,"+++could not open communications with aiv\n");
         }
      }
  }

   if(compute_method==-1)
      compute_method = 0;
      
   printf("loading original data\n");
  /* load the original DWI dataset */
   DSET_mallocize (dset);
   DSET_load (dset);	                /* load dataset */

  /* copy to udset in floats */
  /* printf("Copying to float\n");*/
  udset = Copy_dset_to_float(dset, prefix);
  if(afnitalk_flag) {
      Show_dset_slice(udset);  /* show mid-slice in middle brik */
  }
 
  for(i=0;i<iters;i++){
     printf("iteration %d\n ", i);
     /* compute image diffusion tensor dataset */
     printf("   computing structure tensor\n");
     structtensor =  DWIstructtensor(udset, flag2D3D, maskptr, smooth_flag, save_tempdsets_flag);
     /* Test_data(structtensor);*/
     if((i==iters-1)&&(save_tempdsets_flag)) {
       tross_Make_History ("3danisosmooth", argc, argv, structtensor);
       DSET_write (structtensor);
       printf ("--- Output dataset %s\n", DSET_BRIKNAME(structtensor));
     }
     printf("    applying structure tensor\n");
     /* Smooth udset image using image diffusion tensor */
     Smooth_dset_tensor(udset, structtensor, flag2D3D, maskptr);

     /* display sample udset slice after smoothing for this iteration */
     if(afnitalk_flag) {
       Show_dset_slice(udset); /* show mid-slice in middle brik */
     }

     THD_delete_3dim_dataset( structtensor , False ) ;  /* delete tensor */
  }

  /* save the dataset */
  tross_Copy_History (dset, udset);
  tross_Make_History ("3danisosmooth", argc, argv, udset);
  THD_load_statistics( udset );
  DSET_write (udset);
  printf ("--- Output dataset %s\n", DSET_BRIKNAME(udset));

  if(afnitalk_flag) {
    /* Close viewer stream */
    NI_stream_closenow(ns) ;
    /*    RT_exit();*/
  }
  if(maskptr)
    free(maskptr);
   exit (0);
}

/* copy original_dset to float_dset with float data */
THD_3dim_dataset *
Copy_dset_to_float(THD_3dim_dataset * dset , char * new_prefix )
{
   int iv;
   MRI_IMARR *fim_array;
   MRI_IMAGE *fim;
   THD_3dim_dataset * float_dset;

   ENTRY("Copy_dset_to_float");

   /*-- sanity check --*/

   if( ! ISVALID_3DIM_DATASET(dset) ) return NULL ;

   /*-- make the empty copy --*/

   float_dset = EDIT_empty_copy( dset ) ;

   /*-- change its name? --*/

   if( new_prefix != NULL )
      EDIT_dset_items( float_dset ,
              ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
                          ADN_prefix , new_prefix ,
                          ADN_label1 , new_prefix ,
	                  ADN_datum_all , MRI_float ,
                          ADN_none ) ;

   /*-- make brick(s) for this dataset --*/

   THD_load_datablock( dset->dblk ) ;  /* make sure old one is in memory */



   /* make new Image Array to hold gradients and then gradient products */
   INIT_IMARR(fim_array);

   for( iv=0 ; iv < dset->dblk->nvals ; iv++ ){
      /*- get sub-brick -*/
      fim = THD_extract_float_brick( iv , dset ) ;
      if( fim == NULL ) {
	fprintf(stderr,"***Error - can not allocated float dset");
          exit(1) ;
      }
      ADDTO_IMARR(fim_array, fim);
   }

   float_dset->dblk->brick = fim_array;   /* update pointer to data */
   for(iv=0;iv<dset->dblk->nvals;iv++)
       DSET_BRICK_FACTOR(float_dset, iv) = 0.0;

   RETURN(float_dset);
}

/*! open aiv / AFNIRT stream */
static int Smooth_Open_Stream(port)
     int port;
{
   int nn, Wait_tot;
   char streamname[256];

   ENTRY("Smooth_Open_Stream");

   sprintf(streamname, "tcp:localhost:%d", port);
   ns = NI_stream_open(streamname, "w");

   if(ns==0) {                  /* could not create stream */
     return(1);
   }

   Wait_tot = 0;                /* check connection */
   while(Wait_tot < Smooth_WriteCheckWaitMax){
      nn = NI_stream_writecheck( ns , Smooth_WriteCheckWait) ;
      if( nn == 1 ){ 
         RETURN(0) ; 
      }
      if( nn <  0 ){ 
         ns = NULL;
         RETURN(1);
      }
      Wait_tot += Smooth_WriteCheckWait;
   }
   RETURN(1);                   /* no connection */
}

/*! show the middle brik, middle slice */
static int Show_dset_slice(THD_3dim_dataset *dset)
{
   int mid_brik, nx, ny, nz;
   float *far=NULL;
   THD_dataxes   * daxes=NULL ;
   MRI_IMAGE *data_im=NULL;
   int ret;

   ENTRY("Show_dset_slice");

   mid_brik = (dset->dblk->nvals)/2 ;
   data_im = DSET_BRICK(dset, mid_brik);
   daxes = dset->daxes ;
   nx    = daxes->nxx ;
   ny    = daxes->nyy ;
   nz    = daxes->nzz ;
   far = mri_data_pointer(data_im);
   far += nx*ny*(nz-1)/2;      /* show middle brik, middle Z slice */
   ret = Smooth_Show_Image(far,nx,ny);
   RETURN(ret);
}


/*! create or update image window with the new data */
static int Smooth_Show_Image(far, nx, ny)
    float *far;
     int nx, ny;
{
  MRI_IMAGE *im;
  /*  char default_name[64] = "3danisosmooth image";*/
  NI_element *nel;

   ENTRY("Smooth_Show_Image");
   /*-- compute number of bytes per slice, and per image transmission --*/
   im = mri_new_vol_empty(nx, ny , 1, MRI_float); /* 1 slice volume */
   mri_fix_data_pointer(far, im);   /* set image to float pointer */
   /*   im->name = malloc(strlen(default_name));*/
   im->name = NULL;
   im->kind = MRI_float;
   /*  sprintf(im->name, "%s", default_name);*/
   nel = mri_to_niml(im);
   NI_write_element(ns, nel, NI_BINARY_MODE);
   NI_free_element(nel);
   mri_clear_data_pointer(im);
   mri_free(im);
   RETURN(0);
}


/*! Smooth dataset image using image diffusion tensor */
static void Smooth_dset_tensor(THD_3dim_dataset *udset, THD_3dim_dataset *structtensor, int flag2D3D, byte *maskptr)
{
  MRI_IMARR *Gradient_Im;
  int nbriks,i;
  int sublist[2];
  THD_3dim_dataset *tempdset;

  ENTRY("Smooth_dset_tensor");

  /* find mean and max of (Dxx+Dxy)*/
  Compute_Dstats(structtensor,flag2D3D, maskptr); 
  /* deviation from mean, can use structtensor space */
  /*  printf("Compute Ematrix\n");*/
  Compute_Ematrix(structtensor, flag2D3D, maskptr); 
  /*  Compute_IMARR_Max(structtensor->dblk->brick);*/
  nbriks =   udset->dblk->nvals;
  sublist[0] = 1;
  for(i=0;i<nbriks;i++) {
     sublist[1] = i;
     tempdset = THD_copy_dset_subs(udset, sublist);  /* copy current brik to tempdset */
     if(tempdset==NULL) {
       fprintf(stderr, "Can not create temporary dataset in Smooth_dset_tensor\n");
       EXRETURN;
     }

     /* compute gradient of tempdset - results in tempdset  */
     /*printf("Compute Gradient_Matrix\n");*/
     Gradient_Im = Compute_Gradient_Matrix(tempdset, flag2D3D, maskptr,0);
     /*Compute_IMARR_Max(Gradient_Im);*/

     /* Compute flux - results in Gradient_Im */
     /*printf("Compute Flux\n");*/
     Compute_Flux(Gradient_Im, structtensor, flag2D3D, maskptr);
     /*Compute_IMARR_Max(Gradient_Im);*/
     /* Compute anisotropic component of smoothing, G */
     /*   put in Gradient_Im space */
     /*printf("Compute Gmatrix\n");*/
     Compute_Gmatrix(Gradient_Im, flag2D3D, maskptr);
     /*Compute_IMARR_Max(Gradient_Im);*/
     /* compute isotropic diffusion component of smooth, F */
     /* and update dset with new smoothed image */
     /*printf("Compute isotropic Fmatrix and final smooth\n");*/
     Compute_Smooth(tempdset, Gradient_Im, flag2D3D, maskptr);
     Update_Brik(tempdset, udset, i);  /* update smoothed values of current brik */
     THD_delete_3dim_dataset( tempdset, False ) ;  /* delete temporary dset */
     DESTROY_IMARR(Gradient_Im);
   }
}

/*! find mean and max of (Dxx+Dxy)*/
static void Compute_Dstats(THD_3dim_dataset *structtensor,int flag2D3D, byte *maskptr)
{
  int i, nvox;
  double s0, s1, ts0;
  MRI_IMAGE *data_im = NULL;
  float *dx, *dy;
  byte *tempmaskptr;

  ENTRY("Compute_Dstats");

  tempmaskptr = maskptr;
  s0 = 0.0; s1 = -1E10;
  data_im = DSET_BRICK(structtensor, 0);
  dx = mri_data_pointer(data_im);
  data_im = DSET_BRICK(structtensor, 2);
  dy = mri_data_pointer(data_im);

  nvox = 0;
  for(i=0;i<data_im->nxyz;i++) {
    if(maskptr && !(*tempmaskptr++)) {
      dx++; dy++;
    }
    else {
      nvox++;
      ts0 = *dx + *dy;
      dx++; dy++;
      if(ts0>s1)
        s1 = ts0;           /* update max(Dxx + Dyy)*/
      s0 +=  ts0;           /* get sum of Dxx + Dyy */
    }
  }

  if(maskptr!=NULL) 
    nvox = data_im->nxyz;
  Dmean = s0 / (2.0 * nvox);  /* Dmean = 1/2 mean(Dxx+Dyy) */
  if(deltatflag==-1.0) {      /* if no user setting for delta T */
     Dmax = s1;
  /* DeltaT  = 1.0/7.0; */
     DeltaT  = Dmax / 4;      /*   set pseudo-time step to Dmax/4 */
  }
  else DeltaT = deltatflag;

  /*  printf("Dmean %g   Dmax %g   DeltaT %g\n", Dmean, Dmax, DeltaT);*/

  EXRETURN;
}

/*! deviation from mean, can use structtensor space */
static void Compute_Ematrix(THD_3dim_dataset *structtensor, int flag2D3D, byte *maskptr)
{

  int i;
  MRI_IMAGE *data_im = NULL;
  float *e0, *e2, tempe0, tempe2;
  byte *tempmaskptr;

  ENTRY("Compute_Ematrix");

  tempmaskptr = maskptr;
  data_im = DSET_BRICK(structtensor, 0);
  e0 = mri_data_pointer(data_im);
  /* e1 = Dxy, so just leave in place at second sub-brik */
  data_im = DSET_BRICK(structtensor, 2);
  e2 = mri_data_pointer(data_im);

  for(i=0;i<data_im->nxyz;i++) {
    if(maskptr && !(*tempmaskptr++)) {
      *e0 = 0.0; *e2 = 0.0;
    }
    else {
      tempe0 = *e0;
      tempe2 = *e2;
      *e0 = tempe0 - Dmean;    /* e0 = Dxx-Dmean */
      *e2 = tempe2 - Dmean;    /* e2 = Dyy-Dmean */
    }
    e0++; e2++;
  }

  EXRETURN;
}

/*! Compute flux */
static void Compute_Flux(MRI_IMARR * Gradient_Im, THD_3dim_dataset *structtensor, int flag2D3D, byte *maskptr)
{
  int i;
  MRI_IMAGE *data_im = NULL;
  float *e0,*e1,*e2, *Gx, *Gy;
  double Jx, Jy;
  byte *tempmaskptr;

  ENTRY("Compute_Flux");

  tempmaskptr = maskptr;
  data_im = DSET_BRICK(structtensor, 0);
  e0 = mri_data_pointer(data_im);
  data_im = DSET_BRICK(structtensor, 1);
  e1 = mri_data_pointer(data_im);
  data_im = DSET_BRICK(structtensor, 2);
  e2 = mri_data_pointer(data_im);
  Gx = mri_data_pointer(Gradient_Im->imarr[0]);
  Gy = mri_data_pointer(Gradient_Im->imarr[1]);


  for(i=0;i<data_im->nxyz;i++) {
    if(maskptr && !(*tempmaskptr++)) {
      *Gx = 0.0;
      *Gy = 0.0;
    }
    else {
    Jx =  (*e0 * *Gx) + (*e1 * *Gy);   /* Jx = Exx * du/dx + Exy * du/dy */
    Jy =  (*e1 * *Gx) + (*e2 * *Gy);   /* Jy = Exy * du/dx + Eyy * du/dy */
    *Gx = Jx;         /* replace gradient values with flux values */
    *Gy = Jy;
    }
    Gx++; Gy++;
    e0++; e1++; e2++;
  }

  EXRETURN;
}

/*! Compute anisotropic component of smoothing, G */
static void Compute_Gmatrix(MRI_IMARR * Flux_Im, int flag2D3D, byte *maskptr)
{
/*   put in Flux_Im space - first sub-brik */
/* compute gradient of Jx, Jy (first two sub-briks) */
/* G = dJx/dx + dJy/dy */

   int i, nxyz;
   float *dJx, *dJy, *Gptr;
   MRI_IMAGE *data_im;
   MRI_IMARR *tempimarr0, *tempimarr1;
   byte *tempmaskptr;

   ENTRY("Compute_Gmatrix");
   tempmaskptr = maskptr;
   data_im = Flux_Im->imarr[0];
   Gptr = mri_data_pointer(data_im);
   nxyz = data_im->nxyz;
   tempimarr0 = Compute_Gradient_Matrix_Im(data_im, flag2D3D, maskptr,1,0);  /* dJx/dx */
   data_im = Flux_Im->imarr[1];
   tempimarr1 = Compute_Gradient_Matrix_Im(data_im, flag2D3D, maskptr,0,1);  /* dJy/dy */

   dJx = mri_data_pointer(tempimarr0->imarr[0]);
   dJy = mri_data_pointer(tempimarr1->imarr[0]);

   for(i=0;i<nxyz;i++) {
      if(maskptr && !(*tempmaskptr++)) {
	*Gptr = 0.0;
      }
      else {
       *Gptr = *dJx + *dJy;  /* G = dJx + dJy */
      }
      Gptr++;
      dJx++; dJy++;
   }

   /* delete tempimarrs */
   DESTROY_IMARR(tempimarr1);
   DESTROY_IMARR(tempimarr0);

   EXRETURN;
}

/*! update dset with new smoothed image */
static void Compute_Smooth(THD_3dim_dataset *tempdset, MRI_IMARR *G_Im, int flag2D3D,byte * maskptr)
{
   byte *tempmaskptr;
   int nx, ny, nz, nbriks, i,j,k,l;
   double a, b, c;
   THD_dataxes   * daxes ;
   float *Gptr, *ar, *Gvalptr;
   MRI_IMAGE *data_im;
   double uval, Fval;

   ENTRY("Compute_Smooth");
   /* compute isotropic diffusion component of smooth, F and then overall smooth*/
   /* F = (Dmean / DeltaX^2) * [ 1/6  2/3  1/6]
                              [ 2/3 -10/3 2/3]
                              [ 1/6  2/3  1/6]U 
    The kernel is applied to the U (original image matrix */
   /* Delta X is 1.0 here for cubic voxels */
   a = 1.0 / 6.0;   /* constants for kernel */
   b = 2.0 / 3.0;
   c = -10.0 / 3.0;

   /** load the grid parameters **/
   daxes = tempdset->daxes ;
   nx    = daxes->nxx ;
   ny    = daxes->nyy ;
   nz    = daxes->nzz ;
   nbriks = tempdset->dblk->nvals;

   for(i=0;i<nbriks; i++) {  /* for each sub-brik in dataset */
      data_im = DSET_BRICK (tempdset, i);  /* set pointer to the ith sub-brik of the dataset */
      ar =  Gptr = mri_data_pointer(data_im); /* ar is pointer to sub-brik, Gptr points to current voxel */
      Gvalptr = mri_data_pointer(G_Im->imarr[0]); /* reset G matrix pointer back to beginning */
      tempmaskptr = maskptr; /* reset mask pointer */
      for(j=0;j<nz;j++) {      /* for each slice in sub-brik */
        for(k=0;k<ny;k++) {    /*   for each row */
	  for(l=0;l<nx;l++) {  /* for each column */
            if(maskptr && !(*tempmaskptr++)) {
               *Gptr = 0.0;
            }
            else {
            uval = vox_val(l,k,j, ar, nx, ny, nz, maskptr,l,k,j);
	    Fval = Dmean * (a * (vox_val(l-1,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k+1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k+1,j, ar, nx, ny, nz, maskptr,l,k,j)) + \
                      b * (vox_val(l,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k+1,j, ar, nx, ny, nz, maskptr,l,k,j)) + \
	              c * uval);
            *Gptr = uval + DeltaT  * (Fval + *Gvalptr);
            }
            Gptr++; Gvalptr++;
	  }
	}
      }
   }

   EXRETURN;
}

/* update values of a dataset sub-brik with values from another single brik dataset*/
static void
Update_Brik(THD_3dim_dataset *indset, THD_3dim_dataset *outdset, int brickn)
/* assumes float type data and same dimension nxyz for data */
{
   int nxyz;
   float *in_ar, *out_ar;
   MRI_IMAGE *data_im;
 
   ENTRY("Update_Brik");
   data_im = DSET_BRICK (indset, 0);  /* 1st sub-brik of the input dataset */
   nxyz = data_im->nxyz;
   in_ar = mri_data_pointer(data_im); /* in_ar is pointer to data */
   data_im = DSET_BRICK(outdset, brickn); 
   out_ar = mri_data_pointer(data_im); /* out_ar is pointer to output data */
   memcpy(out_ar, in_ar, nxyz*sizeof(MRI_float));
   EXRETURN;
}

static void
Test_data(THD_3dim_dataset *indset)
{
   int i, j, nxyz;
   MRI_IMAGE *data_im;
   float *in_ar, uval;

   data_im = DSET_BRICK (indset, 0);  /* 1st sub-brik of the input dataset */
   nxyz = data_im->nxyz;

   for(i=0;i<3;i++) {
     if(i==1) uval = 0.0;  /* fill 2nd sub-brik with 0 */
     if(i==0) uval = 1.0;  /* fill 1st sub-brik with 1 */
     if(i==2) uval = 1;/* fill 3rd sub-brik with 1 */
     data_im = DSET_BRICK (indset, i);  /* 1st sub-brik of the input dataset */
     in_ar = mri_data_pointer(data_im); /* in_ar is pointer to data */
     for(j=0;j<nxyz;j++) {
	*in_ar = uval;
        in_ar++;
     } 
   }	 
}
