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

#ifdef __GNUC__
/*  inline used to make macro-equivalent speed functions */
/* but only available for gcc */
   #define INLINE   inline
#else
   #define INLINE   /**/
#endif
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
static void Compute_Smooth(THD_3dim_dataset *udset, int outbrik,THD_3dim_dataset *tempdset,MRI_IMARR *G_Im,int flag2D3D, byte *maskptr);
static void Compute_Flux(MRI_IMARR *Gradient_im, THD_3dim_dataset *structtensor, int flag2D3D, byte *maskptr);
static void Update_Brik(THD_3dim_dataset *indset, THD_3dim_dataset *outdset, int brickn);
static void Test_data(THD_3dim_dataset *structtensor);
static void Fix_mask(byte *maskptr, THD_3dim_dataset *dset, int flag2D3D);
static int Check_Neighbors_3D(byte *mptr, int nx, int nxy);
static int Check_Neighbors_2D(byte *mptr, int nx);


extern THD_3dim_dataset *
DWIstructtensor(THD_3dim_dataset * DWI_dset, int flag2D3D, byte *maskptr, int smooth_flag, int save_tempdsets_flag);
extern MRI_IMARR *Compute_Gradient_Matrix(THD_3dim_dataset *DWI_dset, int flag2D3D, byte *maskptr, int prodflag, int smoothflag,
float smooth_factor);
extern MRI_IMARR *Compute_Gradient_Matrix_Im(MRI_IMAGE *SourceIm, int flag2D3D, byte *maskptr, int xflag, int yflag, int zflag);
static INLINE float vox_val(int x,int y,int z,float *imptr, int nx, int ny, int nz, byte *maskptr, int i, int j, int k);
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
   MRI_IMAGE *data_im = NULL;

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
   PRINT_VERSION("3danisosmooth"); AUTHOR("Daniel Glen");
   

  datum = MRI_float;
   compute_method = -1;  /* detect multiple or default selection of compute_method */

   deltatflag = -1.0;    /* pseudo-time step */   
   while( nopt < argc && argv[nopt][0] == '-' ){
      if( strcmp(argv[nopt],"-prefix") == 0 ){
	  if (++nopt >= argc)
	    {
	      ERROR_exit("Error - prefix needs an argument!");
	    }
	  MCW_strncpy (prefix, argv[nopt], THD_MAX_PREFIX);	/* change name from default prefix */
	  if (!THD_filename_ok (prefix))
	    {
	      ERROR_exit("Error - %s is not a valid prefix!", prefix);
	    }
	  nopt++;
	  continue;
      }
     if (strcmp (argv[nopt], "-automask") == 0)
	{
         if(maskptr != NULL){
           ERROR_exit("ERROR: can't use -mask with -automask!");
         }
 	  automask = 1;
	  nopt++;
	  continue;
	}

      if( strcmp(argv[nopt],"-mask") == 0 ){
         if( automask ){
           ERROR_exit("ERROR: can't use -mask with -automask!");
          }
         mask_dset = THD_open_dataset(argv[++nopt]) ;
         if( mask_dset == NULL ){
            ERROR_exit("ERROR: can't open -mask dataset!");
         }
         if( maskptr != NULL ){
            ERROR_exit("ERROR: can't have 2 -mask options!");
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
            ERROR_exit("ERROR: can't select both 2D and 3D flags");
          }
          nopt++;
          continue;
        }

      if (strcmp (argv[nopt], "-3D") == 0)
        {
          if(flag2D3D==0)
             flag2D3D = 3;
          else {
            ERROR_exit("can't select both 2D and 3D flags");
            
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
	      ERROR_exit("Error - need an argument after -iters!");
	      
	   }
           iters = strtol(argv[nopt], NULL, 10);
	   if ((iters <1)||(iters>200)) {
	      ERROR_exit("Error - iters must be between 1 and 200");
	     
           }
          nopt++;
	  continue;
      }

     if( strcmp(argv[nopt],"-deltat") == 0 ){
	   if(++nopt >=argc ){
	      ERROR_exit("Error - need an argument after -iters!");
	      
	   }
           deltatflag = atof(argv[nopt]);
	   if (deltatflag <0) {
	      ERROR_exit( "Error - deltatflag must be positive!");
	     
           }
          nopt++;
	  continue;
      }

     if( strcmp(argv[nopt],"-phiding") == 0 ){
           if(compute_method!=-1) {
	      ERROR_exit("Error - can not specify two compute methods!");
	   }
           compute_method  = 0;
	   nopt++;
	   continue;
     }

     if( strcmp(argv[nopt],"-phiexp") == 0 ){
           if(compute_method!=-1) {
	      ERROR_exit("Error - can not specify two compute methods!");
	   }
           compute_method  = 1;
	   nopt++;
	   continue;
     }
     
     ERROR_exit( "Error - unknown option %s", argv[nopt]);
 

   }


   /*----- read input dataset -----*/

   if( nopt >= argc ){
      ERROR_exit("No input dataset!?"); 
   }

   dset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(dset) ){
      ERROR_exit("Can't open dataset %s",argv[nopt]);
   }

   nxyz = DSET_NVOX(dset) ;
   if( maskptr != NULL && mmvox != nxyz ){
      ERROR_exit("Mask and input datasets not the same size!") ;
   }

   if(automask && (maskptr == NULL )){
      maskptr = THD_automask( dset ) ;
   }

   if(flag2D3D == 0)
     flag2D3D = 2;    /* make default 2D processing for speed */ 
   if(maskptr)
      Fix_mask(maskptr, dset, flag2D3D);  /* set mask edge voxels to 2, all others to 1 */

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
        ERROR_message("+++aiv has too many ports open");
      }
      else { 
         sprintf(tempstring,"aiv -p %d &", port);
         ret = system(tempstring); /* use the aiv program to display a slice */

         if(ret==0)
            ret = Smooth_Open_Stream(port); /* Open display stream */
         if (ret!=0) {
            afnitalk_flag = 0;
            ERROR_message("+++could not open communications with aiv");
         }
      }
  }

   if(compute_method==-1)
      compute_method = 0;
      
   INFO_message("loading original data");
  /* load the original DWI dataset */
   DSET_mallocize (dset);
   DSET_load (dset);	                /* load dataset */

  /* copy to udset in floats */
  /* printf("Copying to float");*/
#if 0 
  data_im = DSET_BRICK (dset, 0); /* assume all data is same type as first sub-brik */
  if(data_im->kind==MRI_float) {   /* if data is already float, do not copy */
     udset = dset;
     EDIT_dset_items( dset ,
             ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
                	 ADN_prefix , prefix ,
                	 ADN_label1 , prefix ,
	        	 ADN_datum_all , MRI_float ,
                	 ADN_none ) ;
  }
  else {
#endif

     udset = Copy_dset_to_float(dset, prefix);
     tross_Copy_History (dset, udset);
     THD_delete_3dim_dataset(dset , False ) ;  /* do not need original anymore */

/*  }*/
    
  if(afnitalk_flag) {
      Show_dset_slice(udset);  /* show mid-slice in middle brik */
  }
 
  for(i=0;i<iters;i++){
     INFO_message("iteration %d", i);
     /* compute image diffusion tensor dataset */
     INFO_message("   computing structure tensor");
     structtensor =  DWIstructtensor(udset, flag2D3D, maskptr, smooth_flag, save_tempdsets_flag);
 /* Test_data(structtensor);*/
     if((i==iters-1)&&(save_tempdsets_flag)) {
       tross_Make_History ("3danisosmooth", argc, argv, structtensor);
       DSET_write (structtensor);
       INFO_message("--- Output dataset %s", DSET_BRIKNAME(structtensor));
     }
     INFO_message("    applying structure tensor");
     /* Smooth udset image using image diffusion tensor */
     Smooth_dset_tensor(udset, structtensor, flag2D3D, maskptr);

     /* display sample udset slice after smoothing for this iteration */
     if(afnitalk_flag) {
       Show_dset_slice(udset); /* show mid-slice in middle brik */
     }

     THD_delete_3dim_dataset( structtensor , False ) ;  /* delete tensor */
  }

  /* save the dataset */
  tross_Make_History ("3danisosmooth", argc, argv, udset);
  THD_load_statistics( udset );
  DSET_write (udset);
  INFO_message("--- Output dataset %s", DSET_BRIKNAME(udset));

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



   /* make new Image Array */
   INIT_IMARR(fim_array);

   for( iv=0 ; iv < dset->dblk->nvals ; iv++ ){
      /*- get sub-brick -*/
      fim = THD_extract_float_brick( iv , dset ) ;
      if( fim == NULL ) {
	ERROR_exit("*Error - can not allocated float dset");
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
   far = (float *) mri_data_pointer(data_im);
   nz = (nz-1) / 2;      /* get middle slice in integer */
   far += nx*ny*nz;      /* show middle brik, middle Z slice */
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

  /* find mean and max of (Dxx+Dyy)*/
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
       ERROR_message( "Can not create temporary dataset in Smooth_dset_tensor");
       EXRETURN;
     }

     /* compute gradient of tempdset */
     /*printf("Compute Gradient_Matrix\n");*/
     Gradient_Im = Compute_Gradient_Matrix(tempdset, flag2D3D, maskptr,0,0,0.0);
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
/*     printf("Compute isotropic Fmatrix and final smooth sub-brik %d\n", i);*/
     Compute_Smooth(udset, i, tempdset, Gradient_Im, flag2D3D, maskptr);
     /*Update_Brik(tempdset, udset, i); */ /* update smoothed values of current brik */
     THD_delete_3dim_dataset( tempdset, False ) ;  /* delete temporary dset */
     DESTROY_IMARR(Gradient_Im);
   }
}

/*! find mean and max of (Dxx+Dyy(+Dzz))*/
static void Compute_Dstats(THD_3dim_dataset *structtensor,int flag2D3D, byte *maskptr)
{
  int i, nvox;
  double s0, s1, ts0;
  double ts1, ts2, ts3;
  MRI_IMAGE *data_im = NULL;
  float *dx, *dy, *dz;
  byte *tempmaskptr;

  ENTRY("Compute_Dstats");

  tempmaskptr = maskptr;
  s0 = 0.0; s1 = -1E10;
  ts1 = 0.0; ts2 = 0.0; ts3 = 0.0;
  data_im = DSET_BRICK(structtensor, 0);
  dx = (float *) mri_data_pointer(data_im);
  if(flag2D3D==2) {
     data_im = DSET_BRICK(structtensor, 2);
     dy = (float *) mri_data_pointer(data_im);
     dz = dy;
  }
  else {
     data_im = DSET_BRICK(structtensor, 3);
     dy = (float *) mri_data_pointer(data_im);
     data_im = DSET_BRICK(structtensor, 5);
     dz = (float *) mri_data_pointer(data_im);
  }

  nvox = 0;
  if(flag2D3D==2) {
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
  }
  else {   /* 3D option */
    for(i=0;i<data_im->nxyz;i++) {
      if(maskptr && !(*tempmaskptr++)) {
	dx++; dy++; dz++;
      }
      else {
        nvox++;
        ts0 = *dx + *dy + *dz;
        ts1 = *dx;
        ts2 = *dy;
        ts3 = *dz;
        if(isnan(ts1)||isnan(ts2)||isnan(ts3)) {
	  WARNING_message("D matrix has elements that are not numbers (NAN) at point %d", i);
          WARNING_message("ts1 %g ts2 %g ts3 %g", ts1, ts2, ts3);
        }

        dx++; dy++; dz++;
        if(ts0>s1)
          s1 = ts0;           /* update max(Dxx + Dyy + Dzz)*/
        s0 +=  ts0;           /* get sum of Dxx + Dyy + Dzz */
      }
    }
  }

  /* if(maskptr!=NULL) 
     nvox = data_im->nxyz;*/
  if(nvox==0)
    Dmean = 0.0;
  else
    Dmean = s0 / (flag2D3D * nvox);  /* Dmean = 1/2 or 1/3 mean(Dxx+Dyy(+Dzz)) */
  if(deltatflag==-1.0) {      /* if no user setting for delta T */
     Dmax = s1;
  /* DeltaT  = 1.0/7.0; */
     DeltaT  = Dmax / 4;      /*   set pseudo-time step to Dmax/4 */
  }
  else DeltaT = deltatflag;
  /*printf("s0 %g, nvox %d, s1 %g, ts0 %g\n", s0, nvox,  s1, ts0);
      printf("Dmean %g   Dmax %g   DeltaT %g\n", Dmean, Dmax, DeltaT);*/

  EXRETURN;
}

/*! deviation from mean, can use structtensor space */
static void Compute_Ematrix(THD_3dim_dataset *structtensor, int flag2D3D, byte *maskptr)
{

  int i;
  MRI_IMAGE *data_im = NULL;
  float *e0, *e2, *e3, tempe0, tempe2;
  byte *tempmaskptr;

  ENTRY("Compute_Ematrix");

  tempmaskptr = maskptr;
  data_im = DSET_BRICK(structtensor, 0);
  e0 = (float *) mri_data_pointer(data_im);
  /* e1 = Dxy, so just leave in place at second sub-brick */
  /* for 3D, we also leave 3rd, 5th sub-brick in place */
  if(flag2D3D==2) {
   data_im = DSET_BRICK(structtensor, 2);
   e2 = (float *) mri_data_pointer(data_im);

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
  }
  else {
   data_im = DSET_BRICK(structtensor, 3);
   e2 = (float *) mri_data_pointer(data_im);
   data_im = DSET_BRICK(structtensor, 5);
   e3 = (float *) mri_data_pointer(data_im);

   for(i=0;i<data_im->nxyz;i++) {
     if(maskptr && !(*tempmaskptr++)) {
       *e0 = 0.0; *e2 = 0.0; *e3 = 0.0;
     }
     else {
       *e0 = *e0 - Dmean;    /* e0 = Dxx-Dmean */
       *e2 = *e2 - Dmean;    /* e2 = Dyy-Dmean */
       *e3 = *e3 - Dmean;    /* e3 = Dzz-Dmean */
     }
     e0++; e2++; e3++;
   }

  }
  EXRETURN;
}

/*! Compute flux */
static void Compute_Flux(MRI_IMARR * Gradient_Im, THD_3dim_dataset *structtensor, int flag2D3D, byte *maskptr)
{
  int i;
  MRI_IMAGE *data_im = NULL;
  float *e0,*e1,*e2, *e3, *e4, *e5, *Gx, *Gy, *Gz;
  double Jx, Jy, Jz;
  byte *tempmaskptr;

  ENTRY("Compute_Flux");

  tempmaskptr = maskptr;
  data_im = DSET_BRICK(structtensor, 0);
  e0 = (float *) mri_data_pointer(data_im);
  data_im = DSET_BRICK(structtensor, 1);
  e1 = (float *) mri_data_pointer(data_im);
  data_im = DSET_BRICK(structtensor, 2);
  e2 = (float *) mri_data_pointer(data_im);
  Gx = (float *) mri_data_pointer(Gradient_Im->imarr[0]);
  Gy = (float *) mri_data_pointer(Gradient_Im->imarr[1]);

  if(flag2D3D==2) {
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
  }
  else {
    data_im = DSET_BRICK(structtensor, 3);
    e3 = (float *) mri_data_pointer(data_im);
    data_im = DSET_BRICK(structtensor, 4);
    e4 = (float *) mri_data_pointer(data_im);
    data_im = DSET_BRICK(structtensor, 5);
    e5 = (float *) mri_data_pointer(data_im);

    Gz = (float *) mri_data_pointer(Gradient_Im->imarr[2]);
    for(i=0;i<data_im->nxyz;i++) {
      if(maskptr && !(*tempmaskptr++)) {
        *Gx = 0.0;
        *Gy = 0.0;
        *Gz = 0.0;
      }
      else {
        Jx =  (*e0 * *Gx) + (*e1 * *Gy) + (*e2 * *Gz);   /* Jx = Exx * du/dx + Exy * du/dy  + Exz * du/dz*/
        Jy =  (*e1 * *Gx) + (*e3 * *Gy) + (*e4 * *Gz);   /* Jy = Exy * du/dx + Eyy * du/dy + Eyz * du/dz*/
        Jz =  (*e2 * *Gx) + (*e4 * *Gy) + (*e5 * *Gz);   /* Jz = Exz * du/dx + Eyz * du/dy + Ezz * du/dz*/

        *Gx = Jx;         /* replace gradient values with flux values */
        *Gy = Jy;
        *Gz = Jz;
      }
      Gx++; Gy++; Gz++;
      e0++; e1++; e2++; e3++; e4++; e5++;
     }
  }

  EXRETURN;
}

/*! Compute anisotropic component of smoothing, G */
static void Compute_Gmatrix(MRI_IMARR * Flux_Im, int flag2D3D, byte *maskptr)
{
/*   put in Flux_Im space - first sub-brik */
/* compute gradient of Jx, Jy (first two sub-briks) */
/* G = dJx/dx + dJy/dy */
/* for 3D compute gradient of Jx, Jy, Jz */
/* G = dJx/dx + dJy/dy */

   int i, nxyz;
   float *dJx, *dJy, *dJz, *Gptr;
   MRI_IMAGE *data_im;
   MRI_IMARR *tempimarr0, *tempimarr1, *tempimarr2;
   byte *tempmaskptr;

   ENTRY("Compute_Gmatrix");
   tempmaskptr = maskptr;
   data_im = Flux_Im->imarr[0];
   Gptr = (float *) mri_data_pointer(data_im);
   nxyz = data_im->nxyz;
   tempimarr0 = Compute_Gradient_Matrix_Im(data_im, flag2D3D, maskptr,1,0,0);  /* dJx/dx */
   data_im = Flux_Im->imarr[1];
   tempimarr1 = Compute_Gradient_Matrix_Im(data_im, flag2D3D, maskptr,0,1,0);  /* dJy/dy */

   dJx = (float *) mri_data_pointer(tempimarr0->imarr[0]);
   dJy = (float *) mri_data_pointer(tempimarr1->imarr[0]);
   dJz = dJy; /* if unused pointer - faster than checking on increment */

   if(flag2D3D==3) {
     data_im = Flux_Im->imarr[2];
     tempimarr2 = Compute_Gradient_Matrix_Im(data_im, flag2D3D, maskptr,0,0,1);  /* dJz/dz */
     dJz = (float *) mri_data_pointer(tempimarr2->imarr[0]);
   }

   for(i=0;i<nxyz;i++) {
      if(maskptr && !(*tempmaskptr++)) {
	*Gptr = 0.0;
      }
      else {
       *Gptr = *dJx + *dJy;  /* G = dJx + dJy */
       if(flag2D3D==3) {
         *Gptr += *dJz;
       }
      }
      Gptr++;
      dJx++; dJy++; dJz++;
   }

   /* delete tempimarrs */
   if(flag2D3D==3)
     DESTROY_IMARR(tempimarr2);
   DESTROY_IMARR(tempimarr1);
   DESTROY_IMARR(tempimarr0);

   EXRETURN;
}

/*! update dset with new smoothed image */
static void Compute_Smooth_old(THD_3dim_dataset *tempdset, MRI_IMARR *G_Im, int flag2D3D,byte * maskptr)
{
   byte *tempmaskptr;
   int nx, ny, nz, nbriks, i,j,k,l;
   double a, b, c, d;
   THD_dataxes   * daxes ;
   float *Gptr, *ar, *Gvalptr;
   MRI_IMAGE *data_im;
   double uval, Fval;

   ENTRY("Compute_Smooth");
   /* compute isotropic diffusion component of smooth, F and then overall smooth*/
   /* F = (Dmean / DeltaX^2) * [ 1/6  2/3  1/6]
                              [ 2/3 -10/3 2/3]
                              [ 1/6  2/3  1/6]U 

    The kernel for 3D is 3 3x3 kernel stencils:

    b a b                 c b c
    a d a                 b a b
    b a b                 c b c

    at slice p       at slices p+/-1 
    a, b, c, d values are listed below.
    The kernel is applied to the U (original image matrix */
   /* Delta X is 1.0 here for cubic voxels */
   if(flag2D3D==2) {
   a = 1.0 / 6.0;   /* constants for 2D kernel */
   b = 2.0 / 3.0;
   c = -10.0 / 3.0;
   }
   else {   /* constants for 3D kernel */
     a = 0.4;
     b = 2.0/15.0;
     c = 1.0/60.0;
     d = (-6.0 * a) - (12.0 * b) - (8.0 * c);
   }
   /** load the grid parameters **/
   daxes = tempdset->daxes ;
   nx    = daxes->nxx ;
   ny    = daxes->nyy ;
   nz    = daxes->nzz ;
   nbriks = tempdset->dblk->nvals;

   for(i=0;i<nbriks; i++) {  /* for each sub-brik in dataset */
      data_im = DSET_BRICK (tempdset, i);  /* set pointer to the ith sub-brik of the dataset */
      ar =  Gptr = (float *) mri_data_pointer(data_im); /* ar is pointer to sub-brik, Gptr points to current voxel */
      Gvalptr = (float *) mri_data_pointer(G_Im->imarr[0]); /* reset G matrix pointer back to beginning */
      tempmaskptr = maskptr; /* reset mask pointer */
      for(j=0;j<nz;j++) {      /* for each slice in sub-brik */
        for(k=0;k<ny;k++) {    /*   for each row */
	  for(l=0;l<nx;l++) {  /* for each column */
            if(maskptr && !(*tempmaskptr++)) {
               *Gptr = 0.0;
            }
            else {
            uval = vox_val(l,k,j, ar, nx, ny, nz, maskptr,l,k,j);
            if(flag2D3D==2)
  	       Fval = Dmean * (a * (vox_val(l-1,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k+1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k+1,j, ar, nx, ny, nz, maskptr,l,k,j)) + \
                      b * (vox_val(l,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k+1,j, ar, nx, ny, nz, maskptr,l,k,j)) + \
	              c * uval);
            else {
	      /* multiply by 'a' four voxel values in current slice and in 
                 centers of slices before and after current slice */
  	       Fval = a * (vox_val(l,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k+1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k,j+1, ar, nx, ny, nz, maskptr,l,k,j));
               /* 'b' * corners of kernel stencil in current slice
                     and centers of edges on previous and following slices */
               Fval += b * (vox_val(l-1,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k+1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k+1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k-1,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k+1,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k-1,j+1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k,j+1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k,j+1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k+1,j+1, ar, nx, ny, nz, maskptr,l,k,j));
               /* 'c' * corners of kernel stencil on previous and following slices */
               Fval += c * (vox_val(l-1,k-1,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k-1,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k+1,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k+1,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k-1,j+1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k-1,j+1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k+1,j+1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k+1,j+1, ar, nx, ny, nz, maskptr,l,k,j));
               /* 'd' * voxel value at current point only */
               Fval += d * uval;
               /* scale by Dmean */
               Fval *= Dmean;
            }

            *Gptr = uval + DeltaT  * (Fval + *Gvalptr);
            }
            Gptr++; Gvalptr++;
	  }
	}
      }
   }

   EXRETURN;
}
 
 
#if 1
/*! update dset with new smoothed image */
static void Compute_Smooth(THD_3dim_dataset *udset, int outbrik, THD_3dim_dataset *tempdset, MRI_IMARR *G_Im,
 int flag2D3D,byte * maskptr)
{
   byte *tempmaskptr;
   int nx, ny, nz, nxyz;
   double a, b, c, d;
   THD_dataxes   * daxes ;
   float *Gvalptr, *ar, *ar2;
   MRI_IMAGE *data_im;
   double uval, Fval;
   float *tempptr, *vptr0, *vptr1, *vptr, *vptr2, *vptr3, *vptr4, *vptr5, *vptr6, *vptr7, *vptr8;
   float v0, v1, v2, v3, v4, v5, v6, v7, v8;
   float v9, v10, v11, v12, v13, v14, v15, v16, v17, v18;
   float v19, v20, v21, v22, v23, v24, v25, v26;

   float sv0, sv1, sv2;
   int vx,vy,vz, nxm1, nym1, nzm1,nxy;  
   int maskflag, baseoffset;
   float sv00061824, sv01071925, sv02082026, sv0915, sv1016, sv1117, sv0321, sv0422, sv0523;
   
   ENTRY("Compute_Smooth");
   /* compute isotropic diffusion component of smooth, F and then overall smooth*/
   /* F = (Dmean / DeltaX^2) * [ 1/6  2/3  1/6]
                              [ 2/3 -10/3 2/3]
                              [ 1/6  2/3  1/6]U 

    The kernel for 3D is 3 3x3 kernel stencils:

    b a b                 c b c
    a d a                 b a b
    b a b                 c b c

    at slice p       at slices p+/-1 
    a, b, c, d values are listed below.
    The kernel is applied to the U (original image matrix */
   /* Delta X is 1.0 here for cubic voxels */
   if(flag2D3D==2) {
   a = 1.0 / 6.0;   /* constants for 2D kernel */
   b = 2.0 / 3.0;
   c = -10.0 / 3.0;
   }
   else {   /* constants for 3D kernel */
     a = 0.4;
     b = 2.0/15.0;
     c = 1.0/60.0;
     d = (-6.0 * a) - (12.0 * b) - (8.0 * c);
   }

  /** load the grid parameters **/
   data_im = DSET_BRICK (tempdset, 0);
   nx = data_im->nx; ny = data_im->ny; nz = data_im->nz; nxyz = data_im->nxyz;
   nxy = nx * ny;
   
   /* precompute offsets for each stencil point relative to the center point */
   nxm1 = nx - 1;
   nym1 = ny - 1;
   nzm1 = nz - 1;
   
   baseoffset = 0;

      data_im = DSET_BRICK (tempdset, 0);  /* set pointer to the 0th sub-brik of the dataset */
      ar  = (float *) mri_data_pointer(data_im); /* Gptr points to current voxel */
      Gvalptr = ar2 =(float *) mri_data_pointer(G_Im->imarr[0]); /*set G matrix pointer*/
      data_im = DSET_BRICK(udset, outbrik); /* update output dataset here */
      tempptr =  (float *) mri_data_pointer(data_im);  /*  put calculated values here in output dataset */
      tempmaskptr = maskptr;   /* reset mask pointer */
     
   if(flag2D3D==2) {
      for(vz=0;vz<nz;vz++) {
         for(vy=0;vy<ny;vy++) {
            for(vx=0;vx<nx;vx++) {
               if(maskptr){    /*  check if point is in mask or not */
		  maskflag = *tempmaskptr++;
		  if(!maskflag) {
        	      baseoffset++;
		      vptr++;
		      vptr0++;
		      vptr1++;
		      Gvalptr++;
                      *tempptr++ = 0.0f;      
		      continue;
        	  }
        	}

        	if((maskflag==2) || (vx<1) || (vx==nxm1) || (vy<=1) || (vy==nym1)){   /* special cases at edges */
		    /* get voxels for 3x3 stencil */
		    vptr = (float *) ar + baseoffset;
		    v4 = *vptr++; /* get central value at voxel and move pointer to right */
		    /* set first row of 3 voxels and vptr0 */
                    if(vy==0) {
		       v0 = v1 = v2 = v4; /* all are orig voxel value, don't need vptr0*/
		    } 
                    else {
		       v0 = vox_val(vx-1, vy-1, vz, ar, nx, ny, nz, maskptr, vx,vy,vz);
		       v1 = vox_val(vx, vy-1, vz, ar, nx, ny, nz, maskptr, vx,vy,vz);
		       v2 = vox_val(vx+1, vy-1, vz, ar, nx, ny, nz, maskptr, vx,vy,vz);
                       vptr0 = vptr - nx;  /* init pointer for first row */		     
                    }
                    /* middle row of voxels */
	   	    v3 = vox_val(vx-1, vy, vz, ar, nx, ny, nz, maskptr, vx, vy, vz);
		    v5 = vox_val(vx+1, vy, vz, ar, nx, ny, nz, maskptr, vx, vy, vz);

                    if(vy==nym1) {
		       v6 = v7 = v8 = v4;/* all are orig voxel value, don't need vptr1*/
		    }
                    else {
		       v6 = vox_val(vx-1, vy+1, vz, ar, nx, ny, nz, maskptr, vx, vy, vz);
		       v7 = vox_val(vx, vy+1, vz, ar, nx, ny, nz, maskptr, vx,vy,vz);
		       v8 = vox_val(vx+1, vy+1, vz, ar, nx, ny, nz, maskptr, vx,vy,vz);
                       vptr1 = vptr + nx;  /* init pointer for third row */		     
                    }
		    sv0 = v0 + v6;   /* initialize sums of equivalent column components */
		    sv1 = v1 + v7;
		    sv2 = v2 + v8;

		}
        	else {
                   /* row before voxel */
		   /*v0 = v1;*/
		   /*v1 = v2;*/
		   v2 = *(++vptr0);
	   
                  /* same row as voxel */
                   v3 = v4;
		   v4 = v5;
	           v5 = *(++vptr);
                   /* row after voxel */
                   /*v6 = v7;*/
                   /*v7 = v8;*/
		   v8 = *(++vptr1);
		   sv0 = sv1;          /* slide sums */
		   sv1 = sv2;
		   sv2 = v2 + v8;
                }

/*    	        Fval = Dmean * ((a * (v0 + v2 + v6 + v8)) + (b * (v1 + v3 + v5 + v7)) + c*v4);*/
    	        Fval = Dmean * ((a * (sv0 + sv2)) + (b * (sv1 + v3 + v5)) + c*v4);

        	*tempptr++ =  v4 + DeltaT  *  (Fval + *Gvalptr);
        	Gvalptr++; 
        	baseoffset++;
  	 }
       }
     }
   }
   else {    /* 3D version */
       for(vz=0;vz<nz;vz++) {
          for(vy=0;vy<ny;vy++) {
	     for(vx=0;vx<nx;vx++) {
               if(maskptr) {
	         maskflag = *tempmaskptr++;
		 if (!maskflag) {    /*  check if point is in mask or not */
		    baseoffset++;
		    vptr0++;
		    vptr1++;
		    vptr2++;
		    vptr3++;
		    vptr++;
		    vptr5++;
		    vptr6++;
		    vptr7++;
		    vptr8++;
		    Gvalptr++;
                    *tempptr++ = 0.0f;      
		    
                    continue;
                 }
		 /* edge of mask treat special if value in mask is 2 and not 1*/
               }
	       
   
               if((maskflag==2) || (vx<1) || (vy<=1) || (vx==nxm1) || (vy==nym1) || (vz<=1)
	       || (vz==nzm1)){   /* special cases at edges */
		  /* get voxels for 3x3 stencil  in central slice as before */
		  vptr = ar + baseoffset;
		  v13 = *vptr++; /* get central value at voxel and move pointer to right */
		  /* set first row of 3 voxels and vptr0 */
                  if(vy==0) {
		     v0 = v1 = v2 = v9 = v10 = v11 = v18 = v19 = v20 = v13; /* all are orig voxel value, don't need vptr0*/
                     vptr0 = vptr3 = vptr6 = vptr;
		  } 
                  else {
		     v9 = vox_val(vx-1, vy-1, vz, ar, nx, ny, nz, maskptr, vx,vy,vz);
		     v10 = vox_val(vx, vy-1, vz, ar, nx, ny, nz, maskptr, vx,vy,vz);
		     v11 = vox_val(vx+1, vy-1, vz, ar, nx, ny, nz, maskptr, vx,vy,vz);
                     vptr3 = vptr - nx;  /* init pointer for first row */		     
                  }

                  /* middle row of voxels */
		  v12 = vox_val(vx-1, vy, vz, ar, nx, ny, nz, maskptr, vx, vy, vz);
		  v14 = vox_val(vx+1, vy, vz, ar, nx, ny, nz, maskptr, vx, vy, vz);

                  if(vy==nym1) {
		     v6 = v7 = v8 = v15 = v16 = v17 = v24 = v25 = v26 = v13;/* all are orig voxel value, don't need vptr1*/
                     vptr5 = vptr2 = vptr8 = vptr;
		  }
                  else {
		     v15 = vox_val(vx-1, vy+1, vz, ar, nx, ny, nz, maskptr, vx, vy, vz);
		     v16 = vox_val(vx, vy+1, vz, ar, nx, ny, nz, maskptr, vx,vy,vz);
		     v17 = vox_val(vx+1, vy+1, vz, ar, nx, ny, nz, maskptr, vx,vy,vz);
                     vptr5 = vptr + nx;  /* init pointer for third row */		     
                  }
		  
	  
		  /* now get values from z-1 slice */
   		  /* get voxels for 3x3 stencil  in central slice as before */
		  if(vz==0){
                     v0 = v1 = v2 = v3 = v4 = v5 = v6 =v7 = v8 = v13;
		     vptr0 = vptr3;
		     vptr1 = vptr;
		     vptr2 = vptr5;
		  }
		  else {
		      vptr1 = vptr - nxy;
		      /* set first row of 3 voxels and vptr0 */
                      if(vy!=0) {
			 v0 = vox_val(vx-1, vy-1, vz-1, ar, nx, ny, nz, maskptr, vx,vy,vz);
			 v1 = vox_val(vx, vy-1, vz-1, ar, nx, ny, nz, maskptr, vx,vy,vz);
			 v2 = vox_val(vx+1, vy-1, vz-1, ar, nx, ny, nz, maskptr, vx,vy,vz);
                	 vptr0 = vptr1 - nx;  /* init pointer for first row */		     
                      }

                      /* middle row of voxels */
		      v3 = vox_val(vx-1, vy, vz-1, ar, nx, ny, nz, maskptr, vx, vy, vz);
		      v4 = vox_val(vx, vy, vz-1, ar, nx, ny, nz, maskptr, vx, vy, vz);
		      v5 = vox_val(vx+1, vy, vz-1, ar, nx, ny, nz, maskptr, vx, vy, vz);

                      if(vy!=nym1) {
			 v6 = vox_val(vx-1, vy+1, vz-1, ar, nx, ny, nz, maskptr, vx, vy, vz);
			 v7 = vox_val(vx, vy+1, vz-1, ar, nx, ny, nz, maskptr, vx,vy,vz);
			 v8 = vox_val(vx+1, vy+1, vz-1, ar, nx, ny, nz, maskptr, vx,vy,vz);
                	 vptr2 = vptr1 + nx;  /* init pointer for third row */		     
                      }
                  }                      
  	          /* now get values from z+1 slice */
		  if(vz==nzm1){  /* last slice in volume */
                     v18 = v19 = v20 = v21 = v22 = v23 = v24 =v25 = v26 = v13;
		     vptr6 = vptr3;
		     vptr7 = vptr;
		     vptr8 = vptr5;
		  }
		  else {
		     vptr7 = vptr + nxy;
		     /* set first row of 3 voxels and vptr0 */
                     if(vy!=0) {
			v18 = vox_val(vx-1, vy-1, vz+1, ar, nx, ny, nz, maskptr, vx,vy,vz);
			v19 = vox_val(vx, vy-1, vz+1, ar, nx, ny, nz, maskptr, vx,vy,vz);
			v20 = vox_val(vx+1, vy-1, vz+1, ar, nx, ny, nz, maskptr, vx,vy,vz);
                	vptr6 = vptr7 - nx;  /* init pointer for first row */		     
                     }

                     /* middle row of voxels */
		     v21 = vox_val(vx-1, vy, vz+1, ar, nx, ny, nz, maskptr, vx, vy, vz);
		     v22 = vox_val(vx, vy, vz+1, ar, nx, ny, nz, maskptr, vx, vy, vz);
		     v23 = vox_val(vx+1, vy, vz+1, ar, nx, ny, nz, maskptr, vx, vy, vz);

                     if(vy!=nym1) {
			v24 = vox_val(vx-1, vy+1, vz-1, ar, nx, ny, nz, maskptr, vx, vy, vz);
			v25 = vox_val(vx, vy+1, vz-1, ar, nx, ny, nz, maskptr, vx,vy,vz);
			v26 = vox_val(vx+1, vy+1, vz-1, ar, nx, ny, nz, maskptr, vx,vy,vz);
                	vptr8 = vptr7 + nx;  /* init pointer for third row */		     
                     }
	          }
                 
		 
		 sv00061824 = v0 + v6 + v18 + v24;
		 sv01071925 = v1 + v7 + v19 + v25;
		 
                 sv0915 = v9 + v15;
		 sv1016 = v10 + v16;
		 
		 sv0321 = v3 + v21;
		 sv0422 = v4 + v22;

            }
            else {  /* x>=1, y>=2 */
	         /* z-1 slice */
		 /*v0 = v1;
		 v1 = v2;*/
		 v2 = *(++vptr0);

                 /*v3 = v4;
		 v4 = v5;*/
	         v5 = *(++vptr1);

                 /*v6 = v7;
                 v7 = v8;*/
		 v8 = *(++vptr2);
		 
                 /*z slice */
		 /*v9 = v10;
		 v10 = v11;*/
		 v11 = *(++vptr3);

                 v12 = v13;
		 v13 = v14;
	         v14 = *(++vptr);

                 /*v15 = v16;
                 v16 = v17;*/
		 v17 = *(++vptr5);
		 
		 /* z+1 slice */
		 /*v18 = v19;
		 v19 = v20;*/
		 v20 = *(++vptr6);

                 /*v21 = v22;
		 v22 = v23;*/
	         v23 = *(++vptr7);

                 /*v24 = v25;
                 v25 = v26;*/
		 v26 = *(++vptr8);

		 sv00061824 = sv01071925;
		 sv01071925 = sv02082026;
		 
                 sv0915 = sv1016;
		 sv1016 = sv1117;
		 
		 sv0321 = sv0422;
		 sv0422 = sv0523;
	     }
  /*
  v0  v1  v2    v9  v10 v11    v18 v19 v20
  v3  v4  v5    v12 v13 v14    v21 v22 v23
  v6  v7  v8    v15 v16 v17    v24 v25 v26
     z-1            z              z+1
     */

         /* precomputing sums not only avoids redoing additions but also resetting voxel values
            not on sliding edge */
	  /* we only need to update voxels on right edge v2, v5, v8, v11, v14, v17, v20, v23, v26 */  
          /*  and update v12, v13 */
              sv02082026  = v2 + v8 + v20 + v26;
	      sv1117 = v11 + v17;
	      sv0523 = v5 + v23;
	      
	      /* multiply by 'a' four voxel values in current slice and in 
                 centers of slices before and after current slice */
	       Fval = a * (v12 + v14 + sv1016 + sv0422);
	       /*Fval = a * (v12 + v14 + v10 + v16 + v4 + v22);*/
               /* 'b' * corners of kernel stencil in current slice
                     and centers of edges on previous and following slices */
	       Fval += b * (sv0915 + sv1117 + sv01071925 + sv0321 + sv0523);
	       /*Fval += b * (v9 + v11 + v15 + v17 + v1 + v3 + v5 + v7 + v19 + v21 + v23 + v25);*/
               /* 'c' * corners of kernel stencil on previous and following slices */
	       Fval += c * (sv00061824 + sv02082026);
	       /*Fval += c * (v0 + v2  + v6 + v8 + v18 + v20 + v24 + v26);*/
               /* 'd' * voxel value at current point only */
	       Fval += d * v13;
               /* scale by Dmean */
	       Fval *= Dmean;

        	*tempptr++ =  v13 + DeltaT  *  (Fval + *Gvalptr);
        	Gvalptr++; 
        	baseoffset++;
     
       }   /* x */
      }  /* y */
     } /* z */		 

   }   

   EXRETURN;
}
 
#endif

#if 0
/* old way with calls to vox_val for each voxel and its neighbors */
/*! update dset with new smoothed image */
static void Compute_Smooth(THD_3dim_dataset *udset, int outbrik, THD_3dim_dataset *tempdset, MRI_IMARR *G_Im, int flag2D3D,byte * maskptr)
{
   byte *tempmaskptr;
   int nx, ny, nz, nbriks, i,j,k,l;
   double a, b, c, d;
   THD_dataxes   * daxes ;
   float *Gptr, *ar, *Gvalptr;
   MRI_IMAGE *data_im;
   double uval, Fval;

   ENTRY("Compute_Smooth");
   /* compute isotropic diffusion component of smooth, F and then overall smooth*/
   /* F = (Dmean / DeltaX^2) * [ 1/6  2/3  1/6]
                              [ 2/3 -10/3 2/3]
                              [ 1/6  2/3  1/6]U 

    The kernel for 3D is 3 3x3 kernel stencils:

    b a b                 c b c
    a d a                 b a b
    b a b                 c b c

    at slice p       at slices p+/-1 
    a, b, c, d values are listed below.
    The kernel is applied to the U (original image matrix */
   /* Delta X is 1.0 here for cubic voxels */
   if(flag2D3D==2) {
   a = 1.0 / 6.0;   /* constants for 2D kernel */
   b = 2.0 / 3.0;
   c = -10.0 / 3.0;
   }
   else {   /* constants for 3D kernel */
     a = 0.4;
     b = 2.0/15.0;
     c = 1.0/60.0;
     d = (-6.0 * a) - (12.0 * b) - (8.0 * c);
   }
   /** load the grid parameters **/
   daxes = tempdset->daxes ;
   nx    = daxes->nxx ;
   ny    = daxes->nyy ;
   nz    = daxes->nzz ;
   nbriks = tempdset->dblk->nvals;
 
   for(i=0;i<nbriks; i++) {  /* for each sub-brik in dataset */
      data_im = DSET_BRICK (tempdset, i);  /* set pointer to the ith sub-brik of the dataset */
      ar =  (float *) mri_data_pointer(data_im); /* ar is pointer to sub-brik*/
      data_im = DSET_BRICK(udset, outbrik);
      Gptr = (float *) mri_data_pointer(data_im); /* Gptr points to current voxel */
      Gvalptr = (float *) mri_data_pointer(G_Im->imarr[0]); /* reset G matrix pointer back to beginning */
      tempmaskptr = maskptr; /* reset mask pointer */
      for(j=0;j<nz;j++) {      /* for each slice in sub-brik */
        for(k=0;k<ny;k++) {    /*   for each row */
	  for(l=0;l<nx;l++) {  /* for each column */
            if(maskptr && !(*tempmaskptr++)) {
               *Gptr = 0.0;
            }
            else {
            uval = vox_val(l,k,j, ar, nx, ny, nz, maskptr,l,k,j);
            if(flag2D3D==2)
  	       Fval = Dmean * (a * (vox_val(l-1,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k+1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k+1,j, ar, nx, ny, nz, maskptr,l,k,j)) + \
                      b * (vox_val(l,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k+1,j, ar, nx, ny, nz, maskptr,l,k,j)) + \
	              c * uval);
            else {
	      /* multiply by 'a' four voxel values in current slice and in 
                 centers of slices before and after current slice */
  	       Fval = a * (vox_val(l,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k+1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k,j+1, ar, nx, ny, nz, maskptr,l,k,j));
               /* 'b' * corners of kernel stencil in current slice
                     and centers of edges on previous and following slices */
               Fval += b * (vox_val(l-1,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k-1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k+1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k+1,j, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k-1,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k+1,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k-1,j+1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k,j+1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k,j+1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l,k+1,j+1, ar, nx, ny, nz, maskptr,l,k,j));
               /* 'c' * corners of kernel stencil on previous and following slices */
               Fval += c * (vox_val(l-1,k-1,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k-1,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k+1,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k+1,j-1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k-1,j+1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k-1,j+1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l-1,k+1,j+1, ar, nx, ny, nz, maskptr,l,k,j) + \
                           vox_val(l+1,k+1,j+1, ar, nx, ny, nz, maskptr,l,k,j));
               /* 'd' * voxel value at current point only */
               Fval += d * uval;
               /* scale by Dmean */
               Fval *= Dmean;
            }

            *Gptr = uval + DeltaT  * (Fval + *Gvalptr);
            }
            Gptr++; Gvalptr++;
	  }
	}
      }
   }

   EXRETURN;
}
 
#endif 
 
/*! set mask edge voxels to 2, all others to 1 */
static void
Fix_mask(byte *maskptr, THD_3dim_dataset *dset, int flag2D3D)  
{
   int ii, jj, kk, nx, ny, nz, nxy;
   byte *mptr;
   THD_dataxes   * daxes ;
   
   mptr = maskptr;
   /** load the grid parameters **/
   daxes = dset->daxes ;
   nx    = daxes->nxx ;
   ny    = daxes->nyy ;
   nz    = daxes->nzz ;
 
   nxy = nx * ny;
   
   if(flag2D3D==2) {
       for(kk=0;kk<nz;kk++) {
         for(jj=0;jj<ny;jj++) {
	     for(ii=0;ii<nx;ii++) {  
	        if(*mptr) { /* if mask value anything other than 0 */
		    if((jj>0) && (jj<(ny-1)) && (ii>0) && (ii<nx-1)) {
           /* check all neighboring voxels to see if all surrounding voxels are also included */

        	       if(Check_Neighbors_2D(mptr,nx))
		         *mptr = 1;  /* replace with value of 1 if all neigbors are 1 also*/	
        	       else
        	         *mptr = 2;  /* set edge of mask to 2 */
		    }
		    else
		       *mptr = 2; /* on edge of volume */
		}
		mptr++;
 	   }
	 }
      }
   }
   else {           /* 3D version */
      for(kk=0;kk<nz;kk++) {
         for(jj=0;jj<ny;jj++) {
	     for(ii=0;ii<nx;ii++) {  
                 if(*mptr) { /* if mask value anything other than 0 */
		    if((kk>0)&&(kk<(nz-1)) && (jj>0) && (jj<(ny-1)) && (ii>0) && (ii<nx-1)) {
		       if(Check_Neighbors_3D(mptr, nx, nxy))
		           *mptr = 1;
		       else
		           *mptr = 2;
		    }
		    else
		       *mptr = 2;  /* on edge of volume */
		}
		mptr++;
            }
	 }
      } 
   }
}


/*! for 2D check 3x3 neighborhood to see if any voxels in neighborhod are not in mask */
/* if any neighbors are not in mask, return 0 */
static int Check_Neighbors_2D(byte *mptr, int nx)
{
   byte *bptr;
   int flag;
   
   /* check 1st row of 3 voxels */
   bptr = mptr - nx + 1;
   flag =  (*bptr) && (*(bptr+1)) && (*(bptr+2));
   if(flag==0) {
       return(0);
   }
   /* check central row*/
   flag =  (*mptr-1) && (*mptr) && (*(mptr+1));
   if(flag==0) {
       return(0);
   }
   /* check last row of 3 voxels */
   bptr = mptr+nx-1;
   flag =  (*bptr) && (*(bptr+1)) && (*(bptr+2));
   return(flag);
} 

/*! for 3D check 3x3x3 neighborhood to see if any voxels in neighborhod are not in mask */
/* if any neighbors are not in mask, return 0 */
static int Check_Neighbors_3D(byte *mptr, int nx, int nxy)
{
   byte *bptr;
   int flag;
  
   bptr = mptr - nxy;
   flag = Check_Neighbors_2D(bptr, nx);
   if(flag==0) return(0);
   flag = Check_Neighbors_2D(mptr, nx);
   if(flag==0) return(0);
   bptr = mptr + nxy;
   flag = Check_Neighbors_2D(mptr, nx);
   return(flag);
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
   in_ar = (float *) mri_data_pointer(data_im); /* in_ar is pointer to data */
   data_im = DSET_BRICK(outdset, brickn); 
   out_ar = (float *) mri_data_pointer(data_im); /* out_ar is pointer to output data */
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
     in_ar = (float *) mri_data_pointer(data_im); /* in_ar is pointer to data */
     for(j=0;j<nxyz;j++) {
	*in_ar = uval;
        in_ar++;
     } 
   }	 
}

/*! get voxel value at x,y,z from image but limit by dimensions and mask */
static INLINE
float vox_val(int x,int y,int z,float *imptr, int nx, int ny, int nz, byte *maskptr, int i, int j, int k)
{
   float voxval;
   int offset;
   /* get voxel values within limits of 0 to nx-1 and 0 to ny-1*/
   /* if value is not inside mask use value at i, j, k instead */


#define max(a,b) ((a) > (b) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))

   x = min(x, (nx-1));
   x = max(x,0);

   y = min(y, (ny-1));
   y = max(y, 0);

   z = min(z, (nz-1));
   z = max(z, 0);

   offset = nx*(z*ny+y) + x;
   /* put mask check here too */ 
   if((maskptr!=NULL) && !(*(maskptr+offset))) /* if not in mask use i,j,k offset*/
     offset = nx*(k*ny+j) + i;
   voxval = *(imptr+offset);

   /*define VOX_VAL(x,y,offset,nx, ny) \
     (*((offset) + min(max((y),0),(ny-1))*(nx) + min(max((x),0),(nx-1))))*/
   
   return(voxval);
}

