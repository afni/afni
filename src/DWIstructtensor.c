/******************* DWIstructtensor.c  *************************************/
/* Author: Daniel Glen, 13 Jun 2005                                         */
/* compute tensor of structure of DWI volumes for the purpose of anisotropic*/
/* smoothing of the image.                                                  */
/* called as function to generate D tensor, which can be a 2D or 3D tensor  */
/* The D tensor is then used as basis for smoothing along directionality of */
/* DWI image.                                                               */
/* This tensor is not the same as the traditional diffusion tensor of DTI   */
/* imaging, but it is a diffusion tensor referring to the diffusion within  */
/* the image, the movement of stuff within the image.                       */
/* Only one D tensor volume is generated for all of the DWI volumes         */
/* The 2D form has three elements in tensor form for each voxel or a 2x2    */
/* matrix in matrix form.                                                   */
/* The 3D form has 6 elements in tensor form for each voxel or 3x3 in matrix*/
/* form.                                                                    */
/* This program is appropriate as a function within another main program    */
/* to generate the tensor. The calling function could then use the tensor to*/
/* smooth the image */


#include "thd_shear3d.h"
#include "matrix.h"
#include "afni.h"

#define TINYNUMBER 1E-10
#define SMALLNUMBER 1E-4

static char D_prefix[THD_MAX_PREFIX] = "TempDAni";


THD_3dim_dataset *DWIstructtensor(THD_3dim_dataset * DWI_dset, int flag2D3D, byte *maskptr, int smooth_flag, int save_tempdsets_flag);
void Smooth_DWI_dset(THD_3dim_dataset * DWI_dset, int flag2D3D);
void Smooth_Gradient_Matrix(MRI_IMARR *Gradient_Im, int flag2D3D);
MRI_IMARR *Compute_Gradient_Matrix(THD_3dim_dataset *DWI_dset, int flag2D3D,byte*maskptr,int prodflag);
MRI_IMARR *Compute_Gradient_Matrix_Im(MRI_IMAGE *SourceIm, int flag2D3D, byte *maskptr, int xflag, int yflag);
MRI_IMARR *Eig_Gradient(MRI_IMARR *Gradient_Im, int flag2D3D, byte *maskptr);
MRI_IMARR *Compute_Phi(MRI_IMARR *EV_Im, int flag2D3D, byte *maskptr);
MRI_IMARR *ComputeDTensor(MRI_IMARR *phi_Im, int flag2D3D, byte *maskptr);
THD_3dim_dataset *Copy_IMARR_to_dset(THD_3dim_dataset * base_dset,MRI_IMARR *Imptr, char *new_prefix);
static inline float vox_val(int x,int y,int z,float *imptr, int nx, int ny, int nz, byte *maskptr, int i, int j, int k);
extern THD_3dim_dataset * Copy_dset_to_float(THD_3dim_dataset * dset , char * new_prefix );
void Compute_IMARR_Max(MRI_IMARR *Imptr);
float Find_Max_Im(MRI_IMAGE *im, byte *maskptr);
static void Save_imarr_to_dset(MRI_IMARR *Imarr_Im, THD_3dim_dataset *base_dset, char *dset_name);

extern int compute_method; /* determines which method to compute phi */

/*! compute image diffusion tensor, D, anisotropic smoothing of DWI */
THD_3dim_dataset *
DWIstructtensor(THD_3dim_dataset * DWI_dset, int flag2D3D, byte *maskptr, int smooth_flag, int save_tempdsets_flag)
{
  MRI_IMARR *Gradient_Im, *EV_Im, *phi_Im, *D_Im;
  THD_3dim_dataset *D_dset, *tempdset, *Gradient_dset;

  ENTRY("DWIstructtensor");

  tempdset = Copy_dset_to_float(DWI_dset, "tempani"); /* make another copy for smoothing */

  if(smooth_flag)
     Smooth_DWI_dset(tempdset,flag2D3D);     /* smooth DWI images a little with Gaussian
                                     smoothing */
  /* compute gradients of smoothed DWI images */
  /* and form matrix of gradients - imarr with 3 sub-briks for 2D */
  Gradient_Im = Compute_Gradient_Matrix(tempdset, flag2D3D, maskptr, 1);
  THD_delete_3dim_dataset(tempdset , False ) ;  /* delete tensor */
  if(save_tempdsets_flag)
     Save_imarr_to_dset(Gradient_Im,DWI_dset, "Gradient");

  /*Compute_IMARR_Max(Gradient_Im);*/

  /* smooth each component of gradient matrix more */
  if(smooth_flag)
     Smooth_Gradient_Matrix(Gradient_Im, flag2D3D);

  /* compute eigenvalues, eigenvectors of Smoothed gradient matrix  */
  /* imarr with 6 sub-briks for 2D (extended the Gradient_Im from 3 to 6) */
  EV_Im = Eig_Gradient(Gradient_Im, flag2D3D, maskptr);

  if(save_tempdsets_flag)
    Save_imarr_to_dset(EV_Im,DWI_dset, "Eigens");

  /*Compute_IMARR_Max(EV_Im);*/


   /* compute phi (kind of reciprocal of  eigenvalues) */
  /* replace first two eigenvalue sub-briks for phi_Im */
  phi_Im = Compute_Phi(EV_Im, flag2D3D, maskptr);
  if(save_tempdsets_flag)
     Save_imarr_to_dset(phi_Im,DWI_dset, "phi");
  /*printf("computed phi_Im\n");*/
  /*Compute_IMARR_Max(phi_Im);*/

  /* compute D, diffusion tensor of structure of DWI */
  /* replace first 3 sub-briks for 2D with Dxx, Dxy, Dyy */
  D_Im = ComputeDTensor(phi_Im, flag2D3D, maskptr);
  /* do not have to free any temporary image arrays */
  /*  DESTROY_IMARR(phi_Im);*/
  /* DESTROY_IMARR(EV_Im); */
  /* for 2D, keep first three sub-briks and remove remaining sub-briks */
   TRUNCATE_IMARR(D_Im,3);

   /*printf("computed D_Im\n");*/
   /*Compute_IMARR_Max(D_Im);*/
 
   D_dset = Copy_IMARR_to_dset(DWI_dset, D_Im, D_prefix);

   tross_Copy_History (DWI_dset, D_dset);
   EDIT_dset_items (D_dset, ADN_brick_label_one + 0, "Dxx", ADN_none);
   EDIT_dset_items (D_dset, ADN_brick_label_one + 1, "Dxy", ADN_none);
   EDIT_dset_items (D_dset, ADN_brick_label_one + 2, "Dyy", ADN_none);

   EDIT_dset_items( D_dset ,
              ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
		   ADN_prefix , "Dtensor",
                          ADN_label1 , "Dtensor" ,
                       ADN_none ) ;
   /* return D - diffusion tensor of image */
   RETURN(D_dset);
}

/*! save IMARR structure to temporary dataset and write to disk */
static void
Save_imarr_to_dset(MRI_IMARR *Imarr_Im, THD_3dim_dataset *base_dset, char *dset_name)
{
  THD_3dim_dataset *temp_dset;
  int nbriks,i;
  char tempstring[256];

  ENTRY("Save_imarr_dset");
   temp_dset = Copy_IMARR_to_dset(base_dset, Imarr_Im, dset_name);
   nbriks = temp_dset->dblk->nvals;
   tross_Copy_History (base_dset, temp_dset);
   for(i=0;i<nbriks;i++) {
      sprintf(tempstring,"%s_%d", dset_name, i);
      EDIT_dset_items(temp_dset,ADN_brick_label_one + i,tempstring,ADN_none);
   }

   EDIT_dset_items(temp_dset ,
              ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
	      ADN_prefix , dset_name,
              ADN_label1 , dset_name ,
                       ADN_none ) ;
   DSET_write (temp_dset);
       printf ("--- Output dataset %s\n", DSET_BRIKNAME(temp_dset));  
   temp_dset->dblk->brick = NULL;  /* don't delete MRI_IMARR structure */
   THD_delete_3dim_dataset( temp_dset, False ) ;  /* delete temporary dset */
  					          /* from memory (not disk) */
  
   EXRETURN;
}

/* apply small amount of smoothing to data */
void
Smooth_DWI_dset(THD_3dim_dataset *DWI_dset, int flag2D3D)
{
   float *ar;
   MRI_IMAGE *data_im = NULL;
   int nx, ny, nz, fim_type, i;
   THD_dataxes   * daxes ;

   ENTRY("Smooth_DWI_dset");

   /** load the grid parameters **/
   daxes = DWI_dset->daxes ;
   nx    = daxes->nxx ;
   ny    = daxes->nyy ;
   nz    = daxes->nzz ;

   fim_type = MRI_float ;   /* only works with floats here */

   if(flag2D3D == 2)        /* for 2D, don't smooth in Z direction */
     nz = 1;
   /* smooth DWI images a little with Gaussian smoothing */
   for(i=0;i<DWI_dset->dblk->nvals; i++) {  /* for each sub-brik in dataset */
      data_im = DSET_BRICK (DWI_dset, i);  /* set pointer to the ith sub-brik of the dataset */
      ar = mri_data_pointer(data_im) ;
      EDIT_blur_volume( nx,ny,nz, 1.0f,1.0f,1.0f, fim_type, ar, 0.5f ) ;
   }
   EXRETURN;
}

/* apply small amount of smoothing to data */
void
Smooth_Gradient_Matrix(MRI_IMARR *Gradient_Im, int flag2D3D)
{
   float *ar;
   MRI_IMAGE *data_im = NULL;
   int nx, ny, nz, fim_type, i;

   ENTRY("Smooth_Gradient_Matrix");

   /** load the grid parameters **/

   fim_type = MRI_float ;   /* only works with floats here */

   /* smooth DWI images a little with Gaussian smoothing */
   for(i=0;i<Gradient_Im->num; i++) {  /* for each sub-brik in dataset */
      data_im = Gradient_Im->imarr[i];  /* set pointer to the ith sub-brik of the dataset */
      ar = mri_data_pointer(data_im) ;
      nx  = data_im->nx; ny = data_im->ny; nz = data_im->nz;
      if(flag2D3D == 2)        /* for 2D, don't smooth in Z direction */
         nz = 1;
      EDIT_blur_volume( nx,ny,nz, 1.0f,1.0f,1.0f, fim_type, ar, 1.0f ) ;
   }
   EXRETURN;
}

/* compute numerical gradients for each voxel and compose matrix for smoothing
   including [(du/dx)^2 du/dx*du/dy (du/dy)^2] */
MRI_IMARR *
Compute_Gradient_Matrix(THD_3dim_dataset *DWI_dset, int flag2D3D, byte *maskptr, int prodflag)
{
  /* DWI_dset is input dataset */
  /* flag2D3D is flag for dimensionality of gradient */
  /* maskptr is pointer to mask array to mask data - null if no mask */
  /* prodflag is productflag whether to simply compute du/dx and du/dy or du/dx^2,
     du/dx*du/dy, du/dy^2 */
  /* gradient matrix is returned as MRI_IMARR (2 or 3 sub-briks for 2D case)*/

/* edge points and masked points are treated equivalently */
/*  with a test for the index of each node in the kernels and
    the central voxels themselves */

/* du/dx is calculated with 3x3 kernel for 2D as */
/*       -a 0 a   v0 0 v3 */
/*       -b 0 b   v1 0 v4 */
/*       -a 0 a   v2 0 v5*/
/* where a=3/16, b= 10/16 */

/* du/dy is calculated with 3x3 kernel for 2D as */
/*   c  d  c     v0 v1 v2 */
/*   0  0  0      0  0  0 */
/*  -c -d -c     v3 v4 v5 */
/* where c=3/16, d= 10/16 */

/* for 3d, instead of alternating rows and columns, */
/* use alternating planes in direction  (p+1) - (p-1) for du/dx */
/* a b a    a b a     r+1 */
/* b c b  - b c b     r   */
/* a b a    a b a     r-1 */
/* q-1 q q+1 */
/* where a = 0.02, b=0.06,c =0.18 */
/* two vertical planes before and after the current voxel for du/dx */
/* two horizontal planes above and below the current voxel for du/dy */
/* two slices before and after the current voxel for du/dz */

   MRI_IMARR *Gradient_Im;
   MRI_IMAGE *im, *data_im;
   byte *tempmaskptr;
   byte temp;

   float *ar,*gptr[6];
   static double a, b, c, d; 
   double dudx, dudy, dudz;
   float v0, v1, v2, v3, v4, v5, tempv;
   float vv[3][3][3];  /* voxel values for cubic stencil */
   int nx, ny, nz, i, j, k, l, ii, nbriks, nout,ll ,kk, jj;
   THD_dataxes   * daxes ;
   float dx = 1.0;   /* delta x - assume cubical voxels for now */

   ENTRY("Compute_Gradient_Matrix");
 
   tempmaskptr = maskptr;
   /* set up constants for kernel */
   if(flag2D3D==2) {
   a = 0.1875; /* (2.0 * dx); */  /*3/16;*/
   b = 0.625; /* (2.0 * dx);*/    /* 10/16;*/
   c = 0.1875;
   d = 0.625;

     if(prodflag)
       nout = 3;
     else
       nout = 2;
   }
   else {
      a = 0.02;
      b = 0.06;
      c = 0.18;
      if(prodflag)
         nout = 6;
      else
         nout = 3;
   }

   /** load the grid parameters **/
   daxes = DWI_dset->daxes ;
   nx    = daxes->nxx ;
   ny    = daxes->nyy ;
   nz    = daxes->nzz ;
   nbriks = DWI_dset->dblk->nvals;
   /* make new Image Array to hold gradients and then gradient products */
   INIT_IMARR(Gradient_Im);
   for(i=0;i<nout; i++) {  /* create 3 sub-briks for du/dx^2, du/dx*du/dy and du/dy^2 */
      im = mri_new_vol(nx, ny, nz, MRI_float);
      if(im==NULL) {
	fprintf(stderr,"+++can not create temporary data storage \n");
        RETURN(NULL);
      }
      ADDTO_IMARR(Gradient_Im, im);
   }

  
    for(ii=0;ii<nout;ii++) {
       im  = (Gradient_Im->imarr[ii]);
       gptr[ii] = (float *) mri_data_pointer(im);
       if(gptr[ii]==NULL) {
	 fprintf(stderr,"+++can not create temporary data storage pointers \n");
        RETURN(NULL);
       }
      }

       for(j=0;j<nz;j++) {      /* for each slice in sub-brik */
        for(k=0;k<ny;k++) {    /*   for each row */
	  for(l=0;l<nx;l++) {  /* for each column */
            *gptr[0] = 0.0;  /* initialize each summed gradient product component in the output briks */
            *gptr[1] = 0.0;
            if(prodflag)
              *gptr[2] = 0.0;
            
            if((maskptr!=NULL) && (!*tempmaskptr++)) {    /*  check if point is in mask or not */
	      for(ii=0;ii<nout;ii++)
	          gptr[ii]++;
            }
            else {
            for(i=0;i<nbriks; i++) {  /* for each sub-brik in dataset */ 
               data_im = DSET_BRICK (DWI_dset, i);  /* set pointer to the ith sub-brik of the dataset */
               ar = mri_data_pointer(data_im) ;

	       if(flag2D3D==2) {
 /* column before voxel*/
                            /*  voxel_value(col-1, row-1) */
	      v0 = vox_val(l-1,k-1,j,ar,nx,ny,nz,maskptr,l,k,j);
                            /*  voxel_value(col-1, row) */
	      v1 = vox_val(l-1,k,j,ar,nx,ny,nz,maskptr,l,k,j);
                            /*  voxel_value(col-1, row+1) */
	      v2 = vox_val(l-1,k+1,j,ar,nx,ny,nz,maskptr,l,k,j);

/* column after voxel */
	                    /*  voxel_value(col+1,row-1,l,k,j) */
	      v3 = vox_val(l+1,k-1,j,ar,nx,ny,nz,maskptr,l,k,j);
                            /*  voxel_value(col+1,row,l,k,j) */
	      v4 = vox_val(l+1,k,j,ar,nx,ny,nz,maskptr,l,k,j);

                            /*  voxel_value(col+1,row+1) */
	      v5 = vox_val(l+1,k+1,j,ar,nx,ny,nz,maskptr,l,k,j);
              dudx = a*(v3+v5-v0-v2) + b*(v4-v1);

 /* row before voxel*/
                            /*  voxel_value(col-1, row-1) */
      /*	    v0 = stays same */
                            /*  voxel_value(col-1, row) */
	      v1 = vox_val(l,k-1,j,ar,nx,ny,nz,maskptr,l,k,j);
                            /*  voxel_value(col-1, row+1) */
              tempv = v3;   /* swap v2,v3 for du/dy */
	      v3 = v2; /* previously found, use for lower left corner of kernel */
	      v2 = tempv;  /* vox_val(l+1,k-1,j,ar,nx,ny,nz,maskptr,l,k,j);*/

/* row after voxel */
	                    /*  voxel_value(col+1,row-1) defined above */
	      /* v3 = VOX_VAL(l+1,k-1,sliceoffsetptr,nx,ny);*/
                            /*  voxel_value(col+1,row) */
	      v4 = vox_val(l,k+1,j,ar,nx,ny,nz,maskptr,l,k,j);
                            /*  voxel_value(col+1,row+1) */
	      /* v5 stays same */
              dudy = c*(v3+v5-v0-v2) + d*(v4-v1);
        if(prodflag) {
         *(gptr[0]) += dudx * dudx; /* sum gradient product components in output image array */
         *(gptr[1])  += dudx * dudy;
         *(gptr[2]) += dudy * dudy;
       }
       else {
         *(gptr[0]) += dudx; /* sum gradient components in output image array */
         *(gptr[1]) += dudy;
         }
      } /* end of 2D gradient */
      else {   /* this is 3D */ 

        /* build 27 point stencil (0,0,0) (2,2,2) */
        /* don't actually need to get central point (1,1,1) */
        for(ll=0;ll<3;ll++) {
	  for(jj=0;jj<3;jj++) {
	    for(kk=0;kk<3;kk++) {
	      vv[ll][jj][kk] = vox_val(l-ll-1, k-kk-1, j-jj-1, ar, nx, ny, nz, maskptr, l, k, j);
            }
          }
        }

	/* du/dx  across alternating planes left and right of current voxel */
  /* corners of cube */
  /* centers of edges of cube */
        dudx = a * ( vv[2][0][0] + vv[2][0][2] + vv[2][2][0] + vv[2][2][2] -  \
                     vv[0][0][0] + vv[0][0][2] + vv[0][2][0] + vv[0][2][2]) + \
	  b * ( vv[2][0][1] + vv[2][1][0] + vv[2][1][2] + vv[2][2][1] -  \
                     vv[0][0][0] + vv[0][1][0] + vv[0][1][2] + vv[0][2][1]) + \
	  c * ( vv[2][1][1] - vv[0][1][1]);  /* centers of cube */

	/* du/dy  across alternating planes above and below current voxel */
        dudy = a * ( vv[0][2][0] + vv[2][2][0] + vv[0][2][2] + vv[2][2][2] -  \
                     vv[0][0][0] + vv[2][0][0] + vv[0][0][2] + vv[2][0][2]) + \
	  b * ( vv[1][2][0] + vv[0][2][1] + vv[2][2][1] + vv[1][2][2] -  \
                vv[1][0][0] + vv[0][0][1] + vv[2][0][1] + vv[1][0][2]) + \
	  c * ( vv[1][2][1] - vv[1][0][1]);  /* centers of square faces of cube */

	/* du/dz  across alternating slices before and after current voxel */
        dudz = a * ( vv[0][0][2] + vv[2][0][2] + vv[0][2][2] + vv[2][2][2] -  \
                     vv[0][0][0] + vv[2][0][0] + vv[0][2][0] + vv[2][2][0]) + \
	  b * ( vv[1][0][2] + vv[0][1][2] + vv[2][1][2] + vv[1][2][2] -  \
                vv[1][0][0] + vv[0][1][0] + vv[2][1][0] + vv[1][2][0]) + \
	  c * ( vv[1][1][2] - vv[1][1][0]);  /* centers of square faces of cube */

        if(prodflag) {
         *(gptr[0]) += dudx * dudx; /* sum gradient product components in output image array */
         *(gptr[1]) += dudx * dudy;
         *(gptr[2]) += dudx * dudz;
         *(gptr[3]) += dudy * dudy;
         *(gptr[4]) += dudy * dudz;
         *(gptr[5]) += dudz * dudz;
        }
        else {
         *(gptr[0]) += dudx; /* sum gradient components in output image array */
         *(gptr[1]) += dudy;
         *(gptr[2]) += dudz;
         }



#ifdef individualgradients
	/* left plane first (l-1) */
        /* line through z axis upper left */
	      vv[0] = vox_val(l-1,k-1,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[1] = vox_val(l-1,k-1,j,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[2] = vox_val(l-1,k-1,j+1,ar,nx,ny,nz,maskptr,l,k,j);
        /* line through z axis left */
              vv[3] = vox_val(l-1,k,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[4] = vox_val(l-1,k,j,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[5] = vox_val(l-1,k,j+1,ar,nx,ny,nz,maskptr,l,k,j);
        /* line through z axis lower left */
	      vv[6] = vox_val(l-1,k+1,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[7] = vox_val(l-1,k+1,j,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[8] = vox_val(l-1,k+1,j+1,ar,nx,ny,nz,maskptr,l,k,j);

	/* right plane now (l+1) */
        /* line through z axis upper right */
	      vv[9] = vox_val(l+1,k-1,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[10] = vox_val(l+1,k-1,j,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[11] = vox_val(l+1,k-1,j+1,ar,nx,ny,nz,maskptr,l,k,j);
        /* line through z axis right */
              vv[12] = vox_val(l+1,k,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[13] = vox_val(l+1,k,j,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[14] = vox_val(l+1,k,j+1,ar,nx,ny,nz,maskptr,l,k,j);
        /* line through z axis lower right */
	      vv[15] = vox_val(l+1,k+1,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[16] = vox_val(l+1,k+1,j,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[17] = vox_val(l+1,k+1,j+1,ar,nx,ny,nz,maskptr,l,k,j);

              vv[18] = vv[3];
              vv[19] = vv[12];
              
              dudx = a*(vv[9]+vv[11]+vv[15]+vv[17]-vv[0]-vv[2]-vv[6]-vv[8]) + \
		b*(vv[10]+vv[12]+vv[14]+vv[16]-vv[1]-vv[3]-vv[5]-vv[7]) + \
                c*(vv[13]-vv[4]);

	/* du/dy  across alternating planes above and below current voxel */
	/* upper plane first (k-1) */
        /* line through z axis upper left */
	      /* line already retrieved in dudx */
#if 0
	      vv[0] = vox_val(l-1,k-1,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[1] = vox_val(l-1,k-1,j,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[2] = vox_val(l-1,k-1,j+1,ar,nx,ny,nz,maskptr,l,k,j);
#endif
        /* line through z axis above  */
              vv[3] = vox_val(l,k-1,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[4] = vox_val(l,k-1,j,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[5] = vox_val(l,k-1,j+1,ar,nx,ny,nz,maskptr,l,k,j);
        /* this line out of order for temporary storage */
        /* line through z axis lower left */
	      vv[12] = vv[6];
	      vv[13] = vv[7];
	      vv[14] = vc[8];

        /* line through z axis upper right */
	      vv[6] = vv[9];
	      vv[7] = vv[10];
	      vv[8] = vv[11];

	/* lower plane now (l+1) */
        /* this line out of order to save time */
        /* line through z axis lower left */
	      vv[9] = vv[12];
	      vv[10] = vv[13];
	      vv[11] = vc[14];

        /* line through z axis below */
              vv[12] = vox_val(l,k+1,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[13] = vox_val(l,k+1,j,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[14] = vox_val(l,k+1,j+1,ar,nx,ny,nz,maskptr,l,k,j);

        /* line through z axis lower right */
	      /* line already retrieved in dudx */
#if 0
	      vv[15] = vox_val(l+1,k+1,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[16] = vox_val(l+1,k+1,j,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[17] = vox_val(l+1,k+1,j+1,ar,nx,ny,nz,maskptr,l,k,j);
#endif
              dudy = a*(vv[9]+vv[11]+vv[15]+vv[17]-vv[0]-vv[2]-vv[6]-vv[8]) + \
		b*(vv[10]+vv[12]+vv[14]+vv[16]-vv[1]-vv[3]-vv[5]-vv[7]) + \
                c*(vv[13]-vv[4]);


	/* du/dz  across alternating slices before and after current voxel */
	/* previous slice first (j-1) */
        /* upper row on previous slice */
	      /* vv[0] stays the same */
	      vv[1] = vv[3];
              vv[2] = vv[6];
#if 0
	      vv[0] = vox_val(l-1,k-1,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[1] = vox_val(l,k-1,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[2] = vox_val(l+1,k-1,j-1,ar,nx,ny,nz,maskptr,l,k,j);
#endif

        /* middle row on previous slice */
	      vv[3] = vv[18];
	      vv[4] = vox_val(l,k,j-1,ar,nx,ny,nz,maskptr,l,k,j);
              vv[5] = vv[19];
#if 0
              vv[3] = vox_val(l-1,k,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[5] = vox_val(l+1,k,j-1,ar,nx,ny,nz,maskptr,l,k,j);
#endif

        /* line through z axis upper right */
	      vv[6] = vv[9];
	      vv[7] = vv[10];
	      vv[8] = vv[11];

	/* lower plane now (l+1) */
        /* this line out of order to save time */
        /* line through z axis lower left */
	      vv[9] = vv[12];
	      vv[10] = vv[13];
	      vv[11] = vc[14];

        /* line through z axis below */
              vv[12] = vox_val(l,k+1,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[13] = vox_val(l,k+1,j,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[14] = vox_val(l,k+1,j+1,ar,nx,ny,nz,maskptr,l,k,j);

        /* line through z axis lower right */
	      /* line already retrieved in dudx */
#if 0
	      vv[15] = vox_val(l+1,k+1,j-1,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[16] = vox_val(l+1,k+1,j,ar,nx,ny,nz,maskptr,l,k,j);
	      vv[17] = vox_val(l+1,k+1,j+1,ar,nx,ny,nz,maskptr,l,k,j);
#endif
              dudy = a*(vv[9]+vv[11]+vv[15]+vv[17]-vv[0]-vv[2]-vv[6]-vv[8]) + \
		b*(vv[10]+vv[12]+vv[14]+vv[16]-vv[1]-vv[3]-vv[5]-vv[7]) + \
                c*(vv[13]-vv[4]);
#endif



      } /* end of 3D gradient */

       	 } /* sum over all sub-briks */





        for(ii=0;ii<nout;ii++) {
	  *gptr[ii] = *gptr[ii] / nbriks;/* normalize gradient for number of briks*/
	  gptr[ii]++;    /*and increment pointers*/
        }
      } /* not masked point */
     }
     }
   }
 
   RETURN(Gradient_Im);
}

/* compute numerical gradients for each voxel and compose matrix for smoothing
   including [du/dx du/dy] for single volume MRI_IMAGE */
MRI_IMARR *
Compute_Gradient_Matrix_Im(MRI_IMAGE *SourceIm, int flag2D3D, byte *maskptr, int xflag, int yflag)
{
  /* SourceIm is input volume */
  /* flag2D3D is flag for dimensionality of gradient */
  /* maskptr is pointer to mask array to mask data - null if no mask */
  /* xflag - compute and return dU/dx */
  /* yflag - compute and return dU/dy */
  /* gradient matrix is returned as MRI_IMARR (1 or 2 sub-briks for 2D case)*/

/* edge points and masked points are treated equivalently */
/*  with a test for the index of each node in the kernels and
    the central voxels themselves */

/* du/dx is calculated with 3x3 kernel for 2D as */
/*       -a 0 a   v0 0 v3 */
/*       -b 0 b   v1 0 v4 */
/*       -a 0 a   v2 0 v5*/
/* where a=3/16, b= 10/16 */

/* du/dy is calculated with 3x3 kernel for 2D as */
/*   c  d  c     v0 v1 v2 */
/*   0  0  0      0  0  0 */
/*  -c -d -c     v3 v4 v5 */
/* where c=3/16, d= 10/16 */

   MRI_IMARR *Gradient_Im;
   MRI_IMAGE *im;

   byte *tempmaskptr;
   float *ar,*gptr[3], *tempptr;
   double a, b, c, d; 
   double dudx, dudy; 
   float v0, v1, v2, v3, v4, v5, temp;
   int nx, ny, nz, i, j, k, l, ii, nout;
   float dx = 1.0;   /* delta x - assume cubical voxels for now */

   ENTRY("Compute_Gradient_Matrix_Im");

   tempmaskptr = maskptr;
   /* set up constants for kernel */
   a = 0.1875; /* / (2.0 * dx);*/   /*3/16;*/
   b = 0.625;  /* / (2.0 * dx);*/    /* 10/16;*/
   c = 0.1875;
   d = 0.625;

   nout = 0;
   if(xflag)
     nout++;
   if(yflag)
     nout++;

   if(nout==0) {
      fprintf(stderr,"Nothing to compute in Compute_Gradient_Matrix_Im\n");
      RETURN(NULL);
   }
   /** load the grid parameters **/
   nx    = SourceIm->nx ;
   ny    = SourceIm->ny;
   nz    = SourceIm->nz ;
 
   /* make new Image Array to hold gradients and then gradient products */
   INIT_IMARR(Gradient_Im);
   for(i=0;i<nout; i++) {  /* create sub-briks for output gradient */
      im = mri_new_vol(nx, ny, nz, MRI_float);
      if(im==NULL) {
	fprintf(stderr,"+++can not create temporary data storage \n");
        RETURN(NULL);
      }
      ADDTO_IMARR(Gradient_Im, im);
   }

  
    for(ii=0;ii<nout;ii++) {
       im  = (Gradient_Im->imarr[ii]);
       gptr[ii] = (float *) mri_data_pointer(im);
       if(gptr[ii]==NULL) {
	 fprintf(stderr,"+++can not create temporary data storage pointers \n");
        RETURN(NULL);
       }
     }

       ar = mri_data_pointer(SourceIm);
       for(j=0;j<nz;j++) {      /* for each slice in sub-brik */
        for(k=0;k<ny;k++) {    /*   for each row */
	  for(l=0;l<nx;l++) {  /* for each column */

            if((tempmaskptr!=NULL)&&(!*tempmaskptr++)) {    /*  check if point is in mask or not */
	      for(ii=0;ii<nout;ii++) {
		tempptr = gptr[ii];
                *tempptr = 0.0;
	        gptr[ii]++;
              }
            }
            else {
	      if (xflag) {
 /* column before voxel*/
                            /*  voxel_value(col-1, row-1) */
	      v0 = vox_val(l-1,k-1,j,ar,nx,ny,nz,maskptr,l,k,j);
                            /*  voxel_value(col-1, row) */
	      v1 = vox_val(l-1,k,j,ar,nx,ny,nz,maskptr,l,k,j);
                            /*  voxel_value(col-1, row+1) */
	      v2 = vox_val(l-1,k+1,j,ar,nx,ny,nz,maskptr,l,k,j);

/* column after voxel */
	                    /*  voxel_value(col+1,row-1) */
	      v3 = vox_val(l+1,k-1,j,ar,nx,ny,nz,maskptr,l,k,j);
                            /*  voxel_value(col+1,row) */
	      v4 = vox_val(l+1,k,j,ar,nx,ny,nz,maskptr,l,k,j);

                            /*  voxel_value(col+1,row+1) */
	      v5 = vox_val(l+1,k+1,j,ar,nx,ny,nz,maskptr,l,k,j);
              dudx = a*(v3+v5-v0-v2) + b*(v4-v1);
              } 


	      if (yflag) {
 /* row before voxel*/
		if(xflag) {  /* corners v0,v2,v3,v5 already found for du/dx*/
		  temp = v3;   /* swap v2, v3 */
	          v3 = v2; /* previously found, use for lower left corner of kernel */
                  v2 = temp;
                }
                else {
                            /*  voxel_value(col-1, row-1) */
 	          v0 = vox_val(l-1,k-1,j,ar,nx,ny,nz,maskptr,l,k,j);
                            /*  voxel_value(col-1, row+1) */
  	          v2 = vox_val(l+1,k-1,j,ar,nx,ny,nz,maskptr,l,k,j);
                            /*  voxel_value(col+1,row-1) defined above */
	          v3 = vox_val(l-1,k+1,j,ar,nx,ny,nz,maskptr,l,k,j);
                            /*  voxel_value(col+1,row+1) */
	          v5 = vox_val(l+1,k+1,j,ar,nx,ny,nz,maskptr,l,k,j);
                }
                            /*  voxel_value(col-1, row) */
	        v1 = vox_val(l,k-1,j,ar,nx,ny,nz,maskptr,l,k,j);
                            /*  voxel_value(col+1,row) */
	        v4 = vox_val(l,k+1,j,ar,nx,ny,nz,maskptr,l,k,j);


 	        dudy =  c*(v3+v5-v0-v2) + d*(v4 - v1);
              }

	      if(xflag)
      *(gptr[0]) = dudx; /* put gradient components in output image array */
	      if(yflag)
      *(gptr[nout-1]) = dudy;
	    
	      for(ii=0;ii<nout;ii++)
                 gptr[ii]++;
	    } /* not masked */

	}
     }
   }

 
   RETURN(Gradient_Im);
}

/*! get voxel value at x,y,z from image but limit by dimensions and mask */
static inline float vox_val(int x,int y,int z,float *imptr, int nx, int ny, int nz, byte *maskptr, int i, int j, int k)
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

   offset = z*nx*ny+(y*nx) + x;
   /* put mask check here too */ 
   if((maskptr!=NULL) && !(*(maskptr+offset))) /* if not in mask use i,j,k offset*/
     offset = k*nx*ny+(j*nx) + i;
   voxval = *(imptr+offset);

   /*define VOX_VAL(x,y,offset,nx, ny) \
     (*((offset) + min(max((y),0),(ny-1))*(nx) + min(max((x),0),(nx-1))))*/
   
   RETURN(voxval);
}


/* Compute eigenvectors and eigenvalues for Gradient matrix */
MRI_IMARR *Eig_Gradient(MRI_IMARR *Gradient_Im, int flag2D3D, byte *maskptr)
{
  MRI_IMARR *Eig_Im;
  MRI_IMAGE *im;
  byte *tempmaskptr;

   float *gptr[6];
   int ii, jj;
   register float a1, a2, a3, aa2;
   double a13, rad, L1, L2, x11, x12, x21, x22;
   int nx, ny, nz, nxyz, i;
   float maxim, tempmax0, tempmax2;
   double almostzero;
   int testii;

   ENTRY("Eig_Gradient");
   tempmaskptr = maskptr;
   im = Gradient_Im->imarr[0];
   nx = im->nx; ny = im->ny; nz = im->nz; nxyz = im->nxyz;

   /* will use Gradient_Im structure and data space in place already for
      eigenvalues and eigenvectors ) */

   /* for 2D, need 6 sub-briks in output mri_imarr-2 eigenvalues,4 vector components  */
   for(ii=3;ii<6; ii++) {  /* add 3 sub-briks to the current mri_imarr for each original sub-brik*/
      im = mri_new_vol(nx, ny, nz, MRI_float);
      if(im==NULL) {
	fprintf(stderr,"+++can not create temporary data storage \n");
        RETURN(NULL);
      }
      ADDTO_IMARR(Gradient_Im, im);
   }

    for(ii=0;ii<6;ii++) {
       im  = (Gradient_Im->imarr[ii]);
       gptr[ii] = (float *) mri_data_pointer(im);
     }
    /* find max Sxx, Syy in gradients */
   im  = (Gradient_Im->imarr[0]);
   tempmax0 = Find_Max_Im(im, maskptr);
   im  = (Gradient_Im->imarr[2]);
   tempmax2 = Find_Max_Im(im, maskptr);

   if(tempmax0>tempmax2)
      maxim = tempmax0;
   else
      maxim = tempmax2;
   almostzero = maxim / 100000.0;

   for(ii=0;ii<nxyz;ii++) {
      if(maskptr && (!(*tempmaskptr++))) {

       for(jj=0;jj<6;jj++) {
           *gptr[jj] = 0.0;
        }
      }
      else
       {

      /* for 2D case, solve by "hand" */

      a1 = *gptr[0];
      a2 = *gptr[1];
      aa2 = abs(a2);
      a3 = *gptr[2];
        a13 = a1 + a3;
        rad = sqrt(4.0*a2*a2 +((a1-a3)*(a1-a3)));
        L1 = (a13 + rad) / 2.0; 
        L2 = (a13 - rad) / 2.0;
        if(aa2<=almostzero) {
	  if(a1<=a3) {
           x11 = 0.0;
           x12 = 1.0;
           x21 = 1.0;
           x22 = 0.0;
	  }
          else {
           x11 = 1.0;
           x12 = 0.0;
           x21 = 0.0;
           x22 = 1.0;
	  }
         }
        else {
           rad = (L1-a1)/a2;
           x11 = sqrt(1/(1+rad*rad));
           x12 = x11 * rad;
           rad = (L2-a1)/a2;
           x21 = sqrt(1/(1+rad*rad));
           x22 = x21 * rad;
        }
  /* overwriting gradient values in 3 sub-briks here */
        a1 = abs(L1);
        a2 = abs(L2);
        if(a1>=a2) {
           *gptr[0] = L1;
           *gptr[1] = L2;
           *gptr[2] = x11;
           *gptr[3] = x12;
           *gptr[4] = x21;
           *gptr[5] = x22;
	}
        else {
           *gptr[0] = L2;
           *gptr[1] = L1;
           *gptr[2] = x21;
           *gptr[3] = x22;
           *gptr[4] = x11;
           *gptr[5] = x12;
        }
      } /* not masked */
      for(jj=0;jj<6;jj++) 
	gptr[jj]++;   /* increment pointers for next voxel */
        

    }

   Eig_Im = Gradient_Im;
   RETURN(Eig_Im);
}

float Find_Max_Im(MRI_IMAGE *im, byte *maskptr)
{
   int i, nxyz;
   float *gptr;
   float t1, max_im;
   byte *tempmaskptr;

   ENTRY("Find_Max_Im");

   tempmaskptr = maskptr;
   max_im = 0.0;
   nxyz = im->nxyz;
   gptr = (float *) mri_data_pointer(im);
   for(i=0;i<nxyz;i++) {
     if(maskptr && (!*tempmaskptr++))
        gptr++;
     else {
        t1 = *gptr;
        gptr++;
        if(t1>max_im)
           max_im = t1;
     }

   }
   RETURN(max_im);
}


/* compute inverted eigenvalue matrix */
MRI_IMARR *Compute_Phi(MRI_IMARR *EV_Im, int flag2D3D, byte *maskptr)
  {
    MRI_IMARR *phi_Im;
    MRI_IMAGE *im;
    double c1 = 0.01, c2 = -1.00, mc1 = 0.99 ;
    double e1, e2, e12, a, b, emax, e1me2;
    float *gptr[2];
    int nxyz, ii;
    byte *tempmaskptr;

    ENTRY("Compute_Phi");


    im = EV_Im->imarr[0];
    nxyz = im->nxyz;
    tempmaskptr = maskptr;
    
    emax = Find_Max_Im(EV_Im->imarr[0], maskptr);
    a = emax / 100.0;

    /* replace first two eigenvalue sub-briks with phi values */
    for(ii=0;ii<2;ii++) {
       im  = (EV_Im->imarr[ii]);
       gptr[ii] = (float *) mri_data_pointer(im);
     }


   /* Ding method phi1,2 = c/e1, c/e2 */
   if(compute_method==0) {
   emax = Find_Max_Im(EV_Im->imarr[0], maskptr);
   a = emax / 100.0;
   b = 1.0 / a;
   for(ii=0;ii<nxyz;ii++) {
     if(maskptr && !*tempmaskptr++) {
       *gptr[0] = 0.0;
       *gptr[1] = 0.0; 
     }
     else {

      e1 = *gptr[0];
      e2 = *gptr[1];

      if(e1<=0.0) {    /* e1 equal or close to zero */
        *gptr[0] = 0.5;
        *gptr[1] = 0.5;
      }
      else {
	if(e2<=0.0) {  /* e2 equal or close to zero */
          *gptr[0] = 0.0;
          *gptr[1] = 1.0;
        }
        else {      /* normal case */
          e1 = 1.0/e1;
          e2 = 1.0/e2;
          e12 = e1 + e2;
          *gptr[0] = e1 / e12; 
          *gptr[1] = e2 / e12; 
        }
      }
     } /* included in mask */
     gptr[0]++; gptr[1]++;
   }

  }
  else {                     /* use exponential method instead */
   for(ii=0;ii<nxyz;ii++) {
     if(maskptr && !*tempmaskptr++) {
       *gptr[0] = 0.0;
       *gptr[1] = 0.0; 
     }
     else {

      e1 = *gptr[0];
      e2 = *gptr[1];

      if(e1<=0.0) {    /* e1 equal or close to zero */
        *gptr[0] = c1;
        *gptr[1] = c1;
      }
      else {
	if(e2<=0.0) {  /* e2 equal or close to zero */
          *gptr[0] = c1;
          *gptr[1] = c1;
        }
        else {      /* normal case */
           if(e1==e2)
	     *gptr[0] = c1;
           else {
             e12 = (e1-e2);
             e12 *= e12;
             *gptr[0] =  c1 + (mc1 * exp(c2 / e12) );
           }
           *gptr[1] = c1;
	}
      }
     }   
     gptr[0]++; gptr[1]++;
   }
  }



#if 0
   /* fractional anisotropy method phi1,2 = 1/(e1-e2), 0.01 */
   emax = 0.0;
   for(ii=0;ii<nxyz;ii++) {
      /* for 2D case, solve by "hand" */
      e1 = *gptr[0];
      e2 = *gptr[1];
      e1me2 = e1-e2;
      if(e1me2>=a)
        e1 = 1/e1me2;
      else
        e1 = -9999.0;
      *gptr[0] = e1;
      *gptr[1] = 0.01;
      gptr[0]++; gptr[1]++;
      if(e1>emax)
	emax = e1;
   }

   im  = (EV_Im->imarr[0]);
   gptr[0] = (float *) mri_data_pointer(im);

   for(ii=0;ii<nxyz;ii++) {
     e1 = *gptr[0];
     if(e1==-9999.0) {
	*gptr[0] = emax;
        }
     gptr[0]++;
   }
#endif

#if 0 
     if(e2!=0.0)
        e2 = 1/e2;
      else
        e2 = 1.0;
      if(e1==e2)
	e1 = e2 = 0.5;
      else {
         a = 1/(e1+e2);
         e1 *= a;
         e2 *= a;
      }
#endif


#if 0
      if(e1==e2)
	*gptr[0] = c1;
      else {
        e12 = (e1-e2);
        e12 *= e12;
        *gptr[0] =  c1 + (mc1 * exp(c2 / e12) );
      }
      *gptr[1] = c1;
      gptr[0]++; gptr[1]++;

#endif
    phi_Im = EV_Im;
    RETURN(phi_Im);
  }


/* Compute the D diffusion tensor for the dataset */
MRI_IMARR *ComputeDTensor(MRI_IMARR *phi_Im, int flag2D3D, byte *maskptr)
  {
    MRI_IMARR *DTensor;
    MRI_IMAGE *im;
    double v[4], a1, a2;
    int nxyz, ii, i;
    float *gptr[6], *tempptr;
    byte *tempmaskptr;

    ENTRY("ComputeDTensor");
    /* D = V Phi VT */
    im = phi_Im->imarr[0];
    nxyz = im->nxyz;
    tempmaskptr = maskptr;

    /* replace first three phi,eigenvector sub-briks with Dxx,Dxy,Dyy values */
    for(ii=0;ii<6;ii++) {
       im  = (phi_Im->imarr[ii]);
       gptr[ii] = (float *) mri_data_pointer(im);
     }

    for(ii=0;ii<nxyz;ii++) {
       if(maskptr && !(*tempmaskptr++)) {
          for(i=0;i<6;i++) {
	    tempptr = gptr[i];
            *tempptr = 0.0;
            gptr[i]++;
          }
       }
       else {
      /* for 2D case, solve by "hand" */
      a1 = *gptr[0];
      a2 = *gptr[1];
      v[0] = *gptr[2]; /* don't increment this one, use this one in-place */
      for(i=1;i<4;i++){
	v[i] = *gptr[i+2];
      }
      *gptr[0] = (v[0] * v[0] * a1)+ (v[2] * v[2] * a2);
      *gptr[1] = (v[0] * v[1] * a1)+ (v[2] * v[3] * a2);
      *gptr[2] = (v[1] * v[1] * a1)+ (v[3] * v[3] * a2);
      for(i=0;i<6;i++) {
        gptr[i]++;
       }
      }
    } 

    DTensor = phi_Im;

    RETURN(DTensor);
  }


/* copy IMARR (image array) to new_dset using base_dset as the model */
THD_3dim_dataset *
Copy_IMARR_to_dset(THD_3dim_dataset * base_dset,MRI_IMARR *Imptr, char *new_prefix)
{
   THD_3dim_dataset * new_dset;
   int i;

   ENTRY("Copy_IMARR_to_dset");

   /*-- make the empty copy --*/
   new_dset = EDIT_empty_copy( base_dset ) ;

   THD_init_datablock_labels( new_dset->dblk ) ;
   EDIT_dset_items( new_dset ,
          ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
                       ADN_prefix , new_prefix ,
                       ADN_label1 , new_prefix ,
                       ADN_nvals  , Imptr->num ,           /* # sub-bricks */
                       ADN_ntt    , 0 ,                    /* # time points */
                       ADN_datum_all , MRI_float ,         /* atomic datum */
                       ADN_none ) ;

   THD_init_datablock_keywords( new_dset->dblk ) ;
   THD_init_datablock_stataux( new_dset->dblk ) ;


   /*   new_dset->dblk->brick = Imptr;*/   /* update pointer to data */
   new_dset->dblk->nvals = Imptr->num; 

  for(i=0;i<Imptr->num; i++) {  /* for each sub-brik in dataset */
    /*      Imptr->imarr[i]->kind = MRI_float;*/
      EDIT_substitute_brick(new_dset,i, MRI_float, mri_data_pointer(Imptr->imarr[i]));
  }


   RETURN(new_dset);
}

/* compute maximum and print maximum of each sub-brik in MRI_IMARR data */
/* for debugging */
void Compute_IMARR_Max(MRI_IMARR *Imptr)
{
  int i,j,nxyz;
  float tmax,tt;
  float *gptr;
  MRI_IMAGE *im;

  for(i=0;i<Imptr->num;i++) {  /* for each sub-brik */
    tmax = TINYNUMBER;
    im  = Imptr->imarr[i];
    gptr = (float *) mri_data_pointer(im);
    nxyz = im->nxyz;
    for(j=0;j<nxyz;j++){       /* for each voxel of data */
      tt = *gptr;
      gptr++;
      if(tt>tmax)
	tmax = tt;
    }
    printf("max brik %d = %f\n", i, tmax);
  }
}
