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


#ifdef __GNUC__
/*  inline used to make macro-equivalent speed functions */
/* but only available for gcc */
   #define INLINE   inline
#else
   #define INLINE   /**/
#endif

#include "thd_shear3d.h"
#include "matrix.h"
#include "afni.h"

#define TINYNUMBER 1E-10
#define SMALLNUMBER 1E-4

static char D_prefix[THD_MAX_PREFIX] = "TempDAni";

float aniso_sigma1 = 0.5;
float aniso_sigma2 = 1.0;


THD_3dim_dataset *DWIstructtensor(THD_3dim_dataset * DWI_dset, int flag2D3D,
                                  byte *maskptr, int smooth_flag,
                                  int save_tempdsets_flag, float *cen);
void Smooth_DWI_dset(THD_3dim_dataset * DWI_dset, int flag2D3D);
void Smooth_Gradient_Matrix(MRI_IMARR *Gradient_Im, int flag2D3D);
MRI_IMARR *Compute_Gradient_Matrix(THD_3dim_dataset *DWI_dset, int flag2D3D,byte*maskptr,int prodflag, int
smooth_flag, float smooth_factor);
MRI_IMARR *Compute_Gradient_Matrix_Im(MRI_IMAGE *SourceIm, int flag2D3D, byte *maskptr,
                                      int xflag, int yflag, int zflag);
MRI_IMARR *Eig_Gradient(MRI_IMARR *Gradient_Im, int flag2D3D, byte *maskptr);
MRI_IMARR *Compute_Phi(MRI_IMARR *EV_Im, int flag2D3D, byte *maskptr);
MRI_IMARR *ComputeDTensor(MRI_IMARR *phi_Im, int flag2D3D, byte *maskptr);
THD_3dim_dataset *Copy_IMARR_to_dset(THD_3dim_dataset * base_dset,MRI_IMARR *Imptr, char *new_prefix);
static INLINE float vox_val(int x,int y,int z,float *imptr, int nx, int ny, int nz, byte *maskptr,
                            int i, int j, int k);
extern THD_3dim_dataset * Copy_dset_to_float(THD_3dim_dataset * dset , char * new_prefix );
void Compute_IMARR_Max(MRI_IMARR *Imptr);
float Find_Max_Im(MRI_IMAGE *im, byte *maskptr);
void Save_imarr_to_dset(MRI_IMARR *Imarr_Im, THD_3dim_dataset *base_dset,
                        char *dset_name);

extern int compute_method; /* determines which method to compute phi */

static int with_diffmeasures=0;
void set_with_diff_measures(int v) {  with_diffmeasures = v; }
int get_with_diff_measures(void) { return(with_diffmeasures); }
THD_3dim_dataset *Compute_DiffMeasures(MRI_IMARR *EV_Im, int flag2D3D,
                                       byte *maskptr,
                                       THD_3dim_dataset *grid_dset,
                                       float *cen);

/*! compute image diffusion tensor, D, anisotropic smoothing of DWI
   If (save_tempdsets_flag) then temp datasets are saved
   with a suffix of .DD where DD is the iteration number,
   which is save_tempdsets_flag-1
*/
THD_3dim_dataset *
DWIstructtensor(THD_3dim_dataset * DWI_dset, int flag2D3D, byte *maskptr,
                int smooth_flag, int save_tempdsets_flag, float *cen)
{
  MRI_IMARR *Gradient_Im, *EV_Im, *phi_Im, *D_Im;
  THD_3dim_dataset *D_dset;
  char obuff[128]={""};

  ENTRY("DWIstructtensor");

  /*if(smooth_flag)
     Smooth_DWI_dset(tempdset,flag2D3D);*/    /* smooth DWI images a little with Gaussian
                                     smoothing */
  /* compute gradients of smoothed DWI images */
  /* and form matrix of gradients - imarr with 3 sub-briks for 2D */
  Gradient_Im = Compute_Gradient_Matrix(DWI_dset, flag2D3D, maskptr,
  1,smooth_flag, aniso_sigma1);
/*  THD_delete_3dim_dataset(tempdset , False ) ;*/  /* delete temporary copy */
  if(save_tempdsets_flag) {
     snprintf(obuff,127,"Gradient.%02d", save_tempdsets_flag-1);
     Save_imarr_to_dset(Gradient_Im,DWI_dset, obuff);
  }
  /* smooth each component of gradient matrix more */
  if(smooth_flag)
     Smooth_Gradient_Matrix(Gradient_Im, flag2D3D);

  /* compute eigenvalues, eigenvectors of Smoothed gradient matrix  */
  /* imarr with 6 sub-briks for 2D (extended the Gradient_Im from 3 to 6) */
  EV_Im = Eig_Gradient(Gradient_Im, flag2D3D, maskptr);

  if(save_tempdsets_flag) {
     snprintf(obuff,127,"Eigens.%02d", save_tempdsets_flag-1);
     Save_imarr_to_dset(EV_Im, DWI_dset, obuff);
  }

  if (save_tempdsets_flag && with_diffmeasures) {
      THD_3dim_dataset *dout=NULL;
      if (!(dout = Compute_DiffMeasures(EV_Im, flag2D3D, maskptr,
                                        DWI_dset, cen))) {
         ERROR_message("Tragedy has struck. The turkey is still frozen");
      } else {
         snprintf(obuff,127,"Diff_measures.%02d", save_tempdsets_flag-1);
         EDIT_dset_items(dout,ADN_prefix, obuff, ADN_none);
         DSET_overwrite (dout);
         INFO_message("   Output dataset %s", DSET_BRIKNAME(dout));
         THD_delete_3dim_dataset( dout, False ) ;  /* delete temporary dset
                                                      including DiffMeas_Im */
      }
  }

  /*Compute_IMARR_Max(EV_Im);*/
   /* compute phi (kind of reciprocal of  eigenvalues) */
  /* replace first two eigenvalue sub-briks for phi_Im */
  phi_Im = Compute_Phi(EV_Im, flag2D3D, maskptr);
  if(save_tempdsets_flag) {
     snprintf(obuff,127,"phi.%02d", save_tempdsets_flag-1);
     Save_imarr_to_dset(phi_Im,DWI_dset, obuff);
  }
  /*printf("computed phi_Im\n");*/
  /*Compute_IMARR_Max(phi_Im);*/

  /* compute D, diffusion tensor of structure of DWI */
  /* replace first 3 sub-briks for 2D with Dxx, Dxy, Dyy */
  D_Im = ComputeDTensor(phi_Im, flag2D3D, maskptr);
  /* do not have to free any temporary image arrays */
  /*  DESTROY_IMARR(phi_Im);*/
  /* DESTROY_IMARR(EV_Im); */
  /* for 2D, keep first three sub-briks and remove remaining sub-briks */
  if(flag2D3D==2) {
     TRUNCATE_IMARR(D_Im,3);
     D_dset = Copy_IMARR_to_dset(DWI_dset, D_Im, D_prefix);
     tross_Copy_History (DWI_dset, D_dset);
     EDIT_dset_items (D_dset, ADN_brick_label_one + 0, "Dxx", ADN_none);
     EDIT_dset_items (D_dset, ADN_brick_label_one + 1, "Dxy", ADN_none);
     EDIT_dset_items (D_dset, ADN_brick_label_one + 2, "Dyy", ADN_none);
  }
  else {
     TRUNCATE_IMARR(D_Im,6);
     D_dset = Copy_IMARR_to_dset(DWI_dset, D_Im, D_prefix);
     tross_Copy_History (DWI_dset, D_dset);
     EDIT_dset_items (D_dset, ADN_brick_label_one + 0, "Dxx", ADN_none);
     EDIT_dset_items (D_dset, ADN_brick_label_one + 1, "Dxy", ADN_none);
     EDIT_dset_items (D_dset, ADN_brick_label_one + 2, "Dxz", ADN_none);
     EDIT_dset_items (D_dset, ADN_brick_label_one + 3, "Dyy", ADN_none);
     EDIT_dset_items (D_dset, ADN_brick_label_one + 4, "Dyz", ADN_none);
     EDIT_dset_items (D_dset, ADN_brick_label_one + 5, "Dzz", ADN_none);
  }

   EDIT_dset_items( D_dset ,
      ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
      ADN_prefix , "Dtensor",
      ADN_label1 , "Dtensor" ,
      ADN_none ) ;
   /* return D - diffusion tensor of image */
   RETURN(D_dset);
}

/*! save IMARR structure to temporary dataset and write to disk */
void
Save_imarr_to_dset(MRI_IMARR *Imarr_Im, THD_3dim_dataset *base_dset,
                   char *dset_name)
{
  THD_3dim_dataset *temp_dset;
  int nbriks,i;
  char tempstring[256];

  ENTRY("Save_imarr_dset");
   temp_dset = Copy_IMARR_to_dset(base_dset, Imarr_Im, dset_name);
   nbriks = temp_dset->dblk->nvals;
   tross_Copy_History (base_dset, temp_dset);
   if (!strncmp(dset_name, "Eigens.",7) && (nbriks == 12 || nbriks == 8) ) {
      if (nbriks == 12) { /* More informative labels, check if directions are
                             RAI or IJK...*/
         for(i=0;i<3;i++) {
            sprintf(tempstring,"L%d", i+1);
            EDIT_dset_items(temp_dset,
               ADN_brick_label_one+ i,tempstring,ADN_none);
         }
         for(i=3;i<12;i=i+3) {
            sprintf(tempstring,"V%d.x", i/3);
            EDIT_dset_items(temp_dset,
               ADN_brick_label_one+ i,tempstring,ADN_none);
            sprintf(tempstring,"V%d.y", i/3);
            EDIT_dset_items(temp_dset,
               ADN_brick_label_one+ i+1,tempstring,ADN_none);
            sprintf(tempstring,"V%d.z", i/3);
            EDIT_dset_items(temp_dset,
               ADN_brick_label_one+ i+2,tempstring,ADN_none);
         }
      } else if (nbriks == 8) {
         for(i=0;i<2;i++) {
            sprintf(tempstring,"L%d", i+1);
            EDIT_dset_items(temp_dset,
               ADN_brick_label_one+ i,tempstring,ADN_none);
         }
         for(i=2;i<8;i=i+2) {
            sprintf(tempstring,"V%d.x", i/2);
            EDIT_dset_items(temp_dset,
               ADN_brick_label_one+ i,tempstring,ADN_none);
            sprintf(tempstring,"V%d.y", i/2);
            EDIT_dset_items(temp_dset,
               ADN_brick_label_one+ i+1,tempstring,ADN_none);
         }
      } else { /* Should not be here! */
         for(i=0;i<nbriks;i++) {
            sprintf(tempstring,"%s_%d", dset_name, i);
            EDIT_dset_items(temp_dset,
               ADN_brick_label_one + i,tempstring,ADN_none);
         }
      }
   } else { /* default */
      for(i=0;i<nbriks;i++) {
         sprintf(tempstring,"%s_%d", dset_name, i);
         EDIT_dset_items(temp_dset,ADN_brick_label_one + i,tempstring,ADN_none);
      }
   }

   EDIT_dset_items(temp_dset ,
              ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
	      ADN_prefix , dset_name,
              ADN_label1 , dset_name ,
                       ADN_none ) ;
   DSET_overwrite (temp_dset);
       INFO_message("   Output dataset %s", DSET_BRIKNAME(temp_dset));
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
   float dz;

   ENTRY("Smooth_DWI_dset");

   /** load the grid parameters **/
   daxes = DWI_dset->daxes ;
   nx    = daxes->nxx ;
   ny    = daxes->nyy ;
   nz    = daxes->nzz ;

   fim_type = MRI_float ;   /* only works with floats here */

   if(flag2D3D == 2)        /* for 2D, don't smooth in Z direction */
     dz = 0.0f;
   else
     dz = 1.0f;
   /* smooth DWI images a little with Gaussian smoothing */
   for(i=0;i<DWI_dset->dblk->nvals; i++) {  /* for each sub-brik in dataset */
      data_im = DSET_BRICK (DWI_dset, i);  /* set pointer to the ith sub-brik of the dataset */
      ar = (float *) mri_data_pointer(data_im) ;
      EDIT_blur_volume( nx,ny,nz, 1.0f,1.0f,dz, fim_type, ar, 0.5f ) ;
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
   float dz;

   ENTRY("Smooth_Gradient_Matrix");

   /** load the grid parameters **/

   fim_type = MRI_float ;   /* only works with floats here */

   /* smooth DWI images a little with Gaussian smoothing */
   for(i=0;i<Gradient_Im->num; i++) {  /* for each sub-brik in dataset */
      data_im = Gradient_Im->imarr[i];  /* set pointer to the ith sub-brik of the dataset */
      ar = (float *) mri_data_pointer(data_im) ;
      nx  = data_im->nx; ny = data_im->ny; nz = data_im->nz;
      if(flag2D3D == 2)        /* for 2D, don't smooth in Z direction */
         dz = 0.0f;
      else
         dz = 1.0f;

      EDIT_blur_volume( nx,ny,nz, 1.0f,1.0f,dz, fim_type, ar, aniso_sigma2 ) ;
   }
   EXRETURN;
}

/****************************************************************************************/
/* old unoptimized code */
/* compute numerical gradients for each voxel and compose matrix for smoothing
   including [(du/dx)^2 du/dx*du/dy (du/dy)^2] */
MRI_IMARR *
Compute_Gradient_Matrix_old(THD_3dim_dataset *DWI_dset, int flag2D3D, byte *maskptr, int prodflag)
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

   float *ar,*gptr[6];
   static double a, b, c, d;
   double dudx, dudy, dudz;
   float v0, v1, v2, v3, v4, v5, tempv, temp;
   float vv[3][3][3];  /* voxel values for cubic stencil */
   int nx, ny, nz, i, j, k, l, ii, nbriks, nout,ll ,kk, jj;
   THD_dataxes   * daxes ;
   /*float dx = 1.0;*/   /* delta x - assume cubical voxels for now */

   ENTRY("Compute_Gradient_Matrix_old");

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
	ERROR_message("can not create temporary data storage");
        RETURN(NULL);
      }
      ADDTO_IMARR(Gradient_Im, im);
   }


    for(ii=0;ii<nout;ii++) {
       im  = (Gradient_Im->imarr[ii]);
       gptr[ii] = (float *) mri_data_pointer(im);
       if(gptr[ii]==NULL) {
	ERROR_message("can not create temporary data storage pointers");
        RETURN(NULL);
       }
      }

       for(j=0;j<nz;j++) {      /* for each slice in sub-brik */
        for(k=0;k<ny;k++) {    /*   for each row */
	  for(l=0;l<nx;l++) {  /* for each column */
            for(ii=0;ii<nout;ii++)
               *gptr[ii] = 0.0;  /* initialize each summed gradient product component in the output briks */

            if((maskptr!=NULL) && (!*tempmaskptr++)) {    /*  check if point is in mask or not */
	      for(ii=0;ii<nout;ii++)
	          gptr[ii]++;
            }
            else {
            for(i=0;i<nbriks; i++) {  /* for each sub-brik in dataset */
               data_im = DSET_BRICK (DWI_dset, i);  /* set pointer to the ith sub-brik of the dataset */
               ar = (float *) mri_data_pointer(data_im) ;

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
	  for(kk=0;kk<3;kk++) {
	    for(jj=0;jj<3;jj++) {
	      vv[ll][kk][jj] = vox_val(l-1+ll, k-1+kk, j-1+jj, ar, nx, ny, nz, maskptr, l, k, j);
            }
          }
        }

	/* du/dx  across alternating planes left and right of current voxel */
  /* corners of cube */
  /* centers of edges of cube */
        dudx = a * ( vv[2][0][0] + vv[2][0][2] + vv[2][2][0] + vv[2][2][2] -  \
                     vv[0][0][0] - vv[0][0][2] - vv[0][2][0] - vv[0][2][2]) + \
	  b * ( vv[2][0][1] + vv[2][1][0] + vv[2][1][2] + vv[2][2][1] -  \
                     vv[0][0][0] - vv[0][1][0] - vv[0][1][2] - vv[0][2][1]) + \
	  c * ( vv[2][1][1] - vv[0][1][1]);  /* centers of cube */

	/* du/dy  across alternating planes above and below current voxel */
        dudy = a * ( vv[0][2][0] + vv[2][2][0] + vv[0][2][2] + vv[2][2][2] -  \
                     vv[0][0][0] - vv[2][0][0] - vv[0][0][2] - vv[2][0][2]) + \
	  b * ( vv[1][2][0] + vv[0][2][1] + vv[2][2][1] + vv[1][2][2] -  \
                vv[1][0][0] - vv[0][0][1] - vv[2][0][1] - vv[1][0][2]) + \
	  c * ( vv[1][2][1] - vv[1][0][1]);  /* centers of square faces of cube */

	/* du/dz  across alternating slices before and after current voxel */
        dudz = a * ( vv[0][0][2] + vv[2][0][2] + vv[0][2][2] + vv[2][2][2] -  \
                     vv[0][0][0] - vv[2][0][0] - vv[0][2][0] - vv[2][2][0]) + \
	  b * ( vv[1][0][2] + vv[0][1][2] + vv[2][1][2] + vv[1][2][2] -  \
                vv[1][0][0] - vv[0][1][0] - vv[2][1][0] - vv[1][2][0]) + \
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

      } /* end of 3D gradient */

      } /* sum over all sub-briks */

        for(ii=0;ii<nout;ii++) {
	  *gptr[ii] = *gptr[ii] / nbriks;/* normalize gradient for number of briks*/
	  temp = fabs(*gptr[ii]);
          if(temp<TINYNUMBER)
	     *gptr[ii] = 0.0;
	  gptr[ii]++;    /*and increment pointers*/
        }
      } /* not masked point */
     }
    }
   }

   RETURN(Gradient_Im);
}


/* new optimized code trial*/
/**********************************************************************************************************/
/* compute numerical gradients for each voxel and compose matrix for smoothing
   including [(du/dx)^2 du/dx*du/dy (du/dy)^2] */
MRI_IMARR *
Compute_Gradient_Matrix(THD_3dim_dataset *DWI_dset, int flag2D3D, byte *maskptr, int prodflag, int smoothflag, float
smooth_factor)
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
/*   c  d  c     v0 v1 v3 */
/*   0  0  0      0  0  0 */
/*  -c -d -c     v2 v4 v5 */
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

   float *ar,*gptr[6];
   static double a, b, c, d;
   double dudx, dudy, dudz, dv0, dv1=0.0, dv2=0.0;
   float v0=0.0, v1=0.0, v2=0.0, v3, v4=0.0, v5=0.0, v6=0.0, v7=0.0, v8=0.0, temp;
   int nx, ny, nz, nxy, nxyz, i, ii, nbriks, nout;
   int vx, vy,vz, vi, baseoffset, yp1xp1, yp1xm1, nxp1, nxm1, nym1, nzm1;
   float *blur_data = NULL;
   float *vptr, *vptr0, *vptr1, *vptr2, *vptr3, *vptr5, *vptr6, *vptr7, *vptr8;
   float dz;
   int maskflag;

   float v9, v10=0.0, v11=0.0, v12, v13=0.0, v14=0.0, v15, v16=0.0, v17=0.0, v18=0.0;
   float v19=0.0, v20=0.0, v21, v22=0.0, v23=0.0, v24=0.0, v25=0.0, v26=0.0;
   float dv0600, dv0701=0.0, dv0802=0.0, dv1509, dv1610=0.0, dv1711=0.0, dv2418, dv2519=0.0, dv2620=0.0;
   float sv1824, sv1925=0.0, sv2026=0.0, sv0006, sv0107=0.0, sv0208=0.0, dv2103, dv2204=0.0, dv2305;
   /*float dx = 1.0;*/   /* delta x - assume cubical voxels for now */

   ENTRY("Compute_Gradient_Matrix");

 /* test with old code here - remove */
 /*Gradient_Im = Compute_Gradient_Matrix_v1(DWI_dset, flag2D3D, maskptr, prodflag);*/
/* RETURN(Gradient_Im);*/
 /*****************************************/

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
   data_im = DSET_BRICK (DWI_dset, 0);
   nx = data_im->nx; ny = data_im->ny; nz = data_im->nz; nxyz = data_im->nxyz;
   nxy = nx * ny;
   nbriks = DWI_dset->dblk->nvals;
   /* precompute offsets for each stencil point relative to the center point */
   yp1xp1 = nxp1 = nx + 1;
   yp1xm1 = nxm1 = nx - 1;
   nym1 = ny - 1;
   nzm1 = nz - 1;
   maskflag = 0;

   /* make new Image Array to hold gradients and then gradient products */
   INIT_IMARR(Gradient_Im);
   for(i=0;i<nout; i++) {  /* create 3 sub-briks for du/dx^2, du/dx*du/dy and du/dy^2 */
      im = mri_new_vol(nx, ny, nz, MRI_float);
      if(im==NULL) {
	ERROR_message("can not create temporary data storage");
        RETURN(NULL);
      }
      ADDTO_IMARR(Gradient_Im, im);
   }



    if(smoothflag) {
       blur_data = malloc(nxyz*sizeof(float));
       if(blur_data==NULL) {
         ERROR_message("Error - could not allocate memory for gradient");
         exit(1);
       }
    }

    if(flag2D3D == 2)        /* for 2D, don't smooth in Z direction */
       dz = 0.0f;
    else
       dz = 1.0f;

   if(flag2D3D==2) {
    for(i=0;i<nbriks; i++) {  /* for each sub-brik in dataset */
       data_im = DSET_BRICK (DWI_dset, i);  /* set pointer to the ith sub-brik of the dataset */
       ar = (float *) mri_data_pointer(data_im) ;
       if(smoothflag) {
          memcpy(blur_data, ar, nxyz*sizeof(float));
          EDIT_blur_volume( nx,ny,nz, 1.0f,1.0f,dz, MRI_float, blur_data, smooth_factor ) ;
          ar = blur_data;
       }
       /* reset the output gradient pointers after each sub-brik */
       for(ii=0;ii<nout;ii++) {
         im  = (Gradient_Im->imarr[ii]);
         gptr[ii] =(float *) mri_data_pointer(im);
       }
       baseoffset = 0;
       vptr = vptr0 = vptr1 = ar;
       tempmaskptr = maskptr;

       for(vz=0;vz<nz;vz++) {
          for(vy=0;vy<ny;vy++) {
	     for(vx=0;vx<nx;vx++) {
               if(maskptr) {
	         maskflag = *tempmaskptr++;
		 if (!maskflag) {    /*  check if point is in mask or not */
		    baseoffset++;
		    vptr0++;
		    vptr1++;
		    vptr++;
		    for(ii=0;ii<nout;ii++)
	        	gptr[ii]++;
		    continue;
                 }
		 /* edge of mask treat special if value in mask is 2 and not 1*/
               }

               if((maskflag==2) || (vx<1) || (vy<=1) || (vx==nxm1) || (vy==nym1)){   /* special cases at edges */
		  /* get voxels for 3x3 stencil */
		  vptr = ar + baseoffset;
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

                  dv0 = v6 - v0;
	          dv1 = v7 - v1;
	          dv2 = v8 - v2;
	      }
	      else {  /* x>=2, y>=2 */
		 v0 = v1;
		 v1 = v2;
		 v2 = *(++vptr0);

                 v3 = v4;
		 v4 = v5;
	         v5 = *(++vptr);

   /* row after voxel */
                 v6 = v7;
                 v7 = v8;
		 v8 = *(++vptr1);
                 dv0 = dv1;
	         dv1 = dv2;
 	         dv2 = v8 - v2;
             }

             dudy = a*(dv0 + dv2) + b* dv1;
             dudx = a*(v2+v8-v0-v6) + b*(v5-v3);

             if(prodflag) {
              *(gptr[0]) += dudx * dudx; /* sum gradient product components in output image array */
              *(gptr[1])  += dudx * dudy;
              *(gptr[2]) += dudy * dudy;
	      gptr[2]++;
	     }
	     else {
               *(gptr[0]) += dudx; /* sum gradient components in output image array */
               *(gptr[1]) += dudy;
               }
	     gptr[0]++;
	     gptr[1]++;
             baseoffset++;
	    } /* x loop */
	  } /* y loop*/
      } /* z loop */
   } /* sub-brick loop */
  }
  else {
/* 3D case  */
   /* get 9 row pointers this time and fill 27 values */
   /* this time each slice gets 3 row pointers, but we need z-1, z, z+1 slices
    v0-v8 are voxel values in z-1 slice, v9-v17 in slice z, v18-v26 in slice z+1*/
  /*
  v0  v1  v2    v9  v10 v11    v18 v19 v20
  v3  v4  v5    v12 v13 v14    v21 v22 v23
  v6  v7  v8    v15 v16 v17    v24 v25 v26
     z-1            z              z+1
     */

    for(i=0;i<nbriks; i++) {  /* for each sub-brik in dataset */
       data_im = DSET_BRICK (DWI_dset, i);  /* set pointer to the ith sub-brik of the dataset */
       ar = (float *) mri_data_pointer(data_im) ;
       if(smoothflag) {
          memcpy(blur_data, ar, nxyz*sizeof(float));
          EDIT_blur_volume( nx,ny,nz, 1.0f,1.0f,dz, MRI_float, blur_data, smooth_factor ) ;
          ar = blur_data;
       }
       /* reset the output gradient pointers after each sub-brik */
       for(ii=0;ii<nout;ii++) {
         im  = (Gradient_Im->imarr[ii]);
         gptr[ii] =(float *) mri_data_pointer(im);
       }
       baseoffset = 0;
       vptr = vptr0 = vptr1 = vptr2 = vptr3 = vptr5 = vptr6 = vptr7 = vptr8 = ar;
       tempmaskptr = maskptr;


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
		    for(ii=0;ii<nout;ii++)
	        	gptr[ii]++;
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

        	 /* initialize sliding differences for dudy */
        	 dv0600 = v6 - v0;
		 dv0701 = v7 - v1;
		 dv1509 = v15 - v9;
		 dv1610 = v16 - v10;
		 dv2418 = v24 - v18;
		 dv2519 = v25 - v19;

        	 /* initialize sliding sums for dudz */
        	 sv1824 = v18 + v24;
		 sv1925 = v19 + v25;
		 sv0006 = v0 + v6;
		 sv0107 = v1 + v7;
        	 dv2103 = v21 - v3;
		 dv2204 = v22 - v4;
                 dv2305 = v23 - v5;  /* mod 08-27-2008 drg*/
              }

	      else {  /* x>=1, y>=2 */
	         /* z-1 slice */
		 v0 = v1;
		 v1 = v2;
		 v2 = *(++vptr0);

                 v3 = v4;
		 v4 = v5;
	         v5 = *(++vptr1);

                 v6 = v7;
                 v7 = v8;
		 v8 = *(++vptr2);
                 /*z slice */
		 v9 = v10;
		 v10 = v11;
		 v11 = *(++vptr3);

                 v12 = v13;
		 v13 = v14;
	         v14 = *(++vptr);

                 v15 = v16;
                 v16 = v17;
		 v17 = *(++vptr5);
		 /* z+1 slice */
		 v18 = v19;
		 v19 = v20;
		 v20 = *(++vptr6);

                 v21 = v22;
		 v22 = v23;
	         v23 = *(++vptr7);

                 v24 = v25;
                 v25 = v26;
		 v26 = *(++vptr8);

                 /* slide differences for dudy */
                 dv0600 = dv0701;
		 dv0701 = dv0802;
		 dv1509 = dv1610;
		 dv1610 = dv1711;
		 dv2418 = dv2519;
		 dv2519 = dv2620;

                 /* slide sums for dudz */
                 sv1824 = sv1925;
		 sv1925 = sv2026;
		 sv0006 = sv0107;
		 sv0107 = sv0208;
                 dv2103 = dv2204;
		 dv2204 = dv2305;
             }

	     /* compute new sliding sums and differences  */
	     dv0802 = v8 - v2;
	     dv1711 = v17 - v11;
	     dv2620 = v26 - v20;

	     sv2026 = v20 + v26;
	     sv0208 = v2 + v8;
             dv2204 = v22 - v4;
             dv2305 = v23 - v5; /* mod -drg oops for 3D case, missed this one*/

  /*
  v0  v1  v2    v9  v10 v11    v18 v19 v20
  v3  v4  v5    v12 v13 v14    v21 v22 v23
  v6  v7  v8    v15 v16 v17    v24 v25 v26
     z-1            z              z+1
     */
	/* du/dx  across alternating planes left and right of current voxel */
  /* corners of planes */
  /* centers of edges of planes */
  /* centers of sides - adjacent in x-1, x+1 in same slice */

        /* dudx = a * (v2 + v20 + v8 + v26 - v0 - v18 - v6 -v24) + \
	       b * (v11 + v5 + v23 + v17 - v9 - v3 -v21 - v15) + \
	       c * (v14 - v12);*/
        dudx = a * (sv0208 + sv2026 - sv0006 - sv1824) + \
	       b * (v11 + v5 + v23 + v17 - v9 - v3 -v21 - v15) + \
	       c * (v14 - v12);

/*	dudy = a * (v6 + v8 + v24 + v26 - v0 - v2 - v18 - v20) + \
	       b * (v7 + v15 + v17 + v25 - v1 - v9 - v11 - v19) + \
	       c * (v16 - v10) ; */

	dudy = a * (dv0600 + dv0802 + dv2418 + dv2620) + \
	       b * (dv0701 + dv1509 + dv1711 + dv2519) + \
	       c * dv1610;

/*	dudz = a * (v18 + v20 + v24 + v26 - v0 - v2 - v6 - v8) + \
	       b * (v19 + v21 + v23 + v25 - v1 - v3 - v5 - v7) + \
	       c * (v22 - v4);*/
        dudz = a * (sv1824 + sv2026 - sv0006 - sv0208) + \
               b * (sv1925 + dv2103 + dv2305 - sv0107) + \
               c * dv2204;

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
	baseoffset++;
        for(ii=0;ii<nout;ii++)
           gptr[ii]++;    /*and increment pointers*/
       }   /* x */
      }  /* y */
     } /* z */
    } /* brick loop */
   } /* end 3D case */


   /* final normalization (mean) and check for being very close to zero */
   /* reset the output gradient pointers after each sub-brik */
   for(ii=0;ii<nout;ii++) {
     im  = (Gradient_Im->imarr[ii]);
     gptr[ii] =(float *) mri_data_pointer(im);
   }
   for(vi=0;vi<nxyz;vi++) {
     for(ii=0;ii<nout;ii++) {
        *gptr[ii] = *gptr[ii] / nbriks;/* normalize gradient for number of briks*/
         temp = fabs(*gptr[ii]);
         if(temp<TINYNUMBER)
           *gptr[ii] = 0.0;
         gptr[ii]++;    /*and increment pointers*/
     }
   }
   if(smoothflag)
      free(blur_data);

   RETURN(Gradient_Im);
}



/* compute numerical gradients for each voxel and compose matrix for smoothing
   including [du/dx du/dy] for single volume MRI_IMAGE */
MRI_IMARR *
Compute_Gradient_Matrix_Im(MRI_IMAGE *SourceIm, int flag2D3D, byte *maskptr, int xflag, int yflag, int zflag)
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
   float *ar,*gptr[3];
   double a, b, c, d;
   double dudx, dudy, dudz;

   float *vptr, *vptr0, *vptr1, *vptr2, *vptr3, *vptr5, *vptr6, *vptr7, *vptr8;
   float v0=0.0, v1=0.0, v2=0.0, v3=0.0, v4=0.0, v5=0.0,v6=0.0,v7=0.0,v8=0.0;
   float v9, v10=0.0, v11=0.0, v12, v13=0.0, v14=0.0, v15, v16=0.0, v17=0.0, v18=0.0;
   float v19=0.0, v20=0.0, v21, v22=0.0, v23=0.0, v24=0.0, v25=0.0, v26=0.0;
   float dv0600=0.0, dv0701=0.0, dv0802=0.0, dv1509=0.0, dv1610=0.0, dv1711=0.0, dv2418=0.0, dv2519=0.0, dv2620=0.0;
   float sv1824=0.0, sv1925=0.0, sv2026=0.0, sv0006=0.0, sv0107=0.0, sv0208=0.0, dv2103=0.0, dv2204=0.0, dv2305=0.0;

   float dv0,dv1=0.0,dv2=0.0, temp;
   int nx, ny, nz, i, ii, nout, noutm1, nxm1, nym1, nzm1;
   char maskflag;
   int yp1xp1, yp1xm1, nxy, nxyz, baseoffset;
   int vx,vy,vz;
   int ybrik=0;

   /* float dx = 1.0; */  /* delta x - assume cubical voxels for now */

   ENTRY("Compute_Gradient_Matrix_Im");

    /* set up constants for kernel */
   a = 0.1875; /* / (2.0 * dx);*/   /*3/16;*/
   b = 0.625;  /* / (2.0 * dx);*/    /* 10/16;*/
   c = 0.1875;
   d = 0.625;

   maskflag = 0;
   nout = 0;
   if(xflag)
     nout++;
   if(yflag)
     ybrik = nout++;
   if(zflag)
     nout++;
   if(nout==0) {
      ERROR_message("Nothing to compute in Compute_Gradient_Matrix_Im");
      RETURN(NULL);
   }

   noutm1 = nout - 1;
   /** load the grid parameters **/
   nx    = SourceIm->nx ;
   ny    = SourceIm->ny;
   nz    = SourceIm->nz ;
   nxyz = SourceIm->nxyz;
   nxy = nx * ny;
   yp1xp1 = nx + 1;
   yp1xm1 = nxm1 = nx - 1;
   nym1 = ny - 1;
   nzm1 = nz - 1;

   /* make new Image Array to hold gradients and then gradient products */
   INIT_IMARR(Gradient_Im);
   for(i=0;i<nout; i++) {  /* create sub-briks for output gradient */
      im = mri_new_vol(nx, ny, nz, MRI_float);
      if(im==NULL) {
	ERROR_message("can not create temporary data storage");
        RETURN(NULL);
      }
      ADDTO_IMARR(Gradient_Im, im);
   }


    for(ii=0;ii<nout;ii++) {
       im  = (Gradient_Im->imarr[ii]);
       gptr[ii] = (float *) mri_data_pointer(im);
     }

    ar = mri_data_pointer(SourceIm);
    baseoffset = 0;
    vptr = vptr0 = vptr1 = vptr2 = vptr3 = vptr5 = vptr6 = vptr7 = vptr8 = ar;
    tempmaskptr = maskptr;

#if 1
    if(flag2D3D==2) {   /* 2D option */
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
		      for(ii=0;ii<nout;ii++)
	        	  gptr[ii]++;
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
                    if(xflag) {
                       /* middle row of voxels */
	   	       v3 = vox_val(vx-1, vy, vz, ar, nx, ny, nz, maskptr, vx, vy, vz);
		       v5 = vox_val(vx+1, vy, vz, ar, nx, ny, nz, maskptr, vx, vy, vz);
                    }
                    if(vy==nym1) {
		       v6 = v7 = v8 = v4;/* all are orig voxel value, don't need vptr1*/
		    }
                    else {
		       v6 = vox_val(vx-1, vy+1, vz, ar, nx, ny, nz, maskptr, vx, vy, vz);
		       v7 = vox_val(vx, vy+1, vz, ar, nx, ny, nz, maskptr, vx,vy,vz);
		       v8 = vox_val(vx+1, vy+1, vz, ar, nx, ny, nz, maskptr, vx,vy,vz);
                       vptr1 = vptr + nx;  /* init pointer for third row */
                    }

                    if(yflag) {
                       dv0 = v6 - v0;
	               dv1 = v7 - v1;
	               dv2 = v8 - v2;
		    }
		}
        	else {
                   /* row before voxel */
		   v0 = v1;
		   v1 = v2;
		   v2 = *(++vptr0);
		   if(xflag) {
                   /* same row as voxel */
                      v3 = v4;
		      v4 = v5;
	              v5 = *(++vptr);
		   }
                   /* row after voxel */
                   v6 = v7;
                   v7 = v8;
		   v8 = *(++vptr1);
                }


      		if(xflag) {
        	   dudx = a*(v2+v8-v0-v6) + b*(v5-v3);
		   temp = fabs(dudx);
 	           if(temp>=TINYNUMBER)
        	      *(gptr[0]) = dudx; /* sum gradient components in output image array */
		   /*else
		      *gptr[0] = 0.0; */ /* already zeroed out when allocated */

        	}

        	if(yflag) {
        	   dv0 = dv1;
		   dv1 = dv2;
 		   dv2 = v8 - v2;
        	   dudy = a*(dv0 + dv2) + b* dv1;
		   temp = fabs(dudy);
 	           if(temp>=TINYNUMBER)
		      *(gptr[noutm1]) = dudy;
		   /*else
		      *(gptr[noutm1]) = 0.0;*/ /* already zeroed out when allocated */
        	}

        	for(ii=0;ii<nout;ii++) {
                     gptr[ii]++;
        	}
        	baseoffset++;
  	 }
       }
     }

   }  /* end if 2D */


  else {
/* 3D case  */
   /* get 9 row pointers this time and fill 27 values */
   /* this time each slice gets 3 row pointers, but we need z-1, z, z+1 slices
    v0-v8 are voxel values in z-1 slice, v9-v17 in slice z, v18-v26 in slice z+1*/
  /*
  v0  v1  v2    v9  v10 v11    v18 v19 v20
  v3  v4  v5    v12 v13 v14    v21 v22 v23
  v6  v7  v8    v15 v16 v17    v24 v25 v26
     z-1            z              z+1
     */
       for(vz=0;vz<nz;vz++) {
          for(vy=0;vy<ny;vy++) {
	     for(vx=0;vx<nx;vx++) {
               if(maskptr) {
	         maskflag = *tempmaskptr++;
		 if (!maskflag) {    /*  check if point is in mask or not */
		    baseoffset++;
		    vptr0++;
		    vptr1++;
		    vptr++;
		    vptr2++;
		    vptr3++;
		    vptr5++;
		    vptr6++;
		    vptr7++;
		    vptr8++;
		    for(ii=0;ii<nout;ii++)
	        	gptr[ii]++;
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


        	 /* initialize sliding differences for dudy */
		 if(yflag) {
        	    dv0600 = v6 - v0;
		    dv0701 = v7 - v1;
		    dv1509 = v15 - v9;
		    dv1610 = v16 - v10;
		    dv2418 = v24 - v18;
		    dv2519 = v25 - v19;
                 }

        	 /* initialize sliding sums for dudz */
		 if((xflag)||(zflag)) {
		    sv2026 = v20 + v26; /* for dudx, dudz */
 	            sv0208 = v2 + v8; /* for dudx, dudz */
                    if(zflag) {
        	       sv1824 = v18 + v24;
		       sv1925 = v19 + v25;
		       sv0006 = v0 + v6;
		       sv0107 = v1 + v7;
        	       dv2103 = v21 - v3;
		       dv2204 = v22 - v4;
                       dv2305 = v23 - v5;  /* mod - 08-27-2008 drg */
		    }
		 }
              }

	      else {  /* x>=1, y>=2 */
	         /* z-1 slice */
		 v0 = v1;
		 v1 = v2;
		 v2 = *(++vptr0);

                 v3 = v4;
		 v4 = v5;
	         v5 = *(++vptr1);

                 v6 = v7;
                 v7 = v8;
		 v8 = *(++vptr2);
                 /*z slice */
		 v9 = v10;
		 v10 = v11;
		 v11 = *(++vptr3);

                 v12 = v13;
		 v13 = v14;
	         v14 = *(++vptr);

                 v15 = v16;
                 v16 = v17;
		 v17 = *(++vptr5);
		 /* z+1 slice */
		 v18 = v19;
		 v19 = v20;
		 v20 = *(++vptr6);

                 v21 = v22;
		 v22 = v23;
	         v23 = *(++vptr7);

                 v24 = v25;
                 v25 = v26;
		 v26 = *(++vptr8);

                 /* slide differences for dudy */
		 if(yflag) {
                    dv0600 = dv0701;
		    dv0701 = dv0802;
		    dv1509 = dv1610;
		    dv1610 = dv1711;
		    dv2418 = dv2519;
		    dv2519 = dv2620;
		 }
                 /* slide sums for dudz */
                 if((zflag) || (xflag)) {
                    sv1824 = sv1925; /* for dudx, dudz */
		    sv1925 = sv2026; /* for dudx, dudz */

		    sv0006 = sv0107; /* for dudx, dudz */
		    sv0107 = sv0208; /* for dudx, dudz */
		    sv2026 = v20 + v26; /* for dudx, dudz */
		    sv0208 = v2 + v8; /* for dudx, dudz */

		    if(zflag) {
                       dv2103 = dv2204;
		       dv2204 = dv2305;
                       dv2204 = v22 - v4; /* for dudz */
                       dv2305 = v23 - v5;  /* mod - 08-27-2008 drg */
  	           }

		 }

             }


  /*
  v0  v1  v2    v9  v10 v11    v18 v19 v20
  v3  v4  v5    v12 v13 v14    v21 v22 v23
  v6  v7  v8    v15 v16 v17    v24 v25 v26
     z-1            z              z+1
     */
	/* du/dx  across alternating planes left and right of current voxel */
  /* corners of planes */
  /* centers of edges of planes */
  /* centers of sides - adjacent in x-1, x+1 in same slice */

        /* dudx = a * (v2 + v20 + v8 + v26 - v0 - v18 - v6 -v24) + \
	       b * (v11 + v5 + v23 + v17 - v9 - v3 -v21 - v15) + \
	       c * (v14 - v12);*/
        if(xflag) {
           dudx = a * (sv0208 + sv2026 - sv0006 - sv1824) + \
	       b * (v11 + v5 + v23 + v17 - v9 - v3 -v21 - v15) + \
	       c * (v14 - v12);
           temp = fabs(dudx);
 	   if(temp>=TINYNUMBER)
              *(gptr[0]) = dudx; /* sum gradient components in output image array */
		   /*else
		      *gptr[0] = 0.0; */ /* already zeroed out when allocated */
        }

/*	dudy = a * (v6 + v8 + v24 + v26 - v0 - v2 - v18 - v20) + \
	       b * (v7 + v15 + v17 + v25 - v1 - v9 - v11 - v19) + \
	       c * (v16 - v10) ; */
	if(yflag) {
          dv0802 = v8 - v2;    /* for dudy */
	  dv1711 = v17 - v11; /* for dudy */
	  dv2620 = v26 - v20; /* for dudy */

   	   dudy = a * (dv0600 + dv0802 + dv2418 + dv2620) + \
	       b * (dv0701 + dv1509 + dv1711 + dv2519) + \
	       c * dv1610;
	   temp = fabs(dudy);
           if(temp>=TINYNUMBER)
       	      *(gptr[ybrik]) = dudy; /* sum gradient components in output image array */
	   /*else *gptr[0] = 0.0; */ /* already zeroed out when allocated */
	}

/*	dudz = a * (v18 + v20 + v24 + v26 - v0 - v2 - v6 - v8) + \
	       b * (v19 + v21 + v23 + v25 - v1 - v3 - v5 - v7) + \
	       c * (v22 - v4);*/
	if(zflag) {
           dudz = a * (sv1824 + sv2026 - sv0006 - sv0208) + \
               b * (sv1925 + dv2103 + dv2305 - sv0107) + \
               c * dv2204;
	   temp = fabs(dudz);
           if(temp>=TINYNUMBER)
       	      *(gptr[noutm1]) = dudz; /* sum gradient components in output image array */
	   /*else *gptr[0] = 0.0; */ /* already zeroed out when allocated */

           *(gptr[noutm1]) = dudz;
         }

	baseoffset++;
        for(ii=0;ii<nout;ii++)
           gptr[ii]++;
       }   /* x */
      }  /* y */
     } /* z */
   } /* end 3D case */

#endif


   RETURN(Gradient_Im);
}

/*! get voxel value at x,y,z from image but limit by dimensions and mask */
static INLINE float vox_val(int x,int y,int z,float *imptr, int nx, int ny, int nz, byte *maskptr, int i, int j, int k)
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
     (*((offset) + min(max((y),0),(ny-1))*(nx) + min(max((x),0),(nx-1)))<)*/

   return(voxval);
}


/* Compute eigenvectors and eigenvalues for Gradient matrix */
MRI_IMARR *Eig_Gradient(MRI_IMARR *Gradient_Im, int flag2D3D, byte *maskptr)
{
  MRI_IMARR *Eig_Im;
  MRI_IMAGE *im;
  byte *tempmaskptr;

   float *gptr[12];
   int ii, jj, starti, endi;
   register float a1, a2, a3, aa2;
   double a13, rad, L1, L2, x11, x12, x21, x22;
   int nx, ny, nz, nxyz;
   float maxim, tempmax0, tempmax2;
   double almostzero;
    double aa[9], e[3];

   ENTRY("Eig_Gradient");
   tempmaskptr = maskptr;
   im = Gradient_Im->imarr[0];
   nx = im->nx; ny = im->ny; nz = im->nz; nxyz = im->nxyz;

   /* will use Gradient_Im structure and data space in place already for
      eigenvalues and eigenvectors ) */
   if(flag2D3D==2) {
     starti = 3;
     endi = 6;
   }
   else {
     starti = 6;
     endi = 12;
   }

   /* for 2D, need 6 sub-briks in output mri_imarr-2 eigenvalues,4 vector components  */
   for(ii=starti;ii<endi; ii++) {  /* add 3 sub-briks to the current mri_imarr for each original sub-brik*/
      im = mri_new_vol(nx, ny, nz, MRI_float);
      if(im==NULL) {
	ERROR_message("can not create temporary data storage");
        RETURN(NULL);
      }
      ADDTO_IMARR(Gradient_Im, im);
   }

    for(ii=0;ii<endi;ii++) {
       im  = (Gradient_Im->imarr[ii]);
       gptr[ii] = (float *) mri_data_pointer(im);
     }

    /* find max Sxx, Syy in gradients */
   im  = (Gradient_Im->imarr[0]);
   tempmax0 = Find_Max_Im(im, maskptr);   /* max Sxx */
   if(flag2D3D==2)
      im  = (Gradient_Im->imarr[2]);
   else
      im  = (Gradient_Im->imarr[3]);
   tempmax2 = Find_Max_Im(im, maskptr);   /* max Syy */

   if(tempmax0>tempmax2)
      maxim = tempmax0;
   else
      maxim = tempmax2;

   if(flag2D3D==3) {
      im  = (Gradient_Im->imarr[5]);
      tempmax2 = Find_Max_Im(im, maskptr);   /* max Szz */
      if(tempmax2>maxim)
	maxim = tempmax2;
   }

   almostzero = maxim / 100000.0;

   for(ii=0;ii<nxyz;ii++) {
      if(maskptr && (!(*tempmaskptr++))) {

       for(jj=0;jj<endi;jj++) {
           *gptr[jj] = 0.0;
        }
      }
      else
       {

      /* for 2D case, solve by "hand" */

	 if(flag2D3D==2) {

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

	 }
         else {   /* 3d flag */
	   aa[0] = *gptr[0];
	   aa[1] = *gptr[1];
	   aa[2] = *gptr[2];

	   aa[3] = *gptr[1];
	   aa[4] = *gptr[3];
	   aa[5] = *gptr[4];

	   aa[6] = *gptr[2];
	   aa[7] = *gptr[4];
	   aa[8] = *gptr[5];


           symeig_double(3, aa, e);  /* e has eigenvalues in result, aa has eigenvectors */

           /* add check for small eigenvalues here */


           /* reverse the order in gptr to give maximum value first */
           *gptr[0] = e[2]; /* eigenvalues */
           *gptr[1] = e[1];
           *gptr[2] = e[0];

           *gptr[3] = aa[6]; /* eigenvectors */
           *gptr[4] = aa[7];
           *gptr[5] = aa[8];

           *gptr[6] = aa[3];
           *gptr[7] = aa[4];
           *gptr[8] = aa[5];

           *gptr[9] = aa[0];
           *gptr[10] = aa[1];
           *gptr[11] = aa[2];
         }


      } /* not masked */
      for(jj=0;jj<endi;jj++) {
        if(isnan(*gptr[jj]))
	   *gptr[jj] = 0.0;
	gptr[jj]++;   /* increment pointers for next voxel */
      }

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
    double c1 = 0.01f, c2 = -0.01f;
    /* c2 = -1.00,*/
    double mc1 = 0.99, evensplit, secondsplit;
    double e1, e2,e3, e12, a, b, emax;
    /* e1me2;*/
    float *gptr[3];
    int nxyz, ii, jj, endi;
    byte *tempmaskptr;

    ENTRY("Compute_Phi");

    if(flag2D3D==2) {
      evensplit = 0.5;
      secondsplit = 1.0;
    }
    else {
      evensplit = 1.0/3.0;
      secondsplit = 0.5;
    }
    endi = flag2D3D;

    im = EV_Im->imarr[0];
    nxyz = im->nxyz;
    tempmaskptr = maskptr;

    /* replace first two eigenvalue sub-briks with phi values */
    for(ii=0;ii<endi;ii++) {
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
         for(jj=0;jj<endi;jj++) {
             *gptr[jj] = 0.0;
         }
       }
       else {
         e1 = *gptr[0];
         e2 = *gptr[1];

         if(e1<=0.0) {    /* e1 equal or close to zero */
           for(jj=0;jj<endi;jj++) {
	     *gptr[jj] = evensplit;    /* phi values all equal - 2D 0.5,0.5 or for 3D 0.33, 0.33, 0.33 */
           }
         }
         else {
	    if(e2<=0.0) {  /* e2 equal or close to zero */
             *gptr[0] = 0.0;
             for(jj=1;jj<endi;jj++)
	       *gptr[jj] = secondsplit;  /* remaining phi value  - 2D 0 1 or for 3D 0 0.5 0.5 */
            }
            else {
              e1 = 1.0/e1;
              e2 = 1.0/e2;
              e12 = e1 + e2;
              if(flag2D3D==3) {
 	        e3 = *gptr[2];
                if(e3<=0) {          /* zero e3 - phi for 3D is 0 0 1 */
		  e3 = 1.0;
                  e1 = 0.0;
                  e2 = 0.0;
                  e12 = 1.0;
                }
                else {               /* normal case phi- e1 / sum(1/e1+1/e2+1/e3) */
                  e3 = 1.0 / e3;
	          e12 += e3;
                }
                *gptr[2] = e3 / e12;
              }
              *gptr[0] = e1 / e12;
              *gptr[1] = e2 / e12;
             }
         }
        } /* included in mask */
       for(jj=0;jj<endi;jj++)
  	  gptr[jj]++;
      } /* end for each voxel in volume */
   } /* end Ding method */
   else {                     /* use exponential method instead */
      for(ii=0;ii<nxyz;ii++) {
        if(maskptr && !*tempmaskptr++) {
	  for(jj=0;jj<endi;jj++)
             *gptr[jj] = 0.0;
        }
        else {
           e1 = *gptr[0];
           e2 = *gptr[1];
           if(flag2D3D==3) {    /* 3D case */
             e3 = *gptr[2];
             e12 = e1 + e2 + e3;
             if(e12<TINYNUMBER) {       /* if very small numbers or zero */
	       e1 = e2 =e3 = evensplit;  /* set to be all equal = 1/3 */
             }
             else {        /* scale eigenvalues to sum to 1 */
               e1 = e1 / e12;
               e2 = e2 / e12;
               e3 = e3 / e12;
	     }
             *gptr[0] = c1;
             if(e1==e2) {
               *gptr[1] = c1;
	     }
             else {
               e12 =  e1 - e2;
               e12 *= e12;
               *gptr[1] = 0.01 + (0.99 * exp (c2/e12));
	     }

            if(e1==e3) {
              *gptr[2] = c1;
	    }
            else {
               e12 =  e1 - e3;
               e12 *= e12;
               *gptr[2] = 0.01 + (0.99 * exp (c2/e12));
	     }
 	   }
           else {    /* 2D case */
	       e12 = e1 + e2;
               if(e12<TINYNUMBER)
                 e1 = e2 = evensplit;
               else {
                 e1 = e1 / e12;
                 e2 = e2 / e12;
               }
                if(e1==e2)
	           *gptr[1] = c1;
                else {
                   e12 = (e1-e2);
                   e12 *= e12;
                 *gptr[1] =  c1 + (mc1 * exp(c2 / e12) );
                }
                *gptr[0] = c1;
	   }  /* end in 2D */
	} /* end in mask */
       gptr[0]++; gptr[1]++;
       if(flag2D3D==3) gptr[2]++;
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

/* compute FA, MD, Cl, Cp, Cs, and more */
THD_3dim_dataset *Compute_DiffMeasures(MRI_IMARR *EV_Im, int flag2D3D,
                                       byte *maskptr,
                                       THD_3dim_dataset *grid_dset, float *cen)
  {
    THD_3dim_dataset *dout=NULL;
    MRI_IMARR *TP_Im=NULL;
    MRI_IMAGE *im=NULL;
    int nxyz, ii, jj, endi, nout = 6, vi, vj, vk;
    float *inptr[12], *outptr[nout], e0, e1, e2, tr, d,
          v0[3], nV, nR, R[3], xyz[3];

    byte *tempmaskptr;

    ENTRY("Compute_DiffMeasures");

    if(flag2D3D==2) {
      ERROR_message("Not ready for 2D form, complain heartily to D. Glen!");
      RETURN(NULL);
    }

    if (!EV_Im || !grid_dset) {
      ERROR_message("Lousy input!");
      RETURN(NULL);
    }

    /* Centroid */
    if (!cen) {
      ERROR_message("I need me a centroid");
      RETURN(NULL);
    }

    endi = flag2D3D;

    im = EV_Im->imarr[0];
    nxyz = im->nxyz;
    tempmaskptr = maskptr;

    /*Prepare for output */
    INIT_IMARR(TP_Im);
    for(ii=0;ii<nout; ii++) {
      im = mri_new_vol(im->nx, im->ny, im->nz, MRI_float);
      if(im==NULL) {
	ERROR_message("cannot create data storage");
        RETURN(NULL);
      }
      ADDTO_IMARR(TP_Im, im);
    }

    /* Get eigen values and 1st eigen vector direction
       This is not set to work for 2D smoothing!       */
    for(ii=0;ii<endi+3;ii++) {
       im  = (EV_Im->imarr[ii]);
       inptr[ii] = (float *) mri_data_pointer(im);
    }

    /* init output pointers */
    for(ii=0;ii<nout;ii++) {
       im  = (TP_Im->imarr[ii]);
       outptr[ii] = (float *) mri_data_pointer(im);
    }

   /* Compute nout params */
   for(ii=0;ii<nxyz;ii++) {
       if(maskptr && !*tempmaskptr++) {
         for(jj=0;jj<nout;jj++) {
             *outptr[jj] = 0.0;
         }
       } else {
         e0 = *inptr[0];
         e1 = *inptr[1];
         e2 = *inptr[2];
         v0[0] = *inptr[3];
         v0[1] = *inptr[4];
         v0[2] = *inptr[5];

         tr = (e0+e1+e2);
                           /* FA */
         if ((d = (e0*e0+e1*e1+e2*e2))>0.0f) {
            *outptr[0] = sqrt(0.5*( (e0-e1)*(e0-e1)+
                                    (e1-e2)*(e1-e2)+
                                    (e0-e2)*(e0-e2)  )/ d );
            if (*outptr[0] > 1.0) *outptr[0] = 1.0;
         } else *outptr[0] = 0.0;

                           /* MD */
         *outptr[1] = (tr)/3.0;
         if (*outptr[1] < 0.0) *outptr[1] = 0.0;
                           /* Cl */
         if (tr > 0.0) {
            *outptr[2] = (e0-e1)/tr;
            if (*outptr[2] > 1.0) *outptr[2] = 1.0;
         } else *outptr[2] = 0.0;

                           /* Cp */
         if (tr > 0.0) {
            *outptr[3] = 2.0*(e1-e2)/tr;
            if (*outptr[3] > 1.0) *outptr[3] = 1.0;
         } else *outptr[3] = 0.0;
                           /* Cs */
         if (tr > 0.0) {
            *outptr[4] = 3.0*e2/tr;
            if (*outptr[4] < 0.0) *outptr[4] = 0.0;
         } else *outptr[4] = 0.0;
                           /* Dot product of v0 with direction from center */
         if (tr > 0.0) {
            AFNI_1D_to_3D_index(ii, vi, vj, vk,
                                DSET_NX(grid_dset),
                                DSET_NX(grid_dset)*DSET_NY(grid_dset));
            AFNI_ijk_to_xyz(  grid_dset ,
                              vi, vj, vk,
                              xyz, xyz+1 , xyz+2 );
            /* nV should be 1 already ... */
            nV = sqrt(v0[0]*v0[0]+v0[1]*v0[1]+v0[2]*v0[2]);
            R[0] = xyz[0]-cen[0];
            R[1] = xyz[1]-cen[1];
            R[2] = xyz[2]-cen[2];
            nR = sqrt(R[0]*R[0]+R[1]*R[1]+R[2]*R[2]);
            #if 0
            if (ii==3949365) {
               fprintf(stderr,"Debugging for voxel [%d %d %d] %d\n"
                              "          R = [%f %f %f] %f, cen = [%f %f %f]\n"
                              "Principal V = [%f %f %f] %f, Principal e = %f\n"
                              "Eigs = [%f %f %f]\n"
                              "dot = %f\n",
                       vi, vj, vk, ii,
                       R[0], R[1], R[2], nR, cen[0], cen[1], cen[2],
                       v0[0], v0[1], v0[2], nV, e0,
                       e0, e1, e2,
                       (R[0]*v0[0]+R[1]*v0[1]+R[2]*v0[2])/(nV*nR));

            }
            #endif
            if ((nV = nV*nR) != 0.0) {
               *outptr[5] = (R[0]*v0[0]+R[1]*v0[1]+R[2]*v0[2])/nV;
            } else *outptr[5] = 0.0;
         } else *outptr[5] = 0.0;
        } /* included in mask */

      /* increment to next voxel */
      for(jj=0;jj<endi+3;jj++)  inptr[jj]++;
      for(jj=0;jj<nout;jj++) outptr[jj]++;
   } /* end for each voxel in volume */

   /* make nice dset */
   dout = Copy_IMARR_to_dset(grid_dset, TP_Im, "Diff_Measures");
   tross_Copy_History (grid_dset, dout);
   EDIT_dset_items(dout,ADN_brick_label_one + 0,"FA",ADN_none);
   EDIT_dset_items(dout,ADN_brick_label_one + 1,"MD",ADN_none);
   EDIT_dset_items(dout,ADN_brick_label_one + 2,"Cl",ADN_none);
   EDIT_dset_items(dout,ADN_brick_label_one + 3,"Cp",ADN_none);
   EDIT_dset_items(dout,ADN_brick_label_one + 4,"Cs",ADN_none);
   EDIT_dset_items(dout,ADN_brick_label_one + 5,"Rdot",ADN_none);
   EDIT_dset_items(dout ,
              ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
	      ADN_prefix , "Diff_Measures",
                       ADN_none ) ;

    RETURN(dout);
  }


/* Compute the D diffusion tensor for the dataset */
MRI_IMARR *ComputeDTensor(MRI_IMARR *phi_Im, int flag2D3D, byte *maskptr)
  {
    MRI_IMARR *DTensor;
    MRI_IMAGE *im;
    double v[4], a1, a2;
    int nxyz, ii, i, endi;
    float *gptr[12], *tempptr;
    byte *tempmaskptr;
    double e10, e11, e12, e20, e21, e22, e30, e31, e32;
    double l1, l2, l3;
    double t1, t3, t5, t8, t10, t12, t14, t18, t19, t21, t23, t32, t33, t35,t37;



    ENTRY("ComputeDTensor");
    /* D = V Phi VT */
    im = phi_Im->imarr[0];
    nxyz = im->nxyz;
    tempmaskptr = maskptr;

    if(flag2D3D==2)
      endi = 6;
    else
      endi = 12;
    /* replace first three phi,eigenvector sub-briks with Dxx,Dxy,Dyy values */
    for(ii=0;ii<endi;ii++) {
       im  = (phi_Im->imarr[ii]);
       gptr[ii] = (float *) mri_data_pointer(im);
     }


    if(flag2D3D==2) {
      for(ii=0;ii<nxyz;ii++) {
         if(maskptr && !(*tempmaskptr++)) {
            for(i=0;i<endi;i++) {
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

           for(i=0;i<endi;i++) {
              gptr[i]++;
           }
        }
      }
    }
    else {      /* 3D option */

    for(ii=0;ii<nxyz;ii++) {

         if(maskptr && !(*tempmaskptr++)) {
            for(i=0;i<endi;i++) {
	      tempptr = gptr[i];
              *tempptr = 0.0;
            }
         }
         else {         /* use maple generated code to calculate V Phi Vt */
           l1 = *gptr[0];
           l2 = *gptr[1];
           l3 = *gptr[2];

           e10 = *gptr[3];
           e11 = *gptr[4];
           e12 = *gptr[5];

           e20 = *gptr[6];
           e21 = *gptr[7];
           e22 = *gptr[8];

           e30 = *gptr[9];
           e31 = *gptr[10];
           e32 = *gptr[11];

           t1 = e10 * e10;
           t3 = e20 * e20;
           t5 = e30 * e30;
           t8 = e10 * l1;
           t10 = e20 * l2;
           t12 = e30 * l3;
           t14 = t8 * e11 + t10 * e21 + t12 * e31;
           t18 = t8 * e12 + t10 * e22 + t12 * e32;
           t19 = e11 * e11;
           t21 = e21 * e21;
           t23 = e31 * e31;
           t32 = e11 * l1 * e12 + e21 * l2 * e22 + e31 * l3 * e32;
           t33 = e12 * e12;
           t35 = e22 * e22;
           t37 = e32 * e32;
           *gptr[0] = t1 * l1 + t3 * l2 + t5 * l3;
           *gptr[1] = t14;
           *gptr[2] = t18;
           *gptr[3] = t19 * l1 + t21 * l2 + t23 * l3;
           *gptr[4] = t32;
           *gptr[5] = t33 * l1 + t35 * l2 + t37 * l3;
	 }
        for(i=0;i<endi;i++) {
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
