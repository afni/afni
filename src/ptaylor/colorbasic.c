#include "mrilib.h"
#include "colorbasic.h"
//#include <gsl/gsl_randist.h>
//#include <gsl/gsl_rng.h>
//#include "DoTrackit.h"


/*
  RGB -> HSL calculation, based on this website:
  http://www.niwa.nu/2013/05/math-behind-colorspace-conversions-rgb-hsl/
  Am assuming that RGB elements are already [0,1]
*/

/*
  RGB -> HSL conversion on a trio-by-trio basis
 */
int Color_RGB_to_HSL( float *RGB,           // N by 3 array
                      float *HSL            // N by 3 array
                      )
{
   int j;
   float min_val, max_val;
   int   min_ind, max_ind;
   float mm_diff;
   
   // for: LUMINANCE
   // not so intelligent way to get min/max colors...
   max_val = -1;
   max_ind = -1;
   min_val = 10;
   min_ind = -1;
   for(j=0 ; j<3 ; j++) {
      if( RGB[j] < min_val ){
         min_val = RGB[j];
         min_ind = j;
      }
      if( RGB[j] > max_val ){
         max_val = RGB[j];
         max_ind = j;
      }
   }
   mm_diff = max_val-min_val;
   HSL[2] = 0.5*(min_val+max_val);   // L
   
   // for: SATURATION (and, in some cases, HUE)
   if ( fabs(mm_diff) < 0.0001 ) {
      // case of no saturation -> no Hue either
      HSL[1] = 0.;
      HSL[0] = 0.;
   }
   else {
      if( HSL[2] < 0.5 )
         HSL[1] = mm_diff/(min_val+max_val);
      else  // Should still never be 0 in denom, I think...
         HSL[1] = mm_diff/(2.-min_val-max_val);
      
      // for (remaining cases): HUE
      switch(max_ind) {
         
      case 0: // R is max
         HSL[0] = (RGB[1] - RGB[2])/mm_diff;
         break;
      case 1: // G is max
         HSL[0] = (RGB[2] - RGB[0])/mm_diff;
         HSL[0]+= 2.0;
         break;
      case 2: // B is max
         HSL[0] = (RGB[0] - RGB[1])/mm_diff;
         HSL[0]+= 4.0;
         break;
      default:
         ERROR_exit("Trouble finding min/max in RGB coloration!"
                    " max_ind = %d ???\n\t"
                    "RGB = (%.4f, %.4f, %.4f)",
                    max_ind, RGB[0], RGB[1], RGB[2]);
      }
      // *currently*, range of values: [0, 360], I think, give
      // or take a boundary value
      HSL[0]*= 60;       // to be in units of degrees
      if( HSL[0]<0 )     // color *wheel*
         HSL[0]+= 360;
      HSL[0] = 360 - HSL[0];  // to color with AJJ, which is 'backwards' rot
      if ( HSL[0]<0 )
         ERROR_exit("Negative HUE still:  %f ?!",
                    HSL[0]);
      else if ( HSL[0]>360 )
         ERROR_exit("Huge HUE:  %f ?!",
                    HSL[0]);
      HSL[0]/= 360.;

   }
   
   RETURN(0);
}



/*
  Program with looping
*/
int Color_Vec_RGB_to_HSL( float **RGB,           // N by 3 array
                          float **HSL,           // 3xn array
                          byte *mskd,            // byte mask
                          int Nvox               // num vox  
                          )  
{
   int   i,j;
   int   out;
   float inp[3]= {0.,0.,0.};

   for(i=0 ; i<Nvox ; i++) 
      if (mskd[i]) {
         
         out = Color_RGB_to_HSL( RGB[i], inp);  

         // and copy values over because of odd shape of HSL
         for( j=0 ; j<3 ; j++ )
            HSL[j][i] = inp[j];
      }

   RETURN(0);
}


/*
  XYZ->RGB calculation, based on this website:
  http://www.brucelindbloom.com/index.html?Equations.html
*/
int Color_Vec_XYZdset_to_RGB( THD_3dim_dataset *VEC, // 3-vect
                              float **RGB,           // N by 3 array
                              byte *mskd,            // byte mask
                              int Nvox               // num vox  
                              )  
{
   int i,j,k;

   // selecting 'sRGB (ref. white = D65)', from list
   float Minv[3][3] =    \
      {{3.2404542, -1.5371385, -0.4985314},
       {-0.9692660, 1.8760108,  0.0415560},
       {0.0556434, -0.2040259,  1.0572252}};

   //for(k=0 ; k<3 ; k++)
   // fprintf(stderr,"%f\t", Minv[0][k]);

   for(i=0 ; i<Nvox ; i++) 
      if (mskd[i]) {
         for(j=0 ; j<3 ; j++) {
            RGB[i][j] = fabs(THD_get_voxel(VEC, i, j));
            // for(k=0 ; k<3 ; k++)
            //   RGB[i][j]+= Minv[j][k]* fabs(THD_get_voxel(VEC, i, k));
            // compand
            
            //RGB[i][j] = Color_Compand_sRGB(RGB[i][j]);
            //fprintf(stderr,"%f\t",RGB[i][j]);
         }
      }

   RETURN(0);
}

/*
  Companding recipe, also from:
  http://www.brucelindbloom.com/index.html?Equations.html
*/
float Color_Compand_sRGB(float x)
{
   float y;
   float expfac = 1./2.4;

   if( x<= 0.0031308)
      y = 12.92*x;
   else
      y = 1.055 * pow(x,expfac) - 0.055;

   return y;
}

/* 
   Basic thing to get dimensions of a data set and returned Nvox, just
   at one go. Ndim is either 3 or 4, depending on whether you want only
   spatial or space+time, respectively.
*/
int Basic_Dim_and_Nvox( THD_3dim_dataset *X, int *Dim, int Ndim, 
                         char *prefix)
{
   int Nvox = -1;

   if( Ndim == 4 ) {
      Dim[3] = DSET_NVALS(X); 
      if( Dim[3] <=0 )
         ERROR_exit("\n\n Problem getting %s data set dimension [3].\n\n",
                 prefix);
   }
   else if( Ndim != 3 ){
      ERROR_exit("\n\n 'Ndim' arg must be either 3 or 4.\n\n",
                 prefix);
   }

   Nvox = DSET_NVOX(X);
   Dim[0] = DSET_NX(X); 
   Dim[1] = DSET_NY(X); 
   Dim[2] = DSET_NZ(X); 

   if( (Nvox <=0) || (Dim[0] <=0) || (Dim[1] <=0) || (Dim[2] <=0) ){
      ERROR_exit("\n\n Problem getting %s data set dimensions.\n\n",
                 prefix);
    }

   return Nvox;
}

/*
  Compare basic properties of data set: just dimensions right now
 */
int Basic_compare_DSET_dims( THD_3dim_dataset *X, THD_3dim_dataset *Y,
                         int Ndim, char *prefixX, char *prefixY)
{
   int i;
   int NvoxX = -1, NvoxY = -1;
   int DimX[4]={0,0,0,0};     // dim in each dir
	int DimY[4]={0,0,0,0};     // dim in each dir

   NvoxX = Basic_Dim_and_Nvox( X, 
                               DimX, 
                               Ndim, 
                               prefixX);

   NvoxY = Basic_Dim_and_Nvox( Y, 
                               DimY, 
                               Ndim, 
                               prefixY);

   for( i=0 ; i<Ndim ; i++ )
      if( DimX[i] != DimY[i] ) {
         ERROR_exit("\n\n Dsets %s %s don't match in [%d] dimension.\n\n",
                    prefixX, prefixY, i);
      }

   RETURN(0);
}
