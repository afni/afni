/*---------------------------------------------------------------------
  01 Feb 2001: compute the center of a dataset (DICOM coord order)
  Adapted from part of thd_info.c -- RWCox
-----------------------------------------------------------------------*/

#include "mrilib.h"

THD_fvec3 THD_dataset_center( THD_3dim_dataset * dset )
{
   THD_dataxes * daxes ;
   THD_fvec3 fv1 , fv2 ;

ENTRY("THD_dataset_center") ;

   if( !ISVALID_DSET(dset) ){ LOAD_FVEC3(fv1,0,0,0); RETURN(fv1); }

   daxes = dset->daxes ;

   LOAD_FVEC3(fv1 , daxes->xxorg , daxes->yyorg , daxes->zzorg) ;
   fv1 = THD_3dmm_to_dicomm( dset , fv1 ) ;

   LOAD_FVEC3(fv2 , daxes->xxorg + (daxes->nxx-1)*daxes->xxdel ,
                    daxes->yyorg + (daxes->nyy-1)*daxes->yydel ,
                    daxes->zzorg + (daxes->nzz-1)*daxes->zzdel  ) ;
   fv2 = THD_3dmm_to_dicomm( dset , fv2 ) ;

   fv1.xyz[0] = 0.5f * (fv1.xyz[0]+fv2.xyz[0]) ;
   fv1.xyz[1] = 0.5f * (fv1.xyz[1]+fv2.xyz[1]) ;
   fv1.xyz[2] = 0.5f * (fv1.xyz[2]+fv2.xyz[2]) ;

   RETURN(fv1) ;
}

/*-------------------------------------------------------------------------*/
/*! Get the center of mass of this volume, in DICOM coords, taken from 3dCM.

// [PT, 20 Dec, 2016] Updated to output either: ijk, or the (default)
// xyz in DICOM coords.  New input variable 'cmode' controls this
// (default, cmode=0).

---------------------------------------------------------------------------*/

THD_fvec3 THD_cmass( THD_3dim_dataset *xset , int iv , byte *mmm,
                     int cmode)
{
   THD_fvec3 cmv ;
   THD_ivec3 cmvi ;
   MRI_IMAGE *im ;
   float *far , icm,jcm,kcm ;
   int ii , nvox ;

   LOAD_FVEC3(cmv,0,0,0) ;

   nvox = DSET_NVOX(xset) ;
   im   = mri_to_float( DSET_BRICK(xset,iv) ) ;
                             if( im  == NULL ) return cmv ;
   far = MRI_FLOAT_PTR(im) ; if( far == NULL ) return cmv ;

   if( mmm != NULL ){
     for( ii=0 ; ii < nvox ; ii++ )
       /* if mask is NOT set, clear it    2 Jan 2013 [rickr/dglen] */
       if( ! mmm[ii] ) far[ii] = 0.0 ;
   }

   mri_get_cmass_3D( im , &icm,&jcm,&kcm ) ; mri_free(im) ;

   LOAD_FVEC3(cmv,icm,jcm,kcm) ;
   cmv = THD_3dfind_to_3dmm( xset , cmv ) ;

   // [PT, Dec, 2016]: a bit convoluted, but return ijk (as floats) if
   // the mode suits it.
   if( cmode == 1 ) {  // return int ijk in dset-orientation
      cmvi = THD_3dmm_to_3dind( xset , cmv );
      LOAD_FVEC3(cmv,cmvi.ijk[0], cmvi.ijk[1], cmvi.ijk[2]);
      return cmv; 
   }

   cmv = THD_3dmm_to_dicomm( xset , cmv ) ;
   return cmv ;
}

/*-------------------------------------------------------------------------*/
/*! Get the center of mass of integer labeled ROIs in a sub-brick. Returns
a float vector N_rois XYZ triplets.
---------------------------------------------------------------------------*/

float *THD_roi_cmass(THD_3dim_dataset *xset , int iv , int *rois, 
                     int N_rois, int cmode)
{
   float *xyz=NULL, roi;
   THD_fvec3 cmr ;
   int ir;
   byte *mmm;
   
   ENTRY("THD_roi_cmass");
   
   if (!xset || !rois || N_rois < 1) RETURN(NULL);
   
   xyz = (float *)calloc(N_rois*3, sizeof(float));
   for (ir = 0; ir < N_rois; ++ir) {
      roi = rois[ir];
      mmm = THD_makemask( xset, iv , roi , roi );
      cmr = THD_cmass( xset, iv , mmm, cmode);
      free(mmm); mmm = NULL;
      xyz[3*ir]   = cmr.xyz[0]; 
      xyz[3*ir+1] = cmr.xyz[1]; 
      xyz[3*ir+2] = cmr.xyz[2];
   }
   
   RETURN(xyz);
}

/* find internal center "Icent" in volume
 * for some shapes, cmass can be outside the region
 * this method forces the result to be in a voxel
 * Parameters: xset=dataset, iv = subbrick, mmm=mask array
 * cmode = 0/1 return xyz position or ijk index
 * cmxyz = cm center of mass in xyz  */ 
THD_fvec3 THD_Icent( THD_3dim_dataset *xset , int iv , byte *mmm,
                     int cmode, THD_fvec3 cmxyz)
{
   THD_fvec3 cmv ;
   THD_ivec3 cmvi ;
   MRI_IMAGE *im ;
   float *far, xcm, ycm, zcm ;
   int ii, jj, kk, nvox ;
   THD_dataxes *daxes ;
   int koff, joff, nx,ny,nz,nxy ;
   double xx,yy,zz , vsum , val ;
   double wbest;
   float xmi,ymi,zmi;

   xcm = cmxyz.xyz[0];
   ycm = cmxyz.xyz[1];
   zcm = cmxyz.xyz[2];

   LOAD_FVEC3(cmv,0,0,0) ;

   nvox = DSET_NVOX(xset) ;
   im   = mri_to_float( DSET_BRICK(xset,iv) ) ;
                             if( im  == NULL ) return cmv ;
   far = MRI_FLOAT_PTR(im) ; if( far == NULL ) return cmv ;

   if( mmm != NULL ){
     for( ii=0 ; ii < nvox ; ii++ )
       /* if mask is NOT set, clear it    2 Jan 2013 [rickr/dglen] */
       if( ! mmm[ii] ) far[ii] = 0.0 ;
   }


   daxes = CURRENT_DAXES(xset) ;

   nx  = im->nx ; ny = im->ny ; nz = im->nz ; nxy = nx*ny ;
   /* some default distance to beat and a default center coordinate */ 
   wbest = 1.e+37f ; xmi=ymi=zmi = 0.0f ;

   for( kk=0 ; kk < nz ; kk++ ){
     koff = kk * nxy ;
     zz = daxes->zzorg + kk * daxes->zzdel;
     for( jj=0 ; jj < ny ; jj++ ){
       joff = koff + jj * nx ;
       yy = daxes->yyorg + jj * daxes->yydel;
       for( ii=0 ; ii < nx ; ii++ ){
         if(far[ii+joff]!=0.0) {
            xx = daxes->xxorg + ii * daxes->xxdel;
            vsum =   SQR(xx-xcm) + SQR(yy-ycm) + SQR(zz-zcm); /* use dist^2 here for a bit of speed*/
            if( vsum < wbest ){ xmi = xx; ymi = yy; zmi = zz; wbest=vsum; }
         } 
       }
     }
   }

   mri_free(im) ;

   LOAD_FVEC3(cmv,xmi,ymi,zmi) ;

   // [PT, Dec, 2016]: a bit convoluted, but return ijk (as floats) if
   // the mode suits it.
   if( cmode == 1 ) {  // return int ijk in dset-orientation
      cmvi = THD_3dmm_to_3dind( xset , cmv );
      LOAD_FVEC3(cmv,cmvi.ijk[0], cmvi.ijk[1], cmvi.ijk[2]);
      return cmv; 
   }

   cmv = THD_3dmm_to_dicomm( xset , cmv ) ;
   return cmv ;
}

/* find distance center "Dcent" in volume
 * for some shapes, cmass can be outside the region
 * like Icent this method forces the result to be in a voxel
** but computes the center as the voxel location with the smallest average
 * distance to all the others 
 * Parameters: xset=dataset, iv = subbrick, mmm=mask array
 * cmode = 0/1 return xyz position or ijk index
 * cmxyz = cm center of mass in xyz  */ 
THD_fvec3 THD_Dcent( THD_3dim_dataset *xset , int iv , byte *mmm,
                     int cmode, THD_fvec3 cmxyz)
{
   THD_fvec3 cmv ;
   THD_ivec3 cmvi ;
   MRI_IMAGE *im ;
   float *far ;
   int ii, jj, kk, nvox ;
   THD_dataxes *daxes ;
   int koff, joff, nx,ny,nz,nxy ;
   double xx,yy,zz , vsum , val ;
   double wbest;
   float xmi,ymi,zmi, xorg,yorg, zorg, xdel, ydel, zdel;

   LOAD_FVEC3(cmv,0,0,0) ;

   nvox = DSET_NVOX(xset) ;
   im   = mri_to_float( DSET_BRICK(xset,iv) ) ;
                             if( im  == NULL ) return cmv ;
   far = MRI_FLOAT_PTR(im) ; if( far == NULL ) return cmv ;

   if( mmm != NULL ){
     for( ii=0 ; ii < nvox ; ii++ )
       /* if mask is NOT set, clear it    2 Jan 2013 [rickr/dglen] */
       if( ! mmm[ii] ) far[ii] = 0.0 ;
   }


   daxes = CURRENT_DAXES(xset) ;

   nx  = im->nx ; ny = im->ny ; nz = im->nz ; nxy = nx*ny ;
   /* some default distance to beat and a default center coordinate */ 
   wbest = 1.e+37f ; xmi=ymi=zmi = 0.0f ;
   xorg = daxes->xxorg;
   yorg = daxes->yyorg;
   zorg = daxes->zzorg;
   xdel = daxes->xxdel;
   ydel = daxes->yydel;
   zdel = daxes->zzdel;

   for( kk=0 ; kk < nz ; kk++ ){
     koff = kk * nxy ;
     for( jj=0 ; jj < ny ; jj++ ){
       joff = koff + jj * nx ;
       for( ii=0 ; ii < nx ; ii++ ){
         if(far[ii+joff]!=0.0) {
            zz = zorg + kk * zdel;
            yy = yorg + jj * ydel;
            xx = xorg + ii * xdel;
            /* find average distance of all voxels in the mask to this voxel */
            vsum = THD_xyz_distance(xset, im, xx,yy,zz);  
            if( vsum < wbest ){ xmi = xx; ymi = yy; zmi = zz; wbest=vsum; }
         } 
       }
     }
   }

   mri_free(im) ;

   LOAD_FVEC3(cmv,xmi,ymi,zmi) ;

   // [PT, Dec, 2016]: a bit convoluted, but return ijk (as floats) if
   // the mode suits it.
   if( cmode == 1 ) {  // return int ijk in dset-orientation
      cmvi = THD_3dmm_to_3dind( xset , cmv );
      LOAD_FVEC3(cmv,cmvi.ijk[0], cmvi.ijk[1], cmvi.ijk[2]);
      return cmv; 
   }

   cmv = THD_3dmm_to_dicomm( xset , cmv ) ;
   return cmv ;
}

/* compute average distance in mask to xyz location */
double THD_xyz_distance( THD_3dim_dataset *xset , MRI_IMAGE *im , 
   double xcm, double ycm, double zcm)
{
   float *far;
   int ii, jj, kk, nvox, nmask ;
   THD_dataxes *daxes ;
   int koff, joff, nx,ny,nz,nxy ;
   double xx,yy,zz , vsum , bad_vsum, val ;

   nmask = 0; bad_vsum = 1.e+37f;
   vsum = 0.0;

   nvox = DSET_NVOX(xset) ;
   far = MRI_FLOAT_PTR(im) ; if( far == NULL ) return bad_vsum ;

   daxes = CURRENT_DAXES(xset) ;

   nx  = im->nx ; ny = im->ny ; nz = im->nz ; nxy = nx*ny ;

   for( kk=0 ; kk < nz ; kk++ ){
     koff = kk * nxy ;
     for( jj=0 ; jj < ny ; jj++ ){
       joff = koff + jj * nx ;
       for( ii=0 ; ii < nx ; ii++ ){
         if(far[ii+joff]!=0.0) {
            zz = daxes->zzorg + kk * daxes->zzdel;
            yy = daxes->yyorg + jj * daxes->yydel;
            xx = daxes->xxorg + ii * daxes->xxdel;
            vsum +=   SQR(xx-xcm) + SQR(yy-ycm) + SQR(zz-zcm) ; /* use dist^2 here for a bit of speed*/
            nmask++;
         } 
       }
     }
   }

   if(nmask != 0){
      vsum = vsum / nmask;
      return(vsum);
   }
   else return(bad_vsum);
}
