#include "mrilib.h"

#define DONT_INCLUDE_ANALYZE_STRUCT
#include "nifti1_io.h"

/*******************************************************************/
/********** 26 Aug 2003: read a NIFTI-1 file as a dataset **********/
/*******************************************************************/

THD_3dim_dataset * THD_open_nifti( char *pathname )
{
   THD_3dim_dataset *dset=NULL ;
   nifti_image *nim ;
   int ntt , nbuc , nvals ;
   int statcode = 0 ;
   THD_ivec3 orixyz ;
   THD_fvec3 dxyz , orgxyz ;
   THD_mat33 R ;

ENTRY("THD_open_nifti") ;

   /*-- open input file --*/

   nim = nifti_image_read( pathname, 0 ) ;

   if( nim == NULL || nim->nifti_type == 0 ) RETURN(NULL) ;

   /*-- extract some useful AFNI-ish information from the nim struct --*/

   /* we must have at least 2 spatial dimensions */

   if( nim->nx < 2 || nim->ny < 2 ) RETURN(NULL) ;

   /* 4th dimension = time; 5th dimension = bucket
      these are mutually exclusive in AFNI at present */

   ntt = nim->nt ; nbuc = nim->nu ;
   if( ntt > 1 && nbuc > 1 ){
     fprintf(stderr,
             "** AFNI can't deal with 5 dimensional NIfTI dataset file %s\n",
             pathname ) ;
     RETURN(NULL) ;
   }
   nvals = MAX(ntt,nbuc) ;

   /* determine type of dataset values */

   switch( nim->datatype ){
      default:
        fprintf(stderr,
                "** AFNI can't handle NIFTI datatype=%d (%s) in file %s\n",
                nim->datatype, nifti_datatype_string(nim->datatype), pathname );
        RETURN(NULL) ;
      break ;

      case DT_UINT8:     datum = MRI_byte    ; break ;
      case DT_INT16:     datum = MRI_short   ; break ;
      case DT_FLOAT32:   datum = MRI_float   ; break ;
      case DT_COMPLEX64: datum = MRI_complex ; break ;
      case DT_RGB24:     datum = MRI_rgb     ; break ;
   }

   if( nim->scl_slope != 0.0          &&
       (datum == MRI_byte || datum == MRI_short) ) datum = MRI_float ;

   /* check for statistics code */

   if( nim->intent_code >= NIFTI_FIRST_STATCODE &&
       nim->intent_code <= NIFTI_LAST_STATCODE    ){

     if( nim->intent_code > FUNC_PT_TYPE ){
       fprintf(stderr,
               "** AFNI doesn't understand NIFTI statistic type %d (%s) in file %s\n",
               nim->intent_code , nifti_intent_string(nim->intent_code) , pathname ) ;
     } else {
       statcode = nim->intent_code ;
       if( nbuc > 1 ){
         fprintf(stderr,
                 "** AFNI doesn't support NIFTI voxel-dependent statistic parameters"
                 " in file %s\n" , pathname ) ;
         statcode = 0 ;
       }
     }
   }

   /* determine orientation from the qto_xyz matrix,
      which transforms (i,j,k) voxel indexes to (x,y,z) LPI coordinates */

   LOAD_MAT(R, -nim->qto_xyz.m[0][0] ,  /* negate x and y   */
               -nim->qto_xyz.m[0][1] ,  /* coefficients,    */
               -nim->qto_xyz.m[0][2] ,  /* since AFNI works */
               -nim->qto_xyz.m[1][0] ,  /* with RAI coords  */
               -nim->qto_xyz.m[1][1] ,
               -nim->qto_xyz.m[1][2] ,
                nim->qto_xyz.m[2][0] ,
                nim->qto_xyz.m[2][1] ,
                nim->qto_xyz.m[2][2]  ) ;

   orixyz = THD_matrix_to_orientation( R ) ;   /* orientation codes */

   /* load the offsets and the grid spacings */

   LOAD_FVEC3( orgxyz , -nim->qto_xyz.m[0][3] ,
                        -nim->qto_xyz.m[1][3] ,
                         nim->qto_xyz.m[2][3]  ) ;

   if( nim->xyz_units == NIFTI_UNITS_METER ){
     nim->dx *= 1000.0 ; nim->dy *= 1000.0 ; nim->dz *= 1000.0 ;
   } else if(  nim->xyz_units == NIFTI_UNITS_MICRON ){
     nim->dx *= 0.001 ; nim->dy *= 0.001 ; nim->dz *= 0.001 ;
   }

   LOAD_FVEC3( dxyz , (ORIENT_sign[orixyz.ijk[0]]=='+') ? nim->dx : -nim->dx ,
                      (ORIENT_sign[orixyz.ijk[1]]=='+') ? nim->dy : -nim->dy ,
                      (ORIENT_sign[orixyz.ijk[2]]=='+') ? nim->dz : -nim->dz  ) ;

   /*-- make a dataset --*/

   dset = EDIT_empty_copy(NULL) ;

   ppp  = THD_trailname(pathname,0) ;               /* strip directory */
   MCW_strncpy( prefix , ppp , THD_MAX_PREFIX ) ;   /* to make prefix */

   nxyz.ijk[0] = nim->nx ;                          /* setup axes */
   nxyz.ijk[1] = nim->ny ;
   nxyz.ijk[2] = nim->nz ;

   iview = (nim->qform_code == NIFTI_XFORM_TALAIRACH )
           ? VIEW_TALAIRACH_TYPE : VIEW_ORIGINAL_TYPE ;

   dset->idcode.str[0] = 'n' ;  /* overwrite 1st 3 bytes with something special */
   dset->idcode.str[1] = 'i' ;
   dset->idcode.str[2] = 'i' ;

   EDIT_dset_items( dset ,
                      ADN_prefix      , prefix ,
                      ADN_datum_all   , datum ,
                      ADN_nxyz        , nxyz ,
                      ADN_xyzdel      , dxyz ,
                      ADN_xyzorg      , orgxyz ,
                      ADN_xyzorient   , orixyz ,
                      ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                      ADN_view_type   , iview ,
                      ADN_type        , (statcode != 0) ? HEAD_FUNC_TYPE
                                                        : HEAD_ANAT_TYPE ,
                    ADN_none ) ;

   /* not a time dependent dataset */

   if( ntt < 2 ){
     EDIT_dset_items( dset ,
                        ADN_nvals     , nbuc ,
                        ADN_func_type , (statcode != 0) ? FUNC_BUCK_TYPE
                                                        : ANAT_BUCK_TYPE ,
                      ADN_none ) ;

   } else {  /* is a time dependent dataset */

          if( nim->time_units == NIFTI_UNITS_MSEC ) nim->dt *= 0.001 ;
     else if( nim->time_units == NIFTI_UNITS_USEC ) nim->dt *= 1.e-6 ;
     EDIT_dset_items( dset ,
                        ADN_nvals    , ntt ,
                        ADN_ntt      , ntt ,
                        ADN_ttorg    , 0.0 ,
                        ADN_ttdel    , nim->dt ,
                        ADN_ttdur    , 0.0 ,
                        ADN_tunits   , UNITS_SEC_TYPE ,
                        ADN_func_type , (statcode != 0) ? FUNC_FIM_TYPE
                                                        : ANAT_EPI_TYPE ,
                      ADN_none ) ;
   }

   /* add statistics, if present */

   if( statcode != 0 ){
     int iv ;
     for( iv=0 ; iv < nvals ; iv++ )
       EDIT_STATAUX4(dset,iv,statcode,nim->intent_p1,nim->intent_p2,nim->intent_p3,0) ;
   }

   /*-- flag to read data from disk using NIFTI functions --*/

   dset->dblk->diskptr->storage_mode = STORAGE_BY_NIFTI ;
   strcpy( dset->dblk->diskptr->brick_name , pathname ) ;

   for( ibr=0 ; ibr < nvals ; ibr++ ){     /* make sub-brick labels */
     sprintf(prefix,"%s[%d]",tname,ibr) ;
     EDIT_BRICK_LABEL( dset , ibr , prefix ) ;
   }

   /* return unpopulated dataset */

   nifti_image_free(nim) ; RETURN(dset) ;
}

/*-----------------------------------------------------------------
  Load a NIFTI dataset's data into memory
  (called from THD_load_datablock in thd_loaddblk.c)
-------------------------------------------------------------------*/

void THD_load_nifti( THD_datablock *dblk )
{
   THD_diskptr *dkptr ;
   int nx,ny,nz,nxy,nxyz,nxyzv , nbad,ibr,nv, nslice , datum ;
   void *ptr , *sbuf=NULL ;
   nifti_image *nim ;
   FILE *fp ;

ENTRY("THD_load_nifti") ;

   /*-- open input [these errors should never occur] --*/

   if( !ISVALID_DATABLOCK(dblk)                        ||
       dblk->diskptr->storage_mode != STORAGE_BY_NIFTI ||
       dblk->brick == NULL                               ) EXRETURN ;

   dkptr = dblk->diskptr ;

   nim = nifti_image_read( dset->dblk->diskptr->brick_name, 0 ) ;
   if( nim == NULL ) EXRETURN ;

   fp = fopen( nim->iname , "rb" ) ;
   if( fp == NULL ){ nifti_image_free(nim); EXRETURN; }
   fseek( fp , nim->iname_offset , SEEK_SET ) ;

   /*-- allocate space for data --*/

   nx = dkptr->dimsizes[0] ;
   ny = dkptr->dimsizes[1] ;  nxy   = nx * ny   ;
   nz = dkptr->dimsizes[2] ;  nxyz  = nxy * nz  ;
   nv = dkptr->nvals       ;  nxyzv = nxyz * nv ; nslice = nz*nv ;

   dblk->malloc_type = DATABLOCK_MEM_MALLOC ;

   /** malloc space for each brick separately **/

   for( nbad=ibr=0 ; ibr < nv ; ibr++ ){
     if( DBLK_ARRAY(dblk,ibr) == NULL ){
       ptr = calloc( 1,DBLK_BRICK_BYTES(dblk,ibr) ) ;
       mri_fix_data_pointer( ptr ,  DBLK_BRICK(dblk,ibr) ) ;
       if( ptr == NULL ) nbad++ ;
     }
   }

   /** if couldn't get them all, take our ball and go home in a snit **/

   if( nbad > 0 ){
     fprintf(stderr,"\n** failed to malloc %d NIFTI bricks out of %d\n\a",nbad,nv) ;
     for( ibr=0 ; ibr < nv ; ibr++ ){
       if( DBLK_ARRAY(dblk,ibr) != NULL ){
         free(DBLK_ARRAY(dblk,ibr)) ;
         mri_fix_data_pointer( NULL , DBLK_BRICK(dblk,ibr) ) ;
       }
     }
     nifti_image_free(nim); EXRETURN;
   }

   /* scaling? */

   datum = DBLK_BRICK_TYPE(dblk,0) ;  /* destination data */

   if( nim->scl_slope != 0 &&
       (nim->datatype==DT_UINT8 || nim->datatype==DT_INT16) ){

     sbuf = calloc( nim->nbyper , nxyz ) ;  /* scaling buffer */
   }

   /** read data! **/

   switch( DBLK_BRICK_TYPE(dblk,0) ){  /* output datum */

      /* this case should never happen */

      default:
         fprintf(stderr,"\n** MINC illegal datum %d found\n",
                 DBLK_BRICK_TYPE(dblk,0)                     ) ;
         for( ibr=0 ; ibr < nv ; ibr++ ){
           if( DBLK_ARRAY(dblk,ibr) != NULL ){
              free(DBLK_ARRAY(dblk,ibr)) ;
              mri_fix_data_pointer( NULL , DBLK_BRICK(dblk,ibr) ) ;
           }
         }
      break ;

      /* data is stored as bytes in AFNI */

      case MRI_byte:{
         int kk,ii,qq ; byte *br ;

         if( nv == 1 ){
            code = nc_get_var_uchar( ncid , im_varid , DBLK_ARRAY(dblk,0) ) ;
         } else {
            size_t start[4] , count[4] ;
            start[1] = start[2] = start[3] = 0 ;
            count[0] = 1 ; count[1] = nz ; count[2] = ny ; count[3] = nx ;
            for( ibr=0 ; ibr < nv ; ibr++ ){
               start[0] = ibr ;
               code = nc_get_vara_uchar( ncid,im_varid ,
                                         start,count , DBLK_ARRAY(dblk,ibr) ) ;
            }
         }
         if( code != NC_NOERR ) EPR(code,dkptr->brick_name,"image get_var_uchar") ;

         if( im_max != NULL ){                 /* must scale values */
           for( ibr=0 ; ibr < nv ; ibr++ ){     /* loop over bricks */
             br = DBLK_ARRAY(dblk,ibr) ;
             for( kk=0 ; kk < nz ; kk++ ){      /* loop over slices */
                outbot = im_min[kk+ibr*nz] ;
                outtop = im_max[kk+ibr*nz] ;
                if( outbot >= outtop) continue ;            /* skip */
                fac = (outtop-outbot) / denom ;
                qq = kk*nxy ; outbot += 0.499 ;
                for( ii=0 ; ii < nxy ; ii++ )        /* scale slice */
                   br[ii+qq] = (byte) ((fac*(br[ii+qq]-inbot) + outbot)*sfac) ;
             }
           }
         }
      }
      break ;

      /* data is stored as shorts in AFNI */

      case MRI_short:{
         int kk,ii,qq ; short *br ;

         if( nv == 1 ){
            code = nc_get_var_short( ncid , im_varid , DBLK_ARRAY(dblk,0) ) ;
         } else {
            size_t start[4] , count[4] ;
            start[1] = start[2] = start[3] = 0 ;
            count[0] = 1 ; count[1] = nz ; count[2] = ny ; count[3] = nx ;
            for( ibr=0 ; ibr < nv ; ibr++ ){
               start[0] = ibr ;
               code = nc_get_vara_short( ncid,im_varid ,
                                         start,count , DBLK_ARRAY(dblk,ibr) ) ;
            }
         }
         if( code != NC_NOERR ) EPR(code,dkptr->brick_name,"image get_var_short") ;

         if( im_max != NULL ){                 /* must scale values */
           for( ibr=0 ; ibr < nv ; ibr++ ){     /* loop over bricks */
             br = DBLK_ARRAY(dblk,ibr) ;
             for( kk=0 ; kk < nz ; kk++ ){      /* loop over slices */
                outbot = im_min[kk+ibr*nz] ;
                outtop = im_max[kk+ibr*nz] ;
                if( outbot >= outtop) continue ;            /* skip */
                fac = (outtop-outbot) / denom ;
                qq = kk*nxy ; outbot += 0.499 ;
                for( ii=0 ; ii < nxy ; ii++ )        /* scale slice */
                   br[ii+qq] = (short) ((fac*(br[ii+qq]-inbot) + outbot)*sfac) ;
             }
           }
         }
      }
      break ;

      /* data is stored as floats in AFNI */

      case MRI_float:{
         int kk,ii,qq ; float *br ;

         if( nv == 1 ){
            code = nc_get_var_float( ncid , im_varid , DBLK_ARRAY(dblk,0) ) ;
         } else {
            size_t start[4] , count[4] ;
            start[1] = start[2] = start[3] = 0 ;
            count[0] = 1 ; count[1] = nz ; count[2] = ny ; count[3] = nx ;
            for( ibr=0 ; ibr < nv ; ibr++ ){
               start[0] = ibr ;
               code = nc_get_vara_float( ncid,im_varid ,
                                         start,count , DBLK_ARRAY(dblk,ibr) ) ;
            }
         }
         if( code != NC_NOERR ) EPR(code,dkptr->brick_name,"image get_var_float") ;

         if( im_type == NC_BYTE ){              /* fix sign of bytes */
           for( ibr=0 ; ibr < nv ; ibr++ ){     /* loop over bricks */
             br = DBLK_ARRAY(dblk,ibr) ;
             for( ii=0 ; ii < nxyz ; ii++ )
                if( br[ii] < 0.0 ) br[ii] += 256.0 ;
           }
         }

         if( im_max != NULL ){                 /* must scale values */
           for( ibr=0 ; ibr < nv ; ibr++ ){     /* loop over bricks */
             br = DBLK_ARRAY(dblk,ibr) ;
             for( kk=0 ; kk < nz ; kk++ ){      /* loop over slices */
                outbot = im_min[kk+ibr*nz] ;
                outtop = im_max[kk+ibr*nz] ;
                if( outbot >= outtop) continue ;            /* skip */
                fac = (outtop-outbot) / denom ;
                qq = kk*nxy ;
                for( ii=0 ; ii < nxy ; ii++ )        /* scale slice */
                   br[ii+qq] = (fac*(br[ii+qq]-inbot) + outbot) ;
             }
           }
         }
      }
      break ;

   } /* end of switch on output datum */

   /*-- throw away the trash and return --*/

   if( im_min != NULL ) free(im_min) ;
   if( im_max != NULL ) free(im_max) ;
   nc_close(ncid) ; EXRETURN ;
}
