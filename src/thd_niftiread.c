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

ENTRY("THD_open_nifti") ;

   /*-- open input file --*/

   nim = nifti_image_read( pathname, 0 ) ;

   if( nim == NULL || nim->nifti_type == 0 ) RETURN(NULL) ;

   /*-- extract some useful AFNI-ish information from the nim struct --*/

   /* determine type of dataset values */

   switch( nim->datatype ){
      default:
        fprintf(stderr,
                "** NIFTI error: can't handle datatype=%d (%s) in file %s\n",
                nim->datatype, nifti_datatype_string(nim->datatype), pathname );
        RETURN(NULL) ;
      break ;

      case DT_UINT8:     datum = MRI_byte   ; break ;
      case DT_INT16:     datum = MRI_short  ; break ;
      case DT_FLOAT32:   datum = MRI_float  ; break ;
      case DT_COMPLEX64: datum = MRI_complex; break ;
      case DT_RGB24:     datum = MRI_rgb    ; break ;
   }

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

   /* determine orientation, etc., from the qto_xyz matrix,
      which transforms (i,j,k) voxel indexes to (x,y,z) LPI coordinates */

   { float xi = -nim->qto_xyz.m[0][0] ;  /* negate x and y   */
     float xj = -nim->qto_xyz.m[0][1] ;  /* coefficients,    */
     float xk = -nim->qto_xyz.m[0][2] ;  /* since AFNI works */
     float xo = -nim->qto_xyz.m[0][3] ;  /* with RAI coords  */
     float yi = -nim->qto_xyz.m[1][0] ;
     float yj = -nim->qto_xyz.m[1][1] ;
     float yk = -nim->qto_xyz.m[1][2] ;
     float yo = -nim->qto_xyz.m[1][3] ;
     float zi =  nim->qto_xyz.m[2][0] ;
     float zj =  nim->qto_xyz.m[2][1] ;
     float zk =  nim->qto_xyz.m[2][2] ;
     float zo =  nim->qto_xyz.m[2][3] ;

     float di = sqrt(xi*xi+yi*yi+zi*zi) ;
     float dj = sqrt(xj*xj+yj*yj+zj*zj) ;
     float dk = sqrt(xk*xk+yk*yk+zk*zk) ;

     int ior,jor,kor ; float a,b,c , abc[3] ;

     abc[0] = xi ; abc[1] = xj ; abc[3] = xk ;
     a = fabs(xi) ; ior = 1 ;
     b = fabs(xj) ; if( b > a ){ ior=2 ; a=b; }
     c = fabs(zj) ; if( c > a ){ ior=3 ; }
     if( abc[ior-1] < 0 ) ior = -ior ;

   /*-- make a dataset --*/

   dset = EDIT_empty_copy(NULL) ;

   ppp  = THD_trailname(pathname,0) ;               /* strip directory */
   MCW_strncpy( prefix , ppp , THD_MAX_PREFIX ) ;   /* to make prefix */

   nxyz.ijk[0] = nim->nx ; dxyz.xyz[0] = nim->dx ;       /* setup axes */
   nxyz.ijk[1] = nim->ny ; dxyz.xyz[1] = nim->dy ;
   nxyz.ijk[2] = nim->nz ; dxyz.xyz[2] = nim->dz ;

   orixyz.ijk[0] = xyz[0]->afni_orient ; orgxyz.xyz[0] = xyz[0]->start ;
   orixyz.ijk[1] = xyz[1]->afni_orient ; orgxyz.xyz[1] = xyz[1]->start ;
   orixyz.ijk[2] = xyz[2]->afni_orient ; orgxyz.xyz[2] = xyz[2]->start ;

   iview = (nim->qform_code == NIFTI_XFORM_TALAIRACH )
           ? VIEW_TALAIRACH_TYPE : VIEW_ORIGINAL_TYPE ;

   dset->idcode.str[0] = 'N' ;  /* overwrite 1st 3 bytes with something special */
   dset->idcode.str[1] = 'f' ;
   dset->idcode.str[2] = 'T' ;

   EDIT_dset_items( dset ,
                      ADN_prefix      , prefix ,
                      ADN_datum_all   , datum ,
                      ADN_nxyz        , nxyz ,
                      ADN_xyzdel      , dxyz ,

                      ADN_xyzorg      , orgxyz ,   /* needs a-fixin' */
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

   RETURN(dset) ;
}

/*-----------------------------------------------------------------
  Load a NIFTI dataset's data into memory
  (called from THD_load_datablock in thd_loaddblk.c)
-------------------------------------------------------------------*/

void THD_load_nifti( THD_datablock *dblk )
{
   THD_diskptr *dkptr ;
   int nx,ny,nz,nxy,nxyz,nxyzv , nbad,ibr,nv, nslice ;
   void *ptr ;
   nifti_image *nim ;

ENTRY("THD_load_nifti") ;

   /*-- open input [these errors should never occur] --*/

   if( !ISVALID_DATABLOCK(dblk)                        ||
       dblk->diskptr->storage_mode != STORAGE_BY_NIFTI ||
       dblk->brick == NULL                               ) EXRETURN ;

   dkptr = dblk->diskptr ;

   nim = nifti_image_read( dset->dblk->diskptr->brick_name, 0 ) ;
   if( nim == NULL ) EXRETURN ;

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
     EXRETURN ;
   }

   (void) nc_inq_vartype( ncid , im_varid , &im_type ) ;

   /* N.B.: we don't scale if input data is stored as floats */

   do_scale = (im_type==NC_BYTE || im_type==NC_SHORT || im_type==NC_INT) ;

   if( do_scale && AFNI_noenv("AFNI_MINC_SLICESCALE") ) do_scale = 0 ;

   code = nc_get_att_float( ncid,im_varid , "valid_range" , im_valid_range ) ;

   /** if can't get valid_range, make something up **/

   if( code != NC_NOERR || im_valid_range[0] >= im_valid_range[1] ){

      im_valid_range[0] = 0.0 ;

      switch( im_type ){
         case NC_BYTE:   im_valid_range[1] = 255.0        ; break ;
         case NC_SHORT:  im_valid_range[1] = 32767.0      ; break ;
         case NC_INT:    im_valid_range[1] = 2147483647.0 ; break ;
         case NC_FLOAT:
         case NC_DOUBLE: im_valid_range[1] = 1.0          ; break ;
      break ;
      }
   }
   inbot = im_valid_range[0] ;
   intop = im_valid_range[1] ;
   denom = intop - inbot  ;  /* always positive */

   /** Get range of image (per 2D slice) to which valid_range will be scaled **/
   /** Scaling will only be done if we get both image-min and image-max      **/

   if( do_scale ){

     code = nc_inq_varid( ncid , "image-min" , &im_min_varid ) ;
     if( code == NC_NOERR ){
       im_min = (float *) calloc(sizeof(float),nslice) ;
       code = nc_get_var_float( ncid , im_min_varid , im_min ) ;
       if( code != NC_NOERR ){ free(im_min); im_min = NULL; }
     }

     /* got im_min ==> try for im_max */

     if( im_min != NULL ){
       code = nc_inq_varid( ncid , "image-max" , &im_max_varid ) ;
       if( code == NC_NOERR ){
         im_max = (float *) calloc(sizeof(float),nslice) ;
         code = nc_get_var_float( ncid , im_max_varid , im_max ) ;
         if( code != NC_NOERR ){
           free(im_min); im_min = NULL; free(im_max); im_max = NULL;
         }
       }
     }

     /* 19 Mar 2003: make sure we don't scale out of range! */

     if( im_max != NULL && MRI_IS_INT_TYPE(DBLK_BRICK_TYPE(dblk,0)) ){
       float stop=0.0 , vbot,vtop ;
       int ii ;
       for( ii=0 ; ii < nslice ; ii++ ){
         if( im_min[ii] < im_max[ii] ){
           vbot = fabs(im_min[ii]) ; vtop = fabs(im_max[ii]) ;
           vtop = MAX(vtop,vbot) ; stop = MAX(vtop,stop) ;
         }
       }
       if( stop > MRI_TYPE_maxval[DBLK_BRICK_TYPE(dblk,0)] ){
         sfac = MRI_TYPE_maxval[DBLK_BRICK_TYPE(dblk,0)] / stop ;
         fprintf(stderr,"++ Scaling %s by %g to avoid overflow to %g of %s data!\n",
                 dkptr->brick_name, sfac, stop, MRI_TYPE_name[DBLK_BRICK_TYPE(dblk,0)] ) ;
       } else if( stop == 0.0 ){
         free(im_min); im_min = NULL; free(im_max); im_max = NULL;
       }
     }

   } /* end of do_scale */

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
