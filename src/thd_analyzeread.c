#include "mrilib.h"
#include "mayo_analyze.h"
#include "thd.h"

/*******************************************************************/
/******* 27 Aug 2002: Read an ANALYZE file as an AFNI dataset ******/
/*******************************************************************/

/*---------------------------------------------------------------*/
/*! Swap the 2 bytes pointed to by ppp: ab -> ba. */

static void swap_2(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1;

   b0 = *pntr; b1 = *(pntr+1);
   *pntr = b1; *(pntr+1) = b0;
}

/*---------------------------------------------------------------*/
/*! Swap the 4 bytes pointed to by ppp: abcd -> dcba. */

static void swap_4(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1, b2, b3;

   b0 = *pntr; b1 = *(pntr+1); b2 = *(pntr+2); b3 = *(pntr+3);
   *pntr = b3; *(pntr+1) = b2; *(pntr+2) = b1; *(pntr+3) = b0;
}

#if 0
/*---------------------------------------------------------------*/
/*! Swap the 8 bytes pointed to by ppp: abcdefgh -> hgfedcba. */

static void swap_8(void *ppp)
{
   unsigned char *pntr = (unsigned char *) ppp ;
   unsigned char b0, b1, b2, b3;
   unsigned char b4, b5, b6, b7;

   b0 = *pntr    ; b1 = *(pntr+1); b2 = *(pntr+2); b3 = *(pntr+3);
   b4 = *(pntr+4); b5 = *(pntr+5); b6 = *(pntr+6); b7 = *(pntr+7);

   *pntr     = b7; *(pntr+1) = b6; *(pntr+2) = b5; *(pntr+3) = b4;
   *(pntr+4) = b3; *(pntr+5) = b2; *(pntr+6) = b1; *(pntr+7) = b0;
}
#endif

/*---------------------------------------------------------------*/
/*! Byte swap ANALYZE file header in various places */

static void swap_analyze_hdr( struct dsr *pntr )
{
   swap_4(&pntr->hk.sizeof_hdr) ;
   swap_4(&pntr->hk.extents) ;
   swap_2(&pntr->hk.session_error) ;
   swap_2(&pntr->dime.dim[0]) ;
   swap_2(&pntr->dime.dim[1]) ;
   swap_2(&pntr->dime.dim[2]) ;
   swap_2(&pntr->dime.dim[3]) ;
   swap_2(&pntr->dime.dim[4]) ;
   swap_2(&pntr->dime.dim[5]) ;
   swap_2(&pntr->dime.dim[6]) ;
   swap_2(&pntr->dime.dim[7]) ;
#if 0
   swap_2(&pntr->dime.unused1) ;
#endif
   swap_2(&pntr->dime.datatype) ;
   swap_2(&pntr->dime.bitpix) ;
   swap_4(&pntr->dime.pixdim[0]) ;
   swap_4(&pntr->dime.pixdim[1]) ;
   swap_4(&pntr->dime.pixdim[2]) ;
   swap_4(&pntr->dime.pixdim[3]) ;
   swap_4(&pntr->dime.pixdim[4]) ;
   swap_4(&pntr->dime.pixdim[5]) ;
   swap_4(&pntr->dime.pixdim[6]) ;
   swap_4(&pntr->dime.pixdim[7]) ;
   swap_4(&pntr->dime.vox_offset) ;
   swap_4(&pntr->dime.funused1) ;
   swap_4(&pntr->dime.funused2) ;
   swap_4(&pntr->dime.cal_max) ;
   swap_4(&pntr->dime.cal_min) ;
   swap_4(&pntr->dime.compressed) ;
   swap_4(&pntr->dime.verified) ;
   swap_2(&pntr->dime.dim_un0) ;
   swap_4(&pntr->dime.glmax) ;
   swap_4(&pntr->dime.glmin) ;
}

/*-----------------------------------------------------------------*/
#ifndef DONT_INCLUDE_ANALYZE_STRUCT
#define DONT_INCLUDE_ANALYZE_STRUCT
#endif
#include "nifti1_io.h"                 /*** NIFTI spec and funcs ***/
/*-----------------------------------------------------------------*/

#undef swap_2
#undef swap_4

/*-----------------------------------------------------------------*/
/*! Open an ANALYZE .hdr file as an unpopulated AFNI dataset.
    It will be populated from the .img file, in THD_load_analyze().
-------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_analyze( char *hname )
{
   FILE * fp ;
   char iname[THD_MAX_NAME] ;
   int ii , doswap ;
   struct dsr hdr ;    /* ANALYZE .hdr format */
   int ngood , length , datum_type , datum_len ;
   int   nx,ny,nz,nt ;
   float dx,dy,dz,dt ;
   float fac=0.0 ;     /* brick scaling factor */

   THD_3dim_dataset *dset=NULL ;
   char prefix[THD_MAX_PREFIX] , *ppp ;
   THD_ivec3 nxyz , orixyz ;
   THD_fvec3 dxyz , orgxyz ;
   int iview ;
   short spmorg , spmxx,spmyy,spmzz ;  /* 03 Nov 2003 */

ENTRY("THD_open_analyze") ;

   /* 28 Aug 2003: check if this is a NIFTI file instead */
   { nifti_image *nim = nifti_image_read(hname,0) ;
     if( nim != NULL && nim->nifti_type > 0 ){
       nifti_image_free(nim); dset = THD_open_nifti(hname); RETURN(dset);
     }
   }

   /*-- check & prepare filenames --*/

   if( hname == NULL ) RETURN( NULL );
   ii = strlen(hname) ; if( ii < 5 ) RETURN( NULL );
   if( strcmp(hname+ii-4,".hdr") != 0 ) RETURN( NULL );
   strcpy(iname,hname) ; strcpy(iname+ii-3,"img") ;  /* .img filename */

   /*-- read .hdr file into struct --*/

   fp = fopen( hname , "rb" ) ;
   if( fp == NULL ) RETURN( NULL );
   hdr.dime.dim[0] = 0 ;
   fread( &hdr , 1 , sizeof(struct dsr) , fp ) ;
   fclose(fp) ;
   if( hdr.dime.dim[0] == 0 ){    /* bad input */
     ERROR_message("ANALYZE file %s has dim[0]=0!\n",hname) ;
     RETURN( NULL ) ;
   }

   /*-- check .img file for existence and size --*/

   length = THD_filesize(iname) ;  /* will use this later */
   if( length <= 0 ){
     ERROR_message("Can't find ANALYZE file %s\n",iname) ;
     RETURN( NULL );
   }

   /*-- check for swap-age of header --*/

   doswap = (hdr.dime.dim[0] < 0 || hdr.dime.dim[0] > 15) ;
   if( doswap ) swap_analyze_hdr( &hdr ) ;

   /*-- get a scale factor for images --*/

   if( !AFNI_noenv("AFNI_ANALYZE_SCALE") ){
     fac = hdr.dime.funused1 ;
     (void) thd_floatscan( 1 , &fac ) ;
     if( fac < 0.0 || fac == 1.0 ) fac = 0.0 ;
   }

   /*-- get data type for each voxel --*/

   switch( hdr.dime.datatype ){
     default:
        ERROR_message("File %s: Unsupported ANALYZE datatype=%d (%s)\n",
                hname,hdr.dime.datatype,ANDT_string(hdr.dime.datatype) ) ;
     RETURN( NULL );

     case ANDT_UNSIGNED_CHAR: datum_type = MRI_byte   ; break;
     case ANDT_SIGNED_SHORT:  datum_type = MRI_short  ; break;
     case ANDT_FLOAT:         datum_type = MRI_float  ; break;
     case ANDT_COMPLEX:       datum_type = MRI_complex; break;
     case ANDT_RGB:           datum_type = MRI_rgb    ; break;
#if 0
     case ANDT_SIGNED_INT:    datum_type = MRI_int    ; break; /* not in AFNI */
#endif
   }

   datum_len = mri_datum_size(datum_type) ;  /* number bytes per voxel */

   /*-- compute dimensions of images, and number of images --*/

   nx = hdr.dime.dim[1] ;
   ny = hdr.dime.dim[2] ;
   if( nx < 2 || ny < 2 ) RETURN( NULL );

   switch( hdr.dime.dim[0] ){
     case 2:  nz = 1 ; nt = 1 ;                           ; break ;
     case 3:  nz = hdr.dime.dim[3] ; nt = 1 ;             ; break ;
     case 4:  nz = hdr.dime.dim[3] ; nt = hdr.dime.dim[4] ; break ;

     default:
       ERROR_message("ANALYZE file %s has %d dimensions!\n",
                     hname,hdr.dime.dim[0]) ;
       RETURN( NULL ) ;
   }
   if( nz < 1 ) nz = 1 ;
   if( nt < 1 ) nt = 1 ;

   /*-- voxel sizes --*/

   dx = fabs(hdr.dime.pixdim[1]) ; if( dx == 0.0 )            dx = 1.0 ;
   dy = fabs(hdr.dime.pixdim[2]) ; if( dy == 0.0 )            dy = 1.0 ;
   dz = fabs(hdr.dime.pixdim[3]) ; if( dz == 0.0 )            dz = 1.0 ;
   dt = fabs(hdr.dime.pixdim[4]) ; if( dt == 0.0 || nt == 1 ) dt = 1.0 ;

   ngood = datum_len*nx*ny*nz*nt ;  /* number bytes needed in .img file */
   if( length < ngood ){
     ERROR_message(
       "ANALYZE file %s is %d bytes long but must be %d bytes long\n"
       "**      for nx=%d ny=%d nz=%d nt=%d and %d bytes/voxel\n",
       iname,length,ngood,nx,ny,nz,nt,datum_len ) ;
     RETURN( NULL );  /* file is already closed    30 Nov 2009 [rickr] */
   }

   /* check SPM originator field - 03 Nov 2003 */
   /* 11 Mar 2004 - oops: SPM indexes start at 1 - RWCox */

   spmorg = spmxx = spmyy = spmzz = 0 ;
   { short xyzuv[5] , xx,yy,zz,aa,bb ;
     memcpy( xyzuv , hdr.hist.originator , 10 ) ;
     if( doswap ){ swap_2(&(xyzuv[0])); swap_2(&(xyzuv[1])); swap_2(&(xyzuv[2])); swap_2(&(xyzuv[3])); swap_2(&(xyzuv[4]));}
     /*fprintf(stderr,  "\n"
                        "hdr.hist.originator:   %d %d %d %d %d\n",
                        xyzuv[0], xyzuv[1], xyzuv[2], xyzuv[3], xyzuv[4]
                        );*/
     if( 1 || (xyzuv[3] == 0 && xyzuv[4] == 0) ){ /* ZSS Nov 10 05. Looks like xyzuv[3] and xyzuv[4] can be complete rubbish. Not reliable. */
       xx = xyzuv[0] ; yy = xyzuv[1] ; zz = xyzuv[2] ;
       if( xx > 0 && xx < nx-1 &&
           yy > 0 && yy < ny-1 &&
           zz > 0 && zz < nz-1   ){ spmorg=1; spmxx=xx-1; spmyy=yy-1; spmzz=zz-1; }
       /*fprintf(stderr,  "\n"
                        "        xx      yy       zz     :   %d %d %d \n"
                        "spmorg  spmxx   spmyy    spmzz  : %d %d %d %d\n", 
                                                xx, yy, zz,
                                                spmorg, spmxx, spmyy, spmzz);*/
     }
   }

   /*-- make a dataset --*/

   dset = EDIT_empty_copy(NULL) ;

   dset->idcode.str[0] = 'A' ;  /* overwrite 1st 3 bytes */
   dset->idcode.str[1] = 'N' ;
   dset->idcode.str[2] = 'Z' ;

   MCW_hash_idcode( hname , dset ) ;  /* 06 May 2005 */

   ppp = THD_trailname(hname,0) ;                   /* strip directory */
   MCW_strncpy( prefix , ppp , THD_MAX_PREFIX ) ;   /* to make prefix */

   nxyz.ijk[0] = nx ; dxyz.xyz[0] = dx ;  /* setup axes lengths and voxel sizes */
   nxyz.ijk[1] = ny ; dxyz.xyz[1] = dy ;
   nxyz.ijk[2] = nz ; dxyz.xyz[2] = dz ;

   /*-- default orientation is LPI, but user can alter via environment --*/

   { char *ori = getenv("AFNI_ANALYZE_ORIENT") ;
     int oxx,oyy,ozz ;
     if( ori == NULL || strlen(ori) < 3 ) {
      static int nwarn=0 ;
      ori = "LPI"; /* set default LPI */
      if( nwarn < 2 ){
        WARNING_message("Assuming ANALYZE orientaion is LPI.\n"
                        "++    To change orientation or silence this message,\n"
                        "++    Set AFNI_ANALYZE_ORIENT to the proper orientation\n"
                        "++    in your .afnirc file.\n"
                        "++      e.g.: AFNI_ANALYZE_ORIENT = LPI");
        nwarn++ ;
      }
     }
     oxx = ORCODE(ori[0]); oyy = ORCODE(ori[1]); ozz = ORCODE(ori[2]);
     if( !OR3OK(oxx,oyy,ozz) ){
       oxx = ORI_L2R_TYPE; oyy = ORI_P2A_TYPE; ozz = ORI_I2S_TYPE; /* LPI? */
     }

     orixyz.ijk[0] = oxx ;
     orixyz.ijk[1] = oyy ;
     orixyz.ijk[2] = ozz ;
   }

   /*-- origin of coordinates --*/

   orgxyz.xyz[0] = 0.5*dx ;  /* FSL:  (0,0,0) is at outer corner of 1st voxel */
   orgxyz.xyz[1] = 0.5*dy ;  /* AFNI: these are coords of center of 1st voxel */
   orgxyz.xyz[2] = 0.5*dz ;

   /*-- 04 Oct 2002: allow auto-centering of ANALYZE datasets --*/

   if( AFNI_yesenv("AFNI_ANALYZE_AUTOCENTER") ){
     static int nwarn=0 ;
     if( nwarn < 2 )
       WARNING_message("Autocentering datasets because"
                       " AFNI_ANALYZE_AUTOCENTER is set");
     orgxyz.xyz[0] = -0.5 * (nx-1) * dx ;
     orgxyz.xyz[1] = -0.5 * (ny-1) * dy ;
     orgxyz.xyz[2] = -0.5 * (nz-1) * dz ; nwarn++ ;
   }

   
   iview = VIEW_ORIGINAL_TYPE ;   /* can't tell if it is Talairach-ed (default)*/
   {/* ZSS Dec 15 03 */
      char *vie = getenv("AFNI_ANALYZE_VIEW") ;
      if (!vie) {
        static int nwarn = 0 ;
        iview = VIEW_ORIGINAL_TYPE; 
        if( nwarn < 2 ){
          WARNING_message("Assuming view is +orig.\n"
                          "++    To change view or silence this message,\n"
                          "++    Set AFNI_ANALYZE_VIEW to the proper view\n"
                          "++    in your .afnirc file.\n"
                          "++      e.g.: AFNI_ANALYZE_VIEW = orig");
          nwarn++ ;
        }
      } else {
         static int nwarn = 0 ;
              if (strcmp(vie, "tlrc") == 0) iview = VIEW_TALAIRACH_TYPE; 
         else if (strcmp(vie, "orig") == 0) iview = VIEW_ORIGINAL_TYPE;
         else if (strcmp(vie, "acpc") == 0) iview = VIEW_ACPCALIGNED_TYPE;
         else if( nwarn < 2 ) {
            WARNING_message("Bad value (%s) for environment \n"
                            "++    variable AFNI_ANALYZE_VIEW. Choose from:\n"
                            "++      orig or acpc or tlrc.\n"
                            "++    Assuming orig view.\n", vie);
            iview = VIEW_ORIGINAL_TYPE;   nwarn++ ;
         }
      }
   }
   
   if( AFNI_yesenv("AFNI_ANALYZE_ORIGINATOR") && spmorg ){  /* 03 Nov 2003 */
     if ( !getenv ("AFNI_ANALYZE_VIEW") ) { /* ZSS 16 Dec 2003 */
       iview = VIEW_TALAIRACH_TYPE ;        /* for backward compatibility */
     } 
     orgxyz.xyz[0] = -spmxx * dx ; /* (0,0,0) is at (spmxx,spmyy,spmzz) */
     orgxyz.xyz[1] = -spmyy * dy ;
     orgxyz.xyz[2] = -spmzz * dz ;
   } else {
     if (!spmorg){
       static int nwarn=0 ;
       if( nwarn < 2 )
         WARNING_message("No ANALYZE origin found in file %s\n",hname);
       nwarn++ ;
     } else {
       static int nwarn=0 ;
       if( nwarn < 2 )
         WARNING_message("ANALYZE origin ignored in file %s\n"
                         "++    If datasets are out of alignment,\n"
                         "++    Set AFNI_ANALYZE_ORIGINATOR = YES\n"
                         "++    in your .afnirc file.\n" , hname );
       nwarn++ ;
     }
   }
#if 0
   fprintf (stderr,   "\n"
                      "orgxyz.xyz: %f %f %f\n",
                       orgxyz.xyz[0], orgxyz.xyz[1], orgxyz.xyz[2]);
#endif

   /* 10 Oct 2002: change voxel size signs, if axis orientation is negative */
   /*              [above, we assumed that axes were oriented in - to + way] */

   if( ORIENT_sign[orixyz.ijk[0]] == '-' ){
     dxyz.xyz[0]   = -dxyz.xyz[0]   ;
     orgxyz.xyz[0] = -orgxyz.xyz[0] ;
   }

   if( ORIENT_sign[orixyz.ijk[1]] == '-' ){
     dxyz.xyz[1]   = -dxyz.xyz[1]   ;
     orgxyz.xyz[1] = -orgxyz.xyz[1] ;
   }

   if( ORIENT_sign[orixyz.ijk[2]] == '-' ){
     dxyz.xyz[2]   = -dxyz.xyz[2]   ;
     orgxyz.xyz[2] = -orgxyz.xyz[2] ;
   }

   /*-- actually send the values above into the dataset header --*/

   EDIT_dset_items( dset ,
                      ADN_prefix      , prefix ,
                      ADN_datum_all   , datum_type ,
                      ADN_nxyz        , nxyz ,
                      ADN_xyzdel      , dxyz ,
                      ADN_xyzorg      , orgxyz ,
                      ADN_xyzorient   , orixyz ,
                      ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                      ADN_nvals       , nt ,
                      ADN_type        , HEAD_ANAT_TYPE ,
                      ADN_view_type   , iview ,
                      ADN_func_type   , ANAT_MRAN_TYPE ,
                    ADN_none ) ;

   if( nt > 1 )              /** pretend it is 3D+time **/
      EDIT_dset_items( dset ,
                         ADN_func_type, ANAT_EPI_TYPE ,
                         ADN_ntt      , nt ,
                         ADN_ttorg    , 0.0 ,
                         ADN_ttdel    , dt ,
                         ADN_ttdur    , 0.0 ,
                         ADN_tunits   , UNITS_SEC_TYPE ,
                       ADN_none ) ;

   /* make it a func if is is a FSL special name */

#ifdef ALLOW_FSL_FEAT
   if( strstr(prefix,"stat") != NULL )
     EDIT_dset_items( dset ,
                        ADN_type      , HEAD_FUNC_TYPE ,
                        ADN_func_type , FUNC_FIM_TYPE ,
                      ADN_none ) ;
#endif

   /*-- set brick factors (all the same) --*/

   if( fac > 0.0 ){
     float *bf = AFMALL(float, sizeof(float)*nt) ;
     for( ii=0 ; ii < nt ; ii++ ) bf[ii] = fac ;
     EDIT_dset_items( dset , ADN_brick_fac,bf , ADN_none ) ;
     free(bf) ;
   }

   /*-- set byte order (for reading from disk) --*/

   ii = mri_short_order() ;
   if( doswap ) ii = REVERSE_ORDER(ii) ;
   dset->dblk->diskptr->byte_order = ii ;

   /*-- flag to read data from disk using ANALYZE mode --*/

   dset->dblk->diskptr->storage_mode = STORAGE_BY_ANALYZE ;
   strcpy( dset->dblk->diskptr->brick_name , iname ) ;

   RETURN(dset) ;
}

/*---------------------------------------------------------------------*/
/*! Load an ANALYZE dataset from disk.
-----------------------------------------------------------------------*/

void THD_load_analyze( THD_datablock *dblk )
{
   THD_diskptr *dkptr ;
   int nx,ny,nz,nv , nxy,nxyz,nxyzv , ibr,nbad ;
   FILE *fp ;
   void *ptr ;

ENTRY("THD_load_analyze") ;

   /*-- check inputs --*/

   if( !ISVALID_DATABLOCK(dblk)                          ||
       dblk->diskptr->storage_mode != STORAGE_BY_ANALYZE ||
       dblk->brick == NULL                                 ) EXRETURN ;

   dkptr = dblk->diskptr ;

   fp = fopen( dkptr->brick_name , "rb" ) ;  /* .img file */
   if( fp == NULL ) EXRETURN ;

   /*-- allocate space for data --*/

   nx = dkptr->dimsizes[0] ;
   ny = dkptr->dimsizes[1] ;  nxy   = nx * ny   ;
   nz = dkptr->dimsizes[2] ;  nxyz  = nxy * nz  ;
   nv = dkptr->nvals       ;  nxyzv = nxyz * nv ;

   dblk->malloc_type = DATABLOCK_MEM_MALLOC ;

   /*-- malloc space for each brick separately --*/

   for( nbad=ibr=0 ; ibr < nv ; ibr++ ){
      if( DBLK_ARRAY(dblk,ibr) == NULL ){
         ptr = AFMALL(void, DBLK_BRICK_BYTES(dblk,ibr) ) ;
         mri_fix_data_pointer( ptr ,  DBLK_BRICK(dblk,ibr) ) ;
         if( ptr == NULL ) nbad++ ;
      }
   }

   /*-- if couldn't get them all, take our ball and go home in a snit --*/

   if( nbad > 0 ){
      ERROR_message("failed to malloc %d ANALYZE bricks out of %d\n\a",nbad,nv);
      for( ibr=0 ; ibr < nv ; ibr++ ){
        if( DBLK_ARRAY(dblk,ibr) != NULL ){
           free(DBLK_ARRAY(dblk,ibr)) ;
           mri_fix_data_pointer( NULL , DBLK_BRICK(dblk,ibr) ) ;
        }
      }
      fclose(fp) ; EXRETURN ;
   }

   /*-- read data from .img file into sub-brick arrays! --*/

   for( ibr=0 ; ibr < nv ; ibr++ )
     fread( DBLK_ARRAY(dblk,ibr), 1, DBLK_BRICK_BYTES(dblk,ibr), fp ) ;

   fclose(fp) ;

   /*-- swap bytes? --*/

   if( dkptr->byte_order != mri_short_order() ){
     for( ibr=0 ; ibr < nv ; ibr++ ){
       switch( DBLK_BRICK_TYPE(dblk,ibr) ){
         case MRI_short:
           mri_swap2( DBLK_BRICK_NVOX(dblk,ibr) , DBLK_ARRAY(dblk,ibr) ) ;
         break ;

         case MRI_complex:
           mri_swap4( 2*DBLK_BRICK_NVOX(dblk,ibr), DBLK_ARRAY(dblk,ibr)) ;
         break ;

         case MRI_float:
         case MRI_int:
           mri_swap4( DBLK_BRICK_NVOX(dblk,ibr) , DBLK_ARRAY(dblk,ibr) ) ;
         break ;
       }
     }
   }

   /*-- check floats --*/

   for( ibr=0 ; ibr < nv ; ibr++ ){
     if( DBLK_BRICK_TYPE(dblk,ibr) == MRI_float ){
       nbad += thd_floatscan( DBLK_BRICK_NVOX(dblk,ibr) ,
                              DBLK_ARRAY(dblk,ibr)        ) ;

     } else if( DBLK_BRICK_TYPE(dblk,ibr) == MRI_complex ) {
       nbad += thd_complexscan( DBLK_BRICK_NVOX(dblk,ibr) ,
                                DBLK_ARRAY(dblk,ibr)        ) ;
     }
   }
   if( nbad > 0 )
     ERROR_message("File %s: found %d float errors -- see program float_scan\n" ,
                   dkptr->brick_name , nbad ) ;

   EXRETURN ;
}
