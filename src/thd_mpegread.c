#include "mrilib.h"
#include "thd.h"

/*******************************************************************/
/******** 03 Dec 2003: Read an MPEG file as an AFNI dataset ********/
/*******************************************************************/

/*-----------------------------------------------------------------*/
/*! Open an MPEG file as an unpopulated AFNI dataset.
    It will be populated in THD_load_mpeg().
-------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_mpeg( char *hname )
{
   int nim , ii , datum_type ;
   MRI_IMARR *imar ;
   MRI_IMAGE *im ;
   char *eee ;
   THD_3dim_dataset *dset=NULL ;
   char prefix[THD_MAX_PREFIX] , *ppp ;
   THD_ivec3 nxyz , orixyz ;
   THD_fvec3 dxyz , orgxyz ;
   int iview ;
   int   nx,ny,nz,nt ;
   float dx,dy,dz,dt ;


ENTRY("THD_open_mpeg") ;

   /*-- count slices in the file --*/

   imar = mri_read_mpeg( hname ) ;
   if( imar == NULL ) RETURN(NULL) ;
   nim = IMARR_COUNT(imar) ;

   /*-- get data type for each voxel --*/

   im         = IMARR_SUBIM(imar,0) ;
   datum_type = im->kind ;

   /*-- compute dimensions of images, and number of images --*/

   nx = im->nx ;
   ny = im->ny ;
   DESTROY_IMARR(imar) ;
   if( nx < 2 || ny < 2 ) RETURN(NULL) ;

   eee = getenv("AFNI_MPEG_DATASETS") ;
   if( eee == NULL ) eee = "SPACE" ;
   switch( toupper(*eee) ){
     default:
     case 'T': nz = 1   ; nt = nim ; break ;
     case 'S': nz = nim ; nt = 1   ; break ;
   }

   /*-- voxel sizes --*/

   dx = dy = dz = dt = 1.0 ;

   /*-- make a dataset --*/

   dset = EDIT_empty_copy(NULL) ;

   dset->idcode.str[0] = 'M' ;  /* overwrite 1st 3 bytes */
   dset->idcode.str[1] = 'P' ;
   dset->idcode.str[2] = 'G' ;

   ppp = THD_trailname(hname,0) ;                   /* strip directory */
   MCW_strncpy( prefix , ppp , THD_MAX_PREFIX ) ;   /* to make prefix */

   nxyz.ijk[0] = nx ; dxyz.xyz[0] = dx ;  /* setup axes lengths and voxel sizes */
   nxyz.ijk[1] = ny ; dxyz.xyz[1] = dy ;
   nxyz.ijk[2] = nz ; dxyz.xyz[2] = dz ;

   /*-- set orientation --*/

   { char *ori = "LAI" ;
     int oxx,oyy,ozz ;
     if( ori == NULL || strlen(ori) < 3 ) ori = "LAI"; /* set default LPI */

     oxx = ORCODE(ori[0]); oyy = ORCODE(ori[1]); ozz = ORCODE(ori[2]);
     if( !OR3OK(oxx,oyy,ozz) ){
       oxx = ORI_L2R_TYPE; oyy = ORI_A2P_TYPE; ozz = ORI_I2S_TYPE; /* LAI? */
     }

     orixyz.ijk[0] = oxx ;
     orixyz.ijk[1] = oyy ;
     orixyz.ijk[2] = ozz ;
   }

   /*-- origin of coordinates --*/

   orgxyz.xyz[0] = -0.5 * (nx-1) * dx ;
   orgxyz.xyz[1] = -0.5 * (ny-1) * dy ;
   orgxyz.xyz[2] = -0.5 * (nz-1) * dz ;

   iview = VIEW_ORIGINAL_TYPE ;

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

   /*-- flag to read data from disk using MPEG mode --*/

   dset->dblk->diskptr->storage_mode = STORAGE_BY_MPEG ;
   strcpy( dset->dblk->diskptr->brick_name , hname ) ;

   RETURN(dset) ;
}

/*---------------------------------------------------------------------*/
/*! Load an MPEG dataset from disk.
-----------------------------------------------------------------------*/

void THD_load_mpeg( THD_datablock *dblk )
{
   THD_diskptr *dkptr ;
   int nx,ny,nz,nv , nxy,nxyz,nxyzv , ibr,nbad , nbar ;
   FILE *fp ;
   void *ptr ;
   MRI_IMARR *imar ;
   MRI_IMAGE *im ;
   byte *bim , *bar ;

ENTRY("THD_load_mpeg") ;

   /*-- check inputs --*/

   if( !ISVALID_DATABLOCK(dblk)                       ||
       dblk->diskptr->storage_mode != STORAGE_BY_MPEG ||
       dblk->brick == NULL                              ) EXRETURN ;

   dkptr = dblk->diskptr ;

   imar = mri_read_mpeg( dkptr->brick_name ) ;
   if( imar == NULL ) EXRETURN ;

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
     fprintf(stderr,
             "\n** failed to malloc %d MPEG bricks out of %d\n\a",nbad,nv);
     for( ibr=0 ; ibr < nv ; ibr++ ){
       if( DBLK_ARRAY(dblk,ibr) != NULL ){
         free(DBLK_ARRAY(dblk,ibr)) ;
         mri_fix_data_pointer( NULL , DBLK_BRICK(dblk,ibr) ) ;
       }
     }
     fclose(fp) ; DESTROY_IMARR(imar) ; EXRETURN ;
   }

   /*-- load data from image array into sub-brick arrays! --*/

   nbar = mri_datum_size( DBLK_BRICK_TYPE(dblk,0) ) * nx*ny ;
   if( nv == 1 ){                   /* all data goes into 1 sub-brick */
     bar = DBLK_ARRAY(dblk,0) ;
     for( ibr=0 ; ibr < nz ; ibr++ ){
       im = IMARR_SUBIM(imar,ibr) ; bim = mri_data_pointer(im) ;
       memcpy( bar , bim , nbar ) ;
       bar += nbar ;
     }
   } else {                         /* each slice is a separate sub-brick */
     for( ibr=0 ; ibr < nv ; ibr++ ){
       bar = DBLK_ARRAY(dblk,ibr) ;
       im = IMARR_SUBIM(imar,ibr) ; bim = mri_data_pointer(im) ;
       memcpy( bar , bim , nbar ) ;
     }
   }

   DESTROY_IMARR(imar) ;
   EXRETURN ;
}
