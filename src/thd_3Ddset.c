#include "mrilib.h"

/*******************************************************************/
/********** 20 Mar 2003: Read a 3D file as an AFNI dataset *********/
/*******************************************************************/

/*-----------------------------------------------------------------*/
/*! Open a NIML 3D file as an AFNI dataset.  Each column is a
    separate sub-brick.
-------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_3D( char *pathname )
{
   THD_3dim_dataset *dset=NULL ;
   char prefix[1024] , *ppp , tname[12] ;
   THD_ivec3 nxyz ;
   THD_fvec3 dxyz , orgxyz ;
   int nvals ;
   NI_element *nel ;
   NI_stream ns ;

ENTRY("THD_open_3D") ;

   /*-- read input file into NI_element --*/

   if( pathname == NULL || *pathname == '\0' ) RETURN(NULL) ;

   ppp = calloc( sizeof(char) , (strlen(pathname)+16) ) ;

   if( strstr(pathname,"http://")==NULL && strstr(pathname,"ftp://")==NULL )
     strcpy(ppp,"file:") ;
   strcat(ppp,pathname) ;
   ns = NI_stream_open( ppp , "r" ) ; free(ppp) ;
   if( ns == NULL ) RETURN(NULL) ;

   nel = NI_read_element(ns,333); NI_stream_close(ns);

   if( nel == NULL ) RETURN(NULL) ;
   if( NI_element_type(nel) != NI_ELEMENT_TYPE ){
     NI_free_element(nel) ; RETURN(NULL) ;
   }
   if( nel->vec_num <= 0 || nel->vec_len <= 0 ){
     NI_free_element(nel) ; RETURN(NULL) ;
   }
   if( strcmp(nel->name,"AFNI_3D_dataset") != 0 ){
     NI_free_element(nel) ; RETURN(NULL) ;
   }
   if( nel->vec_rank != 3 ){
     NI_free_element(nel) ; RETURN(NULL) ;
   }

   /*-- now have data element: make a dataset --*/

   dset = EDIT_empty_copy(NULL) ;

   ppp  = THD_trailname(pathname,0) ;              /* strip directory */
   NI_strncpy( prefix , ppp , THD_MAX_PREFIX ) ;   /* to make prefix */

   nxyz.ijk[0] = nel->vec_axis_len[0] ;
   nxyz.ijk[1] = nel->vec_axis_len[1] ;
   nxyz.ijk[2] = nel->vec_axis_len[2] ;

   if( nel->vec_axis_delta != NULL ){
     dxyz.xyz[0] = nel->vec_axis_delta[0] ;
     dxyz.xyz[1] = nel->vec_axis_delta[1] ;
     dxyz.xyz[2] = nel->vec_axis_delta[2] ;
   } else {
     dxyz.xyz[0] = 1.0 ;
     dxyz.xyz[1] = 1.0 ;
     dxyz.xyz[2] = 1.0 ;
   }

   if( nel->vex_axis_origin != NULL ){
     orgxyz.xyz[0] = nel->vec_axis_origin[0] ;
     orgxyz.xyz[1] = nel->vec_axis_origin[1] ;
     orgxyz.xyz[2] = nel->vec_axis_origin[2] ;
   } else {
     orgxyz.xyz[0] = 0.0 ;        /* arbitrary origin */
     orgxyz.xyz[1] = 0.0 ;
     orgxyz.xyz[2] = 0.0 ;
   }

   nvals = nel->vec_num ;           /* number of sub-bricks */

   ppp = NI_get_attribute( nel , "ni_idcode" ) ;
   if( ppp != NULL && *ppp != '\0' ){
     NI_strncpy( dset->idcode.str , ppp , MCW_IDSIZE ) ;
   } else {
     dset->idcode.str[0] = 'A' ;  /* overwrite 1st 4 bytes of IDcode */
     dset->idcode.str[1] = '3' ;
     dset->idcode.str[2] = 'D' ;
     dset->idcode.str[3] = '_' ;
   }

   EDIT_dset_items( dset ,
                      ADN_prefix      , prefix ,
                      ADN_datum_all   , MRI_float ,
                      ADN_nxyz        , nxyz ,
                      ADN_xyzdel      , dxyz ,
                      ADN_xyzorg      , orgxyz ,
                      ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                      ADN_nvals       , nvals ,
                      ADN_type        , HEAD_ANAT_TYPE ,
                      ADN_func_type   , (nvals==1) ? ANAT_MRAN_TYPE
                                                   : ANAT_BUCK_TYPE ,
                    ADN_none ) ;

#if 0
   if( nvals > 9 )              /* pretend it is 3D+time */
      EDIT_dset_items( dset ,
                         ADN_func_type, ANAT_EPI_TYPE ,
                         ADN_ntt      , nvals ,
                         ADN_ttorg    , 0.0 ,
                         ADN_ttdel    , 1.0 ,
                         ADN_ttdur    , 0.0 ,
                         ADN_tunits   , UNITS_SEC_TYPE ,
                       ADN_none ) ;
#endif

   dset->dblk->diskptr->storage_mode = STORAGE_BY_1D ;
   strcpy( dset->dblk->diskptr->brick_name , pathname ) ;

   /*-- purge image data and return the empty dataset */

   mri_free(flim) ; RETURN(dset) ;
}

/*-----------------------------------------------------------------*/
/*!  Load a 1D dataset's data into memory.
     Called from THD_load_datablock() in thd_loaddblk.c.
-------------------------------------------------------------------*/

void THD_load_3D( THD_datablock *dblk )
{
   THD_diskptr *dkptr ;
   MRI_IMAGE *flim ;
   int nxyz , nbad,iv,nv ;
   float *bar , *flar ;

ENTRY("THD_load_1D") ;

   /*-- open input [these errors should never occur] --*/

   if( !ISVALID_DATABLOCK(dblk)                     ||
       dblk->diskptr->storage_mode != STORAGE_BY_1D ||
       dblk->brick == NULL                            ) EXRETURN ;

   dkptr = dblk->diskptr ;

   flim = mri_read_1D( dkptr->brick_name ) ;
   if( flim == NULL ) EXRETURN ;

   /*-- allocate space for data --*/

   nxyz = dkptr->dimsizes[0] ;
   nv   = dkptr->nvals       ;
   if( nxyz != flim->nx || nv != flim->ny ){
     fprintf(stderr,"THD_load_1D(%s): nx or ny mismatch!\n",dkptr->brick_name) ;
     mri_free(flim) ; EXRETURN ;
   }

   dblk->malloc_type = DATABLOCK_MEM_MALLOC ;

   /** malloc space for each brick separately **/

   for( nbad=iv=0 ; iv < nv ; iv++ ){
     if( DBLK_ARRAY(dblk,iv) == NULL ){
       bar = malloc( DBLK_BRICK_BYTES(dblk,iv) ) ;
       mri_fix_data_pointer( bar ,  DBLK_BRICK(dblk,iv) ) ;
       if( bar == NULL ) nbad++ ;
     }
   }

   /** if couldn't get them all, take our ball and go home in a snit **/

   if( nbad > 0 ){
     fprintf(stderr,"\n** failed to malloc %d 1D bricks out of %d\n\a",nbad,nv) ;
     for( iv=0 ; iv < nv ; iv++ ){
       if( DBLK_ARRAY(dblk,iv) != NULL ){
         free(DBLK_ARRAY(dblk,iv)) ;
         mri_fix_data_pointer( NULL , DBLK_BRICK(dblk,iv) ) ;
       }
     }
     mri_free(flim) ; EXRETURN ;
   }

   /** copy data from image to bricks **/

   flar = MRI_FLOAT_PTR(flim) ;

   for( iv=0 ; iv < nv ; iv++ ){
     bar = DBLK_ARRAY(dblk,iv) ;
     memcpy( bar , flar + iv*nxyz , sizeof(float)*nxyz ) ;
   }

   mri_free(flim) ; EXRETURN ;
}
