#include "mrilib.h"

/*******************************************************************/
/********** 04 Mar 2003: Read a 1D file as an AFNI dataset *********/
/*******************************************************************/

/*-----------------------------------------------------------------*/
/*! Open a 1D file as an AFNI dataset.  Each column is a separate
    sub-brick.  Ignore comments at this time.  Data is not loaded
    into bricks.
-------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_1D( char *pathname )
{
   THD_3dim_dataset *dset=NULL ;
   MRI_IMAGE *flim ;
   char prefix[1024] , *ppp , tname[12] ;
   THD_ivec3 nxyz ;
   THD_fvec3 dxyz , orgxyz ;
   int nvals ;

ENTRY("THD_open_1D") ;

   /*-- open input file --*/

   if( pathname == NULL || pathname[0] == '\0' ) RETURN(NULL);

   /* read it now */

   flim = mri_read_1D( pathname ) ; if( flim == NULL ) RETURN(NULL) ;

   /*-- make a dataset --*/

   dset = EDIT_empty_copy(NULL) ;

   ppp  = THD_trailname(pathname,0) ;               /* strip directory */
   MCW_strncpy( prefix , ppp , THD_MAX_PREFIX ) ;   /* to make prefix */
   ppp  = strchr( prefix , '[' ) ;
   if( ppp != NULL ) *ppp = '\0' ;

   nxyz.ijk[0] = flim->nx ; dxyz.xyz[0] = 1.0 ;  /* setup axes */
   nxyz.ijk[1] = 1        ; dxyz.xyz[1] = 1.0 ;
   nxyz.ijk[2] = 1        ; dxyz.xyz[2] = 1.0 ;

   orgxyz.xyz[0] = 0.0 ;        /* arbitrary origin */
   orgxyz.xyz[1] = 0.0 ;
   orgxyz.xyz[2] = 0.0 ;

   nvals = flim->ny ;           /* number of sub-bricks */

   dset->idcode.str[0] = '1' ;  /* overwrite 1st 4 bytes of IDcode */
   dset->idcode.str[1] = 'D' ;
   dset->idcode.str[2] = '_' ;
   dset->idcode.str[3] = '_' ;

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

void THD_load_1D( THD_datablock *dblk )
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

/*------------------------------------------------------------------*/
/*! Write a dataset to disk as a 1D file.
    Called from THD_write_3dim_dataset().
--------------------------------------------------------------------*/

void THD_write_1D( char *sname, char *pname , THD_3dim_dataset *dset )
{
   char fname[THD_MAX_NAME] , *cpt ;
   int iv,nv , nx,ny,nz,nxyz,ii,jj,kk ;
   FILE *fp ;

ENTRY("THD_write_1D") ;

   if( !ISVALID_DSET(dset) || !DSET_LOADED(dset) ) EXRETURN ;

   /* make up a filename for output */

   if( pname != NULL ){
     if( sname != NULL ){
       strcpy(fname,sname) ;
       ii = strlen(fname) ; if( fname[ii-1] != '/' ) strcat(fname,"/") ;
     } else {
       strcpy(fname,"./") ;
     }
     strcat(fname,pname) ;
   } else {
     strcpy(fname,dset->dblk->diskptr->brick_name) ;
     cpt = strchr(fname,'[') ;
     if( cpt != NULL ) *cpt = '\0' ;
   }
   ii = strlen(fname) ;
   if( ii > 10 && strstr(fname,".BRIK") != NULL ){
     fname[ii-10] = '\0' ; strcat(fname,".1D") ;
   }

   fp = fopen( fname , "w" ) ; if( fp == NULL ) EXRETURN ;

   nv = DSET_NVALS(dset) ;  /* number of columns */
   nx = DSET_NX(dset)    ;
   ny = DSET_NY(dset)    ;
   nz = DSET_NZ(dset)    ; nxyz = nx*ny*nz ;  /* number of rows */

   /* write some dataset info as NIML-style header/comments */

   fprintf(fp,
              "# <AFNI_3D_dataset\n"
              "#  ni_idcode = \"%s\"\n"
              "#  ni_type   = \"%d*float\"\n"
              "#  ni_dimen  = \"%d,%d,%d\"\n"
              "#  ni_delta  = \"%f,%f,%f\"\n"
              "#  ni_origin = \"%f,%f,%f\"\n"
              "#  ni_axes   = \"%s,%s,%s\"\n"
           ,
              dset->idcode.str ,
              nv ,
              nx,ny,nz ,
              DSET_DX(dset)  , DSET_DY(dset)  , DSET_DZ(dset)  ,
              DSET_XORG(dset), DSET_YORG(dset), DSET_ZORG(dset),
              ORIENT_shortstr[dset->daxes->xxorient] ,
                ORIENT_shortstr[dset->daxes->yyorient] ,
                  ORIENT_shortstr[dset->daxes->zzorient]
          ) ;

   /* do stataux for bricks, if any are present */

   for( ii=iv=0 ; iv < nv ; iv++ )
     if( DSET_BRICK_STATCODE(dset,iv) > 0 ) ii++ ;

   if( ii > 0 ){
      fprintf(fp, "#  ni_stat   = \"") ;
      for( iv=0 ; iv < nv ; iv++ ){
        ii = DSET_BRICK_STATCODE(dset,iv) ;
        if( ii <=0 ){
          fprintf(fp,"none") ;
        } else {
          fprintf(fp,"%s(",NI_stat_distname(ii)) ;
          kk = NI_stat_numparam(ii) ;
          for( jj=0 ; jj < kk ; jj++ ){
            fprintf(fp,"%f",DSET_BRICK_STATPAR(dset,iv,jj)) ;
            if( jj < kk-1 ) fprintf(fp,",") ;
          }
          fprintf(fp,")") ;
        }
        if( ii < nv-1 ) fprintf(fp,";") ;
      }
      fprintf(fp,"\"\n") ;
   }

   /* close header */

   fprintf(fp,"# >\n") ;

   /* now write data */

   for( ii=0 ; ii < nxyz ; ii++ ){

     for( iv=0 ; iv < nv ; iv++ ){
       switch( DSET_BRICK_TYPE(dset,iv) ){
          default:
            fprintf(fp," 0.0") ;
          break ;

          case MRI_float:{
            float *bar = DSET_ARRAY(dset,iv) ;
            fprintf(fp," %f",bar[ii]) ;
          }
          break ;

          case MRI_short:{
            short *bar = DSET_ARRAY(dset,iv) ;
            float fac = DSET_BRICK_FACTOR(dset,iv) ; if( fac == 0.0 ) fac = 1.0 ;
            fprintf(fp," %f",fac*bar[ii]) ;
          }
          break ;

          case MRI_byte:{
            byte *bar = DSET_ARRAY(dset,iv) ;
            float fac = DSET_BRICK_FACTOR(dset,iv) ; if( fac == 0.0 ) fac = 1.0 ;
            fprintf(fp," %f",fac*bar[ii]) ;
          }
          break ;
       }
     }
     fprintf(fp,"\n") ;
   }

   /* trailer */

   fprintf(fp,"# </AFNI_3D_dataset>\n") ;

   fclose(fp) ; EXRETURN ;
}
