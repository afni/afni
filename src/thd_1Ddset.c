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

   /*-- check if it is a NIML-ish AFNI dataset;
        if so, read it in that way instead of the 1D way [21 Mar 2003] --*/

   if( strchr(pathname,'[') == NULL ){
     char *pn = strdup(pathname) ; FILE *fp = fopen(pn,"r") ;
     if( fp == NULL ){
       char *p1 = strstr(pn,"1D") ;   /* if can't open .1D, try .3D */
       if( p1 != NULL ){
         *p1 = '3' ; fp = fopen(pn,"r") ;
       }
       if( fp == NULL ){
         fprintf(stderr,"** THD_open_1D(%s): can't open file\n",pathname);
         free(pn); RETURN(NULL);
       }
     }
     memset(prefix,0,32) ; fread(prefix,1,24,fp) ; fclose(fp) ;
     if( strstr(prefix,"<AFNI_") != NULL && strstr(prefix,"dataset") != NULL ){
       dset = THD_open_3D(pn) ;
       if( dset != NULL && strcmp(pathname,pn) != 0 )
         fprintf(stderr,"** THD_open_1D(%s): substituted %s\n",pathname,pn) ;
       free(pn) ; return dset ;
     }
   }

   /*-- otherwise, read it into an MRI_IMAGE, then mangle image into dataset --*/

   flim = mri_read_1D( pathname ) ;
   if( flim == NULL ){
     fprintf(stderr,"** Can't read 1D dataset file %s\n",pathname); RETURN(NULL);
   }

   /*-- make a dataset --*/

   dset = EDIT_empty_copy(NULL) ;        /* default (useless) dataset */

   ppp  = THD_trailname(pathname,0) ;              /* strip directory */
   MCW_strncpy( prefix , ppp , THD_MAX_PREFIX ) ;  /* to make prefix */
   ppp  = strchr( prefix , '[' ) ;
   if( ppp != NULL ) *ppp = '\0' ;

   nxyz.ijk[0] = flim->nx ; dxyz.xyz[0] = 1.0 ;    /* setup axes */
   nxyz.ijk[1] = 1        ; dxyz.xyz[1] = 1.0 ;
   nxyz.ijk[2] = 1        ; dxyz.xyz[2] = 1.0 ;

   orgxyz.xyz[0] = 0.0 ;                           /* arbitrary origin */
   orgxyz.xyz[1] = 0.0 ;
   orgxyz.xyz[2] = 0.0 ;

   nvals = flim->ny ;                              /* number of sub-bricks */

   dset->idcode.str[0] = 'A' ;  /* overwrite 1st 4 bytes of IDcode */
   dset->idcode.str[1] = '1' ;
   dset->idcode.str[2] = 'D' ;
   dset->idcode.str[3] = '_' ;

   /* note we accept default orientation (RAI) here
      (no use of ADN_xyzorient), since we actually
      don't have any spatial structure to speak of */

   EDIT_dset_items( dset ,
                      ADN_prefix      , prefix ,
                      ADN_datum_all   , MRI_float ,
                      ADN_nxyz        , nxyz ,
                      ADN_xyzdel      , dxyz ,
                      ADN_xyzorg      , orgxyz ,
                      ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                      ADN_nvals       , nvals ,
                      ADN_type        , HEAD_ANAT_TYPE ,
                      ADN_func_type   , ANAT_BUCK_TYPE ,
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

   dkptr = dblk->diskptr      ;
   nxyz  = dkptr->dimsizes[0] ;
   nv    = dkptr->nvals       ;

   if( nxyz*nv > 1000000 ) fprintf(stderr,"++ Reading %s\n",dkptr->brick_name) ;

   flim = mri_read_1D( dkptr->brick_name ) ;
   if( flim == NULL ){
     fprintf(stderr,"** THD_load_1D(%s): can't read file!\n",dkptr->brick_name) ;
     EXRETURN ;
   }

   /*-- allocate space for data --*/

   if( nxyz != flim->nx || nv != flim->ny ){
     fprintf(stderr,"** THD_load_1D(%s): nx or ny mismatch!\n",dkptr->brick_name) ;
     fprintf(stderr,"**  expect nx=%d; got nx=%d\n",nxyz,flim->nx) ;
     fprintf(stderr,"**  expect ny=%d; got ny=%d\n",nv  ,flim->ny) ;
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

   nv = DSET_NVALS(dset) ;  /* number of columns */
   nx = DSET_NX(dset)    ;
   ny = DSET_NY(dset)    ;
   nz = DSET_NZ(dset)    ; nxyz = nx*ny*nz ;  /* number of rows */

   /* make up a filename for output (into fname) */

   if( pname != NULL ){        /* have input prefix */
     if( sname != NULL ){      /* and input session (directory) */
       strcpy(fname,sname) ;
       ii = strlen(fname) ; if( fname[ii-1] != '/' ) strcat(fname,"/") ;
     } else {
       strcpy(fname,"./") ;    /* don't have input session */
     }
     strcat(fname,pname) ;
   } else {                    /* don't have input prefix */
     strcpy(fname,dset->dblk->diskptr->brick_name) ;  /* so use current name */
     cpt = strchr(fname,'[') ;
     if( cpt != NULL ) *cpt = '\0' ;                  /* without subscripts! */
   }
   ii = strlen(fname) ;
   if( ii > 10 && strstr(fname,".BRIK") != NULL ){    /* delete .BRIK! */
     fname[ii-10] = '\0' ;
     if( DSET_IS_1D(dset) || (ny==1 && nz==1) ) strcat(fname,".1D");
     else                                       strcat(fname,".3D");  /* 21 Mar 2003 */
   }

   /* open output file */

   fp = fopen( fname , "w" ) ; if( fp == NULL ) EXRETURN ;

   /* write some dataset info as NIML-style header/comments */

   fprintf(fp,
              "# <AFNI_3D_dataset\n"
              "#  ni_idcode = \"%s\"\n"
              "#  ni_type   = \"%d*float\"\n"
              "#  ni_dimen  = \"%d,%d,%d\"\n"
              "#  ni_delta  = \"%g,%g,%g\"\n"
              "#  ni_origin = \"%g,%g,%g\"\n"
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
            fprintf(fp,"%g",DSET_BRICK_STATPAR(dset,iv,jj)) ;
            if( jj < kk-1 ) fprintf(fp,",") ;
          }
          fprintf(fp,")") ;
        }
        if( ii < nv-1 ) fprintf(fp,";") ;
      }
      fprintf(fp,"\"\n") ;
   }

   /* close NIML-style header */

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
            fprintf(fp," %g",bar[ii]) ;
          }
          break ;

          case MRI_short:{
            short *bar = DSET_ARRAY(dset,iv) ;
            float fac = DSET_BRICK_FACTOR(dset,iv) ; if( fac == 0.0 ) fac = 1.0 ;
            fprintf(fp," %g",fac*bar[ii]) ;
          }
          break ;

          case MRI_byte:{
            byte *bar = DSET_ARRAY(dset,iv) ;
            float fac = DSET_BRICK_FACTOR(dset,iv) ; if( fac == 0.0 ) fac = 1.0 ;
            fprintf(fp," %g",fac*bar[ii]) ;
          }
          break ;

          /* below here, we convert complicated types to floats, losing data! */

          case MRI_complex:{
            complex *bar = DSET_ARRAY(dset,iv) ; float val ;
            val = CABS(bar[ii]) ; fprintf(fp," %g",val) ;
          }
          break ;

          case MRI_rgb:{
            rgbyte *bar = DSET_ARRAY(dset,iv) ; float val ;
            val = (0.299*bar[ii].r+0.587*bar[ii].g+0.114*bar[ii].g) ;
            fprintf(fp," %g",val) ;
          }
          break ;
       }
     }
     fprintf(fp,"\n") ;
   }

   /* NIML-style trailer */

   fprintf(fp,"# </AFNI_3D_dataset>\n") ;

   fclose(fp) ; EXRETURN ;
}
