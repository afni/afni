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
   char prefix[1024] , *ppp , tname[12] , *pn ;
   THD_ivec3 nxyz ;
   THD_fvec3 dxyz , orgxyz ;
   int nvals , lpn , flip=0 ;
   FILE *fp ;

ENTRY("THD_open_1D") ;

   /*-- open input file --*/

   if( pathname == NULL || pathname[0] == '\0' ) RETURN(NULL);

   /*-- check if it is a NIML-ish AFNI dataset;
        if so, read it in that way instead of the 1D way [21 Mar 2003] --*/

   if( strchr(pathname,'[') == NULL && strncmp(pathname,"1D:",3) != 0 &&
       strchr(pathname,'{') == NULL && strchr(pathname,'\'')     == NULL ){

     pn = strdup(pathname) ; fp = fopen(pn,"r") ;

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
     memset(prefix,0,133) ; fread(prefix,1,128,fp) ; fclose(fp) ;
     if( strstr(prefix,"<AFNI_") != NULL && strstr(prefix,"dataset") != NULL ){
       dset = THD_open_3D(pn) ;
       if( dset != NULL && strcmp(pathname,pn) != 0 )
         fprintf(stderr,"** THD_open_1D(%s): substituted %s\n",pathname,pn) ;
       free(pn) ; return dset ;
     }
   }

   /*-- otherwise, read it into an MRI_IMAGE, then mangle image into dataset --*/

   lpn = strlen(pathname) ; pn = strdup(pathname) ;

#if 0
   flip = (pn[lpn-1] == '\'') ;     /* 12 Jul 2005: allow for tranposing input */
   if( flip ) pn[lpn-1] = '\0' ;
#endif

   flim = mri_read_1D(pn) ;
   if( flim == NULL ){
     fprintf(stderr,"** Can't read 1D dataset file %s\n",pn); free(pn); RETURN(NULL);
   }
   if( flip ){
     MRI_IMAGE *qim = mri_transpose(flim); mri_free(flim); flim = qim;
   }

   /*-- make a dataset --*/

   dset = EDIT_empty_copy(NULL) ;        /* default (useless) dataset */

   ppp  = THD_trailname(pn,0) ;                   /* strip directory */
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

   MCW_hash_idcode( pathname , dset ) ; /* 06 May 2005 */
   dset->idcode.str[0] = 'A' ;          /* overwrite 1st 3 bytes of IDcode */
   dset->idcode.str[1] = '1' ;
   dset->idcode.str[2] = 'D' ;

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

   if( nvals > 1 && AFNI_yesenv("AFNI_1D_TIME") ){ /* pretend it is 3D+time */
     char *eee = getenv("AFNI_1D_TIME_TR") ;
     float dt = 0.0 ;
     if( eee != NULL ) dt = strtod(eee,NULL) ;
     if( dt <= 0.0   ) dt = 1.0 ;
     EDIT_dset_items( dset ,
                        ADN_func_type, ANAT_EPI_TYPE ,
                        ADN_ntt      , nvals ,
                        ADN_ttorg    , 0.0 ,
                        ADN_ttdel    , dt ,    /* TR */
                        ADN_ttdur    , 0.0 ,
                        ADN_tunits   , UNITS_SEC_TYPE ,
                      ADN_none ) ;
   }

   dset->dblk->diskptr->storage_mode = STORAGE_BY_1D ;
   strcpy( dset->dblk->diskptr->brick_name , pathname ) ;

   /*-- purge image data and return the empty dataset */

   mri_free(flim) ; free(pn) ; RETURN(dset) ;
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
   char *pn ; int lpn , flip=0 ;

ENTRY("THD_load_1D") ;

   /*-- open input [these errors should never occur] --*/

   if( !ISVALID_DATABLOCK(dblk)                     ||
       dblk->diskptr->storage_mode != STORAGE_BY_1D ||
       dblk->brick == NULL                            ) EXRETURN ;

   dkptr = dblk->diskptr      ;
   nxyz  = dkptr->dimsizes[0] ;
   nv    = dkptr->nvals       ;

   if( nxyz*nv > 1000000 ) fprintf(stderr,"++ Reading %s\n",dkptr->brick_name) ;

   pn = strdup(dkptr->brick_name) ; lpn = strlen(pn) ;
   flip = (pn[lpn-1] == '\'') ;     /* 12 Jul 2005: allow for tranposing input */
   if( flip ) pn[lpn-1] = '\0' ;

   flim = mri_read_1D(pn) ; free(pn) ;

   if( flim == NULL ){
     fprintf(stderr,"** THD_load_1D(%s): can't read file!\n",dkptr->brick_name) ;
     EXRETURN ;
   }
   if( flip ){
     MRI_IMAGE *qim = mri_transpose(flim); mri_free(flim); flim = qim;
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
       bar = AFMALL( float,  DBLK_BRICK_BYTES(dblk,iv) ) ;
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
   int iv,nv , nx,ny,nz,nxyz,ii,jj,kk , qq ;
   FILE *fp=NULL ;
   int binflag=0 ; char shp ; float val[1] ;

ENTRY("THD_write_1D") ;

   if( !ISVALID_DSET(dset) || !DSET_LOADED(dset) ) EXRETURN ;

   nv = DSET_NVALS(dset) ;  /* number of columns */
   nx = DSET_NX(dset)    ;
   ny = DSET_NY(dset)    ;
   nz = DSET_NZ(dset)    ; nxyz = nx*ny*nz ;  /* number of rows */

   /* make up a filename for output (into fname) */

   cpt = DSET_PREFIX(dset) ;
   if( (pname != NULL && *pname == '-') ||          /* 12 Jul 2005: to stdout */
       (pname == NULL && (*cpt   == '-' || strncmp(cpt,"stdout",6)==0)) ){
     fp = stdout ; strcpy(fname,"stdout") ; binflag = 0 ;
   }

   /* 05 Mar 2008: special case to write as a 'pure' .1D file,
                   if filename ends in '.1D' or specifies stdout */

   if( pname == NULL && AFNI_yesenv("AFNI_1D_TRANOUT") &&
       (STRING_HAS_SUFFIX(cpt,".1D") || *cpt=='-' || strncmp(cpt,"stdout",6)==0) ){

     MRI_IMAGE *qim = THD_dset_to_1Dmri(dset) ;
     mri_write_1D(cpt,qim); mri_free(qim); EXRETURN ;
   }

   /* back to normal 3D mode of writing */

   if( sname == NULL ) sname = DSET_DIRNAME(dset) ;

   if( fp == NULL ){
     if( pname != NULL ){                       /* have input prefix */
       if( sname != NULL && *sname != '\0' ){   /* and input session (directory) */
         strcpy(fname,sname) ;
         ii = strlen(fname) ; if( fname[ii-1] != '/' ) strcat(fname,"/") ;
       } else {
         strcpy(fname,"./") ;    /* don't have input session */
       }
       strcat(fname,pname) ;
     } else {                    /* don't have input prefix */
       if( sname != NULL && *sname != '\0' ){   /* use directory name -- 25 May 2009 */
         strcpy(fname,sname) ;
         ii = strlen(fname) ; if( fname[ii-1] != '/' ) strcat(fname,"/") ;
       } else {
         strcpy(fname,"./") ;    /* don't have input session */
       }
       cpt = DSET_PREFIX(dset) ;
       if( STRING_HAS_SUFFIX(cpt,".3D") || STRING_HAS_SUFFIX(cpt,".1D") )
         strcat(fname,cpt) ;
       else
         strcpy(fname,DSET_BRIKNAME(dset)) ;

       cpt = strchr(fname,'[') ;
       if( cpt != NULL ) *cpt = '\0' ;                  /* without subscripts! */
     }
     ii = strlen(fname) ;
     if( ii > 10 && strstr(fname,".BRIK") != NULL ){    /* delete +view.BRIK */
       fname[ii-10] = '\0' ;
       if( DSET_IS_1D(dset) || (ny==1 && nz==1) )
         strcat(fname,".1D");
       else
         strcat(fname,".3D");                           /* 21 Mar 2003 */
     }

     fp = fopen( fname , "w" ) ; if( fp == NULL ) EXRETURN ;
     binflag = STRING_HAS_SUFFIX(fname,".3D") && AFNI_yesenv("AFNI_3D_BINARY") ;
   }

   strcpy( dset->dblk->diskptr->brick_name , fname ); /* 12 Jul 2005 */

   /* are we going to write in binary? [03 Jun 2005] */

   shp = (binflag) ? ' ' : '#' ;

   /* write some dataset info as NIML-style header/comments */

   if( fp != stdout )
     fprintf(fp,
                "%c <AFNI_3D_dataset\n"
                "%c  self_idcode = \"%s\"\n"
                "%c  ni_type     = \"%d*float\"\n"    /* all columns are floats! */
                "%c  ni_dimen    = \"%d,%d,%d\"\n"
                "%c  ni_delta    = \"%g,%g,%g\"\n"
                "%c  ni_origin   = \"%g,%g,%g\"\n"
                "%c  ni_axes     = \"%s,%s,%s\"\n"
             ,
                shp ,
                shp , dset->idcode.str ,
                shp , nv ,
                shp , nx,ny,nz ,
                shp , DSET_DX(dset)  , DSET_DY(dset)  , DSET_DZ(dset)  ,
                shp , DSET_XORG(dset), DSET_YORG(dset), DSET_ZORG(dset),
                shp , ORIENT_shortstr[dset->daxes->xxorient] ,
                       ORIENT_shortstr[dset->daxes->yyorient] ,
                         ORIENT_shortstr[dset->daxes->zzorient]
            ) ;

   if( fp != stdout && HAS_TIMEAXIS(dset) ){
     float dt = DSET_TR(dset) ;
     if( DSET_TIMEUNITS(dset) == UNITS_MSEC_TYPE ) dt *= 0.001 ;
     fprintf(fp , "%c  ni_timestep = \"%g\"\n" , shp,dt ) ;
   }

   if( fp != stdout && binflag )
     fprintf(fp , "   ni_form     = \"binary.%s\"\n" ,
                  (NI_byteorder()==NI_LSB_FIRST) ? "lsbfirst" : "msbfirst" ) ;

   /* do stataux for bricks, if any are present */

   for( ii=iv=0 ; iv < nv ; iv++ )
     if( DSET_BRICK_STATCODE(dset,iv) > 0 ) ii++ ;

   if( fp != stdout && ii > 0 ){
      fprintf(fp, "%c  ni_stat     = \"",shp) ;
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

   if( fp != stdout ){
     if( binflag ) fprintf(fp," >") ;
     else          fprintf(fp,"# >\n") ;
     fflush(fp) ;
   }

   /* now write data */

   for( ii=0 ; ii < nxyz ; ii++ ){  /* loop over voxels */

     for( iv=0 ; iv < nv ; iv++ ){            /* loop over sub-bricks = columns */
       switch( DSET_BRICK_TYPE(dset,iv) ){
          default:
            val[0] = 0.0f ;
          break ;

          case MRI_float:{
            float *bar = DSET_ARRAY(dset,iv) ; val[0] = bar[ii] ;
          }
          break ;

          case MRI_short:{
            short *bar = DSET_ARRAY(dset,iv) ;
            float fac = DSET_BRICK_FACTOR(dset,iv) ; if( fac == 0.0 ) fac = 1.0 ;
            val[0] = fac*bar[ii] ;
          }
          break ;

          case MRI_byte:{
            byte *bar = DSET_ARRAY(dset,iv) ;
            float fac = DSET_BRICK_FACTOR(dset,iv) ; if( fac == 0.0 ) fac = 1.0 ;
            val[0] = fac*bar[ii] ;
          }
          break ;

          /* below here, we convert complicated types to floats, losing data! */

          case MRI_complex:{
            complex *bar = DSET_ARRAY(dset,iv) ;
            val[0] = CABS(bar[ii]) ;
          }
          break ;

          case MRI_rgb:{
            rgbyte *bar = DSET_ARRAY(dset,iv) ;
            val[0] = (0.299*bar[ii].r+0.587*bar[ii].g+0.114*bar[ii].g) ;
          }
          break ;
       } /* end of switch on sub-brick data type */

       if( binflag ) qq = fwrite( val , sizeof(float) , 1 , fp ) ;
       else          qq = fprintf( fp , " %g" , val[0] ) ;

       if( qq <= 0 ){   /* check for output error */
         ERROR_message("THD_write_1D('%s') failure!",fname) ;
         goto DONE ;
       }

     } /* end of loop over sub-bricks */

     if( !binflag ) fprintf(fp,"\n") ;

   } /* end of loop over voxels */

   /* NIML-style trailer */

 DONE:
   fflush(fp) ;

   if( fp != stdout ){
     fprintf(fp,"%c </AFNI_3D_dataset>\n",shp) ;
     fclose(fp) ;
   }

   EXRETURN ;
}
