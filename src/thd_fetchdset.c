#include "mrilib.h"

/*---------------------------------------------------------------------
   23 Mar 2001: Load a dataset across the Web!
-----------------------------------------------------------------------*/

THD_3dim_dataset * THD_fetch_dataset( char * url )
{
   char *cp,*hp,*bp , *thp ;
   int nhp,nbp , iv ;
   THD_3dim_dataset * dset ;
   int native_order ;

ENTRY("THD_fetch_dset") ;

   if( url == NULL || url[0] == '\0' ) RETURN(NULL) ;

   if( STRING_HAS_SUFFIX(url,".hdr") ) RETURN(NULL) ;  /* 27 Aug 2002 */

   /*** do we have to add .HEAD? ***/

   hp = AFMALL(char, sizeof(char)*(strlen(url)+32)) ; strcpy(hp,url) ;
   cp = strstr(hp,".HEAD") ;
   if( cp == NULL                          &&
       !STRING_HAS_SUFFIX(hp,".nii")       &&  /* 28 Aug 2003 */
       !STRING_HAS_SUFFIX(hp,".nii.gz")    &&  /* 06 Apr 2005 */
       !STRING_HAS_SUFFIX(hp,".niml")      &&  /* 16 Jun 2006 [rickr] */
       !STRING_HAS_SUFFIX(hp,".niml.dset") &&
       !STRING_HAS_SUFFIX(hp,".gii")       &&  /* 13 Feb 2008 [rickr] */
       !STRING_HAS_SUFFIX(hp,".gii.dset")  &&  /* 10 Mar 2008 [rickr] */
       !STRING_HAS_SUFFIX(hp,".mnc")       &&
       !STRING_HAS_SUFFIX(hp,".mnc.gz")   ) strcat(hp,".HEAD") ;

   /*** read the .HEAD file to a temporary file ***/

   fprintf(stderr,"\n+++ Trying to fetch %s",hp) ;
   nhp = NI_read_URL_tmpdir( hp , &thp ) ;
   if( nhp <= 0 ){ fprintf(stderr," **FAILED\n"); free(hp); RETURN(NULL); }

   /*** try to open it as a dataset header ***/

   fprintf(stderr,": %d bytes read\n ++ Trying to initialize dataset %s\n",nhp,thp) ;
   THD_allow_empty_dataset(1) ;
   dset = THD_open_one_dataset(thp) ;
   if( DSET_IS_MINC(dset) || DSET_IS_NIFTI(dset)        || /* 29 Oct 2001 */
       DSET_IS_NIML(dset) || DSET_IS_NI_SURF_DSET(dset) || /* 16 Jun 2006 [r] */
       DSET_IS_GIFTI(dset) )                               /* 13 Feb 2008 [r] */
      DSET_load(dset) ;

   THD_allow_empty_dataset(0) ;
   unlink(thp) ; free(thp) ;
   if( dset == NULL ){ fprintf(stderr," ** Can't decode %s\n",hp); free(hp); RETURN(NULL); }

   if( DSET_IS_VOLUMES(dset) ){  /* 20 Jun 2002 */
     fprintf(stderr," ** Can't load %s by volumes!\n",hp); free(hp);
     DSET_delete(dset); RETURN(NULL);
   }

   DSET_superlock(dset) ;  /* don't let be deleted from memory */
   if( DSET_IS_MINC(dset) || DSET_IS_NIFTI(dset)        || /* 29 Oct 2001 */
       DSET_IS_NIML(dset) || DSET_IS_NI_SURF_DSET(dset) || /* 16 Jun 2006 [r] */
       DSET_IS_GIFTI(dset) )                               /* 13 Feb 2008 [r] */
      RETURN(dset) ;
   DSET_mallocize(dset) ;

   /*** try to read the .BRIK or .BRIK.gz file into memory ***/

   strcpy( hp+(strlen(hp)-5) , ".BRIK.gz" ) ;
   fprintf(stderr," ++ Trying to fetch %s",hp) ; iochan_sleep(100) ;
   nbp = NI_read_URL( hp , &bp ) ;
   if( nbp <= 0 ){
      iv = strlen(hp) ; hp[iv-3] = '\0' ; /* remove the .gz and try again */
      fprintf(stderr," ** FAILED!\n ++ Trying to fetch %s",hp) ; iochan_sleep(100) ;
      nbp = NI_read_URL( hp , &bp ) ;
      if( nbp <= 0 ){
         fprintf(stderr," ** FAILED\n");
         free(hp); DSET_delete(dset); RETURN(NULL);
      }
   }
   if( nbp < dset->dblk->total_bytes ){
      fprintf(stderr,"\n ** Got %s but only had %d bytes, but needed %lld\n",
              hp,nbp,(long long)dset->dblk->total_bytes) ;
      free(bp) ; free(hp) ; DSET_delete(dset) ; RETURN(NULL) ;
   }
   fprintf(stderr,": %d bytes read\n",nbp) ;

   /** now have data: build pointers, load bricks, return **/

   for( iv=0 ; iv < DSET_NVALS(dset) ; iv++ ){
      mri_fix_data_pointer( bp , DBLK_BRICK(dset->dblk,iv) ) ;
      bp += DBLK_BRICK_BYTES(dset->dblk,iv) ;
   }

   /** perhaps need to swap bytes **/

   native_order = mri_short_order() ;

   if( dset->dblk->diskptr->byte_order <= 0 )
      dset->dblk->diskptr->byte_order = native_order ;

   if( dset->dblk->diskptr->byte_order != native_order ){
      for( iv=0 ; iv < DSET_NVALS(dset) ; iv++ ){
         switch( DBLK_BRICK_TYPE(dset->dblk,iv) ){
            default: break ;

            case MRI_short:
               mri_swap2( DBLK_BRICK_NVOX(dset->dblk,iv) , DBLK_ARRAY(dset->dblk,iv) ) ;
            break ;

            case MRI_complex:  /* 14 Sep 1999: swap complex also! */
               mri_swap4( 2*DBLK_BRICK_NVOX(dset->dblk,iv), DBLK_ARRAY(dset->dblk,iv)) ;
            break ;

            case MRI_float:
            case MRI_int:
               mri_swap4( DBLK_BRICK_NVOX(dset->dblk,iv) , DBLK_ARRAY(dset->dblk,iv) ) ;
            break ;
         }
      }
   }

   free(hp) ; RETURN(dset) ;
}

/*------------------------------------------------------------------------------
  26 Mar 2001: fetch a 1D file across the Web
--------------------------------------------------------------------------------*/

MRI_IMAGE * THD_fetch_1D( char * url )
{
   char *fname ;
   int nhp , ii ;
   MRI_IMAGE * flim ;
   float * far ;

ENTRY("THD_fetch_1D") ;

   if( url == NULL || url[0] == '\0' ) RETURN(NULL) ;

   fprintf(stderr,"\n+++ Trying to fetch %s",url) ;
   nhp = NI_read_URL_tmpdir( url , &fname ) ;
   if( nhp <= 0 ){ fprintf(stderr," **FAILED\n"); RETURN(NULL); }
   fprintf(stderr,": %d bytes read",nhp) ;
   flim = mri_read_1D(fname) ; unlink(fname) ; free(fname) ;
   if( flim != NULL ){
      mri_add_name( url , flim ) ; fprintf(stderr,": %dx%d file\n",flim->nx,flim->ny) ;
      far = MRI_FLOAT_PTR(flim) ;
      for( ii=0 ; ii < flim->nvox ; ii++ )
         if( fabs(far[ii]) >= 33333.0 ) far[ii] = WAY_BIG ;
   } else {
      fprintf(stderr," **Can't read as a .1D file!\n") ;
   }
   RETURN(flim) ;
}

/*------------------------------------------------------------------------------
   Load a bunch of datasets, whose names are read from a file given
   by URL -- the datasets are all fetched from the same place as the URL.
--------------------------------------------------------------------------------*/

RwcPointer_array * THD_fetch_many_datasets( char * url )
{
   RwcPointer_array * dsar ;
   int nlist , i1,i2 , nh , ll , nx ;
   char * list ;
   char * tnam , * hnam , * dnam ;
   THD_3dim_dataset * dset ;

ENTRY("THD_fetch_many_datasets") ;

   if( url == NULL || url[0] == '\0' ) RETURN(NULL) ;

   /* make hnam be the URL directory (without the trailing filename) */

   hnam = (char *) malloc(sizeof(char)*(strlen(url)+16)) ;
   strcpy(hnam,url) ;
   tnam = THD_trailname( hnam , 0 ) ;                /* start of trailing name */
   if( tnam == hnam ){ free(hnam); RETURN(NULL); }
   *tnam = '\0' ; nh = strlen(hnam) ;                /* cut trailing name off */

   /* get the list of filenames */

   fprintf(stderr,"\n+++ Trying to fetch %s",url) ;
   nlist = NI_read_URL( url , &list ) ;
   if( nlist <= 0 ){
      fprintf(stderr," **FAILED\n"); free(hnam); RETURN(NULL);
   }
   fprintf(stderr,": %d bytes read\n",nlist) ;

   /* scan from list[i1] forward, looking for filenames to fetch */

   i1 = 0 ; INIT_XTARR(dsar) ;

   while( i1 < nlist ){

      for( ; i1 < nlist && isspace(list[i1]) ; i1++ ) ; /* skip whitespace */
      if( i1 >= nlist ) break ;
      if( list[i1] == '#' ){ /* skip comment line */
         for( ; i1 < nlist && list[i1] != '\n' ; i1++ ) ; /* skip to EOL */
         continue ;                                       /* restart while loop */
      }

      for( i2=i1+1 ; i2 < nlist && !isspace(list[i2]) ; i2++ ) ; /* skip to next blank */

      /* filename is list[i1..i2-1] */

      dnam = (char *)malloc(sizeof(char)*(nh+i2-i1+16)) ;  /* space for new URL */
      strcpy(dnam,hnam);                                   /* put header on */
      memcpy(dnam+nh,list+i1,i2-i1);                       /* put filename on */
      dnam[nh+i2-i1] = '\0';                               /* and end it */

      iochan_sleep(100) ;
      ll = strlen(dnam)-3 ; if( ll < 1 ) continue ;
      if( strcmp(dnam+ll,".1D")==0 ||
          strcmp(dnam+ll,"1Dx")==0 ||
          strcmp(dnam+ll,"1Dv")==0   ){                      /** get a 1D file **/

         MRI_IMAGE * im = THD_fetch_1D( dnam ) ;

         if( im != NULL ){
            ADDTO_XTARR(dsar,im) ;
            nx = XTARR_NUM(dsar)-1 ;
            XTARR_IC(dsar,nx) = IC_FLIM ;
         }

      } else {                                                /** get a dataset **/
         dset = THD_fetch_dataset( dnam ) ;
         if( ISVALID_DSET(dset) ){
            ADDTO_XTARR(dsar,dset) ;
            nx = XTARR_NUM(dsar)-1 ;
            XTARR_IC(dsar,nx) = IC_DSET ;
         }
      }
      free(dnam) ;

      i1 = i2 ; /* restart scan at next position */
   }

   /* toss the trash and go home */

   free(list) ; free(hnam) ;
   if( dsar->num == 0 ){ FREE_XTARR(dsar) ; dsar = NULL ; }
   RETURN(dsar) ;
}
