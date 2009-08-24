/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

static int native_order = -1 ;
static int no_mmap      = -1 ;
static int floatscan    = -1 ;  /* 30 Jul 1999 */

#define PRINT_SIZE 66600000
#define PRINT_STEP 10

static int verbose = 0 ;

void THD_load_datablock_verbose( int v ){ verbose = v; }
int THD_alloc_datablock( THD_datablock *blk ) ;

/*-----------------------------------------------------------------*/
/*! Check if all sub-bricks have the same datum type. [14 Mar 2002]
-------------------------------------------------------------------*/

int THD_datum_constant( THD_datablock *blk )
{
   int ibr , dzero , nv=blk->nvals ;
   if( nv == 1 ) return 1 ;                /* of course */
   dzero = DBLK_BRICK_TYPE(blk,0) ;        /* #0 type */
   for( ibr=1 ; ibr < nv ; ibr++ )
      if( dzero != DBLK_BRICK_TYPE(blk,ibr) ) return 0 ;
   return 1 ;
}

/*---------------------------------------------------------------*/
/*! Return a float representation of a given voxel. [15 Sep 2004]
-----------------------------------------------------------------*/

float THD_get_voxel( THD_3dim_dataset *dset , int ijk , int ival )
{
   void *ar ;
   float val , fac ;

   if( ival < 0 || ival >= DSET_NVALS(dset) ) return 0.0f ;
   if( ijk < 0  || ijk  >= DSET_NVOX(dset)  ) return 0.0f ;

   ar = DSET_ARRAY(dset,ival) ;
   if( ar == NULL ){ DSET_load(dset) ; ar = DSET_ARRAY(dset,ival) ; }
   if( ar == NULL ) return 0.0f ;

   switch( DSET_BRICK_TYPE(dset,ival) ){

     default: return 0.0f ;

     case MRI_byte:
       val = (float)(((byte *)ar)[ijk])   ; break ;
     case MRI_short:
       val = (float)(((short *)ar)[ijk])  ; break ;
     case MRI_int:
       val = (float)(((int *)ar)[ijk])    ; break ;
     case MRI_float:
       val = (float)(((float *)ar)[ijk])  ; break ;
     case MRI_double:
       val = (float)(((double *)ar)[ijk]) ; break ;

     case MRI_complex:{
       complex c = (((complex *)ar)[ijk]) ;
       val = sqrt(c.r*c.r+c.i*c.i) ;
       break ;
     }

     case MRI_rgb:{
       rgbyte c = (((rgbyte *)ar)[ijk]) ;
       val = 0.299f*(float)(c.r) + 0.587f*(float)(c.g) + 0.114f*(float)(c.b) ;
       break ;
     }

     case MRI_rgba:{
       rgba c = (((rgba *)ar)[ijk]) ;
       val = 0.299f*(float)(c.r) + 0.587f*(float)(c.g) + 0.114f*(float)(c.b) ;
       val *= 0.00392157f*(float)(c.a) ;
       break ;
     }
   }

   fac = DSET_BRICK_FACTOR(dset,ival) ;
   if( fac > 0.0f ) val *= fac ;
   return val ;
}

/*---------------------------------------------------------------
  18 Oct 2001:
  Put freeup function here, and set it by a function, rather
  than have it provided on every call to THD_load_datablock()
----------------------------------------------------------------*/

static generic_func *freeup=NULL ;   /* 18 Oct 2001 */

void THD_set_freeup( generic_func *ff ){ freeup = ff; }

/*---------------------------------------------------------------*/

Boolean THD_load_datablock( THD_datablock *blk )
{
   THD_diskptr *dkptr ;
   int id , offset ;
   int nx,ny,nz , nxy,nxyz , nv , ii , ibr ;
   char *ptr ;
   int verb=verbose ;
   int64_t idone , print_size=PRINT_SIZE ;

ENTRY("THD_load_datablock") ; /* 29 Aug 2001 */

   if( native_order < 0 ) native_order = mri_short_order() ; /* 1st time in */

   floatscan = AFNI_yesenv("AFNI_FLOATSCAN") ;  /* check float bricks? */

   if( floatscan ) no_mmap = 1 ;                          /* perhaps disable */
   else            no_mmap = AFNI_yesenv("AFNI_NOMMAP") ; /* use of mmap()  */

   /*-- sanity checks --*/

   if( ! ISVALID_DATABLOCK(blk) || blk->brick == NULL ){
     STATUS("Illegal inputs"); RETURN( False );
   }

   ii = THD_count_databricks( blk ) ;
   if( ii == blk->nvals ) RETURN( True );   /* already loaded! */

   if( blk->malloc_type == DATABLOCK_MEM_UNDEFINED ){
     STATUS("malloc_type forced to DATABLOCK_MEM_MALLOC") ;
     blk->malloc_type = DATABLOCK_MEM_MALLOC ;
   }

   /* these next 2 conditions should never happen */

   dkptr = blk->diskptr ;
   if( ! ISVALID_DISKPTR(dkptr) ){
     STATUS("invalid dkptr!!!"); RETURN( False );
   }
   if( dkptr->storage_mode == STORAGE_UNDEFINED ){
     if( PRINT_TRACING ){
       char str[512] ; THD_3dim_dataset *dset=(THD_3dim_dataset *)(blk->parent) ;
       sprintf(str,"dataset %s == STORAGE_UNDEFINED",DSET_BRIKNAME(dset)) ;
       STATUS(str) ;
     }
     RETURN(False) ;
   }

   if( dkptr->rank != 3 ){
     fprintf(stderr,"\n*** Cannot read non 3D datablocks ***\n"); RETURN(False);
   }

   if( dkptr->storage_mode == STORAGE_BY_VOLUMES ) no_mmap = 1 ;  /* 20 Jun 2002 */

   /*-- 29 Oct 2001: MINC input (etc.) --*/

   if( dkptr->storage_mode == STORAGE_BY_MINC ){
     THD_load_minc( blk ) ;
     ii = THD_count_databricks( blk ) ;
     if( ii == blk->nvals ){
       THD_update_statistics( (THD_3dim_dataset *)blk->parent ) ;
       ii = dblk_floatscan(blk) ;  /* 22 Feb 2007 */
       if(ii>0) WARNING_message("fixed %d bad floats in %s",ii,dkptr->brick_name);
       RETURN( True ) ;
     }
     STATUS("can't read MINC file?!") ;
     RETURN( False ) ;
   }

   { THD_3dim_dataset *ds = (THD_3dim_dataset *)blk->parent ;  /* 04 Aug 2004 */
     if( ISVALID_DSET(ds) && DSET_IS_TCAT(ds) ){
       THD_load_tcat( blk ) ;
       ii = THD_count_databricks( blk ) ;
       if( ii == blk->nvals ){
         THD_update_statistics( (THD_3dim_dataset *)blk->parent ) ;
         RETURN( True ) ;
       }
       STATUS("can't read tcat-ed file?!") ;
       RETURN( False ) ;
     }
   }

   if( dkptr->storage_mode == STORAGE_BY_ANALYZE ){
     THD_load_analyze( blk ) ;
     ii = THD_count_databricks( blk ) ;
     if( ii == blk->nvals ){
       THD_update_statistics( (THD_3dim_dataset *)blk->parent ) ;
       ii = dblk_floatscan(blk) ;  /* 22 Feb 2007 */
       if(ii>0) WARNING_message("fixed %d bad floats in %s",ii,dkptr->brick_name);
       RETURN( True ) ;
     }
     STATUS("can't read ANALYZE file?!") ;
     RETURN( False ) ;
   }

   if( dkptr->storage_mode == STORAGE_BY_CTFMRI ){  /* 04 Dec 2002 */
     THD_load_ctfmri( blk ) ;
     ii = THD_count_databricks( blk ) ;
     if( ii == blk->nvals ){
       THD_update_statistics( (THD_3dim_dataset *)blk->parent ) ;
       ii = dblk_floatscan(blk) ;  /* 22 Feb 2007 */
       if(ii>0) WARNING_message("fixed %d bad floats in %s",ii,dkptr->brick_name);
       RETURN( True ) ;
     }
     STATUS("can't read CTF MRI file?!") ;
     RETURN( False ) ;
   }

   if( dkptr->storage_mode == STORAGE_BY_CTFSAM ){  /* 04 Dec 2002 */
     THD_load_ctfsam( blk ) ;
     ii = THD_count_databricks( blk ) ;
     if( ii == blk->nvals ){
       THD_update_statistics( (THD_3dim_dataset *)blk->parent ) ;
       ii = dblk_floatscan(blk) ;  /* 22 Feb 2007 */
       if(ii>0) WARNING_message("fixed %d bad floats in %s",ii,dkptr->brick_name);
       RETURN( True ) ;
     }
     STATUS("can't read CTF SAM file?!") ;
     RETURN( False ) ;
   }

   if( dkptr->storage_mode == STORAGE_BY_1D ){      /* 04 Mar 2003 */
     THD_load_1D( blk ) ;
     ii = THD_count_databricks( blk ) ;
     if( ii == blk->nvals ){
       THD_update_statistics( (THD_3dim_dataset *)blk->parent ) ;
       RETURN( True ) ;
     }
     STATUS("can't read 1D dataset file?!") ;
     RETURN( False ) ;
   }

   if( dkptr->storage_mode == STORAGE_BY_3D ){      /* 21 Mar 2003 */
     THD_load_3D( blk ) ;
     ii = THD_count_databricks( blk ) ;
     if( ii == blk->nvals ){
       THD_update_statistics( (THD_3dim_dataset *)blk->parent ) ;
       RETURN( True ) ;
     }
     STATUS("can't read 3D dataset file?!") ;
     RETURN( False ) ;
   }

   if( dkptr->storage_mode == STORAGE_BY_NIFTI ){   /* 28 Aug 2003 */
     THD_load_nifti( blk ) ;
     ii = THD_count_databricks( blk ) ;
     if( ii == blk->nvals ){
       THD_update_statistics( (THD_3dim_dataset *)blk->parent ) ;
       RETURN( True ) ;
     }
     STATUS("can't read NIFTI dataset file?!") ;
     RETURN( False ) ;
   }

   if( dkptr->storage_mode == STORAGE_BY_MPEG ){   /* 03 Dec 2003 */
     THD_load_mpeg( blk ) ;
     ii = THD_count_databricks( blk ) ;
     if( ii == blk->nvals ){
       THD_update_statistics( (THD_3dim_dataset *)blk->parent ) ;
       RETURN( True ) ;
     }
     STATUS("can't read MPEG dataset file?!") ;
     RETURN( False ) ;
   }

   if( dkptr->storage_mode == STORAGE_BY_NIML ){   /* 26 May 2006 [rickr] */
     if( THD_load_niml( blk ) ){
       STATUS("failed to load NIML file"); RETURN( False );
     }
     if( THD_count_databricks( blk ) == blk->nvals ){
       THD_update_statistics( (THD_3dim_dataset *)blk->parent ) ;
       ii = dblk_floatscan(blk) ;  /* 22 Feb 2007 */
       if(ii>0) WARNING_message("fixed %d bad floats in %s",ii,dkptr->brick_name);
       RETURN( True ) ;
     }
     STATUS("can't read NIML dataset file?!") ;
     RETURN( False ) ;
   }

   if( dkptr->storage_mode == STORAGE_BY_NI_SURF_DSET ) {
     THD_load_niml( blk ) ;
     ii = THD_count_databricks( blk ) ;
     if( ii == blk->nvals ){
       THD_update_statistics( (THD_3dim_dataset *)blk->parent ) ;
       RETURN( True ) ;
     }
     STATUS("can't read NI_SURF_DSET dataset file?!") ;
     RETURN( False ) ;
   }

   if( dkptr->storage_mode == STORAGE_BY_GIFTI ) {  /* 13 Feb 2008 */
     THD_load_gifti( blk ) ;
     ii = THD_count_databricks( blk ) ;
     if( ii == blk->nvals ){
       THD_update_statistics( (THD_3dim_dataset *)blk->parent ) ;
       RETURN( True ) ;
     }
     STATUS("can't read GIFTI dataset file?!") ;
     RETURN( False ) ;
   }

   /*** END OF SPECIAL CASES ABOVE; NOW FOR THE AFNI FORMAT! ***/

   /*-- allocate data space --*/

   nx = dkptr->dimsizes[0] ;
   ny = dkptr->dimsizes[1] ;  nxy   = nx * ny   ;
   nz = dkptr->dimsizes[2] ;  nxyz  = nxy * nz  ;
   nv = dkptr->nvals       ;

   if( DBLK_IS_MASTERED(blk) && blk->malloc_type == DATABLOCK_MEM_MMAP ) /* 11 Jan 1999 */
      blk->malloc_type = DATABLOCK_MEM_MALLOC ;

   /* the following code is due to Mike Beauchamp's idiocy */

   if( !THD_datum_constant(blk) && blk->malloc_type != DATABLOCK_MEM_MALLOC ){
     fprintf(stderr,"++ WARNING: dataset %s: non-uniform sub-brick types\n",
             blk->diskptr->filecode) ;
     blk->malloc_type = DATABLOCK_MEM_MALLOC ;
   }

   /* 25 April 1998: byte order issues */

   if( dkptr->byte_order <= 0 ) dkptr->byte_order = native_order ;

   /* 05 Jul 2001: if all sub-bricks are bytes,
                   mark dataset as being in native order
      20 Jun 2002: also don't have to swap RGB datasets */

   if( dkptr->byte_order != native_order ){
      for( ii=0 ; ii < nv ; ii++ )
         if( DBLK_BRICK_TYPE(blk,ii) != MRI_byte &&
             DBLK_BRICK_TYPE(blk,ii) != MRI_rgb     ) break ;
      if( ii == nv ) dkptr->byte_order = native_order ;
   }

   /* under some circumstances, we must force use of malloc() */

   if( dkptr->byte_order != native_order || no_mmap ){
     if( blk->malloc_type == DATABLOCK_MEM_MMAP )
       blk->malloc_type = DATABLOCK_MEM_MALLOC ;
   }

   DBLK_mmapfix(blk) ;  /* 18 Mar 2005 */

   /** set up space for bricks via malloc, if so ordered **/

   if( blk->malloc_type == DATABLOCK_MEM_MALLOC ||
       blk->malloc_type == DATABLOCK_MEM_SHARED   ){

     ii = THD_alloc_datablock( blk ) ;   /* 02 May 2003 */
     if( ii == 0 ) RETURN(False) ;

   /** mmap whole file (makes space and reads it all at once) **/
   /** [if this route is followed, then we will finish here]  **/

   } else if( blk->malloc_type == DATABLOCK_MEM_MMAP ){

      int fd ; long long fsize ;
      fd = open( dkptr->brick_name , O_RDONLY ) ;  /* N.B.: readonly mode */
      if( fd < 0 ){
         fprintf( stderr , "\n*** cannot open brick file %s for mmap\n"
                           "   - do you have permission? does file exist?\n" ,
                  dkptr->brick_name ) ;
         perror("*** Unix error message") ;
         STATUS("open failed") ;
         RETURN( False );
      }

      /* 04 May 2001: check file size (the Katie Lee bug) */

      fsize = THD_filesize( dkptr->brick_name ) ;
      if( fsize < blk->total_bytes )
        fprintf(stderr ,
                "\n*** WARNING: file %s size is %lld, but should be at least %lld!\n" ,
                dkptr->brick_name , fsize , (long long)blk->total_bytes ) ;

      /* clear the sub-brick pointers */

      for( ibr=0 ; ibr < nv ; ibr++ )
        mri_clear_data_pointer( DBLK_BRICK(blk,ibr) ) ;

      /* map the file into memory */

      ptr = (char *) mmap( 0 , (size_t)blk->total_bytes ,
                               PROT_READ , THD_MMAP_FLAG , fd , 0 ) ;

      /* if that fails, maybe try again (via freeup) */

      if( ptr == (char *)(-1) ){
         fprintf(stderr ,
                 "\n*** cannot mmap brick file %s - maybe hit a system limit?\n" ,
                 dkptr->brick_name ) ;
         perror("*** Unix error message") ;
         if( freeup != NULL ){
            fprintf(stderr,"*** trying to fix problem\n") ; /* 18 Oct 2001 */
            freeup() ;                          /* AFNI_purge_unused_dsets */
            ptr = (char *) mmap( 0 , (size_t)blk->total_bytes ,
                                     PROT_READ , THD_MMAP_FLAG , fd , 0 ) ;
            if( ptr == (char *)(-1) ){
               fprintf(stderr,"*** cannot fix problem!\n") ;
               close(fd) ;
               STATUS("freeup failed") ; RETURN( False );
            } else {
               fprintf(stderr,"*** was able to fix problem!\n") ;
            }
         } else {       /* no freeup function to try */
            close(fd) ;
            STATUS("mmap failed") ; RETURN( False );
         }
      }

      close(fd) ;  /* can close file after mmap-ing it */

      /* (re)create pointers to all sub-bricks */

      offset = 0 ;
      for( ibr=0 ; ibr < nv ; ibr++ ){
         mri_fix_data_pointer( ptr , DBLK_BRICK(blk,ibr) ) ;
         ptr += DBLK_BRICK_BYTES(blk,ibr) ;
      }

      STATUS("mmap succeeded") ;
      RETURN( True );  /* finito */

   } else {
      STATUS("unknown malloc_type code?!") ;
      RETURN( False ) ;  /* should never happen */
   }

   /*** Below here, space for brick images was malloc()-ed,
        and now we have to read data into them, somehow    ***/

   ptr = getenv("AFNI_LOAD_PRINTSIZE") ;   /* 23 Aug 2002 */
   if( verb && ptr != NULL ){
     char *ept ;
     idone = strtol( ptr , &ept , 10 ) ;
     if( idone > 0 ){
            if( *ept == 'K' || *ept == 'k' ) idone *= 1024 ;
       else if( *ept == 'M' || *ept == 'm' ) idone *= 1024*1024 ;
       print_size = idone ;
     } else {
       print_size = 666000000 ;
     }
   }

   if( verb ) verb = (blk->total_bytes > print_size) ;
   if( verb )
     fprintf(stderr,"reading dataset %s (%s bytes)",
             dkptr->filecode ,
             approximate_number_string((double)blk->total_bytes) ) ;

   switch( dkptr->storage_mode ){

      /*-- should never ever happen --*/

      default:
         fprintf(stderr,"\n*** illegal storage mode in read ***\n") ;
         RETURN( False );
      break ;

      /*-- read everything from .BRIK file --*/

      case STORAGE_BY_BRICK:{
         FILE *far ;

         STATUS("reading from BRIK file") ;

         far = COMPRESS_fopen_read( dkptr->brick_name ) ;

         if( far == NULL ){
            THD_purge_datablock( blk , blk->malloc_type ) ;
            fprintf(stderr,
                    "\n*** failure while opening brick file %s "
                    "- do you have permission?\n" ,
                    dkptr->brick_name ) ;
            perror("*** Unix error message") ;
            RETURN( False );
         }

         /* read each sub-brick all at once */

         idone = 0 ;
         if( ! DBLK_IS_MASTERED(blk) ){      /* read each brick */

            for( ibr=0 ; ibr < nv ; ibr++ ){
              idone += fread( DBLK_ARRAY(blk,ibr), 1,
                              DBLK_BRICK_BYTES(blk,ibr), far ) ;

              if( verb && ibr%PRINT_STEP == 0 ) fprintf(stderr,".") ;
            }

         } else {  /* 11 Jan 1999: read brick from master, put into place(s) */

            int nfilled = 0 , nbuf=0 , jbr, nbr ;
            char *buf=NULL ;  /* temp buffer for master sub-brick */

            /* loop over master sub-bricks until dataset is filled */
            /* [because dataset might be compressed, must read them in
                sequence, even if some intermediate ones aren't used. ] */

            for( ibr=0 ; nfilled < nv && ibr < blk->master_nvals ; ibr++ ){

               if( nbuf < blk->master_bytes[ibr] ){  /* make more space for it */
                 if( buf != NULL ) free(buf) ;
                 nbuf = blk->master_bytes[ibr] ;
                 buf  = AFMALL(char, sizeof(char) * nbuf ) ;
                 if( buf == NULL ) break ;
               }

               /* read the master sub-brick #ibr */

               nbr = fread( buf , 1 , blk->master_bytes[ibr] , far ) ;
               if( verb && ibr%PRINT_STEP == 0 ) fprintf(stderr,".") ;
               if( nbr < blk->master_bytes[ibr] ) break ;

               /* find all the dataset sub-bricks that are copies of this */

               for( jbr=0 ; jbr < nv ; jbr++ ){
                 if( blk->master_ival[jbr] == ibr ){  /* copy it in */
                   memcpy( DBLK_ARRAY(blk,jbr) , buf , blk->master_bytes[ibr] ) ;
                   nfilled++ ;  /* number of bricks filled */
                   idone += nbr ;  /* number of bytes read into dataset */
                 }
               }
            }  /* end of loop over master sub-bricks */

            if( buf != NULL ) free(buf) ;  /* free temp sub-brick */
         }

         /* close input file */

         COMPRESS_fclose(far) ;

         /* check if total amount of data read is correct */

         if( idone != blk->total_bytes ){
            THD_purge_datablock( blk , blk->malloc_type ) ;
            fprintf(stderr ,
                    "\n*** failure while reading from brick file %s\n"
                      "*** desired %lld bytes but only got %lld\n" ,
                    dkptr->brick_name ,
                    (long long)blk->total_bytes , (long long)idone ) ;
            perror("*** Unix error message") ;
            RETURN( False );
         }
      }
      break ;  /* end of STORAGE_BY_BRICK */

      /*** Read from a sequence of volume files (1 per sub-brick) [20 Jun 2002] ***/

      case STORAGE_BY_VOLUMES:{
        ATR_string *atr ;
        char **fnam , *ptr , *flist ;
        FILE *far ;

        STATUS("reading from volume files") ;

        /* get list of filenames */

        atr = THD_find_string_atr(blk,"VOLUME_FILENAMES") ;

        /* the following should never happen */

        if( atr == NULL || atr->nch < 2*nv ){
          THD_purge_datablock( blk , blk->malloc_type ) ;
          fprintf(stderr,
                  "Dataset %s does not have legal VOLUME_FILENAMES attribute!\n",
                  blk->diskptr->filecode) ;
          RETURN( False );
        }

        /* copy filename list into NUL terminated local string */

        flist = AFMALL(char, atr->nch+1) ;
        memcpy( flist , atr->ch , atr->nch ) ;
        flist[atr->nch] = '\0' ;

#if 0
fprintf(stderr,"VOL: flist:\n%s\n",flist) ;
#endif

        /* break filename list into component filenames (1 per sub-brick) */

        fnam = (char **) malloc(sizeof(char *)*nv) ;
        for( ibr=0 ; ibr < nv ; ibr++ ){

          /* find sub-string with next filename */

          ptr = strtok( (ibr==0)?flist:NULL , " \t\n\r\f\v" ) ;

          /* didn't get one => bad news */

          if( ptr == NULL ){
            fprintf(stderr,
                    "Dataset %s has illegal VOLUME_FILENAMES attribute!\n",
                    blk->diskptr->filecode) ;
            for( ii=0 ; ii < nv ; ii++ ){
              free( DBLK_ARRAY(blk,ii) ) ;
              mri_clear_data_pointer( DBLK_BRICK(blk,ii) ) ;
            }
            for( ii=0 ; ii < ibr ; ii++ ) free(fnam[ii]) ;
            free(flist) ; free(fnam) ;
            RETURN( False );
          }

          /* make fnam[ibr] = name ibr-th volume file */

#if 0
fprintf(stderr,"VOL[%d]: ptr=%s\n",ibr,ptr) ;
#endif

          if( *ptr == '/' ){          /* if filename is absolute, duplicate it */
            fnam[ibr] = strdup(ptr) ;
          } else {                    /* otherwise, put directory name in front */
            ii = strlen(blk->diskptr->directory_name) + strlen(ptr) ;
            fnam[ibr] = AFMALL(char,ii+1) ; fnam[ibr][0] = '\0' ;
            strcat(fnam[ibr],blk->diskptr->directory_name) ;
            strcat(fnam[ibr],ptr) ;
          }
#if 0
fprintf(stderr,"VOL[%d]: fnam=%s\n",ibr,fnam[ibr]) ;
#endif
        }
        free(flist) ;  /* done with unparsed filename list copy */

        /*** loop over sub-bricks and read them in ***/

        if( ! DBLK_IS_MASTERED(blk) ){      /* read each brick */

          for( ibr=0 ; ibr < nv ; ibr++ ){

#if 0
fprintf(stderr,"VOL[%d]: opening %s\n",ibr,fnam[ibr]) ;
#endif

            far = COMPRESS_fopen_read( fnam[ibr] ) ;  /* open file */

            if( far == NULL ){                   /* can't open it? */
              fprintf(stderr,
                      "\n*** Failure while opening volume file %s "
                      "- do you have permission?\n" ,
                    fnam[ibr] ) ;
              perror("*** Unix error message") ;
              THD_purge_datablock( blk , blk->malloc_type ) ;
              for( ii=0 ; ii < nv ; ii++ ) free(fnam[ii]) ;
              free(fnam); RETURN( False );
            }

            /* read all of file at once */

            id = fread( DBLK_ARRAY(blk,ibr), 1,
                        DBLK_BRICK_BYTES(blk,ibr), far ) ;
            if( verb && ibr%PRINT_STEP==0 ) fprintf(stderr,".") ;

#if 0
fprintf(stderr,"VOL[%d]: id=%d\n",ibr,id) ;
#endif

            COMPRESS_fclose(far) ;

            if( id < DBLK_BRICK_BYTES(blk,ibr) ){
              fprintf(stderr,
                      "\n*** Volume file %s only gave %d out of %d bytes needed!\n",
                     fnam[ibr] , id , DBLK_BRICK_BYTES(blk,ibr) ) ;
              THD_purge_datablock( blk , blk->malloc_type ) ;
              for( ii=0 ; ii < nv ; ii++ ) free(fnam[ii]) ;
              free(fnam); RETURN( False );
            }

          } /* end of loop over sub-bricks */

        } else {  /*** Mastered dataset ==> must read only selected sub-bricks ***/

          int jbr ;

          /** loop over output sub-bricks **/

          for( jbr=0 ; jbr < nv ; jbr++ ){
            ibr = blk->master_ival[jbr] ;             /* master sub-brick index */
            far = COMPRESS_fopen_read( fnam[ibr] ) ;  /* open file */

            if( far == NULL ){                   /* can't open it? */
              fprintf(stderr,
                      "\n*** Failure while opening volume file %s "
                      "- do you have permission?\n" ,
                    fnam[ibr] ) ;
              perror("*** Unix error message") ;
              THD_purge_datablock( blk , blk->malloc_type ) ;
              for( ii=0 ; ii < nv ; ii++ ) free(fnam[ii]) ;
              free(fnam); RETURN( False );
            }

            /* read all of file at once */

            id = fread( DBLK_ARRAY(blk,jbr), 1,
                        DBLK_BRICK_BYTES(blk,jbr), far ) ;
            if( verb && jbr%PRINT_STEP == 0 ) fprintf(stderr,".") ;

            COMPRESS_fclose(far) ;

            if( id < DBLK_BRICK_BYTES(blk,jbr) ){
              fprintf(stderr,
                      "\n*** Volume file %s only gave %d out of %d bytes needed!\n",
                     fnam[ibr] , id , DBLK_BRICK_BYTES(blk,jbr) ) ;
              THD_purge_datablock( blk , blk->malloc_type ) ;
              for( ii=0 ; ii < nv ; ii++ ) free(fnam[ii]) ;
              free(fnam); RETURN( False );
            }

          } /* end of loop over sub-bricks */

        } /* end of mastered vs. unmastered dataset */

        /*** at this point, all the data has been read in ***/

        /* volume filenames no longer needed */

        for( ii=0 ; ii < nv ; ii++ ) free(fnam[ii]) ;
        free(fnam) ;

      }
      break ; /* end of STORAGE_BY_VOLUMES */

   } /* end of switch on storage modes */

   /************* At this point, data is all in bricks;
                  now do any post-processing before exiting *************/

   STATUS("data has been read in") ;

   /* 25 April 1998: check and fix byte ordering */

   if( dkptr->byte_order != native_order ){
      STATUS("byte swapping") ;
      if( verb ) fprintf(stderr,".byte swap") ;

      for( ibr=0 ; ibr < nv ; ibr++ ){
         switch( DBLK_BRICK_TYPE(blk,ibr) ){
            case MRI_short:
               mri_swap2( DBLK_BRICK_NVOX(blk,ibr) , DBLK_ARRAY(blk,ibr) ) ;
            break ;

            case MRI_complex:  /* 14 Sep 1999: swap complex also! */
               mri_swap4( 2*DBLK_BRICK_NVOX(blk,ibr), DBLK_ARRAY(blk,ibr)) ;
            break ;

            case MRI_float:
            case MRI_int:
               mri_swap4( DBLK_BRICK_NVOX(blk,ibr) , DBLK_ARRAY(blk,ibr) ) ;
            break ;
         }
      }
   }

   /* 30 July 1999: check float sub-brick for errors? */
   /* 14 Sep  1999: also check complex sub-bricks!    */

   if( floatscan ){
      int nerr=0 ;
      STATUS("float scanning") ;
      if( verb ) fprintf(stderr,".float scan") ;

      for( ibr=0 ; ibr < nv ; ibr++ ){
         if( DBLK_BRICK_TYPE(blk,ibr) == MRI_float ){
            nerr += thd_floatscan( DBLK_BRICK_NVOX(blk,ibr) ,
                                   DBLK_ARRAY(blk,ibr)        ) ;

         } else if( DBLK_BRICK_TYPE(blk,ibr) == MRI_complex ) {  /* 14 Sep 1999 */
            nerr += thd_complexscan( DBLK_BRICK_NVOX(blk,ibr) ,
                                     DBLK_ARRAY(blk,ibr)        ) ;
         }
      }
      if( nerr > 0 )
         WARNING_message("file %s: fixed %d float errors" ,
                         dkptr->brick_name , nerr ) ;
   }

   /* 21 Feb 2001: if sub-ranging also used, clip values in brick */

#if 0
fprintf(stderr,"master_bot=%g master_top=%g\n",blk->master_bot,blk->master_top) ;
#endif
   if( DBLK_IS_MASTERED(blk) && blk->master_bot <= blk->master_top )
      THD_apply_master_subrange(blk) ;

   if( verb ) fprintf(stderr,".done\n") ;

   RETURN( True ) ;  /* things are now cool */
}

/*----------------------------------------------------------------------------*/
/*! Allocate memory for the volumes in a datablock -- 02 May 2003.
    Return value is 1 if OK, 0 if not.
------------------------------------------------------------------------------*/

int THD_alloc_datablock( THD_datablock *blk )
{
   int ii,nbad,ibr, nv  ;
   char *ptr ;

ENTRY("THD_alloc_datablock") ;

   /*-- sanity checks --*/

   if( ! ISVALID_DATABLOCK(blk) || blk->brick == NULL ){
     STATUS("Illegal inputs"); RETURN(0);
   }

   nv = blk->nvals ;
   ii = THD_count_databricks( blk ) ;
   if( ii == nv ) RETURN(1);   /* already loaded! */

   switch( blk->malloc_type ){

     default: RETURN(0) ;         /* stupid datablock */

     /*-------------------------------------------*/
     /* malloc separate arrays for each sub-brick */
     /*-------------------------------------------*/

     case DATABLOCK_MEM_MALLOC:{

      /** malloc space for each brick separately **/

      STATUS("trying to malloc sub-bricks") ;
      for( nbad=ibr=0 ; ibr < nv ; ibr++ ){
        if( DBLK_ARRAY(blk,ibr) == NULL ){
          ptr = AFMALL(char, DBLK_BRICK_BYTES(blk,ibr) ) ;
          mri_fix_data_pointer( ptr ,  DBLK_BRICK(blk,ibr) ) ;
          if( ptr == NULL ) nbad++ ;
        }
      }
      if( nbad == 0 ) RETURN(1) ;   /* things are cool */

      /* at least one malloc() failed, so possibly try to free some space */

       fprintf(stderr,
               "\n** failed to malloc %d dataset bricks out of %d - is memory exhausted?\n",
               nbad,nv ) ;
#ifdef USING_MCW_MALLOC
       { char *str = MCW_MALLOC_status ;
         if( str != NULL ) fprintf(stderr,"*** MCW_malloc summary: %s\n",str); }
#endif

       if( freeup == NULL ){                     /* don't have a freeup() function? */
         for( ibr=0 ; ibr < nv ; ibr++ ){        /* then toss all data bricks and scram */
           if( DBLK_ARRAY(blk,ibr) != NULL ){
             free(DBLK_ARRAY(blk,ibr)) ;
             mri_fix_data_pointer( NULL , DBLK_BRICK(blk,ibr) ) ;
           }
         }
         STATUS("malloc failed, no freeup"); RETURN(0);  /* go away */
       }

       /* space freeing is done by caller-supplied function freeup() */

       fprintf(stderr,"** trying to free some memory\n") ;   /* 18 Oct 2001 */
       freeup() ;                          /* cf. AFNI_purge_unused_dsets() */

       /* now try to malloc() those that failed before */

       for( ibr=0 ; ibr < nv ; ibr++ ){
         if( DBLK_ARRAY(blk,ibr) == NULL ){
           ptr = AFMALL(char, DBLK_BRICK_BYTES(blk,ibr) ) ;
           mri_fix_data_pointer( ptr ,  DBLK_BRICK(blk,ibr) ) ;
         }
       }

       /* if it still failed, then free everything and go away */

       if( THD_count_databricks(blk) < nv ){
         fprintf(stderr,"** cannot free up enough memory\n") ;
#ifdef USING_MCW_MALLOC
         { char *str = MCW_MALLOC_status ;
           if( str != NULL ) fprintf(stderr,"*** MCW_malloc summary: %s\n",str); }
#endif
         for( ibr=0 ; ibr < nv ; ibr++ ){  /* 18 Oct 2001 */
           if( DBLK_ARRAY(blk,ibr) != NULL ){
             free(DBLK_ARRAY(blk,ibr)) ;
             mri_fix_data_pointer( NULL , DBLK_BRICK(blk,ibr) ) ;
           }
         }
         STATUS("malloc failed; freeup failed"); RETURN(0);  /* go away */
       }

       /* malloc() didn't fail this time! */

       STATUS("malloc failed; freeup worked") ;
       fprintf(stderr,"*** was able to free up enough memory\n") ;
#ifdef USING_MCW_MALLOC
       { char *str = MCW_MALLOC_status ;
         if( str != NULL ) fprintf(stderr,"*** MCW_malloc summary: %s\n",str); }
#endif
     }
     RETURN(1) ;   /* get to here is good */

     /*-------------------------------------------------------------------*/
     /* create a shared memory segment, then setup sub-bricks inside that */
     /*-------------------------------------------------------------------*/

     case DATABLOCK_MEM_SHARED:
#if !defined(DONT_USE_SHM) && !defined(CYGWIN)
     { unsigned int offset ;
       if( blk->shm_idcode[0] == '\0' ){   /* new segment */
         UNIQ_idcode_fill( blk->shm_idcode ) ;
         blk->shm_idint = shm_create( blk->shm_idcode , (int)blk->total_bytes ) ;
         if( blk->shm_idint < 0 ){ blk->shm_idcode[0] = '\0'; RETURN(0); }
         ptr = shm_attach( blk->shm_idint ) ;
         if( ptr == NULL ){
           shmctl( blk->shm_idint , IPC_RMID , NULL ) ;
           blk->shm_idint = -1; blk->shm_idcode[0] = '\0'; RETURN(0);
         }
         offset = 0 ;
         for( ibr=0 ; ibr < nv ; ibr++ ){
           mri_fix_data_pointer( ptr , DBLK_BRICK(blk,ibr) ) ;
           ptr += DBLK_BRICK_BYTES(blk,ibr) ;
         }
       }
     }
     RETURN(1) ;
#else
     RETURN(0) ;
#endif

   }

   RETURN(0) ; /* should never get to here */
}

/*----------------------------------------------------------------------------*/
/*! Fill up a dataset with zero-ed out sub-bricks, if needed.
------------------------------------------------------------------------------*/

void THD_zerofill_dataset( THD_3dim_dataset *dset )
{
   int ii ;
   void *vpt ;

ENTRY("THD_zerofill_dataset") ;

   if( !ISVALID_DSET(dset) || !ISVALID_DATABLOCK(dset->dblk) ) EXRETURN ;

   for( ii=0 ; ii < DSET_NVALS(dset) ; ii++ ){
     if( DSET_ARRAY(dset,ii) == NULL ){
       vpt = calloc(1,DSET_BRICK_BYTES(dset,ii)) ;
       mri_fix_data_pointer( vpt , DSET_BRICK(dset,ii) ) ;
     }
   }
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/*! Apply master limits to data sub-bricks.                14 Apr 2006 [rickr]
    (from THD_load_datablock())

    return 0 on success
------------------------------------------------------------------------------*/
int THD_apply_master_subrange( THD_datablock * blk )
{
   THD_diskptr * dkptr ;
   float         bot = blk->master_bot , top = blk->master_top ;
   int           jbr, nv, ii, nxyz ;

ENTRY("THD_apply_master_limits") ;

   if( ! DBLK_IS_MASTERED(blk) || blk->master_bot > blk->master_top )
        RETURN(0);

   dkptr = blk->diskptr ;
   nv    = dkptr->nvals ;
   nxyz = dkptr->dimsizes[0] * dkptr->dimsizes[1] * dkptr->dimsizes[2];

   for( jbr=0 ; jbr < nv ; jbr++ ){
     switch( DBLK_BRICK_TYPE(blk,jbr) ){

        default:
           fprintf(stderr,
                   "** Can't sub-range datum type %s!\n",
                   MRI_TYPE_name[DBLK_BRICK_TYPE(blk,jbr)]) ;
           RETURN(1);
        break ;

        case MRI_short:{
           short mbot, mtop, *mar = (short *) DBLK_ARRAY(blk,jbr) ;
           float mfac = DBLK_BRICK_FACTOR(blk,jbr) ;
           if( mfac == 0.0 ) mfac = 1.0 ;
           mbot = SHORTIZE(bot/mfac) ; mtop = SHORTIZE(top/mfac) ;
#if 0
fprintf(stderr,"mbot=%d mtop=%d\n",(int)mbot,(int)mtop) ;
#endif
           for( ii=0 ; ii < nxyz ; ii++ )
              if( mar[ii] < mbot || mar[ii] > mtop ) mar[ii] = 0 ;
        }
        break ;

        case MRI_int:{
           int mbot, mtop, *mar = (int *) DBLK_ARRAY(blk,jbr) ;
           float mfac = DBLK_BRICK_FACTOR(blk,jbr) ;
           if( mfac == 0.0 ) mfac = 1.0 ;
           mbot = rint(bot/mfac) ; mtop = rint(top/mfac) ;
           for( ii=0 ; ii < nxyz ; ii++ )
              if( mar[ii] < mbot || mar[ii] > mtop ) mar[ii] = 0 ;
        }
        break ;

        case MRI_byte:{
           byte mbot, mtop, *mar = (byte *) DBLK_ARRAY(blk,jbr) ;
           float mfac = DBLK_BRICK_FACTOR(blk,jbr) ;
           if( mfac == 0.0 ) mfac = 1.0 ;
           mbot = BYTEIZE(bot/mfac) ; mtop = BYTEIZE(top/mfac) ;
           for( ii=0 ; ii < nxyz ; ii++ )
              if( mar[ii] < mbot || mar[ii] > mtop ) mar[ii] = 0 ;
        }
        break ;

        case MRI_float:{
           float mbot, mtop, *mar = (float *) DBLK_ARRAY(blk,jbr) ;
           float mfac = DBLK_BRICK_FACTOR(blk,jbr) ;
           if( mfac == 0.0 ) mfac = 1.0 ;
           mbot = (bot/mfac) ; mtop = (top/mfac) ;
           for( ii=0 ; ii < nxyz ; ii++ )
              if( mar[ii] < mbot || mar[ii] > mtop ) mar[ii] = 0 ;
        }
        break ;

        case MRI_complex:{
           float mbot, mtop , val ;
           complex *mar = (complex *) DBLK_ARRAY(blk,jbr) ;
           float mfac = DBLK_BRICK_FACTOR(blk,jbr) ;
           if( mfac == 0.0 ) mfac = 1.0 ;
           mbot = (bot/mfac) ; mtop = (top/mfac) ;
           for( ii=0 ; ii < nxyz ; ii++ ){
              val = CABS(mar[ii]) ;
              if( val < mbot || val > mtop ) mar[ii].r = mar[ii].i = 0 ;
           }
        }
        break ;
     }
  }

  RETURN(0) ;
}

/*----------------------------------------------------------------------*/
/*! Set dx,dy,dz fields for MRI_IMAGEs in a dataset.
------------------------------------------------------------------------*/

void THD_patch_brickim( THD_3dim_dataset *dset )
{
   float dx,dy,dz ;
   int iv , nvals ;

ENTRY("THD_patch_dxyz") ;

   if( !ISVALID_DSET(dset) ) EXRETURN ;

   dx = fabs(DSET_DX(dset)) ; if( dx == 0.0f ) dx = 1.0f ;
   dy = fabs(DSET_DY(dset)) ; if( dy == 0.0f ) dy = 1.0f ;
   dz = fabs(DSET_DZ(dset)) ; if( dz == 0.0f ) dz = 1.0f ;

   nvals = DSET_NVALS(dset) ;
   for( iv=0 ; iv < nvals ; iv++ ){
     DSET_BRICK(dset,iv)->dx = dx ;
     DSET_BRICK(dset,iv)->dy = dy ;
     DSET_BRICK(dset,iv)->dz = dz ;
   }

   EXRETURN ;
}
