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

/*---------------------------------------------------------------*/

Boolean THD_load_datablock( THD_datablock * blk , generic_func * freeup )
{
   THD_diskptr * dkptr ;
   int id , offset ;
   int nx,ny,nz , nxy,nxyz,nxyzv , nv,vv , ii , ntot , ibr , nbad ;
   char * ptr ;
   MRI_IMAGE * im ;

ENTRY("THD_load_datablock") ; /* 29 Aug 2001 */

   if( native_order < 0 ) native_order = mri_short_order() ;

   floatscan = AFNI_yesenv("AFNI_FLOATSCAN") ;

   if( floatscan ) no_mmap = 1 ;
   else            no_mmap = AFNI_yesenv("AFNI_NOMMAP") ;

   /*-- sanity checks --*/

   if( ! ISVALID_DATABLOCK(blk) || blk->brick == NULL ) RETURN( False );

   ii = THD_count_databricks( blk ) ;
   if( ii == blk->nvals ) RETURN( True );

   if( blk->malloc_type == DATABLOCK_MEM_UNDEFINED ) RETURN( False );

   dkptr = blk->diskptr ;
   if( ! ISVALID_DISKPTR(dkptr) || dkptr->storage_mode == STORAGE_UNDEFINED )
      RETURN( False );

   if( dkptr->rank != 3 ){
      fprintf(stderr,"\n*** Cannot read non 3D datablocks ***\n") ;
      RETURN( False );
   }

   /*-- allocate data space --*/

   nx = dkptr->dimsizes[0] ;
   ny = dkptr->dimsizes[1] ;  nxy   = nx * ny   ;
   nz = dkptr->dimsizes[2] ;  nxyz  = nxy * nz  ;
   nv = dkptr->nvals       ;  nxyzv = nxyz * nv ; ntot = blk->total_bytes ;

   if( DBLK_IS_MASTERED(blk) )                  /* 11 Jan 1999 */
      blk->malloc_type = DATABLOCK_MEM_MALLOC ;

   /* 25 April 1998: byte order issues */

   if( dkptr->byte_order <= 0 ) dkptr->byte_order = native_order ;

      /* 05 Jul 2001: if all sub-bricks are bytes,
                      mark dataset as being in native order */

   if( dkptr->byte_order != native_order ){
      for( ii=0 ; ii < nv ; ii++ )
         if( DBLK_BRICK_TYPE(blk,ii) != MRI_byte ) break ;
      if( ii == nv ) dkptr->byte_order = native_order ;
   }

   if( dkptr->byte_order != native_order || no_mmap )
      blk->malloc_type = DATABLOCK_MEM_MALLOC ;

   /** set up space for bricks via malloc **/

   if( blk->malloc_type == DATABLOCK_MEM_MALLOC ){

      /** malloc space for each brick separately **/

      for( nbad=ibr=0 ; ibr < nv ; ibr++ ){
         if( DBLK_ARRAY(blk,ibr) == NULL ){
            ptr = malloc( DBLK_BRICK_BYTES(blk,ibr) ) ;
            mri_fix_data_pointer( ptr ,  DBLK_BRICK(blk,ibr) ) ;
            if( ptr == NULL ) nbad++ ;
         }
      }

      if( nbad > 0 ){
         fprintf(stderr,
                "\n*** failed to malloc %d dataset bricks out of %d - is memory exhausted?\n",
                nbad,nv ) ;
#ifdef USING_MCW_MALLOC
         { char * str = mcw_malloc_status() ;
           if( str != NULL ) fprintf(stderr,"*** MCW_malloc summary: %s\n",str);}
#endif
         if( freeup != NULL ){  /* try to free some space? */
            freeup() ;
            for( ibr=0 ; ibr < nv ; ibr++ ){
               if( DBLK_ARRAY(blk,ibr) == NULL ){
                  ptr = malloc( DBLK_BRICK_BYTES(blk,ibr) ) ;
                  mri_fix_data_pointer( ptr ,  DBLK_BRICK(blk,ibr) ) ;
               }
            }
            if( THD_count_databricks(blk) < nv ){
               fprintf(stderr,"*** cannot free up enough memory\n") ;
#ifdef USING_MCW_MALLOC
               { char * str = mcw_malloc_status() ;
                 if( str != NULL ) fprintf(stderr,"*** MCW_malloc summary: %s\n",str);}
#endif
               RETURN( False );
            } else {
               fprintf(stderr,"*** was able to free up enough memory\n") ;
#ifdef USING_MCW_MALLOC
               { char * str = mcw_malloc_status() ;
                 if( str != NULL ) fprintf(stderr,"*** MCW_malloc summary: %s\n",str);}
#endif
            }
         } else {
            RETURN( False );
         }
      }

   /** mmap the whole file at once (makes space and reads it all at once) **/

   } else if( blk->malloc_type == DATABLOCK_MEM_MMAP ){
      int fd , fsize ;
      fd = open( dkptr->brick_name , O_RDONLY ) ;
      if( fd < 0 ){
         fprintf( stderr , "\n*** cannot open brick file %s for mmap\n"
                           "   - do you have permission? does file exist?\n" ,
                  dkptr->brick_name ) ;
         perror("*** Unix error message") ;
         RETURN( False );
      }

      /* 04 May 2001: check file size (the Katie Lee bug) */

      fsize = THD_filesize( dkptr->brick_name ) ;
      if( fsize < blk->total_bytes )
         fprintf(stderr ,
                 "\n*** WARNING: file %s size is %d, but should be at least %d!\n" ,
                 dkptr->brick_name , fsize , blk->total_bytes ) ;

      /* clear the sub-brick pointers */

      for( ibr=0 ; ibr < nv ; ibr++ )
         mri_clear_data_pointer( DBLK_BRICK(blk,ibr) ) ;

      /* map the file into memory */

      ptr = (char *) mmap( 0 , blk->total_bytes ,
                               PROT_READ , THD_MMAP_FLAG , fd , 0 ) ;

      /* if that fails, maybe try again */

      if( ptr == (char *)(-1) ){
         fprintf(stderr ,
                 "\n*** cannot mmap brick file %s - maybe hit a system limit?\n" ,
                 dkptr->brick_name ) ;
         perror("*** Unix error message") ;
         if( freeup != NULL ){
            freeup() ;
            ptr = (char *) mmap( 0 , blk->total_bytes ,
                                     PROT_READ , THD_MMAP_FLAG , fd , 0 ) ;
            if( ptr == (char *)(-1) ){
               fprintf(stderr,"*** cannot fix problem!\n") ;
               close(fd) ;
               RETURN( False );
            } else {
               fprintf(stderr,"*** was able to fix problem!\n") ;
            }
         } else {
            close(fd) ;
            RETURN( False );
         }
      }

      close(fd) ;  /* can close file after mmap-ing it */

      /* (re)create pointers to all sub-bricks */

      offset = 0 ;
      for( ibr=0 ; ibr < nv ; ibr++ ){
         mri_fix_data_pointer( ptr , DBLK_BRICK(blk,ibr) ) ;
         ptr += DBLK_BRICK_BYTES(blk,ibr) ;
      }

      RETURN( True );  /* finito */
   }

   /*** read data into newly malloc-ed bricks ***/

   switch( dkptr->storage_mode ){

      default:
         fprintf(stderr,"\n*** illegal storage mode in read ***\n") ;
         RETURN( False );
      break ;

     /*-- read brick file (there is no other option)--*/

      case STORAGE_BY_BRICK:{
         FILE * far ;

         far = COMPRESS_fopen_read( dkptr->brick_name ) ;

         if( far == NULL ){
            for( ibr=0 ; ibr < nv ; ibr++ ){
               free( DBLK_ARRAY(blk,ibr) ) ;
               mri_clear_data_pointer( DBLK_BRICK(blk,ibr) ) ;
            }
            fprintf(stderr,
                    "\n*** failure while opening brick file %s "
                    "- do you have permission?\n" ,
                    dkptr->brick_name ) ;
            perror("*** Unix error message") ;
            RETURN( False );
         }

         /* read each sub-brick all at once */

         id = 0 ;
         if( ! DBLK_IS_MASTERED(blk) ){      /* read each brick */

            for( ibr=0 ; ibr < nv ; ibr++ )
               id += fread( DBLK_ARRAY(blk,ibr), 1,
                            DBLK_BRICK_BYTES(blk,ibr), far ) ;

         } else {  /* 11 Jan 1999: read brick from master, put into place(s) */

            int nfilled = 0 , nbuf=0 , jbr, nbr ;
            char * buf=NULL ;

            /* loop over master sub-bricks until dataset is filled */

            for( ibr=0 ; nfilled < nv && ibr < blk->master_nvals ; ibr++ ){

               if( nbuf < blk->master_bytes[ibr] ){  /* make more space for it */
                  if( buf != NULL ) free(buf) ;
                  nbuf = blk->master_bytes[ibr] ;
                  buf  = malloc( sizeof(char) * nbuf ) ;
                  if( buf == NULL ) break ;
               }

               /* read the master sub-brick */

               nbr = fread( buf , 1 , blk->master_bytes[ibr] , far ) ;
               if( nbr < blk->master_bytes[ibr] ) break ;

               /* find all the dataset sub-bricks that are copies of this */

               for( jbr=0 ; jbr < nv ; jbr++ ){
                  if( blk->master_ival[jbr] == ibr ){  /* copy it in */
                     memcpy( DBLK_ARRAY(blk,jbr) , buf , blk->master_bytes[ibr] ) ;
                     nfilled++ ;  /* number of bricks filled */
                     id += nbr ;  /* number of bytes read into dataset */
                  }
               }
            }  /* end of loop over master sub-bricks */

            if( buf != NULL ) free(buf) ;
         }

         COMPRESS_fclose(far) ;

         if( id != blk->total_bytes ){
            for( ibr=0 ; ibr < nv ; ibr++ ){
               free( DBLK_ARRAY(blk,ibr) ) ;
               mri_clear_data_pointer( DBLK_BRICK(blk,ibr) ) ;
            }
            fprintf(stderr ,
                    "\n*** failure while reading from brick file %s\n"
                      "*** desired %d bytes but only got %d\n" ,
                    dkptr->brick_name , blk->total_bytes , id ) ;
            perror("*** Unix error message") ;
            RETURN( False );
         }

         /* 25 April 1998: check and fix byte ordering */

         if( dkptr->byte_order != native_order ){
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
               fprintf(stderr ,
                       "*** %s: found %d float errors -- see program float_scan\n" ,
                       dkptr->brick_name , nerr ) ;
         }

         /* 21 Feb 2001: if sub-ranging also used, clip values in brick */

#if 0
fprintf(stderr,"master_bot=%g master_top=%g\n",blk->master_bot,blk->master_top) ;
#endif
         if( DBLK_IS_MASTERED(blk) && blk->master_bot <= blk->master_top ){
            float bot = blk->master_bot , top = blk->master_top , fac ;
            int jbr ;

            for( jbr=0 ; jbr < nv ; jbr++ ){
               switch( DBLK_BRICK_TYPE(blk,jbr) ){

                  default:
                     fprintf(stderr,
                             "** Can't sub-range datum type %s!\n",
                             MRI_TYPE_name[DBLK_BRICK_TYPE(blk,jbr)]) ;
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
                     mbot = (mbot > 0) ? mbot*mbot : 0 ;
                     mtop = (mtop > 0) ? mtop*mtop : 0 ;
                     for( ii=0 ; ii < nxyz ; ii++ ){
                        val = CSQR(mar[ii]) ;
                        if( val < mbot || val > mtop ) mar[ii].r = mar[ii].i = 0 ;
                     }
                  }
                  break ;
               }
            }
         }

         RETURN( True );
      }
      break ;

   } /* end of switch on storage modes */

   RETURN( False );  /* should NEVER be reached */
}
