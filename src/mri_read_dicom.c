#include "mrilib.h"

/*-----------------------------------------------------------------------------------*/

#define NMOMAX 256
typedef struct {
  int good ;                       /* data in here is good? */
  int have_data[3] ;               /* do we have slices 0 and 1 in           *
				    * each dimension to determine z-spacing? *
				    * added 25 Feb 2003 KRH                  */
  int mosaic_num ;                 /* how many slices in 1 'image' */
  float slice_xyz[NMOMAX][3] ;     /* Sag, Cor, Tra coordinates */
} Siemens_extra_info ;

static char * extract_bytes_from_file( FILE *fp, off_t start, size_t len, int strize ) ;
static void get_siemens_extra_info( char *str , Siemens_extra_info *mi ) ;

/*-----------------------------------------------------------------------------------*/
/* Save the Siemens extra info string in case the caller wants to get it. */

static char *str_sexinfo=NULL ;

char * mri_dicom_sexinfo(void){ return str_sexinfo; }  /* 23 Dec 2002 */

/*-----------------------------------------------------------------------------------*/

static int LITTLE_ENDIAN_ARCHITECTURE = -1 ;

static void RWC_set_endianosity(void)
{
   union { unsigned char bb[2] ;
           short         ss    ; } fred ;

   if( LITTLE_ENDIAN_ARCHITECTURE < 0 ){
     fred.bb[0] = 1 ; fred.bb[1] = 0 ;

     LITTLE_ENDIAN_ARCHITECTURE = (fred.ss == 1) ;
   }
}

/*-----------------------------------------------------------------------------------*/

static char *elist[] = {

 "0018 0050" ,  /* Slice thickness */
 "0018 0080" ,  /* Repetition time */
 "0018 0088" ,  /* Spacing between slices */
 "0018 1149" ,  /* Field of view */

 "0020 0020" ,  /* Patient orientation */
 "0020 0032" ,  /* Image position (patient) */
 "0020 0037" ,  /* Image orientation (patient) */
 "0020 1041" ,  /* Slice location */

 "0028 0002" ,  /* Samples per pixel */
 "0028 0008" ,  /* Number of frames */
 "0028 0010" ,  /* Rows */
 "0028 0011" ,  /* Columns */
 "0028 0030" ,  /* Pixel spacing */
 "0028 0100" ,  /* Bits allocated */
 "0028 0101" ,  /* Bits stored */
 "0028 1052" ,  /* Rescale intercept */
 "0028 1053" ,  /* Rescale slope */
 "0028 1054" ,  /* Rescale type */
 "0028 0004" ,  /* Photometric interpretation */
 "0028 0103" ,  /* Pixel representation */
 "0028 0102" ,  /* High bit */
 "0028 1050" ,  /* Window center */
 "0028 1051" ,  /* Window width */

 "0008 0008" ,  /* ID Image type */
 "0008 0070" ,  /* ID Manufacturer */
 "0018 1310" ,  /* Acquisition Matrix */

 "0029 1010" ,  /* Siemens addendum #1 */
 "0029 1020" ,  /* Siemens addendum #2 */

NULL } ;

#define NUM_ELIST (sizeof(elist)/sizeof(char *)-1)

#define E_SLICE_THICKNESS             0
#define E_REPETITION_TIME             1
#define E_SLICE_SPACING               2
#define E_FIELD_OF_VIEW               3

#define E_PATIENT_ORIENTATION         4
#define E_IMAGE_POSITION              5
#define E_IMAGE_ORIENTATION           6
#define E_SLICE_LOCATION              7

#define E_SAMPLES_PER_PIXEL           8
#define E_NUMBER_OF_FRAMES            9
#define E_ROWS                       10
#define E_COLUMNS                    11
#define E_PIXEL_SPACING              12
#define E_BITS_ALLOCATED             13
#define E_BITS_STORED                14
#define E_RESCALE_INTERCEPT          15
#define E_RESCALE_SLOPE              16
#define E_RESCALE_TYPE               17
#define E_PHOTOMETRIC_INTERPRETATION 18
#define E_PIXEL_REPRESENTATION       19
#define E_HIGH_BIT                   20
#define E_WINDOW_CENTER              21
#define E_WINDOW_WIDTH               22

#define E_ID_IMAGE_TYPE              23    /* 28 Oct 2002: for Siemens mosaic */
#define E_ID_MANUFACTURER            24
#define E_ACQ_MATRIX                 25

#define E_SIEMENS_1                  26    /* 31 Oct 2002 */
#define E_SIEMENS_2                  27

/*-----------------------------------------------------------------------------------*/
/*! Read image(s) from a DICOM file, if possible.
-------------------------------------------------------------------------------------*/

MRI_IMARR * mri_read_dicom( char *fname )
{
   char *ppp , *ddd ;
   off_t poff ;
   unsigned int plen ;
   char *epos[NUM_ELIST] ;
   int ii,jj , ee , bpp , datum ;
   int nx,ny,nz , swap , shift=0 ;
   float dx,dy,dz,dt ;
   MRI_IMARR *imar ;
   MRI_IMAGE *im ;
   void *iar ;
   FILE *fp ;
   short sbot,stop ;
   int have_orients=0 ;
   int ior,jor,kor ;
   static int nzoff=0 ;   /* for determining z-axis orientation/offset from multiple calls */

   int mosaic=0 , mos_nx,mos_ny , mos_ix,mos_iy,mos_nz ;  /* 28 Oct 2002 */
   Siemens_extra_info sexinfo ;                           /* 31 Oct 2002 */
   float xcen,ycen,zcen ;
   int use_xycen=0 ;
   float dxx,dyy,dzz ;

   char *eee ;
   float rescale_slope=0.0 , rescale_inter=0.0 ;  /* 23 Dec 2002 */
   float window_center=0.0 , window_width =0.0 ;

ENTRY("mri_read_dicom") ;

   if( str_sexinfo != NULL ){ free(str_sexinfo); str_sexinfo=NULL; }

   if( fname == NULL || fname[0] == '\0' ) RETURN(NULL);

   /* extract header info from file into a string
      - cf. mri_dicom_hdr.[ch]
      - run 'dicom_hdr -noname fname' to see the string format */

   mri_dicom_nohex(1) ;              /* don't print ints in hex mode */
   mri_dicom_noname(1) ;             /* don't print names, just tags */
   ppp = mri_dicom_header( fname ) ; /* print header to malloc()-ed string */
   if( ppp == NULL ) RETURN(NULL);   /* didn't work; not a DICOM file? */

   /* find out where the pixel array is in the file */

   mri_dicom_pxlarr( &poff , &plen ) ;
   if( plen <= 0 ){ free(ppp) ; RETURN(NULL); }

   /* check if file is actually this big (maybe it was truncated) */

   { unsigned int psiz , fsiz ;
     fsiz = THD_filesize( fname ) ;
     psiz = (unsigned int)(poff) + plen ;
     if( fsiz < psiz ){ free(ppp) ; RETURN(NULL); }
   }

   /* find positions in header of elements we care about */

   for( ee=0 ; ee < NUM_ELIST ; ee++ )
     epos[ee] = strstr(ppp,elist[ee]) ;

   /* see if the header has the elements we absolutely need */

   if( epos[E_ROWS]           == NULL ||
       epos[E_COLUMNS]        == NULL ||
       epos[E_BITS_ALLOCATED] == NULL   ){ free(ppp) ; RETURN(NULL); }

   /* check if we have 1 sample per pixel (can't deal with 3 or 4 now) */

   if( epos[E_SAMPLES_PER_PIXEL] != NULL ){
      ddd = strstr(epos[E_SAMPLES_PER_PIXEL],"//") ;
      if( ddd == NULL ){ free(ppp) ; RETURN(NULL); }
      ii = 0 ; sscanf(ddd+2,"%d",&ii) ;
      if( ii != 1 ){ free(ppp) ; RETURN(NULL); }
   }

   /* check if photometric interpretation is MONOCHROME (don't like PALETTE) */

   if( epos[E_PHOTOMETRIC_INTERPRETATION] != NULL ){
      ddd = strstr(epos[E_PHOTOMETRIC_INTERPRETATION],"MONOCHROME") ;
      if( ddd == NULL ){ free(ppp) ; RETURN(NULL); }
   }

   /* check if we have 8, 16, or 32 bits per pixel */

   ddd = strstr(epos[E_BITS_ALLOCATED],"//") ;
   if( ddd == NULL ){ free(ppp); RETURN(NULL); }
   bpp = 0 ; sscanf(ddd+2,"%d",&bpp) ;
   switch( bpp ){
      default: free(ppp); RETURN(NULL);    /* bad value */
      case  8: datum = MRI_byte ; break ;
      case 16: datum = MRI_short; break ;
      case 32: datum = MRI_int  ; break ;  /* probably not present in DICOM? */
   }
   bpp /= 8 ; /* now bytes per pixel, instead of bits */

   /*** Print some warnings if appropriate ***/

   /* check if BITS_STORED and HIGH_BIT are aligned */

#define NWMAX 2
   if( epos[E_BITS_STORED] != NULL && epos[E_HIGH_BIT] != NULL ){
     int bs=0 , hb=0 ;
     ddd = strstr(epos[E_BITS_STORED],"//") ; sscanf(ddd+2,"%d",&bs) ;
     ddd = strstr(epos[E_HIGH_BIT],"//")    ; sscanf(ddd+2,"%d",&hb) ;
     if( bs != hb+1 ){
       static int nwarn=0 ;
       if( nwarn < NWMAX )
         fprintf(stderr,
                 "++ DICOM WARNING: file %s has Bits_Stored=%d and High_Bit=%d\n",
                 fname,bs,hb) ;
       if( nwarn == NWMAX )
         fprintf(stderr,"++ DICOM WARNING: no more Bits_Stored messages will be printed\n") ;
       nwarn++ ;
     }
   }

   /* check if Rescale is ordered */
   /* 23 Dec 2002: actually get the rescale params, if environment says to */

   eee = getenv("AFNI_DICOM_RESCALE") ;
   if( epos[E_RESCALE_INTERCEPT] != NULL && epos[E_RESCALE_SLOPE] != NULL ){
     if( eee == NULL || toupper(*eee) != 'Y' ){
       static int nwarn=0 ;
       if( nwarn < NWMAX )
         fprintf(stderr,
                 "++ DICOM WARNING: file %s has Rescale tags; setenv AFNI_DICOM_RESCALE YES to enforce them\n",
                 fname ) ;
       if( nwarn == NWMAX )
         fprintf(stderr,"++ DICOM WARNING: no more Rescale tags messages will be printed\n") ;
       nwarn++ ;
     } else {
       ddd = strstr(epos[E_RESCALE_INTERCEPT],"//") ; sscanf(ddd+2,"%f",&rescale_inter) ;
       ddd = strstr(epos[E_RESCALE_SLOPE    ],"//") ; sscanf(ddd+2,"%f",&rescale_slope) ;
     }
   }

   /* check if Window is ordered */
   /* 23 Dec 2002: actually get the window params, if environment says to */

   eee = getenv("AFNI_DICOM_WINDOW") ;
   if( epos[E_WINDOW_CENTER] != NULL && epos[E_WINDOW_WIDTH] != NULL ){
     if( eee == NULL || toupper(*eee) != 'Y' ){
       static int nwarn=0 ;
       if( nwarn < NWMAX )
         fprintf(stderr,
                 "++ DICOM WARNING: file %s has Window tags; setenv AFNI_DICOM_WINDOW YES to enforce them\n",
                 fname ) ;
       if( nwarn == NWMAX )
         fprintf(stderr,"++ DICOM WARNING: no more Window tags messages will be printed\n") ;
       nwarn++ ;
     } else {
       ddd = strstr(epos[E_WINDOW_CENTER],"//") ; sscanf(ddd+2,"%f",&window_center) ;
       ddd = strstr(epos[E_WINDOW_WIDTH ],"//") ; sscanf(ddd+2,"%f",&window_width ) ;
     }
   }

   /*** extract attributes of the image(s) to be read in ***/

   /* get image nx & ny */

   ddd = strstr(epos[E_ROWS],"//") ;                 /* 31 Oct 2002: */
   if( ddd == NULL ){ free(ppp) ; RETURN(NULL); }    /* Oops: ROWS is ny and */
   ny = 0 ; sscanf(ddd+2,"%d",&ny) ;                 /*       COLUMNS is nx! */
   if( ny < 2 ){ free(ppp) ; RETURN(NULL); }

   ddd = strstr(epos[E_COLUMNS],"//") ;
   if( ddd == NULL ){ free(ppp) ; RETURN(NULL); }
   nx = 0 ; sscanf(ddd+2,"%d",&nx) ;
   if( nx < 2 ){ free(ppp) ; RETURN(NULL); }

   /* get number of slices */

   nz = 0 ;
   if( epos[E_NUMBER_OF_FRAMES] != NULL ){
     ddd = strstr(epos[E_NUMBER_OF_FRAMES],"//") ;
     if( ddd != NULL ) sscanf(ddd+2,"%d",&nz) ;
   }

   /* if didn't get nz above, make up a value */

   if( nz == 0 ) nz = plen / (bpp*nx*ny) ;    /* compute from image array size */
   if( nz == 0 ){ free(ppp) ; RETURN(NULL); }

   /*-- 28 Oct 2002: Check if this is a Siemens mosaic.        --*/
   /*-- 02 Dec 2002: Don't use Acquisition Matrix anymore;
                     instead, use the Siemens extra info
                     in epos[E_SIEMENS_2].                     --*/
   /*-- 24 Dec 2002: Extract str_sexinfo even if not a mosaic. --*/

   if(        epos[E_ID_MANUFACTURER]            != NULL &&
       strstr(epos[E_ID_MANUFACTURER],"SIEMENS") != NULL &&
              epos[E_SIEMENS_2]                  != NULL    ){

     int len=0,loc=0 , aa,bb ;
     sscanf(epos[E_SIEMENS_2],"%x%x%d [%d" , &aa,&bb , &len,&loc ) ;
     if( len > 0 && loc > 0 ){
       fp = fopen( fname , "rb" ) ;
       if( fp != NULL ){
         str_sexinfo = extract_bytes_from_file( fp, (off_t)loc, (size_t)len, 1 ) ;
         fclose(fp) ;
       }
     }
   }

   /*-- process str_sexinfo only if this is marked as a mosaic image --*/

   if(        epos[E_ID_IMAGE_TYPE]              != NULL &&
       strstr(epos[E_ID_IMAGE_TYPE],"MOSAIC")    != NULL &&
       str_sexinfo                               != NULL   ){

     /* 31 Oct 2002: extract extra Siemens info from str_sexinfo */

     sexinfo.good = 0 ;  /* start by marking it as bad */
     for(ii = 0; ii < 3; ii++) sexinfo.have_data[ii] = 0; /* 25 Feb 03 Initialize new member KRH */

     get_siemens_extra_info( str_sexinfo , &sexinfo ) ;

     if( sexinfo.good ){                                   /* if data is good */

       /* compute size of mosaic layout
          as 1st integer whose square is >= # of images in mosaic */

       for( mos_ix=1 ; mos_ix*mos_ix < sexinfo.mosaic_num ; mos_ix++ ) ; /* nada */
       mos_iy = mos_ix ;

       mos_nx = nx / mos_ix ; mos_ny = ny / mos_iy ;  /* sub-image dimensions */

       if( mos_ix*mos_nx == nx &&               /* Sub-images must fit nicely */
           mos_iy*mos_ny == ny    ){            /* into super-image layout.   */

           static int nwarn=0 ;

           /* should be tagged as a 1 slice image thus far */

           if( nz > 1 ){
             fprintf(stderr,
                     "** DICOM ERROR: %dx%d Mosaic of %dx%d images in file %s, but also have nz=%d\n",
                     mos_ix,mos_iy,mos_nx,mos_ny,fname,nz) ;
             free(ppp) ; RETURN(NULL) ;
           }

           /* mark as a mosaic */

           mosaic = 1 ;
           mos_nz = mos_ix * mos_iy ;   /* number of slices in mosaic */
           if( nwarn < NWMAX )
             fprintf(stderr,"++ DICOM NOTICE: %dx%d Siemens Mosaic of %d %dx%d images in file %s\n",
                    mos_ix,mos_iy,sexinfo.mosaic_num,mos_nx,mos_ny,fname) ;
           if( nwarn == NWMAX )
             fprintf(stderr,"++ DICOM NOTICE: no more Siemens Mosiac messages will be printed\n") ;
           nwarn++ ;

       } /* end of if mosaic sizes are reasonable */

       else {                        /* warn about bad mosaic sizes */
         static int nwarn=0 ;
         if( nwarn < NWMAX )
           fprintf(stderr,
                   "++ DICOM WARNING: bad Siemens Mosaic params: nx=%d ny=%d ix=%d iy=%d imx=%d imy=%d\n",
                   mos_nx,mos_ny , mos_ix,mos_iy , nx,ny ) ;
         if( nwarn == NWMAX )
           fprintf(stderr,"++ DICOM NOTICE: no more Siemens Mosaic param messages will be printed\n");
         nwarn++ ;
       }

     } /* end of if sexinfo was good */

     else {                  /* warn if sexinfo was bad */
       static int nwarn=0 ;
       if( nwarn < NWMAX )
         fprintf(stderr,"++ DICOM WARNING: indecipherable Siemens Mosaic info (%s) in file %s\n",
                 elist[E_SIEMENS_2] , fname ) ;
       if( nwarn == NWMAX )
         fprintf(stderr,"++ DICOM NOTICE: no more Siemens Mosaic info messages will be printed\n");
       nwarn++ ;
     }

   } /* end of if a Siemens mosaic */

   /*-- try to get dx, dy, dz, dt --*/

   dx = dy = dz = dt = 0.0 ;

   /* dx,dy first */

   if( epos[E_PIXEL_SPACING] != NULL ){
     ddd = strstr(epos[E_PIXEL_SPACING],"//") ;
     if( ddd != NULL ) sscanf( ddd+2 , "%f\\%f" , &dx , &dy ) ;
     if( dy == 0.0 && dx > 0.0 ) dy = dx ;
   }
   if( dx == 0.0 && epos[E_FIELD_OF_VIEW] != NULL ){
     ddd = strstr(epos[E_FIELD_OF_VIEW],"//") ;
     if( ddd != NULL ) sscanf( ddd+2 , "%f\\%f" , &dx , &dy ) ;
     if( dx > 0.0 ){
       if( dy == 0.0 ) dy = dx ;
       dx /= nx ; dy /= ny ;
     }
   }

   /*-- 27 Nov 2002: fix stupid GE error,
                     where the slice spacing is really the slice gap --*/

   { int stupid_ge_fix , no_stupidity ;
     float sp=0.0 , th=0.0 ;
     static int nwarn=0 ;

     eee           = getenv("AFNI_SLICE_SPACING_IS_GAP") ;
     stupid_ge_fix = (eee != NULL && (*eee=='Y' || *eee=='y') ) ;
     no_stupidity  = (eee != NULL && (*eee=='N' || *eee=='y') ) ;  /* 03 Mar 2003 */

     if( epos[E_SLICE_SPACING] != NULL ){                  /* get reported slice spacing */
       ddd = strstr(epos[E_SLICE_SPACING],"//") ;
       if( ddd != NULL ) sscanf( ddd+2 , "%f" , &sp ) ;
     }
     if( epos[E_SLICE_THICKNESS] != NULL ){                /* get reported slice thickness */
       ddd = strstr(epos[E_SLICE_THICKNESS],"//") ;
       if( ddd != NULL ) sscanf( ddd+2 , "%f" , &th ) ;
     }

     th = fabs(th) ; sp = fabs(sp) ;                       /* we don't use the sign */

     if( stupid_ge_fix ){                                  /* always be stupid */
       dz = sp+th ;
     } else {
       dz = (sp > th) ? sp : th ;                          /* the correct DICOM way */

#define GFAC 0.99

       if( !no_stupidity ){                                /* unless stupidity is turned off */
         if( sp > 0.0 && sp < GFAC*th ) dz = sp+th ;       /* the stupid GE way again */

         if( sp > 0.0 && sp < GFAC*th && nwarn < NWMAX ){
           fprintf(stderr,
                   "++ DICOM WARNING: file %s has Slice_Spacing=%f smaller than Slice_Thickness=%f\n",
                   fname , sp , th ) ;
           if( nwarn == 0 )
            fprintf(stderr,
              "\n"
              "++  Setting environment variable AFNI_SLICE_SPACING_IS_GAP       ++\n"
              "++   to YES will make the center-to-center slice distance        ++\n"
              "++   be set to Slice_Spacing+Slice_Thickness=%6.3f.             ++\n"
              "++  This is against the DICOM standard [attribute (0018,0088)    ++\n"
              "++   is defined as the center-to-center spacing between slices,  ++\n"
              "++   NOT as the edge-to-edge gap between slices], but it seems   ++\n"
              "++   to be necessary for some GE scanners.                       ++\n"
              "++                                                               ++\n"
              "++  This correction has been made on this data: dz=%6.3f.       ++\n"
              "\n\a" ,
             sp+th , dz ) ;
         }
         if( sp > 0.0 && sp < th && nwarn == NWMAX )
           fprintf(stderr,"++ DICOM WARNING: no more Slice_Spacing messages will be printed\n") ;
         nwarn++ ;
       }
     }
     if( dz == 0.0 && dx != 0.0 ) dz = 1.0 ;               /* nominal dz */

   } /*-- end of dz code, with all its stupidities --*/

   /* get dt */

   if( epos[E_REPETITION_TIME] != NULL ){
     ddd = strstr(epos[E_REPETITION_TIME],"//") ;
     if( ddd != NULL ){
       sscanf( ddd+2 , "%f" , &dt ) ;
       dt *= 0.001 ;   /* ms to s */
     }
   }

   /* check if we might have 16 bit unsigned data that fills all bits */

#if 0
   if( bpp == 2 ){
     if( epos[E_PIXEL_REPRESENTATION] != NULL ){
       ddd = strstr(epos[E_PIXEL_REPRESENTATION],"//") ;
       if( ddd != NULL ){
         ii = 0 ; sscanf( ddd+2 , "%d" , &ii ) ;
         if( ii == 1 ) shift = -1 ;
       }
     }
     if( shift == 0 && epos[E_HIGH_BIT] != NULL ){
       ddd = strstr(epos[E_HIGH_BIT],"//") ;
       if( ddd != NULL ){
         ii = 0 ; sscanf( ddd+2 , "%d" , &ii ) ;
         if( ii == 15 ) shift = 1 ;
       }
     }
     sbot = 32767 ; stop = -32767 ;
   }
#endif

   /** Finally! Read images from file. **/

   fp = fopen( fname , "rb" ) ;
   if( fp == NULL ){ free(ppp) ; RETURN(NULL); }
   lseek( fileno(fp) , poff , SEEK_SET ) ;

   INIT_IMARR(imar) ;

   /* DICOM files are stored in LSB first (little endian) mode */

   RWC_set_endianosity() ; swap = !LITTLE_ENDIAN_ARCHITECTURE ;

   /* 28 Oct 2002: must allow for 2D mosaic mode */

   if( !mosaic ){   /*-- 28 Oct 2002: old method, not a mosaic --*/

    for( ii=0 ; ii < nz ; ii++ ){
      im = mri_new( nx , ny , datum ) ;    /* new MRI_IMAGE struct */
      iar = mri_data_pointer( im ) ;       /* data array in struct */
      fread( iar , bpp , nx*ny , fp ) ;    /* read data directly into it */

      if( swap ){                          /* swap bytes? */
        switch( im->pixel_size ){
          default: break ;
          case 2:  swap_twobytes (   im->nvox, iar ) ; break ;  /* short */
          case 4:  swap_fourbytes(   im->nvox, iar ) ; break ;  /* int, float */
          case 8:  swap_fourbytes( 2*im->nvox, iar ) ; break ;  /* complex */
        }
        im->was_swapped = 1 ;
      }

#if 0
      if( shift == 1 ){
        switch( datum ){
          case MRI_short:{
            short * sar = (short *) iar ;
            for( jj=0 ; jj < im->nvox ; jj++ ){
              sbot = MIN( sar[jj] , sbot ) ;
              stop = MAX( sar[jj] , stop ) ;
            }
          }
          break ;
        }
      }
#endif

      /* store auxiliary data in image struct */

      if( dx > 0.0 && dy > 0.0 && dz > 0.0 ){
        im->dx = dx; im->dy = dy; im->dz = dz; im->dw = 1.0;
      }
      if( dt > 0.0 ) im->dt = dt ;

      ADDTO_IMARR(imar,im) ;
    }

   } else {   /*-- 28 Oct 2002:  is a 2D mosaic --*/

     char *dar , *iar ;
     int last_ii=-1 , nvox , yy,xx,nxx ;

     nvox = mos_nx*mos_ny*mos_nz ;         /* total number of voxels */
     dar  = calloc(bpp,nvox) ;            /* make space for super-image */
     fread( dar , bpp , nvox , fp ) ;    /* read data directly into it */
     if( swap ){                        /* swap bytes? */
       switch( bpp ){
         default: break ;
         case 2:  swap_twobytes (   nvox, dar ) ; break ;  /* short */
         case 4:  swap_fourbytes(   nvox, dar ) ; break ;  /* int, float */
         case 8:  swap_fourbytes( 2*nvox, dar ) ; break ;  /* complex */
       }
     }

     /* load data from dar into images */

     nxx = mos_nx * mos_ix ;              /* # pixels per mosaic line */

     for( yy=0 ; yy < mos_iy ; yy++ ){    /* loop over sub-images in mosaic */
       for( xx=0 ; xx < mos_ix ; xx++ ){

         im = mri_new( mos_nx , mos_ny , datum ) ;
         iar = mri_data_pointer( im ) ;             /* sub-image array */

         /* copy data rows from dar into iar */

         for( jj=0 ; jj < mos_ny ; jj++ )  /* loop over rows inside sub-image */
           memcpy( iar + jj*mos_nx*bpp ,
                   dar + xx*mos_nx*bpp + (jj+yy*mos_ny)*nxx*bpp ,
                   mos_nx*bpp                                    ) ;

         if( dx > 0.0 && dy > 0.0 && dz > 0.0 ){
           im->dx = dx; im->dy = dy; im->dz = dz; im->dw = 1.0;
         }
         if( dt > 0.0 ) im->dt = dt ;
         if( swap ) im->was_swapped = 1 ;

         ADDTO_IMARR(imar,im) ;
       }
     }
     free(dar) ;  /* don't need no more; copied all data out of it now */

     /* truncate zero images out of tail of mosaic */

     if( sexinfo.good ){  /* the new way: use the mosaic count from Siemens extra info */

       if( sexinfo.mosaic_num < IMARR_COUNT(imar) )
         TRUNCATE_IMARR(imar,sexinfo.mosaic_num) ;

     } else {  /* the old way: find the last image with a nonzero value inside */

       for( ii=mos_nz-1 ; ii >= 0 ; ii-- ){  /* loop backwards over images */
         im = IMARR_SUBIM(imar,ii) ;
         switch( im->kind ){
           case MRI_short:{
             short *ar = mri_data_pointer( im ) ;
             for( jj=0 ; jj < im->nvox ; jj++ ) if( ar[jj] != 0 ) break ;
             if( jj < im->nvox ) last_ii = ii ;
           }
           break ;

           case MRI_byte:{
             byte *ar = mri_data_pointer( im ) ;
             for( jj=0 ; jj < im->nvox ; jj++ ) if( ar[jj] != 0 ) break ;
             if( jj < im->nvox ) last_ii = ii ;
           }
           break ;

           case MRI_int:{
             int *ar = mri_data_pointer( im ) ;
             for( jj=0 ; jj < im->nvox ; jj++ ) if( ar[jj] != 0 ) break ;
             if( jj < im->nvox ) last_ii = ii ;
           }
           break ;
         }
         if( last_ii >= 0 ) break ;
       }

       if( last_ii <= 0 ) last_ii = 1 ;
       if( last_ii+1 < IMARR_COUNT(imar) ) TRUNCATE_IMARR(imar,last_ii+1) ;

     } /* end of truncating off all zero images at end */

#if 0
fprintf(stderr,"\nmri_read_dicom Mosaic: mos_nx=%d mos_ny=%d mos_ix=%d mos_iy=%d slices=%d\n",
mos_nx,mos_ny,mos_ix,mos_iy,IMARR_COUNT(imar)) ;
MCHECK ;
#endif

   } /* end of mosaic input mode */

   fclose(fp) ;     /* 10 Sep 2002: oopsie - forgot to close file */

   /*-- 23 Dec 2002: implement Rescale, if ordered --*/

   if( rescale_slope > 0.0 ){
     for( ii=0 ; ii < IMARR_COUNT(imar) ; ii++ ){
       im = IMARR_SUBIM(imar,ii) ;
       switch( im->kind ){
         case MRI_byte:{
           byte *ar = mri_data_pointer( im ) ;
           for( jj=0 ; jj < im->nvox ; jj++ )
             ar[jj] = rescale_slope*ar[jj] + rescale_inter ;
         }
         break ;

         case MRI_short:{
           short *ar = mri_data_pointer( im ) ;
           for( jj=0 ; jj < im->nvox ; jj++ )
             ar[jj] = rescale_slope*ar[jj] + rescale_inter ;
         }
         break ;

         case MRI_int:{
           int *ar = mri_data_pointer( im ) ;
           for( jj=0 ; jj < im->nvox ; jj++ )
             ar[jj] = rescale_slope*ar[jj] + rescale_inter ;
         }
         break ;
       }
     }
   } /* end of Rescale */

   /*-- 23 Dec 2002: implement Window, if ordered --*/
   /*                section C.11.2.1.2 (page 503)  */

   if( window_width >= 1.0 ){
     float wbot,wtop,wfac ;
     int ymax=0 ;

     /* get output range */

     ddd = strstr(epos[E_BITS_STORED],"//") ;
     if( ddd != NULL ){
       ymax = 0 ; sscanf(ddd+2,"%d",&ymax) ;
       if( ymax > 0 ) ymax = (1 << ymax) - 1 ;
     }
     if( ymax <= 0 ){
       switch( IMARR_SUBIM(imar,0)->kind ){
         case MRI_byte:  ymax = MRI_maxbyte  ; break ;
         case MRI_short: ymax = MRI_maxshort ; break ;
         case MRI_int:   ymax = MRI_maxint   ; break ;
       }
     }
                                          /** window_width == 1 is special **/
     if( window_width == 1.0 ){           /** binary threshold case **/

       wbot = window_center - 0.5 ;       /* the threshold */

       for( ii=0 ; ii < IMARR_COUNT(imar) ; ii++ ){
         im = IMARR_SUBIM(imar,ii) ;
         switch( im->kind ){
           case MRI_byte:{
             byte *ar = mri_data_pointer( im ) ;
             for( jj=0 ; jj < im->nvox ; jj++ )
               ar[jj] = (ar[jj] <= wbot) ? 0 : ymax ;
           }
           break ;
  
           case MRI_short:{
             short *ar = mri_data_pointer( im ) ;
             for( jj=0 ; jj < im->nvox ; jj++ )
               ar[jj] = (ar[jj] <= wbot) ? 0 : ymax ;
           }
           break ;
  
           case MRI_int:{
             int *ar = mri_data_pointer( im ) ;
             for( jj=0 ; jj < im->nvox ; jj++ )
               ar[jj] = (ar[jj] <= wbot) ? 0 : ymax ;
           }
           break ;
         }
       }

     } else {                             /** linear windowing case **/

       wbot = (window_center - 0.5) - 0.5*(window_width-1.0) ;
       wtop = (window_center - 0.5) + 0.5*(window_width-1.0) ;
       wfac = ymax                  /     (window_width-1.0) ;

       for( ii=0 ; ii < IMARR_COUNT(imar) ; ii++ ){
         im = IMARR_SUBIM(imar,ii) ;
         switch( im->kind ){
           case MRI_byte:{
             byte *ar = mri_data_pointer( im ) ;
             for( jj=0 ; jj < im->nvox ; jj++ )
               ar[jj] = (ar[jj] <= wbot) ? 0
                       :(ar[jj] >  wtop) ? ymax
                                         : wfac*(ar[jj]-wbot)+0.499 ;
           }
           break ;

           case MRI_short:{
             short *ar = mri_data_pointer( im ) ;
             for( jj=0 ; jj < im->nvox ; jj++ )
               ar[jj] = (ar[jj] <= wbot) ? 0
                       :(ar[jj] >  wtop) ? ymax
                                         : wfac*(ar[jj]-wbot)+0.499 ;
           }
           break ;

           case MRI_int:{
             int *ar = mri_data_pointer( im ) ;
             for( jj=0 ; jj < im->nvox ; jj++ )
               ar[jj] = (ar[jj] <= wbot) ? 0
                       :(ar[jj] >  wtop) ? ymax
                                         : wfac*(ar[jj]-wbot)+0.499 ;
           }
           break ;
         }
       }
     }
   } /* end of Window */

   /*-- store some extra information in MRILIB globals, too? --*/

   if( dt > 0.0 && MRILIB_tr <= 0.0 ) MRILIB_tr = dt ;  /* TR */

   /*-- try to get image orientation fields (also, set ior,jor,kor) --*/

   if( epos[E_IMAGE_ORIENTATION] != NULL ){    /* direction cosines of image plane */

     ddd = strstr(epos[E_IMAGE_ORIENTATION],"//") ;
     if( ddd != NULL ){
       float xc1=0.0,xc2=0.0,xc3=0.0 , yc1=0.0,yc2=0.0,yc3=0.0 ;
       float xn,yn ; int qq ;
       qq = sscanf(ddd+2,"%f\\%f\\%f\\%f\\%f\\%f",&xc1,&xc2,&xc3,&yc1,&yc2,&yc3) ;
       xn = sqrt( xc1*xc1 + xc2*xc2 + xc3*xc3 ) ; /* vector norms */
       yn = sqrt( yc1*yc1 + yc2*yc2 + yc3*yc3 ) ;
       if( qq == 6 && xn > 0.0 && yn > 0.0 ){     /* both vectors OK */

         xc1 /= xn ; xc2 /= xn ; xc3 /= xn ;      /* normalize vectors */
         yc1 /= yn ; yc2 /= yn ; yc3 /= yn ;

         if( !use_MRILIB_xcos ){
           MRILIB_xcos[0] = xc1 ; MRILIB_xcos[1] = xc2 ;  /* save direction */
           MRILIB_xcos[2] = xc3 ; use_MRILIB_xcos = 1 ;   /* cosine vectors */
         }

         if( !use_MRILIB_ycos ){
           MRILIB_ycos[0] = yc1 ; MRILIB_ycos[1] = yc2 ;
           MRILIB_ycos[2] = yc3 ; use_MRILIB_ycos = 1 ;
         }

         /* x-axis orientation */
         /* ior determines which spatial direction is x-axis  */
         /* and is the direction that has the biggest change */

         dxx = fabs(xc1) ; ior = 1 ;
         dyy = fabs(xc2) ; if( dyy > dxx ){ ior=2; dxx=dyy; }
         dzz = fabs(xc3) ; if( dzz > dxx ){ ior=3;        }
         dxx = MRILIB_xcos[ior-1] ; if( dxx < 0. ) ior = -ior;

         if( MRILIB_orients[0] == '\0' ){
           switch( ior ){
             case -1: MRILIB_orients[0] = 'L'; MRILIB_orients[1] = 'R'; break;
             case  1: MRILIB_orients[0] = 'R'; MRILIB_orients[1] = 'L'; break;
             case -2: MRILIB_orients[0] = 'P'; MRILIB_orients[1] = 'A'; break;
             case  2: MRILIB_orients[0] = 'A'; MRILIB_orients[1] = 'P'; break;
             case  3: MRILIB_orients[0] = 'I'; MRILIB_orients[1] = 'S'; break;
             case -3: MRILIB_orients[0] = 'S'; MRILIB_orients[1] = 'I'; break;
             default: MRILIB_orients[0] ='\0'; MRILIB_orients[1] ='\0'; break;
           }
         }

         /* y-axis orientation */
         /* jor determines which spatial direction is y-axis  */
         /* and is the direction that has the biggest change */

         dxx = fabs(yc1) ; jor = 1 ;
         dyy = fabs(yc2) ; if( dyy > dxx ){ jor=2; dxx=dyy; }
         dzz = fabs(yc3) ; if( dzz > dxx ){ jor=3;        }
         dyy = MRILIB_ycos[jor-1] ; if( dyy < 0. ) jor = -jor;
         if( MRILIB_orients[2] == '\0' ){
           switch( jor ){
             case -1: MRILIB_orients[2] = 'L'; MRILIB_orients[3] = 'R'; break;
             case  1: MRILIB_orients[2] = 'R'; MRILIB_orients[3] = 'L'; break;
             case -2: MRILIB_orients[2] = 'P'; MRILIB_orients[3] = 'A'; break;
             case  2: MRILIB_orients[2] = 'A'; MRILIB_orients[3] = 'P'; break;
             case  3: MRILIB_orients[2] = 'I'; MRILIB_orients[3] = 'S'; break;
             case -3: MRILIB_orients[2] = 'S'; MRILIB_orients[3] = 'I'; break;
             default: MRILIB_orients[2] ='\0'; MRILIB_orients[3] ='\0'; break;
           }
         }

         MRILIB_orients[6] = '\0' ;   /* terminate orientation string */

         kor = 6 - abs(ior)-abs(jor) ;   /* which spatial direction is z-axis */
                                         /* where 1=L-R, 2=P-A, 3=I-S */
         have_orients = 1 ;
#if 0
fprintf(stderr,"MRILIB_orients=%s (from IMAGE_ORIENTATION)\n",MRILIB_orients) ;
#endif
       }
     }

   } else if( epos[E_PATIENT_ORIENTATION] != NULL ){  /* symbolic orientation of image */
                                                      /* [not so useful, or common] */
     ddd = strstr(epos[E_PATIENT_ORIENTATION],"//") ;
     if( ddd != NULL ){
       char xc='\0' , yc='\0' ;
       sscanf(ddd+2,"%c\\%c",&xc,&yc) ;   /* e.g., "L\P" */
       switch( toupper(xc) ){
         case 'L': MRILIB_orients[0] = 'L'; MRILIB_orients[1] = 'R'; ior=-1; break;
         case 'R': MRILIB_orients[0] = 'R'; MRILIB_orients[1] = 'L'; ior= 1; break;
         case 'P': MRILIB_orients[0] = 'P'; MRILIB_orients[1] = 'A'; ior=-2; break;
         case 'A': MRILIB_orients[0] = 'A'; MRILIB_orients[1] = 'P'; ior= 2; break;
         case 'F': MRILIB_orients[0] = 'I'; MRILIB_orients[1] = 'S'; ior= 3; break;  /* F = foot */
         case 'H': MRILIB_orients[0] = 'S'; MRILIB_orients[1] = 'I'; ior=-3; break;  /* H = head */
         default:  MRILIB_orients[0] ='\0'; MRILIB_orients[1] ='\0'; ior= 0; break;
       }
       switch( toupper(yc) ){
         case 'L': MRILIB_orients[2] = 'L'; MRILIB_orients[3] = 'R'; jor=-1; break;
         case 'R': MRILIB_orients[2] = 'R'; MRILIB_orients[3] = 'L'; jor= 1; break;
         case 'P': MRILIB_orients[2] = 'P'; MRILIB_orients[3] = 'A'; jor=-2; break;
         case 'A': MRILIB_orients[2] = 'A'; MRILIB_orients[3] = 'P'; jor= 2; break;
         case 'F': MRILIB_orients[2] = 'I'; MRILIB_orients[3] = 'S'; jor= 3; break;
         case 'H': MRILIB_orients[2] = 'S'; MRILIB_orients[3] = 'I'; jor=-3; break;
         default:  MRILIB_orients[2] ='\0'; MRILIB_orients[3] ='\0'; jor= 0; break;
       }
       MRILIB_orients[6] = '\0' ;      /* terminate orientation string */
       kor = 6 - abs(ior)-abs(jor) ;   /* which spatial direction is z-axis */
       have_orients = (ior != 0 && jor != 0) ;
     }

   }  /* end of 2D image orientation */

   /*-- try to get image offset (position), if have orientation from above --*/

   if( nzoff == 0 && have_orients && mosaic && sexinfo.good ){  /* 01 Nov 2002: use Siemens mosaic info */
     int qq ;
     float z0, z1 ;
     /* 25 Feb 2003 changing error checking for mosaics missing one or more *
      * dimension of slice coordinates                                 KRH  */
     if (sexinfo.have_data[kor-1]) {
       z0 = sexinfo.slice_xyz[0][kor-1] ;   /* kor from orients above */
       z1 = sexinfo.slice_xyz[1][kor-1] ;   /* z offsets of 1st 2 slices */
     } else {                  /* warn if sexinfo was bad */
       static int nwarn=0 ;
       if( nwarn < NWMAX )
         fprintf(stderr,"++ DICOM WARNING: Unusable coord. in Siemens Mosaic info (%s) in file %s\n",
                 elist[E_SIEMENS_2] , fname ) ;
       if( nwarn == NWMAX )
         fprintf(stderr,"++ DICOM NOTICE: no more Siemens Mosaic info messages will be printed\n");
       nwarn++ ;
     }


#if 0
     /* Save x,y center of this 1st slice */

     xcen = sexinfo.slice_xyz[0][abs(ior)-1] ;
     ycen = sexinfo.slice_xyz[0][abs(jor)-1] ; use_xycen = 1 ;
#endif

     /* finish z orientation now */

     if( z1-z0 < 0.0 ) kor = -kor ;     /* reversed orientation */
     if( MRILIB_orients[4] == '\0' ){
       switch( kor ){
         case  1: MRILIB_orients[4] = 'R'; MRILIB_orients[5] = 'L'; break;
         case -1: MRILIB_orients[4] = 'L'; MRILIB_orients[5] = 'R'; break;
         case  2: MRILIB_orients[4] = 'A'; MRILIB_orients[5] = 'P'; break;
         case -2: MRILIB_orients[4] = 'P'; MRILIB_orients[5] = 'A'; break;
         case  3: MRILIB_orients[4] = 'I'; MRILIB_orients[5] = 'S'; break;
         case -3: MRILIB_orients[4] = 'S'; MRILIB_orients[5] = 'I'; break;
         default: MRILIB_orients[4] ='\0'; MRILIB_orients[5] ='\0'; break;
       }
     }
     MRILIB_orients[6] = '\0' ;

#if 0
fprintf(stderr,"z0=%f z1=%f kor=%d MRILIB_orients=%s\n",z0,z1,kor,MRILIB_orients) ;
#endif

     MRILIB_zoff = z0 ; use_MRILIB_zoff = 1 ;
     if( kor > 0 ) MRILIB_zoff = -MRILIB_zoff ;
   }

   /** use image position vector to set offsets,
       and (2cd time in) the z-axis orientation **/

   if( nzoff < 2 && epos[E_IMAGE_POSITION] != NULL && have_orients ){
     ddd = strstr(epos[E_IMAGE_POSITION],"//") ;
     if( ddd != NULL ){
       float xyz[3] ; int qq ;
       qq = sscanf(ddd+2,"%f\\%f\\%f",xyz,xyz+1,xyz+2) ;
       if( qq == 3 ){
         static float zoff ;      /* saved from nzoff=0 case */
         float zz = xyz[kor-1] ;  /* kor from orients above */

#if 0
fprintf(stderr,"IMAGE_POSITION=%f %f %f  kor=%d\n",xyz[0],xyz[1],xyz[2],kor) ;
#endif

         if( nzoff == 0 ){  /* 1st DICOM image */

           zoff = zz ;      /* save this for 2nd image calculation */

           /* 01 Nov 2002: in mosaic case, may have set this already */

           if( MRILIB_orients[4] == '\0' ){
             switch( kor ){   /* may be changed on second image */
               case  1: MRILIB_orients[4] = 'L'; MRILIB_orients[5] = 'R'; break;
               case  2: MRILIB_orients[4] = 'P'; MRILIB_orients[5] = 'A'; break;
               case  3: MRILIB_orients[4] = 'I'; MRILIB_orients[5] = 'S'; break;
               default: MRILIB_orients[4] ='\0'; MRILIB_orients[5] ='\0'; break;
             }
             MRILIB_orients[6] = '\0' ;
           }

           /* Save x,y offsets of this 1st slice */

           qq = abs(ior) ;
           MRILIB_xoff = xyz[qq-1] ; use_MRILIB_xoff = 1 ;
           if( ior > 0 ) MRILIB_xoff = -MRILIB_xoff ;

           qq = abs(jor) ;
           MRILIB_yoff = xyz[qq-1] ; use_MRILIB_yoff = 1 ;
           if( jor > 0 ) MRILIB_yoff = -MRILIB_yoff ;

           /* 01 Nov 2002: adjust x,y offsets for mosaic */

           if( mosaic ){
             if( MRILIB_xoff < 0.0 ) MRILIB_xoff += 0.5*dx*mos_nx*(mos_ix-1) ;
             else                    MRILIB_xoff -= 0.5*dx*mos_nx*(mos_ix-1) ;
             if( MRILIB_yoff < 0.0 ) MRILIB_yoff += 0.5*dy*mos_ny*(mos_iy-1) ;
             else                    MRILIB_yoff -= 0.5*dy*mos_ny*(mos_iy-1) ;
           }

         } else if( nzoff == 1 && !use_MRILIB_zoff ){  /* 2nd DICOM image */

           float qoff = zz - zoff ;    /* vive la difference */
           if( qoff < 0 ) kor = -kor ; /* kor determines z-axis orientation */
#if 0
fprintf(stderr,"  nzoff=1 kor=%d qoff=%f\n",kor,qoff) ;
#endif
           switch( kor ){
             case  1: MRILIB_orients[4] = 'R'; MRILIB_orients[5] = 'L'; break;
             case -1: MRILIB_orients[4] = 'L'; MRILIB_orients[5] = 'R'; break;
             case  2: MRILIB_orients[4] = 'A'; MRILIB_orients[5] = 'P'; break;
             case -2: MRILIB_orients[4] = 'P'; MRILIB_orients[5] = 'A'; break;
             case  3: MRILIB_orients[4] = 'I'; MRILIB_orients[5] = 'S'; break;
             case -3: MRILIB_orients[4] = 'S'; MRILIB_orients[5] = 'I'; break;
             default: MRILIB_orients[4] ='\0'; MRILIB_orients[5] ='\0'; break;
           }
           MRILIB_orients[6] = '\0' ;

           /* save spatial offset of first slice              */
           /* [this needs to be positive in the direction of] */
           /* [the -z axis, so may need to change its sign  ] */

           MRILIB_zoff = zoff ; use_MRILIB_zoff = 1 ;
           if( kor > 0 ) MRILIB_zoff = -MRILIB_zoff ;
         }
         nzoff++ ;  /* 3rd and later images don't count for z-orientation */
       }
     }
   }  /* end of using image position */

   /** 23 Dec 2002:
       use slice location value to set z-offset,
       and (2cd time in) the z-axis orientation
       -- only try this if image position vector (code above) isn't present
          AND if we don't have a mosaic image (which already did this stuff)
       -- shouldn't be necessary, since slice location is deprecated        **/

   else if( nzoff < 2 && epos[E_SLICE_LOCATION] != NULL && have_orients && !mosaic ){
     ddd = strstr(epos[E_SLICE_LOCATION],"//") ;
     if( ddd != NULL ){
       float zz ; int qq ;
       qq = sscanf(ddd+2,"%f",&zz) ;
       if( qq == 1 ){
         static float zoff ;      /* saved from nzoff=0 case */

#if 0
fprintf(stderr,"SLICE_LOCATION = %f\n",zz) ;
#endif

         if( nzoff == 0 ){  /* 1st DICOM image */

           zoff = zz ;      /* save this for 2nd image calculation */

           if( MRILIB_orients[4] == '\0' ){
             switch( kor ){   /* may be changed on second image */
               case  1: MRILIB_orients[4] = 'L'; MRILIB_orients[5] = 'R'; break;
               case  2: MRILIB_orients[4] = 'P'; MRILIB_orients[5] = 'A'; break;
               case  3: MRILIB_orients[4] = 'I'; MRILIB_orients[5] = 'S'; break;
               default: MRILIB_orients[4] ='\0'; MRILIB_orients[5] ='\0'; break;
             }
             MRILIB_orients[6] = '\0' ;
           }

         } else if( nzoff == 1 && !use_MRILIB_zoff ){  /* 2nd DICOM image */

           float qoff = zz - zoff ;    /* vive la difference */
           if( qoff < 0 ) kor = -kor ; /* kor determines z-axis orientation */
           switch( kor ){
             case  1: MRILIB_orients[4] = 'R'; MRILIB_orients[5] = 'L'; break;
             case -1: MRILIB_orients[4] = 'L'; MRILIB_orients[5] = 'R'; break;
             case  2: MRILIB_orients[4] = 'A'; MRILIB_orients[5] = 'P'; break;
             case -2: MRILIB_orients[4] = 'P'; MRILIB_orients[5] = 'A'; break;
             case  3: MRILIB_orients[4] = 'I'; MRILIB_orients[5] = 'S'; break;
             case -3: MRILIB_orients[4] = 'S'; MRILIB_orients[5] = 'I'; break;
             default: MRILIB_orients[4] ='\0'; MRILIB_orients[5] ='\0'; break;
           }
           MRILIB_orients[6] = '\0' ;

           /* save spatial offset of first slice              */
           /* [this needs to be positive in the direction of] */
           /* [the -z axis, so may need to change its sign  ] */

           MRILIB_zoff = zoff ; use_MRILIB_zoff = 1 ;
           if( kor > 0 ) MRILIB_zoff = -MRILIB_zoff ;
         }
         nzoff++ ;  /* 3rd and later images don't count for z-orientation */
       }
     }
   } /* end of using slice location */

   /* perhaps shift data shorts */

#if 0
   if( shift == 1 ){
     switch( datum ){
       case MRI_short:{
         if( sbot < 0 ){
           unsigned short *sar ; int nvox = IMARR_SUBIM(imar,0)->nvox ;
           for( ii=0 ; ii < nz ; ii++ ){
             sar = mri_data_pointer( IMARR_SUBIM(imar,ii) ) ;
             for( jj=0 ; jj < nvox ; jj++ ) sar[jj] >>= 1 ;
           }
         }
       }
       break ;
     }
   }
#endif

   RETURN( imar );
}

/*------------------------------------------------------------------------------*/
/*! Count images in a DICOM file, if possible.
--------------------------------------------------------------------------------*/

int mri_imcount_dicom( char *fname )
{
   char *ppp , *ddd ;
   off_t poff ;
   unsigned int plen ;
   char *epos[NUM_ELIST] ;
   int ii , ee , bpp , datum ;
   int nx,ny,nz ;

   int mosaic=0 , mos_nx,mos_ny , mos_ix,mos_iy,mos_nz ;  /* 28 Oct 2002 */
   Siemens_extra_info sexinfo ;                           /* 02 Dec 2002 */

ENTRY("mri_imcount_dicom") ;

   if( str_sexinfo != NULL ){ free(str_sexinfo); str_sexinfo=NULL; }

   if( fname == NULL || fname[0] == '\0' ) RETURN(0);

   /* extract the header from the file (cf. mri_dicom_hdr.[ch]) */

   mri_dicom_nohex(1) ; mri_dicom_noname(1) ;
   ppp = mri_dicom_header( fname ) ;
   if( ppp == NULL ) RETURN(0);

   /* find out where the pixel array is in the file */

   mri_dicom_pxlarr( &poff , &plen ) ;
   if( plen <= 0 ){ free(ppp) ; RETURN(0); }

   /* check if file is actually this big */

   { unsigned int psiz , fsiz ;
     fsiz = THD_filesize( fname ) ;
     psiz = (unsigned int)(poff) + plen ;
     if( fsiz < psiz ){ free(ppp) ; RETURN(0); }
   }

   /* find positions in header of elements we care about */

   for( ee=0 ; ee < NUM_ELIST ; ee++ )
     epos[ee] = strstr(ppp,elist[ee]) ;

   /* see if the header has the elements we absolutely need */

   if( epos[E_ROWS]           == NULL ||
       epos[E_COLUMNS]        == NULL ||
       epos[E_BITS_ALLOCATED] == NULL   ){ free(ppp) ; RETURN(0); }

   /* check if we have 1 sample per pixel (can't deal with 3 or 4 now) */

   if( epos[E_SAMPLES_PER_PIXEL] != NULL ){
      ddd = strstr(epos[E_SAMPLES_PER_PIXEL],"//") ;
      if( ddd == NULL ){ free(ppp) ; RETURN(0); }
      ii = 0 ; sscanf(ddd+2,"%d",&ii) ;
      if( ii != 1 ){ free(ppp) ; RETURN(0); }
   }

   /* check if photometric interpretation is MONOCHROME (don't like PALETTE) */

   if( epos[E_PHOTOMETRIC_INTERPRETATION] != NULL ){
      ddd = strstr(epos[E_PHOTOMETRIC_INTERPRETATION],"MONOCHROME") ;
      if( ddd == NULL ){ free(ppp) ; RETURN(0); }
   }

   /* check if we have 8, 16, or 32 bits per pixel */

   ddd = strstr(epos[E_BITS_ALLOCATED],"//") ;
   if( ddd == NULL ){ free(ppp); RETURN(0); }
   bpp = 0 ; sscanf(ddd+2,"%d",&bpp) ;
   switch( bpp ){
      default: free(ppp) ; RETURN(0);   /* bad value */
      case  8: datum = MRI_byte ; break ;
      case 16: datum = MRI_short; break ;
      case 32: datum = MRI_int  ; break ;
   }
   bpp /= 8 ; /* now bytes per pixel, instead of bits */

   /* get nx, ny, nz */

   ddd = strstr(epos[E_ROWS],"//") ;
   if( ddd == NULL ){ free(ppp) ; RETURN(0); }
   nx = 0 ; sscanf(ddd+2,"%d",&nx) ;
   if( nx < 2 ){ free(ppp) ; RETURN(0); }

   ddd = strstr(epos[E_COLUMNS],"//") ;
   if( ddd == NULL ){ free(ppp) ; RETURN(0); }
   ny = 0 ; sscanf(ddd+2,"%d",&ny) ;
   if( ny < 2 ){ free(ppp) ; RETURN(0); }

   nz = 0 ;
   if( epos[E_NUMBER_OF_FRAMES] != NULL ){
     ddd = strstr(epos[E_NUMBER_OF_FRAMES],"//") ;
     if( ddd != NULL ) sscanf(ddd+2,"%d",&nz) ;
   }
   if( nz == 0 ) nz = plen / (bpp*nx*ny) ;

   /*-- 28 Oct 2002: Check if this is a Siemens mosaic.        --*/
   /*-- 02 Dec 2002: Don't use Acquisition Matrix anymore;
                     instead, use the Siemens extra info
                     in epos[E_SIEMENS_2].                     --*/
   /*-- 24 Dec 2002: Extract str_sexinfo even if not a mosaic. --*/

   if(        epos[E_ID_MANUFACTURER]            != NULL &&
       strstr(epos[E_ID_MANUFACTURER],"SIEMENS") != NULL &&
              epos[E_SIEMENS_2]                  != NULL    ){

     int len=0,loc=0 , aa,bb ;
     sscanf(epos[E_SIEMENS_2],"%x%x%d [%d" , &aa,&bb , &len,&loc ) ;
     if( len > 0 && loc > 0 ){
       FILE *fp = fopen( fname , "rb" ) ;
       if( fp != NULL ){
         str_sexinfo = extract_bytes_from_file( fp, (off_t)loc, (size_t)len, 1 ) ;
         fclose(fp) ;
       }
     }
   }

   if(        epos[E_ID_IMAGE_TYPE]              != NULL &&
       strstr(epos[E_ID_IMAGE_TYPE],"MOSAIC")    != NULL &&
       str_sexinfo                               != NULL   ){

     /* 31 Oct 2002: extract extra Siemens info from file */

     sexinfo.good = 0 ;  /* start by marking it as bad */
     get_siemens_extra_info( str_sexinfo , &sexinfo ) ;

     if( sexinfo.good ){                                   /* if data is good */

       nz = sexinfo.mosaic_num ;

     } /* end of if sexinfo was good */

     else {                  /* warn if sexinfo was bad */
       static int nwarn=0 ;
       if( nwarn < NWMAX )
         fprintf(stderr,"++ DICOM WARNING: indecipherable SIEMENS MOSAIC info (%s) in file %s\n",
                 elist[E_SIEMENS_2] , fname ) ;
       if( nwarn == NWMAX )
         fprintf(stderr,"++ DICOM NOTICE: no more SIEMENS MOSAIC info messages will be printed\n");
       nwarn++ ;
     }

   } /* end of if a Siemens mosaic */

   free(ppp) ; RETURN(nz);
}

/*--------------------------------------------------------------------------------*/
/*! Read some bytes from an open file at a given offset.  Return them in a
    newly malloc()-ed array.  If return value is NULL, something bad happened.
----------------------------------------------------------------------------------*/

static char * extract_bytes_from_file( FILE *fp, off_t start, size_t len, int strize )
{
   char *ar ;
   size_t nn , ii ;

   if( fp == NULL || len == 0 ) return NULL ;    /* bad inputs? */
   ar = calloc(1,len+1) ;                        /* make space for data */
   lseek( fileno(fp) , start , SEEK_SET ) ;      /* set file position */
   nn = fread( ar , 1 , len , fp ) ;             /* read data */
   if( nn == 0 ){ free(ar); return NULL; }       /* bad read? */

   if( strize ){                                 /* convert to C string? */
     for( ii=0 ; ii < nn ; ii++ )                /* scan for NULs and    */
       if( ar[ii] == '\0' ) ar[ii] = ' ' ;       /* replace with blanks  */
   }
   return ar ;
}

/*--------------------------------------------------------------------------------*/
/*! Parse the Siemens extra stuff for mosaic information.
    Ad hoc, based on sample data and no documentation.
----------------------------------------------------------------------------------*/

static void get_siemens_extra_info( char *str , Siemens_extra_info *mi )
{
   char *cpt , *dpt ;
   int nn , mm , snum , last_snum=-1 ;
   int have_x[2] = {0,0},
       have_y[2] = {0,0},
       have_z[2] = {0,0};
   float x,y,z , val ;
   char name[1024] ;

   /*-- check for good inputs --*/

   if( mi == NULL ) return ;

   mi->good = 0 ;
   for( snum=0 ; snum < NMOMAX ; snum++ )
     mi->slice_xyz[snum][0] = mi->slice_xyz[snum][1] = mi->slice_xyz[snum][2] = 0.0 ;

   if( str == NULL || *str == '\0' ) return ;

   /*-- find string that starts the slice information array --*/

   cpt = strstr( str , "sSliceArray.asSlice[" ) ;
   if( cpt == NULL ) return ;

   /*-- scan for coordinates, until can't find a good string to scan --*/

#if 0
   /* 25 Feb 2003 Changed logic here KRH */
   have_x = have_y = have_z = 0 ;
#endif

   while(1){

     /* interepret next string into
         snum = slice subscript (0,1,...)
         name = variable name
         val  = number of RHS of '=' sign
         mm   = # of bytes used in scanning the above */

     nn = sscanf( cpt , "sSliceArray.asSlice[%d].%1022s =%f%n" ,
                  &snum , name , &val , &mm ) ;

     if( nn   <  3                   ) break ;  /* bad conversion set */
     if( snum <  0 || snum >= NMOMAX ) break ;  /* slice number out of range */

/* 21 Feb 2003 rework this section to allow for missing coordinates in
 * some mosaic files                                        --KRH  */
#if 0
     /* assign val based on name */

          if( strcmp(name,"sPosition.dSag") == 0 ){ x = val; have_x = 1; }
     else if( strcmp(name,"sPosition.dCor") == 0 ){ y = val; have_y = 1; }
     else if( strcmp(name,"sPosition.dTra") == 0 ){ z = val; have_z = 1; }

     /* if now have all 3 coordinates, save them */

     if( have_x && have_y && have_z ){
       mi->slice_xyz[snum][0] = x; mi->slice_xyz[snum][1] = y; mi->slice_xyz[snum][2] = z;
       last_snum = snum ;
       have_x = have_y = have_z = 0 ;
     }
#endif

     if( strcmp(name,"sPosition.dSag") == 0 ){ 
       mi->slice_xyz[snum][0] = val;
       if (snum < 2) have_x[snum] = 1;
       last_snum = snum;
     } else if( strcmp(name,"sPosition.dCor") == 0 ){ 
       mi->slice_xyz[snum][1] = val;
       if (snum < 2) have_y[snum] = 1;
       last_snum = snum;
     } else if( strcmp(name,"sPosition.dTra") == 0 ){ 
       mi->slice_xyz[snum][2] = val;
       if (snum < 2) have_z[snum] = 1;
       last_snum = snum;
     }

     /* skip to next slice array assignment string (which may not be a coordinate) */

     dpt = cpt + mm ;                                           /* just after 'val' */
     cpt =  dpt ;
     while( isspace(*cpt) ) cpt++ ;                             /* skip over whitespace */
     if( cpt-dpt > 16 ) break ;                                 /* too much space */
     if( strncmp(cpt,"sSliceArray.asSlice[",20) != 0 ) break ;  /* bad next line */

   }

   /* if got at least 1 slice info, mark data as being good */

   if( last_snum >= 0 ){
     mi->good       = 1 ;
     if (have_x[0] && have_x[1]) mi->have_data[0] = 1;
     if (have_y[0] && have_y[1]) mi->have_data[1] = 1;
     if (have_z[0] && have_z[1]) mi->have_data[2] = 1;
     mi->mosaic_num = last_snum+1 ;
   }

   return ;
}
