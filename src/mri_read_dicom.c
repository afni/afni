#include "mrilib.h"
#include "vecmat.h"

/*-----------------------------------------------------------------------------------*/
#undef DEBUG_ON
/*#define DEBUG_ON 1*/

#define NWMAX 2  /* max number of warning message of each type to print */

#define NMOMAX 256                 /* never seen one this big!!! */
#define EPSILON 0.0001
#define ALMOST(a,b) \
   ( fabs(a - b) < EPSILON)

typedef struct {
  int good ;                       /* data in here is good? */
  int have_data[3] ;               /* do we have slices 0 and 1 in           *
				    * each dimension to determine z-spacing? *
				    * added 25 Feb 2003 KRH                  */
  int mos_ix, mos_nx, mos_ny; /* mosaic properties */
  int mosaic_num ;                 /* how many slices in 1 'image' */
  float slice_xyz[NMOMAX][3] ;     /* Sag, Cor, Tra coordinates */
  int ImageNumbSag, ImageNumbTra, ImageNumbCor; /* reverse the slices */
} Siemens_extra_info ;

typedef struct {
   THD_fvec3 xvec, yvec;            /* Image Orientation fields */
   THD_fvec3 dfpos1;                /* image origin for first two slices*/
   THD_fvec3 dfpos2;
   THD_fvec3 del;                   /* voxel dimensions */
   int mosaic;                      /* data is mosaic */
   int mos_ix, mos_nx, mos_ny, mos_nslice; /* mosaic properties */
   int nx, ny;                      /* overall mosaic dimensions */
   float Tr_dicom[4][4];            /* transformation matrix */
   float slice_xyz[2][3];           /* coordinates for 1st and last slices */
   int mos_sliceinfo;               /* flag for existence of coordinate info */
   int flip_slices;
} oblique_info;

oblique_info obl_info;

/* mod -16 May 2007 */
/* compute Tr transformation matrix for oblique data */
static int read_mosaic_data( FILE *fp, MRI_IMAGE *im, MRI_IMARR *imar,
          int datum, Siemens_extra_info *mi, int * flip_slices,
          int bpp, int kor, int swap, float dx, float dy, float dz, float dt);
static int flip_slices_mosaic (Siemens_extra_info *mi, int kor);

static float *ComputeObliquity(oblique_info *obl_info);
static void Clear_obl_info(oblique_info *obl_info);
static void Fill_obl_info(oblique_info *obl_info, char **epos, Siemens_extra_info *siem);
void mri_read_dicom_reset_obliquity();
void mri_read_dicom_get_obliquity(float *);

static char * extract_bytes_from_file( FILE *fp, off_t start, size_t len, int strize ) ;
static void get_siemens_extra_info( char *str , Siemens_extra_info *mi ) ;
static float get_dz(  char **epos);

static int CheckObliquity(float xc1, float xc2, float xc3, float yc1, float yc2, float yc3);
static int get_posns_from_elist(char *plist[], char *elist[], char *text,
                                int nume);

static int obl_info_set = 0;

static int debugprint = 0;

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

/*---------------------------------------------------------------------------*/

#include "mri_dicom_elist.h"  /** elist now defined elsewhere [05 May 2008] **/

/*---------------------------------------------------------------------------*/
/* global set via 'to3d -assume_dicom_mosaic'            13 Mar 2006 [rickr] */
int assume_dicom_mosaic = 0;   /* (case of 1 is equivalent to Rich's change) */
int use_last_elem = 0;         /* when filling epos list 10 Apr 2009 [rickr] */

#undef  SINT
#undef  SFLT
#define SINT(p) ((int)strtol((p),NULL,10))  /* scan for an integer */
#define SFLT(p) ((float)strtod((p),NULL))   /* scan for a float */

/*---------------------------------------------------------------------------*/
/*! Read image(s) from a DICOM file, if possible.
-----------------------------------------------------------------------------*/

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
   MRI_IMAGE *im=NULL ;
   void *iar ;
   FILE *fp ;
   int have_orients=0 ;
   int ior=0,jor=0,kor=0 ;
   static int nzoff=0 ;   /* for determining z-axis orientation/offset from multiple calls */
   int mosaic=0 , mos_nx=0,mos_ny=0 , mos_ix=0,mos_iy=0,mos_nz=0 ;  /* 28 Oct 2002 */
   Siemens_extra_info sexinfo ;                                     /* 31 Oct 2002 */
#if DEBUG_ON
   short sbot,stop ;
   float xcen,ycen,zcen ;
   int use_xycen=0 ;
#endif
   float dxx,dyy,dzz ;

   char *eee ;
   float rescale_slope=0.0 , rescale_inter=0.0 ;  /* 23 Dec 2002 */
   float window_center=0.0 , window_width =0.0 ;

   char *sexi_start, *sexi_start2;   /* KRH 25 Jul 2003 */
   char *sexi_end;

   int ts_endian = 1 ; /* 05 Jul 2006 - transfer syntax endian-ness
                                      - 1 = little endian, 0 = big - RWCox */
   int un16 = 0 ;      /* 05 Jul 2006 - is it 16 bit unsigned data? */
   int ov16 = 0 ;      /*             - did 16 bit overflow occur? */

   static int obliqueflag = 0;
   float xc1=0.0,xc2=0.0,xc3=0.0 , yc1=0.0,yc2=0.0,yc3=0.0 ;
   float xn,yn ; int qq ;

ENTRY("mri_read_dicom") ;

   if( str_sexinfo != NULL ){ free(str_sexinfo); str_sexinfo=NULL; }

   if( !mri_possibly_dicom(fname) ){                /* 07 May 2003 */
     if( AFNI_yesenv("AFNI_DICOM_VERBOSE") )
       ERROR_message("file %s is not possibly DICOM",fname) ;
     RETURN(NULL) ;
   }

   /* extract header info from file into a string
      - cf. mri_dicom_hdr.[ch]
      - run 'dicom_hdr -noname fname' to see the string format */
   sexinfo.mosaic_num = 1;           /* initialize to non-mosaic */
   mri_dicom_nohex(1) ;              /* don't print ints in hex mode */
   mri_dicom_noname(1) ;             /* don't print names, just tags */
   ppp = mri_dicom_header( fname ) ; /* print header to malloc()-ed string */
   if( ppp == NULL ){                /* didn't work; not a DICOM file? */
     if( AFNI_yesenv("AFNI_DICOM_VERBOSE") )
       ERROR_message("file %s is not interpretable as DICOM",fname) ;
     RETURN(NULL) ;
   }

   /* find positions in header of elements we care about */

   /* allow some choices when filling the list    10 Apr 2009 [rickr] */
   get_posns_from_elist(epos, elist, ppp, NUM_ELIST);

   /* Determine if we need to swap bytes after reading image data */
   /* DICOM files are stored in LSB first (little endian) mode    */
   /* 05 Jul 2006 - not necessarily: check transfer syntax - RWC  */

   RWC_set_endianosity() ;

   /* Transfer Syntax possibilities for the image data are:

        "1.2.840.10008.1.2"       = implicit little endian (default)
        "1.2.840.10008.1.2.1"     = explicit little endian
        "1.2.840.10008.1.2.2"     = explicit big endian

      Various other possibilities (not supported herein) include:

        "1.2.840.10008.1.2.4.70"  = lossless JPEG
        "1.2.840.10008.1.2.4.57"  = another lossless JPEG
        "1.2.840.10008.1.2.4.50"  = 8-bit lossy JPEG
        "1.2.840.10008.1.2.4.51"  = 12-bit lossy JPEG
        "1.2.840.10008.1.2.5"     = RLE compression
        "1.2.840.10008.1.2.4.80"  = lossless JPEG-LS
        "1.2.840.10008.1.2.4.81"  = near-lossless JPEG-LS
        "1.2.840.10008.1.2.4.90"  = lossless JPEG-2000
        "1.2.840.10008.1.2.4.91"  = lossy JPEG-2000
        "1.2.840.10008.1.2.4.100" = MPEG-2
        "1.2.840.10008.1.2.1.99"  = 'deflate' compression

      Plus, 'private' transfer syntaxes are legal.  05 Jul 2006 - RWCox */

   if( epos[E_TRANSFER_SYNTAX] != NULL ){
     ddd = strstr(epos[E_TRANSFER_SYNTAX],"//") ;
     if( ddd != NULL ){
       char ts[256]="\0" ;
       sscanf(ddd+2,"%254s",ts) ;
       ts_endian = -1 ;
       if( strlen(ts) >= 17 && strncmp(ts,"1.2.840.10008.1.2",17)==0 ){
         if( strcmp(ts,"1.2.840.10008.1.2.2") == 0 )        /* big endian */
           ts_endian = 0 ;  /* big endian */
         else if( strcmp(ts,"1.2.840.10008.1.2.1") == 0 ||
                  strcmp(ts,"1.2.840.10008.1.2"  ) == 0   ) /* little endian */
           ts_endian = 1 ;
       }
       if( ts_endian < 0 ){
         static int nwarn=0 ;
         if( nwarn < NWMAX )
           WARNING_message("DICOM file %s: unsupported Transfer Syntax '%s'",fname,ts) ;
         if( nwarn == NWMAX )
           WARNING_message("DICOM: no more Transfer Syntax messages will be printed") ;
         nwarn++ ;
       }
     }
   }

   if( ts_endian < 0 ) ts_endian = 1 ;

   swap = (ts_endian != LITTLE_ENDIAN_ARCHITECTURE) ;

   /* find out where the pixel array is in the file */

   mri_dicom_pxlarr( &poff , &plen ) ;
   if( plen == 0 || poff == 0 ){
     ERROR_message("DICOM file %s: ILLEGAL plen=%d  poff=%u",
                   fname , plen , (unsigned int)poff ) ;
     free(ppp); RETURN(NULL);
   }

   /* check if file is actually this big (maybe it was truncated) */

   { long long psiz , fsiz ;
     fsiz = THD_filesize( fname ) ;
     psiz = (long long)(poff) + (long long)(plen) ;
     if( fsiz < psiz ){
       ERROR_message("DICOM file %s: plen=%u  poff=%u  filesize=%lld",
                     fname , plen , (unsigned int)poff , fsiz ) ;
       free(ppp) ; RETURN(NULL);
     }
   }

   /* see if the header has the elements we absolutely need */

   if( epos[E_ROWS]           == NULL ||
       epos[E_COLUMNS]        == NULL ||
       epos[E_BITS_ALLOCATED] == NULL   ){

     ERROR_message("DICOM file %s: missing mandatory fields",fname) ;
     free(ppp) ; RETURN(NULL);
   }

   /* check if we have 1 sample per pixel (can't deal with 3 or 4 now) */

   if( epos[E_SAMPLES_PER_PIXEL] != NULL ){
      ddd = strstr(epos[E_SAMPLES_PER_PIXEL],"//") ;
      ii = SINT(ddd+2) ;
      if( ii != 1 ){
        ERROR_message("DICOM file %s: %d samples per pixel -- too much!",
                      fname,ii) ;
        free(ppp) ; RETURN(NULL);
      }
   }

   /* check if photometric interpretation is MONOCHROME (don't like PALETTE) */

   if( epos[E_PHOTOMETRIC_INTERPRETATION] != NULL ){
      ddd = strstr(epos[E_PHOTOMETRIC_INTERPRETATION],"MONOCHROME") ;
      if( ddd == NULL ){
        ERROR_message("DICOM file %s: not MONOCHROME!",fname) ;
        free(ppp) ; RETURN(NULL);
      }
   }

   /* check if we have 8, 16, or 32 bits per pixel */

   ddd = strstr(epos[E_BITS_ALLOCATED],"//") ;
   if( ddd == NULL ){ free(ppp); RETURN(NULL); }
   bpp = SINT(ddd+2) ;
   switch( bpp ){
      default:
        ERROR_message(
          "DICOM file %s: unsupported Bits Allocated = %d", fname , bpp ) ;
        free(ppp); RETURN(NULL);    /* bad value */
      case  8: datum = MRI_byte ; break ;
      case 16: datum = MRI_short; break ;
      case 32: datum = MRI_int  ; break ;  /* probably not present in DICOM? */
   }
   bpp /= 8 ; /* now bytes per pixel, instead of bits */

   /*** Print some warnings if appropriate ***/

   /* check if BITS_STORED and HIGH_BIT are aligned */

   if( epos[E_BITS_STORED] != NULL && epos[E_HIGH_BIT] != NULL ){
     int bs=0 , hb=0 ;
     ddd = strstr(epos[E_BITS_STORED],"//") ; bs = SINT(ddd+2) ;
     ddd = strstr(epos[E_HIGH_BIT],"//")    ; hb = SINT(ddd+2) ;
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
       ddd = strstr(epos[E_RESCALE_INTERCEPT],"//"); rescale_inter = SFLT(ddd+2);
       ddd = strstr(epos[E_RESCALE_SLOPE    ],"//"); rescale_slope = SFLT(ddd+2);
     }
   }

   /* check if Window is ordered */
   /* 23 Dec 2002: actually get the window params, if environment says to */

   eee = getenv("AFNI_DICOM_WINDOW") ;
   if( epos[E_WINDOW_CENTER] != NULL && epos[E_WINDOW_WIDTH] != NULL ){
     if( eee == NULL || toupper(*eee) != 'Y' ){
       static int nwarn=NWMAX+1 ; /* never show these messages   31 Aug 2007 */
       if( nwarn < NWMAX )
         fprintf(stderr,
                 "++ DICOM WARNING: file %s has Window tags;\n"
                 " setenv AFNI_DICOM_WINDOW YES to enforce them, but this is rarely necessary or even wanted\n",
                 fname ) ;
       if( nwarn == NWMAX )
         fprintf(stderr,"++ DICOM WARNING: no more Window tags messages will be printed\n") ;
       nwarn++ ;
     } else {
       ddd = strstr(epos[E_WINDOW_CENTER],"//"); window_center = SFLT(ddd+2);
       ddd = strstr(epos[E_WINDOW_WIDTH ],"//"); window_width  = SFLT(ddd+2);
     }
   }

   /*** extract attributes of the image(s) to be read in ***/

   /* get image nx & ny */

   ddd = strstr(epos[E_ROWS],"//") ;                 /* 31 Oct 2002: */
   if( ddd == NULL ){ free(ppp) ; RETURN(NULL); }    /* Oops: ROWS is ny and */
   ny = SINT(ddd+2) ;                                /*       COLUMNS is nx! */
   if( ny < 2 ){ free(ppp) ; RETURN(NULL); }

   ddd = strstr(epos[E_COLUMNS],"//") ;
   if( ddd == NULL ){ free(ppp) ; RETURN(NULL); }
   nx = SINT(ddd+2) ;
   if( nx < 2 ){ free(ppp) ; RETURN(NULL); }

   /* get number of slices */

   nz = 0 ;
   if( epos[E_NUMBER_OF_FRAMES] != NULL ){
     ddd = strstr(epos[E_NUMBER_OF_FRAMES],"//") ;
     if( ddd != NULL ) nz = SINT(ddd+2) ;
   }

   /* if didn't get nz above, make up a value */

   if( nz == 0 ) nz = plen / (bpp*nx*ny) ;    /* compute from image array size */
   if( nz == 0 ){
     ERROR_message("DICOM file %s: not enough data for 1 slice!",fname) ;
     free(ppp) ; RETURN(NULL);
   }

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
/*********Siemens mosaic section ***********************/
   /*-- process str_sexinfo only if this is marked as a mosaic image --*/
   /*-- preliminary processing of sexinfo EVEN IF NOT MARKED AS MOSAIC, --*/
   /*-- SINCE PSYCHOTIC, UNMARKED MOSAICS ARE TURNING UP IN THE WILD!  KRH, 11/6/05 --*/

   /* 31 Oct 2002: extract extra Siemens info from str_sexinfo */

   /* KRH 25 Jul 2003 if start and end markers are present for
    * Siemens extra info, cut string down to those boundaries */

   /* if assume_dicom_mosaic is not set, then require "MOSAIC" string */
   /*                                             13 Mar 2006 [rickr] */
   if( ( assume_dicom_mosaic ||
         ( epos[E_ID_IMAGE_TYPE] && strstr(epos[E_ID_IMAGE_TYPE],"MOSAIC") ) )
       && str_sexinfo                               != NULL   ){

     sexi_start = strstr(str_sexinfo, "### ASCCONV BEGIN ###");
     if(sexi_start != NULL) {  /* search for end after start - drg,fredtam 23 Mar 2007 */
        sexi_start2 = strstr(sexi_start+21, "### ASCCONV BEGIN ###");
        sexi_end = strstr(sexi_start, "### ASCCONV END ###");
        if (sexi_end != NULL) {
           char *sexi_tmp;
           int sexi_size;

           if((sexi_start2!=NULL) && (sexi_start2<sexi_end)) {
              sexi_start = sexi_start2;
            }

	   sexi_size = sexi_end - sexi_start + 19 ;
	   sexi_tmp = AFMALL( char, sexi_size+1 );
	   memcpy(sexi_tmp,sexi_start,sexi_size);
           sexi_tmp[sexi_size] = '\0';
	   free(str_sexinfo);
	   str_sexinfo = sexi_tmp;
        }
     }
     /* end KRH 25 Jul 2003 change */

     sexinfo.good = 0 ;  /* start by marking it as bad */
     for(ii = 0; ii < 3; ii++) sexinfo.have_data[ii] = 0; /* 25 Feb 03 Initialize new member KRH */

     get_siemens_extra_info( str_sexinfo , &sexinfo ) ;

     if( sexinfo.good ){                                   /* if data is good */

       if((        epos[E_ID_IMAGE_TYPE]              != NULL &&
         strstr(epos[E_ID_IMAGE_TYPE],"MOSAIC")    != NULL) ||
         sexinfo.mosaic_num                        > 1   ) {

         /* compute size of mosaic layout
          as 1st integer whose square is >= # of images in mosaic */

         for( mos_ix=1 ; mos_ix*mos_ix < sexinfo.mosaic_num ; mos_ix++ ) ; /* nada */
         sexinfo.mos_ix = mos_iy = mos_ix ;

         sexinfo.mos_nx = mos_nx = nx / mos_ix ;
         sexinfo.mos_ny = mos_ny = ny / mos_iy ;  /* sub-image dimensions */

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
               fprintf(stderr,"++ DICOM NOTICE: no more Siemens Mosaic messages will be printed\n") ;
             nwarn++ ;

         } /* end of if mosaic sizes are reasonable */

         else {                        /* warn about bad mosaic sizes */
           static int nwarn=0 ;
           if( nwarn < NWMAX ) {
             fprintf(stderr,
                   "\n** DICOM WARNING: bad Siemens Mosaic params: nx=%d ny=%d ix=%d iy=%d imx=%d imy=%d\n",
                   mos_nx,mos_ny , mos_ix,mos_iy , nx,ny ) ;
             fprintf(stderr,"   (consider the option -use_last_elem)\n");
           }
           if( nwarn == NWMAX )
             fprintf(stderr,"++ DICOM NOTICE: no more Siemens Mosaic param messages will be printed\n");
           nwarn++ ;
         }
       } /* end of if a Siemens mosaic */

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
   } /* end of if str_sexinfo exists */
/*********end of Siemens mosaic section ***********************/

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

   /* get dz now*/
   dz = get_dz(epos);

   /* get dt */
   if( epos[E_REPETITION_TIME] != NULL ){
     ddd = strstr(epos[E_REPETITION_TIME],"//") ;
     if( ddd != NULL ) dt = 0.001f * SFLT(ddd+2) ;  /* ms to s conversion */
   }


   /* check if we might have 16 bit unsigned data that fills all bits */
   if( bpp == 2 ){
     if( epos[E_PIXEL_REPRESENTATION] != NULL ){
       ddd = strstr(epos[E_PIXEL_REPRESENTATION],"//") ;
       if( ddd != NULL ){
         ii = SINT(ddd+2) ; if( ii == 0 ) un16 = 1 ; /* unsigned */
       }
     }
     if( un16 && epos[E_HIGH_BIT] != NULL ){
       ddd = strstr(epos[E_HIGH_BIT],"//") ;
       if( ddd != NULL ){
         ii = SINT(ddd+2) ; if( ii < 15 ) un16 = 0 ; /* but less than 16 bits */
       }
     }
   }

#if 0
   if( bpp == 2 ){
     if( epos[E_PIXEL_REPRESENTATION] != NULL ){
       ddd = strstr(epos[E_PIXEL_REPRESENTATION],"//") ;
       if( ddd != NULL ){
         ii = SINT(ddd+2) ; if( ii == 1 ) shift = -1 ;
       }
     }
     if( shift == 0 && epos[E_HIGH_BIT] != NULL ){
       ddd = strstr(epos[E_HIGH_BIT],"//") ;
       if( ddd != NULL ){
         ii = SINT(ddd+2) ; if( ii == 15 ) shift = 1 ;
       }
     }
     sbot = 32767 ; stop = -32767 ;
   }
#endif

   /*-- store some extra information in MRILIB globals, too? --*/

   if( dt > 0.0 && MRILIB_tr <= 0.0 ) MRILIB_tr = dt ;  /* TR */

   /*-- try to get image orientation fields (also, set ior,jor,kor) --*/

   if( epos[E_IMAGE_ORIENTATION] != NULL ){    /* direction cosines of image plane */

     ddd = strstr(epos[E_IMAGE_ORIENTATION],"//") ;
     if( ddd != NULL ){
       qq = sscanf(ddd+2,"%f\\%f\\%f\\%f\\%f\\%f",&xc1,&xc2,&xc3,&yc1,&yc2,&yc3);
       xn = sqrt( xc1*xc1 + xc2*xc2 + xc3*xc3 ) ; /* vector norms */
       yn = sqrt( yc1*yc1 + yc2*yc2 + yc3*yc3 ) ;
       if( qq == 6 && xn > 0.0 && yn > 0.0 ){     /* both vectors OK */

         xc1 /= xn ; xc2 /= xn ; xc3 /= xn ;      /* normalize vectors */
         yc1 /= yn ; yc2 /= yn ; yc3 /= yn ;
         if(!obl_info_set) {
            obliqueflag = CheckObliquity(xc1, xc2, xc3, yc1, yc2, yc3);
             if(obliqueflag) {
        	INFO_message("Data detected to be oblique");
             /* can also check obliquity consistency here across dicom files*/
             }
         }
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
#if DEBUG_ON
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
     float z0=0.0, z1=0.0 ;
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

#if DEBUG_ON
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
         float zz = xyz[abs(kor)-1] ;  /* kor from orients above */

#if DEBUG_ON
fprintf(stderr,"IMAGE_POSITION=%f %f %f  kor=%d\n",xyz[0],xyz[1],xyz[2],kor) ;
#endif

         if( nzoff == 0 ){  /* 1st DICOM image */

           zoff = zz ;      /* save this for 2nd image calculation */

           /* 01 Nov 2002: in mosaic case, may have set this already */

           if( MRILIB_orients[4] == '\0' ){
             switch( abs(kor) ){   /* may be changed on second image */
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
#if DEBUG_ON
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

#if DEBUG_ON
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


   if(obl_info_set<2)
         Fill_obl_info(&obl_info, epos, &sexinfo);


   /** Finally! Read images from file. **/

   fp = fopen( fname , "rb" ) ;
   if( fp == NULL ){
     ERROR_message("DICOM file %s: can't open!?",fname) ;
     free(ppp) ; RETURN(NULL);
   }
   lseek( fileno(fp) , poff , SEEK_SET ) ;

   INIT_IMARR(imar) ;

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

   }
   else {         /*copy images from mosaic data into an image array */
      read_mosaic_data( fp, im, imar, datum, &sexinfo, &obl_info.flip_slices,
                        bpp, kor, swap,dx,dy,dz, dt);
   }
   fclose(fp) ;     /* 10 Sep 2002: oopsie - forgot to close file */

   /*-- 05 Jul 2006 - check for 16 bit overflow --*/

   /* make sure im wasn't TRUNCATEd   1 Jul 2008 (and 5 Aug 2008) [rickr] */
   im = IMARR_SUBIM(imar,0) ;

   if( un16 ){
     for( ov16=ii=0 ; ii < IMARR_COUNT(imar) ; ii++ ){
       short *sar = MRI_SHORT_PTR( IMARR_SUBIM(imar,ii) ) ;
       for( jj=0 ; jj < im->nvox ; jj++ ) if( sar[jj] < 0 ) ov16++ ;
     }
     if( ov16 )
       WARNING_message(
        "DICOM file %s: unsigned 16-bit; AFNI stores signed: %d pixels < 0" ,
          fname , ov16 ) ;
   }


   /* rescale data - rarely needed */

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
       ymax = SINT(ddd+2) ; if( ymax > 0 ) ymax = (1 << ymax) - 1 ;
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



   free(ppp); RETURN( imar );
}

/*---------- compute slice thickness from DICOM header ----------*/

static float get_dz(  char **epos)
{
  int stupid_ge_fix , no_stupidity ;
  float sp=0.0 , th=0.0, dz = 0.0 ;
  static int nwarn=0 ;
  char *eee, *ddd ;

  eee           = getenv("AFNI_SLICE_SPACING_IS_GAP") ;
  stupid_ge_fix = (eee != NULL && (*eee=='Y' || *eee=='y') ) ;
  no_stupidity  = (eee != NULL && (*eee=='N' || *eee=='n') ) ;  /* 03 Mar 2003 */

  if( epos[E_SLICE_SPACING] != NULL ){                  /* get reported slice spacing */
    ddd = strstr(epos[E_SLICE_SPACING],"//") ;
    if( ddd != NULL ) {
       if(*(ddd+2)=='\n'){  /* catch carriage returns - Jeff Gunter via DRG 3/14/2007 */
	  sp = 0.0;   /* probably should write this as function to check on all DICOM fields*/
       }
       else {
          sp = SFLT(ddd+2) ;
       }
     }
  }

  if( epos[E_SLICE_THICKNESS] != NULL ){                /* get reported slice thickness */
    ddd = strstr(epos[E_SLICE_THICKNESS],"//") ;
    if( ddd != NULL ) {
       if(*(ddd+2)=='\n'){
	  th = 0.0;
       }
       else {
	  th = SFLT(ddd+2) ;
	  }
    }
  }

  th = fabs(th) ; sp = fabs(sp) ;                       /* we don't use the sign */

  if( stupid_ge_fix ){                                  /* always be stupid */
    dz = sp+th ;
  } else {

    if( no_stupidity && sp > 0.0 )                      /* 13 Jan 2004: if 'NO', then */
      dz = sp ;                                         /* always use spacing if present */
    else
      dz = (sp > th) ? sp : th ;                        /* the correct-ish DICOM way */

#define GFAC 0.99

    if( !no_stupidity ){                                /* unless stupidity is turned off */
      if( sp > 0.0 && sp < GFAC*th ) dz = sp+th ;       /* the stupid GE way again */

      if( sp > 0.0 && sp < GFAC*th && nwarn < NWMAX ){
        fprintf(stderr,
                "++ DICOM WARNING: Slice_Spacing=%f smaller than Slice_Thickness=%f\n",
                 sp , th ) ;
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
           "++                                                               ++\n"
           "++  Setting AFNI_SLICE_SPACING_IS_GAP to NO means that the       ++\n"
           "++  DICOM Slice_Spacing variable will be used for dz, replacing  ++\n"
           "++  the Slice_Thickness variable.  This usage may be required    ++\n"
           "++  for some pulse sequences on Phillips scanners.               ++\n"
           "\n\a" ,
          sp+th , dz ) ;
      }
      if( sp > 0.0 && sp < th && nwarn == NWMAX )
        fprintf(stderr,"++ DICOM WARNING: no more Slice_Spacing messages will be printed\n") ;
      nwarn++ ;
    }
  }
  if( dz == 0.0 ) dz = 1.0 ;               /* nominal dz */

  return(dz);
} /*-- end of dz code, with all its stupidities --*/


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

   char *sexi_start, *sexi_start2;   /* KRH 25 Jul 2003 */
   char *sexi_end;

ENTRY("mri_imcount_dicom") ;

   if( str_sexinfo != NULL ){ free(str_sexinfo); str_sexinfo=NULL; }

   if( !mri_possibly_dicom(fname) ) RETURN(0) ;  /* 07 May 2003 */

   /* extract the header from the file (cf. mri_dicom_hdr.[ch]) */

   mri_dicom_nohex(1) ; mri_dicom_noname(1) ;
   ppp = mri_dicom_header( fname ) ;
   if( ppp == NULL ) RETURN(0);

   /* find out where the pixel array is in the file */

   mri_dicom_pxlarr( &poff , &plen ) ;
   if( plen == 0 ){ free(ppp) ; RETURN(0); }

   /* check if file is actually this big */

   { long long psiz , fsiz ;
     fsiz = THD_filesize( fname ) ;
     psiz = (long long)(poff) + (long long)(plen) ;
     if( fsiz < psiz ){ free(ppp) ; RETURN(0); }
   }

   /* find positions in header of elements we care about */

   /* allow some choices when filling the list    10 Apr 2009 [rickr] */
   get_posns_from_elist(epos, elist, ppp, NUM_ELIST) ;

   /* see if the header has the elements we absolutely need */

   if( epos[E_ROWS]           == NULL ||
       epos[E_COLUMNS]        == NULL ||
       epos[E_BITS_ALLOCATED] == NULL   ){ free(ppp) ; RETURN(0); }

   /* check if we have 1 sample per pixel (can't deal with 3 or 4 now) */

   if( epos[E_SAMPLES_PER_PIXEL] != NULL ){
      ddd = strstr(epos[E_SAMPLES_PER_PIXEL],"//") ;
      ii = SINT(ddd+2) ;
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
   bpp = SINT(ddd+2) ;
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
   ny = SINT(ddd+2) ;
   if( ny < 2 ){ free(ppp) ; RETURN(0); }

   ddd = strstr(epos[E_COLUMNS],"//") ;
   if( ddd == NULL ){ free(ppp) ; RETURN(0); }
   nx = SINT(ddd+2) ;
   if( nx < 2 ){ free(ppp) ; RETURN(0); }

   nz = 0 ;
   if( epos[E_NUMBER_OF_FRAMES] != NULL ){
     ddd = strstr(epos[E_NUMBER_OF_FRAMES],"//") ;
     if( ddd != NULL ) nz = SINT(ddd+2) ;
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

  if(debugprint) {
    printf("str_sexinfo initially set to %d\n", PTOI(str_sexinfo) );
    printf("length %d\n", (int) strlen(str_sexinfo));
  }


   /* if assume_dicom_mosaic is not set, then require "MOSAIC" string */
   /*                                             13 Mar 2006 [rickr] */
   if( ( assume_dicom_mosaic ||
         ( epos[E_ID_IMAGE_TYPE] && strstr(epos[E_ID_IMAGE_TYPE],"MOSAIC") ) )
       && str_sexinfo != NULL   ){

     /* 31 Oct 2002: extract extra Siemens info from file */

     /* KRH 25 Jul 2003 if start and end markers are present for
      * Siemens extra info, cut string down to those boundaries */

     sexi_start = strstr(str_sexinfo, "### ASCCONV BEGIN ###");
     if(sexi_start != NULL) {  /* search for end after start - drg,fredtam 23 Mar 2007 */
        sexi_start2 = strstr(sexi_start+21, "### ASCCONV BEGIN ###");
        sexi_end = strstr(sexi_start, "### ASCCONV END ###");
        if(debugprint)
           printf("sexi_start %d sexi_start2 %d sexi_end %d\n",
                  PTOI(sexi_start), PTOI(sexi_start2),PTOI(sexi_end) );
        if (sexi_end != NULL) {
           char *sexi_tmp;
           int sexi_size;

           if((sexi_start2!=NULL) && (sexi_start2<sexi_end)) {
              sexi_start = sexi_start2;
            }

	   sexi_size = sexi_end - sexi_start + 19 ;
	   sexi_tmp = AFMALL( char, sexi_size+1 );
           if(sexi_tmp==NULL) {
              ERROR_message("Could not allocate memory for Siemens info");
              RETURN(0);
           }
	   memcpy(sexi_tmp,sexi_start,sexi_size);
           sexi_tmp[sexi_size] = '\0';
	   free(str_sexinfo);
	   str_sexinfo = sexi_tmp;
	   if(debugprint)  {
	     printf("str_sexinfo now moved to %d\n", PTOI(str_sexinfo));
	     printf("sexi_size %d\n", (int) sexi_size);
	     printf("length %d\n", (int) strlen(str_sexinfo));
	     }
        }
     }

     /* end KRH 25 Jul 2003 change */

     sexinfo.good = 0 ;  /* start by marking it as bad */
     get_siemens_extra_info( str_sexinfo , &sexinfo ) ;

     if( sexinfo.good ){                                   /* if data is good */

       if((        epos[E_ID_IMAGE_TYPE]              != NULL &&
         strstr(epos[E_ID_IMAGE_TYPE],"MOSAIC")    != NULL ) ||
         sexinfo.mosaic_num > 1 ) {

           nz = sexinfo.mosaic_num ;

       } /* end of if actually MOSAIC */

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

   } /* end of if str_sexinfo != NULL */

   free(ppp); RETURN(nz);
}

/*---------------------------------------------------------------------------*/
/*! Normally we will find the first occurance of each element in the text.
 *  Maybe the user wants to use the last, instead.     10 Apr 2009 [rickr]
-----------------------------------------------------------------------------*/

static int get_posns_from_elist(char *plist[], char *elist[], char *text,
                                int nume)
{
   static int check_env = 1;
   int    ee;
   char * cp;

   ENTRY("flip_slices_mosaic");

   if( check_env && !use_last_elem ) {
        check_env = 0;
        cp = getenv("AFNI_DICOM_USE_LAST_ELEMENT") ;
        if( cp && (*cp == 'Y' || *cp == 'y') ) {
           use_last_elem = 1 ;
           fprintf(stderr,"-- will search for last Dicom elements...\n");
        }
   }

   for( ee=0 ; ee < nume ; ee++ ) {
      plist[ee] = strstr(text,elist[ee]) ;
      if( use_last_elem && plist[ee] ) {
         while( (cp = strstr(plist[ee]+1, elist[ee])) != NULL )
            plist[ee] = cp ;
      }
   }

   RETURN(0);
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
   ar = AFMALL(char, len+1) ;                   /* make space for data */
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
   char *cpt=NULL , *dpt, *ept ;
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
     mi->slice_xyz[snum][0] = mi->slice_xyz[snum][1] = mi->slice_xyz[snum][2] = -9999.9 ;

   if( str == NULL || *str == '\0' ) return ;

   /*-- find string that starts the slice information array --*/
   /* 04 Mar 2003 reworked this section to skip "fake matches" *
    * of the target string present in some mosaic files in the *
    * binary section                                     --KRH */
   nn = 0;
   ept = str;   /* use temporary pointer instead of passed pointer to Siemens */
   if(debugprint){
     printf("Siemens extra info 1\n");
     printf("nn %d strlen str %d\n",  nn, (int) strlen(str));
   }

   /* must be able to read at least 3 of the 4 parameters in slice information */
   while ((nn < 3) && (strlen(ept) > 20)) {  /* mod drg, fredtam */
     cpt = strstr( str , "sSliceArray.asSlice[" ) ; /* 20 characters minimum */
     if( cpt == NULL ) return ;

     /* interpret next string into
         snum = slice subscript (0,1,...)
         name = variable name
         val  = number of RHS of '=' sign
         mm   = # of bytes used in scanning the above */

     nn = sscanf( cpt , "sSliceArray.asSlice[%d].%1022s =%f%n" ,
                  &snum , name , &val , &mm ) ;

     ept = cpt + 20; /* skip to end of "false match", advance to next string KRH */
   }

   /*-- scan for coordinates, until can't find a good string to scan --*/
   while(1){
     if( nn   <  3                   ) break ;  /* bad conversion set */
     if( snum <  0 || snum >= NMOMAX ) break ;  /* slice number out of range */

/* 21 Feb 2003 rework this section to allow for missing coordinates in
 * some mosaic files                                        --KRH  */
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
     if( strncmp(cpt,"sSliceArray.asSlice[",20) != 0 ) break ;   /* bad next line */
     /* 04 Mar 2003 moved this stuff around to allow for locating "fake matches"  *
      * of the target text in some mosaic files' binary sections                  */

     /* interpret next string into
         snum = slice subscript (0,1,...)
         name = variable name
         val  = number of RHS of '=' sign
         mm   = # of bytes used in scanning the above */

     nn = sscanf( cpt , "sSliceArray.asSlice[%d].%1022s =%f%n" ,
                  &snum , name , &val , &mm ) ;
   }

   /* if got at least 1 slice info, mark data as being good */
   if( last_snum >= 0 ){
     mi->good       = 1 ;
     if (have_x[0] && have_x[1]) mi->have_data[0] = 1;
     if (have_y[0] && have_y[1]) mi->have_data[1] = 1;
     if (have_z[0] && have_z[1]) mi->have_data[2] = 1;
     mi->mosaic_num = last_snum+1 ;
   }

   ept = str;
   if(cpt = strstr(str,"\nsSliceArray.ucImageNumbSag")){
      sscanf(cpt, "%1022s = %x", name, &mi->ImageNumbSag);
#if DEBUG_ON
  printf("ImageNumbSag in header %x\n",mi->ImageNumbSag);
#endif
   }
   else
      mi->ImageNumbSag = 0;
   if(cpt = strstr(str,"\nsSliceArray.ucImageNumbTra")){
      sscanf(cpt, "%1022s = %x", name, &mi->ImageNumbTra);
#if DEBUG_ON
  printf("ImageNumbTra in header %x\n",mi->ImageNumbTra);
#endif
   }
   else
      mi->ImageNumbTra = 0;
   if(cpt = strstr(str,"\nsSliceArray.ucImageNumbCor")){
      sscanf(cpt, "%1022s = %x", name, &mi->ImageNumbCor);
#if DEBUG_ON
  printf("ImageNumbCor in header %x\n",mi->ImageNumbCor);
#endif
   }
   else
      mi->ImageNumbCor = 0;

   return ;
}
/* determine if slice order is reversed in Siemens mosaic files */
static int
flip_slices_mosaic (Siemens_extra_info *mi, int kor)
{
  /*       kor = orientation of slices,  which spatial direction is z-axis */
  /* where 1=L-R, 2=P-A, 3=I-S */
   ENTRY("flip_slices_mosaic");
#if DEBUG_ON
  printf("flip_slices_mosaic kor = %d\n", kor);
  printf("ImageNumbSag,Cor,Tra= %d,%d,%d\n",mi->ImageNumbSag, mi->ImageNumbCor, mi->ImageNumbTra);
#endif
   switch(abs(kor)) {
      case 1:
        if(mi->ImageNumbSag==1)
           RETURN(1);
        RETURN(0);
      case 2:
        if(mi->ImageNumbCor==1)
           RETURN(1);
        RETURN(0);
      case 3:
        if(mi->ImageNumbTra==1)
           RETURN(1);
        RETURN(0);
      default :
         /* should never get here */
         RETURN(0);
  }
}

/* copy data from mosaic input to image array */
static int
read_mosaic_data( FILE *fp, MRI_IMAGE *im, MRI_IMARR *imar, int datum,
   Siemens_extra_info *mi, int *flip_slices, int bpp, int kor, int swap,
   float dx, float dy, float dz, float dt)
{   /*-- 28 Oct 2002:  is a 2D mosaic --*******************/

   char *dar , *iar ;
   int last_ii=-1, nvox, yy, xx, nxx, XX, YY, ii, jj, slice ;
   int mos_nx, mos_ny, mos_nz, mos_ix, mos_iy, mosaic_num;

   ENTRY("read_mosaic_data");

   /* determine if slices should be reversed */
   *flip_slices = flip_slices_mosaic(mi, kor);

   /* just to make it a little easier to read */
   mos_nx = mi->mos_nx;   mos_ny = mi->mos_ny;
   mos_ix = mi->mos_ix;   mos_iy = mi->mos_ix; /* always square */
   mos_nz = mos_ix * mos_iy ;   /* number of slices in mosaic */

#if DEBUG_ON
printf("read_mosaic_data flip_slices %d mos_nx,ny,nz = %d,%d,%d  mos_ix = %d\n",
*flip_slices,mos_nx,mos_ny,mos_nz, mos_ix);
#endif

   mosaic_num = mi->mosaic_num;

   nvox = mos_nx*mos_ny*mos_nz ;         /* total number of voxels */
   dar  = (char*)calloc(bpp,nvox) ;      /* make space for super-image */
   if(dar==NULL)  {  /* exit if can't allocate memory */
      ERROR_message("Could not allocate memory for mosaic volume");
      RETURN(0);
   }
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

   for (ii=0;ii<mosaic_num;ii++) {
      /* find right slice - may be reading the series of slices backwards */
      if(*flip_slices) slice = mosaic_num - ii -1;
      else slice = ii;
      xx = slice % mos_ix;   /* xx,yy are indices for position in mosaic matrix */
      yy = slice / mos_iy;
      im = mri_new( mos_nx , mos_ny , datum ) ;
      iar = mri_data_pointer( im ) ;             /* sub-image array */

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
   } /* end of ii sub-image loop */
   free(dar) ;  /* don't need no more; copied all data out of it now */

   /* truncate zero images out of tail of mosaic */
   if( mosaic_num < IMARR_COUNT(imar) )
       TRUNCATE_IMARR(imar,mosaic_num) ;

#if DEBUG_ON
fprintf(stderr,"\nmri_read_dicom Mosaic: mos_nx=%d mos_ny=%d mos_ix=%d mos_iy=%d slices=%d\n",
mos_nx,mos_ny,mos_ix,mos_iy,IMARR_COUNT(imar)) ;
MCHECK ;
#endif
   RETURN(0);
}


/*--------------------------------------------------------------------------*/
/*! Test if a file is possibly a DICOM file.  -- RWCox - 07 May 2003
----------------------------------------------------------------------------*/

int mri_possibly_dicom( char *fname )
{
#undef  BSIZ
#define BSIZ 4096
   FILE *fp ;
   unsigned char buf[BSIZ] , *cpt ;
   int nn , ii ;

   if( fname == NULL || *fname == '\0' ) return 0 ;
   fp = fopen( fname , "rb" ) ; if( fp == NULL ) return 0 ;

   /* read 1st buffer */

   nn = fread( buf , 1 , BSIZ , fp ) ;
   if( nn < 256 ){ fclose(fp) ; return 0 ; }  /* too short */

   /* easy: check if has 'DICM' marker at offset 128..131 */

   if( buf[128]=='D' && buf[129]=='I' && buf[130]=='C' && buf[131]=='M' ){
     fclose(fp) ; return 1 ;
   }

   /* hard: scan file for sequence: E0 7F 10 00 (image data attribute) */

   while(1){

     cpt = memchr( buf, 0xe0, nn ) ;                /* look for E0 */

     if( cpt == NULL ){                        /* skip this buffer */
       nn = fread( buf , 1 , BSIZ , fp ) ;      /* and get another */
       if( nn < 256 ){ fclose(fp) ; return 0 ; }
       continue ;
     }

     ii = nn - (cpt-buf) ;               /* num char to end of buf */
     if( ii <= 4 ){                     /* too close to end of buf */
       memmove( buf , cpt , ii ) ;
       nn = fread( buf+ii , 1 , BSIZ-ii , fp ) ; nn += ii ;
       if( nn < 256 ){ fclose(fp) ; return 0 ; }
       cpt = buf ; ii = nn ;
     }

     /* see if we got what we want */

     if( *cpt==0xe0 && *(cpt+1)==0x7f && *(cpt+2)==0x10 && *(cpt+3)==0x00 ){
       fclose(fp) ; return 1 ;
     }

     /* no?  start again at next char in buf */

     memmove( buf , cpt+1 , ii-1 ) ; nn = ii-1 ;
   }
}


/* clear oblique information structure */
static void
Clear_obl_info(oblique_info *obl_info)
{
   int i,j;

   /* start with a full clearing */
   memset(obl_info, 0, sizeof(*obl_info));

   LOAD_FVEC3(obl_info->dfpos1,0.0,0.0,0.0);
   LOAD_FVEC3(obl_info->dfpos2,0.0,0.0,0.0);
   LOAD_FVEC3(obl_info->del,0.0,0.0,0.0);
   LOAD_FVEC3(obl_info->xvec,0.0,0.0,0.0);
   LOAD_FVEC3(obl_info->yvec,0.0,0.0,0.0);
   obl_info->mosaic = 0;
   obl_info->mos_ix = obl_info->mos_nx = obl_info->mos_ny =
      obl_info->mos_nslice = 1;
   obl_info->nx = obl_info->ny = 1;
   obl_info_set = 0;
   /* make all elements zero flagging it hasn't been computed yet */
   /* lower right corner of valid MAT44 matrix is 1.0, so this is invalid */
   for(i=0;i<4;i++) {
      for(j=0;j<4;j++) {
            obl_info->Tr_dicom[i][j] = 0.0;
      }
   }
/*         memset(&obl_info->Tr_dicom[0][0], 0, 16*sizeof(float)); */
}

/* fill oblique information structure */
static void
Fill_obl_info(oblique_info *obl_info, char **epos, Siemens_extra_info *siem)
{
    float *xyz ; int qq ;
    char *ddd;
    float dx, dy, th, sp, dz;
/*    float xc1, xc2, xc3, yc1, yc2, yc3, xn, yn;*/
    int nx, ny, mos_ix, mos_iy;
    int ii;
    THD_fvec3 xc, yc;

    ENTRY("Fill_obl_info");
    if(obl_info_set) /* if already set all parameters for first slice */
       xyz = obl_info->dfpos2.xyz;   /* only need to set ImagePosition for 2nd slice */
    else
       xyz = obl_info->dfpos1.xyz;


    if(epos[E_IMAGE_POSITION] != NULL ){   /* origin position of slice */
      ddd = strstr(epos[E_IMAGE_POSITION],"//") ;
      if( ddd != NULL ){
        qq = sscanf(ddd+2,"%f\\%f\\%f",xyz,xyz+1,xyz+2) ;
      }
    }

    if(obl_info_set) {
       obl_info_set = 2;
       EXRETURN;
     }


    if( epos[E_PIXEL_SPACING] != NULL ){
      ddd = strstr(epos[E_PIXEL_SPACING],"//") ;
      if( ddd != NULL ) sscanf( ddd+2 , "%f\\%f" , &dx , &dy ) ;
      if( dy == 0.0 && dx > 0.0 ) dy = dx ;
    }

   dz = get_dz(epos);

   /* set voxel sizes */
   LOAD_FVEC3(obl_info->del, dx, dy, dz);

   if(epos[E_IMAGE_ORIENTATION] != NULL ){
     ddd = strstr(epos[E_IMAGE_ORIENTATION],"//") ;
     if( ddd != NULL ){
       qq = sscanf(ddd+2,"%f\\%f\\%f\\%f\\%f\\%f",
          &xc.xyz[0], &xc.xyz[1], &xc.xyz[2],
          &yc.xyz[0], &yc.xyz[1], &yc.xyz[2]);
       /* check if both vectors OK */
       if( qq == 6 && SIZE_FVEC3(xc) > 0.0 && SIZE_FVEC3(yc) > 0.0 ){
          NORMALIZE_FVEC3(xc);
          NORMALIZE_FVEC3(yc);
          /* if the values are close to 0 or 1 make it so */
          for(ii=0;ii<3;ii++) {
             if(ALMOST(xc.xyz[ii],0.0))
                xc.xyz[ii] = 0.0;
             if(ALMOST(xc.xyz[ii],1.0))
                xc.xyz[ii] = 1.0;
             if(ALMOST(xc.xyz[ii],-1.0))
                xc.xyz[ii] = -1.0;
             if(ALMOST(yc.xyz[ii],0.0))
                yc.xyz[ii] = 0.0;
             if(ALMOST(yc.xyz[ii],1.0))
                yc.xyz[ii] = 1.0;
             if(ALMOST(yc.xyz[ii],-1.0))
                yc.xyz[ii] = -1.0;

          }
          obl_info->xvec = xc;
          obl_info->yvec = yc;
       }
      }
   }
    obl_info_set = 1;

    /* handle Siemens mosaic data */
   if(siem->mosaic_num>1) {
      obl_info->mosaic = 1;
      obl_info->mos_sliceinfo = 0;
      /* need nx, ny in mosaic and in each slice and the real number of slices*/
      ddd = strstr(epos[E_COLUMNS],"//") ; nx = SINT(ddd+2) ;
      ddd = strstr(epos[E_ROWS]   ,"//") ; ny = SINT(ddd+2) ;

      /* get siemens in the same way as in the standard mri_read_dicom above */
      /* compute size of mosaic layout
       as 1st integer whose square is >= # of images in mosaic */
      for( mos_ix=1 ; mos_ix*mos_ix < siem->mosaic_num ; mos_ix++ ) ;

      mos_iy = mos_ix ;   /* number of subimages in each direction */
      obl_info->mos_ix = mos_ix;
      obl_info->mos_nx = nx / mos_ix ;  /* sub-image dimensions*/
      obl_info->mos_ny = ny / mos_iy ;

      obl_info->mos_nslice = siem->mosaic_num;
      obl_info->nx = nx; obl_info->ny = ny;
      obl_info_set = 2;

      /* check for alternate slice information */
      if((siem->slice_xyz[0][0] == -9999.9) ||
     (siem->slice_xyz[0][1] == -9999.9) || \
     (siem->slice_xyz[0][2] == -9999.9) || \
     (siem->slice_xyz[obl_info->mos_nslice-1][0] == -9999.9) ||
     (siem->slice_xyz[obl_info->mos_nslice-1][1] == -9999.9) || \
     (siem->slice_xyz[obl_info->mos_nslice-1][2] == -9999.9)) {
         WARNING_message( \
     "cannot compute alternative slice-based center for Siemens mosaic data\n");
      }
      else {
         obl_info->slice_xyz[0][0] = siem->slice_xyz[0][0];
         obl_info->slice_xyz[0][1] = siem->slice_xyz[0][1];
         obl_info->slice_xyz[0][2] = siem->slice_xyz[0][2];
         obl_info->slice_xyz[1][0] = siem->slice_xyz[obl_info->mos_nslice-1][0];
         obl_info->slice_xyz[1][1] = siem->slice_xyz[obl_info->mos_nslice-1][1];
         obl_info->slice_xyz[1][2] = siem->slice_xyz[obl_info->mos_nslice-1][2];
         obl_info->mos_sliceinfo = 1;
      }
   }

}

/* check if data is oblique by using the vectors from the ImageOrientation field */
static int CheckObliquity(float xc1, float xc2, float xc3, float yc1, float yc2, float yc3)
{
   int obliqueflag = 0;
   /* any values not 1 or 0 or really close mean the data is oblique */
   if ((!ALMOST(fabs(xc1),1.0) && !ALMOST(xc1,0.0)) ||
       (!ALMOST(fabs(xc2),1.0) && !ALMOST(xc2,0.0)) ||
       (!ALMOST(fabs(xc3),1.0) && !ALMOST(xc3,0.0)) ||
       (!ALMOST(fabs(yc1),1.0) && !ALMOST(yc1,0.0)) ||
       (!ALMOST(fabs(yc2),1.0) && !ALMOST(yc2,0.0)) ||
       (!ALMOST(fabs(yc3),1.0) && !ALMOST(yc3,0.0)) )
      obliqueflag = 1;
   return(obliqueflag);
}

/* mod -16 May 2007 */
/* compute Tr transformation matrix for oblique data */
/* 16 element float array */
static float *ComputeObliquity(oblique_info *obl_info)
{
/*   THD_fvec3 vec1, vec2;*/
   THD_fvec3 vec3, vec4, vec5, vec6, dc1, dc2, dc3, dc4 ;
   THD_fvec3 offsetxvec, offsetyvec,offsetzvec, Cm, Orgin, Cx;
/*   double dotp, angle, aangle;*/
   float fac=1;
   int altsliceinfo = 0;
   Siemens_extra_info *siem;
   int ii,jj;
   double Cxx, Cxy, Cxz;

   ENTRY("ComputeObliquity");
   /* compute cross product of image orientation vectors*/
   vec3 = CROSS_FVEC3(obl_info->xvec, obl_info->yvec);

   /* compute dfpos (difference between first and second
      ImagePositionPatient fields as a vector */
   vec4 = SUB_FVEC3(obl_info->dfpos2, obl_info->dfpos1);
   /* scale directions by voxel sizes*/
    dc1 = SCALE_FVEC3(obl_info->xvec, obl_info->del.xyz[0]);
    dc2 = SCALE_FVEC3(obl_info->yvec, obl_info->del.xyz[1]);
    dc3 = SCALE_FVEC3(vec3, obl_info->del.xyz[2]);

   /* if not Siemens mosaic this should be enough (GE for instance)*/
   if(!obl_info->mosaic) {
      vec5 = NORMALIZE_FVEC3(vec4);
      vec6 = NORMALIZE_FVEC3(dc3);
      fac = DOT_FVEC3(vec5, vec6);
      if(fac==0){
	 WARNING_message(
          "Bad DICOM header - assuming oblique scaling direction!");
	 fac = 1;
      }
      else {
         if(ALMOST(fac, 1.0))
            fac = 1.0;
         if(ALMOST(fac, -1.0))
            fac = -1.0;

	 if((fac!=1)&&(fac!=-1)) {
           WARNING_message("Image Positions do not lie in same direction as"
            " cross product vector: %f", fac);
	  }

	 if(fac >0) fac = 1;
	 else fac = -1;
      }
    }
    else fac = 1;
    /* switch direction of normal vector by factor */
    dc4 = SCALE_FVEC3(dc3, fac);

#ifdef DEBUG_ON
DUMP_FVEC3("xvec", obl_info->xvec);
DUMP_FVEC3("yvec", obl_info->yvec);
DUMP_FVEC3("vec3", vec3);
DUMP_FVEC3("dfpos1", obl_info->dfpos1);
DUMP_FVEC3("dfpos2", obl_info->dfpos2);
DUMP_FVEC3("vec4", vec4);
DUMP_FVEC3("del",obl_info->del);
DUMP_FVEC3("dc1", dc1);
DUMP_FVEC3("dc2", dc2);
DUMP_FVEC3("dc3", dc3);
DUMP_FVEC3("dc4", dc4);
#endif
   /*   Tr = malloc(16 * sizeof(float));*/
   /*   *Tr = dc1.xyz[0]; *(Tr+4) = dc1.xyz[1]; *(Tr+8) = dc1.xyz[2];*/
   obl_info->Tr_dicom[0][0] = dc1.xyz[0];
   obl_info->Tr_dicom[1][0] = dc1.xyz[1];
   obl_info->Tr_dicom[2][0] = dc1.xyz[2];

   /*   *(Tr+1) = dc2.xyz[0]; *(Tr+5) = dc2.xyz[1]; *(Tr+9) = dc2.xyz[2];*/
   obl_info->Tr_dicom[0][1] = dc2.xyz[0];
   obl_info->Tr_dicom[1][1] = dc2.xyz[1];
   obl_info->Tr_dicom[2][1] = dc2.xyz[2];

   /*   *(Tr+2) = dc4.xyz[0]; *(Tr+6) = dc4.xyz[1]; *(Tr+10) = dc4.xyz[2];*/
   obl_info->Tr_dicom[0][2] = dc4.xyz[0];
   obl_info->Tr_dicom[1][2] = dc4.xyz[1];
   obl_info->Tr_dicom[2][2] = dc4.xyz[2];

   /*   *(Tr+3) = obl_info->dfpos1.xyz[0]; *(Tr+7) = obl_info->dfpos1.xyz[1];
      *(Tr+11) = obl_info->dfpos1.xyz[2];*/
   obl_info->Tr_dicom[0][3] = obl_info->dfpos1.xyz[0];
   obl_info->Tr_dicom[1][3] = obl_info->dfpos1.xyz[1];
   obl_info->Tr_dicom[2][3] = obl_info->dfpos1.xyz[2];
   /*   *(Tr+12) = *(Tr+13) = *(Tr+14) = 0.0; *(Tr+15) = 1.0;*/
   obl_info->Tr_dicom[3][0] = 0; obl_info->Tr_dicom[3][1] = 0;
   obl_info->Tr_dicom[3][2] = 0; obl_info->Tr_dicom[3][3] = 1.0;


   if(!obl_info->mosaic) {
       RETURN(&(obl_info->Tr_dicom[0][0]));
   }
   /* for Siemens mosaic data, seen two cases */
#ifdef DEBUG_ON
    printf("mos_ix %d mos_nx %d, mos_ny %d, mos_nslice %d\n" \
                    "nx %d ny %d\n", \
    obl_info->mos_ix, obl_info->mos_nx, obl_info->mos_ny, obl_info->mos_nslice, \
    obl_info->nx, obl_info->ny);
#endif

   /*  Siemens mosaic with full slice information */

/* will rely on ImagePosition method for now*/
   /*  Siemens mosaic with limited slice information - rely on ImagePosition*/
   /* compute central mosaic point - in funny way */
   offsetxvec = SCALE_FVEC3(dc1, ((obl_info->nx)/2.0));
   offsetyvec = SCALE_FVEC3(dc2, ((obl_info->ny)/2.0));

   offsetzvec = SCALE_FVEC3(dc3, ((obl_info->mos_nslice - 1.0)*fac/2.0));
   if( obl_info->flip_slices ) 
      offsetzvec = SCALE_FVEC3(offsetzvec, -1.0);

   Cm = ADD_FVEC3(obl_info->dfpos1, offsetxvec);
   Cm = ADD_FVEC3(Cm, offsetyvec);
   Cm = ADD_FVEC3(Cm, offsetzvec);

  /* find origin using single slice info */
   offsetxvec = SCALE_FVEC3(dc1, ((obl_info->mos_nx - 1.0)/2.0));
   offsetyvec = SCALE_FVEC3(dc2, ((obl_info->mos_ny - 1.0)/2.0));
   offsetzvec = SCALE_FVEC3(dc3, ((obl_info->mos_nslice - 1.0)*fac/2.0));
   Orgin = SUB_FVEC3(Cm, offsetxvec);
   Orgin = SUB_FVEC3(Orgin, offsetyvec);
   Orgin = SUB_FVEC3(Orgin, offsetzvec);

   /* check if this center of the mosaic is about the same as the slice center*/
   if(obl_info->mos_sliceinfo) {
   /* find center of volume as center of vector from
      center of first slice to center of last slice */
      Cxx = (obl_info->slice_xyz[0][0] + obl_info->slice_xyz[1][0]) / 2.0;
      Cxy = (obl_info->slice_xyz[0][1] + obl_info->slice_xyz[1][1]) / 2.0;
      Cxz = (obl_info->slice_xyz[0][2] + obl_info->slice_xyz[1][2]) / 2.0;
      LOAD_FVEC3(Cx, Cxx, Cxy, Cxz);
      /* also check if the vector from slice 0 to last slice parallel or
	 anti-parallel to the slice normal */
      LOAD_FVEC3(obl_info->dfpos1,
        (obl_info->slice_xyz[1][0] - obl_info->slice_xyz[0][0]),
        (obl_info->slice_xyz[1][1] - obl_info->slice_xyz[0][1]),
        (obl_info->slice_xyz[1][2] - obl_info->slice_xyz[0][2]));
      vec5 = NORMALIZE_FVEC3(obl_info->dfpos1);
      vec6 = NORMALIZE_FVEC3(dc3);
      fac = DOT_FVEC3(vec5, vec6);
      if(fac==0){
      WARNING_message("Bad DICOM header - assuming oblique scaling direction!");
	 fac = 1;
         obl_info->mos_sliceinfo = 0;
      }
      else {
         if(ALMOST(fac, 1.0))
            fac = 1.0;
         if(ALMOST(fac, -1.0))
            fac = -1.0;

	 if((fac!=1.0)&&(fac!=-1.0)) {
           WARNING_message("Image Positions do not lie in same direction"
             " as cross product vector: %f", fac);
	  }

	 if(fac >0) fac = 1;
	 else fac = -1;
         obl_info->mos_sliceinfo = 1;
      }
     if(fac==-1) {
        INFO_message("Assuming anti-parallel (left-handed coordinate system)");
     }

      if(!ALMOST(Cm.xyz[0], Cx.xyz[0]) ||
         !ALMOST(Cm.xyz[1], Cx.xyz[1]) ||
         !ALMOST(Cm.xyz[2], Cx.xyz[2])) {
         WARNING_message("Slice-based center is different from mosaic center");
         WARNING_message("Origin computation of obliquity may be incorrect");
         DUMP_FVEC3("Mosaic Center", Cm);
         DUMP_FVEC3("Slice-based Center", Cx);
      }
      else
         INFO_message("Slice based center matches mosaic center - good!\n");
   }
   
#ifdef DEBUG_ON
   DUMP_FVEC3("Mosaic Center", Cm);
   DUMP_FVEC3("Origin Coordinates", Orgin);
#endif

   /* update 4th column of transformation matrix with computed origin */
   obl_info->Tr_dicom[0][3] = Orgin.xyz[0];
   obl_info->Tr_dicom[1][3] = Orgin.xyz[1];
   obl_info->Tr_dicom[2][3] = Orgin.xyz[2];
   /* adjust for rounding errors by setting values close to 0 or 1 to 0 or 1 */
   for(ii=0;ii<4;ii++) {
      for(jj=0;jj<4;jj++) {
         if(ALMOST(obl_info->Tr_dicom[ii][jj], 0.0))
            obl_info->Tr_dicom[ii][jj] = 0.0;
         if(ALMOST(obl_info->Tr_dicom[ii][jj], 1.0))
            obl_info->Tr_dicom[ii][jj] = 1.0;
         if(ALMOST(obl_info->Tr_dicom[ii][jj], -1.0))
            obl_info->Tr_dicom[ii][jj] = -1.0;
      }
   }

#ifdef DEBUG_ON
DUMP_FVEC3("Image Position", obl_info->dfpos1);
DUMP_FVEC3("dc1", dc1);
DUMP_FVEC3("dc2", dc2);
DUMP_FVEC3("dc3", dc3);
DUMP_FVEC3("Center of Mosaic", Cm);
DUMP_FVEC3("Origin", Orgin);
#endif

   RETURN(&(obl_info->Tr_dicom[0][0]));


#if 0
   /* compute dot product with 0,1,0 axis - put 1 in maximum index
      (closest major axis) */
   vec4.xyz[0] = 0.0; vec4.xyz[1] = 0.0; vec4.xyz[2] = 0.0;
   vec4.xyz[MAXINDEX_FVEC3(vec2)] = 1.0;
DUMP_FVEC3("vec4", vec4);
   dotp = DOT_FVEC3(vec3, vec4);

   /* compute angle as inverse cosine in degrees */
   angle = -(90.0 - 180.0*acos(dotp) / PI);
   aangle = abs(angle);
   if(aangle<0.001)
      angle = 0.0;

   RETURN(angle);
#endif

}

/* externally available function to reset oblique info */
void mri_read_dicom_reset_obliquity()
{
   Clear_obl_info(&obl_info);
}

/* externally available function to compute oblique transformation */
/* and store resulting matrix in 16 element float array */
void mri_read_dicom_get_obliquity(float *Tr)
{
   float *fptr;
   int i,j;

   fptr = Tr;
   if(obl_info_set)  /* if oblique info is filled in, even partially */
     ComputeObliquity(&obl_info); /* compute proper transformation or warn */
                /* otherwise just ignore it-no warnings (for non-DICOM data) */
   for(i=0;i<4;i++)
      for(j=0;j<4;j++)
          *fptr++ = obl_info.Tr_dicom[i][j];

/*   memcpy(Tr, fptr, 16*sizeof(float));*/

   return;
}


/* externally available function to set origin and orientation from obliquity */
/* transformation matrix */
void
Obliquity_to_coords(THD_3dim_dataset *tdset)
{
   THD_ivec3 orixyz;

   mat44 nmat; int icod, jcod, kcod;
   int orimap[7] = { 6 , 1 , 0 , 2 , 3 , 4 , 5 } ;
   float dxtmp, dytmp, dztmp ;
   THD_dataxes      * daxes   = NULL ;

   daxes = tdset->daxes;
   nmat = daxes->ijk_to_dicom_real;
   /* negate first two rows to go from RAI to LPI definition for nifti */
   nmat.m[0][0] = -nmat.m[0][0];
   nmat.m[0][1] = -nmat.m[0][1];
   nmat.m[0][2] = -nmat.m[0][2];
   nmat.m[0][3] = -nmat.m[0][3];
   nmat.m[1][0] = -nmat.m[1][0];
   nmat.m[1][1] = -nmat.m[1][1];
   nmat.m[1][2] = -nmat.m[1][2];
   nmat.m[1][3] = -nmat.m[1][3];
   /* from thd_niftiread.c */
   /* convert nifti orientation codes to AFNI codes and store in vector */

   nifti_mat44_to_orientation( nmat , &icod, &jcod, &kcod ) ;
   LOAD_IVEC3( orixyz , orimap[icod] ,
                        orimap[jcod] ,
                        orimap[kcod] ) ;

   /* load the offsets and the grid spacings */

   daxes->xxorg = daxes->ijk_to_dicom_real.m[ORIENT_xyzint[orixyz.ijk[0]] - 1][3] ;
   daxes->yyorg = daxes->ijk_to_dicom_real.m[ORIENT_xyzint[orixyz.ijk[1]] - 1][3] ;
   daxes->zzorg = daxes->ijk_to_dicom_real.m[ORIENT_xyzint[orixyz.ijk[2]] - 1][3] ;

   daxes->xxorient = orimap[icod] ;
   daxes->yyorient = orimap[jcod] ;
   daxes->zzorient = orimap[kcod] ;

   dxtmp = fabs(daxes->xxdel);
   dytmp = fabs(daxes->yydel);
   dztmp = fabs(daxes->zzdel);

   daxes->xxdel =  (ORIENT_sign[orixyz.ijk[0]]=='+') ? dxtmp : -dxtmp ;
   daxes->yydel =  (ORIENT_sign[orixyz.ijk[1]]=='+') ? dytmp : -dytmp ;
   daxes->zzdel =  (ORIENT_sign[orixyz.ijk[2]]=='+') ? dztmp : -dztmp ;

 #if DEBUG_ON
 printf("Orients = %d %d % d\n", daxes->xxorient, daxes->yyorient, \
       daxes->zzorient);
 printf("daxes origins = %f %f %f\n", daxes->xxorg, daxes->yyorg, \
       daxes->zzorg);
 #endif
 /*     daxes->xxdel    =   user_inputs.xsize ;
   daxes->yydel    =   user_inputs.ysize ;*/

 }
