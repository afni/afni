#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <sys/types.h>

#include "mri_image.h"
#include "mri_dicom_hdr.h"
#include "Amalloc.h"
#include "dbtrace.h"

/*----------------------------------------------------------------------
 * dimon_afni.c
 *
 * This is mostly mri_read_dicom.c, plus some funtions solen from
 * other files.  It is for reading Dicom files, but without having
 * to link libmri.a (and also libXm, to be specific).
 *
 * May, 2005 [rickr] : changes from mri_read_dicom.c:
 *
 *   - inserted some of the MRILIB globals, but using DI_MRL (search)
 *   - added (formerly external) functions: swap_twobytes(),
 *       swap_fourbytes(), set_endianosity(), mri_possibly_dicom()
 *   - removed mri_imcount_dicom()
 *   - added gr_dimon_stuff as a global struct
 *   - added study, series and image numbers to global elist
 *   - added debugging
 *   - RESCALE is now automatic, if found
 *   - WINDOW is ignored
 *   - sexinfo is ignored
 *   - mosaics are not allowed (this is used for slice processing only)
 *   - no mri_new() call, intialize fields here
 *   - a data pointer may be passed in (as NULL if no data is to be read)
 *   - more E_ fields are now required
 *
 * 29 July 2005 [rickr] : updates for Dimon
 *   - mri_read_dicom failure messages (on debug level 3+)
 *   - close the file early when not reading data
 *----------------------------------------------------------------------
*/

/* misc stuff to keep locally (MRILIB -> DI_MRL) */
extern char * mri_dicom_header( char * );
extern void   mri_dicom_pxlarr( off_t *, unsigned int * ) ;
extern void   mri_dicom_noname( int ) ;
extern void   mri_dicom_nohex ( int ) ;

static int   use_DI_MRL_xcos   = 0;
static int   use_DI_MRL_ycos   = 0;
static int   use_DI_MRL_zcos   = 0;

static float DI_MRL_xcos[3]    = { 1.0, 0.0, 0.0 };
static float DI_MRL_ycos[3]    = { 0.0, 1.0, 0.0 };
static float DI_MRL_zcos[3]    = { 0.0, 0.0, 1.0 };

static float DI_MRL_xoff       = 0.0;
static float DI_MRL_yoff       = 0.0;
static float DI_MRL_zoff       = 0.0;
static int   use_DI_MRL_xoff   = 0;
static int   use_DI_MRL_yoff   = 0;
static int   use_DI_MRL_zoff   = 0;

/* exported MRI-style globals */

char  DI_MRL_orients[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
float DI_MRL_tr         = 0.0;

extern unsigned long l_THD_filesize( char * pathname );

void swap_twobytes( int nvals, void * data )
{
   char * dp0 = (char *)data;
   char * dp1 = dp0 + 1;
   int    c;

   for( c = 0; c < nvals; c++, dp0 += 2, dp1 += 2 )
   {
      *dp0 ^= *dp1;
      *dp1 ^= *dp0;
      *dp0 ^= *dp1;
   }
}

void swap_fourbytes( int nvals, void * data )
{
   char * dp0 = (char *)data;
   char * dp1 = dp0 + 1;
   char * dp2 = dp0 + 2;
   char * dp3 = dp0 + 3;
   int    c;

   for( c = 0; c < nvals; c++, dp0 += 4, dp1 += 4, dp2 += 4, dp3 += 4 )
   {
      *dp0 ^= *dp3; *dp3 ^= *dp0; *dp0 ^= *dp3;
      *dp1 ^= *dp2; *dp2 ^= *dp1; *dp1 ^= *dp2;
   }
}

/*--------------------------------------------------------------------*/
/* useful Dicom information that is not stored in an MRI_IMAGE struct */
struct dimon_stuff_t {
   int study, series, image;
} gr_dimon_stuff;

/*-------------------------------------------------------------------------*/

static int LITTLE_ENDIAN_ARCHITECTURE = -1 ;

static void set_endianosity(void)
{
   long val = 1;

   LITTLE_ENDIAN_ARCHITECTURE = (*(char *)&val == 1);
}

/*-------------------------------------------------------------------------*/

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

 "0020 0010" ,  /* study number  */
 "0020 0011" ,  /* series number */
 "0020 0013" ,  /* image number  */

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

#define E_RS_STUDY_NUM               28    /* 10 Feb 2005: for Imon [rickr] */
#define E_RS_SERIES_NUM              29
#define E_RS_IMAGE_NUM               30

/*-------------------------------------------------------------------------*/
/*! Read image(s) from a DICOM file, if possible.
---------------------------------------------------------------------------*/

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


/* modify mri_read_dicom for Dimon's purposes   April 2005 [rickr]
   note: return a single MRI_IMAGE, not an MRI_IMARR */
MRI_IMAGE * r_mri_read_dicom( char *fname, int debug, void ** data )
{
   char *ppp , *ddd ;
   off_t poff ;
   unsigned int plen ;
   char *epos[NUM_ELIST] ;
   int ii,jj , ee , bpp , datum ;
   int nx,ny,nz , swap ;
   float dx,dy,dz,dt ;
   MRI_IMAGE *im ;
   void *iar ;
   FILE *fp ;
   int have_orients=0 ;
   int ior,jor,kor ;
   static int nzoff=0 ;   /* for determining z-axis orientation/offset from multiple calls */

   float dxx,dyy,dzz ;

   char *eee ;
   float rescale_slope=0.0 , rescale_inter=0.0 ;  /* 23 Dec 2002 */

   ENTRY("mri_read_dicom") ;

   if( !mri_possibly_dicom(fname) )
   {
      if(debug > 2) fprintf(stderr,"** MRD: mri_possibly_dicom() failure\n");
      RETURN(NULL) ;  /* 07 May 2003 */
   }

   /* extract header info from file into a string
      - cf. mri_dicom_hdr.[ch]
      - run 'dicom_hdr -noname fname' to see the string format */

   mri_dicom_nohex(1) ;              /* don't print ints in hex mode */
   mri_dicom_noname(1) ;             /* don't print names, just tags */
   ppp = mri_dicom_header( fname ) ; /* print header to malloc()-ed string */
   if( ppp == NULL )
   {
      if(debug > 2) fprintf(stderr,"** MRD: mri_dicom_hdr() failure\n");
      RETURN(NULL);   /* didn't work; not a DICOM file? */
   }

   /* find out where the pixel array is in the file */

   mri_dicom_pxlarr( &poff , &plen ) ;
   if( plen <= 0 ){
      if(debug > 2) fprintf(stderr,"** MRD: bad plen, %u\n", plen);
      free(ppp) ;
      RETURN(NULL);
   }
   if( debug > 3 )
      fprintf(stderr,"-d dicom: poff, plen = %d, %d\n",(int)poff,(int)plen);

   /* check if file is actually this big (maybe it was truncated) */

   { unsigned int psiz , fsiz ;
     fsiz = l_THD_filesize( fname ) ;
     psiz = (unsigned int)(poff) + plen ;
     if( fsiz < psiz ){
        if(debug > 2) fprintf(stderr,"** MRD: fsiz < psiz (%d,%d)\n",fsiz,psiz);
        free(ppp) ;
        RETURN(NULL);
     }
   }

   /* find positions in header of elements we care about */

   for( ee=0 ; ee < NUM_ELIST ; ee++ )
     epos[ee] = strstr(ppp,elist[ee]) ;

   /* see if the header has the elements we absolutely need */

   if( epos[E_ROWS]           == NULL ||
       epos[E_COLUMNS]        == NULL ||
       epos[E_BITS_ALLOCATED] == NULL   ){
      if(debug > 2)
         fprintf(stderr,"** MRD: missing ROWS, COLS or BITS (%p,%p,%p)\n",
                 epos[E_ROWS], epos[E_COLUMNS], epos[E_BITS_ALLOCATED]);
      free(ppp) ;
      RETURN(NULL);
   }

   /* check if we have 1 sample per pixel (can't deal with 3 or 4 now) */

   if( epos[E_SAMPLES_PER_PIXEL] != NULL ){
      ddd = strstr(epos[E_SAMPLES_PER_PIXEL],"//") ;
      if( ddd == NULL ){
         if(debug > 2) fprintf(stderr,"** MRD: missing E_SAMPLES_PER_PIXEL\n");
         free(ppp) ;
         RETURN(NULL);
      }
      ii = 0 ; sscanf(ddd+2,"%d",&ii) ;
       if( ii != 1 ){
         if(debug > 2) fprintf(stderr,"** MRD: SAM_PER_PIX != 1, %d\n",ii);
         free(ppp) ;
         RETURN(NULL);
      }
   }

   /* check if photometric interpretation is MONOCHROME (don't like PALETTE) */

   if( epos[E_PHOTOMETRIC_INTERPRETATION] != NULL ){
      ddd = strstr(epos[E_PHOTOMETRIC_INTERPRETATION],"MONOCHROME") ;
      if( ddd == NULL ){
      if(debug > 2) fprintf(stderr,"** MRD: photometric not monochrome\n");
        free(ppp) ;
        RETURN(NULL);
      }
   }

   /* check if we have 8, 16, or 32 bits per pixel */

   ddd = strstr(epos[E_BITS_ALLOCATED],"//") ;
   if( ddd == NULL ){
      if(debug > 2) fprintf(stderr,"** MRD: missing BITS_ALLOCATED\n");
      free(ppp);
      RETURN(NULL);
   }
   bpp = 0 ; sscanf(ddd+2,"%d",&bpp) ;
   switch( bpp ){
      default:
        if(debug > 2) fprintf(stderr,"** MRD: bad bpp value, %d\n",bpp);
        free(ppp); RETURN(NULL);    /* bad value */
      case  8: datum = MRI_byte ; break ;
      case 16: datum = MRI_short; break ;
      case 32: datum = MRI_int  ; break ;  /* probably not present in DICOM? */
   }
   bpp /= 8 ; /* now bytes per pixel, instead of bits */

   if( debug > 3 ) fprintf(stderr,"-d dicom: datum %d\n",datum);

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

   if( epos[E_RESCALE_INTERCEPT] != NULL && epos[E_RESCALE_SLOPE] != NULL ){
     if( debug > 0 ){
       static int nwarn=0 ;
       if( nwarn == 0 ){
         fprintf(stderr,"++ DICOM file has Rescale tags, enforcing them...\n");
         fprintf(stderr,"   (no more Rescale tags messages will be printed)\n");
       }
       nwarn++ ;
     }
     ddd = strstr(epos[E_RESCALE_INTERCEPT],"//") ;
     sscanf(ddd+2,"%f",&rescale_inter) ;
     ddd = strstr(epos[E_RESCALE_SLOPE    ],"//") ;
     sscanf(ddd+2,"%f",&rescale_slope) ;
   }

   /*** extract attributes of the image(s) to be read in ***/

   /* get image nx & ny */

   ddd = strstr(epos[E_ROWS],"//") ;                         /* 31 Oct 2002: */
   if( ddd == NULL ){                 /* Oops: ROWS is ny and COLUMNS is nx! */
      if(debug > 2) fprintf(stderr,"** MRD: missing E_ROWS\n");
      free(ppp) ;
      RETURN(NULL);
   }
   ny = 0 ; sscanf(ddd+2,"%d",&ny) ;
   if( ny < 2 ){
      if(debug > 2) fprintf(stderr,"** MRD: bad ny = %d\n",ny);
      free(ppp) ;
      RETURN(NULL);
   }

   ddd = strstr(epos[E_COLUMNS],"//") ;
   if( ddd == NULL ){
      if(debug > 2) fprintf(stderr,"** MRD: missing E_COLUMNS\n");
      free(ppp) ;
      RETURN(NULL);
   }
   nx = 0 ; sscanf(ddd+2,"%d",&nx) ;
   if( nx < 2 ){
      if(debug > 2) fprintf(stderr,"** MRD: bad nx = %d\n",nx);
      free(ppp) ;
      RETURN(NULL);
   }

   /* get number of slices */

   nz = 0 ;
   if( epos[E_NUMBER_OF_FRAMES] != NULL ){
     ddd = strstr(epos[E_NUMBER_OF_FRAMES],"//") ;
     if( ddd != NULL ) sscanf(ddd+2,"%d",&nz) ;
     if(debug>3) fprintf(stderr,"-d number of frames = %d\n",nz);
   }

   /* if didn't get nz above, make up a value */

   if( nz == 0 ) nz = plen / (bpp*nx*ny) ;   /* compute from image array size */
   if( nz == 0 ){
      if(debug > 2) fprintf(stderr,"** MRD: bad nz = %d\n", nz);
      free(ppp) ;
      RETURN(NULL);
   }

   if( nz != 1 ){
      fprintf(stderr,"** MRD: nz = %d, plen,bpp,nx,ny = %d,%d,%d,%d\n",
         nz, plen, bpp, nx, ny);
      free(ppp);
      RETURN(NULL);
   }

   /*-- 28 Oct 2002: Check if this is a Siemens mosaic.        --*/
   /*-- 02 Dec 2002: Don't use Acquisition Matrix anymore;
                     instead, use the Siemens extra info
                     in epos[E_SIEMENS_2].                     --*/
   /*-- 24 Dec 2002: Extract str_sexinfo even if not a mosaic. --*/

   /* ignore sexinfo for Dimon, since it becomes a mosaic-only case */

   if(        epos[E_ID_MANUFACTURER]            != NULL &&
       strstr(epos[E_ID_MANUFACTURER],"SIEMENS") != NULL &&
              epos[E_SIEMENS_2]                  != NULL    ){
      if( debug > 3 ) fprintf(stderr,"-d ignoring sexinfo\n");
   }

   /*-- try to get dx, dy, dz, dt --*/

   dx = dy = dz = dt = 0.0 ;

   /* dx,dy first */

   if( epos[E_PIXEL_SPACING] != NULL ){
     ddd = strstr(epos[E_PIXEL_SPACING],"//") ;
     if( ddd != NULL ) sscanf( ddd+2 , "%f\\%f" , &dx , &dy ) ;
     if( debug > 3 )
        fprintf(stderr,"-d dicom: read dx,dy from PIXEL_SP: %f, %f\n", dx, dy);
     if( dy == 0.0 && dx > 0.0 ) dy = dx ;
   }
   if( dx == 0.0 && epos[E_FIELD_OF_VIEW] != NULL ){
     ddd = strstr(epos[E_FIELD_OF_VIEW],"//") ;
     if( ddd != NULL ) sscanf( ddd+2 , "%f\\%f" , &dx , &dy ) ;
     if( debug > 3 )
        fprintf(stderr,"-d dicom: read dx,dy from FOV: %f, %f\n", dx, dy);
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
                                                           /* 03 Mar 2003 */
     eee           = getenv("AFNI_SLICE_SPACING_IS_GAP") ;
     stupid_ge_fix = (eee != NULL && (*eee=='Y' || *eee=='y') );
     no_stupidity  = (eee != NULL && (*eee=='N' || *eee=='n') );

     if( epos[E_SLICE_SPACING] != NULL ){    /* get reported slice spacing */
       ddd = strstr(epos[E_SLICE_SPACING],"//") ;
       if( ddd != NULL ) sscanf( ddd+2 , "%f" , &sp ) ;
     }
     if( epos[E_SLICE_THICKNESS] != NULL ){  /* get reported slice thickness */
       ddd = strstr(epos[E_SLICE_THICKNESS],"//") ;
       if( ddd != NULL ) sscanf( ddd+2 , "%f" , &th ) ;
     }

     if(debug>3) fprintf(stderr,"-d dicom: thick, spacing = %f, %f\n", th, sp);

     th = fabs(th) ; sp = fabs(sp) ;         /* we don't use the sign */

     if( stupid_ge_fix ){                    /* always be stupid */
       dz = sp+th ;
     } else {

       if( no_stupidity && sp > 0.0 )        /* 13 Jan 2004: if 'NO', then */
         dz = sp ;                           /* always use spacing if present */
       else
         dz = (sp > th) ? sp : th ;          /* the correct-ish DICOM way */

#define GFAC 0.99

       if( !no_stupidity ){                 /* unless stupidity is turned off */
         if( sp > 0.0 && sp < GFAC*th ) dz = sp+th;/* the stupid GE way again */

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
     if( dz == 0.0 && dx != 0.0 ) dz = 1.0 ;               /* nominal dz */

   } /*-- end of dz code, with all its stupidities --*/

   if(debug > 3) fprintf(stderr,"-d dicom: using dxyz = %f, %f, %f\n",dx,dy,dz);

   /* get dt */

   if( epos[E_REPETITION_TIME] != NULL ){
     ddd = strstr(epos[E_REPETITION_TIME],"//") ;
     if( ddd != NULL ){
       sscanf( ddd+2 , "%f" , &dt ) ;
       dt *= 0.001 ;   /* ms to s */
       if(debug > 3) fprintf(stderr,"-d dicom: rep. time dt = %f sec.\n",dt);
     }
   }

   /* check if we might have 16 bit unsigned data that fills all bits */

   if( epos[E_RS_STUDY_NUM] != NULL ){
     ddd = strstr(epos[E_RS_STUDY_NUM],"//") ;
     if( ddd != NULL ) sscanf( ddd+2 , "%d" , &gr_dimon_stuff.study);
   }

   if( epos[E_RS_SERIES_NUM] != NULL ){
     ddd = strstr(epos[E_RS_SERIES_NUM],"//") ;
     if( ddd != NULL ) sscanf( ddd+2 , "%d" , &gr_dimon_stuff.series);
   }

   if( epos[E_RS_IMAGE_NUM] != NULL ){
     ddd = strstr(epos[E_RS_IMAGE_NUM],"//") ;
     if( ddd != NULL ) sscanf( ddd+2 , "%d" , &gr_dimon_stuff.image);
   }

   /** Finally! Read images from file. **/

   fp = fopen( fname , "rb" ) ;
   if( fp == NULL ){
      if(debug > 2) fprintf(stderr,"** MRD: failed to open file '%s'\n",fname);
      free(ppp) ;
      RETURN(NULL);
   }
   lseek( fileno(fp) , poff , SEEK_SET ) ;

   /* DICOM files are stored in LSB first (little endian) mode */

   set_endianosity() ; swap = !LITTLE_ENDIAN_ARCHITECTURE ;

   /* use pre-(28 Oct 2002) method, mosaics are not allowed now */

   { /* no mri_new(), as we don't want to link everything */
     im = (MRI_IMAGE *)calloc(1, sizeof(MRI_IMAGE));
     if( !im ){
        fprintf(stderr,"** MRD: im malloc failure!\n");
        free(ppp);
        RETURN(NULL);
     }
     im->nx = nx;  im->ny = ny;
     im->nxy = nx * ny;
     im->nz = im->nt = im->nu = im->nv = im->nw = 1;
     im->nxyz = im->nxyzt = im->nvox = im->nxy; 
     im->kind = datum;
     im->dx = im->dy = im->dz = im->dt = im->du = im->dv = 1.0;
     im->dw = -666.0;
     im->pixel_size = bpp;
     if( data ){ /* if data is not set, do not read it in */
        if( ! *data ){
           *data = calloc(im->nvox, im->pixel_size);
           if( debug > 3 )
              fprintf(stderr,"+d dicom: image data alloc: %d x %d bytes\n",
                 im->nvox, im->pixel_size);
        }
        if( ! *data ){
           fprintf(stderr,"** MRD: image data alloc failure\n");
           free(ppp);
           RETURN(NULL);
        } 
     }
   }

   if( dx > 0.0 && dy > 0.0 && dz > 0.0 ){
     im->dx = dx; im->dy = dy; im->dz = dz; im->dw = 1.0;
   }
   if( dt > 0.0 ) im->dt = dt ;
   
   if( !data ) fclose(fp) ; 
   else{
      iar = *data;
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
   
      /* store auxiliary data in image struct */
   
      fclose(fp) ;     /* 10 Sep 2002: oopsie - forgot to close file */
   
      /*-- 23 Dec 2002: implement Rescale, if ordered --*/
   
      if( rescale_slope > 0.0 ){
        for( ii=0 ; ii < 1 ; ii++ ){
          switch( im->kind ){
            case MRI_byte:{
              byte *ar = (byte *)*data;
              for( jj=0 ; jj < im->nvox ; jj++ )
                ar[jj] = rescale_slope*ar[jj] + rescale_inter ;
            }
            break ;
   
            case MRI_short:{
              short *ar = (short *)*data;
              for( jj=0 ; jj < im->nvox ; jj++ )
                ar[jj] = rescale_slope*ar[jj] + rescale_inter ;
            }
            break ;
   
            case MRI_int:{
              int *ar = (int *)*data;
              for( jj=0 ; jj < im->nvox ; jj++ )
                ar[jj] = rescale_slope*ar[jj] + rescale_inter ;
            }
            break ;
            default:{
              fprintf(stderr,"** MRD: bad kind case (%d) for rescale_slope\n",
                      im->kind);
              free(ppp);  free(*data);  *data = NULL;
              RETURN(NULL);
            }
            break ;
          }
        }
      } /* end of Rescale */
   }

   /*-- no WINDOW in Dimon */

   /*-- store some extra information in DI_MRL globals, too? --*/

   if( dt > 0.0 && DI_MRL_tr <= 0.0 ) DI_MRL_tr = dt ;

   /*-- try to get image orientation fields (also, set ior,jor,kor) --*/

   if( epos[E_IMAGE_ORIENTATION] != NULL ){
     /* direction cosines of image plane */

     ddd = strstr(epos[E_IMAGE_ORIENTATION],"//") ;
     if( ddd != NULL ){
       float xc1=0.0,xc2=0.0,xc3=0.0 , yc1=0.0,yc2=0.0,yc3=0.0 ;
       float xn,yn ; int qq ;
       qq=sscanf(ddd+2,"%f\\%f\\%f\\%f\\%f\\%f",&xc1,&xc2,&xc3,&yc1,&yc2,&yc3);
       xn = sqrt( xc1*xc1 + xc2*xc2 + xc3*xc3 ) ; /* vector norms */
       yn = sqrt( yc1*yc1 + yc2*yc2 + yc3*yc3 ) ;
       if( qq == 6 && xn > 0.0 && yn > 0.0 ){     /* both vectors OK */

         xc1 /= xn ; xc2 /= xn ; xc3 /= xn ;      /* normalize vectors */
         yc1 /= yn ; yc2 /= yn ; yc3 /= yn ;

         if( !use_DI_MRL_xcos ){
           DI_MRL_xcos[0] = xc1 ; DI_MRL_xcos[1] = xc2 ;  /* save direction */
           DI_MRL_xcos[2] = xc3 ; use_DI_MRL_xcos = 1 ;   /* cosine vectors */
         }

         if( !use_DI_MRL_ycos ){
           DI_MRL_ycos[0] = yc1 ; DI_MRL_ycos[1] = yc2 ;
           DI_MRL_ycos[2] = yc3 ; use_DI_MRL_ycos = 1 ;
         }

         /* x-axis orientation */
         /* ior determines which spatial direction is x-axis  */
         /* and is the direction that has the biggest change */

         dxx = fabs(xc1) ; ior = 1 ;
         dyy = fabs(xc2) ; if( dyy > dxx ){ ior=2; dxx=dyy; }
         dzz = fabs(xc3) ; if( dzz > dxx ){ ior=3;        }
         dxx = DI_MRL_xcos[ior-1] ; if( dxx < 0. ) ior = -ior;

         if( DI_MRL_orients[0] == '\0' ){
           switch( ior ){
             case -1: DI_MRL_orients[0] = 'L'; DI_MRL_orients[1] = 'R'; break;
             case  1: DI_MRL_orients[0] = 'R'; DI_MRL_orients[1] = 'L'; break;
             case -2: DI_MRL_orients[0] = 'P'; DI_MRL_orients[1] = 'A'; break;
             case  2: DI_MRL_orients[0] = 'A'; DI_MRL_orients[1] = 'P'; break;
             case  3: DI_MRL_orients[0] = 'I'; DI_MRL_orients[1] = 'S'; break;
             case -3: DI_MRL_orients[0] = 'S'; DI_MRL_orients[1] = 'I'; break;
             default: DI_MRL_orients[0] ='\0'; DI_MRL_orients[1] ='\0'; break;
           }
         }

         /* y-axis orientation */
         /* jor determines which spatial direction is y-axis  */
         /* and is the direction that has the biggest change */

         dxx = fabs(yc1) ; jor = 1 ;
         dyy = fabs(yc2) ; if( dyy > dxx ){ jor=2; dxx=dyy; }
         dzz = fabs(yc3) ; if( dzz > dxx ){ jor=3;        }
         dyy = DI_MRL_ycos[jor-1] ; if( dyy < 0. ) jor = -jor;
         if( DI_MRL_orients[2] == '\0' ){
           switch( jor ){
             case -1: DI_MRL_orients[2] = 'L'; DI_MRL_orients[3] = 'R'; break;
             case  1: DI_MRL_orients[2] = 'R'; DI_MRL_orients[3] = 'L'; break;
             case -2: DI_MRL_orients[2] = 'P'; DI_MRL_orients[3] = 'A'; break;
             case  2: DI_MRL_orients[2] = 'A'; DI_MRL_orients[3] = 'P'; break;
             case  3: DI_MRL_orients[2] = 'I'; DI_MRL_orients[3] = 'S'; break;
             case -3: DI_MRL_orients[2] = 'S'; DI_MRL_orients[3] = 'I'; break;
             default: DI_MRL_orients[2] ='\0'; DI_MRL_orients[3] ='\0'; break;
           }
         }

         DI_MRL_orients[6] = '\0' ;   /* terminate orientation string */

         kor = 6 - abs(ior)-abs(jor) ;   /* which spatial direction is z-axis */
                                         /* where 1=L-R, 2=P-A, 3=I-S */
         have_orients = 1 ;

         if( debug > 3 )
           fprintf(stderr,"-d dicom: DI_MRL_orients = %s, [ijk]or = %d,%d,%d\n",
                   DI_MRL_orients, ior, jor, kor);
       }
       else if( debug > 3 )
         fprintf(stderr,"-d dicom: bad orient vectors qq = %d, xn,yn = %f,%f\n",
                 qq, xn, yn);
     }

   } else if( epos[E_PATIENT_ORIENTATION] != NULL ){
              /* symbolic orientation of image [not so useful, or common] */
     ddd = strstr(epos[E_PATIENT_ORIENTATION],"//") ;
     if( ddd != NULL ){
       char xc='\0' , yc='\0' ;
       sscanf(ddd+2,"%c\\%c",&xc,&yc) ;   /* e.g., "L\P" */
       switch( toupper(xc) ){
         case 'L': DI_MRL_orients[0]='L'; DI_MRL_orients[1]='R'; ior=-1; break;
         case 'R': DI_MRL_orients[0]='R'; DI_MRL_orients[1]='L'; ior= 1; break;
         case 'P': DI_MRL_orients[0]='P'; DI_MRL_orients[1]='A'; ior=-2; break;
         case 'A': DI_MRL_orients[0]='A'; DI_MRL_orients[1]='P'; ior= 2; break;
         case 'F': DI_MRL_orients[0]='I'; DI_MRL_orients[1]='S'; ior= 3; break;
                   /* F=foot */
         case 'H': DI_MRL_orients[0]='S'; DI_MRL_orients[1]='I'; ior=-3; break;
                   /* H=head */
         default:  DI_MRL_orients[0]='\0';DI_MRL_orients[1]='\0'; ior= 0; break;
       }
       switch( toupper(yc) ){
         case 'L': DI_MRL_orients[2]='L'; DI_MRL_orients[3]='R'; jor=-1; break;
         case 'R': DI_MRL_orients[2]='R'; DI_MRL_orients[3]='L'; jor= 1; break;
         case 'P': DI_MRL_orients[2]='P'; DI_MRL_orients[3]='A'; jor=-2; break;
         case 'A': DI_MRL_orients[2]='A'; DI_MRL_orients[3]='P'; jor= 2; break;
         case 'F': DI_MRL_orients[2]='I'; DI_MRL_orients[3]='S'; jor= 3; break;
         case 'H': DI_MRL_orients[2]='S'; DI_MRL_orients[3]='I'; jor=-3; break;
         default:  DI_MRL_orients[2]='\0';DI_MRL_orients[3]='\0'; jor= 0; break;
       }
       DI_MRL_orients[6]='\0' ;      /* terminate orientation string */
       kor = 6 - abs(ior)-abs(jor) ;   /* which spatial direction is z-axis */
       have_orients = (ior != 0 && jor != 0) ;

       if( debug > 3 )
         fprintf(stderr,"-d dicom: DI_MRL_orients P = %s, [ijk]or = %d,%d,%d\n",
                 DI_MRL_orients, ior, jor, kor);
     }

   }  /* end of 2D image orientation */

   if( !have_orients ){
      fprintf(stderr,"** MRD: failed to determine dicom image orientation\n");
      free(ppp);  free(im);
      if( data ){ free(*data); *data = NULL; }
      RETURN(NULL);
   }

   /* skip mosaic use for finishing orientation string */

   /** use image position vector to set offsets,
       and (2cd time in) the z-axis orientation **/

   if( epos[E_IMAGE_POSITION] == NULL ){
      fprintf(stderr,"** MRD: missing image position\n");
      free(ppp);  free(im);
      if( data ){ free(*data); *data = NULL; }
      RETURN(NULL);
   }

   ddd = strstr(epos[E_IMAGE_POSITION],"//") ;
   if( ddd == NULL ){
      fprintf(stderr,"** MRD: missing IMAGE_POSITION\n");
      free(ppp);  free(im);
      if( data ){ free(*data); *data = NULL; }
      RETURN(NULL);
   }

   {   /* old mri_read_dicom: line 982 */
       float xyz[3] ; int qq ;
       qq = sscanf(ddd+2,"%f\\%f\\%f",xyz,xyz+1,xyz+2) ;
       if( qq != 3 ){
         fprintf(stderr,"** MRD: failed to read IMAGE_POSITION\n");
         free(ppp); free(im);
         if( data ){ free(*data); *data = NULL; }
         RETURN(NULL);
       }
       /* now use [ijk]or from above */
       im->xo = xyz[abs(ior)-1];
       im->yo = xyz[abs(jor)-1];
       im->zo = xyz[abs(kor)-1];

       if( debug > 3 )
          fprintf(stderr,"-d dicom: read RAI image position %f, %f, %f\n"
                         "          and dset image position %f, %f, %f\n",
                  xyz[0], xyz[1], xyz[2], im->xo, im->yo, im->zo );

       /* fill the orients string */
       {
         static float zoff ;      /* saved from nzoff=0 case */
         float zz = xyz[kor-1] ;  /* kor from orients above */

         if( nzoff == 0 ){  /* 1st DICOM image */

           zoff = zz ;      /* save this for 2nd image calculation */

           /* 01 Nov 2002: in mosaic case, may have set this already */

           if( DI_MRL_orients[4] == '\0' ){
             switch( kor ){   /* may be changed on second image */
               case  1: DI_MRL_orients[4] = 'L'; DI_MRL_orients[5] = 'R'; break;
               case  2: DI_MRL_orients[4] = 'P'; DI_MRL_orients[5] = 'A'; break;
               case  3: DI_MRL_orients[4] = 'I'; DI_MRL_orients[5] = 'S'; break;
               default: DI_MRL_orients[4] ='\0'; DI_MRL_orients[5] ='\0'; break;
             }
             DI_MRL_orients[6] = '\0' ;
           }

           /* Save x,y offsets of this 1st slice */

           qq = abs(ior) ;
           DI_MRL_xoff = xyz[qq-1] ; use_DI_MRL_xoff = 1 ;
           if( ior > 0 ) DI_MRL_xoff = -DI_MRL_xoff ;

           qq = abs(jor) ;
           DI_MRL_yoff = xyz[qq-1] ; use_DI_MRL_yoff = 1 ;
           if( jor > 0 ) DI_MRL_yoff = -DI_MRL_yoff ;

         } else if( nzoff == 1 && !use_DI_MRL_zoff ){  /* 2nd DICOM image */

           float qoff = zz - zoff ;    /* vive la difference */
           if( qoff < 0 ) kor = -kor ; /* kor determines z-axis orientation */
#if 0
fprintf(stderr,"  nzoff=1 kor=%d qoff=%f\n",kor,qoff) ;
#endif
           switch( kor ){
             case  1: DI_MRL_orients[4] = 'R'; DI_MRL_orients[5] = 'L'; break;
             case -1: DI_MRL_orients[4] = 'L'; DI_MRL_orients[5] = 'R'; break;
             case  2: DI_MRL_orients[4] = 'A'; DI_MRL_orients[5] = 'P'; break;
             case -2: DI_MRL_orients[4] = 'P'; DI_MRL_orients[5] = 'A'; break;
             case  3: DI_MRL_orients[4] = 'I'; DI_MRL_orients[5] = 'S'; break;
             case -3: DI_MRL_orients[4] = 'S'; DI_MRL_orients[5] = 'I'; break;
             default: DI_MRL_orients[4] ='\0'; DI_MRL_orients[5] ='\0'; break;
           }
           DI_MRL_orients[6] = '\0' ;

           /* save spatial offset of first slice              */
           /* [this needs to be positive in the direction of] */
           /* [the -z axis, so may need to change its sign  ] */

           DI_MRL_zoff = zoff ; use_DI_MRL_zoff = 1 ;
           if( kor > 0 ) DI_MRL_zoff = -DI_MRL_zoff ;
         }
         nzoff++ ;  /* 3rd and later images don't count for z-orientation */
       }
   }  /* end of using image position */

   /** 23 Dec 2002:
       use slice location value to set z-offset,
       and (2cd time in) the z-axis orientation
       -- only try this if image position vector (code above) isn't present
          AND if we don't have a mosaic image (which already did this stuff)
       -- shouldn't be necessary, since slice location is deprecated        **/

   /* if this is here, use it for additional accuracy
      (it must basically match current zo) */

   { /* just use one warning counter, since these should be unique */
     static int nwarn = 0;

     if( ( epos[E_SLICE_LOCATION] == NULL) ||
         ( (ddd = strstr(epos[E_SLICE_LOCATION],"//")) == NULL ) )
     {
       if( nwarn == 0 && debug > 1 )
          fprintf(stderr,"-d dimon: missing SLICE_LOCATION, continuing...\n");
       nwarn++;
     } else {
        /* get and test the slice location */
        float zz ; int qq;
        qq = sscanf(ddd+2,"%f",&zz) ;
        if( qq != 1 ){
           if( !nwarn )fprintf(stderr,"** failed to extract SLICE_LOCATION\n");
           nwarn++;
        } else {
           /* it seems we have to add our own sign to the slice location */
           if( zz * im->zo < 0.0 ){
              if( (nwarn == 0) && (debug > 3) )
                fprintf(stderr,"-d image and slice loc diff in sign: %f, %f\n",
                        im->zo, zz);
              nwarn++;
              zz = -zz;
           }
  
           if( fabs(zz - im->zo) > 0.1 ){
              fprintf(stderr,
                      "** MRD: IMAGE_LOCATION and SLICE_LOCATION disagree!\n"
                      "   z coord from IL = %f, from SL = %f\n", im->zo,zz);
              free(ppp); free(im);
              if( data ){ free(*data); *data = NULL; }
              RETURN(NULL);
           }

           if( debug > 3 )
              fprintf(stderr,"-d dicom: using slice location %f (zo = %f)\n",
                      zz, im->zo );
           im->zo = zz;
        }
     }
   }

   free(ppp);  /* free the ASCII header */

   RETURN( im );
}
