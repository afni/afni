#include "mrilib.h"

/*-----------------------------------------------------------------------------------*/

#define NMOMAX 256  /* max mosaic size: have never seen one this big! */
typedef struct {
  int good ;                       /* data in here is any good?              */
  int have_xyz[3] ;                /* do we have slices 0 and 1 in           *
                                    * each dimension to determine z-spacing? *
                                    * added 25 Feb 2003 KRH                  */
  int mosaic_num ;                 /* how many slices detected in 1 'image'  */
  float slice_xyz[NMOMAX][3] ;     /* Sag, Cor, Tra coordinates              */

  int   have_normal_x ,            /* 24 Jan 2006 additions: */
        have_normal_y ,
        have_normal_z  ;           /* do we have the normal components below? */
  float      normal_x ,            /* first slice normals */
             normal_y ,
             normal_z  ;
  int   have_inplane_rot ;         /* and the inplane rotation angle? */
  float      inplane_rot ;
  int   have_AAmatrix ;            /* and the Auto-Align matrix? */
  float      AAmatrix[16] ;
  float thickness ;
  float phaseFOV ;
  float readoutFOV ;
} Siemens_extra_info ;

static char * extract_bytes_from_file( FILE *fp, off_t start, size_t len, int strize ) ;
static void get_siemens_extra_info( char *str , Siemens_extra_info *mi ) ;

static int mosaic_num_method1( char *sexinfo ) ;  /* 18 Jan 2006 */
static int mosaic_num_method2( char *sexinfo ) ;
static int mosaic_num_method3( char *sexinfo ) ;

 /* 20 Jan 2006: move windowing and rescaling operations to functions */

static void mri_dicom_rescaler( MRI_IMAGE *im ,
                                float rescale_slope , float rescale_inter ) ;

static void mri_dicom_window_1( MRI_IMAGE *im , float wbot , int ymax ) ;
static void mri_dicom_window_2( MRI_IMAGE *im ,
                                float wbot, float wtop, float wfac, int ymax ) ;

/*-----------------------------------------------------------------------------------*/
/* Save the Siemens extra info string in case the caller wants to get it. */

static char *str_sexinfo=NULL ;
static char *str_sex1010=NULL ;                        /* 18 Jan 2006 */

char * mri_dicom_sexinfo(void){ return str_sexinfo; }  /* 23 Dec 2002 */
char * mri_dicom_sex1010(void){ return str_sex1010; }  /* 18 Jan 2006 */

/*-----------------------------------------------------------------------------------*/

static int LITTLE_ENDIAN_ARCHITECTURE = -1 ;

static void RWC_set_endianosity(void)   /* set LITTLE_ENDIAN_ARCHITECTURE */
{
   union { unsigned char bb[2] ;
           short         ss    ; } fred ;

   if( LITTLE_ENDIAN_ARCHITECTURE < 0 ){
     fred.bb[0] = 1 ; fred.bb[1] = 0 ;

     LITTLE_ENDIAN_ARCHITECTURE = (fred.ss == 1) ;
   }
   return ;
}

/*-----------------------------------------------------------------------------------*/
/* List of attribute tags that we potentially give a damn about */

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

 "0018 1312" ,  /* Phase Encoding Direction */

NULL } ;  /* NULL marks the end of the list */

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

#define E_ACQ_PHASE_ENCDIR           28    /* 18 Jan 2006 */

/*-----------------------------------------------------------------------------------*/
#undef  CLEANUP   /* macro to delete resources on exit from mri_read_dicom() */
#define CLEANUP                  \
  do{ if(ppp!=NULL)free(ppp) ;   \
      if(fp !=NULL)fclose(fp);   \
  } while(0)
/*-----------------------------------------------------------------------------------*/
/*! Read image(s) from a DICOM file, if possible.
-------------------------------------------------------------------------------------*/

MRI_IMARR * mri_read_dicom( char *fname )
{
   char *ppp=NULL , *ddd ;
   off_t poff ;                    /* offset into file of image data */
   unsigned int plen ;             /* length of image data recorded in header */
   char *epos[NUM_ELIST] ;         /* list of pointers to desired attributes */
   int ii,jj , ee , bpp , datum ;
   int nx,ny,nz , swap ;
   float dx,dy,dz,dt ;
   MRI_IMARR *imar ;               /* array of output images */
   MRI_IMAGE *im ;                 /* one output image */
   void *iar ;
   FILE *fp=NULL ;                 /* pointer to opened fname */
   int have_orients=0 ;
   int ior=0,jor=0,kor=0 ;
   static int nzoff=0; /* for getting z-axis orient/offset frm multiple files */

   int mosaic=0 , mos_nx,mos_ny , mos_ix,mos_iy,mos_nz ;  /* 28 Oct 2002 */
   Siemens_extra_info sexinfo ;                           /* 31 Oct 2002 */
   float xcen,ycen,zcen ;
   int use_xycen=0 ;
   float dxx,dyy,dzz ;

   int mosaic_count=0 , mosaic_mark=0 ;                   /* 18 Jan 2006 */
   int acq_mat_xxx=0 , acq_mat_yyy=0 ;

   char *eee ;
   float rescale_slope=0.0f, rescale_inter=0.0f;          /* 23 Dec 2002 */
   float window_center=0.0f, window_width =0.0f;

   char *sexi_start;                                  /* KRH 25 Jul 2003 */
   char *sexi_end;

   int   have_cosines=0 ;
   float xc1=0.0f,xc2=0.0f,xc3=0.0f ,        /* 24 Jan 2006: promoted up */
         yc1=0.0f,yc2=0.0f,yc3=0.0f ,
         zc1=0.0f,zc2=0.0f,zc3=0.0f  ;
   float xcc=0.0f,ycc=0.0f,zcc=0.0f , have_cc=0 ;         /* 26 Jan 2006 */

   /*.......................................................................*/
   ENTRY("mri_read_dicom") ;

   /*-- clean up any trash left from last call --*/

   if( str_sexinfo != NULL ){ free(str_sexinfo); str_sexinfo=NULL; }
   if( str_sex1010 != NULL ){ free(str_sex1010); str_sex1010=NULL; }

   /*-- make sure this COULD be a DICOM file --*/

   if( !mri_possibly_dicom(fname) ) RETURN(NULL) ;  /* 07 May 2003 */

   /*--- extract header info from file into a string
         - cf. mri_dicom_hdr.[ch]
         - run 'dicom_hdr -noname fname' to see the string format ----------*/

   mri_dicom_nohex(1) ;              /* don't print ints in hex mode */
   mri_dicom_noname(1) ;             /* don't print names, just tags */
   ppp = mri_dicom_header( fname ) ; /* print header to malloc()-ed string */
   if( ppp == NULL ) RETURN(NULL);   /* didn't work; not a DICOM file? */

   /*-------- find out where the pixel array is in the file --------*/

   mri_dicom_pxlarr( &poff , &plen ) ;
   if( plen == 0 ){ CLEANUP; RETURN(NULL); }  /* failed == bad file! */

   /*----- check if file is actually this big (maybe it was truncated) -----*/

   { unsigned int psiz , fsiz ;
     fsiz = THD_filesize( fname ) ;                    /* size of actual file */
     psiz = (unsigned int)(poff) + plen ; /* size we need = offset+image size */
     if( fsiz < psiz ){ CLEANUP; RETURN(NULL); }             /* not big enuf? */
   }

   /*----- open file for the rest of this function -----*/

   if( fp == NULL ){
     fp = fopen( fname , "rb" ) ;
     if( fp == NULL ){ CLEANUP; RETURN(NULL); }
   }

   /*------- find positions in header of elements we care about -------*/

   for( ee=0 ; ee < NUM_ELIST ; ee++ )
     epos[ee] = strstr(ppp,elist[ee]) ;

   /*----- see if the header has the elements we absolutely need -----*/

   if( epos[E_ROWS]           == NULL ||
       epos[E_COLUMNS]        == NULL ||
       epos[E_BITS_ALLOCATED] == NULL   ){ CLEANUP; RETURN(NULL); }

   /*-- check if we have 1 sample per pixel (can't deal with 3 or 4 now) --*/

   if( epos[E_SAMPLES_PER_PIXEL] != NULL ){
     ddd = strstr(epos[E_SAMPLES_PER_PIXEL],"//") ;
     if( ddd == NULL ){ CLEANUP; RETURN(NULL); }
     ii = 0 ; sscanf(ddd+2,"%d",&ii) ;
     if( ii != 1 ){ CLEANUP; RETURN(NULL); }
   }

   /* check if photometric interpretation is MONOCHROME (don't like PALETTE) */

   if( epos[E_PHOTOMETRIC_INTERPRETATION] != NULL ){
     ddd = strstr(epos[E_PHOTOMETRIC_INTERPRETATION],"MONOCHROME") ;
     if( ddd == NULL ){ CLEANUP; RETURN(NULL); }
   }

   /*--------- check if we have 8, 16, or 32 bits per pixel ---------*/

   ddd = strstr(epos[E_BITS_ALLOCATED],"//") ;
   if( ddd == NULL ){ CLEANUP; RETURN(NULL); }
   bpp = 0 ; sscanf(ddd+2,"%d",&bpp) ;    /* bpp = bits per pixel */
   switch( bpp ){
     default: CLEANUP; RETURN(NULL);      /* bad value */
     case  8: datum = MRI_byte ; break ;
     case 16: datum = MRI_short; break ;
     case 32: datum = MRI_int  ; break ;  /* probably not present in DICOM? */
   }
   bpp /= 8 ; /* bpp = bytes per pixel now, instead of bits */

   /***------------ Print some warnings if appropriate ------------***/

   /*----- check if BITS_STORED and HIGH_BIT are aligned -----*/

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
         fprintf(stderr,
          "++ DICOM WARNING: no more 'Bits_Stored' messages\n") ;
       nwarn++ ;
     }
   }

   /*----- check if Rescale is ordered ------------------------------------*/
   /* 23 Dec 2002: actually get the rescale params, if environment says to */

   if( epos[E_RESCALE_INTERCEPT] != NULL && epos[E_RESCALE_SLOPE] != NULL ){
     if( !AFNI_yesenv("AFNI_DICOM_RESCALE") ){
       static int nwarn=0 ;
       if( nwarn < NWMAX )
         fprintf(stderr,
          "++ DICOM WARNING: file %s has Rescale tags;\n"
          "++   setenv AFNI_DICOM_RESCALE YES to enforce them\n",
          fname ) ;
       if( nwarn == NWMAX )
         fprintf(stderr,
          "++ DICOM WARNING: no more 'Rescale tags' messages\n") ;
       nwarn++ ;
     } else {
       ddd = strstr(epos[E_RESCALE_INTERCEPT],"//"); sscanf(ddd+2,"%f",&rescale_inter);
       ddd = strstr(epos[E_RESCALE_SLOPE    ],"//"); sscanf(ddd+2,"%f",&rescale_slope);
     }
   }

   /*----- check if Window is ordered ------------------------------------*/
   /* 23 Dec 2002: actually get the window params, if environment says to */

   if( epos[E_WINDOW_CENTER] != NULL && epos[E_WINDOW_WIDTH] != NULL ){
     if( !AFNI_yesenv("AFNI_DICOM_WINDOW") ){
       static int nwarn=0 ;
       if( nwarn < NWMAX )
         fprintf(stderr,
          "++ DICOM WARNING: file %s has Window tags;\n"
          "++   setenv AFNI_DICOM_WINDOW YES to enforce them\n",
          fname ) ;
       if( nwarn == NWMAX )
         fprintf(stderr,
          "++ DICOM WARNING: no more 'Window tags' messages\n") ;
       nwarn++ ;
     } else {
       ddd = strstr(epos[E_WINDOW_CENTER],"//"); sscanf(ddd+2,"%f",&window_center);
       ddd = strstr(epos[E_WINDOW_WIDTH ],"//"); sscanf(ddd+2,"%f",&window_width );
     }
   }

   /***---------- extract attributes of the image(s) to be read in ----------***/

   /*----- get image nx & ny ------------------------------------------------*/

   ddd = strstr(epos[E_ROWS],"//") ;              /* 31 Oct 2002: */
   if( ddd == NULL ){ CLEANUP; RETURN(NULL); }    /* Oops: ROWS is ny and */
   ny = 0 ; sscanf(ddd+2,"%d",&ny) ;              /*       COLUMNS is nx! */
   if( ny < 2 ){ CLEANUP; RETURN(NULL); }

   ddd = strstr(epos[E_COLUMNS],"//") ;
   if( ddd == NULL ){ CLEANUP; RETURN(NULL); }
   nx = 0 ; sscanf(ddd+2,"%d",&nx) ;
   if( nx < 2 ){ CLEANUP; RETURN(NULL); }

   /*----- get number of slices into nz (not used for mosaics) -----*/

   nz = 0 ;
   if( epos[E_NUMBER_OF_FRAMES] != NULL ){
     ddd = strstr(epos[E_NUMBER_OF_FRAMES],"//") ;
     if( ddd != NULL ) sscanf(ddd+2,"%d",&nz) ;
   }

   /*----- if didn't get nz above, make up a value -----*/

   if( nz == 0 ) nz = plen / (bpp*nx*ny) ;    /* compute from image array size */
   if( nz == 0 ){ CLEANUP; RETURN(NULL); }   /* not enough data for 1 image?! */

   /*----- 28 Oct 2002: Check if this is a Siemens mosaic.        -----*/
   /*----- 02 Dec 2002: Don't use Acquisition Matrix anymore;
                        instead, use the Siemens extra info
                        in epos[E_SIEMENS_2].                     -----*/
   /*----- 24 Dec 2002: Extract str_sexinfo even if not a mosaic. -----*/
   /*----- 18 Jan 2006: Extract str_sex1010 in addition.          -----*/

   if(        epos[E_ID_MANUFACTURER]            != NULL &&
       strstr(epos[E_ID_MANUFACTURER],"SIEMENS") != NULL    ){

     /*--- str_sexinfo get the (0029,1020) data ---*/

     if( epos[E_SIEMENS_2] != NULL ){
       int len=0,loc=0 , aa,bb ;
       sscanf(epos[E_SIEMENS_2],"%x%x%d [%d" , &aa,&bb , &len,&loc ) ;
       if( len > 0 && loc > 0 )
         str_sexinfo = extract_bytes_from_file( fp, (off_t)loc, (size_t)len,1 );
     }

     /*--- str_sex1010 get the (0029,1010) data [18 Jan 2006] ---*/

     if( epos[E_SIEMENS_1] != NULL ){
       int len=0,loc=0 , aa,bb ;
       sscanf(epos[E_SIEMENS_1],"%x%x%d [%d" , &aa,&bb , &len,&loc ) ;
       if( len > 0 && loc > 0 )
         str_sex1010 = extract_bytes_from_file( fp, (off_t)loc, (size_t)len,1 );
     }

   } /* end of extracting special Siemens fields (but not end of using them) */

   /*--- 18 Jan 2006: process acquisition matrix, set acq_mat_* values ---*/
   /*-   these are the number of k-space points in each direction, which -*/
   /*-   we'll use as a lower bound on the size of the mosaic sub-images -*/
   /*-   (one COULD reconstruct to lower resolution, but that's weirdo)  -*/

   if( epos[E_ACQ_MATRIX] != NULL ){
     ddd = strstr(epos[E_ACQ_MATRIX],"//") ;
     if( ddd != NULL ){
       int a1=0,a2=0,a3=0,a4=0 , na ;

       /* the normal format for this is 4 integers (e.g., "0 64 64 0"),
          but in each pair, only one of them is nonzero;
          which one is nonzero varies in ways I don't understand */

       na = sscanf(ddd+2,"%d%d%d%d",&a1,&a2,&a3,&a4) ;  /* get up to 4 */

       switch(na){   /* na = number of values actually found */
         case 1:
           acq_mat_xxx = acq_mat_yyy = a1 ;
         break ;
         case 2:
           acq_mat_xxx = a1 ; acq_mat_yyy = a2 ;
         break ;
         case 4:                      /** the only case **/
           acq_mat_xxx = MAX(a1,a2) ; /** I've actually **/
           acq_mat_yyy = MAX(a3,a4) ; /** ever found    **/
         break ;
       }
     }

     /*-- swap them if phase direction corresponds to image col direction --*/

     if( acq_mat_xxx != acq_mat_yyy && epos[E_ACQ_PHASE_ENCDIR] != NULL ){
       ddd = strstr(epos[E_ACQ_MATRIX],"//COL") ;
       if( ddd != NULL && ddd-epos[E_ACQ_MATRIX] < 66 ){
         int tmp=acq_mat_xxx ; acq_mat_xxx=acq_mat_yyy; acq_mat_yyy=tmp;
       }
     }
   }

   /*-- (OLD) process str_sexinfo only if this is marked as a mosaic image ---*/

   /*-- (NEWER) preliminary processing of sexinfo even if not marked as MOSAIC,
        SINCE UNMARKED MOSAICS ARE TURNING UP IN THE WILD!  KRH, 11/6/05 -----*/

   /*-- 18 Jan 2006: now we check for mosaic-ness in a more complex way,
        but first we still look for the MOSAIC flag as a hint - RWC ----------*/

   if( epos[E_ID_IMAGE_TYPE] != NULL ){          /* check if marked as MOSAIC */
     ddd = strstr(epos[E_ID_IMAGE_TYPE],"MOSAIC") ;
     mosaic_mark = (ddd != NULL) && ((ddd-epos[E_ID_IMAGE_TYPE]) < 66) ;
   }

   memset( &sexinfo , 0 , sizeof(sexinfo) ) ; /* start by marking it as bad */

   if( str_sexinfo != NULL || str_sex1010 != NULL ){

     /* KRH 25 Jul 2003: if start and end markers are present for
      * Siemens extra info, cut string down to those boundaries */

     if( str_sexinfo != NULL ){
       sexi_start = strstr(str_sexinfo, "### ASCCONV BEGIN ###");
       sexi_end   = strstr(str_sexinfo, "### ASCCONV END ###"  );
       if( (sexi_start != NULL) && (sexi_end != NULL) ){
         char *sexi_tmp;
         int sexi_size;

         sexi_size = sexi_end - sexi_start + 19 ;
         sexi_tmp = AFMALL( char, sexi_size ) ;
         memcpy(sexi_tmp,sexi_start,sexi_size) ;
         free(str_sexinfo) ;
         str_sexinfo = sexi_tmp ; str_sexinfo[sexi_size-1] = '\0' ;
       }
     }
     /* end KRH 25 Jul 2003 change */

     /** 18 Jan 2006: use the new methods to find out the mosaic count,
         (RWC)       which has shown up in different ways in various samples **/

                            mosaic_count = mosaic_num_method1(str_sex1010) ;
     if( mosaic_count < 2 ) mosaic_count = mosaic_num_method2(str_sexinfo) ;
     if( mosaic_count < 2 ) mosaic_count = mosaic_num_method3(str_sexinfo) ;

     /*** marked as MOSAIC in header, but can't get mosaic count? Complain! ***/

     if( mosaic_count < 2 && mosaic_mark ){
       static int nwarn=0 ;
       if( nwarn < NWMAX )
         fprintf(stderr,
          "++ DICOM WARNING: file marked as MOSAIC but can't get mosaic count!\n");
       if( nwarn == NWMAX )
         fprintf(stderr,
          "++ DICOM NOTICE: no more Siemens 'MOSAIC mark' messages\n");
       nwarn++ ;
     }

     /*---- if we have a mosaic image count, then try to figure it out ----*/

     if( mosaic_count > 1 ){

       /*-- find size of square (mos_ix) that holds this many sub-images --*/

       for( mos_ix=2 ; mos_ix*mos_ix < mosaic_count ; mos_ix++ ) ; /*nada*/
       mos_iy = mos_ix ;              /* mosaic is mos_ix * mos_iy sub-images */
       mos_nx = nx / mos_ix; mos_ny = ny / mos_iy;    /* sub-image dimensions */
       mos_nz = mos_ix * mos_iy ;         /* total number of slices in mosaic
                                                       (not all need be good) */

       /**--- check for various stupid mismatches;
              if they happen, then don't consider this a true mosaic ---**/

                                                         /** sub-images are  **/
       if( mos_ix*mos_nx != nx || mos_iy*mos_ny != ny ){ /** not an even fit **/
         static int nwarn=0 ;                            /** into image data **/
         if( nwarn < NWMAX )
           fprintf(stderr,
            "++ DICOM WARNING: bad Siemens Mosaic params:"
            " nx=%d ny=%d ix=%d iy=%d imx=%d imy=%d\n",
            mos_nx,mos_ny , mos_ix,mos_iy , nx,ny ) ;
         if( nwarn == NWMAX )
           fprintf(stderr,
            "++ DICOM NOTICE: no more Siemens Mosaic 'bad param' messages\n");
         nwarn++ ;
                                                                  /* sub-images */
       } else if( mos_nx < acq_mat_xxx || mos_ny < acq_mat_yyy ){ /* too small  */
         static int nwarn=0 ;                                     /* vs acq_mat */
         if( nwarn < NWMAX )
           fprintf(stderr,
            "++ DICOM WARNING: bogus Siemens Mosaic params:"
            " nx=%d ny=%d acq_x=%d acq_y=%d\n",
            mos_nx,mos_ny , acq_mat_xxx,acq_mat_yyy ) ;
         if( nwarn == NWMAX )
           fprintf(stderr,
            "++ DICOM NOTICE: no more Siemens Mosaic 'bogus param' messages\n");
         nwarn++ ;

                                                     /*** file doesn't ***/
       } else if( plen < bpp*mos_nx*mos_ny*mos_nz ){ /*** have enough  ***/
         static int nwarn=0 ;                        /*** data in it!  ***/
         if( nwarn < NWMAX )
           fprintf(stderr,
            "++ DICOM WARNING: Siemens Mosaic file is too short"
            " for %d sub-images!\n",mos_nz) ;
         if( nwarn == NWMAX )
           fprintf(stderr,
            "++ DICOM NOTICE: no more Siemens Mosaic 'too short' messages\n");
         nwarn++ ;
                       /**********************************/
       } else {        /**** WE ARE HAPPY HAPPY HAPPY ****/
         mosaic = 1 ;  /**** MARK THIS AS A MOSAIC!!! ****/
       }               /**********************************/
     }

     /*--- at this point,
           variable 'mosaic' indicates if we think this is a mosaic image,
           and 'mosaic_mark' indicates if this file was marked as a MOSAIC ---*/

     if( mosaic != mosaic_mark ){  /* incoherent combination, but does occur */
       static int nwarn=0 ;
       if( nwarn < NWMAX )
         fprintf(stderr,
          "++ DICOM WARNING: %s find mosaic count, but %s find MOSAIC mark!\n",
          (mosaic)     ? "did" : "didn't" ,
          (mosaic_mark)? "did" : "didn't"  ) ;
       if( nwarn == NWMAX )
         fprintf(stderr,
          "++ DICOM NOTICE: no more 'mosaic!=MOSAIC' messages\n");
       nwarn++ ;
     }

     /*-- now try to extract coordinate info about slices from str_sexinfo --*/
     /*-- NOTE: we do this whether or not the file is an image mosaic      --*/

     if( str_sexinfo != NULL ){
       get_siemens_extra_info( str_sexinfo , &sexinfo ) ; /** scan it, baby **/

       if( !sexinfo.good ){   /*** no end of the possible troubles ***/
         static int nwarn=0 ;
         if( nwarn < NWMAX )
           fprintf(stderr,
            "++ DICOM WARNING: undecodable Siemens extra info in file %s\n",
            fname ) ;
         if( nwarn == NWMAX )
           fprintf(stderr,
            "++ DICOM NOTICE: no more Siemens 'undecodable' messages\n");
         nwarn++ ;
       }
     }

   } /*-- end of if str_sexinfo exists at all --*/

   /*--------------- try to get dx, dy, dz, dt (grid sizes) ---------------*/

   dx = dy = dz = dt = 0.0f ;  /* defaults */

   /*---- dx,dy first ----*/

   if( epos[E_PIXEL_SPACING] != NULL ){
     ddd = strstr(epos[E_PIXEL_SPACING],"//") ;
     if( ddd != NULL ) sscanf( ddd+2 , "%f\\%f" , &dx , &dy ) ;
     if( dy == 0.0f && dx > 0.0f ) dy = dx ;
   }
   if( dx == 0.0f && epos[E_FIELD_OF_VIEW] != NULL ){  /* the backup method */
     ddd = strstr(epos[E_FIELD_OF_VIEW],"//") ;
     if( ddd != NULL ) sscanf( ddd+2 , "%f\\%f" , &dx , &dy ) ;
     if( dx > 0.0f ){
       if( dy == 0.0f ) dy = dx ;
       dx /= nx ; dy /= ny ;
     }
   }

   /*-- now try to get dz (slice thickness) --*/

   /*-- 27 Nov 2002: fix stupid GE error,
                     where the slice spacing is really the slice gap --*/

   { int stupid_ge_fix , no_stupidity ;
     float sp=0.0f , th=0.0f ;
     static int nwarn=0 ;

     eee           = getenv("AFNI_SLICE_SPACING_IS_GAP") ;
     stupid_ge_fix = (eee != NULL && (*eee=='Y' || *eee=='y') ) ;
     no_stupidity  = (eee != NULL && (*eee=='N' || *eee=='n') ) ;  /* 03 Mar 2003 */

     if( epos[E_SLICE_SPACING] != NULL ){               /* get reported slice spacing */
       ddd = strstr(epos[E_SLICE_SPACING],"//") ;
       if( ddd != NULL ) sscanf( ddd+2 , "%f" , &sp ) ;
     }
     if( epos[E_SLICE_THICKNESS] != NULL ){             /* get reported slice thickness */
       ddd = strstr(epos[E_SLICE_THICKNESS],"//") ;
       if( ddd != NULL ) sscanf( ddd+2 , "%f" , &th ) ;
     }

     th = fabs(th) ; sp = fabs(sp) ;                    /* we don't use the sign */

     if( stupid_ge_fix ){                               /* always be stupid */
       dz = sp+th ;
     } else {

       if( no_stupidity && sp > 0.0f )                  /* 13 Jan 2004: if 'NO', then */
         dz = sp ;                                      /* always use spacing if present */
       else
         dz = (sp > th) ? sp : th ;                     /* the correct-ish DICOM way */

#define GFAC 0.99

       if( !no_stupidity ){                             /* unless stupidity is turned off */
         if( sp > 0.0f && sp < GFAC*th ) dz = sp+th ;   /* the stupid GE way again */

         if( sp > 0.0f && sp < GFAC*th && nwarn < NWMAX ){
           fprintf(stderr, "++ DICOM WARNING: file %s has Slice_Spacing=%f"
                           " smaller than Slice_Thickness=%f\n",
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
              "++   to be necessary for some GE scanner setups.                 ++\n"
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
         if( sp > 0.0f && sp < th && nwarn == NWMAX )
           fprintf(stderr,
            "++ DICOM WARNING: no more 'Slice_Spacing' messages\n") ;
         nwarn++ ;
       }
     }
     if( dz == 0.0f && dx != 0.0f ) dz = 1.0 ;               /* nominal dz */

   } /*-- end of dz code, with all its stupidities --*/

   /*---- get dt (TR) ----*/

   if( epos[E_REPETITION_TIME] != NULL ){
     ddd = strstr(epos[E_REPETITION_TIME],"//") ;
     if( ddd != NULL ){
       sscanf( ddd+2 , "%f" , &dt ) ;
       dt *= 0.001f ;                  /* convert ms to s */
     }
   }

   /**********-----------------------------------------------------**********/
   /**********---------- Finally! Read images from file. ----------**********/

   rewind(fp) ;                 /* rewind needed on Mac, but don't know why */
   lseek( fileno(fp) , poff , SEEK_SET ) ;   /* seek to start of image data */

   INIT_IMARR(imar) ;    /* output image array */

   /*-- DICOM image data stored in LSB first (little endian) mode --*/

   RWC_set_endianosity() ; swap = !LITTLE_ENDIAN_ARCHITECTURE ;

   /*------- 28 Oct 2002: must allow for evil 2D mosaic mode -------*/

   if( !mosaic ){   /*-- 28 Oct 2002: old method, not a mosaic --*/
                    /*                (ah, the good olde days)   */

    for( ii=0 ; ii < nz ; ii++ ){          /* nz is probably 1 */
      im  = mri_new( nx , ny , datum ) ;   /* new MRI_IMAGE struct */
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

      /*--- store auxiliary data in image struct ---*/

      if( dx > 0.0f && dy > 0.0f && dz > 0.0f ){
        im->dx = dx; im->dy = dy; im->dz = dz; im->dw = 1.0;
      }
      if( dt > 0.0f ) im->dt = dt ;

      ADDTO_IMARR(imar,im) ;

    } /* end of loop over sub-images (usually, only 1 sub-image) */

   /*--------------------------------------------------------------------*/
   } else {   /*-------- 28 Oct 2002:  is a vile 2D mosaic --------------*/

     char *dar , *iar ;
     int last_ii=-1 , nvox , yy,xx,nxx ;

     /*--- read the super-image into array dar all at once ---*/

     nvox = mos_nx*mos_ny*mos_nz ;         /* total number voxels to read */
     dar  = (char*)calloc(bpp,nvox) ;     /* make space for super-image  */
     fread( dar , bpp , nvox , fp ) ;    /* read data directly into it  */
     if( swap ){                        /* swap bytes after input?     */
       switch( bpp ){
         default: break ;
         case 2:  swap_twobytes (   nvox, dar ) ; break ;  /* short */
         case 4:  swap_fourbytes(   nvox, dar ) ; break ;  /* int, float */
         case 8:  swap_fourbytes( 2*nvox, dar ) ; break ;  /* complex */
       }
     }

     /*--- load data from dar into sub-images ---*/

     nxx = mos_nx * mos_ix ;              /* # pixels per mosaic line */

     for( yy=0 ; yy < mos_iy ; yy++ ){    /* loop over sub-images in mosaic */
       for( xx=0 ; xx < mos_ix ; xx++ ){

         im  = mri_new( mos_nx , mos_ny , datum ) ;  /* new sub-image  */
         iar = mri_data_pointer( im ) ;             /* sub-image array */

         /*-- copy data rows from dar into iar --*/

         for( jj=0 ; jj < mos_ny ; jj++ )  /* loop over rows inside sub-image */
           memcpy( iar + jj*mos_nx*bpp ,
                   dar + xx*mos_nx*bpp + (jj+yy*mos_ny)*nxx*bpp ,
                   mos_nx*bpp                                    ) ;

         /*-- load auxiliary data into sub-image struct --*/

         if( dx > 0.0f && dy > 0.0f && dz > 0.0f ){
           im->dx = dx; im->dy = dy; im->dz = dz; im->dw = 1.0;
         }
         if( dt > 0.0f ) im->dt = dt ;
         if( swap ) im->was_swapped = 1 ;

         ADDTO_IMARR(imar,im) ;
       }
     }
     free(dar) ;  /* don't need no more; copied all data out of it now */

     /*--- truncate all-zero images out of tail of mosaic ---*/

#if 1
     if( mosaic_count < IMARR_COUNT(imar) )
       TRUNCATE_IMARR(imar,mosaic_count) ;
#endif

#if 0
     /* the old way: find the last image with a nonzero value inside */

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
#endif

   } /* end of mosaic input mode */

   fclose(fp); fp = NULL;  /* 10 Sep 2002: oopsie - forgot to close file */

   /*----- 23 Dec 2002: implement Rescale, if ordered -----*/

   if( rescale_slope > 0.0f ){
     for( ii=0 ; ii < IMARR_COUNT(imar) ; ii++ )
       mri_dicom_rescaler( IMARR_SUBIM(imar,ii), rescale_slope, rescale_inter );
   }

   /*----- 23 Dec 2002: implement Window, if ordered -----*/

   if( window_width >= 1.0 ){
     float wbot,wtop,wfac ;
     int ymax=0 ;                /* max value of data storable in image */

     /*-- get output range --*/

     ddd = strstr(epos[E_BITS_STORED],"//") ;
     if( ddd != NULL ){
       sscanf(ddd+2,"%d",&ymax) ;
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

       for( ii=0 ; ii < IMARR_COUNT(imar) ; ii++ )
         mri_dicom_window_1( IMARR_SUBIM(imar,ii) , wbot , ymax ) ;

     } else {                             /** linear windowing case **/

       wbot = (window_center - 0.5) - 0.5*(window_width-1.0) ;
       wtop = (window_center - 0.5) + 0.5*(window_width-1.0) ;
       wfac = ymax                  /     (window_width-1.0) ;

       for( ii=0 ; ii < IMARR_COUNT(imar) ; ii++ )
         mri_dicom_window_2( IMARR_SUBIM(imar,ii) , wbot,wtop,wfac , ymax ) ;
     }
   } /* end of Window */

   /*---------------------------------------------------------------------*/
   /*----- store some coordinate information in MRILIB globals, too? -----*/

   if( dt > 0.0f && MRILIB_tr <= 0.0f ) MRILIB_tr = dt ;  /* TR */

   /*-- try to get image orientation fields (also, set ior,jor,kor) --*/

   if( epos[E_IMAGE_ORIENTATION] != NULL ){ /* direction cosines of image axes */

     ddd = strstr(epos[E_IMAGE_ORIENTATION],"//") ;
     if( ddd != NULL ){
       float xn,yn ; int qq ;
       int icod=0,jcod=0,kcod=0 ; /* 24 Jan 2006 */
       mat44 nmat ;
       static int orient_nifti2orc[7] = /* table converts NIfTI-1 */
           { 666 ,                      /* orientation codes      */
             -1  , /* NIFTI_L2R */      /* to what I want herein  */
              1  , /* NIFTI_R2L */
             -2  , /* NIFTI_P2A */
              2  , /* NIFTI_A2P */
              3  , /* NIFTI_I2S */
             -3    /* NIFTI_S2I */
           } ;

       qq = sscanf(ddd+2,"%f\\%f\\%f\\%f\\%f\\%f",&xc1,&xc2,&xc3,&yc1,&yc2,&yc3);
       xn = sqrt( xc1*xc1 + xc2*xc2 + xc3*xc3 ) ; /* vector norms */
       yn = sqrt( yc1*yc1 + yc2*yc2 + yc3*yc3 ) ;
       if( qq == 6 && xn > 0.0f && yn > 0.0f ){   /* both vectors OK */

         xc1 /= xn ; xc2 /= xn ; xc3 /= xn ;      /* normalize vectors */
         yc1 /= yn ; yc2 /= yn ; yc3 /= yn ;      /* just to be safe  */

         have_cosines = 2 ;   /* i and j directions, at least */

         if( !use_MRILIB_xcos ){
           MRILIB_xcos[0] = xc1 ; MRILIB_xcos[1] = xc2 ;  /* save direction */
           MRILIB_xcos[2] = xc3 ; use_MRILIB_xcos = 1 ;   /* cosine vectors */
         }

         if( !use_MRILIB_ycos ){
           MRILIB_ycos[0] = yc1 ; MRILIB_ycos[1] = yc2 ;
           MRILIB_ycos[2] = yc3 ; use_MRILIB_ycos = 1 ;
         }

         /*** Determine AFNI orientation codes the NEW way - 24 Jan 2006 ***/

         /* set the slice normal direction as well */

         if( sexinfo.have_normal_x ||
             sexinfo.have_normal_y || sexinfo.have_normal_z ){

           zc1 = sexinfo.normal_x ;  /* from Siemens */
           zc2 = sexinfo.normal_y ;
           zc3 = sexinfo.normal_z ; have_cosines = 3 ; /* have k direction */
         } else {
           zc1 = xc2*yc3 - xc3*yc2 ; /* cross product */
           zc2 = xc3*yc1 - xc1*yc3 ; /* from x and y  */
           zc3 = xc1*yc2 - xc2*yc1 ;
         }
         yn  = sqrt( zc1*zc1 + zc2*zc2 + zc3*zc3 ) ;
         if( yn > 0.0f ){ zc1 /= yn; zc2 /= yn; zc3 /= yn; }

         /* get the NIfTI codes for orientation,
            recalling that to NIfTI, +x=R +y=A +z=S,
                      while to AFNI, +x=L +y=P +z=S,
            so that in the matrix below, which transforms
            the image (i,j,k) axes to spatial (x,y,z)
            directions, we have to negate the first
            two rows, which compute x and y from (i,j,k). */

         LOAD_MAT44( nmat ,
                     -xc1 , -yc1 , -zc1 , 0.0f ,
                     -xc2 , -yc2 , -zc2 , 0.0f ,
                      xc3 ,  yc3 ,  zc3 , 0.0f  ) ;
         nifti_mat44_to_orientation( nmat , &icod,&jcod,&kcod ) ;

         if( icod > 0 && jcod > 0 && kcod > 0 ){  /* should always be OK */

           ior = orient_nifti2orc[icod] ;  /* convert to the +/- 1, 2, or 3 */
           jor = orient_nifti2orc[jcod] ;  /* codes indicating orientation */
           kor = orient_nifti2orc[kcod] ;  /* of each image direction     */

         } else { /*** Determine AFNI orientation codes the OLD way ***/

           /* i-axis orientation */
           /* ior determines which spatial direction is i-axis  */
           /* and is the direction that has the biggest change */

           dxx = fabs(xc1) ; ior = 1 ;
           dyy = fabs(xc2) ; if( dyy > dxx ){ ior=2; dxx=dyy; }
           dzz = fabs(xc3) ; if( dzz > dxx ){ ior=3;          }
           dxx = MRILIB_xcos[ior-1] ; if( dxx < 0. ) ior = -ior;

           /* j-axis orientation */
           /* jor determines which spatial direction is j-axis  */
           /* and is the direction that has the biggest change */

           dxx = fabs(yc1) ; jor = 1 ;
           dyy = fabs(yc2) ; if( dyy > dxx ){ jor=2; dxx=dyy; }
           dzz = fabs(yc3) ; if( dzz > dxx ){ jor=3;          }
           dyy = MRILIB_ycos[jor-1] ; if( dyy < 0. ) jor = -jor;

           kor = 6 - abs(ior)-abs(jor) ; /* which spatial direction is k-axis */
         }

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
         if( have_cosines == 3 && MRILIB_orients[4] == '\0' ){
           switch( kor ){
             case -1: MRILIB_orients[4] = 'L'; MRILIB_orients[5] = 'R'; break;
             case  1: MRILIB_orients[4] = 'R'; MRILIB_orients[5] = 'L'; break;
             case -2: MRILIB_orients[4] = 'P'; MRILIB_orients[5] = 'A'; break;
             case  2: MRILIB_orients[4] = 'A'; MRILIB_orients[5] = 'P'; break;
             case  3: MRILIB_orients[4] = 'I'; MRILIB_orients[5] = 'S'; break;
             case -3: MRILIB_orients[4] = 'S'; MRILIB_orients[5] = 'I'; break;
             default: MRILIB_orients[4] ='\0'; MRILIB_orients[5] ='\0'; break;
           }
         }

         MRILIB_orients[6] = '\0' ;   /* terminate orientation string */
         have_orients      = 1 ;
       }
     }
   } /* end of decoding direction cosines */

   /**-- a backup method: use the symbolic code [not commonly present] --**/

   if( !have_orients && epos[E_PATIENT_ORIENTATION] != NULL ){
     ddd = strstr(epos[E_PATIENT_ORIENTATION],"//") ;
     if( ddd != NULL ){
       char xc='\0' , yc='\0' ;
       sscanf(ddd+2,"%c\\%c",&xc,&yc) ;   /* e.g., "L\P"    */
       switch( toupper(xc) ){             /* F=foot, H=Head */
         case 'L': MRILIB_orients[0] = 'L'; MRILIB_orients[1] = 'R'; ior=-1; break;
         case 'R': MRILIB_orients[0] = 'R'; MRILIB_orients[1] = 'L'; ior= 1; break;
         case 'P': MRILIB_orients[0] = 'P'; MRILIB_orients[1] = 'A'; ior=-2; break;
         case 'A': MRILIB_orients[0] = 'A'; MRILIB_orients[1] = 'P'; ior= 2; break;
         case 'F': MRILIB_orients[0] = 'I'; MRILIB_orients[1] = 'S'; ior= 3; break;
         case 'H': MRILIB_orients[0] = 'S'; MRILIB_orients[1] = 'I'; ior=-3; break;
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
   }  /* end of decoding symbolic image orientation */

   if( !have_orients ){   /**------- should never be true ------**/
     static int nwarn=0 ; /** but the world out there is screwy **/
     if( nwarn < NWMAX )
       fprintf(stderr,
        "++ DICOM WARNING: can't use Image or Patient Orientation in file %s\n",
        fname) ;
     if( nwarn == NWMAX )
       fprintf(stderr,
        "++ DICOM NOTICE: no more 'Orientation' messages\n");
     nwarn++ ;
   }

   /***------------------------------------------------------------------------
     We now try to find the image offset (position), if we got the orientation
     above.  Due to the various files we've seen out in the wild, several
     methods that may conflict are available.  The user can control which
     methods are used with environment variables.

     Method 1:
     * Use the DICOM Image Position values, which are supposed to be
        (x,y,z) coordinates of the center of the first voxel present.
     * HOWEVER: for a Siemens mosaic, these coordinates have been
        munged and must be un-munged.  This requires the use of the
        direction cosines, so we need have_cosines > 0 for this sub-case.
     * Method 1 can be disabled by setting environment variable
       AFNI_DICOM_USE_IMAGE_POSITION to NO.

     Method 2:
     * Use Siemens extra info (parsed into sexinfo), if present
        These values appear to be the coordinates of the slice CENTER,
        not the edge, which is what we want, so we'll have to translate
        them to the edge of the first slice, using the direction
        cosines again.
     * Method 2 can be disabled by setting environment variable
        AFNI_DICOM_USE_SIEMENS_COORDS to NO.

     Method 3:
     * Use the DICOM Slice Location value.  This value is deprecated in
        DICOM 2004+ files, so this method is a fallback.
     * Method 3 can be disabled by setting environment variable
        AFNI_DICOM_USE_SLICE_LOCATION to NO.
   ------------------------------------------------------------------------***/

   if( have_orients &&
       nzoff < 2    &&
       (!use_MRILIB_xoff || !use_MRILIB_yoff || !use_MRILIB_zoff) ){

     /***** Method 1: use DICOM Image Position values *****/

     if( !AFNI_noenv("AFNI_DICOM_USE_IMAGE_POSITION") &&
          epos[E_IMAGE_POSITION] != NULL                ){

       ddd = strstr(epos[E_IMAGE_POSITION],"//") ;
       if( ddd != NULL ){
         float xyz[3] ; int qq ;
         qq = sscanf(ddd+2,"%f\\%f\\%f",xyz,xyz+1,xyz+2) ;
         if( qq == 3 ){
           static float zoff ;           /* saved from nzoff=0 case */
           float zz = xyz[abs(kor)-1] ;  /* kor from orients above */

           if( mosaic || have_cosines==3 ){
             float xx=xyz[0] , yy=xyz[1] , zz=xyz[2] ;
             if( mosaic ){                    /** mosaic unmunging **/
               float isiz , jsiz ;
               float hmi , hmj ;

               isiz = dx * mos_nx ;       /* sub-image size in i-direction */
               hmi  = (mos_ix/2) * isiz ; /* i-distance to mosaic corner   */
               jsiz = dy * mos_ny ;
               hmj  = (mos_iy/2) * jsiz ;
               xx  += hmi * xc1 + hmj * yc1 ; /* move 1/2 mosaic  */
               yy  += hmi * xc2 + hmj * yc2 ; /* size from corner */
               zz  += hmi * xc3 + hmj * yc3 ; /* along i,j axes   */
             }
             xcc = xx; ycc = yy; zcc = zz; have_cc = 1;
             nzoff = 2 ;
             if( !use_MRILIB_xoff ){
               use_MRILIB_xoff=1; MRILIB_xoff=xx; 
               if( ior > 0 ) MRILIB_xoff = -MRILIB_xoff ;
             }
             if( !use_MRILIB_yoff ){
               use_MRILIB_yoff=1; MRILIB_yoff=yy; 
               if( jor > 0 ) MRILIB_yoff = -MRILIB_yoff ;
             }
             if( !use_MRILIB_zoff ){
               use_MRILIB_zoff=1; MRILIB_zoff=zz; 
               if( kor > 0 ) MRILIB_zoff = -MRILIB_zoff ;
             }
#if 0
fprintf(stderr,"Coords (method 1/%s): %.4f %.4f %.4f\n",
        (mosaic)?"mosaic":"cosines" , MRILIB_xoff,MRILIB_yoff,MRILIB_zoff ) ;
#endif

           } else {  /** not mosaic, or don't have cosines **/

             xcc = xyz[0]; ycc = xyz[1]; zcc = xyz[2]; have_cc = 1;

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

               if( !use_MRILIB_xoff ){
                 qq = abs(ior) ;
                 MRILIB_xoff = xyz[qq-1] ; use_MRILIB_xoff = 1 ;
                 if( ior > 0 ) MRILIB_xoff = -MRILIB_xoff ;
                 if( mosaic ){
                   if( MRILIB_xoff < 0.0f )
                     MRILIB_xoff += 0.5*dx*mos_nx*(mos_ix-1);
                   else
                     MRILIB_xoff -= 0.5*dx*mos_nx*(mos_ix-1);
                 }
               }

               if( !use_MRILIB_yoff ){
                 qq = abs(jor) ;
                 MRILIB_yoff = xyz[qq-1] ; use_MRILIB_yoff = 1 ;
                 if( jor > 0 ) MRILIB_yoff = -MRILIB_yoff ;
                 if( mosaic ){
                   if( MRILIB_yoff < 0.0f )
                     MRILIB_yoff += 0.5*dy*mos_ny*(mos_iy-1);
                   else
                     MRILIB_yoff -= 0.5*dy*mos_ny*(mos_iy-1);
                 }
               }

             } else if( nzoff == 1 && !use_MRILIB_zoff ){  /* 2nd DICOM image */

               float qoff = zz - zoff ;   /* vive la difference */
               if( qoff < 0 ) kor = -kor; /* kor determines k-axis orientation */
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

               if( !use_MRILIB_zoff ){
                 MRILIB_zoff = zoff ; use_MRILIB_zoff = 1 ;
                 if( kor > 0 ) MRILIB_zoff = -MRILIB_zoff ;
               }
             }
             nzoff++ ;  /* 3rd and later images don't count for z-orientation */

#if 0
fprintf(stderr,"Coords (method 1/nonmosaic): %.4f %.4f %.4f\n",
        MRILIB_xoff,MRILIB_yoff,MRILIB_zoff ) ;
#endif

           } /* end of non-mosaic and/or non-cosines option */
         } /* end of getting 3 values from Image Position */
       }
     }  /*** end of Method 1: using Image Position attribute ***/

     /***** Method 2: use parsed Siemens extra info *****/

     if( nzoff == 0                                                  &&
         have_cosines                                                &&
         sexinfo.good                                                &&
         sexinfo.have_xyz[0]+sexinfo.have_xyz[1]+sexinfo.have_xyz[2] &&
         !AFNI_noenv("AFNI_DICOM_USE_SIEMENS_COORDS")                &&
        (!use_MRILIB_xoff || !use_MRILIB_yoff || !use_MRILIB_zoff) ){

       float sx=sexinfo.slice_xyz[0][0] ,   /* slice center locations */
             sy=sexinfo.slice_xyz[0][1] ,   /* extracted from sexinfo */
             sz=sexinfo.slice_xyz[0][2]  ;
       float hisiz , hjsiz , xx,yy,zz ;

       hisiz = 0.5*dx * ( (mosaic) ? mos_nx : nx ) ;  /* 1/2 size of image */
       hjsiz = 0.5*dy * ( (mosaic) ? mos_ny : ny ) ;

       xx = sx - hisiz * xc1 - hjsiz * yc1 ; /* move 1/2 image   */
       yy = sy - hisiz * xc2 - hjsiz * yc2 ; /* size from center */
       zz = sz - hisiz * xc3 - hjsiz * yc3 ; /* along i,j axes   */

       xcc = xx; ycc = yy; zcc = zz; have_cc = 1;

       nzoff = 2 ;
       if( !use_MRILIB_xoff ){ use_MRILIB_xoff=1; MRILIB_xoff=xx; }
       if( !use_MRILIB_yoff ){ use_MRILIB_yoff=1; MRILIB_yoff=yy; }
       if( !use_MRILIB_zoff ){ use_MRILIB_zoff=1; MRILIB_zoff=zz; }

#if 0
fprintf(stderr,"Coords (method 2): %.4f %.4f %.4f\n",
        MRILIB_xoff,MRILIB_yoff,MRILIB_zoff ) ;
#endif

     } /*** end of Method 2: using sexinfo ***/

     /***** Method 3: using Slice Location *****/

     if( epos[E_SLICE_LOCATION] != NULL               &&
         have_orients                                 &&
         !use_MRILIB_zoff                             &&
         !AFNI_noenv("AFNI_DICOM_USE_SLICE_LOCATION")    ){

       ddd = strstr(epos[E_SLICE_LOCATION],"//") ;
       if( ddd != NULL ){
         float zz ; int qq ;
         qq = sscanf(ddd+2,"%f",&zz) ;
         if( qq == 1 ){
           static float zoff ;  /* saved from nzoff=0 case */
           if( nzoff == 0 ){    /* 1st DICOM image */
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

#if 0
fprintf(stderr,"Coords (method 3): %.4f %.4f %.4f\n",
        MRILIB_xoff,MRILIB_yoff,MRILIB_zoff ) ;
#endif
         }
       }
     } /* end of Method 3: using Slice Location */

   } /*************** end of image offset extraction attempts ***************/

   /***** 26 Jan 2005: load global orientation+offset matrix *****/

   if( !use_MRILIB_dicom_matrix && have_cosines == 3 && have_cc ){
      LOAD_MAT44( MRILIB_dicom_matrix ,
                     xc1 , yc1 , zc1 , xcc ,
                     xc2 , yc2 , zc2 , ycc ,
                     xc3 , yc3 , zc3 , zcc  ) ;
      use_MRILIB_dicom_matrix = 1 ;
   }

   /*==============================*/
   /************** DONE ************/
   /*==============================*/

   CLEANUP ; RETURN( imar );
}

/*------------------------------------------------------------------------------*/
/*! Count images in a DICOM file, if possible.
--------------------------------------------------------------------------------*/

int mri_imcount_dicom( char *fname )
{
   char *ppp=NULL , *ddd ;
   FILE *fp=NULL ;
   off_t poff ;
   unsigned int plen ;
   char *epos[NUM_ELIST] ;
   int ii , ee , bpp , datum ;
   int nx,ny,nz ;

   int mosaic=0 , mos_nx,mos_ny , mos_ix,mos_iy,mos_nz ;  /* 28 Oct 2002 */
   Siemens_extra_info sexinfo ;                           /* 02 Dec 2002 */

   char *sexi_start;   /* KRH 25 Jul 2003 */
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
   if( plen <= 0 ){ CLEANUP; RETURN(0); }

   /* check if file is actually this big */

   { unsigned int psiz , fsiz ;
     fsiz = THD_filesize( fname ) ;
     psiz = (unsigned int)(poff) + plen ;
     if( fsiz < psiz ){ CLEANUP; RETURN(0); }
   }

   /* find positions in header of elements we care about */

   for( ee=0 ; ee < NUM_ELIST ; ee++ )
     epos[ee] = strstr(ppp,elist[ee]) ;

   /* see if the header has the elements we absolutely need */

   if( epos[E_ROWS]           == NULL ||
       epos[E_COLUMNS]        == NULL ||
       epos[E_BITS_ALLOCATED] == NULL   ){ CLEANUP; RETURN(0); }

   /* check if we have 1 sample per pixel (can't deal with 3 or 4 now) */

   if( epos[E_SAMPLES_PER_PIXEL] != NULL ){
      ddd = strstr(epos[E_SAMPLES_PER_PIXEL],"//") ;
      if( ddd == NULL ){ CLEANUP; RETURN(0); }
      ii = 0 ; sscanf(ddd+2,"%d",&ii) ;
      if( ii != 1 ){ CLEANUP; RETURN(0); }
   }

   /* check if photometric interpretation is MONOCHROME (don't like PALETTE) */

   if( epos[E_PHOTOMETRIC_INTERPRETATION] != NULL ){
      ddd = strstr(epos[E_PHOTOMETRIC_INTERPRETATION],"MONOCHROME") ;
      if( ddd == NULL ){ CLEANUP; RETURN(0); }
   }

   /* check if we have 8, 16, or 32 bits per pixel */

   ddd = strstr(epos[E_BITS_ALLOCATED],"//") ;
   if( ddd == NULL ){ CLEANUP; RETURN(0); }
   bpp = 0 ; sscanf(ddd+2,"%d",&bpp) ;
   switch( bpp ){
      default: CLEANUP; RETURN(0);   /* bad value */
      case  8: datum = MRI_byte ; break ;
      case 16: datum = MRI_short; break ;
      case 32: datum = MRI_int  ; break ;
   }
   bpp /= 8 ; /* now bytes per pixel, instead of bits */

   /* get nx, ny, nz */

   ddd = strstr(epos[E_ROWS],"//") ;
   if( ddd == NULL ){ CLEANUP; RETURN(0); }
   nx = 0 ; sscanf(ddd+2,"%d",&nx) ;
   if( nx < 2 ){ CLEANUP; RETURN(0); }

   ddd = strstr(epos[E_COLUMNS],"//") ;
   if( ddd == NULL ){ CLEANUP; RETURN(0); }
   ny = 0 ; sscanf(ddd+2,"%d",&ny) ;
   if( ny < 2 ){ CLEANUP; RETURN(0); }

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
       if( fp == NULL ) fp = fopen( fname , "rb" ) ;
       if( fp != NULL )
         str_sexinfo = extract_bytes_from_file( fp, (off_t)loc, (size_t)len, 1 ) ;
     }
   }

   if( str_sexinfo != NULL   ){

     /* 31 Oct 2002: extract extra Siemens info from file */

     /* KRH 25 Jul 2003 if start and end markers are present for
      * Siemens extra info, cut string down to those boundaries */

     sexi_start = strstr(str_sexinfo, "### ASCCONV BEGIN ###");
     sexi_end = strstr(str_sexinfo, "### ASCCONV END ###");
     if ((sexi_start != NULL) && (sexi_end != NULL)) {
       char *sexi_tmp;
       int sexi_size;

       sexi_size = sexi_end - sexi_start + 19 ;
       sexi_tmp = AFMALL( char,  sexi_size );
       memcpy(sexi_tmp,sexi_start,sexi_size);
       free(str_sexinfo);
       str_sexinfo = sexi_tmp;
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
         fprintf(stderr,
           "++ DICOM WARNING: indecipherable SIEMENS MOSAIC info (%s) in file %s\n",
           elist[E_SIEMENS_2] , fname ) ;
       if( nwarn == NWMAX )
         fprintf(stderr,
          "++ DICOM NOTICE: no more SIEMENS MOSAIC 'indecipherable info' messages\n");
       nwarn++ ;
     }

   } /* end of if str_sexinfo != NULL */

   CLEANUP; RETURN(nz);
}

/*--------------------------------------------------------------------------------*/
/*! Read some bytes from an open file at a given offset.  Return them in a
    newly malloc()-ed array.  If return value is NULL, something bad happened.
    If strize!=0, then NUL characters are replaced with blanks, so that the
    thing returned is a single C string.
----------------------------------------------------------------------------------*/

static char * extract_bytes_from_file( FILE *fp, off_t start, size_t len, int strize )
{
   char *ar ;
   size_t nn , ii ;

   if( fp == NULL || len == 0 ) return NULL ;    /* bad inputs? */
   ar = AFMALL(char, len+1) ;                    /* make space for data */
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
/*! Parse the Siemens extra stuff for mosaic and other information.
    Ad hoc, based on sample data and no documentation.
    Subject to the whims of the Siemens trolls.
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

   if( mi == NULL ) return ;  /* stupid user */

   mi->good = 0 ;
   for( snum=0 ; snum < NMOMAX ; snum++ )  /* zero out coords */
     mi->slice_xyz[snum][0] =
      mi->slice_xyz[snum][1] =
       mi->slice_xyz[snum][2] = 0.0f;

   mi->have_normal_x = mi->have_normal_y = mi->have_normal_z = 0    ;
   mi->normal_x      = mi->normal_y      = mi->normal_z      = 0.0f ;

   mi->have_inplane_rot = 0 ; mi->inplane_rot = 0.0f ;
   mi->have_AAmatrix    = 0 ;
   for( mm=0 ; mm < 16 ; mm++ ) mi->AAmatrix[mm] = 0.0f ;

   if( str == NULL || *str == '\0' ) return ;  /* stupider user */

   /*-- 24 Jan 2006: find the AAmatrix info, if possible --*/

   cpt = strstr( str , "sAutoAlign.dAAMatrix[0]" ) ;
   if( cpt != NULL ){
     mi->have_AAmatrix = 1 ;
     while( cpt != NULL ){
       nn = sscanf( cpt , "sAutoAlign.dAAMatrix[%d] =%f%n" ,
                    &snum , &val , &mm ) ;
       if( nn == 2 && snum >= 0 && snum <= 15 )
         mi->AAmatrix[snum] = val ;
       cpt = strstr( cpt+mm , "sAutoAlign.dAAMatrix[" ) ;
     }
   }

   /*-- find string that starts the slice information array --*/
   /* 04 Mar 2003 reworked this section to skip "fake matches" *
    * of the target string present in some mosaic files in the *
    * binary section                                     --KRH */

   nn = 0;
   while (nn == 0) {
     cpt = strstr( str , "sSliceArray.asSlice[" ) ;
     if( cpt == NULL ) return ;
     /* interepret next string into
         snum = slice subscript (0,1,...)
         name = variable name
         val  = number of RHS of '=' sign
         mm   = # of bytes used in scanning the above */

     nn = sscanf( cpt , "sSliceArray.asSlice[%d].%1022s =%f%n" ,
                  &snum , name , &val , &mm ) ;
     str += 20; /* skip to end of "false match" string KRH */
   }

   /*-- scan for coordinates, until can't find a good string to scan --*/

#if 0
   /* 25 Feb 2003 Changed logic here KRH */
   have_x = have_y = have_z = 0 ;
#endif

   while(1){

     if( nn   <  3                   ) break ;  /* bad conversion set */
     if( snum <  0 || snum >= NMOMAX ) break ;  /* slice number out of range */

     /* 21 Feb 2003 rework this section to allow for missing coordinates
      * in some mosaic files                                       -KRH  */
#if 0
     /* assign val based on name */

          if( strcmp(name,"sPosition.dSag") == 0 ){ x = val; have_x = 1; }
     else if( strcmp(name,"sPosition.dCor") == 0 ){ y = val; have_y = 1; }
     else if( strcmp(name,"sPosition.dTra") == 0 ){ z = val; have_z = 1; }

     /* if now have all 3 coordinates, save them */

     if( have_x && have_y && have_z ){
       mi->slice_xyz[snum][0] = x;
       mi->slice_xyz[snum][1] = y;
       mi->slice_xyz[snum][2] = z;
       last_snum = snum ;
       have_x = have_y = have_z = 0 ;
     }
#endif

     /* at present, we just save the coordinates for each slice
        (which appear to be the CENTER of the slice, not the edge) */

     if( strcmp(name,"sPosition.dSag") == 0 ){         /* x */
       mi->slice_xyz[snum][0] = val;
       if (snum < 2) have_x[snum] = 1;
       last_snum = snum;
     } else if( strcmp(name,"sPosition.dCor") == 0 ){  /* y */
       mi->slice_xyz[snum][1] = val;
       if (snum < 2) have_y[snum] = 1;
       last_snum = snum;
     } else if( strcmp(name,"sPosition.dTra") == 0 ){  /* z */
       mi->slice_xyz[snum][2] = val;
       if (snum < 2) have_z[snum] = 1;
       last_snum = snum;
     }

     /* 24 Jan 2006: save other things from 1st slice only */

     else if( !mi->have_normal_x && strcmp(name,"sNormal.dSag") == 0 ){
       mi->normal_x = val ; mi->have_normal_x = 1 ;
     } else if( !mi->have_normal_y && strcmp(name,"sNormal.dCor") == 0 ){
       mi->normal_y = val ; mi->have_normal_y = 1 ;
     } else if( !mi->have_normal_z && strcmp(name,"sNormal.dTra") == 0 ){
       mi->normal_z = val ; mi->have_normal_z = 1 ;
     } else if( !mi->have_inplane_rot && strcmp(name,"dInPlaneRot") == 0 ){
       mi->inplane_rot = val ; mi->have_inplane_rot = 1 ;
     } else if( mi->thickness  == 0.0f && strcmp(name,"dThickness")  == 0 ){
       mi->thickness  = val ;
     } else if( mi->phaseFOV   == 0.0f && strcmp(name,"dPhaseFOV")   == 0 ){
       mi->phaseFOV   = val ;
     } else if( mi->readoutFOV == 0.0f && strcmp(name,"dReadoutFOV") == 0 ){
       mi->readoutFOV = val ;
     }

     /* skip to next slice array assignment string (which may not be a coordinate) */

     dpt = cpt + mm ;                                     /* just after 'val' */
     cpt = dpt ;
     while( isspace(*cpt) ) cpt++ ;                   /* skip over whitespace */
     if( cpt-dpt > 16 ) break ;                             /* too much space */
     if( strncmp(cpt,"sSliceArray.asSlice[",20) != 0 ) break; /* bad nxt line */
     /* 04 Mar 2003 moved this stuff around to allow for locating
      * fake matches of the target text in some mosaic files' binary sections  */

     /* interpret next string into
         snum = slice subscript (0,1,...)
         name = variable name
         val  = number of RHS of '=' sign
         mm   = # of bytes used in scanning the above */

     nn = sscanf( cpt , "sSliceArray.asSlice[%d].%1022s =%f%n" ,
                  &snum , name , &val , &mm ) ;

   } /* end of loop over SliceArray variables */

   /* if got at least 1 slice info, mark data as being good */

   if( last_snum >= 0 ){
     mi->good       = 1 ;
     if (have_x[0] && have_x[1]) mi->have_xyz[0] = 1;
     if (have_y[0] && have_y[1]) mi->have_xyz[1] = 1;
     if (have_z[0] && have_z[1]) mi->have_xyz[2] = 1;
     mi->mosaic_num = last_snum+1 ;
   }

   return ;
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

   /* check for good filename */

   if( fname == NULL || *fname == '\0' ) return 0 ;
   fp = fopen( fname , "rb" ) ; if( fp == NULL ) return 0 ;

   /* read 1st buffer */

   nn = fread( buf , 1 , BSIZ , fp ) ;
   if( nn < 256 ){ fclose(fp); return 0; }  /* too short */

   /*--- easy: check if has 'DICM' marker at offset 128..131 ---*/

   if( buf[128]=='D' && buf[129]=='I' && buf[130]=='C' && buf[131]=='M' ){
     fclose(fp); return 1;
   }

   /*--- hard: scan file for sequence: E0 7F 10 00 (image data attribute) ---*/

   /** at start of loop, buf[] has nn characters of data in it **/

   while(1){

     cpt = memchr( buf, 0xe0, nn ) ;                /* look for E0 */

     if( cpt == NULL ){          /* not found ==> skip this buffer */
       nn = fread( buf , 1 , BSIZ , fp ) ;      /* and get another */
       if( nn < 256 ){ fclose(fp); return 0; }    /* no more data? */
       continue ;                                     /* loop back */
     }

     ii = nn - (cpt-buf) ;         /* number of char to end of buf */
     if( ii <= 4 ){                    /* too close to end of buf? */
       memmove( buf , cpt , ii ) ;            /* move tail to head */
       nn  = fread( buf+ii , 1 , BSIZ-ii , fp ) ; /* re-fill buf[] */
       nn += ii ;                             /* size of buf[] now */
       if( nn < 256 ){ fclose(fp); return 0; }    /* no more data? */
       cpt = buf ; ii = nn ;     /* cpt=loc of E0; ii=size of tail */
     }

          /* see if we got what we want: E0 7F 10 00 stored at cpt */

     if( *cpt==0xe0 && *(cpt+1)==0x7f && *(cpt+2)==0x10 && *(cpt+3)==0x00 ){
       fclose(fp); return 1;
     }

                           /* no?  start again at next char in buf */

     memmove( buf , cpt+1 , ii-1 ) ; nn = ii-1 ;

   } /* never leaves loop at this point */
}

/*---------------------------------------------------------------------------*/
/* The next 3 functions implement various methods for finding
   the mosaic count in the Siemens extra info strings.
    - If return is non-positive, then the search methods failed.
    - If return is 1, then something worked, but it's really not a mosaic.
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/*! Siemens mosaic counting: decode the NumberOfImagesInMosaic field. */

static int mosaic_num_method1( char *sexinfo )
{
   char *apt, *bpt , *cpt ;
   int num=0 ;

   if( sexinfo == NULL || *sexinfo == '\0' ) return num ;

   /*===== Method 1: look for and parse the "NumberOfImagesInMosaic" field =====*/

   if( !AFNI_noenv("AFNI_SIEMENS_DICOM_NumberOfImagesInMosaic") ){
     apt = strstr( sexinfo , "NumberOfImagesInMosaic" ) ;
     if( apt != NULL && apt[68] == 'U' && apt[69] == 'S' && apt[70] == ' ' ){

       /* OK, scan ahead to the next alphanumeric character */

       bpt = apt + 70 ;  /* after the "US" */

       /* scan for next alphanumeric character:
            if it is an isolated letter, skip it and try again;
            if it is a digit, then this is the number we want;
          but don't scan too far ahead                         */

       while( (bpt-apt) < 111 ){
         for( ; *bpt != '\0' && !isalnum(*bpt) ; bpt++ ) ; /* nada */
         if( *bpt == '\0' ) break ;       /* end of the universe ==> quit */
         if( isalpha(*bpt) ){             /* got a letter */
           if( *(bpt+1) != ' ' ) break ;  /* but not isolated ==> quit */
           bpt +=2 ;                      /* was isolated     ==> loop */
         } else {                         /* must be a digit */
                                          /*   so scan to non-digit */
           for( cpt=bpt+1 ; isdigit(*cpt) ; cpt++ ) ; /*nada*/
           if( *cpt != ' ' ) break ;      /* not a blank      ==> quit */
           sscanf( bpt , "%d" , &num ) ;  /*** SUCCESS ***/
           return num ;                   /***************/
         }
       } /* end of loop over strings after "NumberOfImagesInMosaic" */
     } /* end of if we found "NumberOfImagesInMosaic" properly */
   } /* end of if we were allowed to try this method at all */

   return num ;
}

/*---------------------------------------------------------------------------*/
/*! Siemens mosaic counting: find the group array size declaration string. */

static int mosaic_num_method2( char *sexinfo )
{
   char *apt, *bpt , *cpt ;
   int num=0 ;

   if( sexinfo == NULL || *sexinfo == '\0' ) return num ;

   /*===== Method 2: "GroupArray" string method =====*/

   if( !AFNI_noenv("AFNI_SIEMENS_DICOM_GroupArray") ){
     apt = strstr( sexinfo , "sGroupArray.asGroup[0].nSize" ) ;
     if( apt != NULL ){

       /* scan ahead to next nonblank, which should be an '=' */

       bpt = apt + strlen("sGroupArray.asGroup[0].nSize") ;
       for( ; *bpt != '\0' && (bpt-apt) < 100 && *bpt == ' ' ; bpt++ ) ; /*nada*/

       if( *bpt == '=' ){  /* found the '=', so skip it and scan again */

         bpt++ ;
         for( ; *bpt != '\0' && (bpt-apt) < 100 && *bpt == ' ' ; bpt++ ) ; /*nada*/

         /* should be a digit */

         if( isdigit(*bpt) ){
           sscanf( bpt , "%d" , &num ) ;  /*** SUCCESS ***/
           return num ;                   /***************/
         }
       } /* end of if we found the '=' */
     } /* end of if we found "sGroupArray.asGroup[0].nSize" */
   } /* end of if we were allowed to try this method at all */

   return num ;
}

/*---------------------------------------------------------------------------*/
/*! Siemens mosaic counting: scan for slice subscripts. */

static int mosaic_num_method3( char *sexinfo )
{
   char *apt, *bpt , *cpt ;
   int num=0 ;

   if( sexinfo == NULL || *sexinfo == '\0' ) return num ;

   /*===== Method 3: "SliceArray...[%d]" string method =====*/

   if( !AFNI_noenv("AFNI_SIEMENS_DICOM_SliceArray") ){
     int val ;
     apt = strstr( sexinfo , "sSliceArray.asSlice[" ) ;
     if( apt != NULL && isdigit(apt[20]) ){

       sscanf(apt+20,"%d",&val) ; num = val ;

       /* scan all such places, decode the subscripts, and keep the largest */

       while(1){
         bpt = strstr( apt+21 , "sSliceArray.asSlice[" ) ;
         if( bpt != NULL ){
           apt = bpt ;
           if( isdigit(apt[20]) ){
             sscanf(apt+20,"%d",&val) ; if( val > num ) num = val ;
           }
         } else {      /* found the last one */
           break ;
         }
       } /* end of loop over all "sSliceArray.asSlice[%d]" substrings */

       num++ ; return num ;
     } /* end of if we found any "sSliceArray.asSlice[%d]" candidates */
   } /* end of if we were allowed to try this method at all */

   return num ;
}

/*-------------------------------------------------------------------------*/
/*! DICOM rescaling. */

static void mri_dicom_rescaler( MRI_IMAGE *im ,
                                float rescale_slope , float rescale_inter )
{
   register int jj ;

   if( im == NULL || rescale_slope <= 0.0f ) return ;

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

   return ;
}

/*-------------------------------------------------------------------------*/
/*! DICOM binary windowing. */

static void mri_dicom_window_1( MRI_IMAGE *im , float wbot , int ymax )
{
   register int jj ;

   if( im == NULL || ymax == 0 ) return ;

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

   return ;
}

/*-------------------------------------------------------------------------*/
/*! DICOM linear rescaling for windowing. */

static void mri_dicom_window_2( MRI_IMAGE *im ,
                                float wbot, float wtop, float wfac, int ymax )
{
   register int jj ;

   if( im == NULL || ymax == 0 ) return ;

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

   return ;
}
