#include "mrilib.h"

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
   static int nzoff=0 ;
   int ior,jor,kor ;

ENTRY("mri_read_dicom") ;

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
      default: free(ppp) ; RETURN(NULL);   /* bad value */
      case  8: datum = MRI_byte ; break ;
      case 16: datum = MRI_short; break ;
      case 32: datum = MRI_int  ; break ;  /* probably not present in DICOM? */
   }
   bpp /= 8 ; /* now bytes per pixel, instead of bits */

   /*** Print some warnings if appropriate ***/

   /* check if BITS_STORED and HIGH_BIT are aligned */

   if( epos[E_BITS_STORED] != NULL && epos[E_HIGH_BIT] != NULL ){
     int bs=0 , hb=0 ;
     ddd = strstr(epos[E_BITS_STORED],"//") ; sscanf(ddd+2,"%d",&bs) ;
     ddd = strstr(epos[E_HIGH_BIT],"//")    ; sscanf(ddd+2,"%d",&hb) ;
     if( bs != hb+1 ){
       static int nwarn=0 ;
       if( nwarn < 9 )
         fprintf(stderr,
                 "++ WARNING: DICOM file %s has Bits_Stored=%d and High_Bit=%d\n",
                 fname,bs,hb) ;
       nwarn++ ;
     }
   }

   /* check if Rescale is ordered */

   if( epos[E_RESCALE_INTERCEPT] != NULL ){
      static int nwarn=0 ;
      if( nwarn < 9 )
        fprintf(stderr,
                "++ WARNING: DICOM file %s has Rescale tags - not implemented here\n",
                fname ) ;
      nwarn++ ;
   }

   /* check if Window is ordered */

   if( epos[E_WINDOW_CENTER] != NULL ){
      static int nwarn=0 ;
      if( nwarn < 9 )
        fprintf(stderr,
                "++ WARNING: DICOM file %s has Window tags  - not implemented here\n",
                fname ) ;
      nwarn++ ;
   }

   /*** extract attributes of the image(s) to be read in ***/

   /* get nx, ny, nz */

   ddd = strstr(epos[E_ROWS],"//") ;
   if( ddd == NULL ){ free(ppp) ; RETURN(NULL); }
   nx = 0 ; sscanf(ddd+2,"%d",&nx) ;
   if( nx < 2 ){ free(ppp) ; RETURN(NULL); }

   ddd = strstr(epos[E_COLUMNS],"//") ;
   if( ddd == NULL ){ free(ppp) ; RETURN(NULL); }
   ny = 0 ; sscanf(ddd+2,"%d",&ny) ;
   if( ny < 2 ){ free(ppp) ; RETURN(NULL); }

   nz = 0 ;
   if( epos[E_NUMBER_OF_FRAMES] != NULL ){
     ddd = strstr(epos[E_NUMBER_OF_FRAMES],"//") ;
     if( ddd != NULL ) sscanf(ddd+2,"%d",&nz) ;
   }
   if( nz == 0 ) nz = plen / (bpp*nx*ny) ;   /* compute from image array size */
   if( nz == 0 ){ free(ppp) ; RETURN(NULL); }

   /* try to get dx, dy, dz, dt */

   dx = dy = dz = dt = 0.0 ;
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

   if( epos[E_SLICE_SPACING] != NULL ){
     ddd = strstr(epos[E_SLICE_SPACING],"//") ;
     if( ddd != NULL ) sscanf( ddd+2 , "%f" , &dz ) ;
   }
   if( dz == 0.0 && epos[E_SLICE_THICKNESS] != NULL ){
     ddd = strstr(epos[E_SLICE_THICKNESS],"//") ;
     if( ddd != NULL ) sscanf( ddd+2 , "%f" , &dz ) ;
   }

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

   /* store some information in MRILIB globals, too? */

   if( dt > 0.0 && MRILIB_tr <= 0.0 ) MRILIB_tr = dt ;

   /* try to get image orientation fields (also, set ior,jor,kor) */

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

         MRILIB_xcos[0] = xc1 ; MRILIB_xcos[1] = xc2 ;  /* save direction */
         MRILIB_xcos[2] = xc3 ; use_MRILIB_xcos = 1 ;   /* cosine vectors */

         MRILIB_ycos[0] = yc1 ; MRILIB_ycos[1] = yc2 ;
         MRILIB_ycos[2] = yc3 ; use_MRILIB_ycos = 1 ;

         /* x-axis orientation */
         /* ior determines which spatial direction is x-axis  */
         /* and is the direction that has the biggest change */

         dx = fabs(xc1) ; ior = 1 ;
         dy = fabs(xc2) ; if( dy > dx ){ ior=2; dx=dy; }
         dz = fabs(xc3) ; if( dz > dx ){ ior=3;        }
         dx = MRILIB_xcos[ior-1] ; if( dx < 0. ) ior = -ior;
         switch( ior ){
           case -1: MRILIB_orients[0] = 'L'; MRILIB_orients[1] = 'R'; break;
           case  1: MRILIB_orients[0] = 'R'; MRILIB_orients[1] = 'L'; break;
           case -2: MRILIB_orients[0] = 'P'; MRILIB_orients[1] = 'A'; break;
           case  2: MRILIB_orients[0] = 'A'; MRILIB_orients[1] = 'P'; break;
           case  3: MRILIB_orients[0] = 'I'; MRILIB_orients[1] = 'S'; break;
           case -3: MRILIB_orients[0] = 'S'; MRILIB_orients[1] = 'I'; break;
           default: MRILIB_orients[0] ='\0'; MRILIB_orients[1] ='\0'; break;
         }

         /* y-axis orientation */
         /* jor determines which spatial direction is y-axis  */
         /* and is the direction that has the biggest change */

         dx = fabs(yc1) ; jor = 1 ;
         dy = fabs(yc2) ; if( dy > dx ){ jor=2; dx=dy; }
         dz = fabs(yc3) ; if( dz > dx ){ jor=3;        }
         dy = MRILIB_ycos[jor-1] ; if( dy < 0. ) jor = -jor;
         switch( jor ){
           case -1: MRILIB_orients[2] = 'L'; MRILIB_orients[3] = 'R'; break;
           case  1: MRILIB_orients[2] = 'R'; MRILIB_orients[3] = 'L'; break;
           case -2: MRILIB_orients[2] = 'P'; MRILIB_orients[3] = 'A'; break;
           case  2: MRILIB_orients[2] = 'A'; MRILIB_orients[3] = 'P'; break;
           case  3: MRILIB_orients[2] = 'I'; MRILIB_orients[3] = 'S'; break;
           case -3: MRILIB_orients[2] = 'S'; MRILIB_orients[3] = 'I'; break;
           default: MRILIB_orients[2] ='\0'; MRILIB_orients[3] ='\0'; break;
         }

         MRILIB_orients[6] = '\0' ;   /* terminate orientation string */

         kor = 6 - abs(ior)-abs(jor) ;   /* which spatial direction is z-axis */
                                         /* where 1=L-R, 2=P-A, 3=I-S */
         have_orients = 1 ;
       }
     }

   } else if( epos[E_PATIENT_ORIENTATION] != NULL ){  /* symbolic orientation of image */

     ddd = strstr(epos[E_PATIENT_ORIENTATION],"//") ;
     if( ddd != NULL ){
       char xc='\0' , yc='\0' ;
       sscanf(ddd+2,"%c\\%c",&xc,&yc) ;   /* e.g., "L\P" */
       switch( toupper(xc) ){
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
       MRILIB_orients[4] = '\0' ;      /* terminate orientation string */
       kor = 6 - abs(ior)-abs(jor) ;   /* which spatial direction is z-axis */
       have_orients = (ior != 0 && jor != 0) ;
     }

   }  /* end of 2D image orientation */

   /* try to get image offset (position), if have orientation from above */

   if( nzoff < 2 && epos[E_IMAGE_POSITION] != NULL && have_orients ){
     ddd = strstr(epos[E_IMAGE_POSITION],"//") ;
     if( ddd != NULL ){
       float xyz[3] ; int qq ;
       qq = sscanf(ddd+2,"%f\\%f\\%f",xyz,xyz+1,xyz+2) ;
       if( qq == 3 ){
         static float zoff ;      /* saved from nzoff=0 case */
         float zz = xyz[kor-1] ;  /* kor from orients above */

         if( nzoff == 0 ){  /* 1st DICOM image */

           zoff = zz ;      /* save this for 2nd image calculation */
           switch( kor ){   /* may be changed on second image */
             case  1: MRILIB_orients[4] = 'L'; MRILIB_orients[5] = 'R'; break;
             case  2: MRILIB_orients[4] = 'P'; MRILIB_orients[5] = 'A'; break;
             case  3: MRILIB_orients[4] = 'I'; MRILIB_orients[5] = 'S'; break;
             default: MRILIB_orients[4] ='\0'; MRILIB_orients[5] ='\0'; break;
           }
           MRILIB_orients[6] = '\0' ;

           /* Save x,y offsets of this 1st slice */

           qq = abs(ior) ;
           MRILIB_xoff = xyz[qq-1] ; use_MRILIB_xoff = 1 ;
           if( ior > 0 ) MRILIB_xoff = -MRILIB_xoff ;

           qq = abs(jor) ;
           MRILIB_yoff = xyz[qq-1] ; use_MRILIB_yoff = 1 ;
           if( jor > 0 ) MRILIB_yoff = -MRILIB_yoff ;

         } else if( nzoff == 1 ){  /* 2nd DICOM image */

           float qoff = zz - zoff ;    /* vive la difference */
           if( qoff < 0 ) kor = -kor ; /* kor determines z-axis orientation */
           switch( kor ){
             case  1: MRILIB_orients[4] = 'L'; MRILIB_orients[5] = 'R'; break;
             case -1: MRILIB_orients[4] = 'R'; MRILIB_orients[5] = 'L'; break;
             case  2: MRILIB_orients[4] = 'P'; MRILIB_orients[5] = 'A'; break;
             case -2: MRILIB_orients[4] = 'A'; MRILIB_orients[5] = 'P'; break;
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
   }  /* end of image position */

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

ENTRY("mri_imcount_dicom") ;

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

   free(ppp) ; RETURN(nz);
}
