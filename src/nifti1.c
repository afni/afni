#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "nifti1.h"

/*****===================================================================*****/
/*****      Sample functions to deal with NIFTI-1 and ANALYZE files      *****/
/*****...................................................................*****/
/*****            This code is released to the public domain.            *****/
/*****...................................................................*****/
/*****  Author: Robert W Cox, SSCC/DIRP/NIMH/NIH/DHHS/USA/EARTH          *****/
/*****  Date:   August 2003                                              *****/
/*****...................................................................*****/
/*****  Neither the National Institutes of Health (NIH), nor any of its  *****/
/*****  employees imply any warranty of usefulness of this software for  *****/
/*****  any purpose, and do not assume any liability for damages,        *****/
/*****  incidental or otherwise, caused by any use of this document.     *****/
/*****===================================================================*****/

/********************** Some sample data structures **************************/

typedef struct {                   /** 4x4 matrix struct **/
  float m[4][4] ;
} nifti_mat44 ;

/*...........................................................................*/

typedef struct {                  /** Image storage struct **/

  int ndim ;                      /* last dimension greater than 1 (1..7) */
  int nx,ny,nz , nt,nu,nv,nw ;    /* dimensions of grid array */
  int dim[8] ;                    /* dim[0]=ndim, dim[1]=nx, etc. */
  int nvox ;                      /* number of voxels = nx*ny*nz*... */
  int nbyper ;                    /* bytes per voxel */
  int datatype ;                  /* type of data in voxels: DT_* code */

  float dx,dy,dz , dt,du,dv,dw ;  /* grid spacings */
  float pixdim[8] ;               /* pixdim[1]=dx, etc. */

  float scl_slope , scl_inter ;   /* scaling parameters */

  float cal_min , cal_max ;       /* calibration parameters */

  int qform_code , sform_code ;   /* codes for (x,y,z) space meaning */

  float quatern_b, quatern_c,     /* quaternion transform parameters */
        quatern_d, qoffset_x,     /* [when writing a dataset,  these ] */
        qoffset_y, qoffset_z,     /* [are used for qform, NOT qto_xyz] */
        qfac                 ;

  nifti_mat44 qto_xyz ;           /* qform: transform (i,j,k) to (x,y,z) */
  nifti_mat44 qto_ijk ;           /* qform: transform (x,y,z) to (i,j,k) */

  nifti_mat44 sto_xyz ;           /* sform: transform (i,j,k) to (x,y,z) */
  nifti_mat44 sto_ijk ;           /* sform: transform (x,y,z) to (i,j,k) */

  float toffset ;                 /* time coordinate offset */

  int xyz_units , time_units ;    /* dx,dy,dz & dt units: NIFTI_UNITS_* code */

  int nifti_type ;                /* 0==ANALYZE, 2==NIFTI-1 (2 files),
                                                 1==NIFTI-1 (1 file)   */
  int intent_code ;               /* statistic type (or something) */
  float intent_p1, intent_p2,     /* intent parameters */
        intent_p3 ;
  char intent_name[16] ;

  char descrip[80], aux_file[24];

  char *fname ;                   /* header filename (.hdr or .nii) */
  char *iname ;                   /* image filename (.img or .nii)  */
  int   iname_offset ;            /* offset into iname where data starts */
  int   swapsize ;                /* swapping unit in image data */
  void *data ;                    /* pointer to data: nbyper*nvox bytes */

} nifti_image ;

/*****************************************************************************/
/*--------------- Prototypes of functions defined in this file --------------*/

char *nifti_datatype_string( int dt ) ;
nifti_mat44 nifti_mat44_inverse( nifti_mat44 R ) ;
void swap_2bytes ( int n , void *ar ) ;
void swap_4bytes ( int n , void *ar ) ;
void swap_8bytes ( int n , void *ar ) ;
void swap_16bytes( int n , void *ar ) ;
void swap_Nbytes ( int n , int siz , void *ar ) ;
void swap_nifti_header( struct nifti_1_header *h , int is_nifti ) ;
unsigned int get_filesize( char *pathname ) ;

nifti_image * nifti_image_read    ( char *hname , int read_data ) ;
void          nifti_image_load    ( nifti_image *nim ) ;
void          nifti_image_unload  ( nifti_image *nim ) ;
void          nifti_image_free    ( nifti_image *nim ) ;
void          nifti_image_infodump( nifti_image *nim ) ;
void          nifti_image_write   ( nifti_image *nim ) ;

/*-------------------- Some C convenience macros ----------------------------*/

#undef  swap_2
#undef  swap_4
#define swap_2(s) swap_2bytes(1,&(s))  /* s is a 2-byte short; swap in place */
#define swap_4(v) swap_4bytes(1,&(v))  /* v is a 4-byte value; swap in place */

                        /***** isfinite() is a C99 macro, which is
                               present in many C implementations already *****/

#undef IS_GOOD_FLOAT
#undef FIXED_FLOAT

#ifdef isfinite       /* use isfinite() to check floats/doubles for goodness */
#  define IS_GOOD_FLOAT(x) isfinite(x)       /* check if x is a "good" float */
#  define FIXED_FLOAT(x)   (isfinite(x) ? (x) : 0)           /* fixed if bad */
#else
#  define IS_GOOD_FLOAT(x) 1                               /* don't check it */
#  define FIXED_FLOAT(x)   (x)                               /* don't fix it */
#endif

/*---------------------------------------------------------------------------*/
/* Return a pointer to a string holding the name of a NIFTI datatype.
   Don't free() or modify this string!  It points to static storage.
-----------------------------------------------------------------------------*/

char *nifti_datatype_string( int dt )
{
   switch( dt ){
     case DT_UNKNOWN:    return "UNKNOWN"    ;
     case DT_BINARY:     return "BINARY"     ;
     case DT_INT8:       return "INT8"       ;
     case DT_UINT8:      return "UINT8"      ;
     case DT_INT16:      return "INT16"      ;
     case DT_UINT16:     return "UINT16"     ;
     case DT_INT32:      return "INT32"      ;
     case DT_UINT32:     return "UINT32"     ;
     case DT_INT64:      return "INT64"      ;
     case DT_UINT64:     return "UINT64"     ;
     case DT_FLOAT32:    return "FLOAT32"    ;
     case DT_FLOAT64:    return "FLOAT64"    ;
     case DT_FLOAT128:   return "FLOAT128"   ;
     case DT_COMPLEX64:  return "COMPLEX64"  ;
     case DT_COMPLEX128: return "COMPLEX128" ;
     case DT_COMPLEX256: return "COMPLEX256" ;
     case DT_RGB24:      return "RGB24"      ;
   }

   return "**ILLEGAL**" ;
}

/*---------------------------------------------------------------------------*/
/* Compute the inverse of a bordered 4x4 matrix.
   Some numerical code fragments were generated by Maple 8.
   If a singular matrix is input, the output matrix will be all zero.
   You can check for this by examining the [3][3] element, which will
   be 1.0 for the normal case and 0.0 for the bad case.
-----------------------------------------------------------------------------*/

nifti_mat44 nifti_mat44_inverse( nifti_mat44 R )
{
   double r11,r12,r13,r21,r22,r23,r31,r32,r33,v1,v2,v3 , deti ;
   nifti_mat44 Q ;
                                                       /** INPUT MATRIX IS: **/
   r11 = R.m[0][0]; r12 = R.m[0][1]; r13 = R.m[0][2];  /* [ r11 r12 r13 v1 ] */
   r21 = R.m[1][0]; r22 = R.m[1][1]; r23 = R.m[1][2];  /* [ r21 r22 r23 v2 ] */
   r31 = R.m[2][0]; r32 = R.m[2][1]; r33 = R.m[2][2];  /* [ r31 r32 r33 v3 ] */
   v1  = R.m[0][3]; v2  = R.m[1][3]; v3  = R.m[2][3];  /* [  0   0   0   1 ] */

   deti = r11*r22*r33-r11*r32*r23-r21*r12*r33
         +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;

   if( deti != 0.0l ) deti = 1.0l / deti ;

   Q.m[0][0] = deti*( r22*r33-r32*r23) ;
   Q.m[0][1] = deti*(-r12*r33+r32*r13) ;
   Q.m[0][2] = deti*( r12*r23-r22*r13) ;
   Q.m[0][3] = deti*(-r12*r23*v3+r12*v2*r33+r22*r13*v3
                     -r22*v1*r33-r32*r13*v2+r32*v1*r23) ;

   Q.m[1][0] = deti*(-r21*r33+r31*r23) ;
   Q.m[1][1] = deti*( r11*r33-r31*r13) ;
   Q.m[1][2] = deti*(-r11*r23+r21*r13) ;
   Q.m[1][3] = deti*( r11*r23*v3-r11*v2*r33-r21*r13*v3
                     +r21*v1*r33+r31*r13*v2-r31*v1*r23) ;

   Q.m[2][0] = deti*( r21*r32-r31*r22) ;
   Q.m[2][1] = deti*(-r11*r32+r31*r12) ;
   Q.m[2][2] = deti*( r11*r22-r21*r12) ;
   Q.m[2][3] = deti*(-r11*r22*v3+r11*r32*v2+r21*r12*v3
                     -r21*r32*v1-r31*r12*v2+r31*r22*v1) ;

   Q.m[3][0] = Q.m[3][1] = Q.m[3][2] = 0.0l ;
   Q.m[3][3] = (deti == 0.0l) ? 0.0l : 1.0l ; /* failure flag if deti == 0 */

   return Q ;
}

/*---------------------------------------------------------------------------*/
/* Routines to swap byte arrays in various ways:
    -  2 at a time:  ab               -> ba               [short]
    -  4 at a time:  abcd             -> dcba             [int, float]
    -  8 at a time:  abcdDCBA         -> ABCDdcba         [long long, double]
    - 16 at a time:  abcdefghHGFEDCBA -> ABCDEFGHhgfedcba [long double]
-----------------------------------------------------------------------------*/

typedef struct { unsigned char a,b ; } twobytes ;

void swap_2bytes( int n , void *ar )
{
   register int ii ;
   register twobytes *tb = (twobytes *)ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
     tt = tb[ii].a ; tb[ii].a = tb[ii].b ; tb[ii].b = tt ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

typedef struct { unsigned char a,b,c,d ; } fourbytes ;

void swap_4bytes( int n , void *ar )
{
   register int ii ;
   register fourbytes *tb = (fourbytes *)ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
     tt = tb[ii].a ; tb[ii].a = tb[ii].d ; tb[ii].d = tt ;
     tt = tb[ii].b ; tb[ii].b = tb[ii].c ; tb[ii].c = tt ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

typedef struct { unsigned char a,b,c,d , D,C,B,A ; } eightbytes ;

void swap_8bytes( int n , void *ar )
{
   register int ii ;
   register eightbytes *tb = (eightbytes *)ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
     tt = tb[ii].a ; tb[ii].a = tb[ii].A ; tb[ii].A = tt ;
     tt = tb[ii].b ; tb[ii].b = tb[ii].B ; tb[ii].B = tt ;
     tt = tb[ii].c ; tb[ii].c = tb[ii].C ; tb[ii].C = tt ;
     tt = tb[ii].d ; tb[ii].d = tb[ii].D ; tb[ii].D = tt ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

typedef struct { unsigned char a,b,c,d,e,f,g,h ,
                               H,G,F,E,D,C,B,A  ; } sixteenbytes ;

void swap_16bytes( int n , void *ar )
{
   register int ii ;
   register sixteenbytes *tb = (sixteenbytes *)ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
     tt = tb[ii].a ; tb[ii].a = tb[ii].A ; tb[ii].A = tt ;
     tt = tb[ii].b ; tb[ii].b = tb[ii].B ; tb[ii].B = tt ;
     tt = tb[ii].c ; tb[ii].c = tb[ii].C ; tb[ii].C = tt ;
     tt = tb[ii].d ; tb[ii].d = tb[ii].D ; tb[ii].D = tt ;

     tt = tb[ii].e ; tb[ii].e = tb[ii].E ; tb[ii].E = tt ;
     tt = tb[ii].f ; tb[ii].f = tb[ii].F ; tb[ii].F = tt ;
     tt = tb[ii].g ; tb[ii].g = tb[ii].G ; tb[ii].G = tt ;
     tt = tb[ii].h ; tb[ii].h = tb[ii].H ; tb[ii].H = tt ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

void swap_Nbytes( int n , int siz , void *ar )
{
   switch( siz ){
     case 2:  swap_2bytes ( n , ar ) ; break ;
     case 4:  swap_4bytes ( n , ar ) ; break ;
     case 8:  swap_8bytes ( n , ar ) ; break ;
     case 16: swap_16bytes( n , ar ) ; break ;
   }
   return ;
}

/*---------------------------------------------------------------*/
/* Byte swap NIFTI-1 file header in various places and ways.
   If is_nifti is nonzero, will also swap the NIFTI-specific
   components of the header; otherwise, only the components
   common to NIFTI and ANALYZE will be swapped.
---------------------------------------------------------------- */

void swap_nifti_header( struct nifti_1_header *h , int is_nifti )
{

#if 0                /* ANALYZE fields not used by this software */
   swap_4(h->sizeof_hdr) ;
   swap_4(h->extents) ;
   swap_2(h->session_error) ;
   swap_4(h->compressed) ;
   swap_4(h->glmax) ; swap_4(h->glmin) ;
#endif

   /* this stuff is always present, for ANALYZE and NIFTI */

   swap_2bytes( 8 , h->dim ) ;
   swap_4bytes( 8 , h->pixdim ) ;

   swap_2(h->datatype) ;
   swap_2(h->bitpix) ;

   swap_4(h->vox_offset); swap_4(h->cal_max); swap_4(h->cal_min);

   /* this stuff is NIFTI specific */

   if( is_nifti ){
     swap_2(h->qform_code); swap_2(h->sform_code);
     swap_4(h->quatern_b); swap_4(h->quatern_c); swap_4(h->quatern_d);
     swap_4(h->qoffset_x); swap_4(h->qoffset_y); swap_4(h->qoffset_z);
     swap_4(h->intent_p1); swap_4(h->intent_p2); swap_4(h->intent_p3);
     swap_4(h->scl_slope); swap_4(h->scl_inter);
     swap_4bytes(4,h->srow_x);
     swap_4bytes(4,h->srow_y);
     swap_4bytes(4,h->srow_z);
     swap_2(h->intent_code); swap_4(h->toffset);
   }
   return ;
}

#define THIS_IS_UNIX
#ifdef  THIS_IS_UNIX
/*---------------------------------------------------------------------------*/
/* Return the file length (0 if file not found or has no contents).
   This is a Unix-specific function, since it uses stat().
-----------------------------------------------------------------------------*/
#include <sys/types.h>
#include <sys/stat.h>

unsigned int get_filesize( char *pathname )
{
   struct stat buf ; int ii ;

   if( pathname == NULL || *pathname == '\0' ) return 0 ;
   ii = stat( pathname , &buf ); if( ii != 0 ) return 0 ;
   return (unsigned int)buf.st_size ;
}

#else  /*---------- non-Unix version of the above, less efficient -----------*/

unsigned int get_filesize( char *pathname )
{
   FILE *fp ; unsigned int len ;

   if( pathname == NULL || *pathname == '\0' ) return 0 ;
   fp = fopen(pathname,"rb"); if( fp == NULL ) return 0 ;
   fseek(fp,0L,SEEK_END) ; len = (unsigned int)ftell(fp) ;
   fclose(fp) ; return len ;
}

#endif /* THIS_IS_UNIX */

/*--------------------------------------------------------------------------*/
/* Read in a NIFTI-1 or ANALYZE-7.5 file (pair) into a nifti_image struct.
    - Input is .hdr or .nii filename.
    - Return value is NULL if something fails badly.
    - If read_data parameter is nonzero, the image data will actually
      be read in; otherwise, it will have to be read later
      (e.g., using the nifti_image_load() function).
    - The image data will be stored in whatever data format the
      input data is; no scaling will be applied.
    - DT_BINARY data is not supported!
----------------------------------------------------------------------------*/

#undef  ERREX
#define ERREX(msg)                                           \
 do{ fprintf(stderr,"** ERROR: nifti_image_read(%s): %s\n",  \
             (hname != NULL) ? hname : "(null)" , (msg) ) ;  \
     return NULL ; } while(0)

nifti_image * nifti_image_read( char *hname , int read_data )
{
   struct nifti_1_header nhdr ;
   nifti_image *nim ;
   FILE *fp ;
   int   ii , doswap , hlen, ilen, ioff ;
   int   nx,ny,nz,nt,nu,nv,nw , ndim,nvox , ntot ;
   int   is_nifti , is_onefile ;
   float dx,dy,dz,dt,du,dv,dw ;
   short ss ;
   char *iname=NULL ;

   /** check input file(s) for sanity **/

   if( hname == NULL || *hname == '\0' ) ERREX("bad filename") ;

   hlen = strlen(hname) ; if( hlen < 5 ) ERREX("too short filename") ;

   /** open input file **/

   fp = fopen( hname , "rb" ) ;
   if( fp == NULL )                      ERREX("can't open header file") ;

   /** read header **/

   ii = fread( &nhdr , 1 , sizeof(nhdr) , fp ) ;          /* read the thing */
   fclose( fp ) ;                                         /* close the file */
   if( ii < sizeof(nhdr) )               ERREX("bad header read") ;

   /** check if have to swap header bytes **/

   doswap = 0 ;                                           /* swap data flag */
   ss = nhdr.dim[0] ;
   if( ss != 0 ){                            /* check dim[0] for good value */
     if( ss < 0 || ss > 7 ){
       swap_2(ss) ;
       if( ss < 0 || ss > 7 )            ERREX("bad dim[0]") ;
       doswap = 1 ;
     }
   } else {                       /* dim[0] == 0 is illegal, but does occur */
     ii = nhdr.sizeof_hdr ;            /* so check sizeof_hdr field instead */
     if( ii != sizeof(nhdr) ){
       swap_4(ii) ;
       if( ii != sizeof(nhdr) )          ERREX("bad sizeof_hdr") ;
       doswap = 1 ;
     }
   }

   /** determine if this is a NIFTI-1 compliant header **/

   is_nifti = NIFTI_VERSION(nhdr) ;
   if( doswap ) swap_nifti_header( &nhdr , is_nifti ) ;

   if( nhdr.datatype == DT_BINARY ||
       nhdr.datatype == DT_UNKNOWN  )    ERREX("bad datatype") ;

   if( nhdr.dim[1] <= 0 )                ERREX("bad dim[1]") ;

   for( ii=2 ; ii < 7 ; ii++ )
     if( nhdr.dim[ii] <= 0 ) nhdr.dim[ii] = 1 ;  /* fix bad dim[] values */

   /** if dim[0] is 0, get number of dimensions another way **/

   ndim = nhdr.dim[0] ;
   if( ndim == 0 ){
     for( ii=7 ; ii >= 2 ; ii++ )            /* loop backwards until we  */
       if( nhdr.dim[ii] > 1 ) break ;        /* find a dim bigger than 1 */
     ndim = ii ;
   }

   /* set bad grid spacings to 1.0 */

   for( ii=1 ; ii <= 7 ; ii++ ){
     if( nhdr.pixdim[ii] == 0.0         ||
         !IS_GOOD_FLOAT(nhdr.pixdim[ii])  ) nhdr.pixdim[ii] = 1.0 ;
   }

   /** will read image data from file 'iname' starting at offset 'ioff' **/

   is_onefile = is_nifti && NIFTI_ONEFILE(nhdr) ;

   if( is_onefile ){
     ioff = (int)nhdr.vox_offset ;
     if( ioff < sizeof(nhdr) ) ioff = sizeof(nhdr) ;
   } else {
     ioff = 0 ;
   }

   iname = strdup(hname) ;
   if( !is_onefile ) strcpy(iname+hlen-4,".img") ; /* create .img filename */

   ilen = get_filesize(iname) ;            /* find size of image data file */

   if( ilen <= ioff ){ free(iname) ; ERREX("bad data file") ; }

   /*=== create output image struct and start to set it up ===*/

   nim = (nifti_image *) calloc( 1 , sizeof(nifti_image) ) ;

   if( is_nifti ) nim->nifti_type = (is_onefile) ? 1 : 2 ;
   else           nim->nifti_type = 0 ;

   /** dimensions of data array **/

   nim->ndim = nim->dim[0] = ndim ;
   nim->nx   = nim->dim[1] = nhdr.dim[1]; nvox  = nim->nx;
   nim->ny   = nim->dim[2] = nhdr.dim[2]; nvox *= nim->ny;
   nim->nz   = nim->dim[3] = nhdr.dim[3]; nvox *= nim->nz;
   nim->nt   = nim->dim[4] = nhdr.dim[4]; nvox *= nim->nt;
   nim->nu   = nim->dim[5] = nhdr.dim[5]; nvox *= nim->nu;
   nim->nv   = nim->dim[6] = nhdr.dim[6]; nvox *= nim->nv;
   nim->nw   = nim->dim[7] = nhdr.dim[7]; nvox *= nim->nw; nim->nvox = nvox;

   /** type of data in voxels and how many bytes per voxel */

   nim->datatype = nhdr.datatype ;

   switch( nim->datatype ){
     default:
       free(nim) ; free(iname) ; ERREX("bad datatype") ;

     case DT_INT8:
     case DT_UINT8:       nim->nbyper =  1 ; nim->swapsize =  0 ; break ;

     case DT_INT16:
     case DT_UINT16:      nim->nbyper =  2 ; nim->swapsize =  2 ; break ;

     case DT_RGB24:       nim->nbyper =  3 ; nim->swapsize =  0 ; break ;

     case DT_INT32:
     case DT_UINT32:
     case DT_FLOAT32:     nim->nbyper =  4 ; nim->swapsize =  4 ; break ;

     case DT_COMPLEX64:   nim->nbyper =  8 ; nim->swapsize =  4 ; break ;

     case DT_FLOAT64:
     case DT_INT64:
     case DT_UINT64:      nim->nbyper =  8 ; nim->swapsize =  8 ; break ;

     case DT_FLOAT128:    nim->nbyper = 16 ; nim->swapsize = 16 ; break ;

     case DT_COMPLEX128:  nim->nbyper = 16 ; nim->swapsize =  8 ; break ;

     case DT_COMPLEX256:  nim->nbyper = 32 ; nim->swapsize = 16 ; break ;
   }
   if( !doswap ) nim->swapsize = 0 ;

   /** grid spacings **/

   nim->dx = nim->pixdim[1] = nhdr.pixdim[1] ;
   nim->dy = nim->pixdim[2] = nhdr.pixdim[2] ;
   nim->dz = nim->pixdim[3] = nhdr.pixdim[3] ;
   nim->dt = nim->pixdim[4] = nhdr.pixdim[4] ;
   nim->du = nim->pixdim[5] = nhdr.pixdim[5] ;
   nim->dv = nim->pixdim[6] = nhdr.pixdim[6] ;
   nim->dw = nim->pixdim[7] = nhdr.pixdim[7] ;

   /** compute qto_xyz transformation from pixel indexes (i,j,k) to (x,y,z) **/

   /* last row is always [ 0 0 0 1 ] */

   nim->qto_xyz.m[3][0]=nim->qto_xyz.m[3][1]=nim->qto_xyz.m[3][2] = 0.0;
   nim->qto_xyz.m[3][3]= 1.0 ;

   if( !is_nifti || nhdr.qform_code <= 0 ){ /** default transformation **/

     nim->qto_xyz.m[0][0] = nim->dx ;  /* grid spacings */
     nim->qto_xyz.m[1][1] = nim->dy ;
     nim->qto_xyz.m[2][2] = nim->dz ;

     nim->qto_xyz.m[0][1]=nim->qto_xyz.m[0][2]=nim->qto_xyz.m[0][3] = 0.0;
     nim->qto_xyz.m[1][0]=nim->qto_xyz.m[1][2]=nim->qto_xyz.m[1][3] = 0.0;
     nim->qto_xyz.m[2][0]=nim->qto_xyz.m[2][1]=nim->qto_xyz.m[2][3] = 0.0;

     nim->qform_code = NIFTI_XFORM_UNKNOWN ;

   } else {                 /** NIFTI: quaternion-specified transformation **/

     double a,b,c,d , qfac ;

     b = FIXED_FLOAT( nhdr.quatern_b ) ;
     c = FIXED_FLOAT( nhdr.quatern_c ) ;
     d = FIXED_FLOAT( nhdr.quatern_d ) ;
     a = 1.0l - (b*b + c*c + d*d) ;
     if( a < 0.0 ){                      /* weird quaternion input! */
       a = 1.0l / sqrt(1.0l-a) ;
       b *= a ; c *= a ; d *= a ;        /* normalize (b,c,d) vector */
       a = 0.0l ;                        /* a = 0 ==> 180 degree rotation */
     } else{
       a = sqrt(a) ;                     /* angle = 2*arccos(a) */
     }

     nim->quatern_b = b ; nim->quatern_c = c ; nim->quatern_d = d ;

     /* load rotation matrix, including scaling factors for voxel sizes */

     qfac = (nhdr.pixdim[0] < 0.0) ? -1.0l : 1.0l ;  /* left-handedness? */

     nim->qfac = qfac ;

     nim->qto_xyz.m[0][0] = (a*a+b*b-c*c-d*d) * nim->dx * qfac ;
     nim->qto_xyz.m[0][1] = (b*c-a*d        ) * nim->dy * 2.0l ;
     nim->qto_xyz.m[0][2] = (b*d+a*c        ) * nim->dz * 2.0l ;
     nim->qto_xyz.m[1][0] = (b*c+a*d        ) * nim->dx * qfac ;
     nim->qto_xyz.m[1][1] = (a*a+c*c-b*b-d*d) * nim->dy * 2.0l ;
     nim->qto_xyz.m[1][2] = (c*d-a*b        ) * nim->dz * 2.0l ;
     nim->qto_xyz.m[2][0] = (b*d-a*c        ) * nim->dx * qfac ;
     nim->qto_xyz.m[2][1] = (c*d+a*b        ) * nim->dy * 2.0l ;
     nim->qto_xyz.m[2][2] = (a*a+d*d-c*c-b*b) * nim->dz * 2.0l ;

     /* load offsets */

     nim->qoffset_x = nim->qto_xyz.m[0][3] = FIXED_FLOAT(nhdr.qoffset_x) ;
     nim->qoffset_y = nim->qto_xyz.m[0][3] = FIXED_FLOAT(nhdr.qoffset_y) ;
     nim->qoffset_z = nim->qto_xyz.m[2][3] = FIXED_FLOAT(nhdr.qoffset_z) ;

     nim->qform_code = nhdr.qform_code ;
   }

   /** load inverse transformation (x,y,z) -> (i,j,k) **/

   nim->qto_ijk = nifti_mat44_inverse( nim->qto_xyz ) ;

   /** load sto_xyz affine transformation, if present **/

   if( !is_nifti || nhdr.sform_code <= 0 ){ /** no sto transformation **/

     nim->sform_code = NIFTI_XFORM_UNKNOWN ;

   } else {                            /** sto transformation from srow_*[] **/

     nim->sto_xyz.m[0][0] = nhdr.srow_x[0] ;
     nim->sto_xyz.m[0][1] = nhdr.srow_x[1] ;
     nim->sto_xyz.m[0][2] = nhdr.srow_x[2] ;
     nim->sto_xyz.m[0][3] = nhdr.srow_x[3] ;

     nim->sto_xyz.m[1][0] = nhdr.srow_y[0] ;
     nim->sto_xyz.m[1][1] = nhdr.srow_y[1] ;
     nim->sto_xyz.m[1][2] = nhdr.srow_y[2] ;
     nim->sto_xyz.m[1][3] = nhdr.srow_y[3] ;

     nim->sto_xyz.m[2][0] = nhdr.srow_z[0] ;
     nim->sto_xyz.m[2][1] = nhdr.srow_z[1] ;
     nim->sto_xyz.m[2][2] = nhdr.srow_z[2] ;
     nim->sto_xyz.m[2][3] = nhdr.srow_z[3] ;

     nim->sto_xyz.m[3][0]=nim->sto_xyz.m[3][1]=nim->sto_xyz.m[3][2] = 0.0;
     nim->sto_xyz.m[3][3]= 1.0 ;

     nim->sto_ijk = nifti_mat44_inverse( nim->sto_xyz ) ;

     nim->sform_code = nhdr.sform_code ;
   }

   /* miscellaneous NIFTI stuff */

   if( is_nifti ){
     nim->scl_slope   = FIXED_FLOAT( nhdr.scl_slope ) ;
     nim->scl_inter   = FIXED_FLOAT( nhdr.scl_inter ) ;

     nim->intent_code = nhdr.intent_code ;

     nim->intent_p1 = FIXED_FLOAT( nhdr.intent_p1 ) ;
     nim->intent_p2 = FIXED_FLOAT( nhdr.intent_p2 ) ;
     nim->intent_p3 = FIXED_FLOAT( nhdr.intent_p3 ) ;

     nim->toffset   = FIXED_FLOAT( nhdr.toffset ) ;

     memcpy(nim->intent_name,nhdr.intent_name,15); nim->intent_name[15] = '\0';

     nim->xyz_units  = nhdr.xyz_units  ;
     nim->time_units = nhdr.time_units ;
   }

   /* Miscellaneous ANALYZE stuff */

   nim->cal_min = FIXED_FLOAT(nhdr.cal_min) ;
   nim->cal_max = FIXED_FLOAT(nhdr.cal_max) ;

   memcpy(nim->descrip ,nhdr.descrip ,79) ; nim->descrip [79] = '\0' ;
   memcpy(nim->aux_file,nhdr.aux_file,23) ; nim->aux_file[23] = '\0' ;

   /** read the data if desired, then bug out **/

   nim->fname        = strdup(hname) ;  /* save input filename */
   nim->iname        = iname ;          /* save image filename */
   nim->iname_offset = ioff ;

   if( read_data ) nifti_image_load( nim ) ;
   else            nim->data = NULL ;

   return nim ;
}

/*--------------------------------------------------------------------------*/
/* Load the image data from disk into an already-prepared image struct.
----------------------------------------------------------------------------*/

#undef  ERREX
#define ERREX(msg)                                               \
 do{ fprintf(stderr,"** ERROR: nifti_image_load: %s\n",(msg)) ;  \
     return ; } while(0)

void nifti_image_load( nifti_image *nim )
{
   int ntot , ii ;
   FILE *fp ;

   if( nim == NULL      || nim->iname == NULL ||
       nim->nbyper <= 0 || nim->nvox <= 0       ) ERREX("bad input struct") ;

   /** open image data file **/

   fp = fopen( nim->iname , "rb" ) ;
   if( fp == NULL ) ERREX("Can't open data file") ;
   fseek( fp , nim->iname_offset , SEEK_SET ) ;

   /* make space for data, then read all of it in one operation */

   if( nim->data != NULL ) free(nim->data) ;

   ntot      = nim->nbyper * nim->nvox ;            /* total number of bytes */
   nim->data = malloc( ntot ) ;
   if( nim->data == NULL ) ERREX("can't malloc array space") ;

   ii = fread( nim->data , 1 , ntot , fp ) ;              /*** data input! ***/
   fclose( fp ) ;

   /* if read was short, fill rest of array with 0 bytes */

   if( ii < ntot ){
     fprintf(stderr,"++ WARNING: nifti_image_load(%s):\n"
                    "   data bytes needed = %d\n"
                    "   data bytes input  = %d\n"
                    "   number missing    = %d (set to 0)\n",
             nim->iname , ntot, ii, ntot-ii ) ;
     memset( (char *)(nim->data)+ii , 0 , ntot-ii ) ;
   }

   /** byte swap array if needed **/

   if( nim->swapsize > 0 )
     swap_Nbytes( ntot / nim->swapsize , nim->swapsize , nim->data ) ;

#ifdef isfinite
   /** check input float arrays for goodness, and fix bad numbers **/

   switch( nim->datatype ){

     case NIFTI_TYPE_FLOAT32:
     case NIFTI_TYPE_COMPLEX64:{
       register float *far = (float *)nim->data ; register int jj,nj ;
       nj = ntot / nim->swapsize ;
       for( jj=0 ; jj < nj ; jj++ ) far[jj] = FIXED_FLOAT(far[jj]);
     }
     break ;

     case NIFTI_TYPE_FLOAT64:
     case NIFTI_TYPE_COMPLEX128:{
       register double *far = (double *)nim->data ; register int jj,nj ;
       nj = ntot / nim->swapsize ;
       for( jj=0 ; jj < nj ; jj++ ) far[jj] = FIXED_FLOAT(far[jj]);
     }
     break ;

   }
#endif

   return ;
}

/*--------------------------------------------------------------------------*/
/* Unload the data in a nifti_image struct, but keep the metadata.
----------------------------------------------------------------------------*/

void nifti_image_unload( nifti_image *nim )
{
   if( nim != NULL && nim->data != NULL ){
     free(nim->data) ; nim->data = NULL ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/
/* Free a nifti_image struct that was read by nifti_image_read().
----------------------------------------------------------------------------*/

void nifti_image_free( nifti_image *nim )
{
   if( nim == NULL ) return ;
   if( nim->fname != NULL ) free(nim->fname) ;
   if( nim->iname != NULL ) free(nim->iname) ;
   if( nim->data  != NULL ) free(nim->data ) ;
   free(nim) ; return ;
}

/*--------------------------------------------------------------------------*/
/* Print to stdout some info about a nifti_image struct.
----------------------------------------------------------------------------*/

void nifti_image_infodump( nifti_image *nim )
{
   printf("\n"
          "++++++++++++++++++++++++\n"
          "++ nifti_image_infodump:" ) ;

   if( nim == NULL ){
     printf(" ?? input is NULL ??!!\n\n") ; return ;
   }

   printf("\n  header filename = %s\n",nim->fname) ;
   printf(  "  image  filename = %s  offset = %d\n",
          nim->iname,nim->iname_offset) ;

   printf("  ndim = %3d\n"
          "  nx   = %3d    dx = %g\n"
          "  ny   = %3d    dy = %g\n"
          "  nz   = %3d    dz = %g\n"
          "  nt   = %3d    dt = %g\n"
          "  nu   = %3d    du = %g\n"
          "  nv   = %3d    dv = %g\n"
          "  nw   = %3d    dw = %g\n" ,
       nim->ndim ,
       nim->nx , nim->dx , nim->ny , nim->dy ,
       nim->nz , nim->dz , nim->nt , nim->dt ,
       nim->nu , nim->du , nim->nv , nim->dv , nim->nw , nim->dw ) ;

   printf("  nvox = %d  nbyper = %d  datatype = %d (%s)\n",
          nim->nvox , nim->nbyper ,
          nim->datatype , nifti_datatype_string(nim->datatype) ) ;

   printf("  scl_slope = %g  scl_inter = %g\n" ,
          nim->scl_slope , nim->scl_inter       ) ;

   printf("  cal_min   = %g  cal_max   = %g\n" ,
          nim->cal_min   , nim->cal_max         ) ;

   printf("  intent_code = %d  intent_p1=%g  intent_p2=%g  intent_p3=%g\n" ,
          nim->intent_code, nim->intent_p1, nim->intent_p2, nim->intent_p3 ) ;

   if( nim->intent_name[0] != '\0' )
     printf("  intent_name = %s\n",nim->intent_name) ;

   if( nim->descrip[0] != '\0' )
     printf("  descrip = %s\n",nim->descrip) ;

   if( nim->aux_file[0] != '\0' )
     printf("  aux_file = %s\n",nim->aux_file) ;

   printf("  toffset = %g\n",nim->toffset) ;
   printf("  xyz_units = %d  time_units = %d\n" ,
          nim->xyz_units,nim->time_units ) ;

   printf("  qform_code = %d  qto_xyz matrix =\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n" ,
       nim->qform_code      ,
       nim->qto_xyz.m[0][0] , nim->qto_xyz.m[0][1] ,
       nim->qto_xyz.m[0][2] , nim->qto_xyz.m[0][3] ,
       nim->qto_xyz.m[1][0] , nim->qto_xyz.m[1][1] ,
       nim->qto_xyz.m[1][2] , nim->qto_xyz.m[1][3] ,
       nim->qto_xyz.m[2][0] , nim->qto_xyz.m[2][1] ,
       nim->qto_xyz.m[2][2] , nim->qto_xyz.m[2][3] ,
       nim->qto_xyz.m[3][0] , nim->qto_xyz.m[3][1] ,
       nim->qto_xyz.m[3][2] , nim->qto_xyz.m[3][3]  ) ;

   printf("  qto_ijk matrix =\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n" ,
       nim->qto_ijk.m[0][0] , nim->qto_ijk.m[0][1] ,
       nim->qto_ijk.m[0][2] , nim->qto_ijk.m[0][3] ,
       nim->qto_ijk.m[1][0] , nim->qto_ijk.m[1][1] ,
       nim->qto_ijk.m[1][2] , nim->qto_ijk.m[1][3] ,
       nim->qto_ijk.m[2][0] , nim->qto_ijk.m[2][1] ,
       nim->qto_ijk.m[2][2] , nim->qto_ijk.m[2][3] ,
       nim->qto_ijk.m[3][0] , nim->qto_ijk.m[3][1] ,
       nim->qto_ijk.m[3][2] , nim->qto_ijk.m[3][3]  ) ;

   if( nim->sform_code != NIFTI_XFORM_UNKNOWN ){
     printf("  sform_code = %d  sto_xyz matrix =\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n" ,
         nim->sform_code      ,
         nim->sto_xyz.m[0][0] , nim->sto_xyz.m[0][1] ,
         nim->sto_xyz.m[0][2] , nim->sto_xyz.m[0][3] ,
         nim->sto_xyz.m[1][0] , nim->sto_xyz.m[1][1] ,
         nim->sto_xyz.m[1][2] , nim->sto_xyz.m[1][3] ,
         nim->sto_xyz.m[2][0] , nim->sto_xyz.m[2][1] ,
         nim->sto_xyz.m[2][2] , nim->sto_xyz.m[2][3] ,
         nim->sto_xyz.m[3][0] , nim->sto_xyz.m[3][1] ,
         nim->sto_xyz.m[3][2] , nim->sto_xyz.m[3][3]  ) ;

     printf("  sto_ijk matrix =\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n"
            "    %9.6f %9.6f %9.6f   %9.6f\n" ,
         nim->sto_ijk.m[0][0] , nim->sto_ijk.m[0][1] ,
         nim->sto_ijk.m[0][2] , nim->sto_ijk.m[0][3] ,
         nim->sto_ijk.m[1][0] , nim->sto_ijk.m[1][1] ,
         nim->sto_ijk.m[1][2] , nim->sto_ijk.m[1][3] ,
         nim->sto_ijk.m[2][0] , nim->sto_ijk.m[2][1] ,
         nim->sto_ijk.m[2][2] , nim->sto_ijk.m[2][3] ,
         nim->sto_ijk.m[3][0] , nim->sto_ijk.m[3][1] ,
         nim->sto_ijk.m[3][2] , nim->sto_ijk.m[3][3]  ) ;
   }

   if( nim->data == NULL ) printf("  data not loaded\n") ;
   else                    printf("  data loaded at address %p\n",nim->data) ;

   return ;
}

/*--------------------------------------------------------------------------*/
/* Write a nifti_image to disk.  The following fields of nim affect how
   the output appears:
    - nifti_type = 0 ==> ANALYZE-7.5 format file pair will be written
    - nifti_type = 1 ==> NIFTI-1 format single file will be written
    - nifti_type = 2 ==> NIFTI_1 format file pair will be written
    - fname is the name of the output file (header or header+data)
    - if a file pair is being written, iname is the name of the data file
    - existing files WILL be overwritten with extreme prejudice
    - if qform_code > 0, the quatern_*, qoffset_*, and qfac fields determine
      the qform output, NOT the qto_xyz matrix; if you want to compute these
      fields from the qto_xyz matrix, you can use the utility function
      nifti_mat44_to_quatern()
----------------------------------------------------------------------------*/

#undef  ERREX
#define ERREX(msg)                                                \
 do{ fprintf(stderr,"** ERROR: nifti_image_write: %s\n",(msg)) ;  \
     return ; } while(0)

void nifti_image_write( nifti_image *nim )
{
   struct nifti_1_header nhdr ;
   FILE *fp ;
   size_t ss ;

   if( nim        == NULL                          ) ERREX("NULL input") ;
   if( nim->fname == NULL || nim->fname[0] == '\0' ) ERREX("bad fname input") ;
   if( nim->data  == NULL                          ) ERREX("no image data") ;

   memcpy(&nhdr,0,sizeof(nhdr)) ;  /* zero out header, to be safe */

   /* make iname from fname, if needed */

   if( nim->nifti_type != 1 ){            /* writing into 2 files */
     if( nim->iname != NULL && strcmp(nim->fname,nim->fname) == 0 ){
       free(nim->iname) ; nim->iname = NULL ;
     }
     if( nim->iname == NULL ){
       int ll = strlen(nim->fname) ; char *cc ;
       nim->iname = calloc(1,ll+5) ;
       strcpy(nim->iname,nim->fname) ;
       if( ll > 4 ) strcpy(nim->iname+ll-4,".img") ; /* create .img filename */
       else         strcat(nim->iname     ,".img") ;
     }
     nim->iname_offset = 0 ;
   } else {
     nim->iname_offset = sizeof(nhdr) ;   /* writing into 1 file */
   }

   /** load the ANALYZE-7.5 generic parts of the header **/

   nhdr.sizeof_hdr = sizeof(nhdr) ;
   nhdr.regular    = 'r' ;

   nhdr.dim[0] = nim->ndim ;
   nhdr.dim[1] = nim->nx ; nhdr.dim[2] = nim->ny ; nhdr.dim[3] = nim->nz ;
   nhdr.dim[4] = nim->nt ; nhdr.dim[5] = nim->nu ; nhdr.dim[6] = nim->nv ;
   nhdr.dim[7] = nim->nw ;

   nhdr.pixdim[0] = 0.0 ;
   nhdr.pixdim[1] = nim->dx ; nhdr.pixdim[2] = nim->dy ;
   nhdr.pixdim[3] = nim->dz ; nhdr.pixdim[4] = nim->dt ;
   nhdr.pixdim[5] = nim->du ; nhdr.pixdim[6] = nim->dv ;
   nhdr.pixdim[7] = nim->dw ;

   nhdr.datatype = nim->datatype ;
   nhdr.bitpix   = 8 * nim->nbyper ;

   if( nim->cal_max > nim->cal_min ){
     nhdr.cal_max = nim->cal_max ;
     nhdr.cal_min = nim->cal_min ;
   }

   if( nim->scl_slope != 0.0 ){
     nhdr.scl_slope = nim->scl_slope ;
     nhdr.scl_inter = nim->scl_inter ;
   }

   if( nim->descrip[0] != '\0' ){
     memcpy(nhdr.descrip ,nim->descrip ,79) ; nhdr.descrip[79] = '\0' ;
   }
   if( nim->aux_file[0] != '\0' ){
     memcpy(nhdr.aux_file ,nim->aux_file ,23) ; nhdr.aux_file[23] = '\0' ;
   }

   /** Load NIFTI specific stuff into the header **/

   if( nim->nifti_type > 0 ){

     if( nim->nifti_type == 1 ) strcpy(nhdr.magic,"n+1") ;   /* 1 file */
     else                       strcpy(nhdr.magic,"ni1") ;   /* 2 files */

     nhdr.intent_code = nim->intent_code ;
     nhdr.intent_p1   = nim->intent_p1 ;
     nhdr.intent_p2   = nim->intent_p2 ;
     nhdr.intent_p3   = nim->intent_p3 ;
     if( nim->intent_name[0] != '\0' ){
       memcpy(nhdr.intent_name,nim->intent_name,15) ;
       nhdr.intent_name[15] = '\0' ;
     }

     nhdr.vox_offset  = (float) nim->iname_offset ;
     nhdr.xyz_units   = (char) nim->xyz_units ;
     nhdr.time_units  = (char) nim->time_units ;
     nhdr.toffset     = nim->toffset ;

     if( nim->qform_code > 0 ){
       nhdr.qform_code = nim->qform_code ;
       nhdr.quatern_b  = nim->quatern_b ;
       nhdr.quatern_c  = nim->quatern_c ;
       nhdr.quatern_d  = nim->quatern_d ;
       nhdr.qoffset_x  = nim->qoffset_x ;
       nhdr.qoffset_y  = nim->qoffset_y ;
       nhdr.qoffset_z  = nim->qoffset_z ;
       nhdr.pixdim[0]  = nim->qfac ;
     }

     if( nim->sform_code > 0 ){
       nhdr.sform_code = nim->sform_code ;
       nhdr.srow_x[0] = nim->sto_xyz.m[0][0] ;
       nhdr.srow_x[1] = nim->sto_xyz.m[0][1] ;
       nhdr.srow_x[2] = nim->sto_xyz.m[0][2] ;
       nhdr.srow_x[3] = nim->sto_xyz.m[0][3] ;
       nhdr.srow_y[0] = nim->sto_xyz.m[1][0] ;
       nhdr.srow_y[1] = nim->sto_xyz.m[1][1] ;
       nhdr.srow_y[2] = nim->sto_xyz.m[1][2] ;
       nhdr.srow_y[3] = nim->sto_xyz.m[1][3] ;
       nhdr.srow_z[0] = nim->sto_xyz.m[2][0] ;
       nhdr.srow_z[1] = nim->sto_xyz.m[2][1] ;
       nhdr.srow_z[2] = nim->sto_xyz.m[2][2] ;
       nhdr.srow_z[3] = nim->sto_xyz.m[2][3] ;
     }
   }

   /** Open file, write header **/

   fp = fopen( nim->fname , "wb" ) ;
   if( fp == NULL ) ERREX("can't open output file") ;

   ss = fwrite( &nhdr , 1 , sizeof(nhdr) , fp ) ;
   if( ss < sizeof(nhdr) ){
     fclose(fp) ; ERREX("bad write to output file") ;
   }

   /** If not writing 1 file, close header and open image file **/

   if( nim->nifti_type != 1 ){
     fclose(fp) ;
     fp = fopen( nim->iname , "wb" ) ;
     if( fp == NULL ) ERREX("can't open image file") ;
   }

   /** Write all the image data at once **/

   ss = fwrite( nim->data , nim->nbyper , nim->nvox , fp ) ;
   fclose(fp) ;
   if( ss < nim->nvox ) ERREX("bad write to image file") ;
   return ;
}

/****************************************************************************/
int main( int argc , char *argv[] )
{
   nifti_image *nim ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: nifti1 infile [outfile]\n") ;
     printf("sizeof(nifti_1_header)=%d\n",sizeof(nifti_1_header)) ;
     exit(0) ;
   }

   nim = nifti_image_read( argv[1] , 1 ) ;
   if( nim == NULL ) exit(1) ;
   nifti_image_infodump( nim ) ;
   exit(0) ;
}
