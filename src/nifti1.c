#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "nifti1.h"

int main( int argc , char *argv[] )
{
   nifti_1_header h ;
   printf("%d\n",sizeof(h)) ; exit(0) ;
}

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

  char *fname[] ;                 /* input filename (.hdr or .nii) */
  char descrip[80], aux_file[24];
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
nifti_image * nifti_image_read( char *hname ) ;
void nifti_image_free( nifti_image *nim ) ;
void nifti_image_infodump( nifti_image *nim ) ;

/*-------------------- Some C convenience macros ----------------------------*/

#define swap_2(s) swap_2bytes(1,&(s))  /* s is a 2-byte short; swap in place */
#define swap_4(v) swap_4bytes(1,&(v))  /* v is a 4-byte value; swap in place */

#define USE_FINITE      /* use finite() to check floats/doubles for goodness */
#ifdef  USE_FINITE
#  define IS_GOOD_FLOAT(x) finite(x)         /* check if x is a "good" float */

#  define FIXED_FLOAT(x)   (IS_GOOD_FLOAT(x) ? (x) : 0.0)    /* fixed if bad */
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
     Case DT_UINT16:     return "UINT16"     ;
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

   return "**illegal**" ;
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

   if( deti != 0.0 ) deti = 1.0 / deti ;

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

   Q.m[3][0] = Q.m[3][1] = Q.m[3][2] = 0.0 ;
   Q.m[3][3] = (deti == 0.0) ? 0.0 : 1.0 ;  /* failure flag if deti == 0.0 */

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
}

/*---------------------------------------------------------------*/
/* Byte swap NIFTI-1 file header in various places and ways.
   If is_nifti is nonzero, will also swap the NIFTI-specific
   components of the header.
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

   swap_2(h->datatype) ; swap_2(h->bitpix) ;

   swap_4(h->vox_offset) ; swap_4(h->cal_max) ; swap_4(h->cal_min) ;

   if( is_nifti ){
     swap_2(h->qform_code) ; swap_2(h->sform_code) ;
     swap_4(h->quatern_b); swap_4(h->quatern_c); swap_4(h->quatern_d);
     swap_4(h->qoffset_x); swap_4(h->qoffset_y); swap_4(h->qoffset_z);
     swap_4(h->intent_p1); swap_4(h->intent_p2); swap_4(h->intent_p3);
     swap_4(h->scl_slope); swap_4(h->scl_inter);
     swap_4bytes(4,h->srow_x);
     swap_4bytes(4,h->srow_y);
     swap_4bytes(4,h->srow_z);
     swap_2(h->intent_code) ; swap_4(h->toffset) ;
   }
}

#define THIS_IS_UNIX
#ifdef  THIS_IS_UNIX
/*---------------------------------------------------------------------------*/
/* Return the file length (-1 if file not found).
   This is a Unix-specific function, since it uses stat().
-----------------------------------------------------------------------------*/
#include <sys/types.h>
#include <sys/stat.h>

unsigned int get_filesize( char *pathname )
{
   struct stat buf ; int ii ;

   if( pathname == NULL || *pathname == '\0' ) return -1 ;
   ii = stat( pathname , &buf ); if( ii != 0 ) return -1 ;
   return buf.st_size ;
}

#else  /*---------- non-Unix version of the above, less efficient -----------*/

unsigned int get_filesize( char *pathname )
{
   FILE *fp ; unsigned int len ;

   if( pathname == NULL || *pathname == '\0' ) return -1 ;
   fp = fopen(pathname,"rb"); if( fp == NULL ) return -1 ;
   fseek(fp,0L,SEEK_END) ; len = (unsigned int)ftell(fp) ;
   fclose(fp) ; return len ;
}

#endif /* THIS_IS_UNIX */

/*--------------------------------------------------------------------------*/
/* Read in a NIFTI-1 or ANALYZE-7.5 file (pair) into a nifti_image struct.
    - Input is .hdr or .nii filename.
    - Return value is NULL if something fails badly.
    - The image data will be stored in whatever data format the
      input data is; no scaling will be applied.
    - DT_BINARY data is not supported!
----------------------------------------------------------------------------*/

#undef  ERREX
#define ERREX(msg) \
 do{ fprintf(stderr,"** nifti_image_read: %s\n",(msg)); return NULL; } while(0)

nifti_image * nifti_image_read( char *hname )
{
   struct nifti_1_header nhdr ;
   nifti_image *nim ;
   FILE *fp ;
   int   ii , doswap , hlen, ilen, ioff ;
   int   nx,ny,nz,nt,nu,nv,nw , ndim,nvox , ntot ;
   int   is_nifti , is_onefile , swapsize ;
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

   is_nifti   = NIFTI_VERSION(nhdr) ;
   if( doswap ) swap_nifti_header( &nhdr , is_nifti ) ;

   if( nhdr.datatype == DT_BINARY ||
       nhdr.datatype == DT_UNKNOWN  )    ERREX("bad datatype") ;

   is_onefile = is_nifti && NIFTI_ONEFILE(nhdr) ;

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

   /** create output image struct and start to set it up **/

   nim = (nifti_image *) calloc( 1 , sizeof(nifti_image) ) ;

   if( is_nifti ) nim->nifti_type = (is_onefile) ? 1 : 2 ;
   else           nim->nifti_type = 0 ;

   /** dimensions of data array **/

   nim->ndim = nim->dim[0] = ndim ;
   nim->nx   = nim->dim[1] = nhdr.dim[1] ; nvox  = nim->nx ;
   nim->ny   = nim->dim[2] = nhdr.dim[2] ; nvox *= nim->ny ;
   nim->nz   = nim->dim[3] = nhdr.dim[3] ; nvox *= nim->nz ;
   nim->nt   = nim->dim[4] = nhdr.dim[4] ; nvox *= nim->nt ;
   nim->nu   = nim->dim[5] = nhdr.dim[5] ; nvox *= nim->nu ;
   nim->nv   = nim->dim[6] = nhdr.dim[6] ; nvox *= nim->nv ;
   nim->nw   = nim->dim[7] = nhdr.dim[7] ; nvox *= nim->nw ; nim->nvox = nvox ;

   /** type of data in voxels and how many bytes per voxel */

   nim->datatype = nhdr.datatype ;

   switch( nim->datatype ){
     default:
       free(nim) ; ERREX("bad datatype") ;

     case DT_INT8:
     case DT_UINT8:         nim->byper =  1 ; swapsize =  0 ; break ;

     case DT_INT16:
     case DT_UINT16:        nim->byper =  2 ; swapsize =  2 ; break ;

     case DT_RGB24:         nim->byper =  3 ; swapsize =  0 ; break ;

     case DT_INT32:
     case DT_UINT32:
     case DT_FLOAT32:       nim->byper =  4 ; swapsize =  4 ; break ;

     case DT_COMPLEX64:     nim->byper =  8 ; swapsize =  4 ; break ;

     case DT_FLOAT64:
     case DT_INT64:
     case DT_UINT64:        nim->byper =  8 ; swapsize =  8 ; break ;

     case DT_FLOAT128:      nim->byper = 16 ; swapsize = 16 ; break ;

     case DT_COMPLEX128:    nim->byper = 16 ; swapsize =  8 ; break ;

     case DT_COMPLEX256:    nim->byper = 32 ; swapsize = 16 ; break ;
   }

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

     double a,b,c,d , pfac ;

     b = FIXED_FLOAT( nhdr.quatern_b ) ;
     c = FIXED_FLOAT( nhdr.quatern_c ) ;
     d = FIXED_FLOAT( nhdr.quatern_d ) ;
     a = 1.0 - (b*b + c*c + d*d) ;
     if( a < 0.0 ){                      /* weird quaternion input! */
       a = 1.0 / sqrt(1.0-a) ;
       b *= a ; c *= a ; d *= a ;        /* normalize (b,c,d) vector */
       a = 0.0 ;                         /* a = 0 ==> 180 degree rotation */
     } else{
       a = sqrt(a) ;                     /* angle = 2*arccos(a) */
     }

     /* load rotation matrix, including scaling factors for voxel sizes */

     pfac = (pixdim[0] < 0.0) ? -1.0 : 1.0 ;  /* left-handedness? */

     nim->qto_xyz.m[0][0] = (a*a+b*b-c*c-d*d) * nim->dx * pfac ;
     nim->qto_xyz.m[0][1] = (2*b*c-2*a*d    ) * nim->dy ;
     nim->qto_xyz.m[0][2] = (2*b*d+2*a*c    ) * nim->dz ;
     nim->qto_xyz.m[1][0] = (2*b*c+2*a*d    ) * nim->dx * pfac ;
     nim->qto_xyz.m[1][1] = (a*a+c*c-b*b-d*d) * nim->dy ;
     nim->qto_xyz.m[1][2] = (2*c*d-2*a*b    ) * nim->dz ;
     nim->qto_xyz.m[2][0] = (2*b*d-2*a*c    ) * nim->dx * pfac ;
     nim->qto_xyz.m[2][1] = (2*c*d+2*a*b    ) * nim->dy ;
     nim->qto_xyz.m[2][2] = (a*a+d*d-c*c-b*b) * nim->dz ;

     /* load offsets */

     nim->qto_xyz.m[0][3] = FIXED_FLOAT(nhdr.qoffset_x) ;
     nim->qto_xyz.m[0][3] = FIXED_FLOAT(nhdr.qoffset_y) ;
     nim->qto_xyz.m[2][3] = FIXED_FLOAT(nhdr.qoffset_z) ;

     nim->qform_code = nhdr.qform_code ;
   }

   /** load inverse transformation (x,y,z) -> (i,j,k) **/

   nim->qto_ijk = nifti_mat44_inverse( nim->qto_xyz ) ;

   /** load sto affine transformation, if present **/

   if( !is_nifti || nhdr.sform_code <= 0 ){ /** no sto transformation **/

     nim->sform_code = NIFTI_XFORM_UNKNOWN ;

   } else {                            /** sto transformation from srow_*[] **/

     nim->sto_xyz.m[0][0] = srow_x[0] ; nim->sto_xyz.m[0][1] = srow_x[1] ;
     nim->sto_xyz.m[0][2] = srow_x[2] ; nim->sto_xyz.m[0][3] = srow_x[3] ;

     nim->sto_xyz.m[1][0] = srow_y[0] ; nim->sto_xyz.m[1][1] = srow_y[1] ;
     nim->sto_xyz.m[1][2] = srow_y[2] ; nim->sto_xyz.m[1][3] = srow_y[3] ;

     nim->sto_xyz.m[2][0] = srow_z[0] ; nim->sto_xyz.m[2][1] = srow_z[1] ;
     nim->sto_xyz.m[2][2] = srow_z[2] ; nim->sto_xyz.m[2][3] = srow_z[3] ;

     nim->qto_xyz.m[3][0]=nim->qto_xyz.m[3][1]=nim->qto_xyz.m[3][2] = 0.0;
     nim->qto_xyz.m[3][3]= 1.0 ;

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

   /** now prepare to read the data **/

   fp = fopen( iname , "rb" ) ; free(iname) ;
   if( fp == NULL ){ free(nim); return NULL; }           /* shouldn't happen */
   fseek( fp , ioff , SEEK_SET ) ;

   /* make space for data, then read all of it in one operation */

   ntot      = nim->nbyper * nim->nvox ;            /* total number of bytes */
   nim->data = malloc( ntot ) ;
   if( nim->data == NULL ){ free(nim); return NULL; }      /* can't malloc!? */

   ii = fread( nim->data , 1 , ntot , fp ) ;              /*** data input! ***/
   fclose( fp ) ;

   /* if read was short, fill rest of array with 0 bytes */

   if( ii < ntot ) memset( (char *)(nim->data)+ii , 0 , ntot-ii ) ;

   /** byte swap array if needed **/

   if( doswap ) swap_Nbytes( dnum*nvox , dsiz , nim->data ) ;

#ifdef USE_FINITE
   /** check input float arrays for goodness, and fix bad numbers **/

   switch( nim->datatype ){

     case NIFTI_TYPE_FLOAT32:
     case NIFTI_TYPE_COMPLEX64:{
       register float *far = (float *)nim->data ; register int jj,nj ;
       nj = nvox * dnum ;
       for( jj=0 ; jj < nj ; jj++ ) far[jj] = FIXED_FLOAT(far[jj]);
     }
     break ;

     case NIFTI_TYPE_FLOAT64:
     case NIFTI_TYPE_COMPLEX128:{
       register double *far = (double *)nim->data ; register int jj,nj ;
       nj = nvox * dnum ;
       for( jj=0 ; jj < nj ; jj++ ) far[jj] = FIXED_FLOAT(far[jj]);
     }
     break ;

   }
#endif

   /***** return to the place whence we came *****/

   nim->fname = strdup(hname) ;  /* save input filename */
   return nim ;
}

/*--------------------------------------------------------------------------*/
/* Free a nifti_image struct that was read by nifti_image_read().
----------------------------------------------------------------------------*/

void nifti_image_free( nifti_image *nim )
{
   if( nim == NULL ) return ;
   if( nim->fname != NULL ) free(nim->fname) ;
   if( nim->data  != NULL ) free(nim->data ) ;
   free(nim) ; return ;
}

/*--------------------------------------------------------------------------*/
/* Print to stdout some info about a nifti_image struct.
----------------------------------------------------------------------------*/

void nifti_image_infodump( nifti_image *nim )
{
   printf("\n"
          "*** nifti_image_infodump:" ) ;

   if( nim == NULL ){
     printf(" ?? input is NULL ??\n\n") ; return ;
   }

   printf("\n  filename = %s\n",nim->fname) ;

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

   printf("  qto_xyz matrix =\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n" ,
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

   return ;
}
