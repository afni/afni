#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "nifti_1.h"

/*****===================================================================*****/
/*****      Sample functions to deal with NIFTI-1 and ANALYZE files      *****/
/*****...................................................................*****/
/*****            This code is released to the public domain.            *****/
/*****...................................................................*****/
/*****  Author: Robert W Cox, NIMH/NIH/DHHS/USA                          *****/
/*****  Date:   April 2003                                               *****/
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

  int ndim ;                      /* number of dimensions (1..7) */
  int nx,ny,nz , nt,nu,nv,nw ;    /* dimensions of grid array */
  int nvox ;                      /* number of voxels */
  int nbyper ;                    /* bytes per voxel */
  int datatype ;                  /* type of data in voxels: NIFTI_TYPE_* */

  float dx,dy,dz , dt,du,dv,dw ;  /* grid spacings */

  float scl_slope , scl_inter ;   /* affine scaling factors */

  nifti_mat44 to_global ;         /* transform (i,j,k) to global (X,Y,Z) */
  nifti_mat44 to_index ;          /* transform global (X,Y,Z) to (i,j,k) */

                                  /* this block of variables for NIFTI stuff */
  int nifti_type ;                /* 0 for ANALYZE, 1 for NIFTI-1 */
  int stat_code , coord_code ;    /* statistic and coordinate types */
  float statpar_1, statpar_2,     /* statistic parameters */
        statpar_3 ;

  char *fname ;                   /* filename */
  void *data ;                    /* pointer to data: nbyper*nvox bytes */

} nifti_image ;

/*****************************************************************************/
/*--------------- Prototypes of functions defined in this file --------------*/

char *nifti_datatype_string( int dt ) ;
nifti_mat44 nifti_mat44_inverse( nifti_mat44 R ) ;
int get_byte_order(void) ;
void swap_2bytes ( int n , void *ar ) ;
void swap_4bytes ( int n , void *ar ) ;
void swap_8bytes ( int n , void *ar ) ;
void swap_16bytes( int n , void *ar ) ;
void swap_Nbytes ( int n , int siz , void *ar ) ;
void swap_nifti_header( struct nifti_1_header *aptr ) ;
unsigned int get_filesize( char *pathname ) ;
nifti_image * nifti_image_read( char *hname ) ;
void nifti_image_free( nifti_image *nim ) ;
void nifti_image_infodump( nifti_image *nim ) ;

/*-------------------- Some C convenience macros ----------------------------*/

#define NIFTI_SUFFIX  ".nif"                 /* suffix for "all-in-one" file */

#define swap_2(s) swap_2bytes(1,&(s))  /* s is a 2-byte short; swap in place */
#define swap_4(v) swap_4bytes(1,&(v))  /* v is a 4-byte value; swap in place */

#define LSB_FIRST 1                          /* Least Significant Byte First */
#define MSB_FIRST 2                          /* Most  Significant Byte First */

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
   Don't free() this string!  It points to static storage.
-----------------------------------------------------------------------------*/

char *nifti_datatype_string( int dt )
{
   switch( dt ){
     case NIFTI_TYPE_UNKNOWN:        return "UNKNOWN" ;
     case NIFTI_TYPE_BINARY:         return "BINARY" ;
     case NIFTI_TYPE_UNSIGNED_CHAR:  return "UNSIGNED_CHAR" ;
     case NIFTI_TYPE_SIGNED_SHORT:   return "SIGNED_SHORT" ;
     case NIFTI_TYPE_SIGNED_INT:     return "SIGNED_INT" ;
     case NIFTI_TYPE_FLOAT:          return "FLOAT" ;
     case NIFTI_TYPE_COMPLEX:        return "COMPLEX" ;
     case NIFTI_TYPE_DOUBLE:         return "DOUBLE" ;
     case NIFTI_TYPE_RGB:            return "RGB" ;
     case NIFTI_TYPE_UINT16:         return "UINT16" ;
     case NIFTI_TYPE_INT64:          return "INT64" ;
     case NIFTI_TYPE_FLOAT128:       return "FLOAT128" ;
     case NIFTI_TYPE_COMPLEX128:     return "COMPLEX128" ;
     case NIFTI_TYPE_COMPLEX256:     return "COMPLEX256" ;
   }

   return "illegal" ;
}

/*---------------------------------------------------------------------------*/
/* Compute the inverse of a bordered 4x4 matrix.
   Numerical code fragments were generated by Maple 8.
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

   Q.m[3][0] = Q.m[3][1] = Q.m[3][2] = 0.0 ; Q.m[3][3] = 1.0 ;
   return Q ;
}

/*---------------------------------------------------------------------------*/
/* Determine the byte order of this CPU.
   We assume that there are only 2 cases, which can be determined by
   examining a 2 byte short.
-----------------------------------------------------------------------------*/

int get_byte_order(void)
{
   union { unsigned char bb[2] ;
           short         ss    ; } elvis ;

   elvis.bb[0] = 1 ; elvis.bb[1] = 0 ;
   return (elvis.ss == 1) ? LSB_FIRST : MSB_FIRST ;
}

/*---------------------------------------------------------------------------*/
/* Routines to swap byte arrays in various ways
      2 at a time:  ab               -> ba
      4 at a time:  abcd             -> dcba
      8 at a time:  abcdDCBA         -> ABCDdcba
     16 at a time:  abcdefghHGFEDCBA -> ABCDEFGHhgfedcba
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
---------------------------------------------------------------- */

void swap_nifti_header( struct nifti_1_header *aptr )
{
   swap_4(aptr->sizeof_hdr) ;
   swap_4(aptr->extents) ;
   swap_2(aptr->session_error) ;
   swap_2(aptr->dim[0]) ;
   swap_2(aptr->dim[1]) ;
   swap_2(aptr->dim[2]) ;
   swap_2(aptr->dim[3]) ;
   swap_2(aptr->dim[4]) ;
   swap_2(aptr->dim[5]) ;
   swap_2(aptr->dim[6]) ;
   swap_2(aptr->dim[7]) ;
   swap_4(aptr->statpar_1) ;   /* NIFTI variable */
   swap_4(aptr->statpar_2) ;   /* NIFTI variable */
   swap_4(aptr->statpar_3) ;   /* NIFTI variable */
   swap_2(aptr->stat_code) ;   /* NIFTI variable */
   swap_2(aptr->datatype) ;
   swap_2(aptr->bitpix) ;
   swap_4(aptr->pixdim[0]) ;
   swap_4(aptr->pixdim[1]) ;
   swap_4(aptr->pixdim[2]) ;
   swap_4(aptr->pixdim[3]) ;
   swap_4(aptr->pixdim[4]) ;
   swap_4(aptr->pixdim[5]) ;
   swap_4(aptr->pixdim[6]) ;
   swap_4(aptr->pixdim[7]) ;
   swap_4(aptr->vox_offset) ;
   swap_4(aptr->scl_slope) ;   /* NIFTI variable */
   swap_4(aptr->scl_inter) ;   /* NIFTI variable */
   swap_4(aptr->funused3) ;
   swap_4(aptr->cal_max) ;
   swap_4(aptr->cal_min) ;
   swap_4(aptr->compressed) ;
   swap_4(aptr->verified) ;
   swap_4(aptr->glmax) ;
   swap_4(aptr->glmin) ;
   swap_2(aptr->coord_code) ;  /* NIFTI variable */
   swap_4(aptr->quatern_b) ;   /* NIFTI variable */
   swap_4(aptr->quatern_c) ;   /* NIFTI variable */
   swap_4(aptr->quatern_d) ;   /* NIFTI variable */
   swap_4(aptr->offset_x) ;    /* NIFTI variable */
   swap_4(aptr->offset_y) ;    /* NIFTI variable */
   swap_4(aptr->offset_z) ;    /* NIFTI variable */
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
    - Input is .hdr or NIFTI_SUFFIX filename.
    - If input filename ends in .hdr, the .img file must exist also.
    - If input filename ends in NIFTI_SUFFIX, all data is in that one file.
    - Return value is NULL if something fails badly.
    - The output image data will be stored in whatever data format the
      input data is: no scaling will be applied.
----------------------------------------------------------------------------*/

nifti_image * nifti_image_read( char *hname )
{
   struct nifti_1_header nhdr ;
   nifti_image *nim ;
   FILE *fp ;
   int   ii , doswap , hlen, ilen, ioff , dsiz,dnum ;
   int   nx,ny,nz,nt,nu,nv,nw , ndim,nvox , ntot ;
   float dx,dy,dz,dt,du,dv,dw ;
   short ss ;
   char *iname=NULL ;

   /** check input file(s) for sanity **/

   if( hname == NULL || *hname == '\0' ) return NULL ;     /* bad filename */

   ii = strlen(hname) ;     if( ii < 5 ) return NULL ;     /* bad filename */

   hlen = get_filesize( hname ) ;                     /* input file length */
   if( hlen < sizeof(nhdr) )             return NULL ;   /* bad input file */

   /* will read image data from file 'iname' starting at offset 'ioff' */

   iname = strdup(hname) ;

   if( strcmp(hname+ii-4,".hdr") == 0 ){                  /* .hdr filename */
     strcpy(iname+ii-4,".img") ;               /* => create .img filename  */
     ioff = 0 ;
     ilen = get_filesize( iname ) ;
     if( ilen <= 0 )        { free(iname); return NULL; } /* bad .img file */

   } else if( strcmp(hname+ii-4,NIFTI_SUFFIX) == 0 ){     /* all data read */
     ioff = sizeof(nhdr) ;                                /* from one file */
     ilen = hlen ;

   } else                   { free(iname); return NULL; }  /* bad filename */

   /** open input file **/

   fp = fopen( hname , "rb" ) ;
   if( fp == NULL )         { free(iname); return NULL; } /* bad input file */

   /** read header **/

   memset( &nhdr , 0 , sizeof(nhdr) ) ;                   /* set to zero    */
   ii = fread( &nhdr , 1 , sizeof(nhdr) , fp ) ;          /* read the thing */
   fclose( fp ) ;                                         /* close the file */
   if( ii < sizeof(nhdr) )  { free(iname); return NULL; } /* bad .hdr read  */

   /** check if have to swap header bytes **/

   doswap = 0 ;                                           /* swap data flag */
   ss = nhdr.dim[0] ;
   if( ss != 0 ){                            /* check dim[0] for good value */
     if( ss < 0 || ss > 7 ){
       swap_2(ss) ;
       if( ss < 0 || ss > 7 ){ free(iname); return NULL; }    /* bad dim[0] */
       doswap = 1 ;
    }
   } else {                       /* dim[0] == 0 is illegal, but does occur */
     ii = nhdr.sizeof_hdr ;            /* so check sizeof_hdr field instead */
     if( ii != sizeof(nhdr) ){
       swap_4(ii) ;
       if( ii != sizeof(nhdr) ){ free(iname); return NULL; } /* bad */
       doswap = 1 ;
     }
   }

   if( doswap ) swap_nifti_header( &nhdr ) ;

   if( nhdr.dim[1] <= 0 ){ free(iname); return NULL; }      /* stupid data */

   /** if dim[0] is 0, get number of dimensions another way **/

   ndim = nhdr.dim[0] ;
   if( ndim == 0 ){
     for( ii=2 ; ii <= 7 ; ii++ )                  /* loop until we find a */
       if( nhdr.dim[ii] <= 1 ) break ;             /* dim that's too small */
     ndim = ii-1 ;
   }

   /* set unused dimensions to 1 */

   for( ii=ndim+1 ; ii <= 7 ; ii++ ) nhdr.dim[ii] = 1 ;

   /* set bad grid spacings to 1.0 */

   for( ii=1 ; ii <= 7 ; ii++ ){
     if( nhdr.pixdim[ii] == 0.0         ||
         !IS_GOOD_FLOAT(nhdr.pixdim[ii])  ) nhdr.pixdim[ii] = 1.0 ;
   }

   /** create output image struct and start to set it up **/

   nim = (nifti_image *) calloc( 1 , sizeof(nifti_image) ) ;

   /** determine if this is marked as being a NIFTI-1 header **/

   nim->nifti_type = ( nhdr.magic[0] == 'n' && nhdr.magic[1] == 'i' &&
                       nhdr.magic[2] == 'f' && nhdr.magic[3] == 't' &&
                       nhdr.magic[4] == 'i' && nhdr.magic[5] == '-' &&
                       nhdr.magic[6] == '1' && nhdr.magic[7] == '\0'   ) ;

   /* dimensions of array */

   nim->ndim = ndim ;
   nim->nx   = nhdr.dim[1] ; nvox  = nim->nx ;
   nim->ny   = nhdr.dim[2] ; nvox *= nim->ny ;
   nim->nz   = nhdr.dim[3] ; nvox *= nim->nz ;
   nim->nt   = nhdr.dim[4] ; nvox *= nim->nt ;
   nim->nu   = nhdr.dim[5] ; nvox *= nim->nu ;
   nim->nv   = nhdr.dim[6] ; nvox *= nim->nv ;
   nim->nw   = nhdr.dim[7] ; nvox *= nim->nw ; nim->nvox = nvox ;

   /* grid spacings */

   nim->dx = nhdr.pixdim[1] ; nim->dy = nhdr.pixdim[2] ;
   nim->dz = nhdr.pixdim[3] ; nim->dt = nhdr.pixdim[4] ;
   nim->du = nhdr.pixdim[5] ; nim->dv = nhdr.pixdim[6] ;
   nim->dw = nhdr.pixdim[7] ;

   /* transformation from pixel indexes (i,j,k) to global coordinates */
   /* last row is always [ 0 0 0 1 ] */

   nim->to_global.m[3][0]=nim->to_global.m[3][1]=nim->to_global.m[3][2] = 0.0;
   nim->to_global.m[3][3]= 1.0 ;

   if( nim->nifti_type == 0 ){        /* ANALYE-7.5: default transformation */

     nim->to_global.m[0][0] = nim->dx ;  /* grid spacings */
     nim->to_global.m[1][1] = nim->dy ;
     nim->to_global.m[2][2] = nim->dz ;

     nim->to_global.m[0][1]=nim->to_global.m[0][2]=nim->to_global.m[0][3] = 0.0;
     nim->to_global.m[1][0]=nim->to_global.m[1][2]=nim->to_global.m[1][3] = 0.0;
     nim->to_global.m[2][0]=nim->to_global.m[2][1]=nim->to_global.m[2][3] = 0.0;

   } else {                   /* NIFTI: quaternion-specified transformation */

     double a,b,c,d , xoff,yoff,zoff ;

     b = FIXED_FLOAT( nhdr.quatern_b ) ;
     c = FIXED_FLOAT( nhdr.quatern_c ) ;
     d = FIXED_FLOAT( nhdr.quatern_d ) ;
     a = 1.0 - (b*b + c*c + d*d) ;
     if( a < 0.0 ){ a = 1.0; b = c = d = 0.0; }  /* bad quaternion input! */
     else         { a = sqrt(a) ;             }

     /* load rotation matrix, including scaling factors for voxel sizes */

     nim->to_global.m[0][0] = (a*a+b*b-c*c-d*d) * nim->dx ;
     nim->to_global.m[0][1] = (2*b*c-2*a*d    ) * nim->dy ;
     nim->to_global.m[0][2] = (2*b*d+2*a*c    ) * nim->dz ;
     nim->to_global.m[1][0] = (2*b*c+2*a*d    ) * nim->dx ;
     nim->to_global.m[1][1] = (a*a+c*c-b*b-d*d) * nim->dy ;
     nim->to_global.m[1][2] = (2*c*d-2*a*b    ) * nim->dz ;
     nim->to_global.m[2][0] = (2*b*d-2*a*c    ) * nim->dx ;
     nim->to_global.m[2][1] = (2*c*d+2*a*b    ) * nim->dy ;
     nim->to_global.m[2][2] = (a*a+d*d-c*c-b*b) * nim->dz ;

     /* load offsets */

     xoff = FIXED_FLOAT(nhdr.offset_x) ;
     yoff = FIXED_FLOAT(nhdr.offset_y) ;
     zoff = FIXED_FLOAT(nhdr.offset_z) ;

     nim->to_global.m[0][3] = xoff ;
     nim->to_global.m[1][3] = yoff ;
     nim->to_global.m[2][3] = zoff ;
   }

   /* load inverse transformation (X,Y,Z) -> (i,j,k) */

   nim->to_index = nifti_mat44_inverse( nim->to_global ) ;

   /* miscellaneous NIFTI stuff */

   nim->scl_slope = FIXED_FLOAT( nhdr.scl_slope ) ;
   nim->scl_inter = 0.0 ;

   if( nim->nifti_type ){
     nim->stat_code  = nhdr.stat_code ;
     nim->coord_code = nhdr.coord_code ;
     if( nim->stat_code > 0 ){
       nim->statpar_1 = FIXED_FLOAT( nhdr.statpar_1 ) ;
       nim->statpar_2 = FIXED_FLOAT( nhdr.statpar_2 ) ;
       nim->statpar_3 = FIXED_FLOAT( nhdr.statpar_3 ) ;
     } else {
       nim->statpar_1 = nim->statpar_2 = nim->statpar_3 = 0.0 ;
     }
     nim->scl_inter = FIXED_FLOAT( nhdr.scl_inter ) ;
   }

   /* data type code and bytes per voxel */

   /* Also set dsiz = bytes per value within a voxel, and
               dnum = number of values within a voxel.
      Of course, dsiz*dnum == nim->nbyper must be true.
      We need dsiz and dnum to do proper byte swapping on the data. */

   nim->datatype = nhdr.datatype ;

   switch( nhdr.datatype ){
     default:                     free(iname); free(nim); return NULL;

     case NIFTI_TYPE_UINT8:       nim->nbyper = 1 ; dsiz = 1 ; dnum = 1; break;

     case NIFTI_TYPE_INT16:
     case NIFTI_TYPE_UINT16:      nim->nbyper = 2 ; dsiz = 2 ; dnum = 1; break;

     case NIFTI_TYPE_INT32:
     case NIFTI_TYPE_FLOAT32:     nim->nbyper = 4 ; dsiz = 2 ; dnum = 1; break;

     case NIFTI_TYPE_COMPLEX64:   nim->nbyper = 8 ; dsiz = 4 ; dnum = 2; break;

     case NIFTI_TYPE_FLOAT64:
     case NIFTI_TYPE_INT64:       nim->nbyper = 8 ; dsiz = 8 ; dnum = 1; break;

     case NIFTI_TYPE_RGB24:       nim->nbyper = 3 ; dsiz = 1 ; dnum = 3; break;

     case NIFTI_TYPE_COMPLEX128:  nim->nbyper = 16; dsiz = 8 ; dnum = 2; break;

     case NIFTI_TYPE_FLOAT128:    nim->nbyper = 16; dsiz = 16; dnum = 1; break;

     case NIFTI_TYPE_COMPLEX256:  nim->nbyper = 32; dsiz = 16; dnum = 2; break;
   }

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

   printf("  to_global matrix =\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n" ,
       nim->to_global.m[0][0] , nim->to_global.m[0][1] ,
       nim->to_global.m[0][2] , nim->to_global.m[0][3] ,
       nim->to_global.m[1][0] , nim->to_global.m[1][1] ,
       nim->to_global.m[1][2] , nim->to_global.m[1][3] ,
       nim->to_global.m[2][0] , nim->to_global.m[2][1] ,
       nim->to_global.m[2][2] , nim->to_global.m[2][3] ,
       nim->to_global.m[3][0] , nim->to_global.m[3][1] ,
       nim->to_global.m[3][2] , nim->to_global.m[3][3]  ) ;

   printf("  to_index matrix =\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n"
          "    %9.6f %9.6f %9.6f   %9.6f\n" ,
       nim->to_index.m[0][0] , nim->to_index.m[0][1] ,
       nim->to_index.m[0][2] , nim->to_index.m[0][3] ,
       nim->to_index.m[1][0] , nim->to_index.m[1][1] ,
       nim->to_index.m[1][2] , nim->to_index.m[1][3] ,
       nim->to_index.m[2][0] , nim->to_index.m[2][1] ,
       nim->to_index.m[2][2] , nim->to_index.m[2][3] ,
       nim->to_index.m[3][0] , nim->to_index.m[3][1] ,
       nim->to_index.m[3][2] , nim->to_index.m[3][3]  ) ;

   return ;
}
