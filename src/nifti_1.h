#ifndef _NIFTI_HEADER_
#define _NIFTI_HEADER_

/*****************************************************************************
      ** This file defines the "NIFTI-1" header format.               **
      ** It is derived from a meeting at the NIH (31 Mar 2003)        **
      ** of the Data Format Working Group (DFWG), chartered by the    **
      ** NIfTI (Neuroimaging Informatics Technology Initiative)       **
      ** at the National Institutes of Health (NIH).                  **
      **                                                              **
      ** This header is intended to be "mostly compatible" with the   **
      ** ANALYZE (TM) 7.5 file format.  Most of the "unused" fields   **
      ** in that format have been taken, and some of the lesser-used  **
      ** fields have been co-opted for other purposes.                **
      **                                                              **
      ** Neither the National Institutes of Health (NIH), the DFWG,   **
      ** nor any of the members or employees of these institutions    **
      ** imply any warranty of usefulness of this material for any    **
      ** purpose, and do not assume any liability for damages,        **
      ** incidental or otherwise, caused by any use of this document. **
******************************************************************************/

#define NIFTI_VERSION 1
#define NIFTI_DATE    "01 Apr 2003"  /* but not a joke */

/*---------------------------------------------------------------------------*/
/* Note that the ANALYZE-7.5 file header (dbh.h) is
         (c) Copyright 1986-1995
         Biomedical Imaging Resource
         Mayo Foundation

   Incorporation of components of dbh.h are by permission of the
   Mayo Foundation.

   !! Of course, I hope we do get this permission - RWCox !!

   The changes from the ANALYZE-7.5 file header in this file are released
   to the public domain.
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* In the comments below for each field, only NIFTI-1 specific
   requirements or changes from the ANALYZE-7.5 format are described.
   For convenience, the 348 byte header is described as a single
   struct, rather than as the ANALYZE-7.5 group of 3 structs.

   To flag such a struct as being conformant to the NIFTI-1 spec,
   the last 8 bytes of the header must be the C String "nifti-1";
   in hexadecimal, the 8 bytes 6E 69 66 74 69 2D 31 00.  Normally, such
   a "magic number" or flag goes at the start of the file, but trying
   to avoid clobbering widely-used ANALYZE-7.5 fields led to putting
   this marker last (but recall that "the last shall be first").
-----------------------------------------------------------------------------*/

struct nifti_1_header { /* NIFTI-1 usage         */  /* ANALYZE-7.5 field(s) */
                        /*.......................*/  /*......................*/
  int sizeof_hdr;       /* MUST be 348           */  /* int sizeof_hdr;      */
  char data_type[10];                                /* char data_type[10];  */
  char db_name[18];                                  /* char db_name[18];    */
  int extents;                                       /* int extents;         */
  short session_error;                               /* short session_error; */
  char regular;                                      /* char regular;        */
  char hkey_un0;                                     /* char hkey_un0;       */

  short dim[8];         /* Data array dimensions.*/  /* short dim[8];        */
  float statpar_1 ;     /* Data values can be    */  /* short unused8;       */
                        /*  interpreted as from  */  /* short unused9;       */
  float statpar_2 ;     /*  a given statistical  */  /* short unused10;      */
                        /*  distribution using   */  /* short unused11;      */
  float statpar_3 ;     /*  stat_code and up to  */  /* short unused12;      */
                        /*  3 parameters.        */  /* short unused13;      */
  short stat_code ;     /* NIFTI_STAT_* code.    */  /* short unused14;      */
  short datatype;       /* NIFTI_TYPE_* code.    */  /* short datatype;      */
  short bitpix;         /* Number bits/voxel.    */  /* short bitpix;        */
  short dim_un0;                                     /* short dim_un0;       */
  float pixdim[8];      /* Grid spacings.        */  /* float pixdim[8];     */
  float vox_offset;                                  /* float vox_offset;    */
  float scl_slope ;     /* Data scaling:         */  /* float funused1;      */
  float scl_inter ;     /*  slope*val+inter.     */  /* float funused2;      */
  float funused3;                                    /* float funused3;      */
  float cal_max;                                     /* float cal_max;       */
  float cal_min;                                     /* float cal_min;       */
  float compressed;                                  /* float compressed;    */
  float verified;                                    /* float verified;      */
  int glmax;                                         /* int glmax;           */
  int glmin;                                         /* int glmin;           */

  char descrip[80];                                  /* char descrip[80];    */
  char aux_file[24];                                 /* char aux_file[24];   */
  char orient;          /* NIFTI_ORNT_* code.    */  /* char orient;         */
  char originator[10];                               /* char originator[10]; */
  char generated[10];                                /* char generated[10];  */
  char scannum[10];                                  /* char scannum[10];    */
  char patient_id[10];                               /* char patient_id[10]; */
  char exp_date[10];                                 /* char exp_date[10];   */
  char exp_time[10];                                 /* char exp_time[10];   */
  char hist_un0;                                     /* char hist_un0[3];    */
  short coord_code ;    /* NIFTI_CORD code.      */
  float quatern_b ;     /* (x,y,z) orientation   */  /* int views;           */
  float quatern_c ;     /*  is given by a unit   */  /* int vols_added;      */
  float quatern_d ;     /*  quaternion.          */  /* int start_field;     */
  float offset_x ;      /* Absolute (X,Y,Z)      */  /* int field_skip;      */
  float offset_y ;      /*  coords of center of  */  /* int omax;            */
  float offset_z ;      /*  (0,0,0) voxel.       */  /* int omin;            */
  char magic[8] ;       /* MUST be "nifti-1\0".  */  /* int smax;            */
                                                     /* int smin;            */
} ;                     /**** 348 bytes total ****/

/*---------------------------------------------------------------------------*/
/* Acceptable values for datatype field.
   Values below 256 are ANALYZE-7.5 compatible.
   Larger values are NIFTI-1 additions.  They are all multiples of 256, so
   that no bits below position 8 are set in datatype.  But there is no need
   to use only powers-of-2, as the original ANALYZE-7.5 datatype codes do.
-----------------------------------------------------------------------------*/

#define NIFTI_TYPE_NONE            0
#define NIFTI_TYPE_UNKNOWN         0  /* what it says, dude           */
#define NIFTI_TYPE_BINARY          1  /* binary (1 bit/voxel)         */
#define NIFTI_TYPE_UNSIGNED_CHAR   2  /* unsigned char (8 bits/voxel) */
#define NIFTI_TYPE_SIGNED_SHORT    4  /* signed short (16 bits/voxel) */
#define NIFTI_TYPE_SIGNED_INT      8  /* signed int (32 bits/voxel)   */
#define NIFTI_TYPE_FLOAT          16  /* float (32 bits/voxel)        */
#define NIFTI_TYPE_COMPLEX        32  /* complex (64 bits/voxel)      */
#define NIFTI_TYPE_DOUBLE         64  /* double (64 bits/voxel)       */
#define NIFTI_TYPE_RGB           128  /* RGB triple (24 bits/voxel)   */

#define NIFTI_TYPE_UINT8           2  /* synonyms for above */
#define NIFTI_TYPE_INT16           4
#define NIFTI_TYPE_INT32           8
#define NIFTI_TYPE_FLOAT32        16
#define NIFTI_TYPE_COMPLEX64      32
#define NIFTI_TYPE_FLOAT64        64
#define NIFTI_TYPE_RGB24         128

#define NIFTI_TYPE_UINT16        256  /* unsigned short (16 bits)       */
#define NIFTI_TYPE_INT64         512  /* long long (64 bits)            */
#define NIFTI_TYPE_FLOAT128      768  /* long double (128 bits)         */
#define NIFTI_TYPE_COMPLEX128   1024  /* double complex (128 bits)      */
#define NIFTI_TYPE_COMPLEX256   1280  /* long double complex (256 bits) */

/*---------------------------------------------------------------------------*/
/* Acceptable values for stat_code field.
   Values 2..10 are compatible with AFNI 1.5x
   (which is why there is no code with value==1).
-----------------------------------------------------------------------------*/

                                   /* Parameters            */
                                   /*.......................*/
#define NIFTI_STAT_CORREL      2   /* Samples, fits, orts   */
#define NIFTI_STAT_TTEST       3   /* DOF                   */
#define NIFTI_STAT_FTEST       4   /* 2 DOF                 */
#define NIFTI_STAT_ZSCORE      5   /* no params             */
#define NIFTI_STAT_CHISQ       6   /* DOF                   */
#define NIFTI_STAT_BETA        7   /* a and b params        */
#define NIFTI_STAT_BINOM       8   /* # trials, p per trial */
#define NIFTI_STAT_GAMMA       9   /* shape, scale params   */
#define NIFTI_STAT_POISSON    10   /* mean                  */

#define NIFTI_STAT_NORMAL     11   /* mean, variance        */
#define NIFTI_STAT_FTEST_NONC 12   /* 2 DOF, noncentrality  */
#define NIFTI_STAT_CHISQ_NONC 13   /* DOF, noncentrality    */
#define NIFTI_STAT_LOGISTIC   14   /* location, scale       */
#define NIFTI_STAT_LAPLACE    15   /* location, scale       */
#define NIFTI_STAT_UNIFORM    16   /* start, end            */
#define NIFTI_STAT_TTEST_NONC 17   /* DOF, noncentrality    */
#define NIFTI_STAT_WEIBULL    18   /* location, scale, power*/
#define NIFTI_STAT_CHI        19   /* DOF                   */
#define NIFTI_STAT_INVGAUSS   20   /* mu, lambda            */
#define NIFTI_STAT_EXTVAL     21   /* location, scale       */

/*---------------------------------------------------------------------------*/
/* 3D IMAGE (VOLUME) ORIENTATION AND LOCATION IN SPACE

   In this discussion, the (x,y,z) axes refer to the axes of 3D data grid,
   with x varying most rapidly in the data array, y second most rapidly,
   and z third most rapidly.  The (X,Y,Z) axes refer to a subject-based
   global coordinate system, with
     +X = Right  +Y = Anterior  +Z = Superior.
   This is a right-handed coordinate system.  The (x,y,z) coordinate
   system is aligned with the 3D image axes as stored.

   The coord_code field indicates how the (X,Y,Z) coordinates can be
   interpreted relative to some standard space - see the NIFTI_CORD_*
   parameters #define-d below.  This code does not otherwise affect the
   orientation/location discussion.

   The discussion herein explains how to compute the (X,Y,Z) coordinates
   of a voxel stored at index triple (i,j,k).  In addition, some fascinating
   mathematical asides are included to facilitate your illumination.

   To start with, the (x,y,z) coordinates of the (i,j,k) voxel are
     x = pixdim[1]*i   for i=0..dim[1]-1
     y = pixdim[2]*j   for j=0..dim[2]-1
     z = pixdim[3]*k   for k=0..dim[3]-1
   where these coordinates are the CENTER of the voxel.  Note that
   negative pixdim[] values are possible and useful.  The (x,y,z) coordinate
   system must be right-handed.  For example, if a volume has
     i varies Right-to-Left
     j varies Posterior-to-Anterior
     k varies Inferior-to-Superior
   then (i,j,k) is a left-handed coordinate system.  In such a case,
   one way to deal with the (x,y,z) coordinates is to have
     +x = Right  +y = Anterior  +z = Superior
   which would imply
     pixdim[1] < 0   pixdim[2] > 0   pixdim[3] > 0
   That is, in this example, the +x axis points opposite to the
   direction of increasing i index.

   The orientation of the (x,y,z) axes relative to the (X,Y,Z) axes
   in 3D space is specified using a unit quaternion [a,b,c,d], where
   a*a+b*b+c*c+d*d=1.  The (b,c,d) values are all that is needed, since
   we require that a = sqrt(1.0-b*b+c*c+d*d) be positive.  The (b,c,d)
   values are stored in the (quatern_b,quatern_c,quatern_d) fields.

   The quaternion representation is chosen for its compactness in
   representing rotations. The (proper) 3x3 rotation matrix that
   corresponds to [a,b,c,d] is

         [ a*a+b*b-c*c-d*d   2*b*c-2*a*d       2*b*d+2*a*c     ]
     R = [ 2*b*c+2*a*d       a*a+c*c-b*b-d*d   2*c*d-2*a*b     ]
         [ 2*b*d-2*a*c       2*c*d+2*a*b       a*a+d*d-c*c-b*b ]

         [ R11               R12               R13             ]
      == [ R21               R22               R23             ]
         [ R31               R32               R33             ]

   If (p,q,r) is a unit 3-vector, then rotation of angle h about that
   direction is represented by the quaternion

     [a,b,c,d] = [cos(h/2), p*sin(h/2), q*sin(h/2), r*sin(h/2)].

   Requiring a > 0 is equivalent to requiring -Pi < h < Pi.  To rotate a
   3-vector (x,y,z) using quaternions, you compute the quaternion product

     [0,x',y',z'] = [a,b,c,d] * [0,x,y,z] * [a,-b,-c,-d]

   which is equivalent to the matrix-vector multiply

     [ x' ]     [ x ]
     [ y' ] = R [ y ]   (equivalence depends on a*a+b*b+c*c+d*d=1)
     [ z' ]     [ z ]

   Multiplication of 2 quaternions is defined by the following:

     [a,b,c,d] == a*1 + b*I + c*J + d*K
     where
       I*I = J*J = K*K = -1 (I,J,K are square roots of -1)
       I*J =  K  J*K =  I  K*I =  J
       J*I = -K  K*J = -I  I*K = -J  (not commutative!)
     For example
       [a,b,0,0] * [0,0,0,1] = [0,-b,0,a]
     since this expands to
       (a+b*I)*(K) = (a*K+b*I*K) = (a*K-b*J).

   The columns of the R matrix are unit vectors that represent
   the directions of the 3D image x-axis, y-axis, and z-axis with
   respect to the (X,Y,Z) axes; that is, the direction cosines of the
   image x-axis are the first column of this matrix, etc.  These
   direction cosines are relative to the subject-based (X,Y,Z)
   coordinate system, where
     +X = Right  +Y = Anterior  +Z = Superior
   For example, if the quatern_* fields are all zero, then R is
   the identity matrix.

   The above formula shows how to go from quaternion (b,c,d) to
   rotation matrix and direction cosines.  Conversely, given R,
   we can compute the fields for the NIFTI-1 header by

     a = 0.5  * sqrt(1+R11+R22+R33)     (not stored)
     b = 0.25 * (R32-R23) / a       ==> quatern_b
     c = 0.25 * (R13-R31) / a       ==> quatern_c
     d = 0.25 * (R21-R12) / a       ==> quatern_d

   Note that R-transpose (== R-inverse) would lead to the quaternion
   [a,-b,-c,-d].

   R is the rotation that takes (x,y,z) coordinates to (X,Y,Z) orientation:

     [ X ]     [ x ] + [ offset_x ]
     [ Y ] = R [ y ] + [ offset_y ]
     [ Z ]     [ z ] + [ offset_z ]

   The (X,Y,Z) coordinates of the center of the (i,j,k) voxel in the
   3D image are thus given by

     X = R11*pixdim[1]*i + R12*pixdim[2]*j + R13*pixdim[3]*k + offset_x

     Y = R21*pixdim[1]*i + R21*pixdim[2]*j + R23*pixdim[3]*k + offset_y

     Z = R31*pixdim[1]*i + R32*pixdim[2]*j + R33*pixdim[3]*k + offset_z

   Note that some pixdim[] elements may need to be negative.
   For example, if the 3D image as stored is in order
     x=Right-to-Left  y=Anterior-to-Posterior  z=Inferior-to-Superior
   then R = identity, but we must have pixdim[1] and pixdim[2] negative,
   since the x- and y-axes are oriented backwards from the X- and Y-axes.

   The choice to specify the offset_x (etc.) values in the absolute (X,Y,Z)
   coordinate system is partly to make it easy to convert DICOM images to
   this format.  The DICOM attribute "Image Position (Patient)" (0020,0032)
   stores the (Xd,Yd,Zd) coordinates of the center of the first voxel.
   Here, (Xd,Yd,Zd) refer to DICOM coordinates, and Xd=-X, Yd=-Y, Zd=Z
   (i.e., DICOM +Xd is Right, +Yd is Posterior, +Zd is Superior).
-----------------------------------------------------------------------------*/

   /* coord_code values:    */   /* X,Y,Z coordinate system refers to:   */
   /*.......................*/   /*......................................*/
#define NIFTI_CORD_ARBITRARY  0  /* Arbitrary (scanner-based?)           */
#define NIFTI_CORD_TALAIRACH  1  /* Talairach-Tournoux Atlas; (0,0,0)=AC */
#define NIFTI_CORD_MNI_152    2  /* MNI 152 normalized coords            */

/*---------------------------------------------------------------------------*/
/* Acceptable values for orient field.
   The initials after NIFTI_ORNT_ refer to the axis orientation:
     R = Right-to-Left          L = Left-to-Right
     A = Anterior-to-Posterior  P = Posterior-to-Anterior
     I = Inferior-to-Superior   S = Superior-to-Inferior
   The first initial is the orientation of the x-axis, the
   second the y-axis, the third the z-axis.  For example,
   NIFTI_ORNT_LPI means
      -x = Left       +x = Right
      -y = Posterior  +y = Anterior
      -z = Inferior   +z = Superior
   The first 6 codes (0..5) correspond to those defined by the ANALYZE-7.5
   format.  (The ANALYZE-7.5 documentation is unclear on what the orient
   codes exactly mean; I hope these interpretations are correct!)
   The remaining 42 codes are NIFTI-1 additions.
-----------------------------------------------------------------------------*/

#define NIFTI_ORNT_RPI      0   /* ANALYZE-7.5: transverse unflipped */
#define NIFTI_ORNT_RIP      1   /* ANALYZE-7.5: coronal    unflipped */
#define NIFTI_ORNT_PIR      2   /* ANALYZE-7.5: sagittal   unflipped */
#define NIFTI_ORNT_RAI      3   /* ANALYZE-7.5: transverse flipped   */
#define NIFTI_ORNT_RSP      4   /* ANALYZE-7.5: coronal    flipped   */
#define NIFTI_ORNT_PSR      5   /* ANALYZE-7.5: sagittal   flipped   */

#define NIFTI_ORNT_RAS      6
#define NIFTI_ORNT_RPS      7
#define NIFTI_ORNT_RIA      8
#define NIFTI_ORNT_RSA      9
#define NIFTI_ORNT_LAI     10
#define NIFTI_ORNT_LAS     11
#define NIFTI_ORNT_LPI     12
#define NIFTI_ORNT_LPS     13
#define NIFTI_ORNT_LIA     14
#define NIFTI_ORNT_LIP     15
#define NIFTI_ORNT_LSA     16
#define NIFTI_ORNT_LSP     17
#define NIFTI_ORNT_AIR     18
#define NIFTI_ORNT_ASR     19
#define NIFTI_ORNT_IAR     20
#define NIFTI_ORNT_IPR     21
#define NIFTI_ORNT_SAR     22
#define NIFTI_ORNT_SPR     23
#define NIFTI_ORNT_AIL     24
#define NIFTI_ORNT_ASL     25
#define NIFTI_ORNT_PIL     26
#define NIFTI_ORNT_PSL     27
#define NIFTI_ORNT_IAL     28
#define NIFTI_ORNT_IPL     29
#define NIFTI_ORNT_SAL     30
#define NIFTI_ORNT_SPL     31
#define NIFTI_ORNT_IRA     32
#define NIFTI_ORNT_SRA     33
#define NIFTI_ORNT_IRP     34
#define NIFTI_ORNT_SRP     35
#define NIFTI_ORNT_ARI     36
#define NIFTI_ORNT_PRI     37
#define NIFTI_ORNT_ARS     38
#define NIFTI_ORNT_PRS     39
#define NIFTI_ORNT_ILA     40
#define NIFTI_ORNT_SLA     41
#define NIFTI_ORNT_ILP     42
#define NIFTI_ORNT_SLP     43
#define NIFTI_ORNT_ALI     44
#define NIFTI_ORNT_PLI     45
#define NIFTI_ORNT_ALS     46
#define NIFTI_ORNT_PLS     47

/*************************/
#endif /* _NIFTI_HEADER_ */
