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
      ** fields have been co-opted for other purposes.  Notably,      **
      ** most of the data_history substructure has been co-opted for  **
      ** other purposes, since the ANALYZE 7.5 format describes this  **
      ** substructure as "optional.                                   **
      **                                                              **
      ** Neither the National Institutes of Health (NIH), the DFWG,   **
      ** nor any of the members or employees of these institutions    **
      ** imply any warranty of usefulness of this material for any    **
      ** purpose, and do not assume any liability for damages,        **
      ** incidental or otherwise, caused by any use of this document. **
******************************************************************************/

#define NIFTI_VERSION 1
#define NIFTI_DATE    "01 Aug 2003"

/*---------------------------------------------------------------------------*/
/* Note that the ANALYZE 7.5 file header (dbh.h) is
         (c) Copyright 1986-1995
         Biomedical Imaging Resource
         Mayo Foundation

   Incorporation of components of dbh.h are by permission of the
   Mayo Foundation.

   !! Of course, I hope we do get this permission - RWCox !!

   The changes from the ANALYZE 7.5 file header in this file are released
   to the public domain.
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* In the comments below for each field, only NIFTI-1 specific
   requirements or changes from the ANALYZE 7.5 format are described.
   For convenience, the 348 byte header is described as a single
   struct, rather than as the ANALYZE 7.5 group of 3 substructs.

   To flag such a struct as being conformant to the NIFTI-1 spec,
   the last 8 bytes of the header must be the C String "nifti1" or
   "nifti1+"; in hexadecimal, the 8 bytes 6E 69 66 74 69 31 00 00 or
   6E 69 66 74 69 31 2B 00.  Normally, such a "magic number" or flag
   goes at the start of the file, but trying to avoid clobbering widely-
   used ANALYZE 7.5 fields led to putting this marker last.
   (however, recall that "the last shall be first").

   "nifti1" means that the image data is stored in the ".img" file
   corresponding to the header file.

   "nifti1+" means that the image data is stored in the same file as the
   header information.  In this case, the first byte of image data is
   stored at file offset (int)vox_offset into the header file.

   Further comments about the interpretation of various elements of this
   header are after the data type definition itself.  Fields that are
   marked as ++UNUSED++ have no particular interpretation in this standard.
-----------------------------------------------------------------------------*/

struct nifti_1_header { /* NIFTI-1 usage         */  /* ANALYZE 7.5 field(s) */
                        /*-----------------------*/  /*----------------------*/

                                               /*--- header_key substruct ---*/
  int   sizeof_hdr;     /* MUST be 348           */  /* int sizeof_hdr;      */
  char  data_type[10];  /* ++UNUSED++ */             /* char data_type[10];  */
  char  db_name[18];    /* ++UNUSED++ */             /* char db_name[18];    */
  int   extents;        /* ++UNUSED++ */             /* int extents;         */
  short session_error;  /* ++UNUSED++ */             /* short session_error; */
  char  regular;        /* ++UNUSED++ */             /* char regular;        */
  char  stat_dim;       /* Stat params in data?  */  /* char hkey_un0;       */

                                          /*--- image_dimension substruct ---*/
  short dim[8];         /* Data array dimensions.*/  /* short dim[8];        */
  float statpar_1 ;     /* Data values can be    */  /* short unused8;       */
                        /*  interpreted as from  */  /* short unused9;       */
  float statpar_2 ;     /*  a given statistical  */  /* short unused10;      */
                        /*  distribution using   */  /* short unused11;      */
  float statpar_3 ;     /*  stat_code and up to  */  /* short unused12;      */
                        /*  3 parameters.        */  /* short unused13;      */
  short stat_code ;     /* NIFTI_STAT_* code.    */  /* short unused14;      */
  short datatype;       /* Defines data type!    */  /* short datatype;      */
  short bitpix;         /* Number bits/voxel.    */  /* short bitpix;        */
  short byteorder ;     /* NIFTI_ORDER_* flag.   */  /* short dim_un0;       */
  float pixdim[8];      /* Grid spacings.        */  /* float pixdim[8];     */
  float vox_offset;     /* "nifti1+" data offset */  /* float vox_offset;    */
  float scl_slope ;     /* Data scaling:         */  /* float funused1;      */
  float scl_inter ;     /*  slope*val+inter.     */  /* float funused2;      */
  float funused3;       /* ++UNUSED++ */             /* float funused3;      */
  float cal_max;        /* ++UNUSED++ */             /* float cal_max;       */
  float cal_min;        /* ++UNUSED++ */             /* float cal_min;       */
  float compressed;     /* ++UNUSED++ */             /* float compressed;    */
  float verified;       /* ++UNUSED++ */             /* float verified;      */
  int   glmax;          /* ++UNUSED++ */             /* int glmax;           */
  int   glmin;          /* ++UNUSED++ */             /* int glmin;           */

                                             /*--- data_history substruct ---*/
  char  descrip[80];    /* ++UNUSED++ */             /* char descrip[80];    */
  char  aux_file[24];   /* ++UNUSED++ */             /* char aux_file[24];   */

  short qform_code ;    /* NIFTI_XFORM_* code.   */  /* all ANALYZE 7.5      */
  short sform_code ;    /* NIFTI_XFORM_* code.   */  /* below aux_file are   */
                                                     /* replaced             */
  float quatern_b ;     /* Orientation of the    */
  float quatern_c ;     /*  3D volume, given by  */
  float quatern_d ;     /*  a quaternion.        */
  float qoffset_x ;     /* Coords of center of   */
  float qoffset_y ;     /*  (0,0,0) voxel.       */
  float qoffset_z ;

  float srow1[4] ;      /* Rows of standardizing */
  float srow2[4] ;      /*  affine transform.    */
  float srow3[4] ;

  char bunused[12] ;    /* ++UNUSED++ at present */

  char magic[8] ;       /* MUST be "nifti1\0\0"  */
                        /*      or "nifti1+\0"   */

} ;                     /**** 348 bytes total ****/

/*---------------------------------------------------------------------------*/
/* DATA DIMENSIONALITY is as in ANALYZE 7.5:
     dim[0] = number of dimensions;
              if dim[0] is outside range 1..7, then the header information
              needs to be byte swapped

     dim[i] = length of dimension #i, for i=1..dim[0]  (must be positive)

     pixdim[i] = voxel width along dimension #i, i=1..dim[0] (positive)
                 (cf ORIENTATION section below)

   Number of bits per voxel value is in bitpix, which MUST correspond with
   the datatype field.
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* DATA STORAGE:
   If the magic field is "nifti1+", then the voxel data is stored in the
   same file as the header.  In this case, the voxel data starts at offset
   (int) vox_offset into the header file.  Thus, vox_offset=348.0 means that
   the data starts immediately after the NIFTI-1 header.

   If the magic field is "nifti1", then the voxel data is stored in the
   associated ".img" file, starting at offset 0 (i.e., vox_offset is not
   used in this case).

   The byteorder field indicates the voxel data storage byte order.  It should
   be one of the codes below.  Note that it is possible for the header data
   and the voxel data to be stored in different byte orders.

   LSB first and MSB first may not be adequate specifications for all systems.
   If needed, more codes will be added for more complex cases.
-----------------------------------------------------------------------------*/

#define NIFTI_ORDER_SAME      0   /* voxel data is ordered same as header */
#define NIFTI_ORDER_LSB_FIRST 1   /* voxel data is LSB first */
#define NIFTI_ORDER_MSB_FIRST 2   /* voxel data is MSB first */

/*---------------------------------------------------------------------------*/
/* DATA SCALING:
   If the scl_slope field is nonzero, then each voxel value in the dataset
   should be scaled as
      y = scl_slope * x + scl_inter
   where x = voxel value stored
         y = "true" voxel value
   Normally, we would expect this scaling to be used to store "true" floating
   values in a smaller integer datatype, but that is not required.  That is,
   it is legal to use scaling even if the datatype is a float type
   (legal, perhaps, but crazy).
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* TYPE OF DATA: Acceptable values for datatype field.
   Values smaller than 256 are ANALYZE 7.5 compatible.
   Larger values are NIFTI-1 additions.  They are all multiples of 256, so
   that no bits below position 8 are set in datatype.  But there is no need
   to use only powers-of-2, as the original ANALYZE 7.5 datatype codes do.
-----------------------------------------------------------------------------*/

                            /*--- the original ANALYZE 7.5 type codes ---*/
#define DT_NONE                    0
#define DT_UNKNOWN                 0     /* what it says, dude           */
#define DT_BINARY                  1     /* binary (1 bit/voxel)         */
#define DT_UNSIGNED_CHAR           2     /* unsigned char (8 bits/voxel) */
#define DT_SIGNED_SHORT            4     /* signed short (16 bits/voxel) */
#define DT_SIGNED_INT              8     /* signed int (32 bits/voxel)   */
#define DT_FLOAT                  16     /* float (32 bits/voxel)        */
#define DT_COMPLEX                32     /* complex (64 bits/voxel)      */
#define DT_DOUBLE                 64     /* double (64 bits/voxel)       */
#define DT_RGB                   128     /* RGB triple (24 bits/voxel)   */
#define DT_ALL                   255

                            /*---------- a reprise of the above codes ---*/
#define NIFTI_TYPE_UNKNOWN         0
#define NIFTI_TYPE_BINARY          1
#define NIFTI_TYPE_UNSIGNED_CHAR   2
#define NIFTI_TYPE_SIGNED_SHORT    4
#define NIFTI_TYPE_SIGNED_INT      8
#define NIFTI_TYPE_FLOAT          16
#define NIFTI_TYPE_COMPLEX        32
#define NIFTI_TYPE_DOUBLE         64
#define NIFTI_TYPE_RGB           128

                            /*-------------- yet another set of names ---*/
#define NIFTI_TYPE_UINT8           2
#define NIFTI_TYPE_INT16           4
#define NIFTI_TYPE_INT32           8
#define NIFTI_TYPE_FLOAT32        16
#define NIFTI_TYPE_COMPLEX64      32
#define NIFTI_TYPE_FLOAT64        64
#define NIFTI_TYPE_RGB24         128

                         /*---------------------- new codes for NIFTI ---*/
#define NIFTI_TYPE_INT8          256  /* signed char (8 bits)            */
#define NIFTI_TYPE_UINT16        512  /* unsigned short (16 bits)        */
#define NIFTI_TYPE_UINT32        768  /* unsigned int (32 bits)          */
#define NIFTI_TYPE_INT64        1024  /* long long (64 bits)             */
#define NIFTI_TYPE_UINT64       1280  /* unsigned long long (64 bits)    */
#define NIFTI_TYPE_FLOAT128     1536  /* long double (128 bits)          */
#define NIFTI_TYPE_COMPLEX128   1792  /* pair of doubles (128 bits)      */
#define NIFTI_TYPE_COMPLEX256   2048  /* pair of long doubles (256 bits) */

/*---------------------------------------------------------------------------*/
/* INTERPRETATION OF VOXEL DATA:
   The stat_code field can be used to indicate that the voxel data should
   be interpreted as being drawn from some given probablity distribution.

   Values 2..10 are compatible with AFNI 1.5x (which is why there is no
   code with value==1).

   Conventions for distributional parameters:
     If stat_dim == 0, then the distributional parameters are the same for
     each voxel, and are stored in the statpar_* fields.

     If stat_dim != 0, then the parameters are voxel dependent.  They are
     stored in the last dimension of the dataset (i.e., #dim[0]).
     For example, for a 3D dataset of t-statistics where the DOF parameter
     is spatially variable, we would have
       stat_dim  = 1 (say)
       stat_code = 3 (t-test)
       dim[0]    = 4
       dim[1]    = number of voxels along the x-axis
       dim[2]    = number of voxels along the y-axis
       dim[3]    = number of voxels along the z-axis
       dim[4]    = 2
     Then the first 3D volume stored in the voxel data file comprises the
     t-statistic at each location, and the second volume comprises the
     degrees-of-freedom at each location.  Although dim[0] = 4, this should
     be treated as a 3D dataset.  For such datasets, we would normally
     expect the datatype to be a floating point type.
-----------------------------------------------------------------------------*/

                                   /* Parameters             */
                                   /*------------------------*/
#define NIFTI_STAT_NONE        0   /* not applicable         */
#define NIFTI_STAT_CORREL      2   /* Samples, fits, orts    */
#define NIFTI_STAT_TTEST       3   /* DOF                    */
#define NIFTI_STAT_FTEST       4   /* DOF numerator, denom   */
#define NIFTI_STAT_ZSCORE      5   /* no params              */
#define NIFTI_STAT_CHISQ       6   /* DOF                    */
#define NIFTI_STAT_BETA        7   /* a and b params         */
#define NIFTI_STAT_BINOM       8   /* # trials, p per trial  */
#define NIFTI_STAT_GAMMA       9   /* shape, scale params    */
#define NIFTI_STAT_POISSON    10   /* mean                   */

#define NIFTI_STAT_NORMAL     11   /* mean, variance         */
#define NIFTI_STAT_FTEST_NONC 12   /* 2 DOF, noncentrality   */
#define NIFTI_STAT_CHISQ_NONC 13   /* DOF, noncentrality     */
#define NIFTI_STAT_LOGISTIC   14   /* location, scale        */
#define NIFTI_STAT_LAPLACE    15   /* location, scale        */
#define NIFTI_STAT_UNIFORM    16   /* start, end             */
#define NIFTI_STAT_TTEST_NONC 17   /* DOF, noncentrality     */
#define NIFTI_STAT_WEIBULL    18   /* location, scale, power */
#define NIFTI_STAT_CHI        19   /* DOF                    */
#define NIFTI_STAT_INVGAUSS   20   /* mu, lambda             */
#define NIFTI_STAT_EXTVAL     21   /* location, scale        */
#define NIFTI_STAT_PVAL       22   /* no params              */

#define NIFTI_STAT_INTERVAL   23   /* width (about center)   */

                            /*--- these aren't statistics ---*/
                            /*--- and have no statistical ---*/
                            /*--- parameters attached     ---*/

#define NIFTI_STAT_ESTIMATE  1001  /* estimate of some param */

#define NIFTI_STAT_ANATNAME  1002  /* code for anat label    */

/*---------------------------------------------------------------------------*/
/* 3D IMAGE (VOLUME) ORIENTATION AND LOCATION IN SPACE:

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
   /*-----------------------*/   /*--------------------------------------*/
#define NIFTI_CORD_ARBITRARY  0  /* Arbitrary (scanner-based?)           */
#define NIFTI_CORD_TALAIRACH  1  /* Talairach-Tournoux Atlas; (0,0,0)=AC */
#define NIFTI_CORD_MNI_152    2  /* MNI 152 normalized coords            */

/*************************/
#endif /* _NIFTI_HEADER_ */
