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
      ** substructure as "not required".                              **
      **                                                              **
      ** Neither the National Institutes of Health (NIH), the DFWG,   **
      ** nor any of the members or employees of these institutions    **
      ** imply any warranty of usefulness of this material for any    **
      ** purpose, and do not assume any liability for damages,        **
      ** incidental or otherwise, caused by any use of this document. **
      ** If these conditions are not acceptable, do not use this!     **
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

   !!!!!!! Of course, I hope we do get this permission - RWCox !!!!!!!
   !!!!!!! Otherwise, I'll have to modifiy the comments        !!!!!!!

   The changes from the ANALYZE 7.5 file header in this file are released
   to the public domain.
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* In the comments below for each field, only NIFTI-1 specific
   requirements or changes from the ANALYZE 7.5 format are described.
   For convenience, the 348 byte header is described as a single
   struct, rather than as the ANALYZE 7.5 group of 3 substructs.

   NIFTI-1 FLAG (MAGIC NUMBERS):
   To flag such a struct as being conformant to the NIFTI-1 spec,
   the last 4 bytes of the header must be either the C String "ni1" or
   "n1+"; in hexadecimal, the 4 bytes
     6E 69 31 00   or   6E 31 2B 00
   (in any future version of this format, the '1' will be upgraded
   to '2', etc.).  Normally, such a "magic number" or flag goes at the
   start of the file, but trying to avoid clobbering widely-used
   ANALYZE 7.5 fields led to putting this marker last.  However, recall
   that "the last shall be first" (Matthew 20:16).

   "ni1" means that the image data is stored in the ".img" file
   corresponding to the header file (starting at file offset 0).

   "n1+" means that the image data is stored in the same file as the
   header information.  In this case, the first byte of image data is
   stored at file offset (int)vox_offset into the file.  In this case,
   we recommend that the combined header+data filename suffix be ".nii".

   Further comments about the interpretation of various elements of this
   header are after the data type definition itself.  Fields that are
   marked as ++UNUSED++ have no particular interpretation in this standard.
   (Also see the UNUSED FIELDS comment section, far below.)

   The presumption below is that the various C types have particular sizes:
     sizeof(int) == sizeof(float) == 4 ;  sizeof(short) == 2
-----------------------------------------------------------------------------*/

                        /*-----------------------*/  /*----------------------*/
struct nifti_1_header { /* NIFTI-1 usage         */  /* ANALYZE 7.5 field(s) */
                        /*-----------------------*/  /*----------------------*/

                                           /*--- was header_key substruct ---*/
  int   sizeof_hdr;     /* MUST be 348           */  /* int sizeof_hdr;      */
  char  data_type[10];  /* ++UNUSED++            */  /* char data_type[10];  */
  char  db_name[18];    /* ++UNUSED++            */  /* char db_name[18];    */
  int   extents;        /* ++UNUSED++            */  /* int extents;         */
  short session_error;  /* ++UNUSED++            */  /* short session_error; */
  char  regular;        /* ++UNUSED++            */  /* char regular;        */
  char  stat_dim;       /* Stat params in data?  */  /* char hkey_un0;       */

                                      /*--- was image_dimension substruct ---*/
  short dim[8];         /* Data array dimensions.*/  /* short dim[8];        */
  float statpar_1 ;     /* Data values can be    */  /* short unused8;       */
                        /*  interpreted as from  */  /* short unused9;       */
  float statpar_2 ;     /*  a given statistical  */  /* short unused10;      */
                        /*  distribution using   */  /* short unused11;      */
  float statpar_3 ;     /*  intent_code and up   */  /* short unused12;      */
                        /*  to 3 parameters.     */  /* short unused13;      */
  short intent_code ;   /* NIFTI_INTENT_* code.  */  /* short unused14;      */
  short datatype;       /* Defines data type!    */  /* short datatype;      */
  short bitpix;         /* Number bits/voxel.    */  /* short bitpix;        */
  short byteorder ;     /* NIFTI_ORDER_* flag.   */  /* short dim_un0;       */
  float pixdim[8];      /* Grid spacings.        */  /* float pixdim[8];     */
  float vox_offset;     /* "n1+" data offset     */  /* float vox_offset;    */
  float scl_slope ;     /* Data scaling:         */  /* float funused1;      */
  float scl_inter ;     /*  slope*val+inter.     */  /* float funused2;      */
  float funused3;       /* ++UNUSED++            */  /* float funused3;      */
  float cal_max;        /* max display intensity */  /* float cal_max;       */
  float cal_min;        /* min display intensity */  /* float cal_min;       */
  float compressed;     /* ++UNUSED++            */  /* float compressed;    */
  float verified;       /* ++UNUSED++            */  /* float verified;      */
  int   glmax;          /* ++UNUSED++            */  /* int glmax;           */
  int   glmin;          /* ++UNUSED++            */  /* int glmin;           */

                                         /*--- was data_history substruct ---*/
  char  descrip[80];    /* any text you like.    */  /* char descrip[80];    */
  char  aux_file[24];   /* auxiliary filename.   */  /* char aux_file[24];   */

  short qform_code ;    /* NIFTI_XFORM_* code.   */  /*-- all ANALYZE 7.5 ---*/
  short sform_code ;    /* NIFTI_XFORM_* code.   */  /*   fields below here  */
                                                     /*   are replaced       */
  float quatern_b ;     /* Orientation of the    */
  float quatern_c ;     /*  3D volume, given by  */
  float quatern_d ;     /*  a quaternion.        */
  float qoffset_x ;     /* Coords of center of   */
  float qoffset_y ;     /*  (0,0,0) voxel.       */
  float qoffset_z ;     /* (cf. qform_code)      */

  float srow_x[4] ;     /* Rows of standardizing */
  float srow_y[4] ;     /*  affine transform     */
  float srow_z[4] ;     /*  (cf. sform_code).    */

  char idcode[16] ;     /* 128 bit GUID          */

  char magic[4] ;       /* MUST be "ni1\0"       */
                        /*      or "n1+\n"       */

} ;                     /**** 348 bytes total ****/

/*---------------------------------------------------------------------------*/
/* DATA DIMENSIONALITY (as in ANALYZE 7.5):
     dim[0] = number of dimensions;
              if dim[0] is outside range 1..7, then the header information
              needs to be byte swapped

     dim[i] = length of dimension #i, for i=1..dim[0]  (must be positive)

     pixdim[i] = voxel width along dimension #i, i=1..dim[0] (positive)
                 (cf. ORIENTATION section below for use of pixdim[0])

   Number of bits per voxel value is in bitpix, which MUST correspond with
   the datatype field.
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* DATA STORAGE:
   If the magic field is "n1+", then the voxel data is stored in the
   same file as the header.  In this case, the voxel data starts at offset
   (int)vox_offset into the header file.  Thus, vox_offset=348.0 means that
   the data starts immediately after the NIFTI-1 header.

   If the magic field is "ni1", then the voxel data is stored in the
   associated ".img" file, starting at offset 0 (i.e., vox_offset is not
   used in this case, and should be set to 0.0).

   The byteorder field indicates the voxel data storage byte order.  It should
   be one of the codes below.  Note that it is possible for the header data
   and the voxel data to be stored in different byte orders.

   When storing NIFTI-1 datasets in pairs of files, it is customary to name
   the files in the pattern "name.hdr" and "name.img", as in ANALYZE 7.5.
   When storing in a single file ("n1+"), the file name should be in
   the form "name.nii" (the ".nft" and ".nif" suffixes are already taken;
   cf. http://www.icdatamaster.com/n.html).

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
   (crazy, perhaps, but legal).
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* TYPE OF DATA (acceptable values for datatype field):
   Values smaller than 256 are ANALYZE 7.5 compatible.
   Larger values are NIFTI-1 additions.  They are all multiples of 256, so
   that no bits below position 8 are set in datatype.  But there is no need
   to use only powers-of-2, as the original ANALYZE 7.5 datatype codes do.

   The additional codes are intended to include a complete list of basic
   scalar types, including signed and unsigned integers from 8 to 64 bits,
   floats from 32 to 128 bits, and complex (float pairs) from 64 to 256 bits.

   Note that many programs will support only a few of these datatypes!
   A NIFTI-1 program should fail gracefully (e.g., print a warning message)
   when it encounters a dataset with a type it doesn't like.
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
#define DT_ALL                   255     /* not very useful (?)          */

                            /*----- another set of names for the same ---*/
#define DT_UINT8                   2
#define DT_INT16                   4
#define DT_INT32                   8
#define DT_FLOAT32                16
#define DT_COMPLEX64              32
#define DT_FLOAT64                64
#define DT_RGB24                 128

                            /*------------------- new codes for NIFTI ---*/
#define DT_INT8                  256     /* signed char (8 bits)         */
#define DT_UINT16                512     /* unsigned short (16 bits)     */
#define DT_UINT32                768     /* unsigned int (32 bits)       */
#define DT_INT64                1024     /* long long (64 bits)          */
#define DT_UINT64               1280     /* unsigned long long (64 bits) */
#define DT_FLOAT128             1536     /* long double (128 bits)       */
#define DT_COMPLEX128           1792     /* double pair (128 bits)       */
#define DT_COMPLEX256           2048     /* long double pair (256 bits)  */

                            /*------- aliases for all the above codes ---*/
#define NIFTI_TYPE_UINT8           2
#define NIFTI_TYPE_INT16           4
#define NIFTI_TYPE_INT32           8
#define NIFTI_TYPE_FLOAT32        16
#define NIFTI_TYPE_COMPLEX64      32
#define NIFTI_TYPE_FLOAT64        64
#define NIFTI_TYPE_RGB24         128
#define NIFTI_TYPE_INT8          256
#define NIFTI_TYPE_UINT16        512
#define NIFTI_TYPE_UINT32        768
#define NIFTI_TYPE_INT64        1024
#define NIFTI_TYPE_UINT64       1280
#define NIFTI_TYPE_FLOAT128     1536
#define NIFTI_TYPE_COMPLEX128   1792
#define NIFTI_TYPE_COMPLEX256   2048

                         /*-------- sample typedefs for complex types ---*/
#if 0
typedef struct { float       r,i; } complex_float ;
typedef struct { double      r,i; } complex_double ;
typedef struct { long double r,i; } complex_longdouble ;
#endif

/*---------------------------------------------------------------------------*/
/* INTERPRETATION OF VOXEL DATA:
   The intent_code field can be used to indicate that the voxel data has
   some particular meaning.  In particular, a large number of codes is
   given to indicate that the the voxel data should be interpreted as
   being drawn from a given probablity distribution.

   intent_code values 2..10 are compatible with AFNI 1.5x (which is why
   there is no code with value==1, which is obsolescent in AFNI).

   Conventions for statistical distributional parameters:
     If stat_dim == 0, then the distributional parameters are the same
     for each voxel, and are stored in the statpar_* fields.

     If stat_dim != 0, then the parameters are voxel dependent.  They are
     stored in the last dimension of the dataset (i.e., #dim[0]).
     For example, for a 3D dataset of t-statistics where the DOF parameter
     is spatially variable, we would have
       stat_dim    = 1 (say)
       intent_code = 3 (t-test)
       dim[0]      = 4
       dim[1]      = number of voxels along the x-axis
       dim[2]      = number of voxels along the y-axis
       dim[3]      = number of voxels along the z-axis
       dim[4]      = 2
     Then the first 3D volume stored in the voxel data file comprises the
     t-statistic at each location, and the second volume comprises the
     degrees-of-freedom at each location.  Although dim[0] = 4, this should
     be treated as a 3D dataset.  For such datasets, we would normally
     expect the datatype to be a floating point type.
-----------------------------------------------------------------------------*/

#define NIFTI_INTENT_NONE        0   /* the default             */

         /*--- these codes are for probability distributions ---*/
                                     /*-------------------------*/
                                     /* Distribution Parameters */
                                     /*-------------------------*/
#define NIFTI_INTENT_CORREL      2   /* Samples, fits, orts     */
#define NIFTI_INTENT_TTEST       3   /* DOF                     */
#define NIFTI_INTENT_FTEST       4   /* DOF numerator, denom    */
#define NIFTI_INTENT_ZSCORE      5   /* NO PARAMS               */
#define NIFTI_INTENT_CHISQ       6   /* DOF                     */
#define NIFTI_INTENT_BETA        7   /* a and b params          */
#define NIFTI_INTENT_BINOM       8   /* # trials, p per trial   */
#define NIFTI_INTENT_GAMMA       9   /* shape, scale params     */
#define NIFTI_INTENT_POISSON    10   /* mean                    */

#define NIFTI_INTENT_NORMAL     11   /* mean, variance          */
#define NIFTI_INTENT_FTEST_NONC 12   /* 2 DOF, noncentrality    */
#define NIFTI_INTENT_CHISQ_NONC 13   /* DOF, noncentrality      */
#define NIFTI_INTENT_LOGISTIC   14   /* location, scale         */
#define NIFTI_INTENT_LAPLACE    15   /* location, scale         */
#define NIFTI_INTENT_UNIFORM    16   /* start, end              */
#define NIFTI_INTENT_TTEST_NONC 17   /* DOF, noncentrality      */
#define NIFTI_INTENT_WEIBULL    18   /* location, scale, power  */
#define NIFTI_INTENT_CHI        19   /* DOF                     */
#define NIFTI_INTENT_INVGAUSS   20   /* mu, lambda              */
#define NIFTI_INTENT_EXTVAL     21   /* location, scale         */

#define NIFTI_INTENT_PVAL       22   /* NO PARAMS               */

#define NIFTI_FIRST_STATCODE     2
#define NIFTI_LAST_STATCODE     22

                               /*--- these aren't statistics ---*/
                               /*--- and have no statistical ---*/
                               /*--- parameters attached     ---*/

#define NIFTI_INTENT_ESTIMATE 1001   /* =estimate of some param */

#define NIFTI_INTENT_LABEL    1002   /* =index for some label   */

/*---------------------------------------------------------------------------*/
/* 3D IMAGE (VOLUME) ORIENTATION AND LOCATION IN SPACE:

   There are 3 different methods by which continuous coordinates can
   attached to voxels.  The discussion below emphasizes 3D volumes, and
   the continuous coordinates are referred to as (x,y,z).  The voxel
   index coordinates (i.e., the array indexes) are referred to as (i,j,k),
   with valid ranges:
     i = 0 .. dim[1]-1
     j = 0 .. dim[2]-1  (if dim[0] >= 2)
     k = 0 .. dim[3]-1  (if dim[0] >= 3)
   The (x,y,z) coordinates refer to the CENTER of a voxel.  In methods
   2 and 3, the (x,y,z) axes refer to a subject-based coordinate system,
   with
     +x = Right  +y = Anterior  +z = Superior.
   This is a right-handed coordinate system.  However, the exact
   direction these axes point with respect to the subject depends
   on qform_code (Method 2) and sform_code (Method 3).

   N.B.: The i index varies most rapidly, j index next, k index slowest.
   Thus, voxel (i,j,k) is stored starting at location
     (i + j*dim[1] + k*dim[1]*dim[2]) * (bitpix/8)
   into the dataset array.

   N.B.: The ANALYZE 7.5 coordinate system is
     +x = Left  +y = Anterior  +z = Superior
   which is a left-handed coordinate system.  This backwardness is
   too difficult to tolerate, so this NIFTI-1 standard specifies the
   coordinate order which is most common in functional neuroimaging.

   N.B.: The 3 methods below all give the locations of the voxel centers
   in the (x,y,z) coordinate system.  In many cases, programs will wish
   to display image data on some other grid.  In such a case, the program
   will need to convert its desired (x,y,z) values into (i,j,k) values
   in order to extract (or interpolate) the image data.  This operation
   would be done with the inverse transformation to those described below.

 * METHOD 1 (the "old" way, used only when qform_code == 0):
   The coordinate mapping from (i,j,k) to (x,y,z) is the ANALYZE
   7.5 way.  This is a simple scaling relationship:

     x = pixdim[1] * i
     y = pixdim[2] * j
     z = pixdim[3] * k

   No particular spatial orientation is attached to these (x,y,z)
   coordinates.  (NIFTI-1 does not have the ANALYZE 7.5 orient field,
   which is not general and is often not set properly.)

 * METHOD 2 (used when qform_code > 0, which should be the "normal case):
   The (x,y,z) coordinates are given by the pixdim[] scales, a rotation
   matrix, and a shift.  This method is intended to represent
   "scanner-anatomical" coordinates, which are often embedded in the
   image header, and represent the nominal orientation and location of
   the data.

     [ x ]   [ R11 R12 R13 ] [ pfac * pixdim[1] * i ]   [ qoffset_x ]
     [ y ] = [ R21 R22 R23 ] [        pixdim[2] * j ] + [ qoffset_y ]
     [ z ]   [ R31 R32 R33 ] [        pixdim[3] * k ]   [ qoffset_z ]

   The qoffset_* shifts are in the NIFTI-1 header.  The rotation matrix
   R is calculated from the quatern_* parameters, as described below.

   The scaling factor pface is either 1 or -1.  The rotation matrix R
   defined by the quaternion parameters is "proper" (has determinant 1).
   This may not fit the needs of the data; for example, if the image
   grid is
     i increases from Left-to-Right
     j increases form Anterior-to-Posterior
     k increases from Inferior-to-Superior
   Then (i,j,k) is a left-handed triple.  In this example, if pfac=1,
   the R matrix would have to be

     [  1   0   0 ]
     [  0  -1   0 ]  which is "improper" (determinant = 1).
     [  0   0   1 ]

   If we set pfac=-1, then the R matrix would be

     [ -1   0   0 ]
     [  0  -1   0 ]  which is proper.
     [  0   0   1 ]

   This R matrix is represented by quaternion [a,b,c,d] = [0,0,0,1]

   N.B.: We store pfac in the otherwise unused pixdim[0].

 * METHOD 3 (used when sform_code > 0):
   The (x,y,z) coordinates are given by a general affine transformation
   of the (i,j,k) indexes:

     x = srow_x[0] * i + srow_x[1] * j + srow_x[2] * j + srow_x[3]
     y = srow_y[0] * i + srow_y[1] * j + srow_y[2] * j + srow_y[3]
     z = srow_z[0] * i + srow_z[1] * j + srow_z[2] * j + srow_z[3]

   The srow_* vectors are in the NIFTI_1 header.  Note that no use
   is made of pixdim[].

 * WHY 3 METHODS?
   Method 1 is provided only for backwards compatibility.  The intention
   is that Method 2 (qform_code > 0) represents the nominal voxel locations
   as reported by the scanner.  Method 3, if present (sform_code > 0), is to
   be used to give the location of the voxels in some standard space.
   The sform_code indicates which standard space is present.

   In this scheme, a dataset would originally be set up so that the
   Method 2 coordinates represent what the scanner reported.  Later,
   a registration to some standard space can be computed and inserted
   in the header.  Image display software can use either transform,
   depending on its purposes and needs.

   In Method 2, the origin of coordinates would generally be whatever
   the scanner origin is; for example, in MRI, (0,0,0) is the center
   of the gradient coil.

   In Method 3, the origin of coordinates would depend on the value
   of sform_code; for example, for the Talairach coordinate system,
   (0,0,0) corresponds to the Anterior Commissure.

 * QUATERNION REPRESENATION OF ROTATION MATRIX (METHOD 2)
   The orientation of the (x,y,z) axes relative to the (i,j,k) axes
   in 3D space is specified using a unit quaternion [a,b,c,d], where
   a*a+b*b+c*c+d*d=1.  The (b,c,d) values are all that is needed, since
   we require that a = sqrt(1.0-b*b+c*c+d*d) be nonnegative.  The (b,c,d)
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

   Requiring a >= 0 is equivalent to requiring -Pi <= h <= Pi.  To rotate a
   3-vector (x,y,z) using quaternions, we compute the quaternion product

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

   The above formula shows how to go from quaternion (b,c,d) to
   rotation matrix and direction cosines.  Conversely, given R,
   we can compute the fields for the NIFTI-1 header by

     a = 0.5  * sqrt(1+R11+R22+R33)     (not stored)
     b = 0.25 * (R32-R23) / a       ==> quatern_b
     c = 0.25 * (R13-R31) / a       ==> quatern_c
     d = 0.25 * (R21-R12) / a       ==> quatern_d

   [If a==0 (a 180 degree rotation), alternative formulas are needed.]

   Note that R-transpose (== R-inverse) would lead to the quaternion
   [a,-b,-c,-d].

   The choice to specify the qoffset_x (etc.) values in the final
   coordinate system is partly to make it easy to convert DICOM images to
   this format.  The DICOM attribute "Image Position (Patient)" (0020,0032)
   stores the (Xd,Yd,Zd) coordinates of the center of the first voxel.
   Here, (Xd,Yd,Zd) refer to DICOM coordinates, and Xd=-x, Yd=-y, Zd=z
   (i.e., DICOM +Xd is Right, +Yd is Posterior, +Zd is Superior).
-----------------------------------------------------------------------------*/

   /* [qs]form_code value:  */      /* x,y,z coordinate system refers to:    */
   /*-----------------------*/      /*---------------------------------------*/
#define NIFTI_XFORM_UNKNOWN      0  /* Arbitrary (Method 1 coordinates)      */
#define NIFTI_XFORM_SCANNER_ANAT 1  /* Scanner-anatomical orientation        */
#define NIFTI_XFORM_ALIGNED_ANAT 2  /* Coordinates aligned to another file's */
#define NIFTI_XFORM_TALAIRACH    3  /* Talairach-Tournoux Atlas; (0,0,0)=AC  */
#define NIFTI_XFORM_MNI_152      4  /* MNI 152 normalized coords             */

/*---------------------------------------------------------------------------*/
/* 128 BIT GLOBALLY UNIQUE IDENTIFIER (GUID):
   The idcode field is intended to contain a 128 bit GUID that will be
   different for each NIFTI-1 file ever created.  This is intended to
   help with building databases and indexes of imaging data.
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* UNUSED FIELDS:
   Some of the ANALYZE 7.5 fields marked as ++UNUSED++ may need to be set
   to particular values for compatibility with other programs.  The issue
   of interoperability of ANALYZE 7.5 files is a murky one -- not all
   programs require exactly the same set of fields.  (Unobscuring this
   murkiness is a principal motivation behind NIFTI-1.)

   Some of the fields that may need to be set for other (non-NIFTI aware)
   software to be happy are:

     extents    dbh.h says this should be 16384
     regular    dbh.h says this should be the character 'r'
     glmin,   } dbh.h says these values should be the min and max voxel
      glmax   }  values for the entire dataset
     cal_min, } If nonzero, these values might be used as a display
      cal_max }  range for the voxel data

   It is probably best to initialize ALL fields in the NIFTI-1 header to 0
   (e.g., with calloc()), then fill in what is needed.
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* MISCELLANEOUS C MACROS
-----------------------------------------------------------------------------*/

/*.................*/
/*! Given a nifti_1_header struct, check if it has a good magic number.
    Returns 1 if magic is good, 0 if it is not.                         */

#define NIFTI_GOOD_MAGIC(h)                              \
   ( (h).magic[0]=='n' && (h).magic[3]=='\0' &&          \
     (( (h).magic[1]=='1' && (h).magic[2]=='+' ) ||      \
      ( (h).magic[1]=='i' && (h).magic[2]=='1' )   ))

/*.................*/
/*! Check if a nifti_1_header struct says if the data is stored in the
    same file or in a separate file.  Returns 1 if the data is in the same
    file as the header, 0 if it is not.                                   */

#define NIFTI_ONEFILE(h) ( (h).magic[2] == '+' && (h).vox_offset >= 348.0 )

/*.................*/
/*! Check if a nifti_1_header struct needs to be byte swapped.
    Returns 1 if it needs to be swapped, 0 if it does not.     */

#define NIFTI_NEEDS_SWAP(h) ( (h).dim[0] < 0 || (h).dim[0] > 7 )

/*****************************************************************************/
#endif /* _NIFTI_HEADER_ */
