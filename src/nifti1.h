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
      ** ANALYZE (TM) 7.5 file format.  Most of the unused fields     **
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
   to the public domain, including the comments.
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/*! INTRODUCTION TO NIFTI-1:
   ------------------------
   In the comments below for each field, only NIFTI-1 specific
   requirements or changes from the ANALYZE 7.5 format are described.
   For convenience, the 348 byte header is described as a single
   struct, rather than as the ANALYZE 7.5 group of 3 substructs.

   Further comments about the interpretation of various elements of this
   header are after the data type definition itself.  Fields that are
   marked as ++UNUSED++ have no particular interpretation in this standard.
   (Also see the UNUSED FIELDS comment section, far below.)

   The presumption below is that the various C types have particular sizes:
     sizeof(int) == sizeof(float) == 4 ;  sizeof(short) == 2

   NIFTI-1 FLAG (MAGIC NUMBERS):
   ----------------------------
   To flag such a struct as being conformant to the NIFTI-1 spec,
   the last 4 bytes of the header must be either the C String "ni1" or
   "n1+"; in hexadecimal, the 4 bytes
     6E 69 31 00   or   6E 31 2B 00
   (in any future version of this format, the '1' will be upgraded
   to '2', etc.).  Normally, such a "magic number" or flag goes at the
   start of the file, but trying to avoid clobbering widely-used
   ANALYZE 7.5 fields led to putting this marker last.  However, recall
   that "the last shall be first" (Matthew 20:16).

   NIFTI-1 FILE STORAGE:
   --------------------
   "ni1" means that the image data is stored in the ".img" file
   corresponding to the header file (starting at file offset 0).

   "n1+" means that the image data is stored in the same file as the
   header information.  We recommend that the combined header+data
   filename suffix be ".nii".  When the dataset is stored in one file,
   the first byte of image data is stored at file offset (int)vox_offset.
-----------------------------------------------------------------------------*/

                        /*-----------------------*/  /*----------------------*/
struct nifti_1_header { /* NIFTI-1 usage         */  /* ANALYZE 7.5 field(s) */
                        /*-----------------------*/  /*----------------------*/

                                           /*--- was header_key substruct ---*/
 int   sizeof_hdr;    /*!< MUST be 348           */  /* int sizeof_hdr;      */
 char  data_type[10]; /*!< ++UNUSED++            */  /* char data_type[10];  */
 char  db_name[18];   /*!< ++UNUSED++            */  /* char db_name[18];    */
 int   extents;       /*!< ++UNUSED++            */  /* int extents;         */
 short session_error; /*!< ++UNUSED++            */  /* short session_error; */
 char  regular;       /*!< ++UNUSED++            */  /* char regular;        */
 char  intent_vector; /*!< Last dim=vector data? */  /* char hkey_un0;       */

                                      /*--- was image_dimension substruct ---*/
 short dim[8];        /*!< Data array dimensions.*/  /* short dim[8];        */
 float intent_p1 ;    /*!< 1st intent parameter. */  /* short unused8;       */
                                                     /* short unused9;       */
 float intent_p2 ;    /*!< 2nd intent parameter. */  /* short unused10;      */
                                                     /* short unused11;      */
 float intent_p3 ;    /*!< 3rd intent parameter. */  /* short unused12;      */
                                                     /* short unused13;      */
 short intent_code ;  /*!< NIFTI_INTENT_* code.  */  /* short unused14;      */
 short datatype;      /*!< Defines data type!    */  /* short datatype;      */
 short bitpix;        /*!< Number bits/voxel.    */  /* short bitpix;        */
 short byteorder ;    /*!< NIFTI_ORDER_* flag.   */  /* short dim_un0;       */
 float pixdim[8];     /*!< Grid spacings.        */  /* float pixdim[8];     */
 float vox_offset;    /*!< "n1+" data offset     */  /* float vox_offset;    */
 float scl_slope ;    /*!< Data scaling: slope.  */  /* float funused1;      */
 float scl_inter ;    /*!< Data scaling: offset. */  /* float funused2;      */
 short num_head ;     /*!< Number of headers.    */  /* float funused3;      */
 short sunused1 ;     /*!< ++UNUSED++            */
 float funused3;      /*!< ++UNUSED++            */  /* float funused3;      */
 float cal_max;       /*!< max display intensity */  /* float cal_max;       */
 float cal_min;       /*!< min display intensity */  /* float cal_min;       */
 float compressed;    /*!< ++UNUSED++            */  /* float compressed;    */
 float verified;      /*!< ++UNUSED++            */  /* float verified;      */
 int   glmax;         /*!< ++UNUSED++            */  /* int glmax;           */
 int   glmin;         /*!< ++UNUSED++            */  /* int glmin;           */

                                         /*--- was data_history substruct ---*/
 char  descrip[80];   /*!< any text you like.    */  /* char descrip[80];    */
 char  aux_file[24];  /*!< auxiliary filename.   */  /* char aux_file[24];   */

 short qform_code ;   /*!< NIFTI_XFORM_* code.   */  /*-- all ANALYZE 7.5 ---*/
 short sform_code ;   /*!< NIFTI_XFORM_* code.   */  /*   fields below here  */
                                                     /*   are replaced       */
 float quatern_b ;    /*!< Quaternion b param.   */
 float quatern_c ;    /*!< Quaternion c param.   */
 float quatern_d ;    /*!< Quaternion d param.   */
 float qoffset_x ;    /*!< Quaternion x shift.   */
 float qoffset_y ;    /*!< Quaternion y shift.   */
 float qoffset_z ;    /*!< Quaternion z shift.   */

 float srow_x[4] ;    /*!< 1st row affine transform.   */
 float srow_y[4] ;    /*!< 2nd row affine transform.   */
 float srow_z[4] ;    /*!< 3rd row affine transform.   */

 char intent_name[16];/*!< 'name' or meaning of data.  */

 char magic[4] ;      /*!< MUST be "ni1\0" or "n1+\0". */

} ;                   /**** 348 bytes total ****/

/*---------------------------------------------------------------------------*/
/* DATA DIMENSIONALITY (as in ANALYZE 7.5):
   ---------------------------------------
     dim[0] = number of dimensions;
              if dim[0] is outside range 1..7, then the header information
              needs to be byte swapped

     dim[i] = length of dimension #i, for i=1..dim[0]  (must be positive)
              - also see the discussion of intent_code, far below

     pixdim[i] = voxel width along dimension #i, i=1..dim[0] (positive)
                 (cf. ORIENTATION section below for use of pixdim[0])

   Number of bits per voxel value is in bitpix, which MUST correspond with
   the datatype field.  The total number of bytes in the image data is
     dim[1] * ... * dim[dim[0]] * bitpix / 8
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* MULTIPLE HEADERS:
   ----------------
   If the num_head field is > 1, this means that more than one 348 byte header
   is stored in the same file.  This is only allowed if the magic field is
   "n1+", indicating that the voxel data is also stored in the same file.
   The headers are to be stored sequentially -- that is, the i-th header
   is at byte offset (i-1)*348 into the file, for i=1..num_head.  Each
   header will contain (in its vox_offset field) the location of its
   corresponding image data in the same file.

   Multiple sub-datasets per file can be used for various applications:
    - Storing 1 int (sub-dataset #1) and 3 floats (sub-dataset #2) at
      each voxel.
    - Storing (x,y,z) node coordinates at each node in sub-dataset #1 and
      (i,j,k) triangle facet indexes in sub-dataset #2 -- these two
      sub-datasets together define a surface.  Further sub-datasets could
      contain data values at each node or triangle in the surface (e.g.,
      curvature, some activation statistic).
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* DATA STORAGE:
   ------------
   If the magic field is "n1+", then the voxel data is stored in the
   same file as the header.  In this case, the voxel data starts at offset
   (int)vox_offset into the header file.  Thus, vox_offset==348.0 means that
   the data starts immediately after the NIFTI-1 header.  If vox_offset is
   greater than 348, the NIFTI-1 format does not say anything about the
   contents of the dataset file between the end of the header and the
   start of the data.

   FILES:
   -----
   If the magic field is "ni1", then the voxel data is stored in the
   associated ".img" file, starting at offset 0 (i.e., vox_offset is not
   used in this case, and should be set to 0.0).

   When storing NIFTI-1 datasets in pairs of files, it is customary to name
   the files in the pattern "name.hdr" and "name.img", as in ANALYZE 7.5.
   When storing in a single file ("n1+"), the file name should be in
   the form "name.nii" (the ".nft" and ".nif" suffixes are already taken;
   cf. http://www.icdatamaster.com/n.html ).

   BYTE ORDERING:
   -------------
   The byteorder field indicates the voxel data storage byte order.  It should
   be one of the codes below.  Note that it is possible for the header data
   and the voxel data to be stored in different byte orders.  (The header
   byte order is determined by checking dim[0], as described earlier.)

   LSB first and MSB first may not be adequate specifications for all systems.
   If needed, more codes will be added for more complex cases.

   Floating point types are presumed to be stored in IEEE-754 format.
-----------------------------------------------------------------------------*/

 /*! Voxel data is ordered same as header */

#define NIFTI_ORDER_SAME      0

 /*! Voxel data is LSB first */

#define NIFTI_ORDER_LSB_FIRST 1

 /*! Voxel data is MSB first */

#define NIFTI_ORDER_MSB_FIRST 2

/*---------------------------------------------------------------------------*/
/* DATA SCALING:
   ------------
   If the scl_slope field is nonzero, then each voxel value in the dataset
   should be scaled as
      y = scl_slope * x + scl_inter
   where x = voxel value stored
         y = "true" voxel value
   Normally, we would expect this scaling to be used to store "true" floating
   values in a smaller integer datatype, but that is not required.  That is,
   it is legal to use scaling even if the datatype is a float type
   (crazy, perhaps, but legal).

   The cal_min and cal_max fields (if nonzero) are used for mapping (possibly
   scaled) dataset values to display colors:
    - Minimum display intensity (black) corresponds to dataset value cal_min.
    - Maximum display intensity (white) corresponds to dataset value cal_max.
    - Dataset values below cal_min should display as black also, and values
      above cal_max as white.
    - Colors "black" and "white", of course, may refer to any scalar display
      scheme (e.g., a color lookup table specified via aux_file).
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* TYPE OF DATA (acceptable values for datatype field):
   ---------------------------------------------------
   Values of datatype smaller than 256 are ANALYZE 7.5 compatible.
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

                                       /*! unsigned char. */
#define NIFTI_TYPE_UINT8           2
                                       /*! signed short. */
#define NIFTI_TYPE_INT16           4
                                       /*! signed int. */
#define NIFTI_TYPE_INT32           8
                                       /*! 32 bit float. */
#define NIFTI_TYPE_FLOAT32        16
                                       /*! 64 bit complex = 2 32 bit floats. */
#define NIFTI_TYPE_COMPLEX64      32
                                       /*! 64 bit float = double. */
#define NIFTI_TYPE_FLOAT64        64
                                       /*! 3 8 bit bytes. */
#define NIFTI_TYPE_RGB24         128
                                       /*! signed char. */
#define NIFTI_TYPE_INT8          256
                                       /*! unsigned short. */
#define NIFTI_TYPE_UINT16        512
                                       /*! unsigned int. */
#define NIFTI_TYPE_UINT32        768
                                       /*! signed long long. */
#define NIFTI_TYPE_INT64        1024
                                       /*! unsigned long long. */
#define NIFTI_TYPE_UINT64       1280
                                       /*! 128 bit float = long double. */
#define NIFTI_TYPE_FLOAT128     1536
                                       /*! 128 bit complex = 2 64 bit floats. */
#define NIFTI_TYPE_COMPLEX128   1792
                                       /*! 256 bit complex = 2 128 bit floats. */
#define NIFTI_TYPE_COMPLEX256   2048

                     /*-------- sample typedefs for complicated types ---*/
#if 0
typedef struct { float       r,i;     } complex_float ;
typedef struct { double      r,i;     } complex_double ;
typedef struct { long double r,i;     } complex_longdouble ;
typedef struct { unsigned char r,g,b; } rgb_byte ;
#endif

/*---------------------------------------------------------------------------*/
/* INTERPRETATION OF VOXEL DATA:
   ----------------------------
   The intent_code field can be used to indicate that the voxel data has
   some particular meaning.  In particular, a large number of codes is
   given to indicate that the the voxel data should be interpreted as
   being drawn from a given probability distribution.

   VECTOR-VALUED DATASETS:
   ----------------------
   If the intent_vector field is nonzero, this indicates that the last
   dimension of the dataset (e.g., dimension number dim[0]) is not a
   physical dimension, but is just a way of storing multiple values
   (e.g., a vector) at each location.  For example, the header values
     dim[0] = 4
     dim[1] = 64
     dim[2] = 64
     dim[3] = 20
     dim[4] = 3
     intent_vector = 1
     datatype = DT_FLOAT
   mean that this dataset should be interpreted as a 3D volume (64x64x20),
   with a 3-vector of floats defined at each point in the 3D grid.  Note
   that when intent_code != 0, then the true dimensionality of the dataset
   is dim[0]-1, rather than dim[0].

   A program reading a dataset with intent_vector != 0 may want to reformat
   the image data to store each voxels' set of values together in a struct
   or array.  This programming detail, however, is beyond the scope of the
   NIFTI-1 file specification!

   STATISTICAL PARAMETRIC DATASETS (i.e., SPMs):
   --------------------------------------------
   Values of intent_code from NIFTI_FIRST_STATCODE to NIFTI_LAST_STATCODE
   (inclusive) indicate that the numbers in the dataset should be interpreted
   as being drawn from a given distribution.  Most such distributions have
   auxiliary parameters (e.g., NIFTI_INTENT_TTEST has 1 DOF parameter).

   If intent_vector == 0, then the auxiliary parameters are the same for
   each voxel, and are given in header fields intent_p1, intent_p2, and
   intent_p3.

   If intent_vector != 0, then the auxiliary parameters are different for
   each voxel.  For example, the header values
     dim[0] = 3
     dim[1] = 128
     dim[2] = 128
     dim[3] = 2
     intent_vector = 1
     datatype = DT_FLOAT
     intent_code = NIFTI_INTENT_TTEST
   mean that this is a 2D dataset (128x128) of t-statistics, with the
   t-statistic being in the first "plane" of data and the degrees-of-freedom
   parameter being in the second "plane" of data.

   Note: intent_code values 2..10 are compatible with AFNI 1.5x (which is
   why there is no code with value==1, which is obsolescent in AFNI).

   OTHER INTENTIONS:
   ----------------
   The purpose of the intent_* fields is to help interpret the values
   stored in the dataset.  Some non-statistical values for intent_code
   and conventions are provided for storing other complex data types.

   The intent_name field provides space for a 15 character (plus 0 byte)
   'name' for the type of data stored. Examples:
    - intent_code == NIFTI_INTENT_ESTIMATE; intent_name == "T1";
       could be used to signify that the voxel values are estimates of the
       NMR parameter T1.
    - intent_code == NIFTI_INTENT_TTEST; intent_name == "House";
       could be used to signify that the voxel values are t-statistics
       for the significance of 'activation' response to a House stimulus.
    - intent_code == NIFTI_INTENT_DISPVECT; intent_name == "ToMNI152";
       could be used to signify that the voxel values are a displacement
       vector that transforms each voxel (x,y,z) location to the
       corresponding location in the MNI152 standard brain.
    - intent_code == NIFTI_INTENT_MATRIX; intent_name == "DTI";
       could be used to signify that the voxel values comprise a diffusion
       tensor image.

   If no data name is implied or needed, intent_name[0] should be set to 0.
-----------------------------------------------------------------------------*/

 /*! default: no intention is indicated in the header. */

#define NIFTI_INTENT_NONE        0

         /*-------- These codes are for probability distributions --------*/
         /*    Most distributions have a number of parameters,
               below denoted by p1, p2, and p3, and stored in
                 - intent_p1, intent_p2, intent_p3 if intent_vector == 0
                 - image data array                if intent_vector != 0 

            Functions to compute with many of the distributions
            below can be found in the CDF library from U Texas.

            Formulas for and discussion of these distributions
            can be found in the following books:

              [U] Univariate Discrete Distributions,
                  NL Johnson, S Kotz, AW Kemp.

              [C1] Continuous Univariate Distributions, vol. 1,
                   NL Johnson, S Kotz, N Balakrishnan.

              [C2] Continuous Univariate Distributions, vol. 2,
                   NL Johnson, S Kotz, N Balakrishnan.                    */
         /*---------------------------------------------------------------*/

  /*! [C2, chap 32] Correlation coefficient R (3 params):
       p1 = number of sample points
       p2 = number of "fitting" regressors
       p3 = number of "no interest" regressors
      R/sqrt(1-R*R) is t-distributed with (p1-p2-p3) DOF. */

#define NIFTI_INTENT_CORREL      2

  /*! [C2, chap 28] Student t statistic (1 param): p1 = DOF. */

#define NIFTI_INTENT_TTEST       3

  /*! [C2, chap 27] Fisher F statistic (2 params):
       p1 = numerator DOF, p2 = denominator DOF. */

#define NIFTI_INTENT_FTEST       4

  /*! [C1, chap 13] Standard normal (0 params): Density = N(0,1). */

#define NIFTI_INTENT_ZSCORE      5

  /*! [C1, chap 18] Chi-squared (1 param): p1 = DOF.
      Density(x) proportional to exp(-x/2) * x^(p1/2-1). */

#define NIFTI_INTENT_CHISQ       6

  /*! [C2, chap 25] Beta distribution (2 params): p1=a, p2=b.
      Density(x) proportional to x^(a-1) * (1-x)^(b-1). */

#define NIFTI_INTENT_BETA        7

  /*! [U, chap 3] Binomial distribution (2 params):
       p1 = number of trials, p2 = probability per trial.
      Prob(x) = (p1 choose x) * p2^x * (1-p2)^(p1-x), for x=0,1,...,p1. */

#define NIFTI_INTENT_BINOM       8

  /*! [C1, chap 17] Gamma distribution (2 params):
       p1 = shape, p2 = scale.
      Density(x) proportional to x^(p1-1) * exp(-p2*x). */

#define NIFTI_INTENT_GAMMA       9

  /*! [U, chap 4] Poisson distribution (1 param): p1 = mean.
      Prob(x) = exp(-p1) * p1^x / x! , for x=0,1,2,.... */

#define NIFTI_INTENT_POISSON    10

  /*! [C1, chap 13] Normal distribution (2 params):
       p1 = mean, p2 = standard deviation. */

#define NIFTI_INTENT_NORMAL     11

  /*! [C2, chap 30] Noncentral F statistic (3 params):
       p1 = numerator DOF, p2 = denominator DOF,
       p3 = numerator noncentrality parameter.  */

#define NIFTI_INTENT_FTEST_NONC 12

  /*! [C2, chap 29] Noncentral chi-squared statistic (2 params):
       p1 = DOF, p2 = noncentrality parameter.     */

#define NIFTI_INTENT_CHISQ_NONC 13

  /*! [C2, chap 23] Logistic distribution (2 params):
       p1 = location, p2 = scale.
      Density(x) proportional to sech^2((x-p1)/(2*p2)). */

#define NIFTI_INTENT_LOGISTIC   14

  /*! [C2, chap 24] Laplace distribution (2 params):
       p1 = location, p2 = scale.
      Density(x) proportional to exp(-abs(x-p1)/p2). */

#define NIFTI_INTENT_LAPLACE    15

  /*! [C2, chap 26] Uniform distribution: p1 = lower end, p2 = upper end. */

#define NIFTI_INTENT_UNIFORM    16

  /*! [C2, chap 31] Noncentral t statistic (2 params):
       p1 = DOF, p2 = noncentrality parameter. */

#define NIFTI_INTENT_TTEST_NONC 17

  /*! [C1, chap 21] Weibull distribution (3 params):
       p1 = location, p2 = scale, p3 = power.
      Density(x) proportional to
       ((x-p1)/p2)^(p3-1) * exp(-((x-p1)/p2)^p3) for x > p1. */

#define NIFTI_INTENT_WEIBULL    18

  /*! [C1, chap 18] Chi distribution (1 param): p1 = DOF.
      Density(x) proportional to x^(p1-1) * exp(-x^2/2) for x > 0.
       p1 = 1 == 'half normal' distribution
       p1 = 2 == Rayleigh distribution
       p1 = 3 == Maxwell-Boltzmann distribution.                  */

#define NIFTI_INTENT_CHI        19

  /*! [C1, chap 15] Inverse Gaussian (2 params):
       p1 = mu, p2 = lambda
      Density(x) proportional to
       exp(-p2*(x-p1)^2/(2*p1^2*x)) / x^3  for x > 0. */

#define NIFTI_INTENT_INVGAUSS   20

  /*! [C2, chap 22] Extreme value type I (2 params):
       p1 = location, p2 = scale
      cdf(x) = exp(-exp(-(x-p1)/p2)). */

#define NIFTI_INTENT_EXTVAL     21

  /*! Data is a 'p-value' (no params). */

#define NIFTI_INTENT_PVAL       22

  /*! Smallest intent_code that indicates a statistic. */

#define NIFTI_FIRST_STATCODE     2

  /*! Largest intent_code that indicates a statistic. */

#define NIFTI_LAST_STATCODE     22

 /*---------- these values for intent_code aren't for statistics ----------*/

 /*! To signify that the value at each voxel is an estimate
     of some parameter, set intent_code = NIFTI_INTENT_ESTIMATE. */

#define NIFTI_INTENT_ESTIMATE  1001

 /*! To signify that the value at each voxel is an index into
     some set of labels, set intent_code = NIFTI_INTENT_LABEL.
     The filename with the labels may stored in aux_file.        */

#define NIFTI_INTENT_LABEL     1002

 /*! To signify that the value at each voxel is an index into the
     NeuroNames labels set, set intent_code = NIFTI_INTENT_NEURONAME. */

#define NIFTI_INTENT_NEURONAME 1003

 /*! To store an M x N matrix at each voxel:
       - intent_vector must be nonzero
       - intent_code must be NIFTI_INTENT_MATRIX
       - dim[ dim[0] ] must be M*N
       - intent_p1 must be M (in float format)
       - intent_p2 must be N (ditto)             */

#define NIFTI_INTENT_MATRIX    1004

 /*! To signify that the vector value at each voxel is to be taken
     as a displacement field:
       - intent_vector must be nonzero
       - intent_code must be NIFTI_INTENT_DISPVECT
       - dim[ dim[0] ] must be the dimensionality of the displacment
         (e.g., 3 for spatial displacement, 2 for in-plane)          */

#define NIFTI_INTENT_DISPVECT  1005

 /*! To signify that the vector value at each voxel is really a
     spatial coordinates (e.g., the vertices of a surface mesh):
       - intent_vector must be nonzero
       - intent_code must be NIFTI_INTENT_POINT
       - dim[0] must be 2
       - dim[1] must be the number of points
       - dim[2] must be the dimensionality of space (e.g., 3 => 3D space). */

#define NIFTI_INTENT_POINT     1006

/*---------------------------------------------------------------------------*/
/* 3D IMAGE (VOLUME) ORIENTATION AND LOCATION IN SPACE:
   ---------------------------------------------------
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
   This is a right-handed coordinate system.  However, the exact direction
   these axes point with respect to the subject depends on qform_code
   (Method 2) and sform_code (Method 3).

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

   N.B.: Method 2 uses a factor 'pfac' which is either -1 or 1; pfac is
    stored in the otherwise unused pixdim[0].  If pixdim[0]==0.0 (which
    should not occur), we take pfac=1.

   METHOD 1 (the "old" way, used only when qform_code == 0):
   --------------------------------------------------------
   The coordinate mapping from (i,j,k) to (x,y,z) is the ANALYZE
   7.5 way.  This is a simple scaling relationship:

     x = pixdim[1] * i
     y = pixdim[2] * j
     z = pixdim[3] * k

   No particular spatial orientation is attached to these (x,y,z)
   coordinates.  (NIFTI-1 does not have the ANALYZE 7.5 orient field,
   which is not general and is often not set properly.)  This method
   is not recommended, and is present mainly for compatibility with
   ANALYZE 7.5 files.

   METHOD 2 (used when qform_code > 0, which should be the "normal case):
   ---------------------------------------------------------------------
   The (x,y,z) coordinates are given by the pixdim[] scales, a rotation
   matrix, and a shift.  This method is intended to represent
   "scanner-anatomical" coordinates, which are often embedded in the
   image header (e.g., DICOM fields 0020/0032, 0020/0037, 0028/0030, and
   0018/0050), and represent the nominal orientation and location of
   the data.  This method can also be used to represent "aligned"
   coordinates, which would typically result from some post-acquisition
   alignment of the volume to a standard orientation (e.g., the same
   subject on another day, or a rigid rotation to true anatomical
   orientation from the tilted position of the subject in the scanner).
   The formula for (x,y,z) in terms of header parameters and (i,j,k) is:

     [ x ]   [ R11 R12 R13 ] [ pfac * pixdim[1] * i ]   [ qoffset_x ]
     [ y ] = [ R21 R22 R23 ] [        pixdim[2] * j ] + [ qoffset_y ]
     [ z ]   [ R31 R32 R33 ] [        pixdim[3] * k ]   [ qoffset_z ]

   The qoffset_* shifts are in the NIFTI-1 header.  Note that the center
   of the (i,j,k)=(0,0,0) voxel (first value in the dataset array) is
   just (x,y,z)=(qoffset_x,qoffset_y,qoffset_z).

   The rotation matrix R is calculated from the quatern_* parameters.
   This calculation is described below.

   The scaling factor pfac is either 1 or -1.  The rotation matrix R
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

   METHOD 3 (used when sform_code > 0):
   -----------------------------------
   The (x,y,z) coordinates are given by a general affine transformation
   of the (i,j,k) indexes:

     x = srow_x[0] * i + srow_x[1] * j + srow_x[2] * j + srow_x[3]
     y = srow_y[0] * i + srow_y[1] * j + srow_y[2] * j + srow_y[3]
     z = srow_z[0] * i + srow_z[1] * j + srow_z[2] * j + srow_z[3]

   The srow_* vectors are in the NIFTI_1 header.  Note that no use is
   made of pixdim[] in this method.

   WHY 3 METHODS?
   --------------
   Method 1 is provided only for backwards compatibility.  The intention
   is that Method 2 (qform_code > 0) represents the nominal voxel locations
   as reported by the scanner, or as rotated to some fiducial orientation and
   location.  Method 3, if present (sform_code > 0), is to be used to give
   the location of the voxels in some standard space.  The sform_code
   indicates which standard space is present.  Both methods 2 and 3 can be
   present, and be useful in different contexts (method 2 for displaying the
   data on its original grid; method 3 for displaying it on a standard grid).

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

   QUATERNION REPRESENATION OF ROTATION MATRIX (METHOD 2)
   ------------------------------------------------------
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

   Requiring a >= 0 is equivalent to requiring -Pi <= h <= Pi.  (Note that
   [-a,-b,-c,-d] represents the same rotation as [a,b,c,d]; there are 2
   quaternions that can be used to represent a given rotation matrix R.)
   To rotate a 3-vector (x,y,z) using quaternions, we compute the
   quaternion product

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
   this format.  The DICOM attribute "Image Position (Patient)" 0020/0032
   stores the (Xd,Yd,Zd) coordinates of the center of the first voxel.
   Here, (Xd,Yd,Zd) refer to DICOM coordinates, and Xd=-x, Yd=-y, Zd=z
   (i.e., DICOM +Xd is Right, +Yd is Posterior, +Zd is Superior).
-----------------------------------------------------------------------------*/

   /* [qs]form_code value:  */      /* x,y,z coordinate system refers to:    */
   /*-----------------------*/      /*---------------------------------------*/

 /*! Arbitrary coordinates (Method 1). */

#define NIFTI_XFORM_UNKNOWN      0

 /*! Scanner-based anatomical coordinates. */

#define NIFTI_XFORM_SCANNER_ANAT 1

 /*! Coordinates aligned to another file's, or to anatomical "truth". */

#define NIFTI_XFORM_ALIGNED_ANAT 2

 /*! Coordinates aligned to Talairach-Tournoux Atlas; (0,0,0)=AC. */

#define NIFTI_XFORM_TALAIRACH    3

 /*! MNI 152 normalized coordinates. */

#define NIFTI_XFORM_MNI_152      4

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

   It is best to initialize ALL fields in the NIFTI-1 header to 0
   (e.g., with calloc()), then fill in what is needed.
-----------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* MISCELLANEOUS C MACROS
-----------------------------------------------------------------------------*/

/*.................*/
/*! Given a nifti_1_header struct, check if it has a good magic number.
    Returns 1 if magic is good, 0 if it is not.                         */

#define NIFTI_GOOD_MAGIC(h)                                                 \
   ( (h).magic[0]=='n' && (h).magic[3]=='\0' &&                             \
     (( (h).magic[1]>='1' && (h).magic[1]<='9' && (h).magic[2]=='+' ) ||    \
      ( (h).magic[1]=='i' && (h).magic[2]>='1' && (h).magic[2]<='9' )   ))

/*.................*/
/*! Given a nifti_1_header_struct, returns the version (1..9) or 0. */

#define NIFTI_VERSION(h)                                                    \
 (  ( (h).magic[1]>='1' && (h).magic[1]<='9' ) ? (h).magic[1]-'0'           \
  : ( (h).magic[2]>='1' && (h).magic[2]<='9' ) ? (h).magic[2]-'0' : 0 )     \

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
