#ifndef _RWC_ANALYZE_HEADER_
/***************************************************************************
From http://www.mayo.edu/bir/Analyze_Pages/AnalyzeFileInfo.html
Describes the ANALYZE 7.5 file formate (.img/.hdr pairs).
----------------------------------------------------------------
The image database is the system of files that the ANALYZE package uses to
organize and access image data on the disk. Facilities are provided for
converting data from a number of sources for use with the package. A
description of the database format is provided to aid developers in porting
images from other sources for use with the ANALYZE TM system. An ANALYZE
image database consists of at least two files:

   * an image file
   * a header file

The files have the same name being distinguished by the extensions .img for
the image file and .hdr for the header file. Thus, for the image database
heart, there are the UNIX files heart.img and heart.hdr. The ANALYZE
programs all refer to this pair of files as a single entity named heart.

Image File

The format of the image file is very simple containing usually uncompressed
pixel data for the images in one of several possible pixel formats:

   * 1 bit            packed binary (slices must begin on byte boundaries)
   * 8 bit            8 bits per pixel (unsigned char)
   * 16 bit           16 bits per pixel (signed short)
   * 32 bit           32 bits per pixel signed integers, or floating point
   * 64 bit           64 bits per pixel; double precision, floating point,
                       or complex.
   * 24 bit           RGB , 8-bits per channel Red, Green, Blue.

Header File

The header file is represented here as a `C' structure which describes the
dimensions and history of the pixel data. The header structure consists of
three substructures:

     header_key         describes the header
     image_dimension    describes image sizes
     data_history       optional
****************************************************************************/

struct header_key                       /* header key   */
       {                                /* off + size      */
       int sizeof_hdr;                  /* 0 + 4           */
       char data_type[10];              /* 4 + 10          */
       char db_name[18];                /* 14 + 18         */
       int extents;                     /* 32 + 4          */
       short int session_error;         /* 36 + 2          */
       char regular;                    /* 38 + 1          */
       char hkey_un0;                   /* 39 + 1          */
       };                               /* total=40 bytes  */
struct image_dimension
       {                                /* off + size      */
       short int dim[8];                /* 0 + 16          */
       short int unused8;               /* 16 + 2          */
       short int unused9;               /* 18 + 2          */
       short int unused10;              /* 20 + 2          */
       short int unused11;              /* 22 + 2          */
       short int unused12;              /* 24 + 2          */
       short int unused13;              /* 26 + 2          */
       short int unused14;              /* 28 + 2          */
       short int datatype;              /* 30 + 2          */
       short int bitpix;                /* 32 + 2          */
       short int dim_un0;               /* 34 + 2          */
       float pixdim[8];                 /* 36 + 32         */
                       /*
                            pixdim[] specifies the voxel dimensitons:
                            pixdim[1] - voxel width
                            pixdim[2] - voxel height
                            pixdim[3] - interslice distance
                                ...etc
                       */
       float vox_offset;                /* 68 + 4          */
       float funused1;                  /* 72 + 4          */
       float funused2;                  /* 76 + 4          */
       float funused3;                  /* 80 + 4          */
       float cal_max;                   /* 84 + 4          */
       float cal_min;                   /* 88 + 4          */
       float compressed;                /* 92 + 4          */
       float verified;                  /* 96 + 4          */
       int glmax,glmin;                 /* 100 + 8         */
       };                               /* total=108 bytes */
struct data_history
       {                                /* off + size      */
       char descrip[80];                /* 0 + 80          */
       char aux_file[24];               /* 80 + 24         */
       char orient;                     /* 104 + 1         */
       char originator[10];             /* 105 + 10        */
       char generated[10];              /* 115 + 10        */
       char scannum[10];                /* 125 + 10        */
       char patient_id[10];             /* 135 + 10        */
       char exp_date[10];               /* 145 + 10        */
       char exp_time[10];               /* 155 + 10        */
       char hist_un0[3];                /* 165 + 3         */
       int views;                       /* 168 + 4         */
       int vols_added;                  /* 172 + 4         */
       int start_field;                 /* 176 + 4         */
       int field_skip;                  /* 180 + 4         */
       int omax, omin;                  /* 184 + 8         */
       int smax, smin;                  /* 192 + 8         */
       };
struct dsr
       {
       struct header_key hk;            /* 0 + 40          */
       struct image_dimension dime;     /* 40 + 108        */
       struct data_history hist;        /* 148 + 200       */
       };                               /* total= 348 bytes*/

/* Acceptable values for datatype */

#define ANDT_NONE             0
#define ANDT_UNKNOWN          0  /* what it says, dude           */
#define ANDT_BINARY           1  /* binary (1 bit/voxel)         */
#define ANDT_UNSIGNED_CHAR    2  /* unsigned char (8 bits/voxel) */
#define ANDT_SIGNED_SHORT     4  /* signed short (16 bits/voxel) */
#define ANDT_SIGNED_INT       8  /* signed int (32 bits/voxel)   */
#define ANDT_FLOAT           16  /* float (32 bits/voxel)        */
#define ANDT_COMPLEX         32  /* complex (64 bits/voxel)      */
#define ANDT_DOUBLE          64  /* double (64 bits/voxel)       */
#define ANDT_RGB            128  /* RGB triple (24 bits/voxel)   */
#define ANDT_ALL            255

/***************************************************************************
The header format is flexible and can be extended for new user-defined data
types. The essential structures of the header are the header_key and the
image_dimension.

The required elements in the header_key substructure are:

     int sizeof_header      Must indicate the byte size of the header file.
     int extents            Should be 16384, the image file is created as
                              contiguous with a minimum extent size.
     char regular           Must be `r' to indicate that all images and
                            volumes are the same size.

The image_dimension substructure describes the organization and size of the
images. These elements enable the database to reference images by volume and
slice number. Explanation of each element follows:

     short int dim[]; = array of the image dimensions 
          dim[0]      = Number of dimensions in database; usually 4
          dim[1]      = Image X dimension; number of pixels in an image row
          dim[2]      = Image Y dimension; number of pixel rows in slice
          dim[3]      = Volume Z dimension; number of slices in a volume
          dim[4]      = Time points, number of volumes in database.

          char vox_units[4] = specifies the spatial units of measure for a
                               voxel
          char cal_units[4] = specifies the name of the calibration unit

         short int datatype = datatype for this image set 
                               Acceptable values for datatype are one of
                               the ANDT_* defines above

         short int bitpix   = number of bits per pixel; 1, 8, 16, 32, 64
         short int dim_un0  = unused 

         float pixdim[]     = Parallel array to dim[], giving real world
                               measurements in mm and ms.
          pixdim[1]         = voxel width in mm
          pixdim[2]         = voxel height in mm
          pixdim[3]         = slice thickness in mm
         float vox_offset   = byte offset in the .img file at which voxels
                               start. This value can be negative to specify
                               that the absolute value is applied for every image
                               in the file
         float cal_max,     = specify the range of calibration values
               cal_min
         int glmax, glmin   = The maximum and minimum pixel values for the
                               entire database

The data_history substructure is not required, but the orient field is used
to indicate individual slice orientation and determines whether the Movie
program will attempt to flip the images before displaying a movie sequence.

     orient = slice orientation for this dataset.
                0 = transverse unflipped
                1 = coronal unflipped
                2 = sagittal unflipped
                3 = transverse flipped
                4 = coronal flipped
                5 = sagittal flipped
****************************************************************************/
#endif /* _RWC_ANALYZE_HEADER_ */
