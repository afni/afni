
#define MAX_STR_LEN	1024

#define MOD_INVALID       -1
#define MOD_STR            0
#define MOD_CHAR           1		/* char mods */
#define MOD_U1            21		/* int mods  */
#define MOD_S1            22
#define MOD_U2            23
#define MOD_S2            24
#define MOD_U4            25
#define MOD_S4            26
#define MOD_F4            30		/* real mods */
#define MOD_F8            31

#define USE_SHORT	   0
#define USE_LONG           1
#define USE_VERSION        2
#define USE_GE             3
#define USE_HISTORY        4

/* GE diplay bit values */
#define GE_NONE		   0
#define GE_ALL          0xff
#define GE_HEADER	0x01
#define GE_EXTRAS	0x02
#define GE_UV17		0x10
#define GE_OFF		0x20

#define NDISP_NONE	   0
#define NDISP_INT2	0x01
#define NDISP_INT4	0x02
#define NDISP_REAL4     0x04
#define NDISP_ALL	0xff

#define CHECK_NULL_STR(str) ( str ? str : "(nil)" )

				  /* from Ifile.c ...                  */
typedef struct		          /* stuff extracted from GE 5.x image */
{
  int   good;                     /* is this a good image?             */
  int   nx, ny;                   /* image matrix                      */
  int   uv17;                     /* apparently codes for scan index   */
  float dx,dy,dz, zoff, tr,te;    /* various dimensions                */
  char  orients[8];               /* orientation string                */
} ge_header_info;

typedef struct	                  /* extra stuff from mri_read.c       */
{
  int   bpp;                      /* bits per pixel                    */
  int   cflag;                    /* compression flag (0=compressed)   */
  int   hdroff;                   /* offset of image header            */
  int   skip;                     /* offset of image data into file    */
  int   swap;                     /* did we do byte swapping?          */
  float xyz[9];
} ge_extras;

typedef struct
{
    int     num_files;        /* number of files to work with       */
    char ** flist;           /* list of files to work with         */
    int     debug;          /* debug level = 0, 1 or 2            */
    int     data_len;      /* bytes of data in mod_data          */
    int     ge_disp;      /* do we display ge_values            */
    int     ge4_disp;    /* option bits for GEMS 4.x type      */
    int     ndisp;      /* option bits for numeric display    */

    int     swap;	      /* do we need to swap bytes   */
    int     modify;	     /* do we modify the data?     */
    int     mod_type;       /* a string or repeated value */
    long    offset;	   /* starting location          */
    int     length;       /* bytes to display or modify */
    int     quiet;       /* do not display header info */
    char  * mod_data;	/* new data                   */
} param_t;

typedef struct			  /* file offsets for various fields   */
{
    int nx, ny;
    int uv17;
    int dx, dy, dz;
    int tr, te;
    int xyz;			  /* 3x3 float matrix from extras      */
} ge_off;

/* local protos */
int  attack_files      ( param_t * p );
int  check_mod_type    ( char * name );
int  check_usage       ( int argc, char * argv[] );
int  disp_numeric_data ( char * data, param_t * p, FILE * fp );
int  disp_param_data   ( param_t * p );
int  mtype_size        ( int type );

int  read_ge_header    ( char * pathname, ge_header_info * hi, ge_extras * E,
       			 ge_off * off );
int  process_file      ( char * pathname, param_t * p );
int  process_ge        ( char * pathname, param_t * p );
int  process_ge4       ( char * pathname, param_t * p );
int  set_params        ( param_t * p, int argc, char * argv[] );

int  help_full         ( char * prog );
int  help_ge_structs   ( char * prog );
int  usage             ( char * prog, int level );
int  write_data_to_file( FILE * fp, char * filename, param_t * p );

unsigned long THD_filesize  ( char * pathname );

int  disp_ge_offsets        ( char * info, ge_off         * D );
int  r_idisp_ge_extras      ( char * info, ge_extras      * E );
int  r_idisp_ge_header_info ( char * info, ge_header_info * I );

