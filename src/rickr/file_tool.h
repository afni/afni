
				  /* from Ifile.c ... */
typedef struct		          /* stuff extracted from GE I.* image */
{
  int   good;                     /* is this a good image? */
  int   nx, ny;                   /* image matrix */
  int   uv17;                     /* apparently codes for scan index */
  float dx,dy,dz, zoff, tr,te;    /* various dimensions */
  char  orients[8];               /* orientation string */
} ge_header_info;

typedef struct	                  /* extra stuff from mri_read.c     */
{
  int   bpp;                      /* bits per pixel                  */
  int   cflag;                    /* compression flag (0=compressed) */
  int   hdroff;                   /* offset of image header          */
  int   skip;                     /* offset of image data into file  */
  int   swap;                     /* did we do byte swapping?        */
  float xyz[9];
} ge_extras;

typedef struct
{
    int     num_files;
    char ** flist;
    int     debug;	/* debug level = 0, 1 or 2       */
    int     data_len;   /* bytes of data in mod_data     */
    int     ge_disp;    /* do we display ge_values       */

    int     swap;	/* do we need to swap bytes      */
    int     modify;	/* do we modify the data?        */
    int     mod_type;   /* a string or repeated value    */
    long    offset;	/* location to display or modify */
    int     length;     /* length to display or modify   */
    int     quiet;      /* do not display header info    */
    char  * mod_data;	/* new data                      */
} param_t;

#define VERSION		"1.1 - 2003.02.26"
#define MAX_STR_LEN	1024

#define MOD_STRING         0
#define MOD_SINGLE         1

#define USE_SHORT	   0
#define USE_LONG           1
#define USE_VERSION        2
#define USE_GE             3

#define GE_NONE		   0
#define GE_ALL          0xff
#define GE_HEADER	0x01
#define GE_EXTRAS	0x02
#define GE_UV17		0x10


/* local protos */
int  attack_files   ( param_t * p );
int  check_usage    ( int argc, char * argv[] );
int  disp_param_data( param_t * p );
long l_THD_filesize ( char * pathname );
int  read_ge_header ( char * pathname, ge_header_info * hi, ge_extras * E );
int  process_file   ( char * pathname, param_t * p );
int  set_params     ( param_t * p, int argc, char * argv[] );
int  swap_4         ( void * ptr );

int  help_full      ( char * prog );
int  help_ge_structs( char * prog );
int  usage          ( char * prog, int level );

int  r_idisp_ge_extras      ( char * info, ge_extras      * E );
int  r_idisp_ge_header_info ( char * info, ge_header_info * I );
