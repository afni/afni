#ifndef GIFTI_IO_H
#define GIFTI_IO_H

/* ---------------------------------------------------------------------- */
/* These must be 0-based and sequential.
        - 0 matches _UNDEF
        - highest maches _MAX
        - list matches corresponding gifti_*_list
*/

#define GIFTI_IND_ORD_UNDEF     0
#define GIFTI_IND_ORD_ROW_MAJOR 1
#define GIFTI_IND_ORD_COL_MAJOR 2
#define GIFTI_IND_ORD_MAX       2

#define GIFTI_CAT_UNDEF         0
#define GIFTI_CAT_COORDINATES   1
#define GIFTI_CAT_FUNCTIONAL    2
#define GIFTI_CAT_NORMALS       3
#define GIFTI_CAT_LABELS        4
#define GIFTI_CAT_RGBA          5
#define GIFTI_CAT_SHAPE         6
#define GIFTI_CAT_TENSORS       7
#define GIFTI_CAT_TOPO_TRI      8
#define GIFTI_CAT_MAX           8

#define GIFTI_DATALOC_UNDEF     0
#define GIFTI_DATALOC_INT       1
#define GIFTI_DATALOC_EXT       2
#define GIFTI_DATALOC_MAX       2

#define GIFTI_ENCODING_UNDEF     0
#define GIFTI_ENCODING_ASCII     1      /* human readable ASCII data  */
#define GIFTI_ENCODING_B64BIN    2      /* base64 encoded binary data */
#define GIFTI_ENCODING_B64GZ     3      /* base64 compressed binary   */
#define GIFTI_ENCODING_EXTBIN    4      /* external unencoded binary  */
#define GIFTI_ENCODING_MAX       4

#define GIFTI_ENDIAN_UNDEF       0
#define GIFTI_ENDIAN_BIG         1
#define GIFTI_ENDIAN_LITTLE      2
#define GIFTI_ENDIAN_MAX         2

#define GIFTI_B64_CHECK_NONE     0
#define GIFTI_B64_CHECK_DETECT   1
#define GIFTI_B64_CHECK_COUNT    2
#define GIFTI_B64_CHECK_SKIP     3
#define GIFTI_B64_CHECK_SKIPNCOUNT 4

#define GIFTI_DARRAY_DIM_LEN     6  /* length of dims[] array */

/* global declarations of matching lists */
extern char * gifti_index_order_list[];
extern char * gifti_dataloc_list[];
extern char * gifti_encoding_list[] ;
extern char * gifti_endian_list[];

/* ---------------------------------------------------------------------- */

/* notes:

        - all data should be owned by the struct, i.e. strings should be
          allocated copies, never assigned as a pointer used elsewhere
*/

typedef struct {
    int     length;
    char ** name;
    char ** value;
} nvpairs;
typedef nvpairs giiMetaData;

typedef struct {
    int     length;
    int   * index;
    char ** label;
} giiLabelTable;

typedef struct {
    char   * dataspace;
    char   * xformspace;
    double   xform[4][4];       /* ******** no type specified ********** */
} giiCoordSystem;

typedef struct {
    /* attributes */
    int              intent;    /* NIFTI_INTENT code, describing data    */
    int              datatype;  /* numerical type of Data values         */
    int              ind_ord;   /* lowest Dim to highest, or reverse     */
    int              num_dim;   /* level of DimX applied                 */
    int              dims[6];   /* dimension lengths (first num_dim set) */
    int              encoding;  /* format of Data on disk                */
    int              endian;    /* endian, if binary Encoding            */
    char *           ext_fname; /* external filename, in cur directory   */
    long long        ext_offset;/* offset of data within external file   */

    /* elements */
    giiMetaData      meta;
    giiCoordSystem * coordsys;
    void           * data;      /* unencoded, uncompressed, swapped      */

    /* extras */
    long long        nvals;     /* number of values (product of Dims)    */
    int              nbyper;    /* number of bytes per value             */
    nvpairs          ex_atrs;   /* extra attributes                      */
} giiDataArray;

typedef struct {
    /* attributes */
    char           * version;   /* GIFTI version string            */

    /* elements */
    giiMetaData      meta;
    giiLabelTable    labeltable;
    giiDataArray  ** darray;

    /* extras */
    int              numDA;     /* number of DataArrays            */
    int              swapped;   /* were the bytes swapped          */
    int              compressed;/* was the data compressed         */
    nvpairs          ex_atrs;   /* extra attributes                */
} gifti_image;

typedef struct {
    int verb;
} gifti_globals;

typedef struct {
    int    type;          /* should match NIFTI_TYPE_* */
    int    nbyper;        /* bytes per value           */
    int    swapsize;      /* bytes per swap piece      */
    char * name;          /* text string match type    */
} gifti_type_ele;


/* prototypes */

/* main interface protos */
gifti_image * gifti_read_image (const char * fname, int read_data );
gifti_image * gifti_read_da_list (const char * fname, int read_data,
                                  const int * dalist, int len );
int gifti_write_image(gifti_image * gim, const char * fname, int write_data);

int    gifti_free_image         (gifti_image * gim );

int    gifti_check_swap         (void *data, int endian, long long nsets,
                                 int swapsize);
int    gifti_swap_Nbytes        (void *data, long long nsets, int swapsize);

/* end main interface protos */

int    gifti_get_b64_check      (void);
int    gifti_set_b64_check      (int level);
int    gifti_get_zlevel         (void);
int    gifti_set_zlevel         (int level);
int    gifti_get_verb           (void);
int    gifti_set_verb           (int level);

long long gifti_darray_nvals       (giiDataArray * da);

void   gifti_datatype_sizes     (int datatype, int *nbyper, int *swapsize);
char * gifti_datatype2str       (int type);
int    gifti_get_this_endian    (void);
int    gifti_gim_DA_size        (gifti_image * p, int in_mb);
int    gifti_intent_from_string (const char * name);
int    gifti_intent_is_valid    (int code);
char * gifti_intent_to_string   (int code);
char * gifti_list_index2string  (char * list[], int index);
int    gifti_set_xml_buf_size   (int buf_size);
int    gifti_str2attr_gifti     (gifti_image * gim, const char * attr,
                                                        const char * val);
int    gifti_str2attr_darray    (giiDataArray * DA, const char * attr,
                                                 const char * value);
int    gifti_str2ind_ord        (const char * str);
int    gifti_str2dataloc        (const char * str);
int    gifti_str2encoding       (const char * str);
int    gifti_str2endian         (const char * str);
int    gifti_str2datatype       (const char * str);
char * gifti_strdup             (const char * src);
int    gifti_swap_2bytes        (void *data, long long nsets);
int    gifti_swap_4bytes        (void *data, long long nsets);

int    gifti_add_empty_darray   (gifti_image * gim);
int    gifti_add_to_nvpairs     (nvpairs * p, const char * name,
                                              const char * value);

int    gifti_init_darray_from_attrs(giiDataArray * da, const char ** attr);

int    gifti_free_CoordSystem   (giiCoordSystem * cs);
int    gifti_free_DataArray_list(giiDataArray ** darray, int numDA);
int    gifti_free_DataArray     (giiDataArray * darray);
int    gifti_free_LabelTable    (giiLabelTable * t);
int    gifti_free_nvpairs       (nvpairs * p);

int    gifti_valid_darray       (giiDataArray * da, int whine);
int    gifti_valid_datatype     (int dtype, int whine);
int    gifti_valid_dims         (giiDataArray * da, int whine);
int    gifti_valid_nbyper       (int nbyper, int whine);
int    gifti_valid_num_dim      (int num_dim, int whine);
int    gifti_valid_nvpairs      (nvpairs * nvp, int whine);
int    gifti_validate_dims      (giiDataArray * da, int whine);


void   gifti_disp_lib_hist       (void);
void   gifti_disp_lib_version    (void);

int    gifti_disp_nvpairs        (const char *mesg, nvpairs *p);
int    gifti_disp_LabelTable     (const char *mesg, giiLabelTable *p);
int    gifti_disp_CoordSystem    (const char *mesg, giiCoordSystem *p);
int    gifti_disp_DataArray      (const char *mesg, giiDataArray *p, int subs);
int    gifti_disp_gifti_image    (const char *mesg, gifti_image *p, int subs);

int    gifti_disp_hex_data       (const char *mesg, const void *data, int len,
                                  FILE * fp);


int    clear_nvpairs             (nvpairs * p);
int    clear_LabelTable          (giiLabelTable * p);
int    clear_CoordSystem         (giiCoordSystem * p);

giiDataArray * gifti_find_DA     (gifti_image * gim, int intent, int index);
int    gifti_find_DA_list        (gifti_image * gim, int intent,
                                  giiDataArray *** list,int *len);
int    gifti_DA_rows_cols        (giiDataArray * da, int *rows, int *cols);


#endif /* GIFTI_IO_H */
