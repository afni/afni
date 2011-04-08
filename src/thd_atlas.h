#include "thd_ttatlas_query.h"
#undef DEBUG_SPACES
/* continuous colormap */
#define CONT_CMAP 0
/* integer colormap */
#define INT_CMAP 1
/* sparse integer colormap */
#define SPARSE_CMAP 2

/* continuous colormap */
#define CONT_CMAP 0
/* integer colormap */
#define INT_CMAP 1
/* sparse integer colormap */
#define SPARSE_CMAP 2

#define MAXINT 65535

typedef struct {
   char *xform_type, *xform_name, *source, *dest, *coord_order;
   float dist;   /* distance (cost) of xform between two spaces */
   int inverse;  /* inverse transformation from dest to src */
   int prepost;  /* for 2/12 part, evaluate coords pre/post xformation */
   int nelts;    /* number of data elements */
   void *xform;  /* data for xformation */
} ATLAS_XFORM;

typedef struct {
   char *atlas_dset_name;
   char *atlas_space;
   THD_3dim_dataset *atlas_dset;
} ATLAS;

typedef struct {
   char *atlas_space;
   char *generic_space;
} ATLAS_SPACE;

typedef struct {
   char *atlas_template;
   char *atlas_space;
} ATLAS_TEMPLATE;

typedef struct {
   int nxforms;
   ATLAS_XFORM *xform;
} ATLAS_XFORM_LIST;

typedef struct {
   int natlases;
   ATLAS *atlas;
} ATLAS_LIST;

typedef struct {
   int nspaces;
   ATLAS_SPACE *space;
} ATLAS_SPACE_LIST;

typedef struct {
   int ntemplates;
   ATLAS_TEMPLATE *atlas_template;
} ATLAS_TEMPLATE_LIST;

typedef struct {
   int nelts;
   void *rgblist;
} ATLAS_LUT;



ATLAS_SPACE_LIST *global_atlas_spaces;
ATLAS_XFORM_LIST *global_atlas_xfl;
ATLAS_LIST *global_atlas_alist;
ATLAS_TEMPLATE_LIST *global_atlas_templates;

void free_atlas_point_list(ATLAS_POINT_LIST *apl);
void print_atlas_point_list(ATLAS_POINT_LIST *apl);
ATLAS_POINT_LIST *dset_niml_to_atlas_list(THD_3dim_dataset *dset);
char *whereami_9yards(ATLAS_COORD ac, ATLAS_QUERY **wamip, ATLAS_LIST *atlas_alist);
/* AFNI_ATLAS_CODES *atlaslist, int N_atlaslist);*/
void init_custom_atlas(void);

void atlas_read_all(void);
ATLAS_XFORM_LIST *report_xform_chain(char *src, char *dest);

int   init_space_structs(ATLAS_XFORM_LIST **atlas_xfl,
   ATLAS_LIST **atlas_alist,
   ATLAS_SPACE_LIST **atlas_spaces,
   ATLAS_TEMPLATE_LIST **atlas_templates);
int read_space_niml(NI_stream space_niml, ATLAS_XFORM_LIST *atlas_xfl,
   ATLAS_LIST *atlas_alist,
   ATLAS_SPACE_LIST *atlas_spaces,
   ATLAS_TEMPLATE_LIST *atlas_template);
int atlas_read_xform(NI_element *nel, ATLAS_XFORM *atlas_xf);
int atlas_read_template(NI_element *nel, ATLAS_TEMPLATE *atlas_tpl);
int atlas_read_atlas(NI_element *nel, ATLAS *atlas);
int atlas_read_atlas_space(NI_element *nel, ATLAS_SPACE *at_space);
int make_space_neighborhood(ATLAS_SPACE_LIST *at_spl, ATLAS_XFORM_LIST *atlas_xfl);
ATLAS_XFORM *get_xform_neighbor(ATLAS_XFORM_LIST *atlas_xfl, ATLAS_SPACE *at_space, 
   ATLAS_SPACE *dest_space, int *inv_xf);
ATLAS_XFORM_LIST *
   pathlist_to_xform_list(int *nPath, int N_n, ATLAS_XFORM_LIST *atlas_xfl, 
   ATLAS_SPACE_LIST *at_spl);
int copy_xform(ATLAS_XFORM *src_xform, ATLAS_XFORM *dest_xform);
ATLAS_XFORM_LIST *
get_xform_chain(ATLAS_SPACE *at_space, ATLAS_SPACE *dest_space);

ATLAS_XFORM_LIST *read_space_xforms(NI_stream space_niml);
ATLAS_XFORM_LIST *calc_xform_list(ATLAS_XFORM_LIST *xfl);
int find_atlas_space_index(char *spacename);
void report_available_spaces(char *src);
ATLAS_SPACE_LIST *find_available_spaces(char *src_space_name);

void free_atlas_structs(void);

void free_xform_list(ATLAS_XFORM_LIST *xfl);
void free_xform(ATLAS_XFORM *xf);
void free_space_list(ATLAS_SPACE_LIST *xsl);
void free_space(ATLAS_SPACE *xs);
void free_template_list(ATLAS_TEMPLATE_LIST *xtl);
void free_template(ATLAS_TEMPLATE *xt);
void free_atlas_list(ATLAS_LIST *xal);
void free_atlas(ATLAS *xa);
void print_xform_list(ATLAS_XFORM_LIST *xfl);
void print_space_list(ATLAS_SPACE_LIST *xsl);
void print_atlas_list(ATLAS_LIST *xal);
void print_template_list(ATLAS_TEMPLATE_LIST *xtl);
void print_xform(ATLAS_XFORM *xf);
void print_all_xforms(ATLAS_XFORM_LIST *xfl);
void print_affine_xform_data(float *xfptr);

ATLAS_XFORM *calc_xf(ATLAS_XFORM *xf, ATLAS_XFORM *xf2);
int affine_mult(ATLAS_XFORM *xf, ATLAS_XFORM *xf2, ATLAS_XFORM *xf3);
int affine_2piece_mult(ATLAS_XFORM *xf,
   ATLAS_XFORM *xf2, ATLAS_XFORM *xf3, int dir);
int affine_12piece_mult(ATLAS_XFORM *xf,
   ATLAS_XFORM *xf2, ATLAS_XFORM *xf3, int dir);
int x2piece_2piece_mult(ATLAS_XFORM *xf,
   ATLAS_XFORM *xf2, ATLAS_XFORM *xf3);
int x2piece_12piece_mult(ATLAS_XFORM *xf,
   ATLAS_XFORM *xf2, ATLAS_XFORM *xf3, int dir);
int x12piece_12piece_mult(ATLAS_XFORM *xf,
   ATLAS_XFORM *xf2, ATLAS_XFORM *xf3);
   
int invert_xform(ATLAS_XFORM *xf);
int invert_affine(ATLAS_XFORM *xf);
int invert_12piece(ATLAS_XFORM *xf);
int invert_2piece(ATLAS_XFORM *xf);

ATLAS_XFORM *identity_xform(void);

int find_atlas_space(ATLAS_SPACE_LIST *at_spl, ATLAS_SPACE *at_space);
int apply_xform_chain(ATLAS_XFORM_LIST *xfl, float x, float y, float z,
                  float *xout, float *yout, float *zout);

int apply_xform_affine(ATLAS_XFORM *xf, float x, float y, float z, \
    float *xout, float *yout, float *zout);
int apply_xform_2piece(ATLAS_XFORM *xf, float x, float y, float z, \
    float *xout, float *yout, float *zout);
int apply_xform_12piece(ATLAS_XFORM *xf, float x, float y, float z, \
    float *xout, float *yout, float *zout);
int apply_xform_brett_tt2mni(float x, float y, float z, \
    float *xout, float *yout, float *zout);
int apply_xform_brett_mni2tt(float x, float y, float z, \
    float *xout, float *yout, float *zout);

NI_element *NI_find_next_element(NI_stream ns, char *name);

void AFNI_atlas_list_to_niml(void);
void atlas_list_to_niml(ATLAS_POINT_LIST *atp, char *atlas_file,
             int n_regions);
/*static int write_niml_string(char *nimlstring, NI_stream ns);*/
NI_element *atlas_point_to_niml_element(ATLAS_POINT *at_pt);
char *atlas_point_to_niml_string(ATLAS_POINT *at_pt);
static void niml_to_atlas_list(ATLAS_POINT_LIST *atp, char *atlas_file);
static void adjust_atlas_point_list(ATLAS_POINT_LIST *atp, char *match_str,
            float addval);

/* static void niml_string_to_atlas_point(char *niml_string, ATLAS_POINT *at_pt);
 */
static ATLAS_POINT_LIST *AFNI_atlas_list_to_atlas_point_list(
        ATLAS_POINT *afni_at_pts, int npts);

#if 0
char * THD_get_space(THD_3dim_dataset *dset);
int THD_space_code(char *space);

THD_3dim_dataset *
get_session_dset_id(THD_session *sess, MCW_idcode idcode, int space_index);
THD_3dim_dataset *
get_session_dset(THD_session *sess, int index, int space_index);
int
set_session_dset(THD_3dim_dataset *dset, THD_session *sess,
   int index, int space_index);
void set_nspaces(int n);
void set_atlas_nspaces(void);
int get_nspaces(void);
#endif
