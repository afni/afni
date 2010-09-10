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
} atlas_xform;

typedef struct {
   char *atlas_dset;
   char *atlas_space;
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
   atlas_xform *xform;
} atlas_xform_list;

typedef struct {
   int natlases;
   ATLAS *atlas;
} atlas_list;

typedef struct {
   int nspaces;
   ATLAS_SPACE *space;
} atlas_space_list;

typedef struct {
   int ntemplates;
   ATLAS_TEMPLATE *atlas_template;
} atlas_template_list;

typedef struct {
   int nelts;
   void *rgblist;
} atlas_lut;

char * THD_get_space(THD_3dim_dataset *dset);
int THD_space_code(char *space);

atlas_space_list *atlas_spaces;

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
