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

void free_atlas_point_list(ATLAS_POINT_LIST *apl);
void print_atlas_point_list(ATLAS_POINT_LIST *apl);
ATLAS_POINT_LIST *dset_niml_to_atlas_list(THD_3dim_dataset *dset);
char *whereami_9yards(ATLAS_COORD ac, ATLAS_QUERY **wamip, ATLAS_LIST *atlas_alist);
/* AFNI_ATLAS_CODES *atlaslist, int N_atlaslist);*/
void init_custom_atlas(void);


#if 0
char * THD_get_space(THD_3dim_dataset *dset);
int THD_space_code(char *space);

ATLAS_SPACE_LIST *global_atlas_spaces;

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
