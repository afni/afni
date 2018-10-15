#ifndef _THD_ATLAS_H_
#define _THD_ATLAS_H_

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
#define MAX_WAMI_LINE_CHARS  80 

#define LINKRBRAIN_SITE "linkrbrain.eu"   /* 16 Oct 2015 */
extern char * get_linkrbrain_site(void) ;

void free_atlas_point_list(ATLAS_POINT_LIST *apl);
void print_atlas_point_list(ATLAS_POINT_LIST *apl);
ATLAS_POINT_LIST * niml_atlas_label_table_to_atlas_list(NI_group *ngr);
ATLAS_POINT_LIST *dset_niml_to_atlas_list(THD_3dim_dataset *dset);
ATLAS_POINT_LIST *label_table_to_atlas_point_list(Dtable *dtbl);
int whereami_9yards( ATLAS_COORD ac, ATLAS_QUERY **wamip, 
                     ATLAS_LIST *atlas_alist);
void init_custom_atlas(void);
const char *Space_Code_to_Space_Name (AFNI_STD_SPACES cod);
AFNI_STD_SPACES Space_Name_to_Space_Code(char *nm); 
const char *Atlas_Code_to_Atlas_Space_Name (AFNI_ATLAS_CODES cod);
const char *Atlas_Code_to_Atlas_Dset_Name (AFNI_ATLAS_CODES cod);
AFNI_ATLAS_CODES Atlas_Dset_Name_to_Atlas_Code(char *dset_name);
char *Atlas_Name(ATLAS *atl);
int is_Atlas_Named(ATLAS *atl, char *name);
int is_Coord_Space_Named(ATLAS_COORD ac, char *name);
int set_Coord_Space_Name(ATLAS_COORD *ac, char *name);
int init_global_atlas_from_niml_files(void);
ATLAS_LIST *create_atlas_list(void);
ATLAS_XFORM_LIST *report_xform_chain(char *src, char *dest, int report);
int is_known_coord_space(char *space_name);
void print_atlas_coord (ATLAS_COORD ac);
int show_wrapping_line(char * str, char * prefix, int indent, FILE * fp);

int   init_space_structs(ATLAS_XFORM_LIST **atlas_xfl,
   ATLAS_LIST **atlas_alist,
   ATLAS_SPACE_LIST **atlas_spaces,
   ATLAS_TEMPLATE_LIST **atlas_templates);
int read_space_niml_file(char *fname, ATLAS_XFORM_LIST *atlas_xfl,
   ATLAS_LIST *atlas_alist,
   ATLAS_SPACE_LIST *atlas_spaces,
   ATLAS_TEMPLATE_LIST *atlas_template, THD_string_array *sar);
int add_atlas_nel(NI_element *nel, ATLAS_XFORM_LIST *atlas_xfl,
   ATLAS_LIST *atlas_alist,
   ATLAS_SPACE_LIST *atlas_spaces,
   ATLAS_TEMPLATE_LIST *atlas_templates,
   THD_string_array *sar,
   char *parentdir);
int atlas_read_xform(NI_element *nel, ATLAS_XFORM *atlas_xf);
int atlas_read_template(NI_element *nel, ATLAS_TEMPLATE *atlas_tpl);
int atlas_read_atlas(NI_element *nel, ATLAS *atlas, char *pd);
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
ATLAS_SPACE_LIST *find_available_spaces(char *src_space_name, 
                                        ATLAS_SPACE_LIST *this_list);
char * THD_get_generic_space(THD_3dim_dataset *dset);
char * THD_get_view_space(THD_3dim_dataset *dset);

void free_global_atlas_structs(void);

void free_xform_list(ATLAS_XFORM_LIST *xfl);
void free_xform(ATLAS_XFORM *xf);
void free_space_list(ATLAS_SPACE_LIST *xsl);
void free_space(ATLAS_SPACE *xs);
void free_template_list(ATLAS_TEMPLATE_LIST *xtl);
void free_template(ATLAS_TEMPLATE *xt);
void free_atlas_list(ATLAS_LIST *xal);
void free_atlas(ATLAS *xa);
int is_identity_xform_chain(char *src, char *dest);
int is_identity_xform_list(ATLAS_XFORM_LIST *xfl, int combine);
void print_xform_chain(char *src, char *dest);
void print_xform_list(ATLAS_XFORM_LIST *xfl);
void print_space_list(ATLAS_SPACE_LIST *xsl);
void print_atlas_list(ATLAS_LIST *xal);
void print_atlas_table(ATLAS_LIST *xal);
void print_atlas(ATLAS *xa, int level) ;
void print_atlas_comment(ATLAS *xa) ;
void print_atlas_type(ATLAS *xa) ;
void print_atlas_supp_web_info(ATLAS *xa);
void print_atlas_orient(ATLAS *xa) ;
void print_point_lists(ATLAS_LIST *xal);
void print_template_list(ATLAS_TEMPLATE_LIST *xtl);
void print_xform(ATLAS_XFORM *xf);
void print_all_xforms(ATLAS_XFORM_LIST *xfl);
void print_affine_xform_data(float *xfptr);

ATLAS_POINT *atlas_point_named(ATLAS_POINT_LIST *apl, char *name);
char *atlas_point_long_name_named(ATLAS_POINT_LIST *apl, char *name);

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
int invert_brett(ATLAS_XFORM *xf);

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
void atlas_list_to_niml(ATLAS_POINT_LIST *atp, char *atlas_file);
NI_element *atlas_point_to_niml_element(ATLAS_POINT *at_pt);
char *atlas_point_to_niml_string(ATLAS_POINT *at_pt);
int atlas_dup_atlas(ATLAS *srcatlas, ATLAS *destatlas);
int atlas_max_label_length(ATLAS_POINT *ap, int n_points);
int atlas_level(ATLAS_POINT *ap, int n_points);

char *get_jump_space(void);
void set_jump_space(char *spacename);

#endif
