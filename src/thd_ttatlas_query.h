#ifndef _TTATLAS_QUERY_HEADER_
#define _TTATLAS_QUERY_HEADER_

/* for putting strings into strings with #defines */
#define STRINGIFY2( x) #x
#define STRINGIFY(x) STRINGIFY2(x)

/*-----------------------------------------------------------*/
/*----------------- data for Talairach To -------------------*/
/*----------------- Some of that stuff used -----------------*/
/*----------------- be in afni.h. ZSS Feb. 06----------------*/
#define TTO_LMAX    (ATLAS_CMAX+16)
#define TTO_FORMAT  "%." STRINGIFY(ATLAS_CMAX) "s [%3.0f,%3.0f,%3.0f]"
#define IS_BLANK(c) ( ( (c) == ' '  || (c) == '\t' || \
                        (c) == '\n' || (c) == '\v' || \
                        (c) == '\f' || (c) == '\r') ? 1 : 0 )

#define IS_PUNCT(m) (   m=='[' || m==']' || \
                        m=='<' || m=='>' || \
                        m==':' || m==';' || \
                        m=='(' || m==')' || \
                        m=='*' || m==',' || \
                        m=='?') 
      
#define IS_QUOTE(m) (   m=='"' || m=='\'' )

#ifdef MAIN
   /* Table moved to thd_ttatlas_query.c, access is no longer
   restricted to when MAIN is defined */
#else
#endif
extern int atlas_current_structure ;
/*-------------------------------------*/


/*! CA_EZ atlas material is now automatically prepared
from a downloaded SPM toolbox. See the matlab function
CA_EZ_Prep.m */
#include "thd_ttatlas_CA_EZ.h"

/* global web browser, unless we find somewhere better to put it          */
extern char *GLOBAL_browser ;   /* moved from afni.h  22 Feb 2012 [rickr] */


/* generic atlas functions and definitions - 03/13/2009 */
/* #include "thd_atlas.h" */

/*-------- End Atlas Tables ------------*/

#define IS_LETTER(c) ( ( ((c) >= 'A' && (c) <= 'Z') || ((c) >= 'a' && (c) <= 'z') ) ? 1 : 0 )
#define IS_NUMBER(c) ( ( ((c) >= '0' && (c) <= '9') ) ? 1 : 0 )
#define TO_LOWER(c) ( ((c) >= 'A' && (c) <= 'Z') ? (c + 'a' - 'A') : (c) )
#define TO_UPPER(c) ( ((c) >= 'a' && (c) <= 'z') ? (c + 'A' - 'a') : (c) )

#define STR_PRINT(s) ( ((s)) ? (s) : ("NULL") )
#define MIN_PAIR(a,b) ( ((a)<(b)) ? (a) : (b) )
#define COUNTER_SUFFIX(ic)  ( ((ic) == 1) ? "st" : ((ic) == 2) ? "nd" : ((ic) == 3) ? "rd" : "th" )
#define SIDE_MATCH(squery,satlas) ( (squery == satlas || TO_LOWER(satlas) == 'u' || TO_LOWER(squery) == 'u') ? 1:0 )

#define WAMI_HEAD "+++++++ nearby Atlas structures +++++++\n"
#define WAMI_TAIL "\n******** Please use results with caution! ********"  \
                  "\n******** Brain anatomy is quite variable! ********"  \
                  "\n******** The database may contain errors! ********"


typedef struct {
   char side; /*!< u, b, l, or r for: Unknown/Left/Right/Bi */
   char *orig_label;  /*!< label string as provided by atlas */
   char *longname; /*!<  possible long name for region */
   int id; /*!< integer identifier in atlas */
   int N_chnks; /*!< Number of chunks in label, as interpreted by afni */
   char **chnks; /*!< label chunks, as interpreted by afni*/
   char *atlas_name; /*!< Redundant with AFNI_ATLAS's content, but kept
                          in cases where AFNI_ATLAS_REGION are used 
                          separately, such as with ROI_String_Decode.  */
} AFNI_ATLAS_REGION;

typedef struct {
   char *atlas_name;
   int N_regions;
   AFNI_ATLAS_REGION **reg;
} AFNI_ATLAS;

typedef enum { CLASSIC_WAMI_ATLAS_SORT = 1,  TAB1_WAMI_ATLAS_SORT = 2,     TAB2_WAMI_ATLAS_SORT = 3,
               CLASSIC_WAMI_ZONE_SORT,       TAB1_WAMI_ZONE_SORT,          TAB2_WAMI_ZONE_SORT } WAMI_SORT_MODES; 

               
typedef enum { UNKNOWN_ATLAS=0, /*!< Dunno */
               AFNI_TLRC_ATLAS, /*!< The Classic */
               CA_EZ_N27_MPM_ATLAS,  /*!< Eickhoff, Zilles MPM atlas*/
               CA_EZ_N27_ML_ATLAS,  /*!< Eickhoff, Zilles MacroLabels atlas*/
               CA_EZ_N27_PMAPS_ATLAS, /*!< Eickhoff, Zilles Probability maps atlas*/
               CA_EZ_N27_LR_ATLAS,  /*!< Eickhoff, Zilles Left/Right Anatomy atlas*/
               CUSTOM_ATLAS,       /* user specified */
               NUMBER_OF_ATLASES  /*!< A flag for the number of atlases, leave for last */
               } AFNI_ATLAS_CODES;


typedef struct {
   int *iloc;
   float *score;
   int N;
   int nmatch;
}  ATLAS_SEARCH;

typedef struct {
   float x, y, z; /*!< coordinates */
   char space_name[65]; /*!< Name of coordinate space, will supersede space */
   char orcode[4]; /*!< The signs and labels of x y z axis (4th for nil char)*/ 
} ATLAS_COORD;

typedef struct {
   int N_label; /*!< number of label types available in a particular atlas. 
                     For example the "Anterior Cingulate" can coincide with "Brodmann area 25" in TT_Daemon atlas.
                     In this case, N_label = 2 */
   int level; /*!< a number used to group zones together. This can be equal to the 'within' radius ... */
   char **label;  /*!< labels of the zone. label[0] = "Anterior Cingulate", label[1] = "Brodmann area 25" */
   int   *code;  /*!< Integer code of zone in atlas */
   char  **atname; /*!< Integer code of atlas */
   float *prob; /*!< probability, if applicable, of being of a particular label */
   float *radius;   /*!< distance, search distance for reported label.*/
   char **longname; /*!< long name for label/atlas region */
   char **webpage; /*!< webpages for a web-atlas for whereami location */
   char **connpage; /*!< connection info webpages for a web-atlas for whereami location */
} ATLAS_ZONE;

typedef struct {
   int N_zone;      /*!< number of zones found */
   ATLAS_ZONE **zone; /*!< the zones */
} ATLAS_QUERY;

typedef struct {
   int n_points;
   ATLAS_POINT *at_point;
} ATLAS_POINT_LIST;

#define MAX_ELM(apl2) ((apl2) ? (apl2)->n_points:0)

typedef struct {
   THD_3dim_dataset *adset; 
   int mxlablen;
   int probkey;
   byte *lrmask;            /* Do not free this one either */
   int maxkeyval;        /* Highest integral value in dset */
   int minkeyval;        /* Lowest integral value in dset */
   ATLAS_POINT_LIST *apl2;  /* use new list structure for segmentation 
                              At the moment, apl2 is also filled for
                              probabilistic atlases because it is needed
                              to go from sub-brick label to area name. */
   byte duplicateLRentries; /* Are LR labels listed in adh.apl and 
                               under the same code?
                               (only case I know of is in TTO_list) */
   byte build_lr;
   int params_set;
} ATLAS_DSET_HOLDER;

typedef struct {
   char *xform_type, *xform_name, *source, *dest, *coord_order;
   float dist;   /* distance (cost) of xform between two spaces */
   int inverse;  /* inverse transformation from dest to src */
   int post;  /* for 2/12 part, evaluate coords pre/post xformation */
   int nelts;    /* number of data elements */
   void *xform;  /* data for xformation */
} ATLAS_XFORM;

typedef struct {
   char *dset_name;
   char *space;
   char *name;
   char *description;
   char *comment;
   char *atlas_type;  /* web or NULL for now, for web type, dset is http webpage address */
   char *orient;  /* string to specify xyz order requests - Elsevier's web version uses "RSA"*/
   char *supp_web_info; /* string specifying base webpage for supplemental structure info */
   char *supp_web_type; /* extension suffix for webpages - pdf, html,... */
   char *supp_conn_info; /* string specifying base webpage for supplemental connection info */
   char *supp_conn_type; /* extension suffix for webpages - pdf, html,... */
   int atlas_found;
   ATLAS_DSET_HOLDER *adh;
} ATLAS; /*!< All char * should be initialized when .niml file is loaded,
               or when ATLAS_LIST * is formed in the old syle.
              *adh is initialized to zero, then populated if needed
              by the function Atlas_With_Trimming. The latter should
              be used almost exclusively to get an atlas */

/* macro accessors for the atlas fields - first version is to pointer location, 
   second _S version is for default string if NULL string in structure */
#define ATL_COMMENT(xa) ( ( (xa) && (xa)->comment) ?   \
                           (xa)->comment : NULL )
#define ATL_COMMENT_S(xa) ( (ATL_COMMENT(xa)) ? \
                              (ATL_COMMENT(xa)) : "None" )

#define ATL_DESCRIPTION(xa) ( ( (xa) && (xa)->description ) ?   \
                           (xa)->description : NULL )
#define ATL_DESCRIPTION_S(xa) ( (ATL_DESCRIPTION(xa)) ? \
                                 (ATL_DESCRIPTION(xa)) : "None" )

#define ATL_NAME(xa) ( ( (xa) && (xa)->name) ?   \
                           (xa)->name : NULL )
#define ATL_NAME_S(xa) ( (ATL_NAME(xa)) ? \
                                 (ATL_NAME(xa)) : "None" )
                                 
#define ATL_DSET(xa) ( ( (xa) && (xa)->adh ) ? \
                        (xa)->adh->adset : NULL )

#define ATL_ADH_SET(xa) ( ( (xa) && (xa)->adh ) ? \
                           (xa)->adh->params_set : 0 )                            

#define ATL_ORIENT(xa) ( ( (xa) && (xa)->orient) ?   \
                           (xa)->orient : NULL )
#define ATL_ORIENT_S(xa) ( (ATL_ORIENT(xa)) ? \
                              (ATL_ORIENT(xa)) : "RAI" )

#define ATL_TYPE(xa) ( ( (xa) && (xa)->atlas_type) ?   \
                           (xa)->atlas_type : NULL )
#define ATL_TYPE_S(xa) ( (ATL_TYPE(xa)) ? \
                              (xa)->atlas_type : "None" )
/* is the atlas a web type */
#define ATL_WEB_TYPE(xa) (strcasecmp((ATL_TYPE_S(xa)),"web")== 0)

/* is there supplemental information on Internet */
#define ATL_SUPP_WEB_INFO(xa) ((xa) && (xa)->supp_web_info ? 1: 0)
#define ATL_SUPP_WEB_INFO_S(xa) ( (ATL_SUPP_WEB_INFO(xa)) ? \
          ((xa)->supp_web_info)   : "No supplemental info " )
#define ATL_SUPP_WEB_TYPE(xa) ((xa)->supp_web_type ? 1: 0)
#define ATL_SUPP_WEB_TYPE_S(xa) ( (ATL_SUPP_WEB_TYPE(xa)) ? \
          ((xa)->supp_web_type)   : ".html" )

#define ATL_SUPP_CONN_INFO(xa) ((xa) && (xa)->supp_conn_info ? 1: 0)
#define ATL_SUPP_CONN_INFO_S(xa) ( (ATL_SUPP_CONN_INFO(xa)) ? \
          ((xa)->supp_conn_info)   : "No supplemental info " )
#define ATL_SUPP_CONN_TYPE(xa) ((xa)->supp_conn_type ? 1: 0)
#define ATL_SUPP_CONN_TYPE_S(xa) ( (ATL_SUPP_CONN_TYPE(xa)) ? \
          ((xa)->supp_conn_type)   : ".html" )

#define ATL_FOUND(xa) ( (xa)  ? \
                           ((xa)->atlas_found) : 0 )


/* number of atlases in an atlas list */
#define NUM_ATLASES(atl) ( (atl) ? \
                           (atl)->natlases : 0)

#define ATLAS_IN_LIST(atl, i) ( (atl) ? \
                           (atl)->atlas+i : 0)

typedef struct {
   char *atlas_space;
   char *generic_space;
} ATLAS_SPACE;

typedef struct {
   char *template;
   char *space;
   char *description;
   char *comment;
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

typedef enum { LEV=0, /* Levenshtein distance */
               FLD, /* Length Difference */
               FCD, /* Number of characters from s2 found in s1*/
               PMD, /* partial match depth */
               MWI, /* Matching word index (in line of words) */
               MWL, /* Line in multi-line text of matching word */
               IWD, /* Intra Words Distance */ 
               N_APPROX_STR_DIMS /* leave the last */ } APPROX_STR_DIMS;

#define SRCFILE_MAX 32

typedef struct {
   int d[N_APPROX_STR_DIMS];
   char srcfile[SRCFILE_MAX+1]; 
} APPROX_STR_DIFF;

typedef struct {
   float w[N_APPROX_STR_DIMS]; 
} APPROX_STR_DIFF_WEIGHTS;


#define WAMI_WEB_PRINT_XML    1
#define WAMI_WEB_BROWSER      2
#define WAMI_WEB_STRUCT       3

#define MAX_URL 2048

const char *Atlas_Val_Key_to_Val_Name(ATLAS *atlas, int tdval);
int Init_Whereami_Max_Find(void);
void Set_Whereami_Max_Find(int n);
float Init_Whereami_Max_Rad(void);
void Set_Whereami_Max_Rad(float n);
THD_3dim_dataset * get_atlas(char *epath, char *aname) ;
char * get_atlas_dirname(void) ; /* 31 Jan 2008 -- RWCox */
char Is_Side_Label(char *str, char *opt);
int qmode_int(int *iv, int ni);
int *z_rand_order(int bot, int top, long int seed);
int *z_iqsort (float *x , int nx );
int *z_idoubleqsort (double *x , int nx );
int *z_idqsort (int *x , int nx );
int *z_dqsort (int *x , int nx );
int *z_istrqsort (char **x , int nx );
void Show_Atlas_Region (AFNI_ATLAS_REGION *aar);
AFNI_ATLAS_REGION * Free_Atlas_Region (AFNI_ATLAS_REGION *aar);
AFNI_ATLAS_REGION * Atlas_Chunk_Label(char *lbli, int id, char *aname, char *longname);
AFNI_ATLAS *Build_Atlas (char *aname, ATLAS_LIST *atlas_list) ;
void Show_Atlas (AFNI_ATLAS *aa);
AFNI_ATLAS *Free_Atlas(AFNI_ATLAS *aa) ;
AFNI_ATLAS_REGION *ROI_String_Decode(char *str, ATLAS_LIST *atlas_list);
char * deblank_name(char *name);
char *deslash_allname(char *name, char fill);
char *deblank_allname(char *name, char fill);
char *cdeblank_allname(char *name, char fill);
char * depunct_name(char *name);
char * dequote_name(char *name, char qo);
int begins_with(char *name, char *quote, int debl);
int ends_with(char *name, char *quote, int debl);
APPROX_STR_DIFF_WEIGHTS *init_str_diff_weights(APPROX_STR_DIFF_WEIGHTS *Dwi);
float best_approx_str_match(char **words, int N_words, char *str, byte ci,
                           APPROX_STR_DIFF_WEIGHTS *Dwi);
char **approx_str_sort(char **words, int N_words, char *str, byte ci, 
                       float **sorted_score, byte word_split,
                       APPROX_STR_DIFF_WEIGHTS *Dwi,
                       APPROX_STR_DIFF **Dout);
char **approx_str_sort_text(char *text, int *N_ws, char *str, 
                            byte ci, float **sorted_score,
                            APPROX_STR_DIFF_WEIGHTS *Dwi,
                            APPROX_STR_DIFF **Dout, char join_breaks);      
char **approx_str_sort_tfile(char *fname, int textinname, int *N_ws, char *str, 
                            byte ci, float **sorted_score,
                            APPROX_STR_DIFF_WEIGHTS *Dwi,
                            APPROX_STR_DIFF **Dout, int verb, char join_breaks);
THD_string_array *approx_str_sort_Ntfile(
                      char **fnames, int N_names, char *str, 
                      byte ci, float **sorted_score,
                      APPROX_STR_DIFF_WEIGHTS *Dwi,
                      APPROX_STR_DIFF **Doutp, int verb, char join_breaks);
#define APSEARCH_TMP_PREF "__apsearch"
char **approx_str_sort_phelp(char *prog, int textinname, int *N_ws, char *str, 
                            byte ci, float **sorted_score,
                            APPROX_STR_DIFF_WEIGHTS *Dwi,
                            APPROX_STR_DIFF **Dout, int verb, char join_breaks);
char **approx_str_sort_all_popts(char *prog, int textinname, int *N_ws, 
                            byte ci, float **sorted_score,
                            APPROX_STR_DIFF_WEIGHTS *Dwi,
                            APPROX_STR_DIFF **Dout,
                            int uopts, int verb, char join_breaks);
char *get_updated_help_file(int force_recreate, byte verb, char *progname, 
                            int shtp);
char **approx_str_sort_readmes(char *str, int *N_r);
char **unique_str(char **words, int N_words, byte ci, 
                  byte noae, int *N_unq, int **isort_out);
char *approx_string_diff_info(APPROX_STR_DIFF *D,APPROX_STR_DIFF_WEIGHTS *Dwi); 
ATLAS_SEARCH *Find_Atlas_Regions(AFNI_ATLAS *aa, AFNI_ATLAS_REGION *ur , 
                                 ATLAS_SEARCH *usethissearch);
ATLAS_SEARCH *Free_Atlas_Search(ATLAS_SEARCH *as);
char *Report_Found_Regions(AFNI_ATLAS *aa, AFNI_ATLAS_REGION *ur , ATLAS_SEARCH *as, int *nexact);
char * Clean_Atlas_Label( char *lb);
char * Clean_Atlas_Label_to_Prefix( char *lb);
ATLAS_ZONE *Get_Atlas_Zone(ATLAS_QUERY *aq, int level);
ATLAS_ZONE *Atlas_Zone(ATLAS_ZONE *zn, int level, char *label, int code, 
                       float prob, float within, char *aname, char *webpage, char *connpage) ;
ATLAS_ZONE *Free_Atlas_Zone(ATLAS_ZONE *zn);
void Set_Show_Atlas_Mode(int md);
void Show_Atlas_Zone(ATLAS_ZONE *zn, ATLAS_LIST *atlas_list);
void Show_Atlas_Query(ATLAS_QUERY *aq, ATLAS_LIST *atlas_list);
ATLAS_QUERY *Add_To_Atlas_Query(ATLAS_QUERY *aq, ATLAS_ZONE *zn);
ATLAS_QUERY *Free_Atlas_Query(ATLAS_QUERY *aq);
int CA_EZ_LR_load_atlas_old(void);
int CA_EZ_ML_load_atlas_old(void);
int CA_EZ_MPM_load_atlas_old(void);
int CA_EZ_PMaps_load_atlas_old(void);
THD_3dim_dataset *load_atlas_dset(char *dsetname);
void CA_EZ_MPM_purge_atlas(void);
void CA_EZ_PMaps_purge_atlas(void);
void CA_EZ_ML_purge_atlas(void);
char * Atlas_Query_to_String (ATLAS_QUERY *wami, ATLAS_COORD ac,
                              WAMI_SORT_MODES Mode, ATLAS_LIST *atlas_list);
char * genx_Atlas_Query_to_String (ATLAS_QUERY *wami,
                              ATLAS_COORD ac, WAMI_SORT_MODES mode,
                              ATLAS_LIST *atlas_list);
char MNI_Anatomical_Side(ATLAS_COORD ac, ATLAS_LIST *atlas_list);
void TT_whereami_set_outmode(WAMI_SORT_MODES md);
void set_TT_whereami_version(int atlas_list_version, int whereami_version);
char * Atlas_Prob_String(float p);
char * Atlas_Code_String(int c);
byte Same_Chunks(AFNI_ATLAS_REGION *aar1, AFNI_ATLAS_REGION *aar2);
THD_3dim_dataset *Atlas_Region_Mask(AFNI_ATLAS_REGION *aar, 
                                    int *codes, int n_codes,
                                    ATLAS_LIST *atlas_list);
char Atlas_Voxel_Side( THD_3dim_dataset *dset, int k1d, byte *lrmask);
void TT_whereami_remove_atlas(AFNI_ATLAS_CODES ac);
void TT_whereami_add_atlas(AFNI_ATLAS_CODES ac);
THD_3dim_dataset *THD_3dim_G_from_ROIstring(char *shar);
THD_3dim_dataset *THD_3dim_from_ROIstring(char *shar, ATLAS_LIST *atlas_list);
void Set_ROI_String_Decode_Verbosity(byte lvl);
int * UniqueInt (int *y, int ysz, int *kunq, int Sorted );
short * UniqueShort (short *y, int ysz, int *kunq, int Sorted );
byte * UniqueByte (byte *y, int ysz, int *kunq, int Sorted );

ATLAS *Atlas_With_Trimming(char *atname, int LoadLRMask, 
                                       ATLAS_LIST *atlas_list);
int whereami_in_atlas(  char *aname, 
                        ATLAS_COORD ac, 
                        ATLAS_QUERY **wamip);
char *atlas_key_label(ATLAS *atlas, int key, ATLAS_COORD *ac);
char *Atlas_name_choice(ATLAS_POINT *atp);
char *prob_atlas_sb_to_label(ATLAS *atlas, int sb, int *key);
byte is_probabilistic_atlas(ATLAS *atlas);
byte is_integral_atlas(ATLAS *atlas);
byte is_atlas_key_labeled(ATLAS *atlas, int key);
int whereami_3rdBase( ATLAS_COORD aci, ATLAS_QUERY **wamip,
                      ATLAS_SPACE_LIST *asli, ATLAS_LIST *aali);
int XYZ_to_AtlasCoord(float x, float y, float z, char *orcode, 
                              char *spacename, ATLAS_COORD*ac);
/* change these to stop using the term code */
char *Atlas_Code_to_Atlas_Description(AFNI_ATLAS_CODES icod);
char *Atlas_Code_to_Atlas_Name (AFNI_ATLAS_CODES cod);
int init_global_atlas_list (void);
ATLAS *get_Atlas_Named(char *atname, ATLAS_LIST *atlas_list);
char *suggest_Atlas_Named(char *atname, ATLAS_LIST *atlas_list);
ATLAS *get_Atlas_ByDsetID(char *dsetid, ATLAS_LIST *atlas_list);
ATLAS_LIST *Atlas_Names_to_List(char **atnames, int natlases);
char **free_names_list(char **nl, int N_nl);
int find_in_names_list(char **nl, int N_nl, char *name);
char **add_to_names_list(char **nl, int *N_nl, char *name);
int set_adh_old_way(ATLAS_DSET_HOLDER *adh, char *aname);
int find_coords_in_space(ATLAS_COORD *acl, int N_acl, char *space_name); 
int transform_atlas_coords(ATLAS_COORD ac, char **out_spaces, 
                           int N_out_spaces, ATLAS_COORD *acl, char *orcodeout); 
int wami_xform_xyz(float xi,float yi,float zi,
   float *xout, float *yout, float *zout,
   char *srcspace, char *destspace);
int wami_xform_coords_print(float *coords, int ncoords,
   char *srcspace, char *destspace, char *outfile);
void set_atlas_name_code(int code);

void set_wami_verb(int lev);
int wami_verb(void);
int wami_lh(void);
int Init_Atlas_Dset_Holder(ATLAS *atlas) ;
ATLAS_DSET_HOLDER *Free_Atlas_Dset_Holder(ATLAS_DSET_HOLDER *adh);
int is_small_TT(ATLAS *atlas);
int is_big_TT(ATLAS *atlas);
char * TT_whereami_default_spc_name (void);
int is_Dset_Space_Named(THD_3dim_dataset *dset, char *name);
int is_Dset_Atlasy(THD_3dim_dataset *dset, ATLAS_LIST *atlas_alist);
char *gen_space_str(char *space_str);
int equivalent_space(char *inspace_str);
char *get_out_space(void);
void set_out_space(char *space_str);

char **atlas_reference_string_list(char *atname, int *N_refs);
char *atlas_version_string(char *atname);
ATLAS_POINT_LIST *atlas_point_list(char *atname);
ATLAS_POINT_LIST *atlas_point_list_old_way(char *atname);
int genx_load_atlas_dset(ATLAS *atlas);
int purge_atlas(char *atname);
THD_string_array *get_working_atlas_name_list(void);
THD_string_array *recreate_working_atlas_name_list(void);
ATLAS_SPACE_LIST *get_G_space_list(void);
ATLAS_XFORM_LIST *get_G_xform_list(void);
ATLAS_LIST* get_G_atlas_list(void);
ATLAS_TEMPLATE_LIST *get_G_templates_list(void);
char *find_atlas_niml_file(char * nimlname, int nini);
char * get_env_atlas_path(void);
ATLAS_LIST *env_atlas_list(void);
char **env_space_list(int *);
int env_dec_places(void);
char * search_quotes(char *in_str);

char *Current_Atlas_Default_Name(void);
char **Atlas_Names_List(ATLAS_LIST *atl);
int AFNI_get_dset_val_label(THD_3dim_dataset *dset, double val, char *str);  /* 26 Feb 2010 ZSS */
int AFNI_get_dset_label_val(THD_3dim_dataset *dset, double *val, char *str); /* 02 Nov 2010 ZSS */
int AFNI_get_dset_label_ival(THD_3dim_dataset *dset, int *val, char *str);   /* 30 Nov 2016 [rickr] */
int thd_LT_label_to_int_list(THD_3dim_dataset *dset,int_list *ilist,char *str); /* 22 Nov 2016 [rickr] */
int known_atlas_label_to_int_list(int_list * ilist, char * str);


char *elsevier_query(float xx, float yy, float zz, ATLAS *atlas);
char *elsevier_query_request(float xx, float yy, float zz, ATLAS *atlas, int el_req_type);
void wami_query_web(ATLAS *atlas, ATLAS_COORD ac, ATLAS_QUERY *wami);

char * whereami_XML_get(char *data, char *name, char **next);
int whereami_browser(char *url);
char *cleanup_url(char *url);
void set_wami_web_found(int found);
int get_wami_web_found(void);
void set_wami_web_reqtype(int reqtype);
int get_wami_web_reqtype(void);
void set_wami_webpage(char *url);
char * get_wami_webpage(void);
void open_wami_webpage(void);
int AFNI_wami_output_mode(void);
void set_AFNI_wami_output_mode(int webflag);
char * atlas_suppinfo_webpage(ATLAS *atlas, char *blab);
char * atlas_suppinfo_connpage(ATLAS *atlas, char *blab);

size_t CURL_read_URL_http ( char *url, char **data);
void set_wami_minprob(float val);
float get_wami_minprob(void);
float Get_PMap_Factor(void);
int show_neurosynth_link();
int show_linkrbrain_link();
int show_sumsdb_link();
char * neurosynth_coords_link(float x, float y, float z);
char * sumsdb_coords_link(float x, float y, float z);
int make_linkrbrain_xml(float *coords, int ncoords, 
    char *src_space, char *dest_space, char *linkrbrain_xml, int linkr_corr_type);
int send_linkrbrain_xml(char *linkrbrain_xml, char *linkrbrain_results);

/* Transforms for going from one space to another */
#if 0
static char MNI_N27_to_AFNI_TLRC_HEAD[256] = {"TT_N27+tlrc"}; /*!<  TT_N27+tlrc was obtained by transforming N27 from MNI 
                                                    space to AFNI's Talairach space (manual transformation, 12 piece-wise-linear xforms) 
                                                    N27 was taken from Zilles' v12 database (colin_seg.hdr) before it got changed
                                                    to MNI anatomical (by simple shift) in v13 database*/
#endif
static char TT_DAEMON_TT_PREFIX[256] = {"TTatlas"}; /*!< Good old tlrc daemon */
static char CA_EZ_N27_MPM_TT_PREFIX[256] = {"TT_N27_CA_EZ_MPM"};   /*!< Prefix of the Zilles Eickhoff Maximum Probability Maps in TT space */
static char CA_EZ_N27_PMaps_TT_PREFIX[256] = {"TT_N27_CA_EZ_PMaps"};    /*!< Prefix of the Zilles Eickhoff Probability Maps in TT space */
static char CA_EZ_N27_ML_TT_PREFIX[256] = {"TT_N27_EZ_ML"};  /*!< Prefix of the Zilles Eickhoff Macro Labels in TT space */
static char CA_EZ_N27_LR_TT_PREFIX[256] = {"TT_N27_EZ_LR"};   /*!< Prefix of the Zilles Eickhoff Left/Right mask dset */
static char CUSTOM_ATLAS_PREFIX[256] = {"?CUSTOM?"}; /* default prefix of additional custom atlas */
/* static char CUSTOM_ATLAS_PREFIX[256] = {"TTatlas_2010_master"};*/ /* default prefix of additional custom atlas */


static float MNI_N27_to_AFNI_TLRC_WRP_VEC[360] = {       
      0.9444444,              0,              0,              0,      0.9976303,
     0.06880209,              0,    -0.05989829,      0.8685252,       1.058824,
             -0,              0,             -0,      0.9976304,    -0.07902943,
             -0,      0.0688021,       1.145927,              0,      -3.405704,
      -4.103033,             -0,       3.073373,       4.936095,          -9999,
          -9999,              0,              0,              0,         9999.9,
      0.9315069,              0,              0,              0,      0.9976303,
     0.06880209,              0,    -0.05989829,      0.8685252,       1.073529,
             -0,              0,             -0,      0.9976304,    -0.07902943,
             -0,      0.0688021,       1.145927,              0,      -3.405704,
      -4.103033,             -0,       3.073373,       4.936095,              0,
          -9999,              0,         9999.9,              0,         9999.9,
      0.9444444,              0,              0,              0,      0.8154033,
     0.05623471,              0,    -0.05989829,      0.8685252,       1.058824,
             -0,              0,             -0,       1.220581,    -0.07902943,
             -0,     0.08417804,       1.145927,              0,      -2.783618,
      -4.103033,             -0,       3.073372,       4.936095,          -9999,
              0,              0,              0,             23,         9999.9,
      0.9315069,              0,              0,              0,      0.8154033,
     0.05623471,              0,    -0.05989829,      0.8685252,       1.073529,
             -0,              0,             -0,       1.220582,    -0.07902943,
             -0,     0.08417805,       1.145927,              0,      -2.783618,
      -4.103033,             -0,       3.073373,       4.936095,              0,
              0,              0,         9999.9,             23,         9999.9,
      0.9444444,              0,              0,              0,      0.9511568,
     0.06559702,              0,    -0.05989829,      0.8685252,       1.058824,
             -0,              0,             -0,       1.046375,    -0.07902944,
             -0,     0.07216377,       1.145927,              0,      0.5821307,
      -4.103033,             -0,     -0.9333872,       4.659767,          -9999,
             23,              0,              0,           9999,         9999.9,
      0.9315069,              0,              0,              0,      0.9511568,
     0.06559702,              0,    -0.05989829,      0.8685252,       1.073529,
             -0,              0,             -0,       1.046375,    -0.07902943,
             -0,     0.07216376,       1.145927,              0,      0.5821307,
      -4.103033,             -0,     -0.9333871,       4.659767,              0,
             23,              0,         9999.9,           9999,         9999.9,
      0.9444444,              0,              0,              0,      0.9976303,
     0.06880209,              0,    -0.06148272,      0.8914994,       1.058824,
             -0,              0,             -0,      0.9976304,    -0.07699282,
             -0,      0.0688021,       1.116396,              0,      -3.405704,
      -4.211566,             -0,       3.073373,       4.936095,          -9999,
          -9999,          -9999,              0,              0,              0,
      0.9315069,              0,              0,              0,      0.9976303,
     0.06880209,              0,    -0.06148272,      0.8914994,       1.073529,
             -0,              0,             -0,      0.9976304,    -0.07699282,
             -0,      0.0688021,       1.116396,              0,      -3.405704,
      -4.211566,             -0,       3.073373,       4.936095,              0,
          -9999,          -9999,         9999.9,              0,              0,
      0.9444444,              0,              0,              0,      0.8154033,
     0.05623471,              0,    -0.06148272,      0.8914994,       1.058823,
             -0,              0,             -0,       1.220582,    -0.07699282,
             -0,     0.08417804,       1.116396,              0,      -2.783618,
      -4.211566,             -0,       3.073373,       4.936095,          -9999,
              0,          -9999,              0,             23,              0,
      0.9315069,              0,              0,              0,      0.8154033,
     0.05623471,              0,    -0.06148272,      0.8914994,       1.073529,
             -0,              0,             -0,       1.220582,    -0.07699282,
             -0,     0.08417805,       1.116396,              0,      -2.783618,
      -4.211566,             -0,       3.073373,       4.936095,              0,
              0,          -9999,         9999.9,             23,              0,
      0.9444444,              0,              0,              0,      0.9511568,
     0.06559702,              0,    -0.06148272,      0.8914994,       1.058824,
             -0,              0,             -0,       1.046375,    -0.07699282,
             -0,     0.07216378,       1.116396,              0,      0.5821307,
      -4.211566,             -0,     -0.9333872,       4.659767,          -9999,
             23,          -9999,              0,           9999,              0,
      0.9315069,              0,              0,              0,      0.9511568,
     0.06559702,              0,    -0.06148272,      0.8914994,       1.073529,
             -0,              0,             -0,       1.046375,    -0.07699282,
             -0,     0.07216377,       1.116396,              0,      0.5821307,
      -4.211566,             -0,     -0.9333872,       4.659767,              0,
             23,          -9999,         9999.9,           9999,              0 }; /*!< Taken from TT_N27+tlrc which was obtained by transforming 
                                                                                    N27 from MNI space to AFNI's Talairach space (manual transformation, 
                                                                                    12 piece-wise-linear xforms) */
             
THD_fvec3 THD_mni_to_tta_N27( THD_fvec3 mv );
THD_fvec3 THD_tta_to_mni_N27( THD_fvec3 mv );
THD_fvec3 THD_mnia_to_tta_N27( THD_fvec3 mv );
THD_fvec3 THD_tta_to_mnia_N27( THD_fvec3 mv );

#endif
