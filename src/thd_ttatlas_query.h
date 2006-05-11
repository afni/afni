#ifndef _TTATLAS_QUERY_HEADER_
#define _TTATLAS_QUERY_HEADER_


/*-----------------------------------------------------------*/
/*----------------- data for Talairach To -------------------*/
/*----------------- Some of that stuff used -----------------*/
/*----------------- be in afni.h. ZSS Feb. 06----------------*/
#define ATLAS_CMAX    64   /* If you change this parameter,edit constant in CA_EZ_Prep.m (MaxLbl* checks) */
#define TTO_LMAX    (ATLAS_CMAX+16)
#define TTO_FORMAT  "%s [%3d,%3d,%3d]"

typedef struct {
   short tdval;         /* Leave this one to be the very first element */
   char name[ATLAS_CMAX] ;  /* Leave this one to be the second element */  
   short xx,yy,zz,tdlev ;
   char dsetpref[ATLAS_CMAX];
} ATLAS_point ;

#define TTO_COUNT 241

#define TTO_COUNT_BROD    209
#define TTO_COUNT_NONBROD 125

#ifdef MAIN
   /* Table moved to thd_ttatlas_query.c, access is no longer
   restricted to when MAIN is defined */
#else
#endif
extern ATLAS_point TTO_list[TTO_COUNT] ;
extern char * TTO_labels[TTO_COUNT] ;
extern int TTO_labeled ;
extern int TTO_current ;
/*-------------------------------------*/


/*! CA_EZ atlas material is now automatically prepared
from a downloaded SPM toolbox. See the matlab function
CA_EZ_Prep.m */
#include "thd_ttatlas_CA_EZ.h"

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
   int id; /*!< integer identifier in atlas */
   int N_chnks; /*!< Number of chunks in label, as interpreted by afni */
   char **chnks; /*!< label chunks, as interpreted by afni*/
} AFNI_ATLAS_REGION;

typedef struct {
   char *AtlasLabel;
   int N_regions;
   AFNI_ATLAS_REGION **reg;
} AFNI_ATLAS;

typedef enum { CLASSIC_WAMI_ATLAS_SORT = 1,  TAB1_WAMI_ATLAS_SORT = 2,     TAB2_WAMI_ATLAS_SORT = 3,
               CLASSIC_WAMI_ZONE_SORT,       TAB1_WAMI_ZONE_SORT,          TAB2_WAMI_ZONE_SORT } WAMI_SORT_MODES; 

typedef enum { UNKNOWN_SPC=0, /*!< Dunno */
               AFNI_TLRC_SPC, /*!< The Classic */
               MNI_SPC,       /*!< A la Canadienne */ 
               MNI_ANAT_SPC,  /*!< Mit viele liebe */
                 
               NUMBER_OF_SPC  /*!< A flag for the number of spaces, leave for last */
               } AFNI_STD_SPACES;
               
typedef enum { UNKNOWN_ATLAS=0, /*!< Dunno */
               AFNI_TLRC_ATLAS, /*!< The Classic */
               CA_EZ_N27_MPM_ATLAS,  /*!< Eickhoff, Zilles MPM atlas*/
               CA_EZ_N27_ML_ATLAS,  /*!< Eickhoff, Zilles MacroLabels atlas*/
               CA_EZ_N27_PMAPS_ATLAS, /*!< Eickhoff, Zilles Probability maps atlas*/
               CA_EZ_N27_LR_ATLAS,  /*!< Eickhoff, Zilles Left/Right Anatomy atlas*/
               
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
   AFNI_STD_SPACES space; /*!< type of coordinate space */
} ATLAS_COORD;

typedef struct {
   int N_label; /*!< number of label types available in a particular atlas. 
                     For example the "Anterior Cingulate" can coincide with "Brodmann area 25" in TT_Daemon atlas.
                     In this case, N_label = 2 */
   int level; /*!< a number used to group zones together. This can be equal to the 'within' radius ... */
   char **label;  /*!< labels of the zone. label[0] = "Anterior Cingulate", label[1] = "Brodmann area 25" */
   int   *code;  /*!< Integer code of zone in atlas */
   int   *atcode; /*!< Integer code of atlas */
   float *prob; /*!< probability, if applicable, of being of a particular label */
   float *radius;   /*!< distance, search distance for reported label.*/ 
} ATLAS_ZONE;

typedef struct {
   
   int N_zone;      /*!< number of zones found */
   ATLAS_ZONE **zone; /*!< the zones */
} ATLAS_QUERY;

typedef struct {
   THD_3dim_dataset *dset; /* This is a copy of static atlas pointers. Do NOT Free! */
   AFNI_ATLAS_CODES atcode;
   int mxlablen;
   int mxelm;
   float probkey;
   byte *lrmask;        /* Do not free this one either */
   int maxindexcode; /*!< Highest integral value in dset */
   ATLAS_point *apl; /*!< Atlas point list, no free baby*/
   byte duplicateLRentries; /*!< Are LR labels listed in adh.apl and under the same code? (only case I know of is in TTO_list*/
} ATLAS_DSET_HOLDER;

const char *Atlas_Val_to_Atlas_Name(ATLAS_DSET_HOLDER adh, int tdval);
int Init_Whereami_Max_Find(void);
void Set_Whereami_Max_Find(int n);
float Init_Whereami_Max_Rad(void);
void Set_Whereami_Max_Rad(float n);
THD_3dim_dataset * get_altas(char *epath, char *aname) ;
char Is_Side_Label(char *str, char *opt);
int *z_iqsort (float *x , int nx );
int *z_idqsort (int *x , int nx );
void Show_Atlas_Region (AFNI_ATLAS_REGION *aar);
AFNI_ATLAS_REGION * Free_Atlas_Region (AFNI_ATLAS_REGION *aar);
AFNI_ATLAS_REGION * Atlas_Chunk_Label(char *lbli, int id);
AFNI_ATLAS *Build_Atlas (AFNI_ATLAS_CODES ac) ;
void Show_Atlas (AFNI_ATLAS *aa);
AFNI_ATLAS *Free_Atlas(AFNI_ATLAS *aa) ;
AFNI_ATLAS_REGION *ROI_String_Decode(char *str, AFNI_ATLAS_CODES *ac);
ATLAS_SEARCH *Find_Atlas_Regions(AFNI_ATLAS *aa, AFNI_ATLAS_REGION *ur , ATLAS_SEARCH *usethissearch);
ATLAS_SEARCH *Free_Atlas_Search(ATLAS_SEARCH *as);
char *Report_Found_Regions(AFNI_ATLAS *aa, AFNI_ATLAS_REGION *ur , ATLAS_SEARCH *as, int *nexact);
char * Clean_Atlas_Label( char *lb);
char * Clean_Atlas_Label_to_Prefix( char *lb);
const char *Space_Code_to_Space_Name (AFNI_STD_SPACES cod);
const char *Atlas_Code_to_Atlas_Name (AFNI_ATLAS_CODES cod);
const char *Atlas_Code_to_Atlas_Dset_Name (AFNI_ATLAS_CODES cod);
const char *Atlas_Code_to_Atlas_Description (AFNI_ATLAS_CODES cod);
AFNI_ATLAS_CODES Atlas_Name_to_Atlas_Code (char *name);
ATLAS_ZONE *Get_Atlas_Zone(ATLAS_QUERY *aq, int level);
ATLAS_ZONE *Atlas_Zone(ATLAS_ZONE *zn, int level, char *label, int code, float prob, float within, AFNI_ATLAS_CODES atcode) ;
ATLAS_ZONE *Free_Atlas_Zone(ATLAS_ZONE *zn);
void Set_Show_Atlas_Mode(int md);
void Show_Atlas_Zone(ATLAS_ZONE *zn);
void Show_Atlas_Query(ATLAS_QUERY *aq);
ATLAS_QUERY *Add_To_Atlas_Query(ATLAS_QUERY *aq, ATLAS_ZONE *zn);
ATLAS_QUERY *Free_Atlas_Query(ATLAS_QUERY *aq);
int CA_EZ_ML_load_atlas(void);
int CA_EZ_MPM_load_atlas(void);
int CA_EZ_PMaps_load_atlas(void);
void CA_EZ_MPM_purge_atlas(void);
void CA_EZ_PMaps_purge_atlas(void);
void CA_EZ_ML_purge_atlas(void);
char *whereami_9yards(ATLAS_COORD ac, ATLAS_QUERY **wamip, AFNI_ATLAS_CODES *atlaslist, int N_atlaslist);
char * Atlas_Query_to_String (ATLAS_QUERY *wami, ATLAS_COORD ac, WAMI_SORT_MODES Mode);
char MNI_Anatomical_Side(ATLAS_COORD ac);
void TT_whereami_set_outmode(WAMI_SORT_MODES md);
char * Atlas_Prob_String(float p);
char * Atlas_Code_String(int c);
byte Same_Chunks(AFNI_ATLAS_REGION *aar1, AFNI_ATLAS_REGION *aar2);
THD_3dim_dataset *Atlas_Region_Mask(AFNI_ATLAS_CODES ac, AFNI_ATLAS_REGION *aar, int *codes, int n_codes);
char Atlas_Voxel_Side( THD_3dim_dataset *dset, int k1d, byte *lrmask);
void TT_whereami_remove_atlas(AFNI_ATLAS_CODES ac);
void TT_whereami_add_atlas(AFNI_ATLAS_CODES ac);
THD_3dim_dataset *THD_3dim_from_ROIstring(char *shar);
void Set_ROI_String_Decode_Verbosity(byte lvl);
int * UniqueInt (int *y, int ysz, int *kunq, int Sorted );
short * UniqueShort (short *y, int ysz, int *kunq, int Sorted );
byte * UniqueByte (byte *y, int ysz, int *kunq, int Sorted );

ATLAS_DSET_HOLDER Atlas_With_Trimming (AFNI_ATLAS_CODES atcode, int LoadLRMask);


/* Transforms for going from one space to another */
static char MNI_N27_to_AFNI_TLRC_HEAD[256] = {"TT_N27+tlrc"}; /*!<  TT_N27+tlrc was obtained by transforming N27 from MNI 
                                                    space to AFNI's Talairach space (manual transformation, 12 piece-wise-linear xforms) 
                                                    N27 was taken from Zilles' v12 database (colin_seg.hdr) before it got changed
                                                    to MNI anatomical (by simple shift) in v13 database*/
static char TT_DAEMON_TT_PREFIX[256] = {"TTatlas"}; /*!< Good old tlrc daemon */
static char CA_EZ_N27_MPM_TT_PREFIX[256] = {"TT_N27_CA_EZ_MPM"};   /*!< Prefix of the Zilles Eickhoff Maximum Probability Maps in TT space */
static char CA_EZ_N27_PMaps_TT_PREFIX[256] = {"TT_N27_CA_EZ_PMaps"};    /*!< Prefix of the Zilles Eickhoff Probability Maps in TT space */
static char CA_EZ_N27_ML_TT_PREFIX[256] = {"TT_N27_EZ_ML"};  /*!< Prefix of the Zilles Eickhoff Macro Labels in TT space */
static char CA_EZ_N27_LR_TT_PREFIX[256] = {"TT_N27_EZ_LR"};   /*!< Prefix of the Zilles Eickhoff Left/Right mask dset */


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
