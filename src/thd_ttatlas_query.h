#ifndef _TTATLAS_QUERY_HEADER_
#define _TTATLAS_QUERY_HEADER_

#define IS_LETTER(c) ( ( ((c) >= 'A' && (c) <= 'Z') || ((c) >= 'a' && (c) <= 'z') ) ? 1 : 0 )
#define IS_NUMBER(c) ( ( ((c) >= '0' && (c) <= '9') ) ? 1 : 0 )
#define TO_LOWER(c) ( ((c) >= 'A' && (c) <= 'Z') ? (c + 'a' - 'A') : (c) )
#define STR_PRINT(s) ( ((s)) ? (s) : ("NULL") )
#define MIN_PAIR(a,b) ( ((a)<(b)) ? (a) : (b) )

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

typedef struct {
      float x;
      int Index;
} Z_QSORT_FLOAT;

typedef enum { UNKNOWN_SPC=0, /*!< Dunno */
               AFNI_TLRC_SPC, /*!< The Classic */
               MNI_SPC,       /*!< A la Canadienne */ 
               MNI_ANAT_SPC,  /*!< Mit viele liebe */
               
               NUMBER_OF_SPC  /*!< A flag for the number of spaces, leave for last */
               } AFNI_STD_SPACES;
               
typedef enum { UNKNOWN_ATLAS=0, /*!< Dunno */
               AFNI_TLRC_ATLAS, /*!< The Classic */
               CA_EZ_MPM_ATLAS,  /*!< Eickhoff, Zilles MPM atlas*/
               CA_EZ_ML_ATLAS,  /*!< Eickhoff, Zilles MacroLabels atlas*/
               CA_EZ_PMAPS_ATLAS, /*!< Eickhoff, Zilles Probability maps atlas*/
               
               NUMBER_OF_ATLASES  /*!< A flag for the number of atlases, leave for last */
               } AFNI_ATLAS_CODES;


typedef struct {
   float x, y, z; /*!< coordinates */
   AFNI_STD_SPACES space; /*!< type of coordinate space */
} ATLAS_COORD;

typedef struct {
   int N_label; /*!< number of label types available in a particular atlas. 
                     For example the "Anterior Cingulate" can coincide with "Brodmann area 25" in TT_Fox atlas.
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

int compare_Z_IQSORT_FLOAT (Z_QSORT_FLOAT *a, Z_QSORT_FLOAT *b );
char Is_Side_Label(char *str, char *opt);
int *z_iqsort (float *x , int nx );
void Show_Atlas_Region (AFNI_ATLAS_REGION *aar);
AFNI_ATLAS_REGION * Free_Atlas_Region (AFNI_ATLAS_REGION *aar);
AFNI_ATLAS_REGION * Atlas_Chunk_Label(char *lbli, int id);
AFNI_ATLAS *Build_Atlas (char *name) ;
void Show_Atlas (AFNI_ATLAS *aa);
AFNI_ATLAS *Free_Atlas(AFNI_ATLAS *aa) ;
AFNI_ATLAS_REGION *ROI_String_Decode(char *str, char *atlas_name);
int *Find_Atlas_Regions(AFNI_ATLAS *aa, AFNI_ATLAS_REGION *ur , int *nmatch);
char * Clean_Atlas_Label( char *lb);
const char *Space_Code_to_Space_Name (AFNI_STD_SPACES cod);
const char *Atlas_Code_to_Atlas_Name (AFNI_ATLAS_CODES cod);
ATLAS_ZONE *Get_Atlas_Zone(ATLAS_QUERY *aq, int level);
ATLAS_ZONE *Atlas_Zone(ATLAS_ZONE *zn, int level, char *label, int code, float prob, float within, AFNI_ATLAS_CODES atcode) ;
ATLAS_ZONE *Free_Atlas_Zone(ATLAS_ZONE *zn);
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
char *CA_whereami_9yards(ATLAS_COORD ac, ATLAS_QUERY **wamip);

#endif
