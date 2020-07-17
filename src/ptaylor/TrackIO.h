#ifndef TRACKIO_INCLUDED
#define TRACKIO_INCLUDED



// ---------------- for param names while tracking -------------------------

#define N_DEF_PAR_LABS (7) // we define a few things initially
#define N_XTR_PAR_LABS (3) // we define a few things initially

static char *DEF_PAR_LABS[N_DEF_PAR_LABS] = { "NT", "fNT", "PV", 
                                              "fNV", "NV", "BL",
                                              "sBL"}; 
static char *XTR_PAR_LABS[N_XTR_PAR_LABS] = { "NTpTarVol", 
                                              "NTpTarSA",
                                              "NTpTarSAFA"}; 


// temporary candidate for NIML string reading based on ZSS's
// int/float-reading definitions in */suma_afni_surface.h
#define NI_SETA_STR_tmp(ngr, name, val)  {\
   char m_stmp[200]; sprintf(m_stmp,"%s", (val));   \
   NI_set_attribute(ngr, name, m_stmp);  \
}


typedef struct {
   int id;     /* some identifier */
   int N_pts3;  /* total number of values in pts */
   float *pts; /* vector of coordinates xyz, xyz 
                  N_pts3/3 total number of pts*/
} TAYLOR_TRACT; /* KEEP IN SYNC WITH TAYLOR_TRACT_DATUM_DEF */
#define TRACT_NPTS(tt) ( (tt)->N_pts3/3 ) 
#define TAYLOR_TRACT_DATUM_NIML_DEF "int,int,float[#2]"


typedef struct {
   int N_tracts;
   int N_allocated;
   TAYLOR_TRACT *tracts;
   int *tract_P0_offset_private; /* Index of 1st point in each tract 
                                    Index of 1st point of 1st tract is 0 */
   int N_points_private; /* Sum of points in all tracts of bundle */
   char *bundle_ends; // labels, added Sept 2014
} TAYLOR_BUNDLE;

typedef struct {
   THD_3dim_dataset *grid;
   THD_3dim_dataset *FA;
   char atlas_space[65];
   int N_allocated;
   int N_tbv;
   TAYLOR_BUNDLE **tbv;
   int *bundle_tags;
   int *bundle_alt_tags;
   int N_points_private; /* Num of points in all tracts of all bundles in net */
   int N_tracts_private; /* Sum of number of tracts in all bundles */
   int Longest_tract_length_private; /* Length in points of longest tract */
   int Longest_tract_index_in_bundle_private; /* longest tract index in bundle */
   int Longest_tract_bundle_index_in_network_private; /* longest tract bundle */ 
} TAYLOR_NETWORK;

typedef struct {
   THD_3dim_dataset *grid;
} INSTA_TRACT_SETUP;

int Free_Insta_Tract_Setup(INSTA_TRACT_SETUP *ITS);
INSTA_TRACT_SETUP *New_Insta_Tract_Setup(INSTA_TRACT_SETUP *ITS);

float Tract_Length(TAYLOR_TRACT *tt);
void Show_Taylor_Network(TAYLOR_NETWORK *network, FILE *out, int mx, int mxb);
void Show_Taylor_Bundle(TAYLOR_BUNDLE *bundle, FILE *out, int mx);
void Show_Taylor_Tract(TAYLOR_TRACT *tract, FILE *out, int mx);
TAYLOR_TRACT *Create_Tract_NEW(int ptA, int ptB, float **pts_buff, 
                               int id, THD_3dim_dataset *grid);
int Bundle_N_points(TAYLOR_BUNDLE *bun, byte recalc);
int Network_N_points(TAYLOR_NETWORK *network, byte recalc);
int Network_N_tracts(TAYLOR_NETWORK *network, byte recalc);
int Network_N_bundles(TAYLOR_NETWORK *network);
int Network_Max_tract_length(TAYLOR_NETWORK *net, byte recalc,
                             int *t, int *b);
int Network_PTB_to_1P(TAYLOR_NETWORK *network, int p, int t, int b);
int Network_TB_to_1T(TAYLOR_NETWORK *net, int t, int b);
int Network_1P_to_PTB(TAYLOR_NETWORK *network, int P1, 
                      int *p, int *t, int *b, int *l);
int Network_1T_to_TB(TAYLOR_NETWORK *net, int TT, int *t, int *b, 
                     int *P0, int *P1);
int Network_1B_to_1P(TAYLOR_NETWORK *net, int BB, int *PP1);
/*TAYLOR_TRACT *Create_Tract(int N_ptsB, float **pts_buffB,
                          int N_ptsF, float **pts_buffF, 
                          int id, THD_3dim_dataset *grid);*/
TAYLOR_TRACT *Free_Tracts(TAYLOR_TRACT *tt, int N);
TAYLOR_BUNDLE *AppCreateBundle(TAYLOR_BUNDLE *tbu, int N_tractsbuf, 
                               TAYLOR_TRACT *tracts_buff);
TAYLOR_BUNDLE *Free_Bundle(TAYLOR_BUNDLE *tb);
TAYLOR_NETWORK *Free_Network(TAYLOR_NETWORK *net);                       

NI_element *Tract_2_NIel(TAYLOR_TRACT *tt);
TAYLOR_TRACT *NIel_2_Tract(NI_element *nel);

TAYLOR_NETWORK *AppAddBundleToNetwork(TAYLOR_NETWORK *network, 
                                      TAYLOR_BUNDLE **tb,int tag, int alt_tag,
                                      THD_3dim_dataset *grid, char *EleName);
NI_group *Network_2_NIgr(TAYLOR_NETWORK *network, int mode);
NI_group *Network_link(char *filename);
TAYLOR_NETWORK *NIgr_2_Network(NI_group *ngr); 
int Write_NI_Network(NI_group *ngr, char *name, char *mode); 
int Write_Network(TAYLOR_NETWORK *network, char *name, char *mode);
int Write_Bundle(TAYLOR_BUNDLE *tb, char *name, char *mode);
TAYLOR_NETWORK * Read_Network(char *name) ;
NI_group * Read_NI_Network(char *name);

int get_tract_verb(void);
void set_tract_verb(int v);
int get_NI_tract_type(void);

// this currently works for both 3dTrack and 3dProbTrack!
int WriteTractAlgOpts(char *fname, NI_element *nel); 

// new versions for MULTI/HARDI: no M and bvals
int NI_getTractAlgOpts_M(NI_element *nel, float *MinFA, float *MaxAngDeg, 
                       float *MinL, int *SeedPerV);
NI_element * NI_setTractAlgOpts_M(NI_element *nel, float *MinFA, 
										  float *MaxAngDeg, float *MinL, 
										  int *SeedPerV);
NI_element *ReadTractAlgOpts_M(char *fname);
int NI_getProbTractAlgOpts_M(NI_element *nel, float *MinFA, float *MaxAngDeg, 
									float *MinL, float *NmNsFr, int *Nseed, 
									int *Nmonte);
NI_element * NI_setProbTractAlgOpts_M(NI_element *nel, float *MinFA, 
												float *MaxAngDeg, float *MinL,
												float *NmNsFr, int *Nseed, 
												int *Nmonte);
NI_element *ReadProbTractAlgOpts_M(char *fname);

// TrackVis requirements for *.trk file, defining the header as a struct
typedef struct 
{
  // 6     ID string for track file. The first 5 characters must be "TRACK".
  char           id_string[6];   
  // 6   Dimension of the image volume.
  short int   dim[3];           
  // 12  Voxel size of the image volume.
  float           voxel_size[3];   
  // 12  Origin of the image volume (Field is not yet being used by
  //     TrackVis). Origin is always (0, 0, 0).
  float           origin[3];   
  // 2   Number of scalars saved at each track point (besides x, y and z 
  //     coordinates).
  short int   n_scalars;   
  // 200 Name of each scalar. Cannot be longer than 20 characters
  //     each. Can only store up to 10 names.
  char           scal_n[10][20];   
  // 2   Number of properties saved at each track.
  short int   n_properties;   
  // 200 Name of each property. Cannot be longer than 20 characters
  //     each. Can only store up to 10 names.
  char           prop_n[10][20];   
  // 64  4x4 matrix for voxel to RAS (crs to xyz) transf. If
  //     vox_to_ras[3][3] is 0, matrix not recorded.
  float           vox_to_ras[4][4];
  char           reserved[444];  // 444 Reserved space for future version.
  // 4     Storing order of the original image data. Explained here.
  char           voxel_order[4];   
  char           pad2[4];  // 4     Paddings.   
  // 24  Image orientation of the original image. As defined in the
  //     DICOM header.
  float           img_orient_p[6];
  char           pad1[2];  // 2     Paddings.   
  // 1     Inversion/rotation flags used to generate this track file. For 
  // internal use only.   
  unsigned char   invert_x;  
  unsigned char   invert_y;  // 1 As above.   
  unsigned char   invert_z;  // 1 As above.   
  unsigned char   swap_xy;   // 1 As above.
  unsigned char   swap_yz;   // 1 As above.
  unsigned char   swap_zx;   // 1 As above.
  // 4     Number of tracks stored in this track file. 0 means the number 
  //     was NOT stored.
  int           n_count;  // 4     Version number. Current version is 2.
  int           version;      
  // 4     Size of the header. Used to determine byte swap. Should be 1000.
  int           hdr_size;   
} tv_io_header;   

// for writing trackvis track info currently
int SimpleWriteDetNetTr_M(int N_HAR, FILE *file, int ***idx, 
                           THD_3dim_dataset **PARS,
                           int PAR_BOT, int PAR_TOP,
                           float **loc, int **locI, int len,
                           int *TV, int *Dim, float *Ledge);


NI_element * ReadDTI_inputs(char *fname);
int NI_getDTI_inputs( NI_element *nel, 
                      char **NameVECT, 
                      char *NameXF, 
                      char **NameSCAL,
                      char **NamePLUS, 
                      int *extrafile, int *pars_top);

char * SUMA_Taylor_Network_Info(TAYLOR_NETWORK *net, 
                                int show_maxu, int show_maxub);
char *SUMA_Taylor_Bundle_Info(TAYLOR_BUNDLE *tb, int show_maxu); 
char *SUMA_Taylor_Tract_Info(TAYLOR_TRACT *tt, int show_maxu);

#endif
