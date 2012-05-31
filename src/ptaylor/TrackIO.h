#ifndef TRACKIO_INCLUDED
#define TRACKIO_INCLUDED

typedef struct {
   int id;     /* some identifier */
   int N_pts3;  /* total number of values in pts */
   float *pts; /* vector of coordinates xyz, xyz 
                  N_pts3/3 total number of pts*/
} TAYLOR_TRACT; /* KEEP IN SYNC WITH TAYLOR_TRACT_DATUM_DEF */
#define TRACT_NPTS(tt) ( (tt)->N_pts3/3 ) 
#define TAYLOR_TRACT_DATUM_NIML_DEF "int,int,float[#2]"


typedef struct {
   THD_3dim_dataset *grid;
   THD_3dim_dataset *FA;
   int N_tracts;
   int N_allocated;
   TAYLOR_TRACT *tracts;
   char atlas_space[65];
} TAYLOR_BUNDLE;

void Show_Taylor_Bundle(TAYLOR_BUNDLE *bundle, FILE *out, int mx);
void Show_Taylor_Tract(TAYLOR_TRACT *tract, FILE *out, int mx);
TAYLOR_TRACT *Create_Tract(int N_ptsB, float **pts_buffB,
                          int N_ptsF, float **pts_buffF, 
                          int id, THD_3dim_dataset *grid);
TAYLOR_TRACT *Free_Tracts(TAYLOR_TRACT *tt, int N);
TAYLOR_BUNDLE *AppCreateBundle(TAYLOR_BUNDLE *tbu, int N_tractsbuf, 
                              TAYLOR_TRACT *tracts_buff, 
                              THD_3dim_dataset *grid);
TAYLOR_BUNDLE *Free_Bundle(TAYLOR_BUNDLE *tb);
                              

NI_element *Tract_2_NIel(TAYLOR_TRACT *tt);
TAYLOR_TRACT *NIel_2_Tract(NI_element *nel);
NI_group *Bundle_2_NIgr(TAYLOR_BUNDLE *tb, int mode);
TAYLOR_BUNDLE *NIgr_2_Bundle(NI_group *ngr); 
int Write_NI_Bundle(NI_group *ngr, char *name, char *mode); 
int Write_Bundle(TAYLOR_BUNDLE *tb, char *name, char *mode);
TAYLOR_BUNDLE * Read_Bundle(char *name) ;
NI_group * Read_NI_Bundle(char *name);
int get_tract_verb(void);
void set_tract_verb(int v);
int NI_getTractAlgOpts(NI_element *nel, float *MinFA, float *MaxAngDeg, 
                       float *MinL, int *SeedPerV, int *M, int *bval);
NI_element * NI_setTractAlgOpts(NI_element *nel, float *MinFA, 
										  float *MaxAngDeg, float *MinL, 
										  int *SeedPerV, int *M, int *bval);
NI_element *ReadTractAlgOpts(char *fname);
int NI_getProbTractAlgOpts(NI_element *nel, float *MinFA, float *MaxAngDeg, 
									float *MinL, float *NmNsFr, int *Nseed, 
									int *Nmonte, int *M, int *bval);
NI_element * NI_setProbTractAlgOpts(NI_element *nel, float *MinFA, 
												float *MaxAngDeg, float *MinL,
												float *NmNsFr, int *Nseed, 
												int *Nmonte, int *M, int *bval);
NI_element *ReadProbTractAlgOpts(char *fname);
// this currently works for both 3dTrack and 3dProbTrack!
int WriteTractAlgOpts(char *fname, NI_element *nel); 



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


#endif
