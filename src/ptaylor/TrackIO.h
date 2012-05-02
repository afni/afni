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
int NI_getTractAlgOpts(NI_element *nel, float *MinFA, float *MaxAng, 
                       float *MinL, int *SeedPerV, int *M, int *bval);
NI_element * NI_setTractAlgOpts(NI_element *nel, float *MinFA, float *MaxAng, 
                     float *MinL, int *SeedPerV, int *M, int *bval);
NI_element *ReadTractAlgOpts(char *fname);
int WriteTractAlgOpts(char *fname, NI_element *nel); 

#endif
