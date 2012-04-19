#ifndef _DOTRACKIT_HEADER_
#define _DOTRACKIT_HEADER_

#define EPS_V (0.000001) // for eigvec 'vel' to not have badness dividing
#define CONV (3.141592654/180)
#define EPS_MASK (0.001) // theshold for masked data to be ignored
#define EPS_L (0.00001)
#define PIo2 (3.141592654/2)
#define MINEIG (0) // minimum allowed eigenvalue

typedef struct {
   int id;     /* some identifier */
   int N_pts;  /* number of xyz triplets */
   float *pts; /* xyz triplets */
} TAYLOR_TRACT;

typedef struct {
   THD_3dim_dataset *grid;
   THD_3dim_dataset *FA;
   int N_trcts;
   int N_allocated;
   TAYLOR_TRACT *trcts;
} TAYLOR_BUNDLE;

void Show_Taylor_Bundle(TAYLOR_BUNDLE *bundle, FILE *out, int mx);
void Show_Taylor_Tract(TAYLOR_TRACT *tract, FILE *out, int mx);
TAYLOR_TRACT *CreateTract(int N_ptsB, float **pts_buffB,
                          int N_ptsF, float **pts_buffF);
TAYLOR_TRACT *FreeTract(TAYLOR_TRACT *tb);
TAYLOR_BUNDLE *AppCreateBundle(TAYLOR_BUNDLE *tbu, int N_trctsbuf, 
                              TAYLOR_TRACT *trcts_buff);
                              

int TrackIt(float ****CC, int *IND, float *PHYSIND, 
            float *Edge, int *dim, float minFA, 
	    float maxAng, int arrMax, 
            int **T, float **flT, int FB, float *physL);

#endif /* _DOTRACKIT_HEADER_ */








