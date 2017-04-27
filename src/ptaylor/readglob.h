#ifndef _READGLOB_HEADER_
#define _READGLOB_HEADER_


// ------------------- for DTI globbing -------------------------------

#define N_DTI_XTRA (1)     
#define N_DTI_SCAL (4)     
#define N_DTI_VECT (3) 
#define N_DTI_PLUS (4)     // number of allowed additional param files
                           // in NIMLver

// for tracking output
#define N_DTI_MAX_PARS ( N_DTI_SCAL + N_DTI_PLUS + N_DTI_XTRA ) 

//#define N_DTI_SCAL_TOT (10) // 1+2+3, matches with N_DTI_SCAL; !!!

static char *DTI_SCAL_LABS[N_DTI_SCAL] = {"FA", "MD", "L1", "RD"}; // labels
static char *DTI_VECT_LABS[N_DTI_VECT] = {"V1", "V2", "V3"};
static char *DTI_PLUS_LABS[N_DTI_PLUS] = {"P1", "P2", "P3", "P4"};
static char *DTI_XTRA_LABS[N_DTI_XTRA] = {"XF"};

// ------------------- for DTI globbing -------------------------------

int glob_for_DTI_trac( char *infix,
                       THD_3dim_dataset **insetPARS,
                       char **wild_names,
                       int hardi_pref_len,
                       int *pars_top,
                       int SEARCH_NO);

int glob_for_DTI_unc( char *infix,
                      THD_3dim_dataset **insetPARS,
                      int hardi_pref_len);


int glob_for_DTI( char *infix,
                  THD_3dim_dataset **insetPARS,
                  THD_3dim_dataset **insetVECS,
                  int hardi_pref_len,
                  int FULL);

int list_for_DTI( char *dti_listname,
                  THD_3dim_dataset **insetPARS,
                  THD_3dim_dataset **insetVECS,
                  int *extrafile, int *pars_top,
                  char **wild_names,
                  int FULL);

#endif /* _READGLOB_HEADER_ */
