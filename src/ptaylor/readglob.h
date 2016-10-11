#ifndef _READGLOB_HEADER_
#define _READGLOB_HEADER_


// ------------------- for DTI globbing -------------------------------

#define N_DTI_SCAL (3)     
#define N_DTI_VECT (3) 

#define N_DTI_SCAL_TOT (6) // 1+2+3, matches with N_DTI_SCAL
#define N_PLUS_DTIFILE (4) // number of allowed additional param files
                           // in NIMLver

static char *DTI_SCAL_LABS[N_DTI_SCAL] = {"FA", "MD", "L1"}; // labels
static char *DTI_VECT_LABS[N_DTI_VECT] = {"V1", "V2", "V3"};

// ------------------- for DTI globbing -------------------------------

int glob_for_DTI( char *infix,
                  THD_3dim_dataset **insetPARS,
                  THD_3dim_dataset **insetVECS,
                  int hardi_pref_len );

int list_for_DTI( char *dti_listname,
                  THD_3dim_dataset **insetPARS,
                  THD_3dim_dataset **insetVECS );

#endif /* _READGLOB_HEADER_ */
