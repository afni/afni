#ifndef SUMA_SEGOPTS_INCLUDED
#define SUMA_SEGOPTS_INCLUDED

#define SEG_LSQFIT 2
#define SEG_L1FIT 1

typedef struct {
   int N_label;
   char **label;
   int *keys;
   int nP;
   double *P;
   char **pname;
} SUMA_CLASS_STAT;  

typedef struct {
   char *aset_name;
   char *mset_name;
   char *sig_name;
   char *this_pset_name;
   char *this_cset_name;
   char *this_fset_name;
   char *this_xset_name;
   char *ndist_name;
   char *prefix;
   char *crefix;
   char *frefix;
   char *xrefix;
   char *cgrefix;
   char *pgrefix;
   char uid[256+1];
   THD_3dim_dataset *aset;
   THD_3dim_dataset *mset;
   THD_3dim_dataset *sig;
   THD_3dim_dataset *pset;
   THD_3dim_dataset *cset;
   THD_3dim_dataset *fset;
   THD_3dim_dataset *xset;
   THD_3dim_dataset *gset;
   NI_element *ndist;
   int debug;
   int idbg, jdbg, kdbg;
   float binwidth;
   NI_str_array *feats, *clss;
   int *keys;
   
   float *mixfrac;
   int UseTmp;
   byte logp;
   int VoxDbg;
   FILE *VoxDbgOut;
   byte rescale_p;
   byte openmp;
   
   char *labeltable_name;
   
   int smode;
   int Lpo;
   char *bias_classes;
   byte pweight;
   int N_biasgroups;
     
   byte *cmask;
   int dimcmask;
   int cmask_count;
   float mask_top;
   float mask_bot;
   
   byte DO_p;
   byte DO_f;
   byte DO_c;
   byte DO_x;
   byte DO_r;
   
   int fitmeth;
   int N_ibias;
   NI_str_array *group_classes;
   int *group_keys;
   SUMA_GENERIC_ARGV_PARSE *ps;
   
   int clust_cset_init;
   SUMA_CLASS_STAT *cs;
   
   double B;
   double T;
} SEG_OPTS;

void GenPriors_usage(void) ;
SEG_OPTS *Seg_ParseInput (SEG_OPTS *Opt, char *argv[], int argc);
byte *MaskSetup(SEG_OPTS *Opt, THD_3dim_dataset *aset, 
                THD_3dim_dataset **msetp, byte **cmaskp, int dimcmask, 
                float mask_bot, float mask_top, int *mcount); 
THD_3dim_dataset *Seg_load_dset( char *set_name );
SEG_OPTS *SegOpt_Struct(void );
void *Seg_NI_read_file(char *fname);
SEG_OPTS *free_SegOpts(SEG_OPTS *);
int Seg_ClssAndKeys_from_dset(THD_3dim_dataset *dset, 
                              NI_str_array **nstrp, int **keysp);


#endif
