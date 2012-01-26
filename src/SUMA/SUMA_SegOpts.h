#ifndef SUMA_SEGOPTS_INCLUDED
#define SUMA_SEGOPTS_INCLUDED

#define SEG_LSQFIT 2
#define SEG_L1FIT 1

typedef enum {
   SEG_BIM, SEG_LSB, SEG_BNN, SEG_BFT
} BLUR_METH;

typedef struct {
   int N_label;
   char **label;
   int *keys;
   int nP;
   double **Pv;
   char **pname;
} SUMA_CLASS_STAT;
  

typedef struct {
   void (*helpfunc)(int);
   char *aset_name;
   char *mset_name;
   char *sig_name;
   char *gold_name;
   char *gold_bias_name;
   char *this_pset_name;
   char *this_cset_name;
   char *this_fset_name;
   char *this_xset_name;
   char *ndist_name;
   char *proot;
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
   THD_3dim_dataset *gold;
   THD_3dim_dataset *gold_bias;
   THD_3dim_dataset *priCgA;
   double wA;
   THD_3dim_dataset *priCgL;
   double wL;
   THD_3dim_dataset *pCgN;
   char *priCgLname;
   char *priCgAname;
   THD_3dim_dataset *priCgALL;
   char *priCgALLname;
   THD_3dim_dataset *Bset;
   char *Bsetname;
   THD_3dim_dataset *pstCgALL;
   char *pstCgALLname;
   NI_element *ndist;
   int debug;
   int idbg, jdbg, kdbg;
   float binwidth;
   NI_str_array *feats, *clss;
   int Other;
   int *keys;
   
   float *mixfrac;
   char *mixopt;
   int UseTmp;
   byte logp;
   int VoxDbg;
   int VoxDbg3[3];
   FILE *VoxDbgOut;
   byte rescale_p;
   byte openmp;
   
   char *labeltable_name;
   
   int smode;
   float bias_param;
   char *bias_meth;
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
   int N_enhance_cset_init;
   int N_main;
   NI_str_array *group_classes;
   int *group_keys;
   SUMA_GENERIC_ARGV_PARSE *ps;
   
   int clust_cset_init;
   SUMA_CLASS_STAT *cs;
   SUMA_CLASS_STAT *Gcs;
   
   double B;
   double T;
   
   int edge;
   float na;
   
   char *hist;
   int *Split;
   
   BLUR_METH blur_meth;
} SEG_OPTS;

void GenPriors_usage(int) ;
void Seg_usage(int) ;
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
int SUMA_ShortizeDset(THD_3dim_dataset **dsetp, float thisfac);

#endif
