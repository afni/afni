#ifndef _ROIING_HEADER_
#define _ROIING_HEADER_

#define MIN_NTHR_MAX_NROILAB (20000) // large number for internal
                                     // labelling thresholds. If the
                                     // refset label is bigger than this
                                     // throw an error
#define DEP (1)              // search rad of defining ROIs
#define BASE_DVAL (-1)       // intermed values for ROI-finding
//#define ALLOWED_NROI (150)   // buffer size for array of refset...


int MoveData_to_InpSet( int *Dim,
                        float ****map,
                        int ****DATA,
                        short int ***iskel);

int ROI_make_inflate( int *Dim,
                      int INFL_NUM,
                      int SKEL_STOP,
                      int NEIGHBOR_LIMIT,
                      int HAVE_MASK,
                      THD_3dim_dataset *MASK,
                      int ****DATA,
                      short int ***SKEL,
                      int ***COUNT_GM,
                      int **INV_LABELS_GM);


int compfunc_desc(const void * a, const void * b);

void piksr2_FLOAT(int n, float arr[], int brr[]);
int Do_Check_Neigh_Diff(int *D, int NL);

int Make_SepLabels( int *Dim,
                    int ****DATA,
                    int max_nroi,
                    int *N_thr,
                    int *NROI_IN,
                    int **ROI_LABELS_pre,
                    int VOLTHR,
                    int NEIGHBOR_LIMIT,
                    int HOT_POINTS,
                    int HOT_CONN,
                    float ****inset );

int Relabel_IfNecessary( int *Dim,
                         int ****DATA,
                         int *N_thr,
                         int *relab_vox,
                         int *NROI_IN,
                         int *NROI_REF,
                         int **ROI_LABELS_REF,
                         int NEIGHBOR_LIMIT);

int MakeSkels( int *Dim,
               int HAVE_CSFSKEL,
               short int ***CSF_SKEL,
               THD_3dim_dataset *insetCSF_SKEL,
               int HAVESKEL,
               short int ***SKEL,
               THD_3dim_dataset *insetSKEL,
               float SKEL_THR );

int Make_BinaryMask( int *Dim,
                     int HAVE_MASK,
                     THD_3dim_dataset *MASK,
                     float ****inset,
                     float THR,
                     int TRIM_OFF_WM,
                     short int ***SKEL,
                     short int ***CSF_SKEL,
                     int HAVE_CSFSKEL,
                     int ****DATA,
                     int *N_thr );






#endif /* _ROIING_HEADER_ */
