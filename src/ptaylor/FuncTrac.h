#ifndef FUNCTRAC_INCLUDED
#define FUNCTRAC_INCLUDED



/* new struct of all input parameters, allows program to be called
   internally
*/
typedef struct {

   // -help, -h
   // -verb 

   // Necessary for all running
   char *NAMEIN_mode;             //  "-mode"
   char *NAMEIN_netrois;          //  "-netrois"
   char *prefix;                  //  "-prefix"

   // DTI input names/opts
   char *dti_inpref;              //  "-dti_in"
   char *dti_listname;            //  "-dti_list"
   char *in_EXTRA;                //  "-dti_extra"
   int NO_NONDTI_SEARCH;          //  "-dti_search_NO"

   // HARDI input 
   char *hardi_dir;               //  "-hardi_dirs"
   char *hardi_gfa;               //  "-hardi_gfa"
   char *hardi_pars;              //  "-hardi_pars"

   // mask
   char *NAMEIN_mask;             //  "-mask"
   char *NAMEIN_thru;             // "-thru_mask"

   // algorithm options 
   char *algopt_file_name;        //  "-algopt"
   float MinFA;                   //  "-alg_Thresh_FA"     - DET,MINIP,PROB
   float MaxAngDeg;               //  "-alg_Thresh_ANG"    - DET,MINIP,PROB
   float MinL;                    //  "-alg_Thresh_Len"    - DET,MINIP,PROB
   float NmNsFr;                  //  "-alg_Thresh_Frac"   - PROB
   int Nseed;                     //  "-alg_Nseed_Vox"     - PROB
   int Nmonte;                    //  "-alg_Nmonte"        - PROB
   int SeedPerV[3];               //  "-alg_Nseed_{X,Y,Z}" - DET,MINIP

   // uncert filename
   char *NAMEIN_uncert;           //  "-uncert"
   // these mins get strdup'ed
   char *NAMEIN_uncFA;            //  "-unc_min_FA"
   char *NAMEIN_uncEI;            //  "-unc_min_V"

   // other control, minip/logic/.trk/rand-seed
   int LOG_TYPE;                  //  "-logic"
   int ONLY_BT;                   //  "-uncut_at_rois"
   int MINI_PROB_NM;              //  "-mini_num"
   int CHOOSE_SEED;               //  "-choose_seed"
   int OUTPUT_TRK;                //  "-do_trk_out"
   int OUT_INDIPAIR;              //  "-no_indipair_out"
   int THRESH_BUNDS;              //  "-bundle_thr"
   int TARG_SURF_STOP;            //  "-targ_surf_stop"
   int TARG_SURF_TWIXT;           //  "-targ_surf_twixt"

   // minorly used control stuff, mostly default
   char *NAMEIN_outmode;          //  "-tract_out_mode"
   int dump_opts;                 //  "-write_opts"
   int ROIS_OUT;                  //  "-write_rois"
   int DUMP_ORIG_LABS;            //  "-dump_lab_consec"
   int POST;                      //  "-posteriori"
   int DUMP_TYPE;                 //  "-dump_rois"
   int DUMP_with_LABELS;          //  "-dump_no_labtab"
   int RECORD_ORIG;               //  "-rec_orig"
   int PAIRPOWER;                 //  "-pair_out_power"
   int EXTRA_TR_PAR;              //  "-extra_tr_par"
   int NIFTI_OUT;                 //  "-nifti"

   // EXPIRED OPTIONS, just gives message/error:
   // "-cut_at_rois"
   // "-det_net"

} TRACK_RUN_PARAMS;





TRACK_RUN_PARAMS SetInitialTrackingDefaults(void);











#endif
