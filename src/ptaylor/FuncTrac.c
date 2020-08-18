#include "mrilib.h" 
#include "FuncTrac.h"






/*
  In new struct-entry running mode, these are the defaults.
 */
TRACK_RUN_PARAMS SetInitialTrackingDefaults(void)
{
   TRACK_RUN_PARAMS defopt;
   
   // SET THEM:
   defopt.NAMEIN_mode = NULL;             //  "-mode"
   defopt.NAMEIN_netrois = NULL;          //  "-netrois"
   defopt.prefix = NULL;                  //  "-prefix"

   // DTI input names/opts
   defopt.dti_inpref = NULL;              //  "-dti_in"
   defopt.dti_listname = NULL;            //  "-dti_list"
   defopt.in_EXTRA = NULL;                //  "-dti_extra"
   defopt.NO_NONDTI_SEARCH = 0; // "-dti_search_NO"; won't keep scal/nonDTI pars

   // HARDI input 
   defopt.hardi_dir = NULL;               //  "-hardi_dirs"
   defopt.hardi_gfa = NULL;               //  "-hardi_gfa"
   defopt.hardi_pars = NULL;              //  "-hardi_pars"

   // mask
   defopt.NAMEIN_mask = NULL;             //  "-mask"
   defopt.NAMEIN_thru = NULL;               // "-thru_mask"

   // algorithm options 
   defopt.algopt_file_name = NULL;  //  "-algopt"
   defopt.MinFA=0.2;                //  "-alg_Thresh_FA"     - DET,MINIP,PROB
   defopt.MaxAngDeg=60.;            //  "-alg_Thresh_ANG"    - DET,MINIP,PROB
   defopt.MinL=20.;                 //  "-alg_Thresh_Len"    - DET,MINIP,PROB
   defopt.NmNsFr=0.001;             //  "-alg_Thresh_Frac"   - PROB
   defopt.Nseed=5;                  //  "-alg_Nseed_Vox"     - PROB
   defopt.Nmonte=1000;              //  "-alg_Nmonte"        - PROB
   defopt.SeedPerV[0]=2;      //  "-alg_Nseed_{X,Y,Z}" - DET,MINIP
   defopt.SeedPerV[1]=2;      //  "-alg_Nseed_{X,Y,Z}" - DET,MINIP
   defopt.SeedPerV[2]=2;      //  "-alg_Nseed_{X,Y,Z}" - DET,MINIP

   // uncert filename
   defopt.NAMEIN_uncert = NULL;           //  "-uncert"
   // these mins get strdup'ed
   defopt.NAMEIN_uncFA = NULL;            //  "-unc_min_FA"
   defopt.NAMEIN_uncEI = NULL;            //  "-unc_min_V"

   // other control, minip/logic/.trk/rand-seed
   defopt.LOG_TYPE=-1;                 //  "-logic"
   // default now TO cut off tracks at ROI endpts
   defopt.ONLY_BT=1;                   //  "-uncut_at_rois"
   // default: no mini monte carloing for det_net
   defopt.MINI_PROB_NM=0;              //  "-mini_num"
   defopt.CHOOSE_SEED = 0;             //  "-choose_seed"
   defopt.OUTPUT_TRK = 0;              //  "-do_trk_out", Feb,2015:->0
   defopt.TRK_OPP_ORI = 0;             //  "-trk_opp_orient", Jun,2019
   defopt.OUT_INDIPAIR = 1;            //  "-no_indipair_out"
   defopt.THRESH_BUNDS = 1;            //  "-bundle_thr", Feb,2015:NEW
   defopt.TARG_SURF_STOP = 0;          //  "-targ_surf_stop", Nov,2016
   defopt.TARG_SURF_TWIXT = 0;         //  "-targ_surf_twixt", Nov,2016

   // minorly used control stuff, mostly default
   defopt.NAMEIN_outmode = NULL;       //  "-tract_out_mode"
   defopt.dump_opts=0;                 //  "-write_opts"
   defopt.ROIS_OUT = 0;                //  "-write_rois"
   defopt.DUMP_ORIG_LABS=1;            //  "-dump_lab_consec"
   // switch about having no min threshold,and dumping ROIs
   defopt.POST=0;                      //  "-posteriori"
   // switch about whether to dump ascii/afni/both
   defopt.DUMP_TYPE=-1;                //  "-dump_rois"
   defopt.DUMP_with_LABELS=1;          //  "-dump_no_labtab"
   defopt.RECORD_ORIG=0;               //  "-rec_orig"
   defopt.PAIRPOWER=0;                 //  "-pair_out_power"
   defopt.EXTRA_TR_PAR=0;              //  "extra_tr_par"
   defopt.NIFTI_OUT=0;                 //  "-nifti"

   // EXPIRED OPTIONS, just gives message/error:
   // "-cut_at_rois"
   // "-det_net"

   return defopt;
}
