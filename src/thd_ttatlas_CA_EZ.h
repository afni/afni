/*! Data for atlases from Eickhoff's SPM toolbox.
Automatically compiled from: AllAreas_v13b_MPM.mat
 located at: /var/automount/Volumes/elrond0/home4/users/ziad/Programs/matlab/spm2/toolbox/Anatomy_13b2
 by function /Users/ziad/AFNI/src/matlab/CA_EZ_Prep.m
Date: 21-Mar-2006*/

/* ----------- Macro Labels --------------------- */
/* ----------- Based on: Macro.mat -------------*/
#define ML_EZ_COUNT   116

extern ATLAS_point ML_EZ_list[ML_EZ_COUNT] ;
extern char * ML_EZ_labels[ML_EZ_COUNT] ;
extern int ML_EZ_labeled ;
extern int ML_EZ_current ;
/* ----------- Left Right   --------------------- */
/* ---- Based on my understanding -------------- */
#define LR_EZ_COUNT   3

extern ATLAS_point LR_EZ_list[LR_EZ_COUNT] ;
extern char * LR_EZ_labels[LR_EZ_COUNT] ;
extern int LR_EZ_labeled ;
extern int LR_EZ_current ;

/* -----------     MPM      --------------------- */
/* ----------- Based on: AllAreas_v13b_MPM.mat --------------*/
#define CA_EZ_COUNT   28
#define CA_EZ_MPM_MIN 100  /*!< minimum meaningful value in MPM atlas */
extern ATLAS_point CA_EZ_list[CA_EZ_COUNT] ;
extern char * CA_EZ_labels[CA_EZ_COUNT] ;
extern int CA_EZ_labeled ;
extern int CA_EZ_current ;

/* -----------     Refs      --------------------- */
/* ----------- Based on se_note.m --------------*/
extern char CA_EZ_REF_STR[26][142];
