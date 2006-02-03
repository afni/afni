#include "mrilib.h"
#include "afni.h"
#include "thd_ttatlas_query.h"

static int           have_dseTT = -1   ;
static THD_3dim_dataset * dseTT = NULL ;
static THD_3dim_dataset * dseTT_big = NULL ; /* 01 Aug 2001 */
static int           have_dseCA_EZ_MPM = -1   ;
static THD_3dim_dataset * dseCA_EZ_MPM = NULL ;
static int           have_dseCA_EZ_PMaps = -1   ;
static THD_3dim_dataset * dseCA_EZ_PMaps = NULL ;
static int           have_dseCA_EZ_ML = -1   ;
static THD_3dim_dataset * dseCA_EZ_ML = NULL ;
static int           have_dseCA_EZ_LR = -1   ;
static THD_3dim_dataset * dseCA_EZ_LR = NULL ;

#define MAX_FIND 9                    /* max number to find within WAMIRAD  */
#define WAMIRAD  7.5                  /* search radius: must not exceed 9.5 */
static MCW_cluster * wamiclust=NULL ;
static MCW_cluster * wamiclust_CA_EZ=NULL ;

static int TT_whereami_mode = 1;
static char lsep = '\n';

/*! These atlas lists, used to be in afni.h and only included if 
Main is defined. That was nice and dandy when only afni and whereami
used them. Now, every main has a use for them since input datasets
can be a string referring to a particular atlas zone. So they were 
added to libmri.a and accessible without restrictions. libmri.a and other 
binaries are fatter as a result. But you dont' get nothin for nothin.
ZSS Feb. 2006 with a nod of approval by RickR */ 
/* TTatlas by Jack Lancaster and Peter Fox */
TTO_point TTO_list[TTO_COUNT] = {
      {  0, -1, -1,0,  0,"Anterior Commissure....................."} ,
      {  0, 23,  0,0,  0,"Posterior Commissure...................."} ,
      {  0,  7, 21,0,  0,"Corpus Callosum........................."} ,
      { 30, 24, -9,4, 68,"Left  Hippocampus......................."} ,
      {-30, 24, -9,4, 68,"Right Hippocampus......................."} ,
      { 23,  5,-15,4, 71,"Left  Amygdala.........................."} ,
      {-23,  5,-15,4, 71,"Right Amygdala.........................."} ,
      { 10, 54, 14,2, 20,"Left  Posterior Cingulate..............."} ,
      {-10, 54, 14,2, 20,"Right Posterior Cingulate..............."} ,
      {  8,-32,  7,2, 21,"Left  Anterior Cingulate................"} ,
      { -8,-32,  7,2, 21,"Right Anterior Cingulate................"} ,
      { 11,-11,-12,2, 22,"Left  Subcallosal Gyrus................."} ,
      {-11,-11,-12,2, 22,"Right Subcallosal Gyrus................."} ,
      { 50, 22, 12,2, 24,"Left  Transverse Temporal Gyrus........."} ,
      {-50, 22, 12,2, 24,"Right Transverse Temporal Gyrus........."} ,
      { 25,  2,-28,2, 25,"Left  Uncus............................."} ,
      {-25,  2,-28,2, 25,"Right Uncus............................."} ,
      {  7,-30,-23,2, 26,"Left  Rectal Gyrus......................"} ,
      { -7,-30,-23,2, 26,"Right Rectal Gyrus......................"} ,
      { 40, 48,-16,2, 27,"Left  Fusiform Gyrus...................."} ,
      {-40, 48,-16,2, 27,"Right Fusiform Gyrus...................."} ,
      { 35, 86, -7,2, 28,"Left  Inferior Occipital Gyrus.........."} ,
      {-35, 86, -7,2, 28,"Right Inferior Occipital Gyrus.........."} ,
      { 56, 39,-13,2, 29,"Left  Inferior Temporal Gyrus..........."} ,
      {-56, 39,-13,2, 29,"Right Inferior Temporal Gyrus..........."} ,
      { 39,  7,  9,2, 30,"Left  Insula............................"} ,
      {-39,  7,  9,2, 30,"Right Insula............................"} ,
      { 25, 25,-12,2, 31,"Left  Parahippocampal Gyrus............."} ,
      {-25, 25,-12,2, 31,"Right Parahippocampal Gyrus............."} ,
      { 14, 78, -3,2, 32,"Left  Lingual Gyrus....................."} ,
      {-14, 78, -3,2, 32,"Right Lingual Gyrus....................."} ,
      { 35, 83,  9,2, 33,"Left  Middle Occipital Gyrus............"} ,
      {-35, 83,  9,2, 33,"Right Middle Occipital Gyrus............"} ,
      { 11,-39,-25,2, 34,"Left  Orbital Gyrus....................."} ,
      {-11,-39,-25,2, 34,"Right Orbital Gyrus....................."} ,
      { 52, 39,  0,2, 35,"Left  Middle Temporal Gyrus............."} ,
      {-52, 39,  0,2, 35,"Right Middle Temporal Gyrus............."} ,
      { 51, 17,  0,2, 36,"Left  Superior Temporal Gyrus..........."} ,
      {-51, 17,  0,2, 36,"Right Superior Temporal Gyrus..........."} ,
      { 37, 82, 27,2, 37,"Left  Superior Occipital Gyrus.........."} ,
      {-37, 82, 27,2, 37,"Right Superior Occipital Gyrus.........."} ,
      { 44,-24,  2,2, 39,"Left  Inferior Frontal Gyrus............"} ,
      {-44,-24,  2,2, 39,"Right Inferior Frontal Gyrus............"} ,
      { 13, 83, 18,2, 40,"Left  Cuneus............................"} ,
      {-13, 83, 18,2, 40,"Right Cuneus............................"} ,
      { 45, 64, 33,2, 41,"Left  Angular Gyrus....................."} ,
      {-45, 64, 33,2, 41,"Right Angular Gyrus....................."} ,
      { 51, 48, 31,2, 42,"Left  Supramarginal Gyrus..............."} ,
      {-51, 48, 31,2, 42,"Right Supramarginal Gyrus..............."} ,
      { 10, 11, 34,2, 43,"Left  Cingulate Gyrus..................."} ,
      {-10, 11, 34,2, 43,"Right Cingulate Gyrus..................."} ,
      { 48, 41, 39,2, 44,"Left  Inferior Parietal Lobule.........."} ,
      {-48, 41, 39,2, 44,"Right Inferior Parietal Lobule.........."} ,
      { 14, 61, 41,2, 45,"Left  Precuneus........................."} ,
      {-14, 61, 41,2, 45,"Right Precuneus........................."} ,
      { 27, 59, 53,2, 46,"Left  Superior Parietal Lobule.........."} ,
      {-27, 59, 53,2, 46,"Right Superior Parietal Lobule.........."} ,
      { 37,-29, 26,2, 47,"Left  Middle Frontal Gyrus.............."} ,
      {-37,-29, 26,2, 47,"Right Middle Frontal Gyrus.............."} ,
      {  7, 32, 53,2, 48,"Left  Paracentral Lobule................"} ,
      { -7, 32, 53,2, 48,"Right Paracentral Lobule................"} ,
      { 43, 25, 43,2, 49,"Left  Postcentral Gyrus................."} ,
      {-43, 25, 43,2, 49,"Right Postcentral Gyrus................."} ,
      { 44,  8, 38,2, 50,"Left  Precentral Gyrus.................."} ,
      {-44,  8, 38,2, 50,"Right Precentral Gyrus.................."} ,
      { 19,-40, 27,2, 51,"Left  Superior Frontal Gyrus............"} ,
      {-19,-40, 27,2, 51,"Right Superior Frontal Gyrus............"} ,
      {  9,-24, 35,2, 52,"Left  Medial Frontal Gyrus.............."} ,
      { -9,-24, 35,2, 52,"Right Medial Frontal Gyrus.............."} ,
      { 22,  1,  2,2, 70,"Left  Lentiform Nucleus................."} ,
      {-22,  1,  2,2, 70,"Right Lentiform Nucleus................."} ,
      {  4,  3, -9,4, 72,"Left  Hypothalamus......................"} ,
      { -4,  3, -9,4, 72,"Right Hypothalamus......................"} ,
      {  5, 19, -4,4, 73,"Left  Red Nucleus......................."} ,
      { -5, 19, -4,4, 73,"Right Red Nucleus......................."} ,
      { 11, 18, -7,4, 74,"Left  Substantia Nigra.................."} ,
      {-11, 18, -7,4, 74,"Right Substantia Nigra.................."} ,
      { 32,  1,  5,2, 75,"Left  Claustrum........................."} ,
      {-32,  1,  5,2, 75,"Right Claustrum........................."} ,
      { 12, 19,  8,2, 76,"Left  Thalamus.........................."} ,
      {-12, 19,  8,2, 76,"Right Thalamus.........................."} ,
      { 11, -7,  9,2, 77,"Left  Caudate..........................."} ,
      {-11, -7,  9,2, 77,"Right Caudate..........................."} ,
      { 27, 35,  9,4,124,"Left  Caudate Tail......................"} ,
      {-27, 35,  9,4,124,"Right Caudate Tail......................"} ,
      { 12, -6, 14,4,125,"Left  Caudate Body......................"} ,
      {-12, -6, 14,4,125,"Right Caudate Body......................"} ,
      {  9,-13,  0,4,126,"Left  Caudate Head......................"} ,
      { -9,-13,  0,4,126,"Right Caudate Head......................"} ,
      { 11,  6,  9,4,128,"Left  Ventral Anterior Nucleus.........."} ,
      {-11,  6,  9,4,128,"Right Ventral Anterior Nucleus.........."} ,
      { 15, 20,  4,4,129,"Left  Ventral Posterior Medial Nucleus.."} ,
      {-15, 20,  4,4,129,"Right Ventral Posterior Medial Nucleus.."} ,
      { 18, 19,  5,4,130,"Left  Ventral Posterior Lateral Nucleus."} ,
      {-18, 19,  5,4,130,"Right Ventral Posterior Lateral Nucleus."} ,
      {  6, 16,  8,4,131,"Left  Medial Dorsal Nucleus............."} ,
      { -6, 16,  8,4,131,"Right Medial Dorsal Nucleus............."} ,
      { 12, 20, 16,4,132,"Left  Lateral Dorsal Nucleus............"} ,
      {-12, 20, 16,4,132,"Right Lateral Dorsal Nucleus............"} ,
      { 16, 27,  8,4,133,"Left  Pulvinar.........................."} ,
      {-16, 27,  8,4,133,"Right Pulvinar.........................."} ,
      { 17, 20, 14,4,134,"Left  Lateral Posterior Nucleus........."} ,
      {-17, 20, 14,4,134,"Right Lateral Posterior Nucleus........."} ,
      { 14, 12,  9,4,135,"Left  Ventral Lateral Nucleus..........."} ,
      {-14, 12,  9,4,135,"Right Ventral Lateral Nucleus..........."} ,
      {  7, 18, 16,4,136,"Left  Midline Nucleus..................."} ,
      { -7, 18, 16,4,136,"Right Midline Nucleus..................."} ,
      {  8,  8, 12,4,137,"Left  Anterior Nucleus.................."} ,   /* 04 Mar 2002 */
      { -8,  8, 12,4,137,"Right Anterior Nucleus.................."} ,
      { 11, 20,  2,4,138,"Left  Mammillary Body..................."} ,
      {-11, 20,  2,4,138,"Right Mammillary Body..................."} ,
      { 15,  4, -2,4,144,"Left  Medial Globus Pallidus............"} ,
      {-15,  4, -2,4,144,"Right Medial Globus Pallidus............"} ,
      { 20,  5,  0,4,145,"Left  Lateral Globus Pallidus..........."} ,
      {-20,  5,  0,4,145,"Right Lateral Globus Pallidus..........."} ,
      { 24,  0,  3,4,151,"Left  Putamen..........................."} ,
      {-24,  0,  3,4,151,"Right Putamen..........................."} ,
      { 12, -8, -8,4,146,"Left  Nucleus Accumbens................."} , /* 20 Aug */
      {-12, -8, -8,4,146,"Right Nucleus Accumbens................."} , /* 2001 */
      { 17, 24, -2,4,147,"Left  Medial Geniculum Body............."} ,
      {-17, 24, -2,4,147,"Right Medial Geniculum Body............."} ,
      { 22, 24, -1,4,148,"Left  Lateral Geniculum Body............"} ,
      {-22, 24, -1,4,148,"Right Lateral Geniculum Body............"} ,
      { 10, 13, -3,4,149,"Left  Subthalamic Nucleus..............."} ,
      {-10, 13, -3,4,149,"Right Subthalamic Nucleus..............."} ,
      { 53, 19, 50,4, 81,"Left  Brodmann area 1..................."} ,
      {-53, 19, 50,4, 81,"Right Brodmann area 1..................."} ,
      { 49, 26, 43,4, 82,"Left  Brodmann area 2..................."} ,
      {-49, 26, 43,4, 82,"Right Brodmann area 2..................."} ,
      { 39, 23, 50,4, 83,"Left  Brodmann area 3..................."} ,
      {-39, 23, 50,4, 83,"Right Brodmann area 3..................."} ,
      { 39, 18, 49,4, 84,"Left  Brodmann area 4..................."} ,
      {-39, 18, 49,4, 84,"Right Brodmann area 4..................."} ,
      { 16, 40, 57,4, 85,"Left  Brodmann area 5..................."} ,
      {-16, 40, 57,4, 85,"Right Brodmann area 5..................."} ,
      { 29,  0, 50,4, 86,"Left  Brodmann area 6..................."} ,
      {-29,  0, 50,4, 86,"Right Brodmann area 6..................."} ,
      { 16, 60, 48,4, 87,"Left  Brodmann area 7..................."} ,
      {-16, 60, 48,4, 87,"Right Brodmann area 7..................."} ,
      { 24,-30, 44,4, 88,"Left  Brodmann area 8..................."} ,
      {-24,-30, 44,4, 88,"Right Brodmann area 8..................."} ,
      { 32,-33, 30,4, 89,"Left  Brodmann area 9..................."} ,
      {-32,-33, 30,4, 89,"Right Brodmann area 9..................."} ,
      { 24,-56,  6,4, 90,"Left  Brodmann area 10.................."} ,
      {-24,-56,  6,4, 90,"Right Brodmann area 10.................."} ,
      { 17,-43,-18,4, 91,"Left  Brodmann area 11.................."} ,
      {-17,-43,-18,4, 91,"Right Brodmann area 11.................."} ,
      { 39,  4,  8,4, 93,"Left  Brodmann area 13.................."} ,
      {-39,  4,  8,4, 93,"Right Brodmann area 13.................."} ,
      { 10, 88,  5,4, 94,"Left  Brodmann area 17.................."} ,
      {-10, 88,  5,4, 94,"Right Brodmann area 17.................."} ,
      { 19, 85,  4,4, 95,"Left  Brodmann area 18.................."} ,
      {-19, 85,  4,4, 95,"Right Brodmann area 18.................."} ,
      { 34, 80, 18,4, 96,"Left  Brodmann area 19.................."} ,
      {-34, 80, 18,4, 96,"Right Brodmann area 19.................."} ,
      { 47, 21,-23,4, 97,"Left  Brodmann area 20.................."} ,
      {-47, 21,-23,4, 97,"Right Brodmann area 20.................."} ,
      { 58, 18,-10,4, 98,"Left  Brodmann area 21.................."} ,
      {-58, 18,-10,4, 98,"Right Brodmann area 21.................."} ,
      { 57, 23,  5,4, 99,"Left  Brodmann area 22.................."} ,
      {-57, 23,  5,4, 99,"Right Brodmann area 22.................."} ,
      {  4, 37, 24,4,100,"Left  Brodmann area 23.................."} ,
      { -4, 37, 24,4,100,"Right Brodmann area 23.................."} ,
      {  6, -6, 30,4,101,"Left  Brodmann area 24.................."} ,
      { -6, -6, 30,4,101,"Right Brodmann area 24.................."} ,
      {  6,-15,-13,4,102,"Left  Brodmann area 25.................."} ,
      { -6,-15,-13,4,102,"Right Brodmann area 25.................."} ,
      { 15, 35,  0,4,103,"Left  Brodmann area 27.................."} ,
      {-15, 35,  0,4,103,"Right Brodmann area 27.................."} ,
      { 22, -2,-24,4,104,"Left  Brodmann area 28.................."} ,
      {-22, -2,-24,4,104,"Right Brodmann area 28.................."} ,
      {  6, 48, 11,4,105,"Left  Brodmann area 29.................."} ,
      { -6, 48, 11,4,105,"Right Brodmann area 29.................."} ,
      { 13, 62, 10,4,106,"Left  Brodmann area 30.................."} ,
      {-13, 62, 10,4,106,"Right Brodmann area 30.................."} ,
      {  9, 47, 32,4,107,"Left  Brodmann area 31.................."} ,
      { -9, 47, 32,4,107,"Right Brodmann area 31.................."} ,
      {  8,-24, 30,4,108,"Left  Brodmann area 32.................."} ,
      { -8,-24, 30,4,108,"Right Brodmann area 32.................."} ,
      {  5,-12, 24,4,109,"Left  Brodmann area 33.................."} ,
      { -5,-12, 24,4,109,"Right Brodmann area 33.................."} ,
      { 18,  0,-16,4,110,"Left  Brodmann area 34.................."} ,
      {-18,  0,-16,4,110,"Right Brodmann area 34.................."} ,
      { 23, 25,-15,4,111,"Left  Brodmann area 35.................."} ,
      {-23, 25,-15,4,111,"Right Brodmann area 35.................."} ,
      { 33, 33,-15,4,112,"Left  Brodmann area 36.................."} ,
      {-33, 33,-15,4,112,"Right Brodmann area 36.................."} ,
      { 48, 55, -7,4,113,"Left  Brodmann area 37.................."} ,
      {-48, 55, -7,4,113,"Right Brodmann area 37.................."} ,
      { 41,-12,-23,4,114,"Left  Brodmann area 38.................."} ,
      {-41,-12,-23,4,114,"Right Brodmann area 38.................."} ,
      { 48, 64, 28,4,115,"Left  Brodmann area 39.................."} ,
      {-48, 64, 28,4,115,"Right Brodmann area 39.................."} ,
      { 51, 40, 38,4,116,"Left  Brodmann area 40.................."} ,
      {-51, 40, 38,4,116,"Right Brodmann area 40.................."} ,
      { 47, 26, 11,4,117,"Left  Brodmann area 41.................."} ,
      {-47, 26, 11,4,117,"Right Brodmann area 41.................."} ,
      { 63, 22, 12,4,118,"Left  Brodmann area 42.................."} ,
      {-63, 22, 12,4,118,"Right Brodmann area 42.................."} ,
      { 58, 10, 16,4,119,"Left  Brodmann area 43.................."} ,
      {-58, 10, 16,4,119,"Right Brodmann area 43.................."} ,
      { 53,-11, 12,4,120,"Left  Brodmann area 44.................."} ,
      {-53,-11, 12,4,120,"Right Brodmann area 44.................."} ,
      { 54,-23, 10,4,121,"Left  Brodmann area 45.................."} ,
      {-54,-23, 10,4,121,"Right Brodmann area 45.................."} ,
      { 50,-38, 16,4,122,"Left  Brodmann area 46.................."} ,
      {-50,-38, 16,4,122,"Right Brodmann area 46.................."} ,
      { 38,-24,-11,4,123,"Left  Brodmann area 47.................."} ,
      {-38,-24,-11,4,123,"Right Brodmann area 47.................."} ,
      {  2, 65,-32,2, 53,"Left  Uvula of Vermis..................."} ,
      { -2, 65,-32,2, 53,"Right Uvula of Vermis..................."} ,
      {  2, 73,-28,2, 54,"Left  Pyramis of Vermis................."} ,
      { -2, 73,-28,2, 54,"Right Pyramis of Vermis................."} ,
      {  2, 71,-24,2, 55,"Left  Tuber of Vermis..................."} ,
      { -2, 71,-24,2, 55,"Right Tuber of Vermis..................."} ,
      {  2, 72,-17,2, 56,"Left  Declive of Vermis................."} ,
      { -2, 72,-17,2, 56,"Right Declive of Vermis................."} ,
      {  3, 63, -3,2, 57,"Left  Culmen of Vermis.................."} ,
      { -3, 63, -3,2, 57,"Right Culmen of Vermis.................."} ,
      { 28, 51,-36,2, 58,"Left  Cerebellar Tonsil................."} ,
      {-28, 51,-36,2, 58,"Right Cerebellar Tonsil................."} ,
      { 29, 71,-38,2, 59,"Left  Inferior Semi-Lunar Lobule........"} ,
      {-29, 71,-38,2, 59,"Right Inferior Semi-Lunar Lobule........"} ,
      {  7, 54,-20,2, 60,"Left  Fastigium........................."} ,
      { -7, 54,-20,2, 60,"Right Fastigium........................."} ,
      {  7, 55,-27,2, 61,"Left  Nodule............................"} ,
      { -7, 55,-27,2, 61,"Right Nodule............................"} ,
      { 21, 76,-26,2, 62,"Left  Uvula............................."} ,
      {-21, 76,-26,2, 62,"Right Uvula............................."} ,
      { 27, 74,-30,2, 63,"Left  Pyramis..........................."} ,
      {-27, 74,-30,2, 63,"Right Pyramis..........................."} ,
      { 20, 46,-16,2, 66,"Left  Culmen............................"} ,
      {-20, 46,-16,2, 66,"Right Culmen............................"} ,
      { 26, 69,-17,2, 65,"Left  Declive..........................."} ,
      {-26, 69,-17,2, 65,"Right Declive..........................."} ,
      { 14, 54,-23,4,127,"Left  Dentate..........................."} ,
      {-14, 54,-23,4,127,"Right Dentate..........................."} ,
      { 44, 71,-27,2, 64,"Left  Tuber............................."} ,
      {-44, 71,-27,2, 64,"Right Tuber............................."} ,
      {  4, 45,-13,2, 67,"Left  Cerebellar Lingual................"} ,
      { -4, 45,-13,2, 67,"Right Cerebellar Lingual................"}
} ;

char * TTO_labels[TTO_COUNT] ;

int TTO_labeled = 0 ;  /* flag that labels not yet computed */
int TTO_current = 0 ;  /* last chosen TTO */

/*! Atlas by Eickhoff, Zilles et al.
current version of list from MacroLabels_Lookup.rtf */
ML_EZ_point ML_EZ_list[ML_EZ_COUNT] = {
   {   1 , "Left Precentral Gyrus............................."},
   {   2 , "Right Precentral Gyrus............................"},
   {   3 , "Left Superior Frontal Gyrus......................."},
   {   4 , "Right Superior Frontal Gyrus......................"},
   {   5 , "Left Superior Orbital Gyrus......................."},
   {   6 , "Right Superior Orbital Gyrus......................"},
   {   7 , "Left Middle Frontal Gyrus........................."},
   {   8 , "Right Middle Frontal Gyrus........................"},
   {   9 , "Left Middle Orbital Gyrus........................."},
   {  10 , "Right Middle Orbital Gyrus........................"},
   {  11 , "Left Inferior Frontal Gyrus (p. Opercularis)......"},
   {  12 , "Right Inferior Frontal Gyrus (p. Opercularis)....."},
   {  13 , "Left Inferior Frontal Gyrus (p. Triangularis)....."},
   {  14 , "Right Inferior Frontal Gyrus (p. Triangularis)...."},
   {  15 , "Left Inferior Frontal Gyrus (p. Orbitalis)........"},
   {  16 , "Right Inferior Frontal Gyrus (p. Orbitalis)......."},
   {  17 , "Left Rolandic Operculum..........................."},
   {  18 , "Right Rolandic Operculum.........................."},
   {  19 , "Left SMA.........................................."},
   {  20 , "Right SMA........................................."},
   {  21 , "Left Olfactory cortex............................."},
   {  22 , "Right Olfactory cortex............................"},
   {  23 , "Left Superior Medial Gyrus........................"},
   {  24 , "Right Superior Medial Gyrus......................."},
   {  25 , "Left Mid Orbital Gyrus............................"},
   {  26 , "Right Mid Orbital Gyrus..........................."},
   {  27 , "Left Rectal Gyrus................................."},
   {  28 , "Right Rectal Gyrus................................"},
   {  29 , "Left Insula Lobe.................................."},
   {  30 , "Right Insula Lobe................................."},
   {  31 , "Left Anterior Cingulate Cortex...................."},
   {  32 , "Right Anterior Cingulate Cortex..................."},
   {  33 , "Left Middle Cingulate Cortex......................"},
   {  34 , "Right Middle Cingulate Cortex....................."},
   {  35 , "Left Posterior Cingulate Cortex..................."},
   {  36 , "Right Posterior Cingulate Cortex.................."},
   {  37 , "Left Hippocampus.................................."},
   {  38 , "Right Hippocampus................................."},
   {  39 , "Left ParaHippocampal Gyrus........................"},
   {  40 , "Right ParaHippocampal Gyrus......................."},
   {  41 , "Left Amygdala....................................."},
   {  42 , "Right Amygdala...................................."},
   {  43 , "Left Calcarine Gyrus.............................."},
   {  44 , "Right Calcarine Gyrus............................."},
   {  45 , "Left Cuneus......................................."},
   {  46 , "Right Cuneus......................................"},
   {  47 , "Left Lingual Gyrus................................"},
   {  48 , "Right Lingual Gyrus..............................."},
   {  49 , "Left Superior Occipital Gyrus....................."},
   {  50 , "Right Superior Occipital Gyrus...................."},
   {  51 , "Left Middle Occipital Gyrus......................."},
   {  52 , "Right Middle Occipital Gyrus......................"},
   {  53 , "Left Inferior Occipital Gyrus....................."},
   {  54 , "Right Inferior Occipital Gyrus...................."},
   {  55 , "Left Fusiform Gyrus..............................."},
   {  56 , "Right Fusiform Gyrus.............................."},
   {  57 , "Left Postcentral Gyrus............................"},
   {  58 , "Right Postcentral Gyrus..........................."},
   {  59 , "Left Superior Parietal Lobule ...................."},
   {  60 , "Right Superior Parietal Lobule ..................."},
   {  61 , "Left Inferior Parietal Lobule ...................."},
   {  62 , "Right Inferior Parietal Lobule ..................."},
   {  63 , "Left SupraMarginal Gyrus.........................."},
   {  64 , "Right SupraMarginal Gyrus........................."},
   {  65 , "Left Angular Gyrus................................"},
   {  66 , "Right Angular Gyrus..............................."},
   {  67 , "Left Precuneus...................................."},
   {  68 , "Right Precuneus..................................."},
   {  69 , "Left Paracentral Lobule..........................."},
   {  70 , "Right Paracentral Lobule.........................."},
   {  71 , "Left Caudate Nucleus.............................."},
   {  72 , "Right Caudate Nucleus............................."},
   {  73 , "Left Putamen......................................"},
   {  74 , "Right Putamen....................................."},
   {  75 , "Left Pallidum....................................."},
   {  76 , "Right Pallidum...................................."},
   {  77 , "Left Thalamus....................................."},
   {  78 , "Right Thalamus...................................."},
   {  79 , "Left Heschls Gyrus................................"},
   {  80 , "Right Heschls Gyrus..............................."},
   {  81 , "Left Superior Temporal Gyrus......................"},
   {  82 , "Right Superior Temporal Gyrus....................."},
   {  83 , "Left Temporal Pole................................"},
   {  84 , "Right Temporal Pole..............................."},
   {  85 , "Left Middle Temporal Gyrus........................"},
   {  86 , "Right Middle Temporal Gyrus......................."},
   {  87 , "Left Medial Temporal Pole........................."},
   {  88 , "Right Medial Temporal Pole........................"},
   {  89 , "Left Inferior Temporal Gyrus......................"},
   {  90 , "Right Inferior Temporal Gyrus....................."},
   {  91 , "Left Cerebelum (Crus 1)..........................."},
   {  92 , "Right Cerebelum (Crus 1).........................."},
   {  93 , "Left Cerebelum (Crus 2)..........................."},
   {  94 , "Right Cerebelum (Crus 2).........................."},
   {  95 , "Left Cerebelum (III).............................."},
   {  96 , "Right Cerebelum (III)............................."},
   {  97 , "Left Cerebelum (IV-V)............................."},
   {  98 , "Right Cerebelum (IV-V)............................"},
   {  99 , "Left Cerebelum (VI)..............................."},
   { 100 , "Right Cerebelum (VI).............................."},
   { 101 , "Left Cerebelum (VII).............................."},
   { 102 , "Right Cerebelum (VII)............................."},
   { 103 , "Left Cerebelum (VIII)............................."},
   { 104 , "Right Cerebelum (VIII)............................"},
   { 105 , "Left Cerebelum (IX)..............................."},
   { 106 , "Right Cerebelum (IX).............................."},
   { 107 , "Left Cerebelum (X)................................"},
   { 108 , "Right Cerebelum (X)..............................."},
   { 109 , "Cerebellar Vermis (1/2)..........................."},
   { 110 , "Cerebellar Vermis (3)............................."},
   { 111 , "Cerebellar Vermis (4/5)..........................."},
   { 112 , "Cerebellar Vermis (6)............................."},
   { 113 , "Cerebellar Vermis (7)............................."},
   { 114 , "Cerebellar Vermis (8)............................."},
   { 115 , "Cerebellar Vermis (9)............................."},
   { 116 , "Cerebellar Vermis (10)............................"}
};
   
char * ML_EZ_labels[ML_EZ_COUNT] ;

int ML_EZ_labeled = 0 ;  /* flag that labels not yet computed */
int ML_EZ_current = 0 ;  /* last chosen ML_EZ */

/* Left Right atlas by Eickhoff & Zilles */
LR_EZ_point LR_EZ_list[LR_EZ_COUNT] = {
   {   0 , "Non-Brain..."},
   {   1 , "Right Brain."},
   {   2 , "Left Brain.."},
};

char * LR_EZ_labels[LR_EZ_COUNT] ;

int LR_EZ_labeled = 0 ;  /* flag that labels not yet computed */
int LR_EZ_current = 0 ;  /* last chosen LR_EZ */

/* PMAPS atlases by Eickhoff and Zilles
current version of list from Lookup_AllAreas_v12.rtf */
CA_EZ_point CA_EZ_list[CA_EZ_COUNT] = { 
      {  "Amygdala (CM)...........................",  124, "Amygdala_CM................" },
      {  "Amygdala (LB)...........................",  220, "Amygdala_LB................" },
      {  "Amygdala (SF)...........................",  154, "Amygdala_SF................" },
      {  "Area 1..................................",  184, "PSC_1......................" },
      {  "Area 2..................................",  232, "PSC_2......................" },
      {  "Area 3a.................................",  226, "PSC_3a....................." },
      {  "Area 3b.................................",  142, "PSC_3b....................." },
      {  "Area 4a.................................",  118, "PMC_4a....................." },
      {  "Area 4p.................................",  178, "PMC_4p....................." },
      {  "Area 6..................................",  214, "premotor_6................." },
      {  "Area 17.................................",  148, "visual_V1.................." },
      {  "Area 18.................................",  190, "visual_V2.................." },
      {  "Area 44.................................",  166, "Broca_44..................." },
      {  "Area 45.................................",  112, "Broca_45..................." },
      {  "Hippocampus (CA)........................",  100, "Hippocampus_CA............." },
      {  "Hippocampus (EC)........................",  136, "Hippocampus_EC............." },
      {  "Hippocampus (FD)........................",  172, "Hippocampus_FD............." },
      {  "Hippocampus (HATA)......................",  202, "Hippocampus_HATA..........." },
      {  "Hippocampus (SUB).......................",  250, "Hippocampus_SUB............" },
      {  "OP 1....................................",  106, "SII_OP1...................." },
      {  "OP 2....................................",  208, "SII_OP2...................." },
      {  "OP 3....................................",  160, "SII_OP3...................." },
      {  "OP 4....................................",  244, "SII_OP4...................." },
      {  "TE 1.0..................................",  130, "PAC_TE10..................." },
      {  "TE 1.1..................................",  196, "PAC_TE11..................." },
      {  "TE 1.2..................................",  238, "PAC_TE12..................." },      
   };
   
char * CA_EZ_labels[CA_EZ_COUNT] ;

int CA_EZ_labeled = 0 ;  /* flag that labels not yet computed */
int CA_EZ_current = 0 ;  /* last chosen CA_EZ */


/*-----------------------------------------------------------------------*/

THD_3dim_dataset * TT_retrieve_atlas(void)
{
   if( have_dseTT < 0 ) TT_load_atlas() ;
   return dseTT ;                         /* might be NULL */
}

/*-----------------------------------------------------------------------*/

THD_3dim_dataset * TT_retrieve_atlas_big(void) /* 01 Aug 2001 */
{
   char sbuf[256];
   
   if( dseTT_big != NULL ) return dseTT_big ;
   if( have_dseTT < 0    ) TT_load_atlas() ;
   if( dseTT == NULL     ) return NULL ;
   sprintf(sbuf,"%s_big", TT_DAEMON_TT_PREFIX);
   dseTT_big = THD_zeropad( dseTT , 10,0,0,0,0,0 , sbuf , 0 ) ;
   DSET_unload( dseTT ) ; /* probably won't need again */
   return dseTT_big ;
}

/*-----------------------------------------------------------------------*/

THD_3dim_dataset * TT_retrieve_atlas_either(void) /* 22 Aug 2001 */
{
   if( dseTT_big != NULL ) return dseTT_big ;
   if( dseTT     != NULL ) return dseTT     ;
   if( have_dseTT < 0    ) TT_load_atlas()  ;
   return dseTT ;
}

/*-----------------------------------------------------------------------*/

THD_3dim_dataset * get_altas(char *epath, char *aname) 
{
   char dname[THD_MAX_NAME], ename[THD_MAX_NAME], *elocal=NULL, *eee;
   THD_3dim_dataset *dset = NULL;
   int epos=0, ll=0, ii=0, id = 0;
   
   ENTRY("get_altas");

   if( epath != NULL ){ /* A path or dset was specified */
      if (aname == NULL) { /* all in epath */
         dset = THD_open_one_dataset( epath ) ;  /* try to open it */
      } else  {
         strncpy(dname,epath, (THD_MAX_NAME-2)*sizeof(char)) ;
         ii = strlen(dname);
         if (dname[ii-1] != '/') {
            dname[ii] = '/'; dname[ii+1] = '\0';
         }
         strncat(dname,aname, THD_MAX_NAME - strlen(dname)) ;               /* add dataset name */
         dset = THD_open_one_dataset( dname ) ; 
      }
      if( !dset ){                     /* got it!!! */
         ERROR_message("Failed to read dset %s\n", epath);
         RETURN(dset); 
      }
   } else { /* no path given */
      if (aname == NULL) { /* nothing to work with here !*/
         ERROR_message("No path, no name, no soup for you.\n");
         RETURN(dset); 
      }
      /* a name was given, search for it */
      /*----- get path to search -----*/

                          epath = getenv("AFNI_PLUGINPATH") ;
      if( epath == NULL ) epath = getenv("AFNI_PLUGIN_PATH") ;
      if( epath == NULL ) epath = getenv("PATH") ;
      if( epath == NULL ) RETURN(dset) ;

      /*----- copy path list into local memory -----*/

      ll = strlen(epath) ;
      elocal = AFMALL(char, sizeof(char) * (ll+2) ) ;

      /*----- put a blank at the end -----*/

      strcpy( elocal , epath ) ; elocal[ll] = ' ' ; elocal[ll+1] = '\0' ;

      /*----- replace colons with blanks -----*/

      for( ii=0 ; ii < ll ; ii++ )
        if( elocal[ii] == ':' ) elocal[ii] = ' ' ;

      /*----- extract blank delimited strings;
              use as directory names to look for atlas -----*/

      epos = 0 ;

      do{
         ii = sscanf( elocal+epos , "%s%n" , ename , &id ); /* next substring */
         if( ii < 1 ) break ;                               /* none -> done   */

         epos += id ;                                 /* char after last scanned */

         ii = strlen(ename) ;                         /* make sure name has   */
         if( ename[ii-1] != '/' ){                    /* a trailing '/' on it */
             ename[ii]  = '/' ; ename[ii+1] = '\0' ;
         }
         strcpy(dname,ename) ;
         strcat(dname,aname) ;               /* add dataset name */

         dset = THD_open_one_dataset( dname ) ;      /* try to open it */

         if( dset != NULL ){                         /* got it!!! */
            free(elocal); RETURN(dset);
         }
         
      } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */


   } /* No path given */

   RETURN(dset); 
}

/*-----------------------------------------------------------------------*/
int TT_load_atlas(void)
{
   char *epath, sbuf[256] ;

ENTRY("TT_load_atlas") ;

   if( have_dseTT >= 0 ) RETURN(have_dseTT) ;  /* for later calls */

   have_dseTT = 0 ;  /* don't have it yet */

   /*----- 20 Aug 2001: see if user specified alternate database -----*/

   epath = getenv("AFNI_TTATLAS_DATASET") ;   /* suggested path, if any */
   sprintf(sbuf,"%s+tlrc", TT_DAEMON_TT_PREFIX);
   dseTT = get_altas( epath, sbuf ) ;  /* try to open it */
   if (!dseTT) { /* try for NIFTI */
      sprintf(sbuf,"%s.nii.gz", TT_DAEMON_TT_PREFIX);
      dseTT = get_altas( epath, sbuf) ;
   }
   if( dseTT != NULL ){                     /* got it!!! */
      have_dseTT = 1; RETURN(1);
   }
      

   RETURN(0) ; /* got here -> didn't find it */
}

/*----------------------------------------------------------------------
  Allows the program to purge the memory used by the TT atlas dataset
------------------------------------------------------------------------*/

void TT_purge_atlas(void)
{
  PURGE_DSET(dseTT) ; return ;
}

void TT_purge_atlas_big(void)
{
   if( dseTT_big != NULL ){ DSET_delete(dseTT_big) ; dseTT_big = NULL ; }
   return ;
}

/*----------------------------------------------------------------------
   Begin coordinate transformation functions
------------------------------------------------------------------------*/
static THD_3dim_dataset *MNI_N27_to_TLRC_DSET = NULL;

/*------------------------------------------------------------------------
   Forward transform a vector following a warp
   Don't mess with this function, many programs use it. ZSS Feb 06
--------------------------------------------------------------------------*/

THD_fvec3 AFNI_forward_warp_vector( THD_warp * warp , THD_fvec3 old_fv )
{
   THD_fvec3 new_fv ;

   if( warp == NULL ) return old_fv ;

   switch( warp->type ){

      default: new_fv = old_fv ; break ;

      case WARP_TALAIRACH_12_TYPE:{
         THD_linear_mapping map ;
         int iw ;

         /* forward transform each possible case,
            and test if result is in bot..top of defined map */

         for( iw=0 ; iw < 12 ; iw++ ){
            map    = warp->tal_12.warp[iw] ;
            new_fv = MATVEC_SUB(map.mfor,old_fv,map.bvec) ;

            if( new_fv.xyz[0] >= map.bot.xyz[0] &&
                new_fv.xyz[1] >= map.bot.xyz[1] &&
                new_fv.xyz[2] >= map.bot.xyz[2] &&
                new_fv.xyz[0] <= map.top.xyz[0] &&
                new_fv.xyz[1] <= map.top.xyz[1] &&
                new_fv.xyz[2] <= map.top.xyz[2]   ) break ;  /* leave loop */
         }
      }
      break ;

      case WARP_AFFINE_TYPE:{
         THD_linear_mapping map = warp->rig_bod.warp ;
         new_fv = MATVEC_SUB(map.mfor,old_fv,map.bvec) ;
      }
      break ;

   }
   return new_fv ;
}

/*------------------------------------------------------------------------
   Backward transform a vector following a warp
   Don't mess with this function, many programs use it. ZSS Feb 06
--------------------------------------------------------------------------*/
THD_fvec3 AFNI_backward_warp_vector( THD_warp * warp , THD_fvec3 old_fv )
{
   THD_fvec3 new_fv ;

   if( warp == NULL ) return old_fv ;

   switch( warp->type ){

      default: new_fv = old_fv ; break ;

      case WARP_TALAIRACH_12_TYPE:{
         THD_linear_mapping map ;
         int iw ;

         /* test if input is in bot..top of each defined map */

         for( iw=0 ; iw < 12 ; iw++ ){
            map = warp->tal_12.warp[iw] ;

            if( old_fv.xyz[0] >= map.bot.xyz[0] &&
                old_fv.xyz[1] >= map.bot.xyz[1] &&
                old_fv.xyz[2] >= map.bot.xyz[2] &&
                old_fv.xyz[0] <= map.top.xyz[0] &&
                old_fv.xyz[1] <= map.top.xyz[1] &&
                old_fv.xyz[2] <= map.top.xyz[2]   ) break ;  /* leave loop */
         }
         new_fv = MATVEC_SUB(map.mbac,old_fv,map.svec) ;
      }
      break ;

      case WARP_AFFINE_TYPE:{
         THD_linear_mapping map = warp->rig_bod.warp ;
         new_fv = MATVEC_SUB(map.mbac,old_fv,map.svec) ;
      }
      break ;

   }
   return new_fv ;
}
 
/*!
   Coordinate transformation between N27 MNI 
   and AFNI's TLRC. Transform was obtained by manually AFNI TLRCing
   the N27 dset supplied in Eickhoff & Zilles' v12 dbase before 
   the volume itself was placed in MNI Anatomical space as was done in v13.
   Input and output are in RAI
*/
THD_fvec3 THD_mni__tta_N27( THD_fvec3 mv, int dir )
{
   static THD_talairach_12_warp *ww=NULL;
   float mx,my,mz , tx,ty,tz ;
   int iw, ioff;
   THD_fvec3 tv, tv2;

   tx = ty = tz = -9000.0;
   LOAD_FVEC3( tv , tx,ty,tz ) ;
   LOAD_FVEC3( tv2 , tx,ty,tz ) ;
   
   if (0) { /* Meth. 1, Left here should we allow user someday to specify a .HEAD with their own transform... */
      INFO_message("What about the path?\nNeed something fool proof\n");
      if (!MNI_N27_to_TLRC_DSET) {
        MNI_N27_to_TLRC_DSET = THD_open_one_dataset( MNI_N27_to_AFNI_TLRC_HEAD ) ;
        if (!MNI_N27_to_TLRC_DSET) {
         ERROR_message("Failed to open transform dset %s\nNo transformation done.", MNI_N27_to_AFNI_TLRC_HEAD ) ;
         return tv ;
        }
      }
      /* get the warp */
      if (!MNI_N27_to_TLRC_DSET->warp) {
         ERROR_message("No Warp Found in %s\nNo transformation done.", MNI_N27_to_AFNI_TLRC_HEAD ) ;
         return tv ;   
      }
      if (MNI_N27_to_TLRC_DSET->warp->type != WARP_TALAIRACH_12_TYPE) {
         ERROR_message("Warp of unexpected type in %s.\nNo transformation done.", MNI_N27_to_AFNI_TLRC_HEAD ) ;
         return tv ; 
      }

      if (dir > 0) tv = AFNI_forward_warp_vector(MNI_N27_to_TLRC_DSET->warp, mv);
      else tv = AFNI_backward_warp_vector(MNI_N27_to_TLRC_DSET->warp, mv);
      if (0) {
         INFO_message("tv(Meth 1): %f %f %f\n", tv.xyz[0], tv.xyz[1], tv.xyz[2]);
      }
   }
   
   /* Meth 2, xform in code, more fool proof*/
   if (!ww) {
      /* load the transform */
      ww = myXtNew( THD_talairach_12_warp ) ;
      ww->type = WARP_TALAIRACH_12_TYPE;
      ww->resam_type = 0;
      for (iw=0; iw < 12; ++iw) {
         ww->warp[iw].type = MAPPING_LINEAR_TYPE ;

         ioff = iw * MAPPING_LINEAR_FSIZE ;
         COPY_INTO_STRUCT( ww->warp[iw] ,
                           MAPPING_LINEAR_FSTART ,
                           float ,
                           &(MNI_N27_to_AFNI_TLRC_WRP_VEC[ioff]) ,
                           MAPPING_LINEAR_FSIZE ) ;

      }
   }
   
   if (!ww) {
      ERROR_message("Failed to form built-in warp.");
      return tv2;
   } else {
      if (dir > 0) tv2 = AFNI_forward_warp_vector((THD_warp *)ww, mv);
      else tv2 = AFNI_backward_warp_vector((THD_warp *)ww, mv);
   }
   
   if (0) {
      INFO_message("tv2(Meth. 2): %f %f %f\n", tv2.xyz[0], tv2.xyz[1], tv2.xyz[2]);
   }
   
   return tv2 ;
}
 
THD_fvec3 THD_mni_to_tta_N27( THD_fvec3 mv )
{
   return (THD_mni__tta_N27( mv , 1));
}

THD_fvec3 THD_tta_to_mni_N27( THD_fvec3 mv )
{
   return (THD_mni__tta_N27( mv , -1));
}

THD_fvec3 THD_mnia_to_tta_N27( THD_fvec3 mv )
{
   THD_fvec3 mva;
   /*go from MNI Anat to MNI*/
   mva.xyz[0] = mv.xyz[0] + 0.0 ;
   mva.xyz[1] = mv.xyz[1] + 4.0 ;
   mva.xyz[2] = mv.xyz[2] - 5.0 ;
   
   return (THD_mni__tta_N27( mva , 1));
}

THD_fvec3 THD_tta_to_mnia_N27( THD_fvec3 mv )
{
   THD_fvec3 mva;
   
   mva = THD_mni__tta_N27( mv , -1);
   
   /*go from MNI to MNI Anat */
   mva.xyz[0] = mva.xyz[0] + 0.0 ;
   mva.xyz[1] = mva.xyz[1] - 4.0 ;
   mva.xyz[2] = mva.xyz[2] + 5.0 ;
   
   return (mva);
}

/*! are we on the left or right of Colin? */
char MNI_Anatomical_Side(ATLAS_COORD ac)
{
   THD_ivec3 ijk ;
   int  ix,jy,kz , nx,ny,nz,nxy, ii=0, kk=0;
   byte *ba=NULL;
   byte LocalHead = 0;
   
   ENTRY("MNI_Anatomical_Side");
   
   if (ac.space != AFNI_TLRC_SPC) {
      ERROR_message("Coordinates must be in AFNI_TLRC_SPC");
      RETURN('u');
   }
   
   if (dseCA_EZ_LR == NULL) {
      if (LocalHead) fprintf(stderr,"Loading %s\n",  Atlas_Code_to_Atlas_Name(CA_EZ_LR_ATLAS));
      ii = CA_EZ_LR_load_atlas(); 
      if (ii == 0) {
         WARNING_message("Could not read LR atlas (dset %s+tlrc)", 
            Atlas_Code_to_Atlas_Dset_Name(CA_EZ_LR_ATLAS));
      }
   }
   
   if (dseCA_EZ_LR == NULL) {
      WARNING_message("Relying on x coordinate to guess side");
      if (ac.x<0.0) { 
         RETURN('r'); 
      } else { 
         RETURN('l'); 
      }
   } else {
      DSET_load(dseCA_EZ_LR);
      
      /* where are we in the ijk grid ? */
      ijk = THD_3dmm_to_3dind( dseCA_EZ_LR , TEMP_FVEC3(ac.x,ac.y,ac.z) ) ;  /* get indexes */
      UNLOAD_IVEC3(ijk,ix,jy,kz) ;                               /* from coords */

      nx = DSET_NX(dseCA_EZ_LR) ;               /* size of atlas dataset axes */
      ny = DSET_NY(dseCA_EZ_LR) ;
      nz = DSET_NZ(dseCA_EZ_LR) ; nxy = nx*ny ;

      /*-- check the exact input location --*/
      ba = DSET_BRICK_ARRAY(dseCA_EZ_LR,0);
      kk = ix + jy*nx + kz*nxy ;        /* index into brick arrays */
      if( ba[kk] == 2 ){
         RETURN('l'); 
      }else if( ba[kk] == 1 ){
         RETURN('r'); 
      }else {
         /* not in mask, use coord */
         if (ac.x<0.0) { 
            RETURN('r'); 
         } else { 
            RETURN('l'); 
         }
      }
   }
   
   /* should not get here */
   RETURN('u');
}
/*! What side are we on ?*/
char Atlas_Voxel_Side( THD_3dim_dataset *dset, int k1d, byte *lrmask)
{
   THD_ivec3 ijk ;
   THD_fvec3 xyz ;
   int  ix,jy,kz , nx,ny,nz,nxy, ii=0, kk=0;
   byte LocalHead = 0;
   
   ENTRY("Atlas_Voxel_Side");
   
   
   if ( lrmask ) { /* easy */
      if (lrmask[k1d] == 2 ){
         RETURN('l'); 
      }else if( lrmask[k1d] == 1 ){
         RETURN('r'); 
      } else {
         RETURN('u'); 
      }
   }
   
   if (!dset) {
      ERROR_message("Need an atlas dset");
      RETURN('u'); 
   }
   
   /* based on coords */
   nx = DSET_NX(dset) ;               /* size of atlas dataset axes */
   ny = DSET_NY(dset) ;
   nz = DSET_NZ(dset) ; nxy = nx*ny ;
   
   kz = (k1d / nxy); 
   jy = (k1d % nxy);   
   ix = (jy  % nx);  
   jy = (jy  / nx); 
   
   LOAD_IVEC3(ijk, ix, jy, kz);
   xyz = THD_3dind_to_3dmm(dset, ijk);
      
   if (xyz.xyz[0]<0.0) { 
      RETURN('r'); 
   } else { 
      RETURN('l'); 
   }
    
   
   /* should not get here */
   RETURN('u');
}
/*----------------------------------------------------------------------
   Return a multi-line string of TT atlas labels near the given point
   (xx,yy,zz are in Dicom order coordinates).
   If NULL is returned, an error happened.  If no labels are near the
   given point, then a single line saying "you're lost" is returned.
   The string returned is malloc()-ed and should be free()-ed later.
   The string will end with a newline '\n' character.
------------------------------------------------------------------------*/
char * Atlas_Query_to_String (ATLAS_QUERY *wami, ATLAS_COORD ac, WAMI_SORT_MODES mode)
{
   char *rbuf = NULL;
   char xlab[5][24], ylab[5][24] , zlab[5][24], clab[5][24], lbuf[500], tmps[128] ;
   THD_fvec3 t, m;
   THD_string_array *sar =NULL;
   ATLAS_COORD acv[NUMBER_OF_SPC];
   AFNI_ATLAS_CODES atcode = UNKNOWN_ATLAS;
   int i, ii, nfind=0, nfind_one = 0, iq=0, il=0, newzone = 0;
   byte LocalHead = 0;
   
   ENTRY("Atlas_Query_to_String") ;
   
   if (!wami) {
      ERROR_message("NULL wami");
      RETURN(rbuf);
   }
   
   /* get the coordinates into as many spaces as possible */
   /* first put ac in AFNI_TLRC */
   LOAD_FVEC3( m , ac.x, ac.y, ac.z ) ;
   switch (ac.space) {
      case AFNI_TLRC_SPC:
         acv[AFNI_TLRC_SPC].x = ac.x;
         acv[AFNI_TLRC_SPC].y = ac.y;
         acv[AFNI_TLRC_SPC].z = ac.z;
         acv[AFNI_TLRC_SPC].space = AFNI_TLRC_SPC;
         break;
      case MNI_ANAT_SPC:
         t = THD_mnia_to_tta_N27(m);
         if (t.xyz[0] < -500) { ERROR_message("Failed in xforming the data"); }
         acv[AFNI_TLRC_SPC].x = t.xyz[0];
         acv[AFNI_TLRC_SPC].y = t.xyz[1];
         acv[AFNI_TLRC_SPC].z = t.xyz[2];
         acv[AFNI_TLRC_SPC].space = AFNI_TLRC_SPC;
         break;
      case MNI_SPC:
         t = THD_mnia_to_tta_N27(m);
         if (t.xyz[0] < -500) { ERROR_message("Failed in xforming the data"); }
         acv[AFNI_TLRC_SPC].x = t.xyz[0];
         acv[AFNI_TLRC_SPC].y = t.xyz[1];
         acv[AFNI_TLRC_SPC].z = t.xyz[2];
         acv[AFNI_TLRC_SPC].space = AFNI_TLRC_SPC;
         RETURN(rbuf);
         break;
   }
   
   
   if (LocalHead) {/* Show me all the coordinates */
      INFO_message("Original Coordinates in %s: %f %f %f\n", Space_Code_to_Space_Name(ac.space), ac.x, ac.y, ac.z);
      for (i=UNKNOWN_SPC+1; i<NUMBER_OF_SPC; ++i) {
         INFO_message("Coordinate in %s: %f %f %f\n", Space_Code_to_Space_Name(acv[i].space), acv[i].x, acv[i].y, acv[i].z);
      }
   }
   ac = acv[AFNI_TLRC_SPC]; /* TLRC from now on */
      
   /* Prep the string toys */   
   INIT_SARR(sar) ; ADDTO_SARR(sar,WAMI_HEAD) ;
   
   /* form the coordinate labels, all results in LPI */
      /* good olde tlrc */
      sprintf(xlab[0],"%4.0f mm [%c]",-ac.x,(ac.x<0.0)?'R':'L') ;
      sprintf(ylab[0],"%4.0f mm [%c]",-ac.y,(ac.y<0.0)?'A':'P') ;
      sprintf(zlab[0],"%4.0f mm [%c]", ac.z,(ac.z<0.0)?'I':'S') ;
      sprintf(clab[0],"{T-T Atlas}");
      /* good olde MNI, via approximate equation */
      LOAD_FVEC3(m,ac.x,ac.y,ac.z);
      t = THD_tta_to_mni(m); 
      sprintf(xlab[1],"%4.0f mm [%c]",t.xyz[0],(t.xyz[0]>=0.0)?'R':'L') ;
      sprintf(ylab[1],"%4.0f mm [%c]",t.xyz[1],(t.xyz[1]>=0.0)?'A':'P') ;
      sprintf(zlab[1],"%4.0f mm [%c]",t.xyz[2],(t.xyz[2]< 0.0)?'I':'S') ;
      sprintf(clab[1],"{MNI Brain}");
      /* good olde MNI_Anatomical, a la Zilles */
      LOAD_FVEC3(m,ac.x,ac.y,ac.z);
      t = THD_tta_to_mnia_N27(m);
      /* find the LR, if possible from mask */
      sprintf(xlab[2],"%4.0f mm [%c]",-t.xyz[0], TO_UPPER(MNI_Anatomical_Side(ac))) ;
      sprintf(ylab[2],"%4.0f mm [%c]",-t.xyz[1], (ac.y<0.0)?'A':'P') ;
      sprintf(zlab[2],"%4.0f mm [%c]",t.xyz[2], (ac.z<0.0)?'I':'S') ;  
      sprintf(clab[2],"{MNI Anat.}");
   
   /* form the Focus point part */
   switch (mode) {
      case CLASSIC_WAMI_ATLAS_SORT:
      case CLASSIC_WAMI_ZONE_SORT:
            sprintf(lbuf,"Focus point (LPI)=%c"
                         "   %s,%s,%s %s%c"
                         "   %s,%s,%s %s%c"
                         "   %s,%s,%s %s%c",
                         lsep,
                         xlab[0], ylab[0], zlab[0], clab[0], lsep,
                         xlab[1], ylab[1], zlab[1], clab[1], lsep,
                         xlab[2], ylab[2], zlab[2], clab[2], lsep); 
            ADDTO_SARR(sar,lbuf);
         break;
      #if 0 /* ugly */
      case TAB1_WAMI_ATLAS_SORT:
      case TAB1_WAMI_ZONE_SORT:
            sprintf(lbuf,"Focus point (LPI)=%c"
                         "   %s,%s,%s %s%c"
                         "   %s,%s,%s %s%c"
                         "   %s,%s,%s %s%c",
                         lsep,
                         xlab[0], ylab[0], zlab[0], clab[0], lsep,
                         xlab[1], ylab[1], zlab[1], clab[1], lsep,
                         xlab[2], ylab[2], zlab[2], clab[2], lsep); 
            ADDTO_SARR(sar,lbuf);
         break;
      #endif
      case TAB1_WAMI_ATLAS_SORT:
      case TAB1_WAMI_ZONE_SORT:
      case TAB2_WAMI_ATLAS_SORT:
      case TAB2_WAMI_ZONE_SORT:         
            sprintf(lbuf,"%-36s\t%-12s", "Focus point (LPI)", "Coord.Space");
            ADDTO_SARR(sar,lbuf);
            for (ii=0; ii<3; ++ii) {
               sprintf(tmps,"%s,%s,%s", xlab[ii], ylab[ii], zlab[ii]);
               sprintf(lbuf,"%-36s\t%-12s", tmps, clab[ii]);
               ADDTO_SARR(sar,lbuf);
            }
         break;
      default:
         ERROR_message("Bad mode %d", mode);
         RETURN(rbuf);
   }

   switch (mode) {
      case CLASSIC_WAMI_ATLAS_SORT: /* the olde ways */
            /*-- assemble output string(s) for each atlas --*/
            nfind = 0;
            for (atcode=UNKNOWN_ATLAS+1; atcode < NUMBER_OF_ATLASES; ++atcode) { /* for each atlas atcode */
               nfind_one = 0;
               for (iq=0; iq<wami->N_zone; ++iq) { /* for each zone iq */
                  newzone = 1;
                  for (il=0; il<wami->zone[iq]->N_label; ++il) { /* for each label in a zone il */
                     if (wami->zone[iq]->atcode[il] == atcode) {
                        if (!nfind_one) {
                           sprintf(lbuf, "Atlas %s:", Atlas_Code_to_Atlas_Name(atcode));
                           ADDTO_SARR(sar,lbuf);
                        }
                        if (newzone) {
                           if (wami->zone[iq]->radius[il] == 0.0) {
                              sprintf(lbuf, "   Focus point: %s", 
                                             Clean_Atlas_Label(wami->zone[iq]->label[il]));
                           } else {
                              sprintf(lbuf, "   Within %1d mm: %s", 
                                             (int)wami->zone[iq]->radius[il], 
                                             Clean_Atlas_Label(wami->zone[iq]->label[il]));
                           }
                           newzone = 0;
                        } else {
                           sprintf(lbuf, "          -AND- %s", 
                                             Clean_Atlas_Label(wami->zone[iq]->label[il]));
                        }
                        if (wami->zone[iq]->prob[il] > 0.0) {
                           sprintf(lbuf, "%s   (p = %s)", lbuf, Atlas_Prob_String(wami->zone[iq]->prob[il]));
                        }
                        ADDTO_SARR(sar,lbuf);
                        ++nfind; ++nfind_one;
                     }
                  } /* il */
               } /* iq */
               if (nfind_one) {
                  ADDTO_SARR(sar,"");
               }
            } /* atcode */
            
         break;
      case CLASSIC_WAMI_ZONE_SORT: 
            /*-- assemble output string(s) for each atlas --*/
            nfind = 0;
            for (iq=0; iq<wami->N_zone; ++iq) { /* iq */
               if (wami->zone[iq]->level == 0) {
                  sprintf(lbuf, "Focus point:"); 
               } else {
                  sprintf(lbuf, "Within %1d mm:", 
                                 (int)wami->zone[iq]->level);
               }
               ADDTO_SARR(sar,lbuf);
               for (il=0; il<wami->zone[iq]->N_label; ++il) { /* il */
                  sprintf(lbuf, "   %-32s, Atlas %-15s", 
                     Clean_Atlas_Label(wami->zone[iq]->label[il]),
                     Atlas_Code_to_Atlas_Name(wami->zone[iq]->atcode[il]));

                  if (wami->zone[iq]->prob[il] > 0.0) {
                     sprintf(lbuf, "%s, prob. = %-3s", lbuf, Atlas_Prob_String(wami->zone[iq]->prob[il]));
                  }
                  ADDTO_SARR(sar,lbuf);
                  ++nfind;
               } /* il */
            } /* iq */
            
         break;
      #if 0 /* UGLY, no need for it */
      case TAB1_WAMI_ATLAS_SORT: /* like 1 but tab-separated for easy spreadsheet use UGLY!!! */
         /*-- assemble output string(s) for each atlas --*/
            nfind = 0;
            for (atcode=UNKNOWN_ATLAS+1; atcode < NUMBER_OF_ATLASES; ++atcode) { /* atcode */
               nfind_one = 0;
               for (iq=0; iq<wami->N_zone; ++iq) { /* iq */
                  for (il=0; il<wami->zone[iq]->N_label; ++il) { /* il */
                     if (wami->zone[iq]->atcode[il] == atcode) {
                        if (wami->zone[iq]->radius[il] == 0.0) {
                           sprintf(lbuf, "Atlas %s\tFocus point:\t%s",
                                          Atlas_Code_to_Atlas_Name(atcode), 
                                          Clean_Atlas_Label(wami->zone[iq]->label[il]));
                        } else {
                           sprintf(lbuf, "Atlas %s\tWithin %1d mm:\t%s", 
                                          Atlas_Code_to_Atlas_Name(atcode), 
                                          (int)wami->zone[iq]->radius[il], 
                                          Clean_Atlas_Label(wami->zone[iq]->label[il]));
                        }
                        sprintf(lbuf, "%s\tp = %s", lbuf, Atlas_Prob_String(wami->zone[iq]->prob[il]));
                        sprintf(lbuf, "%s\tcode = %-3d", lbuf, wami->zone[iq]->code[il]);
                        ADDTO_SARR(sar,lbuf);
                        ++nfind; ++nfind_one;
                     }
                  } /* il */
               } /* iq */
               
            } /* atcode */
            
         break;
      #endif
      case TAB1_WAMI_ZONE_SORT:
      case TAB2_WAMI_ZONE_SORT: 
         /*-- assemble output string(s) for each atlas --*/
            sprintf(lbuf, "%-3s\t%-15s\t%-32s\t%-3s\t%-3s", "Within", "Atlas", "Label", "Prob.", "Code"); 
            ADDTO_SARR(sar,lbuf);
            nfind = 0;
               for (iq=0; iq<wami->N_zone; ++iq) { /* iq */
                  for (il=0; il<wami->zone[iq]->N_label; ++il) { /* il */
                     if (1) {
                        sprintf(tmps, "%.1f", wami->zone[iq]->radius[il]);
                        sprintf(lbuf, "%-3s\t%-15s\t%-32s\t%-3s\t%-3d", 
                                    tmps, 
                                    Atlas_Code_to_Atlas_Name(wami->zone[iq]->atcode[il]), 
                                    Clean_Atlas_Label(wami->zone[iq]->label[il]), 
                                    Atlas_Prob_String(wami->zone[iq]->prob[il]), 
                                    wami->zone[iq]->code[il]); 
                        ADDTO_SARR(sar,lbuf);
                        ++nfind; ++nfind_one;
                     }
                  } /* il */
               } /* iq */
                           
         break;
      case TAB1_WAMI_ATLAS_SORT:
      case TAB2_WAMI_ATLAS_SORT: /* like TAB1_WAMI_ATLAS_SORT but more to my liking for easy spreadsheet use */
            /*-- assemble output string(s) for each atlas --*/
            sprintf(lbuf, "%-15s\t%-3s\t%-32s\t%-3s\t%-3s", "Atlas", "Within", "Label", "Prob.", "Code"); 
            ADDTO_SARR(sar,lbuf);
            nfind = 0;
            for (atcode=UNKNOWN_ATLAS+1; atcode < NUMBER_OF_ATLASES; ++atcode) { /* atcode */
               nfind_one = 0;
               for (iq=0; iq<wami->N_zone; ++iq) { /* iq */
                  for (il=0; il<wami->zone[iq]->N_label; ++il) { /* il */
                     if (wami->zone[iq]->atcode[il] == atcode) {
                        sprintf(tmps, "%.1f", wami->zone[iq]->radius[il]);
                        sprintf(lbuf, "%-15s\t%-3s\t%-32s\t%-3s\t%-3d", 
                                    Atlas_Code_to_Atlas_Name(atcode), 
                                    tmps, 
                                    Clean_Atlas_Label(wami->zone[iq]->label[il]), 
                                    Atlas_Prob_String(wami->zone[iq]->prob[il]), 
                                    wami->zone[iq]->code[il]); 
                        ADDTO_SARR(sar,lbuf);
                        ++nfind; ++nfind_one;
                     }
                  } /* il */
               } /* iq */
               
            } /* atcode */
            
         break;      
      default:
         ERROR_message("Bad mode (%d).", mode);
         RETURN(rbuf);
   }
   
   /* anything ? */
   if (!nfind) {
      ADDTO_SARR(sar,"***** Not near any region stored in databases *****\n");
   }
   /*- if didn't make any label, must produce something -*/

   if( sar->num == 1 ){    /* shouldn't ever happen */
      sprintf(lbuf,"Found %d marked but unlabeled regions???\n",nfind) ;
      ADDTO_SARR(sar,lbuf) ;
   } else if( !AFNI_noenv("AFNI_TTATLAS_CAUTION") ){
      ADDTO_SARR(sar,WAMI_TAIL) ;  /* cautionary tail */
   }

   /*- convert list of labels into one big multi-line string -*/

   for( nfind=ii=0 ; ii < sar->num ; ii++ ) nfind += strlen(sar->ar[ii]) ;
   rbuf = AFMALL(char, nfind + 2*sar->num + 32 ) ; rbuf[0] = '\0' ;
   for( ii=0 ; ii < sar->num ; ii++ ){
      strcat(rbuf,sar->ar[ii]) ; strcat(rbuf,"\n") ;
   }

   DESTROY_SARR(sar) ; 
   
   if (LocalHead) {
      INFO_message("Have:\n%s\n", rbuf);
   }
   
   RETURN(rbuf);
}

void TT_whereami_set_outmode(WAMI_SORT_MODES md)
{
   
   TT_whereami_mode = md;
   switch (md) {
      case TAB2_WAMI_ATLAS_SORT:
      case TAB2_WAMI_ZONE_SORT:
         lsep =  '\t';
         break;
      case TAB1_WAMI_ATLAS_SORT:
      case TAB1_WAMI_ZONE_SORT:
         lsep =  '\t';
         break;
      case CLASSIC_WAMI_ATLAS_SORT:
      case CLASSIC_WAMI_ZONE_SORT:
         lsep =  '\n';
         break;
      default:
         WARNING_message("Mode not supported.Using Default.");
         TT_whereami_mode = CLASSIC_WAMI_ATLAS_SORT;
         lsep =  '\n';
         break;
   }
   
   return;
}

static int TT_whereami_n_atlas_list = 0;
static AFNI_ATLAS_CODES TT_whereami_atlas_list[NUMBER_OF_ATLASES] = { UNKNOWN_ATLAS, UNKNOWN_ATLAS, UNKNOWN_ATLAS, UNKNOWN_ATLAS };

void TT_whereami_add_atlas(AFNI_ATLAS_CODES ac)
{
   int i, fnd=0;
   
   if (ac <=  UNKNOWN_ATLAS || ac >= NUMBER_OF_ATLASES) {
      ERROR_message("What are you doing?");
      return;
   }
   
   i = 0;
   fnd = 0;
   while (i<TT_whereami_n_atlas_list && i<NUMBER_OF_ATLASES && !fnd) {
      if (TT_whereami_atlas_list[i] == ac) fnd = 1;  
      ++i; 
   }
   
   if (!fnd) {
      if (TT_whereami_n_atlas_list < NUMBER_OF_ATLASES) {
         TT_whereami_atlas_list[TT_whereami_n_atlas_list] = ac;
         ++TT_whereami_n_atlas_list;
      } else {
         ERROR_message("Why O Lord, Why?");
         return;
      }
   }
   /* fprintf(stderr,"Up to %d atlases\n", TT_whereami_n_atlas_list); */
   return;
}

void TT_whereami_remove_atlas(AFNI_ATLAS_CODES ac)
{
   int i, fnd=0;
   
   if (ac <=  UNKNOWN_ATLAS || ac >= NUMBER_OF_ATLASES) {
      ERROR_message("What are you doing?");
      return;
   }
   
   i = 0;
   fnd = -1;
   while (i<TT_whereami_n_atlas_list && i<NUMBER_OF_ATLASES && fnd < 0) {
      if (TT_whereami_atlas_list[i] == ac) fnd = i;  
      ++i; 
   }
   
   if (fnd >=0 && TT_whereami_n_atlas_list) { /* uber careful */
      TT_whereami_atlas_list[fnd] = TT_whereami_atlas_list[TT_whereami_n_atlas_list-1]; /* replace with one on end */
      --TT_whereami_n_atlas_list;
   }
   
   /* fprintf(stderr,"Down to %d atlases\n", TT_whereami_n_atlas_list); */
   return;
}

/* a new version of TT_whereami that can use the variety
   of atlases */
char * TT_whereami( float xx , float yy , float zz ) 
{
   ATLAS_COORD ac;
   ATLAS_QUERY *wami = NULL;
   char *rbuf = NULL, *strg = NULL ;
   int k;
   
   ENTRY("TT_whereami") ;
   
   /* build atlas list */
   if (TT_whereami_n_atlas_list == 0) {
      /* Uninitialized, get them all */
      for (k=UNKNOWN_ATLAS+1; k<NUMBER_OF_ATLASES; ++k) {
         TT_whereami_add_atlas(k);
      }
   }

   /* build coord structure */
   ac.x = xx; ac.y = yy; ac.z = zz; ac.space = AFNI_TLRC_SPC;
   
   strg = whereami_9yards(ac, &wami, TT_whereami_atlas_list, TT_whereami_n_atlas_list);
   if (strg) {
      WARNING_message("Unexpected string (%s) from whereami_9yards\n", strg);
      free(strg); strg = NULL; /* nothing useful here */
   }
   if (!wami) {
      ERROR_message("No atlas regions found.");
      RETURN(rbuf) ;
   }
   
   /* Now form the string */
   rbuf =  Atlas_Query_to_String (wami, ac, TT_whereami_mode);  
   
   /*cleanup*/
   if (wami)  wami = Free_Atlas_Query(wami);
   
   RETURN(rbuf) ;
}
char * TT_whereami_old( float xx , float yy , float zz ) /* ZSS */
{
   int ii,kk , ix,jy,kz , nx,ny,nz,nxy , aa,bb,cc , ff,b2f,b4f,rff ;
   THD_ivec3 ijk ;
   byte *b2 , *b4 ;
   THD_string_array *sar ;
   char *b2lab , *b4lab ;
   char lbuf[256] , *rbuf ;
   int nfind, b2_find[MAX_FIND], b4_find[MAX_FIND], rr_find[MAX_FIND] ;

   THD_3dim_dataset * dset ; /* 01 Aug 2001 */

ENTRY("TT_whereami_old") ;

   /*-- setup stuff: load atlas dataset, prepare search mask --*/

   if( dseTT == NULL ){
      ii = TT_load_atlas() ; if( ii == 0 ) RETURN(NULL) ;
   }

   /* 01 Aug 2001: maybe use big dataset (so don't need both in memory) */

   dset = (dseTT_big != NULL) ? dseTT_big : dseTT ;

#if 0
if( dset == dseTT_big ) fprintf(stderr,"TT_whereami using dseTT_big\n") ;
else                    fprintf(stderr,"TT_whereami using dseTT\n") ;
#endif

   DSET_load(dset) ;
   b2 = DSET_BRICK_ARRAY(dset,0) ; if( b2 == NULL ) RETURN(NULL) ;
   b4 = DSET_BRICK_ARRAY(dset,1) ; if( b4 == NULL ) RETURN(NULL) ;

   if( wamiclust == NULL ){
      wamiclust = MCW_build_mask( 1.0,1.0,1.0 , WAMIRAD ) ;
      if( wamiclust == NULL ) RETURN(NULL) ;  /* should not happen! */

      for( ii=0 ; ii < wamiclust->num_pt ; ii++ )       /* load radius */
         wamiclust->mag[ii] = (int)rint(sqrt((double)(
                                         wamiclust->i[ii]*wamiclust->i[ii]
                                        +wamiclust->j[ii]*wamiclust->j[ii]
                                        +wamiclust->k[ii]*wamiclust->k[ii]))) ;

      MCW_sort_cluster( wamiclust ) ;  /* sort by radius */
   }

   /*-- find locations near the given one that are in the Atlas --*/

   ijk = THD_3dmm_to_3dind( dset , TEMP_FVEC3(xx,yy,zz) ) ;  /* get indexes */
   UNLOAD_IVEC3(ijk,ix,jy,kz) ;                               /* from coords */

   nx = DSET_NX(dset) ;               /* size of TT atlas dataset axes */
   ny = DSET_NY(dset) ;
   nz = DSET_NZ(dset) ; nxy = nx*ny ;

   nfind = 0 ;

   /*-- check the exact input location --*/

   kk = ix + jy*nx + kz*nxy ;        /* index into brick arrays */
   if( b2[kk] != 0 || b4[kk] != 0 ){
      b2_find[0] = b2[kk] ;
      b4_find[0] = b4[kk] ;
      rr_find[0] = 0      ; nfind++ ;
   }

   /*-- check locations near it --*/

   for( ii=0 ; ii < wamiclust->num_pt ; ii++ ){

      /* compute index of nearby location, skipping if outside atlas */

      aa = ix + wamiclust->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
      bb = jy + wamiclust->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
      cc = kz + wamiclust->k[ii] ; if( cc < 0 || cc >= nz ) continue ;

      kk  = aa + bb*nx + cc*nxy ;   /* index into bricks */
      b2f = b2[kk] ; b4f = b4[kk] ; /* TT structures markers there */

      if( b2f == 0 && b4f == 0 )                            continue ;

      for( ff=0 ; ff < nfind ; ff++ ){       /* cast out         */
         if( b2f == b2_find[ff] ) b2f = 0 ;  /* duplicate labels */
         if( b4f == b4_find[ff] ) b4f = 0 ;  /* we already found */
      }
      if( b2f == 0 && b4f == 0 )                            continue ;

      b2_find[nfind] = b2f ;  /* save what we found */
      b4_find[nfind] = b4f ;
      rr_find[nfind] = (int) wamiclust->mag[ii] ;
      nfind++ ;

      if( nfind == MAX_FIND ) break ;  /* don't find TOO much */
   }

   /*-- assemble output string(s) --*/

   if( nfind == 0 ){
      char xlab[24], ylab[24] , zlab[24] ;
      THD_fvec3 tv , mv ;
      float mx,my,mz ;
      char mxlab[24], mylab[24] , mzlab[24] ;

      sprintf(xlab,"%4.0f mm [%c]",-xx,(xx<0.0)?'R':'L') ;
      sprintf(ylab,"%4.0f mm [%c]",-yy,(yy<0.0)?'A':'P') ;
      sprintf(zlab,"%4.0f mm [%c]", zz,(zz<0.0)?'I':'S') ;

      LOAD_FVEC3(tv,xx,yy,zz);
      mv = THD_tta_to_mni(tv); UNLOAD_FVEC3(mv,mx,my,mz);
      sprintf(mxlab,"%4.0f mm [%c]",mx,(mx>=0.0)?'R':'L') ;
      sprintf(mylab,"%4.0f mm [%c]",my,(my>=0.0)?'A':'P') ;
      sprintf(mzlab,"%4.0f mm [%c]",mz,(mz< 0.0)?'I':'S') ;

      rbuf = AFMALL(char, 500) ;
      sprintf(rbuf,"%s\n"
                   "Focus point=%s,%s,%s {T-T Atlas}\n"
                   "           =%s,%s,%s {MNI Brain}\n"
                   "\n"
                   "***** Not near any region stored in database *****\n" ,
              WAMI_HEAD , xlab,ylab,zlab , mxlab,mylab,mzlab ) ;
      RETURN(rbuf) ;
   }

   /*-- bubble-sort what we found, by radius --*/

   if( nfind > 1 ){  /* don't have to sort only 1 result */
     int swap, tmp ;
     do{
        swap=0 ;
        for( ii=1 ; ii < nfind ; ii++ ){
           if( rr_find[ii-1] > rr_find[ii] ){
             tmp = rr_find[ii-1]; rr_find[ii-1] = rr_find[ii]; rr_find[ii] = tmp;
             tmp = b2_find[ii-1]; b2_find[ii-1] = b2_find[ii]; b2_find[ii] = tmp;
             tmp = b4_find[ii-1]; b4_find[ii-1] = b4_find[ii]; b4_find[ii] = tmp;
             swap++ ;
           }
        }
     } while(swap) ;
   }

   /*-- find anatomical label for each found marker, make result string --*/

   INIT_SARR(sar) ; ADDTO_SARR(sar,WAMI_HEAD) ;

   /* 04 Apr 2002: print coordinates (LPI) as well (the HH-PB addition) */

   { char lbuf[128], xlab[24], ylab[24] , zlab[24] ;
     sprintf(xlab,"%4.0f mm [%c]",-xx,(xx<0.0)?'R':'L') ;
     sprintf(ylab,"%4.0f mm [%c]",-yy,(yy<0.0)?'A':'P') ;
     sprintf(zlab,"%4.0f mm [%c]", zz,(zz<0.0)?'I':'S') ;
     sprintf(lbuf,"Focus point=%s,%s,%s {T-T Atlas}",xlab,ylab,zlab) ;
     ADDTO_SARR(sar,lbuf) ;
   }

   /* 29 Apr 2002: print MNI coords as well */

   { THD_fvec3 tv , mv ;
     float mx,my,mz ;
     char mxlab[24], mylab[24] , mzlab[24] , lbuf[128] ;
     LOAD_FVEC3(tv,xx,yy,zz);
     mv = THD_tta_to_mni(tv); UNLOAD_FVEC3(mv,mx,my,mz);
     sprintf(mxlab,"%4.0f mm [%c]",mx,(mx>=0.0)?'R':'L') ;
     sprintf(mylab,"%4.0f mm [%c]",my,(my>=0.0)?'A':'P') ;
     sprintf(mzlab,"%4.0f mm [%c]",mz,(mz< 0.0)?'I':'S') ;
     sprintf(lbuf,"Focus point=%s,%s,%s {MNI Brain}\n",mxlab,mylab,mzlab) ;
     ADDTO_SARR(sar,lbuf) ;
   }

   rff = -1 ;  /* rff = radius of last found label */

   for( ff=0 ; ff < nfind ; ff++ ){
      b2f = b2_find[ff] ; b4f = b4_find[ff] ; b2lab = NULL ; b4lab = NULL ;

      if( b2f != 0 ){                               /* find label     */
         for( ii=0 ; ii < TTO_COUNT ; ii++ )        /* in AFNI's list */
            if( b2f == TTO_list[ii].tdval ) break ;
         if( ii < TTO_COUNT )                       /* always true? */
            b2lab = TTO_list[ii].name ;

         if( b2lab != NULL && xx < 0 && strstr(b2lab,"Left") != NULL ) /* maybe is Right */
            b2lab = TTO_list[ii+1].name ;
      }

      if( b4f != 0 ){
         for( ii=0 ; ii < TTO_COUNT ; ii++ )
            if( b4f == TTO_list[ii].tdval ) break ;
         if( ii < TTO_COUNT )
            b4lab = TTO_list[ii].name ;
         if( b4lab != NULL && xx < 0 && strstr(b4lab,"Left") != NULL )
            b4lab = TTO_list[ii+1].name ;
      }

      if( b2lab == NULL && b4lab == NULL ) continue ;  /* no labels? */

      /* make output label into lbuf */

      lbuf[0] = '\0' ;
      if( b2lab != NULL ){
         if( rr_find[ff] != rff ){
            if( rr_find[ff] > 0 )
              sprintf( lbuf , "Within %d mm: %s" , rr_find[ff] , b2lab ) ;
            else
              sprintf( lbuf , "Focus point: %s" , b2lab ) ;
         } else {
            sprintf( lbuf , "             %s" , b2lab ) ;
         }

         for( kk=strlen(lbuf)-1 ; kk > 0 && lbuf[kk] == '.' ; kk-- )
            lbuf[kk] = '\0' ;                  /* trim trailing .'s */
      }

      if( b4lab != NULL ){
         kk = strlen(lbuf) ;
         if( kk > 0 ){
            sprintf( lbuf+kk , " -AND- %s" , b4lab ) ;
         } else if( rr_find[ff] != rff ){
            if( rr_find[ff] > 0 )
              sprintf( lbuf , "Within %d mm: %s" , rr_find[ff] , b4lab ) ;
            else
              sprintf( lbuf , "Focus point: %s" , b4lab ) ;
         } else {
            sprintf( lbuf , "             %s" , b4lab ) ;
         }

         for( kk=strlen(lbuf)-1 ; kk > 0 && lbuf[kk] == '.' ; kk-- )
            lbuf[kk] = '\0' ;
      }

      ADDTO_SARR(sar,lbuf) ;  /* make a list of labels */

      rff = rr_find[ff] ;  /* save for next time around */
   }

   /*- if didn't make any label, must produce something -*/

   if( sar->num == 1 ){    /* shouldn't ever happen */
      sprintf(lbuf,"Found %d marked but unlabeled regions???\n",nfind) ;
      ADDTO_SARR(sar,lbuf) ;
   } else if( !AFNI_noenv("AFNI_TTATLAS_CAUTION") ){
      ADDTO_SARR(sar,WAMI_TAIL) ;  /* cautionary tail */
   }

   /*- convert list of labels into one big multi-line string -*/

   for( nfind=ii=0 ; ii < sar->num ; ii++ ) nfind += strlen(sar->ar[ii]) ;
   rbuf = AFMALL(char, nfind + 2*sar->num + 32 ) ; rbuf[0] = '\0' ;
   for( ii=0 ; ii < sar->num ; ii++ ){
      strcat(rbuf,sar->ar[ii]) ; strcat(rbuf,"\n") ;
   }

   DESTROY_SARR(sar) ; RETURN(rbuf) ;
}

/* Begin ZSS: Additions for Eickhoff and Zilles Cytoarchitectonic maps */


int compare_Z_IQSORT_FLOAT (Z_QSORT_FLOAT *a, Z_QSORT_FLOAT *b )
{
   if (a->x < b->x)
      return (1);
   else if (a->x == b->x)
      return (0);
   else if (a->x > b->x)
      return (-1);
   /* this will never be reached but it will shut the compiler up */
   return (0);
}

/*!
    l left
    u unknown
    r right
*/
char Is_Side_Label(char *str, char *opt)
{
   int k, nc;
   char *strd=NULL;
   ENTRY("atlas_label_side");
   
   if (!str) RETURN('u');
   
   strd = strdup(str);
   nc = strlen(strd);
   for (k=0; k<nc; ++k) strd[k] = TO_LOWER(strd[k]);
   
   if (strncmp(strd,"left", 4) == 0) RETURN('l');
   else if (strncmp(strd,"right", 5) == 0) RETURN('r');
   
   free(strd); strd = NULL;
   RETURN('u');
}

/* inverse sort */
int *z_iqsort (float *x , int nx )
{/*z_iqsort*/
   static char FuncName[]={"z_iqsort"}; 
   int *I, k;
   Z_QSORT_FLOAT *Z_Q_fStrct;
   
   ENTRY("z_iqsort");

   /* allocate for the structure */
   Z_Q_fStrct = (Z_QSORT_FLOAT *) calloc(nx, sizeof (Z_QSORT_FLOAT));
   I = (int *) calloc (nx, sizeof(int));

   if (!Z_Q_fStrct || !I)
      {
         ERROR_message("Allocation problem");
         RETURN (NULL);
      }

   for (k=0; k < nx; ++k) /* copy the data into a structure */
      {
         Z_Q_fStrct[k].x = x[k];
         Z_Q_fStrct[k].Index = k;
      }

   /* sort the structure by it's field value */
   qsort(Z_Q_fStrct, nx, sizeof(Z_QSORT_FLOAT), (int(*) (const void *, const void *)) compare_Z_IQSORT_FLOAT);

   /* recover the index table */
   for (k=0; k < nx; ++k) /* copy the data into a structure */
      {
         x[k] = Z_Q_fStrct[k].x;
         I[k] = Z_Q_fStrct[k].Index;
      }

   /* free the structure */
   free(Z_Q_fStrct);

   /* return */
   RETURN (I);


}/*z_iqsort*/

void Show_Atlas_Region (AFNI_ATLAS_REGION *aar)
{
   int k = 0;
   
   ENTRY("Show_Atlas_Region") ;
   
   if (!aar) {
      WARNING_message("NULL atlas region structure");
      EXRETURN;
   }
   
   fprintf(stdout,""
                  "Side      : %c\n"
                  "orig_label: %s\n"
                  "id        : %d\n"
                  "N_chnks     : %d\n",
                  aar->side, STR_PRINT(aar->orig_label), aar->id, aar->N_chnks);
   for (k=0; k<aar->N_chnks; ++k) {
      fprintf(stdout,"aar->chnks[%d] = %s\n", k, STR_PRINT(aar->chnks[k]));
   }
   fprintf(stdout,"\n");
   
   EXRETURN;
}


AFNI_ATLAS_REGION * Free_Atlas_Region (AFNI_ATLAS_REGION *aar)
{
   int k = 0;
   
   ENTRY("Free_Atlas_Region");
   
   if (!aar) {
      WARNING_message("NULL aar");
      RETURN(NULL);
   }
   
   if (aar->chnks) {
      for (k=0; k<aar->N_chnks; ++k) {
         if (aar->chnks[k]) free(aar->chnks[k]); 
      }
      free(aar->chnks);
   }
   
   if (aar->orig_label) free(aar->orig_label);
   
   free(aar);
   
   RETURN(NULL);
}

byte Same_Chunks(AFNI_ATLAS_REGION *aar1, AFNI_ATLAS_REGION *aar2)
{
   int i;
   
   ENTRY("Same_Chunks");
   if (!aar1 || !aar2) RETURN(0);
   if (aar1->N_chnks != aar2->N_chnks) RETURN(0);
   for (i=0; i<aar1->N_chnks; ++i) {
      if (strcmp(aar1->chnks[i], aar2->chnks[i])) RETURN(0);
   }
   RETURN(1);
}

/*!
   given a string, chunk it 
*/
AFNI_ATLAS_REGION * Atlas_Chunk_Label(char *lbli, int id)
{
   AFNI_ATLAS_REGION *aar = NULL;
   char lachunk[500], sd, *lbl = NULL;
   int ic = 0, nc = 0, k = 0, block = 0, isnum=0;
   byte LocalHead = 0;
   
   ENTRY("Atlas_Chunk_Label") ;
   if (!lbli) {
      ERROR_message("NULL label");
      RETURN(aar) ;
   }
   
   nc = strlen(lbli);
   
   if (lbli[0] == '\0') {
      ERROR_message("Empty label");
      RETURN(aar) ;   
   }
   
   lbl = strdup(lbli);
   
   aar = (AFNI_ATLAS_REGION*) malloc(sizeof(AFNI_ATLAS_REGION));
   aar->side = 'u';
   aar->orig_label = strdup(lbl);
   aar->id = id;
   aar->N_chnks = 0;
   aar->chnks = NULL;
   
   /* is this all numbers ? */
   isnum = 1;
   for (k=0; k<nc; ++k) {
      if (!IS_NUMBER(lbl[k])) isnum = 0;
   }
   /* it is an integer, stop the machines */
   if (isnum) {
      isnum = atoi(lbl);
      free(lbl); lbl = NULL;  /* no need no more */
      if (aar->id && isnum != aar->id) {
         ERROR_message("Information conflict!");
         RETURN(Free_Atlas_Region(aar)) ; 
      }
      aar->id = isnum;
      if (LocalHead) fprintf(stderr,"Have number (%d), will travel\n", aar->id);
      RETURN(aar);
   }
   
   /* change any '.' surrounded by digits to a 0 */
   for (k=1; k<nc-1; ++k) {
      if (lbl[k] == '.' && IS_NUMBER(lbl[k+1]) && IS_NUMBER(lbl[k-1])) lbl[k] = '0';
   }
   
   ic = 0;
   k = 0;
   block = 0;
   while (!IS_LETTER(lbl[k]) && !IS_NUMBER(lbl[k]) && k < nc) ++k;
   if (IS_LETTER(lbl[k])) block = 1;
   else if (IS_NUMBER(lbl[k])) block = 2;
   else block = 0;
         
   while (k < nc) {
      if (IS_LETTER(lbl[k]) && block == 1) {
         lachunk[ic] = TO_LOWER(lbl[k]); ++ic;
         ++k;
      } else if (IS_NUMBER(lbl[k]) && block == 2) {
         lachunk[ic] = TO_LOWER(lbl[k]); ++ic;
         ++k;
      } else {
         lachunk[ic] = '\0'; /* seal */
         if (LocalHead) fprintf(stderr,"Have chunk %s, will eat...\n", lachunk);
         sd = '\0';
         if (aar->N_chnks == 0) { /* check on side */
            sd = Is_Side_Label(lachunk, NULL);
            if (LocalHead) fprintf(stderr,"Side check on %s returned %c\n", lachunk, sd);
            if (sd == 'l' || sd == 'r' || sd == 'b') {
               aar->side = sd;
            } else {
               /* unknown */
               sd = '\0';
            }   
         }
         if (sd == '\0') { /* new, non left/right chunk */
            /* store lachunk, skip to next char or number */
            aar->chnks = (char **)realloc(aar->chnks, sizeof(char*)*(aar->N_chnks+1));
            aar->chnks[aar->N_chnks] = strdup(lachunk);
            ++ aar->N_chnks; 
         }
         ic = 0; lachunk[ic] = '\0'; /* seal */
         while (!IS_LETTER(lbl[k]) && !IS_NUMBER(lbl[k]) && k < nc) ++k;
         if (IS_LETTER(lbl[k])) block = 1;
         else if (IS_NUMBER(lbl[k])) block = 2;
         else block = 0;
      }   
   }
   
   /* add last chunk */
   if (lachunk[0] != '\0') {
      lachunk[ic] = '\0';
      /* first check on side */
      sd = '\0';
      if (aar->N_chnks == 0) { /* check on side */
         sd = Is_Side_Label(lachunk, NULL);
         if (LocalHead) fprintf(stderr,"Side check on %s returned %c\n", lachunk, sd);
         if (sd == 'l' || sd == 'r' || sd == 'b') {
            aar->side = sd;
         } else {
            /* unknown */
            sd = '\0';
         }   
      }
      if (sd == '\0') { /* new, non left/right chunk */
         aar->chnks = (char **)realloc(aar->chnks, sizeof(char*)*(aar->N_chnks+1));
         aar->chnks[aar->N_chnks] = strdup(lachunk);
         ++ aar->N_chnks; 
      }
      ic = 0; lachunk[ic] = '\0'; /* seal */
   }
         
   if (LocalHead) {
      fprintf(stderr,"Atlas_Chunk_Label:\n" );
      Show_Atlas_Region (aar); 
   }
   free(lbl); lbl = NULL;
   RETURN(aar) ;
}

AFNI_ATLAS *Build_Atlas (AFNI_ATLAS_CODES ac) 
{
   AFNI_ATLAS *aa=NULL;
   int k = 0;
   
   ENTRY("Build_Atlas") ;
   
   switch (ac) {
      case AFNI_TLRC_ATLAS:
         aa = (AFNI_ATLAS *)malloc(sizeof(AFNI_ATLAS));
         aa->AtlasLabel = strdup(Atlas_Code_to_Atlas_Name(AFNI_TLRC_ATLAS));
         aa->N_regions = TTO_COUNT;
         aa->reg = (AFNI_ATLAS_REGION **)calloc(aa->N_regions, sizeof(AFNI_ATLAS_REGION *));
         for (k=0; k<aa->N_regions; ++k) {
            aa->reg[k] = Atlas_Chunk_Label(TTO_list[k].name, TTO_list[k].tdval);
            /* Show_Atlas_Region (aa->reg[k]); */
         }
         break;
      case CA_EZ_MPM_ATLAS:
         aa = (AFNI_ATLAS *)malloc(sizeof(AFNI_ATLAS));
         aa->AtlasLabel = strdup(Atlas_Code_to_Atlas_Name(CA_EZ_MPM_ATLAS));
         aa->N_regions = CA_EZ_COUNT;
         aa->reg = (AFNI_ATLAS_REGION **)calloc(aa->N_regions, sizeof(AFNI_ATLAS_REGION *));
         for (k=0; k<aa->N_regions; ++k) {
            aa->reg[k] = Atlas_Chunk_Label(CA_EZ_list[k].name, CA_EZ_list[k].tdval);
            /* Show_Atlas_Region (aa->reg[k]); */
         }
         break;
      case CA_EZ_PMAPS_ATLAS:
         aa = (AFNI_ATLAS *)malloc(sizeof(AFNI_ATLAS));
         aa->AtlasLabel = strdup(Atlas_Code_to_Atlas_Name(CA_EZ_PMAPS_ATLAS));
         aa->N_regions = CA_EZ_COUNT;
         aa->reg = (AFNI_ATLAS_REGION **)calloc(aa->N_regions, sizeof(AFNI_ATLAS_REGION *));
         for (k=0; k<aa->N_regions; ++k) {
            aa->reg[k] = Atlas_Chunk_Label(CA_EZ_list[k].name, CA_EZ_list[k].tdval);
            /* Show_Atlas_Region (aa->reg[k]); */
         }
         break;
      case CA_EZ_ML_ATLAS:
         aa = (AFNI_ATLAS *)malloc(sizeof(AFNI_ATLAS));
         aa->AtlasLabel = strdup(Atlas_Code_to_Atlas_Name(CA_EZ_ML_ATLAS));
         aa->N_regions = ML_EZ_COUNT;
         aa->reg = (AFNI_ATLAS_REGION **)calloc(aa->N_regions, sizeof(AFNI_ATLAS_REGION *));
         for (k=0; k<aa->N_regions; ++k) {
            aa->reg[k] = Atlas_Chunk_Label(ML_EZ_list[k].name, ML_EZ_list[k].tdval);
            /* Show_Atlas_Region (aa->reg[k]); */
         }
         break;
      case CA_EZ_LR_ATLAS:
         aa = (AFNI_ATLAS *)malloc(sizeof(AFNI_ATLAS));
         aa->AtlasLabel = strdup(Atlas_Code_to_Atlas_Name(CA_EZ_LR_ATLAS));
         aa->N_regions = LR_EZ_COUNT;
         aa->reg = (AFNI_ATLAS_REGION **)calloc(aa->N_regions, sizeof(AFNI_ATLAS_REGION *));
         for (k=0; k<aa->N_regions; ++k) {
            aa->reg[k] = Atlas_Chunk_Label(LR_EZ_list[k].name, LR_EZ_list[k].tdval);
            /* Show_Atlas_Region (aa->reg[k]); */
         }
         break;
      default:
         ERROR_message( "No such atlas code %d \n"
                        "Available names (codes) are:\n"
                        "%s (%d), %s (%d), %s (%d), %s (%d), %s (%d)\n", ac, 
                        Atlas_Code_to_Atlas_Name(AFNI_TLRC_ATLAS), AFNI_TLRC_ATLAS,
                        Atlas_Code_to_Atlas_Name(CA_EZ_MPM_ATLAS), CA_EZ_MPM_ATLAS, 
                        Atlas_Code_to_Atlas_Name(CA_EZ_PMAPS_ATLAS), CA_EZ_PMAPS_ATLAS,
                        Atlas_Code_to_Atlas_Name(CA_EZ_ML_ATLAS), CA_EZ_ML_ATLAS,
                        Atlas_Code_to_Atlas_Name(CA_EZ_LR_ATLAS), CA_EZ_LR_ATLAS);
         RETURN(aa);
   }
   
   RETURN(aa);
}

void Show_Atlas (AFNI_ATLAS *aa)
{
   int k = 0;
   
   ENTRY("Show_Atlas");
   
   if (!aa) {
      WARNING_message("NULL atlas");
      EXRETURN;
   }
   
   fprintf(stdout,"\n"
                  "Atlas     :%s\n"
                  "N_regions :%d\n"
                  "----------- Begin regions for %s atlas-----------\n"
                  , STR_PRINT(aa->AtlasLabel), aa->N_regions, STR_PRINT(aa->AtlasLabel));
   for (k=0; k<aa->N_regions; ++k) {
      fprintf(stdout,"%d%s region:\n", k, COUNTER_SUFFIX(k));
      Show_Atlas_Region(aa->reg[k]);
   }
   fprintf(stdout,"----------- End regions for %s atlas --------------\n\n", STR_PRINT(aa->AtlasLabel));
   EXRETURN;
}

AFNI_ATLAS *Free_Atlas(AFNI_ATLAS *aa) 
{
   int k = 0;
   
   ENTRY("Free_Atlas");
   
   if (!aa) {
      ERROR_message("NULL atlas");
      RETURN(aa);
   }
   
   if (aa->AtlasLabel) free(aa->AtlasLabel);
   for (k=0; k<aa->N_regions; ++k) {
      if (aa->reg[k]) Free_Atlas_Region(aa->reg[k]);
   }
   free(aa->reg); 
   free(aa);
   
   RETURN(NULL);
}
static int SpeakEasy = 1;
void Set_ROI_String_Decode_Verbosity(byte lvl)
{
   SpeakEasy = lvl;
   return;
}  
/*!
   Decode a given ROI specifying string
   
*/
AFNI_ATLAS_REGION *ROI_String_Decode(char *str, AFNI_ATLAS_CODES *ac)
{
   AFNI_ATLAS_REGION *aar = NULL;
   int nc=0, k, icol[10], shft=0, ncol = 0;
   char *lbl = NULL, atlas_name[256];
   byte LocalHead = SpeakEasy;
   ENTRY("ROI_String_Decode");
   
   if (!str ) {
      if (LocalHead) ERROR_message("NULL input");
      RETURN(aar) ;
   }
   atlas_name[0] = '\0';
   
   nc = strlen(str);
   if (nc < 3) {
      if (LocalHead) ERROR_message("Get Shorty");
      RETURN(aar) ;
   }
   
   /* find my ':' */
   icol[0] = 0;
   ncol = 0;
   k = 0;
   while (k<nc) {
      if (str[k] == ':') {
         if (ncol < 2) {
            ++ncol;
            icol[ncol] = k; 
         } else {
            if (LocalHead) ERROR_message("Too many ':' in ROI string");
            RETURN(aar) ;
         }
      }
      ++k;
   }
   
   
   if (!ncol){
      if (LocalHead) ERROR_message("Failed to find ':'\nin '%s'\n", str);
      RETURN(aar) ;
   }
   
   
   /* by now, we have at least one column */
   /* get the atlas name, 1st item*/
   for (k=icol[0]; k<icol[1]; ++k) atlas_name[k] = str[k];
   atlas_name[icol[1]] = '\0';
   /* fprintf(stderr,"atlas_name from %s is: %s\n", str, atlas_name); */
   *ac = Atlas_Name_to_Atlas_Code(atlas_name);
   /* is this an OK atlas */
   if (*ac <= UNKNOWN_ATLAS || *ac >= NUMBER_OF_ATLASES) {
      if (LocalHead) {
         ERROR_message("Atlas %s not recognized\nAvailable atlas names are:\n", atlas_name);
         for (k=UNKNOWN_ATLAS+1; k<NUMBER_OF_ATLASES; ++k) {
            fprintf(stderr,"   %s (code %d)\n", Atlas_Code_to_Atlas_Name(k), k);
         }
      }
      if (LocalHead) WARNING_message("Proceeding with hope for an impossible miracle...\n");
   }   
   
   /* get the label, last item */
   lbl = (char*)malloc(sizeof(char)*(nc+1));
   shft = icol[ncol]+1;
   for (k=shft; k<nc; ++k) lbl[k-shft] = str[k];
   lbl[nc-shft] = '\0';
   /*fprintf(stderr,"lbl from %s(%d to %d) is : '%s'\n", str, shft, nc, lbl); */
   
   /* Now get aar */
   if (!(aar = Atlas_Chunk_Label(lbl, 0))) {
      if (LocalHead) ERROR_message("Failed in processing label");
      RETURN(aar) ;   
   }
   
   free(lbl); lbl = NULL;
   
   /* set the side if possible */
   if (ncol == 2 && (icol[2] - icol[1] > 1)) {
      aar->side = TO_LOWER(str[icol[1]+1]);
      if (aar->side != 'l' && aar->side != 'r' &&  aar->side != 'u'  &&  aar->side != 'b') {
         if (LocalHead) ERROR_message("Bad side specifier");
         aar = Free_Atlas_Region(aar);
         RETURN(aar) ;
      }
   }
   
   RETURN(aar) ;   
}

char *Report_Found_Regions(AFNI_ATLAS *aa, AFNI_ATLAS_REGION *ur , ATLAS_SEARCH *as, int *nexact)
{
   char *rbuf = NULL, lbuf[500];
   THD_string_array *sar = NULL;
   int nfind = 0, ii = 0, k= 0;
   
   ENTRY("Find_Atlas_Regions");
   
   if (!as || !ur || !aa) {
      ERROR_message("NULL input");
      RETURN(rbuf);
   }
   /* Prep the string toys */   
   INIT_SARR(sar) ;

   *nexact = 0;
   /* do we have a search by number ? */
   if (ur->id > 0 && ur->N_chnks == 0) {
      if (as->nmatch) {
         snprintf (lbuf, 480*sizeof(char), "Best match for %s (code %-3d):", ur->orig_label, ur->id);
         for (ii=0; ii<as->nmatch; ++ii) {
            snprintf (lbuf, 480*sizeof(char), "%s\n   %s", lbuf, aa->reg[as->iloc[ii]]->orig_label);
         }
         *nexact = as->nmatch;
      }else {
         snprintf (lbuf, 480*sizeof(char),  "No match for integer code %-3d", ur->id);
      }
      ADDTO_SARR(sar,lbuf) ;
      goto PACK_AND_GO;
   }
   
   /* the whole deal */
   if (!as->nmatch) {
      snprintf (lbuf, 480*sizeof(char),  "No exact match for %s", ur->orig_label);
      ADDTO_SARR(sar,lbuf) ;
      if (as->score[0] > 0 && as->score[0] > as->score[5]) { /* maybe some useful suggestions */
         snprintf (lbuf, 480*sizeof(char),  "Closest few guesses:");
         ADDTO_SARR(sar,lbuf) ;
         k = 0;
         while (as->score[k] == as->score[0] && k < 5) {
            snprintf (lbuf, 480*sizeof(char),  "   %s, as->score %.3f", aa->reg[as->iloc[0]]->orig_label, as->score[k]);
            ADDTO_SARR(sar,lbuf) ;
            ++k;
         }
      } else {
         snprintf (lbuf, 480*sizeof(char),  "   I don't even have good suggestions.\n"
                           "   Try to be more explicit.");
         ADDTO_SARR(sar,lbuf) ;                  
      }
   } else {
      if (as->score[0] > as->score[1]) { /* unique best fit */
         snprintf (lbuf, 480*sizeof(char),"Best match for %s:\n   %s (code %-3d)", ur->orig_label, aa->reg[as->iloc[0]]->orig_label, aa->reg[as->iloc[0]]->id);
         ADDTO_SARR(sar,lbuf) ;   
         *nexact = 1;         
      } else if ( as->score[0] == as->score[1] && 
                  (as->N > 2 && as->score[1] > as->score[2]) &&
                  Same_Chunks(aa->reg[as->iloc[0]], aa->reg[as->iloc[1]])) { /* LR unspecified  */
         snprintf (lbuf, 480*sizeof(char),"Best match for %s:\n   %s (code %-3d)\n   %s (code %-3d)", 
            ur->orig_label, 
            aa->reg[as->iloc[0]]->orig_label, aa->reg[as->iloc[0]]->id,
            aa->reg[as->iloc[1]]->orig_label, aa->reg[as->iloc[1]]->id);
         ADDTO_SARR(sar,lbuf) ; 
         *nexact = 2;
      } else {
         k=0; 
         snprintf (lbuf, 480*sizeof(char),"%d potential matches for %s:", as->nmatch, ur->orig_label);
         ADDTO_SARR(sar,lbuf) ;   
         while (as->score[k] == as->score[0] && k<aa->N_regions) {
            snprintf (lbuf, 480*sizeof(char),"         %s (code %-3d):", aa->reg[as->iloc[k]]->orig_label, aa->reg[as->iloc[k]]->id);
            ADDTO_SARR(sar,lbuf) ;   
            ++k;
         }
      }
   }
   

   PACK_AND_GO:
   /*- convert list of labels into one big multi-line string -*/
   for( nfind=ii=0 ; ii < sar->num ; ii++ ) nfind += strlen(sar->ar[ii]) ;
   rbuf = AFMALL(char, nfind + 2*sar->num + 32 ) ; rbuf[0] = '\0' ;
   for( ii=0 ; ii < sar->num ; ii++ ){
      strcat(rbuf,sar->ar[ii]) ; strcat(rbuf,"\n") ;
   }

   DESTROY_SARR(sar) ;  sar = NULL;
   
   if (0) {
      INFO_message("Have:\n%s\n", rbuf);
   }
   
   RETURN(rbuf);
}

ATLAS_SEARCH *Free_Atlas_Search(ATLAS_SEARCH *as)
{
   ENTRY("Free_Atlas_Search");
   if (!as) RETURN(NULL);

   if (as->iloc) free(as->iloc);
   if (as->score) free(as->score);
   free(as);
   RETURN(NULL);   
}

/*!
   Return a byte mask for a particular atlas region
*/
THD_3dim_dataset *Atlas_Region_Mask(AFNI_ATLAS_CODES atcode, AFNI_ATLAS_REGION *aar, int *codes, int n_codes)
{
      byte *bmask = NULL, *ba=NULL, LocalHead = 0, build_lr, *lrmask=NULL;
      int ii=0, sb, nxyz, nx, ny, nz, kk, ll = 0, fnd = -1, fnd2=-1;
      THD_3dim_dataset * dset ; /* 01 Aug 2001 */
      THD_3dim_dataset * maskset = NULL;
      char madeupname[500], madeuplabel[40];
      
      ENTRY("Atlas_Region_Mask");
      
      if (!codes || n_codes == 0) {
         ERROR_message("Nothing to do");
         RETURN(NULL);
      }
      
      if (LocalHead) {
         fprintf(stderr,"Looking for %d codes: \n   ", n_codes);
         for (kk=0; kk<n_codes; ++kk) {
            fprintf(stderr,"%d   ", codes[kk]);   
         }
         fprintf(stderr,"\n");
      }
      
      dset = NULL;
      build_lr = 0;
      lrmask = NULL;
      switch (atcode) {
         case CA_EZ_MPM_ATLAS:
            if (dseCA_EZ_MPM == NULL) {
               if (LocalHead) fprintf(stderr,"Loading %s\n", Atlas_Code_to_Atlas_Name(atcode));
               ii = CA_EZ_MPM_load_atlas(); 
               if (ii == 0) {
                  ERROR_message("Could not read MPM atlas(dset %s+tlrc)", 
                              Atlas_Code_to_Atlas_Dset_Name(atcode));
                  RETURN(NULL);
               }
            }
            dset = dseCA_EZ_MPM;
            build_lr = 1;
            break;
         case CA_EZ_ML_ATLAS:
            if (dseCA_EZ_ML == NULL) {
               if (LocalHead) fprintf(stderr,"Loading %s\n",  Atlas_Code_to_Atlas_Name(atcode));
               ii = CA_EZ_ML_load_atlas(); 
               if (ii == 0) {
                  ERROR_message("Could not read ML atlas(dset %s+tlrc)", 
                              Atlas_Code_to_Atlas_Dset_Name(atcode));
                  RETURN(NULL);
               }
            }
            dset = dseCA_EZ_ML;
            build_lr = 1;
            break;
         case CA_EZ_LR_ATLAS:
            if (dseCA_EZ_LR == NULL) {
               if (LocalHead) fprintf(stderr,"Loading %s\n",  Atlas_Code_to_Atlas_Name(atcode));
               ii = CA_EZ_LR_load_atlas(); 
               if (ii == 0) {
                  ERROR_message("Could not read LR atlas(dset %s+tlrc)", 
                              Atlas_Code_to_Atlas_Dset_Name(atcode));
                  RETURN(NULL);
               }
            }
            dset = dseCA_EZ_LR;
            build_lr = 1;
            break;
         case CA_EZ_PMAPS_ATLAS:
            /* Load the PMaps */
            if (dseCA_EZ_PMaps == NULL) {
               if (LocalHead) fprintf(stderr,"Loading %s\n",  Atlas_Code_to_Atlas_Name(atcode));
               ii = CA_EZ_PMaps_load_atlas(); 
               if (ii == 0) {
                  ERROR_message("Could not read PMAPS atlas(dset %s+tlrc)", 
                              Atlas_Code_to_Atlas_Dset_Name(atcode));
                  RETURN(NULL);
               }
            }
            dset = dseCA_EZ_PMaps;
            build_lr = 1;
            break;
         case AFNI_TLRC_ATLAS:
            /* Load the AFNI_TLRC atlas */
            if (dseTT == NULL && dseTT_big == NULL) {
               if (LocalHead) fprintf(stderr,"Loading %s\n", Atlas_Code_to_Atlas_Name(atcode));
               ii = TT_load_atlas() ; 
               if (ii == 0) {
                  ERROR_message("Could not read TLRC atlas (dset %s+tlrc)", 
                              Atlas_Code_to_Atlas_Dset_Name(atcode));
                  RETURN(NULL);
               }

            }
            /* 01 Aug 2001: maybe use big dataset (so don't need both in memory) */
            dset = (dseTT_big != NULL) ? dseTT_big : dseTT ;

            #if 0
            if( dset == dseTT_big ) fprintf(stderr,"TT_whereami using dseTT_big\n") ;
            else                    fprintf(stderr,"TT_whereami using dseTT\n") ;
            #endif
            break;
         default:
            ERROR_message("Should not be here");
            RETURN(NULL);
      }
      
      if (!dset) RETURN(NULL); 
      /* load the dset */
      DSET_load(dset);
      if (LocalHead) INFO_message("   loaded dset");
      
      /* have LR mask ? */
      if (build_lr) {
         if (dseCA_EZ_LR == NULL) {
            if (LocalHead) fprintf(stderr,"Loading %s\n",  Atlas_Code_to_Atlas_Name(atcode));
            ii = CA_EZ_LR_load_atlas(); 
            if (ii == 0) {
               WARNING_message(  "Could not read LR atlas (dset %s+tlrc)\n"
                                 "LR decision will be based on coordinates.",
                                 Atlas_Code_to_Atlas_Dset_Name(atcode));
            } else {
               DSET_load(dseCA_EZ_LR);
               lrmask = DSET_BRICK_ARRAY(dseCA_EZ_LR,0);
               if (!lrmask) { ERROR_message("Unexpected NULL array.\nProceeding without LR mask"); }
            }
         }
      }

      nxyz = DSET_NX(dset) * DSET_NY(dset) * DSET_NZ(dset);
      if (!(bmask = (byte*)calloc(nxyz, sizeof(byte)))) {
         ERROR_message("Failed to allocate for mask");
         RETURN(maskset);
      }
      
      if (atcode != CA_EZ_PMAPS_ATLAS) {
         for (sb=0; sb < DSET_NVALS(dset); ++sb) {
            ba = DSET_BRICK_ARRAY(dset,sb); 
            if (!ba) { ERROR_message("Unexpected NULL array"); free(bmask); bmask = NULL; RETURN(maskset); }

            for (kk=0; kk < n_codes; ++kk) {
               for (ii=0; ii< nxyz; ++ii) {
                  if (ba[ii] == codes[kk]) bmask[ii] = codes[kk];
               }
            }

         }
      } else { /* got to dump a particular sub-brick */
         if (LocalHead) INFO_message("Speciality");
         for (kk=0; kk < n_codes; ++kk) {
            /* find label to go with code */
            ll = 0;
            fnd = -1;
            while (ll<CA_EZ_COUNT && fnd < 0) {
               if (CA_EZ_list[ll].tdval == codes[kk]) fnd = ll;
               else ++ll;
            }
            if (fnd < 0) {ERROR_message("Unexpected negative find"); free(bmask); bmask = NULL; RETURN(maskset); }
            
            if (LocalHead) INFO_message("Looking for sub-brick labeled %s\n", Clean_Atlas_Label(CA_EZ_list[fnd].dsetpref)); 
            fnd2 = -1;
            sb = 0;
            while (sb < DSET_NVALS(dset) && fnd2 < 0) { /* sb in question should be nothing but fnd. But be careful nonetheless */ 
               if (DSET_BRICK_LAB(dset,sb) && strcmp(DSET_BRICK_LAB(dset,sb), Clean_Atlas_Label(CA_EZ_list[fnd].dsetpref)) == 0) fnd2 = sb;
               else ++sb;
            }
            if (fnd2 < 0) {ERROR_message("Unexpected negative find"); free(bmask); bmask = NULL; RETURN(maskset); }
            ba = DSET_BRICK_ARRAY(dset,fnd2); 
            for (ii=0; ii< nxyz; ++ii) {
               if (ba[ii]) bmask[ii] = ba[ii];
            }
         }
      }
      
      /* Now trim the LR business, if required. */
      if (aar->side == 'l' || aar->side == 'r') {
         for (ii=0; ii<nxyz; ++ii) { 
            if ( bmask[ii] && Atlas_Voxel_Side(dset, ii, lrmask) != aar->side ) bmask[ii] = 0; 
         }
         snprintf(madeupname, 400*sizeof(char), "%s.%s.%c",Atlas_Code_to_Atlas_Name(atcode), Clean_Atlas_Label_to_Prefix(Clean_Atlas_Label(aar->orig_label)), aar->side);
         snprintf(madeuplabel, 36*sizeof(char), "%c.%s", aar->side, Clean_Atlas_Label_to_Prefix(Clean_Atlas_Label(aar->orig_label)));
      } else {
         snprintf(madeupname, 400*sizeof(char), "%s.%s",Atlas_Code_to_Atlas_Name(atcode), Clean_Atlas_Label_to_Prefix(Clean_Atlas_Label(aar->orig_label)));
         snprintf(madeuplabel, 36*sizeof(char), "%s", Clean_Atlas_Label_to_Prefix(Clean_Atlas_Label(aar->orig_label)));
      }
      
      /* Now form the output mask dataset */
     
      maskset = EDIT_empty_copy( dset ) ;
      EDIT_dset_items(  maskset,
                          ADN_prefix    , madeupname ,
                          ADN_datum_all , MRI_byte ,
                          ADN_nvals     , 1 ,
                          ADN_ntt       , 0 ,
                          ADN_func_type , ISANAT(dset) ? dset->func_type
                                                       : FUNC_FIM_TYPE ,
                          ADN_brick_label_one, madeuplabel,
                          ADN_directory_name , "./" ,
                          ADN_none ) ;
      
      EDIT_substitute_brick( maskset , 0 , MRI_byte , bmask ) ;  /* will make array */
      
      /* all done */
      RETURN(maskset);
}

/*!
   Try to locate a region in an atlas. 
   
   "left" and "right" strings are not used in the matching.
   
   Returns a search struct containing 
   array (as->iloc) of integers that is aa->N_regions long
   aa->reg[as->iloc[0]]->orig_label is the best match if as->nmatch is > 0
   aa->reg[as->iloc[1]]->orig_label is the second best match, etc.
   
   if ur->id > 0 && ur->N_chnks == 0 : An exact search is done based on ur->id
   
   Returned struct must be freed by the user (Free_Atlas_Search), but can be re-used by
   passing it as the last parameter.
*/ 
ATLAS_SEARCH * Find_Atlas_Regions(AFNI_ATLAS *aa, AFNI_ATLAS_REGION *ur , ATLAS_SEARCH *reusethis )
{
   int chnk_match[500], bs = 0;
   int k = 0, iu=0, lu=0, lr=0, ir=0, MinAcc=0, ngood = 0;
   ATLAS_SEARCH *as=NULL;
   
   ENTRY("Find_Atlas_Regions");
   
   if (!aa || !ur) {
      ERROR_message("NULL input");
      RETURN(as);
   }
   
   if (reusethis) as = reusethis;
   else {
      as = (ATLAS_SEARCH *)malloc(sizeof(ATLAS_SEARCH));
      as->iloc = (int *)calloc(aa->N_regions, sizeof(int));
      as->score = (float *)calloc(aa->N_regions, sizeof(float));
      as->N = aa->N_regions;
      as->nmatch = 0;
   }
   
   if (as->N < aa->N_regions) {
      ERROR_message("Reused as structure too small for this atlas!");
      RETURN(Free_Atlas_Search(as));
   }
   
   /* do we have a search by number ? */
   if (ur->id > 0 && ur->N_chnks == 0) {
      /* search by id */
      as->nmatch = 0;
      for (k=0; k<aa->N_regions; ++k) {/* aa->N_regions */
         if (aa->reg[k]->id == ur->id) { /* found */
            as->iloc[as->nmatch] = k;
            ++as->nmatch;
         }
      }
      RETURN(as);
   }
   
   
   /* Compare each region to input */
   for (k=0; k<aa->N_regions; ++k) {/* aa->N_regions */
      as->score[k] = 0.0; /* as->score of match for particular region */
      for (iu=0; iu < ur->N_chnks; ++iu) {  /* for each chunk in the user input */
         /* find best chunk match in atlas region, no regard for ordering of chunks */
         lu = strlen(ur->chnks[iu]); 
         for (ir=0; ir < aa->reg[k]->N_chnks; ++ir) {
            chnk_match[ir] = 0; /* how well does atlas region chunk ir match with user chunk */
            lr = strlen(aa->reg[k]->chnks[ir]);
            if (strncmp(ur->chnks[iu], aa->reg[k]->chnks[ir], MIN_PAIR( lu, lr)) == 0) {
               /* one of the strings is inside the other */
               if (lu == lr) { /* identical match */
                  chnk_match[ir] = 4;
               } if (lu < lr) { /* user provided subset */
                  chnk_match[ir] = 2;
               } if (lu > lr) { /* user provided superset */
                  chnk_match[ir] = 1;
               }
               /* fprintf(stderr,"User string %s, Region %s: match = %d\n", 
                  ur->chnks[iu], aa->reg[k]->chnks[ir],chnk_match[ir]);*/
            }
         }
         /* keep the best match for iu */
         bs = 0;
         for (ir=0; ir < aa->reg[k]->N_chnks; ++ir) {
            if (bs < chnk_match[ir]) bs = chnk_match[ir];
         }
         if (!bs) {
            /* user provided a chunk that matched nothing, penalize! 
               (you might want to skip penalty for first chunks, 
               if they are l,r, left, or right ...) */
            if (  0 && iu == 0 &&
                  (  strcmp("l",ur->chnks[0]) == 0 ||
                     strcmp("l",ur->chnks[0]) == 0 ||
                     strcmp("l",ur->chnks[0]) == 0 ||
                     strcmp("l",ur->chnks[0]) == 0 ) ) {
               /* clemency please*/
               bs = 0;        
            } else { /* big penalty if user provides bad clues */
               bs = -4;
            }
         }
         
         /* add the as->score contribution for this chunk */
         as->score[k] += (float)bs;
      }
      /* small penalty if number of chunks is larger in label */
      if (aa->reg[k]->N_chnks - ur->N_chnks > 0) {
         as->score[k] -= (aa->reg[k]->N_chnks - ur->N_chnks);
      }
      /* bias by side matching */
      if (SIDE_MATCH(ur->side, aa->reg[k]->side)) {
         ++as->score[k];
      }
      /* fprintf (stderr,"Region %s as->scored %f with %s\n",  aa->reg[k]->orig_label, as->score[k], ur->orig_label);  */
   }
   
   /* sort the as->scores, largest to smallest */
   {  
      int *itmp = NULL;
      itmp = z_iqsort(as->score, aa->N_regions);
      if (itmp) {
         memcpy((void*)as->iloc, (void *)itmp, aa->N_regions*sizeof(int));
         free(itmp); itmp = NULL;
      }else {
         ERROR_message("Error sorting!");
         RETURN(Free_Atlas_Search(as));
      }
   }
      
   /* show results where at least all chunks had partial match */
   k = 0;
   ngood = 0;
   MinAcc = 2*ur->N_chnks; /* the least acceptable value should be a partial match for user supplied chunk */ 
   while (as->score[k] >= MinAcc && k < aa->N_regions) {
      /* fprintf (stderr,"Match %d for %s is %s at as->score %f\n", k, ur->orig_label, aa->reg[as->iloc[k]]->orig_label, as->score[k]); */
      ++ngood;
      ++k;
   }
   
   if (!ngood) {
      /* fprintf (stderr,  "No match for %s\n", ur->orig_label); */
      if (as->score[0] > 0 && as->score[0] > as->score[5]) { /* maybe some useful suggestions */
         /* fprintf (stderr,  "Closest few guesses:\n"); */
         k = 0;
         while (as->score[k] == as->score[0] && k < 5) {
            /* fprintf (stderr,  "   %s, as->score %f\n", aa->reg[as->iloc[0]]->orig_label, as->score[k]); */
            ++k;
         }
      } else {
         /* fprintf (stderr,  "   I don't even have good suggestions.\n"
                           "   Try to be more explicit.\n"); */
      }
      
      as->nmatch = 0;
   } else {
      if (as->score[0] > as->score[1]) { /* unique best fit */
         /* fprintf (stderr,"Best match for %s is %s (code %d)\n", 
                     ur->orig_label, aa->reg[as->iloc[0]]->orig_label, aa->reg[as->iloc[0]]->id); */
         as->nmatch = 1;
      } else {
         k=0; 
         /* fprintf (stderr,"Potential match for %s:\n", ur->orig_label); */
         while (as->score[k] == as->score[0] && k<aa->N_regions) {
            /* fprintf (stderr,"         %s (code %d):\n", aa->reg[as->iloc[k]]->orig_label, aa->reg[as->iloc[k]]->id); */
            as->nmatch += 1;
            ++k;
         }
      }
   }
     
     
   RETURN(as);
}

char * Atlas_Prob_String(float p)
{
   static char probs[256];
   
   if (p == -2.0) {
      sprintf(probs," MPM");   
   } else if (p == -1.0) { 
      sprintf(probs," ---");   
   } else {
      sprintf(probs,"%.2f", p); 
   }
   return(probs);
   
}

char * Atlas_Code_String(int c)
{
   static char codes[256];
   
   if (c == -1) {
      sprintf(codes,"---");   
   } else {
      sprintf(codes,"%3d", c); 
   } 
   return(codes);
   
}

char * Clean_Atlas_Label( char *lb)
{
   static char lab_buf[256];
   int nlab=0, nn=0;
   
   ENTRY("Clean_Atlas_Label");
   
   lab_buf[0] = '\0';
   
   if (!lb) {
      ERROR_message("NULL input!\n");
      RETURN(lab_buf);
   }
   
   nlab = strlen(lb); 
   if (nlab > 250) {
      ERROR_message("Dset labels too long!\n");
      RETURN(lab_buf);
   }
   
   nn = nlab-1;
   while (nn >=0 && lb[nn] == '.') --nn;
   
   
   nlab = nn;
   nn = 0;
   if (nlab) {
      while (nn<=nlab) {
         lab_buf[nn] = lb[nn]; 
         ++nn;
      }
      lab_buf[nn] = '\0';
   }
   
   /* fprintf(stderr,"lbl = %s, clean = %s\n", lb, lab_buf); */
   RETURN(lab_buf);
}

char * Clean_Atlas_Label_to_Prefix( char *lb)
{
   static char lab_buf[256];
   int nn=0, cnt=0, nlab=0;
   
   ENTRY("Clean_Atlas_Label_to_Prefix");
   
   lab_buf[0] = '\0';
   
   nlab = strlen(lb); 
   if (nlab > 250) {
      ERROR_message("Dset labels too long!\n");
      RETURN(lab_buf);
   }

   cnt=0;
   for (nn=0; nn<nlab; ++nn) {
      if (!IS_LETTER(lb[nn]) && lb[nn] != '-' && lb[nn] != '_' && lb[nn] != '.') {
         if (cnt==0 || lab_buf[cnt-1] != '_') {
            lab_buf[cnt] = '_';
            ++cnt;
         } 
      } else {
         lab_buf[cnt] = lb[nn];
         ++cnt;
      }
   }
   
   lab_buf[cnt] = '\0';
   
   RETURN(lab_buf);
}   

const char *Space_Code_to_Space_Name (AFNI_STD_SPACES cod)
{
   ENTRY("Space_Code_to_Space_Name");
   
   switch(cod) {
      case UNKNOWN_SPC:
         RETURN("Unknown");
      case AFNI_TLRC_SPC:
         RETURN("Afni_TLRC");
      case MNI_SPC:
         RETURN("MNI");
      case MNI_ANAT_SPC:
         RETURN("MNI_Anatomical");
      case NUMBER_OF_SPC:
         RETURN("Flag for number of spaces");
      default:
         RETURN("Willis?");
   }
   
   RETURN("No way Willis.");
}

AFNI_ATLAS_CODES Atlas_Name_to_Atlas_Code (char *name)
{
   int k = 0;
   ENTRY("Atlas_Name_to_Atlas_Code");
   
   for (k=UNKNOWN_ATLAS; k<NUMBER_OF_ATLASES; ++k) {
      if (strlen(name) == strlen(Atlas_Code_to_Atlas_Name(k)) &&
            strcmp(name, Atlas_Code_to_Atlas_Name(k)) == 0) {
            RETURN(k);
      }
   }
   
   RETURN(UNKNOWN_ATLAS);
}

const char *Atlas_Code_to_Atlas_Dset_Name (AFNI_ATLAS_CODES cod)
{
   ENTRY("Atlas_Code_to_Atlas_Name");
   
   switch(cod) {
      case UNKNOWN_ATLAS:
         RETURN("Unknown ");
      case AFNI_TLRC_ATLAS:
         RETURN(TT_DAEMON_TT_PREFIX);
      case CA_EZ_MPM_ATLAS:
         RETURN(CA_EZ_MPM_TT_PREFIX);
      case CA_EZ_ML_ATLAS:
         RETURN(CA_EZ_ML_TT_PREFIX);
      case CA_EZ_LR_ATLAS:
         RETURN(CA_EZ_LR_TT_PREFIX);
      case CA_EZ_PMAPS_ATLAS:
         RETURN(CA_EZ_PMaps_TT_PREFIX);
      case NUMBER_OF_ATLASES:
         RETURN("Flag for number of atlases");
      default:
         RETURN("Bert?");
   }
   
   RETURN("No way Bert.");
}
const char *Atlas_Code_to_Atlas_Name (AFNI_ATLAS_CODES cod)
{
   ENTRY("Atlas_Code_to_Atlas_Name");
   
   switch(cod) {
      case UNKNOWN_ATLAS:
         RETURN("Unknown");
      case AFNI_TLRC_ATLAS:
         RETURN("TT_Daemon");
      case CA_EZ_MPM_ATLAS:
         RETURN("CA_MPM");
      case CA_EZ_ML_ATLAS:
         RETURN("CA_MacroLabels");
      case CA_EZ_LR_ATLAS:
         RETURN("CA_LeftRight");
      case CA_EZ_PMAPS_ATLAS:
         RETURN("CA_PMaps");
      case NUMBER_OF_ATLASES:
         RETURN("Flag for number of atlases");
      default:
         RETURN("Bert?");
   }
   
   RETURN("No way Bert.");
}

/*!
   Locate or create a zone of a particular level.
   \param aq (ATLAS_QUERY *)
   \param level (int)
   \return zn (ATLAS_ZONE) a new zone if none was 
               found in aq at the proper level. Or 
               whatever was found in aq.
*/
ATLAS_ZONE *Get_Atlas_Zone(ATLAS_QUERY *aq, int level)
{  
   int ii=0, fnd=0;
   ATLAS_ZONE *zn=NULL;
   
   ENTRY("Get_Atlas_Zone");
   
   if (!aq) {
      ERROR_message("NULL atlas query");
      RETURN(zn);
   }
   
   /* fprintf(stderr,"Looking for zone level %d\n", level); */
   
   ii = 0;
   while (ii<aq->N_zone) {
      if (aq->zone[ii]->level == level) {
         if (fnd) {
            WARNING_message(  "More than one (%d) zone of level %d found in query.\n"
                              "Function will ignore duplicates.\n", fnd, level ) ;
         }else {
            /* fprintf(stderr,"Zone with level %d found\n", level);  */
            zn = aq->zone[ii];  
         }
         ++fnd;
      }
      ++ii;
   }
   
   if (!zn) {
      /* fprintf(stderr,"Zone with level %d NOT found\n", level);  */
      zn = (ATLAS_ZONE *)malloc(sizeof(ATLAS_ZONE));
      zn->level = level;
      zn->N_label = 0;
      zn->label = NULL;
      zn->code = NULL;
      zn->atcode = NULL;
      zn->prob = NULL;
      zn->radius = NULL;
   }
   
   RETURN(zn);
}
/*!
   Create or Add to an Atlas Zone
   \param zn (ATLAS_ZONE *) If null then create a new one.
                            If a zone is given then add new labels to it
   \param level (int) a classifier of zones, in a way, perhaps most cases, it is the equivalent
                     of the within parameter. 
   \param label (char *) a label string for this zone
   \param code (int) the integer code for this zone (not added if label is NULL)
   \param prob (float) the probability of that zone being label (not added if label is NULL)
   \param within (float) radius of label's occurrence
   \return zno (ATLAS_ZONE *) either a new structure (if zn == NULL) or a modified one (if zn != NULL)

   \sa free with Free_Atlas_Zone
*/
ATLAS_ZONE *Atlas_Zone(ATLAS_ZONE *zn, int level, char *label, int code, float prob, float within, AFNI_ATLAS_CODES atcode) 
{
   ATLAS_ZONE *zno = NULL;
   
   ENTRY("Atlas_Zone");
   
   if ( (prob < 0 && prob != -1.0 && prob != -2.0) || prob > 1) {
      ERROR_message( "Probability must be 0<=prob<=1 or -1.0\n"
                     "You sent %f\n", prob);
      RETURN(zno);
   }
   if (within < 0 && within != -1.0 ) {
      ERROR_message( "'Within' must be > 0 or -1.0\n"
                     "You sent %f\n", within);
      RETURN(zno);
   }
   if (!zn) {
      zno = (ATLAS_ZONE *)malloc(sizeof(ATLAS_ZONE));
      zno->level = level;
      zno->N_label = 0;
      zno->label = NULL;
      zno->code = NULL;
      zno->atcode = NULL;
      zno->prob = NULL;
      zno->radius = NULL;
   } else {
      zno = zn;
      if (zno->level != level) {
         ERROR_message( "When zn is not null\n"
                        "level (%d) must be equal to zn->level (%d)\n", level, zn->level);
         RETURN(zno);
      }
   }
   if (label) {
      /* add label */
      ++zno->N_label;
      zno->label = (char **)realloc(zno->label, sizeof(char *)*zno->N_label);
      zno->label[zno->N_label-1] = strdup(label);
      zno->code = (int *)realloc(zno->code, sizeof(int)*zno->N_label);
      zno->code[zno->N_label-1] = code;
      zno->atcode = (int *)realloc(zno->atcode, sizeof(int)*zno->N_label);
      zno->atcode[zno->N_label-1] = atcode;
      zno->prob = (float *)realloc(zno->prob, sizeof(float)*zno->N_label);
      zno->prob[zno->N_label-1] = prob;
      zno->radius = (float *)realloc(zno->radius, sizeof(float)*zno->N_label);
      zno->radius[zno->N_label-1] = within;
   }
   
   RETURN(zno);
}

ATLAS_ZONE *Free_Atlas_Zone(ATLAS_ZONE *zn)
{
   int k=0;
   
   ENTRY("Free_Atlas_Zone");
   
   if (!zn) RETURN(NULL);
   
   if (zn->label) {
      for (k=0; k<zn->N_label; ++k) if (zn->label[k]) free(zn->label[k]);
      free(zn->label);
   }
   free(zn->code);
   free(zn->atcode);
   free(zn->prob);
   free(zn->radius);
   free(zn);
        
   RETURN(NULL);
}

void Show_Atlas_Zone(ATLAS_ZONE *zn)
{
   int k=0;
   char probs[16], codes[16], radiuss[16];
   
   ENTRY("Show_Atlas_Zone");
   
   if (!zn) { fprintf(stderr,"NULL zone"); EXRETURN;}
   
   fprintf(stderr,
            "     level     :   %d\n"
            "     N_label(s):   %d\n",
            zn->level, zn->N_label);
   if (zn->label) {
      for (k=0; k<zn->N_label; ++k) {
         sprintf(probs, "%s", Atlas_Prob_String(zn->prob[k]));
         sprintf(codes, "%s", Atlas_Code_String(zn->code[k])); 
         
         sprintf(radiuss,"%.1f", zn->radius[k]);
         
         fprintf(stderr,"     %d: label=%-32s, prob=%-3s, rad=%-3s, code=%-3s, atlas=%-10s\n", 
                  k, Clean_Atlas_Label(zn->label[k]), probs, radiuss, codes, Atlas_Code_to_Atlas_Name (zn->atcode[k]));
      }
   } else {
      fprintf(stderr,"     label (NULL");
   }
     
   EXRETURN;
}

void Show_Atlas_Query(ATLAS_QUERY *aq)
{
   int k=0;
   
   ENTRY("Show_Atlas_Query");
   
   if (!aq) { fprintf(stderr,"NULL query\n"); EXRETURN;}
   
   fprintf(stderr,
            "----------------------\n"
            "Atlas_Query: %d zones\n", 
            aq->N_zone);
   if (aq->zone) {
      for (k=0; k<aq->N_zone; ++k) {
         fprintf(stderr,"  zone[%d]:\n", k);
         Show_Atlas_Zone(aq->zone[k]);
         fprintf(stderr,"\n");
      }
   } else {
      fprintf(stderr,"  zone (NULL)\n");
   }
   fprintf(stderr,
            "----------------------\n");
   EXRETURN;
}

ATLAS_QUERY *Add_To_Atlas_Query(ATLAS_QUERY *aq, ATLAS_ZONE *zn)
{
   int i, fnd=0;
   ATLAS_QUERY *aqo;
   
   ENTRY("Add_To_Atlas_Query");
   
   if (!aq) {
      aqo = (ATLAS_QUERY *)malloc(sizeof(ATLAS_QUERY));
      aqo->N_zone = 0;
      aqo->zone = NULL;
   }else{
      aqo = aq;
   }
   
   if (zn) {
      /* make sure this one does not exist already */
      for (i=0; i<aqo->N_zone; ++i) if (aqo->zone[i] == zn) fnd = 1;
      if (!fnd) {
         /* add zone */
         ++aqo->N_zone;
         aqo->zone = (ATLAS_ZONE **)realloc(aqo->zone, sizeof(ATLAS_ZONE*)*aqo->N_zone);
         aqo->zone[aqo->N_zone-1] = zn;
      }
   }
   RETURN(aqo);
}

ATLAS_QUERY *Free_Atlas_Query(ATLAS_QUERY *aq)
{
   int k=0;
   
   ENTRY("Free_Atlas_Query");
   
   if (!aq) RETURN(NULL);
   
   if (aq->zone) {
      for (k=0; k<aq->N_zone; ++k) if (aq->zone[k]) Free_Atlas_Zone(aq->zone[k]);
      free(aq->zone);
   }
   free(aq);
        
   RETURN(NULL);
}

int CA_EZ_ML_load_atlas(void)
{
   char *epath ;
   char atpref[256];
   
   ENTRY("CA_EZ_ML_load_atlas");
   
   if( have_dseCA_EZ_ML >= 0 ) RETURN(have_dseCA_EZ_ML) ;  /* for later calls */

   have_dseCA_EZ_ML = 0 ;  /* don't have it yet */

   /*----- 20 Aug 2001: see if user specified alternate database -----*/

   epath = getenv("AFNI_CA_EZ_ML_ATLAS_DATASET") ;   /* suggested path, if any */
   snprintf(atpref, 255*sizeof(char), "%s+tlrc", CA_EZ_ML_TT_PREFIX);
   dseCA_EZ_ML = get_altas( epath, atpref ) ;  /* try to open it */
   if (!dseCA_EZ_ML) { /* try for NIFTI */
      snprintf(atpref, 255*sizeof(char), "%s.nii.gz", CA_EZ_ML_TT_PREFIX);
      dseCA_EZ_ML = get_altas( epath, atpref) ;
   }
   if( dseCA_EZ_ML != NULL ){                     /* got it!!! */
      have_dseCA_EZ_ML = 1; RETURN(1);
   }

   RETURN(0) ; /* got here -> didn't find it */
}

int CA_EZ_LR_load_atlas(void)
{
   char *epath ;
   char atpref[256];
   
   ENTRY("CA_EZ_LR_load_atlas");
   
   if( have_dseCA_EZ_LR >= 0 ) RETURN(have_dseCA_EZ_LR) ;  /* for later calls */

   have_dseCA_EZ_LR = 0 ;  /* don't have it yet */

   /*----- 20 Aug 2001: see if user specified alternate database -----*/

   epath = getenv("AFNI_CA_EZ_LR_ATLAS_DATASET") ;   /* suggested path, if any */
   snprintf(atpref, 255*sizeof(char), "%s+tlrc", CA_EZ_LR_TT_PREFIX);
   dseCA_EZ_LR = get_altas( epath, atpref ) ;  /* try to open it */
   if (!dseCA_EZ_LR) { /* try for NIFTI */
      snprintf(atpref, 255*sizeof(char), "%s.nii.gz", CA_EZ_LR_TT_PREFIX);
      dseCA_EZ_LR = get_altas( epath, atpref) ;
   }
   if( dseCA_EZ_LR != NULL ){                     /* got it!!! */
      have_dseCA_EZ_LR = 1; RETURN(1);
   }

   RETURN(0) ; /* got here -> didn't find it */
}

int CA_EZ_MPM_load_atlas(void)
{
   char *epath ;
   char atpref[256];
   
   ENTRY("CA_EZ_MPM_load_atlas");
   
   if( have_dseCA_EZ_MPM >= 0 ) RETURN(have_dseCA_EZ_MPM) ;  /* for later calls */

   have_dseCA_EZ_MPM = 0 ;  /* don't have it yet */

   /*----- 20 Aug 2001: see if user specified alternate database -----*/

   epath = getenv("AFNI_CA_EZ_MPM_ATLAS_DATASET") ;   /* suggested path, if any */
   snprintf(atpref, 255*sizeof(char), "%s+tlrc", CA_EZ_MPM_TT_PREFIX);  
   dseCA_EZ_MPM = get_altas( epath, atpref ) ;  /* try to open it */
   if (!dseCA_EZ_MPM) { /* try for NIFTI */
      snprintf(atpref, 255*sizeof(char), "%s.nii.gz", CA_EZ_MPM_TT_PREFIX);  
      dseCA_EZ_MPM = get_altas( epath, atpref) ;
   }
   if( dseCA_EZ_MPM != NULL ){                     /* got it!!! */
      have_dseCA_EZ_MPM = 1; RETURN(1);
   }

   RETURN(0) ; /* got here -> didn't find it */
}

int CA_EZ_PMaps_load_atlas(void)
{
   char *epath ;
   char atpref[256];
   
   ENTRY("CA_EZ_PMaps_load_atlas");
   
   if( have_dseCA_EZ_PMaps >= 0 ) RETURN(have_dseCA_EZ_PMaps) ;  /* for later calls */

   have_dseCA_EZ_PMaps = 0 ;  /* don't have it yet */

   /*----- 20 Aug 2001: see if user specified alternate database -----*/

   epath = getenv("AFNI_CA_EZ_PMAPS_ATLAS_DATASET") ;   /* suggested path, if any */
   snprintf(atpref, 255*sizeof(char), "%s+tlrc", CA_EZ_PMaps_TT_PREFIX) ;
   dseCA_EZ_PMaps = get_altas( epath, atpref ) ;  /* try to open it */
   if (!dseCA_EZ_PMaps) { /* try for NIFTI */
      snprintf(atpref, 255*sizeof(char), "%s.nii.gz", CA_EZ_PMaps_TT_PREFIX) ;
      dseCA_EZ_PMaps = get_altas( epath, atpref) ;
   }
   if( dseCA_EZ_PMaps != NULL ){                     /* got it!!! */
      have_dseCA_EZ_PMaps = 1; RETURN(1);
   }

   RETURN(0) ; /* got here -> didn't find it */
}

void CA_EZ_MPM_purge_atlas(void)
{
   PURGE_DSET(dseCA_EZ_MPM); return;
}

void CA_EZ_PMaps_purge_atlas(void)
{
   PURGE_DSET(dseCA_EZ_PMaps); return;
}

void CA_EZ_ML_purge_atlas(void)
{
   PURGE_DSET(dseCA_EZ_ML); return;
}

void CA_EZ_LR_purge_atlas(void)
{
   PURGE_DSET(dseCA_EZ_LR); return;
}


char *whereami_9yards(ATLAS_COORD aci, ATLAS_QUERY **wamip, AFNI_ATLAS_CODES *atlaslist, int N_atlaslist)
{
   char *s = NULL;
   float probkey = -1.0;
   int ii,kk , ix,jy,kz , nx,ny,nz,nxy , aa,bb,cc , ff,baf,rff, iatlas=0,mxlablen=0, mxelm=0, sb = 0 ;
   THD_ivec3 ijk ;
   byte *ba = NULL ;
   THD_string_array *sar ;
   char *blab ;
   char lbuf[256] , *rbuf ;
   int nfind, b_find[MAX_FIND], rr_find[MAX_FIND] ;
   ATLAS_QUERY *wami = NULL;
   ATLAS_ZONE *zn = NULL;
   AFNI_ATLAS_CODES atcode=UNKNOWN_ATLAS;
   THD_3dim_dataset * dset = NULL ;
   THD_fvec3 vn3, vo3;
   ATLAS_COORD ac;
   byte LocalHead = 0;
   
   ENTRY("whereami_9yards");
   
   if (0 && *wamip) { /* Could be building on other wamis */
      ERROR_message("Send me a null wamip baby\n");
      RETURN(s);
   }
   
   if (!atlaslist || N_atlaslist == 0) {
      ERROR_message("Send me a non null or non empty atlaslist\n");
      RETURN(s);
   }
   
   /* check on coord system (!!have to change coord system depending on atlas) */
   switch (aci.space) {
      case MNI_ANAT_SPC: /* change to AFNI_TLRC */
         LOAD_FVEC3(vo3, aci.x, aci.y, aci.z);
         vn3 = THD_mnia_to_tta_N27(vo3);
         ac.x = vn3.xyz[0]; ac.y = vn3.xyz[1]; ac.z = vn3.xyz[2]; ac.space = AFNI_TLRC_SPC;
         break;
      case MNI_SPC: /* change to AFNI_TLRC*/
         LOAD_FVEC3(vo3, aci.x, aci.y, aci.z);
         vn3 = THD_mni_to_tta_N27(vo3);
         ac.x = vn3.xyz[0]; ac.y = vn3.xyz[1]; ac.z = vn3.xyz[2]; ac.space = AFNI_TLRC_SPC;
         break;
      case AFNI_TLRC_SPC: /* make conversion using 12 pwl xform */
         ac.x = aci.x; ac.y = aci.y; ac.z = aci.z; ac.space = AFNI_TLRC_SPC;
         break;
      default:
         ERROR_message("Coordinates in bad space.");
         RETURN(s);
   }
   
   
   if (!*wamip) { /* A new query structure, if necessary*/
      if (LocalHead) INFO_message("New wami");
      wami = Add_To_Atlas_Query(NULL, NULL);
   }
   
   if (LocalHead) INFO_message("Coords: %f %f %f (%d)\n", ac.x, ac.y, ac.z, ac.space);
   
   for (iatlas=0; iatlas<N_atlaslist; ++iatlas) {/* iatlas loop */
      atcode = atlaslist[iatlas];
      if (LocalHead) INFO_message("Now Processing atlas %s (%d)", Atlas_Code_to_Atlas_Name(atcode), atcode);
      dset = NULL;
      switch (atcode) {
         case CA_EZ_MPM_ATLAS:
            /* Load the MPM */
            if (dseCA_EZ_MPM == NULL) {
               if (LocalHead) fprintf(stderr,"Loading %s\n", Atlas_Code_to_Atlas_Name(atcode));
               ii = CA_EZ_MPM_load_atlas(); 
               if (ii == 0) {
                  WARNING_message("Could not read MPM atlas(dset %s+tlrc)", 
                              Atlas_Code_to_Atlas_Dset_Name(atcode));
                  break;
               }
            }
            dset = dseCA_EZ_MPM;
            mxlablen = CA_EZ_CMAX;
            mxelm = CA_EZ_COUNT;
            probkey = -2.0;
            break;
         case CA_EZ_ML_ATLAS:
            /* Load the MacroLabels */
            if (dseCA_EZ_ML == NULL) {
               if (LocalHead) fprintf(stderr,"Loading %s\n",  Atlas_Code_to_Atlas_Name(atcode));
               ii = CA_EZ_ML_load_atlas(); 
               if (ii == 0) {
                  WARNING_message("Could not read ML atlas (dset %s+tlrc)", 
                              Atlas_Code_to_Atlas_Dset_Name(atcode));
                  break;
               }
            }
            dset = dseCA_EZ_ML;
            atcode = CA_EZ_ML_ATLAS;
            mxlablen = ML_EZ_CMAX;
            mxelm = ML_EZ_COUNT;
            probkey = -1.0;
            break;
         case CA_EZ_LR_ATLAS:
            /* Load the MacroLabels */
            if (dseCA_EZ_LR == NULL) {
               if (LocalHead) fprintf(stderr,"Loading %s\n",  Atlas_Code_to_Atlas_Name(atcode));
               ii = CA_EZ_LR_load_atlas(); 
               if (ii == 0) {
                  WARNING_message("Could not read LR atlas (dset %s+tlrc)", 
                              Atlas_Code_to_Atlas_Dset_Name(atcode));
                  break;
               }
            }
            dset = dseCA_EZ_LR;
            atcode = CA_EZ_LR_ATLAS;
            mxlablen = LR_EZ_CMAX;
            mxelm = LR_EZ_COUNT;
            probkey = -1.0;
            break;
         case CA_EZ_PMAPS_ATLAS:
            /* Load the PMaps */
            if (dseCA_EZ_PMaps == NULL) {
               if (LocalHead) fprintf(stderr,"Loading %s\n",  Atlas_Code_to_Atlas_Name(atcode));
               ii = CA_EZ_PMaps_load_atlas(); 
               if (ii == 0) {
                  WARNING_message("Could not read PMAPS atlas (dset %s+tlrc)", 
                              Atlas_Code_to_Atlas_Dset_Name(atcode));
                  break;
               }
            }
            dset = dseCA_EZ_PMaps;
            mxlablen = CA_EZ_CMAX;
            mxelm = CA_EZ_COUNT;
            break;
         case AFNI_TLRC_ATLAS:
            /* Load the AFNI_TLRC atlas */
            if (dseTT == NULL && dseTT_big == NULL) {
               if (LocalHead) fprintf(stderr,"Loading %s\n", Atlas_Code_to_Atlas_Name(atcode));
               ii = TT_load_atlas() ; 
               if (ii == 0) {
                  WARNING_message("Could not read TLRC atlas (dset %s+tlrc)", 
                              Atlas_Code_to_Atlas_Dset_Name(atcode));
                  break;
               }

            }
            /* 01 Aug 2001: maybe use big dataset (so don't need both in memory) */
            dset = (dseTT_big != NULL) ? dseTT_big : dseTT ;

            #if 0
            if( dset == dseTT_big ) fprintf(stderr,"TT_whereami using dseTT_big\n") ;
            else                    fprintf(stderr,"TT_whereami using dseTT\n") ;
            #endif
            mxlablen = TTO_CMAX;
            mxelm = TTO_COUNT;
            probkey = -1.0;
            break;
         default:
            ERROR_message("Should not be here");
            RETURN(s);
      }
      
      if (!dset) continue; 
      
      /* load the dset */
      DSET_load(dset);
      if (LocalHead) INFO_message("   loaded dset");
      
      if (atcode == CA_EZ_ML_ATLAS || atcode == CA_EZ_MPM_ATLAS || atcode == AFNI_TLRC_ATLAS || atcode == CA_EZ_LR_ATLAS ) { /* the multi-radius searches */
         for (sb=0; sb < DSET_NVALS(dset); ++sb) {
            if (LocalHead)  fprintf(stderr,"Processing sub-brick %d with %s\n",sb,  Atlas_Code_to_Atlas_Name(atcode));  
            ba = DSET_BRICK_ARRAY(dset,sb); if (!ba) { ERROR_message("Unexpected NULL array"); RETURN(s); }

            if( wamiclust_CA_EZ == NULL ){
               wamiclust_CA_EZ = MCW_build_mask( 1.0,1.0,1.0 , WAMIRAD ) ;
               if( wamiclust_CA_EZ == NULL ) RETURN(NULL) ;  /* should not happen! */

               for( ii=0 ; ii < wamiclust_CA_EZ->num_pt ; ii++ )       /* load radius */
                  wamiclust_CA_EZ->mag[ii] = (int)rint(sqrt((double)(
                                                  wamiclust_CA_EZ->i[ii]*wamiclust_CA_EZ->i[ii]
                                                 +wamiclust_CA_EZ->j[ii]*wamiclust_CA_EZ->j[ii]
                                                 +wamiclust_CA_EZ->k[ii]*wamiclust_CA_EZ->k[ii]))) ;

               MCW_sort_cluster( wamiclust_CA_EZ ) ;  /* sort by radius */
            }

            /*-- find locations near the given one that are in the Atlas --*/

            ijk = THD_3dmm_to_3dind( dset , TEMP_FVEC3(ac.x,ac.y,ac.z) ) ;  /* get indexes */
            UNLOAD_IVEC3(ijk,ix,jy,kz) ;                               /* from coords */

            nx = DSET_NX(dset) ;               /* size of atlas dataset axes */
            ny = DSET_NY(dset) ;
            nz = DSET_NZ(dset) ; nxy = nx*ny ;

            nfind = 0 ;

            /*-- check the exact input location --*/

            kk = ix + jy*nx + kz*nxy ;        /* index into brick arrays */
            if( ba[kk] != 0 && !(atcode == CA_EZ_MPM_ATLAS && ba[kk] < CA_EZ_MPM_MIN)){
               b_find[0] = ba[kk] ;
               rr_find[0] = 0     ; 
               if (LocalHead)  fprintf(stderr,"Adding b_find[%d]=%d rr_find[%d]=%d\n",nfind, b_find[nfind], nfind, rr_find[nfind]);
               nfind++ ;
            }

            /*-- check locations near it --*/

            for( ii=0 ; ii < wamiclust_CA_EZ->num_pt ; ii++ ){

               /* compute index of nearby location, skipping if outside atlas */

               aa = ix + wamiclust_CA_EZ->i[ii] ; if( aa < 0 || aa >= nx ) continue ;
               bb = jy + wamiclust_CA_EZ->j[ii] ; if( bb < 0 || bb >= ny ) continue ;
               cc = kz + wamiclust_CA_EZ->k[ii] ; if( cc < 0 || cc >= nz ) continue ;

               kk  = aa + bb*nx + cc*nxy ;   /* index into bricks */
               baf = ba[kk] ;                 /* Atlas structure marker there */

               if( baf == 0 )                            continue ;

               for( ff=0 ; ff < nfind ; ff++ ){       /* cast out         */
                  if( baf == b_find[ff] ) baf = 0 ;  /* duplicate labels  */
               }

               if (atcode == CA_EZ_MPM_ATLAS) { /* cast out inappropriate values*/
                  if (baf < CA_EZ_MPM_MIN) baf = 0;
               }

               if( baf == 0 )                            continue ;

               b_find[nfind] = baf ;  /* save what we found */
               rr_find[nfind] = (int) wamiclust_CA_EZ->mag[ii] ;
               if (LocalHead)  fprintf(stderr,"Adding b_find[%d]=%d rr_find[%d]=%d\n",nfind, b_find[nfind], nfind, rr_find[nfind]);
               nfind++ ;

               if( nfind == MAX_FIND ) {
                 WARNING_message("More regions found than the %d to be reported.\n"
                                 "Please report this case on the AFNI message board.\n", MAX_FIND); 
                 break ;  /* don't find TOO much */
               }
            }

            /*-- bubble-sort what we found, by radius --*/

            if( nfind > 1 ){  /* don't have to sort only 1 result */
              int swap, tmp ;
              do{
                 swap=0 ;
                 for( ii=1 ; ii < nfind ; ii++ ){
                    if( rr_find[ii-1] > rr_find[ii] ){
                      tmp = rr_find[ii-1]; rr_find[ii-1] = rr_find[ii]; rr_find[ii] = tmp;
                      tmp = b_find[ii-1]; b_find[ii-1] = b_find[ii]; b_find[ii] = tmp;
                      swap++ ;
                    }
                 }
              } while(swap) ;
            }

            /* build query results */
            rff = -1 ;  /* rff = radius of last found label */

            if (LocalHead) INFO_message("   %d findings...\n", nfind);  

            for( ff=0 ; ff < nfind ; ff++ ){
               baf = b_find[ff] ; blab = NULL ;

               if( baf != 0 ){                               /* find label     */
                  for( ii=0 ; ii < mxelm ; ii++ ) {          /* in AFNI's atlas list */
                     if( atcode == CA_EZ_MPM_ATLAS && baf == CA_EZ_list[ii].tdval ) break ;
                     else if( atcode == CA_EZ_ML_ATLAS && baf == ML_EZ_list[ii].tdval ) break ;
                     else if( atcode == CA_EZ_LR_ATLAS && baf == LR_EZ_list[ii].tdval ) break ;
                     else if( atcode == AFNI_TLRC_ATLAS && baf == TTO_list[ii].tdval ) break ;
                  }   
                  if( ii < mxelm )  {                     /* always true? */
                     if( atcode == CA_EZ_MPM_ATLAS) blab = CA_EZ_list[ii].name ;
                     else if( atcode == CA_EZ_ML_ATLAS) blab = ML_EZ_list[ii].name ;
                     else if( atcode == CA_EZ_LR_ATLAS) blab = LR_EZ_list[ii].name ;
                     else if( atcode == AFNI_TLRC_ATLAS) blab = TTO_list[ii].name ; 
                  }
               }

               if( blab == NULL  ) {
                  WARNING_message("No label found for code %d in atlas %s\nContinuing...", baf, Atlas_Code_to_Atlas_Name(atcode));
                  continue ;  /* no labels? */
               }

               zn = Get_Atlas_Zone (wami, (int)rr_find[ff] ); /* zone levels are based on search radius */
               zn = Atlas_Zone(  zn, zn->level, 
                                 blab, baf, probkey, rr_find[ff], atcode); 
               wami = Add_To_Atlas_Query(wami, zn);

               rff = rr_find[ff] ;  /* save for next time around */
            }
         } /* for each sub-brick */   
      } else { /* the PMAPS */
         if (LocalHead)  fprintf(stderr,"Processing with %s\n", Atlas_Code_to_Atlas_Name(atcode)); 
         
         /*-- find locations near the given one that are in the Atlas --*/
         ijk = THD_3dmm_to_3dind( dset , TEMP_FVEC3(ac.x,ac.y,ac.z) ) ;  /* get indexes */
         UNLOAD_IVEC3(ijk,ix,jy,kz) ;                               /* from coords */

         nx = DSET_NX(dset) ;               /* size of atlas dataset axes */
         ny = DSET_NY(dset) ;
         nz = DSET_NZ(dset) ; nxy = nx*ny ;
         kk = ix + jy*nx + kz*nxy ;        /* index into brick arrays */
         
         zn = Get_Atlas_Zone(wami, 0);    /* get the zero level zone */
         for (ii=0; ii<DSET_NVALS(dset); ++ii) {
            ba = DSET_BRICK_ARRAY(dset,ii); if (!ba) { ERROR_message("Unexpected NULL array"); RETURN(s); }
            if( ba[kk] != 0 ){
               if( dset->dblk->brick_lab == NULL || dset->dblk->brick_lab[ii] == NULL) {
                  zn = Atlas_Zone(zn, 0, "No Label", -1, (float)ba[kk]/250.0, 0, atcode);
               } else {
                  int nn=0, nlab=0;
                  char *lab_buf; /* do not free this one */
                  if( dset->dblk->brick_lab[ii] && dset->dblk->brick_lab[ii][0] != '\0' ){
                     /* find the code that area label that goes with this sub-brick 
                        Remember to account for the '.'*/
                     blab = NULL; nn = 0; nlab = strlen(dset->dblk->brick_lab[ii]); 
                     if (nlab > mxlablen) {
                        ERROR_message("Dset labels too long!");
                     }
                     while( !blab && nn < mxelm ) {
                        lab_buf = Clean_Atlas_Label(CA_EZ_list[nn].dsetpref);
                        /* fprintf(stderr,"Key:>%s<, N:%d, Str:>%s<\n", dset->dblk->brick_lab[ii], nlab, lab_buf); */
                        if ((nlab == strlen(lab_buf)) && (strcmp(lab_buf, dset->dblk->brick_lab[ii]) == 0) ) {
                           blab = CA_EZ_list[nn].name;
                           /* fprintf(stderr," Matched %s with %s\n", dset->dblk->brick_lab[ii], CA_EZ_list[nn].dsetpref); */
                        }
                        ++nn;
                     } 
                     --nn; /* go back one to get back to proper indexing */
                     if (blab) {       
                        zn = Atlas_Zone(zn, 0, blab, CA_EZ_list[nn].tdval , (float)ba[kk]/250.0, 0, atcode);
                     } else {
                        zn = Atlas_Zone(zn, 0, "Unexpected trouble.", -1, -1.0, 0, atcode);
                     }
                  } else {
                     zn = Atlas_Zone(zn, 0, "Empty Label", -1, (float)ba[kk]/250.0, 0, atcode);
                  }
               }
            }
         }   
         
      }
      /* Show_Atlas_Query(wami); */
   } /* iatlas loop */
   

   /* sort the query by zone levels, be nice */
   if( wami && wami->N_zone > 1 ){  /* don't have to sort only 1 result */
     int swap;
     ATLAS_ZONE *tmp ;
     do{
        swap=0 ;
        for( ii=1 ; ii < wami->N_zone ; ii++ ){
           if( wami->zone[ii-1]->level > wami->zone[ii]->level ){
             tmp = wami->zone[ii-1]; 
             wami->zone[ii-1] = wami->zone[ii]; 
             wami->zone[ii] = tmp;
             swap++ ;
           }
        }
     } while(swap) ;
   }

                                       
   if (LocalHead) { 
      Show_Atlas_Query(wami);   
   }
   
   *wamip = wami;
   RETURN(s);
}

THD_3dim_dataset *THD_3dim_from_ROIstring(char *shar)
{
   THD_3dim_dataset *maskset = NULL;
   AFNI_ATLAS_CODES ac;
   AFNI_ATLAS_REGION *aar= NULL;
   AFNI_ATLAS *aa = NULL;
   ATLAS_SEARCH *as=NULL;
   char *string=NULL;
   int nbest = 0, codes[3], n_codes;
   byte LocalHead = 0;
   
   ENTRY("THD_3dim_from_ROIstring");
   
   ac = UNKNOWN_ATLAS;
   if (!shar) RETURN(maskset);
   if (strlen(shar) < 3) RETURN(maskset);
   Set_ROI_String_Decode_Verbosity(0); /* must be discreet here */

   if (!(aar = ROI_String_Decode(shar, &ac))) {
      if (LocalHead) ERROR_message("ROI string decoding failed.");
      RETURN(maskset);
   } 
   
   if (LocalHead) { 
      fprintf(stderr,"User seeks the following region in atlas %s:\n", Atlas_Code_to_Atlas_Name(ac));
      Show_Atlas_Region(aar);  
   }
   
   /* is this an OK atlas */
   if (ac <= UNKNOWN_ATLAS || ac >= NUMBER_OF_ATLASES) {
      if (LocalHead) ERROR_message("Atlas not found");
      RETURN(maskset);
   }
   if (aar->N_chnks < 1 && aar->id <= 0) {
      if (LocalHead) ERROR_message("bad or empty label");
      RETURN(maskset);
   }
   if (!(aa = Build_Atlas(ac))) {
      if (LocalHead) ERROR_message("Failed to build atlas");
      RETURN(maskset);
   }
   if (LocalHead > 1) Show_Atlas(aa); 
   as = Find_Atlas_Regions(aa,aar, NULL);
   
   /* analyze the matches,*/
   string = Report_Found_Regions(aa, aar, as, &nbest);
   if (string) {
      if (LocalHead) fprintf(stderr,"%s\n", string);   
      free(string); string = NULL;
   } else {
      if (LocalHead) ERROR_message("NULL string returned"); /* something went wrong, although I care not for string ... */
      RETURN(maskset);
   }
   /* Now we know what matches, give me a mask */
   if (nbest) {
      if (nbest > 2) {
         ERROR_message( "More than 2 choices available. I am not use to this.\n"
                        "Please post a message explaining how you generated \n"
                        "this on the AFNI message board");
         RETURN(maskset);
      }
      n_codes = 1;
      codes[0] = aa->reg[as->iloc[0]]->id;
      if (nbest == 2) {
         if (aa->reg[as->iloc[0]]->id != aa->reg[as->iloc[1]]->id) {
            n_codes = 2;
            codes[1] = aa->reg[as->iloc[1]]->id;
         }
      }
      if (!(maskset = Atlas_Region_Mask(ac, aar, codes, n_codes))) {
         ERROR_message("Failed to create mask");
         RETURN(maskset);
      } 
   }
   
   if (aar) aar = Free_Atlas_Region(aar);
   if (as) as = Free_Atlas_Search(as); 
   if (string) free(string); string = NULL;
   if (aa) aa = Free_Atlas(aa);
   
   RETURN(maskset);
}
/* End ZSS: Additions for Eickhoff and Zilles Cytoarchitectonic maps */
