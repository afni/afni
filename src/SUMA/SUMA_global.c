#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; 
   /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; 
   /*!< Global pointer to the vector containing Surface Viewer Structures */
int SUMAg_N_SVv = 0; 
   /*!< Number of SVs stored in SVv */
SUMA_DO *SUMAg_DOv = NULL;	
   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; 
   /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; 
   /*!< Global pointer to structure containing info common to all viewers */
