#include "SUMA_suma.h"
#include "SUMA_plot.h"
#include "SUMA_clippingPlanes.h"

#define SUMA_getStringFromNiml(m_nel, m_attr, m_fv, m_n, m_fail) {\
   char *m_atmp = NULL; \
   int m_nr = 0; \
   m_fail = 0; \
   m_atmp = NI_get_attribute(m_nel, m_attr); \
   if (!m_atmp) {   \
      m_fail = 1; \
   }  \
   m_fv = m_atmp;    \
}

#define SUMA_S2FV_ATTR(m_nel, m_attr, m_fv, m_n, m_fail) {\
   char *m_atmp = NULL; \
   int m_nr = 0; \
   m_fail = 0; \
   m_atmp = NI_get_attribute(m_nel, m_attr); \
   if (!m_atmp) {   \
      m_fail = 1; \
   }  \
   m_nr = SUMA_StringToNum(m_atmp, (void*)m_fv, m_n,1);  \
   if (m_nr != m_n) {  \
      m_fail = 2; \
   }  \
}

Bool clippingPlaneMode = 0;
SUMA_SurfaceObject* clipIdentificationPlane[6];
SUMA_SurfaceObject* axisObject = NULL;

Bool clipPlaneIdentificationMode, previousClipPlaneIdentificationMode=1;
float activeClipPlane[4];
Bool active[6] = {1,1,1,1,1,1};
Bool previouslyActive[6] = {0,0,0,0,0,0};
float clippingPlaneAxisRanges[3][2] = {{0.0f,0.0f},{0.0f,0.0f},{0.0f,0.0f}};
SUMA_SurfaceObject * clippingPlaneIDDisplayableObjects[6] = {NULL, NULL, NULL, NULL, NULL, NULL};
int  selectedPlane;
Bool resetClippingPlanes=0;
float  scrollInc = 1.0;
float  tiltInc = 1.0;
Boolean justEnteredClippingPlaneMode = 1;
float clippingPlaneTheta[SUMA_MAX_N_CLIP_PLANES]={0,90,0,180,270,180};
float clippingPlanePhi[SUMA_MAX_N_CLIP_PLANES]={0,0,90,0,0,270};
Boolean activeClipPlanes = True;
int locallySelectedPlane;
DList *list = NULL;

void initializeIncrement(float objectMinMax[3][2]){
    float max = 0;
    int i;
    float dim;
    
    for (i=0; i<3; ++i){
        dim = objectMinMax[i][1] - objectMinMax[i][0];
        max = MAX(max, dim);
    }
    
    scrollInc = max/40;
}

Boolean toggleClippingPlaneMode(SUMA_SurfaceViewer *sv, Widget w, int *locallySelectedPlane){
    int i, planeIndex;
    char *FuncName = "toggleClippingPlaneMode";

    clippingPlaneMode = !clippingPlaneMode; // Toggle clipping plane state

    if (SUMAg_CF->clippingPlaneVerbose) fprintf(stderr, "### Clipping plane mode %s\n",
        clippingPlaneMode? "on" : "off");

    //Update title bar
    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Update title bar\n");
    sv->GVS[sv->StdView].ClippingPlane =
                         !sv->GVS[sv->StdView].ClippingPlane;
    if (sv->GVS[sv->StdView].ClippingPlane)
        sv->clippingPlaneIncrement = scrollInc;
    SUMA_UpdateViewerTitle(sv);

    if (clippingPlaneMode){
        if (resetClippingPlanes){
            // SUMAg_CF->N_ClipPlanes = 1;
            resetClippingPlanes=0;
        }

        // Make sure there are all six clipping plane with their colored squares but only first active
        if (SUMAg_CF->N_ClipPlanes < 6){
            clippingPlaneMode = 1;
            clipPlaneIdentificationMode = 1;    // Start with colored squares on
            for (i=5; i>=0; --i){
                sprintf(SUMAg_CF->ClipPlanesLabels[SUMAg_CF->N_ClipPlanes], "%d", SUMAg_CF->N_ClipPlanes+1);
                clipPlaneTransform(0,0,0,0,SUMAg_CF->N_ClipPlanes, 0, 0);
                // if (i>0) clipPlaneTransform(0,0,0,0,i-1, 1, 0);
                // if (!makeClipIdentificationPlane(SUMAg_CF->N_ClipPlanes-1, w, sv)){
                if (!makeClipIdentificationPlane(i, w, sv)){
                    fprintf(stderr, "Error SUMA_input: Failed to make clip plane indentification square.\n");
                    exit(1);
                }
            }
            active[0] = 1;    // First clipping plane will be active (as it will be toggled twice)
            previouslyActive[0] = 1;    // First clipping plane will be active (as it will be toggled twice)
            *locallySelectedPlane = 0;
            SUMAg_CF->N_ClipPlanes = 6;
        } else if (!activeClipPlanes){  // Toggle plane 1 on
            if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
                fprintf(stderr, "### Toggle plane 1 on\n");
            clipPlaneTransform(0,0,0,0,0, 1, 0);
            previouslyActive[0] = active[0];
        } else {
            clipPlaneIdentificationMode = previousClipPlaneIdentificationMode;
        }

        // Load saved clipping planes if available
        if (clippingPlaneFile){
            if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
                fprintf(stderr, "### Load clipping planes\n");
            loadSavedClippingPlanes(clippingPlaneFile, locallySelectedPlane);
            sv->clippingPlaneIncrement = scrollInc;
        }
/*
        This part is commented out because it causes the gray ghost plane
        // Turn on clipping planes and their colored squares.  
        // Ghost plane seems to be produced here.  Seems to be related to first plane
        if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
            fprintf(stderr, "### Turn on clipping planes and their colored squares\n");
        // DEBUG for (i=1; i<SUMAg_CF->N_ClipPlanes; ++i){
        // DEBUG for (i=0; i<1; ++i){
        for (i=0; i<SUMAg_CF->N_ClipPlanes; ++i){
            active[i] = !(previouslyActive[i]); // Invert activation state since it's about to be toggled
            clipPlaneTransform(0,0,0,0,i, 1, 0);   // Toggle activation state

            // Display clip plane identification mode if required
            // DEBUG if (clipPlaneIdentificationMode) clipIdentificationPlane[i]->Show = 1;
        }
*/
        if (!((XtPointer)sv->X->GLXAREA)){
            fprintf(stderr, "*** Error: Color map widget, GLXAREA, is NULL\n");
            // SUMA_X_SurfaceViewer_Create();
        }

        if ((XtPointer)sv->X->GLXAREA) SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);    // Refresh

        SUMA_postRedisplay(w, NULL, NULL);  // Refresh window

        // Squares only displayed for active clipping planes
        for (planeIndex=0; planeIndex<SUMAg_CF->N_ClipPlanes; ++planeIndex)
            clipIdentificationPlane[planeIndex]->Show = active[planeIndex];
/*
        This part is commented out because it causes the gray ghost plane
        clipPlaneTransform(0,0,0,0,*locallySelectedPlane, 0, 0);     // Ensure correct plane selected
*/
        // Darken inactive clip planes
        if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
            fprintf(stderr, "### Darken inactive clip planes\n");
        darkenInactiveClipPlaneSquares(*locallySelectedPlane);

        lightenActiveClipPlaneSquare(*locallySelectedPlane);
    } else {
        previousClipPlaneIdentificationMode = clipPlaneIdentificationMode;
        for (i=0; i<6; ++i){
            previouslyActive[i] = active[i];    // Record activation state before leaving clip plane mode
            clipPlaneIdentificationMode = 0;
        }

        // Turn off display of clip plane identification squares
        for (planeIndex=0; planeIndex<6; ++planeIndex)
            clipIdentificationPlane[planeIndex]->Show = 0;
    }

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Update increment in header and refresh viewer\n");
    SUMA_UpdateViewerTitle(sv);         // Update increment in header
    SUMA_postRedisplay(w, NULL, NULL);  // Refresh window

    if (!clippingPlaneMode && SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### toggleClippingPlaneMode: sv->N_ColList = %d\n", sv->N_ColList);

    justEnteredClippingPlaneMode = 0;
    
    // Hide squares associated with inactive planes
    for (i=0; i<SUMAg_CF->N_ClipPlanes; ++i){
        if (!(active[i])) clipIdentificationPlane[i]->Show = 0;
    }

    return 1;
}

Boolean determineAdditionalRotationsFromRequiredAndExistingRotations(float theta, float phi,
    int planeIndex, float *deltaTheta, float *deltaPhi){

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Determine additional rotations based on plane index\n");

    // Determine additional rotations based on plane index
    switch (planeIndex){
    case 0:
        *deltaTheta = theta;
        *deltaPhi = phi;
        break;
    case 1:
        *deltaTheta = theta - 90.0;
        *deltaPhi = phi;
        break;
    case 2:
        *deltaTheta = theta;
        *deltaPhi = phi - 90.0;
        break;
    case 3:
        *deltaTheta = theta - 180.0;
        *deltaPhi = phi;
        break;
    case 4:
        *deltaTheta = theta - 270.0;
        *deltaPhi = phi;
        break;
    case 5:
        *deltaTheta = theta - 180.0;
        *deltaPhi = phi - 270.0;
        break;
    }

    return 1;
}

Boolean determineRotationAnglesFromEquation(float *equation, float *theta, float *phi){
    static float rad2degrees=180.0/M_PI, degrees2rad=M_PI/180;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1) fprintf(stderr, "### Determine rotation angles from equation\n");

    if (equation[2] == 1.0){
        *theta = 0.0;
        *phi = 0.0;
    } else if (equation[2] == -1.0){
        *theta = 180.0;
        *phi = 0.0;
    } else if (fabs(equation[2]) < 0.000001f){
        if (fabs(equation[0]) < 0.000001f){
            *theta = 90.0;
            *phi = 0.0;
        } else if (fabs(equation[1]) < 0.000001f) {
            *theta = 0.0;
            *phi = 90.0;
        } else {
            *theta = atan(equation[1]/equation[0])*rad2degrees;
            *phi = atan(equation[0]/equation[1])*rad2degrees;
        }
    } else {
        *theta = atan(equation[1]/equation[2])*rad2degrees;
        *phi = atan(equation[0]/equation[2])*rad2degrees;

        if (equation[2] < 0){
            *theta = 180.0 - *theta;
            *phi = 180.0 - *phi;
        }
    }

    return 1;
}

Boolean determineDeltaDFromExistingDAndRequiredD(float requiredD, int planeIndex, float *deltaD){

    *deltaD = requiredD - SUMAg_CF->ClipPlanes[4*planeIndex + 3];

    return 1;
}

Boolean applyEquationParametersToClippingPlane(int planeIndex, float *theta, float *phi, float *offset){
    float deltaTheta, deltaPhi, deltaD;
    int     i;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1) fprintf(stderr, "### Apply equation parameters to clipping plane\n");

    // Determine additional rotations from required and existing rotations
    determineAdditionalRotationsFromRequiredAndExistingRotations(theta[planeIndex], phi[planeIndex],
        planeIndex, &deltaTheta, &deltaPhi);

    // Determine delta D from existing D and required D
    determineDeltaDFromExistingDAndRequiredD(offset[planeIndex], planeIndex, &deltaD);

    // Apply rotations and delta Ds
    clipPlaneTransform(deltaTheta, deltaPhi, deltaD, 0, planeIndex, 0, 0);

    return 1;
}

Boolean applyEquationToClippingPlane(float *equation, int planeIndex){
    float theta, phi, deltaTheta, deltaPhi, deltaD;
    int     i;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1) fprintf(stderr, "### Apply equation to clipping plane %d\n", planeIndex+1);

    // Determine rotation angles from equation
    determineRotationAnglesFromEquation(equation, &theta, &phi);

    // Determine additional rotations from required and existing rotations
    determineAdditionalRotationsFromRequiredAndExistingRotations(theta, phi,
        planeIndex, &deltaTheta, &deltaPhi);

    // Determine delta D from existing D and required D
    determineDeltaDFromExistingDAndRequiredD(equation[3], planeIndex, &deltaD);

    // Apply rotations and delta Ds
    clipPlaneTransform(deltaTheta, deltaPhi, deltaD, 0, planeIndex, 0, 0);

    return 1;
}

Boolean loadSavedClippingPlanes(char *clippingPlaneFile, int *locallySelectedPlane){
    int feyl, planeIndex, i;
    Boolean isActive;
    float   equation[4], floatBuf[4];
    char    attribute[32];
    NI_stream nstdin;
    NI_element *nel = NULL;
    char *strbuf;
    float   theta[SUMA_MAX_N_CLIP_PLANES], phi[SUMA_MAX_N_CLIP_PLANES], offset[SUMA_MAX_N_CLIP_PLANES];

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1) fprintf(stderr, "### Load saved clipping planes\n");

    // Make sure correct form of filename supplied
    if (!clippingPlaneFile){
        fprintf(stderr, "Clipping plane file not supplied.\n");
        return 0;
    }
    if (!strstr(clippingPlaneFile, ".niml.vvs")){
        fprintf(stderr, "Invalid clipping plane file name.\n");
        return 0;
    }

    // Allocate memory to filename buffer
    if (!(strbuf=(char *)malloc((strlen(clippingPlaneFile) + 8)*sizeof(char)))){
        fprintf(stderr, "Error allocating memory to clipping plane file string\n");
        return 0;
    }


    // Open NIML stream
    sprintf(strbuf, "file:%s", clippingPlaneFile);
    if (!(nstdin = NI_stream_open( strbuf,"r"))){
        perror("Error opening clipping plane file.");
        free(strbuf);
        return 0;
    }
    free(strbuf);

    // Read NIML element
    if (!(nel = NI_read_element (nstdin, 1))) {
        perror("Failed to read nel.");
        return 0;
    }

    // Read NIML entries for clipping planes
    SUMA_S2FV_ATTR(nel, "sel_plane_num", floatBuf, 1, feyl);
      if (!feyl) {
        *locallySelectedPlane = (int)(floatBuf[0]+0.5);
        clipPlaneTransform(0,0,0,0,*locallySelectedPlane, 0, 0);
      }
    SUMA_S2FV_ATTR(nel, "tilt_inc", floatBuf, 1, feyl);
      if (!feyl) {
         tiltInc = floatBuf[0];
      }
    SUMA_S2FV_ATTR(nel, "scroll_inc", floatBuf, 1, feyl);
      if (!feyl) {
         scrollInc = floatBuf[0];
      }
    for (planeIndex=1; planeIndex<=6; ++planeIndex){
        sprintf(attribute, "plane_%d_act", planeIndex);
        SUMA_S2FV_ATTR(nel, attribute, floatBuf, 1, feyl);
          if (!feyl) {
            isActive = (int)(floatBuf[0]+0.5);
             previouslyActive[planeIndex-1] = isActive;
          }
    }

    // Apply equation parameters to active clipping planes
    getClippingEquationParameters(nel, "x_axis_rotations", theta);
    getClippingEquationParameters(nel, "y_axis_rotations", phi);
    getClippingEquationParameters(nel, "normal_offsets", offset);

    for (planeIndex=0; planeIndex<6; ++planeIndex)
        if (previouslyActive[planeIndex]){
            applyEquationParametersToClippingPlane(planeIndex, theta, phi, offset);
    }

    NI_stream_close(nstdin);
    NI_free_element(nel); nel = NULL;

    return 1;
}

Boolean getClippingEquationParameters(NI_element *nel, char *attribute, float *parameters){
    char *strbuffer;
    int feyl, i;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Get clipping plane parameters\n");

    SUMA_getStringFromNiml(nel, attribute, strbuffer, 1, feyl);
      if (!feyl) {
        parameters[0] = atof(strtok(strbuffer, ","));
        for (i=1; i<SUMA_MAX_N_CLIP_PLANES; ++i){
            parameters[i] = atof(strtok(NULL, ","));
        }

        return 1;
      }

      return 0;
}

Boolean getEquationForClippingPlane(NI_element *nel, char attribute[32], float equation[4]){
    char *strbuf, *eqnBuffer;
    int feyl, i;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Get get equation for clipping plane\n");

    SUMA_getStringFromNiml(nel, attribute, eqnBuffer, 1, feyl);
      if (!feyl) {
        equation[0] = atof(strtok(eqnBuffer, "+"));
        for (i=1; i<=3; ++i){
            equation[i] = atof(strtok(NULL, "+"));
        }

        return 1;
      }

      return 0;
}

// #include "GL/glcorearb.h"
/* GL/glcorearb.h is restricted to newer style functionality,
 * and we are dependend on the old.  Hopefully it is not needed.
 *                                           [23 Jun 2021 rickr] */
/* #include "GL/glcorearb.h"                                     */

int colorPlanes(SUMA_SurfaceViewer *sv, SUMA_SurfaceObject *SO,
                     SUMA_PICK_RESULT **PRi)
{
   static char FuncName[]={"colorPlanes"};
   SUMA_ALL_DO *ado=NULL;
   SUMA_PICK_RESULT *PR;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Process color planes\n");

   if (!sv || !SO || !PRi || !*PRi) { SUMA_S_Err("Niente"); SUMA_RETURN(-1); }

   // Mark intersection Facsets
   if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
    fprintf(stderr, "### Mark intersection Facsets\n");
   ado = (SUMA_ALL_DO *)SO;

   PR = *PRi;   // Keep local copy
   // Store the PR in ado, hide it from return potential
   if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Store the PR in ado, hide it from return potential\n");
   (*PRi)->ado_idcode_str = NULL;
   (*PRi)->dset_idcode_str = NULL;
   (*PRi)->primitive = NULL;
   (*PRi)->evr = NULL;

   if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### SUMA_ADO_SSaux\n");
   SUMA_SURF_SAUX *Saux = SUMA_ADO_SSaux(ado);

   // This part is necessary to prevent the program from crashing in some instances,
   //   apparently when volumes, rather than surfaces, are used
   if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Try to prevent program crashing with volumes\n");
   Saux->PR->primitive = NULL;
   Saux->PR->ado_idcode_str = NULL;
   Saux->PR->dset_idcode_str = NULL;
   Saux->PR->evr = NULL;

   if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Store pick result\n");
   SUMA_ADO_StorePickResult(ado, PRi);

   if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Update viewer title\n");
   sv->Focus_DO_ID = ADO_iDO(ado);
   SUMA_UpdateViewerTitle(sv);

   SUMA_LH("Returning");
   SUMA_RETURN (1); /* OK */
}/* determine intersection */

Boolean activeClippingPlanes(){
    int i;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Activate clipping planes\n");

    for (i=0; i<SUMAg_CF->N_ClipPlanes; ++i)
        if (active[i]) return True;

    return False;
}

void getObjectMinMaxForAxes(float objectMinMax[][2]){
    int allowableMin = -SUMA_TESSCON_DIFF_FLAG/2;
    int allowableMax = SUMA_TESSCON_DIFF_FLAG/2;
    int i, dov_ID;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Get object min/max for axes\n");

    // Itialise
    for (i=0; i<3; ++i){
        objectMinMax[i][0] = 10000.0;
        objectMinMax[i][1] = -10000.0;
    }

    // Update min and max for each axis
    for (dov_ID=0; dov_ID<SUMAg_N_DOv; ++dov_ID){
        SUMA_SurfaceObject *soOld = (SUMA_SurfaceObject *)SUMAg_DOv[dov_ID].OP;

        if (soOld->Show==1) for (i=0; i<3; ++i){
            if (soOld->MaxDims[i]<= allowableMax) objectMinMax[i][1] = MAX(objectMinMax[i][1], soOld->MaxDims[i]);
            if (soOld->MinDims[i] >= allowableMin) objectMinMax[i][0] = MIN(objectMinMax[i][0], soOld->MinDims[i]);
        }
    }

    // Ensure some gap between planes
    for (i=0; i<3; ++i){
        if (objectMinMax[i][0]>=objectMinMax[i][1]){
            objectMinMax[i][0] = -100;
            objectMinMax[i][1] = 100;
        }
    }
}

void dimensionsInscribeThoseOfPreviousSurfaceObjects(SUMA_SurfaceObject *SO){
    int allowableMin = -SUMA_TESSCON_DIFF_FLAG/2;
    int allowableMax = SUMA_TESSCON_DIFF_FLAG/2;
    int i, dov_ID;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Dimensions inscribe those of previous surface objects\n");

    // Initialize
    for (i=0; i<3; ++i){
        SO->MaxDims[i] = -1000.0;
        SO->MinDims[i] = 1000.0;
    }
    SO->aMaxDims = -1000.0;
    SO->aMinDims = 1000.0;

    // Update min and max for each axis
    for (dov_ID=0; dov_ID<SUMAg_N_DOv; ++dov_ID){
        SUMA_SurfaceObject *soOld = (SUMA_SurfaceObject *)SUMAg_DOv[dov_ID].OP;
        for (i=0; i<3; ++i){
            if (soOld->MaxDims[i]<= allowableMax)SO->MaxDims[i] = MAX(SO->MaxDims[i], soOld->MaxDims[i]);
            if (soOld->MinDims[i]>= allowableMin)SO->MinDims[i] = MIN(SO->MinDims[i], soOld->MinDims[i]);
        }
    }

    // Update overall min and max
    for (i=0; i<3; ++i){
        if (SO->MaxDims[i]<SO->MinDims[i]){
            SO->MaxDims[i] = 100.0;
            SO->MinDims[i] = -100.0;
        }
        SO->aMaxDims = MAX(SO->MaxDims[i], SO->aMaxDims);
        SO->aMinDims = MIN(SO->MinDims[i], SO->aMinDims);
    }
    if (SO->aMaxDims - SO->aMinDims > SUMA_TESSCON_DIFF_FLAG){
        SO->aMaxDims = MIN(100.0, SO->aMaxDims);
        SO->aMinDims = MAX(-100.0, SO->aMinDims);
    }
}

void determineCornersOfSquare(SUMA_SurfaceObject *SO){

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Determine corners of square\n");

    /*!< The maximum along each of the XYZ dimensions */
    SO->MaxDims[0] = clippingPlaneAxisRanges[0][1];
    SO->MaxDims[1] = clippingPlaneAxisRanges[1][1];
    SO->MaxDims[2] = clippingPlaneAxisRanges[2][1];
    SO->MinDims[0] = clippingPlaneAxisRanges[0][0];
    SO->MinDims[1] = clippingPlaneAxisRanges[1][0];
    SO->MinDims[2] = clippingPlaneAxisRanges[2][0];
    SO->aMinDims = (SO->MinDims[0]<SO->MinDims[1])? MIN(SO->MinDims[0], SO->MinDims[2]) :
        MIN(SO->MinDims[1], SO->MinDims[2]);
    SO->aMaxDims = (SO->MaxDims[0]>SO->MaxDims[1])? MAX(SO->MaxDims[0], SO->MaxDims[2]) :
        MIN(SO->MaxDims[1], SO->MaxDims[2]);
}

#define DARK_COLOR 0.4
#define DARK_BACKGROUND 0.1

void makeCommonNodesOfRectangleDarkRed(SUMA_SurfaceObject *SO){
    int i;
    for (i=0; i<16; ++i) SO->Overlays[0]->ColVec[i] = DARK_BACKGROUND;
    SO->Overlays[0]->ColVec[0] = DARK_COLOR;
    SO->Overlays[0]->ColVec[3] = DARK_COLOR;
    SO->Overlays[0]->ColVec[6] = DARK_COLOR;
    SO->Overlays[0]->ColVec[9] = DARK_COLOR;
}

void makeCommonNodesOfRectangleDarkGreen(SUMA_SurfaceObject *SO){
    int i;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1){
        fprintf(stderr, "### makeCommonNodesOfRectangleDarkGreen: SO = %p\n", SO);
        if (SO) fprintf(stderr, "### makeCommonNodesOfRectangleDarkGreen: SO->idcode_str = %s\n", SO->idcode_str);
    }

    for (i=0; i<16; ++i) SO->Overlays[0]->ColVec[i] = DARK_BACKGROUND;

    SO->Overlays[0]->ColVec[1] = DARK_COLOR;
    SO->Overlays[0]->ColVec[4] = DARK_COLOR;
    SO->Overlays[0]->ColVec[7] = DARK_COLOR;
    SO->Overlays[0]->ColVec[10] = DARK_COLOR;
}

void makeCommonNodesOfRectangleDarkBlue(SUMA_SurfaceObject *SO){
    int i;
    for (i=0; i<16; ++i) SO->Overlays[0]->ColVec[i] = DARK_BACKGROUND;

    SO->Overlays[0]->ColVec[2] = DARK_COLOR;
    SO->Overlays[0]->ColVec[5] = DARK_COLOR;
    SO->Overlays[0]->ColVec[8] = DARK_COLOR;
    SO->Overlays[0]->ColVec[11] = DARK_COLOR;
}

void makeCommonNodesOfRectangleDarkCyan(SUMA_SurfaceObject *SO){
    int i;
    for (i=0; i<16; ++i) SO->Overlays[0]->ColVec[i] = DARK_BACKGROUND;

    SO->Overlays[0]->ColVec[1] = SO->Overlays[0]->ColVec[2] = DARK_COLOR;
    SO->Overlays[0]->ColVec[7] = SO->Overlays[0]->ColVec[8] = DARK_COLOR;
    SO->Overlays[0]->ColVec[4] = SO->Overlays[0]->ColVec[5] = DARK_COLOR;
    SO->Overlays[0]->ColVec[10] = SO->Overlays[0]->ColVec[11] = DARK_COLOR;
}

void makeCommonNodesOfRectangleDarkMagenta(SUMA_SurfaceObject *SO){
    int i;
    for (i=0; i<16; ++i) SO->Overlays[0]->ColVec[i] = DARK_BACKGROUND;

    SO->Overlays[0]->ColVec[0] = SO->Overlays[0]->ColVec[2] = DARK_COLOR;
    SO->Overlays[0]->ColVec[6] = SO->Overlays[0]->ColVec[8] = DARK_COLOR;
    SO->Overlays[0]->ColVec[3] = SO->Overlays[0]->ColVec[5] = DARK_COLOR;
    SO->Overlays[0]->ColVec[9] = SO->Overlays[0]->ColVec[11] = DARK_COLOR;
}

void makeCommonNodesOfRectangleDarkYellow(SUMA_SurfaceObject *SO){
    int i;

    for (i=0; i<16; ++i) SO->Overlays[0]->ColVec[i] = DARK_BACKGROUND;

    SO->Overlays[0]->ColVec[0] = SO->Overlays[0]->ColVec[1] = DARK_COLOR;
    SO->Overlays[0]->ColVec[6] = SO->Overlays[0]->ColVec[7] = DARK_COLOR;
    SO->Overlays[0]->ColVec[3] = SO->Overlays[0]->ColVec[4] = DARK_COLOR;
    SO->Overlays[0]->ColVec[9] = SO->Overlays[0]->ColVec[10] = DARK_COLOR;
}


void makeCommonNodesOfRectangleRed(SUMA_SurfaceObject *SO){
    int i;

    // fprintf(stderr, "makeCommonNodesOfRectangleRed\n");

    for (i=0; i<16; ++i) SO->Overlays[0]->ColVec[0] = 0.0;

    SO->Overlays[0]->ColVec[0] = 1.0;
    SO->Overlays[0]->ColVec[3] = 1.0;
    SO->Overlays[0]->ColVec[6] = 1.0;
    SO->Overlays[0]->ColVec[9] = 1.0;
}

void makeCommonNodesOfRectangleGreen(SUMA_SurfaceObject *SO){
    int i;

    for (i=0; i<16; ++i) SO->Overlays[0]->ColVec[0] = 0.0;

    SO->Overlays[0]->ColVec[1] = 1.0;
    SO->Overlays[0]->ColVec[4] = 1.0;
    SO->Overlays[0]->ColVec[7] = 1.0;
    SO->Overlays[0]->ColVec[10] = 1.0;
}

void makeCommonNodesOfRectangleBlue(SUMA_SurfaceObject *SO){
    int i;
    // for (i=0; i<16; ++i) SO->Overlays[0]->ColVec[0] = 0.5;
    for (i=0; i<16; ++i) SO->Overlays[0]->ColVec[0] = 0.0;

    SO->Overlays[0]->ColVec[2] = 1.0;
    SO->Overlays[0]->ColVec[5] = 1.0;
    SO->Overlays[0]->ColVec[8] = 1.0;
    SO->Overlays[0]->ColVec[11] = 1.0;
}

void makeCommonNodesOfRectangleCyan(SUMA_SurfaceObject *SO){
    int i;

    for (i=0; i<16; ++i) SO->Overlays[0]->ColVec[0] = 0.0;

    SO->Overlays[0]->ColVec[1] = SO->Overlays[0]->ColVec[2] = 1.0;
    SO->Overlays[0]->ColVec[7] = SO->Overlays[0]->ColVec[8] = 1.0;
    SO->Overlays[0]->ColVec[4] = SO->Overlays[0]->ColVec[5] = 1.0;
    SO->Overlays[0]->ColVec[10] = SO->Overlays[0]->ColVec[11] = 1.0;
}

void makeCommonNodesOfRectangleMagenta(SUMA_SurfaceObject *SO){
    int i;

    for (i=0; i<16; ++i) SO->Overlays[0]->ColVec[0] = 0.0;

    SO->Overlays[0]->ColVec[0] = SO->Overlays[0]->ColVec[2] = 1.0;
    SO->Overlays[0]->ColVec[6] = SO->Overlays[0]->ColVec[8] = 1.0;
    SO->Overlays[0]->ColVec[3] = SO->Overlays[0]->ColVec[5] = 1.0;
    SO->Overlays[0]->ColVec[9] = SO->Overlays[0]->ColVec[11] = 1.0;
}

void makeCommonNodesOfRectangleYellow(SUMA_SurfaceObject *SO){
    int i;

    for (i=0; i<16; ++i) SO->Overlays[0]->ColVec[0] = 0.0;

    SO->Overlays[0]->ColVec[0] = SO->Overlays[0]->ColVec[1] = 1.0;
    SO->Overlays[0]->ColVec[6] = SO->Overlays[0]->ColVec[7] = 1.0;
    SO->Overlays[0]->ColVec[3] = SO->Overlays[0]->ColVec[4] = 1.0;
    SO->Overlays[0]->ColVec[9] = SO->Overlays[0]->ColVec[10] = 1.0;
}

SUMA_SurfaceObject *makeAxisPlaneFromNodeAndFaceSetList(SUMA_SurfaceViewer *sv,
    SUMA_FreeSurfer_struct FS){
    int i;

    // Set global variables
    char *FuncName = "makeAxisPlaneFromNodeAndFaceSetList";
    SUMA_DO *dov = SUMAg_DOv;
    int N_dov = SUMAg_N_DOv-1;
    SUMA_ALL_DO *ado;
    ado = SUMA_SV_Focus_ADO(sv);
    SUMA_OVERLAYS *NewColPlane=NULL;
    static int squareIndex = 0;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Make axis plane from node and face set list\n");

    SUMA_SurfaceObject *SO = (SUMA_SurfaceObject *)calloc(1, sizeof(SUMA_SurfaceObject));
    SO->N_Node = FS.N_Node;
    // Save the pointers to NodeList and FaceSetList and
    //  clear what is left of FS structure at the end
    SO->NodeList = FS.NodeList;
    SO->FaceSetList = FS.FaceSetList;

    SO->N_FaceSet = FS.N_FaceSet;
    SO->FaceSetDim = 3; //This must also be automated

    SO->SUMA_VolPar_Aligned = NOPE;
    SO->normdir = 1; // normals point out

    if (SO->isSphere == SUMA_GEOM_NOT_SET) {
        SUMA_SetSphereParams(SO, -0.1);
    }  // sets the spheriosity parameters

    // Miscelaneous fields
    if (SO->isSphere == SUMA_GEOM_NOT_SET) {
        SUMA_SetSphereParams(SO, -0.1);   /* sets the spheriosity parameters */
    }
    SO->do_type = SO_type;
    SO->SurfCont = NULL;

    // dimensionsInscribeThoseOfPreviousSurfaceObjects(SO);
    determineCornersOfSquare(SO);
    SO->aMaxDims = SO->aMinDims = 0;

    // SO->EmbedDim = 2;
    SO->Side = SUMA_GuessSide (SO);
    SO->AnatCorrect = NOPE;

    SO->FileType = SUMA_FREE_SURFER;
    SO->Name.Path = NULL;
    SO->Name.FileName = NULL;
    SO->idcode_str = "axisObject";

    SUMA_AutoLoad_SO_Dsets(SO);

    /* set its MappingRef id to NULL if none is specified */
    // make sure that specified Mapping ref had been loaded
    SO->LocalDomainParentID = (char *)calloc( strlen(SO->idcode_str)+1,
                        sizeof(char));
    sprintf(SO->idcode_str, "%s", SO->idcode_str);
    if (SO->LocalDomainParentID == NULL) {
        fprintf(stderr,
        "Error SUMA_display_one: Failed to allocate for "
        "SO->LocalDomainParentID. \n"
        "That is pretty bad.\n");
        return NULL;
    }

    char sid[100];
    SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt = (SUMA_GENERIC_PROG_OPTIONS_STRUCT *)
        SUMA_calloc(1,sizeof(SUMA_GENERIC_PROG_OPTIONS_STRUCT));
    SO->Group = SUMA_copy_string(SUMA_DEF_TOY_GROUP_NAME);
    /* change this in sync with string in macro
    SUMA_BLANK_NEW_SPEC_SURF*/
    sprintf(sid, "%s_%d", SUMA_DEF_STATE_NAME, Opt->obj_type);
    SO->State = SUMA_copy_string(sid);
    sprintf(sid, "clippingPlaneIdentificationSquare_%d", SUMAg_CF->N_ClipPlanes-1);
    SO->Label = SUMA_copy_string(sid);
    SO->EmbedDim = 3;
    SO->AnatCorrect = NOPE;

    // make this surface friendly for suma
    if (!SUMA_PrepSO_GeomProp_GL(SO)) {
       SUMA_S_Err("Failed in SUMA_PrepSO_GeomProp_GL");
    }

    /* Add this surface to SUMA's displayable objects */
    if (SO->Overlays && !SUMA_PrepAddmappableSO(SO, SUMAg_DOv, &(SUMAg_N_DOv), 0, SUMAg_CF->DsetList)) {
       SUMA_S_Err("Failed to add mappable SOs ");
    }

    if (!SO->Group || !SO->State || !SO->Label) {
        fprintf(SUMA_STDERR,"Error %s: Error allocating lameness.\n", FuncName);
        return NULL;
    }

    /* Non Mappable surfaces */
    /* if the surface is loaded OK,
    and it has not been loaded previously, register it */
    SO->MeshAxis = NULL;
    SO->NodeDim = 3;

    /* Change the defaults of Mesh axis to fit standard  */
    SUMA_MeshAxisStandard (SO->MeshAxis, (SUMA_ALL_DO *)SO);

    /* toggle the viewing for the cartesian axes */
    // DEBUG SO->ShowMeshAxis = clipPlaneIdentificationMode;

    /* Create a Mesh Axis for the surface */
    SO->MeshAxis = SUMA_Alloc_Axis ("Surface Mesh Axis", AO_type);
        if (SO->MeshAxis == NULL) {
        fprintf( SUMA_STDERR,
        "Error %s: Error Allocating axis\n", FuncName);
        return NULL;
    }

    // Store it into dov
    SO->patchNodeMask = NULL;
    sprintf(SO->Group, "DefGroup");
    SO->SphereRadius = -1.0;
    SO->SphereCenter[0] = -1.0;
    SO->SphereCenter[1] = -1.0;
    SO->SphereCenter[2] = -1.0;
    SO->LocalDomainParent = "SAME";
    if (!SUMA_AddDO(dov, &SUMAg_N_DOv, (void *)SO,  SO_type, SUMA_WORLD)) {
        fprintf(SUMA_STDERR,"Error %s: Error Adding DO\n", FuncName);
        return NULL;
    }

    N_dov = SUMAg_N_DOv-1;

     // register DO with viewer
    if (!SUMA_RegisterDO(N_dov, sv)) {
       fprintf(SUMA_STDERR,
                "Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
       return NULL;
    }

    // SO->LocalDomainParentID = ((SUMA_SurfaceObject *)(dov[N_dov-1].OP))->LocalDomainParentID;
    SO->LocalDomainParentID = NULL;
    SO->Saux = SUMA_ADO_Saux(ado);

   SO->Show = 1;    // *** The axis is not shown if this value is zero
   SO->NodeList_swp = NULL;

   // Colors
   SO->N_Overlays = 0;
   // SO->Overlays = (SUMA_OVERLAYS **)calloc(1, sizeof(SUMA_OVERLAYS *));
   // Note that ado remains the same and second argument may not be >0
   // SO->Overlays[0] = SUMA_ADO_Overlay(ado, 0);
   // SUMA_OVERLAYS *tempOverlay = SUMA_ADO_Overlay(ado, 0);
   // SO->Overlays[0] = (SUMA_OVERLAYS *)calloc(1, sizeof(SUMA_OVERLAYS));
   // memcpy((void *)(SO->Overlays[0]),(void *)(tempOverlay),sizeof(SUMA_OVERLAYS));
   // SO->Overlays[0]->ColVec = (float *)calloc(16, sizeof(float));
/*
    if (!(SO->Overlays)){
        SUMA_S_Err("NULL Overlays pointer.");
        SO->N_Overlays = 0;
    }
    if ((intptr_t)(SO->Overlays) == 1){
        SUMA_S_Err("Invalid Overlays pointer: 0x1.");
        SO->N_Overlays = 0;
    }
    SO->Overlays[0]->GlobalOpacity = 0.0;
*/
    // NBB: This block is vitally important in preventing the clipping plane surface from being gray
    //   for some surface objects.  Not common but it happens.
    {
        SO->SurfCont = SUMA_CreateSurfContStruct(SO->idcode_str, SO_type);
        SO->SurfCont->curColPlane = SUMA_ADO_CurColPlane(ado);

       // switch to the recently loaded  cmap
        SUMA_COLOR_MAP *Cmp = SUMA_FindNamedColMap ("ngray20");
        if (!SUMA_SwitchColPlaneCmap(ado, Cmp)) {
            fprintf(stderr, "Failed in SUMA_SwitchColPlaneCmap");
            return NULL;
        }

        SUMA_PICK_RESULT *PR = (SUMA_PICK_RESULT *)SUMA_calloc(1,sizeof(SUMA_PICK_RESULT));
        if (!colorPlanes(sv, SO,&PR)){
            fprintf(stderr, "ERROR: colorPlanes failed in makeAxisPlaneFromNodeAndFaceSetList\n");
            return NULL;
        }
    }

    // Reduce opacity of current surface object (clipping plane square)
    for (i=0; i<2; ++i){
       SUMA_Set_ADO_TransMode(SUMA_SV_Focus_ADO(sv), sv->TransMode,
                              SUMAg_CF->TransModeStep, 1);
       SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
    }

    axisObject = SO;

    return SO;
    }

SUMA_SurfaceObject *drawPlaneFromNodeAndFaceSetList(SUMA_SurfaceViewer *sv,
    SUMA_FreeSurfer_struct FS, int planeIndex){
    int i;
    // Set global variables
    char *FuncName = "drawPlaneFromNodeAndFaceSetList";
    SUMA_DO *dov = SUMAg_DOv;
    int N_dov = SUMAg_N_DOv-1;
    SUMA_ALL_DO *ado;
    ado = SUMA_SV_Focus_ADO(sv);
    SUMA_OVERLAYS *NewColPlane=NULL;
    static int squareIndex = 0;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Draw plane node and face set list\n");

    SUMA_SurfaceObject *SO = (SUMA_SurfaceObject *)calloc(1, sizeof(SUMA_SurfaceObject));
    SO->N_Node = FS.N_Node;
    // Save the pointers to NodeList and FaceSetList and
    //  clear what is left of FS structure at the end
    SO->NodeList = FS.NodeList;
    SO->FaceSetList = FS.FaceSetList;

    SO->N_FaceSet = FS.N_FaceSet;
    SO->FaceSetDim = 3; //This must also be automated

    SO->SUMA_VolPar_Aligned = NOPE;
    SO->normdir = 1; // normals point out

    if (SO->isSphere == SUMA_GEOM_NOT_SET) {
        SUMA_SetSphereParams(SO, -0.1);
    }  // sets the spheriosity parameters

    // Miscelaneous fields
    if (SO->isSphere == SUMA_GEOM_NOT_SET) {
        SUMA_SetSphereParams(SO, -0.1);   /* sets the spheriosity parameters */
    }
    SO->do_type = SO_type;
    SO->SurfCont = NULL;

    // dimensionsInscribeThoseOfPreviousSurfaceObjects(SO);
    determineCornersOfSquare(SO);

    // SO->EmbedDim = 2;
    SO->Side = SUMA_GuessSide (SO);
    SO->AnatCorrect = NOPE;

    SO->FileType = SUMA_FREE_SURFER;
    SO->Name.Path = NULL;
    SO->Name.FileName = NULL;
    switch (planeIndex){
        case 0: SO->idcode_str = "ClipSquare1"; break;
        case 1: SO->idcode_str = "ClipSquare2"; break;
        case 2: SO->idcode_str = "ClipSquare3"; break;
        case 3: SO->idcode_str = "ClipSquare4"; break;
        case 4: SO->idcode_str = "ClipSquare5"; break;
        case 5: SO->idcode_str = "ClipSquare6"; break;
    }

    SUMA_AutoLoad_SO_Dsets(SO);

    /* set its MappingRef id to NULL if none is specified */
    // make sure that specified Mapping ref had been loaded
    SO->LocalDomainParentID = (char *)calloc( strlen(SO->idcode_str)+1,
                        sizeof(char));
    sprintf(SO->idcode_str, "%s", SO->idcode_str);
    if (SO->LocalDomainParentID == NULL) {
        fprintf(stderr,
        "Error SUMA_display_one: Failed to allocate for "
        "SO->LocalDomainParentID. \n"
        "That is pretty bad.\n");
        return NULL;
    }

    char sid[100];
    SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt = (SUMA_GENERIC_PROG_OPTIONS_STRUCT *)
        SUMA_calloc(1,sizeof(SUMA_GENERIC_PROG_OPTIONS_STRUCT));
    SO->Group = SUMA_copy_string(SUMA_DEF_TOY_GROUP_NAME);
    /* change this in sync with string in macro
    SUMA_BLANK_NEW_SPEC_SURF*/
    sprintf(sid, "%s_%d", SUMA_DEF_STATE_NAME, Opt->obj_type);
    SO->State = SUMA_copy_string(sid);
    sprintf(sid, "clippingPlaneIdentificationSquare_%d", SUMAg_CF->N_ClipPlanes-1);
    SO->Label = SUMA_copy_string(sid);
    SO->EmbedDim = 3;
    SO->AnatCorrect = NOPE;

    // make this surface friendly for suma
    if (!SUMA_PrepSO_GeomProp_GL(SO)) {
       SUMA_S_Err("Failed in SUMA_PrepSO_GeomProp_GL");
    }

    /* Add this surface to SUMA's displayable objects */
    if (SO->Overlays && !SUMA_PrepAddmappableSO(SO, SUMAg_DOv, &(SUMAg_N_DOv), 0, SUMAg_CF->DsetList)) {
       SUMA_S_Err("Failed to add mappable SOs ");
    }

    if (!SO->Group || !SO->State || !SO->Label) {
        fprintf(SUMA_STDERR,"Error %s: Error allocating lameness.\n", FuncName);
        return NULL;
    }

    /* Non Mappable surfaces */
    /* if the surface is loaded OK,
    and it has not been loaded previously, register it */
    SO->MeshAxis = NULL;
    SO->NodeDim = 3;

    /* Change the defaults of Mesh axis to fit standard  */
    SUMA_MeshAxisStandard (SO->MeshAxis, (SUMA_ALL_DO *)SO);

    /* toggle the viewing for the cartesian axes */
    // DEBUG SO->ShowMeshAxis = clipPlaneIdentificationMode;

    /* Create a Mesh Axis for the surface */
    SO->MeshAxis = SUMA_Alloc_Axis ("Surface Mesh Axis", AO_type);
        if (SO->MeshAxis == NULL) {
        fprintf( SUMA_STDERR,
        "Error %s: Error Allocating axis\n", FuncName);
        return NULL;
    }

    // Store it into dov
    SO->patchNodeMask = NULL;
    sprintf(SO->Group, "DefGroup");
    SO->SphereRadius = -1.0;
    SO->SphereCenter[0] = -1.0;
    SO->SphereCenter[1] = -1.0;
    SO->SphereCenter[2] = -1.0;
    SO->LocalDomainParent = "SAME";
    if (!SUMA_AddDO(dov, &SUMAg_N_DOv, (void *)SO,  SO_type, SUMA_WORLD)) {
        fprintf(SUMA_STDERR,"Error %s: Error Adding DO\n", FuncName);
        return NULL;
    }

    N_dov = SUMAg_N_DOv-1;

     // register DO with viewer
    if (!SUMA_RegisterDO(N_dov, sv)) {
       fprintf(SUMA_STDERR,
                "Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
       return NULL;
    }

    // SO->LocalDomainParentID = ((SUMA_SurfaceObject *)(dov[N_dov-1].OP))->LocalDomainParentID;
    SO->LocalDomainParentID = NULL;
    SO->Saux = SUMA_ADO_Saux(ado);

   SO->Show = clipPlaneIdentificationMode;    // *** Most important part.  The plane is not shown if this value is zero
   SO->NodeList_swp = NULL;

   // Colors
   SO->N_Overlays = 1;
   SO->Overlays = (SUMA_OVERLAYS **)calloc(1, sizeof(SUMA_OVERLAYS *));
   // Note that ado remains the same and second argument may not be >0
   // SO->Overlays[0] = SUMA_ADO_Overlay(ado, 0);
   SUMA_OVERLAYS *tempOverlay = SUMA_ADO_Overlay(ado, 0);
   SO->Overlays[0] = (SUMA_OVERLAYS *)calloc(1, sizeof(SUMA_OVERLAYS));
   memcpy((void *)(SO->Overlays[0]),(void *)(tempOverlay),sizeof(SUMA_OVERLAYS));
   SO->Overlays[0]->ColVec = (float *)calloc(16, sizeof(float));

    if (!(SO->Overlays)){
        SUMA_S_Err("NULL Overlays pointer.");
        SO->N_Overlays = 0;
    }
    if ((intptr_t)(SO->Overlays) == 1){
        SUMA_S_Err("Invalid Overlays pointer: 0x1.");
        SO->N_Overlays = 0;
    }
    SO->Overlays[0]->GlobalOpacity = 1.0;
    // SO->Overlays[0]->GlobalOpacity = 0.7;

    // Make common nodes of rectangle the RGBCMY color for the particular plane
    switch(planeIndex){
        case 0: makeCommonNodesOfRectangleRed(SO); break;
        case 1: makeCommonNodesOfRectangleGreen(SO); break;
        case 2: makeCommonNodesOfRectangleBlue(SO); break;
        case 3: makeCommonNodesOfRectangleCyan(SO); break;
        case 4: makeCommonNodesOfRectangleMagenta(SO); break;
        case 5: makeCommonNodesOfRectangleYellow(SO); break;
    }

    // NBB: This block is vitally important in preventing the clipping plane surface from being gray
    //   for some surface objects.  Not common but it happens.
    {
        SO->SurfCont = SUMA_CreateSurfContStruct(SO->idcode_str, SO_type);
        SO->SurfCont->curColPlane = SUMA_ADO_CurColPlane(ado);

       // switch to the recently loaded  cmap
        SUMA_COLOR_MAP *Cmp = SUMA_FindNamedColMap ("ngray20");

        SUMA_PICK_RESULT *PR = (SUMA_PICK_RESULT *)SUMA_calloc(1,sizeof(SUMA_PICK_RESULT));
        if (!colorPlanes(sv, SO,&PR)) {
            fprintf(stderr, "ERROR: colorPlanes failed in drawPlaneFromNodeAndFaceSetList\n");
            return NULL;
        }
    }

    // Reduce opacity of current surface object (clipping plane square)
    for (i=0; i<2; ++i){
       SUMA_Set_ADO_TransMode(SUMA_SV_Focus_ADO(sv), sv->TransMode,
                              SUMAg_CF->TransModeStep, 1);
       SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
    }

    clippingPlaneIDDisplayableObjects[planeIndex] = SO;

    return SO;
}

void compareSurfaces(SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2){
    fprintf(stderr, "Beginning surface comparison\n");

    fprintf(stderr, "do_type (1) = %d\n", SO1->do_type);
    fprintf(stderr, "do_type (2) = %d\n", SO2->do_type);
    fprintf(stderr, "idcode_str (1) = %s\n", SO1->idcode_str);
    fprintf(stderr, "idcode_str (2) = %s\n", SO2->idcode_str);
    fprintf(stderr, "Label (1) = %s\n", SO1->Label);
    fprintf(stderr, "Label (2) = %s\n", SO2->Label);
    fprintf(stderr, "N_Node (1) = %d\n", SO1->N_Node);
    fprintf(stderr, "N_Node (2) = %d\n", SO2->N_Node);
    fprintf(stderr, "NodeDim (1) = %d\n", SO1->NodeDim);
    fprintf(stderr, "NodeDim (2) = %d\n", SO2->NodeDim);
    fprintf(stderr, "EmbedDim (1) = %d\n", SO1->EmbedDim);
    fprintf(stderr, "EmbedDim (2) = %d\n", SO2->EmbedDim);
    fprintf(stderr, "NodeList (1) = %p\n", SO1->NodeList);
    fprintf(stderr, "NodeList (2) = %p\n", SO2->NodeList);
    fprintf(stderr, "N_FaceSet (1) = %d\n", SO1->N_FaceSet);
    fprintf(stderr, "N_FaceSet (2) = %d\n", SO2->N_FaceSet);
    fprintf(stderr, "FaceSetDim (1) = %d\n", SO1->FaceSetDim);
    fprintf(stderr, "FaceSetDim (2) = %d\n", SO2->FaceSetDim);
    fprintf(stderr, "FaceSetList (1) = %p\n", SO1->FaceSetList);
    fprintf(stderr, "FaceSetList (2) = %p\n", SO2->FaceSetList);
    fprintf(stderr, "aSO (1) = %p\n", SO1->aSO);
    fprintf(stderr, "aSO (2) = %p\n", SO2->aSO);
    fprintf(stderr, "FileType (1) = %d\n", SO1->FileType);
    fprintf(stderr, "FileType (2) = %d\n", SO2->FileType);
    fprintf(stderr, "FileFormat (1) = %d\n", SO1->FileFormat);
    fprintf(stderr, "FileFormat (2) = %d\n", SO2->FileFormat);
    /*
    fprintf(stderr, "Name (1) = %s, %s\n", SO1->Name.Path, SO1->Name.FileName);
    fprintf(stderr, "Name (2) = %s, %s\n", SO2->Name.Path, SO2->Name.FileName);
    fprintf(stderr, "Name_coord (1) = %s, %s\n", SO1->Name_coord.Path, SO1->Name_coord.FileName);
    fprintf(stderr, "Name_coord (2) = %s, %s\n", SO2->Name_coord.Path, SO2->Name_coord.FileName);
    fprintf(stderr, "Name_topo (1) = %s, %s\n", SO1->Name_topo.Path, SO1->Name_topo.FileName);
    fprintf(stderr, "Name_topo (2) = %s, %s\n", SO2->Name_topo.Path, SO2->Name_topo.FileName);
    fprintf(stderr, "SpecFile (1) = %s, %s\n", SO1->SpecFile.Path, SO1->SpecFile.FileName);
    fprintf(stderr, "SpecFile (2) = %s, %s\n", SO2->SpecFile.Path, SO2->SpecFile.FileName);
    */
    fprintf(stderr, "parent_vol_idcode_str (1) = %s\n", SO1->parent_vol_idcode_str);
    fprintf(stderr, "parent_vol_idcode_str (2) = %s\n", SO2->parent_vol_idcode_str);
    fprintf(stderr, "facesetlist_idcode_str (1) = %s\n", SO1->facesetlist_idcode_str);
    fprintf(stderr, "facesetlist_idcode_str (2) = %s\n", SO2->facesetlist_idcode_str);
    fprintf(stderr, "nodelist_idcode_str (1) = %s\n", SO1->nodelist_idcode_str);
    fprintf(stderr, "nodelist_idcode_str (2) = %s\n", SO2->nodelist_idcode_str);
    fprintf(stderr, "facenormals_idcode_str (1) = %s\n", SO1->facenormals_idcode_str);
    fprintf(stderr, "facenormals_idcode_str (2) = %s\n", SO2->facenormals_idcode_str);
    fprintf(stderr, "polyarea_idcode_str (1) = %s\n", SO1->polyarea_idcode_str);
    fprintf(stderr, "polyarea_idcode_str (2) = %s\n", SO2->polyarea_idcode_str);
    fprintf(stderr, "Name_NodeParent (1) = %s\n", SO1->Name_NodeParent);
    fprintf(stderr, "Name_NodeParent (2) = %s\n", SO2->Name_NodeParent);
    fprintf(stderr, "Group_idcode_str (1) = %s\n", SO1->Group_idcode_str);
    fprintf(stderr, "Group_idcode_str (2) = %s\n", SO2->Group_idcode_str);
    fprintf(stderr, "Group (1) = %s\n", SO1->Group);
    fprintf(stderr, "Group (2) = %s\n", SO2->Group);
    fprintf(stderr, "State (1) = %s\n", SO1->State);
    fprintf(stderr, "State (2) = %s\n", SO2->State);
    fprintf(stderr, "Side (1) = %d\n", SO1->Side);
    fprintf(stderr, "Side (2) = %d\n", SO2->Side);
    fprintf(stderr, "isSphere (1) = %d\n", SO1->isSphere);
    fprintf(stderr, "isSphere (2) = %d\n", SO2->isSphere);
    fprintf(stderr, "SphereRadius (1) = %f\n", SO1->SphereRadius);
    fprintf(stderr, "SphereRadius (2) = %f\n", SO2->SphereRadius);
    fprintf(stderr, "SphereCenter (1) = %f, %f, %f\n", SO1->SphereCenter[0], SO1->SphereCenter[1], SO1->SphereCenter[2]);
    fprintf(stderr, "SphereCenter (2) = %f, %f, %f\n", SO2->SphereCenter[0], SO2->SphereCenter[1], SO2->SphereCenter[2]);
    fprintf(stderr, "AnatCorrect (1) = %d\n", SO1->AnatCorrect);
    fprintf(stderr, "AnatCorrect (2) = %d\n", SO2->AnatCorrect);
    fprintf(stderr, "DomainGrandParentID (1) = %s\n", SO1->DomainGrandParentID);
    fprintf(stderr, "DomainGrandParentID (2) = %s\n", SO2->DomainGrandParentID);
    fprintf(stderr, "OriginatorID (1) = %s\n", SO1->OriginatorID);
    fprintf(stderr, "OriginatorID (2) = %s\n", SO2->OriginatorID);
    fprintf(stderr, "OriginatorLabel (1) = %s\n", SO1->OriginatorLabel);
    fprintf(stderr, "OriginatorLabel (2) = %s\n", SO2->OriginatorLabel);
    fprintf(stderr, "LocalCurvatureParent (1) = %s\n", SO1->LocalCurvatureParent);
    fprintf(stderr, "LocalCurvatureParent (2) = %s\n", SO2->LocalCurvatureParent);
    fprintf(stderr, "LocalCurvatureParentID (1) = %s\n", SO1->LocalCurvatureParentID);
    fprintf(stderr, "LocalCurvatureParentID (2) = %s\n", SO2->LocalCurvatureParentID);
    fprintf(stderr, "LocalDomainParent (1) = %s\n", SO1->LocalDomainParent);
    fprintf(stderr, "LocalDomainParent (2) = %s\n", SO2->LocalDomainParent);
    fprintf(stderr, "LocalDomainParentID (1) = %s\n", SO1->LocalDomainParentID);
    fprintf(stderr, "LocalDomainParentID (2) = %s\n", SO2->LocalDomainParentID);
    fprintf(stderr, "SUMA_VolPar_Aligned (1) = %d\n", SO1->SUMA_VolPar_Aligned);
    fprintf(stderr, "SUMA_VolPar_Aligned (2) = %d\n", SO2->SUMA_VolPar_Aligned);
    fprintf(stderr, "APPLIED_A2Exp_XFORM (1) = %d\n", SO1->APPLIED_A2Exp_XFORM);
    fprintf(stderr, "APPLIED_A2Exp_XFORM (2) = %d\n", SO2->APPLIED_A2Exp_XFORM);
    fprintf(stderr, "Saux (1) = %p\n", SO1->Saux);
    fprintf(stderr, "Saux (2) = %p\n", SO2->Saux);
    fprintf(stderr, "SentToAfni (1) = %d\n", SO1->SentToAfni);
    fprintf(stderr, "SentToAfni (2) = %d\n", SO2->SentToAfni);
    fprintf(stderr, "Show (1) = %d\n", SO1->Show);
    fprintf(stderr, "Show (2) = %d\n", SO2->Show);
    fprintf(stderr, "PolyMode (1) = %d\n", SO1->PolyMode);
    fprintf(stderr, "PolyMode (2) = %d\n", SO2->PolyMode);
    fprintf(stderr, "TransMode (1) = %d\n", SO1->TransMode);
    fprintf(stderr, "TransMode (2) = %d\n", SO2->TransMode);
    fprintf(stderr, "NodeNormList (1) = %p\n", SO1->NodeNormList);
    fprintf(stderr, "NodeNormList (2) = %p\n", SO2->NodeNormList);
    fprintf(stderr, "FaceNormList (1) = %p\n", SO1->FaceNormList);
    fprintf(stderr, "FaceNormList (2) = %p\n", SO2->FaceNormList);
    fprintf(stderr, "normdir (1) = %d\n", SO1->normdir);
    fprintf(stderr, "normdir (2) = %d\n", SO2->normdir);
    fprintf(stderr, "Center (1) = %f, %f, %f\n", SO1->Center[0], SO1->Center[1], SO1->Center[2]);
    fprintf(stderr, "Center (2) = %f, %f, %f\n", SO2->Center[0], SO2->Center[1], SO2->Center[2]);
    fprintf(stderr, "MaxDims (1) = %f, %f, %f\n", SO1->MaxDims[0], SO1->MaxDims[1], SO1->MaxDims[2]);
    fprintf(stderr, "MaxDims (2) = %f, %f, %f\n", SO2->MaxDims[0], SO2->MaxDims[1], SO2->MaxDims[2]);
    fprintf(stderr, "MinDims (1) = %f, %f, %f\n", SO1->MinDims[0], SO1->MinDims[1], SO1->MinDims[2]);
    fprintf(stderr, "MinDims (2) = %f, %f, %f\n", SO2->MinDims[0], SO2->MinDims[1], SO2->MinDims[2]);
    fprintf(stderr, "patchCenter (1) = %f, %f, %f\n", SO1->patchCenter[0], SO1->patchCenter[1], SO1->patchCenter[2]);
    fprintf(stderr, "patchCenter (2) = %f, %f, %f\n", SO2->patchCenter[0], SO2->patchCenter[1], SO2->patchCenter[2]);
    fprintf(stderr, "patchMaxDims (1) = %f, %f, %f\n", SO1->patchMaxDims[0], SO1->patchMaxDims[1], SO1->patchMaxDims[2]);
    fprintf(stderr, "patchMaxDims (2) = %f, %f, %f\n", SO2->patchMaxDims[0], SO2->patchMaxDims[1], SO2->patchMaxDims[2]);
    fprintf(stderr, "patchMinDims (1) = %f, %f, %f\n", SO1->patchMinDims[0], SO1->patchMinDims[1], SO1->patchMinDims[2]);
    fprintf(stderr, "patchMinDims (2) = %f, %f, %f\n", SO2->patchMinDims[0], SO2->patchMinDims[1], SO2->patchMinDims[2]);
    fprintf(stderr, "aMinDims (1) = %f\n", SO1->aMinDims);
    fprintf(stderr, "aMinDims (2) = %f\n", SO2->aMinDims);
    fprintf(stderr, "aMaxDims (1) = %f\n", SO1->aMaxDims);
    fprintf(stderr, "aMaxDims (2) = %f\n", SO2->aMaxDims);
    fprintf(stderr, "MaxCentDist (1) = %f\n", SO1->MaxCentDist);
    fprintf(stderr, "MaxCentDist (2) = %f\n", SO2->MaxCentDist);
    fprintf(stderr, "MaxCentDistNode (1) = %d\n", SO1->MaxCentDistNode);
    fprintf(stderr, "MaxCentDistNode (2) = %d\n", SO2->MaxCentDistNode);
    fprintf(stderr, "MinCentDist (1) = %f\n", SO1->MinCentDist);
    fprintf(stderr, "MinCentDist (2) = %f\n", SO2->MinCentDist);
    fprintf(stderr, "MinCentDistNode (1) = %d\n", SO1->MinCentDistNode);
    fprintf(stderr, "MinCentDistNode (2) = %d\n", SO2->MinCentDistNode);
    fprintf(stderr, "N_patchNode (1) = %d\n", SO1->N_patchNode);
    fprintf(stderr, "N_patchNode (2) = %d\n", SO2->N_patchNode);
    fprintf(stderr, "patchNodeMask (1) = %p\n", SO1->patchNodeMask);
    fprintf(stderr, "patchNodeMask (2) = %p\n", SO2->patchNodeMask);
    fprintf(stderr, "patchaMinDims (1) = %f\n", SO1->patchaMinDims);
    fprintf(stderr, "patchaMinDims (2) = %f\n", SO2->patchaMinDims);
    fprintf(stderr, "patchaMaxDims (1) = %f\n", SO1->patchaMaxDims);
    fprintf(stderr, "patchaMaxDims (2) = %f\n", SO2->patchaMaxDims);
    fprintf(stderr, "RotationWeight (1) = %d\n", SO1->RotationWeight);
    fprintf(stderr, "RotationWeight (2) = %d\n", SO2->RotationWeight);
    fprintf(stderr, "ViewCenterWeight (1) = %d\n", SO1->ViewCenterWeight);
    fprintf(stderr, "ViewCenterWeight (2) = %d\n", SO2->ViewCenterWeight);
    fprintf(stderr, "glar_NodeList (1) = %p\n", SO1->glar_NodeList);
    fprintf(stderr, "glar_NodeList (2) = %p\n", SO2->glar_NodeList);
    fprintf(stderr, "glar_FaceSetList (1) = %p\n", SO1->glar_FaceSetList);
    fprintf(stderr, "glar_FaceSetList (2) = %p\n", SO2->glar_FaceSetList);
    fprintf(stderr, "glar_FaceNormList (1) = %p\n", SO1->glar_FaceNormList);
    fprintf(stderr, "glar_FaceNormList (2) = %p\n", SO2->glar_FaceNormList);
    fprintf(stderr, "PermCol (1) = %p\n", SO1->PermCol);
    fprintf(stderr, "PermCol (2) = %p\n", SO2->PermCol);
    fprintf(stderr, "glar_NodeNormList (1) = %p\n", SO1->glar_NodeNormList);
    fprintf(stderr, "glar_NodeNormList (2) = %p\n", SO2->glar_NodeNormList);
    fprintf(stderr, "ShowMeshAxis (1) = %d\n", SO1->ShowMeshAxis);
    fprintf(stderr, "ShowMeshAxis (2) = %d\n", SO2->ShowMeshAxis);
    fprintf(stderr, "MeshAxis (1) = %p\n", SO1->MeshAxis);
    fprintf(stderr, "MeshAxis (2) = %p\n", SO2->MeshAxis);
    fprintf(stderr, "MF (1) = %p\n", SO1->MF);
    fprintf(stderr, "MF (2) = %p\n", SO2->MF);
    fprintf(stderr, "FN (1) = %p\n", SO1->FN);
    fprintf(stderr, "FN (2) = %p\n", SO2->FN);
    fprintf(stderr, "EL (1) = %p\n", SO1->EL);
    fprintf(stderr, "EL (2) = %p\n", SO2->EL);
    fprintf(stderr, "PolyArea (1) = %p\n", SO1->PolyArea);
    fprintf(stderr, "PolyArea (2) = %p\n", SO2->PolyArea);
    fprintf(stderr, "SC (1) = %p\n", SO1->SC);
    fprintf(stderr, "SC (2) = %p\n", SO2->SC);
    fprintf(stderr, "SelectedNode (1) = %d\n", SO1->SelectedNode);
    fprintf(stderr, "SelectedNode (2) = %d\n", SO2->SelectedNode);
    fprintf(stderr, "NodeMarker (1) = %p\n", SO1->NodeMarker);
    fprintf(stderr, "NodeMarker (2) = %p\n", SO2->NodeMarker);
    fprintf(stderr, "SelectedFaceSet (1) = %d\n", SO1->SelectedFaceSet);
    fprintf(stderr, "SelectedFaceSet (2) = %d\n", SO2->SelectedFaceSet);
    fprintf(stderr, "FaceSetMarker (1) = %p\n", SO1->FaceSetMarker);
    fprintf(stderr, "FaceSetMarker (2) = %p\n", SO2->FaceSetMarker);
    fprintf(stderr, "VolPar (1) = %p\n", SO1->VolPar);
    fprintf(stderr, "VolPar (2) = %p\n", SO2->VolPar);
    fprintf(stderr, "Overlays (1) = %p\n", SO1->Overlays);
    fprintf(stderr, "Overlays (2) = %p\n", SO2->Overlays);
    fprintf(stderr, "N_Overlays (1) = %d\n", SO1->N_Overlays);
    fprintf(stderr, "N_Overlays (2) = %d\n", SO2->N_Overlays);
    fprintf(stderr, "SurfCont (1) = %p\n", SO1->SurfCont);
    fprintf(stderr, "SurfCont (2) = %p\n", SO2->SurfCont);
    fprintf(stderr, "CommonNodeObject (1) = %p\n", SO1->CommonNodeObject);
    fprintf(stderr, "CommonNodeObject (2) = %p\n", SO2->CommonNodeObject);
    fprintf(stderr, "NodeObjects (1) = %p\n", SO1->NodeObjects);
    fprintf(stderr, "NodeObjects (2) = %p\n", SO2->NodeObjects);
    fprintf(stderr, "NodeNIDOObjects (1) = %p\n", SO1->NodeNIDOObjects);
    fprintf(stderr, "NodeNIDOObjects (2) = %p\n", SO2->NodeNIDOObjects);
    fprintf(stderr, "NodeAreas (1) = %p\n", SO1->NodeAreas);
    fprintf(stderr, "NodeAreas (2) = %p\n", SO2->NodeAreas);
    fprintf(stderr, "VisX0 (1) = %p, %d, %p\n", SO1->VisX0.Xchain, SO1->VisX0.Applied, SO1->VisX0.XformedCoords);
    fprintf(stderr, "VisX0 (2) = %p, %d, %p\n", SO2->VisX0.Xchain, SO2->VisX0.Applied, SO2->VisX0.XformedCoords);
    fprintf(stderr, "VisX (1) = %p, %d, %p\n", SO1->VisX.Xchain, SO1->VisX.Applied, SO1->VisX.XformedCoords);
    fprintf(stderr, "VisX (2) = %p, %d, %p\n", SO2->VisX.Xchain, SO2->VisX.Applied, SO2->VisX.XformedCoords);
    fprintf(stderr, "NodeList_swp (1) = %p\n", SO1->NodeList_swp);
    fprintf(stderr, "NodeList_swp (2) = %p\n", SO2->NodeList_swp);
    fprintf(stderr, "PointSize (1) = %f\n", SO1->PointSize);
    fprintf(stderr, "PointSize (2) = %f\n", SO2->PointSize);
    fprintf(stderr, "DW (1) = %p\n", SO1->DW);
    fprintf(stderr, "DW (2) = %p\n", SO2->DW);

    fprintf(stderr, "Ending surface comparison\n");
}

void getSquareOnPlane(float *plane, float points[4][3]){

    float planeOrigin[3], otherPoint[3], normal[3], tangent[3], bitangent[3];
    float   divisor;
    float objectMinMax[2], axisMinMax[3][2];
    int i;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Get plane on square\n");

    getOveralMinAndMaxOfCurrentSurfaceObjects(axisMinMax, objectMinMax);

    // Get plane point closest to view origin
    getPlanePtClosestToViewerOrigin(plane, planeOrigin);

    // Get other point on plane.  Adjust x and determine y to setisfy Ax+By+Cz=D
    float otherViewerPt[3] = {10.0, 10.0, 10.0};
    getPlanePtClosestToViewerPoint(plane, otherViewerPt, otherPoint);

    // Get normal vector of plane
    divisor = sqrt((plane[0]*plane[0]) + (plane[1]*plane[1]) + (plane[2]*plane[2]));
    for (i=0; i<3; ++i ) normal[i] = plane[i]/divisor;

    // Get unnormalized tangent of plane
    for (i=0; i<3; ++i ) tangent[i] = otherPoint[i] - planeOrigin[i];

    // Normalize tangent
    divisor = sqrt((tangent[0]*tangent[0]) + (tangent[1]*tangent[1]) + (tangent[2]*tangent[2]));
    for (i=0; i<3; ++i ) tangent[i] = tangent[i]/divisor;

    // Get bitangent which is the cross product of the tangent and the normal
    crossProduct(tangent, normal, bitangent);

    // Get points from tangent and bitangent
    float overallMax = MAX (SUMA_ABS(objectMinMax[0]), SUMA_ABS(objectMinMax[1]));
    for (i=0; i<3; ++i){
        points[0][i]=planeOrigin[i]+overallMax*tangent[i]-overallMax*bitangent[i];
        points[1][i]=planeOrigin[i]+overallMax*tangent[i]+overallMax*bitangent[i];
        points[2][i]=planeOrigin[i]-overallMax*tangent[i]+overallMax*bitangent[i];
        points[3][i]=planeOrigin[i]-overallMax*tangent[i]-overallMax*bitangent[i];
    }
}

Boolean updateClipSquare(int planeIndex){
    float plane[4], points[4][3];
    int i, j;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Update clip square\n");

    if (!(clipIdentificationPlane[planeIndex])){
        fprintf(stderr, "ERROR: Index %d exceeds number of clip planes\n", planeIndex);
        return 0;
    }

    // Test values for plane
    for (i=0; i<3; ++i) plane[i]=activeClipPlane[i];
    plane[3] = -activeClipPlane[3];
    plane[3] += 1;


    getSquareOnPlane(plane, points);

    int inc=0;
    for (i=0; i<4; ++i)
        for (j=0; j<3; ++j)
            clipIdentificationPlane[planeIndex]->NodeList[inc++] = points[i][j];

    return 0;
}

Bool makeAxisObject(Widget w, SUMA_SurfaceViewer *sv){

    float plane[4], points[4][3];
    int i, j;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Make axis object\n");

    // Axis plane object already exists?
    if (axisObject) return 1;

    // Test values for plane
    for (i=0; i<4; ++i) plane[i]=activeClipPlane[i];
    plane[3] += 1999;   // Send object rectangle into space so it won't be seen

    getSquareOnPlane(plane, points);

    static SUMA_FreeSurfer_struct FS;

    FS.N_Node = 4;
    FS.N_FaceSet = 3;

    FS.NodeList = (float *)malloc(FS.N_Node*3*sizeof(float));
    int inc=0;
    for (i=0; i<4; ++i)
        for (j=0; j<3; ++j)
            FS.NodeList[inc++] = points[i][j];

    FS.FaceSetList = (int *)malloc(FS.N_FaceSet*3*sizeof(int));
    inc = 0;
    FS.FaceSetList[inc++] = 0;
    FS.FaceSetList[inc++] = 1;
    FS.FaceSetList[inc++] = 2;
    FS.FaceSetList[inc++] = 1;
    FS.FaceSetList[inc++] = 2;
    FS.FaceSetList[inc++] = 3;
    FS.FaceSetList[inc++] = 3;
    FS.FaceSetList[inc++] = 0;
    FS.FaceSetList[inc++] = 1;

    SUMA_SurfaceObject *SO = makeAxisPlaneFromNodeAndFaceSetList(sv, FS);
    if (!SO){
        fprintf(stderr, "Error makeAxisObject: Error drawing clipping plane rectangle.\n");
        return False;
    }
    axisObject = SO;   // Record pointer to clip identification plane object

    axisObject-> ShowMeshAxis = FALSE;  // Display of mesh axis initialized to false

    SUMA_postRedisplay(w, NULL, NULL);  // Refresh window

    return TRUE;
}

Bool makeClipIdentificationPlane(int planeIndex, Widget w, SUMA_SurfaceViewer *sv){
    float plane[4], points[4][3];
    int i, j;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Make clip identification plane\n");

    // Clipping plane identification object already exists?
    if (clippingPlaneIDDisplayableObjects[planeIndex]) return 1;

    getSquareOnPlane(plane, points);

    static SUMA_FreeSurfer_struct FS;

    FS.N_Node = 4;
    FS.N_FaceSet = 3;

    FS.NodeList = (float *)malloc(FS.N_Node*3*sizeof(float));
    int inc=0;
    for (i=0; i<4; ++i)
        for (j=0; j<3; ++j)
            FS.NodeList[inc++] = points[i][j];

    FS.FaceSetList = (int *)malloc(FS.N_FaceSet*3*sizeof(int));
    inc = 0;
    FS.FaceSetList[inc++] = 0;
    FS.FaceSetList[inc++] = 1;
    FS.FaceSetList[inc++] = 2;
    FS.FaceSetList[inc++] = 1;
    FS.FaceSetList[inc++] = 2;
    FS.FaceSetList[inc++] = 3;
    FS.FaceSetList[inc++] = 3;
    FS.FaceSetList[inc++] = 0;
    FS.FaceSetList[inc++] = 1;

    SUMA_SurfaceObject *SO = drawPlaneFromNodeAndFaceSetList(sv, FS, planeIndex);
    if (!SO){
        fprintf(stderr, "Error makeClipIdentificationPlane: Error drawing clipping plane rectangle.\n");
        return False;
    } 
    clipIdentificationPlane[planeIndex] = SO;   // Record pointer to clip identification plane object

    // Avoid gray planes
    for (i=0; i<4; ++i){
        SO->Overlays[0]->V[i] = 0.583694;
    }
    for (i=0; i<4; ++i) SO->Overlays[0]->NodeDef[i] = i;
    SO->Overlays[0]->isBackGrnd = 1;

    SUMA_postRedisplay(w, NULL, NULL);  // Refresh window

    return TRUE;
}

void lightenActiveClipPlaneSquare(int planeIndex){
    SUMA_SurfaceObject* SO =clipIdentificationPlane[planeIndex];
    int i;
    SUMA_SurfaceViewer *sv;
    Widget w=NULL;
    int isv;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Lighten active clip plane square\n");

    // This block is necessary for the color changes to be applied to the square object
    {
        // SO->SurfCont = SUMA_CreateSurfContStruct(SO->idcode_str, SO_type);
        SUMA_GLXAREA_WIDGET2SV(w, sv, isv);
        SUMA_ALL_DO *ado = SUMA_SV_Focus_ADO(sv);
        SO->SurfCont->curColPlane = SUMA_ADO_CurColPlane(ado);

       // switch to the recently loaded  cmap
        SUMA_COLOR_MAP *Cmp = SUMA_FindNamedColMap ("ngray20");
        Cmp->idvec = SO->idcode_str;
        if (!SUMA_SwitchColPlaneCmap(ado, Cmp)) {
            fprintf(stderr, "Failed in SUMA_SwitchColPlaneCmap");
            return;
        }

        SUMA_PICK_RESULT *PR = (SUMA_PICK_RESULT *)SUMA_calloc(1,sizeof(SUMA_PICK_RESULT));
        colorPlanes(sv, SO, &PR);
    }


    switch(planeIndex){
        case 0: makeCommonNodesOfRectangleRed(SO); break;
        case 1: makeCommonNodesOfRectangleGreen(SO); break;
        case 2: makeCommonNodesOfRectangleBlue(SO); break;
        case 3: makeCommonNodesOfRectangleCyan(SO); break;
        case 4: makeCommonNodesOfRectangleMagenta(SO); break;
        case 5: makeCommonNodesOfRectangleYellow(SO); break;
    }
}

 void darkenClipPlaneSquare(int planeIndex){
    SUMA_SurfaceObject* SO =clipIdentificationPlane[planeIndex];
        SUMA_SurfaceViewer *sv;
        Widget w=NULL;
        int isv, i;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Darken clipping plane square %d\n", planeIndex+1);

    switch(planeIndex){
        case 0: makeCommonNodesOfRectangleDarkRed(SO); break;
        case 1: makeCommonNodesOfRectangleDarkGreen(SO); break;
        case 2: makeCommonNodesOfRectangleDarkBlue(SO); break;
        case 3: makeCommonNodesOfRectangleDarkCyan(SO); break;
        case 4: makeCommonNodesOfRectangleDarkMagenta(SO); break;
        case 5: makeCommonNodesOfRectangleDarkYellow(SO); break;
    }

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Darken clipping plane square: color changes to be applied to the square object\n");

    // This block is necessary for the color changes to be applied to the square object
    // PDL: Not clear why two iterations are necessary to the last two planes.  Hopefully,
    //  this will become clear with further analysis of the code duirng the addition of
    //  added functionality.
    int iterations = (planeIndex > 3)? 2 : 1;
    for (i = 0; i < iterations; ++i)
    {
        // SO->SurfCont = SUMA_CreateSurfContStruct(SO->idcode_str, SO_type);
        if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
            fprintf(stderr, "### Darken clipping plane square: Make SO->SurfCont\n");
        SUMA_GLXAREA_WIDGET2SV(w, sv, isv);
        SUMA_ALL_DO *ado = SUMA_SV_Focus_ADO(sv);
        SO->SurfCont->curColPlane = SUMA_ADO_CurColPlane(ado);

       // switch to the recently loaded  cmap
        SUMA_COLOR_MAP *Cmp = SUMA_FindNamedColMap ("ngray20");
        Cmp->idvec = SO->idcode_str;
        if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        {
            fprintf(stderr, "### Darken clipping plane square: switch to the recently loaded  cmap\n");
            fprintf(stderr, "### Darken clipping plane square: Cmp = %p\n", Cmp);
            fprintf(stderr, "### Darken clipping plane square: Cmp Name = %s\n", Cmp->Name);
            fprintf(stderr, "### Darken clipping plane square: Cmp cname = %ls\n", Cmp->cname);
            fprintf(stderr, "### Darken clipping plane square: justEnteredClippingPlaneMode = %d\n", justEnteredClippingPlaneMode);
            fprintf(stderr, "### Darken clipping plane square: sv->N_ColList = %d\n", sv->N_ColList);
        }
        if (!justEnteredClippingPlaneMode && !SUMA_SwitchColPlaneCmap(ado, Cmp)) {
            fprintf(stderr, "Failed in SUMA_SwitchColPlaneCmap");
            return;
        }

        if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
            fprintf(stderr, "### Darken clipping plane square: SUMA_PICK_RESULT\n");
        SUMA_PICK_RESULT *PR = (SUMA_PICK_RESULT *)SUMA_calloc(1,sizeof(SUMA_PICK_RESULT));
        colorPlanes(sv, SO,&PR);
    }

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Darken clipping plane square: completed\n");
}

void darkenInactiveClipPlaneSquares(int activePlane){
    int p, i;

    if (SUMAg_CF->clippingPlaneVerbose && SUMAg_CF->clippingPlaneVerbosityLevel>1)
        fprintf(stderr, "### Darken inactive clipping plane squares\n");

    for (p=0; p<SUMAg_CF->N_ClipPlanes; ++p) if (p!=activePlane){
        darkenClipPlaneSquare(p);
    }

        SUMA_SurfaceObject* SO =clipIdentificationPlane[1];
}

void resetClippingPlaneParameters(float *planeTheta, float *planePhi, float *planeA,
    float *planeB, float *planeC){
    char chrTmp[64];
    int isv;
    SUMA_SurfaceViewer *sv;
    Widget w=NULL;
    int i;

    SUMA_GLXAREA_WIDGET2SV(w, sv, isv);

    // Reset clipping plane parameters
    for (i=0; i<6; ++i){
         active[i] = 0;
         previouslyActive[i] = 0;
         planeTheta[i] = planePhi[i] = planeA[i] =  planeB[i] = 0.0f;
         if (clippingPlaneIDDisplayableObjects[i]) clippingPlaneIDDisplayableObjects[i]->Show = 0;
         planeC[i] = 1.0f;
    }
    planeTheta[1] = 90.0f;
    planeTheta[3] = 180.0f;
    planeTheta[4] = 270.0f;
    planeTheta[5] = 180.0f;

    planePhi[2] = 90.0f;
    planePhi[5] = 270.0f;
    /*
    planeA[2] = 1.0f;
    planeB[1] = -1.0f;
    */
    // Reset plane parameters
    for (i=0; i<6; ++i){
        planeA[i]=planeB[i]=0.0f;
        planeC[i] = 1.0f;
    }
    planeA[2] = 1.0f;
    planeB[1] = -1.0f;

    // Deactivate all clipping planes
    for (i=1; i<SUMAg_CF->N_ClipPlanes; ++i){
        active[i] = 0;
        sprintf(chrTmp, "%s: %f,%f,%f,%d", SUMAg_CF->ClipPlanesLabels[i], planeA[i], planeB[i],
            planeC[i], 99999999);

        SUMA_SetObjectClip(chrTmp, sv);
    }

    previousClipPlaneIdentificationMode=1;
    // clippingPlaneMode=0;
}

void clipPlaneTransform(float  deltaTheta, float deltaPhi, float deltaPlaneD, Bool flip,
    int activePlane, Bool toggleOffOn, Bool reset){
    static float  planeTheta[SUMA_MAX_N_CLIP_PLANES]={0,90,0,180,270,180};
    static float  planePhi[SUMA_MAX_N_CLIP_PLANES]={0,0,90,0,0,270};
    static float  planeA[SUMA_MAX_N_CLIP_PLANES]={0.0,0.0,1.0,0.0,0.0,0.0};
    static float  planeB[SUMA_MAX_N_CLIP_PLANES]={0.0,-1.0,0.0,0.0,0.0,0.0};
    static float  planeC[SUMA_MAX_N_CLIP_PLANES]={1.0,1.0,1.0,1.0,1.0,1.0};
    static float  planeD[SUMA_MAX_N_CLIP_PLANES]={0,0,0,50,50,50};
    static float rad2degrees=180.0/M_PI, degrees2rad=M_PI/180;
    static float objectMinMax[3][2] = {{0.0f, 0.0f}, {0.0f, 0.0f}, {0.0f, 0.0f}};
    char chrTmp[64];
    int isv, i;
    static int planeIndex = 0;
    SUMA_SurfaceViewer *sv;
    Widget w=NULL;
    static SUMA_Boolean    firstCall = 1;
    
    if (reset){
        planeIndex = 0;
        // These ranges determine
        planeD[0] = -objectMinMax[2][0];
        planeD[3] = objectMinMax[2][1];
        planeD[1] = objectMinMax[1][1];
        planeD[4] = -objectMinMax[1][0];
        planeD[2] = -objectMinMax[0][0];
        planeD[5] = objectMinMax[0][1];

        resetClippingPlaneParameters(planeTheta, planePhi, planeA, planeB, planeC);
        activePlane = 0;
        active[0] = 1;
        planeIndex = 0;
    }
    
    // Change active plane.  Input active plane index is 1-indexed but local planeIndex is 0-indexed
    //  activePlane<-0 means keep existing active plane.  If activePlane too high, select highest indexed plane
    if (activePlane >=0 && !toggleOffOn){
        if (activePlane <= SUMAg_CF->N_ClipPlanes)  planeIndex = activePlane;
        else  planeIndex = SUMAg_CF->N_ClipPlanes;
    }
    
    // Set up normal offset loactions s.t. clipping planes just enclose existing objects
    if (firstCall)  {
        // Get ranges of orthogonal axes
        getObjectMinMaxForAxes(objectMinMax);

        // These ranges determine
        planeD[0] = -objectMinMax[2][0];
        planeD[3] = objectMinMax[2][1];
        planeD[1] = objectMinMax[1][1];
        planeD[4] = -objectMinMax[1][0];
        planeD[2] = -objectMinMax[0][0];
        planeD[5] = objectMinMax[0][1];

        // Store previous object axes ranges as gloabl for clipping plane functions
        memcpy(clippingPlaneAxisRanges, objectMinMax, 6*sizeof(float));
        firstCall = 0;
        
        // Initialize increment to 1/40 max width of bounding box
        initializeIncrement(objectMinMax);
        // TODO: Add code

        for (i=1; i<6; ++i) planeD[i] = HUGE;
    }
    
    // Turn clipping plane on or off as required
    if (toggleOffOn){
        active[activePlane] = !(active[activePlane]);
        clipIdentificationPlane[activePlane]->Show = (clipPlaneIdentificationMode && active[activePlane]);
    }
    
    if (flip){
        planeA[planeIndex] = -planeA[planeIndex];
        planeB[planeIndex] = -planeB[planeIndex];
        planeC[planeIndex] = -planeC[planeIndex];
        planeD[planeIndex] = -planeD[planeIndex];
        planeTheta[planeIndex] = (int)(asin(-planeB[planeIndex])*rad2degrees+0.5);
        planePhi[planeIndex] = (int)(acos(planeC[planeIndex]/cos(planeTheta[planeIndex]*degrees2rad))*rad2degrees+0.5);
    } else if (!reset && active[planeIndex]){
        // Update rotation and (normal) translation parameters
        planeTheta[planeIndex] += deltaTheta;
        planePhi[planeIndex] += deltaPhi;
        planeD[planeIndex] += deltaPlaneD;

        // Rotate around x-axis
        planeB[planeIndex] = -sin(planeTheta[planeIndex]*degrees2rad);
        planeC[planeIndex] = cos(planeTheta[planeIndex]*degrees2rad);

        // Rotate arount y axis
        float oldPlaneA = planeA[planeIndex];
        planeA[planeIndex] = planeC[planeIndex]*sin(planePhi[planeIndex]*degrees2rad);
        if (oldPlaneA*planeA[planeIndex] < 0.1) planeA[planeIndex] = -planeA[planeIndex];
        planeC[planeIndex] = planeC[planeIndex]*cos(planePhi[planeIndex]*degrees2rad);
    }
    
    // Apply rotational, and translational, parameters to selected clipping plane
    SUMA_GLXAREA_WIDGET2SV(w, sv, isv);

    // Record active clip plane as global variable
    activeClipPlane[0] = planeA[planeIndex];
    activeClipPlane[1] = planeB[planeIndex];
    activeClipPlane[2] = planeC[planeIndex];
    activeClipPlane[3] = planeD[planeIndex];
    
    // Show user which clip plane is active
    if (clipPlaneIdentificationMode){
        if (planeIndex != SUMAg_CF->N_ClipPlanes) updateClipSquare(planeIndex);

        SUMA_postRedisplay(w, NULL, NULL);  // Refresh window.  (Not sure this is necessary or helps)
    }
    
    // Record rotation angles
    clippingPlaneTheta[planeIndex] = planeTheta[planeIndex];
    clippingPlanePhi[planeIndex] = planePhi[planeIndex];

    // Activate/update clip plane
    sprintf(chrTmp, "%s: %.4f,%.4f,%.4f,%.4f", SUMAg_CF->ClipPlanesLabels[planeIndex], planeA[planeIndex], planeB[planeIndex],
        planeC[planeIndex], (active[planeIndex])? planeD[planeIndex]:99999999);

     // Record selected clipping plane index for saving to file
     selectedPlane = planeIndex;

    SUMA_SetObjectClip(chrTmp, sv);
}

void writeClippingPlanes (char *s, void *data){
    SUMA_SurfaceViewer *sv = (SUMA_SurfaceViewer *)data;
    FILE *outFile;
    int     i, j, parameterInc=0, lastPlane = SUMAg_CF->N_ClipPlanes-1;

     fprintf(stderr, "s = %s\n", s);


     // Open output file
    if (!(outFile = fopen(s, "w"))){
        perror("Error opening output file");
        return;
    }

    // Write opening tag
     fprintf(outFile, "# <Viewer_Visual_Setting\n");

     // Write global settings
     fprintf(outFile, "# sel_plane_num  = \"%d\"\n", selectedPlane);
     fprintf(outFile, "# tilt_inc  = \"%f\"\n", tiltInc);
     fprintf(outFile, "# scroll_inc  = \"%f\"\n", scrollInc);
     fprintf(outFile, "# x_axis_rotations  = \"");
     for (i=0; i<SUMAg_CF->N_ClipPlanes; ++i)
        fprintf(outFile, "%f%s", clippingPlaneTheta[i], (i<lastPlane)? "," : "\"\n");
     fprintf(outFile, "# y_axis_rotations  = \"");
     for (i=0; i<SUMAg_CF->N_ClipPlanes; ++i)
        fprintf(outFile, "%f%s", clippingPlanePhi[i], (i<lastPlane)? "," : "\"\n");
     fprintf(outFile, "# normal_offsets  = \"");
     for (i=0; i<SUMAg_CF->N_ClipPlanes; ++i)
        fprintf(outFile, "%f%s", SUMAg_CF->ClipPlanes[i*4 +3], (i<lastPlane)? "," : "\"\n");

     for (i=0; i<SUMAg_CF->N_ClipPlanes; ++i)
     {
        fprintf(outFile, "# plane_%d_act   = \"%d\"\n", i+1, active[i]);
        /*
        fprintf(outFile, "# plane_%d_eq   = \"", i+1);
        for (j=0; j<3; ++j) fprintf(outFile, "%f + ", SUMAg_CF->ClipPlanes[parameterInc++]);
        fprintf(outFile, "%f\"\n", SUMAg_CF->ClipPlanes[parameterInc++]);
        */
     }

    // Write closing tag
     fprintf(outFile, "# />\n");

     // Close output file
     fclose(outFile);
}

 void getPlanePtClosestToViewerOrigin(float *plane, float *point){
    int i;
    /* Returns the point, on the plane, closest to the viewer origin <0,0,0>.  This
    point is given by P = k<A,B,C> s.t. k(A^2 + B^2 + C^2) = D is satisfied.  I.e.
    k = D/(A^2 + B^2 + C^2)
    */

    float k = plane[3]/((plane[0]*plane[0])+(plane[1]*plane[1])+(plane[2]*plane[2]));

    for (i=0; i<3; ++i) point[i] = k*plane[i];
}

 void getPlanePtClosestToViewerPoint(float *plane, float *viewerPt, float *point){
     int i;
   /* Returns the point, on the plane, closest to the viewer origin <0,0,0>.  This
    point is given by P = k<A,B,C> s.t. k(A^2 + B^2 + C^2) = D is satisfied.  I.e.
    k = D/(A^2 + B^2 + C^2)
    */

    float k = (plane[3]-(plane[0]*viewerPt[0])-(plane[1]*viewerPt[1])-(plane[2]*viewerPt[2]))/((plane[0]*plane[0])+(plane[1]*plane[1])+(plane[2]*plane[2]));

    for (i=0; i<3; ++i) point[i] = viewerPt[i] + k*plane[i];
}

void crossProduct(float input1[], float input2[], float output[]){
    output[0] = (input1[1]*input2[2]) - (input1[2]*input2[1]);
    output[1] = (input1[2]*input2[0]) - (input1[0]*input2[2]);
    output[2] = (input1[0]*input2[1]) - (input1[1]*input2[0]);
}

void getOveralMinAndMaxOfCurrentSurfaceObjects(float axisMinMax[3][2], float *objectMinMax){
    int i, dov_ID;

    objectMinMax[0] = 1000.0;
    objectMinMax[1] = -1000.0;

    for (i=0; i<3; ++i){
        axisMinMax[i][0] = 1000.0;
        axisMinMax[i][1] = -1000.0;
    }

    for (dov_ID=0; dov_ID<SUMAg_N_DOv; ++dov_ID){
        SUMA_SurfaceObject *soOld = (SUMA_SurfaceObject *)SUMAg_DOv[dov_ID].OP;
        if (soOld->N_Node>0 && soOld->NodeDim==3){
            objectMinMax[0] = MIN(objectMinMax[0], soOld->aMinDims);
            objectMinMax[1] = MAX(objectMinMax[1], soOld->aMaxDims);
            for (i=0; i<3; ++i){
                axisMinMax[i][0] = MIN(axisMinMax[i][0], soOld->MaxDims[i]);
                axisMinMax[i][1] = MAX(axisMinMax[i][1], soOld->MaxDims[i]);
            }
        }
    }

    // Account for possibility of no valid surface objects
    if (objectMinMax[0] > objectMinMax[1]){
        objectMinMax[0] = -100.0;
        objectMinMax[1] = 100.0;
        for (i=0; i<3; ++i){
            axisMinMax[i][0] = -100.0;
            axisMinMax[i][1] = 100.0;
        }
    }
}

void getFourCoordsJustInsideClipPlane(float *plane, float points[4][3]){

    float divisor=plane[0]+plane[1]+plane[2];
    float D = plane[3] + ((plane[3]>0)? - 1 : 1);

    float x=-plane[0]*D/divisor;
    float y=-plane[1]*D/divisor;
    float z=-plane[2]*D/divisor;

    // First point
    points[0][0]=x;
    points[0][1]=y;
    points[0][2]=z;

    // Second point
    x+=50;
    y=D-x-z;
    points[1][0]=x;
    points[1][1]=y;
    points[1][2]=z;

    // Third point
    x-=50;
    y=-plane[1]*D/divisor+50;
    z=D-x-y;
    points[2][0]=x;
    points[2][1]=y;
    points[2][2]=z;

    // Fourth point
    y-=50;
    z=-plane[2]*D/divisor+50;
    x=D-z-y;
    points[3][0]=x;
    points[3][1]=y;
    points[3][2]=z;
}
