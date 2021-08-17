
Boolean toggleClippingPlaneMode(SUMA_SurfaceViewer *sv, Widget w, int *locallySelectedPlane);
void clipPlaneTransform(float  deltaTheta, float deltaPhi, float deltaPlaneD, Bool flip,
    int activePlane, Bool toggleOffOn, Bool reset);
void drawClipPlane(float planeA, float planeB, float planeC, float planeD, Widget w,
    SUMA_SurfaceViewer *sv, int isv);
void getFourCoordsJustInsideClipPlane(float *plane, float points[4][3]);
static Bool clipPlaneIdentificationMode, previousClipPlaneIdentificationMode=1;
static SUMA_SurfaceObject* clipIdentificationPlane[6];
static float activeClipPlane[4];
static Bool active[6] = {1,1,1,1,1,1};
static Bool previouslyActive[6] = {0,0,0,0,0,0};
static float clippingPlaneAxisRanges[3][2] = {{0.0f,0.0f},{0.0f,0.0f},{0.0f,0.0f}};
static SUMA_SurfaceObject * clippingPlaneIDDisplayableObjects[6] = {NULL, NULL, NULL, NULL, NULL, NULL};
int  selectedPlane;
static Bool resetClippingPlanes=0;
static float  scrollInc = 1.0;
static float  tiltInc = 1.0;
static Boolean justEnteredClippingPlaneMode;
static float clippingPlaneTheta[SUMA_MAX_N_CLIP_PLANES]={0,90,0,180,270,180};
static float clippingPlanePhi[SUMA_MAX_N_CLIP_PLANES]={0,0,90,0,0,270};
static Boolean activeClipPlanes = True;

void writeClippingPlanes (char *s, void *data);
void determineCornersOfSquare(SUMA_SurfaceObject *SO);
void getObjectMinMaxForAxes(float objectMinMax[][2]);
int colorPlanes(SUMA_SurfaceViewer *sv, SUMA_SurfaceObject *SO,
                     SUMA_PICK_RESULT **PRi);
Boolean activeClippingPlanes();
void dimensionsInscribeThoseOfPreviousSurfaceObjects(SUMA_SurfaceObject *SO);
void makeCommonNodesOfRectangleDarkRed(SUMA_SurfaceObject *SO);
void makeCommonNodesOfRectangleDarkGreen(SUMA_SurfaceObject *SO);
void makeCommonNodesOfRectangleDarkBlue(SUMA_SurfaceObject *SO);
void makeCommonNodesOfRectangleDarkCyan(SUMA_SurfaceObject *SO);
void makeCommonNodesOfRectangleDarkMagenta(SUMA_SurfaceObject *SO);
void makeCommonNodesOfRectangleDarkYellow(SUMA_SurfaceObject *SO);
void makeCommonNodesOfRectangleRed(SUMA_SurfaceObject *SO);
void makeCommonNodesOfRectangleGreen(SUMA_SurfaceObject *SO);
void makeCommonNodesOfRectangleBlue(SUMA_SurfaceObject *SO);
void makeCommonNodesOfRectangleCyan(SUMA_SurfaceObject *SO);
void makeCommonNodesOfRectangleMagenta(SUMA_SurfaceObject *SO);
void makeCommonNodesOfRectangleYellow(SUMA_SurfaceObject *SO);
SUMA_SurfaceObject *drawPlaneFromNodeAndFaceSetList(SUMA_SurfaceViewer *sv,
    SUMA_FreeSurfer_struct FS, int planeIndex);
void compareSurfaces(SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2);
void getSquareOnPlane(float *plane, float points[4][3]);
Boolean updateClipSquare(int planeIndex);
Bool makeClipIdentificationPlane(int planeIndex, Widget w, SUMA_SurfaceViewer *sv);
void lightenActiveClipPlaneSquare(int activePlane);
void darkenClipPlaneSquare(int planeIndex);
void darkenInactiveClipPlaneSquares(int activePlane);
void getPlanePtClosestToViewerOrigin(float *plane, float *point);
void getPlanePtClosestToViewerPoint(float *plane, float *viewerPt, float *point);
void crossProduct(float input1[], float input2[], float output[]);
void getOveralMinAndMaxOfCurrentSurfaceObjects(float axisMinMax[3][2], float *objectMinMax);
void getFourCoordsJustInsideClipPlane(float *plane, float points[4][3]);
void resetClippingPlaneParameters(float *planeTheta, float *planePhi, float *planeA,
        float *planeB, float *planeC);
Boolean loadSavedClippingPlanes(char *clippingPlaneFile);
Boolean applyEquationToClippingPlane(float *equation, int planeIndex);
Boolean determineRotationAnglesFromEquation(float *equation, float *theta, float *phi);
Boolean determineAdditionalRotationsFromRequiredAndExistingRotations(float theta, float phi,
    int planeIndex, float *deltaTheta, float *deltaPhi);
Boolean determineDeltaDFromExistingDAndRequiredD(float requiredD, int planeIndex, float *deltaD);
Boolean getEquationForClippingPlane(NI_element *nel, char attribute[16], float equation[4]);
Boolean getClippingEquationParameters(NI_element *nel, char *attribute, float *parameters);
Boolean applyEquationParametersToClippingPlane(int planeIndex, float *theta, float *phi, float *offset);
