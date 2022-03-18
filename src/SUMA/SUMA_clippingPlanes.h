
// Clipping plane variables
extern Bool clippingPlaneMode;
extern SUMA_SurfaceObject* clipIdentificationPlane[6];
extern SUMA_SurfaceObject* axisObject;
extern Bool clipPlaneIdentificationMode;
extern Bool active[6];
extern Bool previouslyActive[6];
extern int  selectedPlane;
extern Bool resetClippingPlanes;
extern float  scrollInc;
extern float  tiltInc;
extern Boolean activeClipPlanes;
extern int locallySelectedPlane;
extern DList *list;
extern float clippingPlaneTheta[6], clippingPlanePhi[6];

#define HUGE 9.9e9;

void initializeIncrement(float objectMinMax[3][2]);
Boolean toggleClippingPlaneMode(SUMA_SurfaceViewer *sv, Widget w, int *locallySelectedPlane);
void clipPlaneTransform(float  deltaTheta, float deltaPhi, float deltaPlaneD, Bool flip,
    int activePlane, Bool toggleOffOn, Bool reset);
void drawClipPlane(float planeA, float planeB, float planeC, float planeD, Widget w,
    SUMA_SurfaceViewer *sv, int isv);
void getFourCoordsJustInsideClipPlane(float *plane, float points[4][3]);
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
SUMA_SurfaceObject *makeAxisPlaneFromNodeAndFaceSetList(SUMA_SurfaceViewer *sv,
    SUMA_FreeSurfer_struct FS);
void compareSurfaces(SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2);
void getSquareOnPlane(float *plane, float points[4][3]);
Boolean updateClipSquare(int planeIndex);
Bool makeClipIdentificationPlane(int planeIndex, Widget w, SUMA_SurfaceViewer *sv);
Bool makeAxisObject(Widget w, SUMA_SurfaceViewer *sv);
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
Boolean loadSavedClippingPlanes(char *clippingPlaneFile, int *locallySelectedPlane);
Boolean applyEquationToClippingPlane(float *equation, int planeIndex);
Boolean determineRotationAnglesFromEquation(float *equation, float *theta, float *phi);
Boolean determineAdditionalRotationsFromRequiredAndExistingRotations(float theta, float phi,
    int planeIndex, float *deltaTheta, float *deltaPhi);
Boolean determineDeltaDFromExistingDAndRequiredD(float requiredD, int planeIndex, float *deltaD);
Boolean getEquationForClippingPlane(NI_element *nel, char attribute[16], float equation[4]);
Boolean getClippingEquationParameters(NI_element *nel, char *attribute, float *parameters);
Boolean applyEquationParametersToClippingPlane(int planeIndex, float *theta, float *phi, float *offset);
