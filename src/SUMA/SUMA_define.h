#ifndef SUMA_DEFINE_INCLUDED
#define SUMA_DEFINE_INCLUDED

#define ARRAY 1
#define STRAIGHT 2
#define TRIANGLES 1
#define POINTS 2

#define DRAW_METHOD ARRAY
#define RENDER_METHOD TRIANGLES
#define DO_MESH
#define DO_MESH_AXIS
/*#define ZERO_CENTER*/
	
#define NODE_COLOR_R 0.35
#define NODE_COLOR_G 0.35
#define NODE_COLOR_B 0.35
#define SUMA_GRAY_NODE_COLOR 0.30
#define SUMA_DIM_AFNI_COLOR_FACTOR 0.5 /*!< 0.4 works well, use higher factors for flashiness scaling factor (0..1) applied to afni's rgb colors, lower values help retain surface shape info */
#define SUMA_AFNI_COLORPLANE_OPACITY 1
#define SUMA_DIM_CONVEXITY_COLOR_FACTOR 0.5
#define SUMA_CONVEXITY_COLORPLANE_OPACITY 1
#define SUMA_BACKGROUND_MODULATION_FACTOR 3	/*!< 0 background does not modulate foreground, 
																	Color = Fore * avg_Bright * AttenFactor (0 <= avg_Bright <=1)
																	a good setting is such that SUMA_BACKGROUND_ATTENUATION_FACTOR * SUMA_DIM_AFNI_COLOR_FACTOR = 1
																	 Watch for saturation effects!*/

#define SUMA_MAT_SHININESS_INIT 0 /*!< Surface object shininess, 0 20, 50 .. 128*/
#define SUMA_MAT_SPECULAR_INIT 	0.0, 0.0, 0.0, 1.0 /*<! The specular color of the material, keep this and the exponent (that's MAT_SHININESS) 0 to keep shininess down*/
#define SUMA_MAT_AMBIENT_INIT 	0.2, 0.2, 0.2, 1.0 /*<! Ambient light has an undetermined direction and is scattered equally in all directions */
#define SUMA_MAT_DIFFUSE_INIT 	0.8, 0.8, 0.8, 1.0 /*<! Diffuse light comes from one direction, but is scattered equally in all directions and appears equally bright no matter where the eye is located*/
#define SUMA_MAT_EMISSION_INIT 	0.0, 0.0, 0.0, 1.0 /*<! Emissive color is emanated from the object and is unaffected by light sources. It adds no light to other objects in the scene */

#define SUMA_LMODEL_AMBIENT 		1.0, 1.0, 1.0, 1.0 /*<! keep the ambient light high */
#define SUMA_CLEAR_COLOR			0.0, 0.0, 0.0, 0.0

#define SUMA_BACKFACE_CULL 0 /*<! 1/0 flag for culling backface facesets */
#define SUMA_CHECK_WINDING 0 /*<! 1/0 flag for checking triangle winding */

#define SUMA_LIGHT0_COLOR_INIT 	1.0, 1.0, 1.0,  1.0 /*<! add some local light for shading */
#define SUMA_INTITIAL_LIGHT0_SWITCH 1 /*!< -1 works well for SureFit Surfaces, 1 works well for iv and FreeSurfer surfaces */
#define SUMA_STDERR stderr
#define SUMA_STDOUT stdout

#define SUMA_CROSS_HAIR_LINE_WIDTH 1.5
#define SUMA_CROSS_HAIR_RADIUS 6
#define SUMA_CROSS_HAIR_GAP 2
#define SUMA_CROSS_HAIR_SPHERE_RADIUS 0.5
#define SUMA_SELECTED_NODE_SPHERE_RADIUS 0.25

#define SUMA_XYZ_XFORM_BOXDIM_MM 5 /*!< search box width (in mm) used to change XYZ to the closest node index. Keep this one small, 5 mm works for me. Otherwise you may get thrown way off of where you should be. It is no guarantee that the closest node is part of the faceset you are looking at*/
#define SUMA_SELECTED_FACESET_LINE_WIDTH 2 /*!< Line Width of highlighting triangles */
#define SUMA_SELECTED_FACESET_OFFSET_FACTOR 0.01 /*!< highlighting is done by drawing two triangles at a fractional distance of the normal vector */
#define SUMA_SELECTED_FACESET_LINE_INTENSITY 0.75 /*!< line gray color intensity */
#define SUMA_NODE_ALPHA 1 /*!< Node Color Intensity 1, max intensity 0 min intensity*/
#define FOV_INITIAL 30
#define FOV_MIN 0.01
#define FOV_MAX 140
#define FOV_IN_FACT 1.05
#define FOV_OUT_FACT 0.95
#define MOUSE_ZOOM_FACT 30 /*!< The larger, the slower the gain on mouse movement */
#define TRANSLATE_GAIN 50 /*!< between 40 and 80 */
#define ARROW_TRANSLATE_DELTAX 30
#define ARROW_TRANSLATE_DELTAY 30
#define SUMA_MAX_MEMBER_FACE_SETS 60 /*!< Maximum number of facesets a node can be part of */
#define SUMA_MAX_FACESET_EDGE_NEIGHB 3 /*!< Maximum number of adjoining FaceSets a triangular faceset can have.*/
#define SUMA_MAX_DISPLAYABLE_OBJECTS 50
#define SUMA_MAX_SURF_VIEWERS 10 /*!< Maximum number of surface viewers allowed */
#define SUMA_DEFAULT_VIEW_FROM 300 /*!< default view from location on Z axis */
#define SUMA_MAX_NAME_LENGTH 500	/*!< Maximum number of characters in a filename */
#define SUMA_MAX_DIR_LENGTH 2000 	/*!< Maximum number of characters in a directory name */
#define SUMA_MAX_COMMAND_LENGTH		2000/*!< Maximum number of characters in a command string */
#define SUMA_MAX_LABEL_LENGTH 100 /*!< Maximum number of characters for labeling and naming suma fields and objects */
#define SUMA_IDCODE_LENGTH 50	/*!< Max. length of idcode_str of all suma objects */
#define SUMA_MAX_STRING_LENGTH 1000 /*!< Maximum number of characters in a string */ 
#define SUMA_MAX_NUMBER_NODE_NEIGHB	50 /*!< Maximum number of neighbors any one node can have */
#define SUMA_MAX_OVERLAYS 50 /*!< Maximum number of color overlay planes allowed */
#define SUMA_COMMAND_DELIMITER '|'
#define SUMA_COMMAND_TERMINATOR '~'
#define SUMA_PERSPECTIVE_NEAR	1.0	/*!< Z Near, distance from the viewer to the near clipping plane (for gluPerspective)*/
#define SUMA_PERSPECTIVE_FAR		900 /*!< Z Far, distance from the viewer to the far clipping plane (for gluPerspective)*/
#define SUMA_TESSCON_TO_MM 		319.7 /*!< The mysterious Tesscon units */
#define SUMA_TESSCON_DIFF_FLAG 	1000	/*!< If aMaxDim - aMinDim > SUMA_TESSCON_DIFF_FLAG in a .iv file, scaling by SUMA_TESSCON_TO_MM is applied */

#define SUMA_WriteCheckWait 400 /*!< Milliseconds to wait for each stream_writecheck call */ 
#define SUMA_WriteCheckWaitMax 2000 /*!< Milliseconds to try and establish a good WriteCheck */

#define SUMA_MAX_N_SURFACE_SPEC 20/*!< Maximum number of surfaces allowed in a spec file */

typedef enum  { SUMA_FREE_SURFER, SUMA_SUREFIT, SUMA_INVENTOR_GENERIC } SUMA_SO_File_Type;
typedef enum { SUMA_ASCII, SUMA_BINARY } SUMA_SO_File_Format;
typedef enum { NOPE, YUP} SUMA_Boolean;
typedef enum {SO_type, AO_type, GO_type} SUMA_DO_Types;	/*!< Displayable Object Types S: surface, A: axis, G: grid*/
typedef enum {SUMA_SCREEN, SUMA_LOCAL} SUMA_DO_CoordType; /*!< Coordinate system that Displayable object is attached to
																						SCREEN is for a fixed system, LOCAL is for a mobile system,
																						ie one that is rotated by the mouse movements */
typedef enum {SUMA_SOLID_LINE, SUMA_DASHED_LINE} SUMA_STIPPLE;

typedef enum {	SE_Empty, \
					SE_SetLookAt, SE_SetLookFrom, SE_Redisplay, SE_Home, SE_SetNodeColor, \
					SE_FlipLight0Pos, SE_GetNearestNode, SE_SetLookAtNode, SE_HighlightNodes, SE_SetRotMatrix, \
					SE_SetCrossHair, SE_ToggleCrossHair, SE_SetSelectedNode, SE_ToggleShowSelectedNode, SE_SetSelectedFaceSet,\
					SE_ToggleShowSelectedFaceSet, SE_ToggleTalkToAfni, SE_SetAfniCrossHair, SE_SetAfniSurf, SE_BindCrossHair,\
					SE_BadCode} SUMA_ENGINE_CODE;
typedef enum { SEF_Empty, \
					SEF_fm, SEF_im, SEF_fv3, SEF_iv3, SEF_fv15, \
					SEF_iv15, SEF_i, SEF_f, SEF_s, \
					SEF_BadCode} SUMA_ENGINE_FIELD_CODE; 
typedef enum { SES_Empty,\
					SES_Afni,\
					SES_Suma,\
					SES_Unknown} SUMA_ENGINE_SOURCE;
typedef enum { SUMA_int, SUMA_float } SUMA_VARTYPE;

typedef enum { SUMA_CMAP_UNDEFINED, SUMA_CMAP_RGYBR20,  SUMA_CMAP_nGRAY20,\
					SUMA_CMAP_GRAY20, SUMA_CMAP_BW20, SUMA_CMAP_BGYR19, \
					SUMA_CMAP_MATLAB_DEF_BGYR64} SUMA_STANDARD_CMAP; /*!< Names of standard colormaps. RGYBR20 reads Red, Green, Yellow, Blue, Red, 20 colors total */

/*! structure containing a data block information */
typedef struct {
	void *data;	/*!< pointer to data location */
	int N_link; /*!< number of links to data location */
	char ParentIDcode[SUMA_IDCODE_LENGTH]; /* IDcode of the creator of data */
} SUMA_INODE;

/*! Structure containing a color map */
typedef struct {
	float ** M; /*!< N_Col x 3 matrix of R G B values (0..1) */
	int N_Col; /*!< number of colors in the color map */
	char *Name; /*!< Name of colormap */
} SUMA_COLOR_MAP;

/*! Structure containing one color overlay */
typedef struct {
	SUMA_Boolean Show; /*!< if YUP then this overlay enters the composite color map */
	char Name[SUMA_MAX_LABEL_LENGTH]; /*!< name of ovelay, CONVEXITY or Functional or areal boundaries perhaps*/
	int *NodeDef; /*!< nodes overwhich the colors are defined*/
	int N_NodeDef; /*!< total number of nodes specified in NodeDef*/
	int N_Alloc; /*!< You'd think this should be equal to NodeDef, but in instances where you may be receiving
	          varying numbers of colors to the same plane, it's a pane to have to free and realloc space.
				 So, while the juice is only up to N_NodeDef, the allocation is for N_Alloc */
	float **ColMat; /*!< N_NodeDef x 3 matrix containing colors of nodes specified in NodeDef */
	float GlobalOpacity; /*!< Opacity factor between 0 and 1 to apply to all values in ColMat */
	float *LocalOpacity; /*!< Opacity factor vector between 0 and 1 to apply to each individual node color */
	int PlaneOrder; /*!< Order of the overlay plane, 1st plane is 0 and is farthest away from the top */  
	SUMA_Boolean BrightMod; /*!< if YUP then colors overlaid on top of this plane have their brightness modulated by the average intensity of the colors in that plane see the function SUMA_Overlays_2_GLCOLAR4 for details*/  
} SUMA_OVERLAYS;

/*! a structure holding the options for the function SUMA_ScaleToMap 
\sa SUMA_ScaleToMapOptInit to allocate and initialize such a structure 
to free this structure use the free function
*/
typedef struct {
	SUMA_Boolean ApplyMask; /*!< if YUP then values that fall in MaskRange are assigned the color in MaskColor */
	float MaskRange[2]; /*!< values between MaskRange[0] and MaskRange[1] (inclusive) are assigned MaskColor */
	float MaskColor[3]; /*!< color to assign to masked nodes */
	SUMA_Boolean ApplyClip; /*!< if YUP then values that range clipping using Range is applied */
	float ClipRange[2]; /*!< nodes with values <= Range[0] are given the first color in the color map, values >= Range[1] get the last color in the map */
	float BrightFact; /*!< a brightness factor to apply to the color map. This factor is applied to the colors in the colormap and the mask colors*/
} SUMA_SCALE_TO_MAP_OPT;

/*! structure containing the color mapping of a vector */
typedef struct {
	float **cM; /*!< N_Node x 3 matrix containing the colors at each node*/
	int N_Node; /*!< obvious */
	SUMA_Boolean *isMasked; /*!< if isMasked[i] then node i has a mask color associated with it */ 
} SUMA_COLOR_SCALED_VECT;

/*! 
Stucture to hold the contents of the specs file 
*/
typedef struct {
	char SurfaceType[SUMA_MAX_N_SURFACE_SPEC][100];
	char SurfaceFormat[SUMA_MAX_N_SURFACE_SPEC][100]; 
	char SureFitTopo[SUMA_MAX_N_SURFACE_SPEC][1000];
	char SureFitCoord[SUMA_MAX_N_SURFACE_SPEC][1000];
	char MappingRef[SUMA_MAX_N_SURFACE_SPEC][1000];
	char SureFitVolParam[SUMA_MAX_N_SURFACE_SPEC][1000];
	char FreeSurferSurface[SUMA_MAX_N_SURFACE_SPEC][1000];
	char InventorSurface[SUMA_MAX_N_SURFACE_SPEC][1000];
	char *IDcode[SUMA_MAX_N_SURFACE_SPEC];
	char State[SUMA_MAX_N_SURFACE_SPEC][100];
	char Group[SUMA_MAX_N_SURFACE_SPEC][1000];
	int N_Surfs;
	int N_States;
	int N_Groups;
	char StateList[SUMA_MAX_N_SURFACE_SPEC*100];
} SUMA_SurfSpecFile;

/*! structure that containing node's first order neighbors */
typedef struct {
	int N_Node; /*!< Number of nodes whos neighbors are listed in this structure */
	int *NodeId; /*!< Id of each node whos neighbors are listed in this structure */
	int **FirstNeighb; /*!< N_Node x N_Neighb_max matrix with each row specifying the indices of neighboring nodes */
	int *N_Neighb; /*!< maximum number of neighbors for a particular node */
	int N_Neighb_max; /*!< maximum number of neighbors of all nodes */
} SUMA_NODE_FIRST_NEIGHB;

/*! structure that contains faceset's first order neighbors */
typedef struct {
	int N_FaceSet; /*!< Number of nodes whos neighbors are listed in this structure */
	int **FirstNeighb; /*!< N_Node x N_Neighb_max matrix with each row specifying the indices of neighboring facesets */
	int *N_Neighb; /*!< maximum number of neighbors for a particular faceset */
	int N_Neighb_max; /*!< maximum number of neighbors of all facesets */
	int N_Neighb_min; /*!< minimum number of neighbors of all facesets */
} SUMA_FACESET_FIRST_EDGE_NEIGHB;

/*!
	structure containing surface curvature parameters
*/
typedef struct {
	int N_Node; /*!< Number of nodes in the surface */
	float **T1; /*!< N_Node x 3 matrix with each row specifying the 1st principal direction of the surface */
	float **T2; /*!< N_Node x 3 matrix with each row specifying the 2nd principal direction of the surface */
	float *Kp1; /*!< N_Node x 1 vector with each row specifying the curvature along the 1st principal direction */
	float *Kp2; /*!< N_Node x 1 vector with each row specifying the curvature along the 2nd principal direction */
	int N_SkipNode; /*!< number of nodes for which the curvature could not be computed */
} SUMA_SURFACE_CURVATURE;


/*! 
	structure containing the edges that make up a triangular faceset list

*/
typedef struct {
	int ** EL; /*!< pointer to where the Edge List ( N_EL x 2 ) will be placed
								each row is an edge, i1 i2 where i1 is always <= i2
								EL is sorted by row */
	int ** ELps; /*!< pointer to where the Edge List Property matrix ( N_EL x 2 )will be placed 
								1st column, row i = 1 means edge i: i1,i2 was encountered as i2->i1 in the triangle J (so it was flipped when stored in EL)
														= -1 means edge i: i1,i2 was encountered as i1->i2 in the triangle J (so no flipping was done to store it in EL)
								2nd column, row i = J is the triangle ( FL[J] ) that the segment belongs to. 
								3rd column, row i = Numer of triangles that contain this edge. This number is positive for the first occurence
								of the edge in EL, it is -1 afterwards. A decent edge has 2 hosting triangles, an edge edge
								has 1 hosting triangle. Bad edges come in all other colors*/
								
	int *ELloc; /*!< k x 1 vector that allows one to find a certain edge in EL. To locate edge [a, b], find the min of a,b (say b)
                 and the first occurrence of the edge is at EL(ELloc(b),:). 
                 k is usually N_Node of the surface. Nodes between 0 & k-1 not existing in FaceSetList will have a value of -1 
					NOTE: ELloc contains an entry for each node in FaceSetList, except the largest node index since that's never in the 
					first column of EL*/
	int N_EL; /*!< Number of segments = 3 * N_Facesets */
   int max_N_Hosts; /*!< Maximum number of triangle hosts any one edge has (max ( ELps(:,2) != -1 ) )*/
	int  min_N_Hosts; /*!< Minimum version of max_N_Hosts */
	
	int **Tri_limb; /*!< each row j of Tri_limb contains the indices into EL (and ELps) of the edges that make it up */
} SUMA_EDGE_LIST;

/*! structure that contains array pointers from function SUMA_isinbox */
#define SUMA_isinbox_struct
typedef struct {
	int *IsIn; /*!< Indices of nodes inside the box */
	int nIsIn; /*!< Number of nodes inside the box */
	float *d; /*!< Distance of each node to the center of the box */
	float **dXYZ; /*!< Not implemented */
} SUMA_ISINBOX;

/*! structure that contains array pointers from function isinsphere */
#define SUMA_isinsphere_struct
typedef struct {
	int *IsIn;  /*!< Indices of nodes inside the sphere */
	int nIsIn; /*!< Number of nodes inside the sphere */
	float *d;  /*!< Not implemented Distance of each node to the center of the shpere */
	float **dXYZ; /*!< Not implemented */
} SUMA_ISINSPHERE;

/*! Displayable Object Type */
typedef struct {
	void *OP;	/*<! Object Pointer */
	SUMA_DO_Types ObjectType; /*<! Type of displayable object */
	SUMA_DO_CoordType CoordType; /*<! Type of coordinate system that the object is attached to
												This is used to determine whether the object is drawn before or 
												or after the shift and rotation matrices are applied */
} SUMA_DO;

/*! structure containg X vars */
typedef struct {
	Display *DPY;
	XtAppContext APP;
	Widget TOPLEVEL, FORM, FRAME, GLXAREA, Wd;
	XVisualInfo *VISINFO;
	GLXContext GLXCONTEXT;
	Colormap CMAP;
	Bool DOUBLEBUFFER;
	int REDISPLAYPENDING;
	int WIDTH, HEIGHT;
	XtWorkProcId REDISPLAYID, MOMENTUMID;
}SUMA_X;

/*! filename and path */
typedef struct {
	char *Path;
	char *FileName;
}SUMA_FileName;


/*! structure defining a cross hair */
typedef struct {	
	GLfloat XaxisColor[4] ;
	GLfloat YaxisColor[4] ;
	GLfloat ZaxisColor[4] ;
	
	GLfloat LineWidth;
	SUMA_STIPPLE Stipple; /*!< dashed or solid line */

	GLfloat c[3]; /*!< Cross Hair center */
	GLfloat r; /*!< Cross Hair radius */
	GLfloat g; /*!< 1/2 of gap between center and ray (should be less than radius/2) */
	
	SUMA_Boolean ShowSphere; /*!< YUP/NOPE, starting to regret this. */
	GLUquadricObj *sphobj; /*!< quadric object, representing central sphere */
	GLfloat sphcol[3]; /*!< Sphere color */
	GLdouble sphrad; /*!< Sphere radius */
	GLint slices; /*!< think pizza */
	GLint stacks; /*!< think lattitudes */
	
	int SurfaceID; /*!< If the cross hair is tied to a surface, SurfaceID contains the index into SUMAg_DOv of that surface. -1 if that cross hair is wild and loose */
	int NodeID; /*!< a node from SurfaceID can be associated with the cross hair (-1 for nothing) */
}SUMA_CrossHair;	

typedef struct {		
	GLUquadricObj *sphobj; /*!< quadric object, representing central sphere */
	GLfloat sphcol[3]; /*!< Sphere color */
	GLdouble sphrad; /*!< Sphere radius */
	GLint slices; /*!< think pizza */
	GLint stacks; /*!< think lattitudes */
	GLfloat c[3]; /*!< center of Sphere Marker */
}SUMA_SphereMarker;	

typedef struct {
	GLfloat n0[3]; /*!< Node 1 XYZ*/
	GLfloat n1[3]; /*!< Node 2 XYZ*/
	GLfloat n2[3]; /*!< Node 3 XYZ*/
	GLfloat LineWidth; /*!< LineWidth of Edge*/
	GLfloat LineCol[3]; /*!< LineColor of Edge*/
	GLfloat NormVect[3]; /*!< normal vector of faceset, two triangles are drawn at a small distance from the selected FaceSet */
}SUMA_FaceSetMarker;

/*! Structure containing the communication info and status with AFNI */
typedef struct {
	SUMA_Boolean Connected;	/*!< flag indicating connection state */
	int ConSock;
	
} SUMA_AfniCom;

/* structure defining the former state of a surface viewer window */
typedef struct {
	int N_DO;		/*!< Total number of surface objects registered with the viewer */
	int *ShowDO; 	/*!< ShowSO[i] (i=0..N_DO) contains Object indices into DOv for DOs visible in the surface viewer*/
	float ViewFrom[3]; /*!< Location of observer's eyes */
	float ViewFromOrig[3]; /*!< Original Location of observer's eyes */
	float ViewCenter[3];	/*!< Center of observer's gaze */
	float ViewCenterOrig[3];	/*!< Original Center of observer's gaze */
	float ViewCamUp[3];	/*!< Camera Up direction vector */
	float ViewDistance; /*!< Viewing distance */
	float FOV; /*!< Field of View (affects zoom level)*/
	float Aspect;	/*!< Aspect ratio of the viewer*/
} SUMA_ViewState_Hist;


/*! structure defining the viewing state of the viewer window */
typedef struct {
	char *Name; /*!< The name of the viewing state, fiducial, inflated, etc .. */
	int *MembSOs; /*!< Indices into DOv of SOs that are members of the viewing state */
	int N_MembSOs; /*!< Number of members in MembSOs. Only SOs that are in MembSOs can
							be placed into ShowDO of the viewer in a particular viewing state.*/						
	SUMA_ViewState_Hist *Hist; /*!< Pointer to structure containing various parameter settings for that viewing state */				
} SUMA_ViewState;

/*! structure defining the state of a viewer window */
typedef struct {
	float ViewFrom[3]; /*!< Location of observer's eyes */
	float ViewFromOrig[3]; /*!< Original Location of observer's eyes */
	float ViewCenter[3];	/*!< Center of observer's gaze */
	float ViewCenterOrig[3];	/*!< Original Center of observer's gaze */
	float ViewCamUp[3];	/*!< Camera Up direction vector */
	float ViewDistance; /*!< Viewing distance */
	float FOV; /*!< Field of View (affects zoom level)*/
	float Aspect;	/*!< Aspect ratio of the viewer*/
	
	int translateBeginX; /*!< User Input (mouse) X axis current position for translation */
	int translateBeginY; /*!< User Input (mouse) Y axis current position for translation */
	float translateDeltaX;	/*!< User Input (mouse) X axis position increment for translation */
	float translateDeltaY;	/*!< User Input (mouse) Y axis position increment for translation */
	float TranslateGain;	/*!< gain applied to mouse movement */
	float ArrowtranslateDeltaX;	/*!< User Input (Keyboard) X axis position increment for translation */
	float ArrowtranslateDeltaY;	/*!< User Input (Keyboard) X axis position increment for translation */
	GLfloat translateVec[2];		/*!< translation vector, in screen coordinates, equal to [translateDeltaX translateDeltaY]. The third translation (Z dimension) is 0.0*/
	GLfloat RotaCenter[3];	/*!<Center of Rotation */
	float zoomDelta;	 	/*!< Zoom increment */
	float zoomBegin; 	/*!< Current zoom level*/
	int spinDeltaX;				/*!< User Input (mouse) X axis position increment for spinning*/
	int spinDeltaY;				/*!< User Input (mouse) Y axis position increment for spinning*/
	int spinBeginX;				/*!< User Input (mouse) X axis current position for spinning */
	int spinBeginY;				/*!< User Input (mouse) Y axis current position for spinning */
	int MinIdleDelta; 		/*!< minimum spinDeltaX or spinDeltaY to initiate momentum rotation */
	float deltaQuat[4];	/*!< Quaternion increment */
	float currentQuat[4]; /*!< Current quaternion */
	Boolean ApplyMomentum;	/*<! Turn momentum ON/OFF */

	short verbose;	/*!< Verbosity of viewer */

	GLfloat light0_position[4]; /*!< Light 0 position: 1st 3 vals --> direction of light . Last value is 0 -->  directional light*/
	GLfloat light1_position[4]; /*!< Light 1 position: 1st 3 vals --> direction of light. Last value is 0 -->  directional light*/
	
	int WindWidth;	/*!< Width of window */
	int WindHeight;	/*!< Height of window */
	
	int N_DO;		/*!< Total number of surface objects registered with the viewer */
	int *ShowDO; 	/*!< ShowSO[i] (i=0..N_DO) contains Object indices into DOv for DOs visible in the surface viewer*/
	
	int ShowEyeAxis ; /*!< ShowEyeAxis */
	int ShowMeshAxis; /*!< ShowEyeAxis */
	int ShowCrossHair; /*!< ShowCrossHair */
	
	int Focus_SO_ID; /*!< index into SUMAg_DOv of the surface currently in focus, -1 for nothing*/
	int Focus_DO_ID; /*!< index into SUMAg_DOv of the Displayabl Object currently in focus -1 for nothing*/
	
	SUMA_X *X; /*!< structure containing X widget midgets */

	GLdouble Pick0[3];	/*!< Click location in World coordinates, at z = 0 (near clip plane)*/
	GLdouble Pick1[3];	/*!< Click location in World coordinates, at z = 1.0 (far clip plane)*/
	
	SUMA_CrossHair *Ch; /*!< Pointer to Cross Hair structure */
	
	SUMA_Boolean TalkToAfni; /*!< YUP = communicate certain actions to AFNI */
	NI_stream ns; /*!< Yaking niml stream */
	
	SUMA_ViewState *VSv; /*!< Vector of Viewing State Structures */
	int N_VSv; /*!< Number of Viewing State structures */
	char *State; /*!< The current state of the viewer. This variable should no be freed since it points to locations within VSv*/
	
	int PolyMode; /*!< polygon viewing mode, 0, filled, 1, outline, 0, points */
	SUMA_Boolean BF_Cull; /*!< flag for backface culling */
}SUMA_SurfaceViewer;

/*! structure defining an EngineData structure */
typedef struct {
	float fv3[3]; /*!< Float vector, 3 values */
	int fv3_Dest; /*!< float3 vector destination */
	int fv3_Source; /*!< float3 vector source */
	
	int iv3[3];		/*!< Integer vector, 3 values */
	int iv3_Dest;  /*!< Integer3 vector destination */
	int iv3_Source;  /*!< Integer3 vector source */
	
	float fv15[15]; /*!< Float vector, 15 values */
	int fv15_Dest; /*!< float15 vector destination */
	int fv15_Source; /*!< float15 vector source */
	
	float iv15[15];/*!< Integer vector, 15 values */
	int iv15_Dest;/*!< Integer15 vector destination */
	int iv15_Source; /*!< Integer15 vector source */
	
	int i;		/*!< integer */
	int i_Dest;	/*!< integer destination */
	int i_Source; /*!< integer source */
	
	float f; /*!< float, ingenious ain't it! */
	int f_Dest; /*!< float destination */
	int f_Source; /*!< float source */
	
	char s[SUMA_MAX_STRING_LENGTH]; /*!< string */
	int s_Dest; /*!< string destination */
	int s_Source; /*!< string source */
	
	float **fm; /*!< float matrix pointer */
	SUMA_Boolean fm_LocalAlloc; /*!< Locally allocated matrix pointer ? (if it is then it is freed in SUMA_ReleaseEngineData ) */
	int fm_Dest; /*!< destination of fm */
	int fm_Source; /*!< source of fm*/
	
	int **im; /*!< Same dance as fm but for integers */
	SUMA_Boolean im_LocalAlloc;
	int im_Dest;
	int im_Source; /*!< source of im */
	
	int N_rows;
	int N_cols;
	
} SUMA_EngineData;
 
/*! structure defininf an axis object */
typedef struct {
	GLfloat XaxisColor[4] ;
	GLfloat YaxisColor[4] ;
	GLfloat ZaxisColor[4] ;
	
	GLfloat LineWidth;
	SUMA_STIPPLE Stipple; /*!< dashed or solid line */
	
	GLfloat XYZspan[3]; /*!< the axis will span +/- span[i] in the three dimensions */
	GLfloat Center[3]; /*!< origin of axis */
	char *Name; /*!< name of axis */
	char *idcode_str; /*! idcode of axis */
}SUMA_Axis;


/*! structure that contains the output of SurfNorm function */
#define SUMA_SurfNorm_struct
typedef struct {
	int N_Node; /*!< Number of nodes, 1st dim of Node_NormList*/
	int N_Face;/*!< Number of facesets, 1st dim of Face_NormList*/
	float **Face_NormList ; /*!< N_Face x 3 matrix containing normalized normal vectors for each triangular faceset*/ 
	float **Node_NormList ; /*!< N_Node x 3 matrix containing normalized normal vectors for each node*/
} SUMA_SURF_NORM; /*!< structure that contains the output of SurfNorm function */

/*! structure that contains the output of SUMA_MemberFaceSets function */
#define SUMA_MemberFaceSets_struct
typedef struct {
	int N_Memb_max;/*!< Maximum number of Facesets any node belonged to*/
	int Nnode; /*! Total number of nodes examined (0..Nnode-1) */
	int **NodeMemberOfFaceSet ; /*!< Nnode x N_Memb_max matrix containing for each row i, the indices of the facesets containing node i*/ 
	int *N_Memb ; /*!< Nnode x 1 vetor containing for each node i, the number of facesets that contain it*/
} SUMA_MEMBER_FACE_SETS; /*!< structure that contains the output of SurfNorm function */


/*! structure containing results of intersection of a ray with triangles */
typedef struct {
	int N_el; /*!< Number of elements in each vector */
	SUMA_Boolean *isHit;	/*!< Is the triangle hit ? */
	float *t;	/*!< Distance from ray source to triangle */
	float *u;	/*!< location of intersection in triangle in Barycentric coordinates, V0P = u V0V1 + vV0V2*/
	float *v;	/*!< location of intersection in triangle */
	int ifacemin; /*!< index of the faceset closest to the ray's origin */
	int ifacemax; /*!< index of the faceset farthest from the ray's origin */
	int N_hits; /*!< Number of intersections between ray and surface */
	float P[3]; /*!< XYZ of intersection with ifacemin */
	float d; /*!< Distance from the closest node in ifacemin to P */
	int inodemin; /*!< node index (into NodeList)that is closest to P */
	int inodeminlocal; /*!< node in FaceSet[ifacemin] that is closest to P, 
						inodemin = FaceSet[ifacemin][inodeminlocal]*/
} SUMA_MT_INTERSECT_TRIANGLE;

/*! Structure defining the surface's volume parent info */
typedef struct {
	int isanat; /*!< 1 if parent volume is of type anat */
	int nx, ny, nz; /*!< number of voxels in the three dimensions */
	float dx, dy, dz; /*!< delta x, y, z in mm */
	float xorg, yorg, zorg; /*!< voxel origin in three dimensions */
	char *prefix; /*!< parent volume prefix */
	char *filecode; /*!< parent volume prefix + view */
	char *dirname; /*!< parent volume directory name */
	char *idcode_str; /*!< idcode string*/
	char *idcode_date; /*!< idcode date */
	int xxorient, yyorient, zzorient; /*!< orientation of three dimensions*/ 
	float *VOLREG_CENTER_OLD; /*!< pointer to the named attribute (3x1) in the .HEAD file of the experiment-aligned Parent Volume */
	float *VOLREG_CENTER_BASE; /*!< pointer to the named attribute (3x1) in the .HEAD file of the experiment-aligned Parent Volume */
	float *VOLREG_MATVEC; /*!< pointer to the named attribute (12x1) in the .HEAD file of the experiment-aligned Parent Volume */
} SUMA_VOLPAR;

/*! structure defining a Surface Object */
typedef struct {
	SUMA_SO_File_Type FileType; /*!< Type of Surface file */
	SUMA_SO_File_Format FileFormat; /*!< Format of Surface file ascii or binary*/
	
	SUMA_FileName Name; /*!< Directory and Name of surface object file (SO) */
	SUMA_FileName Name_coord; /*!< Directory and Name of surface coordinate file (for SureFit files) */
	SUMA_FileName Name_topo; /*!< Directory and Name of surface topology file  (for SureFit files)*/
	char *idcode_str; /*!< string containing the idcode of the surface */
	
	SUMA_Boolean SUMA_VolPar_Aligned; /*!< Surface aligned to Parent Volume data sets ?*/
	SUMA_Boolean VOLREG_APPLIED; /*!< YUP if VP->VOLREG_CENTER_BASE, VP->VOLREG_CENTER_OLD, VP->VOLREG_MATVEC were successfully applied*/
	
	int N_Node; /*!< Number of nodes in the SO */
	int NodeDim; /*!< Dimension of Node coordinates, 2 for 2D (Z = 0), 3 for 3D */
	float **NodeList; /*!< N_Node x 3 matrix containing the XYZ node coordinates. 
								If NodeDim is 2 then the third column is all zeros*/
	char *MapRef_idcode_str; /*!< if NULL, then it is not known whether surface is mappable or not
	                              if equal to idcode_str then surface surface is Mappable, 
											otherwise it specifies the idcode of the Mapping reference surface */
	
	int N_FaceSet; /*!< Number of polygons defining the surface  */
	int FaceSetDim; /*!< Number of sides on the polygon */
	int **FaceSetList; /*!< N_FaceSetList x FaceSetDim matrix describing the polygon set that makes up the SO.
							Each row contains the indices (into NodeList) of the nodes that make up a polygon */
	
	float **NodeNormList ; /*!< N_Node x 3 matrix containing normalized normal vectors for each node*/
	float **FaceNormList ; /*!< N_FaceSet x 3 matrix containing normalized normal vectors for each polygon*/ 
	
	float Center[3]; 		/*!< The centroid of the surface */
	float MaxDims[3];		/*!< The maximum along each of the XYZ dimensions */
	float MinDims[3];		/*!< The minimum along each of the XYZ dimensions */
	float aMinDims;		/*!< The maximum across all dimensions*/
	float aMaxDims;		/*!< The minimum across all dimensions*/
	
	int RotationWeight; /*!< Contribution to center of rotation calculation. 
									set to 0 if not contributing.
								 	set to N_Node to have the number of nodes weigh into the center's location, center of mass effect
									set to 1 to give each object equal weight */
	int ViewCenterWeight; /*!< Contribution to center of gaze and viewfrom location */
	
	char *Name_NodeParent; /*!< Node parent of the SO.	Node Indices of SO are into NodeList matrix of the NodeParent SO*/					

	GLfloat *glar_NodeList;			/*!< pointer to the 1D NodeList array*/
	GLuint  *glar_FaceSetList;		/*!< pointer to the 1D FaceSetList array*/
	GLfloat *glar_FaceNormList;	 /*!< pointer to the 1D glar_FaceNormList array*/
	GLfloat *glar_ColorList; 		/*!< pointer to the 1D ColorList array*/
	GLfloat *glar_NodeNormList; 	/*!< pointer to the 1D NodeNormList array*/
	
	SUMA_Boolean ShowMeshAxis; /*!< flag to show Mesh Axis if it is created */
	SUMA_Axis *MeshAxis;	/*!< pointer to XYZ axis centered on the surface's centroid */
	
	SUMA_MEMBER_FACE_SETS *MF; /*!< structure containing the facesets representing each node */
	SUMA_NODE_FIRST_NEIGHB *FN; /*!< structure containing the first order neighbors of each node */
	SUMA_EDGE_LIST *EL; /*!< structure containing the edge list */
	float *PolyArea; /*!< N_FaceSet x 1 vector containing the area of each polygon in FaceSetList */
	SUMA_SURFACE_CURVATURE *SC; /*!< Structure containing the surface curvature info */
	
	float *Cx; /*!< vector containing surface convexity at each node */
	SUMA_INODE *Cx_Inode; /*!< Inode structure for Cx */
	/* selection stuff */
	SUMA_Boolean ShowSelectedNode; /*!< flag for obvious reasons */
	int SelectedNode; /*!< index of one selected node, -1 if no node is selected */
	SUMA_SphereMarker *NodeMarker; /*!< Node Marker object structure*/
	
	SUMA_Boolean ShowSelectedFaceSet; /*!< you know what I mean */
	int SelectedFaceSet; /*!< index of one selected faceset, -1 if no faceset is selected */
	SUMA_FaceSetMarker *FaceSetMarker; /*!< Aha, I hear ya */
	
	SUMA_VOLPAR *VolPar; /*!< Parent Volume structure */
	
	char *Group;	/*!< Group the surface belongs to, like Simpsons H. */
	char *State; /*!< State of SO (like inflated, bloated, exploded) */
	
	SUMA_OVERLAYS **Overlays; /*!< vector of pointers to color overlay structures */
	SUMA_INODE **Overlays_Inode; /*!< vector of pointers to Inodes corresponding to each Overlays struct */
	int N_Overlays; /*!< number of pointers to overlay structures */

	float Back_Modfact; /*!< Factor to apply when modulating foreground color with background intensity
									background does not modulate foreground, 
									Color = Fore * avg_Bright * AttenFactor; (w/ 0 <= avg_Bright <=1)
									a good setting is such that SUMA_BACKGROUND_ATTENUATION_FACTOR * SUMA_DIM_AFNI_COLOR_FACTOR = 1
									 Watch for saturation effects!  */
									 
}SUMA_SurfaceObject; /*!< \sa Alloc_SurfObject_Struct in SUMA_DOmanip.c
							\sa SUMA_Free_Surface_Object in SUMA_Load_Surface_Object.c
							\sa SUMA_Print_Surface_Object in SUMA_Load_Surface_Object.c
							\sa SUMA_Load_Surface_Object in SUMA_Load_Surface_Object.c
					*/		



/*! structure containing SureFit Surface*/
typedef struct {
	/* coord files */
	char name_coord[SUMA_MAX_NAME_LENGTH];
	int N_Node; /*!< Number of nodes */
	float **NodeList; /*!< N_Node x 3 matrix containing node coordinates */
	int *NodeId; /*!< Node ID, that's normaly from 0..N_Nodes-1 but since it's in .coord file, I keep it anyway */
	char encoding_coord[100];
	char configuration_id[100];
	char coordframe_id[100];
	/* Topo files */
	char name_topo[SUMA_MAX_NAME_LENGTH];
	char encoding_topo[100];
	char date[100];
	char perimeter_id[100];
	int N_Node_Specs; /*!< Number of nodes with listed node specs */
	int **Specs_mat; /*!< Node Specs matrix. Columns appear to be arraged as such NodeId #Neighbors ? ? NodeId ? */
	SUMA_NODE_FIRST_NEIGHB FN; /*!< First order neighbor structure */
	int N_FaceSet; /*!< Number of polygons making up surface */
	int **FaceSetList; /*!< definition of polygons */
	/* Param Files */
	char name_param[SUMA_MAX_NAME_LENGTH];
	float AC_WholeVolume[3]; /*!< XYZ (from .Orient.params file) of Anterior Comissure of whole volume */
	float AC[3]; /*!< XYZ of Anterior Comissure of cropped volume */
} SUMA_SureFit_struct;

/* structure containing FreeSurfer Surface */
typedef struct {
	char name[SUMA_MAX_NAME_LENGTH];
	int N_Node; /*!< Number of nodes */
	int *NodeId; /*!< Node ID, that's normaly from 0..N_Nodes-1 unless the surface is a patch of another surface see FaceSetIndexInParent*/
	float **NodeList; /*!< N_Node x 3 matrix containing node coordinates */
	int N_FaceSet; /*!< Number of polygons making up surface */
	int **FaceSetList; /*!< definition of polygons. For a complete surface, these are indices into NodeList's rows
	                        For a patch, these are indices into NodeList of the parent surface*/
	char comment[SUMA_MAX_STRING_LENGTH]; /*!< comment at beginning of patch or surface */
	SUMA_Boolean isPatch; /*!< is the surface a patch of another ? */
	int *FaceSetIndexInParent; /*!< for a FaceSet in patch, this represents its index in FaceSetList of the parent surface.
												This is the FaceSet equivalent of NodeId*/ 
} SUMA_FreeSurfer_struct;

/* structure containing SureFit name*/
typedef struct {
	char name_coord[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH];
	char name_topo[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH]; 
	char name_param[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH];
} SUMA_SFname;

/*! structure containing information global to all surface viewers */
typedef struct {
	char AfniHostName[SUMA_MAX_NAME_LENGTH]; /*!< name or ipaddress of afni host maximum allowed name is 20 chars less than allocated for, see SUMA_Assign_AfniHostName*/ 
	char NimlAfniStream[SUMA_MAX_NAME_LENGTH]; /*!< niml stream name for communicating with afni */
} SUMA_CommonFields;
#endif
