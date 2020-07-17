#ifndef SUMA_DEFINE_INCLUDED
#include "TrackIO.h"
#define SUMA_DEFINE_INCLUDED


#define SUMA_DEF_GROUP_NAME "DefGroup"
#define SUMA_DEF_TOY_GROUP_NAME "DeDe"
#define SUMA_DEF_STATE_NAME "Default_state"

#define SUMA_SUMA_NIML_DEBUG 0
#define SUMA_SEPARATE_SURF_CONTROLLERS 0 /*!< 0 if you want surfaces sharing the same LocalDomainParent 
                                                to use the same controller. 
                                                If you choose 1, then the controllers will not be linked
                                                and there will be no parameter update for that colorplane
                                                in the other controllers. The problem is that every little
                                                callback will have to make a call to SUMA_UpdateColPlaneShellAsNeeded
                                                and that's a pain, to say the least.*/

#define ARRAY 1
#define STRAIGHT 2
#define TRIANGLES 1
#define POINTS 2

#define DRAW_METHOD ARRAY
#define RENDER_METHOD TRIANGLES
#define DO_MESH
#define DO_MESH_AXIS
/*#define ZERO_CENTER*/

#define SUMA_DOUBLE_CLICK_MAX_DELAY 250 /*!< Maximum delay in ms to consider a double click */

#define NODE_COLOR_R 0.35
#define NODE_COLOR_G 0.35
#define NODE_COLOR_B 0.35
#define SUMA_GRAY_NODE_COLOR 0.30
#define SUMA_DIM_AFNI_COLOR_FACTOR 0.5 /*!< 0.4 works well, use higher factors for flashiness scaling factor (0..1) applied to afni's rgb colors, lower values help retain surface shape info */
#define SUMA_AFNI_COLORPLANE_OPACITY 1
#define SUMA_DIM_CONVEXITY_COLOR_FACTOR 0.5
#define SUMA_CONVEXITY_COLORPLANE_OPACITY 0.8
#define SUMA_BACKGROUND_MODULATION_FACTOR 3   /*!< 0 background does not modulate         foreground, Color = Fore * avg_Bright * AttenFactor (0 <= avg_Bright <=1)
        a good setting is such that 
        SUMA_BACKGROUND_ATTENUATION_FACTOR * SUMA_DIM_AFNI_COLOR_FACTOR = 1
        Watch for saturation effects!*/

#define SUMA_MAT_SHININESS_INIT 0 /*!< Surface object shininess, 0 20, 50 .. 128*/
#define SUMA_MAT_SPECULAR_INIT    0.0, 0.0, 0.0, 1.0 /*!< The specular color of the material, keep this and the exponent (that's MAT_SHININESS) 0 to keep shininess down*/
#define SUMA_MAT_AMBIENT_INIT    0.2, 0.2, 0.2, 1.0 /*!< Fraction of Ambient light reflected.Ambient light has an undetermined direction and is scattered equally in all directions */
#define SUMA_MAT_DIFFUSE_INIT    0.8, 0.8, 0.8, 1.0 /*!<  Fraction of Diffuse light reflected.Diffuse light comes from one direction, but is scattered equally in all directions and appears equally bright no matter where the eye is located*/
#define SUMA_MAT_EMISSION_INIT    0.0, 0.0, 0.0, 1.0 /*!< Emissive color/light emanated from object.
                                                           and unaffected by light sources. 
                                                           It adds no light to other objects in the scene */

#define SUMA_LMODEL_AMBIENT_COLOR_R   1.0       /*!< keep the ambient light high */
#define SUMA_LMODEL_AMBIENT_COLOR_G   1.0
#define SUMA_LMODEL_AMBIENT_COLOR_B   1.0
#define SUMA_LMODEL_AMBIENT_COLOR_A   1.0
#define SUMA_LMODEL_AMBIENT       SUMA_LMODEL_AMBIENT_COLOR_R, SUMA_LMODEL_AMBIENT_COLOR_G, SUMA_LMODEL_AMBIENT_COLOR_B,  SUMA_LMODEL_AMBIENT_COLOR_A /*!< keep the ambient light high */

#define SUMA_RED_GL 1.0, 0.0, 1.0, 1.0 /*!< red color */
#define SUMA_YELLOW_GL 0.0, 1.0, 1.0, 1.0 /*!< yellow color */
#define SUMA_BLUE_GL 0.0, 0.0, 1.0, 1.0 /*!< yellow color */
#define SUMA_GREEN_GL 0.0, 1.0, 0.0, 1.0 /*!< yellow color */

#define SUMA_CLEAR_COLOR_R         0.0 /*!< clear color (viewer background) Red */
#define SUMA_CLEAR_COLOR_G         0.0 /*!< clear color (viewer background) Green */
#define SUMA_CLEAR_COLOR_B         0.0 /*!< clear color (viewer background) Blue */
#define SUMA_CLEAR_COLOR_A         0.0 /*!< clear color (viewer background) Alpha */


#define SUMA_BACKFACE_CULL 0 /*!< 1/0 flag for culling backface facesets */
#define SUMA_CHECK_WINDING 0 /*!< 1/0 flag for checking triangle winding */

#define SUMA_LIGHT0_COLOR_R   1.0
#define SUMA_LIGHT0_COLOR_G   1.0
#define SUMA_LIGHT0_COLOR_B   1.0
#define SUMA_LIGHT0_COLOR_A   1.0
#define SUMA_LIGHT0_COLOR_INIT    SUMA_LIGHT0_COLOR_R, SUMA_LIGHT0_COLOR_G, SUMA_LIGHT0_COLOR_B,  SUMA_LIGHT0_COLOR_A 
#define SUMA_INTITIAL_LIGHT0_SWITCH 1 /*!< -1 works well for SureFit Surfaces, 1 works well for iv and FreeSurfer surfaces */
#define SUMA_STDERR stderr
#define SUMA_STDOUT stdout

#define SUMA_CROSS_HAIR_LINE_WIDTH 1.5
#define SUMA_CROSS_HAIR_RADIUS 6
#define SUMA_CROSS_HAIR_GAP 2
#define SUMA_CROSS_HAIR_SPHERE_RADIUS 0.5
#define SUMA_SELECTED_NODE_SPHERE_RADIUS 0.25

#define SUMA_BEEP_LENGTH_MS 50 /*!< beep time in ms */
#define SUMA_XYZ_XFORM_BOXDIM_MM 5 /*!< search box width (in mm) used to change XYZ to the closest node index. Keep this one small, 5 mm works for me. Otherwise you may get thrown way off of where you should be. It is no guarantee that the closest node is part of the faceset you are looking at*/
#define SUMA_SELECTED_FACESET_LINE_WIDTH 2 /*!< Line Width of highlighting triangles */
#define SUMA_SELECTED_FACESET_OFFSET_FACTOR 0.01 /*!< highlighting is done by drawing two triangles at a fractional distance of the normal vector */
#define SUMA_SELECTED_FACESET_LINE_INTENSITY 0.75 /*!< line gray color intensity */
#define SUMA_NODE_ALPHA 1 /*!< Node Color Intensity 1, max intensity 0 min intensity*/
#define FOV_INITIAL 30.0  /*!< Leave this at 30 always, change defaults from .sumarc's SUMA_FOV_Original */
#define FOV_MIN 0.001
#define FOV_MAX 179
#define FOV_IN_FACT 1.05   /*!< Still used for colormap display, 
                           Obsolete for surface viewers, see sv->KeyZoomGain */
#define FOV_OUT_FACT 0.95  /*!< Still used for colormap display, 
                           Obsolete for surface viewers, see sv->KeyZoomGain */
#define MOUSE_ZOOM_FACT 30 /*!< The larger, the slower the gain on mouse movement */
#define TRANSLATE_GAIN 50 /*!< between 40 and 80 */
#define ARROW_TRANSLATE_DELTAX 30
#define ARROW_TRANSLATE_DELTAY 30
#define ARROW_ROTATION_ANGLE_DEG 15 
#define SUMA_MAX_N_GROUPS 100 /*!< Maximum number of surface groups */
#define SUMA_MAX_MESSAGES 100 /*!< Maximum number of messages stored in list */
#define SUMA_MAX_MEMBER_FACE_SETS 110 /*!< Maximum number of facesets a node can 
                                          be part of. Used to be 60 but that was 
                                          not enough for a few
                                          funky FS surfaces. ZSS Mon Mar 24 
                                          16:14:12 EST 2003*/
#define SUMA_MAX_FACESET_EDGE_NEIGHB 3 /*!< Maximum number of adjoining FaceSets 
                                            a triangular faceset can have.*/
#define SUMA_MAX_DISPLAYABLE_OBJECTS 1000 /*!< Max # of displayable Objects */
#define SUMA_MAX_SURF_VIEWERS 10 /*!< Maximum number of surface viewers allowed,
                                     Javier needs 7 ! */
#define SUMA_SURF_CONT_SWITCH_LABEL_LENGTH 40
#define SUMA_DEFAULT_VIEW_FROM 300 /*!< default view from location on Z axis */
#define SUMA_MAX_FP_NAME_LENGTH ( SUMA_MAX_DIR_LENGTH + SUMA_MAX_NAME_LENGTH )
#define SUMA_MAX_COMMAND_LENGTH      2000 /*!< Maximum number of characters in a 
                                               command string */
#define SUMA_MAX_LABEL_LENGTH 300 /*!< Maximum number of characters for labeling 
                                       and naming suma fields and objects */
#define SUMA_MAX_STRING_LENGTH 1000 /*!< Max number of characters in a string */ 
#define SUMA_MAX_COLOR_NAME 50 /*!< Max. length of the name of a color */
#define SUMA_MAX_NUMBER_NODE_NEIGHB   100 /*!< Maximum number of neighbors any 
                                          one node can have.
                                          Used to be 50 but that was not enough 
                                          for a few funky FS surfaces. 
                                          ZSS Mon Mar 24 16:14:12 EST 2003*/
#define SUMA_MAX_OVERLAYS 50 /*!< Maximum number of color overlay planes 
                                  allowed */
#define SUMA_COMMAND_DELIMITER '|'
#define SUMA_COMMAND_TERMINATOR '~'
#define SUMA_PERSPECTIVE_NEAR   1.0   /*!< Z Near, distance from the viewer to 
                                 the near clipping plane (for gluPerspective)*/
#define SUMA_PERSPECTIVE_FAR      900 /*!< Z Far, distance from the viewer to the
                                 far clipping plane (for gluPerspective)*/
#define SUMA_TESSCON_TO_MM       319.7 /*!< The mysterious Tesscon units */
#define SUMA_TESSCON_DIFF_FLAG    1000 /*!< Apply scaling by SUMA_TESSCON_TO_MM
                                 If aMaxDim - aMinDim > SUMA_TESSCON_DIFF_FLAG in
                                 a .iv file */


#define SUMA_MAX_N_SURFACE_SPEC 1000/*!< Max # of surfs allowed in a spec file */
#define SUMA_MAX_N_DO_SPEC 100

#define SUMA_MEMTRACE_BLOCK 10000 /*!< Number of elements to allocate for when 
                                       keeping track of allocated memory. If 
                                       needed more space is reallocated with 
                                       SUMA_MEMTRACE_BLOCK increments. */
#define SUMA_MEMTRACE_FLAG 1    /*!< Flag to turn on(1) or off (0) the 
                                     memory tracing capability */
#define SUMA_PI 3.141592653589793115997963 
#define SUMA_EPSILON 0.000001

#define SUMA_MAX_N_CLIP_PLANES 6 /*!< Limited by maximum nuber of clip planes allowed by OpenGL, assume min of 6*/

/*!
   Debugging flags
*/
#define SUMA_NIML_WORKPROC_IO_NOTIFY 0  /*!< If set to 1 then SUMA_niml_workprocess will send a notification when InOut_Notify is ON
                                          You should keep it off unless you suspect a problem in that function. Otherwise
                                         you'll get many reports from the function making it difficult to see other messages. */
#define SUMA_WORKPROC_IO_NOTIFY 0  /*!< Same as above but for SUMA_workprocess */

typedef enum { SUMA_VOX_NEIGHB_FACE, SUMA_VOX_NEIGHB_EDGE, SUMA_VOX_NEIGHB_CORNER } SUMA_VOX_NEIGHB_TYPES;
typedef enum { SUMA_DONT_KNOW = 0, SUMA_IN_TRIBOX_OUTSIDE = 1, SUMA_INTERSECTS_TRIANGLE_OUTSIDE, 
SUMA_ON_NODE, 
SUMA_INTERSECTS_TRIANGLE_INSIDE, 
SUMA_IN_TRIBOX_INSIDE, 
SUMA_INSIDE_SURFACE } SUMA_SURF_GRID_INTERSECT_OPTIONS; 
                                    
typedef enum { SUMA_GEOM_NOT_SET=-1, SUMA_GEOM_IRREGULAR = 0,    
               SUMA_GEOM_SPHERE = 1, SUMA_GEOM_ICOSAHEDRON, 
               SUMA_N_GEOM } SUMA_GEOM_TYPE;

#define SUMA_IS_GEOM_SYMM(gt) ( ( (gt) == SUMA_GEOM_SPHERE || (gt) == SUMA_GEOM_ICOSAHEDRON ) ? 1:0 ) 

typedef enum  { SUMA_NO_ANSWER, SUMA_YES, SUMA_NO, SUMA_HELP, SUMA_CANCEL, SUMA_YES_ALL, SUMA_NO_ALL, SUMA_WHAT_THE_HELL } SUMA_QUESTION_DIALOG_ANSWER; /* DO NOT CHANGE THE ORDER OF THE FIRST 4 */

typedef enum  { SUMA_FT_ERROR = -1, SUMA_FT_NOT_SPECIFIED, 
               SUMA_FREE_SURFER, SUMA_FREE_SURFER_PATCH, SUMA_SUREFIT, 
               SUMA_INVENTOR_GENERIC, SUMA_PLY, SUMA_VEC, SUMA_CMAP_SO,
               SUMA_BRAIN_VOYAGER , 
               SUMA_OPENDX_MESH, SUMA_BYU, SUMA_GIFTI, SUMA_MNI_OBJ,
               SUMA_PREDEFINED, SUMA_OBJ_MESH, SUMA_STL,
                  SUMA_N_SO_FILE_TYPE} SUMA_SO_File_Type; /* add types always between SUMA_FT_NOT_SPECIFIED AND SUMA_N_SO_FILE_TYPE */
typedef enum { SUMA_FF_ERROR = -1, SUMA_FF_NOT_SPECIFIED, SUMA_ASCII, SUMA_BINARY, SUMA_BINARY_BE, SUMA_BINARY_LE, SUMA_XML_SURF, SUMA_XML_ASCII_SURF,  SUMA_XML_B64_SURF, SUMA_XML_B64GZ_SURF } SUMA_SO_File_Format;


typedef enum { SUMA_COORD_TYPE_ERROR=0, 
                  SUMA_SCREEN, 
                  SUMA_WORLD} SUMA_DO_CoordType; /*!< Coordinate system that 
                          Displayable object that is attached to SCREEN is for a
                          fixed system, WORLD is for a mobile system, ie one 
                          that is rotated by the mouse movements */
typedef enum { SUMA_COORD_UNITS_ERROR=0,
               SUMA_NORM_SCREEN_UNIT, /* XYZ coordinates between [0,1] relative
                                       to the drawing screen X Y are the fixed
                                       eye axes and Z is 1 closest to users and
                                       Z = 0 is all the way back. Z=0.5 is where
                                       the fixed eye axes lies. To display an 
                                       object at such a location, it needs to be
                                       transformed to SUMA_WORLD_UNIT with the 
                                       function SUMA_NormScreenToWorld. If the
                                       latter function is called with only the 
                                       projection matrix set, i.e. before the                                           rotation and translations are applied,
                                       as is done when SUMA_DO_CoordType is set 
                                       to SUMA_SCREEN then the location does not
                                       move with moving surface (mouse 
                                       movements). If SUMA_DO_CoordType is set 
                                       to SUMA_WORLD then the transformed 
                                       location will track moving surface 
                                       because rotation and translation matrices
                                       are taken into account. */
              SUMA_WORLD_UNIT }   SUMA_DO_CoordUnits;                      
          
                                       
typedef enum {SUMA_SOLID_LINE, SUMA_DASHED_LINE} SUMA_STIPPLE;

typedef enum {SUMA_Button_12_Motion, SUMA_Button_2_Shift_Motion, SUMA_Button_1_Motion, SUMA_Button_2_Motion, SUMA_Button_3_Motion} SUMA_MOTION_TYPES; /*!< Types of mouse motion */

typedef enum { SE_Empty, 
               SE_SetLookAt, SE_SetLookFrom, SE_Redisplay, 
               SE_Home, SE_SetNodeColor, 
               SE_FlipLight0Pos, SE_GetNearestNode, SE_SetLookAtNode,
               SE_HighlightNodes, SE_SetRotMatrix, 
               SE_SetCrossHair, SE_ToggleCrossHair, SE_SetSelectedNode, 
               SE_ToggleShowSelectedNode, SE_SetSelectedFaceSet,
               SE_ToggleShowSelectedFaceSet, SE_ToggleConnected, 
               SE_SetAfniCrossHair, SE_SetAfniSurf, SE_SetAfniSurfList, 
               SE_SetAfniThisSurf, SE_SetAfniMask,
               SE_SetForceAfniSurf, SE_BindCrossHair, SE_ToggleForeground, 
               SE_ToggleBackground, SE_FOVreset, SE_CloseStream4All, 
               SE_Redisplay_AllVisible, SE_RedisplayNow, SE_ResetOpenGLState, 
               SE_LockCrossHair, SE_SetGICORnode, 
               SE_ToggleLockAllCrossHair, SE_SetLockAllCrossHair, 
               SE_ToggleLockView, SE_ToggleLockAllViews, 
               SE_Load_Group, SE_Home_AllVisible, SE_Help, SE_Help_Cmap, 
               SE_Help_Plot, SE_Help_Xform, SE_Whereami, SE_Log,
               SE_UpdateLog, SE_SetRenderMode, SE_SetTransMode, SE_OpenDrawROI,
               SE_RedisplayNow_AllVisible, SE_RedisplayNow_AllOtherVisible,  
               SE_SetLight0Pos, SE_OpenColFileSelection,
               SE_SaveDrawnROIFileSelection, SE_OpenDrawnROIFileSelection, 
               SE_SaveXformOptsFileSelection, SE_OpenXformOrtFileFileSelection,
               SE_SendColorMapToAfni, SE_SaveSOFileSelection,
               SE_SetSOinFocus, SE_StartListening, SE_LoadViewFileSelection, 
               SE_SaveViewFileSelection, SE_LoadSegDO,
               SE_OpenDsetFileSelection, SE_OpenCmapFileSelection, SE_SetClip, 
               SE_OpenDsetFile, SE_OpenColFile, SE_OneOnly, SE_OpenSurfCont,
               SE_SetSurfCont, SE_SetObjectCont, SE_SetViewerCont, 
               SE_SetRecorderCont,
               SE_SetDsetViewMode, SE_SetDsetFont, SE_SetDsetNodeRad, 
               SE_SetDsetThrough, 
               SE_SetDsetNodeCol, SE_SetDsetEdgeThick, SE_SetDsetEdgeStip,
               SE_SetDsetGmatBord, SE_SetDsetTxtShad, SE_SetTractMask,
               SE_SetDsetAlphaVal, SE_SetTractStyle, SE_SetATransMode,
               SE_OpenMaskFileSelection, SE_SaveMaskFileSelection, 
               SE_OpenMaskFile,
               SE_BadCode} SUMA_ENGINE_CODE; 
                        /* DO not forget to modify SUMA_CommandCode */
typedef enum { SE_niEmpty,
               SE_niSetSurfCont, SE_niSetObjectCont, SE_niSetViewerCont, 
               SE_niSetRecorderCont, 
               SE_niKillSuma, SE_GetLabel,
               SE_niBadCode} SUMA_NI_COMMAND_CODE;
                                
typedef enum { SEF_Empty, 
               SEF_fm, SEF_im, SEF_fv3, SEF_iv3, SEF_fv15, 
               SEF_iv15, SEF_i, SEF_f, SEF_s, SEF_vp, 
               SEF_cp, SEF_fp, SEF_ip, SEF_iv200, SEF_fv200, 
               SEF_ivec, SEF_fvec, SEF_ngr, SEF_nel, 
               SEF_BadCode} SUMA_ENGINE_FIELD_CODE; 
               
typedef enum { SES_Empty,
               SES_Afni,  /*!< command from Afni directly which practically means                                that Srcp in EngineData is not SUMA_SurfaceViewer 
                               * . In the future, some Afni related pointer might                                get passed here. */
               SES_Suma,  /*!< command from Suma, which means that Srcp is a 
                               SUMA_SurfaceViewer * to the viewer making the 
                               command. */
               SES_SumaWidget,  /*!< command from a widget in Suma. Usually 
                                     means, do not try to update widget ... */
               SES_SumaFromAfni,   /*!< command from Suma in response to a 
                                        request from Afni. Srcp is still a 
                                        SUMA_SurfaceViewer * but Afni, having 
                                        initiated the command should not receive 
                                        the command back from Suma. Think 
                                        cyclical cross hair setting... */
               SES_SumaFromAny,  /*!< Same concept as SES_SumaFromAfni but from 
                                      generic program. */
               SES_Unknown} SUMA_ENGINE_SOURCE;

typedef enum { SEI_WTSDS,  
               SEI_Head, SEI_Tail, SEI_Before, SEI_After, SEI_In,
               SEI_BadLoc } SUMA_ENGINE_INSERT_LOCATION;
               
typedef enum {    SOPT_ibbb,  /*!< int, byte, byte, byte, null */
                  SOPT_ifff   /*!< int, float, float, float, null */
            } SUMA_OVERLAY_PLANE_TYPE; /*!< type of color plane data, letters 
                               code for index red green blue and alpha values */



typedef enum { SW_File, 
               SW_FileOpen, SW_FileOpenSpec, SW_FileOpenSurf, SW_FileClose,
               SW_FileSaveView, SW_FileLoadView, 
               SW_N_File } SUMA_WIDGET_INDEX_FILE; 
                     /*!< Indices to widgets under File menu. 
                          Make sure you begin with SW_File and end
                                                      with SW_N_File */
typedef enum { SW_Tools,
               SW_ToolsDrawROI,
               SW_N_Tools } SUMA_WIDGET_INDEX_TOOLS; 
                     /*!< Indices to widgets under Tools menu. 
                          Make sure you begin with SW_Tools and end
                                                      with  SW_N_Tools*/
typedef enum { SW_View, 
               SW_ViewSumaCont, SW_ViewSurfCont, SW_ViewViewCont, 
               SW_ViewSep1,
               SW_ViewCrossHair, SW_ViewNodeInFocus, SW_ViewSelectedFaceset,
               SW_N_View } SUMA_WIDGET_INDEX_VIEW; 
                     /*!< Indices to widgets under View menu. 
                          Make sure you begin with SW_View and end
                                                      with SW_N_View */
typedef enum { SW_Help, 
               SW_HelpWeb,
               SW_HelpUsage,  SW_HelpMessageLog, SW_HelpSep1, 
               SW_HelpSUMAGlobal, SW_HelpViewerStruct, SW_HelpSurfaceStruct, 
               SW_HelpSep2, SW_HelpIONotify, SW_HelpEchoKeyPress, 
               SW_HelpMemTrace,  
               SW_N_Help } SUMA_WIDGET_INDEX_HELP; 
                  /*!< Indices to widgets under Help menu.
                       Make sure you begin with SW_View and end
                                                         with SW_N_View */                                                   
typedef enum { SW_SurfCont_Render,
               SW_SurfCont_RenderViewerDefault, SW_SurfCont_RenderFill, 
               SW_SurfCont_RenderLine, SW_SurfCont_RenderPoints, 
               SW_SurfCont_RenderHide,
               SW_N_SurfCont_Render } SUMA_WIDGET_INDEX_SURFCONT_RENDER; 
                              /*!< Indices to widgets in SurfaceController under
                                   RenderMode */
typedef enum { SW_SurfCont_Trans,
               SW_SurfCont_TransViewerDefault,
               SW_SurfCont_Trans0, SW_SurfCont_Trans1, 
               SW_SurfCont_Trans2, SW_SurfCont_Trans3, SW_SurfCont_Trans4, 
               SW_SurfCont_Trans5, SW_SurfCont_Trans6, SW_SurfCont_Trans7,
               SW_SurfCont_Trans8, SW_SurfCont_Trans9, SW_SurfCont_Trans10,
               SW_SurfCont_Trans11, SW_SurfCont_Trans12, SW_SurfCont_Trans13,
               SW_SurfCont_Trans14,SW_SurfCont_Trans15, SW_SurfCont_Trans16,
               SW_N_SurfCont_Trans } SUMA_WIDGET_INDEX_SURFCONT_TRANS; 
                              /*!< Indices to widgets in SurfaceController under
                                   TransMode */
typedef enum { SW_SurfCont_ATrans,
               SW_SurfCont_ATransViewerDefault, SW_SurfCont_Alpha,
               SW_SurfCont_ATrans0, SW_SurfCont_ATrans1, 
               SW_SurfCont_ATrans2, SW_SurfCont_ATrans3, SW_SurfCont_ATrans4, 
               SW_SurfCont_ATrans5, SW_SurfCont_ATrans6, SW_SurfCont_ATrans7,
               SW_SurfCont_ATrans8, SW_SurfCont_ATrans9, SW_SurfCont_ATrans10,
               SW_SurfCont_ATrans11, SW_SurfCont_ATrans12, SW_SurfCont_ATrans13,
               SW_SurfCont_ATrans14,SW_SurfCont_ATrans15, SW_SurfCont_ATrans16,
               SW_N_SurfCont_ATrans } SUMA_WIDGET_INDEX_SURFCONT_ATRANS; 
                              /*!< Indices to widgets in SurfaceController under
                                   TransMode, with alpha mode */
typedef enum { SW_SurfCont_DsetView,
               SW_SurfCont_DsetViewCol,
               SW_SurfCont_DsetViewCon,
               SW_SurfCont_DsetViewCaC,
               SW_SurfCont_DsetViewXXX,  /* do not show, keep it last in list */
               SW_N_SurfCont_DsetView } SUMA_WIDGET_INDEX_SURFCONT_DSETVIEW;
               
typedef enum { SW_SurfCont_DsetFont,
               SW_SurfCont_DsetFont8,
               SW_SurfCont_DsetFont9,
               SW_SurfCont_DsetFontTR10,
               SW_SurfCont_DsetFontHE10,
               SW_SurfCont_DsetFontHE12,
               SW_SurfCont_DsetFontHE18,
               SW_SurfCont_DsetFontTR24,
               SW_SurfCont_DsetFontXXX, /* do not show, keep it last in list */
               SW_N_SurfCont_DsetFont } SUMA_WIDGET_INDEX_SURFCONT_DSETFONT;

typedef enum { SW_SurfCont_DsetNodeCol,
               SW_SurfCont_DsetNodeColWhite,
               SW_SurfCont_DsetNodeColBlack,
               SW_SurfCont_DsetNodeColRed,
               SW_SurfCont_DsetNodeColGreen,
               SW_SurfCont_DsetNodeColBlue,
               SW_SurfCont_DsetNodeColYellow,
               SW_SurfCont_DsetNodeColGray50,
               SW_SurfCont_DsetNodeColVal,/* colorize based on data value */
               SW_SurfCont_DsetNodeColGrp,/*  based on group membership */
               SW_N_SurfCont_DsetNodeCol }
                                          SUMA_WIDGET_INDEX_SURFCONT_DSETNODECOL;

typedef enum { SW_SurfCont_DsetTxtShad,
               SW_SurfCont_DsetTxtShad1,
               SW_SurfCont_DsetTxtShad2,
               SW_SurfCont_DsetTxtShad3,
               SW_SurfCont_DsetTxtShad4,
               SW_SurfCont_DsetTxtShad5,
               SW_SurfCont_DsetTxtShad6,
               SW_N_SurfCont_DsetTxtShad }
                                          SUMA_WIDGET_INDEX_SURFCONT_DSETTXTSHAD;

typedef enum { SW_SurfCont_DsetGmatBord,
               SW_SurfCont_DsetGmatBord0,
               SW_SurfCont_DsetGmatBord5,
               SW_SurfCont_DsetGmatBord10,
               SW_SurfCont_DsetGmatBord20,
               SW_SurfCont_DsetGmatBord40,
               SW_N_SurfCont_DsetGmatBord }
                                         SUMA_WIDGET_INDEX_SURFCONT_DSETGMATBORD;
typedef enum { SW_SurfCont_DsetNodeRad,
               SW_SurfCont_DsetNodeRadConst,
               SW_SurfCont_DsetNodeRadVal,
               SW_SurfCont_DsetNodeRadXXX,
               SW_N_SurfCont_DsetNodeRad }
                                          SUMA_WIDGET_INDEX_SURFCONT_DSETNODERAD;

typedef enum { SW_SurfCont_DsetThrough,
               SW_SurfCont_DsetThroughEdge,
               SW_SurfCont_DsetThroughCol,
               SW_SurfCont_DsetThroughRad,
               SW_SurfCont_DsetThroughCaR,
               SW_SurfCont_DsetThroughXXX,
               SW_N_SurfCont_DsetThrough }
                                          SUMA_WIDGET_INDEX_SURFCONT_DSETTHROUGH;

typedef enum { SW_SurfCont_DsetEdgeThick,
               SW_SurfCont_DsetEdgeThickConst,
               SW_SurfCont_DsetEdgeThickVal,
               SW_N_SurfCont_DsetEdgeThick }
                                       SUMA_WIDGET_INDEX_SURFCONT_DSETEDGETHICK;
                                       
typedef enum { SW_SurfCont_DsetEdgeStip,
               SW_SurfCont_DsetEdgeStipXXX,
               SW_SurfCont_DsetEdgeStipVal,
               SW_SurfCont_DsetEdgeStip1,
               SW_SurfCont_DsetEdgeStip2,
               SW_SurfCont_DsetEdgeStip3,
               SW_SurfCont_DsetEdgeStip4,
               SW_SurfCont_DsetEdgeStip5,
               SW_SurfCont_DsetEdgeStip6,
               SW_SurfCont_DsetEdgeStip7,
               SW_SurfCont_DsetEdgeStip8,
               SW_SurfCont_DsetEdgeStip9,
               SW_SurfCont_DsetEdgeStip10,
               SW_SurfCont_DsetEdgeStip11,
               SW_SurfCont_DsetEdgeStip12,
               SW_SurfCont_DsetEdgeStip13,
               SW_SurfCont_DsetEdgeStip14,
               SW_SurfCont_DsetEdgeStip15,
               SW_N_SurfCont_DsetEdgeStip }
                                       SUMA_WIDGET_INDEX_SURFCONT_DSETEDGESTIP;
typedef enum { SW_SurfCont_TractStyle,
               SW_SurfCont_TractStyleSOLID,
               SW_SurfCont_TractStyleHIDE,
               SW_SurfCont_TractStyleST1,
               SW_SurfCont_TractStyleST2,
               SW_SurfCont_TractStyleST3,
               SW_SurfCont_TractStyleST4,
               SW_SurfCont_TractStyleST5,
               SW_SurfCont_TractStyleST6,
               SW_SurfCont_TractStyleST7,
               SW_SurfCont_TractStyleST8,
               SW_SurfCont_TractStyleST9,
               SW_SurfCont_TractStyleST10,
               SW_SurfCont_TractStyleST11,
               SW_SurfCont_TractStyleST12,
               SW_SurfCont_TractStyleST13,
               SW_SurfCont_TractStyleST14,
               SW_SurfCont_TractStyleST15,
               SW_N_SurfCont_TractStyle }
                                       SUMA_WIDGET_INDEX_SURFCONT_TRACTSTYLE;
typedef enum { SW_SurfCont_DsetAlphaVal,
               SW_SurfCont_DsetAlphaVal_Max,
               SW_SurfCont_DsetAlphaVal_Avg,
               SW_SurfCont_DsetAlphaVal_Min,
               SW_SurfCont_DsetAlphaVal_I,
               SW_SurfCont_DsetAlphaVal_T,
               SW_SurfCont_DsetAlphaVal_B,
               SW_SurfCont_DsetAlphaVal_XXX,
               SW_N_SurfCont_DsetAlphaVal }
                                       SUMA_WIDGET_INDEX_SURFCONT_DSETALPHAVAL;
            
typedef enum { SW_SurfCont_TractMask,
               SW_SurfCont_TractMaskHide,
               SW_SurfCont_TractMaskGray,
               SW_SurfCont_TractMaskDim,
               SW_SurfCont_TractMaskHair,
               SW_SurfCont_TractMaskIgnore,
               SW_N_SurfCont_TractMask }
                                       SUMA_WIDGET_INDEX_SURFCONT_TRACT_MASK;
            
typedef enum { SW_DrawROI_SaveMode,
               SW_DrawROI_SaveMode1D, SW_DrawROI_SaveModeNIML, 
               SW_N_DrawROI_SaveMode } SUMA_WIDGET_INDEX_DRAWROI_SAVEMODE; 
                           /*!< Indices to widgets in DrawROI under SavingMode */
typedef enum { SW_DrawROI_SaveWhat,
               SW_DrawROI_SaveWhatThis, SW_DrawROI_SaveWhatRelated, 
               SW_N_DrawROI_SaveWhat } SUMA_WIDGET_INDEX_DRAWROI_SAVEWHAT; /*!< Indices to widgets in DrawROI under SavingWhat */

typedef enum { SW_DrawROI_WhatDist,
               SW_DrawROI_WhatDistNothing, SW_DrawROI_WhatDistTrace, SW_DrawROI_WhatDistAll, 
               SW_N_DrawROI_WhatDist } SUMA_WIDGET_INDEX_DRAWROI_WHATDIST; /*!< Indices to widgets in DrawROI under
                                                                           WhatDist */
typedef enum { SUMA_NO_ORDER, SUMA_ROW_MAJOR, SUMA_COLUMN_MAJOR }  SUMA_INDEXING_ORDER;

typedef enum { SW_CoordBias,
               SW_CoordBias_None,
               SW_CoordBias_X, SW_CoordBias_Y, SW_CoordBias_Z,
               SW_CoordBias_N, 
               SW_N_CoordBias } SUMA_WIDGET_INDEX_COORDBIAS;

typedef enum { SW_LinkMode,
               SW_LinkMode_None,
               SW_LinkMode_Pls1,
               SW_LinkMode_Same,
               SW_LinkMode_Stat,  
               SW_N_LinkMode } SUMA_WIDGET_LINK_MODE;

typedef enum { SW_CmapMode,
                  SW_Direct, SW_NN, SW_Interp,  
                  SW_N_CmapMode } 
               SUMA_WIDGET_CMAP_MODE;  /*keep parallel with enum of SUMA_COLORMAP_INTERP_MODE */

typedef enum { SUMA_NO_CLIP_PLANE_TYPE, SUMA_SCREEN_CLIP, SUMA_ALL_OBJECT_CLIP} SUMA_CLIP_PLANE_TYPES;
                                 
typedef enum {
   SUMA_RDC_ERROR = -1,
   SUMA_RDC_NOT_SET = 0,
   SUMA_RDC_X_START, /*!< flag, Beginning of X reasons */
   SUMA_RDC_X_EXPOSE,
   SUMA_RDC_X_RESIZE,
   SUMA_RDC_X_MAPSTATE,
   SUMA_RDC_X_ENTER_WINDOW,
   SUMA_RDC_X_END, /*!< flag, End of X reasons */
   SUMA_RDC_NEW_CROSSHAIR,
   SUMA_RDC_NEW_DATA,
   SUMA_RDC_NEW_SHUTUP_AND_RECORD,  
} SUMA_REDISPLAY_CAUSE; /*!< reasons for requesting a redisplay */
                                                                           
typedef struct {
   int *i;  /*!< node index */
   float *r; /*!< node red */
   float *g; /*!< node green */
   float *b;/*!< node blue */
   int N; /*!< number of elements */
}SUMA_IRGB; /*!< structure containing node colors */


typedef struct {
   SUMA_OVERLAY_PLANE_TYPE Type; /*!< This variable determines the types 
                                      of the variables below */
   SUMA_ENGINE_SOURCE Source; /*!< provenance of plane */
   SUMA_DATUM_LEVEL dtlvl; /* What does node refer to? Data can be passed
                              at multiple levels */
   void *i; /*!< Node index */
   void *r; /*!< Node red */
   void *g; /*!< Node green */
   void *b; /*!< Node blue */
   void *a; /*!< Node alpha */
   int N; /*!< number of elements in each vector above */
   float DimFact; /*!< global factor applied to each color */
   SUMA_Boolean Show; /*!< show plane ?*/
   float GlobalOpacity; /*!< Global opacity factor */
   SUMA_Boolean isBackGrnd; /*!< Brightness modulation */
}  SUMA_OVERLAY_PLANE_DATA; /*!< This is a conveninence structure 
                           meant to carry data required to fill a color plane. 
                                 \sa SUMA_OVERLAYS*/

typedef enum { SUMA_CMAP_ERROR=-1, SUMA_CMAP_UNDEFINED, 
                           /* Begin adding colormaps next: */
               SUMA_CMAP_RGYBR20,  SUMA_CMAP_nGRAY20, 
               SUMA_CMAP_GRAY02, SUMA_CMAP_flpGRAY02, 
               SUMA_CMAP_GRAY20, SUMA_CMAP_BW20, SUMA_CMAP_BGYR19, 
               SUMA_CMAP_MATLAB_DEF_BYR64, SUMA_CMAP_BGYR64, 
               SUMA_CMAP_ROI64, SUMA_CMAP_ROI128,
               SUMA_CMAP_ROI256,
               SUMA_CMAP_N_MAPS /* Don't add after this one */
               } SUMA_STANDARD_CMAP; /*!< Names of standard colormaps. 
                  RGYBR20 reads Red, Green, Yellow, Blue, Red, 20 colors total */

typedef enum { SUMA_ROI_InCreation, SUMA_ROI_Finished, 
               SUMA_ROI_InEdit} SUMA_ROI_DRAWING_STATUS;

typedef enum { SUMA_ROI_OpenPath,   /*!< Topologically connected nodes*/
               SUMA_ROI_ClosedPath, /*!< A closed OpenPath */
               SUMA_ROI_FilledArea, /*!< A filled ClosePath */
                                    /* Preserve the order of the above three */
               SUMA_ROI_Collection  /*!< A collection of nodes */
            } SUMA_ROI_DRAWING_TYPE;  /*!< an ROI created by drawing 
                                          (or other means)*/

typedef enum { SUMA_BSA_Undefined, SUMA_BSA_AppendStroke, SUMA_BSA_AppendStrokeOrFill, SUMA_BSA_JoinEnds, SUMA_BSA_FillArea } SUMA_BRUSH_STROKE_ACTION; 

typedef enum { SUMA_ROI_Undefined,
               SUMA_ROI_NodeGroup, /*!< A collection of nodes */
               SUMA_ROI_EdgeGroup, /*!< A collection of edges */
               SUMA_ROI_FaceGroup, /*!< A collection of Faces */
               SUMA_ROI_NodeSegment /*!< A series of connected nodes */
             } SUMA_ROI_TYPE; /* a generic types of ROI datums*/

typedef enum { SXR_default, SXR_Euro, SXR_Afni , SXR_Bonaire, SXR_Print } SUMA_XRESOURCES;   /* flags for different X resources */

typedef enum { SRM_ViewerDefault, SRM_Fill, SRM_Line, SRM_Points , SRM_Hide, 
               SRM_N_RenderModes} SUMA_RENDER_MODES; /*!< flags for various 
                                                            rendering modes */

typedef enum { STM_ViewerDefault,
               STM_0, STM_1, STM_2,STM_3,STM_4,STM_5,
               STM_6, STM_7, STM_8, STM_9, STM_10, STM_11, STM_12, STM_13,
               STM_14, STM_15, STM_16, STM_N_TransModes} SUMA_TRANS_MODES; 
                                 /*!< flags for various transparency values */

typedef enum { SATM_ViewerDefault, SATM_ALPHA,
               SATM_0, SATM_1, SATM_2,SATM_3,SATM_4,SATM_5,
               SATM_6, SATM_7, SATM_8, SATM_9, SATM_10, SATM_11, SATM_12, 
               SATM_13, SATM_14, SATM_15, SATM_16, 
                                       SATM_N_TransModes} SUMA_ATRANS_MODES; 
                                 /*!< flags for various transparency values,
                                    including Alpha mode */


typedef enum { 
               SDODM_Error=-1,
               SDODM_All, 
               SDODM_n3CrossHair,  
               SDODM_n2CrossHair, SDODM_n1CrossHair, SDODM_n0CrossHair, 
               SDODM_Hide,  
               SDODM_N_DO_DrawMasks}  SUMA_DO_DRAW_MASK;

   /*!< number of useful views enumerated in SUMA_STANDARD_VIEWS */
typedef enum {    SUMA_2D_Z0, SUMA_2D_Z0L, 
                  SUMA_3D, SUMA_3D_Z0,
                  SUMA_N_STANDARD_VIEWS } SUMA_STANDARD_VIEWS; 
                     /*!< Standard viewing modes. These are used to decide what 
                     viewing parameters to carry on when switching states 
                     SUMA_2D_Z0 2D views, with Z = 0 good for flat surfaces
                     SUMA_2D_Z0L 2D views, with Z = 0 
                                          good for left hemi flat surfaces
                     SUMA_3D standard 3D view
                     SUMA_3D_Z0 straight down the Z, good for balls
                     SUMA_N_STANDARD_VIEWS used to flag errors leave  at the end 
                     */
typedef enum {   SUMA_No_Lock, SUMA_I_Lock, SUMA_XYZ_Lock, SUMA_N_Lock_Types}  SUMA_LINK_TYPES; /*!< types of viewer linking. Keep SUMA_N_Lock_Types at the end, it is used to keep track of the number of types*/
                                                                 
typedef enum {  SWP_DONT_CARE,
                SWP_TOP_RIGHT, /*!< Position to the top right of reference */
                SWP_BOTTOM_RIGHT_CORNER, 
                SWP_STEP_DOWN_RIGHT,
                SWP_TOP_LEFT,
                SWP_POINTER, /*!< Position centered to the pointer */
                SWP_POINTER_OFF,
                SWP_POINTER_LEFT_BOTTOM,
                SWP_TOP_BEST /*!< Top right, else top left with no overlap */
             } SUMA_WINDOW_POSITION; /*!< Types of relative window positions */

typedef enum {    SAR_Undefined,
                  SAR_Fail, /*!< Failed action */
                  SAR_Succeed,
               }  SUMA_ACTION_RESULT;  

typedef enum { SAP_Do,
               SAP_Undo,
               SAP_Redo,
            } SUMA_ACTION_POLARITY;               

typedef enum { SUMA_NO_BLEND=0, SUMA_BLEND1, SUMA_BLEND2, SUMA_N_BLEND_MODES} SUMA_BLEND_MODES;

typedef enum {
   SUMA_ROI_FILL_TO_ALLROI, /*!< Fill until you encounter a node part of any ROI */
   SUMA_ROI_FILL_TO_THISROI, /*!< Fill until you encounter a node part of this ROI */
}SUMA_ROI_FILL_MODES; 

typedef struct {
   SUMA_ACTION_RESULT (*ActionFunction)(void *ActionData, SUMA_ACTION_POLARITY Pol); /*!< The function to call for performing the action */
   void *ActionData; /*!< The data to be passed to the function performing the action */
   void (*ActionDataDestructor)(void *Actiondata); /*!< The function to call that destroys ActionData */
} SUMA_ACTION_STACK_DATA; /*!< a structure containing the data to form the element of the Action Stack element*/

/*! structure to keep track of allocate memory */
typedef struct {
   void **Pointers; /*!< vector of pointers for which memory was allocated */
   int *Size; /*!< vector of sizes of allocated memory blocks. Pointers[i] has Size[i] bytes allocated for it */
   int N_alloc; /*!< number of meaningful entries in Pointers and Size */
   int N_MaxPointers; /*!< Maximum number of elements allocated for in Pointers and Size */
} SUMA_MEMTRACE_STRUCT;

/*! structure containing a data block information */
typedef struct {
   void *data;   /*!< pointer to data location */
   int N_link; /*!< number of links to data location */
   char ParentIDcode[SUMA_IDCODE_LENGTH]; /* IDcode of the creator of data */
} SUMA_INODE;


/*! structure containing an ROI Plane's infor */ 
typedef struct {
   char *name; /*!< name of plane and the indices of the ROIs that belong to it */
   DList *ROI_index_lst;   /*!< list of indices (into SUMA_DO * SUMAg_DOv) of 
                               SUMA_DRAWN_ROI * objects that belong to a certain 
                               plane. That sounds confusing now, I'm sure it'll 
                               sound clear a year from now */  
} SUMA_ROI_PLANE;

typedef enum { SUMA_UNDEFINED_MODE, 
               SUMA_DIRECT, /*!< no interpolation on the colormap, node value is 
                                 typecast to int and directly used
                                 to access color map */
               SUMA_NO_INTERP,   /*!< no interpolation on the colormap 
                                     (like in afni with paned colormaps) but 
                                     ranging is applied */  
               SUMA_INTERP       /*!< interpolation on the colormap, 
                                      SUMA's default */
            } SUMA_COLORMAP_INTERP_MODE; /* keep parallel with enums of 
                                            SUMA_WIDGET_CMAP_MODE */
               
typedef enum {
               SUMA_NOT_SET=-1,
               SUMA_NO_THRESH,
               SUMA_LESS_THAN,   /*!< Mask if T[i] < Opt->ThreshRange[0] */
               SUMA_ABS_LESS_THAN, /*!< Mask if T[i] < Opt->ThreshRange[0] || 
                                        T[i] > -Opt->ThreshRange[0] */
               SUMA_THRESH_OUTSIDE_RANGE, /*!< Mask if T[i] < Opt->ThreshRange[0]
                                                 || T[i] > Opt->ThreshRange[1] */
               SUMA_THRESH_INSIDE_RANGE, /*!< Mask if T[i] >= Opt->ThreshRange[0]
                                              && T[i] <= Opt->ThreshRange[1] */
            }  SUMA_THRESH_MODE;
/*! a structure holding the options for the function SUMA_ScaleToMap 
\sa SUMA_ScaleToMapOptInit to allocate and initialize such a structure 
to free this structure use the free function
*/

typedef enum { 
   SUMA_BAD_MODE=-1, 
   SUMA_ORIG_MIX_MODE,  /*!< The original mode for color overlaying: 
                     if (Col1) Col = (1-opacity) Col1 + opacity Col2 */
   SUMA_4AML,  /*!< A modified mixing mode to keep colors from getting dimmed 
                  (as with opacity of 0.5 on Col1 = 0.3 0 0 and Col2 = 0 0.3 0)
                  resultant is a very dim yellow 0.15 0.15 0 
                  Named after the eminent A. M. L.*/
   SUMA_MAX_MODES /*!< The limit, used for cycling */
} SUMA_COL_MIX_MODE;

/*! structure containing the color mapping of a vector */
typedef struct {
   /* float **cM; Prior to Mar 17 03*/
            /*!< N_Node x 3 matrix containing the colors at each node*/
   float *BiasCoordVec; /*!< A vector of coordinate bias */
   float *cV; /*!< N_Node x 3 vector containing the colors at each node*/
   int N_Node; /*!< obvious */
   SUMA_Boolean *isMasked; /*!< if isMasked[i] then node i has a mask color 
                                associated with it */ 
   int N_VCont; /* Number of nodes not masked */
   int *VCont; /* An N_VCont x 1 vector to hold a flag value to label this node*/
} SUMA_COLOR_SCALED_VECT;




/*! TRY TO MAKE DO WITHOUT THIS THING, IF POSSIBLE. 
It is a pain to work with two types of ROI structues 
structure to hold an ROI */
typedef struct { 
   SUMA_ROI_TYPE Type;   /*!< The type of ROI */
   
   char *idcode_str;    /*!< unique idcode for ROI */
   char *Parent_idcode_str; /*!< idcode of parent surface */
   char *Label; /*!< ascii label for ROI */

   int *ElInd; /*!< Pointer to vector containing indices into the parent 
                     surface (SO has Parent_idcode_str) of ROI elements.
                    If Type is SUMA_ROI_NodeGroup then ElementIndex contains 
                     indices to SO->NodeList .
                    If Type is SUMA_ROI_FaceGroup then ElementIndex contains 
                     indices to SO->FaceList.
                    If Type is SUMA_ROI_EdgeGroup then ElementIndex contains 
                     indices to SO->EL->EL. */
   int N_ElInd; /*!< Number of elements in ElementIndex */ 
} SUMA_ROI; 



typedef struct {
   SUMA_ROI_TYPE Type; /*!< Type of ROI in datum */
   int N_n; /*!< Number of elements in nPath */
   int N_t; /*!< Number of elements in tPath */
   int *nPath; /*!< Vector of N node indices. These nodes must be immediate 
                     (linked) neighbours of each other */
   int *tPath; /*!< Vector of N triangle indices. These triangles must 
                     be connected to each other */
   float tDistance; /*!< distance from the first node to the last 
                        taken along the surface (geodesic)*/
   float nDistance; /*!< distance from the first node to the last by summing the 
                         length of segments between nodes */
   SUMA_BRUSH_STROKE_ACTION action; /*!< a record of the action that went with 
            this datum. 
            This field is used to recreate the ROI drawing history from a saved
            niml file */
} SUMA_ROI_DATUM; /*!< elementary datum of a drawn ROI */


#define SUMA_MAX_ROI_CTRL_NODES 100 /*!< Maximum number of control nodes in an ROI */
#define SUMA_MAX_ROI_CTRL_NODES3 300 
#define SUMA_MAX_ROI_ON_SURFACE 100 /*!< Maximum number of ROIs Drawn on a surface */

typedef struct {   
   int n1;  /*!<index of edge's first node */
   int n2; /*!<index of edge's second node */
} SUMA_CONTOUR_EDGES; /*<! structure defining an edge by the nodes forming it*/

/*! structure to hold the drawing of an ROI */
typedef struct {   
      /* FIRST VARIABLES MUST RETAIN THEIR ORDER and follow SUMA_ALL_DO */
   SUMA_DO_Types do_type; 
   char *idcode_str;    /*!< unique idcode for ROI */
   char *Label; /*!< ascii label for ROI */ 

      /* Begin specific fields */
   SUMA_ROI_DRAWING_TYPE Type;   /*!< The type of ROI drawn, 
                              that would be closed path, etc, etc, */

   char *Parent_idcode_str; /*!< idcode of parent surface */
   SUMA_SO_SIDE Parent_side; /*!< Hemisphere of parent. 
                  Comes in handy when trying to find parent if 
                  Parent_idcode_str does not work */
   char *ColPlaneName;  /*!< Name of color plane that the ROI is painted in.
                     If this field is set to NULL then the ROI will be painted
                     in the generic ROI_Plane plane. 
                     For the moment, NULL is the only
                     option */
   float FillColor[4];  /*!< RGB fill color */
   float EdgeColor[4];  /*!< RGB edge color */
   int EdgeThickness;   /*!< thickness of edge */
   int iLabel; /*!< An integer value, another way to represent a Label */
   SUMA_Boolean ColorByLabel; /*!< flag indicating that ROI node colors should
                                   be based on the value in iLabel and not the 
                                   one specified in FillColor */
   SUMA_ROI_DRAWING_STATUS DrawStatus; /*!< Status of the ROI being drawn, 
                                 finished, being drawn, being edited, etc. */

   DList *ROIstrokelist;   /*!< a doubly linked list with the data element 
                                 being a (void *)SUMA_ROI_DATUM * */

   DList *ActionStack; /*!< a stack containing the various actions performed*/
   DListElmt *StackPos; /*!< The element of ActionStack that represents 
                             the current position */
   
   int N_CE; /*!< number of contour edges */
   SUMA_CONTOUR_EDGES *CE; /*!< a vector of edges that form the contour 
                                of the ROI */
} SUMA_DRAWN_ROI;

typedef struct {
   int Type;         /*!< The final type of the DrawnROI, 
                           see SUMA_ROI_DRAWING_TYPE*/
   char *idcode_str;
   char *Parent_idcode_str;
   SUMA_SO_SIDE Parent_side;
   char *Label;
   int *iNode; /*!< A node's index */
   int *iLabel; /*!< A node's value */
   int N; /*!< NUmber of elements in iNode and iLabel */
} SUMA_1D_DRAWN_ROI; /*!< a version of SUMA_DRAWN_ROI struct that can 
                     be used by 1D functions.
                     Fields are a reflection of those in SUMA_DRAWN_ROI*/

typedef struct {
   SUMA_ROI_DATUM *ROId;
   SUMA_DRAWN_ROI *DrawnROI;
} SUMA_ROI_ACTION_STRUCT;  /*!< a structure packaging data for the 
                                 routines acting on drawn ROIs */

typedef enum { SUMA_SORT_CLUST_NOT_SET, SUMA_SORT_CLUST_NO_SORT, SUMA_SORT_CLUST_BY_NUMBER_NODES, SUMA_SORT_CLUST_BY_AREA } SUMA_SURF_CLUST_SORT_MODES;

typedef struct {
   char *in_name;
   int nodecol;
   int labelcol;
   char *out_prefix;  /* this one's dynamically allocated so you'll have 
                         to free it yourself */
   float DistLim;
   float AreaLim;
   int NodeLim;
   SUMA_THRESH_MODE DoThreshold;
   float ThreshR[2];
   int tind;
   float update;
   int DoCentrality;
   SUMA_Boolean OutROI;
   SUMA_Boolean OutClustDset;
   SUMA_Boolean WriteFile;
   SUMA_SURF_CLUST_SORT_MODES SortMode;
   SUMA_Boolean FullROIList;
   SUMA_Boolean prepend_node_index;
   SUMA_DSET_FORMAT oform;
   
} SUMA_SURFCLUST_OPTIONS;



typedef struct {
   SUMA_Boolean ApplyMask; /*!< if YUP then values that fall in MaskRange 
                                 are assigned the color in MaskColor */
   float MaskRange[2];     /*!< values between MaskRange[0] and 
                                MaskRange[1] (inclusive) are assigned 
                                MaskColor */
   float MaskColor[4];     /*!<   color to assign to masked nodes */
   SUMA_Boolean ApplyClip; /*!< if YUP then range clipping using Range 
                                 is applied */
   
   /* fields used in the _Interactive scale to map mode */
   float BrightFact; /*!< a brightness factor to apply to the color map. 
                           This factor is applied to the colors in the 
                           colormap and the mask colors
                           This overrides DimFact in SUMA_OVERLAYS*/
   SUMA_Boolean MaskZero; /*!<   values equal to zero will be masked 
                                 no matter what */
   double ThreshRange[2]; /*!< Thresholding range.  */
   float ThreshStats[2]; /*!<  Thresholding statistics, 
                              ThreshStats[0] = p(ThreshRange[0])
                              ThreshStats[1] = q(ThreshRange[0]) */
   SUMA_NUMERICAL_UNITS RangeUnits;
   double IntRange[2]; /*!< nodes with values <= Range[0] are 
                           given the first color in the color map, 
                           values >= Range[1] get the last color in the map
                            (USED to be called ClipRange*/
   double BrightRange[2]; /*!< Same as IntRange but for brightness 
                              modulating column */
   float BrightMap[2]; /*!<   BrightRange[0] is mapped to BrightMap[0],
                              BrightRange[1] is mapped to BrightMap[1] */
   SUMA_Boolean alaAFNI; /*!< If yes, use ScaleToMap_alaAFNI, if NOPE, 
                              use ScaleToMap */
   SUMA_COLORMAP_INTERP_MODE interpmode; /*!< see typedef.*/
   int find;   /*!< index of function sub-brick */
   int tind;   /*!< index of thresholf sub-brick */
   int bind;   /*!< index of attenuation sub-brick */
   SUMA_Boolean UseThr; /*!< use or ignore tind */
   SUMA_THRESH_MODE ThrMode;  /*!< how to apply the thresholding */
   SUMA_Boolean UseBrt; /*!< use or ignore bind */
   SUMA_WIDGET_INDEX_COORDBIAS DoBias;  /*!< use coordinate bias */
   double CoordBiasRange[2]; /*!< Same as IntRange but for brightness 
                                 modulating column */
   float *BiasVect; /*!< A vector of values to add to the coordinates 
                         of the mesh */
   int AutoIntRange;
   int AutoBrtRange;
   int ColsContMode; /*!< a flag to indicate what to do about contours */
   
   SUMA_SURFCLUST_OPTIONS *ClustOpt; /*!< Clusterizing options */
   int Clusterize; /*!< If on, do some cluster business */
   int RecomputeClust; /*!< 1 when clusters should be recomputed at 
                              recolorization */

} SUMA_SCALE_TO_MAP_OPT;


/*! Structure containing one color overlay */
typedef struct {
   SUMA_DO_Types do_type;  /*!< To check if this is a displayable object */
   int LinkedPtrType; /*!< Indicates the type of linked pointer */
   int N_links;   /*!< Number of links to this pointer */
   char owner_id[SUMA_IDCODE_LENGTH];   /*!< The id of whoever created 
                                    that pointer. */

   SUMA_DATUM_LEVEL dtlvl; /* What does a 'node' refer to? Data can be passed
                              at multiple levels */

   int ShowMode; /*!< negative do not show, postive, 
                  ShowMode can be +/-SW_SurfCont_DsetViewCol
                  +/-SW_SurfCont_DsetViewCon, and +/-SW_SurfCont_DsetViewC&C 
                  It cannot be +/-SW_SurfCont_DsetViewXXX
                  see SUMA_WIDGET_INDEX_SURFCONT_DSETVIEW */
   int Font; /*!< negative do not show, postive, 
                  Font can be +/- most of SUMA_WIDGET_INDEX_SURFCONT_DSETFONT
                  It cannot be +/-SW_SurfCont_DsetFontXXX
                  see SUMA_WIDGET_INDEX_SURFCONT_DSETFONT */
   int NodeRad; /*!< negative do not show, postive, 
               NodeRad can be +/- most of SUMA_WIDGET_INDEX_SURFCONT_DSETNODERAD
                  It cannot be +/-SW_SurfCont_DsetNodeRadXXX
                  see SUMA_WIDGET_INDEX_SURFCONT_DSETNODERAD */
   int Through; /*!< negative do not show, postive, 
               Through can be +/- most of SUMA_WIDGET_INDEX_SURFCONT_DSETTHROUGH
                  It cannot be +/-SW_SurfCont_DsetThroughXXX
                  see SUMA_WIDGET_INDEX_SURFCONT_DSETTHROUGH */
   float NodeRadGain;
   int NodeCol; /*!< Node colors, either constant or from dset 
                  see SUMA_WIDGET_INDEX_SURFCONT_DSETNODECOL */
   int BordFrac; /*!< Thinckness of border relative to cell width */
   int TxtShad; /*!< Text shadowing, see SUMA_WIDGET_INDEX_SURFCONT_DSETTXTSHAD*/
   
   int EdgeThick; /* see SUMA_WIDGET_INDEX_SURFCONT_DSETEDGETHICK */
   float EdgeThickGain;
   int EdgeStip; /* see SUMA_WIDGET_INDEX_SURFCONT_DSETEDGESTIP,
                        Also see  SUMA_WIDGET_INDEX_SURFCONT_TRACTSTYLE*/
   int AlphaVal; /* see SUMA_WIDGET_INDEX_SURFCONT_DSETALPHAVAL */
   char *Name; /*!<  name of ovelay, CONVEXITY or Functional or areal boundaries 
                     perhaps. The Name can be a filename with path*/
   char *Label; /*!< Usually the same as Name without any existing path */
      
      /* These can't just come from dset_link because as you change the 
         threshold, and other parameters, some nodes may not get colored so the 
         NodeDef list will differ from that in dset.
         The macros COLP_NODEDEF, COLP_N_NODEDEF and COLP_N_ALLOC will be 
         redefined to point to fields inside the overlays structure                           Mon Mar 29 15:11:01 EST 2004*/
   int *NodeDef; /*!< nodes over which the colors are defined*/
   int N_NodeDef; /*!< total number of nodes specified in NodeDef*/
   #if 0
      /* That one is still borrowed from dset structure */
   int N_Alloc; /*!< You'd think this should be equal to NodeDef, but in 
                     instances where you may be receiving varying numbers of 
                     colors to the same plane, it's a pain to have to free and 
                     realloc space. So, while the juice is only up to N_NodeDef, 
                     the allocation is for N_Alloc */
   #endif
   int FullList; /*!< if 1 then it indicates that a full listing of node 
                     colors exists.
                     i.e. nodes need not be defined explicitly, in that case 
                     NodeDef is still explicitly defined. I have my reasons.*/
                     
   float *ColVec; /*!< N_NodeDef x 3 vector containing colors of nodes 
                       specified in NodeDef, Replaces ColMat, Wed Mar 17 04*/
   byte *ColAlpha; /*!< Color Alpha obtained directly from the colors in ColVec,
                        or from the I,T,B. selectors in the data.
                        This Alpha is only used for Volume objects at the moment,
                        and is different from LocalOpacity in that this Alpha
                        does not enter into any of the SUMA controlled mixing
                        of color overlay planes. */
   int RemixOID;/*!< A number that changes each time ColVec content is changed */
   
   float *V; /*!< A copy of the dataset's column that produced the colors in
                      ColVec. */
   int N_V; /*!< Should be the same as SDSET_VECFILLED(Sover->dset_link),
                 But it is set when V is created to be safe */
   char V_identifier[32+SUMA_IDCODE_LENGTH];
   float *Vperc;
   int N_Vperc;
   
   float *T; /*!< A copy of the dataset's column that corresponds to 
                  the threshold of V */
   int N_T; /*!< Should be the same as SDSET_VECFILLED(Sover->dset_link),
                 But it is set when V is created to be safe */
   char T_identifier[32+SUMA_IDCODE_LENGTH];
   float *Tperc;
   int N_Tperc;
   
   
   float GlobalOpacity; /*!< Opacity factor between 0 and 1 to apply to 
                           all values in ColMat */
   float *LocalOpacity; /*!< Opacity factor vector between 0 and 1 to apply to 
                             each individual node color */
   int PlaneOrder; /*!< Order of the overlay plane, 1st plane is 0 and is 
                        farthest away from the top */  
   SUMA_Boolean isBackGrnd; /*!< if YUP then colors overlaid on top of this plane
            have their brightness modulated by the average intensity of the 
            colors in that plane see the function SUMA_Overlays_2_GLCOLAR4 for 
            details. 
            In other obscure words, if YUP then plane is part of background.*/ 
   float DimFact;    /*!< a scaling factor applied to the colors in ColVec 
                           This is overriden by BrightFact in OptScl which is
                           defined for non-explicitly colored planes*/
   double ForceIntRange[2]; /*!< Use values here to set OptScl->IntRange instead 
         of the true range of values in the dataset.
         The idea is to allow particular settings for the autoranging 
         options that are not from the dset's min to max.
         Usually, this field is not used and both values are set to 0.0
                                 */
   /* New additions, Fri Feb 20 13:21:28 EST 2004 */
   SUMA_DSET *dset_link; /*!< A COPY OF THE POINTER to the dataset this plane is 
                              attached to. DO NOT FREE THIS POINTER MANUALLY.
                              This is done in the functions for creating and 
                              destroying
                              overlay planes */ 
   char *cmapname; /*!< name of colormap (must be in SUMAg_CF->scm)  */
   SUMA_SCALE_TO_MAP_OPT *OptScl;   /* Options for mapping values in 
                                       dset to colormap */
   int SymIrange;
   
   MEM_topshell_data *rowgraph_mtd;
   int rowgraph_num;
   
   int N_Contours;            /* Number of contours ROIs*/
   SUMA_DRAWN_ROI **Contours; /* Using the ROI structure to store contours
                                 which can be displayed along with color blobs */
   SUMA_WIDGET_LINK_MODE LinkMode;          /* How to link I & T selectors */
   
   DList *ClustList; /*!< The list of clusters */
   byte *ClustOfNode; /*!< Tells which cluster a node belongs to, Should have
                           SO->N_Node values in it*/
   float AlphaThresh;
} SUMA_OVERLAYS;


/*! 
Stucture to hold the contents of the specs file 
*/
typedef struct {
   char **SurfaceType;    /*!< Type of surface loaded: 
                               FreeSurfer, SureFit/Caret, 1D format, 
                               inventor, Ply */ 
   char **SurfaceFormat;  /*!< ASCII or Binary */
   char **TopoFile; /*!< Surface Topology (mesh) file 
                         renamed from SureFitTopo because 1D uses it too */ 
   char **CoordFile; /*!< Surface Coordinate (XYZ) file
                          renamed from SureFitCoord because 1D uses it too  */ 
   char **MappingRef; /*!< Becoming obsolete. Jan 2 03 */
   char **SureFitVolParam; /*!<  For SureFit only: Name of file containing 
                                 anatomical coordinates modification. */
   char **SurfaceFile;  /*!< File containing topology and geometry of surface. */
   char **VolParName;   /*!< Now known as surface volume in the documentation 
                             This is the volume from which the surface was 
                             created, aligned to the experiment's data. Alignment                              transforms added by 3dVolreg or 3dAnatNudge that are                              stored in this volume ar applied to the surface.  
                             Also, tlrc transforms of this volume can be applied 
                             to the surface. */
   char **IDcode;      /*!< Unique identifier for the surface object */
   char **State;       /*!< Geometrical state of the surface. For example:
                            pial, white, inflated, spherical, etc... */
   char **LabelDset;    /*!< Name of a label dset, like the annotation file */                                                                        
   char **Group;        /*!< Some identifier, best thought of as the name of 
                             the subject */
   char **SurfaceLabel; /*!< A user defined "short" label to use in GUI */
   int *EmbedDim;       /*!< 2 for flat surfaces, 3 for 3D dwelling ones. */
   
   /* modifications to the lame MappingRef field */
   char **AnatCorrect;    /*!< Does surface geometry match the anatomy ?*/
   char **Hemisphere;     /*!< Left/Right */
   char **DomainGrandParentID;   /*!< Grandparent's mesh ID 
                                    (icosahedron's for std-meshes) */
   char **OriginatorID;   /*!<  ID common to surfaces from one subject that are 
                                 created at one point in time. Surfaces of the 
                                 same subject, created at different points in 
                                 time (like in a longitudinal study) will have 
                                 differing OriginatorID fields */
   char **LocalCurvatureParent;   /*!<  Name of surface (in current spec file)
                      from which the curvature will be borrowed.
                      The LocalCurvatureParent must be isotopic to 
                      the child surface. This Parent used to be
                      the MappingRef field*/
   char **LocalDomainParent;       /*!< Name of surface (in current spec file)
                       from which EdgeLists and other shared information                                 will be borrowed. This field used to be 
                       the MappingRef field. Naturally, Parent and 
                       Child must be isotopic.
                       You must have at least one of the surfaces loaded
                       into SUMA be the Parent. Use SAME for this field when
                       a surface is a LocalDomainParent.
                      */
   char **NodeMarker;  /*!< File containing node marker DO */

   int N_Surfs;         /*!< Number of surfaces, in the spec file */
   int N_States;                                                     
   int N_Groups;
   
   int N_DO;
   int *DO_type;
   char **DO_name;
   
   char *StateList;
   char *SpecFilePath;
   char *SpecFileName;
} SUMA_SurfSpecFile;

/*! structure that containing node's first order neighbors */
typedef struct {
   SUMA_DO_Types do_type;  /*!< To check if this is a displayable object */
   int LinkedPtrType; /*!< Indicates the type of linked pointer */
   int N_links;   /*!< Number of links to this pointer */
   char owner_id[SUMA_IDCODE_LENGTH];   /*!< The id of whoever created that 
                                       pointer. Might never get used.... */


   char *idcode_str; /*!< identifier of element containing node's first 
                           order neighbors */
   int N_Node; /*!< Number of nodes whose neighbors are listed in this 
                    structure */
   int *NodeId; /*!< Id of each node whose neighbors are listed in this structure 
            *** WARNING: *** A lot of functions do not use this field and assume
            N_Node = number of nodes in the surface! */
   int **FirstNeighb; /*!< N_Node x N_Neighb_max matrix with each row specifying 
                        the indices of neighboring nodes.
                       After Tue Jan  7 18:13:44 EST 2003: The nodes are now 
                       ordered to form a path on the surface.
                   Note: There is no guarantee that the path is closed. */
   int *N_Neighb; /*!< maximum number of neighbors for a particular node */
   int N_Neighb_max; /*!< maximum number of neighbors of all nodes */
} SUMA_NODE_FIRST_NEIGHB;

/*! structure that contains faceset's first order neighbors */
typedef struct {
   int N_FaceSet; /*!< Number of nodes whos neighbors are listed in 
                       this structure */
   int **FirstNeighb; /*!< N_Node x N_Neighb_max matrix with each row 
                        specifying the indices of neighboring facesets */
   int *N_Neighb; /*!< maximum number of neighbors for a particular faceset */
   int N_Neighb_max; /*!< maximum number of neighbors of all facesets */
   int N_Neighb_min; /*!< minimum number of neighbors of all facesets */
} SUMA_FACESET_FIRST_EDGE_NEIGHB;

/*!
   structure containing surface curvature parameters
*/
typedef struct {
   int N_Node; /*!< Number of nodes in the surface */
   float **T1; /*!< N_Node x 3 matrix with each row specifying the 
                     1st principal direction of the surface */
   float **T2; /*!< N_Node x 3 matrix with each row specifying the 
                     2nd principal direction of the surface */
   float *Kp1; /*!< N_Node x 1 vector with each row specifying the 
                     curvature along the 1st principal direction */
   float *Kp2; /*!< N_Node x 1 vector with each row specifying the 
                     curvature along the 2nd principal direction */
   int N_SkipNode; /*!< number of nodes for which the curvature could 
                        not be computed */
} SUMA_SURFACE_CURVATURE;


/*! 
   structure containing the edges that make up a triangular faceset list

*/
typedef struct {
   SUMA_DO_Types do_type;  /*!< To check if this is a displayable object */
   int LinkedPtrType; /*!< Indicates the type of linked pointer */
   int N_links;   /*!< Number of links to this pointer */
   char owner_id[SUMA_IDCODE_LENGTH];   /*!< The id of whoever created 
                                 that pointer. Might never get used.... */

   
   char *idcode_str; /*!< ID of this particular edge list */
   int ** EL; /*!< pointer to where the Edge List ( N_EL x 2 ) will be placed
                        each row is an edge, i1 i2 where i1 is always <= i2
                        EL is sorted by row */
   int ** ELps; /*!< pointer to where the Edge List Property matrix 
                     ( N_EL x 3 )will be placed 
                  1st column, row i = 1 means edge i: i1,i2 was encountered as
                     i2->i1 in the triangle J (so it was flipped when stored in
                      EL) = -1 means edge i: i1,i2 was encountered as i1->i2 in 
                      the triangle J (so no flipping was done to store it in EL)
                  2nd column, row i = J is the triangle ( FL[J] ) that the 
                        segment belongs to. 
                  3rd column, row i = Numer of triangles that contain this edge. 
                     This number is positive for the first occurence
                     of the edge in EL, it is -1 afterwards. A decent edge has 2 
                     hosting triangles, an edge edge
                     has 1 hosting triangle. Bad edges come in all other colors*/
                        
   int *ELloc; /*!< N_ELloc x 1 vector that stores where each node's 
                     listing begins. 
                     ELloc is used to quickly find a certain edge in EL
                     to find the edge formed by nodes na-nb
                     find the minimum of na and nb (say it's nb)
                     the first reference of an edge containing nb 
                     starts at EL(ELloc(nb),:)
                     NOTE: ELloc contains an entry for each node in
                      FaceSetList, except the 
                     largest node index since that's never in the 
                     first column of EL */
   int N_ELloc; /*! Contains the number of values in ELloc */
                     
   int N_EL; /*!< Number of segments = 3 * N_Facesets */
   int N_Distinct_Edges; /*! Number of distinct edges 
                              (no multiple counts as in N_EL) */
   int max_N_Hosts; /*!< Maximum number of triangle hosts any one edge has 
                        (max ( ELps(:,2) != -1 ) )*/
   int  min_N_Hosts; /*!< Minimum version of max_N_Hosts */
   
   int **Tri_limb; /*!< each row j of Tri_limb contains the indices into EL 
                        (and ELps) of the edges that make it up */
   float *Le; /*!< Vector N_EL elements long containing 
                  the length of each edge in EL */
   float AvgLe; /*!< Average of Le (internodal distance).
                     This is an approximate average lenght, 
                     since some edges may counted more than others  */
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
   float *d;  /*!< Not implemented Distance of each node to the center 
                   of the shpere */
   float **dXYZ; /*!< Not implemented */
} SUMA_ISINSPHERE;

typedef struct {  /* Any changes in this structure must be mirrored 
                     in other DO structures. As a general rule,
                     don't mess with the order, append to the
                     bottom.
                     Fields prepended with private_ should not be
                     accessed outside of the accessor functions even
                     if they have the same offsets as their mirrors in 
                     other DOs.
                     */
   SUMA_DO_Types do_type;  /* This should always remain on top. 
                              Sync with SUMA_DSET struct */
   char *private_idcode_str; /* DO NOT ACCESS THIS POINTER directly from 
                        any SUMA_ALL_DO pointer. Use SUMA_ADO_idcode()
                        instead */
   char *private_Label;      /* DO NOT ACCESS THIS POINTER directly from 
                        any SUMA_ALL_DO pointer. Use SUMA_ADO_Label()
                        instead */
} SUMA_ALL_DO; /* See SUMA_ADO_* functions and iDO_ macros for a variety
                  of utility functions */
   
/*! Displayable Object Type */
typedef struct {
   void *OP;   /*!< Object Pointer */
   SUMA_DO_Types ObjectType; 
         /*!< Type of displayable object (redundant with OP->type) */
   SUMA_DO_CoordType CoordType; 
      /*!< Type of coordinate system that the object is attached to
         This is used to determine whether the object is drawn before or 
         or after the shift and rotation matrices are applied */
} SUMA_DO;


typedef struct {
   Widget toplevel;  /*!< toplevel widget of the text display window */
   Widget text_w; /*!<  text widget containing string to be displayed */
   Widget search_w;  /*!< widget of string search field */
   Widget text_output;  /*!< widget of search result field */
   SUMA_Boolean case_sensitive;  /*!< Case sensitive widget search */
   SUMA_Boolean allow_edit; /*!< allow editing of text displayed*/
   void (*OpenCallBack)(void *data); /*!< call back performed when 
                              SUMA_CreateTextShell is entered */
   void * OpenData;  /*!< data sent along with OpenCallBack */
   char *OpenDataType; /* The type of data struct in OpenData_Type */
   void (*DestroyCallBack)(void *data);   /*!< call back performed when 
                                 SUMA_DestroyTextShell is entered */
   void * DestroyData; /*!< data sent along with DestroyCallBack */
   SUMA_Boolean CursorAtBottom; /*!< If YUP then cursor is positioned at 
                                       end of text field */
   char *title; /* the title string */
   char *weblink;
} SUMA_CREATE_TEXT_SHELL_STRUCT; /*!< structure containing options and widgets 
                                       for the text shell window */

typedef enum {SUMA_OK_BUTTON, SUMA_APPLY_BUTTON, 
               SUMA_CLEAR_BUTTON, SUMA_CANCEL_BUTTON, 
               SUMA_HELP_BUTTON, SUMA_N_PROMPT_BUTTONS }SUMA_PROMPT_BUTTONS;

typedef enum { SUMA_OK, SUMA_OK_HELP, 
               SUMA_OK_CANCEL, SUMA_OK_CANCEL_HELP,
               SUMA_OK_CLEAR_CANCEL, SUMA_OK_CLEAR_CANCEL_HELP,
               SUMA_OK_APPLY_CANCEL, SUMA_OK_APPLY_CANCEL_HELP,
               SUMA_OK_APPLY_CLEAR_CANCEL, SUMA_OK_APPLY_CLEAR_CANCEL_HELP} SUMA_PROMPT_MODE;
               
typedef enum {
   SUMA_LSP_SINGLE, SUMA_LSP_BROWSE, SUMA_LSP_MULTIPLE, SUMA_LSP_EXTENDED
}  SUMA_ListSelectPolicy; /*!< Flags for motif list selection policy */

typedef struct {
   char ** clist; /*!< strings displayed in the Scrolled list window */
   int N_clist; /*!< Number of strings in clist */
   void **oplist; /*!< list of pointers to objects in the scrolled list */
   char *content_id; /*!< A string identifier to indicate which object
                          provided the content of this list */
} SUMA_ASSEMBLE_LIST_STRUCT;

/*!
   Structure containing widgets and settings for a list widget 
*/
typedef struct {
   Widget toplevel; /*!< top level shell for list */
   Widget rc;  /*!< rowcolumn containing all the widgets of the scrolled list */
   Widget list; /*!< list widget */
   
   Widget PosRef; /*!< Widget relative to which list is positioned */
   SUMA_WINDOW_POSITION Pos; /*! Position of list relative to PosRef*/
   SUMA_ListSelectPolicy SelectPolicy; /*!< Sets the XmNselectionPolicy resource:
                          SUMA_LSP_SINGLE: XmSINGLE_SELECT, 
                          SUMA_LSP_BROWSE: XmBROWSE_SELECT, 
                          SUMA_LSP_MULTIPLE: XmMULTIPLE_SELECT, 
                          SUMA_LSP_EXTENDED: XmEXTENDED_SELECT */
   SUMA_Boolean ShowSorted; /*!< Sort the list in alphabetical order */
   SUMA_Boolean RemoveDups; /*!< Remove duplicates in list */                        
   void (*Default_cb)(Widget w, XtPointer data, XtPointer calldata); /*!< callback to make when a default selection mode is made */ 
   void *Default_Data; /*!< pointer to data to go with Default_cb. If you pass NULL, the pointer to the List Widget is sent */
   void (*Select_cb)(Widget w, XtPointer data, XtPointer calldata); /*!< callback to make when a selection is made */ 
   void *Select_Data; /*!< pointer to data to go with Select_cb. If you pass NULL, the pointer to the List Widget is sent */
   void (*CloseList_cb)(Widget w, XtPointer data, XtPointer calldata); /*!< callbak to make when a selection is made */
   void *CloseList_Data; /*!< pointer to data to go with CloseList_cb. If you pass NULL, the pointer to the List Widget is sent */
   char *Label;
   SUMA_Boolean isShaded; /*!< YUP if the window is minimized or shaded, NOPE if you can see its contents */
   
   SUMA_ASSEMBLE_LIST_STRUCT *ALS; /*!< structure containing the list of strings shown in the widget and the pointers 
                                       of the objects the list refers to*/ 
   int lastitempos;
   
   int width; /* initial width in pixels of list area (excluding margins) */ 
} SUMA_LIST_WIDGET;

/*! structure containing widgets for surface viewer controllers ViewCont */
typedef struct {
   Widget TopLevelShell;/*!< Top level shell for a viewer's controller */
   Widget Mainform; 
   Widget ViewerInfo_pb;
   Widget Info_lb;
   SUMA_LIST_WIDGET *SwitchGrouplst; /*!< a structure containing widgets 
                                    and options for the switch Group list */
   SUMA_LIST_WIDGET *SwitchStatelst; /*!< a structure containing widgets 
                                 and options for the switch State list */
   SUMA_CREATE_TEXT_SHELL_STRUCT * ViewerInfo_TextShell; /*!< structure 
                  containing widgets and options of the viewer info text shell */
}SUMA_X_ViewCont;

typedef struct {
   SUMA_PROMPT_MODE Mode;
   SUMA_PROMPT_BUTTONS default_button; /*!< button to call when return key is hit in the text field.*/
   void (*SelectCallback)(char *selection, void *data); /*!< function called when a selection is made 
                                            See note for Preserve field*/
   void *SelectData; /*!< data sent along to SelectCallback */
   void (*CancelCallback)(void *data); /*!< function called when cancel or kill is called */
   void *CancelData; /*!< data sent along to CancelCallback */
   void (*HelpCallback)(void *data);
   void (*HelpData);
   int (*VerifyFunction)(char *word, void *data);
   void (*VerifyData); 
   Widget actionarea;
   Widget pane;
   Widget dialog; /*!< widget of dialog */
   Widget daddy; /*!< widget of parent */ 
   Widget text_w; /*!< Text entry widget */
   char *selection; /*!< What the lame user wrote */
   char *label; /*!< Label for the text field */
   SUMA_Boolean preserve; 
} SUMA_PROMPT_DIALOG_STRUCT; /*!< \sa similar fields in SUMA_SELECTION_DIALOG_STRUCT */

typedef enum { SUMA_FILE_OPEN, SUMA_FILE_SAVE } 
            SUMA_FILE_SELECT_MODE; /*!< mode of file selection dialog */

typedef struct {
   SUMA_FILE_SELECT_MODE Mode; 
   void (*SelectCallback)(char *filename, void *data); /*!< function called 
                                    when a selection is made 
                                    See note for Preserve field*/
   void *SelectData; /*!< data sent along to SelectCallback */
   void (*CancelCallback)(void *data); /*!< function called when cancel or 
                                            kill is called */
   void *CancelData; /*!< data sent along to CancelCallback */
   Widget dlg_w; /*!< widget of dialog */
   Widget daddy; /*!< widget of parent */
   char *filename; /*!< selected filename. 
               NOTE: This is only valid when a selection has been made */
   char *FilePattern; /*!< Pattern for filename filtering
                           Only relevant when window is opened */
   SUMA_Boolean preserve; /*!< If YUP, then widget is only unmanaged when 
                     selection is made or cancel is pressed. In 
                     this case, you should take care of dlg's safekeeping
                     and eventual destruction.
                     If Nope, then the widget is destroyed after selection
                     and/or cancel and the dlg structure is destroyed.
                     Be careful, if Preserve is NOPE, that your callbacks
                     do not return before being done with this structure*/ 
} SUMA_SELECTION_DIALOG_STRUCT;

/*!
   Structure containing widgets and settings of an arrow and or a text field
   
   - When adding fields to this stucture, make sure you initialize them
   appropriately in the functions SUMA_CreateTextField and SUMA_CreateArrowField
*/ 
typedef struct {
   Widget rc;  /*!< rowcolumn containing all the widgets of the arrow field */
   Widget textfield;  /*! text label */
   Widget up;     /*!< up arrow */
   Widget down;   /*!< down arrow */
   Widget label;  /*!< label widget */
   
   float step; /*!< increment */
   float min;  /*!< minimum value */
   float max;  /*!< maximum value */
   SUMA_Boolean wrap; /*!< YUP: wrap value in min-max range, else clip it*/
   float value;   /*!< current value */
   int cwidth; /*!< charcter spaces to save for widget */
   SUMA_VARTYPE type; /*!< SUMA_int or SUMA_float or SUMA_string */
   int direction; /*!< +1 up, -1 down */
   
   XtIntervalId arrow_timer_id; /*!< time out process id */
   
   void (*NewValueCallback)(void *data); /*!< callback when a new value is set */
   void *NewValueCallbackData; 
   SUMA_Boolean modified; /*!< set to YUP when user edits the value field */
   SUMA_Boolean arrow_action; /*!< set to YUP when user clicks one of the arrows */
} SUMA_ARROW_TEXT_FIELD; 

typedef enum { SUMA_ERROR_CELL, SUMA_ROW_TIT_CELL, SUMA_COL_TIT_CELL, SUMA_ENTRY_CELL } SUMA_CELL_VARIETY;

typedef struct{
   Widget rc;
   Widget rco;
   Widget *cells; /* table cells, Ncol x Nrow total */
   SUMA_Boolean HasColTit; /*!< YUP = table's 1st row is titles */
   SUMA_Boolean HasRowTit; /*!< YUP = table's 1st col is titles */
   int Ni;   /*!< Number of rows = Number of elements PER COLUMN (1st dim)*/
   int Nj;   /*!< Number of columns = Number of elements PER ROW (2nd dim)*/
   int *cwidth; /*!< charcter spaces to save for widget per column */
   byte *but_flag; /*!< Flags to indicate button status of a cell. That is 
                        to allow the highjacking of text fields to make them
                        toggle buttons. This should normally be used for column 
                        and row titles only. But you never know */
   float *num_value;   /*!< current value at each cell (for numeric cells)*/
   char **str_value;   /*!< current string at each cell (for string cells) */
   SUMA_Boolean editable;
   SUMA_VARTYPE type; /*!< SUMA_int or SUMA_float or SUMA_string */
   void (*NewValueCallback)(void *data); /*!< callback to make when 
                                              a new value is set */
   void *NewValueCallbackData;
   void (*TitLabelEVHandler)( Widget w , XtPointer cd , XEvent *ev , 
                              Boolean *ctd); 
   void *TitLabelEVHandlerData; 
   void (*CellEVHandler)(Widget w , XtPointer cd , XEvent *ev , Boolean *ctd); 
   void *CellEVHandlerData;
   int cell_modified; /*!< set to 1D index (column major) of cell_value edited, 
                           i = cell_modified % Ni, j = cell_modified / Ni 
                           cell_modified = j * Ni + i */
   SUMA_NUMERICAL_UNITS num_units;
   char **rowobject_id;
   char wname[64];
} SUMA_TABLE_FIELD;

typedef struct {
   Widget cmap_wid;  /*! GLXAREA widget for displaying colormap */
   int CrappyDrawable;
   float FOV;  /*! FOV for viewing colormap */
   GLXContext cmap_context;   /* graphic context for cmap */
   float translateVec[3];
} SUMA_CMAP_RENDER_AREA;

typedef struct {
   Widget *mw; /* MenuWidgets vector */
   int N_mw; /* number of widgets in MenuWidgets vector */
   SUMA_LIST_WIDGET *lw; /* list version of menu widgets */
   SUMA_ARROW_TEXT_FIELD *af; /* arrow field version of menu widgets*/
   int menu_type;
} SUMA_MENU_WIDGET;

typedef struct {
   Widget rc; /*! container row column */
   Widget lab; /*! Label */
   Widget sl;  /*! Slider widget */
   Widget tb;  /*! Toggle button for view slice */
   Widget text; /*! Text area where user enters slice number */
   Widget mont;  /* Text area where user enters montage */
   int Nslc;
   char *variant;
   char *slice_num_str;
   char *mont_str;
   float slice_num;
   float mont_num;
   float mont_inc;
   SUMA_NUMERICAL_UNITS slice_units;
   SUMA_NUMERICAL_UNITS mont_units;
   void (*NewValueCallback)(void *data); /*!< callback when a new value is set */
   void *NewValueCallbackData; 
   SUMA_Boolean modified; /*!< set to YUP when user edits the value field */
   char wname[64];
} SUMA_SLICE_FIELD;

typedef struct {
   Widget rc; /*! container row column */
   Widget lab; /*! Label */
   Widget tb;  /*! Toggle button for view slice */
   Widget tbs;  /*! Toggle button for allow selection */
   Widget text; /*! Text area where user enters slice number */
   int Nslc;
   char *N_slice_num_str;
   float N_slice_num;
   SUMA_NUMERICAL_UNITS N_slice_units;
   void (*NewValueCallback)(void *data); /*!< callback when a new value is set */
   void *NewValueCallbackData; 
   SUMA_Boolean modified; /*!< set to YUP when user edits the value field */
   char wname[64];
} SUMA_VR_FIELD;


/*! structure containing widgets for surface  controllers SurfCont */
typedef struct {
   /* *** DO NOT ADD ANYTHING BEFORE THESE FIELDS
          DO NOT CHANGE THE ORDER OF THESE FIELDS
          These fields are use for tracking copies
          (links) to a pointer.
          ANY CHANGES HERE SHOULD BE REFLECTED IN 
          SUMA_LinkedPtr structure 
   */
   SUMA_DO_Types do_type;  /*!< To check if this is a displayable object */
   int LinkedPtrType; /*!< Indicates the type of linked pointer */
   int N_links;   /*!< Number of links to this pointer */
   char owner_id[SUMA_IDCODE_LENGTH];   /*!< The id of whoever created that 
                                       pointer. Might never get used.... */
   
   int Open; /*!< Flag indicating that controller is open 
                  This was introduced to help deal with crashes on 
                  OS X 10.5 and 10.6*/
   Widget TLS;/*!< Top level shell for a Surface's controller */
   Widget PosRef; /*!< reference position widget */
   Widget Page; /*!< parent of Mainform, child of Notebook 
                         if using one controller */
   Widget Mainform; /*!< main form, child of TopLevelShell, or NoteBook*/
   Widget SurfInfo_pb; /*!< More info push button */
   Widget SurfInfo_label; /*!< Le label */
   Widget Mask_pb; /*!< Mask push button */
   Widget VSliceAtXYZ_tb; /*!< Make slices jump to crosshair location */
   SUMA_CREATE_TEXT_SHELL_STRUCT * SurfInfo_TextShell; /*!< structure containing 
                        widgets and options of the surface info text shell */
   SUMA_MENU_WIDGET *RenderModeMenu; /*!<[SW_N_SurfCont_Render] widgets 
                                       controlling the rendering mode menu */
   SUMA_MENU_WIDGET *TransModeMenu; /*!<[SW_N_SurfCont_Trans] widgets 
                                       controlling the transparency menu */
   SUMA_MENU_WIDGET *VTransModeMenu; /*!<[SW_N_SurfCont_Trans] widgets 
                                    controlling the volume transparency menu */
   SUMA_MENU_WIDGET *DsetViewModeMenu; /*!<[SW_N_SurfCont_DsetView]  widgets 
                                       controlling the dataset view mode menu */
   SUMA_MENU_WIDGET *DsetFontMenu; /*!<[SW_N_SurfCont_DsetFont] widgets                                   controlling the font displayed on graph nodes */
   SUMA_MENU_WIDGET *DsetNodeColMenu; /*!<[SW_N_SurfCont_DsetNodeCol] widgets                                   controlling the color mapping of graph nodes */
   SUMA_MENU_WIDGET *DsetTxtShadMenu; /*!<[SW_N_SurfCont_DsetTxtShad] widgets                                   controlling the shading/shadowing of txt */
   SUMA_MENU_WIDGET *DsetGmatBordMenu; /*!<[SW_N_SurfCont_DsetGmatBord] widgets                                   controlling the border thickness in GMATRIX */
   SUMA_MENU_WIDGET *DsetNodeRadMenu; /*!<[SW_N_SurfCont_DsetNodeRad] widgets                                   controlling the sizing of graph nodes */
   SUMA_MENU_WIDGET *DsetThroughMenu; /*!<[SW_N_SurfCont_DsetThrough] widgets                          controlling the way connections from a node are displayed */
   SUMA_MENU_WIDGET *DsetEdgeThickMenu; /*!<[SW_N_SurfCont_DsetEdgeThick] widgets                                   controlling the sizing of graph nodes */
   SUMA_MENU_WIDGET *DsetEdgeStipMenu; /*!<[SW_N_SurfCont_DsetEdgeStip] widgets                                   controlling the sizing of graph nodes */
   SUMA_MENU_WIDGET *TractStyleMenu; /*!<[SW_N_SurfCont_TractStyle] widgets                                   controlling the sizing of graph nodes */
   SUMA_MENU_WIDGET *DsetAlphaValMenu; /*!<[SW_N_SurfCont_DsetAlphaVal] widgets                                   controlling the sizing of graph nodes */
   SUMA_MENU_WIDGET *TractMaskMenu; /*!<[SW_N_SurfCont_TractMask] widgets 
                                controlling the masking method */
   Widget ColPlane_fr; /*!< the frame controlling the colorplanes */
   Widget Slice_fr;
   Widget DispFrame;
   Widget SurfFrame;
   Widget VR_fr;
   Widget DsetMap_fr; /*!< the frame for mapping Dset to colormap */
   Widget Xhair_fr; /*!< The frame for cross hair Info and controls */ 
   Widget SurfContPage_label; /*!< Le label */
   SUMA_ARROW_TEXT_FIELD *SurfContPage; /*!< arrow/text field  
                                              controlling color plane order */
   SUMA_ARROW_TEXT_FIELD *NodeRadGainAF; /*!< arrow/text field  
                                              controlling node radius gain */
   SUMA_ARROW_TEXT_FIELD *EdgeThickGainAF; /*!< arrow/text field  
                                              controlling node radius gain */
   SUMA_ARROW_TEXT_FIELD *ColPlaneOrder; /*!< arrow/text field  
                                              controlling color plane order */
   SUMA_ARROW_TEXT_FIELD *ColPlaneOpacity; /*!< arrow/text field 
                                             controlling color plane opacity */
   SUMA_ARROW_TEXT_FIELD *ColPlaneDimFact; /*!< arrow/text field 
                                             controlling color plane DimFact */
   SUMA_ARROW_TEXT_FIELD *ColPlaneAlphaThresh; /*!< arrow/text field 
                                           controlling color plane AlphaThresh */
   SUMA_ARROW_TEXT_FIELD *TractMaskGray; /*!< arrow/text field 
                                      controlling grayness of masked tracts */
   SUMA_TABLE_FIELD *SetRangeTable; /*!< structure for range setting table */
   SUMA_TABLE_FIELD *RangeTable; /*!< structure for range  table */
   SUMA_TABLE_FIELD *MaskTable;
   SUMA_TABLE_FIELD *MaskEvalTable;
   Widget MaskEval_tb;
   SUMA_Boolean UseMaskEval;
   SUMA_TABLE_FIELD *MaskLenTable;
   Widget MaskLen_tb;
   SUMA_TABLE_FIELD *XhairTable; /*!< structure for Cross hair  table */
   SUMA_TABLE_FIELD *NodeTable; /*!< structure for node index  table */
   SUMA_TABLE_FIELD *FaceTable;
   SUMA_TABLE_FIELD *DataTable;
   SUMA_TABLE_FIELD *LabelTable;
   SUMA_TABLE_FIELD *SetThrScaleTable; 
   SUMA_TABLE_FIELD *SetClustTable; /*!< structure for clust setting table */
   /* Obsolete since Nov 09,
      Replaced with DsetViewModeMenu 
      Widget ColPlaneShow_tb; *//*!< show/hide color plane */
   Widget ColPlaneShowOneFore_tb; /*!< show only one color plane at a time*/
   Widget GDSET_ShowBundles_tb; /*!< show bundles insted of edges*/
   Widget GDSET_ShowUncon_tb; /*!< show unconnected nodes */
   Widget SymIrange_tb; /*!< Symmetric intensity range */
   Widget AbsThresh_tb; /*!< absolute threshold */
   Widget ShowZero_tb; /*!< Show zero values */
   SUMA_LIST_WIDGET *SwitchDsetlst; /*!< a structure containing widgets and 
                                    options for the switch color plane list */
   SUMA_TABLE_FIELD *ColPlaneLabelTable; 
   SUMA_OVERLAYS *curColPlane; /*!< a copy of the pointer to the selected color 
                                    plane */
   SUMA_Boolean ShowCurForeOnly; /*!< Show current plane only out of the entire 
                                    stack */
   SUMA_Boolean GraphHidden; /*!< Graph update even in ShowCurForeOnly */
   void **prv_curDOp; /*!< copy of the pointer to the displayable object 
                       for which the controller is open. Private field
                       only access with SUMA_SurfCont_GetcurDOp() and
                       SUMA_SurfCont_SetcurDOp() 
                       This pointer should have some permanence, like a 
                       surface object or a graph dataset - Note that
                       I don't handle replacement of graph dataset pointers
                       yet. This field would need attention when I reload
                       a graph dataset. */
   char *prv_variant; /*!< Rendering variant for pointer in prv_curDOp
                       This is needed because the same DO pointer can be
                       rendered in multiple ways. This field is also private
                       and is set at the same time as prv_curDOp */
   SUMA_CMAP_RENDER_AREA *cmp_ren;   /* data for cmap rendering zone */
   Widget thr_sc;   /*! scale for threshold data */
   Widget brt_sc;   /*! scale for brightness data */
   SUMA_VR_FIELD *VR_fld;
   SUMA_SLICE_FIELD *Ax_slc;
   SUMA_SLICE_FIELD *Sa_slc;
   SUMA_SLICE_FIELD *Co_slc;
   Widget thr_lb;  /*! threshold title 
                        No longer used, 
                        using SetThrScaleTable instead */ 
   Widget thrstat_lb;  /*! pvalue associated with threshold */
   Widget cmaptit_lb;  /*! title of cmap */
   Widget cmapswtch_pb; /*! button for switching color map */
   SUMA_MENU_WIDGET *SwitchIntMenu; /* vector of widgets controlling 
                                       the switch intensity widgets */
   SUMA_MENU_WIDGET *SwitchThrMenu; /* vector of widgets controlling 
                                       the switch threshold widgets */
   SUMA_MENU_WIDGET *SwitchBrtMenu; /* vector of widgets controlling 
                                       the switch brightness widgets */
   #if 0 /* Now folded into SUMA_MENU_WIDGET struct */
   SUMA_ARROW_TEXT_FIELD *SwitchIntArrow;
   SUMA_ARROW_TEXT_FIELD *SwitchThrArrow;
   SUMA_ARROW_TEXT_FIELD *SwitchBrtArrow;
   
   SUMA_LIST_WIDGET *SwitchIntLst; /*!< list widget for switching intensity */
   SUMA_LIST_WIDGET *SwitchThrLst; /*!< list widget for switching intensity */
   SUMA_LIST_WIDGET *SwitchBrtLst; /*!< list widget for switching intensity */
   #endif
   SUMA_MENU_WIDGET *SwitchCmapMenu; /* vector of widgets controlling 
                                        the switch cmap widgets */
   #if 0 /* Now inside SUMA_MENU_WIDGET */
   int N_CmapMenu; /* Number of widgets in SwitchCmapMenu */
   #endif
   Widget rc_CmapCont; /* rc container to contain Cmap menu */
   SUMA_MENU_WIDGET *CoordBiasMenu; /* [SW_N_CoordBias]vector of widgets 
                                 controlling the coord bias widgets */
   SUMA_MENU_WIDGET *LinkModeMenu; /*[SW_N_LinkMode] vector of widgets 
                                 controlling the linking of I, T widgets */
   SUMA_MENU_WIDGET *CmapModeMenu; /* [SW_N_CmapMode] */
   Widget opts_rc; /*!< rowcolumn for color map, color bar and switch buttons */
   Widget opts_form; /*!< rowcolumn containing all options for colormapping */
   Widget rcvo; /*!< vertical rowcol for colormapping options */
   Widget rcsw; /*!<  rowcol for switching intensity, threshold and brightness */
   Widget rcsw_v1;/*!< rowcol containing Menu for Int. Thr. and Brt. */
   Widget rcsw_v2;/*!< rowcol containing toggle buttons for Int. Thr. and Brt. */
   Widget rcswr;  /*!< horizontal rowcol for Intensity column range label */
   Widget rccm;  /*!< rowcol containing colormap selectors and ranging options */
   Widget rccm_swcmap;
   Widget IntRange_lb; /*!< label widget containing range values */
   Widget Int_tb;/* Toggle buttons for intensity, threshold & brightness toys */
   Widget Thr_tb;
   Widget Brt_tb;
   Widget CmapLoad_pb;
   Widget Fake_pbar;
   int IntRangeLocked;
   int BrtRangeLocked;
   Widget rcclust; /*!< rowcol holding clusterizing options */
   Widget DeleteMask_pb;
   int DeleteMask_first;
   int DeleteMask_row;
   float tract_length_mask[2];
   int UseMaskLen;
}SUMA_X_SurfCont;

typedef struct {
   int N_rb_group; /*!< number of radio buttons in group */
   int N_but; /*!< number of buttons per radio button group */
   Widget *tb; /*!< vector of N_rb_group * N_but toggle button widgets */
   Widget *rb; /*!< vetor of N_rb_group radio box widget */
   Widget arb; /*!< widget of radiobox for all lock buttons */
   Widget *atb; /*!< widget of toggle buttons in arb */
}SUMA_rb_group;

/*! structure containing widgets for Suma's controller SumaCont */
typedef struct {
   Widget AppShell; /*!< AppShell widget for Suma's controller */
   Widget form;
   Widget LockFrame;
   Widget AppFrame;
   Widget quit_pb; /*!< quit push button */
   SUMA_Boolean quit_first;   /*!< flag indicating first press of done button */
   SUMA_rb_group *Lock_rbg; /*!< pointer to structure contining N radio 
                                 button groups */
   Widget *LockView_tbg;   /*!< vector of toggleview buttons */
   Widget LockAllView_tb;  /*!< widget of toggleAllview button */
   SUMA_CREATE_TEXT_SHELL_STRUCT *SumaInfo_TextShell;
}SUMA_X_SumaCont;


/*! structure containing widgets and data for the DrawROI window*/
typedef struct {
   Widget AppShell; /*!< AppShell widget for the DrawROI window*/ 
   Widget DrawROImode_tb; /*!< widget for toggling draw ROI mode */
   Widget ContROImode_tb;
   Widget form;
   Widget frame;
   Widget Penmode_tb;   /*!< widget for toggling draw with Pen mode */
   Widget AfniLink_tb; /*!< widget for toggling link to Afni */
   Widget ParentLabel_lb; /*!< widget for specifying a label for 
                              the parent surface */ 
   Widget Redo_pb;
   Widget Undo_pb;
   Widget Save_pb;
   Widget Load_pb;
   Widget Close_pb;
   Widget Finish_pb;
   Widget Join_pb;
   Widget Delete_pb;
   SUMA_Boolean Delete_first; /*! Flag indicating putton has been 
                                 pressed for the first time */
   SUMA_ARROW_TEXT_FIELD *ROIval; /*!< pointer to arrow field */
   SUMA_ARROW_TEXT_FIELD *ROIlbl; /*!< pointer to text field */
   SUMA_DRAWN_ROI *curDrawnROI; /*!< A pointer to the DrawnROI structure
                                    currently in use by window.
                                    This is a copy of another pointer, 
                                    NEVER FREE IT*/
   SUMA_LIST_WIDGET *SwitchROIlst; /*!< a structure containing widgets and 
                                    options for the switch ROI list */
   int SaveWhat;  /*!< option for determining what ROI to save, acceptable values
                     are in SUMA_WIDGET_INDEX_DRAWROI_SAVEWHAT */
   int SaveMode;  /*!< option for determining format of ROI to save, acceptable
                     values are in SUMA_WIDGET_INDEX_DRAWROI_SAVEMODE */ 
   int WhatDist;  /*!< option for determining format of ROI to save, acceptable  
                        values are in SUMA_WIDGET_INDEX_DRAWROI_SAVEMODE */ 
   SUMA_MENU_WIDGET *SaveModeMenu; /*!<[SW_N_DrawROI_SaveMode] set of 
                           widgets for SaveMode menu */
   SUMA_MENU_WIDGET *SaveWhatMenu; /*!<[SW_N_DrawROI_SaveWhat] set of 
                           widgets for SaveWhat menu */
   SUMA_MENU_WIDGET *WhatDistMenu; /*!<[SW_N_DrawROI_WhatDist] set of 
                           widgets for SaveWhat menu */
} SUMA_X_DrawROI;


typedef struct {
   GLXContext last_context;
   Display *last_context_DPY;
   Window last_context_WDW;
   char setting_function[64];
   char widget_label[64];
} SUMA_GLCONTEXT_RECORD;               
               
/*! structure containg X vars for surface viewers*/
typedef struct {
   Display *DPY; /*!< display of toplevel widget */
   Widget TOPLEVEL, FORM, FRAME, GLXAREA;
   XVisualInfo *VISINFO;
   GLXContext GLXCONTEXT;
   Colormap CMAP;
   Bool DOUBLEBUFFER;
   char *Title; 
   int REDISPLAYPENDING, CrappyDrawable;
   int aWIDTH, aHEIGHT; /* These are the widths and heights of the drawing area, 
                         rather than the whole window */
   XtWorkProcId REDISPLAYID;
   XtIntervalId MOMENTUMID;
   GC gc;
   SUMA_X_ViewCont *ViewCont; /*!< pointer to structure containing viewer 
                                    controller widget structure */
   Widget ToggleCrossHair_View_tglbtn; /*!< OBSOLETE Toggle button in 
                                             View-> menu */
   SUMA_MENU_WIDGET *FileMenu; /*!<[SW_N_File] File Menu Vector of widgets */   
   SUMA_MENU_WIDGET *ToolsMenu; /*!<[SW_N_Tools]of widgets tools menu */
   SUMA_MENU_WIDGET *ViewMenu; /*!< [SW_N_View]Vector of widgets  View Menu */
   SUMA_MENU_WIDGET *HelpMenu; /*!< [SW_N_Help]Vector of widgets  Help Menu */
   SUMA_PROMPT_DIALOG_STRUCT *LookAt_prmpt; /*!< structure for LookAt dialog */
   SUMA_PROMPT_DIALOG_STRUCT *SetRot_prmpt; /*!< structure for set rotation 
                                                dialog */
   SUMA_PROMPT_DIALOG_STRUCT *JumpIndex_prmpt; /*!< structure for the 
                                                   Jump To Index dialog */
   SUMA_PROMPT_DIALOG_STRUCT *JumpXYZ_prmpt; /*!< structure for the 
                                                   Jump To XYZ dialog */
   SUMA_PROMPT_DIALOG_STRUCT *JumpFocusNode_prmpt; /*!< structure for setting                                                   Focus Node dialog */
   SUMA_PROMPT_DIALOG_STRUCT *JumpFocusFace_prmpt; /*!< structure for setting                                                   Focus FaceSet dialog */
   SUMA_PROMPT_DIALOG_STRUCT *HighlightBox_prmpt; /*!<  structure for 
                                             highlighting nodes in Box dialog */
   SUMA_PROMPT_DIALOG_STRUCT *SetRenderOrder_prmpt; 
}SUMA_X;

/*! structure containg X vars common to all viewers */
typedef struct {
   SUMA_X_SumaCont *SumaCont; /*!< structure containing widgets 
                                    for Suma's controller */
   SUMA_X_DrawROI *DrawROI; /*!< structure containing widgets for 
                                 DrawROI window */
   XtAppContext App; /*!< Application Context for SUMA */
   Display *DPY_controller1; /*!< Display of 1st controller's top level shell */
   SUMA_XRESOURCES X_Resources; /*!< flag specifying the types of 
                                    resources to use */
   SUMA_CREATE_TEXT_SHELL_STRUCT *Help_TextShell; /*!< structure containing 
                                    widgets and options of SUMA_help window */
   SUMA_CREATE_TEXT_SHELL_STRUCT *Help_Cmap_TextShell; /*!< structure containing
                                 widgets and options of colormap help window */
   SUMA_CREATE_TEXT_SHELL_STRUCT *Help_Plot_TextShell;
   SUMA_CREATE_TEXT_SHELL_STRUCT *Whereami_TextShell;
   SUMA_CREATE_TEXT_SHELL_STRUCT *Log_TextShell; /*!<  structure containing
                                       widgets and options of SUMA_log window */
   SUMA_SELECTION_DIALOG_STRUCT *FileSelectDlg; /*!< structure containing widgets
                              and options of a generic file selection dialog */
   SUMA_PROMPT_DIALOG_STRUCT *N_ForeSmooth_prmpt; /*!< structure for the number  
                           of foreground smoothing dialog */
   SUMA_PROMPT_DIALOG_STRUCT *N_FinalSmooth_prmpt; /*!< structure for the number 
                           of final smoothing dialog */
   int NumForeSmoothing;   /*!< Number of steps for smoothing the foreground  
                                colors prior to mixing with background. Default 
                                is set by environment variable 
                                SUMA_NumForeSmoothing which 
                                is set to 0 (No smoothing). */
   int NumFinalSmoothing;   /*!< Number of steps for smoothing the blended  
                                colors (after foreground is mixed with
                                background). Default 
                                is set by environment variable 
                                SUMA_NumFinalSmoothing which 
                                is set to 0 (No smoothing). */
   SUMA_Boolean WarnClose; /*!< Pops up a window to double check 
                                 before SUMA quits */
   SUMA_LIST_WIDGET *SwitchCmapLst; /*!< list widget for switching colormaps */
   SUMA_PROMPT_DIALOG_STRUCT *Clip_prmpt; /*!< structure for clipping dialog */
   SUMA_PROMPT_DIALOG_STRUCT *ClipObj_prmpt; /*!< structure for clip obj dialg */
   
   XmFontList TableTextFontList; /*! Font list for table's text fields */
   
   SUMA_Boolean UseSameSurfCont; /* Use one surface controller for all surfs? */
   SUMA_Boolean SameSurfContOpen;
   Widget CommonSurfContTLW; /* If not null, then surface controller will
                                   all be sharing this top level widget */
   Widget SC_Notebook;  /* Surface Controllers Notebook */
   int ButtonDown; /* Is mouse button pressed? */
   
   int roffx; /* relative offset of widget in window */
   int roffy;
   
   SUMA_GLCONTEXT_RECORD *Cr;
   
   SUMA_X_SurfCont *AllMaskCont;
   int MaskStateID; /* A flag that is updated anytime masks are modified
                       requiring a new computation of intersections */
}SUMA_X_AllView;

/*! structure defining a cross hair */
typedef struct {   
   GLfloat XaxisColor[4] ;
   GLfloat YaxisColor[4] ;
   GLfloat ZaxisColor[4] ;
   
   GLfloat LineWidth;
   SUMA_STIPPLE Stipple; /*!< dashed or solid line */

   GLfloat c[3]; /*!< Cross Hair center */
   float c_noVisX[3]; /*!< Cross Hair center without VisX transforms */
   GLfloat r; /*!< Cross Hair radius */
   GLfloat g; /*!< 1/2 of gap between center and ray 
                  (should be less than radius/2) */
   
   SUMA_Boolean ShowSphere; /*!< YUP/NOPE, starting to regret this. */
   GLUquadricObj *sphobj; /*!< quadric object, representing central sphere */
   GLfloat sphcol[4]; /*!< Sphere color */
   GLdouble sphrad; /*!< Sphere radius */
   GLint slices; /*!< think pizza */
   GLint stacks; /*!< think lattitudes */
   
   int adoID; /*!<  If the cross hair is tied to a surface/DO, adoID 
                        contains the index into SUMAg_DOv of that surface. 
                        -1 if that cross hair is wild and loose */
   int datumID; /*!< a node/datum from adoID can be associated with the cross 
                     hair (-1 for nothing) */   
   int secID;   /*!< a secondary selection like FaceSet or Node of a graph. */
   GLUquadricObj *sphobjCmax; /*!< quadric object, representing Max cluster */
   GLfloat sphcolCmax[4]; /*!< Sphere color */
}SUMA_CrossHair;   

typedef struct {      
   GLUquadricObj *sphobj; /*!< quadric object, representing central sphere */
   GLfloat sphcol[4]; /*!< Sphere color */
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
   GLfloat LineCol[4]; /*!< LineColor of Edge*/
   GLfloat NormVect[3]; /*!< normal vector of faceset, two triangles are drawn at a small distance from the selected FaceSet */
}SUMA_FaceSetMarker;



/*!
   Structure containg a bunch of segments defined between n0 and n1
*/
typedef struct {
      /* FIRST VARIABLES MUST RETAIN THEIR ORDER and follow SUMA_ALL_DO */
   SUMA_DO_Types do_type; 
   char *idcode_str;    /*!< unique idcode for DO */
   char *Label; /*!< ascii label for DO */ 
   
      /* These next three are to follow their equivalent is in SUMA_NB_DO */
   int NodeBased; /*!< flag: 1 if segments are formed by vectors at surface 
                             nodes */
   char *Parent_idcode_str; /*!< Parent surface's id 
                                 (only used if NodeBased = 1)*/
   int *NodeID; /*!< ID of the node at which the vector is represented
                     NULL if NodeBased = 0 */
                     
      /* Begin specific fields */               
   SUMA_DO_Types Parent_do_type;
   char *DrawnDO_variant;
   int *NodeID1; /*!< Used to define the 2 node of vectors that are fully
                      nodebased */
   
   GLfloat *n0; /*!< vector containing XYZ of nodes 1 (3*N_n elements long)
                     NULL if NodeBased*/
   GLfloat *n1; /*!< vector containing XYZ of nodes 2 (3*N_n elements long)*/
   
   GLUquadricObj *topobj; /*!< quadric object, representing n1 */
   GLUquadricObj *botobj; /*!< quadric object, representing n0 */
   
   int N_n; /*!< Number of elements in n0 and n1 */
   
   GLfloat LineWidth; /*!< Common LineWidth of all segment*/
   GLfloat LineCol[4]; /*!< Common LineColor of all segments*/
   GLfloat *colv; /*!< Vector of segment colors, 4 elements per segment. 
                        NULL if using LineCol */
   GLfloat *thickv; /*!< Vector of segment thincknesses, 
                         1 elements per segment. NULL if using LineWidth */
   SUMA_STIPPLE Stipple; /*!< dashed or solid line */
   GLushort *stipv;  /* stippling pattern for each seg. */
   int Mstip;
   GLushort stip;    /* common stippling pattern */
   
   int N_SegNodes; /* Number of unique nodes that are used to form the segments
                      That is the number of elements in the set formed by NodeID1
                      and NodeID, -1 if it is not set */
   int N_AllNodes; /* All defined nodes in parent set */
} SUMA_SegmentDO;

typedef struct {
   char *parent_ID;
   DList *Edges;
   DList *Points;
   DList *Nodes;
   DList *Triangles;
} SUMA_STRIP;

/*!
   Structure containg a bunch of spheres 
*/
typedef struct {
      /* FIRST VARIABLES MUST RETAIN THEIR ORDER and follow SUMA_ALL_DO */
   SUMA_DO_Types do_type;
   char *idcode_str;    /*!< unique idcode for DO */
   char *Label; /*!< ascii label for DO */ 
   
      /* These next three are to follow their equivalent is in SUMA_NB_DO */
   int NodeBased; /*!< flag: 1 if segments are formed by vectors at nodes */
   char *Parent_idcode_str; /*!< Parent surface's id 
                                 (only used if NodeBased = 1
                                 NULL if NodeBased)*/
   int *NodeID; /*!< ID of the node at which the vector is represented
                     NULL if NodeBased = 0 */
                     
                     
       /* Begin specific fields */              
   GLfloat *cxyz; /*!< vector containing XYZ of centers (3*N_n elements long)*/
   GLUquadricObj *sphobj; /*!< quadric object, representing central sphere */
   int N_n; /*!< Number of spheres */
   GLfloat LineWidth; /*!< LineWidth*/
   GLfloat CommonRad; /*!< common radius */
   GLfloat CommonCol[4]; /*!< common colors */
   GLint CommonSlices; /*!< think pizza */
   GLint CommonStacks; /*!< think lattitudes */
   GLfloat *radv; /*!< Vector of sphere radii, 1 elements per sphere. 
                        NULL if using CommonRad */
   GLfloat *colv; /*!< Vector of sphere colors, 4 elements per sphere. 
                        NULL if using CommonCol */
   GLenum CommonStyle; /*!< Common sphere drawing style. 
            Choose from: GLU_FILL, GLU_LINE, GLU_SILHOUETTE, GLU_POINT */
   GLenum *stylev; /*!< Vector of sphere styles */
   
}SUMA_SphereDO;

/* A structure to hold tract masking results with boolean expressions */
typedef struct {
   int N_vals;          /* Number of vals (tracts) in DO to be intersected */
   byte varcol[26*4];   /* RGB colors associated with each of the 26 variables
                           'a' to 'z' */
   byte varsused[26];   /* varsused[k] = 1 --> variable 'a'+k is in 'expr' */
   char varsmdo[26][1+SUMA_IDCODE_LENGTH]; /* varsmdo[k] contains the IDcode
                                             of the object (MaskDO) corresponding
                                             to variable 'a'+k */
   char mdoused[26*(1+SUMA_IDCODE_LENGTH)]; /* A brute catenation of IDcodes 
                                         of DOs referenced by the variables */
   char allvarsineq[27]; /* A catenation of all variables in 'expr' */
   byte **marr;          /* a pointer to 26 (byte *) pointers to arrays of
                          N_vals bytes each. marr[k] is NULL is varsused[k]=0 */
   char *expr;    /* The boolean expression */
} SUMA_MASK_EVAL_PARAMS;

typedef struct {
      /* FIRST VARIABLES MUST RETAIN THEIR ORDER and follow SUMA_ALL_DO */
   SUMA_DO_Types do_type; 
   char *idcode_str;
   char *Label;
   
      /* Not sure whether or not these things need to be moved into 
         the common stuff in SUMA_ALL_DO yet ... */
   void *Saux; /* A pointer to a structure for SUMA's use */
   void (*FreeSaux)(void *Saux); /* Function to free Saux */
   int N_datum; /* Here number of control points forming tracts */
   
      /* Begin specific fields */
   char *Parent_idcode_str;
   
   TAYLOR_NETWORK *net; 

   GLfloat LineWidth; /*!< Common LineWidth of all segment*/
   GLfloat LineCol[4]; /*!< Common LineColor of all segments*/
   GLfloat *colv; /*!< Vector of segment colors, 4 elements per segment. 
                        NULL if using LineCol */
   GLfloat *thickv; /*!< Vector of segment thincknesses, 
                         1 elements per segment. NULL if using LineWidth */
   SUMA_STIPPLE Stipple; /*!< dashed or solid line */

   SUMA_MASK_EVAL_PARAMS *mep;
   
   byte *tmask;
   int N_tmask;
   int MaskStateID;
   byte *tcols;
   byte usetcols;
} SUMA_TractDO;

typedef struct {
      /* FIRST VARIABLES MUST RETAIN THEIR ORDER and follow SUMA_ALL_DO */
   SUMA_DO_Types do_type;
   char *idcode_str;
   char *Label;
      
      /* Begin specific fields */
   char *Parent_idcode_str;
   char *variant;
} SUMA_GraphLinkDO;

typedef struct {
      /* FIRST VARIABLES MUST RETAIN THEIR ORDER and follow SUMA_ALL_DO */
   SUMA_DO_Types do_type; 
   char *idcode_str;    /*!< unique idcode for DO */
   char *Label; /*!< ascii label for DO */ 
      
      /* Any change below should be reflected in all stucts that
         can get typecast to SUMA_NB_DO */
   int NodeBased; /*!< flag: 1 if segments are formed by 
                        vectors at surface nodes */
   char *Parent_idcode_str; /*!< Parent surface's id 
                                 (only used if NodeBased = 1
                                 NULL if NodeBased)*/
   int *NodeID; /*!< ID of the node at which the vector is represented
                     NULL if NodeBased = 0 */
 
} SUMA_NB_DO; /*!< generic struct for common fields to Node-Based DOs */

/*!
   Structure containg a bunch of planes 
*/
typedef struct {
      /* FIRST VARIABLES MUST RETAIN THEIR ORDER and follow SUMA_ALL_DO */
   SUMA_DO_Types do_type; 
   char *idcode_str;    /*!< unique idcode for DO */
   char *Label; /*!< ascii label for DO */ 
   
      /* Begin specific fields */
   GLfloat *cxyz; /*!< vector containing XYZ of centers (3*N_n elements long)*/
   GLfloat *pleq; /*!< plane equations 4*N_n elements long */
   int N_n; /*!< Number of planes */
   GLfloat LineWidth; /*!< LineWidth*/
   GLfloat CommonBoxDims[3] ; /*!< common dimensions of box containing 
                                    plane (centered on cxyz) */
   GLfloat CommonCol[4]; /*!< common colors */
   GLfloat *boxdimv; /*!< Vector of box dimensions radii, 
                          3 elements per plane. NULL if using CommonBoxDims */
   GLfloat *colv; /*!< Vector of plane colors, 4 elements per plane. 
                        NULL if using CommonCol */
   GLfloat *NodeList;
   GLint *FaceSetList;
   GLfloat *nodecol;
   GLfloat *NodeNormList;
   int N_Node;
   int N_FaceSet;
   SUMA_RENDER_MODES PolyMode; /*!< polygon viewing mode, SRM_Fill, 
                                    SRM_Line, SRM_Points */
   SUMA_TRANS_MODES TransMode; /*!< polygon transparency  */
}SUMA_PlaneDO;


/*! Structure containing the communication info and status with AFNI */
typedef struct {
   SUMA_Boolean Connected;   /*!< flag indicating connection state */
   int ConSock;
   
} SUMA_AfniCom;

/* structure defining the former state of a surface viewer window */
typedef struct {
   float ViewFrom[3]; /*!< Location of observer's eyes */
   float ViewFromOrig[3]; /*!< Original Location of observer's eyes */
   float ViewCenter[3];   /*!< Center of observer's gaze */
   float ViewCenterOrig[3];   /*!< Original Center of observer's gaze */
   float ViewCamUp[3];   /*!< Camera Up direction vector */
   float ViewDistance; /*!< Viewing distance */
   float FOV; /*!< Field of View (affects zoom level)*/
   float Aspect;   /*!< Aspect ratio of the viewer*/
} SUMA_ViewState_Hist;

/* A structure to quickly locate DO in DOv
   Originally, the structure was nothing but an integer.
   But this integer becomes useless when SUMAg_DOv gets
   scrambled, mostly when certain objects get deleted.
   The more reliable string ID is now included along
   with a bunch of Refresh functions to reset the dov_ind
   via idcode_str directly */
typedef struct {
   int dov_ind; /* index of object into SUMAg_DOv */
   char idcode_str[SUMA_IDCODE_LENGTH]; /* id of object in question */
} SUMA_DO_LOCATOR;

/*! structure defining the viewing state of the viewer window */
typedef struct {
   char *Name; /*!< The name of the viewing state, fiducial, inflated, etc .. */
   SUMA_Boolean AnatCorrect; /* is this state anatomically correct? */
   char *Group; /*!< The group to which the viewing state belongs. */
   SUMA_DO_LOCATOR *MembDO; 
   int N_MembDO; /*!< Number of members in MembDO. 
                     Only SOs that are in MembDO can
                     be placed into RegisteredDO of the viewer in a particular 
                     viewing state.*/                  
   SUMA_ViewState_Hist *Hist; /*!< Pointer to structure containing various 
                                   parameter settings for that viewing state */
} SUMA_ViewState;

/*! structure containing the geometric settings for viewing the surface */
typedef struct {
   float DimSclFac;
   float ViewFrom[3]; /*!< Location of observer's eyes */
   float ViewFromOrig[3]; /*!< Original Location of observer's eyes */
   float ViewCenter[3];   /*!< Center of observer's gaze */
   float ViewCenterOrig[3];   /*!< Original Center of observer's gaze */
   float ViewCamUp[3];   /*!< Camera Up direction vector */
   float ViewDistance; /*!< Viewing distance */
   
   float translateBeginX; /*!< User Input (mouse) X axis current position 
                               for translation */
   float translateBeginY; /*!< User Input (mouse) Y axis current position 
                               for translation */
   float translateDeltaX;   /*!< User Input (mouse) X axis position increment 
                                 for translation */
   float translateDeltaY;   /*!< User Input (mouse) Y axis position increment 
                                 for translation */
   float TranslateGain;   /*!< gain applied to mouse movement */
   float ArrowtranslateDeltaX;   /*!< User Input (Keyboard) X axis position 
                                      increment for translation */
   float ArrowtranslateDeltaY;   /*!< User Input (Keyboard) X axis position 
                                      increment for translation */
   GLfloat translateVec[3];      /*!< translation vector, in screen coordinates, 
                                 equal to [translateDeltaX translateDeltaY]. 
                                 The third translation (Z dimension) is 0.0*/
   GLfloat RotaCenter[3];   /*!<Center of Rotation */
   float zoomDelta;       /*!< Zoom increment */
   float zoomBegin;    /*!< Current zoom level*/
   float spinDeltaX;   /*!< User Input (mouse) X axis position increment 
                            for spinning*/
   float spinDeltaY;   /*!< User Input (mouse) Y axis position increment 
                            for spinning*/
   float spinBeginX;  /*!< User Input (mouse) X axis current position 
                           for spinning */
   float spinBeginY;  /*!< User Input (mouse) Y axis current position for 
                           spinning */
   int MinIdleDelta;  /*!< minimum spinDeltaX or spinDeltaY to initiate momentum 
                           rotation */
   float deltaQuat[4];   /*!< Quaternion increment */
   float currentQuat[4]; /*!< Current quaternion */
   float lastQuat[4]; /*!< Quaternion last time we displayed*/
   Boolean ApplyMomentum;   /*!< Turn momentum ON/OFF */
   
   float vLHpry[3];
   float vLHpry0[3];
   int LHlol;           /*!< if 1 then the left hemi starts up displayed
                             on the left side of the screen.
                             if -1 then left starts on the right.
                             This flag is only set when both left and 
                             right surfaces are present */
} SUMA_GEOMVIEW_STRUCT; /* DO NOT ADD POINTERS TO THIS STRUCT, otherwise you
                           need to change SUMA_CopyGeomViewStruct() and maybe
                           SUMA_DiffGeomViewStruct() */

/*! structure holding the pointer the node color assignment and a bit more */
typedef struct {
   SUMA_DO_Types do_type;  /*!< To check if this is a displayable object */
   int LinkedPtrType; /*!< Indicates the type of linked pointer */
   int N_links;   /*!< Number of links to this pointer */
   char owner_id[SUMA_IDCODE_LENGTH];   /*!< The id of whoever created that 
                                       pointer. Might never get used.... */
   
   GLfloat *glar_ColorList_private; /*!< pointer to the 1D ColorList array */
   int N_glar_ColorList; /*!< Number of elements in glar_ColorList 4xNumber 
                              of nodes in the surface or edges in a graph */
   char *idcode_str; /*!< string containing the idcode of the surface/object to 
                         which glar_ColorList belongs*/
   SUMA_Boolean Remix; /*!< flag indicating that colors need to be remixed */ 
   int RemixID; /*!< An id of sorts updated each time remix is done */
   int per_sv_extra[SUMA_MAX_SURF_VIEWERS]; /* Some flags needed to perform extra
                                    tasks (binding textures) without needing to 
                                    do the whole remix thing. This is needed when
                                    ColList is shared amongst multiple viewers to
                                    save memory */
} SUMA_COLORLIST_STRUCT;

typedef enum { SUMA_STD_ZERO_CENTERED, SUMA_SCALE_BOX } SUMA_AxisType;

/*! structure defining an axis object */
typedef struct {
      /* FIRST VARIABLES MUST RETAIN THEIR ORDER and follow SUMA_ALL_DO */
   SUMA_DO_Types do_type; 
   char *idcode_str; /*! idcode of axis */
   char *Label;
      
      /* Begin specific fields */
   SUMA_AxisType atype;
   GLfloat XaxisColor[4] ;
   GLfloat YaxisColor[4] ;
   GLfloat ZaxisColor[4] ;
   
   GLfloat LineWidth;
   SUMA_STIPPLE Stipple; /*!< dashed or solid line */
   
   GLfloat XYZspan[3]; /*!< the axis will span +/- span[i] 
                            in the three dimensions */
   GLfloat Center[3]; /*!< origin of axis */
   GLfloat BR[3][2]; /*!< Box Range values */
   double MTspace;   /*!< Major tick spacing */
   double MTsize;    /*!< Major tick size */
   double mTspace;   /*!< Minor tick spacing */
   double mTsize;    /*!< Minor tick size */
   int DoCross;      /*!< if 1 then ticks are centered on line. 
                         (total length is same as *Tsize value)*/
}SUMA_Axis;

typedef struct {
   int N;   /*!< Number of points in vectors x and y */
   int Nalloc; /*!< Number of elements allocated for in x and y */
   int *x;  /*!< vector containing x coordinates */
   int *y;  /*!< vector containing y coordinates */
   float *NPv; /*!< vector containing x y z triplets of near plane selection points */
   float *FPv; /*!< vector containing x y z triplets of far plane selection points */   
   int *SurfNodes; /*!< vector containing indices of nodes corresponding to the 
                        intersection between line [ NPv[j] FPv[j] ] and surface object */   
   int *SurfTri; /*!< vector containing indices of triangles corresponding to the 
                        intersection between line [ NPv[j] FPv[j] ] and surface object */
   int *ProjectionOf; /*!< if ProjectionOf[31] = 78; it means SurfNodes[31] 
                           is the intersection between line [ NPv[78] FPv[78] ] and the surface object. */
   int N_SurfNodes;  /*!< NUmber of SurfNodes in SurfNodes (always <= N) */
   
} SUMA_OLD_BRUSH_STROKE; /*!< Structure containing the path of the mouse in the viewer window. 
                        See functions SUMA_CreateBrushStroke(), SUMA_AddToBrushStroke(), 
                        SUMA_ClearBrushStroke(), SUMA_ShowBrushStroke()*/


typedef struct {
   float x; /*!< x screen coordinate. This is typically an integer value except in places of interpolation*/
   float y; /*!< y screen coordinate. This is typically an integer value except in places of interpolation*/
   
   float NP[3];   /*!< x y z triplet of near plane selection point */
   float FP[3];   /*!< x y z triplet of far plane selection point */
   
   int SurfNode;  /*!< index of node corresponding to the 
                        intersection between line [NP FP] and surface object.
                        initialized to -1 */
   int SurfTri;   /*!< index of triangle corresponding to the 
                        intersection between line [NP FP] and surface object.
                        initialized to -1 */  
   SUMA_Boolean Decimated; /*!< Flag to indicate if datum was obtained by a mouse trace (NOPE)
                               or through decimation (YUP)*/                      
} SUMA_BRUSH_STROKE_DATUM; /*!< Data structure for the doubly linked version of brushstroke.  */

typedef enum {  SUMA_NO_WAX, SUMA_THREE_WAX, SUMA_THREE_TEXT_WAX, SUMA_BOX_WAX, SUMA_BOX_TEXT_WAX, SUMA_N_WAX_OPTIONS } SUMA_WORLD_AXIS_TOGGLE_METHODS;
/*! structure defining the state of a viewer window */

typedef struct {
   char Label[256];
   int ALPHA_TEST;
   int DEPTH_TEST;
   int COLOR_MATERIAL;
   int TEXTURE_2D;
   int TEXTURE_3D_EXT;
   int TEXTURE_3D;
   int TEXTURE_GEN_S;
   int TEXTURE_GEN_T;
   int TEXTURE_GEN_R;
   int CLIP_PLANE0;
   int CLIP_PLANE1;
   int CLIP_PLANE2;
   int CLIP_PLANE3;
   int CLIP_PLANE4;
   int CLIP_PLANE5;
   int LIGHTING;
   int LIGHT0;
   int LIGHT1;
   int LIGHT2;
   int BLEND;
   int LINE_SMOOTH;
   int ColMatParam;
   int ColMatFace;
   GLfloat CurCol[4];
} SUMA_EnablingRecord;

/* a structure for the list of pickable objects (excluding surfaces) */
typedef struct {
   SUMA_DO_Types ref_do_type;
   char *ref_idcode_str;
   char *idcode_str;
   
   char *primitive;
   char *variant;
   char *Label;
   long int i0;
   long int i1;
   long int idatum;
} SUMA_COLID_OFFSET_DATUM;

#define SUMA_NET_BUN          0  /* selectedNetworkBundle */
#define SUMA_VOL_I            0  /* selectedVoxelI */
#define SUMA_SURF_TRI         0  /* selected triangle */
#define SUMA_ENODE_0          0  /* 1st point/node forming edge */

#define SUMA_BUN_TRC          1  /* selectedBundleTract */
#define SUMA_VOL_J            1  /* selectedVoxelJ */
#define SUMA_ENODE_1          1  /* 2st point/node forming edge */

#define SUMA_TRC_PNT          2  /* selectedTractPoint */
#define SUMA_VOL_K            2  /* selectedVoxelK */

#define SUMA_NET_TRC          3  /* selectedNetworkTract */
#define SUMA_VOL_IJK          3  /* selectedVoxelIJK, 1D index */

#define SUMA_VOL_SLC_NUM      4  /* selected slice, 1D index */
#define SUMA_VOL_SLC_VARIANT  5  /* selected slice, 1D index */

#define SUMA_N_IALTSEL_TYPES  6 /* If this goes over 15, make sure you 
                                 start using something bigger than
                                 iv15 in SUMA_Engine Calls */

#define SUMA_VOL_SLC_EQ0 0 /* 1st coefficient of plane equation */

#define SUMA_VOL_SLC_EQ1 1 /* 2nd coefficient of plane equation */

#define SUMA_VOL_SLC_EQ2 2 /* 3rd coefficient of plane equation */

#define SUMA_VOL_SLC_EQ3 3 /* 4th coefficient of plane equation */

#define SUMA_N_DALTSEL_TYPES 4 /* If this goes over 15, make sure you 
                                 start using something bigger than
                                 fv15 in SUMA_Engine Calls */

/* 
   *************** Convolution utilities *************** 
   based on example in glut's convolve.c by  
   Tom McReynolds, SGI 
   *****************************************************
*/
/* convolution choices */
typedef enum {
      SUMA_CONV_NONE, 
      SUMA_CONV_BOX_3X3,
      SUMA_CONV_BOX_5X5,
      SUMA_CONV_SOBEL_X,
      SUMA_CONV_LAPLACE,
      SUMA_N_CONV_MODES,
} SUMA_CONV_MODES;

/* Filter contents and size */
typedef struct {
  GLfloat scale; /* 1/scale applied to image to prevent overflow */
  GLfloat bias; /* for biasing images */
  int rows;
  int cols;
  GLfloat *array;
} SUMA_C_FILTER;

/* *************** End Convolution utilities *************** */

typedef struct {
      /* variables copied from the event structs */
   int ktype;         /* Event type (Kev.type) */
   int kstate;        /* Event state (Kev.state) */
   char transl[16];  /* Translated event characters (buffer) */
   KeySym keysym;       /* key symbol (keysym) */
   int mtype;         /* Event type (Mev.type) */
   int mstate;        /* Event state (Mev.state) */
   int bButton;      /* (Bev.button) */
   int mButton;      /* (Mev.button) */
   Time bTime;       /* (Bev.time) */
   Time mTime;       /* (Mev.time) */
   int mX;
   int mY;
   int bX;
   int bY;
      /* inferred variables */
   int set;
   byte b1;
   byte b2;
   byte b3;
   byte b4;
   byte b5;
   byte b6;
   byte b7;
   byte b1m;
   byte b2m;
   byte b3m;
   byte b4m;
   byte b5m;
   byte Shift;
   byte Control;
   byte Mod1;
   byte Mod2;
   byte Mod3;
   byte Mod4;
   byte Mod5;
   byte AppleAltOpt;
   byte DoubleClick;
   Time mDelta;
   int mDeltaX;
   int mDeltaY;
   int bDeltaX;
   int bDeltaY;
} SUMA_EVENT; /* A record of last event sent to SUMA_input.
                 comments in () above refer to equiv. variables
                 in SUMA_input() function */

typedef struct {
   char *ado_idcode_str;   /* id of ado that was picked */
   char *primitive;        /* Type of object selected, for graphs, for 
                              example, it could be an edge, or a node 
                              Not all primitives carry data*/
   long int primitive_index; 
   long int datum_index;   /* If intersection is found for something 
                              that carries an elementary data
                              then this value is set.*/
   int ignore_same_datum;
   
   float PickXYZ[3]; /* Location of index */
   
   long int  iAltSel[SUMA_N_IALTSEL_TYPES];
   double    dAltSel[SUMA_N_DALTSEL_TYPES];
   
   char *dset_idcode_str; /* ID of volume where selection was made, 
                      if a volume was selected */
   SUMA_EVENT *evr; /* A record of the pick event at the time PR was created */
} SUMA_PICK_RESULT; /* Structure holding results of pointer selection*/


typedef struct {
   int N_DO;   /*!< Total number of surface objects registered with the viewer */
   SUMA_DO_LOCATOR *RegistDO;
   #if 0 /* Replaced with SUMA_DO_LOCATOR above*/
   int *RegisteredDO;    /*!< RegisteredDO[i] (i=0..N_DO) contains Object indices
                              into DOv for DOs visible in the surface viewer*/
   char **RegisteredDOid; /*!< A more reliable version of RegisteredDO. 
                              RegisteredDO starts to fail miserably when objects
                              are deleted from SUMAg_DOv thereby altering the
                              order of objects inside SUMAg_DOv.
                              The same problem needs to be dealt with in MembDO*/
   #endif
   
   
   SUMA_Boolean Record; /*!< Set record mode */
   SUMA_Boolean ShowLeft; /*!< Show left side surfaces */
   SUMA_Boolean ShowRight; /*!< Show right side surfaces */
   
   SUMA_COLORLIST_STRUCT **ColList; /*!< pointer to structures containing 
                        NodeColorLists for surfaces listed in RegisteredDO */
   int N_ColList; /*!< Number of structures in ColList */
   
   SUMA_STANDARD_VIEWS StdView; /*!< viewing mode, for 2D or 3D */
   SUMA_GEOMVIEW_STRUCT *GVS; /*! pointer to structures containing 
                                  geometric viewing settings */
   int N_GVS; /*!< Number of different geometric viewing structures */
   SUMA_GEOMVIEW_STRUCT *GVS_last_PickMode; 
               /*! A snapshot of GVS the last time display was done 
                   in DO_PickMode */
   short verbose;   /*!< Verbosity of viewer */

   SUMA_X *X; /*!< structure containing X widget midgets */
   
   DList *SelAdo; /* A list of DOs intersected by selection line */
   char *LastSel_ado_idcode_str; 
   
   int ShowSelectedDatum;
   int ShowSelectedFaceSet;
   int ShowSelectedEdge;
   int ortho; /*!< Orthographic (1) or perspective (0, default) projection */
   int ShowLabelAtXhair; /*!< Show label at location of cross hair */
   float Aspect;   /*!< Aspect ratio of the viewer*/
   int wWindWidth;   /*!< Width of whole window */
   int wWindHeight;   /*!< Height of whole window */
   int DrawAreaHeightOffset; /*!< Offset of GLX draw area height from enclosing
                                  window */
   int DrawAreaWidthOffset; /*!< Offset of GLX draw area width from enclosing
                                  window */
   float ZoomCompensate; /*!< Compensate mouse movements by zoom factor */
   float *FOV; /*!< Field of View (affects zoom level, there is a 
                    separate FOV for each ViewState)*/
   float *auto_FOV_val; /*!< Field of view computed on selectable content */
   float *FOV_last_PickMode; /*!< FOV last time we picked a DO */
   float FOV_original; /*!< Original field of view of viewer */
   float ArrowRotationAngle; /*!< Angle to rotate surface by when arrows 
                                 are used.
                                 Units are in radians */
   float KeyZoomGain; /*!< gain for zooming in and out with the 'z' and 'Z' keys.
                           Typical range from 0 to 0.5. Must be < 1*/
   float KeyNodeJump; /*!< Number of node jumps to do in response to 'alt+arrow'
                           clicks. Default is 1 */
   byte BF_Cull; /*!< flag for backface culling */
   SUMA_RENDER_MODES PolyMode; /*!< polygon viewing mode, SRM_Fill, 
            SRM_Line, SRM_Points
            There is a similar field for each surface object to 
            allow independent control for each surface. If the rendering mode
            is specified for a certain surface, it takes precedence over the
            one specified here*/
   SUMA_TRANS_MODES TransMode; 
   SUMA_DO_DRAW_MASK DO_DrawMask; /*!< What to draw of displayable objects */
   int MouseMode; /* Behavior when mouse gets busy */
   char *MouseMode_ado_idcode_str;
   
   float Back_Modfact; /*!< Factor to apply when modulating foreground 
               color with background intensity
               background does not modulate foreground, 
               Color = Fore * avg_Bright * AttenFactor; (w/ 0 <= avg_Bright <=1)
               a good setting is such that 
      SUMA_BACKGROUND_ATTENUATION_FACTOR * SUMA_DIM_AFNI_COLOR_FACTOR = 1
               Watch for saturation effects!  */

   float ContThick;  /*!< Thickness of contours drawn on datasets */
   int lit_for;   /*! 1 = lit for surfaces of normdir = 1, -1 for normdir = -1, 
                      0 for not set. */
   GLfloat light0_position[4]; /*!< Light 0 position: 
                                    1st 3 vals --> direction of light . 
                                    Last value is 0 -->  directional light*/
   GLfloat light1_position[4]; /*!< Light 1 position */
   GLfloat light0_color[4];   /*!< Light 0 color */
   GLfloat lmodel_ambient[4]; /*!< ambient light model */
   
   GLfloat dim_spe;  /*!< Color dimming factors for light properties */
   GLfloat dim_dif;
   GLfloat dim_emi;
   GLfloat dim_amb;
   
   GLfloat clear_color[4]; /*!< viewer background color */
      
   SUMA_Boolean Open; /*! Viewer visible to the human eye */
   int ShowEyeAxis ; /*!< ShowEyeAxis */
   int ShowMeshAxis; /*!< ShowMeshAxis (attached to each surface)*/
   int ShowWorldAxis; /*!< ShowWorldAxis */
   SUMA_Axis *WorldAxis;   /*!< pointer to world coordinate axis  */
   int ShowCrossHair; /*!< ShowCrossHair */
   SUMA_Boolean ShowForeground;    /*!< Flag for showing/not showing 
                                        foreground colors */
   SUMA_Boolean ShowBackground; /*!< Flag for showing/not showing 
                                     background colors */   
   SUMA_Boolean UsePatchDims; /*!< Flag for using patch based dimensions 
                                   (rather than entire nodelist) */
   
   #if 0 /* considering rendering this obsolete */
   int Focus_SO_ID; /*!< index into SUMAg_DOv of the surface currently in focus, 
                        -1 for nothing*/
   #endif
   int Focus_DO_ID; /*!< index into SUMAg_DOv of the Displayable Object 
                           currently in focus -1 for nothing*/
   
   int PickPix[2]; /*!< Location of click in pixel buffer */
   GLdouble Pick0[3];   /*!< Click location in World coordinates, at z = 0 
                           (near clip plane)*/
   GLdouble Pick1[3];   /*!< Click location in World coordinates, 
                              at z = 1.0 (far clip plane)*/
   GLdouble Pcenter_close[3];  /*!< Center of screen in World coordinates , 
                                    at z = 0 (near clip plane)*/
   GLdouble Pcenter_far[3];    /*!< Center of screen in World coordinates , 
                                    at z = 1 (near far plane)*/
   GLdouble Plist_close[3];   /*!< lists of points on screen in world coordinates
                  at z = 0.
                  it holds N/3 points where N is the array length
                  At the moment, all I need is one point, the lower left
                  Should more be needed, I will add them to the list and
                  document them here.*/
   SUMA_CrossHair *Ch; /*!< Pointer to Cross Hair structure */
   SUMA_Axis *WAx;  /*!< Pointer to world axis */   
   
   SUMA_ViewState *VSv; /*!< Vector of Viewing State Structures */
   int N_VSv; /*!< Number of Viewing State structures */
   char *State; /*!< The current state of the viewer. 
      This variable should no be freed since it points to locations within VSv*/
   int iState; /*!< index into VSv corresponding to State */
   int LastNonMapStateID; /*!< Index into the state in VSv from which a 
                              toggle to the mappable state was initiated */ 
   
   SUMA_Boolean isShaded; /*!< YUP if the window is minimized or shaded, 
                              NOPE if you can see its contents */

   SUMA_Boolean LinkAfniCrossHair; /*!< YUP if the cross hair location is to be 
                        sent (and accepted from AFNI, when the stream is open) */
   SUMA_Boolean ResetGLStateVariables; /*!< YUP if you need to run the function 
            that resets the Eye Axis before display. 
            see functions SUMA_display and SUMA_OpenGLStateReset for more info */
   SUMA_Boolean NewGeom; /*!< YUP if viewer has new geometry in it and 
                           needs to have its default viewing settings updated */
   DList *BS; /*!< The new version of BrushStroke, in doubly linked list form */
   
   char *CurGroupName; /*!< current name of group */
   int iCurGroup; /*!< index into GroupList (stored in SUMAg_CF) 
                        of current group of Surface Viewer */
   SUMA_REDISPLAY_CAUSE rdc;  /*!< Why has a redisplay been requested */
   SUMA_BLEND_MODES Blend_Mode; /*!< blending mode */
   int FreezeZoomXstates; /*!< if 1, keep zoom constant across states */
   
   int Do_3Drender;
   SUMA_EnablingRecord SER;
   
   int DO_PickMode; /*! Picking mode for selecting non-surface objects */
   DList *pick_colid_list;
   GLubyte *pickrenpix4; /*! An array holding the RGBA rendering buffer */
   
   int LoadedTextures[SUMA_MAX_DISPLAYABLE_OBJECTS+1];
   
   SUMA_C_FILTER *C_filter;
   SUMA_CONV_MODES C_mode;
   
   int otseq[N_DO_TYPES]; /* Object type sequence 
                             This is used to control the order in which 
                             different object types are rendered.
                             Default is: VO_type, SO_type, GRAPH_LINK_type*/
   int N_otseq;           /* Number of types specified in otseq, 
                             Default is 3 types*/
   int PryAx;             /* Prying axis. 3 == Z axis, 2 == Y axis */
} SUMA_SurfaceViewer;

/*! structure defining an EngineData structure */
typedef struct {
   SUMA_ENGINE_CODE CommandCode; /*!< Code of command to be executed by 
         SUMA_Engine function, 
         this is the same as the _Dest fields for each variable type.
         However, the _Dest fields are left as a way to make sure that
         the user has correctly initialized EngineData for a certain command.*/
   
   void *Srcp; /*!<  Pointer to data structure of the calling source, 
                     typically, a typecast version of SUMA_SurfaceViewer * */
   SUMA_ENGINE_SOURCE Src; /*!<  Source of command. This replaces the _Source 
                                 fields in the older version of the structure */
   
   float fv3[3]; /*!< Float vector, 3 values */
   SUMA_ENGINE_CODE fv3_Dest; /*!<  float3 vector destination */
   SUMA_ENGINE_SOURCE fv3_Source; /*!< OBSOLETE float3 vector source */
   
   int iv3[3];      /*!< Integer vector, 3 values */
   SUMA_ENGINE_CODE iv3_Dest;  /*!<  Integer3 vector destination */
   SUMA_ENGINE_SOURCE iv3_Source;  /*!<OBSOLETE  Integer3 vector source */
   
   float fv15[15]; /*!< Float vector, 15 values */
   SUMA_ENGINE_CODE fv15_Dest; /*!<  float15 vector destination */
   SUMA_ENGINE_SOURCE fv15_Source; /*!< OBSOLETE float15 vector source */
   
   float fv200[200]; /*!< Float vector, 200 values */
   SUMA_ENGINE_CODE fv200_Dest; /*!<  float200 vector destination */
   SUMA_ENGINE_SOURCE fv200_Source; /*!< OBSOLETE float15 vector source */
  
   int iv15[15];/*!< Integer vector, 15 values */
   SUMA_ENGINE_CODE iv15_Dest;/*!<  Integer15 vector destination */
   SUMA_ENGINE_SOURCE iv15_Source; /*!< OBSOLETE Integer15 vector source */

   int iv200[200];/*!< Integer vector, 200 values */
   SUMA_ENGINE_CODE iv200_Dest;/*!<  Integer200 vector destination */
   SUMA_ENGINE_SOURCE iv200_Source; /*!< OBSOLETE Integer200 vector source */
   
   int i;      /*!< integer */
   SUMA_ENGINE_CODE i_Dest;   /*!<  integer destination */
   SUMA_ENGINE_SOURCE i_Source; /*!< OBSOLETE integer source */
   
   float f; /*!< float, ingenious ain't it! */
   SUMA_ENGINE_CODE f_Dest; /*!<  float destination */
   SUMA_ENGINE_SOURCE f_Source; /*!< OBSOLETE float source */
   
   char s[SUMA_MAX_STRING_LENGTH]; /*!< string */
   SUMA_ENGINE_CODE s_Dest; /*!<  string destination */
   SUMA_ENGINE_SOURCE s_Source; /*!< OBSOLETE string source */
   
   int *ip; /*!< integer pointer */
   SUMA_ENGINE_CODE ip_Dest; /*!<  integer pointer destination */
   
   float *fp; /*!< float pointer */
   SUMA_ENGINE_CODE fp_Dest; /*!< float pointer destination */
   
   char *cp; /*!< char pointer */
   SUMA_ENGINE_CODE cp_Dest; /*!< character pointer destination */
   
   float **fm; /*!< float matrix pointer */
   SUMA_Boolean fm_LocalAlloc; /*!< Locally allocated matrix pointer ? 
                  (if it is then it is freed in SUMA_ReleaseEngineData ) */
   SUMA_ENGINE_CODE fm_Dest; /*!<  destination of fm */
   SUMA_ENGINE_SOURCE fm_Source; /*!< OBSOLETE source of fm*/
   
   int **im; /*!< Same dance as fm but for integers */
   SUMA_Boolean im_LocalAlloc;
   SUMA_ENGINE_CODE im_Dest; /*!<  destination of im */
   SUMA_ENGINE_SOURCE im_Source; /*!< OBSOLETE source of im */

   SUMA_IVEC *ivec; /*!< Same dance as fm but for integers */
   SUMA_Boolean ivec_LocalAlloc;
   SUMA_ENGINE_CODE ivec_Dest; /*!<  destination of im */
   SUMA_ENGINE_SOURCE ivec_Source; /*!< OBSOLETE source of im */

   SUMA_FVEC *fvec; /*!< Same dance as fm but for integers */
   SUMA_Boolean fvec_LocalAlloc;
   SUMA_ENGINE_CODE fvec_Dest; /*!<  destination of im */
   SUMA_ENGINE_SOURCE fvec_Source; /*!< OBSOLETE source of im */
   
   void *vp; /*!< pointer to void */
   SUMA_ENGINE_CODE vp_Dest; /*!<  destination of fm */
   SUMA_ENGINE_SOURCE vp_Source; /*!< OBSOLETE source of fm*/
   
   int N_rows; /*!< Number of rows in fm or im */
   int N_cols; /*!< Number of colums in fm or im */
   
   NI_group *ngr;
   SUMA_ENGINE_CODE ngr_Dest; /*!<  destination of ngr */
   SUMA_ENGINE_SOURCE ngr_Source; /*!< OBSOLETE source of ngr*/
   
   NI_element *nel;
   SUMA_ENGINE_CODE nel_Dest; /*!<  destination of nel */
   SUMA_ENGINE_SOURCE nel_Source; /*!< OBSOLETE source of nel*/
   
} SUMA_EngineData;



/*! structure that contains the output of SurfNorm function */
#define SUMA_SurfNorm_struct
typedef struct {
   int N_Node; /*!< Number of nodes, 1st dim of NodeNormList*/
   int N_Face;/*!< Number of facesets, 1st dim of FaceNormList*/
   float *FaceNormList ; /*!< N_Face x 3 vector (was matrix prior to SUMA 1.2) 
            containing normalized normal vectors for each triangular faceset*/ 
   float *NodeNormList ; /*!< N_Node x 3 vector (was matrix prior to SUMA 1.2) 
            containing normalized normal vectors for each node*/
} SUMA_SURF_NORM; /*!< structure that contains the output of SurfNorm function */

/*! structure that contains the output of SUMA_MemberFaceSets function */
#define SUMA_MemberFaceSets_struct
typedef struct {
   SUMA_DO_Types do_type;  /*!< To check if this is a displayable object */
   int LinkedPtrType; /*!< Indicates the type of linked pointer */
   int N_links;   /*!< Number of links to this pointer */
   char owner_id[SUMA_IDCODE_LENGTH];   /*!< The id of whoever created 
                                 that pointer.*/

   byte *nodemask; /*! Pointer to  nodes in the mask */
   int N_nz_nodemask; /*! number of non-zero values in mask */
   DList *DrwPtchs; /*! Pointer to a list of drawing patches */ 
   int PatchRegenID; /*! An integer that gets incremented anytime you want
                         the drawing patches to get regenerated. */
   int PatchGenID; /*! if != PatchRegenID then regenerate patches and set
                         to PatchRegenID */
   char *user_exp; /*!< User masking expression */
   char *cmask_exp; /*!< User expression, preppred for cmask evaluation */
   char *last_cmask_exp; /*!< Last expression evaluated to form nodemask */
} SUMA_DRAW_MASKS; /*!< Drawing masks formed on the fly */

typedef struct {
   SUMA_DO_Types do_type;  /*!< To check if this is a displayable object */
   int LinkedPtrType; /*!< Indicates the type of linked pointer */
   int N_links;   /*!< Number of links to this pointer */
   char owner_id[SUMA_IDCODE_LENGTH];   /*!< The id of whoever created 
                                 that pointer. Might never get used.... */

   char *idcode_str;
   int N_Memb_max;/*!< Maximum number of Facesets any node belonged to*/
   int Nnode; /*! Total number of nodes examined (0..Nnode-1) */
   int **NodeMemberOfFaceSet ; /*!< Nnode x N_Memb_max matrix containing 
                                    for each row i, the indices of the 
                                    facesets containing node i*/ 
   int *N_Memb ; /*!< Nnode x 1 vetor containing for each node i, 
                      the number of facesets that contain it*/
} SUMA_MEMBER_FACE_SETS; /*!< structure that contains the output of 
                              SurfNorm function */


/*! structure containing results of intersection of a ray with triangles */
typedef struct {
   int N_el; /*!< Number of elements in each vector */
   SUMA_Boolean *isHit;   /*!< Is the triangle hit ? */
   float *t;   /*!< SIGNED Distance from ray source to triangle */
   float *u;   /*!< location of intersection in triangle in 
                   Barycentric coordinates, V0P = u V0V1 + vV0V2*/
   float *v;   /*!< location of intersection in triangle */
   int ifacemin; /*!< index of the faceset closest (NOT SIGNED, abs(t)) 
                      to the ray's origin */
   int ifacemax; /*!< index of the faceset farthest (NOT SIGNED, abs(t)) 
                      from the ray's origin */
   int N_hits; /*!< Number of intersections between ray and surface */
   int N_poshits; /*!< Number of intersections such that t is positive */
   float P[3]; /*!< XYZ of intersection with ifacemin */
   float d; /*!< Distance from the closest node in ifacemin to P */
   int inodemin; /*!< node index (into NodeList)that is closest to P  */
   int inodeminlocal; /*!< node in FaceSet[ifacemin] that is closest to P , 
                  inodemin = FaceSet[ifacemin][inodeminlocal]*/
} SUMA_MT_INTERSECT_TRIANGLE;

typedef enum { BAD_WARP=-1, NO_WARP, ROTATE_WARP, VOLREG_WARP, 
               TAGALIGN_WARP,  WARPDRIVE_WARP, ALLINEATE_WARP, 
               N_WARP_TYPES } SUMA_WARP_TYPES;

/*! Structure defining the surface's volume parent info */
typedef struct {
   char *idcode_str; /*!< the id of the vol par element */
   int isanat; /*!< 1 if parent volume is of type anat */
   int nx, ny, nz; /*!< number of voxels in the three dimensions */
   float dx, dy, dz; /*!< delta x, y, z in mm */
   float xorg, yorg, zorg; /*!< voxel origin in three dimensions */
   char *prefix; /*!< parent volume prefix */
   char *headname;
   char *filecode; /*!< parent volume prefix + view */
   char *dirname; /*!< parent volume directory name */
   char *vol_idcode_str; /*!< idcode string OF parent volume*/
   char *vol_idcode_date; /*!< idcode date */
   int xxorient, yyorient, zzorient; /*!< orientation of three dimensions*/ 
   double *CENTER_OLD; /*!< pointer to the named attribute (3x1) in the 
                           .HEAD file of the experiment-aligned Parent Volume */
   double *CENTER_BASE; /*!< pointer to the named attribute (3x1) in the 
                           .HEAD file of the experiment-aligned Parent Volume */
   double *MATVEC; /*!< pointer to the warp attribute (12x1) in the .HEAD file 
                        of the experiment-aligned Parent Volume */
   SUMA_WARP_TYPES MATVEC_source;
#if 0
   float *VOLREG_MATVEC; /*!< pointer to the named attribute (12x1) in the 
                           .HEAD file of the experiment-aligned Parent Volume */
   float *TAGALIGN_MATVEC; /*!< pointer to the named attribute (12x1) in the 
                           .HEAD file of the tag aligned Parent Volume */
   float *WARPDRIVE_MATVEC; /*!< pointer to the named attribute (12x1) in the 
                           .HEAD file of the warpdrive aligned Parent Volume */
   float *ALLINEATE_MATVEC; /*!< pointer to the named attribute (12x1) in the 
                           .HEAD file of the warpdrive aligned Parent Volume */
   float *ROTATE_MATVEC; /*!< pointer to the named attribute (12x1) in the 
                           .HEAD file of the rotate aligned Parent Volume */
#endif
   int Hand; /*!< Handedness of axis 1 RH, -1 LH*/
} SUMA_VOLPAR;

typedef struct {
   SUMA_OVERLAYS *Overlay; /*!< pointer to color overlay structures */
} SUMA_OVERLAY_LIST_DATUM;   /*!< a structure used to create linked lists 
                                  of SO->Overlays and co */ 

typedef enum {  ADD_BEFORE=-1, ADD_NOT=0, ADD_AFTER=1 } SUMA_VISX_ADD_POSITIONS;
typedef enum {  UNDO_XFORM=0, FORWARD_XFORM=1 } SUMA_VISX_XFORM_DIRECTIONS;
typedef enum {  ID=0, SHIFT=1, AFFINE=2, DISP=3 } SUMA_VISX_XFORM_TYPE;

typedef struct {
   SUMA_VISX_XFORM_TYPE XformType; 
                     /*!< Is it identity (0), shift (1), or affine (2), 
                        (3) displacement field*/
   double Xform[4][4]; /*!< The augmented affine transform */
   float *dxyz;   /* displacement field, only set with XformType == 4 */
   char label[64]; /* some string identifier */
} SUMA_VIS_XFORM_DATUM;

/*! Structure for defining spatial transformations for visualization purposes */
typedef struct {
   DList *Xchain;
   int Applied; /*!< Xform has been applied to coords */
   float *XformedCoords; /*!< Tranformed coords. If NULL and XformApplied, 
                              it means the results of the transform have
                              been stored in the surface's NodeList */   
} SUMA_VIS_XFORM;

/*! structure defining a Surface Object */
typedef struct {
      /* FIRST VARIABLES MUST RETAIN THEIR ORDER and follow SUMA_ALL_DO */
   SUMA_DO_Types do_type; 
   char *idcode_str; /*!< string containing the idcode of the surface */
   char *Label; /*!< string containing a label for the surface. 
                     Used for window titles and saved image names */
               
   
   /* Begin by fields that are generic    
                  and need to be accessed from AFNI 
                  Keep in sync with AFNI_SurfaceObject STRUCT 
                  AND function SUMA_MergeAfniSO_In_SumaSO
   **********  BEGIN KEEP THIS SECTION TIGHT 
               WITH AFNI_SURFACEOBJECT STRUCT ***********
   */
   int N_Node; /*!< Number of nodes in the SO */
   int NodeDim; /*!< Dimension of Node coordinates 3 for 3D only 3 
                     is used for now, with flat surfaces having z = 0*/
   int EmbedDim; /*!<   Embedding dimension of the surface, 
                        2 for flat surfaces 3 for ones with non zero 
                        curvature. */ 
   float *NodeList; /*!< N_Node x 3 vector containing the XYZ node coordinates. 
                        If NodeDim is 2 then the third column is all zeros
                        Prior to SUMA  1.2 this used to be a 2D matrix 
                        (a vector of vectors) */
   
   int N_FaceSet; /*!< Number of polygons defining the surface  */
   int FaceSetDim; /*!< Number of sides on the polygon */
   int *FaceSetList; /*!<  N_FaceSetList x FaceSetDim vector describing 
                           the polygon set that makes up the SO.
                           Each row contains the indices (into NodeList) of 
                           the nodes that make up a polygon 
                           Prior to SUMA  1.2 this used to be a 2D matrix 
                           (a vector of vectors) */
   /*
   **********  END KEEP THIS SECTION TIGHT 
               WITH AFNI_SurfaceObject STRUCT 
               in suma_datasets.h     ***********
   */
   
   /* Include also AFNI_SurfaceObject which would contain GIFTI's information
   with pointers stolen for list above set to NULL 
      (see SUMA_MergeAfniSO_In_SumaSO)  */
   NI_group *aSO;
   
   SUMA_SO_File_Type FileType; /*!< Type of Surface file */
   SUMA_SO_File_Format FileFormat; /*!< Format of Surface file ascii or binary*/
   SUMA_FileName Name; /*!< Directory and Name of surface object file (SO) */
   SUMA_FileName Name_coord; /*!< Directory and Name of surface coordinate 
                                 file (for SureFit files) */
   SUMA_FileName Name_topo; /*!< Directory and Name of surface topology file
                                (for SureFit files)*/
   SUMA_FileName SpecFile; /*!< To be added for use in AFNI's 
                                 mapping interface*/
   
   char *parent_vol_idcode_str; /*!< IDcode of the volume from which the surface
                                     was created. 
                                     Called SurfVol (NOT SurfVol_AlndExp) 
                                    That ID does not usually refer to the volume
                                    from which VolPar is created. 
                                    Except in the case 
                                    where you are viewing the surfaces on 
                                    the orignal volume (SurfVol) then this 
                                    field and
                                    SurfVol (afni dset *) ->idcode.str and
                                    VolPar->vol_idcode_str should be identical*/
   char *facesetlist_idcode_str;   /*!< ID of facesets element */
   char *nodelist_idcode_str; /*!< ID of nodelist element */
   char *facenormals_idcode_str; /*!< ID of facenormals element */
   char *nodenormals_idcode_str; /*!< ID of nodenormals element */
   char *polyarea_idcode_str; /*!< ID of polygon areas element */
   char *Name_NodeParent; /*!<   Node parent of the SO.   
                                 Node Indices of SO are into NodeList 
                                 matrix of the NodeParent SO*/               
   char *Group_idcode_str;  /*!< IDcode of group */
   char *Group;   /*!<  Group the surface belongs to, 
                        like Simpsons H. (aka. SubjectLabel)*/
   char *State; /*!< State of SO (like inflated, bloated, exploded) */
   
   SUMA_SO_SIDE Side; /*!< Left/right */
   SUMA_GEOM_TYPE isSphere;  /*!< yes/no */
   float SphereRadius; /*!< If a sphere, then this is its radius */
   float SphereCenter[3]; /*!< If a sphere, then this it is center */
   SUMA_Boolean AnatCorrect;    /*!<   Does surface geometry match 
                                       anatomy ? (YUP/NOPE)*/
   char *DomainGrandParentID;        /*!< Grandparent's mesh ID 
                                          (icosahedron's for std-meshes) */
   char *OriginatorID;          /*!<   ID common for surfaces from one subject
                                       that are created
                                       at one point in time. Surfaces of the  
                                       same subject,
                                      created at different points in time 
                                      (like in a longitudinal
                                      study) will have differing 
                                      OriginatorID fields (aka InstanceID)*/
   char *OriginatorLabel;        /*!< aka InstanceLabel */
   char *LocalCurvatureParent;   /*!< \sa same field in SUMA_SurfSpecFile 
                                       structure */
   char *LocalCurvatureParentID;     /*!< \sa   idcode_str of 
                                                LocalCurvatureParent*/
   char *LocalDomainParent;   /*!< \sa same field in SUMA_SurfSpecFile 
                                       structure */
   char *LocalDomainParentID;      /*!< \sa idcode_str of LocalDomainParent */

   SUMA_Boolean SUMA_VolPar_Aligned; /*!< Surface aligned to Parent Volume 
                                          data sets ?*/
   SUMA_WARP_TYPES APPLIED_A2Exp_XFORM; /*! Type of warp applied
                                            to bring SurfVol into alignment
                                            with ExpVol 
                                          Replaces VOLREG_APPLIED,
                                                   TAGALIGN_APPLIED,
                                                   ROTATE_APPLIED,
                                                   etc*/
   void *Saux; /* A pointer to a structure for SUMA's use 
                  For now this is mostly for accessing the
                  pick result structure. 
                  In the future, all selections currently
                  under SO-> should move to SO->Saux->PR
                  sane for overlays, etc. This would make
                  SOs behave more like the other displayable
                  objects. All in good time */
   void (*FreeSaux)(void *Saux); /* Function to free Saux */

   SUMA_Boolean SentToAfni; /*!< YUP if the surface has been 
                                 niml-sent to AFNI */
   SUMA_Boolean Show; /*!< YUP then the surface is visible in the viewer. 
                           Used in conjunction with PolyMode*/
   SUMA_RENDER_MODES PolyMode; /*!< polygon viewing mode, SRM_Fill, 
                                    SRM_Line, SRM_Points */
   SUMA_TRANS_MODES TransMode; /*!< transprency */
      
   float *NodeNormList ; /*!< N_Node x 3 vector (used to be matrix prior 
                              to SUMA 1.2) containing normalized normal 
                              vectors for each node*/
   float *FaceNormList ; /*!< N_FaceSet x 3 vector (used to be matrix prior 
                              to SUMA 1.2) containing normalized normal vectors 
                              for each polygon*/ 
   int normdir;        ; /*!< direction of normals, 0 not known. 
                              1 outwards, -1 inwards */
   
   float Center[3];       /*!< The centroid of the surface 
                              (using all the nodes in NodeList)*/
   float MaxDims[3];      /*!< The maximum along each of the XYZ dimensions */
   float MinDims[3];      /*!< The minimum along each of the XYZ dimensions */
   float aMinDims;      /*!< The maximum across all dimensions*/
   float aMaxDims;      /*!< The minimum across all dimensions*/
   
   float MaxCentDist;   /* Maximal node distance from center */
   int MaxCentDistNode; /* Node which has maximal node distance from center */
   float MinCentDist;   /* Minimal node distance from center */
   int MinCentDistNode; /* Node which has minimal node distance from center */
   
   
   int N_patchNode; /*!<   Number of nodes used in the mesh. 
                           For patches, this number is < SO->N_Node */
   byte *patchNodeMask; /*!< if not NULL, then if patchNodeMask[i] then
                              that node is part of the patch. Else it is not.
                              i goes from 0 to SO->N_Node */
   float patchCenter[3];  /*!< The centroid of the surface 
                              (using all the nodes in FaceSetList)*/
   float patchMaxDims[3];      /*!< The maximum along each of the XYZ dimensions
                                    (using all the nodes in FaceSetList)*/
   float patchMinDims[3];      /*!< The minimum along each of the XYZ dimensions
                                    (using all the nodes in FaceSetList)*/
   float patchaMinDims;      /*!<   The maximum across all dimensions(using all 
                                    the nodes in FaceSetList)*/
   float patchaMaxDims;      /*!<   The minimum across all dimensions(using all 
                                    the nodes in FaceSetList)*/
   
   int RotationWeight; /*!<   Contribution to center of rotation calculation. 
                              set to 0 if not contributing.
                              set to N_Node to have the number of nodes weigh 
                              into the center's location, center of mass effect
                              set to 1 to give each object equal weight */
   int ViewCenterWeight; /*!< Contribution to center of gaze and 
                              viewfrom location */
   

   GLfloat *glar_NodeList;         /*!< pointer to the 1D NodeList array 
                                    - DO NOT FREE IT, 
                                    it is a pointer copy of NodeList*/
   GLint  *glar_FaceSetList;      /*!< pointer to the 1D FaceSetList array 
                                    - DO NOT FREE IT, 
                                    it is a pointer copy of FaceSetList*/
   GLfloat *glar_FaceNormList;    /*!< pointer to the 1D FaceNormList array 
                                    - DO NOT FREE IT, 
                                    it is a pointer copy of NodeNormList*/
   GLfloat *PermCol; /*!< pointer to a 1D ColorList array. 
                         If this vector is not null then it specifies the colors
                         of the nodes on the surface. 
                         It is illegal to have this if Overlays != NULL */ 
   GLfloat *glar_NodeNormList;    /*!< pointer to the 1D NodeNormList array 
                                       - DO NOT FREE IT, 
                                       it is a pointer copy of NodeNormList*/
   
   int ShowMeshAxis; /*!< flag to show Mesh Axis if it is created */
   SUMA_Axis *MeshAxis;   /*!< pointer to XYZ axis  */
   
   SUMA_MEMBER_FACE_SETS *MF; /*!<  structure containing the facesets 
                                    containing each node */
   SUMA_NODE_FIRST_NEIGHB *FN; /*!< structure containing the first order
                                    neighbors of each node */
   SUMA_EDGE_LIST *EL; /*!< structure containing the edge list */
   float *PolyArea; /*!< N_FaceSet x 1 vector containing the area of 
                         each polygon in FaceSetList */
   SUMA_SURFACE_CURVATURE *SC; /*!< Structure containing the surface 
                                    curvature info */
   
   
   /* selection stuff */
   int SelectedNode; /*!< index of one selected node, 
                           -1 if no node is selected */
   SUMA_SphereMarker *NodeMarker; /*!< Node Marker object structure*/
   
   int SelectedFaceSet; /*!< index of one selected faceset, 
                              -1 if no faceset is selected */
   SUMA_FaceSetMarker *FaceSetMarker; /*!< Aha, I hear ya */
   
   SUMA_VOLPAR *VolPar; /*!< Parent Volume structure */
   
   SUMA_OVERLAYS **Overlays; /*!< vector of pointers to color overlay 
                                  structures */
   int N_Overlays; /*!< number of pointers to overlay structures */
   
   SUMA_X_SurfCont *SurfCont;/*!< pointer to structure containing surface  
                                  controller widget structure */
   
   SUMA_DO *CommonNodeObject; /*!< a node marker which can be any of the 
                            node-based displayable objects. 
                            The DO is for one node only, 
                            something like a sphere say.
                            At load time, the marker is replicated
                            to form a DO that covers all the nodes. */
   SUMA_DO *NodeObjects;   /*!< a replication of CommonNodeObject where
                           each node gets a shape defined by CommonNodeObject */
   SUMA_NIDO **NodeNIDOObjects;   /*!< a more flexible replication of NIDO 
                           CommonNodeObject where */
                           
   float *NodeAreas; /*!< A way to keep node areas around */
   
   SUMA_VIS_XFORM VisX0; /*!< a coordinate transform applied at the moment
                              the surface is loaded and the resultant 
                              coordinates are to be stored in the the
                              surface's NodeList */
   SUMA_VIS_XFORM VisX; /*!< a coordinate transform generated from interactive
                              vicissitudes */
   
   float *NodeList_swp; /*!< A temporary pointer copy for swapping NodeList with
                             VisX's transformed coordinates. Use sparingly */
   float PointSize; /*! Set to negative to make it so it has no effect */
   
   SUMA_DRAW_MASKS *DW; /*!< A structure containing node masking information
                             for on the fly masking */
} SUMA_SurfaceObject; /*!< \sa Alloc_SurfObject_Struct in SUMA_DOmanip.c
                     \sa SUMA_Free_Surface_Object in SUMA_Load_Surface_Object.c
                     \sa SUMA_Print_Surface_Object in SUMA_Load_Surface_Object.c
                     \sa SUMA_Load_Surface_Object in SUMA_Load_Surface_Object.c
               */  

typedef struct {
   SUMA_Boolean FreeFaceSetList; /*! Set to YUP if FaceSetList is not a 
                                     pointer copy */
   int *FaceSetList;            /*! A subset of the parent surface's triangles 
                                   to be drawn */
   int N_FaceSet;
   
   
   /* See SUMA_SurfaceObject for interpretation of the following parameters */
   SUMA_Boolean Show; 
   SUMA_RENDER_MODES PolyMode; 
   SUMA_TRANS_MODES TransMode; 
}  SUMA_DrawPatch; /* Surfaces can be drawn in patches for nefarious reasons. */

typedef struct {
      /* FIRST VARIABLES MUST RETAIN THEIR ORDER and follow SUMA_ALL_DO */
   SUMA_DO_Types do_type; 
   char *idcode_str;
   char *Label;
   
      /* Not sure whether or not these things need to be moved into 
         the common stuff in SUMA_ALL_DO yet ... */
   void *Saux; /* A pointer to a structure for SUMA's use */
   void (*FreeSaux)(void *Saux); /* Function to free Saux */
   int N_datum; /* Here number of control points forming tracts */
   
      /* Begin specific fields */
   char *Parent_idcode_str;   /* Object a mask is attached to (positioned on)*/
   int Parent_datum_index;    /* Datum index a mask is attached to */
   
   char mtype[64]; /* A string defining mask subtypes 
                      cube --> box
                      ball --> sphere
                      surface --> surface of generic shape 
                      volume --> volume of generic shape */
   int N_obj;
   float *cen; /* Center of mask */
   float *hdim; /* XYZ half dims of mask */
   
   float *init_cen; /* Initial centers */
   float *init_hdim; /* Initial dims */
   
   SUMA_SurfaceObject *SO; /* Surface defining mask shape */ 
   
   float *init_col;
   float dim;
   
   GLfloat *dcolv;
   
   char varname[3];
   SUMA_TRANS_MODES trans;
   
   float dopxyz[3];
   int dodop;
} SUMA_MaskDO;


typedef struct {
   /* *** DO NOT ADD ANYTHING BEFORE THESE FIELDS
          DO NOT CHANGE THE ORDER OF THESE FIELDS
          ANY CHANGES HERE SHOULD BE REFLECTED IN 
          SUMA_LinkedPtr structure 
   */
   SUMA_DO_Types do_type;  /*!< To check if this is a displayable object */
   int LinkedPtrType; /*!< Indicates the type of linked pointer */
   int N_links;   /*!< Number of links to this pointer */
   char owner_id[SUMA_IDCODE_LENGTH];   /*!< The id of whoever created 
                                 that pointer. Might never get used.... */
   SUMA_GRAPH *gr;
} SUMA_GRAPH_DO;     /* A graph displayable object */


#define SUMA_MAX_N_VE 5
typedef struct {
   char *dset_idcode_str;
   
   GLubyte *texvec;  /* vector of voxel textures */
   GLuint *texName;
   
   int Ni;
   int Nj;
   int Nk;
   
   float I2X[4][4];  /* ijk to dicom real */
   float X2I[4][4];  /* dicom real to ijk */
   float X2T[4][4];  /* dicom real to texture */
   float T2X[4][4];  /* texture to dicom real */
   float bcorners[24];/* box corners (outer edges) */
   float vcorners[24];/* vox corners (voxel centers) */
   
   float vo0[3];  /* coordinates of 1st voxel */
   float voN[3];  /* coordinates of last voxel */
   float bo0[3];  /* coordinates of 1st box corner */
   float boN[3];  /* coordinates of last box corner */
} SUMA_VolumeElement;

typedef struct {
      /* FIRST VARIABLES MUST RETAIN THEIR ORDER and follow SUMA_ALL_DO */
   SUMA_DO_Types do_type; 
   char *idcode_str;    
   char *Label; 
   
      /* Begin specific fields */
   SUMA_VolumeElement **VE;
   GLfloat TexEnvMode;
   
   void *Saux; /* A pointer to a structure for SUMA's use */
   void (*FreeSaux)(void *Saux); /* Function to free Saux */

   GLdouble CutPlane[6][4];
   byte UseCutPlane[6];
   int SelectedCutPlane;
   SUMA_SurfaceObject **SOcut;
   
   void *VoxelMarker; /* a TBD structure to hold object used to 
                        highlight a selected voxel */
   int SelectedVoxel;
   byte ShowSelectedVoxel;
   
   byte Show;
   
} SUMA_VolumeObject; /*! \sa SUMA_CreateVolumeObject */

typedef struct {
      /* FIRST VARIABLES MUST RETAIN THEIR ORDER and follow SUMA_ALL_DO */
   SUMA_DO_Types do_type; 
   char *idcode_str;    
   char *Label; 
   
      /* Begin specific fields */
   int N_subdoms; /* Number of sub objects defining domain */
   char **subdoms_id; /* Collection of sub-domains */ 
   
   void *Saux; /* A pointer to a structure for SUMA's use */
   void (*FreeSaux)(void *Saux); /* Function to free Saux */

   int SelectedDatum;
   int SelectedSubAdo;
   
   byte Show;
   
} SUMA_CIFTI_DO; /*! \sa SUMA_CreateCIFTIObject */

                   
/*! Structure for creating a hash of color ids */
typedef struct {
    int id;    /* keep it named 'id' to facilitate use of convenience
                  macros in uthash . The integer id of a color*/
    int colmapindex; /* the index into the colormap of id */
    UT_hash_handle hh;  /* keep it named 'hh' for same reasons  */
}  SUMA_COLOR_MAP_HASH_DATUM;

/* Structure keeping track of what was selected in a surface viewer*/
typedef struct {
   char *ado_idcode_str;
   char *variant;
   SUMA_PICK_RESULT *PR;
} SUMA_SEL_ADO_DATUM;

/*! Structure containing a color map */
typedef struct {
   float ** M; /*!< N_Col x 3 matrix of R G B values (0..1) */
   char **cname; /*!< N_Col pointers to strings containing name of 
                       each color. This can be NULL when no names are 
                       assigned*/
   int N_M[2]; /*!< number of colors in the color map x number of columns in M
                     Used to be N_Col, now N_Col = N_M[0] and the
                     number of columns in M is N_M[1] */
   float *frac; /*!< N_col x 1 vector containing the fraction of scale 
                     assigned to each color, these are
                     the values shown on the right of the colorbar in 
                     AFNI.
                     This field is NULL if the map is linear*/
   float top_frac; /* Not all afni maps end with 1 as the top fraction.
                      For equivalent mapping in AFNI, the range needs 
                      to be multiplied by this factor, set to 0 if not 
                      in use */ 
   int Sgn; /*!         +1  colormap is positive ranging (a la afni)  
                         0  field is not set
                         -1 colormap is negative ranging (a la afni)*/
   char *Name; /*!< Name of colormap */
   
   SUMA_SurfaceObject *SO;    /*!< Surface object used to represent map */
   
   float M0[4];   /*!< The very first color at Map creation, needed to reset
                      when rotations are performed*/
   byte flipped;  /*!< if the colormap is flipped */
   
   /* Hashing needs */
   int *idvec; /*!< N_Col vector containing the id of each of the colors in M 
                     This vector is redundant if chd is used, but it is kept */
   SUMA_COLOR_MAP_HASH_DATUM *chd;  /* This is a pointer to the hash table, and 
                                       can always change during hash table 
                                       operations*/
} SUMA_COLOR_MAP;

/*! structure containing a mapping of one surface to another*/
typedef struct {
   float *NewNodeList; /*!< N_Node x 3 vector containing new mapping 
                           of node coordinates */
   int N_Node; /*!< Number of nodes in NodeList */
   float *NodeVal; 
   float *NodeCol;
   float *NodeDisp;
} SUMA_SO_map;

/*! structure containing SureFit Surface*/
typedef struct {
   /* coord files */
   char name_coord[SUMA_MAX_NAME_LENGTH];
   int N_Node; /*!< Number of nodes */
   float *NodeList; /*!< N_Node x 3 vector containing node coordinates */
   int *NodeId; /*!< Node ID, that's normaly from 0..N_Nodes-1 but 
                    since it's in .coord file, I keep it anyway */
   byte *allzerocoord;
   char encoding_coord[100];
   char configuration_id[100];
   char coordframe_id[100];
   /* Topo files */
   char name_topo[SUMA_MAX_NAME_LENGTH];
   char encoding_topo[100];
   char date[100];
   char perimeter_id[100];
   int N_Node_Specs; /*!< Number of nodes with listed node specs */
   int **Specs_mat; /*!< Node Specs matrix. Columns appear to be 
                         arraged as such NodeId #Neighbors ? ? NodeId ? */
   SUMA_NODE_FIRST_NEIGHB FN; /*!< First order neighbor structure */
   int N_FaceSet; /*!< Number of polygons making up surface */
   int *FaceSetList; /*!< definition of polygons. Became a vector in SUMA 1.2*/
   /* Param Files */
   char name_param[SUMA_MAX_NAME_LENGTH];
   float AC_WholeVolume[3]; /*!< XYZ (from .Orient.params file) 
                               of Anterior Comissure of whole volume */
   float AC[3]; /*!< XYZ of Anterior Comissure of cropped volume */
   float CropMin[3];
   float CropMax[3];
   float tag_version;
   float caret_version;
} SUMA_SureFit_struct;

/* structure containing FreeSurfer Surface */
typedef struct {
   char name[SUMA_MAX_NAME_LENGTH];
   int N_Node; /*!< Number of nodes */
   int *NodeId; /*!< Node ID, that's normaly from 0..N_Nodes-1 unless the surface is a patch of another surface see FaceSetIndexInParent*/
   float *NodeList; /*!< N_Node x 3 vector containing node coordinates */
   int N_FaceSet; /*!< Number of polygons making up surface */
   int *FaceSetList; /*!< definition of polygons. For a complete surface, these are indices into NodeList's rows
                           For a patch, these are indices into NodeList of the parent surface.
                        Became a vector in SUMA 1.2*/
   char comment[SUMA_MAX_STRING_LENGTH]; /*!< comment at beginning of patch or surface */
   SUMA_Boolean isPatch; /*!< is the surface a patch of another ? */
   int *FaceSetIndexInParent; /*!< for a FaceSet in patch, this represents its index in FaceSetList of the parent surface.
                                    This is the FaceSet equivalent of NodeId*/ 
} SUMA_FreeSurfer_struct;

/* structure containing SureFit name*/
typedef struct {
   char name_coord[SUMA_MAX_FILENAME_LENGTH];
   char name_topo[SUMA_MAX_FILENAME_LENGTH]; 
   char name_param[SUMA_MAX_FILENAME_LENGTH];
} SUMA_SFname;

typedef enum {    SMT_Nothing, 
                  SMT_Notice, SMT_Warning, SMT_Error, SMT_Critical, SMT_Text, 
                  SMT_N }  SUMA_MESSAGE_TYPES; /*!< different types of messages */

typedef enum {    SMA_Nothing, 
                  SMA_Log, SMA_LogAndPopup,  
                  SMA_N }  SUMA_MESSAGE_ACTION; /*!< different actions to perform with messages */

 
                                 
/*! structure containing a SUMA Message structure */
typedef struct {
   SUMA_MESSAGE_TYPES Type;   /*!< type of message */
   SUMA_MESSAGE_ACTION  Action; /*!< what to do with message*/
   char *Message; /*!< null terminated message string */
   char *Source;  /*!< source of message, usually calling function */
}  SUMA_MessageData;


              
/*! structure containing a surface patch */
typedef struct {
   int N_FaceSet;    /*!< Number of Facesets forming patch */
   int *FaceSetList; /*!< vector (was a matrix prior to SUMA 1.2) (N_FaceSet x 3)                           containing indices of nodes forming triangles making up                           the patch */
   int *FaceSetIndex; /*!< vector (N_FaceSet x 1) containing indices of triangles                            in FaceSetList in the FaceSetList of the surface that 
                           the patch was taken from */
   int *nHits; /*!< (N_FaceSet x 1) If patch is created from a set of nodes,
                     nHits[i] is the number of nodes refering to this Faceset */
} SUMA_PATCH; /*!< A surface patch, typically created from a set of nodes */

/*! structure containing ClientData 
This remains to be used somewhere ... */
typedef struct {
   SUMA_SurfaceViewer *sv; /*!< pointer to surface viewer from which the 
                                 callback was made */
   int svi; /*!< index of sv into SUMAg_SVv */
}SUMA_CLIENT_DATA;

/*! Maximum nuber of branches that can be found in the intersection 
   of a plane with the surface model */
#define SUMA_BRANCHMAX 500   

/*! Maximum nuber of nodes that can form one branch */
#define SUMA_NODEINLISTMAX 500

/*!
\brief Structure forming a branch 

A branch is a protruded part of a tree often resulting in chainsaw accidents.
It is used in determining the intersection of a plane with a surface
*/
typedef struct {
   int begin, start;   /*!< first node of the branch */
   int last;   /*!< last node of the branch */
   int closed; /*!< flag. 0--> open, 1--> closed */
   int list[SUMA_NODEINLISTMAX]; /*!< list of nodes per branch */
   int listsz; /*!< Number of nodes in list*/
} SUMA_BRANCH;

/*!
\brief Structure forming a triangle branch 

A Tiangle branch represents a strip of connected triangles.

*/
typedef struct {
   int begin, start;   /*!< first node of the branch */
   int last;   /*!< last node of the branch */
   int iBranch; /*!< index identifying branch */
   SUMA_Boolean closed; /*!< flag. 0--> open, 1--> closed */
   int * list; /*!< list of nodes per branch */
   int N_list; /*!< Number of nodes in list*/
} SUMA_TRI_BRANCH;

/*!
\brief Structure defining the intersection of a surface with a plane 
*/
typedef struct {
   int N_IntersEdges; /*!< Number of edges intersected by the plane */
   int *IntersEdges;  /*!< Vector containing indices of edges intersected by the plane. The indices
                        are into SO->EL->EL matrix. The space allocate for this vector is SO->EL->N_EL
                        But that is way more than ususally needed. For this vector and others in
                        this structure, reallocation is not done to save time. Useful values for IntersEdges
                        are between IntersEdges[0]  and IntersEdges[N_IntersEdges-1]*/
   SUMA_Boolean *isEdgeInters; /*!< Vector specifying if an edge i (index into SO->EL->EL) was intersected. */
   #if 0
   /* old way, less memory usage, slower access - pre Wed Dec  4 16:57:03 EST 2002*/
   float *IntersNodes;  /*!< Vector containing XYZ coordinates of the intersection point on each 
                           intersected segment. Allocated space is for 3*SO->EL->N_EL, useful space is 
                           between IntersNodes[0] and IntersNodes[3*(N_IntersEdges-1) + 2]. Intersection point 
                           of edge IntersEdges[k] has  X = IntersNodes[3*k], Y = IntersNodes[3*k+1] and
                           Z = IntersNodes[3*k+2] */
   #endif
   float *IntersNodes;  /*!< Vector containing XYZ coordinates of the intersection point on each 
                           intersected segment. Allocated space is for 3*SO->EL->N_EL, meaningful values are
                           for intersected segments only. Intersection point 
                           of edge SO->EL->EL[k][:] has  X = IntersNodes[3*k], Y = IntersNodes[3*k+1] and
                           Z = IntersNodes[3*k+2] */
   int *IntersTri; /*!< Vector containing indices of triangles intersected by the plane (i.e. containing
                        and edge that was intersected. Allocation is done for SO->N_FaceSet. 
                        But meaningful values are between IntersETri[0] and IntersTri[N_IntersTri-1]. */
   int N_IntersTri; /*!< Number of intersected triangles. */
   SUMA_Boolean *isNodeInMesh;   /*!< Vector of SO->N_Node elements indicating whether a certain node
                                    belongs to an intersected seqment or not */
   int N_NodesInMesh;  /*!< Total number of nodes belonging to intersected segments */
   
   SUMA_Boolean *isTriHit; /*!< Vector of SO->N_FaceSet elements indicating whether a triangle was intersected by the plane.
                     if isTriHit[j] then triangle SO->FaceSetList[3*j], [3*j+1], [3*j+2] was intersected. You should
                     have a total of N_IntersTri YUP values in this vector*/
                        
} SUMA_SURF_PLANE_INTERSECT;




/*!
   Structure for passing info to function SUMA_SaveSOascii
*/
typedef struct {
   SUMA_SurfaceObject *SO;
   SUMA_SurfaceViewer *sv;
} SUMA_SAVESO_STRUCT;

/*!
   Structure containing a named color 
*/
typedef struct {
   float r;
   float g;
   float b;
   float a;
   char Name[SUMA_MAX_COLOR_NAME];
} SUMA_RGB_NAME;

/*!
   Structure containing the colormaps used by AFNI 
*/
typedef struct {
   SUMA_COLOR_MAP **CMv;   /* a vector of pointers to colormap structures */
   int N_maps;             /* the number of colormaps in CMv */
   SUMA_RGB_NAME *Cv;      /* a vector of RGB_Name structures containing the named colors used by AFNI */
   int N_cols;             /* the number of defined colors in Cv */
} SUMA_AFNI_COLORS;

typedef struct {              
   int N_Neighb;           /*!< Number of neighbors for a particular node */
   int *Neighb_ind;        /*!< N_Neighb x 1 vector containing  nodes neighboring node i */
   float *Neighb_dist;     /*!< N_Neighb x 1 vector containing node distances from node i. 
                               These are the shortes distances ON THE GRAPH. */
   float *Neighb_PropLoc;  /*!< N_Neighb x 3 vector containing XYZ of estimated node propagation
                                from one layer to the next. */
} SUMA_OFFSET_STRUCT;      /*!< The structure returned by SUMA_FormNeighbOffset */

typedef struct {
   int *NodesInLayer;   /*!< Vector containing nodes that are neighbors to node n
                            (Vector contains N_NodesInLayer useful values but has
                             N_AllocNodesInLayer allocated spots)
                            Those neighbors can be of any order (see below and
                             structure SUMA_GET_OFFSET_STRUCT) */
   int N_NodesInLayer;     /*!< Number of nodes in this layer */
   int N_AllocNodesInLayer;   /*!< Number of nodes allocated for in this layer.
                                 Allocation is done in chunks of 200 at a time */
} SUMA_NODE_NEIGHB_LAYER;  /*!< Structure containing the layers neighboring 
                                 a node n.
                                The 0th order layer contain n only
                                The 1st order layer contains the first 
                                order neighbors of n
                                etc.. */

typedef struct {
   int N_layers;    /*!< Number of node neighborhoods of a certain node n. 
                    The 0th order layer contain n only.
                    The 1st order layer contains the first order neighbors of n.
                                etc.. */
   SUMA_NODE_NEIGHB_LAYER *layers;  /*!<  layers[0] is the zeroth order layer
                                          layers[1] is the 1st order layer, etc.
                                          See  SUMA_NODE_NEIGHB_LAYER */
   
   int N_Nodes;            /*!< Number of nodes in mesh */
   int *LayerVect;         /*!< vector containing the neighborhood layer of a 
                                certain node from node n.
              LayerVect[5] = 2; means node 5 is a 2nd order neighbor to node n.
              LayerVect[5] = -1; means node 5 is not within the limit distance
                                 from node n.
              All values in LayerVect are initialized to -1 */
   float *OffVect;         /*!< vector containing the distance of nodes in the 
                                 mesh from node n.
                d = OffVect[5]; is the geodesic distance of node 5 from node n.
                OffVect is N_Nodes long. 
                OffVect[k] is meaningless if LayerVect[k] < 0 */
} SUMA_GET_OFFSET_STRUCT;  /*!< Structure containing nodes that are within a
               certain geodesic distance (lim) from a certain node n. */

typedef struct {
   int talk_suma;
   int comm_NI_mode;
   float rps;
   float nelps;  /*!<   number of NI elements to send per second 
                        -1 for going as fast as possible */
   int TrackID;            /*!<  ID of next element to be sent 
                                 NI_element StartTracking has an ID of 1 */
   SUMA_Boolean GoneBad;   /*!< Flag indicating that stream went bad */
   SUMA_Boolean Send;      /*!< Flag indicating that elements should be sent 
                                As long as GoneBad is NOPE */
   SUMA_Boolean afni_GoneBad;
   SUMA_Boolean afni_Send;
   int istream; /*!< index of the stream used in SUMAg_CF->ns_v */
   int afni_istream; /*!< index of stream used to connect to afni */
   char *suma_host_name;
   char *afni_host_name; 
   int ElInd[SUMA_N_DSET_TYPES]; /* index of elements of a certain type 
                                    to be sent to SUMA */
   int kth;    /* send kth element to SUMA */
   int Feed2Afni;
} SUMA_COMM_STRUCT;

typedef enum {
   SUMA_DOMAINS_ERROR = -1,
   SUMA_DOMAINS_NOT_RELATED = 0,  /*!< Surfaces are not related 
                                       Above that flag, surfaces are related*/
   SUMA_SO1_is_SO2,           /*!< Surface1 is the same as Surface2 */
   SUMA_SO1_is_LDPSO2,        /*!< SO1 is the local domain parent of SO2 */
   SUMA_SO2_is_LDPSO1,        /*!< SO2 is the local domain parent of SO1 */
   SUMA_LDPSO1_is_LDPSO2,   /*!< SO1 and SO2 have the same local domain parent */
   SUMA_NUCELAR_FAMILY,       /*!< A flag to indicate limit of immediate kinship
                                  (Don't blame me, official syntax in use) 
                                  Above that flag, kinship is distant */
   SUMA_SO1_is_GPSO2,         /*!< SO1 is the granddaddy of SO2 */
   SUMA_SO2_is_GPSO1,         /*!< SO2 is the granddaddy of SO1 */
   SUMA_GPSO1_is_GPSO2,     /*!< SO1 and SO2 have the same  granddaddy*/
   
   SUMA_N_NODE_SAME,        /*! SO1 and SO2 have the same number of nodes */
} SUMA_DOMAIN_KINSHIPS; /*!< The type of relationships between surfaces, modify 
                              function SUMA_DomainKinships_String; */
typedef struct {
   char name[100];
   double lastcall;
   struct timeval tt;
} SUMA_TIMER;

#define SUMA_MAX_N_TIMER 50

/*! structure containing information global to all surface viewers */
typedef struct {
   SUMA_Boolean Dev; /*!< Flag for developer option 
                     (allows the use of confusing or kludge options) */
   SUMA_Boolean Fake_Cmap; /*!< Flag to create X11 cmap for the purpose of 
                              automated picture taking. It should not be set
                              on for any other reason. */
   SUMA_Boolean InOut_Notify; /*!< prints to STDERR a notice when a function 
                                 is entered or exited */ 
   SUMA_Boolean Echo_KeyPress; /*!< prints to STDERR a notice when a function 
                                 is entered or exited */ 
   int InOut_Level; /*!< level of nested function calls */
   int PointerSize; /*!< size of void * */
   int N_OpenSV; /*!< Number of open (visible) surface viewers.
                     Do not confuse this with the number of surface viewers
                     created (SUMAg_N_SVv)*/
   
   SUMA_MEMTRACE_STRUCT *Mem; /*!< structure used to keep track of 
                                    memory usage */
   SUMA_Boolean MemTrace; /*!< Flag for keeping track of memory usage 
                              (must also set SUMA_MEMTRACE_FLAG ) */

   char HostName_v[SUMA_MAX_STREAMS][SUMA_MAX_NAME_LENGTH];   
         /*!<  name or ipaddress of hosts maximum allowed name is 20
             chars less than allocated for, see SUMA_Assign_AfniHostName
             *** Dec. 19 03: This field used to be called AfniHostName
             It is now a vector of hostnmes allowing for multiple
             connections. 
             AfniHostName = HostName_v[SUMA_AFNI_STREAM_INDEX]*/ 
   char NimlStream_v[SUMA_MAX_STREAMS][SUMA_MAX_NAME_LENGTH]; 
         /*!< niml stream name for communicating with other programs 
           *** Dec. 19 03: This field used to be called AfniNimlStream
           AfniNimlStream = NimlStream_v[SUMA_AFNI_STREAM_INDEX]*/
   NI_stream ns_v[SUMA_MAX_STREAMS]; /*!< 
                     *** Pre: Dec 19 03:
                     Stream used to communicate with AFNI. 
                     It is null when no communication stream is open. 
                     The stream can be open with Connected set to NOPE.
                     In that case no communication between the two programs
                     but resuming communication is easy since surfaces need
                     not be sent to AFNI again as would be the case if the stream 
                     was completely closed 
                     *** Dec. 19 03
                     Used to be called ns for connecting to AFNI.
                     ns = ns_v[SUMA_AFNI_STREAM_INDEX]*/
   int ns_to[SUMA_MAX_STREAMS];
   int ns_toc[SUMA_MAX_STREAMS];
   int ns_flags_v[SUMA_MAX_STREAMS];
   int TCP_port[SUMA_MAX_STREAMS];
   int TalkMode[SUMA_MAX_STREAMS];  /* Talk text or talk binary? */
   SUMA_Boolean Connected_v[SUMA_MAX_STREAMS]; 
     /*!< YUP/NOPE, if SUMA is sending (or accepting) communication from AFNI 
         *** Dec. 19 03
         Vectorized Connected like fields above*/
   int TrackingId_v[SUMA_MAX_STREAMS]; 
      /*!<  for keeping track of serial number of incoming nels 
            0 if not keeping track. So start numbering at 1*/


   SUMA_Boolean Listening; /*!< SUMA is listening for connections */
   SUMA_Boolean niml_work_on; /*!< Flag indicating that niml workprocess is ON */
   SUMA_LINK_TYPES Locked[SUMA_MAX_SURF_VIEWERS]; 
         /*!< All viewers i such that Locked[i] != SUMA_No_Lock 
               have their cross hair locked together */   
   SUMA_Boolean ViewLocked[SUMA_MAX_SURF_VIEWERS]; 
         /*!< All viewers i such that ViewLocked[i] = YUP 
               have their view point locked together */    
   SUMA_Boolean SwapButtons_1_3; 
         /*!< YUP/NOPE, if functions of mouse buttons 1 and 3 are swapped */
   SUMA_X_AllView *X; 
         /*!< structure containing widgets and other X related 
               variables that are common to all viewers */ 
   DList *MessageList; 
      /*!< a doubly linked list with data elements 
           containing notices, warnings and error messages*/
   SUMA_Boolean ROI_mode; 
      /*!< Flag specifying that SUMA is in ROI drawing mode */
   SUMA_Boolean ROI_contmode;
      /*!< Flag specifying that ROI contours should be drawn 
            This field and Pen_mode and perhaps ROI_CM should
            be inside SUMA_X_DrawROI, not here!*/
   SUMA_Boolean Pen_mode;  
      /*!< Flag specifying that a pen is being used for drawing */
   SUMA_COLOR_MAP *ROI_CM; 
      /*!< Color map used to map an ROI's index to a color */
   SUMA_ROI_FILL_MODES ROI_FillMode; 
      /*!< flag indicating how to fill a closed contour */
   SUMA_COL_MIX_MODE ColMixMode; 
      /*!< controls the way colors from multiple planes are mixed together */
   SUMA_Boolean ROI2afni; /*!< Send ROIs to afni as you draw them*/
   int nimlROI_Datum_type; /*!< the code for nimlROI_Datum_type */

   char **GroupList; /*!< Names of surface groups */
   int N_Group;   /*!< number of groups  available */

   SUMA_AFNI_COLORS *scm;  
      /*!< a structure containing all the colormaps available to SUMA */
   DList *DsetList;  /*!< List containing datasets */
   SUMA_Boolean Allow_Dset_Replace; 
      /*!< Allow replacement of old dset with new dset having same id */
   SUMA_Boolean Home_After_Prying; 
      /*!< Call 'Home' callback as prying is applied */
   int SUMA_ThrScalePowerBias;
   int SUMA_SnapshotOverSampling; 
   SUMA_Boolean IgnoreVolreg; 
      /*!< if YUP then ignore any Volreg or TagAlign 
         transform in the header of the surface volume */
   SUMA_Boolean isGraphical; 
      /*!<if YUP then Named afni colors will 
          get resolved when creating color maps. 
          Otherwise they are set to gray. 
          Only suma and ScaleToMap will need to set 
          this variable to YUP, for the moment June 3 05 */

   int N_ClipPlanes; 
      /*!< Number of screen clipping planes, 3 max allowed */
   GLdouble ClipPlanes[4*SUMA_MAX_N_CLIP_PLANES]; 
      /*!< Equations of  clipping planes */
   SUMA_CLIP_PLANE_TYPES ClipPlaneType[SUMA_MAX_N_CLIP_PLANES]; 
      /*!< Screen clipping, object clipping, etc. */
   char ClipPlanesLabels[SUMA_MAX_N_CLIP_PLANES][9]; 
   
   int NoDuplicatesInRecorder;
   int N_Timer;
   SUMA_TIMER Timer[SUMA_MAX_N_TIMER];
   
   char *cwd;
   
   float CmapRotaFrac; /*!< fraction by which to rotate colormap */
   int   TransModeStep; /*!< levels to jump when changing transparency*/
   DList *xforms;    /*!<  List of transforms that apply to certain dsets 
                           or surfaces */
   DList *callbacks; /*!< List of callbacks that apply to certain dsets or
                          surfaces */
   
   SUMA_Boolean HoldClickCallbacks;
   int PointerLastInViewer; /* index of latest viewer visited by mouse pointer
                           -1 if unknown */
   
   GICOR_setup *giset; /*!< parameters for group icorr setup */
   INSTA_TRACT_SETUP *ITset; /*!< parameters for instatract setup */
   
   SUMA_PARSED_NAME *autorecord; /*!< Autorecord prefix */
   
   DList *SaveList; /*!< List of objects set to be saved when user chooses to */
   
   SUMA_Boolean YokeIntToNode;
   
   SUMA_EVENT *lev; /*!< A record of the last input event */

   char **dcom;
   int N_dcom;
   
} SUMA_CommonFields;


typedef enum { SUMA_NO_SORT, SUMA_BY_PLANE_DISTANCE, SUMA_BY_SEGMENT_DISTANCE, SUMA_SORT_BY_LLC_DISTANCE, SUMA_SORT_BY_LL_QUAD } SUMA_SORT_BOX_AXIS_OPTION;
typedef enum { SUMA_LOWER_LEFT_SCREEN, SUMA_UPPER_LEFT_SCREEN, SUMA_UPPER_RIGHT_SCREEN, SUMA_LOWER_RIGHT_SCREEN } SUMA_SCREEN_CORNERS;
typedef struct {
   double world_length;
   double ScreenProj[3];
   double ScreenProj_xy_length;
   double P1[3];
   double P2[3];
   int SegIndex;
   int PointIndex[2];
   int FaceIndex[2];
   int Quad[2];
   double tick1_dir[3];
   double tick2_dir[3];
   double MidFaceDist;
   double MidSegDist;
   int AxisDim;
   int LLCclosestPoint;
   double LLCclosestDist;
   double TxOff[3];
}  SUMA_AxisSegmentInfo;



  
typedef struct {
   char *master;
   char *mask;
   char *prefix;
   char *prefix_path;
   char *orcode;
   int do_ijk;
   int dimen_ii;
   int dimen_jj;
   int dimen_kk;
   int datum;
   float dval;
   float fval;
   byte *mmask;
   int full_list;
   THD_3dim_dataset *mset;
   int exists;
   int coorder_xyz;
}  SUMA_FORM_AFNI_DSET_STRUCT;
 
extern SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
extern SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures */
extern int SUMAg_N_SVv; /*!< Number of SVs stored in SVv */
extern SUMA_DO *SUMAg_DOv;	/*!< Global pointer to Displayable Object structure vector*/
extern int SUMAg_N_DOv; /*!< Number of DOs stored in DOv */
extern SUMA_CommonFields *SUMAg_CF; /*!< Global pointer to structure containing info common to all viewers */

#endif
