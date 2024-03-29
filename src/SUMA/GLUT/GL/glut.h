#ifndef __glut_h__
#define __glut_h__

/* Copyright (c) Mark J. Kilgard, 1994, 1995, 1996. */

/* This program is freely distributable without licensing fees  and is
   provided without guarantee or warrantee expressed or  implied. This
   program is -not- in the public domain. */

#include <GL/gl.h>
#include <GL/glu.h>

#ifdef __cplusplus
extern "C" {
#endif

/* 
 * GLUT API revision history:
 *
 * GLUT_API_VERSION is updated to reflect incompatible GLUT
 * API changes (interface changes, semantic changes, deletions,
 * or additions).
 *
 * GLUT_API_VERSION=1  First public release of GLUT.  11/29/94
 *
 * GLUT_API_VERSION=2  Added support for OpenGL/GLX multisampling,
 * extension.  Supports new input devices like tablet, dial and button
 * box, and Spaceball.  Easy to query OpenGL extensions.
 *
 * GLUT_API_VERSION=3  glutMenuStatus added.
 *
 */
#ifndef GLUT_API_VERSION  /* allow this to be overridden */
#define GLUT_API_VERSION		3
#endif

/* 
 * GLUT implementation revision history:
 * 
 * GLUT_XLIB_IMPLEMENTATION is updated to reflect both GLUT
 * API revisions and implementation revisions (ie, bug fixes).
 *
 * GLUT_XLIB_IMPLEMENTATION=1  mjk's first public release of
 * GLUT Xlib-based implementation.  11/29/94
 *
 * GLUT_XLIB_IMPLEMENTATION=2  mjk's second public release of
 * GLUT Xlib-based implementation providing GLUT version 2 
 * interfaces.
 *
 * GLUT_XLIB_IMPLEMENTATION=3  mjk's GLUT 2.2 images. 4/17/95
 *
 * GLUT_XLIB_IMPLEMENTATION=4  mjk's GLUT 2.3 images. 6/?/95
 *
 * GLUT_XLIB_IMPLEMENTATION=5  mjk's GLUT 3.0 images. 10/?/95
 *
 * GLUT_XLIB_IMPLEMENTATION=5  mjk's GLUT 3.1 images.
 */
#ifndef GLUT_XLIB_IMPLEMENTATION  /* allow this to be overridden */
#define GLUT_XLIB_IMPLEMENTATION	6
#endif

/* display mode bit masks */
#define GLUT_RGB			0
#define GLUT_RGBA			GLUT_RGB
#define GLUT_INDEX			1
#define GLUT_SINGLE			0
#define GLUT_DOUBLE			2
#define GLUT_ACCUM			4
#define GLUT_ALPHA			8
#define GLUT_DEPTH			16
#define GLUT_STENCIL			32
#if (GLUT_API_VERSION >= 2)
#define GLUT_MULTISAMPLE		128
#define GLUT_STEREO			256
#endif
#if (GLUT_API_VERSION >= 3)
#define GLUT_LUMINANCE			512
#endif

/* mouse buttons */
#define GLUT_LEFT_BUTTON		0
#define GLUT_MIDDLE_BUTTON		1
#define GLUT_RIGHT_BUTTON		2

/* mouse button callback state */
#define GLUT_DOWN			0
#define GLUT_UP				1

#if (GLUT_API_VERSION >= 2)
/* function keys */
#define GLUT_KEY_F1			1
#define GLUT_KEY_F2			2
#define GLUT_KEY_F3			3
#define GLUT_KEY_F4			4
#define GLUT_KEY_F5			5
#define GLUT_KEY_F6			6
#define GLUT_KEY_F7			7
#define GLUT_KEY_F8			8
#define GLUT_KEY_F9			9
#define GLUT_KEY_F10			10
#define GLUT_KEY_F11			11
#define GLUT_KEY_F12			12
/* directional keys */
#define GLUT_KEY_LEFT			100
#define GLUT_KEY_UP			101
#define GLUT_KEY_RIGHT			102
#define GLUT_KEY_DOWN			103
#define GLUT_KEY_PAGE_UP		104
#define GLUT_KEY_PAGE_DOWN		105
#define GLUT_KEY_HOME			106
#define GLUT_KEY_END			107
#define GLUT_KEY_INSERT			108
#endif

/* entry/exit callback state */
#define GLUT_LEFT			0
#define GLUT_ENTERED			1

/* menu usage callback state */
#define GLUT_MENU_NOT_IN_USE		0
#define GLUT_MENU_IN_USE		1

/* visibility callback state */
#define GLUT_NOT_VISIBLE		0
#define GLUT_VISIBLE			1

/* color index component selection values */
#define GLUT_RED			0
#define GLUT_GREEN			1
#define GLUT_BLUE			2

/* layers for use */
#define GLUT_NORMAL			0
#define GLUT_OVERLAY			1

/* stroke font opaque addresses (use constants instead in source code) */
extern void *glutStrokeRoman;
extern void *glutStrokeMonoRoman;

/* stroke font constants (use these in GLUT program) */
#define GLUT_STROKE_ROMAN		(&glutStrokeRoman)
#define GLUT_STROKE_MONO_ROMAN		(&glutStrokeMonoRoman)

/* bitmap font opaque addresses (use constants instead in source code) */
extern void *glutBitmap9By15;
extern void *glutBitmap8By13;
extern void *glutBitmapTimesRoman10;
extern void *glutBitmapTimesRoman24;
extern void *glutBitmapHelvetica10;
extern void *glutBitmapHelvetica12;
extern void *glutBitmapHelvetica18;

/* bitmap font constants (use these in GLUT program) */
#define GLUT_BITMAP_9_BY_15		(&glutBitmap9By15)
#define GLUT_BITMAP_8_BY_13		(&glutBitmap8By13)
#define GLUT_BITMAP_TIMES_ROMAN_10	(&glutBitmapTimesRoman10)
#define GLUT_BITMAP_TIMES_ROMAN_24	(&glutBitmapTimesRoman24)
#if (GLUT_API_VERSION >= 3)
#define GLUT_BITMAP_HELVETICA_10	(&glutBitmapHelvetica10)
#define GLUT_BITMAP_HELVETICA_12	(&glutBitmapHelvetica12)
#define GLUT_BITMAP_HELVETICA_18	(&glutBitmapHelvetica18)
#endif

/* glutGet parameters */
#define GLUT_WINDOW_X			100
#define GLUT_WINDOW_Y			101
#define GLUT_WINDOW_WIDTH		102
#define GLUT_WINDOW_HEIGHT		103
#define GLUT_WINDOW_BUFFER_SIZE		104
#define GLUT_WINDOW_STENCIL_SIZE	105
#define GLUT_WINDOW_DEPTH_SIZE		106
#define GLUT_WINDOW_RED_SIZE		107
#define GLUT_WINDOW_GREEN_SIZE		108
#define GLUT_WINDOW_BLUE_SIZE		109
#define GLUT_WINDOW_ALPHA_SIZE		110
#define GLUT_WINDOW_ACCUM_RED_SIZE	111
#define GLUT_WINDOW_ACCUM_GREEN_SIZE	112
#define GLUT_WINDOW_ACCUM_BLUE_SIZE	113
#define GLUT_WINDOW_ACCUM_ALPHA_SIZE	114
#define GLUT_WINDOW_DOUBLEBUFFER	115
#define GLUT_WINDOW_RGBA		116
#define GLUT_WINDOW_PARENT		117
#define GLUT_WINDOW_NUM_CHILDREN	118
#define GLUT_WINDOW_COLORMAP_SIZE	119
#if (GLUT_API_VERSION >= 2)
#define GLUT_WINDOW_NUM_SAMPLES		120
#define GLUT_WINDOW_STEREO		121
#endif
#if (GLUT_API_VERSION >= 3)
#define GLUT_WINDOW_CURSOR		122
#endif
#define GLUT_SCREEN_WIDTH		200
#define GLUT_SCREEN_HEIGHT		201
#define GLUT_SCREEN_WIDTH_MM		202
#define GLUT_SCREEN_HEIGHT_MM		203
#define GLUT_MENU_NUM_ITEMS		300
#define GLUT_DISPLAY_MODE_POSSIBLE	400
#define GLUT_INIT_WINDOW_X		500
#define GLUT_INIT_WINDOW_Y		501
#define GLUT_INIT_WINDOW_WIDTH		502
#define GLUT_INIT_WINDOW_HEIGHT		503
#define GLUT_INIT_DISPLAY_MODE		504
#if (GLUT_API_VERSION >= 2)
#define GLUT_ELAPSED_TIME		700
#endif

#if (GLUT_API_VERSION >= 2)
/* glutDeviceGet parameters */
#define GLUT_HAS_KEYBOARD		600
#define GLUT_HAS_MOUSE			601
#define GLUT_HAS_SPACEBALL		602
#define GLUT_HAS_DIAL_AND_BUTTON_BOX	603
#define GLUT_HAS_TABLET			604
#define GLUT_NUM_MOUSE_BUTTONS		605
#define GLUT_NUM_SPACEBALL_BUTTONS	606
#define GLUT_NUM_BUTTON_BOX_BUTTONS	607
#define GLUT_NUM_DIALS			608
#define GLUT_NUM_TABLET_BUTTONS		609
#endif

#if (GLUT_API_VERSION >= 3)
/* glutLayerGet parameters */
#define GLUT_OVERLAY_POSSIBLE           800
#define GLUT_LAYER_IN_USE		801
#define GLUT_HAS_OVERLAY		802
#define GLUT_TRANSPARENT_INDEX		803
#define GLUT_NORMAL_DAMAGED		804
#define GLUT_OVERLAY_DAMAGED		805

/* glutUseLayer parameters */
#define GLUT_NORMAL			0
#define GLUT_OVERLAY			1

/* glutGetModifiers return mask */
#define GLUT_ACTIVE_SHIFT               1
#define GLUT_ACTIVE_CTRL                2
#define GLUT_ACTIVE_ALT                 4

/* glutSetCursor parameters */
/* Basic arrows */
#define GLUT_CURSOR_RIGHT_ARROW		0
#define GLUT_CURSOR_LEFT_ARROW		1
/* Symbolic cursor shapees */
#define GLUT_CURSOR_INFO		2
#define GLUT_CURSOR_DESTROY		3
#define GLUT_CURSOR_HELP		4
#define GLUT_CURSOR_CYCLE		5
#define GLUT_CURSOR_SPRAY		6
#define GLUT_CURSOR_WAIT		7
#define GLUT_CURSOR_TEXT		8
#define GLUT_CURSOR_CROSSHAIR		9
/* Directional cursors */
#define GLUT_CURSOR_UP_DOWN		10
#define GLUT_CURSOR_LEFT_RIGHT		11
/* Sizing cursors */
#define GLUT_CURSOR_TOP_SIDE		12
#define GLUT_CURSOR_BOTTOM_SIDE		13
#define GLUT_CURSOR_LEFT_SIDE		14
#define GLUT_CURSOR_RIGHT_SIDE		15
#define GLUT_CURSOR_TOP_LEFT_CORNER	16
#define GLUT_CURSOR_TOP_RIGHT_CORNER	17
#define GLUT_CURSOR_BOTTOM_RIGHT_CORNER	18
#define GLUT_CURSOR_BOTTOM_LEFT_CORNER	19
/* Inherit from parent window */
#define GLUT_CURSOR_INHERIT		100
/* Blank cursor */
#define GLUT_CURSOR_NONE		101
/* Fullscreen crosshair (if available) */
#define GLUT_CURSOR_FULL_CROSSHAIR	102
#endif

/* GLUT initialization sub-API */
extern void glutInit(int *argcp, char **argv);
extern void glutInitDisplayMode(unsigned int mode);
extern void glutInitWindowPosition(int x, int y);
extern void glutInitWindowSize(int width, int height);
extern void glutMainLoop(void);

/* GLUT window sub-api */
extern int glutCreateWindow(char *title);
extern int glutCreateSubWindow(int win, int x, int y, int width, int height);
extern void glutDestroyWindow(int win);
extern void glutPostRedisplay(void);
extern void glutSwapBuffers(void);
extern int glutGetWindow(void);
extern void glutSetWindow(int win);
extern void glutSetWindowTitle(char *title);
extern void glutSetIconTitle(char *title);
extern void glutPositionWindow(int x, int y);
extern void glutReshapeWindow(int width, int height);
extern void glutPopWindow(void);
extern void glutPushWindow(void);
extern void glutIconifyWindow(void);
extern void glutShowWindow(void);
extern void glutHideWindow(void);
#if (GLUT_API_VERSION >= 3)
extern void glutFullScreen(void);
extern void glutSetCursor(int cursor);

/* GLUT overlay sub-API */
extern void glutEstablishOverlay(void);
extern void glutRemoveOverlay(void);
extern void glutUseLayer(GLenum layer);
extern void glutPostOverlayRedisplay(void);
extern void glutShowOverlay(void);
extern void glutHideOverlay(void);
#endif

/* GLUT menu sub-API */
extern int glutCreateMenu(void (*)(int));
extern void glutDestroyMenu(int menu);
extern int glutGetMenu(void);
extern void glutSetMenu(int menu);
extern void glutAddMenuEntry(char *label, int value);
extern void glutAddSubMenu(char *label, int submenu);
extern void glutChangeToMenuEntry(int item, char *label, int value);
extern void glutChangeToSubMenu(int item, char *label, int submenu);
extern void glutRemoveMenuItem(int item);
extern void glutAttachMenu(int button);
extern void glutDetachMenu(int button);

/* GLUT callback sub-api */
extern void glutDisplayFunc(void (*)(void));
extern void glutReshapeFunc(void (*)(int width, int height));
extern void glutKeyboardFunc(void (*)(unsigned char key, int x, int y));
extern void glutMouseFunc(void (*)(int button, int state, int x, int y));
extern void glutMotionFunc(void (*)(int x, int y));
extern void glutPassiveMotionFunc(void (*)(int x, int y));
extern void glutEntryFunc(void (*)(int state));
extern void glutVisibilityFunc(void (*)(int state));
extern void glutIdleFunc(void (*)(void));
extern void glutTimerFunc(unsigned int millis, void (*)(int value), int value);
extern void glutMenuStateFunc(void (*)(int state));
#if (GLUT_API_VERSION >= 2)
extern void glutSpecialFunc(void (*)(int key, int x, int y));
extern void glutSpaceballMotionFunc(void (*)(int x, int y, int z));
extern void glutSpaceballRotateFunc(void (*)(int x, int y, int z));
extern void glutSpaceballButtonFunc(void (*)(int button, int state));
extern void glutButtonBoxFunc(void (*)(int button, int state));
extern void glutDialsFunc(void (*)(int dial, int value));
extern void glutTabletMotionFunc(void (*)(int x, int y));
extern void glutTabletButtonFunc(void (*)(int button, int state, int x, int y));
#if (GLUT_API_VERSION >= 3)
extern void glutMenuStatusFunc(void (*)(int status, int x, int y));
extern void glutOverlayDisplayFunc(void (*)(void));
#endif
#endif

/* GLUT color index sub-api */
extern void glutSetColor(int, GLfloat red, GLfloat green, GLfloat blue);
extern GLfloat glutGetColor(int ndx, int component);
extern void glutCopyColormap(int win);

/* GLUT state retrieval sub-api */
extern int glutGet(GLenum type);
extern int glutDeviceGet(GLenum type);
#if (GLUT_API_VERSION >= 2)
/* GLUT extension support sub-API */
extern int glutExtensionSupported(char *name);
#endif
#if (GLUT_API_VERSION >= 3)
extern int glutGetModifiers(void);
extern int glutLayerGet(GLenum type);
#endif

/* GLUT font sub-API */
extern void glutBitmapCharacter(void *font, int character);
extern int glutBitmapWidth(void *font, int character);
extern void glutStrokeCharacter(void *font, int character);
extern int glutStrokeWidth(void *font, int character);

/* GLUT pre-built models sub-API */
extern void glutWireSphere(GLdouble radius, GLint slices, GLint stacks);
extern void glutSolidSphere(GLdouble radius, GLint slices, GLint stacks);
extern void glutWireCone(GLdouble base, GLdouble height, GLint slices, GLint stacks);
extern void glutSolidCone(GLdouble base, GLdouble height, GLint slices, GLint stacks);
extern void glutWireCube(GLdouble size);
extern void glutSolidCube(GLdouble size);
extern void glutWireTorus(GLdouble innerRadius, GLdouble outerRadius, GLint sides, GLint rings);
extern void glutSolidTorus(GLdouble innerRadius, GLdouble outerRadius, GLint sides, GLint rings);
extern void glutWireDodecahedron(void);
extern void glutSolidDodecahedron(void);
extern void glutWireTeapot(GLdouble size);
extern void glutSolidTeapot(GLdouble size);
extern void glutWireOctahedron(void);
extern void glutSolidOctahedron(void);
extern void glutWireTetrahedron(void);
extern void glutSolidTetrahedron(void);
extern void glutWireIcosahedron(void);
extern void glutSolidIcosahedron(void);

#ifdef __cplusplus
}

#endif
#endif                  /* __glut_h__ */
