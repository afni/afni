/*
 * vp_renderR.c
 *
 * Function to render raw (unclassified) volumes.
 *
 * Copyright (c) 1994 The Board of Trustees of The Leland Stanford
 * Junior University.  All rights reserved.
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice and this permission notice appear in
 * all copies of this software and that you do not sell the software.
 * Commercial licensing is available by contacting the author.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Author:
 *    Phil Lacroute
 *    Computer Systems Laboratory
 *    Electrical Engineering Dept.
 *    Stanford University
 */

/*
 * $Date$
 * $Revision$
 */

#include "vp_global.h"

#define COMP_AR1PB_FUNC		VPCompAR1PB
extern void VPCompAR1PB();

#define COMP_AR3PB_FUNC		VPCompAR3PB
extern void VPCompAR3PB();

#ifdef COMP_AR11B
#define COMP_AR11B_FUNC		VPCompAR11B
extern void VPCompAR11B();
#else
#define COMP_AR11B_FUNC		VPCompAR1NB
#endif

#ifdef COMP_AR31B
#define COMP_AR31B_FUNC		VPCompAR31B
extern void VPCompAR31B();
#else
#define COMP_AR31B_FUNC		VPCompAR3NB
#endif

#ifdef COMP_AR12B
#define COMP_AR12B_FUNC		VPCompAR12B
extern void VPCompAR12B();
#else
#define COMP_AR12B_FUNC		VPCompAR1NB
#endif

#ifdef COMP_AR32B
#define COMP_AR32B_FUNC		VPCompAR32B
extern void VPCompAR32B();
#else
#define COMP_AR32B_FUNC		VPCompAR3NB
#endif

#define COMP_AR1NB_FUNC		VPCompAR1NB
extern void VPCompAR1NB();

#define COMP_AR3NB_FUNC		VPCompAR3NB
extern void VPCompAR3NB();


#define COMP_AR1PS_FUNC		VPCompAR1PB

#define COMP_AR3PS_FUNC		VPCompAR3PB

#ifdef COMP_AR11S
#define COMP_AR11S_FUNC		VPCompAR11S
extern void VPCompAR11S();
#else
#define COMP_AR11S_FUNC		VPCompAR1NS
#endif

#ifdef COMP_AR31S
#define COMP_AR31S_FUNC		VPCompAR31S
extern void VPCompAR31S();
#else
#define COMP_AR31S_FUNC		VPCompAR3NS
#endif

#ifdef COMP_AR12S
#define COMP_AR12S_FUNC		VPCompAR12S
extern void VPCompAR12S();
#else
#define COMP_AR12S_FUNC		VPCompAR1NS
#endif

#ifdef COMP_AR32S
#define COMP_AR32S_FUNC		VPCompAR32S
extern void VPCompAR32S();
#else
#define COMP_AR32S_FUNC		VPCompAR3NS
#endif

#define COMP_AR1NS_FUNC		VPCompAR1NS
extern void VPCompAR1NS();

#define COMP_AR3NS_FUNC		VPCompAR3NS
extern void VPCompAR3NS();

#ifdef INDEX_VOLUME
extern void VPCompAI11B();
#endif

#define SHADOWS_OFF		0
#define SHADOWS_ON		1
#define SHADOW_OPTS		2

#define MATERIAL_CALLBACK	0
#define MATERIAL_ONE		1
#define MATERIAL_TWO		2
#define MATERIAL_MORE		3
#define MATERIAL_OPTS		4

#define COLOR_GRAY		0
#define COLOR_RGB		1
#define COLOR_OPTS		2

static void (*AffineProcTable[SHADOW_OPTS][MATERIAL_OPTS][COLOR_OPTS])() = {
    {
	{ COMP_AR1PB_FUNC, COMP_AR3PB_FUNC },
	{ COMP_AR11B_FUNC, COMP_AR31B_FUNC },
	{ COMP_AR12B_FUNC, COMP_AR32B_FUNC },
	{ COMP_AR1NB_FUNC, COMP_AR3NB_FUNC }
    },
    {
	{ COMP_AR1PS_FUNC, COMP_AR3PS_FUNC },
	{ COMP_AR11S_FUNC, COMP_AR31S_FUNC },
	{ COMP_AR12S_FUNC, COMP_AR32S_FUNC },
	{ COMP_AR1NS_FUNC, COMP_AR3NS_FUNC }
    }
};

/*
 * vpRenderRawVolume
 *
 * Render an uclassified volume using the shear-warp algorithm.
 */

vpResult
vpRenderRawVolume(vpc)
vpContext *vpc;
{
    int retcode;
    void (*composite_func)();
    int shadow_option, material_option, color_option;

    /* check for errors and initialize */
    if ((retcode = VPCheckRawVolume(vpc)) != VP_OK)
	return(retcode);
    if ((retcode = VPCheckClassifier(vpc)) != VP_OK)
	return(retcode);
    if ((retcode = VPCheckShader(vpc)) != VP_OK)
	return(retcode);
    if ((retcode = VPCheckImage(vpc)) != VP_OK)
	return(retcode);
    if (vpc->num_clsfy_params > 2)
	return(VPSetError(vpc, VPERROR_LIMIT_EXCEEDED));
    if ((retcode = VPFactorView(vpc)) != VP_OK)
	return(retcode);

    Debug((vpc, VPDEBUG_RENDER, "Algorithm: affine RAWvolume (%s)\n",
	   vpc->mm_octree == NULL ? "no octree" : "with octree"));

    /* determine which options are enabled */
    if (vpc->enable_shadows)
	shadow_option = SHADOWS_ON;
    else
	shadow_option = SHADOWS_OFF;
    if (vpc->shading_mode == CALLBACK_SHADER)
	material_option = MATERIAL_CALLBACK;
    else if (vpc->num_materials == 1)
	material_option = MATERIAL_ONE;
    else if (vpc->num_materials == 2)
	material_option = MATERIAL_TWO;
    else
	material_option = MATERIAL_MORE;
    if (vpc->color_channels == 1)
	color_option = COLOR_GRAY;
    else
	color_option = COLOR_RGB;

    /* render */
    if (vpc->affine_view) {
	/* choose a compositing function */
	composite_func = AffineProcTable[shadow_option][material_option]
					[color_option];
	VPRenderAffine(vpc, USE_RAWVOLUME, composite_func);
    } else {
	/* XXX perspective rendering not available yet */
	return(VPSetError(vpc, VPERROR_BAD_OPTION));
    }

    return(VP_OK);
}
