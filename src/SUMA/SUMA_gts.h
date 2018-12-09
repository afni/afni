/***************************************************************************
                          sumagts.h  -  description
                             -------------------
    begin                : Mon Jun 14 2004
    copyright            : (C) 2004 by Jakub Otwinowski
    email                : jakub@hurin
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef __SUMAGTS_H__
#define __SUMAGTS_H__
#ifdef HAVE_CONFIG_H
#include "gts/config.h"
#endif
#include "gts.h"
#include "gts-private.h"

#include "SUMA_suma.h"

#define GTS_OUT(fname,s){\
   FILE *m_fp = fopen(fname, "w"); \
   if (!m_fp) { SUMA_S_Err("Failed to write GTS surface"); }   \
   else { gts_surface_write(s, m_fp); fclose(m_fp); m_fp = NULL; }   \
}
#define GTS_VTK_OUT(fname,s){\
   FILE *m_fp = fopen(fname, "w"); \
   if (!m_fp) { SUMA_S_Err("Failed to write VTK surface"); }   \
   else { gts_surface_write_vtk(s, m_fp); fclose(m_fp); m_fp = NULL; }   \
}

void gts_set_debug_suma(int val);
int  gts_get_debug_suma(void);
void gts_surface_suma (GtsSurface * s,
                        float **NodeListp, int *N_Nodep, int *NodeDimp,
                        int **FaceSetListp, int *N_FaceSetp, int *FaceSetDimp);

GtsSurface* SumaToGts( SUMA_SurfaceObject *SO); /*!< copies suma surface into a gts surface */
void coarsen( GtsSurface* s, int stop); /*!< coarsens with set parameters*/
void refine( GtsSurface* s, int stop); /*!< coarsens with set parameters*/
SUMA_SurfaceObject *SUMA_Mesh_Resample (SUMA_SurfaceObject *SO,
                                        float edge_factor);
SUMA_SurfaceObject *SUMA_Mesh_Resample_nodes(SUMA_SurfaceObject *SO,
                                             float new_N_Nodes);
#endif /* __SUMAGTS_H__ */
