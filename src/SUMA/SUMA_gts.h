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
#include "gts.h"
#include "SUMA_suma.h"

 
GtsSurface* SumaToGts( SUMA_SurfaceObject *SO); /*!< copies suma surface into a gts surface */
SUMA_SurfaceObject* GtsToSuma( GtsSurface *gs); /*!< copies gts surface into a suma surface */
void MakeNodeList_foreach_vertex ( GtsVertex* v, gpointer* data);
void MakeFaceList_foreach_face ( GtsFace* f, gpointer* data);
void coarsen( GtsSurface* s, int stop); /*!< coarsens with set parameters*/
void refine( GtsSurface* s, int stop); /*!< coarsens with set parameters*/

#endif /* __SUMAGTS_H__ */
