/***************************************************************************
                          sumagts.c  -  description
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
#ifdef HAVE_CONFIG_H
#include "gts/config.h"
#endif

#include "SUMA_suma.h"
#include "SUMA_gts.h"

extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  

#if 0
/* Normally, functions in SUMA_gts_insert.c would be here */
#endif 

GtsSurface* SumaToGts( SUMA_SurfaceObject *SO)
{
	static char FuncName[]={"SumaToGts"};
	GtsSurface* s = NULL;
	GtsVertex ** vertices = NULL;
	GtsEdge ** edges = NULL;
	int i = 0; /*counters */
	int n = 0;
	int *MapToGts = NULL; /*used to map EL vector to edge vector*/
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
	if (!SO->EL) {
      SUMA_SL_Err("Null Edgeist!");
      SUMA_RETURN(s);
   }
   
   SUMA_LH("In with ");
   if (LocalHead) SUMA_Print_Surface_Object(SO, stderr);
   
   s = gts_surface_new( gts_surface_class (),
									 gts_face_class (),
									 gts_edge_class (),
									 gts_vertex_class ());
	vertices = (GtsVertex **)g_malloc (SO->N_Node * sizeof (GtsVertex *));
	n = 0;
   for ( i=0; i< SO->N_Node*3; i+=3)
	{
		vertices[n] = gts_vertex_new( s->vertex_class,
			(gdouble)SO->NodeList[i],
			(gdouble)SO->NodeList[i+1],
			(gdouble)SO->NodeList[i+2]);
      if (LocalHead) 
         fprintf(SUMA_STDERR, "Added vertex (%d/%d)%f %f %f\n"
                                          "Stored gts (fails for some reason, but surface is OK)  %f %f %f\n"
                                          , n, SO->N_Node-1, SO->NodeList[i], SO->NodeList[i+1], SO->NodeList[i+2]
                                          , (float)(vertices[n]->p.x), (float)(vertices[n]->p.y), (float)(vertices[n]->p.z));  
	   ++n;
   }
   edges = (GtsEdge**)g_malloc (SO->EL->N_Distinct_Edges * sizeof (GtsEdge*));
	n = 0;
	MapToGts = (int *)g_malloc ( SO->EL->N_EL * sizeof(int));
   for ( i=0; i< SO->EL->N_EL; i++)
	{
		if (SO->EL->ELps[i][2] > 0)
		{ /* a unique edge*/
			GtsVertex* v1 = vertices[SO->EL->EL[i][0]];
			GtsVertex* v2 = vertices[SO->EL->EL[i][1]];
			if (SO->EL->ELps[i][0] == 1) {
				edges[n++] = gts_edge_new ( s->edge_class, v2, v1 );
			   if (LocalHead) fprintf(SUMA_STDERR,"Added edge of vertices: %d %d \n", SO->EL->EL[i][1], SO->EL->EL[i][0]);
         } else {
            edges[n++] = gts_edge_new ( s->edge_class, v1, v2 );
				if (LocalHead) fprintf(SUMA_STDERR,"Added edge of nodes: %d %d \n", SO->EL->EL[i][0], SO->EL->EL[i][1]);
         }
		}
      MapToGts[i] = n-1; /* n-1 bc n was just incremented */
		if (LocalHead) fprintf(SUMA_STDERR,"SUMA edge %d is mapped to GTS edge %d\n", i, n-1);
	}

	if (n != SO->EL->N_Distinct_Edges)
	{
		fprintf(SUMA_STDERR, "distinct edges didn't equal N_Distinct_Edges");
		exit(1);
	}
	/* this loop isn't working, stupid tri_limb
	for ( i=0; i< SO->N_FaceSet; i++)
	{
		gts_surface_add_face(s,
		   gts_face_new ( s->face_class,
			edges[MapToGts[SO->EL->Tri_limb[i][0]]],
			edges[MapToGts[SO->EL->Tri_limb[i][1]]],
			edges[MapToGts[SO->EL->Tri_limb[i][2]]]));
	}
	*/
	for ( i=0; i< SO->N_FaceSet*3; i+=3)
	{
		int n1 = SO->FaceSetList[i];
		int n2 = SO->FaceSetList[i+1];
		int n3 = SO->FaceSetList[i+2];
		GtsEdge* e1 = edges[MapToGts[SUMA_FindEdge( SO->EL, n1, n2)]];
		GtsEdge* e2 = edges[MapToGts[SUMA_FindEdge( SO->EL, n2, n3)]];
		GtsEdge* e3 = edges[MapToGts[SUMA_FindEdge( SO->EL, n3, n1)]];
		gts_surface_add_face(s,
		   gts_face_new ( s->face_class, e1, e2, e3));
      if (LocalHead) fprintf(SUMA_STDERR, "Added face of SUMA edges: %d %d %d\n"
                                          "              MapToGTS  : %d %d %d\n", 
                                          SUMA_FindEdge( SO->EL, n1, n2), SUMA_FindEdge( SO->EL, n2, n3), SUMA_FindEdge( SO->EL, n3, n1),
                                          MapToGts[SUMA_FindEdge( SO->EL, n1, n2)], MapToGts[SUMA_FindEdge( SO->EL, n2, n3)], MapToGts[SUMA_FindEdge( SO->EL, n3, n1)]);
	}
   
	g_free (vertices);
	g_free (edges);
	g_free (MapToGts);
	SUMA_RETURN( s );
}


#if 0
/* These functions fail on OSX, see function gts_surface_suma and comments preceding it */ 
SUMA_SurfaceObject* GtsToSuma( GtsSurface *gs)
{
	static char FuncName[]={"GtsToSuma"};
   GHashTable *hash = g_hash_table_new(NULL,NULL);
	SUMA_SurfaceObject* SO = NULL; 
	int i = 0;
	gpointer data1[3];
   
   SUMA_ENTRY;
   
   GTS_OUT("fails.surf", gs);
   
   SO = (SUMA_SurfaceObject *)SUMA_malloc(sizeof(SUMA_SurfaceObject));
	SO->N_Node = gts_surface_vertex_number(gs);
	SO->NodeList = (float*) SUMA_calloc(SO->N_Node * 3, sizeof(float));
	SO->N_FaceSet = gts_surface_face_number(gs);
	SO->FaceSetList = (int*) SUMA_calloc(SO->N_FaceSet * 3, sizeof(int));

	/*
	foreach vertex
		get xyz,
		make hash key=pvertex value=integer
		make NodeList from integer and xyz
	foreach face
		get 3 vertexes
		find their integers from hash
		make FaceSetList from integers
	*/
	data1[0] = hash;
	data1[1] = SO;
	data1[2] = &i;
	i = 0;
	gts_surface_foreach_vertex( gs, (GtsFunc) MakeNodeList_foreach_vertex, data1);
	i = 0;
	gts_surface_foreach_face( gs, (GtsFunc) MakeFaceList_foreach_face, data1);
	g_hash_table_destroy(hash);
	SUMA_RETURN(SO);
}


void MakeNodeList_foreach_vertex ( GtsPoint *p, gpointer* data)
{
	GHashTable *hash = data[0];
	SUMA_SurfaceObject* SO = data[1];
	int* i = data[2];
	fprintf(SUMA_STDERR,"this %f %f %f\n", p->x, p->y, p->z);
   SO->NodeList[(*i)*3] = p->x;
	SO->NodeList[(*i)*3+1] = p->y;
	SO->NodeList[(*i)*3+2] = p->z;
	if (*i >= SO->N_Node || *i < 0)
	{
		fprintf(SUMA_STDERR, "node %i out of range", *i);
		exit(1);
	}
	
	if (g_hash_table_lookup(hash, p) == NULL)
		g_hash_table_insert(hash, p, GINT_TO_POINTER(*i));
	else
	{
		fprintf(SUMA_STDERR, "something already in hash table??");
		exit(1);
	}
	(*i)++;
}

void MakeFaceList_foreach_face ( GtsFace* f, gpointer* data)
{
	GHashTable *hash = data[0];
	SUMA_SurfaceObject* SO = data[1];
	int* i = data[2];
	GtsVertex *v1,*v2,*v3;
	gpointer presult = NULL;
	int iresult = 0;
	gpointer temp = NULL;
	gts_triangle_vertices(&(f->triangle), &v1, &v2, &v3);
	if ((*i) >= SO->N_FaceSet)
	{
		fprintf(SUMA_STDERR, "counter %i passed N_FaceSet %i",(*i),SO->N_FaceSet);
		exit(1);
	}

	if (!g_hash_table_lookup_extended(hash, v1, temp, &presult))
	{
		fprintf(SUMA_STDERR, "hash table lookup failed");
		exit(1);
	}
	iresult = GPOINTER_TO_INT (presult);
	if (iresult >= SO->N_Node || iresult < 0)
	{
		fprintf(SUMA_STDERR, "node %i out of range", iresult);
		exit(1);
	}
	SO->FaceSetList[(*i)*3] = iresult;
	
	if (!g_hash_table_lookup_extended(hash, v2, temp, &presult))
	{
		fprintf(SUMA_STDERR, "hash table lookup failed");
		exit(1);
	}
	iresult = GPOINTER_TO_INT (presult);
	if (iresult >= SO->N_Node || iresult < 0)
	{
		fprintf(SUMA_STDERR, "node %i out of range", iresult);
		exit(1);
	}
	SO->FaceSetList[(*i)*3+1] = iresult;
	
	if (!g_hash_table_lookup_extended(hash, v3, temp, &presult))
	{
		fprintf(SUMA_STDERR, "hash table lookup failed");
		exit(1);
	}
	iresult = GPOINTER_TO_INT (presult);
	if (iresult >= SO->N_Node || iresult < 0)
	{
		fprintf(SUMA_STDERR, "node %i out of range", iresult);
		exit(1);
	}
	SO->FaceSetList[(*i)*3+2] = iresult;
	(*i)++;
}
#endif

void coarsen( GtsSurface* s, int stop)
{
   GtsVolumeOptimizedParams params = { 0.5, 0.5, 0. };
   gdouble fold = PI/180.;
   gts_surface_coarsen (s,
      (GtsKeyFunc) gts_volume_optimized_cost,
      &params,
      (GtsCoarsenFunc) gts_volume_optimized_vertex,
      &params,
      (GtsStopFunc) gts_coarsen_stop_number,
      &stop,
      fold);
}

gboolean stop_number (gdouble cost, guint number, guint * max)
{
  if (number > *max)
    return TRUE;
  return FALSE;
}

void refine( GtsSurface* s, int stop)
{
   gts_surface_refine(s, NULL, NULL, NULL, NULL,
      (GtsStopFunc)stop_number, &stop);
}

/*!
   \brief resample a mesh so that the resultant surface has edge_factor as many edges as the original one
*/
SUMA_SurfaceObject *SUMA_Mesh_Resample (SUMA_SurfaceObject *SO, float edge_factor)
{
   static char FuncName[]={"SUMA_Mesh_Resample"};
   SUMA_SurfaceObject *S2=NULL;
   GtsSurface* s  = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SO) {
      SUMA_SL_Err("NULL SO");
      SUMA_RETURN(S2);
   }
   if (!SO->EL) {
      SUMA_S_Warn("NULL Edge List, computing it");
      if (!SUMA_SurfaceMetrics(SO, "EdgeList", NULL)) {
         SUMA_SL_Err("Failed to create EdgeList");
         SUMA_RETURN(S2);
      }
   }
   /* create a GTS surface */
   s  = SumaToGts(SO);
   if (!s) {
      SUMA_S_Err("Failed to change SO to GTS surface");
      SUMA_RETURN(S2);
   }
   if (LocalHead) { /* see if surface was created well in GTS format */
      SUMA_S_Note("Writing initial surface in GTS form");
      GTS_OUT("gtsout.surf",s);
      GTS_VTK_OUT("vtkout.surf",s);
   }

   /* resample */
   if (1) {
      SUMA_S_Note("Changing mesh density\n");
      if (edge_factor < 1)
         coarsen(s, SO->EL->N_Distinct_Edges * edge_factor);
      else
         refine(s, SO->EL->N_Distinct_Edges * edge_factor);
   } else {
      SUMA_S_Note("Leaving surface untouched\n");
   }
   if (!s) {
      SUMA_SL_Err("Failed to refine");
      SUMA_RETURN(S2);
   }
  
   /* change the surface back to SUMA */
   S2 = SUMA_Alloc_SurfObject_Struct(1);
   gts_surface_suma (s, 
                     &(S2->NodeList), &(S2->N_Node), &(S2->NodeDim), 
                           &(S2->FaceSetList), &(S2->N_FaceSet), &(S2->FaceSetDim));
   
   gts_object_destroy((GtsObject*)s); s = NULL;

   SUMA_RETURN(S2);
}
