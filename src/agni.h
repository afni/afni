#ifndef _AGNI_HEADER_
#define _AGNI_HEADER_

/********************************************************************/
/**** Header for AGNI functions (AFNI Geometry Nodal Interface?) ****/
/**** Strawman: 24 Aug 2001 - RWCox                              ****/
/********************************************************************/

#include "mrilib.h"   /* RWCox library for everything */
#include "sipp.h"     /* graphics rendering library (temporary?) */

/*--- define types ---*/

typedef struct {      /* a location in space */
  float x,y,z ;
} AGNI_nod ;

#define AGNI_GOOD_NOD(an)   ((an).x < WAY_BIG)
#define AGNI_badify_nod(an) ((an).x = WAY_BIG)

typedef struct {      /* a list of nod indexes */
  float i,j,k ;
} AGNI_tri ;

typedef struct {      /* list of nod indexes in boxes */
  int mx,my,mz ;
  float xb,yb,zb , dx,dy,dz , eps ;
  int ****nlist ;
} AGNI_spatial_index ;

#define AGNI_GOOD_TRI(at)   ((at).i >= 0)
#define AGNI_badify_tri(at) ((at).i = -1)

/* the central structure:
   a bunch of nods,
   a bunch of triangles linking them together,
   colors for the nods,
   et cetera                                  */

typedef struct AGNI_surface {
  int num_nod , num_tri , nall_nod , nall_tri ;
  AGNI_nod *nod  ;  /* num_nod long */
  AGNI_tri *tri  ;  /* num_tri long */
  rgba     *clr  ;  /* num_nod long */

  AGNI_spatial_index *spindex ; /* for fast spatial lookup */

  struct AGNI_surface *sparent ; /* derived from a parent surface? */

  SIPPSurface * sippsurf ;       /* for rendering */

} AGNI_surface ;

#define AGNI_nodcount(ag)  ((ag)->num_nod)
#define AGNI_tricount(ag)  ((ag)->num_tri)

/*--- prototypes ---*/

extern void AGNI_set_default_rgba( rgba ) ;
extern AGNI_surface * AGNI_create_empty_surface(void) ;
extern void AGNI_destroy_surface( AGNI_surface * ) ;

extern void AGNI_add_nodes_xyzcol( AGNI_surface *, int,
                                   float *, float *, float *, rgba * ) ;
extern void AGNI_add_nodes_xyz( AGNI_surface *, int,
                                float *, float *, float *) ;
extern void AGNI_add_node_xyzcol( AGNI_surface *, float ,float ,float ,rgba );
extern void AGNI_add_node_xyz( AGNI_surface *, float, float, float );

extern void AGNI_truncate_memory( AGNI_surface * ) ;
extern void AGNI_add_triangles( AGNI_surface *, int, int *, int *, int * ) ;
extern void AGNI_add_triangle( AGNI_surface *, int, int, int ) ;

extern void AGNI_create_spatial_index( AGNI_surface * ) ;
extern void AGNI_destroy_spatial_index( AGNI_spatial_index * ) ;


#endif /* _AGNI_HEADER_ */
