#ifndef _AGNI_HEADER_FILE_
#define _AGNI_HEADER_FILE_

/********************************************************************/
/**** Header for AGNI functions (AFNI Geometry Nodal Interface?) ****/
/**** Strawman version: 24 Aug 2001 - RWCox                      ****/
/**** Woodman         : 29 Aug 2001                              ****/
/********************************************************************/

/*--- define types ---*/

typedef struct {      /* a location in space */
  int id ;
  float x,y,z ;
} AGNI_nod ;

#define AGNI_GOOD_NOD(an)   ((an).x < WAY_BIG)
#define AGNI_badify_nod(an) ((an).x = WAY_BIG)

typedef struct {      /* a list of nod indexes */
  float i,j,k ;
} AGNI_tri ;

#define AGNI_GOOD_TRI(at)   ((at).i >= 0)
#define AGNI_badify_tri(at) ((at).i = -1)

/* the central structure:
   a bunch of nods,
   a bunch of triangles linking them together,
   colors for the nods,
   et cetera                                  */

typedef struct AGNI_surface {
  int num_nod , num_tri , nall_nod , nall_tri ;
  int seq , seqbase , sorted ;

  AGNI_nod *nod  ;  /* num_nod long */
  AGNI_tri *tri  ;  /* num_tri long */

  float xbot,ybot,zbot , xtop,ytop,ztop ;

  struct AGNI_surface *sparent ; /* derived from a parent surface? */

} AGNI_surface ;

#define AGNI_nodcount(ag)  ((ag)->num_nod)
#define AGNI_tricount(ag)  ((ag)->num_tri)

/*--- prototypes ---*/

extern AGNI_surface * AGNI_create_empty_surface(void) ;
extern void AGNI_destroy_surface( AGNI_surface * ) ;

extern void AGNI_add_nodes_ixyz( AGNI_surface *, int,
                                 int *, float *, float *, float *) ;
extern void AGNI_add_node_ixyz( AGNI_surface *, int, float, float, float );

extern void AGNI_add_triangles( AGNI_surface *, int, int *, int *, int * ) ;
extern void AGNI_add_triangle( AGNI_surface *, int, int, int ) ;

extern void AGNI_truncate_memory ( AGNI_surface * ) ;
extern void AGNI_nodesort_surface( AGNI_surface * ) ;
extern int  AGNI_find_node_id    ( AGNI_surface *, int ) ;

extern AGNI_surface * AGNI_read_surface( char * ) ;

extern int * AGNI_map_vol_to_surf( AGNI_surface * ,
                                   int nx    , int ny    , int nz    ,
                                   float xoff, float yoff, float zoff,
                                   float dx  , float dy  , float dz   ) ;

extern int * AGNI_map_dset_to_surf( AGNI_surface * , struct THD_3dim_dataset * ) ;

extern void AGNI_get_sname( struct THD_3dim_dataset * ) ;
extern void AGNI_load     ( struct THD_3dim_dataset * ) ;
extern void AGNI_unload   ( struct THD_3dim_dataset * ) ;

#endif /* _AGNI_HEADER_FILE */
