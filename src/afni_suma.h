#ifndef _SUMA_HEADER_FILE_
#define _SUMA_HEADER_FILE_

/********************************************************************/
/**** Header for SUMA functions (AFNI Geometry Nodal Interface?) ****/
/**** Strawman version: 24 Aug 2001 - RWCox                      ****/
/**** Woodman         : 29 Aug 2001                              ****/
/********************************************************************/

/*--- define types ---*/

typedef struct {      /* a location in space */
  int id ;
  float x,y,z ;
} SUMA_nod ;

#define SUMA_GOOD_NOD(an)   ((an).x < WAY_BIG)
#define SUMA_badify_nod(an) ((an).x = WAY_BIG)

typedef struct {      /* a list of nod indexes */
  float i,j,k ;
} SUMA_tri ;

#define SUMA_GOOD_TRI(at)   ((at).i >= 0)
#define SUMA_badify_tri(at) ((at).i = -1)

/* the central structure:
   a bunch of nods,
   a bunch of triangles linking them together,
   colors for the nods,
   et cetera                                  */

typedef struct SUMA_surface {
  int num_nod , num_tri , nall_nod , nall_tri ;
  int seq , seqbase , sorted ;

  SUMA_nod *nod  ;  /* num_nod long */
  SUMA_tri *tri  ;  /* num_tri long */

  float xbot,ybot,zbot , xtop,ytop,ztop ;

  struct SUMA_surface *sparent ; /* derived from a parent surface? */

} SUMA_surface ;

#define SUMA_nodcount(ag)  ((ag)->num_nod)
#define SUMA_tricount(ag)  ((ag)->num_tri)

#define SUMA_MAX_NODES         (1<<26)

#define SUMA_VMAP_LEVMASK(ll)  (ll << 26)       /* for ll=0..7 only! */
#define SUMA_VMAP_UNMASK(v)    ((v) & ((1<<26)-1))
#define SUMA_VMAP_LEVEL(v)     (((v) & (7<<26)) >> 26)
#define SUMA_VMAP_LEVZERO(v)   (((v) & (7<<26)) == 0)

#define SUMA_VMAP_TO_ID(ag,v)  ((ag)->nod[SUMA_VMAP_UNMASK(v)])

/*--- prototypes ---*/

extern SUMA_surface * SUMA_create_empty_surface(void) ;
extern void SUMA_destroy_surface( SUMA_surface * ) ;

extern void SUMA_add_nodes_ixyz( SUMA_surface *, int,
                                 int *, float *, float *, float *) ;
extern void SUMA_add_node_ixyz( SUMA_surface *, int, float, float, float );

extern void SUMA_add_triangles( SUMA_surface *, int, int *, int *, int * ) ;
extern void SUMA_add_triangle( SUMA_surface *, int, int, int ) ;

extern void SUMA_truncate_memory ( SUMA_surface * ) ;
extern void SUMA_nodesort_surface( SUMA_surface * ) ;
extern int  SUMA_find_node_id    ( SUMA_surface *, int ) ;

extern SUMA_surface * SUMA_read_surface( char * , struct THD_3dim_dataset * ) ;

extern int * SUMA_map_vol_to_surf( SUMA_surface * ,
                                   int nx    , int ny    , int nz    ,
                                   float xoff, float yoff, float zoff,
                                   float dx  , float dy  , float dz   ) ;

extern int * SUMA_map_dset_to_surf( SUMA_surface * , struct THD_3dim_dataset * ) ;

extern void SUMA_get_sname( struct THD_3dim_dataset * ) ;
extern void SUMA_load     ( struct THD_3dim_dataset * ) ;
extern void SUMA_unload   ( struct THD_3dim_dataset * ) ;

extern THD_fvec3 THD_dicomm_to_surefit( struct THD_3dim_dataset *, THD_fvec3 ) ;
extern THD_fvec3 THD_surefit_to_dicomm( struct THD_3dim_dataset *, THD_fvec3 ) ;
extern void SUMA_import_surefit( SUMA_surface *, char *, struct THD_3dim_dataset * );

#endif /* _SUMA_HEADER_FILE */
