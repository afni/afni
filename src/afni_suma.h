#ifndef _SUMA_HEADER_FILE_
#define _SUMA_HEADER_FILE_

/**************************************************************/
/**** Header for SUMA functions (SUrface Mapping to AFNI?) ****/
/**** Strawman version: 24 Aug 2001 - RWCox                ****/
/**** Woodman         : 01 Mar 2002                        ****/
/**************************************************************/

/*! Port number for talking to AFNI */

#ifndef SUMA_TCP_PORT
#define SUMA_TCP_PORT 53211
#endif

/*--- define types ---*/

/*! Type to store a node in 3D space. */

typedef struct {
  int  id ;         /*!< Node identifier (>= 0) */
  float x ;         /*!< x-coordinate */
  float y ;         /*!< y-coordinate */
  float z ;         /*!< z-coordinate */
} SUMA_ixyz ;

/*! Type to store a triangle (a triple of node identifiers). */

typedef struct {
  float id,jd,kd ;
} SUMA_ijk ;

/*! Type to store a node+color list */

typedef struct {
  int id ;
  unsigned char r,g,b,a ;
} SUMA_irgba ;

/*! Type code for SUMA_surface structs */

#define SUMA_SURFACE_TYPE 53001

/*! A large number */

#ifndef WAY_BIG
#define WAY_BIG 1.e+10
#endif

/*! A surface structure in 3D space:
     - a bunch of SUMA_ixyz's
     - a bunch of SUMA_ijk's linking them together */

typedef struct {
  int type     ;               /*!< == SUMA_SURFACE_TYPE */
  int num_ixyz ;               /*!< Number of nodes */
  int nall_ixyz;               /*!< Number of node malloc-ed */
  int num_ijk  ;               /*!< Number of triangles */
  int nall_ijk ;               /*!< Number of triangles malloc-ed */
  int seq      ;               /*!< If 1, node .id's are sequential */
  int seqbase  ;               /*!< If .id's sequential, is smallest .id */
  int sorted   ;               /*!< If 1, node .id's are sorted */

  SUMA_ixyz *ixyz ;            /*!< Node list: num_ixyz long */
  SUMA_ijk  *ijk  ;            /*!< Triangle list: num_ijk long */

  float xbot ;                 /*!< Smallest x-coordinate in nodes */
  float ybot ;                 /*!< Smallest y-coordinate in nodes */
  float zbot ;                 /*!< Smallest z-coordinate in nodes */
  float xtop ;                 /*!< Largest x-coordinate in nodes */
  float ytop ;                 /*!< Largest y-coordinate in nodes */
  float ztop ;                 /*!< Largest z-coordinate in nodes */

  char idcode[32] ;            /*!< IDCODE string for this structure */
  char idcode_dset[32] ;       /*!< IDCODE string for AFNI dataset */
} SUMA_surface ;

/*! Macro for node count in a SUMA_surface struct */

#define SUMA_NODE_COUNT(su)      ((su)->num_ixyz)

/*! Macro for triangle count in a SUMA_surface struct */

#define SUMA_TRIANGLE_COUNT(su)  ((su)->num_ijk)

/*! Max number nodes allowed in a surface (67.1 million)
    [higher order bits are reserved for other purposes]. */

#define SUMA_MAX_NODES         (1<<26)

#define SUMA_VMAP_LEVMASK(ll)  (ll << 26)       /* for ll=0..7 only! */
#define SUMA_VMAP_UNMASK(v)    ((v) & ((1<<26)-1))
#define SUMA_VMAP_LEVEL(v)     (((v) & (7<<26)) >> 26)
#define SUMA_VMAP_LEVZERO(v)   (((v) & (7<<26)) == 0)

#define SUMA_VMAP_TO_ID(ag,v)  ((ag)->ixyz[SUMA_VMAP_UNMASK(v)])

/*! Typedef for a voxel-node list */

typedef struct {
   int nvox ;       /*!< Number of voxels */
   int *voxijk ;    /*!< Voxel indexes */
   int *numnod ;    /*!< Number of nodes */
   int **nlist ;    /*!< Array of node indexes */

   struct THD_3dim_dataset * dset ;  /*!< Dataset to which this is linked */
} SUMA_vnlist ;

/*--- prototypes ---*/

extern SUMA_surface * SUMA_create_empty_surface(void) ;
extern void SUMA_destroy_surface( SUMA_surface * ) ;

extern void SUMA_add_nodes_ixyz( SUMA_surface *, int,
                                 int *, float *, float *, float *) ;
extern void SUMA_add_node_ixyz( SUMA_surface *, int, float, float, float );

extern void SUMA_add_triangles( SUMA_surface *, int, int *, int *, int * ) ;
extern void SUMA_add_triangle( SUMA_surface *, int, int, int ) ;

extern void SUMA_truncate_memory ( SUMA_surface * ) ;
extern void SUMA_ixyzsort_surface( SUMA_surface * ) ;
extern int  SUMA_find_node_id    ( SUMA_surface *, int ) ;

extern SUMA_surface * SUMA_read_surface( char * , struct THD_3dim_dataset * ) ;

extern void SUMA_get_surfname( struct THD_3dim_dataset * ) ;

extern int * SUMA_map_vol_to_surf( SUMA_surface * ,
                                   int nx    , int ny    , int nz    ,
                                   float xoff, float yoff, float zoff,
                                   float dx  , float dy  , float dz   ) ;

extern int * SUMA_map_dset_to_surf( SUMA_surface *, struct THD_3dim_dataset *);

extern SUMA_vnlist *SUMA_make_vnlist(SUMA_surface *,struct THD_3dim_dataset *);

extern void SUMA_load  ( struct THD_3dim_dataset * ) ;
extern void SUMA_unload( struct THD_3dim_dataset * ) ;

extern THD_fvec3 THD_dicomm_to_surefit( struct THD_3dim_dataset *, THD_fvec3 ) ;
extern THD_fvec3 THD_surefit_to_dicomm( struct THD_3dim_dataset *, THD_fvec3 ) ;
extern void SUMA_import_surefit( SUMA_surface *, char *, struct THD_3dim_dataset * );

#endif /* _SUMA_HEADER_FILE */
