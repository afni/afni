#ifndef _AFNI_SUMA_HEADER_FILE_
#define _AFNI_SUMA_HEADER_FILE_

/**************************************************************/
/**** Header for SUMA functions (SUrface Mapping to AFNI?) ****/
/**** Strawman version: 24 Aug 2001 - RWCox                ****/
/**** Woodman         : 01 Mar 2002                        ****/
/**** Tinman          : 20 Jan 2004                        ****/
/**************************************************************/

#include "vecmat.h"   /* for THD_fvec3 type */

/*! Port number for talking to AFNI */

#ifndef SUMA_TCP_PORT
#define SUMA_TCP_PORT 53211   /* my Zip code in Wisconsin */
#endif

/*---------------------------- define types ----------------------------*/

/*! Type to store a node in 3D space. */

typedef struct {
  int  id ;         /*!< Node identifier (>= 0) */
  float x ;         /*!< x-coordinate */
  float y ;         /*!< y-coordinate */
  float z ;         /*!< z-coordinate */
} SUMA_ixyz ;

/*! NIML rowtype definition for SUMA_ixyz struct */

#define SUMA_ixyz_defn "int,3*float"

/*! Type to store a triangle (a triple of node identifiers). */

typedef struct {
  float id,jd,kd ;
} SUMA_ijk ;

/*! NIML rowtype definition for SUMA_ijk struct */

#define SUMA_ijk_defn "3*int"

/*! Type to store a node+color list */

typedef struct {
  int id ;
  unsigned char r,g,b,a ;
} SUMA_irgba ;

/*! NIML rowtype definition for SUMA_irgba struct */

#define SUMA_irgba_defn "int,4*byte"

/*! Typedef for a voxel-node list */

typedef struct {
   int nvox ;       /*!< Number of voxels stored herein            */
   int *voxijk ;    /*!< [i] = voxel index in dataset, i=0..nvox-1 */
   int *numnod ;    /*!< [i] = number of nodes in voxel #i         */
   int **nlist ;    /*!< [i] = array of node indexes for voxel #i;
                         nnlist[i][j] for j=0..numnod[i]-1         */

   struct THD_3dim_dataset * dset ;  /*!< Dataset to which this is linked */
} SUMA_vnlist ;

/*! Type code for SUMA_surface structs */

#define SUMA_SURFACE_TYPE 53001

/*! A large number */

#ifndef WAY_BIG
#define WAY_BIG 1.e+10
#endif

/*! Typedef for voxel value list (used to store ROIs from SUMA) */

typedef struct {
   int nvox ;       /*!< number of voxels    */
   int   *voxijk ;  /*!< voxel indexes       */
   float *voxval ;  /*!< value at each voxel */
} SUMA_vvlist ;

/*! Macro to free a SUMA_vvlist struct */

#define DESTROY_VVLIST(vv)                          \
 do{ if( vv != NULL ){                              \
       if( vv->voxijk != NULL ) free(vv->voxijk) ;  \
       if( vv->voxval != NULL ) free(vv->voxval) ;  \
       free(vv) ;                                   \
 }} while(0)

/*! A surface structure in 3D space:
     - a bunch of SUMA_ixyz's
     - a bunch of SUMA_ijk's linking them together
     - other miscellaneous and convenient information */

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
  THD_fvec3 *norm ;            /*!< Normals list: num_ixyz long */
  SUMA_ijk  *ijk  ;            /*!< Triangle list: num_ijk long */

  float xbot ;                 /*!< Smallest  x-coordinate in ixyz */
  float ybot ;                 /*!< Smallest  y-coordinate in ixyz */
  float zbot ;                 /*!< Smallest  z-coordinate in ixyz */
  float xtop ;                 /*!< Largest   x-coordinate in ixyz */
  float ytop ;                 /*!< Largest   y-coordinate in ixyz */
  float ztop ;                 /*!< Largest   z-coordinate in ixyz */
  float xcen ;                 /*!< Averagest x-coordinate in ixyz */
  float ycen ;                 /*!< Averagest y-coordinate in ixyz */
  float zcen ;                 /*!< Averagest z-coordinate in ixyz */

  char idcode[32] ;            /*!< IDCODE for this structure */
  /* changed idcode_domaingroup to idcode_ldp            06 Oct 2004 [rickr] */
  char idcode_ldp[32] ;        /*!< IDCODE for surface's local domain parent */
  char idcode_dset[32] ;       /*!< IDCODE for AFNI dataset domain parent */

  char label[64] ;             /*!< Label for user interaction */
  char label_ldp[64] ;         /*!< Label of surface's local domain parent */

  SUMA_vvlist *vv ;            /*!< For ROIs from SUMA */
  SUMA_vnlist *vn ;            /*!< Voxel-to-node mapping, for overlays */
} SUMA_surface ;

/*! Macro for node count in a SUMA_surface struct */

#define SUMA_NODE_COUNT(su)      ((su)->num_ixyz)

/*! Macro for triangle count in a SUMA_surface struct */

#define SUMA_TRIANGLE_COUNT(su)  ((su)->num_ijk)

/*! Max number nodes allowed in a surface (67.1 million)
    [higher order bits are reserved for other purposes]. */

#define SUMA_MAX_NODES         (1<<26)

/** These macros are used in SUMA_map_dset_to_surf()
    to create an easily searched map between dataset
    voxel indexes and surface nodes (currently disabled) **/

#define SUMA_VMAP_LEVMASK(ll)  (ll << 26)       /* for ll=0..7 only! */
#define SUMA_VMAP_UNMASK(v)    ((v) & ((1<<26)-1))
#define SUMA_VMAP_LEVEL(v)     (((v) & (7<<26)) >> 26)
#define SUMA_VMAP_LEVZERO(v)   (((v) & (7<<26)) == 0)

#define SUMA_VMAP_TO_ID(ag,v)  ((ag)->ixyz[SUMA_VMAP_UNMASK(v)])

/*! For the SUMA_surfacegroup typedef below. */

#define SUMA_SURFACEGROUP_TYPE 53003

/*! A typedef for a struct that contains a bunch of associated surfaces. */

typedef struct {
  int type ;                   /*!< == SUMA_SURFACEGROUP_TYPE */
  int num_surf ;               /*!< number of surfaces herein */
  SUMA_surface **surf ;        /*!< array of pointers to surface */
  char idcode[32] ;            /*!< IDCODE for this group of surfaces */
} SUMA_surfacegroup ;

/*------------------------ function prototypes -----------------------*/

extern SUMA_surface * SUMA_create_empty_surface(void) ;
extern void SUMA_destroy_surface( SUMA_surface * ) ;

extern void SUMA_add_nodes_ixyz( SUMA_surface *, int,
                                 int *, float *, float *, float *) ;
extern void SUMA_add_node_ixyz( SUMA_surface *, int, float, float, float );
extern int  SUMA_add_norms_xyz( SUMA_surface *, int, float *, float *, float *);

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

extern SUMA_vnlist * SUMA_make_vnlist(SUMA_surface *,struct THD_3dim_dataset *);

extern int AFNI_find_closest_node( int , SUMA_ixyz *,    /* 20 Feb 2003 */
                                   float,float,float ,
                                   float,float , float,float , float,float ) ;
extern void AFNI_get_xhair_node( void *, int *, int * ) ;

extern void SUMA_load  ( struct THD_3dim_dataset * ) ;
extern void SUMA_unload( struct THD_3dim_dataset * ) ;

extern THD_fvec3 THD_dicomm_to_surefit( struct THD_3dim_dataset *, THD_fvec3 ) ;
extern THD_fvec3 THD_surefit_to_dicomm( struct THD_3dim_dataset *, THD_fvec3 ) ;
extern void SUMA_import_surefit( SUMA_surface *, char *, struct THD_3dim_dataset * );

extern void SUMA_destroy_vnlist( SUMA_vnlist *vnlist ) ;

#endif /* _SUMA_HEADER_FILE */
