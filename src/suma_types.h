#ifndef _SUMA_TYPES_HEADER_FILE_
#define _SUMA_TYPES_HEADER_FILE_

/*! Port number for talking to AFNI */

#define SUMA_TCP_PORT 53211

/************************************************/
/**** Header for SUMA types (SUrface MApper) ****/
/**** Strawman version: 27 Feb 2001 - RWCox  ****/
/************************************************/

/*--- define types ---*/

/*! Type to store a node in 3D space. */

typedef struct {
  int  id ;         /*!< Node identifier (>= 0) */
  float x ;         /*!< x-coordinate */
  float y ;         /*!< y-coordinate */
  float z ;         /*!< z-coordinate */
} SUMA_ixyz ;

/*! Type to store a triangle (a list of node indexes). */

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

/*! A surface structure in 3D space:
     - a bunch of SUMA_ixyz's
     - a bunch of SUMA_ijk's linking them together */

typedef struct {
  int type     ;               /*!< == SUMA_SURFACE_TYPE */
  int num_ixyz ;               /*!< Number of nodes */
  int num_ijk  ;               /*!< Number of triangles */
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

  char idc[32] ;               /*!< UNIQ_idcode() for this structure */
} SUMA_surface ;

/*! Macro for node count in a SUMA_surface struct */

#define SUMA_NODE_COUNT(su)      ((su)->num_ixyz)

/*! Macro for triangle count in a SUMA_surface struct */

#define SUMA_TRIANGLE_COUNT(su)  ((su)->num_ijk)

#endif /* _SUMA_TYPES_HEADER_FILE */
