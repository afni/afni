#ifndef _3DSURF2VOL_H_
#define _3DSURF2VOL_H_

#define PROG_NAME		"3dSurf2Vol"

#define S2V_USE_LONG      	  1
#define S2V_USE_SHORT     	  2
#define S2V_USE_VERSION   	  3

#define S2V_DEBUG_MAX_LEV	  4
#define S2V_DEBUG_TEST_NODE	  7

/* surface to voxel mapping codes, along with command-line strings */
typedef enum
{
    S2V_MAP_INVALID = -1,
    S2V_MAP_NONE,			/* do not change INVALID or NONE */
    S2V_MAP_MASK, S2V_MAP_MASK2,
    S2V_MAP_AVE,  S2V_MAP_COUNT,
    S2V_MAP_MIN,  S2V_MAP_MAX,
    S2V_MAP_FINAL			/* do not change FINAL */
} s2v_map_num;


/* user options */
typedef struct
{
    char   * gpar_file;			/* AFNI grid parent filename   */
    char   * oset_file;			/* filename for ascii output   */
    char   * spec_file;			/* surface specification file  */
    char   * sv_file;			/* AFNI surface volume dataset */
    char   * cmask_cmd;      		/* 3dcalc style mask command   */
    char   * map_str;			/* how to map surf(s) to dset  */
    char   * datum_str;			/* data type of output dset    */
    int      noscale;			/* no scale of integer output */
    int      debug;			/* level of debug output       */
    int      no_headers;		/* do not write output headers */
    int      m2_steps;			/* # int steps for mask2 map   */
} opts_t;

typedef struct
{
    int      map;			/* type of mapping from surfs */
    int      datum;			/* data type of output dset   */
    int      noscale;			/* no scale of integer output */
    int      debug;			/* for printing extra output  */
    byte   * cmask;			/* computed mask              */
    int      m2_steps;			/* # int steps for mask2 map  */
} s2v_opts_t;

/* computational parameters */
typedef struct
{
    THD_3dim_dataset * gpar;		/* input dataset              */
    THD_3dim_dataset * oset;		/* output dataset             */
    int                nvox;		/* gpar nxyz                  */
    byte             * cmask;		/* computed mask              */
    int                ncmask;		/* nvox for cmask             */
    int                ccount;          /* mask size                  */
} param_t;

/* node list structure */
typedef struct
{
    THD_fvec3 * nodes;			/* depth x nnodes in size     */
    int         depth;			/* major axis                 */
    int         nnodes;			/* minor axis                 */
} node_list_t;


/* ---- function prototypes ---- */

/* library protos - rcr - move to separate file */
int s2v_fill_mask     ( node_list_t * N, THD_3dim_dataset * gpar,
	  		float * fdata, byte * mask, s2v_opts_t * sopt );
int s2v_fill_mask2    ( node_list_t * N, THD_3dim_dataset * gpar,
	  		float * fdata, byte * mask, s2v_opts_t * sopt );
int s2v_map_type      ( char * map_str );

THD_3dim_dataset * s2v_nodes2volume( node_list_t * N, THD_3dim_dataset * gpar,
				     byte * cmask, s2v_opts_t * sopt );



int alloc_node_list   ( s2v_opts_t * sopt, node_list_t * N, int nsurf);
int check_datum_type  ( char * datum_str, int default_type );
int check_map_func    ( char * map_str );
int create_node_list  ( s2v_opts_t * sopt, node_list_t * N );
int disp_opts_t       ( char * info, opts_t * opts );
int disp_param_t      ( char * info, param_t * p );
int disp_s2v_opts_t   ( char * info, s2v_opts_t * sopt );
int final_clean_up    ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec,
       			node_list_t * N );
int get_mappable_surfs( SUMA_SurfaceObject ** slist, int how_many, int debug );
int init_options      ( opts_t * opts, int argc, char * argv [] );
int read_surf_files   ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec );
int set_map_opts      ( opts_t * opts, param_t * p, s2v_opts_t * sopt );
int usage             ( char * prog, int level );
int validate_datasets ( opts_t * opts, param_t * p );
int validate_options  ( opts_t * opts, param_t * p );
int validate_surface  ( opts_t * opts, param_t * p );
int write_output      ( s2v_opts_t * sopt, opts_t * opts, param_t * p,
	                node_list_t * N, int argc, char * argv[] );


#endif   /* _3DSURF2VOL_H_ */
