#ifndef _3DSURF2VOL_H_
#define _3DSURF2VOL_H_

#define PROG_NAME		"3dSurfMaskDump"

#define SMD_USE_LONG      	  1
#define SMD_USE_SHORT     	  2
#define SMD_USE_VERSION   	  3

#define SMD_DEBUG_MAX_LEV	  4
#define SMD_DEBUG_TEST_NODE	  7

#define SMD_M2_INDEX_VOXEL        0
#define SMD_M2_INDEX_NODE         1

#define SMD_M2_STEPS_DEFAULT      2

/* surface to voxel mapping codes, along with command-line strings */
typedef enum
{
    E_SMAP_INVALID,			/* do not change INVALID or FINAL */
    E_SMAP_MASK, E_SMAP_MIDPT,
    E_SMAP_MASK2,
    E_SMAP_AVE,  E_SMAP_COUNT,
    E_SMAP_MIN,  E_SMAP_MAX,
    E_SMAP_FINAL			/* do not change FINAL */
} smap_nums;


/* user options */
typedef struct
{
    char   * gpar_file;			/* AFNI grid parent filename   */
    char   * out_file;			/* filename for ascii output   */
    char   * spec_file;			/* surface specification file  */
    char   * sv_file;			/* AFNI surface volume dataset */
    char   * cmask_cmd;      		/* 3dcalc style mask command   */
    char   * map_str;			/* how to map surf(s) to dset  */
    int      no_head;			/* do not write output headers */
    int      debug;			/* level of debug output       */
    char   * m2_index_str;		/* node or voxel index type    */
    int      m2_steps;			/* # int steps for mask2 map   */
} opts_t;

typedef struct
{
    int      map;			/* type of mapping from surfs  */
    int      debug;			/* for printing extra output   */
    int      no_head;			/* do not write output headers */
    int      m2_index;			/* node or voxel index type    */
    int      m2_steps;			/* # int steps for mask2 map   */
    byte   * cmask;			/* computed mask               */
} smap_opts_t;

/* computational parameters */
typedef struct
{
    THD_3dim_dataset * gpar;		/* input dataset              */
    THD_fvec3          f3mm_min;	/* numerical min xyz points   */
    THD_fvec3          f3mm_max;	/* numerical max xyz points   */
    FILE             * outfp;		/* out_file FILE stream       */
    byte             * cmask;		/* computed mask              */
    int                ncmask;		/* nvox for cmask             */
    int                ccount;          /* mask size                  */
    int                nvox;		/* gpar nxyz                  */
} param_t;

/* node list structure */
typedef struct
{
    THD_fvec3  * nodes;			/* depth x nnodes in size     */
    int          depth;			/* major axis                 */
    int          nnodes;		/* minor axis                 */
    char      ** labels;		/* #depth labels              */
} node_list_t;


/* ---- function prototypes ---- */

int alloc_node_list   ( smap_opts_t * sopt, node_list_t * N, int nsurf);
int check_datum_type  ( char * datum_str, int default_type );
int check_map_func    ( char * map_str );
int create_node_list  ( smap_opts_t * sopt, node_list_t * N );
int disp_opts_t       ( char * info, opts_t * opts );
int disp_param_t      ( char * info, param_t * p );
int disp_smap_opts_t  ( char * info, smap_opts_t * sopt );
int dump_ave_map      ( smap_opts_t * sopt, param_t * p, node_list_t * N );
int dump_midpt_map    ( smap_opts_t * sopt, param_t * p, node_list_t * N );
int dump_single_map   ( smap_opts_t * sopt, param_t * p, node_list_t * N );
int final_clean_up    ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec,
       			node_list_t * N );
int get_mappable_surfs( SUMA_SurfaceObject ** slist, int how_many, int debug );
int init_options      ( opts_t * opts, int argc, char * argv [] );
int read_surf_files   ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec );
int set_smap_opts     ( opts_t * opts, param_t * p, smap_opts_t * sopt );
int set_outfile       ( opts_t * O, param_t * P );
int smd_map_type      ( char * map_str );
int usage             ( char * prog, int level );
int validate_datasets ( opts_t * opts, param_t * p );
int validate_options  ( opts_t * opts, param_t * p );
int validate_surface  ( opts_t * opts, param_t * p );
int write_output      ( smap_opts_t * sopt, opts_t * opts, param_t * p,
	                node_list_t * N );

int f3mm_out_of_bounds( THD_fvec3 * cp, THD_fvec3 * min, THD_fvec3 * max );
int set_3dmm_bounds   ( THD_3dim_dataset *dset, THD_fvec3 *min, THD_fvec3 *max);
float dist_f3mm       ( THD_fvec3 * p1, THD_fvec3 * p2 );

#endif   /* _3DSURF2VOL_H_ */
