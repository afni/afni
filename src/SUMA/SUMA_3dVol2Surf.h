#ifndef _3dVOL2SURF_H_
#define _3dVOL2SURF_H_

#define PROG_NAME		"3dVol2Surf"

#define V2S_USE_LONG      	  1
#define V2S_USE_SHORT     	  2
#define V2S_USE_HIST     	  3
#define V2S_USE_VERSION   	  4

#define V2S_DEBUG_MAX_LEV	  5
#define V2S_DEBUG_TEST_NODE	  7

#define V2S_MAX_SURFS             2

#define V2S_M2_INDEX_VOXEL        0
#define V2S_M2_INDEX_NODE         1

#define V2S_M2_STEPS_DEFAULT      2

#define V2S_EPSILON		  0.0001

#define CHECK_NULL_STR(str) ( str ? str : "(NULL)" )

/* surface to voxel mapping codes, along with command-line strings */
typedef enum
{
    E_SMAP_INVALID,			/* do not change INVALID or FINAL */
    E_SMAP_MASK,    E_SMAP_MIDPT,
    E_SMAP_MASK2,
    E_SMAP_AVE,     E_SMAP_COUNT,
    E_SMAP_MIN,     E_SMAP_MAX,
    E_SMAP_MAX_ABS, E_SMAP_SEG_VALS,
    /* sorted ones: */
    E_SMAP_MEDIAN,  E_SMAP_MODE,
    E_SMAP_FINAL			/* do not change FINAL */
} smap_nums;

typedef struct
{
    int   show;
    int   index;
    float value;
} oob_t;

typedef struct
{
    int     nused;
    int     nalloc;
    float * list;
} float_list;


/* user options */
typedef struct
{
    char   * gpar_file;			/* AFNI grid parent filename   */
    char   * out_file;			/* filename for ascii output   */
    char   * spec_file;			/* surface specification file  */
    char   * sv_file;			/* AFNI surface volume dataset */
    char   * cmask_cmd;      		/* 3dcalc style mask command   */
    char   * map_str;			/* how to map surf(s) to dset  */
    char   * snames[V2S_MAX_SURFS];	/* list of surfaces to use     */
    int      no_head;			/* do not write output headers */
    int      use_norms;			/* use normals for segments    */
    float    norm_len;			/* signed length of normal     */
    int      keep_norm_dir;		/* no directional check        */
    int      debug;			/* level of debug output       */
    int      dnode;			/* node watched for debug      */
    char   * f_index_str;		/* node or voxel index type    */
    int      f_steps;			/* # int steps for mask2 map   */
    float    f_p1_fr;			/* fractional dist: add to p1  */
    float    f_pn_fr;			/* fractional dist: add to pn  */
    float    f_p1_mm;			/* mm distance to add to p1    */
    float    f_pn_mm;			/* mm distance to add to pn    */
    oob_t    oob;			/* display info for oob nodes  */
    oob_t    oom;			/* display info for oom nodes  */
} opts_t;

typedef struct
{
    int    map;				/* type of mapping from surfs  */
    int    debug;			/* for printing extra output   */
    int    dnode;			/* node watched for debug      */
    int    no_head;			/* do not write output headers */
    int    use_norms;			/* use normals for segments    */
    float  norm_len;			/* signed length of normal     */
    int    keep_norm_dir;		/* no directional check        */
    int    f_index;			/* node or voxel index type    */
    int    f_steps;			/* # int steps for mask2 map   */
    float  f_p1_fr;			/* fractional dist: add to p1  */
    float  f_pn_fr;			/* fractional dist: add to pn  */
    float  f_p1_mm;			/* mm distance to add to p1    */
    float  f_pn_mm;			/* mm distance to add to pn    */
    byte * cmask;			/* computed mask               */
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
    oob_t              oob;		/* display info for oob nodes  */
    oob_t              oom;		/* display info for oom nodes  */
} param_t;

/* node list structure */
typedef struct
{
    THD_fvec3  * nodes;			/* depth x nnodes in size     */
    int          depth;			/* major axis                 */
    int          nnodes;		/* minor axis                 */
    float        center[3];		/* centroid of first surface  */
    char      ** labels;		/* #depth labels              */
    float      * norms[V2S_MAX_SURFS];  /* pointers to normals        */
} node_list_t;

typedef struct
{
    THD_3dim_dataset * dset;		/* for data and geometry     */
    THD_fvec3          p1;		/* segment endpoints         */
    THD_fvec3          pn;
    int                debug;		/* for local control         */
} range_3dmm;

typedef struct
{
    MRI_IMARR   ims;			/* the image array struct     */
    int         masked;			/* number of masked points    */
    int         ifirst;			/* 1D index of first point    */
    THD_ivec3   i3first;		/* i3ind index of first point */
    THD_ivec3 * i3arr;			/* i3ind index array          */
} range_3dmm_res;


/* ---- function prototypes ---- */

int alloc_node_list   ( smap_opts_t * sopt, node_list_t * N, int nsurf);
int check_datum_type  ( char * datum_str, int default_type );
int check_map_func    ( char * map_str );
int check_norm_dirs   ( smap_opts_t * sopt, node_list_t * N, int surf );
int create_node_list  ( smap_opts_t * sopt, node_list_t * N );
int disp_fvec         ( char * info, float * f, int len );
int disp_mri_imarr    ( char * info, MRI_IMARR * dp );
int disp_opts_t       ( char * info, opts_t * opts );
int disp_param_t      ( char * info, param_t * p );
int disp_range_3dmm   ( char * info, range_3dmm * dp );
int disp_range_3dmm_res( char * info, range_3dmm_res * dp );
int disp_smap_opts_t  ( char * info, smap_opts_t * sopt );
int dump_surf_3dt     ( smap_opts_t * sopt, param_t * p, node_list_t * N );
int final_clean_up    ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec,
       			node_list_t * N );
int get_mappable_surfs( SUMA_SurfaceObject ** slist, int how_many, int debug );
int init_options      ( opts_t * opts, int argc, char * argv [] );
int print_default_line( FILE * fp, int max_ind, int node_ind,
	                int vind, int i, int j, int k, float fval );
int print_header      ( FILE * outfp, char * surf, char * map, int nvals);
int read_surf_files   ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec );
int segment_imarr     ( range_3dmm_res *res, range_3dmm *R, smap_opts_t *sopt );
int set_smap_opts     ( opts_t * opts, param_t * p, smap_opts_t * sopt );
int set_outfile       ( opts_t * O, param_t * P );
int smd_map_type      ( char * map_str );
int surf_ave_radius   ( float * rad, SUMA_SurfaceObject * so, int disp );
int usage             ( char * prog, int level );
int v2s_adjust_endpts ( smap_opts_t * sopt, THD_fvec3 * p1, THD_fvec3 * pn );
int v2s_map_needs_sort( int map );
int validate_datasets ( opts_t * opts, param_t * p );
int validate_options  ( opts_t * opts, param_t * p );
int validate_surface  ( opts_t * opts, param_t * p );
int vals_over_steps   ( int map );
int write_output      ( smap_opts_t * sopt, param_t * p, node_list_t * N );


/* float list functions */
int float_list_alloc     ( float_list * f, int **ilist, int size, int truncate);
int float_list_comp_mode ( float_list * f, float *mode, int *nvals, int *index);
int float_list_slow_sort ( float_list * f, int * ilist );


int f3mm_out_of_bounds( THD_fvec3 * cp, THD_fvec3 * min, THD_fvec3 * max );
int set_3dmm_bounds   ( THD_3dim_dataset *dset, THD_fvec3 *min, THD_fvec3 *max);

float  directed_dist   ( float * pnew, float * pold, float * dir, float dist );
float  dist_f3mm       ( THD_fvec3 * p1, THD_fvec3 * p2 );
double magnitude_f     ( float * p, int length );
float  v2s_apply_filter( range_3dmm_res *rr, smap_opts_t *sopt, int index,
			int * findex );

#endif   /* _3dVOL2SURF_H_ */
