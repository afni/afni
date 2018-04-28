#ifndef _3DSURF2VOL_H_
#define _3DSURF2VOL_H_

#define PROG_NAME               "3dSurf2Vol"

#define S2V_USE_LONG              1
#define S2V_USE_SHORT             2
#define S2V_USE_HIST              3
#define S2V_USE_VERSION           4

#define S2V_F_STEPS_MIN           1

#define S2V_F_INDEX_VOXEL         0
#define S2V_F_INDEX_POINT         1

#define S2V_MAX_SURFS             2

#define S2V_DEBUG_MAX_LEV         5
#define S2V_DEBUG_TEST_NODE       7

#undef  CHECK_NULL_STR
#define CHECK_NULL_STR(str) ((str) ? (str) : "(NULL)")

/* surface to voxel mapping codes, along with command-line strings */
typedef enum
{
    E_SMAP_INVALID = -1,
    E_SMAP_NONE,                        /* do not change INVALID or NONE */
    E_SMAP_MASK, E_SMAP_MASK2,
    E_SMAP_AVE, E_SMAP_NZ_AVE,
    E_SMAP_COUNT,
    E_SMAP_MIN,  E_SMAP_MAX,
    E_SMAP_MAX_ABS,
    E_SMAP_MODE, E_SMAP_NZ_MODE,        /* 3 Nov 2011, 27 Apr 2018 [rickr] */
    E_SMAP_MEDIAN, E_SMAP_NZ_MEDIAN,    /* 27 Apr 2018 [rickr, drg]  */
    E_SMAP_FINAL                        /* do not change FINAL */
} s2v_map_num;


/* user options */
typedef struct
{
    char   * gpar_file;                 /* AFNI grid parent filename   */
    char   * oset_file;                 /* filename for ascii output   */
    char   * spec_file;                 /* surface specification file  */
    char   * sv_file;                   /* AFNI surface volume dataset */
    char   * surf_xyz_1D_file;          /* input 1D surface coord file */
    char   * sdata_file_1D;             /* input 1D surface data file  */
    char   * sdata_file_niml;           /* input niml surf data file   */
    char   * cmask_cmd;                 /* 3dcalc style mask command   */
    char   * data_expr;                 /* comp. float from node data  */ 
    char   * map_str;                   /* how to map surf(s) to dset  */
    char   * datum_str;                 /* data type of output dset    */
    char   * f_index_str;               /* count by voxels or points   */
    char   * snames[S2V_MAX_SURFS];     /* list of surfaces to use     */
    int      sxyz_ori_gpar;             /* input xyz use gpar orient   */
    int      stop_gap;                  /* stop at mask gap            */
    int      debug;                     /* level of debug output       */
    int      dnode;                     /* node watched for debug      */
    int      dvox;                      /* voxel watched for debug     */
    int      noscale;                   /* no scale of integer output  */
    int      f_steps;                   /* # int steps for mask2 map   */
    float    f_p1_fr;                   /* fractional dist: add to p1  */
    float    f_pn_fr;                   /* fractional dist: add to pn  */
    float    f_p1_mm;                   /* mm distance to add to p1    */
    float    f_pn_mm;                   /* mm distance to add to pn    */
} opts_t;

typedef struct
{
    int      map;                       /* type of mapping from surfs  */
    int      datum;                     /* data type of output dset    */
    int      noscale;                   /* no scale of integer output  */
    int      debug;                     /* for printing extra output   */
    int      dnode;                     /* node watched for debug      */
    int      dvox;                      /* voxel watched for debug     */
    byte   * cmask;                     /* computed mask               */
    int      stop_gap;                  /* stop at mask gap            */
    int      sxyz_ori_gpar;             /* input xyz use gpar orient   */
    int      f_steps;                   /* # int steps for mask2 map   */
    int      f_index;                   /* count by voxels or points   */
    float    f_p1_fr;                   /* fractional dist: add to p1  */
    float    f_pn_fr;                   /* fractional dist: add to pn  */
    float    f_p1_mm;                   /* mm distance to add to p1    */
    float    f_pn_mm;                   /* mm distance to add to pn    */
} s2v_opts_t;

/* PARSER stuff */
typedef struct
{
    PARSER_code * pcode;
    double        atoz[26];
    int           has_sym[26];
    int           max_sym;
} parser_t;

/* computational parameters */
typedef struct
{
    THD_3dim_dataset * gpar;            /* input dataset               */
    THD_3dim_dataset * oset;            /* output dataset              */
    MRI_IMAGE        * sxyz_im;         /* surface coordinate image    */
    MRI_IMAGE        * sdata_im;        /* sub-surface data image      */
    SUMA_DSET        * dset;            /* Dset instead of sdata_im    */
    THD_fvec3          f3mm_min;        /* numerical min xyz points    */
    THD_fvec3          f3mm_max;        /* numerical max xyz points    */
    parser_t           parser;          /* code for expression eval    */
    int                nvox;            /* gpar nxyz                   */
    int                nsubs;           /* number of output sub-bricks */
    byte             * cmask;           /* computed mask               */
    int                ncmask;          /* nvox for cmask              */
    int                ccount;          /* mask size                   */
} param_t;

/* node list structure */
typedef struct
{
    THD_fvec3  * nodes;                 /* depth x nnodes in size      */
    int          depth;                 /* major axis                  */
    int          nnodes;                /* minor axis                  */

    float      * fdata;                 /* data list (length as ilist) */
    int        * ilist;                 /* index list - nodes indices  */
    int          ilen;                  /* length of ilist             */
} node_list_t;

/* aggregate voxel list structure (allow for multiple possibilities)   */
typedef struct {
    float_list * vlist;
} aggr_list_t;

/* ---- function prototypes ---- */

/* library protos - rcr - move to separate file */
float dist_f3mm       ( THD_fvec3 * p1, THD_fvec3 * p2 );
int   s2v_map_type    ( char * map_str );
int   compute_results ( param_t * p, node_list_t * N, s2v_opts_t * sopt,
                         double * ddata, int * idata, THD_fvec3 * pary,
                         aggr_list_t * aggr );
THD_3dim_dataset * s2v_nodes2volume(node_list_t *N,param_t *p,s2v_opts_t *sopt);


int adjust_endpts     ( s2v_opts_t * sopt, THD_fvec3 * p1, THD_fvec3 * pn );
int check_datum_type  ( char * datum_str, int default_type );
int check_map_func    ( char * map_str );
int disp_node_list_t  ( char * info, node_list_t * d );
int disp_opts_t       ( char * info, opts_t * opts );
int disp_param_t      ( char * info, param_t * p );
int disp_parser_t     ( char * info, parser_t * d );
int disp_s2v_opts_t   ( char * info, s2v_opts_t * sopt );
int f3mm_out_of_bounds( THD_fvec3 * cp, THD_fvec3 * min, THD_fvec3 * max );
int fill_node_list    ( opts_t * opts, param_t * p, node_list_t * N );
int fill_SUMA_structs ( opts_t * opts, SUMA_SurfSpecFile * spec );
int final_clean_up    ( node_list_t * N );
int final_computations(double *ddata, int *idata, s2v_opts_t *sopt, int nvox,
                       aggr_list_t * aggr);
int get_mappable_surfs( SUMA_SurfaceObject ** slist, int how_many, int debug );
int init_node_list    (opts_t *opts,param_t *p,s2v_opts_t *sopt,node_list_t *N);
int init_options      ( opts_t * opts, int argc, char * argv [] );
int insert_list       ( node_list_t * N, param_t * p, s2v_opts_t * sopt,
                        THD_fvec3 *pary, int nindex, double *ddata, int *idata,
                        aggr_list_t * aggr );
int insert_value      ( s2v_opts_t * sopt, double *dv, int *iv, int vox,
                        int node, float value, aggr_list_t * aggr );
int integral_doubles  ( double * dp, int nvals );
int is_aggregate_type ( int map_func );
int make_point_list   ( THD_fvec3 * list, THD_fvec3 * p1, THD_fvec3 * pn,
                        s2v_opts_t * sopt );
int read_surf_files   ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec,
                        s2v_opts_t * sopt, node_list_t * N );
int read_sxyz_1D      ( opts_t * opts, param_t * p );
int sdata_from_1D     ( opts_t * opts, param_t * p, node_list_t * N );
int sdata_from_niml     ( opts_t * opts, param_t * p, node_list_t * N );
int sdata_from_default( node_list_t * N );
int set_3dmm_bounds   ( THD_3dim_dataset *dset, THD_fvec3 *min, THD_fvec3 *max);
int set_node_list_data( node_list_t *N, param_t *p, s2v_opts_t *sopt, int col);
int set_smap_opts     ( opts_t * opts, param_t * p, s2v_opts_t * sopt );
int surf_to_node_list ( s2v_opts_t * sopt, node_list_t * N, int nsurf);
int sxyz_1D_to_nlist  ( opts_t * opts, s2v_opts_t * sopt, param_t * p,
                        node_list_t *N, int * nsurf);
int usage             ( char * prog, int level );
int validate_datasets ( opts_t * opts, param_t * p );
int validate_options  ( opts_t * opts, param_t * p );
int validate_surface  ( opts_t * opts, param_t * p );
int verify_node_list  ( node_list_t * N );
int verify_parser_expr( opts_t * opts, param_t * p );
int write_output      ( s2v_opts_t * sopt, opts_t * opts, param_t * p,
                        node_list_t * N, int argc, char * argv[] );


#endif   /* _3DSURF2VOL_H_ */
