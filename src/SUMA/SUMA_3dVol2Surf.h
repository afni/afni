#ifndef _3dVOL2SURF_H_
#define _3dVOL2SURF_H_

#define PROG_NAME		"3dVol2Surf"

#define V2S_USE_LONG      	  1
#define V2S_USE_SHORT     	  2
#define V2S_USE_HIST     	  3
#define V2S_USE_LIB_HIST     	  4
#define V2S_USE_VERSION   	  5

#define V2S_DEBUG_MAX_LEV	  5
#define V2S_DEBUG_TEST_NODE	  7

#define V2S_M2_STEPS_DEFAULT      2

/* user options */
typedef struct
{
    char      * gpar_file;		/* AFNI grid parent filename    */
    char      * outfile_1D;		/* filename for ascii output    */
    char      * outfile_niml;		/* filename for NIML output     */
    char      * spec_file;		/* surface specification file   */
    char      * sv_file;		/* AFNI surface volume dataset  */
    char      * cmask_cmd;     		/* 3dcalc style mask command    */
    char      * map_str;		/* how to map surf(s) to dset   */
    char      * snames[V2S_MAX_SURFS];	/* list of surfaces to use      */
    int         no_head;		/* do not write output headers  */
    int         skip_cols;		/* which output columns to skip */
    int         first_node;		/* skip nodes before this index */
    int         last_node;		/* skip nodes after this index  */
    int         use_norms;		/* use normals for segments     */
    float       norm_len;		/* signed length of normal      */
    int         keep_norm_dir;		/* no directional check         */
    int         debug;			/* level of debug output        */
    int         dnode;			/* node watched for debug       */
    char      * f_index_str;		/* node or voxel index type     */
    int         f_steps;		/* # int steps for mask2 map    */
    float       f_p1_fr;		/* fractional dist: add to p1   */
    float       f_pn_fr;		/* fractional dist: add to pn   */
    float       f_p1_mm;		/* mm distance to add to p1     */
    float       f_pn_mm;		/* mm distance to add to pn     */
    v2s_oob_t   oob;			/* display info for oob nodes   */
    v2s_oob_t   oom;			/* display info for oom nodes   */
} opts_t;

/* ---- function prototypes ---- */

int check_datum_type  ( char * datum_str, int default_type );
int check_map_func    ( char * map_str );
int check_norm_dirs   ( v2s_opts_t * sopt, v2s_param_t * p, int surf );
int check_outfile     ( opts_t * O, v2s_param_t * P );
int copy_surfaces     ( v2s_opts_t * sopt, v2s_param_t * p, int nsurf);
int disp_fvec         ( char * info, float * f, int len );
int disp_opts_t       ( char * info, opts_t * opts );
int final_clean_up    ( opts_t *opts, v2s_param_t *p, SUMA_SurfSpecFile *spec );int get_mappable_surfs( SUMA_SurfaceObject ** slist, int how_many, int debug );
int get_surf_data     ( v2s_opts_t * sopt, v2s_param_t * p );
int init_options      ( opts_t * opts, int argc, char * argv [] );
int print_default_line( FILE * fp, int max_ind, int node_ind,
                        int vind, int i, int j, int k, float fval );
int print_header      ( FILE * outfp, char * surf, char * map, v2s_results *sd);int read_surf_files   ( opts_t * opts, SUMA_SurfSpecFile * spec );
int set_smap_opts     ( opts_t * opts, v2s_param_t * p, v2s_opts_t * sopt );
int smd_map_type      ( char * map_str );
int suma2afni_surf    ( v2s_opts_t * sopt, v2s_param_t * p,
                        SUMA_SurfaceObject * so, int sindex );
int surf_ave_radius   ( float * rad, SUMA_SurfaceObject * so, int disp );
int usage             ( char * prog, int level );
int validate_datasets ( opts_t * opts, v2s_param_t * p );
int validate_options  ( opts_t * opts, v2s_param_t * p );
int validate_surface  ( opts_t * opts, v2s_param_t * p );
int write_outfile_1D  ( v2s_opts_t * sopt, v2s_results * sd, char * label );
int write_outfile_niml( v2s_opts_t * sopt, v2s_results * sd, int free_vals );
int write_output      ( v2s_opts_t * sopt, v2s_param_t * p );

#endif   /* _3dVOL2SURF_H_ */
