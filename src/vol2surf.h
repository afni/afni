#ifndef _VOL2SURF_H_
#define _VOL2SURF_H_

#define V2S_MAX_SURFS             2

#define V2S_INDEX_VOXEL           0
#define V2S_INDEX_NODE            1

#define V2S_STEPS_TOOOOO_BIG  	  10000

#define V2S_NORM_DEFAULT  	  0
#define V2S_NORM_KEEP		  1
#define V2S_NORM_REVERSE	  2

#define V2S_EPSILON	      	  0.0001

#define V2S_SKIP_NONE		  0
#define V2S_SKIP_NODES		  1
#define V2S_SKIP_VOLIND		  2
#define V2S_SKIP_I		  4
#define V2S_SKIP_J		  8
#define V2S_SKIP_K		 16
#define V2S_SKIP_NVALS		 32
#define V2S_SKIP_VALS		 64	/* never skip first result value */
#define V2S_SKIP_ALL		127

#define CHECK_NULL_STR(str)  ( str ? str : "<NULL>" )
#define CHECK_EMPTY_STR(str) ( str[0] ? str : "<empty>" )

/* surface to voxel mapping codes, along with command-line strings */
typedef enum
{
    E_SMAP_INVALID = 0,			/* do not change INVALID (from 0) */
    E_SMAP_MASK,    E_SMAP_MIDPT,
    E_SMAP_MASK2,
    E_SMAP_AVE,     E_SMAP_COUNT,
    E_SMAP_MIN,     E_SMAP_MAX,
    E_SMAP_MAX_ABS, E_SMAP_SEG_VALS,
    /* sorted ones: */
    E_SMAP_MEDIAN,  E_SMAP_MODE,
    E_SMAP_FINAL			/* leave FINAL at the end */
} v2s_map_nums;

typedef struct
{
    int   show;
    int   index;
    float value;
} v2s_oob_t;

typedef struct
{
    int      nalloc;
    int      nused;
    int      max_vals;
    int      memory;
    int    * nodes;
    int    * volind;
    int    * i;
    int    * j;
    int    * k;
    int    * nvals;
    float ** vals;
} v2s_results;

typedef struct
{
    int         map;			/* type of mapping from surfs    */
    int         gp_index;		/* grid parent sub-brick (or -1) */
    int         debug;			/* for printing extra output     */
    int         dnode;			/* node watched for debug        */
    int         no_head;		/* do not write output headers   */
    int         skip_cols;		/* which output columns to skip  */
    int         first_node;		/* skip nodes before this index  */
    int         last_node; 		/* skip nodes after this index   */
    int         use_norms;		/* use normals for segments      */
    float       norm_len;		/* signed length of normal       */
    int         norm_dir;		/* default, keep or reverse      */
    int         f_index;		/* node or voxel index type      */
    int         f_steps;		/* # int steps for mask2 map     */
    float       f_p1_fr;		/* fractional dist: add to p1    */
    float       f_pn_fr;		/* fractional dist: add to pn    */
    float       f_p1_mm;		/* mm distance to add to p1      */
    float       f_pn_mm;		/* mm distance to add to pn      */
    char      * outfile_1D;		/* filename for ascii output     */
    char      * outfile_niml;		/* filename for NIML output      */
    v2s_oob_t   oob;			/* display info for oob nodes    */
    v2s_oob_t   oom;			/* display info for oom nodes    */
} v2s_opts_t;

typedef struct
{
    int        ready, use0, use1;
    int        s0A, s0B;
    int        s1A, s1B;
    v2s_opts_t sopt;
} v2s_plugin_opts;

/* computational parameters */
typedef struct
{
    THD_3dim_dataset * gpar;		/* input dataset               */
    byte             * cmask;		/* computed mask               */
    int                nvox;		/* gpar nxyz                   */
    int                over_steps;      /* vals computed over steps    */
    int                nsurf;           /* number of surfaces to apply */
    SUMA_surface       surf[V2S_MAX_SURFS]; /* surface structure info  */
} v2s_param_t;

/* ---- export function prototypes ---- */

v2s_results * afni_vol2surf	( THD_3dim_dataset * gpar, int gp_index,
				  SUMA_surface * sA, SUMA_surface * sB,
				  byte * mask, int use_defaults );
v2s_results * vol2surf		( v2s_opts_t * sopt, v2s_param_t * p );

int disp_mri_imarr      ( char * info, MRI_IMARR * dp );
int disp_v2s_opts_t     ( char * info, v2s_opts_t * sopt );
int disp_v2s_param_t    ( char * info, v2s_param_t * p );
int disp_v2s_plugin_opts( char * mesg, v2s_plugin_opts * d );
int disp_v2s_results    ( char * mesg, v2s_results * d );
int free_v2s_results    ( v2s_results * sd );
int v2s_is_good_map     ( int map, int from_afni );
int v2s_map_type        ( char * map_str );
int v2s_vals_over_steps ( int map );
int v2s_write_outfile_1D( v2s_opts_t * sopt, v2s_results * sd, char * label );
int v2s_write_outfile_niml( v2s_opts_t * sopt, v2s_results * sd, int free_vals);


/* special thd function - might be moved from vol2surf.[ch] */
int thd_mask_from_brick(THD_3dim_dataset * dset, int volume, float thresh,
			byte ** mask, int absolute);

/* ---- define globals for everyone but vol2surf.c ---- */
#ifndef _VOL2SURF_C_
    extern v2s_plugin_opts   gv2s_plug_opts;
    extern char            * gv2s_map_names[];
    extern char              gv2s_history[];
#endif


#endif   /* _VOL2SURF_H_ */
