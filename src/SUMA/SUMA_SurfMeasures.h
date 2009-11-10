#ifndef _SUMA_SURFMEASURES_H_
#define _SUMA_SURFMEASURES_H_

#define PROG_NAME		"SurfMeasures"

#define ST_USE_LONG		1
#define ST_USE_SHORT		2
#define ST_USE_HIST		3
#define ST_USE_VERSION		4

#define ST_INFO_NONE		0
#define ST_INFO_AREA		1
#define ST_INFO_NORMS		2
#define ST_INFO_THICK           4
#define ST_INFO_VOL             8
#define ST_INFO_VOLG           16
#define ST_INFO_ALL        0xffff

#define ST_MAX_SURFS		2

#define ST_DEBUG_MAX_LEVEL      5
#define ST_DEFAULT_FALLOC      10	/* init alloc for num functions  */

#define ST_SINFO                1	/* infomation type */
#define ST_SMEASURE             2	/* measure type    */

#define ST_PI			3.141592653589793

#define RANGE(a,b,c) do {if ((b)<(a)) (b)=(a); if ((b)>(c)) (b)=(c);} while (0)

#undef  CHECK_NULL_STR
#define CHECK_NULL_STR(str) ( str ? str : "(NULL)" )

/* measure codes, to match g_smeasure_names */
typedef enum
{
    E_SM_INVALID,			/* do not change INVALID or FINAL     */
    E_SM_ANG_NORMS,			/* give angle diff between normals    */
    E_SM_ANG_NS_A,			/* give norm A angle to segment       */
    E_SM_ANG_NS_B,			/* give norm B angle to segment       */
    E_SM_COORD_A,			/* give coordinates of first node     */
    E_SM_COORD_B,			/* give coordinates of second node    */
    E_SM_N_AREA_A,			/* give node area of first node       */
    E_SM_N_AREA_B,			/* give node area of second node      */
    E_SM_N_AVEAREA_A,			/* average area of included triangles */
    E_SM_N_AVEAREA_B,			/* average area of included triangles */
    E_SM_NTRI,		 		/* give number of included triangles  */
    E_SM_NODE_VOL,			/* give volume for each node          */
    E_SM_NODE_VOLG,			/* give volume for each node with Gauss' Theorem */
    E_SM_NODES,				/* print out node indices             */
    E_SM_NORM_A,			/* give normal vector for first node  */
    E_SM_NORM_B,			/* give normal vector for second node */
    E_SM_THICK,				/* display between surface thickness  */
    E_SM_FINAL
} smeasure_codes_e;

#define ALLOC_CHECK(ptr,name,nel)					  \
	do { if (!(ptr))						  \
	     { fprintf(stderr,"** failed to allocate %d '%s' elements\n", \
			nel, name);					  \
	       exit(1);							  \
	     } } while (0)

typedef struct
{
    SUMA_SurfSpecFile     spec;         /* spec file structure           */
    SUMA_SurfaceObject ** slist;	/* list of SO pointers           */
    float               * narea[2];     /* list of computed node areas   */
    float               * nvol;         /* list of computed node volumes */
    float               * nvolg;        /* list of computed node volumes 
                                           with Gauss Theorem          */
    float               * fvol;         /* list of computed face volumes */
    int                   nsurf;	/* number in list                */
    int                   salloc;	/* number allocated for          */
    int                   nnodes;       /* N_Nodes for each surf         */
    int                   nfaces;       /* N_FaceSet for each surf       */
} surf_t;

typedef struct
{
    char ** names;			/* list of function names      */
    int   * codes;			/* list of function codes      */
    int     nalloc;			/* number allocated for        */
    int     nused;			/* number actually used        */
} func_t;

typedef struct
{
    func_t   F;				/* funtion list struct         */
    char   * spec_file;			/* surface specification file  */
    char   * sv_file;			/* AFNI surface volume dataset */
    char   * out_1D_file;		/* surface output filename     */
    char   * cmask_cmd;                 /* 3dcalc style cmask command  */
    char   * nodes_1D_file;             /* node list to apply cmask to */
    char   * surf_names[ST_MAX_SURFS];  /* list of surfaces to be used */
    int      info;			/* display final information   */
    int      debug;			/* level of debug output       */
    int      dnode;			/* node watched for debug      */
} opts_t;

typedef struct
{
    surf_t   S;
    func_t * F;
    FILE   * outfp;
    int    * nodes;
    int      nnodes;			/* length of node_list         */
    byte   * cmask;
    int      ncmask;			/* length of cmask             */
    int      ccount;			/* number of set entries       */
} param_t;


/* protos */

int	add_to_flist         ( func_t * F, char * fname );
int	all_mappable_surfs   ( opts_t * opts, param_t * p );
int	check_func_name      ( char * func );
int	compute_face_vols    ( opts_t * opts, param_t * p );
int	compute_node_areas   ( opts_t * opts, param_t * p, int sindex );
int	compute_node_vols    ( opts_t * opts, param_t * p );
int	compute_node_vols_G  ( opts_t * opts, param_t * p );
int	disp_f3_point        ( char * info, float * d );
int	disp_func_t          ( char * info, func_t * d );
int	disp_opts_t          ( char * info, opts_t * d );
int	disp_param_t         ( char * info, param_t * d );
int	disp_surf_t          ( char * info, surf_t * d );
int	final_cleanup        ( opts_t * opts, param_t * p );
int	get_cmask            ( opts_t * opts, param_t * p );
int	get_surf_data        ( opts_t * opts, param_t * p );
int	get_surf_measures    ( opts_t * opts, param_t * p );
int	init_options         ( opts_t * opts, int argc, char * argv[] );
int	init_opts_t          ( opts_t * opts );
int	print_column_headers ( opts_t * opts, param_t * p );
int	read_nodes_file      ( opts_t * opts, param_t * p );
int	spec2SUMA            ( SUMA_SurfSpecFile * spec, opts_t * opts );
int	usage                ( char * prog, int use_type );
int	validate_option_lists( opts_t * opts, param_t * p );
int	validate_options     ( opts_t * opts, param_t * p );
int	verify_surf_t        ( opts_t * opts, param_t * p );
int	write_output         ( opts_t * opts, param_t * p );

/* geometric routines */
int     cross_product        ( double * res, double * u, double * v );
double  dist_fn              ( int len, float * p1, float * p2 );
double  dot_product          ( double * u, double * v );
float   fvec_magnitude       ( float * vec, int length );
float   magnitude_vec_N      ( float * vec, int length );
float	norm2seg_angle       ( THD_fvec3 * p0, THD_fvec3 * p1, float * norm );
int     surf_triangle_match  ( opts_t * opts, param_t * p);
double  tetra_volume         ( float * p0, float * p1, float * p2, float * p3 );
float	vector_angle         ( float * v0, float * v1 );

double  lazy_det             ( int width, double * data );

#endif     /* #define _SUMA_SURFMEASURES_H_ */
