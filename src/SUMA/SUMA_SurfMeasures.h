#ifndef _SUMA_SURFMEASURES_H_
#define _SUMA_SURFMEASURES_H_

#define PROG_NAME		"SurfMeasures"

#define ST_USE_LONG		1
#define ST_USE_SHORT		2
#define ST_USE_VERSION		3

#define ST_DEBUG_MAX_LEVEL      5
#define ST_DEFAULT_FALLOC      10	/* init alloc for num functions  */

#define ST_SINFO                1	/* infomation type */
#define ST_SMEASURE             2	/* measure type    */

#define CHECK_NULL_STR(str) ( str ? str : "(NULL)" )

/* measure codes, to match g_smeasure_names */
typedef enum
{
    E_SM_INVALID,			/* do not change INVALID or FINAL    */
    E_SM_COORD_A,			/* give coordinates of first node    */
    E_SM_COORD_B,			/* give coordinates of second node   */
    E_SM_N_AREA_A,			/* give node area of first node      */
    E_SM_N_AREA_B,			/* give node area of second node     */
    E_SM_NODES,				/* print out node indices            */
    E_SM_NODE_VOL,			/* give volume for each node         */
    E_SM_THICK,				/* display between surface thickness */
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
    SUMA_SurfSpecFile     spec;         /* spec file structure         */
    SUMA_SurfaceObject ** slist;	/* list of SO pointers         */
    float               * narea[2];     /* list of computed node areas */
    int                   nsurf;	/* number in list              */
    int                   salloc;	/* number allocated for        */
    int                   nnodes;       /* N_Nodes for each surf       */
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
    int      total_vol;			/* compute total volume        */
    int      debug;			/* level of debug output       */
    int      dnode;			/* node watched for debug      */
} opts_t;

typedef struct
{
    surf_t   S;
    func_t * F;
    FILE   * outfp;
} param_t;


/* protos */

int	add_to_flist         ( func_t * F, char * fname );
int	all_mappable_surfs   ( opts_t * opts, param_t * p );
int	check_func_name      ( char * func );
int	compute_node_areas   ( opts_t * opts, param_t * p, int sindex );
int	disp_surf_t          ( char * info, surf_t * d );
float   dist_f3mm            ( THD_fvec3 * p1, THD_fvec3 * p2 );
int	final_cleanup        ( opts_t * opts, param_t * p );
int	get_surf_data        ( opts_t * opts, param_t * p );
int	get_surf_measures    ( opts_t * opts, param_t * p );
int	init_options         ( opts_t * opts, int argc, char * argv[] );
int	init_opts_t          ( opts_t * opts );
int	print_column_headers ( opts_t * opts, param_t * p );
int	spec2SUMA            ( SUMA_SurfSpecFile * spec, char * spec_file,
	                       char * sv_file, int debug );
int	usage                ( char * prog, int use_type );
int	validate_option_lists( opts_t * opts, param_t * p );
int	validate_options     ( opts_t * opts, param_t * p );
int	verify_surf_t        ( opts_t * opts, param_t * p );
int	write_output         ( opts_t * opts, param_t * p );



#endif     /* #define _SUMA_SURFMEASURES_H_ */
