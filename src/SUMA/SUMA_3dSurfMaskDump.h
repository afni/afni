#ifndef _3DSURFMASKDUMP_H_
#define _3DSURFMASKDUMP_H_

#define PROG_NAME         "3dSurfMaskDump"

#define SMD_USE_LONG      1
#define SMD_USE_SHORT     2
#define SMD_USE_VERSION   3

#define SMD_MAX_DEBUG     4

/* user options */
typedef struct
{
    char   * dset_file;			/* AFNI dset for output        */
    char   * out_file;			/* filename for ascii output   */
    char   * spec_file;			/* surface specification file  */
    char   * sv_file;			/* AFNI surface volume dataset */
    char   * cmask_cmd;      		/* 3dcalc style mask command   */
    int      range_bot, range_top;      /* ? */
    int      debug;			/* level of debug output       */
    int      no_headers;		/* do not write output headers */
} opts_t;

/* computational parameters */
typedef struct
{
    THD_3dim_dataset * dset;		/* dataset - basis for output */
    FILE             * outfp;		/* out_file FILE stream       */
    int                nvox;		/* dset nxyz                  */
    byte             * cmask;		/* computed mask              */
    int                ncmask;		/* nvox for cmask             */
    int                ccount;          /* mask size                  */
} param_t;


/* function prototypes */

int disp_opts_t       ( char * info, opts_t * opts );
int disp_param_t      ( char * info, param_t * p );
int final_clean_up    ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec );
int init_options      ( opts_t * opts, param_t * p, int argc, char * argv [] );
int read_surf_files   ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec );
int usage             ( char * prog, int level );
int validate_datasets ( opts_t * opts, param_t * p );
int validate_options  ( opts_t * opts, param_t * p );
int validate_surface  ( opts_t * opts, param_t * p );
int write_output      ( opts_t * opts, param_t * p, SUMA_SurfSpecFile * spec );
int write_so_data     ( opts_t * opts, param_t * p, SUMA_SurfaceObject * so );


#endif   /* _3DSURFMASKDUMP_H_ */
