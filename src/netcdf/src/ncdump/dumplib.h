/*********************************************************************
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header$
 *********************************************************************/

extern char *progname;		/* for error messages */

#define NO_NETCDF_2		/* assert we aren't using any netcdf-2 stuff */

#ifndef EXIT_FAILURE
#ifndef vms
#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
#else
#define EXIT_SUCCESS 1
#define EXIT_FAILURE 0
#endif
#endif

#define FLT_DIGITS 7		/* default sig. digits for float data */
#define DBL_DIGITS 15		/* default sig. digits for double data */

extern int float_precision_specified; /* -p option specified float precision */
extern int double_precision_specified; /* -p option specified double precision */
extern char float_var_fmt[];
extern char double_var_fmt[];
extern char float_att_fmt[];
extern char double_att_fmt[];

#ifdef __cplusplus
extern "C" {
#endif

/* Print error message to stderr and exit */
extern void	error ( const char *fmt, ... );

/* set position in line before lput() calls */
extern void	set_indent ( int indent );

/* set maximum line length */
extern void	set_max_len ( int len );

/* splits lines to keep them short */
extern void	lput ( const char *string );

/* In case different formats specified with -d option, set them here. */
extern void	set_formats ( int flt_digs, int dbl_digs );

/* Determine print format to use for each value for this variable. */
char *		get_fmt ( int ncid, int varid, nc_type type );

/* structure for list of variables specified with -v option */
struct vnode
{
    struct vnode* next;
    int id;
};
typedef struct vnode vnode;

/* Get new variable list */
extern vnode*	newvlist ( void );

/* Add a variable id to variable list */
extern void	varadd ( vnode* vlist, int varid );

/* Test if a variable id is in variable list */
extern int	varmember ( const vnode* vlist, int varid );

#ifdef __cplusplus
}
#endif
