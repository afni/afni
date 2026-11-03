#ifndef _THD_DATATABLE_HEADER_
#define _THD_DATATABLE_HEADER_

#include <stdio.h>

/*----------------------------------------------------------------------------
  A '-dataTable' reader, in the format long used by the R programs 3dMVM,
  3dLME, 3dISC and friends: a header row of column names, then one row per
  input dataset.  Until now that format had no C implementation; this is it.

  A table looks like

      Subj  Group  Age  InputFile
      s01   ctl    22   s01.betas+tlrc
      s02   pat    31   s02.betas+tlrc

  Files may instead place the schema in a leading '# Columns: ...' line.
  Spaces and tabs separate fields; other '#' comments are ignored. Quote a
  field containing whitespace or literal comment characters; double a literal
  quote inside a quoted field. THD_1D_print_token writes this representation.

  Two columns are special, and both are optional:
    'InputFile' -- the dataset for that row
    'Subj'      -- the subject label; if absent, labels are synthesized

  Every other column is available to the calling program, as text always and
  as a float when the whole column parses numerically.
                                                     -- P Molfese, Jul 2026
------------------------------------------------------------------------------*/

typedef struct {
   int     ncol ;       /* number of columns */
   int     nrow ;       /* number of data rows (not counting the header) */

   char  **cname ;      /* [ncol] column names, as written */
   char  **cell ;       /* [nrow*ncol] raw text, row major; see DT_CELL */
   float **val ;        /* [ncol][nrow] numeric values; junk unless isnum */
   int    *isnum ;      /* [ncol] 1 if every cell in the column is a number */

   char  **subj ;       /* [nrow] subject labels (synthesized if no Subj col) */
   char  **fname ;      /* [nrow] InputFile values, or NULL if no such column */

   int     icol_subj ;  /* index of the Subj column, or -1 */
   int     icol_input ; /* index of the InputFile column, or -1 */

   char   *source ;     /* where the table came from, for error messages */
   int     from_argv ;  /* 1 if read inline from the command line */
} THD_datatable ;

/*! A value-based Cartesian index over one or more table columns.  row_of is
    laid out in row-major coordinate order (the last indexed column varies
    fastest) and stores the corresponding original table row.  Thus consumers
    can accept arbitrarily ordered input without sorting or mutating the source
    table, while retaining original row numbers for diagnostics. */
typedef struct {
   int ndim , nrow , ncell ;
   int *icol , *nlevel , *stride ;
   char **column ;
   char ***level ;
   int *row_coord ;          /* [nrow*ndim] coordinate of each original row */
   int *row_of ;             /* [ncell] original row at each Cartesian cell */
} THD_datatable_index ;

#define THD_DT_LEVELS_FIRST    0  /* discover in first-appearance order */
#define THD_DT_LEVELS_LEXICAL -1  /* discover, then strcmp lexical order */

#define DT_CELL(dt,i,j)  ( (dt)->cell[ (i)*(dt)->ncol + (j) ] )  /* row i, col j */
#define DT_NROW(dt)      ( (dt)->nrow )
#define DT_NCOL(dt)      ( (dt)->ncol )

/*-- design matrices: a table can name each row's X.xmat.1D in a column (Xmat);
     thd_xmat.h reads them and adds THD_datatable_expand_xmat_items() and
     THD_datatable_add_xmat_columns().  They live in thd_xmat.c, so programs
     that do not use design matrices do not link it. --*/

/*-- reading --*/

/*! Read a table from a file.  The header is the first non-comment line, and
    the column count comes from it, so 'InputFile' may sit in any column.  A
    line ending in backslash continues onto the next.  '#' at the start of a
    word begins a comment; inside a word it is text (face#0,
    stats+tlrc[face#0_Coef]).  A word that begins with a quote, "..." or '...',
    runs to the matching quote, so a value can hold spaces or start with '#';
    a quote inside a word is text, as in AFNI's x.1D' (transposed). */
extern THD_datatable * THD_read_datatable_file( char *fname ) ;

/*! Read a table from the command line, starting at argv[nopt].  Handles both
      -dataTable @table.txt
      -dataTable Subj Age InputFile s01 22 s01+tlrc ...
    For the inline form there is no line structure to count columns with, so
    'InputFile' must be the LAST column, exactly as the R programs require.

    'stop_opts' is a NULL-terminated list of the calling program's option
    names; scanning of the inline form stops at the first one seen.  On return
    *nused holds the number of argv entries consumed. */
extern THD_datatable * THD_read_datatable_args( int argc , char **argv , int nopt ,
                                                char **stop_opts , int *nused ) ;

/*! Parse a flat token list, with 'InputFile' required to be last. */
extern THD_datatable * THD_parse_datatable( char **tok , int ntok , char *source ) ;

/*-- using --*/

/*! Index of a column by name, case-insensitive; -1 if there is no such one. */
extern int THD_datatable_column( THD_datatable *dt , char *name ) ;

/*! The numeric values of a named column, or NULL if it is missing or is not
    numeric.  The array belongs to the table -- do not free it. */
extern float * THD_datatable_values( THD_datatable *dt , char *name ) ;

/*! Copy named numeric columns into an owned row-major float matrix:
    result[i*ncolumn+j] corresponds to columns[j] and rows[i]. Column names
    match case-insensitively; missing, empty, repeated or nonnumeric columns
    are errors. Subj/InputFile retain their special nonnumeric status.
    rows may reorder/repeat/subset zero-based table rows. With rows==NULL,
    nrow must equal dt->nrow and all rows are copied in table order.
    The whole column must have numeric metadata; selected values must also
    be finite as floats (including after conversion from the original text).
    No centering, scaling, imputation or model fitting is performed.
    Positive row/column counts are required. Inputs are unmodified. Returns
    NULL on failure with an optional bounded diagnostic; row numbers in
    diagnostics are one-based output/table rows. Free the result with free(). */
extern float * THD_datatable_numeric_columns(
                                      const THD_datatable *dt,
                                      const char * const *columns, int ncolumn,
                                      const int *rows, int nrow,
                                      char *err, int nerr ) ;

/*! Make a deep copy containing the selected rows, in the requested order.
    Row indices are zero based.  This is useful when a long/repeated-measures
    input table must be reduced to one representative row per independent
    unit before ordinary model-column processing. */
extern THD_datatable * THD_datatable_select_rows( THD_datatable *dt ,
                                                  int *rows , int nrow ) ;

/*! Append second's rows after first's, matching columns case-insensitively.
    Both inputs must be nonempty tables with the same unique column-name set;
    their column order may differ. Output column order/spelling follows first.
    Rows (including duplicates), cell text and paths are preserved verbatim.
    Subj and InputFile are optional; no analysis-specific columns are added.
    The result owns all its storage and rebuilds numeric/subject/file caches
    from the combined cells. Inputs are unmodified and may be the same table.
    source is a synthetic description, from_argv is zero; callers needing
    per-row source provenance should add that metadata themselves.
    Returns NULL with an optional error message on schema, size or allocation
    failure. Free a successful result with THD_free_datatable(). */
extern THD_datatable * THD_datatable_concat_rows(
                                      const THD_datatable *first,
                                      const THD_datatable *second,
                                      char *err, int nerr ) ;

/*! Build and validate a complete Cartesian index over named columns.
    For dimension d, caller_nlevel[d] > 0 means caller_level[d] gives the
    required value order; THD_DT_LEVELS_FIRST discovers levels in first-
    appearance order; THD_DT_LEVELS_LEXICAL discovers and sorts them by value.
    Values are matched exactly (column names remain case-insensitive).  Empty,
    duplicate, unexpected, and missing key cells are fatal input errors. */
extern THD_datatable_index * THD_datatable_index_columns(
                                      THD_datatable *dt , int ndim ,
                                      char **columns , int *caller_nlevel ,
                                      char ***caller_level ) ;

extern void THD_free_datatable_index( THD_datatable_index *dx ) ;

/*! Print the table, for '-show_table' style debugging. */
extern void THD_datatable_print( THD_datatable *dt , FILE *fp ) ;

extern void THD_free_datatable( THD_datatable *dt ) ;

/* A square numeric table with independently labeled rows and columns.
   Both axes are aligned to label[], using exact, case-sensitive identities.
   Values are finite floats; symmetry/diagonal semantics belong to the caller.
   kind is optional # matrix_kind metadata (similarity or dissimilarity). */
typedef struct {
   int n;
   char *axis, **label, *kind;
   float *mat;                 /* row major, in label order */
} THD_labeled_matrix;

/* Read through the ordinary datatable tokenizer, including # Columns: and
   quoted/doubled-quote labels. The first column must be axis (case-sensitive).
   With order==NULL, norder must be zero and the column labels establish order.
   Otherwise both label sets must equal order exactly, without duplicates.
   Returns NULL with a bounded diagnostic for matrix-contract errors; ordinary
   table syntax/I/O errors retain THD_read_datatable_file's fatal diagnostics.
   Storage is owned by the result. No source table or file is modified. */
extern THD_labeled_matrix *THD_read_labeled_matrix(
   char *filename, const char *axis, int norder, const char * const *order,
   char *err, int nerr);
extern void THD_free_labeled_matrix(THD_labeled_matrix *matrix);

/* Quoted whitespace-table token; doubled quotes preserve literal quotes. */
extern void THD_1D_print_token(FILE *,const char *);

#endif /* _THD_DATATABLE_HEADER_ */
