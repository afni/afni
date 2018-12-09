#ifndef _LIST_STRUCT_H_
#define _LIST_STRUCT_H_

/*----------------------------------------------------------------------
 * list_struct.h      - for creating and destroying lists
 *
 * I tried to follow Bob's structure and element naming style.
 *
 * Rick Reynolds - December 6, 2004
 *----------------------------------------------------------------------*/

/* value lists include number used, number allocated, and the list */
typedef struct { int num,nall;        float *  list; } float_list;
typedef struct { int num,nall;        int   *  list; } int_list;
typedef struct { int num,nall;        short *  list; } short_list;
typedef struct { int num,nall,alloc;  char  ** list; } string_list;

/* pointer lists also include an element length field (# elements allocated) */
typedef struct { int num,nall;  float ** list; } floatp_list;
typedef struct { int num,nall;  int   ** list; } intp_list;
typedef struct { int num,nall;  short ** list; } shortp_list;
typedef struct { int num,nall;  void  ** list; } voidp_list;

/* create lists of the requested number of elements */
int init_float_list  ( float_list  * d_list, int nel );
int init_int_list    ( int_list    * d_list, int nel );
int init_short_list  ( short_list  * d_list, int nel );
int init_string_list ( string_list * d_list, int nel, int alloc );

/* create nel lists, of length len */
int init_floatp_list ( floatp_list * d_list, int nel );
int init_intp_list   ( intp_list   * d_list, int nel );
int init_shortp_list ( shortp_list * d_list, int nel );
int init_voidp_list  ( voidp_list  * d_list, int nel );

/* add to lists, possibly increment length */
int add_to_float_list ( float_list  * d_list, float  val, int inc_size );
int add_to_int_list   ( int_list    * d_list, int    val, int inc_size );
int add_to_string_list( string_list * d_list, char * val, int inc_size );

/* resize list allocation*/
int resize_int_list  ( int_list * L, int len );

/* add to lists, possibly increment length */
int extend_int_list  ( int_list * Ldest, int_list * Lsrc );
int extend_str_list  ( int_list * Ldest, int_list * Lsrc );

/* other */

/* free simple lists and clear structures */
int free_float_list  ( float_list  * d_list );
int free_int_list    ( int_list    * d_list );
int free_short_list  ( short_list  * d_list );
int free_string_list ( string_list * d_list );

int clear_int_list   ( int_list    * d_list );
int clear_string_list( string_list * d_list );

/* free list lists, and clear structures */
int free_floatp_list ( floatp_list * d_list );
int free_intp_list   ( intp_list   * d_list );
int free_shortp_list ( shortp_list * d_list );
int free_voidp_list  ( voidp_list  * d_list );

#endif
