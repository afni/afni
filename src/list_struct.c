
#include "stdlib.h"
#include "list_struct.h"


/*----------------------------------------------------------------------
 *
 * list_struct.c            - for creating and destroying list structures
 * 
 * each list structure contains:
 *
 *   num   - the number of elements used       (the user may alter this number)
 *   nall  - the number of elements allocated
 *   list  - the 'list', the list of elements (the user may alter these values)
 *
 * each pointer list structure also contains:
 *
 *   elen  - the length of each list in the main array
 *
 * Rick Reynolds - December 6, 2004
 *----------------------------------------------------------------------*/


/*----------------------------------------------------------------------
 * sample struct and prototypes:
 *
 *   I will use float for examples, noting that float may be replaced
 *   with any of int, short and void, also.
 *  
 *   typedef struct { int num,nall;  float *  list; } float_list;
 *
 *   int init_float_list ( float_list  * d_list, int nel );
 *   int init_floatp_list( floatp_list * d_list, int nel, int len );
 *   int free_float_list ( float_list  * d_list );
 *   int free_floatp_list( floatp_list * d_list );
 *  
 * brief sample function descriptions:
 *  
 *   Note that all of these functions set d_list->num to 0.  That
 *   variable is for the user to apply, if they wish.
 *  
 *   int init_float_list( float_list * d_list, int nel );
 *
 *       The user should pass a (useless) float_list structure.
 *       This function will set num to 0, nall to the input nel
 *       and allocate an array of length nel.
 *  
 *       This will return nel (>= 0) on success, and < 0 on failure.
 *  
 *   int init_floatp_list( floatp_list * d_list, int nel, int len );
 *  
 *       Like above, but now d_list->list will be an array of
 *       (float *).  Also, if len > 0, each list[i] will be
 *       allocated to an array of len floats.
 *  
 *   int free_float_list( float_list * d_list );
 *  
 *       This function will free d_list->list, and set num and nall to 0.
 *  
 *   int free_floatp_list( floatp_list * d_list );
 *  
 *       Like free_float_list, but before free(d_list->list), we must
 *       free(d_list->list[i]), for each i.
 *  
 *----------------------------------------------------------------------*/


/*----------------------------------------------------------------------
 * init_XXXX_list:
 *  
 *   input: structure pointer and number of elements
 *  
 *   if nel <= 0, set all fields to zero (or NULL)
 *   if nel >  0, attempt to malloc the requested number of elements
 *  
 *   return:
 *       success: nel (>= 0)
 *       failure: < 0
 *----------------------------------------------------------------------*/
int init_float_list( float_list * d_list, int nel )
{
    if ( !d_list ) return -1;

    if ( nel <= 0 ) {
        d_list->num = 0;  d_list->nall = 0;  d_list->list = NULL;
        return 0;
    }

    d_list->list = (float *)malloc(nel * sizeof(float));   /* allocate memory */

    if ( d_list->list == NULL ) return -1;                 /* malloc failure  */

    d_list->num = 0;
    d_list->nall = nel;

    return nel;
}

int init_int_list( int_list * d_list, int nel )
{
    if ( !d_list ) return -1;

    if ( nel <= 0 ) {
        d_list->num = 0;  d_list->nall = 0;  d_list->list = NULL;
        return 0;
    }

    d_list->list = (int *)malloc(nel * sizeof(int));       /* allocate memory */

    if ( d_list->list == NULL ) return -1;                 /* malloc failure  */

    d_list->num = 0;
    d_list->nall = nel;

    return nel;
}

int init_short_list( short_list * d_list, int nel )
{
    if ( !d_list ) return -1;

    if ( nel <= 0 ) {
        d_list->num = 0;  d_list->nall = 0;  d_list->list = NULL;
        return 0;
    }

    d_list->list = (short *)malloc(nel * sizeof(short));   /* allocate memory */

    if ( d_list->list == NULL ) return -1;                 /* malloc failure  */

    d_list->num = 0;
    d_list->nall = nel;

    return nel;
}

int init_void_list( void_list * d_list, int nel )
{
    if ( !d_list ) return -1;

    if ( nel <= 0 ) {
        d_list->num = 0;  d_list->nall = 0;  d_list->list = NULL;
        return 0;
    }

    /* special case, list is considered a list of bytes, so use char */
    d_list->list = (void *)malloc(nel * sizeof(char));     /* allocate memory */

    if ( d_list->list == NULL ) return -1;                 /* malloc failure  */

    d_list->num = 0;
    d_list->nall = nel;

    return nel;
}

/*----------------------------------------------------------------------
 * pointer lists:
 *
 * in addition to what is above, pass the list length 
 *
 * after allocating the nel pointers,
 * len elements will be allocated to each pointer
 *
 * the return values are the same
 *----------------------------------------------------------------------*/
int init_floatp_list( floatp_list * d_list, int nel, int len )
{
    int count;

    if ( !d_list ) return -1;

    /* an 'empty' structure will contain 0 and NULL field entries */
    if ( nel <= 0 ) {
        d_list->num = d_list->nall = d_list->elen = 0;
        d_list->list  = NULL;
        return 0;
    }

    d_list->list = (float **)malloc(nel * sizeof(float *));/* allocate memory */

    if ( d_list->list == NULL ) return -1;                 /* malloc failure  */

    d_list->num = 0;                                     /* none used yet   */
    d_list->nall = nel;                     /* number of pointers allocated */
    d_list->elen = len;                             /* length of each list */


    /* now repeat the process for each pointer, allocating 'len' elements    */

    /* trivial case, where the user requests no list allocation */
    if ( len <= 0 ){
        d_list->elen = 0;                   /* number of elements allocated */
        for ( count = 0; count < nel; count++ )
             d_list->list[count] = NULL;
        return nel;
    }

    /* general case, the user wants data, too */
    for ( count = 0; count < nel; count++ ){
        d_list->list[count] = malloc(len * sizeof(float));

        /* on malloc failure, free() everything else, of course */
        if ( d_list->list[count] == NULL ){
            while ( --count >= 0 ) free(d_list->list[count]);
            free(d_list->list);
            return -1;
        }
    }

    return nel;
}

int init_intp_list( intp_list * d_list, int nel, int len )
{
    int count;

    if ( !d_list ) return -1;

    /* an 'empty' structure will contain 0 and NULL field entries */
    if ( nel <= 0 ) {
        d_list->num = d_list->nall = d_list->elen = 0;
        d_list->list  = NULL;
        return 0;
    }

    d_list->list = (int **)malloc(nel * sizeof(int *));    /* allocate memory */

    if ( d_list->list == NULL ) return -1;                 /* malloc failure  */

    d_list->num = 0;                                       /* none used yet */
    d_list->nall = nel;                     /* number of pointers allocated */
    d_list->elen = len;                             /* length of each list */


    /* now repeat the process for each pointer, allocating 'len' elements    */

    /* trivial case, where the user requests no list allocation */
    if ( len <= 0 ){
        d_list->elen = 0;                   /* number of elements allocated */
        for ( count = 0; count < nel; count++ )
             d_list->list[count] = NULL;
        return nel;
    }

    /* general case, the user wants data, too */
    for ( count = 0; count < nel; count++ ){
        d_list->list[count] = malloc(len * sizeof(int));

        /* on malloc failure, free() everything else, of course */
        if ( d_list->list[count] == NULL ){
            while ( --count >= 0 ) free(d_list->list[count]);
            free(d_list->list);
            return -1;
        }
    }

    return nel;
}

int init_shortp_list( shortp_list * d_list, int nel, int len )
{
    int count;

    if ( !d_list ) return -1;

    /* an 'empty' structure will contain 0 and NULL field entries */
    if ( nel <= 0 ) {
        d_list->num = d_list->nall = d_list->elen = 0;
        d_list->list  = NULL;
        return 0;
    }

    d_list->list = (short **)malloc(nel * sizeof(short *));/* allocate memory */

    if ( d_list->list == NULL ) return -1;                 /* malloc failure  */

    d_list->num = 0;                                       /* none used yet */
    d_list->nall = nel;                     /* number of pointers allocated */
    d_list->elen = len;                             /* length of each list */


    /* now repeat the process for each pointer, allocating 'len' elements    */

    /* trivial case, where the user requests no list allocation */
    if ( len <= 0 ){
        d_list->elen = 0;                   /* number of elements allocated */
        for ( count = 0; count < nel; count++ )
             d_list->list[count] = NULL;
        return nel;
    }

    /* general case, the user wants data, too */
    for ( count = 0; count < nel; count++ ){
        d_list->list[count] = malloc(len * sizeof(short));

        /* on malloc failure, free() everything else, of course */
        if ( d_list->list[count] == NULL ){
            while ( --count >= 0 ) free(d_list->list[count]);
            free(d_list->list);
            return -1;
        }
    }

    return nel;
}

int init_voidp_list( voidp_list * d_list, int nel, int len )
{
    int count;

    if ( !d_list ) return -1;

    /* an 'empty' structure will contain 0 and NULL field entries */
    if ( nel <= 0 ) {
        d_list->num = d_list->nall = d_list->elen = 0;
        d_list->list  = NULL;
        return 0;
    }

    d_list->list = (void **)malloc(nel * sizeof(void *));  /* allocate memory */

    if ( d_list->list == NULL ) return -1;                 /* malloc failure  */

    d_list->num = 0;                                       /* none used yet */
    d_list->nall = nel;                     /* number of pointers allocated */
    d_list->elen = len;                             /* length of each list */


    /* now repeat the process for each pointer, allocating 'len' elements    */

    /* trivial case, where the user requests no list allocation */
    if ( len <= 0 ){
        d_list->elen = 0;                   /* number of elements allocated */
        for ( count = 0; count < nel; count++ )
             d_list->list[count] = NULL;
        return nel;
    }

    /* general case, the user wants data, too */
    for ( count = 0; count < nel; count++ ){
        /* again, the special case for void * is a list of bytes */
        d_list->list[count] = malloc(len * sizeof(char));

        /* on malloc failure, free() everything else, of course */
        if ( d_list->list[count] == NULL ){
            while ( --count >= 0 ) free(d_list->list[count]);
            free(d_list->list);
            return -1;
        }
    }

    return nel;
}

/*----------------------------------------------------------------------
 * free_XXXX_list:
 *
 * free all lists
 *
 * return 0 on success, -1 on error
 *----------------------------------------------------------------------*/
int free_float_list( float_list * d_list )
{
    if ( !d_list ) return -1;

    if ( d_list->list ) { free(d_list->list);  d_list->list = NULL; }
    d_list->num = d_list->nall = 0;

    return 0;
}

int free_int_list( int_list * d_list )
{
    if ( !d_list ) return -1;

    if ( d_list->list ) { free(d_list->list);  d_list->list = NULL; }
    d_list->num = d_list->nall = 0;

    return 0;
}

int free_short_list( short_list * d_list )
{
    if ( !d_list ) return -1;

    if ( d_list->list ) { free(d_list->list);  d_list->list = NULL; }
    d_list->num = d_list->nall = 0;

    return 0;
}

int free_void_list( void_list * d_list )
{
    if ( !d_list ) return -1;

    if ( d_list->list ) { free(d_list->list);  d_list->list = NULL; }
    d_list->num = d_list->nall = 0;

    return 0;
}


/*----------------------------------------------------------------------
 * free_XXXXp_list:
 *
 * free all sub-lists and lists
 *
 * return 0 on success, -1 on error
 *----------------------------------------------------------------------*/
int free_floatp_list( floatp_list * d_list )
{
    int count;

    if ( !d_list ) return -1;

    /* if nothing has been set, just clear values and return */
    if ( d_list->num <= 0 ){
        d_list->num = d_list->nall = d_list->elen = 0;
        d_list->list = NULL;
        return 0;
    }

    /* this is bad, but nothing to do */
    if ( !d_list->list ){
        d_list->num = d_list->nall = d_list->elen = 0;
        return -1;
    }

    /* first, free the nall lists (of length elen) */

    for ( count = 0; count < d_list->nall; count++ )
        if ( d_list->list[count] ) free(d_list->list[count]);

    /* now free the list and clear all values */
    free(d_list->list);
    d_list->list = NULL;

    d_list->num = d_list->nall = d_list->elen = 0;

    return 0;
}

int free_intp_list( intp_list * d_list )
{
    int count;

    if ( !d_list ) return -1;

    /* if nothing has been set, just clear values and return */
    if ( d_list->num <= 0 ){
        d_list->num = d_list->nall = d_list->elen = 0;
        d_list->list = NULL;
        return 0;
    }

    /* this is bad, but nothing to do */
    if ( !d_list->list ){
        d_list->num = d_list->nall = d_list->elen = 0;
        return -1;
    }

    /* first, free the nall lists (of length elen) */

    for ( count = 0; count < d_list->nall; count++ )
        if ( d_list->list[count] ) free(d_list->list[count]);

    /* now free the list and clear all values */
    free(d_list->list);
    d_list->list = NULL;

    d_list->num = d_list->nall = d_list->elen = 0;

    return 0;
}

int free_shortp_list( shortp_list * d_list )
{
    int count;

    if ( !d_list ) return -1;

    /* if nothing has been set, just clear values and return */
    if ( d_list->num <= 0 ){
        d_list->num = d_list->nall = d_list->elen = 0;
        d_list->list = NULL;
        return 0;
    }

    /* this is bad, but nothing to do */
    if ( !d_list->list ){
        d_list->num = d_list->nall = d_list->elen = 0;
        return -1;
    }

    /* first, free the nall lists (of length elen) */

    for ( count = 0; count < d_list->nall; count++ )
        if ( d_list->list[count] ) free(d_list->list[count]);

    /* now free the list and clear all values */
    free(d_list->list);
    d_list->list = NULL;

    d_list->num = d_list->nall = d_list->elen = 0;

    return 0;
}

int free_voidp_list( voidp_list * d_list )
{
    int count;

    if ( !d_list ) return -1;

    /* if nothing has been set, just clear values and return */
    if ( d_list->num <= 0 ){
        d_list->num = d_list->nall = d_list->elen = 0;
        d_list->list = NULL;
        return 0;
    }

    /* this is bad, but nothing to do */
    if ( !d_list->list ){
        d_list->num = d_list->nall = d_list->elen = 0;
        return -1;
    }

    /* first, free the nall lists (of length elen) */

    for ( count = 0; count < d_list->nall; count++ )
        if ( d_list->list[count] ) free(d_list->list[count]);

    /* now free the list and clear all values */
    free(d_list->list);
    d_list->list = NULL;

    d_list->num = d_list->nall = d_list->elen = 0;

    return 0;
}

