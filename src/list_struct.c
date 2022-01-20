
#include "stdlib.h"
#include "string.h"
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
 *   int init_floatp_list( floatp_list * d_list, int nel );
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
 *       (float *).
 *  
 *   int free_float_list( float_list * d_list );
 *  
 *       This function will free d_list->list, and set num and nall to 0.
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

/* string_list: memory allocation is optional        30 Jun 2014 [rickr] */
/* alloc: flag to allocate string memory, does not affect init           */
int init_string_list( string_list * d_list, int nel, int alloc )
{
    int ind;

    if ( !d_list ) return -1;

    if ( nel <= 0 ) {
        d_list->num = 0;  d_list->nall = 0;  d_list->list = NULL;
        return 0;
    }

    d_list->list = (char **)malloc(nel * sizeof(char *));  /* allocate memory */
    if ( d_list->list == NULL ) return -1;                 /* malloc failure  */

    /* init to NULL, just to be safe */
    for( ind = 0; ind < nel; ind++ ) d_list->list[ind] = NULL;

    d_list->num  = 0;
    d_list->nall = nel;
    d_list->alloc  = alloc;

    return nel;
}


/*----------------------------------------------------------------------
 * pointer lists:
 *----------------------------------------------------------------------*/
int init_floatp_list( floatp_list * d_list, int nel )
{
    if ( !d_list ) return -1;

    /* an 'empty' structure will contain 0 and NULL field entries */
    if ( nel <= 0 ) {
        d_list->num = d_list->nall = 0;
        d_list->list  = NULL;
        return 0;
    }

    d_list->list = (float **)malloc(nel * sizeof(float *));/* allocate memory */

    if ( d_list->list == NULL ) return -1;                 /* malloc failure  */

    d_list->num = 0;                                     /* none used yet   */
    d_list->nall = nel;                     /* number of pointers allocated */

    return nel;
}

int init_intp_list( intp_list * d_list, int nel )
{
    if ( !d_list ) return -1;

    /* an 'empty' structure will contain 0 and NULL field entries */
    if ( nel <= 0 ) {
        d_list->num = d_list->nall = 0;
        d_list->list  = NULL;
        return 0;
    }

    d_list->list = (int **)malloc(nel * sizeof(int *));    /* allocate memory */

    if ( d_list->list == NULL ) return -1;                 /* malloc failure  */

    d_list->num = 0;                                       /* none used yet */
    d_list->nall = nel;                     /* number of pointers allocated */

    return nel;
}

int init_shortp_list( shortp_list * d_list, int nel )
{
    if ( !d_list ) return -1;

    /* an 'empty' structure will contain 0 and NULL field entries */
    if ( nel <= 0 ) {
        d_list->num = d_list->nall = 0;
        d_list->list  = NULL;
        return 0;
    }

    d_list->list = (short **)malloc(nel * sizeof(short *));/* allocate memory */

    if ( d_list->list == NULL ) return -1;                 /* malloc failure  */

    d_list->num = 0;                                       /* none used yet */
    d_list->nall = nel;                     /* number of pointers allocated */

    return nel;
}

int init_voidp_list( voidp_list * d_list, int nel )
{
    if ( !d_list ) return -1;

    /* an 'empty' structure will contain 0 and NULL field entries */
    if ( nel <= 0 ) {
        d_list->num = d_list->nall = 0;
        d_list->list  = NULL;
        return 0;
    }

    d_list->list = (void **)malloc(nel * sizeof(void *));  /* allocate memory */

    if ( d_list->list == NULL ) return -1;                 /* malloc failure  */

    d_list->num = 0;                                       /* none used yet */
    d_list->nall = nel;                     /* number of pointers allocated */

    return nel;
}

/*----------------------------------------------------------------------
 * add_to_XXXX_list:                                 4 Nov, 2011 [rickr]
 *  
 *   input: structure pointer, value, list increment size
 *  
 *   return:
 *       success: nel (>= 1)
 *----------------------------------------------------------------------*/
int add_to_float_list( float_list * d_list, float val, int inc_size )
{
    int llen;

    if ( !d_list ) return -1;

    /* maybe we need more space */
    if ( d_list->num >= d_list->nall ) {
        if (inc_size <= 0) llen = d_list->nall + 1;
        else               llen = d_list->nall + inc_size;
        d_list->nall = llen;
        d_list->list = (float *)realloc(d_list->list, llen*sizeof(float));
        if( !d_list->list ) return -1;
    }

    d_list->list[d_list->num++] = val;

    return d_list->num;
}

int add_to_int_list( int_list * d_list, int val, int inc_size )
{
    int llen;

    if ( !d_list ) return -1;

    /* maybe we need more space */
    if ( d_list->num >= d_list->nall ) {
        if (inc_size <= 0) llen = d_list->nall + 1;
        else               llen = d_list->nall + inc_size;
        d_list->nall = llen;
        d_list->list = (int *)realloc(d_list->list, llen*sizeof(int));
        if( !d_list->list ) return -1;
    }

    d_list->list[d_list->num++] = val;

    return d_list->num;
}

/* must also allocate for passed string */
int add_to_string_list( string_list * d_list, char * val, int inc_size )
{
    int llen, nadd, ind;

    if ( !d_list ) return -1;

    /* maybe we need more space */
    if ( d_list->num >= d_list->nall ) {
        /* note new size for NULL init */
        if (inc_size <= 0) nadd = 1;
        else               nadd = inc_size;
        llen = d_list->nall + nadd;
        d_list->list = (char **)realloc(d_list->list, llen*sizeof(char *));
        if( !d_list->list ) return -1;
        /* init to NULL */
        for( ind = 0; ind < nadd; ind++ ) d_list->list[ind+d_list->nall] = NULL;
        d_list->nall = llen;
    }

    /* allocate memory or just copy pointer (copy includes NULL case) */
    if( val && d_list->alloc ) d_list->list[d_list->num++] = strdup(val);
    else                       d_list->list[d_list->num++] = val;

    return d_list->num;
}


/*----------------------------------------------------------------------
 * resize_XXXX_list:                                         20 Aug 2014
 *
 * reset nalloc to the given length, return new length
 *----------------------------------------------------------------------*/
int resize_int_list(int_list * L, int len)
{
    int oldlen;

    if( !L || len < 0 )  return -1;
    if( len == 0 )       return free_int_list(L);   /* nuke */
    if( len == L->nall ) return len;                /* already done */

    L->list = (int *)realloc(L->list, len*sizeof(int));
    if( !L->list ) return -1;

    /* either clear new memory or reset used length */
    if(len > L->nall) memset(L->list+L->nall, '\0', (len-L->nall)* sizeof(int));
    else              L->num = len;

    L->nall = len;

    return len;
}



/*----------------------------------------------------------------------
 * extend_XXXX_list:                                         26 Apr 2012
 *
 * extend first list by another, returning the new length (or -1 on error)
 *----------------------------------------------------------------------*/
int extend_int_list( int_list * Ldest, int_list * Lsrc )
{
    int newlen;

    if ( !Ldest || !Lsrc ) return -1;

    newlen = Ldest->num + Lsrc->num;

    /* maybe we need more space */
    if ( newlen >= Ldest->nall ) {
        Ldest->nall = newlen;
        Ldest->list = (int *)realloc(Ldest->list, newlen*sizeof(int));
        if( !Ldest->list ) return -1;
    }

    /* now append Lsrc to Ldest */
    memcpy(Ldest->list+Ldest->num, Lsrc->list, Lsrc->num * sizeof(int));
    Ldest->num = newlen;

    return Ldest->num;
}


int extend_string_list( string_list * Ldest, string_list * Lsrc )
{
    int newlen, ind;

    if ( !Ldest || !Lsrc ) return -1;

    newlen = Ldest->num + Lsrc->num;

    /* maybe we need more space */
    if ( newlen >= Ldest->nall ) {
        Ldest->nall = newlen;
        Ldest->list = (char **)realloc(Ldest->list, newlen*sizeof(char *));
        if( !Ldest->list ) return -1;
    }

    /* now append Lsrc to Ldest (need to dupe strings) */
    for( ind = 0; ind < Lsrc->num; ind++ )
       if( add_to_string_list(Ldest, Lsrc->list[ind], 0) < 0 ) return -1;
    Ldest->num = newlen;

    return Ldest->num;
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

/* keep list intact, but clear values */
int clear_float_list( float_list * d_list )
{
    int ind;

    if ( ! d_list ) return -1;
    if ( d_list->nall <= 0 || ! d_list->list ) return 0;

    memset(d_list->list, '\0', d_list->nall*sizeof(float));
    d_list->num = 0;

    return 0;
}

/* keep list intact, but clear values */
int clear_int_list( int_list * d_list )
{
    int ind;

    if ( ! d_list ) return -1;
    if ( d_list->nall <= 0 || ! d_list->list ) return 0;

    memset(d_list->list, '\0', d_list->nall*sizeof(int));
    d_list->num = 0;

    return 0;
}

int free_short_list( short_list * d_list )
{
    if ( !d_list ) return -1;

    if ( d_list->list ) { free(d_list->list);  d_list->list = NULL; }
    d_list->num = d_list->nall = 0;

    return 0;
}

/* keep list intact, but free/clear any strings */
int clear_string_list( string_list * d_list )
{
    int ind;

    if ( ! d_list ) return -1;

    if ( d_list->list ) {
       for( ind = 0; ind < d_list->num; ind++ ) {
          if( d_list->list[ind] ) {
             if( d_list->alloc ) free(d_list->list[ind]);
             d_list->list[ind] = NULL;
          }
       }
    }

    d_list->num = 0;

    return 0;
}

int free_string_list( string_list * d_list )
{
    int ind;

    if ( ! d_list ) return -1;

    if ( d_list->list ) {
       if( d_list->alloc ) { /* then try to free */
          for( ind = 0; ind < d_list->num; ind++ )
             if( d_list->list[ind] ) free(d_list->list[ind]);
       }
       free(d_list->list);  d_list->list = NULL;
    }

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
    if ( !d_list ) return -1;

    /* this is bad, but nothing to do */
    if ( d_list->list ) free(d_list->list);

    d_list->list = NULL;
    d_list->num = d_list->nall = 0;

    return 0;
}

int free_intp_list( intp_list * d_list )
{
    if ( !d_list ) return -1;

    if ( d_list->list ) free(d_list->list);

    d_list->list = NULL;
    d_list->num = d_list->nall = 0;

    return 0;
}

int free_shortp_list( shortp_list * d_list )
{
    if ( !d_list ) return -1;

    if ( d_list->list ) free(d_list->list);

    d_list->list = NULL;
    d_list->num = d_list->nall = 0;

    return 0;
}

int free_voidp_list( voidp_list * d_list )
{
    if ( !d_list ) return -1;

    if ( d_list->list ) free(d_list->list);

    d_list->list = NULL;
    d_list->num = d_list->nall = 0;

    return 0;
}

