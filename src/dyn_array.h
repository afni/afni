#ifndef _RWC_DYN_ARRAY_HEADER_
#define _RWC_DYN_ARRAY_HEADER_

/*--------------------------------------------------------------------------*/
/*! Set of macros to define and use dynamic arrays.
     * If the array element type is not defined by one symbol,
        then you must typedef it, as in "typedef char * str".
     * DECLARE_ARRAY_TYPE(str) then typedefs the new
        type "str_array".
     * To declare a variable of type "str_array *", you can
        either do "str_array *sar", or "DECLARE_ARRAY(sar,str)".
     * To initialize this variable to point to the empty array,
        you do "INIT_ARRAY(sar,str)".
     * To add an element to the array, do "ADDTO_ARRAY(sar,str,val)"
        where "val" is assigned to the newly allocated last
        location in the array.  Since this is an assignment, if
        "val" is a pointer, it should be to malloc()-ed space,
        not to a local variable.
     * To free an array that is composed of un-malloc()-ed elements,
        use "DELETE_ARRAY(sar)".  To free an array that is composed
        of malloc()-ed element, use "FREE_ARRAY(sar)", which will
        call free() on each element of the array before deleting
        the array struct itself.  To free an array that requires
        a more elaborate destructor for each element, use
        "KILL_ARRAY(sar,kfunc)", where kfunc() will be called
        on each element of the array before the array struct
        is deleted.
     * The number of elements in an array is "sar->num".
     * The i-th element in an array is "sar->ar[i]",
        for i=0 .. sar->num-1.
----------------------------------------------------------------------------*/

#include <stdlib.h>  /* for malloc */

/*! Declare an array type of "typ_array". */

#define DECLARE_ARRAY_TYPE(typ)                                 \
  typedef struct { int num ; typ * ar ; } typ ## _array

/*! Declare a pointer of type "typ_array *". */

#define DECLARE_ARRAY(anam,typ) typ ## _array * anam

/*! Initialize an array pointer of type "typ_array *". */

#define INIT_ARRAY(anam,typ)                                    \
 do{ anam = malloc(sizeof(typ ## _array *)) ;                   \
     anam->num = 0 ; anam->ar = NULL ;       } while(0)

/*! Append element "val" to an array pointer "anam" of type "typ_array *". */

#define ADDTO_ARRAY(anam,typ,val)                               \
 do{ int n=anam->num ;                                          \
     anam->ar = realloc(anam->ar,sizeof(typ)*(n+1)) ;           \
     anam->ar[n] = (val) ; anam->num++ ;             } while(0)

/*! Delete the array pointed to by "anam". */

#define DELETE_ARRAY(anam)                                      \
 do{ free(anam->ar) ; free(anam) ; anam = NULL ; } while(0)

/*! Apply "killer()" to each element of the array
    pointed to by "anam", then delete anam itself. */

#define KILL_ARRAY(anam,killer)                                 \
 do{ int i ;                                                    \
     for( i=0 ; i < anam->num ; i++ ) killer(anam->ar[i]) ;     \
     free(anam->ar) ; free(anam) ; anam = NULL ;                \
 } while(0)

/*! Apply free() to each element of the array
    pointed to by "anam", then delete anam itself. */

#define FREE_ARRAY(anam) KILL_ARRAY(anam,free)

/*============================================*/
/* Sample program that uses the macros above. */
/*============================================*/

#if 0
typedef char * str ;      /* declare this type */

DECLARE_ARRAY_TYPE(int) ; /* declare type of int_array */
DECLARE_ARRAY_TYPE(str) ; /* declare type of str_array */

int main( int argc , char *argv[] )
{
   DECLARE_ARRAY(iar,int) ; /* declare iar to be of type int_array * */
   DECLARE_ARRAY(sar,str) ; /* declare sar to be of type str_array * */
   int ii ;
   char buf[32] , *tb ;

   INIT_ARRAY(iar,int) ;    /* initialize the two arrays */
   INIT_ARRAY(sar,str) ;

   /* fill the two arrays with some stuff */

   for( ii=0 ; ii < 9 ; ii++ ){
      ADDTO_ARRAY(iar,int,2*ii+7) ;  /* just assign a value to int_array */

      /* for str_array, we are assigning pointers,
         so each one must be unique; we do this by using strdup() */

      sprintf(buf,"%05d",2*ii+7) ; tb = strdup(buf) ;
      ADDTO_ARRAY(sar,str,tb) ;
   }

   /* print out info from each array */

   printf("iar->num=%d  sar->num=%d\n",iar->num,sar->num) ;

   for( ii=0 ; ii < 9 ; ii++ )
      printf(" iar[%d]=%d   sar[%d]=%s\n",
             ii,iar->ar[ii] , ii,sar->ar[ii] ) ;

   /* erase the two arrays */

   DELETE_ARRAY(iar) ;   /* can just delete the array struct itself */
   FREE_ARRAY(sar) ;     /* must use free() on each element first */

   exit(0) ;
}
#endif

#endif /* _RWC_DYN_ARRAY_HEADER_ */
