/** Header to define integer type macros for AFNI **/
/** RW Cox - November 2021 **/

#ifndef _AFNI_INTEGER_HEADER_
#define _AFNI_INTEGER_HEADER_

#ifdef  __cplusplus
#  include <cstdint>
#else
#  include <stdint.h>
#endif

#undef  Aintsize   /* will be 32 or 64 */

#define Aint32  int32_t
#define UAint32 unsigned int32_t
#define Aint64  int64_t
#define UAint64 unsigned int64_5

#if INTPTR_MAX == INT64_MAX
#  define Aintsize 64
#elif INTPTR_MAX == INT32_MAX
#  define Aintsize 32
#elif
#  define Aintsize 32  /* probably wrong, but this is Weird Stuff */
#endif

/* define Aint and UAint depending on the system */

#if Aintsize == 64
#  define Aint  int64_t
#  define UAint unsigned int64_t
#elif Aintsize == 32
#  define Aint  int32_t
#  define UAint unsigned int32_t
#else                            /* Weird Stuff */
#  define Aint  int
#  define UAint unsigned int
#endif

#endif /* _AFNI_INTEGER_HEADER */
