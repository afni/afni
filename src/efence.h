#ifdef SPARKY
#undef _POSIX_SOURCE
#endif
#include <sys/types.h>

/*
 * This is used to declare functions with "C" linkage if we are compiling
 * with C++ .
 */
#ifdef	__cplusplus
#define	C_LINKAGE	"C"
#else
#define	C_LINKAGE
#endif

void			Page_AllowAccess(void * address, size_t size);
void *			Page_Create(size_t size);
void			Page_Delete(void * address, size_t size);
void			Page_DenyAccess(void * address, size_t size);
size_t			Page_Size(void);

void			EF_Abort(const char * message, ...);
void			EF_Exit(const char * message, ...);
void			EF_Print(const char * message, ...);
