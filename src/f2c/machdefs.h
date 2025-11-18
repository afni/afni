#if 1  /* this change for compiling on 32 or 64 bit systems */
# define SZADDR   sizeof(void *)
# define SZSHORT  sizeof(short)
# define SZINT    sizeof(int)
# define SZLONG   SZINT
# define SZDREAL  sizeof(double)
#else
# define SZADDR   4
# define SZSHORT  2
# define SZINT    4
# define SZLONG   4
# define SZDREAL  8
#endif

#define TYLENG   TYLONG      /* char string length field */
#define TYINT    TYLONG
#define SZLENG   SZLONG

/* Alignment restrictions */

#define ALIADDR   SZADDR
#define ALISHORT  SZSHORT
#define ALILONG   SZLONG
#define ALIDOUBLE SZDREAL
#define ALIINT    ALILONG
#define ALILENG   ALILONG

#define BLANKCOMMON "_BLNK__"   /* Name for the unnamed
                                   common block; this is unique
                                   because of underscores */

#define LABELFMT "%s:\n"

#define MAXREGVAR 4
#define TYIREG    TYLONG
#define MSKIREG   (M(TYSHORT)|M(TYLONG))   /* allowed types of DO indicies
                                              which can be put in registers */
