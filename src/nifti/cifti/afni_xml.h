#ifndef AFNI_XML_H
#define AFNI_XML_H

#define AXML_MAX_DEPTH 16    /* maximum stack depth */
#define AXML_MAX_ELEN  128   /* maximum element length */

#define AFNI_XML_VERSION       "0.0"
#define AFNI_XML_ENCODING      "UTF-8"

/* potential encoding types, maybe with B64 */
#define AXML_ENCODING_UNDEF  0  /* undefined */
#define AXML_ENCODING_ASCII  1  /* human readable ASCII data  */
#define AXML_ENCODING_MAX    1

#include <stdio.h>

/* ----------------------------------------------------------------------
   This code is for generic reading of xml into structures.

   Basically, everything is read in (data is optional) and can be
   evaluated for structural integrity (the default).  Control options
   are provided by the afni_xml_control struct, via accessor functions.

   Much of afni_xml will be left as static.
 * ----------------------------------------------------------------------*/

typedef struct {
    char ** name;
    char ** value;
    int     length;
} nvpairs;

/* all memory implied by this structure is self contained */
typedef struct afni_xml_s {
   char               * name;        /* name of element                     */

   char               * xdata;       /* XML data, still in text             */
   int                  dlen;        /* length of (nul-terminated) XML data */
   int                  cdata;       /* flag: is data stored as CDATA       */
   int                  encode;      /* encoding type (e.g. b64 binary)     */

   int                  nchild;      /* number of child elements            */
   struct afni_xml_s ** xchild;      /* child elements                      */
   nvpairs              attrs;       /* attributes                          */
} afni_xml_t;

typedef struct {
   int           len;
   afni_xml_t ** xlist;
} afni_xml_list;
   
typedef struct {
   /* general control */
   int     verb;        /* verbose level (0=quiet, 1=default) */
   int     dstore;      /* flag: store data on read? */
   int     indent;      /* spaces to indent when writing */
   int     buf_size;    /* size of xml reading buffer */
   FILE  * stream;      /* show stream, maybe stderr */

   /* active control and information */
   int           depth;  /* current depth */
   int           dskip;  /* stack depth to skip */
   int           errors; /* reading errors */
   afni_xml_t  * stack[AXML_MAX_DEPTH+1]; /* xml stack of pointers */

   afni_xml_list * xroot;   /* list of root XML tree pointers */
} afni_xml_control;


/* protos */

/* main interface */
afni_xml_list  axml_read_file(const char * fname, int read_data);
/* afni_xml_list  axml_read_buf (const char * buf_in, int64_t blen); */

int          axml_write_stream(FILE * fp, afni_xml_t * xroot, int write_data);

int   axml_set_verb        ( int val );
int   axml_get_verb        ( void    );
int   axml_set_dstore      ( int val );
int   axml_get_dstore      ( void    );
int   axml_set_indent      ( int val );
int   axml_get_indent      ( void    );
int   axml_set_buf_size    ( int val );
int   axml_get_buf_size    ( void    );

#endif /* AFNI_XML_H */
