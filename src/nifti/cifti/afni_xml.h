#ifndef AFNI_XML_H
#define AFNI_XML_H

#define AXML_MAX_DEPTH 16    /* maximum stack depth */
#define AXML_MAX_ELEN  128   /* maximum element length */

#include <stdio.h>

/* ----------------------------------------------------------------------
   This code is for generic reading of xml into structures.

   Basically, everything is read in (data is optional) and can be
   evaluated for structural integrity (the default).  Control options
   are provided by the afni_xml_control struct, via accessor functions.

   Much of afni_xml will be left as static.
 * ----------------------------------------------------------------------*/

/* --------------------------- structures --------------------------------- */

typedef struct {
    char ** name;
    char ** value;
    int     length;
} nvpairs;

/* all memory implied by this structure is self contained */
typedef struct afni_xml_s {
   char               * name;        /* name of element                     */

   char               * xtext;       /* XML data, still in text             */
   int                  xlen;        /* length of (nul-terminated) XML data */
   int                  cdata;       /* flag: is data stored as CDATA       */
   int                  encode;      /* encoding type (e.g. b64 binary)     */

   /* these fields are not for use by afni_xml, and are left clear          */
   void               * bdata;       /* decoded binary data                 */
   int64_t              blen;        /* number of elements of btype         */
   int                  btype;       /* probably a NIFTI data type          */

   nvpairs              attrs;       /* attributes                          */
   int                  nchild;      /* number of child elements            */
   struct afni_xml_s ** xchild;      /* child elements                      */
   struct afni_xml_s  * xparent;     /* parent element                      */
} afni_xml_t;

typedef struct {
   int           len;
   afni_xml_t ** xlist;
} afni_xml_list;
   
typedef struct {
   /* general control */
   int     verb;        /* verbose level (0=quiet, 1=default) */
   int     dstore;      /* flag: store data on read?         */
   int     indent;      /* spaces to indent when writing    */
   int     buf_size;    /* size of xml reading buffer      */
   FILE  * wstream;     /* show stream, maybe stderr      */

   /* active control and information                                     */
   int           depth;  /* current depth                                */
   int           dskip;  /* stack depth to skip                          */
   int           errors; /* reading errors                               */
   int           wkeep;  /* flag: keep found whitespace char             */
                         /* (keep once non-white is seen, until any pop) */
   afni_xml_t  * stack[AXML_MAX_DEPTH]; /* xml stack of pointers         */

   afni_xml_list * xroot;   /* list of root XML tree pointers            */
} afni_xml_control;


/* --------------------------- prototypes --------------------------------- */

/* main interface */
afni_xml_list axml_read_buf (const char * buf_in, int64_t bin_len);
afni_xml_list axml_read_file(const char * fname, int read_data);

int axml_disp_xlist(char * mesg, afni_xml_list * axlist, int verb);
int axml_disp_xml_t(char * mesg, afni_xml_t * ax, int indent, int verb);


/* create/free */
afni_xml_t * new_afni_xml   (const char * name);
int          axml_add_attrs (afni_xml_t * ax, const char ** attr);
int          axml_free_xml_t(afni_xml_t * ax);
int          axml_free_xlist(afni_xml_list * axlist);

char * axml_attr_value(afni_xml_t * ax, const char * name);
int    axml_recur(int(*func)(FILE*,afni_xml_t*,int), afni_xml_t * root);


/* control API */
int    axml_set_verb        ( int val  );
int    axml_get_verb        ( void     );
int    axml_set_dstore      ( int val  );
int    axml_get_dstore      ( void     );
int    axml_set_indent      ( int val  );
int    axml_get_indent      ( void     );
int    axml_set_buf_size    ( int val  );
int    axml_get_buf_size    ( void     );
int    axml_set_wstream     ( FILE *fp );
FILE * axml_get_wstream     ( void     );

#endif /* AFNI_XML_H */
