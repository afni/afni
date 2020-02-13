#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <expat.h>
#include <inttypes.h>
#include "afni_xml.h"

#define AXML_MIN_BSIZE 2048
#define AXML_DEF_BSIZE 32768

/* local prototypes */

#ifndef XMLCALL
/* XMLCALL was added to expat in version 1.95.7 to define a calling convention,
 * as cdecl
 */
#if defined(XML_USE_MSC_EXTENSIONS)
#define XMLCALL __cdecl
#elif defined(__GNUC__) && defined(__i386)
#define XMLCALL __attribute__((cdecl))
#else
#define XMLCALL
#endif
#endif /* not defined XMLCALL */

#ifndef XML_STATUS_ERROR
#define XML_STATUS_ERROR 0
#endif
#ifndef XML_STATUS_OK
#define XML_STATUS_OK 1
#endif


/*
 * realloc does not clear memory of ptr on failure.
 * This wrapper function checks the return status,
 * and deletes ptr if reallocation fails.
 */
static void * safe_realloc(void * ptr , size_t size)
{
  void * ptr_realloc = realloc( ptr, size );
  if(!ptr_realloc)
  {
    free(ptr);
  }
  return ptr_realloc;
}

/* ---------------------------------------------------------------------- */
/* XML global struct and access functions                                 */

static afni_xml_control gAXD = {
   1,          /* verb, default to 1 (0 means quiet)         */
   1,          /* dstore, flag whether to store data         */
   3,          /* indent, spaces per indent level            */
   AXML_DEF_BSIZE, /* buf_size, allocated for XML parsing    */
   NULL,       /* wstream                                    */

   0,          /* depth, current stack depth                 */
   0,          /* dskip depth (at positive depth, 0 is clear)*/
   0,          /* errors, number of encountered errors       */
   0,          /* wkeep, keep white until pop                */
   {NULL},     /* stack, of afni_xml_t *                     */

   NULL,       /* afni_xml_list * xroot                      */
};


/*---------- accessor functions for user-controllable variables ----------*/
/*----------     (one can pass -1 to set the default value)     ----------*/

/*! verb is the verbose level, with 0 meaning "only report errors" (up to 7) */
int axml_get_verb( void    ){ return gAXD.verb; }
int axml_set_verb( int val )
{
    if     ( val == -1 ) gAXD.verb = 1;
    else if( val >=  0 ) gAXD.verb = val;
    return 0;
}

/*! the dstore flag controls whether data is read from a GIFTI file */
int axml_get_dstore( void    ){ return gAXD.dstore; }
int axml_set_dstore( int val )
{
    if( val ) gAXD.dstore = 1;   /* includes -1 as applying the default */
    else      gAXD.dstore = 0;
    return 0;
}

/*! indent is the number of spaces per indent level written to a GIFTI file */
int axml_get_indent( void    ){ return gAXD.indent; }
int axml_set_indent( int val )
{
    if      ( val == -1 ) gAXD.indent = 3;
    else if ( val >=  0 ) gAXD.indent = val;
    else return 1;  /* failure - no action */
    return 0;
}

/*! buf_size is the size of the XML I/O buffer given to expat */
int axml_get_buf_size( void    ){ return gAXD.buf_size; }
int axml_set_buf_size( int val )
{
    if      ( val == -1 ) gAXD.buf_size = AXML_DEF_BSIZE;
    else if ( val  >  0 ) gAXD.buf_size = val;
    else return 1;      /* failure - no action */
    return 0;
}

/*! wstream is the write stream, should be valid or NULL */
FILE * axml_get_wstream( void      ) { return gAXD.wstream; }
int    axml_set_wstream( FILE * fp ) { gAXD.wstream = fp; return 0; }

/*----------------------- local prototypes -----------------------------*/

static XML_Parser init_xml_parser   (void *);

static void XMLCALL cb_char         (void *, const char *, int);
static void XMLCALL cb_default      (void *, const char *, int);
static void XMLCALL cb_start_ele(void *, const char *, const char **);
static void XMLCALL cb_stop_ele (void *, const char *);

static int  epush(afni_xml_control *, const char *, const char **);
static int  epop (afni_xml_control *, const char *);

static int  add_to_xchild_list(afni_xml_t * parent, afni_xml_t * child);
static int  add_to_xroot_list (afni_xml_control * xd, afni_xml_t * newp);
static int  append_to_string(char **, int *, const char *, int);
static int  disp_axml_ctrl ( const char *mesg, afni_xml_control * dp, int show_all );
static int  disp_gen_text(afni_xml_control *, const char *, const char *, int);
static void free_whitespace(void);
static int  init_axml_ctrl (afni_xml_control *xd, int doall);
static int  process_popped_element(afni_xml_control * xd, const char * ename);
static int  reset_xml_buf  (afni_xml_control * xd, char ** buf, int * bsize);
static int  white_first    (const char * str, int len);
static int  white_last     (const char * str, int len);

static int  show_depth     (afni_xml_control *, int show);
static int  show_attrs     (afni_xml_control *, const char **, int);

static int64_t      loc_strnlen     (const char * str, int64_t maxlen);
static afni_xml_t * make_afni_xml   (const char * ename, const char ** attr);
static char       * strip_whitespace(const char * str, int slen);

/*----------------------- main I/O functions ---------------------------*/

/* note: the buffer needs to be large enough to contain any contiguous
         piece of (CDATA?) text, o.w. it will require parsing in pieces */
afni_xml_list axml_read_file(const char * fname, int read_data)
{
   afni_xml_control * xd = &gAXD;        /* point to global struct */
   afni_xml_list      xlist = {0, NULL};

   XML_Parser   parser;
   unsigned     blen;
   FILE       * fp;
   char       * buf = NULL;
   int64_t      bshort;
   int          bsize;    /* be sure it doesn't change at some point */
   int          done = 0, pcount = 1;

   if( init_axml_ctrl(xd, 0) ) /* reset non-user variables */
      return xlist;

   xd->xroot = &xlist;      /* return struct */
   xd->dstore = read_data;  /* store for global access */

   if( !fname ) {
      fprintf(stderr,"** axml_read_image: missing filename\n");
      xd->xroot = NULL;
      return xlist;
   }

   fp = fopen(fname, "r");
   if( !fp ) {
      fprintf(stderr,"** failed to open XML file '%s'\n", fname);
      xd->xroot = NULL;
      return xlist;
   }

   /* create a new buffer */
   bsize = 0;
   if( reset_xml_buf(xd, &buf, &bsize) ) {
     fclose(fp);
     xd->xroot = NULL;
     return xlist;
   }

   if(xd->verb > 1) fprintf(stderr,"-- reading xml file '%s'\n", fname);

   /* create parser, init handlers */
   parser = init_xml_parser((void *)xd);

   while( !done )
   {
      if( reset_xml_buf(xd, &buf, &bsize) ) break;

      blen = fread(buf, 1, bsize, fp);

      /* check for early termination */
      bshort = loc_strnlen(buf, blen);
      if( bshort < blen ) {
         if( xd->verb > 1 )
            fprintf(stderr,"-- AXML: truncating fbuffer from %u to %" PRId64  "\n",
                    blen, bshort);
         blen = (int)bshort;
      }

      done = blen < (unsigned)  bsize;

      if(xd->verb > 4) fprintf(stderr,"-- XML_Parse # %d\n", pcount);
      pcount++;
      if( XML_Parse(parser, buf, blen, done) == XML_STATUS_ERROR) {
          fprintf(stderr,"** %s at line %u\n",
                  XML_ErrorString(XML_GetErrorCode(parser)),
                  (unsigned int)XML_GetCurrentLineNumber(parser));
          break;
      }
   }

   fclose(fp);
   if( buf ) free(buf);        /* parser buffer */
   XML_ParserFree(parser);

   if(xd->verb > 1) fprintf(stderr,"++ done parsing XML file %s\n", fname);

   xd->xroot = NULL; /* Stop pointing to xlist */
   return xlist;
}


/* parse a complete buffer as XML */
afni_xml_list axml_read_buf(const char * buf_in, int64_t bin_len)
{
    afni_xml_control * xd = &gAXD;        /* point to global struct */
    afni_xml_list      xlist = {0, NULL};

    XML_Parser   parser;
    unsigned     blen;
    int64_t      bin_remain;
    const char * bin_ptr = buf_in;      /* current buffer location */
    char       * buf = NULL;
    int          bsize;    /* be sure it doesn't change at some point */
    int          done = 0, pcount = 1;

    if( init_axml_ctrl(xd, 0) ) /* reset non-user variables */
        return xlist;

    xd->xroot = &xlist;      /* return struct */
    xd->dstore = 1;          /* store for global access */

    if( ! buf_in || bin_len < 0L ) {
       fprintf(stderr,"** axml_read_buf: missing buffer\n");
       xd->xroot=NULL;
       return xlist;
    }

    /* check for early termination */
    bin_remain = loc_strnlen(buf_in, bin_len);
    if( bin_remain < bin_len && xd->verb > 1 )
       fprintf(stderr,"-- AXML: truncating buffer from %" PRId64 
                      " to %" PRId64 "\n", bin_len, bin_remain);

    /* create a new buffer */
    bsize = 0;
    if( reset_xml_buf(xd, &buf, &bsize) )
    {
      xd->xroot = NULL; /* Stop pointing to xlist */
      return xlist;
    }

    if(xd->verb > 1)
       fprintf(stderr,"-- reading xml from length %" PRId64 " buffer\n",
               bin_remain);

    /* create parser, init handlers */
    parser = init_xml_parser((void *)xd);

    while( !done )
    {
        if( reset_xml_buf(xd, &buf, &bsize) ) break;

        /*--- replace fread with buffer copy ---*/

        /* decide how much to copy and copy */
        if( bin_remain >= bsize ) blen = bsize;
        else                      blen = bin_remain;

        if(blen > 0 && blen <= (unsigned)bsize) {
           memcpy(buf, bin_ptr, blen);
           buf[blen] = '\0';
        }

        /* update bytes remaining to process and decide if done */
        bin_remain -= blen;
        bin_ptr += blen;
        done = bin_remain <= 0;

        if(xd->verb > 4) fprintf(stderr,"-- XML_Parse # %d\n", pcount);
        pcount++;
        if( XML_Parse(parser, buf, blen, done) == XML_STATUS_ERROR) {
            fprintf(stderr,"** %s at line %u\n",
                    XML_ErrorString(XML_GetErrorCode(parser)),
                    (unsigned int)XML_GetCurrentLineNumber(parser));
            break;
        }
    }

    if( buf ) free(buf);        /* parser buffer */
    XML_ParserFree(parser);

    if(xd->verb > 1) fprintf(stderr,"++ done parsing XML buffer\n");

    xd->xroot = NULL; /* Stop pointing to xlist */
    return xlist;
}


/* display the list of afni_xml_t structs */
int axml_disp_xlist( const char *mesg, afni_xml_list * axlist, int verb)
{
   FILE * fp = stderr;
   int    ind;

   if( gAXD.wstream != NULL ) fp = gAXD.wstream;

   if( mesg ) fputs(mesg, fp);

   if( ! axlist || ! axlist->xlist ) {
      fprintf(fp, "afni_xml_list is EMPTY\n");
      return 0;
   }

   fprintf(fp, "afni_xml_list, len = %d\n", axlist->len);
   for(ind = 0; ind < axlist->len; ind++ ) {
      fprintf(fp, "   afni_xml_t root %d of %d:\n", ind+1, axlist->len);
      axml_disp_xml_t(NULL, axlist->xlist[ind], gAXD.indent, verb);
   }

   return 0;
}

/* recursive function to display an afni_xml_t struct */
int axml_disp_xml_t( const char *mesg, afni_xml_t * ax, int indent, int verb)
{
   FILE * fp = stderr;
   int    ind;

   if( gAXD.wstream != NULL ) fp = gAXD.wstream;
   if( mesg ) fprintf(fp, "%*s%s", indent, "", mesg);

   /* bail on no struct */
   if( ! ax ) { fprintf(fp, "%*sNULL\n", indent, ""); return 1; }


   if( verb > 1 ) {
      fprintf(fp, "%*s - name   : %s\n", indent-gAXD.indent, "", ax->name);
      if( verb > 2 ) {
         if(ax->xtext) {
           if(verb>3) fprintf(fp, "%*sxtext   : %s\n",indent,"",ax->xtext);
           else fprintf(fp, "%*sxtext   : %.30s...\n",indent,"", ax->xtext);
         } else fprintf(fp, "%*sxtext  : NULL\n", indent, "");
         fprintf(fp, "%*sxlen   : %d\n", indent, "", ax->xlen);
         fprintf(fp, "%*scdata  : %d\n", indent, "", ax->cdata);
         fprintf(fp, "%*sencode : %d\n", indent, "", ax->encode);

         /* only display binary information if there is something there */
         if( ax->bdata || ax->blen > 0 ) {
            fprintf(fp, "%*sbdata  : %s\n", indent, "",
                        ax->bdata ? "SET" : "CLEAR");
            fprintf(fp, "%*sblen   : %" PRId64 "\n", indent, "", ax->blen);
            fprintf(fp, "%*sbtype  : %d\n", indent, "", ax->btype);
         }
      }
      fprintf(fp, "%*snattrs : %d\n", indent, "", ax->attrs.length);
      if( verb > 2 ) {
         for(ind=0; ind < ax->attrs.length; ind++)
            fprintf(fp, "%*s         '%s' = '%s'\n",
                    indent, "", ax->attrs.name[ind], ax->attrs.value[ind]);
      }
      fprintf(fp, "%*snchild : %d\n", indent, "", ax->nchild);
      if(verb>3) fprintf(fp, "%*sxparent : %s\n",indent,"",
                         ax->xparent ? "SET" : "NONE" );
   } else
      /* just show the name */
      fprintf(fp, "%*s%s\n", indent, "", ax->name);

   /* recursively display the child structures */
   for(ind=0; ind < ax->nchild; ind++)
      axml_disp_xml_t(NULL, ax->xchild[ind], indent+gAXD.indent, verb);

   return 0;
}


/* allocate a new afni_xml_t struct, and possibly copy the name */
afni_xml_t * new_afni_xml(const char * name)
{
   afni_xml_t * newp;

   newp = (afni_xml_t *)calloc(1, sizeof(afni_xml_t));
   if( ! newp ) {
      fprintf(stderr,"** failed to alloc afni_xml_t struct\n");
      return NULL;
   }

   /* be picky with pointers */
   newp->name    = NULL;
   newp->xtext   = NULL;
   newp->bdata   = NULL;
   newp->xparent = NULL;
   newp->xchild  = NULL;

   if( name ) newp->name = strdup(name);

   return newp;
}


int axml_add_attrs(afni_xml_t * ax, const char ** attr)
{
   int c, aind, natr;

   if( ! ax ) return 1;

   if( ! attr ) return 0;

   if( ax->attrs.name || ax->attrs.value )
      fprintf(stderr,"** axml_add_attrs: have non-NULL at input\n");

   for(c=0, natr=0; attr[c]; c += 2, natr++)
      ;  /* just count */

   ax->attrs.length = natr;

   if( natr == 0 ) { /* we are done */
      ax->attrs.name = NULL;
      ax->attrs.value = NULL;
      return 0;
   }

   ax->attrs.name = (char **)malloc(natr*sizeof(char *));
   ax->attrs.value = (char **)malloc(natr*sizeof(char *));

   /* failure? */
   if( ! ax->attrs.name || ! ax->attrs.value ) {
      fprintf(stderr,"** NAX: failed to alloc 2 sets of %d char*\n", natr);
      ax->attrs.length = 0;
      if( ax->attrs.name )  free(ax->attrs.name);
      if( ax->attrs.value ) free(ax->attrs.value);
      ax->attrs.name = ax->attrs.value = NULL;
      return 1;
   }

   /* and get the attributes */
   for(c = 0, aind = 0; attr[c]; c += 2, aind++) {
      ax->attrs.name[aind]  = strdup(strip_whitespace(attr[c],0));
      ax->attrs.value[aind] = strdup(strip_whitespace(attr[c+1],0));
   }

   return 0;
}


/* free the list of afni_xml_t structs */
int axml_free_xlist(afni_xml_list * axlist)
{
   int    ind;

   /* check for quick returns */
   if( ! axlist ) return 0;
   if( ! axlist->xlist ) { axlist->len = 0; return 0; }

   for( ind = 0; ind < axlist->len; ind++ )
      axml_free_xml_t(axlist->xlist[ind]);

   free(axlist->xlist);

   axlist->xlist = NULL;
   axlist->len   = 0;

   free_whitespace(); /* this frees static string in strip_whitespace */

   return 0;
}

/* recursive free */
int axml_free_xml_t(afni_xml_t * ax)
{
   int ind;

   if( !ax ) return 0;

   if( ax->name )  { free(ax->name);  ax->name  = NULL; }
   if( ax->xtext ) { free(ax->xtext); ax->xtext = NULL; }
   if( ax->bdata ) { free(ax->bdata); ax->bdata = NULL; }
   ax->xlen = 0;

   /* free all attributes */
   for(ind = 0; ind < ax->attrs.length; ind++ ) {
      if( ax->attrs.name  && ax->attrs.name[ind] )  free(ax->attrs.name[ind]);
      if( ax->attrs.value && ax->attrs.value[ind] ) free(ax->attrs.value[ind]);
   }
   if( ax->attrs.name )  { free(ax->attrs.name);  ax->attrs.name = NULL; }
   if( ax->attrs.value ) { free(ax->attrs.value); ax->attrs.value = NULL; }
   ax->attrs.length = 0;

   /* and free children */
   if( ax->nchild > 0 && ax->xchild )
      for( ind=0; ind < ax->nchild; ind++ ) axml_free_xml_t(ax->xchild[ind]);
   ax->nchild = 0;
   if( ax->xchild ) { free(ax->xchild); ax->xchild = NULL; }
   ax->xparent = NULL;

   free(ax);

   return 0;
}

/* generic recursive function that processes an afni_xml_t tree */
int axml_recur(int(*func)(FILE * fp, afni_xml_t *, int), afni_xml_t * ax)
{
   static int depth = 1;
   int        ind;

   if( !func || !ax ) return 1;

   /* call on current struct and recur over kids */

   func(gAXD.wstream, ax, depth);

   depth++;
   if( ax->nchild > 0 && ax->xchild )
      for( ind=0; ind < ax->nchild; ind++ )
         axml_recur(func, ax->xchild[ind]);
   depth--;

   return 0;
}


/* generic recursive function to find something in an xml tree
   (using a depth-first search)

      func      : function to determine whether a struct is the desired one
      ax        : root of the search tree
      depth     : current depth
      max_depth : if > 0, limit the search to this depth

 */
afni_xml_t * axml_recur_find_xml(int(*func)(afni_xml_t *, int), afni_xml_t * ax,
                                 int depth, int max_depth)
{
   afni_xml_t * rv = NULL;
   int          ind;

   if( !func || !ax ) return NULL;

   /* if we are looking at the correct struct, return it */
   if( func(ax, depth) ) return ax;

   /* if we are at the maximum depth and have not found it, fail */
   if( max_depth > 0 && depth >= max_depth ) return NULL;

   /* search deeper */
   if( ax->nchild > 0 && ax->xchild )
      for( ind=0; ind < ax->nchild; ind++ ) {
         rv = axml_recur_find_xml(func, ax, depth+1, max_depth);
         if( rv ) return rv;
      }

   return NULL;  /* failure - not in this tree */
}


/* return corresponding value pointer for attribute */
char * axml_attr_value(afni_xml_t * ax, const char * name)
{
   int ind;
   if( !ax ) return NULL;
   for( ind = 0; ind < ax->attrs.length; ind++ )
      if( ! strcmp(ax->attrs.name[ind], name) ) return ax->attrs.value[ind];

   return NULL;  /* not found */
}



/*----------------------- end main I/O functions -----------------------*/



static int init_axml_ctrl(afni_xml_control *xd, int doall)
{
   if( doall ) {         /* user modifiable - init all to defaults */
      xd->verb     = 1;
      xd->dstore   = 1;
      xd->indent   = 3;
      xd->buf_size = AXML_DEF_BSIZE;
      xd->wstream  = stderr;    /* rcr - ponder accessor (might need close) */
   }

   xd->depth = 0;
   xd->dskip = 0;
   xd->errors = 0;
   xd->wkeep = 0;
   memset(xd->stack, 0, sizeof(xd->stack));

   xd->xroot = NULL;

   /* maybe show the user */
   if( xd->verb > 2 )
       disp_axml_ctrl("-- user opts: ", xd, xd->verb > 3);

   return 0;
}


static int disp_axml_ctrl( const char *mesg, afni_xml_control * dp, int show_all )
{
   if( mesg ) fputs(mesg, stderr);

   if( !dp ) return 1;

   fprintf(stderr,"afni_xml_control :\n"
                  "   verb        : %d\n"
                  "   dstore      : %d\n"
                  "   indent      : %d\n"
                  "   buf_size    : %d\n"
          , dp->verb, dp->dstore, dp->indent, dp->buf_size);

   if( show_all )
      fprintf(stderr,
              "   depth       : %d\n"
              "   dskip       : %d\n"
              "   errors      : %d\n"
              "   wkeep       : %d\n"
              , dp->depth, dp->dskip, dp->errors, dp->wkeep);

   return 0;
}

/* if bsize is no longer correct, update it and safe_realloc the buffer */
static int reset_xml_buf(afni_xml_control * xd, char ** buf, int * bsize)
{
    if( *bsize == xd->buf_size ) {
        if( xd->verb > 3 )
            fprintf(stderr,"-- buffer kept at %d bytes\n", *bsize);
        return 0;
    }

    if( xd->verb > 2 )
        fprintf(stderr,"++ update buf, %d to %d bytes\n",*bsize,xd->buf_size);

    *bsize = xd->buf_size;
    *buf = (char *)safe_realloc(*buf, (*bsize+1) * sizeof(char));
    if( ! *buf ) {
        fprintf(stderr,"** failed to alloc %d bytes of xml buf!\n", *bsize);
        *bsize = 0;
        return 1;
    }

    return 0;
}


static XML_Parser init_xml_parser(void * user_data)
{
   XML_Parser parser = XML_ParserCreate(NULL);

   XML_SetUserData(parser, user_data);

   XML_SetStartElementHandler(parser, cb_start_ele);
   XML_SetEndElementHandler  (parser, cb_stop_ele);
   XML_SetCharacterDataHandler(parser, cb_char);
   XML_SetDefaultHandler     (parser, cb_default);

   if( gAXD.verb > 3 ) fprintf(stderr,"-- parser initialized\n");

   return parser;
}

static void XMLCALL cb_start_ele(void *udata, const char *ename,
                                              const char **attr)
{
   afni_xml_control * xd = (afni_xml_control *)udata;

   if( xd->wkeep ) xd->wkeep = 0; /* clear storage continuation */

   (void)epush(xd, ename, attr);
}

static void XMLCALL cb_stop_ele(void *udata, const char *ename)
{
   afni_xml_control * xd = (afni_xml_control *)udata;

   if( xd->wkeep ) xd->wkeep = 0; /* clear storage continuation */

   (void)epop(xd, ename);
}

static int epush(afni_xml_control * xd, const char * ename, const char ** attr)
{
   afni_xml_t * acur = NULL;
   int          errs = 0;

   if( xd->wkeep ) xd->wkeep = 0; /* clear storage continuation */

   xd->depth++;

   if( xd->depth <= 0 || xd->depth > AXML_MAX_DEPTH ) {
       fprintf(stderr,"** push: stack depth %d out of [0,%d] range\n",
               xd->depth, AXML_MAX_DEPTH);
       xd->errors++;
       errs = 1;
   }

   if( xd->verb > 2 ) {       /* maybe we want to print something */
       show_depth(xd, 1);
       fprintf(stderr,"++ push '%s'\n", ename);
       if( xd->verb > 3 ) show_attrs(xd, attr, 1);
   }

   /* determine whether we should go into a skip block */
   if( errs ) xd->dskip = xd->depth;

   /* if we are in a skip block, do nothing but monitor stack */
   if( xd->dskip ) {
       if( xd->verb > 3 )
           fprintf(stderr,"-- skip=%d, depth=%d, skipping push element '%s'\n",
                   xd->dskip, xd->depth, ename);
   } else {
      /* --- control depth and corresponding vars --- */
      acur = make_afni_xml(ename, attr);
      if( ! acur ) { xd->dskip = xd->depth; return 1; }
      xd->stack[xd->depth-1] = acur;

      /* we have either a root element or a child */
      if( xd->depth == 1 ) {
         if( add_to_xroot_list(xd, acur) ) { xd->dskip = xd->depth; return 1; }
      } else {
         if( add_to_xchild_list(xd->stack[xd->depth-2], acur) ) {
            xd->dskip = xd->depth; return 1;
         }
      }
   }

   return 0;
}


static int epop(afni_xml_control * xd, const char * ename)
{
   if( xd->wkeep ) xd->wkeep = 0; /* clear storage continuation */

   if( xd->dskip ) {
      if( xd->dskip == xd->depth ) xd->dskip = 0;  /* clear */

      if( xd->verb > 3 )
          fprintf(stderr,"-- skip=%d, depth=%d, skipping pop element '%s'\n",
                  xd->dskip, xd->depth, ename);
   } else {
      process_popped_element(xd, ename);
   }

   if( ! xd->dskip ) {
      xd->stack[xd->depth-1] = NULL;  /* should be irrelevant */

      if( xd->verb > 4 ) {
          show_depth(xd, 1);
          fprintf(stderr,"++ pop '%s'\n", ename);
      }
   }

   xd->depth--;

   return 0;
}

static int process_popped_element(afni_xml_control * xd, const char * ename)
{
   afni_xml_t * ax;
   ax = xd->stack[xd->depth-1];

   if( strcmp(ename, ax->name) ) {
      if( gAXD.verb ) fprintf(stderr,"** pop mismatch!\n");
      return 1;
   }

   return 0;
}


static int add_to_xroot_list(afni_xml_control * xd, afni_xml_t * newp)
{
   /* init */
   if( xd->xroot->len <= 0 ) { xd->xroot->len = 0; xd->xroot->xlist = NULL; }

   xd->xroot->len++;
   xd->xroot->xlist = (afni_xml_t **)safe_realloc(xd->xroot->xlist,
                                     xd->xroot->len * sizeof(afni_xml_t *));
   if( ! xd->xroot->xlist ) {
      fprintf(stderr,"** failed to alloc %d AXMLT pointers\n", xd->xroot->len);
      return 1;
   }
   xd->xroot->xlist[xd->xroot->len-1] = newp;

   return 0;
}


static int add_to_xchild_list(afni_xml_t * parent, afni_xml_t * child)
{
   /* init */
   if( parent->nchild <= 0 ) { parent->nchild = 0; parent->xchild = NULL; }

   parent->nchild++;
   parent->xchild = (afni_xml_t **)safe_realloc(parent->xchild,
                                   parent->nchild * sizeof(afni_xml_t *));
   if( ! parent->xchild ) {
      fprintf(stderr,"** failed to alloc %d AXML pointers\n", parent->nchild);
      return 1;
   }
   parent->xchild[parent->nchild-1] = child;

   /* and note the child's parent */
   child->xparent = parent;

   return 0;
}


static afni_xml_t * make_afni_xml(const char * ename, const char ** attr)
{
   afni_xml_t * newp;

   newp = new_afni_xml(ename);
   if( ! newp ) return NULL;

   axml_add_attrs(newp, attr);

   return newp;
}


static int show_attrs(afni_xml_control * xd, const char ** attr, int showd)
{
   int count;
   for( count = 0; attr[count]; count += 2 ){
      if( showd ) show_depth(xd, 0);
      fprintf(stderr,"      attr: %s='%s'\n", attr[count], attr[count+1]);
   }
   return 0;
}

static void free_whitespace(void) { strip_whitespace(NULL,-2); }

/* if slen == 0, use entire length */
static char * strip_whitespace(const char * str, int slen)
{
   static char * buf = NULL;
   static int    blen = 0;
   int           len, ifirst, ilast;  /* first non-white char and AFTER last */

   /* backdoor to free this memory, e.g. on call to free list */
   if(!str && slen == -2){if(buf) { free(buf); buf=NULL; } blen=0; return 0; }

   /* if string is long, forget it */
   if( !str || slen > 1024 ) return (char *)str;

   len = strlen(str);
   if( slen > 0 && slen < len ) len = slen;
   if( len <= 0 ) return (char *)str;

   /* make sure we have local space */
   if( len > blen ) { /* allocate a bigger buffer */
      buf = (char *)safe_realloc(buf, (len+1) * sizeof(char));
      if( !buf ) {
         fprintf(stderr,"** failed to alloc wspace buf of len %d\n", len+1);
         return (char *)str;
      }
      blen = len;
   }

   ifirst = white_first(str, len);
   ilast  = white_last(str, len);

   if( ifirst == len ) *buf = '\0';
   else {
      strncpy(buf, str+ifirst, len-ifirst-ilast);
      buf[len-ifirst-ilast] = '\0';
   }

   return buf;
}

static void XMLCALL cb_char(void *udata, const char * cdata, int length)
{
   afni_xml_control * xd = (afni_xml_control *)udata;
   afni_xml_t       * parent;
   int                wlen = white_first(cdata,length);


   /* wkeep: set once non-white chars are found, clear on push/pop */
   /* once set: even white chars are appended to current xtext */

   if( !xd->wkeep && wlen == length ) {
      if(xd->verb > 4) fprintf(stderr,"-- skipping white char [%d]\n",length);
      return;
   }

   if( xd->dskip ) {
      if(xd->verb > 3) fprintf(stderr,"-- skipping char [%d]\n",length);
      return;
   }

   if( ! xd->wkeep ) xd->wkeep = 1; /* store everything at this point */

   if( xd->verb > 4 ) disp_gen_text(xd, "char", cdata, length);

   parent = xd->stack[xd->depth-1];
   if( !parent ) {
      fprintf(stderr,"** no parent to store char data under\n");
      return;
   }

   if( gAXD.dstore )
      append_to_string(&parent->xtext, &parent->xlen, cdata, length);
}

/* append new string (of given length) to old string and length */
static int append_to_string(char ** ostr, int * olen,
                            const char * istr, int ilen)
{
   int newlen;

   /* be safe; also init old length to 1 */
   if( !*ostr || *olen <= 0 ) { *ostr = NULL; *olen = 1; }

   newlen = *olen + ilen;

   *ostr = (char *)safe_realloc(*ostr, newlen * sizeof(char));
   if( !*ostr ) {
      fprintf(stderr,"** AX.A2S: failed to alloc %d chars\n", newlen);
      return 1;
   }

   /* copy, starting at old nul char (if any), and terminate */
   strncpy((*ostr)+*olen-1, istr, ilen);
   (*ostr)[newlen-1] = '\0';
   *olen = newlen;

   return 0;
}

static int disp_gen_text(afni_xml_control * xd, const char * header,
                         const char * cdata, int length)
{
   const char * str = cdata;
   int          wlen = white_first(cdata,length);
   int          len = length;

   if( len == wlen ) {
       str = "whitespace";     /* just note the whitespace */
       len = strlen(str);
   }

   show_depth(xd, 1);
   fprintf(stderr, "%s [len %d]: '%s'\n", header, length,
                   strip_whitespace(str, len));

   return 0;
}

static void XMLCALL cb_default(void * udata, const char * cdata, int length)
{
   afni_xml_control * xd = (afni_xml_control *)udata;

   if( xd->wkeep ) xd->wkeep = 0; /* clear storage continuation */

   if( xd->verb > 5 ) disp_gen_text(xd, "default XML", cdata, length);
}


static int show_depth(afni_xml_control * xd, int show)
{
   FILE * fp = stderr;
   if( xd->wstream ) fp = xd->wstream;

   if( show ) fprintf(fp, "%*s %02d ", xd->indent*xd->depth, "", xd->depth);
   else       fprintf(fp, "%*s    ", xd->indent*xd->depth, "");
   return 0;
}


static int white_first(const char * str, int len)
{
    int c;
    if( !str || !*str || len < 1 ) return 0;
    for( c = 0; c < len; c++ )
        if( !isspace(str[c]) ) return c;

    return len;
}

static int white_last(const char * str, int len)
{
    int c;
    if( !str || !*str || len < 1 ) return 0;
    for( c = len-1; c >= 0; c-- )
        if( !isspace(str[c]) ) return (len-1-c);

    return len;
}

/* like strlen(), but do not look or return past maxlen */
/* note: strnlen() is probably too new to assume        */
static int64_t loc_strnlen(const char * str, int64_t maxlen)
{
   const char * sptr;
   int64_t      len;

   for( sptr=str, len=0; *sptr && len<maxlen; sptr++, len++)
      ;

   return len;  /* max of maxlen */
}
