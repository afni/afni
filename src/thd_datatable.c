#include "mrilib.h"
#include "thd_datatable.h"
#include <limits.h>
#include <stdint.h>

/*----------------------------------------------------------------------------
  Reading '-dataTable' options in C.  See thd_datatable.h for the format.
                                                     -- P Molfese, Jul 2026
------------------------------------------------------------------------------*/

#define DT_SEP " \t\n\r"

/*============================================================================*/
/*  Tokenizing                                                                */
/*============================================================================*/

typedef struct { int ntok ; char **tok ; int lnum ; } dt_line ;

static void dt_addtok( char ***tok , int *ntok , char *s )
{
   *tok = (char **)realloc( *tok , sizeof(char *)*(*ntok+1) ) ;
   (*tok)[(*ntok)++] = strdup(s) ;
}

/*! Cut a line at its comment: a '#' at the start of a word (the line's start,
    or after white space), outside a quoted word.  A '#' inside a word is text,
    so AFNI labels like face#0 and selectors like stats+tlrc[face#0_Coef] can
    be written as they are. */

static void dt_strip_comment( char *buf )
{
   char *c=buf ;
   while( *c != '\0' ){
     while( *c != '\0' && isspace((int)*c) ) c++ ;
     if( *c == '\0' ) return ;
     if( *c == '#' ){ *c = '\0' ; return ; }
     if( *c == '"' || *c == '\'' ){              /* a quoted word: to its closing quote */
       char q=*c++ ;
       while(*c){
         if(*c==q){if(c[1]==q){c+=2;continue;}c++;break;}
         c++;
       }
     }
     while( *c != '\0' && !isspace((int)*c) ) c++ ;
   }
}

/*! Split on white space.  A word that begins with a quote ("..." or '...')
    runs to the matching quote, white space and '#' included, and the quotes
    are removed: "face#0", "two words".  A quote inside a word is text, as in
    AFNI's x.1D' (transposed).  Returns 1 on an unclosed quote, 2 on text right
    after a closing quote. */

static int dt_split( char *acc , char ***tok , int *ntok )
{
   char *c=acc , *w ; size_t n=strlen(acc)+1 ;
   w=(char *)malloc(n) ;
   while( *c != '\0' ){
     int k=0 ;
     while( *c != '\0' && isspace((int)*c) ) c++ ;
     if( *c == '\0' ) break ;
     if( *c == '"' || *c == '\'' ){
       char q=*c++ ;
       while( *c != '\0' ){
         if(*c==q){if(c[1]==q){w[k++]=q;c+=2;continue;}break;}
         w[k++]=*c++;
       }
       if( *c != q ){ free(w) ; return 1 ; }
       c++ ;
       if( *c != '\0' && !isspace((int)*c) ){ free(w) ; return 2 ; }
     } else
       while( *c != '\0' && !isspace((int)*c) ) w[k++]=*c++ ;
     w[k]='\0' ; dt_addtok( tok , ntok , w ) ;
   }
   free(w) ; return 0 ;
}

/*! Read a file into lines of tokens.  Blank lines and comments (see
    dt_strip_comment) are dropped; quotes group text (see dt_split); a line
    ending in '\' continues onto the next one, so a table can be wrapped the
    same way it would be on a command line. */

static dt_line * dt_read_lines( char *fname , int *nline_out )
{
   FILE *fp ; char buf[16384] , acc[65536] ;
   dt_line *ln=NULL ; int nline=0 , lnum=0 , acc_start=0 ; int pending=0 ;

   fp = fopen( fname , "r" ) ;
   if( fp == NULL ) ERROR_exit("can't open dataTable file '%s'",fname) ;

   acc[0] = '\0' ;

   while( fgets(buf,sizeof(buf),fp) != NULL ){
     int len ;
     lnum++ ;

     /* Explicit commented schema for AFNI-style mixed text/numeric 1D tables.
        Plain headers remain accepted. Only this marker is interpreted. */
     { char *p=buf; while(isspace((unsigned char)*p))p++;
       if(!strncmp(p,"# Columns:",10)){
          if(nline || acc[0]) ERROR_exit("dataTable '%s': duplicate or late # Columns: header",fname);
          memmove(buf,p+10,strlen(p+10)+1);
       } else dt_strip_comment(buf);
     }

     /* trailing backslash means 'continued on the next line' */
     for( len=strlen(buf) ; len > 0 && isspace((int)buf[len-1]) ; len-- ) ;
     buf[len] = '\0' ;
     if( len > 0 && buf[len-1] == '\\' ){ buf[len-1] = '\0' ; pending = 1 ; }
     else                                                     pending = 0 ;

     if( acc[0] == '\0' ) acc_start = lnum ;
     if( strlen(acc) + strlen(buf) + 2 >= sizeof(acc) )
       ERROR_exit("dataTable file '%s': line %d is absurdly long",fname,lnum) ;
     strcat( acc , " " ) ; strcat( acc , buf ) ;

     if( pending ) continue ;             /* keep accumulating */

     { char **tok=NULL ; int ntok=0 ;
       int rc = dt_split( acc , &tok , &ntok ) ;
       if( rc == 1 ) ERROR_exit("dataTable file '%s': line %d has a quote that is never closed",fname,acc_start) ;
       if( rc == 2 ) ERROR_exit("dataTable file '%s': line %d has text right after a closing quote (quote the whole value)",fname,acc_start) ;

       if( ntok > 0 ){
         ln = (dt_line *)realloc( ln , sizeof(dt_line)*(nline+1) ) ;
         ln[nline].ntok = ntok ; ln[nline].tok = tok ; ln[nline].lnum = acc_start ;
         nline++ ;
       } else if( tok != NULL ) free(tok) ;
     }
     acc[0] = '\0' ;
   }
   fclose(fp) ;

   *nline_out = nline ; return ln ;
}

/*============================================================================*/
/*  Building the table                                                        */
/*============================================================================*/

/*! Fill in the derived parts of a table whose cname/cell/ncol/nrow are set. */

static int dt_finish( THD_datatable *dt )
{
   int ii , jj ; char *cpt ;

   if( dt == NULL ) return 0 ;
   dt->icol_subj = dt->icol_input = -1 ;
   if( dt->ncol < 1 || dt->nrow < 1 ) return 0 ;

   for( jj=0 ; jj < dt->ncol ; jj++ ){
     if( strcasecmp(dt->cname[jj],"Subj")      == 0 ) dt->icol_subj  = jj ;
     if( strcasecmp(dt->cname[jj],"InputFile") == 0 ) dt->icol_input = jj ;
   }

   /* a column is numeric only if every one of its cells parses completely */
   dt->val   = (float **)calloc( dt->ncol , sizeof(float *) ) ;
   dt->isnum = (int *)calloc( dt->ncol , sizeof(int) ) ;
   if( dt->val==NULL || dt->isnum==NULL ) return 0 ;

   for( jj=0 ; jj < dt->ncol ; jj++ ){
     if( jj == dt->icol_subj || jj == dt->icol_input ) continue ;
     dt->val[jj] = (float *)calloc( dt->nrow , sizeof(float) ) ;
     if( dt->val[jj]==NULL ) return 0 ;
     dt->isnum[jj] = 1 ;
     for( ii=0 ; ii < dt->nrow ; ii++ ){
       char *s = DT_CELL(dt,ii,jj) ;
       dt->val[jj][ii] = (float)strtod( s , &cpt ) ;
       if( cpt == s || *cpt != '\0' ){ dt->isnum[jj] = 0 ; break ; }
     }
     if( !dt->isnum[jj] ){ free(dt->val[jj]) ; dt->val[jj] = NULL ; }
   }

   /* convenience copies of the two special columns */
   dt->subj = (char **)calloc( dt->nrow , sizeof(char *) ) ;
   if( dt->subj==NULL ) return 0 ;
   for( ii=0 ; ii < dt->nrow ; ii++ ){
     if( dt->icol_subj >= 0 ){
       dt->subj[ii] = strdup( DT_CELL(dt,ii,dt->icol_subj) ) ;
     } else {
       char bb[32] ; sprintf(bb,"row%03d",ii+1) ; dt->subj[ii] = strdup(bb) ;
     }
     if( dt->subj[ii]==NULL ) return 0 ;
   }

   if( dt->icol_input >= 0 ){
     dt->fname = (char **)calloc( dt->nrow , sizeof(char *) ) ;
     if( dt->fname==NULL ) return 0 ;
     for( ii=0 ; ii < dt->nrow ; ii++ ){
       dt->fname[ii] = strdup( DT_CELL(dt,ii,dt->icol_input) ) ;
       if( dt->fname[ii]==NULL ) return 0 ;
     }

     /* A dataset name is never a bare number, so a numeric cell in the
        InputFile column means the columns are misaligned -- almost always
        because 'InputFile' was not the last column of an inline table.
        Catching it here beats a baffling "can't open dataset '1.0'". */
     for( ii=0 ; ii < dt->nrow ; ii++ ){
       (void)strtod( dt->fname[ii] , &cpt ) ;
       if( cpt != dt->fname[ii] && *cpt == '\0' )
         ERROR_exit(
           "dataTable (%s): the InputFile column of row %d holds '%s', which\n"
           "       is a number, not a dataset.  The columns are misaligned.%s" ,
           (dt->source != NULL) ? dt->source : "?" , ii+1 , dt->fname[ii] ,
           dt->from_argv
             ? "\n       Given directly on the command line, 'InputFile' must be"
               "\n       the LAST column.  Put the table in a file to lift that."
             : "" ) ;
     }
   } else {
     dt->fname = NULL ;
   }
   return 1 ;
}

/*----------------------------------------------------------------------------*/

THD_datatable * THD_read_datatable_file( char *fname )
{
   THD_datatable *dt ; dt_line *ln ; int nline , ii , jj ;

   ln = dt_read_lines( fname , &nline ) ;

   if( nline == 0 )
     ERROR_exit("dataTable file '%s' is empty",fname) ;
   if( nline == 1 )
     ERROR_exit("dataTable file '%s' has a header but no data rows",fname) ;

   dt = (THD_datatable *)calloc( 1 , sizeof(THD_datatable) ) ;
   dt->source = strdup(fname) ;
   dt->ncol   = ln[0].ntok ;      /* the header line defines the width */
   dt->nrow   = nline - 1 ;

   dt->cname = (char **)calloc( dt->ncol , sizeof(char *) ) ;
   for( jj=0 ; jj < dt->ncol ; jj++ ) dt->cname[jj] = strdup( ln[0].tok[jj] ) ;

   dt->cell = (char **)calloc( (size_t)dt->nrow*dt->ncol , sizeof(char *) ) ;
   for( ii=0 ; ii < dt->nrow ; ii++ ){
     if( ln[ii+1].ntok != dt->ncol )
       ERROR_exit(
         "dataTable file '%s', line %d: found %d columns, but the header has\n"
         "       %d.  A file name containing a space is the usual cause; a\n"
         "       missing value is the other." ,
         fname , ln[ii+1].lnum , ln[ii+1].ntok , dt->ncol ) ;
     for( jj=0 ; jj < dt->ncol ; jj++ )
       DT_CELL(dt,ii,jj) = strdup( ln[ii+1].tok[jj] ) ;
   }

   for( ii=0 ; ii < nline ; ii++ ){
     for( jj=0 ; jj < ln[ii].ntok ; jj++ ) free(ln[ii].tok[jj]) ;
     free(ln[ii].tok) ;
   }
   free(ln) ;

   if( !dt_finish(dt) ) ERROR_exit("dataTable: cannot allocate table metadata") ;
   return dt ;
}

/*----------------------------------------------------------------------------*/

static THD_datatable * dt_parse_flat( char **tok , int ntok , char *source ,
                                      int from_argv )
{
   THD_datatable *dt ; int ii , jj , ncol=0 ;

   for( ii=0 ; ii < ntok ; ii++ )
     if( strcasecmp(tok[ii],"InputFile") == 0 ){ ncol = ii+1 ; break ; }

   if( ncol == 0 )
     ERROR_exit(
       "dataTable (%s): no 'InputFile' column found in the header.\n"
       "       When the table is given directly on the command line there is\n"
       "       no line structure to count columns with, so 'InputFile' must be\n"
       "       the last column.  Put the table in a file to lift that rule." ,
       source ) ;
   if( ntok == ncol )
     ERROR_exit("dataTable (%s): a header but no data rows",source) ;
   if( (ntok - ncol) % ncol != 0 )
     ERROR_exit(
       "dataTable (%s): %d values after a %d-column header, which is not a\n"
       "       whole number of rows.  A ragged row, or a file name with a\n"
       "       space in it, is the usual cause." , source , ntok-ncol , ncol ) ;

   dt = (THD_datatable *)calloc( 1 , sizeof(THD_datatable) ) ;
   dt->source    = strdup(source) ;
   dt->from_argv = from_argv ;
   dt->ncol      = ncol ;
   dt->nrow      = (ntok - ncol) / ncol ;

   dt->cname = (char **)calloc( ncol , sizeof(char *) ) ;
   for( jj=0 ; jj < ncol ; jj++ ) dt->cname[jj] = strdup( tok[jj] ) ;

   dt->cell = (char **)calloc( (size_t)dt->nrow*ncol , sizeof(char *) ) ;
   for( ii=0 ; ii < dt->nrow ; ii++ )
     for( jj=0 ; jj < ncol ; jj++ )
       DT_CELL(dt,ii,jj) = strdup( tok[ ncol + ii*ncol + jj ] ) ;

   if( !dt_finish(dt) ) ERROR_exit("dataTable: cannot allocate table metadata") ;
   return dt ;
}

THD_datatable * THD_parse_datatable( char **tok , int ntok , char *source )
{
   return dt_parse_flat( tok , ntok , source , 0 ) ;
}

/*----------------------------------------------------------------------------*/

THD_datatable * THD_read_datatable_args( int argc , char **argv , int nopt ,
                                         char **stop_opts , int *nused )
{
   THD_datatable *dt ; int start = nopt ;

   if( nopt >= argc )
     ERROR_exit("dataTable: nothing follows the option") ;

   if( argv[nopt][0] == '@' ){                    /* -dataTable @file.txt */
     dt = THD_read_datatable_file( argv[nopt]+1 ) ;
     if( nused != NULL ) *nused = 1 ;
     return dt ;
   }

   { char **tok=NULL ; int ntok=0 , ii , stop ;

     while( nopt < argc ){
       stop = 0 ;
       if( argv[nopt][0] == '-' && stop_opts != NULL ){
         for( ii=0 ; stop_opts[ii] != NULL ; ii++ )
           if( strcasecmp(argv[nopt],stop_opts[ii]) == 0 ){ stop = 1 ; break ; }
       }
       if( stop ) break ;
       dt_addtok( &tok , &ntok , argv[nopt] ) ; nopt++ ;
     }

     dt = dt_parse_flat( tok , ntok , "given on the command line" , 1 ) ;

     for( ii=0 ; ii < ntok ; ii++ ) free(tok[ii]) ;
     free(tok) ;
   }

   if( nused != NULL ) *nused = nopt - start ;
   return dt ;
}

/*============================================================================*/
/*  Using the table                                                           */
/*============================================================================*/

int THD_datatable_column( THD_datatable *dt , char *name )
{
   int jj ;
   if( dt == NULL || name == NULL ) return -1 ;
   for( jj=0 ; jj < dt->ncol ; jj++ )
     if( strcasecmp(dt->cname[jj],name) == 0 ) return jj ;
   return -1 ;
}

float * THD_datatable_values( THD_datatable *dt , char *name )
{
   int jj = THD_datatable_column( dt , name ) ;
   if( jj < 0 || !dt->isnum[jj] ) return NULL ;
   return dt->val[jj] ;
}

float * THD_datatable_numeric_columns( const THD_datatable *dt,
                                      const char * const *columns, int ncolumn,
                                      const int *rows, int nrow,
                                      char *err, int nerr )
{
   float *out=NULL ; int ii,jj,kk,cc,rr ;
#define NUMERIC_FAIL(...) do{ if(err && nerr>0)snprintf(err,(size_t)nerr,__VA_ARGS__); goto fail; }while(0)
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( dt==NULL || dt->nrow<1 || dt->ncol<1 || dt->cname==NULL ||
       dt->isnum==NULL || dt->val==NULL )
     NUMERIC_FAIL("numeric extraction requires a nonempty table with numeric metadata") ;
   if( columns==NULL || ncolumn<1 || nrow<1 )
     NUMERIC_FAIL("numeric extraction requires columns and positive row/column counts") ;
   if( rows==NULL && nrow!=dt->nrow )
     NUMERIC_FAIL("numeric extraction without a row map requires all %d table rows",dt->nrow) ;
   if( (size_t)nrow>SIZE_MAX/sizeof(float)/(size_t)ncolumn )
     NUMERIC_FAIL("numeric column matrix is too large") ;
   for( cc=0 ; cc<dt->ncol ; cc++ )
     if( dt->cname[cc]==NULL ) NUMERIC_FAIL("table column %d has no name",cc+1) ;
   for( ii=0 ; ii<nrow ; ii++ ){
     rr=rows?rows[ii]:ii ;
     if( rr<0 || rr>=dt->nrow )
       NUMERIC_FAIL("numeric extraction: output row %d has invalid table row index %d",ii+1,rr) ;
   }
   out=(float *)malloc(sizeof(float)*(size_t)nrow*ncolumn) ;
   if( out==NULL ) NUMERIC_FAIL("cannot allocate numeric column matrix") ;
   for( jj=0 ; jj<ncolumn ; jj++ ){
     if( columns[jj]==NULL || columns[jj][0]=='\0' )
       NUMERIC_FAIL("numeric column name at position %d is empty",jj+1) ;
     cc=THD_datatable_column((THD_datatable *)dt,(char *)columns[jj]) ;
     if( cc<0 || !dt->isnum[cc] || dt->val[cc]==NULL )
       NUMERIC_FAIL("column '%s' must contain finite numeric values",columns[jj]) ;
     for( kk=0 ; kk<jj ; kk++ )
       if( !strcasecmp(columns[jj],columns[kk]) )
         NUMERIC_FAIL("duplicate numeric column '%s'",columns[jj]) ;
     for( ii=0 ; ii<nrow ; ii++ ){
       float value ; rr=rows?rows[ii]:ii ; value=dt->val[cc][rr] ;
       if( !isfinite(value) )
         NUMERIC_FAIL("column '%s', output row %d (table row %d) is not finite",columns[jj],ii+1,rr+1) ;
       out[(size_t)ii*ncolumn+jj]=value ;
     }
   }
   return out ;
fail:
   free(out) ; return NULL ;
#undef NUMERIC_FAIL
}

static int dt_level_index( char **level , int nlevel , const char *value )
{
   int ii ;
   for( ii=0 ; ii<nlevel ; ii++ ) if( strcmp(level[ii],value)==0 ) return ii ;
   return -1 ;
}

static int dt_string_cmp( const void *a , const void *b )
{
   const char *aa=*(const char * const *)a,*bb=*(const char * const *)b ;
   return strcmp(aa,bb) ;
}

THD_datatable_index * THD_datatable_index_columns(
                                      THD_datatable *dt , int ndim ,
                                      char **columns , int *caller_nlevel ,
                                      char ***caller_level )
{
   THD_datatable_index *dx ; int dd,rr,cc ; long long nc=1 ;
   if( dt==NULL || ndim<1 || columns==NULL ) return NULL ;

   dx=(THD_datatable_index *)calloc(1,sizeof(*dx)) ;
   dx->ndim=ndim ; dx->nrow=dt->nrow ;
   dx->icol=(int *)malloc(sizeof(int)*ndim) ;
   dx->nlevel=(int *)calloc(ndim,sizeof(int)) ;
   dx->stride=(int *)malloc(sizeof(int)*ndim) ;
   dx->column=(char **)calloc(ndim,sizeof(char *)) ;
   dx->level=(char ***)calloc(ndim,sizeof(char **)) ;

   for( dd=0 ; dd<ndim ; dd++ ){
     if( caller_nlevel!=NULL && caller_nlevel[dd]<THD_DT_LEVELS_LEXICAL )
       ERROR_exit("dataTable: invalid level-order code %d for column '%s'",
                  caller_nlevel[dd],columns[dd]) ;
     dx->icol[dd]=THD_datatable_column(dt,columns[dd]) ;
     if( dx->icol[dd]<0 )
       ERROR_exit("dataTable (%s): index column '%s' is missing",
                  (dt->source!=NULL)?dt->source:"?",columns[dd]) ;
     dx->column[dd]=strdup(dt->cname[dx->icol[dd]]) ;
     if( caller_nlevel!=NULL && caller_nlevel[dd]>0 ){
       int ll ; dx->nlevel[dd]=caller_nlevel[dd] ;
       dx->level[dd]=(char **)calloc(dx->nlevel[dd],sizeof(char *)) ;
       if( caller_level==NULL || caller_level[dd]==NULL )
         ERROR_exit("dataTable: explicit level count for '%s' has no level list",columns[dd]) ;
       for( ll=0 ; ll<dx->nlevel[dd] ; ll++ ){
         const char *s=caller_level[dd][ll] ;
         if( s==NULL || *s=='\0' )
           ERROR_exit("dataTable: empty requested level %d for column '%s'",ll+1,columns[dd]) ;
         if( dt_level_index(dx->level[dd],ll,s)>=0 )
           ERROR_exit("dataTable: duplicate requested level '%s' for column '%s'",s,columns[dd]) ;
         dx->level[dd][ll]=strdup(s) ;
       }
     } else {
       for( rr=0 ; rr<dt->nrow ; rr++ ){
         const char *s=DT_CELL(dt,rr,dx->icol[dd]) ;
         if( s==NULL || *s=='\0' )
           ERROR_exit("dataTable (%s): empty '%s' key in row %d",
                      (dt->source!=NULL)?dt->source:"?",columns[dd],rr+1) ;
         if( dt_level_index(dx->level[dd],dx->nlevel[dd],s)<0 ){
           dx->level[dd]=(char **)realloc(dx->level[dd],
                                          sizeof(char *)*(dx->nlevel[dd]+1)) ;
           dx->level[dd][dx->nlevel[dd]++]=strdup(s) ;
         }
       }
       if( caller_nlevel!=NULL && caller_nlevel[dd]==THD_DT_LEVELS_LEXICAL )
         qsort(dx->level[dd],dx->nlevel[dd],sizeof(char *),dt_string_cmp) ;
     }
     if( dx->nlevel[dd]<1 ) ERROR_exit("dataTable: index column '%s' has no levels",columns[dd]) ;
     nc *= dx->nlevel[dd] ;
     if( nc > INT_MAX ) ERROR_exit("dataTable: requested Cartesian index is too large") ;
   }

   dx->ncell=(int)nc ; dx->stride[ndim-1]=1 ;
   for( dd=ndim-2 ; dd>=0 ; dd-- )
     dx->stride[dd]=dx->stride[dd+1]*dx->nlevel[dd+1] ;
   dx->row_coord=(int *)malloc(sizeof(int)*(size_t)dt->nrow*ndim) ;
   dx->row_of=(int *)malloc(sizeof(int)*dx->ncell) ;
   for( cc=0 ; cc<dx->ncell ; cc++ ) dx->row_of[cc]=-1 ;

   for( rr=0 ; rr<dt->nrow ; rr++ ){
     int flat=0 ;
     for( dd=0 ; dd<ndim ; dd++ ){
       const char *s=DT_CELL(dt,rr,dx->icol[dd]) ;
       int ll=dt_level_index(dx->level[dd],dx->nlevel[dd],s) ;
       if( ll<0 )
         ERROR_exit("dataTable (%s): unexpected %s '%s' in row %d",
                    (dt->source!=NULL)?dt->source:"?",dx->column[dd],s,rr+1) ;
       dx->row_coord[(size_t)rr*ndim+dd]=ll ; flat+=ll*dx->stride[dd] ;
     }
     if( dx->row_of[flat]>=0 ){
       char key[1024] ; int used=0 ; key[0]='\0' ;
       for( dd=0 ; dd<ndim && used<(int)sizeof(key)-1 ; dd++ ){
         int got=snprintf(key+used,sizeof(key)-used,"%s%s=%s",dd?", ":"",
                          dx->column[dd],DT_CELL(dt,rr,dx->icol[dd])) ;
         if( got<0 ) break ;
         used += (got<(int)sizeof(key)-used) ? got : (int)sizeof(key)-used-1 ;
       }
       ERROR_exit("dataTable (%s): duplicate key {%s} in rows %d and %d",
                  (dt->source!=NULL)?dt->source:"?",key,dx->row_of[flat]+1,rr+1) ;
     }
     dx->row_of[flat]=rr ;
   }

   for( cc=0 ; cc<dx->ncell ; cc++ ) if( dx->row_of[cc]<0 ){
     char key[1024] ; int used=0,rem=cc ; key[0]='\0' ;
     for( dd=0 ; dd<ndim ; dd++ ){
       int ll=rem/dx->stride[dd] ; rem%=dx->stride[dd] ;
       if( used<(int)sizeof(key)-1 ){
         int got=snprintf(key+used,sizeof(key)-used,"%s%s=%s",dd?", ":"",
                          dx->column[dd],dx->level[dd][ll]) ;
         if( got>=0 )
           used += (got<(int)sizeof(key)-used) ? got : (int)sizeof(key)-used-1 ;
       }
     }
     ERROR_exit("dataTable (%s): incomplete Cartesian table; missing {%s}",
                (dt->source!=NULL)?dt->source:"?",key) ;
   }
   return dx ;
}

void THD_free_datatable_index( THD_datatable_index *dx )
{
   int dd,ll ; if( dx==NULL ) return ;
   for( dd=0 ; dd<dx->ndim ; dd++ ){
     free(dx->column[dd]) ;
     for( ll=0 ; ll<dx->nlevel[dd] ; ll++ ) free(dx->level[dd][ll]) ;
     free(dx->level[dd]) ;
   }
   free(dx->icol); free(dx->nlevel); free(dx->stride); free(dx->column);
   free(dx->level); free(dx->row_coord); free(dx->row_of); free(dx) ;
}

THD_datatable * THD_datatable_select_rows( THD_datatable *dt ,
                                           int *rows , int nrow )
{
   THD_datatable *out ; int ii,jj,rr ; char bb[1024] ;
   if( dt==NULL || rows==NULL || nrow<1 ) return NULL ;
   out=(THD_datatable *)calloc(1,sizeof(THD_datatable)) ;
   out->ncol=dt->ncol ; out->nrow=nrow ; out->from_argv=dt->from_argv ;
   snprintf(bb,sizeof(bb),"subject rows selected from %s",
            (dt->source!=NULL)?dt->source:"dataTable") ;
   out->source=strdup(bb) ;
   out->cname=(char **)calloc(out->ncol,sizeof(char *)) ;
   for( jj=0 ; jj<out->ncol ; jj++ ) out->cname[jj]=strdup(dt->cname[jj]) ;
   out->cell=(char **)calloc((size_t)nrow*out->ncol,sizeof(char *)) ;
   for( ii=0 ; ii<nrow ; ii++ ){
     rr=rows[ii] ;
     if( rr<0 || rr>=dt->nrow ){
       THD_free_datatable(out) ;
       ERROR_exit("dataTable: selected row index %d is outside 0..%d",rr,dt->nrow-1) ;
     }
     for( jj=0 ; jj<out->ncol ; jj++ )
       DT_CELL(out,ii,jj)=strdup(DT_CELL(dt,rr,jj)) ;
   }
   if( !dt_finish(out) ) ERROR_exit("dataTable: cannot allocate selected-row metadata") ;
   return out ;
}

THD_datatable * THD_datatable_concat_rows( const THD_datatable *first,
                                            const THD_datatable *second,
                                            char *err, int nerr )
{
   const THD_datatable *tables[2]={first,second};
   THD_datatable *out=NULL;int *map=NULL,s,c,j,r,nrow,ncol;
#define CONCAT_FAIL(...) do{ if(err && nerr>0)snprintf(err,(size_t)nerr,__VA_ARGS__); goto fail; }while(0)
   if(err && nerr>0)err[0]='\0';
   for(s=0;s<2;s++)if(!tables[s] || tables[s]->nrow<1 || tables[s]->ncol<1)
      CONCAT_FAIL("row concatenation requires two nonempty tables");
   if(first->ncol!=second->ncol)
      CONCAT_FAIL("tables must have the same column names (%d versus %d columns)",first->ncol,second->ncol);
   ncol=first->ncol;
   /* DT_CELL and legacy table consumers use int row/column products. */
   if(first->nrow>INT_MAX-second->nrow ||
      first->nrow+second->nrow>INT_MAX/ncol)
      CONCAT_FAIL("concatenated table has too many rows or cells");
   nrow=first->nrow+second->nrow;
   if((size_t)nrow*ncol>SIZE_MAX/sizeof(char *) ||
      (size_t)ncol>SIZE_MAX/sizeof(int) ||
      (size_t)nrow>SIZE_MAX/sizeof(float))
      CONCAT_FAIL("concatenated table allocation is too large");
   for(s=0;s<2;s++){
      const THD_datatable *dt=tables[s];
      if(!dt->cname || !dt->cell)CONCAT_FAIL("table %d has no column names or cells",s+1);
      for(c=0;c<ncol;c++){
         if(!dt->cname[c] || !dt->cname[c][0])CONCAT_FAIL("table %d has an empty column name",s+1);
         for(j=0;j<c;j++)if(!strcasecmp(dt->cname[c],dt->cname[j]))
            CONCAT_FAIL("table %d has duplicate column '%s'",s+1,dt->cname[c]);
      }
   }
   map=malloc(sizeof(int)*(size_t)ncol);
   if(!map)CONCAT_FAIL("cannot allocate concatenation column map");
   for(c=0;c<ncol;c++){
      for(j=0;j<ncol;j++)if(!strcasecmp(first->cname[c],second->cname[j]))break;
      if(j==ncol)CONCAT_FAIL("second table is missing column '%s'; both tables must have the same column names",first->cname[c]);
      map[c]=j;
   }
   out=calloc(1,sizeof(*out));
   if(!out)CONCAT_FAIL("cannot allocate concatenated table");
   out->nrow=nrow;out->ncol=ncol;
   out->source=strdup("concatenated table rows");
   out->cname=calloc((size_t)ncol,sizeof(char *));
   out->cell=calloc((size_t)nrow*ncol,sizeof(char *));
   if(!out->source || !out->cname || !out->cell)CONCAT_FAIL("cannot allocate concatenated table storage");
   for(c=0;c<ncol;c++)if(!(out->cname[c]=strdup(first->cname[c])))
      CONCAT_FAIL("cannot copy concatenated column names");
   for(s=0;s<2;s++)for(r=0;r<tables[s]->nrow;r++)for(c=0;c<ncol;c++){
      const char *value=DT_CELL(tables[s],r,s?map[c]:c);
      int row=s?first->nrow+r:r;
      if(!value)CONCAT_FAIL("table %d has a missing cell at row %d, column '%s'",s+1,r+1,first->cname[c]);
      if(!(DT_CELL(out,row,c)=strdup(value)))CONCAT_FAIL("cannot copy concatenated table cells");
   }
   if(!dt_finish(out))CONCAT_FAIL("cannot allocate concatenated table metadata");
   free(map);return out;
fail:
   free(map);THD_free_datatable(out);return NULL;
#undef CONCAT_FAIL
}

/*! A cell as it must be written to read back: quoted if it is empty, holds
    white space, or begins with a quote or a '#' (which would start a comment). */

static void dt_print_cell( FILE *fp , const char *s , int last )
{
   int need = (s[0] == '\0' || s[0] == '#' || s[0] == '"' || s[0] == '\'') , ii ; char q='"' ;
   for( ii=0 ; s[ii] != '\0' ; ii++ ) if( isspace((int)s[ii]) ) need = 1 ;
   if( strchr(s,'"') != NULL ) q = '\'' ;
   if( need ){
     char b[THD_MAX_NAME+4] ; snprintf(b,sizeof(b),"%c%s%c",q,s,q) ;
     fprintf(fp,"%-16s%s",b,last ? "\n" : " ") ;
   } else fprintf(fp,"%-16s%s",s,last ? "\n" : " ") ;
}

void THD_datatable_print( THD_datatable *dt , FILE *fp )
{
   int ii , jj ;
   if( dt == NULL ) return ;
   if( fp == NULL ) fp = stdout ;

   fprintf(fp,"# dataTable from %s: %d rows x %d columns\n",
           (dt->source != NULL) ? dt->source : "?" , dt->nrow , dt->ncol) ;
   for( jj=0 ; jj < dt->ncol ; jj++ )
     dt_print_cell( fp , dt->cname[jj] , jj==dt->ncol-1 ) ;
   for( ii=0 ; ii < dt->nrow ; ii++ ){
     for( jj=0 ; jj < dt->ncol ; jj++ )
       dt_print_cell( fp , DT_CELL(dt,ii,jj) , jj==dt->ncol-1 ) ;
   }
   fprintf(fp,"# numeric columns:") ;
   for( jj=0 ; jj < dt->ncol ; jj++ )
     if( dt->isnum[jj] ) fprintf(fp," %s",dt->cname[jj]) ;
   fprintf(fp,"\n") ;
}

void THD_free_datatable( THD_datatable *dt )
{
   int ii , jj ;
   if( dt == NULL ) return ;

   if( dt->cell != NULL ){
     for( ii=0 ; ii < dt->nrow*dt->ncol ; ii++ ) if( dt->cell[ii] ) free(dt->cell[ii]) ;
     free(dt->cell) ;
   }
   if( dt->cname != NULL ){
     for( jj=0 ; jj < dt->ncol ; jj++ ) if( dt->cname[jj] ) free(dt->cname[jj]) ;
     free(dt->cname) ;
   }
   if( dt->val != NULL ){
     for( jj=0 ; jj < dt->ncol ; jj++ ) if( dt->val[jj] ) free(dt->val[jj]) ;
     free(dt->val) ;
   }
   if( dt->subj != NULL ){
     for( ii=0 ; ii < dt->nrow ; ii++ ) if( dt->subj[ii] ) free(dt->subj[ii]) ;
     free(dt->subj) ;
   }
   if( dt->fname != NULL ){
     for( ii=0 ; ii < dt->nrow ; ii++ ) if( dt->fname[ii] ) free(dt->fname[ii]) ;
     free(dt->fname) ;
   }
   if( dt->isnum  != NULL ) free(dt->isnum) ;
   if( dt->source != NULL ) free(dt->source) ;
   free(dt) ;
}

void THD_1D_print_token(FILE *fp,const char *text)
{
   const char *p;int quote=0;
   if(!text)text="NA";
   if(!*text || *text=='#' || *text=='\'' || *text=='"')quote=1;
   for(p=text;*p;p++)if(isspace((unsigned char)*p) || *p=='"')quote=1;
   if(!quote){fputs(text,fp);return;}
   fputc('"',fp);for(p=text;*p;p++){if(*p=='"')fputc('"',fp);fputc(*p,fp);}fputc('"',fp);
}
