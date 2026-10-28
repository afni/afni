#include "mrilib.h"
#include "thd_xmat.h"
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

/*----------------------------------------------------------------------------
  Design matrices (X.xmat.1D).  See thd_xmat.h for the format and the API.
                                                     -- P Molfese, Oct 2026
------------------------------------------------------------------------------*/

static void xm_err( char *err , int nerr , const char *fmt , const char *a )
{
   if( err != NULL && nerr > 0 ) snprintf(err,(size_t)nerr,fmt,a) ;
}

/*! A ';'-separated string attribute, each entry trimmed. */
static NI_str_array * xm_strings( NI_element *nel , const char *name )
{
   char *a = NI_get_attribute_nocase(nel,(char *)name) ; NI_str_array *sar ; int ii ;
   if( a == NULL ) return NULL ;
   sar = NI_decode_string_list(a,";") ;
   if( sar == NULL ) return NULL ;
   for( ii=0 ; ii < sar->num ; ii++ ){
     char *p = sar->str[ii] , *e ;
     while( isspace((unsigned char)*p) ) p++ ;
     memmove(sar->str[ii],p,strlen(p)+1) ;
     e = sar->str[ii] + strlen(sar->str[ii]) ;
     while( e > sar->str[ii] && isspace((unsigned char)e[-1]) ) *(--e) = '\0' ;
   }
   return sar ;
}

static int * xm_ints( NI_element *nel , const char *name , int *n )
{
   char *a = NI_get_attribute_nocase(nel,(char *)name) ; NI_int_array *ia ; int *v ;
   *n = 0 ;
   if( a == NULL ) return NULL ;
   ia = NI_decode_int_list(a,";,") ;
   if( ia == NULL ) return NULL ;
   *n = ia->num ; v = (int *)malloc(sizeof(int)*(size_t)(ia->num>0?ia->num:1)) ;
   memcpy(v,ia->ar,sizeof(int)*(size_t)ia->num) ;
   NI_delete_int_array(ia) ;
   return v ;
}

/* A design column belongs to at most one stimulus. Also protect callers of
   THD_xmat_items() that construct a THD_xmat without using the file reader. */
static int xm_stim_ranges_valid( const THD_xmat *xm )
{
   int s,t ;
   if( xm==NULL || xm->ncol<1 || xm->nstim<0 ||
       (xm->nstim>0 && xm->stim==NULL) ) return 0 ;
   for( s=0 ; s<xm->nstim ; s++ ){
     int bot=xm->stim[s].bot,top=xm->stim[s].top ;
     if( bot<0 || top<bot || top>=xm->ncol ) return 0 ;
     for( t=0 ; t<s ; t++ )
       if( bot<=xm->stim[t].top && xm->stim[t].bot<=top ) return 0 ;
   }
   return 1 ;
}

THD_xmat * THD_xmat_read( const char *fname , char *err , int nerr )
{
   NI_element *nel ; THD_xmat *xm ; NI_str_array *sar ; int ii , c , r , nb , nt ;
   int *bot , *top ; char *a ;

   if( fname == NULL ){ xm_err(err,nerr,"%s","no file name") ; return NULL ; }
   nel = (NI_element *)NI_read_element_fromfile((char *)fname) ;
   if( nel == NULL || nel->type != NI_ELEMENT_TYPE ){
     xm_err(err,nerr,"cannot read '%s' as an X.xmat.1D (a NIML matrix from 3dDeconvolve)",fname) ;
     if( nel != NULL ) NI_free_element(nel) ;
     return NULL ;
   }
   xm = (THD_xmat *)calloc(1,sizeof(THD_xmat)) ;
   xm->fname = strdup(fname) ;
   xm->ncol = nel->vec_num ; xm->nrow = nel->vec_len ;
   if( xm->ncol < 1 || xm->nrow < 1 ){
     xm_err(err,nerr,"'%s' holds no matrix",fname) ; NI_free_element(nel) ; THD_xmat_free(xm) ; return NULL ;
   }

   /* the matrix, as float, column by column */
   xm->mat = (float *)malloc(sizeof(float)*(size_t)xm->ncol*(size_t)xm->nrow) ;
   for( c=0 ; c < xm->ncol ; c++ ){
     float *col = xm->mat + (size_t)c*xm->nrow ;
     for( r=0 ; r < xm->nrow ; r++ ){
       switch( nel->vec_typ[c] ){
         case NI_FLOAT:  col[r] = ((float  *)nel->vec[c])[r] ; break ;
         case NI_DOUBLE: col[r] = (float)((double *)nel->vec[c])[r] ; break ;
         case NI_INT:    col[r] = (float)((int    *)nel->vec[c])[r] ; break ;
         default:
           xm_err(err,nerr,"'%s' has a matrix column that is not numeric",fname) ;
           NI_free_element(nel) ; THD_xmat_free(xm) ; return NULL ;
       }
     }
   }

   /* every header attribute, as written */
   xm->nattr = nel->attr_num ;
   xm->attr_name  = (char **)calloc((size_t)(xm->nattr>0?xm->nattr:1),sizeof(char *)) ;
   xm->attr_value = (char **)calloc((size_t)(xm->nattr>0?xm->nattr:1),sizeof(char *)) ;
   for( ii=0 ; ii < xm->nattr ; ii++ ){
     xm->attr_name[ii]  = strdup(nel->attr_lhs[ii] ? nel->attr_lhs[ii] : "") ;
     xm->attr_value[ii] = strdup(nel->attr_rhs[ii] ? nel->attr_rhs[ii] : "") ;
   }

   /* column labels */
   xm->collab = (char **)calloc((size_t)xm->ncol,sizeof(char *)) ;
   sar = xm_strings(nel,"ColumnLabels") ;
   for( c=0 ; c < xm->ncol ; c++ ){
     char b[32] ;
     if( sar != NULL && c < sar->num ) xm->collab[c] = strdup(sar->str[c]) ;
     else { snprintf(b,sizeof(b),"col#%d",c) ; xm->collab[c] = strdup(b) ; }
   }
   if( sar != NULL ) NI_delete_str_array(sar) ;

   /* the timeline: full length, the rows kept, the runs */
   a = NI_get_attribute_nocase(nel,"NRowFull") ;
   xm->nfull = (a != NULL) ? (int)strtod(a,NULL) : xm->nrow ;
   xm->good = xm_ints(nel,"GoodList",&nt) ;
   if( xm->good == NULL ){
     xm->good = (int *)malloc(sizeof(int)*(size_t)xm->nrow) ;
     for( r=0 ; r < xm->nrow ; r++ ) xm->good[r] = r ;
     nt = xm->nrow ;
   }
   if( nt != xm->nrow || xm->nfull < xm->nrow ){
     xm_err(err,nerr,"'%s': GoodList and NRowFull do not match the matrix",fname) ;
     NI_free_element(nel) ; THD_xmat_free(xm) ; return NULL ;
   }
   for( r=0 ; r < xm->nrow ; r++ )
     if( xm->good[r] < 0 || xm->good[r] >= xm->nfull || (r > 0 && xm->good[r] <= xm->good[r-1]) ){
       xm_err(err,nerr,"'%s': GoodList has bad values",fname) ;
       NI_free_element(nel) ; THD_xmat_free(xm) ; return NULL ;
     }
   xm->runstart = xm_ints(nel,"RunStart",&xm->nrun) ;
   if( xm->runstart == NULL || xm->nrun < 1 ){
     free(xm->runstart) ; xm->runstart = (int *)calloc(1,sizeof(int)) ; xm->nrun = 1 ;
   }
   for( ii=0 ; ii < xm->nrun ; ii++ )
     if( xm->runstart[ii] < 0 || xm->runstart[ii] >= xm->nfull || (ii == 0 && xm->runstart[ii] != 0)
         || (ii > 0 && xm->runstart[ii] <= xm->runstart[ii-1]) ){
       xm_err(err,nerr,"'%s': RunStart has bad values",fname) ;
       NI_free_element(nel) ; THD_xmat_free(xm) ; return NULL ;
     }
   a = NI_get_attribute_nocase(nel,"RowTR") ;
   xm->tr = (a != NULL) ? (float)strtod(a,NULL) : 0.0f ;

   /* the stimuli */
   sar = xm_strings(nel,"StimLabels") ;
   bot = xm_ints(nel,"StimBots",&nb) ; top = xm_ints(nel,"StimTops",&nt) ;
   if( sar != NULL && bot != NULL && top != NULL && nb == sar->num && nt == sar->num ){
     xm->nstim = sar->num ;
     xm->stim = (THD_xmat_stim *)calloc((size_t)(xm->nstim>0?xm->nstim:1),sizeof(THD_xmat_stim)) ;
     for( ii=0 ; ii < xm->nstim ; ii++ ){
       char an[64] ;
       xm->stim[ii].label = strdup(sar->str[ii]) ;
       xm->stim[ii].bot = bot[ii] ; xm->stim[ii].top = top[ii] ;
       if( bot[ii] < 0 || top[ii] < bot[ii] || top[ii] >= xm->ncol ){
         xm_err(err,nerr,"'%s': a stimulus has columns outside the matrix",fname) ;
         NI_delete_str_array(sar) ; free(bot) ; free(top) ; NI_free_element(nel) ; THD_xmat_free(xm) ; return NULL ;
       }
       snprintf(an,sizeof(an),"BasisOption_%06d",ii+1) ; a = NI_get_attribute_nocase(nel,an) ;
       xm->stim[ii].option = (a != NULL) ? strdup(a) : NULL ;
       snprintf(an,sizeof(an),"BasisFormula_%06d",ii+1) ; a = NI_get_attribute_nocase(nel,an) ;
       xm->stim[ii].formula = (a != NULL) ? strdup(a) : NULL ;
     }
     if( !xm_stim_ranges_valid(xm) ){
       xm_err(err,nerr,"'%s': stimulus column ranges overlap",fname) ;
       NI_delete_str_array(sar) ; free(bot) ; free(top) ; NI_free_element(nel) ; THD_xmat_free(xm) ; return NULL ;
     }
   }
   if( sar != NULL ) NI_delete_str_array(sar) ;
   free(bot) ; free(top) ;
   NI_free_element(nel) ;
   return xm ;
}

void THD_xmat_free( THD_xmat *xm )
{
   int ii ;
   if( xm == NULL ) return ;
   free(xm->fname) ; free(xm->mat) ; free(xm->good) ; free(xm->runstart) ;
   if( xm->collab != NULL ){ for( ii=0 ; ii < xm->ncol ; ii++ ) free(xm->collab[ii]) ; free(xm->collab) ; }
   if( xm->stim != NULL ){
     for( ii=0 ; ii < xm->nstim ; ii++ ){ free(xm->stim[ii].label) ; free(xm->stim[ii].option) ; free(xm->stim[ii].formula) ; }
     free(xm->stim) ;
   }
   if( xm->attr_name != NULL ){
     for( ii=0 ; ii < xm->nattr ; ii++ ){ free(xm->attr_name[ii]) ; free(xm->attr_value[ii]) ; }
     free(xm->attr_name) ; free(xm->attr_value) ;
   }
   free(xm) ;
}

const char * THD_xmat_attr( const THD_xmat *xm , const char *name )
{
   int ii ;
   if( xm == NULL || name == NULL ) return NULL ;
   for( ii=0 ; ii < xm->nattr ; ii++ ) if( strcasecmp(xm->attr_name[ii],name) == 0 ) return xm->attr_value[ii] ;
   return NULL ;
}

int THD_xmat_run_of( const THD_xmat *xm , int t )
{
   int r ;
   if( xm == NULL || t < 0 || t >= xm->nfull ) return -1 ;
   for( r=0 ; r+1 < xm->nrun && t >= xm->runstart[r+1] ; r++ ) ;
   return r ;
}

int THD_xmat_run_length( const THD_xmat *xm , int r )
{
   if( xm == NULL || r < 0 || r >= xm->nrun ) return 0 ;
   return ((r+1 < xm->nrun) ? xm->runstart[r+1] : xm->nfull) - xm->runstart[r] ;
}

unsigned char * THD_xmat_censored( const THD_xmat *xm )
{
   unsigned char *c ; int r ;
   if( xm == NULL ) return NULL ;
   c = (unsigned char *)malloc((size_t)xm->nfull) ; memset(c,1,(size_t)xm->nfull) ;
   for( r=0 ; r < xm->nrow ; r++ ) c[xm->good[r]] = 0 ;
   return c ;
}

int THD_xmat_ncensored( const THD_xmat *xm )
{
   return (xm == NULL) ? 0 : xm->nfull - xm->nrow ;
}

unsigned char * THD_xmat_stims_wanted( const THD_xmat *xm , const char *list , int *nfound )
{
   unsigned char *w ; int s , nf=0 ;
   if( xm == NULL ) return NULL ;
   w = (unsigned char *)calloc((size_t)(xm->nstim>0?xm->nstim:1),1) ;
   for( s=0 ; s < xm->nstim ; s++ ){
     if( list == NULL ) w[s] = 1 ;
     else {
       char *copy=strdup(list) , *tok ;
       for( tok=strtok(copy,",") ; tok != NULL && !w[s] ; tok=strtok(NULL,",") ) w[s] = (strcmp(tok,xm->stim[s].label) == 0) ;
       free(copy) ;
     }
     nf += w[s] ;
   }
   if( nfound != NULL ) *nfound = nf ;
   return w ;
}

int * THD_xmat_condition_labels( const THD_xmat *xm , double frac , const unsigned char *wanted )
{
   int *which , s , c , r , t ; double *best ;
   if( xm == NULL ) return NULL ;
   which = (int *)malloc(sizeof(int)*(size_t)xm->nfull) ; best = (double *)malloc(sizeof(double)*(size_t)xm->nfull) ;
   for( t=0 ; t < xm->nfull ; t++ ){ which[t] = -1 ; best[t] = -1.0 ; }
   for( s=0 ; s < xm->nstim ; s++ ){
     double mx=0.0 ;
     if( wanted != NULL && !wanted[s] ) continue ;
     for( c=xm->stim[s].bot ; c <= xm->stim[s].top ; c++ )
       for( r=0 ; r < xm->nrow ; r++ ){ double v=fabs(xm->mat[(size_t)c*xm->nrow+r]) ; if( v > mx ) mx = v ; }
     if( mx <= 0.0 ) continue ;
     for( r=0 ; r < xm->nrow ; r++ ){
       double v=0.0 ; t = xm->good[r] ;
       for( c=xm->stim[s].bot ; c <= xm->stim[s].top ; c++ ){ double u=fabs(xm->mat[(size_t)c*xm->nrow+r])/mx ; if( u > v ) v = u ; }
       if( v >= frac && v > best[t] ){ best[t] = v ; which[t] = s ; }
     }
   }
   free(best) ;
   return which ;
}

THD_xmat_item * THD_xmat_items( const THD_xmat *xm , const unsigned char *wanted , int *nitem )
{
   THD_xmat_item *it ; int s , c , r , n=0 ;
   if( nitem == NULL ) return NULL ;
   *nitem = 0 ;
   if( !xm_stim_ranges_valid(xm) ) return NULL ;
   it = (THD_xmat_item *)calloc((size_t)(xm->ncol>0?xm->ncol:1),sizeof(THD_xmat_item)) ;
   if( it == NULL ) return NULL ;
   for( s=0 ; s < xm->nstim ; s++ ){
     if( wanted != NULL && !wanted[s] ) continue ;
     for( c=xm->stim[s].bot ; c <= xm->stim[s].top ; c++ ){
       THD_xmat_item *q = it + n ; char b[THD_MAX_NAME] ; double mx=0.0 ; int run=-1 , one=1 ;
       q->key = strdup( (xm->stim[s].top > xm->stim[s].bot) ? xm->collab[c] : xm->stim[s].label ) ;
       q->condition = strdup(xm->stim[s].label) ;
       snprintf(b,sizeof(b),"%s_Coef",xm->collab[c]) ; q->coef_label = strdup(b) ;
       q->stim = s ; q->column = c ;
       for( r=0 ; r < xm->nrow ; r++ ){ double v=fabs(xm->mat[(size_t)c*xm->nrow+r]) ; if( v > mx ) mx = v ; }
       for( r=0 ; r < xm->nrow && one ; r++ ) if( fabs(xm->mat[(size_t)c*xm->nrow+r]) > 1.0e-6*mx ){
         int rr = THD_xmat_run_of(xm,xm->good[r]) ;
         if( run < 0 ) run = rr ; else if( rr != run ) one = 0 ;
       }
       q->run = (one && mx > 0.0 && xm->nrun > 1) ? run : -1 ;
       n++ ;
     }
   }
   *nitem = n ;
   return it ;
}

void THD_xmat_items_free( THD_xmat_item *it , int nitem )
{
   int ii ;
   if( it == NULL ) return ;
   for( ii=0 ; ii < nitem ; ii++ ){ free(it[ii].key) ; free(it[ii].condition) ; free(it[ii].coef_label) ; }
   free(it) ;
}

/* Run extraction and serialization. Metadata is rebuilt in one place so the
   in-memory object, its attributes, and the on-disk matrix agree. */
static void xm_set(THD_xmat *x, const char *key, const char *val)
{
   int i;
   for(i=0;i<x->nattr;i++) if(!strcasecmp(x->attr_name[i],key)) break;
   if(i==x->nattr){
      x->attr_name=realloc(x->attr_name,sizeof(char *)*(i+1));
      x->attr_value=realloc(x->attr_value,sizeof(char *)*(i+1));
      x->attr_name[i]=strdup(key); x->attr_value[i]=NULL; x->nattr++;
   }
   free(x->attr_value[i]); x->attr_value[i]=strdup(val);
}
static void xm_setpath(THD_xmat *x,const char *key,const char *path)
{
   /* A relative input name must not be reinterpreted beside the output. */
   char *absolute=path?realpath(path,NULL):NULL;
   xm_set(x,key,absolute?absolute:(path?path:"")); free(absolute);
}
static void xm_setint(THD_xmat *x,const char *key,int value)
{
   char buf[40]; snprintf(buf,sizeof(buf),"%d",value); xm_set(x,key,buf);
}
static void xm_setlist(THD_xmat *x,const char *key,int n,int *v)
{
   NI_int_array a; char *s;
   a.num=n; a.ar=v; s=NI_encode_int_list(&a,",");
   xm_set(x,key,s?s:""); if(s) NI_free(s);
}
static int xm_core_attr(const char *k)
{
   return !strcasecmp(k,"ColumnLabels") || !strcasecmp(k,"GoodList") ||
          !strcasecmp(k,"RunStart") || !strcasecmp(k,"NRowFull") ||
          !strcasecmp(k,"RowTR") || !strcasecmp(k,"Nstim") ||
          !strcasecmp(k,"StimLabels") || !strcasecmp(k,"StimBots") ||
          !strcasecmp(k,"StimTops");
}
static int xm_removed_attr(const char *k)
{
   return xm_core_attr(k) || !strncasecmp(k,"Basis",5) ||
          !strncasecmp(k,"Glt",3) || !strcasecmp(k,"Nglt") ||
          !strcasecmp(k,"ColumnMasks") || !strcasecmp(k,"ColumnGroups") ||
          !strncasecmp(k,"XmatSource",10) || !strcasecmp(k,"XmatSupportFile") ||
          !strcasecmp(k,"XmatZeroTolerance") || !strcasecmp(k,"CommandLine");
}
static void xm_sync_core(THD_xmat *x)
{
   char *s=NULL,buf[64]; int i,*v;
   for(i=0;i<x->ncol;i++) s=THD_zzprintf(s,"%s%s",i?" ; ":"",x->collab[i]);
   xm_set(x,"ColumnLabels",s?s:""); free(s);
   xm_setint(x,"NRowFull",x->nfull);
   xm_setlist(x,"GoodList",x->nrow,x->good);
   xm_setlist(x,"RunStart",x->nrun,x->runstart);
   snprintf(buf,sizeof(buf),"%.9g",x->tr); xm_set(x,"RowTR",buf);
   xm_setint(x,"Nstim",x->nstim);
   v=calloc((size_t)(x->nstim?x->nstim:1),sizeof(int)); s=NULL;
   for(i=0;i<x->nstim;i++){
      v[i]=x->stim[i].bot;
      s=THD_zzprintf(s,"%s%s",i?" ; ":"",x->stim[i].label);
   }
   xm_set(x,"StimLabels",s?s:""); free(s);
   xm_setlist(x,"StimBots",x->nstim,v);
   for(i=0;i<x->nstim;i++) v[i]=x->stim[i].top;
   xm_setlist(x,"StimTops",x->nstim,v); free(v);
}
static int xm_structure_valid(const THD_xmat *x)
{
   int r,c;
   if(!x || !xm_stim_ranges_valid(x) || x->nrow<1 || x->nfull<x->nrow ||
      x->nrun<1 || !x->runstart || !x->good || !x->mat || !x->collab) return 0;
   for(r=0;r<x->nrun;r++)
      if(x->runstart[r]<0 || x->runstart[r]>=x->nfull ||
         (!r && x->runstart[r]!=0) || (r && x->runstart[r]<=x->runstart[r-1])) return 0;
   for(r=0;r<x->nrow;r++)
      if(x->good[r]<0 || x->good[r]>=x->nfull || (r && x->good[r]<=x->good[r-1])) return 0;
   for(c=0;c<x->ncol;c++) if(!x->collab[c]) return 0;
   for(c=0;c<x->nstim;c++) if(!x->stim[c].label) return 0;
   return isfinite(x->tr) && x->tr>=0;
}
static int xm_stim_at(const THD_xmat *x,int c)
{
   int s;
   for(s=0;s<x->nstim;s++) if(c>=x->stim[s].bot && c<=x->stim[s].top) return s;
   return -1;
}
static int xm_im(const THD_xmat *x,int s)
{
   return s>=0 && x->stim[s].option && !strcmp(x->stim[s].option,"-stim_times_IM");
}

void THD_xmat_run_map_free(THD_xmat_run_map *m)
{
   if(!m) return;
   free(m->rows); free(m->columns); free(m->inverse); free(m);
}

/* Shared by run extraction and arbitrary column selection. */
static int xm_subset(const THD_xmat *a,THD_xmat *x,const THD_xmat_run_map *m,
                     char *err,int nerr)
{
   int nr=m->nrow,nc=m->ncol,start=m->start,r,c,s,i,j;
   int *sm=NULL,*g=NULL; NI_int_array *groups=NULL; const char *v;
   char key[128],buf[128];
#define XM_FAIL(MSG) do{xm_err(err,nerr,"%s",MSG);goto fail;}while(0)
   /* This new object has no file identity until it is written and reread. */
   x->nrow=nr; x->ncol=nc;
   x->nfull=m->length; x->nrun=m->run<0?a->nrun:1; x->tr=a->tr;
   x->runstart=calloc((size_t)x->nrun,sizeof(int));
   if(m->run<0 && x->runstart) memcpy(x->runstart,a->runstart,sizeof(int)*a->nrun); x->good=malloc(sizeof(int)*(size_t)nr);
   x->mat=malloc(sizeof(float)*(size_t)nr*nc); x->collab=calloc((size_t)nc,sizeof(char *));
   x->stim=calloc((size_t)(a->nstim?a->nstim:1),sizeof(THD_xmat_stim));
   sm=malloc(sizeof(int)*(size_t)(a->nstim?a->nstim:1)); g=malloc(sizeof(int)*(size_t)nc);
   if(!x->runstart || !x->good || !x->mat || !x->collab || !x->stim || !sm || !g)
      XM_FAIL("out of memory");
   for(r=0;r<nr;r++) x->good[r]=a->good[m->rows[r]]-start;
   for(c=0;c<nc;c++){
      x->collab[c]=strdup(a->collab[m->columns[c]]);
      for(r=0;r<nr;r++) x->mat[(size_t)c*nr+r]=a->mat[(size_t)m->columns[c]*a->nrow+m->rows[r]];
   }
   for(s=0;s<a->nstim;s++){
      int bot=-1,top=-1;
      sm[s]=-1;
      for(c=a->stim[s].bot;c<=a->stim[s].top;c++) if(m->inverse[c]>=0){
         if(bot<0) bot=m->inverse[c]; top=m->inverse[c];
      }
      if(bot>=0){
         THD_xmat_stim *t=x->stim+x->nstim; sm[s]=x->nstim++;
         t->bot=bot; t->top=top; t->label=strdup(a->stim[s].label);
         if(a->stim[s].option) t->option=strdup(a->stim[s].option);
         if(a->stim[s].formula) t->formula=strdup(a->stim[s].formula);
      }
   }
   for(i=0;i<a->nattr;i++) if(!xm_removed_attr(a->attr_name[i]))
      xm_set(x,a->attr_name[i],a->attr_value[i]);
   /* Renumber all per-stimulus Basis attributes, including future ones. */
   for(s=0;s<a->nstim;s++) if(sm[s]>=0){
      char suffix[24]; snprintf(suffix,sizeof(suffix),"_%06d",s+1);
      for(i=0;i<a->nattr;i++){
         const char *k=a->attr_name[i],*p=strchr(k,'_');
         if(!strncasecmp(k,"Basis",5) && p && !strcmp(p,suffix)){
            snprintf(key,sizeof(key),"%.*s_%06d",(int)(p-k),k,sm[s]+1);
            xm_set(x,key,a->attr_value[i]);
         }
      }
      j=sm[s];
      snprintf(key,sizeof(key),"BasisColumns_%06d",j+1);
      snprintf(buf,sizeof(buf),"%d:%d",x->stim[j].bot,x->stim[j].top); xm_set(x,key,buf);
      snprintf(key,sizeof(key),"BasisName_%06d",j+1); xm_set(x,key,x->stim[j].label);
   }
   xm_setint(x,"BasisNstim",x->nstim);
   v=THD_xmat_attr(a,"ColumnGroups");
   if(v){
      groups=NI_decode_int_list((char *)v,",;");
      if(!groups || groups->num!=a->ncol) XM_FAIL("ColumnGroups length does not match the matrix");
      for(c=0;c<a->ncol;c++) if(groups->ar[c]>0 &&
         (groups->ar[c]>a->nstim || xm_stim_at(a,c)!=groups->ar[c]-1))
         XM_FAIL("ColumnGroups disagrees with stimulus ranges");
   }
   for(c=0;c<nc;c++){
      int old=m->columns[c]; s=xm_stim_at(a,old);
      g[c]=groups?groups->ar[old]:0;
      if(g[c]>0){
         if(g[c]>a->nstim || sm[g[c]-1]<0 || s!=g[c]-1)
            XM_FAIL("ColumnGroups disagrees with stimulus ranges");
         g[c]=sm[g[c]-1]+1;
      }else if(!groups && s>=0) g[c]=sm[s]+1;
   }
   xm_setlist(x,"ColumnGroups",nc,g); xm_sync_core(x);
   for(i=0;i<a->nattr;i++) if(!strncasecmp(a->attr_name[i],"Glt",3) ||
       !strcasecmp(a->attr_name[i],"Nglt") || !strcasecmp(a->attr_name[i],"ColumnMasks")){
      xm_set(x,"XmatDroppedAttributes","GLT attributes and ColumnMasks removed by subsetting");break;
   }
   free(sm);free(g);if(groups)NI_delete_int_array(groups);return 1;
fail:
   free(sm);free(g);if(groups)NI_delete_int_array(groups);return 0;
#undef XM_FAIL
}

THD_xmat *THD_xmat_extract_run(const THD_xmat *a,const THD_xmat *u,
   int run,double tol,THD_xmat_run_map **mapping,char *err,int nerr)
{
   const THD_xmat *ref=u?u:a;
   THD_xmat *x=NULL; THD_xmat_run_map *m=NULL;
   NI_int_array *groups=NULL; int *sm=NULL,*g=NULL;
   int r,c,s,i,j,start,end,nr=0,nc=0; const char *v; char key[80],buf[128];
#define XM_FAIL(MSG) do{ xm_err(err,nerr,"%s",MSG); goto fail; }while(0)
   if(mapping) *mapping=NULL;
   if(err && nerr>0) *err='\0';
   if(!mapping || !xm_structure_valid(a) ||
      run<0 || run>=a->nrun || !isfinite(tol) || tol<0 || tol>=1)
      XM_FAIL("invalid run extraction arguments (zero tolerance must be in [0,1))");
   if(!THD_xmat_attr(a,"RunStart") || !THD_xmat_attr(a,"NRowFull") ||
      !THD_xmat_attr(a,"GoodList"))
      XM_FAIL("run extraction requires explicit RunStart, NRowFull and GoodList");
   v=THD_xmat_attr(a,"Nstim");
   if(v && atoi(v)!=a->nstim) XM_FAIL("Nstim disagrees with parsed stimulus metadata");
   if(a->nstim==0){
      v=THD_xmat_attr(a,"StimLabels");
      if(v && *v) XM_FAIL("incomplete stimulus metadata");
   }
   /* Refuse inconsistent BasisColumns rather than silently repairing a design. */
   for(s=0;s<a->nstim;s++){
      int b,t,n=0;
      snprintf(key,sizeof(key),"BasisColumns_%06d",s+1); v=THD_xmat_attr(a,key);
      if(v && (sscanf(v,"%d:%d%n",&b,&t,&n)!=2 || v[n] ||
               b!=a->stim[s].bot || t!=a->stim[s].top))
         XM_FAIL("BasisColumns disagrees with stimulus ranges");
   }
   for(i=0;i<a->nattr;i++){
      const char *k=a->attr_name[i],*p=strchr(k,'_');
      if(!strncasecmp(k,"Basis",5) && p && (atoi(p+1)<1 || atoi(p+1)>a->nstim))
         XM_FAIL("a Basis attribute refers to an unknown stimulus");
   }
   if(u){
      if(!xm_structure_valid(u) || u->nrow!=a->nfull || u->nfull!=a->nfull ||
         u->ncol!=a->ncol || u->nrun!=a->nrun || u->nstim!=a->nstim || u->tr!=a->tr)
         XM_FAIL("uncensored reference dimensions, stimuli or TR do not match");
      for(r=0;r<u->nrow;r++) if(u->good[r]!=r)
         XM_FAIL("uncensored reference must contain every full-timeline row");
      for(r=0;r<a->nrun;r++) if(a->runstart[r]!=u->runstart[r])
         XM_FAIL("uncensored reference RunStart does not match");
      for(s=0;s<a->nstim;s++)
         if(a->stim[s].bot!=u->stim[s].bot || a->stim[s].top!=u->stim[s].top ||
            strcmp(a->stim[s].label,u->stim[s].label) ||
            strcmp(a->stim[s].option?a->stim[s].option:"",u->stim[s].option?u->stim[s].option:""))
            XM_FAIL("uncensored reference stimulus metadata does not match");
      for(c=0;c<a->ncol;c++){
         if(strcmp(a->collab[c],u->collab[c])) XM_FAIL("uncensored reference ColumnLabels do not match");
         for(r=0;r<a->nrow;r++)
            if(a->mat[(size_t)c*a->nrow+r]!=u->mat[(size_t)c*u->nrow+a->good[r]])
               XM_FAIL("uncensored reference differs at a retained matrix value");
      }
   }
   start=a->runstart[run]; end=start+THD_xmat_run_length(a,run);
   m=calloc(1,sizeof(*m)); x=calloc(1,sizeof(*x));
   if(!m || !x) XM_FAIL("out of memory");
   m->run=run; m->start=start; m->length=end-start; m->source_ncol=a->ncol;
   m->rows=malloc(sizeof(int)*(size_t)a->nrow);
   m->columns=malloc(sizeof(int)*(size_t)a->ncol);
   m->inverse=malloc(sizeof(int)*(size_t)a->ncol);
   if(!m->rows || !m->columns || !m->inverse) XM_FAIL("out of memory");
   for(r=0;r<a->nrow;r++) if(a->good[r]>=start && a->good[r]<end) m->rows[nr++]=r;
   if(!nr) XM_FAIL("selected run has no retained rows");
   for(c=0;c<a->ncol;c++){
      double peak=0,local=0,kept=0; int owner=-1,mixed=0;
      s=xm_stim_at(a,c);
      for(r=0;r<ref->nrow;r++){
         double z=fabs(ref->mat[(size_t)c*ref->nrow+r]);
         if(!isfinite(z)) XM_FAIL("matrix contains a non-finite value");
         if(z>peak) peak=z;
      }
      for(r=0;r<ref->nrow;r++){
         double z=fabs(ref->mat[(size_t)c*ref->nrow+r]);
         if(z>tol*peak){
            int rr=THD_xmat_run_of(ref,ref->good[r]);
            if(owner<0) owner=rr; else if(owner!=rr) mixed=1;
            if(rr==run && z>local) local=z;
         }
      }
      for(r=0;r<nr;r++){
         double z=fabs(a->mat[(size_t)c*a->nrow+m->rows[r]]);
         if(z>kept) kept=z;
      }
      if(xm_im(a,s) && (owner<0 || mixed)){
         xm_err(err,nerr,"IM column '%s' has zero or cross-run support; run ownership is ambiguous",a->collab[c]);
         goto fail;
      }
      if(xm_im(a,s) && local>0 && kept<=tol*peak){
         xm_err(err,nerr,"IM column '%s' loses its support after censoring",a->collab[c]); goto fail;
      }
      m->inverse[c]=-1;
      if(local>0 && kept>tol*peak){ m->inverse[c]=nc; m->columns[nc++]=c; }
   }
   if(!nc) XM_FAIL("selected run has no active columns");
   m->nrow=nr; m->ncol=nc;
   if(!xm_subset(a,x,m,err,nerr)) goto fail;
   xm_setpath(x,"XmatSourceFile",a->fname);
   xm_setint(x,"XmatSourceRun",run+1); xm_setint(x,"XmatSourceStart",start);
   xm_setlist(x,"XmatSourceRows",nr,m->rows); xm_setlist(x,"XmatSourceColumns",nc,m->columns);
   xm_setpath(x,"XmatSupportFile",ref->fname);
   snprintf(buf,sizeof(buf),"%.17g",tol); xm_set(x,"XmatZeroTolerance",buf);
   v=THD_xmat_attr(a,"CommandLine"); if(v) xm_set(x,"XmatSourceCommandLine",v);
   xm_set(x,"CommandLine","THD_xmat_extract_run (see XmatSource* provenance)");
   for(i=0;i<a->nattr;i++) if(!strncasecmp(a->attr_name[i],"Glt",3) ||
       !strcasecmp(a->attr_name[i],"Nglt") || !strcasecmp(a->attr_name[i],"ColumnMasks")){
      xm_set(x,"XmatDroppedAttributes","GLT attributes and ColumnMasks are not projected by run extraction"); break;
   }
   free(sm); free(g); if(groups) NI_delete_int_array(groups);
   *mapping=m; return x;
fail:
   free(sm); free(g); if(groups) NI_delete_int_array(groups);
   THD_xmat_free(x); THD_xmat_run_map_free(m); return NULL;
#undef XM_FAIL
}

THD_xmat *THD_xmat_select_columns(const THD_xmat *a,const unsigned char *keep,
   int drop_glts,THD_xmat_run_map **mapping,char *err,int nerr)
{
   THD_xmat *x=NULL; THD_xmat_run_map *m=NULL; int c,r;
   if(mapping)*mapping=NULL;
   if(!mapping || !keep || !xm_structure_valid(a)){
      xm_err(err,nerr,"%s","invalid column selection arguments");return NULL;
   }
   if(!drop_glts) for(c=0;c<a->nattr;c++)
      if(!strncasecmp(a->attr_name[c],"Glt",3) || !strcasecmp(a->attr_name[c],"Nglt")){
         xm_err(err,nerr,"%s","column selection requires -drop_glts when GLT metadata is present");return NULL;
      }
   m=calloc(1,sizeof(*m));x=calloc(1,sizeof(*x));
   if(!m || !x)goto fail;
   m->run=-1;m->length=a->nfull;m->nrow=a->nrow;m->source_ncol=a->ncol;
   m->rows=malloc(sizeof(int)*a->nrow);m->columns=malloc(sizeof(int)*a->ncol);m->inverse=malloc(sizeof(int)*a->ncol);
   if(!m->rows || !m->columns || !m->inverse)goto fail;
   for(r=0;r<a->nrow;r++)m->rows[r]=r;
   for(c=0;c<a->ncol;c++){
      m->inverse[c]=-1;
      if(keep[c]){m->inverse[c]=m->ncol;m->columns[m->ncol++]=c;}
   }
   if(!m->ncol){xm_err(err,nerr,"%s","column selection is empty");goto fail;}
   if(!xm_subset(a,x,m,err,nerr))goto fail;
   xm_setpath(x,"XmatSourceFile",a->fname);
   xm_setlist(x,"XmatSourceRows",m->nrow,m->rows);xm_setlist(x,"XmatSourceColumns",m->ncol,m->columns);
   xm_set(x,"CommandLine","THD_xmat_select_columns (see XmatSource* provenance)");
   if(THD_xmat_attr(a,"CommandLine"))xm_set(x,"XmatSourceCommandLine",THD_xmat_attr(a,"CommandLine"));
   *mapping=m;return x;
fail:
   THD_xmat_free(x);THD_xmat_run_map_free(m);return NULL;
}

/* Unit-norm column SVD. Padding wide matrices avoids relying on a particular
   SVD backend's convention for m<n. Values are never changed in the caller. */
int THD_xmat_diagnostics(const THD_xmat *x,double tol,THD_xmat_diagnostic *d,
                         char *err,int nerr)
{
   double *a=NULL,*sv=NULL,hi=0,lo=HUGE_VAL;int m,n,r,c,k;
   if(!d || !xm_structure_valid(x) || !isfinite(tol) || tol<=0 || tol>=1){
      xm_err(err,nerr,"%s","invalid diagnostic arguments");return 0;
   }
   memset(d,0,sizeof(*d));n=x->ncol;m=MAX(x->nrow,n);
   a=calloc((size_t)m*n,sizeof(double));sv=calloc((size_t)n,sizeof(double));
   if(!a || !sv){free(a);free(sv);xm_err(err,nerr,"%s","out of memory");return 0;}
   for(c=0;c<n;c++){
      double norm=0;
      for(r=0;r<x->nrow;r++){
         double v=x->mat[(size_t)c*x->nrow+r];
         if(!isfinite(v)){free(a);free(sv);xm_err(err,nerr,"%s","matrix contains a non-finite value");return 0;}
         norm+=v*v;
      }
      norm=sqrt(norm);
      if(norm==0)d->zero_columns++;
      else for(r=0;r<x->nrow;r++)a[(size_t)c*m+r]=x->mat[(size_t)c*x->nrow+r]/norm;
   }
   for(c=0;c<n;c++)for(k=0;k<c;k++){
      double dot=0;for(r=0;r<x->nrow;r++)dot+=a[(size_t)c*m+r]*a[(size_t)k*m+r];
      if(fabs(dot)>=1-tol)d->duplicate_pairs++;
      if(fabs(dot)>d->max_abs_cosine)d->max_abs_cosine=fabs(dot);
   }
   svd_double(m,n,a,sv,NULL,NULL);
   for(c=0;c<n;c++)if(sv[c]>hi)hi=sv[c];
   for(c=0;c<n;c++)if(sv[c]>tol*hi){d->rank++;if(sv[c]<lo)lo=sv[c];}
   d->residual_df=x->nrow-d->rank;
   d->condition=(d->rank<n || hi==0)?HUGE_VAL:hi/lo;
   free(a);free(sv);return 1;
}

/* Publish a complete temporary file without replacing an existing path. */
static int xm_publish(const char *tmp,const char *fname,char *err,int nerr)
{
   int ok=link(tmp,fname)==0;
   if(!ok) xm_err(err,nerr,"cannot create output (exists or not writable): %s",fname);
   unlink(tmp); return ok;
}
int THD_xmat_write(const THD_xmat *x,const char *fname,char *err,int nerr)
{
   NI_element *nel; THD_xmat core={0}; double *col; char *tmp;
   int c,r,i,fd,ok; int64_t nw;
   if(!fname || !*fname || !xm_structure_valid(x)){
      xm_err(err,nerr,"%s","invalid matrix or output path"); return 0;
   }
   for(c=0;c<x->ncol;c++) for(r=0;r<x->nrow;r++)
      if(!isfinite(x->mat[(size_t)c*x->nrow+r])){
         xm_err(err,nerr,"%s","matrix contains a non-finite value");return 0;
      }
   col=malloc(sizeof(double)*(size_t)x->nrow); tmp=malloc(strlen(fname)+16);
   if(!col || !tmp){free(col);free(tmp);xm_err(err,nerr,"%s","out of memory");return 0;}
   nel=NI_new_data_element("matrix",x->nrow);
   for(c=0;c<x->ncol;c++){
      for(r=0;r<x->nrow;r++) col[r]=x->mat[(size_t)c*x->nrow+r];
      NI_add_column(nel,NI_DOUBLE,col);
   }
   free(col);
   for(i=0;i<x->nattr;i++) if(!xm_core_attr(x->attr_name[i]))
      NI_set_attribute(nel,x->attr_name[i],x->attr_value[i]);
   core=*x; core.nattr=0; core.attr_name=NULL; core.attr_value=NULL;
   xm_sync_core(&core);
   for(i=0;i<core.nattr;i++){
      NI_set_attribute(nel,core.attr_name[i],core.attr_value[i]);
      free(core.attr_name[i]); free(core.attr_value[i]);
   }
   free(core.attr_name); free(core.attr_value);
   sprintf(tmp,"%s.tmp.XXXXXX",fname); fd=mkstemp(tmp);
   if(fd<0){xm_err(err,nerr,"cannot create temporary output beside %s",fname);free(tmp);NI_free_element(nel);return 0;}
   close(fd);
   nw=NI_write_element_tofile(tmp,nel,NI_HEADERSHARP_FLAG|NI_TEXT_MODE);
   NI_free_element(nel);
   if(nw<=0){unlink(tmp);xm_err(err,nerr,"failed writing %s",fname);ok=0;}
   else ok=xm_publish(tmp,fname,err,nerr);
   free(tmp); return ok;
}
static void xm_tsv(FILE *fp,const char *s)
{
   for(;*s;s++) switch(*s){
      case '\\': fputs("\\\\",fp); break; case '\t': fputs("\\t",fp); break;
      case '\r': fputs("\\r",fp); break; case '\n': fputs("\\n",fp); break;
      default: fputc(*s,fp);
   }
}
int THD_xmat_write_run_map(const THD_xmat *a,const THD_xmat *x,
   const THD_xmat_run_map *m,const char *rows,const char *cols,char *err,int nerr)
{
   FILE *fp; int fd,c,r,s,t,k,bad;
   if(!xm_structure_valid(a) || !xm_structure_valid(x) || !m || !rows || !cols ||
      !m->rows || !m->columns || !m->inverse || m->nrow!=x->nrow ||
      m->ncol!=x->ncol || m->source_ncol!=a->ncol || m->run<-1 || m->run>=a->nrun ||
      (m->run>=0 && (m->start!=a->runstart[m->run] || m->length!=THD_xmat_run_length(a,m->run) || x->nrun!=1)) ||
      (m->run<0 && (m->start!=0 || m->length!=a->nfull || x->nrun!=a->nrun)) || x->nfull!=m->length){
      xm_err(err,nerr,"%s","invalid provenance mapping");return 0;
   }
   for(r=0;r<m->nrow;r++)
      if(m->rows[r]<0 || m->rows[r]>=a->nrow || (r && m->rows[r]<=m->rows[r-1]) ||
         a->good[m->rows[r]]!=x->good[r]+m->start){
         xm_err(err,nerr,"%s","invalid provenance row mapping");return 0;
      }
   for(c=0;c<m->ncol;c++)
      if(m->columns[c]<0 || m->columns[c]>=a->ncol ||
         (c && m->columns[c]<=m->columns[c-1]) || m->inverse[m->columns[c]]!=c){
         xm_err(err,nerr,"%s","invalid provenance column mapping");return 0;
      }
   for(c=0;c<a->ncol;c++)
      if(m->inverse[c]<-1 || m->inverse[c]>=x->ncol ||
         (m->inverse[c]>=0 && m->columns[m->inverse[c]]!=c)){
         xm_err(err,nerr,"%s","invalid inverse column mapping");return 0;
      }
   fd=open(rows,O_WRONLY|O_CREAT|O_EXCL,0666);
   if(fd<0){xm_err(err,nerr,"cannot create %s",rows);return 0;}
   fp=fdopen(fd,"w");
   if(!fp){close(fd);unlink(rows);xm_err(err,nerr,"cannot open %s",rows);return 0;}
   fputs("# Columns: Row SourceRow RunTR SourceTR\n",fp);
   for(r=0;r<m->nrow;r++) fprintf(fp,"%d %d %d %d\n",r,m->rows[r],x->good[r],a->good[m->rows[r]]);
   bad=ferror(fp); if(fclose(fp)) bad=1;
   if(bad){unlink(rows);xm_err(err,nerr,"failed writing %s",rows);return 0;}
   fd=open(cols,O_WRONLY|O_CREAT|O_EXCL,0666);
   if(fd<0){unlink(rows);xm_err(err,nerr,"cannot create %s",cols);return 0;}
   fp=fdopen(fd,"w");
   if(!fp){close(fd);unlink(rows);unlink(cols);xm_err(err,nerr,"cannot open %s",cols);return 0;}
   fputs("# Columns: SourceColumn Column SourceStim Stim SourceIMVolume IMVolume Kept\n",fp);
   for(c=0;c<a->ncol;c++){
      fprintf(fp,"# ColumnLabel %d ",c); xm_tsv(fp,a->collab[c]); fputc('\n',fp);
   }
   for(c=0;c<a->ncol;c++){
      k=m->inverse[c]; s=xm_stim_at(a,c); t=k<0?-1:xm_stim_at(x,k);
      fprintf(fp,"%d %d %d %d %d %d %d\n",c,k,s,t,xm_im(a,s)?c-a->stim[s].bot:-1,
              xm_im(x,t)?k-x->stim[t].bot:-1,k>=0);
   }
   bad=ferror(fp); if(fclose(fp)) bad=1;
   if(bad){unlink(rows);unlink(cols);xm_err(err,nerr,"failed writing %s",cols);return 0;}
   return 1;
}


/*============================================================================*/
/*  Tables with an Xmat column                                                */
/*============================================================================*/

typedef struct { int ntok , ncap ; char **tok ; } xm_tokens ;

static void xm_add( xm_tokens *t , const char *s )
{
   if( t->ntok >= t->ncap ){ t->ncap = 2*t->ncap + 64 ; t->tok = (char **)realloc(t->tok,sizeof(char *)*(size_t)t->ncap) ; }
   t->tok[t->ntok++] = strdup(s) ;
}

static THD_datatable * xm_table( xm_tokens *t , char *source )
{
   THD_datatable *out = THD_parse_datatable(t->tok,t->ntok,source) ; int ii ;
   for( ii=0 ; ii < t->ntok ; ii++ ) free(t->tok[ii]) ;
   free(t->tok) ;
   return out ;
}

static THD_xmat * xm_row_xmat( THD_datatable *dt , int row , int icol )
{
   const char *fn = DT_CELL(dt,row,icol) ; char err[512] ; THD_xmat *xm ;
   if( fn[0] == '\0' || strcmp(fn,"-") == 0 || strcasecmp(fn,"NA") == 0 )
     ERROR_exit("Subj %s: no Xmat on row %d of the table",dt->subj[row],row+1) ;
   xm = THD_xmat_read(fn,err,sizeof(err)) ;
   if( xm == NULL ) ERROR_exit("Subj %s: %s",dt->subj[row],err) ;
   return xm ;
}

static int xm_manifest_index( const char *s , const char *name , int row , int limit )
{
   char *end ; long v ;
   errno = 0 ; v = strtol(s,&end,10) ;
   if( errno || end == s || *end || v < 0 || v >= limit )
     ERROR_exit("trial manifest row %d: %s '%s' must be a zero-based integer in 0..%d",
                row+1,name,s,limit-1) ;
   return (int)v ;
}

THD_datatable * THD_datatable_trial_manifest( THD_datatable *dt )
{
   xm_tokens t = {0,0,NULL} ; int iv,ix,ic,r,c ;
   THD_3dim_dataset *ds=NULL ; THD_xmat *xm=NULL ;
   char *last_input=NULL,*last_xmat=NULL ;
   if( dt == NULL ) return NULL ;
   iv = THD_datatable_column(dt,"Volume") ;
   ix = THD_datatable_column(dt,"Xmat") ;
   ic = THD_datatable_column(dt,"XColumn") ;
   if( dt->icol_input < 0 || iv < 0 )
     ERROR_exit("trial manifest requires InputFile and Volume columns") ;
   if( (ix < 0) != (ic < 0) )
     ERROR_exit("trial manifest: optional Xmat and XColumn columns must occur together") ;
   /* The canonical parser requires InputFile last. Keep provenance as metadata. */
   for( c=0 ; c < dt->ncol ; c++ ) if( c != dt->icol_input ) xm_add(&t,dt->cname[c]) ;
   xm_add(&t,"InputFile") ;
   for( r=0 ; r < dt->nrow ; r++ ){
     const char *fn = DT_CELL(dt,r,dt->icol_input) ; int b ; char *sel ;
     if( !*fn || strchr(fn,'[') || !strcmp(fn,"-") || !strcasecmp(fn,"NA") )
       ERROR_exit("trial manifest row %d: InputFile must name a whole beta bucket",r+1) ;
     if( last_input == NULL || strcmp(last_input,fn) ){
       if( ds != NULL ) DSET_delete(ds) ;
       ds = THD_open_dataset((char *)fn) ; CHECK_OPEN_ERROR(ds,fn) ;
       last_input = (char *)fn ;
     }
     b = xm_manifest_index(DT_CELL(dt,r,iv),"Volume",r,DSET_NVALS(ds)) ;
     if( ix >= 0 ){
       const char *xf = DT_CELL(dt,r,ix) ; char err[512] ;
       if( last_xmat == NULL || strcmp(last_xmat,xf) ){
         THD_xmat_free(xm) ; xm = THD_xmat_read(xf,err,sizeof(err)) ;
         if( xm == NULL ) ERROR_exit("trial manifest row %d: %s",r+1,err) ;
         last_xmat = (char *)xf ;
       }
       (void)xm_manifest_index(DT_CELL(dt,r,ic),"XColumn",r,xm->ncol) ;
     }
     for( c=0 ; c < dt->ncol ; c++ ) if( c != dt->icol_input ) xm_add(&t,DT_CELL(dt,r,c)) ;
     sel = (char *)malloc(strlen(fn)+32) ; sprintf(sel,"%s[%d]",fn,b) ;
     xm_add(&t,sel) ; free(sel) ;
   }
   if( ds != NULL ) DSET_delete(ds) ;
   THD_xmat_free(xm) ;
   return xm_table(&t,"trial manifest") ;
}

/* LSS volumes follow the single IM BasisColumns range, not the stats-bucket
   _Coef labels. Event identities must be supplied explicitly by the caller. */
THD_datatable * THD_datatable_expand_lss_items( THD_datatable *dt ,
                                               const char *item_col , const char *run_col )
{
   int ix,ie,r,c,j,ne=0,add_run=1 ;
   THD_datatable **events ; int **runs ; char **enames=NULL ;
   xm_tokens t = {0,0,NULL} ;
   ix=THD_datatable_column(dt,"Xmat") ; ie=THD_datatable_column(dt,"Events") ;
   if( ix<0 || ie<0 || dt->icol_subj<0 || dt->icol_input<0 )
     ERROR_exit("-lss requires Subj, Xmat, Events, and InputFile columns") ;
   if( THD_datatable_column(dt,(char *)item_col)>=0 || THD_datatable_column(dt,"Trial")>=0 ||
       THD_datatable_column(dt,"Censor")>=0 )
     ERROR_exit("-lss: put trial items in Events files; do not supply Item, Trial, or Censor in the subject table") ;
   if( THD_datatable_column(dt,(char *)run_col)>=0 ) add_run=0 ;
   events=(THD_datatable **)calloc(dt->nrow,sizeof(*events)) ;
   runs=(int **)calloc(dt->nrow,sizeof(*runs)) ;
   for( r=0 ; r<dt->nrow ; r++ ){
     THD_xmat *xm=xm_row_xmat(dt,r,ix) ; THD_datatable *ev ; THD_3dim_dataset *ds ;
     const char *fn=DT_CELL(dt,r,dt->icol_input), *a, *label="LSS" ;
     int nb,im=0,bot=-1,top=-1,it,ni,*seen ; char an[64],tail ;
     a=THD_xmat_attr(xm,"BasisNstim") ;
     if( a==NULL || sscanf(a,"%d %c",&nb,&tail)!=1 || nb<1 || nb>xm->ncol )
       ERROR_exit("Subj %s: -lss needs a valid BasisNstim in '%s'",dt->subj[r],xm->fname) ;
     for( j=1 ; j<=nb ; j++ ){
       snprintf(an,sizeof(an),"BasisOption_%06d",j) ; a=THD_xmat_attr(xm,an) ;
       if( a==NULL || strcmp(a,"-stim_times_IM") ) continue ;
       if( im ) ERROR_exit("Subj %s: -lss requires exactly one -stim_times_IM in the Xmat",dt->subj[r]) ;
       im=j ; snprintf(an,sizeof(an),"BasisColumns_%06d",j) ; a=THD_xmat_attr(xm,an) ;
       if( a==NULL || sscanf(a,"%d:%d %c",&bot,&top,&tail)!=2 || bot<0 || top<=bot || top>=xm->ncol )
         ERROR_exit("Subj %s: -lss needs a valid multi-trial %s range",dt->subj[r],an) ;
       snprintf(an,sizeof(an),"BasisName_%06d",j) ; a=THD_xmat_attr(xm,an) ;
       if( a!=NULL ) label=a ;
     }
     if( !im ) ERROR_exit("Subj %s: -lss requires exactly one -stim_times_IM in the Xmat",dt->subj[r]) ;
     ni=top-bot+1 ;
     if( strchr(fn,'[') ) ERROR_exit("Subj %s: -lss InputFile must be the whole LSS dataset, without a selector",dt->subj[r]) ;
     ds=THD_open_dataset((char *)fn) ; CHECK_OPEN_ERROR(ds,fn) ;
     if( DSET_NVALS(ds)!=ni )
       ERROR_exit("Subj %s: LSS dataset has %d volumes but Xmat describes %d IM trials",dt->subj[r],DSET_NVALS(ds),ni) ;
     for( j=0 ; j<ni ; j++ ){
       char *want=(char *)malloc(strlen(label)+32) ; const char *got=DSET_BRICK_LABEL(ds,j) ;
       sprintf(want,"%s#%03d",label,j) ;
       if( got==NULL || strcmp(got,want) )
         ERROR_exit("Subj %s: LSS volume %d must be labelled '%s'; for renamed datasets use an explicit item table",dt->subj[r],j,want) ;
       free(want) ;
     }
     DSET_delete(ds) ;
     ev=events[r]=THD_read_datatable_file(DT_CELL(dt,r,ie)) ;
     it=THD_datatable_column(ev,"Trial") ;
     if( it<0 || THD_datatable_column(ev,(char *)item_col)<0 )
       ERROR_exit("Subj %s: Events needs Trial and %s columns",dt->subj[r],item_col) ;
     if( ev->nrow!=ni ) ERROR_exit("Subj %s: Events must map all %d LSS trials exactly once (has %d rows)",dt->subj[r],ni,ev->nrow) ;
     for( c=0 ; c<ev->ncol ; c++ ){
       if( THD_datatable_column(dt,ev->cname[c])>=0 || !strcasecmp(ev->cname[c],"Censor") )
         ERROR_exit("Subj %s: Events column '%s' conflicts with the subject table or is reserved",dt->subj[r],ev->cname[c]) ;
     }
     if( r==0 ){
       ne=ev->ncol ; enames=(char **)calloc(ne,sizeof(char *)) ;
       for( c=0 ; c<ne ; c++ ) enames[c]=strdup(ev->cname[c]) ;
     } else {
       if( ev->ncol!=ne ) ERROR_exit("-lss: all Events files must have the same columns") ;
       for( c=0 ; c<ne ; c++ ) if( THD_datatable_column(ev,enames[c])<0 )
         ERROR_exit("Subj %s: Events is missing column '%s'",dt->subj[r],enames[c]) ;
     }
     if( THD_datatable_column(ev,(char *)run_col)>=0 ) add_run=0 ;
     seen=(int *)calloc(ni,sizeof(int)) ; runs[r]=(int *)malloc(ni*sizeof(int)) ;
     for( j=0 ; j<ni ; j++ ){
       int b=xm_manifest_index(DT_CELL(ev,j,it),"Trial",j,ni), q,run=-1 ;
       if( seen[b]++ ) ERROR_exit("Subj %s: duplicate Trial %d in Events",dt->subj[r],b) ;
       /* GoodList maps the censored design rows back to the full timeline. */
       for( q=0 ; q<xm->nrow ; q++ ) if( xm->mat[(size_t)(bot+b)*xm->nrow+q]!=0.0f ){
         int rr=THD_xmat_run_of(xm,xm->good[q]) ;
         if( run==-1 ) run=rr ; else if( run!=rr ){ run=-2 ; break ; }
       }
       runs[r][j]=run ; if( run<0 ) add_run=0 ;
     }
     free(seen) ; THD_xmat_free(xm) ;
   }
   for( c=0 ; c<dt->ncol ; c++ ) if( c!=ix && c!=ie && c!=dt->icol_input ) xm_add(&t,dt->cname[c]) ;
   for( c=0 ; c<ne ; c++ ) xm_add(&t,enames[c]) ;
   if( add_run ) xm_add(&t,run_col) ;
   xm_add(&t,"InputFile") ;
   for( r=0 ; r<dt->nrow ; r++ ){
     THD_datatable *ev=events[r] ; int it=THD_datatable_column(ev,"Trial") ;
     for( j=0 ; j<ev->nrow ; j++ ){
       const char *fn=DT_CELL(dt,r,dt->icol_input) ; char *sel=(char *)malloc(strlen(fn)+32), rn[32] ;
       for( c=0 ; c<dt->ncol ; c++ ) if( c!=ix && c!=ie && c!=dt->icol_input ) xm_add(&t,DT_CELL(dt,r,c)) ;
       for( c=0 ; c<ne ; c++ ) xm_add(&t,DT_CELL(ev,j,THD_datatable_column(ev,enames[c]))) ;
       if( add_run ){ snprintf(rn,sizeof(rn),"%d",runs[r][j]+1) ; xm_add(&t,rn) ; }
       sprintf(sel,"%s[%d]",fn,atoi(DT_CELL(ev,j,it))) ; xm_add(&t,sel) ; free(sel) ;
     }
     THD_free_datatable(ev) ; free(runs[r]) ;
   }
   for( c=0 ; c<ne ; c++ ) free(enames[c]) ;
   free(enames) ; free(events) ; free(runs) ;
   return xm_table(&t,"LSS event-to-item tables") ;
}

THD_datatable * THD_datatable_expand_xmat_items( THD_datatable *dt , const char *xmat_col ,
                                                 const char *item_col , const char *run_col ,
                                                 const char *stims , int *have_run_out )
{
   int icol , r , c , j , ncarry=0 , *carry , have_run=1 , *nit ; THD_xmat_item **it ;
   xm_tokens t = {0,0,NULL} ; THD_datatable *out ;
   if( dt == NULL || xmat_col == NULL ) return NULL ;
   icol = THD_datatable_column(dt,(char *)xmat_col) ;
   if( icol < 0 ) ERROR_exit("the table has no '%s' column",xmat_col) ;
   if( dt->icol_input < 0 ) ERROR_exit("the data table needs an InputFile column") ;
   if( dt->icol_subj < 0 ) ERROR_exit("a table with an %s column needs a Subj column",xmat_col) ;
   for( c=0 ; c < dt->ncol ; c++ )
     if( !strcasecmp(dt->cname[c],item_col) || !strcasecmp(dt->cname[c],"Condition") || !strcasecmp(dt->cname[c],run_col) )
       ERROR_exit("with an %s column the items, their conditions, and runs come from the design matrix: remove the '%s' column",
                  xmat_col,dt->cname[c]) ;
   carry = (int *)malloc(sizeof(int)*(size_t)dt->ncol) ;
   for( c=0 ; c < dt->ncol ; c++ ) if( c != icol && c != dt->icol_input ) carry[ncarry++] = c ;
   nit = (int *)calloc((size_t)dt->nrow,sizeof(int)) ;
   it  = (THD_xmat_item **)calloc((size_t)dt->nrow,sizeof(THD_xmat_item *)) ;

   /* each row: its items, and the bucket volume of each (found by label) */
   for( r=0 ; r < dt->nrow ; r++ ){
     const char *bn = DT_CELL(dt,r,dt->icol_input) ; THD_xmat *xm ; THD_3dim_dataset *ds ;
     unsigned char *want ; int nf ;
     if( strchr(bn,'[') != NULL )
       ERROR_exit("Subj %s: with an %s column InputFile is the whole stats bucket, without a selector ('%s')",dt->subj[r],xmat_col,bn) ;
     xm = xm_row_xmat(dt,r,icol) ;
     if( xm->nstim < 1 ) ERROR_exit("Subj %s: '%s' has no stimulus columns (StimLabels, StimBots, StimTops)",dt->subj[r],xm->fname) ;
     want = THD_xmat_stims_wanted(xm,stims,&nf) ;
     if( nf < 1 ){
       if( stims != NULL ) ERROR_exit("Subj %s: no stimulus of '%s' is in the list '%s'",dt->subj[r],xm->fname,stims) ;
       ERROR_exit("Subj %s: '%s' has no stimulus",dt->subj[r],xm->fname) ;
     }
     it[r] = THD_xmat_items(xm,want,&nit[r]) ;
     if( it[r] == NULL )
       ERROR_exit("Subj %s: cannot expand valid stimulus items from '%s'",dt->subj[r],xm->fname) ;
     ds = THD_open_dataset((char *)bn) ; CHECK_OPEN_ERROR(ds,bn) ;
     for( j=0 ; j < nit[r] ; j++ ){
       int b ;
       for( b=0 ; b < DSET_NVALS(ds) ; b++ )
         if( DSET_BRICK_LABEL(ds,b) != NULL && strcmp(DSET_BRICK_LABEL(ds,b),it[r][j].coef_label) == 0 ) break ;
       if( b == DSET_NVALS(ds) )
         ERROR_exit("Subj %s: '%s' has no volume labelled %s; is it the stats bucket of the regression in '%s'?",
                    dt->subj[r],bn,it[r][j].coef_label,xm->fname) ;
       it[r][j].column = b ;                          /* from here: the bucket volume */
       if( it[r][j].run < 0 ) have_run = 0 ;
     }
     DSET_delete(ds) ; free(want) ; THD_xmat_free(xm) ;
   }

   /* the item table */
   for( j=0 ; j < ncarry ; j++ ) xm_add(&t,dt->cname[carry[j]]) ;
   xm_add(&t,item_col) ; xm_add(&t,"Condition") ;
   if( have_run ) xm_add(&t,run_col) ;
   xm_add(&t,"InputFile") ;
   for( r=0 ; r < dt->nrow ; r++ ) for( j=0 ; j < nit[r] ; j++ ){
     char sel[THD_MAX_NAME+32] , rn[24] ; int q ;
     for( q=0 ; q < ncarry ; q++ ) xm_add(&t,DT_CELL(dt,r,carry[q])) ;
     xm_add(&t,it[r][j].key) ; xm_add(&t,it[r][j].condition) ;
     if( have_run ){ snprintf(rn,sizeof(rn),"%d",it[r][j].run+1) ; xm_add(&t,rn) ; }
     snprintf(sel,sizeof(sel),"%s[%d]",DT_CELL(dt,r,dt->icol_input),it[r][j].column) ; xm_add(&t,sel) ;
   }
   for( r=0 ; r < dt->nrow ; r++ ) THD_xmat_items_free(it[r],nit[r]) ;
   free(it) ; free(nit) ; free(carry) ;
   out = xm_table(&t,"the items of the Xmat column") ;
   if( out == NULL ) ERROR_exit("could not build the item table from the %s column",xmat_col) ;
   if( have_run_out != NULL ) *have_run_out = have_run ;
   return out ;
}

THD_datatable * THD_datatable_add_xmat_columns( THD_datatable *dt , const char *xmat_col )
{
   static const char *nm[6] = { "Xmat.TR" , "Xmat.nrun" , "Xmat.ntime" , "Xmat.ncensored" , "Xmat.frac_censored" , "Xmat.nstim" } ;
   int icol , r , c , q ; xm_tokens t = {0,0,NULL} ; THD_datatable *out ;
   char **lastf=NULL ; double (*val)[6] ;
   if( dt == NULL || xmat_col == NULL ) return NULL ;
   icol = THD_datatable_column(dt,(char *)xmat_col) ;
   if( icol < 0 ) ERROR_exit("the table has no '%s' column",xmat_col) ;
   if( dt->icol_input < 0 ) ERROR_exit("the data table needs an InputFile column") ;
   for( q=0 ; q < 6 ; q++ ) if( THD_datatable_column(dt,(char *)nm[q]) >= 0 ) return dt ;   /* already there */
   val = (double (*)[6])malloc(sizeof(double)*6*(size_t)dt->nrow) ;
   lastf = (char **)calloc((size_t)dt->nrow,sizeof(char *)) ;
   for( r=0 ; r < dt->nrow ; r++ ){
     int prev=-1 ;
     for( q=0 ; q < r && prev < 0 ; q++ ) if( strcmp(DT_CELL(dt,q,icol),DT_CELL(dt,r,icol)) == 0 ) prev = q ;
     if( prev >= 0 ){ memcpy(val[r],val[prev],sizeof(double)*6) ; continue ; }  /* rows sharing a file */
     { THD_xmat *xm = xm_row_xmat(dt,r,icol) ;
       val[r][0] = xm->tr ; val[r][1] = xm->nrun ; val[r][2] = xm->nfull ; val[r][3] = THD_xmat_ncensored(xm) ;
       val[r][4] = (xm->nfull > 0) ? (double)THD_xmat_ncensored(xm)/xm->nfull : 0.0 ; val[r][5] = xm->nstim ;
       THD_xmat_free(xm) ; }
   }
   for( c=0 ; c < dt->ncol ; c++ ) if( c != dt->icol_input ) xm_add(&t,dt->cname[c]) ;
   for( q=0 ; q < 6 ; q++ ) xm_add(&t,nm[q]) ;
   xm_add(&t,"InputFile") ;
   for( r=0 ; r < dt->nrow ; r++ ){
     char b[64] ;
     for( c=0 ; c < dt->ncol ; c++ ) if( c != dt->icol_input ) xm_add(&t,DT_CELL(dt,r,c)) ;
     for( q=0 ; q < 6 ; q++ ){ snprintf(b,sizeof(b),"%.6g",val[r][q]) ; xm_add(&t,b) ; }
     xm_add(&t,DT_CELL(dt,r,dt->icol_input)) ;
   }
   free(val) ; free(lastf) ;
   out = xm_table(&t,dt->source != NULL ? dt->source : "the table") ;
   if( out == NULL ) ERROR_exit("could not add the %s summary columns",xmat_col) ;
   return out ;
}
