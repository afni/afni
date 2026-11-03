#include "thd_mvpa.h"

#include <float.h>
#include <errno.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/stat.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <strings.h>

static int pp_error( char *err, int nerr, const char *fmt, ... )
{
   va_list ap ;
   if( err!=NULL && nerr>0 ){
     va_start(ap,fmt) ; vsnprintf(err,(size_t)nerr,fmt,ap) ; va_end(ap) ;
   }
   return 0 ;
}

void THD_centroid_free( THD_centroid_model *m )
{
   if( m==NULL ) return ;
   free(m->class_count) ; free(m->centroid) ;
   free(m->centroid_mean) ; free(m->centroid_norm) ; free(m) ;
}

static THD_centroid_model * centroid_alloc( int nclass, int max_feature,
                                            char *err, int nerr )
{
   THD_centroid_model *m ;
   if( nclass<2 || max_feature<1 ||
       (size_t)nclass > SIZE_MAX/sizeof(double)/(size_t)max_feature ){
     pp_error(err,nerr,"invalid or excessive centroid dimensions") ; return NULL ;
   }
   m=(THD_centroid_model *)calloc(1,sizeof(*m)) ;
   if( m==NULL ){ pp_error(err,nerr,"out of memory allocating centroid model") ; return NULL ; }
   m->nclass=nclass ; m->nfeature_alloc=max_feature ;
   m->class_count=(int *)calloc((size_t)nclass,sizeof(int)) ;
   m->centroid=(double *)calloc((size_t)nclass*max_feature,sizeof(double)) ;
   m->centroid_mean=(double *)calloc((size_t)nclass,sizeof(double)) ;
   m->centroid_norm=(double *)calloc((size_t)nclass,sizeof(double)) ;
   if( m->class_count==NULL || m->centroid==NULL ||
       m->centroid_mean==NULL || m->centroid_norm==NULL ){
     THD_centroid_free(m) ;
     pp_error(err,nerr,"out of memory allocating centroid model") ; return NULL ;
   }
   return m ;
}

const char * THD_centroid_distance_name( int distance )
{
   switch( distance ){
     case THD_CENTROID_EUCLIDEAN:   return "euclidean" ;
     case THD_CENTROID_CORRELATION: return "correlation" ;
     case THD_CENTROID_COSINE:      return "cosine" ;
   }
   return NULL ;
}

int THD_centroid_distance_parse( const char *name, int *distance,
                                 char *err, int nerr )
{
   int dd ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( name==NULL || distance==NULL )
     return pp_error(err,nerr,"centroid distance parsing needs a name and output") ;
   for( dd=THD_CENTROID_EUCLIDEAN ; dd<=THD_CENTROID_COSINE ; dd++ )
     if( strcasecmp(name,THD_centroid_distance_name(dd))==0 ){
       *distance=dd ; return 1 ;
     }
   return pp_error(err,nerr,"unknown centroid distance '%s'",name) ;
}

static int centroid_refit( THD_centroid_model *m, int distance,
                           const float *x, const int *class_id,
                           int nsample, int nfeature, char *err, int nerr )
{
   int ii,jj,cc ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( m==NULL || m->class_count==NULL || m->centroid==NULL ||
       m->centroid_mean==NULL || m->centroid_norm==NULL ||
       THD_centroid_distance_name(distance)==NULL )
     return pp_error(err,nerr,"invalid centroid model or distance") ;
   if( x==NULL || class_id==NULL || nsample<1 || nfeature<1 ||
       nfeature>m->nfeature_alloc )
     return pp_error(err,nerr,"centroid fitting matrix exceeds the model workspace") ;
   if( m->nclass>nsample )
     return pp_error(err,nerr,"centroid fitting needs at least one sample in every class") ;
   if( distance==THD_CENTROID_CORRELATION && nfeature<2 )
     return pp_error(err,nerr,"correlation distance needs at least two features") ;

   m->distance=distance ; m->nfeature=nfeature ; m->nfit=nsample ;
   memset(m->class_count,0,sizeof(int)*(size_t)m->nclass) ;
   memset(m->centroid,0,sizeof(double)*(size_t)m->nclass*nfeature) ;
   memset(m->centroid_mean,0,sizeof(double)*(size_t)m->nclass) ;
   memset(m->centroid_norm,0,sizeof(double)*(size_t)m->nclass) ;
   for( ii=0 ; ii<nsample ; ii++ ){
     cc=class_id[ii] ;
     if( cc<0 || cc>=m->nclass )
       return pp_error(err,nerr,"training class %d at sample %d is outside [0,%d]",cc,ii+1,m->nclass-1) ;
     m->class_count[cc]++ ;
     for( jj=0 ; jj<nfeature ; jj++ ){
       float vv=x[(size_t)ii*nfeature+jj] ;
       if( !isfinite(vv) )
         return pp_error(err,nerr,"training sample %d, feature %d is not finite",ii+1,jj+1) ;
       m->centroid[(size_t)cc*nfeature+jj]+=(double)vv ;
     }
   }
   for( cc=0 ; cc<m->nclass ; cc++ ){
     if( m->class_count[cc]==0 )
       return pp_error(err,nerr,"training data have no samples for class %d",cc) ;
     for( jj=0 ; jj<nfeature ; jj++ )
       m->centroid[(size_t)cc*nfeature+jj]/=(double)m->class_count[cc] ;
   }
   if( distance==THD_CENTROID_EUCLIDEAN ) return 1 ;
   for( cc=0 ; cc<m->nclass ; cc++ ){
     const double *cent=m->centroid+(size_t)cc*nfeature ;
     double mean=0.0,ss=0.0,rawss=0.0 ;
     if( distance==THD_CENTROID_CORRELATION ){
       for( jj=0 ; jj<nfeature ; jj++ ) mean+=cent[jj] ;
       mean/=(double)nfeature ; m->centroid_mean[cc]=mean ;
     }
     for( jj=0 ; jj<nfeature ; jj++ ){
       double vv=cent[jj],dd=vv-mean ; ss+=dd*dd ; rawss+=vv*vv ;
     }
     if( ss<=DBL_EPSILON*rawss ){
       if( distance==THD_CENTROID_CORRELATION )
         return pp_error(err,nerr,"class %d centroid is constant for correlation distance",cc) ;
       return pp_error(err,nerr,"class %d centroid has zero norm for cosine distance",cc) ;
     }
     m->centroid_norm[cc]=sqrt(ss) ;
   }
   return 1 ;
}

THD_centroid_model * THD_centroid_fit( int distance, const float *x,
                                        const int *class_id,
                                        int nsample, int nfeature, int nclass,
                                        char *err, int nerr )
{
   THD_centroid_model *m=centroid_alloc(nclass,nfeature,err,nerr) ;
   if( m==NULL ) return NULL ;
   if( !centroid_refit(m,distance,x,class_id,nsample,nfeature,err,nerr) ){
     THD_centroid_free(m) ; return NULL ;
   }
   return m ;
}

int THD_centroid_predict( const THD_centroid_model *m,
                          const float *x, int nsample, int nfeature,
                          int *prediction, double *decision,
                          char *err, int nerr )
{
   int ii,jj,cc ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( m==NULL || m->centroid==NULL || m->class_count==NULL || x==NULL ||
       prediction==NULL || nsample<1 || nfeature!=m->nfeature || m->nclass<2 )
     return pp_error(err,nerr,"invalid centroid model, prediction matrix, or output shape") ;
   if( m->distance!=THD_CENTROID_EUCLIDEAN &&
       (m->centroid_mean==NULL || m->centroid_norm==NULL) )
     return pp_error(err,nerr,"centroid model is missing normalization statistics") ;
   for( ii=0 ; ii<nsample ; ii++ ){
     const float *row=x+(size_t)ii*nfeature ;
     double mean=0.0,norm=0.0,best=0.0 ; int best_class=0 ;
     for( jj=0 ; jj<nfeature ; jj++ ){
       if( !isfinite(row[jj]) )
         return pp_error(err,nerr,"prediction sample %d, feature %d is not finite",ii+1,jj+1) ;
       if( m->distance==THD_CENTROID_CORRELATION ) mean+=row[jj] ;
     }
     if( m->distance==THD_CENTROID_CORRELATION ) mean/=(double)nfeature ;
     if( m->distance!=THD_CENTROID_EUCLIDEAN ){
       double rawss=0.0 ;
       for( jj=0 ; jj<nfeature ; jj++ ){
         double vv=row[jj],dd=vv-mean ; norm+=dd*dd ; rawss+=vv*vv ;
       }
       if( norm<=DBL_EPSILON*rawss ){
         if( m->distance==THD_CENTROID_CORRELATION )
           return pp_error(err,nerr,"prediction sample %d is constant for correlation distance",ii+1) ;
         return pp_error(err,nerr,"prediction sample %d has zero norm for cosine distance",ii+1) ;
       }
       norm=sqrt(norm) ;
     }
     for( cc=0 ; cc<m->nclass ; cc++ ){
       const double *cent=m->centroid+(size_t)cc*nfeature ; double value=0.0 ;
       if( m->distance==THD_CENTROID_EUCLIDEAN ){
         for( jj=0 ; jj<nfeature ; jj++ ){
           double dd=(double)row[jj]-cent[jj] ; value-=dd*dd ;
         }
       } else {
         double cmean=m->centroid_mean[cc] ;
         for( jj=0 ; jj<nfeature ; jj++ )
           value+=((double)row[jj]-mean)*(cent[jj]-cmean) ;
         value/=norm*m->centroid_norm[cc] ;
         if( value>1.0 ) value=1.0 ; else if( value < -1.0 ) value=-1.0 ;
       }
       if( !isfinite(value) )
         return pp_error(err,nerr,"decision score is not finite for sample %d, class %d",ii+1,cc) ;
       if( decision!=NULL ) decision[(size_t)ii*m->nclass+cc]=value ;
       if( cc==0 || value>best ){ best=value ; best_class=cc ; }
     }
     prediction[ii]=best_class ;
   }
   return 1 ;
}

const char * THD_mvpa_metric_name( int metric )
{
   switch( metric ){
     case THD_METRIC_ACCURACY:           return "accuracy" ;
     case THD_METRIC_BALANCED_ACCURACY:  return "balanced_accuracy" ;
     case THD_METRIC_PEARSON:            return "pearson" ;
     case THD_METRIC_SPEARMAN:           return "spearman" ;
     case THD_METRIC_CROSS_VALIDATED_R2: return "cross_validated_r2" ;
     case THD_METRIC_MAE:                return "mae" ;
     case THD_METRIC_RMSE:               return "rmse" ;
   }
   return NULL ;
}

int THD_mvpa_metric_parse( const char *name, int *metric, char *err, int nerr )
{
   int mm ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( name==NULL || metric==NULL )
     return pp_error(err,nerr,"metric parsing needs a name and output") ;
   for( mm=THD_METRIC_ACCURACY ; mm<=THD_METRIC_RMSE ; mm++ )
     if( strcasecmp(name,THD_mvpa_metric_name(mm))==0 ){ *metric=mm ; return 1 ; }
   return pp_error(err,nerr,"unknown MVPA metric '%s'",name) ;
}

static void mvpa_class_score_from_confusion( const int *confusion,
                                              int nsample, int nclass,
                                              double *recall,
                                              THD_mvpa_class_score *score )
{
   int cc,ncorrect=0,npresent=0 ; double recall_sum=0.0 ;
   for( cc=0 ; cc<nclass ; cc++ ){
     int pp,total=0 ;
     for( pp=0 ; pp<nclass ; pp++ ) total+=confusion[(size_t)cc*nclass+pp] ;
     ncorrect+=confusion[(size_t)cc*nclass+cc] ;
     if( total==0 ) recall[cc]=NAN ;
     else{
       recall[cc]=(double)confusion[(size_t)cc*nclass+cc]/(double)total ;
       recall_sum+=recall[cc] ; npresent++ ;
     }
   }
   score->nsample=nsample ; score->nclass=nclass ; score->ncorrect=ncorrect ;
   score->nclass_present=npresent ;
   score->accuracy=(double)ncorrect/(double)nsample ;
   score->balanced_accuracy=npresent>0 ? recall_sum/(double)npresent : NAN ;
}

int THD_mvpa_score_classification( const int *truth, const int *prediction,
                                    int nsample, int nclass,
                                    int *confusion, double *recall,
                                    THD_mvpa_class_score *score,
                                    char *err, int nerr )
{
   int ii ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( truth==NULL || prediction==NULL || confusion==NULL || recall==NULL ||
       score==NULL || nsample<1 || nclass<2 )
     return pp_error(err,nerr,"classification scoring needs samples, at least two classes, and output storage") ;
   if( (size_t)nclass > SIZE_MAX/sizeof(int)/(size_t)nclass )
     return pp_error(err,nerr,"classification confusion matrix is too large") ;
   memset(confusion,0,sizeof(int)*(size_t)nclass*nclass) ;
   for( ii=0 ; ii<nsample ; ii++ ){
     int tt=truth[ii],pp=prediction[ii] ;
     if( tt<0 || tt>=nclass )
       return pp_error(err,nerr,"true class %d at sample %d is outside [0,%d]",tt,ii+1,nclass-1) ;
     if( pp<0 || pp>=nclass )
       return pp_error(err,nerr,"predicted class %d at sample %d is outside [0,%d]",pp,ii+1,nclass-1) ;
     confusion[(size_t)tt*nclass+pp]++ ;
   }
   mvpa_class_score_from_confusion(confusion,nsample,nclass,recall,score) ;
   return 1 ;
}

static void mvpa_rank_values( const float *value, int nvalue, double *rank,
                              int *order, int *temp )
{
   int ii,width ;
   for( ii=0 ; ii<nvalue ; ii++ ) order[ii]=ii ;
   for( width=1 ; width<nvalue ; width*=2 ){
     int start ;
     for( start=0 ; start<nvalue ; start+=2*width ){
       int aa=start,bb=start+width,end=start+2*width,out=start ;
       if( bb>nvalue ) bb=nvalue ;
       if( end>nvalue ) end=nvalue ;
       {
         int amid=bb ;
         while( aa<amid && bb<end ){
           int ia=order[aa],ib=order[bb] ;
           if( value[ia]<value[ib] || (value[ia]==value[ib] && ia<ib) ) temp[out++]=order[aa++] ;
           else temp[out++]=order[bb++] ;
         }
         while( aa<amid ) temp[out++]=order[aa++] ;
         while( bb<end ) temp[out++]=order[bb++] ;
       }
     }
     for( ii=0 ; ii<nvalue ; ii++ ) order[ii]=temp[ii] ;
     if( width>nvalue/2 ) break ;
   }
   for( ii=0 ; ii<nvalue ; ){
     int jj=ii+1 ; double average ;
     while( jj<nvalue && value[order[jj]]==value[order[ii]] ) jj++ ;
     average=0.5*((double)ii+1.0+(double)jj) ;
     while( ii<jj ){ rank[order[ii]]=average ; ii++ ; }
   }
}

static double mvpa_corr_float( const float *aa, const float *bb, int nn )
{
   int ii ; double ma=0.0,mb=0.0,ab=0.0,a2=0.0,b2=0.0 ;
   for( ii=0 ; ii<nn ; ii++ ){ ma+=aa[ii] ; mb+=bb[ii] ; }
   ma/=(double)nn ; mb/=(double)nn ;
   for( ii=0 ; ii<nn ; ii++ ){
     double da=(double)aa[ii]-ma,db=(double)bb[ii]-mb ;
     ab+=da*db ; a2+=da*da ; b2+=db*db ;
   }
   return (a2>0.0 && b2>0.0) ? ab/sqrt(a2*b2) : NAN ;
}

static double mvpa_corr_double( const double *aa, const double *bb, int nn )
{
   int ii ; double ma=0.0,mb=0.0,ab=0.0,a2=0.0,b2=0.0 ;
   for( ii=0 ; ii<nn ; ii++ ){ ma+=aa[ii] ; mb+=bb[ii] ; }
   ma/=(double)nn ; mb/=(double)nn ;
   for( ii=0 ; ii<nn ; ii++ ){
     double da=aa[ii]-ma,db=bb[ii]-mb ;
     ab+=da*db ; a2+=da*da ; b2+=db*db ;
   }
   return (a2>0.0 && b2>0.0) ? ab/sqrt(a2*b2) : NAN ;
}

int THD_mvpa_score_continuous( const float *truth, const float *prediction,
                                const float *baseline, int nsample,
                                double *rank_scratch, int *index_scratch,
                                THD_mvpa_continuous_score *score,
                                char *err, int nerr )
{
   int ii ; double sae=0.0,sse=0.0,baseline_sse=0.0 ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( truth==NULL || prediction==NULL || baseline==NULL || rank_scratch==NULL ||
       index_scratch==NULL || score==NULL || nsample<1 )
     return pp_error(err,nerr,"continuous scoring needs nonempty inputs and scratch/output storage") ;
   for( ii=0 ; ii<nsample ; ii++ ){
     double ee,bb ;
     if( !isfinite(truth[ii]) || !isfinite(prediction[ii]) || !isfinite(baseline[ii]) )
       return pp_error(err,nerr,"continuous score input at sample %d is not finite",ii+1) ;
     ee=(double)truth[ii]-prediction[ii] ;
     bb=(double)truth[ii]-baseline[ii] ;
     sae+=fabs(ee) ; sse+=ee*ee ; baseline_sse+=bb*bb ;
   }
   mvpa_rank_values(truth,nsample,rank_scratch,index_scratch,index_scratch+nsample) ;
   mvpa_rank_values(prediction,nsample,rank_scratch+nsample,index_scratch,index_scratch+nsample) ;
   score->nsample=nsample ;
   score->pearson=mvpa_corr_float(truth,prediction,nsample) ;
   score->spearman=mvpa_corr_double(rank_scratch,rank_scratch+nsample,nsample) ;
   score->cv_r2=baseline_sse>0.0 ? 1.0-sse/baseline_sse : NAN ;
   score->mae=sae/(double)nsample ; score->rmse=sqrt(sse/(double)nsample) ;
   return 1 ;
}

/* Match complete prediction domains before computing descriptive contrasts. */
typedef struct {
   const THD_mvpa_pair_row *row;
   const char *group,*stratum;
   int index,other;
} mvpa_pair_index;

static int mvpa_pair_unit_order(const THD_mvpa_pair_row *a,const THD_mvpa_pair_row *b)
{
   int d=strcmp(a->unit_type,b->unit_type);if(d)return d;
   d=strcmp(a->unit,b->unit);if(d)return d;
   return strcmp(a->response,b->response);
}
static int mvpa_pair_key_order(const void *aa,const void *bb)
{
   const mvpa_pair_index *a=aa,*b=bb;int d=mvpa_pair_unit_order(a->row,b->row);
   return d?d:strcmp(a->row->sample,b->row->sample);
}
static int mvpa_pair_group_order(const void *aa,const void *bb)
{
   const mvpa_pair_index *a=aa,*b=bb;int d=mvpa_pair_unit_order(a->row,b->row);
   if(d)return d;d=strcmp(a->group,b->group);if(d)return d;
   d=strcmp(a->stratum,b->stratum);return d?d:strcmp(a->row->sample,b->row->sample);
}
static int mvpa_pair_same_group(const mvpa_pair_index *a,const mvpa_pair_index *b)
{return !mvpa_pair_unit_order(a->row,b->row) && !strcmp(a->group,b->group) && !strcmp(a->stratum,b->stratum);}

void THD_mvpa_pair_result_free(THD_mvpa_pair_result *r)
{if(r){free(r->row_a);free(r->row_b);free(r->score);free(r);}}

THD_mvpa_pair_result *THD_mvpa_compare_predictions(
 const THD_mvpa_pair_row *a,int na,const THD_mvpa_pair_row *b,int nb,
 int classification,int nclass,char *err,int nerr)
{
   THD_mvpa_pair_result *r=NULL;mvpa_pair_index *ia=NULL,*ib=NULL;
   float *y=NULL,*p=NULL,*base=NULL;double *rank=NULL,*recall=NULL;
   int *order=NULL,*truth=NULL,*pred=NULL,*conf=NULL;
   int i,j,k,side,scope,nt=0,capacity=0;
   if(err && nerr>0)err[0]='\0';
   if(!a || !b || na<1 || na!=nb || (classification!=0 && classification!=1) ||
      (classification && (nclass<2 || (size_t)nclass>SIZE_MAX/sizeof(int)/(size_t)nclass))){
      pp_error(err,nerr,"paired comparisons require equal nonempty row counts and valid scoring dimensions");return NULL;
   }
   if((size_t)na>SIZE_MAX/sizeof(mvpa_pair_index) || (size_t)na>SIZE_MAX/sizeof(double)/2){
      pp_error(err,nerr,"paired prediction domain is too large");return NULL;
   }
   r=calloc(1,sizeof(*r));ia=malloc(sizeof(*ia)*(size_t)na);ib=malloc(sizeof(*ib)*(size_t)na);
   if(!r || !ia || !ib)goto nomem;
   r->nrow=na;r->row_a=malloc(sizeof(int)*(size_t)na);r->row_b=malloc(sizeof(int)*(size_t)na);
   if(!r->row_a || !r->row_b)goto nomem;
   for(side=0;side<2;side++)for(i=0;i<na;i++){
      const THD_mvpa_pair_row *v=(side?b:a)+i;
      mvpa_pair_index *ix=(side?ib:ia)+i;
      const char *s[]={v->unit_type,v->unit,v->response,v->sample,v->subject,v->run,v->item,v->input_fold,v->fold,v->stratum};
      for(j=0;j<10;j++)if(!s[j] || !*s[j]){pp_error(err,nerr,"model %c row %d has an empty identity field",'A'+side,i+1);goto fail;}
      if(v->tested!=0 && v->tested!=1){pp_error(err,nerr,"invalid tested status at model %c row %d",'A'+side,i+1);goto fail;}
      if(classification){
         if(v->truth_class<0 || v->truth_class>=nclass || (v->tested && (v->prediction_class<0 || v->prediction_class>=nclass))){
            pp_error(err,nerr,"invalid class at model %c row %d",'A'+side,i+1);goto fail;
         }
      }else if(!isfinite(v->observed) || (v->tested && (!isfinite(v->predicted) || !isfinite(v->baseline)))){
         pp_error(err,nerr,"nonfinite continuous value at model %c row %d",'A'+side,i+1);goto fail;
      }
      ix->row=v;ix->index=i;
   }
   qsort(ia,(size_t)na,sizeof(*ia),mvpa_pair_key_order);qsort(ib,(size_t)na,sizeof(*ib),mvpa_pair_key_order);
   for(i=0;i<na;i++){
      const THD_mvpa_pair_row *u=ia[i].row,*v=ib[i].row;
      if(i && (!mvpa_pair_key_order(ia+i-1,ia+i) || !mvpa_pair_key_order(ib+i-1,ib+i))){
         pp_error(err,nerr,"duplicate unit/response/sample key in prediction input");goto fail;
      }
      if(mvpa_pair_key_order(ia+i,ib+i)){pp_error(err,nerr,"sample/unit/response domains differ near sample '%s'",u->sample);goto fail;}
#define PAIR_MATCH(field) if(strcmp(u->field,v->field)){pp_error(err,nerr,"paired %s mismatch at sample '%s'",#field,u->sample);goto fail;}
      PAIR_MATCH(subject);PAIR_MATCH(run);PAIR_MATCH(item);PAIR_MATCH(input_fold);PAIR_MATCH(fold);PAIR_MATCH(stratum);
#undef PAIR_MATCH
      if(u->tested!=v->tested){pp_error(err,nerr,"tested/training status mismatch at sample '%s'",u->sample);goto fail;}
      if(classification?u->truth_class!=v->truth_class:u->observed!=v->observed){pp_error(err,nerr,"observed outcome mismatch at sample '%s'",u->sample);goto fail;}
      if(!classification && u->tested && u->baseline!=v->baseline){pp_error(err,nerr,"training-mean baseline mismatch at sample '%s'; CV-R2 requires a common baseline",u->sample);goto fail;}
      r->row_a[i]=ia[i].index;r->row_b[i]=ib[i].index;ia[i].other=ib[i].index;
      if(u->tested)nt++;
   }
   if(!nt){pp_error(err,nerr,"paired comparison has no held-out predictions");goto fail;}
   /* Reuse ib as a held-out grouping index. No training row enters a score. */
   y=malloc(sizeof(float)*(size_t)nt);p=malloc(sizeof(float)*(size_t)nt);base=malloc(sizeof(float)*(size_t)nt);
   rank=malloc(sizeof(double)*2*(size_t)nt);order=malloc(sizeof(int)*2*(size_t)nt);
   truth=malloc(sizeof(int)*(size_t)nt);pred=malloc(sizeof(int)*(size_t)nt);
   if(classification){conf=malloc(sizeof(int)*(size_t)nclass*nclass);recall=malloc(sizeof(double)*(size_t)nclass);}
   if(!y || !p || !base || !rank || !order || !truth || !pred || (classification && (!conf || !recall)))goto nomem;
   for(scope=0;scope<4;scope++){
      for(i=0,j=0;i<na;i++)if(ia[i].row->tested){
         const THD_mvpa_pair_row *v=ia[i].row;ib[j]=ia[i];
         ib[j].group=scope==1?v->fold:scope==2?v->subject:scope==3?v->stratum:"All";
         ib[j].stratum=scope==1?v->stratum:scope==3?v->stratum:"NA";j++;
      }
      qsort(ib,(size_t)nt,sizeof(*ib),mvpa_pair_group_order);
      for(i=0;i<nt;i=j){
         THD_mvpa_pair_score *s;
         for(j=i+1;j<nt && mvpa_pair_same_group(ib+i,ib+j);j++);
         if(r->nscore==capacity){
            THD_mvpa_pair_score *tmp;int cap;
            if(capacity>INT_MAX/2){pp_error(err,nerr,"too many paired score groups");goto fail;}
            cap=capacity?2*capacity:32;
            if((size_t)cap>SIZE_MAX/sizeof(*tmp))goto nomem;
            tmp=realloc(r->score,sizeof(*tmp)*(size_t)cap);if(!tmp)goto nomem;
            r->score=tmp;capacity=cap;
         }
         s=r->score+r->nscore++;s->unit_type=ib[i].row->unit_type;s->unit=ib[i].row->unit;s->response=ib[i].row->response;
         s->scope=scope;s->group=ib[i].group;s->stratum=ib[i].stratum;s->nsample=j-i;
         for(k=0;k<7;k++)s->a[k]=s->b[k]=NAN;
         for(side=0;side<2;side++){
            double *out=side?s->b:s->a;
            for(k=i;k<j;k++){
               const THD_mvpa_pair_row *v=side?b+ib[k].other:ib[k].row;
               if(classification){truth[k-i]=v->truth_class;pred[k-i]=v->prediction_class;}
               else{y[k-i]=v->observed;p[k-i]=v->predicted;base[k-i]=v->baseline;}
            }
            if(classification){
               THD_mvpa_class_score sc;
               if(!THD_mvpa_score_classification(truth,pred,j-i,nclass,conf,recall,&sc,err,nerr))goto fail;
               out[THD_METRIC_ACCURACY]=sc.accuracy;out[THD_METRIC_BALANCED_ACCURACY]=sc.balanced_accuracy;
            }else{
               THD_mvpa_continuous_score sc;
               if(!THD_mvpa_score_continuous(y,p,base,j-i,rank,order,&sc,err,nerr))goto fail;
               out[THD_METRIC_PEARSON]=sc.pearson;out[THD_METRIC_SPEARMAN]=sc.spearman;
               out[THD_METRIC_CROSS_VALIDATED_R2]=sc.cv_r2;out[THD_METRIC_MAE]=sc.mae;out[THD_METRIC_RMSE]=sc.rmse;
            }
         }
      }
   }
   goto done;
nomem: pp_error(err,nerr,"cannot allocate paired comparison workspace");
fail: THD_mvpa_pair_result_free(r);r=NULL;
done: free(ia);free(ib);free(y);free(p);free(base);free(rank);free(order);free(truth);free(pred);free(conf);free(recall);return r;
}

/* Training-only OLS in a centered/scaled nuisance design. Twice-reorthogonalized
   modified Gram-Schmidt avoids forming the normal equations. */
struct THD_nuisance {
   const float *z;
   int n,k,pmax,pfit;
   double *mean,*scale,*q,*r,*beta,*xmean;
   float *residual;
};

static void nuisance_free(THD_nuisance *a)
{
   if(!a)return;
   free(a->mean);free(a->scale);free(a->q);free(a->r);
   free(a->beta);free(a->xmean);free(a->residual);free(a);
}

int THD_preprocess_set_nuisance(THD_preprocessor *p,const float *z,
   int n,int k,char *err,int nerr)
{
   THD_nuisance *a;size_t i;
   if(err && nerr>0)err[0]='\0';
   if(!p)return pp_error(err,nerr,"nuisance adjustment needs a preprocessor");
   if(!z && k==0){nuisance_free(p->nuisance);p->nuisance=NULL;p->nfit=0;return 1;}
   if(!z || n<1 || k<1 || k>=n || p->nfeature_alloc<1 ||
      (size_t)n>SIZE_MAX/sizeof(double)/(size_t)k ||
      (size_t)k>SIZE_MAX/sizeof(double)/(size_t)p->nfeature_alloc ||
      (size_t)n>SIZE_MAX/sizeof(float)/(size_t)p->nfeature_alloc)
      return pp_error(err,nerr,"invalid or excessive nuisance dimensions");
   for(i=0;i<(size_t)n*k;i++)if(!isfinite(z[i]))
      return pp_error(err,nerr,"nuisance covariates must be finite");
   a=calloc(1,sizeof(*a));if(!a)return pp_error(err,nerr,"cannot allocate nuisance adjustment");
   a->z=z;a->n=n;a->k=k;a->pmax=p->nfeature_alloc;
   a->mean=malloc(sizeof(double)*(size_t)k);a->scale=malloc(sizeof(double)*(size_t)k);
   a->q=malloc(sizeof(double)*(size_t)n*k);a->r=malloc(sizeof(double)*(size_t)k*k);
   a->beta=malloc(sizeof(double)*(size_t)k*a->pmax);a->xmean=malloc(sizeof(double)*(size_t)a->pmax);
   a->residual=malloc(sizeof(float)*(size_t)n*a->pmax);
   if(!a->mean || !a->scale || !a->q || !a->r || !a->beta || !a->xmean || !a->residual){
      nuisance_free(a);return pp_error(err,nerr,"cannot allocate nuisance buffers");
   }
   nuisance_free(p->nuisance);p->nuisance=a;p->nfit=0;return 1;
}

static double nuisance_value(const THD_nuisance *a,const float *x,int p,int row,int col)
{
   double raw=x[(size_t)row*p+col],fit=a->xmean[col],magnitude=fabs(raw)+fabs(fit);int j;
   for(j=0;j<a->k;j++){
      double term=((double)a->z[(size_t)row*a->k+j]-a->mean[j])/a->scale[j]*a->beta[(size_t)j*p+col];
      fit+=term;magnitude+=fabs(term);
   }
   /* Suppress roundoff remnants of exactly explained signals before zscoring. */
   return fabs(raw-fit)<=64*DBL_EPSILON*fmax(1.0,magnitude)?0.0:raw-fit;
}

static int nuisance_refit(THD_nuisance *a,const float *x,int n,int p,
   const int *rows,int nt,char *err,int nerr)
{
   int i,j,l,f,pass,k=a->k;
   a->pfit=0;
   if(n!=a->n || p>a->pmax || nt<=k+1)
      return pp_error(err,nerr,"nuisance adjustment needs more training rows than covariates plus intercept (%d rows, %d covariates)",nt,k);
   memset(a->r,0,sizeof(double)*(size_t)k*k);
   for(i=0;i<nt;i++)if(rows[i]<0 || rows[i]>=n)
      return pp_error(err,nerr,"invalid nuisance training row");
   for(j=0;j<k;j++){
      double mean=0,ss=0;
      for(i=0;i<nt;i++)mean+=a->z[(size_t)rows[i]*k+j];mean/=nt;a->mean[j]=mean;
      for(i=0;i<nt;i++){double v=(double)a->z[(size_t)rows[i]*k+j]-mean;ss+=v*v;}
      a->scale[j]=sqrt(ss/nt);
      if(!isfinite(a->scale[j]) || a->scale[j]<=DBL_EPSILON*fmax(1.0,fabs(mean)))
         return pp_error(err,nerr,"nuisance design is rank deficient: column %d is constant in training",j+1);
      for(i=0;i<nt;i++)a->q[(size_t)i*k+j]=((double)a->z[(size_t)rows[i]*k+j]-mean)/a->scale[j];
      for(pass=0;pass<2;pass++)for(l=0;l<j;l++){
         double dot=0;for(i=0;i<nt;i++)dot+=a->q[(size_t)i*k+l]*a->q[(size_t)i*k+j];
         a->r[(size_t)l*k+j]+=dot;
         for(i=0;i<nt;i++)a->q[(size_t)i*k+j]-=dot*a->q[(size_t)i*k+l];
      }
      ss=0;for(i=0;i<nt;i++){double v=a->q[(size_t)i*k+j];ss+=v*v;}
      a->r[(size_t)j*k+j]=sqrt(ss);
      if(!isfinite(ss) || sqrt(ss)<=1e-10*sqrt((double)nt))
         return pp_error(err,nerr,"nuisance design is rank deficient or ill-conditioned at column %d",j+1);
      for(i=0;i<nt;i++)a->q[(size_t)i*k+j]/=sqrt(ss);
   }
   for(f=0;f<p;f++){
      double mean=0;for(i=0;i<nt;i++)mean+=x[(size_t)rows[i]*p+f];mean/=nt;a->xmean[f]=mean;
      for(j=0;j<k;j++){
         double dot=0;for(i=0;i<nt;i++)dot+=a->q[(size_t)i*k+j]*((double)x[(size_t)rows[i]*p+f]-mean);
         a->beta[(size_t)j*p+f]=dot;
      }
      for(j=k-1;j>=0;j--){
         double b=a->beta[(size_t)j*p+f];
         for(l=j+1;l<k;l++)b-=a->r[(size_t)j*k+l]*a->beta[(size_t)l*p+f];
         a->beta[(size_t)j*p+f]=b/a->r[(size_t)j*k+j];
      }
   }
   for(i=0;i<nt;i++)for(f=0;f<p;f++){
      double value=nuisance_value(a,x,p,rows[i],f);
      if(!isfinite(value) || fabs(value)>FLT_MAX)return pp_error(err,nerr,"nuisance residual is outside the float range");
      a->residual[(size_t)rows[i]*p+f]=(float)value;
   }
   a->pfit=p;return 1;
}

static int nuisance_permutation(const THD_preprocessor *p,const int *perm,int n,char *err,int nerr)
{
   const THD_nuisance *a=p->nuisance;int i,j;
   if(!a)return 1;
   if(a->n!=n)return pp_error(err,nerr,"nuisance permutation dimensions do not match the sample set");
   for(i=0;i<a->n;i++)for(j=0;j<a->k;j++)
      if(a->z[(size_t)i*a->k+j]!=a->z[(size_t)perm[i]*a->k+j])
         return pp_error(err,nerr,"nuisance permutation must preserve every covariate row; use nuisance-constant exchangeability blocks");
   return 1;
}

void THD_preprocess_free( THD_preprocessor *p )
{
   if( p==NULL ) return ;
   free(p->group_value) ; free(p->group_count) ;
   free(p->center) ; free(p->scale) ;
   free(p->constant) ; nuisance_free(p->nuisance) ; free(p) ;
}

const char * THD_preprocess_name( int method )
{
   switch( method ){
     case THD_PREP_NONE:           return "none" ;
     case THD_PREP_FEATURE_ZSCORE: return "feature_zscore" ;
     case THD_PREP_SAMPLE_CENTER:  return "sample_center" ;
     case THD_PREP_RUN_CENTER:     return "run_center" ;
   }
   return NULL ;
}

int THD_preprocess_parse( const char *name, int *method, char *err, int nerr )
{
   int mm ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( name==NULL || method==NULL )
     return pp_error(err,nerr,"preprocessing needs a method name and output") ;
   for( mm=THD_PREP_NONE ; mm<=THD_PREP_RUN_CENTER ; mm++ )
     if( strcasecmp(name,THD_preprocess_name(mm))==0 ){ *method=mm ; return 1 ; }
   return pp_error(err,nerr,"unknown preprocessing method '%s'",name) ;
}

static int pp_check_rows( const float *x, int nsample, int nfeature,
                          const int *index, int nindex, int **seen_out,
                          char *err, int nerr )
{
   int ii,jj,*seen ;
   if( x==NULL || nsample<1 || nfeature<1 || index==NULL || nindex<1 )
     return pp_error(err,nerr,"preprocessing needs a nonempty matrix and sample list") ;
   seen=(int *)calloc((size_t)nsample,sizeof(int)) ;
   if( seen==NULL ) return pp_error(err,nerr,"out of memory validating preprocessing rows") ;
   for( ii=0 ; ii<nindex ; ii++ ){
     int ss=index[ii] ;
     if( ss<0 || ss>=nsample || seen[ss] ){
       free(seen) ; return pp_error(err,nerr,"invalid or duplicate preprocessing sample index %d",ss) ;
     }
     seen[ss]=1 ;
     for( jj=0 ; jj<nfeature ; jj++ ) if( !isfinite(x[(size_t)ss*nfeature+jj]) ){
       free(seen) ; return pp_error(err,nerr,"sample %d, feature %d is not finite",ss+1,jj+1) ;
     }
   }
   if( seen_out!=NULL ) *seen_out=seen ; else free(seen) ;
   return 1 ;
}

static int pp_group_index( const THD_preprocessor *p, int value )
{
   int gg ;
   for( gg=0 ; gg<p->ngroup ; gg++ ) if( p->group_value[gg]==value ) return gg ;
   return -1 ;
}

static THD_preprocessor * pp_alloc( int max_feature, int max_group,
                                    char *err, int nerr )
{
   THD_preprocessor *p ;
   if( max_feature<1 || max_group<1 ||
       (size_t)max_group > SIZE_MAX/sizeof(double)/(size_t)max_feature ){
     pp_error(err,nerr,"invalid or excessive preprocessing workspace dimensions") ;
     return NULL ;
   }
   p=(THD_preprocessor *)calloc(1,sizeof(*p)) ;
   if( p==NULL ){ pp_error(err,nerr,"out of memory allocating preprocessing") ; return NULL ; }
   p->nfeature_alloc=max_feature ; p->ngroup_alloc=max_group ;
   p->group_value=(int *)calloc((size_t)max_group,sizeof(int)) ;
   p->group_count=(int *)calloc((size_t)max_group,sizeof(int)) ;
   p->center=(double *)calloc((size_t)max_group*max_feature,sizeof(double)) ;
   p->scale=(double *)calloc((size_t)max_feature,sizeof(double)) ;
   p->constant=(unsigned char *)calloc((size_t)max_feature,sizeof(unsigned char)) ;
   if( p->group_value==NULL || p->group_count==NULL || p->center==NULL ||
       p->scale==NULL || p->constant==NULL ){
     THD_preprocess_free(p) ;
     pp_error(err,nerr,"out of memory allocating preprocessing") ; return NULL ;
   }
   return p ;
}

static int pp_refit( THD_preprocessor *p, int method, const float *x,
                     int nsample, int nfeature, const int *fit_index, int nfit,
                     const int *group_id, int check_rows, char *err, int nerr )
{
   int ii,jj ;
   if( p==NULL || x==NULL || fit_index==NULL || nfit<1 || nfeature<1 ||
       nfeature>p->nfeature_alloc || THD_preprocess_name(method)==NULL )
     return pp_error(err,nerr,"preprocessing input exceeds its fitted workspace") ;
   if( check_rows && !pp_check_rows(x,nsample,nfeature,fit_index,nfit,NULL,err,nerr) )
     return 0 ;
   if( method==THD_PREP_RUN_CENTER && group_id==NULL )
     return pp_error(err,nerr,"run_center needs one group ID per sample") ;
   p->nfit=0;
   if(p->nuisance){
      if(!nuisance_refit(p->nuisance,x,nsample,nfeature,fit_index,nfit,err,nerr))return 0;
      x=p->nuisance->residual;
   }
   p->method=method ; p->nfeature=nfeature ; p->nfit=nfit ;
   p->ngroup=0 ; p->nconstant=0 ;
   if( method==THD_PREP_NONE || method==THD_PREP_SAMPLE_CENTER ) return 1 ;

   if( method==THD_PREP_FEATURE_ZSCORE ){
     p->ngroup=1 ;
     memset(p->center,0,sizeof(double)*(size_t)nfeature) ;
     memset(p->constant,0,sizeof(unsigned char)*(size_t)nfeature) ;
     for( ii=0 ; ii<nfit ; ii++ ){
       const float *row=x+(size_t)fit_index[ii]*nfeature ;
       for( jj=0 ; jj<nfeature ; jj++ ) p->center[jj]+=row[jj] ;
     }
     for( jj=0 ; jj<nfeature ; jj++ ) p->center[jj]/=(double)nfit ;
     for( jj=0 ; jj<nfeature ; jj++ ){
       double ss=0.0,tol ;
       for( ii=0 ; ii<nfit ; ii++ ){
         double dd=(double)x[(size_t)fit_index[ii]*nfeature+jj]-p->center[jj] ;
         ss+=dd*dd ;
       }
       p->scale[jj]=sqrt(ss/(double)nfit) ;
       tol=DBL_EPSILON*fmax(1.0,fabs(p->center[jj])) ;
       if( p->scale[jj]<=tol ){
         p->constant[jj]=1 ; p->nconstant++ ; p->scale[jj]=1.0 ;
       }
     }
     return 1 ;
   }

   for( ii=0 ; ii<nfit ; ii++ ){
     int value=group_id[fit_index[ii]],gg ;
     if( value<0 )
       return pp_error(err,nerr,"sample %d has no valid run/group ID",fit_index[ii]+1) ;
     for( gg=0 ; gg<p->ngroup ; gg++ ) if( p->group_value[gg]==value ) break ;
     if( gg==p->ngroup ){
       if( p->ngroup>=p->ngroup_alloc )
         return pp_error(err,nerr,"run_center has more groups than its workspace") ;
       p->group_value[p->ngroup++]=value ;
     }
   }
   memset(p->center,0,sizeof(double)*(size_t)p->ngroup*nfeature) ;
   memset(p->group_count,0,sizeof(int)*(size_t)p->ngroup) ;
   for( ii=0 ; ii<nfit ; ii++ ){
     int ss=fit_index[ii],gg=pp_group_index(p,group_id[ss]) ;
     p->group_count[gg]++ ;
     for( jj=0 ; jj<nfeature ; jj++ )
       p->center[(size_t)gg*nfeature+jj]+=x[(size_t)ss*nfeature+jj] ;
   }
   for( ii=0 ; ii<p->ngroup ; ii++ ) for( jj=0 ; jj<nfeature ; jj++ )
     p->center[(size_t)ii*nfeature+jj]/=(double)p->group_count[ii] ;
   return 1 ;
}

int THD_mvpa_check_nuisance(const THD_foldplan *plan,
   const THD_nested_foldplan *nested,const float *z,int k,char *err,int nerr)
{
   THD_preprocessor *p=NULL;float *dummy=NULL;int *rows=NULL,f,g,i,ok=0;char why[512];
   if(!THD_foldplan_validate(plan,err,nerr))return 0;
   if(nested && (nested->outer!=plan || !nested->inner))return pp_error(err,nerr,"nuisance nested plan does not match outer plan");
   p=pp_alloc(1,1,err,nerr);if(!p)return 0;
   dummy=calloc((size_t)plan->nsample,sizeof(float));rows=malloc(sizeof(int)*(size_t)plan->nsample);
   if(!dummy || !rows){pp_error(err,nerr,"cannot allocate nuisance fold validation");goto done;}
   if(!THD_preprocess_set_nuisance(p,z,plan->nsample,k,err,nerr))goto done;
   for(f=0;f<plan->nfold;f++){
      if(!pp_refit(p,THD_PREP_NONE,dummy,plan->nsample,1,plan->train[f],plan->ntrain[f],NULL,0,why,sizeof(why))){
         pp_error(err,nerr,"outer fold %s: %s",plan->name[f],why);goto done;
      }
      if(nested){
         const THD_foldplan *in=nested->inner[f];
         if(!THD_foldplan_validate(in,err,nerr))goto done;
         if(in->nsample!=plan->ntrain[f]){pp_error(err,nerr,"nuisance inner fold size mismatch");goto done;}
         for(g=0;g<in->nfold;g++){
            for(i=0;i<in->ntrain[g];i++)rows[i]=plan->train[f][in->train[g][i]];
            if(!pp_refit(p,THD_PREP_NONE,dummy,plan->nsample,1,rows,in->ntrain[g],NULL,0,why,sizeof(why))){
               pp_error(err,nerr,"outer fold %s, inner fold %s: %s",plan->name[f],in->name[g],why);goto done;
            }
         }
      }
   }
   ok=1;
done:
   free(rows);free(dummy);THD_preprocess_free(p);return ok;
}

THD_preprocessor * THD_preprocess_fit( int method, const float *x,
                                        int nsample, int nfeature,
                                        const int *fit_index, int nfit,
                                        const int *group_id,
                                        char *err, int nerr )
{
   THD_preprocessor *p ; int ii,jj,max_group=1 ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( THD_preprocess_name(method)==NULL || x==NULL || fit_index==NULL ||
       nsample<1 || nfeature<1 || nfit<1 ){
     pp_error(err,nerr,"preprocessing needs a valid method, matrix, and sample list") ;
     return NULL ;
   }
   if( !pp_check_rows(x,nsample,nfeature,fit_index,nfit,NULL,err,nerr) ) return NULL ;
   if( method==THD_PREP_RUN_CENTER ){
     if( group_id==NULL ){
       pp_error(err,nerr,"run_center needs one group ID per sample") ; return NULL ;
     }
     max_group=0 ;
     for( ii=0 ; ii<nfit ; ii++ ){
       int value=group_id[fit_index[ii]] ;
       if( value<0 ){
         pp_error(err,nerr,"sample %d has no valid run/group ID",fit_index[ii]+1) ;
         return NULL ;
       }
       for( jj=0 ; jj<ii ; jj++ ) if( group_id[fit_index[jj]]==value ) break ;
       if( jj==ii ) max_group++ ;
     }
   }
   p=pp_alloc(nfeature,max_group,err,nerr) ;
   if( p==NULL ) return NULL ;
   if( !pp_refit(p,method,x,nsample,nfeature,fit_index,nfit,group_id,0,err,nerr) ){
     THD_preprocess_free(p) ; return NULL ;
   }
   return p ;
}

static int pp_apply( const THD_preprocessor *p, const float *x,
                     int nsample, int nfeature, const int *apply_index,
                     int napply, const int *group_id, float *out,
                     int check_rows, char *err, int nerr )
{
   int ii,jj ;
   if( p==NULL || out==NULL || p->nfit<1 || p->nfeature!=nfeature ||
       (p->nuisance && (p->nuisance->n!=nsample || p->nuisance->pfit!=nfeature)) )
     return pp_error(err,nerr,"invalid fitted preprocessing object or output shape") ;
   if( out==x ) return pp_error(err,nerr,"preprocessing output must not alias its input matrix") ;
   if( p->method==THD_PREP_RUN_CENTER && group_id==NULL )
     return pp_error(err,nerr,"run_center needs one group ID per sample") ;
   if( check_rows && !pp_check_rows(x,nsample,nfeature,apply_index,napply,NULL,err,nerr) )
     return 0 ;
   for( ii=0 ; ii<napply ; ii++ ){
     int ss=apply_index[ii],gg=-1 ; const float *row=x+(size_t)ss*nfeature ;
     float *dest=out+(size_t)ii*nfeature ; double rowmean=0.0 ;
     if(p->nuisance){
       for(jj=0;jj<nfeature;jj++){
         double v=nuisance_value(p->nuisance,x,nfeature,ss,jj);
         if(!isfinite(v) || fabs(v)>FLT_MAX)return pp_error(err,nerr,"nuisance residual is outside the float range at sample %d",ss+1);
         dest[jj]=(float)v;
       }
       row=dest;
     }
     if( p->method==THD_PREP_RUN_CENTER ){
       gg=pp_group_index(p,group_id[ss]) ;
       if( gg<0 ) return pp_error(err,nerr,"sample %d has run/group %d, which was not present in training",ss+1,group_id[ss]) ;
     } else if( p->method==THD_PREP_SAMPLE_CENTER ){
       for( jj=0 ; jj<nfeature ; jj++ ) rowmean+=row[jj] ;
       rowmean/=(double)nfeature ;
     }
     for( jj=0 ; jj<nfeature ; jj++ ){
       double value ;
       if( p->method==THD_PREP_NONE ) value=row[jj] ;
       else if( p->method==THD_PREP_FEATURE_ZSCORE )
         value=p->constant[jj]?0.0:((double)row[jj]-p->center[jj])/p->scale[jj] ;
       else if( p->method==THD_PREP_SAMPLE_CENTER ) value=(double)row[jj]-rowmean ;
       else value=(double)row[jj]-p->center[(size_t)gg*nfeature+jj] ;
       if( !isfinite(value) || fabs(value)>FLT_MAX )
         return pp_error(err,nerr,"sample %d, feature %d transforms outside the float range",ss+1,jj+1) ;
       dest[jj]=(float)value ;
     }
   }
   return 1 ;
}

int THD_preprocess_apply( const THD_preprocessor *p, const float *x,
                           int nsample, int nfeature,
                           const int *apply_index, int napply,
                           const int *group_id, float *out,
                           char *err, int nerr )
{
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   return pp_apply(p,x,nsample,nfeature,apply_index,napply,group_id,out,1,err,nerr) ;
}

int THD_preprocess_fold( const THD_foldplan *plan, int fold, int method,
                          const float *x, int nfeature, const int *group_id,
                          float *train_out, float *test_out,
                          THD_preprocessor **fitted, char *err, int nerr )
{
   THD_preprocessor *p ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( fitted!=NULL ) *fitted=NULL ;
   if( !THD_foldplan_validate(plan,err,nerr) ) return 0 ;
   if( fold<0 || fold>=plan->nfold ) return pp_error(err,nerr,"invalid fold index %d",fold) ;
   if( train_out==NULL || test_out==NULL ) return pp_error(err,nerr,"fold preprocessing needs train and test outputs") ;
   p=THD_preprocess_fit(method,x,plan->nsample,nfeature,plan->train[fold],
                        plan->ntrain[fold],group_id,err,nerr) ;
   if( p==NULL ) return 0 ;
   if( !THD_preprocess_apply(p,x,plan->nsample,nfeature,plan->train[fold],
                             plan->ntrain[fold],group_id,train_out,err,nerr) ||
       !THD_preprocess_apply(p,x,plan->nsample,nfeature,plan->test[fold],
                             plan->ntest[fold],group_id,test_out,err,nerr) ){
     THD_preprocess_free(p) ; return 0 ;
   }
   if( fitted!=NULL ) *fitted=p ; else THD_preprocess_free(p) ;
   return 1 ;
}

struct THD_lda_model {
   int mode,max_sample,max_feature,nclass,p,fitted,form;
   int *count;
   double reference,sd;
   double *center,*mean,*variance,*weight,*intercept;
   double *residual,*factor,*rhs;
};

void THD_lda_free(THD_lda_model *m)
{
   if(!m)return;
   free(m->count);free(m->center);free(m->mean);free(m->variance);
   free(m->weight);free(m->intercept);free(m->residual);free(m->factor);free(m->rhs);free(m);
}

THD_lda_model *THD_lda_alloc(int mode,int n,int p,int nc,char *err,int nerr)
{
   THD_lda_model *m;int d=n<p?n:p;
   if((mode!=THD_LDA_DIAGONAL && mode!=THD_LDA_SHRINKAGE) || n<1 || p<1 || nc<2 ||
      (size_t)p>SIZE_MAX/sizeof(double)/(size_t)nc ||
      (mode==THD_LDA_SHRINKAGE && ((size_t)p>SIZE_MAX/sizeof(double)/(size_t)n ||
                                 (size_t)d>SIZE_MAX/sizeof(double)/(size_t)d))){
      pp_error(err,nerr,"invalid LDA mode or allocation dimensions");return NULL;
   }
   m=calloc(1,sizeof(*m));if(!m){pp_error(err,nerr,"cannot allocate LDA model");return NULL;}
   m->mode=mode;m->max_sample=n;m->max_feature=p;m->nclass=nc;
   m->count=malloc(sizeof(int)*(size_t)nc);m->center=malloc(sizeof(double)*(size_t)p);
   m->mean=malloc(sizeof(double)*(size_t)nc*p);m->variance=malloc(sizeof(double)*(size_t)p);
   m->weight=malloc(sizeof(double)*(size_t)nc*p);m->intercept=malloc(sizeof(double)*(size_t)nc);
   if(mode==THD_LDA_SHRINKAGE){
      m->residual=malloc(sizeof(double)*(size_t)n*p);
      m->factor=malloc(sizeof(double)*(size_t)d*d);m->rhs=malloc(sizeof(double)*(size_t)d*nc);
   }
   if(!m->count || !m->center || !m->mean || !m->variance || !m->weight || !m->intercept ||
      (mode==THD_LDA_SHRINKAGE && (!m->residual || !m->factor || !m->rhs))){
      THD_lda_free(m);pp_error(err,nerr,"cannot allocate LDA buffers");return NULL;
   }
   return m;
}

static int lda_solve(double *a,double *b,int d,int q,char *err,int nerr)
{
   int i,j,k,o;
   for(i=0;i<d;i++)for(j=0;j<=i;j++){
      double v=a[(size_t)i*d+j];
      for(k=0;k<j;k++)v-=a[(size_t)i*d+k]*a[(size_t)j*d+k];
      if(i==j){
         if(!isfinite(v) || v<=0)return pp_error(err,nerr,"LDA factorization failed; increase shrinkage or variance floor");
         a[(size_t)i*d+j]=sqrt(v);
      }else a[(size_t)i*d+j]=v/a[(size_t)j*d+j];
   }
   for(i=0;i<d;i++)for(o=0;o<q;o++){
      double v=b[(size_t)i*q+o];
      for(k=0;k<i;k++)v-=a[(size_t)i*d+k]*b[(size_t)k*q+o];
      b[(size_t)i*q+o]=v/a[(size_t)i*d+i];
   }
   for(i=d-1;i>=0;i--)for(o=0;o<q;o++){
      double v=b[(size_t)i*q+o];
      for(k=i+1;k<d;k++)v-=a[(size_t)k*d+i]*b[(size_t)k*q+o];
      b[(size_t)i*q+o]=v/a[(size_t)i*d+i];
   }
   return 1;
}

int THD_lda_fit(THD_lda_model *m,const float *x,const int *classes,int n,int p,
 double alpha,double floor,int empirical,char *err,int nerr)
{
   int i,j,k,c,d,dual,nc;double reference=0,shrink,diag;
   if(m)m->fitted=0;
   if(!m || !x || !classes || n<=m->nclass || n>m->max_sample || p<1 || p>m->max_feature ||
      !isfinite(floor) || floor<1e-12 || floor>1 ||
      (m->mode==THD_LDA_SHRINKAGE && (!isfinite(alpha) || alpha<=0 || alpha>1)))
      return pp_error(err,nerr,"LDA needs more training rows than classes, matching dimensions, a variance floor in [1e-12,1], and shrinkage in (0,1]");
   nc=m->nclass;
   memset(m->count,0,sizeof(int)*(size_t)nc);memset(m->center,0,sizeof(double)*(size_t)p);
   memset(m->mean,0,sizeof(double)*(size_t)nc*p);memset(m->variance,0,sizeof(double)*(size_t)p);
   for(i=0;i<n;i++){
      c=classes[i];if(c<0 || c>=nc)return pp_error(err,nerr,"LDA row %d has invalid class",i+1);
      m->count[c]++;
      for(j=0;j<p;j++){
         double v=x[(size_t)i*p+j];if(!isfinite(v))return pp_error(err,nerr,"LDA predictor is not finite");
         m->center[j]+=v;m->mean[(size_t)c*p+j]+=v;
      }
   }
   for(j=0;j<p;j++)m->center[j]/=n;
   for(c=0;c<nc;c++){
      if(!m->count[c])return pp_error(err,nerr,"LDA training rows have no class %d",c+1);
      for(j=0;j<p;j++)m->mean[(size_t)c*p+j]/=m->count[c];
   }
   for(i=0;i<n;i++)for(j=0;j<p;j++){
      double v=x[(size_t)i*p+j]-m->mean[(size_t)classes[i]*p+j];
      m->variance[j]+=v*v;
      if(m->residual)m->residual[(size_t)i*p+j]=v;
   }
   for(j=0;j<p;j++){m->variance[j]/=n-nc;reference+=m->variance[j];}
   reference/=p;
   if(reference==0){
      for(i=0;i<n;i++)for(j=0;j<p;j++){
         double v=x[(size_t)i*p+j]-m->center[j];reference+=v*v/((double)(n-1)*p);
      }
      if(reference==0)reference=1;
   }
   if(!isfinite(reference) || reference<=0)return pp_error(err,nerr,"LDA covariance scale is not finite and positive");
   /* Work in units of sqrt(reference), keeping the covariance factor scaled. */
   m->reference=reference;m->sd=sqrt(reference);m->form=THD_LDA_FORM_DIAGONAL;
   for(c=0;c<nc;c++)for(j=0;j<p;j++)m->mean[(size_t)c*p+j]=(m->mean[(size_t)c*p+j]-m->center[j])/m->sd;
   if(m->mode==THD_LDA_DIAGONAL || alpha==1){
      for(c=0;c<nc;c++)for(j=0;j<p;j++){
         double v=m->mode==THD_LDA_DIAGONAL?fmax(m->variance[j]/reference,floor):1;
         m->weight[(size_t)c*p+j]=m->mean[(size_t)c*p+j]/v;
      }
   }else{
      shrink=(1-alpha)/(n-nc);diag=fmax(alpha,floor);dual=p>n;d=dual?n:p;
      /* Woodbury uses d*I + shrink*R*R' when a feature-space factor is larger. */
      m->form=dual?THD_LDA_FORM_DUAL:THD_LDA_FORM_PRIMAL;
      for(i=0;i<n;i++)for(j=0;j<p;j++)m->residual[(size_t)i*p+j]/=m->sd;
      for(i=0;i<d;i++)for(j=0;j<=i;j++){
         double v=0;
         for(k=0;k<(dual?p:n);k++)v+=dual?
            m->residual[(size_t)i*p+k]*m->residual[(size_t)j*p+k]:
            m->residual[(size_t)k*p+i]*m->residual[(size_t)k*p+j];
         m->factor[(size_t)i*d+j]=shrink*v+(i==j?diag:0);
      }
      for(i=0;i<d;i++)for(c=0;c<nc;c++){
         double v=0;
         if(dual)for(j=0;j<p;j++)v+=m->residual[(size_t)i*p+j]*m->mean[(size_t)c*p+j];
         else v=m->mean[(size_t)c*p+i];
         m->rhs[(size_t)i*nc+c]=v;
      }
      if(!lda_solve(m->factor,m->rhs,d,nc,err,nerr))return 0;
      for(c=0;c<nc;c++)for(j=0;j<p;j++){
         double v;
         if(dual){
            double s=0;for(i=0;i<n;i++)s+=m->residual[(size_t)i*p+j]*m->rhs[(size_t)i*nc+c];
            v=(m->mean[(size_t)c*p+j]-shrink*s)/diag;
         }else v=m->rhs[(size_t)j*nc+c];
         m->weight[(size_t)c*p+j]=v;
      }
   }
   for(c=0;c<nc;c++){
      double v=0;
      for(j=0;j<p;j++)v+=m->mean[(size_t)c*p+j]*m->weight[(size_t)c*p+j];
      m->intercept[c]=-.5*v+log(empirical?(double)m->count[c]/n:1.0/nc);
      if(!isfinite(m->intercept[c]))return pp_error(err,nerr,"LDA discriminant is not finite");
   }
   m->p=p;m->fitted=1;return 1;
}

int THD_lda_predict(const THD_lda_model *m,const float *x,int n,int p,int *prediction,
 double *decision,char *err,int nerr)
{
   int i,j,c;
   if(!m || !m->fitted || !x || !prediction || n<1 || p!=m->p)
      return pp_error(err,nerr,"LDA prediction needs a fitted model and matching rows");
   for(i=0;i<n;i++){
      int best=0;double bestscore=-HUGE_VAL;
      for(c=0;c<m->nclass;c++){
         double v=m->intercept[c];
         for(j=0;j<p;j++){
            double z=x[(size_t)i*p+j];if(!isfinite(z))return pp_error(err,nerr,"LDA prediction input is not finite");
            v+=(z-m->center[j])/m->sd*m->weight[(size_t)c*p+j];
         }
         if(!isfinite(v))return pp_error(err,nerr,"LDA decision is not finite");
         if(decision)decision[(size_t)i*m->nclass+c]=v;
         if(v>bestscore){bestscore=v;best=c;}
      }
      prediction[i]=best;
   }
   return 1;
}

struct THD_ridge_workspace {
   int maxn,maxp,maxq,n,p,q,fitted;
   double *factor,*rhs,*coef,*xm,*ym;
};

void THD_ridge_free(THD_ridge_workspace *w)
{
   if(!w) return;
   free(w->factor); free(w->rhs); free(w->coef); free(w->xm); free(w->ym); free(w);
}

THD_ridge_workspace *THD_ridge_alloc(int n,int p,int q,char *err,int nerr)
{
   THD_ridge_workspace *w; int d;
   if(n<1 || p<1 || q<1){pp_error(err,nerr,"ridge dimensions must be positive");return NULL;}
   d=n<p?n:p;
   if((size_t)d>SIZE_MAX/sizeof(double)/(size_t)d ||
      (size_t)p>SIZE_MAX/sizeof(double)/(size_t)q){
      pp_error(err,nerr,"ridge dimensions are too large");return NULL;
   }
   w=calloc(1,sizeof(*w));
   if(!w){pp_error(err,nerr,"out of memory allocating ridge");return NULL;}
   w->maxn=n;w->maxp=p;w->maxq=q;
   w->factor=malloc(sizeof(double)*(size_t)d*d);
   w->rhs=malloc(sizeof(double)*(size_t)d*q);
   w->coef=malloc(sizeof(double)*(size_t)p*q);
   w->xm=malloc(sizeof(double)*(size_t)p);w->ym=malloc(sizeof(double)*(size_t)q);
   if(!w->factor || !w->rhs || !w->coef || !w->xm || !w->ym){
      THD_ridge_free(w);pp_error(err,nerr,"out of memory allocating ridge");return NULL;
   }
   return w;
}

static int ridge_fit_penalties(THD_ridge_workspace *w,const float *x,const float *y,
 int n,int p,int q,double lambda,const double *penalties,int form,char *err,int nerr)
{
   int i,j,k,t,o,d,dual,g; double *a,*b;
   if(w) w->fitted=0;
   if(!w || !x || !y || n<1 || n>w->maxn || p<1 || p>w->maxp ||
      q<1 || q>w->maxq || !isfinite(lambda) || lambda<=0 || form<0 || form>2)
      return pp_error(err,nerr,"invalid ridge dimensions, form, or positive lambda");
   if(penalties)for(o=0;o<q;o++)if(!isfinite(penalties[o]) || penalties[o]<=0)
      return pp_error(err,nerr,"response ridge penalties must be finite and positive");
   dual=form==THD_RIDGE_DUAL || (form==THD_RIDGE_AUTO && p>n);
   d=dual?n:p;
   if(d>(w->maxn<w->maxp?w->maxn:w->maxp))
      return pp_error(err,nerr,"forced ridge form exceeds the allocated factor capacity");
   a=w->factor;b=w->rhs;
   for(j=0;j<p;j++){
      double s=0;for(i=0;i<n;i++){
         double v=x[(size_t)i*p+j];if(!isfinite(v)) return pp_error(err,nerr,"ridge predictor is not finite");s+=v;
      }w->xm[j]=s/n;
   }
   for(o=0;o<q;o++){
      double s=0;for(i=0;i<n;i++){
         double v=y[(size_t)i*q+o];if(!isfinite(v)) return pp_error(err,nerr,"ridge response is not finite");s+=v;
      }w->ym[o]=s/n;
   }
   for(g=0;g<(penalties?q:1);g++){
      if(penalties){
         for(o=0;o<g;o++)if(penalties[o]==penalties[g])break;
         if(o<g)continue;
         lambda=penalties[g];
      }
   for(i=0;i<d;i++) for(j=0;j<=i;j++){
      double s=0;
      for(t=0;t<(dual?p:n);t++){
         double u=dual?x[(size_t)i*p+t]-w->xm[t]:x[(size_t)t*p+i]-w->xm[i];
         double v=dual?x[(size_t)j*p+t]-w->xm[t]:x[(size_t)t*p+j]-w->xm[j];
         s+=u*v;
      }
      a[(size_t)i*d+j]=s+(i==j?lambda:0);
   }
   /* Cholesky factorization and triangular solves share one factor for all responses. */
   for(i=0;i<d;i++) for(j=0;j<=i;j++){
      double s=a[(size_t)i*d+j];
      for(k=0;k<j;k++) s-=a[(size_t)i*d+k]*a[(size_t)j*d+k];
      if(i==j){
         if(!isfinite(s) || s<=0) return pp_error(err,nerr,"ridge factorization failed; increase lambda or rescale predictors");
         a[(size_t)i*d+j]=sqrt(s);
      }else a[(size_t)i*d+j]=s/a[(size_t)j*d+j];
   }
   for(i=0;i<d;i++) for(o=0;o<q;o++){
      double s=0;if(penalties && penalties[o]!=lambda)continue;
      if(dual) s=y[(size_t)i*q+o]-w->ym[o];
      else for(t=0;t<n;t++) s+=(x[(size_t)t*p+i]-w->xm[i])*(y[(size_t)t*q+o]-w->ym[o]);
      for(k=0;k<i;k++) s-=a[(size_t)i*d+k]*b[(size_t)k*q+o];
      b[(size_t)i*q+o]=s/a[(size_t)i*d+i];
   }
   for(i=d-1;i>=0;i--) for(o=0;o<q;o++){
      double s;if(penalties && penalties[o]!=lambda)continue;s=b[(size_t)i*q+o];
      for(k=i+1;k<d;k++) s-=a[(size_t)k*d+i]*b[(size_t)k*q+o];
      b[(size_t)i*q+o]=s/a[(size_t)i*d+i];
   }
   for(j=0;j<p;j++) for(o=0;o<q;o++){
      double s=0;if(penalties && penalties[o]!=lambda)continue;
      if(dual) for(i=0;i<n;i++) s+=(x[(size_t)i*p+j]-w->xm[j])*b[(size_t)i*q+o];
      else s=b[(size_t)j*q+o];
      if(!isfinite(s)) return pp_error(err,nerr,"ridge coefficient is not finite");
      w->coef[(size_t)j*q+o]=s;
   }
   } /* distinct response penalties */
   w->n=n;w->p=p;w->q=q;w->fitted=1;return 1;
}

int THD_ridge_fit(THD_ridge_workspace *w,const float *x,const float *y,
 int n,int p,int q,double lambda,int form,char *err,int nerr)
{return ridge_fit_penalties(w,x,y,n,p,q,lambda,NULL,form,err,nerr);}
int THD_ridge_fit_response_lambda(THD_ridge_workspace *w,const float *x,const float *y,
 int n,int p,int q,const double *lambda,int form,char *err,int nerr)
{
   if(!lambda){if(w)w->fitted=0;return pp_error(err,nerr,"response ridge needs a penalty vector");}
   return ridge_fit_penalties(w,x,y,n,p,q,1,lambda,form,err,nerr);
}

int THD_ridge_coefficients(const THD_ridge_workspace *w,double *coef,
 double *intercept,char *err,int nerr)
{
   int j,o;
   if(!w || !w->fitted || !coef || !intercept)
      return pp_error(err,nerr,"ridge coefficient export needs a fitted model and output buffers");
   for(o=0;o<w->q;o++){
      double v=w->ym[o];
      for(j=0;j<w->p;j++)v-=w->xm[j]*w->coef[(size_t)j*w->q+o];
      if(!isfinite(v))return pp_error(err,nerr,"ridge intercept is not finite");
      intercept[o]=v;
   }
   memcpy(coef,w->coef,sizeof(double)*(size_t)w->p*w->q);
   return 1;
}

int THD_ridge_predict(const THD_ridge_workspace *w,const float *x,int n,
                      float *out,char *err,int nerr)
{
   int i,j,o;
   if(!w || !w->fitted || !x || !out || n<1) return pp_error(err,nerr,"ridge prediction needs a fitted model and rows");
   for(i=0;i<n;i++) for(o=0;o<w->q;o++){
      double s=w->ym[o];
      for(j=0;j<w->p;j++){
         double v=x[(size_t)i*w->p+j];
         if(!isfinite(v)) return pp_error(err,nerr,"ridge prediction input is not finite");
         s+=(v-w->xm[j])*w->coef[(size_t)j*w->q+o];
      }
      if(!isfinite(s) || fabs(s)>FLT_MAX) return pp_error(err,nerr,"ridge prediction exceeds float range");
      out[(size_t)i*w->q+o]=(float)s;
   }return 1;
}

/* Reusable continuous estimators. Spatial extraction and fold construction
   remain in their existing callers; no model sees rows outside its fit. */
struct THD_regression {
   THD_regression_config config;
   int maxn,maxp,maxq,n,p,q,fitted;
   double *parameter,*penalty;int *group;
   double *coef,*intercept,*xm,*ym,*xx,*yy,*residual,*norm,*dual;
   double *weight,*old,*score,*loading,*response,*rotation,*loadings;
   THD_ridge_workspace *linear;
   THD_regression_diagnostic *diagnostic;
};
void THD_regression_free(THD_regression *m)
{
   if(!m)return;
   free(m->parameter);free(m->penalty);free(m->group);free(m->coef);free(m->intercept);
   free(m->xm);free(m->ym);free(m->xx);free(m->yy);free(m->residual);free(m->norm);free(m->dual);
   free(m->weight);free(m->old);free(m->score);free(m->loading);free(m->response);
   free(m->rotation);free(m->loadings);free(m->diagnostic);THD_ridge_free(m->linear);free(m);
}
THD_regression *THD_regression_alloc(int n,int p,int q,
 const THD_regression_config *cfg,char *err,int nerr)
{
   THD_regression *m=NULL;int i,j,d=n<p?n:p;
   if(!cfg || n<2 || p<1 || q<1 || cfg->method<1 || cfg->method>4 || cfg->ncandidate<1 ||
      !cfg->parameter || !isfinite(cfg->tolerance) || cfg->tolerance<1e-12 || cfg->tolerance>1e-2 || cfg->max_iterations<1){
      pp_error(err,nerr,"invalid continuous estimator configuration/capacity");return NULL;}
   if((size_t)n>SIZE_MAX/sizeof(double)/(size_t)(p>q?p:q) ||
      (size_t)p>SIZE_MAX/sizeof(double)/(size_t)q ||
      (size_t)cfg->ncandidate>SIZE_MAX/sizeof(double)/2){pp_error(err,nerr,"continuous estimator too large");return NULL;}
   for(i=0;i<cfg->ncandidate;i++){
      double a=cfg->parameter[2*i],b=cfg->parameter[2*i+1];
      if(!isfinite(a) || !isfinite(b) || a<=0 ||
         (cfg->method==THD_REG_ELASTIC_NET && (b<0 || b>1)) ||
         (cfg->method==THD_REG_SVR && b<0) ||
         (cfg->method==THD_REG_PLS && (a!=floor(a) || a>INT_MAX))){
         pp_error(err,nerr,"invalid regression candidate parameters");return NULL;}
   }
   if(cfg->method==THD_REG_BANDED_RIDGE){
      if(cfg->nfeature!=p || cfg->ngroup<1 || cfg->ngroup>p || !cfg->feature_group || !cfg->group_penalty ||
         (size_t)cfg->ncandidate>SIZE_MAX/sizeof(double)/(size_t)cfg->ngroup){
         pp_error(err,nerr,"banded ridge needs complete design-column groups and penalty vectors");return NULL;}
      for(j=0;j<p;j++)if(cfg->feature_group[j]<0 || cfg->feature_group[j]>=cfg->ngroup){pp_error(err,nerr,"invalid predictor group ID");return NULL;}
      for(i=0;i<cfg->ngroup;i++){
         for(j=0;j<p && cfg->feature_group[j]!=i;j++);
         if(j==p){pp_error(err,nerr,"empty predictor group");return NULL;}
      }
      for(i=0;i<cfg->ncandidate;i++)for(j=0;j<cfg->ngroup;j++)if(!isfinite(cfg->group_penalty[(size_t)i*cfg->ngroup+j]) || cfg->group_penalty[(size_t)i*cfg->ngroup+j]<=0){pp_error(err,nerr,"group penalties must be finite and positive");return NULL;}
   }
   m=calloc(1,sizeof(*m));if(!m)goto fail;
   m->config=*cfg;m->maxn=n;m->maxp=p;m->maxq=q;
#define REG_ALLOC(field,count) do{m->field=calloc((size_t)(count),sizeof(*m->field));if(!m->field)goto fail;}while(0)
   REG_ALLOC(parameter,(size_t)cfg->ncandidate*2);memcpy(m->parameter,cfg->parameter,sizeof(double)*(size_t)cfg->ncandidate*2);m->config.parameter=m->parameter;
   if(cfg->method==THD_REG_BANDED_RIDGE){
      REG_ALLOC(group,p);REG_ALLOC(penalty,(size_t)cfg->ncandidate*cfg->ngroup);
      memcpy(m->group,cfg->feature_group,sizeof(int)*(size_t)p);
      memcpy(m->penalty,cfg->group_penalty,sizeof(double)*(size_t)cfg->ncandidate*cfg->ngroup);
      m->config.feature_group=m->group;m->config.group_penalty=m->penalty;
      m->linear=THD_ridge_alloc(n,p,q,err,nerr);if(!m->linear)goto fail;
   }
   REG_ALLOC(coef,(size_t)p*q);REG_ALLOC(intercept,q);REG_ALLOC(xm,p);REG_ALLOC(ym,q);
   REG_ALLOC(xx,(size_t)n*p);REG_ALLOC(yy,(size_t)n*q);REG_ALLOC(diagnostic,q);
   REG_ALLOC(residual,n);REG_ALLOC(norm,p>n?p:n);REG_ALLOC(dual,n);
   if(cfg->method==THD_REG_PLS){
      REG_ALLOC(weight,p);REG_ALLOC(old,p);REG_ALLOC(score,n);REG_ALLOC(loading,p);REG_ALLOC(response,q);
      REG_ALLOC(rotation,(size_t)d*p);REG_ALLOC(loadings,(size_t)d*p);
   }
#undef REG_ALLOC
   return m;
fail:THD_regression_free(m);pp_error(err,nerr,"out of memory allocating continuous estimator");return NULL;
}
static double reg_soft(double v,double threshold)
{return v>threshold?v-threshold:v< -threshold?v+threshold:0;}
static double reg_kkt(double gradient,double beta,double l1)
{return beta>0?fabs(gradient+l1):beta<0?fabs(gradient-l1):fmax(0,fabs(gradient)-l1);}
static int reg_elastic(THD_regression *m,int candidate,char *err,int nerr)
{
   int n=m->n,p=m->p,q=m->q,i,j,o,it;double a=m->parameter[2*candidate],rho=m->parameter[2*candidate+1];
   double l1=a*rho,l2=a*(1-rho),tol=m->config.tolerance;
   for(j=0;j<p;j++){double sum=0;for(i=0;i<n;i++)sum+=m->xx[(size_t)i*p+j]*m->xx[(size_t)i*p+j];m->norm[j]=sum/n;}
   for(o=0;o<q;o++){
      THD_regression_diagnostic *dg=m->diagnostic+o;
      for(i=0;i<n;i++)m->residual[i]=m->yy[(size_t)i*q+o];
      for(it=1;it<=m->config.max_iterations;it++){
         for(j=0;j<p;j++){
            double old=m->coef[(size_t)j*q+o],v=0,b,den=m->norm[j]+l2;
            for(i=0;i<n;i++)v+=m->xx[(size_t)i*p+j]*m->residual[i];
            b=den>0?reg_soft(v/n+m->norm[j]*old,l1)/den:0;
            m->coef[(size_t)j*q+o]=b;
            for(i=0;i<n;i++)m->residual[i]-=m->xx[(size_t)i*p+j]*(b-old);
         }
         /* Recompute residual and every KKT component together; small updates
            alone are insufficient on nearly collinear designs. */
         for(i=0;i<n;i++){double r=m->yy[(size_t)i*q+o];for(j=0;j<p;j++)r-=m->xx[(size_t)i*p+j]*m->coef[(size_t)j*q+o];m->residual[i]=r;}
         dg->residual=0;
         for(j=0;j<p;j++){
            double g=l2*m->coef[(size_t)j*q+o];
            for(i=0;i<n;i++)g-=m->xx[(size_t)i*p+j]*m->residual[i]/n;
            dg->residual=fmax(dg->residual,reg_kkt(g,m->coef[(size_t)j*q+o],l1));
         }
         if(!isfinite(dg->residual))return pp_error(err,nerr,"elastic-net nonfinite KKT residual");
         if(dg->residual<=tol)break;
      }
      if(it>m->config.max_iterations)return pp_error(err,nerr,"elastic-net did not converge: response %d KKT %.9g exceeds %.9g after %d sweeps",o+1,dg->residual,tol,m->config.max_iterations);
      dg->iterations=it;
      for(i=0;i<n;i++)dg->objective+=m->residual[i]*m->residual[i]/(2*n);
      for(j=0;j<p;j++){double b=m->coef[(size_t)j*q+o];dg->objective+=l1*fabs(b)+l2*b*b/2;dg->nonzero+=b!=0;}
   }
   return 1;
}
static int reg_banded(THD_regression *m,int candidate,char *err,int nerr)
{
   int n=m->n,p=m->p,q=m->q,dual=p>n,d=dual?n:p,i,j,k,o;
   double *a=m->linear->factor,*b=m->linear->rhs,*pen=m->penalty+(size_t)candidate*m->config.ngroup;
   for(i=0;i<d;i++)for(j=0;j<=i;j++){
      double sum=0;
      for(k=0;k<(dual?p:n);k++)sum+=dual?
         m->xx[(size_t)i*p+k]*m->xx[(size_t)j*p+k]/pen[m->group[k]]:
         m->xx[(size_t)k*p+i]*m->xx[(size_t)k*p+j];
      a[(size_t)i*d+j]=sum+(i==j?(dual?1:pen[m->group[i]]):0);
   }
   for(i=0;i<d;i++)for(o=0;o<q;o++){
      double sum=0;
      if(dual)sum=m->yy[(size_t)i*q+o];
      else for(k=0;k<n;k++)sum+=m->xx[(size_t)k*p+i]*m->yy[(size_t)k*q+o];
      b[(size_t)i*q+o]=sum;
   }
   if(!lda_solve(a,b,d,q,err,nerr))return 0;
   for(j=0;j<p;j++)for(o=0;o<q;o++){
      double v=0;
      if(dual){for(i=0;i<n;i++)v+=m->xx[(size_t)i*p+j]*b[(size_t)i*q+o];v/=pen[m->group[j]];}
      else v=b[(size_t)j*q+o];
      m->coef[(size_t)j*q+o]=v;
   }
   for(o=0;o<q;o++){
      THD_regression_diagnostic *dg=m->diagnostic+o;dg->iterations=1;
      for(i=0;i<n;i++){double r=m->yy[(size_t)i*q+o];for(j=0;j<p;j++)r-=m->xx[(size_t)i*p+j]*m->coef[(size_t)j*q+o];m->residual[i]=r;dg->objective+=r*r;}
      for(j=0;j<p;j++){
         double beta=m->coef[(size_t)j*q+o],g=pen[m->group[j]]*beta;
         for(i=0;i<n;i++)g-=m->xx[(size_t)i*p+j]*m->residual[i];
         dg->residual=fmax(dg->residual,fabs(g));dg->objective+=pen[m->group[j]]*beta*beta;dg->nonzero+=beta!=0;
      }
   }
   return 1;
}
static int reg_svr(THD_regression *m,int candidate,char *err,int nerr)
{
   int n=m->n,p=m->p,q=m->q,i,j,o,it;double c=m->parameter[2*candidate],eps=m->parameter[2*candidate+1],diag=1/(2*c);
   for(i=0;i<n;i++){double v=1+diag;for(j=0;j<p;j++)v+=m->xx[(size_t)i*p+j]*m->xx[(size_t)i*p+j];m->norm[i]=v;}
   for(o=0;o<q;o++){
      THD_regression_diagnostic *dg=m->diagnostic+o;
      memset(m->dual,0,sizeof(double)*(size_t)n);m->intercept[o]=0;
      for(it=1;it<=m->config.max_iterations;it++){
         for(i=0;i<n;i++){
            double pred=m->intercept[o],g,old=m->dual[i],v,delta;
            for(j=0;j<p;j++)pred+=m->xx[(size_t)i*p+j]*m->coef[(size_t)j*q+o];
            g=pred-m->yy[(size_t)i*q+o]+diag*old;
            v=reg_soft(old-g/m->norm[i],eps/m->norm[i]);delta=v-old;m->dual[i]=v;
            m->intercept[o]+=delta;
            for(j=0;j<p;j++)m->coef[(size_t)j*q+o]+=delta*m->xx[(size_t)i*p+j];
         }
         /* Reconstruct primal variables to prevent coordinate-update drift. */
         m->intercept[o]=0;for(j=0;j<p;j++)m->coef[(size_t)j*q+o]=0;
         for(i=0;i<n;i++){m->intercept[o]+=m->dual[i];for(j=0;j<p;j++)m->coef[(size_t)j*q+o]+=m->dual[i]*m->xx[(size_t)i*p+j];}
         dg->residual=0;
         for(i=0;i<n;i++){
            double g=m->intercept[o]-m->yy[(size_t)i*q+o]+diag*m->dual[i];
            for(j=0;j<p;j++)g+=m->xx[(size_t)i*p+j]*m->coef[(size_t)j*q+o];
            dg->residual=fmax(dg->residual,reg_kkt(g,m->dual[i],eps));
         }
         if(!isfinite(dg->residual))return pp_error(err,nerr,"SVR nonfinite dual KKT residual");
         if(dg->residual<=m->config.tolerance)break;
      }
      if(it>m->config.max_iterations)return pp_error(err,nerr,"SVR did not converge: response %d dual KKT %.9g exceeds %.9g after %d sweeps",o+1,dg->residual,m->config.tolerance,m->config.max_iterations);
      dg->iterations=it;dg->objective=m->intercept[o]*m->intercept[o]/2;
      for(j=0;j<p;j++){double b=m->coef[(size_t)j*q+o];dg->objective+=b*b/2;dg->nonzero+=b!=0;}
      for(i=0;i<n;i++){double r=m->yy[(size_t)i*q+o]-m->intercept[o];for(j=0;j<p;j++)r-=m->xx[(size_t)i*p+j]*m->coef[(size_t)j*q+o];r=fmax(0,fabs(r)-eps);dg->objective+=c*r*r;}
   }
   return 1;
}
static int reg_pls(THD_regression *m,int candidate,char *err,int nerr)
{
   int n=m->n,p=m->p,q=m->q,limit=(int)m->parameter[2*candidate],i,j,o,k,h,it,used=0,maxit=0;
   double initial=0,last=0,cutoff;
   if(limit>p)limit=p;
   if(limit>n-1)limit=n-1;
   for(i=0;i<n;i++)for(j=0;j<p;j++)initial+=m->xx[(size_t)i*p+j]*m->xx[(size_t)i*p+j];
   cutoff=initial*64*DBL_EPSILON*(n>p?n:p);
   for(k=0;k<limit;k++){
      int col=0;double largest=0,tt=0;
      for(o=0;o<q;o++){double sum=0;for(i=0;i<n;i++)sum+=m->yy[(size_t)i*q+o]*m->yy[(size_t)i*q+o];if(sum>largest){largest=sum;col=o;}}
      if(largest<=DBL_MIN)break;
      for(i=0;i<n;i++)m->residual[i]=m->yy[(size_t)i*q+col];
      memset(m->old,0,sizeof(double)*(size_t)p);
      for(it=1;it<=m->config.max_iterations;it++){
         double norm=0,cc=0,diff=0;
         for(j=0;j<p;j++){double sum=0;for(i=0;i<n;i++)sum+=m->xx[(size_t)i*p+j]*m->residual[i];m->weight[j]=sum;norm+=sum*sum;}
         if(norm<=DBL_MIN){tt=0;break;}
         norm=sqrt(norm);for(j=0;j<p;j++)m->weight[j]/=norm;
         /* Fix each iterate's sign, including repeated singular-value cases. */
         {int pivot=0;for(j=1;j<p;j++)if(fabs(m->weight[j])>fabs(m->weight[pivot]))pivot=j;
          if(m->weight[pivot]<0)for(j=0;j<p;j++)m->weight[j]=-m->weight[j];}
         tt=0;for(i=0;i<n;i++){double sum=0;for(j=0;j<p;j++)sum+=m->xx[(size_t)i*p+j]*m->weight[j];m->score[i]=sum;tt+=sum*sum;}
         if(tt<=cutoff)break;
         for(o=0;o<q;o++){double sum=0;for(i=0;i<n;i++)sum+=m->yy[(size_t)i*q+o]*m->score[i];m->response[o]=sum/tt;cc+=m->response[o]*m->response[o];}
         if(cc<=DBL_MIN){tt=0;break;}
         for(j=0;j<p;j++){double d=m->weight[j]-m->old[j];diff+=d*d;m->old[j]=m->weight[j];}
         last=sqrt(diff);
         if(q==1 || last<=m->config.tolerance){if(q==1)last=0;break;}
         for(i=0;i<n;i++){double sum=0;for(o=0;o<q;o++)sum+=m->yy[(size_t)i*q+o]*m->response[o];m->residual[i]=sum/cc;}
      }
      if(it>m->config.max_iterations)return pp_error(err,nerr,"PLS power iteration did not converge at component %d (change %.9g)",k+1,last);
      if(tt<=cutoff)break;
      if(it>maxit)maxit=it;
      for(j=0;j<p;j++){
         double sum=0;for(i=0;i<n;i++)sum+=m->xx[(size_t)i*p+j]*m->score[i];
         m->loading[j]=sum/tt;m->loadings[(size_t)k*p+j]=m->loading[j];m->rotation[(size_t)k*p+j]=m->weight[j];
      }
      for(h=0;h<k;h++){
         double cross=0;for(j=0;j<p;j++)cross+=m->loadings[(size_t)h*p+j]*m->weight[j];
         for(j=0;j<p;j++)m->rotation[(size_t)k*p+j]-=m->rotation[(size_t)h*p+j]*cross;
      }
      for(j=0;j<p;j++)for(o=0;o<q;o++)m->coef[(size_t)j*q+o]+=m->rotation[(size_t)k*p+j]*m->response[o];
      for(i=0;i<n;i++){
         for(j=0;j<p;j++)m->xx[(size_t)i*p+j]-=m->score[i]*m->loading[j];
         for(o=0;o<q;o++)m->yy[(size_t)i*q+o]-=m->score[i]*m->response[o];
      }
      used++;
   }
   for(o=0;o<q;o++){
      THD_regression_diagnostic *dg=m->diagnostic+o;dg->components=used;dg->iterations=maxit;dg->residual=used?last:0;
      for(i=0;i<n;i++)dg->objective+=m->yy[(size_t)i*q+o]*m->yy[(size_t)i*q+o]/n;
      for(j=0;j<p;j++)dg->nonzero+=m->coef[(size_t)j*q+o]!=0;
   }
   return 1;
}
int THD_regression_fit(THD_regression *m,const float *x,const float *y,
 int n,int p,int q,int candidate,char *err,int nerr)
{
   int i,j,o,ok=0,center;
   if(m)m->fitted=0;
   if(!m || !x || !y || n<2 || n>m->maxn || p<1 || p>m->maxp || q<1 || q>m->maxq || candidate<0 || candidate>=m->config.ncandidate ||
      (m->config.method==THD_REG_BANDED_RIDGE && p!=m->config.nfeature))return pp_error(err,nerr,"invalid continuous estimator fit dimensions/candidate");
   m->n=n;m->p=p;m->q=q;center=m->config.method!=THD_REG_SVR;
   memset(m->coef,0,sizeof(double)*(size_t)p*q);memset(m->diagnostic,0,sizeof(*m->diagnostic)*(size_t)q);
   for(j=0;j<p;j++){
      double sum=0;for(i=0;i<n;i++){double v=x[(size_t)i*p+j];if(!isfinite(v))return pp_error(err,nerr,"nonfinite regression predictor");sum+=v;}
      m->xm[j]=center?sum/n:0;
      for(i=0;i<n;i++)m->xx[(size_t)i*p+j]=x[(size_t)i*p+j]-m->xm[j];
   }
   for(o=0;o<q;o++){
      double sum=0;for(i=0;i<n;i++){double v=y[(size_t)i*q+o];if(!isfinite(v))return pp_error(err,nerr,"nonfinite regression response");sum+=v;}
      m->ym[o]=sum/n;m->intercept[o]=center?m->ym[o]:0;
      for(i=0;i<n;i++)m->yy[(size_t)i*q+o]=y[(size_t)i*q+o]-(center?m->ym[o]:0);
   }
   switch(m->config.method){
      case THD_REG_ELASTIC_NET:ok=reg_elastic(m,candidate,err,nerr);break;
      case THD_REG_BANDED_RIDGE:ok=reg_banded(m,candidate,err,nerr);break;
      case THD_REG_PLS:ok=reg_pls(m,candidate,err,nerr);break;
      case THD_REG_SVR:ok=reg_svr(m,candidate,err,nerr);break;
   }
   if(!ok)return 0;
   for(o=0;o<q;o++){
      for(j=0;j<p;j++){
         double b=m->coef[(size_t)j*q+o];if(!isfinite(b))return pp_error(err,nerr,"nonfinite regression coefficient");
         m->intercept[o]-=m->xm[j]*b;
      }
      if(!isfinite(m->intercept[o]) || !isfinite(m->diagnostic[o].objective) || !isfinite(m->diagnostic[o].residual))return pp_error(err,nerr,"nonfinite regression fit diagnostics");
   }
   m->fitted=1;return 1;
}
int THD_regression_predict(const THD_regression *m,const float *x,int n,float *out,char *err,int nerr)
{
   int i,j,o;
   if(!m || !m->fitted || !x || !out || n<1)return pp_error(err,nerr,"regression prediction requires a fitted model");
   for(i=0;i<n;i++)for(o=0;o<m->q;o++){
      double v=m->intercept[o];
      for(j=0;j<m->p;j++){double z=x[(size_t)i*m->p+j];if(!isfinite(z))return pp_error(err,nerr,"nonfinite regression prediction input");v+=z*m->coef[(size_t)j*m->q+o];}
      if(!isfinite(v) || fabs(v)>FLT_MAX)return pp_error(err,nerr,"regression prediction exceeds float range");
      out[(size_t)i*m->q+o]=(float)v;
   }return 1;
}
int THD_regression_coefficients(const THD_regression *m,double *coef,double *intercept,char *err,int nerr)
{
   if(!m || !m->fitted || !coef || !intercept)return pp_error(err,nerr,"regression coefficients require a fitted model");
   memcpy(coef,m->coef,sizeof(double)*(size_t)m->p*m->q);memcpy(intercept,m->intercept,sizeof(double)*(size_t)m->q);return 1;
}
const THD_regression_diagnostic *THD_regression_diagnostics(const THD_regression *m)
{return m && m->fitted?m->diagnostic:NULL;}

const char *THD_mvpa_tune_objective_name(int objective)
{
   static const char *names[]={"default","normalized_mse","accuracy","balanced_accuracy","MSE","RMSE","MAE","log_loss","brier"};
   return objective>=0 && objective<=THD_TUNE_BRIER?names[objective]:NULL;
}
int THD_mvpa_tune_objective_parse(const char *name,int *objective,char *err,int nerr)
{
   int i;
   if(!name || !objective)return pp_error(err,nerr,"tuning objective needs a name and output");
   if(!strcasecmp(name,"ACC")){*objective=THD_TUNE_ACCURACY;return 1;}
   if(!strcasecmp(name,"BACC")){*objective=THD_TUNE_BALANCED_ACCURACY;return 1;}
   for(i=0;i<=THD_TUNE_BRIER;i++)if(!strcasecmp(name,THD_mvpa_tune_objective_name(i))){*objective=i;return 1;}
   return pp_error(err,nerr,"unknown tuning objective '%s'; use default, normalized_mse, ACC, BACC, MSE, RMSE, MAE, log_loss, or brier (probability losses require logistic)",name);
}
int THD_mvpa_tune_objective_resolve(int requested,int classification,int ridge,
 int score_metric,int *objective,char *err,int nerr)
{
   int value=requested;
   if(!objective || !THD_mvpa_tune_objective_name(requested))return pp_error(err,nerr,"invalid tuning objective");
   if(value==THD_TUNE_DEFAULT)value=ridge?THD_TUNE_NORMALIZED_MSE:
      score_metric==THD_METRIC_ACCURACY?THD_TUNE_ACCURACY:THD_TUNE_BALANCED_ACCURACY;
   if(classification){
      if(value!=THD_TUNE_ACCURACY && value!=THD_TUNE_BALANCED_ACCURACY && !(ridge && value==THD_TUNE_NORMALIZED_MSE))
         return pp_error(err,nerr,"classification tuning needs ACC/BACC, or normalized_mse for ridge; probability losses require logistic");
   }else if(!ridge || (value!=THD_TUNE_NORMALIZED_MSE && value!=THD_TUNE_MSE && value!=THD_TUNE_RMSE && value!=THD_TUNE_MAE))
      return pp_error(err,nerr,"continuous tuning needs normalized_mse, MSE, RMSE, or MAE; probability losses require logistic classification");
   *objective=value;return 1;
}
int THD_mvpa_ridge_set_objective(THD_mvpa_ridge_workspace *w,int objective,char *err,int nerr)
{
   int value,*conf=NULL;
   if(!w || !w->nested)return pp_error(err,nerr,"tuning objective requires configured inner CV");
   if(!THD_mvpa_tune_objective_resolve(objective,w->classification,1,0,&value,err,nerr))return 0;
   if(value==THD_TUNE_ACCURACY || value==THD_TUNE_BALANCED_ACCURACY){
      if((size_t)w->max_output>SIZE_MAX/sizeof(int)/(size_t)w->max_output/(size_t)w->nlambda)
         return pp_error(err,nerr,"ridge tuning confusion dimensions too large");
      conf=calloc((size_t)w->nlambda*w->max_output*w->max_output,sizeof(int));
      if(!conf)return pp_error(err,nerr,"cannot allocate ridge tuning confusion tables");
   }
   free(w->inner_confusion);w->inner_confusion=conf;w->tune_objective=value;return 1;
}
int THD_mvpa_logistic_objective(int requested,int *objective,char *err,int nerr)
{
   if(!objective)return pp_error(err,nerr,"logistic objective needs an output");
   if(requested==THD_TUNE_DEFAULT)requested=THD_TUNE_LOG_LOSS;
   if(requested!=THD_TUNE_LOG_LOSS && requested!=THD_TUNE_BRIER &&
      requested!=THD_TUNE_ACCURACY && requested!=THD_TUNE_BALANCED_ACCURACY)
      return pp_error(err,nerr,"logistic tuning needs log_loss, brier, ACC, or BACC");
   *objective=requested;return 1;
}
int THD_mvpa_classification_set_objective(THD_mvpa_workspace *w,int objective,char *err,int nerr)
{
   int value,metric;
   if(!w)return pp_error(err,nerr,"classification objective needs a workspace");
   if(w->ridge_cv)return THD_mvpa_ridge_set_objective(w->ridge_cv,objective,err,nerr);
   if(!w->class_nested && !(w->selection && w->selection->ngrid>1) && !(w->pca && w->pca->nested))
      return pp_error(err,nerr,"tuning objective requires configured inner CV");
   metric=w->pca?w->pca->metric:w->selection?w->selection->metric:w->class_metric;
   if(w->logistic){if(!THD_mvpa_logistic_objective(objective,&value,err,nerr))return 0;}
   else if(!THD_mvpa_tune_objective_resolve(objective,1,0,metric,&value,err,nerr))return 0;
   w->tune_objective=value;return 1;
}
static int tune_class_loss(const int *conf,int nc,int objective,double *loss,int *nvalid,char *err,int nerr)
{
   int i,j,total=0,correct=0,present=0;double recall=0;
   for(i=0;i<nc;i++){
      int count=0;for(j=0;j<nc;j++)count+=conf[(size_t)i*nc+j];
      total+=count;correct+=conf[(size_t)i*nc+i];
      if(count){recall+=(double)conf[(size_t)i*nc+i]/count;present++;}
   }
   if(!total || !present)return pp_error(err,nerr,"classification inner objective is undefined");
   *loss=1-(objective==THD_TUNE_ACCURACY?(double)correct/total:recall/present);
   *nvalid=present;return 1;
}
static int tune_continuous_loss(const double *error,const double *baseline,int q,
 int ntest,int objective,double *loss,int *nvalid,char *err,int nerr)
{
   int o,valid=0;double sum=0;
   if(ntest<1)return pp_error(err,nerr,"continuous inner objective has no test rows");
   for(o=0;o<q;o++){
      double value;
      if(objective==THD_TUNE_NORMALIZED_MSE){
         if(baseline[o]<=0)continue;
         value=error[o]/baseline[o];
      }else{
         value=error[o]/ntest;
         if(objective==THD_TUNE_RMSE)value=sqrt(value);
      }
      if(!isfinite(value))return pp_error(err,nerr,"continuous inner objective is nonfinite");
      sum+=value;valid++;
   }
   *loss=valid?sum/valid:NAN;*nvalid=valid;return 1;
}

/* Reusable fold-local supervised feature selection. No allocations in fits. */
typedef struct { double score; int index; } FS_rank;
static int fs_compare(const void *aa,const void *bb)
{
   const FS_rank *a=aa,*b=bb;
   if(a->score>b->score)return -1;
   if(a->score<b->score)return 1;
   return (a->index>b->index)-(a->index<b->index);
}
/* Worst-first heap produces descending score order without libc qsort's
   implementation-dependent scratch allocations on large patterns. */
static void fs_sift(FS_rank *rank,int root,int n)
{
   while(root<n/2){
      int child=2*root+1;FS_rank tmp;
      if(child+1<n && fs_compare(rank+child+1,rank+child)>0)child++;
      if(fs_compare(rank+root,rank+child)>=0)break;
      tmp=rank[root];rank[root]=rank[child];rank[child]=tmp;root=child;
   }
}
static void fs_sort(FS_rank *rank,int n)
{
   int i;FS_rank tmp;
   for(i=n/2;i>0;i--)fs_sift(rank,i-1,n);
   for(i=n-1;i>0;i--){
      tmp=rank[0];rank[0]=rank[i];rank[i]=tmp;fs_sift(rank,0,i);
   }
}
static void fs_free(THD_feature_selection *s)
{
   if(!s)return;
   free(s->grid);free(s->chosen);free(s->count);free(s->indices);free(s->scores);
   free(s->loss);free(s->rank);free(s->row);free(s->sums);free(s->counts);
   free(s->train);free(s->test);free(s);
}
static THD_feature_selection *fs_alloc(const THD_foldplan *plan,int p,int nc,
 const int *grid,int nk,int nl,const THD_nested_foldplan *nested,int metric,char *err,int nerr)
{
   THD_feature_selection *s;int i,j;
   if(!grid || nk<1 || nl<1 || p<1 || nc<1 || (nk>1 && !nested)){
      pp_error(err,nerr,"feature selection needs positive counts and grouped inner folds for a count grid");return NULL;
   }
   for(i=0;i<nk;i++){
      if(grid[i]<1){pp_error(err,nerr,"feature counts must be positive integers");return NULL;}
      for(j=0;j<i;j++)if(grid[i]==grid[j]){pp_error(err,nerr,"duplicate feature count");return NULL;}
   }
   if(nested){
      if(nested->outer!=plan || !nested->inner){pp_error(err,nerr,"feature selection nested plan mismatch");return NULL;}
      for(i=0;i<plan->nfold;i++){
         const THD_foldplan *in=nested->inner[i];
         if(!THD_foldplan_validate(in,err,nerr))return NULL;
         if(in->nsample!=plan->ntrain[i] || in->nfold<2 || in->kind==THD_CV_FIXED){
            pp_error(err,nerr,"feature count tuning needs grouped inner folds over outer training rows");return NULL;
         }
      }
   }
   if((size_t)nk>SIZE_MAX/sizeof(double)/(size_t)nl/(size_t)plan->nfold ||
      (size_t)p>SIZE_MAX/sizeof(double)/(size_t)plan->nfold){
      pp_error(err,nerr,"feature selection dimensions too large");return NULL;
   }
   s=calloc(1,sizeof(*s));if(!s){pp_error(err,nerr,"cannot allocate feature selection");return NULL;}
   s->max_feature=p;s->ngrid=nk;s->npenalty=nl;s->nested=nested;s->metric=metric;s->nclass=nc;
   s->grid=malloc(sizeof(int)*(size_t)nk);s->chosen=calloc((size_t)plan->nfold,sizeof(int));
   s->count=calloc((size_t)plan->nfold,sizeof(int));
   s->indices=malloc(sizeof(int)*(size_t)plan->nfold*p);s->scores=malloc(sizeof(double)*(size_t)plan->nfold*p);
   s->loss=malloc(sizeof(double)*(size_t)plan->nfold*nk*nl);
   s->rank=malloc(sizeof(FS_rank)*(size_t)p);s->row=malloc(sizeof(float)*(size_t)p);
   s->sums=malloc(sizeof(double)*(size_t)nc);s->counts=malloc(sizeof(int)*(size_t)nc);
   s->train=malloc(sizeof(int)*(size_t)plan->nsample);s->test=malloc(sizeof(int)*(size_t)plan->nsample);
   if(!s->grid || !s->chosen || !s->count || !s->indices || !s->scores || !s->loss ||
      !s->rank || !s->row || !s->sums || !s->counts || !s->train || !s->test){
      fs_free(s);pp_error(err,nerr,"cannot allocate feature selection buffers");return NULL;
   }
   memcpy(s->grid,grid,sizeof(int)*(size_t)nk);return s;
}
int THD_mvpa_ridge_set_selection(THD_mvpa_ridge_workspace *w,
 const int *counts,int nk,char *err,int nerr)
{
   if(!w || w->regression || w->selection || w->pca || w->response_ridge)return pp_error(err,nerr,"configure feature selection once after ridge tuning");
   w->selection=fs_alloc(w->plan,w->max_feature,w->max_output,counts,nk,
                        w->nested?w->nlambda:1,w->nested,THD_METRIC_RMSE,err,nerr);
   return w->selection!=NULL;
}
int THD_mvpa_classification_set_selection(THD_mvpa_workspace *w,
 const int *counts,int nk,const THD_nested_foldplan *nested,int metric,char *err,int nerr)
{
   if(!w || w->selection || w->pca || (metric!=THD_METRIC_ACCURACY && metric!=THD_METRIC_BALANCED_ACCURACY))
      return pp_error(err,nerr,"configure classification selection once with ACC/BACC");
   if(w->ridge_cv){
      if(nested!=w->ridge_cv->nested)return pp_error(err,nerr,"ridge selection must share penalty tuning folds");
      return THD_mvpa_ridge_set_selection(w->ridge_cv,counts,nk,err,nerr);
   }
   if(w->class_nested && nested!=w->class_nested)return pp_error(err,nerr,"feature and classifier penalty tuning must share inner folds");
   w->selection=fs_alloc(w->plan,w->nfeature_alloc,w->nclass,counts,nk,
                        w->class_nested?w->class_ngrid:1,nested,metric,err,nerr);
   return w->selection!=NULL;
}
static int fs_fit(THD_feature_selection *s,const float *x,int n,int p,
 const float *y,int q,const int *classes,char *err,int nerr)
{
   FS_rank *rank=s->rank;int j,i,o;
   if(n<2)return pp_error(err,nerr,"feature selection needs at least two training samples");
   if(classes){
      memset(s->counts,0,sizeof(int)*(size_t)s->nclass);
      for(i=0;i<n;i++){
         if(classes[i]<0 || classes[i]>=s->nclass)return pp_error(err,nerr,"invalid selection class");
         s->counts[classes[i]]++;
      }
      for(o=0;o<s->nclass;o++)if(!s->counts[o])return pp_error(err,nerr,"feature selection training split lacks a class");
   }
   for(j=0;j<p;j++){
      double mean=0,ss=0,score=0;int valid=0;
      for(i=0;i<n;i++)mean+=x[(size_t)i*p+j];mean/=n;
      for(i=0;i<n;i++){double v=x[(size_t)i*p+j]-mean;ss+=v*v;}
      if(ss>0 && classes){
         memset(s->sums,0,sizeof(double)*(size_t)s->nclass);
         for(i=0;i<n;i++)s->sums[classes[i]]+=x[(size_t)i*p+j]-mean;
         for(o=0;o<s->nclass;o++)score+=s->sums[o]*s->sums[o]/s->counts[o];
         score/=ss;
      }else if(ss>0){
         for(o=0;o<q;o++){
            double ym=0,ys=0,cov=0,r;
            for(i=0;i<n;i++)ym+=y[(size_t)i*q+o];ym/=n;
            for(i=0;i<n;i++){double v=y[(size_t)i*q+o]-ym;ys+=v*v;cov+=(x[(size_t)i*p+j]-mean)*v;}
            if(ys>0){r=cov/sqrt(ss)/sqrt(ys);score+=r*r;valid++;}
         }
         if(valid)score/=valid;
      }
      if(!isfinite(score))return pp_error(err,nerr,"nonfinite feature selection score");
      rank[j].score=fmax(0,fmin(1,score));rank[j].index=j;
   }
   fs_sort(rank,p);return 1;
}
static void fs_apply(THD_feature_selection *s,float *x,int n,int p,int k)
{
   FS_rank *rank=s->rank;int i,j;
   for(i=0;i<n;i++){
      for(j=0;j<k;j++)s->row[j]=x[(size_t)i*p+rank[j].index];
      memcpy(x+(size_t)i*k,s->row,sizeof(float)*(size_t)k);
   }
}
static int fs_count(THD_feature_selection *s,int candidate,int p)
{ return s?(s->grid[candidate]<p?s->grid[candidate]:p):p; }
static void fs_record(THD_feature_selection *s,int fold,int candidate,int p)
{
   FS_rank *rank=s->rank;int j,k=fs_count(s,candidate,p);
   s->chosen[fold]=candidate;s->count[fold]=k;
   for(j=0;j<k;j++){
      s->indices[(size_t)fold*s->max_feature+j]=rank[j].index;
      s->scores[(size_t)fold*s->max_feature+j]=rank[j].score;
   }
}

/* Centered PCA through the smaller feature/sample Gram matrix. A cyclic
   Jacobi eigensolver uses caller-owned scratch and explicitly checks convergence. */
void THD_pca_free(THD_pca_model *m)
{
   if(!m)return;
   free(m->mean);free(m->loading);free(m->variance);free(m->matrix);
   free(m->vectors);free(m->eigen);free(m->row);free(m);
}
THD_pca_model *THD_pca_alloc(int n,int p,int capacity,char *err,int nerr)
{
   THD_pca_model *m;int d=n<p?n:p,k=capacity<d?capacity:d;
   if(n<2 || p<1 || k<1 || (size_t)d>SIZE_MAX/sizeof(double)/(size_t)d ||
      (size_t)p>SIZE_MAX/sizeof(double)/(size_t)k){pp_error(err,nerr,"invalid or excessive PCA dimensions");return NULL;}
   m=calloc(1,sizeof(*m));if(!m){pp_error(err,nerr,"cannot allocate PCA");return NULL;}
   m->max_sample=n;m->max_feature=p;m->capacity=k;m->dimension=d;
   m->mean=malloc(sizeof(double)*(size_t)p);m->row=malloc(sizeof(double)*(size_t)p);
   m->loading=malloc(sizeof(double)*(size_t)p*k);m->variance=malloc(sizeof(double)*(size_t)k);
   m->matrix=malloc(sizeof(double)*(size_t)d*d);m->vectors=malloc(sizeof(double)*(size_t)d*d);
   m->eigen=malloc(sizeof(double)*(size_t)d);
   if(!m->mean || !m->row || !m->loading || !m->variance || !m->matrix || !m->vectors || !m->eigen){
      THD_pca_free(m);pp_error(err,nerr,"cannot allocate PCA buffers");return NULL;
   }return m;
}
static int mvpa_symmetric_eigen(double *a,double *v,double *eigen,int d,char *err,int nerr)
{
   int i,j,k,sweep,converged=0;
   memset(v,0,sizeof(double)*(size_t)d*d);for(i=0;i<d;i++)v[(size_t)i*d+i]=1;
   for(sweep=0;sweep<100;sweep++){
      double diag=0,off=0,tol;
      for(i=0;i<d;i++){
         diag=fmax(diag,fabs(a[(size_t)i*d+i]));
         for(j=i+1;j<d;j++)off=fmax(off,fabs(a[(size_t)i*d+j]));
      }
      tol=8*DBL_EPSILON*diag;
      if(off<=tol){converged=1;break;}
      for(i=0;i<d;i++)for(j=i+1;j<d;j++){
         double ab=a[(size_t)i*d+j],ai=a[(size_t)i*d+i],aj=a[(size_t)j*d+j];
         double tau,t,c,s;
         if(fabs(ab)<=tol)continue;
         tau=(aj-ai)/(2*ab);t=copysign(1.0,tau)/(fabs(tau)+hypot(1.0,tau));
         c=1/sqrt(1+t*t);s=t*c;
         a[(size_t)i*d+i]=ai-t*ab;a[(size_t)j*d+j]=aj+t*ab;
         a[(size_t)i*d+j]=a[(size_t)j*d+i]=0;
         for(k=0;k<d;k++){
            double vi=v[(size_t)k*d+i],vj=v[(size_t)k*d+j];
            v[(size_t)k*d+i]=c*vi-s*vj;v[(size_t)k*d+j]=s*vi+c*vj;
            if(k!=i && k!=j){
               double ki=a[(size_t)k*d+i],kj=a[(size_t)k*d+j];
               a[(size_t)k*d+i]=a[(size_t)i*d+k]=c*ki-s*kj;
               a[(size_t)k*d+j]=a[(size_t)j*d+k]=s*ki+c*kj;
            }
         }
      }
   }
   if(!converged)return pp_error(err,nerr,"symmetric eigensolver did not converge in 100 sweeps");
   for(i=0;i<d;i++)eigen[i]=a[(size_t)i*d+i];
   /* Stable descending eigenvalue order. */
   for(i=0;i<d-1;i++){
      int best=i;double tmp;
      for(j=i+1;j<d;j++)if(eigen[j]>eigen[best])best=j;
      if(best==i)continue;
      tmp=eigen[i];eigen[i]=eigen[best];eigen[best]=tmp;
      for(k=0;k<d;k++){tmp=v[(size_t)k*d+i];v[(size_t)k*d+i]=v[(size_t)k*d+best];v[(size_t)k*d+best]=tmp;}
   }return 1;
}
int THD_pca_fit(THD_pca_model *m,const float *x,int n,int p,int requested,char *err,int nerr)
{
   int i,j,k,d=n<p?n:p,dual=n<p;double cutoff;
   if(m)m->nfeature=0;
   if(!m || !x || n<2 || n>m->max_sample || p<1 || p>m->max_feature || requested<1)
      return pp_error(err,nerr,"invalid PCA fit dimensions/count");
   m->rank=m->count=0;m->total_variance=0;
   for(j=0;j<p;j++){
      double mean=0;
      for(i=0;i<n;i++){if(!isfinite(x[(size_t)i*p+j]))return pp_error(err,nerr,"nonfinite PCA input");mean+=x[(size_t)i*p+j];}
      m->mean[j]=mean/n;
   }
   for(i=0;i<d;i++)for(j=i;j<d;j++){
      double sum=0;
      if(dual)for(k=0;k<p;k++)sum+=(x[(size_t)i*p+k]-m->mean[k])*(x[(size_t)j*p+k]-m->mean[k]);
      else for(k=0;k<n;k++)sum+=(x[(size_t)k*p+i]-m->mean[i])*(x[(size_t)k*p+j]-m->mean[j]);
      if(!isfinite(sum))return pp_error(err,nerr,"nonfinite PCA Gram matrix");
      m->matrix[(size_t)i*d+j]=m->matrix[(size_t)j*d+i]=sum;
      if(i==j)m->total_variance+=sum/(n-1);
   }
   if(!mvpa_symmetric_eigen(m->matrix,m->vectors,m->eigen,d,err,nerr))return 0;
   cutoff=fmax(0,m->eigen[0])*64*DBL_EPSILON*(n>p?n:p);
   for(i=0;i<d && i<n-1;i++)if(m->eigen[i]>cutoff)m->rank++;
   m->count=requested<m->rank?requested:m->rank;
   if(m->count>m->capacity)return pp_error(err,nerr,"PCA requested count exceeds allocated capacity");
   for(k=0;k<m->count;k++){
      double norm=0;int pivot=0;
      for(j=0;j<p;j++){
         double v=0;
         if(dual){for(i=0;i<n;i++)v+=(x[(size_t)i*p+j]-m->mean[j])*m->vectors[(size_t)i*d+k];v/=sqrt(m->eigen[k]);}
         else v=m->vectors[(size_t)j*d+k];
         m->loading[(size_t)k*p+j]=v;norm+=v*v;
         if(fabs(v)>fabs(m->loading[(size_t)k*p+pivot]))pivot=j;
      }
      if(!(norm>0) || !isfinite(norm))return pp_error(err,nerr,"invalid PCA component norm");
      norm=copysign(sqrt(norm),m->loading[(size_t)k*p+pivot]);
      for(j=0;j<p;j++)m->loading[(size_t)k*p+j]/=norm;
      m->variance[k]=m->eigen[k]/(n-1);
   }m->nfeature=p;return 1;
}
int THD_pca_apply(THD_pca_model *m,float *x,int n,char *err,int nerr)
{
   int i,j,k,p,width;
   if(!m || !x || n<0 || m->nfeature<1)return pp_error(err,nerr,"PCA application needs a fitted projection");
   p=m->nfeature;width=m->count?m->count:1;
   for(i=0;i<n;i++){
      for(j=0;j<p;j++)m->row[j]=x[(size_t)i*p+j]-m->mean[j];
      if(!m->count)x[i]=0;
      for(k=0;k<m->count;k++){
         double v=0;for(j=0;j<p;j++)v+=m->row[j]*m->loading[(size_t)k*p+j];
         if(!isfinite(v) || fabs(v)>FLT_MAX)return pp_error(err,nerr,"nonfinite PCA projection");
         x[(size_t)i*width+k]=(float)v;
      }
   }return 1;
}
static void pca_free(THD_mvpa_pca *s)
{
   if(!s)return;THD_pca_free(s->model);
   free(s->grid);free(s->chosen);free(s->count);free(s->rank);free(s->input_count);
   free(s->variance);free(s->total);free(s->loss);free(s->inner_min);free(s->inner_max);
   free(s->means);free(s->loadings);free(s->train);free(s->test);free(s);
}
static THD_mvpa_pca *pca_alloc(const THD_foldplan *plan,int p,const int *grid,int nk,
 int ns,int nl,const THD_nested_foldplan *nested,int metric,int keep,char *err,int nerr)
{
   THD_mvpa_pca *s;int i,j,cap=0,n=plan->nsample,nf=plan->nfold;
   if(!grid || nk<1 || ns<1 || nl<1 || (nk>1 && !nested) || (nested && (nested->outer!=plan || !nested->inner))){
      pp_error(err,nerr,"PCA count tuning requires matching grouped inner folds");return NULL;
   }
   for(i=0;i<nk;i++){
      if(grid[i]<1){pp_error(err,nerr,"PCA counts must be positive");return NULL;}
      for(j=0;j<i;j++)if(grid[j]==grid[i]){pp_error(err,nerr,"duplicate PCA count");return NULL;}
      if(grid[i]>cap)cap=grid[i];
   }
   for(i=0;i<nf;i++){
      if(plan->ntrain[i]<2){pp_error(err,nerr,"PCA needs at least two training rows");return NULL;}
      if(nested){
         const THD_foldplan *in=nested->inner[i];
         if(!THD_foldplan_validate(in,err,nerr))return NULL;
         if(in->nsample!=plan->ntrain[i] || in->nfold<2 || in->kind==THD_CV_FIXED){pp_error(err,nerr,"PCA needs grouped inner folds over outer training rows");return NULL;}
         for(j=0;j<in->nfold;j++)if(in->ntrain[j]<2){pp_error(err,nerr,"PCA needs at least two inner training rows");return NULL;}
      }
   }
   if(cap>p)cap=p;if(cap>n-1)cap=n-1;
   if(ns>INT_MAX/nk || ns*nk>INT_MAX/nl ||
      (size_t)nf>SIZE_MAX/sizeof(double)/(size_t)ns/(size_t)nk/(size_t)nl ||
      (size_t)nf>SIZE_MAX/sizeof(double)/(size_t)cap/(size_t)p){pp_error(err,nerr,"PCA audit/candidate dimensions too large");return NULL;}
   s=calloc(1,sizeof(*s));if(!s){pp_error(err,nerr,"cannot allocate PCA configuration");return NULL;}
   s->ngrid=nk;s->nselection=ns;s->npenalty=nl;s->nested=nested;s->metric=metric;
   s->model=THD_pca_alloc(n,p,cap,err,nerr);
   s->grid=malloc(sizeof(int)*(size_t)nk);s->chosen=calloc((size_t)nf,sizeof(int));
   s->count=calloc((size_t)nf,sizeof(int));s->rank=calloc((size_t)nf,sizeof(int));s->input_count=calloc((size_t)nf,sizeof(int));
   s->variance=calloc((size_t)nf*cap,sizeof(double));s->total=calloc((size_t)nf,sizeof(double));
   s->loss=malloc(sizeof(double)*(size_t)nf*ns*nk*nl);
   s->inner_min=malloc(sizeof(int)*(size_t)nf*ns*nk);s->inner_max=malloc(sizeof(int)*(size_t)nf*ns*nk);
   s->train=malloc(sizeof(int)*(size_t)n);s->test=malloc(sizeof(int)*(size_t)n);
   if(keep){s->means=malloc(sizeof(double)*(size_t)nf*p);s->loadings=malloc(sizeof(double)*(size_t)nf*cap*p);}
   if(!s->model || !s->grid || !s->chosen || !s->count || !s->rank || !s->input_count ||
      !s->variance || !s->total || !s->loss || !s->inner_min || !s->inner_max || !s->train || !s->test || (keep && (!s->means || !s->loadings))){
      pca_free(s);pp_error(err,nerr,"cannot allocate PCA workspace/audits");return NULL;
   }
   memcpy(s->grid,grid,sizeof(int)*(size_t)nk);return s;
}
int THD_mvpa_ridge_set_pca(THD_mvpa_ridge_workspace *w,const int *grid,int nk,int keep,char *err,int nerr)
{
   if(!w || w->regression || w->pca || w->response_ridge)return pp_error(err,nerr,"configure PCA once after tuning and feature selection");
   w->pca=pca_alloc(w->plan,w->max_feature,grid,nk,w->selection?w->selection->ngrid:1,
                   w->nested?w->nlambda:1,w->nested,0,keep,err,nerr);return w->pca!=NULL;
}
int THD_mvpa_classification_set_pca(THD_mvpa_workspace *w,const int *grid,int nk,
 const THD_nested_foldplan *nested,int metric,int keep,char *err,int nerr)
{
   if(!w || w->pca || (metric!=THD_METRIC_ACCURACY && metric!=THD_METRIC_BALANCED_ACCURACY))
      return pp_error(err,nerr,"configure PCA once after classifier and feature selection, with ACC/BACC");
   if(w->ridge_cv){
      if(nested!=w->ridge_cv->nested)return pp_error(err,nerr,"PCA and ridge tuning folds differ");
      return THD_mvpa_ridge_set_pca(w->ridge_cv,grid,nk,keep,err,nerr);
   }
   if((w->class_nested && nested!=w->class_nested) || (w->selection && nested!=w->selection->nested))
      return pp_error(err,nerr,"PCA, feature and penalty tuning folds differ");
   w->pca=pca_alloc(w->plan,w->nfeature_alloc,grid,nk,w->selection?w->selection->ngrid:1,
                   w->class_nested?w->class_ngrid:1,nested,metric,keep,err,nerr);return w->pca!=NULL;
}
static int pca_transform(THD_mvpa_pca *s,int candidate,float *train,int nt,float *test,int ne,int *width,char *err,int nerr)
{
   if(!s)return 1;
   if(!THD_pca_fit(s->model,train,nt,*width,s->grid[candidate],err,nerr) ||
      !THD_pca_apply(s->model,train,nt,err,nerr) || (test && !THD_pca_apply(s->model,test,ne,err,nerr)))return 0;
   *width=s->model->count?s->model->count:1;return 1;
}
static void pca_record(THD_mvpa_pca *s,int f)
{
   THD_pca_model *m=s->model;int p=m->nfeature,cap=m->capacity;
   s->count[f]=m->count;s->rank[f]=m->rank;s->input_count[f]=p;s->total[f]=m->total_variance;
   memcpy(s->variance+(size_t)f*cap,m->variance,sizeof(double)*(size_t)m->count);
   if(s->means){
      memcpy(s->means+(size_t)f*m->max_feature,m->mean,sizeof(double)*(size_t)p);
      memcpy(s->loadings+(size_t)f*cap*m->max_feature,m->loading,sizeof(double)*(size_t)p*m->count);
   }
}
static int pca_ngrid(const THD_mvpa_pca *s){return s?s->ngrid:1;}
static void pca_inner_record(THD_mvpa_pca *s,int outer,int batch)
{
   size_t at=((size_t)outer*s->nselection*s->ngrid)+batch;int k=s->model->count;
   if(k<s->inner_min[at])s->inner_min[at]=k;if(k>s->inner_max[at])s->inner_max[at]=k;
}

void THD_activation_patterns_free(THD_activation_patterns *w)
{
   if(!w)return;
   free(w->rank);free(w->input);free(w->scores);free(w->pattern);free(w->xmean);free(w->smean);
   free(w->covariance);free(w->inverse);free(w->spectrum);free(w->cutoff);
   free(w->matrix);free(w->vectors);free(w->eigen);free(w->cross);free(w);
}
THD_activation_patterns *THD_activation_patterns_alloc(int n,int p,int q,int nf,char *err,int nerr)
{
   THD_activation_patterns *w;int width=p>q?p:q;
   if(n<2 || p<1 || q<1 || nf<1 ||
      (size_t)n>SIZE_MAX/sizeof(double)/(size_t)width ||
      (size_t)nf>SIZE_MAX/sizeof(double)/(size_t)width/(size_t)q){
      pp_error(err,nerr,"invalid or excessive activation-pattern dimensions");return NULL;
   }
   w=calloc(1,sizeof(*w));if(!w){pp_error(err,nerr,"cannot allocate activation patterns");return NULL;}
   w->max_sample=n;w->max_feature=p;w->max_output=q;w->nfold=nf;
   w->rank=malloc(sizeof(int)*(size_t)nf);w->input=malloc(sizeof(float)*(size_t)n*p);
   w->scores=malloc(sizeof(double)*(size_t)n*q);w->pattern=malloc(sizeof(double)*(size_t)nf*p*q);
   w->xmean=malloc(sizeof(double)*(size_t)nf*p);w->smean=malloc(sizeof(double)*(size_t)nf*q);
   w->covariance=malloc(sizeof(double)*(size_t)nf*q*q);w->inverse=malloc(sizeof(double)*(size_t)nf*q*q);
   w->spectrum=malloc(sizeof(double)*(size_t)nf*q);w->cutoff=malloc(sizeof(double)*(size_t)nf);
   w->matrix=malloc(sizeof(double)*(size_t)q*q);w->vectors=malloc(sizeof(double)*(size_t)q*q);
   w->eigen=malloc(sizeof(double)*(size_t)q);w->cross=malloc(sizeof(double)*(size_t)p*q);
   if(!w->rank || !w->input || !w->scores || !w->pattern || !w->xmean || !w->smean ||
      !w->covariance || !w->inverse || !w->spectrum || !w->cutoff || !w->matrix || !w->vectors || !w->eigen || !w->cross){
      THD_activation_patterns_free(w);pp_error(err,nerr,"cannot allocate activation-pattern buffers");return NULL;
   }
   for(n=0;n<nf;n++)w->rank[n]=-1;
   return w;
}
int THD_activation_patterns_fit(THD_activation_patterns *w,const float *x,
 const double *scores,int n,int p,int q,int fold,char *err,int nerr)
{
   int i,j,k,o;double *xm,*sm,*cov,*inv,*a,cutoff;
   if(!w || !x || !scores || n<2 || n>w->max_sample || p<1 || p>w->max_feature ||
      q<1 || q>w->max_output || fold<0 || fold>=w->nfold)
      return pp_error(err,nerr,"activation patterns need matching training rows and dimensions");
   w->rank[fold]=-1;
   xm=w->xmean+(size_t)fold*w->max_feature;sm=w->smean+(size_t)fold*w->max_output;
   cov=w->covariance+(size_t)fold*w->max_output*w->max_output;
   inv=w->inverse+(size_t)fold*w->max_output*w->max_output;
   a=w->pattern+(size_t)fold*w->max_feature*w->max_output;
   for(j=0;j<p;j++){
      double sum=0;for(i=0;i<n;i++){
         double value=x[(size_t)i*p+j];if(!isfinite(value))return pp_error(err,nerr,"nonfinite pattern input");sum+=value-x[j];
      }xm[j]=x[j]+sum/n;
   }
   for(o=0;o<q;o++){
      double sum=0;for(i=0;i<n;i++){
         double value=scores[(size_t)i*q+o];if(!isfinite(value))return pp_error(err,nerr,"nonfinite pattern score");sum+=value-scores[o];
      }sm[o]=scores[o]+sum/n;
      if(!isfinite(sm[o]))return pp_error(err,nerr,"nonfinite pattern score mean");
   }
   for(o=0;o<q;o++)for(k=o;k<q;k++){
      double sum=0;for(i=0;i<n;i++)sum+=(scores[(size_t)i*q+o]-sm[o])*(scores[(size_t)i*q+k]-sm[k]);
      sum/=n-1;if(!isfinite(sum))return pp_error(err,nerr,"nonfinite score covariance");
      cov[(size_t)o*q+k]=cov[(size_t)k*q+o]=w->matrix[(size_t)o*q+k]=w->matrix[(size_t)k*q+o]=sum;
   }
   if(!mvpa_symmetric_eigen(w->matrix,w->vectors,w->eigen,q,err,nerr))return 0;
   cutoff=fmax(0,w->eigen[0])*64*DBL_EPSILON*(n>q?n:q);w->cutoff[fold]=cutoff;
   memcpy(w->spectrum+(size_t)fold*w->max_output,w->eigen,sizeof(double)*(size_t)q);
   memset(inv,0,sizeof(double)*(size_t)q*q);w->rank[fold]=0;
   for(k=0;k<q;k++)if(w->eigen[k]>cutoff){
      w->rank[fold]++;
      for(o=0;o<q;o++)for(j=0;j<q;j++)inv[(size_t)o*q+j]+=w->vectors[(size_t)o*q+k]*w->vectors[(size_t)j*q+k]/w->eigen[k];
   }
   /* Compute A through the retained eigenspace rather than cross * inverse:
      this avoids cancellation from large inverse entries near the cutoff. */
   for(j=0;j<p;j++){
      for(o=0;o<q;o++){
         double sum=0;for(i=0;i<n;i++)sum+=(x[(size_t)i*p+j]-xm[j])*(scores[(size_t)i*q+o]-sm[o]);
         w->cross[(size_t)j*q+o]=sum/(n-1);a[(size_t)j*q+o]=0;
      }
      for(k=0;k<q;k++)if(w->eigen[k]>cutoff){
         double value=0;
         for(o=0;o<q;o++)value+=w->cross[(size_t)j*q+o]*w->vectors[(size_t)o*q+k];
         value/=w->eigen[k];
         for(o=0;o<q;o++)a[(size_t)j*q+o]+=value*w->vectors[(size_t)o*q+k];
      }
      for(o=0;o<q;o++)if(!isfinite(a[(size_t)j*q+o]))return pp_error(err,nerr,"nonfinite activation pattern");
   }
   for(o=0;o<q;o++)for(k=0;k<q;k++)if(!isfinite(inv[(size_t)o*q+k]))return pp_error(err,nerr,"nonfinite score covariance inverse");
   return 1;
}
int THD_mvpa_ridge_enable_patterns(THD_mvpa_ridge_workspace *w,char *err,int nerr)
{
   int f;
   if(!w || w->regression || w->patterns)return pp_error(err,nerr,"configure ridge activation patterns once");
   for(f=0;f<w->plan->nfold;f++)if(w->plan->ntrain[f]<2)
      return pp_error(err,nerr,"activation patterns need at least two outer training rows");
   w->patterns=THD_activation_patterns_alloc(w->plan->nsample,w->max_feature,w->max_output,w->plan->nfold,err,nerr);
   return w->patterns!=NULL;
}
static int ridge_record_patterns(THD_mvpa_ridge_workspace *w,int fold,int n,int p,int q,char *err,int nerr)
{
   THD_activation_patterns *pat=w->patterns;THD_ridge_workspace *r=w->ridge;int i,j,o;
   if(!pat)return 1;
   /* Centered scores in double precision avoid losing the score subspace to
      float rounding (especially the constant sum of class indicator fits). */
   for(i=0;i<n;i++)for(o=0;o<q;o++){
      double value=0;
      for(j=0;j<r->p;j++)value+=(w->train_x[(size_t)i*r->p+j]-r->xm[j])*r->coef[(size_t)j*q+o];
      pat->scores[(size_t)i*q+o]=value;
   }
   if(!THD_activation_patterns_fit(pat,pat->input,pat->scores,n,p,q,fold,err,nerr))return 0;
   for(o=0;o<q;o++)pat->smean[(size_t)fold*pat->max_output+o]+=r->ym[o];
   return 1;
}

static void response_ridge_free(THD_response_ridge *a)
{
   if(a){free(a->selected);free(a->loss);free(a->config_loss);free(a->best);free(a->nvalid);free(a);}
}
int THD_mvpa_ridge_enable_response_lambda(THD_mvpa_ridge_workspace *w,char *err,int nerr)
{
   THD_response_ridge *a;size_t nf,nc,nq,nl,cells;
   if(!w || w->regression || w->response_ridge || !w->nested || w->classification || w->inner_confusion)
      return pp_error(err,nerr,"per-response ridge requires continuous nested tuning; configure it once after the pipeline");
   nf=w->plan->nfold;nq=w->max_output;nl=w->nlambda;
   nc=(size_t)(w->selection?w->selection->ngrid:1)*pca_ngrid(w->pca);
   if(!nc || nc>INT_MAX || nf>SIZE_MAX/nc || nf*nc>SIZE_MAX/nl || nf*nc*nl>SIZE_MAX/nq ||
      nf*nc*nl*nq>SIZE_MAX/sizeof(double))return pp_error(err,nerr,"per-response ridge audit dimensions are too large");
   cells=nf*nc*nl*nq;a=calloc(1,sizeof(*a));if(!a)goto nomem;
   a->nconfig=(int)nc;a->selected=malloc(sizeof(double)*nf*nq);
   a->loss=malloc(sizeof(double)*cells);a->config_loss=malloc(sizeof(double)*nf*nc);
   a->best=malloc(sizeof(int)*nf*nc*nq);a->nvalid=malloc(sizeof(int)*nf*nc);
   if(!a->selected || !a->loss || !a->config_loss || !a->best || !a->nvalid){response_ridge_free(a);goto nomem;}
   w->response_ridge=a;return 1;
nomem:return pp_error(err,nerr,"cannot allocate per-response ridge audits");
}

void THD_mvpa_ridge_workspace_free(THD_mvpa_ridge_workspace *w)
{
   if(!w)return;
   THD_regression_free(w->regression);free(w->reg_diagnostic);free(w->reg_inner);free(w->reg_coef);free(w->reg_group);
   response_ridge_free(w->response_ridge);
   THD_preprocess_free(w->prep);THD_ridge_free(w->ridge);fs_free(w->selection);pca_free(w->pca);THD_activation_patterns_free(w->patterns);
   free(w->train_x);free(w->test_x);free(w->train_y);free(w->test_y);
   free(w->prediction);free(w->baseline);free(w->truth_scratch);free(w->pred_scratch);
   free(w->base_scratch);free(w->rank_scratch);free(w->index_scratch);
   free(w->score);free(w->fold_score);free(w->selected_lambda);free(w->inner_nvalid);
   free(w->lambda_grid);free(w->inner_loss);free(w->inner_train);free(w->inner_test);
   free(w->inner_sse);free(w->inner_baseline);free(w->inner_confusion);free(w);
}

THD_mvpa_ridge_workspace *THD_mvpa_ridge_workspace_alloc(
 const THD_foldplan *plan,int p,int q,int preprocess,char *err,int nerr)
{
   THD_mvpa_ridge_workspace *w;int n;
   if(!THD_foldplan_validate(plan,err,nerr))return NULL;
   if(p<1 || q<1 || !THD_preprocess_name(preprocess)){
      pp_error(err,nerr,"invalid ridge CV dimensions or preprocessing");return NULL;
   }
   n=plan->nsample;
   if((size_t)n>SIZE_MAX/sizeof(double)/(size_t)(p>q?p:q) ||
      (size_t)plan->nfold>SIZE_MAX/sizeof(THD_mvpa_continuous_score)/(size_t)q){
      pp_error(err,nerr,"ridge CV dimensions too large");return NULL;
   }
   w=calloc(1,sizeof(*w));
   if(!w){pp_error(err,nerr,"out of memory allocating ridge CV");return NULL;}
   w->plan=plan;w->max_feature=p;w->max_output=q;w->preprocess=preprocess;
   w->prep=pp_alloc(p,preprocess==THD_PREP_RUN_CENTER?n:1,err,nerr);
   w->ridge=THD_ridge_alloc(n,p,q,err,nerr);
   w->train_x=malloc(sizeof(float)*(size_t)n*p);w->test_x=malloc(sizeof(float)*(size_t)n*p);
   w->train_y=malloc(sizeof(float)*(size_t)n*q);w->test_y=malloc(sizeof(float)*(size_t)n*q);
   w->prediction=malloc(sizeof(float)*(size_t)n*q);w->baseline=malloc(sizeof(float)*(size_t)n*q);
   w->truth_scratch=malloc(sizeof(float)*(size_t)n);w->pred_scratch=malloc(sizeof(float)*(size_t)n);
   w->base_scratch=malloc(sizeof(float)*(size_t)n);w->rank_scratch=malloc(sizeof(double)*(size_t)n*2);
   w->index_scratch=malloc(sizeof(int)*(size_t)n*2);
   w->score=malloc(sizeof(*w->score)*(size_t)q);
   w->fold_score=malloc(sizeof(*w->fold_score)*(size_t)plan->nfold*q);
   w->selected_lambda=malloc(sizeof(double)*(size_t)plan->nfold);
   w->inner_nvalid=calloc((size_t)plan->nfold,sizeof(int));
   if(!w->prep || !w->ridge || !w->train_x || !w->test_x || !w->train_y || !w->test_y ||
      !w->prediction || !w->baseline || !w->truth_scratch || !w->pred_scratch ||
      !w->base_scratch || !w->rank_scratch || !w->index_scratch || !w->score || !w->fold_score ||
      !w->selected_lambda || !w->inner_nvalid){
      THD_mvpa_ridge_workspace_free(w);pp_error(err,nerr,"out of memory allocating ridge CV");return NULL;
   }return w;
}

int THD_mvpa_ridge_set_tuning(THD_mvpa_ridge_workspace *w,
 const THD_nested_foldplan *nested,const double *grid,int nl,char *err,int nerr)
{
   int i,j;
   if(!w || w->regression || w->selection || w->pca || w->nested || !nested || nested->outer!=w->plan || !nested->inner || !grid || nl<1)
      return pp_error(err,nerr,"ridge tuning needs a matching nested plan and at least one penalty; configure it once");
   for(i=0;i<nl;i++){
      if(!isfinite(grid[i]) || grid[i]<=0)return pp_error(err,nerr,"lambda grid values must be finite and positive");
      for(j=0;j<i;j++)if(grid[i]==grid[j])return pp_error(err,nerr,"lambda grid contains duplicate penalties");
   }
   for(i=0;i<w->plan->nfold;i++){
      const THD_foldplan *in=nested->inner[i];
      if(!THD_foldplan_validate(in,err,nerr))return 0;
      if(in->nsample!=w->plan->ntrain[i] || in->nfold<2 || in->kind==THD_CV_FIXED)
         return pp_error(err,nerr,"inner plan needs at least two grouped folds over exactly the outer training samples");
   }
   if((size_t)nl>SIZE_MAX/sizeof(double)/(size_t)w->max_output ||
      (size_t)nl>SIZE_MAX/sizeof(double)/(size_t)w->plan->nfold)
      return pp_error(err,nerr,"lambda grid is too large");
   w->lambda_grid=malloc(sizeof(double)*(size_t)nl);
   w->inner_loss=malloc(sizeof(double)*(size_t)w->plan->nfold*nl);
   w->inner_train=malloc(sizeof(int)*(size_t)w->plan->nsample);
   w->inner_test=malloc(sizeof(int)*(size_t)w->plan->nsample);
   w->inner_sse=malloc(sizeof(double)*(size_t)nl*w->max_output);
   w->inner_baseline=malloc(sizeof(double)*(size_t)w->max_output);
   if(!w->lambda_grid || !w->inner_loss || !w->inner_train || !w->inner_test ||
      !w->inner_sse || !w->inner_baseline){
      free(w->lambda_grid);w->lambda_grid=NULL;free(w->inner_loss);w->inner_loss=NULL;
      free(w->inner_train);w->inner_train=NULL;free(w->inner_test);w->inner_test=NULL;
      free(w->inner_sse);w->inner_sse=NULL;free(w->inner_baseline);w->inner_baseline=NULL;
      return pp_error(err,nerr,"out of memory allocating ridge tuning buffers");
   }
   memcpy(w->lambda_grid,grid,sizeof(double)*(size_t)nl);
   w->nlambda=nl;w->nested=nested;return 1;
}

int THD_mvpa_set_regression(THD_mvpa_ridge_workspace *w,
 const THD_regression_config *config,char *err,int nerr)
{
   size_t nf,np,nq,nc;int i;
   if(!w || !config || w->regression || w->classification || w->response_ridge || w->patterns)
      return pp_error(err,nerr,"configure a continuous estimator once, last, without ridge-specific exports or response penalties");
   if(config->ncandidate!=(w->nested?w->nlambda:1))return pp_error(err,nerr,"regression candidate count does not match nested search");
   if(w->nested)for(i=0;i<w->nlambda;i++)if(w->lambda_grid[i]!=i+1)return pp_error(err,nerr,"regression tuning grid must contain consecutive one-based candidate IDs");
   if(config->method==THD_REG_BANDED_RIDGE && (w->selection || w->pca))return pp_error(err,nerr,"banded ridge preserves named design groups; selection/PCA are not supported");
   if(config->method==THD_REG_ELASTIC_NET && w->pca)return pp_error(err,nerr,"elastic net original-feature sparsity requires no PCA");
   nf=w->plan->nfold;np=w->max_feature;nq=w->max_output;
   nc=(size_t)(w->selection?w->selection->ngrid:1)*pca_ngrid(w->pca)*config->ncandidate;
   if(nf>SIZE_MAX/sizeof(THD_regression_diagnostic)/nc || nf>SIZE_MAX/sizeof(double)/np/nq)
      return pp_error(err,nerr,"regression audits exceed capacity");
   w->regression=THD_regression_alloc(w->plan->nsample,w->max_feature,w->max_output,config,err,nerr);
   if(!w->regression)return 0;
   w->reg_diagnostic=calloc(nf*nq,sizeof(*w->reg_diagnostic));
   w->reg_inner=calloc(nf*nc,sizeof(*w->reg_inner));
   if(config->method==THD_REG_ELASTIC_NET)w->reg_coef=calloc(nf*np*nq,sizeof(double));
   if(config->method==THD_REG_BANDED_RIDGE){
      if(nf>SIZE_MAX/sizeof(double)/(size_t)config->ngroup/nq/2)return pp_error(err,nerr,"group contribution audits exceed capacity");
      w->reg_group=calloc(nf*config->ngroup*nq*2,sizeof(double));
   }
   if(!w->reg_diagnostic || !w->reg_inner || (config->method==THD_REG_ELASTIC_NET && !w->reg_coef) ||
      (config->method==THD_REG_BANDED_RIDGE && !w->reg_group))return pp_error(err,nerr,"cannot allocate regression audits");
   return 1;
}
static int regression_cv_fit(THD_mvpa_ridge_workspace *w,int nt,int width,int q,double candidate,char *err,int nerr)
{
   if(w->regression)return THD_regression_fit(w->regression,w->train_x,w->train_y,nt,width,q,(int)candidate-1,err,nerr);
   return THD_ridge_fit(w->ridge,w->train_x,w->train_y,nt,width,q,candidate,THD_RIDGE_AUTO,err,nerr);
}
static int regression_cv_predict(THD_mvpa_ridge_workspace *w,int ne,char *err,int nerr)
{
   if(w->regression)return THD_regression_predict(w->regression,w->test_x,ne,w->test_y,err,nerr);
   return THD_ridge_predict(w->ridge,w->test_x,ne,w->test_y,err,nerr);
}
static double regression_cv_mean(const THD_mvpa_ridge_workspace *w,int o)
{return w->regression?w->regression->ym[o]:w->ridge->ym[o];}
static void regression_cv_record(THD_mvpa_ridge_workspace *w,int fold,int nt,int p,int width,int q)
{
   THD_regression *m=w->regression;int j,o,g,i;
   if(!m)return;
   memcpy(w->reg_diagnostic+(size_t)fold*w->max_output,m->diagnostic,sizeof(*m->diagnostic)*(size_t)q);
   if(w->reg_coef){
      double *coef=w->reg_coef+(size_t)fold*w->max_feature*w->max_output;
      memset(coef,0,sizeof(double)*(size_t)w->max_feature*w->max_output);
      for(j=0;j<width;j++){
         int id=w->selection?w->selection->indices[(size_t)fold*w->max_feature+j]:j;
         for(o=0;o<q;o++)coef[(size_t)id*w->max_output+o]=m->coef[(size_t)j*q+o];
      }
   }
   if(w->reg_group)for(g=0;g<m->config.ngroup;g++)for(o=0;o<q;o++){
      double var=0,cov=0,*out=w->reg_group+(((size_t)fold*m->config.ngroup+g)*w->max_output+o)*2;
      for(i=0;i<nt;i++){
         double part=0,total=0;
         for(j=0;j<p;j++){
            double value=(w->train_x[(size_t)i*p+j]-m->xm[j])*m->coef[(size_t)j*q+o];
            total+=value;if(m->group[j]==g)part+=value;
         }
         var+=part*part;cov+=part*total;
      }
      out[0]=var/(nt-1);out[1]=cov/(nt-1);
   }
}

typedef struct {
   THD_mvpa_ridge_workspace *w;
   const float *x,*y;const int *group;
   int p,q,objective,ntest,width,outer;
} Ridge_tuning;
static int ridge_candidate_setup(void *data,int batch,char *err,int nerr)
{
   Ridge_tuning *t=data;THD_mvpa_ridge_workspace *w=t->w;
   (void)batch;(void)err;(void)nerr;
   memset(w->inner_sse,0,sizeof(double)*(size_t)w->nlambda*t->q);
   memset(w->inner_baseline,0,sizeof(double)*(size_t)t->q);
   if(w->inner_confusion)memset(w->inner_confusion,0,sizeof(int)*(size_t)w->nlambda*t->q*t->q);
   if(w->pca){size_t at=(size_t)t->outer*w->pca->nselection*w->pca->ngrid+batch;
      w->pca->inner_min[at]=INT_MAX;w->pca->inner_max[at]=0;}
   if(w->regression){
      int l,nc=(w->selection?w->selection->ngrid:1)*pca_ngrid(w->pca);
      for(l=0;l<w->nlambda;l++){
         THD_regression_diagnostic *dg=w->reg_inner+((size_t)t->outer*nc+batch)*w->nlambda+l;
         memset(dg,0,sizeof(*dg));dg->components=INT_MAX;
      }
   }
   t->ntest=0;return 1;
}
static int ridge_candidate_prepare(void *data,int batch,const int *train,int nt,
 const int *test,int ne,char *err,int nerr)
{
   Ridge_tuning *t=data;THD_mvpa_ridge_workspace *w=t->w;
   THD_feature_selection *sel=w->selection;int i,o,n=w->plan->nsample,p=t->p,q=t->q;
   if(!pp_refit(w->prep,w->preprocess,t->x,n,p,train,nt,t->group,0,err,nerr) ||
      !pp_apply(w->prep,t->x,n,p,train,nt,t->group,w->train_x,0,err,nerr) ||
      !pp_apply(w->prep,t->x,n,p,test,ne,t->group,w->test_x,0,err,nerr))return 0;
   for(i=0;i<nt;i++)for(o=0;o<q;o++)w->train_y[(size_t)i*q+o]=t->y[(size_t)train[i]*q+o];
   if(sel){
      int width=fs_count(sel,batch/pca_ngrid(w->pca),p);
      if(sel->classes)for(i=0;i<nt;i++)sel->train[i]=sel->classes[train[i]];
      if(!fs_fit(sel,w->train_x,nt,p,w->train_y,q,sel->classes?sel->train:NULL,err,nerr))return 0;
      fs_apply(sel,w->train_x,nt,p,width);fs_apply(sel,w->test_x,ne,p,width);
   }
   t->width=fs_count(sel,batch/pca_ngrid(w->pca),p);
   if(!pca_transform(w->pca,batch%pca_ngrid(w->pca),w->train_x,nt,w->test_x,ne,&t->width,err,nerr))return 0;
   if(w->pca)pca_inner_record(w->pca,t->outer,batch);
   t->ntest+=ne;return 1;
}
static int ridge_candidate_evaluate(void *data,int batch,int candidate,
 const int *train,int nt,const int *test,int ne,char *err,int nerr)
{
   Ridge_tuning *t=data;THD_mvpa_ridge_workspace *w=t->w;int i,o,q=t->q;
   int width=t->width;(void)train;(void)batch;
   if(!regression_cv_fit(w,nt,width,q,w->lambda_grid[candidate],err,nerr) ||
      !regression_cv_predict(w,ne,err,nerr))return 0;
   if(w->regression){
      int nc=(w->selection?w->selection->ngrid:1)*pca_ngrid(w->pca);
      THD_regression_diagnostic *dg=w->reg_inner+((size_t)t->outer*nc+batch)*w->nlambda+candidate;
      for(o=0;o<q;o++){
         const THD_regression_diagnostic *fit=w->regression->diagnostic+o;
         if(fit->iterations>dg->iterations)dg->iterations=fit->iterations;
         dg->residual=fmax(dg->residual,fit->residual);
         if(fit->components<dg->components)dg->components=fit->components;
      }
   }
   for(i=0;i<ne;i++){
      if(w->inner_confusion){
         int truth=0,pred=0;
         for(o=0;o<q;o++){
            if(!isfinite(w->test_y[(size_t)i*q+o]))return pp_error(err,nerr,"nonfinite inner prediction");
            if(t->y[(size_t)test[i]*q+o]>t->y[(size_t)test[i]*q+truth])truth=o;
            if(w->test_y[(size_t)i*q+o]>w->test_y[(size_t)i*q+pred])pred=o;
         }
         w->inner_confusion[((size_t)candidate*q+truth)*q+pred]++;
      }else for(o=0;o<q;o++){
         double truth=t->y[(size_t)test[i]*q+o],e=truth-w->test_y[(size_t)i*q+o];
         if(!isfinite(e))return pp_error(err,nerr,"nonfinite inner prediction error");
         if(t->objective==THD_TUNE_MAE)w->inner_sse[(size_t)candidate*q+o]+=fabs(e);
         else w->inner_sse[(size_t)candidate*q+o]+=e*e;
         if(candidate==0){double d=truth-regression_cv_mean(w,o);w->inner_baseline[o]+=d*d;}
      }
   }
   return 1;
}
static int ridge_candidate_finish(void *data,int batch,int candidate,double *loss,int *nvalid,char *err,int nerr)
{
   Ridge_tuning *t=data;THD_mvpa_ridge_workspace *w=t->w;
   if(w->response_ridge){
      THD_response_ridge *a=w->response_ridge;int o,nv;
      for(o=0;o<t->q;o++)if(!tune_continuous_loss(w->inner_sse+(size_t)candidate*t->q+o,
         w->inner_baseline+o,1,t->ntest,t->objective,
         a->loss+(((size_t)t->outer*a->nconfig+batch)*w->nlambda+candidate)*w->max_output+o,
         &nv,err,nerr))return 0;
   }
   if(w->inner_confusion)return tune_class_loss(w->inner_confusion+(size_t)candidate*t->q*t->q,t->q,t->objective,loss,nvalid,err,nerr);
   return tune_continuous_loss(w->inner_sse+(size_t)candidate*t->q,w->inner_baseline,t->q,t->ntest,t->objective,loss,nvalid,err,nerr);
}
static int ridge_candidate_prefer(void *data,int a,int b)
{
   Ridge_tuning *t=data;THD_mvpa_ridge_workspace *w=t->w;THD_feature_selection *sel=w->selection;
   int np=pca_ngrid(w->pca),ac=a/w->nlambda,bc=b/w->nlambda;
   if(sel && sel->grid[ac/np]!=sel->grid[bc/np])return sel->grid[ac/np]<sel->grid[bc/np];
   if(w->pca && w->pca->grid[ac%np]!=w->pca->grid[bc%np])return w->pca->grid[ac%np]<w->pca->grid[bc%np];
   if(w->regression)return a%w->nlambda<b%w->nlambda;
   return w->lambda_grid[a%w->nlambda]>w->lambda_grid[b%w->nlambda];
}
static int ridge_tune_fold(THD_mvpa_ridge_workspace *w,int outer,
 const float *x,int p,const float *y,int q,const int *group,char *err,int nerr)
{
   static const THD_cv_candidate_ops ops={ridge_candidate_setup,ridge_candidate_prepare,
      ridge_candidate_evaluate,ridge_candidate_finish,ridge_candidate_prefer};
   THD_feature_selection *sel=w->selection;THD_mvpa_pca *pc=w->pca;
   int nk=sel?sel->ngrid:1,np=pca_ngrid(pc),best,nvalid,l,c;
   double *loss=pc?pc->loss+(size_t)outer*nk*np*w->nlambda:sel?sel->loss+(size_t)outer*nk*w->nlambda:w->inner_loss+(size_t)outer*w->nlambda;
   Ridge_tuning context={w,x,y,group,p,q,w->tune_objective?w->tune_objective:THD_TUNE_NORMALIZED_MSE,0,0,outer};
   if(!THD_nested_candidate_search(w->nested,outer,nk*np,w->nlambda,&ops,&context,
      w->inner_train,w->inner_test,loss,&best,&nvalid,err,nerr))return 0;
   if(w->response_ridge){
      THD_response_ridge *a=w->response_ridge;int config,o,chosen=-1;double best_value=NAN;
      for(config=0;config<nk*np;config++){
         size_t at=(size_t)outer*a->nconfig+config;double total=0;int valid=0;
         for(o=0;o<q;o++){
            int winner=0;double value=NAN;
            for(l=0;l<w->nlambda;l++){
               double v=a->loss[(at*w->nlambda+l)*w->max_output+o];
               if((isfinite(v) && (!isfinite(value) || v<value)) ||
                  ((v==value || (!isfinite(v) && !isfinite(value))) && w->lambda_grid[l]>w->lambda_grid[winner])){winner=l;value=v;}
            }
            a->best[at*w->max_output+o]=winner;
            if(isfinite(value)){total+=value;valid++;}
         }
         a->nvalid[at]=valid;a->config_loss[at]=valid?total/valid:NAN;
         {double value=a->config_loss[at];
          if(chosen<0 || (isfinite(value) && (!isfinite(best_value) || value<best_value)) ||
             ((value==best_value || (!isfinite(value) && !isfinite(best_value))) &&
              ridge_candidate_prefer(&context,config*w->nlambda,chosen*w->nlambda))){chosen=config;best_value=value;}}
      }
      best=chosen*w->nlambda;nvalid=a->nvalid[(size_t)outer*a->nconfig+chosen];
      for(o=0;o<q;o++)a->selected[(size_t)outer*w->max_output+o]=w->lambda_grid[a->best[((size_t)outer*a->nconfig+chosen)*w->max_output+o]];
   }
   if(pc)pc->chosen[outer]=(best/w->nlambda)%np;
   if(sel){
      sel->chosen[outer]=best/w->nlambda/np;
      if(pc)for(c=0;c<nk;c++)for(l=0;l<w->nlambda;l++)
         sel->loss[((size_t)outer*nk+c)*w->nlambda+l]=loss[(c*np+pc->chosen[outer])*w->nlambda+l];
   }
   if(sel || pc)for(l=0;l<w->nlambda;l++)w->inner_loss[(size_t)outer*w->nlambda+l]=loss[(best/w->nlambda)*w->nlambda+l];
   w->selected_lambda[outer]=w->response_ridge?NAN:w->lambda_grid[best%w->nlambda];w->inner_nvalid[outer]=nvalid;return 1;
}

int THD_mvpa_run_ridge_cv(THD_mvpa_ridge_workspace *w,const float *x,int p,
 const float *y,int q,const int *group,double lambda,char *err,int nerr)
{
   int f,i,o,n,total;const THD_foldplan *plan;
   if(!w || !x || !y || p<1 || p>w->max_feature || q<1 || q>w->max_output)
      return pp_error(err,nerr,"ridge CV inputs exceed workspace capacity");
   plan=w->plan;n=plan->nsample;
   for(i=0;i<n;i++)for(o=0;o<p;o++)
      if(!isfinite(x[(size_t)i*p+o]))return pp_error(err,nerr,"ridge CV predictor is not finite");
   for(i=0;i<n;i++)for(o=0;o<q;o++){
      if(!isfinite(y[(size_t)i*q+o]))return pp_error(err,nerr,"ridge CV response is not finite");
      w->prediction[(size_t)i*q+o]=w->baseline[(size_t)i*q+o]=NAN;
   }
   for(f=0;f<plan->nfold;f++){
      int nt=plan->ntrain[f],ne=plan->ntest[f],width=p;
      THD_feature_selection *sel=w->selection;
      if(sel)sel->chosen[f]=0;
      if(w->pca)w->pca->chosen[f]=0;
      w->selected_lambda[f]=w->regression?1:lambda;w->inner_nvalid[f]=0;
      if(w->nested && !ridge_tune_fold(w,f,x,p,y,q,group,err,nerr))return 0;
      if(!pp_refit(w->prep,w->preprocess,x,n,p,plan->train[f],nt,group,0,err,nerr) ||
         !pp_apply(w->prep,x,n,p,plan->train[f],nt,group,w->train_x,0,err,nerr) ||
         !pp_apply(w->prep,x,n,p,plan->test[f],ne,group,w->test_x,0,err,nerr))return 0;
      for(i=0;i<nt;i++)for(o=0;o<q;o++)w->train_y[(size_t)i*q+o]=y[(size_t)plan->train[f][i]*q+o];
      if(w->patterns)memcpy(w->patterns->input,w->train_x,sizeof(float)*(size_t)nt*p);
      if(sel){
         if(sel->classes)for(i=0;i<nt;i++)sel->train[i]=sel->classes[plan->train[f][i]];
         if(!fs_fit(sel,w->train_x,nt,p,w->train_y,q,sel->classes?sel->train:NULL,err,nerr))return 0;
         fs_record(sel,f,sel->chosen[f],p);width=sel->count[f];
         fs_apply(sel,w->train_x,nt,p,width);fs_apply(sel,w->test_x,ne,p,width);
      }
      if(!pca_transform(w->pca,w->pca?w->pca->chosen[f]:0,w->train_x,nt,w->test_x,ne,&width,err,nerr))return 0;
      if(w->pca)pca_record(w->pca,f);
      if(!(w->response_ridge?
         THD_ridge_fit_response_lambda(w->ridge,w->train_x,w->train_y,nt,width,q,w->response_ridge->selected+(size_t)f*w->max_output,THD_RIDGE_AUTO,err,nerr):
         regression_cv_fit(w,nt,width,q,w->selected_lambda[f],err,nerr)) ||
         !regression_cv_predict(w,ne,err,nerr))return 0;
      regression_cv_record(w,f,nt,p,width,q);
      if(!ridge_record_patterns(w,f,nt,p,q,err,nerr))return 0;
      for(o=0;o<q;o++){
         for(i=0;i<ne;i++){
            int s=plan->test[f][i];
            w->prediction[(size_t)s*q+o]=w->test_y[(size_t)i*q+o];
            w->baseline[(size_t)s*q+o]=(float)regression_cv_mean(w,o);
            w->truth_scratch[i]=y[(size_t)s*q+o];w->pred_scratch[i]=w->prediction[(size_t)s*q+o];
            w->base_scratch[i]=w->baseline[(size_t)s*q+o];
         }
         if(!THD_mvpa_score_continuous(w->truth_scratch,w->pred_scratch,w->base_scratch,ne,
            w->rank_scratch,w->index_scratch,w->fold_score+(size_t)f*q+o,err,nerr))return 0;
      }
   }
   for(o=0;o<q;o++){
      total=0;for(i=0;i<n;i++)if(plan->sample_test_fold[i]>=0){
         w->truth_scratch[total]=y[(size_t)i*q+o];w->pred_scratch[total]=w->prediction[(size_t)i*q+o];
         w->base_scratch[total++]=w->baseline[(size_t)i*q+o];
      }
      if(!THD_mvpa_score_continuous(w->truth_scratch,w->pred_scratch,w->base_scratch,total,
         w->rank_scratch,w->index_scratch,w->score+o,err,nerr))return 0;
   }return 1;
}

/* Frozen model v1: explicit stages preserve float rounding in pp_apply/PCA. */
struct THD_mvpa_frozen_ridge {
   int p,q,k,scale,ns,pc,ncomp,width; /* pc flag; ncomp==0 is PCA rank zero */
   int *index,*constant;
   double *center,*sd,*zmean,*zsd,*zbeta,*xmean,*pcmean,*loading;
   double *xm,*ym,*coef,*lambda;
};
void THD_mvpa_frozen_free(THD_mvpa_frozen_ridge *m)
{
   if(!m)return;
   free(m->index);free(m->constant);free(m->center);free(m->sd);free(m->zmean);free(m->zsd);
   free(m->zbeta);free(m->xmean);free(m->pcmean);free(m->loading);free(m->xm);free(m->ym);free(m->coef);free(m->lambda);free(m);
}
static THD_mvpa_frozen_ridge *frozen_alloc(int p,int q,int k,int scale,int ns,int pc,int ncomp)
{
   THD_mvpa_frozen_ridge *m;size_t width=pc?(ncomp?ncomp:1):ns;
   if(p<1 || q<1 || k<0 || ns<1 || ns>p || pc<0 || pc>1 || ncomp<0 || ncomp>ns || (!pc && ncomp) || scale<0 || scale>THD_PREP_SAMPLE_CENTER ||
      (size_t)p>SIZE_MAX/sizeof(double)/(size_t)(k?k:1) || (size_t)ns>SIZE_MAX/sizeof(double)/(size_t)(ncomp?ncomp:1) || width>SIZE_MAX/sizeof(double)/(size_t)q)return NULL;
   m=calloc(1,sizeof(*m));if(!m)return NULL;
   m->p=p;m->q=q;m->k=k;m->scale=scale;m->ns=ns;m->pc=pc;m->ncomp=ncomp;m->width=(int)width;
#define FALLOC(field,type,n) m->field=calloc((size_t)(n)?(size_t)(n):1,sizeof(type));if(!m->field)goto bad
   FALLOC(index,int,ns);FALLOC(constant,int,p);FALLOC(center,double,p);FALLOC(sd,double,p);
   FALLOC(zmean,double,k);FALLOC(zsd,double,k);FALLOC(zbeta,double,(size_t)k*p);FALLOC(xmean,double,p);
   FALLOC(pcmean,double,ns);FALLOC(loading,double,(size_t)ncomp*ns);
   FALLOC(xm,double,width);FALLOC(ym,double,q);FALLOC(coef,double,width*q);FALLOC(lambda,double,q);
#undef FALLOC
   return m;
bad:THD_mvpa_frozen_free(m);return NULL;
}
THD_mvpa_frozen_ridge *THD_mvpa_ridge_freeze(const THD_mvpa_ridge_workspace *w,char *err,int nerr)
{
   THD_mvpa_frozen_ridge *m;const THD_nuisance *z;int p,q,ns,i;
   if(!w || w->regression || !w->plan || w->plan->kind!=THD_CV_FIXED || w->plan->nfold!=1 || !w->ridge || !w->ridge->fitted || w->prep->method==THD_PREP_RUN_CENTER){
      pp_error(err,nerr,"frozen ridge v1 needs one fitted fixed partition and none/zscore/sample_center scaling");return NULL;}
   p=w->prep->nfeature;q=w->ridge->q;z=w->prep->nuisance;
   ns=w->selection?w->selection->count[0]:p;
   m=frozen_alloc(p,q,z?z->k:0,w->prep->method,ns,w->pca!=NULL,w->pca?w->pca->model->count:0);
   if(!m){pp_error(err,nerr,"cannot allocate frozen ridge pipeline");return NULL;}
   if(m->width!=w->ridge->p){THD_mvpa_frozen_free(m);pp_error(err,nerr,"frozen ridge pipeline dimensions disagree");return NULL;}
   for(i=0;i<p;i++){
      m->constant[i]=w->prep->method==THD_PREP_FEATURE_ZSCORE?w->prep->constant[i]:0;
      m->center[i]=w->prep->method==THD_PREP_FEATURE_ZSCORE?w->prep->center[i]:0;
      m->sd[i]=w->prep->method==THD_PREP_FEATURE_ZSCORE?w->prep->scale[i]:1;
   }
   for(i=0;i<ns;i++)m->index[i]=w->selection?w->selection->indices[i]:i;
   if(z){memcpy(m->zmean,z->mean,sizeof(double)*m->k);memcpy(m->zsd,z->scale,sizeof(double)*m->k);
      memcpy(m->zbeta,z->beta,sizeof(double)*(size_t)m->k*p);memcpy(m->xmean,z->xmean,sizeof(double)*p);}
   if(w->pca){memcpy(m->pcmean,w->pca->model->mean,sizeof(double)*ns);
      memcpy(m->loading,w->pca->model->loading,sizeof(double)*(size_t)m->ncomp*ns);}
   memcpy(m->xm,w->ridge->xm,sizeof(double)*m->width);memcpy(m->ym,w->ridge->ym,sizeof(double)*q);
   memcpy(m->coef,w->ridge->coef,sizeof(double)*(size_t)m->width*q);
   for(i=0;i<q;i++)m->lambda[i]=w->response_ridge?w->response_ridge->selected[i]:w->selected_lambda[0];
   return m;
}
int THD_mvpa_frozen_predict(const THD_mvpa_frozen_ridge *m,const float *x,int n,int p,
 const float *z,int k,float *out,char *err,int nerr)
{
   float *row=NULL,*sel=NULL,*model=NULL;int i,j,c,o;double mean;
   if(!m || !x || !out || n<1 || p!=m->p || k!=m->k || (k && !z) || out==x || (z && out==z) ||
      (size_t)n>SIZE_MAX/sizeof(float)/(size_t)(p>m->q?p:m->q))return pp_error(err,nerr,"saved model input/covariate dimensions do not match");
   row=malloc(sizeof(float)*(size_t)p);sel=malloc(sizeof(float)*(size_t)m->ns);model=malloc(sizeof(float)*(size_t)m->width);
   if(!row || !sel || !model){pp_error(err,nerr,"cannot allocate saved-model prediction scratch");goto bad;}
   for(i=0;i<n;i++){
      for(c=0;c<k;c++)if(!isfinite(z[(size_t)i*k+c])){pp_error(err,nerr,"nonfinite saved-model nuisance input");goto bad;}
      mean=0;
      for(j=0;j<p;j++){
         double raw=x[(size_t)i*p+j],value=raw,fit=m->xmean[j],mag=fabs(raw)+fabs(fit);
         if(!isfinite(raw)){pp_error(err,nerr,"nonfinite saved-model neural input");goto bad;}
         if(k){
            for(c=0;c<k;c++){double term=((double)z[(size_t)i*k+c]-m->zmean[c])/m->zsd[c]*m->zbeta[(size_t)c*p+j];fit+=term;mag+=fabs(term);}
            value=fabs(raw-fit)<=64*DBL_EPSILON*fmax(1,mag)?0:raw-fit;
         }
         if(!isfinite(value) || fabs(value)>FLT_MAX){pp_error(err,nerr,"saved-model nuisance residual exceeds float range");goto bad;}
         row[j]=(float)value;mean+=row[j];
      }
      mean/=p;
      for(j=0;j<p;j++){
         double v=row[j];
         if(m->scale==THD_PREP_FEATURE_ZSCORE)v=m->constant[j]?0:((double)row[j]-m->center[j])/m->sd[j];
         else if(m->scale==THD_PREP_SAMPLE_CENTER)v-=mean;
         if(!isfinite(v) || fabs(v)>FLT_MAX){pp_error(err,nerr,"saved-model scaling exceeds float range");goto bad;}
         row[j]=(float)v;
      }
      for(j=0;j<m->ns;j++)sel[j]=row[m->index[j]];
      if(!m->pc)memcpy(model,sel,sizeof(float)*(size_t)m->width);
      else if(!m->ncomp)model[0]=0;
      else for(c=0;c<m->ncomp;c++){
         double v=0;for(j=0;j<m->ns;j++)v+=((double)sel[j]-m->pcmean[j])*m->loading[(size_t)c*m->ns+j];
         if(!isfinite(v) || fabs(v)>FLT_MAX){pp_error(err,nerr,"saved-model PCA projection exceeds float range");goto bad;}
         model[c]=(float)v;
      }
      for(o=0;o<m->q;o++){
         double v=m->ym[o];for(j=0;j<m->width;j++)v+=((double)model[j]-m->xm[j])*m->coef[(size_t)j*m->q+o];
         if(!isfinite(v) || fabs(v)>FLT_MAX){pp_error(err,nerr,"saved-model prediction exceeds float range");goto bad;}
         out[(size_t)i*m->q+o]=(float)v;
      }
   }
   free(row);free(sel);free(model);return 1;
bad:free(row);free(sel);free(model);return 0;
}

/* Strict, portable model data. Strings are byte-counted hex tokens; doubles
   use 17 significant digits. No struct dumps, pickle, paths or code execution. */
static int model_token(FILE *fp,char *s,size_t cap)
{
   int c;size_t n=0;do{c=fgetc(fp);}while(c!=EOF && isspace((unsigned char)c));
   if(c==EOF)return 0;
   do{if(n+1>=cap)return 0;s[n++]=(char)c;c=fgetc(fp);}while(c!=EOF && !isspace((unsigned char)c));
   s[n]=0;return 1;
}
static int model_tag(FILE *fp,const char *expected)
{char text[80];return model_token(fp,text,sizeof(text)) && !strcmp(text,expected);}
static int model_int(FILE *fp,int *value)
{
   char text[80],*end;unsigned long v;
   if(!model_token(fp,text,sizeof(text)) || !isdigit((unsigned char)text[0]))return 0;
   errno=0;v=strtoul(text,&end,10);if(errno || *end || v>INT_MAX)return 0;*value=(int)v;return 1;
}
static int model_number(FILE *fp,double *value)
{
   char text[128],*end;double v;
   if(!model_token(fp,text,sizeof(text)))return 0;
   errno=0;v=strtod(text,&end);if(end==text || *end || !isfinite(v) || (errno && v==0))return 0;*value=v;return 1;
}
static int model_room(FILE *fp,size_t values)
{
   struct stat st;off_t pos=ftello(fp);
   return pos>=0 && !fstat(fileno(fp),&st) && st.st_size>=pos && values<=(size_t)(st.st_size-pos)/2;
}
static void model_put_string(FILE *fp,const char *s)
{size_t i,n=strlen(s);fprintf(fp,"%zu ",n);for(i=0;i<n;i++)fprintf(fp,"%02x",(unsigned char)s[i]);fputc('\n',fp);}
static int model_hex(int c)
{if(c>='0' && c<='9')return c-'0';if(c>='a' && c<='f')return c-'a'+10;return -1;}
static char *model_string(FILE *fp)
{
   char *s;int n,i,a,b,c;
   if(!model_int(fp,&n) || n<1 || n>65535 || !model_room(fp,(size_t)n))return NULL;
   s=malloc((size_t)n+1);if(!s)return NULL;
   for(i=0;i<n;i++){a=model_hex(fgetc(fp));b=model_hex(fgetc(fp));if(a<0 || b<0 || (a==0 && b==0)){free(s);return NULL;}s[i]=(char)(16*a+b);}
   c=fgetc(fp);if(c==EOF || !isspace((unsigned char)c)){free(s);return NULL;}s[n]=0;return s;
}
static int model_array(FILE *fp,const char *tag,double *a,size_t n,int write)
{
   size_t i;
   if(write){fprintf(fp,"%s\n",tag);for(i=0;i<n;i++)fprintf(fp,"%.17g%c",a[i],(i+1)%8?' ':'\n');fputc('\n',fp);return !ferror(fp);}
   if(!model_tag(fp,tag) || !model_room(fp,n))return 0;
   for(i=0;i<n;i++)if(!model_number(fp,a+i))return 0;return 1;
}
static int model_int_array(FILE *fp,const char *tag,int *a,size_t n,int write)
{
   size_t i;if(write){fprintf(fp,"%s\n",tag);for(i=0;i<n;i++)fprintf(fp,"%d ",a[i]);fputc('\n',fp);return !ferror(fp);}
   if(!model_tag(fp,tag) || !model_room(fp,n))return 0;
   for(i=0;i<n;i++)if(!model_int(fp,a+i))return 0;return 1;
}
static int frozen_arrays(FILE *fp,THD_mvpa_frozen_ridge *m,int write)
{
   return model_int_array(fp,"INDEX",m->index,m->ns,write) && model_int_array(fp,"CONSTANT",m->constant,m->p,write) &&
      model_array(fp,"CENTER",m->center,m->p,write) && model_array(fp,"SCALE",m->sd,m->p,write) &&
      model_array(fp,"ZMEAN",m->zmean,m->k,write) && model_array(fp,"ZSCALE",m->zsd,m->k,write) &&
      model_array(fp,"ZBETA",m->zbeta,(size_t)m->k*m->p,write) && model_array(fp,"XMEAN",m->xmean,m->p,write) &&
      model_array(fp,"PCAMEAN",m->pcmean,m->ns,write) && model_array(fp,"LOADINGS",m->loading,(size_t)m->ncomp*m->ns,write) &&
      model_array(fp,"RIDGEXMEAN",m->xm,m->width,write) && model_array(fp,"RIDGEYMEAN",m->ym,m->q,write) &&
      model_array(fp,"COEFFICIENTS",m->coef,(size_t)m->width*m->q,write) && model_array(fp,"LAMBDA",m->lambda,m->q,write);
}
static int model_int_order(const void *a,const void *b)
{int x=*(const int *)a,y=*(const int *)b;return (x>y)-(x<y);}
static int model_unique_indices(const int *a,int n,int bound)
{
   int *copy,i,ok=1;if(n<1)return 0;
   copy=malloc(sizeof(int)*(size_t)n);if(!copy)return 0;
   for(i=0;i<n;i++){if(a[i]<0 || a[i]>=bound){free(copy);return 0;}copy[i]=a[i];}
   qsort(copy,(size_t)n,sizeof(int),model_int_order);for(i=1;i<n;i++)if(copy[i]==copy[i-1])ok=0;
   free(copy);return ok;
}
static int model_labels(char **labels,int n)
{
   int i,j;for(i=0;i<n;i++){
      if(!labels || !labels[i] || !*labels[i] || strlen(labels[i])>65535)return 0;
      for(j=0;j<i;j++)if(!strcmp(labels[i],labels[j]))return 0;
   }return 1;
}
static int model_validate(const THD_mvpa_model *m)
{
   int i,j,*ids,nvox;size_t nv;double det;
   if(!m || m->classification<0 || m->classification>1 || m->searchlight<0 || m->searchlight>1 ||
      m->nunit<1 || !m->unit || m->nresponse<(m->classification?2:1) || m->ncovariate<0 || m->ntrain<1 ||
      (m->ncovariate && m->ncovariate>=m->ntrain-1) ||
      m->view<0 || m->view>2 || !m->space || !*m->space || !m->target || !*m->target ||
      strlen(m->space)>65535 || strlen(m->target)>65535 || !model_labels(m->response,m->nresponse) || !model_labels(m->covariate,m->ncovariate))return 0;
   nv=1;for(i=0;i<3;i++){if(m->dim[i]<1 || nv>(size_t)INT_MAX/m->dim[i])return 0;nv*=m->dim[i];}nvox=(int)nv;
   for(i=0;i<12;i++)if(!isfinite(m->affine[i]))return 0;
   det=m->affine[0]*(m->affine[5]*m->affine[10]-m->affine[6]*m->affine[9])-m->affine[1]*(m->affine[4]*m->affine[10]-m->affine[6]*m->affine[8])+m->affine[2]*(m->affine[4]*m->affine[9]-m->affine[5]*m->affine[8]);
   if(!isfinite(det) || fabs(det)<1e-12)return 0;
   ids=malloc(sizeof(int)*(size_t)m->nunit);if(!ids)return 0;
   for(i=0;i<m->nunit;i++)ids[i]=m->unit[i].id;
   j=model_unique_indices(ids,m->nunit,INT_MAX);free(ids);if(!j)return 0;
   for(i=0;i<m->nunit;i++){
      const THD_mvpa_model_unit *u=m->unit+i;const THD_mvpa_frozen_ridge *a=u->fit;
      if(!a || !u->label || !*u->label || strlen(u->label)>65535 || !u->feature || u->nfeature<1 || u->nfeature>nvox ||
         a->p!=u->nfeature || a->q!=m->nresponse || a->k!=m->ncovariate || a->scale<0 || a->scale>THD_PREP_SAMPLE_CENTER ||
         a->ns<1 || a->ns>a->p || a->pc<0 || a->pc>1 || a->ncomp<0 || a->ncomp>a->ns || (!a->pc && a->ncomp) ||
         a->width!=(a->pc?(a->ncomp?a->ncomp:1):a->ns) || (a->pc && (m->ntrain<2 || a->ncomp>=m->ntrain)) || !model_unique_indices(u->feature,u->nfeature,nvox) || !model_unique_indices(a->index,a->ns,a->p))return 0;
      for(j=0;j<a->p;j++)if(a->constant[j]<0 || a->constant[j]>1 || !isfinite(a->sd[j]) || a->sd[j]<=0)return 0;
      for(j=0;j<a->k;j++)if(!isfinite(a->zsd[j]) || a->zsd[j]<=0)return 0;
      for(j=0;j<a->q;j++)if(!isfinite(a->lambda[j]) || a->lambda[j]<=0)return 0;
   }return 1;
}
void THD_mvpa_model_free(THD_mvpa_model *m)
{
   int i;if(!m)return;
   free(m->space);free(m->target);
   if(m->response)for(i=0;i<m->nresponse;i++)free(m->response[i]);free(m->response);
   if(m->covariate)for(i=0;i<m->ncovariate;i++)free(m->covariate[i]);free(m->covariate);
   if(m->unit)for(i=0;i<m->nunit;i++){free(m->unit[i].label);free(m->unit[i].feature);THD_mvpa_frozen_free(m->unit[i].fit);}
   free(m->unit);free(m);
}
int THD_mvpa_model_write(const char *path,const THD_mvpa_model *m,int overwrite,char *err,int nerr)
{
   char *temp;FILE *fp;int fd,i,ok=1;
   if(!path || !*path || !model_validate(m))return pp_error(err,nerr,"invalid saved-model metadata or pipeline");
   temp=malloc(strlen(path)+16);if(!temp)return pp_error(err,nerr,"cannot allocate model temporary path");
   snprintf(temp,strlen(path)+16,"%s.tmp.XXXXXX",path);fd=mkstemp(temp);
   if(fd<0){free(temp);return pp_error(err,nerr,"cannot create saved-model temporary file");}
   fp=fdopen(fd,"w");if(!fp){close(fd);unlink(temp);free(temp);return pp_error(err,nerr,"cannot open saved-model stream");}
   fprintf(fp,"AFNI_MVPA_MODEL 1\nMETA %d %d %d %d %d %d\nGRID %d %d %d %d\n",m->classification,m->searchlight,m->nunit,m->nresponse,m->ncovariate,m->ntrain,m->dim[0],m->dim[1],m->dim[2],m->view);
   model_array(fp,"AFFINE",(double *)m->affine,12,1);fputs("SPACE\n",fp);model_put_string(fp,m->space);fputs("TARGET\n",fp);model_put_string(fp,m->target);
   fputs("RESPONSES\n",fp);for(i=0;i<m->nresponse;i++)model_put_string(fp,m->response[i]);
   fputs("COVARIATES\n",fp);for(i=0;i<m->ncovariate;i++)model_put_string(fp,m->covariate[i]);
   for(i=0;i<m->nunit;i++){
      const THD_mvpa_model_unit *u=m->unit+i;THD_mvpa_frozen_ridge *a=u->fit;
      fprintf(fp,"UNIT %d %d\n",u->id,u->nfeature);model_put_string(fp,u->label);model_int_array(fp,"FEATURES",u->feature,u->nfeature,1);
      fprintf(fp,"PIPELINE %d %d %d %d %d %d %d\n",a->p,a->q,a->k,a->scale,a->ns,a->pc,a->ncomp);
      if(!frozen_arrays(fp,a,1))ok=0;fputs("END_UNIT\n",fp);
   }
   fputs("END_MODEL\n",fp);if(ferror(fp))ok=0;if(fclose(fp))ok=0;
   if(ok){if(overwrite){if(rename(temp,path))ok=0;}else if(link(temp,path))ok=0;}
   unlink(temp);free(temp);
   return ok?1:pp_error(err,nerr,"cannot write/publish saved model (existing destination without overwrite, or I/O error)");
}
THD_mvpa_model *THD_mvpa_model_read(const char *path,char *err,int nerr)
{
   FILE *fp=fopen(path,"r");THD_mvpa_model *m=NULL;int v,i,j,ok=0;
   if(!fp){pp_error(err,nerr,"cannot open saved model '%s'",path);return NULL;}
   if(!model_tag(fp,"AFNI_MVPA_MODEL") || !model_int(fp,&v) || v!=1){pp_error(err,nerr,"unsupported saved-model format/version");fclose(fp);return NULL;}
   m=calloc(1,sizeof(*m));if(!m)goto done;
   if(!model_tag(fp,"META") || !model_int(fp,&m->classification) || !model_int(fp,&m->searchlight) || !model_int(fp,&m->nunit) ||
      !model_int(fp,&m->nresponse) || !model_int(fp,&m->ncovariate) || !model_int(fp,&m->ntrain) ||
      m->nunit<1 || m->nresponse<1 || !model_room(fp,(size_t)m->nunit*10+(size_t)m->nresponse+m->ncovariate))goto done;
   if(!model_tag(fp,"GRID"))goto done;
   for(i=0;i<3;i++)if(!model_int(fp,m->dim+i))goto done;
   if(!model_int(fp,&m->view) || !model_array(fp,"AFFINE",m->affine,12,0) || !model_tag(fp,"SPACE") || !(m->space=model_string(fp)) ||
      !model_tag(fp,"TARGET") || !(m->target=model_string(fp)))goto done;
   m->response=calloc((size_t)m->nresponse,sizeof(char *));m->covariate=calloc(m->ncovariate?(size_t)m->ncovariate:1,sizeof(char *));m->unit=calloc((size_t)m->nunit,sizeof(*m->unit));
   if(!m->response || !m->covariate || !m->unit || !model_tag(fp,"RESPONSES"))goto done;
   for(i=0;i<m->nresponse;i++)if(!(m->response[i]=model_string(fp)))goto done;
   if(!model_tag(fp,"COVARIATES"))goto done;
   for(i=0;i<m->ncovariate;i++)if(!(m->covariate[i]=model_string(fp)))goto done;
   for(i=0;i<m->nunit;i++){
      THD_mvpa_model_unit *u=m->unit+i;int d[7];size_t cells,width;
      if(!model_tag(fp,"UNIT") || !model_int(fp,&u->id) || !model_int(fp,&u->nfeature) || u->nfeature<1 || !model_room(fp,u->nfeature) || !(u->label=model_string(fp)))goto done;
      u->feature=malloc(sizeof(int)*(size_t)u->nfeature);if(!u->feature || !model_int_array(fp,"FEATURES",u->feature,u->nfeature,0) || !model_tag(fp,"PIPELINE"))goto done;
      for(j=0;j<7;j++)if(!model_int(fp,d+j))goto done;
      if(d[0]!=u->nfeature || d[1]!=m->nresponse || d[2]!=m->ncovariate || d[3]>THD_PREP_SAMPLE_CENTER || d[4]<1 || d[4]>d[0] || d[5]>1 || d[6]>d[4] || (!d[5] && d[6]))goto done;
      width=d[5]?(d[6]?d[6]:1):d[4];
      /* Check each product against available file bytes before any large allocation. */
      if(!model_room(fp,(size_t)d[0]*d[2]) || !model_room(fp,(size_t)d[4]*d[6]) || !model_room(fp,width*d[1]))goto done;
      cells=(size_t)d[0]*4+(size_t)d[2]*2+(size_t)d[0]*d[2]+(size_t)d[4]*2+(size_t)d[4]*d[6]+width+2*(size_t)d[1]+width*d[1];
      if(!model_room(fp,cells) || !(u->fit=frozen_alloc(d[0],d[1],d[2],d[3],d[4],d[5],d[6])) || !frozen_arrays(fp,u->fit,0) || !model_tag(fp,"END_UNIT"))goto done;
   }
   if(!model_tag(fp,"END_MODEL"))goto done;
   /* Whitespace only after the final sentinel; reject even an overlong token. */
   do{j=fgetc(fp);}while(j!=EOF && isspace((unsigned char)j));
   if(j!=EOF || ferror(fp) || !model_validate(m))goto done;
   ok=1;
done:if(fclose(fp))ok=0;
   if(!ok){THD_mvpa_model_free(m);pp_error(err,nerr,"malformed/truncated saved model, invalid feature/label/pipeline domain, or allocation failure");return NULL;}
   return m;
}

int THD_mvpa_run_ridge_permutations(THD_mvpa_ridge_workspace *w,
 const float *x,int p,const float *y,int q,const int *group,double lambda,
 const PERM_set *pset,int shuffle_predictors,int metric,int aggregate,
 float *permuted,float *null_score,char *err,int nerr)
{
   int ip,i,o,n,nout,width;const float *attr;char why[512];
   if(!w || !x || !y || p<1 || p>w->max_feature || q<1 || q>w->max_output ||
      !pset || pset->nobs!=w->plan->nsample || pset->nperm<1 ||
      !pset->perm || !pset->sign || !permuted || !null_score ||
      metric<THD_METRIC_PEARSON || metric>THD_METRIC_RMSE)
      return pp_error(err,nerr,"ridge permutations need matching folds, relabelings, continuous metric, and scratch");
   n=pset->nobs;nout=aggregate?1:q;width=shuffle_predictors?p:q;attr=shuffle_predictors?x:y;
   for(ip=0;ip<pset->nperm;ip++){
      const int *perm=pset->perm+(size_t)ip*n;
      memset(w->index_scratch,0,sizeof(int)*(size_t)n);
      for(i=0;i<n;i++){
         int src=perm[i];
         if(src<0 || src>=n || w->index_scratch[src] || pset->sign[(size_t)ip*n+i]!=1 || (ip==0 && src!=i))
            return pp_error(err,nerr,"invalid continuous row relabeling %d at sample %d; slot zero must be identity",ip+1,i+1);
         w->index_scratch[src]=1;
         memcpy(permuted+(size_t)i*width,attr+(size_t)src*width,sizeof(float)*(size_t)width);
      }
      if(!nuisance_permutation(w->prep,perm,n,err,nerr))return 0;
      if(!THD_mvpa_run_ridge_cv(w,shuffle_predictors?permuted:x,p,
            shuffle_predictors?y:permuted,q,group,lambda,why,sizeof(why)))
         return pp_error(err,nerr,"continuous row relabeling %d: %s",ip+1,why);
      for(o=0;o<nout;o++){
         double sum=0;int j,nvalid=0;
         for(j=aggregate?0:o;j<(aggregate?q:o+1);j++){
            const THD_mvpa_continuous_score *s=w->score+j;double v;
            switch(metric){
               case THD_METRIC_PEARSON:v=s->pearson;break;
               case THD_METRIC_SPEARMAN:v=s->spearman;break;
               case THD_METRIC_CROSS_VALIDATED_R2:v=s->cv_r2;break;
               case THD_METRIC_MAE:v=s->mae;break;
               default:v=s->rmse;break;
            }
            if(isfinite(v)){sum+=v;nvalid++;}
         }
         if(nvalid && (!isfinite(sum/nvalid) || fabs(sum/nvalid)>FLT_MAX))
            return pp_error(err,nerr,"continuous permutation score exceeds float range");
         null_score[(size_t)ip*nout+o]=nvalid?(float)(sum/nvalid):NAN;
      }
   }
   if(!THD_mvpa_run_ridge_cv(w,x,p,y,q,group,lambda,why,sizeof(why)))
      return pp_error(err,nerr,"restoring observed continuous analysis: %s",why);
   return 1;
}

/* Native L2-regularized, squared-hinge linear SVM. Each one-vs-rest problem
   minimizes .5*(||w||^2+b^2) + C*sum(max(0,1-y*(Xw+b))^2).
   Appending a constant 1 gives a penalized intercept. The nonnegative dual
   has Hessian y_i*y_j*(x_i.x_j+1) + I/(2*C). Exact cyclic coordinate updates
   maintain w,b; an independent final-sweep KKT check certifies convergence.
   No kernel matrix, random state, warm starts or per-fit allocations. */
struct THD_svm_model {
   int maxn,maxp,nclass,p,fitted,max_iterations;
   double residual;
   double *weight,*alpha,*diagonal;
   int *count;
};

void THD_svm_free(THD_svm_model *m)
{
   if(!m)return;
   free(m->weight);free(m->alpha);free(m->diagonal);free(m->count);free(m);
}

THD_svm_model *THD_svm_alloc(int n,int p,int nc,char *err,int nerr)
{
   THD_svm_model *m;
   if(n<2 || p<1 || p==INT_MAX || nc<2 ||
      (size_t)(p+1)>SIZE_MAX/sizeof(double)/(size_t)nc){
      pp_error(err,nerr,"SVM needs samples, features, at least two classes and bounded dimensions");return NULL;
   }
   m=calloc(1,sizeof(*m));
   if(!m){pp_error(err,nerr,"cannot allocate SVM");return NULL;}
   m->maxn=n;m->maxp=p;m->nclass=nc;
   m->weight=malloc(sizeof(double)*(size_t)(p+1)*nc);
   m->alpha=malloc(sizeof(double)*(size_t)n);m->diagonal=malloc(sizeof(double)*(size_t)n);
   m->count=malloc(sizeof(int)*(size_t)nc);
   if(!m->weight || !m->alpha || !m->diagonal || !m->count){
      THD_svm_free(m);pp_error(err,nerr,"cannot allocate SVM buffers");return NULL;
   }
   return m;
}

int THD_svm_fit(THD_svm_model *m,const float *x,const int *classes,int n,int p,
 double cost,double tol,int maxiter,char *err,int nerr)
{
   int i,j,c,iteration,constant=1;double penalty=.5/cost;
   if(m)m->fitted=0;
   if(!m || !x || !classes || n<2 || n>m->maxn || p<1 || p>m->maxp ||
      !isfinite(cost) || cost<=0 || !isfinite(penalty) || penalty<=0 ||
      !isfinite(tol) || tol<1e-12 || tol>1e-2 || maxiter<1)
      return pp_error(err,nerr,"invalid SVM dimensions, positive C, tolerance [1e-12,1e-2], or iteration limit");
   memset(m->count,0,sizeof(int)*(size_t)m->nclass);
   for(i=0;i<n;i++){
      double d=1+penalty;
      if(classes[i]<0 || classes[i]>=m->nclass)return pp_error(err,nerr,"SVM class index is invalid");
      m->count[classes[i]]++;
      for(j=0;j<p;j++){
         double v=x[(size_t)i*p+j];
         if(!isfinite(v))return pp_error(err,nerr,"SVM predictor is not finite");
         d+=v*v;
         if(i>0 && v!=x[j])constant=0;
      }
      m->diagonal[i]=d;
   }
   for(c=0;c<m->nclass;c++)if(!m->count[c])return pp_error(err,nerr,"SVM training set has no class %d",c+1);
   memset(m->weight,0,sizeof(double)*(size_t)(p+1)*m->nclass);
   m->max_iterations=0;m->residual=0;
   if(constant){
      /* All rows share z=[x,1]. The minimum-norm weight is z*f/||z||^2,
         where f=(npos-nneg)/(n+1/(2*C*||z||^2)). Both labels are present,
         hence |f|<1 and all rows lie in the quadratic hinge region. This
         exact reduction avoids very slow dual cancellation at large C. */
      double norm=1;
      for(j=0;j<p;j++)norm+=(double)x[j]*x[j];
      for(c=0;c<m->nclass;c++){
         double margin=(2.0*m->count[c]-n)/(n+penalty/norm);
         double *weight=m->weight+(size_t)c*(p+1),s;
         for(j=0;j<p;j++)weight[j]=x[j]*margin/norm;
         weight[p]=margin/norm;
         s=weight[p];for(j=0;j<p;j++)s+=weight[j]*x[j];
         for(i=0;i<n;i++){
            double y=classes[i]==c?1:-1,alpha=(1-y*margin)/penalty;
            double residual=fabs(y*s-1+penalty*alpha);
            if(!isfinite(alpha) || !isfinite(residual) || residual>tol)
               return pp_error(err,nerr,"constant-pattern SVM solution exceeds numeric precision; reduce C or rescale");
            if(residual>m->residual)m->residual=residual;
         }
      }
      m->max_iterations=1;m->p=p;m->fitted=1;return 1;
   }
   for(c=0;c<m->nclass;c++){
      double *weight=m->weight+(size_t)c*(p+1),residual=HUGE_VAL;
      memset(m->alpha,0,sizeof(double)*(size_t)n);
      for(iteration=0;iteration<maxiter;iteration++){
         for(i=0;i<n;i++){
            double y=classes[i]==c?1:-1,s=weight[p],gradient,alpha,delta;
            for(j=0;j<p;j++)s+=weight[j]*x[(size_t)i*p+j];
            gradient=y*s-1+penalty*m->alpha[i];
            alpha=fmax(0,m->alpha[i]-gradient/m->diagonal[i]);
            delta=(alpha-m->alpha[i])*y;m->alpha[i]=alpha;
            if(!isfinite(alpha) || !isfinite(gradient))return pp_error(err,nerr,"SVM iterate is not finite; rescale predictors or reduce C");
            for(j=0;j<p;j++)weight[j]+=delta*x[(size_t)i*p+j];
            weight[p]+=delta;
         }
         residual=0;
         for(i=0;i<n;i++){
            double y=classes[i]==c?1:-1,s=weight[p],gradient,pg;
            for(j=0;j<p;j++)s+=weight[j]*x[(size_t)i*p+j];
            gradient=y*s-1+penalty*m->alpha[i];
            if(!isfinite(gradient))return pp_error(err,nerr,"SVM convergence residual is not finite");
            pg=m->alpha[i]>0?fabs(gradient):fmax(0,-gradient);
            if(pg>residual)residual=pg;
         }
         if(residual<=tol)break;
      }
      if(iteration==maxiter)return pp_error(err,nerr,"SVM class %d did not converge after %d sweeps (KKT residual %.9g > %.9g); increase -svm_max_iter, rescale, or reduce C",c+1,maxiter,residual,tol);
      if(iteration+1>m->max_iterations)m->max_iterations=iteration+1;
      if(residual>m->residual)m->residual=residual;
   }
   m->p=p;m->fitted=1;return 1;
}

int THD_svm_predict(const THD_svm_model *m,const float *x,int n,int p,
 int *prediction,double *decision,char *err,int nerr)
{
   int i,j,c;
   if(!m || !m->fitted || !x || !prediction || n<1 || p!=m->p)
      return pp_error(err,nerr,"SVM prediction needs a fitted model and matching rows");
   for(i=0;i<n;i++){
      double bestscore=-HUGE_VAL;int best=0;
      for(j=0;j<p;j++)if(!isfinite(x[(size_t)i*p+j]))return pp_error(err,nerr,"SVM prediction input is not finite");
      for(c=0;c<m->nclass;c++){
         const double *weight=m->weight+(size_t)c*(p+1);double s=weight[p];
         for(j=0;j<p;j++)s+=weight[j]*x[(size_t)i*p+j];
         if(!isfinite(s))return pp_error(err,nerr,"SVM decision is not finite");
         if(decision)decision[(size_t)i*m->nclass+c]=s;
         if(s>bestscore){bestscore=s;best=c;}
      }
      prediction[i]=best;
   }
   return 1;
}

/* Joint multinomial logistic regression. Workspace is reused across folds and
   permutations; all numerical state is reset for every fit. */
#define LOGISTIC_HISTORY 7
struct THD_logistic_model {
   int max_sample,max_feature,nclass,nfeature,fitted,iterations;
   double residual,*theta,*gradient,*trial,*trial_gradient,*direction;
   double *s,*y,*logits,*prob;int *count;
};
void THD_logistic_free(THD_logistic_model *m)
{
   if(!m)return;
   free(m->theta);free(m->gradient);free(m->trial);free(m->trial_gradient);
   free(m->direction);free(m->s);free(m->y);free(m->logits);free(m->prob);free(m->count);free(m);
}
THD_logistic_model *THD_logistic_alloc(int n,int p,int nc,char *err,int nerr)
{
   THD_logistic_model *m;size_t d;
   if(n<2 || p<1 || nc<2 || p==INT_MAX ||
      (size_t)(p+1)>SIZE_MAX/sizeof(double)/LOGISTIC_HISTORY/(size_t)nc){
      pp_error(err,nerr,"invalid logistic model dimensions");return NULL;
   }
   d=(size_t)(p+1)*nc;m=calloc(1,sizeof(*m));
   if(!m){pp_error(err,nerr,"cannot allocate logistic model");return NULL;}
   m->max_sample=n;m->max_feature=p;m->nclass=nc;
   m->theta=calloc(d,sizeof(double));m->gradient=malloc(d*sizeof(double));
   m->trial=malloc(d*sizeof(double));m->trial_gradient=malloc(d*sizeof(double));m->direction=malloc(d*sizeof(double));
   m->s=malloc(d*LOGISTIC_HISTORY*sizeof(double));m->y=malloc(d*LOGISTIC_HISTORY*sizeof(double));
   m->logits=malloc((size_t)nc*sizeof(double));m->prob=malloc((size_t)nc*sizeof(double));m->count=malloc((size_t)nc*sizeof(int));
   if(!m->theta || !m->gradient || !m->trial || !m->trial_gradient || !m->direction || !m->s || !m->y || !m->logits || !m->prob || !m->count){
      THD_logistic_free(m);pp_error(err,nerr,"cannot allocate logistic solver buffers");return NULL;
   }return m;
}
/* Subtract the maximum before exponentiating. The true-class log loss uses
   the logit difference directly, even when its probability underflows to 0. */
int THD_mvpa_probability_score(const double *z,int nc,int truth,double *ll,double *br,char *err,int nerr)
{
   double mx,sum=0,b=0;int c;
   if(!z || !ll || !br || nc<2 || truth<0 || truth>=nc)return pp_error(err,nerr,"invalid probability scoring input");
   mx=z[0];for(c=0;c<nc;c++){if(!isfinite(z[c]))return pp_error(err,nerr,"nonfinite class logit");if(z[c]>mx)mx=z[c];}
   for(c=0;c<nc;c++)sum+=exp(z[c]-mx);
   for(c=0;c<nc;c++){double r=exp(z[c]-mx)/sum-(c==truth);b+=r*r;}
   *ll=(mx-z[truth])+log(sum);*br=b;
   return isfinite(*ll)?1:pp_error(err,nerr,"log loss exceeds numerical range");
}
static double logistic_value(THD_logistic_model *m,const float *x,const int *cls,
 int n,int p,double lambda,const double *b,double *g)
{
   int i,j,c,k=m->nclass;size_t d=(size_t)(p+1)*k,a;double value=0;
   memset(g,0,d*sizeof(double));
   for(i=0;i<n;i++){
      double mx=-DBL_MAX,sum=0;
      for(c=0;c<k;c++){
         double z=b[(size_t)p*k+c];
         for(j=0;j<p;j++)z+=x[(size_t)i*p+j]*b[(size_t)j*k+c];
         if(!isfinite(z))return INFINITY;
         m->logits[c]=z;if(z>mx)mx=z;
      }
      for(c=0;c<k;c++)sum+=(m->prob[c]=exp(m->logits[c]-mx));
      value+=((mx-m->logits[cls[i]])+log(sum))/n;
      for(c=0;c<k;c++){
         double r=(m->prob[c]/sum-(c==cls[i]))/n;
         g[(size_t)p*k+c]+=r;
         for(j=0;j<p;j++)g[(size_t)j*k+c]+=r*x[(size_t)i*p+j];
      }
   }
   for(a=0;a<(size_t)p*k;a++){value+=.5*lambda*b[a]*b[a];g[a]+=lambda*b[a];}
   for(a=0;a<d;a++)if(!isfinite(g[a]))return INFINITY;
   return value;
}
static double logistic_dot(const double *a,const double *b,size_t n)
{size_t i;double v=0;for(i=0;i<n;i++)v+=a[i]*b[i];return v;}
int THD_logistic_fit(THD_logistic_model *m,const float *x,const int *cls,
 int n,int p,double lambda,double tol,int maxiter,char *err,int nerr)
{
   size_t a,d;int i,j,c,iter,nh=0,next=0;double value,rho[LOGISTIC_HISTORY],alpha[LOGISTIC_HISTORY];
   if(m){m->fitted=0;m->iterations=0;m->residual=INFINITY;}
   if(!m || !x || !cls || n<2 || n>m->max_sample || p<1 || p>m->max_feature ||
      !isfinite(lambda) || lambda<=0 || !isfinite(tol) || tol<1e-12 || tol>1e-2 || maxiter<1)
      return pp_error(err,nerr,"invalid logistic inputs, positive lambda or solver options");
   d=(size_t)(p+1)*m->nclass;memset(m->count,0,(size_t)m->nclass*sizeof(int));
   for(i=0;i<n;i++){
      if(cls[i]<0 || cls[i]>=m->nclass)return pp_error(err,nerr,"invalid logistic class ID");
      m->count[cls[i]]++;
      for(j=0;j<p;j++)if(!isfinite(x[(size_t)i*p+j]))return pp_error(err,nerr,"nonfinite logistic training feature");
   }
   memset(m->theta,0,d*sizeof(double));
   {double mean=0;for(c=0;c<m->nclass;c++){
      if(!m->count[c])return pp_error(err,nerr,"logistic training has no class %d",c+1);
      mean+=log((double)m->count[c]/n)/m->nclass;
   }
   for(c=0;c<m->nclass;c++)m->theta[(size_t)p*m->nclass+c]=log((double)m->count[c]/n)-mean;}
   value=logistic_value(m,x,cls,n,p,lambda,m->theta,m->gradient);
   if(!isfinite(value))return pp_error(err,nerr,"nonfinite initial logistic objective");
   for(iter=0;iter<=maxiter;iter++){
      double slope,step=1,newvalue=INFINITY;int ls;
      m->residual=0;for(a=0;a<d;a++)if(fabs(m->gradient[a])>m->residual)m->residual=fabs(m->gradient[a]);
      m->iterations=iter;
      if(m->residual<=tol){m->nfeature=p;m->fitted=1;return 1;}
      if(iter==maxiter)break;
      memcpy(m->direction,m->gradient,d*sizeof(double));
      for(i=0;i<nh;i++){
         int h=(next+LOGISTIC_HISTORY-1-i)%LOGISTIC_HISTORY;
         double *s=m->s+(size_t)h*d,*y=m->y+(size_t)h*d;
         alpha[h]=rho[h]*logistic_dot(s,m->direction,d);
         for(a=0;a<d;a++)m->direction[a]-=alpha[h]*y[a];
      }
      if(nh){int h=(next+LOGISTIC_HISTORY-1)%LOGISTIC_HISTORY;
         double *s=m->s+(size_t)h*d,*y=m->y+(size_t)h*d;
         double scale=logistic_dot(s,y,d)/logistic_dot(y,y,d);
         for(a=0;a<d;a++)m->direction[a]*=scale;
      }
      for(i=nh-1;i>=0;i--){
         int h=(next+LOGISTIC_HISTORY-1-i)%LOGISTIC_HISTORY;
         double *s=m->s+(size_t)h*d,*y=m->y+(size_t)h*d;
         double beta=rho[h]*logistic_dot(y,m->direction,d);
         for(a=0;a<d;a++)m->direction[a]+=s[a]*(alpha[h]-beta);
      }
      for(a=0;a<d;a++)m->direction[a]=-m->direction[a];
      slope=logistic_dot(m->gradient,m->direction,d);
      if(!isfinite(slope) || slope>=0){
         nh=next=0;for(a=0;a<d;a++)m->direction[a]=-m->gradient[a];
         slope=-logistic_dot(m->gradient,m->gradient,d);
      }
      for(ls=0;ls<80;ls++,step*=.5){
         for(a=0;a<d;a++)m->trial[a]=m->theta[a]+step*m->direction[a];
         /* Fix the softmax gauge, preserving predictions and lowering the penalty. */
         for(j=0;j<=p;j++){double mean=0;
            for(c=0;c<m->nclass;c++)mean+=m->trial[(size_t)j*m->nclass+c]/m->nclass;
            for(c=0;c<m->nclass;c++)m->trial[(size_t)j*m->nclass+c]-=mean;
         }
         newvalue=logistic_value(m,x,cls,n,p,lambda,m->trial,m->trial_gradient);
         if(isfinite(newvalue)){
            double norm=logistic_dot(m->trial_gradient,m->trial_gradient,d);
            /* At machine precision, objective rounding can obscure a genuine
               descent step. Permit that step only with a smaller gradient;
               the same strict tolerance is still required for convergence. */
            if(newvalue<=value+1e-4*step*slope ||
               (newvalue<=value+8*DBL_EPSILON*(1+fabs(value)) && norm<logistic_dot(m->gradient,m->gradient,d)))break;
         }
      }
      if(ls==80)return pp_error(err,nerr,"logistic line search failed (gradient %.9g); scale features or increase regularization",m->residual);
      {double *s=m->s+(size_t)next*d,*y=m->y+(size_t)next*d,sy,ss,yy;
       for(a=0;a<d;a++){s[a]=m->trial[a]-m->theta[a];y[a]=m->trial_gradient[a]-m->gradient[a];}
       sy=logistic_dot(s,y,d);ss=logistic_dot(s,s,d);yy=logistic_dot(y,y,d);
       if(isfinite(sy) && sy>1e-12*sqrt(ss)*sqrt(yy) && sy>DBL_MIN){rho[next]=1/sy;next=(next+1)%LOGISTIC_HISTORY;if(nh<LOGISTIC_HISTORY)nh++;}
       else nh=next=0;
      }
      memcpy(m->theta,m->trial,d*sizeof(double));memcpy(m->gradient,m->trial_gradient,d*sizeof(double));value=newvalue;
   }
   return pp_error(err,nerr,"logistic did not converge in %d iterations (gradient %.9g > tolerance %.9g)",maxiter,m->residual,tol);
}
int THD_logistic_predict(const THD_logistic_model *m,const float *x,int n,int p,
 int *pred,double *decision,double *prob,char *err,int nerr)
{
   int i,j,c,k;
   if(!m || !m->fitted || !x || !pred || !decision || !prob || n<1 || p!=m->nfeature)
      return pp_error(err,nerr,"logistic prediction needs a fitted model and logits/probability outputs");
   k=m->nclass;
   for(i=0;i<n;i++){
      double mx=-DBL_MAX,sum=0;int best=0;
      for(j=0;j<p;j++)if(!isfinite(x[(size_t)i*p+j]))return pp_error(err,nerr,"nonfinite logistic prediction feature");
      for(c=0;c<k;c++){
         double z=m->theta[(size_t)p*k+c];
         for(j=0;j<p;j++)z+=x[(size_t)i*p+j]*m->theta[(size_t)j*k+c];
         if(!isfinite(z))return pp_error(err,nerr,"nonfinite logistic prediction logit");
         decision[(size_t)i*k+c]=z;if(z>mx){mx=z;best=c;}
      }
      for(c=0;c<k;c++)sum+=(prob[(size_t)i*k+c]=exp(decision[(size_t)i*k+c]-mx));
      for(c=0;c<k;c++)prob[(size_t)i*k+c]/=sum;
      pred[i]=best;
   }return 1;
}

void THD_mvpa_workspace_free( THD_mvpa_workspace *w )
{
   if( w==NULL ) return ;
   THD_mvpa_ridge_workspace_free(w->ridge_cv);free(w->class_response);
   THD_logistic_free(w->logistic);free(w->probability);free(w->test_probability);
   free(w->fold_log_loss);free(w->fold_brier);
   THD_svm_free(w->svm);free(w->class_grid);free(w->class_selected);free(w->class_loss);
   free(w->fold_solver_residual);free(w->fold_solver_iterations);free(w->class_inner_train);free(w->class_inner_test);
   THD_lda_free(w->lda);free(w->fold_covariance_scale);free(w->fold_variance_floor);free(w->fold_covariance_form);
   free(w->train_x) ; free(w->test_x) ;
   free(w->train_class) ; free(w->test_class) ; free(w->test_prediction) ;
   free(w->test_decision) ; free(w->train_count) ;
   free(w->fold_confusion) ; free(w->fold_recall) ;
   free(w->prediction) ; free(w->decision) ; free(w->fold_nsample) ;
   free(w->fold_accuracy) ; free(w->fold_balanced_accuracy) ;
   free(w->confusion) ; free(w->recall) ;
   fs_free(w->selection); pca_free(w->pca); THD_preprocess_free(w->prep) ; THD_centroid_free(w->centroid) ; free(w) ;
}

static THD_mvpa_workspace * workspace_fail( THD_mvpa_workspace *w,
                                             char *err, int nerr,
                                             const char *message )
{
   THD_mvpa_workspace_free(w) ; pp_error(err,nerr,"%s",message) ; return NULL ;
}

THD_mvpa_workspace * THD_mvpa_workspace_alloc( const THD_foldplan *plan,
                                                int max_feature, int nclass,
                                                int preprocess,
                                                const int *group_id,
                                                char *err, int nerr )
{
   THD_mvpa_workspace *w ; int ff,ii,jj,max_group=1 ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( !THD_foldplan_validate(plan,err,nerr) ) return NULL ;
   if( max_feature<1 || nclass<2 || THD_preprocess_name(preprocess)==NULL ){
     pp_error(err,nerr,"workspace needs features, at least two classes, and valid preprocessing") ;
     return NULL ;
   }
   if( preprocess==THD_PREP_RUN_CENTER && group_id==NULL ){
     pp_error(err,nerr,"run_center workspace needs one group ID per sample") ;
     return NULL ;
   }
   w=(THD_mvpa_workspace *)calloc(1,sizeof(*w)) ;
   if( w==NULL ){ pp_error(err,nerr,"out of memory allocating MVPA workspace") ; return NULL ; }
   w->plan=plan ; w->nsample=plan->nsample ; w->nfold=plan->nfold ;
   w->group_id=(preprocess==THD_PREP_RUN_CENTER)?group_id:NULL ;
   w->nclass=nclass ; w->nfeature_alloc=max_feature ; w->preprocess=preprocess ;
   for( ff=0 ; ff<plan->nfold ; ff++ ){
     if( plan->ntrain[ff]>w->max_train ) w->max_train=plan->ntrain[ff] ;
     if( plan->ntest[ff]>w->max_test ) w->max_test=plan->ntest[ff] ;
   }
   if( (size_t)w->max_train > SIZE_MAX/sizeof(float)/(size_t)max_feature ||
       (size_t)w->max_test > SIZE_MAX/sizeof(float)/(size_t)max_feature ||
       (size_t)w->max_test > SIZE_MAX/sizeof(double)/(size_t)nclass ||
       (size_t)w->nsample > SIZE_MAX/sizeof(double)/(size_t)nclass ||
       (size_t)nclass > SIZE_MAX/sizeof(int)/(size_t)nclass )
     return workspace_fail(w,err,nerr,"MVPA workspace dimensions are too large") ;

   w->train_x=(float *)malloc(sizeof(float)*(size_t)w->max_train*max_feature) ;
   w->test_x=(float *)malloc(sizeof(float)*(size_t)w->max_test*max_feature) ;
   w->train_class=(int *)malloc(sizeof(int)*(size_t)w->max_train) ;
   w->test_class=(int *)malloc(sizeof(int)*(size_t)w->max_test) ;
   w->test_prediction=(int *)malloc(sizeof(int)*(size_t)w->max_test) ;
   w->test_decision=(double *)malloc(sizeof(double)*(size_t)w->max_test*nclass) ;
   w->train_count=(int *)malloc(sizeof(int)*(size_t)nclass) ;
   w->fold_confusion=(int *)malloc(sizeof(int)*(size_t)nclass*nclass) ;
   w->fold_recall=(double *)malloc(sizeof(double)*(size_t)nclass) ;
   w->prediction=(int *)malloc(sizeof(int)*(size_t)w->nsample) ;
   w->decision=(double *)malloc(sizeof(double)*(size_t)w->nsample*nclass) ;
   w->fold_nsample=(int *)malloc(sizeof(int)*(size_t)w->nfold) ;
   w->fold_accuracy=(double *)malloc(sizeof(double)*(size_t)w->nfold) ;
   w->fold_balanced_accuracy=(double *)malloc(sizeof(double)*(size_t)w->nfold) ;
   w->confusion=(int *)malloc(sizeof(int)*(size_t)nclass*nclass) ;
   w->recall=(double *)malloc(sizeof(double)*(size_t)nclass) ;
   if( w->train_x==NULL || w->test_x==NULL || w->train_class==NULL ||
       w->test_class==NULL || w->test_prediction==NULL || w->test_decision==NULL ||
       w->train_count==NULL || w->fold_confusion==NULL ||
       w->fold_recall==NULL || w->prediction==NULL || w->decision==NULL ||
       w->fold_nsample==NULL || w->fold_accuracy==NULL ||
       w->fold_balanced_accuracy==NULL || w->confusion==NULL || w->recall==NULL )
     return workspace_fail(w,err,nerr,"out of memory allocating MVPA workspace buffers") ;
   if( preprocess==THD_PREP_RUN_CENTER ){
     max_group=0 ;
     for( ii=0 ; ii<w->nsample ; ii++ ){
       if( group_id[ii]<0 ){
         THD_mvpa_workspace_free(w) ;
         pp_error(err,nerr,"sample %d has no valid run/group ID",ii+1) ; return NULL ;
       }
       for( jj=0 ; jj<ii ; jj++ ) if( group_id[jj]==group_id[ii] ) break ;
       if( jj==ii ) max_group++ ;
     }
   }
   w->prep=pp_alloc(max_feature,max_group,err,nerr) ;
   if( w->prep==NULL ){ THD_mvpa_workspace_free(w) ; return NULL ; }
   w->centroid=centroid_alloc(nclass,max_feature,err,nerr) ;
   if( w->centroid==NULL ){ THD_mvpa_workspace_free(w) ; return NULL ; }
   return w ;
}

int THD_mvpa_classification_set_ridge(THD_mvpa_workspace *w,double lambda,
 const THD_nested_foldplan *nested,const double *grid,int nlambda,char *err,int nerr)
{
   THD_mvpa_ridge_workspace *ridge;float *response;
   if(!w || w->selection || w->pca || w->ridge_cv || w->lda || w->svm || w->logistic || !isfinite(lambda) || lambda<=0 ||
      (!nested && (grid || nlambda)))
      return pp_error(err,nerr,"ridge classifier needs an unconfigured workspace and positive lambda");
   ridge=THD_mvpa_ridge_workspace_alloc(w->plan,w->nfeature_alloc,w->nclass,w->preprocess,err,nerr);
   if(!ridge)return 0;
   if(nested && !THD_mvpa_ridge_set_tuning(ridge,nested,grid,nlambda,err,nerr)){
      THD_mvpa_ridge_workspace_free(ridge);return 0;
   }
   response=malloc(sizeof(float)*(size_t)w->nsample*w->nclass);
   if(!response){THD_mvpa_ridge_workspace_free(ridge);return pp_error(err,nerr,"cannot allocate class responses");}
   ridge->classification=1;w->ridge_cv=ridge;w->class_response=response;w->ridge_lambda=lambda;return 1;
}

int THD_mvpa_classification_set_lda(THD_mvpa_workspace *w,int mode,double alpha,
 double floor,int empirical,char *err,int nerr)
{
   THD_lda_model *lda;double *scale,*vf;int *form,f;
   if(!w || w->selection || w->pca || w->ridge_cv || w->lda || w->svm || w->logistic || !isfinite(floor) || floor<1e-12 || floor>1 ||
      (mode==THD_LDA_SHRINKAGE && (!isfinite(alpha) || alpha<=0 || alpha>1)))
      return pp_error(err,nerr,"LDA needs an unconfigured workspace, a valid variance floor and shrinkage");
   for(f=0;f<w->nfold;f++)if(w->plan->ntrain[f]<=w->nclass)
      return pp_error(err,nerr,"fold %s: LDA needs more training rows than classes to estimate pooled variance",w->plan->name[f]);
   lda=THD_lda_alloc(mode,w->max_train,w->nfeature_alloc,w->nclass,err,nerr);
   if(!lda)return 0;
   scale=malloc(sizeof(double)*(size_t)w->nfold);vf=malloc(sizeof(double)*(size_t)w->nfold);
   form=malloc(sizeof(int)*(size_t)w->nfold);
   if(!scale || !vf || !form){
      THD_lda_free(lda);free(scale);free(vf);free(form);return pp_error(err,nerr,"cannot allocate LDA fold diagnostics");
   }
   w->lda=lda;w->lda_shrinkage=mode==THD_LDA_DIAGONAL?0:alpha;
   w->lda_variance_floor=floor;w->lda_empirical_prior=empirical;
   w->fold_covariance_scale=scale;w->fold_variance_floor=vf;w->fold_covariance_form=form;return 1;
}

static int classification_set_penalty(THD_mvpa_workspace *w,int logistic,double cost,double tol,
 int maxiter,int metric,const THD_nested_foldplan *nested,const double *grid,int ng,
 char *err,int nerr)
{
   THD_svm_model *svm=NULL;THD_logistic_model *lm=NULL;double *selected,*loss=NULL,*copy=NULL,*residual;
   int *iterations,*train=NULL,*test=NULL,f,i,j;
   if(!w || w->selection || w->pca || w->ridge_cv || w->lda || w->svm || w->logistic || !isfinite(cost) || cost<=0 ||
      !isfinite(.5/cost) || .5/cost<=0 || !isfinite(tol) || tol<1e-12 || tol>1e-2 || maxiter<1 ||
      (metric!=THD_METRIC_ACCURACY && metric!=THD_METRIC_BALANCED_ACCURACY) ||
      (nested?(!grid || ng<1 || nested->outer!=w->plan || !nested->inner):(grid || ng)))
      return pp_error(err,nerr,"classifier needs an unconfigured workspace, valid penalty/solver options and matching optional nested folds");
   if(nested){
      if((size_t)ng>SIZE_MAX/sizeof(double)/(size_t)w->nfold)
         return pp_error(err,nerr,"classifier tuning grid is too large");
      for(i=0;i<ng;i++){
         if(!isfinite(grid[i]) || grid[i]<=0 || !isfinite(.5/grid[i]) || .5/grid[i]<=0)
            return pp_error(err,nerr,"classifier penalty grid values must be finite, positive and numerically representable");
         for(j=0;j<i;j++)if(grid[i]==grid[j])return pp_error(err,nerr,"classifier penalty grid contains duplicates");
      }
      for(f=0;f<w->nfold;f++){
         const THD_foldplan *in=nested->inner[f];
         if(!THD_foldplan_validate(in,err,nerr))return 0;
         if(in->nsample!=w->plan->ntrain[f] || in->nfold<2 || in->kind==THD_CV_FIXED)
            return pp_error(err,nerr,"classifier tuning needs grouped inner folds over exactly each outer training set");
      }
   }
   if(logistic)lm=THD_logistic_alloc(w->max_train,w->nfeature_alloc,w->nclass,err,nerr);
   else svm=THD_svm_alloc(w->max_train,w->nfeature_alloc,w->nclass,err,nerr);
   selected=malloc(sizeof(double)*(size_t)w->nfold);residual=malloc(sizeof(double)*(size_t)w->nfold);
   iterations=malloc(sizeof(int)*(size_t)w->nfold);
   if(nested){
      copy=malloc(sizeof(double)*(size_t)ng);loss=malloc(sizeof(double)*(size_t)w->nfold*ng);
      train=malloc(sizeof(int)*(size_t)w->nsample);test=malloc(sizeof(int)*(size_t)w->nsample);
   }
   if((logistic?!lm:!svm) || !selected || !residual || !iterations || (nested && (!copy || !loss || !train || !test))){
      THD_logistic_free(lm);THD_svm_free(svm);free(selected);free(residual);free(iterations);free(copy);free(loss);free(train);free(test);
      return pp_error(err,nerr,"cannot allocate classifier/tuning buffers");
   }
   if(nested)memcpy(copy,grid,sizeof(double)*(size_t)ng);
   w->logistic=lm;w->svm=svm;w->class_penalty=cost;w->class_tolerance=tol;w->class_max_iter=maxiter;w->class_metric=metric;
   w->class_nested=nested;w->class_grid=copy;w->class_ngrid=ng;w->class_selected=selected;w->class_loss=loss;
   w->fold_solver_residual=residual;w->fold_solver_iterations=iterations;
   w->class_inner_train=train;w->class_inner_test=test;return 1;
}

int THD_mvpa_classification_set_svm(THD_mvpa_workspace *w,double cost,double tol,
 int maxiter,int metric,const THD_nested_foldplan *nested,const double *grid,int ng,char *err,int nerr)
{return classification_set_penalty(w,0,cost,tol,maxiter,metric,nested,grid,ng,err,nerr);}
int THD_mvpa_classification_set_logistic(THD_mvpa_workspace *w,double lambda,double tol,
 int maxiter,int metric,const THD_nested_foldplan *nested,const double *grid,int ng,char *err,int nerr)
{
   double *pr,*tp,*ll,*br;
   if(!w)return pp_error(err,nerr,"logistic needs a workspace");
   pr=malloc(sizeof(double)*(size_t)w->nsample*w->nclass);
   tp=malloc(sizeof(double)*(size_t)w->max_test*w->nclass);
   ll=malloc(sizeof(double)*(size_t)w->nfold);br=malloc(sizeof(double)*(size_t)w->nfold);
   if(!pr || !tp || !ll || !br){
      free(pr);free(tp);free(ll);free(br);return pp_error(err,nerr,"cannot allocate logistic probability buffers");
   }
   if(!classification_set_penalty(w,1,lambda,tol,maxiter,metric,nested,grid,ng,err,nerr)){
      free(pr);free(tp);free(ll);free(br);return 0;
   }
   w->probability=pr;w->test_probability=tp;w->fold_log_loss=ll;w->fold_brier=br;return 1;
}

static int class_fit(THD_mvpa_workspace *w,int distance,int n,int p,double cost,char *err,int nerr)
{
   if(w->logistic)return THD_logistic_fit(w->logistic,w->train_x,w->train_class,n,p,cost,w->class_tolerance,w->class_max_iter,err,nerr);
   if(w->svm)return THD_svm_fit(w->svm,w->train_x,w->train_class,n,p,cost,w->class_tolerance,w->class_max_iter,err,nerr);
   if(w->lda)return THD_lda_fit(w->lda,w->train_x,w->train_class,n,p,w->lda_shrinkage,w->lda_variance_floor,w->lda_empirical_prior,err,nerr);
   return centroid_refit(w->centroid,distance,w->train_x,w->train_class,n,p,err,nerr);
}
static int class_predict(THD_mvpa_workspace *w,int n,int p,char *err,int nerr)
{
   if(w->logistic)return THD_logistic_predict(w->logistic,w->test_x,n,p,w->test_prediction,w->test_decision,w->test_probability,err,nerr);
   if(w->svm)return THD_svm_predict(w->svm,w->test_x,n,p,w->test_prediction,w->test_decision,err,nerr);
   if(w->lda)return THD_lda_predict(w->lda,w->test_x,n,p,w->test_prediction,w->test_decision,err,nerr);
   return THD_centroid_predict(w->centroid,w->test_x,n,p,w->test_prediction,w->test_decision,err,nerr);
}
typedef struct {
   THD_mvpa_workspace *w;const float *x;const int *classes,*groups;
   int p,distance,npenalty,objective,width,outer;
   double probability_loss;int probability_count;
} Class_tuning;
static int class_candidate_setup(void *data,int batch,char *err,int nerr)
{
   Class_tuning *t=data;(void)batch;(void)err;(void)nerr;
   if(t->w->pca){THD_mvpa_pca *pc=t->w->pca;size_t at=(size_t)t->outer*pc->nselection*pc->ngrid+batch/t->npenalty;
      if(batch%t->npenalty==0){pc->inner_min[at]=INT_MAX;pc->inner_max[at]=0;}}
   t->probability_loss=0;t->probability_count=0;
   memset(t->w->fold_confusion,0,sizeof(int)*(size_t)t->w->nclass*t->w->nclass);return 1;
}
static int class_candidate_prepare(void *data,int batch,const int *train,int nt,
 const int *test,int ne,char *err,int nerr)
{
   Class_tuning *t=data;THD_mvpa_workspace *w=t->w;THD_feature_selection *sel=w->selection;
   int i,width=fs_count(sel,batch/t->npenalty/pca_ngrid(w->pca),t->p);(void)test;(void)ne;
   if(!pp_refit(w->prep,w->preprocess,t->x,w->nsample,t->p,train,nt,t->groups,0,err,nerr) ||
      !pp_apply(w->prep,t->x,w->nsample,t->p,train,nt,t->groups,w->train_x,0,err,nerr))return 0;
   for(i=0;i<nt;i++)w->train_class[i]=t->classes[train[i]];
   if(sel){
      if(!fs_fit(sel,w->train_x,nt,t->p,NULL,0,w->train_class,err,nerr))return 0;
      fs_apply(sel,w->train_x,nt,t->p,width);
   }
   if(!pca_transform(w->pca,(batch/t->npenalty)%pca_ngrid(w->pca),w->train_x,nt,NULL,0,&width,err,nerr))return 0;
   t->width=width;if(w->pca)pca_inner_record(w->pca,t->outer,batch/t->npenalty);
   return 1;
}
static int class_candidate_evaluate(void *data,int batch,int candidate,
 const int *train,int nt,const int *test,int ne,char *err,int nerr)
{
   Class_tuning *t=data;THD_mvpa_workspace *w=t->w;THD_feature_selection *sel=w->selection;
   int i,width=fs_count(sel,batch/t->npenalty/pca_ngrid(w->pca),t->p);
   double cost=w->class_nested?w->class_grid[batch%t->npenalty]:w->class_penalty;
   (void)candidate;(void)train;
   if(!class_fit(w,t->distance,nt,t->width,cost,err,nerr))return 0;
   /* An inner test group may exceed the outer test buffer capacity. */
   for(i=0;i<ne;i++){
      int row=test[i];
      if(!pp_apply(w->prep,t->x,w->nsample,t->p,&row,1,t->groups,w->test_x,0,err,nerr))return 0;
      if(sel)fs_apply(sel,w->test_x,1,t->p,width);
      if(w->pca && !THD_pca_apply(w->pca->model,w->test_x,1,err,nerr))return 0;
      if(!class_predict(w,1,t->width,err,nerr))return 0;
      w->fold_confusion[(size_t)t->classes[row]*w->nclass+w->test_prediction[0]]++;
      if(t->objective==THD_TUNE_LOG_LOSS || t->objective==THD_TUNE_BRIER){
         double ll,br;if(!THD_mvpa_probability_score(w->test_decision,w->nclass,t->classes[row],&ll,&br,err,nerr))return 0;
         t->probability_loss+=t->objective==THD_TUNE_LOG_LOSS?ll:br;t->probability_count++;
      }
   }
   return 1;
}
static int class_candidate_finish(void *data,int batch,int candidate,double *loss,int *nvalid,char *err,int nerr)
{
   Class_tuning *t=data;(void)batch;(void)candidate;
   if(t->objective==THD_TUNE_LOG_LOSS || t->objective==THD_TUNE_BRIER){
      if(!t->probability_count)return pp_error(err,nerr,"probability tuning has no held-out rows");
      *loss=t->probability_loss/t->probability_count;*nvalid=1;return isfinite(*loss)?1:pp_error(err,nerr,"nonfinite probability loss");
   }
   return tune_class_loss(t->w->fold_confusion,t->w->nclass,t->objective,loss,nvalid,err,nerr);
}
static int class_candidate_prefer(void *data,int a,int b)
{
   Class_tuning *t=data;THD_mvpa_workspace *w=t->w;THD_feature_selection *sel=w->selection;
   int np=pca_ngrid(w->pca),ac=a/t->npenalty,bc=b/t->npenalty;
   if(sel && sel->grid[ac/np]!=sel->grid[bc/np])return sel->grid[ac/np]<sel->grid[bc/np];
   if(w->pca && w->pca->grid[ac%np]!=w->pca->grid[bc%np])return w->pca->grid[ac%np]<w->pca->grid[bc%np];
   return w->class_nested && (w->logistic?w->class_grid[a%t->npenalty]>w->class_grid[b%t->npenalty]:w->class_grid[a%t->npenalty]<w->class_grid[b%t->npenalty]);
}
static int class_tune_fold(THD_mvpa_workspace *w,int outer,int distance,const float *x,int p,
 const int *classes,const int *groups,char *err,int nerr)
{
   static const THD_cv_candidate_ops ops={class_candidate_setup,class_candidate_prepare,
      class_candidate_evaluate,class_candidate_finish,class_candidate_prefer};
   THD_feature_selection *sel=w->selection;THD_mvpa_pca *pc=w->pca;
   const THD_nested_foldplan *nested=pc?pc->nested:w->class_nested?w->class_nested:sel->nested;
   int *train=pc?pc->train:sel?sel->train:w->class_inner_train,*test=pc?pc->test:sel?sel->test:w->class_inner_test;
   int nk=sel?sel->ngrid:1,np=pca_ngrid(pc),nl=w->class_nested?w->class_ngrid:1,l,c,best,nvalid;
   int metric=pc?pc->metric:sel?sel->metric:w->class_metric;
   double *loss=pc?pc->loss+(size_t)outer*nk*np*nl:sel?sel->loss+(size_t)outer*nk*nl:w->class_loss+(size_t)outer*nl;
   Class_tuning context={w,x,classes,groups,p,distance,nl,w->tune_objective?w->tune_objective:
      w->logistic?THD_TUNE_LOG_LOSS:metric==THD_METRIC_ACCURACY?THD_TUNE_ACCURACY:THD_TUNE_BALANCED_ACCURACY,0,outer,0,0};
   if(nk>INT_MAX/np || nk*np>INT_MAX/nl)return pp_error(err,nerr,"too many tuning candidates");
   if(!THD_nested_candidate_search(nested,outer,nk*np*nl,1,&ops,&context,train,test,
                                  loss,&best,&nvalid,err,nerr))return 0;
   if(pc)pc->chosen[outer]=(best/nl)%np;
   if(sel){
      sel->chosen[outer]=best/nl/np;
      if(pc)for(c=0;c<nk;c++)for(l=0;l<nl;l++)sel->loss[((size_t)outer*nk+c)*nl+l]=loss[(c*np+pc->chosen[outer])*nl+l];
   }
   if((sel || pc) && w->class_nested)for(l=0;l<nl;l++)w->class_loss[(size_t)outer*nl+l]=loss[(best/nl)*nl+l];
   if(w->svm || w->logistic)w->class_selected[outer]=w->class_nested?w->class_grid[best%nl]:w->class_penalty;
   return 1;
}

int THD_mvpa_run_classification_cv( const THD_foldplan *plan,
                               int distance, const float *x,
                               int nfeature, const int *class_id,
                               const int *group_id, THD_mvpa_workspace *w,
                               char *err, int nerr )
{
   int ff,ii,jj,total=0 ; char why[512] ; THD_mvpa_class_score fold_score ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( w==NULL || plan==NULL || plan!=w->plan || x==NULL || class_id==NULL ||
       nfeature<1 || nfeature>w->nfeature_alloc )
     return pp_error(err,nerr,"CV inputs do not match the allocated MVPA workspace") ;
   if( !w->ridge_cv && !w->lda && !w->svm && !w->logistic && THD_centroid_distance_name(distance)==NULL )
     return pp_error(err,nerr,"invalid centroid distance %d",distance) ;
   if( w->preprocess==THD_PREP_RUN_CENTER && group_id!=w->group_id )
     return pp_error(err,nerr,"run_center group IDs do not match the allocated workspace") ;
   for( ii=0 ; ii<w->nsample ; ii++ ){
     if( class_id[ii]<0 || class_id[ii]>=w->nclass )
       return pp_error(err,nerr,"sample %d has invalid class index %d",ii+1,class_id[ii]) ;
     for( jj=0 ; jj<nfeature ; jj++ )
       if( !isfinite(x[(size_t)ii*nfeature+jj]) )
         return pp_error(err,nerr,"sample %d, feature %d is not finite",ii+1,jj+1) ;
   }
   for( ff=0 ; ff<w->nfold ; ff++ ){
     memset(w->train_count,0,sizeof(int)*(size_t)w->nclass) ;
     for( ii=0 ; ii<plan->ntrain[ff] ; ii++ )
       w->train_count[class_id[plan->train[ff][ii]]]++ ;
     for( ii=0 ; ii<w->nclass ; ii++ ) if( w->train_count[ii]==0 )
       return pp_error(err,nerr,"fold %s has no class %d in its training set",plan->name[ff],ii+1) ;
   }

   if(w->ridge_cv){
     const THD_nested_foldplan *nested=w->ridge_cv->nested;
     if(nested)for(ff=0;ff<w->nfold;ff++){
       const THD_foldplan *inner=nested->inner[ff];int f;
       for(f=0;f<inner->nfold;f++){
         memset(w->train_count,0,sizeof(int)*(size_t)w->nclass);
         for(ii=0;ii<inner->ntrain[f];ii++)
           w->train_count[class_id[plan->train[ff][inner->train[f][ii]]]]++;
         for(ii=0;ii<w->nclass;ii++)if(!w->train_count[ii])
           return pp_error(err,nerr,"outer fold %s, inner fold %s has no class %d in training",plan->name[ff],inner->name[f],ii+1);
       }
     }
     for(ii=0;ii<w->nsample;ii++)for(jj=0;jj<w->nclass;jj++)
       w->class_response[(size_t)ii*w->nclass+jj]=(class_id[ii]==jj)?1.0f:0.0f;
     if(w->ridge_cv->selection)w->ridge_cv->selection->classes=class_id;
     if(!THD_mvpa_run_ridge_cv(w->ridge_cv,x,nfeature,w->class_response,w->nclass,
                              group_id,w->ridge_lambda,why,sizeof(why)))
       return pp_error(err,nerr,"ridge classification: %s",why);
   }

   for( ii=0 ; ii<w->nsample ; ii++ ){
     w->prediction[ii]=-1 ;
     if(w->probability)for(jj=0;jj<w->nclass;jj++)w->probability[(size_t)ii*w->nclass+jj]=NAN;
     for( jj=0 ; jj<w->nclass ; jj++ )
       w->decision[(size_t)ii*w->nclass+jj]=NAN ;
   }
   memset(w->confusion,0,sizeof(int)*(size_t)w->nclass*w->nclass) ;
   w->ntest=0 ;
   for( ff=0 ; ff<w->nfold ; ff++ ){
     int ntrain=plan->ntrain[ff],ntest=plan->ntest[ff] ;
     if(!w->ridge_cv){
     int width=nfeature;THD_feature_selection *sel=w->selection;
     if(sel)sel->chosen[ff]=0;
     if(w->pca)w->pca->chosen[ff]=0;
     if(w->svm || w->logistic)w->class_selected[ff]=w->class_penalty;
     if((w->class_nested || (sel && sel->ngrid>1) || (w->pca && w->pca->nested)) &&
        !class_tune_fold(w,ff,distance,x,nfeature,class_id,group_id,why,sizeof(why)))
       return pp_error(err,nerr,"classification tuning: %s",why);
     if( !pp_refit(w->prep,w->preprocess,x,w->nsample,nfeature,
                   plan->train[ff],ntrain,group_id,0,why,sizeof(why)) )
       return pp_error(err,nerr,"fold %s preprocessing fit: %s",plan->name[ff],why) ;
     if( !pp_apply(w->prep,x,w->nsample,nfeature,plan->train[ff],ntrain,
                   group_id,w->train_x,0,why,sizeof(why)) ||
         !pp_apply(w->prep,x,w->nsample,nfeature,plan->test[ff],ntest,
                   group_id,w->test_x,0,why,sizeof(why)) )
       return pp_error(err,nerr,"fold %s preprocessing apply: %s",plan->name[ff],why) ;
     for( ii=0 ; ii<ntrain ; ii++ ) w->train_class[ii]=class_id[plan->train[ff][ii]] ;
     for( ii=0 ; ii<ntest ; ii++ ) w->test_class[ii]=class_id[plan->test[ff][ii]] ;
     if(sel){
       if(!fs_fit(sel,w->train_x,ntrain,nfeature,NULL,0,w->train_class,err,nerr))return 0;
       fs_record(sel,ff,sel->chosen[ff],nfeature);width=sel->count[ff];
       fs_apply(sel,w->train_x,ntrain,nfeature,width);fs_apply(sel,w->test_x,ntest,nfeature,width);
     }
     if(!pca_transform(w->pca,w->pca?w->pca->chosen[ff]:0,w->train_x,ntrain,w->test_x,ntest,&width,err,nerr))return 0;
     if(w->pca)pca_record(w->pca,ff);
     if(!class_fit(w,distance,ntrain,width,(w->svm || w->logistic)?w->class_selected[ff]:0,why,sizeof(why)) ||
        !class_predict(w,ntest,width,why,sizeof(why)))
       return pp_error(err,nerr,"fold %s classifier: %s",plan->name[ff],why);
     if(w->svm){
       w->fold_solver_iterations[ff]=w->svm->max_iterations;w->fold_solver_residual[ff]=w->svm->residual;
     }
     if(w->logistic){
       w->fold_solver_iterations[ff]=w->logistic->iterations;w->fold_solver_residual[ff]=w->logistic->residual;
       w->fold_log_loss[ff]=w->fold_brier[ff]=0;
       for(ii=0;ii<ntest;ii++){
         double ll,br;if(!THD_mvpa_probability_score(w->test_decision+(size_t)ii*w->nclass,w->nclass,w->test_class[ii],&ll,&br,err,nerr))return 0;
         w->fold_log_loss[ff]+=ll/ntest;w->fold_brier[ff]+=br/ntest;
         memcpy(w->probability+(size_t)plan->test[ff][ii]*w->nclass,w->test_probability+(size_t)ii*w->nclass,sizeof(double)*(size_t)w->nclass);
       }
     }
     if(w->lda){
       w->fold_covariance_scale[ff]=w->lda->reference;
       w->fold_variance_floor[ff]=w->lda->reference*w->lda_variance_floor;
       w->fold_covariance_form[ff]=w->lda->form;
     }
     }else for(ii=0;ii<ntest;ii++){
       int row=plan->test[ff][ii],best=0;
       const float *scores=w->ridge_cv->prediction+(size_t)row*w->nclass;
       w->test_class[ii]=class_id[row];
       for(jj=0;jj<w->nclass;jj++){
         w->test_decision[(size_t)ii*w->nclass+jj]=scores[jj];
         if(scores[jj]>scores[best])best=jj;
       }
       w->test_prediction[ii]=best;
     }
     if( !THD_mvpa_score_classification(w->test_class,w->test_prediction,
                                        ntest,w->nclass,w->fold_confusion,
                                        w->fold_recall,&fold_score,why,sizeof(why)) )
       return pp_error(err,nerr,"fold %s scoring: %s",plan->name[ff],why) ;
     w->fold_nsample[ff]=ntest ; w->fold_accuracy[ff]=fold_score.accuracy ;
     w->fold_balanced_accuracy[ff]=fold_score.balanced_accuracy ;
     for( ii=0 ; ii<ntest ; ii++ ){
       int ss=plan->test[ff][ii],truth=w->test_class[ii],pred=w->test_prediction[ii] ;
       w->prediction[ss]=pred ;
       for( jj=0 ; jj<w->nclass ; jj++ )
         w->decision[(size_t)ss*w->nclass+jj]=w->test_decision[(size_t)ii*w->nclass+jj] ;
       w->confusion[(size_t)truth*w->nclass+pred]++ ; total++ ;
     }
   }
   w->ntest=total ;
   mvpa_class_score_from_confusion(w->confusion,total,w->nclass,w->recall,&w->score) ;
   return 1 ;
}

int THD_mvpa_run_classification_permutations(
                               const THD_foldplan *plan,
                               int distance, const float *x,
                               int nfeature, const int *class_id,
                               const int *group_id, int metric,
                               const PERM_set *pset,
                               int *permuted_class, float *null_score,
                               THD_mvpa_workspace *w,
                               char *err, int nerr )
{
   int ip,ii ; char why[512] ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( plan==NULL || class_id==NULL || pset==NULL || permuted_class==NULL ||
       null_score==NULL || w==NULL || plan!=w->plan || pset->nobs!=plan->nsample ||
       pset->nperm<1 || pset->perm==NULL || pset->sign==NULL )
     return pp_error(err,nerr,"permutation CV needs matching folds, relabelings, scratch, and workspace") ;
   if( metric!=THD_METRIC_ACCURACY && metric!=THD_METRIC_BALANCED_ACCURACY )
     return pp_error(err,nerr,"classification permutations need accuracy or balanced_accuracy") ;

   for( ip=0 ; ip<pset->nperm ; ip++ ){
     const int *perm=pset->perm+(size_t)ip*pset->nobs ;
     const signed char *sign=pset->sign+(size_t)ip*pset->nobs ;
     memset(w->prediction,0,sizeof(int)*(size_t)pset->nobs);
     for( ii=0 ; ii<pset->nobs ; ii++ ){
       int src=perm[ii] ;
       if( src<0 || src>=pset->nobs || sign[ii]!=1 || w->prediction[src] || (ip==0 && src!=ii))
         return pp_error(err,nerr,"invalid label relabeling %d at sample %d",ip+1,ii+1) ;
       w->prediction[src]=1;
       permuted_class[ii]=class_id[src] ;
     }
     if(!nuisance_permutation(w->ridge_cv?w->ridge_cv->prep:w->prep,perm,pset->nobs,err,nerr))return 0;
     if( !THD_mvpa_run_classification_cv(plan,distance,x,nfeature,permuted_class,
                                    group_id,w,why,sizeof(why)) )
       return pp_error(err,nerr,"label relabeling %d: %s",ip+1,why) ;
     null_score[ip]=(float)(metric==THD_METRIC_ACCURACY ? w->score.accuracy
                                                        : w->score.balanced_accuracy) ;
     if( !isfinite(null_score[ip]) )
       return pp_error(err,nerr,"label relabeling %d produced a non-finite score",ip+1) ;
   }

   if( !THD_mvpa_run_classification_cv(plan,distance,x,nfeature,class_id,group_id,
                                  w,why,sizeof(why)) )
     return pp_error(err,nerr,"restoring observed labels after permutation: %s",why) ;
   return 1 ;
}

int THD_mvpa_run_centroid_cv(const THD_foldplan *plan,int distance,const float *x,
 int p,const int *classes,const int *groups,THD_mvpa_workspace *w,char *err,int nerr)
{
   if(w && (w->ridge_cv || w->lda || w->svm || w->logistic))return pp_error(err,nerr,"centroid runner needs a centroid-configured workspace");
   return THD_mvpa_run_classification_cv(plan,distance,x,p,classes,groups,w,err,nerr);
}

int THD_mvpa_run_centroid_permutations(const THD_foldplan *plan,int distance,
 const float *x,int p,const int *classes,const int *groups,int metric,
 const PERM_set *pset,int *permuted,float *scores,THD_mvpa_workspace *w,char *err,int nerr)
{
   if(w && (w->ridge_cv || w->lda || w->svm || w->logistic))return pp_error(err,nerr,"centroid runner needs a centroid-configured workspace");
   return THD_mvpa_run_classification_permutations(plan,distance,x,p,classes,groups,
      metric,pset,permuted,scores,w,err,nerr);
}

/* Training requirements without allocating an estimator. */
int THD_mvpa_validate_fold_inputs(const THD_foldplan *plan,
   const int *classes,int nclass,const int *groups,int lda,const char *scope,char *err,int nerr)
{
   int f,i,j;char detail[512];
   if(!scope)scope="folds";
   if(!THD_foldplan_validate(plan,err,nerr))return 0;
   if(classes && !THD_foldplan_check_classes(plan,classes,nclass,0,detail,sizeof(detail)))
      return pp_error(err,nerr,"%s: %s",scope,detail);
   for(f=0;f<plan->nfold;f++){
      if(lda && plan->ntrain[f]<=nclass)
         return pp_error(err,nerr,"%s fold %s: LDA needs more training rows than classes",
                    scope,plan->name[f]);
      if(groups)for(i=0;i<plan->ntest[f];i++){
         int row=plan->test[f][i];
         for(j=0;j<plan->ntrain[f];j++)
            if(groups[row]==groups[plan->train[f][j]])break;
         if(j==plan->ntrain[f])
            return pp_error(err,nerr,"%s fold %s: run_center sample %d has a run/group absent from training",
                       scope,plan->name[f],row+1);
      }
   }
   return 1;
}

int THD_mvpa_fixed_class_order(const THD_foldplan *plan,int *classes,
                               char **labels,int nclass,char *err,int nerr)
{
   int i,n=0,*code=NULL; char **ordered=NULL;
   if(!THD_foldplan_validate(plan,err,nerr))return 0;
   if(plan->kind!=THD_CV_FIXED || !classes || !labels || nclass<2)
      return pp_error(err,nerr,"class ordering needs a fixed plan and at least two classes");
   for(i=0;i<plan->nsample;i++)if(classes[i]<0 || classes[i]>=nclass)
      return pp_error(err,nerr,"sample %d has invalid class index %d",i+1,classes[i]);
   for(i=0;i<nclass;i++)if(!labels[i])return pp_error(err,nerr,"missing class label %d",i+1);
   code=malloc(sizeof(int)*(size_t)nclass);ordered=malloc(sizeof(char *)*(size_t)nclass);
   if(!code || !ordered){free(code);free(ordered);return pp_error(err,nerr,"cannot order training classes");}
   for(i=0;i<nclass;i++)code[i]=-1;
   for(i=0;i<plan->ntrain[0];i++){
      int c=classes[plan->train[0][i]];
      if(code[c]<0){code[c]=n;ordered[n++]=labels[c];}
   }
   if(n!=nclass){free(code);free(ordered);return pp_error(err,nerr,"fixed partition has a class absent from training");}
   for(i=0;i<plan->nsample;i++)classes[i]=code[classes[i]];
   memcpy(labels,ordered,sizeof(char *)*(size_t)nclass);
   free(code);free(ordered);return 1;
}

struct THD_mvpa_refit_workspace {
   int max_sample,max_feature,max_response;
   int *rows;
   float *transformed;
   double *coef,*intercept;
   THD_ridge_workspace *ridge;
   THD_preprocessor *prep;
   THD_mvpa_refit_result result;
};

void THD_mvpa_refit_free(THD_mvpa_refit_workspace *w)
{
   if(!w)return;
   THD_preprocess_free(w->prep);THD_ridge_free(w->ridge);
   free(w->rows);free(w->transformed);free(w->coef);free(w->intercept);free(w);
}

THD_mvpa_refit_workspace *THD_mvpa_refit_alloc(int n,int p,int q,char *err,int nerr)
{
   THD_mvpa_refit_workspace *w;int i;
   if(n<1 || p<1 || q<1 || (size_t)n>SIZE_MAX/sizeof(int) ||
      (size_t)n>SIZE_MAX/sizeof(float)/(size_t)p ||
      (size_t)p>SIZE_MAX/sizeof(double)/(size_t)q){
      pp_error(err,nerr,"invalid or excessive descriptive refit dimensions");return NULL;
   }
   w=calloc(1,sizeof(*w));
   if(!w){pp_error(err,nerr,"cannot allocate descriptive refit");return NULL;}
   w->max_sample=n;w->max_feature=p;w->max_response=q;
   w->rows=malloc(sizeof(int)*(size_t)n);
   w->transformed=malloc(sizeof(float)*(size_t)n*p);
   w->coef=malloc(sizeof(double)*(size_t)p*q);
   w->intercept=malloc(sizeof(double)*(size_t)q);
   if(!w->rows || !w->transformed || !w->coef || !w->intercept){
      pp_error(err,nerr,"cannot allocate descriptive refit buffers");goto bad;
   }
   w->ridge=THD_ridge_alloc(n,p,q,err,nerr);if(!w->ridge)goto bad;
   for(i=0;i<n;i++)w->rows[i]=i;
   return w;
bad:THD_mvpa_refit_free(w);return NULL;
}

const THD_mvpa_refit_result *THD_mvpa_refit_ridge(
   THD_mvpa_refit_workspace *w,const float *x,const float *y,int n,int p,int q,
   int preprocess,const int *groups,double lambda,char *err,int nerr)
{
   if(!w){pp_error(err,nerr,"descriptive refit needs a workspace");return NULL;}
   THD_preprocess_free(w->prep);w->prep=NULL;
   memset(&w->result,0,sizeof(w->result));
   if(!x || !y || n<1 || p<1 || q<1 || n>w->max_sample ||
      p>w->max_feature || q>w->max_response || !isfinite(lambda) || lambda<=0){
      pp_error(err,nerr,"descriptive refit needs matching dimensions and a finite positive penalty");return NULL;
   }
   w->prep=THD_preprocess_fit(preprocess,x,n,p,w->rows,n,groups,err,nerr);
   if(!w->prep || !THD_preprocess_apply(w->prep,x,n,p,w->rows,n,groups,w->transformed,err,nerr) ||
      !THD_ridge_fit(w->ridge,w->transformed,y,n,p,q,lambda,THD_RIDGE_AUTO,err,nerr) ||
      !THD_ridge_coefficients(w->ridge,w->coef,w->intercept,err,nerr)){
      THD_preprocess_free(w->prep);w->prep=NULL;return NULL;
   }
   w->result.nsample=n;w->result.nfeature=p;w->result.nresponse=q;
   w->result.prep=w->prep;w->result.coef=w->coef;w->result.intercept=w->intercept;
   return &w->result;
}
