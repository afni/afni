#include "thd_mvpa.h"

#include <float.h>
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

int THD_ridge_fit(THD_ridge_workspace *w,const float *x,const float *y,
                  int n,int p,int q,double lambda,int form,char *err,int nerr)
{
   int i,j,k,t,o,d,dual; double *a,*b;
   if(w) w->fitted=0;
   if(!w || !x || !y || n<1 || n>w->maxn || p<1 || p>w->maxp ||
      q<1 || q>w->maxq || !isfinite(lambda) || lambda<=0 || form<0 || form>2)
      return pp_error(err,nerr,"invalid ridge dimensions, form, or positive lambda");
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
      double s=0;
      if(dual) s=y[(size_t)i*q+o]-w->ym[o];
      else for(t=0;t<n;t++) s+=(x[(size_t)t*p+i]-w->xm[i])*(y[(size_t)t*q+o]-w->ym[o]);
      for(k=0;k<i;k++) s-=a[(size_t)i*d+k]*b[(size_t)k*q+o];
      b[(size_t)i*q+o]=s/a[(size_t)i*d+i];
   }
   for(i=d-1;i>=0;i--) for(o=0;o<q;o++){
      double s=b[(size_t)i*q+o];
      for(k=i+1;k<d;k++) s-=a[(size_t)k*d+i]*b[(size_t)k*q+o];
      b[(size_t)i*q+o]=s/a[(size_t)i*d+i];
   }
   for(j=0;j<p;j++) for(o=0;o<q;o++){
      double s=0;
      if(dual) for(i=0;i<n;i++) s+=(x[(size_t)i*p+j]-w->xm[j])*b[(size_t)i*q+o];
      else s=b[(size_t)j*q+o];
      if(!isfinite(s)) return pp_error(err,nerr,"ridge coefficient is not finite");
      w->coef[(size_t)j*q+o]=s;
   }
   w->n=n;w->p=p;w->q=q;w->fitted=1;return 1;
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

void THD_mvpa_ridge_workspace_free(THD_mvpa_ridge_workspace *w)
{
   if(!w)return;
   THD_preprocess_free(w->prep);THD_ridge_free(w->ridge);
   free(w->train_x);free(w->test_x);free(w->train_y);free(w->test_y);
   free(w->prediction);free(w->baseline);free(w->truth_scratch);free(w->pred_scratch);
   free(w->base_scratch);free(w->rank_scratch);free(w->index_scratch);
   free(w->score);free(w->fold_score);free(w->selected_lambda);free(w->inner_nvalid);
   free(w->lambda_grid);free(w->inner_loss);free(w->inner_train);free(w->inner_test);
   free(w->inner_sse);free(w->inner_baseline);free(w);
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
   if(!w || w->nested || !nested || nested->outer!=w->plan || !nested->inner || !grid || nl<2)
      return pp_error(err,nerr,"ridge tuning needs a matching nested plan and at least two penalties; configure it once");
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

static int ridge_tune_fold(THD_mvpa_ridge_workspace *w,int outer,
 const float *x,int p,const float *y,int q,const int *group,char *err,int nerr)
{
   const THD_foldplan *in=w->nested->inner[outer];
   int f,i,o,l,best=0,nvalid=0,n=w->plan->nsample;double bestloss=HUGE_VAL;
   memset(w->inner_sse,0,sizeof(double)*(size_t)w->nlambda*q);
   memset(w->inner_baseline,0,sizeof(double)*(size_t)q);
   for(f=0;f<in->nfold;f++){
      int nt=in->ntrain[f],ne=in->ntest[f];
      for(i=0;i<nt;i++)w->inner_train[i]=w->plan->train[outer][in->train[f][i]];
      for(i=0;i<ne;i++)w->inner_test[i]=w->plan->train[outer][in->test[f][i]];
      if(!pp_refit(w->prep,w->preprocess,x,n,p,w->inner_train,nt,group,0,err,nerr) ||
         !pp_apply(w->prep,x,n,p,w->inner_train,nt,group,w->train_x,0,err,nerr) ||
         !pp_apply(w->prep,x,n,p,w->inner_test,ne,group,w->test_x,0,err,nerr))return 0;
      for(i=0;i<nt;i++)for(o=0;o<q;o++)w->train_y[(size_t)i*q+o]=y[(size_t)w->inner_train[i]*q+o];
      for(l=0;l<w->nlambda;l++){
         if(!THD_ridge_fit(w->ridge,w->train_x,w->train_y,nt,p,q,w->lambda_grid[l],THD_RIDGE_AUTO,err,nerr) ||
            !THD_ridge_predict(w->ridge,w->test_x,ne,w->test_y,err,nerr))return 0;
         for(i=0;i<ne;i++)for(o=0;o<q;o++){
            double truth=y[(size_t)w->inner_test[i]*q+o];
            double e=truth-w->test_y[(size_t)i*q+o];
            w->inner_sse[(size_t)l*q+o]+=e*e;
            if(l==0){double b=truth-w->ridge->ym[o];w->inner_baseline[o]+=b*b;}
         }
      }
   }
   for(o=0;o<q;o++)if(w->inner_baseline[o]>0)nvalid++;
   for(l=0;l<w->nlambda;l++){
      double loss=0;
      for(o=0;o<q;o++)if(w->inner_baseline[o]>0)loss+=w->inner_sse[(size_t)l*q+o]/w->inner_baseline[o];
      loss=nvalid?loss/nvalid:NAN;
      w->inner_loss[(size_t)outer*w->nlambda+l]=loss;
      if((nvalid && loss<bestloss) || ((!nvalid || loss==bestloss) && w->lambda_grid[l]>w->lambda_grid[best])){
         best=l;bestloss=loss;
      }
   }
   w->selected_lambda[outer]=w->lambda_grid[best];w->inner_nvalid[outer]=nvalid;
   return 1;
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
      int nt=plan->ntrain[f],ne=plan->ntest[f];
      w->selected_lambda[f]=lambda;w->inner_nvalid[f]=0;
      if(w->nested && !ridge_tune_fold(w,f,x,p,y,q,group,err,nerr))return 0;
      if(!pp_refit(w->prep,w->preprocess,x,n,p,plan->train[f],nt,group,0,err,nerr) ||
         !pp_apply(w->prep,x,n,p,plan->train[f],nt,group,w->train_x,0,err,nerr) ||
         !pp_apply(w->prep,x,n,p,plan->test[f],ne,group,w->test_x,0,err,nerr))return 0;
      for(i=0;i<nt;i++)for(o=0;o<q;o++)w->train_y[(size_t)i*q+o]=y[(size_t)plan->train[f][i]*q+o];
      if(!THD_ridge_fit(w->ridge,w->train_x,w->train_y,nt,p,q,w->selected_lambda[f],THD_RIDGE_AUTO,err,nerr) ||
         !THD_ridge_predict(w->ridge,w->test_x,ne,w->test_y,err,nerr))return 0;
      for(o=0;o<q;o++){
         for(i=0;i<ne;i++){
            int s=plan->test[f][i];
            w->prediction[(size_t)s*q+o]=w->test_y[(size_t)i*q+o];
            w->baseline[(size_t)s*q+o]=(float)w->ridge->ym[o];
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

void THD_mvpa_workspace_free( THD_mvpa_workspace *w )
{
   if( w==NULL ) return ;
   THD_mvpa_ridge_workspace_free(w->ridge_cv);free(w->class_response);
   THD_svm_free(w->svm);free(w->svm_grid);free(w->svm_selected);free(w->svm_loss);
   free(w->fold_svm_residual);free(w->fold_svm_iterations);free(w->svm_inner_train);free(w->svm_inner_test);
   THD_lda_free(w->lda);free(w->fold_covariance_scale);free(w->fold_variance_floor);free(w->fold_covariance_form);
   free(w->train_x) ; free(w->test_x) ;
   free(w->train_class) ; free(w->test_class) ; free(w->test_prediction) ;
   free(w->test_decision) ; free(w->train_count) ;
   free(w->fold_confusion) ; free(w->fold_recall) ;
   free(w->prediction) ; free(w->decision) ; free(w->fold_nsample) ;
   free(w->fold_accuracy) ; free(w->fold_balanced_accuracy) ;
   free(w->confusion) ; free(w->recall) ;
   THD_preprocess_free(w->prep) ; THD_centroid_free(w->centroid) ; free(w) ;
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
   if(!w || w->ridge_cv || w->lda || w->svm || !isfinite(lambda) || lambda<=0 ||
      (!nested && (grid || nlambda)))
      return pp_error(err,nerr,"ridge classifier needs an unconfigured workspace and positive lambda");
   ridge=THD_mvpa_ridge_workspace_alloc(w->plan,w->nfeature_alloc,w->nclass,w->preprocess,err,nerr);
   if(!ridge)return 0;
   if(nested && !THD_mvpa_ridge_set_tuning(ridge,nested,grid,nlambda,err,nerr)){
      THD_mvpa_ridge_workspace_free(ridge);return 0;
   }
   response=malloc(sizeof(float)*(size_t)w->nsample*w->nclass);
   if(!response){THD_mvpa_ridge_workspace_free(ridge);return pp_error(err,nerr,"cannot allocate class responses");}
   w->ridge_cv=ridge;w->class_response=response;w->ridge_lambda=lambda;return 1;
}

int THD_mvpa_classification_set_lda(THD_mvpa_workspace *w,int mode,double alpha,
 double floor,int empirical,char *err,int nerr)
{
   THD_lda_model *lda;double *scale,*vf;int *form,f;
   if(!w || w->ridge_cv || w->lda || w->svm || !isfinite(floor) || floor<1e-12 || floor>1 ||
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

int THD_mvpa_classification_set_svm(THD_mvpa_workspace *w,double cost,double tol,
 int maxiter,int metric,const THD_nested_foldplan *nested,const double *grid,int ng,
 char *err,int nerr)
{
   THD_svm_model *svm;double *selected,*loss=NULL,*copy=NULL,*residual;
   int *iterations,*train=NULL,*test=NULL,f,i,j;
   if(!w || w->ridge_cv || w->lda || w->svm || !isfinite(cost) || cost<=0 ||
      !isfinite(.5/cost) || .5/cost<=0 || !isfinite(tol) || tol<1e-12 || tol>1e-2 || maxiter<1 ||
      (metric!=THD_METRIC_ACCURACY && metric!=THD_METRIC_BALANCED_ACCURACY) ||
      (nested?(!grid || ng<2 || nested->outer!=w->plan || !nested->inner):(grid || ng)))
      return pp_error(err,nerr,"SVM needs an unconfigured workspace, valid C/solver options and matching optional nested folds");
   if(nested){
      if((size_t)ng>SIZE_MAX/sizeof(double)/(size_t)w->nfold)
         return pp_error(err,nerr,"SVM tuning grid is too large");
      for(i=0;i<ng;i++){
         if(!isfinite(grid[i]) || grid[i]<=0 || !isfinite(.5/grid[i]) || .5/grid[i]<=0)
            return pp_error(err,nerr,"SVM C grid values must be finite, positive and numerically representable");
         for(j=0;j<i;j++)if(grid[i]==grid[j])return pp_error(err,nerr,"SVM C grid contains duplicates");
      }
      for(f=0;f<w->nfold;f++){
         const THD_foldplan *in=nested->inner[f];
         if(!THD_foldplan_validate(in,err,nerr))return 0;
         if(in->nsample!=w->plan->ntrain[f] || in->nfold<2 || in->kind==THD_CV_FIXED)
            return pp_error(err,nerr,"SVM tuning needs grouped inner folds over exactly each outer training set");
      }
   }
   svm=THD_svm_alloc(w->max_train,w->nfeature_alloc,w->nclass,err,nerr);
   selected=malloc(sizeof(double)*(size_t)w->nfold);residual=malloc(sizeof(double)*(size_t)w->nfold);
   iterations=malloc(sizeof(int)*(size_t)w->nfold);
   if(nested){
      copy=malloc(sizeof(double)*(size_t)ng);loss=malloc(sizeof(double)*(size_t)w->nfold*ng);
      train=malloc(sizeof(int)*(size_t)w->nsample);test=malloc(sizeof(int)*(size_t)w->nsample);
   }
   if(!svm || !selected || !residual || !iterations || (nested && (!copy || !loss || !train || !test))){
      THD_svm_free(svm);free(selected);free(residual);free(iterations);free(copy);free(loss);free(train);free(test);
      return pp_error(err,nerr,"cannot allocate SVM classification/tuning buffers");
   }
   if(nested)memcpy(copy,grid,sizeof(double)*(size_t)ng);
   w->svm=svm;w->svm_cost=cost;w->svm_tolerance=tol;w->svm_max_iter=maxiter;w->svm_metric=metric;
   w->svm_nested=nested;w->svm_grid=copy;w->svm_ngrid=ng;w->svm_selected=selected;w->svm_loss=loss;
   w->fold_svm_residual=residual;w->fold_svm_iterations=iterations;
   w->svm_inner_train=train;w->svm_inner_test=test;return 1;
}

static int svm_tune_fold(THD_mvpa_workspace *w,int outer,const float *x,int p,
 const int *classes,const int *groups,char *err,int nerr)
{
   const THD_foldplan *in=w->svm_nested->inner[outer];int l,f,i,best=0;
   double bestloss=HUGE_VAL;char why[512];
   for(l=0;l<w->svm_ngrid;l++){
      int total=0;THD_mvpa_class_score score;
      memset(w->fold_confusion,0,sizeof(int)*(size_t)w->nclass*w->nclass);
      for(f=0;f<in->nfold;f++){
         int nt=in->ntrain[f],ne=in->ntest[f];
         for(i=0;i<nt;i++)w->svm_inner_train[i]=w->plan->train[outer][in->train[f][i]];
         for(i=0;i<ne;i++)w->svm_inner_test[i]=w->plan->train[outer][in->test[f][i]];
         if(!pp_refit(w->prep,w->preprocess,x,w->nsample,p,w->svm_inner_train,nt,groups,0,why,sizeof(why)) ||
            !pp_apply(w->prep,x,w->nsample,p,w->svm_inner_train,nt,groups,w->train_x,0,why,sizeof(why)))
            return pp_error(err,nerr,"outer fold %s, inner fold %s SVM preprocessing: %s",w->plan->name[outer],in->name[f],why);
         for(i=0;i<nt;i++)w->train_class[i]=classes[w->svm_inner_train[i]];
         if(!THD_svm_fit(w->svm,w->train_x,w->train_class,nt,p,w->svm_grid[l],w->svm_tolerance,w->svm_max_iter,why,sizeof(why)))
            return pp_error(err,nerr,"outer fold %s, inner fold %s, C %.9g: %s",w->plan->name[outer],in->name[f],w->svm_grid[l],why);
         /* An inner test group can exceed the largest outer test group. Apply
            one row at a time so existing test buffers remain sufficient. */
         for(i=0;i<ne;i++){
            int row=w->svm_inner_test[i];
            if(!pp_apply(w->prep,x,w->nsample,p,&row,1,groups,w->test_x,0,why,sizeof(why)) ||
               !THD_svm_predict(w->svm,w->test_x,1,p,w->test_prediction,NULL,why,sizeof(why)))
               return pp_error(err,nerr,"outer fold %s, inner fold %s SVM prediction: %s",w->plan->name[outer],in->name[f],why);
            w->fold_confusion[(size_t)classes[row]*w->nclass+w->test_prediction[0]]++;total++;
         }
      }
      mvpa_class_score_from_confusion(w->fold_confusion,total,w->nclass,w->fold_recall,&score);
      {
         double loss=1-(w->svm_metric==THD_METRIC_ACCURACY?score.accuracy:score.balanced_accuracy);
         if(!isfinite(loss))return pp_error(err,nerr,"SVM inner validation score is undefined");
         w->svm_loss[(size_t)outer*w->svm_ngrid+l]=loss;
         if(loss<bestloss || (loss==bestloss && w->svm_grid[l]<w->svm_grid[best])){best=l;bestloss=loss;}
      }
   }
   w->svm_selected[outer]=w->svm_grid[best];return 1;
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
   if( !w->ridge_cv && !w->lda && !w->svm && THD_centroid_distance_name(distance)==NULL )
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
     if(!THD_mvpa_run_ridge_cv(w->ridge_cv,x,nfeature,w->class_response,w->nclass,
                              group_id,w->ridge_lambda,why,sizeof(why)))
       return pp_error(err,nerr,"ridge classification: %s",why);
   }

   for( ii=0 ; ii<w->nsample ; ii++ ){
     w->prediction[ii]=-1 ;
     for( jj=0 ; jj<w->nclass ; jj++ )
       w->decision[(size_t)ii*w->nclass+jj]=NAN ;
   }
   memset(w->confusion,0,sizeof(int)*(size_t)w->nclass*w->nclass) ;
   w->ntest=0 ;
   for( ff=0 ; ff<w->nfold ; ff++ ){
     int ntrain=plan->ntrain[ff],ntest=plan->ntest[ff] ;
     if(!w->ridge_cv){
     if(w->svm){
       w->svm_selected[ff]=w->svm_cost;
       if(w->svm_nested && !svm_tune_fold(w,ff,x,nfeature,class_id,group_id,why,sizeof(why)))
         return pp_error(err,nerr,"SVM tuning: %s",why);
     }
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
     if(w->svm){
       if(!THD_svm_fit(w->svm,w->train_x,w->train_class,ntrain,nfeature,w->svm_selected[ff],
                      w->svm_tolerance,w->svm_max_iter,why,sizeof(why)) ||
          !THD_svm_predict(w->svm,w->test_x,ntest,nfeature,w->test_prediction,w->test_decision,why,sizeof(why)))
         return pp_error(err,nerr,"fold %s SVM: %s",plan->name[ff],why);
       w->fold_svm_iterations[ff]=w->svm->max_iterations;w->fold_svm_residual[ff]=w->svm->residual;
     }else if(w->lda){
       if(!THD_lda_fit(w->lda,w->train_x,w->train_class,ntrain,nfeature,w->lda_shrinkage,
                      w->lda_variance_floor,w->lda_empirical_prior,why,sizeof(why)) ||
          !THD_lda_predict(w->lda,w->test_x,ntest,nfeature,w->test_prediction,w->test_decision,why,sizeof(why)))
         return pp_error(err,nerr,"fold %s LDA: %s",plan->name[ff],why);
       w->fold_covariance_scale[ff]=w->lda->reference;
       w->fold_variance_floor[ff]=w->lda->reference*w->lda_variance_floor;
       w->fold_covariance_form[ff]=w->lda->form;
     }else{
     if( !centroid_refit(w->centroid,distance,w->train_x,w->train_class,
                         ntrain,nfeature,why,sizeof(why)) )
       return pp_error(err,nerr,"fold %s classifier fit: %s",plan->name[ff],why) ;
     if( !THD_centroid_predict(w->centroid,w->test_x,ntest,nfeature,
                               w->test_prediction,w->test_decision,why,sizeof(why)) )
       return pp_error(err,nerr,"fold %s prediction: %s",plan->name[ff],why) ;
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
   if(w && (w->ridge_cv || w->lda || w->svm))return pp_error(err,nerr,"centroid runner needs a centroid-configured workspace");
   return THD_mvpa_run_classification_cv(plan,distance,x,p,classes,groups,w,err,nerr);
}

int THD_mvpa_run_centroid_permutations(const THD_foldplan *plan,int distance,
 const float *x,int p,const int *classes,const int *groups,int metric,
 const PERM_set *pset,int *permuted,float *scores,THD_mvpa_workspace *w,char *err,int nerr)
{
   if(w && (w->ridge_cv || w->lda || w->svm))return pp_error(err,nerr,"centroid runner needs a centroid-configured workspace");
   return THD_mvpa_run_classification_permutations(plan,distance,x,p,classes,groups,
      metric,pset,permuted,scores,w,err,nerr);
}
