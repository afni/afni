#include "thd_sampleset.h"

#include <stdarg.h>
#include <stdint.h>

/*----------------------------------------------------------------------------
  Canonical sample organization for pattern analyses.  This file knows table
  rows, dataset selectors, and metadata levels; it knows nothing about models,
  folds, spatial neighborhoods, or inference.
                                                     -- P Molfese, Oct 2026
------------------------------------------------------------------------------*/

static int ss_error( char *err, int nerr, const char *fmt, ... )
{
   va_list ap ;
   if( err != NULL && nerr > 0 ){
     va_start(ap,fmt) ; vsnprintf(err,(size_t)nerr,fmt,ap) ; va_end(ap) ;
   }
   return 0 ;
}

void THD_sampleset_free_identity( THD_sampleset_identity *identity )
{
   int dd ;
   if( identity==NULL ) return ;
   if( identity->dataset_path!=NULL )
     for( dd=0 ; dd<identity->ndset ; dd++ ) free(identity->dataset_path[dd]) ;
   free(identity->dataset_path) ; free(identity->first_sample) ; free(identity) ;
}

THD_sampleset_identity * THD_sampleset_build_identity( const THD_sampleset *ss,
                                                       char *err, int nerr )
{
   THD_sampleset_identity *identity=NULL ; int dd,ii,jj ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( ss==NULL || ss->nsample<1 || ss->ndset<1 || ss->dset==NULL ||
       ss->sample_dset==NULL || ss->sample_brick==NULL ){
     ss_error(err,nerr,"sample identity requires a nonempty resolved sample set") ; return NULL ;
   }
   if( (size_t)ss->nsample>SIZE_MAX/sizeof(int) ||
       (size_t)ss->ndset>SIZE_MAX/sizeof(char *) ){
     ss_error(err,nerr,"sample identity map is too large") ; return NULL ;
   }
   identity=(THD_sampleset_identity *)calloc(1,sizeof(*identity)) ;
   if( identity==NULL ) goto nomem ;
   identity->nsample=ss->nsample ; identity->ndset=ss->ndset ;
   identity->first_sample=(int *)malloc(sizeof(int)*(size_t)ss->nsample) ;
   identity->dataset_path=(char **)calloc((size_t)ss->ndset,sizeof(char *)) ;
   if( identity->first_sample==NULL || identity->dataset_path==NULL ) goto nomem ;
   for( dd=0 ; dd<ss->ndset ; dd++ ){
     const char *name ;
     if( !ISVALID_DSET(ss->dset[dd]) ||
         (name=DSET_HEADNAME(ss->dset[dd]))==NULL || *name=='\0' ){
       ss_error(err,nerr,"sample identity: dataset %d has no valid header path",dd+1) ; goto fail ;
     }
     identity->dataset_path[dd]=realpath(name,NULL) ;
     if( identity->dataset_path[dd]==NULL ) identity->dataset_path[dd]=strdup(name) ;
     if( identity->dataset_path[dd]==NULL ) goto nomem ;
   }
   for( ii=0 ; ii<ss->nsample ; ii++ ){
     dd=ss->sample_dset[ii] ;
     if( dd<0 || dd>=ss->ndset || ss->sample_brick[ii]<0 ||
         ss->sample_brick[ii]>=DSET_NVALS(ss->dset[dd]) ){
       ss_error(err,nerr,"sample identity: sample %d has an invalid dataset/brick index",ii+1) ; goto fail ;
     }
     identity->first_sample[ii]=ii ;
     for( jj=0 ; jj<ii ; jj++ )
       if( ss->sample_brick[ii]==ss->sample_brick[jj] &&
           !strcmp(identity->dataset_path[dd],identity->dataset_path[ss->sample_dset[jj]]) ){
         identity->first_sample[ii]=jj ; break ;
       }
   }
   return identity ;
nomem:
   ss_error(err,nerr,"cannot allocate sample identity map") ;
fail:
   THD_sampleset_free_identity(identity) ; return NULL ;
}

void THD_sampleset_free_levels( char **level, int nlevel )
{
   int ii ;
   if( level == NULL ) return ;
   for( ii=0 ; ii<nlevel ; ii++ ) free(level[ii]) ;
   free(level) ;
}

static int ss_level( char **level, int nlevel, const char *value )
{
   int ii ;
   for( ii=0 ; ii<nlevel ; ii++ ) if( strcmp(level[ii],value)==0 ) return ii ;
   return -1 ;
}

static int ss_encode_table_column( const THD_datatable *dt, const char *column,
                                   int **code_out, char ***level_out, int *nlevel_out,
                                   char *err, int nerr )
{
   int icol,rr,nlevel=0,*code ; char **level=NULL ;
   if( code_out==NULL || level_out==NULL || nlevel_out==NULL )
     return ss_error(err,nerr,"internal error: no output for column encoding") ;
   *code_out=NULL ; *level_out=NULL ; *nlevel_out=0 ;
   if( dt==NULL || column==NULL )
     return ss_error(err,nerr,"cannot encode an unnamed metadata column") ;
   icol=THD_datatable_column((THD_datatable *)dt,(char *)column) ;
   if( icol<0 )
     return ss_error(err,nerr,"dataTable (%s): column '%s' is missing",
                     dt->source?dt->source:"?",column) ;
   code=(int *)malloc(sizeof(int)*(size_t)dt->nrow) ;
   if( code==NULL ) return ss_error(err,nerr,"out of memory encoding column '%s'",column) ;
   for( rr=0 ; rr<dt->nrow ; rr++ ){
     const char *s=DT_CELL(dt,rr,icol) ; int ll ;
     if( s==NULL || *s=='\0' ){
       free(code) ; THD_sampleset_free_levels(level,nlevel) ;
       return ss_error(err,nerr,"dataTable (%s): empty '%s' in row %d",
                       dt->source?dt->source:"?",column,rr+1) ;
     }
     ll=ss_level(level,nlevel,s) ;
     if( ll<0 ){
       char **more=(char **)realloc(level,sizeof(char *)*(size_t)(nlevel+1)) ;
       if( more==NULL ){
         free(code) ; THD_sampleset_free_levels(level,nlevel) ;
         return ss_error(err,nerr,"out of memory encoding column '%s'",column) ;
       }
       level=more ; level[nlevel]=strdup(s) ;
       if( level[nlevel]==NULL ){
         free(code) ; THD_sampleset_free_levels(level,nlevel) ;
         return ss_error(err,nerr,"out of memory encoding column '%s'",column) ;
       }
       ll=nlevel++ ;
     }
     code[rr]=ll ;
   }
   *code_out=code ; *level_out=level ; *nlevel_out=nlevel ;
   return 1 ;
}

int THD_sampleset_encode_column( const THD_sampleset *ss, const char *column,
                                 int **code, char ***level, int *nlevel,
                                 char *err, int nerr )
{
   if( ss==NULL ) return ss_error(err,nerr,"cannot encode a column of a NULL sample set") ;
   return ss_encode_table_column(ss->metadata,column,code,level,nlevel,err,nerr) ;
}

float * THD_sampleset_numeric_columns( const THD_sampleset *ss,
                                      const char * const *columns, int ncolumn,
                                      char *err, int nerr )
{
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( ss==NULL || ss->nsample<1 || ss->table_row==NULL ){
     ss_error(err,nerr,"numeric extraction requires samples with a table-row map") ; return NULL ;
   }
   return THD_datatable_numeric_columns(ss->metadata,columns,ncolumn,
                                        ss->table_row,ss->nsample,err,nerr) ;
}

void THD_sampleset_detect_columns( const THD_datatable *dt,
                                   THD_sampleset_columns *columns )
{
   if( columns==NULL ) return ;
   memset(columns,0,sizeof(*columns)) ;
   if( dt==NULL ) return ;
   if( THD_datatable_column((THD_datatable *)dt,"Subj")>=0 ) columns->subject="Subj" ;
   if( THD_datatable_column((THD_datatable *)dt,"Run")>=0 ) columns->run="Run" ;
   if( THD_datatable_column((THD_datatable *)dt,"Trial")>=0 ) columns->item="Trial" ;
   else if( THD_datatable_column((THD_datatable *)dt,"Item")>=0 ) columns->item="Item" ;
   if( THD_datatable_column((THD_datatable *)dt,"Fold")>=0 ) columns->fold="Fold" ;
   if( THD_datatable_column((THD_datatable *)dt,"Sample")>=0 ) columns->sample="Sample" ;
}

void THD_sampleset_free( THD_sampleset *ss )
{
   int ii ;
   if( ss==NULL ) return ;
   if( ss->dset!=NULL ) for( ii=0 ; ii<ss->ndset ; ii++ ) if( ss->dset[ii]!=NULL ) DSET_delete(ss->dset[ii]) ;
   if( ss->dset_name!=NULL ) for( ii=0 ; ii<ss->ndset ; ii++ ) free(ss->dset_name[ii]) ;
   if( ss->sample_label!=NULL ) for( ii=0 ; ii<ss->nsample ; ii++ ) free(ss->sample_label[ii]) ;
   free(ss->dset) ; free(ss->dset_name) ; free(ss->sample_dset) ; free(ss->sample_brick) ;
   free(ss->subject) ; free(ss->run) ; free(ss->item) ; free(ss->fold) ;
   THD_sampleset_free_levels(ss->subject_label,ss->nsubject) ;
   THD_sampleset_free_levels(ss->run_label,ss->nrun) ;
   THD_sampleset_free_levels(ss->item_label,ss->nitem) ;
   THD_sampleset_free_levels(ss->fold_label,ss->nfold) ;
   free(ss->sample_label) ; free(ss->table_row) ;
   THD_free_datatable(ss->metadata) ; free(ss->source) ; free(ss) ;
}

static THD_sampleset * ss_fail( THD_sampleset *ss, char *err, int nerr,
                                const char *fmt, ... )
{
   va_list ap ;
   if( err!=NULL && nerr>0 ){
     va_start(ap,fmt) ; vsnprintf(err,(size_t)nerr,fmt,ap) ; va_end(ap) ;
   }
   THD_sampleset_free(ss) ; return NULL ;
}

static THD_sampleset * ss_fail_encoded( THD_sampleset *ss )
{
   THD_sampleset_free(ss) ; return NULL ;
}

/*! Separate the base dataset from one optional [...] sub-brick selector. */
static int ss_dataset_spec( const char *spec, char **base_out, const char **sel_out,
                            char *err, int nerr )
{
   const char *sel,*end ; size_t n ; char *base ;
   *base_out=NULL ; *sel_out=NULL ;
   if( spec==NULL || *spec=='\0' ) return ss_error(err,nerr,"empty InputFile") ;
   if( strchr(spec,'<')!=NULL || strchr(spec,'{')!=NULL )
     return ss_error(err,nerr,"InputFile '%s': use one sub-brick selector [..]; <..> and {..} selectors are not sample selectors",spec) ;
   sel=strchr(spec,'[') ;
   if( sel==NULL ) n=strlen(spec) ;
   else {
     end=strrchr(sel,']') ;
     if( end==NULL || end[1]!='\0' )
       return ss_error(err,nerr,"InputFile '%s': malformed sub-brick selector",spec) ;
     n=(size_t)(sel-spec) ;
   }
   if( n==0 ) return ss_error(err,nerr,"InputFile '%s': no base dataset before the selector",spec) ;
   base=(char *)malloc(n+1) ;
   if( base==NULL ) return ss_error(err,nerr,"out of memory resolving InputFile '%s'",spec) ;
   memcpy(base,spec,n) ; base[n]='\0' ;
   *base_out=base ; *sel_out=sel ; return 1 ;
}

static int ss_encode_role( THD_sampleset *ss, const char *column, int **code,
                           char ***level, int *nlevel, char *err, int nerr )
{
   int ii ;
   *code=(int *)malloc(sizeof(int)*(size_t)ss->nsample) ;
   if( *code==NULL ) return ss_error(err,nerr,"out of memory allocating sample metadata") ;
   if( column==NULL ){
     for( ii=0 ; ii<ss->nsample ; ii++ ) (*code)[ii]=-1 ;
     *level=NULL ; *nlevel=0 ; return 1 ;
   }
   free(*code) ; *code=NULL ;
   return ss_encode_table_column(ss->metadata,column,code,level,nlevel,err,nerr) ;
}

THD_sampleset * THD_sampleset_from_table( const THD_datatable *dt,
                                           const THD_sampleset_columns *given,
                                           char *err, int nerr )
{
   THD_sampleset *ss ; THD_sampleset_columns detected ; const THD_sampleset_columns *col=given ;
   int rr,*rows ;
   if( err!=NULL && nerr>0 ) err[0]='\0' ;
   if( dt==NULL || dt->nrow<1 ){
     ss_error(err,nerr,"cannot build samples from an empty dataTable") ; return NULL ;
   }
   if( dt->icol_input<0 ){
     ss_error(err,nerr,"dataTable (%s): an InputFile column is required",dt->source?dt->source:"?") ; return NULL ;
   }
   if( col==NULL ){ THD_sampleset_detect_columns(dt,&detected) ; col=&detected ; }

   ss=(THD_sampleset *)calloc(1,sizeof(*ss)) ;
   if( ss==NULL ){ ss_error(err,nerr,"out of memory building the sample set") ; return NULL ; }
   ss->nsample=dt->nrow ; ss->source=strdup(dt->source?dt->source:"dataTable") ;
   rows=(int *)malloc(sizeof(int)*(size_t)dt->nrow) ;
   if( rows==NULL ) return ss_fail(ss,err,nerr,"out of memory copying the dataTable") ;
   for( rr=0 ; rr<dt->nrow ; rr++ ) rows[rr]=rr ;
   ss->metadata=THD_datatable_select_rows((THD_datatable *)dt,rows,dt->nrow) ; free(rows) ;
   if( ss->metadata==NULL ) return ss_fail(ss,err,nerr,"could not copy the dataTable") ;
   free(ss->metadata->source) ; ss->metadata->source=strdup(ss->source) ;

   if( col->subject!=NULL ){
     if( !ss_encode_role(ss,col->subject,&ss->subject,&ss->subject_label,&ss->nsubject,err,nerr) )
       return ss_fail_encoded(ss) ;
   } else {
     ss->subject=(int *)calloc((size_t)ss->nsample,sizeof(int)) ;
     ss->subject_label=(char **)malloc(sizeof(char *)) ;
     if( ss->subject==NULL || ss->subject_label==NULL ) return ss_fail(ss,err,nerr,"out of memory encoding subjects") ;
     ss->subject_label[0]=strdup("All") ; ss->nsubject=1 ;
   }
   if( !ss_encode_role(ss,col->run,&ss->run,&ss->run_label,&ss->nrun,err,nerr) )
     return ss_fail_encoded(ss) ;
   if( !ss_encode_role(ss,col->item,&ss->item,&ss->item_label,&ss->nitem,err,nerr) )
     return ss_fail_encoded(ss) ;
   if( !ss_encode_role(ss,col->fold,&ss->fold,&ss->fold_label,&ss->nfold,err,nerr) )
     return ss_fail_encoded(ss) ;

   ss->sample_label=(char **)calloc((size_t)ss->nsample,sizeof(char *)) ;
   ss->table_row=(int *)malloc(sizeof(int)*(size_t)ss->nsample) ;
   ss->sample_dset=(int *)malloc(sizeof(int)*(size_t)ss->nsample) ;
   ss->sample_brick=(int *)malloc(sizeof(int)*(size_t)ss->nsample) ;
   ss->dset=(THD_3dim_dataset **)calloc((size_t)ss->nsample,sizeof(THD_3dim_dataset *)) ;
   ss->dset_name=(char **)calloc((size_t)ss->nsample,sizeof(char *)) ;
   if( ss->sample_label==NULL || ss->table_row==NULL || ss->sample_dset==NULL || ss->sample_brick==NULL ||
       ss->dset==NULL || ss->dset_name==NULL ) return ss_fail(ss,err,nerr,"out of memory allocating samples") ;

   { int icol=(col->sample!=NULL) ? THD_datatable_column(ss->metadata,(char *)col->sample) : -1 ;
     if( col->sample!=NULL && icol<0 ) return ss_fail(ss,err,nerr,"dataTable (%s): column '%s' is missing",ss->source,col->sample) ;
     for( rr=0 ; rr<ss->nsample ; rr++ ){
       char label[32] ; const char *s ; int ii ;
       ss->table_row[rr]=rr ;
       if( icol>=0 ) s=DT_CELL(ss->metadata,rr,icol) ;
       else { snprintf(label,sizeof(label),"row%06d",rr+1) ; s=label ; }
       if( s==NULL || *s=='\0' ) return ss_fail(ss,err,nerr,"dataTable (%s): empty sample label in row %d",ss->source,rr+1) ;
       for( ii=0 ; ii<rr ; ii++ ) if( strcmp(ss->sample_label[ii],s)==0 )
         return ss_fail(ss,err,nerr,"dataTable (%s): duplicate sample label '%s' in rows %d and %d",ss->source,s,ii+1,rr+1) ;
       ss->sample_label[rr]=strdup(s) ;
     }
   }

   for( rr=0 ; rr<ss->nsample ; rr++ ){
     const char *spec=ss->metadata->fname[rr],*sel=NULL ; char *base=NULL,why[512] ; int dd,brick=0 ;
     THD_3dim_dataset *ds ;
     if( !ss_dataset_spec(spec,&base,&sel,why,sizeof(why)) )
       return ss_fail(ss,err,nerr,"dataTable (%s), row %d: %s",ss->source,rr+1,why) ;
     for( dd=0 ; dd<ss->ndset ; dd++ ) if( strcmp(ss->dset_name[dd],base)==0 ) break ;
     if( dd==ss->ndset ){
       ds=THD_open_dataset(base) ;
       if( ds==NULL ){ free(base) ; return ss_fail(ss,err,nerr,"dataTable (%s), row %d: cannot open dataset '%s'",ss->source,rr+1,spec) ; }
       if( ss->ndset>0 && !EQUIV_GRIDS(ds,ss->dset[0]) ){
         DSET_delete(ds) ; free(base) ;
         return ss_fail(ss,err,nerr,"dataTable (%s), row %d: dataset '%s' is not on the grid of '%s'",
                        ss->source,rr+1,spec,ss->dset_name[0]) ;
       }
       if( ss->ndset==0 ) ss->nvox=DSET_NVOX(ds) ;
       ss->dset[dd]=ds ; ss->dset_name[dd]=base ; ss->ndset++ ;
     } else { ds=ss->dset[dd] ; free(base) ; }
     if( sel!=NULL ){
       int *iv=MCW_get_thd_intlist(ds,(char *)sel) ;
       if( iv==NULL || iv[0]!=1 ){
         int nsel=(iv==NULL)?0:iv[0] ; free(iv) ;
         return ss_fail(ss,err,nerr,"dataTable (%s), row %d: InputFile '%s' selects %d sub-bricks; each sample needs exactly one",
                        ss->source,rr+1,spec,nsel) ;
       }
       brick=iv[1] ; free(iv) ;
     } else if( DSET_NVALS(ds)!=1 )
       return ss_fail(ss,err,nerr,"dataTable (%s), row %d: dataset '%s' has %d sub-bricks; select exactly one with [..]",
                      ss->source,rr+1,spec,DSET_NVALS(ds)) ;
     ss->sample_dset[rr]=dd ; ss->sample_brick[rr]=brick ;
   }
   return ss ;
}

void THD_sampleset_print( const THD_sampleset *ss, FILE *fp )
{
   int ii ;
   if( ss==NULL || fp==NULL ) return ;
   fprintf(fp,"# samples from %s: %d rows, %d dataset%s, %d subject%s\n",
           ss->source?ss->source:"?",ss->nsample,ss->ndset,ss->ndset==1?"":"s",
           ss->nsubject,ss->nsubject==1?"":"s") ;
   fprintf(fp,"Row\tSample\tSubj\tRun\tItem\tFold\tDataset\tBrick\tBrickLabel\n") ;
   for( ii=0 ; ii<ss->nsample ; ii++ ){
     THD_3dim_dataset *ds=ss->dset[ss->sample_dset[ii]] ;
     const char *bl=DSET_BRICK_LABEL(ds,ss->sample_brick[ii]) ;
     fprintf(fp,"%d\t%s\t%s\t%s\t%s\t%s\t%s\t%d\t%s\n",ss->table_row[ii]+1,ss->sample_label[ii],
             ss->subject_label[ss->subject[ii]],ss->run[ii]>=0?ss->run_label[ss->run[ii]]:"-",
             ss->item[ii]>=0?ss->item_label[ss->item[ii]]:"-",
             ss->fold[ii]>=0?ss->fold_label[ss->fold[ii]]:"-",
             ss->dset_name[ss->sample_dset[ii]],ss->sample_brick[ii],bl?bl:"-") ;
   }
}
