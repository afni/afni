/* Unit tests for the file syntax handled by thd_datatable.c. */

#include "mrilib.h"
#include "thd_datatable.h"
#include <limits.h>

#define CHECK(COND,MSG) do{ if(!(COND)){ fprintf(stderr,"FAIL: %s\n",MSG); return 1; } }while(0)

static int write_table( const char *fn )
{
   FILE *fp=fopen(fn,"w") ;
   if( fp==NULL ) return 1 ;
   fprintf(fp,
     "# a full-line comment\n"
     "Subj Run InputFile Note\n"
     "s01 1 \"stats+tlrc[face#0_Coef]\" \"quoted # value\" # trailing comment\n"
     "s02 2 stats+tlrc[house#1_Coef] plain#value\n"
     "s03 3 stats+tlrc[cat#2_Coef] \"#not-a-comment\"\n") ;
   fclose(fp) ;
   return 0 ;
}

static int check_table( THD_datatable *dt )
{
   int inote ;
   CHECK(dt!=NULL,"read the table") ;
   inote=THD_datatable_column(dt,"Note") ;
   CHECK(dt->nrow==3 && dt->ncol==4,"comments do not change the table shape") ;
   CHECK(inote==3,"find the Note column") ;
   CHECK(!strcmp(dt->fname[0],"stats+tlrc[face#0_Coef]"),
         "a hash inside a quoted AFNI selector is data") ;
   CHECK(!strcmp(dt->fname[1],"stats+tlrc[house#1_Coef]"),
         "a hash inside an unquoted AFNI selector is data") ;
   CHECK(!strcmp(DT_CELL(dt,0,inote),"quoted # value"),
         "quoted whitespace and hash are data") ;
   CHECK(!strcmp(DT_CELL(dt,1,inote),"plain#value"),
         "an embedded unquoted hash is data") ;
   CHECK(!strcmp(DT_CELL(dt,2,inote),"#not-a-comment"),
         "a quoted value may begin with a hash") ;
   return 0 ;
}

static int check_concat(const char *fn)
{
   THD_datatable *a,*b,*out,*dup,*plain;THD_datatable shape={0};
   char err[256],tiny[2]={'x','x'},*saved;FILE *fp;int c;
   fp=fopen(fn,"w");CHECK(fp,"open first concatenation fixture");
   fputs("Subj InputFile Numeric Mixed Note\n"
         "s1 relative/a+orig 10 1 alpha\n"
         "s2 relative/b+orig 20 2 beta\n",fp);fclose(fp);
   a=THD_read_datatable_file((char *)fn);
   fp=fopen(fn,"w");CHECK(fp,"open second concatenation fixture");
   fputs("note MIXED numeric inputfile SUBJ\n"
         "gamma 3 30 ../c.nii s3\n"
         "delta text 40 ../d.nii s4\n",fp);fclose(fp);
   b=THD_read_datatable_file((char *)fn);
   strcpy(err,"stale error");out=THD_datatable_concat_rows(a,b,err,sizeof(err));
   CHECK(out && !err[0],"concatenation succeeds and clears error");
   CHECK(out->nrow==4 && out->ncol==5,"concatenation shape");
   for(c=0;c<a->ncol;c++)CHECK(!strcmp(out->cname[c],a->cname[c]),"first schema order and spelling preserved");
   CHECK(out->icol_input==1 && out->icol_subj==0,"special columns need not be last");
   CHECK(!strcmp(out->subj[2],"s3") && !strcmp(out->fname[2],"../c.nii"),"reordered subject/file caches and verbatim paths");
   CHECK(out->isnum[2] && out->val[2][0]==10 && out->val[2][3]==40,"numeric metadata rebuilt across tables");
   CHECK(a->isnum[3] && !out->isnum[3] && !out->val[3],"mixed concatenation cannot retain a stale numeric cache");
   CHECK(!strcmp(DT_CELL(out,2,4),"gamma") && !strcmp(DT_CELL(out,3,3),"text"),"second rows matched by names");
   CHECK(!out->from_argv && out->source && out->source!=a->source,"derived source metadata owned");
   CHECK(out->cname[0]!=a->cname[0] && out->cell[0]!=a->cell[0] &&
         out->subj[0]!=a->subj[0] && out->fname[0]!=a->fname[0] &&
         out->val[2]!=a->val[2],"output storage does not alias inputs");
   DT_CELL(out,0,4)[0]='A';
   CHECK(!strcmp(DT_CELL(a,0,4),"alpha"),"changing output does not change input");
   DT_CELL(b,0,0)[0]='G';
   CHECK(!strcmp(DT_CELL(out,2,4),"gamma"),"changing input does not change output");
   dup=THD_datatable_concat_rows(a,a,NULL,0);
   CHECK(dup && dup->nrow==4 && !strcmp(dup->subj[0],dup->subj[2]),"self-concatenation permits repeated rows and IDs");
   THD_free_datatable(dup);

   CHECK(!THD_datatable_concat_rows(NULL,b,err,sizeof(err)) && strstr(err,"nonempty"),"NULL table rejected");
   CHECK(!THD_datatable_concat_rows(a,&shape,err,sizeof(err)),"empty table rejected");
   shape.nrow=INT_MAX;shape.ncol=a->ncol;
   CHECK(!THD_datatable_concat_rows(a,&shape,err,sizeof(err)) && strstr(err,"too many"),"row sum overflow rejected before storage access");
   shape.nrow=INT_MAX/a->ncol;
   CHECK(!THD_datatable_concat_rows(a,&shape,err,sizeof(err)) && strstr(err,"too many"),"cell product overflow rejected");
   shape.ncol=a->ncol+1;
   CHECK(!THD_datatable_concat_rows(a,&shape,err,sizeof(err)) && strstr(err,"same column names"),"different schema sizes rejected");
   saved=b->cname[0];b->cname[0]="Other";
   CHECK(!THD_datatable_concat_rows(a,b,err,sizeof(err)) && strstr(err,"missing column 'Note'"),"missing/extra column rejected");
   CHECK(!THD_datatable_concat_rows(a,b,tiny,sizeof(tiny)) && !tiny[1],"error message bounded and terminated");
   b->cname[0]="NUMERIC";
   CHECK(!THD_datatable_concat_rows(a,b,err,sizeof(err)) && strstr(err,"duplicate column"),"duplicate second-column names rejected case-insensitively");
   CHECK(!THD_datatable_concat_rows(b,a,err,sizeof(err)) && strstr(err,"duplicate column"),"duplicate first-column names rejected");
   b->cname[0]="";
   CHECK(!THD_datatable_concat_rows(a,b,err,sizeof(err)) && strstr(err,"empty column"),"empty column name rejected");
   b->cname[0]=saved;
   saved=b->cell[0];b->cell[0]=NULL;
   CHECK(!THD_datatable_concat_rows(a,b,err,sizeof(err)) && strstr(err,"missing cell"),"missing cell failure cleans partial result");
   b->cell[0]=saved;
   THD_free_datatable(a);THD_free_datatable(b);
   CHECK(!strcmp(out->fname[3],"../d.nii") && out->val[2][3]==40,"output survives freeing both inputs");
   THD_free_datatable(out);

   /* A general table helper must not require datasets, subjects or MVPA roles. */
   fp=fopen(fn,"w");CHECK(fp,"open metadata-only fixture");
   fputs("Feature Weight\nvisual 1.25\naudio 2.5\n",fp);fclose(fp);
   plain=THD_read_datatable_file((char *)fn);
   out=THD_datatable_concat_rows(plain,plain,err,sizeof(err));
   CHECK(out && out->ncol==2 && out->icol_input==-1 && !out->fname && out->icol_subj==-1,
         "metadata-only tables need no special columns");
   CHECK(!strcmp(out->subj[2],"row003") && out->val[1][3]==2.5f,"synthetic subjects and numeric values rebuilt");
   CHECK(THD_datatable_column(out,"MVPA_Partition")==-1,"generic helper injects no analysis roles");
   THD_free_datatable(plain);THD_free_datatable(out);
   return 0;
}

static int check_numeric_columns(const char *fn)
{
   THD_datatable *dt,invalid;FILE *fp;float *v;char err[256],tiny[2]={'x','x'};
   const char *columns[]={"B","a"},*duplicate[]={"A","a"},*empty[]={""},*nullname[]={NULL};
   const char *badnames[]={"Missing","Text","Subj","InputFile"};
   const char *nonfinite[]={"BadNan","BadInf","Overflow"};
   int rows[]={2,0,2},badrow=-1,goodrow=0,nonfinite_row=1,i;
   const float expected[]={30,300,10,1.25f,30,300};
   fp=fopen(fn,"w");CHECK(fp,"open numeric extraction fixture");
   fputs("Subj A B Text BadNan BadInf Overflow InputFile\n"
         "1 1.25 10 1 1 1 1 mock_a\n"
         "2 -2 20 word nan inf 1e100 mock_b\n"
         "3 3e2 30 3 3 3 3 mock_c\n",fp);fclose(fp);
   dt=THD_read_datatable_file((char *)fn);
   strcpy(err,"stale error");v=THD_datatable_numeric_columns(dt,columns,2,NULL,3,err,sizeof(err));
   CHECK(v && !err[0],"numeric extraction succeeds and clears error");
   CHECK(v[0]==10 && v[1]==1.25f && v[2]==20 && v[3]==-2 && v[4]==30 && v[5]==300,
         "case-insensitive requested columns copied in row-major order");
   v[0]=-999;CHECK(dt->val[2][0]==10,"numeric matrix does not alias table cache");free(v);
   v=THD_datatable_numeric_columns(dt,columns,2,rows,3,NULL,0);CHECK(v,"mapped rows extract without error buffer");
   for(i=0;i<6;i++)CHECK(v[i]==expected[i],"reordered and repeated row values");free(v);
   for(i=0;i<4;i++)
     CHECK(!THD_datatable_numeric_columns(dt,&badnames[i],1,&goodrow,1,err,sizeof(err)) && strstr(err,"finite numeric"),
           "missing/nonnumeric/special columns rejected, including nonnumeric cells outside selection");
   CHECK(!THD_datatable_numeric_columns(dt,duplicate,2,NULL,3,err,sizeof(err)) && strstr(err,"duplicate numeric"),
         "duplicate columns rejected case-insensitively");
   CHECK(!THD_datatable_numeric_columns(dt,empty,1,NULL,3,err,sizeof(err)),"empty requested name rejected");
   CHECK(!THD_datatable_numeric_columns(dt,nullname,1,NULL,3,err,sizeof(err)),"NULL requested name rejected");
   for(i=0;i<3;i++){
     CHECK(!THD_datatable_numeric_columns(dt,&nonfinite[i],1,&nonfinite_row,1,err,sizeof(err)) &&
           strstr(err,"output row 1 (table row 2) is not finite"),"NaN, infinity and float overflow rejected with row provenance");
     v=THD_datatable_numeric_columns(dt,&nonfinite[i],1,&goodrow,1,err,sizeof(err));
     CHECK(v && v[0]==1,"finite check applies to selected rows");free(v);
   }
   CHECK(!THD_datatable_numeric_columns(NULL,columns,2,NULL,3,err,sizeof(err)),"NULL table rejected");
   CHECK(!THD_datatable_numeric_columns(dt,NULL,2,NULL,3,err,sizeof(err)),"NULL column list rejected");
   CHECK(!THD_datatable_numeric_columns(dt,columns,0,NULL,3,err,sizeof(err)),"empty column list rejected");
   CHECK(!THD_datatable_numeric_columns(dt,columns,2,rows,0,err,sizeof(err)),"empty row selection rejected");
   CHECK(!THD_datatable_numeric_columns(dt,columns,-1,NULL,3,err,sizeof(err)),"negative column count rejected");
   CHECK(!THD_datatable_numeric_columns(dt,columns,2,NULL,2,err,sizeof(err)),"implicit row count must match table");
   CHECK(!THD_datatable_numeric_columns(dt,columns,2,&badrow,1,err,sizeof(err)),"negative row index rejected");
   badrow=3;
   CHECK(!THD_datatable_numeric_columns(dt,columns,2,&badrow,1,tiny,sizeof(tiny)) && !tiny[1],"row bounds and bounded error");
   invalid=*dt;invalid.val=NULL;
   CHECK(!THD_datatable_numeric_columns(&invalid,columns,2,NULL,3,err,sizeof(err)),"missing numeric metadata rejected");
   v=THD_datatable_numeric_columns(dt,columns,2,rows,3,err,sizeof(err));CHECK(v,"extract independent copy");
   THD_free_datatable(dt);
   for(i=0;i<6;i++)CHECK(v[i]==expected[i],"numeric matrix survives freeing source table");free(v);
   return 0;
}

static int check_labeled_matrix(const char *filename)
{
   const char *order[]={"place #1","face \"A\"","Subj"};
   const char *missing[]={"place #1","face \"A\"","missing"};
   THD_labeled_matrix *m;char err[512],tiny[2];FILE *fp;int i;
   const float want[]={0,2,-3,2,0,4,-3,4,0};
   fp=fopen(filename,"w");CHECK(fp,"open labeled matrix fixture");
   fputs("# matrix_format labeled\n# matrix_kind dissimilarity\n"
         "# Columns: Condition Subj \"face \"\"A\"\"\" \"place #1\"\n"
         "\"place #1\" -3 2 0\nSubj 0 4 -3\n\"face \"\"A\"\"\" 4 0 2\n",fp);
   fclose(fp);
   m=THD_read_labeled_matrix((char *)filename,"Condition",3,order,err,sizeof(err));
   CHECK(m && !err[0],"read labeled matrix with explicit order");
   CHECK(m->n==3 && !strcmp(m->kind,"dissimilarity"),"matrix shape and kind");
   for(i=0;i<9;i++)CHECK(m->mat[i]==want[i],"independent row/column alignment and negative values");
   for(i=0;i<3;i++)CHECK(!strcmp(m->label[i],order[i]),"quoted labels preserve exact identity");
   THD_free_labeled_matrix(m);
   m=THD_read_labeled_matrix((char *)filename,"Condition",0,NULL,err,sizeof(err));
   CHECK(m && !strcmp(m->label[0],"Subj") && m->mat[1]==4,"default order follows columns, including reserved table names");
   THD_free_labeled_matrix(m);
   CHECK(!THD_read_labeled_matrix((char *)filename,"Condition",3,missing,err,sizeof(err)) && strstr(err,"missing"),"reject missing condition");
   CHECK(!THD_read_labeled_matrix((char *)filename,"Time",0,NULL,tiny,sizeof(tiny)) && !tiny[1],"reject wrong axis with bounded diagnostic");
   CHECK(!THD_read_labeled_matrix((char *)filename,"Condition",2,order,err,sizeof(err)),"reject different condition count");
   {
     const char *bad[]={
       "Condition A A\nA 0 1\nB 1 0\n",
       "Condition A B\nA 0 1\nA 1 0\n",
       "Condition A B\nA 0 1\nb 1 0\n",
       "Condition A B\nA 0 nan\nB 1 0\n",
       "Condition A B\nA 0 1e99\nB 1 0\n",
       "Condition A B\nA 0 1x\nB 1 0\n",
       "Condition A B\nA 0 1\n",
       "# matrix_kind unknown\nCondition A B\nA 0 1\nB 1 0\n",
       "# matrix_kind\nCondition A B\nA 0 1\nB 1 0\n",
       "# matrix_kind similarity\n# matrix_kind dissimilarity\nCondition A B\nA 0 1\nB 1 0\n"
     };
     for(i=0;i<(int)(sizeof(bad)/sizeof(bad[0]));i++){
       fp=fopen(filename,"w");CHECK(fp,"open malformed matrix fixture");fputs(bad[i],fp);fclose(fp);
       CHECK(!THD_read_labeled_matrix((char *)filename,"Condition",0,NULL,err,sizeof(err)) && err[0],"reject malformed matrix contract");
     }
   }
   fp=fopen(filename,"w");CHECK(fp,"open similarity matrix fixture");
   fputs("# matrix_kind similarity\nCondition A B\nB 0.25 1\nA 1 0.25\n",fp);fclose(fp);
   m=THD_read_labeled_matrix((char *)filename,"Condition",0,NULL,err,sizeof(err));
   CHECK(m && !strcmp(m->kind,"similarity") && m->mat[0]==1,"generic reader retains similarity semantics for caller");
   THD_free_labeled_matrix(m);
   fp=fopen(filename,"w");CHECK(fp,"open reserved-name matrix fixture");
   fputs("Condition InputFile Subj\nSubj 2 0\nInputFile 0 2\n",fp);fclose(fp);
   m=THD_read_labeled_matrix((char *)filename,"Condition",0,NULL,err,sizeof(err));
   CHECK(m && !strcmp(m->label[0],"InputFile") && m->mat[1]==2,
         "matrix InputFile labels do not acquire dataset-path semantics");
   THD_free_labeled_matrix(m);
   return 0;
}

int main( int argc , char **argv )
{
   char fn[512],printed[512],*dir=getenv("TMPDIR") ;
   THD_datatable *dt,*roundtrip ; FILE *fp ;

   snprintf(fn,sizeof(fn),"%s/thd_datatable_unit_%d.txt",
            dir?dir:".",(int)getpid()) ;
   snprintf(printed,sizeof(printed),"%s/thd_datatable_roundtrip_%d.txt",
            dir?dir:".",(int)getpid()) ;
   CHECK(write_table(fn)==0,"write the test table") ;

   dt=THD_read_datatable_file(fn) ;
   CHECK(check_table(dt)==0,"parse hashes and comments") ;

   fp=fopen(printed,"w") ;
   CHECK(fp!=NULL,"open the round-trip table") ;
   THD_datatable_print(dt,fp) ; fclose(fp) ;
   roundtrip=THD_read_datatable_file(printed) ;
   CHECK(check_table(roundtrip)==0,"printed tables preserve hashes") ;

   THD_free_datatable(roundtrip) ; THD_free_datatable(dt) ;
   fp=fopen(printed,"w");CHECK(fp,"open commented-header test");
   fputs("# Comments are ignored\n# Columns: Subj Note InputFile\n",fp);
   THD_1D_print_token(fp,"subject with spaces");fputc(' ',fp);
   THD_1D_print_token(fp,"say \"hi # there\" and don't");fputs(" mock+orig\n",fp);
   fclose(fp);
   roundtrip=THD_read_datatable_file(printed);
   CHECK(roundtrip->nrow==1 && roundtrip->ncol==3,"commented schema is a header, not data");
   CHECK(!strcmp(DT_CELL(roundtrip,0,1),"say \"hi # there\" and don't"),"quoted text and embedded hash round-trip");
   CHECK(!strcmp(roundtrip->subj[0],"subject with spaces"),"space-delimited quoted subject");
   THD_free_datatable(roundtrip);
   CHECK(check_concat(printed)==0,"shared row concatenation");
   CHECK(check_numeric_columns(printed)==0,"shared numeric column extraction");
   CHECK(check_labeled_matrix(printed)==0,"labeled square matrix reading and alignment");
   remove(printed) ; remove(fn) ;
   (void)argc ; (void)argv ;
   printf("thd_datatable tests passed\n") ;
   return 0 ;
}
