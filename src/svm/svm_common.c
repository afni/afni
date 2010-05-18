/************************************************************************/
/*                                                                      */
/*   svm_common.c                                                       */
/*                                                                      */
/*   Definitions and functions used in both svm_learn and svm_classify. */
/*                                                                      */
/*   Author: Thorsten Joachims                                          */
/*   Date: 02.07.02                                                     */
/*                                                                      */
/*   Copyright (c) 2002  Thorsten Joachims - All rights reserved        */
/*                                                                      */
/*   This software is available for non-commercial use only. It must    */
/*   not be modified and distributed without prior permission of the    */
/*   author. The author is not responsible for implications from the    */
/*   use of this software.                                              */
/*                                                                      */
/************************************************************************/

# include "ctype.h"
# include "svm_common.h"
# include "kernel.h"           /* this contains a user supplied kernel */

long   kernel_cache_statistic;

double classify_example(MODEL *model, DOC *ex) 
     /* classifies one example */
{
  register long i;
  register double dist;

  dist=0;
  for(i=1;i<model->sv_num;i++) {  
    dist+=kernel(&model->kernel_parm,model->supvec[i],ex)*model->alpha[i];
  }
  return(dist-model->b);
}

double classify_example_linear(MODEL *model, DOC *ex) 
     /* classifies example for linear kernel */
     
     /* important: the model must have the linear weight vector computed */

     /* important: the feature numbers in the example to classify must */
     /*            not be larger than the weight vector!               */
{
  return((double)(sprod_ns(model->lin_weights,ex->words)-model->b));
}

CFLOAT kernel(KERNEL_PARM *kernel_parm, DOC *a, DOC *b) 
     /* calculate the kernel function */
{
  kernel_cache_statistic++;
  switch(kernel_parm->kernel_type) {
    case 0: /* linear */ 
            return((CFLOAT)sprod_ss(a->words,b->words)); 
    case 1: /* polynomial */
            return((CFLOAT)pow(kernel_parm->coef_lin*sprod_ss(a->words,b->words)+kernel_parm->coef_const,(double)kernel_parm->poly_degree)); 
    case 2: /* radial basis function */
            return((CFLOAT)exp(-kernel_parm->rbf_gamma*(a->twonorm_sq-2*sprod_ss(a->words,b->words)+b->twonorm_sq)));
    case 3: /* sigmoid neural net */
            return((CFLOAT)tanh(kernel_parm->coef_lin*sprod_ss(a->words,b->words)+kernel_parm->coef_const)); 
    case 4: /* custom-kernel supplied in file kernel.h*/
            return((CFLOAT)custom_kernel(kernel_parm,a,b)); 
    default: printf("Error: Unknown kernel function\n"); exit(1);
  }
}

double sprod_ss(WORD *a, WORD *b) 
     /* compute the inner product of two sparse vectors */
{
    register FVAL sum=0;
    register WORD *ai,*bj;
    
    ai=a;
    bj=b;
    while (ai->wnum && bj->wnum) {
      if(ai->wnum > bj->wnum) {
        bj++;
      }
      else if (ai->wnum < bj->wnum) {
        ai++;
      }
      else {
        sum+=ai->weight * bj->weight;
        
        ai++;
        bj++;
      }
    } 
    
    return((double)sum);
}

WORD* sub_ss(WORD *a, WORD *b) 
     /* compute the difference a-b of two sparse vectors */
{
    register WORD *sum,*sumi;
    register WORD *ai,*bj;
    long veclength;
  
    ai=a;
    bj=b;
    veclength=0;
    while (ai->wnum && bj->wnum) {
      if(ai->wnum > bj->wnum) {
	veclength++;
	bj++;
      }
      else if (ai->wnum < bj->wnum) {
	veclength++;
	ai++;
      }
      else {
	veclength++;
	ai++;
	bj++;
      }
    }
    while (bj->wnum) {
      veclength++;
      bj++;
    }
    while (ai->wnum) {
      veclength++;
      ai++;
    }
    veclength++;

    sum=(WORD *)my_malloc(sizeof(WORD)*veclength);
    sumi=sum;
    ai=a;
    bj=b;
    while (ai->wnum && bj->wnum) {
      if(ai->wnum > bj->wnum) {
	(*sumi)=(*bj);
	sumi->weight*=(-1);
	sumi++;
	bj++;
      }
      else if (ai->wnum < bj->wnum) {
	(*sumi)=(*ai);
	sumi++;
	ai++;
      }
      else {
	(*sumi)=(*ai);
	sumi->weight-=bj->weight;
	sumi++;
	ai++;
	bj++;
      }
    }
    while (bj->wnum) {
      (*sumi)=(*bj);
      sumi->weight*=(-1);
      sumi++;
      bj++;
    }
    while (ai->wnum) {
      (*sumi)=(*ai);
      sumi++;
      ai++;
    }
    sumi->wnum=0;
    return(sum);
}

double model_length_s(MODEL *model, KERNEL_PARM *kernel_parm) 
     /* compute length of weight vector */
{
  register long i,j;
  register double sum=0,alphai;
  register DOC *supveci;

  for(i=1;i<model->sv_num;i++) {  
    alphai=model->alpha[i];
    supveci=model->supvec[i];
    for(j=1;j<model->sv_num;j++) {
      sum+=alphai*model->alpha[j]
	   *kernel(kernel_parm,supveci,model->supvec[j]);
    }
  }
  return(sqrt(sum));
}

void clear_vector_n(double *vec, long int n)
{
  register long i;
  for(i=0;i<=n;i++) vec[i]=0;
}

void add_vector_ns(double *vec_n, WORD *vec_s, double faktor)
{
  register WORD *ai;
  ai=vec_s;
  while (ai->wnum) {
    vec_n[ai->wnum]+=(faktor*ai->weight);
    ai++;
  }
}
  
double sprod_ns(double *vec_n, WORD *vec_s)
{
  register double sum=0;
  register WORD *ai;
  ai=vec_s;
  while (ai->wnum) {
    sum+=(vec_n[ai->wnum]*ai->weight);
    ai++;
  }
  return(sum);
}

void add_weight_vector_to_linear_model(MODEL *model)
     /* compute weight vector in linear case and add to model */
{
  long i;

  model->lin_weights=(double *)my_malloc(sizeof(double)*(model->totwords+1));
  clear_vector_n(model->lin_weights,model->totwords);
  for(i=1;i<model->sv_num;i++) {
    add_vector_ns(model->lin_weights,(model->supvec[i])->words,
		  model->alpha[i]);
  }
}

void read_model(char *modelfile, MODEL *model, long int max_words, long int ll)
{
  FILE *modelfl;
  long j,i;
  char *line;
  WORD *words;
  register long wpos;
  long wnum,pos;
  double weight;
  char version_buffer[100];
  int numread;

  if(verbosity>=1) {
    printf("Reading model..."); fflush(stdout);
  }
  words = (WORD *)my_malloc(sizeof(WORD)*(max_words+10));
  line = (char *)my_malloc(sizeof(char)*ll);

  if ((modelfl = fopen (modelfile, "r")) == NULL)
  { perror (modelfile); exit (1); }

  fscanf(modelfl,"SVM-light Version %s\n",version_buffer);
  if(strcmp(version_buffer,VERSION_SVMLIGHT)) {
    perror ("Version of model-file does not match version of svm_classify!"); 
    exit (1); 
  }
  fscanf(modelfl,"%ld%*[^\n]\n", &model->kernel_parm.kernel_type);  
  fscanf(modelfl,"%ld%*[^\n]\n", &model->kernel_parm.poly_degree);
  fscanf(modelfl,"%lf%*[^\n]\n", &model->kernel_parm.rbf_gamma);
  fscanf(modelfl,"%lf%*[^\n]\n", &model->kernel_parm.coef_lin);
  fscanf(modelfl,"%lf%*[^\n]\n", &model->kernel_parm.coef_const);
  fscanf(modelfl,"%[^#]%*[^\n]\n", model->kernel_parm.custom);

  fscanf(modelfl,"%ld%*[^\n]\n", &model->totwords);
  fscanf(modelfl,"%ld%*[^\n]\n", &model->totdoc);
  fscanf(modelfl,"%ld%*[^\n]\n", &model->sv_num);
  fscanf(modelfl,"%lf%*[^\n]\n", &model->b);

  for(i=1;i<model->sv_num;i++) {
    fgets(line,(int)ll,modelfl);
    pos=0;
    wpos=0;
    sscanf(line,"%lf",&model->alpha[i]);
    while(!isspace((int)line[++pos]));
    while(((numread=sscanf(line+pos,"%ld:%lf",&wnum,&weight)) != EOF) 
	  && (wpos<max_words)) {
      if(numread != 2) {
	perror("Parsing error while reading model!");
	printf("LINE: %s\n",line);
      }
      while(!isspace((int)line[++pos]));
      words[wpos].wnum=wnum;
      words[wpos].weight=(FVAL)weight; 
      wpos++;
    } 
    model->supvec[i] = (DOC *)my_malloc(sizeof(DOC));
    (model->supvec[i])->words = (WORD *)my_malloc(sizeof(WORD)*(wpos+1));
    for(j=0;j<wpos;j++) {
      (model->supvec[i])->words[j]=words[j]; 
    }
    ((model->supvec[i])->words[wpos]).wnum=0;
    (model->supvec[i])->twonorm_sq = sprod_ss((model->supvec[i])->words,
					      (model->supvec[i])->words);
    (model->supvec[i])->docnum = -1;
  }
  fclose(modelfl);
  free(line);
  free(words);
  if(verbosity>=1) {
    fprintf(stdout, "OK. (%d support vectors read)\n",(int)(model->sv_num-1));
  }
}

void read_documents(char *docfile, DOC *docs, double *label, 
		    long int max_words_doc, long int ll, 
		    long int *totwords, long int *totdoc)
{
  char *line;
  DOC doc;
  long dnum=0,wpos,i,dpos=0,dneg=0,dunlab=0;
  double doc_label;
  FILE *docfl;

  line = (char *)my_malloc(sizeof(char)*ll);

  if ((docfl = fopen (docfile, "r")) == NULL)
  { perror (docfile); exit (1); }

  doc.words = (WORD *)my_malloc(sizeof(WORD)*(max_words_doc+10));
  if(verbosity>=1) {
    printf("Reading examples into memory..."); fflush(stdout);
  }
  dnum=0;
  (*totwords)=0;
  while((!feof(docfl)) && fgets(line,(int)ll,docfl)) {
    if(line[0] == '#') continue;  /* line contains comments */
    if(!parse_document(line,&doc,&doc_label,&wpos,max_words_doc)) {
      printf("\nParsing error in line %ld!\n%s",dnum,line);
      exit(1);
    }
    label[dnum]=doc_label;
    /*  printf("Class=%ld ",doc_label);  */
    if(doc_label > 0) dpos++;
    if (doc_label < 0) dneg++;
    if (doc_label == 0) dunlab++;
    if((wpos>1) && ((doc.words[wpos-2]).wnum>(*totwords))) 
      (*totwords)=(doc.words[wpos-2]).wnum;
    docs[dnum].queryid = doc.queryid;
    docs[dnum].costfactor = doc.costfactor;
    docs[dnum].words = (WORD *)my_malloc(sizeof(WORD)*(wpos));
    docs[dnum].docnum=dnum;
    for(i=0;i<wpos;i++) { 
      docs[dnum].words[i]=doc.words[i];
      /*  printf("%ld::%f ",(docs[dnum].words[i]).wnum,(docs[dnum].words[i]).weight);  */

    }
    docs[dnum].twonorm_sq=doc.twonorm_sq;
    /* printf("\nNorm=%f\n",docs[dnum].twonorm_sq);  */
    dnum++;  
    if(verbosity>=1) {
      if((dnum % 100) == 0) {
	printf("%ld..",dnum); fflush(stdout);
      }
    }
  } 

  fclose(docfl);
  free(line);
  free(doc.words);
  if(verbosity>=1) {
    fprintf(stdout, "OK. (%ld examples read)\n", dnum);
  }
  (*totdoc)=dnum;
}

int parse_document(char *line, DOC *doc, double *label, 
		   long int *numwords, long int max_words_doc)
{
  register long wpos,pos;
  long wnum;
  double weight;
  int numread;
  char featurepair[1000],junk[1000];

  doc->queryid=0;
  doc->costfactor=1;

  pos=0;
  while(line[pos]) {      /* cut off comments */
    if(line[pos] == '#') {
      line[pos]=0;
    }
    else {
      pos++;
    }
  }
  wpos=0;
  if(sscanf(line,"%lf",label) == EOF) return(0);
  pos=0;
  while(isspace((int)line[pos])) pos++;
  while((!isspace((int)line[pos])) && line[pos]) pos++;
  while(((numread=sscanf(line+pos,"%s",featurepair)) != EOF) && 
	(wpos<max_words_doc)) {
    /* printf("%s\n",featurepair); */
    while(isspace((int)line[pos])) pos++;
    while((!isspace((int)line[pos])) && line[pos]) pos++;
    if(sscanf(featurepair,"qid:%ld%s",&wnum,junk)==1) {
      /* it is the query id */
      doc->queryid=(long)wnum;
    }
    else if(sscanf(featurepair,"cost:%lf%s",&weight,junk)==1) {
      /* it is the example-dependent cost factor */
      doc->costfactor=(double)weight;
    }
    else if(sscanf(featurepair,"%ld:%lf%s",&wnum,&weight,junk)==2) {
      /* it is a regular feature */
      if(wnum<=0) { 
	perror ("Feature numbers must be larger or equal to 1!!!\n"); 
	printf("LINE: %s\n",line);
	exit (1); 
      }
      if((wpos>0) && ((doc->words[wpos-1]).wnum >= wnum)) { 
	perror ("Features must be in increasing order!!!\n"); 
	printf("LINE: %s\n",line);
	exit (1); 
      }
      (doc->words[wpos]).wnum=wnum;
      (doc->words[wpos]).weight=(FVAL)weight; 
      wpos++;
    }
    else {
      perror ("Cannot parse feature/value pair!!!\n"); 
      printf("'%s' in LINE: %s\n",featurepair,line);
      exit (1); 
    }
  }
  (doc->words[wpos]).wnum=0;
  (*numwords)=wpos+1;
  doc->docnum=-1;
  doc->twonorm_sq=sprod_ss(doc->words,doc->words);
  return(1);
}

void nol_ll(char *file, long int *nol, long int *wol, long int *ll) 
     /* Grep through file and count number of lines, maximum number of
        spaces per line, and longest line. */
{
  FILE *fl;
  int ic;
  char c;
  long current_length,current_wol;

  if ((fl = fopen (file, "r")) == NULL)
  { perror (file); exit (1); }
  current_length=0;
  current_wol=0;
  (*ll)=0;
  (*nol)=1;
  (*wol)=0;
  while((ic=getc(fl)) != EOF) {
    c=(char)ic;
    current_length++;
    if(isspace((int)c)) {
      current_wol++;
    }
    if(c == '\n') {
      (*nol)++;
      if(current_length>(*ll)) {
	(*ll)=current_length;
      }
      if(current_wol>(*wol)) {
	(*wol)=current_wol;
      }
      current_length=0;
      current_wol=0;
    }
  }
  fclose(fl);
}

long minl(long int a, long int b)
{
  if(a<b)
    return(a);
  else
    return(b);
}

long maxl(long int a, long int b)
{
  if(a>b)
    return(a);
  else
    return(b);
}

long get_runtime(void)
{
  clock_t start;
  start = clock();
  return((long)((double)start*100.0/(double)CLOCKS_PER_SEC));
}


# ifdef MICROSOFT

int isnan(double a)
{
  return(_isnan(a));
}

# endif


void *my_malloc(size_t size)
{
  void *ptr;
  ptr=(void *)malloc(size);
  if(!ptr) { 
    perror ("Out of memory!\n"); 
    exit (1); 
  }
  return(ptr);
}

void copyright_notice(void)
{
  printf("\nCopyright: Thorsten Joachims, thorsten@ls8.cs.uni-dortmund.de\n\n");
  printf("This software is available for non-commercial use only. It must not\n");
  printf("be modified and distributed without prior permission of the author.\n");
  printf("The author is not responsible for implications from the use of this\n");
  printf("software.\n\n");
}
