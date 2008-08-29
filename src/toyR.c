#include <stdio.h>
#include <stdlib.h>

#include "AFNI_embeddedRCall.h"
#include "afni.h"
#include <Rdefines.h>
#include <R_ext/Rdynload.h>


/* A simple C function that R will call */
SEXP callback(SEXP x){
  SEXP y;  
  PROTECT(y = duplicate(x));
  INTEGER(y)[0]=2*INTEGER(y)[0];
  UNPROTECT(1);   /* a little confused here...
                     Perhaps R will protect what is returned, until
                     necessary */
  return y;
}

int main (int argc,char *argv[])
{/* Main */
   double *f=NULL;
   int i;
   char *pp=NULL;
   FILE *fout=NULL;
   SEXP e, e1, rv, rs;
   
   init_R(argc, argv);
   
/* Calling R and asking it to call compiled C routines! */
   {
      int deuce=-999;
      DllInfo *info;
      R_CallMethodDef callMethods[]  = {
                  {"callback", (DL_FUNC) &callback, 1},
                  {NULL, NULL, 0}
      };
      info  = R_getEmbeddingDllInfo();
      R_registerRoutines(info, NULL, callMethods, NULL, NULL);
      /* .Call is the R function used to call compiled 
         code that uses internal R objects */
      PROTECT(e1=lang3( install(".Call"),
                        mkString("callback"),ScalarInteger(100)));    
      /* evaluate the R command in the global environment*/
      PROTECT(e=eval(e1,R_GlobalEnv));
      /* show the value */
      printf("Answer returned by R:"); Rf_PrintValue(e);
      /* store the value in a local variable */
      deuce = INTEGER(e)[0];
      printf("Got %d back from result SEXP\n\n", deuce);
      
      UNPROTECT(2); /* allow for R's garbage collection */
   }
   
/* Calling R and asking it to do computation on a C array */
   f = (double *)malloc(sizeof(double)*256);
   for (i=0; i<256;++i) f[i]=(double)rand()/(double)RAND_MAX+i/64;

   /*Now copy array into R structs */ 
   PROTECT(rv=allocVector(REALSXP, 256));
   defineVar(install("f"), rv, R_GlobalEnv); /* put rv in R's environment and 
                                                name it "f" */
   for (i=0; i<256;++i) REAL(rv)[i] = f[i];  /* fill rv with values */
   
   /* plot that array with R's: plot(f) */   
   PROTECT(e = lang1(install("x11")));
   eval(e, R_GlobalEnv);
   UNPROTECT(1);
   PROTECT(e=lang2(install("plot"),install("f")));
   eval(e, R_GlobalEnv);
   UNPROTECT(1);
   
   /* calculate the log of the values with log(f) */
   PROTECT(e1=lang2(install("log"),install("f")));    
   PROTECT(e=eval(e1,R_GlobalEnv));
   for (i=0; i<256;++i) { 
      if (i<5 || i>250) {
         printf("%d: log(%f)=%f\n", i, f[i], REAL(e)[i]);
      } else if (!(i%20)) {
         printf("...");
      }
   }
   
   UNPROTECT(2); 
    
   /* Now run some R script with source(".../ExamineXmat.R") */
   if (!(pp = Add_plausible_path("ExamineXmat.R"))) {
      fprintf(stderr,"Failed to find ExamineXmat.R\n");
      exit(1);
   }
   PROTECT(rs=mkString(pp));
   defineVar(install("sss"), rs, R_GlobalEnv);
   fprintf(stderr,"checking on script name: %s\n", STRING_VALUE(rs));
   PROTECT(e=lang2(install("source"),install("sss")));
   eval(e, R_GlobalEnv);
   UNPROTECT(2);
   fprintf(stderr,"Hit enter to proceed\n");
   free(pp); pp=NULL;
   /* Here is should test calling R functions from some functions
   that we create. I will need to sort out how packges are formed
   for R and how R can find them on any machine etc. Nuts and bolts...
   A simple exercise here would be to learn how to construct our R library
   and call its functions from here ... */
   
   free(f); f = NULL; free(pp); pp=NULL;
   
   getchar();
}
