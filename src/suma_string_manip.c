/* 
   Assortment of functions to maniplate strings, particularly those
   from help output.
   
   Many of those functions originated in SUMA/ but they are now part
   of libmri.a.
   
   While the functions herein use SUMA_[c,m,re]alloc, you're OK using either
   free or SUMA_free on returned pointers because the SUMA's allocation 
   functions are the same as AFNI's mcw_[c,m,re]alloc
      
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>
#include "mrilib.h"
#include "niml.h"
#include "../niml/niml_private.h"
#include "xutil.h"
#include "suma_suma.h"

#if defined SUMA_COMPILED
   extern SUMA_CommonFields *SUMAg_CF;
   extern int SUMAg_N_DOv; 
   extern SUMA_DO *SUMAg_DOv;
#endif


/*
   SUMA_EscapeChars ("Hallo_Baby%Hallo", "_%", "//")
   returns
   Hallo\\_Baby\\%Hallo
   \sa SUMA_ReplaceChars
*/
char *SUMA_EscapeChars(char *s1, char *ca, char *es)
{
   static char FuncName[]={"SUMA_EscapeChars"};
   char *ses =NULL;
   int nca=0, nes=0, ns1 = 0, nses = 0;
   int i=0, j=0, k=0, l=0, nfound = 0;
   SUMA_ENTRY;

   if (!s1 || !ca || !es) SUMA_RETURN(ses);

   nca = strlen(ca);
   nes = strlen(es);
   ns1 = strlen(s1);
   nfound = 0;
   for (i=0;i<ns1;++i) {
      for (j=0; j<nca; ++j) if (s1[i] == ca[j]) ++nfound;
   }
   nses = ns1+nfound*nes+1;
   ses = (char *)SUMA_calloc(nses, sizeof(char));

   i=0;l=0; 
   while (s1[i]) {
      for (j=0; j<nca; ++j) {
         if (s1[i] == ca[j]) {
            for (k=0; k<nes; ++k) { ses[l] = es[k]; ++l;}
            continue;
         } 
      }
      ses[l] =  s1[i]; ++l;
      ++i;
   }
   ses[l] = '\0';
   
   SUMA_RETURN(ses);
}

char *SUMA_ReplaceChars(char *s1, char *ca, char *es)
{
   static char FuncName[]={"SUMA_ReplaceChars"};
   char *ses =NULL;
   int nca=0, nes=0, ns1 = 0, nses = 0;
   int i=0, j=0, k=0, l=0, nfound = 0, rpl = 0;
   SUMA_ENTRY;

   if (!s1 || !ca || !es) SUMA_RETURN(ses);

   nca = strlen(ca);
   nes = strlen(es);
   ns1 = strlen(s1);
   nfound = 0;
   for (i=0;i<ns1;++i) {
      for (j=0; j<nca; ++j) if (s1[i] == ca[j]) ++nfound;
   }
   nses = ns1-nfound+nfound*nes+1;
   ses = (char *)SUMA_calloc(nses, sizeof(char));

   i=0;l=0; 
   while (s1[i]) {
      for (j=0; j<nca; ++j) {
         rpl = 0 ;
         if (s1[i] == ca[j]) {
            for (k=0; k<nes; ++k) { ses[l] = es[k]; ++l;}
            rpl  = 1;
            continue;
         } 
      }
      if (!rpl) { ses[l] =  s1[i]; ++l; }
      ++i;
   }
   ses[l] = '\0';
   
   SUMA_RETURN(ses);
} 

/* 
   Insert string 'ins' at pointer 'pos' inside of string '*s' which
   has at most *nalloc characters.
   Returns the string with the insertion, and reallocates and updates
   *nalloc if needed.
   Call with something like:
   s = insert_in_string(&s, pos, ins, nalloc);
*/
char *insert_in_string(char **s, char *pos, char *ins, int *nalloc)
{
   char *sp=NULL;
   int ns = -1, n_ins=-1, i_ins, i;
   
   if (!s || !*s || !pos || !nalloc) return(sp);
   
   sp = *s;
   if (!ins || ins[0] == '\0') return(sp); /* nothing to do */
   ns = strlen(sp);
   n_ins = strlen(ins);
   
   if ((i_ins = pos - sp) < 0 || (i_ins > ns)) {
      ERROR_message("Inserting outside of boundaries of string");
      return(*s);
   }
   
   /* fprintf(stderr,"i_ins=%d, ins=%s, ns=%d",i_ins, ins, ns); */
   /* Check for enough allocation */
   if (ns+n_ins >= *nalloc) {
      *nalloc += 500;
      *s = (char *)realloc(sp, (*nalloc+1)*sizeof(char));
      sp = *s;
   }
   
   /* Now move the second half ins steps */
   for (i=ns; i>=i_ins; --i) {
      sp[i+n_ins] = sp[i];
   }
   
   /* And now put in the insertion string */
   for (i=0; i<n_ins; ++i) {
      sp[i_ins+i] = ins[i];
   }

   return(*s);
}


void write_string(char *s, char *prelude, char *postscript,
                 int nmax, int multiline, FILE *fout)
{
   int k, ns;
   
   if (!fout) fout = stdout;
   if (prelude) fprintf(fout, "%s", prelude);
   if (s) {
      ns = strlen(s);
      if (nmax>ns) nmax = ns;
      else if (nmax<0) nmax = ns;
      k = 0;
      if (multiline) {
         while (k<nmax ) { 
            fprintf(stderr,"%c",*(s+k)); 
            ++k; 
         }
      } else {
         while (k<nmax && s[k] !='\n') { 
            fprintf(stderr,"%c",*(s+k)); 
            ++k; 
         }
      }
   }
   if (postscript) fprintf(fout, "%s", postscript);
   return;
}

/* 
Find 1st location in cur that begins with string opt.
Blanks are ignored.
Function returns pointer to beginning of opt in the line, 
and sets number of blanks preceding it */
char *line_begins_with(char *cur, char *opt, int *nb)
{
   char *loc=NULL, *nl=NULL;
   int bad = 1;
   
   if (!cur || !opt) {
      ERROR_message("NULL option or null string");
      return(loc);
   } 
   if (nb) *nb = -1;
   do {
      loc = strstr(cur, opt);
      if (loc) {
         bad = 0; /* Assume it is good */
         if (loc == cur) {
            if (nb) *nb = 0;
            return(loc);
         }
         /* search back to new line */
         nl = loc-1; 
         while (nl != cur && *nl != '\n') {
            if (*nl != ' ' && *nl != '\t') { /* No need to continue */
               bad = 1;  
            }
            --nl;
         }
         if (!bad) { /* Good */
            if (*nl == '\n') ++nl;
            if (nb) *nb = loc -nl;
            return(loc);
         } else {
            /* continue search past this find. */
            cur = loc+1;
         }
      } else {
         /* nothing found, get out */
         return(NULL);
      }
      
   } while (*cur != '\n');
   
   return(NULL);
}



/*--------------------------------------------------------------------*/
/*! My version of Bob's Decode a single string into a bunch of strings, separated
    by characters from the list in sep.
    - Passing sep in as NULL means to use "," as the separator.
    - In each sub-string, leading and trailing blanks will be excised.
    - This can result in 0 length strings (e.g., "1,,2," will result
      in the second and fourth output strings having 0 length).
   \sa SUMA_NI_get_ith_string
----------------------------------------------------------------------*/
NI_str_array * SUMA_NI_decode_string_list( char *ss , char *sep )
{
   static char FuncName[]={"SUMA_NI_decode_string_list"};
   NI_str_array *sar ;
   int num , nn,id,jd , lss ;

   if( ss == NULL || ss[0] == '\0' ) return NULL ; /* bad input */

   if( sep == NULL || sep[0] == '\0' ) sep = "," ;  /* default sep */

   sar = NI_malloc(NI_str_array, sizeof(NI_str_array)) ;  /* create output */
   sar->num = 0 ; sar->str = NULL ;

   /* scan for sub-strings */

   lss = NI_strlen(ss) ;
   num = id = 0 ;
   while( id < lss ){

      /* skip current position ahead over whitespace */

      while( id < lss && isspace(ss[id]) ) id++ ;
      if( id == lss ) break ;                           /* ran out of string */

      jd = id ;               /* save current position (start of new string) */

      /* skip ahead until ss[id] is a separator 
            [or a space - 10 Dec 2002 ZSS I don't like that one, 
             gives me funny looking results with legitimate spaces ,
             line below was: 
             while( id < lss && strchr(sep,ss[id]) == NULL  && 
                    !isspace(ss[id])) id++; ] */

      while( id < lss && strchr(sep,ss[id]) == NULL ) id++;
      if( id == jd ){ /* a blank string */
         /* Prior to Dec. 17 2013, I would:    
            id++; continue; 
            But that is a bad idea in cases when parsing
            strings that have something like "...;;..." where
            ';;' indicates an empty string. That can come up for
            column range of data elements when a range cannot be
            computed. */
      }

      /* new sub-string runs from ss[jd] to ss[id-1] */

      sar->str = NI_realloc( sar->str , char*, sizeof(char *)*(num+1) ) ;

      nn = id-jd ;                                   /* length of sub-string */
#if 0
      while( nn > 0 && isspace(ss[jd+nn-1]) ) nn-- ; /* clip trailing blanks */
#endif
      sar->str[num] = NI_malloc(char, (nn+1)*sizeof(char)); /* output string  */
      if( nn > 0 ) memcpy(sar->str[num],ss+jd,nn) ;  /* copy sub-string    */
      sar->str[num++][nn] = '\0' ;                   /* terminate output  */

      id++ ;                                         /* skip separator  */
   }

   sar->num = num ; return sar ;
}

NI_str_array * SUMA_NI_string_vec_to_str_array( char **ss , int nss )
{
   static char FuncName[]={"SUMA_NI_string_vec_to_str_array"};
   NI_str_array *sar ;
   int num ,id, nn=0 ;

   if( ss == NULL || nss == 0 ) return NULL ; /* bad input */

   sar = NI_malloc(NI_str_array, sizeof(NI_str_array)) ;  /* create output */
   sar->num = nss ; sar->str=NULL;
   sar->str = NI_realloc( sar->str , char*, sizeof(char *)*nss ) ;

   /* scan for sub-strings */

   num = 0 ;
   while( num < nss ){
      if (ss[num]) nn = strlen(ss[num]);
      else nn=0;
      sar->str[num] = NI_malloc(char, (nn+1)*sizeof(char)) ;                 
      memcpy(sar->str[num],ss[num], nn) ;  
      sar->str[num++][nn] = '\0' ;                   
   }

   return sar ;
}


/*--------------------------------------------------------------------*/
/*! \brief Returns a copy of the ith string in a string list. 
\sa SUMA_NI_decode_string_list ( on which this function is based)
----------------------------------------------------------------------*/
char  * SUMA_NI_get_ith_string( char *ss , char *sep, int i )
{
   static char FuncName[]={"SUMA_NI_get_ith_string"};
   char *str =NULL;
   int num , nn,id,jd , lss ;
   
   SUMA_ENTRY;
   
   if( ss == NULL || ss[0] == '\0' || i<0) SUMA_RETURN( NULL ) ; /* bad input */

   if( sep == NULL || sep[0] == '\0' ) sep = "," ;  /* default sep */


   /* scan for sub-strings */

   lss = NI_strlen(ss) ;
   num = id = 0 ;
   while( id < lss ){

      /* skip current position ahead over whitespace */

      while( id < lss && isspace(ss[id]) ) id++ ;
      if( id == lss ) break ;                           /* ran out of string */

      jd = id ;               /* save current position (start of new string) */

      /* skip ahead until ss[id] is a separator 
       [or a space - 10 Dec 2002 ZSS I don't like that one, 
        gives me funny looking results with legitimate spaces ,
        line below was: 
       while( id < lss && strchr(sep,ss[id]) == NULL  && !isspace(ss[id])) id++;]
         */

      while( id < lss && strchr(sep,ss[id]) == NULL ) id++;
      if( id == jd ){ id++; continue; }    /* is only a separator? */



      nn = id-jd ;                                   /* length of sub-string */
      
      if (i==num) { /* that is the one I want */
         /* new sub-string runs from ss[jd] to ss[id-1] */
         str = (char *) SUMA_malloc( sizeof(char )*(nn+1) ) ;
         if( nn > 0 ) memcpy(str,ss+jd,nn) ;  /* copy sub-string    */
         str[nn] = '\0' ;                   /* terminate output  */
         SUMA_RETURN(str);
      } 
      ++num;   
      id++ ;                                         /* skip separator  */
   }

   /* not found, return with NULL */
   SUMA_RETURN( str );
}

/*--------------------------------------------------------------------*/
/*! \brief Returns the index of a string in a string list. 
\sa SUMA_NI_decode_string_list ( on which this function is based)
----------------------------------------------------------------------*/
int  SUMA_NI_find_in_cs_string( char *ss , char *sep, char *str )
{
   static char FuncName[]={"SUMA_NI_find_in_cs_string"};
   int i = -1;
   int num , nn,id,jd , lss ;
   
   SUMA_ENTRY;
   
   if( ss == NULL || ss[0] == '\0' || str == NULL) SUMA_RETURN(i);/* bad input */

   if( sep == NULL || sep[0] == '\0' ) sep = "," ;  /* default sep */


   /* scan for sub-strings */

   lss = NI_strlen(ss) ;
   num = id = 0 ;
   while( id < lss ){

      /* skip current position ahead over whitespace */

      while( id < lss && isspace(ss[id]) ) id++ ;
      if( id == lss ) break ;                           /* ran out of string */

      jd = id ;               /* save current position (start of new string) */

      /* skip ahead until ss[id] is a separator 
       [or a space - 10 Dec 2002 ZSS I don't like that one, 
        gives me funny looking results with legitimate spaces ,
        line below was: 
       while( id < lss && strchr(sep,ss[id]) == NULL  && !isspace(ss[id])) id++;]
         */

      while( id < lss && strchr(sep,ss[id]) == NULL ) id++;
      if( id == jd ){ id++; continue; }    /* is only a separator? */



      nn = id-jd ;                                   /* length of sub-string */
      
      /* new sub-string runs from ss[jd] to ss[id-1] */
      if (nn == strlen(str)) { /* a strict search, might want to allow for 
                                 blanks at some point ... */
         if (!strncmp(str,ss+jd, strlen(str))) SUMA_RETURN(num);
      } 
      ++num;   
      id++ ;                                         /* skip separator  */
   }

   /* not found */
   SUMA_RETURN( -1 );
}

/*--------------------------------------------------------------------*/
/*! \brief Returns a the number of composite strings in a string list. 
\sa SUMA_NI_decode_string_list ( on which this function is based)
----------------------------------------------------------------------*/

int SUMA_NI_get_num_strings( char *ss , char *sep)
{
   static char FuncName[]={"SUMA_NI_get_num_strings"};
   char *str =NULL;
   int num , nn,id,jd , lss ;
   
   SUMA_ENTRY;
   
   if( ss == NULL || ss[0] == '\0') SUMA_RETURN( -1 ) ; /* bad input */

   if( sep == NULL || sep[0] == '\0' ) sep = "," ;  /* default sep */


   /* scan for sub-strings */

   lss = NI_strlen(ss) ;
   num = id = 0 ;
   while( id < lss ){

      /* skip current position ahead over whitespace */

      while( id < lss && isspace(ss[id]) ) id++ ;
      if( id == lss ) break ;                           /* ran out of string */

      jd = id ;               /* save current position (start of new string) */

      /* skip ahead until ss[id] is a separator 
        [or a space - 10 Dec 2002 ZSS I don't like that one, 
         gives me funny looking results with legitimate spaces ,
         line below was: 
         while( id < lss && strchr(sep,ss[id]) == NULL  && 
               !isspace(ss[id])) id++; ] */

      while( id < lss && strchr(sep,ss[id]) == NULL ) id++;
      if( id == jd ){ id++; continue; }    /* is only a separator? */



      nn = id-jd ;                                   /* length of sub-string */
       
      ++num;   
      id++ ;                                         /* skip separator  */
   }

   SUMA_RETURN( num );
}

void SUMA_Show_NI_str_ar(NI_str_array *nisa, FILE *out)
{
   static char FuncName[]={"SUMA_Show_NI_str_ar"};
   int i;
   char *s=NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   if (!out) out = SUMA_STDOUT;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (!nisa) SS = SUMA_StringAppend_va(SS, "NULL struct");
   else {
      SS = SUMA_StringAppend_va(SS, "%d strings:\n", nisa->num);
      for (i=0; i<nisa->num; ++i) {
         SS = SUMA_StringAppend_va(SS, "\t%d->>>%s<<<\n", 
                  i, nisa->str[i]?nisa->str[i]:"NULL nisa str");
      }
   }
   
   SUMA_SS2S(SS,s);
   
   fprintf(out, "%s", s); SUMA_free(s); s= NULL;
   fflush(out);
   SUMA_RETURNe;
}

/*!
   \brief take a bunch of strings stored in NI_str_array
   and turn them into a composite string
   Free result with SUMA_free
   \sa SUMA_NI_str_ar_2_comp_str
*/
char *SUMA_NI_str_ar_2_comp_str (NI_str_array *nisa, char *sep)
{
   static char FuncName[]={"SUMA_NI_str_ar_2_comp_str"};
   char *ar = NULL, *s=NULL;
   int i, nsep, k, ns, cnt, Nchars = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) SUMA_Show_NI_str_ar(nisa, NULL);
   
   if (!nisa) SUMA_RETURN(NULL);
   
   if (sep) nsep = strlen(sep);
   else nsep = 0;
   
   /* what's the total number of chars ? */
   for (i=0; i<nisa->num; ++i) {
      if (nisa->str[i]) { 
         Nchars += (strlen(nisa->str[i])+nsep+1) ; 
      } /* be safe allocate a bit more ...*/
      else Nchars += (nsep+1); /* for separator */
   }
   
   ar = (char *)SUMA_malloc(sizeof(char)*Nchars);
   
   cnt = 0;
   for (i=0; i<nisa->num; ++i) { 
      s = nisa->str[i];
      if (s) {
         ns = strlen(s); 
      } else {
         ns = 0;
      }
      k = 0;
      while (k < ns) { ar[cnt] = s[k]; ++k; ++cnt; }
      k = 0;
      while (k < nsep) { ar[cnt] = sep[k]; ++k; ++cnt; }
   }
   ar[cnt] = '\0'; /* le bouchon */
   
   SUMA_RETURN(ar);
}

/*!
   \brief Inverse of SUMA_NI_str_ar_2_comp_str
   free output with SUMA_free_NI_str_array
*/
NI_str_array *SUMA_comp_str_2_NI_str_ar(char *s, char *sep)
{
   static char FuncName[]={"SUMA_comp_str_2_NI_str_ar"};
   NI_str_array *nisa = NULL;
   
   SUMA_ENTRY;
   
   if (!s) SUMA_RETURN(nisa);
   
   nisa = SUMA_NI_decode_string_list(s, sep);
   
   SUMA_RETURN(nisa);
}

NI_str_array *SUMA_NI_str_array(NI_str_array *clss, char *what, char *action) 
{
   static char FuncName[]={"SUMA_NI_str_array"};
   int i=0;
   
   SUMA_ENTRY;
   
   if (!what || !action) SUMA_RETURN(clss);
   if (!clss) {
      clss = (NI_str_array *)NI_calloc(1,sizeof(NI_str_array));
      clss->num = 0;
      clss->str = NULL;
   }
   if (action[0] == 'a' || 
       (action[0] == 'A' && NI_str_array_find(what, clss) < 0)) { /* add */
      clss->num = clss->num+1;
      clss->str = 
         NI_realloc(clss->str, char *, sizeof(char *)*(clss->num));
      clss->str[clss->num-1] = NI_malloc(char, strlen(what)+1);
      strcpy(clss->str[clss->num-1], what);
      clss->str[clss->num-1][strlen(what)]='\0';
   } else if ( action[0] == 'r' ) {/* remove */
      i=NI_str_array_find(what,clss); 
      if (i>=0 && i!=clss->num-1) {
         NI_free(clss->str[i]); clss->str[i] = clss->str[clss->num-1];
      }
      clss->num = clss->num-1;
      clss->str = 
         NI_realloc(clss->str, char *, sizeof(char *)*(clss->num));
   } else if (action[0] == 'c') {
      /* change mode, get the index */
      if ((i=(int)strtol(action+1, NULL, 10))>10000) {
         SUMA_S_Errv("I have a feeling %d is in error...\n", i);
         SUMA_RETURN(clss);
      }
      if (i >= clss->num || !clss->str) {
         clss->num = i+1;
         clss->str = NI_realloc(clss->str, char *, sizeof(char *)*(clss->num));
         clss->str[i] = NI_malloc(char, strlen(what)+1);
      } else {
         clss->str[i] = NI_realloc(clss->str[i], char, (strlen(what)+1));
      }
      strcpy(clss->str[i], what);
      clss->str[i][strlen(what)]='\0';
   } else if (action[0] != 'A'){
      SUMA_S_Warnv("action %s unknown, nothing done\n", action);
   } 
   
   SUMA_RETURN(clss);
   
}

/* WARNING: For partial match, only the first hit is returned */
int SUMA_NI_str_array_find( char *targ , NI_str_array *sar , int partial, int ci)
{
   static char FuncName[]={"SUMA_NI_str_array_find"};
   int ii ;

   SUMA_ENTRY;
   
   if( targ == NULL || *targ == '\0' || sar == NULL || sar->num < 1 ) 
      SUMA_RETURN(-1);

   if (!partial) {
      if (!ci) {
         for( ii=0 ; ii < sar->num ; ii++ )
            if( strcmp(targ,sar->str[ii]) == 0 ) SUMA_RETURN(ii) ;
      } else {
         for( ii=0 ; ii < sar->num ; ii++ )
            if( strcasecmp(targ,sar->str[ii]) == 0 ) SUMA_RETURN(ii) ;
      }
   } else {
      if (!ci) {
         for( ii=0 ; ii < sar->num ; ii++ )
            if( strstr(sar->str[ii], targ) == NULL ) SUMA_RETURN(ii) ;
      } else {
         for( ii=0 ; ii < sar->num ; ii++ )
            if( !AFNI_strcasestr(sar->str[ii], targ) ) SUMA_RETURN(ii) ;
      }
   }
   SUMA_RETURN(-1) ;
}


NI_str_array *SUMA_free_NI_str_array(NI_str_array *nisa)
{
   static char FuncName[]={"SUMA_free_NI_str_array"}; 
   int i;
   
   SUMA_ENTRY;
   
   if (nisa) {
      if (nisa->str) {
         for (i=0; i<nisa->num; ++i) {
            if (nisa->str[i]) NI_free(nisa->str[i]); nisa->str[i] = NULL;
         }
         NI_free(nisa->str); 
      }
      NI_free(nisa); nisa = NULL;
   }
   
   SUMA_RETURN(nisa);
}

/*!
   \brief returns the iith string in a sep separated composite string cs 
   free result with SUMA_free
*/
char *SUMA_Get_Sub_String(char *cs, char *sep, int ii)
{
   static char FuncName[]={"SUMA_Get_Sub_String"};
   NI_str_array *nisa=NULL;
   char *s = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (ii < 0) { SUMA_SL_Err("Bad index"); SUMA_RETURN(s); }
   if (!cs) { SUMA_SL_Err("NULL input"); SUMA_RETURN(s); }
   #if 0 /* old slow way */
      nisa = SUMA_comp_str_2_NI_str_ar(cs, sep);
      if (LocalHead) SUMA_Show_NI_str_ar(nisa, NULL);
      if (!nisa) { 
         SUMA_SL_Err("Failed in SUMA_comp_str_2_NI_str_ar"); SUMA_RETURN(s); }
      if (ii >= nisa->num) { 
         /* SUMA_SL_Warn("not enough strings"); */ SUMA_RETURN(s); }
      s = SUMA_copy_string(nisa->str[ii]);
      SUMA_free_NI_str_array(nisa); nisa = NULL;
   #else 
      s = SUMA_NI_get_ith_string( cs , sep, ii );

   #endif
   SUMA_RETURN(s);
}

int SUMA_Find_Sub_String(char *cs, char *sep, char *ss)
{
   static char FuncName[]={"SUMA_Find_Sub_String"};
   NI_str_array *nisa=NULL;
   int ii = -1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ss) { SUMA_SL_Err("Bad string"); SUMA_RETURN(ii); }
   if (!cs) { SUMA_SL_Err("NULL input"); SUMA_RETURN(ii); }

   SUMA_RETURN(SUMA_NI_find_in_cs_string ( cs, sep, ss));
   
   SUMA_RETURN(ii);
}

SUMA_Boolean SUMA_Set_Sub_String(char **cs, char *sep, int ii, char *str)
{
   static char FuncName[]={"SUMA_Set_Sub_String"};
   NI_str_array *nisa=NULL;
   char *s = NULL, act[64];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (ii < 0) { SUMA_SL_Err("Bad index"); SUMA_RETURN(NOPE); }
   if (!cs || !str) { SUMA_SL_Err("NULL input %p %p", cs, str); 
                      if (LocalHead) SUMA_DUMP_TRACE("Why"); 
                      SUMA_RETURN(NOPE); }
   if (!*cs && ii != 0) { 
      SUMA_S_Errv("Bad spot %d with NULL string", ii); SUMA_RETURN(NOPE); }
   if (!*cs && ii == 0) {
      *cs = SUMA_copy_string(str);
      SUMA_RETURN(YUP);
   }  
   sprintf(act,"c%d",ii);
   nisa = SUMA_NI_decode_string_list( *cs , sep );
   /* SUMA_LHv("act: >>%s<< >>%s<< >>%s<< >>%s<<\n", act, *cs, sep, str); */
   nisa = SUMA_NI_str_array(nisa,str,act);
   SUMA_free(*cs); 
   *cs = SUMA_NI_str_ar_2_comp_str(nisa, sep);
   if (nisa) SUMA_free_NI_str_array(nisa); nisa = NULL;
   SUMA_RETURN(YUP);
}

/*!
   \brief removes a string in a sep separated 
   composite string cs
   The function does not reallocate for cs 
   returns 0 fail
           1 strn found and removed
           -1 strn not found
*/
int SUMA_Remove_Sub_String(char *cs, char *sep, char *strn)
{
   static char FuncName[]={"SUMA_Remove_Sub_String"};
   NI_str_array *nisa=NULL;
   char *s = NULL, *s0=NULL, *s1=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!cs || !strn || !sep) SUMA_RETURN(0);
   
   if (LocalHead) fprintf(SUMA_STDERR, "Strng was:\n"
                                       ">>>%s<<<\n"
                                       "id>%s<<<\n", 
                                       cs, strn);

   if (!(s0 = strstr(cs, strn))) {
      SUMA_LH("id not in strn");
      SUMA_RETURN(-1); /* nothing to do */
   }
   /* advance past strn */
   s = s0+strlen(strn);
   /* advance past sep */
   s1 = strstr(s, sep);
   if (s1) s1 = s1+strlen(sep);
   else s1 = s;

   /* now copy all that is left into s */
   while (*s1 != '\0') {
      *s0 = *s1; ++s0; ++s1;
   }
   *s0 = '\0';
   
   /* Do not bother reallocating */  
   
   if (LocalHead) fprintf(SUMA_STDERR, "Strng now:\n"
                                       ">>>%s<<<\n", cs);
   SUMA_RETURN(1);
}

/*!
   \brief Reads in a sequence of numbers of an undetermined length
   Not for reading in large numbers of numbers!
   \param op (char *) pointing to the beginning of a 
                     blank delimited series of numbers
   \param opend (char **) if not NULL, *opend will contain the value
                           of op at the end of successful reads
   \param tp (SUMA_VARTYPE) SUMA_int, SUMA_float, SUMA_double supported 
                           at the moment
   \return ans (void*) if  tp == SUMA_int then ans is (SUMA_IVEC *)
                           tp == SUMA_float then ans is (SUMA_FVEC *)
                           tp == SUMA_double then ans is (SUMA_DVEC *)
   \sa SUMA_strtol_vec  
   \sa SUMA_SringToNum
*/
void *SUMA_AdvancePastNumbers(char *op, char **opend, SUMA_VARTYPE tp)
{
   static char FuncName[]={"SUMA_AdvancePastNumbers"};
   double *d=NULL, db;
   int nrealloc = 0, Chunk = 100, nalloc = 0;
   int Found = 0, i, nread;
   void *ans;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   nread = 0;
   Found = 1;
   while (Found) {
      SUMA_ADVANCE_PAST_NUM(op, db, Found);
      if (Found) {
         if (nread == nalloc) {
            nalloc += Chunk; ++nrealloc;
            d = (double*)SUMA_realloc(d, nalloc*sizeof(double));
            if (!d) { SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NULL); }
            if (!(nrealloc % 10)) { 
               SUMA_SL_Warn("Too much reallocation, improper use of function?");
            }
         }
         d[nread] = db;
         ++(nread);
      }
   } 
   
   if (LocalHead) { 
      fprintf(SUMA_STDERR,"%s: Found %d numbers:\n", FuncName, nread);
      for (i=0; i<nread; ++i) fprintf(SUMA_STDERR,"%f\t", d[i]);
      fprintf(SUMA_STDERR,"\n");
   }
   
   if (opend) *opend = op;
   
   ans = NULL;
   switch (tp) {
      case SUMA_int:
         {
            SUMA_IVEC *ivec= (SUMA_IVEC *)SUMA_calloc(1,sizeof(SUMA_IVEC));
            ivec->v = (int *)SUMA_calloc(nread,sizeof(int));
            ivec->n = nread;
            for (i=0; i<nread; ++i) ivec->v[i] = (int)d[i];
            ans = (void *)ivec;
         }
         break;
      case SUMA_float:
         {
            SUMA_FVEC *fvec= (SUMA_FVEC *)SUMA_calloc(1,sizeof(SUMA_FVEC));
            fvec->v = (float *)SUMA_calloc(nread,sizeof(float));
            fvec->n = nread;
            for (i=0; i<nread; ++i) fvec->v[i] = (float)d[i];
            ans = (void *)fvec;
         }
         break;
      case SUMA_double:
         {
            SUMA_DVEC *dvec= (SUMA_DVEC *)SUMA_calloc(1,sizeof(SUMA_DVEC));
            dvec->v = (double *)SUMA_calloc(nread,sizeof(double));
            dvec->n = nread;
            for (i=0; i<nread; ++i) dvec->v[i] = (double)d[i];
            ans = (void *)dvec;
         }
         break;
      case SUMA_notypeset:
         SUMA_SL_Err("Type not set");
         ans = NULL;
         break;   
      default:
         SUMA_SL_Err("Type not supported by this function");
         ans = NULL;
         break;   
         
   }
   if (d) SUMA_free(d); d = NULL;
   
   SUMA_RETURN(ans);
   
}
   
/*!
   \brief change a character string of numbers to a vector of values.
   op must be NULL terminated!
   
   \sa SUMA_AdvancePastNumbers
   \sa SUMA_StringToNum
*/
void *SUMA_strtol_vec(char *op, int nvals, int *nread, 
                      SUMA_VARTYPE vtp, char **opend)
{
   static char FuncName[]={"SUMA_strtol_vec"};
   void *ans = NULL;
   long lv;
   double dv;
   char *endptr=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   *nread = 0;
   if (opend) *opend = op;

   if (!SUMA_OK_OPENDX_DATA_TYPE(vtp)) {
      SUMA_SL_Err("Bad type");
      SUMA_RETURN(ans);
   }
   
   ans = NULL;
   switch (vtp) {
      case SUMA_byte:
         {
            byte *bvec=NULL;
            bvec = (byte *)SUMA_calloc(nvals,sizeof(byte));
            lv = strtol(op, &endptr, 10);
            while (endptr && endptr != op && *nread < nvals) {
               bvec[*nread] = (byte)lv;
               /* if (LocalHead) 
                  fprintf(SUMA_STDERR,">>>%d<<<\t", bvec[*nread]);  */
               ++(*nread);
               op = endptr;
               lv = strtol(op, &endptr, 10);
            }
            ans = (void *)bvec;
         }
         break;
      case SUMA_int:
         {
            int *ivec=NULL;
            ivec = (int *)SUMA_calloc(nvals,sizeof(int));
            lv = strtol(op, &endptr, 10);
            while (endptr && endptr != op && *nread < nvals) {
               ivec[*nread] = (int)lv;
               /* if (LocalHead && *nread < 10) 
                  fprintf(SUMA_STDERR,">>>%d<<<\t", ivec[*nread]); */  
               ++(*nread);
               op = endptr;
               lv = strtol(op, &endptr, 10);
            }
            ans = (void *)ivec;
         }
         break;
      case SUMA_float:
         {
            float *fvec=NULL;
            fvec = (float *)SUMA_calloc(nvals,sizeof(float));
            dv = strtod(op, &endptr);
            while (endptr && endptr != op && *nread < nvals) {
               fvec[*nread] = (float)dv;
               /* if (LocalHead) 
                  fprintf(SUMA_STDERR,">>>%f<<<\t", fvec[*nread]); */
               ++(*nread);
               op = endptr;
               dv = strtod(op, &endptr);
            }
            ans = (void *)fvec;
         }
         break;
      case SUMA_double:
         {
            double *dvec=NULL;
            dvec = (double *)SUMA_calloc(nvals,sizeof(double));
            dv = strtod(op, &endptr);
            while (endptr && endptr != op && *nread < nvals) {
               dvec[*nread] = (double)dv;
               /* if (LocalHead) 
                  fprintf(SUMA_STDERR,">>>%f<<<\t", dvec[*nread]); */
               ++(*nread);
               op = endptr;
               dv = strtod(op, &endptr);
            }
            ans = (void *)dvec;
         }
         break;
      case SUMA_notypeset:
         SUMA_SL_Err("Type not set");
         ans = NULL;
         break;   
      default:
         SUMA_SL_Err("Type not supported by this function");
         ans = NULL;
         break;   
         
   }

   if (opend) *opend = op;
   SUMA_RETURN(ans);
}

/* 
   Break long lines into ones that are no longer than mxln 
   You need to handle the freeing of the string returned by this function 
*/
char *SUMA_Break_String(char *si, int mxln)
{
   static char FuncName[]={"SUMA_Break_String"};
   char *so = NULL;
   int nsi, nso, nso_max, bsi, bso, ex, slen, ln;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!si) SUMA_RETURN(so);
      
   SUMA_LH("Have string:>s=>%s<\n", si);
   slen = strlen(si);
   nso_max = slen+100;
   so = (char *)SUMA_calloc(nso_max, sizeof(char));
   
   bsi = bso = -1; /* index of last encountered blank */
   ln = 0; /* Last line length in output string */
   ex = 0; /* Number of extra chars */
   nso = 0; nsi = 0; /* write/read position in so and si */
   while (si[nsi]) {
      while (si[nsi] && ln < mxln) {
         if (SUMA_IS_BLANK(si[nsi])) {
            bsi = nsi; bso = nso;
         }
         so[nso++] = si[nsi++]; 
         if (si[nsi] == '\n') {
            ln = 0; bsi = bso = -1;
         } else {
            ++ln;
         }
      }
      if (ln == mxln) { /* need to make a cut */
         if (bso > 0 && ((nso-bso)) < mxln-15) { 
            /* had a good blank preceding, but not too far*/
            nso = bso; /* rewind on so */
            nsi = bsi; /* rewind on si */
            so[++nso] = '\n'; /* add new line after blank */
            ex += 1; /* added one new char */
            ln = 0; bsi = bso = -1;
            ++nsi; ++nso;
         } else {
            /* add a '-' */
            so[nso++] = '-'; so[nso++] = '\n';
            ex += 2;
            ln = 0; bsi = bso = -1;
         }
      }
      
      /* realloc ? */
      if (ex >= (nso_max - slen - 5)) {
         nso_max += 100;
         so = (char *)SUMA_realloc(so, nso_max*sizeof(char));
      }
         
   }
   
   so[nso] = '\0';
   SUMA_LH("Returning:>so=>%s>", so);
   SUMA_RETURN(so);
}

/* 
   Offset each line by off blanks
   You must handle the freeing of the returned string 
*/
char *SUMA_Offset_Lines(char *si, int off)
{
   static char FuncName[]={"SUMA_Offset_Lines"};
   char *so = NULL, *s=NULL;
   int nnl=0, nso_max, nso=0, i, slen;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!si) SUMA_RETURN(so);
      
   SUMA_LH("Have string:>s=>%s<\n", si);
   slen = strlen(si);
   s = si; nnl = 1;
   while (*s != '\0') {
      if (*s == '\n') ++ nnl;
      ++s;
   }
   nso_max = slen+(nnl+1)*off;
   so = (char *)SUMA_calloc(nso_max, sizeof(char));
   nso = 0;
   for (i=0; i<off; ++i) so[nso++] = ' ';
   s = si;
   while (*s != '\0') {
      so[nso++] = *s;
      if (*s == '\n' && (strncmp(s+1,":NOF:",5))) {
         /* You could conceivably get rid of :NOF: here...
         but I get rid of it later so that's OK for now */
         for (i=0; i<off; ++i) so[nso++] = ' ';
      } 
      ++s;  
   }
   
   so[nso] = '\0';
   SUMA_LH("Returning:>so=>%s>", so);
   SUMA_RETURN(so);
}

/*
   Cut string sc from string s
*/
char *SUMA_Cut_String(char *s, char *sc)
{
   static char FuncName[]={"SUMA_Cut_String"};
   char *so, *ss=NULL;
   int nso=0;
   
   SUMA_ENTRY;
   
   if (!s || !sc || !(ss=strstr(s, sc))) {
      SUMA_RETURN(s);
   }
   
   so = s;
   nso = 0; 
   while (ss) {
      while (s < ss) {
         so[nso++]=*(s++);      
      }
      s += strlen(sc);
      ss=strstr(s, sc);
   }
   /* copy till end */
   while (*s != '\0') {
      so[nso++]=*(s++);
   }
   so[nso] = '\0';
   
   SUMA_RETURN(so);
}

/*
   Given a string that contains sphinx markup like
   
   s = "That's one :ref:`nice<tag>` pen."
   
   the function SUMA_Sphinx_DeRef(s, ":ref:") returns :
   
   "That's one nice pen."
   
   Note that 's' cannot be a constant string because 
   the function will need to write into it.
   
*/
char *SUMA_Sphinx_DeRef(char *s, char *r)
{
   static char FuncName[]={"SUMA_Sphinx_DeRef"};
   char *so, *ss=NULL, *se=NULL, *sef=NULL;
   int nso=0;
   
   SUMA_ENTRY;
   
   if (!s || !r || !(ss=strstr(s, r))) {
      SUMA_RETURN(s);
   }
   
   if (!strcmp(r,":LIT:")) { /* special case for non Sphinx directive */
      so = s;
      nso = 0; 
      while (ss) {
         while (s < ss) {
            so[nso++]=*(s++);      
         }
         if (nso && !SUMA_IS_PURE_BLANK(so[nso-1])) so[nso++] = ':'; 
         s += strlen(r);
         ss=strstr(s, r);
      }
      /* copy till end */
      while (*s != '\0') {
         so[nso++]=*(s++);
      }
      so[nso] = '\0';
   
      SUMA_RETURN(so);
   }
   
   /* Things of the form :DIREC:`something <SOMETHING>` */
   so = s;
   nso = 0; 
   while (ss) {
      while (s < ss) {
         so[nso++]=*(s++);      
      }
      s += strlen(r); /* s->`blah blah <REF>` */
      if (*s == '`') {
         s++; se = s;
         while (*se != '`' && *se != '\0') ++se;
         if (*se == '`') { /* found closing quote */
            sef = se;
            /* backup till you find > */
            while (se > s && *se != '>') { --se; }
            if (*se == '>') {
               /* backup till you find < */
               while (se > s && *se != '<') { --se; }
               if (*se == '<') { /* All good, copy blah blah */
                  while (s < se) {
                     so[nso++]=*(s++); 
                  }  
               }
            } else {
               /*copy all between quotes */
               while (s < sef) {
                  so[nso++]=*(s++); 
               }
            }
            /* move s till after closing quote */
            s = sef+1;
         } else {
            SUMA_S_Warn("No closing forward quote after ref! in %s", so);
         }
      } else {
         SUMA_S_Warn("No forward quote after ref! in %s", so);
      }
      ss=strstr(s, r);
   }
   /* copy till end */
   while (*s != '\0') {
      so[nso++]=*(s++);
   }
   so[nso] = '\0';
   
   SUMA_RETURN(so);
}

/*
   Switch occurence of 'sc' in 's' with 'sw'
   
   At the moment, the function will insist that
   sw be smaller or equal to sc in length.
   
*/
char *SUMA_Swap_String(char *s, char *sc, char *sw)
{
   static char FuncName[]={"SUMA_Swap_String"};
   char *so, *ss=NULL;
   int nso=0, ww;
   
   SUMA_ENTRY;
   
   if (!s || !sc || !sw || !(ss=strstr(s, sc))) {
      SUMA_RETURN(s);
   }
   if (strlen(sw) > strlen(sc)) {
      SUMA_S_Err( "Not in the mood for reallocing, fix if you must, "
                  "or perhaps write other function a la SUMA_Break_String");
      SUMA_RETURN(s);
   }
   
   so = s;
   nso = 0; 
   while (ss) {
      while (s < ss) {
         so[nso++]=*(s++);      
      }
      for (ww=0; ww<strlen(sw); ++ww) so[nso++]=sw[ww];
      s += strlen(sc);
      ss=strstr(s, sc);
   }
   /* copy till end */
   while (*s != '\0') {
      so[nso++]=*(s++);
   }
   so[nso] = '\0';
   
   SUMA_RETURN(so);
}

/*
   Split a string into multiple ones using string sc
   as a deliminter.
   
   Use SUMA_free_NI_str_array() to free NI_str_array*

 */
NI_str_array *SUMA_Split_String(char *s, char *sc)
{
   static char FuncName[]={"SUMA_Split_String"};
   char *so, *ss=NULL;
   int nso=0;
   NI_str_array *nisa = NULL;
   
   SUMA_ENTRY;
   
   if (!s || !sc) {
      SUMA_RETURN(NULL);
   }
   
   nisa = NI_malloc(NI_str_array, sizeof(NI_str_array)) ;  /* create output */
   nisa->num = 0 ; nisa->str = NULL ;

   if (!(ss=strstr(s, sc))) { /* Just one found */
      nisa->str = NI_realloc( nisa->str , char*, sizeof(char *)*(nisa->num+1) ) ;
      nisa->str[nisa->num] = NI_malloc(char, ((strlen(s)+1)*sizeof(char)));
      strcat(nisa->str[nisa->num], s);
      nisa->num++;
      SUMA_RETURN(nisa);
   }
   
   so = s;
   nso = 0; 
   while (ss) {
      nisa->str = NI_realloc( nisa->str , char*, sizeof(char *)*(nisa->num+1) ) ;
      nisa->str[nisa->num] = NI_malloc(char, ((ss-s+1)*sizeof(char)));
      nso = 0;
      while (s < ss) {
         nisa->str[nisa->num][nso++]=*(s++);      
      }
      nisa->str[nisa->num][nso]='\0'; ++nisa->num;
      s += strlen(sc);
      ss=strstr(s, sc);
   }
   
   if (*s != '\0') {/* copy till end */
      nisa->str = NI_realloc( nisa->str , char*, sizeof(char *)*(nisa->num+1) ) ;
      nisa->str[nisa->num] = NI_malloc(char, ((strlen(s)+1)*sizeof(char)));
      nso = 0;
      while (*s != '\0') {
         nisa->str[nisa->num][nso++]=*(s++);
      }
      nisa->str[nisa->num][nso]='\0'; ++nisa->num;
   }
   
   SUMA_RETURN(nisa);
}

/*
   Cut out chunk of string 's' bracketed by 'sc0' and 'sc1', but do
   leave any portion between 'save' and 'sc1', if any exist.
   
   This function is used to handle the markup:
   
   :SPX:
   ........
   :DEF:
   ........
   :SPX:
    
*/
char *SUMA_Cut_Between_String(char *s, char *sc0, char *sc1, char *save)
{
   static char FuncName[]={"SUMA_Cut_Between_String"};
   char *so, *ss0=NULL, *ss1=NULL, *ssa=NULL;
   int nso=0;
   
   SUMA_ENTRY;
   
   if (!sc1) sc1 = sc0;
   
   if (!s || !sc1 || !sc0
                  || !(ss0=strstr(s, sc0)) 
                  || !(ss1=strstr(ss0+strlen(sc0), sc1)) || (ss1==ss0) ) {
      SUMA_RETURN(s);
   }
   
   so = s;
   nso = 0; 
   while (ss0 && ss1 && ss0 != ss1) {
      while (s < ss0) {
         so[nso++]=*(s++);      
      }
      
      if ( save && (ssa = af_strnstr(ss0+strlen(sc0), save, ss1-ss0) ) ) {
         s = ssa+strlen(save);
         while (s < ss1) {
            so[nso++]=*(s++);
         }
         s += strlen(sc1);
      } else {
         s += strlen(sc1)+ss1-ss0;
      }
      
      ss0=strstr(s, sc0);
      if (ss0) ss1=strstr(ss0+strlen(sc0), sc1);
   }
   /* copy till end */
   while (*s != '\0') {
      so[nso++]=*(s++);
   }
   so[nso] = '\0';
   
   SUMA_RETURN(so);
}

/*
   A function to illustrate the use of markup gimmicks.
   if fout == NULL, use stderr for output.
*/
void SUMA_Sphinx_String_Edit_Help(FILE *fout)
{
   static char FuncName[]={"SUMA_Sphinx_String_Edit_Help"};
   char *s0, *s1;
   char intro[]={
"Simple trickery to use same string for both SUMA and SPHINX\n"
"formatting.\n Function SUMA_Sphinx_String_Edit is used to \n"
"take strings with these special markers and return them in\n"
"either Sphinx or regular text.\n"
"\n"
" :SPX: Hiding a SPHINX directive with minimal fanfare:\n"
"     Text between :SPX: markers does not appear in default output\n"
"     format.\n"
"        :SPX: Sphinx chunk :DEF: regular chunk :SPX:\n"
"     Use this to insert into a text string a section that is\n"
"     only displayed when Sphinx output is requested.\n"
"     It is also possible to provide an alternate section\n"
"     after the :DEF: marker between the opening and closing\n"
"     :SPX: markers. The alternate section is used when the\n"
"     requested output format is simple text.\n"
"\n"
"     The example coming up next will show how we can have\n"
"     alternate output where a key press would be mentioned\n"
"     simply in the SUMA output but with a reference directive\n"
"     when SPHINX output is used:\n\n"
" :LR: Replace this marker with a new line character for \n"
"      Sphinx output. Cut it out for regular output.\n"
" :LIT: Replace this marker with '::\n' to mark an upoming literal\n"
"       paragraph for sphinx. If the character before :LIT:\n"
"       is a non blank, a ':' will terminate the sentence preceding\n"
"       the literal paragraph.\n"
"       For regular output, :LIT: is cut out if it is preceded by\n"
"       a blank. Otherwise it is replaced by a ':'\n"
"       Note that the literal paragraph must be indented relative to\n"
"       the preceding one.\n"
"\n"
" :ref:`Some Label <reference_key>` Leave such a block untouched for\n"
"                              sphinx format. Replace whole thing\n"
"                              with just 'Some Label' for default format.\n"
"\n"
" :[blanks]: Cut this marker out of string for Sphinx output,\n"
"            but keep all blanks and pads with two more in regular\n"
"            output to compensate for the ':' characters.\n"
"            Also, for the Sphinx format, a newline directly preceding\n"
"            the opening ':' gets cut out.\n"
"\n"
" '\\|' Escaped vertical bar are kept as such for Sphinx, but shown\n"
"       without the escape character in default output. This is\n"
"       needed to keep sphinx from considering words between vertical\n"
"       bars to be substitution references.\n"
"\n"
" :NOF:When found right after a new line, don't let function \n"
"      SUMA_Offset_Lines() insert any spaces. :NOF: is otherwise cut\n"
"      from all output\n"
"\n" 
"See function SUMA_Sphinx_String_Edit_Help() for a code sample.\n"
"\n"
                };
   char s[] = {
"Example 1:\n"
"Below you will see a figure directive, but only for Sphinx format.\n"
":SPX:\n\n"
".. :figure: _static/junk.jpg\n"
"            :align: center\n"
"\n:SPX:"
"And now the rest of text continues...\n"
"\n"
"Example 2:\n"
"Press buton :SPX::ref:`a <LC_a>`:DEF:'a':SPX: to attenuate...\n" 
"\n"
"Example 2.1 (simpler version):\n"
"Press buton :ref:`a <LC_a>` to attenuate...\n" 
"\n"
"Example 3:\n"
"For 'Trn' choose one of::LR:\n"
"   0: No transparency.\n"
":    :Surface is opaque.:LR:\n"
"   8: 50% transparency.\n"
":    :Surface is in cheese cloth transparency.:LR:\n"
"\n"
"Example 4:\n"
"... or if '\\|T\\|' is used then ...\n"
"\n"
"Example 5:\n"
"A sample file would be: test.1D.col with content:LIT:\n"   \
"   0    0.1 0.2 1   \n"   
"   1    0   1   0.8 \n"   
"   4    1   1   1   \n"   
"   7    1   0   1   \n"
"   14   0.7 0.3 0   "
"\n"
};
      
   if (!fout) fout = SUMA_STDERR;
      
   fprintf(fout,"\n%s\n", intro);
   s0 = strdup(s); s1 = strdup(s);
   fprintf(fout,"\n        Source Code Version:\n%s\n    -------\n", s);
   fprintf(fout,"\n        Edited   for   SUMA:\n%s\n    -------\n", 
                  SUMA_Sphinx_String_Edit(&s0,0,0));
   fprintf(fout,"\n        Edited  for  SPHINX:\n%s\n    -------\n", 
                  SUMA_Sphinx_String_Edit(&s1,1, 0));
   free(s0); free(s1);

   return;
}

/*
   Format the content of file fname for regular or sphinx output.
   
   \sa SUMA_Sphinx_String_Edit()
   
   \param fname: (char *)the filename
   \param targ:  (TFORM) the format, 0 for regular, 1 for sphinx
   \param off: (int) Number of blank characters to insert at
                     the beginning of each line that does not
                     begin with :NOF:
   \return s (char *) The edited/formatted string.
*/ 
char *SUMA_Sphinx_File_Edit(char *fname, TFORM targ, int off)
{
   static char FuncName[]={"SUMA_Sphinx_File_Edit"};
   char *s=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!fname) SUMA_RETURN(s);
   
   if (!SUMA_suck_file(fname, &s)) {
      SUMA_S_Err("Empty file or file not found");
      SUMA_RETURN(NULL);
   }
   
   SUMA_RETURN(SUMA_Sphinx_String_Edit(&s, targ, off));
}


/*
   A function that allows me to format help strings for 
   display in SUMA as was done in the past, and for 
   fancier SPHINX formatted output.
   
   \param suser (char **): Pointer to user's string
   \param targ (TFORM) the format, 0 for regular, 1 for sphinx
   \param off: (int) Number of blank characters to insert at
                     the beginning of each line that does not
                     begin with :NOF:
   \return: s (char *): Edited string. Note that usually
                        s = *suser, unless reallocation
                        was necessary. In that case the 
                        function takes care of freeing 
                        *suser and resetting it to 
                        new pointer.
   
   \sa SUMA_Sphinx_String_Edit_Help() for documentation.
*/

char *SUMA_Sphinx_String_Edit(char **suser, TFORM targ, int off) 
{
   static char FuncName[]={"SUMA_Sphinx_String_Edit"};
   char stmp[6]={""}, *s=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!suser || !(*suser)) SUMA_RETURN(s);
   
   s = *suser;
   
   switch (targ) {
      case NO_FORMAT:
         SUMA_RETURN(s);
         break;
      case TXT: /* Default C output */
         SUMA_LH(">s=>\n%s\n<", s);
         SUMA_Cut_Between_String(s, ":SPX:", ":SPX:", ":DEF:");
         SUMA_Cut_String(s,":LR:"); SUMA_Cut_String(s,":NOF:");  
         SUMA_Sphinx_LineSpacer(s, targ);
         sprintf(stmp,"\\|"); /* to avoid compile warning for 
                                 direct use of "\|" in SUMA_Swap_String below */
         SUMA_Swap_String(s, stmp,"|");
         SUMA_Sphinx_DeRef(s,":ref:");
         SUMA_Sphinx_DeRef(s,":term:");
         SUMA_Sphinx_DeRef(s, ":LIT:");
         SUMA_LH(">so=>\n%s\n<", s);
         SUMA_RETURN(s);
         break;
      case SPX: /* Sphinx */
         SUMA_Cut_String(
               SUMA_Cut_Between_String(s, ":DEF:", ":SPX:", NULL), ":SPX:");
         SUMA_Swap_String(s, ":LR:","\n");
         SUMA_Sphinx_LineSpacer(s, targ);
         SUMA_Swap_String(s, ":LIT:","::\n");
         SUMA_Cut_String(s,"(more with BHelp)");
         if (off) {
            *suser = SUMA_Offset_Lines(s,off);
            SUMA_ifree(s); s = *suser;
         }
         SUMA_Cut_String(s,":NOF:"); 
         SUMA_Cut_String(s,"(BHelp for more)");
         SUMA_Cut_String(s,"(much more with BHelp)");
         break;
      default:
         SUMA_S_Err("What is TFORM of %d?", targ);
         SUMA_RETURN(s);
         break;
   }
   
   SUMA_RETURN(s); 
}

char * format_prog_help(char *prog, TFORM targ, int verb) 
{
   static char FuncName[]={"format_prog_help"};
   char *oh=NULL;
   
   SUMA_ENTRY;
   
   if (!prog) {
      SUMA_RETURN(NULL);
   }
   
   
   switch (targ) {
      default:
         ERROR_message("No format from adam!");
         SUMA_RETURN(NULL);
         break;
      case NO_FORMAT:
         if (!(oh = phelp(prog, targ, verb))) {
            ERROR_message("Weird, dude");
            SUMA_RETURN(NULL);
         }
         SUMA_RETURN(oh);
         break;
      case TXT:
         if (!(oh = phelp(prog, targ, verb))) {
            ERROR_message("Weird, dude");
            SUMA_RETURN(NULL);
         }
         SUMA_RETURN(SUMA_Sphinx_String_Edit(&oh, TXT, 0));
         break;
      case SPX:
         SUMA_RETURN(sphinxize_prog_help(prog, verb));
         break;
   }
   
   SUMA_RETURN(NULL);
}

/*
   Take the help output of program prog and
   return it as a string in sphinx format.
*/
char *sphinxize_prog_help (char *prog, int verb) 
{
   static char FuncName[]={"sphinxize_prog_help"};
   char **ws=NULL, *sout=NULL, *ofile=NULL, *bb=NULL;
   char *sh=NULL, *oh=NULL, *l=NULL, sins[1024]={""};
   int N_ws=0, ishtp=0, nb = 0, i, k, nalloc, offs;
   SUMA_Boolean LocalHead=YUP;
   
   SUMA_ENTRY;
   
   if (!prog || !(ws = approx_str_sort_all_popts(prog, &N_ws,  
                   1, NULL,
                   NULL, NULL, 1, 0, '\\'))) {
      SUMA_RETURN(NULL);
   }
   /* Get the original help string */
   if (!(oh = phelp(prog, SPX, verb))) {
      SUMA_S_Err("Weird, dude");
      SUMA_RETURN(NULL);
   }
   
   SUMA_LH("Have %d opts total", N_ws);
   
   nalloc = 2*strlen(oh);
   sh = (char*)calloc(2*strlen(oh), sizeof(char));
   strcpy(sh, oh);
   sh[strlen(oh)]='\0';
   
   snprintf(sins, 1020, "%s\n", prog); bb = sins+strlen(sins);
   for (i=0; i<strlen(prog); ++i) {*bb='-'; ++bb;}
   *bb='\0';
   strcat(sins,"\n\n");
   sh = insert_in_string(&sh, sh, sins, &nalloc); 
   for (i=0; i<N_ws; ++i) {
      if (ws[i]) {
         l = find_popt(sh,ws[i], &nb);
         if (l) {
            offs = l - sh -nb;
            if (verb) {
               fprintf(stderr,"Found option %s at::", ws[i]);
               write_string(l-nb, "\n", "\n",50, 0, stderr);
            }
            snprintf(sins, 1020, "\n.. _%s-%s:\n\n", 
                     prog, ws[i]);
            sh = insert_in_string(&sh, l-nb, sins, &nalloc);
            sh = insert_in_string(&sh, l+strlen(sins), "**", &nalloc);
            sh = insert_in_string(&sh, l+strlen(sins)+2+strlen(ws[i]), 
                                                       "**\\ ", &nalloc);
            if (verb) {
               write_string(sh+offs, "    Now have\n", "\n\n",50, 1, stderr);
            }
         } else {
            fprintf(stderr,"Option %s not found\n\n", ws[i]);
         }
         SUMA_free(ws[i]); ws[i]=NULL;
      }
   }
   SUMA_free(ws); ws = NULL;
   SUMA_free(oh); oh = NULL;
   
   SUMA_RETURN(SUMA_Sphinx_String_Edit(&sh, SPX, 0));
}


/*
   Check if string begins with sphinx directives
   used in SUMA's code 
*/
SUMA_Boolean SUMA_Known_Sphinx_Dir(char *s)
{
   static char FuncName[]={"SUMA_Known_Sphinx_Dir"};
   if (!s) return(NOPE);
   if (!strncmp(s,":ref:",5)) return(YUP);
   if (!strncmp(s,":term:",5)) return(YUP);
   return(NOPE);
}

/* 
   
   Handle white space markup
   
{ char *sdo, so[]={
   "Choose the rendering mode for this surface.\n" 
   "   Viewer: Surface's rendering mode is set "  
   ":         :by the viewer's setting which can "   
   ":         :be changed with the 'p' option.:LR:\n"  
   "   Fill:   Shaded rendering mode.:LR:\n"  
   "   Line:   Mesh rendering mode.:LR:\n"    
   "   Points: Points rendering mode.:LR:\n"};
   
   sdo = SUMA_Sphinx_LineSpacer(so , 1);
   fprintf(SUMA_STDERR,"%s\n", sdo); 
}

*/
char *SUMA_Sphinx_LineSpacer(char *s, TFORM targ)
{
   static char FuncName[]={"SUMA_Sphinx_LineSpacer"};
   int bln, ns, nso, slen;
   char *so=NULL;
   
   SUMA_ENTRY;
   
   /* search for :.*: */
   
   if (!s) SUMA_RETURN(s);
   
   slen = strlen(s);
   
   ns = 0; nso = 0;
   so = s;
   while (s[ns]) {
      if (s[ns] == ':' && ns < slen-1) {
         bln=0;
         while (s[ns+bln+1] && SUMA_IS_PURE_BLANK(s[ns+1+bln])) { ++bln; }
         if (bln > 0 && s[ns+1+bln] == ':' && 
             !SUMA_Known_Sphinx_Dir(s+ns+1+bln)) {
            /* Have blank gap */
            switch(targ) {
               case TXT: /* just replace : with space */
                  if (nso>1 && SUMA_IS_PURE_BLANK(so[nso-1])) {
                     so[nso-1] = '\n';/* Need newline to make it come out nice */
                  }
                  so[nso++] = ' '; ++ns;
                  while(s[ns] != ':') { so[nso++] = s[ns++]; }
                  so[nso++] = ' '; ++ns;
                  break;
               case SPX: /* remove all spaces */
                  /* remove preceding new line just to keep superfluous 
                  new line characters that were there for the purpose of keeping
                  the output width short. Do not remove the newline if there
                  is two of them in a row, or there is certain punctuation 
                  before the newline.*/
                  if (nso>1 && so[nso-1] == '\n' && 
                              (so[nso-2] != '\n' && so[nso-2] != ':')) {
                     so[nso-1]=' ';
                  }
                  ns += bln+2;
                  break;
               case NO_FORMAT: /* You asked for it! */
                  break;
               default:
                  SUMA_S_Warn("Not equipped for this %d!", targ);
                  break;
            }
         } else {
            /* nothing, copy character and move on */
            so[nso++] = s[ns++];
         } 
      } else {
         so[nso++] = s[ns++];
      }
   }
   so[nso] = '\0';
   SUMA_RETURN(so);
}

/*
   A function that formats the content of a printf statement
   to handle Sphinx markups as well as our own, before writing
   the results.
   
   \param targ: (int) 0 -- Regular output
                      1 -- Sphinx output
   \param off: (int) Number of blank characters to insert at
                     the beginning of each line that does not
                     begin with :NOF:
   \param fout: (FILE *) Where do you want the output?
                        NULL == stdout
   \param ...: Whatever you'd put in an printf() call.
   
   \return (int): Whatever the final fprintf returns.
   
   \sa  macros: sphinx_printf(int targ, char *str, ...)
          and  sphinx_fprintf(int targ, FILE *fout, char *str, ...)
          
   Examples:
   {  
      sphinx_printf(1,NULL);
      sphinx_printf(1,"Hello\n");
      sphinx_printf(1,"Hello %s, %d donuts\n","Jimminy Cricket", 12344554);
   }
   

*/
int sphinx_offprintf(TFORM targ, int off, FILE *fout, char *str, ... )
{
   static char FuncName[]={"sphinx_offprintf"};
   char *s=NULL;
   va_list vararg_ptr , saved_copy;
   int rr = 1, nalloc, nchunk=30000, nout, toolittle, ns;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!fout) fout = stdout;
   
   if (!str) SUMA_RETURN(rr);
   
   va_copy( saved_copy, vararg_ptr );
   va_start( vararg_ptr ,  str) ;   
   nalloc = strlen(str)+nchunk;
   if (!(s = (char *)SUMA_calloc(nalloc, sizeof(char)))) {
      SUMA_S_Err("Failed to allocate for %d chars", nalloc);
      SUMA_RETURN(rr);
   } 
   s[0] = s[nalloc-1] = '\0';
   do {
      nout = vsnprintf (s, nalloc*sizeof(char), str, vararg_ptr); 
      SUMA_LH("nout=%d", nout);
      if (nout < 0) {
         SUMA_SL_Err("Error reported by  vsnprintf");
         strcat(s,"Error SUMA_StringAppend_va:"
                  " ***Error reported by  vsnprintf");
         SUMA_free(s);
         SUMA_RETURN(1);
      }
      toolittle = 0;
      if (nout >= nalloc) {
         SUMA_LH("Reallocating %d more", nchunk);
         toolittle = 1;
         nalloc += nchunk;
         if (!(s = (char *)SUMA_realloc(s, sizeof(char)*(nalloc)))){
            SUMA_S_Err("Failed to allocate for %d chars", nalloc);
            SUMA_RETURN(rr);
         } 
         s[0] = s[nalloc-1] = '\0';
         va_end(vararg_ptr); /* clean this copy then recreate */
         va_copy( vararg_ptr, saved_copy);
         va_start(vararg_ptr, str);
      }
   } while(toolittle);
   
   va_end(vararg_ptr);  /* cleanup */
   va_end(saved_copy);
   
   ns = strlen(s);
   s = (char *)SUMA_realloc(s, sizeof(char)*(ns+1));s[ns]='\0';
   
   if ((s = SUMA_Sphinx_String_Edit(&s, targ, off))) {
      rr = fprintf(fout,"%s",s);
      SUMA_ifree(s);
   } else {
      ERROR_message("Failed miserably");
      rr = 1;
   }
   
   SUMA_RETURN(rr);
}

