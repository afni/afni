/* 
   Assortment of functions to maniplate strings, particularly those
   from help output.
   
   Many of those functions originated in SUMA/ but they are now part
   of libmri.a.
   
   While the functions herein use SUMA_[c,m,re]alloc, you're OK using either
   free or SUMA_free on returned pointers because the SUMA's allocation 
   functions are the same as AFNI's mcw_[c,m,re]alloc
      
*/

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
   \param targ:  (int) the format, 0 for regular, 1 for sphinx
   \param off: (int) Number of blank characters to insert at
                     the beginning of each line that does not
                     begin with :NOF:
   \return s (char *) The edited/formatted string.
*/ 
char *SUMA_Sphinx_File_Edit(char *fname, int targ, int off)
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
   \param targ (int) the format, 0 for regular, 1 for sphinx
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

char *SUMA_Sphinx_String_Edit(char **suser, int targ, int off) 
{
   static char FuncName[]={"SUMA_Sphinx_String_Edit"};
   char stmp[6]={""}, *s=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!suser || !(*suser)) SUMA_RETURN(s);
   
   s = *suser;
   
   switch (targ) {
      case 0: /* Default C output */
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
      case 1: /* Sphinx */
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
         SUMA_RETURN(s);
         break;
   }
   
   SUMA_RETURN(s); 
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
   
   SUMA_ENTRY;
   
   if (!prog || !(ws = approx_str_sort_all_popts(prog, &N_ws,  
                   1, NULL,
                   NULL, NULL, 1, 0, '\\'))) {
      SUMA_RETURN(0);
   }
   
   /* Get the original help string */
   if (!(oh = phelp(prog, verb))) {
      ERROR_message("Weird, dude");
      SUMA_RETURN(0);
   }
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
   
   SUMA_RETURN(SUMA_Sphinx_String_Edit(&sh, 1, 0));
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
char *SUMA_Sphinx_LineSpacer(char *s, int targ)
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
            if (targ == 0) { /* just replace : with space */
               if (nso>1 && SUMA_IS_PURE_BLANK(so[nso-1])) {
                  so[nso-1] = '\n'; /* Need a newline to make it come out nice */
               }
               so[nso++] = ' '; ++ns;
               while(s[ns] != ':') { so[nso++] = s[ns++]; }
               so[nso++] = ' '; ++ns;
            } else { /* remove all spaces */
               /* remove preceding new line just to keep superfluous 
               new line characters that were there for the purpose of keeping
               the output width short. Do not remove the newline if there
               is two of them in a row, or there is certain punctuation 
               before the newline.*/
               if (nso>1 && so[nso-1] == '\n' && 
                           (so[nso-2] != '\n' && so[nso-2] != ':'))so[nso-1]=' ';
               ns += bln+2;
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
