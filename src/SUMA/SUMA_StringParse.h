#ifndef SUMA_STRING_PARSE_INCLUDED
#define SUMA_STRING_PARSE_INCLUDED

/* >>>>>>>>>>>>>>>>>>>>>>> Begin string parsing macros <<<<<<<<<<<<<<<<<<<<<<< */
/* See also SUMA_IS_DIGIT_CHAR, SUMA_IS_NUM_CHAR */
#define SUMA_IS_DIGIT(c) ( ((c) < '0' || (c) > '9') ? 0 : 1 )
 
#define SUMA_IS_PURE_BLANK(c) ( ((c) == ' ' || (c) == '\t' ) )

/*! a macro version of C's isspace
   returns 1 if charater is considered a blank
   \sa SUMA_SKIP_BLANK
*/
#define SUMA_IS_BLANK(c) ( (SUMA_IS_PURE_BLANK(c) || (c) == '\n' || (c) == '\v' || (c) == '\f' || (c) == '\r') ? 1 : 0 )

#define SUMA_IS_PUNCT(c) ( ((c) == '.' || (c) == ',' || (c) == ':' || (c) == ';') ? 1 : 0 )

/* See also SUMA_IS_EOL */
#define SUMA_IS_LINE_END(c) ( ((c) == '\n' || (c) == '\f' || (c) == '\r') ? 1 : 0 )
#define SUMA_IS_NICE_PREFIX_CHAR(c) ( (SUMA_IS_BLANK(c) \
                                       || (c) == ':' || (c) == ';' || (c) == '[' || (c) == ']' || (c) == '{' || (c) == '}'  \
                                       || (c) == '<' || (c) == '>' || (c) == '#' || (c) == '*' || (c) == '?' )   \
                                       ? 1 : 0 )    

/* Is this a sphinx underline character? */
#define SUMA_IS_UNDERLINE_CHAR(cc) ((\
                   (cc) == '-' || \
                   (cc) == '=' || \
                   (cc) == '*' || \
                   (cc) == '#' ) ? 1:0)

/*!
   \brief advances pointer to next non-space, see isspace function for characters I check for.
   op must be NULL terminated, if eop is NULL
   eop is a limit address not to be reached by op
   \sa SUMA_IS_BLANK
*/
#define SUMA_SKIP_BLANK(op, eop){  \
   while (*op != '\0' && op != eop && SUMA_IS_BLANK(*op)) ++op; \
}

#define SUMA_SKIP_PURE_BLANK(op, eop){  \
   while (*op != '\0' && op != eop && SUMA_IS_PURE_BLANK(*op)) ++op; \
}

#define SUMA_SKIP_LINE(op, eop){   \
   while (*op != '\0' && op != eop && *op != '\n' && *op != '\f' && *op != '\r') ++op; \
   SUMA_SKIP_BLANK(op, eop);\
}
#define SUMA_IS_COMMENT_LINE(opor, eop, cc, ans){   \
   char *m_op = opor;  \
   ans = 0;\
   SUMA_SKIP_BLANK(m_op, eop);\
   if (*m_op == cc) { ans = 1; } \
}

#define SUMA_DEBLANK(name,repl) {  \
   if (name)   {  \
      int m_i, m_r=0; \
      if (repl != '\0') { \
         for (m_i=0; m_i<strlen(name); ++m_i) { if (SUMA_IS_BLANK(name[m_i])) { name[m_i] = repl; }  } \
      }  else {   \
         for (m_i=0; m_i<strlen(name); ++m_i) { if (!SUMA_IS_BLANK(name[m_i])) { name[m_r] = name[m_i]; ++m_r;} } \
         name[m_r] = '\0'; \
      }  \
   }\
}


#define SUMA_NICEATE_FILENAME(name,repl) {  \
   if (name)   {  \
      int m_i, m_r=0; \
      if (repl) { \
         for (m_i=0; m_i<strlen(name); ++m_i) { if (SUMA_IS_NICE_PREFIX_CHAR(name[m_i])) { name[m_i] = repl; }  } \
      }  else {   \
         for (m_i=0; m_i<strlen(name); ++m_i) { if (!SUMA_IS_NICE_PREFIX_CHAR(name[m_i])) { name[m_r] = name[m_i]; ++m_r;} } \
         name[m_r] = '\0'; \
      }  \
   }\
}
/*!
   \brief advance pointer to next blank, skips quoted strings (works with " and ' combos, I hope)
   Hello 'djjdk sskjd' Jon
   if op[0] is the ' then after the macro, op[0] will be the space
   just before Jon
   op must be NULL terminated, if eop is NULL
   eop is a limit address not to be reached by op
*/
#define SUMA_SKIP_TO_NEXT_BLANK(op, eop){  \
   char m_quote_open = '\0';   \
   while (*op != '\0' && op !=eop && !( !m_quote_open && (*op == ' ' || *op == '\t' || *op == '\n' || *op == '\v' || *op == '\f' || *op == '\r')) ) { \
      if (*op == '"' || *op == '\'') {  \
         if (!m_quote_open) m_quote_open = *op; \
         else if (m_quote_open == *op) m_quote_open = '\0'; \
      }  \
      ++op; \
   }  \
}

/* See also SUMA_IS_LINE_END */
#define SUMA_IS_EOL(cc) ((  (cc) == '\n' || (cc) == '\v'                      \
                          ||(cc) == '\f' || (cc) == '\r' || (cc) == '\0') )
#define SUMA_SKIP_TO_EOL(op, eop){  \
   char m_quote_open = '\0';   \
   while (*op != '\0' && op !=eop && !(!m_quote_open && SUMA_IS_EOL(*op)) ) { \
      if (*op == '"' || *op == '\'') {  \
         if (!m_quote_open) m_quote_open = *op; \
         else if (m_quote_open == *op) m_quote_open = '\0'; \
      }  \
      ++op; \
   }  \
}

#define SUMA_SKIP_TO_NEXT_CHAR(op, eop, ch){  \
   char m_quote_open = '\0';   \
   while (*op != '\0' && op !=eop && !( !m_quote_open && (*op == ch)) ) { \
      if (*op == '"' || *op == '\'') {  \
         if (!m_quote_open) m_quote_open = *op; \
         else if (m_quote_open == *op) m_quote_open = '\0'; \
      }  \
      ++op; \
   }  \
}


/*!
   \brief Find the addresses limiting a section between two blanks, 
   Hello   'djjdk sskjd'    Jon
   if op is pointing the blank space somewhere after Hello then
   op will then point to '
   and op2 will point to the first blank after sskjd'
   op must be NULL terminated, if eop is NULL
   eop is a limit address not to be reached by op
*/
#define SUMA_GET_BETWEEN_BLANKS(op, eop, op2){  \
   SUMA_SKIP_BLANK(op, eop); /* skip first blanks*/   \
   op2 = op;                 /* skip till next blanks */ \
   SUMA_SKIP_TO_NEXT_BLANK(op2, eop);  \
}

#define SUMA_GET_TO_EOL(op, eop, op2){  \
   if (!SUMA_IS_EOL(*op)) { \
      SUMA_SKIP_BLANK(op, eop); /* skip first blanks*/   \
      op2 = op;                 /* skip till next blanks */ \
      SUMA_SKIP_TO_EOL(op2, eop);  \
   } else { op2 = op; } /* Stay right where you are */ \
}

/*! \brief Count number of blank delimited words. 
      Does not alter op or eop
      Returns number of words in N_word;
   */
#define SUMA_COUNT_WORDS(op, eop, N_word){   \
   char *m_ops=op, *m_opn=op;   \
   N_word = 0; \
   do { \
      m_ops = m_opn; SUMA_GET_BETWEEN_BLANKS(m_ops,eop,m_opn); \
      if (m_opn > m_ops) ++N_word;  \
   } while (*m_opn != '\0' && m_opn != eop && m_ops != m_opn);   \
}
   
/*!
   \brief advance pointer past a string
   \param op (char *) pointer to char array
   \param eop (char *) DO not search op past eop or '\0' in op
               DO NOT CALL THE MACRO WITH eop SET TO (op+Nchars), i.e. do not do this:
               SUMA_ADVANCE_PAST(op,(op+5),attr,Found,Word); to check only 5 chars ahead
               if you do so, a part of the if condition (op < eop) will always evaluate to 
               true because it is expanded to op < (op+5) !
   \param attr (char *) character string searched (NULL terminated)
   \Found (int)   0 --> Not found, op is not changed
                  1 --> Found, op is set just past the location of attr
   \Word (int)    0 --> Search for an exact match of attr, regardless of how its surrounded
                  1 --> Make sure attr is surrounded by blanks
*/
#define SUMA_ADVANCE_PAST(op,eop,attr,Found,Word){ \
   int m_natr = strlen(attr); \
   char *m_bop = op;    \
   Found = 0;  \
   while (op < eop && *op != '\0' && Found < m_natr) { \
      if (*op == attr[Found]) {  \
         /* found a match, increment match counter */ \
         ++Found; \
      } else { /* no match, break */   \
         Found = 0;  \
      }  \
      ++op; \
      if (Word && Found == m_natr) { /* make sure word is surrounded by blank */\
         if ( !(*op == '\0' || op == eop || SUMA_IS_BLANK(*op)) ) { /* character after word is not blank */ \
            Found = 0;  /* reset  */ \
         } else { /* check for blank after word */ \
            char *m_bef = op - m_natr - 1;/* pointer to character before attr */ \
            if ( !(m_bef < m_bop || SUMA_IS_BLANK(*m_bef)) ) { /* character before word is not blank */ \
               Found = 0;  /* reset */ \
            }  \
         }  \
      }  \
   }  \
   /* fprintf(SUMA_STDERR,"%s: Searched %d chars.\n", FuncName, op-m_bop);  */\
   if (Found != m_natr) { Found = 0; op = m_bop; }/* reset pointer to origin */ \
}

/*!
   Like SUMA_ADVANCE_PAST, but uses a NULL-terminated list of words rather than just one word
   char *key_list[] = { "static", "char", "FuncName", "=", "{" , "}", ";", NULL};
   int  *gap_list[] = {    -1   ,  5    ,  5        ,  5 , 2   , -1 ,  20, -1 };
   Find a sequence of keys where key_list[i] is present and follows key_list[i-1] by 
   less than gap_list[i] (if gap_list[i] is >= 0) 
   If the sequence is not found then:
   nFound = 0 and op is unchanged and op_beg is NULL
   else op is set past the sequence of strings, and op_beg is set to the beginning
   of the string sequence
*/
#define SUMA_ADVANCE_PAST_SEQUENCE(op, eop, op_beg, key_list, gap, nFound, Word) {   \
   char *m_op_func , *m_op_prev; \
   int m_good, m_found, m_d;   \
   nFound = 0;  \
   m_found = 0;   \
   m_good = 1;  \
   m_op_func = m_op_prev = op;   \
   while (key_list[nFound] && m_good) {  \
      /* if (LocalHead) fprintf(SUMA_STDERR,"key[%d]=%s ", nFound, key_list[nFound]); */\
      m_op_prev = m_op_func;  \
      SUMA_ADVANCE_PAST(m_op_func, eop, key_list[nFound], m_found, Word);   \
      if (m_found > 0) { /* key found */\
         /* if (LocalHead) fprintf(SUMA_STDERR," Found "); */\
         if (!nFound || gap[nFound] < 0 || m_op_func - m_op_prev - strlen(key_list[nFound]) < gap[nFound]) { /* inside limit */  \
            /* if (LocalHead) fprintf(SUMA_STDERR," in gap "); */\
            if (!nFound) op_beg = m_op_func - strlen(key_list[0]);   \
            ++nFound; \
         } else { /* outside gap */ \
            /* if (LocalHead) fprintf(SUMA_STDERR," out of gap (%d, Augment by %d) ", m_op_func - m_op_prev, (int)strlen(key_list[0])); */\
            nFound = 0;                               /* reset search */   \
            m_op_func = op_beg + strlen(key_list[0]); /* at after first key */   \
            op_beg = m_op_func;  \
         }  \
      } else {    \
         { /* nothing here */   \
            m_good = 0;  \
         }  \
      }  \
   }  \
   if (!m_good) {  \
      /* SUMA_LH("Done."); */\
      nFound = 0;  \
      op_beg = NULL; \
   } else { \
      op = m_op_func;   \
   }  \
}

/*!
   \brief advance pointer past the next number
         Number has to start at op
   \param op (char *) pointer to char array (NULL terminated)
   \param num (double) output of strtod function
   \Found (int)   0 --> Not found, op is not changed
                  1 --> Found, op is set just past the location after number
*/

#define SUMA_ADVANCE_PAST_NUM(op, num, Found){\
   char *m_ope=NULL;    \
   Found = 0;  \
   num = strtod(op, &m_ope); \
   if (m_ope > op) { /* something found */ \
      Found = 1; \
      op = m_ope; \
   } else { /* just to be safe */\
      num = 0;\
   }  \
}

/*!
   \brief advance pointer past the next integer. 
            Unlike SUMA_ADVANCE_PAST_NUM, 
            integer does not have to start at op 
   \param op (char *) pointer to char array (NULL terminated)
   \param num (int) output of (int) strtod function
   \Found (int)   0 --> Not found, op is not changed
                  1 --> Found, op is set just past the location after number
*/

#define SUMA_ADVANCE_PAST_INT(op, num, Found){\
   char *m_ope=NULL;    \
   Found = 0;  \
   m_ope = op; \
   while (!SUMA_IS_DIGIT(*m_ope)) ++m_ope; /* skip till digit */\
   num = (int)strtod(m_ope, &m_ope); \
   if (m_ope > op) { /* something found */ \
      Found = 1; \
      op = m_ope; \
   } else { /* just to be safe */\
      num = 0;\
   }  \
}

/*!
   \brief copies characters between [op,op2[ into a new NULL terminated string str
   str should be freed with SUMA_free
*/
#define SUMA_COPY_TO_STRING(op,op2,sval){   \
   int m_imax, m_i; \
   if (!op) { SUMA_SL_Err("NULL input"); }\
   if (sval) { SUMA_SL_Err("sval must be null when macro is called"); } \
   else {\
      if (op2 == NULL) op2 = op+strlen(op);  \
      if (op2 > op) { /* copy the deed */  \
         m_imax = op2 - op;   \
         if (m_imax > 5000) {   SUMA_SL_Warn("Unexpectedly large field!"); } \
         sval = (char *)SUMA_calloc(m_imax + 2, sizeof(char));   \
         if (!sval) { SUMA_S_Crit("Failed To Allocate"); } \
         else { \
            for (m_i=0; m_i < m_imax; ++m_i) { sval[m_i] = op[m_i]; }   \
            sval[m_imax] = '\0'; \
         }\
      }  \
   }  \
}

/*!
   \brief Fills characters between [m_op,m_op2[ into a preallocated string str
*/
#define SUMA_FILL_STRING(m_op,m_op2,strinp){   \
   char *sval = strinp;\
   if (!m_op2) { /* cm_opy till end */  \
      while (*m_op != '\0') { *sval = *m_op; ++sval; ++m_op; } \
   } else { \
      while (*m_op != '\0' && m_op < (char*)m_op2) {  \
         *sval = *m_op; ++sval; ++m_op; \
      } \
   }  \
   *sval = '\0'; \
}

/*!
   \brief Fills N characters between [m_op,m_op2[ into a preallocated string str
   Make sure str can take N+1 chars
*/
#define SUMA_NFILL_STRING(m_op,m_op2,strinp,N){   \
   char *sval = strinp; int m_n=0;\
   if (!m_op2) { /* cm_opy till end */  \
      while (*m_op != '\0' && m_n < N) { *sval = *m_op; ++sval; ++m_op; ++m_n;} \
   } else { \
      while (*m_op != '\0' && m_op < (char*)m_op2 && m_n < N) {  \
         *sval = *m_op; ++sval; ++m_op; ++m_n;\
      } \
   }  \
   *sval = '\0'; \
}

/*!
   \brief print N characters between [op,op2[ into file pointer out
   Leaves op and op2 in place
*/
#define SUMA_NPRINT_STRING(opi,op2,out,N, head, foot){   \
   int m_n=0; FILE *m_outp=SUMA_STDERR; char *m_op=opi, *m_op2=op2;\
   if (out) m_outp=out; \
   if (head) fprintf(m_outp,"%s", head);  \
   if (!m_op2) { /* write till end */  \
      while (*m_op != '\0' && m_n < N) { \
         fprintf(m_outp,"%c", *m_op); ++m_op; ++m_n;\
      } \
   } else { \
      while (*m_op != '\0' && m_op < (char*)m_op2 && m_n < N) {  \
         fprintf(m_outp,"%c", *m_op); ++m_op; ++m_n;\
      } \
   }  \
   if (foot) fprintf(m_outp,"%s", foot);  \
}
/*!
   \brief print characters between [op,op2[ into file pointer out
   Leaves op and op2 in place
*/
#define SUMA_PRINT_STRING(opi,opi2,out,head, foot){   \
   FILE *m_outp=SUMA_STDERR; char *m_op=opi, *m_op2 = opi2;\
   if (out) m_outp=out; \
   if (head) fprintf(m_outp,"%s", head);  \
   if (!m_op2) { /* write till end */  \
      while (*m_op != '\0') { fprintf(m_outp,"%c", *m_op); ++m_op;} \
   } else { \
      while (*m_op != '\0' && m_op < (char*)m_op2) {  \
         fprintf(m_outp,"%c", *m_op); ++m_op;\
      } \
   }  \
   if (foot) fprintf(m_outp,"%s", foot);  \
}

#define SUMA_DEBLANK_RHS(s) {\
   int ns;  \
   if (s && (ns=strlen(s))) { --ns; \
      while (ns >= 0 && SUMA_IS_BLANK(s[ns])) { s[ns]='\0'; --ns; }  \
   }\
}\


/* >>>>>>>>>>>>>>>>>>>>>>> End string parsing macros <<<<<<<<<<<<<<<<<<<<<<<< */

#endif
