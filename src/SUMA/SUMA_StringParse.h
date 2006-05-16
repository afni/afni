#ifndef SUMA_STRING_PARSE_INCLUDED
#define SUMA_STRING_PARSE_INCLUDED

/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Begin string parsing macros <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< */
/*! a macro version of C's isspace
   returns 1 if charater is considered a blank
   \sa SUMA_SKIP_BLANK
*/
#define SUMA_IS_BLANK(c) ( ((c) == ' ' || (c) == '\t' || (c) == '\n' || (c) == '\v' || (c) == '\f' || (c) == '\r') ? 1 : 0 )
/*!
   \brief advances pointer to next non-space, see isspace function for characters I check for.
   op must be NULL terminated, if eop is NULL
   eop is a limit address not to be reached by op
   \sa SUMA_IS_BLANK
*/
#define SUMA_SKIP_BLANK(op, eop){  \
   while (*op != '\0' && op != eop && SUMA_IS_BLANK(*op)) ++op; \
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
/*!
   \brief advance pointer past a string
   \param op (char *) pointer to char array
   \param eop (char *) DO not search op past eop (NULL ok if op is NULL terminated)
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
   \brief advance pointer past a next number
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
   \brief copies characters between [op,op2[ into a new NULL terminated string str
   str should be freed with SUMA_free
*/
#define SUMA_COPY_TO_STRING(op,op2,sval){   \
   int m_imax, m_i; \
   if (sval) { SUMA_SL_Err("sval must be null when macro is called"); } \
   else if (op2 > op) { /* copy the deed */  \
      m_imax = op2 - op;   \
      if (m_imax > 5000) {   SUMA_SL_Warn("Unexpectedly large field!"); } \
      sval = (char *)SUMA_calloc(m_imax + 2, sizeof(char));   \
      if (!sval) { SUMA_SL_Crit("Failed To Allocate"); } \
      else { for (m_i=0; m_i < m_imax; ++m_i) { sval[m_i] = op[m_i]; } sval[m_imax] = '\0'; }\
   }  \
}

/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> End string parsing macros <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< */

#endif
