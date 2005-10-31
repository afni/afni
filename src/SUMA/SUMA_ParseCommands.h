#ifndef SUMA_PARSECOMMANDS_INCLUDED
#define SUMA_PARSECOMMANDS_INCLUDED

/* structures to be used by most command line programs */
#define SUMA_GENERIC_PROG_MAX_SURF 10  /*!< Maximum number of surfaces allowed*/
typedef struct {
   SUMA_SO_File_Type iType;
   char *sv_name;
   char *surf_names[SUMA_GENERIC_PROG_MAX_SURF];
   int N_surf;
   char *spec_file;
   char *in_name;
   char *surftype;
   char *out_prefix;   /* this one's dynamically allocated so you'll have to free it yourself */
   char *out_vol_prefix; /* this one's dynamically allocated so you'll have to free it yourself */
   char out_vol_view[5];
   int out_vol_exists;
   char *out_grid_prefix; /* this one's dynamically allocated so you'll have to free it yourself */
   char out_grid_view[5];
   int out_grid_exists;
   char *in_vol_prefix; /* this one's dynamically allocated so you'll have to free it yourself */
   char in_vol_view[5];
   int in_vol_exists;
   int MaskMode;
   char *cmask;
   THD_3dim_dataset *in_vol;
   float VolCM[3];   /* input volume's center of mass */
   double *mcdatav; /* the dataset that is passed to the marching cube algorithm */
   int debug;
   int ninmask;
   int fix_winding;
   float v0;
   float v1;
   int nvox;
   double *dvec;
   int obj_type;
   int obj_type_res;
   int xform;
   SUMA_SO_File_Format SurfFileFormat;
   SUMA_SO_File_Type SurfFileType;
   /* following fields are intended for use in ConvexHull only */
   char *in_1D;  /* name of 1D file containing XYZ coords */
   float *XYZ; /* a 3*N_XYZ vector of XYZ coordinates. This vector should be freed at the end*/
   int N_XYZ;  /* number of points in XYZ */
   /* following fields are intended for use in BrainWarp only */
   float ExpFrac; /* a fraction (0.01) used to control the rate of expansion of the surface (see su3 variable in SUMA_StretchToFitLeCerveau ) */
   float Zt; /* a fraction controlling the separation between brain and non brain, see variable tb in SUMA_StretchToFitLeCerveau ) */
   int N_it; /* number of iterations */
   int Icold; /* number of Ico subdivisions */
   int NodeDbg; /* node to debug */
   float t;
   float tm;
   float t2;
   float t98;
   float r;
   float cog[3];
   float d1;
   float su1;
   float UseNew;
   float d4;
   float *ztv;
   int Kill98;
   int NoEyes;
   int NNsmooth;
   int smootheach;
   float avoid_vent;
   int smooth_end;
   int *k98mask;
   int k98maskcnt;
   float travstp;
   float *Stop;
   int MaxIntIter;
   int UseExpansion;
   float PercInt;
   int UseSkull;
   float bot_lztclip;
   float var_lzt;
   int send_hull;
   int DemoPause;
   int DoSpatNorm;
   float SpatNormDxyz;
   int monkey;
   int WriteSpatNorm;
   int fillhole;
   THD_3dim_dataset *iset;
   FILE *dbg_eyenodes;
   float SpatShift[3];
   THD_3dim_dataset *OrigSpatNormedSet;   
   THD_3dim_dataset *in_edvol;
   float blur_fwhm;
   int iset_hand;
   char *shrink_bias_name;
   float *shrink_bias;
   int NearestNode;
   int NearestTriangle;
   int DistanceToMesh;
   int ProjectionOnMesh;
   int Data;
   
   char *in_nodeindices;
   
   byte b1;
   byte b2;
   
   void *popt;    /*< NULL pointer to hide program specific structure */
} SUMA_GENERIC_PROG_OPTIONS_STRUCT;

#define SUMA_MAX_SURF_ON_COMMAND 100
#define SUMA_N_ARGS_MAX 1000

typedef struct {
   /* spec related input */
   char *spec_names[SUMA_MAX_SURF_ON_COMMAND];
   int N_spec_names;
   
   char *s_surfnames[SUMA_MAX_SURF_ON_COMMAND];
   char *s_surfprefix[SUMA_MAX_SURF_ON_COMMAND];
   char *s_surfpath[SUMA_MAX_SURF_ON_COMMAND];
   int s_N_surfnames;
   
   /* -i_ related input */
   char *i_surfnames[SUMA_MAX_SURF_ON_COMMAND];
   char *i_surftopo[SUMA_MAX_SURF_ON_COMMAND];
   char *i_surfpath[SUMA_MAX_SURF_ON_COMMAND];
   char *i_surfprefix[SUMA_MAX_SURF_ON_COMMAND];
   char *i_state[SUMA_MAX_SURF_ON_COMMAND];
   char *i_group[SUMA_MAX_SURF_ON_COMMAND];
   int i_anatomical[SUMA_MAX_SURF_ON_COMMAND];
   int i_N_surfnames;
   SUMA_SO_File_Format i_FF[SUMA_MAX_SURF_ON_COMMAND];
   SUMA_SO_File_Type i_FT[SUMA_MAX_SURF_ON_COMMAND];
   
   /* -ipar_ related input */
   char *ipar_surfnames[SUMA_MAX_SURF_ON_COMMAND];
   char *ipar_surftopo[SUMA_MAX_SURF_ON_COMMAND];
   char *ipar_surfpath[SUMA_MAX_SURF_ON_COMMAND];
   char *ipar_surfprefix[SUMA_MAX_SURF_ON_COMMAND];
   char *ipar_state[SUMA_MAX_SURF_ON_COMMAND];
   char *ipar_group[SUMA_MAX_SURF_ON_COMMAND];
   int ipar_anatomical[SUMA_MAX_SURF_ON_COMMAND];
   int ipar_N_surfnames;
   SUMA_SO_File_Format ipar_FF[SUMA_MAX_SURF_ON_COMMAND];
   SUMA_SO_File_Type ipar_FT[SUMA_MAX_SURF_ON_COMMAND];
   
   /* -o_related input */
   char *o_surfnames[SUMA_MAX_SURF_ON_COMMAND];
   char *o_surftopo[SUMA_MAX_SURF_ON_COMMAND];
   char *o_surfpath[SUMA_MAX_SURF_ON_COMMAND];
   char *o_surfprefix[SUMA_MAX_SURF_ON_COMMAND];
   char *o_state[SUMA_MAX_SURF_ON_COMMAND];
   char *o_group[SUMA_MAX_SURF_ON_COMMAND];
   int o_anatomical[SUMA_MAX_SURF_ON_COMMAND];
   int o_N_surfnames;
   SUMA_SO_File_Format o_FF[SUMA_MAX_SURF_ON_COMMAND];
   SUMA_SO_File_Type o_FT[SUMA_MAX_SURF_ON_COMMAND];
   
   /* -t_related input */
   char *t_surfnames[SUMA_MAX_SURF_ON_COMMAND];
   char *t_surftopo[SUMA_MAX_SURF_ON_COMMAND];
   char *t_surfpath[SUMA_MAX_SURF_ON_COMMAND];
   char *t_surfprefix[SUMA_MAX_SURF_ON_COMMAND];
   char *t_state[SUMA_MAX_SURF_ON_COMMAND];
   char *t_group[SUMA_MAX_SURF_ON_COMMAND];
   int t_anatomical[SUMA_MAX_SURF_ON_COMMAND];
   int t_N_surfnames;
   SUMA_SO_File_Format t_FF[SUMA_MAX_SURF_ON_COMMAND];
   SUMA_SO_File_Type t_FT[SUMA_MAX_SURF_ON_COMMAND];
   
   byte arg_checked[SUMA_N_ARGS_MAX];
   int N_args;
   char *sv[SUMA_MAX_SURF_ON_COMMAND];
   int N_sv;
   char *vp[SUMA_MAX_SURF_ON_COMMAND];
   int N_vp;
   
   /* -talk_suma options */
   SUMA_COMM_STRUCT *cs;

   /* flags for what to read */
   byte accept_t;
   byte accept_s;
   byte accept_i;
   byte accept_ipar;
   byte accept_o;
   byte accept_spec;
   byte accept_sv;
   byte accept_talk_suma;
   byte check_input_surf;
} SUMA_GENERIC_ARGV_PARSE;
int  SUMA_GetNextCommand (char *S, char d, char term, char *Scom);
SUMA_Boolean  SUMA_RegisterCommand(char *S, char d, char term, char *Scom, SUMA_Boolean Prepend);
int SUMA_CommandCode(char *Scom);
const char *SUMA_CommandString (SUMA_ENGINE_CODE code);
SUMA_Boolean SUMA_RegisterEngineData (SUMA_EngineData *MTI, char *Fldname, void *FldValp, char *DestName, char *SourceName, SUMA_Boolean PassByPointer);
SUMA_Boolean SUMA_FreeEngineData (SUMA_EngineData *MTI);
SUMA_ENGINE_FIELD_CODE SUMA_EngineFieldCode(char *Scom);
const char *SUMA_EngineFieldString (SUMA_ENGINE_FIELD_CODE i);
SUMA_Boolean SUMA_ReleaseEngineData (SUMA_EngineData *MTI, char *Location);
SUMA_Boolean SUMA_InitializeEngineData (SUMA_EngineData *MTI);
int SUMA_EngineSourceCode (char *Scom);
void SUMA_EngineSourceString (char *Scom, int ses_code);
const char *SUMA_DomainKinships_String (SUMA_DOMAIN_KINSHIPS code);
DList *SUMA_CreateList (void);
SUMA_EngineData *SUMA_InitializeEngineListData (SUMA_ENGINE_CODE CommandCode);
DListElmt * SUMA_RegisterEngineListCommand (DList *list, SUMA_EngineData * EngineData,  
                                             SUMA_ENGINE_FIELD_CODE Fld, void *FldValp, 
                                             SUMA_ENGINE_SOURCE Src, void *Srcp, SUMA_Boolean PassByPointer, 
                                             SUMA_ENGINE_INSERT_LOCATION InsertAt, DListElmt *Element);
SUMA_Boolean SUMA_ReleaseEngineListElement (DList *list, DListElmt *element);
DList * SUMA_DestroyList (DList *list);
DList * SUMA_EmptyDestroyList (DList *list);
void SUMA_FreeEngineListData(void *MTI);
SUMA_ENGINE_CODE SUMA_GetListNextCommand (DList *list);
void SUMA_ShowList (DList *list, FILE *Out);
void SUMA_FreeMessageListData(void *Hv);
SUMA_Boolean SUMA_ReleaseMessageListElement (DList *list, DListElmt *element) ;
DList *SUMA_CreateMessageList (void);
SUMA_Boolean SUMA_RegisterMessage ( DList *list, char *Message, char *Source, SUMA_MESSAGE_TYPES Type, SUMA_MESSAGE_ACTION Action);
char *SUMA_BuildMessageLog (DList *ML);
void SUMA_FreeActionStackData(void *asdata);
DList *SUMA_CreateActionStack (void);
void SUMA_ReleaseActionStackData (void *asdata);
DList *SUMA_EmptyDestroyActionStack (DList *AS);
const char *SUMA_ColMixModeString (SUMA_COL_MIX_MODE mode);
SUMA_SO_File_Type SUMA_SurfaceTypeCode (char *cd);
const char * SUMA_SurfaceTypeString (SUMA_SO_File_Type tp);
SUMA_SO_File_Type SUMA_guess_surftype_argv(char *str);
void *SUMA_strtol_vec(char *op, int nvals, int *nread, SUMA_VARTYPE vtp);
SUMA_GENERIC_ARGV_PARSE *SUMA_CreateGenericArgParse(char *optflags);
SUMA_GENERIC_ARGV_PARSE *SUMA_FreeGenericArgParse(SUMA_GENERIC_ARGV_PARSE *ps);
char *SUMA_help_IO_Args(SUMA_GENERIC_ARGV_PARSE *opt);
SUMA_GENERIC_ARGV_PARSE *SUMA_Parse_IO_Args (int argc, char *argv[], char *optflags);
SUMA_GENERIC_PROG_OPTIONS_STRUCT * SUMA_Alloc_Generic_Prog_Options_Struct(void);
SUMA_GENERIC_PROG_OPTIONS_STRUCT * SUMA_Free_Generic_Prog_Options_Struct(SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt);
void *SUMA_AdvancePastNumbers(char *op, char **opend, SUMA_VARTYPE tp);

/*!
   \brief Macro that adds a command to the head of command list.
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, Command, Src, Srcp)

   \param list (DList *) pointer to list 
   \param Command (SUMA_ENGINE_CODE) command code
   \param Src (SUMA_ENGINE_SOURCE) source of command
   \param Srcp (void *) pointer to source pointer. (No need to type cast it yourself, macro will)

   - Expects the variable FuncName (char *) to be defined already (that's the case in all of SUMA's functions)
   - No Engine Data can be passed with this macro

*/
#define SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, Command, Src, Srcp) {\
   SUMA_EngineData *ED_macro; \
   ED_macro = SUMA_InitializeEngineListData (Command);   \
   if (!SUMA_RegisterEngineListCommand (  list, ED_macro, \
                                          SEF_Empty, NULL,  \
                                          Src, (void *)Srcp, NOPE,   \
                                          SEI_Head, NULL)) {   \
      fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);   \
   }  \
}

/*!
   \brief Macro that adds a command to the tail of command list.
   
   \sa SUMA_REGISTER_HEAD_COMMAND_NO_DATA
*/
#define SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, Command, Src, Srcp) {\
   SUMA_EngineData *ED_macro; \
   ED_macro = SUMA_InitializeEngineListData (Command);   \
   if (!SUMA_RegisterEngineListCommand (  list, ED_macro, \
                                          SEF_Empty, NULL,  \
                                          Src, (void *)Srcp, NOPE,   \
                                          SEI_Tail, NULL)) {   \
      fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);   \
   }  \
}

/*!
   \brief Macro that reports an error to the log 

*/
#define SUMA_L_Err(msg) {\
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Error, SMA_Log); \
}
/*!
   \brief Macro that reports an error to stderr 

*/
#define SUMA_S_Err(msg) {\
   fprintf (SUMA_STDERR, "Error %s (%s:%d):\n %s\n", FuncName, __FILE__ , __LINE__, msg);  \
}
/*!
   \brief Macro that reports an error to stderr and log 

*/
#define SUMA_SL_Err(msg) {\
   SUMA_S_Err(msg);  \
   SUMA_L_Err(msg); \
}
/*!
   \brief Macro that reports an error to stderr and log and popup

*/
#define SUMA_SLP_Err(msg) {\
   SUMA_S_Err(msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Error, SMA_LogAndPopup); \
}

/*!
   \brief Macro that reports a notice to the log 

*/
#define SUMA_L_Note(msg) {\
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Notice, SMA_Log); \
}
/*!
   \brief Macro that reports a notice to stderr 

*/
#define SUMA_S_Note(msg) {\
   fprintf (SUMA_STDERR, "Notice %s:\n %s\n", FuncName, msg);  \
}
/*!
   \brief Macro that reports a notice to stderr and log 

*/
#define SUMA_SL_Note(msg) {\
   fprintf (SUMA_STDERR, "Notice %s:\n %s\n", FuncName, msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Notice, SMA_Log); \
}
/*!
   \brief Macro that reports a notice to stderr and log and popup

*/
#define SUMA_SLP_Note(msg) {\
   fprintf (SUMA_STDERR, "Notice %s:\n %s\n", FuncName, msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Notice, SMA_LogAndPopup); \
}

/*!
   \brief Macro that reports a text message to the log 

*/
#define SUMA_L_Text(msg) {\
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Text, SMA_Log); \
}
/*!
   \brief Macro that reports a text message to stderr 

*/
#define SUMA_S_Text(msg) {\
   fprintf (SUMA_STDERR, "%s\n", msg);  \
}
/*!
   \brief Macro that reports a text message to stderr and log 

*/
#define SUMA_SL_Text(msg) {\
   fprintf (SUMA_STDERR, "%s\n", msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Text, SMA_Log); \
}
/*!
   \brief Macro that reports a text message to stderr and log and popup

*/
#define SUMA_SLP_Text(msg) {\
   fprintf (SUMA_STDERR, "%s\n", msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Text, SMA_LogAndPopup); \
}

/*!
   \brief Macro that reports a warning to the log 

*/
#define SUMA_L_Warn(msg) {\
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Warning, SMA_Log); \
}
/*!
   \brief Macro that reports a warning to stderr 

*/
#define SUMA_S_Warn(msg) {\
   fprintf (SUMA_STDERR, "Warning %s:\n %s\n", FuncName, msg);  \
}
/*!
   \brief Macro that reports a warning to stderr and log 

*/
#define SUMA_SL_Warn(msg) {\
   fprintf (SUMA_STDERR, "Warning %s:\n %s\n", FuncName, msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Warning, SMA_Log); \
}
/*!
   \brief Macro that reports a warning to stderr and log and popup

*/
#define SUMA_SLP_Warn(msg) {\
   fprintf (SUMA_STDERR, "Warning %s:\n %s\n", FuncName, msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Warning, SMA_LogAndPopup); \
}

/*!
   \brief Macro that reports a critical error to the log 

*/
#define SUMA_L_Crit(msg) {\
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Critical, SMA_Log); \
}
/*!
   \brief Macro that reports a critical error to stderr 

*/
#define SUMA_S_Crit(msg) {\
   fprintf (SUMA_STDERR, "Critical %s:\n %s\n", FuncName, msg);  \
}
/*!
   \brief Macro that reports a critical error to stderr and log 

*/
#define SUMA_SL_Crit(msg) {\
   fprintf (SUMA_STDERR, "Critical %s:\n %s\n", FuncName, msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Critical, SMA_Log); \
}
/*!
   \brief Macro that reports a critical error to stderr and log and popup

*/
#define SUMA_SLP_Crit(msg) {\
   fprintf (SUMA_STDERR, "Critical %s:\n %s\n", FuncName, msg);  \
   SUMA_RegisterMessage (SUMAg_CF->MessageList, msg, FuncName, SMT_Critical, SMA_LogAndPopup); \
}

#define SUMA_BEEP {  \
   if (SUMAg_SVv[0].X->TOPLEVEL) XBell (XtDisplay (SUMAg_SVv[0].X->TOPLEVEL), SUMA_BEEP_LENGTH_MS);  \
}
/*!
   \brief Macro that reports a message to SUMA_STDERR if LocalHead is set to YUP
*/
#define SUMA_LH(msg) {\
   if (LocalHead) fprintf (SUMA_STDERR, "%s (%s:%d):\n %s\n", FuncName, __FILE__, __LINE__,msg);  \
}

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
