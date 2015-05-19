#include "afni.h"
#include "suma_suma.h"
#include "afni_whelp.h"

char *AFNI_All_Documented_Widgets(void)
{
   static char FuncName[]={"AFNI_All_Documented_Widgets"};
   char *s=NULL;
   SUMA_ENTRY;
   s = SUMA_append_replace_string(s,AFNI_Help_AllMainCont(TXT),"\n",3);
   
   /* s = SUMA_append_replace_string(s,SUMA_Help_AllInstaCorrCont(TXT),"\n",3);*/
   SUMA_RETURN(s);
}

char * AFNI_gsf(char *uwname, TFORM target, char **hintout, char **helpout)
{
   static char FuncName[]={"AFNI_gsf"};
   static char sss[64]={"You Should Never Get This"};
   static int lock = 0;
   char *DW = SUMA_get_DocumentedWidgets();
   
   if (target == WEB && !DW) { /* That is when gsf needs DocumentedWidgets */
      char *ss=NULL;
      if (!lock) {
         /* Need to init list of all documented widgets */
         /* Careful - next function will call SUMA_gsf() also.
         Make sure AFNI_gsf() does not rely on DocumentedWidgets
         for anything but WEB targ */
         ss = AFNI_All_Documented_Widgets();
         SUMA_set_DocumentedWidgets(&ss);
         DW = SUMA_get_DocumentedWidgets();
         if (!DW) {
            SUMA_S_Err("Should not fail here");
            lock = 1;
            SUMA_RETURN(sss);
         }
      } else {
         SUMA_S_Err("Failed and locked out");
         SUMA_RETURN(sss);
      }
   }
   
   SUMA_RETURN(SUMA_gsf_eng(uwname, target, hintout, helpout));
}

char * AFNI_Help_AllMainCont (TFORM targ)
{
   static char FuncName[]={"AFNI_Help_AllMainCont"};
   char *s = NULL, *shh=NULL, *sii=NULL;
   int k=0;
   SUMA_STRING *SS = NULL;
   char *worder[] = {
                     "AfniCont",
                     "AfniCont->ProgCont",
                     "AfniCont->ProgCont->done",
                     NULL };
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   k = 0;
   while (worder[k]) {
         s = AFNI_gsf(worder[k], targ, &sii, &shh);
         if (!shh || strstr(sii, shh)) {/* help same as hint */
            SS = SUMA_StringAppend_va(SS, "%s\n", s);
         } else {
            SS = SUMA_StringAppend_va(SS, "%s\n%s\n", 
                                   s, shh?shh:"");
         }
         SUMA_ifree(sii); SUMA_ifree(shh);
      ++k;
   }
          
   SUMA_SS2S(SS, s);
      
   SUMA_RETURN(SUMA_Sphinx_String_Edit(&s, targ, 0));
}

