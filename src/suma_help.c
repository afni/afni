/* Trying to isolate SUMA's help functions so that they can 
be used from AFNI */
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

static DList *All_GUI_Help = NULL;
static char *DocumentedWidgets = NULL; /*!< Widget names for which a Sphinx 
                                     documentation entry has been created */ 
char *SUMA_get_DocumentedWidgets(void) { return(DocumentedWidgets); }
char *SUMA_set_DocumentedWidgets(char **s) {
   static char FuncName[]={"SUMA_set_DocumentedWidgets"};
   if (!s || !*s) {
      SUMA_S_Err("Come on friend!");
      SUMA_RETURN(DocumentedWidgets);
   }
   SUMA_ifree(DocumentedWidgets);
   DocumentedWidgets = *s; *s = NULL;
   SUMA_RETURN(DocumentedWidgets);
}

void SUMA_free_DocumentedWidgets(void) { 
                        SUMA_ifree(DocumentedWidgets); return; }
                        
void SUMA_Free_Widget_Help(void *data)
{
   static char FuncName[]={"SUMA_Free_Widget_Help"};
   GUI_WIDGET_HELP *gwh = (GUI_WIDGET_HELP *)data;
   
   SUMA_ENTRY;
   if (data) SUMA_free(data);
   SUMA_RETURNe;
}

/* Format help key string */
char * SUMA_hkf_eng(char *keyi, TFORM target, char *cm)
{
   static char FuncName[]={"SUMA_hkf_eng"};
   static char ss[20][512];
   char key1[256], key2[256], *direc="kbd";
   static int c;
   char *s, cs[5]={""}, *wname_URI=NULL;
   int ichar=-1;
   
   if (!cm) cm = "";
   
   ++c;
   if (c > 19) c = 0;
   s = (char *)ss[c]; s[0] = s[511] = '\0';
   if (!keyi) return(s);
   switch (target) {
      default:
      case TXT: /* SUMA */
         /* Actually COMMA, PERIOD, STAR mods are not needed, leave 
         for posterity.*/
         if (strstr(keyi,"COMMA")) {
            snprintf(key1, 255, ",");
         } else if (strstr(keyi,"PERIOD")) {
            snprintf(key1, 255, ".");
         } else if (strstr(keyi,"STAR")) {
            snprintf(key1, 255, "*");
         } else {
            snprintf(key1, 255, "%s", keyi);
         }
            snprintf(s, 511, "  %s", key1);
         return(s);
         break;
      case SPX: /* Sphinx */
         if (strstr(keyi,"->") == keyi) {
            /* Won't work if you pass key with blanks before '->'
               But why do such a thing? */ 
            snprintf(key1, 255, "%s", keyi+2);
            snprintf(key2, 255, "%s", keyi+2);
            direc = "menuselection";
         } else {
            snprintf(key1, 255, "%s", keyi);
            snprintf(key2, 255, "%s", keyi);
            direc = "kbd";
         }
         
         if (key1[1] == '\0') {
            ichar = 0;
         } else if (key1[strlen(key1)-2] == '+'){
            ichar = strlen(key1)-1;
         } else ichar = -1;
         
         if (ichar > -1) { 
            if (SUMA_IS_UPPER_C(key1[ichar])) {
               sprintf(cs,"UC_");
            } else { 
               sprintf(cs,"LC_");
            }
         } else {
            cs[0] = '\0';
         }
         
         #if 0 /* Good for sphinx, not good for having permalinks ! */
         snprintf(s, 511, "\n.. _%s%s%s:\n\n:%s:`%s`"
            , cm, cs, deblank_allname(key1,'_') 
            , direc, deblank_name(key2));
         #elif 1 /* Good for sphinx and for permalinks */
         direc = "";
         snprintf(s, 511, "\n.. _%s%s%s:\n\n:ref:`%s %s<%s%s%s>`"
            , cm, cs, deblank_allname(key1,'_') 
            , deblank_name(key2), direc, cm, cs, deblank_allname(key1,'_'));
         #else
         /* Brute force, and a pain, as you can see below.
            Left here as illustration for 'raw html use */
         /* Note that I endup with two labels for the key,
         one as an html permalink and another sphinx one
         preceding the text of the first line. The reason
         this was done has to do with how the help
         for each key is defined explicitly in 
         a series of SUMA_StringAppend() calls.
         I could skip the second (sphinx) label but then
         my html page would have ':' at the beginning of
         each line. The solution would be to store the
         help for each key much as I do for each widget
         and make SUMA_hkf() take the 1st line and body (a parallel 
         to the widget hint and help) as options. That's a lot
         of tediousness I don't care for quite yet. */
         wname_URI = SUMA_append_replace_string(cm,
                              deblank_allname(key1,'_'),cs,0);
         SUMA_Sphinx_Widget_Name_2_Link(wname_URI);
               snprintf(s, 511, "\n"
           ".. _%s%s%s:\n"
           "\n"
           ".. only:: latex or latexpdf\n"
           "\n"
           "   :%s:`%s`\n"
           "\n"
           "..\n"
           "\n"
           ".. only:: html\n"
           "\n"
           "   .. raw:: html\n"
           "\n"
           "      <div class=\"section\" id=\"%s\">\n"
           "      <p><a class=\"section\" href=\"#%s\" title=\"%s keyb link\">"
           "<strong>%s</strong>:</a> </p></div>\n"
           "\n"
           "..\n"
           "\n"
           ":%s:`%s`",
                  cm, cs, deblank_allname(key1,'_') ,
                  direc, deblank_name(key2),
                  wname_URI, wname_URI,
                  deblank_name(key2), deblank_name(key2),
                  direc, deblank_name(key2)); 
                              
            SUMA_ifree(wname_URI);

         #endif
         
         return(s);
         break;
   }
   return(s);
}

char * SUMA_hkf(char *keyi, TFORM target) {
   /* for main suma area keys */
   return(SUMA_hkf_eng(keyi,target,""));
}

char * SUMA_hkcf(char *keyi, TFORM target) {
   /* for colormap area keys */
   return(SUMA_hkf_eng(keyi,target,"CM_"));
}

char *SUMA_Sphinx_Widget_Name_2_Link(char *name) 
{
   static char FuncName[]={"SUMA_Sphinx_Widget_Name_2_Link"};
   int m_i, m_c=0; 
   
   SUMA_ENTRY;
   
   if (name)   {
      SUMA_TO_LOWER(name);
      if (name[strlen(name)-1] == '.') name[strlen(name)-1]='\0';
      
      for (m_i=0, m_c=0; m_i<strlen(name); ++m_i) {
         if (SUMA_IS_BLANK(name[m_i]) || name[m_i] == '/' || 
             name[m_i] == '[' || name[m_i] == ']' || name[m_i] == '.' ||
             name[m_i] == '_' || name[m_i] == '+') {
            name[m_c++] = '-'; 
         } else if (name[m_i] == '>') {
            /* ignore it */
         } else {
            name[m_c++] = name[m_i];
         }
      }
   }
   name[m_c] = '\0';
   
   SUMA_RETURN(name);
}

/* Format GUI section */
char * SUMA_gsf_eng(char *uwname, TFORM target, char **hintout, char **helpout)
{
   static char FuncName[]={"SUMA_gsf_eng"};
   static char ss[20][512], wnameclp[256];
   char key1[256], key2[256], *direc="kbd", *lnm=NULL;
   static int c;
   char *s=NULL, *su=NULL, *shh=NULL, *sii=NULL, *stmp=NULL, 
         *wname = NULL, *wname_URI=NULL;
   int ichar=-1, i, ntip=0;
   SUMA_Boolean found = NOPE;
   GUI_WIDGET_HELP *gwh=NULL, *gwhi=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   ++c;
   if (c > 19) c = 0;
   s = (char *)ss[c]; s[0] = s[511] = '\0';
   
   if ((helpout && *helpout) || (hintout && *hintout)) {
      SUMA_S_Err("string init error");
      return(s);
   }

   
   if (!uwname) return(s);
   /* make a copy, uwname is likely a static pointer from a convenience
   function. Could change underneath you */
   wname = SUMA_copy_string(uwname);
   
   switch (target) {
      default:
      case TXT: /* SUMA */
         snprintf(s, 511, "  %s", wname);
         SUMA_ifree(wname); return(s);
         break;
      case WEB:
         SUMA_LH("Webbing with %s", wname);
         if (helpout || hintout) {
            SUMA_S_Err("Not supposed to call WEB as target with "
                       "helpout or hintout");
            SUMA_ifree(wname); return(s);
         }
         lnm = SUMA_copy_string(wname);
         if (!(gwh = SUMA_Get_GUI_Help(lnm, target, NULL, NULL,0))) {
            SUMA_S_Err("No help for %s\n", lnm);
            SUMA_suggest_GUI_Name_Match(lnm, 8, NULL);
         }
         SUMA_ifree(lnm);

         if (!gwh) { SUMA_ifree(wname); return(s); }

         
         found = NOPE;
         lnm = SUMA_copy_string(wname);
         if (gwh->type == 1) { /* a regular olde widget */
            /* DO NOT rely on SUMA_is_Documented_Widget()
               outside of targ == WEB */
            if (!SUMA_is_Documented_Widget(lnm)) {
               /* Not all widget in a table have entries so get
               rid of .c??, .r??, .[*], or .[*,*] and 
               try again. */
               ntip = strlen(lnm)-1;
               if (ntip > 4) {
                  while (ntip > 0 &&lnm[ntip] != '.') --ntip;
               }
               if ( (strlen(lnm)-ntip) == 4 && 
                     (lnm[ntip+1] == 'r' || lnm[ntip+1] == 'c') &&
                     SUMA_IS_DIGIT(lnm[ntip+2]) &&
                     SUMA_IS_DIGIT(lnm[ntip+3]) ) {
                  lnm[ntip] = '\0';
               } else if ((strlen(lnm)-ntip) > 3 &&
                          lnm[strlen(lnm)-1] == ']' &&
                          lnm[ntip+1] == ']') {
                  lnm[ntip] = '\0';   
               }
               /* try again */
               if (SUMA_is_Documented_Widget(lnm)) {
                  SUMA_LH("Found after much suffering as %s", lnm);
                  found = YUP;
               }
            } else {
               SUMA_LH("Got Widget with %s, %s", s, lnm);
               found=YUP;
            }
         }
         snprintf(s,511,"https://afni.nimh.nih.gov/pub/dist/doc/htmldoc");
         if (found) {
            SUMA_Sphinx_Widget_Name_2_Link(lnm);
            SUMA_strncat(s,"/SUMA/Controllers.html#", 511);
            SUMA_strncat(s,lnm,511);
            SUMA_ifree(sii);
            SUMA_ifree(lnm);
         } else { /* Either a container widget
                    or a widget for which no match
                    was found */
            /*  backup until you find a container widget then try for it */
            gwhi = NULL;
            i = 1;
            while (!gwhi && i < gwh->name_lvl){
                        /* Try some guessing. 
                           Before March 4 2015 only headings had permalinks
                           created by SPHINX. 
                           Now I manually insert permalinks for the vast 
                           majority of the widgets. Still, some widgets 
                           such as table cells, or frames, might not have their
                           own entries so we try to get help from their container
                           */
               stmp = SUMA_copy_string(SUMA_Name_GUI_Help_eng(gwh,-i));
               SUMA_LH("Now at %s", stmp);
               gwhi = SUMA_Get_GUI_Help(stmp, target, NULL, NULL, 0);
               if (gwhi && gwhi->type == 0) {
                  SUMA_LH("Got one at %s!",stmp);

               } else {
                  gwhi = NULL; /* try again */
               }
               SUMA_ifree(stmp);
               ++i; /* keep going lower */
            }

            gwh = gwhi;
            if (!gwh || gwh->name_lvl<1) { 
               SUMA_LH("No good link found, going with default");
               SUMA_ifree(wname); return(s); 
            }

            /* Turn the container name to a link */
            if (gwh->hint) {
               sii = SUMA_copy_string(gwh->hint);
               SUMA_Sphinx_Widget_Name_2_Link(sii);
               SUMA_strncat(s,"/SUMA/Controllers.html#", 511);
               SUMA_strncat(s,sii,511);
               SUMA_ifree(sii);
            }
         }
         SUMA_ifree(wname); 
         return(s);
         break;
      case SPX: /* Sphinx */
         if (!(gwh = SUMA_Get_GUI_Help(wname, target, &shh, &sii,3))) {
            SUMA_S_Err("No help for %s\n", wname);
            SUMA_suggest_GUI_Name_Match(wname, 8, NULL);
            shh = SUMA_copy_string(wname);
            sii = SUMA_copy_string(wname);
         }
         
         if (!sii) sii = SUMA_copy_string("No Hint");
         if (helpout) *helpout = shh;
         if (hintout) *hintout = sii;
         
         if (!gwh) { 
            if (!hintout) SUMA_ifree(sii); SUMA_ifree(wname); return(s); 
         }
         
         su = (char *)SUMA_calloc(strlen(sii)+2, sizeof(char));
         
         lnm = gwh->name[gwh->name_lvl-1];
         snprintf(wnameclp, 255, "%s", lnm);
         if (strstr(wnameclp,".r00")) { /* get rid of .r00 */
            wnameclp[strlen(lnm)-4]='\0';
         }
         if (strstr(wnameclp,".c00")) { /* get rid of .c00 */
            wnameclp[strlen(lnm)-4]='\0';
         }
         
         switch (gwh->type) {
            case 0: /* container only */
               if (gwh->name_lvl == 1) {
                  for (i=0; i<strlen(sii); ++i) {su[i] = '-';} su[i] = '\0';
                  snprintf(s, 511, "\n"
                                   ".. _%s:\n"
                                   "\n"
                                   "%s\n"
                                   "%s\n",
                              wname, sii, su);
               } else if (gwh->name_lvl == 2) {
                  for (i=0; i<strlen(sii); ++i) {su[i] = '^';} su[i] = '\0';
                  snprintf(s, 511, "\n"
                                   ".. _%s:\n"
                                   "\n"
                                   "%s\n"
                                   "%s\n",
                              wname, sii, su);
               } else if (gwh->name_lvl == 3) {
                  for (i=0; i<strlen(sii); ++i) {su[i] = '"';} su[i] = '\0';
                  snprintf(s, 511, "\n"
                                   ".. _%s:\n"
                                   "\n"
                                   "%s\n"
                                   "%s\n",
                              wname, sii, su);
               } else if (gwh->name_lvl == 4) {
                  for (i=0; i<strlen(sii); ++i) {su[i] = '.';} su[i] = '\0';
                  snprintf(s, 511, "\n"
                                   ".. _%s:\n"
                                   "\n"
                                   "%s\n"
                                   "%s\n",
                              wname, sii, su);
               } else {
                  snprintf(s, 511, "\n"
                                   "   .. _%s:\n"
                                   "\n"
                                   "**%s**: %s\n"
                                   "\n",
                              wname, wnameclp,sii);
               }
               break;
            case 1: /* actual widget */
               #if 0 /* Looks nice, but no permalinks */
               snprintf(s, 511, "\n"
                                "   .. _%s:\n"
                                "\n"
                                "**%s**: %s\n"
                                "\n",
                              wname, wnameclp,sii);
               #elif 1 /* Good for sphinx and for permalinks */
               snprintf(s, 511, "\n"
                                "   .. _%s:\n"
                                "\n"
                                ":ref:`%s<%s>`: %s\n"
                                "\n",
                              wname, wnameclp, wname, sii);
                #else
               /* Brute force, and a pain, as you can see below.
               Left here as illustration for 'raw html use */
               /* The "only::" directives below are not
               necessary if we are only producing html
               output. I kept them in should we build 
               other than html in the future */
               wname_URI = SUMA_copy_string(wname);
               SUMA_Sphinx_Widget_Name_2_Link(wname_URI);
               snprintf(s, 511, "\n"
        " .. _%s:\n"
        "\n"
        "   .. only:: latex or latexpdf\n"
        "\n"
        "      **%s**:\n"
        "\n"
        "   ..\n"
        "\n"
        "   .. only:: html\n"
        "\n"
        "      .. raw:: html\n"
        "\n"
        "         <div class=\"section\" id=\"%s\">\n"
        "         <p><a class=\"section\" href=\"#%s\" title=\"%s widget link\">"
        "<strong>%s</strong>:</a> %s</p></div>\n"
        "\n"
        "   ..\n"
        "\n",
                              wname, 
                              wnameclp,
                              wname_URI, wname_URI, wnameclp,
                              wnameclp,sii);
               SUMA_ifree(wname_URI);
               #endif
               break;
            case 2: /* Just permalink, no text to appear on purpose */
               wname_URI = SUMA_copy_string(wname);
               SUMA_Sphinx_Widget_Name_2_Link(wname_URI);
               snprintf(s, 511, "\n"
           ".. only:: html\n"
           "\n"
           "   .. raw:: html\n"
           "\n"
           "      <div class=\"section\" id=\"%s\">\n"
           "      <p><a class=\"section\" href=\"#%s\" title=\"%s widget link\">"
           "</a> </p></div>\n"
           "\n"
           "..\n"
           "\n",
                              wname_URI, wname_URI, wnameclp);
               SUMA_ifree(wname_URI);
               break;
            default:
               SUMA_S_Err("Bad type %d", gwh->type);
               break;
         }
         
         if (!hintout) SUMA_ifree(sii); 
         if (!helpout) SUMA_ifree(shh); 
         SUMA_ifree(su);
         SUMA_ifree(wname);
         return(s);
         break;
   }
   
   return(s);
}


/* 
   Register help for a widget. This is to replace all individual
calls to MCW_register_help and _hint.

   Pointer to help is copied so make sure help is not freed.
   
   Widget help becoming centralized to help with auto generation of
   help webpage
*/

SUMA_Boolean SUMA_Register_Widget_Help(Widget w, int type, char *name, 
                                       char *hint, char *help)
{
   static char FuncName[]={"SUMA_Register_Widget_Help"};
   char *s=NULL, *st=NULL;
   
   SUMA_ENTRY;
   
   if (!SUMA_Register_GUI_Help(name, hint, help, w, type)) {
      SUMA_S_Err("Failed at string level registration");
      SUMA_RETURN(NOPE);
   }
   
   if (w) {
      if (help) {
         s = SUMA_copy_string(help);
         s = SUMA_Sphinx_String_Edit(&s, TXT, 0);
         st = s;
         s = SUMA_Break_String(st, 60); SUMA_ifree(st); 
         /* DO not free s, MCW_register_help uses the pointer as 
            data to the help callback */
         MCW_register_help(w, s);
      }
      if (hint) {
         /* Just make a copy of the hint and don't worry about
         what got passed! */
         s = SUMA_copy_string(hint);
         MCW_register_hint(w, s);
      }
   }
      
   SUMA_RETURN(YUP);
}  

SUMA_Boolean SUMA_Register_Widget_Children_Help(Widget w, int type, char *name, 
                                                char *hint, char *help)
{
   static char FuncName[]={"SUMA_Register_Widget_Children_Help"};
   char *s=NULL, *st=NULL;
   
   SUMA_ENTRY;
   
   if (!w || !help) {
      SUMA_S_Err("NULL widget!!! or No Help");
      SUMA_RETURN(NOPE);
   }
   
   if (!SUMA_Register_GUI_Help(name, hint, help, w, type)) {
      SUMA_S_Err("Failed at string level registration");
      SUMA_RETURN(NOPE);
   }
   
   if (help) {
      s = SUMA_copy_string(help);
      s = SUMA_Sphinx_String_Edit(&s, TXT, 0);
      st = s;
      s = SUMA_Break_String(st, 60); SUMA_ifree(st); 
         /* DO not free s, MCW_register_help uses the pointer as 
            data to the help callback */
      MCW_reghelp_children(w, s);
   }
   
   if (hint) {
      /* Just make a copy of the hint and don't worry about
      what got passed! */
      s = SUMA_copy_string(hint);
      MCW_register_hint(w, s);
   }
   SUMA_RETURN(YUP);
}

/*!
   Return the help and hint strings stored in the GUI help list.
   
   \param gname (char *)Name of widget
   \param format (int) 0: Default
                       1: Sphinx format
   \param helpout (char **): If Not NULL, this will contain
                             the pointer to a copy of the help string
                             formatted per format.
                             Note that if helpout, *helpout 
                             must be NULL at the function call.
                             You must also free *helpout when 
                             done with it.
   \param hintout (char **): If Not NULL, this will contain
                             the pointer to a copy of the hint string
                             formatted per format. 
                             if (hintout), *hintout must be NULL at  
                             function call. You must also free *hintout 
                             when done with it.
   \param whelp_off (int ): If non-zero, number of blank charcters by which
                            to offset the lines of a widget's help string 
   \return gwh (GUI_WIDGET_HELP *)  A pointer to the structure containing 
                                    the widget help. 
                                    NULL if nothing was found.
*/ 
GUI_WIDGET_HELP *SUMA_Get_GUI_Help( char *gname, TFORM format, 
                                    char **helpout, char **hintout,
                                    int whelp_off)
{
   static char FuncName[]={"SUMA_Get_GUI_Help"};
   char *s = NULL, *ss=NULL;
   DListElmt *el = NULL;
   int nn;
   GUI_WIDGET_HELP *gwhc=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!gname) { SUMA_S_Err("NULL name"); SUMA_RETURN(gwhc);}
   
   if (!All_GUI_Help || !dlist_size(All_GUI_Help)) {
      SUMA_S_Err("No help list");
      SUMA_RETURN(gwhc);
   }
   if ((helpout && *helpout) || (hintout && *hintout)) {
      SUMA_S_Err("string init error");
      SUMA_RETURN(gwhc);
   }
   
   /* Seek name in list */
   SUMA_LH("Seeking %s", gname);
   /* Find name in list. 
      Note that no attempt at fast search is done here even though the 
      list is alphabetical... */
   gwhc = NULL;
   do {
      if (!el) el = dlist_head(All_GUI_Help);
      else el = dlist_next(el);
      gwhc = (GUI_WIDGET_HELP *)el->data;
      ss = SUMA_Name_GUI_Help(gwhc);
      SUMA_LH("Comparing %s to %s (nn=%d)", 
                  ss, gname, 
                  strcmp(SUMA_Name_GUI_Help(gwhc), 
                       gname));
      if (ss == gname) { /* Safety oblige */
         SUMA_S_Crit("Collision handsome! Don't send me gname pointers "
                     "returned by the evil SUMA_Name_GUI_Help()!");
      }
      if ((nn = strcmp(ss, gname)) == 0) {
         el = NULL;
      } else {
         gwhc = NULL;
      }
   } while (el && el != dlist_tail(All_GUI_Help));
   
   if (gwhc) {
      SUMA_LH("Got new: %s, nn=%d", SUMA_Name_GUI_Help(gwhc), nn);
      if (helpout) {
         *helpout = SUMA_copy_string(gwhc->help);
         if (gwhc->type == 1 && whelp_off) {/* widget and offset requested */
            SUMA_Sphinx_String_Edit(helpout, format, whelp_off);
         } else {
            SUMA_Sphinx_String_Edit(helpout, format, 0);
         }
      }
      if (hintout) {
         *hintout = SUMA_copy_string(gwhc->hint);
         SUMA_Sphinx_String_Edit(hintout, format, 0);
      }
   } else {
      SUMA_LH("Got nothing for %s", gname);
   }
   
   SUMA_RETURN(gwhc);
}

/*!
   Return the help struct for a certain widget.
   
*/ 
GUI_WIDGET_HELP *SUMA_Get_Widget_Help( Widget w )
{
   static char FuncName[]={"SUMA_Get_Widget_Help"};
   char *s = NULL;
   DListElmt *el = NULL;
   int nn;
   GUI_WIDGET_HELP *gwhc=NULL;
   
   SUMA_ENTRY;
   
   if (!w) { SUMA_S_Err("NULL w"); SUMA_RETURN(gwhc);}
   
   if (!All_GUI_Help || !dlist_size(All_GUI_Help)) {
      SUMA_S_Err("No help list");
      SUMA_RETURN(gwhc);
   }
   
   
  
   /* Find widget in list. */
   gwhc = NULL;
   el = NULL;
   do {
      if (!el) el = dlist_head(All_GUI_Help);
      else el = dlist_next(el);
      gwhc = (GUI_WIDGET_HELP *)el->data;
      if (w == gwhc->w) {
         el = NULL;
      } else {
         gwhc = NULL;
      }
   } while (el && el != dlist_tail(All_GUI_Help));
   
   if (!gwhc && (s = XtName(w))) { /* Try matching the hint to the widget name,
                   This is done for container widgets */
      el = NULL;
      do {
         if (!el) el = dlist_head(All_GUI_Help);
         else el = dlist_next(el);
         gwhc = (GUI_WIDGET_HELP *)el->data;
         if (gwhc->hint && !strcmp(s, gwhc->hint)) {
            el = NULL;
         } else {
            gwhc = NULL;
         }
      } while (el && el != dlist_tail(All_GUI_Help));   
   }
   
   SUMA_RETURN(gwhc);
}

void SUMA_Show_All_GUI_Help(DList *dl, FILE *fout, int detail, int format)
{
   static char FuncName[]={"SUMA_Show_All_GUI_Help"};
   char *s=NULL;
   
   SUMA_ENTRY;
   
   if (!fout) fout = stdout;
   
   s = SUMA_All_GUI_Help_Info(dl, detail, format);
   
   fprintf(fout, "%s", s);
   
   SUMA_ifree(s);
   
   SUMA_RETURNe;
}

int SUMA_Register_GUI_Help(char *which, char *hint, char *help, 
                           Widget widget, int type)
{
   static char FuncName[]={"SUMA_Register_GUI_Help"};
   GUI_WIDGET_HELP *gwh=NULL, *gwhc=NULL;
   char *sstmp = NULL, *s=NULL, buf[64]={""};
   DListElmt *el=NULL;
   static char WhinedNames[1025]={""};
   int nn;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!hint && !which) {
      SUMA_S_Err("No hint, no which");
      SUMA_RETURN(NOPE);
   }
   
   if (!which) { /* get from hint if it has "which" string between
                   ":which:" directives.
                   ":which:SurfCont->more:which:hint goes here"*/
      if ( !(strstr(hint,":which:") == hint) || 
           !(sstmp = strstr(hint+strlen(":which:"),":which:"))  ) {
         SUMA_S_Err("No which and no :which: in hint. No good");
         SUMA_RETURN(NOPE);        
      }
      which = hint+strlen(":which:");
      hint = sstmp+strlen(":which:");
   }
   
   gwh = (GUI_WIDGET_HELP *)SUMA_calloc(1,sizeof(GUI_WIDGET_HELP));
   
   gwh->w = (void *)widget;
   gwh->type = type; /* 1, regular widget, 
                        0, container widget, used for organizing the help, 
                        2, a way to insert an html URI for widgets not
                           individually tracked in the help. For now
                           this is only done for table entries. */
   
   /* parse which: SurfCont->more */
   sstmp = which;
   gwh->name_lvl = 0;
   while( (s = strstr(sstmp, "->")) && gwh->name_lvl < 9 ) {
      if (s == sstmp) {
         SUMA_S_Err("Empty child in %s\n", which);
         SUMA_free(gwh);
         SUMA_RETURN(NOPE);
      }
      nn = s - sstmp;
      if (nn > 63) {
         SUMA_S_Err("Too wordy for me.");
         SUMA_free(gwh);
         SUMA_RETURN(NOPE);
      }
      strncpy(gwh->name[gwh->name_lvl], sstmp, nn);
                     gwh->name[gwh->name_lvl][nn+1] = '\0';
      sstmp = s+2; /* skip -> */
      ++gwh->name_lvl;
   }
   /* copy last one */
   strncpy(gwh->name[gwh->name_lvl], sstmp, 63); 
                     gwh->name[gwh->name_lvl][64] = '\0'; 
   ++gwh->name_lvl;
   
   /* store the hint */
   if (hint) {
      if (strlen(hint)>255) {
         SUMA_S_Err("Hint too long");
         SUMA_free(gwh);
         SUMA_RETURN(NOPE);
      }
      strncpy(gwh->hint, hint, 255); gwh->hint[255] = '\0';
   } 
   
   /* store the help */   
   gwh->help = help;
   
   /* Put it all in */
   if (!All_GUI_Help) {
      All_GUI_Help = (DList *)SUMA_calloc(1, sizeof(DList));
      dlist_init(All_GUI_Help, SUMA_Free_Widget_Help);
   }
   
   /* insert in list */
   if (!dlist_size(All_GUI_Help)) {
      dlist_ins_next(All_GUI_Help, dlist_head(All_GUI_Help), (void *)gwh);
      SUMA_RETURN(YUP);
   }
   
   SUMA_LH("Inserting '%s' with  %s %s", 
               SUMA_Name_GUI_Help(gwh), gwh->hint, gwh->help);
   /* Insert in alphabetical order */
   el = dlist_head(All_GUI_Help);
   do {
      gwhc = (GUI_WIDGET_HELP *)el->data;
      if ((nn = strcmp(SUMA_Name_GUI_Help(gwhc), 
                       SUMA_Name_GUI_Help(gwh))) == 0) {
         snprintf(buf, 63, "%s;",SUMA_Name_GUI_Help(gwh));
         if (LocalHead || !(sstmp=strstr(WhinedNames, buf))) {
            SUMA_S_Note("GUI Name %s already in use. No special help entry."
                        "%s",
                        SUMA_Name_GUI_Help(gwh), 
                  LocalHead ? "":"\nFurther warnings for this name curtailed.");
            if (!sstmp) SUMA_strncat(WhinedNames,buf, 1023);
            if (LocalHead) SUMA_DUMP_TRACE("Trace at duplicate GUI name");
            SUMA_free(gwh);
         }
         SUMA_RETURN(YUP);
      } else if (nn < 0) {
         dlist_ins_next(All_GUI_Help, el, (void *)gwh);
         SUMA_RETURN(YUP);
      } else {
         el = dlist_next(el);
      }
   } while (el && el != dlist_tail(All_GUI_Help));
   
   /* Reached bottom without going over, put on the top */
   dlist_ins_prev(All_GUI_Help, dlist_head(All_GUI_Help), (void *)gwh);
   
   /* A debug for when you get to the bottom condition */
   if (LocalHead) {
      SUMA_Show_All_GUI_Help(All_GUI_Help, NULL, 0, 0);
   }
   
   SUMA_RETURN(YUP);
}

char *SUMA_Name_GUI_Help(GUI_WIDGET_HELP *gwh) {
   static char FuncName[]={"SUMA_Name_GUI_Help"};
   return(SUMA_Name_GUI_Help_eng(gwh, 0));
}

char *SUMA_Name_GUI_Help_eng(GUI_WIDGET_HELP *gwh, int lvl)
{
   static char FuncName[]={"SUMA_Name_GUI_Help_eng"};
   static char sa[10][641], *s=NULL;
   static int nc=0;
   int k;
   
   SUMA_ENTRY;
   
   ++nc; if (nc > 9) nc = 0;
   s = (char *)sa[nc]; s[0] = '\0';
   
   if (!gwh) SUMA_RETURN(s);
   
   if (lvl <= 0) lvl = gwh->name_lvl+lvl;
   if (lvl > gwh->name_lvl) lvl = gwh->name_lvl;
   
   for (k=0; k<lvl; ++k) {
      SUMA_strncat(s,gwh->name[k], 640);
      if (k<lvl-1) SUMA_strncat(s,"->", 640);
   }
   
   SUMA_RETURN(s);
}

char *SUMA_All_GUI_Help_Info(DList *dl, int detail, int format)
{
   static char FuncName[]={"SUMA_All_GUI_Help_Info"};
   SUMA_STRING *SS=NULL;
   DListElmt *el=NULL;
   char *s=NULL;
   GUI_WIDGET_HELP *gwh=NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);

   if (!dl) {
      SS = SUMA_StringAppend(SS,"NULL dl");  
   } else {
      SS = SUMA_StringAppend_va(SS,
                                "Help for %d widgets. Detail %d, Format %d\n"
                                "--------------------------------------------\n",
                                dlist_size(dl), detail, format);
      el = dlist_head(dl);
      do {
         gwh = (GUI_WIDGET_HELP *)el->data;
         if (!gwh) SUMA_StringAppend(SS,"NULL widget data!");
         else {
               SUMA_StringAppend_va(SS,"Widget: %s (%p)\n", 
                           SUMA_Name_GUI_Help(gwh), gwh->w);
            if (detail > 0)
               SUMA_StringAppend_va(SS,"  hint: %s\n", gwh->hint);
            if (detail > 1) {
               s = SUMA_copy_string(gwh->help);
               switch (format) {
                  case 0:
                     SUMA_Sphinx_String_Edit(&s, TXT, 0);
                     SUMA_StringAppend_va(SS,"  help: %s\n", s);
                     SUMA_ifree(s);
                     break;
                  default:
                  case 1:
                     SUMA_Sphinx_String_Edit(&s, SPX, 0);
                     SUMA_StringAppend_va(SS,"  help: %s\n", s);
                     SUMA_ifree(s);
                     break;
               }
            }
            SUMA_StringAppend_va(SS,"\n");
         }
         el = dlist_next(el);   
      } while (el);
   }
   
   SUMA_StringAppend_va(SS,"\n");
   
   SUMA_SS2S(SS, s);
   SUMA_RETURN(s);
}

SUMA_Boolean SUMA_is_Documented_Widget(char *wname)
{
   static char FuncName[]={"SUMA_is_Documented_Widget"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!wname) SUMA_RETURN(NOPE);
   if (!DocumentedWidgets) {
      SUMA_S_Err("Must call SUMA_set_DocumentedWidgets() first!");
      SUMA_RETURN(NOPE);
   }
   if (strstr(DocumentedWidgets, wname)) SUMA_RETURN(YUP);
   
   SUMA_LH("Widget %s not in:\n%s", wname, DocumentedWidgets);
   SUMA_RETURN(NOPE); 
}


void SUMA_suggest_GUI_Name_Match(char *wname, int nmx, DList *dl)
{
   static char FuncName[]={"SUMA_suggest_GUI_Name_Match"};
   int i, nlot;
   char **lot=NULL, **slot=NULL;
   DListElmt *el=NULL;
   GUI_WIDGET_HELP *gwhc=NULL;
   
   SUMA_ENTRY;
   
   if (!dl) dl = All_GUI_Help;
   
   if (!dl || !dlist_size(dl)) {
      SUMA_S_Err("No list to be had");
      SUMA_RETURNe;
   }
   lot = (char **)SUMA_calloc(dlist_size(dl), sizeof(char *));
   nlot = 0; i = 0;
   gwhc = NULL;
   do {
      if (!el) el = dlist_head(dl);
      else el = dlist_next(el);
      gwhc = (GUI_WIDGET_HELP *)el->data;
      lot[i] = SUMA_copy_string(SUMA_Name_GUI_Help(gwhc));
      ++i;
   } while (el && el != dlist_tail(dl));
   nlot = i;
  
   slot = approx_str_sort(lot, nlot, wname, 0, NULL, 0, NULL, NULL);
   
   if (nmx < 0) nmx = nlot;
   fprintf(SUMA_STDERR,
               "Suggestions for %s\n"
               "---------------\n", wname);
   for (i=0; i < nlot && i < nmx; ++i) {
      fprintf(SUMA_STDERR,
               "                %s\n", slot[i]);
   }
   
   for (i=0; i < nlot; ++i) {
      SUMA_ifree(lot[i]);
      SUMA_ifree(slot[i]);
   }
   SUMA_ifree(lot);
   SUMA_ifree(slot);
   SUMA_RETURNe;
}

