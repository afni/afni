#include "SUMA_suma.h"
#include "coxplot.h"
#include "SUMA_plot.h"
 
/* SUMA_display.c is too big for my own good. 
   This is part the second                    */
   
void SUMA_cb_createSurfaceCont_MDO(Widget w, XtPointer data, 
                                     XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_createSurfaceCont_MDO"};
   Widget tl, pb, form, DispFrame, SurfFrame, 
          rc_left, rc_right, rc_mamma, rc_gmamma, tls=NULL;
   Display *dpy;
   SUMA_ALL_DO *ado;
   SUMA_MaskDO *mdo;
   char *slabel, *lbl30, *sss=NULL;
   XmString xmstmp; 
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL, *over0=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ado = (SUMA_ALL_DO *)data;
   if (ado->do_type != MASK_type) {
      SUMA_S_Errv("Calling me with (%s) other than MASK_type type,\n" 
                  "I don't like that, call me with MDO",
                  SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
      SUMA_RETURNe;
   }
   mdo = (SUMA_MaskDO *)ado;
   
   SUMA_LHv("Creating SurfaceCont for type %s, label %s\n",
            ADO_TNAME(ado), SUMA_ADO_Label(ado));
   if (LocalHead) {
      SUMA_DUMP_TRACE("Creating SurfaceCont");
   }
   
   if (!(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_S_Errv("Failed to get Controller for ado %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }

   if (!SUMA_SurfCont_SetcurDOp(SurfCont, ado)) {
      SUMA_S_Errv("Failed to Set curDOp for %s\n",
                  SUMA_ADO_Label(ado));
      SUMA_RETURNe;
   }
   
   if (!(curColPlane = SUMA_ADO_CurColPlane(ado))) {
      SUMA_LH("No current col plane for ado %s, OK for masks\n",
              SUMA_ADO_Label(ado));
   }
   
   if (SurfCont->TLS) {
      fprintf (SUMA_STDERR,
               "Error %s: SurfCont->TopLevelShell!=NULL.\n"
               "Should not be here.\n", FuncName);
      SUMA_RETURNe;
   }
   
   tl = SUMA_GetTopShell(w); /* top level widget */
   dpy = XtDisplay(tl);
   
   slabel = (char *)SUMA_malloc(sizeof(char) * 
                                 (strlen(SUMA_ADO_Label(ado)) + 100));
   if (strlen(SUMA_ADO_Label(ado)) > 40) {
      char *tmpstr=NULL;
      tmpstr = SUMA_truncate_string(SUMA_ADO_Label(ado), 40);
      if (tmpstr) { 
         sprintf(slabel,"[%s] Mask Controller", tmpstr);
         free(tmpstr); tmpstr=NULL;
      }
   } else {
      sprintf(slabel,"[%s] Mask Controller", SUMA_ADO_Label(ado));
   }
   
   /* March 12 08: Made font8 default for surface controller */
   if (SUMA_isEnv("SUMA_SurfContFontSize", "BIG")) {
      sss = "font9";
   } else {
      sss = "font8";
   }

   SUMA_LH("Creating dialog shell.");
   if (!SUMAg_CF->X->UseSameSurfCont || 
       !SUMAg_CF->X->CommonSurfContTLW) { /* need a new one */
      #if SUMA_CONTROLLER_AS_DIALOG /* xmDialogShellWidgetClass, 
                                       topLevelShellWidgetClass*/
      tls = XtVaCreatePopupShell (sss,
         XmNtitle, slabel,
         xmDialogShellWidgetClass, tl,
         XmNallowShellResize, True, /* let code resize shell */
         XmNdeleteResponse, XmDO_NOTHING,
         NULL);    
      #else
      SUMA_LH("Creating toplevel shell.");
      /** Feb 03/03:    I was using XtVaCreatePopupShell to create a 
                        topLevelShellWidgetClass. 
                        XtVaCreatePopupShell is used to create dialog 
                        shells not toplevel or appshells. 
                        Of course, it made no difference! */
      tls = XtVaAppCreateShell (sss, "Suma",
         topLevelShellWidgetClass, SUMAg_CF->X->DPY_controller1 ,
         XmNtitle, slabel,
         XmNdeleteResponse, XmDO_NOTHING,
         NULL);   
      #endif

      /* allow for code to resize the shell */
      XtVaSetValues (tls, 
            XmNresizePolicy , XmRESIZE_NONE , 
            XmNallowShellResize , True ,       /* let code resize shell */
            NULL);

      /* handle the close button from window manager */
      XmAddWMProtocolCallback(/* make "Close" window menu work */
         tls,
         XmInternAtom( dpy , "WM_DELETE_WINDOW" , False ) ,
         SUMA_cb_closeSurfaceCont, (XtPointer) ado) ;
      
      if (SUMAg_CF->X->UseSameSurfCont) {
         Widget scroller;
         SUMAg_CF->X->CommonSurfContTLW = tls;
         SUMAg_CF->X->SC_Notebook = 
            XtVaCreateWidget("ControllerBook", xmNotebookWidgetClass,
                             SUMAg_CF->X->CommonSurfContTLW, 
                             XmNbindingWidth, XmNONE,
                             XmNbackPageNumber, 0, 
                             XmNmajorTabSpacing, 0, 
                             XmNminorTabSpacing, 0, 
                             NULL);
            /*XmCreateNotebook (SUMAg_CF->X->CommonSurfContTLW, "ControllerBook",
                              NULL, 0);
              XtVaSetValues(SUMAg_CF->X->SC_Notebook,
                          XmNbindingWidth, XmNONE,
                          NULL); */
            
         
         /* Kill the scroller from hell otherwise no keyboard input
            gets to the baby widgets. Better write my own scroller
            if need be in the future */
         scroller = XtNameToWidget (SUMAg_CF->X->SC_Notebook, "PageScroller");
         XtUnmanageChild (scroller);
      }
   }
   
   if (SUMAg_CF->X->UseSameSurfCont) {
      SurfCont->TLS = SUMAg_CF->X->CommonSurfContTLW;
   } else {
      SurfCont->TLS = tls;
   }
   
   if (!SurfCont->TLS) {
      SUMA_S_Err("Bad logic");
      SUMA_RETURNe;
   }
   
   SUMA_LH("Widgets...");
   if (SUMAg_CF->X->UseSameSurfCont) {
      Arg args[20];
      /* add the page */
      XtSetArg (args[0], XmNnotebookChildType, XmPAGE);
      SurfCont->Page = 
         XmCreateRowColumn (SUMAg_CF->X->SC_Notebook,
                     SUMA_ADO_Label(ado)?SUMA_ADO_Label(ado):"page",
                                              args, 1);
   }
   
   /* create a form widget, manage it at the end ...*/
   SurfCont->Mainform = XtVaCreateWidget ("dialog", 
      xmFormWidgetClass, SurfCont->Page ? 
                              SurfCont->Page:SurfCont->TLS,
      XmNborderWidth , 0 ,
      XmNmarginHeight , SUMA_MARGIN ,
      XmNmarginWidth  , SUMA_MARGIN ,
      XmNshadowThickness, 2,
      XmNshadowType, XmSHADOW_ETCHED_IN,
      NULL); 
   
   rc_gmamma = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->Mainform,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            XmNtopAttachment  , XmATTACH_FORM ,
            XmNbottomAttachment  , XmATTACH_FORM ,
            NULL);
   

   DispFrame = SUMA_CloseBhelp_Frame(rc_gmamma,
                     SUMA_cb_closeSurfaceCont, (XtPointer) ado,
                     "Close Surface controller", SUMA_closeSurfaceCont_help,
                     NULL, NULL, NULL, NULL);
   
   XtVaCreateManagedWidget ("sep", xmSeparatorWidgetClass, rc_gmamma , NULL);
         
   rc_mamma = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rc_gmamma,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            XmNleftAttachment , XmATTACH_FORM ,
            XmNtopAttachment  , XmATTACH_FORM ,
            XmNrightAttachment , XmATTACH_FORM ,
            NULL);
            
   rc_left = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rc_mamma,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL);
   
   rc_right = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, rc_mamma,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, SUMA_MARGIN ,
            XmNmarginWidth , SUMA_MARGIN ,
            NULL); 
   
   {/*surface properties */ 
      Widget rc, label, rc_SurfProp, pb;
     
      /* put a frame */
      SurfFrame = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      XtVaCreateManagedWidget ("Masks",
            xmLabelWidgetClass, SurfFrame, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      
      rc_SurfProp = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfFrame,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL); 
      
      if (1) { /* The properties area */
         char *col_tit[]=  {  "+", "Label", "Type", "Center", "Size", 
                                    "Color", NULL};
         char *col_hint[]= {  "Add new mask", 
                              "Label",
                              "Type ('box' or 'sphere')", 
                              "Center X,Y,Z", 
                              "Size Sx,Sy,Sz", 
                              "Color R G B", NULL };
         char *col_help[]= {  SUMA_SurfContHelp_MaskTypeTbl_c0, 
                              SUMA_SurfContHelp_MaskTypeTbl_c1,
                              SUMA_SurfContHelp_MaskTypeTbl_c2, 
                              SUMA_SurfContHelp_MaskTypeTbl_c3, 
                              SUMA_SurfContHelp_MaskTypeTbl_c4,
                              SUMA_SurfContHelp_MaskTypeTbl_c5, NULL };
         char *row_tit[]=  {  "+", "x", NULL };
         char *row_hint[]= {  "Add new mask", "Mask Properties", NULL};
         char *row_help[]= {  SUMA_SurfContHelp_MaskTypeTbl_c0, 
                              SUMA_SurfContHelp_MaskTypeTbl_r1, NULL};
         if (!SurfCont->rcswr) {
            /* This row column is typically for the dataset range 
               values. For masks we repurpose the variable */
            SurfCont->rcswr = XtVaCreateWidget ("rowcolumn",
                     xmRowColumnWidgetClass, rc_SurfProp,
                     XmNpacking, XmPACK_TIGHT, 
                     XmNorientation , XmVERTICAL ,
                     XmNtopAttachment, XmATTACH_WIDGET ,
                     NULL);
         }
         if (!SurfCont->MaskTable->cells) {
            int colw[6] = { 1, 6, 6, 11, 11, 11 };
            SUMA_CreateTable( SurfCont->rcswr,
                              2, 6, 
                              row_tit, col_tit,  
                              row_hint, col_hint,  
                              row_help, col_help,  
                              colw, YUP, SUMA_string, 
                              SUMA_cb_SetMaskTypeTableValue, NULL,
                              SUMA_MaskTableLabel_EV, NULL,  
                              SUMA_MaskTableCell_EV, NULL, 
                              SurfCont->MaskTable);
         }
      }
      
      XtManageChild (rc_SurfProp);
      if (!XtIsManaged(SurfCont->rcswr)) XtManageChild (SurfCont->rcswr);
      XtManageChild (SurfFrame);
   }
   if (!SUMA_InitMasksTable(SurfCont)) {
      SUMA_S_Err("Failed to initialize table");
      SUMA_RETURNe;
   }

   if (SUMAg_CF->X->UseSameSurfCont) {
      Widget rc=NULL;
      /* put something to cycle through objects */
      if ((rc = SUMA_FindChildWidgetNamed(DispFrame, "rowcolumnCBF"))) {
         XtVaCreateManagedWidget (  "sep", 
                              xmSeparatorWidgetClass, rc, 
                              XmNorientation, XmVERTICAL,NULL);
         SUMA_CreateArrowField ( rc, "Switch",
                           1, 1, 20, 1,
                           2, SUMA_int,
                           YUP,
                           SUMA_cb_SurfCont_SwitchPage, (void *)ado,
                           "Switch to other object controller", 
                           "Switch to other object controller",
                           SurfCont->SurfContPage);
         xmstmp = XmStringCreateLtoR (SUMA_ADO_CropLabel(ado,
                                       SUMA_SURF_CONT_SWITCH_LABEL_LENGTH), 
                                      XmSTRING_DEFAULT_CHARSET);
         SurfCont->SurfContPage_label = XtVaCreateManagedWidget ("dingel", 
               xmLabelWidgetClass, rc,
               XmNlabelString, xmstmp,
               NULL);
         XmStringFree (xmstmp);
      }
   }

   
   
   
   SUMA_LHv("Management ...%p %p %p %p %p\n",
            rc_right, rc_left, rc_mamma, SurfCont->Mainform, SurfCont->Page);

   XtManageChild (rc_right);
   XtManageChild (rc_left);
   XtManageChild (rc_mamma);
   XtManageChild (rc_gmamma);
   XtManageChild (SurfCont->Mainform);
   if (SUMAg_CF->X->UseSameSurfCont) XtManageChild (SurfCont->Page);
   
   #if SUMA_CONTROLLER_AS_DIALOG    
   #else
   /** Feb 03/03: pop it up if it is a topLevelShellWidgetClass, 
   you should do the popping after all the widgets have been created.
   Otherwise, the window does not size itself correctly when open */
   XtPopup(SurfCont->TLS, XtGrabNone);
   #endif
   
   /* realize the widget */
   if (SUMAg_CF->X->UseSameSurfCont) XtManageChild (SUMAg_CF->X->SC_Notebook);
   XtRealizeWidget (SurfCont->TLS);
   
   SUMA_LH("%s",slabel);
   SUMA_free (slabel);

   #if USING_LESSTIF
   SUMA_LH("Less tif fix");
   /* A quick fix to ensure Dset_Mapping 
      gets displayed properly the first time */
   SUMA_cb_ToggleManagementColPlaneWidget(NULL, (XtPointer)(&ado), NULL);
   SUMA_cb_ToggleManagementColPlaneWidget(NULL, (XtPointer)(&ado), NULL);
   #endif

   SUMA_LH("going home.");

   SUMA_MarkSurfContOpen(1, ado);
   SUMA_RETURNe;
}

/*!
   Called when user clicks on Mask table cell
   expects in cd
*/
void SUMA_MaskTableCell_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   static char FuncName[]={"SUMA_MaskTableCell_EV"};
   SUMA_ALL_DO *ado=NULL, *curDO=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   XButtonEvent * bev = (XButtonEvent *) ev ;
   int  i, j, n, Found, incr=0;
   float fv[4];
   void *cv=NULL;
   SUMA_MaskDO *mdo = NULL;
   DList *list=NULL;
   SUMA_TABLE_FIELD *TF = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called Button %d", bev->button);
   
   SurfCont = SUMAg_CF->X->AllMaskCont;
   curDO = SUMA_Cont_ADO(SurfCont);
   TF = SurfCont->MaskTable;
   
   /* see note in bbox.c optmenu_EV for the condition below*/
   if( bev->button == Button2 ) {
     XUngrabPointer( bev->display , CurrentTime ) ;
     SUMA_RETURNe ;
   }
   
   if( w == NULL || TF == NULL ) { SUMA_RETURNe ; }

   incr = 0;
   switch (bev->button) {
      case Button1:
         SUMA_LH("Button 1");
         break;
      case Button2:
         SUMA_LH("Button 2");
         break;
      case Button3:
         SUMA_LH("Button 3");
         break;
      case Button4:
      case 6:  /* This is shift and wheel on mac, Button6 is not in X.h ! */
         SUMA_LH("Button 4/6 %d", bev->button);
         incr = -1;
         break;
      case Button5:
      case 7: 
         SUMA_LH("Button 5/7 %d", bev->button);
         incr = 1;
         break;
      default:
         SUMA_RETURNe;
   }
   
   /* which cell is calling? */
   n = 0;
   Found = -1;
   while (n<TF->Nj*TF->Ni && Found == -1) {
      if (TF->cells[n] == w) {
         Found = n;
      } else ++n;
   }
   
   if (Found <0) {
      SUMA_SL_Err("Widget not found ????");
      SUMA_RETURNe;
   }
   
   /* find out widget's place in table*/
   i = Found % TF->Ni; j = Found / TF->Ni ;
   n = Found; 
   
   /* ado from above is not the mask we are to work with */
   if (!(ado = SUMA_whichADOg(TF->rowobject_id[i]))) {
      SUMA_S_Err("Failed to find mask object from row %d!", i);
      SUMA_RETURNe;
   }
   if (ado->do_type != MASK_type) {
      SUMA_S_Err("Need masks only here");
      SUMA_RETURNe;
   }
   mdo = (SUMA_MaskDO *)ado;
   
   switch (j) {
      case 0:
         break;
      case 1:
         SUMA_LH("Need to set label for mask %s", ADO_LABEL(ado));
         break;
      case 2:
         SUMA_LH("Need to set type for mask %s", ADO_LABEL(ado));
         break;
      case 3:
         SUMA_LH("Need to set center for mask %s", ADO_LABEL(ado));
         break;
      case 4:
         if (incr) {
            fv[0] = mdo->hdim[0]+(incr*0.2*mdo->init_hdim[0]); 
            fv[1] = mdo->hdim[1]+(incr*0.2*mdo->init_hdim[1]); 
            fv[2] = mdo->hdim[2]+(incr*0.2*mdo->init_hdim[2]);
            SUMA_MDO_New_Dim(mdo, fv);
         }
         SUMA_InitMasksTable_row(SurfCont,mdo, i);
         /* redisplay */
         if (!list) list = SUMA_CreateList ();
         SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                            SES_Suma, NULL); 
         if (!SUMA_Engine(&list)) SUMA_SLP_Err("Failed to redisplay.");
         break;
      case 5:
         SUMA_LH("Need to set color for mask %s", ADO_LABEL(ado));
         break;
      default:
         SUMA_SL_Err("Did not know you had so many");
         break;
   }
   
   SUMA_RETURNe;
}

/*!
   Called when user clicks on table title 
   Expects SUMA_SRV_DATA in TF->NewValueCallbackData
*/
void SUMA_MaskTableLabel_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   static char FuncName[]={"SUMA_MaskTableLabel_EV"};
   Dimension lw ;
   Widget * children , wl = NULL;
   XButtonEvent * bev = (XButtonEvent *) ev ;
   int  num_children , i, j, Found, AutoHist;
   DList *list=NULL;
   SUMA_TABLE_FIELD *TF = (SUMA_TABLE_FIELD *)cd;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   /* see note in bbox.c optmenu_EV for the condition below*/
   if( bev->button == Button2 ){
     XUngrabPointer( bev->display , CurrentTime ) ;
     SUMA_RETURNe ;
   }
   
   if( w == NULL || TF == NULL ) SUMA_RETURNe ;

   switch (bev->button) {
      case Button1:
         SUMA_LH("Button 1");
         break;
      case Button2:
         SUMA_LH("Button 2");
         break;
      case Button3:
         SUMA_LH("Button 3");
         break;
      default:
         SUMA_RETURNe;
   }
   
   /* which column title (i == 0) widget is calling ? */
   /* check the first column */
   i = 0; j = 0;
   Found = 0;
   while (j<TF->Nj && !Found) {
      if (TF->cells[j*TF->Ni+i] == w) {
         Found = 1;
      } else ++j;
   }
   
   if (!Found) { /* maybe it is a row title */
      i = 0; j = 0;
      Found = 0;
      while (i<TF->Ni && !Found) {
         if (TF->cells[j*TF->Ni+i] == w) {
            Found = 1;
         } else ++i;
      }
   }
   
   if (Found >= 0) {
      SUMA_LHv("Click on cell [%d %d]\n", i, j);
   } else {
      SUMA_SL_Err("CEll not found!");
      SUMA_RETURNe;
   }
   
   SurfCont = SUMAg_CF->X->AllMaskCont;
   
   if (!SurfCont) {
      SUMA_SL_Err("No  SurfCont !");
      SUMA_RETURNe;
   }
   
   /* Now do something */
   if (j == 0) { /* clicked on one of the row's titles */
      switch (i) {
         case 0:
            if (bev->button == Button1) {
               /* Add a new mask and update the table */
               if (SUMA_NewSymMaskDO()<0) {
                  SUMA_S_Err("Failed create new mask");
                  SUMA_RETURNe;
               } 

               if (!SUMA_InitMasksTable(SurfCont)) {
                  SUMA_S_Err("Failed to add row");
                  SUMA_RETURNe;
               }
               /* redisplay */
               if (!list) list = SUMA_CreateList ();
               SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                                  SES_Suma, NULL); 
               if (!SUMA_Engine(&list)) SUMA_SLP_Err("Failed to redisplay.");
            }
            break;
         default:
            if (bev->button == Button1) { /* delete? */
               SUMA_cb_Mask_Delete(w, (XtPointer)TF->rowobject_id[i], NULL);
            }else if (bev->button == Button3) { /* reset to autorange values */
               
            }
            break;
      }
   }
   if (i == 0) { /* clicked on one of the column's titles */
      switch (j) {
         case 1:
            break;
         case 2:
            break;
         case 3:
            break;
         default:
            break;
      }
   }
   
   SUMA_RETURNe;
}

void SUMA_cb_SetMaskTypeTableValue (void *data) 
{
   static char FuncName[]={"SUMA_cb_SetMaskTypeTableValue"};
   SUMA_ALL_DO *ado=NULL;
   int n=-1,row=-1,col=-1, an=0;
   void *cv=NULL; 
   SUMA_TABLE_FIELD *TF=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SurfCont = SUMAg_CF->X->AllMaskCont;
   
   TF = SurfCont->MaskTable;
   if (LocalHead) {
      fprintf(SUMA_STDERR, 
         "%s:\n Entered mask table cell, cell modified %d \n", 
         FuncName, TF->cell_modified);
   }

   if (TF->cell_modified<0) SUMA_RETURNe;
   n = TF->cell_modified;
   row = n % TF->Ni;
   col = n / TF->Ni;
   XtVaGetValues(TF->cells[n], XmNvalue, &cv, NULL);
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%s, %s\n", 
                           FuncName, row, col, (char *)cv, TF->str_value[n]);
   }

   an = SUMA_SetMaskTableValueNew(row, col,
                          (char *)cv,
                          0, 1, TF->num_units);
   if (an < 0) {
      SUMA_BEEP; 
   }
   
   SUMA_RETURNe;
}

/*!
   \brief modify an existing table widget 
*/
SUMA_Boolean SUMA_ModifyTable(SUMA_TABLE_FIELD *TF, int Nrows)
{
   static char FuncName[]={"SUMA_ModifyTable"};
   char *row_tit_buf=NULL, *col_tit_buf=NULL;
   char **row_tit=NULL,  **col_tit=NULL,  /* These should be passed if you */
        **row_hint=NULL, **row_help=NULL,/* desire them.  */
        **col_hint=NULL, **col_help=NULL,
        **sv=NULL;
   int i, j, n, titw, xmw, shad;
   int init_modified_row=-1, init_modified_col=-1;
   float *nv=NULL;
   byte *bf=NULL;
   Widget rcc, *wn=NULL;
   XtPointer cd;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!TF) {
      SUMA_S_Err("NULL TF");
      SUMA_RETURN(NOPE);
   }
   
   if (Nrows < 0) {
      SUMA_S_Err("Negative Nrows (%d)", Nrows);
      SUMA_RETURN(NOPE);
   }
   
   if (Nrows == TF->Ni) {
      SUMA_LH("Nrows (%d) == Number of rows in table. Nothing to do.", TF->Ni);
      SUMA_RETURN(NOPE);
   }

   if (TF->cell_modified >= 0) { 
      init_modified_row = TF->cell_modified % TF->Ni;
      init_modified_col = TF->cell_modified / TF->Ni;
      TF->cell_modified = -1; /* Will be useless soon */
   } else {
      init_modified_row = -1; init_modified_col = -1;
   }
   
   if (TF->Ni > Nrows) {
      SUMA_LH("Removing %d rows", TF->Ni-Nrows);
      SUMA_LH("First get rid of the widgets");
      
      i = TF->Ni-1; j = 0;
      while (i >= Nrows && i >= 0) {
         n = j * TF->Ni + i;
         rcc = XtParent(TF->cells[n]);
         XtDestroyWidget(rcc);
         SUMA_ifree(TF->str_value[n]);
         --i;
      }
      SUMA_LH("And now to reallocate..");
      /* new allocation, can't just realloc, storage is column major! */
      wn = (Widget *)SUMA_calloc(Nrows*TF->Nj, sizeof(Widget));
      bf = (byte *)SUMA_calloc(Nrows*TF->Nj, sizeof(byte));
      if (!bf || !wn) {  SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NOPE); }
      /* copy old pointers */
      for (i=0; i<Nrows; ++i) { for (j=0; j<TF->Nj; ++j) {
         wn[j*Nrows+i] = TF->cells[j*TF->Ni+i];
         bf[j*Nrows+i] = TF->but_flag[j*TF->Ni+i];
      } }
      SUMA_free(TF->cells); TF->cells = wn; wn = NULL;
      SUMA_free(TF->but_flag); TF->but_flag = bf; bf = NULL;
      TF->rowobject_id = (char **)SUMA_realloc(TF->rowobject_id,
                                                      Nrows*sizeof(char *));
      switch (TF->type) {
         case SUMA_int:
         case SUMA_float:
            nv = (float *)SUMA_calloc(TF->Nj*Nrows, sizeof(float));
            for (i=0; i<Nrows; ++i) { for (j=0; j<TF->Nj; ++j) {
               nv[j*Nrows+i] = TF->num_value[j*TF->Ni+i];
            } }
            SUMA_free(TF->num_value); TF->num_value = nv; nv = NULL;
            break;
         case SUMA_string:
            sv = (char **)SUMA_calloc(TF->Nj*Nrows, sizeof(char *));
            for (i=0; i<Nrows; ++i) { for (j=0; j<TF->Nj; ++j) {
               sv[j*Nrows+i] = TF->str_value[j*TF->Ni+i];
            } }
            SUMA_free(TF->str_value); TF->str_value = sv; sv = NULL;
            break;
         default:
            SUMA_SL_Err("Comme tu es bete!");
            SUMA_RETURN(NOPE);
            break;  
      }

      TF->Ni = Nrows;
   } else if (TF->Ni < Nrows){
      SUMA_LH("Adding %d rows", Nrows-TF->Ni);
      SUMA_LH("First reallocating");
      /* new allocation, can't just realloc, storage is column major! */
      wn = (Widget *)SUMA_calloc(Nrows*TF->Nj, sizeof(Widget));
      bf = (byte *)SUMA_calloc(Nrows*TF->Nj, sizeof(byte));
      if (!bf || !wn) {  SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NOPE); }
      /* copy old pointers */
      for (i=0; i<TF->Ni; ++i) { for (j=0; j<TF->Nj; ++j) {
         wn[j*Nrows+i] = TF->cells[j*TF->Ni+i];
         bf[j*Nrows+i] = TF->but_flag[j*TF->Ni+i];
      } }
      SUMA_free(TF->cells); TF->cells = wn; wn = NULL;
      SUMA_free(TF->but_flag); TF->but_flag = bf; bf = NULL;
      TF->rowobject_id = (char **)SUMA_realloc(TF->rowobject_id,
                                                   Nrows*sizeof(char *));
      for (i=TF->Ni; i<Nrows; i++) TF->rowobject_id[i] = NULL;           
      switch (TF->type) {
         case SUMA_int:
         case SUMA_float:
            nv = (float *)SUMA_calloc(TF->Nj*Nrows, sizeof(float));
            for (i=0; i<TF->Ni; ++i) { for (j=0; j<TF->Nj; ++j) {
               nv[j*Nrows+i] = TF->num_value[j*TF->Ni+i];
            } }
            SUMA_free(TF->num_value); TF->num_value = nv; nv = NULL;
            break;
         case SUMA_string:
            sv = (char **)SUMA_calloc(TF->Nj*Nrows, sizeof(char *));
            for (i=0; i<TF->Ni; ++i) { for (j=0; j<TF->Nj; ++j) {
               sv[j*Nrows+i] = TF->str_value[j*TF->Ni+i];
            } }
            SUMA_free(TF->str_value); TF->str_value = sv; sv = NULL;
            break;
         default:
            SUMA_SL_Err("Comme tu es bete!");
            SUMA_RETURN(NOPE);
            break;  
      }
      /* Now adding rows */
      i = TF->Ni;
      TF->Ni = Nrows;
      while (i != TF->Ni) {
         rcc = XtVaCreateManagedWidget ("rowcolumn",
         xmRowColumnWidgetClass, TF->rco,
         XmNorientation , XmHORIZONTAL ,
         XmNmarginHeight, 0,
         XmNmarginHeight, 0,
         XmNmarginWidth, 0, 
         NULL);
      
         if (i == 0 && TF->HasColTit) { 
            /* See comment in comparable section of SUMA_CreateTable() */
            XtVaSetValues (rcc, XmNpacking, XmPACK_TIGHT, NULL); 
         } else {
            XtVaSetValues (rcc, XmNpacking, XmPACK_TIGHT, NULL); 
         }

         for (j=0; j<TF->Nj; ++j) { /* for each column */
            n = j * TF->Ni + i;
            switch (SUMA_cellvariety(TF, n)) {
               case SUMA_ROW_TIT_CELL: /* row's title */
                  if (!row_tit || !row_tit[i]) {
                     SUMA_LH("No row_tit assuming default");
                     row_tit_buf = "x";
                  } else {
                     row_tit_buf = row_tit[i];
                  }
                  if (LocalHead) 
                     fprintf( SUMA_STDERR,
                              "%s:\nAdding RT [%d %d] (%d) %s\n", 
                              FuncName, i, j, n, row_tit_buf );
                  TF->cells[n] = 
                     XtVaCreateManagedWidget("table",   
                           xmTextFieldWidgetClass, rcc,
                           XmNvalue, row_tit_buf,
                           XmNmarginHeight, 0,
                           XmNmarginWidth, 0,
                           XmNmarginTop, 0,
                           XmNmarginBottom, 0,
                           XmNmarginLeft, 0,
                           XmNmarginRight, 0,
                           XmNeditable, False, 
                           XmNshadowThickness , 0,    
                           XmNcursorPositionVisible, False,
                           XmNcolumns, strlen(row_tit_buf), 
                           NULL);
                  XtVaSetValues( TF->cells[n], 
                                 XmNfontList, 
                                 SUMAg_CF->X->TableTextFontList, NULL);

                  if (!TF->TitLabelEVHandlerData) 
                     cd = (XtPointer) TF; 
                  else cd = (XtPointer)TF->TitLabelEVHandlerData;
                  if (TF->TitLabelEVHandler) {
                     /* insert handler to catch clicks on titles */
                     XtInsertEventHandler( 
                        TF->cells[n] ,      /* handle events in title cell */
                        ButtonPressMask ,  /* button presses */
                        FALSE ,            /* nonmaskable events? */
                        TF->TitLabelEVHandler,  /* handler */
                        cd ,   /* client data */
                        XtListTail ) ; 
                  }
                  if (row_hint)  MCW_register_hint( TF->cells[n], row_hint[i] );
                  if (row_help)  MCW_register_help( TF->cells[n], row_help[i] ) ;
                  break;

               case SUMA_COL_TIT_CELL: /* column's title */
                  if (!col_tit || !col_tit[i]) {
                     SUMA_S_Err("No col_tit assuming default");
                     col_tit_buf = "x";
                  } else {
                     col_tit_buf = col_tit[i];
                  }
                  if (LocalHead) 
                     fprintf( SUMA_STDERR,
                              "%s:\nAdding CT [%d %d] (%d) %s\n", 
                              FuncName, i, j, n, col_tit_buf);
                  /* padd to fit cell entry fields*/
                  if (i == 0 && j != 0 && TF->HasColTit) { 
                     titw = TF->cwidth[j]; 
                     /* set the margins to meet those of cell entries */
                     xmw = 5;
                     shad = 1;
                  } else {
                     titw = TF->cwidth[j];
                     /* set the margins to meet those of Labels */
                     xmw = 0;
                     shad = 0;
                  }
                   
                  TF->cells[n] = 
                     XtVaCreateManagedWidget("table",   
                           xmTextFieldWidgetClass, rcc,
                           XmNvalue, col_tit_buf,
                           XmNmarginHeight, 0,
                           XmNmarginWidth, xmw+shad,/*include shadow size 
                                                     of text entry cells*/
                           XmNmarginTop, 0,
                           XmNmarginBottom, 0,
                           XmNmarginLeft, 0,
                           XmNmarginRight, 0,
                           XmNeditable, False, 
                           XmNshadowThickness , 0,       /* hide the border */
                           XmNcursorPositionVisible, False, /* hide the cursor */
                              /* Keep these two out: See SUMA_CreateTable() 
                                 for comments ...
                              XmNfontList, SUMAg_CF->X->TableTextFontList,
                              XmNcolumns, titw,
                              */ 
                           NULL);

                  XtVaSetValues( TF->cells[n], 
                                 XmNfontList, 
                                 SUMAg_CF->X->TableTextFontList, NULL);
                  XtVaSetValues( TF->cells[n], XmNcolumns, titw, 
                           NULL);

                  if (i == 0 && j != 0) { 
                     XtVaSetValues( TF->cells[n], 
                                    XmNalignment, XmALIGNMENT_BEGINNING, NULL);
                  }

                  /* insert handler to catch clicks on titles */
                  if (!TF->TitLabelEVHandlerData) 
                     cd = (XtPointer) TF; 
                  else cd = (XtPointer)TF->TitLabelEVHandlerData;
                  if (TF->TitLabelEVHandler) {
                     /* insert handler to catch clicks on titles */
                     XtInsertEventHandler( 
                        TF->cells[n] ,      /* handle events in title cell */
                        ButtonPressMask ,  /* button presses */
                        FALSE ,            /* nonmaskable events? */
                        TF->TitLabelEVHandler,  /* handler */
                        cd ,   /* client data */
                        XtListTail ) ; 
                  }                 
                  if (col_hint)  MCW_register_hint( TF->cells[n], col_hint[j] );
                  if (col_help)  MCW_register_help( TF->cells[n], col_help[j] ) ;
                  break;
               case SUMA_ENTRY_CELL: /* entry cell */
                  if (LocalHead) 
                     fprintf( SUMA_STDERR,
                              "%s:\nAdding [%d %d] (%d) entry cell\n", 
                              FuncName, i, j, n);
                  TF->cells[n] = XtVaCreateManagedWidget(
                                 "entry",  
                                 xmTextFieldWidgetClass, rcc,
                                 XmNuserData, (XTP_CAST)n,
                                 XmNvalue, "-",
                                 XmNmarginHeight, 0,
                                 XmNmarginTop, 0,
                                 XmNmarginBottom, 0,
                                 XmNmarginWidth, 5, 
                                 NULL);
                  XtVaSetValues( TF->cells[n], 
                                    XmNfontList, 
                                    SUMAg_CF->X->TableTextFontList, NULL);
                  if (col_help || col_hint || row_help || row_hint)  {
                     if (!row_tit && !col_tit && TF->Ni == 1 && TF->Nj == 1) {
                        if (col_help) 
                           MCW_register_help( TF->cells[n], col_help[0]) ;
                        else if (row_help) 
                           MCW_register_help( TF->cells[n], row_help[0]) ;
                        if (col_hint) 
                           MCW_register_hint( TF->cells[n], col_hint[0]) ;
                        else if (row_hint) 
                           MCW_register_hint( TF->cells[n], row_hint[0]) ;
                     } else {
                        MCW_register_help( TF->cells[n], 
                           "Hints and help messages "
                           "are attached to table's "
                           "column and row titles.") ;
                        MCW_register_hint( TF->cells[n], 
                                    "Hints and help messages "
                                    "are attached to table's "
                                    "column and row titles.") ;
                     }
                  } 
                  if (TF->cwidth[j] > 0) {  
                     XtVaSetValues(TF->cells[n], XmNcolumns, 
                                   TF->cwidth[j], NULL); 
                  }
                  if (!TF->editable) { 
                     SUMA_SetCellEditMode(TF, i, j, 0);
                  } else {
                     SUMA_SetCellEditMode(TF, i, j, 1);
                  }

                  /* insert handlers if any */
                  if (!TF->CellEVHandlerData) cd = (XtPointer) TF; 
                     else cd = (XtPointer)TF->CellEVHandlerData;
                  if (TF->CellEVHandler) {
                     /* insert handler to catch clicks on cells */
                     XtInsertEventHandler( 
                                 TF->cells[n] ,      /* handle events in cell */
                                 ButtonPressMask ,  /* button presses */
                                 FALSE ,            /* nonmaskable events? */
                                 TF->CellEVHandler,  /* handler */
                                 cd ,   /* client data */
                                 XtListTail ) ; 
                  }                 
                  break;
               default:
                  SUMA_SL_Err("Bad cell type");
                  SUMA_RETURN(NOPE);
                  break;
            }     
         }  
         ++i;
      }/* until we have enough rows */
      
   }
   
   /* Reset the cell's user data, some of the Set calls may be redundant.*/
   for (i=0; i<TF->Ni; ++i) {
      for (j=0; j<TF->Nj; ++j) {
         n  = j * TF->Ni + i;
         switch (SUMA_cellvariety(TF, n)) {
            case SUMA_ENTRY_CELL:
               XtVaSetValues(TF->cells[n], XmNuserData, (XTP_CAST)n, NULL);
               break;
         }
      }
   }

   /* Reset cell_modified */
   TF->cell_modified = 
         SUMA_CELL_ROW_COL_2_1D(TF, init_modified_row, init_modified_col);
         
   SUMA_RETURN(YUP);
}


DList *SUMA_AssembleMasksList(int withShadow)
{
   return(SUMA_AssembleMasksList_inDOv(NULL, -1, withShadow));
}

DList *SUMA_AssembleMasksList_inDOv(SUMA_DO *dov, int N_dov, int withShadow)
{
   static char FuncName[]={"SUMA_AssembleMasksList_inDOv"};
   SUMA_MaskDO *MDO;
   int i;
   DList *dl=NULL;
   
   SUMA_ENTRY;
   
   if (!dov) {
      dov = SUMAg_DOv; N_dov = SUMAg_N_DOv;
   }
   for (i=0; i<N_dov; ++i) {
      if (dov[i].ObjectType == MASK_type) {
         if (!dl) {
            dl = (DList *)SUMA_calloc(1,sizeof(DList));
            dlist_init(dl, NULL); 
         }
         MDO = (SUMA_MaskDO *)dov[i].OP;
         if (!MDO_IS_SHADOW(MDO)) {
            dlist_ins_next(dl, dlist_tail(dl), (void *)MDO);
         } else {
            if (withShadow) { /* Shadow will always be 1st */
               dlist_ins_prev(dl, dlist_head(dl), (void *)MDO);
            }
         }        
      }
   }
   
   SUMA_RETURN(dl);
}

SUMA_Boolean  SUMA_InitMasksTable_row(SUMA_X_SurfCont *SurfCont, 
                                      SUMA_MaskDO *mdo, int row)
{
   static char FuncName[]={"SUMA_InitMasksTable_row"};
   char str[256];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SurfCont || !SurfCont->MaskTable || !mdo) SUMA_RETURN(NOPE);
   if (row < 0 || row > SurfCont->MaskTable->Ni-1) {
      SUMA_S_Err("Bad row index of %d", row);
      SUMA_RETURN(NOPE);
   }
   SUMA_LH("MDO %s, row %d/%d, (%p)", 
            ADO_LABEL((SUMA_ALL_DO *)mdo), row, 
            SurfCont->MaskTable->Ni,
            SurfCont->MaskTable->rowobject_id);
   
   /* First set object id for entire row. Should come in handy */
   SUMA_STRING_REPLACE(SurfCont->MaskTable->rowobject_id[row],
                       ADO_ID((SUMA_ALL_DO *)mdo));
   if (row == 0) {
      /* that's all folks */
      SUMA_RETURN(YUP);
   }
   
   /* get rid of initial delete press */
   SUMA_INSERT_CELL_STRING(SurfCont->MaskTable,row, 0, "x");
   
   SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 1, 
                           ADO_LABEL((SUMA_ALL_DO *)mdo));

   if      (MDO_IS_SPH(mdo)) {
      SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 2, "sphere");
   } else if (MDO_IS_BOX(mdo)) {
      SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 2, "box");
   } else {
      SUMA_S_Err("Not ready for type %s, not here at least", mdo->mtype);
   }
   
   SUMA_RGBA_to_string(mdo->cen, 3, 1.0, str, NULL, ",",3);
   SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 3,  str);
   
   SUMA_RGBA_to_string(mdo->hdim, 3, 1.0, str, NULL, ",",3);
   SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 4,  str);

   SUMA_RGBA_to_string(mdo->colv, 4, 1.0, str, NULL, ",",-1);
   SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 5,  str);
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean  SUMA_InitMasksTable(SUMA_X_SurfCont *SurfCont)
{
   static char FuncName[]={"SUMA_InitMasksTable"};
   DList *dl=NULL;
   DListElmt *el=NULL;
   int cnt;
   SUMA_MaskDO *mdo;
   
   SUMA_ENTRY;
   
   if (!SurfCont || !SurfCont->MaskTable) SUMA_RETURN(NOPE);
   
   dl = SUMA_AssembleMasksList_inDOv(SUMAg_DOv, SUMAg_N_DOv, 0);
   if (!dl || dlist_size(dl) == 0) {
      SUMA_ModifyTable(SurfCont->MaskTable, 1);
      SUMA_RETURN(YUP);
   } else {
      SUMA_ModifyTable(SurfCont->MaskTable, dlist_size(dl)+1);
   }

   if (SUMA_ShadowMaskDO(&mdo)>=0) { /* add the shadow to row 0 */
      if (!SUMA_InitMasksTable_row(SurfCont, mdo, 0)) {
         SUMA_S_Err("Failed to init row 0");
         SUMA_RETURN(NOPE);
      }
   }
     
   cnt = 0; el = NULL;
   do {
      if (!el) el = dlist_head(dl);
      else el = dlist_next(el);
      mdo = (SUMA_MaskDO *)el->data;
      if (!SUMA_InitMasksTable_row(SurfCont, mdo, cnt+1)) {
         SUMA_S_Err("Failed to init row %d", cnt+1);
         SUMA_RETURN(NOPE);
      }
      ++cnt;
   } while (el != dlist_tail(dl));
   
   SUMA_RETURN(YUP);
}

int SUMA_NewSymMaskDO(void) 
{
   static char FuncName[]={"SUMA_NewSymMaskDO"};
   SUMA_MaskDO *mdo=NULL;
   int ido;
   static int icall=0;
   char mtype[32], hid[32];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ido = -1;
   sprintf(hid,"msk%d", icall); 
   mdo = SUMA_SymMaskDO("box(0, 0, 0; 20, 20, 20)", mtype, hid, 0);
   switch(icall) {
      case 0:{
         float cc[4] = {1, 1, 1, 1};
         SUMA_Set_MaskDO_Color(mdo, cc);
         break; }
      case 1:{
         float cc[4] = {1, 0, 0, 1};
         SUMA_Set_MaskDO_Color(mdo, cc);
         break; }
      case 2:{
         float cc[4] = {0, 1, 0, 1};
         SUMA_Set_MaskDO_Color(mdo, cc);
         break; }
      case 3:{
         float cc[4] = {0, 0, 1, 1};
         SUMA_Set_MaskDO_Color(mdo, cc);
         break; }
      case 4: {
         float cc[4] = {1, 1, 0, 1};
         SUMA_Set_MaskDO_Color(mdo, cc);
         break; }
      case 5: {
         float cc[4] = {0, 1, 1, 1};
         SUMA_Set_MaskDO_Color(mdo, cc);
         break; }
      case 6: {
         float cc[4] = {1, 0, 1, 1};
         SUMA_Set_MaskDO_Color(mdo, cc);
         break; }
      default: {
         float cc[4] = {1, 0, 1, 1};
         SUMA_a_good_col("ROI_i256", icall-6,cc);
         SUMA_Set_MaskDO_Color(mdo, cc);
         break; }
   }
   if (!SUMA_AccessorizeMDO(mdo)) {
      SUMA_S_Err("No accessorizing");
      SUMA_RETURN(ido);
   }

   /* addDO */
   SUMA_LH("Adding DO");
   if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, 
                   (void *)mdo, MASK_type, SUMA_WORLD)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
      SUMA_RETURN(ido);
   }
   ido = SUMAg_N_DOv-1;
      
   /* register DO with viewer */
   SUMA_LH("Registrar");
   if (!SUMA_RegisterDO(ido, NULL)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
      SUMA_RETURN(-1);
   }
   ++icall;
   SUMA_RETURN(ido);
}

int SUMA_ShadowMaskDO(SUMA_MaskDO **mdop) 
{
   static char FuncName[]={"SUMA_ShadowMaskDO"};
   SUMA_MaskDO *mdo=NULL;
   int ido;
   char mtype[32], hid[32];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (mdop) *mdop = NULL;
   
   /* If it exists, return it */
   ido = -1;
   SUMA_findShadowMDOp_inDOv(SUMAg_DOv, SUMAg_N_DOv, &ido);
   if (ido >=0) {
      if (mdop) *mdop = (SUMA_MaskDO *)iDO_ADO(ido);
      SUMA_RETURN(ido);
   }
   /* Otherwise create it */
   sprintf(hid,"TheShadow"); 

   /* Now create the mask */
   SUMA_LH("Creating mask");
   if (!(mdo = SUMA_Alloc_MaskDO (1, hid, hid, NULL, 1))) {
      SUMA_S_Err("Failed in SUMA_Allocate_MaskDO.");
      SUMA_RETURN(-1);
   }
   strcpy(mdo->mtype, "CASPER");
   
   if (!SUMA_AddMaskSaux(mdo)) {
      SUMA_S_Err("Failed to add Mask Saux");
      SUMA_RETURN(-1);
   }

   /* addDO */
   SUMA_LH("Adding DO");
   if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, 
                   (void *)mdo, MASK_type, SUMA_WORLD)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
      SUMA_RETURN(-1);
   }
   ido = SUMAg_N_DOv-1;
      
   /* register DO with viewer */
   SUMA_LH("Registrar");
   if (!SUMA_RegisterDO(ido, NULL)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
      SUMA_RETURN(-1);
   }
   
   if (mdop) *mdop = (SUMA_MaskDO *)iDO_ADO(ido);

   SUMA_RETURN(ido);
}

/*! 
   \brief Switches to the controller of Masks.
   Creates a new Mask if none exists
*/
void SUMA_cb_Mask (Widget w, XtPointer client_data, XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_Mask"};
   SUMA_ALL_DO *ado=NULL;
   SUMA_MaskDO *mdo=NULL;
   SUMA_TractDO *tdo=NULL;
   SUMA_TRACT_SAUX *TSaux=NULL;
   int ido;
   void *n=NULL;
   char *s = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   ado = (SUMA_ALL_DO *)client_data;
   if (ado->do_type != TRACT_type) {
      SUMA_S_Err("Expect tracts only here");
      SUMA_RETURNe;
   }
   tdo = (SUMA_TractDO *)ado;
   SurfCont = SUMA_ADO_Cont(ado);
   TSaux = TDO_TSAUX(tdo);
   
   /* How many masks do we have so far? */
   if (!SUMA_findanyMDOp_inDOv(SUMAg_DOv, SUMAg_N_DOv, &ido)) {
      /* Create a dummy uber owner that will never get displayed
         but can always be relied upon to exit. Other masks can 
         get deleted, leaving the mask controller ownerless */
      SUMA_ShadowMaskDO(NULL);   
      /* Now Need to create a new Mask for real*/
      SUMA_LH("Need a new mask");
      if ((ido = SUMA_NewSymMaskDO())<0) {
         SUMA_S_Err("Failed to create SymMask");
         SUMA_RETURNe;
      }
      mdo = (SUMA_MaskDO *)iDO_ADO(ido);
      SUMA_LH("Outa here");
      XtSetSensitive(SurfCont->TractMaskMenu->mw[SW_SurfCont_TractMask], 1);
      XtSetSensitive(SurfCont->TractMaskGray->rc, 1);
      
      /* Redisplay related */
      SUMA_RedisplayAllShowing(iDO_idcode(ido), NULL, 0);
      SUMA_RETURNe;
   } else {
      SUMA_LH("Have ido %d %s", ido, iDO_label(ido));
      mdo = (SUMA_MaskDO *)iDO_ADO(ido);
   }
   
   /* Have something to work with, switch to the controller of the Mask */
   SurfCont = SUMA_ADO_Cont((SUMA_ALL_DO *)mdo);
   if (!SUMA_viewSurfaceCont(NULL, (SUMA_ALL_DO *)mdo, 
                             SUMA_BestViewerForADO(ado))) {
      SUMA_S_Err("Failed to view surface cont");
      SUMA_RETURNe;
   }
   if (!SUMA_InitMasksTable(SurfCont)) {
      SUMA_S_Err("Failed to initialize table");
      SUMA_RETURNe;
   }
   SUMA_RETURNe;
}



void SUMA_cb_Mask_Delete(Widget wcall, XtPointer cd1, XtPointer cbs)
{
   static char *PlaneName=NULL, FuncName[] = {"SUMA_cb_Mask_Delete"};
   XmPushButtonCallbackStruct * pbcbs = (XmPushButtonCallbackStruct *) cbs ;
   static int ErrCnt =0;
   DList *list=NULL;
   SUMA_MaskDO *mdo=NULL;
   SUMA_ALL_DO *ado=NULL, *curDO=NULL;
   int found=0, ii, rownum=-1;
   char *ado_id = NULL;
   SUMA_X_SurfCont *SurfCont = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called with: %p %s %p", wcall, ado_id?ado_id:"NULL", cbs);
   
   SurfCont = SUMAg_CF->X->AllMaskCont;
   if (!SurfCont || !SurfCont->MaskTable) {
      SUMA_S_Err("Should not happen for mask interface");
      SUMA_RETURNe;
   }
   
   if (wcall) { /* This is a call from the delete 'x' button, 
                   call from callback does not have widget set */
      if (SurfCont->MaskTable->Ni < 1) {
         if (!ErrCnt) SUMA_SLP_Note ("No mask to delete");
         ErrCnt ++;
         SUMA_RETURNe;
      }
      ado_id = (char *)cd1; 
   } else {/* Call from callback, reset the button to lowercase */
      rownum = (int)((long)cd1);
      SUMA_LH("Resetting button for row %d.\n", rownum); 
      if (rownum == SurfCont->DeleteMask_row) {
         /* reset the x and clear up selection */
         SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, 
                           SUMA_RowTitCell(SurfCont->MaskTable,rownum), 0, "x");
         SurfCont->DeleteMask_first = YUP ;
         SurfCont->DeleteMask_row = -1;
         SUMA_RETURNe ;
      } else if (SurfCont->DeleteMask_row = -1) { 
                     /* row must have been deleted, nothing to be done */
         SUMA_RETURNe ;
      } else { /* row index and delete row don't match, reset all */
         for (ii=1; ii<SurfCont->MaskTable->Ni; ++ii) {
            SUMA_INSERT_CELL_STRING(SurfCont->MaskTable,ii, 0, "x");
         }
         SurfCont->DeleteMask_first = YUP ;
         SurfCont->DeleteMask_row = -1;
         SUMA_RETURNe ;
      }
      SUMA_RETURNe ;
   }
   
   if (!ado_id) {
      SUMA_S_Err("Should not be without ado_id at this stage");
   }
   
   if( SurfCont->DeleteMask_first ){/* First press -->  change button label */
      if (SurfCont->DeleteMask_row != -1) {
         SUMA_S_Warn("DeleteMask_row not initialized?");
      }
      if ((found = SUMA_ObjectID_Row(SurfCont->MaskTable, ado_id))< 0) {
         SUMA_S_Err("Failed to find object");
         SUMA_RETURNe ;
      }
      SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, 
                           SUMA_RowTitCell(SurfCont->MaskTable,found), 0, "X" ) ;
      SurfCont->DeleteMask_first = NOPE ;
      SurfCont->DeleteMask_row = found;
      
      /* if not re-pressed in 5 seconds, will reset to lowercase */
      SUMA_LH("First Press, adding time out.");
      (void) XtAppAddTimeOut(
               XtWidgetToApplicationContext(SurfCont->MaskTable->cells[0]) ,
               5000 , SUMA_delete_mask_timeout_CB , (XtPointer)((long)found) ) ;

       SUMA_RETURNe;
   } else { /* Second press --> delete row */
      if ((found = SUMA_ObjectID_Row(SurfCont->MaskTable, ado_id))< 0) {
         SUMA_S_Err("Failed to find object");
         SUMA_RETURNe ;
      }
      if (ado = SUMA_whichADOg(ado_id)) {
         SUMA_LH("ado_id is for %s", ADO_LABEL(ado));
      } else {
         SUMA_LH("ado_id has no object");
      }
      if (SurfCont->DeleteMask_row != found) {
         SUMA_LH("Clicking left and right?");
         for (ii=1; ii<SurfCont->MaskTable->Ni; ++ii) {
            SUMA_INSERT_CELL_STRING(SurfCont->MaskTable,ii, 0, "x");
         }
         SurfCont->DeleteMask_first = YUP ;
         SurfCont->DeleteMask_row = -1;
         SUMA_RETURNe;
      } else {  
         /* delete mask */
         ErrCnt = 0;
         SUMA_LHv("Should be deleting Masks here ...\n");
         if (SurfCont->MaskTable->Ni>1) {
            curDO = SUMA_SurfCont_GetcurDOp(SurfCont);
            if (curDO == ado) {
               /* Need to find another DO */
               for (ii=SurfCont->MaskTable->Ni-1; ii<=0; ++ii) {
                  if (SurfCont->MaskTable->rowobject_id[ii] &&
                      strcmp(SurfCont->MaskTable->rowobject_id[ii],ado_id)) {
                   curDO = SUMA_whichADOg(SurfCont->MaskTable->rowobject_id[ii]);
                     SUMA_SurfCont_SetcurDOp(SurfCont, curDO);
                     SUMA_LH("CurDO now %s", ADO_LABEL(curDO));
                  }
               }
            }
            /* unregister do from all viewers */
            SUMA_UnRegisterDO_idcode(ado_id,NULL);
      
            /* delelte the current mask from DOv */
            if (!SUMA_RemoveDO(SUMAg_DOv, &SUMAg_N_DOv, (void *)ado, 1)){
               SUMA_S_Err("Failed to dump DO");
               SUMA_RETURNe;
            }
            if (!SUMA_ModifyTable(SurfCont->MaskTable, 
                                  SurfCont->MaskTable->Ni-1)) {
               SUMA_S_Err("Failed to delete table row");
               SUMA_RETURNe;
            }
            SurfCont->DeleteMask_first = YUP ;
            SurfCont->DeleteMask_row = -1;
            /* ModifyTable, just deletes the bottom row of widgets. You need to 
               reinitialize */
            SUMA_InitMasksTable(SurfCont);
         }
      
         /* redisplay */
         if (!list) list = SUMA_CreateList ();
         SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                            SES_Suma, NULL); 
         if (!SUMA_Engine(&list)) SUMA_SLP_Err("Failed to redisplay.");
      }
   }
   
   SUMA_RETURNe;
}

void SUMA_delete_mask_timeout_CB( XtPointer client_data , XtIntervalId * id )
{
   static char FuncName[] = {"SUMA_delete_mask_timeout_CB"};

   SUMA_ENTRY;

   SUMA_cb_Mask_Delete(NULL, client_data, NULL);

   SUMA_RETURNe; 
}

int SUMA_SetMaskTableValueNew(  int row, int col,
                                char *s1, 
                                int setmen, 
                                int redisplay, 
                                SUMA_NUMERICAL_UNITS num_units) 
{
   static char FuncName[]={"SUMA_SetMaskTableValueNew"};
   int NewDisp=0, isCur=0, Err=0, init_row=0;
   SUMA_MaskDO *mdo=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   float *fv;
   SUMA_ALL_DO *ado=NULL;
   char str[256];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      SUMA_LH("Called on cell[%d %d] with %s", row, col, s1?s1:"NULL");
      SUMA_DUMP_TRACE("Who called SUMA_SetMaskTableValueNew?");
   }
   
   if (!(SurfCont=SUMAg_CF->X->AllMaskCont)) {
      SUMA_RETURN(0);
   }
   
      
   TF = SurfCont->MaskTable;
   if (!TF) setmen = 0; /* can't set nothing */
   
   if (num_units == SUMA_PERC_VALUE_UNITS) {
      SUMA_S_Err("No percentiles allowed here");
      SUMA_RETURN(NOPE);
   }
   
   if (row < 1) {
      SUMA_S_Err("What is for row %d < 1?", row); SUMA_RETURN(NOPE);
   }
   if (col < 1) {
      SUMA_S_Err("What is for col %d < 1?", col); SUMA_RETURN(NOPE);
   }
   NewDisp = NOPE;
   
   /* ado for that row */
   if (!(ado = SUMA_whichADOg(TF->rowobject_id[row]))) {
      SUMA_S_Err("Failed to find mask object from row %d!", row);
      SUMA_RETURN(NOPE);
   }
   if (ado->do_type != MASK_type) {
      SUMA_S_Err("Need MDO");
      SUMA_RETURN(NOPE);
   }
   mdo = (SUMA_MaskDO *)ado;
   /* What are we dealing with ? */
   switch (col) {
      case 1:  
         SUMA_LHv("Setting Label of %s to %s, isCur=%d [%d %d] \n", 
                   ADO_LABEL(ado), s1, isCur, row, col);
         if (SUMA_MDO_New_Label(mdo, s1)) {
            if (setmen) {
               SUMA_LHv("Inserting ado label back %s\n", 
                        ADO_LABEL(ado));
               SUMA_INSERT_CELL_STRING(TF, row, col, ADO_LABEL(ado));
            }
         } else { /* failed, reset string */
               SUMA_LHv("Resetting ado label back %s\n", 
                        ADO_LABEL(ado));
               SUMA_INSERT_CELL_STRING(TF, row, col, ADO_LABEL(ado));
         }
         break;
      case 2:
         SUMA_LHv("Setting Type of %s to %s, isCur=%d [%d %d] \n", 
                   mdo->mtype, s1, isCur, row, col);
         if (SUMA_Ok_Sym_MaskDO_Type(s1)) {
            SUMA_MDO_New_Type(mdo, s1);
            if (setmen) {
               SUMA_LHv("Inserting ado type back %s\n", 
                        mdo->mtype);
               SUMA_INSERT_CELL_STRING(TF, row, col, mdo->mtype);
            }
            init_row = 1; /* dims can also get changed by changing type, 
                           make sure table reflects this */
         } else { /* failed, reset string */
               SUMA_LHv("Resetting ado mtype back %s\n", 
                        mdo->mtype);
               SUMA_INSERT_CELL_STRING(TF, row, col, mdo->mtype);
         }
         break;
      case 3:
         SUMA_LHv("Setting Center of %f %f %f to %s, isCur=%d [%d %d] \n", 
                   mdo->cen[0], mdo->cen[1], mdo->cen[2], s1, isCur, row, col);
         fv = SUMA_string_to_RGBA(s1, NULL, 1.0, &Err);
         if (!Err) {
            SUMA_MDO_New_Cen(mdo, fv);
            if (setmen) {
               SUMA_RGBA_to_string(mdo->cen, 3, 1.0, str, NULL, ",",-1);
               SUMA_LHv("Inserting ado cen back %s\n", 
                        str);
               SUMA_INSERT_CELL_STRING(TF, row, col, str);
            }
         } else { /* failed, reset string */
            SUMA_RGBA_to_string(mdo->cen, 3, 1.0, str, NULL, ",",-1);
            SUMA_LHv("Resetting ado center string back %s\n", 
                     str);
            SUMA_INSERT_CELL_STRING(TF, row, col, str);
         }
         break;
      case 4:
         SUMA_LHv("Setting Size of %f %f %f to %s, isCur=%d [%d %d] \n", 
                  mdo->hdim[0], mdo->hdim[1], mdo->hdim[2], s1, isCur, row, col);
         fv = SUMA_string_to_RGBA(s1, NULL, 1.0, &Err);
         if (!Err) {
            SUMA_MDO_New_Dim(mdo, fv);
            if (setmen) {
               SUMA_RGBA_to_string(mdo->hdim, 3, 1.0, str, NULL, ",",3);
               SUMA_LHv("Inserting ado dim back %s\n", 
                        str);
               SUMA_INSERT_CELL_STRING(TF, row, col, str);
            }
         } else { /* failed, reset string */
            SUMA_RGBA_to_string(mdo->hdim, 3, 1.0, str, NULL, ",",3);
            SUMA_LHv("Resetting ado center string back %s\n", 
                     str);
            SUMA_INSERT_CELL_STRING(TF, row, col, str);
         }
         break;
      case 5:
         SUMA_LHv("Setting color of %f %f %f %f to %s, isCur=%d [%d %d] \n", 
                  mdo->colv[0], mdo->colv[1], mdo->colv[2], mdo->colv[3], 
                  s1, isCur, row, col);
         fv = SUMA_string_to_RGBA(s1, NULL, 1.0, &Err);
         if (!Err) {
            SUMA_MDO_New_Color(mdo, fv);
            if (setmen) {
               SUMA_RGBA_to_string(mdo->colv, 4, 1.0, str, NULL, ",",4);
               SUMA_LHv("Inserting ado col back %s\n", 
                        str);
               SUMA_INSERT_CELL_STRING(TF, row, col, str);
            }
         } else { /* failed, reset string */
            SUMA_RGBA_to_string(mdo->colv, 3, 1.0, str, NULL, ",",3);
            SUMA_LHv("Resetting ado colv string back %s\n", 
                     str);
            SUMA_INSERT_CELL_STRING(TF, row, col, str);
         }
         break;
      default:
         SUMA_SL_Err("You make me sick");
         break;
   }
   
   if (init_row) {
      SUMA_InitMasksTable_row(SurfCont, mdo, row);
   }
   /* Now, you need to redraw the deal */
   if (redisplay) {
      DList *list = NULL;
      /* redisplay */
      if (!list) list = SUMA_CreateList ();
      SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                         SES_Suma, NULL); 
      if (!SUMA_Engine(&list)) SUMA_SLP_Err("Failed to redisplay.");
   }   
      
   
   SUMA_RETURN(1);
}



/*!
   \brief create VrF selection widgets
*/
void SUMA_CreateVrFields(  Widget parent,
                        char *tit, char *hint, char *help, 
                        int Nslc, SUMA_ALL_DO *ado,
                        void (*NewValueCallback)(void * data), void *cb_data,
                        SUMA_VR_FIELD *VrF) 
{
   static char FuncName[]={"SUMA_CreateVrFields"};
   int i, j, n, titw, xmw, shad, mult;
   char *tmp, sbuf[12];
   XtPointer cd;
   XtVarArgsList arglist=NULL;
   XtCallbackProc slcb, sllbcb, slcactcb, shwslccb;
   int shw_init = 0;
   SUMA_VOL_SAUX *VSaux=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;                        

   if (!parent) { SUMA_SL_Err("NULL parent"); SUMA_RETURNe; }
   if (!ado) { SUMA_S_Err("NULL ado"); SUMA_RETURNe; }
   if (!(VSaux = SUMA_ADO_VSaux(ado))) { SUMA_S_Err("No VSaux"); SUMA_RETURNe; }
   
   if (LocalHead) {
      SUMA_S_Warn("Are NewValueCallback and its data needed at all?");
   }
   /* initialize font list if need be */
   if (!SUMAg_CF->X->TableTextFontList) {
      if (SUMA_isEnv("SUMA_SurfContFontSize", "BIG")) {
         SUMAg_CF->X->TableTextFontList = 
               SUMA_AppendToFontList( SUMAg_CF->X->TableTextFontList, 
                                       parent, "*9x15*", NULL);
      } else {
         SUMAg_CF->X->TableTextFontList = 
               SUMA_AppendToFontList( SUMAg_CF->X->TableTextFontList, 
                                       parent, "*8x13*", NULL);
      }    
   }
   
   if (!VrF) { SUMA_SL_Err("NULL VrF"); SUMA_RETURNe; }
   SUMA_LH("Init");
   VrF->Nslc = Nslc;
   VrF->NewValueCallback = NewValueCallback;
   VrF->NewValueCallbackData = cb_data;
   /* An outer row column to keep the inner one from resizing with parent 
   YOU COULD HAVE SET XmNadjustLast to False, instead ....*/
   VrF->rc = XtVaCreateManagedWidget ("rowcolumn",
      xmRowColumnWidgetClass, parent,
      XmNorientation , XmHORIZONTAL ,
      XmNpacking, XmPACK_TIGHT,
      XmNmarginHeight, 0,
      XmNmarginWidth, 0,
      NULL);
               
   SUMA_LH("Widgets, Nslc = %d", 
            VrF->Nslc);
   VrF->lab = XtVaCreateManagedWidget(tit, xmLabelWidgetClass, VrF->rc, 
                           XmNfontList, SUMAg_CF->X->TableTextFontList, 
                                     NULL);
   if (hint)  MCW_register_hint( VrF->lab, hint );
   if (help)  MCW_register_help( VrF->lab, help ) ;
   
   sprintf(sbuf,"%-3d", (int)VrF->N_slice_num);
   VrF->text = XtVaCreateManagedWidget(
                     "slice",  
                     xmTextFieldWidgetClass, VrF->rc,
                     XmNuserData, (XTP_CAST)ado,
                     XmNvalue, sbuf,
                     XmNmarginHeight, 0,
                     XmNmarginTop, 0,
                     XmNmarginBottom, 0,
                     XmNmarginWidth, 5, 
                     NULL);
   XtVaSetValues( VrF->text, XmNfontList, 
                  SUMAg_CF->X->TableTextFontList, NULL);

   if (help) MCW_register_help( VrF->text, help) ;
   if (hint) MCW_register_hint( VrF->text, hint) ;
   XtVaSetValues(VrF->text, XmNcolumns, 3, NULL); 
   XtVaSetValues(VrF->text, XmNeditable, True, 
                 XmNshadowThickness , 2,         
                 XmNcursorPositionVisible, True, 
                 NULL);

   XtAddCallback (VrF->text, XmNactivateCallback, 
               SUMA_VrF_cb_N_slc_change, (XtPointer)VrF);
   /* add event handler to notify when widget was left */
                              
   XtInsertEventHandler( VrF->text ,        /* notify when */
                         LeaveWindowMask ,  /* pointer leaves */
                         FALSE ,            /* this window */
                         SUMA_leave_NslcField,
                         (XtPointer) VrF ,
                         XtListTail ) ;     /* last in queue */

   /* Now for the toggle button */
   VrF->tb = XtVaCreateManagedWidget("v", 
      xmToggleButtonWidgetClass, VrF->rc, NULL);
   XtAddCallback (VrF->tb, 
         XmNvalueChangedCallback, SUMA_cb_ShowVrF_toggled, ado);
   MCW_register_hint(VrF->tb,   
                     "View (ON)/Hide VrF");
   MCW_register_help(VrF->tb,   
                     SUMA_SurfContHelp_ShowVrFTgl);

   SUMA_SET_SELECT_COLOR(VrF->tb);
   XmToggleButtonSetState (VrF->tb, VSaux->ShowVrSlc , NOPE);
   
   SUMA_RETURNe;
}

void SUMA_cb_ShowVrF_toggled(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_ShowVrF_toggled"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) { 
      SUMA_S_Warn("NULL input"); SUMA_RETURNe; }
      
   SUMA_SetShowSlice((SUMA_VolumeObject *)ado, "Vr", 
                      XmToggleButtonGetState (SurfCont->VR_fld->tb));
   SUMA_RETURNe;
}

/*!
   \brief This function is called when mouse pointer leaves slice field
   modeled after SUMA_leave_SliceField
*/
void SUMA_leave_NslcField( Widget w , XtPointer client_data ,
                            XEvent * ev , Boolean * continue_to_dispatch )
{
   static char FuncName[]={"SUMA_leave_NslcField"};
   SUMA_VR_FIELD *VrF=NULL; 
   XLeaveWindowEvent * lev = (XLeaveWindowEvent *) ev ;
   XmAnyCallbackStruct cbs ;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("Called");
   VrF = (SUMA_VR_FIELD *)client_data ;
   if( lev->type != LeaveNotify) SUMA_RETURNe; 

   if (LocalHead) fprintf (SUMA_STDERR, "%s: Leave notification.\n", FuncName);
   
   SUMA_VrF_cb_N_slc_change( w , (XtPointer)VrF , NULL ) ;

   SUMA_RETURNe;
}

/*!
   \brief User entered new slice value
   modeled after: SUMA_SliceF_cb_label_change
*/
void SUMA_VrF_cb_N_slc_change (  Widget w, XtPointer client_data, 
                                    XtPointer call_data)
{
   static char FuncName[]={"SUMA_VrF_cb_N_slc_change"};
   SUMA_VR_FIELD *VrF=NULL;
   float val;
   int N_words = 0;
   XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;
   void *n=NULL;
   char *cs=NULL;
   int unt = SUMA_NO_NUM_UNITS;
   SUMA_Boolean DoCallBacks;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SUMA_LH("Called");
   /* make call to NewValue callback */
   VrF = (SUMA_VR_FIELD *)client_data;
   
   DoCallBacks = NOPE;
   if (call_data) { 
      /* do the call backs if carriage return even if nothing is modified */
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: cbs->reason = %d (CR=%d)\n", 
                              FuncName, cbs->reason, XmCR_ACTIVATE);
      if (cbs->reason == XmCR_ACTIVATE) { 
         DoCallBacks = YUP;
      }
   }
   
   DoCallBacks = YUP;   /* do the callbacks even if no carriage return ... */
   /* Check if the string is numerical, and get unit */
   XtVaGetValues (w, XmNvalue, &n, NULL);
   cs = (char *)n;
   if (!cs || !strlen(cs)) {/* empty cell, leave it alone */ 
      SUMA_LHv("empty %s", cs);
      SUMA_RETURNe;
   } else  {
      SUMA_COUNT_WORDS(cs, NULL, N_words);
      if (!N_words) { /* no harm, go back */ 
         SUMA_LHv("spacy %s", cs);
         SUMA_RETURNe; 
      }
   }
   unt = SUMA_NumStringUnits(cs, 0);
   if (SUMA_StringToNum(cs, (void *)&val, 1, 1) != 1) {
      SUMA_BEEP;
      /* bad syntax, reset value*/
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Bad syntax.\n", FuncName);
      SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                            "Bad value in text field", FuncName, 
                            SMT_Error, SMA_Log);
      SUMA_VrF_SetNslcString (VrF); 
   }else {
      if (VrF->N_slice_num == val &&
          VrF->N_slice_units == unt) { 
            SUMA_LH("Same value"); 
            SUMA_RETURNe; 
      }
      SUMA_LH("A new beast? %f, %f, %d %d",
              VrF->N_slice_num, val, VrF->N_slice_units, unt);
      VrF->N_slice_num = val;
      VrF->N_slice_units = unt;
      SUMA_VrF_SetNslcString (VrF);
   }

   if (DoCallBacks) { 
      SUMA_set_slice((SUMA_ALL_DO *)VrF->NewValueCallbackData, "VR", 
                      &val, "text_field", 1);
   }
   
   SUMA_RETURNe;
}

/* model based on SUMA_SliceF_SetString */
void SUMA_VrF_SetNslcString(SUMA_VR_FIELD * VrF)
{
   static char FuncName[]={"SUMA_VrF_SetNslcString"};
   char buf[36];

   SUMA_ENTRY;

   if (VrF->N_slice_units == SUMA_NO_NUM_UNITS) {
      sprintf (buf, "%-4d", (int)VrF->N_slice_num);
   }else if (VrF->N_slice_units == SUMA_MM_UNITS) {
      sprintf (buf, "%s", 
               MV_format_fval2(  VrF->N_slice_num, 3));
   }else {
      /* fair enough, must be stringy */
   }
   
   XtVaSetValues (VrF->text, XmNvalue, buf, NULL);
   SUMA_RETURNe;
}

/*!
   \brief set transmode
*/ 
SUMA_Boolean SUMA_Set_ADO_TransMode(SUMA_ALL_DO *ado, int i) 
{
   static char FuncName[]={"SUMA_Set_ADO_TransMode"};
   
   SUMA_ENTRY;
   
   if (!ado) SUMA_RETURN(NOPE);
   
   switch (ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO = (SUMA_SurfaceObject *)ado;
         if (i < 0 || i >= STM_N_TransModes) { 
            SO->TransMode = STM_ViewerDefault;
         } else { SO->TransMode = i; }
         if (SO->TransMode == STM_16) { SO->Show = NOPE; } 
         else { SO->Show = YUP; } 
         break; }
      case VO_type: {
         SUMA_VolumeObject *VO = (SUMA_VolumeObject *)ado;
         SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
         if (!VSaux) SUMA_RETURN(NOPE);
         if (i < 0 || i >= SATM_N_TransModes) { 
            VSaux->TransMode = SATM_ViewerDefault;
         } else { VSaux->TransMode = i; }
         if (VSaux->TransMode == SATM_16) { VO->Show = NOPE; } 
         else { VO->Show = YUP; } 
         break; }
      default: 
         SUMA_S_Err("Not ready for %s (%s)", ADO_LABEL(ado), ADO_TNAME(ado));
         break;
   }
   
   SUMA_RETURN(YUP);
}

SUMA_ATRANS_MODES SUMA_TransMode2ATransMode(SUMA_TRANS_MODES ii) 
{
   static char FuncName[]={"SUMA_TransMode2ATransMode"};

   if (ii < 0 || ii > STM_N_TransModes) {
      SUMA_S_Err("Bad TransMode %d, returning viewerdefault", ii);
      return(SATM_ViewerDefault);
   }
   switch (ii) {
      case STM_ViewerDefault:
         return(SATM_ViewerDefault);
      case STM_N_TransModes:
         return(SATM_N_TransModes);
      default:
         return(SATM_0+ii-STM_0);
   }
   return(SATM_ViewerDefault);
}

SUMA_TRANS_MODES SUMA_ATransMode2TransMode(SUMA_ATRANS_MODES ii) 
{
   static char FuncName[]={"SUMA_ATransMode2TransMode"};

   if (ii < 0 || ii > SATM_N_TransModes) {
      SUMA_S_Err("Bad ATransMode %d, returning viewerdefault", ii);
      return(STM_ViewerDefault);
   }
   switch (ii) {
      case SATM_ALPHA:
         SUMA_S_Warn("No alpha available, returning viewerdefault");
         return(STM_ViewerDefault);
      case SATM_ViewerDefault:
         return(STM_ViewerDefault);
      case SATM_N_TransModes:
         return(STM_N_TransModes);
      default:
         return(STM_0+ii-SATM_0);
   }
   return(STM_ViewerDefault);
}
