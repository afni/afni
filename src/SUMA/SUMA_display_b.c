#include "SUMA_suma.h"
#include "coxplot.h"
#include "SUMA_plot.h"
 
/* SUMA_display.c is too big for my own good. 
   This is part the second                    */
   
void SUMA_cb_createSurfaceCont_MDO(Widget w, XtPointer data, 
                                     XtPointer callData)
{
   static char FuncName[] = {"SUMA_cb_createSurfaceCont_MDO"};
   Widget tl, pb, form,  
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
   
   
   SurfCont->DispFrame = SUMA_CloseBhelp_Frame(rc_gmamma,
                     SUMA_cb_closeSurfaceCont, (XtPointer) ado,
                     "MaskCont", "Close Surface controller", 
                     SUMA_closeSurfaceCont_help,
                     NULL, NULL, NULL, NULL);
   SUMA_Register_Widget_Help( SurfCont->DispFrame , 0,
                              "MaskCont",
                              "Network tracts mask controller",
"The mask controller is used for manipulating masks for network tracts"
":SPX:"
"You can launch the :ref:`Mask Controller <MaskCont>` from the "
":ref:`tract controller <TractCont>` by clicking on :ref:`Masks<TractCont->Coloring_Controls->Masks>` twice."
"\n\n"
".. figure:: media/MaskController.02.jpg\n"
"   :align: center\n"
"   :name: media/MaskController.02.jpg\n"
"\n"
"   :ref:`(link)<media/MaskController.02.jpg>`\n"
"   ..\n\n"
":DEF:"
"You can launch the Mask Controller by clicking twice on the 'Masks' "
"button of the tract controller.\n"
":SPX:"
"\n"  );
   
   /* Also stick in some help for fictitious widget of mask manipulation mode*/
   SUMA_Register_Widget_Help( NULL , 0,
                              "Mask_Manipulation_Mode",
                              "Mask Manipulation Mode",
"To move the mask interactively, right-double click on it to place SUMA "
"in :term:`Mask Manipulation Mode` which is indicated by displaying the "
"moveable mask in mesh mode (see help in SUMA, (:ref:`ctrl+h<LC_Ctrl+h>`),"
" section :ref:`Button 3-DoubleClick<Button_3-DoubleClick>` for details.)."
" Selecting a location on the tracts, the slices, or surfaces, will make the"
" mask jump to that location. The mask should also be visibile in AFNI (if "
"SUMA is launched with -dev option), and clicking in AFNI will make the mask "
"move in SUMA also.\n\n"
"To turn off 'Mask Manipulation Mode' right-double click in open air, or on the"
" manipulated mask itself.\n"
":SPX:\n"
".. figure:: media/MaskManipulationMode_OFF.jpg\n"
"   :align: left\n"
"   :figwidth: 45%\n"
"   :name: media/MaskManipulationMode_OFF.jpg\n"
"\n" 
"   :ref:`Mask manipulation OFF.<media/MaskManipulationMode_OFF.jpg>\n"
"\n"
".. figure:: media/MaskManipulationMode_ON.jpg\n"
"   :align: right\n"
"   :figwidth: 45%\n"
"   :name: media/MaskManipulationMode_ON.jpg\n"
"\n"
"   :ref:`Mask manipulation ON.<media/MaskManipulationMode_ON.jpg>`\n"
SUMA_SHPINX_BREAK
":SPX:"
   );
   
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
      Widget rc, label, rc_SurfProp, pb, www;
     
      /* put a frame */
      SurfCont->SurfFrame = XtVaCreateWidget ("dialog",
         xmFrameWidgetClass, rc_left,
         XmNshadowType , XmSHADOW_ETCHED_IN ,
         XmNshadowThickness , 5 ,
         XmNtraversalOn , False ,
         NULL); 
      
      www = XtVaCreateManagedWidget ("Masks",
            xmLabelWidgetClass, SurfCont->SurfFrame, 
            XmNchildType, XmFRAME_TITLE_CHILD,
            XmNchildHorizontalAlignment, XmALIGNMENT_BEGINNING,
            NULL);
      SUMA_Register_Widget_Help( www , 0,
                                 "MaskCont->Masks",
                          "Create/delete masks and setup masking expression",
                  ":SPX:\n\n"
                  ".. figure:: media/MaskCont.auto.Masks.jpg\n"
                  "   :align: right\n"
                  "   :name: media/MaskCont.auto.Masks.jpg\n"
                  "\n"
                  "   :ref:`(link)<media/MaskCont.auto.Masks.jpg>`\n"
                  "   ..\n\n"
                  ":SPX:") ;            

      rc_SurfProp = XtVaCreateWidget ("rowcolumn",
            xmRowColumnWidgetClass, SurfCont->SurfFrame,
            XmNpacking, XmPACK_TIGHT, 
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            NULL); 
      
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

      if (1) {
         /* add the mask equation region */
         char *row_tit[]=  {  "Mask Eval", NULL };
         char *row_hint[]= {  "Evaluate mask expression", NULL};
         char *row_help[]= {  SUMA_SurfContHelp_EvalMaskExpr_r0, NULL};
         Widget rc_maskeval;
         rc_maskeval = XtVaCreateManagedWidget ("rowcolumn",
                                       xmRowColumnWidgetClass, SurfCont->rcswr,
                                                XmNorientation , XmHORIZONTAL ,
                                                XmNmarginHeight, 0,
                                                XmNmarginHeight, 0,
                                                XmNmarginWidth, 0, 
                                                NULL);
         if (!SurfCont->MaskEvalTable->cells) {
            int colw[6] = { 3, 24 };
            SUMA_CreateTable( rc_maskeval,
                              1, 2, 
                              "MaskCont->Masks->Mask_Eval",
                              row_tit, NULL,  
                              row_hint, NULL,  
                              row_help, NULL,  
                              colw, YUP, SUMA_string, 
                              SUMA_cb_SetMaskEvalTableValue, NULL,
                              SUMA_MaskEvalTableLabel_EV, NULL,  
                              SUMA_MaskEvalTableCell_EV, NULL, 
                              SurfCont->MaskEvalTable);
         }
         /* And baby toggle  */
         SurfCont->MaskEval_tb = XtVaCreateManagedWidget("v", 
                                 xmToggleButtonWidgetClass, rc_maskeval, NULL);
         XtAddCallback (SurfCont->MaskEval_tb, 
                        XmNvalueChangedCallback, SUMA_cb_UseMaskEval_toggled,
                        NULL);
         SUMA_Register_Widget_Help(SurfCont->MaskEval_tb, 1,
                                   "MaskCont->Masks->Mask_Eval->v",
                           "Enable (ON)/Disable Mask expression evaluation",
                           "Enable (ON)/Disable Mask expression evaluation");

         SUMA_SET_SELECT_COLOR(SurfCont->MaskEval_tb);
         XmToggleButtonSetState (SurfCont->MaskEval_tb, 
                                 SurfCont->UseMaskEval , NOPE);

         if (!SurfCont->MaskLenTable->cells) {
            int colw[6] = { 3, 4, 4 };  
            char *row_tit[]=  {  " Tract Length", NULL };
            char *row_hint[]= {  "Mask based on tract length", NULL};
            char *row_help[]= {  SUMA_SurfContHelp_DistMask_r0, NULL};
            
            XtVaCreateManagedWidget (  "sep", 
                              xmSeparatorWidgetClass, rc_maskeval, 
                              XmNorientation, XmVERTICAL,NULL);
            
            SUMA_CreateTable( rc_maskeval,
                              1, 3, 
                              "MaskCont->Masks->Tract_Length",
                              row_tit, NULL,  
                              row_hint, NULL,  
                              row_help, NULL,  
                              colw, YUP, SUMA_float, 
                              SUMA_cb_SetMaskLenTableValue, NULL,
                              SUMA_MaskLenTableLabel_EV, NULL,  
                              SUMA_MaskLenTableCell_EV, NULL, 
                              SurfCont->MaskLenTable);
            SUMA_INSERT_CELL_VALUE(SurfCont->MaskLenTable, 0, 1, 
                                      SurfCont->tract_length_mask[0]);
            SUMA_INSERT_CELL_VALUE(SurfCont->MaskLenTable, 0, 2, 
                                      SurfCont->tract_length_mask[1]);
         }
         /* And baby toggle  */
         SurfCont->MaskLen_tb = XtVaCreateManagedWidget("v", 
                                 xmToggleButtonWidgetClass, rc_maskeval, NULL);
         XtAddCallback (SurfCont->MaskLen_tb, 
                        XmNvalueChangedCallback, SUMA_cb_UseMaskLen_toggled,
                        NULL);
         SUMA_Register_Widget_Help(SurfCont->MaskLen_tb, 1,
                                   "MaskCont->Masks->Tract_Length->v",
                           "Enable (ON)/Disable Tract Length masking",
                           "Enable (ON)/Disable Tract Length masking");

         SUMA_SET_SELECT_COLOR(SurfCont->MaskLen_tb);
         XmToggleButtonSetState (SurfCont->MaskLen_tb, 
                                 SurfCont->UseMaskLen , NOPE);
      }
      
      if (1) { /* The properties area */
         char *col_tit[]=  {  "+", " ", "Label", "Type", "Center", "Size", 
                                    "RGB", "A", "T", "D", NULL};
         char *col_hint[]= {  "Add new mask", 
                              "Variable",
                              "Label",
                              "Type ('box' or 'sphere')", 
                              "Center X,Y,Z", 
                              "Size Sx,Sy,Sz", 
                              "Color R G B (A)",
                              "A",
                              "T", "D", NULL };
         char *col_help[]= {  SUMA_SurfContHelp_MaskTypeTbl_c0, 
                              SUMA_SurfContHelp_MaskTypeTbl_c05,
                              SUMA_SurfContHelp_MaskTypeTbl_c1,
                              SUMA_SurfContHelp_MaskTypeTbl_c2, 
                              SUMA_SurfContHelp_MaskTypeTbl_c3, 
                              SUMA_SurfContHelp_MaskTypeTbl_c4,
                              SUMA_SurfContHelp_MaskTypeTbl_c5, 
                              SUMA_SurfContHelp_MaskTypeTbl_c6, 
                              SUMA_SurfContHelp_MaskTypeTbl_c7, 
                              SUMA_SurfContHelp_MaskTypeTbl_c8, 
                              NULL };
         char *row_tit[]=  {  "+", "x", NULL };
         char *row_hint[]= {  "Add new mask", "Mask Properties", NULL};
         char *row_help[]= {  SUMA_SurfContHelp_MaskTypeTbl_c0, 
                              SUMA_SurfContHelp_MaskTypeTbl_r1, NULL};
         if (!SurfCont->MaskTable->cells) {
            int colw[15] = { 1, 1, 6, 6, 11, 11, 11, 1, 1, 1};
            SUMA_CreateTable( SurfCont->rcswr,
                              2, 10, 
                              "MaskCont->Masks->Table",
                              row_tit, col_tit,  
                              row_hint, col_hint,  
                              row_help, col_help,  
                              colw, YUP, SUMA_string, 
                              SUMA_cb_SetMaskTableValue, NULL,
                              SUMA_MaskTableLabel_EV, NULL,  
                              SUMA_MaskTableCell_EV, NULL, 
                              SurfCont->MaskTable);
         }
      }
      
      XtVaCreateManagedWidget (  "sep", 
                                 xmSeparatorWidgetClass, SurfCont->rcswr, 
                                 XmNorientation, XmHORIZONTAL,NULL);

      /* row column for Switch, Load, Delete */
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, SurfCont->rcswr,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         NULL);
         
      pb = XtVaCreateWidget ("Load Masks", 
            xmPushButtonWidgetClass, rc, 
            NULL);   
         XtAddCallback (pb, XmNactivateCallback, 
                        SUMA_cb_Masks_Load, (XtPointer) ado);
         SUMA_Register_Widget_Help(pb, 1, "MaskCont->Masks->Load_Masks", 
                                   "Load the masks (much more with BHelp)",
                                   SUMA_SurfContHelp_MasksLoad ) ; 
         XtManageChild (pb);
 
      pb = XtVaCreateWidget ("Save Masks", 
            xmPushButtonWidgetClass, rc, 
            NULL);   
         XtAddCallback (pb, XmNactivateCallback, 
                        SUMA_cb_Masks_Save, (XtPointer) ado);
         SUMA_Register_Widget_Help(pb, 1, "MaskCont->Masks->Save_Masks",
                                   "Save the masks (much more with BHelp)",
                                   SUMA_SurfContHelp_MasksSave ) ;
         XtManageChild (pb);
 
      XtManageChild (rc);
      
      XtManageChild (rc_SurfProp);
      if (!XtIsManaged(SurfCont->rcswr)) XtManageChild (SurfCont->rcswr);
      XtManageChild (SurfCont->SurfFrame);
   }
   
   if (!SUMA_InitMasksTable(SurfCont)) {
      SUMA_S_Err("Failed to initialize table");
      SUMA_RETURNe;
   }

   if (SUMAg_CF->X->UseSameSurfCont) {
      Widget rc=NULL;
      /* put something to cycle through objects */
      if ((rc = SUMA_FindChildWidgetNamed(SurfCont->DispFrame,"rowcolumnCBF"))) {
         XtVaCreateManagedWidget (  "sep", 
                              xmSeparatorWidgetClass, rc, 
                              XmNorientation, XmVERTICAL,NULL);
         pb = XtVaCreateWidget ("All Objs.", 
                  xmPushButtonWidgetClass, rc, 
                  NULL);   
         XtAddCallback (pb, XmNactivateCallback, 
                        SUMA_cb_AllConts, NULL);
         SUMA_Register_Widget_Help(pb, 1, "MaskCont->Disp_Cont->AllObjs",
                                "Initialize Controllers for All Objects",
                                SUMA_SurfContHelp_AllObjs) ;
         XtManageChild (pb);

         SUMA_CreateArrowField ( rc, "Switch",
                           1, 1, 20, 1,
                           2, SUMA_int,
                           YUP,
                           SUMA_cb_SurfCont_SwitchPage, (void *)ado,
                           "MaskCont->Disp_Cont->Switch",
                           "Switch to other object controller", 
                           SUMA_Switch_Cont_BHelp,
                           SurfCont->SurfContPage);
         xmstmp = XmStringCreateLtoR (SUMA_ADO_CropLabel(ado,
                                       SUMA_SURF_CONT_SWITCH_LABEL_LENGTH), 
                                      XmSTRING_DEFAULT_CHARSET);
         SurfCont->SurfContPage_label = XtVaCreateManagedWidget ("dingel-7", 
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

SUMA_Boolean SUMA_Set_UseMaskEval(int v, int redisp, int setmen)
{
   static char FuncName[]={"SUMA_Set_UseMaskEval"};
   SUMA_X_SurfCont *SurfCont=NULL;
   DList *list=NULL;
   int vi=0;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SurfCont = SUMAg_CF->X->AllMaskCont;
   if (v) v = 1;
   else v = 0;
   if (setmen && SurfCont->MaskEval_tb) {
      vi = XmToggleButtonGetState (SurfCont->MaskEval_tb);
      if (v != vi) {
         XmToggleButtonSetState(SurfCont->MaskEval_tb, v, NOPE);
      }
   }
   SurfCont->UseMaskEval = v;
   
   if (redisp) {
      SUMA_NEW_MASKSTATE();
      /* redisplay */
      if (!list) list = SUMA_CreateList ();
      SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                         SES_Suma, NULL); 
      if (!SUMA_Engine(&list)) SUMA_SLP_Err("Failed to redisplay.");
   }
   
   SUMA_RETURN(NOPE);
}

void SUMA_cb_UseMaskEval_toggled(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_UseMaskEval_toggled"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   DList *list=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SurfCont = SUMAg_CF->X->AllMaskCont;
   
   SUMA_Set_UseMaskEval(XmToggleButtonGetState(SurfCont->MaskEval_tb),1,0);

   SUMA_RETURNe;
}


SUMA_Boolean SUMA_Set_UseMaskLen(int v, int redisp, int setmen)
{
   static char FuncName[]={"SUMA_Set_UseMaskLen"};
   SUMA_X_SurfCont *SurfCont=NULL;
   DList *list=NULL;
   int vi=0;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   SurfCont = SUMAg_CF->X->AllMaskCont;
   if (v) v = 1;
   else v = 0;
   if (setmen && SurfCont->MaskLen_tb) {
      vi = XmToggleButtonGetState (SurfCont->MaskLen_tb);
      if (v != vi) {
         XmToggleButtonSetState(SurfCont->MaskLen_tb, v, NOPE);
      }
   }
   SurfCont->UseMaskLen = v;
   
   if (redisp) {
      SUMA_NEW_MASKSTATE();
      /* redisplay */
      if (!list) list = SUMA_CreateList ();
      SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                         SES_Suma, NULL); 
      if (!SUMA_Engine(&list)) SUMA_SLP_Err("Failed to redisplay.");
   }
   
   SUMA_RETURN(NOPE);
}

void SUMA_cb_UseMaskLen_toggled(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_UseMaskLen_toggled"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   DList *list=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SurfCont = SUMAg_CF->X->AllMaskCont;
   
   SUMA_Set_UseMaskLen(XmToggleButtonGetState(SurfCont->MaskLen_tb),1,0);

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
   int  i, j, n, Found, incr=0, ii;
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
         break;
      case 2:
         SUMA_LH("Need to set label for mask %s", ADO_LABEL(ado));
         break;
      case 3:
         SUMA_LH("Need to set type for mask %s", ADO_LABEL(ado));
         break;
      case 4:
         SUMA_LH("Need to set center for mask %s", ADO_LABEL(ado));
         if (incr) {
         } else{
            if (bev->button == Button3) {/* reset dim */
               SUMA_MDO_New_Cen(mdo,mdo->init_cen);
            }
            SUMA_InitMasksTable_row(SurfCont,mdo, i);
            SUMA_NEW_MASKSTATE();
            /* redisplay */
            if (!list) list = SUMA_CreateList ();
            SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                               SES_Suma, NULL); 
            if (!SUMA_Engine(&list)) SUMA_SLP_Err("Failed to redisplay.");
         }
         break;
      case 5:
         if (incr) {
            fv[0] = mdo->hdim[0]+(incr*0.2*mdo->init_hdim[0]); 
            fv[1] = mdo->hdim[1]+(incr*0.2*mdo->init_hdim[1]); 
            fv[2] = mdo->hdim[2]+(incr*0.2*mdo->init_hdim[2]);
            SUMA_MDO_New_Dim(mdo, fv);
         } else {
            if (bev->button == Button3) {/* reset dim */
               SUMA_MDO_New_Dim(mdo,mdo->init_hdim);
            }
         }
         SUMA_InitMasksTable_row(SurfCont,mdo, i);
         
         SUMA_NEW_MASKSTATE();
         /* redisplay */
         if (!list) list = SUMA_CreateList ();
         SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                            SES_Suma, NULL); 
         if (!SUMA_Engine(&list)) SUMA_SLP_Err("Failed to redisplay.");
         break;
      case 6:
         SUMA_LH("Need to set color for mask %s", ADO_LABEL(ado));
         break;
      case 7:
         if (incr) {
            fv[0] = mdo->init_col[0]; fv[1] = mdo->init_col[1]; 
            fv[2] = mdo->init_col[2];
            ii = SUMA_A_to_1dig(mdo->init_col[3])+incr;
            fv[3] = SUMA_1dig_to_A(ii);
            if (fv[3] != mdo->init_col[3]) {
               SUMA_MDO_New_Alpha(mdo, fv[3]);
               SUMA_InitMasksTable_row(SurfCont,mdo, i);
               
               if (SurfCont->UseMaskEval) SUMA_NEW_MASKSTATE();
               /* redisplay */
               if (!list) list = SUMA_CreateList ();
               SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                                  SES_Suma, NULL); 
               if (!SUMA_Engine(&list)) SUMA_SLP_Err("Failed to redisplay.");
            } else {
               SUMA_InitMasksTable_row(SurfCont,mdo, i);
            }
         }
         break;
      case 8:
         if (incr) {
            ii = SUMA_T_to_1dig(mdo->SO->TransMode)+incr;
            if (ii >= 0 && ii < 10) {
               SUMA_Set_MaskDO_Trans(mdo,(SUMA_TRANS_MODES)SUMA_1dig_to_T(ii));
               SUMA_InitMasksTable_row(SurfCont,mdo, i);
               /* redisplay */
               if (!list) list = SUMA_CreateList ();
               SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                                  SES_Suma, NULL); 
               if (!SUMA_Engine(&list)) SUMA_SLP_Err("Failed to redisplay.");
            } else {
               SUMA_InitMasksTable_row(SurfCont,mdo, i);
            }
         }
         break;
      case 9:
         if (incr) {
            fv[0] = mdo->init_col[0]; fv[1] = mdo->init_col[1]; 
            fv[2] = mdo->init_col[2];
            ii = SUMA_A_to_1dig(mdo->dim)+incr;
            fv[3] = SUMA_1dig_to_A(ii);
            if (fv[3] != mdo->dim) {
               SUMA_MDO_New_CDim(mdo, fv[3]);
               SUMA_InitMasksTable_row(SurfCont,mdo, i);
               
               if (SurfCont->UseMaskEval) SUMA_NEW_MASKSTATE();
               /* redisplay */
               if (!list) list = SUMA_CreateList ();
               SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                                  SES_Suma, NULL); 
               if (!SUMA_Engine(&list)) SUMA_SLP_Err("Failed to redisplay.");
            } else {
               SUMA_InitMasksTable_row(SurfCont,mdo, i);
            }
         }
         break;
      default:
         SUMA_SL_Err("Did not know you had so many");
         break;
   }
   
   SUMA_RETURNe;
}

/* 
   Turns a user typed expression to one that the parser likes.
   Must have spaces around vars for parser, but a little ugly for
   typing. 
   string expr is not changed.
   string evale must be pre-allocated and be about twice as long as expr,
   just to be safe.
   string tight is perhaps a nicer to look at version of expr
*/

SUMA_Boolean SUMA_DispExpr_To_EvalExpr(char *uexpr, char *evale, char *tight)
{
   static char FuncName[]={"SUMA_DispExpr_To_EvalExpr"};
   int n, k, t, havevar, nex, pad;
   char *expr=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!uexpr || !evale) SUMA_RETURN(NOPE);

   expr = SUMA_copy_string(uexpr);
   deblank_name(expr);
   SUMA_LH("Have %s", expr);
   
   if (!strcasecmp(expr,"AND")) {
      /* All AND */
      sprintf(evale,"AND");
      sprintf(tight,"AND");
      SUMA_RETURN(YUP);
   } else if (!strcasecmp(expr,"OR")) {
      /* All OR */
      sprintf(evale,"OR");
      sprintf(tight,"OR");
      SUMA_RETURN(YUP);
   }
   
   /* switch AND and OR to | and & and trim && and || */
   nex = strlen(expr);
   n = 0; k = 0;
   while (expr[n] != '\0') {
      if (expr[n]   == 'A' && n<nex-3 &&
          expr[n+1] == 'N' && expr[n+2] == 'D') {
          evale[k++] = '&';
         n = n + 2;
      } else if (expr[n]   == 'O' && n<nex-2 &&
          expr[n+1] == 'R') {
          evale[k++] = '|';
         n = n + 1;
      } else if (expr[n] == '+') {
         evale[k++] = '|';
      } else if (expr[n] == '*') {
         evale[k++] = '&';
      } else {
         if ((k > 0 && expr[n] != evale[k]) 
             || k == 0) { /* to skip dups of | and & */
            evale[k++]=expr[n];
         } else {
         }
      }
      n = n + 1;
   }
   evale[k] = '\0';
   SUMA_STRING_REPLACE(expr, evale);  
   SUMA_LH("Now have %s", expr);

   /* remove all blanks from expr */
   n = 0; k = 0; t= 0;
   while (expr[n] != '\0') {
      if (SUMA_IS_BLANK((expr[n]))) {
      } else {
         evale[k++] = expr[n];
         if (tight) tight[t++] = expr[n];
      }
      ++n;
   }
   evale[k] = '\0';
   SUMA_STRING_REPLACE(expr, evale);  
   SUMA_LH("No blank has %s", expr);
   
   /* Now insert space between each character */
   evale[0] = expr[0];
   n = 1; k = 1; t= 0;
   while (expr[n] != '\0') {
      if (SUMA_IS_BLANK((expr[n]))) {
      } else {
         if (!SUMA_IS_BLANK(evale[k-1])) {
            evale[k++] = ' ';
            evale[k++] = expr[n];
            if (expr[n+1] != '\0') evale[k++] = ' ';
         } else {
            evale[k++] = expr[n];
         }
      }
      ++n;
   }
   evale[k] = '\0';
   SUMA_LH("And now have %s", evale);
   
   /* Make sure everything is blank separatd */
   
   
   /* Now make sure the equation has nothing but acceptable chars */
   k = 0;
   while (evale[k] != '\0') {
      if (evale[k] == '|' || evale[k] == '&'   || 
          (evale[k] >= 'a' && evale[k] <= 'z') ||
          evale[k] == '(' || evale[k] == ')'   ||
          evale[k] == '!'                      ||
          SUMA_IS_BLANK(evale[k])              ) {
         /* all good */
      } else {
         SUMA_S_Err("Character %c (#%d) in %s from %s not allowed",
                     evale[k], k, evale, expr);
         SUMA_ifree(expr);
         SUMA_RETURN(NOPE);
      }
      ++k; 
   }
   
   /* And make sure the equation does not start or end with and or or */
   k = 0; havevar = 0;
   while (evale[k] != '\0' && !havevar) { 
      if ((evale[k] == '|' || evale[k] == '&') && !havevar) {
         SUMA_S_Err(
            "Encountered operator %c before variable at char #%d in %s from %s",
            evale[k], k, evale, expr);
         SUMA_ifree(expr);
         SUMA_RETURN(NOPE);
      } else if (evale[k] >= 'a' && evale[k] <= 'z') {
         havevar = 1;
      }
      ++k;
   }
   k = strlen(evale)-1; havevar = 0;
   while (k >=0 && !havevar) { 
      if ((evale[k] == '|' || evale[k] == '&') && !havevar) {
         SUMA_S_Err(
         "Encountered operator %c after last variable at char #%d in %s from %s",
            evale[k], k, evale, expr);
         SUMA_ifree(expr);
         SUMA_RETURN(NOPE);
      } else if (evale[k] >= 'a' && evale[k] <= 'z') {
         havevar = 1;
      }
      --k;
   }
   
   SUMA_LH("About to return: expr %s, tight %s, evale %s",
            expr, tight, evale);
   SUMA_ifree(expr);
   SUMA_RETURN(YUP);
}

void SUMA_MaskEvalTableCell_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   static char FuncName[]={"SUMA_MaskEvalTableCell_EV"};
   SUMA_ALL_DO *ado=NULL, *curDO=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   XButtonEvent * bev = (XButtonEvent *) ev ;
   int  i, j, n, Found, incr=0;
   float fv[4];
   char evale[256]={""}, tight[128]={""}, exp[128]={""};
   void *cv=NULL;
   SUMA_MaskDO *mdo = NULL;
   DList *list=NULL;
   SUMA_TABLE_FIELD *TF = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called Button %d", bev->button);
   
   SurfCont = SUMAg_CF->X->AllMaskCont;
   curDO = SUMA_Cont_ADO(SurfCont);
   TF = SurfCont->MaskEvalTable;
   
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
         break;
      case Button5:
      case 7: 
         SUMA_LH("Button 5/7 %d", bev->button);
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
   
   switch (j) {
      case 0:
         break;
      case 1:
         break;
      case 2:
         break;
      case 3:
         break;
      case 4:
         break;
      case 5:
         break;
      default:
         SUMA_SL_Err("Did not know you had so many");
         break;
   }
   SUMA_RETURNe;
}

void SUMA_MaskLenTableCell_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   static char FuncName[]={"SUMA_MaskLenTableCell_EV"};
   SUMA_ALL_DO *ado=NULL, *curDO=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   XButtonEvent * bev = (XButtonEvent *) ev ;
   int  i, j, n, Found, incr=0;
   float fv[4];
   char evale[256]={""}, tight[128]={""}, exp[128]={""};
   void *cv=NULL;
   SUMA_MaskDO *mdo = NULL;
   DList *list=NULL;
   SUMA_TABLE_FIELD *TF = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called Button %d", bev->button);
   
   SurfCont = SUMAg_CF->X->AllMaskCont;
   curDO = SUMA_Cont_ADO(SurfCont);
   TF = SurfCont->MaskLenTable;
   
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
   
   switch (j) {
      case 0:
         break;
      case 1:
      case 2:
         if (incr) {
              if (TF->num_value[n]>1000) incr = incr*100;
            else if (TF->num_value[n]>100) incr = incr*10;
            else if (TF->num_value[n]>50) incr = incr*5;
            else if (TF->num_value[n]>10) incr = incr*2;
            SUMA_SetMaskLenTableValueNew(i, j,
                          TF->num_value[n]+incr,
                          1, 1, TF->num_units);
         }
         break;
      case 3:
         break;
      case 4:
         break;
      case 5:
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
               if (SUMA_NewSymMaskDO(NULL)<0) {
                  SUMA_S_Err("Failed create new mask");
                  SUMA_RETURNe;
               } 

               if (!SUMA_InitMasksTable(SurfCont)) {
                  SUMA_S_Err("Failed to add row");
                  SUMA_RETURNe;
               }
               SUMA_NEW_MASKSTATE();
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
            }else if (bev->button == Button3) { 
               
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

void SUMA_MaskEvalTableLabel_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   static char FuncName[]={"SUMA_MaskEvalTableLabel_EV"};
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
            }
            break;
         default:
            if (bev->button == Button1) { 

            }else if (bev->button == Button3) { 
               
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

void SUMA_cb_SetMaskEvalTableValue (void *data) 
{
   static char FuncName[]={"SUMA_cb_SetMaskEvalTableValue"};
   SUMA_ALL_DO *ado=NULL;
   int n=-1,row=-1,col=-1, an=0;
   void *cv=NULL; 
   SUMA_TABLE_FIELD *TF=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SurfCont = SUMAg_CF->X->AllMaskCont;
   
   TF = SurfCont->MaskEvalTable;
   if (LocalHead) {
      fprintf(SUMA_STDERR, 
         "%s:\n Entered mask eval table cell, cell modified %d \n", 
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

   an = SUMA_SetMaskEvalTableValueNew(row, col,
                          (char *)cv,
                          0, 1, TF->num_units);
   if (an < 0) {
      SUMA_BEEP; 
   }
   
   SUMA_RETURNe;
}

void SUMA_MaskLenTableLabel_EV ( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   static char FuncName[]={"SUMA_MaskLenTableLabel_EV"};
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
            }
            break;
         default:
            if (bev->button == Button1) { 

            }else if (bev->button == Button3) { 
               
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

void SUMA_cb_SetMaskLenTableValue (void *data) 
{
   static char FuncName[]={"SUMA_cb_SetMaskLenTableValue"};
   SUMA_ALL_DO *ado=NULL;
   int n=-1,row=-1,col=-1, an=0;
   void *cv=NULL; 
   SUMA_TABLE_FIELD *TF=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SurfCont = SUMAg_CF->X->AllMaskCont;
   
   TF = SurfCont->MaskLenTable;
   if (LocalHead) {
      fprintf(SUMA_STDERR, 
         "%s:\n Entered mask dist table cell, cell modified %d \n", 
         FuncName, TF->cell_modified);
   }

   if (TF->cell_modified<0) SUMA_RETURNe;
   n = TF->cell_modified;
   row = n % TF->Ni;
   col = n / TF->Ni;
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\nTable cell[%d, %d]=%f\n", 
                           FuncName, row, col, TF->num_value[n]);
   }

   an = SUMA_SetMaskLenTableValueNew(row, col,
                          TF->num_value[n],
                          0, 1, TF->num_units);
   if (an < 0) {
      SUMA_BEEP; 
   }
   
   SUMA_RETURNe;
}

void SUMA_cb_SetMaskTableValue (void *data) 
{
   static char FuncName[]={"SUMA_cb_SetMaskTableValue"};
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
   char *row_tit_buf=NULL, *col_tit_buf=NULL, wname[64]={"NOTSETATALL"};
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
                  snprintf(wname, 63, "%s.r%02d", TF->wname, i);
                  SUMA_Register_Widget_Help(TF->cells[n], 1, wname, 
                                            row_hint?row_hint[i]:NULL, 
                                            row_help?row_help[i]:NULL ) ;
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
                  snprintf(wname, 63, "%s.c%02d", TF->wname, i);
                  SUMA_Register_Widget_Help(TF->cells[n], 1, wname, 
                                            col_hint?col_hint[j]:NULL, 
                                            col_help?col_help[j]:NULL ) ;
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
                        char *shh=NULL, *sii=NULL;
                        if (col_help) shh = col_help[0] ;
                        else if (row_help) shh = row_help[0] ;
                        if (col_hint) sii = col_hint[0] ;
                        else if (row_hint) sii =  row_hint[0] ;
                        if (shh || sii) {
                           if (TF->Ni>1) {
                              snprintf(wname, 63, "%s[%d,%d]", TF->wname, i,j);
                           } else {
                              snprintf(wname, 63, "%s[%d]", TF->wname, n);
                           }
                           SUMA_Register_Widget_Help(TF->cells[n], 1, wname,
                                            sii,
                                            shh ) ;
                        }
                        
                     } else {
                        if (TF->Ni>1) {
                           snprintf(wname, 63, "%s[%d,%d]", TF->wname, i,j);
                        } else {
                           snprintf(wname, 63, "%s[%d]", TF->wname, n);
                        }
                        SUMA_Register_Widget_Help(TF->cells[n], 1, wname,
                                    NULL,
                                    "Use BHelp on table's column and row titles"
                                    "for usage information.") ;
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

/* Change a color alpha to range from 0 to 9 */
int SUMA_A_to_1dig(float v)
{
   if (v<0) v = 0;
   else if (v>1) v = 1.0;
   return((int)(v*9.0));
}

float SUMA_1dig_to_A(int i)
{
   if (i < 0) i = 0;
   else if (i > 9) i = 9;
   return(i/9.0);
}

/* Change a transparency setting to a range from 0 to 9 */
int SUMA_T_to_1dig(SUMA_TRANS_MODES stm)
{
   switch(stm) {
      case STM_ViewerDefault:
         return(0);
      case STM_0:
      case STM_1:
         return(1);
      case STM_2:
      case STM_3:
         return(2);
      case STM_4:
      case STM_5:
         return(3);
      case STM_6:
      case STM_7:
         return(4);
      case STM_8:
      case STM_9:
         return(5);
      case STM_10:
      case STM_11:
         return(6);
      case STM_12:
      case STM_13:
         return(7);
      case STM_14:
      case STM_15:
         return(8);
      case STM_16:
         return(9);
      default:
         return(0);
   }
   return(0);
}

SUMA_TRANS_MODES SUMA_1dig_to_T(int i)
{
   switch(i){
      default:
      case 0:
         return(STM_ViewerDefault);
      case 1:
         return(STM_0);
      case 2:
         return(STM_2);
      case 3:
         return(STM_4);
      case 4:
         return(STM_6);
      case 5:
         return(STM_8);
      case 6:
         return(STM_10);
      case 7:
         return(STM_12);
      case 8:
         return(STM_14);
      case 9:
         return(STM_16);
   }
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

   SUMA_INSERT_CELL_STRING(SurfCont->MaskTable,row, 1, mdo->varname);
   
   SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 2, 
                           ADO_LABEL((SUMA_ALL_DO *)mdo));

   if      (MDO_IS_SPH(mdo)) {
      SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 3, "sphere");
   } else if (MDO_IS_BOX(mdo)) {
      SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 3, "box");
   } else {
      SUMA_S_Err("Not ready for type %s, not here at least", mdo->mtype);
   }
   
   SUMA_RGBA_to_string(mdo->cen, 3, 1.0, str, NULL, ",",4);
   SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 4,  str);
   
   SUMA_RGBA_to_string(mdo->hdim, 3, 1.0, str, NULL, ",",5);
   SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 5,  str);

   SUMA_RGBA_to_string(mdo->init_col, 4, 1.0, str, NULL, ",",-1);
   SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 6,  str);
   
   sprintf(str,"%d", SUMA_A_to_1dig(mdo->init_col[3]));
   SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 7,  str);
   
   sprintf(str,"%d", SUMA_T_to_1dig(mdo->trans));
   SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 8,  str);
   
   sprintf(str,"%d", SUMA_A_to_1dig(mdo->dim));
   SUMA_INSERT_CELL_STRING(SurfCont->MaskTable, row, 9,  str);
   
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
   
   dlist_destroy(dl);SUMA_free(dl);
   
   SUMA_RETURN(YUP);
}

int SUMA_NewSymMaskDO(SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_NewSymMaskDO"};
   SUMA_MaskDO *mdo=NULL;
   float cen[3] = {0, 0, 0};
   int ido;
   static int icall=0;
   char mtype[32], hid[32];
   char symstr[256]={"sphere(0, 0, 0; 20, 20, 20)"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ido = -1;
   sprintf(hid,"msk%d", icall); 
   mdo = SUMA_SymMaskDO(symstr, mtype, hid, 0);
   
   if (!ado) {
      ado = (SUMA_ALL_DO *)SUMA_findanyTDOp_inDOv(SUMAg_DOv, SUMAg_N_DOv, NULL);
   }
   if (ado) {
      SUMA_ADO_Center(ado, cen);
   }
   
   switch(icall) {
      case 0:{
         float cc[4] = {1, 1, 1, 1};
         SUMA_Set_MaskDO_Cen(mdo, cen);
         SUMA_Set_MaskDO_Color(mdo, cc, -1);
         break; }
      case 1:{
         float cc[4] = {1, 0, 0, 1};
         cen[0] += 40; 
         cen[1] -= 65;
         cen[2] -= 20;
         SUMA_Set_MaskDO_Color(mdo, cc, -1);
         SUMA_Set_MaskDO_Cen(mdo, cen);
         break; }
      case 2:{
         float cc[4] = {0, 1, 0, 1};
         cen[0] -= 25; 
         cen[1] -= 65;
         cen[2] -= 5;
         SUMA_Set_MaskDO_Color(mdo, cc, -1);
         SUMA_Set_MaskDO_Cen(mdo, cen);
         break; }
      case 3:{
         float cc[4] = {0, 0, 1, 1};
         cen[0] += 0; 
         cen[1] += 35;
         cen[2] += 10;
         SUMA_Set_MaskDO_Color(mdo, cc, -1);
         SUMA_Set_MaskDO_Cen(mdo, cen);
         break; }
      case 4: {
         float cc[4] = {1, 1, 0, 1};
         cen[0] += 60; 
         cen[1] -= 10;
         cen[2] -= 40;
         SUMA_Set_MaskDO_Color(mdo, cc, -1);
         SUMA_Set_MaskDO_Cen(mdo, cen);
         break; }
      case 5: {
         float cc[4] = {0, 1, 1, 1};
         cen[0] += 20; 
         cen[1] -= 40;
         cen[2] += 10;
         SUMA_Set_MaskDO_Color(mdo, cc, -1);
         SUMA_Set_MaskDO_Cen(mdo, cen);
         break; }
      case 6: {
         float cc[4] = {1, 0, 1, 1};
         cen[0] -= 20; 
         cen[1] += 40;
         cen[2] -= 10;
         SUMA_Set_MaskDO_Color(mdo, cc, -1);
         SUMA_Set_MaskDO_Cen(mdo, cen);
         break; }
      default: {
         float cc[4] = {1, 0, 1, 1};
         SUMA_a_good_col("ROI_i256", icall-6,cc);
         SUMA_Set_MaskDO_Color(mdo, cc, -1);
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

   if (!client_data) {
      SUMA_S_Err("Bad call, no clients.");
      SUMA_RETURNe;
   }
   ado = (SUMA_ALL_DO *)client_data;
   if (ado->do_type != TRACT_type) {
      SUMA_S_Err("Expect tracts only here");
      SUMA_RETURNe;
   }
   tdo = (SUMA_TractDO *)ado;
   if (!(SurfCont = SUMA_ADO_Cont(ado))) {
      SUMA_S_Err("No surfcont? Work with me please!");
      SUMA_RETURNe;
   }
   TSaux = TDO_TSAUX(tdo);
   if (!w || !callData) {
      /* OK, callback used by driver too */
      SUMA_LH("Driven? Make sure tracts controller already up");
      if (!SUMA_isADO_Cont_Created(ado)) {
         if (!SUMA_OpenCloseSurfaceCont(w, ado, NULL)) {
            SUMA_S_Err("Could not open tract cont");
            SUMA_RETURNe;
         }
      }
   }
   
   /* How many masks do we have so far? */
   if (!SUMA_findanyMDOp_inDOv(SUMAg_DOv, SUMAg_N_DOv, &ido)) {
      /* Create a dummy uber owner that will never get displayed
         but can always be relied upon to exit. Other masks can 
         get deleted, leaving the mask controller ownerless */
      SUMA_ShadowMaskDO(NULL);   
      /* Now Need to create a new Mask for real*/
      SUMA_LH("Need a new mask");
      if ((ido = SUMA_NewSymMaskDO(NULL))<0) {
         SUMA_S_Err("Failed to create SymMask");
         SUMA_RETURNe;
      }
      mdo = (SUMA_MaskDO *)iDO_ADO(ido);
      SUMA_LH("Outa here");
      XtSetSensitive(SurfCont->TractMaskMenu->mw[SW_SurfCont_TractMask], 1);
      XtSetSensitive(SurfCont->TractMaskGray->rc, 1);
      
      SUMA_NEW_MASKSTATE();

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
   SUMA_SurfaceViewer *sv=NULL;
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
         /* delete mask SEE ALSO function SUMA_DeleteMask(), might want to use
            it instead of this chunk */
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
            
            /* Make sure  object is not the one selected for mouse movement */
            for (ii=0; ii<SUMAg_N_SVv; ++ii) {
               sv = SUMAg_SVv+ii;
               if ( sv && sv->MouseMode_ado_idcode_str) {
                  if ( !strcmp(sv->MouseMode_ado_idcode_str, ado_id)) {
                     SUMA_LH("Mask selected mask will be deleted, leave mask"
                             "manip mode.");
                     if (!SUMA_SetMouseMode(sv,SUMA_MASK_MANIP_MMODE,NULL)) {
                        SUMA_S_Warn("Mask manip mode could not be set");
                     }
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
      
         SUMA_NEW_MASKSTATE();
         
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

SUMA_Boolean SUMA_DeleteAllMasks(char *labeled, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]= {"SUMA_DeleteAllMask"};
   int i = 0;
   SUMA_MaskDO *MDO = NULL;
   SUMA_ALL_DO *ado = NULL;
   
   SUMA_ENTRY;
   
   if (!dov) {
      dov = SUMAg_DOv;
      N_dov = SUMAg_N_DOv;
   }
   for (i=0; i< N_dov; ++i) {
      if (dov[i].ObjectType != MASK_type) continue;
      MDO = (SUMA_MaskDO *)dov[i].OP;
      ado = (SUMA_ALL_DO *)MDO;
      
      if (!MDO_IS_SHADOW(MDO) && 
          (!labeled || (!strcmp(labeled,ADO_LABEL(ado)))) ) {
         if (!(SUMA_DeleteMask(ADO_ID(ado)))) {
            SUMA_S_Err("Failed to delete MDO");
         }
      }
   }
   
   SUMA_RETURN(YUP);
}

/* Delete a MaskDO from everything and everywhere.
   Make sure changes here, parallel those in function SUMA_cb_Mask_Delete()
   right where SUMA_DeleteMask() is mentioned.*/
SUMA_Boolean SUMA_DeleteMask(char *ado_id)
{
   static char FuncName[]= {"SUMA_DeleteMask"};
   SUMA_ALL_DO *ado = NULL, *curDO = NULL;
   int found = -1, OKtable=0, ii;
   SUMA_X_SurfCont *SurfCont = NULL;
   SUMA_SurfaceViewer *sv = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado_id) SUMA_RETURN(YUP); /* Don't complain, nothing to delete */
   
   if (ado = SUMA_whichADOg(ado_id)) {
      SUMA_LH("ado_id is for %s", ADO_LABEL(ado));
      if (ado->do_type != MASK_type) {
         SUMA_S_Err("ADOid not for mask type");
         SUMA_RETURN(NOPE);
      }
   } else {
      SUMA_LH("ado_id does not exist, return without complaint");
      SUMA_RETURN(YUP);
   }
         
   SurfCont = SUMAg_CF->X->AllMaskCont;

   found = -1;
   if (SurfCont && SurfCont->MaskTable) {
      if ((found = SUMA_ObjectID_Row(SurfCont->MaskTable, ado_id))< 0) {
         SUMA_LH("ado not in table");
      } else {
         SUMA_LH("Making sure current is OK");
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
         }
      }
   }
   
   /* Make sure this object is not the one selected for mouse movement */
   for (ii=0; ii<SUMAg_N_SVv; ++ii) {
      sv = SUMAg_SVv+ii;
      if ( sv && sv->MouseMode_ado_idcode_str) {
         if ( !strcmp(sv->MouseMode_ado_idcode_str, ado_id)) {
            SUMA_LH("Woops, can't be in mask manip mode any more");
            if (!SUMA_SetMouseMode(sv,SUMA_MASK_MANIP_MMODE,NULL)) {
               SUMA_S_Warn("Mask manip mode could not be set");
            }
         } 
      }
   }
   
   /* unregister do from all viewers */
   SUMA_UnRegisterDO_idcode(ado_id,NULL);

   /* delelte the current mask from DOv */
   if (!SUMA_RemoveDO(SUMAg_DOv, &SUMAg_N_DOv, (void *)ado, 1)){
      SUMA_S_Err("Failed to dump DO");
      SUMA_RETURN(NOPE);
   }
   
   if (found >= 0 && SurfCont && SurfCont->MaskTable) {
      if (!SUMA_ModifyTable(SurfCont->MaskTable, 
                           SurfCont->MaskTable->Ni-1)) {
         SUMA_S_Err("Failed to delete table row");
         SUMA_RETURN(NOPE);
      } else {
         SUMA_InitMasksTable(SurfCont);         
      }
   }
         
      
   SUMA_NEW_MASKSTATE();
   
   SUMA_RETURN(YUP);
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
   int dg;
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
         if (SUMA_MDO_OkVarName(s1)) {
            if (setmen) {
               SUMA_INSERT_CELL_STRING(TF, row, col, mdo->varname);
            }
            SUMA_MDO_SetVarName(mdo, s1);
            init_row = 2;
         } else {
            SUMA_INSERT_CELL_STRING(TF, row, col, mdo->varname);
         }
         break;
      case 2:  
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
      case 3:
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
      case 4:
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
      case 5:
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
      case 6:
         SUMA_LHv("Setting color of %f %f %f %f to %s, isCur=%d [%d %d] \n", 
                  mdo->init_col[0], mdo->init_col[1], 
                  mdo->init_col[2], mdo->init_col[3], 
                  s1, isCur, row, col);
         fv = SUMA_string_to_RGBA(s1, NULL, 1.0, &Err);
         if (!Err) {
            SUMA_MDO_New_Color(mdo, fv);
            if (setmen) {
               SUMA_RGBA_to_string(mdo->init_col, 4, 1.0, str, NULL, ",",4);
               SUMA_LHv("Inserting ado col back %s\n", 
                        str);
               SUMA_INSERT_CELL_STRING(TF, row, col, str);
            }
         } else { /* failed, reset string */
            SUMA_RGBA_to_string(mdo->init_col, 4, 1.0, str, NULL, ",", 4);
            SUMA_LHv("Resetting ado colv string back %s\n", 
                     str);
            SUMA_INSERT_CELL_STRING(TF, row, col, str);
         }
         break;
      case 7:
         dg = (int)atoi(s1);
         if (dg < 0) dg = 0;
         else if (dg > 9) dg = 9;
         SUMA_LH("Setting Alpha from %s (%d, %f)(setmen is %d)\n"
                 "%f %f %f %f\n", 
                  s1, dg, SUMA_1dig_to_A(dg), setmen,
                  mdo->init_col[0], mdo->init_col[1], 
                  mdo->init_col[2], mdo->init_col[3]);
         SUMA_MDO_New_Alpha(mdo, SUMA_1dig_to_A(dg));
         /* changing A affects the color string */
         SUMA_RGBA_to_string(mdo->init_col, 4, 1.0, str, NULL, ",",4);
         SUMA_LH("Setting RGB to %s %f %f %f %f\n", 
                  str,
                  mdo->init_col[0], mdo->init_col[1], 
                  mdo->init_col[2], mdo->init_col[3]);
         SUMA_INSERT_CELL_STRING(TF, row, 6, str);
         if (setmen) {
            sprintf(str,"%d",dg);
            SUMA_LHv("Inserting ado A back %s\n", str);
            SUMA_INSERT_CELL_STRING(TF, row, col, str);
         }
         break;
      case 8:
         SUMA_LH("Setting Trans from %s", s1);
         dg = (int)atoi(s1);
         if (dg < 0) dg = 0;
         else if (dg > 9) dg = 9;
         SUMA_MDO_New_Trans(mdo, SUMA_1dig_to_T(dg));
         if (setmen) {
            sprintf(str,"%d",dg);
            SUMA_LHv("Inserting ado tran back %s\n", 
                     str);
            SUMA_INSERT_CELL_STRING(TF, row, col, str);
         }
         break;
      case 9:
         dg = (int)atoi(s1);
         if (dg < 0) dg = 0;
         else if (dg > 9) dg = 9;
         SUMA_LH("Setting dim from %s (%d, %f)(setmen is %d)\n"
                 "%f %f %f %f\n", 
                  s1, dg, SUMA_1dig_to_A(dg), setmen,
                  mdo->init_col[0], mdo->init_col[1], 
                  mdo->init_col[2], mdo->init_col[3]);
         SUMA_MDO_New_CDim(mdo, SUMA_1dig_to_A(dg));
         if (setmen) {
            sprintf(str,"%d",dg);
            SUMA_LHv("Inserting ado Dim back %s\n", str);
            SUMA_INSERT_CELL_STRING(TF, row, col, str);
         }
         break;
      default:
         SUMA_SL_Err("You make me sick");
         break;
   }
   
   if (init_row == 1) {
      SUMA_InitMasksTable_row(SurfCont, mdo, row);
   } else if (init_row == 2) { /* The whole table needs love */
      SUMA_InitMasksTable(SurfCont);
   }
   
   SUMA_NEW_MASKSTATE();

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

char *SUMA_GetMaskEvalExpr(void) 
{
   static char FuncName[]={"SUMA_GetMaskEvalExpr"};
   static int icall=0;
   static char expv[10][128];
   char evale[256]={""}, tight[128]={""};
   char *exp=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ++icall;
   if (icall > 9) icall = 0;
   exp = expv[icall];
   exp[0] = '\0';
   
   if (!(SurfCont=SUMAg_CF->X->AllMaskCont) || 
       !(TF = SurfCont->MaskEvalTable) ||
       TF->Ni < 1 /* Ensure it is actually created */) {
      SUMA_RETURN(exp);
   }
   
   if (!SurfCont->UseMaskEval) {
      SUMA_RETURN(exp);
   }
   
   if (TF->str_value[1*TF->Ni+0]) {
      strncpy(exp, TF->str_value[1*TF->Ni+0], 127);
      if (!SUMA_DispExpr_To_EvalExpr(exp, evale, tight)) {
         SUMA_SLP_Warn("Parsing error encountered. Check command line");
         exp[0] = '\0';
         SUMA_RETURN(exp);
      } else {
         SUMA_LH("exp >%s<, evale >%s<, tight >%s<",
                  exp, evale, tight);
         /* use the eval version */
         strncpy(exp, evale, 127);
         /* and set the tight version for display */
         if (strcmp(exp, tight)) SUMA_INSERT_CELL_STRING(TF, 0, 1, tight);
      }
   }
   SUMA_RETURN(exp);   
}

int SUMA_SetMaskEvalTableValueNew(  int row, int col,
                                char *s1, 
                                int setmen, 
                                int redisplay, 
                                SUMA_NUMERICAL_UNITS num_units) 
{
   static char FuncName[]={"SUMA_SetMaskEvalTableValueNew"};
   int NewDisp=0, isCur=0, Err=0, init_row=0;
   SUMA_MaskDO *mdo=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   float *fv;
   SUMA_ALL_DO *ado=NULL;
   char evale[256]={""}, tight[128]={""};
   char str[256];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      SUMA_LH("Called on cell[%d %d] with %s", row, col, s1?s1:"NULL");
      SUMA_DUMP_TRACE("Who called SUMA_SetMaskEvalTableValueNew?");
   }
   
   if (!(SurfCont=SUMAg_CF->X->AllMaskCont)) {
      SUMA_RETURN(0);
   }
   
      
   TF = SurfCont->MaskEvalTable;
   if (!TF) setmen = 0; /* can't set nothing */
   
   if (num_units == SUMA_PERC_VALUE_UNITS) {
      SUMA_S_Err("No percentiles allowed here");
      SUMA_RETURN(NOPE);
   }
   
   if (row < 0) {
      SUMA_S_Err("What is for row %d < 0?", row); SUMA_RETURN(NOPE);
   }
   if (col < 1) {
      SUMA_S_Err("What is for col %d < 1?", col); SUMA_RETURN(NOPE);
   }
   NewDisp = NOPE;
   
   /* What are we dealing with ? */
   switch (col) {
      case 1:  
         SUMA_LHv("Setting expression to %s [%d %d] \n", 
                    s1, row, col);
         if (SUMA_DispExpr_To_EvalExpr(s1, evale, tight)) { 
            if (setmen) {
               SUMA_INSERT_CELL_STRING(TF, row, col, tight);
            } else { /* Just the table field */
               if (TF->str_value) { 
                  SUMA_STRING_REPLACE(TF->str_value[col*TF->Ni+row], tight);
               }
            }
         } else { /* failed, reset string */
            SUMA_INSERT_CELL_STRING(TF, row, col, 
                                    TF->str_value[col*TF->Ni+row]);
            SUMA_RETURN(NOPE);
         }
         break;
      default:
         SUMA_SL_Err("You make me sick");
         break;
   }
   
   SUMA_NEW_MASKSTATE();
   
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

int SUMA_SetMaskLenTableValueNew(  int row, int col,
                                float v, 
                                int setmen, 
                                int redisplay, 
                                SUMA_NUMERICAL_UNITS num_units) 
{
   static char FuncName[]={"SUMA_SetMaskLenTableValueNew"};
   int NewDisp=0, isCur=0, Err=0, init_row=0;
   SUMA_MaskDO *mdo=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_TABLE_FIELD *TF=NULL;
   float *fv;
   SUMA_ALL_DO *ado=NULL;
   char evale[256]={""}, tight[128]={""};
   char str[256];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) {
      SUMA_LH("Called on cell[%d %d] with %f", row, col, v);
      SUMA_DUMP_TRACE("Who called SUMA_SetMaskLenTableValueNew?");
   }
   
   if (!(SurfCont=SUMAg_CF->X->AllMaskCont)) {
      SUMA_RETURN(0);
   }
   
      
   TF = SurfCont->MaskLenTable;
   if (!TF) setmen = 0; /* can't set nothing */
   
   if (num_units == SUMA_PERC_VALUE_UNITS) {
      SUMA_S_Err("No percentiles allowed here");
      SUMA_RETURN(NOPE);
   }
   
   if (row < 0) {
      SUMA_S_Err("What is for row %d < 0?", row); SUMA_RETURN(NOPE);
   }
   if (col < 1) {
      SUMA_S_Err("What is for col %d < 1?", col); SUMA_RETURN(NOPE);
   }
   NewDisp = NOPE;
   
   /* What are we dealing with ? */
   switch (col) {
      case 1:  
         SUMA_LHv("Setting min to %f [%d %d] (%f)\n", 
                   v, row, col, SurfCont->tract_length_mask[1]);
         if (v>=0.0 && v<=2000) {
            if (v > SurfCont->tract_length_mask[1]-1)
                                 v = SurfCont->tract_length_mask[1]-1;
            if (SurfCont->tract_length_mask[0] == v) {
               SUMA_BEEP;
               /* floor, get out */
               SUMA_RETURN(YUP);
            }
            SurfCont->tract_length_mask[0] = v;
            if (setmen) {
               SUMA_INSERT_CELL_VALUE(TF, 0, col, 
                                      SurfCont->tract_length_mask[0]);
            }
         } else { /* failed, reset string */
            SUMA_BEEP;
            SUMA_INSERT_CELL_VALUE(TF, 0, col, SurfCont->tract_length_mask[0]);
            SUMA_RETURN(NOPE);
         }
         break;
      case 2:
         SUMA_LHv("Setting max to %f [%d %d] (%f)\n", 
                   v, row, col, SurfCont->tract_length_mask[0]);
         if (v>=0.0 && v<=2000) { 
            if (v < SurfCont->tract_length_mask[0]+1)
                                 v = SurfCont->tract_length_mask[0]+1;
            if (SurfCont->tract_length_mask[1] == v) {
               SUMA_BEEP;
               /* ceiling, get out */
               SUMA_RETURN(YUP);
            }
            SurfCont->tract_length_mask[1] = v;
            if (setmen) {
               SUMA_INSERT_CELL_VALUE(TF, 0, col, 
                                      SurfCont->tract_length_mask[1]);
            }
         } else { /* failed, reset string */
            SUMA_BEEP;
            SUMA_INSERT_CELL_VALUE(TF, 0, col, SurfCont->tract_length_mask[1]);
            SUMA_RETURN(NOPE);
         }
         break;
      default:
         SUMA_SL_Err("You make me sick");
         break;
   }
   
   if (0) { /* Should not need this */
      SUMA_NEW_MASKSTATE();
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
   char *tmp, sbuf[12], wname[64];
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
   if (hint || help) {
      snprintf(wname,63,"%s->lab", VrF->wname);
      SUMA_Register_Widget_Help(VrF->lab, 1, wname, hint, help);
   }
   
   if (VrF->N_slice_num < 0) {
      VrF->N_slice_num = SUMA_VO_N_Slices((SUMA_VolumeObject *)ado, "Mx");
   }
   if (VrF->N_slice_num <= 0) VrF->N_slice_num = 150;
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

   if (hint || help) {
      snprintf(wname,63,"%s->Ns", VrF->wname);
      SUMA_Register_Widget_Help(VrF->text, 1, wname, hint, help);
   }
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

   /* Now for the view toggle button */
   VrF->tb = XtVaCreateManagedWidget("v", 
      xmToggleButtonWidgetClass, VrF->rc, NULL);
   XtAddCallback (VrF->tb, 
         XmNvalueChangedCallback, SUMA_cb_ShowVrF_toggled, ado);
   if (hint || help) {
      snprintf(wname,63,"%s->Ns->v", VrF->wname);
      SUMA_Register_Widget_Help(VrF->tb, 1, wname, 
                                "View (ON)/Hide VrF", 
                                SUMA_SurfContHelp_ShowVrFTgl);
   }

   SUMA_SET_SELECT_COLOR(VrF->tb);
   XmToggleButtonSetState (VrF->tb, VSaux->ShowVrSlc , NOPE);
   
   /* Now for the selectable toggle button */
   VrF->tbs = XtVaCreateManagedWidget("s", 
      xmToggleButtonWidgetClass, VrF->rc, NULL);
   XtAddCallback (VrF->tbs, 
         XmNvalueChangedCallback, SUMA_cb_VrSelect_toggled, ado);
   if (hint || help) {
      snprintf(wname,63,"%s->Ns->s", VrF->wname);
      SUMA_Register_Widget_Help(VrF->tbs, 1, wname, 
                                "Allow voxel selection on rendered volume (ON)", 
                                SUMA_SurfContHelp_VrSelectTgl);
   }

   SUMA_SET_SELECT_COLOR(VrF->tbs);
   XmToggleButtonSetState (VrF->tbs, VSaux->VrSelect , NOPE);
   
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

void SUMA_cb_VrSelect_toggled(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_VrSelect_toggled"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) { 
      SUMA_S_Warn("NULL input"); SUMA_RETURNe; }
      
   SUMA_SetShowSlice((SUMA_VolumeObject *)ado, "Sel", 
                      XmToggleButtonGetState (SurfCont->VR_fld->tbs));
   SUMA_RETURNe;
}

void SUMA_cb_VSliceAtXYZ_toggled(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_VSliceAtXYZ_toggled"};
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   ado = (SUMA_ALL_DO *)data;
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) { 
      SUMA_S_Warn("NULL input"); SUMA_RETURNe; }
      
   SUMA_SetShowSlice((SUMA_VolumeObject *)ado, "AtXYZ", 
                      XmToggleButtonGetState (SurfCont->VSliceAtXYZ_tb));
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
   \brief set polymode, if SurfCont is not null, also set related widget
   
   if delta != 0, then increment current transparency by delta.
                  When using delta, you should pass sv->TransMode
                  in i, for when the starting transparency is that of the viewer.
*/ 
SUMA_Boolean SUMA_Set_ADO_RenderMode(SUMA_ALL_DO *ado, int i, int delta,
                                    int update_widgets ) 
{
   static char FuncName[]={"SUMA_Set_ADO_RenderMode"};
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ado) SUMA_RETURN(NOPE);
   if (update_widgets) SurfCont = SUMA_ADO_Cont(ado);
   
   switch (ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO = (SUMA_SurfaceObject *)ado;
         if (delta) {
            if (SO->PolyMode == SRM_ViewerDefault) {
               /* ambiguous case, start at i */
               SO->PolyMode = i;
            }
            if (delta < 0) {
               i = ((SO->PolyMode-delta) % SRM_N_RenderModes);
               if (i <= SRM_ViewerDefault) i = SRM_Fill;
            } else if (delta > 0) {
               i = ((SO->PolyMode-delta) % (SRM_N_RenderModes));
               if (i <= SRM_ViewerDefault) i = SRM_Points;
            }
         }
         SO->PolyMode = (i % SRM_N_RenderModes);
         if (SO->PolyMode <= SRM_ViewerDefault) SO->PolyMode = SRM_Fill; 
         if (SurfCont && SurfCont->RenderModeMenu) { /* also set widgets */
             SUMA_Set_Menu_Widget( SurfCont->RenderModeMenu, 
                        SUMA_RenderMode2RenderModeMenuItem(SO->PolyMode+1));
         }
         break; }
      case VO_type: {
         SUMA_VolumeObject *VO = (SUMA_VolumeObject *)ado;
         SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
         if (!VSaux) SUMA_RETURN(NOPE);
         SUMA_LH("What to do for VO polymode %s (%s)?", 
                    ADO_LABEL(ado), ADO_TNAME(ado));
         break; }
      default: 
         SUMA_S_Err("Not ready for %s (%s)", ADO_LABEL(ado), ADO_TNAME(ado));
         break;
   }
   
   SUMA_RETURN(YUP);
}

/*!
   \brief set transmode, if SurfCont is not null, also set related widget
   
   if delta != 0, then increment current transparency by delta.
                  When using delta, you should pass sv->TransMode
                  in i, for when the starting transparency is that of the viewer.
*/ 
SUMA_Boolean SUMA_Set_ADO_TransMode(SUMA_ALL_DO *ado, int i, int delta,
                                    int update_widgets ) 
{
   static char FuncName[]={"SUMA_Set_ADO_TransMode"};
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_ENTRY;
   
   if (!ado) SUMA_RETURN(NOPE);
   if (update_widgets) SurfCont = SUMA_ADO_Cont(ado);
   
   switch (ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO = (SUMA_SurfaceObject *)ado;
         if (delta) {
            if (SO->TransMode == STM_ViewerDefault) {
               /* ambiguous case, start at i */
               SO->TransMode = i;
            }
            if (delta < 0) {
               i = ((SO->TransMode-delta) % (STM_N_TransModes-2));
               if (i <= STM_ViewerDefault) i = STM_16;
            } else if (delta > 0) {
               i = ((SO->TransMode-delta) % (STM_N_TransModes-2));
               if (i <= STM_ViewerDefault) i = STM_0;
            }
         }
         if (i < 0 || i >= STM_N_TransModes) { 
            SO->TransMode = STM_ViewerDefault;
         } else { SO->TransMode = i; }
         if (SO->TransMode == STM_16) { SO->Show = NOPE; } 
         else { SO->Show = YUP; } 
         if (SurfCont && SurfCont->TransModeMenu) { /* also set widgets */
            SUMA_Set_Menu_Widget(SurfCont->TransModeMenu,
                              SUMA_TransMode2TransModeMenuItem(SO->TransMode+1));
         }
         break; }
      case VO_type: {
         SUMA_VolumeObject *VO = (SUMA_VolumeObject *)ado;
         SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
         if (!VSaux) SUMA_RETURN(NOPE);
         if (delta) {
            if (VSaux->TransMode == SATM_ViewerDefault) {
               /* ambiguous case, start at i */
               VSaux->TransMode = SUMA_TransMode2ATransMode(i);
            }
            if (delta < 0) {
               i = ((VSaux->TransMode-delta) % (SATM_N_TransModes-2));
               if (i <= SATM_ViewerDefault) i = SATM_16;
            } else if (delta > 0) {
               i = ((VSaux->TransMode-delta) % (SATM_N_TransModes-2));
               if (i <= SATM_ViewerDefault) i = SATM_0;
            }
         }
         if (i < 0 || i >= SATM_N_TransModes) { 
            VSaux->TransMode = SATM_ViewerDefault;
         } else { VSaux->TransMode = i; }
         if (VSaux->TransMode == SATM_16) { VO->Show = NOPE; } 
         else { VO->Show = YUP; } 
         if (SurfCont && SurfCont->VTransModeMenu) { /* also set widgets */
            SUMA_Set_Menu_Widget(SurfCont->VTransModeMenu,
                         SUMA_ATransMode2ATransModeMenuItem(VSaux->TransMode+1));
         }
         break; }
      default: 
         SUMA_S_Err("Not ready for %s (%s)", ADO_LABEL(ado), ADO_TNAME(ado));
         break;
   }
   
   SUMA_RETURN(YUP);
}

int SUMA_Get_ADO_TransMode(SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_Get_ADO_TransMode"};
   
   SUMA_ENTRY;
   
   if (!ado) SUMA_RETURN(STM_ViewerDefault);
   
   switch (ado->do_type) {
      case SO_type: {
         SUMA_SurfaceObject *SO = (SUMA_SurfaceObject *)ado;
         SUMA_RETURN((int)SO->TransMode); 
         break; }
      case VO_type: {
         SUMA_VolumeObject *VO = (SUMA_VolumeObject *)ado;
         SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
         if (!VSaux) SUMA_RETURN(NOPE);
         SUMA_RETURN((int)VSaux->TransMode);
         break; }
      default: 
         SUMA_S_Err("Not ready for %s (%s)", ADO_LABEL(ado), ADO_TNAME(ado));
         break;
   }
   
   SUMA_RETURN(STM_ViewerDefault);
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

/*!
   Load Masks
*/
void SUMA_cb_Masks_Load(Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_Masks_Load"};
   SUMA_LIST_WIDGET *LW=NULL;
   DList *list = NULL;
   SUMA_EngineData *ED = NULL;
   DListElmt *NextElm = NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   SUMA_LH("Called");
      
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_OpenMaskFileSelection);
   if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_vp, (void *)data,
                                          SES_Suma, NULL, NOPE,
                                          SEI_Head, NULL))) {
      fprintf (SUMA_STDERR, 
         "Error %s: Failed to register command.\n", FuncName);
   }
   if (!SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_ip, (int *)w,
                                          SES_Suma, NULL, NOPE,
                                          SEI_In, NextElm)) {
      fprintf (SUMA_STDERR, 
         "Error %s: Failed to register command.\n", FuncName);
   }
   
   if (!SUMA_Engine (&list)) {
      fprintf(SUMA_STDERR, 
         "Error %s: SUMA_Engine call failed.\n", FuncName);
   }
   
   SUMA_RETURNe;
}

/*!
   \brief Save the masks to disk
*/
void SUMA_cb_Masks_Save (Widget w, XtPointer data, XtPointer client_data)
{
   static char FuncName[]={"SUMA_cb_Masks_Save"};
   SUMA_DRAWN_ROI *dROI=NULL;
   DList *list = NULL;
   SUMA_EngineData *ED = NULL;
   DListElmt *NextElm = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_SaveMaskFileSelection);
   if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_vp, (void *)data,
                                          SES_Suma, NULL, NOPE,
                                          SEI_Head, NULL))) {
      fprintf (SUMA_STDERR, 
         "Error %s: Failed to register command.\n", FuncName);
   }
   if (!SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_ip, (int *)w,
                                          SES_Suma, NULL, NOPE,
                                          SEI_In, NextElm)) {
      fprintf (SUMA_STDERR, 
         "Error %s: Failed to register command.\n", FuncName);
   }
   
   if (!SUMA_Engine (&list)) {
      fprintf(SUMA_STDERR, 
         "Error %s: SUMA_Engine call failed.\n", FuncName);
   }
 
   SUMA_RETURNe;
}

/*!
   \brief Loads masks into SUMA land
   
   \param dlg (SUMA_SELECTION_DIALOG_STRUCT *) struture from selection dialogue
*/
void SUMA_LoadMultiMasks (char *filename, void *data)
{
   static char FuncName[]={"SUMA_LoadMultiMasks"};
   
   SUMA_ENTRY;

   if (!filename) {
      SUMA_SLP_Err("Null filename"); 
      SUMA_RETURNe;
   }
      
   if (!SUMA_LoadMultiMasks_eng(filename, 1, 1)) {
      SUMA_SLP_Err("Failed loading, and processing masks"); 
      SUMA_RETURNe;
   }
   
   SUMA_RETURNe;
}

/*!
   Function that does the work of loading masks saved in a file 
*/
SUMA_Boolean SUMA_LoadMultiMasks_eng (char *filename, 
                              int SetupTable, 
                              int LaunchDisplay)
{
   static char FuncName[]={"SUMA_LoadMultiMasks_eng"};
   SUMA_DSET_FORMAT form;
   char *fname = NULL, *att=NULL;
   SUMA_Boolean ans = NOPE;
   NI_stream ns;
   int ip, good, ido, an;
   SUMA_MaskDO *mdo=NULL;
   NI_group *NIcont=NULL, *ngr=NULL;
   NI_element *nel=NULL;
   DList *list = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;

   if (!filename) {
      SUMA_S_Err("Null filename"); 
      SUMA_RETURN(NOPE);
   }
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,
               "%s: Received request to load %s.\n", 
               FuncName, filename);
   }

   /* find out if file exists and how many values it contains */
   if (!SUMA_filexists(filename)) {
      SUMA_SLP_Err("File not found");
      SUMA_RETURN(NOPE);
   }

   /* take a stab at the format */
   form = SUMA_GuessFormatFromExtension(filename, NULL);
   
   /* Read the container group */
   fname = SUMA_append_replace_string("file:", filename, "", 0);
   if (!(ns = NI_stream_open(fname, "r"))) {
      SUMA_SLP_Err("Failed to open %s", fname);
      goto OUT;
   }
   
   /* read the whole thing */
   if (!(NIcont = NI_read_element(ns, 1))) {
      SUMA_SLP_Err("Failed to read element from %s", fname);
      goto OUT;  
   }
   if (NI_element_type(NIcont) != NI_GROUP_TYPE) {
      SUMA_SLP_Err("Failed to NI element %s not group type", fname);
      goto OUT;  
   }
   if (strcmp(NIcont->name, "MaskObjects")) {
      SUMA_SLP_Err("Unexpected NI name of %s, wanted %s", 
                   NIcont->name, "MaskObjects" );
      goto OUT;
   }
   
   /* extract content */
   good = 0; ido = -1;
   for (ip=0; ip<NIcont->part_num; ++ip) {
      switch( NIcont->part_typ[ip] ){
        case NI_GROUP_TYPE:
            ngr = (NI_group *)NIcont->part[ip] ;
            if (!strcmp(ngr->name, "Mask")) {
               /* wipe out existing masks with the same idcode */
               SUMA_DeleteMask(NI_get_attribute(ngr,"idcode_str"));
               if (!(mdo = SUMA_NIMDO_to_MDO(ngr))) {
                  SUMA_S_Err("Failed to translate mask for %s",
                              ngr->name);
                  break;
               }
               if (!SUMA_AccessorizeMDO(mdo)) {
                  SUMA_S_Err("No accessorizing");
                  SUMA_free_MaskDO(mdo); break;
               }
               /* Now add mdo into dov */
               SUMA_LH("Adding DO");
               if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, 
                               (void *)mdo, MASK_type, SUMA_WORLD)) {
                  SUMA_S_Err("Failed in SUMA_AddDO.");
                  SUMA_free_MaskDO(mdo); break;
               }
               ido = SUMAg_N_DOv-1;
               /* register DO with viewer */
               SUMA_LH("Registrar");
               if (!SUMA_RegisterDO(ido, NULL)) {
                  SUMA_S_Err("Failed in SUMA_RegisterDO.");
                  break;
               }
               ++good;
            } else {
               SUMA_S_Warn("Don't know what to make of %s",
                          ngr->name);
            }
            break ;
         case NI_ELEMENT_TYPE:
            nel = (NI_element *)NIcont->part[ip] ;
            SUMA_S_Warn("Don't know what to make of %s",
                          nel->name);
            break;
         default:
            break;
      }
   }

   if (good) {
      SurfCont=SUMAg_CF->X->AllMaskCont;
      if (NI_get_attribute(NIcont, "TractLength")) {
         NI_GET_FLOATv(NIcont, "TractLength", 
                       SurfCont->tract_length_mask, 2, LocalHead);
         NI_GET_INT(NIcont, "UseTractLength", SurfCont->UseMaskLen);
      }
      if (SurfCont->MaskEvalTable &&
          (att=NI_get_attribute(NIcont, "MaskEval"))) {
         an = SUMA_SetMaskEvalTableValueNew(0, 1, att,
                          1, 0, SurfCont->MaskEvalTable->num_units);
         if (an < 0) {
            SUMA_S_Err("Failed to set %s as mask expression",
                        att);
            SUMA_Set_UseMaskEval(0, 0, 1);
         } else {
            NI_GET_INT(NIcont, "UseMaskEval", SurfCont->UseMaskEval);
            SUMA_Set_UseMaskEval(SurfCont->UseMaskEval, 0, 1);
         }
      }
      if (SetupTable && SurfCont) {
         SUMA_InitMasksTable(SurfCont);
      }
      if (LaunchDisplay) {
         SUMA_NEW_MASKSTATE();
         /* redisplay */
         if (!list) list = SUMA_CreateList ();
         SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                            SES_Suma, NULL); 
         if (!SUMA_Engine(&list)) SUMA_SLP_Err("Failed to redisplay.");
      }
   }
   
   ans = YUP;
   
   OUT:
   if (ns) NI_stream_close(ns); ns=NULL;
   SUMA_ifree(fname);
   NI_free(NIcont); NIcont = NULL;
   
   SUMA_RETURN(ans);
}

/*!
   \brief Loads masks into SUMA land
   
   \param dlg (SUMA_SELECTION_DIALOG_STRUCT *) struture from selection dialogue
*/
void SUMA_SaveMultiMasks (char *filename, void *data)
{
   static char FuncName[]={"SUMA_SaveMultiMasks"};
   
   SUMA_ENTRY;

   if (!filename) {
      SUMA_SLP_Err("Null filename"); 
      SUMA_RETURNe;
   }
      
   if (!SUMA_SaveMultiMasks_eng(filename)) {
      SUMA_SLP_Err("Failed saving masks"); 
      SUMA_RETURNe;
   }
   
   SUMA_RETURNe;
}

/*!
   Function that does the work of loading masks saved in a file */
SUMA_Boolean SUMA_SaveMultiMasks_eng (char *filename)
{
   static char FuncName[]={"SUMA_SaveMultiMasks_eng"};
   SUMA_DSET_FORMAT form;
   SUMA_Boolean LocalHead = NOPE;
   DList *dl=NULL;
   DListElmt *el=NULL;
   int cnt;
   char *fname=NULL, *sss;
   SUMA_X_SurfCont *SurfCont=NULL;
   NI_stream ns;
   SUMA_MaskDO *mdo=NULL;
   NI_group *NIcont=NULL;
      
   SUMA_ENTRY;

   if (!filename) {
      SUMA_S_Err("Null data"); 
      SUMA_RETURN(NOPE);
   }
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,
               "%s: Received request to save %s .\n", 
               FuncName, filename);
   }

   /* find out if file exists and how many values it contains */
   fname = SUMA_Extension(filename, ".niml.mo", 0);
   if (SUMA_filexists(fname)) {
      if (SUMA_ForceUser_YesNo(SUMAg_SVv[0].X->TOPLEVEL, 
                                 "Overwrite existing file?", SUMA_YES, 
                                 SWP_DONT_CARE) != SUMA_YES) {
         SUMA_S_Note("File %s exists, user chose not to overwrite", fname);
         SUMA_ifree(fname);
         SUMA_RETURN(NOPE);
      }
   }
   fname = SUMA_append_replace_string("file:", fname, "", 2);
   if (!(ns = NI_stream_open(fname, "w"))) {
      SUMA_SLP_Err("Failed to open %s for writing", fname);
      SUMA_ifree(fname);
      SUMA_RETURN(NOPE);
   }  
   
   /* Check on all masks and save them */
   dl = SUMA_AssembleMasksList_inDOv(SUMAg_DOv, SUMAg_N_DOv, 0);
   if (!dl || dlist_size(dl) == 0) {
      SUMA_S_Note("No masks, nothing written");
      SUMA_RETURN(YUP);
   }
   
   NIcont = NI_new_group_element();
   NI_rename_group(NIcont, "MaskObjects");
   
   /* Some overall attributes */
   SurfCont=SUMAg_CF->X->AllMaskCont;
   
   sss = SUMA_GetMaskEvalExpr();
   if (sss[0] != '\0') {
      NI_set_attribute(NIcont,"MaskEval", sss);
      if (SurfCont) NI_SET_INT(NIcont,"UseMaskEval",SurfCont->UseMaskEval);
   }
      
   if (SurfCont) {
      NI_SET_FLOATv(NIcont, "TractLength", SurfCont->tract_length_mask, 2);
      NI_SET_INT(NIcont,"UseTractLength", SurfCont->UseMaskLen);
   }
   
   cnt = 0; el = NULL;
   do {
      if (!el) el = dlist_head(dl);
      else el = dlist_next(el);
      mdo = (SUMA_MaskDO *)el->data;
      if (!SUMA_MDO_to_NIMDO(mdo, NIcont)) {
         SUMA_S_Err("Failed to transform mdo %s, continuing",
                    ADO_LABEL((SUMA_ALL_DO *)mdo));
      }
      ++cnt;
   } while (el != dlist_tail(dl));

   /* Now write the beast */
   NI_write_element( ns , NIcont , NI_TEXT_MODE ) ;
   
   NI_stream_close(ns); ns=NULL;
   SUMA_ifree(fname);
   NI_free(NIcont); NIcont = NULL;
   dlist_destroy(dl);SUMA_free(dl);

   SUMA_RETURN(YUP);
}

/* 
   *************** Convolution functions *************** 
   based on example in glut's convolve.c by  
   Tom McReynolds, SGI 
   *****************************************************
*/

/* identity filter */
void SUMA_C_identity(SUMA_C_FILTER *mat)
{
  int n, size;
  size = mat->rows * mat->cols;

  mat->array[0] = 1.f;
  for(n = 1; n < size; n++)
    mat->array[n] = 0.f;

  mat->scale = 1.f;
  mat->bias = 0.f;
}


/* create a new filter with identity filter in it */
SUMA_C_FILTER * SUMA_C_newfilter(int rows, int cols)
{
  SUMA_C_FILTER *mat;

  mat = (SUMA_C_FILTER *)malloc(sizeof(SUMA_C_FILTER));
  mat->rows = rows;
  mat->cols = cols;
  mat->array = (GLfloat *)malloc(rows * cols * sizeof(GLfloat));
  SUMA_C_identity(mat);
  
  return(mat);
}

void SUMA_C_free(SUMA_C_FILTER *mat)
{
   if (!mat) return;
   if (mat->array) free(mat->array);
   free(mat);
   return;
}

/* doesn't re-initialize matrix */
void SUMA_C_resize(SUMA_C_FILTER *mat, int rows, int cols)
{
  if(mat->rows != rows ||
     mat->cols != cols) {
    mat->array = (GLfloat *)realloc(mat->array, rows * cols * sizeof(GLfloat));
  }
  mat->rows = rows;
  mat->cols = cols;
}


/* box filter blur */
void SUMA_C_box(SUMA_C_FILTER *mat)
{
  int n, count;
  GLfloat blur;

  count = mat->cols * mat->rows;
  blur = 1.f/count;
  for(n = 0; n < count; n++)
     mat->array[n] = blur;

  mat->scale = 1.f;
  mat->bias = 0.f;
}

/* sobel filter */
void SUMA_C_sobel(SUMA_C_FILTER *mat)
{
  static GLfloat sobel[] = {-.5f, 0.f, .5f,
                            -1.f, 0.f, 1.f,
                            -.5f, 0.f, .5f};

  /* sobel is fixed size */
  SUMA_C_resize(mat, 3, 3); /* will do nothing if size is right already */
  
  memcpy(mat->array, sobel, sizeof(sobel));

  mat->scale = 2.f;
  mat->bias = 0.f;
}

/* laplacian filter */
void SUMA_C_laplace(SUMA_C_FILTER *mat)
{
  static GLfloat laplace[] = {  0.f, -.25f,   0.f,
                              -.25f,   1.f, -.25f,
                                0.f, -.25f,   0.f};

  /* sobel is fixed size */
  SUMA_C_resize(mat, 3, 3); /* will do nothing if size is right already */
  
  memcpy(mat->array, laplace, sizeof(laplace));

  mat->scale = 4.f;
  mat->bias = .125f;
}

void SUMA_C_convolve(SUMA_SurfaceViewer *csv, SUMA_DO *dov, SUMA_C_FILTER *mat)
{
  int i, j;
  int imax, jmax;

  imax = mat->cols;
  jmax = mat->rows;
  for(j = 0; j < jmax; j++) {
      for(i = 0; i < imax; i++) {
        glViewport(-i, -j, csv->X->aWIDTH - i, csv->X->aHEIGHT - j);
        SUMA_display_one(csv, dov);
        glAccum(GL_ACCUM, mat->array[i + j * imax]);
      }
  }
  if (jmax > 0 && imax > 0) {
   glViewport(0, 0, csv->X->aWIDTH, csv->X->aHEIGHT);
  }
}


/* *************** End Convolution utilities *************** */

/*! Based on the venerable MCW_click_webhelp_CB() */
void SUMA_click_webhelp_CB(Widget w, XtPointer data, 
                                     XtPointer callData)
{
   static char FuncName[] = {"SUMA_click_webhelp_CB"};
   Widget whelp, winit ;
   int mx = 0;
   XmAnyCallbackStruct cbs ;
   XEvent ev ;
   char *wlabel=NULL;
   GUI_WIDGET_HELP *gwh=NULL;
   static Cursor cur = 0 ;
   Display *dis = XtDisplay(w) ;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   

   if( cur == 0 ) cur = XCreateFontCursor( dis , XC_coffee_mug ) ;

#ifdef USE_LOCATE  /* old version */
   whelp = XmTrackingLocate( w , cur , False ) ; /* wait for user to click */
#else
   cbs.event = &ev ;
   whelp = XmTrackingEvent( w , cur , False , cbs.event ) ;
#endif

   winit = whelp;
   if( whelp != NULL) {
      SUMA_LH("Eins on %s", XtName(whelp));
      if (!(gwh = SUMA_Get_Widget_Help( whelp ))) {
         SUMA_LH("No help on widget (%s) trying parents...", 
                     XtName(whelp));
      }
      mx = 0;
      while (mx < 5 && (whelp = XtParent(whelp)) && !gwh) {
         SUMA_LH("Seeking fortune with %s...", XtName(whelp));
         gwh = SUMA_Get_Widget_Help( whelp );
         ++mx;
      }
      if (!gwh) {
         SUMA_S_Note("Could not find web help where you clicked (%s)."
                     "Try again in vicinity.", XtName(winit));
         SUMA_RETURNe;                                   
      }
   } else {
      XBell( dis , 100 ) ;
      SUMA_RETURNe;    
   }
   
      
   /* Go from name to permalink  */
   wlabel = SUMA_gsf(SUMA_Name_GUI_Help(gwh), WEB, NULL, NULL);
   whereami_browser(wlabel);
   SUMA_RETURNe;
}



SUMA_Boolean SUMA_wait_till_visible(Widget w, int maxms) 
{
   static char FuncName[]={"SUMA_wait_till_visible"};
   int k, del=100, vis=0;
   
   SUMA_ENTRY;
   
   if (!w) SUMA_RETURN(NOPE);
   
   if (0 && !XtIsManaged(w)) {/* possible to return 0 because of asychrony */
      SUMA_S_Err("Widget not managed");
      SUMA_RETURN(NOPE);
   }
   if (!XtIsRealized(w)) {
      SUMA_S_Err("Widget not realized");
      SUMA_RETURN(NOPE);
   }
   
   if (MCW_widget_visible(w)) SUMA_RETURN(YUP);
   if (maxms < 0) maxms = 10000;
   k = 0;
   while ( !(vis=MCW_widget_visible(w)) && (k < maxms) ) {
      fprintf(stderr,".");
      if (k == 0) {
         /* try to hurry things along */
         XtPopup(w, XtGrabNone);
         XmUpdateDisplay(w ) ;
         XSync(XtDisplay(w), 0);
      }
      NI_sleep(del); k += del;     
   }
   if (k>0) fprintf(stderr,"\n");
   
   SUMA_RETURN((SUMA_Boolean)vis);
}
