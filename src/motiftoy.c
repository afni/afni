#include "afni.h" 

/* Code to reproduce lesstif problem on 64 bit machines
Vanilla flavor chunk is based on textbook example: http://www.ist.co.uk/motif/books/vol6A/ch-19.fm.html
Example 19-4, program simple_option.c
*/
/* Prototype Callback function */
#define NUM_LIST_MODES 2
static char *list_modes[NUM_LIST_MODES] = { "Multiple" , "Extended" } ;
char * MCW_av_substring_CB2( MCW_arrowval *av , XtPointer cd )
{
   char **str = (char **) cd ;
   return str[av->ival] ;
}

void pushed_fn(Widget , XtPointer , 
               XmPushButtonCallbackStruct *);

void option_cb (Widget menu_item, XtPointer client_data,
                XtPointer call_data)
{
    int item_no = (int) client_data;
    puts (XtName (menu_item));
}

main(int argc, char **argv) 

{   
   Widget app_wid, top_wid, button, option_menu, rc;
   XtAppContext  app;
   XmString       draw_shape, line, square, circle;

   app_wid = XtVaAppInitialize(&app, "Push", NULL, 0,
     &argc, argv, NULL, NULL);

   top_wid = XmCreateRowColumn (app_wid, "rowcol", NULL, 0);
     
   { /* Add the dreaded function */      
      new_MCW_optmenu_orig(  top_wid, "Hell", 0, NUM_LIST_MODES-1, 0, 0, 
                        pushed_fn, NULL, MCW_av_substring_CB2, list_modes); 
   } 
   { /* try a vanilla flavor */
      rc = XmCreateRowColumn (top_wid, "rowcol", NULL, 0);
      draw_shape = XmStringCreateLocalized ("Draw Mode:");
      line = XmStringCreateLocalized ("Line");
      square = XmStringCreateLocalized ("Square");
      circle = XmStringCreateLocalized ("Circle");
      option_menu = XmVaCreateSimpleOptionMenu (rc,
                                "option_menu", draw_shape, 'D', 
                                0 /*initial menu selection*/, option_cb,
                                XmVaPUSHBUTTON, line, 'L', NULL, NULL,
                                XmVaPUSHBUTTON, square, 'S', NULL, NULL,
                                XmVaPUSHBUTTON, circle, 'C', NULL, NULL,
                                NULL);
      XmStringFree (line);
      XmStringFree (square);
      XmStringFree (circle);
      XmStringFree (draw_shape);
      XtManageChild (option_menu);
      XtManageChild (rc);
   }
   {  /* fudge  it? */      
      rc = XtVaCreateWidget ("rowcolumn",
         xmRowColumnWidgetClass, top_wid,
         XmNpacking, XmPACK_TIGHT, 
         XmNorientation , XmHORIZONTAL ,
         XmNmarginHeight, 0 ,
         XmNmarginWidth , 0 ,
         NULL);

      XtVaCreateManagedWidget ("Hell", 
                               xmLabelWidgetClass, rc,
                               XmNmarginHeight, 0 ,
                               XmNmarginWidth , 0 ,
                               NULL);

      new_MCW_optmenu_orig(  rc, "", 0, NUM_LIST_MODES-1, 0, 0, 
                        pushed_fn, NULL, MCW_av_substring_CB2, list_modes); 
      XtManageChild (rc);
   }
   {  /* fix ? */
      
      newz_MCW_optmenu(  top_wid, "Hell", 0, NUM_LIST_MODES-1, 0, 0, 
                        pushed_fn, NULL, MCW_av_substring_CB2, list_modes); 
      
   }
   
   XtManageChild (top_wid);
   XtRealizeWidget(app_wid); /* display widget hierarchy */
   XtAppMainLoop(app); /* enter processing loop */ 

}

void pushed_fn(Widget w, XtPointer client_data, 
               XmPushButtonCallbackStruct *cbs) 
  {   
     printf("Don't Push Me!!\n");
  }
