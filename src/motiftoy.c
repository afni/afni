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

/* ************ Begin Menubar Additions ***************** */
typedef struct _menu_item 
{
	char              *label;         /* the label for the item */
	WidgetClass       *class;         /* pushbutton, label, separator... */
	char               mnemonic;      /* mnemonic; NULL if none */
	char              *accelerator;   /* accelerator; NULL if none */
	char              *accel_text;    /* to be converted to compound string */
	void             (*callback)();   /* routine to call; NULL if none */
	XtPointer          callback_data; /* client_data for callback() */
	struct _menu_item *subitems;      /* pullright menu items, if not NULL */
} MenuItem;

/* Build popup, option and pulldown menus, depending on the menu_type.
** It may be XmMENU_PULLDOWN, XmMENU_OPTION or XmMENU_POPUP. Pulldowns
** return the CascadeButton that pops up the menu. Popups return the menu.
** Option menus are created, but the RowColumn that acts as the option
** "area" is returned unmanaged. (The user must manage it.) 
** Pulldown menus are built from cascade buttons, so this function 
** also builds pullright menus. The function also adds the right 
** callback for PushButton or ToggleButton menu items.
*/
Widget BuildMenu (Widget parent, int menu_type, char *menu_title, char menu_mnemonic, 
                  Boolean tear_off, MenuItem *items)
{
   Widget app_wid, top_wid, button, option_menu, rc;
   XtAppContext  app;
   XmString       draw_shape, line, square, circle;
	Widget   menu, cascade, widget;
	int      i;
	XmString str;
	Arg      args[4];
	int      n ;

	if (menu_type == XmMENU_PULLDOWN || menu_type == XmMENU_OPTION)
		menu = XmCreatePulldownMenu (parent, "_pulldown", NULL, 0);
	else if (menu_type == XmMENU_POPUP) {
		n = 0 ;
		XtSetArg (args[n], XmNpopupEnabled, XmPOPUP_AUTOMATIC_RECURSIVE); n++;
		menu = XmCreatePopupMenu (parent, "_popup", args, n);
	}
	else {
		XtWarning ("Invalid menu type passed to BuildMenu()");
		return NULL;
	}
	
	if (tear_off)
		XtVaSetValues (menu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);
		
	/* Pulldown menus require a cascade button to be made */
	if (menu_type == XmMENU_PULLDOWN) {
		str = XmStringCreateLocalized (menu_title);
		n = 0;
		XtSetArg (args[n], XmNsubMenuId, menu); n++;
		XtSetArg (args[n], XmNlabelString, str); n++;
		XtSetArg (args[n], XmNmnemonic, menu_mnemonic); n++;
		cascade = XmCreateCascadeButtonGadget (parent, menu_title, args, n);
		XtManageChild (cascade);
		XmStringFree (str);
	}
	else if (menu_type == XmMENU_OPTION) {
		/* Option menus are a special case, but not hard to handle */
		str = XmStringCreateLocalized (menu_title);
		n = 0;
		XtSetArg (args[n], XmNsubMenuId, menu); n++;
		XtSetArg (args[n], XmNlabelString, str); n++;
		
		/* This really isn't a cascade, but this is the widget handle
		** we're going to return at the end of the function.
		*/
		cascade = XmCreateOptionMenu (parent, menu_title, args, n);
		XmStringFree (str);
	}
	
	/* Now add the menu items */
	
	for (i = 0; items[i].label != NULL; i++) {
		/* If subitems exist, create the pull-right menu by calling this
		** function recursively. Since the function returns a cascade
		** button, the widget returned is used..
		*/
		
		if (items[i].subitems) {
			if (menu_type == XmMENU_OPTION) {
				XtWarning ("You can't have submenus from option menu items.");
				continue;
			}
			else {
				widget = BuildMenu (menu, XmMENU_PULLDOWN, items[i].label, items[i].mnemonic, 
						    tear_off, items[i].subitems);
			}
		}
		else {
			widget = XtVaCreateManagedWidget (items[i].label, *items[i].class, menu, NULL);
		}

		/* Whether the item is a real item or a cascade button with a
		** menu, it can still have a mnemonic. 
		*/
		
		if (items[i].mnemonic) 
			XtVaSetValues (widget, XmNmnemonic, items[i].mnemonic, NULL);
			
		/* any item can have an accelerator, except cascade menus. But,
		** we don't worry about that; we know better in our declarations.
		*/
		
		if (items[i].accelerator) {
			str = XmStringCreateLocalized (items[i].accel_text);
			XtVaSetValues (widget, XmNaccelerator, items[i].accelerator, XmNacceleratorText, str, NULL);
			XmStringFree (str);
		}
		if (items[i].callback) {
			String resource ;

			if (XmIsToggleButton(widget) || XmIsToggleButtonGadget(widget))
				resource = XmNvalueChangedCallback ;
			else
				resource = XmNactivateCallback ;

			XtAddCallback (widget, resource, items[i].callback, (XtPointer) items[i].callback_data);
		}
	}
	
      /* Simulating AFNI crash situation*/
      rc = XmCreateRowColumn (menu, "rowcol", NULL, 0);
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
   	
	/* for popup menus, just return the menu; pulldown menus, return
	** the cascade button; option menus, return the thing returned
	** from XmCreateOptionMenu(). This isn't a menu, or a cascade button!
	*/
		
	return (menu_type == XmMENU_POPUP ? menu : cascade) ;
}

/* callback functions for menu items declared later... */
void set_weight (Widget widget, XtPointer client_data, XtPointer call_data)
{
	int weight = (int) client_data;
	printf ("Setting line weight to %d\n", weight);
}

void set_color (Widget widget, XtPointer client_data, XtPointer call_data)
{
	char *color = (char *) client_data;
	printf ("Setting color to %s\n", color);
}

void set_dot_dash (Widget widget, XtPointer client_data, XtPointer call_data)
{
	int dot_or_dash = (int) client_data;
	printf ("Setting line style to %s\n", dot_or_dash? "dot" : "dash");
}

MenuItem weight_menu[] = {
	{ "1", &xmPushButtonGadgetClass, '1', NULL, NULL, set_weight, (XtPointer) 1, (MenuItem *) NULL },
	{ "2", &xmPushButtonGadgetClass, '2', NULL, NULL, set_weight, (XtPointer) 2, (MenuItem *) NULL },
	{ "3", &xmPushButtonGadgetClass, '3', NULL, NULL, set_weight, (XtPointer) 3, (MenuItem *) NULL },
	{ "4", &xmPushButtonGadgetClass, '4', NULL, NULL, set_weight, (XtPointer) 4, (MenuItem *) NULL },
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL }
};

MenuItem color_menu[] = {
	{ "Cyan", &xmPushButtonGadgetClass, 'C', "Alt<Key>C", "Alt+C", set_color,
		(XtPointer) "cyan", (MenuItem *) NULL },
	{ "Yellow", &xmPushButtonGadgetClass, 'Y', "Alt<Key>Y", "Alt+Y", set_color,
		(XtPointer) "yellow", (MenuItem *) NULL },
	{ "Magenta", &xmPushButtonGadgetClass, 'M', "Alt<Key>M", "Alt+M", set_color,
		(XtPointer) "magenta", (MenuItem *) NULL },
	{ "Black", &xmPushButtonGadgetClass, 'B', "Alt<Key>B", "Alt+B", set_color,
		(XtPointer) "black", (MenuItem *) NULL }, 
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL }
};

MenuItem style_menu[] = {
	{ "Dash", &xmPushButtonGadgetClass, 'D', NULL, NULL, set_dot_dash, (XtPointer) 0, (MenuItem *) NULL },
	{ "Dot", &xmPushButtonGadgetClass, 'o', NULL, NULL, set_dot_dash, (XtPointer) 1, (MenuItem *) NULL },
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL }
};

MenuItem drawing_shapes[] = {
	{ "Lines", &xmPushButtonGadgetClass, 'L', NULL, NULL, 0, 0, NULL },
	{ "Circles", &xmPushButtonGadgetClass, 'C', NULL, NULL, 0, 0, NULL },
	{ "Squares", &xmPushButtonGadgetClass, 'S', NULL, NULL, 0, 0, NULL },
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL }
};

MenuItem drawing_menus[] = {
	{ "Line Weight", &xmCascadeButtonGadgetClass, 'W', NULL, NULL, 0, 0, weight_menu },
	{ "Line Color", &xmCascadeButtonGadgetClass, 'C', NULL, NULL, 0, 0, color_menu },
	{ "Line Style", &xmCascadeButtonGadgetClass, 'S', NULL, NULL, 0, 0, style_menu },
   { "Shapes", &xmCascadeButtonGadgetClass, 'V', NULL, NULL, 0, 0, drawing_shapes},
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL }
};

/* ************* End Menu bar additions  ******************* */
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
      
      new_MCW_optmenu_64fix(  top_wid, "Hell", 0, NUM_LIST_MODES-1, 0, 0, 
                        pushed_fn, NULL, MCW_av_substring_CB2, list_modes); 
      
   }
   {/* menubar bug */
	   Widget menubar, pdm;
      menubar = XmCreateMenuBar (top_wid, "menubar", NULL, 0);
	   pdm = BuildMenu   (menubar, XmMENU_PULLDOWN, "Lines", 
                           'L', True, drawing_menus);
	   XtManageChild (menubar);
 
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
