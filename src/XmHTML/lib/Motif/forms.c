#ifndef production
static char rcsId[]="$Header$";
#endif
/*****
* forms.c : XmHTML form support
*
* This file Version	$Revision$
*
* Creation date:		Thu Apr  3 16:06:10 GMT+0100 1997
* Last modification: 	$Date$
* By:					$Author$
* Current State:		$State$
*
* Author:				newt
*
* Copyright (C) 1994-1997 by Ripley Software Development 
* All Rights Reserved
*
* This file is part of the XmHTML Widget Library.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Library General Public
* License as published by the Free Software Foundation; either
* version 2 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Library General Public License for more details.
*
* You should have received a copy of the GNU Library General Public
* License along with this library; if not, write to the Free
* Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*****/
/*****
* ChangeLog 
* $Log$
* Revision 1.1  2011/06/30 16:10:30  rwcox
* Cadd
*
* Revision 1.9  1998/04/27 06:59:30  newt
* tka stuff
*
* Revision 1.8  1998/04/05 04:17:26  newt
* typo in input_tokens: check instead of checkbox...
*
* Revision 1.7  1998/04/04 06:28:09  newt
* XmHTML Beta 1.1.3
*
* Revision 1.6  1997/10/23 00:25:00  newt
* XmHTML Beta 1.1.0 release
*
* Revision 1.5  1997/08/31 17:35:00  newt
* Fixed translation table memory leak, translation tables are now only
* parsed once and then reused.
*
* Revision 1.4  1997/08/30 01:00:46  newt
* *Lots* of changes. Every HTML form component is now supported.
* Traversal and form reset more-or-less works.
*
* Revision 1.3  1997/08/01 13:00:42  newt
* Much enhancements: all elements except <textarea> are now supported.
*
* Revision 1.2  1997/05/28 01:47:30  newt
* ?
*
* Revision 1.1  1997/04/29 14:19:34  newt
* Initial Revision
*
*****/ 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* motif includes */
#include <Xm/DrawnB.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>

#include "toolkit.h"
#include XmHTMLPrivateHeader

/*** External Function Prototype Declarations ***/

/*** Public Variable Declarations ***/

/*** Private Datatype Declarations ****/

/*** Private Function Prototype Declarations ****/
static componentType getInputType(String attributes);
static void freeForm(XmHTMLForm *entry, Boolean being_destroyed);
static void passwdCB(Widget w, XtPointer client_data, XtPointer call_data);
static void buttonActivateCB(Widget w, XtPointer client_data, XtPointer call_data);
static void radioChangedCB(Widget w, XtPointer client_data, XtPointer call_data);
static void fileOkCB(Widget w, XtPointer client_data, XtPointer call_data);
static void fileActivateCB(Widget w, XtPointer client_data, XtPointer call_data);
static void optionMenuCB(Widget w, XtPointer client_data, XtPointer call_data);
static void finalizeEntry(XmHTMLWidget html, XmHTMLForm *entry, Boolean insert,
	Boolean manage);
static int formCountComponents(XmHTMLForm *parent, XmHTMLForm *comp);
static Widget getNextLeader(XmHTMLFormData *curr_group, int *y_pos);
static Widget getPrevLeader(XmHTMLFormData *curr_group, int *y_pos);
static Widget getPrevTab(XmHTMLForm *curr_tab, int *y_pos);
static Widget getNextTab(XmHTMLForm *curr_tab, Boolean start_at_current,
	int *y_pos);

/*** Private Variable Declarations ***/
#define MY_FONTLIST_DEFAULT_TAG "XmHTMLDefaultFontList"

/* scratch stuff */
static XmHTMLFormData *current_form;
static XmHTMLForm *current_entry;
static XmFontList my_fontList;
static Arg args[20];
static Dimension argc;

/*****
* We override the following translations, we handle navigation accross form
* components ourselves.
******/
static char trav_translations[] =
"~Shift ~Meta ~Ctrl <Key>Tab: traverse-next()\n\
Shift ~Meta ~Ctrl <Key>Tab: traverse-prev()\n\
Ctrl Shift <Key>Tab: traverse-next-or-prev(1)\n\
Ctrl <Key>Tab: traverse-next-or-prev(0)\n\
Shift <Key>Tab: traverse-prev()\n\
<Key>Tab: traverse-next()";

/*****
* Textfield translations
*****/
static char textF_translations[] =
"#override \n\
~Meta ~Alt <Key>Tab: traverse-next()\n\
<Key>Tab: traverse-next()\n\
<Key>Return: traverse-next()\n\
<Key>Linefeed: traverse-next()\n\
<Btn1Down>: grab-focus() traverse-current()";

/*****
* PushButton and toggleButton translations. We want it them to take focus
* when pressed.
*****/
static char pushB_translations[] =
"<Btn1Down>: Arm() traverse-current()";

static XtTranslations textFTranslations = NULL;
static XtTranslations travTranslations = NULL;
static XtTranslations pushBTranslations = NULL;

/* all possible <INPUT> types (12 in total) */
static String input_tokens[] = {"checkbox", "file", "hidden", "image",
	"option", "passwd", "radio", "reset", "select", "submit", "text",
	"textarea", "zzz"};

/*****
* Name: 		getInputType
* Return Type: 	componentType
* Description: 	retrieves the type of an <input> HTML form member.
* In: 
*	attrib..:	attributes to check
* Returns:
*	componenttype if ``type'' is present in attributes. FORM_TEXT is
*	returned if type is not present or type is invalid/misspelled.
*****/
static componentType
getInputType(String attributes)
{
	String chPtr;
	componentType ret_val = FORM_UNKNOWN;

	/* if type isn't specified we default to a textfield */
	if((chPtr = _XmHTMLTagGetValue(attributes, "type")) == NULL)
		return(FORM_TEXT);

	ret_val = (componentType)stringToToken(chPtr, input_tokens,
								(int)FORM_UNKNOWN);
	free(chPtr);
	return(ret_val == FORM_UNKNOWN ? FORM_TEXT : ret_val);
}

/*****
* Name: 		freeForm
* Return Type: 	void
* Description: 	releases all memory occupied by the given form component.
* In: 
*	entry:		form component to be released;
*	being_de..: True if the parent HTML widget is being destroyed, in 
*				which case don't destroy the widgets as they've already been
*				destroyed by the time this is called via the
*				DestroyCallback -- fix 15/12/97-01, offer
* Returns:
*	nothing.
* Background:
*	when the parent HTML widget is being destroyed, the call to XtMoveWidget
*	triggers a call to XtConfigureWidget which in turn triggers a call to
*	XConfigureWindow resulting in a BadWindow as the Window ID already
*	has become invalid.
*****/
static void
freeForm(XmHTMLForm *entry, Boolean being_destroyed)
{
	XmHTMLForm *tmp;

	while(entry != NULL)
	{
		tmp = entry->next;
		if(entry->w && !being_destroyed)
		{
			/* move of screen */
			XtMoveWidget(entry->w, -1000, -1000);
			/* destroy */
			XtDestroyWidget(entry->w);
		}

		if(entry->name)
			free(entry->name);
		if(entry->value)
			free(entry->value);
		if(entry->content)
			free(entry->content);

		/* call ourselves recursively to destroy all option members */
		if(entry->options)
			freeForm(entry->options, being_destroyed);

		free(entry);
		entry = tmp;
	}
}

/********** 
* Various callback routines: text activation & password modification, button
* activation, etc...
**********/

/*****
* Name: 		passwdCB
* Return Type: 	void
* Description: 	password hiding routine. Modifies the chars entered to only
*				display '*' and saves the really entered text in the content
*				field of the current form entry.
* In: 
*	w:			TextField widget id;
*	client..:	client data registered with this callback, in this case the
*				password form component data;
*	call_d..:	call data for this callback, in this case a
*				verifyCallbackStruct.
* Returns:
*	nothing.
*****/
static void
passwdCB(Widget w, XtPointer client_data, XtPointer call_data)
{
	XmHTMLForm *entry = (XmHTMLForm*)client_data;
	XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct*)call_data;
	String passwd = NULL;
	int len = 0;

	if(cbs->text->ptr == NULL)
	{
		/* backspace */
		if(entry->content == NULL)
			return;
		cbs->endPos = strlen(entry->content);	/* delete from here to end */
		entry->content[cbs->startPos] = '\0';	/* backspace end */
		return;
	}
	/*
	* Only one char at a time, no pasting allowed so user *types* in the
	* password.
	*/
	if(cbs->text->length > 1)
	{
		cbs->doit = False;
		/* should we actually be ringing the bell here? */
		XBell(XtDisplay(w), 100);
		return;
	}
	passwd = (String)malloc(cbs->endPos+2);	/* new char + \0 */
	if(entry->content)
	{
		strcpy(passwd, entry->content);
		passwd[strlen(entry->content)] = '\0';	/* NULL terminate */
		free(entry->content);
	}
	else
		passwd[0] = '\0';
	
	entry->content = passwd;
	strncat(entry->content, cbs->text->ptr, cbs->text->length);
	entry->content[cbs->endPos + cbs->text->length] = '\0';
	/* insert starts */
	for(len = 0; len < cbs->text->length; len++)
		cbs->text->ptr[len] = '*';
}

static void
buttonActivateCB(Widget w, XtPointer client_data, XtPointer call_data)
{
	XmHTMLForm *entry = (XmHTMLForm*)client_data;
	XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct*)call_data;
	XmHTMLWidget html;

	/* our html widget id is stored in the parent form of this entry */
	html = (XmHTMLWidget)entry->parent->html;

	/* a submit button was pressed */
	if(entry->type == FORM_SUBMIT)
	{
		_XmHTMLDebug(12, ("forms.c: buttonActivateCB for FORM_SUBMIT\n"));
		_XmHTMLFormActivate(html, cbs->event, entry);
	}
	else if(entry->type == FORM_RESET)	/* a reset button was pressed */
	{
		_XmHTMLDebug(12, ("forms.c: buttonActivateCB for FORM_RESET\n"));
		_XmHTMLFormReset(html, entry);
	}
}

static void
radioChangedCB(Widget w, XtPointer client_data, XtPointer call_data)
{
	XmHTMLForm *tmp, *entry = (XmHTMLForm*)client_data;
	XmToggleButtonCallbackStruct *cbs =
		(XmToggleButtonCallbackStruct*)call_data;

	entry->checked = cbs->set;

	/* return if type ain't radio. Check boxes don't group actions */
	if(entry->type != FORM_RADIO)
		return;

	/* toggle set, unset all other toggles */
	if(cbs->set)
	{
		/* get start of this radiobox */
		for(tmp = entry->parent->components; tmp != NULL; tmp = tmp->next)
			if(tmp->type == FORM_RADIO && !(strcasecmp(tmp->name, entry->name)))
				break;

		/* sanity */
		if(tmp == NULL)
			return;

		/* unset all other toggle buttons in this radiobox */
		for(; tmp != NULL; tmp = tmp->next)
		{
			if(tmp->type == FORM_RADIO && tmp != entry)
			{
				/* same group, unset it */
				if(!(strcasecmp(tmp->name, entry->name)))
				{
					XtVaSetValues(tmp->w, XmNset, False, NULL);
					tmp->checked = False;
				}
				/*****
				* Not a member of this group, we processed all elements in
				* this radio box, break out.
				*****/
				else
					break;
			}
		}
	}
	else /* current toggle can't be unset */
	{
		XtVaSetValues(entry->w, XmNset, True, NULL);
		entry->checked = True;
	}
}

/*****
* Name:			fileOkCB
* Return Type: 	void
* Description: 	XmNactivateCallback for the FORM_FILE entry.
* In: 
*	w:			fileSB Widget id;
*	client_..:	client data for this callback: widget id of the XmHTMLWidget
*				owning this callback;
*	call_da..:	callback data, FileSBCallbackStruct;
* Returns:
*	nothing, but the textField for the corresponding FORM_FILE entry
*	is filled with the name of the selected file;
*****/
static void
fileOkCB(Widget w, XtPointer client_data, XtPointer call_data)
{
	String value;
	XmFileSelectionBoxCallbackStruct *cbs;
	XmHTMLForm *entry = NULL;

	cbs = (XmFileSelectionBoxCallbackStruct*)call_data;
	
	XtVaGetValues(w, XmNuserData, &entry, NULL);

	if(entry == NULL)
	{
		_XmHTMLWarning(__WFUNC__((Widget)client_data, "fileOkCB"),
			XMHTML_MSG_53);
		return;
	}
	/* get selection */
	XmStringGetLtoR(cbs->value, XmFONTLIST_DEFAULT_TAG, &value);

	/* insert it */
	if(value)
	{
		if(entry->multiple)
		{
			XmTextPosition last;
			/* get end of current selection */
			last = XmTextFieldGetLastPosition(entry->child);
			if(last != 0)
			{
				XmTextFieldInsert(entry->child, last, ":");
				last = XmTextFieldGetLastPosition(entry->child);
			}
			XmTextFieldInsert(entry->child, last, value);
		}
		else
			XmTextFieldSetString(entry->child, value);
		free(value);
	}
	XtUnmanageChild(w);
}

/*****
* Name:			fileActivateCB
* Return Type: 	void
* Description: 	creates & pops up a fileSB for the FORM_FILE form component
*				type.
* In: 
*	w:			widget id of pushbutton activating this callback;
*	client_..:	Form entry of this form component;
*	call_da..:	unused;
* Returns:
*	nothing.
* Note:
*	the fileSB created is stored in the parent form for reuse.
*****/
static void
fileActivateCB(Widget w, XtPointer client_data, XtPointer call_data)
{
	XmHTMLForm *entry = (XmHTMLForm*)client_data;
	XmString pattern;

	if(entry->parent->fileSB == NULL)
	{
		entry->parent->fileSB =XmCreateFileSelectionDialog(entry->parent->html,
			"_fileDialog", NULL, 0);

		/* callbacks */
		XtAddCallback(entry->parent->fileSB, XmNcancelCallback,
			(XtCallbackProc)XtUnmanageChild, NULL);
		XtAddCallback(entry->parent->fileSB, XmNokCallback,
			(XtCallbackProc)fileOkCB, entry->parent->html);

		XtVaSetValues(XtParent(entry->parent->fileSB), XmNtitle,
			entry->value ? entry->value : "Select A File", NULL);
	}
	/* check for a file selection pattern */
	pattern = XmStringCreateLocalized(entry->content ? entry->content: "*");
	/*****
	* Store entry as userData, this fileSB is used for every FORM_FILE entry
	* in this form.
	*****/
	XtVaSetValues(entry->parent->fileSB,
		XmNuserData, (XtPointer)entry,
		XmNpattern, pattern,
		NULL);
	XmStringFree(pattern);

	XtManageChild(entry->parent->fileSB);
	XtPopup(XtParent(entry->parent->fileSB), XtGrabNone);
	XMapRaised(XtDisplay(entry->parent->html),
		XtWindow(XtParent(entry->parent->fileSB)));
}

/*****
* Name:			optionMenuCB
* Return Type: 	void
* Description:	callback for option menu selection.
* In: 
*	w:			TextField widget id;
*	client..:	client data registered with this callback, in this case the
*				password form component data;
*	call_d..:	call data for this callback, in this case a
*				verifyCallbackStruct.
* Returns:
*	nothing, but all options belonging to the same option menu get their
*	checked field set according to the selected menu item.
*****/
static void
optionMenuCB(Widget w, XtPointer client_data, XtPointer call_data)
{
	XmHTMLForm *tmp, *entry = (XmHTMLForm*)client_data;
	int i = 0;

	/*****
	* walk all childs of the parent entry, unselecting all childs that
	* don't match the selected item and selecting the one that was.
	*****/
	for(tmp = entry->options; tmp != NULL; tmp = tmp->next, i++)
	{
		if(tmp->w == w)
			tmp->checked = True;
		else
			tmp->checked = False;
	}
}

static void
finalizeEntry(XmHTMLWidget html, XmHTMLForm *entry, Boolean insert,
	Boolean manage) 
{
	if(entry->w)
	{
		Dimension w = 0, h = 0;

		/*
		* Set values. We place this thing completely off screen so
		* no nasty things happen when this widget is mapped to it's correct
		* position for the first time.
		*/
		argc = 0;
#ifdef UNMAP_FORMS
		XtSetArg(args[argc], XmNmappedWhenManaged, False); argc++;
#endif
		XtSetArg(args[argc], XmNx, 0); argc++;
		XtSetArg(args[argc], XmNy, 0); argc++;
		XtSetValues(entry->w, args, argc);

		/* get widget dimensions */
		XtVaGetValues(entry->w,
			XmNwidth, &w,
			XmNheight, &h,
			NULL);
		entry->width = w;
		entry->height = h;

		if(manage)
			XtManageChild(entry->w);
	}
	else
	{
		entry->width = 0;
		entry->height = 0;
	}

	/* add to parent form when requested */
	if(insert)
	{
		if(current_entry)
		{
			entry->prev = current_entry;
			current_entry->next = entry;
			current_entry = entry;
		}
		else
		{
			current_form->components = current_entry = entry;
		}
		/* and keep up component counter */
		current_form->ncomponents++;
	}
	_XmHTMLDebug(12, ("forms.c: finalizeEntry, added form entry, "
		"type = %i, name = %s\n", entry->type, entry->name));
}

/********
****** Public Functions
********/

/*****
* Name:			_XmHTMLStartForm
* Return Type:	void
* Description:	creates and initializes a new form parent
* In:
*	html:		XmHTMLWidget id;
*	attr..:		form attributes.
* Returns:
*	nothing but attaches it to the widget's form_data list
*****/
void
_XmHTMLStartForm(XmHTMLWidget html, String attributes)
{
	static XmHTMLFormData *form;
	XmFontListEntry my_fontEntry;
	String my_fonttag = MY_FONTLIST_DEFAULT_TAG;

	/* empty form, no warning, just return */
	if(attributes == NULL)
		return;

	/* allocate a new entry */
	form = (XmHTMLFormData*)malloc(sizeof(XmHTMLFormData));
	/* initialise to zero */
	(void)memset(form, 0, sizeof(XmHTMLFormData));

	/* initially no clipmask */
	form->clip = None;

	/* but we can create one */
	form->can_clip = True;

	/* this form starts a new set of entries */
	current_entry = NULL;

	/* set form owner */
	form->html = (Widget)html;

	/* pick up action */
	if((form->action = _XmHTMLTagGetValue(attributes, "action")) == NULL)
	{
		/* the action tag is required, so destroy and return if not found */
		free(form);
		form = NULL;
#ifdef PEDANTIC
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLStartForm"), XMHTML_MSG_54);
#endif
		return;
	}
	/* default method is get */
	form->method = XmHTML_FORM_GET;
	{
		char *method = _XmHTMLTagGetValue(attributes, "method"); 
		if(method != NULL)
		{ 
			if(!strncasecmp(method, "get", 3))
				form->method = (int)XmHTML_FORM_GET;
			else if(!strncasecmp(method, "post", 4))
				form->method = (int)XmHTML_FORM_POST;
			else if(!strncasecmp(method, "pipe", 4))
				form->method = (int)XmHTML_FORM_PIPE;
			free(method);
		}
	}

	/* form encoding */
	if((form->enctype = _XmHTMLTagGetValue(attributes, "enctype")) == NULL)
		form->enctype = strdup("application/x-www-form-urlencoded");

	if(html->html.form_data)
	{
		form->prev = current_form;
		current_form->next = form;
		current_form = form;
	}
	else
		html->html.form_data = current_form = form;
	_XmHTMLDebug(12, ("forms.c: _XmHTMLStartForm, created a new form "
		"entry, action = %s\n", form->action));

	/* create a valid fontList context for our default font */
	my_fontEntry = XmFontListEntryCreate(my_fonttag, XmFONT_IS_FONT,
					(XtPointer)html->html.default_font->xfont);
	my_fontList = XmFontListAppendEntry(NULL, my_fontEntry);
	XmFontListEntryFree(&my_fontEntry);

	/* translations overrides */
	if(textFTranslations == NULL)
		textFTranslations = XtParseTranslationTable(textF_translations);
	if(travTranslations == NULL)
		travTranslations = XtParseTranslationTable(trav_translations);
	if(pushBTranslations == NULL)
		pushBTranslations = XtParseTranslationTable(pushB_translations);
}

/*****
* Name:			_XmHTMLEndForm
* Return Type: 	void
* Description: 	invalidates the current parent form.
* In: 
*	html:		XmHTMLWidget id.
* Returns:
*	nothing.
*****/
void
_XmHTMLEndForm(XmHTMLWidget html)
{
#ifdef DEBUG
	current_entry = NULL;
	_XmHTMLDebug(12, ("forms.c: _XmHTMLEndForm, listing for form %s.\n",
		current_form->action));
	for(current_entry = current_form->components; current_entry != NULL;
		current_entry = current_entry->next)
	{
		_XmHTMLDebug(12, ("\tname = %s, type = %i\n", current_entry->name,
			current_entry->type));
	}
#endif

	/* free fontList context */
	XmFontListFree(my_fontList);
	my_fontList  = (XmFontList)NULL;
}

/*****
* Name: 		_XmHTMLFormAddInput
* Return Type: 	XmHTMLForm
* Description: 	creates a form input entry 
* In: 
*	html:		XmHTMLWidget id;
*	attributes:	input attributes;
* Returns:
*	a new XmHTMLForm entry.
*****/
XmHTMLForm*
_XmHTMLFormAddInput(XmHTMLWidget html, String attributes)
{
	static XmHTMLForm *entry;
	XmString label;
	Widget parent;

	/*****
	* HTML form child widgets are childs of the workarea.
	* Making them a direct child of the widget itself messes up scrolling.
	*****/
	parent = html->html.work_area;

	if(attributes == NULL)
		return(NULL);

	if(current_form == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLFormAddInput"),
			XMHTML_MSG_55, html_tokens[HT_INPUT]);
	}

	/* Create and initialise a new entry */
	entry = (XmHTMLForm*)malloc(sizeof(XmHTMLForm));
	(void)memset(entry, 0, sizeof(XmHTMLForm));

	/* set parent form */
	entry->parent = current_form;

	entry->type = getInputType(attributes);

	/* get name. Use type if not given */
	if((entry->name = _XmHTMLTagGetValue(attributes, "name")) == NULL)
		entry->name = strdup(input_tokens[entry->type]);

	entry->value = _XmHTMLTagGetValue(attributes, "value");
	entry->checked = _XmHTMLTagCheck(attributes, "checked");
	entry->selected = entry->checked;	/* save default state */

	if(entry->type == FORM_TEXT || entry->type == FORM_PASSWD)
	{
		/* default to 25 columns if size hasn't been specified */
		entry->size = _XmHTMLTagGetNumber(attributes, "size", 25);

		/* unlimited amount of text input if not specified */
		entry->maxlength = _XmHTMLTagGetNumber(attributes, "maxlength", -1);

		/* passwd can't have a default value */
		if(entry->type == FORM_PASSWD && entry->value)
		{
			free(entry->value);
			entry->value = NULL;
		}
		/* empty value if none given */
		if(entry->value == NULL)
		{
			entry->value = (String)malloc(1);
			entry->value[0] = '\0';
		}
	}
	else if(entry->type == FORM_FILE)
	{
		/* default to 20 columns if size hasn't been specified */
		entry->size = _XmHTMLTagGetNumber(attributes, "size", 20);

		/* check is we are to support multiple selections */
		entry->multiple = _XmHTMLTagCheck(attributes, "multiple");

		/* any dirmask to use? */
		entry->value   = _XmHTMLTagGetValue(attributes, "value");
		entry->content = _XmHTMLTagGetValue(attributes, "src");
	}
	entry->align = _XmHTMLGetImageAlignment(attributes);

	/*****
	* go create the actual widget
	* As image buttons are promoted to image words we don't deal with the
	* FORM_IMAGE case. For hidden form fields nothing needs to be done.
	*****/
	if(entry->type != FORM_IMAGE && entry->type != FORM_HIDDEN)
	{
		/* defaults for all widgets */
		argc = 0;
		XtSetArg(args[argc], XmNborderWidth, 0); argc++;

		/*****
		* Check if we may use the document colors for the form components.
		*****/
		if(html->html.allow_form_coloring)
		{
			XtSetArg(args[argc], XmNbackground, html->html.body_bg); argc++;
			XtSetArg(args[argc], XmNforeground, html->html.body_fg); argc++;
		}
		XtSetArg(args[argc], XmNfontList, my_fontList); argc++;

		switch(entry->type)
		{
			/* text field, set args and create it */
			case FORM_TEXT:
			case FORM_PASSWD:
				XtSetArg(args[argc], XmNcolumns, entry->size); argc++;
				XtSetArg(args[argc], XmNvalue, entry->value); argc++;
				XtSetArg(args[argc], XmNhighlightThickness, 0); argc++;

				if(entry->maxlength != -1)
				{
					XtSetArg(args[argc], XmNmaxLength, entry->maxlength);
					argc++;
				}
				entry->w = XmCreateTextField(parent, entry->name, args, argc);

				/* override some translations */
				XtOverrideTranslations(entry->w, textFTranslations);
				XtOverrideTranslations(entry->w, travTranslations);

				/* callbacks */
				if(entry->type == FORM_PASSWD)
					XtAddCallback(entry->w, XmNmodifyVerifyCallback,
						(XtCallbackProc)passwdCB, (XtPointer)entry);

				break;

			/* toggle buttons, set args and create */
			case FORM_CHECK:
			case FORM_RADIO:
				XtSetArg(args[argc], XmNindicatorType,
					(entry->type == FORM_CHECK ? XmN_OF_MANY : XmONE_OF_MANY));
				argc++;
				/* empty label, text following this input will contain it */
				label = XmStringCreate(" ", MY_FONTLIST_DEFAULT_TAG);
				XtSetArg(args[argc], XmNlabelString, label); argc++;
				XtSetArg(args[argc], XmNset, entry->checked); argc++;
				/* no margins whatsoever */
				XtSetArg(args[argc], XmNhighlightThickness, 0); argc++;
				XtSetArg(args[argc], XmNmarginWidth, 0); argc++;
				XtSetArg(args[argc], XmNmarginHeight, 0); argc++;
				XtSetArg(args[argc], XmNmarginLeft, 0); argc++;
				XtSetArg(args[argc], XmNmarginRight, 0); argc++;
				XtSetArg(args[argc], XmNmarginTop, 0); argc++;
				XtSetArg(args[argc], XmNmarginBottom, 0); argc++;
				XtSetArg(args[argc], XmNspacing, 0); argc++;
				/*****
				* make height of this widget match the default font so it
				* doesn't seem very tiny.
				*****/
				XtSetArg(args[argc], XmNindicatorSize,
					html->html.default_font->height);
				argc++;

				entry->w = XmCreateToggleButton(parent, entry->name, args,
					argc);

				XmStringFree(label);

				/* override some translations */
				XtOverrideTranslations(entry->w, travTranslations);
				XtOverrideTranslations(entry->w, pushBTranslations);

				/* set/unset callback */
				XtAddCallback(entry->w, XmNvalueChangedCallback,
					(XtCallbackProc)radioChangedCB, (XtPointer)entry);
				break;

			/*****
			* special case: this type of input is a textfield with a ``browse''
			* button.
			*****/
			case FORM_FILE:
				{
					static Widget field, button;

					/* first create the form container */
					XtSetArg(args[argc], XmNmarginWidth, 0); argc++;
					XtSetArg(args[argc], XmNmarginHeight, 0); argc++;
					entry->w = XmCreateForm(parent, entry->name, args, argc);

					/* childs need to be mapped when their parent is mapped */
					argc=0;
					XtSetArg(args[argc], XmNcolumns, entry->size); argc++;
					XtSetArg(args[argc], XmNhighlightThickness, 0); argc++;
#ifdef UNMAP_FORMS
					XtSetArg(args[argc], XmNmappedWhenManaged, True); argc++;
#endif
					XtSetArg(args[argc], XmNfontList, my_fontList); argc++;
					XtSetArg(args[argc], XmNleftAttachment, XmATTACH_FORM);
					argc++;

					/* use document colors if allowed */
					if(html->html.allow_form_coloring)
					{
						XtSetArg(args[argc], XmNbackground, html->html.body_bg);
						argc++;
						XtSetArg(args[argc], XmNforeground, html->html.body_fg);
						argc++;
					}

					if(entry->maxlength != -1)
					{
						XtSetArg(args[argc], XmNmaxLength, entry->maxlength);
						argc++;
					}
					field = XmCreateTextField(entry->w, "_fileField", args,
								argc);
					/* override some translations */
					XtOverrideTranslations(field, textFTranslations);
					XtOverrideTranslations(field, travTranslations);

					/* no callbacks */
					
					/* browse button */
					argc = 0;
					label = XmStringCreate(entry->value ? entry->value :
						"Browse...", MY_FONTLIST_DEFAULT_TAG);
					XtSetArg(args[argc], XmNlabelString, label); argc++;
					XtSetArg(args[argc], XmNborderWidth, 0); argc++;
					XtSetArg(args[argc], XmNfontList, my_fontList); argc++;
#ifdef UNMAP_FORMS
					XtSetArg(args[argc], XmNmappedWhenManaged, True); argc++;
#endif
					XtSetArg(args[argc], XmNleftAttachment, XmATTACH_WIDGET);
					argc++;
					XtSetArg(args[argc], XmNleftWidget, field);
					argc++;

					/* use document colors if allowed */
					if(html->html.allow_form_coloring)
					{
						XtSetArg(args[argc], XmNbackground, html->html.body_bg);
						argc++;
						XtSetArg(args[argc], XmNforeground, html->html.body_fg);
						argc++;
					}

					/* create button */
					button = XmCreatePushButton(entry->w, "_fileButton", args,
						argc);

					XmStringFree(label);

					/* callbacks */
					XtAddCallback(button, XmNactivateCallback,
						(XtCallbackProc)fileActivateCB, (XtPointer)entry);

					/* manage the children */
					XtManageChild(field);
					XtManageChild(button);

					/* store textfield as child */
					entry->child = field;
				}
				/*****
				* form *needs* to be realized if we want it's layout
				* computations to succeed --- rmo
				*****/
				XtRealizeWidget(entry->w);
				break;

			/* push buttons, set args and create */
			case FORM_RESET:
			case FORM_SUBMIT:

				label = XmStringCreate(entry->value ? entry->value :
					entry->name, MY_FONTLIST_DEFAULT_TAG);
				XtSetArg(args[argc], XmNlabelString, label); argc++;
				entry->w = XmCreatePushButton(parent, entry->name, args, argc);
				XmStringFree(label);

				/* override some translations */
				XtOverrideTranslations(entry->w, travTranslations);
				XtOverrideTranslations(entry->w, pushBTranslations);

				/* callbacks */
				XtAddCallback(entry->w, XmNactivateCallback,
					(XtCallbackProc)buttonActivateCB, (XtPointer)entry);
				break;

			default:
				break;
		}
	}

	/* Finalize entry, insert in parent form and manage it. */
	finalizeEntry(html, entry, True, True);

	/* all done */
	return(entry);
}

/*****
* Name: 		_XmHTMLFormAddSelect
* Return Type: 	XmHTMLForm
* Description: 	creates a form select entry 
* In: 
*	html:		XmHTMLWidget id;
*	attributes:	select attributes;
* Returns:
*	a new XmHTMLForm entry.
*****/
XmHTMLForm*
_XmHTMLFormAddSelect(XmHTMLWidget html, String attributes)
{
	static XmHTMLForm *entry;
	Widget parent;

	/*****
	* HTML form child widgets are childs of the workarea.
	* Making them a direct child of the widget itself messes up scrolling.
	*****/
	parent = html->html.work_area;

	if(attributes == NULL)
		return(NULL);

	if(current_form == NULL)
	{
		/* too bad, ignore */
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLFormAddSelect"),
			XMHTML_MSG_55, html_tokens[HT_SELECT]);
		return(NULL);
	}

	/* Create and initialise a new entry */
	entry = (XmHTMLForm*)malloc(sizeof(XmHTMLForm));
	(void)memset(entry, 0, sizeof(XmHTMLForm));

	/* set parent form */
	entry->parent = current_form;

	/* form type */
	entry->type = FORM_SELECT;

	/* get name */
	if((entry->name = _XmHTMLTagGetValue(attributes, "name")) == NULL)
		entry->name = strdup("Select");

	/* no of visible items in list */
	entry->size = _XmHTMLTagGetNumber(attributes, "size", 1);

	/* multiple select? */
	entry->multiple = _XmHTMLTagCheck(attributes, "multiple");

	/* default args for both multiple and single select */
	argc = 0;
	XtSetArg(args[argc], XmNborderWidth, 0); argc++;
	XtSetArg(args[argc], XmNfontList, my_fontList); argc++;

	/* use document colors if allowed */
	if(html->html.allow_form_coloring)
	{
		XtSetArg(args[argc], XmNbackground, html->html.body_bg); argc++;
		XtSetArg(args[argc], XmNforeground, html->html.body_fg); argc++;
	}

	/* multiple select or more than one item visible: it's a listbox */
	if(entry->multiple || entry->size > 1)
	{
		parent = html->html.work_area;
		XtSetArg(args[argc], XmNlistSizePolicy, XmVARIABLE); argc++;
		XtSetArg(args[argc], XmNscrollBarDisplayPolicy, XmAS_NEEDED); argc++;
		XtSetArg(args[argc], XmNmarginWidth, 0); argc++;
		XtSetArg(args[argc], XmNmarginHeight, 0); argc++;

		/* at least two items required for list boxes */
		XtSetArg(args[argc], XmNvisibleItemCount,
				(entry->size == 1 ? 2 : entry->size)); argc++;

		/* multiple selection possible */
		if(entry->multiple)
		{
			XtSetArg(args[argc], XmNselectionPolicy, XmMULTIPLE_SELECT);
			argc++;
		}

		/*
		* Create it. This returns the widget id of the listbox, *not*
		* the scrolledWindow parent!
		*/
		entry->w = XmCreateScrolledList(parent, entry->name, args, argc);

		/* override some translations */
		XtOverrideTranslations(entry->w, travTranslations);
#ifdef UNMAP_FORMS
		XtSetMappedWhenManaged(XtParent(entry->w), False);
#endif
		/* Finalize entry, don't insert in parent form and manage it */
		finalizeEntry(html, entry, False, True);
	}
	else	/* an option menu */
	{
		/* the menu that will contain the menu items */
		Widget menu;
		menu = XmCreatePulldownMenu(parent, entry->name, args, argc);
		entry->w = menu;
		/* override some translations */
		XtOverrideTranslations(entry->w, travTranslations);

		/* Finalize entry, don't insert in parent form and don't manage */
		finalizeEntry(html, entry, False, False);

		/* make sure menu get's mapped when it's managed */
#ifdef UNMAP_FORMS
		XtSetMappedWhenManaged(entry->w, True);
#endif

	}

	/* this will be used to keep track of inserted menu items */
	entry->next = NULL;

	return(entry);
}

/*****
* Name:			_XmHTMLFormSelectAddOption
* Return Type: 	void
* Description: 	adds an option button to a form <select>.
* In: 
*	html:		XmHTMLWidget id;
*	entry:		parent for this option;
*	attrib..:	attributes for this option;
*	label:		label for the option button.
* Returns:
*	nothing, but the new button is added to the list of options of the
*	parent entry.
*****/
void
_XmHTMLFormSelectAddOption(XmHTMLWidget html, XmHTMLForm *entry,
	String attributes, String label)
{
	XmHTMLForm *item;
	XmString xms;

	/* Create and initialise a new entry */
	item = (XmHTMLForm*)malloc(sizeof(XmHTMLForm));
	(void)memset(item, 0, sizeof(XmHTMLForm));

	/* form type */
	item->type = FORM_OPTION;

	/* value. Use id if none given */
	if(attributes == NULL ||
		(item->value = _XmHTMLTagGetValue(attributes, "value")) == NULL)
	{
		char dummy[32];	/* 2^32 possible entries...*/
		sprintf(dummy, "%i", entry->maxlength);
		item->value = strdup(dummy);
	}

	/* initial state */
	item->selected = (int)(attributes ?
					_XmHTMLTagCheck(attributes, "selected") : False);
	item->checked  = (Boolean)item->selected;

	/* list box selection */
	if(entry->multiple || entry->size > 1)
	{
		/* append item to bottom of list */
		xms = XmStringCreate(label, MY_FONTLIST_DEFAULT_TAG);
		XmListAddItem(entry->w, xms, 0);
		XmStringFree(xms);

		/* add this item to the list of selected items */
		if(item->checked)
		{
			/* single selection always takes the last inserted item */
			entry->selected = entry->maxlength;

			/*
			* Since we are always inserting items at the end of the list
			* we can simple select it by using 0 as the position arg
			*/
			XmListSelectPos(entry->w, 0, False);
		}
	}
	else
	{
		/* add a new menu item */
		xms = XmStringCreate(label, MY_FONTLIST_DEFAULT_TAG);

		/* option menu button */
		argc = 0;
		/* use document colors if allowed */
		if(html->html.allow_form_coloring)
		{
			XtSetArg(args[argc], XmNbackground, html->html.body_bg); argc++;
			XtSetArg(args[argc], XmNforeground, html->html.body_fg); argc++;
		}
		XtSetArg(args[argc], XmNlabelString, xms); argc++;
		XtSetArg(args[argc], XmNfontList, my_fontList); argc++;
		item->w = XmCreatePushButton(entry->w, label, args, argc);
		XmStringFree(xms);

		/* save as default menu item if initially selected */
		if(item->checked)
			entry->selected = entry->maxlength;

		/* callback to set parent selection state */
		XtAddCallback(item->w, XmNactivateCallback,
			(XtCallbackProc)optionMenuCB, (XtPointer)entry);

		XtManageChild(item->w);
	}

	/* insert item, entry->next contains ptr to last inserted option */
	if(entry->next)
	{
		entry->next->next = item;
		entry->next = item;
	}
	else
	{
		entry->options = entry->next = item;
	}

	/* no of options inserted so far */
	entry->maxlength++;
}

/*****
* Name:			_XmHTMLFormSelectClose
* Return Type: 	void
* Description: 	closes a form <select> tag. Performs required wrapup
*				operations such as actual optionmenu creation and translation
*				stuff.
* In: 
*	html:		XmHTMLWidget id;
*	entry:		entry to be closed.
* Returns:
*	nothing.
*****/
void
_XmHTMLFormSelectClose(XmHTMLWidget html, XmHTMLForm *entry)
{
	/* option menu */
	if(!entry->multiple && entry->size == 1)
	{
		Widget menu = entry->w;		/* rowColumn containing all menubuttons */
		WidgetList children;
		int num_children;
		XmString xms;

		argc = 0;
		XtSetArg(args[argc], XmNx, 0); argc++;
		XtSetArg(args[argc], XmNy, 0); argc++;
		XtSetArg(args[argc], XmNmarginWidth, 0); argc++;
		XtSetArg(args[argc], XmNmarginHeight, 0); argc++;
		XtSetArg(args[argc], XmNsubMenuId, menu); argc++;
		XtSetArg(args[argc], XmNhighlightThickness, 0); argc++;
		XtSetArg(args[argc], XmNfontList, my_fontList); argc++;
		/* use document colors if allowed */
		if(html->html.allow_form_coloring)
		{
			XtSetArg(args[argc], XmNbackground, html->html.body_bg); argc++;
			XtSetArg(args[argc], XmNforeground, html->html.body_fg); argc++;
		}

		entry->w = XmCreateOptionMenu(html->html.work_area, "optionMenu",
			args, argc);

		/* override some translations */
		XtOverrideTranslations(entry->w, travTranslations);

		/*****
		* Remove the label gadget. We first *MUST* set an empty label
		* because Motif always reserves space for the label, EVEN when the
		* label is unmanaged. Sigh.
		*****/
		argc = 0;
		xms = XmStringCreate("", MY_FONTLIST_DEFAULT_TAG);
		XtSetArg(args[argc], XmNlabelString, xms); argc++;
		XtSetValues(XmOptionLabelGadget(entry->w), args, argc);
		XmStringFree(xms);
		XtUnmanageChild(XmOptionLabelGadget(entry->w));

		/* manage the menu */
#ifdef UNMAP_FORMS
		XtSetMappedWhenManaged(entry->w, False);
#endif
		XtManageChild(entry->w);

		/* get current list of menu buttons. This returns a rowColumn */
		menu = NULL;
		XtVaGetValues(entry->w, XmNsubMenuId, &menu, NULL);

		XtVaGetValues(menu,
			XmNnumChildren, &num_children,
			XmNchildren, &children,
			NULL);

		/* and select selected button (or first if no button is selected) */
		XtVaSetValues(entry->w,
			XmNmenuHistory, children[entry->selected], NULL);

		/* store menupane id as child id */
		entry->child = menu;

		/* safety */
		entry->next = NULL;

		/*****
		* Finalize entry, insert in parent form, don't manage 'cause it's
		* already managed.
		*****/
		finalizeEntry(html, entry, True, False);
	}
	else
	{
		/* safety */
		entry->next = NULL;

		/* list is child of ScrolledWindow */
		entry->child = entry->w;

		/* actual widget we'll be moving */
		entry->w = XtParent(entry->child);

		/* Finalize entry, insert in parent form don't manage it */
		finalizeEntry(html, entry, True, False);

		/* make sure child is visible when parent is managed */
#ifdef UNMAP_FORMS
		XtSetMappedWhenManaged(entry->child, True);
#endif
	}
}

/*****
* Name:			_XmHTMLFormAddTextArea
* Return Type: 	XmHTMLForm*
* Description: 	creates a form <textarea> entry.
* In: 
*	html:		XmHTMLWidget id;
*	attrib..:	attributes for this textarea;
*	text:		default text for this entry.
* Returns:
*	a newly created entry.
*****/
XmHTMLForm*
_XmHTMLFormAddTextArea(XmHTMLWidget html, String attributes, String text)
{
	static XmHTMLForm *entry;
	String name = NULL;
	int rows = 0, cols = 0;
	Widget parent;

	/*****
	* HTML form child widgets are childs of the workarea.
	* Making them a direct child of the widget itself messes up scrolling.
	*****/
	parent = html->html.work_area;

	/* these are *required* */
	if(attributes == NULL)
		return(NULL);

	/* sanity, we must have a parent form */
	if(current_form == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLFormAddTextArea"),
			XMHTML_MSG_55, html_tokens[HT_TEXTAREA]);
	}

	/* get form name. Mandatory so spit out an error if not found */
	if((name = _XmHTMLTagGetValue(attributes, "name")) == NULL)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLFormAddTextArea"),
			XMHTML_MSG_56);
		return(NULL);
	}

	/* get form dimensions. Mandatory so spit out an error if not found. */
	rows = _XmHTMLTagGetNumber(attributes, "rows", 0);
	cols = _XmHTMLTagGetNumber(attributes, "cols", 0);
	if(rows <= 0 || cols <= 0)
	{
		_XmHTMLWarning(__WFUNC__(html, "_XmHTMLFormAddTextArea"),
			XMHTML_MSG_57);
	}

	/* Create and initialise a new entry */
	entry = (XmHTMLForm*)malloc(sizeof(XmHTMLForm));
	(void)memset(entry, 0, sizeof(XmHTMLForm));

	/* fill in appropriate fields */
	entry->name      = name;
	entry->parent    = current_form;
	entry->type      = FORM_TEXTAREA;
	entry->size      = cols;
	entry->maxlength = rows;

	/* default text. Empty value if not given */
	if((entry->value = text) == NULL)
	{
		entry->value = (String)malloc(1);
		entry->value[0] = '\0';
	}

	/* set defaults */
	argc = 0;

	/* use document colors if allowed */
	if(html->html.allow_form_coloring)
	{
		XtSetArg(args[argc], XmNbackground, html->html.body_bg); argc++;
		XtSetArg(args[argc], XmNforeground, html->html.body_fg); argc++;
	}
	XtSetArg(args[argc], XmNfontList, my_fontList); argc++;
	XtSetArg(args[argc], XmNvalue, entry->value); argc++;
	XtSetArg(args[argc], XmNcolumns, cols); argc++;
	XtSetArg(args[argc], XmNrows, rows); argc++;
	XtSetArg(args[argc], XmNeditMode, XmMULTI_LINE_EDIT); argc++;
	XtSetArg(args[argc], XmNscrollingPolicy, XmAUTOMATIC); argc++;
	XtSetArg(args[argc], XmNscrollBarDisplayPolicy, XmAS_NEEDED); argc++;
	XtSetArg(args[argc], XmNscrollBarPlacement, html->html.sb_placement);argc++;
	XtSetArg(args[argc], XmNhighlightThickness, 0); argc++;
	XtSetArg(args[argc], XmNborderWidth, 0); argc++;

	/* now create it */
	entry->child = XmCreateScrolledText(parent, entry->name, args, argc);
	entry->w = XtParent(entry->child);	/* actual widget id we'll be moving */

	/*****
	* Manage textArea child and make sure it get's mapped when the parent
	* is mapped.
	*****/
#ifdef UNMAP_FORMS
	XtSetMappedWhenManaged(entry->child, True);
#endif
	XtManageChild(entry->child);

	/* safety */
	entry->next = NULL;

	/* Finalize entry, insert in parent form and don't manage it yet */
	finalizeEntry(html, entry, True, True);

	/* all done! */
	return(entry);
}

void
_XmHTMLFreeForm(XmHTMLWidget html, XmHTMLFormData *form)
{
	XmHTMLFormData *tmp;
	Boolean being_destroyed = html->core.being_destroyed;

	while(form != NULL)
	{
		tmp = form->next;
		freeForm(form->components, being_destroyed);
		if(form->action)
			free(form->action);
		if(form->enctype)
			free(form->enctype);
		if(form->fileSB)	/* only one fileSB can exist for each form */
		{
			if(XtIsManaged(form->fileSB))
				XtUnmanageChild(form->fileSB);
			if(!being_destroyed)
				XtDestroyWidget(form->fileSB);
		}
		if(form->clip != None)
		{
			ToolkitAbstraction *tka = HTML_ATTR(tka);
			FreePixmap(tka->dpy, form->clip);
			form->clip = False;
		}
		free(form);
		form = tmp;
	}
}

/*****
* Name:			formCountComponents
* Return Type:	int
* Description:	count the number of client side components in a form 
*				(called from _XmHTMLFormActivate).
* In:
*   parent:		parent component of the the component that activated the
*   			callback.
*   comp:		component that activated the callback.
*   			
* Returns:
*	the number of client side components.
* Note:
*	written by: offer@sgi.com
*****/
static int
formCountComponents(XmHTMLForm *parent, XmHTMLForm *comp)
{
	int	count=1;
	
	current_entry = NULL;

	/* walk all components for this form and see which ones are selected */
	for(current_entry = parent; current_entry != NULL; 
		current_entry = current_entry->next)
	{
		switch((componentType)current_entry->type)
		{ 
			case FORM_SELECT:
				if(current_entry->multiple || current_entry->size > 1) 
				{
					/* list. Get count of all selected items */
					int *pos_list, pos_cnt = 0;

					/* must take it from child, parent is a scrolledWindow */
					if((XmListGetSelectedPos(current_entry->child, &pos_list,
						&pos_cnt)))
					{
						count += pos_cnt;
						free(pos_list);	/* don't forget! */
					}
				}
				else
				{
					/* option menu, add entry when an item has been selected */
					XmHTMLForm *opt = NULL;
					for(opt = current_entry->options; opt != NULL;
						opt = opt->next)
					{
						if(opt->checked)
							count++;
					}
				}
				break;

			case FORM_CHECK:
			case FORM_RADIO:
				if(current_entry->checked) 
					count++;
				break;

			case FORM_IMAGE:
				if(comp == current_entry) 
					count+=2; 	/* name.x=... and name.y=... */
				break;

			case FORM_RESET:
			case FORM_SUBMIT:
				if(comp == current_entry) 
					count++; 

			case FORM_PASSWD:
				if(current_entry->content != NULL)
					count++;
				break;

			/* only return text fields if these actually contain text */
			case FORM_TEXT:
				if(XmTextFieldGetLastPosition(current_entry->w))
					count++;
				break;
			case FORM_FILE:
				if(XmTextFieldGetLastPosition(current_entry->child))
					count++;
				break;
			case FORM_TEXTAREA:
				if(XmTextGetLastPosition(current_entry->child))
					count++;
				break;

			/* hidden fiels are always returned */
			case FORM_HIDDEN:
				count++;
				break;

			case FORM_OPTION:
				/* is a wrapper, so doesn't do anything */
				break;
			case FORM_UNKNOWN:
#ifdef DEBUG
				/* shouldn't happen */
				_XmHTMLWarning(__WFUNC__(parent->parent->html,
					"formCountComponents"), XMHTML_MSG_78);
#endif
				break;
			/* no default */
		}
	}
	return(count);
}

/*****
* Name:			_XmHTMLFormActivate
* Return Type: 	Boolean
* Description: 	form activator. Will collect all data of the current form
*				and call the form callback.
* In: 
*	html:		XmHTMLWidget id;
*	event:		event structure triggering this callback;
*	entry:		form entry that triggered this routine.
* Returns:
*	value of the doc_modified field
* Note:
*	written by: offer@sgi.com
*****/
Boolean
_XmHTMLFormActivate(XmHTMLWidget html, XEvent *event, XmHTMLForm *entry)
{
	XmHTMLFormCallbackStruct cbs;
	XmHTMLFormDataPtr components;
	int nComponents;
	int	i, j;
	String chPtr;

	_XmHTMLDebug(12, ("forms.c: _XmHTMLFormActivate, activated by component "
		"%s\n", entry->name));

	/* only do something when a form callback has been installed */
	if(html->html.form_callback == NULL)
		return(False);

	/*****
	* Check which components of the current form should be returned.
	*
	* Seems time consuming stepping through the link list twice, but this way 
	* we can guarantee that we malloc the right ammount of memory (there isn't 
	* a one-to-one mapping for internal and application views of the
	* components, _and_ we won't frag memory unlike repeated calls to realloc 
	* -- rmo 
	*****/	
	nComponents = formCountComponents(entry->parent->components, entry);
	components = (XmHTMLFormDataPtr)calloc(nComponents,
					sizeof(XmHTMLFormDataRec)); 
	
	current_entry = NULL;
	for(current_entry = entry->parent->components, j=0;
		current_entry != NULL && j < nComponents; 
		current_entry = current_entry->next)
	{
		/* default settings for this entry. Overridden when required below */
		components[j].type  = current_entry->type;
		components[j].name  = current_entry->name;

		switch((componentType)current_entry->type)
		{ 
			case FORM_SELECT:
				/*****
				* Option menu, get value of selected item (size check required
				* as multiple is false for list boxes offering a single
				* entry).
				*****/
				if(!current_entry->multiple && current_entry->size == 1)
				{ 
					XmHTMLForm *opt = NULL;

					/*****
					* Get selected item (if any). Only one item can be
					* selected at a time as this is an option menu.
					*****/
					for(opt = current_entry->options; opt != NULL &&
						!opt->checked; opt = opt->next);

					if(opt)
					{
						components[j].name  = current_entry->name;
						components[j].type  = FORM_OPTION;	/* override */
						components[j].value = opt->value; 
						j++;
					}
				}
				else
				{
					/* list. Get all selected items and store them */
					int *pos_list, pos_cnt = 0;

					/* must take it from child, parent is a scrolledWindow */
					if((XmListGetSelectedPos(current_entry->child, &pos_list,
						&pos_cnt)))
					{
						XmHTMLForm *opt = NULL;
						int i = 1, k = 0;

						/* don't exceed no of selected items */
						for(opt = current_entry->options; opt != NULL && 
							k < pos_cnt; opt = opt->next, i++)
						{
							if(i == pos_list[k])
							{
								components[j].name  = current_entry->name;
								components[j].type  = FORM_OPTION;
								components[j].value = opt->value; 
								j++;
								k++;
							}
						}
						free(pos_list);	/* don't forget! */
					}
				}
				break;

			/* password entry has really entered text stored */
			case FORM_PASSWD:
				if(current_entry->content != NULL)
					components[j++].value = current_entry->content;
				break;

			/* textfield contents aren't stored by us */
			case FORM_TEXT:
				if((chPtr = XmTextFieldGetString(current_entry->w)) != NULL)
					components[j++].value = chPtr;
				break;

			/*****
			* File contents aren't stored by us and must be taken from the
			* textfield child.
			*****/
			case FORM_FILE:
				if((chPtr = XmTextFieldGetString(current_entry->child)) != NULL)
					components[j++].value = chPtr;
				break;
				
			/*****
			* Textarea contents aren't stored by us and must be taken from
			* the child (current_entry->w is the id of the scrolled window
			* parent for this textarea)
			*****/
			case FORM_TEXTAREA:
				if((chPtr = XmTextGetString(current_entry->child)) != NULL)
					components[j++].value = chPtr;
				break;
				
			/* check/radio boxes are equal in here */
			case FORM_CHECK:
			case FORM_RADIO:
				if(current_entry->checked)
					components[j++].value = current_entry->value;
				break;

			case FORM_IMAGE:
				if(entry == current_entry)
				{ 
					char *xname, *yname;
					char *x, *y;
					xname = calloc(strlen(current_entry->name)+3, sizeof(char));
					yname = calloc(strlen(current_entry->name)+3, sizeof(char));
					x= calloc(16, sizeof(char));
					y= calloc(16, sizeof(char));
					
					memcpy(xname, current_entry->name,
						strlen(current_entry->name)); 
					memcpy(yname, current_entry->name,
						strlen(current_entry->name)); 
					strcat(xname,".x");
					strcat(yname,".y");
					sprintf(x,"%d", event->xbutton.x - entry->data->x); 
					sprintf(y,"%d", event->xbutton.y - entry->data->y); 
					components[j].name  = xname;	/* override */
					components[j].value = x;
					j++;
					components[j].name  = yname;	/* override */
					components[j].value = y;
					j++;
				}
				break;

			/* always return these */
			case FORM_HIDDEN:
				components[j++].value = current_entry->value;
				break;

			/* reset and submit are equal in here */
			case FORM_RESET:
			case FORM_SUBMIT:
				if(entry == current_entry)
					components[j++].value = current_entry->value;
				break;

			case FORM_OPTION:
				/* is a wrapper, so doesn't do anything */
				break;
			case FORM_UNKNOWN:
#ifdef DEBUG
				/* shouldn't happen */
				_XmHTMLWarning(__WFUNC__(html, "_XmHTMLFormActivate"),
					XMHTML_MSG_78);
#endif
				break;
			/* no default */
		}
	}	
	(void)memset(&cbs, 0, sizeof(XmHTMLFormCallbackStruct));

	cbs.reason      = XmCR_HTML_FORM;
	cbs.event       = event;
	cbs.action      = strdup(entry->parent->action);
	cbs.method      = entry->parent->method;
	cbs.enctype     = strdup(entry->parent->enctype);
	cbs.ncomponents = nComponents;
	cbs.components  = components;
	cbs.doc_modified = False;

	XtCallCallbackList((Widget)html, html->html.form_callback, &cbs);

	/* free all */
	for(i = 0; i < j; i++)
	{ 
		/* value of these components is retrieved using XmTextGetValue */
		if(components[i].type == FORM_TEXT || components[i].type == FORM_FILE ||
			components[i].type == FORM_TEXTAREA )
			if(components[i].value) 
				XtFree(components[i].value);
		/* use free to avoid FMM errors in purify */
		if(components[i].type == FORM_IMAGE)
		{
			if(components[i].value) 
				free(components[i].value);
			if(components[i].name) 
				free(components[i].name);
		}
	}
	free(components);
	free(cbs.action);
	free(cbs.enctype);

	return(cbs.doc_modified);
}

/*****
* Name: 		_XmHTMLFormReset
* Return Type: 	void
* Description: 	resets all entries of the given form to their default values.
* In: 
*	html:		XmHTMLWidget id;
*	entry:		form entry that triggered this routine.
* Returns:
*	nothing.
*****/
void _XmHTMLFormReset(XmHTMLWidget html, XmHTMLForm *entry)
{
	XmHTMLFormData *form = entry->parent;
	XmHTMLForm *tmp, *option;
	int i;

	_XmHTMLDebug(12, ("forms.c: _XmHTMLFormReset start\n"));
	for(tmp = form->components; tmp != NULL; tmp = tmp->next)
	{
		_XmHTMLDebug(12, ("\tchecking %s\n", tmp->name));

		switch(tmp->type)
		{
			/* passwd doesn't have a default value, clear it */
			case FORM_PASSWD:
				_XmHTMLDebug(12, ("\t\temptying current password\n"));
				XmTextFieldSetString(tmp->w, NULL);
				if(tmp->content)
				{
					free(tmp->content);
					tmp->content = NULL;
				}
				break;
				
			case FORM_TEXT:
				_XmHTMLDebug(12, ("\t\tsetting XmNvalue to: %s\n", tmp->value));
				XtVaSetValues(tmp->w, XmNvalue, tmp->value, NULL);
				break;

			case FORM_TEXTAREA:
				_XmHTMLDebug(12, ("\t\tsetting XmNvalue to: %s\n", tmp->value));
				XtVaSetValues(tmp->child, XmNvalue, tmp->value, NULL);
				break;

			case FORM_CHECK:
			case FORM_RADIO:
				/* checkbuttons, set default state */
				_XmHTMLDebug(12, ("\t\tsetting state to %s\n", 
					tmp->selected ? "on" : "off"));
				XtVaSetValues(tmp->w, XmNset, (Boolean)tmp->selected, NULL);
				/* store default selection state */
				tmp->checked = (Boolean)tmp->selected;
				break;

			/* clear selection */
			case FORM_FILE:
				_XmHTMLDebug(12, ("\t\temptying current selection\n"));
				XmTextFieldSetString(tmp->child, NULL);
				break;

			case FORM_SELECT:
				if(tmp->multiple || tmp->size > 1)
				{
					/* scrolled list. First deselect all items */
					XmListDeselectAllItems(tmp->child);

					/* now see what options should be selected */
					for(i = 0, option = tmp->options; option != NULL;
						option = option->next, i++)
					{
						if(option->selected)
							XmListSelectPos(tmp->child, i+1, False);
					}
				}
				else
				{
					Widget menu = NULL;
					WidgetList children;
					int i, num_children;
					XmHTMLForm *opt;
					XtVaGetValues(tmp->w, XmNsubMenuId, &menu, NULL);

					XtVaGetValues(menu,
						XmNnumChildren, &num_children,
						XmNchildren, &children,
						NULL);

					/* and set default button  */
					XtVaSetValues(tmp->w,
						XmNmenuHistory, children[tmp->selected], NULL);

					/* reset default selection state */
					for(opt = tmp->options; opt != NULL; opt = opt->next, i++)
					{
						if(opt->w == children[tmp->selected])
							opt->checked = True;
						else
							opt->checked = False;
					}
				}
				break;
			default:
				break;
		}
	}
	_XmHTMLDebug(12, ("forms.c: _XmHTMLFormReset end.\n"));
}

/*****
* form widget traversal handling.
*****/
static Widget
getNextLeader(XmHTMLFormData *curr_group, int *y_pos)
{
	XmHTMLFormData *form;
	XmHTMLForm *entry = NULL;

	my_assert(curr_group != NULL);

	for(form = curr_group->next; form != NULL && entry == NULL;
		form = form->next)
	{
		for(entry = form->components; entry != NULL && entry->w == NULL;
			entry = entry->next);
	}
	_XmHTMLDebug(12, ("forms.c:getNextLeader, returning widget %s\n",
		entry ? XtName(entry->w) : "<NULL>"));

	if(entry)
	{
		*y_pos = entry->y;
		return(entry->w);
	}
	*y_pos = 0;
	return(NULL);
}

static Widget
getPrevLeader(XmHTMLFormData *curr_group, int *y_pos)
{
	XmHTMLFormData *form;
	XmHTMLForm *entry = NULL;

	my_assert(curr_group != NULL);

	/* need to walk all form groups */
	for(form = curr_group->prev; form != NULL && entry == NULL;
		form = form->prev)
	{
		for(entry = form->components; entry != NULL && entry->w == NULL;
			entry = entry->next);
	}
	_XmHTMLDebug(12, ("forms.c:getPrevLeader, returning widget %s\n",
		entry ? XtName(entry->w) : "<NULL>"));

	*y_pos = 0;
	if(entry)
	{
		*y_pos = entry->y;
		return(entry->w);
	}
	return(NULL);
}

static Widget
getNextTab(XmHTMLForm *curr_tab, Boolean start_at_current, int *y_pos)
{
	XmHTMLForm *entry = NULL;

	my_assert(curr_tab != NULL);

#ifdef DEBUG
	_XmHTMLDebug(12, ("forms.c: getNextTab, form listing:\n"));
	for(entry = curr_tab->parent->components; entry != NULL;
		entry = entry->next)
	{
		_XmHTMLDebug(12, ("\t%s", entry->name));
		if(entry == curr_tab)
			_XmHTMLDebug(12, ("\t(--> selected)"));
		_XmHTMLDebug(12, ("\n"));
	}
#endif

	/*****
	* If start_at_current is set, we need to start at the widget with the
	* focus. This can happen when we are traversing down to the widget tree
	* for the *first* time, e.i., the current widget is the HTML widget itself.
	*****/
	if(start_at_current)
		entry = curr_tab;
	else
		entry = curr_tab->next;

	/* get to next visible widget */
	for(; entry != NULL && entry->w == NULL; entry = entry->next);

	_XmHTMLDebug(12, ("forms.c:getNextTab , returning widget %s\n",
		entry ? XtName(entry->w) : "<NULL>"));

	*y_pos = 0;
	if(entry)
	{
		*y_pos = entry->y;
		return(entry->w);
	}
	/* if we didn't find a next entry, get leader of next form */
	return(getNextLeader(curr_tab->parent, y_pos));
}

static Widget
getPrevTab(XmHTMLForm *curr_tab, int *y_pos)
{
	XmHTMLForm *entry = NULL;

	my_assert(curr_tab != NULL);

#ifdef DEBUG
	_XmHTMLDebug(12, ("forms.c: getPrevTab, form listing:\n"));
	for(entry = curr_tab->parent->components; entry != NULL;
		entry = entry->next)
	{
		_XmHTMLDebug(12, ("\t%s", entry->name));
		if(entry == curr_tab)
			_XmHTMLDebug(12, ("\t(--> selected)"));
		_XmHTMLDebug(12, ("\n"));
	}
#endif

	/* walk to previously visible widget */
	for(entry = curr_tab->prev; entry != NULL && entry->w == NULL;
			entry = entry->prev);

	_XmHTMLDebug(12, ("forms.c:getPrevTab , returning widget %s\n",
		entry ? XtName(entry->w) : "<NULL>"));

	*y_pos = 0;
	if(entry)
	{
		*y_pos = entry->y;
		return(entry->w);
	}
	/* if we didn't find a previous entry, get leader of previous form */
	return(getPrevLeader(curr_tab->parent, y_pos));
}

/*****
* Name: 		_XmHTMLProcessTraversal
* Return Type: 	void
* Description: 	XmHTML's version of XmProcessTraversal, required for proper
*				traversal amongst form widgets.
* In: 
*	w:			widget requiring traversal;
*	direction:	which way we should go;
* Returns:
*	nothing, but keyboard focus is changed. As a side effect this routine
*	*can* cause vertical and/or horizontal scrolling to happen (to place a
*	certain widget into focus).
*****/
void
_XmHTMLProcessTraversal(Widget w, int direction)
{
	XmHTMLWidget html;
	XmHTMLFormData *form, *curr_group;		/* current tabgroup */
	XmHTMLForm *entry = NULL, *curr_tab;	/* item that has the focus */
	Widget parent;							/* XmHTML parent */
	Widget current;							/* widget that has the focus */
	Widget next;							/* widget receiving focus */
	int y = 0;

	_XmHTMLDebug(12, ("forms.c: _XmHTMLProcessTraversal, direction = %i\n",
		direction));

	/* first get the XmHTML parent */
	current = parent = w;
	while(parent && !XmIsHTML(parent))
		parent = XtParent(parent);

	/* not found, nothing to do */
	if(!parent || !XmIsHTML(parent))
	{
		_XmHTMLDebug(12, ("forms.c: _XmHTMLProcessTraversal, end, no parent "
			"found\n"));
		return;
	}

	html = (XmHTMLWidget)parent;

	/*****
	* Check if we have any forms in here. If we don't we move traversal
	* on to Motif.
	*****/
	if(html->html.form_data == NULL)
	{
		_XmHTMLDebug(12, ("forms.c: _XmHTMLProcessTraversal, end, no form "
			"present\n"));
		/* pass down to Motif */
		XmProcessTraversal(w, direction);
		return;
	}

	/* now get the XmHTML parent */
	while(!XtIsShell(parent) && !XtIsTopLevelShell(parent))
		parent = XtParent(parent);

	/*****
	* Now get the widget that has the focus. If the current widget equals
	* the widget ID of the workArea, we are about to move into the form
	* hierarchy and thus everything should be initialized to the first form
	* widget. Otherwise we need to compare widget id's.
	*****/
	if(w != html->html.work_area)
	{
		form = html->html.form_data;
		while(form != NULL)
		{
			for(entry = form->components; entry != NULL; entry = entry->next)
			{
				_XmHTMLDebug(12, ("forms.c: _XmHTMLProcessTraversal, "
					"comparing %s with %s\n", entry->name, XtName(w)));
				if(entry->w == w)
					break;
			}
			if(entry)
				break;
			form = form->next;
		}
		/* no widget has got the focus (yet) */
		if(!entry)
		{
			_XmHTMLDebug(12, ("forms.c: _XmHTMLProcessTraversal, end, no "
				"valid entry found!\n"));
			return;
		}
		curr_group = form;
		curr_tab = entry;
		current = entry->w;
	}
	else
	{
		current = w;
		curr_group = html->html.form_data;
		curr_tab = curr_group->components;
	}

	_XmHTMLDebug(12, ("forms.c: _XmHTMLProcessTraversal, traversing %s\n",
		XtName(current)));

#ifdef DEBUG
	_XmHTMLDebug(12, ("forms.c: _XmHTMLProcessTraversal, using traversal "));

	switch(direction)
	{
		case XmTRAVERSE_CURRENT:
			_XmHTMLDebug(12, ("XmTRAVERSE_CURRENT\n"));
			break;
		case XmTRAVERSE_DOWN:
			_XmHTMLDebug(12, ("XmTRAVERSE_DOWN\n"));
			break;
		case XmTRAVERSE_RIGHT:
			_XmHTMLDebug(12, ("XmTRAVERSE_RIGHT\n"));
			break;
		case XmTRAVERSE_NEXT:
			_XmHTMLDebug(12, ("XmTRAVERSE_NEXT\n"));
			break;
		case XmTRAVERSE_UP:
			_XmHTMLDebug(12, ("XmTRAVERSE_UP\n"));
			break;
		case XmTRAVERSE_LEFT:
			_XmHTMLDebug(12, ("XmTRAVERSE_LEFT\n"));
			break;
		case XmTRAVERSE_PREV:
			_XmHTMLDebug(12, ("XmTRAVERSE_PREV\n"));
			break;
		case XmTRAVERSE_HOME:
			_XmHTMLDebug(12, ("XmTRAVERSE_HOME\n"));
			break;
		case XmTRAVERSE_NEXT_TAB_GROUP:
			_XmHTMLDebug(12, ("XmTRAVERSE_NEXT_TAB_GROUP\n"));
			break;
		case XmTRAVERSE_PREV_TAB_GROUP:
			_XmHTMLDebug(12, ("XmTRAVERSE_PREV_TAB_GROUP\n"));
			break;
		default:
			_XmHTMLDebug(12, ("unknown traversal method!!\n"));
			break;
	}
#endif

	switch(direction)
	{
		case XmTRAVERSE_CURRENT:
			/* focus stays at current */
			next = current;
			break;

		case XmTRAVERSE_NEXT:
			next = getNextTab(curr_tab, current == html->html.work_area, &y);
			break;

		case XmTRAVERSE_PREV:
			next = getPrevTab(curr_tab, &y);
			break;

		case XmTRAVERSE_HOME:
			next = (Widget)html;
			break;

		case XmTRAVERSE_NEXT_TAB_GROUP:
			next = getNextLeader(curr_group, &y);
			break;

		case XmTRAVERSE_PREV_TAB_GROUP:
			next = getPrevLeader(curr_group, &y);
			break;

		default:
			next = current;
			break;
	}
	/*
	* Set focus if we've found a valid widget. If we didn't find one, pass
	* down to Motif using the set action.
	*/
	if(next)
	{
		_XmHTMLDebug(12, ("forms.c: _XmHTMLProcessTraversal, setting focus "
			"to %s\n", XtName(next)));
		/*****
		* check if this widget is fully visible. If not we scroll so it
		* becomes visible.
		*****/
#if 0
		/*****
		* FIXME
		* must use bounding box to do this properly.
		*****/
		if(y > (html->html.scroll_y + html->html.work_height) ||
			y < html->html.scroll_y)
		{
			int max = 0, size = 0, value = y;
			XtVaGetValues(html->html.vsb,
				XmNmaximum, &max,
				XmNsliderSize, &size,
				NULL);
			if(value > (max - size))
				value = (max - size);
			_XmHTMLMoveToPos(html->html.vsb, html, value);
		}
#endif
		XtSetKeyboardFocus(parent, next);
	}
	else
	{
		_XmHTMLDebug(12, ("forms.c: _XmHTMLProcessTraversal, passing down to "
			"Motif (name of widget passed is %s)\n", XtName(w)));
		XmProcessTraversal(w, direction);
	}
}

/*****
* Name:			_XmHTMLFormCreateClipmasks
* Return Type: 	Boolean
* Description: 	computes a clipping bitmap required for proper scrolling of
*				all HTML form widgets. One clipmask is created for all
*				possible form entries.
* In: 
*	html:		XmHTMLWidget id;
* Returns:
*	True when a clipmask was successfully created, False if not.
*	Previous clipmask is destroyed and a new one based on the current layout
*	is created.
*****/
Boolean
_XmHTMLFormCreateClipmask(XmHTMLWidget html)
{
	XmHTMLFormData *form, *master;
	XmHTMLForm *entry;
	ToolkitAbstraction *tka = HTML_ATTR(tka);
	int lx = 0xfffffff, ly = 0xfffffff;
	int rx = 0, ry = 0;
	int nfound = 0;
	GC tmpGC;

	master = HTML_ATTR(form_data);

	if(master == NULL || master->can_clip == False)
		return(False);

	/*****
	* Disabled for the time being. Still working on it.
	*****/
	master->can_clip = False;
	return(False);

#if 0

	/* release any previous clipmask */
	FreePixmap(tka->dpy, master->clip);

	master->clip = None;
	master->x = 0;
	master->y = 0;
	master->width = 0;
	master->height = 0;

	for(form = master; form != NULL; form = form->next)
	{
		/* compute clipmask position & dimension */
		for(entry = form->components; entry != NULL; entry = entry->next)
		{
			if(entry->w)
			{
				int llx, lly, rrx, rry;

				llx = entry->data->x;
				lly = entry->data->y;
				rrx = llx + entry->data->width;
				rry = lly + entry->data->height;

				if(lx > llx)
					lx = llx;
				if(rx < rrx)
					rx = rrx;
				if(ly > lly)
					ly = lly;
				if(ry < rry)
					ry = rry;
				nfound++;
			}
		}
	}
	if(0 == nfound)
	{
		master->can_clip = False;
		return(False);
	}

	/* sanity */
	if(0xfffffff == lx)
		lx = 0;
	if(0xfffffff == ly)
		ly = 0;
#ifdef 0
	master->x = lx;
	master->y = ly;
	master->width = rx - lx;
	master->height = ry - ly;
#endif
	master->x = 0;
	master->y = 0;
	master->width = HTML_ATTR(formatted_width);
	master->height = HTML_ATTR(formatted_height);

	if(0 == master->height || 0 == master->width)
	{
		master->can_clip = False;
		return(False);
	}

	_XmHTMLDebug(12, ("forms.c: _XmHTMLFormCreateClipmask, computed "
		"form dimensions as %ix%y:%i-%i\n", master->x, master->y,
		master->width, master->height));

	master->clip = tka->CreatePixmap(tka->dpy, tka->win, master->width,
		master->height, 1);

	/* sanity */
	if(None == master->clip)
	{
		_XmHTMLDebug(12, ("forms.c: _XmHTMLFormCreateClipmask, XCreatePixmap "
			"failed, falling back to default behavior.\n"));
		master->can_clip = False;
		return(False);
	}

	/* temporary gc */
	tmpGC = tka->CreateGC(tka->dpy, master->clip, 0, 0);

	/* initially everything is drawn */
	tka->SetForeground(tka->dpy, tmpGC, 1);
	tka->FillRectangle(tka->dpy, master->clip, tmpGC, 0, 0,
		master->width, master->height);

	/* remove space occupied by the form components */
	tka->SetForeground(tka->dpy, tmpGC, 0);

	for(form = master; form != NULL; form = form->next)
	{
		for(entry = form->components; entry != NULL; entry = entry->next)
		{
			if(entry->w)
			{
				/* positions based on upper-left corner */
				int xs = entry->data->x - master->x;
				int ys = entry->data->y - master->y;
				tka->FillRectangle(tka->dpy, master->clip, tmpGC, xs, ys,
					entry->data->width, entry->data->height);
			}
		}
	}
	/* no longer needed */
	tka->FreeGC(tka->dpy, tmpGC);

#ifdef DEBUG
	if(HTML_ATTR(debug_save_clipmasks))
	{
		char name[128];
		static int num;
		/* write out so we can check it */
		sprintf(name, "form-clipmask.%i.xbm", num);
		XWriteBitmapFile(tka->dpy, name, master->clip, master->width,
			master->height, 0, 0);
		fprintf(stderr, "Wrote <FORM> clipping bitmap to file %s\n", name);
		num++;
	}
#endif

	return(True);
#endif
}
