#include "SUMA_suma.h"

/*
 update 03/08/2017 Justin Rajendra
 changed to be 3 buttons with different text for each button
 returns 1,2,3 for the 3 buttons
 if you don't provide any button labels, it will show Ok
 Based on SUMA_prompt_user.c 
 */


static char * read_file_text(FILE * fp);  /* 29 Jun 2012 [rickr] */

// help!
void usage_prompt_popup (SUMA_GENERIC_ARGV_PARSE *ps)
{
    static char FuncName[]={"usage_prompt_popup"};
    int i;
    printf (
            "\n"
            "Usage: prompt_popup -message MESSAGE -button HELLO \n"
            "  -message MESSAGE: Pops a window prompting the user with MESSAGE.\n"
            "                    Program does not return until user responds.\n"
            "                    note: if MESSAGE is '-', it is read from stdin\n"
            "  -pause MESSAGE:   Same as -message to match the old prompt_user\n"
            "  -button LABEL:    What do you want the buttons to say?\n"
            "                    You can give up to three -button for three buttons.\n"
            "                    Returns integer 1, 2, or 3.\n"
            "                    If there is no -button, there will be one button 'Ok'\n"
            "  -b LABEL:         Same as -button.\n"
            "  -timeout TT:      Timeout in seconds of prompt message. Default answer\n"
            "                    is returned if TT seconds elapse without user\n"
            "                    input.\n"
            "  -to TT:           Same as -timeout TT\n"
            "\n"
            "example: prompt_popup -message 'Best disco ever?' -b Earth -b Wind -b Fire\n"
            "\n");

    printf("Justin Rajendra March 2017 (stolen mostly from Ziad S. Saad)\n");
    exit(0);
}

// stolen from SUMA_PauseForUser
int SUMA_PauseForUserDisco(  Widget parent,char *question,char *yes_user,char *no_user,
                           char *help_user,SUMA_WINDOW_POSITION pos,XtAppContext *app,
                           int withCancel,int withHelp,float timeout)
{
    static char FuncName[]={"SUMA_PauseForUserDisco"};
    static Widget dialog = NULL; /* static to avoid multiple creation */

    // for each button (defaults are Ok, Cancel, Help)
    Widget YesWid;
    Widget NoWid;
    Widget HelpWid;

    int ii;
    XmString text, yes, no, help;
    struct  timeval  tt;
    static int answer;
    SUMA_Boolean LocalHead = NOPE;

    SUMA_ENTRY;
    if (!parent) {
        /* look for the first non-null sv->X->TOPLEVEL */
        ii = 0;
        while (ii<SUMAg_N_SVv && !(parent=SUMAg_SVv[ii].X->TOPLEVEL)) {
            ++ii;
        }
    }
    if (!parent) { /* no widgets, go command line */
        SUMA_PAUSE_PROMPT_STDIN(question);
        SUMA_RETURN(SUMA_YES);
    }
    if (!dialog) {
        SUMA_LH("Creating Dialog");
        dialog = XmCreateQuestionDialog (parent, "dialog", NULL, 0);

        // check to see if you want the 2nd and 3rd button
        if (!withHelp) {
            XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON));
        } else {
            XtAddCallback (dialog, XmNhelpCallback, SUMA_response, &answer);
        }
        if (!withCancel) {
            XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_CANCEL_BUTTON));
        } else {
            XtAddCallback (dialog, XmNcancelCallback, SUMA_response, &answer);
        }

        // always want the ok button
        XtAddCallback (dialog, XmNokCallback, SUMA_response, &answer);

    } else {
        SUMA_LH("Reusing Dialog (SLOW SLOW SLOW)");
    }

    // set time
    SUMA_etime(&tt, 0);

    // text for buttons
    text = XmStringCreateLocalized (question);
    yes = XmStringCreateLocalized (yes_user);
    no = XmStringCreateLocalized (no_user);
    help = XmStringCreateLocalized (help_user);
    answer = 0;

    // set to widget?
    XtVaSetValues (dialog,
                   XmNmessageString,      text,
                   XmNokLabelString,      yes,
                   XmNcancelLabelString,  no,
                   XmNhelpLabelString,    help,
                   XmNdefaultButtonType,  XmDIALOG_OK_BUTTON,
                   NULL);
    // free!
    XmStringFree (text);
    XmStringFree (yes);
    XmStringFree (no);
    XmStringFree (help);

    // set the values of the standard buttons
    YesWid = XmMessageBoxGetChild(dialog, XmDIALOG_OK_BUTTON);
    NoWid = XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON);
    HelpWid = XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON);

    // return values for each button (change here if you want different integers)
    XtVaSetValues(YesWid, XmNuserData, 1, NULL);
    XtVaSetValues(NoWid, XmNuserData, 2, NULL);
    XtVaSetValues(HelpWid, XmNuserData, 3, NULL);

    XtManageChild (dialog);
    XtPopup (XtParent (dialog), XtGrabNone);

    if (pos != SWP_DONT_CARE) SUMA_PositionWindowRelative(dialog, parent, pos);
    if (!app) app = &(SUMAg_CF->X->App);

    if (timeout < 0.0) { /* no timer */
        while ( (answer == 0 && XtIsManaged(dialog)) ) {
            XtAppProcessEvent (*app, XtIMAll);
        }
    } else {
        while ( (answer == 0 && XtIsManaged(dialog)) ) {
            if (timeout < 0.0 || SUMA_etime(&tt,1) < timeout) {
                if (XtAppPending(*app)) { XtAppProcessEvent (*app, XtIMAll); }
            } else {
                XtVaGetValues(YesWid, XmNuserData, &answer, NULL);
                break;
            }
        }
    }  // end time out
#if 1
    SUMA_LH("destroying dialog");

    XtDestroyWidget(dialog);   /* This won't get the widget off of the screen
                                unless there is an XtAppMainLoop running.
                                When that is not the case, you need to
                                trigger an event processing call, see
                                SUMA_prompt_user.c for an example */

    dialog = NULL;
#else /* bad, takes for ever to come back up.
Same for repeated calls of ForceUser if created for the first
time from DriveSuma and not from the interface with, say 'shft+Esc'
See bit of illustration code in SUMA_Engine where PauseForUser
is called*/
    XtUnmanageChild(dialog);
#endif
    SUMA_RETURN(answer);
}

// parse the arguments
SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_prompt_popup_ParseInput(char *argv[],int argc,
                                                              SUMA_GENERIC_ARGV_PARSE *ps)
{
    static char FuncName[]={"SUMA_prompt_popup_ParseInput"};
    SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
    int kar;
    SUMA_Boolean brk;
    SUMA_Boolean LocalHead = NOPE;

    SUMA_ENTRY;
    Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
    Opt->ps = ps;  /* just hold it there for convenience */
    Opt->flt1 = -1.0;

    // for number of answers
    Opt->n_in_namev = 0;

    kar = 1;
    brk = NOPE;
    while (kar < argc) { /* loop accross command ine options */
        if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
            ps->hverb = 1;
            usage_prompt_popup(ps);
            exit (0);
        }

        SUMA_SKIP_COMMON_OPTIONS(brk, kar);

        if (!brk && (strcmp(argv[kar], "-debug") == 0))
        {
            if (kar+1 >= argc)
            {
                fprintf (SUMA_STDERR, "need a number after -debug \n");
                exit (1);
            }
            Opt->debug = atoi(argv[++kar]);
            brk = YUP;
        }

        // the question
        if (!brk && (
                     (strcmp(argv[kar], "-message") == 0) ||
                     (strcmp(argv[kar], "-pause") == 0)) )
        {
            if (kar+1 >= argc)
            {
                fprintf (SUMA_STDERR, "need a string after -message \n");
                exit (1);
            }
            Opt->b1 = 1;
            Opt->in_name = argv[++kar];
            brk = YUP;
            if (!strcmp(Opt->in_name, "-")) Opt->in_name = read_file_text(stdin);
        }

        // timing
        if (!brk && (
                     (strcmp(argv[kar], "-timeout") == 0) ||
                     (strcmp(argv[kar], "-to") == 0)) )
        {
            if (kar+1 >= argc)
            {
                fprintf (SUMA_STDERR,
                         "need a time in seconds after -timeout/-to \n");
                exit (1);
            }
            Opt->flt1 = atof(argv[++kar]);
            brk = YUP;
        }

        // answers (check for how many there are)
        if (!brk && (
                     (strcmp(argv[kar], "-button") == 0) ||
                     (strcmp(argv[kar], "-b") == 0)) )
        {
            if (kar+1 >= argc)
            {
                fprintf (SUMA_STDERR,"Need a button label!\n");
                exit (1);
            }
            if ( Opt->n_in_namev >= 3 )
            {
                fprintf (SUMA_STDERR,"You only get 3 buttons!\n");
                exit (1);
            }
            // increment for each answer and save to that index
            Opt->n_in_namev++;
            Opt->in_namev[Opt->n_in_namev] = argv[++kar];
            brk = YUP;
        }

        // oops
        if (!brk && !ps->arg_checked[kar]) {
            SUMA_S_Errv("Option %s not understood.\n"
                        "Try -help for usage\n",
                        argv[kar]);
            exit (1);
        } else {
            brk = NOPE;
            kar ++;
        }
    }
    SUMA_RETURN(Opt);
}

/* return all of fp (stdin, probably) in a string */
static char * read_file_text(FILE * fp)
{
    static char FuncName[]={"read_file_text"};
    char * str, ch;
    int    i, len, nalloc;

    SUMA_ENTRY;

    if ( ! fp ) SUMA_RETURN(NULL);

    str = NULL;
    len = 0;
    nalloc = 1;  /* add space for nul term */
    while ( ! feof(fp) ) {
        nalloc += 100; /* read block size */
        str = realloc(str, nalloc * sizeof(char));
        if( !str ) {
            fprintf(stderr,"** RFT alloc fail on len %d\n", nalloc);
            SUMA_RETURN(NULL);
        }
        for( i=0; i < 100 && !feof(fp); i++ )
            str[len++] = fgetc(fp);
        if( feof(fp) ) len--;
    }
    str[len] = '\0'; /* terminate */

    SUMA_RETURN(str);
}

int main (int argc,char *argv[])
{/* Main */
    static char FuncName[]={"prompt_popup"};
    SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;
    SUMA_GENERIC_ARGV_PARSE *ps=NULL;
    char * esc_str = NULL;
    int ii;
    int cancel_sel;
    int help_sel;
    Widget w=NULL;
    XtAppContext    app;
    XEvent ev;
    XtInputMask pp;
    SUMA_Boolean LocalHead = NOPE;
    SUMA_STANDALONE_INIT;
    SUMA_mainENTRY;

    /* Allocate space for DO structure */
    SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
    ps = SUMA_Parse_IO_Args(argc, argv, "");

    if (argc < 2) {
        usage_prompt_popup(ps);
        exit (1);
    }

    // parse the arguments
    Opt = SUMA_prompt_popup_ParseInput (argv, argc, ps);

    // see how many answers and give 1-3 buttons
    if ( Opt->n_in_namev == 1 ) {
        cancel_sel = 0;
        help_sel = 0;
    } else if ( Opt->n_in_namev == 2 ) {
        cancel_sel = 1;
        help_sel = 0;
    } else if (Opt->n_in_namev == 0){
        cancel_sel = 0;
        help_sel = 0;
    }  else  {
        cancel_sel = 1;
        help_sel = 1;
    }

    w = XtOpenApplication(&app, "prompt_popup",
                          NULL, 0, &argc, argv,
                          SUMA_get_fallbackResources(),
                          topLevelShellWidgetClass, NULL, 0);

    switch (Opt->b1) {
        case 1:
            /* apply some escape characters     31 Jul 2009 [rickr] */
            esc_str = unescape_unix_str(Opt->in_name);

            // popup buttons and return int answer
            ii = SUMA_PauseForUserDisco(w, esc_str,
                                        Opt->in_namev[1],
                                        Opt->in_namev[2],
                                        Opt->in_namev[3],
                                        SWP_POINTER_LEFT_BOTTOM,&app,
                                        cancel_sel,help_sel,Opt->flt1);
            fprintf(SUMA_STDOUT,"%d\n", ii);
            break;
        default:
            SUMA_S_Err("Bad opt");
            exit(1);

    }

    /* because you have no XtAppMainLoop, you'll need to process the next
     event for when the XtDestroy command on w's child takes effect. So you'll
     just have this zombie widget that stares at you.
     In this simple command line program, the widget dies anyway when you
     exit the program, so the call below is a teaching moment for when
     functions like SUMA_PauseForUser are called from programs without an
     XtAppMainLoop.
     See also SUMA_PAUSE_PROMPT macro */

    while ((pp = XtAppPending(app))) {
        XtAppProcessEvent(app, pp);
    }
    if (Opt->debug > 2) LocalHead = YUP;
    if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
    if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
    if (!SUMA_Free_CommonFields(SUMAg_CF))
        SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);

    if( esc_str ) free(esc_str);

    exit(0);

}
