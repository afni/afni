.. _help_functions:

==========================
How to Format Help Strings
==========================

   This section explains how to go about writing the help section for command-line programs in a manner that is consistent with the AFNI standard of trickery. The process is somewhat different for the different types of programs as detailed below. But first a quick recap of the trickeries.
   
   The point of all this is to allow a fancier formatting of the help output without much effort. Existing help output can be automatically sphinxized with the command::
      
      apsearch -asphinx_phelp PROGNAME
      
.. _hidden_C_formatting:

   However, you can also add markup directives in the help string that alters the output depending on the desrired format. For examples on how to add markup into the help strings, run::
   
      apsearch -doc_markup_sample
   
   .. include:: auto_inc/apsearch_txt.inc


C-programs
----------

   * When writing a new program
      
      * See program 3dToyProg.c for an example on how to format your help, and some examples for how to put special markup in the help strings.

   * To retrofit existing C programs
   
      * Put all help in a function of this prototype::

         int Help_Func(TFORM targ, int detail);


      * Replace printing functions as follows::

         fprintf( --> sphinx_fprintf(targ,
          printf( --> sphinx_printf(targ,

      * Replace the explanation for the *-help* option in Help_Func() with the string from::
      
         SUMA_Offset_SLines(get_help_help(),OFF);

         where OFF is the number of blank characters by which to offset the help string

      * Add CHECK_HELP(arg,Help_Func) macro in the block where you parse the command line arguments. 

         .. note::

            **All parsing decisions** should be made after macro mainENTRY(). Otherwise your program won't make proper use of things like -h_view, and -h_web, etc.

            **Do not parse for -h, -help, or -HELP**. That is now done by CHECK_HELP()

      * Add the following after you detect an illegal option in *arg*::

         suggest_best_prog_option(argv[0], *arg*);

.. _prog_opts:

      * Once you add a new program/script, or upgrade the -help output for an old one, you need to update prog_opts.c and rebuild libmri.a by running this command in src/ directory, to replace existing prog_opts.c::

         apsearch -C_prog_opt_array 3dToyProg > prog_opts.c
         touch thd_getpathprogs.c 
         make libmri.a gitignore

C-GUI
-----

   The documentation for the GUI can be largely automated using existing BHelp and Hint strings. The idea is to put all documentation in one place and display it in multiple ways. The logic behind the process is as follows:
      
         * Keep track of each GUI element's name and its help strings. This is done by replacing all calls to *MCW_register_help()* and *MCW_register_hint()* with a single call to: *SUMA_Register_Widget_Help()*.  *SUMA_Register_Widget_Children_Help()* also replaces *MCW_reghint_children()* and *MCW_reghelp_children()*. 
         
            The prototypes of the old and new functions are::
         
               void MCW_register_help(Widget w , char *help);
               void MCW_register_hint(Widget w , char *hint);
               SUMA_Boolean SUMA_Register_Widget_Help(Widget w, int type, char *name, char *hint, char *help);
         
            The extra variables are **type** and **name** and are used as follows:
         
               **type** is 0 for a container widget, like frames around buttons. An example frame is the :ref:`Surface Properties<SurfCont->Surface_Properties>` in SUMA. **type** is 1 for clickable widgets such as :ref:`Trn<SurfCont->Surface_Properties->Trn>`. 
         
               **name** is a string identifying the widget. The naming has a hierarchical scheme to it. For instance the name for :ref:`Trn<SurfCont->Surface_Properties->Trn>` in the C-code was *"SurfCont->Surface_Properties->Trn"*. Because this widget is in the Surface Controller within the frame called Surface Properties. Names should be unique to get an entry into the help database (a bunch of strings really), but it is expected that there will be indentically named widgets such as when you open two surface controllers.
         
               When building the documentation for the whole GUI, you are expected to have an entry for each naming level. To get to "SurfCont->Surface_Properties->Trn", you should have at the very least three calls to *SUMA_Register_Widget_Help()*, one for **name** "SurfCont" where you say a few kind words about the overall use of the Surface Controller GUI, then for "SurfCont->Surface_Properties", and then for "SurfCont->Surface_Properties->Trn" and whatever else is in it.
         
               In AFNI, frames do not have labels that show up in the GUI, no "Surface Properties" for instance, but you should still name the frame, hopefully with something memorable so you can easily cross reference it when writing help.
         
               Now let's consider what was needed to document a tiny part of the AFNI controller, the done button. To do so, we also need to include something for its containers. 
         
               1. Add an entry for "AfniCont"::
         
                     /* replace */
                     MCW_register_help( vwid->top_form , AFNI_tophelp ) ;
                     /* with */
                     SUMA_Register_Widget_Help( vwid->top_form, 0, "AfniCont", NULL, AFNI_tophelp);

               2. Add an entry for "AfniCont->ProgCont", "ProgCont" stands for "program controls", a phrase I took from the comment in afni_widg.c::
         
                     /* replace */
                     MCW_register_hint( prog->frame , "rowcol to hold all program controls stuff" ) ;
                     MCW_register_help( prog->frame , "Wish to write something here?" ) ;
                     /* with */
                     SUMA_Register_Widget_Help( prog->frame, 0, "AfniCont->ProgCont", "rowcol to hold all program controls stuff", "Wish to write something here?");

               3. Now for the button "AfniCont->ProgCont->done"::
         
                     /* replace */
                     MCW_register_hint( prog->quit_pb, "Click twice to close window");
                     MCW_register_help( prog->quit_pb, AFNI_quit_help);
                     /* with */
                     SUMA_Register_Widget_Help(prog->quit_pb, 1, "AfniCont->ProgCont->done", "Click twice to close window", AFNI_quit_help);
                                   
         
            .. Note::

               You can enhance the help strings with links, images, references, etc. that would get shown only for the Sphinx output. For some help on how to do this take a look at the output of :ref:`apsearch -doc_markup_sample<hidden_C_formatting>`

         * Write a function to assemble all the help information for the controller in question, here "AfniCont". You can use the function below as your template and add your widgets to the decalaration of **worder[]**. It goes without saying that each name in **worder** should have a corresponding call to *SUMA_Register_Widget_Help()* with an identical name.::
         
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

         ..
         
         
         * Automate the process of help generation, by using *@gen_all* script under doc/SphinxDocs/ with the -afni option. The script would launch afni and make it open whatever controllers are to be documented and issue a driver command that calls the function to generate all the help text. For the AFNI controller example above, the driver command "WRITE_CONT_SPX_HELP" will call AFNI_Help_AllMainCont() with TFORM set to SPX. 
         
         * It is also convenient to have a function to automatically snap pictures of controllers or controller groups. This way you don't have to manually recreate a bunch of pictures after you modify the GUI. Since snapping is to be done via :ref:`plugout_drive<plugout_drive>` (SNAP_CONT for this example) it is best to have the snapping in afni_driver.c. You must make sure that the controller is open and setup as you want it to be before snapping the picture or writing the help string. No help is availble until the GUI in question is created and displayed. The function that snaps a picture of the Afni controller is::
         
            static int AFNI_drive_snap_cont( char *cmd );
         
         * Automatically generated help files are included from manually created .rst files such as SphinxDocs/AFNI/Controllers.rst
          
         * Finally, to build the docs::
         
            cd SphinxDocs
            @gen_all -afni -html
            afni_open _build/html/index.html 
         

C-Shell Scripts
---------------

   * Add the following line right after the **#!/bin/tcsh -f** line::
   
      @global_parse `basename $0` "$*" ; if ($status) exit 0

   * Next you will need to add blocks, typically at the end of your scripts that will contain:
         
      * The parsing of the special help options in *HELP*

      * The actual help string, conveniently tucked in *HRAW*

      * The use of the *END* block is to keep the script from getting to these sections without an explicit goto.
         
      * Here is an example for what you would add at the end of the script::
   
         goto END

         HELP:
              if ("$HelpOpt" == "-h_raw") then
            goto HRAW
         else if ("$HelpOpt" == "-h") then
            `basename $0` -h_raw | apsearch -hdoc_2_txt `basename $0` -
         else if ("$HelpOpt" == "-help") then
            `basename $0` -h_raw | apsearch -hdoc_2_txt `basename $0` -
         else if ("$HelpOpt" == "-h_txt") then
            `basename $0` -h_raw | apsearch -hdoc_2_txt `basename $0` -
         else if ("$HelpOpt" == "-h_spx") then
            `basename $0` -h_raw | apsearch -hdoc_2_spx `basename $0` -
         else if ("$HelpOpt" == "-h_aspx") then
            `basename $0` -h_raw | apsearch -hdoc_2_aspx `basename $0` -
         endif
         goto END

         HRAW:
         cat << EOF
         A multi line string that contains all your help, something like:
         Usage: @DriveSuma 

         A script to demonstrate how to drive suma from the command line.
         The script pops messages explaining what will happen in the next command

         You can also read the script, focusing on the DriveSuma commands 
         to understand what is going on.

         See also DriveSuma -help and @DO.examples

         Questions or comments are welcome on AFNI's message board:
         echo ' https://afni.nimh.nih.gov/afni/community/board/list.php?f=1 '
         :SPX:

            .. note::

               This is an example for how you can put special sphinx directives to improve your help. 

         :SPX:

         `@global_parse -gopts_help_formats`

         EOF


         goto END        

         END:
      
   * To get to these sections from where you are parsing the command line arguments, you can add::
      
      set HelpOpt = ''
      echo "$YourArg" | \grep -w -E  '\-h_txt|\-h_spx|\-h_aspx|\-h_raw|\-help|\-h' >& /dev/null
      if ($status == 0) then
         set HelpOpt = "$YourArg"
         goto HELP
      endif
      
   * And finally, update the list of program options in :ref:`prog_opts.c<prog_opts>`


R programs
----------

   * Add formatting argument *targ* to your help function as in::
   
      help.RprogDemo.opts <- function (params, alpha = TRUE, 
                                       itspace='   ', adieu=FALSE, targ ='TXT')
                                       
                                    
   * Add a *file* argument to the command that cats the help::
      
         cat(intro, ex1, ss, sep='\n', 
             file=help.cat.file.AFNI(ExecName,targ));

   * Augment the help options to something like::
   
      '-help' = apl(n=0, h = '-help: this help message, in simple text.\n'),
      '-h_raw' = apl(n=0, h = '-h_raw: this help message, as is in the code.\n'),
      '-h_txt' = apl(n=0, h = '-h_txt: this help message, in simple text\n'),
      '-h_spx' = apl(n=0, h = '-h_spx: this help message, in sphinx format\n'),
      '-h_aspx' = apl(n=0, h = '-h_aspx: like -h_spx, with autolabeling\n'),
      
   * And reflect that in the parsing section::
   
       help = help.RprogDemo.opts(params, adieu=TRUE),
       h_raw = help.RprogDemo.opts(params, adieu=TRUE, targ='RAW'),
       h_spx = help.RprogDemo.opts(params, adieu=TRUE, targ='SPX'),
       h_aspx = help.RprogDemo.opts(params, adieu=TRUE, targ='ASPX'),
       h_txt = help.RprogDemo.opts(params, adieu=TRUE, targ='TXT'),

.. note:: 
   
      For an example, take a look at program **3dRprogDemo**
   
   
Python programs
---------------

   TBW


Building one help file
----------------------

   To compile the rst file into html, you can use the convenience script **@test_html_build** which is in the doc/SphinxDocs/ directory. Here is a sample command to test the build and view the help output of 3dToyProg::
   
       3dToyProg -h_aspx | @test_html_build -
       
   or if your program does not yet support the new help options::
   
       apsearch -asphinx_phelp 3dToyProg | @test_html_build -  
