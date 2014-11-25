.. _help_functions:

==========================
How to Format Help Strings
==========================

   This section explains how to go about writing the help section for command-line programs in a manner that is consistent with the AFNI standard of trickery. The process is somewhat different for the different types of programs as detailed below. But first a quick recap of the trickeries.
   
   The point of all this is to allow a fancier formatting of the help output without much effort. Existing help output can be automatically sphinxized with the command::
      
      apsearch -asphinx_phelp PROGNAME
      
   However, you can also add markup directives in the help string that alters the output depending on the desrired format. For examples on how to add markup into the help strings, run::
   
      apsearch -doc_markup_sample


C-programs:
-----------

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
         make libmri.a

    
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
         echo ' http://afni.nimh.nih.gov/afni/community/board/list.php?f=1 '
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
