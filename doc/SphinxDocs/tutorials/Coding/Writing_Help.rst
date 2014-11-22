.. _help_functions:

C-programs:
===========

   * Put all help in a function of this prototype::
   
      int Help_Func(TFORM targ, int detail);
      
      
   * Replace printing functions as follows::
   
       printf( --> sphinx_printf(targ,
      fprintf( --> sphinx_fprintf(targ,
      
   * Replace the -help explanation in Help_Func() with the string from::
      SUMA_Offset_SLines(get_help_help(),OFF);
      
   where OFF is the number of blank characters by which to offset the help string
   
   * Add CHECK_HELP(arg,Help_Func) macro in the block where you parse the command line arguments. 
   
      _note::
         
         **All parsing decisions** should be made after macro mainENTRY(). Otherwise your program won't make proper use of things like -h_view, and -h_web, etc.
         
         **Do not parse for -h, -help, or -HELP**. That is now done by CHECK_HELP()
         
   * Add the following after you detect an illegal option in *arg*::
         
      suggest_best_prog_option(argv[0], *arg*);

   
   * Once you add a new program, or upgrade the -help output for an old one, you need to update prog_opts.c and rebuild libmri.a by running this command in src/ directory, to replace existing prog_opts.c::
   
      apsearch -C_prog_opt_array 3dToyProg > prog_opts.c
      touch thd_getpathprogs.c 
      make libmri.a
   
   * See program 3dToyProg.c for an example on how to format your help, and some examples for how to put special markup in the help strings.
    
Shell Scripts:
==============

   * gg
