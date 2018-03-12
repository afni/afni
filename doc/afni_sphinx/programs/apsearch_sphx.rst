********
apsearch
********

.. _apsearch:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    A program to perform simple approximate string searching. It's primary
    purpose is to test string matching for atlas area locations.
    
      apsearch <-word WORD> <[-file FILE] | [-text TEXT] | [-phelp PROG]> 
               [OPTIONS]
    
    Parameters:
    ===========
      -word WORD: WORD being sought
      -w WORD: Abbreviated version of -word WORD
      -file FILE: Search for WORD in text file FILE
      -files FILE1 FILE2 ...: Search for WORD in text files FILE1 FILE2 ...
      -text TEXT: Search for WORD in string TEXT
      -stdin: Search for WORD in text from stdin
      -: Same as -stdin
      -phelp PROG: Search for WORD in output of command PROG -help
      -popt PROG: Search for possible options of PROG that match WORD
                  Make sure you add the '-' to WORD if you are looking
                  for an actual option.
      -raw_phelp PROG: Spit out the help string for PROG without modification.
      -txt_phelp PROG: Format the output of PROG -help for simple text.
      -sphinx_phelp PROG: Format the output of PROG -help in a sphinxized way.
      -asphinx_phelp PROG: Format the output of PROG -help in an auto sphinxized
                           way.
      -doc_2_txt: Format AFNI markups from  -file/-files/-stdin content for text
                  output.
      -doc_2_spx: Format AFNI markups from  -file/-files/-stdin content for
                  Sphinx output.
      -hdoc_2_txt PNAME: Format program help output in  -file/-files/-stdin 
                  content for text output. PNAME is needed wherever the program
                  name is needed in the output.
      -hdoc_2_spx PNAME: Format program help output in  -file/-files/-stdin 
                  content for Sphinx output. PNAME is needed wherever the program
                  name is needed in the output.
      -hdoc_2_aspx PNAME: Format program help output in  -file/-files/-stdin  
                  content for Sphinx output with autoformatting of options.
                  PNAME is needed wherever the program name is needed in the 
                  output.
                  Now, why use such an option as opposed to -asphinx_phelp ?
                  That's because the -help option in some programs cannot handle
                  any special markup within it so we write out that string as is
                  to standard out and pipe it to apsearch with:
                  3dinfo -h_raw | apsearch -hdoc_2_aspx 3dinfo -
    
      -race_check PNAME RMAX: Debugging option to test for race conditions where
                  apsearch calls a program which for some reason ends up calling
                  it back until you chew up all allowed processes -- not fun --!
                  This program will now check for such recursive craziness using
                  Rick Reynold's afni_util.py program. To see it in action, 
                  create the following script and call it @rory:
                     #!/bin/tcsh -f
                     echo "Called! `date`"
                     apsearch -DSUMA_CountProcs_Verb=YES -race_check `basename $0`
    
                  @rory should be executable and in your path.
                  Now run @rory and watch it go.
    
      -doc_markup_sample: Shown an example of the types of markups available for
                          the documentation.
      -all_afni_help: Search for WORD in all afni help files.
                      This option is not all that clever at the moment.
      -all_popts PROG: TRY to guess at all the options for PROG
                      The list of options is not guaranteed to be full
                      or accurate. It is created by parsing the program's
                      -help output for likely candidates. 
                      It is meant to act as an aid in locating
                      certain options.
      -list_popts PROG: Like -all_popts, but preserve unique set of options
                        only, no chunks of help output are preserved.
      -popts_complete_command PROG: Generate a csh command that can be sourced
                                    to allow option autocompletion for program
                                    PROG.
                              See also option -bash and -update_all_afni_help
      -bash: Use bash format for the complete command. Default is csh/tcsh
             This option MUST PRECEDE option -popts_complete_command
      -ci: Case insensitive search (default)
      -cs: Case sensitive search
      -global_help: Show help for global options.
      -gopts_help:  Show help for global options.
      -max_hits MH: Return best MH hits only. Default MH = 3.
                    Use -1 to get all results back.
      -m MH: Abbreviated version of -max_hits MH.
      -min_different_hits mDH: Keep outputing hits until you have dDH
                               dissimilar matches. 
                               Default is -1 (no minimum).
      -unique_hits_only: Restrict output to novel hits only.
      -show_score: Show matching word's distance.
      -show_score_detail: That's right.
      -list_all_afni_progs: List all executables in AFNI's bin directory
      -list_all_afni_P_progs: Same as -list_all_afni_progs but with path
      -list_all_afni_readmes: List all README files in AFNI's bin directory
      -list_all_afni_P_readmes: Same as -list_all_afni_readmes but with path
      -list_all_afni_dsets: List all datasets in AFNI's bin directory
      -list_all_afni_P_dsets: Same as -list_all_afni_dsets but with path
      -update_all_afni_help: Build/update -help output under directory:
                         /home/ptaylor/.afni/help
                      If older help files differ by little they are deleted
                      Little differences would be the compile date or the
                      version number. See @clean_help_dir code for details.
                      This option also creates autocompletion code for 
                      csh/tcsh and bash shells.
      -recreate_all_afni_help: Like -update_all_afni_help but force receration
                               even if nothing changed in the help
      -afni_help_dir: Print afni help directory location and quit.
      -afni_data_dir: Print afni data directory location and quit.
      -afni_bin_dir: Print afni's binaries directory location and quit.
      -afni_home_dir: Print afni's home directory and quit.
      -afni_rc_file: Pathname to .afnirc. You'll get one even if none exists.
      -afni_custom_atlas_dir: Print your afni's custom atlas directory 
                              and quit.
      -afni_custom_atlas_file: Print your afni's custom atlas file (if any)
                              and quit.
      -afni_text_editor: Print the name of the GUI editor. Priority goes to 
                         env. variable AFNI_GUI_EDITOR, otherwise afni
                         will try to find something suitable.
      -afni_web_browser: Print the name of the browser used by AFNI. 
                         Priority goes to env. variable AFNI_WEB_BROWSER, 
                         otherwise afni will try to find something suitable.
      -afni_web_downloader: Print the name of the downloader used by AFNI. 
                         Priority goes to env. variable AFNI_WEB_DOWNLOADER, 
                         otherwise afni will try to find something suitable.
      -view_text_file FILE: Open FILE with editor of -afni_text_editor
      -view_readme SOMETHING: Find a readme.SOMETHINGISH and open it
      -apsearch_log_file: Print the name of the logfile that is used to save
                          some results of apsearch's functions. This option
                          is for debugging purposes and is only activated if
                          the environment variable AFNI_LOG_BEST_PROG_OPTION
                          is set to YES.
      -view_prog_help PROG: Open the help file for PROG in a GUI editor.
                            This is like the option -hview in C programs.
      -web_prog_help PROG: Open the help file for PROG in a web brower.
                           This is like the option -hweb in C programs.
                  Use ALL to view the page containing help for all programs.
      -web_class_docs: Open the webpage with latest class pdfs.
    
      NOTE: The maximum number of results depends on the combination of
            -max_hits, -min_different_hits, and -unique_hits_only. 
            Withoug -unique_hits_only, the output will continue 
            while neither -max_hits or -min_different_hits conditions 
            are met.
    
      -func_test: Run sample function testing and quit. Debugging only.
    
    Wildcard expansion tools:
    =========================
    -wild_files 'PAT1 PAT2 ...' : Find files matching PAT1, or PAT2, etc.
                                  Should include PAT1, etc. between quotes or 
                                  the shell will do the expansion for you.
                                  Note that in addition to wildcard expansion, 
                                  the function also sorts the output so the order
                                  is alphabetical. It also dumps duplicate names
                                  which can happen when you start to remove 
                                  extensions known to AFNI. See -wild* options
                                  below.
            Example: -wild_files '*.do *.HEAD'
    -wild_files_noAext: After compiling list, remove all known AFNI extensions 
                        and preserve unique set of resultant names
    -wild_files_noAext_noAview: After compiling list, remove all known AFNI
                        extensions and any view such as +tlrc, +orig, +acpc, 
                        and preserve unique set of resultant names
    -wild_files_orig_name: Output unique list using orignal (full) filename, 
                           rather than the names after extensions or views were
                           removed. This option makes a difference when using
                           one of -wild_files_noAext* options.
    -wild_all_files: Show all files from wildcard expansion. Do not sort, do not
                     trim names, etc.
    -wild_files_debug: Output results in debugging mode.
    -wild_files_ci: When searching for unique set, use case insensitive matching
    -test_unique_str: Run debugging tests for function unique_str().
    
    For hard coders only:
    =====================
    -C_all_prog_opt_array : Output all program options as an array of C structs.
                            Debugging is output to stderr, the beef is in stdout.
                            Executables not found in the afni binaries directory 
                            (now /home/ptaylor/afni_src/linux_ubuntu_12_64/) will be ignored.
    -C_all_append_prog_opt_array: Keep programs already in C struct but no longer
                            in the new list of executables.
    -C_prog_opt_array PROG: Insert/update PROG's options in an array of C 
                            and output the results to stdout as for
                            option -C_all_prog_opt_array
    
                Example:    apsearch -C_prog_opt_array 3dToyProg > prog_opts.c
    
    Examples:
    =========
     1- Search help output of program whereami for the word '-atlas'
            apsearch -ci -phelp whereami -word -atlas
     2- Search all atlas area names for some name (mistakes on purpose)
            whereami -show_atlas_code > all_atlas_area_names.txt
            apsearch -file all_atlas_area_names.txt -word hepp
            apsearch -file all_atlas_area_names.txt -word zipp \
                      -min_different_hits 5 -unique_hits_only 
            apsearch -file all_atlas_area_names.txt -word hipp \
                      -min_different_hits 5 -unique_hits_only 
     3- Debug stupid string matcher:
            apsearch -text 'u:Hippocampus' -word hipp -show_score_detail
            apsearch -text 'u:IPC' -word hipp -show_score_detail
     4- Search help of AFNI programs:
            apsearch -phelp afni -word port
            apsearch -phelp 3dSkullStrip -word hull
            apsearch -phelp afni  -word xt
     5- Suggest a valid option from a program:
            apsearch -popt afni -word xt
            apsearch -popt @ROI_Corr_Mat -word sel
            apsearch -popt @ROI_Corr_Mat -word -sel
     6- Show all(*) options for a program:
            apsearch -all_popts 3dSkullStrip
        (*) see -all_popts in help section
     7- Look for some area named something or other in some atlas:
            whereami -show_atlas_code -atlas DKD_Desai_MPM |\
                                    apsearch -stdin -word insola
        If you really screw up the spelling, you should help the search
        program a little as in:
            whereami -show_atlas_code -atlas DKD_Desai_MPM |\
                                    sed 's/[-_]/ /g' |\
                                    apsearch -stdin -word insolent
     8- Find 10 afni programs with something like 'Surface' in their names:
            apsearch -list_all_afni_progs | \
                 apsearch -stdin -word surface -max_hits 10
     9- Open the readme for driving AFNI:
            apsearch -view_readme driv
     10- Wildcard expansion and sorting:
            apsearch -wild_files '*.1D*' '*.HEAD *.BRIK*' \
                     -wild_all_files 
            apsearch -wild_files '*.1D*' '*.HEAD *.BRIK*' \
                     -wild_files_noAext_noAview 
            apsearch -wild_files '*.1D*' '*.HEAD *.BRIK*' \
                     -wild_files_noAext_noAview -wild_files_orig_name 
    
    Global Options:
    ===============
    
    
    ++ Compile date = Jan 29 2018 {AFNI_18.0.11:linux_ubuntu_12_64}
