.. _ahelp_@global_parse:

*************
@global_parse
*************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    A script to parse for global help options
    The first parameter is the ALWAYS the program name whose help
    output you seek. All other options follow.
    It is meant to be called by other scripts.
    It returns 0 when it has nothing to do.
               1 when it does something and wants calling
    program to quit
    
    To use this in any script follow these steps
    1- Add this line before any parsing, right after the 1st line
          @global_parse `basename $0` "$*" ; if ($status) exit 0
    2- Add this line right where you fail to recognize an option
          apsearch -popt `basename $0` -word $argv[$cnt]
    3- Add this line somewhere in the help section
          @global_parse -gopts_help
    4- Eliminate going to help immediately when too few options
       are set. One option, such as -all_opts is always good
