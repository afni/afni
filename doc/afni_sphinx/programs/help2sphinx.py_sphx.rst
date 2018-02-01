**************
help2sphinx.py
**************

.. _help2sphinx.py:

.. contents:: 
    :depth: 4 

| 

    usage: /home/ptaylor/afni_src/linux_ubuntu_12_64/help2sphinx.py
           [-h] -OutFolder OUTFOLDER [-prog PROG] [-help]
    

Overview
========

.. code-block:: none

    
    Parse the help output of all AFNI programs (or just one) to create a sphinxy version.
    
    This program will look for codes in the help output and use those to make
    sphinx headers and tables of contents.
    
    The codes are 3 characters at the end of a line of the help output:
    ~1~ = Main section
    ~2~ = Sub section
    ~3~ = Sub sub section
    ~4~ = Horizontal line separator above the line with the code
    

Caveats
=======

.. code-block:: none

    
    If there are no codes, this program will create a code-block of the help output.
    It will also create a table of contents with all programs listed in a 3 column table.
    
    If you use the -prog option, you will break the main_toc as it will be overwritten
    with just one entry. Use this option for testing only!
    
    required:
      -OutFolder OUTFOLDER  Where do you want the .rst files to go?
    
    optional:
      -prog PROG            Single AFNI program to sphinxify. (For testing only.
                            Will break main_toc.)
    
    Justin Rajendra 01/2018
    Keep on keeping on!
