********
whirlgif
********

.. _whirlgif:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    whirlgif is a quick program that reads a series of GIF files, and produces
    a single gif file composed of those images.
    
    Usage: whirlgif [-v] [-trans index ] [-time delay] [-o outfile]
                    [-loop] [-i incfile] file1 [ -time delay] file2
    
    options:
       -v              verbose mode
       -loop [count]   add the Netscape 'loop' extension.
       -time delay     inter-frame timing.
       -trans index    set the colormap index 'index' to be transparent
       -o outfile      write the results to 'outfile'
       -i incfile      read a list of names from 'incfile'
    
    TIPS
    
    If you don't specify an output file, the GIF will be sent to stdout. This is
    a good thing if you're using this in a CGI script, a very bad thing if you
    run this from a terminal and forget to redirect stdout.
    
    The output file (if any) and -loop _MUST_ be specified before any gif images.
    
    You can specify several delay statements on the command line to change
    the delay between images in the middle of an animation, e.g.
    
          whirlgif -time 5 a.gif b.gif c.gif -time 100 d.gif -time 5 e.gif f.gif
    
    Although it's generally considered to be evil, you can also specify
    several transparency statements on the command line, to change the transparent
    color in the middle of an animation. This may cause problems for some programs.
    
    
    BUGS
      + The loop 'count' is ineffective because Netspcape always loops infinitely.
      + Should be able to specify delay in an 'incfile' list (see next bug).
      + Does not handle filenames starting with a - (hypen), except in 'incfile'.
    
    This program is available from http://www.msg.net/utility/whirlgif/
    -------------------------------------------------------------------
    Kevin Kadow     kadokev@msg.net
    Based on 'txtmerge' written by:
