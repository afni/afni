
Inter Unix 1. Practice some basic commands, and note the $HOME variable.

   Commands and descriptions:

        cd   : change directories
        ls   : list directory contents
               -a : list all files (include those starting with '.')
               -l : long listing (show file type, ownership, size, date)
        pwd  : show the Present Working Directory
        cat  : display the contents of a file (usually a short text file)
        echo : echo given text to the terminal window


   A. In a terminal window, go to the $HOME directory.  The 'cd' command
      (without any arguments) will do this.

      Commands:
         - go to the $HOME directory
         - display the present working directory
         - echo the value of the $HOME variable
         - list the basic contents of that directory

CODE:
        cd
        pwd
        echo $HOME
        ls
CODEEND:

      The output for this will vary.
      There is no output from a 'cd' command (except for perhaps a change in
      the prompt).  The output from 'pwd' should show the path to the user's
      home directory (as will the output from 'echo $HOME), while the output
      from 'ls' should include AFNI_data6.


   B. Compare the simple listing of files with the listing of all files and
      the long-format listing.  Just for fun, try both options.

CODE:
        ls
        ls -a
        ls -l
CODEEND:

      The 'ls' command, as above, should show a handful of files and
      directories, including AFNI_data6.

      With the -a option, all files (and directories) are shown, including
      those starting with a '.' (which includes '.' and '..', the current
      and parent directories).  There is nothing special about such files,
      except that they are named starting with '.'.  They are typically
      configuration files for various programs (such as .afnirc for AFNI
      programs and .sumarc for SUMA programs).


   C. Move into the AFNI_data6/afni director and see what is there.

CODE:
        cd AFNI_data6/afni
        pwd
        ls
CODEEND:

      There are anatomical, EPI, statistical and mask datasets here, along
      with scripts (text files of commands, such as rall_regress) and stimulus
      timing files (text files of times in seconds, such as stim_AV1_vis.txt).


   D. Look at the contents of some of the text files.  Enter one command at a
      time and note the result.  What follows is meant to provide understanding
      of the contents of each file.

CODE:
        cat stim_AV1_vis.txt
        cat rall_regress
        cat quick.s1.afni_proc
CODEEND:

      file stim_AV1_vis.txt:

         The timing file stim_AV1_vis.txt has 3 rows of real numbers, which are
         onset times for stimulus events across 3 runs.  The first 60 means the
         visual stimulus class (AV1_vis) had its first event 60.0 seconds into
         the first run.  In the third run, that stimulus was given at the very
         beginning of the run, at time 0.

      file rall_regress:

         The file rall_regress contains a single AFNI command, 3dDeconvolve.
         The command is used to model the EPI data via linear regression, where
         the model is specified by the user.

         From the Unix perspective, there are a few key points to make.

            1. This is actually 1 long command.  A '\' character (when it is
               the very last character on a line) tells the shell that the
               current command continues on the following line.  So
        
                  3dDeconvolve -input ... -bucket rall_fun -jobs 2

               is considered one command.

            2. As usual, command options start with '-' and are followed by
               zero or more parameters.  For example, the -num_stimts option
               takes one parameter (the number of stimulus timing files to be
               specified), and in this case 8 is specified.  Note that the
               subsequent 8 lines describe those timing files.

               Alternatively, the -stim_times option takes 3 paramters (the
               stimulus index, the name of the timing file, and the basis
               function).  But our focus is on the Unix aspect, so we just note
               the option and parameter grouping is:

                  -stim_times 1 stim_AV1_vis.txt 'BLOCK(20,1)'

               What that option means is saved for a class on 3dDeconvolve.
               See the output from "3dDeconvolve -help" for details.

            3. Quotes are used for multiple reasons in this one command.
               The quotes around '1D: 0 150 300' have the shell pass that text
               as a single parameter, instead of 4.  So 3dDeconvolve sees
               "-concat" as one parameter, and then "1D: 0 150 300" as the next.
               It does not see -concat then 1D: then 0 then 150 then 300.

               Quotes are used again this way with the -gltsym option.

               Another way to view this is that the quotes hide the spaces from
               from the shell, so that they are not processed as parameter
               separation characters.  Which leads us to the other use of the
               quotes here...


               The stim_times option takes a basis function as its third
               parameter, e.g. 'BLOCK(20,1)'.  But the () characters are
               special to the shell.  So to hide those characters from the
               shell and let 3dDeconvolve see them, they are put within quotes.
               That way 3dDeconvolve reads BLOCK(20,1) as the basis function.

               Similarly, [] are special to the shell (for wildcard matching).
               But we want to pass motion.1D[0] to 3dDeconvolve (using the
               -stim_file option).  And to prevent the shell from trying to use
               [0] for wildcard file name matching, it is put in quotes, as in:

                  -stim_file 3 motion.1D'[0]'

               Note that the quotes could go in multiple places, they are used
               to hide [].  So this would work as well:

                  -stim_file 3 'motion.1D[0]'
               
      file quick.s1.afni_proc:

         Like rall_regress, this file contains a script with just one command.
         The afni_proc.py command is used to generate a single subject FMRI
         processing script, and in this case, execute it.

         This script has line continuation characters '\' as before, as well
         as single quotes hiding special charcters from the shell in the option
         -regress_basis 'BLOCK(20,1)'.

         From a Unix perspective, the additional aspect of this script is the
         pound/sharp character '#'.  When used in a script, this character says
         that the rest of the line is to be ignored.  Effectively, the script
         does not see anything from '#' to the end of the line.

         Such lines are generally used as comment lines, a way to tell/remind
         the reader the purpose of the following line or lines in the script.

         In this example, the comment describes what the afni_proc.py command
         will end up doing (creating and then executing a processing script).


COMMENT:
    1. Get in the habit of using 'ls' after any 'cd', i.e. see what is in any
       directory that you enter.

    2. The 'echo' command is mostly used in processing scripts, to inform the
       user of something.

    3. The 'cat' command is only useful for short files.  For longer files,
       'less' is much more useful ('less' will be covered in another tutorial).
COMMENTEND:

