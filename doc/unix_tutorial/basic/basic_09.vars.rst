.. _U_basic_4:

******************************
Basic Unix : Shell variables
******************************


   Use of variables is essential in writing processing scripts, allowing such a script to be easily modified for use with different subject IDs, data inputs or processing control parameters (blur size, censor limit, etc).

   They allow one to change a single variable assignment, rather than the 50 places where it is used, say.


   Commands and descriptions::

        set      : setting a (csh/tcsh) shell variable
        unset    : clearing a shell variable
        echo     : echo given text to the terminal window

   LLINK:misc/unix_commands#sc_set:help for set:
   LLINK:misc/unix_commands#sc_unset:help for unset:
   LLINK:misc/unix_commands#sc_echo:help for echo:



   A. Setting and accessing variables using 'set'.

      Define a variable 'subj' to be a subject identification code.  Perhaps
      all of our subjects have codes starting with 's', followed by 3 digits.

      Commands:
         - write a few commands to set a variable and access it

CODE:
      set subj = s314
      echo $subj
      echo subj
      echo now processing subject $subj...
CODEEND:

      The first line has no output, it simply creates a varaible 'subj' with
      value 's314'.  The subsequent echo command shows use of '$' to access
      the value of a variable.  A '$' followed by (adjacent) text means to 
      substitute the value of the variable of that name on the command line.

      The 'echo subj' command has no '$' before subj, so 'subj' is treated as
      simple text, not a variable.  The third echo command shows accessing
      $subj in a common way: informing the user of a script what is happening.

   B. Clear the variable and try to access it again.

CODE:
      echo subject is $subj
      unset subj
      echo subject is $subj
CODEEND:

      The first command just confirms the current value of subj, still showing
      s314.  The 'unset' destroys the variable, and it no longer exists.

      Consequently, the final echo $subj command results in a shell error:

         subj: Undefined variable.

      This particular error message is very clear: the shell knows of no
      variable named 'subj'.  So an "Undefined variable" error means that
      the given variable has not been set (maybe a typo, maybe a logic error).

   C. Set a variable for a particular data directory.

CODE:
      set ddir = $HOME/AFNI_data6/afni
      echo some data is in $ddir
      ls $ddir
CODEEND:

      Here the 'ddir' variable is defined to be the name of a directory where
      some datasets exist.

      The 'echo' command simply confirms the new variable's value.

      The 'ls' command shows the contents of that directory.

   D. Maybe we made a typo in the directory name.

CODE:
      set newdir = $HOME/AFNI_data6/pickle
      echo some data is in $ddir
      ls $ddir
CODEEND:

      The first 2 commands are similar to those above, simply setting a new
      variable 'newdir', presumably where we have data.

      But the third command fails because we "accidentally" typed 'pickle'
      instead of afni.  Instead of a directory listing, we see:

         ls: ... : No such file or directory

   E. Play with the $ddir variable.

CODE:
      echo data dir is $ddir
      ls $ddir
      pwd
      ls
      cd $ddir
      ls
CODEEND:

      Since $ddir points to one of the class data directories, the 'ls' command
      should show the contents of that directory, regardless of where we are
      sitting (probably the $HOME directory).

      The 'pwd' command should confirm that we are in the $HOME directory (it
      does not really matter), as should the 'ls' command, in showing the
      contents of the current directory.

      Then 'cd $ddir' takes us to that data directory, and the subsequent 'ls'
      command shows the same output as the initial 'ls' command: the contents
      of the AFNI_data6/afni directory.

      Remember, if you see "No such file or directory", then perhaps there is
      a typo in the $ddir value.  If so, use the set command to set it to the
      correct directory name.


   F. Valid variable names and {}.

      Variable names must start with an alphabetic letter (a-z,A-Z), and can
      then contain alphabetic letters, digits (0-9) and underscores (_).

      valid:    subj    Var12345  subject_ID
      invalid:  123subj vv.T+id

CODE:
      set subj = s314
      echo data for $subj is under subjects/$subj/data
      echo data is under subjects/$subj_data
      echo data is under subjects/${subj}_data
CODEEND:

      After setting 'subj', the first echo statement should work fine.

      However the second echo statement should produce an error.  The shell
      sees '$subj_data' as a variable, and tries to return its value, but
      only $subj is defined, not $subj_data.  This results in the error:

         subj_data: Undefined variable.

      So 'subj' will have to be separated from '_data', allowing it to be
      evaluated on its own.

      That leads to the third echo command, where 'subj' is put within '{}'.
      The {} characters merely separate subj from _data.


   G. Use of variables as part of dataset names.

      A subject ID variable, for example, will often be used as parts of both
      input and output file names, as well as directory names, which helps with
      data organization.

      The only special thing here is a deviation from Unix-land to AFNI-land.
      Datasets do not need the .HEAD or .BRIK extension to be seen by AFNI
      programs, but to be seen by the shell or other Unix programs, they do.

CODE:
      cd ~/AFNI_data6
      set subj = FT
      ls -l ${subj}_analysis/$subj
      set anat_dset = ${subj}_analysis/$subj/${subj}_anat+orig

      echo anat dataset: $anat_dset
      ls -l $anat_dset
      3dinfo $anat_dset

      echo anat dataset: $anat_dset.HEAD
      ls -l $anat_dset.HEAD
      3dinfo $anat_dset.HEAD
CODEEND:

      We start from AFNI_data6 for convenience, and set a subject ID variable
      to FT.  Then we get a listing of the contents of that subject's data
      directory.  Including in that listing is the anatomical dataset
      FT_anat+orig (both the .BRIK and .HEAD files are used to make up the
      dataset).

      So anat_dset is set to the name of that AFNI dataset, including the
      dataset prefix and view, but not the file extension.


      The 'echo anat dataset' command should show 'FT_analysis/FT/FT_anat+orig',
      but the subsequent 'ls -l' command should fail, because there is no actual
      file of that name (the files end in either .HEAD or .BRIK, not +orig).
      The 'ls' command should produce the error:

         ls: ... No such file or directory

      However '3dinfo $anat_dset' works fine, because AFNI programs do not need
      the complete filename, just the prefix and view (+orig, in this case).
      The output is just some text describing the dataset (location in space,
      resolution, command history, etc).


      Next, an echo command merely appends .HEAD, which we expect is the actual
      name of a file (though echo does not care).  The subsequent 'ls' command
      should now succeed, since $anat_dset.HEAD should exist.  It should just
      output the long listing of that particular .HEAD file.

      Finally, 3dinfo is repeated using the complete filename.  The command
      works just as before (without the .HEAD).  


COMMENT:
   c0. Shell variable must start with a letter, and consist of only letters,
       digits and underscores.

   c1. Curly brackets '{}' can be used to separate a variable name from 
       adjacent text, which would otherwise look like a longer variable name.

   c2. AFNI programs can access datasets without the extension in the dataset
       name.  Unix programs need a complete, existing file name.
COMMENTEND:

