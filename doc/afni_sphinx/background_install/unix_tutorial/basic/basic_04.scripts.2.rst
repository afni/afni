.. _U_basic_4:

*******************************
Basic Unix : special characters
*******************************

This section covers two of the common special characters one might encounter
in a shell script, the single quote and backslash characters.

   Commands and descriptions:

   - :ref:`single quotes <U_mcc_squote>`
   - :ref:`double quotes <U_mcc_dquote>`
   - :ref:`backslash characters <U_mcc_backslash>`
   - :ref:`pound characters <U_mcc_pound>`
   - :ref:`U_mc_cat` : display a text file in the terminal window
   - :ref:`U_mc_gedit` : a graphical editor
   - `3dDeconvolve -help <https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dDeconvolve.html>`_


Starting from the ``AFNI_data6/afni`` directory, look at the contents of one
of the other text files.  The file ``rall_regress`` is a text file with a
single command in it.

The program that would get run from the script (which we will not do here)
is ``3dDeconvolve``, an ``AFNI`` program to use linear regression to fit a
model against time series data.  Understanding the command from an ``AFNI``
perspective is outside the scope of this ``Unix`` tutorial.

The goal here is to understand this script from a Unix perspective.


Look at the script
------------------
Display the contents of ``rall_regress``, either in the terminal window or
in an editor.  The script is under ``AFNI_data6/afni``, in case you are not
already there.

   commands (type these in the terminal window)::

        cd ~/AFNI_data6/afni
        cat rall_regress

   Alternatively, it would be fine to open the text file in an editor::

        gedit rall_regress &

   The output (ignoring gedit) should look like this:

   .. image:: media/basic_4_A.jpg
      :align: center

Command overview
----------------

The file ``rall_regress`` contains a single ``AFNI`` command, ``3dDeconvolve``.

The command is used to model aligned EPI data via linear regression, where
the model is specified by the user (via 3dDeconvolve options and stimulus
timing files).

For the purposes of this tutorial, we do not particularly care to evaluate
this command from an ``AFNI`` perspective.  However, the command parameters
and options must be in the format expected by ``3dDeconvolve``, so some
mention will be made.

For example, it is important to know that the ``-concat`` option takes only 1
parameter.  Since we want to pass ``1D: 0 150 300`` as that single parameter
(and not as 4 parameters), special shell characters will be applied (those
terms are surrounded by quotes, just to group them into a single parameter).


Backslash characters (``\``)
----------------------------

The file `rall_regress` is a simple script with one ``3dDeconvolve`` command
in it.

   This is actually one long command.  A ``\`` character (when it is the very
   last character on a line, **including spaces and tabs**) tells the shell
   that the current command continues on the following line.  Repeating that
   notion shows that this is one long command (abbreviated by)::
     
      3dDeconvolve -input rall_vr+orig ... -bucket rall_func -jobs 2

   The ``\`` characters are not required, they are only there to make the file
   more readable.  The ``\`` characters, along with use of indentation to align
   similar options over successive lines, greatly affect the time it takes to
   visually parse and understand the command.  Without ``\`` and indentation,
   the command might look like:
 
   .. image:: media/basic_4_B.jpg
      :align: center

   The commands are identical, but this unindented version is far more
   difficult to read.

   .. seealso:: Unix error message: :ref:`U_MU_err_cmd_not_found`

      - section: incorrect use of ``\`` characters


Option parameter grouping
-------------------------

Command options (generally) start with '-' and are followed by zero or more
parameters.

   For example, the -num_stimts option takes 1 parameter (the number of
   stimulus timing files to be specified), and in this case 8 is specified.
   Note the subsequent 8 script lines describing timing and stimulus files.

   Alternatively, the -stim_times option takes 3 parameters (the stimulus
   index, the name of the timing file, and the basis function).  But our focus
   is on the Unix aspect, so we just note that the option and parameter
   grouping is::

         -stim_times 1 stim_AV1_vis.txt 'BLOCK(20,1)'

      What that option implies is saved for a class on 3dDeconvolve.

.. seealso:: `3dDeconvolve -help <https://afni.nimh.nih.gov/pub/dist/doc/program_help/3dDeconvolve.html>`_


.. _U_basic_4_quotes:

Use of quotes
-------------

Quotes are used for multiple reasons in this one command.

   The quotes around '1D: 0 150 300' have the shell pass that text as a
   single parameter, instead of as 4.  So 3dDeconvolve sees "-concat" as
   one parameter, and then "1D: 0 150 300" as the next.  It does NOT see
   -concat then 1D: then 0 then 150 then 300.

   Quotes are used again this way with the -gltsym option.

   Another way to view this is that the quotes hide the spaces from from the
   shell, so that they are not processed as parameter separation characters.
   Which leads us to the other use of the quotes here...


   The stim_times option takes a basis function as its third parameter,
   e.g. 'BLOCK(20,1)'.  But the ``()`` characters are special to the shell.
   So to hide those characters from the shell and let 3dDeconvolve see them,
   they are put within quotes.  That way 3dDeconvolve reads BLOCK(20,1) as
   the basis function.

   Similarly, [] are special to the shell (for wildcard matching).  But we
   want to pass motion.1D[0] to 3dDeconvolve using the -stim_file option 
   (so that 3dDeconvolve knows the 'roll' regressor comes from column #0
   of motion.1D).  And to prevent the shell from trying to use [0] for
   wildcard file name matching, it is put in quotes, as in::

      -stim_file 3 motion.1D'[0]'

   The quotes could go in multiple places, as they are just used to hide [].
   Plus, either single or double quotes would hide those characters from the
   shell, so the following example would work as well::

      -stim_file 3 "motion.1D[0]"
      

Use of comments
---------------

Another short script can be seen in the file :file:`epi_r1_regress`.

   This script contains 2 AFNI commands, along with comments describing those
   commands.  View the script using::

      cat epi_r1_regress

   .. image:: media/basic_4_C.jpg
      :align: center

   A ``#`` character (that is not hidden from the shell using quotes, say)
   is ignored by the shell, along with the rest of that line, as if it were
   not in the script.  The purpose of such a "comment line" is to describe
   the script's intention to someone reading it.  So even with out knowing
   what ``3dvolreg`` is, we can read that it will register the time series
   dataset, saving the motion parameters (to a file, presumably).

   The next comment suggests that the ``3dDeconvolve`` command is used to
   perform a simple regression on the newly registered data.  It is very
   similar to the rall_regress script, reviewed above.

.. note::

    The 'cat' command is only useful for short files.  For longer files,
    'less' is much more useful ('less' will be covered elsewhere).


