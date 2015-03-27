
.. _U_misc_error_messages:

**************
Error messages
**************

Errors are often a little confusing, so here are some common ``Unix`` and
``AFNI`` error messages, along with what they might imply.

.. _U_misc_unix_errors:

Unix error messages
===================

.. _U_MU_err_cmd_not_found:

Command not found.
------------------
There are many ways one could accidentally get this error.  It means the shell
does not know of a command or program by the given name.

   0. having a typo in the command name

   1. the ``$PATH`` is not properly configureds, so the shell does not know
      about a given program

   2. ``rehash`` is needed - a *new* program is in a ``$PATH`` directory,
      but the t-shell does not know about it (it is not in the hash table)

   3. incorrect use of ``\`` characters at the end of some line(s)

      The problem is on the line *before* the one mentioned in the error.

      Line continuation ``\`` characters must be the final character on a
      given line, followed by a Unix newline character.  Any misuse of it
      at the end of a line means the the will terminate the current command
      on that line, as well as consider any subsequent line the start of a
      new command.  For example::

        3dDeconvolve -input rall_vr+orig \
           -concat concat_file.txt
           -num_stimts 8                 \
           ...

      a. That leads to early termination of the ``3dDeconvolve`` command,
         which would make it fail (with or without error, depending on the
         options actually getting passed).

      b. It would lead to a the shell viewing ``-num_stimts 8`` as the start
         of a new command, resulting in the error::

           -stim_times: Command not found.

      Note that spaces or tabs after any final ``\`` would also break the
      line coninuation interpretation, but they are difficult to see.

      Also, non-Unix newline characters can be problematic.

      For help with such things, consider ``file_tool``, e.g.::

         file_tool -test -infile my_script.txt

      .. seealso:: `file_tool -help <http://afni.nimh.nih.gov/pub/dist/doc/program_help/file_tool.html>`_

.. _U_MU_err_undefined_var:

Undefined variable.
-------------------
This error means no variable exists by the given name.

   0. maybe there is a typo in the variable name

   1. maybe the script author forgot to set the variable

   2. maybe the variable was set in a child shell and does not exist
      in this context (or even in a different terminal window shell)

.. _U_MU_err_todo:

others - todo
-------------

   - No match.
   - ls: ...: No such file or directory

.. _U_misc_afni_errors:

AFNI error messages
===================
These are error messages that are specific to ``AFNI`` programs.

.. _U_MA_err_no_open:

afni fails to open dataset
--------------------------
Some ``AFNI`` program failed to open or read a dataset.

