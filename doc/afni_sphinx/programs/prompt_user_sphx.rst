***********
prompt_user
***********

.. _prompt_user:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
    Mostly replaced by prompt_popup for more customization.
    Usage: prompt_user <-pause MESSAGE> 
      -pause MESSAGE: Pops a window prompting the user with MESSAGE.
                      Program does not return until user responds.
                      note: if MESSAGE is '-', it is read from stdin
      -timeout TT: Timeout in seconds of prompt message. Default answer
                   is returned if TT seconds elapse without user
                   input.
      -to TT: Same as -timeout TT
    
