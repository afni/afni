************
prompt_popup
************

.. _ahelp_prompt_popup:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: prompt_popup -message MESSAGE -button HELLO 
      -message MESSAGE: Pops a window prompting the user with MESSAGE.
                        Program does not return until user responds.
                        note: if MESSAGE is '-', it is read from stdin
      -pause MESSAGE:   Same as -message to match the old prompt_user
      -button LABEL:    What do you want the buttons to say?
                        You can give up to three -button for three buttons.
                        Returns integer 1, 2, or 3.
                        If there is no -button, there will be one button 'Ok'
      -b LABEL:         Same as -button.
      -timeout TT:      Timeout in seconds of prompt message. Default answer
                        is returned if TT seconds elapse without user
                        input.
      -to TT:           Same as -timeout TT
    
    example: prompt_popup -message 'Best disco ever?' -b Earth -b Wind -b Fire
    
