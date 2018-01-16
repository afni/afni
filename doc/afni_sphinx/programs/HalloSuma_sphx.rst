*********
HalloSuma
*********

.. _HalloSuma:

.. contents:: 
    :depth: 4 

.. code-block:: none

    A program to illustrate how to communicate with SUMA
      with the help of AFNI's NIML API. Both the NIML API and this
      program are independent of the rest of AFNI/SUMA libraries and 
      can be compiled with C or C++ compilers.
    
    This sample program was written in response to queries by herren 
      Joachim Bottger und Daniel Margulies
    
      Example:
         Run the following two commands, preferably from different shells.
         suma -npb 0 -niml &
         HalloSuma
