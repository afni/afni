.. _ahelp_siemens_vision:

**************
siemens_vision
**************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    Usage: siemens_vision [options] filename ...
    Prints out information from the Siemens .ima file header(s).
    
    The only option is to rename the file according to the
    TextImageNumber field stored in the header.  The option is:
    
      -rename ppp
    
    which will rename each file to the form 'ppp.nnnn.ima',
    where 'nnnn' is the image number expressed with 4 digits.
    
    When '-rename' is used, the header info from the input files
