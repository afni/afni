***************
3dMVM_validator
***************

.. _3dMVM_validator:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
       ----------------------------------------------------------------------------
       3dMVM_validator
          Launch the 3dMVM model validation shiny app in a web browser.
          Input is a file containing a table formatted like the 3dMVM "-dataTable".
          See 3dMVM -help for the correct format.
          This will create a temporary folder in the current directory with a
          random name similar to:
          __8726_3dMVM_validator_temp_delete
          It will be deleted when you close the shiny app. If it is still there
          after you close the app, it is safe to delete.
          If you seem to be missing some R packages, you may need to run:
          @afni_R_package_install -shiny
    
       -----------------------------------------------------------------------------
       options:
          -dataTable   : A file containing a data table formatted like the
                         3dMVM "-dataTable".
          -ShinyFolder : Use a custom shiny folder (for testing purposes).
          -help        : show this help
    
       -----------------------------------------------------------------------------
       examples:
          3dMVM_validator -dataTable ~/my_dataTable.csv
    
       -----------------------------------------------------------------------------
       Justin Rajendra 11/2017
