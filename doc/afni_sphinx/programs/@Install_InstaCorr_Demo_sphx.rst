***********************
@Install_InstaCorr_Demo
***********************

.. _@Install_InstaCorr_Demo:

.. contents:: 
    :depth: 4 

.. code-block:: none

    Installs and sets up AFNI's InstaCorr demo archive
    After setup, all you need to do is run the demo scripts
    this way:
    *****************
    
    For Volume-based 3dGroupInCorr run:
    cd /Users/discoraj/Documents/nih/sphinx_test/help2sphinx//vol
    tcsh ./@RunVolGroupInCorr 
    
    For Surface-based 3dGroupInCorr run:
    cd /Users/discoraj/Documents/nih/sphinx_test/help2sphinx//srf
    tcsh ./@RunSurfGroupInCorr 
    
    For Surface-based Single-Subject InstaCorr run:
    cd /Users/discoraj/Documents/nih/sphinx_test/help2sphinx//srf
    tcsh ./@RunSingleSurfInstaCorr 
    
    *****************
    Options:
    [-wget]: Use wget to download archive. Script chooses by default
             with preference for curl
    [-curl]: Use curl to download archive. Script chooses by default
             with preference for curl
    [-full]: Install the full version of the demo. This downloads
             all subject surfaces, resting state volume time series
             etc. The script then processes the data and produces
             the files needed for running the various interactive
             InstaCorr demos.
    [-mini]: Install the mini version of the demo. This downloads
             only the files needed for running the various interactive
             InstaCorr demos.
    
    It takes a while to download, unpack, and run the setup scripts
