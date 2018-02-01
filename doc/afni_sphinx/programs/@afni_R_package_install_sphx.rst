***********************
@afni_R_package_install
***********************

.. _@afni_R_package_install:

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
       ----------------------------------------------------------------------------
       @afni_R_package_install
               Helper script to install R packages for various afni-ish purposes.
               You must have R installed, and permissions for its package library.
    
       -----------------------------------------------------------------------------
       options:
    
          -afni   : Current list of packages for afni.
                    Similar to rPkgsInstall.
                    Installs:
                    afex phia snow nlme lme4 paran psych
    
          -shiny  : Current list of packages for afni based shiny apps.
                    Installs:
                    shiny shinydashboard plotly colourpicker data.table
                    gplots RColorBrewer
    
          -circos : Packages for FATCAT_matplot.
                    Installs OmicCircos via biocLite.
                    Actually runs OmicCircos_pkg_install.R.
    
          -custom : Install whatever R packages you desire.
                    Requires a space separated list of packages.
                    Must start and end with double quotes.
                    e.g. "earth wind fire"
    
          -help   : Show this help.
    
       -----------------------------------------------------------------------------
       examples:
    
          @afni_R_package_install -afni
    
          @afni_R_package_install -afni -shiny -custom "earth wind fire"
    
       -----------------------------------------------------------------------------
       Justin Rajendra 11/2017
