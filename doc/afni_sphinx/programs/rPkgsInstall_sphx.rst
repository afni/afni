.. _ahelp_rPkgsInstall:

************
rPkgsInstall
************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
              ================== Welcome to rPkgsInstall ==================          
                         Install/update/remove R packages for AFNI
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    Version 0.0.1, Nov 3, 2014
    Author: Gang Chen (gangchen@mail.nih.gov)
    Website - https://afni.nimh.nih.gov/sscc/gangc
    SSCC/NIMH, National Institutes of Health, Bethesda MD 20892
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    Usage:
    ------ 
     rPkgsInstall is a program for installing, checking, updating, or removing any
     R packages. It conveniently runs on the shell terminal instead of the R prompt.
     Check out the examples below or the option specifications for usage details.
    
    --------------------------------
    Example 1 --- Install all the R packages that are currently required in 
    AFNI programs:
       rPkgsInstall -pkgs ALL
       rPkgsInstall -pkgs ALL -site 'http://cran.us.r-project.org'
    
    
    --------------------------------
    Example 2 --- Install user-specified R packages:
       rPkgsInstall -pkgs 'gsl'
       rPkgsInstall -pkgs 'afex,phia,paran'
       rPkgsInstall -pkgs 'snow,nlme,psych' -site 'http://cran.us.r-project.org'
    
    
    --------------------------------
    Example 3 --- check/update/remove R packages:
       rPkgsInstall -pkgs ALL -check
       rPkgsInstall -pkgs ALL -update
       rPkgsInstall -pkgs ALL -remove
       rPkgsInstall -pkgs ALL -update -site 'http://cran.fhcrc.org/'
       rPkgsInstall -pkgs 'lme4,pixmap,plotrix' -check
       rPkgsInstall -pkgs 'afex,phia,paran' -update
       rPkgsInstall -pkgs 'boot' -remove
       rPkgsInstall -pkgs 'snow,nlme,vars' -update -site 'http://cran.cs.wwu.edu/'
    
    
    Options in alphabetical order:
    ==============================
    
       -check: This option verifies whether all or the user-specified R packages
             listed in option -pkgs are installed on the computer, but it does not
             install/update/remove the packages.
    
       -help: this help message
    
       -pkgs package_list: List all the packages that you would like to install,
             update or move. This option is required for installation, update,
             or removal. The absence of both options -update and -remove means
             installation. The package names should be separated with comma (,)
             without any other characters such as spaces, and should be surrounded
             within single/double quotes. For example, -pkgs "afex,phia". If
             package_list is set as ALL, all the following packages required for
             AFNI programs will be installed, updated, or removed:
    
             'afex', 'phia', 'snow', 'nlme', 'lme4', 'paran', 'psych'.
    
             You can use rPkgsInstall to install, update, or remove any R packages,
             and they do not have to be in the list above. 
    
       -remove: This option indicates that all or the user-specified R packages in AFNI
             will be purged from your computer. The absence of the option (default)
             means installing or updating, but no removing. 
    
       -show_allowed_options: list of allowed options
    
       -site download_website: You can specify the package repository website within
            single/double quotes. The current sites can be found at
    
            http://cran.r-project.org/mirrors.html
    
            The default is 'http://cran.us.r-project.org'
            University, Houghton, MI.
    
       -update: This option indicates that all or the user-specified R packages in AFNI
             will be updated. The absence of the option (default) means no updating.
             A package specified in '-pkgs package_list' that has not been installed on
             the computer will be installed under this option.
             WARNING: Updating some R packages may require that R be ugraded to the
                      most recent version. 
