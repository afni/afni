.. _ahelp_@SUMA_Make_Spec_SF:

******************
@SUMA_Make_Spec_SF
******************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    @SUMA_Make_Spec_SF - prepare for surface viewing in SUMA
    
    Use @SUMA_Make_Spec_Caret for caret surfaces
    
        This script goes through the following steps:
          - determine the location of surfaces and 
            then AFNI volume data sets used to create them.
          - creation of left and right hemisphere SUMA spec files
    
          - all created files are stored in SURFACES directory
    
      Usage: @SUMA_Make_Spec_SF [options] -sid SUBJECT_ID
    
      examples:
    
        @SUMA_Make_Spec_SF -sid subject1
        @SUMA_Make_Spec_SF -help
        @SUMA_Make_Spec_SF -sfpath subject1/surface_stuff -sid subject1
    
      options:
    
        -help    : show this help information
    
        -debug LEVEL    : print debug information along the way
              e.g. -debug 1
              the default level is 0, max is 2
    
        -sfpath PATH    : path to directory containing 'SURFACES'
                          and AFNI volume used in creating the surfaces.
              e.g. -sfpath subject1/surface_models
              the default PATH value is './', the current directory
    
              This is generally the location of the 'SURFACES' directory,
              though having PATH end in SURFACES is OK.  
    
              Note: when this option is provided, all file/path
              messages will be with respect to this directory.
    
    
        -sid SUBJECT_ID : required subject ID for file naming
    
    
      notes:
    
        0. More help may be found at https://afni.nimh.nih.gov/ssc/ziad/SUMA/SUMA_doc.htm
        1. Surface file names should look like the standard names used by SureFit:
           rw_1mmLPI.L.full.segment_vent_corr.fiducial.58064.coord
           Otherwise the script cannot detect them. You will need to decide which
           surface is the most recent (the best) and the script helps you by listing
           the available surfaces with the most recent one first.
           This sorting ususally works except when the time stamps on the surface files
           are messed up. In such a case you just need to know which one to use.
           Once the fiducial surface is chosen, it's complimentary surfaces are selected
           using the node number in the file name.
        3. You can tailor the script to your needs. Just make sure you rename it or risk
           having your modifications overwritten with the next SUMA version you install.
    
         R. Reynolds (rickr@codon.nih.gov), Z. Saad (saadz@mail.nih.gov)
