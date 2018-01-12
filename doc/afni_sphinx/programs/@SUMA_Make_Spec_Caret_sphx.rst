.. contents:: 
    :depth: 4 

*********************
@SUMA_Make_Spec_Caret
*********************

.. code-block:: none

    
    @SUMA_Make_Spec_Caret - prepare for surface viewing in SUMA
    
        This script was tested with Caret-5.2 surfaces.
    
        This script goes through the following steps:
          - determine the location of surfaces and 
            then AFNI volume data sets used to create them.
          - creation of left and right hemisphere SUMA spec files
    
          - all created files are stored in the directory where 
            surfaces are encountered
    
      Usage: @SUMA_Make_Spec_Caret [options] -sid SUBJECT_ID
    
      examples:
    
        @SUMA_Make_Spec_Caret -sid subject1
        @SUMA_Make_Spec_Caret -help
        @SUMA_Make_Spec_Caret -sfpath subject1/surface_stuff -sid subject1
    
      options:
    
        -help    : show this help information
    
        -debug LEVEL    : print debug information along the way
              e.g. -debug 1
              the default level is 0, max is 2
    
        -echo: Turn shell echo on
    
        -sfpath PATH    : path to directory containing 'SURFACES'
                          and AFNI volume used in creating the surfaces.
              e.g. -sfpath subject1/surface_models
              the default PATH value is './', the current directory
    
              This is generally the location of the 'SURFACES' directory,
              though having PATH end in SURFACES is OK.  
    
              Note: when this option is provided, all file/path
              messages will be with respect to this directory.
    
    
        -sid SUBJECT_ID : required subject ID for file naming
    
    
        -side_labels_style STYLE: Naming style for Left, Right sides.
                                  Allowed STYLE values are : 
                                  1 for L R LR style (default)
                                  2 for LEFT RIGHT LR style
                                  3 for A B AB (don't ask)
    
    
      notes:
    
        0. More help may be found at https://afni.nimh.nih.gov/ssc/ziad/SUMA/SUMA_doc.htm
        1. Surface file names should look like the standard names used by Caret:
           Human.3dAnatomy.LR.Fiducial.2006-05-09.54773.coord
           Human.3dAnatomy.LR.CLOSED.2006-05-09.54773.topo
           Otherwise the script cannot detect them. You will need to decide which
           surface is the most recent (the best) and the script helps you by listing
           the available surfaces with the most recent one first.
           This sorting ususally works except when the time stamps on the surface files
           are messed up. In such a case you just need to know which one to use.
           Once the Fiducial surface is chosen, it's complimentary surfaces are selected
           using the node number in the file name.
        3. You can tailor the script to your needs. Just make sure you rename it or risk
           having your modifications overwritten with the next SUMA version you install.
        4. The script looks for Fiducial FIDUCIAL Raw RAW VeryInflated VERY-INFLATED VERY_INFLATED Inflated INFLATED
           surfaces, let us know if more need to be sought.
        5. The test data I had contained .R. and .LR. surfaces! I am not sure what .LR.
           means since the surfaces are for one hemisphere but the script will use
           these surfaces too.
        6. If you have reconstructed each hemisphere separately, follow
           these suggestions to keep your life simple.
           Assume Caret results are in Left_Hem/ and Right_Hem/ directories
    
           mkdir LR_Hem
           cp -p Left_Hem/* Right_Hem/* LR_Hem
           cd LR_Hem
           @SUMA_Make_Spec_Caret -sid Joe
               and merge the two specs with:
            inspec -LRmege Joe_lh.spec Joe_rh.spec -prefix Joe_both.spec
    
         R. Reynolds (rickr@codon.nih.gov), Z. Saad (saadz@mail.nih.gov)
