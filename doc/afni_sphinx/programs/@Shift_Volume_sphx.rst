.. _ahelp_@Shift_Volume:

*************
@Shift_Volume
*************

.. contents:: 
    :depth: 4 

| 

.. code-block:: none

    
    Usage: @Shift_Volume <[-rai_shift dR dA dI] [-MNI_Anat_to_MNI] [-MNI_to_MNI_Anat]> <-dset DSET> [-no_cp] [-prefix PREFIX]  
    
       Shifts a dataset
       -rai_shift dR dA dI: Move dset by dR dA dI mm (RAI coord sys).
       or:
       -MNI_Anat_to_MNI: (same as -rai_shift-rai_shift 0 -4 -5)
                        Moves a dataset from MNI Anatomical space
                        to MNI space.
       -MNI_to_MNI_Anat: (same as -rai_shift-rai_shift 0 4 5)
                        Moves a dataset from MNI space
                        to MNI Anatomical space.
       For the -MNI_* options, See Eickhoff et al. Neuroimage (25) 2005
       -dset DSET: Typically an anatomical dset to be
                   aligned to BASE.
       -no_cp: Do not create new data, shift existing ones
               This is a good option if you know what you 
               are doing. 
       -prefix PREFIX: Prefix for output dset.
    
    Requires 3drefit newer than Oct. 02/02.
    
    Ziad Saad (saadz@mail.nih.gov)
    SSCC/NIMH/ National Institutes of Health, Bethesda Maryland
