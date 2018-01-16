**************
3dDTtoNoisyDWI
**************

.. _3dDTtoNoisyDWI:

.. contents:: 
    :depth: 4 

.. code-block:: none

    
      Take an AFNI-style DT file as input, such as might be output by 3dDWItoDT
      (which means that the DT elements are ordered: Dxx,Dxy,Dyy,Dxz,Dyz,Dzz),
      as well as a set of gradients, and then generate a synthetic set of DWI
      measures with a given SNR. Might be useful for simulations/testing.
    
      Part of FATCAT (Taylor & Saad, 2013) in AFNI.
      It is similar in premise to 3dDTtoDWI, however this allows for the modeled
      inclusion of Rician noise (such as appears in MRI magnitude images).
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + COMMAND: 3dDTtoNoisyDWI -dt_in DTFILE -grads GRADFILE -noise_frac0 FF \
               {-bval BB} {-S0 SS} {-mask MASK } -prefix PREFIX 
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + OUTPUT:
         1) If N gradients are input, then the output is a file with N+1 bricks
         that mimics a set of B0+DWI data (0th brick is the B0 reference).
    
      + RUNNING:
        -dt_in DTFILE    :diffusion tensor file, which should have six bricks
                          of DT components ordered in the AFNI (i.e., 3dDWItoDT)
                          manner:
                          Dxx,Dxy,Dyy,Dxz,Dyz,Dzz.
        -grads GRADFILE  :text file of gradients arranged in three columns.
                          It is assumed that there is no row of all zeros in the
                          GRADFILE (i.e., representing the b=0 line).
                          If there are N rows in GRADFILE, then the output DWI
                          file will have N+1 bricks (0th will be the b=0
                          reference set of noise S0 measures).
        -noise_DWI  FF   :fractional value of noise in DWIs. The magnitude will
                          be set by the b=0 reference signal, S0. Rician noise
                          is used, which is characterized by a standard
                          deviation, sigma, so that FF = sigma/S0 = 1/SNR0.
                          For example, FF=0.05 roughly corresponds to an 
                          SNR0=20 'measurement'.
        -noise_B0   FF2  :optional switch to use a different fraction of Rician
                          noise in the b=0 reference image; one might consider
                          it realistic to have a much lower level of noise in
                          the reference signal, S0, mirroring the fact that
                          generally multiple averages of b=0 acquisitions are
                          averaged together. If no fraction is entered here,
                          then the simulation will run with FF2=FF.
        -prefix PREFIX   :output file name prefix. Will have N+1 bricks when
                          GRADFILE has N rows of gradients.
        -mask   MASK     :can include a mask within which to calculate uncert.
                          Otherwise, data should be masked already.
    
        -bval BB         :optional DW factor to use if one has DT values scaled
                          to something physical (NB: AFNI 3dDWItoDT works in a 
                          world of b=1, so the default setting here is BB=1; one
                          probably doesn't need to change this if using DTs made
                          by 3dDWItoDT).
        -S0  SS          :optional reference b=0 signal strength.  Default value
                          SS=1000.  This just sets scale of output.
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      + EXAMPLE:
        3dDTtoNoisyDWI             \
          -dti_in DTI/DT_DT+orig   \
          -grads GRADS.dat         \
          -noise_DWI 0.1           \
          -noise_B0  0             \
          -prefix NEW_DWIs_SNR10   
    
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    
      If you use this program, please reference the introductory/description
      paper for the FATCAT toolbox:
            Taylor PA, Saad ZS (2013).  FATCAT: (An Efficient) Functional
            And Tractographic Connectivity Analysis Toolbox. Brain 
            Connectivity 3(5):523-535.
