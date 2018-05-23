 
.. begin_title

.. title(s) with links; usually just a single paper here

**Chen GC, Cox RW, Glen DR, Rajendra JK, Reynolds RC, Taylor PA
(2018).** `A tail of two sides: Artificially doubled false positive
rates in neuroimaging due to the sidedness choice with t-tests
<https://www.biorxiv.org/content/early/2018/05/23/328567>`_

.. end_title


.. begin_short_tags

:**tags**: task-block, EPI, MPRAGE, human, control, adult, MNI,
           nonlinear, 

.. end_short_tags


.. begin_long_tags

.. full table format of search strings
.. table::
   :column-alignment: left 
   :column-wrapping: true 
   :column-dividers: double single double

   =======================  ===================
   Tag                      Label
   =======================  ===================
   FMRI paradigm:           task-block 
   FMRI dset:               EPI          
   Anatomical dset:         MPRAGE       
   Subject population:      human        
   Subject characteristic:  control      
   Subject age:             adult        
   Template space:          MNI    
   Template align method:   nonlinear    
   Tissue segmentation:     -   
   Tissue regression:       -
   Comments:                
   =======================  ===================

.. end_long_tags


.. NB, nothing needs to be put into this next field-- could just
   remain blank!
.. begin_script_note

   These scripts describe different approaches for processing FMRI
   data with AFNI.  Please read the comments at the tops of the
   scripts carefully, as well as the bioRxiv papers associated with
   each, in order to understand the steps.

.. end_script_note


.. begin_script_table

.. list-table:: 
   :header-rows: 0

   * - |s01|
     - An example of group level analyses with two-tailed testing
       (using 3dMEMA, 3dClustSim and 3dClusterize, among others)


.. aliases for scripts, so above is easier to read
.. |s01| replace:: :download:`s.nimh_group_level_02_mema_bisided.tcsh
                   <fmri_proc/2018_ChenEtal/s.nimh_group_level_02_mema_bisided.tcsh>`

.. end_script_table
