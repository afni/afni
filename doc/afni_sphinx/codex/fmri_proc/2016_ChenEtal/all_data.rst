
.. begin_title

.. title(s) with links; usually just a single paper here
**Chen GC, Taylor PA, Shin Y-W, Reynolds RC, Cox RW (2016).**
`Untangling the Relatedness among Correlations, Part II: Inter-Subject
Correlation Group Analysis through Linear Mixed-Effects Modeling
<https://www.ncbi.nlm.nih.gov/pubmed/27751943>`_.
Neuroimage 147:825-840.

**Chen GC, Shin Y-W, Taylor PA, Glen DR, Reynolds RC, Israel RB, Cox
RW (2016).** `Untangling the Relatedness among Correlations, Part I:
Nonparametric Approaches to Inter-Subject Correlation Analysis at the
Group Level <https://www.ncbi.nlm.nih.gov/pubmed/27195792>`_.
Neuroimage 142:248-259.  `Corrigendum
<http://www.sciencedirect.com/science/article/pii/S1053811916305754>`_

.. end_title


.. begin_short_tags

:**tags**: naturalistic, EPI, MPRAGE, human, control, adult, Talairach,
           nonlinear, FreeSurfer, fANATICOR

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
   FMRI paradigm:           naturalistic 
   FMRI dset:               EPI          
   Anatomical dset:         MPRAGE       
   Subject population:      human        
   Subject characteristic:  control      
   Subject age:             adult        
   Template space:          Talairach    
   Template align method:   nonlinear    
   Tissue segmentation:     FreeSurfer   
   Tissue regression:       fANATICOR    
   Comments:                
   =======================  ===================

.. end_long_tags


.. NB, nothing needs to be put into this next field-- could just
   remain blank!
.. begin_script_note



.. end_script_note


.. begin_script_table

.. list-table:: 
   :header-rows: 0

   * - |s01a|
     - FreeSurfer segmentation; ``@SUMA_Make_Spec_FS``; tissue selection
   * - |s02a|
     - ``afni_proc.py`` command


.. aliases for scripts, so above is easier to read
.. |s01a| replace:: :download:`s.2016_ChenEtal_01_init.tcsh
                     <fmri_proc/2016_ChenEtal/s.2016_ChenEtal_01_init.tcsh>`
.. |s02a| replace:: :download:`s.2016_ChenEtal_02_ap.tcsh
                     <fmri_proc/2016_ChenEtal/s.2016_ChenEtal_02_ap.tcsh>`
  
.. end_script_table
