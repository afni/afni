
a. There is a very useful script to check if everything you need for
   AFNI is set up (R, Python, etc.).  You can:

   - output the check to the screen with::
    
       afni_system_check.py -check_all

   - or output the check to a text file with::
    
       afni_system_check.py -check_all > out.afni_system_check.txt

   which might be useful to email to your local AFNI Guru if there
   are any problems. 

   .. note:: Please read the **please fix** section at the bottom of
             the ``afni_system_check.py`` output. There are often good
             suggestions of commands to copy+paste, and/or to ask your
             nearest AFNI Guru about.

#. It's a good idea to open up AFNI and SUMA, juuuust to make sure all
   is well.  One by one, the following commands should simply present
   the volumetric- and surface-viewers, as well as the processing
   script GUI::
   
     afni
     suma
     uber_subject.py

   Report any crashes!

#. At this point, if your "system check" doesn't really give any
   errors, you're all set to go. If it *did* give some errors, please:

   - check the list of :ref:`known setup issues <install_error_msgs>`;

   - search on the `Message Board
     <https://afni.nimh.nih.gov/afni/community/board/>`_, and/or
     put the error into google;

   - post a question on the aforementioned `Message Board
     <https://afni.nimh.nih.gov/afni/community/board/>`_.
|
