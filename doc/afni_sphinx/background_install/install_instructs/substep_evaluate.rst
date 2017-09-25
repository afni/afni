
a. Check your whole setup!  Display diagnostics in the terminal
   (**read the "Please Fix" section at the end of the output for
   copy/pasteable suggestions**)::
    
       afni_system_check.py -check_all

   Output all results to a text file, and send it to your local AFNI
   Guru for advice::
    
       afni_system_check.py -check_all > out.afni_system_check.txt

#. Open up the AFNI and SUMA GUIs, juuuust to make sure all is well::
   
     afni
     suma

   Report any crashes!

   (*If* you did the optional PyQt4 installation, then you can also
   run ``uber_subject.py`` in a terminal.)

#. **If the "system check" gives any errors,** please:

   - check the list of :ref:`known setup issues <install_error_msgs>`;

   - search on the `Message Board
     <https://afni.nimh.nih.gov/afni/community/board/>`_, and/or put
     the error into google;

   - post a question on the aforementioned `Message Board
     <https://afni.nimh.nih.gov/afni/community/board/>`_.

