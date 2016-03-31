
a. There is a very useful script to check on your installed AFNI
   and lots of its dependencies, such as looking for the installed
   R libraries, profiles, Python stuff, etc. You can run it

   - outputting to the screen::
    
       afni_system_check.py -check_all

   - outputting to a text file::
    
       afni_system_check.py -check_all > out.afni_system_check.txt

   which might be useful to email to your local AFNI Guru if there
   are any problems. 

#. So, at this point, if your "system check" doesn't really give
   any errors, you're all set to go. If it *did* give some errors,
   please:

   - check the list of :ref:`known setup issues <install_error_msgs>`;

   - search on the `Message Board
     <https://afni.nimh.nih.gov/afni/community/board/>`_, and/or
     put the error into google;

   - post a question on the aforementioned `Message Board
     <https://afni.nimh.nih.gov/afni/community/board/>`_.
|
