Inter Unix 0. Install AFNI and the AFNI_data6 class data tree.

   A. Install AFNI (beyond the scope of this tutorial).

      To install AFNI, please see LLINK:AFNI/pub/dist/HOWTO/howto/ht00_inst/html/index.shtml:HowTo #0:.
      For help with installation difficulties, please search or post to the LLINK:AFNI.board:AFNI Message Board:.

      LLINK:misc/*install.afni:more details:


   B. Install the AFNI_data6 class data tree under the $HOME directory.

      This tutorial relies on the AFNI_data6 directory tree.  In a typical AFNI
      bootcamp, the AFNI_data6 directory will be put under the $HOME directory,
      which is assumed in this tutorial.     

      If you do not yet have the data, use the following commands to download it
      and place it in the $HOME directory:

CODE:
        cd
        curl -O http://afni.nimh.nih.gov/pub/dist/edu/data/AFNI_data6.tgz
        tar xzf AFNI_data6.tgz
CODEEND:

      LLINK:misc/*install.data:more details:


WARNING:
   At this point, it is assumed that AFNI_data6 is located in the $HOME
   directory, and that the AFNI binaries are installed.

   The user should have a termial window open for typing commands.
WARNINGEND:

