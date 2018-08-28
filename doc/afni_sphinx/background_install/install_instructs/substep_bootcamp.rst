
Copy+paste::

  curl -O https://afni.nimh.nih.gov/pub/dist/edu/data/CD.tgz
  tar xvzf CD.tgz
  cd CD
  tcsh s2.cp.files . ~
  cd ..

**Purpose:** In order, these commands: download the tarred+zipped
directory of class data (named "CD") to the current directory;
untar/unzip it (= open it up); move into the newly opened directory;
execute a script to copy the files to ``$HOME/CD/``; and finally exit
the directory.

At this point, if there have been no errors, you can delete/remove
the tarred/zipped package, using "``rm CD.tgz``".  If you are
*really* confident, you can also deleted the CD tree in the present
location (but leaving it in ``$HOME/CD/``).

.. note:: If using Linux terminal commands is new to you, then **do**
          look over the handy :ref:`Unix documentation/tutorial
          <U_misc_bg0>` and practice a few of the basic commands on
          your own system (e.g., `ls`, `cd`, `less`, etc.). It will
          *greatly* enhance your bootcamp experience-- we promise!


