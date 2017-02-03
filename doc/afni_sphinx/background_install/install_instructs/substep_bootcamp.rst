
This step is required if you are about to attend a Bootcamp::

  curl -O https://afni.nimh.nih.gov/pub/dist/edu/data/CD.tgz
  tar xvzf CD.tgz
  cd CD
  tcsh s2.cp.files . ~
  cd ..

In order, these commands: get the tarred+zipped directory that
contains the class data (and is hence named "CD"), downloading it to
the current location in the terminal; untar/unzip it (=opens it up);
go into the newly opened directory; execute a script to copy the files
to ``$HOME/CD/``; and finally exit the directory.

At this point, if there have been no errors, you can delete/remove
the tarred/zipped package, using "``rm CD.tgz``".  If you are
*really* confident, you can also deleted the CD tree in the present
location (but leaving it in ``$HOME/CD/``).

.. note:: **Don't forget** to give a quick glance at the handy
          :ref:`Unix documentation/tutorial <U_misc_bg0>`. And why not
          practice a few of the basic commands on your own system
          (e.g., `ls`, `cd`, `less`, etc.)? It will *greatly* enhance
          your bootcamp experience-- we promise!


