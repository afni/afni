.. _ContributeDocs:

########################
Contribute Your Own Docs
########################

If you feel moved to document a particular aspect of AFNI consider
writing one in the SPHINX format and send it to us. The process should
be relatively simple::

   cd AFNI/doc/SphinxDocs/tutorials
   tcsh @gen_all
   mkdir -p UserDocs/media 
   touch UserDocs/UserDocs.rst
   
.. todo::
   
   Include instructions for how to get needed material from github

All your images would go in directory media/ and all your
documentation can go into UserDocs.rst. Once you have something in
there, add a line in index.rst that parallels the line for
ContributeDocs.rst then from the command line::
   
   cd AFNI/doc/SphinxDocs
   make html
   
For this to work, you will want to have Latex, `Sphinx
<http://sphinx-doc.org>`_ installed, along with `fulltoc
<http://sphinxcontrib-fulltoc.readthedocs.org/en/latest/install.html>`_. An easy  way to install the Sphinx components would be with::
   
   sudo easy_install -U Sphinx
   pip install sphinxcontrib-fulltoc
   
After the build is finished, open :file:`_build/html/index.html` and
browse from there on.

.. _DocTools:

********************
Rst & Sphinx Syntax:
********************

There are plenty of Sphinx tutorials, two important resources are the
RST `primer <http://sphinx-doc.org/rest.html#rst-primer>`_ and the
Sphinx `markup
<http://sphinx-doc.org/markup/index.html#sphinxmarkup>`_
construct. You might just want to start by copying this very file and
start modifying it.
   
   
Some Syntax Examples:
=====================

Here is how we reference a tag in this document tree. This takes you
back to the :ref:`beginning <ContributeDocs>` of this page, and this
next one takes you to some other :ref:`reference
<Color_Plane_Grouping>` elsewhere in this documentation. Use the handy
*Show Source* link towards the bottom of the left sidebar to find
reference tags among other things.

You can also reference terms like :term:`1D` from the glossary easily.

Adding figures:
---------------

Also a breeze as in this example:

.. figure:: media/suma_mni_n27.jpg
   :align: center
   :figwidth: 50%
   :name: media/suma_mni_n27.jpg
      
   :ref:`A random picture here.<media/suma_mni_n27.jpg>`. Note how name is used to create a permalink for the figure which would be accessible from the browser using the link established with 'ref'. Name does not have to be identical to the image file name, but it is a convenient way to do it.

And a few more pictures together:

.. figure:: media/suma_mni_n27_pry1.jpg
   :align: left
   :figwidth: 30%
   :target: ../../_images/suma_mni_n27_pry1.jpg
   :name: media/suma_mni_n27_pry1.jpg
   
   Left aligned! Click on picture to see it in full size. If you're reading the source, the target path is to the image file as copied by the make html process. Images are copied automatically to _build/html/_images/ so you need not make a copy but you have to set the relative path. :ref:`(link)<media/suma_mni_n27_pry1.jpg>`
   
.. figure:: media/suma_mni_n27_pry2.jpg
   :align: right
   :figwidth: 30%
   :name: media/suma_mni_n27_pry2.jpg
   
   :ref:`Right aligned!<media/suma_mni_n27_pry2.jpg>`
   
.. figure:: media/suma_mni_n27_pry3.jpg
   :align: center
   :figwidth: 30%
   :name: media/suma_mni_n27_pry3.jpg
   
   :ref:`Center of course.<media/suma_mni_n27_pry3.jpg>` Note that in the source I add the center figure last. If I define it between the left and right figures, then the spacing gets messed up...
   
   .. container:: clearer
   
      .. image:: media/blank.jpg
   
.. note::
   
   The Preceding block::
      
   .. container:: clearer
   .. image:: media/blank.jpg
   
   is just a trick to keep upcoming text from wrapping around the
   figures. And that here was a demonstration of how to insert a note.
   


Parting note:
^^^^^^^^^^^^^

Once you have something you're pleased with, send us a note, along
with an archive of the directory you have created.

.. todo::

   This should be written to instruct how one can send a pull request
   to github for this.
