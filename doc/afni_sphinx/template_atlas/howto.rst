.. _tempatl_howto:

*************************************
**How to make an atlas for yourself**
*************************************

This document describes the steps to add a new atlas to AFNI, as
guided by the near spoken-word poetry of DR Glen.

.. contents:: :local:

Overview
--------

Making a new **atlas** involves the following few steps:

i. :ref:`Identify a structures and regions <tempatl_howto_id_a_struc>`
   with an intensity level (all voxels with a value of 10 represent
   the structure named the flasturgium, for instance). For probability
   maps a structure is identified by a particular volume, so sub-brick
   3 might represent the probability of finding the flasturgium at any
   voxel.

#. :ref:`Add the information to the dataset header
   <tempatl_howto_add_hdr_info>` in a particular way (a NIML table).

#. :ref:`Add the atlas to a file <tempatl_howto_add_atlas_file>` to
   let AFNI know that it exists.

#. :ref:`If you want AFNI to use it by default in the AFNI GUI
   <tempatl_howto_add_gui_env>`, add the atlas to an AFNI
   environment variable, and the structures will show up in the
   ``whereami`` output.

If you are also using a new template, you might need to create a new
**template "space"** too:

i.  :ref:`Add the new template and the new space
    <tempatl_howto_new_temp_space>` to the same file from :ref:`Step iii
    <tempatl_howto_add_atlas_file>` that contains the atlas
    information.

#.  :ref:`Add the new space to the default list
    <tempatl_howto_add_default_list>` of template spaces used by AFNI.

Each of the steps is detailed below. 

.. _tempatl_howto_id_a_struc:

Identifying and making regions
------------------------------

The hardest part is figuring out what's what -- drawing or making a
region. You might use AFNI's Draw Dataset to assign a number to that
region. Soon those will automatically be the equivalent of atlases,
and you can skip step 2 if you use this method. Otherwise, programs
like FreeSurfer can do automatic segmentation. You can also draw
regions with other programs like MIPAV, Amira, Osirix and many
others. It's easiest to bring the data into AFNI as NIFTI and then
convert that to AFNI format. Many of the traditional atlases have been
drawn using expert anatomical knowledge and take many months or years
to complete. A few minor details:

Non-probabilistic atlases can be multiple sub-bricks (see the TTatlas
dataset for an example). This is useful for cases where regions might
overlap, or different ways to draw regions (gyral/region). You can
also use separate atlas datasets.

Left or right as part of the name is preferred, but if it isn't there,
the program will try to figure this out from the x>=0 is left, x<0 is
right.

Probabilistic atlases can take up a fair amount of disk space and
memory because a separate volume is loaded for each structure. It's
useful to compress the data to save on disk space. Use ``setenv
AFNI_COMPRESSOR gzip``.

Values for structures should be positive integers for
non-probabilistic atlases and stored as byte or short (16-bit integer)
datasets. For probabilistic atlases, the probability values can be
floating point or integer data. If the data is floating point, the
dataset can still be byte or short with a scaling factor, but it can
also be a float dataset. The byte dataset will save memory and disk
space. If the data is greater than one after any scale factors, then
the probability is assumed to be the value divided by 250.

.. _tempatl_howto_add_hdr_info:

Add the atlas-y-ness information to the dataset header
-------------------------------------------------

The remaining steps have been incorporated into a single script to
make this all easier. First, put all the custom atlases in a specific
directory. Then add them with ``@Atlasize``. The script takes an atlas
dataset and a file with two columns specifying intensity level and
structure name. See ``@Atlasize -help`` for more details on the
options::

  @AfniEnv -set AFNI_SUPP_ATLAS_DIR ~/CustomAtlases/

  @Atlasize                                \
      -space MNI                           \
      -dset hmat_spm_final.nii             \
      -lab_file keys.txt 1 0               \
      -lab_file_delim ';'                  \
      -atlas_type G                        \
      -atlas_name HMAT                     \
      -atlas_description 'Motor Meta'

The ``@Atlasize`` script takes care of the details, but you can do this by
hand if needed. Otherwise, just skip the remaining steps. Phew!

Adding the NIML information for the structure means using a specific
format that organizes the description of the atlas structures with a
simple text description. Using a text editor, create a new file. For
this example, name the file MyAtlasStructures.niml. Add text like the
example below, with an entry for each structure in the atlas::

    # ----------- Atlas Example #PI ---------------------

    <atlas_point_list
     ni_form="ni_group" >

    <ATLAS_POINT
      data_type="atlas_point"
      STRUCT="CSF"
      VAL="1"
      OKEY="1"
      GYoAR="0"
      COG="0.0 0.0 0.0"
      />

    <ATLAS_POINT
      data_type="atlas_point"
      STRUCT="gray"
      VAL="2"
      OKEY="2"
      GyoAR="0"
      COG="0.0 0.0 0.0"
      />

    <ATLAS_POINT
      data_type="atlas_point"
      STRUCT="white"
      VAL="3"
      OKEY="3"
      GyoAR="0"
      COG="0.0 0.0 0.0"
      />     

    </atlas_point_list>


Notice each structure is associated with an "ATLAS_POINT" NIML
element, and all the ATLAS_POINT's are part of a group called
"atlas_point_list". The ATLAS_POINT's begin with "<ATLAS_POINT" and
end with "/>". Similarly, the atlas_point_list is enclosed by
``<atlas_point_list ni_form="ni_group">``. The two attributes that need
to be completed are the "STRUCT=" line and the "VAL=" line. For the
first line, put the name of the structure after "STRUCT=" and then put
the intensity value that is associated with that structure in the
"VAL=" line. The structure name can have spaces, but punctuation will
make it difficult to work with later. Again, the value should be a
positive integer. Other attributes are less important and not strictly
required.  

If there had been a different value used previously for the atlas, you
may set the original key value, OKEY, (not important for this
example). GyoAR sets whether the structure should be identified as
gyrus or area. If you don't want to distinguish between the two, leave
it as 0; otherwise, set it to 1 or 2 for gyrus or area,
respectively. Finally, the "COG" attribute sets the center of gravity
position in RAI coordinates. Use a position that you would like as a
center (maybe a maximum probability or a center of mass). This
position is used in the "Go to atlas location" function in the AFNI
GUI. If you don't need a central location for the structure, just put
"0.0 0.0 0.0".

For probabilistic atlases, you will need to add an additional
attribute of "SB_LABEL=" to give the label of the sub-brick that is
associated with the structure. This label can be the same as the
structure and should be at least two characters. For probabilistic
atlases, the values correspond to the sub-brick number, so the
structure in the first sub-brick gets a value of 0 in the "VAL=0"
line. The second structure gets "VAL=1" and so on.

If there are many structures, you can script the creation of this niml
file. See the examples here for how to do this with Matlab or a tcsh
script. For probabilistic atlases, you will need to make sure the
sub-brick labels match the NIML table SB_LABELs for each
sub-brick. You will need to assign sub-brick labels to the dataset if
those have not already been set. Use commands like this to set the
sub-brick labels::

  3drefit -sublabel 0 "sub_brick_0_label" MyAtlas+tlrc

  3drefit -sublabel 1 "sub_brick_1_label" MyAtlas+tlrc

  ...


Add this NIML table to the header of the dataset with this command::

  3drefit                                                       \
      -atrstring ATLAS_LABEL_TABLE file:MyAtlasStructures.niml  \
      MyAtlas+tlrc

Make the atlas show up in the Overlay panel with an integral colormap
using this command::

  3drefit -cmap INT_CMAP MyAtlas+tlrc

Use ``CONT_CMAP`` for a continuous colormap for probabilistic
atlases. Probabilistic atlases also need the additional attribute,
``ATLAS_PROB_MAP``::

  3drefit -cmap CONT_CMAP MyAtlas+tlrc

  3drefit -atrint ATLAS_PROB_MAP 1 MyAtlas+tlrc

If the dataset is not already associated with a template space, add
that here; otherwise, AFNI won't know for which kinds of datasets this
atlas is useful. For example, if the dataset was made from data that
was aligned to the TT_N27 dataset, you might use a command like this::

  3drefit -space TT_N27 MyAtlas+tlrc 


.. _tempatl_howto_add_atlas_file:

Add the atlas to a NIML text file
----------------------------------

Now you're really almost done. Just add the atlas to a new text file
(for this example, name the file ``myafniatlases.niml``. The file that
contains entries for all the atlases that come with AFNI,
``AFNI_atlas_spaces.niml``, is overwritten with AFNI updates, but
we'll be using a similar format. Use this text as an example::

  <ATLAS
   atlas_name="MyAtlas"  
   dset_name="MyAtlas+tlrc"
   template_space="TT_N27"
   description="My Atlas"
   comment="Created by me for my site and my subjects…"
  ></ATLAS>

For AFNI to use this file to define atlases, just set an environment
variable to point to this file. This is best done in the ``~/.afnirc``
file, but it can also be done on the command line with this::

  setenv  AFNI_SUPP_ATLAS myafniatlases.niml

You can also use the variable, ``AFNI_LOCAL_ATLAS``, for a third atlas
definition file.


.. _tempatl_howto_add_gui_env:

Update AFNI environment list (for GUI functionality)
---------------------------------------------------

Finally, for AFNI to use the atlas automatically, add the new atlas to
AFNI environment list. If you only want to see the new atlas and no
others when you use ``whereami`` or the AFNI GUI, add this to your
``~/.afnirc`` file or type on the command line::
  
  setenv AFNI_ATLAS_LIST "MyAtlas"

or if you want to use any other atlases too at the same time, add
those here::

  setenv AFNI_ATLAS_LIST "MyAtlas,TT_Daemon,CA_EZ_ML,Desai_DD_MPM"

For the other places in the AFNI GUI where atlases are used besides
the ``whereami`` menu, like "Show atlas colors" or "Go to atlas
location", you can also set this variable to use your new atlas by
default::

  setenv AFNI_ATLAS_COLORS MyAtlas

That's all that is needed for your own atlas, but if you need to
create a new space, there are a couple more things you might want to
do...


.. _tempatl_howto_new_temp_space:

Add a template space to AFNI's master list
------------------------------------------

Add a template space to the AFNI atlas definition file you created
earlier. This is a similar format::

    <TEMPLATE_SPACE
      space_name="MySpace"
      generic_space="MySpace"
      comment="Aligned to my average group or specific subject"
    ></TEMPLATE_SPACE>

The generic space is the rough equivalent for the space; this might be
useful if you want to distinguish between a Talairached subject and
generally the Talairach space, for example. In this case, we are
assuming a completely new space.

In the same file, you may also add a definition for a new template and
transformations from or to this space from any other defined template
spaces. The template definition isn't strictly required yet, but will
likely be used in future versions of programs like ``@auto_tlrc``. The
transformations are a little more complicated to describe, but these
transformations provide a connection between a pair of spaces so that
AFNI knows how to use atlases made in one space with a dataset that is
in another space. 

If you want to use the TLRC or MNI_ANAT atlases that come with AFNI,
and your data is not in either of these spaces, you can define that
transformation in the same file. There will be another page describing
the different ways to define these transformations, but look at the
existing AFNI_atlas_spaces.niml file for reference.

.. _tempatl_howto_add_default_list:

Add the template space to the environment variable
------------------------------------------

Add the template space to the environment variable for the default
list of spaces to include in the ``whereami`` and AFNI GUI
``whereami`` output::

  setenv AFNI_TEMPLATE_SPACE_LIST "MySpace,TLRC,MNI,MNI_ANAT"

You have now defined everything required for a new atlas and a new
template space. AFNI will use the variables and definitions you have
created just the same as the TLRC daemon or any other AFNI atlas.
