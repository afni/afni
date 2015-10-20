
.. _DealingWithGrads:

**********************************************
Dealing with DW gradients: **1dDW_Grad_o_Mat**
**********************************************

.. contents::
   :depth: 3

Overview
========

In diffusion weighted imaging (DWI), magnetic field gradients are
applied along various spatial directions to probe relative diffusivity
along different orientations. In order to estimate the diffusion
tensor (DT), we need to have a recording of what was the
directionality of each gradient (**g**, a vector of unit length), and
what was the strength of the extra magnetic field (*b*, a scalar) was
used.

Many different programs and software, starting from the MRI machines
themselves, use different notations and methods of reading and writing
the gradient direction and strength information. Drat. Therefore,
there is some need for manipulating them during the analysis process
(sometimes even iteratively to make sure that everything matches).

Since each gradient and *b*\-value have a one-to-one correspondence
with an acquired DWI volume, we would also like to keep the processing
of any one type of data in line with the others.  For example, common
DWI processing includes averaging the *b*\=0 reference images
together, and possibly averaging repetitions of sets of DWIs together
(in both cases, to have higher SNR of individual datasets)-- programs
discussed here allow one to semi-automate these averaging processes
(as well as check that averaging is really feasible) while
appropriately updating gradient information.

.. note:: Below, when referring to DW factors, the assumed units of
          the *b*\-values are always:  :math:`{\rm s~mm}^{-2}`.

|

Diffusion gradients
-------------------

The spatial orientations of the applied diffusion weighting gradients
are typically recorded as unit normal vectors, which can be expressed
as (equivalently):

.. math::
   \mathbf{g} &= (g_x, g_y, g_z),~{\rm or}\\
              &= (g_1, g_2, g_3), 

where :math:`g_x^2 + g_y^2 + g_z^2\equiv1` for DWIs, and :math:`g_x =
g_y = g_z = 0` for the *b*\=0 reference images. For example, a
diffusion gradient applied entirely in the from 'top' to 'bottom' in
the *z*\-direction (of whatever set of axes the scanner is using)
might be expressed as (0, 0, 1), and one purely in the *xy*\-plane
could be (0.707, -0.707, 0) or (-0.950, -0.436, 0), etc. 

.. note:: Sometimes the 'reference' images aren't exactly totally
          unweighed with *b*\=0. Some data acquisition protocols use a
          magnetic field gradient with a small DW factor, such as
          *b*\=5, as a reference volume.  Such data can be processed
          here, one just needs to include the *b* value information
          explicitly and specify below what DW factor are reference
          values.

The gradient information is often saved in a text file as three rows
of numbers (for example, the ``*.bvecs`` files created by ``dcm2nii``)
or as three columns.  If the acquisition contained *N* reference
(*b*\=0) images and *M* DWIs, then these initial files typically have
dimensionality "3 by *N*\+\ *M*" or "*N*\+\ *M* by 3", respectively.
Additionally, a separate file may contain the list of DW factors
(i.e., the *b*\-values), and there would be *N*\+\ *M* in a single row
or column.

Diffusion matrices
------------------

The directionality diffusion gradients may also be encoded as a (3 by 3)
matrix:

.. math::
   \mathbf{G}= 
   \left[\begin{array}{ccc}
   G_{xx}&G_{xy}&G_{xz}\\
   G_{yx}&G_{yy}&G_{yz}\\
   G_{zx}&G_{zy}&G_{zz}
   \end{array}\right],~~{\rm or}~~
   \left[\begin{array}{ccc}
   G_{11}&G_{12}&G_{13}\\
   G_{21}&G_{22}&G_{23}\\
   G_{31}&G_{32}&G_{33}
   \end{array}\right].

The components of the matrix are related to the gradients above:
:math:`G_{xy}\equiv g_x g_y`, :math:`G_{12}\equiv g_1 g_2`,
etc. Formally, **G** is the outer (or dyadic) product of **g**. Here,
the main thing that results from this relation is that **G** is
symmetric (:math:`G_{xy}\equiv G_{yx}`), which means that there are
only six independent components in the 3 by 3 matrix.  Thus, six
numbers are recorded in this format. Generally, these are stored as
columns, so that the files would be (following the previous section's
notation) an "*N*\+\ *M* by 6" array of numbers.

*However, there is the little wrinkle that different programs write
out the components in different ways!*

The standard AFNI style is 'diagonal first': 

.. math::
   G_{xx}, G_{yy}, G_{zz}, G_{xy}, G_{xz}, G_{yz},

while, for example, another output style is 'row first' (and
explicitly includes the factors of two from the symmetry of the
off-diagonals):

.. math::
   G_{xx}, 2\,G_{xy}, 2\,G_{xz}, G_{yy}, 2\,G_{yz}, G_{zz}.

Each of these formats is record equivalent information, it's just a
matter of using the appropriate one with the appropriate software.
When using these dyadic matrices to record spatial information, the
*b*\-value information sits by itself again.

As a final case, one may include the magnitude of the magnetic fields
with the spatial directionality in a single expression, the
*b*\-matrix:

.. math::
   \mathbf{B}= b \mathbf{G},

where every component of the above dyadic matrix, **G**, is simply
multiplied by the DW factor, *b*.  All the other notations, symmetries
and relations remain the same, including the distinctions in row- or
diagonal-first notations.  Of note, TORTOISE by default uses and
outputs a *row-first* *b*\-matrix; if you use the 'AFNI_SAVE' option
at the end of DIFF_PREP, however, the output matrix is actually a
*diagonal-first* *b*\-matrix (AKA: AFNI-style).


The following figure shows a comparison of the same few lines of four
column output formats:

.. tabularcolumns:: |l|l|

+------------------------------------+----------------------------------------+
| Grad/matrix selection              |  Style                                 |
+====================================+========================================+
|.. image:: media/GRAD_Grad.png      | gradient file                          |
|   :width: 4in                      |                                        |
+------------------------------------+----------------------------------------+
|.. image:: media/GRAD_gmatA.png     | diagonal-first (AFNI-style) *g*\-matrix|
|   :width: 4in                      |                                        |
+------------------------------------+----------------------------------------+
|.. image:: media/GRAD_gmatT.png     | row-first (TORTOISE-style) *g*\-matrix |
|   :width: 4in                      |                                        |
+------------------------------------+----------------------------------------+
|.. image:: media/GRAD_bmatT.png     | row-first (TORTOISE-style) *b*\-matrix |
|   :width: 4in                      |                                        |
+------------------------------------+----------------------------------------+


One can verify the dyadic gradient-to-matrix element relation by
comparing values of the uppermost two files (*if* one wants).  Note
that in the 'diagonal-first' case, the first three columns contain
only positive (:math:`\geq0`) numbers. This will always be the case,
since the DT is positive definite, and the property provides a solid
hint as to the style of a given matrix output.  Columns 0, 2 and 5 are
the equivalent ones in the 'row-first' cases (and have matching
values).  The factors of two in the columns representing off-diagonal
DT elements is apparent when comparing the *g*\-matrices. Finally, one
can see how the *b*\=1000 information translates into the *b*\-matrix
file by comparing the last two rows.

|

Operations
==========

Gradient and matrix information
-------------------------------


#.  The relevant formats described above can be converted among each other
    using ``1dDW_Grad_o_Mat``. The formats of inputs and outputs are
    described by the option used, as follows:

    .. _grads_table:

    +---------------------------+---------------------------------------+--------------------------------------------------------+
    |       input/option        |               style                   |       example program                                  |
    +===========================+=======================================+========================================================+
    | -{in,out}_grad_rows       | row gradients                         | dcm2nii output, TORTOISE input                         |
    +---------------------------+---------------------------------------+--------------------------------------------------------+
    | -{in,out}_grad_cols       | column gradients                      | basic input to 3dDWItoDT                               |
    +---------------------------+---------------------------------------+--------------------------------------------------------+
    | -{in,out}_{g,b}matA_cols  | row-first *g*\- or *b*\-matrices      | alt. input to 3dDWItoDT; (some) TORTOISE output        |
    +---------------------------+---------------------------------------+--------------------------------------------------------+
    | -{in,out}_{g,b}matT_cols  | diagonal-first *g*\- or *b*\-matrices | (some) TORTOISE output                                 |
    +---------------------------+---------------------------------------+--------------------------------------------------------+



#.  Additionally, the file of *b*\-values may be input after the
    ``-in_bvals *`` option.  This might be requisite if converting
    gradients to *b*\-matrices, for instance.  

    The *b*\-values can also be used to define which associated
    gradient/matrix entries refer to reference images and which to
    DWIs; if not input, the program will estimate this based on the
    magnitudes of the gradients-- those with essentially zero
    magnitude are treated as reference markers, and the rest are
    treated as DWI markers.  

    In some acquired data, the reference images actually have a small,
    nonzero DW factor applied, such as *b*\=5, so that neither the
    gradient value nor the *b*\-value would be identified as a
    'reference image'.  In this case, one can use the ``-bmax_ref *``
    option to input a number below which *b*\-values will be treated
    as marking reference images.

    .. note:: The great interest in determining which gradient/matrix
       elements correspond to either reference or DW images comes with
       the processing of the DW datasets themselves, as described
       below.  For example, one might want to average together all
       reference images into one, as well as averaging repeated DWI
       sets with each other.  This potentially tedious scripting
       exercise can be slightly automated using the gradient info in
       ``1dDW_Grad_o_Mat``, as described below in :ref:`GradOpsWithImages`.

    

#.  In rare cases, one might want to include a row of *b*\-values in
    the output gradient/matrix file. One example of this is with
    DSI-Studio for HARDI fitting.  One can enact this behavior using
    the ``-out_bval_col`` switch .  The first column of the text file
    will contain the *b*\-values (assuming you either input
    *b*\-matrices or used ``-in_bvals *``). This option only applies to
    columnar output.
   
    

#.  By default, ``1dDW_Grad_o_Mat`` will remove gradient/matrix rows
    corresponding to reference images in the output.  Thus, if one
    inputs a file with *N* reference and *M* DW images, the output
    would have the gradients/matrices of just the *M* DW images. To
    preserve all of the reference values, one can use the
    ``-keep_b0s`` switch.  To remove all reference values but insert a
    row of zeros at the top afterward, one can use the
    ``-put_zeros_top`` switch, instead.

    .. note:: The use of these switches depends on whether one also
              wants to average reference images together, and whether
              one wants the number of gradient/matrix entries to be
              the same as the number of DWI files or not (likely
              determined by the use of particular DT- or
              HARDI-estimating programs).
       
    |
    
.. _GradOpsWithImages:

Simultaneous averaging of datasets
----------------------------------

#.  Generally, DWI data are acquired with multiple reference images
    (*M*\>1), and it might be useful to average these together into a
    single image (at the start of the file) with higher SNR for the
    tensor fitting.  The default behavior of locating and removing
    rows of reference grads/matrices described above can be used to
    aid this.

    Say one starts with *N*\+\ *M* images and grads/matrices.  One can
    input the dataset with the option ``-proc_dset *``.  When
    ``1dDW_Grad_o_Mat`` removes gradients corresponding to the
    reference images, it will identify simultaneously:

    * the related volumes in the dataset, 
    * average them together,
    * and place them as the 0th volume (with the *N* remaining DWIs
      going from 1..end in their original ordering).

    In this case, the output dataset will have *N*\+1 total volumes
    (and the output prefix for it is given via the ``-pref_dset *``
    option).  By default, an output gradient file in this case would
    have only *N* rows, which would be appropriate for default
    ``3dDWItoDT`` usage; other programs might require reinserting a
    row of zeros at the top, parallel to the 0th brick reference
    image, using ``-put_zeros_top``.

    .. note:: There are currently no 'corrective' steps taken in
              ``1dDW_Grad_o_Mat``.  The assumption is that you, the
              user, have performed any corrections for motion, eddy
              currents, EPI distortions, et al. Therefore, you must
              consider the appropriateness of averaging volumes in
              your pipeline, both for reference images here and for
              DWIs (described below).

    
    
#.  Occasionally, diffusion data is acquired with multiple repetitions
    of DWIs.  For example, one might acquire three repetitions of 4
    *b*\=0 images and 30 *b*\=1000 images, for a total of 102 volumes;
    in that case, the 5th, 39th and 73rd bricks will have been
    acquired with the same gradient, etc. However, *you*, the
    analyzer, don't need to do the index math in scripts, because
    ``1dDW_Grad_o_Mat`` can be told to do the appropriate averaging
    among gradients (along with the averaging of the reference images,
    described in the previous section).

    The way to signal ``1dDW_Grad_o_Mat`` to average sets of DWIs is
    to use the ``-dwi_comp_fac *`` to enter the 'compression factor'.
    In this case, with three repeated DWI sets, one would use
    ``-dwi_comp_fac 3`` (and would be so even if the number of
    reference images weren't constant-- this refers only to the DWIs
    themselves). If both the reference images and DWIs are
    respectively averaged, the final data set will have 31 volumes
    (reference one first); with no other flags there would be 30
    gradients, while if using ``-put_zeros_top`` there would be 31.
    
    .. note:: When entering a DWI compression factor, there is a bit
              of an internal check with dot products of the gradients
              to see if they really are the same gradient repeated,
              and a warning will appear if they don't seem similar
              enough.

    |

.. _FlippingGrads:

Flipping Gradients (if necessary)
---------------------------------

.. warning:: This is an annoying feature of DWI/DTI processing.
             Probably my least favorite aspect. But it's also quite
             important to understand and deal with (hopefully just
             once at the beginning of a study).

#.  Preface I: mathematically, there are a lot of symmetries in the
    diffusion tensor model (and also in HARDI ones, for that matter).
    A consequence of this is that using a gradient, :math:`\mathbf{g}
    = (g_x, g_y, g_z)`, or its negative, :math:`\mathbf{-g} = (-g_x,
    -g_y, -g_z)`, makes absolutely no difference in the model
    fitting-- the resulting tensor will look the same. (NB: this
    equanimity is *not* referring to twice refocused spin-echo EPI or
    any sequence features-- purely to post-acquisition analysis.)

    

#.  Preface II: the scanner has its own set of coordinate axes, and
    this determines each dataset's origin and orientation (all of
    which can by reading the file's header information, e.g.,
    ``3dinfo -o3 -orient FILE``).  The scanner axes also determine the
    values of the DW gradient/matrix components, both their magnitude
    and sign.  

    

#.  The issue at hand: for some unbeknownst reason, after converting
    diffusion data from dicom to an analyzable format (such as NIFTI
    or BRIK/HEAD), **the gradient values often don't match well with
    the dataset values.** Specifically, *there is a systematic sign
    change in the recorded gradient components, relative to the
    recorded dataset.* The problem takes the following form: a single
    component of each gradient has had its sign *flipped* in the
    output file (always the same gradient per file)-- for example,
    :math:`g_y \rightarrow -g_y`.

    This is quite an annoying thing to have happen. Furthermore, it
    appears to be dependent as well on the programs used (they somehow
    have separate conventions at times). Fortunately:
    
    * it is pretty straightforward to determine when gradients and
      data are 'unmatched';
    * there's something that can be done to fix the problem,
      relatively simply; and
    * usually, once you determine the fix for one subject's data set,
      the rest of the data from the same scanner+protocol follows
      suit.    
    |
       
#.  The sign flip does **not** affect the scalar DT parameter values
    such as FA, MD, RD, L1, and all others related purely to size and
    shape, due to mathematical symmetries in the DT (and HARDI)
    models.  Therefore, its presence cannot be noticed by looking at
    these scalar maps.  However, the sign flip **does** affect the
    directionality of the modeled shapes, meaning that eigenvectors
    V1, V2 and V3 are rotated in space.

    For me it is difficult to view eigenvector maps and know what's
    going on, so I use a quick, whole brain (WB) tractography as a way
    to see that things have gone wrong. The premise is that, since the
    directionality of most DTs will be wrong, the most basic WM
    features of the brain, such as the corpus callosum, will not look
    correct (NB: if you are working with subjects whose transcallosal
    fibers may be highly nonstandard, I suggest using a control
    subject for checking about gradient flips).

    

#.  The solution: flip back against the system! ``1dDW_Grad_o_Mat``
    contains switches to flip each component (even if one is using
    matrix formats instead of gradients, these apply): ``-flip_x``,
    ``-flip_y``, and ``-flip_z``.  These can be applied individually
    (mathematically in DTI/HARDI models, flipping any two grads
    simultaneously is equivalent to flipping the third, due to the
    sign change symmetry noted at the beginning of this section).  At
    least this means that only a few combinations need to be tested.

    

#.  This then begs the questions, how do you know:
    
    * when you need to perform flipping, and
    * when you have found the correct flipping to do with your data?

    Answer: my preferred method is a visual inspection of a basic,
    whole brain deterministic tractography tracts.  If the whole brain
    mask is called *mask.nii.gz* and the DT parameters are prefixed
    with *DTI/DT*, then this could be calculated and viewed from a
    command line with::

      3dTrackID -mode DET -mask mask.nii.gz -netrois mask.nii.gz    \
           -dti_in DTI/DT -logic OR -prefix DTI/o.WB
      suma -tract DTI/o.WB_000.niml.tract

    Below are sets of images from (bad) data in need of each potential
    kind of flip, as well as a (good) data which has been properly
    flipped.  From left to right, columns show the following
    tractographic views of the same data set: fronto-coronal WB;
    supero-axial WB; supero-axial ROI (spherical mask located in the
    genu and anterior cingulum bundle):


    +------------------------------------+------------------------------------+------------------------------------+
    | good:  no relative flip                                                                                      |
    +====================================+====================================+====================================+
    |.. image:: media/UNFLIPPED_2.jpg    |.. image:: media/UNFLIPPED_1.jpg    |.. image:: media/UNFLIPPED_3.jpg    |
    |   :width: 100%                     |   :width: 100%                     |   :width: 100%                     |
    +------------------------------------+------------------------------------+------------------------------------+

    +------------------------------------+------------------------------------+------------------------------------+
    | bad:  flipped x                                                                                              |
    +====================================+====================================+====================================+
    |.. image:: media/FLIPPED_X_2.jpg    |.. image:: media/FLIPPED_X_1.jpg    |.. image:: media/FLIPPED_X_3.jpg    |
    |   :width: 100%                     |   :width: 100%                     |   :width: 100%                     |
    +------------------------------------+------------------------------------+------------------------------------+

    +------------------------------------+------------------------------------+------------------------------------+
    | bad:  flipped y                                                                                              |
    +====================================+====================================+====================================+
    |.. image:: media/FLIPPED_Y_2.jpg    |.. image:: media/FLIPPED_Y_1.jpg    |.. image:: media/FLIPPED_Y_3.jpg    |
    |   :width: 100%                     |   :width: 100%                     |   :width: 100%                     |
    +------------------------------------+------------------------------------+------------------------------------+

    +------------------------------------+------------------------------------+------------------------------------+
    | bad:  flipped z                                                                                              |
    +====================================+====================================+====================================+
    |.. image:: media/FLIPPED_Z_2.jpg    |.. image:: media/FLIPPED_Z_1.jpg    |.. image:: media/FLIPPED_Z_3.jpg    |
    |   :width: 100%                     |   :width: 100%                     |   :width: 100%                     |
    +------------------------------------+------------------------------------+------------------------------------+

    As seen above, several of the badly flipped sets have (among other
    detrimental features) variously missing corpus
    callosum/genu/splenium/cingulate tracts, poor WB coverage, and
    oddly spiking (blue) tracts in the superior region (known as the
    **bad hair day** effect). In practice, the y-flip might be the
    least obvious to detect at first glance, but several features are
    different-- for instance, the genu and splenium are missing.  The
    badly flipped images are in contrast with the nice, full
    quasi-cauliflower that is the well flipped set in the top row.

    .. note:: Anecdotally, it seems that data from Siemens scanners
              often requires a ``-flip_y`` when using ``3dTrackID``.
              However, it is always worth using a WB tracking run at
              the start of a study in order to check for yourself.

    |

Example commands
----------------

Consider a case where ``dcm2nii`` has been used to convert data from a
DWI acquisition, resulting in: a NIFTI file called ``ALL.nii.gz``; a
row gradient file called ``ALL.bvec``; and a (row) *b*\-value file
called ``ALL.bval``.  Let's say that the acquisition aquired: 4 *b*\=0
reference images; then 30 DW images with *b*\=1000; then another 2
volumes with *b*\=0 and a repeated 30 DW volumes (same gradients) with
*b*\=1000.  To start, there are a total of 66 volumes. Then:

    #. The following produces a gradient file with 3 columns and 66
       rows::

         1dDW_Grad_o_Mat -in_grad_rows ALL.bvec    \
            -out_grad_cols GRAD_ALL.dat            \
            -keep_b0s

    #. The following flips the y-component of the input DW gradients
       and produces a row-first *b*\-matrix file with 66 rows::

         1dDW_Grad_o_Mat -in_grad_rows ALL.bvec    \
            -in_bvals ALL.bval                     \
            -out_bmatT_cols BMAT_ALL.dat           \
            -keep_b0s                              \
            -flip_y

       

    #. The following produces a gradient file with 3 columns and 60
       rows (reference grads are not kept), and a dataset with 61
       volumes (reference images have been averaged, with the
       resulting volume at brick [0])::

         1dDW_Grad_o_Mat -in_grad_rows ALL.bvec    \
            -out_grad_cols GRAD_allDWI.dat         \
            -proc_dset ALL.nii.gz                  \
            -pref_dset AVEB0_allDWI.nii.gz

    #. The following adds DWI averaging to the previous command,
       producing a grad file of 30 rows and a dataset with 31
       volumes::

         1dDW_Grad_o_Mat -in_grad_rows ALL.bvec    \
            -out_grad_cols GRAD_aveDWI.dat         \
            -dwi_comp_fac 2                        \
            -proc_dset ALL.nii.gz                  \
            -pref_dset AVEB0_aveDWI.nii.gz

    #. The following first selects only the first 25 acquisitions (for
       example, if motion had occured), averages the reference images,
       and puts a row of zeros at the top of the file; therefore, the
       output grad file has 22 columns (four reference images averaged
       to 1, plus the remaining 21 DWIs), as does the output dataset::

         1dDW_Grad_o_Mat -in_grad_rows ALL.bvec'[0..24]'  \
            -out_grad_cols GRAD_mot25.dat                 \
            -proc_dset ALL.nii.gz'[0..24]'                \
            -pref_dset AVEB0_mot25.nii.gz                 \
            -put_zeros_top

       .. note:: Subset selection works similarly as in other AFNI
                 programs, both for datasets and the row/column
                 files. For row text files, one uses square-brackets
                 '[*i*..\ *j*\]' to select the gradients *i* to
                 *j*. For column text files, one would do the same
                 using curly brackets '{*i*..\ *j*}'.

    #. Consider the same data acquisition and file naming conventions
       as above, but where the reference volumes were actually
       acquired with small but nonzero DW factors *b*\=5. Then, there
       are no '0 0 0' gradients, and to determined reference volumes,
       we instead have to look where *b*\-values are <6, for example.
       The following produces a gradient file with 60 rows and a
       dataset with 61 volumes::

         1dDW_Grad_o_Mat -in_grad_rows ALL.bvec    \
            -in_bvals ALL.bval                     \
            -bmax_ref 6                            \
            -out_grad_cols GRAD_allDWI.dat         \
            -proc_dset ALL.nii.gz                  \
            -pref_dset AVEB0_allDWI.nii.gz
