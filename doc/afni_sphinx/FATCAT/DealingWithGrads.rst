
.. _DealingWithGrads:

************************************************
**Dealing with DW gradients: 1dDW_Grad_o_Mat++**
************************************************

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
          the *b*\-values are always: :math:`{\rm s~mm}^{-2}` (unless
          otherwise stated.

|

Diffusion gradients
-------------------

The spatial orientations of the applied diffusion weighting gradients
are typically recorded as unit normal vectors, which can be expressed
as (equivalently, just with different notations):

.. math::
   \mathbf{g} &= (g_x, g_y, g_z),~{\rm or}\\
              &= (g_1, g_2, g_3), 

where :math:`g_x^2 + g_y^2 + g_z^2\equiv1` for DWIs, and :math:`g_x =
g_y = g_z = 0` for the *b*\=0 reference images. For example, a
diffusion gradient applied entirely in the from 'top' to 'bottom' in
the *z*\-direction (of whatever set of axes the scanner is using)
might be expressed as (0, 0, 1), and one purely in the *xy*\-plane
could be (0.707, -0.707, 0) or (-0.950, -0.436, 0), etc. 

.. note:: Sometimes the 'reference' images aren't *exactly* totally
          unweighed with *b*\=0. Some data acquisition protocols use a
          magnetic field gradient with a small DW factor, such as
          *b*\=5, as a reference volume.  Such data can be processed
          here, one just needs to include the *b* value information
          explicitly and specify below what DW factor are reference
          values.

The gradient information is often saved in a text file as three rows
of numbers (for example, the ``*.bvecs`` files created by
``dcm2niix``) or as three columns.  If the acquisition contained *N*
reference (*b*\=0) images and *M* DWIs, then these initial files
typically have dimensionality :math:`3\times(N+M)` or
:math:`(N+M)\times3`, respectively.  Additionally, a separate file
may contain the list of DW factors (i.e., the *b*\-values), and there
would be :math:`N+M` in a single row or column.

Diffusion matrices
------------------

The directionality diffusion gradients may also be encoded as a
:math:`3\times3` matrix:

.. math::
   \mathbf{G}= 
   \left[\begin{array}{ccc}
   G_{xx}&G_{xy}&G_{xz}\\
   G_{yx}&G_{yy}&G_{yz}\\
   G_{zx}&G_{zy}&G_{zz}
   \end{array}\right],~~~{\rm or}~~~
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
only six independent components in the :math:`3\times3` matrix.  Thus,
six numbers are recorded in this format. Generally, these are stored
as columns, so that the files would be (following the previous
section's notation) an :math:`(N+M)\times6` array of numbers.

*However, there is the little wrinkle that different programs write
out the components in different ways!*

The standard style for inputting into AFNI functions is 'diagonal
first' *g*-matrix:

.. math::
   G_{xx}~~~ G_{yy}~~~ G_{zz}~~~ G_{xy}~~~ G_{xz}~~~ G_{yz}\,,

while, for example, another output style is 'row first' *g*-matrix
(and it may often explicitly include the factors of two from the
symmetry of the off-diagonals):

.. math::
   G_{xx}~~~2\,G_{xy}~~~2\,G_{xz}~~~G_{yy}~~~2\,G_{yz}~~~G_{zz}\,.

Each of these formats record equivalent information, so it's just a
matter of using the appropriate one with the appropriate software.
When using these dyadic *g*-matrices to record spatial information,
the *b*\-value information sits by itself again, similar as with the
**g** vector gradients.

As a final case, one may include the magnitude of the magnetic fields
with the spatial directionality in a single expression, the
*b*\-matrix:

.. math::
   \mathbf{B}= b \mathbf{G},

where every component of the above dyadic matrix, **G**, is simply
multiplied by the DW factor, *b*.  All the other notations, symmetries
and relations for the *b*\-matrices remain the same as for the
*g*\-matrices, including the distinctions in row- or diagonal-first
notations.  

Of note, TORTOISE functions typically use and output a *row-first*
*b*\-matrix (with the factors of 2, as above).  

.. note:: In some versions of TORTOISE v2.*, there is an 'AFNI_SAVE'
          option that can be used when exporting the *b*\-matrix; in
          this case, the output matrix is *diagonal-first*, such as
          would be called "AFNI-style" above.  **However**, this issue
          is further complicated by the fact that some versions of
          TORTOISE had the factor of 2 included, which AFNI typically
          does not use...  This is actually easily managed because the
          conversion from matrix-to-vector is not affected by that
          factor of two (in an interesting algebraic quirk, only the
          sign information comes from the off-diagonal elements for
          this operation); so one could convert the TORTOISE matrix to
          a vector and then to an AFNI style matrix in such cases.

          TORTOISE v3.* does not appear to have these options as yet;
          we only deal with and convert the TORTOISE-style matrices.


The following figure shows a comparison of the same few lines of *b*\-
and *g*\- matrix and vector formats:

.. list-table:: 
   :header-rows: 1
   :widths: 64 40
   :stub-columns: 0

   *  - Grad/matrix selection
      - Style description
   *  - .. image:: media/dwi_gvec_row.png
           :width: 100%
      - **(row, unit-magnitude) gradient file**; note the arrows on
        the edge signifying that each line is actually wrapped over
        many rows of the text editor
   *  - .. image:: media/dwi_bval_row.png
           :width: 100%
      - **(row) b-value file**; the single line is wrapped around to
        many rows in the text editor
   *  - .. image:: media/dwi_bval.png
           :width: 100%
      - **(column) b-value file**  
   *  - .. image:: media/dwi_gvec.png
           :width: 100%
      - **(column, unit-magnitude) gradient file**
   *  - .. image:: media/dwi_bvec.png
           :width: 100%
      - **(column, DW-scaled) gradient file**
   *  - .. image:: media/dwi_bmatT.png
           :width: 100%
      - **row-first (TORTOISE-style) b-matrix**; the three columns
        with no negative values contain the diagonal elements of the
        matrix; this has a different order and a factor of 2 scaling
        the off-diagonal elements, compared to the 'AFNI-style'.
   *  - .. image:: media/dwi_bmatA.png
           :width: 100%
      - **diagonal-first (AFNI-style) b-matrix**; the three columns
        with no negative values contain the diagonal elements of the
        matrix.
   *  - .. image:: media/dwi_gmatA.png
           :width: 100%
      - **row-first (AFNI-style) g-matrix**

Note that in the 'diagonal-first' matrix case, the first three columns
contain only non-negative (:math:`\geq0`) numbers. This will always be the
case, since the *b*\- or *g*\-matrix is positive definite, and this
property provides a solid hint as to the style of a given matrix
output.  (Columns of off-diagonal elements may or may not contain
negatives). In the 'row-first' cases columns 0, 2 and 5 contain the
matrix diagonals.  The factors of two in the columns representing
off-diagonal matrix elements is apparent when comparing the
*b*\-matrices. Finally, one can see how the *b*\=1000 information
translates into the *b*\-matrix file by comparing the last two rows.

.. note:: This is discussed more below, but current recommendations
          for using AFNI DT-calculating functions (e.g., ``3dDWItoDT``
          and ``3dDWUncert``) is to make AFNI-style *b*\-matrices.  

          1. We like the *b*\-matrix format because we can use all of
             the rows when inputting into ``3dDWItoDT`` or
             ``3dDWUncert`` with the ``-bmatrix_FULL *`` option;
             gradient vector-based options would want one less row,
             just assuming that the 0th volume in the set is *b*\=0,
             which might not be the case.

          2. We like having DW scaling in the matrix info (the
             *b*\value), so that we preserve real physical units in
             the tensor estimates. When using ``3dDWItoDT`` or
             ``3dDWUncert``, one should probably also use the
             ``-scale_out_1000`` switch to have nice numbers, which
             are then interpreted as :math:`10^{-3}~{\rm s~mm}^{-2}`
             instead of the default :math:`{\rm s~mm}^{-2}`; thus, the
             number part for average healthy adult parenchyma would be
             "0.7" (in units of :math:`10^{-3}~{\rm s~mm}^{-2}`)
             rather than "0.0007" (in units of :math:`{\rm
             s~mm}^{-2}`), which might be more annoying for
             bookkeeping/calculations.

          |

Operations
==========

Note the name of the function, ``1dDW_Grad_o_Mat++``, which is now the
recommended processor for gradient/matrix things in AFNI.  It
supercedes the older, clunkier ``1dDW_Grad_o_Mat``.  The newer
``1dDW_Grad_o_Mat++`` has clearer syntax, better defaults and promotes
world peace (in its own small way).

Gradient and matrix information
-------------------------------

#.  The relevant formats described above can be converted among each other
    using ``1dDW_Grad_o_Mat++``. The formats of inputs and outputs are
    described by the option used, as follows:

    .. list-table:: 
       :header-rows: 1
       :widths: 30 30 40
       :stub-columns: 0

       *  - input/option
          - style description
          - example program
       *  - -{in,out}_row_vec
          - row gradients
          - ``dcm2niix`` output, ``TORTOISE`` input
       *  - -{in,out}_col_vec
          - column gradients
          - basic input to ``3dDWItoDT`` (not preferred one, tho')
       *  - -{in,out}_col_matA
          - row-first *g*\- or *b*\-matrices (user can choose scaling)
          - alt. input to ``3dDWItoDT`` (preferred!); (some, maybe)
            ``TORTOISE`` output
       *  - -{in,out}_col_matT
          - diagonal-first *g*\- or *b*\-matrices
          - (some/typical) ``TORTOISE`` output

|

#.  Additionally, the file of *b*\-values may be input after the
    ``-in_bvals *`` option.  This might be requisite if converting
    gradients to *b*\-matrices, for instance (but be sure not to scale
    up an already-scaled set of vectors/matrices!).  One can input
    either a row- or column-oriented file here; ``1dDW_Grad_o_Mat++``
    will know what to do with either one (because it will be
    1-by-something or something-by-1).  When outputting a separate
    file of *b*\-values, one *does* have to specify either row or
    column, using: ``-out_row_bval_sep *`` or ``-out_col_bval_sep *``,
    respectively.

    The *b*\-values can also be used to define which associated
    gradient/matrix entries refer to reference images and which to
    DWIs; if not input, the program will estimate this based on the
    magnitudes of the gradients-- those with essentially zero
    magnitude are treated as reference markers, and the rest are
    treated as DWI markers.  *In general now, the distinction between
    reference and DW-scaled gradients is not very important: we no
    longer average reference volumes by default, and it probably
    shouldn't be done.*

#.  In rare cases, one might want to include a column of *b*\-values
    in the output gradient/matrix file. One example of this is with
    DSI-Studio for HARDI fitting.  One can enact this behavior using
    the ``-out_col_bval`` switch.  The first column of the text file
    will contain the *b*\-values (assuming you either input
    *b*\-matrices or used ``-in_bvals *``). This option only applies
    to columnar output.
   
#.  In contrast to the older ``1dDW_Grad_o_Mat``, the newer
    ``1dDW_Grad_o_Mat`` does **not** try to average *b*\=0 files or to
    remove the top row of reference volumes from the top of the
    gradient/matrix files.  Nowadays, if one inputs a file with *N*
    reference and *M* DW images, the output would have the
    gradients/matrices of all :math:`N+M`.  One major reason for
    preferring using the AFNI-style *b*\-matrix as the format of
    choice is because the full set of :math:`N+M` values are used via
    the ``-bmatrix_FULL *`` option in ``3dDWItoDT``, ``3dDWUncert``,
    etc. (as opposed to :math:`N+M-1` ones if using grads or a
    difference *b*\-matrix option, for historical reasons).
       
    |
    
.. _GradOpsWithImages:

Simultaneous averaging of datasets
----------------------------------

**This is not performed in ``1dDW_Grad_o_Mat``.  We no longer
recommend doing this, based on the way tensor fitting is peformed.**

.. _FlippingGrads:

Flipping Gradients (if necessary) and using @GradFlipTest
---------------------------------------------------------

.. warning:: This is an annoying feature of DWI/DTI processing.
             Probably my least favorite aspect. But it's also quite
             important to understand and deal with (hopefully just
             once at the beginning of a study).

#.  **Preface I:** mathematically, there are a lot of symmetries in
    the diffusion tensor model (and also in HARDI ones, for that
    matter).  A consequence of this is that using a gradient,
    :math:`\mathbf{g} = (g_x, g_y, g_z)`, or its negative,
    :math:`\mathbf{-g} = (-g_x, -g_y, -g_z)`, makes absolutely no
    difference in the model fitting-- the resulting tensor will look
    the same. (NB: this equanimity is *not* referring to twice
    refocused spin-echo EPI or any sequence features-- purely to
    post-acquisition analysis.)

#.  **Preface II:** the above symmetry does not quite apply in the
    case that *not all* components are simultaneously flipped.  If
    just one or two are, then the *scalar* parameter values of the
    tensor will be the same (i.e., things that describe magnitudes,
    such as FA, MD, RD, L1, L2 and L3), but some of the *vector*
    parameters (i.e., the eigenvectors V1, V2 and V3 that describe the
    orientation orientation) can/will differ.  So, if one fits using a
    gradient :math:`\mathbf{g_1} = (g_x, g_y, g_z)` and then with
    another related one :math:`\mathbf{g_2} = (g_x, -g_y, g_z)`, then
    the two fits would have the same scalar parameters (:math:`FA_1 =
    FA_2`, etc.) but different vectors (:math:`V1_1 \neq V1_2`, etc.).

#.  **Preface III:** any two flips leads to equivalent fitting by just
    flipping the third gradient (due to the symmetries described above
    in Prefaces I and II).  Thus, using the gradient
    :math:`\mathbf{g_2} = (g_x, -g_y, g_z)` or :math:`\mathbf{g_3} =
    (-g_x, g_y, -g_z)` would lead to the same tensor fit.

#.  **Preface IV:** the scanner has its own set of coordinate axes,
    and this determines each dataset's origin and orientation (all of
    which can by reading the file's header information, e.g.,
    ``3dinfo -o3 -orient FILE``).  The scanner axes also determine the
    values of the DW gradient/matrix components, both their magnitude
    and sign.

#.  The issue at hand: for some unbeknownst reason, the way gradients
    are stored in dicoms (and subsequent file formats such as NIFTI,
    etc.) may have a *there is a systematic sign change in the
    recorded gradient components, relative to how a software
    interprets them.* The problem takes the following form: a single
    component of each gradient appears to have had its sign *flipped*
    in the output file (always the same gradient per file): for
    example, :math:`g_y \rightarrow -g_y`.

    This is quite an annoying thing to have happen. Furthermore, it
    appears to be dependent as well on the programs used (they often
    have separate conventions). **Fortunately:**
    
    * it is pretty straightforward to determine when gradients and
      data are 'unmatched';
    * there's something that can be done to fix the problem,
      relatively simply; and
    * usually, once you determine the fix for one subject's data set,
      the rest of the data from the same scanner+protocol follows
      suit. *Usually*.
       
#.  For me it is difficult to view eigenvector maps and know what's
    going on, so I use a quick, whole brain (WB) tractography as a way
    to see that things have gone wrong. The premise is that, since the
    directionality of most DTs will be wrong, the most basic WM
    features of the brain, such as the corpus callosum, will not look
    correct (NB: if you are working with subjects whose transcallosal
    fibers may be highly nonstandard, I suggest using a control
    subject for checking about gradient flips).

#.  The solution: flip back against the system! ``1dDW_Grad_o_Mat++``
    contains switches to flip each component (even if one is using
    matrix formats instead of gradients, these apply): ``-flip_x``,
    ``-flip_y``, and ``-flip_z`` (default is just "no flip", but there
    actually is an explicit option for this, ``-no_flip``, which might
    seem useless but actually makes scripting easier).  These can be
    applied individually (as noted in Preface III above,
    mathematically in DTI/HARDI models, flipping any two grads
    simultaneously is equivalent to flipping the third).  At least
    this means that only a few combinations need to be tested.

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

    **but even better nowadays** is that the function
    ``@GradFlipTest`` exists to do this automatically:

    * tensors are fit
    * tracking is performed with each possible flip (x, y, z and none)
    * numbers of long tracts is calculated
    * and based on the relative numbers of tracts, there should be a
      clear winner from the possible options
    * users are prompted to look at the results with ``suma`` commands
      that are displayed in the terminal
    * the best guess is dumped into a file, for scriptability.

    Below are sets of images from (bad) data in need of each potential
    kind of flip, as well as a (good) data which has been properly
    flipped.  From left to right, columns show the following
    tractographic views of the same data set: fronto-coronal WB;
    supero-axial WB; supero-axial ROI (spherical mask located in the
    genu and anterior cingulum bundle):

    .. list-table:: 
       :header-rows: 1
       :widths: 33 33 33
       :stub-columns: 0

       *  - good:  no relative flip
          -  
          -  
       *  - .. image:: media/UNFLIPPED_2.jpg
               :width: 100%
          - .. image:: media/UNFLIPPED_1.jpg 
               :width: 100%
          - .. image:: media/UNFLIPPED_3.jpg
               :width: 100%  

    .. list-table:: 
       :header-rows: 1
       :widths: 33 33 33
       :stub-columns: 0

       *  - bad:  flipped x
          -  
          -  
       *  - .. image:: media/FLIPPED_X_2.jpg
               :width: 100%
          - .. image:: media/FLIPPED_X_1.jpg 
               :width: 100%
          - .. image:: media/FLIPPED_X_3.jpg
               :width: 100%  

    .. list-table:: 
       :header-rows: 1
       :widths: 33 33 33
       :stub-columns: 0

       *  - bad:  flipped y
          -  
          -  
       *  - .. image:: media/FLIPPED_Y_2.jpg
               :width: 100%
          - .. image:: media/FLIPPED_Y_1.jpg 
               :width: 100%
          - .. image:: media/FLIPPED_Y_3.jpg
               :width: 100%  

    .. list-table:: 
       :header-rows: 1
       :widths: 33 33 33
       :stub-columns: 0

       *  - bad:  flipped z
          -  
          -  
       *  - .. image:: media/FLIPPED_Z_2.jpg
               :width: 100%
          - .. image:: media/FLIPPED_Z_1.jpg 
               :width: 100%
          - .. image:: media/FLIPPED_Z_3.jpg
               :width: 100%  

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

Consider a case where ``dcm2niix`` has been used to convert data from
a DWI acquisition, resulting in: a NIFTI file called ``ALL.nii.gz``; a
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
