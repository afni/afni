.. _DealingWithGrads:

======================
Dealing with gradients
======================

.. contents::
   :depth: 3

Overview
========

In diffusion weighted imaging (DWI), magnetic field gradients are
applied along various spatial directions to probe relative diffusivity
along different orientations. In order to estimate the diffusion
tensor (DT), we need to have a recording of what was the
directionality of each gradient (**g**, a vector), and what was the
strength of the extra magnetic field (*b*, a scalar) was used.  

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

.. note:: Below, when refering to DW factors, the assumed units of the
          *b*\-values are always: :math:`{\rm s~mm}^{-2}`.

|

Diffusion gradients
-------------------

The spatial orientations of the applied diffusion weighting gradients
are typically recorded as unit normal vectors which can be expressed
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

where every component of the above dyadic matrix, **G**, is simply multiplied
by the DW factor, *b*.  All the other notations, symmetries and relations
remain the same, including the distinctions in row- or diagonal-first
notations.  Of note, TORTOISE uses and outputs a *row-first* *b*\-matrix.


Operations
==========

Gradient and matrix information
-------------------------------


#.  The relevant formats described above can be converted among each other
    using ``1dDW_Grad_o_Mat``. The formats of inputs and outputs are
    described by the option used, as follows:

    .. _grads_table:

    +---------------------------+---------------------------------------+--------------------------------+
    |       input/option        |               style                   |       example program          |
    +===========================+=======================================+================================+
    | -{in,out}_grad_rows       | row gradients                         | dcm2nii output, TORTOISE input |
    +---------------------------+---------------------------------------+--------------------------------+
    | -{in,out}_grad_cols       | column gradients                      | basic input to 3dDWItoDT       |
    +---------------------------+---------------------------------------+--------------------------------+
    | -{in,out}_{g,b}matA_cols  | row-first *g*\- or *b*\-matrices      | alt. input to 3dDWItoDT        |
    +---------------------------+---------------------------------------+--------------------------------+
    | -{in,out}_{g,b}matT_cols  | diagonal-first *g*\- or *b*\-matrices | TORTOISE output                |
    +---------------------------+---------------------------------------+--------------------------------+

    |

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

    |

#.  In rare cases, one might want to include a row of *b*\-values in
    the output gradient/matrix file. One example of this is with
    DSI-Studio for HARDI fitting.  One can enact this behavior using
    the ``-out_bval_col`` switch .  The first column of the text file
    will contain the *b*\-values (assuming you either input
    *b*\-matrices or used ``-in_bvals *``). This option only applies to
    columnar output.
   
    |

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

Simultaneous operations with images
-----------------------------------

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

    |
    
#.  Occasionally, diffusion data is acquired with multiple repetitions
    of DWIs.  For example, one might acquire three repetitions of 4
    *b*\=0 images and 30 *b*\=1000 images, for a total of 102 volumes;
    in that case, the 5th, 39th and 73rd bricks will have been
    acquired with the same gradient, etc. However, *you*, the
    analyzer, don't need to do the index math, because
    ``1dDW_Grad_o_Mat`` can be told to do the appropriate averaging
    among gradients (along with the averaging of the reference images,
    described in the previous section).

    


