Creating strings with special markup for classic and sphinx display::


   Function SUMA_Sphinx_String_Edit is used to take strings with 
   the following special markers and return them formatted in either
   Sphinx or regular text. What follows is a list of special directives
   that change the output string depending on the desired format and a bunch
   examples to illustrate their use.
   
    :SPX: Hiding a SPHINX directive with minimal fanfare:
        Text between :SPX: markers does not appear in default output
        format.
           :SPX: Sphinx chunk :DEF: regular chunk :SPX:
        Use this to insert into a text string a section that is
        only displayed when Sphinx output is requested.
        It is also possible to provide an alternate section
        after the :DEF: marker between the opening and closing
        :SPX: markers. The alternate section is used when the
        requested output format is simple text.
   
        The example coming up next will show how we can have
        alternate output where a key press would be mentioned
        simply in the SUMA output but with a reference directive
        when SPHINX output is used:
   
    :LR: Replace this marker with a new line character for 
         Sphinx output. Cut it out for regular output.
    :LIT: Replace this marker with '::
   ' to mark an upoming literal
          paragraph for sphinx. If the character before :LIT:
          is a non blank, a ':' will terminate the sentence preceding
          the literal paragraph.
          For regular output, :LIT: is cut out if it is preceded by
          a blank. Otherwise it is replaced by a ':'
          Note that the literal paragraph must be indented relative to
          the preceding one.
   
    :ref:`Some Label <reference_key>` Leave such a block untouched for
                                 sphinx format. Replace whole thing
                                 with just 'Some Label' for default format.
   
    :[blanks]: Cut this marker out of string for Sphinx output,
               but keep all blanks and pads with two more in regular
               output to compensate for the ':' characters.
               Also, for the Sphinx format, a newline directly preceding
               the opening ':' gets cut out.
   
    '\|' Escaped vertical bar are kept as such for Sphinx, but shown
          without the escape character in default output. This is
          needed to keep sphinx from considering words between vertical
          bars to be substitution references.
   
    :NOF: When found right after a new line, don't let function 
         SUMA_Offset_Lines() insert any spaces. :NOF: is otherwise cut
         from all output
   
    :=ABIN: Replace with afni bin directory
    :=AFACE: Replace with afni face directory
   
   See function SUMA_Sphinx_String_Edit_Help() for a code sample.
   
   
Strings as defined in the source code::

   Example 1:
   Below you will see a figure directive, but only for Sphinx format.
   :SPX:
   
   .. figure:: media/face_houstonbull.jpg
      :align: center
   
   :SPX:And now the rest of text continues...
   
   Example 2:
   Press buton :SPX::ref:`a <LC_a>`:DEF:'a':SPX: to attenuate...
   
   Example 2.1 (simpler version):
   Press buton :ref:`a <LC_a>` to attenuate...
   
   Example 3:
   For 'Trn' choose one of::LR:
      0: No transparency.
   :    :Surface is opaque.:LR:
      8: 50% transparency.
   :    :Surface is in cheese cloth transparency.:LR:
   
   Example 4:
   ... or if '\|T\|' is used then ...
   
   Example 5:
   A sample file would be: test.1D.col with content:LIT:
      0    0.1 0.2 1   
      1    0   1   0.8 
      4    1   1   1   
      7    1   0   1   
      14   0.7 0.3 0   
   
    -------

Edited for display in AFNI or SUMA::

   Example 1:
   Below you will see a figure directive, but only for Sphinx format.
   And now the rest of text continues...
   
   Example 2:
   Press buton 'a' to attenuate...
   
   Example 2.1 (simpler version):
   Press buton a  to attenuate...
   
   Example 3:
   For 'Trn' choose one of:
      0: No transparency.
  
      Surface is opaque.
      8: 50% transparency.
  
      Surface is in cheese cloth transparency.
   
   Example 4:
   ... or if '|T|' is used then ...
   
   Example 5:
   A sample file would be: test.1D.col with content:
      0    0.1 0.2 1   
      1    0   1   0.8 
      4    1   1   1   
      7    1   0   1   
      14   0.7 0.3 0   
   
    -------

Edited  for  SPHINX::

   Example 1:
   Below you will see a figure directive, but only for Sphinx format.
   
   
   .. figure:: media/face_houstonbull.jpg
      :align: center
   
   And now the rest of text continues...
   
   Example 2:
   Press buton :ref:`a <LC_a>` to attenuate...
   
   Example 2.1 (simpler version):
   Press buton :ref:`a <LC_a>` to attenuate...
   
   Example 3:
   For 'Trn' choose one of:
   
      0: No transparency. Surface is opaque.
   
      8: 50% transparency. Surface is in cheese cloth transparency.
   
   
   Example 4:
   ... or if '\|T\|' is used then ...
   
   Example 5:
   A sample file would be: test.1D.col with content::
   
      0    0.1 0.2 1   
      1    0   1   0.8 
      4    1   1   1   
      7    1   0   1   
      14   0.7 0.3 0   
   
    -------

As would be displayed by SPHINX once compiled:

   Example 1:
   Below you will see a figure directive, but only for Sphinx format.
   
   
   .. figure:: media/face_houstonbull.jpg
      :align: center
   
   And now the rest of text continues...
   
   Example 2:
   Press buton :ref:`a <LC_a>` to attenuate...
   
   Example 2.1 (simpler version):
   Press buton :ref:`a <LC_a>` to attenuate...
   
   Example 3:
   For 'Trn' choose one of:
   
      0: No transparency. Surface is opaque.
   
      8: 50% transparency. Surface is in cheese cloth transparency.
   
   
   Example 4:
   ... or if '\|T\|' is used then ...
   
   Example 5:
   A sample file would be: test.1D.col with content::
   
      0    0.1 0.2 1   
      1    0   1   0.8 
      4    1   1   1   
      7    1   0   1   
      14   0.7 0.3 0   
   
    -------
