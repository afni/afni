TO GENERATE ALL THE HELPS
-------------------------

To make the table of links to all Sphinxified AFNI helps, run:

    help2sphinx.py -OutFolder ../programs



TO GENERATE THE CLASSIFIED HELP
-------------------------------

The file for people to edit/regroup/add/subtract programs and
descriptions is: list_AFNI_PROGS_classed.txt.

Please pay attention to the formatting therein, esp. the "++" starting
the Group Title lines (and the Group Rank must follow the "++" in each
case), and "::" demarking things.

Making the RST file is a 2-step process:

    convert_list_to_fields_pandas.py         \
       list_AFNI_PROGS_classed.txt           \
       list_STYLED_NEW.txt

    convert_fields_to_rst.py                 \
       list_STYLED_NEW.txt                   \
       ../educational/classified_progs.rst

