#!/usr/bin/env python

# ----------------------------------------------------------------------
# This is a library for storing basic infomation regarding options and
# examples for afni_proc.py.
#
#    - examples are stored as individual dictionaries
#    - examples from afni_proc.py -help are created by:
#       - set noglob
#       - add '-optlist_show_argv_array dict' to command
#
#    - for a given example, want:
#       - name (e.g. Example 11) - want case insensitive checking
#       - source (from help, from AFNI_data6, etc.)
#       - keywords?  too hard to match
#       - description ()
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# main array of APExample instances
ap_examples = []

# ----------------------------------------------------------------------
# class definition for instances in ap_examples array
class APExample:
   def __init__(self, name, odict, aphelp=0, source='', descrip=''):
      self.name     = name          # used to refernce example
      self.aphelp   = aphelp        # flag: shown as part of afni_proc.py -help
      self.source   = source        # from AP help, AD6, etc.
      self.descrip  = descrip       
      # self.keywords = keywords

      self.odict    = odict         # dict of options {opt:[params]}

def populate_examples():
   """only populate the examples array if someone wants it
   """
   global ap_examples

   ap_examples.append( APExample( 'Example 1', aphelp=1,
      source='afni_proc.py -help',
      descrip='',
      odict = {
         '-dsets'                    : ['epiRT*.HEAD'],
         '-regress_stim_files'       : ['stims.1D'],
         }))

   ap_examples.append( APExample('Example 2', aphelp=1,
      source='afni_proc.py -help',
      descrip='',
      odict = {
         '-subj_id'                  : ['sb23.e2.simple'],
         '-dsets'                    : ['sb23/epi_r??+orig.HEAD'],
         '-tcat_remove_first_trs'    : ['3'],
         '-regress_stim_times'       : ['sb23/stim_files/blk_times.*.1D'],
         '-regress_basis'            : ['BLOCK(30,1)'],
         }))

   return

if __name__ == '__main__':
   print('** this is not a main module')
   sys.exit(1)

