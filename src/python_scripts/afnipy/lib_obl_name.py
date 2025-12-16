#!/usr/bin/env python

# Part of a library of functions for dealing with obliquity in
# datasets.
#
# Here, we store an object that divides up a potential input or output
# name (which can contain path info) into various pieces.
#
# auth : PA Taylor (SSCC, NIMH, NIH, USA)
# ============================================================================

import sys, os

from   afnipy import afni_base as ab
from   afnipy import afni_util as au

# ============================================================================

# pieces of possible BRIK/HEAD file extension, which might get removed;
# NB: the order matters, with the right-most pieces first in the list
LIST_afni_ext_pieces = [ '.gz', 'BRIK', 'HEAD', '.', \
                         '+orig', '+tlrc', '+mni', '+acpc' ]

class NameObj:
    """Object for pieces of an input or prefix name.  Some of the
functionality exists here specifically to deal with the fact that the
main input string might be a file that exists (with various
flexibility for file type and subset of string used to refer to a
file, esp. for BRIK/HEAD files), or a prefix of something that does
not yet exist.

This object breaks down the input name and stores it as variously
useful pieces (including expansions).  In terms of notations:

  name*  : refers to the full name (including path)
  bname* : refers to just the basename part of a file (no path)
  dname* : refers to just the dirname part of a file (only path)

The avsp (=`3dinfo -av_space ..`) info can also be added, which might
be useful if making the output prefix for a new AFNI-format dset

Parameters
----------
name : str 
    str of an actual or potential file or prefix name; may contain
    path info
avsp : str 
    str of the AFNI viewspace info

    """

    def __init__(self, name=None, avsp=None):

        # ----- set up attributes

        # main input variables
        self.name            = name    # copy of input name
        self.avsp            = avsp    # AFNI view space
        self.name_full       = ''      # dirname + header_name
        self.name_full_noext = ''      # dirname + bname_noext
        self.name_fullest    = ''      # dirname + bname_noext [+ext, if known]

        # basename info
        self.bname           = ''      # basename of name w/o (opt) path
        self.bname_noext     = ''      # bname w/o (opt) ext
        self.bname_type      = ''      # 'NIFTI' or 'AFNI'

        # dirname info
        self.dname           = ''      # dirname of name
        self.dname_abs       = ''      # abs path of dname

        # ----- take action(s)

        # prelim stuff
        if name :
            tmp1 = self.set_bname_info()
            tmp2 = self.set_dname_info()
            tmp3 = self.set_extra_name_info()

    # ----- methods
    
    def set_extra_name_info(self):
        """Generate extra name info beyond what is provided in input, like:
        name_full
        name_full_noext
        name_fullest
        """

        self.name_full       = self.dname + '/' + self.bname
        self.name_full_noext = self.dname + '/' + self.bname_noext

        # For all NIFTI, and for AFNI w/o avsp, this is the rule...
        self.name_fullest = self.name_full
        # ... but, in one case, this can be derived otherwise
        if self.bname_type == 'AFNI' and not(self.avsp is None) :
            self.name_fullest = self.name_full_noext + self.avsp + '.HEAD'

    def print_dict(self):
        """Print the contents of the object, for debugging/display."""

        print("++++ NameObj for name: " + self.name)
        all_keys = list(self.__dict__.keys())
        all_len  = [len(key) for key in all_keys]
        maxlen   = max(all_len) + 2

        print('-'*60)
        for key in all_keys:
            print("{:<{:d}s} : {:<s}".format(str(key), maxlen, 
                                             str(self.__dict__[key])))
        print('-'*60)

    def set_bname_info(self):
        """Generate various pieces of basename info from name:
        bname
        bname_noext
        bname_type
        """

        try:
            # use 3dinfo to get most info, if name is an existing dset
            exists, is_nifti, name_noext, header_name = \
                au.info_dset_exists_with_names(self.name)

            if not(exists):
                sys.exit(-1) # unseen error just to use other branch
            self.bname       = os.path.basename(header_name)
            self.bname_noext = name_noext

            if is_nifti :
                if header_name.endswith('.nii.gz') :
                    self.bname_type = 'NIFTI_GZ'
                elif header_name.endswith('.nii') :
                    self.bname_type = 'NIFTI'
                else:
                    ab.EP("Unknown ext on NIFTI dset: " + name)
            else:
                self.bname_type = 'AFNI'
        except:
            # name is not a real dset (e.g., might be potential output)
            self.bname = os.path.basename(self.name)

            if (self.name).endswith('.nii') :
                self.bname_type  = 'NIFTI'
                self.bname_noext = self.bname[:-4]
            elif (self.name).endswith('.nii.gz') :
                self.bname_type  = 'NIFTI_GZ'
                self.bname_noext = self.bname[:-7]
            else:
                self.bname_type  = 'AFNI'
                self.bname_noext = self.bname
                # ... and remove any possible AFNI ext piece
                for ext in LIST_afni_ext_pieces :
                    if (self.bname_noext).endswith(ext) :
                        self.bname_noext = self.bname_noext[:-len(ext)]

        return 0

    def set_dname_info(self):
        """Generate various pieces of dirname info from name:
        dname
        dname_abs
        """

        self.dname     = au.get_dirname_from_prefix(self.name)
        self.dname_abs = os.path.abspath(self.dname)

        return 0

    # ----- decorators

    @property
    def add_ext_for_nifti(self):
        """Provide an appropriate extension for NIFTI, like .nii or .nii.gz;
        if the object name is an AFNI one, then provide an empty str."""

        if self.bname_type == 'NIFTI' :
            return '.nii'
        elif nobj.bname_type == 'NIFTI_GZ' :
            return '.nii.gz'
        else:
            return ''

# ============================================================================

if __name__ == "__main__" :

    # an example use case
    print("++ No example")

