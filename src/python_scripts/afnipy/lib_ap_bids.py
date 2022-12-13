#!/usr/bin/env python

# python3 status: compatible

# -------------------------------------------------------------------------
#
# A rough draft at storing data from BIDS datasets.  The goal of this
# is to build something usable for an afni_proc.py BIDS-App. 
#
# The means for *getting* information are not going to be used
# longterm.  A BIDS expert will know better approaches for that aspect.
#
# -------------------------------------------------------------------------
#
# This basically comprises a set of objects to contain the
# (conceptual) data hierarchy:
# 
# dataset (AKA data_collection; daset)
# |----subject (subj: sub-001, sub-002, ...)
#      |----ses (ses: ses-01, ses-02, ...; also sometimes just 'ses')
#           |----data_type (AKA modality; datype: anat, func, ...)
#                |----twig (prefix of NIFTI, JSON, ...)
#                     |----archive info (archinfo: dictionary from archivotron)
#                     |----extension list (list_ext: list of file exts for twig)
#
# Here, these are represented as nested dictionaries, with each set of
# keys being the items of the next level down, until one gets to
# data_files, which just exist in a list.
#
# In *many* cases, the hierarchical level is also the name of a
# directory containing other objects, until one reaches the
# 'data_files', which are just 'leaf' items.  However, the 'ses' level
# might not exist explicitly, breaking the concept-directory duality.
# We deal with that here by labelling such as a session as 'ses', with
# no hyphen, in the dictionary of hierarchies.
#
# -------------------------------------------------------------------------

import sys, os, glob
import subprocess
import argparse

from archivotron import bids
BIDS_GENERATOR = bids.generate_bids()

TWIG_EXTENSIONS = (
    '.nii.gz',
    '.nii',
    '.json',
    '.HEAD',
)

# -------------------------------------------------------------------------

def make_abspath_from_dirname(dirname):
    '''
    Parameters
    ----------
    dirname : str
              relative or abs path

    Return 
    ------
            : str
              abs path name
    '''

    if not os.path.isdir(dirname) :
        print("** WARN: {} is not a valid dirname".format(dirname))
        return ''
    return os.path.abspath(dirname)

def make_tail_from_dirname(dirname):
    '''
    Parameters
    ----------
    dirname : str
              relative or abs path

    Return 
    ------
    tail    : str
              tail of abs path (i.e., last item in path)
    '''

    if not os.path.isdir(dirname) :
        print("+* WARN: {} is not a valid dirname".format(dirname))
        return ''

    apath = os.path.abspath(dirname)
    tail  = apath.split('/')[-1]

    if not( len(tail) ):
        print("+* WARN: cannot find tail from dir: {}\n"
              "   abspath = {}".format(dirname, apath))
        return ''
    return tail

# -------------------------------------------------------------------------

class bids_dataset:
    """An object storing the full BIDS tree for a dataset.  

    Importantly, contains a dictionary of session info (dict_subj).

    """

    def __init__(self, dirname, label='', verb=0):
        dirname = dirname.rstrip('/')          # remove any trailing '/'

        self.dirname  = dirname                # entered path
        self.abspath  = ''                     # abs path of dirname
        self.daset    = ''
        self.label    = label                  # dataset/proj label

        self.list_subjname = []                # list of 'sub-*' str
        self.nsubj         = 0     
        self.dict_subj     = {}                # dict of bids_subject objs

        self.verb          = verb
        # -----------------------

        self.abspath = make_abspath_from_dirname(dirname)
        self.daset   = make_tail_from_dirname(dirname)

        self.set_list_subjname()

        # populate all ses for this subj
        if self.nsubj :
            for subj in self.list_subjname:
                self.add_subj(subj)

    # ----------------------------------------------------------

    def set_list_subjname(self):
        all_subj = glob.glob(self.abspath + '/sub-*')
        all_subj.sort()
        nsubj = len(all_subj)

        if nsubj :
            self.has_subj      = True
            self.list_subjname = [x.split('/')[-1] for x in all_subj]
            self.nsubj         = nsubj
            if self.verb :
                print("++ list of {} subj:\n   {}"
                      "".format(nsubj, '\n   '.join(self.list_subjname)))
        else:
            # problem if no other dirs exist
            print("** ERROR: no subj in '{}'?".format(self.abspath))
            sys.exit(1)

    def add_subj(self, subj):
        """Add a subj.

        This increases the size of the object's dictionary of subj,
        self.dict_subj.

        Parameters
        ----------
        subj : str
               name of subj and subj directory

        """
        subjdir = self.dirname + '/' + subj
        self.dict_subj[subj] = bids_subject( subjdir,
                                                 verb   = self.verb )

# -------------------------------------------------------------------------

class bids_subject:
    """An object storing the full BIDS tree for a subj.  

    Importantly, contains a dictionary of session info (dict_ses).

    Trickily, there may or may not be a ses-level dir for this
    (reflected in ses ID).  We refer to this as an "unseparated"
    session, and it will just bear the label 'ses' (rather than 'ses-*').

    """

    def __init__(self, dirname, verb=0):
        dirname = dirname.rstrip('/')          # remove any trailing '/'

        self.dirname  = dirname                # entered path
        self.abspath  = ''                     # abs path of dirname
        self.subj     = ''                     # subj ID

        self.has_ses      = None               # does 'ses-*' level exist?
        self.list_sesname = []                 # list of 'ses-*' str
        self.nses         = 0     
        self.dict_ses     = {}                 # dict of bids_session objs

        self.verb         = verb
        # -----------------------

        self.abspath = make_abspath_from_dirname(dirname)
        self.subj    = make_tail_from_dirname(dirname)

        self.set_list_sesname()

        # populate all ses for this subj
        if self.has_ses :
            for ses in self.list_sesname:
                self.add_ses_sep(ses)
        elif self.has_ses == False :
            self.add_ses_unsep()

    # ----------------------------------------------------------

    def set_list_sesname(self):
        all_ses = glob.glob(self.abspath + '/ses-*')
        all_ses.sort()
        nses = len(all_ses)

        if nses :
            self.has_ses      = True
            self.list_sesname = [x.split('/')[-1] for x in all_ses]
            self.nses         = nses
            if self.verb :
                print("++ list of {} ses:\n   {}"
                      "".format(nses, '\n   '.join(self.list_sesname)))
        elif len(next(os.walk(self.abspath))[1]) :
            # check if there are ANY other (=non-ses) dirs here
            self.has_ses  = False
        else:
            # problem if no other dirs exist
            print("** ERROR: no directories in '{}'?".format(self.abspath))
            sys.exit(1)

    def add_ses_sep(self, ses):
        """Add a session that is 'separate' in the sense of existing in a
        'ses-*' dir of its own.

        This increases the size of the object's dictionary of
        sessions, self.dict_ses.

        Parameters
        ----------
        ses : str
              name of session and session directory

        """
        sesdir = self.dirname + '/' + ses
        self.dict_ses[ses] = bids_session( sesdir,
                                           subj = self.subj, 
                                           ses  = ses,
                                           verb = self.verb )

    def add_ses_unsep(self):
        """Add a session that is 'unseparated' in the sense of NOT existing in
        a 'ses-*' dir of its own.  There can be only one of these, by
        definition.  The key of this in the dictionary will just be
        'ses'.

        This increases the size of the object's dictionary of
        sessions, self.dict_ses.

        Parameters
        ----------
        ses : str
              name of session and session directory

        """

        # This ses is wicked, tricksy, false! Note the tricksiness here.
        ses    = 'ses'  
        sesdir = self.dirname 

        self.dict_ses[ses] = bids_session( sesdir,
                                           subj = self.subj, 
                                           ses  = ses,
                                           verb = self.verb )

# -------------------------------------------------------------------------

class bids_session:
    """An object storing the session-level information for a subject.

    Importantly, contains a dictionary of 'data_type' info
    (dict_datype).

    NB: we introduce the term "datype", because "datatype" and "dtype"
    exist already in Python. **This term might change to a better one
    in development**

    """

    def __init__(self, dirname, subj='', ses='', verb=0):
        dirname        = dirname.rstrip('/')

        self.dirname  = dirname                # directory
        self.abspath  = ''                     # abs path of dirname
        self.subj     = subj                   # subj ID
        self.ses      = ses                    # ses ID; stays '' if not subdir

        self.list_datypename = []              # anat, func, etc.
        self.ndatype         = 0
        self.dict_datype     = {}              # dictionary of interest

        self.verb            = verb
        # -----------------------

        self.abspath = make_abspath_from_dirname(dirname)

        self.set_list_datypename()

        # populate all datype for this ses
        if self.ndatype :
            for datype in self.list_datypename:
                self.add_datype(datype)

    # ----------------------------------------------------------

    def set_list_datypename(self):
        all_datype = next(os.walk(self.abspath))[1]  # all dirs present
        all_datype.sort()
        ndatype = len(all_datype)

        if ndatype :
            self.list_datypename = [x.split('/')[-1] for x in all_datype]
            self.ndatype         = ndatype
            if self.verb :
                print("++ list of {} data types in {}:\n   {}"
                      "".format( ndatype, self.ses,
                                 '\n   '.join(self.list_datypename) ))
        else:
            print("** WARN: no datype dirs in '{}'?".format(self.abspath))

    def add_datype(self, datype):
        """Add a datype directory (i.e., anat, func, etc.).

        This increases the size of the object's dictionary of
        BIDS data_types, self.dict_datype.

        Parameters
        ----------
        datype : str
              name of datype and datype directory

        """
        datypedir = self.dirname + '/' + datype
        self.dict_datype[datype] = bids_datype( datypedir,
                                                subj   = self.subj, 
                                                ses    = self.ses,
                                                datype = datype,
                                                verb   = self.verb )

# -------------------------------------------------------------------------

class bids_datype:
    """An object storing the BIDS data_type (datype)-level (contents of
    anat/, func/, etc.) information for a subject.

    NB: we introduce "datype", because "datatype" and "dtype" exist
    already in Python.

    Importantly, contains a dictionary of 'data_files' info
    (dict_dafile).

    """

    def __init__(self, dirname, subj='', ses='', datype='', verb=0):
        dirname       = dirname.rstrip('/')

        self.dirname  = dirname                # directory
        self.abspath  = ''                     # abs path of dirname

        self.subj     = subj                   # subj ID
        self.ses      = ses                    # ses ID; stays '' if not subdir
        self.datype   = datype                 # anat, func, etc.

        self.list_dafile = []                   # list of datafiles
        self.ndafile     = 0
        self.dict_dafile = {}

        self.nnifti     = 0                    # how many dafiles are nii?

        self.verb     = verb
        # ----------------------------

        self.abspath = make_abspath_from_dirname(dirname)

        # leaf level: populate all dafiles for this datype
        self.set_list_dafile()
       
        dict_ext = {}
        for f in self.list_dafile:
            for ext in TWIG_EXTENSIONS:
                if f.endswith(ext):
                    f = f.removesuffix(ext)
                    if f in dict_ext.keys():
                        dict_ext[f].append(ext)
                    else:
                        dict_ext[f] = [ext]
        self.list_twig = [
            DataTwig(self.dirname, k, dict_ext[k], subj=subj, ses=ses) \
            for k in dict_ext.keys()
        ]

    # ----------------------------------------------------------

    def set_list_dafile(self):
        all_dafile = glob.glob(self.abspath + '/*')  # all files present
        all_dafile.sort()
        ndafile = len(all_dafile)
        if ndafile :
            self.list_dafile = [x.split('/')[-1] for x in all_dafile]
            self.ndafile     = ndafile

            if self.verb :
                print("++ list of {} data files in '{}':\n   {}"
                      "".format( ndafile, self.datype,
                                 '\n   '.join(self.list_dafile) ))
        else:
            print("** WARN: no dafile files in '{}'?".format(self.abspath))


    def add_dafile(self, dafile):
        """Add a dafile items (i.e., sub-001_T1w.nii.gz, sub-001_T1w.json,
        etc.).

        This increases the size of the object's dictionary of
        BIDS data_files, self.dict_dafile.

        Parameters
        ----------
        dafile : str
              name of datafile

        """
        dafiledir = self.dirname #+ '/' + dafile
        self.dict_dafile[dafile] = bids_dafile( dafiledir,
                                                subj   = self.subj, 
                                                ses    = self.ses,
                                                datype = self.datype,
                                                dafile = dafile,
                                                verb   = self.verb )

    def get_num_nifti(self):
        num = 0
        for dafile in self.list_dafile:
            if dafile.endswith('.nii') or dafile.endswith('.nii.gz'):
                num+= 1
        return num
        
# =========================================================================

class bids_dafile:
    """
    """

    def __init__(self, dirname, subj='', ses='', datype='', dafile='', verb=0):
        dirname       = dirname.rstrip('/')

        self.dirname  = dirname                # directory
        print(self.dirname)
        self.abspath  = ''                     # abs path of dirname

        self.subj     = subj                   # subj ID
        self.ses      = ses                    # ses ID; stays '' if not subdir
        self.datype   = datype                 # anat, func, etc.
        self.dafile   = dafile                 # AAA.nii.gz, BBB.json

        self.dict_archinfo = {}                   # list of datafiles

        self.nnifti     = 0                    # how many dafiles are nii?

        self.verb     = verb
        # ----------------------------

        self.abspath = make_abspath_from_dirname(dirname)

        self.add_archinfo()

    # ----------------------------------------------------------

    def add_archinfo(self):
        """Add a archival info for this dafile.

        """

        self.dict_archinfo = BIDS_GENERATOR.into_attributes(self.dirname + '/' 
                                                            + self.dafile)

class DataTwig:
    """A collection of closely related data files which share the same
prefix."""
    def __init__(self,
        dirname: str,
        prefix: str,
        extensions: list[str],
        subj: str = '',
        ses: str = '',
        datype: str = '',
        verb: int = 0,
    ):
        """Construct a DataTwig

        Parameters
        ----------
        dirname: str
            The relative path from the root of the collection to this
            twig's parent.
        prefix: str
            The common prefix for all files in this twig.
        extensions: list[str]
            The available file extensions for this twig.

        """
        self.dirname = dirname
        self.prefix = prefix
        self.list_ext = extensions
        self.arch_info = BIDS_GENERATOR.into_attributes(
            self.dirname + '/' + self.prefix
        )

    @property
    def leaves(self) -> list[str]:
        return [
            self.dirname + '/' + self.prefix + ext for ext in self.list_ext
        ]

# TODO: break this into its own script
def main():
    parser = argparse.ArgumentParser(
        prog='arco',
        description='Investigate a dataset structure'
    )
    parser.add_argument(
        'dataset',
        help='The dataset to investigate',
    )
    args = parser.parse_args()
    dataset = os.path.relpath(args.dataset)
    bd = bids_dataset(dataset)
    for _, subj in bd.dict_subj.items():
        for _, ses in subj.dict_ses.items():
            for _, datype in ses.dict_datype.items():
                for twig in datype.list_twig:
                    print(
                        f'{twig.prefix}: {twig.arch_info} with leaves'
                        f'{twig.leaves}'
                    )

if __name__ == '__main__':
    main()
