#!/usr/bin/env python

# python3 status: compatible

# -------------------------------------------------------------------------
#
# Work in Progress
# Goals:
# - Ingest a root BIDS directory and be capable of IDing the contents of its
#   leaves.
# - Use this library and a set of afni_proc options to generate a
#   collection-wide set of afni_proc scripts.
#
# TODO: understand what PT means here
# The means for *getting* information are not going to be used
# longterm.  A BIDS expert will know better approaches for that aspect.
#
# -------------------------------------------------------------------------
#
# This basically comprises a set of objects to contain the
# (conceptual) data hierarchy:
# 
# collection
# |----subj (subject: sub-001, sub-002, ...)
#      |----ses (session: ses-01, ses-02, ...; also sometimes just 'ses')
#           |----modality (AKA 'data type': anat, func, ...)
#                |----twig (data file prefixes: sub-102_ses-01_run-01_T1w, ...)
#                     |----ext_list (list of file extensions for twig)
#                          ... and leaf list (full path of each twig file)
#                     |----arch_dict (dict of component pieces of twig)
#
# Here, these are represented as nested dictionaries, with each set of
# keys being the items of the next level down, until one gets to
# twigs, which allow you to generate the filenames of the leaves but do not
# directly store the filenames (this would waste memory since they only differ
# by extensions).
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

INDENT_STR = ' '*4

# [Q] do we want to keep the leading '.' in the ext list?  typically,
# people don't refer to the dot as part of the ext.  We'd have to
# adjust how they were used/counted below, but it might make sense.
POSSIBLE_EXTENSIONS = (
    '.nii.gz',
    '.nii',
    '.json',
    '.HEAD',            # Q: don't need to include BRIK and BRIK.gz?
)

LIB_AP_BIDS_VERBOSITY = 0

# -------------------------------------------------------------------------

def indent_str_by_line(s):
    """
    Parameter
    ---------
    s     : str
            A string s, which can contain 0 or more newline chars.

    Return
    ------
          : str
            A string with INDENT_STR prepended to the start of each 
            'line' in the input str.

    """

    if not s :
        return ''

    all_line = s.split('\n')
    return '\n'.join([INDENT_STR + line for line in all_line])

# -------------------------------------------------------------------------

def make_abspath_from_dirname(dirname):
    '''
    Parameters
    ----------
    dirname : str
              relative or abs path

    str
              abs path name

    '''
    if not os.path.isdir(dirname) :
        # TODO: make this a UserWarning?
        print("** WARN: {} is not a valid dirname".format(dirname))
        return ''
    return os.path.abspath(dirname)

def make_tail_from_dirname(dirname):
    '''
    Parameters
    ----------
    dirname : str
              relative or abs path to a *directory*, specifically

    Return
    ------
    tail    : str
              tail of abs path (i.e., last item in path)

    '''
    if not os.path.isdir(dirname) :
        # TODO: make this a UserWarning?
        print("+* WARN: {} is not a valid dirname".format(dirname))
        return ''

    apath = os.path.abspath(dirname)
    tail  = apath.split('/')[-1]

    if not len(tail):
        # TODO: make this a UserWarning?
        print(
            "+* WARN: cannot find tail from dir: {}\n"
            "   abspath = {}".format(dirname, apath)
        )
        return ''
    return tail

def make_tail_from_filename(fname):
    '''
    Parameters
    ----------
    fname : str
              relative or abs path to a *file*, specifically

    Return
    ------
    tail    : str
              tail of abs path (i.e., last item in path)

    '''
    if not os.path.isfile(fname) :
        # TODO: make this a UserWarning?
        print("+* WARN: {} is not a valid filename".format(fname))
        return ''

    apath = os.path.abspath(fname)
    tail  = apath.split('/')[-1]

    if not len(tail):
        # TODO: make this a UserWarning?
        print(
            "+* WARN: cannot find tail from file: {}\n"
            "   abspath = {}".format(fname, apath)
        )
        return ''
    return tail

# -------------------------------------------------------------------------

class Collection:
    """An object storing the full data collection.

    The primary data structure is subj_dict, a dictionary of subjects
    organized as subj_id:subj_obj pairs.

    """
    def __init__(self, dirname: str, label: str = '', verb: int = 0):
        """A collection of group-level data that follows a well-defined
        rubric, such as BIDS or BIDS-ish.

        Parameters
        ----------
        dirname: str
            The directory (relative or absolute path) that acts as the 
            collection root.
        label: str, optional
            The label to give this dataset. Default ''.
        verb: int, optional
            Verbosity level for chatting while working (def = 0).

        """

        self.dirname = dirname.rstrip('/')    # entered path, no trailing '/'
        self.label   = label                  # dataset/proj label
        self.verb    = verb

        self.abspath         = make_abspath_from_dirname(self.dirname)
        self.collection_name = make_tail_from_dirname(self.dirname)
        self.subj_dict = {}

        # glob for all sub-* directories in self.abspath
        all_dir = [ x for x in glob.glob( self.abspath + '/sub-*' ) \
                    if os.path.isdir(x) ]

        if len(all_dir) :
            # make dictionary of subject IDs
            self.subj_dict = {
                make_tail_from_dirname(s): \
                Subject(s, verb=verb) for s in all_dir
            }
        else:
            print("+* WARNING: found no subj")


    @property
    def subj_list(self) -> list:
        """Return list of subject IDs."""
        lll = list(self.subj_dict.keys())
        lll.sort()
        return lll

    @property
    def n_subj(self) -> int:
        """Return number of subjects."""
        return len(self.subj_dict)

    def __str__(self) -> str:
        """Return a (multiline) string of the data collection contents,
        hierarchically indenting by each level (kinda like tree).
        """
        lines = []
        for subj in self.subj_list :
            subj_obj   = self.subj_dict[subj]
            lines.append(subj_obj.get_subj)
            lines.append(indent_str_by_line(str(subj_obj)))
        return '\n'.join(lines)



class Subject:
    """An object storing one subject's information.

    The primary data structure is ses_dict, a dictionary of sessions
    organized as ses_id:ses_obj pairs.

    NB: A subject always contain at least one session, even if the
    file structure does not contain an actual session directory. We
    refer to this as an "unseparated" session, and it will just bear
    the label 'ses' (rather than 'ses-*').

    """

    def __init__(self, dirname, verb: int = 0):
        """A collection of subject data that follows a well-defined rubric,
        such as BIDS or BIDS-ish.

        Parameters
        ----------
        dirname: str
            The directory (relative or absolute path) that acts as the 
            subject root.
        verb: int, optional
            Verbosity level for chatting while working (def = 0).

        """

        self.dirname  = dirname.rstrip('/')    # entered path, no trailing '/'
        self.verb     = verb

        self.abspath  = make_abspath_from_dirname(self.dirname)
        self.subj     = make_tail_from_dirname(self.dirname)
        self.ses_dict = {}                     # dict of sessions

        # glob for all ses-* directories in self.abspath
        all_dir = [ x for x in glob.glob( self.abspath + '/ses-*' ) \
                    if os.path.isdir(x) ]

        # make dictionary of all session IDs
        if len(all_dir) == 0:
            # Invisible session, make a session from the same dir
            self.ses_dict = {"ses": Session(self.abspath)}
        else:
            self.ses_dict = {
                make_tail_from_dirname(s): \
                Session(s, verb=self.verb) for s in all_dir
            }

    @property
    def get_subj(self) -> str:
        """Return subject ID (in a safe way!)"""
        return self.subj

    @property
    def ses_list(self) -> list:
        """Return list of session IDs."""
        lll = list(self.ses_dict.keys())
        lll.sort()
        return lll

    @property
    def n_ses(self) -> int:
        """Return number of sessions."""
        return len(self.ses_dict)

    def __str__(self) -> str:
        """Return a (multiline) string of the subject collection contents,
        hierarchically indenting by each level (kinda like tree).
        """
        lines = []
        for ses in self.ses_list :
            ses_obj   = self.ses_dict[ses]
            lines.append(ses_obj.get_ses)
            lines.append(indent_str_by_line(str(ses_obj)))
        return '\n'.join(lines)

    
class Session:
    """An object storing one session's modality information.

    The primary data structure is modality_dict, a dictionary of
    modalities organized as modality_id:modality_obj pairs.

    """

    def __init__(self, dirname, verb: int = 0):
        """A collection of session data that follows a well-defined rubric,
        such as BIDS or BIDS-ish.

        Parameters
        ----------
        dirname: str
            The directory (relative or absolute path) that acts as the 
            subject root.
        verb: int, optional
            Verbosity level for chatting while working (def = 0).

        """

        self.dirname  = dirname.rstrip('/')       # directory, no trailing '/'
        self.verb     = verb

        self.abspath  = make_abspath_from_dirname(self.dirname)
        tail_dirname  = make_tail_from_dirname(self.dirname)
        self.ses      = tail_dirname if "ses" in tail_dirname else "ses"
        self.modality_dict = {}

        # glob for all directories in self.abspath
        all_dir = [ x for x in glob.glob( self.abspath + '/*' ) \
                     if os.path.isdir(x) ]

        if len(all_dir) :
            # dictionary of modalities
            self.modality_dict = {
                make_tail_from_dirname(m): \
                Modality(m, verb=self.verb) for m in all_dir
            }
        else:
            print("+* WARNING: found no modalities")


    @property
    def get_ses(self) -> str:
        """Return session ID (in a safe way!)."""
        return self.ses

    @property
    def subject_label(self) -> str:
        return self.modality_dict[list(self.modality_dict.keys())[0]].subject

    @property
    def session_label(self) -> str:
        return self.modality_dict[list(self.modality_dict.keys())[0]].session

    @property
    def modality_list(self) -> list:
        """Return (lexicographically sorted) list of modalities."""
        lll = list(self.modality_dict.keys())
        lll.sort()
        return lll

    @property
    def n_modality(self) -> int:
        """Return number of modalities."""
        return len(self.modality_dict)

    def __str__(self) -> str:
        """Return a (multiline) string of the session collection contents,
        hierarchically indenting by each level (kinda like tree).
        """
        lines = []
        for modality in self.modality_list :
            modality_obj = self.modality_dict[modality]
            lines.append(modality_obj.get_modality)
            lines.append(indent_str_by_line(str(modality_obj)))
        return '\n'.join(lines)

              
class Modality:
    """An object storing one modality's twig information.

    The primary data structure in this object is twig_dict, a dict of
    twigs.

    Nomenclature note: A 'modality is a 'data type' in BIDS parlance,
    such as 'anat', 'func', etc.).

    Rather than more directories, this object contains a dict of
    "twigs", which bundle files that share prefixes.

    """

    def __init__(self, dirname, verb: int = 0):
        """A collection of modality data that follows a well-defined rubric,
        such as BIDS or BIDS-ish.

        Parameters
        ----------
        dirname: str
            The directory (relative or absolute path) that acts as the 
            subject root.
        verb: int, optional
            Verbosity level for chatting while working (def = 0).

        """

        self.dirname  = dirname.rstrip('/')       # directory, no trailing '/'
        self.verb     = verb

        self.abspath   = make_abspath_from_dirname(self.dirname)
        self.modality  = make_tail_from_dirname(self.dirname)
        self.twig_dict = {}

        # glob for all files in self.abspath
        all_fname = [ x for x in glob.glob( self.abspath + '/*' ) \
                      if os.path.isfile(x) ]

        # create name_ext dict
        name_ext = {}
        for fname in all_fname:
            for ext in POSSIBLE_EXTENSIONS:
                if fname.endswith(ext):
                    tail = make_tail_from_filename(fname)
                    # make key value juuust from tail (dooon't use rstrip())
                    pref = tail[:-len(ext)]
                    if pref in name_ext.keys():
                        name_ext[pref].append(ext)
                    else:
                        name_ext[pref] = [ext]

        if len(name_ext) :
            # dictionary of twigs
            self.twig_dict = {
                k: Twig(self.dirname, k, name_ext[k], verb=self.verb)   \
                for k in name_ext.keys() # these keys have no path piece
            }
        else:
            print("+* WARNING: found no twigs (files with possible ext)")


#    @property
#    def subject(self) -> str:
#        return self.twig_list[0].subject
#
#    @property
#    def session(self) -> str:
#        return self.twig_list[0].session

    @property
    def get_modality(self) -> str:
        """Return modality name (in a safe way!)."""
        return self.modality

    @property
    def n_twig(self) -> int:
        """Return number of twigs."""
        return len(self.twig_dict)

    @property
    def twig_list(self) -> list:
        """Return (lexicographically sorted) list of twigs."""
        lll = list(self.twig_dict.keys())
        lll.sort()
        return lll

    def __str__(self) -> str:
        """Return a (multiline) string of the modality collection contents,
        hierarchically indenting by each level (kinda like tree).
        """
        lines = []
        for twig in self.twig_list :
            twig_obj = self.twig_dict[twig]
            lines.append(twig_obj.get_twig)
            lines.append(indent_str_by_line(str(twig_obj)))
        return '\n'.join(lines)


class Twig:
    """An object storing one twig's ext_list, which in combination with
    the twig name yields the leaf_list (all data files sharing the
    same prefix), and arch_dict information.

    The primary data structure in this object is arch_dict, a dict of
    Archivotron's analytic representation of the leaf basename.

    Nomenclature note: a 'twig' is the prefix shared by one or more
    files of interest (like the "X" in "X.nii.gz and X.json").  A
    'leaf' is a collection of closely related data files which share
    the same prefix.  Knowing the twig and ext_list allows you to
    reconstruct the leaf_list.

    """

    def __init__(self,
                 dirname    : str,
                 twig       : str,
                 ext_list   : list,
                 verb       : int = 0,
    ):
        """A collection of "twig" data that follows a well-defined rubric,
        such as BIDS or BIDS-ish.

        Parameters
        ----------
        dirname: str
            The directory (relative or absolute path) that acts as the 
            subject root.
        twig: str
            The common prefix for all files in this twig.
        ext_list: list
            The available file extensions for this twig.
        verb: int, optional
            Verbosity level for chatting while working (def = 0).

        """

        self.dirname   = dirname.rstrip('/')
        self.twig      = twig
        self.ext_list  = ext_list
        self.verb      = verb

        self.arch_dict = {}

        self.arch_dict = BIDS_GENERATOR.into_attributes(
            self.dirname + '/' + self.twig
        )

        if len(self.arch_dict) == 0 :
            print("+* WARNING: empty arch_dict")


    @property
    def get_twig(self) -> str:
        """Return twig name (in a safe way!)."""
        return self.twig

    @property
    def leaf_list(self) -> list:
        """Return list of leaves."""
        return [
            self.dirname + '/' + self.twig + ext for ext in self.ext_list
        ]

    @property
    def n_leaf(self) -> int:
        """Return number of leaves."""
        return len(self.ext_list) # should be same len, and faster to get

    @property
    def subj(self) -> str:
        return 'sub-' + self.arch_dict['sub']

    @property
    def ses(self) -> str:
        if "ses" in self.arch_dict.keys():
            return 'ses-' + self.arch_dict['ses']
        else:
            return "ses"

    @property
    def modality(self) -> str:
        return self.arch_dict['modality']

    def __str__(self) -> str:
        """Return a (single line, comma-separated) string of the list of file
        extensions for this twig.
        """
        return ', '.join(self.ext_list)


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
    args    = parser.parse_args()
    dataset = os.path.relpath(args.dataset)
    bd      = Collection(dataset)
    print(bd)

if __name__ == '__main__':
    main()
