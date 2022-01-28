#!/usr/bin/env python

# This library contains functions for creating part of the
# afni_proc.py QC HTML.  Specifically, these functions focus primarily
# on the 'vstat' QC block, parsing the stats dset for task-based cases
# and deciding which volumes should be included in the QC HTML.
#
auth = 'PA Taylor'
#
#########################################################################
#
ver = '1.0' ; date = 'Jan 19, 2022'
#
#
#
#
#
#########################################################################

import os, copy, sys

from afnipy import afni_base           as ab

# ----------------------------------------------------------------------

# for parsing stats_dset labels and setting up vstat testing
class vstat_obj:

    def __init__(self) :

        self.label      = ""             # what user enters
        self.stats_dset = ""             # dset searched within
        self.label_in_dset = False       # does the label exist meaningfully?

        self.olay_label = ""             # subbrick label/selector of stats_dset
        self.olay_index = -1             # subbrick index selector of stats_dset
        self.olay_type  = ""             # 'Coef', 'Tstat', 'Fstat'
        self.olay_posonly = False        # For colorbar
        self.olay_pbar  = ""             # Name of pbar, decided by posonly

        self.thr_label  = ""             # subbrick label/selector of stats_dset
        self.thr_index  = ""             # subbrick index selector of stats_dset
        self.thr_type   = ""             # 'Tstat', 'Fstat'
        self.thr_sided  = "2sided"       # '1sided', '2sided', 'bisided'
        self.thr_sided_txt = "two-sided" # 'one-sided', 'two-sided', 'bisided'
        self.thr_mode   = "pvalue"       # 'percentile' or 'pvalue'

    def set_label(self, ss):
        self.label = ss

    def set_stats_dset(self, ss):
        self.stats_dset = ss

    def found_label(self):
        self.label_in_dset = True
        
    def set_olay_label(self, ss):
        self.olay_label = ss

    def set_olay_index(self, ii):
        self.olay_index = int(ii)

    def set_olay_type(self, ss):
        self.olay_type = ss

    def set_olay_posonly(self):
        self.olay_posonly = True

    def set_olay_pbar(self, ss):
        self.olay_pbar = ss

    def set_thr_label(self, ss):
        self.thr_label = ss

    def set_thr_index(self, ii):
        self.thr_index = int(ii)

    def set_thr_type(self, ss):
        self.thr_type = ss

    def set_thr_sided(self, ss):
        self.thr_sided = ss
        if ss == '1sided' :
            self.thr_sided_txt = 'one-sided'
        elif ss == '2sided' :
            self.thr_sided_txt = 'two-sided'
        elif ss == 'bisided' :
            self.thr_sided_txt = 'bi-sided'
        else :
            print("*+ WARNING: unknown sidedness of testing: {}"
                  "".format(ss))

    def set_thr_mode(self, ss):
        self.thr_mode = ss

    ### semi-omnibus settings for olay/thr
    ##  olay can be Coef or Fstat
    #   thr can be Tstat or Fstat
    def set_olay_all_coef(self, label, ind):
        self.found_label()
        self.set_olay_label(label)
        self.set_olay_index(ind)
        self.set_olay_type('Coef')
        self.set_olay_pbar('Reds_and_Blues_Inv')

    def set_olay_all_fstat(self, label, ind):
        self.found_label()
        self.set_olay_label(label)
        self.set_olay_index(ind)
        self.set_olay_type('Fstat')
        self.set_olay_pbar('Plasma')
        self.set_olay_posonly()

    def set_thr_all_tstat(self, label, ind):
        self.set_thr_label(label)
        self.set_thr_index(ind)
        self.set_thr_type('Tstat')
        self.set_thr_sided("2sided")

    def set_thr_all_fstat(self, label, ind):
        self.set_thr_label(label)
        self.set_thr_index(ind)
        self.set_thr_type('Fstat')
        self.set_thr_sided("1sided")
        if self.thr_label == 'Full_Fstat' :
            self.set_thr_mode("percentile")

    def disp_olay_all(self):
        print("label         : " + self.label)
        print("stats_dset    : " + self.stats_dset)
        print("label_in_dset : " + str(self.label_in_dset))

        print("olay_label    : " + self.olay_label)
        print("olay_index    : " + str(self.olay_index))
        print("olay_type     : " + self.olay_type)
        print("olay_posonly  : " + str(self.olay_posonly))
        print("olay_pbar     : " + self.olay_pbar)
                     
    def disp_thr_all(self):
        print("label         : " + self.label)
        print("stats_dset    : " + self.stats_dset)
        print("label_in_dset : " + str(self.label_in_dset))

        print("thr_label     : " + self.thr_label)
        print("thr_index     : " + str(self.thr_index))
        print("thr_type      : " + self.thr_type )
        print("thr_sided     : " + self.thr_sided)
        print("thr_sided_txt : " + self.thr_sided_txt)
        print("thr_mode      : " + self.thr_mode )

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# helper function(s) for reading dset header 

def get_dset_label_list(fname, sb_delim='|'):
    '''Get list of labels from dset header.

    Parameters
    ----------
    fname       : (str) filename of dset
    sb_delim    : (str) delimiter to use to ask for labels in dset
                  (should *not* be a char in any labels themselves);
                  cannot have both ' and " in it (why would you,
                  anyways?)

    Return
    ------
    olist       : (list) list of labels (each a str)

    '''

    if  not sb_delim.__contains__('"') :
        sb_delimq = '"{}"'.format(sb_delim)
    elif  not sb_delim.__contains__("'") :
        sb_delimq = "'{}'".format(sb_delim)
    else:
        print('''** ERROR: Can't have both ' and " in the sb_delim''')
        sys.exit(6)

    cmd_get_label = '''3dinfo -sb_delim {} -label {}'''.format(sb_delimq, 
                                                               fname)

    sint, so, se = ab.simple_shell_exec(cmd_get_label, capture=1)
    sos          = so.split(sb_delim)
    llabels      = [x.strip() for x in sos]
    Nlabels      = len(llabels)

    cmd_get_nv   = '''3dinfo -nv {}'''.format(fname)

    sint, so, se = ab.simple_shell_exec(cmd_get_nv, capture=1)
    Nvols        = int(so.strip())

    if Nlabels != Nvols :
        print("** ERROR: Nlabels ({}) does not match Nvols ({})\n"
              "   in stats file {}.\n"
              "   Possible sb_delim char ({}) in str labels?\n"
              "".format( Nlabels, Nvols, fname, sb_delim ))
        sys.exit(5)

    return llabels

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# series of helper functions for checking if a particular entry in
# stats dset list of labels is of a certain category

def is_label_Full_Fstat(idx, list_label):
    '''Check a label in a stats dset for being of a certain category.

    Here, check if it is Full_Fstat (admittedly pretty easy).
    
    Parameters
    ----------
    idx          : (int) index within the list of labels
    list_label   : (list) list of stats dset labels

    Return
    ------
    yn           : (int) 1 if label is desired stat, 0 otherwise
    
    '''

    nlabel = len(list_label)
    if idx >= nlabel :
        return 0

    if list_label[idx] == "Full_Fstat" :
        return 1
    
    return 0

def is_label_some_Coef_with_Tstat(idx, list_label):
    '''Check a label in a stats dset for being of a certain category.

    Here, check if it is any Coef, followed by a Tstat.  *This*
    function is likely just a supplementary check, because we will
    also care about whether it is a GLT item, a stim coef, or one of a
    train of TENT coefs.  This function does NOT check for an
    associated Fstat (at present, he writes ominously...).
    
    Parameters
    ----------
    idx          : (int) index within the list of labels
    list_label   : (list) list of stats dset labels

    Return
    ------
    yn           : (int) 1 if label is desired stat, 0 otherwise

    '''

    nlabel = len(list_label)
    if idx >= nlabel :
        return 0

    # convert pythonic neg index to pos, as python would/does
    if idx < 0 :
        idx = nlabel + idx
    ilab = list_label[idx]

    if '#' in ilab and ilab.endswith("_Coef"):
        jdx = idx+1
        if jdx < nlabel:
            jlab = list_label[jdx]
            if '#' in jlab and jlab.endswith("_Tstat"):
                if ilab[:-4] == jlab[:-5] :
                    return 1
    return 0


def is_label_SERIES_Coef_with_Tstat(idx, list_label):
    '''Check a label in a stats dset for being of a certain category.

    Here, check if the current label is a Coef, followed by a Tstat,
    that is part of a series of related Coef+Tstat pairs---this can
    happen when using TENT or when GLT includes '|', for example.
    **Because these are special**, also return the index of the Fstat
    at the end of the train
    
    Parameters
    ----------
    idx          : (int) index within the list of labels
    list_label   : (list) list of stats dset labels

    Return
    ------
    yn           : (int) 1 if label is desired stat, 0 otherwise
    fidx         : (int) index of Fstat at end of train; if can't be found,
                   is -1

    '''

    nlabel = len(list_label)
    if idx >= nlabel :
        return 0, -1

    # first check: is coef+tstat pair?
    if not(is_label_some_Coef_with_Tstat(idx, list_label)) :
        return 0, -1

    # convert pythonic neg index to pos, as python would/does
    if idx < 0 :
        idx = nlabel + idx
    ilab = list_label[idx]

    # now check forward and backward for dsets with same prefix
    FOUND_MORE = 0
    pre = ilab.split("#")[0]

    # forward pass
    kdx = idx+2
    if kdx < nlabel:
        klab = list_label[kdx]
        if klab.endswith("_Coef") and klab.split("#")[0] == pre :
            FOUND_MORE = 1

    # backward pass, if necessary
    if not(FOUND_MORE) :
        kdx = idx-2
        if kdx >= 0:
            klab = list_label[kdx]
            if klab.endswith("_Coef") and klab.split("#")[0] == pre :
                FOUND_MORE = 1

    if not(FOUND_MORE) :
        return 0, -1

    fidx  = idx + 2
    name = pre + "_Fstat"
    while fidx < nlabel:
        if list_label[fidx] == name :
            break
        fidx+=1 

    # must have found its Fstat
    if fidx < nlabel :
        return 1, fidx
    else:
        # should not happen
        print("+* WARN: looks like part of TENT train, but cannot find "
              "Fstat in label list?")
        return 1, -1

    return 0, -1

def is_label_Fstat_and_lone(idx, list_label):
    '''Check a label in a stats dset for being of a certain category.

    Here, check if the current label is an Fstat, and also if it is
    'lone' (= has no partner Coef).  The Full_Fstat would be a lone
    Fstat, for example.
    
    Parameters
    ----------
    idx          : (int) index within the list of labels
    list_label   : (list) list of stats dset labels

    Return
    ------
    yn           : (int) 1 if label at idx is an Fstat, 0 otherwise.
    is_lone      : (int) 1 if Fstat is 'lone', 0 otherwise.

    '''

    nlabel = len(list_label)
    if idx >= nlabel :
        return 0, 0

    # convert pythonic neg index to pos, as python would/does
    if idx < 0 :
        idx = nlabel + idx
    ilab = list_label[idx]

    if not(ilab.endswith('_Fstat')) :
        return 0, 0

    # at this point, we assume this idx DOES have an Fstat

    # search backwards for Coef
    ipre  = ilab[:-6]
    cidx  = idx-1
    while cidx > 0:
        clab = list_label[cidx]
        cpre = clab.split("#")[0]
        if cpre == ipre and clab.endswith("_Coef") :
            return 1, 0
        cidx-= 1

    return 1, 1


def is_label_REG_Coef_with_Tstat(idx, list_label):
    '''Check a label in a stats dset for being of a certain category.

    Here, check if the current label is a 'regular' Coef, followed by
    a Tstat (= not part of a 'series' of Coef/Tstat pairs, like for
    TENT or '|'-GLT).  Also see if there is an associated Fstat.
    
    Parameters
    ----------
    idx          : (int) index within the list of labels
    list_label   : (list) list of stats dset labels

    Return
    ------
    yn           : (int) 1 if label at idx is a Coef and at idx+1 is an
                   associated Tstat
    fidx         : (int) idx of associated Fstat, -1 otherwise

    '''

    nlabel = len(list_label)
    if idx >= nlabel :
        return 0, -1

    # first check: is coef+tstat pair?
    if not(is_label_some_Coef_with_Tstat(idx, list_label)) :
        return 0, -1

    # is it a SERIES?
    yesno, tmp = is_label_SERIES_Coef_with_Tstat(idx, list_label)
    if yesno :
        return 0, -1

    # If we make it here, then by process of elimination, item idx
    # must be 'regular' Coef+Tstat

    # check for associated F
    fidx = get_Fstat_index_for_this_Coef(idx, list_label)

    return 1, fidx

def get_Fstat_index_for_this_Coef(idx, list_label):
    '''Check a label in a stats dset for being of a certain category.

    Here, we *assume* that we are already sitting at a Coef index, and want
    to know if there is an associated Fstat with it. If so, return
    that index, which will be in range [0, nlabel); otherwise, return
    -1.
    
    Parameters
    ----------
    idx          : (int) index within the list of labels
    list_label   : (list) list of stats dset labels

    Return
    ------
    fidx         : (int) idx of associated Fstat, -1 otherwise

    '''

    nlabel = len(list_label)

    fidx = -1

    kdx  = idx+2
    pre  = list_label[idx].split("#")[0]
    name = pre + "_Fstat"

    if kdx < nlabel:
        if list_label[kdx] == name :
            fidx = kdx

    return fidx

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# more primary functions

# a primary function (probably wrapped with stats_dset fname as input,
# though)
def get_vstat_stats_default(list_label, nmax_selfchosen=5, verb=0):
    '''From a list of labels in a stats dset, determine a list of them to
include for APQC vstat visualization, up to some max.

The output 


    Parameters
    ----------
    list_label   : (list) list of full labels from stats dset header
    verb         : (int) verbosity level

    Return
    ------
    nvso_final     : (int) number of vstat objects (vsos) in the output list
                     -> essentially how many vstat images we will have
    list_vso_final : (list) list of vstat objects (vsos) to use
    good_labs      : (list) list of good labels---superset (which may match)
                     of labels within vso list (if you want to see list of 
                     labels used, loop over list_vso_final)
                     -> these contained vsos become the main output to use
    bad_labs       : (list) list of bad labels that caused trouble in 
                     searching through the stats header
 
    '''
    
    nlabel = len(list_label)

    # label lists
    good_labs = []
    bad_labs  = []

    # vso lists
    list_vso_init       = []
    list_vso_final      = []
    mini_Full_Fstat     = []
    mini_GLT            = []
    mini_nonGLT         = []
    mini_other_Fstat    = []

    idx = 0
    while idx < nlabel :
        ilab = list_label[idx]
        ipre = ilab.split("_GLT")[0].split("#")[0].split("_Fstat")[0]

        vso = vstat_obj()
        vso.set_label(ipre)

        # these conditions are in decreasing order of priority for
        # display (like cases...)
        if is_label_Full_Fstat(idx, list_label):
            # Full_Fstat
            vso.set_olay_all_fstat(ilab, idx)
            vso.set_thr_all_fstat(ilab, idx)
            good_labs.append(ilab)
            mini_Full_Fstat.append([idx, vso])

        elif '_GLT#' in ilab :
            if is_label_REG_Coef_with_Tstat(idx, list_label)[0] :
                # here, is single pair, maybe with ftstat
                vso.set_olay_all_coef(ilab, idx)
                vso.set_thr_all_tstat(list_label[idx+1], idx+1)
                good_labs.append(ilab)
                mini_GLT.append([idx, vso])
                # check for fstat; jump index there, if it exists
                TF, fidx = is_label_REG_Coef_with_Tstat(idx, list_label)
                if fidx >= 0 :
                    idx = fidx
                else:
                    idx+=1   # bc we use both Coef and Tstat here

            elif is_label_SERIES_Coef_with_Tstat(idx, list_label)[0] :
                # here, is series of pairs, with probable fstat
                yesno, fidx = is_label_SERIES_Coef_with_Tstat(idx, list_label)
                if fidx >= 0 :
                    flab = list_label[fidx]
                    vso.set_olay_all_fstat(flab, fidx)
                    vso.set_thr_all_fstat(flab, fidx)
                    good_labs.append(flab)
                    mini_GLT.append([idx, vso])
                    # jump index
                    idx = fidx
                else:
                    bad_labs.append(ilab)

            else:
                print("+* WARN: GLT label with unmatched properties?\n"
                      "   Observed for: {}"
                          "".format(ilab))
                bad_labs.append(ilab)

        elif is_label_some_Coef_with_Tstat(idx, list_label) :
            if is_label_REG_Coef_with_Tstat(idx, list_label)[0] :
                # here, is single pair, maybe with ftstat
                vso.set_olay_all_coef(ilab, idx)
                vso.set_thr_all_tstat(list_label[idx+1], idx+1)
                good_labs.append(ilab)
                mini_nonGLT.append([idx, vso])
                # check for fstat; jump index there, if it exists
                TF, fidx = is_label_REG_Coef_with_Tstat(idx, list_label)
                if fidx >= 0 :
                    idx = fidx
                else:
                    idx+=1   # bc we use both Coef and Tstat here

            elif is_label_SERIES_Coef_with_Tstat(idx, list_label)[0] :
                # here, is series of pairs, with probable fstat
                yesno, fidx = is_label_SERIES_Coef_with_Tstat(idx, list_label)
                if fidx >= 0 :
                    flab = list_label[fidx]
                    vso.set_olay_all_fstat(flab, fidx)
                    vso.set_thr_all_fstat(flab, fidx)
                    good_labs.append(flab)
                    mini_nonGLT.append([idx, vso])
                    # jump index
                    idx = fidx
                else:
                    bad_labs.append(list_label[idx])

            else:
                print("+* WARN: nonGLT label with unmatched properties?\n"
                      "   Observed for: {}"
                          "".format(ilab))
                bad_labs.append(ilab)

        elif is_label_Fstat_and_lone(idx, list_label)[0] :
            vso.set_olay_all_fstat(ilab, idx)
            vso.set_thr_all_fstat(ilab, idx)
            good_labs.append(ilab)
            mini_other_Fstat.append([idx, vso])

        else:
            print("+* WARN: found no home for: {}"
                  "".format(ilab))
            bad_labs.append(ilab)

        idx+= 1

    if mini_Full_Fstat :
        if verb > 1 :
            print("++ Num Full_Fstat: {}".format(len(mini_Full_Fstat)))
        list_vso_init.extend(mini_Full_Fstat)
    if mini_GLT :
        if verb > 1 :
            print("++ Num GLT: {}".format(len(mini_GLT)))
        list_vso_init.extend(mini_GLT)
    if mini_nonGLT :
        if verb > 1 :
            print("++ Num nonGLT: {}".format(len(mini_nonGLT)))
        list_vso_init.extend(mini_nonGLT)
    if mini_other_Fstat :
        if verb > 1 :
            print("++ Num other_Fstat: {}".format(len(mini_other_Fstat)))
        list_vso_init.extend(mini_other_Fstat)

    # cut down number of stats, perhaps
    nvso_init = len(list_vso_init)
    if nvso_init > nmax_selfchosen:
        list_vso_init = list_vso_init[:nmax_selfchosen]
        if verb :
            print("++ Trimming number of stats dsets to view from {} to {}"
                  "".format(nvso_init, nmax_selfchosen))

    # preserve the relative order of stats within the stats dset
    list_vso_init.sort()
    for ll in list_vso_init :
        list_vso_final.append(ll[1])

    nvso_final = len(list_vso_final)

    # NB: len(good_labs) may not be eq to nvso_final
    return nvso_final, list_vso_final, good_labs, bad_labs

# ----------------------------------------------------------------------

# This is kind of an annoyingly long function, but kind of has the
# equivalent of 'case ..' in C.  With a lot of subcases.  
def check_plabs_in_label_list(list_plab, list_label, fname='', verb=0):
    '''Label prefixes (=prelabs) are the start of labels attached with
datasets.  This function finds which full labels are associated with
pre-labels (e.g., stimulus names).


    Parameters
    ----------
    list_plab    : (list) list of prelabs to be found homes within the  
                   list of labels
    list_label   : (list) list of full labels from stats dset header
    fname        : (str) filename from which info is coming---just to store
                   in any vso objects, for info
    verb         : (int) verbosity level

    Return
    ------
    nbad         : (int) number of bad plabs (i.e., unfound ones), so that
                   having a zero output means all matched within list_label
    bad_plabs    : (list) list of prelabs that do NOT appear within list_label
    good_plabs   : (list) list of prelabs that DO appear within list_label
    list_vso     : (list) list of vstat objects to use (one per 'good' prelab)
                   -> these become the main output to use

    '''
    
    ### below note:
    #    plab = the prefix part of a label (not: #, _GLT, _Coef,
    #           _Tstat, _Fstat)
    #    flab = full label, like the [i] entry in a stats dset header label
    #           list
    #    glab = full label, like flab, but of a different index
    #    vso  = the 'VStat Object' we want to listify for later APQC

    nplab  = len(list_plab)
    nlabel = len(list_label)
    nbad   = 0

    # check they are all present
    bad_plabs  = []
    good_plabs = []
    list_vso   = []

    # loop over each pre-label
    for plab in list_plab :
        # start obj; just is discarded if no match is found
        vso = vstat_obj()
        vso.set_label(plab)

        idx = 0
        while idx < nlabel :
            flab = list_label[idx]            # full label

            if flab == plab:
                # this should probably only happen for Full_Fstat?
                TF, is_lone = is_label_Fstat_and_lone(idx, list_label)
                if TF :
                    # is *some* Fstat
                    vso.set_olay_all_fstat(flab, idx)
                    vso.set_thr_all_fstat(flab, idx)
                    good_plabs.append(plab)
                    list_vso.append(vso)
                elif flab.endswith('_Coef') and \
                     is_label_some_Coef_with_Tstat(idx, list_label) :
                    # is *some* Coef/Tstat pair, perhaps even TENT, oddly
                    # but if user asked for it *exactly*, provide it
                    vso.set_olay_all_coef(flab, idx)
                    vso.set_thr_all_tstat(list_label[idx+1], idx+1)
                    good_plabs.append(plab)
                    list_vso.append(vso)
                else:
                    print("** ERROR: confused at full label == prelab check.\n"
                          "   Don't know what to do with prelabel: {}"
                          "".format(plab))
                    bad_plabs.append(plab)
                break

            elif flab.startswith(plab) and flab.endswith('_Coef') :
                # Now check if it is a good Coef/Tstat pairing
                if not(is_label_some_Coef_with_Tstat(idx, list_label)) :
                    print("** ERROR: confused at coef check.\n"
                          "   Don't know what to do with prelabel: {}"
                          "".format(plab))
                    bad_plabs.append(plab)
                    break

                # check if it is SERIES of Coef/Tstat
                TF, fidx = is_label_SERIES_Coef_with_Tstat(idx, list_label)
                if TF :
                    # since it is a SERIES, jump to the Fstat for this plab
                    vso.set_olay_all_fstat(list_label[fidx], fidx)
                    vso.set_thr_all_fstat(list_label[fidx], fidx)
                    good_plabs.append(plab)
                    list_vso.append(vso)
                    break
                else:
                    # is 'regular' or GLT Coef+Tstat pair: both fine
                    vso.set_olay_all_coef(flab, idx)
                    vso.set_thr_all_tstat(list_label[idx+1], idx+1)
                    good_plabs.append(plab)
                    list_vso.append(vso)
                    break

            elif flab.startswith(plab + '_GLT#') and \
                 flab.endswith('_Fstat') :
                # should not reach here: should be caught earlier
                print("+* WARN: should have caught GLT FStat earlier?\n"
                      "   Observed for: {}"
                          "".format(plab))
                vso.set_olay_all_fstat(flab, idx)
                vso.set_thr_all_fstat(flab, idx)
                good_plabs.append(plab)
                list_vso.append(vso)
                break

            elif flab == plab + '_Fstat' :
                # should be a lone Fstat here, since no plab/'_Coef'
                # combo was found above
                vso.set_olay_all_fstat(flab, idx)
                vso.set_thr_all_fstat(flab, idx)
                good_plabs.append(plab)
                list_vso.append(vso)
                break
            idx+=1

        # if we don't find anything, it must be a bad label
        if idx >=nlabel:
            bad_plabs.append(plab)

    if bad_plabs :
        nbad = len(bad_plabs)
        print("+* WARNING: {} unlocatable user prefix label(s):\n"
              "      {}\n".format(nbad, ', '.join(bad_plabs)))
        if verb :
            print("   ... when searching through these actual labels:\n:"
                  "      {}\n"
                  "".format(', '.join(list_label)))

    return nbad, bad_plabs, good_plabs, list_vso

# ----------------------------------------------------------------------

def parse_stats_dset_labels( fname, 
                             use_Full_Fstat=True, 
                             nmax_selfchosen=5,
                             user_plabs=[],
                             verb=0):
    '''For a given stats_dset 'fname', make a list of subbricks that will
be visualized in the vstat QC block.  Users can specify a list of
label prefixes 'user_plabs' (to which Full will be prepended, by
default, if not included).  NB: These label prefixes don't include the
'_GLT', "#N", 'Tstat', 'Fstat', 'Coef', etc. parts of the full labels.  The
order of specified labels will be reflected in the output (see above
about Full).

If the user does not provide a list, this program has internal logic
to decide what to put into a list (up to nmax_selfchosen items).  The
relative order of chosen items will still follow the stats_dset
ordering.  Priority order:
  + Full_Fstat
  + *_GLT*{Coef,Tstat} pairs
  + remaining (stim) {Coef,Tstat} pairs
... all while avoiding TENT-like cases (pref#0_Coef, pref#1_Coef,
pref#2_Coef, etc.)
    
    Parameters
    ----------
    fname           : (str) name of a stats_dset
    use_Full_Fstat  : (bool) include Full (for Full_Fstat) in list, even if  
                      not specified by user (probably always want this True)
    nmax_selfchosen : (int) if the user has not specified a list of labels,
                      how many items should we choose to output
    user_labpre     : (list) a list of user-specified stim or GLT
                      labels to find within the stats_dset labels.  If 
                      empty, use internal 'default logic' to decide what 
                      to include in the QC block
    verb            : (int) verbosity level

    Return
    ------
    list_objs       : (list) a list of vstat_obj objects that contain all
                      necessary information for the vstat-processing function 
                      here

    '''

    # info from the dset
    dset_labels = get_dset_label_list(fname)
    nlabels     = len(dset_labels)

    list_vso    = []

    ### At the moment, don't go down this route---need to wait for
    ### uvar functionality for user passing this list in; but the
    ### function should be ready for parsing
    if 0 and user_plabs :
        if use_Full_Fstat :
            if not('Full_Fstat' in user_plabs) :
                user_plabs.insert(0, 'Full_Fstat')

        nbad, bad_plabs, good_plabs, list_vso = \
            check_plabs_in_label_list( user_plabs, dset_labels, fname )

    else:
        # 'default' case of using internal logit to decide.
        # also using default number of items to return
        nvso, list_vso, good_labs, bad_labs = \
            get_vstat_stats_default( dset_labels,
                                     nmax_selfchosen=nmax_selfchosen, 
                                     verb=verb )

    print("++ Found {} stats items to use for vstat QC block".format(nvso))

    if verb :
        for i in range(nvso): 
            print("{:4d} : olay = {:<20s} | thr = {}"
                  "".format(i, list_vso[i].olay_label, list_vso[i].thr_label) )

    return list_vso


# ---------------------------------------------------------------------

### ***This is not used anymore; this was only used in special cases
### ***before, previously, too, if someone put in a user_stats uvar.
### ***See the same function name *without* the "_OLD" in it.
def parse_stats_dset_labels_OLD( fname, lsearch = [] ) :
    '''fname is a stats_dset.

    lsearch is a list of user-specified stim or GLT labels (as well as
    the default 'Full_Fstat' label) to find within the stats_dset labels.

    OUTPUT: a list of vstat_obj objects that contain all necessary
    information for the vstat-processing function here.

    '''

    cmd_get_labelstr = '''3dinfo -label {}'''.format(fname)

    sint, so, se = ab.simple_shell_exec(cmd_get_labelstr, capture=1)
    sos          = so.split('|')
    llabels      = [x.strip() for x in sos]
    Nlabels      = len(llabels)

    cmd_get_nv   = '''3dinfo -nv {}'''.format(fname)

    sint, so, se = ab.simple_shell_exec(cmd_get_nv, capture=1)
    Nvols        = int(so.strip())

    if Nlabels != Nvols :
        print("** ERROR: Nlabels ({}) does not match Nvols ({})\n"
              "   in stats file {}.\n"
              "   Possible bad char (e.g., |) in str label?\n"
              "".format( Nlabels, Nvols, fname))
        sys.exit(5)

    list_pre_found = []
    list_objs = []

    for pre in lsearch:
        i = 0
        while i < Nlabels :
            x = llabels[i]
            if x.find(pre)==0 :
                # first one found will be for the olay
                list_pre_found.append(pre)
                vso = vstat_obj()
                vso.set_label(pre)
                vso.set_stats_dset(fname)
                addtoi = 1

                post = x[-5:]
                if post == '_Coef' :
                    vso.set_olay_all_coef(x, i)
                    # and check for accompanying stat
                    if i+1 < Nlabels :
                        j = i+1
                        y = llabels[j]
                        if y.find(pre)==0 :
                            addtoi+=1
                            post = y[-5:]
                            if post == 'Tstat' :
                                vso.set_thr_all_tstat(y, j)
                                if j+1 < Nlabels :
                                    k = j+1
                                    z = llabels[k]
                                    if z.find(pre)==0 :
                                        post = z[-5:]
                                        if post == 'Fstat' :
                                            # all we do is add to
                                            # index-- ignore other steps
                                            addtoi+=1
                            elif post == 'Fstat' :
                                vso.set_thr_all_fstat(y, j)
                            else:
                                print("ERROR: no stat brick to accompany"
                                      "[{}]th volume {} in {}?"
                                      "".format(i, x, fname))
                                sys.exit(7)
                elif post == 'Tstat' :
                    vso.set_thr_all_tstat(x, i)
                    if i+1 < Nlabels :
                        j = i+1
                        y = llabels[j]
                        if y.find(pre)==0 :
                            post = y[-5:]
                            if post == 'Fstat' :
                                # all we do is add to index-- ignore
                                # other steps
                                addtoi+=1
                elif post == 'Fstat' :
                    vso.set_olay_all_fstat(x, i)
                    vso.set_thr_all_fstat(x, i)
                else:
                    print("ERROR: what unexpected volume is this, of unknown"
                          " type?  [{}]th volume {} in {}?"
                          "".format(i, x, fname))
                    sys.exit(7)

                #print("++ Found: {} in: {} and {}  ... addtoi= {}"
                #      "".format(pre, vso.olay_label, vso.thr_label, addtoi))
                list_objs.append(vso) 
                
                i+= addtoi # we accummulate how many to add
            else :
                i+= 1

    for pre in lsearch:
        if not(list_pre_found.__contains__(pre)) :
            print("*+ WARNING: user-asked-for stim or GLT label '{}'\n"
                  "            has not been found in the stats dset: {}"
                  "".format(pre, fname))

    return list_objs







# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

if __name__ == "__main__" :

    # demo examples
    homedir  = os.path.expanduser('~')
    # standard Bootcamp stats dset from s05*
    bootstat = homedir + '/AFNI_data6/FT_analysis/FT.results/'
    bootstat+= 'stats.FT+tlrc.HEAD'

    # from tweaked s05* to run with TENTs
    bootstat2 = homedir + '/AFNI_data6/FT_analysis/FT_TENT.results/'
    bootstat2+= 'stats.FT_TENT+tlrc.HEAD'

    list_stats = [ bootstat, bootstat2 ]

    for stats_dset in list_stats:
        if os.path.exists(stats_dset) :
            print("\n"*2)
            print("++ Test stats stuff for dset:")
            print("   {}".format(stats_dset))

            labs = get_dset_label_list(stats_dset)

            print(80*'=')
            print("++ Check for Full_Fstat")
            print("{:>4s}    {:2s}    {:<20s}"
                  "".format('IDX', 'TF', 'STATS LABEL'))
            print("{:>4s}    {:2s}    {:<20s}"
                  "".format(4*'-', 2*'-', 20*'-'))
            for i in range(len(labs)): 
                yesno = is_label_Full_Fstat(i, labs)
                print("{:4d} -> {:2d} :  {:<20s}"
                      "".format(i, yesno, labs[i]))
            print(80*'=')


            print(80*'=')
            print("++ Check for regular_Coef_with_Tstat")
            print("{:>4s}    {:2s}  {:4s}    {:<20s}"
                  "".format('IDX', 'TF', 'FIDX', 'STATS LABEL'))
            print("{:>4s}    {:2s}  {:4s}    {:<20s}"
                  "".format(4*'-', 2*'-', 4*'-', 20*'-'))
            for i in range(len(labs)): 
                yesno, fidx = is_label_REG_Coef_with_Tstat(i, labs)
                print("{:4d} -> {:2d}, {:4d} :  {:<20s}"
                      "".format(i, yesno, fidx, labs[i]))
            print(80*'=')


            print(80*'=')
            print("++ Check for TENT_Coef_with_Tstat")
            print("{:>4s}    {:2s}  {:4s}    {:<20s}"
                  "".format('IDX', 'TF', 'FIDX', 'STATS LABEL'))
            print("{:>4s}    {:2s}  {:4s}    {:<20s}"
                  "".format(4*'-', 2*'-', 4*'-', 20*'-'))
            for i in range(len(labs)): 
                yesno, fidx = is_label_SERIES_Coef_with_Tstat(i, labs)
                print("{:4d} -> {:2d}, {:4d} :  {:<20s}"
                      "".format(i, yesno, fidx, labs[i]))
            print(80*'=')
        

            print(80*'=')
            print("++ Check for Fstat_and_lone")
            print("{:>4s}    {:2s}  {:4s}    {:<20s}"
                  "".format('IDX', 'TF', 'LONE', 'STATS LABEL'))
            print("{:>4s}    {:2s}  {:4s}    {:<20s}"
                  "".format(4*'-', 2*'-', 4*'-', 20*'-'))
            for i in range(len(labs)): 
                yesno, fidx = is_label_Fstat_and_lone(i, labs)
                print("{:4d} -> {:2d}, {:4d} :  {:<20s}"
                      "".format(i, yesno, fidx, labs[i]))
            print(80*'=')
        
        # test user plabs funcs
        if 1 :
            list_plab = ['vis', 'aud', 'V-A', 'Full_Fstat', 'blah']
            nbad, list_bad, list_good, list_vso = \
                check_plabs_in_label_list(list_plab, labs, stats_dset)

            print(80*'=')
            print("++ user list labels:\n   {}\n".format(list_plab))
            print("++ full label list:\n   {}\n".format(labs))
            print("-> bad label list:\n   {}\n".format(list_bad))
            print("-> good label list (detailed, with olay and thr labels):")
            for i in range(len(list_good)):
                print("{:<15s} : {:<20s} &  {:<20s}"
                      "".format(list_good[i], 
                                list_vso[i].olay_label,
                                list_vso[i].thr_label))

