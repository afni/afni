#!/usr/bin/env python

# all about options (so may merge with afni_base)

# do we want all of the 

import sys, glob, string
import afni_base

# ---------------------------------------------------------------------------
# This class provides functionality for processing lists of comopt elements.
class OptionList:
    def __init__(self, label):
        self.label    = label
        self.olist    = []      # list of comopt elements
        self.trailers = False   # for  read_options: no trailing args allowed
                                # from read_options: say there were such args

    def show(self, mesg = ''):
        print "%sOptionList: %s (len %d)" % \
               (mesg, self.label, len(self.olist))
        for index in range(len(self.olist)):
            str = "opt %d: " % index
            self.olist[index].show(str)

    def find_opt(self, name, nth=1):    # find nth occurance of option label
        """return nth comopt where name=name, else None"""
        index = 0
        for com in self.olist:
            if com.name == name:
                index += 1
                if index == nth: return com
        return None

    def count_opt(self, name):
        """return number of comopts where name=name"""
        count = 0
        for com in self.olist:
            if com.name == name: count += 1
        return count

    def add_opt(self, name, npar, defpar, acplist=[], req=False, setpar=False):
        com = afni_base.comopt(name, npar, defpar, acplist)
        com.required = req
        if setpar: com.parlist = com.deflist
        self.olist.append(com)

    def del_opt(self, name, nth=1):     # delete nth occurance of option label
        """delete nth comopt where name=name, else None"""
        count = 0
        for index in range(len(self.olist)):
            if self.olist[index].name == name:
                count += 1
                if count == nth:
                    del self.olist[index]
                    return 1

# ---------------------------------------------------------------------------
# read_options:
#   given an argument list, and OptionList of acceptable options,
#   return an OptionList of found options, or None on failure
def read_options(argv, oplist, verb = 1):
    """Input an OptionList element, containing a list of options, required
       or not, and return an OptionList of options as they are found.

       return: an OptionList element, or None on a terminal error
       note: options may occur more than once
    """

    OL = OptionList("read_options")

    alen = len(argv)
    if alen == 0: return OL

    # prepare a dictionary counting uses of each user option
    namelist = {}
    for co in oplist.olist:
        if co.name in namelist:   # complain if input list contains repeats
            print "** RO warning: option '%s' appears more than once", co.name
        namelist[co.name] = 0
    if verb > 1 : print "-d namelist: ", namelist

    # parse the input arguments:
    #   for each arg, verify arg is option, then process params
    #   so ac increments by 1+num_params each time
    ac = 1
    while ac < alen:
        com = oplist.find_opt(argv[ac])
        if com:
            namelist[argv[ac]] += 1     # increment dictionary count

            # create new return option
            newopt = afni_base.comopt(com.name, com.n_exp, com.deflist)
            newopt.i_name = ac          # current index into argv
            newopt.acceptlist = com.acceptlist
            newopt.required = com.required 
            ac += 1                     # now point to next argument

            # create parlist of potential parameters
            if newopt.n_exp > 0:    # try to insert that number of args
                if newopt.n_exp <= alen - ac:
                    parlist = argv[ac:ac+newopt.n_exp]
                else:   # too few args
                    print "** error: arg #%d (%s) requires %d params" % \
                          (ac-1, newopt.name, newopt.n_exp)
                    return None
            elif newopt.n_exp < 0:  # grab everything, and truncate later
                parlist = argv[ac:]
            else: parlist = []      # n_exp == 0

            # truncate parlist if it contains an option
            for pc in range(len(parlist)):
                if parlist[pc] in namelist: # then we have pc 'good' params
                    parlist = parlist[:pc]
                    if verb > 1: print "-d truncate %s after %d of %d" % \
                                       (newopt.name, pc, len(parlist))
                    break;

            # now check parlist against acceptlist
            if newopt.acceptlist:
                for par in parlist:
                    # check against repr(list), since par is a string
                    if par not in repr(newopt.acceptlist):  # unacceptable!!
                        print "** option %s, param %s is not in: %s" % \
                              (newopt.name, par, newopt.acceptlist)
                        return None  # what else can we do?

            # so do we still have enough parameters?
            if len(parlist) < newopt.n_exp:
                print "** error: arg #%d (%s) requires %d params, found %d" % \
                      (ac-1, newopt.name, newopt.n_exp, pc)
                return None

            # success!  insert the remaining list
            newopt.parlist = parlist
            newopt.n_found = len(parlist)

        else:   # we seem to be done with expected arguments
            # there should not be any options in this final list
            for arg in argv[ac:]:
                if arg in namelist:
                    print "** error: option %s follows unknown arg #%d (%s)" % \
                          (arg, ac, argv[ac])
                    return None

            if not oplist.trailers :   # then trailers are not allowed
                print "** error: trailing arguements found: %s" % argv[ac:]
                return None

            # insert remaining args as trailers
            newopt = afni_base.comopt('trailers', -1, [])
            newopt.n_found = alen - ac
            newopt.parlist = argv[ac:]

        OL.olist.append(newopt) # insert newopt into our return list
        ac += newopt.n_found    # and increment the argument counter

    # now we have processed all of argv
    # any unused comopt that has a deflist can be used (else error)
            
    for co in oplist.olist:
        if namelist[co.name] == 0:  # may still be okay
            if co.required: 
                print "** error: missing option %s" % co.name
                return None
            elif co.n_exp > 0 and co.n_exp == len(co.deflist):  # use it
                newopt = afni_base.comopt(co.name, co.n_exp, co.deflist)
                newopt.parlist = newopt.deflist
                # leave n_found at -1, so calling function knows
                OL.olist.append(newopt) # insert newopt into our return list

    if verb > 1 :
        print "-d namelist: ", namelist
        print "-d clist: "

    return OL

def test_comopts():

    okopts = OptionList('for_input')
    okopts.add_opt('-a',      1, ['4'       ]               )
    okopts.add_opt('-dsets', -1, [          ]               )
    okopts.add_opt('-debug',  1, ['0'       ],     range(4) )
    okopts.add_opt('-c',      2, ['21', '24']               )
    okopts.add_opt('-d',     -1, [          ]               )
    okopts.trailers = True # allow trailing args

    okopts.show('------ possible input options ------ ')

    found_opts = read_options(sys.argv, okopts)
    
    if found_opts: found_opts.show('------ found options ------ ')

if __name__ == '__main__':
    test_comopts()

