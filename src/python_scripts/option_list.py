#!/usr/bin/env python

# all about options (so may merge with afni_base)

# do we want all of the 

import sys
import afni_base

# hisory:
#   
#   07 May 2008 [rickr]:
#     - added doc string and reformatted add_opt()
#     - modified show()
#     - added class functions get_string_opt, get_string_list,
#       get_type_opt and get_type_list  
#
#   06 June 2008 [rickr]:
#     - get_*_opt functions now return an error code along with the result
#

# ---------------------------------------------------------------------------
# This class provides functionality for processing lists of comopt elements.
class OptionList:
    def __init__(self, label):
        self.label    = label
        self.olist    = []      # list of comopt elements
        self.trailers = 0       # for  read_options: no trailing args allowed
                                # from read_options: say there were such args

    def add_opt(self, name, npar, deflist, acplist=[], req=0, setpar=0,  \
                helpstr = ""):
        """add an option to the current OptionList

                name    : option name, to be provided on command line
                npar    : number of parameters
                              > 0 --> require exactly that number
                              < 0 --> require at least the positive number
                deflist : default parmeter list (required, for now)
                acplist : list of acceptable values
                req     : flag: is this required?
                setpar  : flag: set option parlist from deflist
        """
        
        com = afni_base.comopt(name, npar, deflist, acplist, helpstr)
        com.required = req
        if setpar: com.parlist = com.deflist
        self.olist.append(com)

    def show(self, mesg = '', verb = 0):
        if verb: print "%sOptionList: %s (len %d)" % \
                       (mesg, self.label, len(self.olist))
        for index in range(len(self.olist)):
            # possibly add short help string
            if verb and self.olist[index].helpstr :
                hs = ": %s" % self.olist[index].helpstr
            else :
                hs = ''
            print "%sopt %02d: %-20s%s" % \
                (mesg, index, self.olist[index].name, hs)

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

    def del_opt(self, name, nth=1):     # delete nth occurance of option label
        """delete nth comopt where name=name, else None"""
        count = 0
        for index in range(len(self.olist)):
            if self.olist[index].name == name:
                count += 1
                if count == nth:
                    del self.olist[index]
                    return 1

    def get_string_opt(self, opt_name):
        """return the option parameter string and err
           err = 0 on success, 1 on failure"""

        opt = self.find_opt(opt_name)
        if not opt or not opt.parlist: return None, 0
        if len(opt.parlist) != 1:
            print '** option %s takes exactly 1 parameter, have: %s' % \
                  (opt_name, opt.parlist)
            return None, 1
        return opt.parlist[0], 0

    def get_string_list(self, opt_name):
        """return the option parameter string and an error code"""

        opt = self.find_opt(opt_name)
        if not opt or not opt.parlist or len(opt.parlist) < 1: return None,0
        return opt.parlist, 0

    def get_type_opt(self, type, opt_name):
        """return the option param value converted to the given type, and err
           (err = 0 on success, 1 on failure)"""

        opt = self.find_opt(opt_name)
        if not opt or not opt.parlist: return None, 0
        if len(opt.parlist) != 1:
            print '** option %s takes exactly 1 parameter, have: %s' % \
                  (opt_name, opt.parlist)
            return None, 1
        try: val = type(opt.parlist[0])
        except:
            print "** cannot convert '%s' to %s" % (opt.parlist[0], type)
            return None, 1

        return val, 0

    def get_type_list(self, type, opt_name, length, len_name, verb=1):
        """return a list of values of the given type, and err

            err will be set (1) if there is an error

            type      : expected conversion type
            opt_name  : option name to find in opts list
            length    : expected length of option parameters (or 1)
            len_name  : name of option that would define expected length
            verb      : verbose level

            Find opt_name in opts list.  Verify that the parlist values are of
            the proper type and that there are either 1 or 'length' of them.
            If 1, duplicate it to length."""

        opt = self.find_opt(opt_name)
        if not opt or not opt.parlist: return None, 0
        olen = len(opt.parlist)
        if olen != 1 and olen != length:
            print '** %s takes 1 or %s (%d) values, have %d: %s' % \
                  (opt_name, len_name, length, olen, ', '.join(opt.parlist))
            return 1, 1
        try:
            tlist = map(type,opt.parlist)
        except:
            print "** %s takes only %s, have: %s" % (opt_name,type,opt.parlist)
            return None, 1
        if olen != length:     # expand the list
            tlist = [tlist[0] for i in range(length)]
            if verb > 1: print '++ expanding %s to list %s' % (opt_name, tlist)
        elif verb > 1: print '-- have %s list %s' % (opt_name, tlist)

        return tlist, 0        # return the list


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
            if verb > 2: print "+d found option '%s'" % com.name
            if verb > 3: print "-d remaining args: %s" % argv[ac:-1]

            # create new return option
            newopt = afni_base.comopt(com.name, com.n_exp, com.deflist)
            newopt.i_name = ac          # current index into argv
            newopt.acceptlist = com.acceptlist
            newopt.required = com.required 
            ac += 1                     # now point to next argument

            # create parlist of potential parameters
            if newopt.n_exp > 0:    # try to insert that number of args
                if newopt.n_exp <= alen - ac:
                    if verb > 2: print "+d adding %d params" % newopt.n_exp
                    parlist = argv[ac:ac+newopt.n_exp]
                else:   # too few args
                    print "** error: arg #%d (%s) requires %d params" % \
                          (ac-1, newopt.name, newopt.n_exp)
                    return None
            elif newopt.n_exp < 0:  # grab everything, and truncate later
                if verb > 2: print "+d start with all %d params" % (alen-ac)
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
                    # check against repr(list element), since par is a string
                    # (search slowly for older versions of python)
                    found = 0
                    for accpar in newopt.acceptlist:
                        if par == str(accpar): found = 1
                    if not found:  # panic into error!  aaas yoooou wiiiiish...
                        print "** option %s: param '%s' is not in: %s" % \
                              (newopt.name, par, newopt.acceptlist)
                        return None  # what else can we do?

            # so do we still have enough parameters?
            if newopt.n_exp < 0: nreq = abs(newopt.n_exp)
            else:                nreq = newopt.n_exp
            if len(parlist) < nreq:
                print "** error: arg #%d (%s) requires %d params, found %d" % \
                      (ac-1, newopt.name, nreq, len(parlist))
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
                print "** error: unknown trailing arguments : %s" % argv[ac:]
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
            elif len(co.deflist) > 0:  # use it
                newopt = afni_base.comopt(co.name, len(co.deflist), co.deflist)
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
    okopts.add_opt('-e',     -2, ['21', '24', '265']        )
    okopts.trailers = 1 # allow trailing args

    okopts.show('------ possible input options ------ ')

    found_opts = read_options(sys.argv, okopts)
    
    if found_opts: found_opts.show('------ found options ------ ')

if __name__ == '__main__':
    test_comopts()

