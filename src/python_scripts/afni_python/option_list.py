#!/usr/bin/env python

# python3 status: started

# all about options (so may merge with afni_base)

# do we want all of the 

import sys, os
import afni_base as BASE
import afni_util as UTIL

# whine about execution as a main program
if __name__ == '__main__':
   import sys
   print('** %s: not a main program' % sys.argv[0].split('/')[-1])
   sys.exit(1)

# ---------------------------------------------------------------------------
# history:              see: afni_history -program option_list.py
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
#   06 Nov 2008 [rickr]:
#     - added 'opt' param to get_type_opt and get_type_list
#       (to skip find_)
#
#   01 Dec 2008 [rickr]:
#     - added 'opt' param to get_string_opt and get_string_list
#     - initialized more parameters (to get_*) to make them optional
#
#   03 Oct 2012 [rickr]:
#     - add okdash parameter to option instances, to denote whether any
#       parameters may have dashes
#
#   27 Feb 2013 [rickr]:
#     - added Ziad's apsearch options: -all_opts, -h_find, -h_view
#
#   09 May 2014 [rickr]:
#     - added find_opt_index, which allows for popping
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# This class provides functionality for processing lists of comopt elements.
class OptionList:
    def __init__(self, label):
        self.label    = label
        self.olist    = []      # list of comopt elements
        self.trailers = 0       # for  read_options: no trailing args allowed
                                # from read_options: say there were such args
        self.show_count = 1     # display option count in show()
        self.verb     = 1       # display option count in show()

    def add_opt(self, name, npar, deflist=[], acplist=[], req=0, setpar=0,  \
                helpstr = "", okdash=1):
        """add an option to the current OptionList

                name    : option name, to be provided on command line
                npar    : number of parameters
                              > 0 --> require exactly that number
                              < 0 --> require at least the positive number
                deflist : default parmeter list (required, for now)
                acplist : list of acceptable values
                req     : flag: is this required?
                setpar  : flag: set option parlist from deflist
                okdash  : flag: if set, params are allowed to start with '-'
        """
        
        com = BASE.comopt(name, npar, deflist, acplist, helpstr)
        com.required = req
        com.okdash = okdash
        if setpar: com.parlist = com.deflist
        self.olist.append(com)

    def sort(self):
        """sort command option list by name"""
        # cmp keywork has been removed in python3, use key instead
        # self.olist.sort(cmp=compare_comopts)
        self.olist.sort(key=comopts_key)

    def show(self, mesg = '', verb = 0, show_count=-1):
        if verb or mesg != '': print("%sOptionList: %s (len %d)" % \
                                      (mesg, self.label, len(self.olist)))
        # allow override of class
        if show_count < 0: show_count = self.show_count
        for index in range(len(self.olist)):
            # possibly add short help string
            if verb and self.olist[index].helpstr :
                hs = ": %s" % self.olist[index].helpstr
            elif self.olist[index].n_found > 0 :
                hs = '  args found = %2d' % self.olist[index].n_found
            else :
                hs = ''
            if show_count:
               print("opt %02d: %-24s%s" % (index, self.olist[index].name, hs))
            else: 
               print("    %-24s%s" % (self.olist[index].name, hs))

    def find_opt(self, name, nth=1):    # find nth occurance of option name
        """return nth comopt where name=name, else None"""
        index = 0
        for com in self.olist:
            if com.name == name:
                index += 1
                if index == nth: return com
        return None

    def find_opt_index(self, name, nth=1): # same, but return the index
        """return nth comopt index where name=name, else -1
           same as find_opt, but return index
        """
        index = 0
        cind = 0        # avoid enumerate, since python might be old?
        for com in self.olist:
            if com.name == name:
                index += 1
                if index == nth: return cind
            cind += 1
        return -1

    def find_all_opts(self, name):
        """return all comopts where name=name"""
        olist = []
        for com in self.olist:
            if com.name == name:
                olist.append(com)
        return olist

    def have_yes_opt(self, name, default=0, nth=1):
        """return whether such an option exists and param[0] looks like 'yes'

           default : default value to return if the option does not exist
           nth     : parameter for matching 'find_opt'
        """
        opt = self.find_opt(name, nth=nth)
        if opt == None: return default
        if opt_is_yes(opt): return 1
        return 0

    def have_no_opt(self, name, default=0, nth=1):
        """return whether such an option exists and param[0] looks like 'no'

           default : default value to return if the option does not exist
           nth : parameter for matching 'find_opt'
        """
        opt = self.find_opt(name, nth=nth)
        if opt == None: return default
        if opt_is_no(opt): return 1
        return 0

    def opt_has_arg(self, opt_name=None, opt=None, arg=''):
        """is the given argument in opt.parlist
           (if opt is passed, we don't need to find it)"""

        if opt == None: opt = self.find_opt(opt_name)
        if not opt or not opt.parlist or len(opt.parlist) < 1: return 0
        return arg in opt.parlist

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

    def get_string_opt(self, opt_name=None, opt=None, default=None):
        """return the option parameter string and err
           (if opt is passed, we don't need to find it)
           err = 0 on success, 1 on failure"""

        if opt == None: opt = self.find_opt(opt_name)
        if not opt or not opt.parlist: return default, 0
        if not opt_name: opt_name = opt.name
        if len(opt.parlist) != 1:
            print("** expecting 1 parmeter for option '%s', have: %s" % \
                  (opt_name, opt.parlist))
            return default, 1
        return opt.parlist[0], 0

    def get_joined_strings(self, opt_name=None, opt=None, prefix=''):
        """like get_string_list(), but join any list together and only
           return a string

           only apply 'prefix' if something is found"""
        olist, rv = self.get_string_list(opt_name=opt_name, opt=opt)
        if rv or olist == None: return ''
        if len(olist) < 1:      return ''

        # we have something
        return prefix + ' '.join(UTIL.quotize_list(olist))

    def get_string_list(self, opt_name=None, opt=None):
        """return the option parameter string and an error code
           (if opt is passed, we don't need to find it)"""

        if opt == None: opt = self.find_opt(opt_name)
        if not opt or not opt.parlist or len(opt.parlist) < 1: return None,0
        return opt.parlist, 0

    def get_type_opt(self, otype, opt_name='', opt=None, default=None):
        """return the option param value converted to the given type, and err
           (err = 0 on success, 1 on failure)

           If the opt element is passed, we don't need to find it.
        """

        # if no opt was passed, try to find it
        if opt == None: opt = self.find_opt(opt_name)

        if not opt or not opt.parlist: return default, 0
        if not opt_name: opt_name = opt.name
        if len(opt.parlist) != 1:
            print("** expectin 1 parameter for option '%s', have: %s" % \
                  (opt_name, opt.parlist))
            return default, 1
        try: val = otype(opt.parlist[0])
        except:
            print("** cannot convert '%s' to %s" % (opt.parlist[0], otype))
            return default, 1

        return val, 0

    def get_type_list(self, otype, opt_name='', length=0, len_name='',
                      opt=None, verb=1):
        """return a list of values of the given otype, and err

            err will be set (1) if there is an error

            otype     : expected conversion type
            opt_name  : option name to find in opts list
            length    : expected length of option parameters (or 1)
                        (if length == 0, return whatever is found)
            len_name  : name of option that would define expected length
            opt       : optionally provide a comopt element
            verb      : verbose level

            Find opt_name in opts list.  Verify that the parlist values are of
            the proper otype and that there are either 1 or 'length' of them.
            If 1, duplicate it to length."""

        if opt == None: opt = self.find_opt(opt_name)
        if not opt or not opt.parlist: return None, 0
        if not opt_name: opt_name = opt.name
        olen = len(opt.parlist)
        if length > 0 and olen != 1 and olen != length:
            if verb: 
               print('** %s takes 1 or %s (%d) values, have %d: %s' % \
                  (opt_name, len_name, length, olen, ', '.join(opt.parlist)))
            return None, 1
        try:
            tlist = list(map(otype,opt.parlist))
        except:
            if verb: print("** %s takes only %ss, have: %s"  \
                           % (opt_name,otype,opt.parlist))
            return None, 1
        if length > 0 and olen != length:     # expand the list
            tlist = [tlist[0] for i in range(length)]
            if verb > 1: print('++ expanding %s to list %s' % (opt_name, tlist))
        elif verb > 1: print('-- have %s list %s' % (opt_name, tlist))

        return tlist, 0        # return the list

    def replace_opt(self, opt_name, vals):
        """replace the parlist from the first instace of opt_name with vals
           if not found, add a new option
        """

        opt = self.find_opt(opt_name)
        if not opt:
           setpar = len(vals)
           self.add_opt(opt_name, len(vals), deflist=vals, setpar=setpar)
           return

        # make a copy, to be safe
        if len(vals) == 0: opt.parlist = []
        else:              opt.parlist = vals[:]

        return

    def append_to_opt(self, opt_name, vals):
        """append the vals to parlist from the first instace of opt_name
           if not found, add a new option
        """

        opt = self.find_opt(opt_name)
        if not opt:
           setpar = len(vals)
           self.add_opt(opt_name, len(vals), deflist=vals, setpar=setpar)
           return

        # make a copy, to be safe
        if len(vals) == 0: opt.parlist = vals[:]
        else:              opt.parlist.extend(vals)

        return

    # rcr - improve this garbage
    def check_special_opts(self, argv):
        """process known '-optlist_* options' and other global_opts,
           nuking them from argv

           some options are terminal
        """

        # global options (some take a parameter)
        global_opts = [ '-optlist_verbose', '-optlist_no_show_count',
                        '-all_opts', '-h_find', '-h_view', '-hview' ]

        alen = len(argv)

        if '-optlist_verbose' in argv:
            ind = argv.index('-optlist_verbose')
            self.verb = 4
            argv[ind:ind+1] = []
            print('++ optlist: setting verb to %d' % self.verb)
        if '-optlist_no_show_count' in argv:
            ind = argv.index('-optlist_no_show_count')
            self.show_count = 0
            argv[ind:ind+1] = []
            if self.verb>1: print('++ optlist: clearing show_count')

        # terminal options (all end in exit)
        if '-all_opts' in argv:
            oname = '-all_opts'
            ind = argv.index(oname)
            prog = os.path.basename(argv[0])
            self.show(verb=1)
            sys.exit(0)

        if '-h_find' in argv:
            oname = '-h_find'
            ind = argv.index(oname)
            prog = os.path.basename(argv[0])
            if ind == alen-1:
               print('** global opt %s needs %s option as parameter' \
                     % (oname, prog))
               sys.exit(1)
            cmd = 'apsearch -phelp %s -word %s' % (prog, argv[ind+1])
            if self.verb>1: print('++ optlist: applying %s via: %s'%(oname,cmd))
            BASE.simple_shell_exec(cmd)
            sys.exit(0)

        if '-h_view' in argv:
            oname = '-h_view'
            ind = argv.index(oname)
            prog = os.path.basename(argv[0])
            cmd = 'apsearch -view_prog_help %s' % prog
            if self.verb>1: print('++ optlist: applying %s via: %s'%(oname,cmd))
            BASE.simple_shell_exec(cmd)
            sys.exit(0)

        if '-hview' in argv:
            oname = '-hview'
            ind = argv.index(oname)
            prog = os.path.basename(argv[0])
            cmd = 'apsearch -view_prog_help %s' % prog
            if self.verb>1: print('++ optlist: applying %s via: %s'%(oname,cmd))
            BASE.simple_shell_exec(cmd)
            sys.exit(0)

        if self.verb > 1:
            print('-- argv: orig len %d, new len %d' % (alen,len(argv)))


# ---------------------------------------------------------------------------
# read_options:
#   given an argument list, and OptionList of acceptable options,
#   return an OptionList of found options, or None on failure
def read_options(argv, oplist, verb = -1):
    """Input an OptionList element, containing a list of options, required
       or not, and return an OptionList of options as they are found.

       If verb is not passed, apply that of oplist.

       return: an OptionList element, or None on a terminal error
       note: options may occur more than once
    """

    OL = OptionList("read_options")

    if verb < 0: verb = oplist.verb

    alen = len(argv)
    if alen == 0: return OL

    # prepare a dictionary counting uses of each user option
    namelist = {}
    for co in oplist.olist:
        if co.name in namelist:   # complain if input list contains repeats
            print("** RO warning: option '%s' appears more than once"%co.name)
        namelist[co.name] = 0
    if verb > 1 : print("-d namelist: ", namelist)

    # parse the input arguments:
    #   for each arg, verify arg is option, then process params
    #   so ac increments by 1+num_params each time
    ac = 1
    while ac < alen:
        # -optlist_* : global options to be ignored
        if argv[ac] in [ '-optlist_verbose', '-optlist_no_show_count' ]:
            if oplist.verb > 1: print("-- found optlist opt '%s'" % argv[ac])
            ac += 1
            continue

        com = oplist.find_opt(argv[ac])
        if com:
            namelist[argv[ac]] += 1     # increment dictionary count
            if verb > 2: print("+d found option '%s'" % com.name)
            if verb > 3: print("-d remaining args: %s" % argv[ac:-1])

            # create new return option
            newopt = BASE.comopt(com.name, com.n_exp, com.deflist)
            newopt.i_name = ac          # current index into argv
            newopt.acceptlist = com.acceptlist
            newopt.required = com.required 
            ac += 1                     # now point to next argument

            # create parlist of potential parameters
            if newopt.n_exp > 0:    # try to insert that number of args
                if newopt.n_exp <= alen - ac:
                    if verb > 2: print("+d adding %d params" % newopt.n_exp)
                    parlist = argv[ac:ac+newopt.n_exp]
                else:   # too few args
                    print("** error: arg #%d (%s) requires %d params" % \
                          (ac-1, newopt.name, newopt.n_exp))
                    return None
            elif newopt.n_exp < 0:  # grab everything, and truncate later
                if verb > 2: print("+d start with all %d params" % (alen-ac))
                parlist = argv[ac:]
            else: parlist = []      # n_exp == 0

            # truncate parlist if it contains an option
            for pc in range(len(parlist)):
                if parlist[pc] in namelist: # then we have pc 'good' params
                    parlist = parlist[:pc]
                    if verb > 1: print("-d truncate %s after %d of %d" % \
                                       (newopt.name, pc, len(parlist)))
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
                        print("** option %s: param '%s' is not in: %s" % \
                              (newopt.name, par, newopt.acceptlist))
                        return None  # what else can we do?

            # so do we still have enough parameters?
            if newopt.n_exp < 0: nreq = abs(newopt.n_exp)
            else:                nreq = newopt.n_exp
            if len(parlist) < nreq:
                print("** error: arg #%d (%s) requires %d params, found %d" % \
                      (ac-1, newopt.name, nreq, len(parlist)))
                return None

            # we have a full parlist, possibly check for dashes now
            if not com.okdash:
               for par in parlist:
                  if not par: continue  # check for empty param?  too anal?
                  if par[0] == '-':
                     print('** option %s has illegal dashed parameter: %s' \
                           % (newopt.name, par))
                     print('   --> maybe parameter is a mis-typed option?')
                     return None

            # success!  insert the remaining list
            newopt.parlist = parlist
            newopt.n_found = len(parlist)

        else:   # we seem to be done with expected arguments
            # there should not be any options in this final list
            for arg in argv[ac:]:
                if arg in namelist:
                    print("** error: option %s follows unknown arg #%d (%s)" % \
                          (arg, ac, argv[ac]))
                    return None

            if not oplist.trailers :   # then trailers are not allowed
                print("** error: unknown trailing arguments : %s" % argv[ac:])
                return None

            # insert remaining args as trailers
            newopt = BASE.comopt('trailers', -1, [])
            newopt.n_found = alen - ac
            newopt.parlist = argv[ac:]
            OL.trailers = 1   # flag to calling function
            if verb > 2: print("-- found trailing args: %s" % newopt.parlist)

        OL.olist.append(newopt) # insert newopt into our return list
        ac += newopt.n_found    # and increment the argument counter

    # now we have processed all of argv
    # any unused comopt that has a deflist can be used (else error)
            
    for co in oplist.olist:
        if namelist[co.name] == 0:  # may still be okay
            if co.required: 
                print("** error: missing option %s" % co.name)
                return None
            elif len(co.deflist) > 0:  # use it
                newopt = BASE.comopt(co.name, len(co.deflist), co.deflist)
                newopt.parlist = newopt.deflist
                # leave n_found at -1, so calling function knows
                OL.olist.append(newopt) # insert newopt into our return list
                if verb > 2: print("++ applying default opt '%s', args: %s" % \
                                   (co.name, newopt.deflist))

    if verb > 1 : OL.show("-d all found options: ")
    if verb > 3 : print("-d final optlist with counts: ", namelist)

    return OL

def opt_is_yes(opt):
    """return 1 if and only if option has yes/Yes/YES for oplist[0]"""

    if opt == None: return 0

    rv = 0
    try:
        val = opt.parlist[0]
        if val == 'yes' or val == 'Yes' or val == 'YES' \
                        or val == 'Y'   or val == 'y': rv = 1
    except: pass

    return rv

def opt_is_no(opt):
    """return 1 if and only if option has no/No/NO for oplist[0]"""

    if opt == None: return 0

    rv = 0
    try:
        val = opt.parlist[0]
        if val == 'no' or val == 'No' or val == 'NO' \
                       or val == 'N'  or val == 'n': rv = 1
    except: pass

    return rv

def opt_is_val(opt, optval):
    """return 1 if and only if opt.oplist[0] == optval"""

    if opt == None: return 0

    rv = 0
    try:
        if opt.parlist[0] == optval: rv = 1
    except: pass

    return rv

def comopts_key(copt):
    """function to be called on each comopts struct for use
       in sort(key=), since the cmp parameter is gone in python3

       return name field, as that is comparable for sort()
    """
    return copt.name

def compare_comopts(c1, c2):
    """comparison function for use in sort()
     return -1, 0, 1 for c1 compared with c2
    """
    if c1.name < c2.name: return -1
    if c1.name > c2.name: return  1
    return 0

def test_comopts():

    okopts = OptionList('for_input')
    okopts.add_opt('-a',      1, ['4'       ]               )
    okopts.add_opt('-dsets', -1, [          ]               )
    okopts.add_opt('-debug',  1, ['0'       ],     list(range(4)) )
    okopts.add_opt('-c',      2, ['21', '24']               )
    okopts.add_opt('-d',     -1, [          ]               )
    okopts.add_opt('-e',     -2, ['21', '24', '265']        )
    okopts.trailers = 1 # allow trailing args

    okopts.show('------ possible input options ------ ')

    found_opts = read_options(sys.argv, okopts)
    
    if found_opts: found_opts.show('------ found options ------ ')

# if __name__ == '__main__':
#     test_comopts()

