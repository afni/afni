#!/usr/bin/env python

# ============================================================================
# 
# A library file for read HEAD files.
#
# We try to match the output of '3dAttribute -all ...' as closely as
# possible. This includes keeping specific features (AKA quirks) of
# the output, like: 
# + adding an extra space at the end of numerical lists,
# + replacing a long string-attribute value of '~' with '(null)'
# + maintaining some apparently extraneous newline chars in some text fields
# 
# We have now gone after more of the rounding logic that 3dAttribute
# imposes on floating point values. However, even so, there can still
# be some very tiny differences at truncation points. Such is life. We
# don't take those as failures, just another reminder that one should
# not try for strict equality with floating point numbers.
# 
# auth: PA Taylor (SSCC, NIMH, NIH, USA)
#
# ============================================================================

import os, sys, copy
import math

# ----------------------------------------------------------------------------

# list of all known AFNI attribute "type = " values
LIST_attribute_types = [
    'float-attribute',
    'integer-attribute',
    'string-attribute',
]
STR_attribute_types = ', '.join(LIST_attribute_types)

# character used to separate per-volume values in string-attributes
ssep = '~'

# what are keyword values that 'something' can take in helper function
# below, appropriate for HEAD file
LIST_something = [
    'type', 
    'name',
    'count'
]
STR_something = ', '.join(LIST_something)

# ----------------------------------------------------------------------------

# these attributes are output by '3dAttribute -all ...', with these
# values, even if they don't actually appear in the *.HEAD file
DICT_default_attributes = {
    'BRICK_LABS'     : ['string-attribute', '#0~'],
    'BRICK_KEYWORDS' : ['string-attribute', '~'],
    'TEMPLATE_SPACE' : ['string-attribute', 'ORIG~'],
    'INT_CMAP'       : ['integer-attribute', '0'],
}

# ============================================================================

class HeadFile:
    """Object for reading in a HEAD file from an AFNI BRIK/HEAD dset
formatted volume.

Parameters
----------
inset : str
    filename for an input BRIK/HEAD file. Can be specified in any of
    the usual AFNI ways: DSET+orig, DSET+orig., DSET+orig.BRIK,
    DSET+orig.BRIK.gz, DSET+orig.HEAD, etc.
add_defaults : bool
    flag to imitate '3dAttribute -all ...' and include some extra BRIK/HEAD
    attributes in the output, even if they don't appear in the *.HEAD file
    itself
verb : int
    verbosity for terminal text whilst processing

    """

    def __init__(self, inset=None, add_defaults=True, verb=1):

        # ----- set up attributes

        # general variables
        self.verb            = verb

        # main filenames
        self.inset           = inset
        self.full_inset      = None           # inset with any ~ expanded
        self.headset         = None           # *.HEAD file of any inset name

        # main data
        self.headtext        = []             # list of lines of *.HEAD files
        self.all_attributes  = []             # list of all attributes

        self.report          = []             # list of strings to write out

        # ----- take action(s)

        if not(inset is None) :
            tmp1 = self.load_inset()
            tmp2 = self.read_headset()
            tmp3 = self.extract_attributes()
            if add_defaults : 
                tmp3b = self.set_default_attributes()
            tmp4 = self.make_report()

    # ----- methods

    def load_inset(self):
        """Verify that the infile exists, and load it as the headset
        attribute. That means, identify the HEAD file specifically of
        the BRIK/HEAD dset. This step allows the user to specify a
        dset in any of the usual AFNI ways.
        """

        if self.verb > 1 :
            print("++ Load inset: {}".format(self.inset))
        
        BAD_RETURN = -1

        if not(self.inset) :
            print("+* No inset? Nothing to do.")
            return BAD_RETURN

        self.full_inset = os.path.expanduser(self.inset)
        # ... and for convenience
        inset = self.full_inset

        # derive potential headset filename
        if inset.endswith('.HEAD') :
            self.headset = inset
        elif inset.endswith('.BRIK') :
            self.headset = inset[:-4] + 'HEAD'
        elif inset.endswith('.BRIK.gz') :
            self.headset = inset[:-7] + 'HEAD'
        elif inset.endswith('.') :
            self.headset = inset + 'HEAD'
        else:
            self.headset = inset + '.HEAD'
        
        # verify proposed headset
        if not(os.path.exists(self.headset)) :
            print("** Cannot find supposed headset: {}".format(self.headset))
            return BAD_RETURN

        return 0


    def read_headset(self):
        """Read the headset, which should be a straightforward *.HEAD text
        file, into a list of strings called headtext."""

        if self.verb > 1 :
            print("++ Read headset")

        BAD_RETURN = -1

        if self.headset is None :
            print("** No headset supplied to read")
            return BAD_RETURN

        fff = open(self.headset, 'r')
        self.headtext = fff.readlines()
        fff.close()

        if not(self.nlines_headtext) :
            print("** No lines of text found from *.HEAD file")
            return BAD_RETURN

        return 0

    def extract_attributes(self):
        """Go through headtext line-by-line and extract attributes. Each is
        attribute is stored as a HeadAttribute object, and these are
        stored within the all_attributes list."""

        if self.verb > 1 :
            print("++ Extract attributes")

        BAD_RETURN = -1 

        # start with empty list
        self.all_attributes = []

        # initialize quantities
        N    = self.nlines_headtext
        ibot = -1
        itop = 0

        # loop through and find attributes, skipping first empty lines
        while itop < N :
            line = self.headtext[itop]
            is_fail, is_something, something_val = \
                is_line_SOMETHING(line, something='type')
            if is_fail :
                return BAD_RETURN
            elif is_something :
                if ibot >= 0 :
                    # grab text from ibot through itop-1, and parse for attr
                    minitext = self.headtext[ibot:itop]
                    attr = HeadAttribute(L=minitext, verb=self.verb)
                    self.all_attributes.append(attr)
                    # prepare for finding next attribute
                ibot = itop
            itop+= 1
        # get final attribute
        minitext = self.headtext[ibot:itop]
        attr = HeadAttribute(L=minitext, verb=self.verb)
        self.all_attributes.append(attr)
        
        if self.verb :
            print("++ Number of attributes found: {}".format(self.nattr))

        return 0

    def set_default_attributes(self):
        """To imitate '3dAttribute -all ...', include a set of default 
        attributes+values in the list of attributes, even if they don't 
        actually appear in the *.HEAD file itself."""

        BAD_RETURN = -1

        for key in DICT_default_attributes.keys():

            if not(key in self.list_attr_names):
                name  = key
                attr_type = DICT_default_attributes[key][0]
                value     = DICT_default_attributes[key][1:]

                is_fail, M = \
                    create_minitext_from_attr_info(name, value, attr_type)

                if is_fail :
                    print("** ERROR: failed for def attr: {}".format(name))
                    return BAD_RETURN

                attr = HeadAttribute(L=M, verb=self.verb)
                self.all_attributes.append(attr)
        
        if self.verb :
            print("++ Final number of attributes: {}".format(self.nattr))

        return 0

    def disp_attr_names_and_values(self):
        """Display the names and values of all attributes in the list,
        mirroring '3dAttribute -all ...' output """

        print('\n'.join(self.report))

    def make_report(self):
        """Make a list of text strings with the names and values of all
        attributes in the list, mirroring '3dAttribute -all ...'
        output.
        """

        self.report = []

        for ii in range(self.nattr):
            attr   = self.all_attributes[ii]
            atype  = attr.type
            aname  = attr.name
            acount = attr.count
            avalue = attr.value
            
            if atype in ['integer-attribute'] :
                # match the spacing of '3dAttribute -all ...', even
                # including an extra ' ' at the end of the line
                str_val = ''
                for val in avalue :
                    str_val+= str(val) + ' '
                txt = "{} = {}".format(aname, str_val)
                self.report.append(txt)

            elif atype in ['float-attribute'] :
                # match the spacing of '3dAttribute -all ...', even
                # including an extra ' ' at the end of the line
                txt = "{} = ".format(aname)
                for val in avalue :
                    is_fail, ttt = MV_fval_to_char( val )
                    txt+= ttt.strip() + ' '
                self.report.append(txt)

            elif atype == 'string-attribute' :
                txt = "{} = {}".format(aname, avalue[0])
                self.report.append(txt)
                
                # NB: for the purposes of matching the count value,
                # explicit '\n' at the end of some lines was left
                # earlier, but will be removed here for printing
                for nn in range(1, len(avalue)):
                    txt = "{}".format(avalue[nn])
                    self.report.append(txt.rstrip())

        return 0

    def write_report(self, fname):
        """Write the report to a text file, fname."""

        fname_full = os.path.expanduser(fname)

        fff = open(fname_full, 'w')
        fff.write('\n'.join(self.report))
        fff.write('\n') # add final new line
        fff.close()

        return 0

    # ----- decorators

    @property
    def nlines_headtext(self):
        """number of lines in headtext"""
        return len(self.headtext)

    @property
    def nattr(self):
        """number of attributes in the list"""
        return len(self.all_attributes)

    @property
    def list_attr_names(self):
        """return a list of the names of all attributes in the list"""
        all_name = []
        for ii in range(self.nattr):
            all_name.append(self.all_attributes[ii].name)
        return all_name

    @property
    def disp_attr_names(self):
        """display the names of all attributes in the list"""
        all_name = self.list_attr_names
        print('\n'.join(all_name))



# ============================================================================

class HeadAttribute:
    """Object for storing pieces of an AFNI attribute in a BRIK/HEAD file.

After running this, one has the very useful object attributes to use:
    .type
    .name
    .count
    .value
These will help for working with the AFNI attributes.

Parameters
----------
L : list (of str)
    list of text lines taken from within an AFNI *.HEAD file, that
    contain the contents of an attribute.  May or may not have empty lines 
    at start/finish

    """

    def __init__(self, L=[], verb=1):

        # ----- set up attributes

        # general variables
        self.verb            = verb

        # main input
        self.L               = L              

        # attributes storing header info
        self.type            = None            # str, *-attribute type name
        self.name            = None            # str, label for attr
        self.count           = None            # int, str len or num of items
        self.value           = None            # more useful/parsed value
        self.raw_value       = None            # actual attribute value

        # ----- take action(s)

        if self.L :
            tmp1 = self.check_L()
            tmp2 = self.parse_L()
            tmp3 = self.verify_attribute_ANY()

    # ----- methods

    def check_L(self):
        """Verify that L has desired properties. Namely, that it is a list of
        strings.
        """

        if self.verb > 1 : 
            print("")
            print("++ Checking input list L")

        BAD_RETURN = -1 

        if not(isinstance(self.L, list)) :
            print("** Input L is not a list, so exiting")
            return BAD_RETURN

        if len(self.L) == 0 :
            print("** Input list L has len=0, so exiting")
            return BAD_RETURN

        for line in self.L :
            if type(line) != str :
                print("** Input list L has non-string element, so exiting")
                return BAD_RETURN

        return 0


    def parse_L(self):
        """Go through list L and get necessary pieces.
        """

        if self.verb > 1 : 
            print("++ Parsing input list L")

        if self.verb > 2 :
            print("++ list of text to parse is:")
            print("-" * 20)
            print('   ' + '   '.join(self.L))
            print("-" * 20)

        BAD_RETURN = -1 

        # initialize properties
        N  = len(self.L)
        i0 = 0

        # find: type
        for ii in range(i0, N):
            line = self.L[ii]
            is_fail, is_something, something_val = \
                is_line_SOMETHING(line, something='type')
            if is_fail :
                return BAD_RETURN
            elif is_something :
                self.type = something_val
                i0 = ii + 1
                break

        # find: name
        for ii in range(i0, N):
            line = self.L[ii]
            is_fail, is_something, something_val = \
                is_line_SOMETHING(line, something='name')
            if is_fail :
                return BAD_RETURN
            elif is_something :
                self.name = something_val
                i0 = ii + 1
                break

        # find: count
        for ii in range(i0, N):
            line = self.L[ii]
            is_fail, is_something, something_val = \
                is_line_SOMETHING(line, something='count')
            if is_fail :
                return BAD_RETURN
            elif is_something :
                self.count = int(something_val)
                i0 = ii + 1
                break

        # find: raw_value
        self.raw_value = self.L[i0:]

        # ... and remove any empty line at the end of the value list
        nvalue = len(self.raw_value)
        for ii in range(nvalue):
            jj   = ii + 1
            line = self.raw_value[-jj]
            if len(line.strip()) :
                break
        self.raw_value = self.raw_value[:nvalue-ii]
        
        return 0            

    def verify_attribute_ANY(self):
        """Do some consistency checks on the attribute. The sub-functions here
        also should set the self.value attribute from self.raw_value"""

        BAD_RETURN = -1

        if not(self.type in LIST_attribute_types):
            print("** ERROR: when parsing attr: {}".format(self.name))
            print("   Unknown attribute type: {}".format(self.type))
            print("   Not in recognized list of types:")
            print("     {}".format(STR_attribute_types))
            return BAD_RETURN

        if self.type == 'integer-attribute' :
            is_fail = self.verify_integer_attribute()
        elif self.type == 'float-attribute' :
            is_fail = self.verify_float_attribute()
        elif self.type == 'string-attribute' :
            is_fail = self.verify_string_attribute()
        else:
            print("** ERROR: should not reach this unknown type err")
            return BAD_RETURN

        return is_fail

    def verify_integer_attribute(self):
        """Do some consistency checks on the integer-attribute type. If all
        goes well, this should set the value in a parsed/appropriate
        way. Values here are now treated like ints.

        """

        BAD_RETURN = -1
        attr_type  = 'integer-attribute'

        if self.verb > 2 :
            print("++ Verify attr of type {}: {}".format(attr_type, self.name))

        if self.type != attr_type :
            print("** ERROR: when parsing attr: {}".format(self.name))
            print("   the type is not actually " + attr_type)
            return BAD_RETURN

        all_val = []
        for line in self.raw_value :
            all_val.extend(line.split())

        # verify int values: test 1
        # ... and this is also where the list of actual values is created
        try:
            self.value = [int(val) for val in all_val]
        except:
            print("** ERROR: when parsing attr: {}".format(self.name))
            print("   fail to convert integer-attributes to int:")
            print("   {}".format(' '.join(all_val)))
            return BAD_RETURN

        nvalue = len(self.value)
        if nvalue != self.count :
            print("** ERROR: when parsing attr: {}".format(self.name))
            print("   mismatch in attr count (= {})".format(self.count))
            print("   and number of values (= {})".format(nvalue))
            return BAD_RETURN

        # verify int values: test 2
        for ii in range(nvalue):
            if self.value[ii] != float(all_val[ii]) :
                print("** ERROR: when parsing attr: {}".format(self.name))
                print("   integer-attribute is not int-valued:")
                print("   {}".format(all_val[ii]))
            return BAD_RETURN

        return 0

    def verify_float_attribute(self):
        """Do some consistency checks on the float-attribute type. If all
        goes well, this should set the value in a parsed/appropriate way."""

        BAD_RETURN = -1
        attr_type  = 'float-attribute'

        if self.verb > 2 :
            print("++ Verify attr of type {}: {}".format(attr_type, self.name))

        if self.type != attr_type :
            print("** ERROR: when parsing attr: {}".format(self.name))
            print("   the type is not actually " + attr_type)
            return BAD_RETURN

        all_val = []
        for line in self.raw_value :
            all_val.extend(line.split())

        # verify float values
        # ... and this is also where the list of actual values is created
        try:
            self.value = [float(val) for val in all_val]
        except:
            print("** ERROR: when parsing attr: {}".format(self.name))
            print("   fail to convert float-attributes to float:")
            print("   {}".format(' '.join(all_val)))
            return BAD_RETURN

        nvalue = len(self.value)
        if nvalue != self.count :
            print("** ERROR: when parsing attr: {}".format(self.name))
            print("   mismatch in attr count (= {})".format(self.count))
            print("   and number of values (= {})".format(nvalue))
            return BAD_RETURN

        return 0

    def verify_string_attribute(self):
        """Do some consistency checks on the string-attribute type. If all
        goes well, this should set the value in a parsed/appropriate way."""

        BAD_RETURN = -1
        attr_type  = 'string-attribute'

        if self.verb > 2 :
            print("++ Verify attr of type {}: {}".format(attr_type, self.name))

        if self.type != attr_type :
            print("** ERROR: when parsing attr: {}".format(self.name))
            print("   the type is not actually " + attr_type)
            return BAD_RETURN

        nval_raw = len(self.raw_value)

        if not(nval_raw) :
            print("** ERROR: when parsing attr: {}".format(self.name))
            print("   -> it appears empty")
            return BAD_RETURN

        # make a list of all strings, but remove the first ' and the
        # last \n (and see note in self.make_report() method about
        # where we do/don't use rstrip() here).
        all_val = [self.raw_value[0][1:].rstrip()]
        nchar   = len(all_val[0])
        
        # NB: a special case of replacement comes in, when the
        # string-attribute has count=1 and the only char is '~'; in
        # this case, '3dAttribute -all ...' replaces the '~' with
        # '(null)', so we do so here.
        if self.count == 1 and all_val[0] == '~' :
            all_val[0] = '(null)'

        for ii in range(1, nval_raw):
            line  = self.raw_value[ii]
            nchar+= len(line)
            all_val.append(line)

        self.value = copy.deepcopy(all_val)

        if self.count != nchar :
            print("** ERROR: when parsing attr: {}".format(self.name))
            print("   count = {}, but nchar = {}".format(self.count, nchar))
            return BAD_RETURN

        return 0
            
# ============================================================================
# useful helper functions

def is_line_SOMETHING(line, something=None):
    """Check if a given str line appears to be a line that defines a
useful kind of 'something' in the *.HEAD file.  

User must provide the name of 'something'.  Appropriate keywords at
present include:  type, name, count.

Parameters 
----------
line : str
    one candidate line

Returns
-------
is_fail : int
    0 for success, nonzero for failure
is_something : bool
    True for line is a 'something = ...' line; False for not.
something_val : str
    if is_something=True, the string that is the something's value;
    else, return ''

    """

    is_something  = False
    something_val = ''

    BAD_RETURN = (-1, is_something, something_val)

    if something is None :
        print("** ERROR: need to provide a keyword for 'something'")
        return BAD_RETURN
    elif not(something in LIST_something) :
        print("** ERROR: keyword for 'something' must be one of these items:")
        print("   {}".format(STR_something))
        print("   ... and that does not include: {}".format(something))
        return BAD_RETURN

    if not(isinstance(line, str)) :
        print("** ERROR: input is not a str, but {}:".format(type(line)))
        print("   {}".format(line))
        return BAD_RETURN

    line_clean = line.strip()
    if len(line_clean):
        line_split = line_clean.split()
        if len(line_split) == 3 :
            if line_split[0] == something and line_split[1] == '=' :
                is_something  = True
                something_val = line_split[2]

    return 0, is_something, something_val

def create_minitext_from_attr_info(name, value, attr_type):
    """Create an list of strings from a minimal set of attribute
information. The list of strings is made so that it can be provided as
an input argument to the HeadAttribute class, for generating a full
attribute with appropriate info.

For integer-attribute or float-attribute, we assume

Parameters
----------
name : str
    the name portion of attribute
value : list (of str)
    the value portion of attribute; this could in general be a
    multiline thing, so we all for that possibility as a list of str
attr_type : str
    the attribute portion of the attribute

Returns
-------
is_fail : int
    0 for success, nonzero for failure
M : list (of str)
    minitext that imitates what the 
    """

    BAD_RETURN = (-1, [])

    if not(attr_type in LIST_attribute_types) :
        print("** ERROR:")
        print("   Unknown attribute type: {}".format(self.type))
        print("   Not in recognized list of types:")
        print("     {}".format(STR_attribute_types))
        return BAD_RETURN

    M = []

    # type and name are straightforward
    txt_type = "type = {}".format(attr_type)
    txt_name = "name = {}".format(name)
    M.append(txt_type)
    M.append(txt_name)

    # determine count from value list
    if attr_type in ['integer-attribute', 'float-attribute'] :
        all_val = []
        for line in value:
            line_split = line.split()
            all_val.extend(line_split)
        count = len(all_val)
    elif attr_type == 'string-attribute' :
        count = 0
        for line in value:
            count+= len(line)
    
    txt_count = "count = {}".format(count)
    M.append(txt_count)

    # append values, with initial prepend of ' for str-attr
    pre_text = "'"*int(attr_type == 'string-attribute')
    M.append(pre_text + value[0])
    for ii in range(1, len(value)):
        M.append(value[ii])

    return 0, M

# ============================================================================

def MV_fval_to_char( qval ):
    """Follow the same atr_print -> MV_format_fval -> MV_fval_to_char
(last one is in src/multivector.c) formatting used by 3dAttribute.

Parameters
----------
qval : float (or str)
    floating point value (if qval is a str, convert with float())

Returns
-------
is_fail : int
    0 for success, nonzero for fail
buf : str
    formatted version of qval

    """

    BAD_RETURN = (-1, '')

    # in case a string is input
    if isinstance(qval, str):
        qval = float(qval)

    aval = abs(qval)

    # ----- special case if the value is an integer

    if qval == 0.0 :
        buf = "0"
        return 0, buf

    if aval < 99999999.0 :
        lv = int(qval)
    else:
        lv = 100000001

    if qval == lv and abs(lv) < 100000000 :
        buf = "{:d}".format(lv)
        return 0, buf

    # noninteger: choose floating format based on magnitude

    lv = int(10.0001 + math.log10(aval))

    if 6 <= lv and lv <= 10 :
        # 0.0001-0.001, 0.001-0.01, 0.01-0.1, 0.1-1, 1-9.99
        is_fail, buf = BSTRIP("{:-9.6f}".format(qval))
        return 0, buf

    elif lv == 11 :
        # 10-99.9
        is_fail, buf = BSTRIP("{:-9.5f}".format(qval))
        return 0, buf

    elif lv == 12 :
        # 100-999.9
        is_fail, buf = BSTRIP("{:-9.4f}".format(qval))
        return 0, buf

    elif lv == 13 :
        # 1000-9999.9
        is_fail, buf = BSTRIP("{:-9.3f}".format(qval))
        return 0, buf

    elif lv == 14 :
        # 10000-99999.9 
        is_fail, buf = BSTRIP("{:-9.2f}".format(qval))
        return 0, buf

    elif lv == 15 :
        # 100000-999999.9 
        is_fail, buf = BSTRIP("{:-9.1f}".format(qval))
        return 0, buf

    elif lv == 16 :
        # 1000000-9999999.9
        is_fail, buf = BSTRIP("{:-9.0f}".format(qval))
        return 0, buf

    elif qval > 0.0 :
        buf = "{:-12.6e}".format(qval)
        return 0, buf

    else:
        buf = "{:-12.5e}".format(qval)
        return 0, buf

def BSTRIP(x):
    """Strip trailing zeros (or rightside whitespace) from str x.

This is also a macro in src/multivector.c. 

**WARNING** NB: I have added some checks+warnings in this version, but
you should be very careful about using this widely. Within the
confines of the conditional logic within MV_factor_fval() above, it
should be pretty safe. But used incorrectly, this might wipe out 0s
that carry real meaning!  For example, make sure this only operates on
numbers like 100.0, and not 100!

Parameters
----------
x : str
    string (hopefully) representing a floating point number

Returns
-------
is_fail : int
    0 for success, nonzero for failure
y : str
    processed string

    """

    BAD_RETURN = (-1, '')

    # in case a string is input
    if not(isinstance(x, str)) :
        msg = "** ERROR: input to BSTRIP() must be a str, "
        msg+= "not type: {}".format(type(x))
        print(msg)
        return BAD_RETURN

    # warn if no decimal point, because removing 0 might be dangerous
    if not('.' in x):
        msg = "+* WARN: no decimal point in x. "
        msg+= "Is it OK to remove '0' on the right side of: {}?".format(x)
        print(msg)

    # warn if using exponential notation, because removing 0 might be dangerous
    if 'e' in x or 'E' in x :
        msg = "+* WARN: have an e or E in x. "
        msg+= "Is it OK to remove '0' on the right side of: {}?".format(x)
        print(msg)

    # remove whitespace at the right
    y = x.rstrip()

    # remove 0 at right; 
    y = y.rstrip('0')

    return 0, y

# ============================================================================

if __name__ == "__main__" :

    x = HeadFile('~/AFNI_data6/FT_analysis/FT.results/FT_anat+orig.HEAD')
    x.write_report('~/AFNI_data6/FT_analysis/FT.results/report_py_head.txt')

    print("\n"
    "-> Then run this in the command line:\n"
    "\n"
    "   cd ~/AFNI_data6/FT_analysis/FT.results \n"
    "   3dAttribute -all FT_anat+orig.HEAD > report_3dAttr_head.txt \n"
    "   meld report_3dAttr_head.txt report_py_head.txt\n")
