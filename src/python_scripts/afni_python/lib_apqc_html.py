#
# ver : 1.2 || date: Oct 17, 2018 || auth: PA Taylor
# + separate title and text strings
# + new warn type
#
# ver : 1.3 || date: Nov 1, 2018 
# + [PT] wrap_imag now includes href, so clicking on image opens it in
#   new link
#
# ver : 1.4 || date: Nov 1, 2018 
# + [PT] Making py3 compatible: 
#        from 2to3, updating DICT.has_key(x) -> x in DICT
#
#########################################################################

# mostly ways to read & wrap text & images.
import sys
import os
import json

# -------------------------------------------------------------------

# these are the properties/fields that the incoming text might have.
# These define what fields the jsons created by @ss_review_html (and
# therefore, ultimately in lib_apqc_tcsh.py) can have.
class apqc_item_info:

    title      = ""
    text       = ""
    subtext    = ""
    linkid     = ""
    linkid_hov = ""

    def set_title(self, DICT):
        if 'title' in DICT :
            self.title = DICT['title']

    def set_linkid(self, DICT):
        if 'linkid' in DICT :
            self.linkid = DICT['linkid']

    def set_linkid_hov(self, DICT):
        if 'linkid_hov' in DICT :
            self.linkid_hov = DICT['linkid_hov']

    def add_text(self, DICT):
        if 'text' in DICT :
            if type(DICT['text']) == list :
                xx = '\n'.join(DICT['text'])
                self.text+= xx
            else:
                self.text+= DICT['text']

    def add_subtext(self, DICT):
        if 'subtext' in DICT :
            if type(DICT['subtext']) == list :
                xx = '\n'.join(DICT['subtext'])
                self.subtext+= xx
            else:
                self.subtext+= DICT['subtext']

    # this just runs through all possible things above and fills in
    # what it can
    def set_all_from_dict(self, DICT):
        self.set_title(DICT)
        self.set_linkid(DICT)
        self.set_linkid_hov(DICT)
        self.add_text(DICT)
        self.add_subtext(DICT)

# -------------------------------------------------------------------

# these are the properties/fields that the incoming *page-title* info
# might have.  These define what fields the jsons created by
# @ss_review_html (and therefore, ultimately in lib_apqc_tcsh.py) can
# have.
class apqc_title_info:

    title      = ""
    subj       = ""
    taskname   = ""
    linkid     = ""
    linkid_hov = ""

    def set_title(self, DICT):
        if 'title' in DICT :
            self.title = DICT['title']

    def set_linkid(self, DICT):
        if 'linkid' in DICT :
            self.linkid = DICT['linkid']

    def set_linkid_hov(self, DICT):
        if 'linkid_hov' in DICT :
            self.linkid_hov = DICT['linkid_hov']

    def set_taskname(self, DICT):
        if 'taskname' in DICT :
            self.taskname = DICT['taskname']

    def set_subj(self, DICT):
        if 'subj' in DICT :
            self.subj = DICT['subj']

    # this just runs through all possible things above and fills in
    # what it can
    def set_all_from_dict(self, DICT):
        self.set_title(DICT)
        self.set_linkid(DICT)
        self.set_linkid_hov(DICT)
        self.set_taskname(DICT)
        self.set_subj(DICT)

# -------------------------------------------------------------------

def wrap_block_lab(x, vpad=0):
    y = """<h3><center>block: """+x+"""</center></h3>"""
    if vpad:
        y= """\n"""+y
        y+="""\n"""
    return y

# -------------------------------------------------------------------

#def make_nav_table(llinks):
#
#    N = len(llinks)
#    idx = 0
#
#    y = '''<div>\n'''
#
#    for i in range(N):
#        ll, hov = llinks[i][0], llinks[i][1]
#        lname = ll #str(i)+ '.'+ll
#        y+= '''<a href="#{}" title="{}">'''.format( ll, hov )
#        y+= '''<b>{}</b></a>\n'''.format( lname )
#    y+= '''</div>'''
#    
#    return y

def make_nav_table(llinks):

    N = len(llinks)
    idx = 0

    y = '''<div><nav><ul class="section">\n'''

    for i in range(N):
        ll, hov = llinks[i][0], llinks[i][1]
        lname = ll #str(i)+ '.'+ll
        y+= '''<li><a href="#{}" title="{}">'''.format( ll, hov )
        y+= '''<b>{}</b></a></li>\n'''.format( lname )
    y+= '''</ul></nav></div>'''
    
    return y

# -------------------------------------------------------------------

def wrap_image_title(x, vpad=0, addclass="", linkid=''):
    y = '''<div'''
    if linkid :
        y+= ''' id="{}" '''.format(linkid)
    # this line offsets the anchor location for the navigation bar to
    # head to: the values here should be equal to the height of the
    # navigation bar (plus the 2px line beneath it).
    y+= ''' style="padding-top: 38px; margin-top: -38px;"'''
    y+= """><pre """
    y+= ''' {} '''.format(addclass)
    y+= """ width="80"><center><b>"""+x+"""</b></center></pre></div>"""
    if vpad:
        y= """\n"""+y
        y+="""\n"""
    return y

# -------------------------------------------------------------------

def wrap_image_txt(x, vpad=0, addclass="", dobold=True):
    #y = """<h3><center>"""+x+"""</center></h3>"""
    y = """<div """
    y+= ''' {} >'''.format(addclass)
    if dobold :
        y+= """<pre><b>"""+x+"""</b></pre></div>"""
    else:
        y+= """<pre>"""+x+"""</pre></div>"""
    if vpad:
        y= """\n"""+y
        y+="""\n"""
    return y

# -------------------------------------------------------------------

def wrap_img(x, wid=500, vpad=0, addclass=""):
    # [PT: Nov 20, 2018] needed this next line to center the text, and
    # needed "display: inline-block" in the img {} def to not have
    # whole line be a link.
    y = '''<div style="text-align: center">'''  
    y+= """<a  href=\""""+x+"""\"><img src=\""""+x+"""\" alt=\""""+x+"""\" """
    y+= ''' {} '''.format(addclass)
    y+= """display: inline-block; text-align: center; width=\""""+str(wid)+"""\" ></a> """
    y+= '''</div>'''
    if vpad:
        y= """\n"""+y
        y+="""\n"""
    return y

# -------------------------------------------------------------------

# string literal
def wrap_dat(x, wid=500, vpad=0, addclass=""):
    y = """<pre """
    y+= ''' {} '''.format(addclass)
    y+= """ width="80"><left><b>"""+x+"""</b></left></pre>"""
    if vpad:
        y= """\n"""+y
        y+="""\n"""
    return y

# -------------------------------------------------------------------

def read_descrip_json(x):
    '''Take the input json file 'x' and return an instance of the
apqc_item_info class.
'''

    # get json as dictionary
    ddd       = read_json_to_dict(x)
    # initialize object to hold info
    apqc_info = apqc_item_info()
    # set everything in this object that we can
    apqc_info.set_all_from_dict(ddd)

    return apqc_info

# -------------------------------------------------------------------

def read_title_json(x):
    '''Take the input json file 'x' and return an instance of the
apqc_title_info class.
'''

    # get json as dictionary
    ddd       = read_json_to_dict(x)
    # initialize object to hold info
    apqc_info = apqc_title_info()
    # set everything in this object that we can
    apqc_info.set_all_from_dict(ddd)

    return apqc_info

# ----------------------------------------------------------------------

def read_dat(x):

    fff = open(x, 'r')
    txt = fff.readlines()
    fff.close()

    out = ''.join(txt)
    return out

# ----------------------------------------------------------------------


# !!!!!!!!!! just delete this- nothing particular to 'pbar'
# check if json exists- return full or null dict
#def read_pbar_json(x):
#
#    if os.path.isfile(x):
#        with open(x, 'r') as fff:
#            xdict = json.load(fff)
#    else:
#        xdict = {}
#
#    return xdict

# check if json exists- return full or null dict
def read_json_to_dict(x):

    if os.path.isfile(x):
        with open(x, 'r') as fff:
            xdict = json.load(fff)
    else:
        xdict = {}

    return xdict

# ----------------------------------------------------------------------

#    if dobold :
#        y+= """<pre><b>"""+x+"""</b></pre></div>"""
#    else:
#        y+= """<pre>"""+x+"""</pre></div>"""

def make_pbar_line(d, imgpbar, vpad=0, addclassdiv="", addclassimg="",
                   dobold=True):

    y = """<div """
    y+= '''{} >'''.format(addclassdiv)
    if dobold :
        y+= """<pre><b>olay:  {} """.format(d['pbar_bot'])
    else:
        y+= """<pre>olay:  {} """.format(d['pbar_bot'])
    y+= '''<img {} '''.format(addclassimg)
    y+= '''style="display: inline; margin: -5 -5px;" '''
    y+= '''src="{}" > '''.format(imgpbar)
    y+= '''{} ({})\n'''.format(d['pbar_top'], d['pbar_reason'])
    if dobold :
        y+= '''thr :  {} ({}) </b></pre></div>\n'''.format(d['vthr'], 
                                                           d['vthr_reason'])
    else:
        y+= '''thr :  {} ({}) </pre></div>\n'''.format(d['vthr'], 
                                                       d['vthr_reason'])
    if vpad:
        y= """\n"""+y
        y+="""\n"""

    return y

# ----------------------------------------------------------------------

def read_pbar_range(x, dtype="NA"):

    fff = open(x, 'r')
    txt = fff.readlines()
    fff.close()

    Nlines = len(txt)
    if not(Nlines):
        sys.exit("** ERROR: no lines of text in {}?".format(x))
    elif Nlines > 1:
        sys.exit("** ERROR: too many lines (={}) in {}?".format(Nlines, x))

    l0 = txt[0]
    y  = l0.split()

    Nnums = len(y) 
    if Nnums != 3:
        sys.exit("** ERROR: wrong number of nums (={}) in {}?".format(Nnums, x))

    out = []
    if dtype == int :
        for nn in y:
            z = int(nn)
            out.append(z)
    elif dtype == float :
        for nn in y:
            z = float(nn)
            out.append(z)
    else: # the NA or other cases...
        for nn in y:
            if nn.__contains__('.'):
                z = float(nn)
            else:
                z = int(nn)
            out.append(z)

    return out[0], out[1], out[2]









button_GBU = """
<center>
<input type="radio" name="rbuttons" value="1"/> Good
<input type="radio" name="rbuttons" value="2"/> Bad
<input type="radio" name="rbuttons" value="3"/> Undefinedly Ugly
</center>
"""
