#
# ver : 1.2 || date: Oct 17, 2018 || auth: PA Taylor
# + separate title and text strings
# + new warn type
#
# ver : 1.3 || date: Nov 1, 2018 
# + [PT] wrap_imag now includes href, so clicking on image opens it in
#   new link
#
#########################################################################

# mostly ways to read & wrap text & images.
import sys
import os
import json

def wrap_block_lab(x, vpad=0):
    y = """<h3><center>block: """+x+"""</center></h3>"""
    if vpad:
        y= """\n"""+y
        y+="""\n"""
    return y

# -------------------------------------------------------------------

def wrap_image_title(x, vpad=0, addclass=""):
    y = """<pre """
    y+= ''' {} '''.format(addclass)
    y+= """ width="80"><center><b>"""+x+"""</b></center></pre>"""
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

##### old, pre-json
#def make_pbar_line(pbar_min, pbar_max, pbar_thr, imgpbar, vpad=0, addclass=""):
#
#    y = """<div """
#    y+= '''{} >'''.format(addclass)
#    y+= """<pre>{} """.format(pbar_min)
#    y+= '''<img style="display: inline; margin: -5 -5px;" '''
#    y+= '''src="{}" '''.format(imgpbar)
#    y+= '''vertical-align="middle" height="20px"> '''
##    y+= '''{} </pre></div>\n'''.format(pbar_max)
#    y+= '''{}\n'''.format(pbar_max)
#    y+= '''vthr: {} (90 %ile) </pre></div>\n'''.format(pbar_thr)
#    if vpad:
#        y= """\n"""+y
#        y+="""\n"""
#
#    #y = '''\n<div class=container><pre> {} <img style="display: inline; margin: 0 0px;" src="{}" align="middle" width="160px" height="25px"> {} </pre></div>\n'''.format(pbar_min, imgpbar, pbar_max)
#    return y

# -------------------------------------------------------------------

def wrap_img(x, wid=500, vpad=0, addclass=""):
    y = """<a  href=\""""+x+"""\"><img src=\""""+x+"""\" alt=\""""+x+"""\" """
    y+= ''' {} '''.format(addclass)
    y+= """ width=\""""+str(wid)+"""\" ></a> """
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

def read_descrip_txt(x, nline_title=1):
    '''Take the input text file 'x' and return two strings:

    the first line of 'x' by itself, which will be a title; and

    the remaining lines of 'x', which will be centered block of
    left-justified text in the output.

    If 'x' only has one line, then the second part is an empty string.
    '''


    fff = open(x, 'r')
    txt = fff.readlines()
    fff.close()

    Nlines = len(txt)
    if not(Nlines):
        sys.exit("** ERROR: no lines of text in {}?".format(x))

    if nline_title : 
        title = ''.join(txt[:nline_title])
    else:
        title = ''

    if Nlines-nline_title > 0 :
        out = ''.join(txt[nline_title:])
    else:
        out = ''
    return title, out

# ----------------------------------------------------------------------

def read_dat(x):

    fff = open(x, 'r')
    txt = fff.readlines()
    fff.close()

    out = ''.join(txt)
    return out

# ----------------------------------------------------------------------

# check if json exists- return full or null dict
def read_pbar_json(x):

    if os.path.isfile(x):
        with open(x, 'r') as fff:
            xdict = json.load(fff)
    else:
        xdict = {}

    return xdict

# ----------------------------------------------------------------------

#!!!!!!!!!!!!!!!!!
# "pbar_bot"
# "pbar_top"
# "pbar_reason"
# "vthr"
# "vthr_reason"

def make_pbar_line(d, imgpbar, vpad=0, addclassdiv="", addclassimg=""):

    y = """<div """
    y+= '''{} >'''.format(addclassdiv)
    y+= """<pre>olay:  {} """.format(d['pbar_bot'])
    y+= '''<img {} '''.format(addclassimg)
    y+= '''style="display: inline; margin: -5 -5px;" '''
    y+= '''src="{}" > '''.format(imgpbar)
    y+= '''{} ({})\n'''.format(d['pbar_top'], d['pbar_reason'])
    y+= '''thr :  {} ({}) </pre></div>\n'''.format(d['vthr'], d['vthr_reason'])
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
