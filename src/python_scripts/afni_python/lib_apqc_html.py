#
# ver : 1.2 || date: Oct 17, 2018 || auth: PA Taylor
# + separate title and text strings
# + new warn type
#
#########################################################################

# mostly ways to read & wrap text & images.

def wrap_block_lab(x, vpad=0):
    y = """<h3><center>block: """+x+"""</center></h3>"""
    if vpad:
        y= """\n"""+y
        y+="""\n"""
    return y

# -------------------------------------------------------------------

def wrap_image_title(x, vpad=0, addclass=""):
    #y = """<h3><center>"""+x+"""</center></h3>"""
    y = """<pre """
    y+= ''' {} '''.format(addclass)
    y+= """ width="80"><center><b>"""+x+"""</b></center></pre>"""
    if vpad:
        y= """\n"""+y
        y+="""\n"""
    return y

# -------------------------------------------------------------------

def wrap_image_txt(x, vpad=0, addclass=""):
    #y = """<h3><center>"""+x+"""</center></h3>"""
    y = """<div """
    y+= ''' {} >'''.format(addclass)
    y+= """<pre><b>"""+x+"""</b></pre></div>"""
    if vpad:
        y= """\n"""+y
        y+="""\n"""
    return y

# -------------------------------------------------------------------

def wrap_img(x, wid=500, alt = "asdf", vpad=0, addclass=""):
    y = """<img src=\""""+x+"""\" alt=\""""+alt+"""\""""
    y+= ''' {} '''.format(addclass)
    y+= """ width=\""""+str(wid)+"""\" > """
    if vpad:
        y= """\n"""+y
        y+="""\n"""
    return y

# -------------------------------------------------------------------

# string literal
def wrap_dat(x, wid=500, alt = "asdf", vpad=0, addclass=""):
    y = """<pre """
    y+= ''' {} '''.format(addclass)
    y+= """ width="80"><left><b>"""+x+"""</b></left></pre>"""
    if vpad:
        y= """\n"""+y
        y+="""\n"""
    return y

# -------------------------------------------------------------------

def read_descrip_txt(x):
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

    title = txt[0]

    if Nlines > 1 :
        out = ''.join(txt[1:])
    else:
        out = ''
    return title, out


def read_dat(x):

    fff = open(x, 'r')
    txt = fff.readlines()
    fff.close()

    out = ''.join(txt)
    return out










button_GBU = """
<center>
<input type="radio" name="rbuttons" value="1"/> Good
<input type="radio" name="rbuttons" value="2"/> Bad
<input type="radio" name="rbuttons" value="3"/> Undefinedly Ugly
</center>
"""
