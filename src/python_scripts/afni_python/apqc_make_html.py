#!/usr/bin/env python
#
# ver : 1.3 || date: Oct 17, 2018 || auth: PA Taylor
# + new container/div for centering text box (while left-ifying text)
#
# ver : 1.4 || date: Oct 19, 2018 || auth: PA Taylor
# + parse inp opts
#
#########################################################################

import os
import sys
import glob
import json
import lib_apqc_html  as lah
import lib_apqc_tcsh  as lat
import lib_apqc_io    as laio

# ------------------------------------------------------------------------

ohtml   = 'index.html'

# =========================================================================
if __name__ == "__main__":

    # parse inputs, and get current dir (to return to at end)
    iopts  = laio.parse_html_args(sys.argv[1:])
    my_cwd = os.getcwd()

    # move to subj qc dir
    os.chdir(iopts.qcdir)

    # ========================= HTML: start =========================== #

    ht = """<html>"""

    # ========================= HTML: style =========================== #

    ht+= '''
    <head>
    <style>

    h1 {
        color: #00ff00;
        font-weight: bold;
        text-decoration: underline;
        font-size: 30px;
        font-family: "courier new", courier, monospace;
    }

    h2 {
        color: #00ff00;
        font-weight: bold;
        font-size: 30px;
        font-family: "courier new", courier, monospace;
    }

    img {
        display: block;
        padding: 2px 1px;
        margin-left: auto;
        margin-right: auto;
        width:90%; 
    }

    pre {
        font-family: "courier new", courier, monospace;
        font-size: 20px;
        color: #FFC310;
    }

    body {
        background-color: #014E33;
        font-family: "Lucida Console", Monaco, monospace
    }

    hr {
        display: block;
        height: 5px;
        border: 0;
        border-top: 2px solid #ccc;
        margin: 1em 0;
        padding: 0;
    }

    .padtop {
        padding-top: 10px;
        padding-bottom: 0px;
        text-decoration: underline;
    }

    .bordered {
        padding: 0px;
        border:  3px solid black;
    }

    .warnbord {
        padding: 0px;
        border:  3px solid black;
        color: #000000;
        background-color: #FFC3C4;
        padding-left: 20px;
        margin-left: auto;
        margin-right: auto;
        width:90%; 
    }

    .datbord {
        padding: 0px;
        border:  3px solid black;
        color: #000000;
        background-color: #ffffff;
        padding-left: 20px;
        margin-left: auto;
        margin-right: auto;
        width:90%; 
    }

    .container {
        text-align: center;
        padding-top: 0px;
        margin: 3px;
    }
    
    .container pre {
        margin: 3px;
        padding-top: 0px;
        display: inline-block;
        text-align: left;
        font-family: "courier new", courier, monospace;
        font-size: 20px;
        color: #FFC310;
    }

    </style>
    '''

    # light gray background?    background-color: #DCDCDC;

    # ========================= HTML: title =========================== #

    page_title    = lat.dir_dat + '/_page_title.dat'
    ptitle, ptext = lah.read_descrip_txt(page_title)

    ht+= """

    <h1><center> {} <center></h1>
    <h2><center> {} <center></h2>

    </head>
    """.format(ptitle, ptext)

    # ========================= HTML: body =========================== #

    ht += """
    <body>
    """

    # ---------------- get images with any associated text ----------------

    # all images
    imglob       = lat.dir_img + '/IMG*jpg'
    list_imglob  = glob.glob(imglob)
    txtglob      = lat.dir_img + '/IMG*txt'
    list_txtglob = glob.glob(txtglob)

    # we don't want the 'cor' ones, for space considerations
    list_imgs = []
    for x in list_imglob:
        if not(x.__contains__('.cor.jpg')):
            list_imgs.append(x)
    list_imgs.sort()

    #print(list_imgs)

    # for each image, check if it has associated text to place above; glue
    # in that text (if any), and then the image.
    for img in list_imgs:

        imgtxt = img.replace('.jpg', '.txt')
        if list_txtglob.__contains__(imgtxt):

            # start with a line of separation
            ht+='''\n\n<hr/>\n'''   
            title, text = lah.read_descrip_txt(imgtxt)
            ht+= lah.wrap_image_title( title,
                                       vpad=1,
                                       addclass=" class='padtop' " )
            if text :
                ht+= lah.wrap_image_txt( text,
                                  addclass=" class='container' " )
                #ht+= lah.wrap_image_txt( text,
                #                  addclass=" class='centerbox_leftjust' " )
        if img.__contains__('_1D_'):
            ht+=lah.wrap_img( img, vpad=True,
                              addclass=" class='bordered' ")
        else:
            ht+=lah.wrap_img(img, vpad=True)

    # ---------------------------------------------------------------------
    #   get text warnings (at bottom because they can be variable length
    # ---------------------------------------------------------------------

    # all dat/txt files
    datglob       = lat.dir_dat + '/WARN*dat'
    list_datglob  = glob.glob(datglob)
    txtglob2      = lat.dir_dat + '/WARN*txt'
    list_txtglob2 = glob.glob(txtglob2)

    list_datglob.sort()
    print('')
    print(list_datglob)

    for dat in list_datglob:
        dattxt = dat.replace('.dat', '.txt')
        if list_txtglob2.__contains__(dattxt):
            # start with a line of separation
            ht+='''\n\n<hr/>\n'''   
            title, text = lah.read_descrip_txt(dattxt)
            ht+= lah.wrap_image_title( title,
                                       addclass=" class='padtop' " )
            if text :
                ht+= lah.wrap_image_txt( text,
                                  addclass=" class='container' " )
        if 1 :
            ht+=lah.wrap_dat( lah.read_dat(dat),
                              addclass=" class='warnbord' ")

    # --------------------------- text info -------------------------------

    # all dat/txt files
    datglob       = lat.dir_dat + '/TXT*dat'
    list_datglob  = glob.glob(datglob)
    txtglob2      = lat.dir_dat + '/TXT*txt'
    list_txtglob2 = glob.glob(txtglob2)

    list_datglob.sort()
    #print('')
    #print(list_datglob)

    for dat in list_datglob:
        dattxt = dat.replace('.dat', '.txt')
        if list_txtglob2.__contains__(dattxt):
            # start with a line of separation
            ht+='''\n\n<hr/>\n'''   
            title, text = lah.read_descrip_txt(dattxt)
            ht+= lah.wrap_image_title( title,
                                       addclass=" class='padtop' " )
            if text :
                ht+= lah.wrap_image_txt( text,
                                  addclass=" class='container' " )
        if 1 :
            ht+=lah.wrap_dat( lah.read_dat(dat),
                              addclass=" class='datbord' ")

    # -------------- done: wrap up and close body text ------------------
    ht+="""</body>\n\n</html>"""

    # ------------- write ----------------
    fff = open(ohtml, 'w')
    fff.write(ht)
    fff.close()

    os.chdir(my_cwd)

    # exit, pursued by a bear
    sys.exit(0)
