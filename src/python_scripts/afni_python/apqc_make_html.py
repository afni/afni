#!/usr/bin/env python
#
# ver : 1.3 || date: Oct 17, 2018 || auth: PA Taylor
# + new container/div for centering text box (while left-ifying text)
#
# ver : 1.4 || date: Oct 19, 2018 
# + parse inp opts
#
# ver : 1.5 || date: Nov 1, 2018 
# + check for "subtext" files-- things to be put beneath IMG/TXT/WARN for
#   extra info.
#
# ver : 1.5 || date: Nov 20, 2018 
# + put in container2 to have gray subtext (differentiate from titles)
# + make all subtxt be BOLD
# + fixed making the whole image line *not* being a link, while also
#   having it be centered
#
ver = '1.7' ; date = 'Nov 23, 2018' ; auth = 'PA Taylor'
# + for each image/section, will now have json files that describe
#   the contents of the title/text/subtext, etc.
#   this will allow for easier button definition
# + pages get jumped to correctly now
# + hyperrefs have abbrevs and links to top of page, where a navigator
#   menu sits (permanently) 
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

ohtml     = 'index.html'
tobetable = "IHAVEACUNNINGPLAN!"

# =========================================================================
if __name__ == "__main__":

    # parse inputs, and get current dir (to return to at end)
    iopts  = laio.parse_html_args(sys.argv[1:])
    my_cwd = os.getcwd()
    list_links = []                   # will hold list of links

    # move to subj qc dir
    os.chdir(iopts.qcdir)

    # ========================= HTML: start =========================== #

    ht = """<html>"""

    # ========================= HTML: style =========================== #

    ht+= '''
<head>
<style>

h1 {
    padding-top: 38px;
    padding-bottom: 0px;
    color: #ccc;
    font-weight: bold;
    text-decoration: underline;
    font-size: 26px;
    font-family: "courier new", courier, monospace;
}

h2 {
    padding-top: 0px;
    padding-bottom: 0px;
    color: #00ff00;
    font-weight: bold;
    font-size: 26px;
    font-family: "courier new", courier, monospace;
}

img {
    padding: 2px 1px;
    display: inline-block;
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
    margin: 4px 0;
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
    white-space: pre;
    display: inline-block;
    text-align: left;
    font-family: "courier new", courier, monospace;
    font-size: 20px;
    color: #FFC310;
}

.container2 {
    text-align: center;
    padding-top: 0px;
    margin: 3px;
}

.container2 pre {
    margin: 3px;
    padding-top: 0px;
    white-space: pre;
    display: inline-block;
    text-align: left;
    font-family: "courier new", courier, monospace;
    font-size: 20px;
    color: #ccc;
}

img.pbar {
    width: 20rem;
    height: 1rem;
    vertical-align: "middle";
}

ul {
    list-style-type: none;
    margin: 0;
    padding: 0;
    overflow: hidden;
    background-color: #444;
    position: fixed;
    top: 0;
    width: 100%;
    border-bottom: 2px solid #ccc;
    font-family: "courier new", courier, monospace;
    font-size: 20px;
    color: #ccc;
}

li {
    float: left;
    border-right: 1px solid #bbb;
}

li:last-child {
    border-right: none;
} 

li a {
    display: block;
    color: white;
    text-align: center;
    padding: 8px 8px;
    text-decoration: none;
}

li a:hover:not(.active) {
    background-color: #111;
    color: yellow;
}

li a:active {
    background-color: #4CAF50;
    color: green;
}

    </style>
    '''

    # Notes on above:
    #
    # + need to use "display: inline-block" in the img { } def in
    #   order to not have the whole line be a clickable link
    #
    # + .container and .container2 are the same except for the font
    #   colors in each's "pre"; at this present time, that is to have
    #   "subtxt" be gray, and header stuff be yellow.
    # 
    # + In the .container pre { } stuff, one could add the following
    #   to have wrapping on long lines (though it won't help too much
    #   with long censor lists):
    #   white-space: pre;
    # 
    # + Move someday to light gray background?  background-color:
    #   #DCDCDC;
    # 
    # + Re. the navigation bar: at the moment, the text height is
    #   20px, and the vertical margins on it are 8px, so the total
    #   height is 36 px; additionally, there is a 2px line beneath it,
    #   for a total of 38px. That total navbar height determines the
    #   amount of padding needed above the title text per section for
    #   jumping around, which is actually done for most parts in
    #   lah.wrap_image_title().
    
    # ========================= HTML: title =========================== #

    page_title    = lat.dir_dat + '/' + lat.page_title_json + '.json'
    # 'AATI' = all APQC title info.
    AATI = lah.read_title_json(page_title)
    #!!!!!!!!!!ptitle, ptext = lah.read_descrip_txt(page_title)

    list_links.append([AATI.linkid, AATI.linkid_hov])

    ht+= """
    {}

    <div id="{}" style="padding-top: 38px; margin-top: -38px;">
    <h1><center> {} <center></h1></div>
    <h2><center> {} <center></h2>
    <h2><center> {} <center></h2>

    </head>
    """.format( tobetable, 
                AATI.linkid, AATI.title, 
                "[taskname]", AATI.subj )

    # ========================= HTML: body =========================== #

    ht += """
    <body>
    """

    # ---------------------------------------------------------------------
    # ---------------- get images with any associated text ----------------
    # ---------------------------------------------------------------------

    # all images
    imglob        = lat.dir_img + '/IMG*jpg'
    list_imglob   = glob.glob(imglob)
    jsonglob      = lat.dir_img + '/IMG*json'
    list_jsonglob = glob.glob(jsonglob)

    # NB: The initial "list_imglob" above is a list of all *possible*
    # images, but might contain ones we don't want; some we will
    # discard, others we will use later.

    # we don't want the 'cor' ones, for space considerations;
    # and now, we don't want the colorbars *here*, either-- will
    # read those in based on the names of *axi.jpg.
    list_imgs = []
    for x in list_imglob:
        if not(x.__contains__('.cor.jpg') or x.__contains__('.pbar.jpg')):
            list_imgs.append(x)
    list_imgs.sort()

    #print(list_imgs)

    # for each image, check if it has associated text to place above; glue
    # in that text (if any), and then the image.
    for img in list_imgs:

        # each new block of images gets a text header and a line that
        # separates it from before; each block can contain more than
        # one row of images/content
        imgjson = img.replace('.jpg', '.json')

        # 'AAII' = all APQC item info.
        # initialize empty, may not need/use
        AAII = lah.apqc_item_info()
        if list_jsonglob.__contains__(imgjson):
            AAII = lah.read_descrip_json(imgjson)

        # 1) Try to get title+text+linkid
        if AAII.title :
            # start with a line of separation
            ht+='''\n\n<hr/>\n'''   
            ht+= lah.wrap_image_title( AAII.title,
                                       vpad=1,
                                       addclass=" class='padtop' ",
                                       linkid=AAII.linkid )
            list_links.append( [AAII.linkid, AAII.linkid_hov] )

        if AAII.text :
            ht+= lah.wrap_image_txt( AAII.text,
                                     addclass=" class='container' " )

        # 2) Add in image
        if img.__contains__('_1D_'):
            # put a border around this type
            ht+=lah.wrap_img( img, vpad=True,
                              addclass=" class='bordered' ")
        else:
            ht+=lah.wrap_img( img, vpad=True )

        # 2b) Check specifically for colorbar
        if img.__contains__('sag.jpg'):
            imgpbar = img.replace('sag.jpg', 'pbar.jpg')
            if list_imglob.__contains__(imgpbar):

                # if that pbar image exists, it should have an associated
                # txt file of ranges that we can get like this
                jsonpbar = imgpbar.replace('jpg', 'json')
                pbar_dict = lah.read_json_to_dict(jsonpbar)
                if pbar_dict:
                    pbar_line = lah.make_pbar_line( pbar_dict, imgpbar,
                                           addclassdiv=" class='container2' ",
                                           addclassimg=" class='pbar' ",
                                           dobold=True)
                ht+= pbar_line

        # 3) Add in any 'subtext'
        if AAII.subtext :
            ht+= lah.wrap_image_txt( AAII.subtext,
                                     addclass=" class='container2' ",
                                     dobold=True )

    # ---------------------------------------------------------------------
    #   get text warnings (at bottom because they can be variable length
    # ---------------------------------------------------------------------

    # all dat/txt files
    datglob        = lat.dir_dat + '/WARN*dat'
    list_datglob   = glob.glob(datglob)
    jsonglob2      = lat.dir_dat + '/WARN*json'
    list_jsonglob2 = glob.glob(jsonglob2)

    list_datglob.sort()

    for dat in list_datglob:
        datjson = dat.replace('.dat', '.json')

        # 'AAII' = all APQC item info.
        # initialize empty, may not need/use
        AAII = lah.apqc_item_info()
        if list_jsonglob2.__contains__(datjson):
            AAII = lah.read_descrip_json(datjson)

        # start with a line of separation
        ht+='''\n\n<hr/>\n'''   

        # 1) Try to get title+text+linkid
        if AAII.title :
            ht+= lah.wrap_image_title( AAII.title,
                                       vpad=1,
                                       addclass=" class='padtop' ",
                                       linkid=AAII.linkid )
            list_links.append( [AAII.linkid, AAII.linkid_hov] )
        if AAII.text :
            ht+= lah.wrap_image_txt( AAII.text,
                                     addclass=" class='container' " )


        # 2) Add in the actual data file
        if 1 :
            ht+=lah.wrap_dat( lah.read_dat(dat),
                              addclass=" class='warnbord' ")

        # 3) Add in any 'subtext'
        if AAII.subtext :
            ht+= lah.wrap_image_txt( AAII.subtext,
                                     addclass=" class='container2' ",
                                     dobold=True )

    # ---------------------------------------------------------------------
    # --------------------------- text info -------------------------------
    # ---------------------------------------------------------------------

    # all dat/txt files
    datglob        = lat.dir_dat + '/TXT*dat'
    list_datglob   = glob.glob(datglob)
    jsonglob2      = lat.dir_dat + '/TXT*json'
    list_jsonglob2 = glob.glob(jsonglob2)

    list_datglob.sort()

    for dat in list_datglob:
        datjson   = dat.replace('.dat', '.json')

        # 'AAII' = all APQC item info.
        # initialize empty, may not need/use
        AAII = lah.apqc_item_info()
        if list_jsonglob2.__contains__(datjson):
            AAII = lah.read_descrip_json(datjson)

        # start with a line of separation
        ht+='''\n\n<hr/>\n'''   

        # 1) Try to get title+text+linkid
        if AAII.title :
            ht+= lah.wrap_image_title( AAII.title,
                                       vpad=1,
                                       addclass=" class='padtop' ",
                                       linkid=AAII.linkid )
            list_links.append( [AAII.linkid, AAII.linkid_hov] )

        if AAII.text :
            ht+= lah.wrap_image_txt( AAII.text,
                                     addclass=" class='container' " )

        # 2) Add in the actual data file
        if 1 :
            ht+=lah.wrap_dat( lah.read_dat(dat),
                              addclass=" class='datbord' ")

        # 3) Add in any 'subtext'
        if AAII.subtext :
            ht+= lah.wrap_image_txt( AAII.subtext,
                                     addclass=" class='container2' ",
                                     dobold=True )


    # ---------------------------------------------------------------------
    # -------------- put the nav link table in  ------------------
    # ---------------------------------------------------------------------

    txt_for_navtable = lah.make_nav_table(list_links)
    ht = ht.replace(tobetable, txt_for_navtable)


    # -------------- done: wrap up and close body text ------------------
    ht+="""</body>\n\n</html>"""


    # ------------- write to file ----------------
    fff = open(ohtml, 'w')
    fff.write(ht)
    fff.close()
    print('\n++ Done! Wrote QC HTML: {}/{}\n'.format(iopts.qcdir, ohtml))
    os.chdir(my_cwd)

    # exit, pursued by a bear
    sys.exit(0)
