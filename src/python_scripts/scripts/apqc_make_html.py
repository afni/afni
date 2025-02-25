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
#ver = '1.7' ; date = 'Nov 23, 2018' ; auth = 'PA Taylor'
# + for each image/section, will now have json files that describe
#   the contents of the title/text/subtext, etc.
#   this will allow for easier button definition
# + pages get jumped to correctly now
# + hyperrefs have abbrevs and links to top of page, where a navigator
#   menu sits (permanently) 
#
#ver = '1.7' ; date = 'May 17, 2019' 
# + [PT] simplifying pbar behavior
#
#ver = '1.8' ; date = 'May 22, 2019' 
# + [PT] help updates
# + [PT] warning level stuff
#
#ver = '2.3' ; date = 'July 3, 2019' 
# + [PT] Colorbars standard widths
# + [PT] QC block ID now in QC block titles
# + [PT] added more help descriptions
#
#ver = '2.31' ; date = 'July 17, 2019' 
# + [PT] tiny tweak in departurating message: guard against dreaded
#        double slash
#
#ver = '2.4' ; date = 'March 27, 2020' 
# [PT] remove dependency on lib_apqc_html_helps.py
#    + dir_img now from lah
#
#ver = '2.41' ; date = 'Oct 22, 2021' 
# [PT] better path reporting for the "done" message, hopefully
#
ver = '2.5' ; date = 'Jan 18, 2022' 
# [PT] add link buttons
#    + particular case: mecho QC block, m_tedana link buttons
#
#########################################################################

import os
import sys
import glob
import json

from afnipy import lib_apqc_html       as lah
from afnipy import lib_apqc_html_css   as lahc
from afnipy import lib_apqc_tcsh       as lat
from afnipy import lib_apqc_io         as laio

# ------------------------------------------------------------------------

ohtml     = lat.ohtml                # output file, HTML page
ocss      = lat.dir_info + '/styles.css' # CSS of formats/attributes/etc.
ohelp     = 'help.html'              # output help file, also html
oids      = lat.dir_info + '/list_ids.txt' # list of jump_to-able IDs
tobetable = "IHAVEACUNNINGPLAN!"     # string to be replaced later
ftypes    = [ 'jpg', 'dat', 'txt' ]  # types of data to populate HTML page
allblocks = lah.qc_blocks.keys()    # SHOULD be ordered list of QC blocks

MAX_WLEVEL      = ''
MAX_WLEVEL_RANK =  lahc.wlevel_ranks[MAX_WLEVEL]

# =========================================================================

if __name__ == "__main__":

    # parse inputs, and get current dir (to return to at end)
    iopts  = laio.parse_html_args(sys.argv[1:])
    my_cwd = os.getcwd()
    list_links = []                   # will hold list of links

    # move to subj qc dir
    os.chdir(iopts.qcdir)

    # get dictionary form of json, title page info to get subj ID
    fname = lah.dir_img + '/' + lat.page_title_json + '.json'
    with open( fname, 'r' ) as fff:
        titlepg_dict = json.load(fff)    

    # output JSON file, for rating/comments. Now includes SUBJ ID in
    # it, for easier identification
    oapqcjson = 'apqc_{}.json'.format( titlepg_dict['subj'] )

    # make a list of lists: each sublist is either 'blockid' or
    # 'itemid', and then the id. E.g.: ['blockid', 'regr'], 
    # ['itemid', 'tsnr_vreg']
    list_ids = []

    # ========================= HTML: start =========================== #

    ht = """<html>"""

    # ========================= HTML: style =========================== #

    # [PT: Jan 14, 2019] Have finally moved CSS attributes to their
    # own, external CSS file.  They grow up soooo fast... 
    ht+= '''

<!-- START of pre-QC-block part -->
<head>
<title>{subj}</title>
<link rel="stylesheet" type="text/css" href="{ocss}" />
<link rel="icon" type="icon.svg" href="extra_info/apqc_logo_main.svg"> 
'''.format( subj=titlepg_dict['subj'], ocss=ocss )

    # javascript functions
    ht+= lah.make_javascript_btn_func( titlepg_dict['subj'] )
    
    
    # ========================= HTML: title =========================== #

    # Offset when jumping to div IDs with #id.  Value comes from: 
    # navbar height = 70px
    # border bottom =  3px
    # padding top   =  2px
    # [other]       =  5px
    PADMARG_VAL = 80 


    page_title    = lah.dir_img + '/' + lat.page_title_json + '.json'
    # 'AATI' = all APQC title info.
    AATI = lah.read_title_json(page_title)

    ht+= '''
    {}
    '''.format( tobetable ) 

    # NB: someday, xstudy can be the task_name, likely from a uvar and
    # field in page_title_json
    ht+= lah.wrap_page_title( AATI.title, AATI.subj,
                              xstudy='task_name',
                              vpad=1,
                              blockid=AATI.blockid,
                              padmarg=PADMARG_VAL )
    list_links.append( [AATI.blockid, AATI.blockid_hov] )

    ht+= '''</head> <!-- END of pre-QC-block part -->
'''

    # ========================= HTML: body =========================== #

    ht += '''
<body onload="RunAtStart()">
    '''

    # ---------------------------------------------------------------------
    # ---------------- get images with any associated text ----------------
    # ---------------------------------------------------------------------

    # First, find ALL images and jsons, and then we'll exclude some
    # because they are supplementary sub-images and not independent
    # ones (like the *.cor.*, *.sag.* and *pbar* ones)
    DID_START_QC_BLOCKS = False
    list_allglob = []
    for ff in ftypes:
        list_allglob += glob.glob(lah.dir_img + '/*.' + ff)
    list_jsonglob = glob.glob(lah.dir_img + '/*.json')

    #print(list_allglob)
    #print(list_jsonglob)

    # we don't want the 'cor' ones, for space considerations;
    # and now, we don't want the colorbars *here*, either-- will
    # read those in based on the names of *axi.jpg.
    list_use = []
    for x in list_allglob:
        if not(x.__contains__('.cor.jpg') or x.__contains__('.pbar.jpg')) :
            list_use.append(x)
    list_use.sort()

    #print(list_use)
    
    # for each QC block
    for qcb in allblocks:

        # find the QC items that belong to it, by parsing names
        list_qci = []
        for img in list_use:
            if img.__contains__('_' + qcb + '_') :
                list_qci.append(img)

        Nqui = len(list_qci)

        # loop through any
        for ii in range(Nqui):
            img = list_qci[ii]
            # get extension of file, must be one of ftypes (how
            # could it not be??)
            img_ext  = img.split('.')[-1] 
            img_json = img.replace(img_ext, 'json')
            
            # 'AAII' = 'all APQC item info'
            # initialize empty, may not need/use
            AAII = lah.apqc_item_info()
            if list_jsonglob.__contains__(img_json) :
                AAII = lah.read_descrip_json(img_json)

            # 1) Try to get title+text+blockid, only for first one in
            # list
            if AAII.title and not(ii):
                if DID_START_QC_BLOCKS :    dcpd = True
                else:                       dcpd = False
                ht+= lah.wrap_block_title( AAII.title,
                                           vpad=1,
                                           addclass=" class='padtop' ",
                                           blockid=AAII.blockid,
                                           padmarg=PADMARG_VAL,
                                           do_close_prev_div=dcpd )
                list_links.append( [AAII.blockid, AAII.blockid_hov] )
                DID_START_QC_BLOCKS = True
                list_ids.append( ['blockid', AAII.blockid] )
                list_ids.append( ['itemid', AAII.itemid] )

            # 2) Try to add text above it
            if AAII.text :
                ht+= lah.wrap_block_text( AAII.text,
                                          addclass="class='container' ",
                                          itemid=AAII.itemid,
                                          padmarg=PADMARG_VAL,
                                          vpad=1 )
                if AAII.itemid != list_ids[-1][1] :
                    list_ids.append( ['itemid', AAII.itemid] )

            # 3) Try to add image or dat
            if AAII.itemtype == '1D':
                # put a border around this type
                ht+=lah.wrap_img( img, vpad=True,
                                  addclass=" class='bordered' " )
            elif AAII.itemtype == 'VOL':
                # decide when to put AV/NV buttons (or not)
                if AAII.text and AAII.itemid != 'olap' :
                    add_nvbtn = True
                else:
                    add_nvbtn = False
                ht+=lah.wrap_img( img, itemid=AAII.itemid, vpad=True, 
                                  add_nvbtn=add_nvbtn,
                                  av_file=AAII.av_file,
                                  ic_file=AAII.ic_file,
                                  ic_args=AAII.ic_args,
                                  gv_file=AAII.gv_file,
                                  gv_args=AAII.gv_args )

                if AAII.nv_html :
                    fname = lah.dir_img + '/' + AAII.nv_html
                    ht+= lah.wrap_nv_html(fname)
                if AAII.itemid != list_ids[-1][1] :
                    list_ids.append( ['itemid', AAII.itemid] )

            elif AAII.itemtype == 'WARN':
                ht+=lah.wrap_dat( lah.read_dat(img),
                                  addclass=" class='warnbord' ",
                                  warn_level = AAII.warn_level,
                                  remove_top_empty = True)
                if lahc.wlevel_ranks[AAII.warn_level] > MAX_WLEVEL_RANK :
                    MAX_WLEVEL = AAII.warn_level
                    MAX_WLEVEL_RANK = lahc.wlevel_ranks[MAX_WLEVEL]
                if AAII.itemid != list_ids[-1][1] :
                    list_ids.append( ['itemid', AAII.itemid] )

            elif AAII.itemtype == 'DAT':
                ht+=lah.wrap_dat( lah.read_dat(img),
                                  addclass=" class='datbord' ")
                if AAII.itemid != list_ids[-1][1] :
                    list_ids.append( ['itemid', AAII.itemid] )

            elif AAII.itemtype == 'BUTTON':
                ht+=lah.wrap_button( lah.read_dat(img),
                                     button_type=AAII.itemid )

            # 4) Add in any 'subtext'
            if AAII.subtext :
                ht+= lah.wrap_block_text( AAII.subtext,
                                          addclass=" class='container2' ",
                                          dobold=True,
                                          vpad=2 )

    # ---------------------------------------------------------------------
    # -------------- put the nav link table in  ------------------
    # ---------------------------------------------------------------------

    # close final section div
    ht+= '''
</div> <!-- close of final QC block div -->
'''

    list_links.append( lah.qc_link_final )

    txt_for_navtable = lah.make_nav_table(list_links, subj=AATI.subj,
                                          max_wlevel = MAX_WLEVEL)
    ht               = ht.replace(tobetable, txt_for_navtable)

    # -------------- done: wrap up and close body text ------------------

    ht+='''
</body>
</html>
'''

    # ------------- write to file ----------------

    # main index.html output file
    fff = open(ohtml, 'w')
    fff.write(ht)
    fff.close()

    # write the external style file for the HTML files
    lahc.write_css_file( ocss ) 

    # at least here, the JSON has indentation
    lah.write_json_file(list_links, oapqcjson) 

    # output help html file; reuse same external CSS file
    lah.write_help_html_file( ohelp, ocss ) 

    # output list of IDs to a text file, to display 
    lah.write_list_ids_file( oids, list_ids ) 

    # silly check, so no doubling of slash in path (not harmful, but
    # annoyingly unaesthetic)
    path_qcdir = iopts.qcdir
    if path_qcdir[-1] == '/' :
        path_qcdir = iopts.qcdir[:-1]

    # [PT: Oct 22, 2021] update the 'done' message path, because the
    # originally-reported relative path is often not useful.  here,
    # get abs path to current dir, which should be QC_*/, because of
    # os.chdir(..)  above
    pwd_res    = os.getcwd()
    qcfile_abs = pwd_res + '/' + ohtml

    bye_msg = """
++ Done! Wrote QC HTML.
   To view, run either this (without server):

       afni_open -b {qcfile_abs}

   ... or this (with server):

       open_apqc.py -infiles {qcfile_abs}

""".format(qcfile_abs=qcfile_abs)


    print(bye_msg)

    os.chdir(my_cwd)

    # exit, pursued by a bear
    sys.exit(0)


