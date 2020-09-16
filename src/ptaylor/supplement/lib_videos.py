# library functions for managing the video recordings and their
# accompany text files.

import os, sys
import requests

# ----------------------------------------------------------------------------
auth = 'PA Taylor'
#ver  = '1.0' ; date = 'April 16, 2020'
#
#ver  = '1.01' ; date = 'April 16, 2020'
# [PT] verify scripts on the disk, and weblinks (by default)
#
ver = '1.2' ; date = 'April 16, 2020'  
# [PT] bug fix: put setting weblinks earlier
# 
# ----------------------------------------------------------------------------

# assumed start of path for link:
ASOP = '/pub/dist/edu'
AWEB = 'https://afni.nimh.nih.gov'

help_string = '''

Parse text files accompanying movies for making youtube files.

'presentation' and 'scripts' can be a list of comma separated values
(yes, I know only one of those is a plural term, but that's how life
is).


By default, output is directed to terminal, being:
-----------------------------------------
TITLE

OUTLINE
-----------------------------------------
This can also be redirected to a text file, with the '-prefix_yt ..' opt.

ver  = {ver}
auth = {auth}
date = {date}

============================================================================

RUNNING:

req:

  -movie MOVIE_FILE  : name of movie file
  
opt:

  -prefix_yt PREF_YT : instead of outputting youtube details to
                       screen, write them to this file (def: output
                       printed in terminal)

  -web_pref WEB_PREF : name of web address to prepend to presentations
                       and scripts.  An example of a WEB_PREF might be:
          https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2020-03-NIH/afni_TimeSeriesAnalysis'
                       This opt is only useful if one is *not* running
                       this program on 'afni' (where the weblink can
                       be generated automatically from file paths)
    
   -verify_weblinks_off : by default, this prog will check that the purported
                       weblinks to scripts/presentations work; this opt
                       would disable that functionality

   -disp_obj         : just a debugging option to see all the object values

--------------------------------------------------------------------------------

EXAMPLES

   1) example of running on afni:
      parse_video_text.py                         \\
         -movie afni_TimeSeriesAnalysis.01.mp4    \\
         -prefix_yt  output.txt

   1) example of running on *not* afni:
      parse_video_text.py                         \\
         -movie afni_TimeSeriesAnalysis.01.mp4    \\
         -prefix_yt  output.txt                   \\
         -web_pref 'https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2020-03-NIH/afni_TimeSeriesAnalysis' 


'''.format( ver=ver, auth=auth, date=date )
 
# =============================================================================

def ARG_missing_arg(arg):
    print("** ERROR: missing argument after option flag: {}".format(arg))
    sys.exit(1)

class iopts_videos:

    def __init__(self ) :

        self.movie        = ''

        self.prefix_yt    = ''

        self.web_pref     = ''
        self.do_fancy_out = False
        self.do_disp_obj  = False   # just for debugging
        self.do_verify_weblinks = True

    def set_movie(self, ss):
        self.movie = ss

    def set_prefix_yt(self, ss):
        self.prefix_yt = ss

    def set_web_pref(self, ss):
        self.web_pref = ss

    def set_fancy_out( self, LL ):
        if LL :
            self.do_fancy_out = True
        else:
            self.do_fancy_out = False

    def set_disp_obj( self, LL ):
        if LL :
            self.do_disp_obj = True
        else:
            self.do_disp_obj = False

    def set_verify_weblinks( self, LL ):
        if LL :
            self.do_verify_weblinks = True
        else:
            self.do_verify_weblinks = False

    def check_req(self):
        ''' Check for and point out any missing inputs.'''
        MISS = 0

        if not(self.movie) :
            print("** ERROR: missing '-movie' dset")
            MISS+=1

        return MISS


def parse_args_videos(full_argv):

    argv = full_argv[1:]
    Narg = len(argv)

    if not(Narg):
        print(help_string)
        sys.exit(0)

    iopts  = iopts_videos()

    i = 0
    while i < Narg:
        if  argv[i] == "-help" or \
            argv[i] == "-h" :
            print(help_string)
            sys.exit(0)

        elif argv[i] == "-movie" :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_movie( argv[i] )


        elif argv[i] == "-prefix_yt" :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_prefix_yt( argv[i] )

        elif argv[i] == "-web_pref" :
            if i >= Narg:
                ARG_missing_arg(argv[i])
            i+= 1
            iopts.set_web_pref( argv[i] )

        elif argv[i] == "-fancy_out" :
            iopts.set_fancy_out(True)

        elif argv[i] == "-disp_obj" :
            iopts.set_disp_obj(True)

        elif argv[i] == "-verify_weblinks_off" :
            iopts.set_verify_weblinks(False)

        # --------- finish -------------

        else:
            print("** ERROR: unknown opt: '{}'".format(argv[i]))
            sys.exit(2)
        i+=1

    if iopts.check_req():
        print("   -------------------------------")
        print("** ERROR with input arguments (see detailed whining above).")
        sys.exit(1)

    return iopts

# =============================================================================

class vid_txt:

    def __init__(self, vname, aweb_dir='', verify_weblinks=True):

        # keywords for types of input items; can grow over time
        #self.title        = ""
        #self.presentation = []
        #self.scripts      = []
        #self.date         = ""
        #self.speaker      = ""
        #self.outline      = []
        # ... all the keywords now just dictionary items
        self.dd           = {}  # dictionary to read all into

        self.aweb_dir     = aweb_dir

        # video name and info
        self.vname        = ""
        self.vbase        = ""
        self.vext         = ""
        self.vdir         = ""

        # txt file name
        self.fname        = ""

        self.do_verify_weblinks = verify_weblinks
        self.raw_text     = []
        self.details_yt   = [] 

        # ----------- work: do these things --------------------------

        self.get_info_videofile( vname )
        self.find_accompanying_textfile( )
        self.read_textfile( )
        self.text_to_dictionary( )

        if not(self.aweb_dir):
            self.fpath_to_link() 

        self.verify_script_and_pres()
        if self.do_verify_weblinks :
            self.check_weblinks()

        self.make_youtube_details( )
        
        # --------------------------------------------------------

    def get_info_videofile( self, vname ):
        '''Input: video file name (with rel/abs path)
        
        Output: all necessary path info for video file, stored in attr

        '''

        if os.path.exists(vname) :
            self.vname = os.path.basename(vname)

        # path info: dir containing
        apath        = os.path.abspath(vname)
        self.vdir    = '/'.join(apath.split('/')[:-1])

        # remove extension, parse
        vsplit     = self.vname.split('.')
        self.vbase = '.'.join(vsplit[:-1])
        self.vext  = vsplit[-1]


    def fpath_to_link(self ):
        '''Convert full path of file on 'afni' comp sys to a https link'''

        if not(self.aweb_dir) :
            # assume we are on system with files, so use locations to
            # build World-facing path, per usual
            try:
                idx = self.vdir.index(ASOP)
            except:
                print("** ERROR: looks like user needs to provide their own "
                      " link to webdir?\n"
                      "   Can't find:\n"
                      "       {}\n"
                      "   as part of file path:\n"
                      "       {}\n".format( ASOP, self.vdir ))
                raise

            self.aweb_dir = AWEB + self.vdir[idx:]

        else:
            print("** ERROR: looks like path name was actually provided?:\n"
                  "   {}".format( self.aweb_dir ))
            sys.exit(3)

    def find_accompanying_textfile( self ):
        '''Input: obj that must have video info known
        
        Output: all necessary path info for text file, stored in attr

        '''

        # find accompanying text info: guess at name
        fguess = self.vdir + '/' + self.vbase + '.' + 'txt'
        if os.path.exists(fguess) :
            self.fname = os.path.basename(fguess)
        else:
            print("** ERROR: can't find text file for this vid:\n"
                  "   vname  : {}\n   fguess : {}".format(vname, fguess))
            sys.exit(1)

    def read_textfile ( self ):
        ''' Open and read text file into raw text list'''

        ffull = '/'.join([self.vdir, self.fname])
        fff   = open(ffull, 'r')
        self.raw_text = fff.readlines()
        fff.close()
        
    def text_to_dictionary( self ):
        '''Convert the text file (now self.raw_text list) to a dictionary of
useful/parsed items

        '''

        OPEN_OUTLINE = 0
        rowidx = -1
        for row in self.raw_text:
            rowidx +=1 
            
            if OPEN_OUTLINE :
                kw = 'outline'
                self.dd[kw].append(row) # keep newline chars


            else:
                # each non-outline entry line should have a '::' separator
                Nsep = row.count("::")

                if Nsep != 1:
                    print("** ERROR: expected 'keyword :: value' format\n"
                          "   but got something weird in line {}:\n"
                          "   {}".format(rowidx, row))
                    sys.exit(2)

                x   = row.split("::") # at this point, we know we only have 1 sep
                kw  = x[0].strip()
                val = x[1].strip()

                if ['title', 'date', 'speaker'].__contains__(kw) :
                    # items with simple/single parsing
                    self.dd[kw] = val

                elif ['presentation', 'scripts'].__contains__(kw) :
                    # items with possible lists of entries
                    if val :
                        self.dd[kw] = [x.strip() for x in val.split(',')]
                    else:
                        self.dd[kw] = []

                elif ['outline'].__contains__(kw) :
                    # only 1 thing to be done for this line
                    OPEN_OUTLINE = 1  
                    self.dd[kw]  = []

                else:
                    print("** ERROR: unknown happenstance in line {}:\n"
                          "   {}".format(rowidx, row))
                    sys.exit(2)

    def verify_script_and_pres( self ):
        '''Make sure input script names and things are correct'''

        Npres   = len(self.dd['presentation'])
        Nscript = len(self.dd['scripts'])

        count_bad = 0
        list_bad  = []

        for ii in range(Npres):        
            ppp = self.vdir + '/' + self.dd['presentation'][ii]
            if not(os.path.exists(ppp)):
                count_bad+= 1
                list_bad.append(ppp)

        for ii in range(Nscript):        
            ppp = self.vdir + '/' + self.dd['scripts'][ii]
            if not(os.path.exists(ppp)):
                count_bad+= 1
                list_bad.append(ppp)

        if count_bad :
            print("** ERROR: can't find the following {} pres/scripts:"
                  "".format(count_bad))
            for bb in list_bad:
                print('     {}'.format(bb))
            print("")
            sys.exit(11)

    def check_weblinks( self ):

        Npres   = len(self.dd['presentation'])
        Nscript = len(self.dd['scripts'])

        count_bad = 0
        list_bad  = []

        for ii in range(Npres):        
            ppp = self.aweb_dir + '/' + self.dd['presentation'][ii]
            my_request = requests.get(ppp)
            if my_request.status_code != 200 : 
                count_bad+= 1
                list_bad.append(ppp)

        for ii in range(Nscript):        
            ppp = self.aweb_dir + '/' + self.dd['scripts'][ii]
            my_request = requests.get(ppp)
            if my_request.status_code != 200 : 
                count_bad+= 1
                list_bad.append(ppp)

        if count_bad :
            print("** ERROR: weblinks failed for the following {} pres/scripts:"
                  "".format(count_bad))
            for bb in list_bad:
                print('     {}'.format(bb))
            print("")
            sys.exit(11)


    def make_youtube_details( self ):
        '''Take dictionary of items and filepath info to make details text for
youtube.

        '''

        self.details_yt = []

        for x in self.dd['outline']:
            self.details_yt.append(x)

        self.details_yt.append("\n")
        
        Npres   = len(self.dd['presentation'])
        Nscript = len(self.dd['scripts'])

        if Npres :
            ending = ''
            if Npres > 1:
                ending = 'S'
            self.details_yt.append( "PRESENTATION{}: \n".format(ending) )
            for ii in range(Npres):        
                str_pres = self.aweb_dir + '/' + self.dd['presentation'][ii]
                str_pres+= '\n'
                self.details_yt.append(str_pres)

        if Nscript :
            ending = ''
            if Nscript > 1:
                ending = 'S'
            self.details_yt.append( "SCRIPT{}: \n".format(ending) )
            for ii in range(Nscript):        
                str_pres = self.aweb_dir + '/' + self.dd['scripts'][ii]
                str_pres+= '\n'
                self.details_yt.append(str_pres)

        self.details_yt.append('\n-') # extra char at end for "empty" line
