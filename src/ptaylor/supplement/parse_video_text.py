#!/usr/bin/env python

import os, sys
import lib_videos as lv

# 'https://afni.nimh.nih.gov/pub/dist/edu/class_lectures/2020-03-NIH/' + DIRECTORY

if __name__ == "__main__" :

    # parse input args
    iopts = lv.parse_args_videos(sys.argv)

    # do all the work
    vo = lv.vid_txt( iopts.movie,
                     aweb_dir=iopts.web_pref,
                     verify_weblinks=iopts.do_verify_weblinks )


    # for debugging, in either py2 or py3
    if iopts.do_disp_obj :
        try:
            for attr, value in vo.__dict__.items():
                print("{:20s} : {}".format(attr, value))
        except:
            for attr, value in vo.__dict__.iteritems():
                print("{:20s} : {}".format(attr, value))


    # main outputs
    out_youtube_details = '\n'.join(['', 
                                     vo.dd['title'],
                                     '',
                                     ''.join(vo.details_yt),
                                     ''])

    if iopts.prefix_yt :
        f = open(iopts.prefix_yt, 'w')
        f.write(out_youtube_details)
        f.close()
    else:
        print(out_youtube_details)

