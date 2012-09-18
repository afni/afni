from __future__ import with_statement

from datetime import timedelta
import sys
import time

def get_termsize():
    """Return terminal size as a tuple (height, width)."""
    try:
        # this works on unix machines
        import struct, fcntl, termios
        height, width = struct.unpack("hhhh",
                                      fcntl.ioctl(0,termios.TIOCGWINSZ,
                                                  "\000"*8))[0:2]
        if not (height and width):
            height, width = 24, 79
    except ImportError:
        # for windows machins, use default values
        # Does anyone know how to get the console size under windows?
        # One approach is:
        # http://code.activestate.com/recipes/440694/
        height, width = 24, 79
    return height, width

def fmt_time(t, delimiters):
    """Return time formatted as a timedelta object."""
    meta_t = timedelta(seconds=round(t))
    return ''.join([delimiters[0], str(meta_t), delimiters[1]])

def _progress(percent, last, style, layout):
    # percentage string
    percent_s = "%3d%%" % int(round(percent*100))
    if style == 'bar':
        # how many symbols for such percentage
        symbols = int(round(percent * layout['width']))
        # build percent done arrow
        done = ''.join([layout['char1']*(symbols), layout['char2']])
        # build remaining space
        todo = ''.join([layout['char3']*(layout['width']-symbols)])
        # build the progress bar
        box =  ''.join([layout['delimiters'][0],
                        done, todo,
                        layout['delimiters'][1]])
        if layout['position'] == 'left':
            # put percent left
            box = ''.join(['\r', layout['indent'], percent_s, box])
        elif layout['position'] == 'right':
            # put percent right
            box = ''.join(['\r', layout['indent'], box, percent_s])
        else:
            # put it in the center
            percent_s = percent_s.lstrip()
            percent_idx = (len(box) // 2) - len(percent_s) + 2
            box = ''.join(['\r', layout['indent'],
                           box[0:percent_idx],
                           percent_s,
                           box[percent_idx+len(percent_s):]])
    else:
        now = time.time()
        if percent == 0:
            # write the time box directly
            tbox = ''.join(['?', layout['separator'], '?'])
        else:
            # Elapsed
            elapsed = now - layout['t_start']
            # Estimated total time
            if layout['speed'] == 'mean':
                e_t_a = elapsed/percent - elapsed
            else:
                # instantaneous speed
                progress = percent-_progress.last_percent
                e_t_a = (1 - percent)/progress*(now-_progress.last_time)
            # build the time box
            tbox = ''.join([fmt_time(elapsed, layout['delimiters']),
                            layout['separator'],
                            fmt_time(e_t_a, layout['delimiters'])])
        # compose progress info box
        if layout['position'] == 'left':
            box = ''.join(['\r',
                           layout['indent'],
                           percent_s,
                           ' ',
                           tbox])
        else:
            box = ''.join(['\r',
                           layout['indent'],
                           tbox,
                           ' ',
                           percent_s])

        _progress.last_percent = percent
        _progress.last_time = now

    # print it only if something changed from last time
    if box != last:
        sys.stdout.write(box)
        sys.stdout.flush()
    return box

def progressinfo(sequence, length = None, style = 'bar', custom = None):
    """A fully configurable text-mode progress info box tailored to the
       command-line die-hards.

       To get a progress info box for your loops use it like this:

          >>> for i in progressinfo(sequence):
          ...     do_something(i)

      You can also use it with generators, files or any other iterable object,
      but in this case you have to specify the total length of the sequence:

          >>> for line in progressinfo(open_file, nlines):
          ...     do_something(line)

      If the number of iterations is not known in advance, you may prefer
      to iterate on the items directly. This can be useful for example if
      you are downloading a big file in a subprocess and want to monitor
      the progress. If the file to be downloaded is TOTAL bytes large and
      you are downloading it on local:
          >>> def done():
          ...     yield os.path.getsize(localfile)
          >>> for bytes in progressinfo(done(), -TOTAL)
          ...     time.sleep(1)
          ...     if download_process_has_finished():
          ...         break


     Arguments:

     sequence    - if it is a Python container object (list,
                   dict, string, etc...) and it supports the
                   __len__ method call, the length argument can
                   be omitted. If it is an iterator (generators,
                   file objects, etc...) the length argument must
                   be specified.

     Keyword arguments:

     length     - length of the sequence. Automatically set
                  if `sequence' has the __len__ method. If length is
                  negative, iterate on items.

     style      - If style == 'bar', display a progress bar. The
                  default layout is:

                  [===========60%===>.........]

                  If style == 'timer', display a time elapsed / time
                  remaining info box. The default layout is:

                  23% [02:01:28] - [00:12:37]

                  where fields have the following meaning:

                  percent_done% [time_elapsed] - [time_remaining]

     custom     - a dictionary for customizing the layout.
                  Default layout for the 'bar' style:
                   custom = { 'indent': '',
                              'width' : terminal_width - 1,
                              'position' : 'middle',
                              'delimiters' : '[]',
                              'char1' : '=',
                              'char2' : '>',
                              'char3' : '.' }


                  Default layout for the 'timer' style:
                   custom = { 'speed': 'mean',
                              'indent': '',
                              'position' : 'left',
                              'delimiters' : '[]',
                              'separator' : ' - ' }

                  Description:
                    speed = completion time estimation method, must be one of
                            ['mean', 'last']. 'mean' uses average speed, 'last'
                            uses last step speed.
                    indent = string used for indenting the progress info box
                    position = position of the percent done string,
                               must be one out of ['left', 'middle', 'right']


     Note 1: by default sys.stdout is flushed each time a new box is drawn.
             If you need to rely on buffered stdout you'd better not use this
             (any?) progress info box.
     Note 2: progressinfo slows down your loops. Always profile your scripts
             and check that you are not wasting 99% of the time in drawing
             the progress info box.
    """
    iterate_on_items = False
    # try to get the length of the sequence
    try:
        length = len(sequence)
    # if the object is unsized
    except TypeError:
        if length is None:
            err_str = "Must specify 'length' if sequence is unsized."
            raise Exception(err_str)
        elif length < 0:
            iterate_on_items = True
            length = -length
    length = float(length)
    # set layout
    if style == 'bar':
        layout = { 'indent': '',
                   'width' : get_termsize()[1],
                   'position' : 'middle',
                   'delimiters' : '[]',
                   'char1' : '=',
                   'char2' : '>',
                   'char3' : '.' }
        if custom is not None:
            layout.update(custom)
        fixed_lengths = len(layout['indent']) + 4
        if layout['position'] in ['left', 'right']:
            fixed_lengths += 4
        layout['width'] = layout['width'] - fixed_lengths
    elif style == 'timer':
        layout = { 'speed': 'mean',
                   'indent': '',
                   'position' : 'left',
                   'delimiters' : '[]',
                   'separator': ' - ',
                   't_start' : time.time()
                   }
        if custom is not None:
            layout.update(custom)
    else:
        err_str = "Style `%s' not known." % style
        raise ValueError(err_str)

    # start main loop
    last = None
    for count, value in enumerate(sequence):
        # generate progress info
        if iterate_on_items:
            last = _progress(value/length, last, style, layout)
        else:
            last = _progress(count/length, last, style, layout)
        yield value
    else:
        # we need this for the 100% notice
        if iterate_on_items:
            last = _progress(1., last, style, layout)
        else:
            last = _progress((count+1)/length, last, style, layout)
    # clean up terminal
    sys.stdout.write('\n\r')

# execute this file for a demo of the progressinfo style
if __name__ == '__main__':
    #import random
    import mdp
    import tempfile
    print 'Testing progressinfo...'
    # test various customized layouts
    cust_list = [ {'position' : 'left',
                   'indent': 'Progress: ',
                   'delimimters': '()',
                   'char3': ' '},
                  {},
                  {'position': 'right',
                   'width': 50} ]
    for cust in cust_list:
        test = 0
        for i in progressinfo(range(100, 600), style = 'bar', custom = cust):
            test += i
            time.sleep(0.001)
        if test != 174750:
            raise Exception('Something wrong with progressinfo...')
    # generate random character sequence
    inp_list = []
    for j in range(500):
    #    inp_list.append(chr(random.randrange(256)))
        inp_list.append(chr(mdp.numx_rand.randint(256)))
    string = ''.join(inp_list)
    # test various customized layouts
    cust_list = [ {'position': 'left',
                   'separator': ' | ',
                   'delimiters': '()'},
                  {'position':'right'}]
    for cust in cust_list:
        out_list = []
        for i in progressinfo(string, style = 'timer', custom = cust):
            time.sleep(0.02)
            out_list.append(i)
        if inp_list != out_list:
            raise Exception('Something wrong with progressinfo...' )

    # write random file
    with tempfile.TemporaryFile(mode='r+') as fl:
        for i in range(1000):
            fl.write(str(i)+'\n')
        fl.flush()
        # rewind
        fl.seek(0)
        lines = []
        for line in progressinfo(fl, 1000):
            lines.append(int(line))
            time.sleep(0.01)
        if lines != range(1000):
            raise Exception('Something wrong with progressinfo...' )

    # test iterate on items
    with tempfile.TemporaryFile(mode='r+') as fl:
        for i in range(10):
            fl.write(str(i)+'\n')
        fl.flush()
        # rewind
        fl.seek(0)
        def gen():
            for line_ in fl:
                yield int(line_)
        for line in progressinfo(gen(), -10, style='timer',
                                 custom={'speed':'last'}):
            time.sleep(1)

    print 'Done.'
