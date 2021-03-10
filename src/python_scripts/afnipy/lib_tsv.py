
import copy

# ======================================================================
# globals
# ======================================================================
g_seplist = ['\t', ',', '::', ':', ' ']
g_main_fields = ['cue_type', 'duration', 'onset', 'trial_phase', 'trial_type']

# mtypes for stored messages
M_INFO  = 0
M_WARN  = 1
M_ERR   = 2

# ======================================================================
# main TSV/CSV class
# ======================================================================
class Events(object):
   """a general class for manipulating BIDS events files

      guide:
         messages:
            - no direct print() statements, use mesg()
            - any mesg(M_ERR, ...) sets status to 1
            - can set show=1 for immediate display
   """

   def __init__(self, fname=None, sep=None, name='pickles', verb=1):
      # main vars
      self.name    = name
      self.table   = []
      self.header  = []         # list of column headers

      self.isep    = sep        # input separator, if specified
      self.sep     = None       # applied separator
      self.verb    = verb

      # special elements
      self.status   = 0         # 0 is ok
      self.messages = []        # list of [mtype, message] pairs

      if fname:
         if self.read_table(fname):
            return None

   def copy_by_fields(self, flist=None, name='copy'):
      """return a copy
         if flist is None, return a full copy
         if flist is given, adjust
            table  - with restricted columns
            header - equals flist
      """
      # get column index list, either empty (for all) or listed
      if flist is None:
         ilist = []
      else:
         ilist = [self.field_index(field) for field in flist]
         if min(ilist) < 0:
            self.mesg("cannot copy table with invalid fields: %s"%flist)
            return None

      # lazy, but easier
      newtab = copy.deepcopy(self)
      newtab.name    = name

      # if field list, restrict header and table
      if len(ilist) > 0:
         newtab.header  = flist
         del(newtab.table)
         newtab.table = []
         for row in self.table:
            newtab.table.append([row[ind] for ind in ilist])

      return newtab

   def append_field(self, fname, initval):
      """append a field to the table, and adjust the header"""
      self.header.append(fname)
      for row in self.table:
         row.append(initval)

   def remove_rows(self, cflist):
      """remove all rows specified by cflist, where cflist entries are:
         [fieldname, fieldval]
      """
      # always replace the table
      newtab = []
      findlist = [self.field_index(cfent[0]) for cfent in cflist]
      ncf = len(cflist)

      for row in self.table:
         keep = 1
         for findex in range(ncf):
            if findlist[findex] < 0: continue
            if row[findlist[findex]] == cflist[findex][1]:
               keep = 0
               break
         # either keep or ignore row
         if keep:
            newtab.append(row)

      # and replace (no del(), as rows are copied)
      self.table = newtab

   def field_index(self, fname):
      if fname in self.header:
         return self.header.index(fname)

      # failure
      self.mesg(M_ERR, "missing header field %s" % fname)
      return -1

   def field_entries(self, fname):
      """return a list of elements for the given field"""
      find = self.field_index(fname)
      if find < 0: return None

      return [row[find] for row in self.table]

   def read_table(self, fname):
      """populate: table and header
      """
      # --------------------------------------------------
      # read all lines read from file
      self.mesg(M_INFO, "reading table file %s..." % fname)

      try:
         fd = open(fname, 'r')
      except:
         self.mesg(M_ERR, "read_table: failed to open file %s"%fname)
         return 1

      # get rid of newlines, and any other surrounding whitespace
      # get rid of empty lines
      lines = []
      for line in fd.readlines():
         sline = line.strip()
         if sline == '':
            continue
         lines.append(sline)
      fd.close()

      # require at least a header and subsequent line
      if len(lines) < 2:
         self.mesg(M_ERR, "read_table: empty table in file %s"%fname)
         return 1

      # --------------------------------------------------
      # guess separator and set header (remove from lines)
      hline = lines.pop(0)
      sep = self.guess_separator(hline)
      if sep is None:
         return 1
      self.sep = sep

      # set header
      self.header = hline.split(sep)

      # --------------------------------------------------
      # populate table
      self.table = [line.split(sep) for line in lines]

      return 0

   def guess_separator(self, tstr):
      """look for self.sep or g_seplist separators"""
      if self.isep is not None:
         slist = [self.isep]
         slist.extend(g_seplist)
      else:
         slist = g_seplist

      # count occurances of each
      scounts = [tstr.count(s) for s in slist]
      scmax = max(scounts)

      # now just return the first one
      for sind, sc in enumerate(scounts):
         if sc > 0:
            return slist[sind]
        
      # failure
      self.mesg(M_ERR, "found no table separators in file %s"%fname)

      return None

   def mesg(self, mtype, mesg, show=0):
      """store (and possibly print) message
         a. track status (M_ERR sets status to 1)
         b. store message (type and text)
         c. possibly print
            - in verbose mode, show all
            - in normal (verb=1) mode, show warnings and errors
            - in quite (verb=0), show nothing
      """
      if self.verb > 1:
         show = 1
      if mtype == M_ERR:
         if self.verb > 0:
            show = 1
         self.status = 1
      self.messages.append([mtype, mesg])
      if show:
         self.show_mesg(mtype, mesg)

   def show_mesg(self, mtype, mesg):
      """oh, right, maybe actually print a message (who even does that?!?)"""
      if mtype == M_INFO:
         print("++ info: %s" % mesg)
      elif mtype == M_WARN:
         print("** warning: %s" % mesg)
      elif mtype == M_ERR:
         print("** error: %s" % mesg)
      else:
         print("** UNKNOWN: %s" % mesg)

   def write_table(self, fname):
      self.mesg(M_INFO, "writing (%d entries) to table file %s..." \
                        % (len(self.table), fname))
      try:
         fd = open(fname, 'w')
      except:
         self.mesg(M_ERR, "write_table: failed to open file %s"%fname)
         return 1

      fd.write('%s\n' % self.sep.join(self.header))

      for row in self.table:
         rstr = [str(entry) for entry in row]
         fd.write('%s\n' % self.sep.join(rstr))
      fd.close()

      self.mesg(M_INFO, "wrote table to %s"%fname)

   def set_field_type(self, fname, ftype, na_val='0'):
      """try to set field elements to the given type
      """
      self.mesg(M_INFO, "setting field %s types to %s..." % (fname, ftype))

      if fname not in self.header:
         self.mesg(M_INFO, "** no field %s in header" % fname)
         return

      fcol = self.field_index(fname)
      if fcol < 0: return

      for rind, row in enumerate(self.table):
         val = row[fcol]
         if val == 'n/a':
            val = na_val

         try:
            row[fcol] = ftype(val)
         except:
            self.mesg(M_ERR, "failed to convert field %s row %d type to %s" \
                      % (fname, rind, ftype))

   def mod_field_val(self, field, oldval, newval,
                     where_field=None, where_val=None):
      """In 'field' column, change 'oldval' entires to 'newval'.
         If where_field and where_val, restrict this to such cases.
            todo: these could be lists
      """

      # get index for field of choice
      fcol = self.field_index(field)
      if fcol < 0: return 1

      if where_field is not None and where_val is not None:
         wcol = self.field_index(where_field)
      else:
         wcol = -1

      count = 0
      for row in self.table:
         if row[fcol] == oldval:
            # do we skip, based on where_field?
            if wcol >= 0:
               if row[wcol] != where_val:
                  continue

            # okay, change away
            count += 1
            row[fcol] = newval

      if self.verb > 1:
         self.mesg(M_INFO, "-- mod %d events: %s.%s to %s" \
                   % (count, field, oldval, newval))

      return 0

