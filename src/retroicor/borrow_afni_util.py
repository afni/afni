#!/usr/bin/env python

# python3 status: started

# afni_util.py : general utilities for python programs

# ------------------------------------------------------
# no longer usable as a main: see afni_python_wrapper.py
# ------------------------------------------------------

g_valid_slice_patterns = [ # synonymous pairs      # z2-types
                           'zero',  'simult',
                           'seq+z', 'seqplus',
                           'seq-z', 'seqminus',
                           'alt+z', 'altplus',     'alt+z2',    
                           'alt-z', 'altminus',    'alt-z2',    
                         ]
g_tpattern_irreg = 'irregular'

def slice_pattern_to_order(pattern, nslices):
   """return a list of slice order indices
      (a permuted list of 0..nslices-1)

        pattern : slice pattern (based on to3d - e.g. alt+z, simult)
        nslices : number of slices to apply the pattern to

      Given one of the g_valid_slice_patterns and a number of slices, return
      an array of slice indices, in the order they would be acquired.

      This assumes equal timing across the slices over the course of a TR.

      Note that zero/simult are not considered valid patterns here, since there
      is no sorted order in such a case (they are all equal).

      return : a list of slice indices, in order
             : None on error
   """

   if pattern not in g_valid_slice_patterns:
      print("** pattern_to_order, invalid pattern", pattern)
      return None
   if pattern in ['zero', 'simult']:
      print("** pattern_to_order, cannot make ordering from pattern", pattern)
      return None

   # sequential
   if pattern == 'seq+z' or pattern == 'seqplus':
      order = [ind for ind in range(nslices)]
   # reverse sequential
   elif pattern == 'seq-z' or pattern == 'seqminus':
      order = [(nslices-1-ind) for ind in range(nslices)]

   # alternating positive, get evens then odds
   elif pattern == 'alt+z' or pattern == 'altplus':
      order = list(range(0, nslices, 2))
      order.extend(list(range(1, nslices, 2)))
   # alternating negative, similar but top-down
   elif pattern == 'alt-z' or pattern == 'altminus':
      # start from final position (nslices-1) and work downward
      order =      list(range(nslices-1, -1, -2))
      order.extend(list(range(nslices-2, -1, -2)))

   # the z2 patterns are similar to alt, but odds come first
   elif pattern == 'alt+z2' :
      order = list(range(1, nslices, 2))
      order.extend(list(range(0, nslices, 2)))
   # alternating negative, similar but top-down
   elif pattern == 'alt-z2' :
      order =      list(range(nslices-2, -1, -2))
      order.extend(list(range(nslices-1, -1, -2)))

   else:
      print("** pattern_to_order, unhandled pattern", pattern)
      return None

   return order

def slice_pattern_to_timing(pattern, nslices, TR=0):
   """given tpattern, nslices and TR, return a list of slice times

      special case: if TR == 0 (or unspecifiec)
         - do not scale (so output is int list, as if TR==nslices)

      method:
         - get slice_pattern_to_order()
           - this is a list of slice indexes in the order acquired
         - attach the consecutive index list, range(nslices)
           - i.e, make list of [ [slice_index, acquisition_index] ]
         - sort() - i.e. by slice_index
           - so element [0] values will be the sorted list of slices
         - grab element [1] from each
           - this is the order the given slice was acquired in
         - scale all by TR/nslices

      return a list of slice times, or an empty list on error
   """
   if nslices <= 0 or TR < 0.0:
      return []
   if nslices == 1:
      return [0]

   if pattern not in g_valid_slice_patterns:
      print("** slice_pattern_to_timing, invalid pattern", pattern)
      return []

   # if there is no time to partition or slices are simulaneous, return zeros
   if pattern in ['zero', 'simult']:
      return [0] * nslices

   # first get the slice order
   order = slice_pattern_to_order(pattern, nslices)
   if order is None:
      return []

   # attach index and sort
   slice_ordering = [ [order[ind], ind] for ind in range(nslices)]
   slice_ordering.sort()

   # grab each element [1] and scale by TR/nslices
   # (if TR == 0, do not scale)
   if TR == 0:
      stimes = [so[1]            for so in slice_ordering]
   else:
      stimes = [so[1]*TR/nslices for so in slice_ordering]

   return stimes
