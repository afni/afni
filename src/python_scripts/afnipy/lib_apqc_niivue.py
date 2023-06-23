#!/usr/bin/env python

# This library contains functions for creating part of the
# afni_proc.py QC HTML.  Specifically, these functions build the
# NiiVue-specific parts
#
# =======================================================================

import os, sys
import copy, json

from   afnipy import afni_util as au

# Dictionary for converting an AFNI cbar name (keys) to NiiVue cmap
# name(s). Note that NiiVue will split bi-directional pbars into 2
# cmaps (pos first, then neg), so we always make a list of any mapping.
afni2nv_cmaps = {
    'Plasma'      : ['plasma'],
    'CET_L17'     : ['cet_l17'],
    'gray_scale'  : ['gray'],
    'gray_scale flip' : ['gray'],    # special case for ve2a
    'GoogleTurbo' : ['turbo'],
    'Reds_and_Blues_Inv' : ['afni_reds_inv', 'afni_blues_inv'],
}


def translate_pbar_dict_afni2nv(pbar_dict):
    """Take a pbar_dict from @chauffeur_afni and translate pieces for NiiVue

Parameters
----------
pbar_dict : dict
    pbar dictionary output by @chauffeur_afni

Returns
-------
nv_dict : dict
    dictionary of ulay, olay, thr and colorizing info for niivue to use

"""

    # start the niivue dict
    nv_dict = {}

    # subbricks of ulay, olay and thr, respectively
    subbb   = copy.deepcopy(pbar_dict['subbb'])

    # cmap or cmaps in niivue
    if '=' in pbar_dict['cbar']:
        # currently, special case of discrete 'one-off' cbar
        cmap = ['jet']
    else:
        cmap = copy.deepcopy(afni2nv_cmaps[pbar_dict['cbar']])

    # show/hide olay button for alignment/flip checks
    if pbar_dict['pbar_fname'].endswith('_va2t_anat2temp.pbar.jpg') or \
       pbar_dict['pbar_fname'].endswith('_ve2a_epi2anat.pbar.jpg') or \
       pbar_dict['pbar_fname'].endswith('_warns_flip_0.pbar.jpg') or \
       pbar_dict['pbar_fname'].endswith('_warns_flip_1.pbar.jpg') :
        nv_dict['olay_btn_sh'] = True
    else:
        nv_dict['olay_btn_sh'] = False

    # topval of ulay grayscale cbar
    nv_dict['cal_max_ulay'] = pbar_dict['ulay_top']

    # L/R directionality
    if pbar_dict['is_left_left'].lower() == "yes" :
       nv_dict['isRadCon'] = 'false'
    else:
       nv_dict['isRadCon'] = 'true'
       
    # A/P directionality
    if pbar_dict['is_left_post'].lower() == "no" :
       nv_dict['sagNoseLeft'] = 'true'
    else:
       nv_dict['sagNoseLeft'] = 'false'

    # possible 'jump to' coords
    nv_dict['coor_type'] = pbar_dict['coor_type']
    # coors are XYZ or IJK vals, depending on coor_type
    if nv_dict['coor_type'] == 'SET_DICOM_XYZ' :
        # NiiVue always uses RAS notation (what AFNI calls LPI)
        coors_ras = afni_rai_dicom_to_niivue_ras(pbar_dict['coors'])
        nv_dict['coors_str'] = ', '.join([str(x) for x in coors_ras])
    else :
        nv_dict['coors_str'] = ', '.join([str(x) for x in pbar_dict['coors']])
    
    # is there an overlay to display?
    nv_dict['see_overlay'] = pbar_dict['see_olay']
    if nv_dict['see_overlay']  == '+' :
        nv_dict['cmap']     = cmap[0]                 # cbar name(s)
        if len(cmap) > 1 :
            nv_dict['cmap_neg'] = cmap[1]             # cbar name(s)
        nv_dict['idx_olay'] = subbb[1]                # idx of olay subbrick
        nv_dict['idx_thr']  = subbb[2]                # idx of thr subbrick
        nv_dict['cal_max_olay'] = pbar_dict['pbar_top']   # cbar max
        nv_dict['cal_max_thr']  = pbar_dict['vthr']       # thr val
        # if thresholding, use AFNI-like alpha thresholding; else, don't
        if float(nv_dict['cal_max_thr']) != 0.0 :
            nv_dict['modAlpha'] = 2.0
        else:
            nv_dict['modAlpha'] = 0

        if pbar_dict['olay_alpha'] != 'No' :          # use alpha fade?
            nv_dict['do_alpha'] = 'true'
        else:
            nv_dict['do_alpha'] = 'false'

        if pbar_dict['olay_boxed'] != 'No' :          # use box lines
            nv_dict['do_boxed'] = 1
        else:
            nv_dict['do_boxed'] = 0
        if 'olay_opacity' in pbar_dict :
            nv_dict['olay_opacity'] = float(pbar_dict['olay_opacity'])/9.
        else:
            nv_dict['olay_opacity'] = 1

    return nv_dict

def set_niivue_ulay(nv_dict):
    """Use the NiiVue dict to build the NVOBJ.loadVolumes([..]) info for
the ulay dset.

Parameters
----------
nv_dict : dict
    dictionary of ulay, olay, thr and colorizing info for niivue to use

Returns
-------
nv_ulay_txt : str
    text for the underlay info in loading a NiiVue volume

    """

    nv_ulay_txt = '''      {{ // ulay
        url:"{ulay}",
        cal_max: {cal_max_ulay},
      }}'''.format( **nv_dict )

    return nv_ulay_txt

def afni_rai_dicom_to_niivue_ras(A):
    """The AFNI GUI by default will report in RAI-DICOM convention, where
Right/Anterior/Inferior XYZ-coords have negative signs.  NiiVue uses
RAS+ notation, where Left/Posterior/Inferior are all negative.  This
function takes an XYZ triplet of numbers A that is RAI-DICOM notation
and returns an array B of RAS+ coords.

Parameters
----------
A : array/list/tuple
    An ordered collection of len=3 numbers, assumed to be in RAI-DICOM 
    coordinate notation.

Returns
-------
B : list
    A list of len=3 numbers (floats), which should now be in RAS+ 
    coordinate notation.

    """
    
    N = len(A)
    if N != 3 :
        print("** ERROR: ordered collection A should have len=3, not {}"
              "".format(N))

    B = [0.0] * 3    # to store coord values
    S = [1.0] * 3    # signum values, +/- 1

    # coords where signs will flip
    if A[0] > 0 :    S[0] = -1
    if A[1] > 0 :    S[1] = -1

    for i in range(3):
        B[i] = S[i] * A[i]

    return B


def set_niivue_olay(nv_dict):

    """Use the NiiVue dict to build the NVOBJ.loadVolumes([..]) info for
the olay dset.

Parameters
----------
nv_dict : dict
    dictionary of ulay, olay, thr and colorizing info for niivue to use

Returns
-------
nv_olay_txt : str
    text for the overlay (and thr) info in loading a NiiVue volume

    """

    nv_olay_txt = ''', {{ // olay
        url:"{olay}",
        frame4D: {idx_olay},  // idx of vol
        colorMap: "{cmap}",'''.format(**nv_dict)

    if 'cmap_neg' in nv_dict :
        nv_olay_txt+= '''
        colorMapNegative: "{cmap_neg}",'''.format(**nv_dict)

    nv_olay_txt+= '''
        cal_min: 0,
        cal_max: {cal_max_olay},
        opacity: {olay_opacity},
      }}'''.format(**nv_dict)

    return nv_olay_txt

def set_niivue_thr(nv_dict):
    """Use the NiiVue dict to build the NVOBJ.loadVolumes([..]) info for
the thr dset.

Parameters
----------
nv_dict : dict
    dictionary of ulay, olay, thr and colorizing info for niivue to use

Returns
-------
nv_thr_txt : str
    text for the thr info in loading a NiiVue volume

    """

    # thr+olay dsets are same, but might be diff subbricks
    nv_thr_txt = ''', {{ // thr
        url:"{olay}",  // same dset as olay
        frame4D: {idx_thr},  // idx of vol
        colorMap: "blue",'''.format(**nv_dict) # doesn't matter, not shown
        #colorMap: "{cmap}",'''.format(**nv_dict)

    if 'cmap_neg' in nv_dict :
        nv_thr_txt+= '''
        colorMapNegative: "blue",'''.format(**nv_dict) # also not shown

    nv_thr_txt+= '''
        cal_min: 0,
        cal_max: {cal_max_thr},
        opacity: 0,
      }}'''.format(**nv_dict)

    return nv_thr_txt


def set_niivue_then(nv_dict):
    """Use the NiiVue dict to build the 'then' text after
NVOBJ.loadVolumes([..]).

Parameters
----------
nv_dict : dict
    dictionary of ulay, olay, thr and colorizing info for niivue to use

Returns
-------
nv_then_txt : str
    text for the 'then' features in loading a NiiVue volume

    """

    ### NB: these colorbarVisible calls could be used, but we turn off
    ### using the extra NiiVue cbar now, because there is one below
    ### anyways:
    #{nobj}.volumes[0].colorbarVisible = false; // no ulay bar
    #{nobj}.volumes[1].colorbarVisible = true;  // yes olay bar
    #{nobj}.volumes[2].colorbarVisible = false; // no thr bar
    nv_then_txt = '''
      {nobj}.volumes[1].alphaThreshold = {do_alpha}; // alpha for olay
      {nobj}.overlayOutlineWidth = {do_boxed};
      {nobj}.opts.multiplanarForceRender = true;
      {nobj}.backgroundMasksOverlays = true;
      {nobj}.setModulationImage(
        {nobj}.volumes[1].id,
        {nobj}.volumes[2].id,
        modulateAlpha = {modAlpha} 
      ); // activate+specify mapping'''.format(**nv_dict)

    if nv_dict['coor_type'] == "SET_DICOM_XYZ" :
        nv_then_txt+= '''
        {nobj}.scene.crosshairPos = '''.format(**nv_dict)
        nv_then_txt+= '''{nobj}.mm2frac([{coors_str}]);'''.format(**nv_dict)
        nv_then_txt+= ''' // jump to XYZ'''.format(**nv_dict)

    nv_then_txt+= '''
      {nobj}.updateGLVolume();'''.format(**nv_dict)

    return nv_then_txt


# =======================================================================

def make_niivue_2dset(qcdir, ulay_name, pbar_dict, olay_name=None, itemid='',
                      path='..', verb=0):
    """This function creates the mini-html for each dataset(s) that can be
viewed in NiiVue.  It also creates the text that gets inserted into
the primary index.html, where NiiVue will actually open when toggled on.

Parameters
----------
qcdir : str
    name of QC directory (QC_<subj>/)
ulay_name : str
    name of ulay dataset
pbar_dict : dict
    pbar (=cmap = cbar) dictionary of items; made by converting
    @chauffeur_afni's '-pbar_saveim ..' txt file -> JSON and reading
    it in as a dict. Contains both ulay and olay (if present)
    information
olay_name : str
    name of olay dataset
itemid : str
    ID of the div that will be activated using this NiiVue instance
path : str
    path to the data (from QC_*/), which will typically be the default
verb : str
    verbosity level whilst processing

Returns
-------
otxt : str
    string of the NiiVue canvas to stick into the HTML

    """

    # Make the dsets with paths
    pdata = path.rstrip('/')
    if ulay_name[0] == '/' : 
        # the template, which can only be a ulay, could be abs path
        ulay = ulay_name
    else:
        ulay  = pdata + '/' + ulay_name
    if olay_name :  olay = pdata + '/' + olay_name

    # NiiVue canvas ID and object name (*NB: latter can't have '-' chars)
    nid = 'nvcan_' + itemid
    nobj = 'nvobj_' + au.rename_label_safely(itemid) 

    # Setup a NiiVue dict for loading and displaying volumes, based on
    # the AFNI pbar.  Translate names of things, and load in various
    # other useful pieces of information for string generation
    nv_dict = translate_pbar_dict_afni2nv(pbar_dict)
    nv_dict['ulay'] = ulay
    if olay :  nv_dict['olay'] = olay

    nv_dict['nid']  = nid
    nv_dict['nobj'] = nobj

    # create pieces of text within NiiVue canvas
    nv_ulay_txt = set_niivue_ulay(nv_dict)
    nv_olay_txt = set_niivue_olay(nv_dict)
    nv_thr_txt  = set_niivue_thr(nv_dict)
    nv_then_txt = set_niivue_then(nv_dict)

    # top of mini-html: the typecast part
    ohtml = '''
<html>
<head>
  <link rel="stylesheet" type="text/css" href="extra_info/styles.css" />
  <script src="./niivue.umd.js"> </script>
  <script type="text/javascript">

    /* show/hide olay in NV (for align checks)
        obj : NV object ID
        bid : button ID in that object's NV canvas
    */
    function niivue_ShowHideOlay(obj, bid) {
      let element = document.getElementById(bid);
      if (obj.volumes[1].opacity) {
        obj.setOpacity(1, 0);
        element.innerText = "View Olay";
      } else {
        obj.setOpacity(1, 1);
        element.innerText = "Hide Olay";
      }
    }

    // used for string formatting numbers (C Rorden), below
    function flt2str0(flt, ndec = 0) {
      //retain trailing zero
      return flt.toFixed(ndec); //return string
    }

    /* more string formatting numbers, below ... WITH directionality here,
       assuming the coords are what AFNI calls LPI (and what some other
       software call RAS); basically, L/P/I are neg coords, and R/A/S are
       positive.  
    */
    function flt2str0_dir(flt, ndec = 0, dir = '') {
      //retain trailing zero
      if (dir == '') {
        return flt.toFixed(ndec); //return string
      } else if (dir == 'RL') {
        let aflt = Math.abs(flt);
        let lab = (flt < 0) ? 'L' : 'R';
        return aflt.toFixed(ndec) + lab;
      } else if (dir == 'AP') {
        let aflt = Math.abs(flt);
        let lab = (flt < 0) ? 'P' : 'A';
        return aflt.toFixed(ndec) + lab;
      } else if (dir == 'IS') {
        let aflt = Math.abs(flt);
        let lab = (flt < 0) ? 'I' : 'S';
        return aflt.toFixed(ndec) + lab;
      } else {
        return '';
      }
    }

  </script>
</head>
<body>
'''

    ohtml+= '''
<!-- start NiiVue canvas for: {nid} -->
<div class="class_niivue" id="{nid}_container">
  <canvas id="{nid}" height=480 width=640>
  </canvas>
  <footer id="{nid}_xyz" style="color:#fff; ">
    &nbsp;
  </footer>
'''.format( nid=nid )

    # for align checks, can have show/hide button
    if nv_dict['olay_btn_sh'] :
        ohtml+= '''  
  <button class="button-generic btn7"
          id="{nid}_btnSHO" 
          onclick="niivue_ShowHideOlay({nobj}, '{nid}_btnSHO')">
  Hide Olay
  </button>

'''.format( nid=nid, nobj=nobj )

    ohtml+= '''  <script>
    function reportCoorAndValues(data) {{
      // coord str
      let x = flt2str0_dir(data.mm[0], 3, 'RL')
      let y = flt2str0_dir(data.mm[1], 3, 'AP')
      let z = flt2str0_dir(data.mm[2], 3, 'IS')
      let str_c = '&nbspxyz:[' + 
                  x.padStart(8).replace(/ /g, '&nbsp;') + ' ' +
                  y.padStart(8).replace(/ /g, '&nbsp;') + ' ' +
                  z.padStart(8).replace(/ /g, '&nbsp;') + '], '
      // value str
      let u = flt2str0(data.values[0].value, 6)
      let o = flt2str0(data.values[1].value, 6)
      let t = flt2str0(data.values[2].value, 6)
      let str_v = 'UOT:[' + 
                  u.padStart(12).replace(/ /g, '&nbsp;') + ' ' + 
                  o.padStart(12).replace(/ /g, '&nbsp;') + ' ' + 
                  t.padStart(12).replace(/ /g, '&nbsp;') + ']' 
      document.getElementById('{nid}_xyz').innerHTML = str_c + str_v;
    }}
'''.format( nid=nid )

    ohtml+= '''
    const {nobj} = new niivue.Niivue(
      {{ logging: true, 
        show3Dcrosshair: true,
        onLocationChange: reportCoorAndValues,
      }}
    )
    {nobj}.opts.isColorbar = false; // just use from HTML
    {nobj}.opts.sagittalNoseLeft = {sagNoseLeft};
    {nobj}.opts.isRadiologicalConvention = {isRadCon};
    {nobj}.opts.isCornerOrientationText = true;
    {nobj}.attachTo('{nid}');
    {nobj}.loadVolumes([
{nv_ulay_txt}{nv_olay_txt}{nv_thr_txt}
    ]).then(()=>{{ // more actions
{nv_then_txt}
    }})'''.format( nv_ulay_txt=nv_ulay_txt, nv_olay_txt=nv_olay_txt,
                   nv_thr_txt=nv_thr_txt, nv_then_txt=nv_then_txt,
                   **nv_dict )

    ohtml+= '''
  </script>
</div>
</body>
</html>
'''

    # write out the html to the main QC_*/ dir
    fname = "{qcdir}/{nid}_container.html".format(qcdir=qcdir, nid=nid)
    fff   = open(fname, 'w')
    fff.write(ohtml)
    fff.close()

    otxt = '''
<!-- placeholder for NiiVue canvas: {nid} -->
<div id="{nid}_container" class="">
</div> 
'''.format(nid=nid)

    return otxt





