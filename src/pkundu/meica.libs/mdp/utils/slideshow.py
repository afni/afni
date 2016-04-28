"""
Module for HTML slideshows.

It uses the templating library 'Templet'.

The slideshow base class HTMLSlideShow does not display anything, but can
be used to derive custom slideshows like in BiMDP.

The JavaScript slideshow code in this module was originally inspired by a
slideshow script found at
http://javascript.internet.com/miscellaneous/image-slideshow.html
(which in turn seems to be based on something from http://www.ricocheting.com)
"""

from __future__ import with_statement

import random
import tempfile
import os
import webbrowser
import warnings

import templet

_BASIC_CSS_FILENAME = "basic.css"
_SLIDESHOW_CSS_FILENAME = "slideshow.css"

def basic_css():
    """Return the basic default CSS."""
    css_filename = os.path.join(os.path.split(__file__)[0],
                                _BASIC_CSS_FILENAME)
    with open(css_filename, 'r') as css_file:
        css = css_file.read()
    return css

def slideshow_css():
    """Return the additional CSS for a slideshow."""
    css_filename = os.path.join(os.path.split(__file__)[0],
                                _SLIDESHOW_CSS_FILENAME)
    with open(css_filename, 'r') as css_file:
        css = css_file.read()
    return css


class HTMLSlideShow(templet.Template):
    """Abstract slideshow base class.

    It does not display anything, but can be adapted by overriding
    some of the templating attributes. See ImageHTMLSlideShow for an example.
    """

    def __init__(self, title=None, delay=100, delay_delta=20,
                 loop=True, slideshow_id=None, shortcuts=True, **kwargs):
        """Return the complete HTML code for the slideshow.

        title -- Optional slideshow title (for defualt None not title is shown).
        delay - Delay between slides in ms (default 100).
        delay_delta - Step size for increasing or decreasing the delay.
        loop -- If True continue with first slide when the last slide is
            reached during the automatic slideshow (default is False).
        slideshow_id -- String with the id used for the JS closure, and this
            is also the id of the div with the slideshow (so it can be used
            by CSS) and it is used as a prefix for the HTML elements.
            If the value is None (default) then a random id is used.
        shortcuts -- Bind keyboard shortcuts to this slideshow (default is
            True). Note that keyboard shortcuts only work for a single
            slideshow per page.
        """
        # translate boolean variable into JS format
        if loop:
            loop = "true"
        else:
            loop = "false"
        if slideshow_id is None:
            slideshow_id = self._get_random_id()
        self.slideshow_id = slideshow_id
        kwargs.update(vars())
        del kwargs["self"]
        super(HTMLSlideShow, self).__init__(**kwargs)

    def _get_random_id(self):
        """Factory method for random slideshow id."""
        return "slideshow%d" % random.randint(10000, 99999)


    template = r'''
<script language="JavaScript">
<!-- Begin

// use closure to create object, the slideshow_id is the only global variable
var $slideshow_id = function () {

    var that = {};

    var current_slide = 0; // current slide index
    var show_delay = $delay; // delay in milliseconds
    var loop_slideshow = $loop; // loop in auto mode
    // shortcuts to form elements, initialized in onLoad
    var slideform;
    var slideselect;

    $<js_controls_template>
    $<js_loadslide_template>
    $<js_update_template>
    $<js_onload_template>

    that.onSelectorChange = function () {
        current_slide = slideselect.selectedIndex;
        that.updateSlide();
    }

    that.next = function () {
        if (slideselect[current_slide+1]) {
            current_slide += 1;
            that.updateSlide();
        }
        else if (loop_slideshow) {
            that.first();
        }
    }

    that.previous = function () {
        if (current_slide-1 >= 0) {
            current_slide -= 1;
            that.updateSlide();
        }
        else if (loop_slideshow) {
            that.last();
        }
    }

    that.first = function () {
        current_slide = 0;
        that.updateSlide();
    }

    that.last = function () {
        current_slide = slideselect.length-1;
        that.updateSlide();
    }

    // start or stop the slideshow
    that.startstop = function (text) {
        if (text === "Start") {
            slideform.startbutton.value = "Stop";
            if (!loop_slideshow &&
                (current_slide === slideselect.length-1)) {
                // restart slideshow
                current_slide = -1;  // is directly increased by showAuto
            }
            that.showAuto();
        } else {
            slideform.startbutton.value = "Start";
        }
    }

    // continuously show the slideshow
    that.showAuto = function () {
        if (slideform.startbutton.value == "Stop") {
            if (current_slide == slideselect.length-1) {
                if (loop_slideshow) {
                    current_slide = 0;
                    that.updateSlide();
                } else {
                    slideform.startbutton.value = "Start";
                }
            } else {
                current_slide = current_slide+1;
                that.updateSlide();
            }
            window.setTimeout("$slideshow_id.showAuto()", show_delay);
        }
    }

    // end of closure, return created object
    return that;
}();

${{
if shortcuts:
    self.js_keyboard_shortcuts_template(vars())
}}

//  End -->
</script>

<div class="slideshow" id="$slideshow_id">

$<html_top_template>

<form name=${slideshow_id}_slideform>
<table class="slideshow">
${{
if title:
    self.write('<tr><td><b> %s </b></td></tr>' % title)
}}

$<html_box_template>

<tr><td>
<select name="${slideshow_id}_slideselect"
    onChange="$slideshow_id.onSelectorChange();">
<option value="${filenames[0]}" selected>${filenames[0]}
${{
for filename in filenames[1:]:
    self.write('<option value="%s">%s\n' % (filename, filename))
}}
</select>
</td></tr>
<tr><td>

$<html_buttons_template>

</td></tr>

$<html_controls_template>

</table>
</form>

$<html_bottom_template>

</div>

<SCRIPT LANGUAGE="JavaScript">
<!-- Begin
$slideshow_id.onLoad();
//  End -->
</script>
'''

    js_controls_template = r'''
    // step size for in- or decreasing the delay
    var delay_delta = $delay_delta;

    that.slower = function () {
        show_delay += delay_delta;
        slideform.${slideshow_id}_delaytext.value = show_delay.toString();
    }

    that.faster = function (text) {
        show_delay -= delay_delta;
        if (show_delay < 0) {
            show_delay = 0;
        }
        slideform.${slideshow_id}_delaytext.value = show_delay.toString();
    }

    that.changeDelay = function () {
        var new_delay = parseInt(slideform.${slideshow_id}_delaytext.value, 10);
        if (new_delay < 0) {
             new_delay = 0;
            }
        show_delay = new_delay;
        slideform.${slideshow_id}_delaytext.value = new_delay.toString();
    }
'''

    js_update_template = r'''
    that.updateSlide = function () {
        slideselect.selectedIndex = current_slide;
        that.loadSlide();
    }
'''

    # overwrite this to implement the actual slide change
    js_loadslide_template = r'''
    that.loadSlide = function () {
    }
'''

    js_onload_template = r'''
    that.onLoad = function () {
        slideform = document.${slideshow_id}_slideform;
        slideselect = slideform.${slideshow_id}_slideselect;
        current_slide = slideselect.selectedIndex;
        that.updateSlide();
        slideform.${slideshow_id}_delaytext.value = show_delay.toString();
    }
'''

    # define keyboard shortcuts,
    # note that these are also mentionend in the button hover-text
    js_keyboard_shortcuts_template = r'''
document.onkeydown = function(e) {
    if (!e.ctrlKey) {  // control key must be pressed
        return;
    }
    else if (e.which == 37) { // left key
        document.getElementById("${slideshow_id}_prevButton").click();
    }
    else if(e.which == 39) { // right key
        document.getElementById("${slideshow_id}_nextButton").click();
    }
    else if(e.which == 38) { // up key
        document.getElementById("${slideshow_id}_firstButton").click();
    }
    else if(e.which == 40) { // down key
        document.getElementById("${slideshow_id}_lastButton").click();
    }
    else if(e.which == 45) { // insert key
        document.getElementById("${slideshow_id}_startButton").click();
    }
}
'''

    html_buttons_template = r'''
<input type=button onClick="$slideshow_id.first();"
    value="|<<" title="First" id="${slideshow_id}_firstButton">
<input type=button onClick="$slideshow_id.previous();"
    value="<" title="Previous (Ctrl+Left)" id="${slideshow_id}_prevButton">
<input type=button name="startbutton"
    onClick="$slideshow_id.startstop(this.value);"
    value="Start" title="Autoplay (Ctrl+Insert)"
    id="${slideshow_id}_startButton">
<input type=button onClick="$slideshow_id.next();" value=">"
    title="Next (Ctrl+Right)"
    id="${slideshow_id}_nextButton">
<input type=button onClick="$slideshow_id.last();" value=">>|" title="Last"
    id="${slideshow_id}_lastButton">
'''

    html_controls_template = r'''
${{
if delay is not None:
    self.write('<tr><td>\n')
    self.html_delay_template(vars())
    self.write('</td></tr>\n')
}}
'''

    html_delay_template = r'''
delay: <input type="text" name="${slideshow_id}_delaytext"
    onChange="$slideshow_id.changeDelay();" value="0" size="4"> ms
<input type=button onClick="$slideshow_id.faster();" value="-" title="Faster">
<input type=button onClick="$slideshow_id.slower();" value="+" title="Slower">
'''

    html_top_template = r'''
'''

    html_box_template = r'''
'''

    html_bottom_template = r'''
'''


class SectionHTMLSlideShow(HTMLSlideShow):
    """Astract slideshow with additional support for section markers."""

    def __init__(self, section_ids, slideshow_id=None, **kwargs):
        """Return the complete HTML code for the slideshow.

        section_ids -- List with the section id for each slide index. The id
            can be a string or a number.

        For additional keyword arguments see the super class.
        """
        # we need the slideshow_id for the section names
        if slideshow_id is None:
            slideshow_id = self._get_random_id()
        kwargs.update(vars())
        # check if there is more than one section slideshow_id,
        # otherwise some controls must be disabled to prevent infinite loop
        only_one_section = "false"
        first_section_id = section_ids[0]
        for section_id in section_ids:
            if section_id != first_section_id:
                break
        else:
            only_one_section = "true"
        kwargs["only_one_section"] = only_one_section
        # translate section_id_list into JavaScript list
        section_ids = [str(section_id) for section_id in section_ids]
        js_section_ids = "".join(['        "%s_section_id_%s",\n' %
                                  (slideshow_id, section_id)
                                  for section_id in section_ids])
        js_section_ids = "\n" + js_section_ids[:-2]
        kwargs["js_section_ids"] = js_section_ids
        del kwargs["self"]
        super(SectionHTMLSlideShow, self).__init__(**kwargs)


    js_update_template = r'''
    // maps slide index to section slideshow_id
    var section_ids = new Array($js_section_ids);
    // currently highlighted section slideshow_id
    var current_section_id = section_ids[0];

    that.updateSlide = function () {
        document.getElementById(current_section_id).className =
            "inactive_section";
        current_section_id = section_ids[current_slide]
        document.getElementById(current_section_id).className =
            "active_section";
        slideselect.selectedIndex = current_slide;
        that.loadSlide();
    }

    // use this function when a section is selected,
    // e.g. onClick="setSlide(42)"
    that.setSlide = function (index) {
        current_slide = index;
        that.updateSlide();
    }

    that.previousSection = function () {
        if ($only_one_section) {
            return;
        }
        while (current_section_id === section_ids[current_slide]) {
            if (current_slide > 0) {
                current_slide -= 1;
            } else {
                current_slide = slideselect.length-1;
            }
        }
        var new_section_id = section_ids[current_slide];
        // now go to start of this section
        while (new_section_id === section_ids[current_slide]) {
            current_slide -= 1;
            if (current_slide < 0) {
                break;
            }
        }
        current_slide += 1;
        that.updateSlide();
    }

    that.nextSection = function () {
        if ($only_one_section) {
            return;
        }
        while (current_section_id === section_ids[current_slide]) {
            if (current_slide+1 < slideselect.length) {
                current_slide += 1;
            } else {
                current_slide = 0;
            }
        }
        that.updateSlide();
    }

    $<js_loadslide_template>
'''

    # define keyboard shortcuts,
    # note that these are also mentionend in the button hover-text
    js_keyboard_shortcuts_template = r'''
document.onkeydown = function(e) {
    if (!e.ctrlKey) { // control key must be pressed
        return;
    }
    else if (e.which === 37) { // left key
        document.getElementById("${slideshow_id}_prevButton").click();
    }
    else if(e.which === 39) { // right key
        document.getElementById("${slideshow_id}_nextButton").click();
    }
    else if(e.which === 38) { // up key
        document.getElementById("${slideshow_id}_prevSectionButton").click();
    }
    else if(e.which === 40) { // down key
        document.getElementById("${slideshow_id}_nextSectionButton").click();
    }
    else if(e.which === 45) { // insert key
        document.getElementById("${slideshow_id}_startButton").click();
    }
}
    '''

    html_buttons_template = r'''
<input type=button onClick="$slideshow_id.first();"
    value="|<<" title="First" id="${slideshow_id}_firstButton">
<input type=button onClick="$slideshow_id.previousSection();" value="|<"
    title="Previous Section (Ctrl+Up)" id="${slideshow_id}_prevSectionButton">
<input type=button onClick="$slideshow_id.previous();"
    value="<" title="Previous (Ctrl+Left)" id="${slideshow_id}_prevButton">
<input type=button name="startbutton"
    onClick="$slideshow_id.startstop(this.value);"
    value="Start" title="Autoplay (Ctrl+Insert)"
    id="${slideshow_id}_startButton">
<input type=button onClick="$slideshow_id.next();" value=">"
    title="Next (Ctrl+Right)"
    id="${slideshow_id}_nextButton">
<input type=button onClick="$slideshow_id.nextSection();" value=">|"
    title="Next Section (Ctrl+Down)" id="${slideshow_id}_nextSectionButton">
<input type=button onClick="$slideshow_id.last();" value=">>|" title="Last"
    id="${slideshow_id}_lastButton">
'''

    html_controls_template = r'''
${{super(SectionHTMLSlideShow, self).html_controls_template(vars())}}

<tr><td>
<div id="${slideshow_id}_sections_panel"
    style="margin-left:auto; margin-right: auto;">
${{
last_section_id = None
link = ''
for index, section_id in enumerate(section_ids):
    if section_id != last_section_id:
        if index > 0:
            self.write(link + '&nbsp;| ')
        last_section_id = section_id
        link = ('<span class="inactive_section" ' +
                'id="%s_section_id_%s" ' % (slideshow_id, section_id) +
                'onClick="%s.setSlide(%d);">%s</span>' %
                    (slideshow_id, index, section_id))
self.write(link + '\n')
}}
</div>
</td></tr>
'''


def image_slideshow_css():
    """Use nearest neighbour resampling in Firefox 3.6+ and IE.

    Webkit (Chrome, Safari) does not support this yet.
    (see http://code.google.com/p/chromium/issues/detail?id=1502)
    """
    return slideshow_css() + '''
img.slideshow {
    image-rendering: -moz-crisp-edges;
    -ms-interpolation-mode: nearest-neighbor;
}
'''


class ImageHTMLSlideShow(HTMLSlideShow):
    """Slideshow for images.

    This also serves as an example for implementing a slideshow based on
    HTMLSlideShow.
    """

    def __init__(self, filenames, image_size,
                 magnification=1, mag_control=True, **kwargs):
        """Return the complete HTML code for a slideshow of the given images.

        filenames -- sequence of strings, containing the path for each image
        image_size -- Tuple (x,y) with the original image size, or enter
            a different size to force scaling.
        magnification -- Magnification factor for images (default 1). This
            factor is applied on top of the provided image size.
        mag_control -- Set to True (default) to display a magnification control
            element.

        For additional keyword arguments see the super class.
        """
        if len(filenames) == 0:
            raise Exception("Empty list was given.")
        kwargs.update(vars())
        # translate image size to width and heigh to be used in the templates
        del kwargs["image_size"]
        kwargs["width"] = image_size[0]
        kwargs["height"] = image_size[1]
        del kwargs["self"]
        super(ImageHTMLSlideShow, self).__init__(**kwargs)

    js_controls_template = r'''
    ${{super(ImageHTMLSlideShow, self).js_controls_template(vars())}}

    var magnification = $magnification; // image magnification
    var original_width = $width; // original image width
    var original_height = $height; // original image height

    that.smaller = function () {
        magnification = magnification / 2;
        slideform.${slideshow_id}_magtext.value = magnification.toString();
        that.resizeImage();
    }

    that.larger = function (text) {
        magnification = magnification * 2;
        slideform.${slideshow_id}_magtext.value = magnification.toString();
        that.resizeImage();
    }

    that.changeMag = function () {
        magnification = parseFloat(slideform.${slideshow_id}_magtext.value);
        that.resizeImage();
    }

    $<js_controls_resize_template>
'''

    js_controls_resize_template = r'''
    that.resizeImage = function () {
        document.images.${slideshow_id}_image_display.width =
            parseInt(magnification * original_width, 10);
        document.images.${slideshow_id}_image_display.height =
            parseInt(magnification * original_height, 10);
    }
'''

    js_loadslide_template = r'''
    that.loadSlide = function () {
        document.images.${slideshow_id}_image_display.src =
            slideselect[current_slide].value;
    }
'''

    js_onload_template = r'''
    that.onLoad = function () {
        slideform = document.${slideshow_id}_slideform;
        slideselect = slideform.${slideshow_id}_slideselect;
        current_slide = slideselect.selectedIndex;
        that.updateSlide();
        ${{
        if delay is not None:
            self.write('slideform.%s_delaytext.value = ' % slideshow_id +
                       'show_delay.toString();\n')
        }}
        ${{
        if mag_control:
            self.write('slideform.%s_magtext.value = ' % slideshow_id +
                       'magnification.toString();\n')
        }}
        that.resizeImage();
    }
'''

    html_box_template = r'''
<tr>
<td style="padding: 20 20 20 20">
<img class="slideshow" src="" name="${slideshow_id}_image_display"
    width="$width" height="$height">
</td>
</tr>
'''

    html_controls_template = r'''
${{
if mag_control or (delay is not None):
    self.write('<tr><td align=center>\n')
    if mag_control:
        self.html_mag_template(vars())
        if delay is not None:
            self.write('<br>\n')
    if delay is not None:
        self.html_delay_template(vars())
    self.write('</td></tr>\n')
}}
'''

    html_mag_template = r'''
magnification: <input type="text" name="${slideshow_id}_magtext"
    onChange="$slideshow_id.changeMag();" value="0" size="2">
<input type=button onClick="$slideshow_id.smaller();" value="-" title="Smaller">
<input type=button onClick="$slideshow_id.larger();" value="+" title="Larger">
'''


class SectionImageHTMLSlideShow(SectionHTMLSlideShow, ImageHTMLSlideShow):
    """Image slideshow with section markers."""

    def __init__(self, filenames, section_ids, image_size, **kwargs):
        """Return the HTML code for a sectioned slideshow of the given images.

        For keyword arguments see the super classes.
        """
        if len(section_ids) != len(filenames):
            err = ("The number of section slideshow_id entries does not match "
                   "the number of slides / filenames.")
            raise Exception(err)
        kwargs.update(vars())
        del kwargs["self"]
        super(SectionImageHTMLSlideShow, self).__init__(**kwargs)

    js_controls_resize_template = r'''
    that.resizeImage = function () {
        document.images.${slideshow_id}_image_display.width =
            parseInt(magnification * original_width, 10);
        document.images.${slideshow_id}_image_display.height =
            parseInt(magnification * original_height, 10);
        // make sure that section ids are nicely line wrapped
        var section_panel_width = 250;
        if (magnification * original_height > section_panel_width) {
            section_panel_width = magnification * original_width;
        }
        document.getElementById("${slideshow_id}_sections_panel").style.width =
            parseInt(section_panel_width, 10) + "px";
    }
'''


### helper functions ###

# TODO: extract image size automatically,
#    but this introduces an optional dependency on PIL

def image_slideshow(filenames, image_size, title=None, section_ids=None,
                    delay=100, delay_delta=20, loop=True, slideshow_id=None,
                    magnification=1, mag_control=True, shortcuts=True):
    """Return a string with the JS and HTML code for an image slideshow.

    Note that the CSS code for the slideshow is not included, so you should
    add SLIDESHOW_STYLE or a custom style to your CSS code.

    filenames -- Sequence of the image filenames.
    image_size -- Tuple (x,y) with the original image size, or enter
        a different size to force scaling.
    title -- Optional slideshow title (for default None not title is shown).
    section_ids -- List with the section id for each slide index. The id
            can be a string or a number. Default value None disables the
            section feature.

    For additional keyword arguments see the ImageHTMLSlideShow class.
    """
    if section_ids:
        slideshow = SectionImageHTMLSlideShow(**vars())
    else:
        slideshow = ImageHTMLSlideShow(**vars())
    return str(slideshow)

def show_image_slideshow(filenames, image_size, filename=None, title=None,
                         section_ids=None, delay=100, delay_delta=20,
                         loop=True, slideshow_id=None,
                         magnification=1, mag_control=True, open_browser=True):
    """Write the slideshow into a HTML file, open it in the browser and
    return a file object pointing to the file. If the filename is not given,
    a temporary file is used, and will be deleted when the returned file object
    is closed or destroyed.

    filenames -- Sequence of the image filenames.
    image_size -- Tuple (x,y) with the original image size, or enter
        a different size to force scaling.
    filename -- Filename for the HTML file to be created. If None
            a temporary file is created.
    title -- Optional slideshow title (for default None not title is shown).
    section_ids -- List with the section id for each slide index. The id
            can be a string or a number. Default value None disables the
            section feature.
    open_browser -- If True (default value) then the slideshow file is
        automatically opened in a webbrowser. One can also use string value
        with the browser name (for webbrowser.get) to request a specific
        browser.

    For additional keyword arguments see the ImageHTMLSlideShow class.
    """
    if filename is None:
        html_file = tempfile.NamedTemporaryFile(suffix=".html", prefix="MDP_")
    else:
        html_file = open(filename, 'w')
    html_file.write('<html>\n<head>\n<title>%s</title>\n' % title)
    html_file.write('<style type="text/css" media="screen">')
    html_file.write(basic_css() + image_slideshow_css())
    html_file.write('</style>\n</head>\n<body>\n')
    kwargs = vars()
    del kwargs['filename']
    del kwargs['open_browser']
    del kwargs['html_file']
    html_file.write(image_slideshow(**kwargs))
    html_file.write('</body>\n</html>')
    html_file.flush()

    if open_browser:
        if isinstance(open_browser, str):
            try:
                custom_browser = webbrowser.get(open_browser)
                custom_browser.open(os.path.abspath(filename))
            except webbrowser.Error:
                err = ("Could not open browser '%s', using default." %
                       open_browser)
                warnings.warn(err)
                webbrowser.open(os.path.abspath(filename))
        else:
            webbrowser.open(os.path.abspath(filename))
    return html_file
