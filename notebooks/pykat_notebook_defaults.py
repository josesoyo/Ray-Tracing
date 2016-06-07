from __future__ import absolute_import
from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

# importing a few useful things
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import math
import pykat

from pykat import finesse

print("Imported matplotlib.pyplot as plt");
print("Imported numpy as np");

from IPython.display import display, HTML
from IPython.utils.ipstruct import Struct
from IPython.core.magic import Magics, magics_class, line_magic

HTML_TEMPLATE = """<style>
{}
</style>
{}
"""

# running in 'capture' to suppress install message
from IPython.utils import io

with io.capture_output() as captured:
	# importing ipython to run magic functions from script
	from IPython import get_ipython
	ipython = get_ipython()
	ipython.magic("matplotlib inline");

# applying custom CSS file
style="""
<style>
div.html_cell_render{
}
		
.rendered_html pre,
.rendered_html code {
}

div.text_cell_render {
font-family: "Helvetica Neue", Arial, Helvetica, Geneva, sans-serif;
/*font-family: "Charis SIL", serif;  Make non-code text serif. */
line-height: 145%; /* added for some line spacing of text. */
width: 105ex; /* instead of 'inherit' for shorter lines */
font-size: 12pt;
}

div.text_cell_render h1,
div.text_cell_render h2,
div.text_cell_render h3,
div.text_cell_render h4,
div.text_cell_render h5,
div.text_cell_render h6 {
/*    font-family: 'Kameron';*/
    font-weight: 300;
}

div.text_cell_render h1 {
    font-size: 24pt;
}

div.text_cell_render h2 {
    font-size: 18pt;
}

div.text_cell_render h3 {
    font-size: 14pt;
}

.rendered_html pre,
.rendered_html code {
    font-size: medium;
}

.rendered_html ol {
    list-style:decimal;
    margin: 1em 2em;
}


div.cell.code_cell {
}
</style>
"""

display(HTML(style));

print("You can now use 'show_finesse(kat)' to display the Finesse code of a 'kat' object");

# function to display finesse file with source highlighting
def show_finesse(kat):
	[print(x.replace('\n','')) for x in kat.generateKatScript()];
    

