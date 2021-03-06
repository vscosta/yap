"""Simple magics for display formats"""
#-----------------------------------------------------------------------------
#  Copyright (c) 2012 The yap_ipython Development Team.
#
#  Distributed under the terms of the Modified BSD License.
#
#  The full license is in the file COPYING.txt, distributed with this software.
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Imports
#-----------------------------------------------------------------------------

# Our own packages
from yap_ipython.core.display import display, Javascript, Latex, SVG, HTML, Markdown
from yap_ipython.core.magic import  (
    Magics, magics_class, cell_magic
)

#-----------------------------------------------------------------------------
# Magic implementation classes
#-----------------------------------------------------------------------------


@magics_class
class DisplayMagics(Magics):
    """Magics for displaying various output types with literals

    Defines javascript/latex/svg/html cell magics for writing
    blocks in those languages, to be rendered in the frontend.
    """

    @cell_magic
    def js(self, line, cell):
        """Run the cell block of Javascript code
        
        Alias of `%%javascript`
        """
        self.javascript(line, cell)

    @cell_magic
    def javascript(self, line, cell):
        """Run the cell block of Javascript code"""
        display(Javascript(cell))


    @cell_magic
    def latex(self, line, cell):
        """Render the cell as a block of latex

        The subset of latex which is support depends on the implementation in
        the client.  In the Jupyter Notebook, this magic only renders the subset
        of latex defined by MathJax
        [here](https://docs.mathjax.org/en/v2.5-latest/tex.html)."""
        display(Latex(cell))

    @cell_magic
    def svg(self, line, cell):
        """Render the cell as an SVG literal"""
        display(SVG(cell))

    @cell_magic
    def html(self, line, cell):
        """Render the cell as a block of HTML"""
        display(HTML(cell))
        
    @cell_magic    
    def markdown(self, line, cell):
        """Render the cell as Markdown text block"""
        display(Markdown(cell))
