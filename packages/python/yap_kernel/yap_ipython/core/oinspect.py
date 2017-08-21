from IPython.core.oinspect import *


from pygments import highlight
from pygments.lexers import PrologLexer
from pygments.formatters import HtmlFormatter


def yaplight(code):
    return highlight(code, PythonLexer(), HtmlFormatter(noclasses=True))

def _get_info(self, obj, oname='', formatter=None, info=None, detail_level=0):
    """Retrieve an info dict and format it.

    Parameters
    ==========

    obj: any
        Object to inspect and return info from
    oname: str (default: ''):
        Name of the variable pointing to `obj`.
    formatter: callable
    info:
        already computed informations
    detail_level: integer
        Granularity of detail level, if set to 1, give more informations.
    """

    info = self._info(obj, oname=oname, info=info, detail_level=detail_level)

    _mime = {
        'text/plain': [],
        'text/html': '',
    }

    def append_field(bundle, title, key, formatter=None):
        field = info[key]
        if field is not None:
            formatted_field = self._mime_format(field, formatter)
            bundle['text/plain'].append((title, formatted_field['text/plain']))
            bundle['text/html'] += '<h1>' + title + '</h1>\n' + formatted_field['text/html'] + '\n'

    def code_formatter(text):
        return {
            'text/plain': self.format(text),
            'text/html': yaplight(text)
        }

    if info['isalias']:
        append_field(_mime, 'Repr', 'string_form')

    elif info['ismagic']:
        if detail_level > 0:
            append_field(_mime, 'Source', 'source', code_formatter)
        else:
            append_field(_mime, 'Docstring', 'docstring', formatter)
        append_field(_mime, 'File', 'file')

    # elif info['isclass'] or is_simple_callable(obj):
    #     # Functions, methods, classes
    #     append_field(_mime, 'Signature', 'definition', code_formatter)
    #     append_field(_mime, 'Init signature', 'init_definition', code_formatter)
    #     if detail_level > 0 and info['source']:
    #         append_field(_mime, 'Source', 'source', code_formatter)
    #     else:
    #         append_field(_mime, 'Docstring', 'docstring', formatter)
    #         append_field(_mime, 'Init docstring', 'init_docstring', formatter)
    #
    #     append_field(_mime, 'File', 'file')
    #     append_field(_mime, 'Type', 'type_name')

    else:
        # General Python objects
        append_field(_mime, 'Signature', 'definition', code_formatter)
        append_field(_mime, 'Call signature', 'call_def', code_formatter)
        append_field(_mime, 'Type', 'type_name')
        append_field(_mime, 'String form', 'string_form')

        # Namespace
        if info['namespace'] != 'Interactive':
            append_field(_mime, 'Namespace', 'namespace')

        append_field(_mime, 'Length', 'length')
        append_field(_mime, 'File', 'file')

        # Source or docstring, depending on detail level and whether
        # source found.
        # if detail_level > 0 and info['source']:
        append_field(_mime, 'Source', 'source', code_formatter)
        # else:
        #     append_field(_mime, 'Docstring', 'docstring', formatter)

        append_field(_mime, 'Class docstring', 'class_docstring', formatter)
        append_field(_mime, 'Init docstring', 'init_docstring', formatter)
        append_field(_mime, 'Call docstring', 'call_docstring', formatter)


    return self.format_mime(_mime)

Inspector._get_info = _get_info
