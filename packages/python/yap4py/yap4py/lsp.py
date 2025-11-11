#!/home/vsc/venv/bin/python3
###########################################################################
# Copyright(c) Open Law Library. All rights reserved.                      #
# See ThirdPartyNotices.txt in the project root for additional notices.    #
#                                                                          #
# Licensed under the Apache License, Version 2.0 (the "License")           #
# you may not use this file except in compliance with the License.         #
# You may obtain a copy of the License at                                  #
#                                                                          #
#     http: // www.apache.org/licenses/LICENSE-2.0                         #
#                                                                          #
# Unless required by applicable law or agreed to in writing, software      #
# distributed under the License is distributed on an "AS IS" BASIS,        #
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. #
# See the License for the specific language governing permissions and      #
# limitations under the License.                                           #
############################################################################
"""This implements the various Goto "X" requests from the specification.

- :lsp:`textDocument/definition`
- :lsp:`textDocument/declaration`
- :lsp:`textDocument/implementation`
- :lsp:`textDocument/typeDefinition`

Along with the :lsp:`textDocument/references` request.

As you will see all of these methods are essentially the same, they accept a document
uri and they return zero or more locations (even goto definition can return multiple
results!).
The only difference between them are whatever the semantic differences are between say a
definition and a declaration in your target language.

This means the choices of what the example server below will return results for are
completely arbitrary.
"""

import logging
import re

from collections import namedtuple

from yap4py.yapi import Engine, EngineArgs, set_prolog_flag


pred_refs = namedtuple("pred_refs","ls uri line position")
add_dir = namedtuple("add_dir", "ls uri")
validate_text = namedtuple("validate_text", "ls uri source")
complete = namedtuple("complete","ls line position subline")
open_uri = namedtuple("open_uri","uri")
pred_def = namedtuple("pred_def","ls word" )
highlight_uri = namedtuple("highlight_uri", "ls uri data")


from lsprotocol import types

from pygls.cli import start_server
from pygls.lsp.server import LanguageServer
from pygls.workspace import TextDocument

ARGUMENT = re.compile(r"(?P<name>\w+): (?P<type>\w+)")
FUNCTION = re.compile(r"^fn ([a-z]\w+)\(")
TYPE = re.compile(r"^type ([A-Z]\w+)\(")


class YAPLanguageServer(LanguageServer):
    """Language server demonstrating the various "Goto X" methods in the LSP
    specification."""

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        eargs = EngineArgs({})
        eargs.jupyter = True
        self.engine =Engine( eargs)
        self.engine.load_library("lsp")
        self.index = {}

    def sev_as_enum(self, sev_as_string):
        if sev_as_string == 'warning':
            return types.DiagnosticSeverity.Warning
        else:
            return types.DiagnosticSeverity.Error        
        

def validate_yap(ls,uri,source,version):
    """Validates prolog file"""
    try:
        ls.errors[uri] = []
        logging.info(str(ls.errors))
        logging.info(str(version))
        ls.engine.goal(validate_text(ls,uri,source))
    except:
        logging.info("validation failed")
    logging.info(str(ls.errors))
    if ls.errors:
        for (myuri, uri_diags) in ls.errors.items():
            ls.text_document_publish_diagnostics(
                types.PublishDiagnosticsParams(
                    uri=myuri,
                    version=version,
                    diagnostics=
                    [types.Diagnostic(
                        message=msg,
                        severity = ls.sev_as_enum(sev),
                        range=types.Range(
                            start=types.Position(line=line - 1, character=col),
                            end=types.Position(line=line - 1, character=col+1)
                        ),
                    )
                     for (sev, msg,line,col) in uri_diags]
                )
            )

def init_doc(ls, uri, text_document):
    try:
        source = text_document.source
        ls.errors = {}
        ls.errors[uri] = []
        return validate_yap(ls, uri,source,text_document.version)
        
    except Exception as e:
        logging.info(f'Error ocurred: {e}')
        print(uri)
        print(text_document)

def semantic_tokens(self, uri, doc: TextDocument):
    """See https://microsoft.github.io/language-server-protocol/specification#textDocument_semanticTokens
        for details on how semantic tokens are encoded.
        """
    last_line = 0
    last_start = 0          
    self.data =[]
    self.engine.goal(highlight_uri(self,uri,doc.source))
    return types.SemanticTokens(data=self.data)


server = YAPLanguageServer("YAP-server", "v1")





@server.feature(
    types.TEXT_DOCUMENT_SEMANTIC_TOKENS_FULL,
    types.SemanticTokensLegend(
        token_types = [
            'namespace',
            'class',
            'struct',
             'parameter',
            'variable',
            'method',
            'keyword',
            'modifier',
            'comment',
            'string',
            'number',
            'operator'],
        token_modifiers = [
            'declaration',
	        'definition',
            'readonly',
            'static',
            'deprecated',
            'abstract',
            'async',
            'modification',
            'documentation',
            'defaultLibrary'
        ]
    )
)



@server.feature(types.TEXT_DOCUMENT_TYPE_DEFINITION)
def goto_type_definition(ls: YAPLanguageServer, params: types.TypeDefinitionParams):
    """Jump to an object's type definition."""
    doc = ls.workspace.get_text_document(params.text_document.uri)
    index = ls.index.get(doc.uri)
    if index is None:
        return

    try:
        line = doc.lines[params.position.line]
    except IndexError:
        line = ""

    word = doc.word_at_position(params.position)

    for match in ARGUMENT.finditer(line):
        if match.group("name") == word:
            if (range_ := index["types"].get(match.group("type"), None)) is not None:
                return types.Location(uri=doc.uri, range=range_)


@server.feature(types.TEXT_DOCUMENT_DEFINITION)
def goto_definition(ls: YAPLanguageServer, params: types.DefinitionParams):
    """Jump to an object's definition."""
    doc = ls.workspace.get_text_document(params.text_document.uri)
    index = ls.index.get(doc.uri)
    if index is None:
        return

    word = doc.word_at_position(params.position)
    try:
        ls.defs=[]
        ls.engine.goal(pred_def(ls,word))
        if ls.defs:
            infg = ls.defs[0]
            linum = infg[1]
            uri="file://"+infg[0]
            start_char =infg[2]
            
            return types.Location(uri=doc.uri, range=types.Range(
                                  start=types.Position(line=linum, character=start_char),
                    end=types.Position(line=linum, character=start_char + len(word))))
    except Exception as e:
        logging.info(f'Error ocurred: {e}')

    # # Is word a type?
    # if (range_ := index["types"].get(word, None)) is not None:
    #     return types.Location(uri=doc.uri, range=range_)


@server.feature(types.TEXT_DOCUMENT_DECLARATION)
def goto_declaration(ls: YAPLanguageServer, params: types.DeclarationParams):
    """Jump to an object's declaration."""
    doc = ls.workspace.get_text_document(params.text_document.uri)
    index = ls.index.get(doc.uri)
    if index is None:
        return

    try:
        line = doc.lines[params.position.line]
    except IndexError:
        line = ""

    word = doc.word_at_position(params.position)

    for match in ARGUMENT.finditer(line):
        if match.group("name") == word:
            linum = params.position.line
            return types.Location(
                uri=doc.uri,
                range=types.Range(
                    start=types.Position(line=linum, character=match.start()),
                    end=types.Position(line=linum, character=match.end()),
                ),
            )



@server.feature(types.TEXT_DOCUMENT_DID_OPEN)
def did_open(ls:YAPLanguageServer , params: types.DidOpenTextDocumentParams):
    """Parse each document when it is opened"""
    try:
        uri = params.text_document.uri
        doc = ls.workspace.get_text_document(uri)
        init_doc(ls, uri, doc)
        return semantic_tokens(ls,uri,doc)

    except Exception as e:
        logging.info(f'Error ocurred: {e}')
        print(uri)
        print(document)

@server.feature(types.TEXT_DOCUMENT_DID_CHANGE)
def did_change(ls:YAPLanguageServer , params: types.DidOpenTextDocumentParams):
    """Parse each document when it is changed"""
    uri = params.text_document.uri
    doc = ls.workspace.get_text_document(uri)
    return init_doc(ls,uri,doc)
    return semantic_tokens(ls,uri,doc)

@server.feature(
    types.TEXT_DOCUMENT_COMPLETION,
    types.CompletionOptions(trigger_characters=[",","\t"], all_commit_characters=["\n"]),
)
def completions(ls: YAPLanguageServer, params: types.CompletionParams = None) -> types.CompletionList:
    """Returns completion items."""
    ls.items = []
    document = ls.workspace.get_text_document(params.text_document.uri)
    current_line = document.lines[params.position.line]
    print(current_line,params.position.character)
    try:
        ls.engine.goal(complete(ls,current_line,params.position.character,current_line[:params.position.character].strip()))
    except Exception as e:
        logging.info(f'Error ocurred: {e}')                       
    return types.CompletionList(
        is_incomplete=True,
        items=[types.CompletionItem(label=i) for i in ls.items]
    )

@server.feature(types.TEXT_DOCUMENT_IMPLEMENTATION)
def goto_implementation(ls: YAPLanguageServer, params: types.ImplementationParams):
    """Jump to an object's implementation."""
    doc = ls.workspace.get_text_document(params.text_document.uri)
    index = ls.index.get(doc.uri)
    if index is None:
        return

    word = doc.word_at_position(params.position)

    # Is word a function?
    if (range_ := index["functions"].get(word, None)) is not None:
        return types.Location(uri=doc.uri, range=range_)


@server.feature(types.TEXT_DOCUMENT_REFERENCES)
def find_references(ls: YAPLanguageServer, params: types.ReferenceParams):
    """Find references of an object."""
    doc = ls.workspace.get_text_document(params.text_document.uri)
    index = ls.index.get(doc.uri)
    if index is None:
        return

    word = doc.word_at_position(params.position)
    is_object = any([word in index[name] for name in index])
    if not is_object:
        return

    references = []
    for linum, line in enumerate(doc.lines):
        for match in re.finditer(f"\\b{word}\\b", line):
            references.append(
                types.Location(
                    uri=doc.uri,
                    range=types.Range(
                        start=types.Position(line=linum, character=match.start()),
                        end=types.Position(line=linum, character=match.end()),
                    ),
                )
            )

    return references


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format="%(message)s")
    start_server(server)
