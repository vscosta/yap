site_name: YAP Prolog Reference Manual
site_url: https://www.dcc.fc.up.pt/YAP
use_directory_urls: false
theme:
  name: 'readthedocs'
  highlightjs: true
  hljs_languages:
    - prolog
    - c
    - python
    - c++
    - java
    - javascript
    - R
  logo: 'img/favicon.ico'
plugins:
  - search
  - autorefs
  - mkdoxy:
      # debug: true
      ignore-errors: true
      projects:
        YAP:
          src-dirs:       ${CMAKE_SOURCE_DIR}/CXX  
          full-doc: True
          doxy-cfg-file: ${CMAKE_BINARY_DIR}/Doxyfile.dox

markdown_extensions:
  - attr_list
  - def_list
  - toc:
      permalink: True
  - admonition
  - markdown.extensions.md_in_html

nav:
   - Home: index.md
   - INSTALL: INSTALL.md
   - Calling YAP: CALLING_YAP.md
   -   Built-ins:
       - Core: group__Builtins.md
       - Input-Output: group__InputOutput.md
   - Programming: group__YAPProgramming.md
   - Extensions: group__YapExtensions.md
   - Library: group__YAPLibrary.md
   - Available Packages: group__YAPPackages.md
   - Foreign Language Interface : group__YAPAPI.md
   - Indices:
       - modules.md
       - namespaces.md
       - links.md
       - Implementation:
           - classes.md
           - files.md

