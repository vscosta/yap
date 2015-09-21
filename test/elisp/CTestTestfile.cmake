# CMake generated Testfile for 
# Source directory: /Users/vsc/.emacs.d/elpa/irony-20141030.1555/server/test/elisp
# Build directory: /Users/vsc/Yap/yap-6.3/test/elisp
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(irony-el "/Applications/emacs.app/Contents/MacOS/Emacs" "-batch" "-l" "package" "--eval" "(package-initialize) (unless (require 'cl-lib nil t) (package-refresh-contents) (package-install 'cl-lib))" "-l" "/Users/vsc/.emacs.d/elpa/irony-20141030.1555/server/test/elisp/irony.el" "-f" "ert-run-tests-batch-and-exit")
add_test(irony-cdb-el "/Applications/emacs.app/Contents/MacOS/Emacs" "-batch" "-l" "package" "--eval" "(package-initialize) (unless (require 'cl-lib nil t) (package-refresh-contents) (package-install 'cl-lib))" "-l" "/Users/vsc/.emacs.d/elpa/irony-20141030.1555/server/test/elisp/irony-cdb.el" "-f" "ert-run-tests-batch-and-exit")
