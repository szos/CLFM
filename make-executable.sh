#!/bin/sh

LISP=$(which sbcl)
exit="$?"

if [ ! "$exit" -eq 0 ]; then
    /bin/echo -e "\e[1;31mSBCL not installed, exiting\e[0m"
    exit 1
else
    /bin/echo -e "\e[1;32mSBCL is installed, proceeding\e[0m"
fi

sbcl --eval "(progn (handler-case (in-package :ql) (PACKAGE-DOES-NOT-EXIST () (exit :code 1))) (sb-ext:exit :code 0))" > /dev/null 2>&1

exit="$?"

if [ ! "$exit" -eq 0 ]; then
    /bin/echo -e "\e[1;31mQuicklisp not installed, exiting\e[0m"
    exit 1
else
    /bin/echo -e "\e[1;32mQuicklisp is installed, proceeding\e[0m"
fi

sbcl --eval "(push #p\"./\" asdf:*central-registry*)" --eval "(ql:quickload :clfm-2)" --eval "(clfm-2::make-executable)"

exit="$?"

if [ ! "$exit" -eq 0 ]; then
    /bin/echo -e "\e[1;31m:CLFM not found, aborting\e[0m"
    exit 1
fi

