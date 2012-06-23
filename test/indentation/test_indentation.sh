#!/bin/bash

# This script can be used to test lua-mode indentation engine. For a slightly
# longer description, run it without parameters and consult the output.

# Need to preserve input arguments, because subshells overwrite them
declare -a PARAMS
PARAMS=( "$@" )

set ${EMACS=emacs}
set ${LUA_MODE=$(dirname $0)/../../lua-mode.el}

if [ ${#PARAMS[@]} -eq 0 ]; then
    cat <<EOF
Usage: `basename $0` <FILE> [ <FILE> ... ]

Test lua-mode indentation: each FILE is checked to be indented as FILE.etalon.
The following environment variables are considered:

  - EMACS (default value: emacs)
    specify emacs version to be run

  - LUA_MODE (default value: $(readlink -m $(dirname $0)/../lua-mode.el))
    specify location where tested lua-mode.el resides
EOF
    exit 1
fi

# All output will be redirected into ERROR_LOG temporary file to make sure
# that if everything goes ok, the script will remain silent. In case of an
# error, ERROR_LOG contents may be retrieved and printed onto the screen.
ERROR_LOG=`mktemp` || { echo "can't create tempfile"; exit 1; }
exec 6<>"$ERROR_LOG"
unlink "$ERROR_LOG"
ERROR_LOG=/proc/$$/fd/6

OUTPUT=`mktemp` ||  { echo "can't create temporary output file"; exit 1; }
exec 7<>"$OUTPUT"
unlink "$OUTPUT"
OUTPUT=/proc/$$/fd/7

test_file_indentation() {
    INPUT="$1"
    ETALON="$INPUT.etalon"

    echo -n "Testing $INPUT... "

    test -f "$INPUT"  || { echo "input file '$INPUT' not exists or not a file"; return 1; }
    test -f "$ETALON" || { echo "etalon file '$ETALON' not exists or not a file"; return 1; }

# run emacs with lua-mode and indent input file
# disable backup file creation to prevent emacs from trying to write to procfs
    $EMACS --quick --batch \
        --load $LUA_MODE \
        --eval "(setq make-backup-files nil)" \
        --eval "\
(with-temp-buffer
  ;; lua-mode indents by 3s, that's not even a multiple of tab width (4/8)
  (set-default 'indent-tabs-mode nil) \
  (insert-file-contents \"$INPUT\")
  (lua-mode)

  ;; permit unsafe (e.g. lua-*) local variables and read them
  (setq enable-local-variables :all)
  (hack-local-variables)

  (indent-region (point-min) (point-max))
  (write-file \"$OUTPUT\"))" \
      > $ERROR_LOG 2>&1 \
      || { echo "indentation failed (Emacs log):"; cat <$ERROR_LOG ; return 1; }

    diff -u $ETALON $OUTPUT >$ERROR_LOG 2>&1 \
        || { echo "indentation mismatch:"; cat <$ERROR_LOG; return 1; }

    echo OK
}

FAILED=no

for INPUT in "${PARAMS[@]}"; do
    test_file_indentation "$INPUT" || FAILED=yes
done

test "$FAILED" = "no" || exit 1
