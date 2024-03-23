#!/bin/sh
#
# Tests writing JSON problem files
#
# Requires jd: https://github.com/josephburnett/jd
#

# locate the executable for jd
jd=jd
if [ ! $(command -v jd) ]; then
    if [ -f /opt/jd/jd ]; then
        jd=/opt/jd/jd
    else
        echo "error: unable to locate jd executable" 1>&2
        exit 1
    fi
fi

# locate our lisp. assume sbcl for now...
cl=sbcl
if [ ! $(command -v $cl) ]; then
    echo "error: unable to find common lisp executable (assumed sbcl for now)" 1>&2
    exit 1
fi

# locate the semgus parser
parser=semgus-parser
if [ ! $(command -v $parser) ]; then
    echo "error: unable to find semgus parser" 1>&2
    exit 1
fi

# build the tool
$cl --non-interactive --eval '(asdf:make "com.kjcjohnson.synthkit/test.json-export")'


tool=bin/synthkit-json-export
if [ ! -f $tool ]; then
    echo "error: unable to build json export tool"
    exit 1
fi

for bm in benchmarks/*.sl; do
    echo "=== ${bm} ==="
    $parser --format json --mode batch --output "${bm}.json" --no-legacy-symbols "$bm"
    $parser --format sexpr --output "${bm}.sexpr" "$bm"
    $tool "${bm}.sexpr"
    $jd -mset "${bm}.json" "${bm}.sexpr.test.json"
done
