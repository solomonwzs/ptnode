#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin \
    -sname "$1" \
    -s ssl \
    -s ptnode \
    -s reloader
