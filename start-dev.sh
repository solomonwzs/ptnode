#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin \
    -name "$1@127.0.0.1" \
    -setcookie "test-cookie" \
    -env PT_PROTOCOL "$2" \
    -s ssl \
    -s ptnode \
    -s example_helper \
    -s reloader
