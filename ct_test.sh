#! /bin/sh

logdir=./ct_logs

[ ! -d $logdir ] && mkdir -p $logdir

ct_run -dir test \
    -logdir $logdir \
    -suite mono_node_SUITE \
    --sname ct_tester@localhost
