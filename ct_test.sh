#! /bin/sh

logdir=./ct_logs

[ ! -d $logdir ] && mkdir -p $logdir

ct_run -dir test -logdir $logdir --sname ct_tester@localhost
