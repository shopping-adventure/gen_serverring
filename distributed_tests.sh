#! /bin/sh

HOST=`hostname`
[ -e $1 ] && echo "$1 already exists" && exit 1

logdir=./ct_multi_logs

[ ! -d $logdir ] && mkdir -p $logdir

for NODE in n1 n2 n3 n4
  do echo "{node, $NODE, $NODE@$HOST}." >> $1
done

cat dist.spec_template >> $1

erl -sname ct_master@$HOST -run ct_master run $1 -s init stop
