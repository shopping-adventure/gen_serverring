#! /bin/sh

HOST=`hostname`
[ -e $1 ] && echo "$1 already exists" && exit 1

logdir=./ct_multi_logs

[ ! -d $logdir ] && mkdir -p $logdir

cd test
erlc ct_elixir_wrapper.erl
cd ..

for NODE in n1 n2 n3 n4
  do echo "{node, $NODE, $NODE@$HOST}." >> $1
done

cat dist.spec_template >> $1

elixir --erl "-s init stop" \
    --sname ct_master@$HOST \
    -e "\
        Mix.start ; \
        Code.load_file(\"mix.exs\") ; \
        Mix.Project.get! ; \
        Mix.Task.run(\"loadpaths\"); \
        :ct_master.run('$1')"
