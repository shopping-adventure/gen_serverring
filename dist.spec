{node, n1, n1@cantor}.
{node, n2, n2@cantor}.
{node, n3, n3@cantor}.
{node, n4, n4@cantor}.

{logdir, all_nodes, "./ct_multi_logs"}.
{logdir, master, "./ct_multi_logs"}.

{init, [n1, n2, n3, n4],
    {node_start, [
        {monitor_master, true},
        {boot_timeout, 10},
        {init_timeout, 5},
        {startup_timeout, 5},
        {startup_functions, [
            {code, add_patha, ["./test"]},
            {ct_elixir_wrapper, apps_path, []},
            {ct_elixir_wrapper, elixir_init, []}
        ]}
    ]}
}.

{suites, [n1], "./test", writer_SUITE}.
{suites, [n2], "./test", reader_SUITE}.
{suites, [n3], "./test", monitor_SUITE}.
{suites, [n4], "./test", crasher_SUITE}.
