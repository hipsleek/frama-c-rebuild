  $ mkdir -p subdir && cd subdir && dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module eva,report ../alarm.c -add-symbolic-path=..:. -eva -eva-verbose 0 -report-csv report.csv && cat report.csv
  [kernel] Parsing alarm.c (with preprocessing)
  [eva] Warning: The inout plugin is missing: some features are disabled, and the analysis may have degraded precision and performance.
  [eva:alarm] alarm.c:2: Warning: division by zero. assert 0 ≢ 0;
  [eva] Warning: The scope plugin is missing: cannot remove redundant alarms.
  [report] Dumping properties in 'report.csv'
  directory	file	line	function	property kind	status	property
  .	alarm.c	2	main	division_by_zero	Invalid or unreachable	0 ≢ 0
