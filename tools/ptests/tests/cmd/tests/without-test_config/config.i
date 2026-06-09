/* run.config
OPT: test#0
STDOPT: +"test#1"

MACRO: test_config3 tests/test_config3-redef
OPT: test#2
STDOPT: #"test#3"

MACRO: test_config3 tests/test_config3-redef
CMD: echo CMD @test_config1@ @test_config2@ @test_config3@ @PTEST_OPTIONS@
OPT: test#4
*/
