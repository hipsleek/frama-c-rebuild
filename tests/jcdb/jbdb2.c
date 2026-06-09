/* run.config
OPT: -compilation-db %{dep:./build_commands.json} -print
*/

int f1 () {
  return RETCODE; // defined in build_commands.json
}
