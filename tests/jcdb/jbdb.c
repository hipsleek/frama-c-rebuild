/* run.config
OPT: -compilation-db %{dep:./build_commands.json} -print
*/

int f2 () {
  return RETCODE; // defined in build_commands.json
}
