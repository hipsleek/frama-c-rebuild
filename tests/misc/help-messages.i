/* run.config
   COMMENT: Test help message for every internalized plug-in. Ideally this
   COMMENT: should be done automatically but then it would not be stable if one
   COMMENT: locally internalize a plug-in.

   FILTER: sed -e 's/^This is Frama-C [1-9][0-9]*.*$/This is Frama-C XX.X/g'
   STDOPT: +"-h"

   STDOPT: +"-kernel-h"

   PLUGIN: acsl-importer
   STDOPT: +"-acsl-import-h"

   PLUGIN: alias
   STDOPT: +"-alias-h"

   PLUGIN: aorai
   STDOPT: +"-aorai-h"

   PLUGIN: callgraph
   STDOPT: +"-cg-h"

   PLUGIN: dive
   STDOPT: +"-dive-h"

   PLUGIN: e-acsl
   STDOPT: +"-e-acsl-h"

   PLUGIN: eva
   STDOPT: +"-eva-h"

   PLUGIN: from
   STDOPT: +"-from-h"

   PLUGIN: impact
   STDOPT: +"-impact-h"

   PLUGIN: inout
   STDOPT: +"-inout-h"

   PLUGIN: instantiate
   STDOPT: +"-instantiate-h"

   PLUGIN: loop-analysis
   STDOPT: +"-loop-h"

   PLUGIN: markdown-report
   STDOPT: +"-mdr-h"

   PLUGIN: metrics
   STDOPT: +"-metrics-h"

   PLUGIN: eva
   STDOPT: +"-mt-h"

   PLUGIN: nonterm
   STDOPT: +"-nonterm-h"

   PLUGIN: obfuscator
   STDOPT: +"-obfuscator-h"

   PLUGIN: occurrence
   STDOPT: +"-occurrence-h"

   PLUGIN: pdg
   STDOPT: +"-pdg-h"

   PLUGIN: reduc
   STDOPT: +"-reduc-h"

   PLUGIN: region
   STDOPT: +"-region-h"

   PLUGIN: report
   STDOPT: +"-report-h"

   PLUGIN: rtegen
   STDOPT: +"-rte-h"

   PLUGIN: scope
   STDOPT: +"-scope-h"

   PLUGIN: security_slicing
   STDOPT: +"-security-slicing-h"

   PLUGIN: constant_propagation
   STDOPT: +"-scf-h"

   PLUGIN: api_generator
   STDOPT: +"-server-tsc-h"

   PLUGIN: slicing
   STDOPT: +"-slicing-h"

   PLUGIN: sparecode
   STDOPT: +"-sparecode-h"

   PLUGIN: studia
   STDOPT: +"-studia-h"

   PLUGIN: volatile
   STDOPT: +"-volatile-h"

   PLUGIN: wp
   FILTER: sed -e 's+\(proof process .default: \).*+\1<nproc>\)+g'
   STDOPT: +"-wp-h"

   PLUGIN: server
   ENABLED_IF: %{lib-available:zmq}
   STDOPT: +"-server-h"
*/
