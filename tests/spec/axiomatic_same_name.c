/* run.config*
STDOPT: +"-kernel-warn-key annot-error=warning"
*/

/*@ axiomatic A {
lemma foo: \true;
}
*/

/*@ axiomatic A { // should be rejected: Axiomatic A already exists
lemma bar: \true;
}
*/
