/* run.config
   DONTRUN:
*/

/* run.config_qualif
   OPT: -wp-status -wp-prover tip -wp-script dry
 */

/*@ lemma dn3:
    \forall unsigned char c, d;
    (c & 0x8E)==2 && (c & 0x01)==1 && (d & 0x8F)==0
    ==>
    ((c+d) & 0x03)==0x03;
*/

/*@
  strategy RangeThenProver :
  \tactic ("Wp.range",
      \pattern(is_uint8(E)) ,
      \select(E), \param("inf",0), \param("sup",255),
      \children(RangeThenProver)
  ),
  \prover("Alt-Ergo");

  proof RangeThenProver: dn3;
*/
