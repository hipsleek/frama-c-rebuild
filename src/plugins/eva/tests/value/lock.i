/* run.config*
   STDOPT: #"-main locks0_good"
 */

/*@ ghost int ghost_loctable[100] ;*/

/*@ axiomatic Locked {
  @   predicate locked{L}(struct mutex *m);
  @      // reads m, ghost_loctable[..] ;
  @
  @   axiom locked_dummy_axiom_for_reads{L} :
  @      \forall struct mutex *m;
  @        locked(m) && ghost_loctable[0] == 0 ==>
  @           locked(m) && ghost_loctable[0] == 0 ;
  @ }
  @*/

/*@
  requires !(locked(m));
  ensures locked(m);
  assigns ghost_loctable[0..99] \from indirect:*m;

 */
void acquire_lock(struct mutex *m);

/*@
  requires locked(m);
  ensures !(locked(m));
  assigns ghost_loctable[..] \from indirect:*m;

 */
void release_lock(struct mutex *m);

/*@
  requires !(locked(m));
  assigns ghost_loctable[..] \from indirect:*m;
  behavior success:
  ensures (\result != 0) ==> locked(m);

  behavior failure:
  ensures (\result == 0) ==> !(locked(m));

 */
int try_acquire_lock(struct mutex *m);

struct mutex { int __id; };
extern struct mutex mutex;

/*@ requires !(locked(&mutex)); */
void locks0_good(int flag)
{
    acquire_lock(&mutex);
    release_lock(&mutex);
}
