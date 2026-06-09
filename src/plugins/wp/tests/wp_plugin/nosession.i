/* run.config
   DONTRUN:
*/
/* run.config_qualif
   EXIT: 1
   CMD: FRAMAC_WP_CACHEDIR= @frama-c@ -wp-share @PTEST_SHARE_DIR@ -wp-msg-key shell -wp-warn-key pedantic-assigns=inactive
PLUGIN: wp,rtegen
   OPT: -wp -wp-session shall_not_exists_dir -wp-cache offline
   COMMENT: The session directory shall not be created
 */

//@ ensures \false ;
void f(void) { return; }
