/* run.config
  OPT: -keep-comments -print -then -pp-annot -then -ocode ocode_@PTEST_NUMBER@_@PTEST_NAME@.c -then ocode_@PTEST_NUMBER@_@PTEST_NAME@.c
  COMMENT: Tests parsing of several comments that "look like" ACSL but are not
  COMMENT: (e.g. Doxygen-related).
*/

/*@{*/

/*@{ Bla */

//@{

//@{ Blu

void doxygen_group (void) {
}

/*@} Bli */

/*@} */

//@}

//@} Bly


void main(void);


// Code excerpt based on freetype2/freetype/config/ftheader.h:

/*@***********************************************************************/
/*                                                                       */
/* <Macro>                                                               */
/*    BEGIN_HEADER                                                       */
/*                                                                       */
/* <Description>                                                         */
/*    This macro is used in association with @END_HEADER in header       */
/*    files to ensure that the declarations within are properly          */
/*    encapsulated in an `extern "C" { .. }` block when included from a  */
/*    C++ compiler.                                                      */
/*                                                                       */
