// This file contains a definition for function 'external', but it is _not_
// included when parsing 'make-wrapper.c'. This triggers a make-wrapper message.
int external(int a) {
  return a + 3;
}
