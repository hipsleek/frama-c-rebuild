/* run.config
   MODULE: @PTEST_NAME@
   STDOPT:
*/

/* attribute annotation are not supported anymore, they should be replaced with
   normal attributes which can be registered via the API.
   The script ghost_attribute.ml registers the attribute "registered_attr" on
   types.
*/

/* Not supported anymore, by default annotations will be ignored with a warning
   in parsing phase
*/
void attr_annot(int* p /*@ my_attribute */){
  int /*@ my_attribute */ v ;
}

/*@ ghost
  void attr_annot_ghost(int* p /@ my_attribute @/){
    int /@ my_attribute @/ v ;
  }
*/

/* Unregistered attribute will be kept, but a warning will be emitted, and these
   attributes are ignored when comparing types.
*/
void unregistered_attr(int* p __attribute__((unregistered_attr))){
  int __attribute__((unregistered_attr)) v ;
}

/*@ ghost
  void unregistered_attr_ghost(int* p __attribute__((unregistered_attr))){
    int __attribute__((unregistered_attr)) v ;
  }
*/

void registered_attr(int* p __attribute__((registered_attr))){
  int __attribute__((registered_attr)) v ;
}

/*@ ghost
  void registered_attr_ghost(int* p __attribute__((registered_attr))){
    int __attribute__((registered_attr)) v ;
  }
*/
