/* run.config*
   DEPS: @PTEST_NAME@.mlw
   OPT: -wp-library "."
*/

// Builtin types + synonyms

//@ import why3: int::Int \as I ;
//@ import why3: real::Real \as R ;
//@ import why3: bool::Bool \as B ;

//@ import why3: import_unitex::Builtin \as BI ;

/*@ predicate compat_int(integer a, BI::bint b) = a == b ;
    lemma L_compat_int_1: compat_int(I::zero, 0) ;
    lemma L_compat_int_2: compat_int(0, I::zero) ;
*/
/*@ predicate compat_real(real a, BI::breal b) = a == b ;
    lemma L_compat_real_1: compat_real(R::zero, 0) ;
    lemma L_compat_real_2: compat_real(0, R::zero) ;
*/
/*@ predicate compat_bool(boolean a, BI::bbool b) = B::andb(a, b) == \true ;
    lemma L_compat_bool: compat_bool(\true, \true) ;
*/

// Type without definition

//@ import why3: import_unitex::No_def ;
//@ import why3: import_unitex::No_def_syn ;
//@ lemma L_no_def: No_def::x == No_def_syn::y ;

// ADT

//@ import why3: import_unitex::ADT ;

/*@ predicate compat_adt(ADT::t<real> a, ADT::s<real> b) = a == b ;
    lemma L_compat_adt_1: compat_adt(ADT::A, ADT::A) ;
    lemma L_compat_adt_2: compat_adt(ADT::B(1), ADT::B(1)) ;
    lemma L_compat_adt_3: compat_adt(ADT::C(0.), ADT::C(0.)) ;
*/

// Range

//@ import why3: import_unitex::Range \as Rg ;

/*@ lemma L_rg_1:
      \forall integer i ;
        -42 <= i <= 42 ==> Rg::to_int (Rg::of_int (i)) == i ;
*/

//@ lemma L_rg_2: \forall Rg::r42 i ; -42 <= Rg::to_int (i) <= 42 ;

// Float

//@ import why3: import_unitex::Float8 \as F8 ;

//@ lemma L_f8: F8::is_finite(F8::f1) ;

// Symbols

//@ import why3: import_unitex::Symbols \as S1 ;
//@ import why3: import_unitex::Symbols \as S2 ;
//@ import why3: import_unitex::Symbols ;
//@ import why3: import_unitex::Symbols ; // should not fail

//@ lemma L_sym_1: S1::(==)(S1::(!)(0), 42) ;
//@ lemma L_sym_2: S2::pred(S2::func(0), 42) ;
//@ lemma L_sym_3: Symbols::pos(4);
