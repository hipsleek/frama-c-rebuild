//@ import why3: list::Distinct ;

// WP cannot deal with a fully polymorphic value for now
// @ lemma L1 <A> : Distinct::distinct([| |]) ;


//@ lemma L2: Distinct::distinct([| 0 |]) ;
//@ lemma L3: Distinct::distinct([| 1 , 2 |]) ;
