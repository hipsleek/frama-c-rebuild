[kernel] Parsing bool.c (with preprocessing)
[eva] Analyzing a complete application starting at main
[eva:progress] Computing initial state
[eva:progress] Initial state computed
[eva:initial-state] Values of globals at initialization
  x ∈ {0}
  y ∈ {0}
[eva:progress] 
  computing for function f <- main.
  Called from bool.c:27.
[eva:progress] bool.c:19: assertion got status valid.
[eva:partition] bool.c:20: starting to merge loop iterations
[eva:alarm] bool.c:20: Warning: signed overflow. assert -2147483648 ≤ i - 1;
[eva:progress] Recording results for f
[eva:progress] Done for function f
[eva:progress] 
  computing for function printf_va_1 <- main.
  Called from bool.c:29.
[eva] using specification for function printf
[eva:progress] bool.c:29: function printf_va_1: precondition got status valid.
[eva:progress] Done for function printf_va_1
[eva:progress] 
  computing for function printf_va_2 <- main.
  Called from bool.c:31.
[eva:progress] bool.c:31: function printf_va_2: precondition got status valid.
[eva:progress] Done for function printf_va_2
[eva:progress] 
  computing for function printf_va_3 <- main.
  Called from bool.c:33.
[eva:progress] bool.c:33: function printf_va_3: precondition got status valid.
[eva:progress] Done for function printf_va_3
[eva:progress] 
  computing for function printf_va_4 <- main.
  Called from bool.c:35.
[eva:progress] bool.c:35: function printf_va_4: precondition got status valid.
[eva:progress] Done for function printf_va_4
[eva:progress] 
  computing for function printf_va_5 <- main.
  Called from bool.c:37.
[eva:progress] bool.c:37: function printf_va_5: precondition got status valid.
[eva:progress] Done for function printf_va_5
[eva:progress] Recording results for main
[eva:progress] Done for function main
[eva] ====== VALUES COMPUTED ======
[eva:final-states] Values at end of function f:
  i ∈ [-2147483648..9]
  j ∈ {5}
  __retres ∈ {0}
[eva:final-states] Values at end of function main:
  x ∈ {1}
  y ∈ {2}
  S___fc_stdout[0..1] ∈ [--..--]
