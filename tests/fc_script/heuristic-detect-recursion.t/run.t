  $ PTESTS_TESTING=1 frama-c-script heuristic-detect-recursion heuristic-detect-recursion.i
  recursive cycle detected: 
    heuristic-detect-recursion.i:44: direct_rec -> direct_rec
  recursive cycle detected: 
    heuristic-detect-recursion.i:8: f -> f
  recursive cycle detected: 
    heuristic-detect-recursion.i:13: h -> h
  recursive cycle detected: 
    heuristic-detect-recursion.i:54: indirect_rec1 -> indirect_rec2
    heuristic-detect-recursion.i:50: indirect_rec2 -> indirect_rec1
  recursive cycle detected: 
    heuristic-detect-recursion.i:29: k -> l
    heuristic-detect-recursion.i:33: l -> m
    heuristic-detect-recursion.i:37: m -> k
  recursive cycle detected: 
    heuristic-detect-recursion.i:69: multiple_indirect1 -> multiple_indirect2
    heuristic-detect-recursion.i:70: multiple_indirect1 -> multiple_indirect2
    heuristic-detect-recursion.i:64: multiple_indirect2 -> multiple_indirect1
    heuristic-detect-recursion.i:65: multiple_indirect2 -> multiple_indirect1
