int x;

void named_1(void){
  x = 4;
Init: Pre: Old: Post: Here: LoopCurrent: LoopEntry: A:;
  x = 5;
X: Y: ;
  x = 6;
Z: T: ;
  x = 7;
}

void named_2(void){
  x = 4;
A: Init: Pre: Old: Post: Here: LoopCurrent: LoopEntry: ;
  x = 5;
X: Y: ;
  x = 6;
Z: T: ;
  x = 7;
}

void named_3(void){
  x = 4;
Init: Pre: Old: Post: A: Here: LoopCurrent: LoopEntry: ;
  x = 5;
X: Y: ;
  x = 6;
Z: T: ;
  x = 7;
}
