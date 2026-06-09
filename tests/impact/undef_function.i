/* run.config
   STDOPT: +"-impact-annot main"
   */
int y;
void g(int);

int main() {
  /*@ impact_stmt; */
  y=2;
  g(y);
  return y;
}
