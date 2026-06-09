struct foo {
  int i;
};

int main() {
  struct foo *foo = (struct foo *)main; // generates warning
  float f = 1.0/0.0; // generates alarm
}
