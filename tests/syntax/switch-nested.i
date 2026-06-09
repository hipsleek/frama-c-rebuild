int main(int x, int y) {
  for (int i = 0; i < 4; i++) {
    switch (x) {
      case 1:
        return 1;
      case 2:
        switch (y) {
          case 3:
            return 3;
          default:
            return 4;
        }
      default:
        return 5;
    }
  }
}
