void array_blob() {
  int arr[3];
  int* p = &arr[1];
}

void struct_blob() {
  struct { int f; char g; } y;
  int *p = &y.f;
}

void var_blob() {
  int x;
  int* p = &x;
}
