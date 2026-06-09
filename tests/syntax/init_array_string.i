char* ptr = "A" "B";

char* ptr2 = ("A" "B");

char a[] = "A" "B";

char b[] = ("A" "B");

char c[] = "ABC";

char d[] = ("ABC");

struct S {
  char c[42];
};

struct S s = { .c = ("foo") };

char as[3][4] = { ("bar"), [2] = ("bla") };
