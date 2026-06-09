/*@ region *p; */
void readwrite(int *p) { *p = 1; }

/*@ region *p, \readonly; */
void readonly(int *p) { *p = 1; }
