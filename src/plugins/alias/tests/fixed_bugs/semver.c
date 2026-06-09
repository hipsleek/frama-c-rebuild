void f (int* x) {
}

int main(void)
{
	f ((int*)*("1" + 2));
	return 0;
}
