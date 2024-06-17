#include <stdio.h>
int writeln(int a) { return printf("%d\n", a); }
int write(int a) { return printf("%d ", a); }
int readln(int *a) { return scanf("%d", a); }
