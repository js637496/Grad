#include <stdio.h>

int main(int argc, char *argv[])
{
  int intArr[9], sum = 0;
  FILE *fp;
  fp = fopen(argv[1], "r");

  for (int i = 0; !feof (fp) && i <= sizeof(intArr) / sizeof(int); i++)
  {
    fscanf (fp, "%d", &intArr[i]);
    printf("%d\n", intArr[i]);
    sum += intArr[i];
  }

  fclose (fp);

  printf("%d\n", sum);

  return 0;
}
