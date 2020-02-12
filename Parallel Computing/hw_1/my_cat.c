#include <stdio.h>

int main(int argc, char *argv[])
{
  if (argc == 1)
  {
    return(0);
  }

  char c;
  int i;
  FILE *fp;

  for (int i = 1; i < argc; i++)
  {
    fp = fopen(argv[i], "r");
    if (fp == NULL)
    {
      printf("my_cat: cannot open file\n");
      return(1);
    }

    c = fgetc(fp);
    while (c != EOF)
    {
        printf ("%c", c);
        c = fgetc(fp);
    }
  }

  return(0);
}
