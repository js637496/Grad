#include <stdio.h>

void quicksort(int intArr[], int left, int right)
{
  if (left >= right)
  {
    return;
  }

  int pivot = intArr[right];
  int count = left;

  for (int i = left; i <= right; i++)
  {
    if (intArr[i] <= pivot)
    {
      int temp = intArr[count];
      intArr[count] = intArr[i];
      intArr[i] = temp;
      count++;
    }
  }

  quicksort(intArr, left, count-2);
  quicksort(intArr, count, right);
}

int main(int argc, char *argv[])
{
  int count = 0, x;
  FILE *fp;
  fp = fopen(argv[1], "r");

  //Figure out how many numbers are in the file
  while (!feof (fp))
  {
    fscanf (fp, "%d", &x);
    count++;
  }

  //EOF over shoots by 1
  count = count - 1;
  int intArr[count];

  //Reset file pointer
  rewind(fp);

  for (int i = 0; i <= count; i++)
  {
    fscanf (fp, "%d", &intArr[i]);
  }

  fclose (fp);

  quicksort(intArr, 0, count-1);

  for (int i = 0; i < count; i++)
  {
    printf("%d ", intArr[i]);
  }

  printf("\n");

  return 0;
}
