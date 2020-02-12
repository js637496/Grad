#include <stdio.h>

int main(int argc, char *argv[])
{
  char first_name[sizeof(argv[1])/sizeof(char)], last_name[sizeof(argv[2])/sizeof(char)];
  int hours_worked;
  double hourly_rate, total_payment;

  sscanf(argv[1], "%s", first_name);
  sscanf(argv[2], "%s", last_name);
  sscanf(argv[3], "%d", &hours_worked);
  sscanf(argv[4], "%lf", &hourly_rate);

  total_payment = hours_worked * hourly_rate;

  printf("%s, %s: %.02lf\n", last_name, first_name, total_payment);

  return 0;
}
