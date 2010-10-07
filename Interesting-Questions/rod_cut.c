#include <stdio.h>
#include <stdlib.h>

int Price[11]   = {0, 1, 5, 8, 9, 10, 17, 17, 20, 24, 30};
int Revenue[11] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
int Lenght[11]  = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
int Sltn[11]    = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

void extended_cut_rod(int n) {
  int i = 0;
  int j = 0;
  int q = -1;

  // for each smaller section of "n"
  for(i = 1; i <= n; i++) {
    q = -1;
    // solve the sub problem by selecting the right place to cut for
    // each sub problem
    for (j = 1; j <= i; j++) {
      // at any time, if we see that a newer cut location is giving more 
      // revenue then, update the revenue array and the cut position
      if (q < Price[j] + Revenue[i - j]) {
	q = Price[j] + Revenue[i - j];
	Sltn[i] = j;
      }
    }
    
    Revenue[i] = q;

  }
  
  return;
}

void extended_cut_rod_fc(int n) {
  int i = 0;
  int j = 0;
  int q = -1;

  // for each smaller section of "n"
  for(i = 1; i <= n; i++) {
    q = -1;
    // solve the sub problem by selecting the right place to cut for
    // each sub problem
    for (j = 1; j <= i; j++) {
      // at any time, if we see that a newer cut location is giving more 
      // revenue then, update the revenue array and the cut position
      if (q < Price[j] + Revenue[i - j] && i - j == 0) { // no loss if there is not cut
	q = Price[j] + Revenue[i - j];
	Sltn[i] = j;
      } else if(q < Price[j] + Revenue[i - j] - 1) { // there is loss but 1 if there is a cut
	q = Price[j] + Revenue[i - j] - 1;
	Sltn[i] = j;
      }
    }
    
    Revenue[i] = q;

  }
  
  return;
}

int main(void) {
  int max = 10;
  int i   = 0;
  extended_cut_rod_fc(max);
  
  for (i = 0; i <= max; i++)
    printf("%d\t", Lenght[i]);
  printf("\n");
  for (i = 0; i <= max; i++)
    printf("%d\t", Revenue[i]);
  printf("\n");
  for (i = 0; i <= max; i++)
    printf("%d\t", Sltn[i]);
  printf("\n");

  return 0;
}
