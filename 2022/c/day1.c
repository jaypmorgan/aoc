/**
 *   \file day1.c
 *   \brief Day 1 -- Advent of code -- Calorie Counting
 *
 * Santa's reindeer typically eat regular reindeer food, but they need
 * a lot of magical energy to deliver presents on Christmas. For that,
 * their favorite snack is a special type of star fruit that only
 * grows deep in the jungle. The Elves have brought you on their
 * annual expedition to the grove where the fruit grows.

 * To supply enough magical energy, the expedition needs to retrieve a
 * minimum of fifty stars by December 25th. Although the Elves assure
 * you that the grove has plenty of fruit, you decide to grab any
 * fruit you see along the way, just in case.

 * Collect stars by solving puzzles. Two puzzles will be made
 * available on each day in the Advent calendar; the second puzzle is
 * unlocked when you complete the first. Each puzzle grants one
 * star. Good luck!

 * The jungle must be too overgrown and difficult to navigate in
 * vehicles or access from the air; the Elves' expedition
 * traditionally goes on foot. As your boats approach land, the Elves
 * begin taking inventory of their supplies. One important
 * consideration is food - in particular, the number of Calories each
 * Elf is carrying (your puzzle input).

 * The Elves take turns writing down the number of Calories contained
 * by the various meals, snacks, rations, etc. that they've brought
 * with them, one item per line. Each Elf separates their own
 * inventory from the previous Elf's inventory (if any) by a blank
 * line.

 * For example, suppose the Elves finish writing their items' Calories
 * and end up with the following list:
 *
 * 1000
 * 2000
 * 3000
 *
 * 4000
 *
 * 5000
 * 6000
 *
 * 7000
 * 8000
 * 9000
 * 
 * 10000
 *
 * This list represents the Calories of the food carried by five
 * Elves:
 * 
 * The first Elf is carrying food with 1000, 2000, and 3000 Calories,
 * a total of 6000 Calories.
 *
 * The second Elf is carrying one food item with 4000 Calories.
 *
 * The third Elf is carrying food with 5000 and 6000 Calories, a total
 * of 11000 Calories.
 *
 * The fourth Elf is carrying food with 7000, 8000, and 9000 Calories,
 * a total of 24000 Calories.
 *
 * The fifth Elf is carrying one food item with 10000 Calories.
 *
 * In case the Elves get hungry and need extra snacks, they need to
 * know which Elf to ask: they'd like to know how many Calories are
 * being carried by the Elf carrying the most Calories. In the example
 * above, this is 24000 (carried by the fourth Elf).

 * Find the Elf carrying the most Calories. How many total Calories is
 * that Elf carrying?
 *
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void print_array(int *arr, int size)
{
  for (int i = 0; i < size; ++i) {
    printf("%d, ", arr[i]);
  }
  printf("\n");
}

/* Add an element to an array, only if it's larger than any of the
   elements within the current array. Arr is sorted from largest to
   smallest. */
void add_large_element(int *arr, int size, int val)
{
  for (int i = 0; i < size; ++i) {
    if (val > arr[i]) {
      /* the current calories is greater than one (or more) of the
         current top elves. */
      /* shift the other values down */
      for (int j = size-1; j > i; --j)
        arr[j] = arr[j-1];
      /* add the new element to the array */
      arr[i] = val;
      break;
    }
  }
}


int main(int argc, char *argv[])
{
  char *filename = "./data/day1.test.txt"; /* test file by default */
  
  if (argc > 1) {
    filename = argv[1];
  }

  int best_calories = 0;
  char c;
  int temp_calories = 0;
  char line[5000];              /* maximum size of line is 5000 */

  int NUM_ELVES = 3;
  int top_elves[NUM_ELVES];
  for (int i = 0; i < NUM_ELVES; ++i) {
    top_elves[i] = 0;
  }

  /* read the input file */
  FILE *f = fopen(filename, "r");

  while (fgets(line, sizeof(line), f)) {
    if (line[0] == '\n') {
      add_large_element(top_elves, NUM_ELVES, temp_calories);
      temp_calories = 0;
    } else {
      temp_calories += atoi(line);
    }
  }

  add_large_element(top_elves, NUM_ELVES, temp_calories);

  fclose(f);

  int best_sum = 0;

  for (int i = 0; i < NUM_ELVES; ++i) {
    if (top_elves[i] > best_calories)
      best_calories = top_elves[i];
    best_sum += top_elves[i];
    printf("top (%d) elf: %d\n", i+1, top_elves[i]);
  }

  printf("%d is the highest number of calories\n", best_calories);
  printf("%d is the top %d elves in total\n", best_sum, NUM_ELVES);
  
  return 0;
}
