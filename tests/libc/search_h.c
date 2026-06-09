// This code is the example from POSIX Programmer's Manual's TDELETE(3P) page.

#include <limits.h>
#include <search.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

struct element {      /* Pointers to these are stored in the tree. */
  int     count;
  char    string[];
};

void  *root = NULL;          /* This points to the root. */

int main_tdelete(void);
int main_hsearch(void);

volatile int nondet;
int main(void) {
  // in case missing/unsupported specs cause failure, we wrap the calls inside
  // 'if (nondet)' blocks
  if (nondet) main_tdelete();
  if (nondet) main_hsearch();
}

int main_tdelete(void) {
  char   str[_POSIX2_LINE_MAX+1];
  int    length = 0;
  struct element *elementptr;
  void   *node;
  void   print_node(const void *, VISIT, int);
  int    node_compare(const void *, const void *),
    delete_root(const void *, const void *);

  while (fgets(str, sizeof(str), stdin))  {
    /* Set element. */
    length = strlen(str);
    if (str[length-1] == '\n')
      str[--length] = '\0';
    elementptr = malloc(sizeof(struct element) + length + 1);
    strcpy(elementptr->string, str);
    elementptr->count = 1;
    /* Put element into the tree. */
    node = tsearch((void *)elementptr, &root, node_compare);
    if (node == NULL) {
      fprintf(stderr,
              "tsearch: Not enough space available\n");
      exit(EXIT_FAILURE);
    }
    else if (*(struct element **)node != elementptr) {
      /* A node containing the element already exists */
      (*(struct element **)node)->count++;
      free(elementptr);
    }
  }
  twalk(root, print_node);

  /* Delete all nodes in the tree */
  while (root != NULL) {
    elementptr = *(struct element **)root;
    printf("deleting node: string = %s,  count = %d\n",
           elementptr->string,
           elementptr->count);
    tdelete((void *)elementptr, &root, delete_root);
    free(elementptr);
  }

  return 0;
}

int node_compare(const void *node1, const void *node2) {
  return strcmp(((const struct element *) node1)->string,
                ((const struct element *) node2)->string);
}

int delete_root(const void *node1, const void *node2) {
  return 0;
}

void print_node(const void *ptr, VISIT order, int level) {
  const struct element *p = *(const struct element **) ptr;

  if (order == postorder || order == leaf)  {
    (void) printf("string = %s,  count = %d\n",
                  p->string, p->count);
  }
}

// Example from HCREATE(3P) man page

struct info {        /* This is the info stored in the table */
  int age, room;   /* other than the key. */
};

#define NUM_EMPL    5000    /* # of elements in search table. */

int main_hsearch(void)
{
  char string_space[NUM_EMPL*20];   /* Space to store strings. */
  struct info info_space[NUM_EMPL]; /* Space to store employee info. */
  char *str_ptr = string_space;     /* Next space in string_space. */
  struct info *info_ptr = info_space;
  /* Next space in info_space. */
  ENTRY item;
  ENTRY *found_item; /* Name to look for in table. */
  char name_to_find[30];

  int i = 0;

  /* Create table; no error checking is performed. */
  (void) hcreate(NUM_EMPL);
  while (scanf("%s%d%d", str_ptr, &info_ptr->age,
               &info_ptr->room) != EOF && i++ < NUM_EMPL) {

    /* Put information in structure, and structure in item. */
    item.key = str_ptr;
    item.data = info_ptr;
    str_ptr += strlen(str_ptr) + 1;
    info_ptr++;

    /* Put item into table. */
    (void) hsearch(item, ENTER);
  }

  /* Access table. */
  item.key = name_to_find;
  while (scanf("%s", item.key) != EOF) {
    if ((found_item = hsearch(item, FIND)) != NULL) {

      /* If item is in the table. */
      (void)printf("found %s, age = %d, room = %d\n",
                   found_item->key,
                   ((struct info *)found_item->data)->age,
                   ((struct info *)found_item->data)->room);
    } else
      (void)printf("no such employee %s\n", name_to_find);
  }
  return 0;
}
