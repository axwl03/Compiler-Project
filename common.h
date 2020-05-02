#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct entry;
typedef struct entry{
	int index;
	char name[100];
	char type[10];
	int address;
	int lineno;
	char element_type[10];
	struct entry *next;
} entry;

#endif /* COMMON_H */
