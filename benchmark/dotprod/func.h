#ifndef FUNC_H
#define FUNC_H
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include "data.h"
static inline int* randomList(size_t a) {
    srand((unsigned int)time(NULL));
    int * tmp = (int *) malloc(sizeof(int) * a);
    for (size_t i = 0; i < a; i++) {
        tmp[i] = rand() + rand();
    }
    return tmp;
} 

static inline void printList(List_int a) {
    printf("size is %u\n", a.size);
    for(size_t i = 0; i < a.size; i++) {
        printf("%d ", a.value[i]);
    }
    printf("\n");
}

static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec * 1e-6;
}

static inline int dotp(Prod_List_int_List_int x)
{
    size_t size = x.fst.size;
    int ret = 0;
    for (size_t i = 0; i < size; i++)
    {
        ret += x.fst.value[i] * x.snd.value[i];
    }
    return ret;
}

static inline int isum(Prod_int_int a)
{
    return a.fst + a.snd;
}

static inline int imult(Prod_int_int a)
{
    return a.fst * a.snd;
}

static inline Prod_List_int_List_int split(List_int a)
{
    size_t size = a.size;
    int *value = a.value;
    size_t leftSize = size / 2;
    size_t rightSize = size - leftSize;
    List_int left = (List_int){leftSize, value};
    List_int right = (List_int){rightSize, value + leftSize};
    return (Prod_List_int_List_int){left, right};
}

static inline Prod_Prod_List_int_List_int_Prod_List_int_List_int interleave(Prod_Prod_List_int_List_int_Prod_List_int_List_int x) {
    List_int a = x.fst.fst;
    List_int b = x.fst.snd;
    List_int c = x.snd.fst;
    List_int d = x.snd.snd;
    return (Prod_Prod_List_int_List_int_Prod_List_int_List_int) {{a, c},{b, d}};
}

static inline Sum_Sum_unit_Prod_int_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int splitv(Prod_List_int_List_int a)
{
    size_t size = a.fst.size;
    if (size == 0)
    {
        return (Sum_Sum_unit_Prod_int_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int){LEFT, .value.left = {LEFT, .value.left = 0}};
    }
    else if (size == 1)
    {
        int left = a.fst.value[0];
        int right = a.snd.value[0];
        Prod_int_int tmp = {left, right};
        Sum_unit_Prod_int_int tmp2 = {.label = RIGHT, .value.right = tmp};
        return (Sum_Sum_unit_Prod_int_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int){LEFT, .value.left = tmp2};
    }
    else
    {
        Prod_List_int_List_int left = split(a.fst);
        Prod_List_int_List_int right = split(a.snd);
        Sum_Sum_unit_Prod_int_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int b = {RIGHT, .value.right = {left, right}};
        return b;
   }
}

#endif
