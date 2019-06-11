#ifndef FUNC_H
#define FUNC_H
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include <stdlib.h>
#include "data.h"
static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec * 1e-6;
}
static inline Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int split4(Prod_List_int_List_int a)
{
    size_t length = a.fst.size;
    size_t subListLength = length / 4;
    if (subListLength <= 0)
    {
        List_int empty = {0, NULL};
        Prod_List_int_List_int tem1 = {empty, empty};
        return (Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int){a, (Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int){tem1, (Prod_Prod_List_int_List_int_Prod_List_int_List_int){tem1, tem1}}};
    }
    else
    {
        Prod_List_int_List_int tem1 = {{subListLength, a.fst.value}, {subListLength, a.snd.value}};
        Prod_List_int_List_int tem2 = {{subListLength, a.fst.value + subListLength}, {subListLength, a.snd.value + subListLength}};
        Prod_List_int_List_int tem3 = {{subListLength, a.fst.value + 2 * subListLength}, {subListLength, a.snd.value + 2 * subListLength}};
        Prod_List_int_List_int tem4 = {{(length - 3 * subListLength), a.fst.value + 3 * subListLength}, {(length - 3 * subListLength), a.snd.value + 3 * subListLength}};
        return (Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int){tem1, (Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int){tem2, (Prod_Prod_List_int_List_int_Prod_List_int_List_int){tem3, tem4}}};
    }
}

static inline int addition(Prod_int_int a)
{
    return a.fst + a.snd;
}

static inline Prod_Prod_List_int_List_int_Prod_List_int_List_int split2(Prod_List_int_List_int a)
{
    size_t length = a.fst.size;
    size_t subListLength = length / 2;
    if (subListLength <= 0) {
        List_int empty = {0, NULL};
        Prod_List_int_List_int tem1 = {empty, empty};
        return (Prod_Prod_List_int_List_int_Prod_List_int_List_int) {a, tem1};
    } else {
        Prod_List_int_List_int tem1 = {{subListLength, a.fst.value}, {subListLength, a.snd.value}};
        Prod_List_int_List_int tem2 = {{length - subListLength, a.fst.value + subListLength}, { length - subListLength, a.snd.value + subListLength}};
        return (Prod_Prod_List_int_List_int_Prod_List_int_List_int) {tem1, tem2};
    }
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

static inline int reducer(Prod_int_Prod_int_Prod_int_int x) {
    return x.fst + x.snd.fst + x.snd.snd.fst + x.snd.snd.snd;
}
#endif
