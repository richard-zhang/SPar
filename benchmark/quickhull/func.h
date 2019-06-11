#ifndef FUNC_H
#define FUNC_H
#include <time.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include "data.h"
#include "hashmap.h"
DEFINE_HASHMAP(pairHashMap, Prod_int_int)
#define ENTRY_CMP(left, right) left->fst == right->snd && left->snd == right-> snd ? 0 : 1
#define ENTRY_HASH(entry) ((entry->fst + entry->snd) * (entry->fst + entry->snd + 1)/2 + entry->snd)
DECLARE_HASHMAP(pairHashMap, ENTRY_CMP, ENTRY_HASH, free, realloc)
static inline void printPair(Prod_int_int a) {
    printf("%d %d \n", a.fst, a.snd);
}
static inline int findSide(Prod_int_int p1, Prod_int_int p2, Prod_int_int p) 
{ 
    int val = (p.snd - p1.snd) * (p2.fst - p1.fst) - 
              (p2.snd - p1.snd) * (p.fst - p1.fst); 
  
    if (val > 0) 
        return 1; 
    if (val < 0) 
        return -1; 
    return 0; 
}
static int lineDist(Prod_int_int p1, Prod_int_int p2, Prod_int_int p) 
{ 
    return abs ((p.snd - p1.snd) * (p2.fst - p1.fst) - 
               (p2.snd - p1.snd) * (p.fst - p1.fst)); 
}
static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec * 1e-6;
}

static inline Prod_Prod_int_int_Prod_int_int findMaxMin (List_Prod_int_int a) {
    int min_x = 0;
    int max_x = 0;
    size_t size = a.size;
    for(size_t i = 0; i < size; i++) {
        if (a.value[i].fst < a.value[min_x].fst) {
            min_x = i;
        }
        if (a.value[i].fst > a.value[max_x].fst) {
            max_x = i;
        }
    }
    // printPair(a.value[min_x]);
    // printPair(a.value[max_x]);
    return (Prod_Prod_int_int_Prod_int_int) {a.value[min_x], a.value[max_x]};
}

static inline Prod_Prod_int_int_Prod_int_int swap(Prod_Prod_int_int_Prod_int_int a) {
    return (Prod_Prod_int_int_Prod_int_int) {a.snd, a.fst};
}

static inline Prod_Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int_Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int split(Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int a) {
    Prod_int_int p1 = a.snd.fst;
    Prod_int_int p2 = a.snd.snd;
    int ind = -1;
    int max_dist = 0;
    size_t n = a.fst.size;
    for (size_t i = 0; i < n; i++) {
        int temp = lineDist(p1, p2, a.fst.value[i]);
        if (temp > max_dist && findSide(p1, p2, a.fst.value[i]) == -1) {
            ind = i;
            max_dist = temp;
        }
    }    

    if (ind == -1) {
        Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int x = {0, NULL, 0, 0, 0, 0};
        return (Prod_Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int_Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int) {x, x};
    }


    Prod_int_int c = a.fst.value[ind];
    Prod_int_int p = a.snd.fst;
    Prod_int_int q = a.snd.snd;
    Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int left = {a.fst.size, a.fst.value, p, c};
    Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int right = {a.fst.size, a.fst.value, c, q};
    return (Prod_Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int_Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int) {left, right};
}

static inline List_Prod_int_int baseFunc(Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int a) {
    Prod_int_int p1 = a.snd.fst;
    Prod_int_int p2 = a.snd.snd;
    int ind = -1;
    int max_dist = 0;
    size_t n = a.fst.size;
    for (size_t i = 0; i < n; i++) {
        int temp = lineDist(p1, p2, a.fst.value[i]);
        if (temp > max_dist && findSide(p1, p2, a.fst.value[i]) == -1) {
            ind = i;
            max_dist = temp;
        }
    }    

    if (ind == -1) {
        List_Prod_int_int i = {0, NULL};
        return i;
    }
    Prod_int_int c = a.fst.value[ind];
    Prod_int_int p = a.snd.fst;
    Prod_int_int q = a.snd.snd;
    Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int left = {a.fst, {p, c}};
    Prod_List_Prod_int_int_Prod_Prod_int_int_Prod_int_int right = {a.fst, {c, q}};
    List_Prod_int_int lr = baseFunc(left);
    List_Prod_int_int rr = baseFunc(right);
    size_t size = 1 + lr.size + rr.size;
    Prod_int_int *value = (Prod_int_int *) malloc(sizeof(Prod_int_int) * size);
    value[0] = c;
    if (lr.size > 0) {
        memcpy(value+1, lr.value, lr.size * sizeof(Prod_int_int));
        free(lr.value);
    }
    if (rr.size > 0) {
        memcpy(value+1+lr.size, rr.value, sizeof(Prod_int_int) * rr.size);
        free(rr.value);
    }
    return (List_Prod_int_int) {size, value};
}

static inline List_Prod_int_int merge(Prod_List_Prod_int_int_List_Prod_int_int a) {
    size_t size = a.fst.size + a.snd.size;
    Prod_int_int *value = (Prod_int_int *) malloc(sizeof(Prod_int_int) * size);
    if (a.fst.size > 0) {
        memcpy(value, a.fst.value, sizeof(Prod_int_int) * a.fst.size);
        free(a.fst.value);
    }

    if (a.snd.size > 0) {
        memcpy(value + a.fst.size, a.snd.value, sizeof(Prod_int_int) * a.snd.size);
        free(a.snd.value);
    }

    return (List_Prod_int_int) {size, value};
}

static inline void printList(List_Prod_int_int a) {
    printf("size is %lu\n", a.size);
    for(size_t i = 0 ; i < a.size; i++) {
        printf("%d %d \n", a.value[i].fst, a.value[i].snd);
    }
}

#endif
