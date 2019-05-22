#ifndef FUNC_H
#define FUNC_H
#include<time.h>
#include<sys/time.h>
#include<sys/resource.h>
#include<stdio.h>
#include<stdlib.h>
#include "data.h"
#include "hashmap.h"
struct entry {
    int hash;
    int counter;
};
DEFINE_HASHMAP(intHashMap, struct entry)
#define ENTRY_CMP(left, right) left->hash == right->hash ? 0 : 1
#define ENTRY_HASH(entry) entry->hash
DECLARE_HASHMAP(intHashMap, ENTRY_CMP, ENTRY_HASH, free, realloc)

static inline double get_time()
{
    struct timeval t;
    gettimeofday(&t, NULL);
    return t.tv_sec + t.tv_usec*1e-6;
}
static inline List_Prod_int_int count(List_int a) {
    intHashMap map;
    intHashMapNew(&map);
    struct entry entryRead, *entryFound;
    for (size_t i = 0; i < a.size; i++) {
        entryRead = (struct entry) {a.value[i], 0};
        entryFound = &entryRead;
        intHashMapPut(&map, &entryFound, HMDR_FIND);
        ++entryFound->counter;
    }
    size_t mapSize = map.size;
    Prod_int_int *ret = (Prod_int_int *) malloc(sizeof(Prod_int_int) * mapSize);
    size_t i = 0;
    HASHMAP_FOR_EACH(intHashMap, entryFound, map) {
        ret[i] = (Prod_int_int) { entryFound->hash, entryFound->counter}; 
        i++;
    } HASHMAP_FOR_EACH_END;
    intHashMapDestroy(&map);
    // free(a.value);
    return (List_Prod_int_int) {mapSize, ret};
} 
static inline Sum_List_int_Prod_List_int_List_int split(List_int a) {
    size_t size = a.size;
    int *value = a.value;
    if (size <= 1) {
        return (Sum_List_int_Prod_List_int_List_int) {LEFT, .value.left = a};
    } else {
        size_t leftSize = size/2;
        size_t rightSize = size - leftSize;
        List_int left = (List_int) {leftSize, value};
        List_int right = (List_int) {rightSize, value + leftSize};
        Prod_List_int_List_int prodList = (Prod_List_int_List_int) {left, right};
        return (Sum_List_int_Prod_List_int_List_int) {RIGHT, {.right = prodList}};
    }
}
static inline List_Prod_int_int myunion(Prod_List_Prod_int_int_List_Prod_int_int a) {
    intHashMap map;
    intHashMapNew(&map);
    struct entry entryRead, *entryFound;
    for(size_t i = 0; i < a.fst.size; i++) {
        entryRead =  (struct entry) {a.fst.value[i].fst, 0};
        entryFound = &entryRead;
        intHashMapPut(&map, &entryFound, HMDR_FIND);
        entryFound->counter += a.fst.value[i].snd;
    }
    for(size_t  i = 0; i < a.snd.size; i++) {
        entryRead =  (struct entry) {a.snd.value[i].fst, 0};
        entryFound = &entryRead;
        intHashMapPut(&map, &entryFound, HMDR_FIND);
        entryFound->counter += a.snd.value[i].snd;
    }
    size_t mapSize = map.size;
    Prod_int_int *ret = (Prod_int_int *) malloc(sizeof(Prod_int_int) * mapSize);
    size_t i = 0;
    HASHMAP_FOR_EACH(intHashMap, entryFound, map) {
        ret[i] = (Prod_int_int) { entryFound->hash, entryFound->counter}; 
        i++;
    } HASHMAP_FOR_EACH_END;
    intHashMapDestroy(&map);
    free(a.fst.value);
    free(a.snd.value);
    return (List_Prod_int_int) {mapSize, ret};
}
void printListProdIntInt(List_Prod_int_int a) {
    size_t size = a.size;
    Prod_int_int *value = a.value;
    for(size_t i = 0; i < size; i++) {
        printf("%d %d\n", value[i].fst, value[i].snd);
    }
}
#endif