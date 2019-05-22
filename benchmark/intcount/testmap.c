#include <stdio.h>
#include <stdlib.h>
#include "hashmap.h"
struct entry {
    int hash;
    int counter;
};
DEFINE_HASHMAP(intHashMap, struct entry)
#define ENTRY_CMP(left, right) left->hash == right->hash ? 0 : 1
#define ENTRY_HASH(entry) entry->hash
DECLARE_HASHMAP(intHashMap, ENTRY_CMP, ENTRY_HASH, free, realloc)

int main() {
    intHashMap map;
    intHashMapNew(&map);
    int arr[] = {1,2,1,1,1,2,2,4,6,7,4};
    struct entry *entryFound;
    struct entry entryRead;
    for(int i = 0; i < 11; i++) {
        entryRead = (struct entry) {arr[i], 0};
        entryFound = &entryRead;
        HashMapPutResult result = intHashMapPut(&map, &entryFound, HMDR_FIND);
        ++entryFound->counter;
    }
    printf("%d\n", map.size);
    HASHMAP_FOR_EACH(intHashMap, entryFound, map){
        printf("%d %d\n", entryFound->hash, entryFound->counter); 
    } HASHMAP_FOR_EACH_END
    intHashMapDestroy(&map);
    return 0;
}