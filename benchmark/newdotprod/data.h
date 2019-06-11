#ifndef DATA_H
#define DATA_H
typedef enum Label {
            LEFT, RIGHT
        } Label;
typedef struct Prod_int_int {
            int fst; int snd;
        } Prod_int_int;
typedef struct List_int {
            size_t size; int * value;
        } List_int;
typedef struct Prod_Prod_int_int_Prod_int_int {
            Prod_int_int fst; Prod_int_int snd;
        } Prod_Prod_int_int_Prod_int_int;
typedef struct Prod_int_Prod_int_int {
            int fst; Prod_int_int snd;
        } Prod_int_Prod_int_int;
typedef struct Prod_List_int_List_int {
            List_int fst; List_int snd;
        } Prod_List_int_List_int;
typedef struct Prod_int_Prod_int_Prod_int_int {
            int fst; Prod_int_Prod_int_int snd;
        } Prod_int_Prod_int_Prod_int_int;
typedef struct Prod_Prod_List_int_List_int_Prod_List_int_List_int {
            Prod_List_int_List_int fst; Prod_List_int_List_int snd;
        } Prod_Prod_List_int_List_int_Prod_List_int_List_int;
typedef struct Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int {
            Prod_List_int_List_int fst;
            Prod_Prod_List_int_List_int_Prod_List_int_List_int snd;
        } Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int;
typedef struct Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int {
            Prod_List_int_List_int fst;
            Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int snd;
        } Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int;
#endif
