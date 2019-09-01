#ifndef DATA_H
#define DATA_H
typedef enum Label {
            LEFT, RIGHT
        } Label;
typedef struct Prod_int_int {
            int fst; int snd;
        } Prod_int_int;
typedef struct Prod_Prod_int_int_Prod_int_int {
            Prod_int_int fst; Prod_int_int snd;
        } Prod_Prod_int_int_Prod_int_int;
typedef struct Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int {
            Prod_Prod_int_int_Prod_int_int fst;
            Prod_Prod_int_int_Prod_int_int snd;
        } Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int;
typedef struct Prod_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int {
            Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int fst;
            Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int snd;
        } Prod_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int;
#endif
