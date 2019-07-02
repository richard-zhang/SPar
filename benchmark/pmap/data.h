#ifndef DATA_H
#define DATA_H
typedef enum Label {
            LEFT, RIGHT
        } Label;
typedef struct Prod_int_int {
            int fst; int snd;
        } Prod_int_int;
typedef struct Prod_int_Prod_int_int {
            int fst; Prod_int_int snd;
        } Prod_int_Prod_int_int;
typedef struct Prod_int_Prod_int_Prod_int_int {
            int fst; Prod_int_Prod_int_int snd;
        } Prod_int_Prod_int_Prod_int_int;
#endif
