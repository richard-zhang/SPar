#include<stdint.h>
#include<stdio.h>
#include<stdlib.h>
#include<chan.h>
#include<pthread.h>
#include"../data.h"
#include"../func.h"
chan_t * c1;
chan_t * c2;
chan_t * c3;
chan_t * c4;
chan_t * c5;
chan_t * c6;
chan_t * c7;
void proc1Rt()
{
    Prod_List_int_List_int v0;
    chan_recv_buf(c1, &v0, sizeof(Prod_List_int_List_int));
    Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_Prod_List_int_List_int_Prod_List_int_List_int v1;
    v1 = split4(v0);
    Prod_List_int_List_int v2;
    v2 = v1.fst;
    chan_send_buf(c2, &v2, sizeof(Prod_List_int_List_int));
    Prod_List_int_List_int v3;
    v3 = v1.snd.fst;
    chan_send_buf(c3, &v3, sizeof(Prod_List_int_List_int));
    Prod_List_int_List_int v4;
    v4 = v1.snd.snd.fst;
    chan_send_buf(c4, &v4, sizeof(Prod_List_int_List_int));
    int v5;
    v5 = dotp(v1.snd.snd.snd);
    chan_send_int(c4, v5);
}
void proc2Rt()
{
    Prod_List_int_List_int v6;
    chan_recv_buf(c4, &v6, sizeof(Prod_List_int_List_int));
    int v7;
    v7 = dotp(v6);
    int v8;
    chan_recv_int(c4, &v8);
    Prod_int_int v9;
    v9 = (Prod_int_int) {v7, v8};
    chan_send_buf(c5, &v9, sizeof(Prod_int_int));
}
void proc3Rt()
{
    Prod_List_int_List_int v10;
    chan_recv_buf(c3, &v10, sizeof(Prod_List_int_List_int));
    int v11;
    v11 = dotp(v10);
    Prod_int_int v12;
    chan_recv_buf(c5, &v12, sizeof(Prod_int_int));
    Prod_int_Prod_int_int v13;
    v13 = (Prod_int_Prod_int_int) {v11, v12};
    chan_send_buf(c6, &v13, sizeof(Prod_int_Prod_int_int));
}
void proc4Rt()
{
    Prod_List_int_List_int v14;
    chan_recv_buf(c2, &v14, sizeof(Prod_List_int_List_int));
    int v15;
    v15 = dotp(v14);
    Prod_int_Prod_int_int v16;
    chan_recv_buf(c6, &v16, sizeof(Prod_int_Prod_int_int));
    Prod_int_Prod_int_Prod_int_int v17;
    v17 = (Prod_int_Prod_int_Prod_int_int) {v15, v16};
    int v18;
    v18 = reducer(v17);
    chan_send_int(c7, v18);
}
void * proc1()
{
    proc1Rt();
    return NULL;
}
void * proc2()
{
    proc2Rt();
    return NULL;
}
void * proc3()
{
    proc3Rt();
    return NULL;
}
void * proc4()
{
    proc4Rt();
    return NULL;
}
int proc0(Prod_List_int_List_int v0)
{
    c1 = chan_init(1);
    c2 = chan_init(1);
    c3 = chan_init(1);
    c4 = chan_init(1);
    c5 = chan_init(1);
    c6 = chan_init(1);
    c7 = chan_init(1);
    pthread_t th1;
    pthread_create(&th1, NULL, proc1, NULL);
    pthread_t th2;
    pthread_create(&th2, NULL, proc2, NULL);
    pthread_t th3;
    pthread_create(&th3, NULL, proc3, NULL);
    pthread_t th4;
    pthread_create(&th4, NULL, proc4, NULL);
    chan_send_buf(c1, &v0, sizeof(Prod_List_int_List_int));
    int v1;
    chan_recv_int(c7, &v1);
    pthread_join(th1, NULL);
    pthread_join(th2, NULL);
    pthread_join(th3, NULL);
    pthread_join(th4, NULL);
    chan_dispose(c1);
    chan_dispose(c2);
    chan_dispose(c3);
    chan_dispose(c4);
    chan_dispose(c5);
    chan_dispose(c6);
    chan_dispose(c7);
    return v1;
}
int main()
{
    int tmpLeft[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
    List_int aLeft = (List_int) {16, tmpLeft};
    int tmpRight[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
    List_int aRight = (List_int) {16, tmpRight};
    Prod_List_int_List_int a = (Prod_List_int_List_int) {aLeft, aRight};
    double start = get_time();
    proc0(a);
    double end = get_time();
    printf("%lf\n", end - start);
    return 0;
}
