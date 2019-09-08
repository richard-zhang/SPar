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
chan_t * c8;
chan_t * c9;
chan_t * c10;
void proc1Rt()
{
    Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int v0;
    chan_recv_buf(c1,
                  &v0,
                  sizeof(Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int));
    Prod_int_int v1;
    v1 = (Prod_int_int) {v0.fst.fst.fst, v0.snd.fst.fst};
    chan_send_buf(c2, &v1, sizeof(Prod_int_int));
    Prod_int_int v2;
    v2 = (Prod_int_int) {v0.fst.fst.snd, v0.snd.fst.snd};
    chan_send_buf(c3, &v2, sizeof(Prod_int_int));
    Prod_int_int v3;
    v3 = (Prod_int_int) {v0.fst.snd.fst, v0.snd.snd.fst};
    chan_send_buf(c4, &v3, sizeof(Prod_int_int));
    Prod_int_int v4;
    v4 = (Prod_int_int) {v0.fst.snd.snd, v0.snd.snd.snd};
    chan_send_buf(c5, &v4, sizeof(Prod_int_int));
}
void proc2Rt()
{
    Prod_int_int v5;
    chan_recv_buf(c2, &v5, sizeof(Prod_int_int));
    int v6;
    v6 = result(v5);
    chan_send_int(c6, v6);
}
void proc3Rt()
{
    Prod_int_int v7;
    chan_recv_buf(c3, &v7, sizeof(Prod_int_int));
    int v8;
    v8 = result(v7);
    chan_send_int(c7, v8);
}
void proc4Rt()
{
    Prod_int_int v9;
    chan_recv_buf(c4, &v9, sizeof(Prod_int_int));
    int v10;
    v10 = result(v9);
    chan_send_int(c8, v10);
}
void proc5Rt()
{
    Prod_int_int v11;
    chan_recv_buf(c5, &v11, sizeof(Prod_int_int));
    int v12;
    v12 = result(v11);
    chan_send_int(c9, v12);
}
void proc6Rt()
{
    int v13;
    chan_recv_int(c6, &v13);
    int v14;
    chan_recv_int(c7, &v14);
    int v15;
    chan_recv_int(c8, &v15);
    int v16;
    chan_recv_int(c9, &v16);
    Prod_Prod_int_int_Prod_int_int v17;
    v17 = (Prod_Prod_int_int_Prod_int_int) {(Prod_int_int) {v13, v14}, (Prod_int_int) {v15, v16}};
    chan_send_buf(c10, &v17, sizeof(Prod_Prod_int_int_Prod_int_int));
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
void * proc5()
{
    proc5Rt();
    return NULL;
}
void * proc6()
{
    proc6Rt();
    return NULL;
}
Prod_Prod_int_int_Prod_int_int proc0(Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int v0)
{
    c1 = chan_init(1);
    c2 = chan_init(1);
    c3 = chan_init(1);
    c4 = chan_init(1);
    c5 = chan_init(1);
    c6 = chan_init(1);
    c7 = chan_init(1);
    c8 = chan_init(1);
    c9 = chan_init(1);
    c10 = chan_init(1);
    pthread_t th1;
    pthread_create(&th1, NULL, proc1, NULL);
    pthread_t th2;
    pthread_create(&th2, NULL, proc2, NULL);
    pthread_t th3;
    pthread_create(&th3, NULL, proc3, NULL);
    pthread_t th4;
    pthread_create(&th4, NULL, proc4, NULL);
    pthread_t th5;
    pthread_create(&th5, NULL, proc5, NULL);
    pthread_t th6;
    pthread_create(&th6, NULL, proc6, NULL);
    chan_send_buf(c1,
                  &v0,
                  sizeof(Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int));
    Prod_Prod_int_int_Prod_int_int v1;
    chan_recv_buf(c10, &v1, sizeof(Prod_Prod_int_int_Prod_int_int));
    pthread_join(th1, NULL);
    pthread_join(th2, NULL);
    pthread_join(th3, NULL);
    pthread_join(th4, NULL);
    pthread_join(th5, NULL);
    pthread_join(th6, NULL);
    chan_dispose(c1);
    chan_dispose(c2);
    chan_dispose(c3);
    chan_dispose(c4);
    chan_dispose(c5);
    chan_dispose(c6);
    chan_dispose(c7);
    chan_dispose(c8);
    chan_dispose(c9);
    chan_dispose(c10);
    return v1;
}
int main()
{
    int aLeft3 = 1;
    int aRight3 = 2;
    Prod_int_int aLeft2 = (Prod_int_int) {aLeft3, aRight3};
    int aLeft6 = 3;
    int aRight6 = 4;
    Prod_int_int aRight2 = (Prod_int_int) {aLeft6, aRight6};
    Prod_Prod_int_int_Prod_int_int aLeft1 = (Prod_Prod_int_int_Prod_int_int) {aLeft2, aRight2};
    int aLeft10 = 4;
    int aRight10 = 3;
    Prod_int_int aLeft9 = (Prod_int_int) {aLeft10, aRight10};
    int aLeft13 = 2;
    int aRight13 = 1;
    Prod_int_int aRight9 = (Prod_int_int) {aLeft13, aRight13};
    Prod_Prod_int_int_Prod_int_int aRight1 = (Prod_Prod_int_int_Prod_int_int) {aLeft9, aRight9};
    Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int a = (Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int) {aLeft1, aRight1};
    double start = get_time();
    debug(proc0(a));
    double end = get_time();
    printf("%lf\n", end - start);
    return 0;
}
