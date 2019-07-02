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
    Prod_int_Prod_int_Prod_int_int v0;
    chan_recv_buf(c1, &v0, sizeof(Prod_int_Prod_int_Prod_int_int));
    int v1;
    v1 = v0.fst;
    chan_send_int(c2, v1);
    int v2;
    v2 = v0.snd.fst;
    chan_send_int(c3, v2);
    int v3;
    v3 = v0.snd.snd.fst;
    chan_send_int(c4, v3);
    int v4;
    v4 = result(v0.snd.snd.snd);
    chan_send_int(c4, v4);
}
void proc2Rt()
{
    int v5;
    chan_recv_int(c4, &v5);
    int v6;
    v6 = result(v5);
    int v7;
    chan_recv_int(c4, &v7);
    Prod_int_int v8;
    v8 = (Prod_int_int) {v6, v7};
    chan_send_buf(c5, &v8, sizeof(Prod_int_int));
}
void proc3Rt()
{
    int v9;
    chan_recv_int(c3, &v9);
    int v10;
    v10 = result(v9);
    Prod_int_int v11;
    chan_recv_buf(c5, &v11, sizeof(Prod_int_int));
    Prod_int_Prod_int_int v12;
    v12 = (Prod_int_Prod_int_int) {v10, v11};
    chan_send_buf(c6, &v12, sizeof(Prod_int_Prod_int_int));
}
void proc4Rt()
{
    int v13;
    chan_recv_int(c2, &v13);
    int v14;
    v14 = result(v13);
    Prod_int_Prod_int_int v15;
    chan_recv_buf(c6, &v15, sizeof(Prod_int_Prod_int_int));
    Prod_int_Prod_int_Prod_int_int v16;
    v16 = (Prod_int_Prod_int_Prod_int_int) {v14, v15};
    chan_send_buf(c7, &v16, sizeof(Prod_int_Prod_int_Prod_int_int));
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
Prod_int_Prod_int_Prod_int_int proc0(Prod_int_Prod_int_Prod_int_int v0)
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
    chan_send_buf(c1, &v0, sizeof(Prod_int_Prod_int_Prod_int_int));
    Prod_int_Prod_int_Prod_int_int v1;
    chan_recv_buf(c7, &v1, sizeof(Prod_int_Prod_int_Prod_int_int));
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
    int aLeft0 = 1;
    int aLeft1 = 1;
    int aLeft2 = 1;
    int aRight2 = 1;
    Prod_int_int aRight1 = (Prod_int_int) {aLeft2, aRight2};
    Prod_int_Prod_int_int aRight0 = (Prod_int_Prod_int_int) {aLeft1, aRight1};
    Prod_int_Prod_int_Prod_int_int a = (Prod_int_Prod_int_Prod_int_int) {aLeft0, aRight0};
    double start = get_time();
    debug(proc0(a));
    double end = get_time();
    printf("%lf\n", end - start);
    return 0;
}
