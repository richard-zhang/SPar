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
chan_t * c11;
chan_t * c12;
chan_t * c13;
chan_t * c14;
chan_t * c15;
chan_t * c16;
chan_t * c17;
chan_t * c18;
void proc1Rt()
{
    Prod_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int v0;
    chan_recv_buf(c1,
                  &v0,
                  sizeof(Prod_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int));
    Prod_int_int v1;
    v1 = (Prod_int_int) {v0.fst.fst.fst.fst, v0.snd.fst.fst.fst};
    chan_send_buf(c2, &v1, sizeof(Prod_int_int));
    Prod_int_int v2;
    v2 = (Prod_int_int) {v0.fst.fst.fst.snd, v0.snd.fst.fst.snd};
    chan_send_buf(c3, &v2, sizeof(Prod_int_int));
    Prod_int_int v3;
    v3 = (Prod_int_int) {v0.fst.fst.snd.fst, v0.snd.fst.snd.fst};
    chan_send_buf(c4, &v3, sizeof(Prod_int_int));
    Prod_int_int v4;
    v4 = (Prod_int_int) {v0.fst.fst.snd.snd, v0.snd.fst.snd.snd};
    chan_send_buf(c5, &v4, sizeof(Prod_int_int));
    Prod_int_int v5;
    v5 = (Prod_int_int) {v0.fst.snd.fst.fst, v0.snd.snd.fst.fst};
    chan_send_buf(c6, &v5, sizeof(Prod_int_int));
    Prod_int_int v6;
    v6 = (Prod_int_int) {v0.fst.snd.fst.snd, v0.snd.snd.fst.snd};
    chan_send_buf(c7, &v6, sizeof(Prod_int_int));
    Prod_int_int v7;
    v7 = (Prod_int_int) {v0.fst.snd.snd.fst, v0.snd.snd.snd.fst};
    chan_send_buf(c8, &v7, sizeof(Prod_int_int));
    Prod_int_int v8;
    v8 = (Prod_int_int) {v0.fst.snd.snd.snd, v0.snd.snd.snd.snd};
    chan_send_buf(c9, &v8, sizeof(Prod_int_int));
}
void proc2Rt()
{
    Prod_int_int v9;
    chan_recv_buf(c2, &v9, sizeof(Prod_int_int));
    int v10;
    v10 = result(v9);
    chan_send_int(c10, v10);
}
void proc3Rt()
{
    Prod_int_int v11;
    chan_recv_buf(c3, &v11, sizeof(Prod_int_int));
    int v12;
    v12 = result(v11);
    chan_send_int(c11, v12);
}
void proc4Rt()
{
    Prod_int_int v13;
    chan_recv_buf(c4, &v13, sizeof(Prod_int_int));
    int v14;
    v14 = result(v13);
    chan_send_int(c12, v14);
}
void proc5Rt()
{
    Prod_int_int v15;
    chan_recv_buf(c5, &v15, sizeof(Prod_int_int));
    int v16;
    v16 = result(v15);
    chan_send_int(c13, v16);
}
void proc6Rt()
{
    Prod_int_int v17;
    chan_recv_buf(c6, &v17, sizeof(Prod_int_int));
    int v18;
    v18 = result(v17);
    chan_send_int(c14, v18);
}
void proc7Rt()
{
    Prod_int_int v19;
    chan_recv_buf(c7, &v19, sizeof(Prod_int_int));
    int v20;
    v20 = result(v19);
    chan_send_int(c15, v20);
}
void proc8Rt()
{
    Prod_int_int v21;
    chan_recv_buf(c8, &v21, sizeof(Prod_int_int));
    int v22;
    v22 = result(v21);
    chan_send_int(c16, v22);
}
void proc9Rt()
{
    Prod_int_int v23;
    chan_recv_buf(c9, &v23, sizeof(Prod_int_int));
    int v24;
    v24 = result(v23);
    chan_send_int(c17, v24);
}
void proc10Rt()
{
    int v25;
    chan_recv_int(c10, &v25);
    int v26;
    chan_recv_int(c11, &v26);
    int v27;
    chan_recv_int(c12, &v27);
    int v28;
    chan_recv_int(c13, &v28);
    int v29;
    chan_recv_int(c14, &v29);
    int v30;
    chan_recv_int(c15, &v30);
    int v31;
    chan_recv_int(c16, &v31);
    int v32;
    chan_recv_int(c17, &v32);
    Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int v33;
    v33 = (Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int) {(Prod_Prod_int_int_Prod_int_int) {(Prod_int_int) {v25, v26}, (Prod_int_int) {v27, v28}}, (Prod_Prod_int_int_Prod_int_int) {(Prod_int_int) {v29, v30}, (Prod_int_int) {v31, v32}}};
    chan_send_buf(c18,
                  &v33,
                  sizeof(Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int));
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
void * proc7()
{
    proc7Rt();
    return NULL;
}
void * proc8()
{
    proc8Rt();
    return NULL;
}
void * proc9()
{
    proc9Rt();
    return NULL;
}
void * proc10()
{
    proc10Rt();
    return NULL;
}
Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int proc0(Prod_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int v0)
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
    c11 = chan_init(1);
    c12 = chan_init(1);
    c13 = chan_init(1);
    c14 = chan_init(1);
    c15 = chan_init(1);
    c16 = chan_init(1);
    c17 = chan_init(1);
    c18 = chan_init(1);
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
    pthread_t th7;
    pthread_create(&th7, NULL, proc7, NULL);
    pthread_t th8;
    pthread_create(&th8, NULL, proc8, NULL);
    pthread_t th9;
    pthread_create(&th9, NULL, proc9, NULL);
    pthread_t th10;
    pthread_create(&th10, NULL, proc10, NULL);
    chan_send_buf(c1,
                  &v0,
                  sizeof(Prod_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int));
    Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int v1;
    chan_recv_buf(c18,
                  &v1,
                  sizeof(Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int));
    pthread_join(th1, NULL);
    pthread_join(th2, NULL);
    pthread_join(th3, NULL);
    pthread_join(th4, NULL);
    pthread_join(th5, NULL);
    pthread_join(th6, NULL);
    pthread_join(th7, NULL);
    pthread_join(th8, NULL);
    pthread_join(th9, NULL);
    pthread_join(th10, NULL);
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
    chan_dispose(c11);
    chan_dispose(c12);
    chan_dispose(c13);
    chan_dispose(c14);
    chan_dispose(c15);
    chan_dispose(c16);
    chan_dispose(c17);
    chan_dispose(c18);
    return v1;
}
int main()
{
    int aLeft4 = 1;
    int aRight4 = 2;
    Prod_int_int aLeft3 = (Prod_int_int) {aLeft4, aRight4};
    int aLeft7 = 3;
    int aRight7 = 4;
    Prod_int_int aRight3 = (Prod_int_int) {aLeft7, aRight7};
    Prod_Prod_int_int_Prod_int_int aLeft2 = (Prod_Prod_int_int_Prod_int_int) {aLeft3, aRight3};
    int aLeft11 = 5;
    int aRight11 = 6;
    Prod_int_int aLeft10 = (Prod_int_int) {aLeft11, aRight11};
    int aLeft14 = 7;
    int aRight14 = 8;
    Prod_int_int aRight10 = (Prod_int_int) {aLeft14, aRight14};
    Prod_Prod_int_int_Prod_int_int aRight2 = (Prod_Prod_int_int_Prod_int_int) {aLeft10, aRight10};
    Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int aLeft1 = (Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int) {aLeft2, aRight2};
    int aLeft19 = 8;
    int aRight19 = 7;
    Prod_int_int aLeft18 = (Prod_int_int) {aLeft19, aRight19};
    int aLeft22 = 6;
    int aRight22 = 5;
    Prod_int_int aRight18 = (Prod_int_int) {aLeft22, aRight22};
    Prod_Prod_int_int_Prod_int_int aLeft17 = (Prod_Prod_int_int_Prod_int_int) {aLeft18, aRight18};
    int aLeft26 = 4;
    int aRight26 = 3;
    Prod_int_int aLeft25 = (Prod_int_int) {aLeft26, aRight26};
    int aLeft29 = 2;
    int aRight29 = 1;
    Prod_int_int aRight25 = (Prod_int_int) {aLeft29, aRight29};
    Prod_Prod_int_int_Prod_int_int aRight17 = (Prod_Prod_int_int_Prod_int_int) {aLeft25, aRight25};
    Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int aRight1 = (Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int) {aLeft17, aRight17};
    Prod_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int a = (Prod_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int_Prod_Prod_Prod_int_int_Prod_int_int_Prod_Prod_int_int_Prod_int_int) {aLeft1, aRight1};
    double start = get_time();
    debug(proc0(a));
    double end = get_time();
    printf("%lf\n", end - start);
    return 0;
}
