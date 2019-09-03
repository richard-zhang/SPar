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
chan_t * c19;
chan_t * c20;
chan_t * c21;
chan_t * c22;
chan_t * c23;
chan_t * c24;
chan_t * c25;
chan_t * c26;
chan_t * c27;
void proc1Rt()
{
    List_Prod_float_float v0;
    chan_recv_buf(c1, &v0, sizeof(List_Prod_float_float));
    List_Prod_float_float v1;
    v1 = addPadding(v0);
    Prod_List_Prod_float_float_List_Prod_float_float v2;
    v2 = splitList(v1);
    List_Prod_float_float v3;
    v3 = v2.fst;
    chan_send_buf(c2, &v3, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v4;
    v4 = splitList(v2.snd);
    List_Prod_float_float v5;
    v5 = v4.fst;
    chan_send_buf(c3, &v5, sizeof(List_Prod_float_float));
    List_Prod_float_float v6;
    v6 = v4.snd;
    chan_send_buf(c3, &v6, sizeof(List_Prod_float_float));
}
void proc2Rt()
{
    List_Prod_float_float v7;
    chan_recv_buf(c3, &v7, sizeof(List_Prod_float_float));
    List_Prod_float_float v8;
    v8 = v7;
    List_Prod_float_float v9;
    chan_recv_buf(c3, &v9, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v10;
    v10 = (Prod_List_Prod_float_float_List_Prod_float_float) {v8, v9};
    chan_send_buf(c4,
                  &v10,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc3Rt()
{
    List_Prod_float_float v11;
    chan_recv_buf(c2, &v11, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v12;
    v12 = splitList(v11);
    List_Prod_float_float v13;
    v13 = v12.fst;
    chan_send_buf(c5, &v13, sizeof(List_Prod_float_float));
    List_Prod_float_float v14;
    v14 = v12.snd;
    chan_send_buf(c5, &v14, sizeof(List_Prod_float_float));
}
void proc4Rt()
{
    List_Prod_float_float v15;
    chan_recv_buf(c5, &v15, sizeof(List_Prod_float_float));
    List_Prod_float_float v16;
    v16 = v15;
    List_Prod_float_float v17;
    chan_recv_buf(c5, &v17, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v18;
    v18 = (Prod_List_Prod_float_float_List_Prod_float_float) {v16, v17};
    Prod_List_Prod_float_float_List_Prod_float_float v19;
    chan_recv_buf(c4,
                  &v19,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v20;
    v20 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v18, v19};
    Prod_List_Prod_float_float_List_Prod_float_float v21;
    v21 = v20.fst;
    chan_send_buf(c6,
                  &v21,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v22;
    v22 = v20.snd.fst;
    chan_send_buf(c7, &v22, sizeof(List_Prod_float_float));
    List_Prod_float_float v23;
    v23 = baseFFT(v20.snd.snd);
    chan_send_buf(c7, &v23, sizeof(List_Prod_float_float));
}
void proc5Rt()
{
    List_Prod_float_float v24;
    chan_recv_buf(c7, &v24, sizeof(List_Prod_float_float));
    List_Prod_float_float v25;
    v25 = baseFFT(v24);
    List_Prod_float_float v26;
    chan_recv_buf(c7, &v26, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v27;
    v27 = (Prod_List_Prod_float_float_List_Prod_float_float) {v25, v26};
    List_Prod_float_float v28;
    v28 = v27.fst;
    chan_send_buf(c8, &v28, sizeof(List_Prod_float_float));
    List_Prod_float_float v29;
    v29 = cmulexp(2, 0, v27.snd);
    chan_send_buf(c8, &v29, sizeof(List_Prod_float_float));
}
void proc6Rt()
{
    List_Prod_float_float v30;
    chan_recv_buf(c8, &v30, sizeof(List_Prod_float_float));
    List_Prod_float_float v31;
    v31 = v30;
    List_Prod_float_float v32;
    chan_recv_buf(c8, &v32, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v33;
    v33 = (Prod_List_Prod_float_float_List_Prod_float_float) {v31, v32};
    Prod_List_Prod_float_float_List_Prod_float_float v34;
    v34 = v33;
    chan_send_buf(c9,
                  &v34,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc7Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v35;
    chan_recv_buf(c9,
                  &v35,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v36;
    v36 = addSub(v35);
    chan_send_buf(c10,
                  &v36,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc8Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v37;
    chan_recv_buf(c10,
                  &v37,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v38;
    v38 = v37;
    Prod_List_Prod_float_float_List_Prod_float_float v39;
    v39 = v38;
    chan_send_buf(c11,
                  &v39,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc9Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v40;
    chan_recv_buf(c6,
                  &v40,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v41;
    v41 = v40.fst;
    chan_send_buf(c12, &v41, sizeof(List_Prod_float_float));
    List_Prod_float_float v42;
    v42 = baseFFT(v40.snd);
    chan_send_buf(c12, &v42, sizeof(List_Prod_float_float));
}
void proc10Rt()
{
    List_Prod_float_float v43;
    chan_recv_buf(c12, &v43, sizeof(List_Prod_float_float));
    List_Prod_float_float v44;
    v44 = baseFFT(v43);
    List_Prod_float_float v45;
    chan_recv_buf(c12, &v45, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v46;
    v46 = (Prod_List_Prod_float_float_List_Prod_float_float) {v44, v45};
    List_Prod_float_float v47;
    v47 = v46.fst;
    chan_send_buf(c13, &v47, sizeof(List_Prod_float_float));
    List_Prod_float_float v48;
    v48 = cmulexp(2, 0, v46.snd);
    chan_send_buf(c13, &v48, sizeof(List_Prod_float_float));
}
void proc11Rt()
{
    List_Prod_float_float v49;
    chan_recv_buf(c13, &v49, sizeof(List_Prod_float_float));
    List_Prod_float_float v50;
    v50 = v49;
    List_Prod_float_float v51;
    chan_recv_buf(c13, &v51, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v52;
    v52 = (Prod_List_Prod_float_float_List_Prod_float_float) {v50, v51};
    Prod_List_Prod_float_float_List_Prod_float_float v53;
    v53 = v52;
    chan_send_buf(c14,
                  &v53,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc12Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v54;
    chan_recv_buf(c14,
                  &v54,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v55;
    v55 = addSub(v54);
    chan_send_buf(c15,
                  &v55,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc13Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v56;
    chan_recv_buf(c15,
                  &v56,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v57;
    v57 = v56;
    Prod_List_Prod_float_float_List_Prod_float_float v58;
    v58 = v57;
    Prod_List_Prod_float_float_List_Prod_float_float v59;
    chan_recv_buf(c11,
                  &v59,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v60;
    v60 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v58, v59};
    Prod_List_Prod_float_float_List_Prod_float_float v61;
    v61 = v60.fst;
    chan_send_buf(c16,
                  &v61,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v62;
    v62 = v60.snd.fst;
    chan_send_buf(c17, &v62, sizeof(List_Prod_float_float));
    List_Prod_float_float v63;
    v63 = cmulexp(4, 1, v60.snd.snd);
    chan_send_buf(c17, &v63, sizeof(List_Prod_float_float));
}
void proc14Rt()
{
    List_Prod_float_float v64;
    chan_recv_buf(c17, &v64, sizeof(List_Prod_float_float));
    List_Prod_float_float v65;
    v65 = cmulexp(4, 0, v64);
    List_Prod_float_float v66;
    chan_recv_buf(c17, &v66, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v67;
    v67 = (Prod_List_Prod_float_float_List_Prod_float_float) {v65, v66};
    chan_send_buf(c18,
                  &v67,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc15Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v68;
    chan_recv_buf(c16,
                  &v68,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v69;
    v69 = v68;
    Prod_List_Prod_float_float_List_Prod_float_float v70;
    chan_recv_buf(c18,
                  &v70,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v71;
    v71 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v69, v70};
    Prod_List_Prod_float_float_List_Prod_float_float v72;
    v72 = (Prod_List_Prod_float_float_List_Prod_float_float) {v71.fst.fst, v71.snd.fst};
    chan_send_buf(c19,
                  &v72,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v73;
    v73 = (Prod_List_Prod_float_float_List_Prod_float_float) {v71.fst.snd, v71.snd.snd};
    chan_send_buf(c20,
                  &v73,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc16Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v74;
    chan_recv_buf(c19,
                  &v74,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v75;
    v75 = addSub(v74);
    chan_send_buf(c21,
                  &v75,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc17Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v76;
    chan_recv_buf(c20,
                  &v76,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v77;
    v77 = addSub(v76);
    chan_send_buf(c22,
                  &v77,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
}
void proc18Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v78;
    chan_recv_buf(c21,
                  &v78,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v79;
    chan_recv_buf(c22,
                  &v79,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v80;
    v80 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {v78, v79};
    Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float v81;
    v81 = (Prod_Prod_List_Prod_float_float_List_Prod_float_float_Prod_List_Prod_float_float_List_Prod_float_float) {(Prod_List_Prod_float_float_List_Prod_float_float) {v80.fst.fst, v80.snd.fst}, (Prod_List_Prod_float_float_List_Prod_float_float) {v80.fst.snd, v80.snd.snd}};
    Prod_List_Prod_float_float_List_Prod_float_float v82;
    v82 = v81.fst;
    chan_send_buf(c23,
                  &v82,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v83;
    v83 = v81.snd.fst;
    chan_send_buf(c24, &v83, sizeof(List_Prod_float_float));
    List_Prod_float_float v84;
    v84 = v81.snd.snd;
    chan_send_buf(c24, &v84, sizeof(List_Prod_float_float));
}
void proc19Rt()
{
    List_Prod_float_float v85;
    chan_recv_buf(c24, &v85, sizeof(List_Prod_float_float));
    List_Prod_float_float v86;
    v86 = v85;
    List_Prod_float_float v87;
    chan_recv_buf(c24, &v87, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v88;
    v88 = (Prod_List_Prod_float_float_List_Prod_float_float) {v86, v87};
    List_Prod_float_float v89;
    v89 = concatenate(v88);
    chan_send_buf(c25, &v89, sizeof(List_Prod_float_float));
}
void proc20Rt()
{
    Prod_List_Prod_float_float_List_Prod_float_float v90;
    chan_recv_buf(c23,
                  &v90,
                  sizeof(Prod_List_Prod_float_float_List_Prod_float_float));
    List_Prod_float_float v91;
    v91 = v90.fst;
    chan_send_buf(c26, &v91, sizeof(List_Prod_float_float));
    List_Prod_float_float v92;
    v92 = v90.snd;
    chan_send_buf(c26, &v92, sizeof(List_Prod_float_float));
}
void proc21Rt()
{
    List_Prod_float_float v93;
    chan_recv_buf(c26, &v93, sizeof(List_Prod_float_float));
    List_Prod_float_float v94;
    v94 = v93;
    List_Prod_float_float v95;
    chan_recv_buf(c26, &v95, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v96;
    v96 = (Prod_List_Prod_float_float_List_Prod_float_float) {v94, v95};
    List_Prod_float_float v97;
    v97 = concatenate(v96);
    List_Prod_float_float v98;
    chan_recv_buf(c25, &v98, sizeof(List_Prod_float_float));
    Prod_List_Prod_float_float_List_Prod_float_float v99;
    v99 = (Prod_List_Prod_float_float_List_Prod_float_float) {v97, v98};
    List_Prod_float_float v100;
    v100 = concatenate(v99);
    chan_send_buf(c27, &v100, sizeof(List_Prod_float_float));
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
void * proc11()
{
    proc11Rt();
    return NULL;
}
void * proc12()
{
    proc12Rt();
    return NULL;
}
void * proc13()
{
    proc13Rt();
    return NULL;
}
void * proc14()
{
    proc14Rt();
    return NULL;
}
void * proc15()
{
    proc15Rt();
    return NULL;
}
void * proc16()
{
    proc16Rt();
    return NULL;
}
void * proc17()
{
    proc17Rt();
    return NULL;
}
void * proc18()
{
    proc18Rt();
    return NULL;
}
void * proc19()
{
    proc19Rt();
    return NULL;
}
void * proc20()
{
    proc20Rt();
    return NULL;
}
void * proc21()
{
    proc21Rt();
    return NULL;
}
List_Prod_float_float proc0(List_Prod_float_float v0)
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
    c19 = chan_init(1);
    c20 = chan_init(1);
    c21 = chan_init(1);
    c22 = chan_init(1);
    c23 = chan_init(1);
    c24 = chan_init(1);
    c25 = chan_init(1);
    c26 = chan_init(1);
    c27 = chan_init(1);
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
    pthread_t th11;
    pthread_create(&th11, NULL, proc11, NULL);
    pthread_t th12;
    pthread_create(&th12, NULL, proc12, NULL);
    pthread_t th13;
    pthread_create(&th13, NULL, proc13, NULL);
    pthread_t th14;
    pthread_create(&th14, NULL, proc14, NULL);
    pthread_t th15;
    pthread_create(&th15, NULL, proc15, NULL);
    pthread_t th16;
    pthread_create(&th16, NULL, proc16, NULL);
    pthread_t th17;
    pthread_create(&th17, NULL, proc17, NULL);
    pthread_t th18;
    pthread_create(&th18, NULL, proc18, NULL);
    pthread_t th19;
    pthread_create(&th19, NULL, proc19, NULL);
    pthread_t th20;
    pthread_create(&th20, NULL, proc20, NULL);
    pthread_t th21;
    pthread_create(&th21, NULL, proc21, NULL);
    chan_send_buf(c1, &v0, sizeof(List_Prod_float_float));
    List_Prod_float_float v1;
    chan_recv_buf(c27, &v1, sizeof(List_Prod_float_float));
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
    pthread_join(th11, NULL);
    pthread_join(th12, NULL);
    pthread_join(th13, NULL);
    pthread_join(th14, NULL);
    pthread_join(th15, NULL);
    pthread_join(th16, NULL);
    pthread_join(th17, NULL);
    pthread_join(th18, NULL);
    pthread_join(th19, NULL);
    pthread_join(th20, NULL);
    pthread_join(th21, NULL);
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
    chan_dispose(c19);
    chan_dispose(c20);
    chan_dispose(c21);
    chan_dispose(c22);
    chan_dispose(c23);
    chan_dispose(c24);
    chan_dispose(c25);
    chan_dispose(c26);
    chan_dispose(c27);
    return v1;
}
int main()
{
    Prod_float_float tmp[] = { (Prod_float_float) {1.0, 0.0}, (Prod_float_float) {1.0, 0.0}, (Prod_float_float) {1.0, 0.0}, (Prod_float_float) {1.0, 0.0}, (Prod_float_float) {0.0, 0.0}, (Prod_float_float) {0.0, 0.0}, (Prod_float_float) {0.0, 0.0}, (Prod_float_float) {0.0, 0.0} };
    List_Prod_float_float a = (List_Prod_float_float) {8, tmp};
    double start = get_time();
    debug(proc0(a));
    double end = get_time();
    printf("%lf\n", end - start);
    return 0;
}
