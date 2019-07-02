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
void proc1Rt()
{
    List_int v0;
    chan_recv_buf(c1, &v0, sizeof(List_int));
    Sum_Sum_unit_int_Prod_List_int_List_int v1;
    v1 = split(v0);
    Sum_Sum_unit_int_Prod_List_int_List_int v2;
    v2 = v1;
    Label v3;
    v3 = v2.label;
    chan_send_int(c2, v3);
    chan_send_int(c3, v3);
    chan_send_int(c4, v3);
    chan_send_int(c5, v3);
    chan_send_int(c6, v3);
    chan_send_int(c7, v3);
    chan_send_int(c8, v3);
    Sum_unit_int v4;
    Prod_List_int_List_int v5;
    int v6;
    if (v2.label == LEFT)
    {
        v4 = v2.value.left;
        Sum_Sum_unit_int_Prod_List_int_List_int v7;
        v7 = (Sum_Sum_unit_int_Prod_List_int_List_int) {LEFT, { .left = v4 }};
        chan_send_buf(c8,
                      &v7,
                      sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
        v6 = 0;
    }
    else
    {
        v5 = v2.value.right;
        Prod_List_int_List_int v8;
        v8 = v5;
        chan_send_buf(c5, &v8, sizeof(Prod_List_int_List_int));
        List_int v9;
        v9 = v5.snd;
        Sum_Sum_unit_int_Prod_List_int_List_int v10;
        v10 = split(v9);
        Sum_Sum_unit_int_Prod_List_int_List_int v11;
        v11 = v10;
        Label v12;
        v12 = v11.label;
        chan_send_int(c2, v12);
        chan_send_int(c3, v12);
        chan_send_int(c4, v12);
        Sum_unit_int v13;
        Prod_List_int_List_int v14;
        int v15;
        if (v11.label == LEFT)
        {
            v13 = v11.value.left;
            Sum_Sum_unit_int_Prod_List_int_List_int v16;
            v16 = (Sum_Sum_unit_int_Prod_List_int_List_int) {LEFT, { .left = v13 }};
            chan_send_buf(c4,
                          &v16,
                          sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
            v15 = 0;
        }
        else
        {
            v14 = v11.value.right;
            Prod_List_int_List_int v17;
            v17 = v14;
            chan_send_buf(c3, &v17, sizeof(Prod_List_int_List_int));
            List_int v18;
            v18 = v14.snd;
            Sum_Sum_unit_int_Prod_List_int_List_int v19;
            v19 = split(v18);
            Sum_Sum_unit_int_Prod_List_int_List_int v20;
            v20 = v19;
            Label v21;
            v21 = v20.label;
            chan_send_int(c2, v21);
            Sum_unit_int v22;
            Prod_List_int_List_int v23;
            int v24;
            if (v20.label == LEFT)
            {
                v22 = v20.value.left;
                Sum_Sum_unit_int_Prod_List_int_List_int v25;
                v25 = (Sum_Sum_unit_int_Prod_List_int_List_int) {LEFT, { .left = v22 }};
                chan_send_buf(c2,
                              &v25,
                              sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
                v24 = 0;
            }
            else
            {
                v23 = v20.value.right;
                Prod_List_int_List_int v26;
                v26 = v23;
                chan_send_buf(c2, &v26, sizeof(Prod_List_int_List_int));
                List_int v27;
                v27 = v23.snd;
                List_int v28;
                v28 = sort(v27);
                chan_send_buf(c2, &v28, sizeof(List_int));
                v24 = 0;
            }
            v15 = 0;
        }
        v6 = 0;
    }
}
void proc2Rt()
{
    int v30;
    Label v29;
    chan_recv_int(c2, &v29);
    if (v29 == LEFT)
    {
        v30 = 0;
    }
    else
    {
        Label v31;
        chan_recv_int(c2, &v31);
        if (v31 == LEFT)
        {
        }
        else
        {
            Sum_Sum_unit_int_Prod_List_int_List_int v33;
            Label v32;
            chan_recv_int(c2, &v32);
            if (v32 == LEFT)
            {
                Sum_Sum_unit_int_Prod_List_int_List_int v34;
                chan_recv_buf(c2,
                              &v34,
                              sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
                v33 = v34;
            }
            else
            {
                Prod_List_int_List_int v35;
                chan_recv_buf(c2, &v35, sizeof(Prod_List_int_List_int));
                List_int v36;
                v36 = v35.fst;
                List_int v37;
                v37 = sort(v36);
                List_int v38;
                chan_recv_buf(c2, &v38, sizeof(List_int));
                Prod_List_int_List_int v39;
                v39 = (Prod_List_int_List_int) {v37, v38};
                v33 = (Sum_Sum_unit_int_Prod_List_int_List_int) {RIGHT, { .right = v39 }};
            }
            Sum_Sum_unit_int_Prod_List_int_List_int v40;
            v40 = v33;
            List_int v41;
            v41 = merge(v40);
            chan_send_buf(c9, &v41, sizeof(List_int));
        }
        v30 = 0;
    }
}
void proc3Rt()
{
    int v43;
    Label v42;
    chan_recv_int(c3, &v42);
    if (v42 == LEFT)
    {
        v43 = 0;
    }
    else
    {
        int v45;
        Label v44;
        chan_recv_int(c3, &v44);
        if (v44 == LEFT)
        {
            v45 = 0;
        }
        else
        {
            Prod_List_int_List_int v46;
            chan_recv_buf(c3, &v46, sizeof(Prod_List_int_List_int));
            List_int v47;
            v47 = v46.fst;
            Sum_Sum_unit_int_Prod_List_int_List_int v48;
            v48 = split(v47);
            Sum_Sum_unit_int_Prod_List_int_List_int v49;
            v49 = v48;
            Label v50;
            v50 = v49.label;
            chan_send_int(c10, v50);
            Sum_unit_int v51;
            Prod_List_int_List_int v52;
            int v53;
            if (v49.label == LEFT)
            {
                v51 = v49.value.left;
                Sum_Sum_unit_int_Prod_List_int_List_int v54;
                v54 = (Sum_Sum_unit_int_Prod_List_int_List_int) {LEFT, { .left = v51 }};
                chan_send_buf(c10,
                              &v54,
                              sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
                v53 = 0;
            }
            else
            {
                v52 = v49.value.right;
                Prod_List_int_List_int v55;
                v55 = v52;
                chan_send_buf(c10, &v55, sizeof(Prod_List_int_List_int));
                List_int v56;
                v56 = v52.snd;
                List_int v57;
                v57 = sort(v56);
                chan_send_buf(c10, &v57, sizeof(List_int));
                v53 = 0;
            }
            v45 = v53;
        }
        v43 = v45;
    }
}
void proc4Rt()
{
    Label v58;
    chan_recv_int(c4, &v58);
    if (v58 == LEFT)
    {
    }
    else
    {
        Sum_Sum_unit_int_Prod_List_int_List_int v60;
        Label v59;
        chan_recv_int(c4, &v59);
        if (v59 == LEFT)
        {
            Sum_Sum_unit_int_Prod_List_int_List_int v61;
            chan_recv_buf(c4,
                          &v61,
                          sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
            v60 = v61;
        }
        else
        {
            Sum_Sum_unit_int_Prod_List_int_List_int v63;
            Label v62;
            chan_recv_int(c10, &v62);
            if (v62 == LEFT)
            {
                Sum_Sum_unit_int_Prod_List_int_List_int v64;
                chan_recv_buf(c10,
                              &v64,
                              sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
                v63 = v64;
            }
            else
            {
                Prod_List_int_List_int v65;
                chan_recv_buf(c10, &v65, sizeof(Prod_List_int_List_int));
                List_int v66;
                v66 = v65.fst;
                List_int v67;
                v67 = sort(v66);
                List_int v68;
                chan_recv_buf(c10, &v68, sizeof(List_int));
                Prod_List_int_List_int v69;
                v69 = (Prod_List_int_List_int) {v67, v68};
                v63 = (Sum_Sum_unit_int_Prod_List_int_List_int) {RIGHT, { .right = v69 }};
            }
            Sum_Sum_unit_int_Prod_List_int_List_int v70;
            v70 = v63;
            List_int v71;
            v71 = merge(v70);
            List_int v72;
            chan_recv_buf(c9, &v72, sizeof(List_int));
            Prod_List_int_List_int v73;
            v73 = (Prod_List_int_List_int) {v71, v72};
            v60 = (Sum_Sum_unit_int_Prod_List_int_List_int) {RIGHT, { .right = v73 }};
        }
        Sum_Sum_unit_int_Prod_List_int_List_int v74;
        v74 = v60;
        List_int v75;
        v75 = merge(v74);
        chan_send_buf(c11, &v75, sizeof(List_int));
    }
}
void proc5Rt()
{
    int v77;
    Label v76;
    chan_recv_int(c5, &v76);
    if (v76 == LEFT)
    {
        v77 = 0;
    }
    else
    {
        Prod_List_int_List_int v78;
        chan_recv_buf(c5, &v78, sizeof(Prod_List_int_List_int));
        List_int v79;
        v79 = v78.fst;
        Sum_Sum_unit_int_Prod_List_int_List_int v80;
        v80 = split(v79);
        Sum_Sum_unit_int_Prod_List_int_List_int v81;
        v81 = v80;
        Label v82;
        v82 = v81.label;
        chan_send_int(c12, v82);
        chan_send_int(c13, v82);
        chan_send_int(c14, v82);
        Sum_unit_int v83;
        Prod_List_int_List_int v84;
        int v85;
        if (v81.label == LEFT)
        {
            v83 = v81.value.left;
            Sum_Sum_unit_int_Prod_List_int_List_int v86;
            v86 = (Sum_Sum_unit_int_Prod_List_int_List_int) {LEFT, { .left = v83 }};
            chan_send_buf(c14,
                          &v86,
                          sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
            v85 = 0;
        }
        else
        {
            v84 = v81.value.right;
            Prod_List_int_List_int v87;
            v87 = v84;
            chan_send_buf(c13, &v87, sizeof(Prod_List_int_List_int));
            List_int v88;
            v88 = v84.snd;
            Sum_Sum_unit_int_Prod_List_int_List_int v89;
            v89 = split(v88);
            Sum_Sum_unit_int_Prod_List_int_List_int v90;
            v90 = v89;
            Label v91;
            v91 = v90.label;
            chan_send_int(c12, v91);
            Sum_unit_int v92;
            Prod_List_int_List_int v93;
            int v94;
            if (v90.label == LEFT)
            {
                v92 = v90.value.left;
                Sum_Sum_unit_int_Prod_List_int_List_int v95;
                v95 = (Sum_Sum_unit_int_Prod_List_int_List_int) {LEFT, { .left = v92 }};
                chan_send_buf(c12,
                              &v95,
                              sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
                v94 = 0;
            }
            else
            {
                v93 = v90.value.right;
                Prod_List_int_List_int v96;
                v96 = v93;
                chan_send_buf(c12, &v96, sizeof(Prod_List_int_List_int));
                List_int v97;
                v97 = v93.snd;
                List_int v98;
                v98 = sort(v97);
                chan_send_buf(c12, &v98, sizeof(List_int));
                v94 = 0;
            }
            v85 = 0;
        }
        v77 = v85;
    }
}
void proc6Rt()
{
    int v100;
    Label v99;
    chan_recv_int(c6, &v99);
    if (v99 == LEFT)
    {
        v100 = 0;
    }
    else
    {
        Label v101;
        chan_recv_int(c12, &v101);
        if (v101 == LEFT)
        {
        }
        else
        {
            Sum_Sum_unit_int_Prod_List_int_List_int v103;
            Label v102;
            chan_recv_int(c12, &v102);
            if (v102 == LEFT)
            {
                Sum_Sum_unit_int_Prod_List_int_List_int v104;
                chan_recv_buf(c12,
                              &v104,
                              sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
                v103 = v104;
            }
            else
            {
                Prod_List_int_List_int v105;
                chan_recv_buf(c12, &v105, sizeof(Prod_List_int_List_int));
                List_int v106;
                v106 = v105.fst;
                List_int v107;
                v107 = sort(v106);
                List_int v108;
                chan_recv_buf(c12, &v108, sizeof(List_int));
                Prod_List_int_List_int v109;
                v109 = (Prod_List_int_List_int) {v107, v108};
                v103 = (Sum_Sum_unit_int_Prod_List_int_List_int) {RIGHT, { .right = v109 }};
            }
            Sum_Sum_unit_int_Prod_List_int_List_int v110;
            v110 = v103;
            List_int v111;
            v111 = merge(v110);
            chan_send_buf(c15, &v111, sizeof(List_int));
        }
        v100 = 0;
    }
}
void proc7Rt()
{
    int v113;
    Label v112;
    chan_recv_int(c7, &v112);
    if (v112 == LEFT)
    {
        v113 = 0;
    }
    else
    {
        int v115;
        Label v114;
        chan_recv_int(c13, &v114);
        if (v114 == LEFT)
        {
            v115 = 0;
        }
        else
        {
            Prod_List_int_List_int v116;
            chan_recv_buf(c13, &v116, sizeof(Prod_List_int_List_int));
            List_int v117;
            v117 = v116.fst;
            Sum_Sum_unit_int_Prod_List_int_List_int v118;
            v118 = split(v117);
            Sum_Sum_unit_int_Prod_List_int_List_int v119;
            v119 = v118;
            Label v120;
            v120 = v119.label;
            chan_send_int(c16, v120);
            Sum_unit_int v121;
            Prod_List_int_List_int v122;
            int v123;
            if (v119.label == LEFT)
            {
                v121 = v119.value.left;
                Sum_Sum_unit_int_Prod_List_int_List_int v124;
                v124 = (Sum_Sum_unit_int_Prod_List_int_List_int) {LEFT, { .left = v121 }};
                chan_send_buf(c16,
                              &v124,
                              sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
                v123 = 0;
            }
            else
            {
                v122 = v119.value.right;
                Prod_List_int_List_int v125;
                v125 = v122;
                chan_send_buf(c16, &v125, sizeof(Prod_List_int_List_int));
                List_int v126;
                v126 = v122.snd;
                List_int v127;
                v127 = sort(v126);
                chan_send_buf(c16, &v127, sizeof(List_int));
                v123 = 0;
            }
            v115 = v123;
        }
        v113 = v115;
    }
}
void proc8Rt()
{
    Sum_Sum_unit_int_Prod_List_int_List_int v129;
    Label v128;
    chan_recv_int(c8, &v128);
    if (v128 == LEFT)
    {
        Sum_Sum_unit_int_Prod_List_int_List_int v130;
        chan_recv_buf(c8,
                      &v130,
                      sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
        v129 = v130;
    }
    else
    {
        Sum_Sum_unit_int_Prod_List_int_List_int v132;
        Label v131;
        chan_recv_int(c14, &v131);
        if (v131 == LEFT)
        {
            Sum_Sum_unit_int_Prod_List_int_List_int v133;
            chan_recv_buf(c14,
                          &v133,
                          sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
            v132 = v133;
        }
        else
        {
            Sum_Sum_unit_int_Prod_List_int_List_int v135;
            Label v134;
            chan_recv_int(c16, &v134);
            if (v134 == LEFT)
            {
                Sum_Sum_unit_int_Prod_List_int_List_int v136;
                chan_recv_buf(c16,
                              &v136,
                              sizeof(Sum_Sum_unit_int_Prod_List_int_List_int));
                v135 = v136;
            }
            else
            {
                Prod_List_int_List_int v137;
                chan_recv_buf(c16, &v137, sizeof(Prod_List_int_List_int));
                List_int v138;
                v138 = v137.fst;
                List_int v139;
                v139 = sort(v138);
                List_int v140;
                chan_recv_buf(c16, &v140, sizeof(List_int));
                Prod_List_int_List_int v141;
                v141 = (Prod_List_int_List_int) {v139, v140};
                v135 = (Sum_Sum_unit_int_Prod_List_int_List_int) {RIGHT, { .right = v141 }};
            }
            Sum_Sum_unit_int_Prod_List_int_List_int v142;
            v142 = v135;
            List_int v143;
            v143 = merge(v142);
            List_int v144;
            chan_recv_buf(c15, &v144, sizeof(List_int));
            Prod_List_int_List_int v145;
            v145 = (Prod_List_int_List_int) {v143, v144};
            v132 = (Sum_Sum_unit_int_Prod_List_int_List_int) {RIGHT, { .right = v145 }};
        }
        Sum_Sum_unit_int_Prod_List_int_List_int v146;
        v146 = v132;
        List_int v147;
        v147 = merge(v146);
        List_int v148;
        chan_recv_buf(c11, &v148, sizeof(List_int));
        Prod_List_int_List_int v149;
        v149 = (Prod_List_int_List_int) {v147, v148};
        v129 = (Sum_Sum_unit_int_Prod_List_int_List_int) {RIGHT, { .right = v149 }};
    }
    Sum_Sum_unit_int_Prod_List_int_List_int v150;
    v150 = v129;
    List_int v151;
    v151 = merge(v150);
    chan_send_buf(c17, &v151, sizeof(List_int));
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
List_int proc0(List_int v0)
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
    chan_send_buf(c1, &v0, sizeof(List_int));
    List_int v1;
    chan_recv_buf(c17, &v1, sizeof(List_int));
    pthread_join(th1, NULL);
    pthread_join(th2, NULL);
    pthread_join(th3, NULL);
    pthread_join(th4, NULL);
    pthread_join(th5, NULL);
    pthread_join(th6, NULL);
    pthread_join(th7, NULL);
    pthread_join(th8, NULL);
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
    return v1;
}
int main()
{
    int tmp[] = { 49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 };
    List_int a = (List_int) {49, tmp};
    double start = get_time();
    debug(proc0(a));
    double end = get_time();
    printf("%lf\n", end - start);
    return 0;
}
