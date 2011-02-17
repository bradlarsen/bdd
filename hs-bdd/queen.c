/**************************************************************************
  BDD demonstration of the N-Queen chess problem.
  -----------------------------------------------
  The BDD variables correspond to a NxN chess board like:

     0    N      2N     ..  N*N-N
     1    N+1    2N+1   ..  N*N-N+1
     2    N+2    2N+2   ..  N*N-N+2
     ..   ..     ..     ..  ..
     N-1  2N-1   3N-1   ..  N*N-1

   So for example a 4x4 is:

     0 4  8 12
     1 5  9 13
     2 6 10 14
     3 7 11 15

   One solution is then that 2,4,11,13 should be true, meaning a queen
   should be placed there:

     . X . .
     . . . X
     X . . .
     . . X .

**************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "bddlib.h"

int N;                   /* Size of the chess board */
bdd_t **X;               /* BDD variable array */
bdd_t queen;             /* N-queen problem express as a BDD */
bdd_mgr_t *mgr;

static void
ref (bdd_t b)
{
    bdd_inc_ref (mgr, b);
    /* bdd_mgr_perform_gc (mgr); */
}

static void
deref (bdd_t b)
{
    bdd_dec_ref (mgr, b);
}

/* Build the requirements for all other fields than (i,j) assuming
   that (i,j) has a queen */
static void build(int i, int j)
{
    bdd_t a, b, c, d;
    bdd_t ab, abc, abcd, queen_abcd;
    int k,l;

    a = bdd_true;
    ref (a);
    b = bdd_true;
    ref (b);
    c = bdd_true;
    ref (c);
    d = bdd_true;
    ref (d);
   
    /* No one in the same column */
    for (l=0 ; l<N ; l++)
        if (l != j) {
            bdd_t tmp1, tmp2, tmp3;
            tmp1 = bdd_not (mgr, X[i][l]);
            ref (tmp1);
            tmp2 = bdd_implies (mgr, X[i][j], tmp1);
            ref (tmp2);
            tmp3 = bdd_and (mgr, a, tmp2);
            ref (tmp3);
            deref (a);
            a = tmp3;
            ref (a);
            deref (tmp3);
            deref (tmp2);
            deref (tmp1);
        }

    /* No one in the same row */
    for (k=0 ; k<N ; k++)
        if (k != i) {
            bdd_t tmp1, tmp2, tmp3;
            tmp1 = bdd_not (mgr, X[k][j]);
            ref (tmp1);
            tmp2 = bdd_implies (mgr, X[i][j], tmp1);
            ref (tmp2);
            tmp3 = bdd_and (mgr, b, tmp2);
            ref (tmp3);
            deref (b);
            b = tmp3;
            ref (b);
            deref (tmp3);
            deref (tmp2);
            deref (tmp1);
        }

    /* No one in the same up-right diagonal */
    for (k=0 ; k<N ; k++)
    {
        int ll = k-i+j;
        if (ll>=0 && ll<N)
            if (k != i) {
                bdd_t tmp1, tmp2, tmp3;
                tmp1 = bdd_not (mgr, X[k][ll]);
                ref (tmp1);
                tmp2 = bdd_implies (mgr, X[i][j], tmp1);
                ref (tmp2);
                tmp3 = bdd_and (mgr, c, tmp2);
                ref (tmp3);
                deref (c);
                c = tmp3;
                ref (c);
                deref (tmp3);
                deref (tmp2);
                deref (tmp1);
            }
    }

    /* No one in the same down-right diagonal */
    for (k=0 ; k<N ; k++)
    {
        int ll = i+j-k;
        if (ll>=0 && ll<N)
            if (k != i) {
                bdd_t tmp1, tmp2, tmp3;
                tmp1 = bdd_not (mgr, X[k][ll]);
                ref (tmp1);
                tmp2 = bdd_implies (mgr, X[i][j], tmp1);
                ref (tmp2);
                tmp3 = bdd_and (mgr, d, tmp2);
                ref (tmp3);
                deref (d);
                d = tmp3;
                ref (d);
                deref (tmp3);
                deref (tmp2);
                deref (tmp1);
            }
    }

    ab = bdd_and (mgr, a, b);
    ref (ab);
    abc = bdd_and (mgr, ab, c);
    ref (abc);
    abcd = bdd_and (mgr, abc, d);
    ref (abcd);
    queen_abcd = bdd_and (mgr, queen, abcd);
    ref (queen_abcd);
    deref (queen);
    queen = queen_abcd;
    ref (queen);

    deref (ab);
    deref (abc);
    deref (abcd);
    deref (queen_abcd);
    deref (d);
    deref (c);
    deref (b);
    deref (a);
}

static void
print_results ()
{
    void _bdd_mgr_var_order_fprint (bdd_mgr_t *mgr, FILE *handle);

    _bdd_mgr_var_order_fprint (mgr, stderr);
    fprintf (stderr, "There are %.0f solutions\n", bdd_sat_count(mgr, queen));
    fprintf (stderr, "%u nodes in use\n", bdd_mgr_get_num_nodes(mgr));
    fprintf (stderr, "%u nodes currently allocated\n",
             bdd_mgr_get_num_allocated(mgr));
}

static void
randomly_swap_variable_order ()
{
    int num_swaps;
    for (num_swaps = 0; num_swaps < 2 * N * N; num_swaps += 1) {
        unsigned idx = rand () % (N * N - 1);
        bdd_mgr_swap_variables (mgr, idx);
    }
}

static void
random_test_var_swapping ()
{
    unsigned i;
    for (i = 0; i < 128; i += 1) {
        double old_num_solutions = bdd_sat_count (mgr, queen);
        randomly_swap_variable_order ();
        print_results ();
        assert (bdd_sat_count (mgr, queen) == old_num_solutions);
    }
}

int main(int ac, char **av)
{
    int n,i,j;

    if (ac != 2)
    {
        fprintf(stderr, "USAGE:  queen N\n");
        return 1;
    }

    N = atoi(av[1]);
    if (N <= 0)
    {
        fprintf(stderr, "USAGE:  queen N\n");
        return 1;
    }

    mgr = bdd_mgr_create_with_hint(N*N, 1024);

    fprintf (stderr, "initialized manager\n");

    queen = bdd_true;
    ref (queen);
   
    /* Build variable array */
    X = (bdd_t **) malloc (N * sizeof(bdd_t**));
    for (n=0 ; n<N ; n++)
        X[n] = (bdd_t *) malloc (N * sizeof(bdd_t*));

    fprintf (stderr, "allocated variable array\n");

    for (i=0 ; i<N ; i++)
        for (j=0 ; j<N ; j++) {
            X[i][j] = bdd_ith_var(mgr, i*N+j);
            ref (X[i][j]);
            fprintf (stderr, "added variable for %dx%d\n", i, j);
        }


    fprintf (stderr, "built variable array\n");

    /* Place a queen in each row */
    for (i=0 ; i<N ; i++)
    {
        bdd_t queen_e;
        bdd_t e = bdd_false;
        ref (e);
        for (j=0 ; j<N ; j++) {
            bdd_t tmp1 = bdd_or (mgr, e, X[i][j]);
            ref (tmp1);
            deref (e);
            e = tmp1;
            ref (e);
            deref (tmp1);
        }
        queen_e = bdd_and (mgr, queen, e);
        ref (queen_e);
        deref (queen);
        queen = queen_e;
        ref (queen);
        deref (queen_e);
        deref (e);
        fprintf (stderr, "placed a queen in row %d\n", i);
    }

    /* Build requirements for each variable(field) */
    for (i=0 ; i<N ; i++)
    {
        for (j=0 ; j<N ; j++)
        {
            fprintf (stderr, "Adding position %d, %d\n", i, j);
            build(i,j);
        }
    }

    print_results ();

    random_test_var_swapping ();

    fprintf (stderr, "result cache: ");
    bdd_cache_stats_fprint (stderr, bdd_mgr_get_cache_stats (mgr));
    fprintf (stderr, "\n");

    bdd_mgr_destroy(mgr);

    for (n=0 ; n<N ; n++)
        free (X[n]);
    free (X);

    return 0;
}
