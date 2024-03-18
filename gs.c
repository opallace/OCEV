#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef struct {
    int *alelo;
    int n_alelos;
    int fitness;
} CROMOSSOMO;

typedef struct {
    CROMOSSOMO **populacao;
    int n_populacao;
} POPULACAO;

CROMOSSOMO* inicializa_cromossomo(int n_alelos){
    CROMOSSOMO *c = malloc(sizeof(CROMOSSOMO));
    c->alelo      = malloc(sizeof(int) * n_alelos);
    c->n_alelos   = n_alelos;
    c->fitness    = 0; 

    for (int i = 0; i < n_alelos; i++){
        c->alelo[i] = rand() % 2;
    }
    

    return c;
}

POPULACAO* inicializa_populacao(int n_populacao, int n_alelos){
    POPULACAO *p  = malloc(sizeof(POPULACAO));
    
    p->populacao  = malloc(sizeof(CROMOSSOMO*) * n_populacao);
    p->n_populacao = n_populacao;

    for (int i = 0; i < n_populacao; i++){
        p->populacao[i] = inicializa_cromossomo(n_alelos);
    }
    
    return p;
}

void mostra_populacao(POPULACAO* p){
    for (int i = 0; i < p->n_populacao; i++){
        printf("Cromossomo %i [", i);

        for (int j = 0; j < p->populacao[0]->n_alelos - 1; j++){
            printf("%i,", p->populacao[i]->alelo[j]);
        }
        
        printf("%i]\n", p->populacao[i]->alelo[p->populacao[i]->n_alelos - 1]);
    }
}

void avalia_cromossomo(CROMOSSOMO *c, int clausulas[430][3]){
    for (size_t i = 0; i < 430; i++){
        int l1 = clausulas[i][0];
        int l2 = clausulas[i][1];
        int l3 = clausulas[i][2];

        if(l1 < 0){
            l1 = !c->alelo[l1];
        }else {
            l1 = c->alelo[l1];
        }

        if(l2 < 0){
            l2 = !c->alelo[l2];
        }else {
            l2 = c->alelo[l2];
        }

        if(l3 < 0){
            l3 = !c->alelo[l3];
        }else {
            l3 = c->alelo[l3];
        }

        if(l1 || l2 || l3){
            c->fitness++;
        }
    }
}

int main(){

    srand(time(NULL));

    FILE *f = fopen("entrada.txt", "r");

    char *file_contents = malloc(30);
    int clausulas[430][3];
    int i = 0;

     while (fscanf(f, "%[^\n] ", file_contents) != EOF) {
        sscanf(file_contents, "%i %i %i 0", &clausulas[i][0], &clausulas[i][1], &clausulas[i][2]);
        // printf("%i : %i %i %i\n", i, clausulas[i][0], clausulas[i][1], clausulas[i][2]);
        i++;
    }

    POPULACAO *p = inicializa_populacao(10, 100);

    do{
        
    } while ();
    




    int notfind = 1;

    while(notfind){
        for (int i = 0; i < p->n_populacao; i++){
            avalia_cromossomo(p->populacao[i], clausulas);


            if(p->populacao[i]->fitness == 429){
                notfind = 0;

                printf("Cromossomo vencedor %i [", i);

                for (int j = 0; j < p->populacao[i]->n_alelos - 1; j++){
                    printf("%i,", p->populacao[i]->alelo[j]);
                }
        
                printf("%i]\n", p->populacao[i]->alelo[p->populacao[i]->n_alelos - 1]);

                break;
            }
        }

        p = inicializa_populacao(10, 100);
        // mostra_populacao(p);
    }

    
    




   

    return 0;
}