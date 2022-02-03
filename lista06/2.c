#include <stdio.h>
#define MENOR(X, Y) (X > Y ? Y : X)

int contagemAvaliacoes = 0;

int retornaValor(int valor)
{
    contagemAvaliacoes++;
    return valor;
}

int main()
{
    MENOR(5, retornaValor(3));
    printf("Numero de avaliacoes: %d\n", contagemAvaliacoes);
    return 0;
}