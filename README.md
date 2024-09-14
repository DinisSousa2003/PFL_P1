# Polynomial Calculator

## PFL 2022/23 - 3rd Year
## Functional and Logic Programming

## Description

Polynomial Calculator,used to normalize, multiply, derive, read and write polynomials. Developed using Haskell


#### Escolha da representação interna dos polinómios:
Os polinómios estão representados como uma lista de elementos de um polinómio (monómios). Estes elementos são representados por um par (coeficiente, lista de pares variável e grau). Escolhemos esta representação de forma a permitir que um elemento de um polinómio tivesse várias variáveis, cada uma com um grau diferente e ambos estivessem associados um ao outro num par. Cada elemento tem apenas um coeficiente e uma lista de variáveis e graus, logo associá-los também num par pareceu lógico. O polinómio ser um conjunto de monómios parte da definição de polinómio, e achamos que coloca-los numa lista seria uma boa opção.

```hs
type PolElement = (Float, [(Char, Float)]) {-Coeficient, variable, grade-}
type Polinomyal = [PolElement]
```

#### Descrição de cada funcionalidade
- Começando pela função de **normalizar polinómios**, esta ação é realizada pela função *reducePolinomial*. Primeiro todos os elementos são reduzidos (ou seja, x^2\*x^2 -> x^4). De seguida são filtrados todos os elementos que tenham o coeficiente 0, as variáveis que tenham expoente 0 são removidas (x^0 = 1, que é o elemento neutro da mutiplicação) e finalmente todos os *PolElement* que tenham uma lista de variáveis e graus iguais (as variáveis e graus de um termo são ordenadas para poder fazer a comparação entre termos) juntam-se no mesmo elemento e vêm os seus coeficientes somados. No final, os elementos são postos por ordem, primeiro surgem aqueles que têm mais variáveis, em caso de empate ordem alfabética das variáveis e em caso de empate grau. Isto é feito para ser possível comparar facilmente polinómios e ser também mais fácil a um utilizador de os ler.

- A função de **adicionar polinómios** funciona de forma idêntica à de reduzir um polinómio. Na verdade, o ato de somar dois polinómios nada mais é do que juntar dois polinómios num só e reduzir o novo polinómio à forma normal. E foi exatamente isso que fizemos, na função *addPolinomial*.

- Para a **mutiplicação de polinómios**, todos os membros de um polinómio 1 são mutiplicados por todos os membros de um polinómio 2. Então, dando uso da recursão, mutiplicamos cada termo do primeiro polinómio por cada termo do segundo polinómio. E em que consiste a mutiplicação de dois termos de um polinómio? Mutiplicam-se os coeficientes, e 'juntam-se' as variáveis, somando os graus das variáveis iguais. Para isto, usamos a função já definida para a redução de um *PolElement*, *reduceTerm*. Esta funcionalidade está implementada na função *multPolinomial*.

- Por último, no que toca às funções obrigatórias, a função de **derivação de um polinómio**. Para derivar um polinómio, é primeiro preciso saber em função de que termo queremos derivar esse polinómio. Daí esta função receber como argumentos um char (a variável em ordem à qual queremos derivar) e um polinómio. Depois disso, é preciso verificar em cada membro do polinómio se a variável em ordem à qual estamos a derivar está presente. Se não estiver, não há nada a fazer, se estiver, reduz-se o grau do membro em uma unidade e mutiplica-se o coeficiente pelo antigo grau. Esta funcionalidade encontra-se implementada em *derivePolinomial*

- Além das funcionalidade obrigatórias, temos ainda a destacar o **parse de uma string num polinómio**, através da função *parseInit*. Para isso, dada uma string, primeiro dividimos a string pelos seus '+' e '-' (exceto aqueles que vêm depois de um ^ de forma a permitir expoentes negativos). Assim, temos uma lista de strings em que cada uma representa um elemento de um polinómio. Depois disso, é feito o parse de cada termo, na função *parseNewTerm*. Dentro de cada termo, a leitura é feita numa ideia de 'máquina de estados'. Primeiro verifica-se de que forma é que começa o termo. Se o termo começar por um número (ou sinal, no caso de ser um número negativo), lê se até chegar ao primeiro termo não numérico e a informação é guardada no coeficiente. Com o resto da string, é feito o parse das variáveis e respetivos graus, sendo os graus lidos de forma similar aos coeficientes. Se o termo começar com uma letra, passa-se a parte do coeficiente à frente, sendo este colocado a 1.

- Por último a função que nos permite escrever a informação guardada sobre os polinómios de uma forma legível ao utilizador, a função que **transforma o polinómio em string**, *polinomyalToString*. Cada elemento é transformado numa string, que começa pelo seu sinal, coeficiente e depois a escrita das variáveis e respetivo grau. Se o polinómio for nulo é devolvido 0.

- Nota: Qualquer elemento do polinómio lido pode ter coeficiente ou expoente negativo, e os coeficientes e expoentes podem ser dados como decimais (ex.: -123.7).

#### Exemplos de utilização do programa
Para correr o programa, basta ter os ficheiros necessários no mesmo diretório do io.hs (Logic.hs, Parse.hs, Print.hs) e dar ao ficheiro io.hs (:l io.hs). De seguida basta escrever main, e irá aparecer na consola texto do programa, a interagir com o utilizador.

**Exemplos de polinómios:**
```
1
-3.10*x^2*y^7.3 + 2*x^2*x^3
2*z^3
```

**Exemplos de utilização**
- Redução e derivação <br>
![Reducing and deriving a polinomial](https://i.imgur.com/vMJGFu4.png)

- Adição <br>
![Adding polinomials](https://i.imgur.com/1IFTRbN.png)

- Mutiplicação <br>
![Mutiplying polinomials](https://i.imgur.com/gJNmiLS.png)


**Com uso do QuickCheck:**

Para correr os testes automáticos basta escrever numa linha de comando:

``` 
cabal update
cabal install --lib QuickCheck
ghci
ghci> import Test.QuickCheck
ghci> quickCheck ...(nome do teste)
```

Os três testes que temos disponíveis são:
 - prop_Add
 - prop_Mult
 - prop_Reduce

Ao correr os testes, apesar de os da adição e da redução funcionarem como esperado, o da multiplicação deu erro no arreondamento dos floats, algo que não conseguimos resolver.
A primeira imagem mostra a mensagem de erro do teste:<br>
![Error in prop_Mult](https://i.imgur.com/4Hj5iRe.png)

A segunda é o resultado do teste e mostra o valor diferente(1.9000001 /= 1.9)<br>
![Reason of error](https://i.imgur.com/Kx9ZVGM.png)

Quando os testes são bem sucedidos o resultado aparece neste formato:<br>
![Correct result](https://i.imgur.com/wVOGITa.png)
