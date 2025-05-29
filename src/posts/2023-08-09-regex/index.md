---
layout: ../../layouts/PostLayout.astro
title: De onde vem o RegEx? (rascunho)
subtitle: Implementando expressões regulares a partir da teoria
author: Eduardo Sandalo Porto
date: August 9, 2023
lang: Portuguese
---

<!-- <img src="./img/nanana.png" style="width: 70%; margin: auto; display: block;"> -->

*Nota: Esta versão do artigo é um rascunho e está incompleta!*

As expressões regulares, também chamadas de *regex*, compõem uma ferramenta extremamente útil no arsenal de todo programador. Usando uma curta sequência de símbolos, podemos definir padrões complexos para a realização de buscas e identificação de cadeias de caracteres em largos corpos de texto. Mas você sabia que elas estão fortemente ligadas ao assunto de "o que é **computação**", e ao trabalho de pesquisa de grandes nomes como Alan Turing e Noam Chomsky?

Neste post, veremos de onde vêm as expressões regulares e como implementá-las a partir de suas bases teóricas, explorando um pouco da área de **teoria da computação**. O conteúdo aqui apresentado é baseado no livro [*Introduction to the Theory of Computation*](https://www.amazon.com/Introduction-Theory-Computation-Michael-Sipser/dp/113318779X), de Michael Sipser, cuja leitura recomendo veementemente caso tenha se interessado no tópico; e também baseado nas aulas da disciplina [Autômatos, Computabilidade e Complexidade (MAC0414)](https://uspdigital.usp.br/jupiterweb/obterDisciplina?sgldis=mac0414) do IME-USP.

## Modelo computacional

> *"Se você deseja assar uma torta do zero, você precisa antes inventar o universo."* - **Carl Sagan**

Para implementarmos expressões regulares, devemos antes inventar o computador. Antes que você se desespere, não vamos realmente discutir todas as peças necessárias para a criação de um computador moderno, como o processador, a placa gráfica, as interfaces de entrada e saída...

Em vez disso, temos o interesse em modelar apenas o funcionamento básico de tais instrumentos, definindo quais são as operações fundamentais que um computador pode realizar em sua estrutura mais simples. Chamamos de *modelo computacional* toda maneira idealizada de representar computadores dentro da matemática, tendo como exemplo a famosa *máquina de Turing*, citada mais para frente.

### Autômato finito

Um dos modelos computacionais mais simples é o **autômato finito**. A estrutura de um autômato é baseada em três componentes:

- Uma *fita*, que serve como a entrada de dados para este computador simples;
- Um conjunto de *estados*, que representa etapas de computação;
- Uma *função de transição*, que descreve como o computador deve alterar estados com base nos valores lidos da fita.

Nosso computador conceitual deverá percorrer os itens desta fita sequencialmente, lendo cada um deles e realizando uma ação com base em suas regras predeterminadas.

Imagine que estamos modelando o sistema computacional de uma porta eletrônica, capaz de fechar quando apertamos um botão e abrir quando apertamos outro. Dentro da ideia de autômato finito descrita acima, podemos modelar este sistema da seguinte forma:

- A fita é composta por uma sequência de comandos de **abre** ou **fecha**.
- Os estados da porta devem ser **aberto** ou **fechado**.
- As transições entre estados são definidas da seguinte forma: um comando de **abre** deve causar a porta a ficar **aberta**, um comando de **fecha** deve transitar ao estado de **fechado**.

Vamos ver a representação de *diagrama de estados* do sistema acima. Para facilitar o desenho, representamos os estados de **aberto** e **fechado** pelas letras **A** e **F** respectivamente.

<div style="overflow-x: auto; overflow-y: hidden;">
<svg width="350" height="100" style="display: block; margin: auto;" version="1.1" xmlns="http://www.w3.org/2000/svg">
	<ellipse  stroke-width="1" fill="none" cx="119.5" cy="52.5" rx="30" ry="30"/>
	<text x="112.5" y="58.5" font-family="Times New Roman" font-size="20">A</text>
	<ellipse  stroke-width="1" fill="none" cx="226.5" cy="52.5" rx="30" ry="30"/>
	<text x="220.5" y="58.5" font-family="Times New Roman" font-size="20">F</text>
	<path  stroke-width="1" fill="none" d="M 144.662,36.507 A 77.724,77.724 0 0 1 201.338,36.507"/>
	<polygon  stroke-width="1" points="201.338,36.507 195.712,28.934 192.066,38.246"/>
	<text x="151.5" y="22.5" font-family="Times New Roman" font-size="20">fecha</text>
	<path  stroke-width="1" fill="none" d="M 198.688,63.492 A 108.752,108.752 0 0 1 147.312,63.492"/>
	<polygon  stroke-width="1" points="147.312,63.492 153.904,70.24 156.266,60.523"/>
	<text x="155.5" y="87.5" font-family="Times New Roman" font-size="20">abre</text>
	<path  stroke-width="1" fill="none" d="M 92.703,65.725 A 22.5,22.5 0 1 1 92.703,39.275"/>
	<text x="12.5" y="58.5" font-family="Times New Roman" font-size="20">abre</text>
	<polygon  stroke-width="1" points="92.703,39.275 89.17,30.527 83.292,38.618"/>
	<path  stroke-width="1" fill="none" d="M 253.297,39.275 A 22.5,22.5 0 1 1 253.297,65.725"/>
	<text x="299.5" y="58.5" font-family="Times New Roman" font-size="20">fecha</text>
	<polygon  stroke-width="1" points="253.297,65.725 256.83,74.473 262.708,66.382"/>
</svg>
</div>

Isto é *quase* um autômato finito, como logo veremos, mas ilustra bem a ideia geral. Neste tipo de diagrama, representamos os estados do autômato usando círculos e as transições com setas. Um computador com esta definição que comece no estado **A** e processe uma fita contendo os comandos `[fecha, abre, abre]`, nesta ordem, deverá transitar entre os estados **F**, **A**, e **A** (note que uma porta aberta permanece igual quando recebe o comando `abre`).

Nos resta adicionar dois conceitos para completar a definição de um autômato finito: em um autômato deve haver um estado de início, indicado por uma seta cuja origem não seja outro estado; e os estados de terminação, indicados por um círculo adicional interno. O estado de início é auto-explicativo, já os estados de terminação são um pouco mais complicados.

O modelo computacional que definimos deve ser capaz de *aceitar* ou *rejeitar* as fitas que recebe como entrada; se seu último estado antes do fim da fita for um estado de terminação, a fita é aceita, caso contrário é rejeitada. Dizemos que a **linguagem** de um autômato é o conjunto de todas as fitas que aceita. Isto não é muito importante para o exemplo dado acima, afinal, toda sequência de apertos de botão para controlar uma porta é válida; no entanto, esta noção será fundamental para entender expressões regulares.

O autômato finito abaixo é uma versão completa do sistema controlador de uma porta eletrônica. Ele aceita todas as possíveis entradas que recebe, como `[abre]`, `[fecha]`, `[abre, fecha]`, `[fecha, abre]`, etc. Desta forma, sua linguagem é o conjunto de todas as suas fitas.

<div style="overflow-x: auto; overflow-y: hidden;">
<svg width="350" height="120" style="display: block; margin: auto;" version="1.1" xmlns="http://www.w3.org/2000/svg">
	<ellipse  stroke-width="1" fill="none" cx="119.5" cy="52.5" rx="30" ry="30"/>
	<text x="112.5" y="58.5" font-family="Times New Roman" font-size="20">A</text>
	<ellipse  stroke-width="1" fill="none" cx="119.5" cy="52.5" rx="24" ry="24"/>
	<ellipse  stroke-width="1" fill="none" cx="226.5" cy="52.5" rx="30" ry="30"/>
	<text x="220.5" y="58.5" font-family="Times New Roman" font-size="20">F</text>
	<ellipse  stroke-width="1" fill="none" cx="226.5" cy="52.5" rx="24" ry="24"/>
	<path  stroke-width="1" fill="none" d="M 144.662,36.507 A 77.724,77.724 0 0 1 201.338,36.507"/>
	<polygon  stroke-width="1" points="201.338,36.507 195.712,28.934 192.066,38.246"/>
	<text x="151.5" y="22.5" font-family="Times New Roman" font-size="20">fecha</text>
	<path  stroke-width="1" fill="none" d="M 198.688,63.492 A 108.752,108.752 0 0 1 147.312,63.492"/>
	<polygon  stroke-width="1" points="147.312,63.492 153.904,70.24 156.266,60.523"/>
	<text x="155.5" y="87.5" font-family="Times New Roman" font-size="20">abre</text>
	<path  stroke-width="1" fill="none" d="M 92.703,65.725 A 22.5,22.5 0 1 1 92.703,39.275"/>
	<text x="12.5" y="58.5" font-family="Times New Roman" font-size="20">abre</text>
	<polygon  stroke-width="1" points="92.703,39.275 89.17,30.527 83.292,38.618"/>
	<path  stroke-width="1" fill="none" d="M 253.297,39.275 A 22.5,22.5 0 1 1 253.297,65.725"/>
	<text x="299.5" y="58.5" font-family="Times New Roman" font-size="20">fecha</text>
	<polygon  stroke-width="1" points="253.297,65.725 256.83,74.473 262.708,66.382"/>
	<polygon  stroke-width="1" points="119.5,117.5 119.5,82.5"/>
	<polygon  stroke-width="1" points="119.5,82.5 114.5,90.5 124.5,90.5"/>
</svg>
</div>

Agora, temos bagagem suficiente para definir *o que é um autômato determinístico* de maneira formal, isto é, dentro da matemática. Não se desespere, pois tentarei detalhar o que cada um dos símbolos esquisitos utilizados significam. Definições formais são úteis para especificar conceitos de forma inequívoca, evitando que pessoas diferentes tenham interpretações distintas de um mesmo assunto. Também aproveitaremos esta definição como base do programa que iremos escrever em breve...

Um **autômato finito** é composto por $(Q, \Sigma, \delta, q_0, F)$, onde:

1. $Q$ é o conjunto de **estados**,

2. $\Sigma$, pronunciado "sigma", é o conjunto do **alfabeto**,

3. $\delta : Q \times \Sigma \to Q$, com seu símbolo principal "delta", é a **função de transição**,

4. $q_0 \in Q$ é o **estado inicial**,

5. $F \subseteq Q$ é o **conjunto de estados de terminação**.

Vamos discutir estes itens um por um. Já sabemos que o conjunto de estados representa as etapas de computação nas quais um computador pode estar, mas o que é este tal de *alfabeto*?

O **alfabeto** de um autômato é o conjunto dos símbolos que cada pedaço de sua fita pode conter. No exemplo de sistema de porta eletrônica, os símbolos possíveis de uma fita eram `abre` e `fecha`, mas poderiam ser outros, como as letras `a` e `b`.

A **função de transição**, quando representada matematicamente, é uma função que recebe um estado e um símbolo do alfabeto e te devolve outro estado. Lembre do diagrama de estados do exemplo: nele, cada seta de transição começava em um estado, tinha um símbolo associado a ela, e terminava em outro estado. Matematicamente, podemos retratar estas setas como:

- $\delta(A, \text{abre}) = A$
- $\delta(A, \text{fecha}) = F$
- $\delta(F, \text{abre}) = A$
- $\delta(F, \text{fecha}) = F$

O **estado inicial** é nada mais que um dos estados do conjunto de estados, como indicado pela relação de pertencimento $q_0 \in Q$. De forma semelhante, o **conjunto de estados de terminação** é um *subconjunto* de $Q$, ou seja, é um outro conjunto de estados cujos elementos todos também devem estar em $Q$.

Tendo uma especificação de um autômato finito mais bem definida, podemos traduzí-la em código de programação.

#### Código

Todo o código deste post estará escrito na linguagem Haskell. Não se preocupe se nunca tiver tido contato com ela, tentarei explicar sua sintaxe e seu funcionamento de forma breve. Você pode testar o código deste post sem baixar o compilador da linguagem pelo [Haskell Playground](https://play.haskell.org/).

Podemos representar nosso autômato finito da seguinte forma. Note que o nome do tipo criado é `DFA`, que vem do inglês *deterministic finite automaton*. Vale a pena mencionar que até agora estávamos estudando uma versão específica de autômato chamada *determinísica*, mas também existe o autômato finito *não determinístico*.

```haskell
import qualified Data.Set as S

data DFA state symbol = MkDFA
  { transition :: state -> symbol -> state,
    start :: state,
    endings :: S.Set state
  }
```

Acima, criamos um tipo chamado `DFA` que é genérico em cima de outros dois tipos: `state` e `symbol`, que representam estados e símbolos do alfabeto respectivamente. Desta forma, para que um DFA seja criado, precisamos informar qual será o conjunto de estados e o alfabeto usado, como veremos em um exemplo daqui a pouco.

Valores do tipo `DFA` podem ser criados usando a função construtora `MkDFA` (diz-se "*make DFA*"). Como atributos, um `DFA` deve ter:

- Uma função de transição chamada `transition`, que recebe um estado e um símbolo e retorna outro estado. Note que o tipo de funções em Haskell é escrito somente usando setas – o tipo depois da última seta será o retorno da função e todos os outros serão seus parâmetros,
- Um estado de início chamado `start`,
- Um conjunto de estados de terminação chamado `ending`. Todas as funções associadas à manipulação de conjuntos foram importadas no comando `import qualified Data.Set as S`, e são acessadas a partir do nome `S`.


Lembre do exemplo da porta eletrônica. Vamos representá-lo em Haskell!

```haskell
data DoorState = Open | Closed deriving (Eq, Ord, Show)
data DoorSymbol = DoOpen | DoClose
```

Neste bloco, criamos um tipo `DoorState`, que representa os estados nos quais uma porta pode estar e cujos valores podem ser ou `Open` ou `Closed`. Também criamos um tipo `DoorSymbol`, que configura o alfabeto do exemplo e é habitado por `DoOpen` e `DoClose`. O trecho `deriving (Eq, Ord)` permite que valores de `DoorState` possam ser comparados por igualdade, por ordem (algo necessário para que possamos criar conjuntos de `DoorState`), e possam ser transformados em texto.

Definimos o autômato do exemplo da seguinte forma:

```haskell
example1 :: DFA DoorState DoorSymbol
example1 = MkDFA transition start endings
  where
    transition :: DoorState -> DoorSymbol -> DoorState
    transition Open DoOpen = Open
    transition Open DoClose = Closed
    transition Closed DoOpen = Open
    transition Closed DoClose = Closed
    start :: DoorState
    start = Open
    endings :: S.Set DoorState
    endings = S.fromList [Open, Closed]
```

Acima, criamos um valor de `DFA` parametrizado pelos tipos `DoorState` e `DoorSymbol`, usando os membros `transition`, `start`, e `endings` definidos após a cláusula `where`.

A função `transition` é definida usando a técnica de *pattern matching* em cima de seus parâmetros: quando recebe um valor `Open` e um `DoOpen`, retorna `Open`; quando recebe `Open` e `DoClose`, retorna `Closed`; e assim por diante. `start` é nada mais que o estado no qual o sistema começa e `endings` é um conjunto criado a partir da lista `[Open, Closed]`.

## Linguagens regulares

Até este momento, estávamos pensando em autômatos como um modelo abstrato para sistemas concretos; a partir de agora, vamos focar um pouco mais nas abstrações, com os holofotes no conceito de *linguagem* brevemente mencionado.

A linguagem de um modelo computacional baseado em *aceitação* e *rejeição*, como discutido, é o conjunto de todas as entradas que, no final de sua computação, acaba em um estado de aceitação. Por definição, as **linguagens regulares** são *todas* as linguagens que podem ser descritas por autômatos finitos.

Imagine que estamos lidando com uma fita cujo alfabeto contêm somente as letras 'a' e 'b'. O conjunto de todas as fitas possíveis com esta caracterísitica seria:

<!-- <div style="overflow-x: auto; overflow-y: hidden;"> -->
$$\{\text{`` ''}, \text{``a''}, \text{``b''}, \text{``aa''}, \text{``ab''}, \text{``ba''}, \text{``bb''},  \text{``aaa''}, \text{``aab''}...\}$$
<!-- </div> -->

Desta forma, qualquer autômato que tenha ao menos um estado e que todos sejam de aceitação terá este conjunto como linguagem. Mas e se quisermos fazer alguma restrição? Abaixo, vemos o diagrama de estados de um autômato cuja linguagem contêm *apenas* as fitas que tenham um número par de aparições da letra 'a' e qualquer número da letra 'b'.

<div style="overflow-x: auto; overflow-y: hidden;">
<svg width="215" height="150" style="display: block; margin: auto;" version="1.1" xmlns="http://www.w3.org/2000/svg">
	<ellipse  stroke-width="1" fill="none" cx="64.5" cy="45.5" rx="30" ry="30"/>
	<text x="56.5" y="51.5" font-family="Times New Roman" font-size="20">q&#8320;</text>
	<ellipse  stroke-width="1" fill="none" cx="64.5" cy="45.5" rx="24" ry="24"/>
	<ellipse  stroke-width="1" fill="none" cx="182.5" cy="45.5" rx="30" ry="30"/>
	<text x="174.5" y="51.5" font-family="Times New Roman" font-size="20">q&#8321;</text>
	<path  stroke-width="1" fill="none" d="M 91.733,33.135 A 110.786,110.786 0 0 1 155.267,33.135"/>
	<polygon  stroke-width="1" points="155.267,33.135 149.037,26.051 146.17,35.631"/>
	<text x="119.5" y="19.5" font-family="Times New Roman" font-size="20">a</text>
	<path  stroke-width="1" fill="none" d="M 77.725,72.297 A 22.5,22.5 0 1 1 51.275,72.297"/>
	<text x="59.5" y="134.5" font-family="Times New Roman" font-size="20">b</text>
	<polygon  stroke-width="1" points="51.275,72.297 42.527,75.83 50.618,81.708"/>
	<path  stroke-width="1" fill="none" d="M 195.725,72.297 A 22.5,22.5 0 1 1 169.275,72.297"/>
	<text x="177.5" y="134.5" font-family="Times New Roman" font-size="20">b</text>
	<polygon  stroke-width="1" points="169.275,72.297 160.527,75.83 168.618,81.708"/>
	<path  stroke-width="1" fill="none" d="M 154.146,55.125 A 140.25,140.25 0 0 1 92.854,55.125"/>
	<polygon  stroke-width="1" points="92.854,55.125 99.568,61.752 101.753,51.993"/>
	<text x="119.5" y="79.5" font-family="Times New Roman" font-size="20">a</text>
	<polygon  stroke-width="1" points="2.5,45.5 34.5,45.5"/>
	<polygon  stroke-width="1" points="34.5,45.5 26.5,40.5 26.5,50.5"/>
</svg>
</div>

<!-- <div style="overflow-x: auto; overflow-y: hidden;"> -->
$$\{\text{`` ''}, \text{``b''}, \text{``aa''}, \text{``aab''}, \text{``baa''}, \text{``aabb''}, \text{``baab''}, \text{``bbaa''},  \text{``aaaa''}, ...\}$$
<!-- </div> -->

Em código, escrevemos este autômato como:

```haskell
data Q = Q1 | Q2 deriving (Eq, Ord, Show)
data S = A | B

example2 :: DFA Q S
example2 = MkDFA transition start endings
  where
    transition :: Q -> S -> Q
    transition Q1 A = Q2
    transition Q2 A = Q1
    transition q B = q
    start :: Q
    start = Q1
    endings :: S.Set Q
    endings = S.singleton Q1
```

Note que um *singleton* é um conjunto de apenas um elemento. Podemos usar o programa que descreve este autômato para verificar, de forma automática, se uma fita (ou *string*) pertence à sua linguagem; mas a questão é, como fazemos isso?

Vamos precisar escrever uma função que percorre os estados do modelo, alternando-os a partir das regras da função de transição. Em pseudo-código de uma linguagem imperativa tradicional, faríamos algo similar a isto:

<div style="overflow-x: auto; overflow-y: hidden;">
<pre style="margin: 0">
run(transition, start, tape):
  state <- start
  for symbol in tape:
    state <- transition(state, symbol)
  return state
</pre>
</div>

Este padrão é muito comum quando escrevendo programas, e na progrmação funcional o chamamos de **fold** (dobra), já que seu comportamento é de "dobrar" sua entrada aos poucos até terminar com um único valor final. Em Haskell, a assinatura do `fold` dobrando elementos "da esquerda para a direita" é a seguinte:

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
```

`foldl` recebe uma função que acumulará um resultado do tipo genérico `b` transformando-o a partir de cada elemento `a`, um valor inicial do tipo `b`, e uma lista de elementos do tipo `a`, retornando no final o resultado desta acumulação.

A partir desta definição, a função que executa um autômato finito para cada fita diferente pode ser escrita assim:

```haskell
run :: DFA state symbol -> [symbol] -> state
run dfa tape = foldl (transition dfa) (start dfa) tape
```

Os trechos `(transition dfa)` e `(start dfa)` extraem a função de transição e o estado inicial de um autômato, respectivamente. Após o fim da execução, precisamos saber se o estado final é de aceitação para descobrir se uma dada fita é parte da linguagem do autômato.

```haskell
accepts :: Ord state => DFA state symbol -> [symbol] -> Bool
accepts dfa tape = run dfa tape `S.member` endings dfa
```

Alguns detalhes de notação:

- `Ord state =>` indica que os estados devem ser ordenáveis, algo necessário para que possamos realizar uma busca em conjunto,
- funções entre *backticks* (\`) são consideradas *infixas*, isto é, são escritas entre seus parâmetros, de forma que `S.member element set` seja igual a ``element `S.member` set``.

Então, esta função verifica se o estado final é membro do conjunto de estados de terminação.

Para testar as funções que acabamos de definir com alguns exemplos, definimos a função *main* de entrada do programa que imprime alguns resultados:

```haskell
main = do
  print (run example2 [A, A, B, A, B, A]) -- imprime Q1
  print (run example2 [A, A, B, A, B, B]) -- imprime Q2
  print (example2 `accepts` [A, A, B, A, B, A]) -- imprime True
  print (example2 `accepts` [A, A, B, A, B, B]) -- imprime False
```

### Operações regulares

Estamos quase chegando em expressões regulares! Só nos resta entender um último conceito: como *compor* diferentes linguagens.

---

*Você chegou ao fim do rascunho! Volte em breve...*

<!-- ## Expressões regulares -->

<!-- ## Conclusão -->

![*Na-Na-Na-Na-Na... Batman!*](./images/nanana.png "*Na-Na-Na-Na-Na... Batman!*")

<!-- <figure>
<img src="./images/nanana.png" alt="Na-Na-Na-Na-Na... Batman!" class="go-center">
<figcaption aria-hidden="true">
<em>Na-Na-Na-Na-Na... Batman!</em>
</figcaption>
</figure> -->
