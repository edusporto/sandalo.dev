---
title: De onde vem o RegEx?
subtitle: Implementando expressões regulares a partir da teoria
author: Eduardo Sandalo Porto
---

<!-- <img src="./img/nanana.png" style="width: 70%; margin: auto; display: block;"> -->
<!-- -->

\EverySelectfont{\color{red}}
\everymath{\color{black}}
\everydisplay{\color{black}}

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

Vamos ver a representação de *diagrama de estados* do sistema acima. Para facilitar o desenho, vamos representar os estados de **aberto** e **fechado** pelas letras **A** e **F** respectivamente.

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

Isto é *quase* um autômato finito, como logo veremos, mas ilustra bem a ideia geral. Neste tipo de diagrama, representamos os estados do autômato usando círculos e as transições com setas. Um computador com esta definição que comece no estado **A** e processe uma fita contendo os comandos `[fecha, abre, abre]`, nesta ordem, deverá transitar entre os estados **F**, **A**, e **A** (note que uma porta aberta permanece igual quando recebe o comando `abre`).

Nos resta adicionar dois conceitos para completar a definição de um autômato finito: em um autômato deve haver um estado de início, indicado por uma seta cuja origem não seja outro estado; e os estados de terminação, indicados por um círculo adicional interno. O estado de início é auto-explicativo, já os estados de terminação são um pouco mais complicados.

O modelo computacional que definimos deve ser capaz de *aceitar* ou *rejeitar* as fitas que recebe como entrada; se seu último estado antes do fim da fita for um estado de terminação, a fita é aceita, caso contrário é rejeitada. Dizemos que a **linguagem** de um autômato é o conjunto de todas as fitas que aceita. Isto não é muito importante para o exemplo dado acima, afinal, toda sequência de abertos de botão para controlar uma porta é válida; no entanto, esta noção será fundamental para entender expressões regulares.

O autômato finito abaixo é uma versão completa do sistema controlador de uma porta eletrônica. Ele aceita todas as possíveis entradas que recebe, como `[abre]`, `[fecha]`, `[abre, fecha]`, `[fecha, abre]`, etc. Desta forma, sua linguagem é o conjunto de todas as suas fitas.

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

Podemos representar nosso autômato finito da seguinte forma. Note que o nome do tipo criado é `DFA`, que vem do ingles *deterministic finite automaton*. Vale a pena mencionar que até agora estávamos estudando uma versão específica de autômato chamada *determinísica*, mas também existe o autômato finito *não determinístico*.

```haskell
data DFA state symbol = MkDFA
  { transition :: state -> symbol -> state,
    start :: state,
    ending :: state -> Bool
  }
```

Acima, criamos um tipo chamado `DFA` que é genérico em cima de outros dois tipos: `state` e `symbol`, que representam estados e símbolos do alfabeto respectivamente. Desta forma, para que um DFA seja criado, precisamos informar qual será o conjunto de estados e o alfabeto usado, como veremos em um exemplo daqui a pouco.

Valores do tipo `DFA` podem ser criados usando a função construtora `MkDFA` (diz-se "*make DFA*"). Como atributos, um `DFA` deve ter:

- Uma função de transição chamada `transition`, que recebe um estado e um símbolo e retorna outro estado. Note que o tipo de funções em Haskell é escrito somente usando setas – o tipo depois da última seta será o retorno da função e todos os outros serão seus parâmetros,
- Um estado de início chamado `start`,
- Uma função que retorna `True` caso um estado seja de terminação e `False` caso contrário. Funções que retornam valores *booleanos* podem ser chamadas de *predicados*, e são equivalentes a conjuntos – se um parâmetro está no conjunto que representa, retornará *verdadeiro* e, caso contrário, *falso*.

Lembre do exemplo da porta eletrônica. Vamos representá-lo em Haskell!

```haskell
data DoorState = Open | Closed
data DoorSymbol = PleaseOpen | PleaseClose

example1 :: DFA DoorState DoorSymbol
example1 = MkDFA transition start ending
  where
    transition Open PleaseOpen = Open
    transition Open PleaseClose = Closed
    transition Closed PleaseOpen = Open
    transition Closed PleaseClose = Closed
    start = Open
    ending state = True
```

---
TODO


```haskell
run :: DFA state symbol -> [symbol] -> state
run dfa = foldl (transition dfa) (start dfa)
```

## Linguagens regulares

Bla bla



## Expressões regulares

## Conclusão

muito legal

![*Na-Na-Na-Na-Na... Batman!*](./img/nanana.png){ class="go-center" }
