# Clodable

Clodable is a Command Line interface game inspired by Kodable ([https://www.kodable.com](https://www.kodable.com/)), a game for young kids to learn how to think about algorithms and programming. The project is built using Haskell, a purely functional programming language.

### Description

The game has a map with the path which means the ball could roll on the path and some of the tiles of the path have special color, like pink, orange, and yellow. The goal of the game is to take the ball to the target using sequence of moves.

A map should look like this - 

```
* * * * * - - - - - - - - - - - - - - - - - - - * * * * * 
* * * * * b - - - - - - - - - - - - - - - - - b * * * * * 
* * * * * - * * * * * * * * * * * * * * * * * - * * * * * 
* * * * * - * * - - - - * * * * * - - - - * * - * * * * * 
* * * * * - * * - y y - * * * * * - y y - * * - * * * * * 
* * * * * - * * - - - - * * * * * - - - - * * - * * * * * 
* * * * * - * * * * * * - - b - - * * * * * * - * * * * *  
* * * * * - * * * * * * - * * * - * * * * * * - * * * * * 
@ - - - - - * * * * * * - * * * - * * * * * * p - - - - t 
* * * * * - * * * * * * - * * * - * * * * * * - * * * * * 
* * * * * - - - - - - - - * * * - - - - - - - - * * * * * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
```

**Meaning of characters - **

+ ‘@’ represents the ball.

+ ‘-’ represents a path block that the ball could roll on.
+ ’*’ represents the grass (obstacles).
+ ‘p’ represents the tile of the path’s block color is the special color : pink.
+ ‘o’ represents the tile of the path’s block color is the special color : orange.
+ ‘y’ represents the tile of the path’s block color is the special color : yellow.
+ ‘b’ represents the bonus (the stars in Kodable).
+ ‘t’ represents the target point.



**Clodable mimicks much of the functionality of the original Kodable game:**

1. The ball will from the start point initially.
2. The ball needs to roll from the start point to the end point. And it should find the best path to get all the three bonus points (the stars).
3. Once it has decided the direction, the ball will stop only when it comes across obstacles or conditional (colored) blocks (if a conditional instruction is given)
4. If the ball comes across a color tile and the next instruction is a conditional instruction with the same color, the ball will turn to the direction specified in the conditional instruction.
5. The player can use functions, and functions should include only three directions and without loop.
6. The player can use loops. A loop should include only two directions and one loop can be used up to 5 iterations.
7. Maps must be rectangular, but they can otherwise be of arbitrary size.
8. Unlike Kodable, there is no restriction on the number of instructions that can be used to solve a maze.





### 1. How to Build

Step 1 - Install _System.Random_ using Cabal or Stack if not already installed

​	`cabal install random`

​	OR

​	`stack install random`

Step 2 - In the project directory, compile the project

​	`ghc --make Main`

Step 3 - Run

​	`./Main`



### 2a. Basic Functionality

After running the program, enter commands listed below to play the game.

**Load a map from a text file**

​	`load "<filename>"`

**Check a loaded map if it is solvable**

​	`check`

**Generate a solvable map**

​	`generate`

**Play game with the loaded or generated map without Function**

​	`play`

**Play game with the loaded or generated map with Function {m1,m2,m3}**

​	`play <m1> <m2> <m3>`

where `<m1>`, `<m2>`, `<m3>` can be directions or conditionals as described in 2b

**Solve the loaded/generated map and display the path collecting all bonuses with minimum number of turns**

​	`solve`

**Take** `<n>`  **hints for the loaded/generated map**

​	`hint <n>`

**Quit Game**

​	`quit`



### 2b. How to Play

After entering the play command, the interface will ask for a sequence of moves. To play, type the moves in the following format "_without any spaces"_.

**Directions -**

​	`Right`

​	`Left`

​	`Up`

​	`Down`

**Conditionals -**

​	`Cond{<ch>}{<dir>}`

where `<ch>` is the character (p,y, or o) where the ball is supposed to turn and `<dir>` is the direction in which the ball turns.

**Loops -**

​	`Loop{<n>}{<m1>,<m2>}`

where `<n>` is an integer between 0 and 5 inclusive, and `<m1>`, `<m2>` are directions or conditionals.

**Function -**

​	`Function`

If playing with function, the Function command can be used to call function {`<m1>`,`<m2>`,`<m3>`}.



### 3. Choice of Data Structures

**Map (or Board)**

The map (referred to as board in code) is stored using `[[Char]]`, a 2D list of `Char`. Combined with a user-defined function `changeElem`, it allows for easy access and update of map positions.

**Moves**

A user-defined data type called Move is used. Parsers have been defined and used to convert input into Move Type. It allows ease of working with moves without having to deal with unreadable code and repeatedly using strings. This also allowed using &#39;Show instance&#39; to easily convert them back to strings for displaying.

```haskell
dataMove =
Dir String |
Cond CharMove |
Loop IntMoveMove |
Func
deriving (Eq,Ord)
instanceShowMovewhere
-- show :: Move -\&gt; String
show (Dir d) = d
show (Cond t d) = "Cond{"++[t]++"}{"++(show d)++"}"
show (Loop c m1 m2) = "Loop{"++(show c)++"}{"++(show m1)++","++(show m2)++"}"
show Func = "Function"
```

**SimpleMoves**

Like a programming language compiler compiles the program into assembly-level language, this project converts all `Move`s to `SimpleMove`s which can either be `Start` or `Stop`. Having to work with such simpler moves, allows to easier implementation of `applyMoves` and `solveBoard`

```haskell
dataSimpleMove =
Start IntInt |
Stop Char
deriving (Show,Eq,Ord)
```

**Commands**

A user-defined data type called `Command` is used. It uses `Parser` just like `Move`s. It allows ease of working with moves without having to deal with unreadable code or repeatedly using strings.

```haskell
dataCommand =
Load String |
Play |
PlayF MoveMoveMove |
Check |
Solve |
Hint Int |
Generate |
Quit
deriving (Show)
```

**Use of functions of type IO**

Only those functions which either interact with the user or work with random number generators are impure functions of type `IO`. Most of the project functionality is implemented using pure functions. Impure functions are only found in Main.hs and Generator.hs. The impure functions of the project are separated from pure functions as much as possible as prescribed here - [https://wiki.haskell.org/Programming\_guidelines#IO](https://wiki.haskell.org/Programming_guidelines#IO)



### 4. Error Handling

The project handles errors at different stages of the program.

**Taking commands -**

When taking commands (load, check etc.), it enforces the following sequence and displays error messages if a command is invalid or if the following sequence is disobeyed -

- A user can `load` a map, `generate` a map, or `quit` at any point in the game.
- A user can only `check` after a map is loaded/generated. (Generated maps are already checked however)
- A user can only `play`/`solve`/`hint` after a map is checked.

**Taking moves -**

When inputting moves during `play`, error is displayed if an invalid move is entered and the user is asked to re-enter the move.

**Moving the ball -**

When a list of valid moves is taken from the user, the ball is moved from its starting position according to the input by the user. If the ball encounters either of the following situations it displays an error message -

- When the ball cannot move in the specified direction
- When the ball does not find the specified Condition character

**Note -** Invalid Moves and Commands are handled using Maybe as recommended in [https://wiki.haskell.org/Programming\_guidelines#Types](https://wiki.haskell.org/Programming_guidelines#Types)

**Ending -**

The game ends with success/failure messages based on whether the ball reached the target and how many bonuses it did collect if it did reach the target.



### 5. Extra Functionality

**Generate a random map**

The `generate` map functionality works in the following steps -

1. Create a blank 20 x 30 map of &#39; \* &#39;
2. Fix the starting position to be (10,0)
3. Take the first move to be 7 steps Right
4. Then randomly select direction, and the number of steps (between 5 and 7) and create a list of such moves
5. Move along those moves and convert &#39; \* &#39; to &#39; - &#39;
6. If the target is not in the rightmost column, back to step 1
7. If the map has any big empty areas, back to step 1
8. Add conditions to wherever the paths diverge
9. Add up to 3 bonuses randomly
10. If the map is not solvable, back to step 1
11. Solvable map ready

Sometimes, it may take up to a minute to generate a map. Usually, it takes less than a second.

**Hint**

This is a much simpler functionality. This uses the already existing `solveBoard` function to get the solution and then display the first `<n>` Directions or Conditionals in the solution. Unlike `solve`, `hint` does not convert Directions and Conditionals into Loops, to avoid giving too much hint at once.
