# War Zone Game

This is a Java based Command Line implementation of popular "Risk" game, compatible with the rules and map files and the command-line play of the “Warzone” version of Risk, which can be found at: https://www.warzone.com/.

# Introduction

In this project we are building a computer battle game, ”Risk” based on the functionality of Warzone game where the main goal of the game is to own all the territories of the map. The map is a connected graph with countries as nodes and edges between the nodes represent the adjacency of the countries. The game starts with the startup phase where the number of players are determined, and the territories are randomly allocated among the players. The game is turn based where the players first issue the orders and then they are executed.

# Goal of the Project

## BUILD 1

The main goal of the project is to build a battle game where the player who owns all the territories wins.
Implementation of the Warzone Engine which is initiated by the Game Engine to control game phase.
Implementation of the startup phase where the game begins with the selection of a user-saved map file, loading it as a connected directed graph.
  1. Load the existing map
  2. Edit the map
  3. Create a new map
  4. Validate
The territories are allocated to the players randomly.
In the Startup phase of the game, the players can 
  1. Issue orders
  2. Execute orders

## BUILD 2

<img width="643" alt="image" src="https://github.com/raghavmanchanda2/Warzone-Game/assets/53889865/c8652184-eba1-4340-991b-6fb31d639166">


# Tools Used

1. GitHub
   1. Version control and collaboration
   2. Repository setup and cloning
   3. Collaboration with team members
   4. Git commands for branching, committing, and pushing
3. Eclipse IDE
   1. Integrated Development Environment
   2. Project setup and coding
   3. Running JUnit tests
   4. Debugging and Git integration
5. Javadoc
   1. Code documentation with comments
   2. Generating Javadoc in Eclipse
   3. Clear code documentation for classes and methods
7. JUnit
   1. Unit testing framework
   2. Writing test classes with annotations
   3. Running tests and using assertions
  
# Coding Conventions Followed

## Naming conventions
1. class names in CamelCase that starts with a capital letter
2. data members start with d_
3. method parameters start with p_
4. local variables start with l_
5. global variables in capital letters
6. static members start with a capital letter, non-static members start with a lower case letter
   
## Code layout 
1. consistent layout throughout code (use an IDE auto-formatter)
2. Commenting convention
3. javadoc comments for every class and method
4. long methods (more than 10 lines) are documented with comments for procedural steps
5. no commented-out code
   
## Project Structure
1. one folder for every module in the high-level design
2. tests are in a separate folder that has the exact same structure as the code folder
3. 1-1 relationship between tested classes and test classes
