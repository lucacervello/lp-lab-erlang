# Programming Languages exercise

## Base

### Exercise 1: Sequential Erlang.
    Define the following functions in Erlang:
    
    is_palindrome: string → bool that checks if the string given as input is palindrome, a string is palindrome when the represented sentence can be read the same way in either directions in spite of spaces, punctual and letter cases, e.g., detartrated, "Do geese see God?", "Rise to vote, sir.", ...;
    is_an_anagram : string → string list → boolean that given a dictionary of strings, checks if the input string is an anagram of one or more of the strings in the dictionary;
    factors: int → int list that given a number calculates all its prime factors;
    is_proper: int → boolean that given a number calculates if it is a perfect number or not, where a perfect number is a positive integer equal to the sum of its proper positive divisors (excluding itself), e.g., 6 is a perfect number since 1, 2 and 3 are the proper divisors of 6 and 6 is equal to 1+2+3;
    
### Exercise 2: Evaluating Expressions.
    This exercise asks you to build a collection of functions that manipulate arithmetical expressions. Start with an expression such as the following: ((2+3)-4), 4 and ~((2*3)+(3*4)) which is fully bracketed and where you use a tilde (~) for unary minus.

    First, write a parser for these, turning them into Erlang representations, such as the following: {minus, {plus, {num, 2}, {num,3}}, {num, 4}} which represents ((2+3)-4). We call these exp``s. Now, write an evaluator, which takes an ``exp and returns its value.

    You can also extend the collection of expressions to add conditionals: if ((2+3)-4) then 4 else ~((2*3)+(3*4)) where the value returned is the “then” value if the “if” expression evaluates to 0, and it is the “else” value otherwise.

### Exercise 3: The Process Ring.
    Write a program that will create N processes connected in a ring. Once started, these processes will send M number of messages around the ring and then terminate gracefully when they receive a quit message. You can start the ring with the call ring:start(M, N, Message).

    There are two basic strategies to tackling this exercise. The first one is to have a central process that sets up the ring and initiates sending the message. The second strategy consists of the new process spawning the next process in the ring. With this strategy, you have to find a method to connect the first process to the second process.

    Try to solve the exercise in both manners. Note, when writing your program, make sure your code has many io:format statements in every loop iteration; this will give you a complete overview of what is happening (or not happening) and should help you solve the exercise.

### Exercise 4: Ping Pong Server.
    Write a server that will wait in a receive loop until a message is sent to it. Depending on the message, it should either print its contents and loop again, or terminate. You want to hide the fact that you are dealing with a process, and access its services through a functional interface, which you can call from the shell.

    This functional interface, exported in the echo.erl module, will spawn the process and send messages to it. The function interfaces are shown here:

    echo:start() ⇒ ok
    echo:print(Term) ⇒ ok
    echo:stop() ⇒ ok
    Hint: use the register/2 built-in function, and test your echo server using the process manager.

    Warning: use an internal message protocol to avoid stopping the process when you, for example, call the function echo:print(stop).

    Then write a client to be connected to such a server and link these two processes each other. When the stop function is called, instead of sending the stop message, make the first process terminate abnormally. This should result in the EXIT signal propagating to the other process, causing it to terminate as well.

### Exercise 5: Counting Calls.
    Write a module counting which provides the functionality for interacting with a server that counts how many times its services has been requested.

    It has to implement several services dummy1, ... dummyn (doesn't matter what they do or their real interface) and a service tot that returns a list of records indexed on each service (tot included) containing also how many times such a service has been requested. Test it from the shell.
    
## Advanced

### Exercise 1: List Comprehensions.
    Write the following functions by using list comprehensions:

    squared_int that removes all non-integers from a polymorphic list and returns the resulting list of integers squared, e.g., squared_int([1, hello, 100, boo, “boo”, 9]) should return [1, 10000, 81].
    intersect that given two lists, returns a new list that is the intersection of the two lists (e.g., intersect([1,2,3,4,5], [4,5,6,7,8]) should return [4,5]).
    symmetric_difference that given two lists, returns a new list that is the symmetric difference of the two lists. For example symmetric_difference([1,2,3,4,5], [4,5,6,7,8]) should return [1,2,3,6,7,8].
    
### Exercise 2: Distributed Associative Store.
    Design a distributed version of an associative store in which values are associated with tags. It is possible to store a tag/value pair, and to look up the value(s) associated with a tag. One example for this is an address book for email, in which email addresses (values) are associated with nicknames (tags).

    Replicate the store across two nodes on the same host, send lookups to one of the nodes (chosen either at random or alternately), and send updates to both.

    Reimplement your system with the store nodes on other hosts (from each other and from the frontend). What do you have to be careful about when you do this?

    How could you reimplement the system to include three or four store nodes?

    Design a system to test your answer to this exercise. This should generate random store and lookup requests.

### Exercise 3: Master and Slaves.
    This problem illustrates a situation where we have a process (the master) which supervises other processes (the slaves). In a real example the slave could, for example, be controlling different hardware units. The master's job is to ensure that all the slave processes are alive. If a slave crashes (maybe because of a software fault), the master is to recreate the failed slave.

    Write a module ms with the following interface:

    start(N) which starts the master and tell it to start N slave processes and registers the master as the registered process master.
    to_slave(Message, N) which sends a message to the master and tells it to relay the message to slave N; the slave should exit (and be restarted by the master) if the message is die.
    the master should detect the fact that a slave process dies, restart it and print a message that it has done so.
    The slave should print all messages it receives except the message die
    Hints:

    the master should trap exit messages and create links to all the slave processes.
    the master should keep a list of pids of the slave processes and their associated numbers.
    Example:

    > ms:start(4).

    => true

    > ms:to_slave(hello, 2).

    => {hello,2}

    Slave 2 got message hello

    > ms:to_slave(die, 3).

    => {die,3}

    master restarting dead slave3

### Exercise 4: Distributed Reverse String.
    
    To reverse the order of the characters in a string is quite trivial but when the length of the string grows trivial could means extremely slow. To decompose the input string in shorter strings and to join back the inverted strings in order to keep the algorithm working always on a short input is a good idea to fast the process.

    Write a master-slave (see the previous exercise) distributed system where:
    
    a server process (MasterProcess) provides a service called long_reverse_string() that given a very large string (≥ 1000 characters) returns the reversed string;
    the service long_reversed_string() decomposes the input w into 10 strings w⁰, ..., w⁹ with the following lengths (# represents the operator length of a string, and n=#w%10): #w⁰=...=#wⁿ=#w/10+1 e #wⁿ⁺¹=...=#w⁹=#w/10 and forwards to 10 distinct actors (SlaveProcess) to reverse the 10 substrings w⁰, ..., w⁹ (service reverse_string()) and joins the 10 results.
    the client process (ClientProcess) just ask for the service on a given string.
    When done with the exercise try to relax the constraint on the number of substrings from ten to a generic M passed as an input to the long_reversed_string service.

### Exercise 5: IRC lite P2P.
    Consider the IRC lite application shown in the lectures and re-engineer it to support a more peer-to-peer architecture where each client is connected via a socket to all the other clients that join to the same IRC channel.

    Note that also joining/unjoining could be dealt in a peer-to-peer fashion by sending the message (both joining or unjoining) to a client which propagate the action to the other clients.
