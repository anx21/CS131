CS131
=====

Programming Languages, Fall '14<br/>
Course website: http://www.cs.ucla.edu/classes/spring14/cs131/

####HW1: Grammar Filter (OCaml)
Write a function filter_blind_alleys g that returns a copy of the grammar g with all blind-alley rules removed. This function should preserve the order of rules: that is, all rules that are returned should be in the same order as the rules in g.

####HW2: Grammar Parser (OCaml)
Write a function parse_prefix gram that returns a matcher for the grammar gram. When applied to an acceptor accept and a fragment frag, the matcher must return the first acceptable match of a prefix of frag, by trying the grammar rules in order; this is not necessarily the shortest nor the longest acceptable match. A match is considered to be acceptable if accept succeeds when given a derivation and the suffix fragment that immediately follows the matching prefix. When this happens, the matcher returns whatever the acceptor returned. If no acceptable match is found, the matcher returns None.

####HW3: KenKen Solver (Prolog)
Write a predicate kenken/3 that makes use of finite domain solver and another plain_kenken/3 that does not to solver a KenKen puzzle.

####HW4: Java Shared Memory Performance Races (Java)
Modify a simple, multi-threaded computational benchmark with different performance and reliability tradeoffs under the JMM.

####HW5: Continuation-based Fragment Analyzer (Scheme)
Write a procedure that returns a match for an input fragment and pattern. The procedure makes use of call-with-current-continuation to iterate through all suitable matches.

####HW6: Containerization Support Languages
Determine the feasibility of re-implementing Docker, currently in Go, using Java, Python, and Dart. 

####Project: Twisted Places Proxy Herd
Implement an application server herd using Twisted framework. Servers handles client updates and performs Google Places API calls upon request. Client updates are exchanged between servers using flooding algorithm.
