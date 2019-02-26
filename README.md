# CSCI_AI_Missionary-CannibalsProblem
projects for AI

Three missionaries and three cannibals are on one side of a crocodile infested river,
along with a boat that can hold one or two people.
The following finds a way to get everyone to the other side,
without ever leaving a group of missionaries in one place outnumbered by the cannibals on that side of the river. 
It uses blind (uninformed) search find a solution to this problem.
The solution seeking program is able to use either depth-first search (dfs) or breadth-first search (bfs).
The solution is be implemented in Scheme.
The implement of both bfs and dfs use the same search engine.
Hence both bfs and dfs are simply wrapper functions that call the same search function, properly parameterized.

Representation of a state:
                            (3 3 #t)
 1st item in list represents the number of missionaries on the west bank of the river
 2nd item in list represents the number of cannibals on the west bank of the river
 3rd number in list represents which bank of the river the boat is on (#t is west and #f is east)

The following code is divided into 3 parts notated by the commented line of ";------------...".
The 1st code part are functions unique to the Missionaries & Cannibals Problem.
The 2nd code part are functions unique to the general search engine.
The 3rd code part contains the instructions on what to call to run the full program including
    two examples to run from the start state.
