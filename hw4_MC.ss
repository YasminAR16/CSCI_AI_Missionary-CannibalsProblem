;Yasmin Alvarado-Rayo
#lang racket
;Missionaries & Cannibals HW 4 Problem

;Three missionaries and three cannibals are on one side of a crocodile infested river,
;along with a boat that can hold one or two people.
;The following finds a way to get everyone to the other side,
;without ever leaving a group of missionaries in one place outnumbered by the cannibals on that side of the river. 
;It uses blind (uninformed) search find a solution to this problem.
;The solution seeking program is able to use either depth-first search (dfs) or breadth-first search (bfs).
;The solution is be implemented in Scheme.
;The implement of both bfs and dfs use the same search engine.
;Hence both bfs and dfs are simply wrapper functions that call the same search function, properly parameterized.

;Representation of a state:
;                            (3 3 #t)
; 1st item in list represents the number of missionaries on the west bank of the river
; 2nd item in list represents the number of cannibals on the west bank of the river
; 3rd number in list represents which bank of the river the boat is on (#t is west and #f is east)

;The following code is divided into 3 parts notated by the commented line of ";------------...".
;The 1st code part are functions unique to the Missionaries & Cannibals Problem.
;The 2nd code part are functions unique to the general search engine.
;The 3rd code part contains the instructions on what to call to run the full program including
;    two examples to run from the start state.


;following functions are unique to the Missionaries & Cannibals Problem
;--------------------------------------------------------------------------------------------

;function that takes a path and returns true if the
;path contains the goal state, else false
;defined goal with 0 missionary and 0 cannibals at the left side and the boat on the right side
(define goal? ;goal function
  (lambda (lst) ;parameters- lst = list given
    ;(display lst)
    (cond
      ((null? lst) #f) ;if lst is empty, return false
      ((equal? '(0 0 #f) lst) #t) ;if (0 0 #f) == lst, return true
      (else #f)))) ;else return false


;fuction that takes the path-container extends the paths
(define path-extender ;path-extender function
  (lambda (path-container extend-method) ;parameters- path-container = list given, extend-method = specifies a bf or df search
    ;extendMethod(path-container[1,], foil(path-container[0], filterNodes(next-paths( (path-container[0])[0]) ) ))
    (extend-method (cdr path-container) (foil (car path-container) (filterNodes (next-paths (caar path-container)))))))


;fuction that takes a list of paths derived from next-paths to filter the legal paths only and returns a list of legal paths
(define filterNodes ;filterNodes function
  (lambda (all-paths) ;parameters- all-paths = list given with nodes represented as (M C B)
    (cond
      ((null? all-paths) '()) ;if lst is empty, return '()
      ((> (caar all-paths) 3) (filterNodes (cdr all-paths))) ;if M > 3, then don't add and filterNodes(all-paths[1,])
      ((< (caar all-paths) 0) (filterNodes (cdr all-paths))) ;if M < 0, then don't add and filterNodes(all-paths[1,])
      ((> (cadar all-paths) 3) (filterNodes (cdr all-paths))) ;if C > 3, then don't add and filterNodes(all-paths[1,])
      ((< (cadar all-paths) 0) (filterNodes (cdr all-paths))) ;if C < 0, then don't add and filterNodes(all-paths[1,])
      ((< (caar all-paths) (cadar all-paths)) (filterNodes (cdr all-paths))) ;if M < C don't add and filterNodes(all-paths[1,])
      (else (cons (car all-paths) (filterNodes (cdr all-paths))))))) ;else all-paths[0] + filterNodes(all-paths[1,])


;fuction that takes a path list and returns a list of all paths that are reachable in one move from given path
(define next-paths ;next-paths function
  (lambda (lst) ;parameters- lst = list given
    (cond
      ;if lst is empty, return '()
      ((null? lst) '())
      ;if #t is in lst,
      ((member? #t lst)
       ;then CreateNode(lst[0]-1, lst[1], #f) + CreateNode(lst[0]-2, lst[1], #f) + CreateNode(lst[0], lst[1]-1, #f)
       ;     + CreateNode(lst[0], lst[1]-2, #f) + CreateNode(lst[0]-1, lst[1]-1, #f)
       (append (append (append (cons (createNode (- (car lst) 1) (cadr lst) #f) 
                                     (list (createNode (- (car lst) 2) (cadr lst) #f)))
                               (list (createNode (car lst) (- (cadr lst) 1) #f)))
                       (list (createNode (car lst) (- (cadr lst) 2) #f)))
               (list (createNode (- (car lst) 1) (- (cadr lst) 1) #f))))
      ;if #f is in lst,
      ((member? #f lst)
       ;then CreateNode(lst[0]+1, lst[1], #t) + CreateNode(lst[0]+2, lst[1], #t) + CreateNode(lst[0], lst[1]+1, #t)
       ;     + CreateNodelst[0], lst[1]+2, (#t) + CreateNode(lst[0]+1, lst[1]+1, #t)
       (append (append (append (cons (createNode (+ (car lst) 1) (cadr lst) #t)
                                     (list (createNode (+ (car lst) 2) (cadr lst) #t)))
                               (list (createNode (car lst) (+ (cadr lst) 1) #t)))
                       (list (createNode (car lst) (+ (cadr lst) 2) #t)))
               (list (createNode (+ (car lst) 1) (+ (cadr lst) 1) #t)))))))


;function that creates a node given 3 things (# of missionaries, # of canabals, #f or #t depending on which side the boat is at)
(define createNode ;createNode function
  (lambda (m c b) ;parameters- m = # of missionaries, c = # of canabals, b = #f or #t depending on which side the boat is at
    (append (list m) (list c) (list b)))) ;(m c b)


;--------------------------------------------------------------------------------------------
;following functions are the search engine of General Search and contains helper functions for it
;--------------------------------------------------------------------------------------------
;generic search function that returns the path and then node count if there exist a path from start state given to goal state given, else displays sorry
(define GeneralSearch ;generalSearch function
  (lambda (path-container visited goal? path-extender cnt extend-method)
    ;parameters- path-container = list of paths to check, visited = nodes it has beenn through,
    ;            path-extender = function to find next paths, cnt = node count, extend-method = specifies a bf or df search
    (display "path container: ")
    (display path-container)
    (newline)
    (display "visited: ")
    (display visited)
    (newline)
    (cond
      ;if path-container == null, then display 'sorry' (no more paths to check)
      ((null? path-container) (display "sorry"))
      ;if (path-container[0])[0].contains(goal state), then displays 'wahoo! path is: (path from start to finish) node count: (cnt)
      ((goal? (caar path-container)) (display "wahoo! path is: ") (display (reverse (car path-container))) (display "node count: ") (display cnt))
      ;if (path-container[0])[0].contains(a visited state), then (GeneralSearch(path-container[1,]) visited goal? path-extender cnt extend-method)
      ((member? (caar path-container) visited) (GeneralSearch (cdr path-container) visited goal? path-extender cnt extend-method))
      ;else (GeneralSearch((path-extender(path-container, extend-method)), (visited.insert((path-container[0])[0])), goal?, path-extender, cnt++, extend-method)
      (else (GeneralSearch (path-extender path-container extend-method) (cons (caar path-container) visited) goal? path-extender (+ cnt 1) extend-method)))))


;join new extended paths to queue for depth first search
(define df-extend-method
  (lambda (queue newPath) ;parameters- queue = list to add to, newPath = list to be added
    (append newPath queue))) ;newPath + queue


;join new extended paths to queue for breadth first search
(define bf-extend-method
  (lambda (queue newPath) ;parameters- queue = list to add to, newPath = list to be added
    (append queue newPath))) ;queue + newPath


;function which returns true if atm appears in lst, else false
(define member? ;member function
  (lambda (atm lst) ;parameters- atm = item looking for, lst = list given
    (cond
      ((null? lst) #f) ;if lst is empty, return true
      ((equal? atm (car lst)) #t) ;if lst[0] == atm, return true
      (else (member? atm (cdr lst)))))) ;else member?(atm, lst[1,])


;returns a list of paths that represents the product of a current path with its legal next paths
(define foil ;foil function
  ;parameters- current-path = list representation of current path, add-nodes = list representation of legal nodes to add to current path
  (lambda (current-path add-nodes)
    (cond
      ;if add-nodes is empty, then return '()
      ((null?  add-nodes) '())
      ;else (add-nodes[0] + current-path) + (foil(current-path, add-nodes[1,]))
      (else (cons (append (list (car add-nodes)) current-path) (foil current-path (cdr add-nodes)))))))


;--------------------------------------------------------------------------------------------
;Code to run the whole program:
;--------------------------------------------------------------------------------------------

;final BFS search run on start state of '(((3 3 #t))) where there hasn't been any visited states and a count of 0
(GeneralSearch '(((3 3 #t))) '() goal? path-extender 0 bf-extend-method)

(newline)

;final DFS search run on start state of '(((3 3 #t))) where there hasn't been any visited states and a count of 0
(GeneralSearch '(((3 3 #t))) '() goal? path-extender 0 df-extend-method)
;--------------------------------------------------------------------------------------------