% ---------- Wumpus World ------------
% Reference:
% https://www.javatpoint.com/the-wumpus-world-in-artificial-intelligence
% https://github.com/krishnangovindraj/wumpusagent
% 
% map: 4 * 4
% Types Cells: Wumpus, Pit, Gold, Agent
% Sensors: Stench, Breeze, Glitter, Scream
% Accions: Up, Down, Right, Left, Shoot, Grab
% Display map.

% Rewards
% +1000 reward points if the agent comes out of the cave with the gold.
% -1000 points penalty for being eaten by the Wumpus or falling into the pit.
% -1 for each action, and -10 for using an arrow.
% The game ends if either agent dies or came out of the cave.

% Define the predicates
:- dynamic
  	agent_position/2,
    wumpus_position/2,
    pit_position/2,
    gold_position/2,
    score/1,
    gold/1,
    agent_health/1,
    agent_in_cave/1,
    agent_arrows/1,
	map/1.

%wumpus_world_states([wumpus_position(2,2),pit_position(3,3),gold_position(2,3)]).

% ------- Help Manual --------
help :-
    format('Wumpus World Manual\n'),
    format('Write start to init the game\n'),
    format('Actions available to the user: Up, Down, Right, Left, Shoot, Grab, Climb\n'),
    format('Methods that you can use.\n'),
	format('display_map\n').

% ------- Start the Game ------

start :-
    format('Welcome to Wumpus World~n'),
    create_matrix(4,4,M),
    replace_row_col(M,1,1,x,NewMatrix),
    assert(map(NewMatrix)),
    display_map(NewMatrix),
    init,
    game.

init :-
    init_agent,
    init_states_wumpus,
    init_sensors.

init_sensors :-
    stench(Stench),
    breeze(Breeze),
    glitter(Glitter),
    format('Sensors: [Stench(~p),Breeze(~p),Glitter(~p),Scream(no)]\n', [Stench,Breeze,Glitter]).

init_states_wumpus :-
    assert(wumpus_position(2,2)),
    assert(pit_position(3,3)),
    assert(gold_position(2,3)).

init_agent :-
    assert(agent_position(1,1)),
    assert(agent_health(alive)),
    assert(agent_in_cave(yes)),
    assert(agent_arrows(3)),
    assert(score(0)),
	assert(gold(0)).

game :-
    display_score,
    check_can_execute_action(Answer),
    Answer == yes, !,
    menu.
	
menu :-
    format('Actions available to the user:
            1.Up ⇡
            2.Down ⇣ 
            3.Right ⇢
            4.Left ⇠
            5.Shoot
            6.Grab
            7.Climb'),
    write('Selecciona una opcion\n'),
    read(Opcion),
    execute_accion(Opcion).

execute_accion(Opcion) :-
    Opcion == 1, up, game;
    Opcion == 2, down, game;
    Opcion == 3, right, game;
    Opcion == 4, left, game;
    Opcion == 5, shoot, game;
    Opcion == 6, grab, game;
    Opcion == 7, climb, game;
	Opcion == 0, true.
    

% ------- Move of Agent --------
up :- 
    decrement_action_score,
    agent_position(X,Y),
    X1 is X - 1,
    map(M),
    replace_row_col(M,X,Y,+,M1),
    replace_row_col(M1,X1,Y,x,NewMatrix),
	display_map(NewMatrix),
    format('PosAgent: ~p\n',[agent_position(X1,Y)]),
    % Update Position of Agent and Map
    retractall(agent_position(_,_)),
	assert(agent_position(X1,Y)),
    retractall(map(_)),
    assert(map(NewMatrix)),
    update_agent_health,
	stench(Stench),
    breeze(Breeze),
    glitter(Glitter),
    format('Sensors: [Stench(~p),Breeze(~p),Glitter(~p),Scream(no)]\n', [Stench,Breeze,Glitter]).

down :- 
    decrement_action_score,
    agent_position(X,Y),
    X1 is X + 1,
    map(M),
    replace_row_col(M,X,Y,+,M1),
    replace_row_col(M1,X1,Y,x,NewMatrix),
	display_map(NewMatrix),
	format('Accion: ~p\n',[agent_position(X1,Y)]),
    % Update Position of Agent and Map
    retractall(agent_position(_,_)),
	assert(agent_position(X1,Y)),
    retractall(map(_)),
    assert(map(NewMatrix)),
    update_agent_health,
    stench(Stench),
    breeze(Breeze),
    glitter(Glitter),
    format('Sensors: [Stench(~p),Breeze(~p),Glitter(~p),Scream(no)]\n', [Stench,Breeze,Glitter]).
right :- 
    decrement_action_score,
    agent_position(X,Y),
    Y1 is Y + 1,
    map(M),
    replace_row_col(M,X,Y,+,M1),
    replace_row_col(M1,X,Y1,x,NewMatrix),
	display_map(NewMatrix),
	format('Accion: ~p\n',[agent_position(X,Y1)]),
    % Update Position of Agent and Map
    retractall(agent_position(_,_)),
	assert(agent_position(X,Y1)),
    retractall(map(_)),
    assert(map(NewMatrix)),
    update_agent_health,
    stench(Stench),
    breeze(Breeze),
    glitter(Glitter),
    format('Sensors: [Stench(~p),Breeze(~p),Glitter(~p),Scream(no)]\n', [Stench,Breeze,Glitter]).

left :- 
    decrement_action_score,
    agent_position(X,Y),
    Y1 is Y - 1,
    map(M),
    replace_row_col(M,X,Y,+,M1),
    replace_row_col(M1,X,Y1,x,NewMatrix),
	display_map(NewMatrix),
	format('Accion: ~p\n',[agent_position(X,Y1)]),
    % Update Position of Agent and Map
    retractall(agent_position(_,_)),
	assert(agent_position(X,Y1)),
    retractall(map(_)),
    assert(map(NewMatrix)),
    update_agent_health,
    stench(Stench),
    breeze(Breeze),
    glitter(Glitter),
    format('Sensors: [Stench(~p),Breeze(~p),Glitter(~p),Scream(no)]\n', [Stench,Breeze,Glitter]).

% Accions of Agent
grab :-
    decrement_action_score,
    pickup_gold,
    stench(Stench),
    breeze(Breeze),
    glitter(Glitter),
    format('Sensors: [Stench(~p),Breeze(~p),Glitter(~p),Scream(no)]\n', [Stench,Breeze,Glitter]).

shoot :-
    decrement_shoot_score,
    stench(Stench),
    breeze(Breeze),
    glitter(Glitter),
    shoot_arrow(Scream),
    format('Sensors: [Stench(~p),Breeze(~p),Glitter(~p),Scream(~p)]\n', [Stench,Breeze,Glitter,Scream]).

climb :-
    decrement_action_score,
    can_leave_cave(Answer),
    Answer \== yes, !.

can_leave_cave(yes) :-
    agent_position(1,1), !,
    gold(G),
    score(S),
    S1 is (S + (1000 * G)),
    retract(score(S)),
    assert(score(S1)),
    retract(agent_in_cave(yes)),
  	assert(agent_in_cave(no)),
    final_results.

can_leave_cave(no) :-
    format("You cannot leave the cave from here.~n").

kill_wumpus(X,Y) :-
    retract(wumpus_position(X,Y)).

update_arrows :-
    agent_arrows(Arrows),
    Arrows > 0, !,
    NewArrows is Arrows - 1,
    retract(agent_arrows(Arrows)),
  	assert(agent_arrows(NewArrows)),
    format("You have ~d arrow(s).~n",NewArrows).

shoot_arrow(Scream) :-
	format('Select a direction to shoot:
           1.Up ⇡
           2.Down ⇣ 
           3.Right ⇢
           4.Left ⇠\n'),
	read(ArrowDirection),
    agent_position(X,Y),
	propagate_arrow(X,Y,ArrowDirection,Scream),
    update_arrows.

shoot_arrow(no) :-
    write('No arrows\n').

propagate_arrow(X,Y,1,Scream) :-
    X1 is X - 1,
    propagate_arrow(X1,Y,Scream).

propagate_arrow(X,Y,2,Scream) :-
    X1 is X + 1,
    propagate_arrow(X1,Y,Scream).

propagate_arrow(X,Y,3,Scream) :-
    Y1 is Y + 1,
    propagate_arrow(X,Y1,Scream).

propagate_arrow(X,Y,4,Scream) :-
    Y1 is Y + 1,
    propagate_arrow(X,Y1,Scream).

propagate_arrow(X,Y,yes) :-
    wumpus_position(X,Y), !,
    kill_wumpus(X,Y).

propagate_arrow(_,_,no).
    
    

% Sensors of Agent
stench(yes) :-
    agent_position(X,Y),
    X1 is X + 1,
    X0 is X - 1,
    Y1 is Y + 1,
    Y0 is Y - 1,
  ( wumpus_position(X1,Y) ;
    wumpus_position(X0,Y) ;
    wumpus_position(X,Y1) ;
    wumpus_position(X,Y0) ;
    wumpus_position(X,Y) ),
  !.

stench(no).

breeze(yes) :-
  agent_position(X,Y),
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( pit_position(X1,Y) ;
    pit_position(X0,Y) ;
    pit_position(X,Y1) ;
    pit_position(X,Y0) ;
    pit_position(X,Y)  ),
  !.

breeze(no).

glitter(yes) :-
  agent_position(X,Y),
  gold_position(X,Y),
  !.

glitter(no).

% Check Health of Agent

check_can_execute_action(no) :-
    agent_health(dead), !,
    write('You are dead!\n').

check_can_execute_action(no) :-
    agent_in_cave(no), !,
    write('You have left the cave\n').

check_can_execute_action(yes).

update_agent_health :-
  agent_health(alive),
  agent_position(X,Y),
  wumpus_position(X,Y),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(score(S)),
  S1 is S - 1000,
  assert(score(S1)),
  format("You have died for the Wumpus~n").

update_agent_health :-
  agent_health(alive),
  agent_position(X,Y),
  pit_position(X,Y),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(score(S)),
  S1 is S - 1000,
  assert(score(S1)),
  format("You're stuck in the pit~n").

update_agent_health.

% Scores
decrement_action_score :-
    retract(score(S)),
    S1 is S - 1,
    assert(score(S1)).

decrement_shoot_score :-
    retract(score(S)),
    S1 is S - 10,
    assert(score(S1)).

display_score :-
    score(S),
    format('Score: ~p\n',[S]).

final_results :-
    score(S),
    format('Your is out of cave\n'),
    format('Your Score is: ~p\n',[S]).

% Accions
pickup_gold :-
    agent_position(X,Y),
    gold_position(X,Y), !,
    gold(NumberGold),              
    NumberGold1 is NumberGold + 1,
    retract(gold(NumberGold)),
    assert(gold(NumberGold1)),
    format("You have ~d piece(s) of gold!~n",NumberGold1),
    retract(gold_position(X,Y)).

display_map(Map) :- 
    write_matrix(Map).

% Methods complementary

do_list(N,L) :- do_list1(N, [], L).
do_list1(0, L, L) :- !.
do_list1(N, R, L) :- 
    N > 0,
    N1 is N-1,
    do_list1(N1, [·|R], L).

create_matrix(Rows,Cols,Matrix) :-
	do_list(Cols,List),
    create_matrix1(Rows,[],List,Matrix).
    
    

create_matrix1(0,M,_,M).
create_matrix1(N,R,L,M) :-
    N > 0,
    N1 is N - 1,
    append(R,[L],L1),
    create_matrix1(N1, L1,L, M).

replace_nth(Index,List,Value,NewList) :-
    nth1(Index,List,_,Transfer),
    nth1(Index,NewList,Value,Transfer).

replace_row_col(M,Row,Col,Cell,N) :-
    nth1(Row,M,Old),
    replace_nth(Col,Old,Cell,Upd),
    replace_nth(Row,M,Upd,N).
    
write_matrix([]).
write_matrix([H|T]) :-
    write_row(H),
    write_matrix(T).

write_row([]) :-
    format('|\n').
write_row([H|T]) :-
    format('| ~p ',H),
    write_row(T).