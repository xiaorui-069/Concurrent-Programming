-module(shipping).
-compile(export_all).
-include_lib("./shipping.hrl").

get_ship(Shipping_State, Ship_ID) ->
    SHIPS = Shipping_State#shipping_state.ships,
    S = lists:keyfind(Ship_ID, #ship.id, SHIPS),
    case S of
        false -> throw(my_exception);
        _ -> S
    end .

get_container(Shipping_State, Container_ID) ->
    CONTAINERS = Shipping_State#shipping_state.containers,
    C = lists:keyfind(Container_ID, #container.id, CONTAINERS),
    case C of
        false -> throw(my_exception);
        _ -> C
    end .

get_port(Shipping_State, Port_ID) ->
    PORTS = Shipping_State#shipping_state.ports,
    P = lists:keyfind(Port_ID, #port.id, PORTS),
    case P of
        false -> throw(my_exception);
        _ -> P
    end .

get_occupied_docks(Shipping_State, Port_ID) ->
    findDocks(Shipping_State#shipping_state.ship_locations, Port_ID) .

findDocks([], _Port_ID) -> [];
findDocks([{Port, Dock, _Ship} | T], Port_ID) ->
    case Port of
        Port_ID -> [Dock | findDocks(T, Port_ID)];
        _ -> findDocks(T, Port_ID)
    end.


get_ship_location(Shipping_State, Ship_ID) ->
    findShips(Shipping_State#shipping_state.ship_locations, Ship_ID) .

findShips([], Ship_ID) -> throw(my_exception);
findShips([{Port, Dock, Ship} | T], Ship_ID) ->
    case Ship of
        Ship_ID -> {Port, Dock};
        _ -> findShips(T, Ship_ID)
    end.

get_container_weight(Shipping_State, Container_IDs) ->
    try
        get_total_container_weight(Shipping_State, Container_IDs)
    catch
        throw:my_exception -> throw(my_exception)
    end .

get_total_container_weight(Shipping_State, []) -> 0;
get_total_container_weight(Shipping_State, [Container_ID | T]) -> 
        try
            #container{id = Id, weight = Weight} = get_container(Shipping_State,Container_ID),
            Weight + get_total_container_weight(Shipping_State, T)
        catch
            throw:my_exception -> throw(my_exception)
        end.

get_ship_weight(Shipping_State, Ship_ID) ->
    Map = Shipping_State#shipping_state.ship_inventory,
    case maps:find(Ship_ID, Map) of
        {ok, Value} -> 
            try
                get_total_container_weight(Shipping_State, Value)
            catch
                throw:my_exception -> throw(my_exception)
            end ;
        error -> throw(my_exception)
    end.
    

load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    {Port_ID, _} = get_ship_location(Shipping_State, Ship_ID),
    Map_ship = Shipping_State#shipping_state.ship_inventory,
    Map_port = Shipping_State#shipping_state.port_inventory,
    case {maps:find(Ship_ID, Map_ship), maps:find(Port_ID, Map_port)} of
        {error, _}-> 
            throw(my_exception);
        {_, error}-> 
            throw(my_exception);
        _ -> ok
    end,
    {ok, Ship} = maps:find(Ship_ID, Map_ship),
    {ok, Port} = maps:find(Port_ID, Map_port),
    case {is_sublist(Ship, Container_IDs), is_sublist(Port, Container_IDs)} of
        {true, _} -> throw(my_exception);
        {_, false} -> throw(my_exception);
        _ -> ok
    end,
    #ship{container_cap = Capacity} = get_ship(Shipping_State, Ship_ID),
    New_ship = lists:append(Ship, Container_IDs),
    New_port = [C || C <- Port, not lists:member(C, Container_IDs)],
    if 
        length(New_ship) > Capacity -> throw(my_exception);
        true -> ok
    end,
    New_map_ship = maps:put(Ship_ID, New_ship, Map_ship),
    New_map_port = maps:put(Port_ID, New_port, Map_port),
    Shipping_State#shipping_state{ship_inventory = New_map_ship, port_inventory = New_map_port}.

unload_ship_all(Shipping_State, Ship_ID) ->
    {Port_ID, _} = get_ship_location(Shipping_State, Ship_ID),
    Map_ship = Shipping_State#shipping_state.ship_inventory,
    Map_port = Shipping_State#shipping_state.port_inventory,
    case {maps:find(Ship_ID, Map_ship), maps:find(Port_ID, Map_port)} of
        {error, _}-> 
            throw(my_exception);
        {_, error}-> 
            throw(my_exception);
        _ -> ok
    end,
    {ok, Ship} = maps:find(Ship_ID, Map_ship),
    {ok, Port} = maps:find(Port_ID, Map_port),
    #port{container_cap = Capacity} = get_port(Shipping_State, Port_ID),
    New_ship = [],
    New_port = Port ++ Ship,
    if 
        length(New_port) > Capacity -> throw(my_exception);
        true -> ok
    end,
    New_map_ship = maps:put(Ship_ID, New_ship, Map_ship),
    New_map_port = maps:put(Port_ID, New_port, Map_port),
    Shipping_State#shipping_state{ship_inventory = New_map_ship, port_inventory = New_map_port}.

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    {Port_ID, _} = get_ship_location(Shipping_State, Ship_ID),
    Map_ship = Shipping_State#shipping_state.ship_inventory,
    Map_port = Shipping_State#shipping_state.port_inventory,
    case {maps:find(Ship_ID, Map_ship), maps:find(Port_ID, Map_port)} of
        {error, _}-> 
            throw(my_exception);
        {_, error}-> 
            throw(my_exception);
        _ -> ok
    end,
    {ok, Ship} = maps:find(Ship_ID, Map_ship),
    {ok, Port} = maps:find(Port_ID, Map_port),
    case {is_sublist(Ship, Container_IDs), is_sublist(Port, Container_IDs)} of
        {false, _} -> throw(my_exception);
        {_, true} -> throw(my_exception);
        _ -> ok
    end,
    #port{container_cap = Capacity} = get_port(Shipping_State, Port_ID),
    New_ship = [C || C <- Ship, not lists:member(C, Container_IDs)],
    New_port = lists:append(Port, Container_IDs),
    if 
        length(New_port) > Capacity -> throw(my_exception);
        true -> ok
    end,
    New_map_ship = maps:put(Ship_ID, New_ship, Map_ship),
    New_map_port = maps:put(Port_ID, New_port, Map_port),
    Shipping_State#shipping_state{ship_inventory = New_map_ship, port_inventory = New_map_port}.

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    {Port_ID_before, _} = get_ship_location(Shipping_State, Ship_ID),
    case Port_ID_before =:= Port_ID of
        true -> throw(my_exception);
        false -> ok
    end,
    case has_match(Shipping_State#shipping_state.ship_locations, {Port_ID, Dock}) of
        true -> throw(my_exception);
        false -> ok
    end,
    New_ship_locations = [{P, D, S} || {P, D, S} <- Shipping_State#shipping_state.ship_locations, S =/= Ship_ID] ++ [{Port_ID, Dock, Ship_ID}],
    Shipping_State#shipping_state{ship_locations = New_ship_locations}.


has_match([], _) -> false;
has_match([{P, D, _} | _], {P, D}) -> true;
has_match([_ | T], {P, D}) -> has_match(T, {P, D}).


%% Determines whether all of the elements of Sub_List are also elements of Target_List
%% @returns true is all elements of Sub_List are members of Target_List; false otherwise
is_sublist(Target_List, Sub_List) ->
    lists:all(fun (Elem) -> lists:member(Elem, Target_List) end, Sub_List).




%% Prints out the current shipping state in a more friendly format
print_state(Shipping_State) ->
    io:format("--Ships--~n"),
    _ = print_ships(Shipping_State#shipping_state.ships, Shipping_State#shipping_state.ship_locations, Shipping_State#shipping_state.ship_inventory, Shipping_State#shipping_state.ports),
    io:format("--Ports--~n"),
    _ = print_ports(Shipping_State#shipping_state.ports, Shipping_State#shipping_state.port_inventory).


%% helper function for print_ships
get_port_helper([], _Port_ID) -> error;
get_port_helper([ Port = #port{id = Port_ID} | _ ], Port_ID) -> Port;
get_port_helper( [_ | Other_Ports ], Port_ID) -> get_port_helper(Other_Ports, Port_ID).


print_ships(Ships, Locations, Inventory, Ports) ->
    case Ships of
        [] ->
            ok;
        [Ship | Other_Ships] ->
            {Port_ID, Dock_ID, _} = lists:keyfind(Ship#ship.id, 3, Locations),
            Port = get_port_helper(Ports, Port_ID),
            {ok, Ship_Inventory} = maps:find(Ship#ship.id, Inventory),
            io:format("Name: ~s(#~w)    Location: Port ~s, Dock ~s    Inventory: ~w~n", [Ship#ship.name, Ship#ship.id, Port#port.name, Dock_ID, Ship_Inventory]),
            print_ships(Other_Ships, Locations, Inventory, Ports)
    end.

print_containers(Containers) ->
    io:format("~w~n", [Containers]).

print_ports(Ports, Inventory) ->
    case Ports of
        [] ->
            ok;
        [Port | Other_Ports] ->
            {ok, Port_Inventory} = maps:find(Port#port.id, Inventory),
            io:format("Name: ~s(#~w)    Docks: ~w    Inventory: ~w~n", [Port#port.name, Port#port.id, Port#port.docks, Port_Inventory]),
            print_ports(Other_Ports, Inventory)
    end.
%% This functions sets up an initial state for this shipping simulation. You can add, remove, or modidfy any of this content. This is provided to you to save some time.
%% @returns {ok, shipping_state} where shipping_state is a shipping_state record with all the initial content.

shipco() ->
    Ships = [#ship{id=1,name="Santa Maria",container_cap=20},
              #ship{id=2,name="Nina",container_cap=20},
              #ship{id=3,name="Pinta",container_cap=20},
              #ship{id=4,name="SS Minnow",container_cap=20},
              #ship{id=5,name="Sir Leaks-A-Lot",container_cap=20}
             ],
    Containers = [
                  #container{id=1,weight=200},
                  #container{id=2,weight=215},
                  #container{id=3,weight=131},
                  #container{id=4,weight=62},
                  #container{id=5,weight=112},
                  #container{id=6,weight=217},
                  #container{id=7,weight=61},
                  #container{id=8,weight=99},
                  #container{id=9,weight=82},
                  #container{id=10,weight=185},
                  #container{id=11,weight=282},
                  #container{id=12,weight=312},
                  #container{id=13,weight=283},
                  #container{id=14,weight=331},
                  #container{id=15,weight=136},
                  #container{id=16,weight=200},
                  #container{id=17,weight=215},
                  #container{id=18,weight=131},
                  #container{id=19,weight=62},
                  #container{id=20,weight=112},
                  #container{id=21,weight=217},
                  #container{id=22,weight=61},
                  #container{id=23,weight=99},
                  #container{id=24,weight=82},
                  #container{id=25,weight=185},
                  #container{id=26,weight=282},
                  #container{id=27,weight=312},
                  #container{id=28,weight=283},
                  #container{id=29,weight=331},
                  #container{id=30,weight=136}
                 ],
    Ports = [
             #port{
                id=1,
                name="New York",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=2,
                name="San Francisco",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=3,
                name="Miami",
                docks=['A','B','C','D'],
                container_cap=200
               }
            ],
    %% {port, dock, ship}
    Locations = [
                 {1,'B',1},
                 {1, 'A', 3},
                 {3, 'C', 2},
                 {2, 'D', 4},
                 {2, 'B', 5}
                ],
    Ship_Inventory = #{
      1=>[14,15,9,2,6],
      2=>[1,3,4,13],
      3=>[],
      4=>[2,8,11,7],
      5=>[5,10,12]},
    Port_Inventory = #{
      1=>[16,17,18,19,20],
      2=>[21,22,23,24,25],
      3=>[26,27,28,29,30]
     },
    #shipping_state{ships = Ships, containers = Containers, ports = Ports, ship_locations = Locations, ship_inventory = Ship_Inventory, port_inventory = Port_Inventory}.
