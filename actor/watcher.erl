-module(watcher).
-export([watch/1, watcher_start/3, start/0]).

%% start watcher then start its sensors
watcher_start(N, End_ID, Sensor_list) ->
  if
    N =< End_ID ->
      {Sensor_PID, _} = spawn_monitor(sensor, measure, [self(), N]),
      watcher_start(N + 1, End_ID, Sensor_list ++ [{N, Sensor_PID}]);
    true ->
      io:fwrite("Watcher starts, initial list of sensors: ~p~n", [Sensor_list]),
      watch(Sensor_list)
  end.

%% watch loop, receive measurements and exit message
%% if sensor exit, watcher will restart the sensor with same ID
watch(Sensor_list) ->
  receive
    {Sensor, Measurement} ->
      io:fwrite("receive measurement ~p from sensor ~p ~n", [Measurement, Sensor]),
      watch(Sensor_list);
    {'DOWN', _, process, _, {Reason, Sensor_ID}} ->
      io:fwrite("Sensor ~p died with reason ~p~n", [Sensor_ID, Reason]),
      Filtered_sensor_list = lists:filter(fun({Sid, _}) -> Sensor_ID /= Sid end, Sensor_list),
      {Sensor_PID, _} = spawn_monitor(sensor, measure, [self(), Sensor_ID]),
      Updated_sensor_list = Filtered_sensor_list ++ [{Sensor_ID, Sensor_PID}],
      io:fwrite("Sensor restarts, updated list: ~p~n", [Updated_sensor_list]),
      watch(Updated_sensor_list)
  end.

%% assign sensor IDs to watcher
%% every watcher watch at most 10 sensors
distribute(Start_ID, End_ID) ->
  if
    Start_ID + 9 =< End_ID ->
      spawn(watcher, watcher_start, [Start_ID, Start_ID + 9, []]),
      distribute(Start_ID + 10, End_ID);
    true ->
      spawn(watcher, watcher_start, [Start_ID, End_ID, []])
  end.

%% sensors distribution entrance function
distribute(Sensor_num) ->
  distribute(0, Sensor_num - 1).

%% read number of sensors from standard input
start() ->
  {ok, [Sensor_num]} = io:fread("Enter number of sensors> ", "~d"),
  distribute(Sensor_num),
  receive
    done -> 
      ok
  end.

