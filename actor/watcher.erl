-module(watcher).
-export([watch/1, watcher_start/3, start/0, main/1]).

watcher_start(N, End_ID, Sensor_list) ->
  if
    N =< End_ID ->
      {Sensor_PID, _} = spawn_monitor(sensor, measure, [self(), N]),
      watcher_start(N + 1, End_ID, Sensor_list ++ [{N, Sensor_PID}]);
    true ->
      watch(Sensor_list)
  end.


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
      io:fwrite("Sensor restart, updated list: ~p~n", [Updated_sensor_list]),
      watch(Updated_sensor_list)
  end.

distribute(Start_ID, End_ID) ->
  if
    Start_ID + 9 =< End_ID ->
      spawn(watcher, watcher_start, [Start_ID, Start_ID + 9, []]),
      distribute(Start_ID + 10, End_ID);
    true ->
      spawn(watcher, watcher_start, [Start_ID, End_ID, []])
  end.

start() ->
  {ok, [Sensor_num]} = io:fread("Enter number of sensors> ", "~d"),
  distribute(0, Sensor_num),
  receive
    done -> 
      ok
  end.

main(_) ->
  start().
