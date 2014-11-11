-module(watcher).
-export([watch/1, start_sensor/4, start/0]).

watch(Sensor_list) ->
  receive
    {Sensor, Measurement} ->
      io:fwrite("receive measurement ~p from sensor ~p ~n", [Measurement, Sensor]),
      watch(Sensor_list);
    {'EXIT', Sensor_ID, Reason} ->
      io:fwrite("Sensor ~p died with reason ~p~n", [Sensor_ID, Reason]),
      Filtered_sensor_list = lists:filter(fun({Sid, _}) -> Sensor_ID /= Sid end, Sensor_list),
      {Sensor_PID, _} = spawn_monitor(sensor, measure, [self(), Sensor_ID]),
      Updated_sensor_list = Filtered_sensor_list ++ [{Sensor_ID, Sensor_PID}],
      io:fwrite("Sensor restart, updated list: ~p~n", [Updated_sensor_list]),
      watch(Updated_sensor_list)
  end.

start_sensor(Watcher, N, Sensor_num, Sensor_list) ->
  if
    (N == Sensor_num) orelse (N > 0 and (N rem 10 == 0)) ->
      io:fwrite("Watcher start with sensors: ~p~n", [Sensor_list]);
    true ->
      ok
  end,

  if
    N == Sensor_num ->
      ok;
    (N rem 10) == 0 ->
      New_sensor_list = [],
      {New_watcher, _} = spawn_monitor(watcher, watch, [New_sensor_list]),
      {Sensor_PID, _} = spawn_monitor(sensor, measure, [New_watcher, N]),
      start_sensor(New_watcher, N + 1, Sensor_num, New_sensor_list ++ [{N, Sensor_PID}]);
    true ->
      {Sensor_PID, _} = spawn_monitor(sensor, measure, [Watcher, N]),
      start_sensor(Watcher, N + 1, Sensor_num, Sensor_list ++ [{N, Sensor_PID}])
  end.

start() ->
  {ok, [Sensor_num]} = io:fread("Enter number of sensors> ", "~d"),
  start_sensor(ok, 0, Sensor_num, []),
  receive
    done -> 
      ok
  end.

main(_) ->
  start().
