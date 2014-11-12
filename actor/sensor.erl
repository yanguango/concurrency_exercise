-module(sensor).
-export([measure/2]).

handle(_, Sensor_ID, 11) ->
  exit({anomalous_reading, Sensor_ID});

handle(Watcher,Sensor_ID,  Measurement) -> 
  Watcher ! {Sensor_ID, Measurement}.

measure(Watcher, Sensor_ID) ->
  random:seed(erlang:now()),
  Measurement = random:uniform(11),
  handle(Watcher, Sensor_ID,  Measurement),
  Sleep_time = random:uniform(1000),
  timer:sleep(Sleep_time),
  measure(Watcher, Sensor_ID).
  

  

