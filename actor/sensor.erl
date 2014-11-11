-module(sensor).
-export([measure/2]).

handle(Watcher, SensorID, 11) ->
  Watcher ! {'EXIT', SensorID, anomalous_reading },
  catch exit(anomalous_reading);

handle(Watcher,SensorID,  Measurement) -> 
  Watcher ! {SensorID, Measurement}.

measure(Watcher, SensorID) ->
  random:seed(erlang:now()),
  Measurement = random:uniform(11),
  handle(Watcher,SensorID,  Measurement),
  Sleep_time = random:uniform(1000),
  timer:sleep(Sleep_time),
  measure(Watcher, SensorID).
  

  

