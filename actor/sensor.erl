-module(sensor).
-export([measure/2]).

%% if measurement is 11, sensor should exit
%% and send message to watcher
handle(_, Sensor_ID, 11) ->
  exit({anomalous_reading, Sensor_ID});

%% if measurement is 1-10, send it to watcher
handle(Watcher,Sensor_ID,  Measurement) -> 
  Watcher ! {Sensor_ID, Measurement}.

%% measure process, generate random measurement between 1 and 11
%% handle the measure and sleep for a random number of milliseconds
measure(Watcher, Sensor_ID) ->
  random:seed(erlang:now()),
  Sleep_time = random:uniform(10000),
  timer:sleep(Sleep_time),
  Measurement = random:uniform(11),
  handle(Watcher, Sensor_ID,  Measurement),
  measure(Watcher, Sensor_ID).
  

  

