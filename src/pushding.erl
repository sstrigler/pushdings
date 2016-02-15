-module(pushding).

-export([publish/2,
         subscribe/1]).

publish(Topic, Message) ->
    gproc_ps:publish(l, {?MODULE, Topic}, Message).

subscribe(Topic) ->
    gproc_ps:subscribe(l, {?MODULE, Topic}).
