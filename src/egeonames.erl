%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 20 Jan 2014 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('egeonames').
-author('mats cronqvist').

%% the API
-export([start/0,stop/0,state/0,lookup/1]).

%% for application supervisor
-export([start_link/0]).

%% gen_server callbacks
-behaviour(gen_server).
-export([init/1,terminate/2,code_change/3,
         handle_call/3,handle_cast/2,handle_info/2]).

%% the API
start() ->
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).

state() ->
  gen_server:call(?MODULE,state).

lookup(Name) when is_binary(Name) ->
  try
    Tab = tablename(),
    IDs = ets:match(Tab,{{Name,'$1'}}),
    [ets:lookup(Tab,ID) || [ID] <- IDs]
  catch
    _:R -> {error,R}
  end;
lookup(Name) when is_list(Name) ->
  lookup(list_to_binary(Name)).

%% for application supervisor
start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% gen_server callbacks
init(_) ->
  {ok,do_init()}.

terminate(_Reason,_State) ->
  ok.

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.

handle_call(state,_From,State) ->
  {reply,State,State};
handle_call(What,_From,State) ->
  {reply,What,State}.

handle_cast(_What,State) ->
  {noreply,State}.

handle_info(_What,State) ->
  {noreply,State}.

%% end of boilerplate
dirname() -> filename:join(code:priv_dir(?MODULE),data).
filename() -> "SE.txt".
tablename() -> egeonames_se.

do_init() ->
  FN = filename:join([dirname(),filename()]),
  {ok,FD} = file:open(FN,[read,raw,binary,read_ahead]),
  filefold(FD,mk_liner(tablename())),
  file:close(FD),
  #{dirname => dirname(),
    filename => filename(),
    tablename => tablename()}.

mk_liner(Tab) ->
  fun({ok,Data})      -> handle_line(Tab,Data);
     ({error,Reason}) -> error({read_line,Reason});
     (eof)            -> throw(eof)
  end.

filefold(FD,Fun) ->
  try
    Fun(file:read_line(FD)),
    filefold(FD,Fun)
  catch
    throw:eof -> ok
  end.

handle_line(Tab,Data) ->
  try
    SD = re:split(Data,"\t"),
    [BID,RealName,LatinName,BNames,BLat,BLong,<<"P">>,_,CC|_] = SD,
    ID = list_to_integer(binary_to_list(BID)),
    Lat = bin_to_float(BLat),
    Long = bin_to_float(BLong),
    CCA = binary_to_atom(CC,latin1),
    ets:insert(Tab,{ID,RealName,CC,Lat,Long}),
    insert_names(Tab,ID,CCA,[RealName,LatinName|split_comma(BNames)])
  catch
    _:_ -> ok
  end.

bin_to_float(B) ->
  case re:run(B,"\\.") of
    nomatch -> float(list_to_integer(binary_to_list(B)));
    _       -> list_to_float(binary_to_list(B))
  end.

split_comma(B) ->
  case re:split(B,",") of
    [<<>>] -> [];
    Ns     -> Ns
  end.

insert_names(Tab,ID,CC,Names) ->
  [ets:insert(Tab,{{Name,CC,ID}}) || Name <- Names].
