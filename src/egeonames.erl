%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 20 Jan 2014 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('egeonames').
-author('mats cronqvist').

%% the API
-export([start/0,stop/0,state/0,unlink/0,lookup/1]).

%% for application supervisor
-export([start_link/0]).

%% gen_server callbacks
-behaviour(gen_server).
-export([init/1,terminate/2,code_change/3,
         handle_call/3,handle_cast/2,handle_info/2]).

%% declare the state
-record(state,{dirname = dirname(),
               filenames = filenames(dirname())}).

%% add all records here, to kludge around the record kludge.
rec_info(state) -> record_info(fields,state);
rec_info(_)     -> [].

%% the API
start() ->
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).

unlink() ->
  gen_server:call(?MODULE,unlink).

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
lookup(Name) ->
  lookup(list_to_binary(Name)).

%% for application supervisor
start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% gen_server callbacks
init(_) ->
  {ok,do_init(#state{})}.

terminate(_Reason,_State) ->
  ok.

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.

handle_call(state,_From,State) ->
  {reply,expand_recs(State),State};
handle_call(unlink,_From,State) ->
  {links,Links} = process_info(self(),links),
  lists:foreach(fun unlink/1,Links),
  {reply,ok,State};
handle_call(What,_From,State) ->
  {reply,What,State}.

handle_cast(_What,State) ->
  {noreply,State}.

handle_info(_What,State) ->
  {noreply,State}.

%% utility to print state
expand_recs(List) when is_list(List) ->
  [expand_recs(I) || I <- List];
expand_recs(Tup) when is_tuple(Tup) ->
  case tuple_size(Tup) of
    L when L < 1 -> Tup;
    L ->
      try Fields = rec_info(element(1,Tup)),
          L = length(Fields)+1,
          lists:zip(Fields,expand_recs(tl(tuple_to_list(Tup))))
      catch _:_ ->
          list_to_tuple(expand_recs(tuple_to_list(Tup)))
      end
  end;
expand_recs(Term) ->
  Term.

%% end of boilerplate
dirname() -> filename:join(code:priv_dir(?MODULE),data).
filenames(Dir) -> filelib:wildcard(filename:join(Dir,"[A-Z][A-Z].txt")).
tablename() -> egeonames.

do_init(S) ->
  lists:foreach(fun process_file/1,S#state.filenames),
  S.

process_file(FN) ->
  {ok,FD} = file:open(FN,[read,raw,binary,read_ahead]),
  filefold(FD,mk_liner()).

mk_liner() ->
  Tab = tablename(),
  ets:new(Tab,[ordered_set,named_table]),
  ets:insert(Tab,{count,1}),
  fun({ok,Data}) -> ets:update_counter(Tab,count,1),handle_line(Tab,Data);
     ({error,Reason}) -> error({read_line,Reason});
     (eof) -> throw(eof)
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
    [BID,RealName,LatinName,BNames,BLat,BLong,<<"P">>|_] = re:split(Data,"\t"),
    ID = list_to_integer(binary_to_list(BID)),
    Lat = bin_to_float(BLat),
    Long = bin_to_float(BLong),
    ets:insert(Tab,{ID,RealName,Lat,Long}),
    insert_names(Tab,ID,[RealName,LatinName|split_comma(BNames)])
  catch
    _:_ -> ok
  end.

bin_to_float(B) ->
  case re:run(B,"\\.") of
    nomatch -> float(list_to_integer(binary_to_list(B)));
    _ -> list_to_float(binary_to_list(B))
  end.

split_comma(B) ->
  case re:split(B,",") of
    [<<>>] -> [];
    Ns     -> Ns
  end.

insert_names(Tab,ID,Names) ->
  [ets:insert(Tab,{{Name,ID}}) || Name <- Names].
