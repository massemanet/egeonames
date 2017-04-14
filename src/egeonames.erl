%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 20 Jan 2014 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('egeonames').
-author('mats cronqvist').

%% the API
-export([start/0, stop/0, state/0,
         lookup/2, add_country/1, which_countries/0]).

%% for application supervisor
-export([start_link/0]).

%% gen_server callbacks
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the API

start() ->
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).

state() ->
  gen_server:call(?MODULE, state).

lookup(Country, Name) when is_list(Name) ->
  lookup(Country, list_to_binary(Name));
lookup(Country, Name) when is_list(Country) ->
  lookup(list_to_atom(Country), Name);
lookup(Country, Name) when is_binary(Name) ->
  try
    IDs = ets:match(?MODULE, {{Name, Country, '$1'}}),
    [ets:lookup(?MODULE, ID) || [ID] <- IDs]
  catch
    _:R -> {error, R}
  end.

add_country(Country) ->
  gen_server:call(?MODULE, {add_country, Country}, infinity).

which_countries() ->
  try
    [{countries, Countries}] = ets:lookup(?MODULE, countries),
    Countries
  catch _:badarg -> not_started
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% for application supervisor
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init(_) ->
  {ok, do_init()}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_call(state, _From, State) ->
  {reply, State, State};
handle_call({add_country, Country}, _From, State) ->
  {reply, do_add_country(State, Country), State};
handle_call(What, _From, State) ->
  {reply, What, State}.

handle_cast(_What, State) ->
  {noreply, State}.

handle_info(_What, State) ->
  {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% end of boilerplate
do_init() ->
  FileName = filename:join([code:lib_dir(?MODULE), data, "egeonames.ets"]),
  case ets:file2tab(FileName) of
    {error, _} ->
      ets:new(?MODULE, [ordered_set, named_table]),
      ets:insert(?MODULE, {countries, []});
    {ok, ?MODULE} ->
      ok
  end,
  #{filename => FileName}.

do_add_country(#{filename := Filename}, Country) ->
  [{countries, Countries}] = ets:lookup(?MODULE, countries),
  country_loaded(Country, Countries) orelse
    load_country(Filename, Country, Countries).

country_loaded(Country, Countries) ->
  lists:member(Country, Countries).

load_country(Filename, Country, Countries) ->
  application:ensure_all_started(inets),
  COUNTRY = string:to_upper(atom_to_list(Country)),
  io:fwrite("fetching - ~p~n", [Country]),
  try
    Body = http_get(COUNTRY),
    io:fwrite("inflating - ~p~n", [Country]),
    TSV = unzip(COUNTRY, Body),
    io:fwrite("inserting - ~p~n", [Country]),
    lists:foreach(fun(L) -> liner(Country, L) end, re:split(TSV, "\n")),
    ets:insert(?MODULE, {countries, [Country|Countries]}),
    io:fwrite("saving - ~p~n", [Country]),
    filelib:ensure_dir(Filename),
    ets:tab2file(?MODULE, Filename)
  catch _:R -> {error, R}
  end.

http_get(Country) ->
  URL = "http://download.geonames.org/export/dump/"++Country++".zip",
  Opts = [{body_format,binary}, {full_result,false}],
  case httpc:request(get, {URL, []}, [], Opts) of
    {ok, {200, Body}} -> Body;
    {ok, {Status, _}} -> throw({http_error,{Status,URL}});
    {error, R} -> throw({http_error, {R, URL}})
  end.

unzip(Country, Bin) ->
  ZipFile = Country++".txt",
  case zip:unzip(Bin, [{file_list, [ZipFile]}, memory]) of
    {ok, [{ZipFile, TSV}]} -> TSV;
    {error, R} -> throw({unzip_error, R})
  end.

liner(Country, Line) ->
  Bits = re:split(Line, "\t"),
  try
    [BID, RealName, LatinName, BNames, BLat, BLong, <<"P">>|_] = Bits,
    ID = list_to_integer(binary_to_list(BID)),
    Lat = bin_to_float(BLat),
    Long = bin_to_float(BLong),
    Names = [RealName, LatinName|split_comma(BNames)],
    ets:insert(?MODULE, {ID, RealName, Lat, Long}),
    lists:foreach(fun(N) -> ets:insert(?MODULE, {{N, Country, ID}}) end, Names)
  catch
    _:_ -> ok
  end.

bin_to_float(B) ->
  L = binary_to_list(B),
  try list_to_float(L)
  catch _:_ -> float(list_to_integer(L))
  end.

split_comma(B) ->
  case re:split(B, ",") of
    [<<>>] -> [];
    Ns     -> Ns
  end.
