%%%-------------------------------------------------------------------
%%% @author Grzegorz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. cze 2018 20:55
%%%-------------------------------------------------------------------
-module(pollution_gen_server_supervisor).
-behaviour(supervisor).
-author("Grzegorz").

%% API
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, serverSupervisor}, ?MODULE, []).

init(InitValue) ->
  {ok, {
    {one_for_all, 2, 3},
    [ {pollution_gen_server,
      {pollution_gen_server, start, []},
      permanent, brutal_kill, worker, [pollution_gen_server]}
    ]}
  }.
