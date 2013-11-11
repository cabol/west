-module(ybed_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

start_link(Args) when is_list(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Args]).

init(Args) ->
    YBed = {ybed,
            {ybed, start, Args},
            permanent,
            2000,
            worker,
            [ybed]},
    {ok, {{one_for_all, 0, 1}, [YBed]}}.