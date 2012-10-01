%% Copyright (c) 2012 Concurix, Inc.

-module(pmc).
-export([allocate/1, release/1,
         attach/1,
         start/1, stop/1,
	 read/1, write/2,
         call/3]).

%% A NIF module to allow the use of hardware performance counters
%% through the pmc(3) library.

-ifdef(HAVE_PMCS).
-on_load(init/0).

init() ->
    case code:where_is_file("pmc_nif.so") of
        non_existing ->
            false;
        Filename ->
            NoSuffix = string:sub_string(Filename, 1, length(Filename)-3),
            ok = erlang:load_nif(NoSuffix, 0)
    end.

%% Allocate a counter for an event specifier.

-spec allocate(atom()) -> integer().
allocate(_ES) ->
    exit(nif_library_not_loaded).

-spec release(integer()) -> atom().
release(_Id) ->
    exit(nif_library_not_loaded).

-spec attach(integer()) -> atom().
attach(_Id) ->
    exit(nif_library_not_loaded).

-spec start(integer()) -> atom().
start(_Id) ->
    exit(nif_library_not_loaded).

-spec stop(integer()) -> atom().
stop(_Id) ->
    exit(nif_library_not_loaded).

-spec read(integer()) -> atom() | integer().
read(_Id) ->
    exit(nif_library_not_loaded).

-spec write(integer(), integer()) -> atom().
write(_Id, _Val) ->
    exit(nif_library_not_loaded).

%% Measure the performance of a function call using a list of counters.
-spec call(fun(), list(), list()) -> term().
call(Fun, Args, Events) when is_list(Events), length(Events) =< 4 ->
    call_with_pmcs(Fun, Args, Events).

call_with_pmcs(Fun, Args, []) ->
    {apply(Fun, Args), []};
call_with_pmcs(Fun, Args, [E|Es]) ->
    case pmc:allocate(E) of
        Pmc when is_number(Pmc) ->
            ok = pmc:write(Pmc, 0),
            ok = pmc:attach(Pmc),
            ok = pmc:start(Pmc),
            Result = call_with_pmcs(Fun, Args, Es),
            ok = pmc:stop(Pmc),
            Count = pmc:read(Pmc),
            ok = pmc:release(Pmc),

            %% In the case of errors, just propagate the error message
            %% In the case of success, propagate the result with
            %% counts attached.
            case Result of
                {error, _} ->
                    Result;
                {CallResult, Counts} ->
                    {CallResult, [{E, Count}|Counts]}
            end;
        Error ->
            {error, {E, Error}}
    end.

-else.

%% Stub definitions for platforms that do not support PMCs.

-spec allocate(atom()) -> not_supported.
allocate(_ES) ->
    not_supported.

-spec release(integer()) -> not_supported.
release(_Id) ->
    not_supported.

-spec attach(integer()) -> not_supported.
attach(_Id) ->
    not_supported.

-spec start(integer()) -> not_supported.
start(_Id) ->
    not_supported.

-spec stop(integer()) -> not_supported.
stop(_Id) ->
    not_supported.

-spec read(integer()) -> not_supported.
read(_Id) ->
    not_supported.

-spec write(integer(), integer()) -> not_supported.
write(_Id, _Val) ->
    not_supported.

%% Measure the performance of a function call using a list of counters.
-spec call(fun(), list(), list()) -> not_supported.
call(_Fun, _Args, _Events) ->
    not_supported.

-endif.

