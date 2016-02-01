%%%-------------------------------------------------------------------
%%% @author Guido <agx@bogon.m.sigxcpu.org>
%%% @copyright (C) 2015, Guido Günther
%%% @doc
%%%
%%% @end
%%% Created :  2 Sep 2015 by Guido <agx@bogon.m.sigxcpu.org>
%%%-------------------------------------------------------------------
-module(simplec_vms).

-behaviour(gen_server).

-include("simplec.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(QUERY_INTERVAL, 2000).

-record(state, {url, verxref, cur}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args = {url, Url}) ->
    Name = list_to_atom(lists:flatten(io_lib:format("~p_~s", [?SERVER, Url] ))),
    gen_server:start_link({local, Name}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({url, Url}) ->
    io:format("Monitoring ~s~n~n", [Url]),
    {ok, Ref} = verx_client:start(),
    {ok, [1]} = verx:auth_polkit(Ref),
    ok = verx:connect_open(Ref, [Url, 0]),
    {ok, #state{url=Url, verxref=Ref}, ?QUERY_INTERVAL}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
    io:format("Call ~s~n~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("Msg ~s~n~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State = #state{url=Url}) ->
    {ok, Res} = get_addresses(State#state.verxref),
    case Res == State#state.cur of
	false -> print_addresses(Res),
		 simplec_hostsfile:write(Res, Url);
	true -> true
    end,
    {noreply, State#state{cur=Res}, ?QUERY_INTERVAL};
handle_info(Info, State) ->
    io:format("Info ~s~n~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    io:format("Terminating"),
    ok = verx:connect_close(State#state.verxref),
    ok = verx_client:stop(State#state.verxref),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

filter_addresses(Addrs) ->
    [ begin
	{_Iface, _Mac, [{_Type, Addr, _Netmask}]} = A,
        Addr
      end || A <- Addrs ].


addresses(Ref, Domain) ->
    case verx:domain_interface_addresses(Ref, [Domain, 0, 0]) of
	{ok, [Addrs]} -> Addrs;
	{error, _Error} -> []
    end.


domains(Ref, IDs) ->
    [ begin
        {ok, [Domain]} = verx:domain_lookup_by_id(Ref, [N]),
	Domain
      end || N <- IDs ].


print_addr({Name, Adresses}) ->
    io:format("~20s: ", [Name]),
    [ io:format("~s ", [A]) || A <- Adresses ],
    io:format("~n").


print_addresses(Res) ->
    [ print_addr(R) || R <- Res ],
    io:format("~n").


get_addresses(Ref) ->
    {ok, [NumRun]} = verx:connect_num_of_domains(Ref),
    {ok, [Running]} = verx:connect_list_domains(Ref, [NumRun]),

    Domains = domains(Ref, Running),
    Res = [ begin
		{Name, _, _} = D,
		Addrs = addresses(Ref, D),
		{Name, filter_addresses(Addrs)}
	    end || D <- Domains ],
    {ok, Res}.
