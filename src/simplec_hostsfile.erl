%%%-------------------------------------------------------------------
%%% @author Guido <agx@bogon.m.sigxcpu.org>
%%% @copyright (C) 2015, Guido Günther
%%% @doc
%%%
%%% @end
%%% Created : 30 Aug 2015 by Guido <agx@bogon.m.sigxcpu.org>
%%%-------------------------------------------------------------------
-module(simplec_hostsfile).

-behaviour(gen_server).

-include("simplec.hrl").

%% API
-export([start_link/1,
	 write/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {config}).

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
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).


%%--------------------------------------------------------------------
%% @doc
%% Writes out the hosts
%% @end
%%--------------------------------------------------------------------
write(Hosts) ->
    gen_server:cast(simplec_hostsfile, {hosts, Hosts}).


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
init(Config) ->
    State = #state{config=Config},
    % Make sure the file exists
    write_hostsfile(State, {hosts, []}),
    {ok, State}.

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
handle_call(_Request, _From, State) ->
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
handle_cast({hosts, Hosts}, State) ->
    write_hostsfile(State, {hosts, Hosts}),
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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
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


write_line(Host, Addr) ->
    write_line(Host, Addr, []).

write_line(_Host, [], Aggr) ->
    Aggr;
write_line(Host, [H|T], Aggr) ->
    write_line(Host, T, [io_lib:format("~s ~s~n",[H,Host])|Aggr]).


write_hostsfile(#state{config=Config}, {hosts, Hosts}) ->
    File = filename:join(Config#config.dir, 
			 "libvirt-" ++ http_uri:encode(Config#config.url) ++ ".hosts"),
    TmpFile = File ++ [".tmp"],
    Data = [ write_line(Host, Addrs) || {Host, Addrs} <- Hosts ],
    file:write_file(TmpFile, Data),
    file:rename(TmpFile, File).
