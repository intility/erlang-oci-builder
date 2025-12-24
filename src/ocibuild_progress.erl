%%%-------------------------------------------------------------------
-module(ocibuild_progress).
-moduledoc """
Multi-line progress bar display for concurrent operations.

Provides process-bound progress bars that can display multiple concurrent
download/upload operations, each on its own terminal line.

Usage:
```
%% Start the progress manager (call once before parallel operations)
ocibuild_progress:start_manager(),

%% In each worker process, register a progress bar
Ref = ocibuild_progress:register_bar(#{
    label => ~"Layer 1/2 (amd64)",
    total => 28400000
}),

%% Update progress (called repeatedly during download/upload)
ocibuild_progress:update(Ref, 5000000),

%% Mark as complete
ocibuild_progress:complete(Ref),

%% Stop the manager when done
ocibuild_progress:stop_manager().
```
""".

-export([
    start_manager/0,
    stop_manager/0,
    register_bar/1,
    update/2,
    complete/1,
    clear/0,
    make_callback/1
]).

%% Internal exports
-export([manager_loop/1]).

-record(bar_state, {
    ref :: reference(),
    label :: binary(),
    total :: non_neg_integer(),
    current :: non_neg_integer(),
    complete :: boolean()
}).

-record(manager_state, {
    bars :: #{reference() => #bar_state{}},
    % Order bars were registered
    bar_order :: [reference()],
    % Number of lines we've actually printed to terminal
    lines_printed :: non_neg_integer(),
    is_tty :: boolean()
}).

%%%===================================================================
%%% Public API
%%%===================================================================

-doc "Start the progress manager process.".
-spec start_manager() -> ok | {error, already_started}.
start_manager() ->
    case whereis(ocibuild_progress_manager) of
        undefined ->
            IsTTY = is_tty(),
            State = #manager_state{
                bars = #{},
                bar_order = [],
                lines_printed = 0,
                is_tty = IsTTY
            },
            Pid = spawn_link(fun() -> manager_loop(State) end),
            register(ocibuild_progress_manager, Pid),
            ok;
        _ ->
            {error, already_started}
    end.

-doc "Stop the progress manager and clean up.".
-spec stop_manager() -> ok.
stop_manager() ->
    case whereis(ocibuild_progress_manager) of
        undefined ->
            ok;
        Pid ->
            Pid ! {stop, self()},
            receive
                stopped -> ok
            after 2000 ->
                ok
            end
    end.

-doc "Register a new progress bar. Returns a reference for updates.".
-spec register_bar(map()) -> reference().
register_bar(Opts) ->
    Ref = make_ref(),
    Label = maps:get(label, Opts, ~"Progress"),
    Total = maps:get(total, Opts, 0),
    case whereis(ocibuild_progress_manager) of
        undefined ->
            %% No manager, just return ref (progress won't display)
            Ref;
        Pid ->
            Pid ! {register, Ref, Label, Total},
            Ref
    end.

-doc "Update progress for a bar.".
-spec update(reference(), non_neg_integer()) -> ok.
update(Ref, Current) ->
    case whereis(ocibuild_progress_manager) of
        undefined ->
            ok;
        Pid ->
            Pid ! {update, Ref, Current},
            ok
    end.

-doc "Mark a progress bar as complete.".
-spec complete(reference()) -> ok.
complete(Ref) ->
    case whereis(ocibuild_progress_manager) of
        undefined ->
            ok;
        Pid ->
            Pid ! {complete, Ref},
            ok
    end.

-doc """
Clear all progress bars from the manager.

Use this between major operations (e.g., between save and push) to reset
the display and start fresh with new progress bars.
""".
-spec clear() -> ok.
clear() ->
    case whereis(ocibuild_progress_manager) of
        undefined ->
            ok;
        Pid ->
            Pid ! clear,
            ok
    end.

-doc """
Create a progress callback function compatible with ocibuild_registry.

The callback will register a bar on first call and update it on subsequent calls.
""".
-spec make_callback(map()) -> fun((map()) -> ok).
make_callback(Opts) ->
    %% Use process dictionary to track the ref for this callback
    Label = maps:get(label, Opts, ~"Downloading"),
    fun(Info) ->
        #{total_bytes := Total} = Info,
        Bytes = maps:get(bytes_received, Info, maps:get(bytes_sent, Info, 0)),

        %% Get or create ref for this callback instance
        Key = {ocibuild_progress_ref, self(), Total},
        Ref =
            case get(Key) of
                undefined ->
                    NewRef = register_bar(#{label => Label, total => Total}),
                    put(Key, NewRef),
                    NewRef;
                ExistingRef ->
                    ExistingRef
            end,

        update(Ref, Bytes),

        %% Mark complete if done
        case Bytes >= Total of
            true -> complete(Ref);
            false -> ok
        end
    end.

%%%===================================================================
%%% Manager Process
%%%===================================================================

manager_loop(State) ->
    receive
        {register, Ref, Label, Total} ->
            Bar = #bar_state{
                ref = Ref,
                label = Label,
                total = Total,
                current = 0,
                complete = false
            },
            NewBars = maps:put(Ref, Bar, State#manager_state.bars),
            NewOrder = State#manager_state.bar_order ++ [Ref],
            NewState = State#manager_state{
                bars = NewBars,
                bar_order = NewOrder
            },
            %% redraw_all handles printing new lines if needed
            FinalState = redraw_all(NewState),
            manager_loop(FinalState);
        {update, Ref, Current} ->
            case maps:find(Ref, State#manager_state.bars) of
                {ok, Bar} ->
                    NewBar = Bar#bar_state{current = Current},
                    NewBars = maps:put(Ref, NewBar, State#manager_state.bars),
                    NewState = State#manager_state{bars = NewBars},
                    FinalState = redraw_all(NewState),
                    manager_loop(FinalState);
                error ->
                    manager_loop(State)
            end;
        {complete, Ref} ->
            case maps:find(Ref, State#manager_state.bars) of
                {ok, Bar} ->
                    NewBar = Bar#bar_state{current = Bar#bar_state.total, complete = true},
                    NewBars = maps:put(Ref, NewBar, State#manager_state.bars),
                    NewState = State#manager_state{bars = NewBars},
                    FinalState = redraw_all(NewState),
                    manager_loop(FinalState);
                error ->
                    manager_loop(State)
            end;
        clear ->
            %% Clear all bars and reset display state
            %% Move cursor below current progress area first
            case State#manager_state.is_tty andalso State#manager_state.lines_printed > 0 of
                true -> io:format("~n");
                false -> ok
            end,
            NewState = State#manager_state{
                bars = #{},
                bar_order = [],
                lines_printed = 0
            },
            manager_loop(NewState);
        {stop, From} ->
            %% Move cursor below progress area and clean up
            case State#manager_state.is_tty andalso State#manager_state.lines_printed > 0 of
                true -> io:format("~n");
                false -> ok
            end,
            From ! stopped,
            ok
    end.

%%%===================================================================
%%% Rendering
%%%===================================================================

redraw_all(#manager_state{is_tty = false} = State) ->
    %% Non-TTY mode: don't do animated updates
    State;
redraw_all(#manager_state{bars = Bars, bar_order = Order, lines_printed = LinesPrinted} = State) ->
    BarCount = length(Order),
    case BarCount of
        0 ->
            State;
        _ ->
            %% Print new lines if we have more bars than lines printed
            NewLinesNeeded = BarCount - LinesPrinted,
            lists:foreach(fun(_) -> io:format("~n") end, lists:seq(1, max(0, NewLinesNeeded))),

            %% Move cursor to column 1, then up to the top of the progress area
            %% \r = carriage return (column 1)
            %% ESC[nA = move cursor up n lines (ESC = \033 = ASCII 27)
            io:format("\r\033[~wA", [BarCount]),

            %% Render each bar on its own line
            lists:foreach(
                fun(Ref) ->
                    case maps:find(Ref, Bars) of
                        {ok, Bar} ->
                            render_bar(Bar),
                            io:format("~n");
                        error ->
                            io:format("~n")
                    end
                end,
                Order
            ),

            %% Return updated state with lines_printed = BarCount
            State#manager_state{lines_printed = BarCount}
    end.

render_bar(#bar_state{label = Label, total = Total, current = Current}) ->
    %% Clear line and render progress bar (ESC[K = clear to end of line)
    io:format("\r\033[K"),

    Percent =
        case Total of
            0 -> 0;
            _ -> min(100, (Current * 100) div Total)
        end,

    BarWidth = 30,
    Filled = (Percent * BarWidth) div 100,
    Empty = BarWidth - Filled,

    FilledStr = lists:duplicate(Filled, $=),
    EmptyStr = lists:duplicate(Empty, $\s),

    CurrentStr = format_bytes(Current),
    TotalStr = format_bytes(Total),

    io:format(
        "  ~s: [~s~s] ~3B% ~s/~s",
        [Label, FilledStr, EmptyStr, Percent, CurrentStr, TotalStr]
    ).

%%%===================================================================
%%% Helpers
%%%===================================================================

is_tty() ->
    case io:columns() of
        {ok, _} -> true;
        {error, _} -> false
    end.

format_bytes(Bytes) when Bytes < 1024 ->
    io_lib:format("~B B", [Bytes]);
format_bytes(Bytes) when Bytes < 1024 * 1024 ->
    io_lib:format("~.1f KB", [Bytes / 1024]);
format_bytes(Bytes) when Bytes < 1024 * 1024 * 1024 ->
    io_lib:format("~.1f MB", [Bytes / (1024 * 1024)]);
format_bytes(Bytes) ->
    io_lib:format("~.2f GB", [Bytes / (1024 * 1024 * 1024)]).
