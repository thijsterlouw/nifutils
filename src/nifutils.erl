-module(nifutils).

-export([nif_uniform/1, nif_random/0, nif_time/0, nif_now/0]).

-on_load(init/0).


init() ->
    SoName = case code:priv_dir(?MODULE) of
    {error, bad_name} ->
        case filelib:is_dir(filename:join(["..", "priv"])) of
        true ->
            filename:join(["..", "priv", "nifutils"]);
        false ->
            filename:join(["priv", "nifutils"])
        end;
    Dir ->
        filename:join(Dir, "nifutils")
    end,
    LoadInfo = (catch erlang:load_nif(SoName, 0)),
	case LoadInfo of
		ok -> ok;
		_ ->
			io:format("Error loading ~p : ~p\n", [SoName, LoadInfo])
	end,
    case erlang:system_info(otp_release) of
    	"R13B03" -> true;
    	_ -> ok
    end.


%% @doc Returns tuple of {MegaS, S, MicroS}
nif_now() ->
	erlang:now().

%% @doc Returns unix timestamp as integer
nif_time() ->
	{MegaS, S, _MicroS} = erlang:now(),
	MegaS*1000000 + S.	

%% @doc Returns random integer
nif_random() ->
	{_, _, MicroS} = erlang:now(),
	MicroS.

%% @doc Returns random integer in a range 1..N where N>1
nif_uniform(N) when N>1 ->
	{A1,A2,A3} = erlang:now(),
	random:seed(A1, A2, A3),
	random:uniform(N).	
