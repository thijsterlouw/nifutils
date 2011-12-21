-module(nifutils).

-export([nif_uniform/1, nif_random/0, nif_time/0, nif_now/0]).
-export([nif_rot13/1]).

-on_load(init/0).

-type ascii_string() :: [1..255].
-type timestamp() :: {MegaSecs::non_neg_integer(), Secs::non_neg_integer(), MicroSecs::non_neg_integer()}.

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


-spec nif_now/0 :: ( ) -> timestamp().
%% @doc Returns tuple of {MegaS, S, MicroS}
nif_now() ->
	erlang:now().

-spec nif_time/0 :: ( ) -> integer().
%% @doc Returns unix timestamp as integer
nif_time() ->
	{MegaS, S, _MicroS} = erlang:now(),
	MegaS*1000000 + S.	

-spec nif_random/0 :: ( ) -> integer().
%% @doc Returns random integer
nif_random() ->
	{_, _, MicroS} = erlang:now(),
	MicroS.

-spec nif_uniform/1 :: ( integer() ) -> integer().
%% @doc Returns random integer in a range 1..N where N>1
nif_uniform(N) when N>1 ->
	{A1,A2,A3} = erlang:now(),
	random:seed(A1, A2, A3),
	random:uniform(N).	

-spec nif_rot13/1 :: ( ascii_string() ) -> ascii_string().
%% @doc Performs rot13 against input string. Requires ascii input!
nif_rot13(Input) when is_list(Input) ->
	lists:map(fun(Char) ->
						rot13(Char)
				end,
				Input	   
			).
	
%===========================================
	
rot13(X) when X >= $a,
			  X <  $n; 
			  X >= $A, 
			  X <  $N ->
    X+13;

rot13(X) when X >= $n,
	      	  X <  ${;
	      	  X >= $N,
	      	  X <  $[ ->
    X-13;

rot13(X) ->
    X.



	
	
