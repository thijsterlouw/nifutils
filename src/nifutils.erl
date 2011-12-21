-module(nifutils).

-export([nif_now/0]).

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
    (catch erlang:load_nif(SoName, 0)),
    case erlang:system_info(otp_release) of
    "R13B03" -> true;
    _ -> ok
    end.



nif_now() ->
    exit(snappy_nif_not_loaded).
