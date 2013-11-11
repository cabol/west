-module(ybed).

-compile(export_all).

start(Args) ->
    {ok, spawn(?MODULE, run, [Args])}.

run(Args) ->
    Id = "embedded",
    case proplists:lookup(gconf, Args) of
        none ->
            GconfList = [{id, Id}, {ebin_dir, ["./ebin"]}, {runmod, "yapp"}];
        {_, Gconf} ->
            GconfList = Gconf
    end,
    case proplists:lookup(sconf, Args) of
        none ->
            Docroot = "./www",
            SconfList = [{servername, "west_server"},
                         {listen, {127,0,0,1}},
                         {port, 8080},
                         {docroot, Docroot},
                         {appmods, [{"west", west_yout}]},
                         {opaque, [{yapp_server_id, "yapp_west"},
                                   {bootstrap_yapps, "west"}]}];
        {_, Sconf} ->
            SconfList = Sconf,
            case proplists:lookup(docroot, SconfList) of
                none        -> Docroot = "./www";
                {_, Val}    -> Docroot = Val
             end
    end,
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(ybed_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.

