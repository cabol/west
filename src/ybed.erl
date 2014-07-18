-module(ybed).

-compile(export_all).

start(Args) ->
    {ok, spawn(?MODULE, run, [Args])}.

run(Args) ->
    Id = "embedded",
    GconfList = case proplists:lookup(gconf, Args) of
                    none ->
                        [{id, Id}, {ebin_dir, ["./ebin"]}, {runmod, "yapp"}];
                    {_, Gconf} ->
                        Gconf
                end,
    SconfList = case proplists:lookup(sconf, Args) of
                    none ->
                        [{servername, "west_server"},
                         {listen, {127,0,0,1}},
                         {port, 8080},
                         {docroot, "./www"},
                         {appmods, [{"west", west_ws_endpoint}]},
                         {opaque, [{yapp_server_id, "yapp_west"},
                                   {bootstrap_yapps, "west"}]}];
                    {_, Sconf} ->
                        Sconf
                end,
    Docroot = case proplists:lookup(docroot, SconfList) of
                  {_, Val} -> Val;
                  none     -> "./www"
              end,
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(ybed_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.

