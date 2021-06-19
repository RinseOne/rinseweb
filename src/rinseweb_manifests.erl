%%%-------------------------------------------------------------------
%% @doc rinseweb manifests
%% @end
%%%-------------------------------------------------------------------

-module(rinseweb_manifests).

%% API
-export([get_all/0]).

%% Types
-type match_type() :: regex.
-type match() :: #{
    type := match_type(),
    value := binary()
}.
-type manifest() :: #{
    matches := [match()],
    handler := atom()
}.

-export_type([match/0]).
-export_type([manifest/0]).

%%====================================================================
%% API
%%====================================================================

-spec get_all() -> [manifest()].
get_all() ->
    [
        #{ % converter
            matches => [
                #{
                    type => regex,
                    value => <<"^(?i:convert)\s+([-]?[0-9]*[.]?[0-9]+)\s*([a-zA-Z]+)\s+to\s+([a-zA-Z]+)$">>
                }
            ],
            handler => rinseweb_wiz_convert
        },
        #{ % hash
            matches => [
                #{
                    type => regex,
                    value => <<"^(sha|sha2|md5)\s+(.*)$">>
                }
            ],
            handler => rinseweb_wiz_hash
        },
        #{ % UUID
            matches => [
                #{
                    type => regex,
                    value => <<"^(?i:uuid)$">>
                }
            ],
            handler => rinseweb_wiz_uuid
        },
        #{ % fallthrough default
            matches => [
                #{
                    type => regex,
                    value => <<"^(.*)$">>
                }
            ],
            handler => rinseweb_wiz_default
        }
    ].
