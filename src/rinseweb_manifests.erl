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
                    value => <<"^(?i:convert)\\s+([-]?[0-9]*[.]?[0-9]+)\\s*([a-zA-Z]+[\\^]?[2-3]?)\\s+to\\s+([a-zA-Z]+[\\^]?[2-3]?)$">>
                }
            ],
            handler => rinseweb_wiz_convert
        },
        #{ % unix timestamp
            matches => [
                #{
                    type => regex,
                    value => <<"^([\\d]{10}|[\\d]{13})$">>
                },
                #{
                    type => regex,
                    value => <<"^(?i:unix timestamp|now|timestamp)$">>
                }
            ],
            handler => rinseweb_wiz_timestamp
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
