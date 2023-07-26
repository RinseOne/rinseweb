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
    cache => rinseweb_cache:options(),
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
            cache => #{
                n => 10,          % 10 segments
                ttl => 86400,     % 1 day
                memory => 1048576 % 1 MB limit (rough)
            },
            handler => rinseweb_wiz_convert
        },
        #{ % wikipedia
            matches => [
                #{
                    type => regex,
                    value => <<"^(?i:wiki)\s+(.*)$">>
                }
            ],
            cache => #{
                n => 10,           % 10 segments
                ttl => 86400,      % 1 day
                memory => 10485760 % 10 MB
            },
            handler => rinseweb_wiz_wikipedia
        },
        #{ % wikihow
            matches => [
                #{
                    type => regex,
                    value => <<"^(?i:wikihow)\s+(.*)$">>
                },
                #{
                    type => regex,
                    value => <<"^(?i:how\s*to)\s+(.*)$">>
                },
                #{
                    type => regex,
                    value => <<"^(?i:how\s*do\s*i)\s+(.*)$">>
                }
            ],
            cache => #{
                n => 10,           % 10 segments
                ttl => 86400,      % 1 day
                memory => 10485760 % 10 MB
            },
            handler => rinseweb_wiz_wikihow
        },
        #{ % definition
            matches => [
                #{
                    type => regex,
                    value => <<"^(?i:define)\s+(.*)$">>
                }
            ],
            cache => #{
                n => 10,           % 10 segments
                ttl => 86400,      % 1 day
                memory => 10485760 % 10 MB
            },
            handler => rinseweb_wiz_dictionaryapi
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
        #{ % redirects
            matches => [
                #{
                    type => regex,
                    value => <<"^(ddg|ddgi|ddgv|ddgn|ddgm)\s+(.*)$">>
                }
            ],
            handler => rinseweb_wiz_redirect
        },
        #{ % what is my ip
            matches => [
                #{
                    type => regex,
                    value => <<"^(?i:wimip|wimua|(what is my)\\s+(ip|user agent))$">>
                }
            ],
            handler => rinseweb_wiz_whatismy
        },
        #{ % operators
            matches => [
                #{
                    type => regex,
                    value => <<"^\\(?\\s*(\\+|-|\\/|\\*)((\\s+[-]?[0-9]*[.]?[0-9]+)+)\\s*\\)?$">>
                }
            ],
            parser => {rinseweb_wiz_apply, parse_args},
            handler => rinseweb_wiz_apply
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
