%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the State Channel solo snapshot transaction
%%% @end
%%%=============================================================================
-module(aesc_snapshot_solo_tx).

-behavior(aetx).

%% Behavior API
-export([new/1,
         type/0,
         fee/1,
         gas/1,
         ttl/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         signers/2,
         version/0,
         serialization_template/1,
         serialize/1,
         deserialize/2,
         for_client/1
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(CHANNEL_SNAPSHOT_SOLO_TX_VSN, 1).
-define(CHANNEL_SNAPSHOT_SOLO_TX_TYPE, channel_snapshot_solo_tx).

-type vsn() :: non_neg_integer().

-record(channel_snapshot_solo_tx, {
          channel_id :: aec_id:id(),
          from_id    :: aec_id:id(),
          payload    :: binary(),
          ttl        :: aetx:tx_ttl(),
          fee        :: non_neg_integer(),
          nonce      :: non_neg_integer()
         }).

-opaque tx() :: #channel_snapshot_solo_tx{}.

-export_type([tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, aetx:tx()}.
new(#{channel_id := ChannelId,
      from_id    := FromId,
      payload    := Payload,
      fee        := Fee,
      nonce      := Nonce} = Args) ->
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(FromId),
    Tx = #channel_snapshot_solo_tx{
            channel_id = ChannelId,
            from_id    = FromId,
            payload    = Payload,
            ttl        = maps:get(ttl, Args, 0),
            fee        = Fee,
            nonce      = Nonce},
    {ok, aetx:new(?MODULE, Tx)}.

type() ->
    ?CHANNEL_SNAPSHOT_SOLO_TX_TYPE.

-spec fee(tx()) -> non_neg_integer().
fee(#channel_snapshot_solo_tx{fee = Fee}) ->
    Fee.

-spec gas(tx()) -> non_neg_integer().
gas(#channel_snapshot_solo_tx{}) ->
    0.

-spec ttl(tx()) -> aetx:tx_ttl().
ttl(#channel_snapshot_solo_tx{ttl = TTL}) ->
    TTL.

-spec nonce(tx()) -> non_neg_integer().
nonce(#channel_snapshot_solo_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(tx()) -> aec_keys:pubkey().
origin(#channel_snapshot_solo_tx{} = Tx) ->
    from_pubkey(Tx).

channel_pubkey(#channel_snapshot_solo_tx{channel_id = ChannelId}) ->
    aec_id:specialize(ChannelId, channel).

from_pubkey(#channel_snapshot_solo_tx{from_id = FromId}) ->
    aec_id:specialize(FromId, account).

-spec check(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees()} | {error, term()}.
check(#channel_snapshot_solo_tx{payload    = Payload,
                                fee        = Fee,
                                nonce      = Nonce} = Tx,
      Trees, _Env) ->
    ChannelPubKey = channel_pubkey(Tx),
    FromPubKey    = from_pubkey(Tx),
    case aesc_utils:check_solo_snapshot_payload(ChannelPubKey, FromPubKey, Nonce, Fee,
                                        Payload, Trees) of
        ok -> {ok, Trees};
        Err -> Err
    end.

-spec process(tx(), aec_trees:trees(), aetx_env:env()) -> {ok, aec_trees:trees(), aetx_env:env()}.
process(#channel_snapshot_solo_tx{payload    = Payload,
                                  fee        = Fee,
                                  nonce      = Nonce} = Tx,
        Trees, Env) ->
    ChannelPubKey = channel_pubkey(Tx),
    FromPubKey    = from_pubkey(Tx),
    aesc_utils:process_solo_snapshot(ChannelPubKey, FromPubKey, Nonce, Fee, Payload, Trees, Env).

-spec signers(tx(), aec_trees:trees()) -> {ok, list(aec_keys:pubkey())}.
signers(#channel_snapshot_solo_tx{} = Tx, _) ->
    {ok, [from_pubkey(Tx)]}.

-spec serialize(tx()) -> {vsn(), list()}.
serialize(#channel_snapshot_solo_tx{channel_id = ChannelId,
                                    from_id    = FromId,
                                    payload    = Payload,
                                    ttl        = TTL,
                                    fee        = Fee,
                                    nonce      = Nonce}) ->
    {version(),
     [ {channel_id, ChannelId}
     , {from_id   , FromId}
     , {payload   , Payload}
     , {ttl       , TTL}
     , {fee       , Fee}
     , {nonce     , Nonce}
     ]}.

-spec deserialize(vsn(), list()) -> tx().
deserialize(?CHANNEL_SNAPSHOT_SOLO_TX_VSN,
            [ {channel_id, ChannelId}
            , {from_id   , FromId}
            , {payload   , Payload}
            , {ttl       , TTL}
            , {fee       , Fee}
            , {nonce     , Nonce}]) ->
    channel = aec_id:specialize_type(ChannelId),
    account = aec_id:specialize_type(FromId),
    #channel_snapshot_solo_tx{channel_id = ChannelId,
                              from_id    = FromId,
                              payload    = Payload,
                              ttl        = TTL,
                              fee        = Fee,
                              nonce      = Nonce}.

-spec for_client(tx()) -> map().
for_client(#channel_snapshot_solo_tx{channel_id = ChannelId,
                                     from_id    = FromId,
                                     payload    = Payload,
                                     ttl        = TTL,
                                     fee        = Fee,
                                     nonce      = Nonce}) ->
    #{<<"channel_id">>  => aehttp_api_encoder:encode(id_hash, ChannelId),
      <<"from_id">>     => aehttp_api_encoder:encode(id_hash, FromId),
      <<"payload">>     => aehttp_api_encoder:encode(transaction, Payload),
      <<"ttl">>         => TTL,
      <<"fee">>         => Fee,
      <<"nonce">>       => Nonce}.

serialization_template(?CHANNEL_SNAPSHOT_SOLO_TX_VSN) ->
    [ {channel_id, id}
    , {from_id   , id}
    , {payload   , binary}
    , {ttl       , int}
    , {fee       , int}
    , {nonce     , int}
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec version() -> non_neg_integer().
version() ->
    ?CHANNEL_SNAPSHOT_SOLO_TX_VSN.

