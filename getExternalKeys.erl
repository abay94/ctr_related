%%-----------------------------------------------------------------
%% Copyright (c) 2019, Zeinet&SSE. All Rights Reserved.
%% Author: Abay Aubakirov, a.aubakirov@zeinetsse.com
%%-----------------------------------------------------------------
-module(fp2flash_keys).

%% API
-export([
  get_external_id/0,
  get_external_key/0, get_external_key/1
]).

get_mac_addr() ->
  { ok , NetAttributes}  = inet:getifaddrs(),
  Hwaddr=get_hwaddr(NetAttributes),
  MacList = lists:map(fun(Elem) -> integer_to_list(Elem, 16) end, Hwaddr),
  MacUpper = string:join(MacList, ":"),
  string:to_lower(MacUpper).

to_hex(Str) ->
  Len = string:length(Str),
  if Len < 2 -> "0" ++ Str;
     Len > 1  -> Str end.
     
make_hash(Id) ->
  Hashed = crypto:hash(md5, Id),
  HashList = binary:bin_to_list(Hashed),
  List = lists:map(fun(Elem) -> to_hex(integer_to_list(Elem, 16)) end, HashList),
  Hashed_upper = string:join(List, ""),
  string:to_lower(Hashed_upper).


get_external_id() ->
  get_mac_addr().

get_external_key()->
  get_external_key(get_mac_addr()).
get_external_key(Mac)->
  make_hash(Mac).

get_hwaddr([{_,Attr}|Rest])->
  case proplists:get_value(hwaddr,Attr,[0,0,0,0,0,0]) of
    [0,0,0,0,0,0]->get_hwaddr(Rest);
    Address->Address
  end.


