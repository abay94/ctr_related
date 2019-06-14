%%-----------------------------------------------------------------
%% Copyright (c) 2019, Zeinet&SSE. All Rights Reserved.
%% Author: Rustam Krikbaev, rkrikbaev@gmail.com
%%-----------------------------------------------------------------
-module(fp2flash_bootstrap).

-include("fp2flash_util.hrl").

-behaviour(gen_server).

%%===========================================================================
%% API
%%===========================================================================
-export([
  start_link/0
]).

%%===========================================================================
%% gen_server callbacks
%%===========================================================================
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).


-define(PROJECT_PARAMS,[
  <<"usergroups">>,
  <<"users">>,
  <<"taggroups">>,
  <<"tags">>,
  <<"archives">>,
  <<"alarms">>,
  <<"connections">>,
  <<"agents">>,
  <<"images">>,
  <<"patterns">>,
  <<"faceplates">>,
  <<"screens">>,
  <<"rights">>,
  <<"trends">>,
  <<"scripts">>,
  <<"reports">>,
  <<"video">>,
  <<"logic">>,
  <<"localization">>
]).


%%=====================================================================
%% API
%%=====================================================================
start_link()->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%%=====================================================================
%% Gen server behaviour
%%=====================================================================
init([])->
  ok=fp_lib:login(),
  Cycle=ecomet_lib:get_env(fp2flash,update_cycle,30000),
  self()!on_cycle,
  {ok, Cycle}.

%% No API for communication with the external world is needed
handle_call(_Request,_From,State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

%% We are notified about connection got ready
handle_info(on_cycle,Cycle) ->
  timer:send_after(Cycle,on_cycle),
  {ok,SettingsID}=ecomet:path_to_oid(<<"/root/PROJECT">>),
  {ok,Settings}=ecomet:open_nolock(SettingsID),
  Config=
    case ecomet:read_field(Settings,<<"bootstrap">>) of
      {ok,none}->#{};
      {ok,ConfigValue}->ConfigValue
    end,
  case request_config() of
    error->ok;
    NewConfig->
      Firmware=fp2flash_util:json_path([<<"content">>,<<"firmware">>],NewConfig),
      case fp2flash_util:json_path([<<"content">>,<<"firmware">>],Config) of
        Firmware->ok;
        _->import_firmware(Firmware)
      end,
      case ecomet:edit_object(Settings,[{<<"bootstrap">>,NewConfig},{<<"RunTime">>,<<"true">>}]) of
        {error,Error}->?LOGERROR("Unable to save new config ~p",[Error]);
        _->ok
      end
  end,
  {noreply, Cycle}.

%% LOOP. Connection is ready, gather and send the data packet

terminate(_Reason, _State) ->
  ok.

% Hot update (not needed yet)
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


request_config()->
  Address=ecomet_lib:get_env(fp2flash,bootstrap_address,"134.209.240.215"),
  Port=integer_to_list(ecomet_lib:get_env(fp2flash,bootstrap_port,8200)),
  ExternalID=fp2flash_keys:get_external_id(),
  URL = "http://"++Address++":"++Port++"/things/bootstrap/"++ExternalID,
  Key=fp2flash_keys:get_external_key(ExternalID),
  Header = [{"Authorization", Key}, {"Content-Type", "application/json"}],
  HTTPOptions = [],
  Options = [{body_format,binary}],
  case httpc:request(get,{URL,Header},HTTPOptions, Options) of
    {ok,{{_,200,_},_,Response}}->
      Config=jsx:decode(Response,[return_maps]),
      Content=maps:get(<<"content">>,Config),
      Config#{<<"content">>=>jsx:decode(Content,[return_maps])};
  Unexpected->
  ?LOGERROR("Unable to get bootstrap configuration ~p, key ~p, error ~p",[URL, Key, Unexpected]),
    error
  end.
%   #{<<"content">>=>#{<<"firmware">>=><<"fp_project.prj">>}}.


import_firmware(Firmware)->
  Firmware_name=binary_to_list(Firmware),
  Address=ecomet_lib:get_env(fp2flash,firmware_address,"192.168.1.54"),
  Port=integer_to_list(ecomet_lib:get_env(fp2flash,firmware_port,8180)),
  case httpc:request(get,{"http://"++Address++":"++Port++"/firmware/"++Firmware_name,[]},[],[{body_format, binary}]) of
    {ok,{{_,200,_},_,Response}}->
      ?LOGINFO("Cleaning project"),
      clean_project(),
      ?LOGINFO("Start importing firmware"),
      case fp_project:from_zip(?PROJECT_PARAMS, Response) of
        {error, Error}->?LOGERROR("Error importing firmware ~p",[Error]);
        _->?LOGINFO("Importing firmware ok")
      end;
    Unexpected-> ?LOGERROR("Unable to download firmware ~p, error ~p",[Firmware, Unexpected])
  end.

clean_project()->
  ?LOGINFO("cleaning project"),
  fp_project:clean(),
  ?LOGINFO("cleaning project finished").





