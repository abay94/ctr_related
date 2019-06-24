%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
-module(fp_ctr).
-include("fp_struct.hrl").

-behaviour(fp_connection).

-define(TIMEOUT,2000).
%% ====================================================================
%% Connection functions
%% ====================================================================
-export([
  init_fields/0,
  connect/1,
  close/1,
  read_tag/2,
  write_tag/3
]).

-export([
  export_fields/0
]).

export_fields()->
  [].
%%=====================================================================
%%  fp connection implementation
%%=====================================================================
init_fields()->
  {ok,export_fields()}.

connect(_Settings)->
  Dir=code:priv_dir(fp)++"/cWrapper",
  Port = open_port({spawn, Dir},[stream, overlapped_io, use_stdio, in, out, binary, exit_status]),
  receive
    {Port, {data, <<"OK">>}}->{ok,Port};
    {Port, {data, <<"ERROR: ",Error/binary>>}}->{error,Error}
  after
    ?TIMEOUT->{error,timeout}
  end.

close(Port)->
  port_close(Port).

read_tag(Source,Port)->
  {ok,Type}=ecomet:read_field(Source,<<"type">>),
  {ok,Address}=ecomet:read_field(Source,<<"address">>),
  {ok,Mode}=ecomet:read_field(Source,<<"mode">>),
  Cmd=build_read_cmd(Type,Address,Source),
  ?LOGINFO("COMMANDDDD : READAD TAG ~p", [Cmd]),                        %  DELEETEEEEE
  port_command(Port,[Cmd]),
  case get_response(Port) of
    {ok,Response}->parse_response(Type,Response,Mode);
    {error,Error}->{error,Error}
  end.

% Write operations are not supported
write_tag(Source,Value,Port)->
  {ok,Type}=ecomet:read_field(Source,<<"type">>),
  {ok,Address}=ecomet:read_field(Source,<<"address">>),
  {ok,Mode}=ecomet:read_field(Source,<<"mode">>),
  Cmd=build_write_cmd(Type,Address,Value,Source),
  ?LOGINFO("WRITEEEEEEEEEEE ~p",[Cmd]),
  port_command(Port,[Cmd]),
  case get_response(Port) of
    {ok,Response}->parse_response(Type,Response,Mode);
    {error,Error}->{error,Error}
  end.

build_read_cmd(<<"DI">>,Address,_Source)->
  Address1=integer_to_binary(Address),
  <<"di ",Address1/binary>>;
build_read_cmd(<<"DO">>,Address,Source)->
  {ok,Mode}=ecomet:read_field(Source,<<"mode">>),
  Address1=integer_to_binary(Address),
  <<"do ",Address1/binary," ", Mode/binary>>;
build_read_cmd(<<"AI">>,Address,Source)->
  {ok,Mode}=ecomet:read_field(Source,<<"mode">>),
  Mode1=
    case Mode of
      <<"current">>-><<"1">>;
      <<"voltage">>-><<"0">>
    end,
  Address1=integer_to_binary(Address),
  <<"ai ",Address1/binary," ",Mode1/binary>>.

build_write_cmd(<<"DO">>,Address,_Value,Source)->
  {ok,Mode}=ecomet:read_field(Source,<<"mode">>),
  Address1=integer_to_binary(Address),
  <<"do ",Address1/binary," ", Mode/binary>>;
build_write_cmd(<<"AI">>,Address,_Value,Source)->
  {ok,Mode}=ecomet:read_field(Source,<<"mode">>),
  Mode1=
    case Mode of
      <<"current">>-><<"1">>;
      <<"voltage">>-><<"0">>
    end,
  Address1=integer_to_binary(Address),
  <<"ai ",Address1/binary," ",Mode1/binary>>.

get_response(Port)->
  receive
    {Port, {data, Data}}->{ok,Data}
  after
    ?TIMEOUT->{error,timeout}
  end.

parse_response(<<"DI">>,Data,_Mode)->
  {ok,binary_to_integer(Data)};
parse_response(<<"AI">>,Data,Mode)->
  Range = case Mode of
            <<"current">> -> 19544;
            <<"voltage">>-> 28016
          end,
  CoeffMultiply = case Mode of
            <<"current">> -> 16;
            <<"voltage">>-> 10
          end,
  Converted  =  binary_to_integer(Data) * CoeffMultiply / Range,
  Temp = Converted *  18.75 - 125,
  ?LOGINFO("COMMANDDDD : CONVERTINGGGG  ~p",[Converted]),                                %  DELEETEEEEE
  ?LOGINFO("COMMANDDDD : DATAAA   ~p",[Data]),                                %  DELEETEEEEE
  {ok,Temp};
parse_response(<<"DO">>,Data,_Mode)->
  ?LOGINFO("DO RESPONESEEEEEE ~p",[Data]),
  case Data of
    <<"ok">>->ok;
    Invalid->{error,Invalid}
  end.



