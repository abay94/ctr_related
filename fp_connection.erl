%%-----------------------------------------------------------------
%% Copyright (c) 2018, Vozzhenikov Roman. All Rights Reserved.
%% Author: Vozzhenikov Roman, vzroman@gmail.com
%%-----------------------------------------------------------------

-module(fp_connection).

-include("fp_struct.hrl").

-behaviour(gen_server).

-behaviour(fp_items_sup).

%%=================================================================
%%	OTP
%%=================================================================
-export([
	start_link/1
]).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).
-export([
	items_query/0,
	stop/1,
	lookup_pid/1,
	get_item_id/1
]).

-define(STOP_TIMEOUT,10000).
-define(CONNECT_INTERVAL,2000).
-record(item,{item_id,handler,connection,cycle,bindings,state}).
-record(binding,{object,tag,field,type,scale_a,scale_b,value}).
%%=================================================================
%%	Connection API
%%=================================================================
-export([
	get_type/1,
	get_path/1,
	get_binding_type/1
]).

%%=================================================================
%%	Export/Import API
%%=================================================================
-export([
	export_fields/1,
	export_items/1,
	export_value/2,
	import_value/2,
	import_item/2
]).
%%====================================================================
%%	Connection behaviour interface
%%====================================================================
-callback init_fields() -> {ok,term()}.
-callback connect(ConnectionObject ::term()) -> {ok,ConnectionState::term()}|{error,term()}.
-callback close(ConnectionState::term()) -> ok|{error,term()}.
-callback read_tag(PlcTag::term(),ConnectionState::term()) -> {ok,term()}|{error,term()}.
-callback write_tag(PlcTag::term(),Value::term(),ConnectionState::term()) -> ok|{error,term()}.

%%=================================================================
%%	OTP
%%=================================================================
items_query()->
	{_,ChildPatterns}=ecomet:find(none,{'AND',[
		{<<".pattern">>,{link,<<"/root/.patterns/.pattern">>}},
		{<<"parent_pattern">>,{link,<<"/root/.patterns/plc_connection">>}}
	]}),
	{'AND',[
		{'OR',[{<<".pattern">>,?OID2B(Pattern)}||{Pattern,_}<-ChildPatterns]},
		{<<"enable">>,<<"true">>}
	]}.

stop(PID)->
  gen_server:cast(PID,{stop,self()}),
  receive
    {stopped,PID}->ok
  after
    ?STOP_TIMEOUT->error
  end.

lookup_pid(OID)->
	{ok,Object}=ecomet:open_nolock(OID),
	{ok,PID}=ecomet:read_field(Object,<<"pid">>),
	PID.

get_item_id(PID)->
	case gen_server:call(PID,get_item_id) of
		{ok,OID}->OID;
		_->none
	end.

start_link(ItemID)->
	gen_server:start_link(?MODULE, [ItemID], []).

init([OID])->
	ok=fp_lib:login(),
	{ok,Object}=ecomet:open_nolock(OID),
	ecomet:edit_object(Object,[{<<"pid">>,self()}]),
	Handler=get_handler(Object),
	{ok,RestartFields}=Handler:init_fields(),
	{ok,Path}=ecomet:oid_to_path(OID),
	{ok,Cycle}=ecomet:read_field(Object,<<"cycle">>),
	{ok,Name}=ecomet:read_field(Object,<<".name">>),
	 ?LOGINFO("Starting connection ~ts\r\n",[Name]),
	ok=ecomet:subscribe(<<"CONNECTION">>,RestartFields,{'AND',[
		{<<"enable">>,<<"true">>},
		{<<".PATH">>,Path}
	]}),
	%Subscribe to connection tags
	ok=ecomet:subscribe(<<"BINDINGS">>,[<<"tag">>,<<"field">>,<<"scale_A">>,<<"scale_B">>],{'ANDNOT',
		{<<".folder">>,?OID2B(OID)},
		{<<"disabled">>,<<"true">>}
	}),
	State=
		#item{
			item_id=OID,
			handler=Handler,
			bindings=[],
			cycle=Cycle,
			state=init
		},
	self()!on_cycle,
	{ok,State}.

handle_call(get_item_id, _From, State) ->
	{reply, {ok,State#item.item_id}, State}.


handle_cast({stop,From},State)->
  From!{stopped,self()},
  {stop, normal, State};
handle_cast(_Request,State)->
	{noreply,State}.

%%============================================================================
%%	Connection state handling
%%============================================================================
handle_info({ecomet_subscription,<<"CONNECTION">>,{edit,_ConnectionID,_ChangedFields}},State)->
	close(State#item.handler,State#item.connection),
	{noreply,State#item{state=init}};

%%============================================================================
%%	Bindings handling
%%============================================================================
% New binding
handle_info({ecomet_subscription,<<"BINDINGS">>,{create,BindingID,_Fields}},#item{bindings=Bindings}=State)->
	{ok,Binding}=add_binding(BindingID),
	{noreply,State#item{bindings= orddict:store(BindingID,Binding,Bindings)}};
% Binding is changed
handle_info({ecomet_subscription,<<"BINDINGS">>,{edit,BindingID,_Fields}},#item{bindings=Bindings}=State)->
	{ok,Binding}=add_binding(BindingID),
	{noreply,State#item{bindings=orddict:store(BindingID,Binding,Bindings)}};
% Binding is deleted
handle_info({ecomet_subscription,<<"BINDINGS">>,{delete,BindingID,_Fields}},#item{bindings=Bindings}=State)->
	Bindings1=
		case orddict:take(BindingID,Bindings) of
			{#binding{tag = Tag,field = Field },NewBindings}->
				ecomet:edit_object(Tag,[{Field,none}]),
				NewBindings;
			_->Bindings
		end,
	{noreply,State#item{bindings=Bindings1}};

%%============================================================================
%%	Cycle
%%============================================================================
% Connection is not ready
handle_info(on_cycle,#item{state=init,item_id=OID,handler=Handler,cycle=Cycle}=State)->
	timer:send_after(Cycle,on_cycle),
	try
		{ok,Object}=ecomet:open_nolock(OID),
		case Handler:connect(Object) of
			{ok,Connection}->
				set_state(Object,connected),
				{noreply,State#item{connection=Connection,state=ok}};
			{error,Error}->
				set_state(Object,{error,Error}),
				{noreply,State#item{state=error}}
		end
	catch
		_:_ ->{noreply,State}
	end;
% Connection error
handle_info(on_cycle,#item{state=error,connection=Connection,handler=Handler,cycle=Cycle}=State)->
	timer:send_after(Cycle,on_cycle),
	close(Handler,Connection),
	{noreply,State#item{state=init}};
% Connection ready
handle_info(on_cycle,#item{state = ok,bindings=Bindings,cycle=Cycle,connection=Connection,handler=Handler}=State)->
	timer:send_after(Cycle,on_cycle),
	Start=erlang:system_time(millisecond),
	try
		Result=run_bindings(Bindings,Connection,Handler,[]),
		log_cycle(Start,Cycle,State#item.item_id),
		case Result of
			{ok,Bindings1}->
				{noreply,State#item{bindings=Bindings1}};
			{error,Error}->
				{ok,Object}=ecomet:open_nolock(State#item.item_id),
				set_state(Object,{error,Error}),
				{noreply,State#item{state=error}};
			stop->{stop, normal, State}
		end
	catch
		_:_ ->{noreply,State}
	end;

handle_info(on_cycle,State)->
	timer:send_after(1000,on_cycle),
	{noreply,State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason,#item{item_id = OID,bindings=Bindings,connection=Connection,handler=Handler})->
	close(Handler,Connection),
	case ecomet:open_nolock(OID) of
		{ok,Object}->
			{ok,Name}=ecomet:read_field(Object,<<".name">>),
			?LOGINFO("Stop connection ~ts\r\n",[Name]),
			ecomet:edit_object(Object,[{<<"pid">>,self()}]),
			lists:foreach(fun({_,#binding{tag=Tag,field=Field}})->
				ecomet:edit_object(Tag,[{Field,none}])
										end,Bindings);
		_->
			?LOGINFO("Delete connection ~p\r\n",[self()])
	end,
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%===========================================================================
%% Export/Import
%%===========================================================================
export_fields(Type)->
	Handler=
	case Type of
		<<"MODBUS_CONNECTION">>->fp_modbus;
		<<"S7-LOGO_CONNECTION">>->fp_s7_LOGO;
		<<"S7-200_CONNECTION">>->fp_s7_200;
		<<"S7-1200_CONNECTION">>->fp_s7_1200;
		<<"S7-300_400_CONNECTION">>->fp_s7_300;
		<<"SNMP_CONNECTION">>->fp_snmp;
		<<"MERCURY230_CONNECTION">>->fp_mercury230;
		<<"MBUS_CONNECTION">>->fp_mbus;
		<<"DCON_CONNECTION">>->fp_dcon;
		<<"OPCDA_CONNECTION">>->fp_opcda;
		<<"IGLA_CONNECTION">>->fp_igla;
		<<"CTR_CONNECTION">>->fp_ctr
	end,
	[<<".name">>,<<"enable">>,<<"cycle">>]++Handler:export_fields().

export_items(Type)->
	{_,Objects}=ecomet:find(none,{'AND',[
		{<<".pattern">>,{link,<<"/root/.patterns/",Type/binary>>}},
		{<<".folder">>,{link,<<"/root/PROJECT/CONNECTIONS">>}}
	]}),
	ItemsList=
	lists:map(fun({OID,_})->
		{ok,Object}=ecomet:open_nolock(OID),
		lists:map(fun(Field)->
			export_value(Field,Object)
		end,export_fields(Type))
	end,Objects),
	lists:sort(ItemsList).

export_value(FieldName,Object)->
	{ok,Value}=ecomet:read_field(Object,FieldName,ecomet_impexp),
	Value.

import_value(_FieldName,Value)->Value.

import_item(Type,Fields)->
	{ok,_}=ecomet:transaction(fun()->ecomet:edit_or_create([
		{<<".pattern">>,<<"/root/.patterns/",Type/binary>>},
		{<<".folder">>,<<"/root/PROJECT/CONNECTIONS">>}|Fields
	],ecomet_impexp) end),
	ok.

%% ====================================================================
%% Connection API
%% ====================================================================
get_type(Name)->
	Path=get_path(Name),
	{ok,CID}=ecomet_folder:path_to_oid(Path),
	{ok,Connection}=ecomet:open_nolock(CID),
	{ok,PatternID}=ecomet:read_field(Connection,<<".pattern">>),
	{ok,<<"/root/.patterns/",Type/binary>>}=ecomet:oid_to_path(PatternID),
	Type.

get_path(Name)->
	<<"/root/PROJECT/CONNECTIONS/",Name/binary>>.

get_binding_type(ConnectionType)->
	case ConnectionType of
		<<"MODBUS_CONNECTION">>-><<"MODBUS_TAG">>;
		<<"S7-LOGO_CONNECTION">>-><<"S7_TAG">>;
		<<"S7-200_CONNECTION">>-><<"S7_TAG">>;
		<<"S7-1200_CONNECTION">>-><<"S7_TAG">>;
		<<"S7-300_400_CONNECTION">>-><<"S7_TAG">>;
		<<"SNMP_CONNECTION">>-><<"SNMP_TAG">>;
		<<"MERCURY230_CONNECTION">>-><<"MERCURY230_TAG">>;
		<<"MBUS_CONNECTION">>-><<"MBUS_TAG">>;
		<<"DCON_CONNECTION">>-><<"DCON_TAG">>;
		<<"OPCDA_CONNECTION">>-><<"OPCDA_TAG">>;
		<<"IGLA_CONNECTION">>-><<"IGLA_TAG">>;
		<<"CTR_CONNECTION">>-><<"CTR_TAG">>
	end.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
close(Handler,State)->
	if
		((State/=init) and (State/=error) and (State/=undefined))->Handler:close(State);
		true->ok
	end.

get_handler(Object)->
	{ok,Handler}=ecomet:read_field(Object,<<"back_handler">>),
	list_to_atom(unicode:characters_to_list(Handler)).

set_state(Object,connected)->
	{ok,Name}=ecomet:read_field(Object,<<".name">>),
	?LOGERROR("~ts connection established",[Name]),
	ecomet:edit_object(Object,[
		{<<"state">>,<<"true">>},
		{<<"error">>,none}
	]);
set_state(Object,{error,Error})->
	{ok,Name}=ecomet:read_field(Object,<<".name">>),
	?LOGERROR("~ts connection error ~p",[Name,Error]),
	ErrorStr=list_to_binary(io_lib:format("~p",[Error])),
	ecomet:edit_object(Object,[
		{<<"state">>,<<"false">>},
		{<<"error">>,ErrorStr}
	]),
	{ok,OID}=ecomet:get_oid(Object),
	{_,Bindings}=ecomet:find([<<"tag">>,<<"field">>],{<<".folder">>,?OID2B(OID)}),
	lists:foreach(fun({_,Fields})->
		TagID=proplists:get_value(<<"tag">>,Fields),
		TagField=proplists:get_value(<<"field">>,Fields),
		{ok,TagObject}=ecomet:open_nolock(TagID),
		ecomet:edit_object(TagObject,[{TagField,none}])
	end,Bindings).

add_binding(PlcTagID)->
	ecomet:transaction(fun()->
		{ok,PlcTagObj}=ecomet:open_nolock(PlcTagID),
		{ok,TargetTagID}=ecomet:read_field(PlcTagObj,<<"tag">>),
		{ok,TargetTagObj}=ecomet:open_nolock(TargetTagID),
		{ok,Field}=ecomet:read_field(PlcTagObj,<<"field">>),
		{ok,ScaleA}=ecomet:read_field(PlcTagObj,<<"scale_A">>),
		{ok,ScaleB}=ecomet:read_field(PlcTagObj,<<"scale_B">>),
		{ok,TargetTagObjFields}=ecomet_object:get_fields(TargetTagObj),
		{ok,Type}=ecomet_fields:get_type(TargetTagObjFields,Field),
		#binding{object=PlcTagObj,tag=TargetTagObj,field=Field,type=Type,scale_a=ScaleA,scale_b=ScaleB,value=none}
	end).

run_bindings([{OID,Binding}|Rest],Connection,Handler,Result)->
	case is_stop() of
		false->
			case sync_binding(Binding,Connection,Handler) of
				{ok,Binding1}->run_bindings(Rest,Connection,Handler,[{OID,Binding1}|Result]);
				{error,Error}->{error,Error}
			end;
		true->stop
	end;
run_bindings([],_Connection,_Handler,Result)->{ok,lists:reverse(Result)}.

is_stop()->
	receive
		{stop,Supervisor}->
			Supervisor!{stopping,self()},
			true
	after
		0->false
	end.

sync_binding(Binding,Connection,Handler) when Binding#binding.value=:=none->
	try
		get_binding_value(Binding,Connection,Handler)
	catch
		_:Error->programming_error(Binding,Error)
	end;

sync_binding(#binding{tag=Tag,field=Field,value=LastValue}=Binding,Connection,Handler)->
	try
		case ecomet:read_field(Tag,Field) of
			{ok,LastValue}->
				get_binding_value(Binding,Connection,Handler);
			{ok,none}->
				get_binding_value(Binding,Connection,Handler);
			{ok,NewValue}->
				set_binding_value(Binding,Connection,Handler,NewValue)
		end
	catch
	    _:Error->
        programming_error(Binding,Error)
	end.

get_binding_value(#binding{object=Object,tag=Tag,field=Field}=Binding,Connection,Handler)->
	case Handler:read_tag(Object,Connection) of
		{ok,RawValue}->
			case in_value(Binding,RawValue) of
				{error,Error}->set_tag_error(Binding,Error);
				{ok,ReadyValue}->
					{ok,_}=ecomet:edit_object(Object,[{<<"state">>,<<"true">>}]),
					{ok,_}=ecomet:edit_object(Tag,[{Field,ReadyValue}]),
					{ok,Binding#binding{value=ReadyValue}}
			end;
		{error,{connection_error,Error}}->
			{error,Error};
		{error,Error}->
			set_tag_error(Binding,Error)
	end.
	
set_binding_value(#binding{object=PlcTagObj}=Binding,Connection,Handler,Value)->
	{ok,ReadyValue}=out_value(Binding,Value),
	case Handler:write_tag(PlcTagObj,ReadyValue,Connection) of
		{error,{connection_error,Error}}->{error,Error};
		{error,Error}->
			set_tag_error(Binding,Error);
		ok->
			get_binding_value(Binding,Connection,Handler)
	end.

set_tag_error(#binding{object=PlcTagObj,tag=TargetTagObj,field=Field}=Tag,Error)->
	{ok,Path}=ecomet:get_path(PlcTagObj),
	?LOGERROR("~ts ~p",[Path,Error]),
	ErrorStr=list_to_binary(io_lib:format("~p",[Error])),
	{ok,_}=ecomet:edit_object(PlcTagObj,[
		{<<"state">>,<<"false">>},
		{<<"error">>,ErrorStr}
	]),
	{ok,_}=ecomet:edit_object(TargetTagObj,[{Field,none}]),
	{ok,Tag#binding{value=none}}.
	
in_value(#binding{type = <<"bool">>,scale_a=ScaleA,scale_b=ScaleB}=Binding,RawValue)->
	case RawValue of
		<<1:1>>->{ok,<<"true">>};
		<<0:1>>->{ok,<<"false">>};
		<<1:8>>->{ok,<<"true">>};
		<<0:8>>->{ok,<<"false">>};
		<<0:16>>->{ok,<<"false">>};
		<<_:16>>->{ok,<<"true">>};
		<<0:32>>->{ok,<<"false">>};
		<<_:32>>->{ok,<<"true">>};
		I when is_integer(I)->
			{ok,IValue}=scale_integer(read,I,ScaleA,ScaleB),
			in_value(Binding,IValue/=0);
		F when is_float(F)->
			{ok,FValue}=scale_float(read,F,ScaleA,ScaleB),
			in_value(Binding,(FValue>0.00000001) or (FValue<(-0.00000001)));
		1->{ok,<<"true">>};
		0->{ok,<<"false">>};
		<<"true">>->{ok,<<"true">>};
		<<"false">>->{ok,<<"false">>};
		true->{ok,<<"true">>};
		false->{ok,<<"false">>};
		_->{error,{invalid_bool,RawValue}}
	end;

in_value(#binding{type = <<"integer">>,scale_a=ScaleA,scale_b=ScaleB},RawValue) when is_integer(RawValue)->
	scale_integer(read,RawValue,ScaleA,ScaleB);
in_value(#binding{type = <<"integer">>,scale_a=ScaleA,scale_b=ScaleB},RawValue)->
	case RawValue of
		<<I:1>>->scale_integer(read,I,ScaleA,ScaleB);
		<<I:8>>->scale_integer(read,I,ScaleA,ScaleB);
		<<I:16/big-integer>>->scale_integer(read,I,ScaleA,ScaleB);
		<<I:32/big-integer>>->scale_integer(read,I,ScaleA,ScaleB);
		<<I:64/big-integer>>->scale_integer(read,I,ScaleA,ScaleB);
		I when is_integer(I)->scale_integer(read,I,ScaleA,ScaleB);
		F when is_float(F)->scale_integer(read,round(F),ScaleA,ScaleB);
		<<"true">>->scale_integer(read,1,ScaleA,ScaleB);
		<<"false">>->scale_integer(read,0,ScaleA,ScaleB);
		true->scale_integer(read,1,ScaleA,ScaleB);
		false->scale_integer(read,0,ScaleA,ScaleB);
		_->{error,{invalid_integer,RawValue}}
	end;

in_value(#binding{type = <<"float">>,scale_a=ScaleA,scale_b=ScaleB},RawValue) when is_float(RawValue)->
	scale_float(read,RawValue,ScaleA,ScaleB);
in_value(#binding{type = <<"float">>,scale_a=ScaleA,scale_b=ScaleB},RawValue) when is_integer(RawValue)->
	scale_float(read,RawValue,ScaleA,ScaleB);
in_value(#binding{type = <<"float">>,scale_a=ScaleA,scale_b=ScaleB},RawValue)->
	case RawValue of
		<<F:32/big-float>>->scale_float(read,F,ScaleA,ScaleB);
		<<I:16/big-integer>>->scale_float(read,I,ScaleA,ScaleB);
		<<B:1>>->scale_float(read,B,ScaleA,ScaleB);
		I when is_integer(I)->scale_float(read,I,ScaleA,ScaleB);
		F when is_float(F)->scale_float(read,F,ScaleA,ScaleB);
		<<"true">>->scale_float(read,1.0,ScaleA,ScaleB);
		<<"false">>->scale_float(read,0.0,ScaleA,ScaleB);
		true->scale_float(read,1.0,ScaleA,ScaleB);
		false->scale_float(read,0.0,ScaleA,ScaleB);
		_->{error,{invalid_float,RawValue}}
	end;
in_value(#binding{type = <<"string">>},RawValue)->
	if
		is_atom(RawValue) -> {ok,unicode:characters_to_binary(atom_to_list(RawValue))};
		is_integer(RawValue) -> {ok,integer_to_binary(RawValue)};
		is_float(RawValue) -> {ok,float_to_binary(RawValue)};
		is_binary(RawValue) -> {ok,RawValue};
		true ->
			{ok,unicode:characters_to_binary(io_lib:format("~p",[RawValue]))}
	end;
in_value(<<"binary">>,RawValue)->
	if
		is_binary(RawValue) ->{ok,RawValue};
		true ->
			{ok,unicode:characters_to_binary(io_lib:format("~p",[RawValue]))}
	end.

out_value(#binding{type = <<"bool">>,scale_a=ScaleA,scale_b=ScaleB},Value)
	when ((round(ScaleA)==1) and (round(ScaleB)==0))->
	case Value of
		<<"true">>->{ok,true};
		<<"false">>->{ok,false}
	end;
out_value(#binding{type = <<"bool">>}=Binding,Value)->
	Value1=
		case Value of
			<<"true">>->1;
			<<"false">>->0
		end,
	out_value(Binding#binding{type = <<"float">>},Value1);
out_value(#binding{type = <<"integer">>,scale_a=ScaleA,scale_b=ScaleB},Value)->
	scale_integer(write,Value,ScaleA,ScaleB);
out_value(#binding{type = <<"float">>,scale_a=ScaleA,scale_b=ScaleB},Value)->
	scale_float(write,Value,ScaleA,ScaleB);
out_value(#binding{type = <<"string">>},Value)->
	{ok,Value};
out_value(#binding{type = <<"binary">>},Value)->
	{ok,Value}.

scale_integer(read,I,ScaleA,ScaleB)->
	{ok,round(I*ScaleA+ScaleB)};
scale_integer(write,I,ScaleA,ScaleB)->
	{ok,round((I-ScaleB)/ScaleA)}.
scale_float(read,F,ScaleA,ScaleB)->
	{ok,F*ScaleA+ScaleB};
scale_float(write,F,ScaleA,ScaleB)->
	{ok,(F-ScaleB)/ScaleA}.

log_cycle(Start,Cycle,OID)->
	Duration=erlang:system_time(millisecond)-Start,
	if
		Duration>Cycle ->
			{ok,Object}=ecomet:open_nolock(OID),
			{ok,Name}=ecomet:read_field(Object,<<".name">>),
			?LOGWARNING("~ts connection cycle ~p",[Name,Duration]);
		true -> ok
	end.

programming_error(Binding,Error)->
	{ok,Path}=ecomet:get_path(Binding#binding.object),
	?LOGERROR("programming_error: binding ~s, error ~p",[Path,Error]),
	{ok,OID}=ecomet:get_oid(Binding#binding.object),
	add_binding(OID).



