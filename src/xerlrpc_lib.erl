%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%@author Jovany Leandro G.C <dirindesa@neurotec.co>
-module(xerlrpc_lib).
-include_lib("xmerl/include/xmerl.hrl").

-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-compile(export_all).
-endif.

-type encode_param() :: boolean() 
                      | integer() 
                      | float() 
                      | binary()
                      | {base64, binary()}
                      | proplists:property()
                      | calendar:datetime()
                      | map()
                      | list().

-type decode_response() :: {ok, [any()]} | {fault, proplists:property()} | {error, term()}.


-export_type([decode_response/0]).
-export([encode_call_to_string/2, decode_response_string/1]).


-spec encode_call_to_string({atom(), [encode_param()]}) -> {ok, binary()} | {error, any()}.
encode_call_to_string(Method)  when is_tuple(Method) ->
    L1 = tuple_to_list(Method),
    [Name | Args] = L1,
    encode_call_to_string(Name, Args).

-spec encode_call_to_string(atom(), [encode_param()]) ->{ok, binary()} | {error, any()}.
encode_call_to_string(Name, Params) when is_list(Params), is_atom(Name) ->
    try encode_params(Params) of
        [Data] ->
            Prolog =   ["<?xml version=\"1.0\" encoding=\"utf-8\" ?>" ],
            {ok,
             list_to_binary(
               lists:flatten(xmerl:export_simple([{methodCall, [{methodName, [atom_to_list(Name)]}
                                                               , Data]}], xmerl_xml, [{prolog, Prolog}])))}
    catch
        throw:Throw ->
             {error, Throw}
    end.


-spec decode_response_string(binary() | iolist()) 
                            -> decode_response().
decode_response_string(Xml) when is_list(Xml) ->
    Acc = fun(#xmlElement{} = E, Acc, S) ->
                  {[E | Acc], S};
             (#xmlText{parents=Parents, pos = P} = E, Acc, S) ->
                  {Parent, _} = hd(Parents),
                  Allowed = [name, string 
                            , double, 'dateTime.iso8601', base64, int, i4, boolean],
                  case lists:member(Parent, Allowed) of
                      true ->
                          {[E | Acc], S};
                      false ->
                          {Acc, P, S}
                  end
          end,
    {Data,_} = xmerl_scan:string(Xml, [{acc_fun, Acc}]),
    try decode_xml_response(Data) of
        Dec ->
            case Dec of
                {fault, Fault} ->
                    {fault, Fault};
                _ ->
                    {ok, Dec}
            end
    catch throw:Throw ->
            {error, Throw}
    end.


encode_params(Params) ->
    [{params, encode_param(Params)}].
encode_param([Param | Rest]) ->
    [{param, [{value, [encode(Param)]}]} | encode_param(Rest)];
encode_param([]) ->
    [].

encode(true) ->
    {boolean, "1"};
encode(false) ->
    {boolean, "0"};
encode(Value) when is_float(Value) ->
    {double, [float_to_list(Value)]};
encode(Value) when is_integer(Value) ->
    {int, [integer_to_list(Value)]};
encode({base64, Value}) when is_binary(Value) ->
    {base64, [binary_to_list(base64:encode(Value))]};
encode(Value) when is_binary(Value) ->
    {string, [binary_to_list(Value)]};
encode({{_Year, _Month, _Day}, {_Hour, _Min, _Sec}} = Value) ->
    {'dateTime.iso8601', [binary_to_list(iso8601:format(Value))]};
encode([{Name, _} = Value | Rest]) when not is_tuple(Name) ->
    {struct, [encode_member(Value) | encode_member(Rest)]};
encode(Struct) when is_map(Struct) ->
    encode(maps:to_list(Struct));
encode([{}]) ->
    {struct, [""]};
encode(Values) when is_list(Values) ->
    {array, [{data, encode_data(Values)}]};
encode([]) ->
    [];
encode(_) ->
    throw(badparam).


encode_data([]) ->
    [];
encode_data([Value | Rest]) ->
    [{value, [encode(Value)]} | encode_data(Rest)];
encode_data(Value) ->
    encode(Value).

encode_member([]) ->
    [];
encode_member({Key, Value}) when is_atom(Key) ->
    {member, [{name,  [atom_to_list(Key)]}, {value, [encode(Value)]}]};
encode_member({Key, Value}) when is_binary(Key) ->
    {member, [{name,  [binary_to_list(Key)]}, {value, [encode(Value)]}]};
encode_member({_, _}) ->
    throw(badmember);
encode_member([Value | Rest]) ->
    [encode_member(Value) | encode_member(Rest)].


decode_xml_response(#xmlElement{name=methodResponse, content=[Content]}) ->
    decode_xml_params(Content).
decode_xml_params(#xmlElement{name=fault, content=[Content]}) ->
    {fault, decode_xml(Content)};
decode_xml_params(#xmlElement{name='params', content=Content}) ->
    [decode_xml(hd(Param#xmlElement.content)) ||  Param <- Content, Param#xmlElement.name==param].

decode_xml(#xmlElement{name=array, content=[Content]}) ->
    decode_xml_array(Content);
decode_xml(#xmlElement{name=struct, content=Content}) ->
    decode_xml_struct(Content);
decode_xml(#xmlElement{name=value, content=[Content]}) ->
    decode_xml(Content);
decode_xml(#xmlElement{name=string,content=[Content]}) ->
    decode_xml(Content);
decode_xml(#xmlElement{name=int,content=[Content]}) ->
    binary_to_integer(decode_xml(Content));
decode_xml(#xmlElement{name='i4',content=[Content]}) ->
    binary_to_integer(decode_xml(Content));
decode_xml(#xmlElement{name=double,content=Content}) ->
    binary_to_float(decode_xml(Content));
decode_xml(#xmlElement{name=base64,content=[Content]}) ->
    base64:decode(decode_xml(Content));
decode_xml(#xmlElement{name='dateTime.iso8601', content=[Content]}) ->
    iso8601:parse(decode_xml(Content));
decode_xml(#xmlText{value=Value}) ->
    list_to_binary(Value);
decode_xml(#xmlElement{name=Name}) ->
    throw(Name);
decode_xml([]) ->
    [].

decode_xml_array(#xmlElement{name=data,content=Content}) ->
    [decode_xml(Value) || Value <- Content].

decode_xml_struct([#xmlElement{name=member, content=[
                                                     #xmlElement{name=name,content=[Name]}
                                                    , #xmlElement{name=value, content=[Value]}]} | Rest]) ->
    [{decode_xml(Name), decode_xml(Value)} | decode_xml_struct(Rest)];
decode_xml_struct([]) ->
    [].

    
-ifdef(TEST).
encode_decode_xml_test() ->
    {ok, _} = encode_call_to_string(test, [1,2,3]),
    Time = erlang:localtime(),
    Params = [Time, {base64, <<"xml">>}, 1,2,3, <<"carajo">>,[5,6],7, #{phone => 666}],
    Expect = [Time, <<"xml">>, 1,2,3, <<"carajo">>,[5,6],7, [{<<"phone">>, 666}]],
    EncParams = encode_params(Params),
    Xml = lists:flatten(xmerl:export_simple(EncParams, xmerl_xml)),
    {Ret,_} = xmerl_scan:string(Xml),
    Dec = decode_xml_params(Ret),
    ?assertEqual(Expect,Dec),
    ok.

decode_xml_test() ->
    Data = "<?xml version='1.0'?>\n<methodResponse>\nbaddata<params>\n<param>\n<value><struct>\n<member>\n<name>server_serie</name>\n<value><string>9.0</string></value>\n</member>\n<member>\n<name>server_version_info</name>\n<value><array><data>\n<value><int>9</int></value>\n<value><int>0</int></value>\n<value><int>0</int></value>\n<value><string>final</string></value>\n<value><int>0</int></value>\n<value><string>c</string></value>\n</data></array></value>\n</member>\n<member>\n<name>server_version</name>\n<value><string>9.0c</string></value>\n</member>\n<member>\n<name>protocol_version</name>\n<value><int>1</int></value>\nbaddata</member>\n</struct></value>\n</param>\n</params>\n</methodResponse>\n",
    {ok, _ } = decode_response_string(Data).


-endif.
    





