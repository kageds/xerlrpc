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
-module(xerlrpc).

%% API exports
-export([call/2, call/3]).

%%====================================================================
%% API functions
%%====================================================================
-spec call(string(), any()) -> xerlrpc_lib:decode_response().
call(URL, Call) ->
    case xerlrpc_lib:encode_call_to_string(Call) of
        {ok, Xml}  ->
            docall(URL, Xml);
        {error, Error} ->
            {error, Error}
    end.
-spec call(string(), string()|atom(), any()) -> xerlrpc_lib:decode_response().
call(URL, Method, Args) ->
    case xerlrpc_lib:encode_call_to_string(Method, Args) of
        {ok, Xml} ->
            docall(URL, Xml);
        {error, Error} ->
            {error, Error}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
docall(URL, Xml) ->
    case httpc:request(post, {URL, [{"user-agent", "xerlrpc"},{"accept", "*/*"}], "text/plain", Xml}, [], []) of
        {ok, {_, _, Body}} ->
            xerlrpc_lib:decode_response_string(Body);
        Error ->
            Error
    end.
