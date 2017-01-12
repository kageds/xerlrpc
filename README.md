# XERLRPC

XMLRPC client for erlang, just do

## Usage

Add it to your rebar.config deps:
~~~erlang
{'xerlrpc', ".*", {git, "git@github.com:Neurotec/xerlrpc.git", {tag, "0.1.0"}}}
~~~

## Examples

~~~erlang
H = "http://localhost:8069/xmlrpc/2/common",
{ok, [Resp]} = xerlrpc:call(H, version, []),
proplists:get_value(<<"server_serie">>, Resp).
~~~

or using **record**

~~~erlang
-record(version, {}).
-record(authenticate, {db, username = <<"admin">>, password = <<"admin">>, extra = #{}}).

H = "http://localhost:8069/xmlrpc/2/common",
{ok, _} = xerlrpc:call(H, #version{}),
{ok, [Id]} = xerlrpc:call(H, #authenticate{db="test"}).
~~~

## TODO

  * Complete and Test XML-RPC Spec
