Queuerl [![Build Status](https://secure.travis-ci.org/johannesh/ibrowse.png)](http://travis-ci.org/minostro/queuerl)
=====

A Queue system built in Erlang.

How to use it
=====
Add Queuerl to your deps:

```erlang
{deps, [
{queuerl, {git, "https://github.com/minostro/queuerl.git", {branch, "master"}}}
]}.
```
After you have fetched the dependency, you have to add `queuerl_sup` to your application root supervisor:

```erlang
init([]) ->
  Queuerl = {
    queuerl_sup,
    {queuerl_sup, start_link, []},
    permanent,
    5000,
    supervisor,
    []},
  {ok, {{one_for_one, 10, 20}, [Queuerl, ...]} }.
```

Once your application is up and running, `queuerl_sup` will automatically spawn the queue subsystme.  You can enqueue a `Task` as follows:

```erlang
Task = queuerl:new_task(fun() -> erlang:display(hello) end),
queuerl:enqueue(Task),
flush(),
Shell got {succeeded,{<<32,80,218,144,238,8,17,229,141,104,164,94,96,232,145,185>>}}
```

Supervision Tree
-----
![Supervision Tree](https://github.com/minostro/queuerl/blob/master/docs/supervision-tree.png "Supervision Tree")

Build
-----

    $ rebar3 compile
