
.erlang
%%-*- mode: erlang -*-

code:load_abs(os:getenv("HOME") ++ "/libs/femto_test/ebin/user_default").
code:add_pathz(os:getenv("HOME") ++ "/libs/femto_test/ebin").
code:add_pathz("/usr/local/share/distel/ebin/").



.bash_profile

#Erlang
ERL_LIBS=/udir/tools/otp/lib/erlang/lib
ERL_LIBS=$ERL_LIBS:$HOME/libs/femto_test/deps:$HOME/libs/femto_test/apps:$HOME/repos/
export ERL_LIBS
