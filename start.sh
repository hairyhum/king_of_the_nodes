NAME=$1

echo run_erl -daemon log${NAME}/ log${NAME}/ "erl -eval 'king:start()' -pa ebin -name ${NAME} -setcookie king_node"
mkdir -p log${NAME}
run_erl -daemon log${NAME}/ log${NAME}/ "erl -eval 'king:start()' -pa ebin -name ${NAME} -setcookie king_node"
