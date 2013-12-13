MAX_NUM=6
for i in  $(seq 1 $MAX_NUM)
do
  sh ./start.sh king_node${i}@127.0.0.1
done
