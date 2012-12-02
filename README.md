chordy
======

a distributed hash table in Erlang following the Chord scheme (academic lab project)

**This project does not implement all the features of a complete Chord DHT. Considering N nodes, the lookup operation is in O(n) (ring based) whereas we can achieve O(log(n)) by adding routing tables**