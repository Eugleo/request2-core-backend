
# reQuest2

## Development info

Quick run:

```
mkdir data
cat > config.cfg << ENDCONF
[server]
data_dir=data
listen_port=9080
ENDCONF
cabal run request2 -- config.cfg
```

Quick test (when the server is running):
```
curl localhost:9080/capability
```
(should reply `["request2"]`)
