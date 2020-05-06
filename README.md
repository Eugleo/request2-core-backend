# reQuest2

## Development info

Quick run:

```
mkdir data
cat > config.cfg << ENDCONF
[server]
data_dir=data
listen_port=9080
db_path=data/database.postgresql
ENDCONF
cabal run request2 -- config.cfg
```

Quick test (when the server is running):

```
curl localhost:9080/capability
```

(should reply `["request2"]`)

### Database setup

You can optionally populate the database with some testing values by first starting the server and the running

```
sqlite3 data/database.postgresql "INSERT INTO teams ('Evženův supertým', '1')"
sqlite3 data/database.postgresql 'INSERT INTO users ("admin", "Evžen", "$6$rounds=31337$zqmLJo8T3acQWalf$FvzezmB.v0hbE8HcFhOqgprJ2p9HAloNqRPUzZifI6cDTzP6IGFXvlrYd2tQIjiABJaT.PrLrIfkX8Qwe45Vw0", "1", "Client Operator Admin")'
sqlite3 data/database.postgresql "INSERT INTO announcements VALUES(1, 'test', 'kecykecy', 0, 123, 1)"
sqlite3 data/database.postgresql "INSERT INTO announcements VALUES(2, 'zprava', 'jinykecykecy', 0, 125, 1)"
```

You'll now have a user `admin` with the password `admin` with full privileges.
