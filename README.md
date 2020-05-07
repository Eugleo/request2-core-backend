# reQuest2

## Development info

Quick run:

```
mkdir data
cat > config.cfg << ENDCONF
[server]
listen_port=9080
db_conn=host=localhost dbname=request
allow_cors=True
ENDCONF
cabal run request2 -- config.cfg
```

Quick test (when the server is running):

```
curl localhost:9080/capability
```

(should reply `["request2"]`)

### Database setup

You can optionally populate the database with some testing values by running

```
psql request
INSERT INTO teams (name, active) VALUES ('Evženův supertým', TRUE);
INSERT INTO users (email, name, pw_hash, team_id, roles, created)  VALUES ('admin', 'Evžen', '$6$rounds=31337$zqmLJo8T3acQWalf$FvzezmB.v0hbE8HcFhOqgprJ2p9HAloNqRPUzZifI6cDTzP6IGFXvlrYd2tQIjiABJaT.PrLrIfkX8Qwe45Vw0', '1', 'Client Operator Admin', 1);
```

You'll now have a user `admin` with the password `admin` with full privileges.
