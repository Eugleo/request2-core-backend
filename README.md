# Request2 backend

This is a part of a larger project of Request2. See https://github.com/Eugleo/request2-service for details.

## Development info

### How to start the backend

```sh
cat > config.cfg << ENDCONF
[server]
listen_port=9080
db_host=localhost
db_user=request
db_name=request
allow_cors=True
ENDCONF
cabal run request2 -- run-server -c config.cfg
```

> Make sure to have a database `db_name` created under user `db_user`

Quick test (when the server is running):

```
curl localhost:9080/capability
```

(should reply `["request2"]`)

### Database setup

You can optionally populate the database with some testing values by running

```
psql request
INSERT INTO teams (name, code, active) VALUES ('Evženův supertým', 7, TRUE);
INSERT INTO users (email, name, password, roles, date_created, active, telephone, room)  VALUES ('admin', 'Evžen', '$6$rounds=31337$zqmLJo8T3acQWalf$FvzezmB.v0hbE8HcFhOqgprJ2p9HAloNqRPUzZifI6cDTzP6IGFXvlrYd2tQIjiABJaT.PrLrIfkX8Qwe45Vw0', 'Client,Operator,Admin', 1, TRUE, '145', 'B14');
INSERT INTO member (user_id, team_id) VALUES (3, 1);
```

You'll now have a user `admin` with the password `admin` with full privileges.
