# reQuest2

## Development info

### How to start backend

```
cat > config.cfg << ENDCONF
[server]
listen_port=9080
db_host=localhost
db_user=request
allow_cors=**True**
ENDCONF
cabal run request2 -- run-server -c config.**cfg**
```

Quick test (when the server is running):

```
curl localhost:9080/capability
```

(should reply `["request2"]`)

### How to start frontend

```
cd frontend
cp sample-env .env  #edit if the location of api server is changed
yarnpkg run react-scripts start
```

Building a static site for production:

```
yarnpkg run react-scripts build
ls -R build/ #see the result
```

### Database setup

You can optionally populate the database with some testing values by running

```
psql request
INSERT INTO teams (name, code, active) VALUES ('Evženův supertým', 7, TRUE);
INSERT INTO users (email, name, password, team_id, roles, date_created, active)  VALUES ('admin', 'Evžen', '$6$rounds=31337$zqmLJo8T3acQWalf$FvzezmB.v0hbE8HcFhOqgprJ2p9HAloNqRPUzZifI6cDTzP6IGFXvlrYd2tQIjiABJaT.PrLrIfkX8Qwe45Vw0', 2, 'Client,Operator,Admin', 1, TRUE);
```

You'll now have a user `admin` with the password `admin` with full privileges.
