# Pusheldy Dingseldey

Minimalistic, elegant, powerful

## How?

### Register

```
$ curl -i -X POST -H "Content-type: application/json" -d '{"email": "smeagol@example.com", "password":"myPrecious"}' https://pushdings.com/registrations
HTTP/1.1 303 See Other
server: Cowboy
date: Fri, 29 Apr 2016 20:35:13 GMT
content-length: 0
content-type: text/html
location: https://pushdings.com/registrations/smeagol%40example.com
```

This will send an email to `smeagol@example.com` with a *token* that
you'll need to confirm your registration. Watch out for the *location
header* (redirect) since it points you to where you'll have to send
your confirmation request to.

You can only have *one account per email address*.

### Confirm with token

```
$ curl -i -X PUT -H "Content-type: application/json" -d '{"token": "7135b31b511245ebb15bcab5e04bc763"}' https://smeagol%40example.com:myPrecious@pushdings.com/registrations/smeagol%40example.com
HTTP/1.1 204 No Content
server: Cowboy
date: Sat, 30 Apr 2016 08:28:23 GMT
content-length: 0
content-type: text/html
```

Assuming that `7135b31b511245ebb15bcab5e04bc763` was the token you got
by email.

### Create App

```
$ curl -i -X POST -H "Content-type: application/json" -d '{"auth_uri": "http://example.com/pushdings_auth.php"}' https://smeagol%40example.com:myPrecious@pushdings.com/applications
HTTP/1.1 303 See Other
server: Cowboy
date: Sat, 30 Apr 2016 08:41:30 GMT
content-length: 158
content-type: text/html
location: https://pushdings.com/applications/afdf18dd-4a9e-43a3-bf6a-93bcb9bcb5bc

{"app_id":"afdf18dd-4a9e-43a3-bf6a-93bcb9bcb5bc","auth_uri":"http://example.com/pushdings_auth.php","max_clients":3,"token":"292e0e73-a897-4698-b402-39327e55edd1"}
```

If you don't need authorization handling of your websocket users you
can leave auth_uri away (no payload).

```
$ curl -i -X POST -H "Content-type: application/json" https://smeagol%40example.com:myPrecious@pushdings.com/applications
HTTP/1.1 303 See Other
server: Cowboy
date: Sat, 30 Apr 2016 08:44:56 GMT
content-length: 126
content-type: text/html
location: https://pushdings.com/applications/322bfa18-6864-43d4-ade2-b5c19b70b19e

{"app_id":"322bfa18-6864-43d4-ade2-b5c19b70b19e","auth_uri":"","max_clients":3,"token":"a9f8e245-c13a-43cc-82e6-faef6e4bbfbe"}
```

### Can I haz Websocket?

``` javascript
var app_id = "afdf18dd-4a9e-43a3-bf6a-93bcb9bcb5bc";

var user_id = "user_123";
var user_token = "my_token";

function connect() {
    var ws = new WebSocket("wss://pushdings.com/ws", "pushdings_v1");

    ws.onopen = function() {
        console.log('Connected');
        ws.send(JSON.stringify(
            {app_id: app_id, user_id: user_id, user_token: user_token}));
    };

    ws.onmessage = function (evt)
    {
        var received_msg = evt.data;
        console.log("Received: " + received_msg);
    };
    ws.onclose = function()
    {
        console.log('Connection closed');
    };
    return false;
}
onload  = connect;

```

This results in a call to
`http://example.com/pushdings_auth.php?user=user_123&token=my_token`. If
you want to grant access to that user return 1.


### Push and Profit

```
$ curl -i -X POST -H "Content-type: application/json" -d '"ahoi!"' http://afdf18dd-4a9e-43a3-bf6a-93bcb9bcb5bc:292e0e73-a897-4698-b402-39327e55edd1@pushdings.com/messages
HTTP/1.1 204 No Content
server: Cowboy
date: Sat, 30 Apr 2016 08:49:11 GMT
content-length: 0
content-type: text/html
```

This is a broadcast to any users connected via websocket.
