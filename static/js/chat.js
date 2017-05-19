function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':9000' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

var users = [];
var cuser = "";

function refreshUsers() {
    $('#users').html('');
    for(i in users) {
        $('#users').append($(document.createElement('li')).text(users[i]));
    }
}


function checkImg (img) {
    var img = img.toLowerCase();
    if (img.indexOf(".png") >= 0 || img.indexOf(".jpg") >= 0 || img.indexOf(".gif") >= 0)
      return true;
    else
      return false;
}

function onMessage(event) {
    var p1 = '<plaintext>' + event.data;
  
    var res = event.data.split(": ");
    var p2 = '<plaintext>' + res[0] + ":";
    var p3 = '<img style=\"width: 80%;height: auto;max-width: 80%;\" src=\"' + res[1] + '\"/><br>';
    
    if (checkImg(event.data)){
      $('#messages').append(p2);
      $('#messages').append(p3);
    } else 
      $('#messages').append(p1);
    $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});

    if(event.data.match(/^[^:]* joined/)) {
        var user = event.data.replace(/ .*/, '');
        users.push(user);
        refreshUsers();
    }

    var res = event.data.split(": ");
    if(res[1] == "@" + cuser){
        showNotice("Hello", "你被@了，来Chatroom看看吧");
    }

    if(event.data.match(/^[^:]* disconnected/)) {
        var user = event.data.replace(/ .*/, '');
        var idx = users.indexOf(user);
        users = users.slice(0, idx).concat(users.slice(idx + 1));
        refreshUsers();
    }
}

    var join_chat = function (username) {
        $('#warnings').html('');
        var user = username;
        var ws = createWebSocket('/');

        ws.onopen = function() {
            ws.send(user);
        };

        ws.onmessage = function(event) {
            if(event.data.match('^Welcome! Users: ')) {
                /* Calculate the list of initial users */
                cuser = username;

                var str = event.data.replace(/^Welcome! Users: /, '');
                if(str != "") {
                    users = str.split(", ");
                    refreshUsers();
                }

                $('#chat-section').show();
                $('#users-section').show();

                ws.onmessage = onMessage;

                $('#message-form').submit(function () {
                    var text = $('#text').val();
                    ws.send(text);
                    $('#text').val('');
                    return false;
                });
            } else {
                $('#messages').append(event.data);
                ws.close();
            }
        };
        $('#messages').append('Connecting...');
        return false;
    };


function showNotice(title,msg){
    var Notification = window.Notification || window.mozNotification || window.webkitNotification;
    if(Notification){
        Notification.requestPermission(function(status){
            if("granted" != status){
                return;
            }else{
                var tag = "sds"+Math.random();
                var notify = new Notification(
                    title,
                    {
                        dir:'auto',
                        lang:'en',
                        tag:tag,
                        body:msg 
                    });
                    notify.onclick=function(){
                        window.focus();
                    },
                    notify.onerror = function () {
                        console.log("HTML5 message error");
                    };
                    notify.onshow = function () {
                        setTimeout(function(){
                            notify.close();
                        },2000)
                    };
                    notify.onclose = function () {
                        console.log("HTML5 message closing");
                    };
                }
        });
        }else{
            console.log("Your browser dose not support message");
        }
};
