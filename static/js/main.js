var chat_setup = 0;
var country = "CN";
var starpost0 = function(pageid, line) {
    var op = 1;
    var status ='/world';
    if (line == 'Alpha') {op = 2; status = '/alpha'}
    else if (line == 'Beta') {op = 3; status = '/beta'}
    $.ajax({
            url : "/starpost",
            data : {starpost : pageid,
                    line : op},
            method: "POST"
        }).done(function(){
            alert('Thank you');
        });
}

var flagpost0 = function(tit, u, pageid) {
    $.post( "/flagpost",
            {title : tit,
             url   : u,
             flagpost : pageid},
            function(data){
                if(data == 'ok200') 
                    alert('Flaged it!');
                else 
                    alert(data);
            });
}

var disqusurl = "http://sapphiresoft.io/disqus?uniqueid=";

var disqus_head  = `<blockquote style=\"margin: 10px auto;\">\
                     <h4> {0} • {1} </h4> </blockquote>\
                   <div class=\"panel panel-default\">\
                     <div class=\"panel-body\">\
                       <h3> {2} </h3> <p> {3} </p>\
                       <div class='a2a_kit a2a_kit_size_32 a2a_default_style'>\
                        <a class='a2a_dd' href='https://www.addtoany.com/share'></a>\
                        <a class='a2a_button_email'></a>\
                        <a class='a2a_button_sina_weibo'></a>\
                        <a class='a2a_button_wechat'></a>\
                        <a class='a2a_button_douban'></a>\
                        <a class='a2a_button_evernote'></a>\
                        <a class='a2a_button_pocket'></a>\
                        <a class='a2a_button_facebook'></a>\
                        <a class='a2a_button_twitter'></a>\
                        <a class='a2a_button_google_plus'></a></div>\
                     </div> \
                     <div class=\"panel-footer\">\
                       <a href=\"#navtab\" onclick=\"starpost0(\'{4}\',\'{5}\');\"><span class=\"fa fa-thumbs-o-up\"></span> +1</a> &nbsp;\
                       <a href=\"{6}\" target=\"_blank\"><span class=\"fa fa-external-link\"></span> Open</a> &nbsp;\
                       <a href=\"#navtab\" onclick=\"flagpost0(\'{2}\',\'{6}\',\'{4}\');\"><span class=\"fa fa-flag-o\"></span> Flag</a> &nbsp;\
                     </div></div>\
                     <script>\
                        var a2a_config = a2a_config || {};\
                        a2a_config.linkname = '{2}';\
                        a2a_config.linkurl = disqusurl + '{4}';\
                    </script>\
                    <script async src='https://static.addtoany.com/menu/page.js'></script>`;
                

//string formatting like python-style 
String.format = function(format) {
    var args = Array.prototype.slice.call(arguments, 1);
    return format.replace(/{(\d+)}/g, function(match, number) { 
        return typeof args[number] != 'undefined'
            ? args[number] 
            : match
        ;
    });
};

$(function () { 
    jQuery.get("https://ipinfo.io/", function (response)
    {
        country = response.country;
    }, "jsonp");
    
    $.ajax({
        url : "/union",
        method: "GET"
    }).done(function(data){
        $("#main_frame_1").html(data[0]); 
        $("#main_frame_2").html(data[1]); 
        $("#main_frame_3").html(data[2]); 
    });
});

var disqussPage = function (newid, date, line, title, intro, url) {
    $("#chat_frame").hide();
    $("#main_frame").hide();
    $("#menu_frame").hide();
    $("#disqus_frame").show();
    
    var disqusshead = String.format(disqus_head, date, line, title, intro, newid, line, url);
    reset(newid, disqusurl + newid, title);
    $("#disqusHead").html(disqusshead);

    var link = "world";

    if (line == 'Beta') link = 'beta';
    else if (line == 'Alpha') link = 'alpha';

    $("#navbar-ul").html("<li><a href=\"javascript:window.location.href=\'/#" + link + `\';javascript:window.location.reload();\"><span class='fa fa-space-shuttle'></span> 世界线跃迁</a></li>\
                                  <li><a style="cursor: pointer;" data-toggle="modal" data-target="#submitModal">\
                                  <span class="fa fa-pencil-square-o"></span> Submit</a></li>`);
    window.location.href = "/#disqus_start";
};

var refresh = function(line) {
    var link = '';
    if(line == 'News')
        link = 'world';
    else if(line == 'Ask')
        link = 'alpha';
    else link = 'beta';
    $.ajax({
        url : "/" + link,
        method: "GET"
    }).done(function(data){
        if(link == 'world'){
            $("#main_frame_1").html(data); 
            window.location.href = "/#world";
        }else if (link == 'alpha'){
            $("#main_frame_3").html(data); 
            window.location.href = "/#alpha";
        }else {
            $("#main_frame_2").html(data); 
            window.location.href = "/#beta";
        }
        DISQUSWIDGETS.getCount({reset: true});
    });
};

var aboutPage = function() {
    $("#chat_frame").hide();
    $("#main_frame").hide();
    $("#disqus_frame").hide();
    $("#menu_frame").show();
    $.get("/about", function(data){
        $("#menu_frame").html(data);
        $("#navbar-ul").html(`<li><a href=\"/\"><span class='fa fa-space-shuttle'></span> 世界线跃迁</a></li>\
                              <li><a style="cursor: pointer;" data-toggle="modal" data-target="#submitModal">\
                              <span class="fa fa-pencil-square-o"></span> Submit</a></li>`);
        window.location.href = "/#menu_start";                        
    });
};

var subscr = false;

var setPage = function() {
    $("#chat_frame").hide();
    $("#main_frame").hide();
    $("#disqus_frame").hide();
    $("#menu_frame").show();
    $.get("/setting", function(data){
	    if(data == "/login") {
		    window.location.href = "/login";
	    }else {
            $("#menu_frame").html(data[0]);
            $("#navbar-ul").html(`<li><a href=\"/\"><span class='fa fa-space-shuttle'></span> 世界线跃迁</a></li>\
                                  <li><a style="cursor: pointer;" data-toggle="modal" data-target="#submitModal">\
                                  <span class="fa fa-pencil-square-o"></span> Submit</a></li>`);
            window.location.href = "/#menu_start";
            subscr = (data[1] == 'True' ? true : false);
            $('#checkbox').prop('checked', subscr);
        } 
    });
};

var dashPage = function() {
    $("#chat_frame").hide();
    $("#main_frame").hide();
    $("#disqus_frame").hide();
    $("#menu_frame").show();
    $.get("/dashboard", function(data){
	    if(data == "/login") {
		    window.location.href = "/login";
	    }else {
            $("#menu_frame").html(data);
            $("#navbar-ul").html(`<li><a href=\"/\"><span class='fa fa-space-shuttle'></span> 世界线跃迁</a></li>\
                                  <li><a style="cursor: pointer;" data-toggle="modal" data-target="#submitModal">\
                                  <span class="fa fa-pencil-square-o"></span> Submit</a></li>`);
            window.location.href = "/#menu_start";
        } 
    });
};

var chatRoom = function(){
    $("#main_frame").hide();
    $("#menu_frame").hide();
    $("#disqus_frame").hide();
    if(chat_setup == 0){
        $.get('/chat', function(data){
            if(data == "/login") {
		        window.location.href = "/login";
            } else if (data[0] != "no"){
                $("#chat_frame").html(data[0]);
                join_chat(data[1]);
                $("#navbar-ul").html(`<li><a href=\"/\"><span class='fa fa-space-shuttle'></span> 世界线跃迁</a></li>\
                                  <li><a style="cursor: pointer;" data-toggle="modal" data-target="#submitModal">\
                                  <span class="fa fa-pencil-square-o"></span> Submit</a></li>`);
                window.location.href = "/#chat_start";
            } else {
                alert("Denied!");
                window.location.href = "/";
            }
        });
        chat_setup = 1;
    }
    $("#chat_frame").show();
}

var currentWTPage = 1;
var currentWAPage = 1;
var currentATPage = 1;
var currentAAPage = 1;
var currentBTPage = 1;
var currentBAPage = 1;
var current0Page = 1;
var current1Page = 1;
var current2Page = 1;

var displaypage = function (iftop, pagenum) {
    var currentPage = 1;
    var pageLen = 1;
    switch(iftop)
    {
        case 'WorldTrue':
            currentPage = currentWTPage; 
            pageLen = pageWorldTrueLen; break;
        case 'WorldFalse': 
            currentPage = currentWAPage; 
            pageLen = pageWorldFalseLen; break;
        case 'AlphaTrue':
            currentPage = currentATPage; 
            pageLen = pageAlphaTrueLen; break;
        case 'AlphaFalse': 
            currentPage = currentAAPage; 
            pageLen = pageAlphaFalseLen; break;
        case 'BetaTrue':
            currentPage = currentBTPage; 
            pageLen = pageBetaTrueLen; break;
        case '0' :
            currentPage = current0Page; 
            pageLen = page0Len; break;
        case '1' :
            currentPage = current1Page; 
            pageLen = page1Len; break;
        case '2' :
            currentPage = current2Page; 
            pageLen = page2Len; break;
        default: 
            currentPage = currentBAPage; 
            pageLen = pageBetaFalseLen; break;
    }
    if(pagenum == currentPage) return;
    var div1 = document.getElementById("accordion" + iftop + currentPage); 
    var div2 = document.getElementById("accordion" + iftop + pagenum); 
    var lia1 = document.getElementById("pag_" + iftop + currentPage); 
    var lia2 = document.getElementById("pag_" + iftop + pagenum); 
    var top  = $("#topest" + iftop);
    var previous  = $("#previous" + iftop);
    var next  = $("#next" + iftop);
    var end  = $("#endest" + iftop);
    div1.style.display='none';  
    div2.style.display='block';  
    $("#pag_" + iftop + currentPage).removeClass('active');
    $("#pag_" + iftop + pagenum).addClass('active');
    
    // ==================

    if(currentPage > 1 && currentPage < pageLen){
        if(pagenum == 1){
            top.addClass('disabled');
            previous.addClass('disabled');
        } else if (pagenum == pageLen) {
            end.addClass('disabled');
            next.addClass('disabled');
        }
    } else if (currentPage == 1){
        top.removeClass('disabled');
        previous.removeClass('disabled');
        if(pageLen <= 2) {
            end.addClass('disabled');
            next.addClass('disabled');
        }
    } else if (currentPage == pageLen){
        end.removeClass('disabled');
        next.removeClass('disabled');
        if(pageLen <= 2) {
            top.addClass('disabled');
            previous.addClass('disabled');
        }
    }
    switch(iftop)
    {
        case 'WorldTrue':
            currentWTPage = pagenum; break;
        case 'WorldFalse': 
            currentWAPage = pagenum; break;
        case 'AlphaTrue':
            currentATPage = pagenum; break;
        case 'AlphaFalse': 
            currentAAPage = pagenum; break;
        case 'BetaTrue':
            currentBTPage = pagenum; break;
        case '0':
            current0Page = pagenum; break;
        case '1':
            current1Page = pagenum; break;
        case '2':
            current2Page = pagenum; break;
        default: 
            currentBAPage = pagenum; break;
    }
};

var next = function (iftop) {
    var currentPage = 1;
    switch(iftop)
    {
        case 'WorldTrue':
            currentPage = currentWTPage; 
            pageLen = pageWorldTrueLen; break;
        case 'WorldFalse': 
            currentPage = currentWAPage; 
            pageLen = pageWorldFalseLen; break;
        case 'AlphaTrue':
            currentPage = currentATPage; 
            pageLen = pageAlphaTrueLen; break;
        case 'AlphaFalse': 
            currentPage = currentAAPage; 
            pageLen = pageAlphaFalseLen; break;
        case 'BetaTrue':
            currentPage = currentBTPage; 
            pageLen = pageBetaTrueLen; break;
        case '0' :
            currentPage = current0Page; 
            pageLen = page0Len; break;
        case '1' :
            currentPage = current1Page; 
            pageLen = page1Len; break;
        case '2' :
            currentPage = current2Page; 
            pageLen = page2Len; break;
        default: 
            currentPage = currentBAPage; 
            pageLen = pageBetaFalseLen; break;
    }
    if(currentPage + 1 <= pageLen)
        displaypage(iftop, currentPage + 1);
}

var previous = function (iftop){
    var currentPage = 1;
    switch(iftop)
    {
        case 'WorldTrue':
            currentPage = currentWTPage; break;
        case 'WorldFalse': 
            currentPage = currentWAPage; break;
        case 'AlphaTrue':
            currentPage = currentATPage; break;
        case 'AlphaFalse': 
            currentPage = currentAAPage; break;
        case 'BetaTrue':
            currentPage = currentBTPage; break;
        case '0' :
            currentPage = current0Page; break;
        case '1' :
            currentPage = current1Page; break;
        case '2' :
            currentPage = current2Page; break;
        default: 
            currentPage = currentBAPage; break;
    }
    if(currentPage - 1 > 0)
        displaypage(iftop, currentPage - 1);
}

$("#logout a").click(function(){
    $.get("/logout");
    window.location.href = "/login";
});

//==================== setting =======================

var  getCookie = function(cname) {
    var name = cname + "=";
    var decodedCookie = decodeURIComponent(document.cookie);
    var ca = decodedCookie.split(';');
    for(var i = 0; i <ca.length; i++) {
        var c = ca[i];
        while (c.charAt(0) == ' ') {
            c = c.substring(1);
        }
        if (c.indexOf(name) == 0) {
            return c.substring(name.length, c.length);
        }
    }
    return "";
}

var setting = function() {
    var sub = 0;
    var temp = false;
    var x = getCookie('Lambda');
    var n = getCookie('Name');
    var hash1 = md5($('#inputPassword0').val());
    var hash2 = md5(hash1 + n);
    var hash3 = md5(hash2 + x);

    var hash11 = md5($('#inputPassword1').val());
    var hash12 = md5(hash11 + n);
    var hash21 = md5($('#inputPassword2').val());
    var hash22 = md5(hash21 + n);

    if($("#inputPassword0").val() != ''){
        $("#inputPassword0").val(hash3);
        $("#inputPassword1").val(hash12);
        $("#inputPassword2").val(hash22);
        sub == sub + 1;
    }

    temp = $("#checkbox").is(':checked');
    if(sub == 1) {
        if (subscr == temp) sub = 2;  // not change
        else sub = 0;
    } else {
        if (subscr != temp) sub = 0;  // change
        else sub = -1;
    }

    var postValue = $("#settingForm").serialize() + '&subscr=' + sub;

    $.post('/setting', postValue, function(data) {
        var renderHtml1 = `<div class='alert alert-dismissible alert-danger'>
            <button type='button' class='close' data-dismiss='alert'>&times;</button>
            <strong>Error!</strong> `
        var renderHtml2 = `<div class='alert alert-dismissible alert-success'>
            <button type='button' class='close' data-dismiss='alert'>&times;</button>
            <strong>Cong!</strong> `

        if(data == "success"){
            $("#info-block").html(renderHtml2 + "Success!" + '</div>');
            if(sub >= 0 && sub <= 1) subscr = !subscr;
        }else if (data == ''){
            $("#info-block").html(renderHtml1 + 'Nothing to do!' + '</div>');
        }else{
            $("#info-block").html(renderHtml1 + data + '</div>');
        }

        $('#inputPassword0').val('');
        $('#inputPassword1').val('');
        $('#inputPassword2').val('');
    });
}; 


//=================== star flag =====================

var starpost = function(pageid, line) {
    var op = 1;
    var status ='/world';
    if (line == 'Alpha') {op = 2; status = '/alpha'}
    else if (line == 'Beta') {op = 3; status = '/beta'}
    $.ajax({
            url : "/starpost",
            data : {starpost : pageid,
                    line : op},
            method: "POST"
        }).done(function(data){
	        if(data == 'ok200'){
                $.get(status, function(_data){
                    if (op == 1)
                        $("#main_frame_1").html(_data); 
                    else if (op == 2)
                        $("#main_frame_3").html(_data); 
                    else    
                        $("#main_frame_2").html(_data); 
                    DISQUSWIDGETS.getCount({reset: true});
                });
            } else if (data == '/login'){
                window.location.href = "/login";
            } else alert(data);
        });
}

var purify = function(pageid0, usern, line) {
    var op = 1;
    var status ='/world';
    if (line == 'Alpha') {op = 2; status = '/alpha'}
    else if (line == 'Beta') {op = 3; status = '/beta'}
    $.ajax({
            url : "/purify",
            data : {pageid : pageid0,
                    author : usern,
                    line : op},
            method: "POST"
        }).done(function(data){
	        if(data == 'ok200'){
                $.get(status, function(_data){
                    if (op == 1)
                        $("#main_frame_1").html(_data); 
                    else if (op == 3)
                        $("#main_frame_2").html(_data); 
                    else    
                        $("#main_frame_3").html(_data); 
                });
            } else if (data == '/login'){
                window.location.href = "/login";
            } else alert(data);
        });
}


var deletePost = function(pageid, ops) {
    var op = 1;
    $.ajax({
            url : "/delete",
            data : {deletepost : pageid,
                    line : Number(ops)},
            method: "POST"
        }).done(function(data){
	        if(data == 'ok200'){
                $.get('/dashboard', function(_data){
	                if(_data == "/login") {
		                window.location.href = "/login";
	                } else $("#menu_frame").html(_data); 
                });
            } else alert('Catastropic error!');
        });
}

var flagpost = function(tit, u, pageid) {
    $.post( "/flagpost",
            {title : tit,
             url   : u,
             flagpost : pageid},
            function(data){
                if (data == 'ok200') alert('Flaged it!');
                else {
                  alert(data);
                }
            });
}

var submitnewsHelper2 = function(){

    $.post('/submitnews', $("#submitn").serialize() + "&country=" + country, function(data) {
        var renderHtml = `<div class='alert alert-dismissible alert-danger'>
            <button type='button' class='close' data-dismiss='alert'>&times;</button>
            <strong>Error!</strong> `
        if(data == "ok200") {
                window.location.reload();
        } else if (data == "/login"){
            window.location.href = "/login";
        } else {
            $("#alert-info").html(renderHtml + data + '</div>');
            $("#alert-info").show();
        }
    });
}

function imageExists(url, callback) {
  var img = new Image();
  img.onload = function() { callback(true); };
  img.onerror = function() { callback(false); };
  img.src = url;
}

var submitnewsHelper = function(){

var renderHtml = `<div class='alert alert-dismissible alert-danger'>
                <button type='button' class='close' data-dismiss='alert'>&times;</button>`;
    
var sdata = $("#submitn").serializeArray();
    jQuery.each( sdata, function( i, data ) {
    if (data.name == 'img') 
        if (data.value != ''){
            imageExists(data.value, function(exists) {
                if (exists == false) 
                    $("#alert-info").html(renderHtml + 'Invalid Image! </div>');
                else submitnewsHelper2();
            });
        }else{
            submitnewsHelper2();
        }
    });
}

var submitnews = function() {
    var renderHtml = `<div class='alert alert-dismissible alert-warning'>
                <button type='button' class='close' data-dismiss='alert'>&times;</button>`;
    var renderHtml2 = `<div class='alert alert-dismissible alert-danger'>
                <button type='button' class='close' data-dismiss='alert'>&times;</button>`;
    $("#alert-info").html(renderHtml + 'Pleat wait...The URL is being checked(up to 5 sec)...</div>');

    var sdata = $("#submitn").serializeArray();
    jQuery.each( sdata, function( i, data ) {
      if ( data.name == 'url' && data.value != '') {
          //HTTP access control
          if(data.value.match('^http'))
            $.ajax({
                url: data.value,
                method: "HEAD",
                dataType: "jsonp",  //cross-domain request
                crossDomain: true,
                statusCode:{
                    200 : function(){
                        submitnewsHelper();
                    }
                },
                timeout: 3500
            })
           else {
             $("#alert-info").html(renderHtml2 + 'URL must begin with \'http\'!</div>');
           }
      } else if (data.name == 'url' && data.value == ''){
          submitnewsHelper();
      }
    });

    $("#alert-info").show();
    $("#alert-info2").hide();
    $("#alert-info2").html(renderHtml2 + 'Bad URL or Timeout! Please check the url and your network! </div>')
    $("#alert-info").delay(6000).hide(0);
    $("#alert-info2").delay(6000).show(0);
};

var search = function() {
    $("#chat_frame").hide();
    $("#main_frame").hide();
    $("#disqus_frame").hide();
    $("#menu_frame").show();
        $.get('/search', $("#searchf").serialize(), function(data) {
            if(data == "/login") {
		        window.location.href = "/login";
            } else {
                $("#menu_frame").html(data);
                $("#navbar-ul").html(`<li><a href=\"/\"><span class='fa fa-space-shuttle'></span> 世界线跃迁</a></li>\
                                  <li><a style="cursor: pointer;" data-toggle="modal" data-target="#submitModal">\
                                  <span class="fa fa-pencil-square-o"></span> Submit</a></li>`);
                window.location.href = "/#menu_start";
            }
        });
      };
