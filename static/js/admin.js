var dashOP = function(name, ops) {
    $.ajax({
            url : "/dash",
            data : {username : name,
                    operation : ops},
            method: "POST"
        }).done(function(data){
	        if(data == '/login'){
		        $.get("/login");
		        window.location.reload();
	        } else {
                $("#main_frame").html(data); 
                $("#myhome").removeClass('active');
                $("#admin").addClass('active');
                $("#navtab-dash").find('.active').removeClass('active');
                $("#dash_admin").addClass('active');
            }
        });
}