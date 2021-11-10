dir_orig = function(){
        var a = document.getElementById('dir_orig');
        if(a.value === "")
        {
                noFile.innerHTML = "No folder choosen";
        }
        else
        {
                noFile.innerHTML = "";
        }
};

dir_backup = function(){
        var a = document.getElementById('dir_backup');
        if(a.value === "")
        {
                noFile.innerHTML = "No folder choosen";
        }
        else
        {
                noFile.innerHTML = "";
        }
};