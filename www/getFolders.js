document.getElementById("dir_copy").addEventListener("change", function(e) {
        
        let files = e.target.files;
        var arr = new Array(files.length*2);
        for (let i=0; i<files.length; i++) {
                
                //console.log(files[i].webkitRelativePath);
                //console.log(files[i].name);
                arr[i] = files[i].webkitRelativePath;
                arr[i+files.length] = files[i].name;
                
                
        }
        
        Shiny.onInputChange("mydata", arr);
        
});

document.getElementById("dir_backup").addEventListener("change", function(e) {
        
        let files = e.target.files;
        var arr = new Array(files.length*2);
        for (let i=0; i<files.length; i++) {
                
                //console.log(files[i].webkitRelativePath);
                //console.log(files[i].name);
                arr[i] = files[i].webkitRelativePath;
                arr[i+files.length] = files[i].name;
                
                
        }
        
        Shiny.onInputChange("mydata", arr);
        
});
