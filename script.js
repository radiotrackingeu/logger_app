shinyjs.mark_valid = function(tab_name) {
    var tabset_panel = document.getElementById("data_tab_tabset").children;

    switch (tab_name[0]) {
        case "Logger data":
            tabset_panel[1].children[0].className = "available";
            break;
        case "Receivers":
            tabset_panel[2].children[0].className = "available";
            break;
        case "Connections":
            tabset_panel[3].children[0].className = "available";
            break;
        case "Frequencies":
            tabset_panel[4].children[0].className = "available";
            break;
    }
}

shinyjs.mark_invalid = function(tab_name) {
    var tabset_panel = document.getElementById("data_tab_tabset").children;
    console.log(tab_name[0]);

    switch (tab_name[0]) {
        case "Logger data":
            tabset_panel[1].children[0].className = "needed";
            break;
        case "Receivers":
            tabset_panel[2].children[0].className = "needed";
            break;
        case "Connections":
            tabset_panel[3].children[0].className = "needed";
            break;
        case "Frequencies":
            tabset_panel[4].children[0].className = "needed";
            break;
    }
}

shinyjs.disableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.bind('click.tab', function(e) {
        e.preventDefault();
        return false;
    });
    tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.unbind('click.tab');
    tab.removeClass('disabled');
}

shinyjs.disableButton = function(name) {
    var btn = $()
}
