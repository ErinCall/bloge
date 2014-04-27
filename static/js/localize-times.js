(function () {
    $.each($('.dateTime'), function (_, element) {
        var date = new Date(element.innerHTML);
        element.innerHTML = date.toLocaleString();
    });
})();
