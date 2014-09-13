$(document).ready(function () {
    'use strict';

    $('code').each(function() {
        var $this = $(this),
            language = $this.attr('class');

        if (typeof language === 'string') {
            $this.attr('data-language', language.toLowerCase());
        }
    });

    Rainbow.color();
});
