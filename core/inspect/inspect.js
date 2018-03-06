$(function () {
  var reload_explanation = function () {
    $.getJSON("/explain/" + $('#explain').val(), function (result) {
      $('#explanation').html(
        result.ops + (result.unparsed ?
          "<div class='unparsed'>" + result.unparsed + "</div>" : ""));
    });
  };

  var last_hash = '';
  setInterval(function () {
    if (document.location.hash === last_hash) return;
    last_hash = document.location.hash;
    $('#content').empty();
    $.get(decodeURI(document.location.hash.substr(1)), function (reply) {
      $('#content').html(reply);
    });
  }, 50);

  $('body').on('click', 'a', function (e) {
    document.location.hash = encodeURI($(this).attr('href'));
    e.preventDefault();
    return false;
  });

  $('#explain').on('keyup change', reload_explanation);
  reload_explanation();
});
