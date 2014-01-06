(function() {
  var elm = document.getElementById("errors");
  if (elm) {
    elm.scrollIntoView();
  }

  $('a[href="#preview"]').click(function(e) {
    e.preventDefault();
    var link = $(this);
    var write = link.closest('.bubble').children('.write-content');
    var prev = link.closest('.bubble').children('.preview-content');
    prev.html("<p>Loading...</p>")
    prev.show();
    write.hide();
    link.addClass('active');
    link.siblings('.tab').removeClass('active');
    $.post('/admin/posts/preview', { body: write.children('textarea').text() }, function(data) {
      prev.html(data.body);
    });
    return true;
  });

  $('a[href="#write"]').click(function(e) {
    e.preventDefault();
    var link = $(this);
    var write = link.closest('.bubble').children('.write-content');
    var prev = link.closest('.bubble').children('.preview-content');
    write.show();
    prev.hide();
    link.addClass('active');
    link.siblings('.tab').removeClass('active');
    return true;
  });

  $('a.icon').hover(function() {
    var link = $(this);
    var footnote = $(link.attr('href'));
    footnote.fadeIn(100);
    var origin = link.offset();
    footnote.offset({left: origin.left - footnote.width() / 4, top: origin.top + 20});
  }, function() {
    var link = $(this);
    if (link.attr('clicked') != 'true') {
      $(link.attr('href')).fadeOut(100);
    }
  });

  $('a.icon').click(function(e) {
    e.preventDefault();
    e.stopPropagation();
    var link = $(this);
    link.attr('clicked', 'true');
    var footnote = $(link.attr('href'));
    footnote.show();
    var origin = link.offset();
    footnote.offset({left: origin.left - footnote.width() / 4, top: origin.top + 20});
    $(document).one('click', function() {
      link.attr('clicked', 'false');
      footnote.fadeOut(100);
    });
    return true;
  });
})();
