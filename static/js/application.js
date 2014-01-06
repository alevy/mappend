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
    $.post('/admin/posts/preview', { body: write.children('textarea').val() }, function(data) {
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

  $('a.reference').hover(function() {
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

  $('a.reference').click(function(e) {
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

  $('a[href="#zen"]').click(function(e) {
    e.preventDefault();
    var body = $(this).closest('.bubble')[0];
    if (body.requestFullscreen) {
      body.requestFullscreen();
    } else if (body.mozRequestFullScreen) {
      body.mozRequestFullScreen();
    } else if (body.webkitRequestFullscreen) {
      body.webkitRequestFullscreen();
    } else if (body.msRequestFullscreen) {
      body.msRequestFullscreen();
    }
    return true;
  });

  $('a[href="#exit-zen"]').click(function(e) {
    e.preventDefault();
    var body = document;
    if (body.exitFullscreen) {
      body.exitFullscreen();
    } else if (body.mozCancelFullScreen) {
      body.mozCancelFullScreen();
    } else if (body.webkitExitFullscreen) {
      body.webkitExitFullscreen();
    } else if (body.msExitFullscreen) {
      body.msExitFullscreen();
    }
    return true;
  });

  var fullScreenChangeHandler = function() {
    if (document.fullscreenElement ||
        document.mozFullScreenElement || document.webkitFullscreenElement ||
        document.msFullscreenElement) {
      // now in full screen mode
      $('a[href="#zen"]').addClass('active');
      $('a[href="#exit-zen"]').removeClass('active');
    } else {
      $('a[href="#zen"]').removeClass('active');
      $('a[href="#exit-zen"]').addClass('active');
    }
  }

  $(document).on('fullscreenchange', fullScreenChangeHandler);
  $(document).on('mozfullscreenchange', fullScreenChangeHandler);
  $(document).on('webkitfullscreenchange', fullScreenChangeHandler);
  $(document).on('msfullscreenchange', fullScreenChangeHandler);

})();
