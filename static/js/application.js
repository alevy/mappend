(function() {
  var elm = document.getElementById("errors");
  if (elm) {
    elm.scrollIntoView();
  }

  function removeTag(event) {
    $(this).parent().remove();
    $(event.data).remove();
  }

  function addTag(tagName, form, tagInput) {
    var hiddenTag = document.createElement('input');
    hiddenTag.type = 'hidden';
    hiddenTag.name = 'tags[]';
    hiddenTag.value = tagName;
    $(form).append(hiddenTag);

    var tagSpan = document.createElement('span');
    $(tagSpan).attr('class', 'input-tag')
    tagSpan.textContent = tagName;
    var removeBtn = document.createElement('i');
    $(removeBtn).attr('class', 'fa fa-remove tag-remove');
    $(removeBtn).click(hiddenTag, removeTag);
    $(tagSpan).append(removeBtn);
    $('#tag-spans').append(tagSpan);
  }

  $("input[name='tags[]']").each(function(i, elm) {
    $(elm).remove();
    var tags = $('#tags');
    addTag(elm.value, tags.parent().parent(), tags);
  });

  $('#tags').keypress(function(event) {
    if (event.which == 13) {
      event.preventDefault();
      var nextTag = this.textContent.trim();
      this.innerHTML = '&nbsp;';
      $(this).siblings('span.placeholder').css('display', 'inline')

      if (nextTag.length > 0) {
        addTag(nextTag, $(this).parent().parent(), this);
      }
    } else {
      if (this.textContent.trim().length == 0) {
        $(this).siblings('span.placeholder').css('display', 'inline')
      } else {
        $(this).siblings('span.placeholder').css('display', 'none')
      }
    }
  });

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
    $.post('/dashboard/posts/preview', { body: write.children('textarea').val() }, function(data) {
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
