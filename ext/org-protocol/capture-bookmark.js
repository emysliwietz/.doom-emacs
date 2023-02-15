javascript:location.href='org-protocol://capture?' +
      new URLSearchParams({
            template: 'x', url: window.location.href,
            title: document.title, body: window.getSelection()});
